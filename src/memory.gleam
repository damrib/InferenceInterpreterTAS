import gleam/dict
import gleam/erlang/process
import gleam/int
import gleam/otp/actor
import gleam/result
import gleam/string
import term

pub opaque type MemoryError {
  MemoryAccess(msg: String)
  ChannelCommunication(msg: String)
  UndefinedIndirection(msg: String)
}

type MemoryRegion {
  // the term A = ref(B) where B = ref(t) is represented with indirection
  // In memory {A : Indirection(B), B : Value(t)}
  Indirection(name: String)
  // the term A = ref(t) is represented with value
  // If a value has been sent on a channel A
  // In memory {A : Value(t)}
  Value(term: term.Pterm)
  // When a channel is created
  // In memory {A : Channel}
  Channel(recipient: List(RecvRequest), sender: List(SendRequest))
}

pub type RecvRequest =
  process.Subject(Result(term.Pterm, MemoryError))

pub type SendRequest =
  #(term.Pterm, process.Subject(Result(Nil, MemoryError)))

pub type MemoryRequest {
  Access(
    name: String,
    reply_with: process.Subject(Result(term.Pterm, MemoryError)),
  )
  New(name: String, term: term.Pterm)
  Write(name: String, term: term.Pterm)
  Recv(name: String, client: process.Subject(Result(term.Pterm, MemoryError)))
  Send(
    name: String,
    term: term.Pterm,
    client: process.Subject(Result(Nil, MemoryError)),
  )
  NewChannel(name: String)
}

type MemoryState {
  State(counter: Int, mem: Memory)
}

type Memory =
  dict.Dict(String, MemoryRegion)

pub type MemoryActor =
  actor.Started(process.Subject(MemoryRequest))

pub const actor_timeout = 100

fn new_access_error(name: String) -> MemoryError {
  string.concat(["there is no variable with name: ", name, "in memory"])
  |> MemoryAccess
}

fn new_channel_error(name: String) -> MemoryError {
  string.concat(["there is no channel with name: ", name, "in memory"])
  |> MemoryAccess
}

fn missing_message_error(name: String) -> MemoryError {
  string.append("No message was received on the channel: ", name)
  |> ChannelCommunication
}

fn get_region(mem: Memory, id: String) -> Result(term.Pterm, MemoryError) {
  case dict.get(mem, id) {
    Ok(Value(result)) -> Ok(result)
    Ok(Indirection(next_id)) -> Ok(term.Var(next_id))
    _ -> Error(new_access_error(id))
  }
}

fn get_channel(mem: Memory, id: String) -> Result(MemoryRegion, MemoryError) {
  dict.get(mem, id)
  |> result.map_error(fn(_) { new_channel_error(id) })
}

//fn rewrite_region(
//mem: Memory,
//id: String,
//term: term.Pterm,
//) -> Result(Memory, MemoryError) {
//case dict.get(mem, id) {
//Ok(Value(_)) -> Ok(dict.insert(mem, id, Value(term)))
//Ok(Indirection(next_id)) -> rewrite_region(mem, next_id, term)
//Error(_) -> Error(MemoryAccess(string.append("no region with name: ", id)))
//}
//}

fn create_variable(counter: Int) -> #(Int, String) {
  let region_name = "__MEMREGION__"
  #(counter + 1, string.append(region_name, int.to_string(counter)))
}

/// this function will handle the case where a reference of reference(s) is added in memory
fn add_regions(state: MemoryState, id: String, term: term.Pterm) -> MemoryState {
  case term {
    term.Ref(term.Ref(_) as next) -> {
      let #(new_counter, region_name) = create_variable(state.counter)
      State(new_counter, dict.insert(state.mem, id, Indirection(region_name)))
      |> add_regions(region_name, next)
    }
    term.Ref(t) -> State(state.counter, dict.insert(state.mem, id, Value(t)))
    _ -> panic as "should not happen"
  }
}

fn insert_indirection(
  mem: Memory,
  id: String,
  indirect: String,
) -> Result(Memory, MemoryError) {
  case dict.has_key(mem, indirect) {
    True -> Ok(dict.insert(mem, id, Indirection(indirect)))
    False ->
      Error(
        UndefinedIndirection(string.append(
          "no indirection with name: ",
          indirect,
        )),
      )
  }
}

fn new_channel(state: MemoryState, name: String) -> MemoryState {
  state.mem
  |> dict.insert(name, Channel([], []))
  |> State(state.counter, _)
}

fn memory_access(
  state: MemoryState,
  message: MemoryRequest,
) -> actor.Next(MemoryState, MemoryRequest) {
  case message {
    Access(name, client) -> {
      let val =
        get_region(state.mem, name)
        |> result.map_error(fn(_) { new_access_error(name) })
      actor.send(client, val)
      actor.continue(state)
    }
    Recv(name, client) -> {
      let val = get_channel(state.mem, name)
      let assert Ok(Channel(recipient, sender)) = val
      case sender {
        [] -> {
          let new_mem =
            dict.insert(state.mem, name, Channel([client, ..recipient], sender))
          actor.continue(State(state.counter, new_mem))
        }
        [sender_client, ..rest] -> {
          let new_mem = dict.insert(state.mem, name, Channel(recipient, rest))
          actor.send(client, Ok(sender_client.0))
          actor.send(sender_client.1, Ok(Nil))
          actor.continue(State(state.counter, new_mem))
        }
      }
    }
    Send(name, term, client) -> {
      let val = get_channel(state.mem, name)
      let assert Ok(Channel(recipient, sender)) = val
      case recipient {
        [] -> {
          let new_mem =
            dict.insert(
              state.mem,
              name,
              Channel(recipient, [#(term, client), ..sender]),
            )
          actor.continue(State(state.counter, new_mem))
        }
        [recipient_client, ..rest] -> {
          let new_mem = dict.insert(state.mem, name, Channel(rest, sender))
          actor.send(recipient_client, Ok(term))
          actor.send(client, Ok(Nil))
          actor.continue(State(state.counter, new_mem))
        }
      }
    }
    Write(name, term) -> {
      let assert Ok(new_state) = case term {
        term.Var(region_name) ->
          insert_indirection(state.mem, name, region_name)
        _ -> Ok(dict.insert(state.mem, name, Value(term)))
      }
      actor.continue(State(state.counter, new_state))
    }
    New(name, term) -> add_regions(state, name, term) |> actor.continue
    NewChannel(name) -> new_channel(state, name) |> actor.continue
  }
}

pub fn new_mem(memory: MemoryActor, name: String, term: term.Pterm) {
  memory.data
  |> actor.send(New(name, term))
}

pub fn write_mem(memory: MemoryActor, name: String, term: term.Pterm) {
  memory.data
  |> actor.send(Write(name, term))
}

pub fn channel_mem(memory: MemoryActor, name: String) {
  memory.data
  |> actor.send(NewChannel(name))
}

pub fn recv_message(
  memory: MemoryActor,
  name: String,
) -> Result(term.Pterm, MemoryError) {
  memory.data
  |> actor.call(actor_timeout, fn(arg) { Recv(name, arg) })
}

pub fn send_message(
  memory: MemoryActor,
  name: String,
  term: term.Pterm,
) -> Result(Nil, MemoryError) {
  memory.data
  |> actor.call(actor_timeout, fn(arg) { Send(name, term, arg) })
}

pub fn access_mem(
  memory: MemoryActor,
  name: String,
) -> Result(term.Pterm, MemoryError) {
  memory.data
  |> actor.call(actor_timeout, fn(arg) { Access(name, arg) })
}

fn initial_state() -> MemoryState {
  State(0, dict.new())
}

pub fn memory_actor() -> MemoryActor {
  let assert Ok(mem) =
    initial_state()
    |> actor.new
    |> actor.on_message(memory_access)
    |> actor.start

  mem
}
