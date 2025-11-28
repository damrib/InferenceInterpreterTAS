import gleam/dict
import gleam/erlang/process
import gleam/int
import gleam/otp/actor
import gleam/string
import term.{
  type Pterm, Abs, Add, App, Assign, Cons, Deref, Empty, Head, Ife, Ifz, Integer,
  Let, Rec, Ref, Tail, Unit, Var,
}

const actor_timeout = 100

pub type Alphamessage {
  Incr(req: process.Subject(Int))
  AddCount(n: Int)
  Get(req: process.Subject(Int))
}

pub type AlphaActor =
  actor.Started(process.Subject(Alphamessage))

pub fn new_var(counter: AlphaActor) -> String {
  let var_id = actor.call(counter.data, actor_timeout, Incr)
  string.append("v", int.to_string(var_id))
}

fn alpha_conversion_auxiliary(
  term: Pterm,
  name_table: dict.Dict(String, String),
  counter: AlphaActor,
) -> Pterm {
  case term {
    Var(name) -> {
      case dict.get(name_table, name) {
        Ok(new_name) -> Var(new_name)
        Error(Nil) -> Var(name)
      }
    }
    App(t1, t2) -> {
      let new_t1 = alpha_conversion_auxiliary(t1, name_table, counter)
      let new_t2 = alpha_conversion_auxiliary(t2, name_table, counter)
      App(new_t1, new_t2)
    }
    Abs(name, t) -> {
      let new_name = new_var(counter)
      let name_table = dict.insert(name_table, name, new_name)
      let new_body = alpha_conversion_auxiliary(t, name_table, counter)
      Abs(new_name, new_body)
    }
    Integer(_) -> term
    Add(t1, t2) -> {
      let new_t1 = alpha_conversion_auxiliary(t1, name_table, counter)
      let new_t2 = alpha_conversion_auxiliary(t2, name_table, counter)
      Add(new_t1, new_t2)
    }
    Empty -> term
    Cons(elem, rest) -> {
      let new_elem = alpha_conversion_auxiliary(elem, name_table, counter)
      let new_rest = alpha_conversion_auxiliary(rest, name_table, counter)
      Cons(new_elem, new_rest)
    }
    Head(list) -> {
      let new_list = alpha_conversion_auxiliary(list, name_table, counter)
      Head(new_list)
    }
    Tail(list) -> {
      let new_list = alpha_conversion_auxiliary(list, name_table, counter)
      Head(new_list)
    }
    Ifz(c, t, e) -> {
      let new_c = alpha_conversion_auxiliary(c, name_table, counter)
      let new_t = alpha_conversion_auxiliary(t, name_table, counter)
      let new_e = alpha_conversion_auxiliary(e, name_table, counter)
      Ifz(new_c, new_t, new_e)
    }
    Ife(c, t, e) -> {
      let new_c = alpha_conversion_auxiliary(c, name_table, counter)
      let new_t = alpha_conversion_auxiliary(t, name_table, counter)
      let new_e = alpha_conversion_auxiliary(e, name_table, counter)
      Ife(new_c, new_t, new_e)
    }
    Let(name, b, e) -> {
      let new_name = new_var(counter)
      let new_table = dict.insert(name_table, name, new_name)
      let new_b = alpha_conversion_auxiliary(b, name_table, counter)
      let new_e = alpha_conversion_auxiliary(e, new_table, counter)
      Let(new_name, new_b, new_e)
    }
    Rec(name, fun) -> {
      let new_name = new_var(counter)
      let new_name_table = dict.insert(name_table, name, new_name)
      let new_fun = alpha_conversion_auxiliary(fun, new_name_table, counter)
      Rec(new_name, new_fun)
    }
    Unit -> Unit
    Ref(term) -> {
      alpha_conversion_auxiliary(term, name_table, counter)
      |> Ref
    }
    Deref(term) -> {
      alpha_conversion_auxiliary(term, name_table, counter)
      |> Deref
    }
    Assign(ref, val) -> {
      let new_ref = alpha_conversion_auxiliary(ref, name_table, counter)
      let new_val = alpha_conversion_auxiliary(val, name_table, counter)
      Assign(new_ref, new_val)
    }
    term.Fork(expr1, expr2) -> {
      let new_expr1 = alpha_conversion_auxiliary(expr1, name_table, counter)
      alpha_conversion_auxiliary(expr2, name_table, counter)
      |> term.Fork(new_expr1, _)
    }
    term.Chan -> term
    term.Recv(chan) -> {
      alpha_conversion_auxiliary(chan, name_table, counter)
      |> term.Recv
    }
    term.Send(chan, content) -> {
      let new_chan = alpha_conversion_auxiliary(chan, name_table, counter)
      alpha_conversion_auxiliary(content, name_table, counter)
      |> term.Send(new_chan, _)
    }
    term.Str(_) -> term
    term.Print(expr) ->
      alpha_conversion_auxiliary(expr, name_table, counter) |> term.Print
    term.Println(expr) ->
      alpha_conversion_auxiliary(expr, name_table, counter) |> term.Println
  }
}

fn alpha_conversion(term: Pterm, act: AlphaActor) -> Pterm {
  let res_term = alpha_conversion_auxiliary(term, dict.new(), act)
  res_term
}

pub fn get_count(counter: AlphaActor) -> Int {
  actor.call(counter.data, actor_timeout, Get)
}

pub fn add_count(counter: AlphaActor, n: Int) {
  actor.send(counter.data, AddCount(n))
}

fn handle_message(
  state: Int,
  message: Alphamessage,
) -> actor.Next(Int, Alphamessage) {
  case message {
    Incr(client) -> {
      actor.send(client, state)
      actor.continue(state + 1)
    }
    Get(client) -> {
      actor.send(client, state)
      actor.continue(state)
    }
    AddCount(n) -> actor.continue(state + n)
  }
}

pub fn new_counter() -> AlphaActor {
  let assert Ok(counter) =
    actor.new(0)
    |> actor.on_message(handle_message)
    |> actor.start
  counter
}

pub fn alpha_component() -> fn(Pterm) -> Pterm {
  let counter = new_counter()

  fn(term: Pterm) -> Pterm { alpha_conversion(term, counter) }
}
