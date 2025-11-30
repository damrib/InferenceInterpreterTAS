import gleam/erlang/process
import gleam/io
import gleam/result
import memory
import term

pub type InterpreterError {
  MemoryException(memory.MemoryError)
  RuntimeException(msg: String)
  Deadlock(msg: String)
}

fn eval_add(
  t1: term.Pterm,
  t2: term.Pterm,
  mem: memory.MemoryActor,
) -> Result(term.Pterm, InterpreterError) {
  use v1 <- result.try(eval_term(t1, mem))
  use v2 <- result.try(eval_term(t2, mem))
  case v1, v2 {
    term.Integer(i1), term.Integer(i2) -> Ok(term.Integer(i1 + i2))
    _, _ -> Ok(term.Add(v1, v2))
  }
}

fn eval_sub(
  t1: term.Pterm,
  t2: term.Pterm,
  mem: memory.MemoryActor,
) -> Result(term.Pterm, InterpreterError) {
  use v1 <- result.try(eval_term(t1, mem))
  use v2 <- result.try(eval_term(t2, mem))
  case v1, v2 {
    term.Integer(i1), term.Integer(i2) -> Ok(term.Integer(i1 - i2))
    _, _ -> Ok(term.Sub(v1, v2))
  }
}

fn substitute_var(
  var: String,
  arg: term.Pterm,
  body: term.Pterm,
) -> Result(term.Pterm, InterpreterError) {
  case body {
    term.Var(name) if name == var -> Ok(arg)
    term.Var(_) -> Ok(body)
    term.Abs(name, new_body) if name != var -> {
      use new_term <- result.try(substitute_var(var, arg, new_body))
      Ok(term.Abs(name, new_term))
    }
    term.App(term1, term2) -> {
      use new_term1 <- result.try(substitute_var(var, arg, term1))
      use new_term2 <- result.try(substitute_var(var, arg, term2))
      Ok(term.App(new_term1, new_term2))
    }
    term.Add(term1, term2) -> {
      use new_term1 <- result.try(substitute_var(var, arg, term1))
      use new_term2 <- result.try(substitute_var(var, arg, term2))
      Ok(term.Add(new_term1, new_term2))
    }
    term.Sub(term1, term2) -> {
      use new_term1 <- result.try(substitute_var(var, arg, term1))
      use new_term2 <- result.try(substitute_var(var, arg, term2))
      Ok(term.Sub(new_term1, new_term2))
    }
    term.Cons(t1, rest) -> {
      use new_t1 <- result.try(substitute_var(var, arg, t1))
      use new_rest <- result.try(substitute_var(var, arg, rest))
      Ok(term.Cons(new_t1, new_rest))
    }
    term.Head(t) -> {
      use new_t <- result.try(substitute_var(var, arg, t))
      Ok(term.Head(new_t))
    }
    term.Tail(t) -> {
      use new_t <- result.try(substitute_var(var, arg, t))
      Ok(term.Tail(new_t))
    }
    term.Ref(t) -> {
      use new_t <- result.try(substitute_var(var, arg, t))
      Ok(term.Ref(new_t))
    }
    term.Ife(cond, then, els) -> {
      use new_cond <- result.try(substitute_var(var, arg, cond))
      use new_then <- result.try(substitute_var(var, arg, then))
      use new_else <- result.try(substitute_var(var, arg, els))
      Ok(term.Ife(new_cond, new_then, new_else))
    }
    term.Ifz(cond, then, els) -> {
      use new_cond <- result.try(substitute_var(var, arg, cond))
      use new_then <- result.try(substitute_var(var, arg, then))
      use new_else <- result.try(substitute_var(var, arg, els))
      Ok(term.Ifz(new_cond, new_then, new_else))
    }
    term.Assign(ref, expr) -> {
      use new_ref <- result.try(substitute_var(var, arg, ref))
      use new_expr <- result.try(substitute_var(var, arg, expr))
      Ok(term.Assign(new_ref, new_expr))
    }
    term.Deref(ref) -> {
      use new_ref <- result.try(substitute_var(var, arg, ref))
      Ok(term.Deref(new_ref))
    }
    term.Fork(expr1, expr2) -> {
      use new_expr1 <- result.try(substitute_var(var, arg, expr1))
      use new_expr2 <- result.try(substitute_var(var, arg, expr2))
      Ok(term.Fork(new_expr1, new_expr2))
    }
    term.Let(name, bind, expr) if name != var -> {
      use new_bind <- result.try(substitute_var(var, arg, bind))
      use new_expr <- result.try(substitute_var(var, arg, expr))
      Ok(term.Let(name, new_bind, new_expr))
    }
    term.Print(expr) -> {
      use new_expr <- result.try(substitute_var(var, arg, expr))
      Ok(term.Print(new_expr))
    }
    term.Println(expr) -> {
      use new_expr <- result.try(substitute_var(var, arg, expr))
      Ok(term.Println(new_expr))
    }
    term.Rec(name, fun) if name != var -> {
      use new_fun <- result.try(substitute_var(var, arg, fun))
      Ok(term.Rec(name, new_fun))
    }
    term.Send(chan, _) -> {
      use new_chan <- result.try(substitute_var(var, arg, chan))
      use new_content <- result.try(substitute_var(var, arg, chan))
      Ok(term.Send(new_chan, new_content))
    }
    term.Recv(chan) -> {
      use new_chan <- result.try(substitute_var(var, arg, chan))
      Ok(term.Recv(new_chan))
    }
    term.Str(_)
    | term.Integer(_)
    | term.Abs(_, _)
    | term.Let(_, _, _)
    | term.Rec(_, _)
    | term.Unit
    | term.Chan
    | term.Empty
    | term.Object(_)
    | term.Call(_, _) -> Ok(body)
  }
}

pub fn list_head(term: term.Pterm) -> Result(term.Pterm, InterpreterError) {
  case term {
    term.Cons(t, _) -> Ok(t)
    term.Empty -> Error(RuntimeException("head applied on a empty list"))
    _ -> panic as "should not happen"
  }
}

pub fn list_tail(term: term.Pterm) -> Result(term.Pterm, InterpreterError) {
  case term {
    term.Cons(_, rest) -> Ok(rest)
    term.Empty -> Error(RuntimeException("tail applied on a empty list"))
    _ -> panic as "should not happen"
  }
}

pub fn eval_ifz(
  cond: term.Pterm,
  then: term.Pterm,
  els: term.Pterm,
  mem: memory.MemoryActor,
) -> Result(term.Pterm, InterpreterError) {
  use cond_val <- result.try(eval_term(cond, mem))
  echo cond_val
  case cond_val {
    term.Integer(n) if n == 0 -> eval_term(then, mem)
    term.Integer(_) -> eval_term(els, mem)
    _ -> panic as "nope"
  }
}

pub fn eval_ife(
  cond: term.Pterm,
  then: term.Pterm,
  els: term.Pterm,
  mem: memory.MemoryActor,
) -> Result(term.Pterm, InterpreterError) {
  use cond_val <- result.try(eval_term(cond, mem))
  case cond_val {
    term.Empty -> eval_term(then, mem)
    term.Cons(_, _) -> eval_term(els, mem)
    _ -> Ok(term.Ife(cond_val, then, els))
  }
}

fn eval_fork(
  term: term.Pterm,
  memory: memory.MemoryActor,
) -> Result(term.Pterm, InterpreterError) {
  case term {
    term.Fork(expr1, expr2) -> {
      process.spawn(fn() { eval_term(expr2, memory) })
      use _ <- result.try(eval_term(expr1, memory))
      Ok(term.Unit)
    }
    _ -> {
      use _ <- result.try(eval_term(term, memory))
      Ok(term.Unit)
    }
  }
}

pub fn eval_term(
  term: term.Pterm,
  memory: memory.MemoryActor,
) -> Result(term.Pterm, InterpreterError) {
  case term {
    term.App(term.Abs(var, t), arg) -> {
      use new_term <- result.try(substitute_var(var, arg, t))
      eval_term(new_term, memory)
    }
    term.App(t1, t2) -> {
      use new_t1 <- result.try(eval_term(t1, memory))
      use new_t2 <- result.try(eval_term(t2, memory))
      eval_term(term.App(new_t1, new_t2), memory)
    }
    term.Add(t1, t2) -> eval_add(t1, t2, memory)
    term.Sub(t1, t2) -> eval_sub(t1, t2, memory)
    term.Cons(t, rest) -> {
      use new_term <- result.try(eval_term(t, memory))
      Ok(term.Cons(new_term, rest))
    }
    term.Head(t) -> {
      use t_list <- result.try(eval_term(t, memory))
      list_head(t_list)
    }
    term.Tail(t) -> {
      use t_list <- result.try(eval_term(t, memory))
      list_tail(t_list)
    }
    term.Ifz(cond, then, els) -> eval_ifz(cond, then, els, memory)
    term.Ife(cond, then, els) -> eval_ife(cond, then, els, memory)
    term.Let(var, bind, expr) -> {
      use new_bind <- result.try(eval_term(bind, memory))
      case new_bind {
        term.Ref(t) -> {
          use new_term <- result.try(eval_term(t, memory))
          memory.new_mem(memory, var, term.Ref(new_term))
          eval_term(expr, memory)
        }
        term.Chan -> {
          memory.channel_mem(memory, var)
          eval_term(expr, memory)
        }
        _ -> {
          use new_expr <- result.try(substitute_var(var, new_bind, expr))
          eval_term(new_expr, memory)
        }
      }
    }
    // TODO
    term.Rec(var, fun) -> substitute_var(var, term, fun)
    term.Deref(expr) -> {
      use ref <- result.try(eval_term(expr, memory))
      case ref {
        term.Ref(t) -> eval_term(t, memory)
        term.Var(region) ->
          memory.access_mem(memory, region) |> result.map_error(MemoryException)
        _ -> {
          echo ref
          panic as "should not happen"
        }
      }
    }
    term.Assign(ref_term, right_term) -> {
      use region_term <- result.try(eval_term(ref_term, memory))
      use right_value <- result.try(eval_term(right_term, memory))
      case region_term {
        term.Var(region_name) ->
          memory.write_mem(memory, region_name, right_value)
        // We do not do anything, as the ref will not be saved into the memory
        // and we already evaluate the right_term above
        // we also have to evaluate the right term as there could be side effects
        // in the right term
        term.Ref(_) -> Nil
        _ -> {
          panic as "should not happen"
        }
      }
      Ok(term.Unit)
    }
    term.Print(t) -> {
      use string_term <- result.try(eval_term(t, memory))
      io.print(term.string_of_term(string_term, 0))
      Ok(term.Unit)
    }
    term.Println(t) -> {
      use string_term <- result.try(eval_term(t, memory))
      io.println(term.string_of_term(string_term, 0))
      Ok(term.Unit)
    }
    term.Fork(_, _) -> eval_fork(term, memory)
    term.Recv(chan_expr) -> {
      use chan <- result.try(eval_term(chan_expr, memory))
      case chan {
        term.Var(name) -> {
          memory.recv_message(memory, name)
          |> result.map_error(fn(err) { MemoryException(err) })
        }
        term.Chan -> Error(Deadlock("Reception on an unbinded channel"))
        _ -> panic as "should not happen"
      }
    }
    term.Send(chan_expr, term) -> {
      use chan <- result.try(eval_term(chan_expr, memory))
      use val <- result.try(eval_term(term, memory))
      case chan {
        term.Var(name) -> {
          use _ <- result.try(
            memory.send_message(memory, name, val)
            |> result.map_error(fn(err) { MemoryException(err) }),
          )
          Ok(val)
        }
        term.Chan -> Error(Deadlock("Message sent on an unbinded channel"))
        _ -> panic as "should not happen"
      }
    }
    _ -> Ok(term)
  }
}

pub fn interpreter_component() -> fn(term.Pterm) ->
  Result(term.Pterm, InterpreterError) {
  let mem = memory.memory_actor()

  fn(term: term.Pterm) { eval_term(term, mem) }
}
