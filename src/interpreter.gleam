import gleam/result
import memory
import term

pub type InterpreterError {
  MemoryException(memory.MemoryError)
  RuntimeException(msg: String)
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

fn eval_app(
  var: String,
  arg: term.Pterm,
  body: term.Pterm,
  mem: memory.MemoryActor,
) -> Result(term.Pterm, InterpreterError) {
  case body {
    term.Var(name) if name == var -> Ok(arg)
    term.Var(_) -> Ok(body)
    term.Abs(name, new_body) if name != var -> {
      use new_term <- result.try(eval_app(var, arg, new_body, mem))
      use eval <- result.try(eval_term(new_term, mem))
      Ok(term.Abs(name, eval))
    }
    term.Add(term1, term2) -> {
      use new_term1 <- result.try(eval_app(var, arg, term1, mem))
      use new_term2 <- result.try(eval_app(var, arg, term2, mem))
      eval_add(new_term1, new_term2, mem)
    }
    term.Cons(t1, rest) -> {
      use new_t1 <- result.try(eval_app(var, arg, t1, mem))
      use new_rest <- result.try(eval_app(var, arg, rest, mem))
      Ok(term.Cons(new_t1, new_rest))
    }
    term.Head(t) -> {
      use new_t <- result.try(eval_app(var, arg, t, mem))
      Ok(term.Head(new_t))
    }
    term.Tail(t) -> {
      use new_t <- result.try(eval_app(var, arg, t, mem))
      Ok(term.Tail(new_t))
    }
    _ -> Ok(body)
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
  case cond_val {
    term.Integer(n) if n == 0 -> eval_term(then, mem)
    term.Integer(_) -> eval_term(els, mem)
    _ -> panic as "should not happen"
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
    _ -> panic as "should not happen"
  }
}

pub fn eval_term(
  term: term.Pterm,
  memory: memory.MemoryActor,
) -> Result(term.Pterm, InterpreterError) {
  case term {
    term.App(term.Abs(var, t), arg) -> eval_app(var, arg, t, memory)
    term.App(t1, t2) -> {
      use new_t1 <- result.try(eval_term(t1, memory))
      use new_t2 <- result.try(eval_term(t2, memory))
      eval_term(term.App(new_t1, new_t2), memory)
    }
    term.Add(t1, t2) -> eval_add(t1, t2, memory)
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
    // TODO SEE IF NOT BETTER TO LET MEMORY ACTOR CHANGE NAME OF VAR
    term.Let(var, term.Ref(_) as t, expr) -> {
      use new_term <- result.try(eval_term(t, memory))
      memory.new_mem(memory, var, new_term)
      eval_term(expr, memory)
    }
    term.Let(var, bind, expr) -> {
      use new_bind <- result.try(eval_term(bind, memory))
      use new_expr <- result.try(eval_app(var, new_bind, expr, memory))
      eval_term(new_expr, memory)
    }
    // TODO
    term.Rec(var, fun) -> eval_app(var, term, fun, memory)
    term.Deref(expr) -> {
      use ref <- result.try(eval_term(expr, memory))
      case ref {
        term.Ref(t) -> Ok(t)
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
          echo region_term
          panic as "should not happen"
        }
      }
      Ok(term.Unit)
    }
    _ -> Ok(term)
  }
}

pub fn interpreter_component() -> fn(term.Pterm) ->
  Result(term.Pterm, InterpreterError) {
  let mem = memory.memory_actor()

  fn(term: term.Pterm) { eval_term(term, mem) }
}
