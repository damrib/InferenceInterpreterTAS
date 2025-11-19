import gleam/dict
import gleam/int
import gleam/string

pub type Pterm {
  // syntaxe Var("x") : "x" 
  Var(id: String)
  // syntaxe App(app, arg) : app arg
  App(app: Pterm, arg: Pterm)
  // syntaxe Abs("x", expr) : #x.expr
  Abs(var: String, expr: Pterm)
  // syntaxe Integer(10) : 10
  Integer(n: Int)
  // syntaxe Add(expr1, expr2) : expr1 + expr2
  Add(expr1: Pterm, expr2: Pterm)
  // syntaxe Empty : []
  Empty
  // syntaxe Head(list) : Head(list)
  Head(list: Pterm)
  // syntaxe Tail(list) : Tail(list)
  Tail(list: Pterm)
  // syntaxe Cons(elem1, Cons(elem2, ...)) : [elem1, elem2, ...]
  Cons(elem: Pterm, rest: Pterm)
  // syntaxe Ifz(cond, expr1, expr2) : ifz cond then expr1 else expr2
  Ifz(cond: Pterm, then: Pterm, els: Pterm)
  // syntaxe Ife(cond, expr1, expr2) : ife cond then expr1 else expr2
  Ife(cond: Pterm, then: Pterm, els: Pterm)
  // syntaxe Let("x", bind, expr) : Let x = bind in expr
  Let(var: String, bind: Pterm, expr: Pterm)
  // syntaxe Rec("x", fun) : Rec x = fun
  Rec(name: String, fun: Pterm)
  // syntaxe Unit : ()
  Unit
  // syntaxe Deref(var) : !var 
  Deref(var: Pterm)
  // syntaxe Ref(init) : ref init
  Ref(init: Pterm)
  // syntaxe Assign(ref, expr) : ref := expr
  Assign(ref: Pterm, expr: Pterm)
  // syntaxe Fork(expr1, expr2) : expr1 || expr2
  Fork(expr1: Pterm, expr2: Pterm)
  // syntaxe Chan : chan
  Chan
  // syntaxe Send(chan, content) : chan <- content
  Send(chan: Pterm, content: Pterm)
  // syntax Recv(chan) : <- chan
  Recv(chan: Pterm)
}

fn string_of_term_app(term: Pterm, ident: Int) -> String {
  case term {
    App(app, arg) -> {
      let str_app = string_of_term_app(app, ident)
      let str_arg = string_of_term_app(arg, ident)
      string.concat(["(", str_app, " ", str_arg, ")"])
    }
    Abs(var, expr) -> {
      let str_expr = string_of_term(expr, ident)
      string.concat(["(#", var, ".", str_expr, ")"])
    }
    _ -> string_of_term(term, ident)
  }
}

const ident_string = "  "

pub fn string_of_term(term: Pterm, ident: Int) -> String {
  let identation = string.repeat(ident_string, ident)
  let next_identation = string.repeat(ident_string, ident + 1)
  case term {
    Var(id) -> id
    App(app, arg) -> {
      let str_app = string_of_term_app(app, ident)
      let str_arg = string_of_term_app(arg, ident)
      string.concat([str_app, " ", str_arg])
    }
    Abs(var, expr) -> {
      let str_expr = string_of_term(expr, ident)
      string.concat(["#", var, ".", str_expr])
    }
    Integer(n) -> int.to_string(n)
    Add(expr1, expr2) -> {
      let str_expr1 = string_of_term_app(expr1, ident)
      let str_expr2 = string_of_term_app(expr2, ident)
      string.concat([str_expr1, " + ", str_expr2])
    }
    Empty -> "[]"
    Cons(elem, rest) -> {
      string.concat([
        "[",
        string_of_term(elem, ident),
        string_of_term_cons(rest, ident),
      ])
    }
    Head(list) -> {
      string.concat(["head(", string_of_term(list, ident), ")"])
    }
    Tail(list) -> {
      string.concat(["tail(", string_of_term(list, ident), ")"])
    }
    Ifz(c, t, e) -> {
      string.concat([
        "if ",
        string_of_term(c, ident),
        " == 0 then\n\t",
        string_of_term(t, ident),
        "\nelse\n\t",
        string_of_term(e, ident),
      ])
    }
    Ife(c, t, e) -> {
      string.concat([
        "if ",
        string_of_term(c, ident),
        " == [] then\n\t",
        string_of_term(t, ident),
        "\nelse\n\t",
        string_of_term(e, ident),
      ])
    }
    Let(v, b, e) -> {
      string.concat([
        "let ",
        v,
        " = \n",
        next_identation,
        string_of_term(b, ident + 1),
        "\n",
        identation,
        "in ",
        string_of_term(e, ident + 1),
      ])
    }
    Rec(name, fun) -> {
      string.concat(["Rec, ", name, " ", string_of_term(fun, ident)])
    }
    Unit -> "()"
    Ref(term) -> string.append("ref ", string_of_term(term, ident))
    Deref(term) -> string.append("!", string_of_term(term, ident))
    Assign(name, term) ->
      string.concat([
        "(",
        string_of_term(name, ident),
        " := ",
        string_of_term(term, ident),
        ")",
      ])
    Fork(expr1, expr2) ->
      string.concat([
        string_of_term(expr1, ident),
        " || ",
        string_of_term(expr2, ident),
      ])
    Chan -> "chan"
    Send(chan, content) -> {
      string.concat([
        string_of_term(chan, ident),
        " <- ",
        string_of_term(content, ident),
      ])
    }
    Recv(chan) -> string.append(" <- ", string_of_term(chan, ident))
  }
}

fn string_of_term_cons(term: Pterm, ident: Int) -> String {
  case term {
    Cons(elem, rest) -> {
      string.concat([
        ",",
        string_of_term(elem, ident),
        string_of_term_cons(rest, ident),
      ])
    }
    Empty -> "]"
    Deref(t) -> string.concat([", ..!", string_of_term(t, ident), "]"])
    _ -> string.append(string_of_term(term, ident), "]")
  }
}

fn is_partial_application(term: Pterm) -> Bool {
  case term {
    App(fun, _) -> term_depth(fun, dict.new(), 0) > 1
    _ -> False
  }
}

// 
fn term_depth(term: Pterm, env: dict.Dict(String, Int), cpt: Int) -> Int {
  case term {
    Var(name) -> {
      let assert Ok(depth) = dict.get(env, name)
      depth + cpt
    }
    Let(name, bind, expr) -> {
      let new_env = dict.insert(env, name, term_depth(bind, env, 0))
      term_depth(expr, new_env, cpt)
    }
    Abs(_, fun) -> term_depth(fun, env, cpt + 1)
    App(fun, _) -> term_depth(fun, env, cpt - 1)
    Head(expr) -> term_depth(expr, env, cpt)
    Cons(expr, _) -> term_depth(expr, env, cpt)
    Deref(expr) -> term_depth(expr, env, cpt)
    Ifz(_, then, _) -> term_depth(then, env, cpt)
    Ife(_, then, _) -> term_depth(then, env, cpt)
    Rec(name, fun) -> {
      let new_env = dict.insert(env, name, 0)
      term_depth(fun, new_env, cpt)
    }
    _ -> cpt
  }
}

// si on déclare qu'un terme est expansif, alors on va appliquer du polymorphisme faible
// si on a déclarer du polymorphisme faible pour un term expansif, il n'y aura pas de changement
// lors de l'unification
// TODO: approfondir défintion d'expansif
pub fn is_expansive(term: Pterm) -> Bool {
  case term {
    App(t1, t2) ->
      is_partial_application(term) || is_expansive(t1) || is_expansive(t2)
    Let(_, _, expr) -> is_expansive(expr)
    // is then but els is not expansive typing should probably not work
    Ife(_, then, els) -> is_expansive(then) || is_expansive(els)
    Ifz(_, then, els) -> is_expansive(then) || is_expansive(els)
    Ref(_) -> True
    Fork(expr1, expr2) -> is_expansive(expr1) || is_expansive(expr2)
    Chan -> True
    Send(_, content) -> is_expansive(content)
    // TODO: careful
    Recv(chan) -> is_expansive(chan)
    _ -> False
  }
}
