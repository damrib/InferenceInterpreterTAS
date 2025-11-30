import gleam/int
import gleam/list
import gleam/string

pub type Field {
  Field(name: String, body: Pterm)
}

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
  // syntaxe Sub(expr1, expr2) : expr1 - expr2
  Sub(expr1: Pterm, expr2: Pterm)
  // syntaxe Empty : []
  Empty
  // syntaxe Head(list) : Head(list)
  Head(list: Pterm)
  // syntaxe Tail(list) : Tail(list)
  Tail(list: Pterm)
  // syntaxe Cons(elem1, Cons(elem2, ...)) : [elem1, elem2, ...]
  Cons(elem: Pterm, rest: Pterm)
  // syntaxe Ifz(cond, expr1, expr2) : Ifz cond Then expr1 Else expr2
  Ifz(cond: Pterm, then: Pterm, els: Pterm)
  // syntaxe Ife(cond, expr1, expr2) : Ife cond Then expr1 Else expr2
  Ife(cond: Pterm, then: Pterm, els: Pterm)
  // syntaxe Let("x", bind, expr) : Let x = bind In expr
  Let(var: String, bind: Pterm, expr: Pterm)
  // syntaxe Rec("x", fun) : Rec x = fun
  Rec(name: String, fun: Pterm)
  // syntaxe Unit : ()
  Unit
  // syntaxe Deref(var) : !var 
  Deref(var: Pterm)
  // syntaxe Ref(init) : Ref init
  Ref(init: Pterm)
  // syntaxe Assign(ref, expr) : ref := expr
  Assign(ref: Pterm, expr: Pterm)
  // syntaxe Fork(expr1, expr2) : expr1 || expr2
  Fork(expr1: Pterm, expr2: Pterm)
  // syntaxe Chan : chan
  Chan
  // syntaxe Send(chan, content) : chan <- content
  Send(chan: Pterm, content: Pterm)
  // syntaxe Recv(chan) : <- chan
  Recv(chan: Pterm)
  // syntaxe Str("a") : "a"
  Str(content: String)
  // syntaxe Print(expr) : Print(expr)
  Print(expr: Pterm)
  // syntaxe Println(expr) : Println(expr)
  Println(expr: Pterm)

  Object(fields: List(Field))
  Call(expr: Pterm, name: String)
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
    Sub(expr1, expr2) -> {
      let str_expr1 = string_of_term_app(expr1, ident)
      let str_expr2 = string_of_term_app(expr2, ident)
      string.concat([str_expr1, " - ", str_expr2])
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
    Str(content) -> string.concat(["\"", content, "\""])
    Print(expr) -> string.concat(["print(", string_of_term(expr, ident), ")"])
    Println(expr) ->
      string.concat(["println(", string_of_term(expr, ident), ")"])
    Object(fields) -> {
      string.concat([
        "-----------------------------------------------------------------------------\n",
        "                                 Object\n",
        string_of_fields(fields, ident),
        "\n-----------------------------------------------------------------------------",
      ])
    }
    Call(obj, name) -> {
      string.concat([string_of_term(obj, ident), ".", name])
    }
  }
}

fn string_of_fields(fields: List(Field), ident: Int) -> String {
  list.map(fields, fn(field) {
    string.concat([
      "field ",
      field.name,
      "{",
      string_of_term(field.body, ident),
      "}\n",
    ])
  })
  |> string.concat
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
