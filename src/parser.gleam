import gleam/erlang/charlist
import gleam/list
import term

pub type Tokens

pub type Success

pub type LexerError

pub type ParserAtom {
  Atom(Int, content: String)
}

pub type ParserInteger {
  Integer(Int, content: Int)
}

pub type ParserField {
  Field(name: ParserAtom, body: ParserTerm)
}

pub opaque type ParserTerm {
  Var(ParserAtom)
  App(ParserTerm, ParserTerm)
  Abs(ParserAtom, ParserTerm)
  Int(ParserInteger)
  Add(ParserTerm, ParserTerm)
  Sub(ParserTerm, ParserTerm)
  Empty
  Head(ParserTerm)
  Tail(ParserTerm)
  Cons(ParserTerm, ParserTerm)
  Ifz(ParserTerm, ParserTerm, ParserTerm)
  Ife(ParserTerm, ParserTerm, ParserTerm)
  Letexpr(ParserAtom, ParserTerm, ParserTerm)
  Rec(ParserAtom, ParserTerm)
  Unit
  Deref(ParserTerm)
  Ref(ParserTerm)
  Assign(ParserTerm, ParserTerm)
  Fork(expr1: ParserTerm, expr2: ParserTerm)
  Chan
  Send(chan: ParserTerm, content: ParserTerm)
  Recv(chan: ParserTerm)
  Str(content: ParserAtom)
  Print(expr: ParserTerm)
  Println(expr: ParserTerm)

  Object(fields: List(ParserField))
  Call(expr: ParserTerm, name: ParserAtom)
}

pub type ParserError

@external(erlang, "token_lexer", "string")
pub fn tokenise(term: charlist.Charlist) -> #(Success, List(Tokens), LexerError)

@external(erlang, "grammar_parser", "parse")
pub fn parse(tokens: List(Tokens)) -> Result(ParserTerm, ParserError)

fn atom_to_string(atom: ParserAtom) -> String {
  atom.content
}

fn atom_to_integer(atom: ParserInteger) -> Int {
  atom.content
}

pub fn parser_to_pterm(parser_term: ParserTerm) -> term.Pterm {
  case parser_term {
    Abs(atom1, pt) -> {
      atom_to_string(atom1)
      |> term.Abs(parser_to_pterm(pt))
    }
    Var(atom1) -> atom_to_string(atom1) |> term.Var
    App(pt1, pt2) -> {
      let new_pt1 = parser_to_pterm(pt1)
      let new_pt2 = parser_to_pterm(pt2)
      term.App(new_pt1, new_pt2)
    }
    Int(pinteger) -> atom_to_integer(pinteger) |> term.Integer
    Add(pt1, pt2) -> {
      let new_pt1 = parser_to_pterm(pt1)
      let new_pt2 = parser_to_pterm(pt2)
      term.Add(new_pt1, new_pt2)
    }
    Sub(pt1, pt2) -> {
      let new_pt1 = parser_to_pterm(pt1)
      let new_pt2 = parser_to_pterm(pt2)
      term.Sub(new_pt1, new_pt2)
    }
    Empty -> term.Empty
    Head(pt) -> parser_to_pterm(pt) |> term.Head
    Tail(pt) -> parser_to_pterm(pt) |> term.Tail
    Cons(pt1, pt2) -> {
      let new_pt1 = parser_to_pterm(pt1)
      let new_pt2 = parser_to_pterm(pt2)
      term.Cons(new_pt1, new_pt2)
    }
    Ifz(pt1, pt2, pt3) -> {
      let new_pt1 = parser_to_pterm(pt1)
      let new_pt2 = parser_to_pterm(pt2)
      let new_pt3 = parser_to_pterm(pt3)
      term.Ifz(new_pt1, new_pt2, new_pt3)
    }
    Ife(pt1, pt2, pt3) -> {
      let new_pt1 = parser_to_pterm(pt1)
      let new_pt2 = parser_to_pterm(pt2)
      let new_pt3 = parser_to_pterm(pt3)
      term.Ife(new_pt1, new_pt2, new_pt3)
    }
    Letexpr(atom1, pt1, pt2) -> {
      let name = atom_to_string(atom1)
      let new_pt1 = parser_to_pterm(pt1)
      let new_pt2 = parser_to_pterm(pt2)
      term.Let(name, new_pt1, new_pt2)
    }
    Rec(atom1, pt) -> {
      let name = atom_to_string(atom1)
      let new_pt = parser_to_pterm(pt)
      term.Rec(name, new_pt)
    }
    Unit -> term.Unit
    Deref(pt) -> parser_to_pterm(pt) |> term.Deref
    Ref(pt) -> parser_to_pterm(pt) |> term.Ref
    Assign(pt1, pt2) -> {
      let new_pt1 = parser_to_pterm(pt1)
      let new_pt2 = parser_to_pterm(pt2)
      term.Assign(new_pt1, new_pt2)
    }
    Str(content) -> atom_to_string(content) |> term.Str
    Chan -> term.Chan
    Fork(pt1, pt2) -> {
      let new_pt1 = parser_to_pterm(pt1)
      let new_pt2 = parser_to_pterm(pt2)
      term.Fork(new_pt1, new_pt2)
    }
    Send(pt1, pt2) -> {
      let new_pt1 = parser_to_pterm(pt1)
      let new_pt2 = parser_to_pterm(pt2)
      term.Send(new_pt1, new_pt2)
    }
    Recv(pt) -> parser_to_pterm(pt) |> term.Recv
    Print(pt) -> parser_to_pterm(pt) |> term.Print
    Println(pt) -> parser_to_pterm(pt) |> term.Println
    Call(pt, atom1) -> {
      term.Call(parser_to_pterm(pt), atom_to_string(atom1))
    }
    Object(fields) ->
      list.map(fields, fn(field) {
        term.Field(atom_to_string(field.name), parser_to_pterm(field.body))
      })
      |> term.Object
  }
}
