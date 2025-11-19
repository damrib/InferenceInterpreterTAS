import gleam/erlang/charlist
import term

pub type Tokens

pub type Success

pub type LexerError

pub type ParserAtom {
  Atom(Int, content: String)
}

pub type ParserTerm {
  Var(ParserAtom)
  App(ParserTerm, ParserTerm)
  Abs(ParserAtom, ParserTerm)
  Integer(Int)
  Add(ParserTerm, ParserTerm)
  Empty
  Head(ParserTerm)
  Tail(ParserTerm)
  Cons(ParserTerm, ParserTerm)
  Ifz(ParserTerm, ParserTerm, ParserTerm)
  Ife(ParserTerm, ParserTerm, ParserTerm)
  Let(ParserAtom, ParserTerm, ParserTerm)
  Rec(ParserAtom, ParserTerm)
  Unit
  Deref(ParserTerm)
  Ref(ParserTerm)
  Assign(ParserTerm, ParserTerm)
}

pub type ParserError

@external(erlang, "token_lexer", "string")
pub fn tokenise(term: charlist.Charlist) -> #(Success, List(Tokens), LexerError)

@external(erlang, "grammar_parser", "parse")
pub fn parse(tokens: List(Tokens)) -> Result(ParserTerm, ParserError)

fn atom_to_string(atom: ParserAtom) -> String {
  echo atom
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
    Integer(n) -> term.Integer(n)
    Add(pt1, pt2) -> {
      let new_pt1 = parser_to_pterm(pt1)
      let new_pt2 = parser_to_pterm(pt2)
      term.Add(new_pt1, new_pt2)
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
    Let(atom1, pt1, pt2) -> {
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
  }
}
