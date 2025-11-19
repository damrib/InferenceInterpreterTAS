import gleam/erlang/atom
import gleam/erlang/charlist
import gleam/io
import gleam/string
import gleeunit
import interpreter
import parser
import term.{
  type Pterm, Abs, Add, App, Assign, Chan, Cons, Deref, Empty, Fork, Head, Ife,
  Ifz, Integer, Let, Recv, Ref, Send, Tail, Unit, Var,
}
import typing

pub fn main() {
  gleeunit.main()
}

fn print_test(term: Pterm, res: Result(typing.Ptype, typing.InferrenceError)) {
  let term_string = term.string_of_term(term, 0)
  case res {
    Ok(ty) -> {
      let type_string = typing.string_of_type(typing.pretty_rename(ty))
      io.println(
        string.concat([
          "Term:\n",
          term_string,
          "\nType: ",
          type_string,
          "\n",
        ]),
      )
    }
    Error(typing.UnificationError(msg, #(ty1, ty2))) ->
      io.println(
        string.concat([
          "Term:\n",
          term_string,
          "\nFAILED: ",
          msg,
          " : ",
          typing.string_of_type(ty1),
          " = ",
          typing.string_of_type(ty2),
          "\n",
        ]),
      )
    Error(err) ->
      io.println(
        string.concat(["Term:\n", term_string, "\nFAILED: ", err.msg, "\n"]),
      )
  }
}

const identity_term = Abs("y", Var("y"))

const k_term = Abs("x", Abs("y", Var("x")))

const nat1_term = App(Abs("x", Add(Var("x"), Integer(1))), Integer(3))

const nat2_term = Abs("x", Add(Var("x"), Var("x")))

const omega_term = App(
  Abs("x", App(Var("x"), Var("x"))),
  Abs("y", App(Var("y"), Var("y"))),
)

const let_poly_term = Let(
  "x",
  identity_term,
  Ife(
    App(Var("x"), Empty),
    App(Var("x"), Integer(0)),
    App(Var("x"), Integer(1)),
  ),
)

const weak_poly_term1 = Let(
  "x",
  Ref(Empty),
  Let("_", Cons(Integer(0), Deref(Var("x"))), Cons(Empty, Deref(Var("x")))),
)

const cons_term = Cons(Integer(1), Cons(Integer(2), Cons(Integer(3), Empty)))

const nat3_term = App(nat2_term, identity_term)

const s_term = Abs(
  "x",
  Abs("y", Abs("z", App(App(Var("x"), Var("z")), App(Var("y"), Var("z"))))),
)

const empty_term = Empty

const head_term = Head(cons_term)

const tail_term = Tail(cons_term)

const ifz_term = Ifz(nat1_term, Integer(1), Integer(0))

const let_term = Let("x", cons_term, Head(Var("x")))

const weak_poly_term2 = Let(
  "f",
  Ref(Abs("x", Var("x"))),
  App(
    Abs("y", App(Deref(Var("f")), Unit)),
    Assign(Var("f"), Abs("z", App(s_term, Var("z")))),
  ),
)

const weak_poly_term3 = Let(
  "x",
  Ref(Empty),
  Let(
    "_",
    Let(
      "_",
      Cons(Cons(Integer(0), Empty), Deref(Var("x"))),
      Cons(Integer(0), Deref(Var("x"))),
    ),
    Cons(Empty, Deref(Var("x"))),
  ),
)

const weak_poly_term4 = Let(
  "x",
  Ref(identity_term),
  Let("_", App(Var("x"), Integer(0)), App(Var("x"), Empty)),
)

const let_ref_term = Let(
  "x",
  Ref(Integer(0)),
  Let("_", Assign(Var("x"), Integer(1)), Deref(Var("x"))),
)

const let_ref_expected_type = typing.Tinteger

const let_ref_expected_value = Integer(1)

const ref_ref_term = Let(
  "x",
  Ref(Ref(Integer(0))),
  Let("_", Assign(Deref(Var("x")), Integer(1)), Deref(Deref(Var("x")))),
)

const ref_ref_expected_value = Integer(1)

const fork_term = Let(
  "c",
  Chan,
  Fork(Send(Var("c"), Integer(0)), Recv(Var("c"))),
)

const weak_chan_term = Let(
  "c",
  Chan,
  Fork(Send(Var("c"), Integer(0)), Send(Var("c"), identity_term)),
)

const weak_chan_term2 = Let(
  "c",
  Chan,
  Fork(
    Send(Var("c"), identity_term),
    Let(
      "func",
      Recv(Var("c")),
      Fork(App(Var("func"), Integer(0)), App(Var("func"), Empty)),
    ),
  ),
)

const weak_chan_term3 = Let(
  "c",
  Chan,
  Fork(
    Send(Var("c"), identity_term),
    Let(
      "func",
      Recv(Var("c")),
      App(
        Abs("y", App(Var("func"), Unit)),
        App(Var("func"), Abs("z", App(s_term, Var("z")))),
      ),
    ),
  ),
)

fn test_infer(term: Pterm) {
  let identity_type = typing.inference(term)
  print_test(term, identity_type)
}

pub fn inference_test() {
  test_infer(identity_term)

  let k_type = typing.inference(k_term)
  print_test(k_term, k_type)

  let s_type = typing.inference(s_term)
  print_test(s_term, s_type)

  let nat1_type = typing.inference(nat1_term)
  print_test(nat1_term, nat1_type)

  let nat2_type = typing.inference(nat2_term)
  print_test(nat2_term, nat2_type)

  let nat3_type = typing.inference(nat3_term)
  print_test(nat3_term, nat3_type)

  let omega_type = typing.inference(omega_term)
  print_test(omega_term, omega_type)

  let empty_type = typing.inference(empty_term)
  print_test(empty_term, empty_type)

  let cons_type = typing.inference(cons_term)
  print_test(cons_term, cons_type)

  let head_type = typing.inference(head_term)
  print_test(head_term, head_type)

  let tail_type = typing.inference(tail_term)
  print_test(tail_term, tail_type)

  let ifz_type = typing.inference(ifz_term)
  print_test(ifz_term, ifz_type)

  let let_type = typing.inference(let_term)
  print_test(let_term, let_type)

  let let_poly_type = typing.inference(let_poly_term)
  print_test(let_poly_term, let_poly_type)

  let weak_poly_type1 = typing.inference(weak_poly_term1)
  print_test(weak_poly_term1, weak_poly_type1)

  let weak_poly_type2 = typing.inference(weak_poly_term2)
  print_test(weak_poly_term2, weak_poly_type2)
  // tester prof 2

  let weak_poly_type3 = typing.inference(weak_poly_term3)
  print_test(weak_poly_term3, weak_poly_type3)

  let weak_poly_type4 = typing.inference(weak_poly_term4)
  print_test(weak_poly_term4, weak_poly_type4)

  let let_ref_type = typing.inference(let_ref_term)
  print_test(let_ref_term, let_ref_type)

  let ref_ref_type = typing.inference(ref_ref_term)
  print_test(ref_ref_term, ref_ref_type)

  let fork_type = typing.inference(fork_term)
  print_test(fork_term, fork_type)

  let weak_chan_type = typing.inference(weak_chan_term)
  print_test(weak_chan_term, weak_chan_type)

  let weak_chan_type2 = typing.inference(weak_chan_term2)
  print_test(weak_chan_term2, weak_chan_type2)

  let weak_chan_type3 = typing.inference(weak_chan_term3)
  print_test(weak_chan_term3, weak_chan_type3)
}

fn interpret_term(term: Pterm, expected: Pterm) {
  let interpreter = interpreter.interpreter_component()

  case interpreter(term) {
    Ok(t) ->
      io.println(
        string.concat([
          "Result: ",
          term.string_of_term(t, 0),
          "\nExpected: ",
          term.string_of_term(expected, 0),
          "\n\n",
        ]),
      )
    Error(_) -> panic as "rip"
  }
}

pub fn interpreter_test() {
  io.println("")
  interpret_term(let_ref_term, let_ref_expected_value)

  interpret_term(ref_ref_term, ref_ref_expected_value)
}

pub fn lexer_test() {
  let identity_term = "#x.x"
  let #(_, toks, _) = parser.tokenise(charlist.from_string(identity_term))
  let assert Ok(term) = parser.parse(toks)
  echo typing.inference(term.App(parser.parser_to_pterm(term), term.Integer(0)))

  let k_term = "#x.#y.x"

  let s_term = "#x.#y.#z.(x z) (y z)"
}
