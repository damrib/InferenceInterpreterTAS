import gleam/erlang/charlist
import gleam/io
import gleam/result
import input
import interpreter
import parser
import ptype
import term
import typing

type GlobalError {
  TypingError(typing.InferrenceError)
  RuntimeError(interpreter.InterpreterError)
  ParsingError(parser.ParserError)
}

fn adapt_typing_error(
  res: Result(a, typing.InferrenceError),
) -> Result(a, GlobalError) {
  result.map_error(res, fn(err) { TypingError(err) })
}

fn adapt_runtime_error(
  res: Result(a, interpreter.InterpreterError),
) -> Result(a, GlobalError) {
  result.map_error(res, fn(err) { RuntimeError(err) })
}

fn adapt_parsing_error(
  res: Result(a, parser.ParserError),
) -> Result(a, GlobalError) {
  result.map_error(res, fn(err) { ParsingError(err) })
}

fn main_loop() {
  let interpreter = interpreter.interpreter_component()

  case input.input("> ") {
    Error(Nil) -> Nil
    Ok("exit") -> Nil
    Ok(term_string) -> {
      let res = {
        echo term_string
        let #(_, tokens, _) =
          charlist.from_string(term_string) |> parser.tokenise
        use term <- result.try(adapt_parsing_error(parser.parse(tokens)))
        let term = parser.parser_to_pterm(term)
        use term_type <- result.try(adapt_typing_error(typing.inference(term)))
        use interpreter_res <- result.try(
          adapt_runtime_error(interpreter(term)),
        )
        Ok(#(term_type, interpreter_res))
      }
      case res {
        Ok(#(term_type, final_term)) -> {
          io.print("Type Inference result:\n\t")
          io.println(ptype.string_of_type(term_type))
          io.print("Interpreter result:\n\t")
          io.println(term.string_of_term(final_term, 0))
        }
        Error(TypingError(_)) -> io.println("typing error")
        Error(RuntimeError(_)) -> io.println("eval error")
        Error(ParsingError(_)) -> io.println("parsing error")
      }
      main_loop()
    }
  }
}

pub fn main() {
  main_loop()
}
