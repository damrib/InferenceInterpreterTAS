import gleam/io
import gleam/result
import input
import interpreter
import term
import typing

type GlobalError {
  TypingError(typing.InferrenceError)
  RuntimeError(interpreter.InterpreterError)
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

fn main_loop() {
  let interpreter = interpreter.interpreter_component()

  case input.input("> ") {
    Error(Nil) -> Nil
    Ok("exit") -> Nil
    Ok(_) -> {
      let res = {
        use term_type <- result.try(
          adapt_typing_error(typing.inference(term.Var("x"))),
        )
        use interpreter_res <- result.try(
          adapt_runtime_error(interpreter(term.Var("x"))),
        )
        Ok(#(term_type, interpreter_res))
      }
      case res {
        Ok(#(term_type, final_term)) -> {
          io.print("Type Inference result:\n\t")
          io.println(typing.string_of_type(term_type))
          io.print("Interpreter result:\n\t")
          io.println(term.string_of_term(final_term, 0))
        }
        Error(TypingError(_)) -> io.println("typing error")
        Error(RuntimeError(_)) -> io.println("eval error")
      }
      main_loop()
    }
  }
}

pub fn main() {
  main_loop()
}
