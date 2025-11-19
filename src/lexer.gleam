import gleam/dict
import gleam/int
import gleam/io
import gleam/list
import gleam/order
import gleam/result
import gleam/string

const dot = "."

const lambda = "#"

const add = "+"

const lpar = "("

const rpar = ")"

const space_separator = " "

pub type Token {
  Ident(String)
  Number(Int)
  Lambda
  Dot
  Add
  Lpar
  Rpar
  End
}

pub type TokenizeError {
  IdentError(msg: String)
  NumericError(msg: String)
  InvalidToken(msg: String)
}

const keywords = [
  #(dot, Dot),
  #(lambda, Lambda),
  #(add, Add),
  #(lpar, Lpar),
  #(rpar, Rpar),
]

pub fn string_of_token(t: Token) {
  case t {
    Ident(i) -> string.concat(["Ident(", i, ")"])
    Number(x) -> string.concat(["Number(", int.to_string(x), ")"])
    Lambda -> "Lambda"
    Add -> "Add"
    Lpar -> "Lpar"
    Rpar -> "Rpar"
    Dot -> "Dot"
    End -> "End"
  }
}

pub fn pretty_of_token(t: Token) {
  case t {
    Ident(s) -> s
    Number(x) -> int.to_string(x)
    Lambda -> lambda
    Add -> add
    Lpar -> lpar
    Rpar -> rpar
    Dot -> dot
    End -> ""
  }
}

pub fn print_pretty_tokens(l: List(Token)) {
  list.each(l, fn(t) {
    io.print(pretty_of_token(t))
    io.print(" ")
  })
  io.println("")
}

pub fn print_tokens_list(l: List(Token)) {
  list.each(l, fn(t) {
    io.print(string_of_token(t))
    io.print(" ")
  })
  io.println("")
}

fn start_ident(str: String) -> Bool {
  let assert Ok(first) = string.first(str)
  let order_a = string.compare("a", first)
  let order_z = string.compare(first, "z")
  order_a != order.Gt && order_z != order.Gt
}

fn ident_char(str: String) -> Bool {
  let assert Ok(first) = string.first(str)
  let is_num = num_char(str)
  start_ident(str) || is_num || first == "_"
}

fn keyword_or(
  str: String,
  keywords: dict.Dict(String, Token),
  or: fn(String) -> Token,
) -> Token {
  let res = dict.get(keywords, str)
  result.unwrap(res, or(str))
}

fn parse_ident(
  str: String,
  buffer: List(String),
  keywords: dict.Dict(String, Token),
) -> #(String, Token) {
  let first = string.first(str)
  case first {
    Ok(c) -> {
      let assert Ok(#(_, rest)) = string.split_once(str, c)
      case ident_char(c) {
        True -> parse_ident(rest, [c, ..buffer], keywords)
        False -> {
          let final_string = string.concat(list.reverse(buffer))
          #(str, keyword_or(final_string, keywords, fn(s) { Ident(s) }))
        }
      }
    }
    Error(_) -> {
      let final_string = string.concat(list.reverse(buffer))
      #(str, keyword_or(final_string, keywords, fn(s) { Ident(s) }))
    }
  }
}

fn num_char(c: String) -> Bool {
  let order_num0 = string.compare("0", c)
  let order_num9 = string.compare("9", c)
  order_num0 != order.Gt && order_num9 != order.Lt
}

/// assumption is made that keywords do not start with numbers
fn parse_number(str: String, buffer: List(String)) -> #(String, Token) {
  let first = string.first(str)
  case first {
    Ok(c) -> {
      let assert Ok(#(_, rest)) = string.split_once(c, str)
      case num_char(c) {
        True -> parse_number(rest, [c, ..buffer])
        False -> {
          let final_string = string.concat(list.reverse(buffer))
          let assert Ok(number) = int.base_parse(final_string, 10)
          #(str, Number(number))
        }
      }
    }
    Error(_) -> {
      let final_string = string.concat(list.reverse(buffer))
      let assert Ok(number) = int.base_parse(final_string, 10)
      #(str, Number(number))
    }
  }
}

fn parse_keyword(
  str: String,
  buffer: String,
  found: Bool,
  keywords: dict.Dict(String, Token),
) -> Result(#(String, Token), TokenizeError) {
  let first = string.first(str)
  case first {
    Ok(c) -> {
      let assert Ok(#(_, rest)) = string.split_once(str, c)
      let new_buffer = string.append(buffer, c)
      case dict.has_key(keywords, new_buffer) {
        False if found -> {
          let assert Ok(token) = dict.get(keywords, buffer)
          Ok(#(str, token))
        }
        True -> {
          parse_keyword(rest, new_buffer, True, keywords)
        }
        False -> parse_keyword(rest, new_buffer, False, keywords)
      }
    }
    Error(_) if found -> {
      let assert Ok(token) = dict.get(keywords, buffer)
      Ok(#(str, token))
    }
    Error(_) -> {
      Error(InvalidToken(""))
    }
  }
}

fn create_ident_token(
  first_char: String,
  str: String,
  keywords: dict.Dict(String, Token),
) -> Result(#(String, Token), TokenizeError) {
  case start_ident(first_char) {
    True -> Ok(parse_ident(str, [first_char], keywords))
    False -> Error(InvalidToken(""))
  }
}

fn create_num_token(
  first_char: String,
  str: String,
) -> Result(#(String, Token), TokenizeError) {
  case num_char(first_char) {
    True -> Ok(parse_number(str, [first_char]))
    False -> Error(InvalidToken(""))
  }
}

fn create_keyword_token(
  str: String,
  keywords: dict.Dict(String, Token),
) -> Result(#(String, Token), TokenizeError) {
  case parse_keyword(str, "", False, keywords) {
    Ok(#(str, token)) -> Ok(#(str, token))
    Error(err) -> Error(err)
  }
}

fn split_string_keywords(
  str: String,
  keywords: dict.Dict(String, Token),
  acc: List(Token),
) -> Result(List(Token), TokenizeError) {
  let first_char = string.first(str)
  case first_char {
    Ok(c) -> {
      let assert Ok(#(_, rest)) = string.split_once(str, c)
      let res = {
        use _ <- result.try_recover(create_ident_token(c, rest, keywords))
        use _ <- result.try_recover(create_num_token(c, rest))
        use _ <- result.try_recover(create_keyword_token(str, keywords))
        Error(InvalidToken("Parsing error"))
      }
      case res {
        Ok(#(rest, token)) ->
          split_string_keywords(rest, keywords, [token, ..acc])
        Error(err) -> Error(err)
      }
    }
    Error(_) -> Ok(acc)
  }
}

fn split_keywords(
  list_str: List(String),
  keywords: dict.Dict(String, Token),
  acc: List(Token),
) -> Result(List(Token), TokenizeError) {
  case list_str {
    [] -> Ok(acc)
    [str, ..rest] -> {
      let new_acc = split_string_keywords(str, keywords, acc)
      case new_acc {
        Ok(acc) -> split_keywords(rest, keywords, acc)
        err -> err
      }
    }
  }
}

pub fn tokenize(term: String) -> Result(List(Token), TokenizeError) {
  let term = string.trim(term)
  let string_list = string.split(term, space_separator)

  let keywords_dict = dict.from_list(keywords)
  case split_keywords(string_list, keywords_dict, []) {
    Ok(l) -> Ok(list.reverse([End, ..l]))
    err -> err
  }
}
