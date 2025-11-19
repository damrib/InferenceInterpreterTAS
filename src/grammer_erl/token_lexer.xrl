Definitions.

INT = [0-9]+
ATOM = [a-z][a-z_]*
WHITESPACE = [\s\t\n\r]+

Rules.

{WHITESPACE} : skip_token.
{INT} : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.
"let" : {token, {'let', TokenLine}}.
"in" : {token, {'in', TokenLine}}.
"ife" : {token, {'ife', TokenLine}}.
"ifz" : {token, {'ifz', TokenLine}}.
"then" : {token, {'then', TokenLine}}.
"else" : {token, {'else', TokenLine}}.
"ref" : {token, {'ref', TokenLine}}.
"rec" : {token, {'rec', TokenLine}}.
":=" : {token, {':=', TokenLine}}.
\+ : {token, {'+', TokenLine}}.
\= : {token, {'=', TokenLine}}.
\( : {token, {'(', TokenLine}}.
\) : {token, {')', TokenLine}}.
\# : {token, {'#', TokenLine}}.
\! : {token, {'!', TokenLine}}.
\[ : {token, {'[', TokenLine}}.
\] : {token, {']', TokenLine}}.
\, : {token, {',', TokenLine}}.
\. : {token, {'.', TokenLine}}.
{ATOM} : {token, {atom, TokenLine, list_to_atom(TokenChars)}}.

Erlang code.
