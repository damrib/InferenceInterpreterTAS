Definitions.

INT = [0-9]+
ATOM = [a-z][a-z_]*
WHITESPACE = [\s\t\n\r]+

Rules.

{WHITESPACE} : skip_token.
{INT} : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.
Let : {token, {'Let', TokenLine}}.
In : {token, {'In', TokenLine}}.
Ife : {token, {'Ife', TokenLine}}.
Ifz : {token, {'Ifz', TokenLine}}.
Then : {token, {'Then', TokenLine}}.
Else : {token, {'Else', TokenLine}}.
Ref : {token, {'Ref', TokenLine}}.
Rec : {token, {'Rec', TokenLine}}.
Chan : {token, {'Chan', TokenLine}}.
Print : {token, {'Print', TokenLine}}.
Println : {token, {'Println', TokenLine}}.
\= : {token, {'=', TokenLine}}.
Tail : {token, {'Tail', TokenLine}}.
Head : {token, {'Head', TokenLine}}.

{ATOM} : {token, {atoms, TokenLine, list_to_atom(TokenChars)}}.

\:\= : {token, {':=', TokenLine}}.
\<\- : {token, {'<-', TokenLine}}.
\|\| : {token, {'||', TokenLine}}.
\+ : {token, {'+', TokenLine}}.
\- : {token, {'-', TokenLine}}.
\( : {token, {'(', TokenLine}}.
\) : {token, {')', TokenLine}}.
\# : {token, {'#', TokenLine}}.
\! : {token, {'!', TokenLine}}.
\{ : {token, {'{', TokenLine}}.
\} : {token, {'}', TokenLine}}.
\[ : {token, {'[', TokenLine}}.
\] : {token, {']', TokenLine}}.
\, : {token, {',', TokenLine}}.
\. : {token, {'.', TokenLine}}.
"\"" : {token, {'"', TokenLine}}.

Erlang code.
