Nonterminals term list list_expr elements apply list_proc fork send.
Terminals  integer '(' ')' '[' ']' '{' '}' '+' '-' ',' '#' '.' 'Let' '=' '"'
'In' 'Ref' '!' ':=' '||' '<-' 'Chan' 'Ife' 'Ifz' 'Then' 'Else' 'Rec' 'Head' 'Tail'
'Print' 'Println' atoms.
Rootsymbol term.

Nonassoc 1 'Ife' 'Ifz' 'Else' 'Then'.
Left 100 'Let' 'In' '='.
Left 200 '.'.
Right 300 '(' atoms integer '[' '{'.

Left 400 ':=' '<-'.

Left 500 '+' '-'.

Nonassoc 600 '!'. 

Nonassoc 700 send apply 'Head' 'Tail' 'Ref' 'Rec'.

term -> elements : '$1'.

elements -> 'Ife' elements 'Then' elements 'Else' elements : {ife, '$2', '$4', '$6'}.
elements -> 'Ifz' elements 'Then' elements 'Else' elements : {ifz, '$2', '$4', '$6'}. 
elements -> elements ':=' elements : {assign, '$1', '$3'}.
elements -> '!' elements : {deref, '$2'}.
elements -> 'Ref' elements : {ref, '$2'}.
elements -> 'Rec' elements : {rec, '$2'}.
elements -> 'Head' elements : {head, '$2'}.
elements -> 'Tail' elements : {tail, '$2'}.
elements -> 'Chan' : {chan}.
elements -> 'Let' atoms '=' elements 'In' elements : {letexpr, '$2', '$4', '$6'}.
elements -> '#' atoms '.' elements : {abs, '$2', '$4'}.
elements -> elements '+' elements : {add, '$1', '$3'}.
elements -> elements '-' elements : {sub, '$1', '$3'}.
elements -> '(' elements ')' : '$2'.
elements -> send : '$1'.
elements -> '<-' elements : {recv, '$2'}.
elements -> '(' ')' : {unit}.
elements -> list : '$1'.
elements -> atoms : {var, '$1'}.
elements -> integer : {int, '$1'}.
elements -> fork : '$1'.
elements -> 'Print' '(' elements ')' : {'Print', '$3'}.
elements -> 'Println' '(' elements ')' : {'Println', '$3'}.
elements -> '"' atoms '"' : {str, '$2'}.
elements -> apply : '$1'.

send -> elements '<-' elements : {send, '$1', '$3'}.
apply -> elements elements : {app, '$1', '$2'}.

fork -> '[' elements '||' list_proc ']' : {fork, '$2', '$4'}.

list_proc -> elements : '$1'.
list_proc -> elements '||' list_proc : {fork, '$1', '$3'}.

list -> '{' '}' : {empty}.
list -> '{' list_expr '}' : '$2'.

list_expr -> elements : '$1'.
list_expr -> elements ',' list_expr : {cons, '$1', '$3'}.