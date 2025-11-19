Nonterminals term list list_expr elements apply.
Terminals atom integer '(' ')' '[' ']' '+' ',' '#' '.' 'let' '=' 
'in' 'ref' '!' ':=' 'ife' 'ifz' 'then' 'else' 'rec' 'head' 'tail'.
Rootsymbol term.

Nonassoc 1000 'ife' 'ifz' 'else' 'then'.
Nonassoc 900 'let' 'in'.
Left 800 '.'.
Right 700 '(' atom integer '['.

Left 600 ':='.

Left 500 '+'.

Nonassoc 100 '!'. 

Nonassoc 1 apply 'head' 'tail' 'ref' 'rec'.

term -> elements : '$1'.

elements -> 'ife' elements 'then' elements 'else' elements : {ife, '$2', '$4', '$6'}.
elements -> 'ifz' elements 'then' elements 'else' elements : {ifz, '$2', '$4', '$6'}. 
elements -> elements ':=' elements : {assign, '$1', '$3'}.
elements -> '!' elements : {deref, '$2'}.
elements -> 'ref' elements : {ref, '$2'}.
elements -> 'rec' elements : {rec, '$2'}.
elements -> 'head' elements : {head, '$2'}.
elements -> 'tail' elements : {tail, '$2'}.
elements -> 'let' atom '=' elements 'in' elements : {letexpr, '$2', '$4', '$6'}.
elements -> apply : '$1'.
elements -> '#' atom '.' elements : {abs, '$2', '$4'}.
elements -> elements '+' elements : {add, '$1', '$3'}.
elements -> '(' elements ')' : '$2'.
elements -> '(' ')' : {unit}.
elements -> list : '$1'.
elements -> atom : {var, '$1'}.
elements -> integer : {int, '$1'}.

apply -> elements elements : {app, '$1', '$2'}.

list -> '[' ']' : {empty}.
list -> '[' list_expr ']' : '$2'.

list_expr -> elements : '$1'.
list_expr -> elements ',' list_expr : {cons, '$1', '$3'}.