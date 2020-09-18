#lang brag

program ::= statement+

statement ::= stmt-ass
            | stmt-jmp
            | stmt-label
            | stmt-ret

stmt-ass   ::= ident "=" j-expr ";"
stmt-jmp   ::= "jmp" j-expr ident ";"
stmt-label ::= "label" ident ":"
stmt-ret   ::= "return" ";"

j-expr ::= literal
       | ident
       | j-expr binop j-expr

binop ::= BOP

literal ::= INTEGER

ident ::= ID

