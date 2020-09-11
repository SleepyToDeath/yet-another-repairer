#lang brag

program ::= statement+

statement ::= stmt-ass
            | stmt-jmp
            | stmt-label
            | stmt-ret

stmt-ass   ::= ID "=" expr ";"
stmt-jmp   ::= "jmp" expr ID ";"
stmt-label ::= "label" ID ":"
stmt-ret   ::= "return" expr ";"

expr ::= INTEGER
       | ID
       | expr AOP expr
       | expr BOP expr
       | "(" expr ")"
