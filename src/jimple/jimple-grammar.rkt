#lang brag

j_file
  ::= modifier* file_type class_name extends_clause? implements_clause? file_body 

modifier
  ::= ABSTRACT
    | FINAL
    | NATIVE
    | PUBLIC
    | PROTECTED
    | PRIVATE
    | STATIC
    | SYNCHRONIZED
    | TRANSIENT
    | VOLATILE
    | STRICTFP
    | ENUM
    | ANNOTATION

file_type
  ::= CLASS
    | INTERFACE

extends_clause
  ::= /EXTENDS class_name

implements_clause
  ::= /IMPLEMENTS class_name_list

file_body
  ::= /LBRACE j_member* /RBRACE

name_list
  ::= name
    | name /COMMA name_list
 
class_name_list
  ::= class_name (/COMMA class_name)*

@j_member
  ::= field
    | method

field
  ::= modifier* j_type name /SEMICOLON

method
  ::= modifier* j_type name /LPAREN parameter_list? /RPAREN throws_clause? method_body

j_type
  ::= VOID
    | nonvoid_type

parameter_list
  ::= parameter (/COMMA parameter)*

parameter
  ::= nonvoid_type

throws_clause
  ::= /THROWS class_name_list

base_type_no_name
  ::= BOOLEAN
    | BYTE
    | CHAR
    | SHORT
    | INT
    | LONG
    | FLOAT
    | DOUBLE
    | NULL_TYPE

base_type
  ::= BOOLEAN
    | BYTE
    | CHAR
    | SHORT
    | INT
    | LONG
    | FLOAT
    | DOUBLE
    | NULL_TYPE
    | class_name

nonvoid_type
  ::= base_type_no_name array_brackets*
    | QUOTED_NAME array_brackets*
    | IDENTIFIER array_brackets*
    | FULL_IDENTIFIER array_brackets*

array_brackets
  ::= LBRACKET RBRACKET

method_body
  ::= /SEMICOLON
    | LBRACE declaration* statement* catch_clause* RBRACE

declaration
  ::= jimple_type local_name_list /SEMICOLON

jimple_type
  ::= UNKNOWN
    | nonvoid_type

local_name
  ::= name

local_name_list
  ::= local_name
    | local_name /COMMA local_name_list

statement
  ::= label_stmt
    | breakpoint_stmt
    | entermonitor_stmt
    | exitmonitor_stmt
    | tableswitch_stmt
    | lookupswitch_stmt
    | identity_stmt
    | identity_no_type_stmt
    | assign_stmt
    | if_stmt
    | goto_stmt
    | nop_stmt
    | ret_stmt
    | return_stmt
    | throw_stmt
    | invoke_stmt

label_stmt
  ::= label_name COLON

breakpoint_stmt
  ::= BREAKPOINT /SEMICOLON

entermonitor_stmt
  ::= ENTERMONITOR immediate /SEMICOLON

exitmonitor_stmt
  ::= EXITMONITOR immediate /SEMICOLON

tableswitch_stmt
  ::= TABLESWITCH LPAREN immediate RPAREN LBRACE case_stmt+ RBRACE /SEMICOLON

lookupswitch_stmt
  ::= LOOKUPSWITCH LPAREN immediate RPAREN LBRACE case_stmt+ RBRACE /SEMICOLON

identity_stmt
  ::= local_name COLON_EUQALS AT_IDENTIFIER j_type /SEMICOLON

identity_no_type_stmt
  ::= local_name COLON_EUQALS AT_IDENTIFIER /SEMICOLON

assign_stmt
  ::= variable EQUALS expression /SEMICOLON

goto_stmt
  ::= GOTO label_name /SEMICOLON

if_stmt
  ::= IF bool_expr goto_stmt

nop_stmt
  ::= NOP /SEMICOLON

ret_stmt
  ::= RET immediate? /SEMICOLON

return_stmt
  ::= RETURN immediate? /SEMICOLON

throw_stmt
  ::= THROW immediate /SEMICOLON

invoke_stmt
  ::= invoke_expr /SEMICOLON

label_name
  ::= IDENTIFIER

case_stmt
  ::= case_label COLON goto_stmt

case_label
  ::= CASE MINUS? INTEGER_CONSTANT
  ::= DEFAULT

catch_clause
  ::= CATCH class_name FROM label_name TO label_name WITH label_name /SEMICOLON

expression
  ::= new_expr
    | LPAREN nonvoid_type RPAREN immediate
    | immediate INSTANCEOF nonvoid_type
    | invoke_expr
    | reference
    | binop_expr
    | unop_expr
    | immediate

new_expr
  ::= NEW base_type
    | NEWARRAY LPAREN nonvoid_type RPAREN fixed_array_descriptor
    | NEWMULTIARRAY LPAREN base_type RPAREN array_descriptor+

array_descriptor
  ::= LBRACKET immediate? RBRACKET

variable
  ::= reference
    | local_name

bool_expr
  ::= binop_expr
    | unop_expr

invoke_expr
  ::= nonstatic_invoke local_name DOT method_signature LPAREN arg_list? RPAREN
    | STATICINVOKE method_signature LPAREN arg_list? RPAREN
    | DYNAMICINVOKE STRING_CONSTANT unnamed_method_signature LPAREN arg_list? RPAREN
        method_signature LPAREN arg_list? RPAREN

binop_expr
  ::= immediate binop immediate

unop_expr
  ::= unop immediate

nonstatic_invoke
  ::= SPECIALINVOKE
    | VIRTUALINVOKE
    | INTERFACEINVOKE

unnamed_method_signature
  ::= CMPLT j_type LPAREN parameter_list? RPAREN CMPGT

method_signature
  ::= CMPLT class_name COLON j_type name LPAREN parameter_list? RPAREN CMPGT

reference
  ::= array_ref
    | field_ref

array_ref
  ::= IDENTIFIER fixed_array_descriptor
    | QUOTED_NAME fixed_array_descriptor

field_ref
  ::= local_name DOT field_signature
    | field_signature

field_signature
  ::= CMPLT class_name COLON j_type name CMPGT

fixed_array_descriptor
  ::= LBRACKET immediate RBRACKET

arg_list
  ::= immediate
    | immediate /COMMA arg_list

immediate
  ::= local_name
    | constant

constant
  ::= MINUS? INTEGER_CONSTANT
    | MINUS? FLOAT_CONSTANT
    | STRING_CONSTANT
    | CLASS STRING_CONSTANT
    | NULL

binop
  ::= AND
    | OR
    | XOR
    | MOD
    | CMP
    | CMPG
    | CMPL
    | CMPEQ
    | CMPNE
    | CMPGT
    | CMPGE
    | CMPLT
    | CMPLE
    | SHL
    | SHR
    | USHR
    | PLUS
    | MINUS
    | MULT
    | DIV

unop
  ::= LENGTHOF
    | NEG

class_name
  ::= QUOTED_NAME
    | IDENTIFIER
    | FULL_IDENTIFIER

name
  ::= QUOTED_NAME
    | IDENTIFIER

