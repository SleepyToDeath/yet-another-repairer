#lang racket/base

(provide tokenize)

(require brag/support)
(require br-parser-tools/lex)
(require (prefix-in sre: br-parser-tools/lex-sre))


(define (string->bool str)
  (if (equal? str "true")
      #t
      (if (equal? str "false")
          #f
          (error "Unknown boolean constant: " str))))


(define-lex-abbrevs
  [all any-char]
  [dec_digit (char-range #\0 #\9)]
  [dec_nonzero (char-range #\1 #\9)]
  [dec_constant (sre:+ dec_digit)]
  [hex_digit (sre:or dec_digit (char-range "a" "f") (char-range "A" "F"))]
  [hex_constant (sre:: (:or "0x" "0X") (sre:+ hex_digit))]
  [oct_digit (char-range #\0 #\7)]
  [oct_constant (sre:: "0" (sre:+ oct_digit))]
  [quote_sign "'"]
  [escapable_char (sre:or "\\" " " quote_sign "." "#" "\"" "n" "t" "r" "b" "f")]
  [escape_code (sre:: "u" hex_digit hex_digit hex_digit hex_digit)]
  [escape_char (sre:: "\\" (sre:or escapable_char escape_code))]
  [not_cr_lf (sre:- all (sre:or "\r" "\n"))]
  [not_star (sre:- all "*")]
  [not_star_slash (sre:- all (:or "*" "/"))]
  [alpha_char (sre:or (char-range "a" "z") (char-range "A" "Z"))]
  [simple_id_char (sre:or alpha_char dec_digit "_" "$" "-")]
  [first_id_char (sre:or alpha_char "_" "$")]
  [quotable_char (sre:- not_cr_lf quote_sign)]
  [string_char (sre:or escape_char (char-range #\u0000 #\u0033) (char-range #\u0035 #\u0091) (char-range #\u0093 #\u0127))]
  [line_comment (sre:: "//" (sre:* not_cr_lf))]
  [long_comment (sre:: "/*" (sre:* not_star) (sre:+ "*") (sre:* (sre:: not_star_slash (sre:* not_star) (sre:+ "*"))) "/")]
  [blank (sre:+ (sre:or " " "\t" "\r" "\n"))]
  [ignored (sre:or blank line_comment long_comment)]
  [quoted_name (sre:: quote_sign (sre:+ quotable_char) quote_sign)]
  [full_identifier (sre:: (sre:+ (sre:: (sre:or first_id_char escape_char quote_sign)
                                        (sre:* (sre:or simple_id_char escape_char))
                                        (sre:? quote_sign)
                                        "."))
                          (sre:: (sre:or first_id_char escape_char quote_sign)
                                 (sre:* (sre:or simple_id_char escape_char))
                                 (sre:? quote_sign)))]
  [identifier (sre:or (sre:: (sre:or first_id_char escape_char) (sre:* (sre:or simple_id_char escape_char)))
                      "<clinit>"
                      "<init>")]
  [at_identifier (sre:: "@" (sre:or (sre:: "parameter" (sre:+ dec_digit) ":")
                                    "this:"
                                    "caughtexception"))]
  [bool_constant (sre:or "true" "false")]
  [integer_constant (sre:: (sre:or dec_constant hex_constant oct_constant) (sre:? "L"))]
  [float_constant (sre:or (sre:: (sre:: dec_constant "." dec_constant)
                                 (sre:? (sre:: (sre:or "e" "E") (sre:? "+" "-") dec_constant))
                                 (sre:? (sre:or "f" "F")))
                          (sre:: "#"
                                 (sre:or (sre:: (sre:? "-") "Infinity") "NaN")
                                 (sre:? (sre:or "f" "F"))))]
  [string_constant (sre:: "\"" (sre:* string_char) "\"")])


(define (tokenize ip)
    (port-count-lines! ip)
    (define my-lexer
      (lexer-src-pos
       ["abstract"
        (token 'ABSTRACT lexeme)]
       ["final"
        (token 'FINAL lexeme)]
       ["native"
        (token 'NATIVE lexeme)]
       ["public"
        (token 'PUBLIC lexeme)]
       ["protected"
        (token 'PROTECTED lexeme)]
       ["private"
        (token 'PRIVATE lexeme)]
       ["static"
        (token 'STATIC lexeme)]
       ["synchronized"
        (token 'SYNCHRONIZED lexeme)]
       ["transient"
        (token 'TRANSIENT lexeme)]
       ["volatile"
        (token 'VOLATILE lexeme)]
       ["strictfp"
        (token 'STRICTFP lexeme)]
       ["enum"
        (token 'ENUM lexeme)]
       ["annotation"
        (token 'ANNOTATION lexeme)]
       ["class"
        (token 'CLASS lexeme)]
       ["interface"
        (token 'INTERFACE lexeme)]
       ["void"
        (token 'VOID lexeme)]
       ["boolean"
        (token 'BOOLEAN lexeme)]
       ["byte"
        (token 'BYTE lexeme)]
       ["short"
        (token 'SHORT lexeme)]
       ["char"
        (token 'CHAR lexeme)]
       ["int"
        (token 'INT lexeme)]
       ["long"
        (token 'LONG lexeme)]
       ["float"
        (token 'FLOAT lexeme)]
       ["double"
        (token 'DOUBLE lexeme)]
       ["null_type"
        (token 'NULL_TYPE lexeme)]
       ["unknown"
        (token 'UNKNOWN lexeme)]
       ["extends"
        (token 'EXTENDS lexeme)]
       ["implements"
        (token 'IMPLEMENTS lexeme)]
       ["breakpoint"
        (token 'BREAKPOINT lexeme)]
       ["case"
        (token 'CASE lexeme)]
       ["catch"
        (token 'CATCH lexeme)]
       ["cmp"
        (token 'CMP lexeme)]
       ["cmpg"
        (token 'CMPG lexeme)]
       ["cmpl"
        (token 'CMPL lexeme)]
       ["default"
        (token 'DEFAULT lexeme)]
       ["entermonitor"
        (token 'ENTERMONITOR lexeme)]
       ["exitmonitor"
        (token 'EXITMONITOR lexeme)]
       ["goto"
        (token 'GOTO lexeme)]
       ["if"
        (token 'IF lexeme)]
       ["instanceof"
        (token 'INSTANCEOF lexeme)]
       ["interfaceinvoke"
        (token 'INTERFACEINVOKE lexeme)]
       ["lengthof"
        (token 'LENGTHOF lexeme)]
       ["lookupswitch"
        (token 'LOOKUPSWITCH lexeme)]
       ["neg"
        (token 'NEG lexeme)]
       ["new"
        (token 'NEW lexeme)]
       ["newarray"
        (token 'NEWARRAY lexeme)]
       ["newmultiarray"
        (token 'NEWMULTIARRAY lexeme)]
       ["nop"
        (token 'NOP lexeme)]
       ["ret"
        (token 'RET lexeme)]
       ["return"
        (token 'RETURN lexeme)]
       ["specialinvoke"
        (token 'SPECIALINVOKE lexeme)]
       ["staticinvoke"
        (token 'STATICINVOKE lexeme)]
       ["dynamicinvoke"
        (token 'DYNAMICINVOKE lexeme)]
       ["tableswitch"
        (token 'TABLESWITCH lexeme)]
       ["throws"
        (token 'THROWS lexeme)]
       ["throw"
        (token 'THROW lexeme)]
       ["virtualinvoke"
        (token 'VIRTUALINVOKE lexeme)]
       ["null"
        (token 'NULL lexeme)]
       ["from"
        (token 'FROM lexeme)]
       ["to"
        (token 'TO lexeme)]
       ["with"
        (token 'WITH lexeme)]
       ["cls"
        (token 'CLS lexeme)]
       [","
        (token 'COMMA lexeme)]
       ["{"
        (token 'LBRACE lexeme)]
       ["}"
        (token 'RBRACE lexeme)]
       [";"
        (token 'SEMICOLON lexeme)]
       ["["
        (token 'LBRACKET lexeme)]
       ["]"
        (token 'RBRACKET lexeme)]
       ["("
        (token 'LPAREN lexeme)]
       [")"
        (token 'RPAREN lexeme)]
       [":"
        (token 'COLON lexeme)]
       ["."
        (token 'DOT lexeme)]
       [quote_sign
        (token 'QUOTE lexeme)]
       [":="
        (token 'COLON_EQUALS lexeme)]
       ["&"
        (token 'AND lexeme)]
       ["|"
        (token 'OR lexeme)]
       ["^"
        (token 'XOR lexeme)]
       ["%"
        (token 'MOD lexeme)]
       ["=="
        (token 'CMPEQ lexeme)]
       ["="
        (token 'EQUALS lexeme)]
       ["!="
        (token 'CMPNE lexeme)]
       ["<="
        (token 'CMPLE lexeme)]
       ["<<"
        (token 'SHL lexeme)]
       ["<"
        (token 'CMPLT lexeme)]
       [">>>"
        (token 'USHR lexeme)]
       [">>"
        (token 'SHR lexeme)]
       [">="
        (token 'CMPGE lexeme)]
       [">"
        (token 'CMPGT lexeme)]
       ["+"
        (token 'PLUS lexeme)]
       ["-"
        (token 'MINUS lexeme)]
       ["*"
        (token 'MULT lexeme)]
       ["/"
        (token 'DIV lexeme)]
       [quoted_name
        (token 'QUOTED_NAME lexeme)]
       [full_identifier
        (token 'FULL_IDENTIFIER lexeme)]
       [identifier
        (token 'IDENTIFIER lexeme)]
       [at_identifier
        (token 'AT_IDENTIFIER lexeme)]
       [bool_constant
        (token 'BOOL_CONSTANT (string->bool lexeme))]
       [integer_constant
        (token 'INTEGER_CONSTANT (string->number lexeme))]
       [float_constant
        (token 'FLOAT_CONSTANT (string->number lexeme))]
       [string_constant
        (token 'STRING_CONSTANT (substring lexeme 1 (string-length lexeme)))]
       [ignored
        (token 'WHITESPACE lexeme #:skip? #t)]
       [(eof)
        (void)]))
    (define (next-token) (my-lexer ip))
    next-token)

