#lang br/quicklang
(require brag/support)

(define (make-tokenizer port)
  (define (next-token)
    (define jsonic-lexer (lexer [(from/to "//" "\n") (next-token)]                                   ;; Ignore Comments
                                [(from/to "@$" "$@") (token 'SEXP-TOK (trim-ends "@$" lexeme "$@"))] ;; Matches s-expressions
                                [any-char (token 'CHAR-TOK lexeme)]                                  ;; Matches all other characters
                                ))
    (jsonic-lexer port))
  next-token)
(provide make-tokenizer)