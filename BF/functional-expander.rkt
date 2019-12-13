#lang br/quicklang

;; Structs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The current state of the bf-program
;; arr - A vector of length 30,000
;; ptr - An integer representation of the current byte being modified

(define-struct bf-state (arr ptr))

;; Returns the value of the current byte of the state
(define (current-byte state)
  (vector-ref (bf-state-arr state) (bf-state-ptr state)))

;; Sets the current byte to the given value
(define (set-current-byte state value)
  (vector-set! (bf-state-arr state) (bf-state-ptr state) value)
  state)

;; Macros ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (bf-module-begin PARSE-TREE)
  #'(#%module-begin PARSE-TREE))

(define-macro (bf-program OP-OR-LOOP-ARG ...)
  #'(begin
      (define first-state (bf-state (make-vector 30000 0) 0))
      (void (fold-funcs first-state (list OP-OR-LOOP-ARG ...)))))

(define-macro (bf-loop "[" OP-OR-LOOP-ARG ... "]")
  #'(λ (state)
      (for/fold ([current-state state])
                ([i (in-naturals)] #:break (zero? (current-byte current-state)))
        (fold-funcs current-state (list OP-OR-LOOP-ARG ...)))))

(define-macro-cases bf-op
  [(bf-op ">") #'gt]
  [(bf-op "<") #'lt]
  [(bf-op "+") #'plus]
  [(bf-op "-") #'minus]
  [(bf-op ".") #'period]
  [(bf-op ",") #'comma])

;; Operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gt state) (bf-state (bf-state-arr state) (add1 (bf-state-ptr state))))
(define (lt state) (bf-state (bf-state-arr state) (sub1 (bf-state-ptr state))))
(define (plus state) (set-current-byte state (add1 (current-byte state))))
(define (minus state) (set-current-byte state (sub1 (current-byte state))))
(define (period state) (write-byte (current-byte state)))
(define (comma state) (set-current-byte state (read-byte)))

;; Helper Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fold-funcs state bf-funcs)
  (for/fold ([current-state state])
            ([bf-func (in-list bf-funcs)])
    (bf-func current-state)))

;; Provisions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide (rename-out [bf-module-begin #%module-begin]))
(provide bf-program)
(provide bf-loop)
(provide bf-op)