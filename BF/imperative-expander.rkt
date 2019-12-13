#lang br/quicklang

;; Macros ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (bf-module-begin PARSE-TREE)
  #'(#%module-begin
     PARSE-TREE))
(provide (rename-out [bf-module-begin #%module-begin]))

;; bf-program tokens
(define-macro (bf-program OP-OR-LOOP-ARG ...)
  #'(void OP-OR-LOOP-ARG ...))
(provide bf-program)

;; bf-loop tokens
(define-macro (bf-loop "[" OP-OR-LOOP-ARG ... "]")
  #'(until (zero? (current-byte))
           OP-OR-LOOP-ARG ...))
(provide bf-loop)

;; bf-op tokens
(define-macro-cases bf-op
  [(bf-op ">") #'(gt)]
  [(bf-op "<") #'(lt)]
  [(bf-op "+") #'(plus)]
  [(bf-op "-") #'(minus)]
  [(bf-op ".") #'(period)]
  [(bf-op ",") #'(comma)])
(provide bf-op)

;; Byte Array ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BF Byte Array
(define arr (make-vector 30000))
(define ptr 0)

;; Returns the value at the byte specified by the ptr
(define (current-byte) (vector-ref arr ptr))

;; Sets the current byte to the given value
(define (set-current-byte! val) (vector-set! arr ptr val))

;; Operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gt) (set! ptr (add1 ptr)))
(define (lt) (set! ptr (sub1 ptr)))
(define (plus) (set-current-byte! (add1 (current-byte))))
(define (minus) (set-current-byte! (sub1 (current-byte))))
(define (period) (write-byte (current-byte)))
(define (comma) (set-current-byte! (read-byte)))

