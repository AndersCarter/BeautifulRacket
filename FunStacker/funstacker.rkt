#lang br/quicklang

(define (read-syntax path port)
  (define src-lines (port->lines port))                                     
  (define src-datums (format-datums '~a src-lines))                
  (define module-datums `(module stacker-mod "funstacker.rkt" (handle-args ,@src-datums)))                                                              
  (datum->syntax #f module-datums))

(provide read-syntax)

(define-macro (stacker-module-begin HANDLE-EXPR ...)
  #'(#%module-begin HANDLE-EXPR ... (first stack)))

(provide (rename-out [stacker-module-begin #%module-begin]))

;; Stack Implmentation
(define stack empty)

(define (pop-stack!)
  (define arg (first stack))
  (set! stack (rest stack))
  arg)

(define (push-stack! arg)
  (set! stack (cons arg stack)))

;; Handle Function : X -> Void
;; Handle evaluates the next token/argument and decides what to do with it
(define (handle [arg #f])
  (cond [(number? arg) (push-stack! arg)]
        [(or (equal? + arg) (equal? * arg)) (push-stack! (arg (pop-stack!) (pop-stack!)))]))

(provide handle)
(provide + *)