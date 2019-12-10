#lang br/quicklang

(define (read-syntax path port)
  (define src-lines (port->lines port))                                   ; Retrieves the lines from out input port. Each line represents an argument in a 'stacker' program as a list of strings.
  (define src-datums (format-datums '(handle ~a) src-lines))              ; Converts the 'src-lines' strings into datums using a format string
  (define module-datums `(module stacker-mod "stacker.rkt" ,@src-datums)) ; ` is called a quasiquote. It works the same a ' but it allows for variable into the list. To insert a single value, you
                                                                          ; can use the unquote operator ',' followed by the variable. The ',@' operator is the unquote-splicing operator which is a
                                                                          ; merges the sublist 'src-datums' with the reset of the list. If only the unquote is used the list would itself would be inserted
                                                                          ; instead of the elements in the list.
  (datum->syntax #f module-datums))

(provide read-syntax)

(define-macro (stacker-module-begin HANDLE-EXPR ...) ; define-macro acceps a syntax pattern as the first argument.
  #'(#%module-begin                                  ; The "#'" operator ensures the next code block is evaluated as a syntax object. Syntax objects contain not only the datum but also the lexical context
     HANDLE-EXPR ...
     (first stack)))

(provide (rename-out [stacker-module-begin #%module-begin]))

;; Some basic stack implementation
(define stack empty)

(define (pop-stack!)
  (define arg (first stack))
  (set! stack (rest stack))
  arg)

(define (push-stack! arg)
  (set! stack (cons arg stack)))

;; Handle evaluates the next token/argument and decides what to do with it
(define (handle [arg #f])
  (cond [(number? arg) (push-stack! arg)]
        [(or (equal? + arg) (equal? * arg)) (push-stack! (arg (pop-stack!) (pop-stack!)))]))

(provide handle)
(provide + *)