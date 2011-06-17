#lang racket

(provide (all-defined-out))

;; Environment representation.

(define-struct var
  (id
   global?
   (refs #:mutable)
   (sets #:mutable)
   (defs #:mutable)
   (needed? #:mutable)
   primitive))

(define (mutable-var? var) (not (null? (var-sets var))))

(define-struct primitive (nargs inliner unspecified-result?))

(define-struct renaming (renamings))


;; Environments are boxes containing lists of vars.
;; We need boxes because environments can be updated in an effectful fashion.
(define (env/c x) (and (box? x) (list? (unbox x))))

(define/contract (make-global-env) (-> env/c)
  (box
   (list
    (make-var '#%number? #t '() '() '() #f (make-primitive 1 #f #f))
    (make-var '#%+ #t '() '() '() #f (make-primitive 2 #f #f))
    (make-var '#%- #t '() '() '() #f (make-primitive 2 #f #f))
    (make-var '#%mul-non-neg #t '() '() '() #f (make-primitive 2 #f #f))
    (make-var '#%quotient #t '() '() '() #f (make-primitive 2 #f #f))
    (make-var '#%remainder #t '() '() '() #f (make-primitive 2 #f #f))
    (make-var '#%= #t '() '() '() #f (make-primitive 2 #f #f))
    (make-var '#%< #t '() '() '() #f (make-primitive 2 #f #f))
    (make-var '#%> #t '() '() '() #f (make-primitive 2 #f #f))
    (make-var '#%pair? #t '() '() '() #f (make-primitive 1 #f #f))
    (make-var '#%cons #t '() '() '() #f (make-primitive 2 #f #f))
    (make-var '#%car #t '() '() '() #f (make-primitive 1 #f #f))
    (make-var '#%cdr #t '() '() '() #f (make-primitive 1 #f #f))
    (make-var '#%set-car! #t '() '() '() #f (make-primitive 2 #f #t))
    (make-var '#%set-cdr! #t '() '() '() #f (make-primitive 2 #f #t))
    (make-var '#%null? #t '() '() '() #f (make-primitive 1 #f #f))
    (make-var '#%eq? #t '() '() '() #f (make-primitive 2 #f #f))
    (make-var '#%not #t '() '() '() #f (make-primitive 1 #f #f))
    (make-var '#%get-cont #t '() '() '() #f (make-primitive 0 #f #f))
    (make-var '#%graft-to-cont #t '() '() '() #f (make-primitive 2 #f #f))
    (make-var '#%return-to-cont #t '() '() '() #f (make-primitive 2 #f #f))
    (make-var '#%halt #t '() '() '() #f (make-primitive 0 #f #t))
    (make-var '#%symbol? #t '() '() '() #f (make-primitive 1 #f #f))
    (make-var '#%string? #t '() '() '() #f (make-primitive 1 #f #f))
    (make-var '#%string->list #t '() '() '() #f (make-primitive 1 #f #f))
    (make-var '#%list->string #t '() '() '() #f (make-primitive 1 #f #f))
    (make-var '#%make-u8vector #t '() '() '() #f (make-primitive 1 #f #f))
    (make-var '#%u8vector-ref #t '() '() '() #f (make-primitive 2 #f #f))
    (make-var '#%u8vector-set! #t '() '() '() #f (make-primitive 3 #f #t))
    (make-var '#%print #t '() '() '() #f (make-primitive 1 #f #t))
    (make-var '#%clock #t '() '() '() #f (make-primitive 0 #f #f))
    (make-var '#%motor #t '() '() '() #f (make-primitive 2 #f #t))
    (make-var '#%led #t '() '() '() #f (make-primitive 3 #f #t))
    (make-var '#%led2-color #t '() '() '() #f (make-primitive 1 #f #t))
    (make-var '#%getchar-wait #t '() '() '() #f (make-primitive 2 #f #f))
    (make-var '#%putchar #t '() '() '() #f (make-primitive 2 #f #t))
    (make-var '#%beep #t '() '() '() #f (make-primitive 2 #f #f))
    (make-var '#%adc #t '() '() '() #f (make-primitive 1 #f #f))
    (make-var '#%u8vector? #t '() '() '() #f (make-primitive 1 #f #f))
    (make-var '#%sernum #t '() '() '() #f (make-primitive 0 #f #f))
    (make-var '#%u8vector-length #t '() '() '() #f (make-primitive 1 #f #f))
    (make-var '#%boolean? #t '() '() '() #f (make-primitive 1 #f #f))
    (make-var '#%network-init #t '() '() '() #f (make-primitive 0 #f #t))
    (make-var '#%network-cleanup #t '() '() '() #f (make-primitive 0 #f #t))
    (make-var '#%receive-packet-to-u8vector #t '() '() '() #f (make-primitive 1 #f #f))
    (make-var '#%send-packet-from-u8vector #t '() '() '() #f (make-primitive 2 #f #f))
    (make-var '#%ior #t '() '() '() #f (make-primitive 2 #f #f))
    (make-var '#%xor #t '() '() '() #f (make-primitive 2 #f #f))

    (make-var '#%readyq #t '() '() '() #f #f)
    ;; TODO put in a meaningful order
    )))

;; list of primitives that can be safely substituted for the equivalent
;; function when it is called.
;; this saves the calls to the primitive wrapper functions, which are still
;; needed if a program needs the value of a "primitive", for example in :
;; (define foo car)
(define substitute-primitives
  '((number? . #%number?)
    (remainder . #%remainder)
    (= . #%=)
    (< . #%<)
    (> . #%>)
    (pair? . #%pair?)
    (cons . #%cons)
    (car . #%car)
    (cdr . #%cdr)
    (set-car! . #%set-car!)
    (set-cdr! . #%set-cdr!)
    (null? . #%null?)
    (eq? . #%eq?)
    (not . #%not)
    (modulo . #%remainder)
    (symbol? . #%symbol?)
    (string? . #%string?)
    (string->list . #%string->list)
    (list->string . #%list->string)
    (clock . #%clock)
    (beep . #%beep)
    (light . #%adc)
    (adc . #%adc)
    (sernum . #%sernum)
    (motor . #%motor)
    (led . #%led)
    (bitwise-ior . #%ior)
    (bitwise-xor . #%xor)
    (current-time . #%clock)
    (u8vector-length . #%u8vector-length)
    (u8vector-ref . #%u8vector-ref)
    (u8vector-set! . #%u8vector-set!)
    (boolean? . #%boolean?)
    (network-init . #%network-init)
    (network-cleanup . #%network-cleanup)
    (receive-packet-to-u8vector . #%receive-packet-to-u8vector)
    (send-packet-from-u8vector . #%send-packet-from-u8vector)
    ))

(define/contract (env-lookup env id) (env/c symbol? . -> . var?)
  (let loop ((lst (unbox env)) (id id))
    (let ((b (car lst)))
      (cond ((and (renaming? b)
                  (assq id (renaming-renamings b)))
             =>
             (lambda (x)
               (loop (cdr lst) (cadr x))))
            ((and (var? b)
                  (eq? (var-id b) id))
             b)
            ((null? (cdr lst))
             (let ((x (make-var id #t '() '() '() #f #f)))
               (set-box! env (append lst (list x)))
               x))
            (else
             (loop (cdr lst) id))))))

(define/contract (env-extend env ids def)
  (env/c (listof symbol?) any/c . -> . env/c)
  (box (append (map (lambda (id)
                      (make-var id #f '() '() (list def) #f #f))
                    ids)
               (unbox env))))

(define/contract (env-extend-renamings env renamings)
  (env/c list? . -> . env/c)
  (box (cons (make-renaming renamings) (unbox env))))

(define *macros* '())

(define (update-macros name code)
  (set! *macros* (cons (cons name code) *macros*)))