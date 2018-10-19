;;------------------------------------------------------
;;
;;  P R O J E C T  1 :  R E A L L Y - T I N Y   
;;    T Y P E   R E C O N S T R U C T I O N
;;
;;             CS515  Fall 2018
;;------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  Parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define CurrentCounter 0)

(define newtvar
   (lambda()
      (set! CurrentCounter (+ CurrentCounter 1))
      (string->symbol (string-append "_a" (number->string CurrentCounter)))))

(define name?
  (lambda (s)
    (and (symbol? s)
         (not (memq s '(lambda))))))

(define parse
  (lambda (m)
    (cond
      ((number? m)  `(&const ,m))
      ((eq? #t m)   `(&const true))
      ((eq? #f m)   `(&const false))
      ((name? m)    `(&var ,m))
      ((pair? m)    (cond 
                      ((eq? `lambda (car m))
                        (if (and (= 3 (length m))
                                 (list? (cadr m))
                                 (= 1 (length (cadr m)))
                                 (name? (caadr m)))
                            `(&lambda ,(cadr m) ,(parse (caddr m)))
                            (error 'parse "Syntax error")))
                      (else
                        `(&apply ,(parse (car m)) ,(parse (cadr m))))))
      (else         (error 'parse "Syntax error")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     Initial Environments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define init_E  ;; Type Environment 
  '((add1 (int -> int)) (sub1 (int -> int))
    (zero? (int -> bool)) (not (bool -> bool))
    (and (bool -> (bool -> bool)))
    (or (bool -> (bool -> bool)))
    )
  )

(define init_C '()) ;; Type Constraints


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     Type reconstruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define reduce
  (lambda (op l id)
    (if (null? l)
       id
(op (car l) (reduce op (cdr l) id)) )))

(define printE
  (lambda (E)
    (error 'Help "I'm dead")
  )
)

(define isYields?
  (lambda (R)
    (cond
      ((null? R) #f)
      ((eq? '() (cdr R)) #f)
      ((eq? (cadr R) `->) #t)
      (else #f)
    )
  )
)

(define yieldsIn
  (lambda (R)
    (if (isYields? R) (car R) '())
  )
)

(define yieldsOut
  (lambda (R)
    (if (isYields? R) (caddr R) '())
  )
)

; Go through C2 until we find an entry in C2 s.t. C2 is the yieldsIn of yields.
; if we find such a match, we immediately return yieldsOut of yields.
; If no such match is found, we return an empty list.
(define findMatch
  (lambda (yields C2)
    (cond
      ((eq? C2 '()) '())
      ((null? C2) '())
      ((eq? (yieldsIn yields) (car C2)) (yieldsOut yields))
      (else (findMatch yields (cdr C2)))

    )
  )
)

(define unify
  (lambda (C1 C2)
    (display "C1: ") (display C1) (newline)
    (display "C2: ") (display C2) (newline)
    (reduce
      (lambda (a b)
         (if (eq? '() a) b (cons a b))
      )
      (map
        (lambda (a)
          (cond
            ((isYields? a)
             ; this is a yields value, so let's go ahead and parse everything in C2 that is part of it
             (if (findMatch a C2) (yieldsOut a) '())
            )
            (else '())
          )
        )
        C1
      )
      '()
    )
  )
)

(define error
  (lambda (A B)
    (newline)
    (display "ERROR: ")
    (display A) (display " -> ")
    (display B)
    (newline)
  )
)

(define TR
  (lambda (ast E C)

    ; Check if this ast is empty
    (if (null? ast)
        (cons E (cons C '())) ; Return the current list
        (begin ; Otherwise, let's dive in.
          (display (car ast))
          (cond
            ((eq? (car ast) `&apply) (begin
                                       (display "I'm apply!") (newline)
                                       (unify 
                                        (car (TR (cadr ast) E C)) ; Parse lambda
                                        (TR (caddr ast) E C) ; Parse argument
                                       )
                                     )
            )
            ((eq? (car ast) `&var) (begin
                                     (display "I'm a var!") (newline)
                                     
                                   )
            )
            ((eq? (car ast) `&const) (begin
                                       (display "I'm a const!") (newline)
                                       (cons 
                                        (cond
                                            ((eq? (cdr ast) `true) `bool)
                                            ((eq? (cdr ast) `false) `bool) 
                                            (else `int)
                                          )
                                        C
                                        )
                                     )
            )
            ((eq? (car ast) `&lambda) (begin
                                        (display "I'm a lambda!") (newline)
                                        (cons (cons `(int -> int) '()) C)
                                      )
            )
            (else (begin
                    (error 'TR "oops dropped a packet")
                  )
            )
          )

        )
    )
    
  )
) ;; YOUR CODE GOES HERE

(define TRec
  (lambda (m)
       ;;; extract type expression from compound return type
     (TR (parse m) init_E init_C)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        Sample Test cases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define M1 '((lambda (x) x) 5))
(define M2 '((lambda (x) (sub1 x)) 5))
(define M3 '((lambda (x) 1) ((lambda (x) (x x)) (lambda (x) (x x)))))
(define M4 '((lambda (x) ((and x) #f)) #t))
