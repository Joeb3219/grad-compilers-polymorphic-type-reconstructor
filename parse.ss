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
    (and (bool -> (bool -> bool))) (or (bool -> (bool -> bool)))))

(define init_C '()) ;; Type Constraints

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     Type reconstruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Okay, let's be organized this time.
;; We will use several functions, outlined here:
;; insert: Takes two arguments, list and item, and inserts item into list
;;
;; insertKeyVal: Takes three arguments, list, key, and value, and inserts key => val into list
;;
;; getKey: Takes one argument, item, and returns the key of the item
;;
;; getVal: Takes one argument, item, and return the value of the item
;;
;; inList?: Takes two arguments, list and target, and will return #t if target is in list, and #f otherwise.
;; 
;; filter: Takes two arguments, list and targets, and will return a list of items from list iff their key is in targets.
;;
;; pack: Takes three arguments, typeexp (the returned Type Expression), typeconst (the returned Type Constraints), and typevars (The type variables you'd like to keep)
;;       Will create a return list consisting of (e C_m), where C_m is only the type constraints needed in typevars

(define insert
  (lambda (list item)
    (cons item list)
  )
)

(define insertKeyVal
  (lambda (list key val)
    (insert list (cons key val))
  )
)

(define getKey
  (lambda (item)
    (car item)
  )
)

(define getVal
  (lambda (item)
    (cdr item)
  )
)

(define inList?
  (lambda (list target)
    (cond
      ((null? list) #f) ; Empty list -> #f
      ((eq? '() list) #f) ; Empty list -> #f
      ((eq? (car list) target) #t) ; Is it a match?
      (else (inList? (cdr list) target)) ; If not, check the rest of the list.
    )
  )
)

(define filter
  (lambda (list targets)
    (cond
      ((null? list) '()) ; base case, return null list
      ((eq? '() list) '()) ; base case, return null list
      ((inList? targets (getKey (car list))) (insert (filter (cdr list) targets) (car list)))
      (else (filter (cdr list) targets))
    )
  )
)
  
(define pack
  (lambda (typeexp typeconst typevars)
    (cons
      typeexp
      (filter typeconst typevars)
    )
  )
)

(define error
  (lambda (A B)
    (display "Err:") (display A) (display "->") (display B) (newline)
  )
)
  
(define TR
  (lambda (ast E C)
    (if (null? ast) '()
        ; Non null branch, we actually decode.
        (
          (lambda (label)
            ; Here we select based on what label we are viewing
            (cond
              ; APPLY
              ((eq? `&apply label) (begin
                                     (display "Apply") (newline)
                                   )
              )
              ; LAMBDA
              ((eq? `&lambda label) (begin
                                      (display "Lambda") (newline)
                                   )
              )
              ; CONST
              ((eq? `&const label) (begin
                                     (display "Const") (newline)
                                     ( (lambda (val)
                                         (pack
                                            (cond
                                              ((eq? `false val) `bool)
                                              ((eq? `true val) `bool)
                                              (else `int)
                                            )
                                            C
                                            '()
                                          )
                                        )
                                        ; Argument to val lambda.
                                        (cadr ast)
                                     )
                                   )
              )
              ; VAR
              ((eq? `&var label) (begin
                                     (display "Var") (newline)
                                    ; (
                                     ;  (lambda (
                                     ;)
                                 )
              )
              (else (error 'TR "Unable to parse label"))
            )
          )
          (car ast)
        )
    )
  )
)

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

(define N1 '(lambda (x) x))
(define N0 '1)
(define N2 '#f)

(TRec N0)
(TRec N2)
(TRec N1)