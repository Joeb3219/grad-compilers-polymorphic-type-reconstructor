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
;; findKey: Takes two arguemnts, list and key, and returns '() if key isn't in list, or its value if it is.
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

(define findKey
  (lambda (list key)
    (cond
      ((null? list) '())
      ((eq? list '()) '())
      ((eq? (getKey (car list)) key) (getVal (car list)))
      (else (findKey (cdr list) key))
    )
  )
)

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

(define findUnfulfilled
  (lambda (typeexp)
    (cond
      ((null? typeexp) '())
      ((not (pair? typeexp)) '())
      ((not (or (eq? typeexp `int) (eq? typeexp `bool))) (findUnfulfilled (cdr typeexp)))
      (else (cons (car typeexpr) (findUnfulfilled (cdr typeexp))))
    )
  )
)

(define pack
  (lambda (typeexp typeconst)
    (cons
      typeexp
      (filter typeconst (findUnfulfilled typeexp))
    )
  )
)

(define yields
  (lambda (yieldsIn yieldsOut)
    (cons yieldsIn (cons '-> (cons yieldsOut '())))
  )
)

(define yieldsIn
  (lambda (yields)
    (car yields)
  )
)

(define yieldsOut
  (lambda (yields)
    (caddr yields)
  )
)

(define getPackedTypeExpr
  (lambda (typeexpr)
    (car typeexpr)
  )
)

(define getPackedConstraints
  (lambda (typeexpr)
    (cdr typeexpr)
  )
)

(define mergeC
  (lambda (C1 C2)
    (cond
      ((null? C2) C1)
      ((eq? '() C2) C1)
      ((eq? '() (findKey C1 (getKey (car C2)))) (mergeC (insertKeyVal C1 (getKey (car C2)) (getVal (car C2))) (cdr C2)))
      (else (mergeC C1 (cdr C2)))
    )
  )
)

(define error
  (lambda (A B)
    (display "Err:") (display A) (display "->") (display B) (newline)
  )
)

(define unify
  (lambda (type1 type2 Constraints)
    (display "Being asked to unify") (display type1) (display " and ") (display type2) (newline) (display "MY constraints are ") (display Constraints) (newline)
    (cond
      ((eq? type1 type2) #t) ; If type1 and type2 are equal, return #t
      (else #f)
    )
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
                                     (
                                       (lambda (func appl)
                                         (display func) (newline)
                                         (display appl) (newline)
                                         
                                         ; We will first parse the type of the lambda
                                         (
                                          (lambda (funcReturn)
                                            (display func) (display " => ") (display funcReturn) (newline)
                                            ; We will now create a modified environment based on the returns 
                                            (
                                              (lambda (EL CL)
                                                (display "CL: ") (display CL) (newline)

                                                ; Now that we have a modified E and C, we can parse through the
                                                ; application to find its types
                                                (
                                                  (lambda (applReturn)
                                                    ; With applReturn, we now do an unification of the two to see if we can yield a result.
                                                    (
                                                      (lambda (LeftExpr RightExpr EF CF)
                                                         (
                                                           (lambda (in out)
                                                             (display "Yields decomposed: ") (display in) (display ", ") (display out) (newline)
                                                             ; Now, we must find some pairing of *in* and RightExpr that unifies.
                                                             ; If so, our result is out.
                                                             ; Otherwise, our result is failure.
                                                             (
                                                               (lambda (unifier)
                                                                 (if (eq? unifier #f)
                                                                     (error 'TR "Could not unify type expression")
                                                                     (pack
                                                                       out
                                                                       CF
                                                                     )
                                                                 )
                                                               )
                                                               (unify in RightExpr CF)
                                                             )
                                                           )
                                                           ; in
                                                           (yieldsIn LeftExpr)
                                                           ; out
                                                           (yieldsOut LeftExpr)
                                                         )
                                                      )
                                                      ; LeftExpr
                                                      (getPackedTypeExpr funcReturn)
                                                      ; RightExpr
                                                      (getPackedTypeExpr applReturn)
                                                      ; EF
                                                      EL
                                                      ; CF
                                                      (mergeC CL (getPackedConstraints applReturn))
                                                    )
                                                  )
                                                  (TR appl EL CL)
                                                )
                                                
                                              )
                                              ; EL is E with e added to it.
                                              (insertKeyVal E func (getPackedTypeExpr funcReturn))
                                              ; CL is C merged with the returned constraints added.
                                              (mergeC C (getPackedConstraints funcReturn))
                                            )
                                          )
                                          ; funcReturn
                                          (TR func E C)
                                         )
                                       )
                                       ; func
                                       (cadr ast)
                                       ; application
                                       (caddr ast)
                                    )
                                   )
              )
              ; LAMBDA
              ((eq? `&lambda label) (begin
                                      (display "Lambda") (newline)
                                      (
                                        ; Generate the type of the variable we are using
                                        (lambda (varName varTypeVar)
                                          (
                                            ; Generate the type of the expression
                                            (lambda (E1 C1 expr)
                                              (
                                                ; Now we will recurse and parse expr, using the modified environments.
                                                (lambda (exprReturn)
                                                  ; Our return type is a yielding of varTypeVar -> (getE exprReturn)
                                                  ; The returned constraints are the result of merging all constraints,
                                                  ;     and then extracting the relevant.
                                                  (pack
                                                    ; E
                                                    (yields varTypeVar (getPackedTypeExpr exprReturn))
                                                    ; C
                                                    C1
                                                  )
                                                )
                                                ; Parse expr
                                                (TR expr E1 C1)
                                              )

                                            )
                                            ; E1 is the merging of E and the type expression for the variable.
                                            (insertKeyVal E varName varTypeVar)
                                            ; C1 is still just C
                                            C
                                            ; expr
                                            (caddr ast)
                                          )
                                        )
                                       (caadr ast) (newtvar)
                                      )
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
                                     (
                                       ; Get the name of this variable
                                       (lambda (varName)
                                         (pack
                                           (if (eq? (findKey E varName) '())
                                               ; We need to generate a new variable.
                                               (newtvar)
                                               ; Otherwise, we can just look it up.
                                               (findKey E varName)
                                           )
                                           C
                                         )
                                       )
                                       (cadr ast)
                                     )
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
(Trec M1)