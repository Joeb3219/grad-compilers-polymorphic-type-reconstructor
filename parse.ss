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

(define newYields
  (lambda ()
    (cons (newtvar) (cons `-> (newtvar)))
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

; Searches for K._ and returns _ in L.
(define search
  (lambda (L k)
    (cond
      ((null? L) '())
      ((eq? K (caar L)) (cdar L))
      (else (search (cdr L) K))
    )
  )
)

; Inserts only if K isn't already in L.
(define insNew
  (lambda (L K V)
    (if (eq? '() (search L K)) (insert L K V) L)
  )
)

; Inserts the pair K.V into L.
(define insert
  (lambda (L K V)
    (cons (cons K V) L)
  )
)

(define getE
  (lambda (P)
    (car P)
  )
)

(define getC
  (lambda (P)
    (cdr P)
  )
)

(define pack
  (lambda (E C)
    (cons E C)
  )
)

; If L is currently in E, will just return E
; Otherwise, will generate a new environment variable and associate it with L, inserting into E.
(define eIns
  (lambda (E L)
    (if (eq? (search E L) '()) (insert E L (newtvar)) E)
  )
)

(define eInsYields
  (lambda (E L)
    (if (eq? (search E L) '()) (insert E L (newtvar)) E)
  )
)


(define cIns
  (lambda (C L T)
    (if (eq? (search C L) '()) (insert C L T) C)
  )
)

(define inList?
  (lambda (L I)
    (cond
      ((null? L) #f)
      ((eq? (car L) I) #t)
      (else #f)
    )
  )
)

(define merge
  (lambda (A B)
    (reduce (lambda (N C)
                            (if (inList? N C) N (cons N C))
                          ) A B)
  )
)

(define mergeReturn
  (lambda (A B)
    (pack
      (merge (getE A) (getE B))
      (merge (getC A) (getC B))
    )
  )
)

; Executes F and merges the result into P.
; Returns the packed return type that results.
(define execAndMerge
  (lambda (P F)
    (mergeReturn P (F))
  )
)

; Returns in format of (typeExpression constraints)
; type expression is in terms of type variables, ie x -> beta
; the constraints map a type variable to their definition
; as an example, if there are (x -> alpha) and (y -> beta) in E,
; and (x -> int), (x -> (int -> bool)), and (y -> int) in C,
; There are several possible configurations to return: 
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
                                        (TR (cadr ast) E C) ; Parse lambda
                                        (TR (caddr ast) E C) ; Parse argument
                                       )
                                     )
            )
            ((eq? (car ast) `&var) (begin
                                     (display "I'm a var!") (newline)
                                     (pack
                                      ; First, let's add a definition for the variable to our E
                                      (eInsYields E (cadr ast))
                                      ; Now, we have no idea what its type is, so we leave the type as C.
                                      C
                                     )
                                   )
            )
            ((eq? (car ast) `&const) (begin
                                       (display "I'm a const!") (newline)
                                       (pack
                                         ; Add our e to this if it doesn't exist yet.
                                         (eIns E (cadr ast))
                                         ; Add our c to this if it doesn't exist it
                                         (cIns C (cadr ast)
                                               (cond
                                                 ((eq? `false (cadr ast)) `bool)
                                                 ((eq? `true (cadr ast)) `true)
                                                 (else `int)
                                               )
                                         )
                                       )
                                     )
            )
            ((eq? (car ast) `&lambda) (begin
                                        (display "I'm a lambda!")
                                        (display ast) (newline)
                                        (mergeReturn
                                          (pack
                                            ; First, we compute E by creating an aggregate of the whole function type,
                                            ;
                                            (eIns E ast)
                                            (cIns C (ast) 
                                          )
                                         )
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


;; Basic testing commands
;; (search (getE (TRec '6)) '6) -> int