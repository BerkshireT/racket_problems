#lang racket
(define applytoall2 ; 5.10.3
  (lambda (func lst)
    (letrec ((applyhelp
            (lambda (lst)
              (cond
                ((null? lst) '())
                (else (cons (func (car lst)) (applyhelp (cdr lst))))))))
    (applyhelp lst))))



(define flatten ; 5.10.4
  (lambda (lst)
    (cond
      ((null? lst) '())
      ((list? (car lst)) (append (flatten (car lst)) (flatten (cdr lst)))) ; found list
      (else (cons (car lst) (flatten (cdr lst))))))) ; found atom



(define path ; 5.7.2
  (lambda (n bst)
    (letrec ((pathhelp
              (lambda (bst curpath)
                (cond
                  ((null? bst) (cons 'notfound curpath))
                  ((eq? n (car bst)) curpath) ; found number
                  (else (checkpaths
                         (cons 'left (pathhelp (cadr bst) curpath))
                         (cons 'right (pathhelp (caddr bst) curpath)))))))) ; empty
      (pathhelp bst '()))))

(define checkpaths ; Verifies there is a path
  (lambda (p1 p2)
    (cond
      ((not (member 'notfound p1)) p1)
      ((not (member 'notfound p2)) p2)
      (else (cons 'notfound '())))))



(define numsort ; 5.10.7
  (lambda (lst)
    (letrec ((numsorthelp
              (lambda (lst)
                (cond
                  ((null? lst) '())
                  ((null? (cdr lst)) lst) ; only one item
                  (else
                   (mergelists (numsorthelp (enumbers lst)) (numsorthelp (onumbers lst))))))))
      (numsorthelp lst))))

(define enumbers ; separate even positions
  (lambda (lst)
    (cond
    ((null? lst) '())
    ((null? (cdr lst)) '())
    (else (cons (car (cdr lst)) (enumbers (cdr (cdr lst))))))))

(define onumbers ; separate odd positions
  (lambda (lst)
    (cond
      ((null? lst) '())
      ((null? (cdr lst)) (list (car lst)))
      (else (cons (car lst) (onumbers (cdr (cdr lst))))))))

(define mergelists ; merges lists that are in increasing order
  (lambda (lst1 lst2)
    (cond
      ((null? lst1) lst2)
      ((null? lst2) lst1)
      ((< (car lst1) (car lst2))
       (cons (car lst1) (mergelists (cdr lst1) lst2)))
      (else
       (cons (car lst2) (mergelists (cdr lst2) lst1))))))



(define compsort ; 5.10.9
  (lambda (comp lst)
    (letrec ((compsorthelp
              (lambda (lst)
                (cond
                  ((null? lst) '())
                  ((null? (cdr lst)) lst) ; only one item
                  ((eq? comp <) (mergelists (compsorthelp (enumbers lst)) (compsorthelp (onumbers lst))))
                  (else (decmergelists (compsorthelp (enumbers lst)) (compsorthelp (onumbers lst))))))))
      (compsorthelp lst))))

(define decmergelists ; merges lists that are in order of the comp
  (lambda (lst1 lst2)
    (cond
      ((null? lst1) lst2)
      ((null? lst2) lst1)
      ((> (car lst1) (car lst2))
       (cons (car lst1) (decmergelists (cdr lst1) lst2)))
      (else
       (cons (car lst2) (decmergelists (cdr lst2) lst1))))))



(define (compose p1 p2) ; handout a
  (lambda (x)
    (p1 (p2 x))))




(define car&cdr ; handout b
  (lambda (s slist err)
    (letrec ((help
              (lambda (lst)
                (cond
                  ((null? lst) err)
                  ((eq? s (car lst)) 'car)
                  ((pair? (car lst)) ; car of list is a list
                   (append (list 'compose (help (car lst))) '(car))) 
                  (else (checkcomp
                   (append (list 'compose (help (cdr lst))) '(cdr))
                   err))))))
      (help slist))))

(define checkcomp ; Verifies there is a composition
  (lambda (c1 err)
    (cond
      ((not (member err c1)) c1)
      (else err))))