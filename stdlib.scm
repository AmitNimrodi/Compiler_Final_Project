(define map
  (let ((null? null?)
	(car car) (cdr cdr)
	(cons cons) (apply apply))
    (letrec ((map-loop (lambda (f l . ls)
		     (if (null? l)
			 '() ; simplifying assumption: if l is empty, then ls is also empty
			 (if (null? ls)
			     (cons (f (car l)) (map-loop f (cdr l)))
			     (cons (apply f (car l) (map-loop car ls))
				   (apply map f (cdr l) (map-loop cdr ls))))))))
      map-loop)))

;(fold-left - 0 '(1 5 10)) ---> -16
(define fold-left
  (let ((null? null?)
        (car car) (cdr cdr)
        (apply apply))
    (letrec ((fold-left-loop (lambda (f acc l)
                              (if (null? l)
                                  acc
                                  (if (pair? l)
                                      (fold-left-loop f (apply f acc `(,(car l))) (cdr l) )
                                      (fold-left-loop f (apply f acc `(,l)) '() )
                                      )))))
    fold-left-loop)))

;(fold-right - 0 '(1 5 10)) ---> 6
(define fold-right
  (let ((null? null?)
        (car car) (cdr cdr)
        (apply apply))
    (letrec ((fold-right-loop (lambda (f acc l)
                            (if (null? l)
                                acc
                                (if (pair? l)
                                    (fold-right-loop f (apply f (car l) `(,acc)) (cdr l))
                                    (fold-right-loop f (apply f l `(,acc)) '())
                                 )))))
    fold-right-loop)))

;;tests:
;(cons* '()) ---> ()
;(cons* '(a b)) ---> (a b)
;(cons* 'a 'b 'c) ---> (a b . c)
;(cons* 'a 'b '(c d)) ---> (a b c d)
(define cons*
  (let ((null? null?)
        (car car) (cdr cdr)
        (apply apply))
    (letrec ((cons*-loop (lambda (el . ls)
                           (if (null? el)
                               '()
                               (if (null? ls)
                                   el
                                   (fold-right
                                      (lambda (x y)
                                        (if (pair? y)
                                            (append x y)
                                            (if (null? y)
                                                x
                                            (append `(,y) x))))
                                      '() (append ls el)))))))
             cons*-loop)))


(define append
  (let ((null? null?)
	(fold-right fold-right)
	(cons cons))
    (lambda args
      (fold-right (lambda (e a)
		    (if (null? a)
			e
			(fold-right cons a e)))
		  '() args))))

(define list (lambda x x))

(define list? 
  (let ((null? null?)
	(pair? pair?)
	(cdr cdr))
    (letrec ((list?-loop (lambda (x)
			   (or (null? x)
			       (and (pair? x)
				    (list? (cdr x)))))))
      list?-loop)))

(define length
  (let ((fold-left fold-left)
	(+ +))
    (lambda (l)
      (fold-left (lambda (acc e) (+ acc 1)) 0 l))))

(define make-string
  (let ((null? null?) (car car)
	(make-string make-string))
    (lambda (x . y)
      (if (null? y)
	  (make-string x #\nul)
	  (make-string x (car y))))))

(define not
  (lambda (x) (if x #f #t)))

(define number?
  (let ((float? float?)
	(integer? integer?))
    (lambda (x)
      (or (float? x) (integer? x)))))

(define +
  (let ((fold-left fold-left)
	(+ +))
    (lambda x (fold-left + 0 x))))

(define *
  (let ((fold-left fold-left)
	(* *))
    (lambda x (fold-left * 1 x))))

(define -
  (let ((apply apply)
	(- -) (+ +)
	(null? null?))
    (lambda (x . y)
      (if (null? y)
	  (- 0 x)
	  (- x (apply + y))))))

(define /
  (let ((apply apply)
	(/ /) (* *)
	(null? null?))
    (lambda (x . y)
      (if (null? y)
	  (/ 1 x)
	  (/ x (apply * y))))))

(define =
  (let ((= =) (null? null?)
	(car car) (cdr cdr)
	(apply apply))
    (letrec ((=-loop (lambda (x . y)
		       (if (null? y)
			   #t ; simplifying assumption: x is a number
			   (and (= x (car y)) (apply =-loop x (cdr y)))))))
      =-loop)))

(define <
  (let ((null? null?) (< <)
	(car car) (cdr cdr))
    (letrec ((<-loop (lambda (element lst)
		     (if (null? lst) 
			 #t 
			 (and (< element (car lst))
			     (<-loop (car lst) (cdr lst)))))))
      (lambda (x . y)
	(<-loop x y)))))

(define >
  (let ((null? null?) (< <) (= =)
	(not not) (car car) (cdr cdr))
    (letrec ((>-loop (lambda (element lst)
		     (if (null? lst) 
			 #t
			 (and (not (or
				    (< element (car lst))
				    (= element (car lst))))
			      (>-loop (car lst) (cdr lst)))))))
      (lambda (x . y)
	(>-loop x y)))))

(define zero? 
  (let ((= =))
    (lambda (x) (= x 0))))

(define string->list
  (let ((string-ref string-ref)
	(string-length string-length)
	(< <) (- -))
    (lambda (s)
      (letrec ((s->l-loop (lambda (n a)
			    (if (< n 0)
				a
				(s->l-loop (- n 1) (cons (string-ref s n) a))))))
	(s->l-loop (- (string-length s) 1) '())))))

(define equal?
  (let ((= =) (string->list string->list)
	(integer? integer?) (float? float?)
	(pair? pair?) (char? char?)
	(string? string?) (eq? eq?)
	(car car) (cdr cdr)
	(char->integer char->integer))
    (letrec ((equal?-loop (lambda (x y)
			    (or 
			     (and (integer? x) (integer? y) (= x y))
			     (and (float? x) (float? y) (= x y))
			     (and (pair? x) (pair? y) (equal?-loop (car x) (car y)) (equal?-loop (cdr x) (cdr y)))
			     (and (char? x) (char? y) (= (char->integer x) (char->integer y)))
			     (and (string? x) (string? y) (equal?-loop (string->list x) (string->list y)))
			     (eq? x y)))))
    equal?-loop)))
