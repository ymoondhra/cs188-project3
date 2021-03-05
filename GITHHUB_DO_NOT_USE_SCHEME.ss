#lang racket

(define (single-term-compare x y)
  (cond
    [(equal? x y) x]
    [(and (boolean? x) (boolean? y))
     (if x '% '(not %))]
    [else `(if % ,x ,y)]))

(define (quote-compare x y)
  (if (equal? x y)
  x
  `(if % ,x ,y)))

(define (populate-var-dict x-var-names y-var-names x-var-dict y-var-dict)
  (if (and (empty? x-var-names) (empty? y-var-names))
      (list x-var-dict y-var-dict) ; if we've gone through all variables, return the dicts
      (let ([x-var (car x-var-names)] [y-var (car y-var-names)] [x-tail (cdr x-var-names)] [y-tail (cdr y-var-names)])
        (if (equal? x-var y-var)
            (populate-var-dict x-tail y-tail x-var-dict y-var-dict) ; if they're equal, don't add entries to the dictionaries
            (populate-var-dict x-tail y-tail (dict-set x-var-dict x-var y-var)(dict-set y-var-dict y-var x-var))))))

(define (populate-var-dict-single-vals x-var y-var)
  (if (equal? x-var y-var)
      (list #hash() #hash())
      (list (dict-set #hash() x-var y-var) (dict-set #hash() y-var x-var))))

(define (lambda-compare x y)
  (let ([x-var-names (cadr x)] [y-var-names (cadr y)])
    (cond
      [(or (and (pair?  x-var-names) (not (list? x-var-names))) (and (pair?  y-var-names) (not (list? y-var-names)))) ; checks for improper lists
       (quote-compare x y)]
      [(xor (list? x-var-names) (list? y-var-names)) ; checks for single value and list combo
       `(if % ,x ,y)]
      [(and (list? x-var-names) (list? y-var-names) (not (equal? (length x-var-names) (length y-var-names)))) ; checks for different # of args
       `(if % ,x ,y)]
      [else
       (let ([lambda-form
              (if (not (equal? (car x) (car y))) ; if at least one phrase used the symbol version, then use that for both versions
                  'λ
                  (car x))]
             [var-dicts (if (and (list? x-var-names) (list? y-var-names))
                            (populate-var-dict x-var-names y-var-names '#hash() '#hash())
                            (populate-var-dict-single-vals x-var-names y-var-names))])           
         (cons lambda-form (lambda-body-compare (cdr x) (cdr y) (car var-dicts) (cadr var-dicts))))])))

(define (lambda-body-compare x y x-var-dict y-var-dict)
  (cond
    [(or (empty? x) (empty? y)) ; if either x or y is empty
     (quote-compare x y)]
    [(or (not (pair? x)) (not (pair? y))) ; if x, y, or both is a single value
     (lambda-single-term-compare x y x-var-dict y-var-dict)]
    [(or (and (pair?  x) (not (list? x))) (and (pair?  y) (not (list? y)))) ; if x, y, or both is an improper list
     (quote-compare x y)]
    [(not (equal? (length x) (length y))) ; if x and y are lists of different lengths
     (lambda-single-term-compare x y x-var-dict y-var-dict)]
    [else
     (let ([x-head (car x)] [y-head (car y)] [x-tail (cdr x)] [y-tail (cdr y)])
       (cond
         [(and (pair? x-head) (pair? y-head)) ;if the heads are pairs
          (cons (lambda-body-compare x-head y-head x-var-dict y-var-dict) (lambda-body-compare x-tail y-tail x-var-dict y-var-dict))]
         [else ;if the heads are just normal atoms
          (cond
            [(and (equal? x-head 'quote) (equal? y-head 'quote)) ; if the head is a quote, then just treat as data
             (append (quote-compare `',(car x-tail) `',(car y-tail)) (lambda-body-compare (cdr x-tail) (cdr y-tail) x-var-dict y-var-dict))]
            [(xor (equal? x-head 'if) (equal? y-head 'if)) ; if only one of the heads is an 'if'
             (lambda-single-term-compare x y x-var-dict y-var-dict)]
            [(and (or (equal? x-head 'lambda) (equal? x-head 'λ)) (or (equal? y-head 'lambda) (equal? y-head 'λ))) ; if the head is lambda or λ
              (lambda-compare x y)]
            [else (cons (lambda-single-term-compare x-head y-head x-var-dict y-var-dict) (lambda-body-compare x-tail y-tail x-var-dict y-var-dict))])]))]))

(define (lambda-single-term-compare x y x-var-dict y-var-dict)
  (display x)
  (display "\n")
  (cond
    [(and (boolean? x) (boolean? y))
     (if (equal? x y)
         x
         (if x '% '(not %)))]
    [else
     (let ([x-mapped (dict-ref x-var-dict x #f)] [y-mapped (dict-ref y-var-dict y #f)])
       (cond
         [(and (equal? x-mapped y) (equal? y-mapped x)) ; x and y map to each other
          (combine-terms x y)]
         [(and x-mapped y-mapped) ; both x and y mapped to something
          `(if % ,(combine-terms x x-mapped) ,(combine-terms y-mapped y))]
         [x-mapped ; only x mapped to something
          `(if % ,(combine-terms x x-mapped) ,y)]
         [y-mapped ; only y mapped to something
          `(if % ,x ,(combine-terms y-mapped y))]
         [(equal? x y) x]
         [else `(if % ,x ,y)]))]))
         
(define (combine-terms x y)
  (string->symbol (string-append (symbol->string x) "!" (symbol->string y))))

(define (expr-compare x y)
  (cond
    [(or (empty? x) (empty? y)) ; if either x or y is empty
     (quote-compare x y)]
    [(or (not (pair? x)) (not (pair? y))) ; if x, y, or both is a single value
     (single-term-compare x y)]
    [(or (and (pair?  x) (not (list? x))) (and (pair?  y) (not (list? y)))) ; if x, y, or both is an improper list
     (quote-compare x y)]
    [(not (equal? (length x) (length y))) ; if x and y are lists of different lengths
     `(if % ,x ,y)]
    [else
     (let ([x-head (car x)] [y-head (car y)] [x-tail (cdr x)] [y-tail (cdr y)])
       (cond
         [(and (pair? x-head) (pair? y-head)) ;if the heads are pairs
          (cons (expr-compare x-head y-head) (expr-compare x-tail y-tail))]
         [else ;if the heads are just normal atoms
          (cond
            [(and (equal? x-head 'quote) (equal? y-head 'quote)) ; if the head is a quote, then just treat as data
             (append (quote-compare `',(car x-tail) `',(car y-tail)) (expr-compare (cdr x-tail) (cdr y-tail)))]
            [(xor (equal? x-head 'if) (equal? y-head 'if)) ; if only one of the heads is an 'if'
             (single-term-compare x y)]
            [(and (or (equal? x-head 'lambda) (equal? x-head 'λ)) (or (equal? y-head 'lambda) (equal? y-head 'λ))) ; if the head is lambda or λ
             (lambda-compare x y)]
            [else (cons (single-term-compare x-head y-head) (expr-compare x-tail y-tail))])]))]))

(define (test-expr-compare x y)
  (and
   (equal? (eval (list 'let '((% #t)) (expr-compare x y))) (eval x))
   (equal? (eval (list 'let '((% #f)) (expr-compare x y))) (eval y))))



(expr-compare '((λ (a b) (f a b)) 1 2)
              '((λ (a c) (f c a)) 1 2))
(display ''((λ (a b!c) (f (if % a b!c) (if % b!c a))) 1 2))