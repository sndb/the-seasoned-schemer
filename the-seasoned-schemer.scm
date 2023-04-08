(define-syntax letcc
  (syntax-rules ()
    ((letcc var body ...)
     (call-with-current-continuation
      (lambda (var) body ...)))))

(define-syntax try
  (syntax-rules ()
    ((try var a . b)
     (letcc success
       (letcc var (success a)) . b))))

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (eq? a (car lat))
               (member? a (cdr lat)))))))

(define is-first?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (eq? (car lat) a)))))

(define two-in-a-row?
  (lambda (lat)
    (cond
     ((null? lat) #f)
     (else (or (is-first? (car lat) (cdr lat))
               (two-in-a-row? (cdr lat)))))))

(define two-in-a-row?
  (lambda (lat)
    (cond
     ((null? lat) #f)
     (else (is-first-b? (car lat) (cdr lat))))))

(define is-first-b?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (eq? (car lat) a)
               (two-in-a-row? lat))))))

(define two-in-a-row-b?
  (lambda (preceding lat)
    (cond
     ((null? lat) #f)
     (else (or (eq? (car lat) preceding)
               (two-in-a-row-b? (car lat) (cdr lat)))))))

(define two-in-a-row?
  (lambda (lat)
    (cond
     ((null? lat) #f)
     (else (two-in-a-row-b? (car lat) (cdr lat))))))

(define sum-of-prefixes-b
  (lambda (sonssf tup)
    (cond
     ((null? tup) '())
     (else (cons (+ sonssf (car tup))
                 (sum-of-prefixes-b (+ sonssf (car tup))
                                    (cdr tup)))))))

(define sum-of-prefixes
  (lambda (tup)
    (sum-of-prefixes-b 0 tup)))

(define one? (lambda (x) (= x 1)))
(define add1 (lambda (x) (+ x 1)))
(define sub1 (lambda (x) (- x 1)))

(define pick
  (lambda (n lat)
    (cond
     ((one? n) (car lat))
     (else (pick (sub1 n) (cdr lat))))))

(define scramble-b
  (lambda (tup rev-pre)
    (cond
     ((null? tup) '())
     (else (cons (pick (car tup)
                       (cons (car tup) rev-pre))
                 (scramble-b (cdr tup)
                             (cons (car tup) rev-pre)))))))

(define scramble
  (lambda (tup)
    (scramble-b tup '())))

(define multirember
  (lambda (a lat)
    ((Y (lambda (mr)
          (lambda (lat)
            (cond
             ((null? lat) '())
             ((eq? a (car lat)) (mr (cdr lat)))
             (else (cons (car lat) (mr (cdr lat))))))))
     lat)))

(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

(define multirember
  (lambda (a lat)
    ((letrec
         ((mr (lambda (lat)
                (cond
                 ((null? lat) '())
                 ((eq? a (car lat)) (mr (cdr lat)))
                 (else (cons (car lat) (mr (cdr lat))))))))
       mr)
     lat)))

(define multirember
  (lambda (a lat)
    (letrec
        ((mr (lambda (lat)
               (cond
                ((null? lat) '())
                ((eq? a (car lat)) (mr (cdr lat)))
                (else (cons (car lat) (mr (cdr lat))))))))
      (mr lat))))

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
       ((null? l) '())
       ((test? (car l) a) (cdr l))
       (else (cons (car l) ((rember-f test?) a (cdr l))))))))

(define rember-eq? (rember-f eq?))

(define multirember-f
  (lambda (test?)
    (lambda (a l)
      (cond
       ((null? l) '())
       ((test? (car l) a) ((multirember-f test?) a (cdr l)))
       (else (cons (car l) ((multirember-f test?) a (cdr l))))))))

(define multirember-f
  (lambda (test?)
    (letrec
        ((m-f (lambda (a lat)
                (cond
                 ((null? lat) '())
                 ((test? (car lat) a) (m-f a (cdr lat)))
                 (else (cons (car lat) (m-f a (cdr lat))))))))
      m-f)))

(define union
  (lambda (set1 set2)
    (cond
     ((null? set1) set2)
     ((member? (car set1) set2) (union (cdr set1) set2))
     (else (cons (car set1) (union (cdr set1) set2))))))

(define union
  (lambda (set1 set2)
    (letrec
        ((U (lambda (set)
              (cond
               ((null? set) set2)
               ((member? (car set) set2) (U (cdr set)))
               (else (cons (car set) (U (cdr set))))))))
      (U set1))))

(define union
  (lambda (set1 set2)
    (letrec
        ((U (lambda (set)
              (cond
               ((null? set) set2)
               ((member? (car set) set2) (U (cdr set)))
               (else (cons (car set) (U (cdr set)))))))
         (member? (lambda (a lat)
                    (cond
                     ((null? lat) #f)
                     ((eq? (car lat) a) #t)
                     (else (member? a (cdr lat)))))))
      (U set1))))

(define union
  (lambda (set1 set2)
    (letrec
        ((U (lambda (set)
              (cond
               ((null? set) set2)
               ((M? (car set) set2) (U (cdr set)))
               (else (cons (car set) (U (cdr set)))))))
         (M? (lambda (a lat)
               (cond
                ((null? lat) #f)
                ((eq? (car lat) a) #t)
                (else (M? a (cdr lat)))))))
      (U set1))))

(define union
  (lambda (set1 set2)
    (letrec
        ((U (lambda (set)
              (cond
               ((null? set) set2)
               ((M? (car set) set2) (U (cdr set)))
               (else (cons (car set) (U (cdr set)))))))
         (M? (lambda (a lat)
               (letrec
                   ((N? (lambda (lat)
                          (cond
                           ((null? lat) #f)
                           ((eq? (car lat) a) #t)
                           (else (N? (cdr lat)))))))
                 (N? lat)))))
      (U set1))))

(define two-in-a-row?
  (lambda (lat)
    (letrec
        ((W (lambda (preceding lat)
              (cond
               ((null? lat) #f)
               (else (or (eq? (car lat) preceding)
                         (W (car lat) (cdr lat))))))))
      (cond
       ((null? lat) #f)
       (else (W (car lat) (cdr lat)))))))

(define two-in-a-row?
  (letrec
      ((W (lambda (a lat)
            (cond
             ((null? lat) #f)
             (else (or (eq? (car lat) a)
                       (W (car lat) (cdr lat))))))))
    (lambda (lat)
      (cond
       ((null? lat) #f)
       (else (W (car lat) (cdr lat)))))))

(define sum-of-prefixes
  (lambda (tup)
    (letrec
        ((S (lambda (sss tup)
              (cond
               ((null? tup) '())
               (else (cons (+ sss (car tup))
                           (S (+ sss (car tup)) (cdr tup))))))))
      (S 0 tup))))

(define scramble
  (lambda (tup)
    (letrec
        ((P (lambda (tup rp)
              (cond
               ((null? tup) '())
               (else (cons (pick (car tup)
                                 (cons (car tup) rp))
                           (P (cdr tup)
                              (cons (car tup) rp))))))))
      (P tup '()))))

(define intersect
  (lambda (set1 set2)
    (letrec
        ((I (lambda (set)
              (cond
               ((null? set) '())
               ((member? (car set) set2)
                (cons (car set) (I (cdr set))))
               (else (I (cdr set)))))))
      (I set1))))

(define intersectall
  (lambda (lset)
    (cond
     ((null? lset) '())
     ((null? (cdr lset)) (car lset))
     (else (intersect (car lset) (intersectall (cdr lset)))))))

(define intersectall
  (lambda (lset)
    (letrec
        ((A (lambda (lset)
              (cond
               ((null? (cdr lset)) (car lset))
               (else (intersect (car lset) (A (cdr lset))))))))
      (cond
       ((null? lset) '())
       (else (A lset))))))

(define intersectall
  (lambda (lset)
    (letcc hop
      (letrec
          ((A (lambda (lset)
                (cond
                 ((null? (car lset)) (hop '()))
                 ((null? (cdr lset)) (car lset))
                 (else (intersect (car lset) (A (cdr lset))))))))
        (cond
         ((null? lset) '())
         (else (A lset)))))))

(define intersectall
  (lambda (lset)
    (call/cc
     (lambda (hop)
       (letrec
           ((A (lambda (lset)
                 (cond
                  ((null? (car lset)) (hop '()))
                  ((null? (cdr lset)) (car lset))
                  (else (intersect (car lset) (A (cdr lset))))))))
         (cond
          ((null? lset) '())
          (else (A lset))))))))

(define intersect
  (lambda (set1 set2)
    (letrec
        ((I (lambda (set1)
              (cond
               ((null? set1) '())
               ((member? (car set1) set2)
                (cons (car set1) (I (cdr set1))))
               (else (I (cdr set1)))))))
      (cond
       ((null? set2) '())
       (else (I set1))))))

(define intersectall
  (lambda (lset)
    (letcc hop
      (letrec
          ((A (lambda (lset)
                (cond
                 ((null? (car lset)) (hop '()))
                 ((null? (cdr lset)) (car lset))
                 (else (I (car lset) (A (cdr lset)))))))
           (I (lambda (s1 s2)
                (letrec
                    ((J (lambda (s1)
                          (cond
                           ((null? s1) '())
                           ((member? (car s1) s2)
                            (cons (car s1) (J (cdr s1))))
                           (else (J (cdr s1)))))))
                  (cond
                   ((null? s2) (hop '()))
                   (else (J s1)))))))
        (cond
         ((null? lset) '())
         (else (A lset)))))))

(define rember
  (lambda (a lat)
    (letrec
        ((R (lambda (lat)
              (cond
               ((null? lat) '())
               ((eq? (car lat) a) (cdr lat))
               (else (cons (car lat) (R (cdr lat))))))))
      (R lat))))

(define rember-beyond-first
  (lambda (a lat)
    (letrec
        ((R (lambda (lat)
              (cond
               ((null? lat) '())
               ((eq? (car lat) a) '())
               (else (cons (car lat) (R (cdr lat))))))))
      (R lat))))

(define rember-upto-last
  (lambda (a lat)
    (letcc skip
      (letrec
          ((R (lambda (lat)
                (cond
                 ((null? lat) '())
                 ((eq? (car lat) a) (skip (R (cdr lat))))
                 (else (cons (car lat) (R (cdr lat))))))))
        (R lat)))))

(define leftmost
  (lambda (l)
    (cond
     ((atom? (car l)) (car l))
     (else (leftmost (car l))))))

(define leftmost
  (lambda (l)
    (cond
     ((null? l) '())
     ((atom? (car l)) (car l))
     (else (let ((a (leftmost (car l))))
             (cond
              ((atom? a) a)
              (else (leftmost (cdr l)))))))))

(define equal?
  (lambda (s1 s2)
    (cond
     ((and (atom? s1) (atom? s2)) (eq? s1 s2))
     ((or (atom? s1) (atom? s2)) #f)
     (else (eqlist? s1 s2)))))

(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     (else (and (equal? (car l1) (car l2))
                (eqlist? (cdr l1) (cdr l2)))))))

(define rember1*
  (lambda (a l)
    (letrec
        ((R (lambda (l)
              (cond
               ((null? l) '())
               ((atom? (car l))
                (cond
                 ((eq? (car l) a) (cdr l))
                 (else (cons (car l) (R (cdr l))))))
               (else
                (let ((av (R (car l))))
                  (cond
                   ((eqlist? (car l) av)
                    (cons (car l) (R (cdr l))))
                   (else (cons av (cdr l))))))))))
      (R l))))

(define depth*
  (lambda (l)
    (cond
     ((null? l) 1)
     ((atom? (car l)) (depth* (cdr l)))
     (else
      (let ((a (add1 (depth* (car l))))
            (d (depth* (cdr l))))
        (cond
         ((> d a) d)
         (else a)))))))

(define depth*
  (lambda (l)
    (cond
     ((null? l) 1)
     (else
      (let ((d (depth* (cdr l))))
        (cond
         ((atom? (car l)) d)
         (else
          (let ((a (add1 (depth* (car l)))))
            (cond
             ((> d a) d)
             (else a))))))))))

(define max
  (lambda (n m)
    (if (> n m) n m)))

(define depth*
  (lambda (l)
    (cond
     ((null? l) 1)
     ((atom? (car l)) (depth* (cdr l)))
     (else (max (add1 (depth* (car l)))
                (depth* (cdr l)))))))

(define scramble
  (lambda (tup)
    (letrec
        ((P (lambda (tup rp)
              (cond
               ((null? tup) '())
               (else
                (let ((rp (cons (car tup) rp)))
                  (cons (pick (car tup) rp)
                        (P (cdr tup) rp))))))))
      (P tup '()))))

(define leftmost
  (lambda (l)
    (letcc skip
      (lm l skip))))

(define lm
  (lambda (l out)
    (cond
     ((null? l) '())
     ((atom? (car l)) (out (car l)))
     (else (begin
             (lm (car l) out)
             (lm (cdr l) out))))))

(define leftmost
  (letrec
      ((lm (lambda (l out)
             (cond
              ((null? l) '())
              ((atom? (car l)) (out (car l)))
              (else (begin
                      (lm (car l) out)
                      (lm (cdr l) out)))))))
    (lambda (l)
      (letcc skip
        (lm l skip)))))

(define leftmost
  (lambda (l)
    (letcc skip
      (letrec
          ((lm (lambda (l)
                 (cond
                  ((null? l) '())
                  ((atom? (car l)) (skip (car l)))
                  (else (begin
                          (lm (car l))
                          (lm (cdr l))))))))
        (lm l)))))

;;; rm: letcc variant
(define rm
  (lambda (a l oh)
    (cond
     ((null? l) (oh 'no))
     ((atom? (car l))
      (if (eq? (car l) a)
          (cdr l)
          (cons (car l) (rm a (cdr l) oh))))
     (else
      (let ((new-car
             (letcc oh
               (rm a (car l) oh))))
        (if (atom? new-car)
            (cons (car l) (rm a (cdr l) oh))
            (cons new-car (cdr l))))))))

;;; rm: call/cc variant
(define rm
  (lambda (a l oh)
    (cond
     ((null? l) (oh 'no))
     ((atom? (car l))
      (if (eq? (car l) a)
          (cdr l)
          (cons (car l) (rm a (cdr l) oh))))
     (else
      (let ((new-car (call/cc
                      (lambda (oh)
                        (rm a (car l) oh)))))
        (if (atom? new-car)
            (cons (car l) (rm a (cdr l) oh))
            (cons new-car (cdr l))))))))

;;; rember1*: interface to rm
(define rember1*
  (lambda (a l)
    (let ((new-l (letcc oh (rm a l oh))))
      (if (atom? new-l) l new-l))))

;;; rember1* and rm: try version
(define rember1*
  (lambda (a l)
    (try oh (rm a l oh) l)))

(define rm
  (lambda (a l oh)
    (cond
     ((null? l) (oh 'no))
     ((atom? (car l))
      (if (eq? (car l) a)
          (cdr l)
          (cons (car l) (rm a (cdr l) oh))))
     (else
      (try oh2
        (cons (rm a (car l) oh2) (cdr l))
        (cons (car l) (rm a (cdr l) oh)))))))

(define x (cons 'chicago (cons 'pizza '())))
(set! x 'gone)
(set! x 'skins)

(define gourmet
  (lambda (food)
    (cons food (cons x '()))))

(set! x 'rings)

(define gourmand
  (lambda (food)
    (set! x food)
    (cons food (cons x '()))))

(define dinerR
  (lambda (food)
    (set! x food)
    (cons 'milkshake (cons food '()))))

(define omnivore
  (let ((x 'minestrone))
    (lambda (food)
      (set! x food)
      (cons food (cons x '())))))

(define gobbler
  (let ((x 'minestrone))
    (lambda (food)
      (set! x food)
      (cons food (cons x '())))))

(define nibbler
  (lambda (food)
    (let ((x 'donut))
      (set! x food)
      (cons food (cons x '())))))

(define food 'none)

(define glutton
  (lambda (x)
    (set! food x)
    (cons 'more (cons x (cons 'more (cons x '()))))))

(define chez-nous
  (lambda ()
    (let ((a food))
      (set! food x)
      (set! x a))))

(define sweet-tooth
  (lambda (food)
    (cons food (cons 'cake '()))))

(define last 'angelfood)

(define sweet-toothL
  (lambda (food)
    (set! last food)
    (cons food (cons 'cake '()))))

(define ingredients '())

(define sweet-toothR
  (lambda (food)
    (set! ingredients (cons food ingredients))
    (cons food (cons 'cake '()))))

(define deep
  (lambda (m)
    (cond
     ((zero? m) 'pizza)
     (else (cons (deepM (sub1 m)) '())))))

(define find
  (lambda (n Ns Rs)
    (letrec
        ((A (lambda (ns rs)
              (cond
               ((null? ns) #f)
               ((= (car ns) n) (car rs))
               (else (A (cdr ns) (cdr rs)))))))
      (A Ns Rs))))

(define deepM
  (let ((Rs '())
        (Ns '()))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result (deep n)))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))

(define length
  (lambda (l)
    (cond
     ((null? l) 0)
     (else (add1 (length (cdr l)))))))

(define length
  (let ((h (lambda (l) 0)))
    (set! h
          (lambda (l)
            (cond
             ((null? l) 0)
             (else (add1 (h (cdr l)))))))
    h))

(define L
  (lambda (length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

(define length
  (let ((h (lambda (l) 0)))
    (set! h
          (L (lambda (arg) (h arg))))
    h))

(define Y!
  (lambda (L)
    (let ((h (lambda (l) '())))
      (set! h (L (lambda (arg) (h arg))))
      h)))

(define Y-bang
  (lambda (f)
    (letrec
        ((h (f (lambda (arg) (h arg)))))
      h)))

(define length (Y! L))

(define D
  (lambda (depth*)
    (lambda (l)
      (cond
       ((null? l) 1)
       ((atom? (car l)) (depth* (cdr l)))
       (else (max (add1 (depth* (car l)))
                  (depth* (cdr l))))))))

(define biz
  (let ((x 0))
    (lambda (f)
      (set! x (add1 x))
      (lambda (a)
        (if (= a x)
            0
            (f a))))))

(define deepM
  (let ((Rs '())
        (Ns '()))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result
                   (if (zero? n)
                       'pizza
                       (consC (deepM (sub1 n)) '()))))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))

(define deep
  (lambda (m)
    (if (zero? m)
        'pizza
        (consC (deep (sub1 m)) '()))))

(define counter)
(define set-counter)

(define consC
  (let ((N 0))
    (set! counter (lambda () N))
    (set! set-counter (lambda (x) (set! N x)))
    (lambda (x y)
      (set! N (add1 N))
      (cons x y))))

(define supercounter
  (lambda (f)
    (letrec
        ((S (lambda (n)
              (if (zero? n)
                  (f n)
                  (begin
                    (f n)
                    (S (sub1 n)))))))
      (S 1000)
      (counter))))

(define rember1*
  (lambda (a l)
    (letrec
        ((R (lambda (l oh)
              (cond
               ((null? l) (oh 'no))
               ((atom? (car l))
                (if (eq? (car l) a)
                    (cdr l)
                    (cons (car l) (R (cdr l) oh))))
               (else
                (let ((new-car (letcc oh (R (car l) oh))))
                  (if (atom? new-car)
                      (cons (car l) (R (cdr l) oh))
                      (cons new-car (cdr l)))))))))
      (let ((new-l (letcc oh (R l oh))))
        (if (atom? new-l) l new-l)))))

(define rember1*C
  (lambda (a l)
    (letrec
        ((R (lambda (l oh)
              (cond
               ((null? l) (oh 'no))
               ((atom? (car l))
                (if (eq? (car l) a)
                    (cdr l)
                    (consC (car l) (R (cdr l) oh))))
               (else
                (let ((new-car (letcc oh (R (car l) oh))))
                  (if (atom? new-car)
                      (consC (car l) (R (cdr l) oh))
                      (consC new-car (cdr l)))))))))
      (let ((new-l (letcc oh (R l oh))))
        (if (atom? new-l) l new-l)))))

(define rember1*
  (lambda (a l)
    (letrec
        ((R (lambda (l)
              (cond
               ((null? l) '())
               ((atom? (car l))
                (if (eq? (car l) a)
                    (cdr l)
                    (cons (car l) (R (cdr l)))))
               (else
                (let ((av (R (car l))))
                  (if (eqlist? (car l) av)
                      (cons (car l) (R (cdr l)))
                      (cons av (cdr l)))))))))
      (R l))))

(define rember1*C2
  (lambda (a l)
    (letrec
        ((R (lambda (l)
              (cond
               ((null? l) '())
               ((atom? (car l))
                (if (eq? (car l) a)
                    (cdr l)
                    (consC (car l) (R (cdr l)))))
               (else
                (let ((av (R (car l))))
                  (if (eqlist? (car l) av)
                      (consC (car l) (R (cdr l)))
                      (consC av (cdr l)))))))))
      (R l))))

(define lots
  (lambda (m)
    (cond
     ((zero? m) '())
     (else (kons 'egg (lots (sub1 m)))))))

(define lenkth
  (lambda (l)
    (cond
     ((null? l) 0)
     (else (add1 (lenkth (kdr l)))))))

(define add-at-end
  (lambda (l)
    (cond
     ((null? (kdr l))
      (konsC (kar l) (kons 'egg '())))
     (else (konsC (kar l) (add-at-end (kdr l)))))))

(define add-at-end-too
  (lambda (l)
    (letrec
        ((A (lambda (ls)
              (cond
               ((null? (kdr ls))
                (set-kdr ls (kons 'egg '())))
               (else (A (kdr ls)))))))
      (A l)
      l)))

(define kons
  (lambda (kar kdr)
    (lambda (selector)
      (selector kar kdr))))

(define kar
  (lambda (c)
    (c (lambda (a d) a))))

(define kdr
  (lambda (c)
    (c (lambda (a d) d))))

(define bons
  (lambda (kar)
    (let ((kdr '()))
      (lambda (selector)
        (selector
         (lambda (x) (set! kdr x))
         kar
         kdr)))))

(define kar
  (lambda (c)
    (c (lambda (s a d) a))))

(define kdr
  (lambda (c)
    (c (lambda (s a d) d))))

(define set-kdr
  (lambda (c x)
    ((c (lambda (s a d) s)) x)))

(define kons
  (lambda (a d)
    (let ((c (bons a)))
      (set-kdr c d)
      c)))

(define kounter)
(define set-kounter)

(define konsC
  (let ((N 0))
    (set! kounter (lambda () N))
    (set! set-kounter (lambda (x) (set! N x)))
    (lambda (a d)
      (set! N (add1 N))
      (kons a d))))

(define dozen (lots 12))
(define bakers-dozen (add-at-end dozen))
(define bakers-dozen-too (add-at-end-too dozen))
(define bakers-dozen-again (add-at-end dozen))

(define eklist?
  (lambda (ls1 ls2)
    (cond
     ((null? ls1) (null? ls2))
     ((null? ls2) #f)
     (else
      (and (eq? (kar ls1) (kar ls2))
           (eklist? (kdr ls1) (kdr ls2)))))))

(define same?
  (lambda (c1 c2)
    (let ((t1 (kdr c1))
          (t2 (kdr c2)))
      (set-kdr c1 1)
      (set-kdr c2 2)
      (let ((v (= (kdr c1) (kdr c2))))
        (set-kdr c1 t1)
        (set-kdr c2 t2)
        v))))

(define last-kons
  (lambda (ls)
    (cond
     ((null? (kdr ls)) ls)
     (else (last-kons (kdr ls))))))

(define finite-lenkth
  (lambda (p)
    (letcc infinite
      (letrec
          ((C (lambda (p q)
                (cond
                 ((null? q) 0)
                 ((null? (kdr q)) 1)
                 ((same? p q) (infinite #f))
                 (else (+ (C (sl p) (qk q)) 2)))))
           (qk (lambda (x) (kdr (kdr x))))
           (sl (lambda (x) (kdr x))))
        (cond
         ((null? p) 0)
         (else (add1 (C p (kdr p)))))))))

(define mongo
  (kons 'pie (kons 'a (kons 'la (kons 'mode '())))))

(set-kdr (kdr (kdr (kdr mongo))) (kdr mongo))

(define deep
  (lambda (m)
    (cond
     ((zero? m) 'pizza)
     (else (cons (deep (sub1 m)) '())))))

(define six-layers
  (lambda (p)
    (cons (cons (cons (cons (cons (cons p '()) '()) '()) '()) '()) '())))

(define four-layers
  (lambda (p)
    (cons (cons (cons (cons p '()) '()) '()) '())))

(define toppings)

(define deepB
  (lambda (m)
    (cond
     ((zero? m)
      (letcc jump
        (set! toppings jump)
        'pizza))
     (else (cons (deepB (sub1 m)) '())))))

(define deep&co
  (lambda (m k)
    (cond
     ((zero? m) (k 'pizza))
     (else
      (deep&co (sub1 m)
               (lambda (x)
                 (k (cons x '()))))))))

(define two-layers
  (lambda (p)
    (cons (cons p '()) '())))

(define deep&coB
  (lambda (m k)
    (cond
     ((zero? m)
      (begin
        (set! toppings k)
        (k 'pizza)))
     (else
      (deep&coB (sub1 m)
                (lambda (x)
                  (k (cons x '()))))))))

(define leave)

(define walk
  (lambda (l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (leave (car l)))
     (else
      (begin
        (walk (car l))
        (walk (cdr l)))))))

(define start-it
  (lambda (l)
    (letcc here
      (set! leave here)
      (walk l))))

(define fill)

(define waddle
  (lambda (l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (begin
        (letcc rest
          (set! fill rest)
          (leave (car l)))
        (waddle (cdr l))))
     (else
      (begin
        (waddle (car l))
        (waddle (cdr l)))))))

(define start-it2
  (lambda (l)
    (letcc here
      (set! leave here)
      (waddle l))))

(define get-next
  (lambda (x)
    (letcc here-again
      (set! leave here-again)
      (fill 'go))))

(define get-first
  (lambda (l)
    (letcc here
      (set! leave here)
      (waddle l)
      (leave '()))))

(define two-in-a-row*?
  (lambda (l)
    (let ((fst (get-first l)))
      (if (atom? fst)
          (two-in-a-row-b*? fst)
          #f))))

(define two-in-a-row-b*?
  (lambda (a)
    (let ((n (get-next 'go)))
      (if (atom? n)
          (or (eq? n a)
              (two-in-a-row-b*? n))
          #f))))

(define two-in-a-row*?
  (letrec
      ((T? (lambda (a)
             (let ((n (get-next 0)))
               (if (atom? n)
                   (or (eq? n a) (T? n))
                   #f))))
       (get-next
        (lambda (x)
          (letcc here-again
            (set! leave here-again)
            (fill 'go))))
       (fill (lambda (x) x))
       (waddle
        (lambda (l)
          (cond
           ((null? l) '())
           ((atom? (car l))
            (begin
              (letcc rest
                (set! fill rest)
                (leave (car l)))
              (waddle (cdr l))))
           (else (begin
                   (waddle (car l))
                   (waddle (cdr l)))))))
       (leave (lambda (x) x)))
    (lambda (l)
      (let ((fst (letcc here
                   (set! leave here)
                   (waddle l)
                   (leave '()))))
        (if (atom? fst) (T? fst) #f)))))

(define lookup
  (lambda (table name)
    (table name)))

(define extend
  (lambda (name1 value table)
    (lambda (name2)
      (cond
       ((eq? name2 name1) value)
       (else (table name2))))))

(define define?
  (lambda (e)
    (cond
     ((atom? e) #f)
     ((atom? (car e))
      (eq? (car e) 'define))
     (else #f))))

(define *define
  (lambda (e)
    (set! global-table
          (extend (name-of e)
                  (box (the-meaning (right-side-of e)))
                  global-table))))

(define box
  (lambda (it)
    (lambda (sel)
      (sel it (lambda (new) (set! it new))))))

(define setbox
  (lambda (box new)
    (box (lambda (it set) (set new)))))

(define unbox
  (lambda (box)
    (box (lambda (it set) it))))

(define the-meaning
  (lambda (e)
    (meaning e lookup-in-global-table)))

(define lookup-in-global-table
  (lambda (name)
    (lookup global-table name)))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define *quote
  (lambda (e table)
    (text-of e)))

(define *identifier
  (lambda (e table)
    (unbox (lookup table e))))

(define *set
  (lambda (e table)
    (setbox
     (lookup table (name-of e))
     (meaning (right-side-of e) table))))

(define *lambda
  (lambda (e table)
    (lambda (args)
      (beglis (body-of e)
              (multi-extend (formals-of e)
                            (box-all args)
                            table)))))

(define beglis
  (lambda (es table)
    (cond
     ((null? (cdr es))
      (meaning (car es) table))
     (else ((lambda (val)
              (beglis (cdr es) table))
            (meaning (car es) table))))))

(define box-all
  (lambda (vals)
    (cond
     ((null? vals) '())
     (else (cons (box (car vals)) (box-all (cdr vals)))))))

(define multi-extend
  (lambda (names values table)
    (cond
     ((null? names) table)
     (else
      (extend (car names)
              (car values)
              (multi-extend (cdr names)
                            (cdr values)
                            table))))))

(define odd?
  (lambda (n)
    (cond
     ((zero? n) #f)
     (else (even? (sub1 n))))))

(define even?
  (lambda (n)
    (cond
     ((zero? n) #t)
     (else (odd? (sub1 n))))))

(define *application
  (lambda (e table)
    ((meaning (function-of e) table)
     (evlis (arguments-of e) table))))

(define evlis
  (lambda (args table)
    (cond
     ((null? args) '())
     (else
      ((lambda (val)
         (cons val (evlis (cdr args) table)))
       (meaning (car args) table))))))

(define :car
  (lambda (args-in-a-list)
    (car (car args-in-a-list))))

(define a-prim
  (lambda (p)
    (lambda (args-in-a-list)
      (p (car args-in-a-list)))))

(define b-prim
  (lambda (p)
    (lambda (args-in-a-list)
      (p (car args-in-a-list)
         (car (cdr args-in-a-list))))))

(define *const
  (lambda (e table)
    (cond
     ((number? e) e)
     ((eq? e #t) #t)
     ((eq? e #f) #f)
     ((eq? e 'cons) (b-prim cons))
     ((eq? e 'car) (a-prim car))
     ((eq? e 'cdr) (a-prim cdr))
     ((eq? e 'eq?) (b-prim eq?))
     ((eq? e 'atom?) (a-prim atom?))
     ((eq? e 'null?) (a-prim null?))
     ((eq? e 'zero?) (a-prim zero?))
     ((eq? e 'add1) (a-prim add1))
     ((eq? e 'sub1) (a-prim sub1))
     ((eq? e 'number?) (a-prim number?)))))

(define *const
  (let ((:cons (b-prim cons))
        (:car (a-prim car))
        (:cdr (a-prim cdr))
        (:null? (a-prim null?))
        (:eq? (b-prim eq?))
        (:atom? (a-prim atom?))
        (:zero? (a-prim zero?))
        (:add1 (a-prim add1))
        (:sub1 (a-prim sub1))
        (:number? (a-prim number?)))
    (lambda (e table)
      (cond
       ((number? e) e)
       ((eq? e #t) #t)
       ((eq? e #f) #f)
       ((eq? e 'cons) :cons)
       ((eq? e 'car) :car)
       ((eq? e 'cdr) :cdr)
       ((eq? e 'null?) :null?)
       ((eq? e 'eq?) :eq?)
       ((eq? e 'atom?) :atom?)
       ((eq? e 'zero?) :zero?)
       ((eq? e 'add1) :add1)
       ((eq? e 'sub1) :sub1)
       ((eq? e 'number?) :number?)))))

(define *const
  ((lambda (:cons :car :cdr :null? :eq? :atom? :zero? :add1 :sub1 :number?)
     (lambda (e table)
       (cond
        ((number? e) e)
        ((eq? e #t) #t)
        ((eq? e #f) #f)
        ((eq? e 'cons) :cons)
        ((eq? e 'car) :car)
        ((eq? e 'cdr) :cdr)
        ((eq? e 'null?) :null?)
        ((eq? e 'eq?) :eq?)
        ((eq? e 'atom?) :atom?)
        ((eq? e 'zero?) :zero?)
        ((eq? e 'add1) :add1)
        ((eq? e 'sub1) :sub1)
        ((eq? e 'number?) :number?))))
   (b-prim cons)
   (a-prim car)
   (a-prim cdr)
   (a-prim null?)
   (b-prim eq?)
   (a-prim atom?)
   (a-prim zero?)
   (a-prim add1)
   (a-prim sub1)
   (a-prim number?)))

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define evcon
  (lambda (lines table)
    (cond
     ((else? (question-of (car lines)))
      (meaning (answer-of (car lines)) table))
     ((meaning (question-of (car lines)) table)
      (meaning (answer-of (car lines)) table))
     (else (evcon (cdr lines) table)))))

(define *letcc
  (lambda (e table)
    (letcc skip
      (beglis (ccbody-of e)
              (extend (name-of e)
                      (box (a-prim skip))
                      table)))))

(define value
  (lambda (e)
    (letcc the-end
      (set! abort the-end)
      (cond
       ((define? e) (*define e))
       (else (the-meaning e))))))

(define abort)

(define the-empty-table
  (lambda (name)
    (abort (cons 'no-answer (cons name '())))))

(define global-table the-empty-table)

(define expression-to-action
  (lambda (e)
    (cond
     ((atom? e) (atom-to-action e))
     (else (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    (cond
     ((number? e) *const)
     ((eq? e #t) *const)
     ((eq? e #f) *const)
     ((eq? e 'cons) *const)
     ((eq? e 'car) *const)
     ((eq? e 'cdr) *const)
     ((eq? e 'null?) *const)
     ((eq? e 'eq?) *const)
     ((eq? e 'atom?) *const)
     ((eq? e 'zero?) *const)
     ((eq? e 'add1) *const)
     ((eq? e 'sub1) *const)
     ((eq? e 'number?) *const)
     (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond
     ((atom? (car e))
      (cond
       ((eq? (car e) 'quote) *quote)
       ((eq? (car e) 'lambda) *lambda)
       ((eq? (car e) 'letcc) *letcc)
       ((eq? (car e) 'set!) *set)
       ((eq? (car e) 'cond) *cond)
       (else *application)))
     (else *application))))

(define text-of
  (lambda (x)
    (car (cdr x))))

(define formals-of
  (lambda (x)
    (car (cdr x))))

(define body-of
  (lambda (x)
    (cdr (cdr x))))

(define ccbody-of
  (lambda (x)
    (cdr (cdr x))))

(define name-of
  (lambda (x)
    (car (cdr x))))

(define right-side-of
  (lambda (x)
    (cond
     ((null? (cdr (cdr x))) 0)
     (else (car (cdr (cdr x)))))))

(define cond-lines-of
  (lambda (x)
    (cdr x)))

(define else?
  (lambda (x)
    (cond
     ((atom? x) (eq? x 'else))
     (else #f))))

(define question-of
  (lambda (x)
    (car x)))

(define answer-of
  (lambda (x)
    (car (cdr x))))

(define function-of
  (lambda (x)
    (car x)))

(define arguments-of
  (lambda (x)
    (cdr x)))
