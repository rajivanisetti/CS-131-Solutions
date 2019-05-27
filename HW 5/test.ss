(pretty-print (expr-compare 12 12))
(pretty-print (expr-compare 12 20))
(pretty-print (expr-compare #t #t))
(pretty-print (expr-compare #f #f))
(pretty-print (expr-compare #t #f))
(pretty-print (expr-compare #f #t))
(pretty-print (expr-compare 'a '(cons a b)))
(pretty-print (expr-compare '(cons a b) '(cons a b)))
(pretty-print (expr-compare '(cons a b) '(cons a c)))
(pretty-print (expr-compare '(cons (cons a b) (cons b c))
                            '(cons (cons a c) (cons a c))))
(pretty-print (expr-compare '(cons a b) '(list a b)))
(pretty-print (expr-compare '(list) '(list a)))
(pretty-print (expr-compare ''(a b) ''(a c)))
(pretty-print (expr-compare '(quote (a b)) '(quote (a c))))
(pretty-print (expr-compare '(quoth (a b)) '(quoth (a c))))
(pretty-print (expr-compare '(if x y z) '(if x z z)))
(pretty-print (expr-compare '(if x y z) '(g x y z)))
(pretty-print (expr-compare '((lambda (a) (f a)) 1) '((lambda (a) (g a)) 2)))
(pretty-print (expr-compare '((lambda (a) (f a)) 1) '((λ (a) (g a)) 2)))
(pretty-print (expr-compare '((lambda (a) a) c) '((lambda (b) b) d)))
(pretty-print (expr-compare ''((λ (a) a) c) ''((lambda (b) b) d)))
(pretty-print (expr-compare '(+ #f ((λ (a b) (f a b)) 1 2))
                            '(+ #t ((lambda (a c) (f a c)) 1 2))))
(pretty-print (expr-compare '((λ (a b) (f a b)) 1 2)
              '((λ (a b) (f b a)) 1 2)))
(pretty-print (expr-compare '((λ (a b) (f a b)) 1 2)
              '((λ (a c) (f c a)) 1 2)))
(pretty-print (expr-compare '((lambda (a) (eq? a ((λ (a b) ((λ (a b) (a b)) b a))
                                                  a (lambda (a) a))))
                              (lambda (b a) (b a)))
                            '((λ (a) (eqv? a ((lambda (b a) ((lambda (a b) (a b)) b a))
                                              a (λ (b) a))))
                              (lambda (a b) (a b)))))
(pretty-print (expr-compare '(lambda (a b) a) '(lambda (a) a)))
(pretty-print (expr-compare '(lambda (a b) a) '(lambda (a . b) a)))
(pretty-print (expr-compare '(lambda a a) '(lambda (a) a)))
(pretty-print (expr-compare '(lambda a a) '(lambda b b)))
(pretty-print (expr-compare '(lambda (a b) a) '(lambda (a b) b)))
(pretty-print (expr-compare '(lambda (a b) (a b c)) '(lambda (a b) (c b a))))
(pretty-print (expr-compare '(lambda (a b) (a b c)) '(lambda (a b) a)))
(pretty-print (expr-compare '(lambda (a b) (b a)) '(lambda (b a) (a b))))