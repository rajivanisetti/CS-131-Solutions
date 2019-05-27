#lang racket

(define (choose_lambda a b) (if (equal? a b) a my_lambda))

(define my_lambda (string->symbol "\u03BB"))

(define (get_index lst index element)
  (cond
    [(equal? (car lst) element) index]
    [else (get_index (cdr lst) (+ index 1) element)]
  )
)

(define (expr-compare x y)
  (cond 
    [(and (list? x) (list? y) (equal? (length x) (length y))) (process_possible_bind x y)]
    [else (process_constant x y)]
  )
)

(define (expr-compare-no-lambda x y)
  (cond 
    [(and (list? x) (list? y) (equal? (length x) (length y))) (process_possible_bind_no_lambda x y)]
    [else (process_constant x y)]
  )
)

(define (process_quote x y) (process_constant x y))
(define (process_if x y) (process_list x y))

(define (process_possible_bind x y)
  (let ([first_x (car x)] [first_y (car y)])
    (cond
      [(or (equal? first_x 'quote) (equal? first_y 'quote)) (process_quote x y)]
      [(and (equal? first_x 'if) (equal? first_y 'if)) (process_if x y)]
      [(or (equal? first_x 'if) (equal? first_y 'if)) (process_constant x y)]
      [(and (or (equal? first_x 'lambda) (equal? first_x my_lambda)) (or (equal? first_y 'lambda) (equal? first_y my_lambda))) (process_lambda x y)]
      [else (process_list x y)]
    )
  )
)

(define (process_possible_bind_no_lambda x y)
  (let ([first_x (car x)] [first_y (car y)])
    (cond
      [(or (equal? first_x 'quote) (equal? first_y 'quote)) (process_quote x y)]
      [(and (equal? first_x 'if) (equal? first_y 'if)) (process_if x y)]
      [(or (equal? first_x 'if) (equal? first_y 'if)) (process_constant x y)]
      [else (process_list-no-lambda x y)]
    )
  )
)

(define (bind_single_variable x y) (string->symbol (string-append (symbol->string x) "!" (symbol->string y))))


(define (bind_variables x y binded_list x_binded_vars y_binded_vars)
  (cond
    [(or (empty? x) (empty? y)) (list binded_list x_binded_vars y_binded_vars)]
    [(not (equal? (car x) (car y))) (bind_variables (cdr x) (cdr y) (cons (bind_single_variable (car x) (car y)) binded_list) (cons (car x) x_binded_vars) (cons (car y) y_binded_vars))]
    [else (bind_variables (cdr x) (cdr y) binded_list x_binded_vars y_binded_vars)]
  )
)

(define (attempt_bind x_vars y_vars)
  (cond
    [(and (list? x_vars) (list? y_vars)) (bind_variables x_vars y_vars null null null)]
    [(and (not (list? x_vars)) (not (list? y_vars))) (bind_variables (list x_vars) (list y_vars) null null null)]
  )
)


(define (bind_to_list x y binds binded_variables)
  (cond
    [(empty? x) null]
    [(and (list? (car x)) (list? (car y)) (or (equal? (caar x) 'lambda) (equal? (caar x) my_lambda) (equal? (caar y) 'lambda) (equal? (caar y) my_lambda)) (cons (expr-compare (car x) (car y)) (bind_to_list (cdr x) (cdr y) binds binded_variables)))]
    [(list? (car x)) (cons (bind_to_list (car x) (car y) binds binded_variables) (bind_to_list (cdr x) (cdr y) binds binded_variables))]
    [(member (car x) binded_variables) (cons (list-ref binds (get_index binded_variables 0 (car x))) (bind_to_list (cdr x) (cdr y) binds binded_variables))]
    [else (cons (car x) (bind_to_list (cdr x) (cdr y) binds binded_variables))]
  )        
)

(define (process_nested_lambda_expression expr inner_bindings outer_binds outer_binding_list)
  (cond
    [(empty? expr) null]
    [(not (list? expr)) (if (member expr inner_bindings) expr (if (member  expr outer_binding_list) (list-ref outer_binds (get_index outer_binding_list 0 expr))  expr))]
    [else (cons (if (member (car expr) inner_bindings) (car expr) (if (member (car expr) outer_binding_list) (list-ref outer_binds (get_index outer_binding_list 0 (car expr))) (car expr))) (process_nested_lambda_expression (cdr expr) inner_bindings outer_binds outer_binding_list))]
  )       
)

(define (process_nested_lambda lst outer_binds outer_binding_list)
  (list (car lst) (cadr lst) (process_nested_lambda_expression (caddr lst) (if (list? (cadr lst)) (cadr lst) (list (cadr lst))) outer_binds outer_binding_list))
)

(define (map_binding_list lst binds binding_list)
  (cond
    [(and (not (list? lst)) (member lst binding_list)) (list-ref binds (get_index binding_list 0 lst))]
    [(not (list? lst)) lst]
    [(empty? lst) null]
    [(and (list? (car lst)) (or (equal? (caar lst) 'lambda) (equal? (caar lst) my_lambda))) (cons (process_nested_lambda (car lst) binds binding_list) (map_binding_list (cdr lst) binds binding_list))]
    [(list? (car lst)) (cons (map_binding_list (car lst) binds binding_list) (map_binding_list (cdr lst) binds binding_list))]
    [(member (car lst) binding_list) (cons (list-ref binds (get_index binding_list 0 (car lst))) (map_binding_list (cdr lst) binds binding_list))]
    [else (cons (car lst) (map_binding_list (cdr lst) binds binding_list))]
   )
)

(define (process_lambda_expression x y binded_variables)
  (cond
    [(and (not (list? y)) (list? x) (and (not (equal? (car x) 'lambda)) (not (equal? (car x) my_lambda))  (expr-compare (map_binding_list x (car binded_variables) (cadr binded_variables)) (if (member y (caddr binded_variables)) (list-ref (car binded_variables) (get_index (caddr binded_variables) 0 y)) y))))]
    [(and (not (list? x)) (list? y) (and (not (equal? (car y) 'lambda)) (not (equal? (car y) my_lambda))  (expr-compare (if (member x (cadr binded_variables)) (list-ref (car binded_variables) (get_index (cadr binded_variables) 0 x)) x) (map_binding_list y (car binded_variables) (caddr binded_variables)))))]
    [(and (list? x) (list? y) (or (equal? (car x) 'lambda) (equal? (car y) 'lambda))) (expr-compare x y)]
    [(and (not (list? x)) (not (list? y))) (let ([res (expr-compare (if (member x (cadr binded_variables)) (list-ref (car binded_variables) (get_index (cadr binded_variables) 0 x)) x) (if (member y (caddr binded_variables)) (list-ref (car binded_variables) (get_index (caddr binded_variables) 0 y)) y))]) (if (list? res) res (list res)))]
    [(and (list? x) (list? y)) (expr-compare (map_binding_list x (car binded_variables) (cadr binded_variables)) (map_binding_list y (car binded_variables) (caddr binded_variables)))]
  )
)

(define (process_lambda x y)
  (let ([x_vars (cadr x)] [y_vars (cadr y)])
    (cond
      [(and (list? x_vars) (list? y_vars) (not (equal? (length x_vars) (length y_vars)))) (process_constant x y)]
      [(or (and (list? x_vars) (not (list? y_vars))) (and (not (list? x_vars)) (list? y_vars))) (process_constant x y)]
      [(or (and (pair? x_vars) (not (list? x_vars)) (list? y_vars) (not (pair? y_vars))) (and (list? x_vars) (not (pair? x_vars)) (pair? y_vars) (not (list? y_vars)))) (process_constant x y)]
      [(and (not (list? x_vars)) (not (list? y_vars))) (let ([binded_variables (attempt_bind x_vars  y_vars)]) (cons (choose_lambda (car x) (car y)) (cons (map_binding_list x_vars (car binded_variables) (cadr binded_variables)) (process_lambda_expression (caddr x) (caddr y) binded_variables))))]
      [else (let ([binded_variables (attempt_bind x_vars y_vars)]) (cons (choose_lambda (car x) (car y)) (cons (map_binding_list x_vars (car binded_variables) (cadr binded_variables)) (let ([processed_expression (process_lambda_expression (caddr x) (caddr y) binded_variables)]) (if (and (list? processed_expression) (equal? (length processed_expression) 1)) processed_expression (list processed_expression))))))]   
    )
  )
)

(define (process_list x y)
  (cond
    [(or (empty? x) (empty? y)) '()]
    [else (cons (expr-compare (car x) (car y)) (process_list (cdr x) (cdr y)))]
  )
)

(define (process_list-no-lambda x y)
  (cond
    [(or (empty? x) (empty? y)) '()]
    [else (cons (expr-compare-no-lambda (car x) (car y)) (process_list-no-lambda (cdr x) (cdr y)))]
  )
)

(define (process_constant x y)
  (cond
    [(equal? x y) x]
    [(and (equal? x #t) (equal? y #f)) '%]
    [(and (equal? x #f) (equal? y #t)) '(not %)]
    [else (list 'if '% x y)]
  )                                
)

(define (test-expr-compare x y)
  (let ([expr-compare-result (expr-compare x y)])
    (and (equal? (eval x) (eval (list 'let '((% #t)) expr-compare-result)))
         (equal? (eval y) (eval (list 'let '((% #f)) expr-compare-result))))
  )
)

(define test-expr-x '(list '(+ 3 (if #t (lambda (a b) ((lambda (a) (a #t)) a)) (quote a)) (list 1 2))))
(define test-expr-y '(list '(+ 2 (if #f (lambda (a c) ((lambda (d e) (e d)) a c)) (quote b)) (list 1 5))))

                                 
