#lang racket

(define (is_print_args new_arg)
       ( if ( null? new_arg )
            #t
            ( if ( equal? "'" ( car new_arg ))
                 ( if ( equal? "'" (car ( cdr new_arg )))
                      ( is_print_args (cdr (cdr new_arg)))
                      (begin
                        (print "expected back-quote ' ")
                      #f))
                 ( if ( equal? "," ( car new_arg ))
                      ( is_print_args (cdr new_arg))
                      ( if (equal? ")" (car new_arg))
                          (cdr new_arg)
                          (begin
                        (print "expected closing bracket ')' ")
                      #f)))
                 )))
  
  
(define (is_print_function new_line)
  ( if ( null? new_line )
      #f
      ( if (and '(car new_line) (equal? "printf" ( car new_line )))
           ( if (and '(car new_line) ( equal? "(" ( car (cdr new_line ))))
                (if ( is_print_args ( cdr ( cdr new_line )))
                             (if (and '(car new_line) ( equal? ";" (car (is_print_args ( cdr ( cdr new_line ))))))
                                 #t
                                 (begin
                        (print "expected ';'")
                      #f))
                             (begin
                        (print "syntax error")
                      #f))
                (begin
                        (print "expected '(' after 'printf'")
                      #f))
           (begin
                        (print "expected 'printf'")
                      #f))
      ))
  

(is_print_function '("printf" "(" "'" "'" "," "'" "'" ")" ";"))
(is_print_function '("(" "'" "'" "," "'" "'" ")" ";"))
(is_print_function '("printf" "(" "'" ")" ";"))
(is_print_function '("printf" "(" ")" ";"))
