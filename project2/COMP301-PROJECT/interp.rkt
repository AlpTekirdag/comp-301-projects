#lang eopl

;; interpreter for the LET language. 


(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
;; Page: 71
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

;; value-of : Exp * Env -> ExpVal
;; Page: 71
(define value-of
  (lambda (exp env)
    (cases expression exp

      ;; implement const-exp here
      (const-exp (num) (num-val num))

      ;; implement var-exp here
      (var-exp (var) (apply-env env var))

      ;; implement comp-exp here
      (comp-exp (exp1 cmp exp2)
              (let ((val1 (value-of exp1)) (val2 (value-of exp2)))
                (let ((num1 (expval->num val1)) (num2 (expval->num val2)))
                    (bool-val (cond
                      ((equal? cmp 'greater) (> num1 num2))
                      ((equal? cmp 'equal) (< num1 num2))
                      (else (= num1 num2))
                      ))
              )))

      
      ;; implement op-exp here
      (op-exp (exp1 op exp2)
              (let ((val1 (value-of exp1)) (val2 (value-of exp2)))
                (let ((num1 (expval->num val1)) (num2 (expval->num val2)))
                    (cond
                      ((equal? op 'add) (num-val (+ num1 num2)))
                      ((equal? op 'sub) (num-val (- num1 num2)))
                      ((equal? op 'mult) (num-val (* num1 num2)))
                      (else (num-val (/ num1 num2))))
              )))


      
      ;; if-exp
      (if-exp (cond1 exp1 else-exp)
              (let ((val1 (value-of cond1 env)))      
                (if (expval->bool val1)
                    (value-of exp1 env)
                    (value-of else-exp env))))

      ;; implement my-cond-exp here
      (my-cond-exp (cond1 exp1 else-exp)
              (let ((val1 (value-of cond1 env)))      
                (if (expval->bool val1)
                    (value-of exp1 env)
                    (value-of else-exp env))))

      
      ;; implement str-exp here
      (str-exp (str) (str-val str))


      ;; implement bool-exp here
      (bool-exp (b) (bool-val (cond ((equal? b '#true) #t) (else #f))))
      
      ;; implement zero-exp here
      (zero?-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->num val1)))
                     (if (zero? num1)
                         (bool-val #t)
                         (bool-val #f)))))

      ;; implement let-exp here
      (let-exp (var exp1 body)       
               (let ((val1 (value-of exp1 env)))
                 (value-of body
                           (extend-env var val1 env))))


)))
;(trace value-of)