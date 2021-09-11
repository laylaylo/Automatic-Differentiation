; leyla yayladere
; 2018400216
; compiling: yes
; complete: yes

#lang racket

(provide (all-defined-out))

;; given
(struct num (value grad)
    #:property prop:custom-write
    (lambda (num port write?)
        (fprintf port (if write? "(num ~s ~s)" "(num ~a ~a)")
            (num-value num) (num-grad num))))
;; given
 (define relu (lambda (x) (if (> (num-value x) 0) x (num 0.0 0.0))))
;; given
 (define mse (lambda (x y) (mul (sub x y) (sub x y))))


; 3.1 (get-value num-list)

(define (get-value num-list)
        (cond ((null? num-list) '())
              ((num? num-list) (num-value num-list))
              (else (cons (num-value (eval (car num-list))) (get-value (cdr num-list))))))

; 3.2 (get-grad num-list)

(define (get-grad num-list)
        (cond ((null? num-list) '())
              ((num? num-list) (num-grad num-list))
              (else (cons (num-grad (eval (car num-list))) (get-grad (cdr num-list))))))

; 4.1 (add num1 num2 ...)

(define (add num1 num2 . nums)
        (num (+ (num-value num1) (num-value num2) (apply + (get-value nums)))
        (+ (num-grad num1) (num-grad num2) (apply + (get-grad nums)))))

; 4.2 (mul num1 num2 ...) >> division by zero problem??

(define (mul num1 num2 . nums)
         (num (* (num-value num1) (num-value num2) (apply * (get-value nums)))
         (mul-grad 1 (append (list num1 num2) nums))))

(define (mul-grad x nums)
        (cond ((null? nums) 0)
              (else (+ (* x (num-grad (eval (car nums))) (apply * (get-value (cdr nums)))) (mul-grad (* x (num-value (eval (car nums)))) (cdr nums))))))

; 4.3 (sub num1 num2)

(define (sub num1 num2)
        (num (- (num-value num1) (num-value num2)) (- (num-grad num1) (num-grad num2))))

; 5.1 (create-hash names values var)

(define (create-hash names values var)
        (make-hash (create-list names values var)))

(define (create-list names values var)
        (cond ((null? names) '())
              (else (cond ((eq? (car names) var) (append (list (cons (car names) (num (car values) 1.0))) (create-list (cdr names) (cdr values) var))) 
                          (else (append (list (cons (car names) (num (car values) 0.0))) (create-list (cdr names) (cdr values) var)))))))

; 5.2 (parse hash expr)

(define (parse hash expr)
        (cond ((null? expr) '())
              ((list? expr) (cons (parse hash (car expr)) (parse hash (cdr expr)))) 
              ((eq? '+ expr) 'add)
              ((eq? '* expr) 'mul)
              ((eq? '- expr) 'sub)
              ((eq? 'mse expr) 'mse)
              ((eq? 'relu expr) 'relu)
              ((integer? expr) (num expr 0.0))
              (else (hash-ref hash expr))))

; 5.3 (grad names values var expr)

(define (grad names values var expr)
        (num-grad (eval (parse (create-hash names values var) expr))))

; 5.4 (partial-grad names values vars expr) >> try list iteration w/ map

(define (partial-grad names values vars expr)
        (map (lambda (name)
                     (cond ((member name vars) (grad names values name expr))
                           (else 0.0))) names))
                                            
; 5.5 (gradient-descent names values vars lr expr)

(define (gradient-descent names values vars lr expr)
        (map (lambda (number1 number2) (- number1 number2))
             values (map (lambda (number) (* lr number)) (partial-grad names values vars expr))))

; 5.6 (optimize names values vars lr k expr)

(define (optimize names values vars lr k expr)
        (cond ((= k 1) (gradient-descent names values vars lr expr))
              (else (optimize names (gradient-descent names values vars lr expr) vars lr (- k 1) expr)))) 
      
         

  
