#lang rosette

(current-bitwidth #f)

(require rosette/lib/synthax)

(define (test-run run hyperparameter other-hyperparameter absent-ok? boolean-value string-value  numeric-comparison compare-hyperparameters numeric-value parameter-present negated?)   
    ; look through other hyperparametes, and the return the value
    ; corresponding to the desired one
    (define (ok-other elts)
      (cond ((null? elts) +nan.0)
            ((equal? other-hyperparameter (first (first elts)))
             (second (first elts)))
            (else (ok-other (cdr elts)))))
    
    (define (ok-elt elts)
      (cond
        ; no more hyperparameters, so the chosen one is absent
        ; that can mean either success or failure, but must always mean the same thing
        ((null? elts) absent-ok?)
        ; found the chosen hyperparameter, so check its value
        ((equal? hyperparameter (first (first elts)))
         (let ((v (second (first elts))))
           (cond ((boolean? v)
                  ; booleans must either true or false, chosen symbolically
                  (eq? v boolean-value))
                 ((string? v)
                  ; strings must be a symbolically-chosen constant
                  (eq? (not negated?) (equal? v string-value)))
                 ((number? v)
                  ; numbers must have a relationship to either
                  ; a) a real-valued constant, or
                  ; b) the value of some other hyperparameter
                  (eq? (not negated?) (numeric-comparison v (if compare-hyperparameters (ok-other (rest elts)) numeric-value))))
                 (else
                  ; cannot reason about this value, so try simply asserting that
                  ; the hyperparameter has some value.
                  parameter-present))))
        (else
         ; look at the rest of the hyperparameters that 'run' has
         (ok-elt (rest elts)))))
    
    (ok-elt (third run)))

(define (ok hyperparameter other-hyperparameter absent-ok? boolean-value string-value  numeric-comparison compare-hyperparameters numeric-value parameter-present)   
    (lambda (run)
      (eq? (second run) (test-run run hyperparameter other-hyperparameter absent-ok? boolean-value string-value numeric-comparison compare-hyperparameters numeric-value parameter-present))))


(define (check f data)
  (cond ((null? data)
         #t)
        ((f (first data))
         (check f (rest data)))
        (else #f)))

(define sok-next
  (let ((c 0))
    (define (next-index)
      (let ((v c))
        (set! c (+ c 1))
        v))
    next-index))

(define (indexed-symbol sym idx)
  (string->symbol (format "~A#~A" sym idx)))

(define (stest-run-with-state [s -1])
  (let* ((idx (if (>= s 0) s (sok-next)))
         (hyperparameter (constant (indexed-symbol 'hyperparameter idx) string?))
         (other-hyperparameter (constant (indexed-symbol 'other-hyperparameter idx) string?))
         (absent-ok? (constant (indexed-symbol 'absent-ok? idx) boolean?))
         (boolean-value (constant (indexed-symbol 'boolean-value idx) boolean?))
         (negated? (constant (indexed-symbol 'negated? idx) boolean?))
         (string-value (constant (indexed-symbol 'string-value idx) string?))
         (is-lt (constant (indexed-symbol 'is-less-than idx) boolean?))
         (is-le (constant (indexed-symbol 'is-less-equal idx) boolean?))
         (numeric-comparison
          (cond (is-lt <)
                (is-le <=)
                (else =)))
         (compare-hyperparameters (constant (indexed-symbol 'compare-hyperparameter idx) boolean?))
         (numeric-value (constant (indexed-symbol 'numeric-value idx) real?))
         (parameter-present (constant (indexed-symbol 'parameter-present idx) boolean?)))
    (list
     (lambda (run)
       (test-run run hyperparameter other-hyperparameter absent-ok? boolean-value string-value numeric-comparison compare-hyperparameters numeric-value parameter-present negated?))
     (list hyperparameter other-hyperparameter absent-ok? boolean-value string-value is-lt is-le negated? compare-hyperparameters numeric-value parameter-present))))

(define (stest-run [s -1])
  (car (stest-run-with-state s)))

(define (sok)
  (let ((test-f (stest-run)))
    (lambda (run)
      (eq? (second run) (test-f run)))))

(define (ssplit-run left-run right-run)
  (let ((top-f (stest-run)))
    (lambda (run)
      (if (top-f run)
          (left-run run)
          (right-run run)))))

(define (ssplit-choice left-run right-run)
  (let ((f (ssplit-run left-run right-run)))
    (lambda (run)
      (eq? (second run) (f run)))))

(define (stwo-choice)
  (ssplit-choice (stest-run) (stest-run)))

(define (sthree-choice)
  (ssplit-choice
   (ssplit-run (stest-run) (stest-run))
   (ssplit-run (stest-run) (stest-run))))


(define (spartition n)
  (let ((spine '())
        (leaves '())
        (spine-functions '())
        (leaf-functions '()))
    (define (tests i)
      (if (>= i n)
          '()
          (cons
           (let ((x (stest-run-with-state i)))
             (set! spine `(,@spine ,(cadr x)))
             (set! spine-functions `(,@spine-functions, (car x)))
             (car x))
           (tests (+ i 1)))))
    
    (let ((idx n))
      (define (sp n l)
        (if (<= n 0)
            (let ((x (stest-run-with-state idx)))
              (set! leaves `(,@leaves ,(cadr x)))
              (set! leaf-functions `(,@leaf-functions ,(car x)))
              (set! idx (+ idx 1))
              (car x))
            (let ((test (car l))
                  (left (sp (- n 1) (cdr l))))
              (let ((right (sp (- n 1) (cdr l))))
                (lambda (run)
                  (let ((res (test run)))
                    (if res (and res (left run)) (and (not res) (right run)))))))))
  
      (let ((x (sp n (tests 0))))
        (list x spine leaves spine-functions leaf-functions)))))

(define (spartition-solve n runs)
  (let* ((f (spartition n))
         (m1 (solve
              (assert
               (check (lambda (r) (eq? (second r) ((car f) r))) runs)))))
    (if (unsat? m1)
        #f
        (list m1
              (map (lambda (l)
                     (map (lambda (ll)
                            (map
                             (lambda (v) (list v
                                               (let ((vv (m1 v)))
                                                 (if (rational? vv) (* 1.0 vv) vv))))
                             ll))
                          l))
                   (list (second f) (third f)))
              (fourth f)
              (fifth f)))))

(define (sok-all runs)
  (check (sok) runs))

(define (partition runs tests process)
  (define (p fs r)
    (if (null? fs)
        (process r)
        (append
         (p (cdr fs) (filter (car fs) r))
         (p (cdr fs) (filter (lambda (elt) (not ((car fs) elt))) r)))))
  (p tests runs))

(define (solve-it runs)
  (solve (assert (sok-all runs))))

(define (first-n n lst)
  (if (= n 0) '() (cons (car lst) (first-n (- n 1) (cdr lst)))))

(provide (all-defined-out))
