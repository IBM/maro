#lang rosette

(define (hyperparameter-types run-data)
  (remove-duplicates
   (apply append
     (map
      (lambda (e) (map (lambda (p) (cons (car p) (type-of (cadr p)))) e))
      (map third run-data)))))

(define (get-hyperparameter constraint)
    (cadr (car (filter (lambda (p) (string-prefix? (format "~S" (car p)) "hyperparameter#")) constraint))))

(define (get-data-type constraint run-data)
    (map cdr (filter (lambda (x) (equal? (car x) (get-hyperparameter constraint))) (hyperparameter-types run-data))))

(define (get-relevant-symbolics constraint run-data)
    (let ((types (get-data-type constraint run-data))
          (base (list "hyperparameter#" "absent-ok?#")))
      (filter
       (lambda (p)
        (not (null?
         (filter (lambda (s)
                   (and (string-prefix? (format "~S" (car s)) p)
                        (not (equal? (format "~S" (car s)) (format "~S" (cadr s))))))
                 constraint))))
      (remove-duplicates
             (apply append
        (cons base
                   (map (lambda (t)
                           (cond ((eq? t boolean?)
                                  (list "boolean-value#"))
                                 ((eq? t string?)
                                  (list "string-value#" "negated?#"))
                                 ((or (eq? t integer?) (eq? t real?))
                                  (list "is-less-than#" "is-less-equal#" "negated?#" "compare-hyperparameter#" "other-hyperparameter#" "numeric-value#"))
                                 (else (list  "parameter-present#"))))
                         types)))))))

(define (get-relevant-symbolic-values constraint run-data)
    (let ((vars (get-relevant-symbolics constraint run-data)))
      (map
       (lambda (var)
         (let ((val (cdar (filter (lambda (p) (string-prefix? (format "~S" (car p)) var)) constraint))))
           (cons var val)))
       vars)))

(define (model-to-relevant-symbolics model run-data)
 (list
  (map (lambda (s) (get-relevant-symbolic-values s run-data)) (car (cadr model)))
  (map (lambda (s) (get-relevant-symbolic-values s run-data)) (cadr (cadr model)))))

(define (get-run-symbolics model run-data)
  (let ((symbolics (model-to-relevant-symbolics model run-data))
        (spine (third model))
        (leaves (fourth model)))
    
    (define (l n run)
      (let ((s (list-ref (second symbolics) n)))
        (if (evaluate ((list-ref leaves n) run) (car model)) s (list 'not s))))

    (define (s syms ss n run)
      (if (null? ss)
          (list (l n run))
          (if (evaluate ((car ss) run) (car model))
              (cons (car syms) (s (cdr syms) (cdr ss) (* 2 n) run))
              (cons (list 'not (car syms)) (s (cdr syms) (cdr ss) (+ 1 (* 2 n)) run)))))

    (remove-duplicates (map (lambda (run) (s (car symbolics) spine 0 run)) run-data))))

(define (prune x)
    (let ((negs (filter (lambda (e) (and (null? (cdr e)) (eq? (caar e) 'not) (assoc (cadar e) x))) x)))
      (filter (lambda (e) (not (findf (lambda (v) (or (equal? e v) (equal? e (cdar v)))) negs))) x)))

(define (merge res1)
    (let ((roots (remove-duplicates (map car (remove '() res1)))))
      (map (lambda (root) (cons root (prune (merge (map cdr (filter (lambda (e) (eq? (car e) root)) res1)))))) roots)))

(define (get-test x)
    (define (check y)
      (and (assoc y x) (cadr (assoc y x))))
    (cond ((check "is-less-than#") <)
          ((check "is-greater-than#") >)
          ((check "is-less-equal#") <=)
          ((check "is-greater-equal#") >=)
          (else =)))

(define (get-hyperparameter-name out)
    (cadr (string-split (cadr (assoc "hyperparameter#" out)) ".")))

(define (get-task-name out)
    (car (string-split (cadr (assoc "hyperparameter#" out)) ".")))

(define (get-other-hyperparameter-name out)
    (cadr (string-split (cadr (assoc "other-hyperparameter#" out)) ".")))

(define (get-other-task-name out)
    (car (string-split (cadr (assoc "other-hyperparameter#" out)) ".")))

(define (compare-hyperparameters? out)
  (let ((x (assoc "compare-hyperparameter#" out)))
    (and x (cadr x))))

(define (get-required? out)
  (let ((x (assoc "absent-ok?#" out)))
    (and x (not (cadr x)))))

(define (get-parameter-present out)
  (let ((x (assoc "parameter-present#" out)))
    (and x (cadr x))))

(define (get-numeric-value out)
  (let ((x (assoc "numeric-value#" out)))
    (and x (number? (cadr x)) (cadr x))))

(define (has-boolean-value? out)
  (assoc "boolean-value#" out))

(define (get-boolean-value out)
  (let ((x (assoc "boolean-value#" out)))
    (cadr x)))

(define (get-string-value out)
  (let ((x (assoc "string-value#" out)))
    (and x (string? (cadr x)) (cadr x))))

(define (is-negated-string-test? out)
  (print "neg:")
  (print out)
  (print ":")
  (print (and (assoc "string-value#" out)
              (let ((x (assoc "is-string-equal#" out)))
                (and x (not (cadr x))))))
  (newline)
  (and (assoc "string-value#" out)
       (let ((x (assoc "is-string-equal#" out)))
         (and x (not (cadr x))))))

(define (get-required-constraint out)
  (let ((tt (get-task-name out))
        (hp (get-hyperparameter-name out))
        (t (format "~A" (get-test out))))
    (if (and (get-required? out) (get-parameter-present out))
        tt
        #f)))

(define (get-comparison-constraint out)
  (let ((tt (string->symbol (get-task-name out)))
        (hp (get-hyperparameter-name out)))
    (if (compare-hyperparameters? out)
        (list tt hp (get-test out) (get-other-task-name out) (get-other-hyperparameter-name out))
        #f)))

(define (get-hyperparameter-constraint out)
  (let ((tt (get-task-name out))
        (hp (get-hyperparameter-name out))
        (v
          (or
           (let ((n (get-numeric-value out)))
             (and n (if (integer? n) n (* n 1.0))))
           (get-string-value out)
           (and (has-boolean-value? out)
                (get-boolean-value out)))))
    (and (or v (has-boolean-value? out)) (list tt hp (format "~S" (get-test out))  v))))


(define (require-node name tree node)
  (define (new-steps steps)
    (apply hasheq
           (apply append
                  (map (lambda (n) (list n (require-node n (hash-ref steps n) node))) (hash-keys steps)))))
  (define (fix node key nv)
    (apply hasheq (cons key (cons nv (apply append (map (lambda (n) (list n (hash-ref node n))) (remove key (hash-keys node))))))))
  (if (not (hash-has-key? tree 'steps))
      tree
      (let ((steps (hash-ref tree 'steps)))        
        (cond ((equal? (hash-ref tree 'class) "lale.operators.OperatorChoice")
               (if (hash-has-key? steps node)
                   (hash-ref steps node)
                   (fix tree 'steps (new-steps steps))))
              ((member node (hash-keys steps))
               node)
              (else
               (fix tree 'steps (new-steps steps)))))))

(define (customize-schema . json)
  (hash 'properties
        (hash 'hyperparams (hash 'allOf json))))

(define (make-choose steps)
  (hash 'class "lale.operators.OperatorChoice"
        'state "planned"
        'operator "OperatorChoice"
        'steps (apply hash
                      (apply append
                             (map (lambda (s) (list (gensym 'choice) s))
                                  steps)))))

(define (customize-value hyperparameter min-val max-val)
  (customize-schema
   (hash hyperparameter
         (hash 'type "integer" 'minimum min-val 'maximum max-val))))

(define (customize-single-value hyperparameter val)
  (customize-schema
   (hash hyperparameter
         (hash 'enum (list val)))))

(define (add-limit name tree node limit-json)
  (define (new-steps steps)
    (apply hasheq
           (apply append
                  (map (lambda (n) (list n (add-limit n (hash-ref steps n) node limit-json))) (hash-keys steps)))))
  (define (fix node key nv)
    (apply hasheq
           (cons key
                 (cons nv
                       (apply append (map (lambda (n) (list n (hash-ref node n)))
                                          (remove key (hash-keys node))))))))

  (cond ((equal? name node)
         (fix tree 'customize_schema limit-json))
        ((or (! (hash? tree)) (not (hash-has-key? tree 'steps)))
         tree)
        (else
         (fix tree 'steps (new-steps (hash-ref tree 'steps))))))

(define (redundant-constraint? first second model runs)
  (let ((x (remove-duplicates
            (map
             (lambda (r) (evaluate (second r) model))
             (filter
              (lambda (r) (not (evaluate (first r) model)))
              runs)))))
    (if (= (length x) 1) x #f)))
  
(define (get-nested-hyperparameter-constraints negated? rs original-schema model runs)
  (if (= (length (car rs)) 1)
      (let ((step1 (get-hyperparameter-constraint (car (car rs))))
            (step2 (if (not (redundant-constraint? (car (third model)) ((if negated? cadr car) (fourth model)) (car model) runs))
                       (get-hyperparameter-constraint (if negated? (cadr (cadr rs)) (car (cadr rs))))
                       #f)))
        (print rs)
        (newline)
        (print step1)
        (newline)
        (print step2)
        (newline)
        (customize-schema
         (let ((one (string->symbol (cadr step1)))
               (two (let ((x (hash 'enum (list (fourth step1)))))
                      (if (boolean? (fourth step1))
                          (if negated?
                              (hash 'enum (list (not (fourth step1))))
                              x)
                          (if negated?
                              (hash 'allOf
                                    (list (hash 'not x)
                                          (hash-ref (hash-ref (car (hash-ref original-schema 'allOf)) 'properties) (string->symbol (cadr step1)))))
                              x)))))
           (if step2
               (hash one two
                     (string->symbol (cadr step2))

                     (let ((x (hash 'enum (list (fourth step2)))))
                       (if (boolean? (fourth step2))
                           (if negated?
                               (hash 'enum (list (not (fourth step2))))
                               x)
                           (if negated?
                               (hash 'allOf
                                     (list (hash 'not x)
                                           (hash-ref (hash-ref (car (hash-ref original-schema 'allOf)) 'properties) (string->symbol (cadr step2)))))
                               x))))
               
               (hash one two)))))
      #f))

(provide (all-defined-out))

