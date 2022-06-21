#lang rosette

(require (planet dmac/spin))
(require json)
(require json-parsing)
(require rosette/lib/synthax)
(require "mockup.rkt")
(require "dataset.rkt")
(require "to-json.rkt")

(print-syntax-width 100000)
(error-print-width 100000)

(post "/pipelines"
  (lambda (req)
    (let* ((dataset (json->sjson (regexp-replace* #px"\\bNaN\\b" (params req 'pipelines) "null")))
           (plan (json->sjson (params req 'planned)))
           (schema-properties (pipeline-schema-properties dataset)))
      (let* ((runs (map run-data dataset))
             (x (spartition-solve 0 runs)))
        (print "------------")
        (newline)
        (print x)
        (newline)
        (if (not x)
            (let* ((x (spartition-solve 1 runs))
                   (s (model-to-relevant-symbolics x runs))
                   (step (car (get-hyperparameter-constraint (caar s))))
                   (original-schema
                    (hash-ref (hash-ref (car dataset) 'pipeline_schemas)
                              (string->symbol
                               (hash-ref (pipeline-step (run-pipeline (car dataset)) step) 'class))))
                   (left (get-nested-hyperparameter-constraints #f s original-schema x runs ))
                   (right (get-nested-hyperparameter-constraints #t s original-schema x runs)))
              (print "-- partition --")
              (newline)
              (print x)
              (newline)
              (print "-- symbolics --")
              (newline)
              (print s)
              (newline)
              (jsexpr->string
               (make-choose
                (list
                 (add-limit '() plan step left)
                 (add-limit '() plan step right)))
               #:null #\nul))
            (let ((s (model-to-relevant-symbolics x runs)))
              (print "-- symbolics --")
             (newline)
              (newline)
              (print s)
              (newline)
              (newline)
              (if (and (null? (car s)) (= (length (cadr s)) 1))
                  (let ((step (get-required-constraint (car (cadr s)))))
                    (print "-- constraint --")
                    (newline)
                    (print step)
                    (newline)
                    (newline)
                    (if step
                        (jsexpr->string (require-node '() plan step))
                        (let ((step (get-comparison-constraint (car (cadr s)))))
                        (print "-- step --")
                          (newline)
                          (print step)
                          (newline)
                          (if step
                              (let* ((left (format "~A.~A" (first step) (second step)))
                                     (right (format "~A.~A" (fourth step) (fifth step)))
                                     (all-vals
                                       (remove-duplicates
                                        (append
                                         (map cadr (remove* (list #f) (map (lambda (x) (assoc left (third x))) runs)))
                                         (map cadr (remove* (list #f) (map (lambda (x) (assoc right (third x))) runs))))))
                                     (min-val (apply min all-vals))
                                     (max-val (apply max all-vals))
                                     (vals
                                      (first-n 3 (cdr (map round (sequence->list (in-range min-val max-val (/ (- max-val min-val) 5)))))))
                                     (test
                                      (let ((t (get-test (car (cadr s)))))
                                        (cond ((or (equal? t <) (equal? t <=)) 'maximum)
                                              ((or (equal? t >) (equal? t >=)) 'minimum)
                                              (else 'equal)))))
                                (jsexpr->string
                                 (make-choose
                                  (map
                                   (lambda (n)
                                     (add-limit '()
                                                (add-limit '()
                                                           plan
                                                           (string->symbol (fourth step))
                                                           (customize-value (string->symbol (fifth step))
                                                                            (if (eq? test 'minimum) min-val n)
                                                                            (if (eq? test 'minimum) n max-val)))
                                                (car step) (customize-value (string->symbol (cadr step))
                                                                            (if (eq? test 'maximum) min-val n)
                                                                            (if (eq? test 'maximum) n max-val))))
                                   vals))))
                              (let* ((step (get-hyperparameter-constraint (car (cadr s))))
                                     (mins (get-schema-property schema-properties (car step) (cadr step) 'minimum))
                                     (maxs (get-schema-property schema-properties (car step) (cadr step) 'maximum)))
                                             (print "---")
                                (newline)
                                (print (car (cadr s)))
                                (newline)
                                (cond ((and step (equal? = (third step)))
                                       (let ((value
                                              (or (and (not (get-required? (car (cadr s))))
                                                       (get-schema-property schema-properties (car step) (cadr step) 'default))
                                                  (list (fourth step)))))
                                         (jsexpr->string
                                          (add-limit '() plan (car step) (customize-single-value (string->symbol (cadr step)) (car value)))
                                          #:null #\nul)))
                                      ((and (or (equal? <= (third step)) (equal? < (third step))) mins)
                                       (jsexpr->string
                                        (add-limit '() plan (car step)
                                                   (customize-value (string->symbol (cadr step)) (car mins) (fourth step)))))
                                      (else
                                       ; (print mins)
                                       ; (print maxs)
                                       (format "~S" (or (get-hyperparameter-constraint (car (cadr s))) s)))))))))
                  (format "~S" (or (get-hyperparameter-constraint (car (cadr s))) s)))))))))