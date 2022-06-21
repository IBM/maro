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

(define data "")
(define m '())

(post "/pipelines"
  (lambda (req)
    (let* ((dataset (json->sjson (regexp-replace* #px"\\bNaN\\b" (params req 'pipelines) "null")))
           (plan (json->sjson (params req 'planned)))
           (schema-properties (pipeline-schema-properties dataset)))
      (set! data dataset)
      (let* ((runs (map run-data dataset))
             (model 
              (or (spartition-solve 0 runs)
                  (spartition-solve 1 runs))))
        (print model)
        (newline)
        (set! m model)
        (with-output-to-string
          (λ () (write-json 
                 (let ((to-strs
                        (lambda (s)
                          (let ((pairs (get-relevant-symbolic-values s runs)))
                            (apply hasheq
                                   (apply append
                                          (map (lambda (p) (list (string->symbol (car p)) (cadr p)))
                                               pairs)))))))
                   (if (null? (car (second model)))
                       (list (to-strs (caadr (second m))))
                       (list
                        (to-strs (caar (second model)))
                        (let ((outcomes
                               (lambda (x y z)
                                 (let ((s
                                        (remove-duplicates
                                         (map (lambda (r) (evaluate (x r) (car model)))
                                              (filter (lambda (r) (eq? z (evaluate ((car (third model)) r) (car model))))
                                                      runs)))))
                                   (if (= 1 (length s))
                                       (car s)
                                       (to-strs y))))))
                          (map outcomes (fourth model) (cadr (second model)) (list #t #f)))))))))))))
                            