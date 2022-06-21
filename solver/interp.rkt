#lang racket

(require mzlib/string)

(define (table model)
  (cdr (read-from-string (format "~A" model))))

(define (get-value s model-table)
  (let ((x (assoc s model-table)))
    (if (or (not x) (null? (cdr x)))
        '()
        (second x))))

(define (hyperparameter-required model-table)
  (get-value '??:mockup:31:12 model-table))

(define (get-real-op model-table)
  (cond ((get-value '0$choose:mockup:45:22 model-table) '<)
        ((get-value '1$choose:mockup:45:22 model-table) '>)
        ((get-value '2$choose:mockup:45:22 model-table) '<=)
        ((get-value '3$choose:mockup:45:22 model-table) '>=)
        (else '=)))

(define (constraint model-table)
  (get-value '??:mockup:33:20 model-table))

(define (related-hyperparameter model-table)
  (and (not (get-value '0$choose:mockup:45:45 model-table))
       (get-value '??:mockup:23:20 model-table)))

(define (get-real-value model-table)
  (and (get-value '0$choose:mockup:45:45 model-table)
       (get-value '??:mockup:45:53$choose:mockup:45:45 model-table)))

(define (get-string-value model-table)
  (get-value '??:mockup:40:31 model-table))

(define (get-boolean-value model-table)
  (get-value '??:mockup:37:28 model-table))

(define (get-arbitrary-value model-table)
  (get-value '??:mockup:50:21 model-table))

(define (explain model-table)
  (let ((h (constraint model-table))
        (rv (get-real-value model-table)))
    (writeln rv)
    (if (and rv (not (null? rv)))
        (list h (get-real-op model-table) rv)
        (let ((rh (related-hyperparameter model-table))
              (bv (get-boolean-value model-table))
              (sv (get-string-value model-table)))
          (writeln rh)
          (writeln bv)
          (writeln sv)
          (cond ((not (null? bv))
                 (list h "is" bv))
                ((not (null? sv))
                 (list h "=" sv))
                ((eq? #t (get-value '??:mockup:49:21 model-table))
                 (list h "must be present"))
                ((not (null? rh))
                 (list h (get-real-op model-table) rh))
                 (else
                 (list h "required")))))))
              

(provide (all-defined-out))