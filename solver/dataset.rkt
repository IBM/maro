#lang racket

(define (successful-run? run)
  (let ((result (hash-ref run 'result)))
    (or
     (and (hash-has-key? result 'success)
          (equal? "True" (hash-ref result 'success)))
     (and (hash-has-key? result 'status)
          (equal? "ok" (hash-ref result 'status))))))

(define (run-pipeline run)
  (hash-ref run 'pipeline_definition))

(define (pipeline-steps pipeline)
  (letrec ([f (lambda (p)
                (if (hash-has-key? p 'steps)
                    (apply append
                           (cons (hash-keys (hash-ref p 'steps))
                                 (map f (hash-values (hash-ref p 'steps)))))
                    '()))])
    (if (not (hash-has-key? pipeline 'steps))
        (list 'top)
        (f pipeline))))

(define (pipeline-step pipeline step)
  (if (eq? step 'top)
      pipeline
      (letrec ([get (lambda (p)
                      (if (hash-has-key? p 'steps)
                          (let ((steps (hash-ref p 'steps)))
                            (if (hash-has-key? steps step)
                                (list (hash-ref steps step))
                                (apply append (map get steps))))
                          '()))])
        (car (get pipeline)))))
                            

(define (step-has-params? step)
  (and (hash-has-key? step 'hyperparams)
       (not (eq? #\nul (hash-ref step 'hyperparams)))))

(define (run-defaults run)
  (if (hash-has-key? run 'pipeline-defaults)
      (hash-ref run 'pipeline_defaults)
      (hash)))

(define (pipeline-parameters run)
  (let ((pipeline (run-pipeline run))
        (defaults (run-defaults run)))
    (apply append
           (map (lambda (s)
                  (let* ((step (pipeline-step pipeline s))
                         (nm (string->symbol (hash-ref step 'class)))
                         (dflts (if (hash-has-key? defaults nm)
                                    (hash-ref defaults nm)
                                    (hash))))
                     (if (step-has-params? step)
                        (let* ((params (hash-ref step 'hyperparams))
                               (all-keys
                                (remove-duplicates
                                 (append
                                  (hash-keys params)
                                  (hash-keys dflts)))))
                          (map (lambda (p)
                                 (list (format "~a.~a" s p)
                                       (if (hash-has-key? params p)
                                           (hash-ref params p)
                                           (hash-ref dflts p))))
                               all-keys))
                        '())))
                (pipeline-steps pipeline)))))

(define (run-data run)
  (list
   (hash-ref run 'pipeline_id)
   (successful-run? run)
   (pipeline-parameters run)))

(define (pipeline-schema-properties pipelines)
  (let ((clss
         (let ((pipes (map run-pipeline pipelines)))
           (remove-duplicates
            (apply append
                   (map 
                    (lambda (pipe)
                      (map (lambda (s) (cons s (hash-ref (pipeline-step pipe s) 'class))) (pipeline-steps pipe)))
                    pipes)))))
        (scs (map (lambda (p) (hash-ref p 'pipeline_schemas)) pipelines)))
    (remove-duplicates
     (apply append
            (map (lambda (c)
                   (apply append
                          (map (lambda (s)
                                 (let ((key (string->symbol (cdr c))))
                                   (if (hash-has-key? s key)
                                       (apply append
                                              (map (lambda (t)
                                                     (if (hash-has-key? t 'properties)
                                                         (let ((props (hash-ref t 'properties)))
                                                           (apply append
                                                                  (map (lambda (p)
                                                                         (let ((t (hash-ref props p)))
                                                                           (append
                                                                            (if (hash-has-key? t 'type)
                                                                                (list (list (car c) p 'type (hash-ref t 'type)))
                                                                                '())
                                                                            (if (hash-has-key? t 'default)
                                                                                (list (list (car c) p 'default (hash-ref t 'default)))
                                                                                '())
                                                                            (if (hash-has-key? t 'maximum)
                                                                                (list (list (car c) p 'maximum (hash-ref t 'maximum)))
                                                                                '())
                                                                            (if (hash-has-key? t 'minimum)
                                                                                (list (list (car c) p 'minimum (hash-ref t 'minimum)))
                                                                                '()))))
                                                                       (hash-keys props))))
                                                         '()))
                                                   (hash-ref (hash-ref s key) 'allOf)))
                                       '())))
                               scs)))
                 clss)))))

(define (get-schema-property schema-properties step parameter property)
  (let* ((s (if (string? step) (string->symbol step) step))
         (h (if (string? parameter) (string->symbol parameter) parameter))
         (p (if (string? property) (string->symbol property) property))
         (l (filter
             (lambda (e) (and (equal? (car e) s) (equal? (cadr e) h) (equal? (caddr e) p)))
             schema-properties)))
    (map fourth l)))

(provide (all-defined-out))
