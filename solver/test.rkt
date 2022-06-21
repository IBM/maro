#lang rosette

(require "dataset.rkt")
(require "mockup.rkt")
(require "to-json.rkt")

(require json)
(require json-parsing)

(define dataset
   (call-with-input-file
    "../full_single_dataset_runs.json"
   (lambda (s) 
     (json->sjson s))))

(define (polynomial-random_forest steps params)
  (and (member 'random_forest_classifier steps)
       (member 'polynomial_features steps)))

(define (polynomial-random_forest-degree steps params)
  (and (polynomial-random_forest steps params)
       (member "polynomial_features.degree" (map car params))))

(define (pca-k_nearest_neighbors steps params)
  (and (member 'pca steps)
       (member 'k_neighbors_classifier steps)))

(define (all-runs f)
  (filter
   (lambda (run)
     (f (pipeline-steps (run-pipeline run)) (pipeline-parameters run)))
   (hash-ref dataset 'runs)))

(define (yes-runs f)
  (filter successful-run? (all-runs f)))

(define (no-runs f)
  (filter (lambda (r) (not (successful-run? r))) (all-runs f)))

(define (six-f f)
  (let ((yr (yes-runs f))
        (nr (no-runs f)))
  (solve-it
   (list
    (run-data (first nr))
    (run-data (second nr))
    (run-data (third nr))
    (run-data (fourth nr))
    (run-data (fifth nr))
    (run-data (sixth nr))
    (run-data (first yr))
    (run-data (second yr))
    (run-data (third yr))
    (run-data (fourth yr))
    (run-data (fifth yr))
    (run-data (sixth yr))))))

(define (all-f f)
  (let ((yr (yes-runs f))
        (nr (no-runs f)))
    (solve-it (map run-data (append yr nr)))))

(define (n-f n f)
  (let ((yr (first-n n (yes-runs f)))
        (nr (first-n n (no-runs f))))
    (solve-it (map run-data (append nr yr)))))

(define six-prf (six-f polynomial-random_forest))

(define six-pkn (six-f pca-k_nearest_neighbors))

(define (test-partition runs)
  (partition
   runs
   (list
    (lambda (r) (test-run r "polynomial_features.degree" "" #f #f "" string=? >= #f 3 #t))
    (lambda (r) (test-run r "polynomial_features.include_bias" "" #f #f "" string=? < #f 0.0 #t))
    (lambda (r) (test-run r "polynomial_features.interaction_only" "" #f #t "" string=? < #f 0.0 #t)))
   (lambda (rs) (list rs))))

(define prf-example
  (partition (map run-data (all-runs polynomial-random_forest)) (list
    (lambda (r) (test-run r "polynomial_features.degree" "" #f #f "" string=? >= #f 3 #t))
    (lambda (r) (test-run r "polynomial_features.include_bias" "" #f #f "" string=? < #f 0.0 #t))
    (lambda (r) (test-run r "polynomial_features.interaction_only" "" #f #t "" string=? < #f 0.0 #t)))
   (lambda (rs) (list (solve-it rs)))))

(define prf-example-unified
  (solve
   (assert
    (letrec ((s (lambda (formulae)
                  (if (null? formulae) #t (and (car formulae) (s (cdr formulae)))))))
      (s 
       (partition (map run-data (all-runs polynomial-random_forest))
                  (list
                   (lambda (r) (test-run r "polynomial_features.degree" "" #f #f "" >= #f 3 #t))
                   (lambda (r) (test-run r "polynomial_features.include_bias" "" #f #f "" < #f 0.0 #t))
                   (lambda (r) (test-run r "polynomial_features.interaction_only" "" #f #t "" < #f 0.0 #t)))
                  (lambda (rs) (list (sok-all rs)))))))))
