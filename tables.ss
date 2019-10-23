;;;; Cribbing tables
;;; for now will go with rasmussen's as it's from real play against experts.
(define (deal-discard cards)
  (crib-discard deal-rasmussen cards))

(define (pone-discard cards)
  (crib-discard pone-rasmussen cards))

(define (crib-discard table cards)
  (vector-ref (vector-ref table (rank (car cards)))
              (rank (cadr cards))))

(define deal-rasmussen
  '#(#(5.51 4.35 4.69 5.42 5.38 3.98 4.05 3.77 3.49 3.51 3.57 3.50 3.36)
     #(4.35 5.82 7.14 4.64 5.54 4.15 3.78 3.82 3.91 3.71 4.05 3.86 3.57)
     #(4.69 7.13 6.08 5.13 5.97 4.05 3.33 4.13 4.09 3.51 4.07 3.65 3.89)
     #(5.41 4.63 5.12 5.54 6.53 3.95 3.61 3.77 3.82 3.60 3.98 3.63 3.61)
     #(5.38 5.53 5.97 6.53 8.88 6.81 6.01 5.56 5.43 6.70 7.09 6.59 6.73)
     #(3.97 4.15 4.05 3.95 6.80 5.76 5.14 4.63 5.11 3.31 3.45 3.73 3.21)
     #(4.05 3.77 3.33 3.61 6.00 5.14 5.87 6.44 4.06 3.59 3.83 3.39 3.47)
     #(3.76 3.82 4.13 3.77 5.56 4.63 6.44 5.50 4.77 3.72 3.93 3.19 3.04)
     #(3.49 3.90 4.08 3.82 5.43 5.11 4.06 4.76 5.21 4.40 4.01 2.99 3.07)
     #(3.50 3.71 3.51 3.60 6.69 3.31 3.59 3.72 4.39 4.72 4.76 3.17 2.84)
     #(3.56 4.05 4.06 3.98 7.08 3.45 3.83 3.92 4.01 4.75 5.28 4.83 3.92)
     #(3.50 3.85 3.64 3.63 6.59 3.73 3.38 3.19 2.99 3.16 4.82 4.93 3.48)
     #(3.36 3.56 3.89 3.61 6.72 3.20 3.46 3.04 3.07 2.83 3.92 3.48 4.30)))

(define deal-colvert
  '#(#(5.4 4.1 4.4 5.4 5.5 3.8 3.8 3.8 3.4 3.4 3.7 3.4 3.4)
     #(4.1 5.7 6.9 4.4 5.4 3.8 3.8 3.6 3.7 3.5 3.8 3.5 3.5)
     #(4.4 6.9 5.9 4.7 5.9 3.7 3.7 3.9 3.7 3.6 3.8 3.5 3.5)
     #(5.4 4.4 4.7 5.7 6.4 3.8 3.8 3.8 3.7 3.6 3.8 3.5 3.5)
     #(5.5 5.4 5.9 6.4 8.6 6.5 6.0 5.4 5.4 6.6 6.9 6.6 6.6)
     #(3.8 3.8 3.7 3.8 6.5 5.8 4.8 4.5 5.2 3.1 3.4 3.1 3.1)
     #(3.8 3.8 3.7 3.8 6.0 4.8 5.9 6.6 4.0 3.1 3.5 3.2 3.2)
     #(3.8 3.6 3.9 3.8 5.4 4.5 6.6 5.4 4.6 3.8 3.4 3.2 3.2)
     #(3.4 3.7 3.7 3.7 5.4 5.2 4.0 4.6 5.2 4.2 3.9 3.0 3.1)
     #(3.4 3.5 3.6 3.6 6.6 3.1 3.1 3.8 4.2 4.8 4.5 3.4 2.8)
     #(3.7 3.8 3.8 3.8 6.9 3.4 3.5 3.4 3.9 4.5 5.3 4.7 3.9)
     #(3.4 3.5 3.5 3.5 6.6 3.1 3.2 3.2 3.0 3.4 4.7 4.8 3.4)
     #(3.4 3.5 3.5 3.5 6.6 3.1 3.2 3.2 3.1 2.8 3.9 3.4 4.8)))

(define deal-hessel
  '#(#(5.26 4.18 4.47 5.45 5.48 3.80 3.73 3.70 3.33 3.37 3.65 3.39 3.42)
     #(4.18 5.67 6.97 4.51 5.44 3.87 3.81 3.58 3.63 3.51 3.79 3.52 3.55)
     #(4.47 6.97 5.90 4.88 6.01 3.72 3.67 3.84 3.66 3.61 3.88 3.62 3.66)
     #(5.45 4.51 4.88 5.65 6.54 3.87 3.74 3.84 3.69 3.62 3.89 3.63 3.67)
     #(5.48 5.44 6.01 6.54 8.95 6.65 6.04 5.49 5.47 6.68 7.04 6.71 6.70)
     #(3.80 3.87 3.72 3.87 6.65 5.74 4.94 4.70 5.11 3.15 3.40 3.08 3.13)
     #(3.73 3.81 3.67 3.74 6.04 4.94 5.98 6.58 4.06 3.10 3.43 3.17 3.21)
     #(3.70 3.58 3.84 3.84 5.49 4.70 6.58 5.42 4.74 3.86 3.39 3.16 3.20)
     #(3.33 3.63 3.66 3.69 5.47 5.11 4.06 4.74 5.09 4.27 3.98 2.97 3.05)
     #(3.37 3.51 3.61 3.62 6.68 3.15 3.10 3.86 4.27 4.73 4.64 3.36 2.86)
     #(3.65 3.79 3.88 3.89 7.04 3.40 3.43 3.39 3.98 4.64 5.37 4.90 4.07)
     #(3.39 3.52 3.62 3.63 6.71 3.08 3.17 3.16 2.97 3.36 4.90 4.66 3.50)
     #(3.42 3.55 3.66 3.67 6.70 3.13 3.21 3.20 3.05 2.86 4.07 3.50 4.62)))

(define deal-fuller-systems
  '#(#(5.2 4.4 4.6 5.2 5.2 3.7 3.7 3.7 3.3 3.3 3.5 3.3 3.3)
     #(4.4 5.8 6.9 4.6 5.2 3.9 3.9 3.7 3.7 3.6 3.8 3.6 3.6)
     #(4.6 6.9 5.9 5.0 5.9 3.8 3.8 3.9 3.7 3.6 3.9 3.7 3.7)
     #(5.2 4.6 5.0 5.5 6.3 3.9 3.7 3.9 3.6 3.4 3.7 3.5 3.5)
     #(5.2 5.2 5.9 6.3 8.5 6.4 5.8 5.3 5.1 6.3 6.7 6.4 6.3)
     #(3.7 3.9 3.8 3.9 6.4 5.6 4.9 4.6 4.9 3.0 3.2 3.0 2.9)
     #(3.7 3.9 3.8 3.7 5.8 4.9 5.8 6.4 4.0 3.1 3.3 3.1 3.1)
     #(3.7 3.7 3.9 3.9 5.3 4.6 6.4 5.3 4.5 3.7 3.3 3.1 3.0)
     #(3.3 3.7 3.7 3.6 5.1 4.9 4.0 4.5 4.9 4.1 3.7 2.8 2.8)
     #(3.3 3.6 3.6 3.4 6.3 3.0 3.1 3.7 4.1 4.6 4.3 3.3 2.7)
     #(3.5 3.8 3.9 3.7 6.7 3.2 3.3 3.3 3.7 4.3 5.1 4.5 3.8)
     #(3.3 3.6 3.7 3.5 6.4 3.0 3.1 3.1 2.8 3.3 4.5 4.5 3.4)
     #(3.3 3.6 3.7 3.5 6.3 2.9 3.1 3.0 2.8 2.7 3.8 3.4 4.0)))

(define pone-rasmussen
  '#(#(5.59 5.17 4.96 5.62 5.81 4.97 4.81 4.84 4.34 4.54 4.64 4.24 4.33)
     #(5.17 6.19 7.52 5.21 5.79 4.79 4.80 4.90 4.57 4.54 4.61 4.58 4.45)
     #(4.95 7.52 6.11 5.74 6.72 4.81 4.85 5.20 5.18 4.58 4.71 4.61 4.43)
     #(5.61 5.20 5.74 6.00 6.44 5.06 5.00 4.94 4.57 4.58 5.14 4.50 4.36)
     #(5.81 5.79 6.72 6.43 9.09 6.87 7.08 6.39 6.06 7.22 8.14 7.10 7.13)
     #(4.96 4.79 4.81 5.05 6.86 6.30 6.18 5.86 6.20 4.22 4.53 4.14 4.08)
     #(4.81 4.80 4.84 4.99 7.08 6.17 6.93 6.67 5.10 4.17 4.69 4.24 4.25)
     #(4.84 4.90 5.19 4.93 6.39 5.86 6.67 7.91 5.89 5.59 4.58 4.30 4.15)
     #(4.33 4.57 5.17 4.57 6.06 6.20 5.10 5.89 6.52 5.30 4.86 4.12 3.94)
     #(4.54 4.53 4.57 4.57 7.21 4.22 4.17 5.58 5.29 6.19 5.95 4.64 3.85)
     #(4.64 4.61 4.70 5.14 8.13 4.53 4.69 4.57 4.86 5.95 5.64 5.46 4.63)
     #(4.23 4.57 4.61 4.50 7.10 4.14 4.24 4.29 4.11 4.63 5.46 5.36 4.52)
     #(4.33 4.45 4.43 4.36 7.12 4.07 4.24 4.15 3.93 3.84 4.62 4.51 5.59)))

(define pone-hessel
  '#(#(6.07 5.07 5.17 5.74 6.06 4.93 4.95 4.92 4.66 4.46 4.72 4.41 4.34)
     #(5.07 6.43 7.34 5.44 6.17 5.13 5.12 5.03 4.82 4.64 4.91 4.60 4.53)
     #(5.17 7.34 6.78 6.10 6.85 4.92 5.16 5.08 4.82 4.70 4.97 4.66 4.59)
     #(5.74 5.44 6.10 6.59 7.46 5.47 4.91 5.02 4.75 4.55 4.80 4.49 4.43)
     #(6.06 6.17 6.85 7.46 9.39 7.66 7.08 6.36 6.22 7.46 7.75 7.42 7.31)
     #(4.93 5.13 4.92 5.47 7.66 7.17 6.64 6.05 6.31 4.41 4.61 4.29 4.25)
     #(4.95 5.12 5.16 4.91 7.08 6.64 7.25 7.88 5.46 4.44 4.73 4.44 4.38)
     #(4.92 5.03 5.08 5.02 6.36 6.05 7.88 6.76 5.97 5.02 4.65 4.38 4.31)
     #(4.66 4.82 4.82 4.75 6.22 6.31 5.46 5.97 6.44 5.52 4.98 4.14 4.13)
     #(4.46 4.64 4.70 4.55 7.46 4.41 4.44 5.02 5.52 6.11 5.60 4.65 3.99)
     #(4.72 4.91 4.97 4.80 7.75 4.61 4.73 4.65 4.98 5.60 6.56 5.55 4.89)
     #(4.41 4.60 4.66 4.49 7.42 4.29 4.44 4.38 4.14 4.65 5.55 5.89 4.56)
     #(4.34 4.53 4.59 4.43 7.31 4.25 4.38 4.31 4.13 3.99 4.89 4.56 5.72)))

(define pone-colvert
  '#(#(6.2 5.0 5.1 5.7 6.0 4.9 4.9 4.8 4.6 4.4 4.7 4.4 4.3)
     #(5.0 6.4 7.3 5.3 6.1 5.0 5.0 4.9 4.8 4.6 4.8 4.5 4.4)
     #(5.1 7.3 6.8 5.9 6.7 4.9 5.0 5.0 4.8 4.6 4.9 4.5 4.4)
     #(5.7 5.3 5.9 6.6 7.2 5.3 4.8 4.9 4.7 4.5 4.7 4.4 4.3)
     #(6.0 6.1 6.7 7.2 9.3 7.4 6.9 6.2 6.1 7.4 7.6 7.3 7.2)
     #(4.9 5.0 4.9 5.3 7.4 7.0 6.4 5.7 6.3 4.3 4.5 4.2 4.1)
     #(4.9 5.0 5.0 4.8 6.9 6.4 7.1 7.8 5.2 4.3 4.7 4.3 4.2)
     #(4.8 4.9 5.0 4.9 6.2 5.7 7.8 6.6 5.7 4.9 4.6 4.3 4.2)
     #(4.6 4.8 4.8 4.7 6.1 6.3 5.2 5.7 6.3 5.4 5.0 4.1 4.0)
     #(4.4 4.6 4.6 4.5 7.4 4.3 4.3 4.9 5.4 6.0 5.4 4.5 3.8)
     #(4.7 4.8 4.9 4.7 7.6 4.5 4.7 4.6 5.0 5.4 6.5 5.4 4.7)
     #(4.4 4.5 4.5 4.4 7.3 4.2 4.3 4.3 4.1 4.5 5.4 5.8 4.4)
     #(4.3 4.4 4.4 4.3 7.2 4.1 4.2 4.2 4.0 3.8 4.7 4.4 5.6)))

(define pone-fuller-systems
  '#(#(5.4 4.5 4.7 5.3 5.5 4.4 4.3 4.4 4.1 3.9 4.2 3.9 3.9)
     #(4.5 5.7 6.7 4.8 5.5 4.6 4.5 4.4 4.3 4.1 4.4 4.1 4.1)
     #(4.7 6.7 6.0 5.4 6.0 4.4 4.5 4.5 4.3 4.2 4.5 4.2 4.1)
     #(5.3 4.8 5.4 5.7 6.5 4.7 4.3 4.4 4.3 4.0 4.3 4.0 4.0)
     #(5.5 5.5 6.0 6.5 7.4 6.6 6.1 5.6 5.5 6.4 6.7 6.4 6.4)
     #(4.4 4.6 4.4 4.7 6.6 6.2 5.8 5.4 5.5 3.9 4.1 3.8 3.8)
     #(4.3 4.5 4.5 4.3 6.1 5.8 6.2 6.7 4.8 3.9 4.2 3.9 3.9)
     #(4.4 4.4 4.5 4.4 5.6 5.4 6.7 5.8 5.3 4.5 4.1 3.9 3.8)
     #(4.1 4.3 4.3 4.3 5.5 5.5 4.8 5.3 5.5 4.8 4.4 3.7 3.7)
     #(3.9 4.1 4.2 4.0 6.4 3.9 3.9 4.5 4.8 5.1 5.0 4.1 3.5)
     #(4.2 4.4 4.5 4.3 6.7 4.1 4.2 4.1 4.4 5.0 5.5 5.0 4.4)
     #(3.9 4.1 4.2 4.0 6.4 3.8 3.9 3.9 3.7 4.1 5.0 5.0 4.0)
     #(3.9 4.1 4.1 4.0 6.4 3.8 3.9 3.8 3.7 3.5 4.4 4.0 4.8)))


;;;; Calculated Tables

;;; Basic API
(define (build-2d-table x y)
  (let ((table (make-vector x)))
    (do ((i 0 (1+ i)))
        ((= i y) table)
      (vector-set! table i (make-vector y)))))

(define (get-2d-entry table x y)
  (vector-ref (vector-ref table x) y))

(define (set-2d-entry! table x y entry)
  (vector-set! (vector-ref table x)
               y
               entry))

(define (update-2d-entry table x y f)
  (set-2d-entry! table x y
                 (f (get-2d-entry table x y))))

;;; Persistent tables for expensive calculations
(define (build-table table)
  (let ((table-file (cribbage-table-location table)))
    (unless (file-exists? table-file)
      (format #t "Cooking table ~a~%" table)
      (let ((table ((cribbage-table-recipe table))))
        (format #t "Writing table to ~a~%" table-file)
        (with-output-to-file table-file
          (lambda ()
            (write table)))))))

(define (forced-build-table table)
  (let ((table-file (cribbage-table-location table)))
    (when (file-exists? table-file)
      (delete-file table-file))
    (format #t "Cooking table ~a~%" table)
    (let ((table ((cribbage-table-recipe table))))
      (format #t "Writing table to ~a~%" table-file)
      (with-output-to-file table-file
        (lambda ()
          (write table))))))

(define (fetch-table table)
  (build-table table)
  (with-input-from-file (cribbage-table-location table) read))

(define hand-table-deal-maximize-points
  (make-cribbage-table "calculations/deal-maximize-points.ss"
                       (lambda ()
                         (build-card-frequency-table deal-maximize-points (table-cutoff)))))

(define occurrence-table-deal-maximize-points
  (make-cribbage-table "calculations/pairs-deal-maximize-points.ss"
                       (lambda ()
                         (build-card-occurrence-table deal-maximize-points (table-cutoff)))))

(define hand-table-pone-maximize-points
  (make-cribbage-table "calculations/pone-maximize-points.ss"
                       (lambda ()
                         (build-card-frequency-table pone-maximize-points (table-cutoff)))))

(define occurrence-table-pone-maximize-points
  (make-cribbage-table "calculations/pairs-pone-maximize-points.ss"
                       (lambda ()
                         (build-card-occurrence-table pone-maximize-points (table-cutoff)))))

(define cached-cribbage-tables
  (list hand-table-deal-maximize-points
        occurrence-table-deal-maximize-points
        hand-table-pone-maximize-points
        occurrence-table-pone-maximize-points))

(define (build-all-tables)
  (format #t "Doing ~a iterations per table~%" (table-cutoff))
  (for-all (lambda (table)
             (time (forced-build-table table)))
           cached-cribbage-tables))

;;; Estimating hands from discard strategies.
(define (build-card-occurrence-table discard-strategy trials)
  (let ((table (build-2d-table 13 13)))
    (do ((i 0 (fx1+ i)))
        ((= i trials) table)
      (let ((hand (list-head (deck) 6)))
        (for-all (lambda (xy)
                   (simple-progress-bar (format "~a ~a/~a~%" 'Pairs i trials) i trials)
                   (let-values (((x y) (apply values (map rank xy))))
                     (cond ((= x y)
                            (update-2d-entry table x y 1+))
                           (else
                            (update-2d-entry table x y 1+)
                            (update-2d-entry table y x 1+)))))
                 (combinations (discard-strategy hand) 2))))))

(define (build-card-frequency-table discard-strategy trials)
  (let ((counts (make-vector 13)))
    (do ((i 0 (fx1+ i)))
        ((= i trials) counts)
      (simple-progress-bar (format "~a ~a/~a~%" 'Frequencies i trials) i trials)
      (let ((hand (list-head (deck) 6)))
        (for-all (lambda (card)
                   (vector-inc! counts (rank card)))
                 (discard-strategy hand))))))

