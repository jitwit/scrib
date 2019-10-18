;;;; Encoding Cribbage

(define (crib:pair? xy)
  (apply = (map rank xy)))

(define (crib:rank card)
  (fx1+ (fxmin (rank card) 9)))

;;; Hand scores.
;; Expects argument variable hand to be the cut cons'ed to the hand.

(define (score-pairs hand)
  (fxsll (count crib:pair? (combinations hand 2)) 1))

(define (score-fifteens hand)
  (fxsll (count (lambda (subset)
                  (fx= 15 (fold-left fx+ 0 subset)))
                (subsets (map crib:rank hand)))
         1))

(define (score-flush hand)
  (let ((cut (car hand))
        (s0 (suit (cadr hand))))
    (if (andmap (lambda (c)
                  (fx= s0 (suit c)))
                (cddr hand))
        (fx+ 4 (if (fx= s0 (suit cut)) 1 0))
        0)))

(define (score-crib-flush hand)
  (let ((cut (car hand))
        (s0 (suit (cadr hand))))
    (if (and (andmap (lambda (c)
                       (fx= s0 (suit c)))
                     (cddr hand))
             (fx= s0 (suit cut)))
        5
        0)))

(define (score-runs hand)
  (let ((runs (group-with (lambda (x y)
                            (fx= (fx1+ (car x))
                                 (car y)))
                          (sort-on (eq-histogram
                                    (map rank hand))
                                   car))))
    (cond ((find (lambda (run)
                   (fx< 2 (length run)))
                 runs)
           =>
           (lambda (run)
             (let ((run (map cdr (car runs))))
               (fx* (fold-left fx* 1 run)
                    (length run)))))
          (else 0))))

(define (score-heels hand)
  (if (jack? (car hand)) 2 0))

(define (score-nobs hand)
  (let ((jack-suits (map suit (filter jack? (cdr hand))))
        (nob-suit (suit (car hand))))
    (if (memq nob-suit jack-suits)
        1
        0)))

(define (score-pone hand)
  (+ (score-runs hand)
     (score-fifteens hand)
     (score-pairs hand)
     (score-flush hand)
     (score-nobs hand)))

(define (score-deal hand)
  (+ (score-runs hand)
     (score-fifteens hand)
     (score-pairs hand)
     (score-flush hand)
     (score-nobs hand)
     (score-heels hand)))

(define (score-crib hand)
  (+ (score-runs hand)
     (score-fifteens hand)
     (score-pairs hand)
     (score-crib-flush hand)
     (score-nobs hand)))

;;; Peg Scores
;; Assumes the board is lifo stack
(define (crib-board-total board)
  (fold-left (lambda (total card)
               (fx+ total (crib:rank card)))
             0
             board))

(define (score-peg-count board)
  (case (crib-board-total board)
    ((15 31) 2)
    (else 0)))

(define (score-peg-pairs board)
  (if (pair? board)
      (let ((n (length (car (group-with = (map rank board))))))
        ;;; 2 * (choose n 2) pairs out of top grouping
        (fxsll (s:choose n 2) 1))
      0))

(define (simple-run? ranks n)
  (let ((ranks (sort < ranks)))
    (andmap (lambda (x y)
              (fx= (fx1+ x) y))
            (list-head ranks (fx1- n))
            (cdr ranks))))

(define (score-peg-runs board)
  (let ((n (length board)))
    (let loop ((prefixes (map reverse
                              (suffixes
                               (reverse
                                (map rank board)))))
               (n n))
      (if (fx< n 3)
          0
          (if (simple-run? (car prefixes) n)
              n
              (loop (cdr prefixes) (fx1- n)))))))

(define (score-peg board)
  (+ (score-peg-count board) ;; fifteens scored by count
     (score-peg-pairs board)
     (score-peg-runs board)))
