(define maggie-iterations
  (make-parameter 30))

(define maggie-win-table
  (fetch-table win-probability-table))

(define maggie-chart
  (fetch-table win-probability-table))

(define (peg-score state)
  (min (state-peg-scoreA state)
       (state-peg-scoreB state)))

(define (discard-score state)
  (min (state-discard-scoreA state)
       (state-discard-scoreB state)))

(define (maggie-deal-heuristic a b deck hand)
  (lambda (h)
    (let ((a (inexact->exact (floor (min 121.
                                         (+ a
                                            peg-deal-colvert
                                            (/ (v:sum (vector-map (lambda (c)
                                                                    (score-hand (cons c h)))
                                                                  deck))
                                               (vector-length deck))
                                            (deal-discard (discard h hand)))))))
          (b (inexact->exact (floor (min 121.
                                         (+ b
                                            hand-pone-fuller
                                            peg-pone-colvert))))))
      (- 1 (matrix-ref maggie-chart b a)))))

(define (maggie-pone-heuristic a b deck hand)
  (lambda (h)
    (let ((a (inexact->exact (round (min 121.
                                         (+ a
                                            peg-pone-colvert
                                            (/ (v:sum (vector-map (lambda (c)
                                                                    (score-hand (cons c h)))
                                                                  deck))
                                               (vector-length deck)))))))
          (b (inexact->exact (round (min 121.
                                         (+ b
                                            (pone-discard (discard h hand))
                                            hand-deal-fuller
                                            peg-deal-colvert))))))
      (matrix-ref maggie-chart a b))))

(define (maggie-deal-discard state)
  (let* ((hand (state-discard-hand state))
         (deck (list->vector (deck-without hand))))
    (discard-with-heuristic hand
                            (maggie-deal-heuristic (state-discard-scoreA state)
                                                   (state-discard-scoreB state)
                                                   deck
                                                   hand))))

(define (maggie-pone-discard state)
  (let* ((hand (state-discard-hand state))
         (deck (list->vector (deck-without hand))))
    (discard-with-heuristic hand
                            (maggie-pone-heuristic (state-discard-scoreA state)
                                                   (state-discard-scoreB state)
                                                   deck
                                                   hand))))

;;; remember discards
(define (make-crib-maggie)
  (let ((discards '())
        (monte (make-crib-monte (maggie-iterations))))
    (lambda (state)
      (cond
       ((state-discard? state)
        (let ((hand (state-discard-hand state)))
          (cond ((state-discard-dealer? state)
                 (set! discards (discard (if (< (discard-score state) 80)
                                             (deal-maximize-points hand)
                                             (maggie-deal-discard state))
                                         hand))
                 discards)
                (else
                 (set! discards (discard (if (< (discard-score state) 80)
                                             (pone-maximize-points hand)
                                             (maggie-pone-discard state))
                                         hand))
                 discards))))
       ((state-peg? state)
        (let ((pegs (valid-pegs (state-peg-board state) (state-peg-hand state))))
          (if (null? pegs)
              'go
              (if (< (peg-score state) 60)
                  (monte state)
                  (let ((choice (peg-from-random-states state discards (maggie-iterations))))
                    choice)))))))))

;;; Maggie does min-max from a bunch of randomly reconstructed cribs
;;; to select the best peg.

;;; remember discards
(define (peg-from-random-states state discards trials)
  (let ((pegs (make-eq-hashtable)))
    (do ((i 0 (1+ i)))
        ((= i trials)
         (let ((peg-frequencies (vector->list (hashtable-cells pegs))))
           ;;           (newline) (newline) (newline)
           ;;           (display-histogram peg-frequencies)
           ;;           (display-huge-hand (list (cadr (maximum-on peg-frequencies cdr))))
           (cadr (maximum-on peg-frequencies cdr))))
      (let ((result (maggie-max (peg->crib state discards))))
        (hashtable-update! pegs (cdr result) fx1+ 0)))))

(define (maggie-max state)
  (maximum-on (cribbage-actions state)
              (lambda (action)
                (let ((result.action
                       (maggie-search action (run-cribbage state action))))
                  (car result.action)))))

(define (maggie-min state)
  (let ((result (maximum-on (cribbage-actions state)
                            (lambda (action)
                              (let ((result.action
                                     (maggie-search action (run-cribbage state action))))
                                (- (car result.action)))))))
    (cons (- (car result)) (cdr result))))

(define (maggie-search action guess)
  (case (game-phase guess)
    ((peg)
     (if (eq? 'A (crib-turn guess))
         (maggie-max guess)
         (maggie-min guess)))
    ((won)
     (if (= 121 (crib-scoreA guess))
         (cons 1 action)
         (cons 0 action)))
    ((count)
     (let ((score (current-score (execute-count guess))))
       (cons (if (eq? 'A (car score))
                 (apply matrix-ref maggie-win-table (cdr score))
                 (- 1 (apply matrix-ref maggie-win-table (reverse (cdr score)))))
             action)))))

(define (Maggie)
  (make-cribbot 'Maggie (make-crib-maggie)))
