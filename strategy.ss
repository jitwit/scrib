
;;;; Strategy

;;; Heuristics
(define (deal-maximize-points-heuristic deck hand)
  (lambda (h)
    (+ (/ (v:fxsum (vector-map (lambda (c)
                                 (score-deal (cons c h)))
                               deck))
          (vector-length deck))
       (deal-discard (discard h hand)))))

(define (pone-maximize-points-heuristic deck hand)
  (lambda (h)
    (- (/ (v:fxsum (vector-map (lambda (c)
                                 (score-pone (cons c h)))
                               deck))
          (vector-length deck))
       (pone-discard (discard h hand)))))

(define (deal-maximize-crib-heuristic deck hand)
  (lambda (h)
    (deal-discard (discard h hand))))

(define (pone-minimize-crib-heuristic deck hand)
  (lambda (h)
    (- (pone-discard (discard h hand)))))

;;; Strategies

(define (deal-maximize-points hand)
  (let ((deck (list->vector (deck-without hand))))
    (discard-with-heuristic hand (deal-maximize-points-heuristic deck hand))))

(define (pone-maximize-points hand)
  (let ((deck (list->vector (deck-without hand))))
    (discard-with-heuristic hand (pone-maximize-points-heuristic deck hand))))

(define (deal-maximize-discard hand)
  (let ((deck (list->vector (deck-without hand))))
    (discard-with-heuristic hand (deal-maximize-crib-heuristic deck hand))))

(define (pone-minimize-discard hand)
  (let ((deck (list->vector (deck-without hand))))
    (discard-with-heuristic hand (pone-minimize-crib-heuristic deck hand))))

(define (discard-with-heuristic hand heuristic)
  (cdr (maximum-on (combinations hand 4) heuristic)))

(define (random-discard hand)
  (let ((discards (combinations hand 4)))
    (list-ref discards (random (length discards)))))

;;; Pegging

(define (peg-random state)
  (let ((moves (valid-pegs (state-peg-board state) (state-peg-hand state))))
    (if (null? moves)
        'go
        (list-ref moves (random (length moves))))))

(define (peg-best-single state)
  (let ((hand (state-peg-hand state))
        (board (state-peg-board state)))
    (let ((pegs (valid-pegs board hand)))
      (if (null? pegs)
          'go
          (cdr (maximum-on pegs (lambda (peg) (score-peg (cons peg board)))))))))

(define (peg-worst-single state)
  (let ((hand (state-peg-hand state))
        (board (state-peg-board state)))
    (let ((choices (maximums-on (valid-pegs board hand)
                                (lambda (peg)
                                  (- (score-peg (cons peg board)))))))
      (if (null? choices)
          'go
          (list-ref choices (random (length choices)))))))

(define (display-discard-strategy heuristic hand n)
  (let ((deck (list->vector (deck-without hand))))
    (let ((results (reverse (rank-on (combinations hand 4) (heuristic deck hand)))))
      (display-ln heuristic)
      (for-each (lambda (result)
                  (let ((keeps (cdr result))
                        (cribs (discard (cdr result) hand)))
                    (display-huge-hand keeps)
                    (display-huge-hand cribs)
                    (format #t "H: ~,2f~%~%"
                            (car result))))
                (list-head results n)))))


;;;; Reconstructing cribbage state from view

;;; randomly and dumbly reconstruct a crib state from a peg view
;;; in reconstructed view, A is us, B is opponent.
;;; the goal is then to maximize something like (- scoreA scoreB).
(define (peg->crib state discards)
  (let ((board-cards (state-peg-board* state))
        (dealer (if (state-peg-dealer? state) 'A 'B))
        (last-peg (last-peg-view->cribbage (state-peg-last-peg state))))
    (let* ((opponent-cards (board-view->opponent-cards board-cards))
           (remaining (- 4 (length opponent-cards)))
           (known-cards `(,(state-peg-cut state)
                          ,@(state-peg-hand state)
                          ,@discards
                          ,@(map car board-cards)))
           (deal (random-deal known-cards (+ 4 remaining)))
           (assumed-hand (list-head deal remaining))
           (random-crib (list-tail deal remaining)))
      (make-crib dealer
                 'A
                 (state-peg-scoreA state)
                 (state-peg-scoreB state)
                 (state-peg-hand state)
                 assumed-hand
                 random-crib
                 (state-peg-cut state)
                 last-peg
                 (state-peg-board state)
                 (board-view->cribbage board-cards)))))

(define (board-view->cribbage board-cards)
  (map (lambda (c.id)
         (cons (car c.id)
               (if (eq? 'you (cdr c.id))
                   'A
                   'B)))
       board-cards))

(define (last-peg-view->cribbage last-peg)
  (case last-peg
    ((you) 'A)
    ((opponent) 'B)
    (else #f)))

(define (board-view->opponent-cards board-cards)
  (filter-map (lambda (c.id)
                (and (eq? 'opponent (cdr c.id))
                     (car c.id)))
              board-cards))


