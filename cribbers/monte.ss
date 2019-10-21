
;; randomly and dumbly reconstruct a crib state from a peg view
;; in reconstructed view, A is us, B is opponent.
;; the goal is then to maximize something like (- scoreA scoreB)
(define (peg->crib state)
  (let* ((board-cards (state-peg-board* state))
         (opponent-cards (filter-map (lambda (c.id)
                                       (and (eq? 'opponent (cdr c.id))
                                            (car c.id)))
                                     board-cards))
         (known-cards `(,@(state-peg-hand state)
                        ,@(state-peg-board state)
                        ,(state-peg-cut state)
                        ,@(map car board-cards)))
         (deal (random-deal known-cards
                            (- 8 (length board-cards)))))
    (if (state-peg-dealer? state)
        (make-crib 'A
                   'A
                   (state-peg-scoreA state)
                   (state-peg-scoreB state)
                   (state-peg-hand state)
                   (list-head deal (- 4 (length board-cards)))
                   (list-tail deal (- 4 (length board-cards)))
                   (state-peg-cut state)
                   (case (state-peg-last-peg state)
                     ((you) 'A)
                     ((opponent) 'B)
                     (else #f))
                   (state-peg-board state)
                   (map (lambda (c.id)
                          (cons (car c.id)
                                (if (eq? 'you (cdr c.id))
                                    'A
                                    'B)))
                        board-cards))
        (make-crib 'B
                   'A
                   (state-peg-scoreA state)
                   (state-peg-scoreB state)
                   (state-peg-hand state)
                   (list-head deal (- 4 (length board-cards)))
                   (list-tail deal (- 4 (length board-cards)))
                   (state-peg-cut state)
                   (case (state-peg-last-peg state)
                     ((you) 'A)
                     ((opponent) 'B)
                     (else #f))
                   (state-peg-board state)
                   (map (lambda (c.id)
                          (cons (car c.id)
                                (if (eq? 'you (cdr c.id))
                                    'A
                                    'B)))
                        board-cards)))))

(define (maximin state)
  (case (game-phase state)
    ((count) state)))

;; given peg view, dumbly reconstruct a random state
(define (peg-monte state)
  (let* ((hypothesis (peg->crib state))
         (action (random-element (cribbage-actions hypothesis))))
    hypothesis
    ))



