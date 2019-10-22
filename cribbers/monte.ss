
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
                            (- 8 (length opponent-cards)))))
    (if (state-peg-dealer? state)
        (make-crib 'A
                   'A
                   (state-peg-scoreA state)
                   (state-peg-scoreB state)
                   (state-peg-hand state)
                   (list-head deal (- 4 (length opponent-cards)))
                   (list-tail deal (- 4 (length opponent-cards)))
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
                   (list-head deal (- 4 (length opponent-cards)))
                   (list-tail deal (- 4 (length opponent-cards)))
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

;;; assumed to be called form search, where list of actions is
;;; non-empty
(define (maximin state)
  (let ((results (rank-on (cribbage-actions state)
                          (lambda (action)
                            (- (car (search action (run-cribbage state action))))))))
    (cons (- (caar results))
          (cdar results))))

(define (minimax state)
  (car
   (rank-on (cribbage-actions state)
            (lambda (action)
              (car (search action (run-cribbage state action)))))))

(define (search action guess)
  (case (game-phase guess)
    ((peg) (case (crib-turn guess)
             ((A) (maximin guess))
             ((B) (minimax guess))))
    (else (cons (- (crib-scoreA guess)
                   (crib-scoreB guess))
                action))))

(define (monte-carlo state trials)
  (let ((moves '()))
    (do ((i 0 (1+ i)))
        ((= i trials)
         (caar (sort-on (eq-histogram (map cdr moves))
                        (compose - cdr))))
      (let ((guess (peg->crib state)))
        (push! (maximin guess) moves)))))

;; given peg view, dumbly reconstruct a random state
(define (crib-monte state)
  (cond ((state-discard? state)
         (let ((hand (state-discard-hand state)))
           (if (state-discard-dealer? state)
               (discard (deal-maximize-points hand) hand)
               (discard (pone-maximize-points hand) hand))))
        ((state-peg? state) (monte-carlo state 100))))



