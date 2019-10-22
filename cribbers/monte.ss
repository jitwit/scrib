
(define monte-iterations
  (make-parameter 30))

;; given peg view, dumbly reconstruct a random state
(define (make-crib-monte)
  (let ((discards '())
        (trials (monte-iterations)))
    (lambda (state)
      (cond
       ((state-discard? state)
        (let ((hand (state-discard-hand state)))
          (cond ((state-discard-dealer? state)
                 (set! discards (discard (deal-maximize-points hand) hand))
                 discards)
                (else
                 (set! discards (discard (pone-maximize-points hand) hand))
                 discards))))
       ((state-peg? state)
        (if (null? (valid-pegs (state-peg-board state) (state-peg-hand state)))
            'go
            (monte-peg state discards trials)))))))

(define (monte-peg state discards trials)
  (let ((moves '()))
    (do ((i 0 (1+ i)))
        ((= i trials)
         (cadr (maximum-on (eq-histogram (map cdr moves)) cdr)))
      (let ((guess (peg->crib state discards)))
        ;; start with maximin because we know it's our turn when this is called
        (push! (monte-maximin guess) moves)))))


;;; assumed to be called form search, where list of actions is
;;; non-empty
(define (monte-maximin state)
  (maximum-on (cribbage-actions state)
              (lambda (action)
                (let ((result.action
                       (monte-search action (run-cribbage state action))))
                  (car result.action)))))

(define (monte-minimax state)
  (let ((result (maximum-on (cribbage-actions state)
                            (lambda (action)
                              (let ((result.action
                                     (monte-search action (run-cribbage state action))))
                                (- (car result.action)))))))
    (cons (- (car result)) (cdr result))))

(define (monte-search action guess)
  (case (game-phase guess)
    ((peg)
     (if (eq? 'A (crib-turn guess))
         (monte-maximin guess)
         (monte-minimax guess)))
    (else (cons (- (crib-scoreA guess)
                   (crib-scoreB guess))
                action))))

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

;; randomly and dumbly reconstruct a crib state from a peg view
;; in reconstructed view, A is us, B is opponent.
;; the goal is then to maximize something like (- scoreA scoreB)
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
