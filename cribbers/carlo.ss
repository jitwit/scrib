;;; monte but with pegging-informed discards

(define carlo-iterations
  (make-parameter 30))

;; given peg view, dumbly reconstruct a random state
(define (make-crib-carlo)
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
            (monte-carlo-peg state discards trials)))))))

(define (search-carlo-dealer state trials)
  (let ((moves '()))
    (do ((i 0 (1+ i)))
        ((= i trials)
         (let ((results (equal-histogram moves)))
           (display-ln results)
           (maximum-on results cdr)))
      (push! (run-carlo-dealer (discard->crib state)) moves))))

;; cribbage state. want to study AVERAGE BEST FINAL SCORE? 
(define (run-carlo-dealer state)
  (let ((pone-hand (current-players-hand state)))
    (let ((state* (run-cribbage state (discard (pone-maximize-points
                                                (current-players-hand state))
                                               pone-hand))))
      (maximum-on (combinations (current-players-hand state*) 2)
                  (lambda (discard)
                    (carlo-minimax (run-cribbage state* discard)))))))

;;; assumed to be called form search, where list of actions is
;;; non-empty
(define (carlo-maximin state)
  (car
   (maximum-on (cribbage-actions state)
               (lambda (action)
                 (carlo-search action (run-cribbage state action))))))

(define (carlo-minimax state)
  (let ((result (maximum-on (cribbage-actions state)
                            (lambda (action)
                              (- (carlo-search action (run-cribbage state action)))))))
    (- (car result))))

(define (carlo-search action state)
  (case (game-phase state)
    ((peg)
     (if (eq? 'A (crib-turn state))
         (carlo-maximin state)
         (carlo-minimax state)))
    (else
     (let ((state (execute-count state)))
       (- (crib-scoreA state)
          (crib-scoreB state))))))

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

;;; randomly deal opponent's hand
(define (discard->crib state)
  (let ((deal (random-deal (state-discard-hand state) 7))
        (dealer (if (state-discard-dealer? state) 'A 'B)))
    (let ((opponent-cards (cdr deal))
          (cut-card (car deal)))
      (make-crib dealer
                 (opposite-player dealer)
                 (state-discard-scoreA state)
                 (state-discard-scoreB state)
                 (state-discard-hand state)
                 opponent-cards
                 '()
                 cut-card
                 #f
                 '()
                 '()))))
