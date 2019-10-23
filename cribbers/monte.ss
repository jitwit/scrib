
(define monte-iterations
  (make-parameter 24))

;; given peg view, dumbly reconstruct a random state
(define (make-crib-monte)
  (let ((discards '()))
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
        (let ((pegs (valid-pegs (state-peg-board state) (state-peg-hand state))))
          (if (null? pegs)
              'go
              (monte-peg state discards (monte-iterations)))))))))

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

(define Monte
  (make-cribbot 'Monte (make-crib-monte)))
