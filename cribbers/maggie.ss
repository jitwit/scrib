(define maggie-iterations
  (make-parameter 30))

;;; remember discards
(define (make-crib-maggie)
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
              (peg-from-random-states state discards (maggie-iterations)))))))))

;;; Maggie does min-max from a bunch of randomly reconstructed cribs
;;; to select the best peg.

;;; remember discards
(define (peg-from-random-states state discards trials)
  (let ((pegs (make-eq-hashtable)))
    (do ((i 0 (1+ i)))
        ((= i trials)
         (let ((peg-frequencies (vector->list (hashtable-cells pegs))))
           (cadr (maximum-on peg-frequencies cdr))))
      (let ((result (maggie-max (peg->crib state discards))))
        (hashtable-update! pegs (cdr result) fx1+ 0)))))

(define (maggie-max state)
  (maximum-on (cribbage-actions state)
              (lambda (action)
                (let ((result.action
                       (monte-search action (run-cribbage state action))))
                  (car result.action)))))

(define (maggie-min state)
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
         (maggie-max guess)
         (maggie-min guess)))
    (else (cons (- (crib-scoreA guess)
                   (crib-scoreB guess))
                action))))

(define Maggie
  (make-cribbot 'Maggie (make-crib-maggie)))
