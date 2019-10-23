
(define maggie-iterations
  (make-parameter 12))

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
          (cond ((null? pegs) 'go)
                ((null? (cdr pegs)) (car pegs))
                (else (peg-from-random-states state discards (maggie-iterations))))))))))

;;; Maggie does min-max from a bunch of randomly reconstructed cribs
;;; to select the best peg.
(define Maggie
  (make-cribbot 'Maggie (make-crib-maggie)))

(define (random-element-weighted W n)
  (let ((r (random n)))
    (let loop ((r r) (W W))
      (if (or (null? (cdr W))
              (< r (cdar W)))
          (caar W)
          (loop (- r (cdar W)) (cdr W))))))

;;; remember discards
(define (peg-from-random-states state discards trials)
  (let ((pegs (make-eq-hashtable)))
    (do ((i 0 (1+ i)))
        ((= i trials)
         ;;
         ;;         (display-ln (hashtable-cells pegs))
         ;;         (cadr (maximum-on (vector->list (hashtable-cells pegs)) cdr))
         ;;
         (random-element-weighted (vector->list (hashtable-cells pegs)) trials)
         )
      (let ((result (maggie-max (peg->crib state discards))))
        ;; start with maximin because we know it's our turn when this is called
        (hashtable-update! pegs
                           (cdr result)
                           fx1+
                           0)))))

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

