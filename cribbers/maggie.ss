(define maggie-iterations
  (make-parameter 24))

(define maggie-win-table
  (fetch-table win-probability-table))

(define maggie-chart
  (fetch-table win-probability-table))

;;; remember discards
(define (make-crib-maggie)
  (let ((discards '())
        (monte (cribbot-strategy (Monte))))
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
              (let ((choice (peg-from-random-states state discards (maggie-iterations))))
                ;;                (display-ln state)
                ;;                (display-ln 'Board)
                ;;                (display-huge-hand (state-peg-board state))
                ;;                (display-ln 'Hand)
                ;;                (display-huge-hand (state-peg-hand state))
                ;;                (display-huge-hand (list (monte state) choice))
                choice
                ))))))))

;;; Maggie does min-max from a bunch of randomly reconstructed cribs
;;; to select the best peg.

;;; remember discards
(define (peg-from-random-states state discards trials)
  (let ((pegs (make-eq-hashtable)))
    (do ((i 0 (1+ i)))
        ((= i trials)
         (let ((peg-frequencies (vector->list (hashtable-cells pegs))))
           ;;
           ;;           (display-histogram peg-frequencies) (newline) (newline)
           (cadr (maximum-on peg-frequencies cdr))))
      (let ((result (maggie-max (peg->crib* state discards))))
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
