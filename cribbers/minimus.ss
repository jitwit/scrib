
(define crib-minimus
  (lambda (state)
    (cond ((state-discard? state)
           (let ((hand (state-discard-hand state)))
             (if (state-discard-dealer? state)
                 (discard (deal-maximize-points hand) hand)
                 (discard (pone-minimize-discard hand) hand))))
          ((state-peg? state)
           (peg-best-single state))
          (else (error 'crib-rando "unknown state" state)))))
