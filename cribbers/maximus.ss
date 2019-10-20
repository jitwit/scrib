
(define crib-maximus
  (lambda (state)
    (cond ((state-discard? state)
           (let ((hand (state-discard-hand state)))
             (if (state-discard-dealer? state)
                 (discard (deal-maximize-points hand) hand)
                 (discard (pone-maximize-points hand) hand))))
          ((state-peg? state)
           (peg-worst-single state))
          (else (error 'crib-rando "unknown state" state)))))
