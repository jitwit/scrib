
(define crib-minimus
  (lambda (state)
    (cond ((state-discard? state)
           (let ((hand (state-discard-hand state)))
             (if (state-discard-dealer? state)
                 (discard (deal-maximize-points hand) hand)
                 (discard (pone-minimize-discard hand) hand))))
          ((state-peg? state)
           (let ((moves (valid-pegs (state-peg-board state)
                                    (state-peg-hand state))))
             (if (null? moves)
                 'go
                 (list-ref moves (random (length moves))))))
          (else (error 'crib-rando "unknown state" state)))))
