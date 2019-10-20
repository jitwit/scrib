
(define crib-rando
  (lambda (state)
    (cond ((state-discard? state)
           (let ((discards (combinations (state-discard-hand state) 2)))
             (list-ref discards (random (length discards)))))
          ((state-peg? state)
           (let ((moves (valid-pegs (state-peg-board state)
                                    (state-peg-hand state))))
             (if (null? moves)
                 'go
                 (list-ref moves (random (length moves))))))
          (else (error 'crib-rando "unknown state" state)))))