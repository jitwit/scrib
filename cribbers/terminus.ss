
(define crib-terminus
  (lambda (state)
    (cond ((state-discard? state)
           (prompt-discard state))
          ((state-peg? state)
           (prompt-peg state))
          (else (error 'crib-terminal "unknown state" state)))))

(define (valid-discards cards)
  (nub-eq (filter (lambda (card)
                    (and (number? card)
                         (<= 1 card 6)))
                  cards)))

(define (prompt-discard state)
  (format #t "select two cards [1-6]~%")
  (let loop ((choices '()))
    (if (= 2 (length choices))
        (map (lambda (choice)
               (list-ref (state-discard-hand state) (1- choice)))
             choices)
        (loop (valid-discards (cons (read) choices))))))

(define (valid-peg-selection? cards card)
  (or (and (null? cards)
           (eq? card 'go)
           card)
      (and (number? card)
           (<= 1 card (length cards))
           (list-ref cards (1- card)))))

(define (prompt-peg state)
  (let ((cards (valid-pegs (state-peg-board state) (state-peg-hand state))))
    (format #t "select a card [1-~a]~%" (length cards))
    (if (null? cards)
        'go
        (let loop ((choice (read)))
          (or (valid-peg-selection? cards choice)
              (loop (read)))))))

(define Jitwit
  (make-cribbot 'Jitwit crib-terminus))

