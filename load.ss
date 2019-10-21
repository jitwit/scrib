(import (euler))

(print-gensym #f)

(define agents
  '(rando
    maximus
    minimus
    terminal))

(define source-files
  '("term.ss"
    "playing-cards.ss"
    "records.ss"
    "game.ss"
    "cribbage.ss"
    "tables.ss"
    "strategy.ss"))

(define verbose-cribbage
  (make-parameter #f))

(for-each load source-files)
(for-each (lambda (agent)
            (load (format "cribbers/~a.ss" agent)))
          agents)

(define (v:fxsum V)
  (do ((i (fx1- (vector-length V)) (fx1- i))
       (sum 0 (fx+ sum (vector-ref V i))))
      ((fx< i 0) sum)))

(define (state->dealer? state)
  (cond ((state-discard? state)
         (if (state-discard-dealer? state)
             'dealer
             'pone))
        ((state-peg? state)
         (if (state-peg-dealer? state)
             'dealer
             'pone))))

(define (display-discard state)
  (format #t
          "status: ~a~%scores: ~a ~a~%"
          (state->dealer? state)
          (state-discard-scoreA state)
          (state-discard-scoreB state))
  (display-hand (state-discard-hand state)) (newline))

(define (display-peg state)
  (format #t
          "status: ~a~%scores: ~a ~a~%"
          (state->dealer? state)
          (state-peg-scoreA state)
          (state-peg-scoreB state))
  (display "cut:   ")
  (display-card (state-peg-cut state)) (newline)
  (display "hand:  ")
  (display-hand (state-peg-hand state)) (newline)
  (display "board: ")
  (display-hand* (state-peg-board state)) (newline) (newline)
  (display "valid-pegs: ")
  (display-hand* (valid-pegs (state-peg-board state) (state-peg-hand state))) (newline))

(define (display-hand-report-score hand score)
  (display-hand hand)
  (format #t " got ~a~%" score))

(define C0
  (deal-crib 0 0 'A))
