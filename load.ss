(import (euler))

(print-gensym #f)

(define agents
  '(rando
    terminal))

(define source-files
  '("term.ss"
    "playing-cards.ss"
    "records.ss"
    "game.ss"
    "cribbage.ss"
    "tables.ss"
    "strategy.ss"))

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
  (display-hand (state-discard-hand state)))

(define (display-peg state)
  (format #t
          "status: ~a~%scores: ~a ~a~%"
          (state->dealer? state)
          (state-peg-scoreA state)
          (state-peg-scoreB state))
  (display "cut:   ")
  (display-card (state-peg-cut state)) (newline)
  (display "hand:  ")
  (display-hand (state-peg-hand state))
  (display "board: ")
  (display-hand (state-peg-board state))
  (display "valid-pegs: ")
  (display-hand (valid-pegs (state-peg-board state) (state-peg-hand state))))

(define (display-cribbage state)
  (format #t
          "phase: ~a~%dealer: ~a~%turn:   ~a~%scores: ~a ~a~%"
          (game-phase state)
          (crib-dealer state)
          (crib-turn state)
          (crib-scoreA state)
          (crib-scoreB state))
  (display-ln "Hand A")
  (display-hand (crib-handA state))
  (display-ln "Hand B")
  (display-hand (crib-handB state))
  (display-ln "Crib")
  (display-hand (crib-crib state))
  (display-ln "Board")
  (display-hand (crib-board state))
  (newline))

(define C0
  (deal-crib 0 0 'A))
