(import (euler))

(print-gensym #f)

(define agents
  '(rando))

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

(define handA)

(define handB)

(define cut)

(define (deal)
  (let ((cards (list-head (deck) 13)))
    (set! handA (list-head cards 6))
    (set! handB (list-head (list-tail cards 6) 6))
    (set! cut (list-ref cards 12))))

(define (display-deal)
  (pretty-print-card cut) (newline)
  (pretty-print-hand handA)
  (pretty-print-hand handB))

(define (pretty-print-crib cut hand)
  (pretty-print-card cut)
  (display #\-)
  (pretty-print-hand hand)
  (display-ln (score-deal (cons cut hand))))

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
          "status: ~a~%scores: ~a ~a~%hand:   "
          (state->dealer? state)
          (state-peg-scoreA state)
          (state-peg-scoreB state))
  (display-hand (state-peg-hand state))
  (display "board: ")
  (display-hand (state-peg-board state)))

(define C0
  (deal-crib 0 0 'A))
