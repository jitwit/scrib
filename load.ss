(import (euler))

(define source-files
  '("term.ss"
    "playing-cards.ss"
    "cribbage.ss"
    "strategy.ss"))

(for-each load source-files)

(define handA)

(define handB)

(define cut)

(define (deal)
  (let ((cards (list-head (deck) 13)))
    (set! handA (list-head cards 6))
    (set! handB (list-head (list-tail cards 6) 6))
    (set! cut (list-ref cards 12))))

(define (display-deal)
  (deal)
  (pretty-print-card cut) (newline)
  (pretty-print-hand handA)
  (pretty-print-hand handB))

(define (pretty-print-crib cut hand)
  (pretty-print-card cut)
  (display #\-)
  (pretty-print-hand hand)
  (display-ln (score-deal (cons cut hand))))
