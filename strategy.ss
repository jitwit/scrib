
;;;; Strategy

(define (discard-analyze-hand-strategy heuristic hand)
  (let ((results (reverse (rank-on (combinations hand 4) heuristic))))
    (for-each (lambda (result)
                (pretty-print-hand (cdr result))
                (pretty-print-hand (discard (cdr result) hand))
                (format #t "H: ~,2f~%~%" (car result)))
              (list-head results 5))
    (map cdr results)))

(define (deal-discard-maximize-hand hand)
  (let ((deck (list->vector (deck-without hand))))
    (discard-analyze-hand-strategy (lambda (hand)
                                     (car (mean.stdev
                                           (vector-map (lambda (c)
                                                         (score-deal (cons c hand)))
                                                       deck))))
                                   hand)))

(define (pone-discard-maximize-hand hand)
  (let ((deck (list->vector (deck-without hand))))
    (discard-analyze-hand-strategy (lambda (hand)
                                     (car (mean.stdev
                                           (vector-map (lambda (c)
                                                         (score-pone (cons c hand)))
                                                       deck))))
                                   hand)))

(define (analyze-crib-discard hand crib deck)
  'todo)

