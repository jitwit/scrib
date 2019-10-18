
;;;; Strategy

(define (discard-analyze-hand-strategy heuristic hand)
  (let ((results (reverse (rank-on (combinations hand 4) heuristic))))
    (for-each (lambda (result)
                (pretty-print-hand (cdr result))
                (pretty-print-hand (discard (cdr result) hand))
                (format #t "H: ~,2f~%~%" (car result)))
              (list-head results 5))
    (map cdr results)))

(define (deal-maximize-points hand)
  (let ((deck (list->vector (deck-without hand))))
    (discard-analyze-hand-strategy (lambda (h)
                                     ;; sum of expected value of cut + crib discard
                                     (+ (car (mean.stdev
                                              (vector-map (lambda (c)
                                                            (score-deal (cons c h)))
                                                          deck)))
                                        (deal-discard (discard h hand))))
                                   hand)))

(define (pone-maximize-points hand)
  (let ((deck (list->vector (deck-without hand))))
    (discard-analyze-hand-strategy (lambda (h)
                                     ;; sum of expected value of cut - crib discard
                                     (- (car (mean.stdev
                                              (vector-map (lambda (c)
                                                            (score-deal (cons c h)))
                                                          deck)))
                                        (pone-discard (discard h hand))))
                                   hand)))

