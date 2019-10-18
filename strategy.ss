
;;;; Strategy

(define (discard-analyze-hand-strategy heuristic hand)
  (let ((results (reverse (rank-on (combinations hand 4) heuristic))))
    (for-each (lambda (result)
                (pretty-print-hand (cdr result))
                (pretty-print-hand (discard (cdr result) hand))
                (format #t "H: ~,2f~%~%" (car result)))
              (list-head results 5))
    (map cdr results)))

(define (discard-with-heuristic hand heuristic)
  (let ((results (reverse (rank-on (combinations hand 4) heuristic))))
    (cdar results)))

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
                                                            (score-pone (cons c h)))
                                                          deck)))
                                        (pone-discard (discard h hand))))
                                   hand)))

(define (deal-strategy-maximize-points hand)
  (let ((deck (list->vector (deck-without hand))))
    (discard-with-heuristic hand
                            (lambda (h)
                              (+ (car (mean.stdev
                                       (vector-map (lambda (c)
                                                     (score-deal (cons c h)))
                                                   deck)))
                                 (deal-discard (discard h hand)))))))

(define (pone-strategy-maximize-points hand)
  (let ((deck (list->vector (deck-without hand))))
    (discard-with-heuristic hand
                            (lambda (h)
                              (- (car (mean.stdev
                                       (vector-map (lambda (c)
                                                     (score-pone (cons c h)))
                                                   deck)))
                                 (pone-discard (discard h hand)))))))

(define (simulate-average-scores-for-strategies deal-strategy pone-strategy trials)
  (let ((dealer-results (make-vector trials))
        (crib-results (make-vector trials))
        (pone-results (make-vector trials)))
    (do ((j 0 (fx1+ j)))
        ((fx= j trials)
         (report-simulation-results dealer-results crib-results pone-results))
      (let* ((cards (list-head (deck) 13))
             (cut (car cards))
             (deal-a (list-head (cdr cards) 6))
             (deal-b (list-tail (cdr cards) 6)))
        (let ((hand-a (deal-strategy deal-a))
              (hand-b (pone-strategy deal-b)))
          (let ((crib (append (discard hand-a deal-a)
                              (discard hand-b deal-b))))
            (vector-set! dealer-results
                         j
                         (score-deal (cons cut hand-a)))
            (vector-set! crib-results
                         j
                         (score-crib (cons cut crib)))
            (vector-set! pone-results
                         j
                         (score-pone (cons cut hand-b)))))))))

(define (report-simulation-results deals cribs pones)
  (let ((stats-deal (mean.stdev deals))
        (stats-crib (mean.stdev cribs))
        (stats-pone (mean.stdev pones)))
    (display-ln "Dealer Hands")
    (format #t "mean:  ~,2f~%stdev: ~,2f~%hist: ~a~%~%"
            (car stats-deal)
            (cdr stats-deal)
            (sort-on (eq-histogram (vector->list deals))
                     (compose - cdr)))
    (display-ln "Pone Hands")
    (format #t "mean:  ~,2f~%stdev: ~,2f~%hist: ~a~%~%"
            (car stats-pone)
            (cdr stats-pone)
            (sort-on (eq-histogram (vector->list pones))
                     (compose - cdr)))
    (display-ln "Cribs")
    (format #t "mean:  ~,2f~%stdev: ~,2f~%hist: ~a~%~%"
            (car stats-crib)
            (cdr stats-crib)
            (sort-on (eq-histogram (vector->list cribs))
                     (compose - cdr)))))
