
;;;; Strategy

;;; Heuristics
(define (deal-maximize-points-heuristic deck hand)
  (lambda (h)
    (+ (/ (v:fxsum (vector-map (lambda (c)
                                 (score-deal (cons c h)))
                               deck))
          (vector-length deck))
       (deal-discard (discard h hand)))))

(define (pone-maximize-points-heuristic deck hand)
  (lambda (h)
    (- (/ (v:fxsum (vector-map (lambda (c)
                                 (score-pone (cons c h)))
                               deck))
          (vector-length deck))
       (pone-discard (discard h hand)))))

(define (deal-maximize-crib-heuristic deck hand)
  (lambda (h)
    (deal-discard (discard h hand))))

(define (pone-minimize-crib-heuristic deck hand)
  (lambda (h)
    (- (pone-discard (discard h hand)))))

;;; Strategies

(define (deal-maximize-points hand)
  (let ((deck (list->vector (deck-without hand))))
    (discard-with-heuristic hand (deal-maximize-points-heuristic deck hand))))

(define (pone-maximize-points hand)
  (let ((deck (list->vector (deck-without hand))))
    (discard-with-heuristic hand (pone-maximize-points-heuristic deck hand))))

(define (deal-maximize-discard hand)
  (let ((deck (list->vector (deck-without hand))))
    (discard-with-heuristic hand (deal-maximize-crib-heuristic deck hand))))

(define (pone-minimize-discard hand)
  (let ((deck (list->vector (deck-without hand))))
    (discard-with-heuristic hand (pone-minimize-crib-heuristic deck hand))))

(define (discard-with-heuristic hand heuristic)
  (let ((results (maximums-on (combinations hand 4) heuristic)))
    (car results)))

(define (random-discard hand)
  (let ((discards (combinations hand 4)))
    (list-ref discards (random (length discards)))))

;;; Pegging

(define (peg-random state)
  (let ((moves (valid-pegs (state-peg-board state) (state-peg-hand state))))
    (if (null? moves)
        'go
        (list-ref moves (random (length moves))))))

(define (peg-best-single state)
  (let ((hand (state-peg-hand state))
        (board (state-peg-board state)))
    (let ((choices (maximums-on (valid-pegs board hand)
                                (lambda (peg)
                                  (score-peg (cons peg board))))))
      (if (null? choices)
          'go
          (list-ref choices (random (length choices)))))))

(define (peg-worst-single state)
  (let ((hand (state-peg-hand state))
        (board (state-peg-board state)))
    (let ((choices (maximums-on (valid-pegs board hand)
                                (lambda (peg)
                                  (- (score-peg (cons peg board)))))))
      (if (null? choices)
          'go
          (list-ref choices (random (length choices)))))))

(define (display-discard-strategy heuristic hand n)
  (let ((deck (list->vector (deck-without hand))))
    (let ((results (reverse (rank-on (combinations hand 4) (heuristic deck hand)))))
      (display-ln heuristic)
      (for-each (lambda (result)
                  (let ((keeps (cdr result))
                        (cribs (discard (cdr result) hand)))
                    (display-hand keeps) (newline)
                    (display-hand cribs) (newline)
                    (format #t "H: ~,2f~%~%"
                            (car result))))
                (list-head results n)))))

;;; Agents

(define (simple-agent deal-strategy pone-strategy)
  (cons deal-strategy pone-strategy))

(define (simple-agent-deal agent)
  (car agent))

(define (simple-agent-pone agent)
  (cdr agent))

(define simple-agent-maximize
  (simple-agent deal-maximize-points pone-maximize-points))

(define simple-agent-random
  (simple-agent random-discard random-discard))

(define simple-agent-defensive
  (simple-agent deal-maximize-points pone-minimize-discard))

(define simple-agent-crib-minded
  (simple-agent deal-maximize-discard pone-minimize-discard))

;;; Simulations

(define (simulate-agent-hand player-a player-b)
  (let ((cards (list-head (deck) 13)))
    (let ((cut (car cards))
          (deal-a (list-head (cdr cards) 6))
          (deal-b (list-tail (cdr cards) 6)))
      (let ((hand-a ((simple-agent-deal player-a) deal-a))
            (hand-b ((simple-agent-pone player-b) deal-b)))
        (let ((crib (append (discard hand-a deal-a)
                            (discard hand-b deal-b))))
          (cons (+ (score-deal (cons cut hand-a))
                   (score-crib (cons cut crib)))
                (score-pone (cons cut hand-b))))))))

(define (simulate-agent-matchup player-a player-b trials)
  (let ((deals-a (make-vector trials))
        (deals-b (make-vector trials))
        (pones-a (make-vector trials))
        (pones-b (make-vector trials)))
    (do ((i 0 (1+ i)))
        ((= i trials)
         (report-agent-results deals-a pones-a deals-b pones-b))
      (let ((r1 (simulate-agent-hand player-a player-b))
            (r2 (simulate-agent-hand player-b player-a)))
        (vector-set! deals-a i (car r1))
        (vector-set! pones-b i (cdr r1))
        (vector-set! deals-b i (car r2))
        (vector-set! pones-a i (cdr r2))))))

(define (report-agent-results deals-a pones-a deals-b pones-b)
  (let ((stats-deal-a (mean.stdev deals-a))
        (stats-pone-a (mean.stdev pones-a))
        (stats-deal-b (mean.stdev deals-b))
        (stats-pone-b (mean.stdev pones-b)))
    (display-ln "Agent A")
    (format #t "mean deal: ~,2f~%mean pone: ~,2f~%~%"
            (car stats-deal-a)
            (cdr stats-pone-a))
    (display-ln "Agent B")
    (format #t "mean deal: ~,2f~%mean pone: ~,2f~%~%"
            (car stats-deal-b)
            (cdr stats-pone-b))))

(define (simulate-scores-for-strategies deal-strategy pone-strategy trials)
  (let ((dealer-results (make-vector trials))
        (crib-results (make-vector trials))
        (pone-results (make-vector trials)))
    (do ((j 0 (fx1+ j)))
        ((fx= j trials)
         (report-simulation-results dealer-results crib-results pone-results))
      (let ((cards (list-head (deck) 13)))
        (let ((cut (car cards))
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
                           (score-pone (cons cut hand-b))))))))))

(define (report-simulation-results deals cribs pones)
  (let ((stats-deal (mean.stdev deals))
        (stats-crib (mean.stdev cribs))
        (stats-pone (mean.stdev pones)))
    (display-ln "Dealer Hands")
    (format #t "mean:  ~,2f~%stdev: ~,2f~%~%"
            (car stats-deal)
            (cdr stats-deal))
    (display-ln "Pone Hands")
    (format #t "mean:  ~,2f~%stdev: ~,2f~%~%"
            (car stats-pone)
            (cdr stats-pone))
    (display-ln "Cribs")
    (format #t "mean:  ~,2f~%stdev: ~,2f~%~%"
            (car stats-crib)
            (cdr stats-crib))))





