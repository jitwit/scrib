(import (euler)
        (charset)
        (brzozowski))

(print-gensym #f)

(define agents
  '(rando
    maximus
    minimus
    terminus
    monte
    carlo))

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

(define (random-element X)
  (list-ref X (random (length X))))

(define C0
  (deal-crib 0 0 'A))

(define P0
  (let ((C1 (run-cribbage C0 (random-element (cribbage-actions C0)))))
    (run-cribbage C1 (random-element (cribbage-actions C1)))))

(define (read-input)
  (let loop ((next (read)) (objects '()))
    (if (eof-object? next)
        (reverse objects)
        (loop (read) (cons next objects)))))

(define (read-game-log file)
  (with-input-from-file file read-input))

(define (random-game)
  (read-game-log
   (string-append "games/" (random-element (directory-list "games")))))

(define (display-cribbage-state player state)
  (let ((status (case (crib-dealer state)
                  ((A) 'pone)
                  ((B) 'deal))))
    (for-all (lambda (n) (newline)) (iota 30))
    (format #t "~a~%~a ~a~%" status (crib-scoreB state) (crib-scoreA state))
    (case (game-phase state)
      ((discard)
       (display-huge-hand (crib-handB state)))
      ((peg)
       (display-huge-hand (list (crib-cut state)))
       (display-huge-hand (crib-board state))
       (display-huge-hand (crib-handB state)))
      ((count)
       (cond ((eq? player (crib-dealer state))
              (for-all display-huge-hand
                       (list (crib-handA state)
                             (crib-handB state)
                             (crib-crib state))))
             (else
              (for-all display-huge-hand
                       (list (crib-handB state)
                             (crib-handA state)
                             (crib-crib state)))))))))
