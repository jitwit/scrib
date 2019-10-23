(import (euler)
        (charset)
        (brzozowski))

(print-gensym #f)

(define agents
  '(rando
    maximus
    minimus
    maggie
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
    "strategy.ss"
    "visuals.ss"))

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

(define (saved-games-list)
  (map (lambda (game)
         (string-append "games/" game))
       (directory-list "games")))

(define (random-game)
  (read-game-log (random-element (saved-games-list))))

(random-seed
 (time-nanosecond
  (current-time)))
