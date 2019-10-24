(import (euler)
        (charset)
        (brzozowski)
        (chez vector)
        (sxml-mini))

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
    "study.ss"
    "strategy.ss"
    "tables.ss"
    "visuals.ss"
    
    ))

(define table-cutoff
  (make-parameter 100000))

(define verbose-cribbage
  (make-parameter #f))

(for-each load source-files)
(for-each (lambda (agent)
            (load (format "cribbers/~a.ss" agent)))
          agents)

(define (v:sum V)
  (do ((i (fx1- (vector-length V)) (fx1- i))
       (sum 0 (+ sum (vector-ref V i))))
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

(define (lookup key associations)
  (cond ((assoc key associations)
         => cdr)
        (else (error 'lookup "no key" key associations))))

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

(define (save-thing-to-file thing file)
  (parameterize ((print-gensym #t))
    (delete-file file)
    (with-output-to-file file
      (lambda ()
        (display thing)))))

(random-seed
 (time-nanosecond
  (current-time)))


