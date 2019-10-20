
;;; Turns

(define (opposite-player x)
  (if (eq? x 'A) 'B 'A))

(define (random-dealer)
  (if (zero? (random 2)) 'A 'B))

;;; Run moves
(define (change-turn crib)
  (crib-update-turn crib opposite-player))

(define (move-board crib)
  (crib-update-turn
   (crib-update-last-peg
    (crib-update-board
     (crib-update-board*
      crib
      (curry cons* (crib-board crib)))
     (const '()))
    (const #f))
   (const (opposite-player (crib-last-peg crib)))))

;; todo: settle game winner by raising error if passes 121. could use
;; with-exception-handler in make-cribbage or something
(define (update-score crib who dx)
  (case who
    ((A) (let ((score (+ dx (crib-scoreA crib))))
           (when (<= 121 score)
             (raise 'A))
           (crib-update-scoreA crib (const score))))
    ((B) (let ((score (+ dx (crib-scoreB crib))))
           (when (<= 121 score)
             (raise 'B))
           (crib-update-scoreB crib (const score))))))

(define (execute-discard crib card+)
  (case (crib-turn crib)
    ((A) (crib-update-handA crib (curry discard card+)))
    ((B) (crib-update-handB crib (curry discard card+)))))

(define (execute-discard-crib crib cards)
  (change-turn
   (crib-update-crib (execute-discard crib cards) (curry append cards))))

(define (execute-peg-go crib)
  (if (no-pegs-left? crib)
      (move-board (update-score crib (crib-last-peg crib) 1))
      (change-turn crib)))

(define (execute-peg-discard crib card)
  (let* ((crib* (crib-update-last-peg
                 (crib-update-board
                  (execute-discard crib (list card))
                  (curry cons* card))
                 (const (crib-turn crib))))
         (board* (crib-board crib*))
         (crib** (update-score crib* (crib-turn crib) (score-peg board*))))
    (if (= 31 (crib-board-total board*))
        (move-board crib**)
        (change-turn crib**))))

(define (execute-peg crib card/go)
  (if (eq? card/go 'go)
      (execute-peg-go crib)
      (execute-peg-discard crib card/go)))

(define (execute-count crib handA handB)
  (let ((cut (crib-cut crib))
        (crib (crib-update-handB
               (crib-update-handA
                crib
                (const handA))
               (const handB))))
    (redeal-crib
     (cond ((eq? 'A (crib-dealer crib))
            (update-score
             (update-score
              (update-score
               crib
               'B
               (score-hand (cons cut handB)))
              'A
              (score-hand (cons cut handA)))
             'A
             (score-crib (cons cut (crib-crib crib)))))
           (else
            (update-score
             (update-score
              (update-score
               crib
               'A
               (score-hand (cons cut handA)))
              'B
              (score-hand (cons cut handB)))
             'B
             (score-crib (cons cut (crib-crib crib)))))))))

;;; Game Phase predicates
(define (discard-complete? crib)
  (= 4 (length (crib-crib crib))))

(define (peg-complete? crib)
  (and (null? (crib-handA crib))
       (null? (crib-handB crib))))

(define (no-pegs-left? crib)
  (let ((board (crib-board crib)))
    (and (null? (valid-pegs board (crib-handA crib)))
         (null? (valid-pegs board (crib-handB crib))))))

(define (game-phase crib)
  (cond ((peg-complete? crib) 'count)
        ((discard-complete? crib) 'peg)
        (else 'discard)))

;;; Move checks
(define (valid-discard? state turn move)
  (and (= 2 (length move))
       (eq? turn (crib-turn state))
       (andmap (lambda (card)
                 (memq card
                       (if (eq? turn 'A)
                           (crib-handA state)
                           (crib-handB state))))
               move)))

(define (valid-go? state turn)
  (and (eq? turn (crib-turn state))
       (null? (valid-pegs (crib-board state)
                          (crib-cards state turn)))))

(define (valid-peg-discard? state turn card)
  (and (eq? turn (crib-turn state))
       (memq card (valid-pegs (crib-board state)
                              (crib-cards state turn)))))

(define (valid-peg? state turn move)
  (if (eq? move 'go)
      (valid-go? state turn)
      (valid-peg-discard? state turn move)))

;;; The core
(define (make-cribbage agentA agentB)
  (letrec ((state (deal-crib 0 0 (random-dealer)))
           (handA #f)
           (handB #f)
           (run (lambda ()
                  (case (game-phase state)
                    ((discard)
                     (let* ((turn (crib-turn state))
                            (move ((if (eq? turn 'A) agentA agentB)
                                   (crib->discard state))))
                       (unless (valid-discard? state turn move)
                         (error 'cribbage "bad discard" state move))
                       (set! state (execute-discard-crib state move))
                       (when (discard-complete? state)
                         ;; remember hands before discarding
                         (set! handA (crib-handA state))
                         (set! handB (crib-handB state))
                         (when (jack? (crib-cut state))
                           ;; his heels!
                           (set! state (update-score state (crib-dealer state) 2))))
                       'done))
                    ((peg)
                     (let* ((turn (crib-turn state))
                            (move ((if (eq? turn 'A) agentA agentB)
                                   (crib->peg state))))
                       (unless (valid-peg? state turn move)
                         (error 'cribbage "bad peg" state move))
                       (set! state (execute-peg state move))
                       'done))
                    ((count)
                     (set! state (execute-count state handA handB))
                     'done)
                    (else (error 'cribbage "bad phase" state))))))
    (lambda (M)
      (case M
        ((god-mode) state)
        ((run) (run))
        (else (error 'cribbage "dunno, man" M))))))

;;; Validate ?

;;; Mechanics
(define (deal-crib scoreA scoreB dealer)
  (let ((cards (list-head (deck) 13)))
    (make-crib dealer
               (opposite-player dealer)
               scoreA
               scoreB
               (sort-on (list-head (cdr cards) 6) rank)
               (sort-on (list-tail (cdr cards) 6) rank)
               '()
               (car cards)
               #f
               '()
               '())))

(define (redeal-crib crib)
  (let ((cards (list-head (deck) 13)))
    (deal-crib (crib-scoreA crib)
               (crib-scoreB crib)
               (opposite-player (crib-dealer crib)))))

(define (crib-cards crib who)
  (if (eq? who 'A)
      (crib-handA crib)
      (crib-handB crib)))

(define (game-over? crib)
  (<= 121 (max (crib-scoreA crib)
               (crib-scoreB crib))))

