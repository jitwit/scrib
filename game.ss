
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
     crib
     (const '()))
    (const #f))
   (const (opposite-player (crib-last-peg crib)))))

(define (update-score state who dx)
  (if (game-won? state)
      state
      (case who
        ((A) (let ((score (+ dx (crib-scoreA state))))
               (crib-update-scoreA state (const (min score 121)))))
        ((B) (let ((score (+ dx (crib-scoreB state))))
               (crib-update-scoreB state (const (min score 121))))))))

(define (execute-discard crib card+)
  (case (crib-turn crib)
    ((A) (crib-update-handA crib (curry discard card+)))
    ((B) (crib-update-handB crib (curry discard card+)))))

(define (execute-discard-crib state cards)
  (change-turn
   (crib-update-crib (execute-discard state cards) (curry append cards))))

(define (execute-peg-go state)
  (if (no-pegs-left? state)
      (move-board (update-score state (crib-last-peg state) 1))
      (change-turn state)))

(define (execute-peg-discard state card)
  (let ((turn (crib-turn state))
        (board (cons card (crib-board state))))
    (let ((state* (update-score
                   (crib-update-last-peg
                    (crib-update-board*
                     (crib-update-board
                      (execute-discard state (list card))
                      (curry cons* card))
                     (curry cons* (cons card turn)))
                    (const turn))
                   turn
                   (score-peg board))))
      (if (= 31 (crib-board-total board))
          (move-board state*)
          (change-turn state*)))))

(define (execute-peg crib card/go)
  (if (eq? card/go 'go)
      (execute-peg-go crib)
      (execute-peg-discard crib card/go)))

(define (execute-count crib)
  (let ((cut (crib-cut crib))
        (handA (filter-map (lambda (c.id)
                             (and (eq? (cdr c.id) 'A)
                                  (car c.id)))
                           (crib-board* crib)))
        (handB (filter-map (lambda (c.id)
                             (and (eq? (cdr c.id) 'B)
                                  (car c.id)))
                           (crib-board* crib))))
    (let ((scoreA (score-hand (cons cut handA)))
          (scoreB (score-hand (cons cut handB)))
          (scoreC (score-hand (cons cut (crib-crib crib))))
          (crib (crib-update-handB
                 (crib-update-handA
                  crib
                  (const handA))
                 (const handB))))
      (redeal-crib
       (cond
        ((eq? 'A (crib-dealer crib))
         (when (verbose-cribbage)
           (display "cut: ") (display-card cut) (newline)
           (display-hand-report-score handB scoreB)
           (display-hand-report-score handA scoreA)
           (display-hand-report-score (crib-crib crib) scoreC)
           (sleep (make-time 'time-duration 0 2)))
         (update-score (update-score (update-score crib 'B scoreB) 'A scoreA) 'A scoreC))
        (else
         (when (verbose-cribbage)
           (display "cut: ") (display-card cut) (newline)
           (display-hand-report-score handA scoreA)
           (display-hand-report-score handB scoreB)
           (display-hand-report-score (crib-crib crib) scoreC)
           (sleep (make-time 'time-duration 0 2)))
         (update-score (update-score (update-score crib 'A scoreA) 'B scoreB) 'B scoreC)))))))

;;; Game Phase predicates

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

(define (run-discard state action)
  (let ((state* (execute-discard-crib state action)))
    (if (and (jack? (crib-cut state*))
             (discard-complete? state*))
        (update-score state* (crib-dealer state) 2)
        state*)))

(define (run-cribbage state action)
  (case (game-phase state)
    ((discard) (run-discard state action))
    ((peg) (execute-peg state action))
    ((count) (execute-count state))
    ((won) state)))

;;; The core
(define (make-cribbage agentA agentB)
  (letrec ((state (deal-crib 0 0 (random-dealer)))
           (run (lambda ()
                  (case (game-phase state)
                    ((discard)
                     (let ((move ((if (eq? (crib-turn state) 'A) agentA agentB)
                                  (crib->discard state))))
                       (unless (valid-discard? state (crib-turn state) move)
                         (error 'cribbage "bad discard" state move))
                       (set! state (run-discard state move))
                       'done))
                    ((peg)
                     (let ((move ((if (eq? (crib-turn state) 'A) agentA agentB)
                                  (crib->peg state))))
                       (unless (valid-peg? state (crib-turn state) move)
                         (error 'cribbage "bad peg" state move))
                       (set! state (execute-peg state move))
                       'done))
                    ((count) (set! state (execute-count state)) 'done)
                    ((won) state)
                    (else (error 'cribbage "bad phase" state))))))
    (lambda (M)
      (case M
        ((run) (run))
        ((god-mode) state)
        (else (error 'cribbage "dunno, man" M))))))

(define (run-cribbage-game agentA agentB)
  ;;  (parameterize ((verbose-cribbage #f)))
  (let ((game (make-cribbage agentA agentB)))
    (let loop ((result (game 'run)))
      (case result
        ((done) (loop (game 'run))) ;; note: done indicates game stepped without error 
        (else result)))))

(define (compare-agents agentA agentB trials)
  (parameterize ((verbose-cribbage #f))
    (let ((wins-a 0)
          (wins-b 0))
      (do ((trials trials (1- trials)))
          ((zero? trials) (cons wins-a wins-b))
        (let ((result (run-cribbage-game agentA agentB)))
          (if (eq? (crib-scoreA result) 121)
              (inc! wins-a)
              (inc! wins-b)))))))

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



