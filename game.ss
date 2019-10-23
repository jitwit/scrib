
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
         (update-score (update-score (update-score crib 'B scoreB) 'A scoreA) 'A scoreC))
        (else
         (update-score
          (update-score
           (update-score crib 'A scoreA) 'B scoreB) 'B scoreC)))))))

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

(define (run-peg state action)
  (if (eq? action 'go)
      (execute-peg-go state)
      (execute-peg-discard state action)))

(define (run-cribbage state action)
  (case (game-phase state)
    ((discard) (run-discard state action))
    ((peg) (run-peg state action))
    ((count) (execute-count state))
    ((won) state)))

(define (run-agents state A B)
  (let* ((now (current-time))
         (end #f)
         (out (open-output-file (format "games/~a-~a-~a-~a"
                                        (cribbot-identity A)
                                        (cribbot-identity B)
                                        (time-second now)
                                        (time-nanosecond now))))
         (agent-A (cribbot-strategy A))
         (agent-B (cribbot-strategy B)))
    (let loop ((state state))
      (write (crib->sexp state) out) (newline out)
      (let ((turn (crib-turn state)))
        (case (game-phase state)
          ((discard)
           (if (eq? turn 'A)
               (loop (run-cribbage state (agent-A (crib->discard state))))
               (loop (run-cribbage state (agent-B (crib->discard state))))))
          ((peg)
           (if (eq? turn 'A)
               (loop (run-cribbage state (agent-A (crib->peg state))))
               (loop (run-cribbage state (agent-B (crib->peg state))))))
          ((count) (loop (execute-count state)))
          ((won) (set! end state)))))
    (close-output-port out)
    end))

;;; The core
(define (make-cribbage agentA agentB)
  (letrec ((state (deal-crib 0 0 (random-dealer)))
           (run (lambda ()
                  (case (game-phase state)
                    ((discard)
                     (let ((move ((if (eq? (crib-turn state) 'A) agentA agentB)
                                  (crib->discard state))))
                       (set! state (run-discard state move))
                       'done))
                    ((peg)
                     (let ((move ((if (eq? (crib-turn state) 'A) agentA agentB)
                                  (crib->peg state))))
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

(define (run-cribbage-game agent)
  (parameterize ((verbose-cribbage #t))
    (let ((game (make-cribbage (cribbot-strategy agent) crib-terminus)))
      (let loop ((result (begin
                           (display-cribbage-state 'B (game 'god-mode))
                           (game 'run))))

        (display-cribbage-state 'B (game 'god-mode))
        (case result
          ((done) (loop (game 'run))) ;; note: done indicates game stepped without error 
          (else result))))))

(define (compare-agents A B trials)
  (let ((wins-a 0)
        (wins-b 0))
    (do ((trials trials (1- trials)))
        ((zero? trials) (cons wins-a wins-b))
      (format #t "~a - ~a~%" wins-a wins-b)
      (let ((result (time (run-agents (deal-crib 0 0 (random-dealer))
                                      A
                                      B))))
        (if (eq? (crib-scoreA result) 121)
            (inc! wins-a)
            (inc! wins-b))))))

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

