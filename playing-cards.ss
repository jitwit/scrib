
(define (suit card)
  (fx/ card 13))

(define (rank card)
  (fxmod card 13))

(define (ten? card)
  (fx= 9 (rank card)))

(define (jack? card)
  (fx= 10 (rank card)))

(define (queen? card)
  (fx= 11 (rank card)))

(define (king? card)
  (fx= 12 (rank card)))

(define (ace? card)
  (fx= 0 (rank card)))

(define (nickel? card)
  (fx= 4 (rank card)))

(define (dime? card)
  (fx<= 9 (rank card)))

(define (club? card)
  (fxzero? (suit card)))

(define (diamond? card)
  (fx= 1 (suit card)))

(define (heart? card)
  (fx= 2 (suit card)))

(define (spade? card)
  (fx= 3 (suit card)))

(define (card->description card)
  (let ((suit (case (suit card)
                ((0) 'clubs)
                ((1) 'diamonds)
                ((2) 'hearts)
                ((3) 'spades)))
        (rank (case (rank card)
                ((0) 'ace)
                ((10) 'jack)
                ((11) 'queen)
                ((12) 'king)
                (else (fx1+ (rank card))))))
    (list rank suit)))

(define (description->card desc)
  (let ((suit (case (cadr desc)
                ((clubs) 0)
                ((diamonds) 1)
                ((hearts) 2)
                ((spades) 3)))
        (rank (case (car desc)
                ((ace) 0)
                ((jack) 10)
                ((queen) 11)
                ((king) 12)
                (else (fx1- (car desc))))))
    (fx+ (fx* 13 suit)
         rank)))

(define suit-colors
  ;; looks ok on emacs geiser, which doesn't display colors correctly
  '((clubs . light-green)
    (diamonds . light-red)
    (hearts . magenta)
    (spades . cyan)))

(define (display-card card)
  (let ((desc (card->description card)))
    (display-with-foreground
     (cdr (assoc (cadr desc) suit-colors))
     (cond ((symbol? (car desc))
            (char-upcase
             (string-ref (symbol->string (car desc))
                         0)))
           ((ten? card) 'T)
           (else (car desc))))))

(define (display-suit card)
  (let ((desc (card->description card)))
    (display-with-foreground
     (cdr (assoc (cadr desc) suit-colors))
     (case (cadr desc)
       ((clubs) (unicode->symbol "2663"))
       ((diamonds) (unicode->symbol "2666"))
       ((hearts) (unicode->symbol "2665"))
       ((spades) (unicode->symbol "2660"))))))

(define (display-hand hand)
  (unless (null? hand)
    (let ((hand (sort-on hand rank)))
      (display-card (car hand))
      (for-all (lambda (card)
                 (display #\-)
                 (display-card card))
               (cdr hand)))))

;; unsorted version
(define (display-hand* hand)
  (unless (null? hand)
    (display-card (car hand))
    (for-all (lambda (card)
               (display #\-)
               (display-card card))
             (cdr hand))))

(define (deck)
  (shuffle (iota 52)))

(define (discard cards hand)
  (filter (lambda (card)
            (not (memq card cards)))
          hand))

(define (deck-without hand)
  (discard hand (iota 52)))

(define (random-deal known-cards size)
  (list-head (shuffle (deck-without known-cards))
             size))

;;; box is a list of things to display row by row.
;;; each box

(define (huge-card card)
  `("       "
    " +---+ "
    ,(with-output-to-string
      (lambda ()
        (display " |")
        (display-card card)
        (display "  | ")))
    ,(with-output-to-string
      (lambda ()
        (display " | ")
        (display-suit card)
        (display " | ")))
    ,(with-output-to-string
      (lambda ()
        (display " |  ")
        (display-card card)
        (display "| ")))
    " +---+ "
    "       "))

(define (display-huge-hand hand)
  (if (null? hand)
      (newline)
      (for-all (lambda (fragments)
                 (for-all display fragments)
                 (newline))
               (apply map list (map huge-card hand)))))
