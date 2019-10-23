(define re:digit
  (re:charset "0123456789"))

(define re:letter
  (re:charset "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(define (re:cribbot cribbot)
  (re:+ (re:. (re:string (symbol->string (cribbot-identity cribbot)))
              #\-
              (re:* re:letter)
              #\-
              (re:* re:digit)
              #\-
              (re:* re:digit))
        (re:. (re:* re:letter)
              #\-
              (re:string (symbol->string (cribbot-identity cribbot)))
              #\-
              (re:* re:digit)
              #\-
              (re:* re:digit))))

(define (find-games cribbot)
  (let ((matcher (make-memoized-matcher))
        (re:cribbot (re:cribbot cribbot)))
    (filter (lambda (file)
              (re:empty? (matcher file re:cribbot)))
            (directory-list "games"))))

(define (cribbot-symbol cribbot game)
  (let ((id (symbol->string (cribbot-identity cribbot))))
    (if (string=? id (substring game 0 (string-length id)))
        'A
        'B)))

(define (delete-unfinished-games)
  (for-all delete-file
           (filter (lambda (game)
                     (let* ((log (with-input-from-file game
                                   read-input))
                            (result (car (last-pair log))))
                       (not (or (= 121 (lookup 'scoreA result))
                                (= 121 (lookup 'scoreB result))))))
                   (saved-games-list))))

(define (cribbot-result cribbot game)
  (let ((id (cribbot-symbol cribbot game))
        (result (car (last-pair (with-input-from-file (string-append "games/" game)
                                  read-input)))))
    (let ((a (lookup 'scoreA result))
          (b (lookup 'scoreB result)))
      (if (eq? id 'A)
          (cons a b)
          (cons b a)))))

(define (cribbot-results cribbot)
  (let ((games (find-games cribbot)))
    (map (lambda (game)
           (cribbot-result cribbot game))
         games)))

(define (win-rate cribbot)
  (let ((results (cribbot-results cribbot)))
    (/ (count (lambda (result)
                (= 121 (car result)))
              results)
       (length results))))

(define (cribbot-scoring cribbot)
  (let ((results (cribbot-results cribbot)))
    (/ (fold-left + 0 (map (lambda (result)
                             (- (car result) (cdr result)))
                           results))
       (length results))))

(define (display-win-rate cribbot)
  (format #t "~,2f%~%" (* 100 (win-rate cribbot))))
