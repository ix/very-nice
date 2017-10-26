(load "types/main.scm")

(define (god-i-hate-myself)
  (display "haha yes")
  (newline))

(define (foldr func end list)
  (if (null? list)
      end
      (func (car list) (foldr func end (cdr list)))))
