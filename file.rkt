#lang racket

(define all (set 1 2 3 4 5 6 7 8 9))

(define (tile-solved? t) (= 1 (set-count t)))

(define (transform matrix) 
  (map 
   (lambda (lst) 
     (map
      (lambda (num) (
         if (equal? num 0) all (set num))
        )
      lst))
   matrix))

(define (transformflat matrix) 
  (map 
      (lambda (num) (
         if (equal? num 0) all (set num))
        )
   (flatten matrix)))

(struct tile (row col box values) #:transparent)

(define (get-row pos) (quotient pos 9))
(define (get-col pos) (- pos (* 9 (get-row pos))))
(define (get-box pos) (+ (quotient (get-col pos) 3) (* 3 (quotient (get-row pos) 3))))

(define (make-tile value pos) (if (equal? value 0) (tile 1 2 3 all) (tile 2 4 5 (set value))))
                               

(define (transform-tile matrix) 
  (map make-tile (flatten matrix)))


(provide all tile-solved? transform transformflat tile get-row get-col get-box)