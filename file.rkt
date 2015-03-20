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

(define (make-tile value pos) 
  (if (equal? value 0) 
      (tile (get-row pos) 
            (get-col pos) 
            (get-box pos) all) 
      (tile (get-row pos) 
            (get-col pos) 
            (get-box pos) (set value))))

(define (map-with-index f lst) 
   (let iter ([index-list (range 0 (length lst))]
              [in-list lst]
              [out-list (list)])
     (if (null? in-list)
         (reverse out-list)
         (iter (cdr index-list)
               (cdr in-list)
               (cons (f (car in-list) (car index-list)) out-list)))))
  

(define (transform-tile matrix) 
  (map-with-index make-tile (flatten matrix)))

(struct snapshot (before tile after changed?) #:transparent)


(define (board-iter f board)
  (let iter ([tried '()]
             [to-try board])
    (if (null? to-try)
        tried
        (let ((return (f tried 
                         (car to-try) 
                         (cdr to-try))))
          (if (snapshot-changed? return)
              (iter '() 
                    (append (snapshot-before return) 
                            (cons (snapshot-tile return) 
                                  (snapshot-after return))))
              (iter (append (snapshot-before return) 
                            (list (snapshot-tile return)))
                    (snapshot-after return)))))))  


         


(define (test-board)
  (list
   (list 0 2 0 1 7 8 0 3 0)
   (list 0 4 0 3 0 2 0 9 0)
   (list 1 0 0 0 0 0 0 0 6)
   (list 0 0 8 6 0 3 5 0 0)
   (list 3 0 0 0 0 0 0 0 4)
   (list 0 0 6 7 0 9 2 0 0)
   (list 9 0 0 0 0 0 0 0 2)
   (list 0 8 0 9 0 1 0 6 0)
   (list 0 1 0 4 3 6 0 5 0)))


(provide test-board all tile-solved? transform transformflat tile get-row get-col get-box make-tile transform-tile snapshot board-iter)