#lang racket
(require gigls/unsafe)


; http://www.cs.grinnell.edu/~rebelsky/Courses/CSC151/2016S/eboards/eboard.46.html
(define int->list
  (lambda (n)
    (list (quotient n 100) (quotient (remainder n 100) 10) (remainder n 10))))

(define fancy-int-list
  (lambda (lst)
    0))




(define date-tracker
  (lambda (n)
    (let ([year 0]
          [month 0]
          [day 0])
      0)))


(define image-series
  (lambda (n width height)
    (let* ([image (image-compute
                   (lambda (col row)
                     (irgb
                      (* row (/ 256 (- height 1)))
                      0
                      (* col (/ 256 (- width 1)))))
                   width
                   height)])
      image)))

(define blend-chooser
  (lambda (n width height)
    (let ([row-or-col 0]
          [max-value (- 256 (* n (quotient 256 9)))]))
      
    (when 
    (image-compute
     (lambda (col row)
       


