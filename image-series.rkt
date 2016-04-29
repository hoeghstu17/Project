#lang racket
(require gigls/unsafe)


; http://www.cs.grinnell.edu/~rebelsky/Courses/CSC151/2016S/eboards/eboard.46.html
(define int->list
  (lambda (n)
     (list (quotient n 100) (quotient (remainder n 100) 10) (remainder n 10))))







(define image-series
  (lambda (n width height)
    (let* ([image (image-compute
                   (lambda (col row)
                     (irgb
                      (* col (/ 256 (- height 1)))
                      0
                      (* col (/ 256 (- width 1)))))
                   width
                   height)])
      0)))