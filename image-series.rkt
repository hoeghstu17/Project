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
                      (blend-chooser n width height)
                      (blend-chooser n width height)
                      (blend-chooser n width height)))
                   width
                   height)])
      image)))

(define blend-chooser
  (lambda (n width height)
    (let ([max-value (- 256 (* n (quotient 256 9)))]) 
      (if (even? n)
          (lambda (col row) (* col (/ max-value (- width 1))))
          (lambda (col row) (* row (/ max-value (- height 1))))))))

    (let ([row-or-col 0]
          [max-value (- 256 (* n (quotient 256 9)))]))
      
    (when 
    (image-compute
     (lambda (col row)
       

(define image-series1
  (lambda (n width height)
    (let* ([image (image-compute
                   (lambda (col row)
                     (irgb
                      (* col (/ (* 28 (remainder n 10)) (- width 1)))
                      (* row (/ (* 28 (car (cdr (int->list n)))) (- width 1)))
                      (* row (/ (* 28 (car (int->list n))) (- width 1)))))
                   width
                   height)])
        image)))
        
>>>>>>> 3d5fccae283a8afe04b7d77feae14f3a3f7c6cf4
