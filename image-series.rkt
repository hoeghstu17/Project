#lang racket
(require gigls/unsafe)

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