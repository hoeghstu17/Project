#lang racket
(require gigls/unsafe)

(define int->list2
  (lambda (n)
    (letrec
        ([lst null]
         [ones
          (lambda (n-ones count-ones)
            (cond
              [(zero? (modulo n 10))
               (cons 0 lst)]
              [(zero? (modulo n-ones 10))
               (cons (- 10 count-ones) lst)]
              [else
               (ones (+ n-ones 1) (+ count-ones 1))]))]
         [tens
          (lambda (n-tens count-tens)
            (cond
              [(zero? (modulo n 100))
               (cons 0 lst)]
              [(zero? (modulo n-tens 100))
               (cons (/ (- 100 (* 10 count-tens)) 10) lst)]
              [else
               (tens (+ n-tens 10) (+ count-tens 1))]))])
      
      (ones n 0)
      )))

(define int->list3
  (lambda (n)
    (map (compose (section string->number <>) (section char->string <>)) (string->list (number->string n)))))







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