#lang racket
(require gigls/unsafe)

(define int->list
  (lambda(n)
    (let kernel
      ([n-change n]
       [ones-count 0]
       [tens-count 0]
       [hundreds-count 0]
       [lst null])
      (if (zero? (modulo n 10))
          (cons 0 lst)
          (cond
            [(zero? (modulo n-change 10))
             (cons (- 10 ones-count) lst)]
            [else
             (kernel (+ n-change 1) (+ ones-count 1) tens-count hundreds-count lst)]))
      (if (zero? (modulo n 100))
          (cons 0 lst)
          (cond
            [(zero? (modulo n-change 100))
             (cons (- 100 tens-count) lst)]
            [else
             (kernel (+ n-change 10) ones-count (+ tens-count 1) hundreds-count lst)]))
      lst)))

(define int->list2
  (lambda (n)
    (let ([lst null])
    (letrec
        ([ones
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
      (car lst)
      ;(tens (- n (car lst)) 0)
      ))))

              

  
  
  
  
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