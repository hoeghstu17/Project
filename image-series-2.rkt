#lang racket
(require gigls/unsafe)

(define image-series
  (lambda (n width height)
    (let* ([n-list (list
                    (quotient n 100)
                    (quotient (remainder n 100) 10)
                    (remainder n 10))]
           [n-list-sum (apply + n-list)]
           [n-list-a (car n-list)]
           [n-list-b (cadr n-list)]
           [n-list-c (caddr n-list)]
           [year (+ 1 (quotient n 365))] ;;; one year is 365 days long, starts at year 1
           [element (+ 1 (modulo (ceiling (/ n 73)) 5))] ;;; 73 days per season, 5 seasons
           [arcane (list "violet" "gold")]
           [fire (list "red" "orangered")]
           [water (list "royalblue" "skyblue")]
           [earth (list "sienna" "forestgreen")]
           [air (list "whitesmoke" "skyblue")]
           [image-blend
            (let ([max-value (lambda (val)
                               (cond ;;; Add more variation here, or change to if
                                 [(even? n-list-sum)
                                  (- 256 (* val (quotient 256 9)))]
                                 [else
                                  (- 256 (* val 28))]))])
              (image-compute
               (lambda (x y)
                 (irgb
                  (* (if (odd? n-list-a) x y)
                     (/ (max-value n-list-a)
                        (if (odd? n-list-a) (- width 1) (- height 1))))
                  (* (if (odd? n-list-b) x y)
                     (/ (max-value n-list-b)
                        (if (odd? n-list-b) (- width 1) (- height 1))))
                  (* (if (odd? n-list-c) x y)
                     (/ (max-value n-list-c)
                        (if (odd? n-list-c) (- width 1) (- height 1))))))
               width
               height))])
      image-blend)))