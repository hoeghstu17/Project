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
           [planet (image-load "/home/joshua/git/project/planet.png")] ;;; file path variable                
           [year (+ 1 (quotient n 365))] ;;; one year is 365 days long, start at year 1
           [element-season (+ 1 (modulo (ceiling (/ n 73)) 5))] ;;; 73 days per season, 5 seasons
           [elements (list (list 1 'ARCANE "violet" "gold")
                           (list 2 'FIRE "red" "orangered")
                           (list 3 'WATER "royalblue" "skyblue")
                           (list 4 ' EARTH "sienna" "forestgreen")
                           (list 5 'AIR "whitesmoke" "skyblue"))]
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
               height))]
           [planet-color-changer
            (lambda (image primary secondary)
              (if (equal? "white" image)
                  secondary
                  primary))]           
           [planet-elementifier ;; procedure to check season/element and change colour of planet
            (lambda (image)
              (let kernel [lst elements]
                (let ([element (car (car lst))])
                  (if (equal? element-season element)
                      (image-variant (planet-color-changer
                                      image
                                      (caddr (car lst))
                                      (cadddr (car lst))))                     
                      (kernel (cdr elements)))))]
[planet-placer ;;; procedure to place planet in image based on code from http://www.cs.grinnell.edu/~rebelsky/Courses/CSC151/2016S/labs/collage-lab.html
 (lambda ()
   (let ([planet-width (image-width planet)]
         [planet-height (image-width planet)])
     (image-select-ellipse! planet REPLACE 0 0 planet-width planet-height)
     (gimp-edit-copy-visible planet)
     (let ([pasted (car (gimp-edit-paste (image-get-layer image-blend) 1))])
       (image-select-ellipse! image-blend REPLACE 0 0 width height)                (image-select-nothing! image-blend)
       (gimp-layer-scale pasted (/ width 5) (/ height 5) 1)
       (gimp-image-flatten image-blend))))])
(planet-placer)
(image-show image-blend)
)))