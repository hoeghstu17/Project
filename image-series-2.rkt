#lang racket
(require gigls/unsafe)

(define world (image-show (image-new 500 500)))
(define test-turtle (turtle-new world))
(turtle-set-brush! test-turtle "2. Hardness 100")

(define sol (list "orangered" "sienna" "orange" "yellow")) ;;; sun colors

(define radius
  (lambda (side-length sides)
    (/ side-length (* 2 (sin (/ pi sides))))))

(define sun-orbit-scaler
  (lambda (width height)
    (let ([max-width (* .6 (/ width 2))]
          [max-height (* .6 (/ height 2))])
      (let kernel ([side-length 1])
        (if
         (and
          (< (radius side-length 365) max-width)
          (< (radius side-length 365) max-height))         
         (kernel (+ 1 side-length))
         (- side-length 1))))))


(define turtle-sun-starter
  (lambda (turtle width height)
    (let* ([orbit-side-length (sun-orbit-scaler width height)]
           [orbit-radius (radius orbit-side-length 365)])
      (turtle-up! turtle)
      (turtle-teleport!
       turtle
       (/ (image-width (turtle-world turtle)) 2)
       (/ (image-height (turtle-world turtle)) 2)))))

(define turtle-polygon!
  (lambda (turtle side-length sides center-point)
    (let* ([center-x (car center-point)]
           [center-y (cdr center-point)]
           [polygon-radius (radius side-length sides)]
           [angle-of-exterior-turn
            (/ 360 sides)]
           [angle-of-initialising-turn
            (radians->degrees
             (- (- (* 2 pi) (degrees->radians (+ (turtle-angle turtle) 90)))
                (sin (/ (/ side-length 2) polygon-radius))))]
           [initialise-turtle
            (lambda (turtle)
              (turtle-teleport!
               turtle
               center-x
               center-y)               
              (turtle-up! turtle)
              (turtle-turn! turtle angle-of-initialising-turn)                            
              (turtle-forward! turtle polygon-radius)
              (turtle-face! turtle 0)
              (turtle-down! turtle))]
           [draw-side
            (lambda (turtle)
              (turtle-forward! turtle side-length)
              (turtle-turn! turtle angle-of-exterior-turn))])
      (initialise-turtle turtle)      
      (repeat sides draw-side turtle))))

(define sun-components
  (lambda (turtle sides center-point)
    (let kernel ([count 18]
                 [length 6])
      (cond
        [(zero? count)
         0] ;;; what should the base case be? idk
        [else
         (turtle-polygon! turtle length sides center-point)
         (kernel (- count 1) (* length 1.2))])))) ;;; outer deceahedron side-length is 133.11...


(define sun-maker
  (lambda (turtle center-point) ;;; center-point is a pair, eg. '(250 . 250)
    (sun-components turtle 8 center-point)
    (sun-components turtle 10 center-point)))






(define image-series
  (lambda (n width height)
    (let* ([n-list (list ;;; taken from http://www.cs.grinnell.edu/~rebelsky/Courses/CSC151/2016S/eboards/eboard.46.html
                    (quotient n 100)
                    (quotient (remainder n 100) 10)
                    (remainder n 10))]
           [n-list-sum (apply + n-list)]
           [n-list-a (car n-list)]
           [n-list-b (cadr n-list)]
           [n-list-c (caddr n-list)]
           [year (+ 1 (quotient n 365))] ;;; one year is 365 days long, start at year 1
           [element-season (+ 1 (floor (/ (remainder n 365) 73)))] ;;; 73 days per season, 5 seasons - does not work
           [plan (image-load
                  (cond
                    [(equal? year 1)
                     "/home/joshua/git/project/planet1.png"]
                    [(equal? year 2)
                     "/home/joshua/git/project/planet3.png"]
                    [(equal? year 3)
                     "/home/joshua/git/project/planet5.png"]
                    [else
                     "/home/joshua/git/project/planet6.png"]))]                                            
           
           [elements (list (list 1 'ARCANE "purple" "gold")
                           (list 2 'EARTH "forestgreen" "lightblue")
                           (list 3 'WATER "royalblue" "skyblue")
                           (list 4 'FIRE "gray" "orangered")                           
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
            (lambda (pixel primary secondary)
              (if (equal? pixel (irgb 255 255 255))
                  secondary
                  primary))]
           [planet-elementifier ;; procedure to check season/element and change colour of planet
            (lambda (image)
              (let kernel ([lst elements])
                (let ([element (car (car lst))])
                  (if
                   (equal? element-season element)
                   (let ([primary (color-name->irgb (caddr (car lst)))]
                         [secondary (color-name->irgb (cadddr (car lst)))])
                     (image-transform!
                      image
                      (section planet-color-changer ;;; David Neill Asanza helped with sectioning image-transform                          
                               <>
                               primary
                               secondary)))
                   (kernel (cdr lst))))))]                      
           [planet (planet-elementifier plan)] ;;; must change variable name
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