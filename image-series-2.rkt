#lang racket
(require gigls/unsafe)



(define sol (list "orangered" "sienna" "orange" "yellow")) ;;; sun colors


;;; Procedure:
;;;   radius
;;; Parameters:
;;;   side-length, a real number > 0
;;;   sides, an integer > 0
;;; Purpose:
;;;   calculating the radius of the circle that circumscribes a regular polygon
;;;   with sides number of sides, each of length side-length
;;; Produces:
;;;   radius-of-polygon, a real number
(define radius
  (lambda (side-length sides)
    (/ side-length (* 2 (sin (/ pi sides))))))

;;; UNFINISHED DOCUMENTATION
;;; Procedure:
;;;   turtle-polygon!
;;; Parameters:
;;;   center-point, a pair
;;; Purpose:
;;;   Using turtle to draw a satellite orbiting planet
;;; Produces:
;;;   Nothing, called for side effect
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

;;; UNFINISHED DOCUMENTATION
;;; Procedure:
;;;   satellite-orbit-scaler
;;; Parameters:
;;;   center-point, a pair
;;; Purpose:
;;;   Using turtle to draw a satellite orbiting planet
;;; Produces:
;;;   Nothing, called for side effect
(define satellite-orbit-scaler
  (lambda (width height)
    (let ([max-width (* .8 (/ width 2))] ;;; currently the sun is positioned relative to the height and width of the image, I would like to change this so that it is positioned relative to the center of the image
          [max-height (* .8 (/ height 2))])
      (let kernel ([side-length 1])
        (if
         (and
          (< (radius side-length 365) max-width)
          (< (radius side-length 365) max-height))         
         (kernel (+ 1 side-length))
         (- side-length 1))))))


;;; UNFINISHED DOCUMENTATION
;;; Procedure:
;;;   satellite-maker
;;; Parameters:
;;;   center-point, a pair
;;; Purpose:
;;;   Using turtle to draw a satellite orbiting planet
;;; Produces:
;;;   Nothing, called for side effect
(define satellite-maker
  (lambda (turtle n-of-polygons n-of-sides polygon-side-length center-point)
    (let kernel
      ([count n-of-polygons]
       [length polygon-side-length])
      (when (> count 0)
        (turtle-polygon! turtle length n-of-sides center-point)
        (kernel (- count 1) (* length 1.2))))))

;;; UNFINISHED DOCUMENTATION
;;; Procedure:
;;;   satellite-starter
;;; Parameters:
;;;   center-point, a pair
;;; Purpose:
;;;   Using turtle to draw a satellite orbiting planet
;;; Produces:
;;;   Nothing, called for side effect
(define satellite-starter
  (lambda (turtle n width height)
    (let* ([orbit-side-length (satellite-orbit-scaler width height)]
           [orbit-radius (radius orbit-side-length 365)]
           [angle-of-exterior-turn (/ 360 365)]
           [orbit
            (lambda (turtle)
              (turtle-forward! turtle orbit-side-length)
              (turtle-turn! turtle angle-of-exterior-turn))]
           [remaining-movements (modulo n 365)]
           [n-of-polygons 8]
           [n-of-sides 10]
           [polygon-side-length 6])
      (turtle-teleport!
       turtle
       (/ (image-width (turtle-world turtle)) 2)
       (/ (image-height (turtle-world turtle)) 2))
      (turtle-up! turtle)
      (turtle-face! turtle 270)
      (turtle-forward! turtle orbit-radius)
      (turtle-face! turtle 0)
      (repeat remaining-movements orbit turtle)
      (satellite-maker
       turtle
       n-of-polygons
       n-of-sides
       polygon-side-length
       (turtle-point turtle)))))


(define planet-chooser
  (lambda (n year)
    (image-load
     (cond
       [(> n 990)
        "/home/joshua/git/project/planet6.png"]
       [(equal? year 1)
        "/home/joshua/git/project/planet1.png"]
       [(equal? year 2)
        "/home/joshua/git/project/planet3.png"]
       [(equal? year 3)
        "/home/joshua/git/project/planet5.png"]                    
       [else
        "/home/joshua/git/project/planet6.png"]))))

(define elements
  (list (list 1 'ARCANE "purple" "gold")
        (list 2 'EARTH "forestgreen" "lightblue")
        (list 3 'WATER "royalblue" "skyblue")
        (list 4 'FIRE "gray" "orangered")                           
        (list 5 'AIR "whitesmoke" "skyblue")))

;;; taken from http://www.cs.grinnell.edu/~rebelsky/Courses/CSC151/2016S/eboards/eboard.46.html
(define int->list
  (lambda (int)
    (list 
     (quotient int 100)
     (quotient (remainder int 100) 10)
     (remainder int 10))))

(define background-blend
  (lambda (n width height)
    (let* ([n-list (int->list n)]
           [n-list-sum (apply + n-list)]
           [n-list-a (car n-list)]
           [n-list-b (cadr n-list)]
           [n-list-c (caddr n-list)]
           [max-value
            (lambda (val)
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
       height))))

(define planet-seasoning
  (lambda (n planet)
    (let ([element-season (+ 1 (floor (/ (remainder n 365) 73)))]
          [planet-color-changer
           (lambda (pixel primary secondary)
             (if (equal? pixel (irgb 255 255 255))
                 secondary
                 primary))])     
      (let kernel ([lst elements])
        (let ([element (car (car lst))])
          (if (equal? element-season element)
              (let ([primary (color-name->irgb (caddr (car lst)))]
                    [secondary (color-name->irgb (cadddr (car lst)))])
                (image-transform!
                 planet
                 (section planet-color-changer ;;; David Neill Asanza helped with sectioning image-transform                          
                          <>
                          primary
                          secondary)))
              (kernel (cdr lst))))))))

(define planet-placer
  (lambda (planet image)
    (let ([width-of-planet (image-width planet)]
          [height-of-planet(image-width planet)]
          [width-of-image (image-width image)]
          [height-of-image (image-height image)])
      (image-select-ellipse! planet REPLACE 0 0 width-of-planet height-of-planet)
      (gimp-edit-copy-visible planet)
      (let ([pasted (car (gimp-edit-paste (image-get-layer image) 1))])
        (image-select-ellipse! image REPLACE 0 0 width-of-image height-of-image)
        (image-select-nothing! image)
        (gimp-layer-scale pasted (/ width-of-image 5) (/ height-of-image 5) 1)
        (gimp-image-flatten image)))))


(define image-series
  (lambda (n width height)
    (let* ([year (+ 1 (quotient n 365))]           
           [planet (planet-chooser n year)]
           [image (background-blend n width height)]
           [solaris (turtle-new image)])
      (image-show image)
      (planet-seasoning n planet)
      (planet-placer planet image)
      (turtle-set-brush! solaris "2. Hardness 100")
      (satellite-starter solaris n width height))))
