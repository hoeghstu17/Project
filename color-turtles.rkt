#lang racket
(require gigls/unsafe)

;;; Procedure:
;;;
;;; Parameters:
;;;
;;; Produces:
;;;
;;; Purpose:
;;;
;;; Preconditions:
;;;
;;; Postconditions
;;;
(define rainbow (list "red" "orange" "yellow" "green" "blue" "purple"))
(define earth (list "blue" "green" "green" "blue"))
(define sun (list "red" "goldenrod" "gold" "yellow"))
(define world (image-show (image-new 5000 5000)))
(define test-turtle (turtle-new world))

(define color-turtles
  (lambda (turtle side-length n colors)
    (let* ([lst colors]
          [len (length lst)])
       (let kernel ([count 0]
                     [sides 1])
          (cond
            [(> count n)
             "turtles colorified"]
            [else
             (turtle-polygon2!
              turtle side-length sides (list-ref lst (modulo count len)))
             (kernel (+ count 1) (+ sides 1))])))))

(define turtle-polygon2!
  (lambda (turtle side-length sides color)
    (let* ([radius
            (/ side-length (* 2 (sin (/ pi sides))))]
           [angle-of-exterior-turn
            (/ 360 sides)]
           [angle-of-initialising-turn
            (radians->degrees
             (- (- (* 2 pi) (degrees->radians (+ (turtle-angle turtle) 90)))
                (sin (/ (/ side-length 2) radius))))]
           [initialise-turtle
            (lambda (turtle)
              (turtle-teleport!
               turtle
               (/ (image-width (turtle-world turtle)) 2)
               (/ (image-height (turtle-world turtle)) 2))
              (turtle-up! turtle)
              (turtle-turn! turtle angle-of-initialising-turn)                            
              (turtle-forward! turtle radius)
              (turtle-face! turtle 0)
              (turtle-down! turtle))]           
           [draw-side
            (lambda (turtle)
              (turtle-forward! turtle side-length)
              (turtle-turn! turtle angle-of-exterior-turn))])
      (turtle-set-color! turtle color)
      (initialise-turtle turtle)      
      (repeat sides draw-side turtle))))
