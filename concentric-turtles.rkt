#lang racket
(require gigls/unsafe)

;;; Procedure:
;;;   turtle-polygon!
;;; Paramaters:
;;;   turtle, a turtle
;;;   side-length, a positive number
;;;   sides, a positive integer
;;; Produces:
;;;    <fill me in>
;;; Purpose:
;;;    <fill me in>
;;; Preconditions:
;;;    <fill me in>
;;; Postconditions:
;;;    <fill me in>
(define turtle-polygon!
  (lambda (turtle side-length sides)
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
      (initialise-turtle turtle)      
      (repeat sides draw-side turtle))))

;;; Procedure:
;;;   concentric-turtles!
;;; Paramaeters:
;;;   turtle, a turtle
;;;   length, a positive number
;;;   n, a positive integer
;;; Produces:
;;;   <fill me in>
;;; Purpose:
;;;   <fill me in>
;;; Preconditions:
;;;   <fil me in>
;;; Postconditions:
;;;   <fill me in>
(define concentric-turtles
  (lambda (turtle length n)
    (let kernel ([count n]
                 [sides 1])
      (cond
        [(zero? count)
         "many-turtles completed"]
        [else
         (turtle-polygon! turtle length sides)
         (kernel (- count 1) (+ sides 1))]))))