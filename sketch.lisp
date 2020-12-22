;;; Common Lisp Sketches

; function definition
(defun print-line (output)
  (format t "output ->  ~A~%" output))

(print-line "Hello World")

; function definition with key parameters and default values
(defun distance (&key (x 0) (y 0))
  (sqrt (+ (* x x) (* y y))))

(print-line (distance))
(print-line (distance :x 3 :y 4))

; multiple value return
(defun vector2d (x y)
  (list x y))

(defvar *vector* (vector2d 5 10))

; get first element of list
(print-line (nth 0 *vector*))
(print-line (car *vector*))

; car
; It takes a list as argument, and returns its first element.
; 
; cdr
; It takes a list as argument, and returns a list without the first element
; 
; cons
; It takes two arguments, an element and a list and returns a list with the element inserted at the first place.
; 
; list
; It takes any number of arguments and returns a list with the arguments as member elements of the list.
; 
; append
; It merges two or more list into one.
; 
; last
; It takes a list and returns a list containing the last element.
; 
; member
; It takes two arguments of which the second must be a list, if the first argument is a member of the second argument, and then it returns the remainder of the list beginning with the first argument.
; 
; reverse
; It takes a list and returns a list with the top elements in reverse order.

(defun multiply (numbers)
  (if (> (length numbers) 0)
      (* (car numbers) (multiply (cdr numbers)))
      1))

(print-line (multiply (list 2 4 8 10 20)))

