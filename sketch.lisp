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
  (values x y))


(defvar *vector* (multiple-value-bind (x y)
    (vector2d 5 1)
  (list x y)))

; get first element of list
(print-line "--------")
(print-line (nth 0 *vector*))
(print-line (nth 1 *vector*))
(print-line (car *vector*))
(print-line (cdr *vector*))
(print-line "--------")

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

; local variables
(print-line "--------")
(let ((x 1)
      (y 2)
      (z 3))
  (print-line (multiply (list x y z))))

; define variables whose initial values depend on previous variables
(print-line "--------")
(let* ((x 1)
      (y (+ x 2))
      (z (+ x 4)))
  (print-line (multiply (list x y z))))

; Dynamic variables are sort of like global variables, but more useful: they are dynamically scoped. You define them either with defvar or defparameter, the differences being:
;   defparameter requires an initial value, defvar does not.
;   defparameter variables are changed when code is reloaded with a new initial value, defvar variables are not.
(print-line "--------")
(defparameter *defp* "defparameter")

(defun print-globalvar ()
  (print-line *defp*))

(print-globalvar)

(let ((*defp* "defparameter reloaded in local"))
  (print-globalvar))

(defvar *defv* "defvar")

(defun print-globalvar ()
  (print-line *defv*))

(print-globalvar)

(let ((*defv* "defvar reloaded in local"))
  (print-globalvar))

(defparameter *defp* "defparameter reloaded in global")
(defvar *defv* "defvar reloaded in global") 
(print-line *defp*)
(print-line *defv*)

; modifying list
(defparameter numbers (list 1 2 3 4))
(setf (nth 2 numbers) 80)
(print-line numbers)
