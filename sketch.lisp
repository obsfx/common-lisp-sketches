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

; lexical -> static
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

; https://lispmethods.com/symbols.html
; Essentially, symbols are objects, and, to 
; access them, you either need a reference to them in memory, 
; or you need to know their string name and “home package”.

; (setq bla 10) is roughly equivalent to (set 'bla 10), and can be 
; thought of as “set quoted”. It sets the value of the symbol bla, 
; rather than the value of the symbol stored in bla. This is an 
; oversimplification, as SET won’t work on lexically scoped variables, 
; while SETQ will. Have a look at their CLHS pages for more precise detail.


; https://stackoverflow.com/a/13213772/13615958
; LAMBDA is a macro. It expands (lambda ...) to (function (lambda ...)), 
; which is the equivalent of #'(lambda ...)).
; 
; The macro saves you a bit of writing/reading, that's all. In the first 
; version of Common Lisp (CLtL1) there was no LAMBDA macro. It has been added 
; later and is now a part of ANSI Common Lisp,

; The FUNCTION special operator

; FUNCTION is a special operator. It expects a function name or a lambda 
; expression. Thus the name or the lambda expression are not evaluated. 
; In fact lambda expressions can't be evaluated at all. Inside FUNCTION, 
; the lambda expression is not a macro form and thus will not be expanded again. 
; The purpose of FUNCTION is to return the corresponding function object 
; which is denoted by the name or by the lambda expression. It returns 
; the function object as a value. With this special operator one can access 
; the function object from global functions and lexical functions.
