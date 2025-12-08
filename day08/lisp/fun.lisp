
;; (ql:quickload :uiop)
;; (ql:quickload :alexandria)
;; (ql:quickload :str)

(defpackage :fun
  (:use :cl))
(in-package :fun)

(declaim (optimize (speed 0)(debug 3)(space 0)))

(defun input (filename)
  (with-open-file (stream filename :direction :input)
    (read stream)))

(defstruct point
  x
  y
  z)

;;(make-point :x 0 :y 0 :z 0)

(defun expr-to-points (exprs)
  (map 'list
       (lambda (pt)
	 (destructuring-bind (x y z) pt (make-point :x x :y y :z z)))
       exprs))


(defparameter in (expr-to-points (input "../input.txt")))
(defparameter ex (expr-to-points (input "../example.txt")))


(defun dist (p1 p2)
  (sqrt (+ (abs (- (point-x p1)(point-x p2)))
	   (abs (- (point-y p1)(point-y p2)))
	   (abs (- (point-z p1)(point-z p2))))))






