

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

(defparameter in (coerce (input "../input.txt") 'vector))
(defparameter ex (coerce (input "../example.txt") 'vector))

(defstruct point
  x
  y)

#|

when x1 == x2 the area is 1 times difference in y direction 

case a
1-
-2

case b
-1
2-

case c
-2
1-

case d
2-
-1

case e
1-2

case f
2-1

case g
1
|
2

case h
2
|
1

|#

(defun find-area (vec i j)
  (let* ((pr1 (aref vec i))
	 (pr2 (aref vec j))
	 (point1 (make-point :x (first pr1) :y (second pr1)))
	 (point2 (make-point :x (first pr2) :y (second pr2)))
	 (px1 (point-x point1))
	 (px2 (point-x point2))
	 (py1 (point-y point1))
	 (py2 (point-y point2))
	 (area (* (+ 1 (abs (- px2 px1))) (+ 1 (abs (- py2 py1))))))
    (cond
      ((= px1 px2) (setq area (abs (- py2 py1))))
      ((= py1 py2) (setq area (abs (- px2 px1))))
      (t area))
    area))
  

(defun largest-rect (vec)
  (let ((len (length vec))
	(best-area nil)
	(best-coords nil))
    (loop for i from 0 to (+ -1 len) do
      (loop for j from (+ i 1) to (+ -1 len) do
	(let ((area (find-area vec i j)))
	  (format t "area for ~a ~a => ~a ~%" (aref vec i) (aref vec j) area)
	  (cond
	    ((null best-area)
	     (setq best-area area)
	     (setq best-coords (list i j)))
	    ((> area best-area)
	     (setq best-area area)
	     (setq best-coords (list i j)))))))
    (values best-area best-coords)))


#|

> (largest-rect in)
4776487744
(63 309)
FUN> (aref in 63)
(83796 85128)
FUN> (aref in 309)
(15029 15671)


|#



	
