
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
  z
  id
  closest
  )


;;(make-point :x 0 :y 0 :z 0)

(let ((id 0))
  (defun next-id ()
    (incf id)
    id
  ))

(defun expr-to-points (exprs)
  (coerce 
   (map 'list
	(lambda (pt)
	  (destructuring-bind (x y z) pt (make-point :x x :y y :z z :id (next-id) :closest nil )))
	exprs)
   'vector))


(defparameter in (expr-to-points (input "../input.txt")))
(defparameter ex (expr-to-points (input "../example.txt")))

(defun square (x)
  (* x x ))


(defun distance (p1 p2)
  (sqrt (+ (square (- (point-x p1)(point-x p2)))
	   (square (- (point-y p1)(point-y p2)))
	   (square (- (point-z p1)(point-z p2))))))

(defun find-distances (vec)
  (let* ((len (length vec))
	 (vals (make-array (list len len) :initial-element nil))
	 (distances nil))
    (loop for i from 0 to (- len 1) do
      (loop for j from (+ i 1) to (- len 1) do
	(let* ((p1 (aref vec i))
	       (p2 (aref vec j))
	       (dist (distance p1 p2)))
	  (setf (aref vals i j) dist)
	  (setq distances (cons (list i j dist) distances)))))

    (setq distances (sort distances (lambda (x y) (< (third x)(third y)))))
    distances))

(defun take (xs n)
  (let ((result nil))
    (loop for i from 1 to n do 
      (setq result (cons (car xs) result))
      (setq xs (cdr xs)))
    (nreverse result)))

#|

0 19
0 7
2 13
7 19
17 18
9 12
11 16
2 8
14 19
2 18

... rest 

v.i v.j 

|#
(defun find-connections (connects vec i)
  (let* ((links (make-array (length vec) :initial-element nil))
	 (marked nil)
	 (marks nil)
	 (count 0))
    ;; set 0 as class 1 - others null
    (setf (aref links i) 1)
    (setq marks (list i))
    ;;
    (setq marked t) ;; entry 
    (loop while marked do
      (setq marked nil) ;; 
      (loop for i from 0 to (+ -1 (length connects)) do
	(destructuring-bind (a b d) (aref connects i)
	  (declare (ignore d))
	  (let ((la (aref links a))
		(lb (aref links b)))
	    (cond
	      ;; neither marked - neither yet reachable
	      ((and (null la)
		    (null lb))
	       nil)
	      ;; one is known - mark other
	      ((and (integerp la)
		    (null lb))
	       ;; (format t "~a is reachable from ~a ~%" b a)
	       (setq marks (cons b marks))
	       (incf count)
	       (setf (aref links b) la)
	       (setq marked t))
	      ;; one is known - mark other
	      ((and (integerp lb)
		    (null la))
	       ;; (format t "~a is reachable from ~a ~%" a b)
	       (setf (aref links a) lb)
	       (setq marks (cons a marks))
	       (incf count)
	       (setq marked t))
	      ;; both marked already 
	      ((and (integerp lb)
		    (integerp la))
	       nil))))))
    ;; how many did we mark ?
    ;; (format t "we marked ~a ~%" count)
    ;; (format t "those marked ~a : with length ~a ~%" marks (length marks))
    (sort marks #'<)))

(defun make-connections (vec n)
  (let* ((connects (coerce (take (find-distances vec) n) 'vector))
	 (hash (make-hash-table :test #'equalp))
	 (sizes nil))
    (loop for i from 0 to (+ -1(length vec)) do
      (let ((sorted (find-connections connects vec i)))
	(let ((found (gethash sorted hash nil)))
	  (cond
	    ((null found)
	     (setf (gethash sorted hash) t)
	     (setq sizes (cons (list (length sorted) sorted) sizes)))))))
    (sort sizes (lambda (x y) (> (first x)(first y))))))


(defun example-1 ()
  (let ((out (make-connections ex 10)))
    (format t "out => ~a ~%" out)
    (map 'list #'first (take out 3))))
	     
(defun part-1 ()
  (let ((out (make-connections in 1000)))
    (format t "out => ~a ~%" out)
    (let ((triple (map 'list #'first (take out 3))))
      (values (apply #'* triple) triple))))

;; (example-1)
;; (* 5 4 2 ) => 40
;; (5 4 2)

;; (part-1)
;; 131150
;; (86 61 25)

#|
part2 simply need to pick a number sufficiently high that they are all in one big group
(length (find-distances in)) => 499500 links

(112 498 1015.3344)
...
(64 432 159161.81)

so n is between 1000 and 499500

|#
      
    
(defun part-2 (n)
  (let ((out (make-connections in n)))
    (= (length out) 1)))

#|

FUN> (part-2 4871)
NIL
FUN> (part-2 4872)
T

;;(take (find-distances in) 4872) => (462 703 13933.048)
;; FUN> (aref in 462)
;; #S(POINT :X 1627 :Y 49288 :Z 24472 :ID 463 :CLOSEST NIL)
;; FUN> (aref in 703)
;; #S(POINT :X 1535 :Y 60520 :Z 32716 :ID 704 :CLOSEST NIL)
;; FUN> (* 1627 1535)
;; 2497445

answer accepted ! 

|#



    

