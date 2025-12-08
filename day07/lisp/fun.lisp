
;; (ql:quickload :uiop)
;; (ql:quickload :alexandria)
;; (ql:quickload :str)

(defpackage :fun
  (:use :cl))
(in-package :fun)

(declaim (optimize (speed 0)(debug 3)(space 0)))

(defun input (filename)
  (with-open-file (stream filename :direction :input)
    (let ((lines nil)
	  (result nil))
      (catch 'done 
	(loop while t do
	  (let ((line (read-line stream nil nil)))
	    (format t "line=>~a~%" line)
	    (cond
	      ((null line)
	       (setq result (nreverse lines))
	       (throw 'done t))
	      (t (setq lines (cons line lines)))))))
      result)))

(defparameter *in* (input "../input.txt"))
(defparameter *ex* (input "../example.txt"))

;; verified input -> output same file
(defun repro ()
  (with-open-file (stream "output.txt" :direction :output :if-exists :supersede)
    (dolist (v *in*)
      (format stream "~a~%" v))))

;; (defun show ()
;;   (format t "~%")
;;   (dolist (v *in*)
;;     (format t "~a~%" v))
;;   (format t "~%"))



;; find the S
(defun find-s (in)
  (let* ((s (car in))
	 (len (length s)))
    (catch 'found 
    (loop for i from 0 to (- len 1) do
      (let ((ch (char s i)))
	(cond
	  ((char= ch #\S)
	   (format t "i=~a ~%" i)
	   (throw 'found i)))))
    (error "not found"))))

;; (defparameter *hash* (make-hash-table :test #'equalp))

(defun make-beam-hash (in)
  (let* ((y 0)(len (length (car in)))(hash (make-hash-table :test #'equalp)))
    (setf (gethash 'width hash) len)
    (setf (gethash 'height hash) (length in))    
    (dolist (s in)
      (incf y)
      (loop for x from 0 to (- len 1) do
	(let ((ch (char s x)))
	  (cond
	    ((char= ch #\S) (format t "fbs ~a ~a ~%" x y)
	     (setf (gethash (list x y) hash) 'start)
	     (setf (gethash 'start hash) (list x y))
	     )
	    ((char= ch #\^) (setf (gethash (list x y) hash) 'splitter))
	    ((char= ch #\.) (setf (gethash (list x y) hash) 'empty))
	    (t (error "bad beam type"))))))
    hash))
	    

;; ;; recursive - takes too long as it follows each path until completion 
;; (defun rec (x y)
;;   (cond
;;     ((>= y 142) (format t "too far deep y ~a ~%" y))
;;     ((< x 0) (format t "too far left x ~a ~%" x))
;;     ((>= x (length (car *in*))) (format t "too far right x ~a ~%" x))
;;     (t (let ((elem (gethash (list x y) *hash* nil)))
;; 	 (cond
;; 	   ((null elem) (rec x (+ y 1)))
;; 	   ((eq elem 'start)
;; 	    (format t "we are at the start ~%")
;; 	    (rec x (+ y 1)))
;; 	   ((eq elem 'beam)
;; 	    (rec (- x 1)(+ y 1))
;; 	    (rec (+ x 1)(+ y 1)))
;; 	   (t (error "neither null start or beam")))))))
;; ;; kick off
;; ;; (rec 70 1)
;; ;;
;; ;;    |     | =>  |
;; ;;    ?     ^    |^|
;; ;;

(defun beam (x)
  (eq x 'beam))

(defun start (x)
  (eq x 'start))


(defun splitter (x)
  (eq x 'splitter))
      
(defun empty (x)
  (eq x 'empty))

;; start symbol above and empty then place beam here
;; beams carry on

(defun iterative (hash)
  (let ((beam-split-counter 0))
  (destructuring-bind (sx sy) (gethash 'start hash)
    (let ((y 1)(height (gethash 'height hash))(width (gethash 'width hash)))
      (incf y)
      (loop while (<= y height) do
	(loop for x from 0 to (- 141 1) do
	  (let ((above (gethash (list x (- y 1)) hash nil))
		(here (gethash (list x y) hash nil)))
	    (cond
	      ;; start S above - beam goes here
	      ((and (start above)(= x sx))
	       (setf (gethash (list x y) hash) 'beam))
	      ;; beams carry on 
	      ((and (beam above)(not (splitter here)))
	       (setf (gethash (list x y) hash) 'beam))
	      ;; beams split if meet splitter
	      ((and (beam above)(splitter here))
	       (incf beam-split-counter)
	       (setf (gethash (list (- x 1) y) hash) 'beam)
	       (setf (gethash (list (+ x 1) y) hash) 'beam)
	       ))))
	(incf y))
      (format t "beam split counter ~a ~%" beam-split-counter)
      hash))))


    


(defun show (hash)
  (let* ((wid (gethash 'width hash))
	 (hgt (gethash 'height hash)))
    (format t "~%")
    (loop for y from 1 to hgt do
      (format t "~%")
      (loop for x from 0 to (- wid 1) do
	(let ((elem (gethash (list x y) hash nil)))
	  (cond
	    ((beam elem) (format t "|"))
	    ((start elem) (format t "S"))
	    ((splitter elem) (format t "^"))
	    ((empty elem) (format t "."))
	    ((null elem) (format t "."))
	    (t
	     (format t "Bad show : {~a}~%" elem)
	     (error "bad show"))))))
    (format t "~%")))

;; (iterative)

	   
	
      	
    


