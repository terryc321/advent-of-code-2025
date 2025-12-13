
;;(ql:quickload :uiop)
;;(ql:quickload :split-sequence)
;;(ql:quickload :alexandria)
;;(ql:quickload :fiveam)

;; (run-tests) often

(define-package :fun
  (:use #:cl)
  (:use #:uiop)
  (:use #:split-sequence)
  ;;(:local-nicknames (#:alex #:alexandria))
  (:local-nicknames (#:tt #:fiveam))
  )


(in-package :fun)

(defun read-file (filename)
   (with-open-file (stream filename :direction :input)
     (read stream)))

(defparameter in (read-file "../input.lisp"))
(defparameter ex (read-file "../example.lisp"))

;; 2d grid in text form to fix
(defun boards (input)
  (mapcar 'parse-grid
	  (mapcar (lambda (seq) 
		    (split-sequence:split-sequence #\newline seq :remove-empty-subseqs t))
		  (car input))))

(defstruct g arr)

(defun parse-grid (xs &optional (strict t))
  (assert (and (listp xs) (= (length xs) 3)))
  (mapcar (lambda (x) (assert (stringp x))) xs)
  (let ((arr (make-array '(5 5) :initial-element nil)))
    (loop for y from 0 to 2 do
      (let ((str (nth y xs)))
	(assert (and (stringp str) (= (length str) 3)))
	(loop for x from 0 to 2 do
	  (let ((ch (char str x))
		(val nil))
	    (setq val ch)
	    (when strict
	      (cond
		((char= ch #\.) (setq val nil))
		((char= ch #\#) (setq val t))
		(t (error "bad char"))))
	    (setf (aref arr (+ x 1) (+ y 1)) val)))))
    (make-g :arr arr)))

(defun show-grid (g)
  (assert (eq (type-of g) 'g))
  (loop for y from 1 to 3 do
    (terpri)
    (loop for x from 1 to 3 do
      (let ((e (aref (g-arr g) x y)))
	(cond
	  ((eq e t)  (format t "#"))
	  ((eq e nil) (format t "."))
	  (t (format t "~a" e))))))
  (terpri))


#|
123 => 741
456    852
789    963
|#
(defun rotate (g)
  (assert (eq (type-of g) 'g))
  (let ((arr (make-array '(5 5) :initial-element nil)))
    (setf (aref arr 3 1) (aref (g-arr g) 1 1)) 
    (setf (aref arr 3 2) (aref (g-arr g) 2 1)) 
    (setf (aref arr 3 3) (aref (g-arr g) 3 1)) 

    (setf (aref arr 2 1) (aref (g-arr g) 1 2)) 
    (setf (aref arr 2 2) (aref (g-arr g) 2 2)) 
    (setf (aref arr 2 3) (aref (g-arr g) 3 2))
    
    (setf (aref arr 1 1) (aref (g-arr g) 1 3)) 
    (setf (aref arr 1 2) (aref (g-arr g) 2 3)) 
    (setf (aref arr 1 3) (aref (g-arr g) 3 3)) 
    (make-g :arr arr)))


#|
123 => 321
456    654
789    987
|#
(defun flip (g)
  (assert (eq (type-of g) 'g))
  (let ((arr (make-array '(5 5) :initial-element nil)))
    (setf (aref arr 3 1) (aref (g-arr g) 1 1)) 
    (setf (aref arr 2 1) (aref (g-arr g) 2 1)) 
    (setf (aref arr 1 1) (aref (g-arr g) 3 1)) 

    (setf (aref arr 3 2) (aref (g-arr g) 1 2)) 
    (setf (aref arr 2 2) (aref (g-arr g) 2 2)) 
    (setf (aref arr 1 2) (aref (g-arr g) 3 2))
    
    (setf (aref arr 3 3) (aref (g-arr g) 1 3)) 
    (setf (aref arr 2 3) (aref (g-arr g) 2 3)) 
    (setf (aref arr 1 3) (aref (g-arr g) 3 3)) 
    (make-g :arr arr)))







;; ===== test suite ========

(tt:def-suite test-suite
  :description "Test my system.")
(tt:in-suite test-suite)

(tt:test grid-parse
  (let* ((g (parse-grid '("123" "456" "789") nil))
	 (arr (g-arr g)))	
    (tt:is (and (equalp (aref arr 1 1) #\1)
		(equalp (aref arr 2 1) #\2)
		(equalp (aref arr 3 1) #\3)
		(equalp (aref arr 1 2) #\4)
		(equalp (aref arr 2 2) #\5)
		(equalp (aref arr 3 2) #\6)
		(equalp (aref arr 1 3) #\7)
		(equalp (aref arr 2 3) #\8)
		(equalp (aref arr 3 3) #\9)))))

#|
123 => 741
456    852
789    963
|#
(tt:test grid-rotate
  (let* ((g (parse-grid '("123" "456" "789") nil))
	 (grot (rotate g))
	 (arr (g-arr grot)))	
    (tt:is (and (equalp (aref arr 1 1) #\7)
		(equalp (aref arr 2 1) #\4)
		(equalp (aref arr 3 1) #\1)
		(equalp (aref arr 1 2) #\8)
		(equalp (aref arr 2 2) #\5)
		(equalp (aref arr 3 2) #\2)
		(equalp (aref arr 1 3) #\9)
		(equalp (aref arr 2 3) #\6)
		(equalp (aref arr 3 3) #\3)))))

#|
123 => 321
456    654
789    987
|#
(tt:test grid-flip
  (let* ((g (parse-grid '("123" "456" "789") nil))
	 (gflip (flip g))
	 (arr (g-arr gflip)))	
    (tt:is (and (equalp (aref arr 1 1) #\3)
		(equalp (aref arr 2 1) #\2)
		(equalp (aref arr 3 1) #\1)
		(equalp (aref arr 1 2) #\6)
		(equalp (aref arr 2 2) #\5)
		(equalp (aref arr 3 2) #\4)
		(equalp (aref arr 1 3) #\9)
		(equalp (aref arr 2 3) #\8)
		(equalp (aref arr 3 3) #\7)))))

;; internally common lisp array is upside down on x axis then y axis



(defun run-tests ()
  (tt:run-all-tests))
