
(ql:quickload :fiveam)
(ql:quickload :uiop)

(uiop:define-package :fun
    (:use :cl))

(in-package :fun)

(defun input ()
  (let ((exprs '()))
    (with-open-file (stream "../input.txt" :direction :input)
      (catch 'done
	(loop while t do
	  (let ((expr (read stream nil 'eof)))
	    (cond
	      ((eq expr 'eof) (throw 'done t))
	      (t (setq exprs (cons expr exprs))))))))
    (reverse exprs)))

(defun example ()
  '(L68
    L30
    R48
    L5
    R60
    L55
    L1
    L99
    R14
    L82))

(defun cnv (sym)
  (let* ((s (format nil "~a" sym))
	 (dir (cond
		((char= #\L (char s 0)) 'l)
		((char= #\R (char s 0)) 'r)
		(t (error "bad char"))))
	 (dist (parse-integer (subseq s 1 (length s)))))
    (list dir dist)))
		

(defun dial (n act)
  (cond
    ((eq (car act) 'l) (dial-left n (car (cdr act))))
    ((eq (car act) 'r) (dial-right n (car (cdr act))))
    (t (error "bad dial"))))

(defun dial-left (n dist)
  (let ((r n)
	(ct 0))
    (loop for i from 1 to dist do
      (setq r (- r 1))
      (if (< r 0)
	  (setq r (+ r 100)))
      (when (= r 0) (incf ct)))
    (values r ct)))

(defun dial-right (n dist)
  (let ((r n)
	(ct 0))
    (loop for i from 1 to dist do
      (setq r (+ r 1))
      (if (> r 99)
	  (setq r (- r 100)))
      (when (= r 0) (incf ct)))
    (values r ct)))


(defun test-dial-left ()
  (loop for d from 0 to (* 99 3) do
    (multiple-value-bind (r ct)	(dial-left 50 d)
      (format t "result ~a ; ~a : dial-start ~a : turn left ~a ~%" r ct 0 d))))

(defun test-dial-right ()
  (loop for d from 0 to (* 99 3) do
    (multiple-value-bind (r ct)	(dial-right 50 d)
      (format t "result ~a ; ~a : dial-start ~a : turn right ~a ~%" r ct 0 d))))



(defun run (xs)
  (let ((dial 50)
	(totzero 0))
    (dolist (v xs)
      (let* ((c (cnv v))
	     (dir (car c))
	     (dist (car (cdr c))))
	(cond
	  ((eq dir 'l) (multiple-value-bind (r ct) (dial-left dial dist)
			 (setq dial r)))
	  ((eq dir 'r) (multiple-value-bind (r ct) (dial-right dial dist)
			 (setq dial r)))
	  (t (error "run dial"))))
      (when (= dial 0)
	(incf totzero)))
    totzero))



(defun run2 (xs)
  (let ((dial 50)
	(totzero 0))
    (dolist (v xs)
      (let* ((c (cnv v))
	     (dir (car c))
	     (dist (car (cdr c))))
	(cond
	  ((eq dir 'l) (multiple-value-bind (r ct) (dial-left dial dist)
			 (setq dial r)
			 (incf totzero ct)))
	  ((eq dir 'r) (multiple-value-bind (r ct) (dial-right dial dist)
			 (setq dial r)
			 (incf totzero ct)))
	  (t (error "run dial")))))
    totzero))


(fiveam:def-suite suite-one)
(fiveam:in-suite suite-one)
(fiveam:test dial-left-1 (fiveam:is (= 99 (dial-left 0 1))))
(fiveam:test dial-right-1 (fiveam:is (= 19 (dial-right 11 8))))
(fiveam:test dial-left-2 (fiveam:is (= 0 (dial-left 19 19))))
(fiveam:test dial-left-3 (fiveam:is (= 95 (dial-left 5 10))))
(fiveam:test dial-run-1 (fiveam:is (= 3 (run (example)))))
(fiveam:test dial-run-2 (fiveam:is (= 1086 (run (input)))))
(fiveam:test dial-left-4 (fiveam:is (= 50 (dial-left 50 1000))))
(fiveam:test dial-left-5 (fiveam:is (= 10 (multiple-value-bind (r ct) (dial-left 50 1000) ct))))
(fiveam:run-all-tests)


#|

FUN> (run (example))
3
FUN> (run (input))
1086
FUN> (run2 (input))
6268
FUN> (run2 (example))
6
FUN> (run2 (input))
6268
FUN>
accepted answers part 1 & p(art 2

|#

