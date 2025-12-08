
(ql:quickload :uiop)
(ql:quickload :alexandria)
(ql:quickload :str)

;; make grid from input text
;; option 1,1 top left or 0,0 

(defpackage :fun
  (:use :cl))
(in-package :fun)

(defun example ()
  '((3 5)
    (10 14)
    (16 20)
    (12 18)
    ()
    (1)
    (5)
    (8)
    (11)
    (17)
    (32)))


(defun input ()
  (with-open-file (stream "input.txt" :direction :input)
    (read stream)))

(defun process (xs)
  (let ((ranges nil)
	(nums nil))	
    (loop while xs do
      (let ((entry (car xs)))
	(cond
	  ((null entry) t)
	  ((= (length entry) 2)
	   (let ((lower (car entry))
		 (higher (car (cdr entry))))
	     (assert (>= higher lower))
	     (setq ranges (cons entry ranges))))
	  ((= (length entry) 1)
	   (setq nums (cons (car entry) nums)))
	  (t (error "cannot "))))
      (setq xs (cdr xs)))
    (format t "there are ~a ranges and ~a nums ~%" (length ranges) (length nums))
    (values ranges nums)))

(defun process1 (xs)
  (multiple-value-bind (ranges nums) (process xs)
    (let ((spoiled nil)
	  (fresh nil))
      (dolist (n nums)
	(catch 'fresh 
	  (dolist (range ranges)
	    (destructuring-bind (low hi) range
	      (when
		  (and (>= n low)(<= n hi))
		(setq fresh (cons n fresh))
		(throw 'fresh t))))
	  (setq spoiled (cons n spoiled))))
      (format t "spoiled => ~a ~%" spoiled)
      (format t "fresh => ~a ~%" fresh)      
      (format t "there are ~a spoiled and ~a fresh~%" (length spoiled) (length fresh)))))

;; (process1 (example))
;; there are 4 ranges and 6 nums 
;; spoiled => (1 8 32) 
;; fresh => (5 11 17) 
;; there are 3 spoiled and 3 fresh

;; (process1 (input))
;;there are 288 spoiled and 712 fresh

;; do any ranges overlap
;; how would know
;;
;;
;; overlap r.lo < r2.hi  and r.lo > r2.lo
;;
;;r          +---+
;;r2     *-----*
;;
;; overlap r.hi > r2.lo
;;r   +---+
;;r2    *----*
;;
;;
;; r    +---+
;; r2 *-------*
;;
;; no overlap r < r2
;; r   +---+
;; r2         *---*
;;
;; no overap r > r2
;; r             +---+    
;; r2   *---*
(defun process2 (xs)
  (multiple-value-bind (ranges nums) (process xs)
    (let ((spoiled nil)
	  (fresh nil))
      (dolist (r ranges)
	(dolist (r2 ranges)
	  (when (not (equalp r r2))
	    (destructuring-bind ((lo hi)(lo2 hi2)) (list r r2)
	      t)))))))


;;
;;
;;
      
    
