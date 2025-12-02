
(ql:quickload :fiveam)
(ql:quickload :uiop)

(defpackage :fun
  (:use :cl))
(in-package :fun)

(defun brute ()
  (let ((ct 0)
	(dummy 0)
	(hash (make-hash-table :test #'eql)))
    (loop for i from 1 to 99999 do
      (catch 'enough 
	(let* ((str (format nil "~a" i))
	       (slen (length str))	  
	       (res str)
	       (rlen slen))
	  (loop for j from 2 to 14 do
	    (setq res (concatenate 'string res str))
	    (setq rlen (+ rlen slen))
	    (cond
	      ((> rlen 10) (throw 'enough t))
	      (t (let ((val (parse-integer res)))
		   (let ((stored (gethash val hash nil)))
		     (cond
		       (stored
			;;(format t "value already stored ~a ~%" val)
			(setq dummy 1)
			)
		       (t 
			(setf (gethash val hash) t)
			(incf ct)
			;;(format t "~a~%" val)
			))))))))))
    ;; (format t "ct =~a ~%" ct)
    hash))

(defparameter *hash* (brute))

;; loop over hash keys and see if the value is between range of interest if so add up that number
(defun range (a b)
  (assert (< a b))
  (let* ((ct '())
	 (fn (lambda (n)
	       (when (and (>= n a) (<= n b))
		 (setq ct (cons n ct))))))
    (maphash #'(lambda (k v)
	       (funcall fn k))
	     *hash*)
    (values ct (length ct) (apply #'+ ct))))






	
      

#|

n = i0
n
n n
n n n
n n n n
n n n n n
n n n n n n

i0 i0 i0
i0 i0 i0
1
11
111
1111
11111
111111
1111111

2
22
222
2222
22222
222222

3
33
333
3333
33333

12
1212
121212
12121212
1212121212
121212121212

13
1313
131313
13131313

|#


(defun example ()
  '((11 22)
    (95 115)
    (998 1012)
    (1188511880 1188511890)
    (222220 222224)
    (1698522 1698528)
    (446443 446449)
    (38593856 38593862)
    (565653 565659)
    (824824821 824824827)
    (2121212118 2121212124)))


(defun input ()
  '((7777742220 7777814718)
    (3201990 3447830)
    (49 86)
    (653243 683065)
    (91 129)
    (24 41)
    (1 15)
    (2678 4638)
    (1407 2511)
    (221 504)
    (867867 942148)
    (1167452509 1167622686)
    (9957459726 9957683116)
    (379068 535983)
    (757 1242)
    (955118 1088945)
    (297342 362801)
    (548256 566461)
    (4926 10075)
    (736811 799457)
    (1093342 1130060)
    (620410 651225)
    (65610339 65732429)
    (992946118 993033511)
    (5848473 5907215)
    (17190619 17315301)
    (203488 286290)
    (15631 36109)
    (5858509282 5858695889)
    (87824047 87984031)
    (1313113913 1313147594)
    (795745221 795825571)
    (46303 100636)
    (4743038 4844422)))

(defun do-example ()
  (let ((xs (example))
	(tot 0))
    (dolist (v xs)
      (multiple-value-bind (m ct sum) (apply #'range v)
	(format t "matches ~a : ct ~a : sum ~a ~%" m ct sum)
	(incf tot sum)))
    tot))


(defun part-2 ()
  (let ((xs (input))
	(tot 0))
    (dolist (v xs)
      (multiple-value-bind (m ct sum) (apply #'range v)
	(format t "matches ~a : ct ~a : sum ~a~%" m ct sum)
	(incf tot sum)))
    tot))



