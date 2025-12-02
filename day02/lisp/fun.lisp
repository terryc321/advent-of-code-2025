
(ql:quickload :fiveam)
(ql:quickload :uiop)

(defpackage :fun
  (:use :cl))
(in-package :fun)

#|
11-22 has two invalid IDs, 11 and 22.
95-115 has one invalid ID, 99.
998-1012 has one invalid ID, 1010.
1188511880-1188511890 has one invalid ID, 1188511885.
222220-222224 has one invalid ID, 222222.
1698522-1698528 contains no invalid IDs.
446443-446449 has one invalid ID, 446446.
38593856-38593862 has one invalid ID, 38593859.
The rest of the ranges contain no invalid IDs.
|#

#+nil
(map 'list (lambda (x) (apply #'range x)) (example))

#+nil
(map 'list (lambda (x) (apply #'range x)) (input))

(defun range (a b)
  (assert (< a b))
  (let* ((ct '())
	 (fn (lambda (n)
	       (when (and (>= n a) (<= n b))
		 (setq ct (cons n ct))))))
    (range2 fn)
    (range4 fn)
    (range6 fn)
    (range8 fn)
    (range10 fn)
    (values ct (length ct) (apply #'+ ct))))


  ;; (format t "Range of ~a ... ~a : " a b)
  ;; (cond
  ;;   ((and (= (length (format nil "~a" a))
  ;; 	     (length (format nil "~a" b))))
  ;;    (format t "length of ~a ~%" (length (format nil "~a" a))))
  ;;   (t (format t "differ! ~a <> ~a ~%"
  ;; 	       (length (format nil "~a" a))
  ;; 	       (length (format nil "~a" b))))))




;; Range of 998 ... 1012 : differ! 3 <> 4 
;; Range of 998 ... 999 + 1000 ... 1012 : differ! 3 <> 4

;;  -n1- | -n2-
;; i1 i0   i1 i0
(defun range2 (fn)
  (loop for i0 from 1 to 9 do
      (let* ((n1 (* 10 i0))
	     (n2 (+ i0))
	     (rep (+ n1 n2)))
	(funcall fn rep))))
	;; (format t "~a : n1 = ~a : n2 = ~a : rep = ~a ~%" i0 n1 n2 rep))))

(defun range4 (fn)
  (loop for i0 from 1 to 9 do
    (loop for i1 from 0 to 9 do
      (let* ((n1 (* 100 (+ i1 (* i0 10))))
	     (n2 (+ i1 (* i0 10)))
	     (rep (+ n1 n2)))
	(funcall fn rep)))))
;;	(format t "~a : ~a : n1 = ~a : n2 = ~a : rep = ~a ~%" i0 i1 n1 n2 rep)))))


(defun range6 (fn)
  (loop for i0 from 1 to 9 do
    (loop for i1 from 0 to 9 do
      (loop for i2 from 0 to 9 do      
	(let* ((n1 (* 1000 (+ i2 (* i1 10) (* i0 100))))
	       (n2 (+ i2 (* i1 10) (* i0 100)))
	       (rep (+ n1 n2)))
	  (funcall fn rep))))))
;;	  (format t "~a : ~a : ~a : n1 = ~a : n2 = ~a : rep = ~a ~%" i0 i1 i2 n1 n2 rep))))))


(defun range8 (fn)
  (loop for i0 from 1 to 9 do
    (loop for i1 from 0 to 9 do
      (loop for i2 from 0 to 9 do      
	(loop for i3 from 0 to 9 do      
	  (let* ((n1 (* 10000 (+ i3 (* i2 10) (* i1 100) (* i0 1000))))
		 (n2  (+ i3 (* i2 10) (* i1 100) (* i0 1000)))
		 (rep (+ n1 n2)))
	    (funcall fn rep)))))))
	    ;; (format t "~a : ~a : ~a : ~a : n1 = ~a : n2 = ~a : rep = ~a ~%" i0 i1 i2 i3 n1 n2 rep)))))))

(defun range10 (fn)
  (loop for i0 from 1 to 9 do
    (loop for i1 from 0 to 9 do
      (loop for i2 from 0 to 9 do      
	(loop for i3 from 0 to 9 do
	  (loop for i4 from 0 to 9 do	  
	    (let* ((n1 (* 100000 (+ i4 (* i3 10) (* i2 100) (* i1 1000) (* i0 10000))))
		   (n2  (+ i4 (* i3 10) (* i2 100) (* i1 1000) (* i0 10000)))
		   (rep (+ n1 n2)))
	      (funcall fn rep))))))))
	      ;; (format t "~a : ~a : ~a : ~a : ~a : n1 = ~a : n2 = ~a : rep = ~a ~%" i0 i1 i2 i3 i4 n1 n2 rep))))))))





;; (defun invalid-p (n)
;;   (let ((str (format nil "~a" n)))
;;     (format t "invalid? ~a ~%" str)))


;; (defun drop (n xs)  
;;   (loop for i from 1 to n do
;;     (setq xs (cdr xs)))
;;   xs)

;; (defun take (n xs)
;;   (let ((r '()))
;;     (loop for i from 1 to n do
;;       (setq r (cons (car xs) r))
;;       (setq xs (cdr xs)))
;;     (nreverse r)))

;; substrings of length 1
;; substrings of length 2
;; substrings of length 3
;; substrings of length 4
;; substrings of length 5
;; substrings of length 6
;; longest string ?

;; 1 2 3 4 5 6 7 8 9 0

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


;; sum invalid id - checking had correct input 
;; (defun input2 ()
;;   '(
;; (7777742220 7777814718)
;; (3201990 3447830)
;; (49 86)
;; (653243 683065)
;; (91 129)
;; (24 41)
;; (1 15)
;; (2678 4638)
;; (1407 2511)
;; (221 504)
;; (867867 942148)
;; (1167452509 1167622686)
;; (9957459726 9957683116)
;; (379068 535983)
;; (757 1242)
;; (955118 1088945)
;; (297342 362801)
;; (548256 566461)
;; (4926 10075)
;; (736811 799457)
;; (1093342 1130060)
;; (620410 651225)
;; (65610339 65732429)
;; (992946118 993033511)
;; (5848473 5907215)
;; (17190619 17315301)
;; (203488 286290)
;; (15631 36109)
;; (5858509282 5858695889)
;; (87824047 87984031)
;; (1313113913 1313147594)
;; (795745221 795825571)
;; (46303 100636)
;; (4743038 4844422)
;;     ))


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

(defun largest (xs)
  (let ((big 0))
    (map 'list (lambda (x)
		 (when (> (car x) big) (setq big (car x)))
		 (when (> (car (cdr x)) big) (setq big (car (cdr x)))))
	 xs)
    big))

(defun do-example ()
  (let ((xs (example))
	(tot 0))
    (dolist (v xs)
      (multiple-value-bind (m ct sum) (apply #'range v)
	(format t "matches ~a : ct ~a : sum ~a ~%" m ct sum)
	(incf tot sum)))
    tot))


(defun part-1 ()
  (let ((xs (input))
	(tot 0))
    (dolist (v xs)
      (multiple-value-bind (m ct sum) (apply #'range v)
	(format t "matches ~a : ct ~a : sum ~a~%" m ct sum)
	(incf tot sum)))
    tot))



;; (part-1)
;; The assertion (< A B) failed with A = 4743038, B = 484442.
;;   [Condition of type SIMPLE-ERROR]
;; 4844422
;;       ^ missing last digit

	


#|

length 2
1
 2
>= 10
<= 100

length 4
12
  34
1000
10000

length 6
123 
   456  
100000
1000000

length 8 
1234
    5678 
10000000
100000000

lnegth 10
12345
     54321
1000000000
10000000000

|#








    
