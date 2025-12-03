
;; day 3

(defpackage :fun
  (:use :cl))
(in-package :fun)

(defun input ()
  (with-open-file (stream "../input.txt" :direction :input)
    (let ((vals '()))
      (catch 'done
	(loop while t do
	  (let ((val (read stream nil nil)))
	    (cond
	      (val (setq vals (cons val vals)))
	      (t (throw 'done t))))))
      (nreverse vals))))

	
(defun example ()
  (list 987654321111111
	811111111111119
	234234234234278
	818181911112111))


;; joltage
(defun joltage (n)
  (let* ((str (format nil "~a" n))
	 (last (- (length str) 1))
	 (last2 (- (length str) 2))
	 (hi 0))
    ;; awkward 0 to n-1 indexs
    (loop for i from 0 to last2 do
      (loop for j from (+ i 1) to last do
	(let ((val (parse-integer (format nil "~a~a" (char str i) (char str j)))))
	  (cond
	    ((> val hi) (setq hi val)))
	  ;;(format t "i~a : j~a : val~a : hi ~a ~%" i j val hi)
	  )))
    hi))


(defun total-joltage (xs)
  (apply #'+ (map 'list #'joltage xs)))


(defun total-joltage-example ()
  (total-joltage (example)))

(defun total-joltage-input ()
  (total-joltage (input)))


(defun part-1 ()
  (total-joltage-input))


;; 12 times 
(defun joltage12 (n)
  (let* ((str (format nil "~a" n))
	 (last (- (length str) 1))
	 (last2 (- (length str) 2))
	 (last3 (- (length str) 3))
	 (last4 (- (length str) 4))
	 (last5 (- (length str) 5))
	 (last6 (- (length str) 6))
	 (last7 (- (length str) 7))
	 (last8 (- (length str) 8))
	 (last9 (- (length str) 9))
	 (last10 (- (length str) 10))
	 (last11 (- (length str) 11))
	 (last12 (- (length str) 12))	 
	 (hi 0))
    ;; awkward 0 to n-1 indexs
    (loop for i1 from 0 to last12 do
      (loop for i2 from (+ i1 1) to last11 do
	(loop for i3 from (+ i2 1) to last10 do
	  (loop for i4 from (+ i3 1) to last9 do
	    (loop for i5 from (+ i4 1) to last8 do
	      (loop for i6 from (+ i5 1) to last7 do
		(loop for i7 from (+ i6 1) to last6 do
		  (loop for i8 from (+ i7 1) to last5 do
		    (loop for i9 from (+ i8 1) to last4 do
		      (loop for i10 from (+ i9 1) to last3 do
			(loop for i11 from (+ i10 1) to last2 do
			  (loop for i12 from (+ i11 1) to last do
			    ;; (format t "string ~a has length is ~a ~%" str (length str))
			    ;; (format t "~a ~a ~a ; ~a ~a ~a ; ~a ~a ~a ; ~a ~a ~a ~%"
			    ;; 	    i1 i2 i3
			    ;; 	    i4 i5 i6
			    ;; 	    i7 i8 i9
			    ;; 	    i10 i11 i12)
			    
			    (let ((val (parse-integer (format nil "~a~a~a~a~a~a~a~a~a~a~a~a"
							      (char str i1)
							      (char str i2)
							      (char str i3)
							      (char str i4)
							      (char str i5)
							      (char str i6)
							      (char str i7)
							      (char str i8)
							      (char str i9)
							      (char str i10)
							      (char str i11)
							      (char str i12)
							      ))))
			      (cond
				((> val hi) (setq hi val)))
			      ;;(format t "i~a : j~a : val~a : hi ~a ~%" i j val hi)
			      )))))))))))))
    hi))

(defun total-joltage12 (xs)
  (let ((tot 0)
	(size (length xs))
	(i 0))
    (dolist (v xs)
      (let ((out (joltage12 v)))
	(incf i)
	(incf tot out)
	(format t "computed ~a of ~a ~%" i size)))
    tot))

(defun total-joltage12-example ()
  (total-joltage12 (example)))

(defun total-joltage12-input ()
  (total-joltage12 (input)))

(defun part-2 ()
  (total-joltage12-input))

;; algorithm not good enough to put twelve items onto a string and get largest possible value
;; brute force wont do as theres 200 input numbers like these
;; 4942223224223134312221222433336324234433314222333723222642441142184541322622221421243432273241422334
