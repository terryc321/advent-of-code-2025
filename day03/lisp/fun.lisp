
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

;; -- this approach is too inefficient 
;; ;; 12 times 
;; (defun joltage12 (n)
;;   (let* ((str (format nil "~a" n))
;; 	 (last (- (length str) 1))
;; 	 (last2 (- (length str) 2))
;; 	 (last3 (- (length str) 3))
;; 	 (last4 (- (length str) 4))
;; 	 (last5 (- (length str) 5))
;; 	 (last6 (- (length str) 6))
;; 	 (last7 (- (length str) 7))
;; 	 (last8 (- (length str) 8))
;; 	 (last9 (- (length str) 9))
;; 	 (last10 (- (length str) 10))
;; 	 (last11 (- (length str) 11))
;; 	 (last12 (- (length str) 12))	 
;; 	 (hi 0))
;;     ;; awkward 0 to n-1 indexs
;;     (loop for i1 from 0 to last12 do
;;       (loop for i2 from (+ i1 1) to last11 do
;; 	(loop for i3 from (+ i2 1) to last10 do
;; 	  (loop for i4 from (+ i3 1) to last9 do
;; 	    (loop for i5 from (+ i4 1) to last8 do
;; 	      (loop for i6 from (+ i5 1) to last7 do
;; 		(loop for i7 from (+ i6 1) to last6 do
;; 		  (loop for i8 from (+ i7 1) to last5 do
;; 		    (loop for i9 from (+ i8 1) to last4 do
;; 		      (loop for i10 from (+ i9 1) to last3 do
;; 			(loop for i11 from (+ i10 1) to last2 do
;; 			  (loop for i12 from (+ i11 1) to last do
;; 			    ;; (format t "string ~a has length is ~a ~%" str (length str))
;; 			    ;; (format t "~a ~a ~a ; ~a ~a ~a ; ~a ~a ~a ; ~a ~a ~a ~%"
;; 			    ;; 	    i1 i2 i3
;; 			    ;; 	    i4 i5 i6
;; 			    ;; 	    i7 i8 i9
;; 			    ;; 	    i10 i11 i12)
;;			    
;; 			    (let ((val (parse-integer (format nil "~a~a~a~a~a~a~a~a~a~a~a~a"
;; 							      (char str i1)
;; 							      (char str i2)
;; 							      (char str i3)
;; 							      (char str i4)
;; 							      (char str i5)
;; 							      (char str i6)
;; 							      (char str i7)
;; 							      (char str i8)
;; 							      (char str i9)
;; 							      (char str i10)
;; 							      (char str i11)
;; 							      (char str i12)
;; 							      ))))
;; 			      (cond
;; 				((> val hi) (setq hi val)))
;; 			      ;;(format t "i~a : j~a : val~a : hi ~a ~%" i j val hi)
;; 			      )))))))))))))
;;     hi))


;; (char-code #\0) => 48
;; (char-code #\9) => 57

(defun ch2n (ch)
  (- (char-code ch) (char-code #\0)))

;; for a string str and a given sequence 
;; pick

(defun i_to_str (n)
  (second (assoc n '((1 one)(2 two)(3 three)(4 four)(5 five)(6 six)(7 seven)(8 eight)(9 nine)
		     (10 ten)(11 eleven)(12 twelve)(13 thirteen)))))
    

;; vino vi number
(defun vino ()
  (let* ((v 1)(n (- 12 v)))
    (format t "(let ((v~a (ch2n (char str 0)))~%" v)
    (format t "      (vi~a 0))~%" v)
    (format t " (loop for i from 0 to (+ len -1 -~a) do ~%" n)
    (format t "         (when (> (ch2n (char str i)) v2) ~%")
    (format t " 	    (setq vi1 i) ~%")
    (format t " 	    (setq v1 (ch2n (char str i)))))  ~%")
    (format t " 	(format t \"best initial guess one is ~aa for ~aa ~a%\" vi~a v~a) ~%~%" #\~ #\~ #\~ v v)
  
  (loop for v from 2 to 12 do 
    (let ((n (- 12 v)))
      (format t "(let ((v~a (ch2n (char str (+ vi~a 1))))~%" v (- v 1))
      (format t "      (vi~a (+ vi~a 1)))~%" v (- v 1) )
      (format t " (loop for i from (+ vi~a 1) to (+ len -1 -~a) do ~%" (- v 1) n)
      (format t "         (when (> (ch2n (char str i)) v~a) ~%" v)
      (format t " 	    (setq vi~a i) ~%" v)
      (format t " 	    (setq v~a (ch2n (char str i)))))  ~%" v)
      
      (format t " 	(format t \"best initial guess ~aa is ~aa for ~aa ~a%\" '~a vi~a v~a) ~%~%" #\~ #\~ #\~ #\~ (i_to_str v) v v)))))


  	  
	  

(defun joltage12 (n)
  (let* ((str (format nil "~a" n))
	 (len (length str)))
    ;; find highest 1st value -1 zero based 11 other digits to place
    (catch 'done 
      (let ((v1 (ch2n (char str 0)))
	    (vi1 0))
	(loop for i from 0 to (+ len -1 -11) do 
          (when (> (ch2n (char str i)) v1) 
 	    (setq vi1 i) 
 	    (setq v1 (ch2n (char str i)))))  
	(format t "best initial guess one is ~a for ~a ~%" vi1 v1) 

	(let ((v2 (ch2n (char str (+ vi1 1))))
	      (vi2 (+ vi1 1)))
	  (loop for i from (+ vi1 1) to (+ len -1 -10) do 
            (when (> (ch2n (char str i)) v2) 
 	      (setq vi2 i) 
 	      (setq v2 (ch2n (char str i)))))  
 	  (format t "best initial guess ~a is ~a for ~a ~%" 'TWO vi2 v2) 

	  (let ((v3 (ch2n (char str (+ vi2 1))))
		(vi3 (+ vi2 1)))
	    (loop for i from (+ vi2 1) to (+ len -1 -9) do 
              (when (> (ch2n (char str i)) v3) 
 		(setq vi3 i) 
 		(setq v3 (ch2n (char str i)))))  
 	    (format t "best initial guess ~a is ~a for ~a ~%" 'THREE vi3 v3) 

	    (let ((v4 (ch2n (char str (+ vi3 1))))
		  (vi4 (+ vi3 1)))
	      (loop for i from (+ vi3 1) to (+ len -1 -8) do 
		(when (> (ch2n (char str i)) v4) 
 		  (setq vi4 i) 
 		  (setq v4 (ch2n (char str i)))))  
 	      (format t "best initial guess ~a is ~a for ~a ~%" 'FOUR vi4 v4) 

	      (let ((v5 (ch2n (char str (+ vi4 1))))
		    (vi5 (+ vi4 1)))
		(loop for i from (+ vi4 1) to (+ len -1 -7) do 
		  (when (> (ch2n (char str i)) v5) 
 		    (setq vi5 i) 
 		    (setq v5 (ch2n (char str i)))))  
 		(format t "best initial guess ~a is ~a for ~a ~%" 'FIVE vi5 v5) 

		(let ((v6 (ch2n (char str (+ vi5 1))))
		      (vi6 (+ vi5 1)))
		  (loop for i from (+ vi5 1) to (+ len -1 -6) do 
		    (when (> (ch2n (char str i)) v6) 
 		      (setq vi6 i) 
 		      (setq v6 (ch2n (char str i)))))  
 		  (format t "best initial guess ~a is ~a for ~a ~%" 'SIX vi6 v6) 

		  (let ((v7 (ch2n (char str (+ vi6 1))))
			(vi7 (+ vi6 1)))
		    (loop for i from (+ vi6 1) to (+ len -1 -5) do 
		      (when (> (ch2n (char str i)) v7) 
 			(setq vi7 i) 
 			(setq v7 (ch2n (char str i)))))  
 		    (format t "best initial guess ~a is ~a for ~a ~%" 'SEVEN vi7 v7) 

		    (let ((v8 (ch2n (char str (+ vi7 1))))
			  (vi8 (+ vi7 1)))
		      (loop for i from (+ vi7 1) to (+ len -1 -4) do 
			(when (> (ch2n (char str i)) v8) 
 			  (setq vi8 i) 
 			  (setq v8 (ch2n (char str i)))))  
 		      (format t "best initial guess ~a is ~a for ~a ~%" 'EIGHT vi8 v8) 

		      (let ((v9 (ch2n (char str (+ vi8 1))))
			    (vi9 (+ vi8 1)))
			(loop for i from (+ vi8 1) to (+ len -1 -3) do 
			  (when (> (ch2n (char str i)) v9) 
 			    (setq vi9 i) 
 			    (setq v9 (ch2n (char str i)))))  
 			(format t "best initial guess ~a is ~a for ~a ~%" 'NINE vi9 v9) 

			(let ((v10 (ch2n (char str (+ vi9 1))))
			      (vi10 (+ vi9 1)))
			  (loop for i from (+ vi9 1) to (+ len -1 -2) do 
			    (when (> (ch2n (char str i)) v10) 
 			      (setq vi10 i) 
 			      (setq v10 (ch2n (char str i)))))  
 			  (format t "best initial guess ~a is ~a for ~a ~%" 'TEN vi10 v10) 

			  (let ((v11 (ch2n (char str (+ vi10 1))))
				(vi11 (+ vi10 1)))
			    (loop for i from (+ vi10 1) to (+ len -1 -1) do 
			      (when (> (ch2n (char str i)) v11) 
 				(setq vi11 i) 
 				(setq v11 (ch2n (char str i)))))  
 			    (format t "best initial guess ~a is ~a for ~a ~%" 'ELEVEN vi11 v11) 

			    (let ((v12 (ch2n (char str (+ vi11 1))))
				  (vi12 (+ vi11 1)))
			      (loop for i from (+ vi11 1) to (+ len -1 -0) do 
				(when (> (ch2n (char str i)) v12) 
 				  (setq vi12 i) 
 				  (setq v12 (ch2n (char str i)))))  
 			      (format t "best initial guess ~a is ~a for ~a ~%" 'TWELVE vi12 v12) 

			      (let ((value (parse-integer (format nil "~a~a~a~a~a~a~a~a~a~a~a~a"
								  v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12))))
				(format t "value => ~a~%" value)
				(throw 'done value)))))))))))))))))
                                                                 


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
;;
;; 171846613143331
;; accepted answer !
;;
;; key idea is just find highest digit within range from start to back - excluding 11 digits 
;; that still need to be placed. put the 1st digit there.
;;  now 2nd digit must begin after 1st digit and till back - excluding other 10 digits still to be
;;  placed . find highest in region. first match is optimal - as leaves space for other picks.
;;
;; just optimal by co-incidence gives greater chance finding a higher value
;;
;; even with printing every single guess for each digit and 200 , just done in blink of an eye.
;; Evaluation took:
;;   0.024 seconds of real time
;;   0.006564 seconds of total run time (0.006564 user, 0.000000 system)
;;   29.17% CPU
;;   88,650,734 processor cycles
;;   2,586,864 bytes consed
  
;; 171846613143331

