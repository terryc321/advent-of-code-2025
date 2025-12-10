;; -*- chicken scheme -*-

(import (chicken format))
(import (chicken pretty-print));;pp

#|

#S(COLLAB
:GATES "....##"
:G1 ((0 5) (0 2) (0 2 3 4 5) (0 2 4 5) (1 2 3 4 5) (0 1 4 5) (1 4 5))
:G2 (61 15 50 14 45 50))


((0 5) (0 2) (0 2 3 4 5) (0 2 4 5) (1 2 3 4 5) (0 1 4 5) (1 4 5)) <- buttons
(0 5) * n1

0   1  2  3  4  5
n1              n1   (0         5)
n2    n2             (0   2      )
n3    n3  n3 n3 n3   (0   2 3 4 5)
n4    n4     n4 n4   (0   2   4 5)
n5 n5 n5  n5 n5  (  1 2 3 4 5)
n6 n6        n6 n6   (0 1     4 5)
n7        n7 n7   (  1     4 5)


200 * 200 * 200 * 200 * 200 * 200 * 200
(* 200 200 200 200 200 200 200) 12800000000000000

0: 61 = n1 + n2 + n3 + n4 +      n6
1: 15 =                          n6 + n7
2: 50 =      n2 + n3 + n4 + n5 
3: 14 =           n3 +      n5
4: 45 =           n3 + n4 + n5 + n6 + n7
5: 50 = n1 + ?  + n3 + n4 + n5 + n6 + n7          

0 1  2  3  4  5
end state => (61 15 50 14 45 50)

limit to to size of n1 - now n1 cannot be larger than 50
Ax = B math problem
A = (n1 n2 n3 n4 n5) x = (1 1 1 1 )  B desired output ?
?? nahh ?? 

n1 represents pressing button (0 5) once
n2 repre... button (0 2) once
n3 ... button (0 2 3 4 5)
n4 ... button (0 2 4 5)
n5 ... button (1 2 3 4 5)
n6 ... button (0 1 4 5 )
n7 ..  button (1 4 5)


((0 5) (0 2) (0 2 3 4 5) (0 2 4 5) (1 2 3 4 5) (0 1 4 5) (1 4 5)) 
n1    n2      n3          n4           n5      n6        n7

csp
constraint solver 

0  1  2  3  4  5
end state => (61 15 50 14 45 50)

reasoning engine
forward + rewind 


n1 affects 0 and 5 ->
look at 0 : have 61 ... n1 could be 61
look at 5 : have 50 ... n1 cannot be 61 ..too many .. atmost 50

n2 affects 0 and 2
look at current state due n1 choice - remaining amount 


|#

;; expected output
(define e0 61)
(define e1 15)
(define e2 50)
(define e3 14)
(define e4 45)
(define e5 50)
(define end (vector e0 e1 e2 e3 e4 e5))

;; check if solved given n values
;; each n corresponds to a specific button press
;; n1 means pressed button1 (n1 times)
(define solved-p (lambda (n1 n2 n3 n4 n5 n6 n7)
		   (let* (;; (n1 (vector-ref sn 0))
			  ;; (n2 (vector-ref sn 1))
			  ;; (n3 (vector-ref sn 2))
			  ;; (n4 (vector-ref sn 3))
			  ;; (n5 (vector-ref sn 4))
			  ;; (n6 (vector-ref sn 5))
			  ;; (n7 (vector-ref sn 6))
			  (out0 (+ n1 n2 n3 n4 n6))
			  (out1 (+ n6 n7))
			  (out2 (+ n2 n3 n4 n5))
			  (out3 (+ n3 n5))
			  (out4 (+ n3 n4 n5 n6 n7))
			  (out5 (+ n1 n3 n4 n5 n6 n7))
			  (solved (and (= out0 e0)(= out1 e1) (= out2 e2)(= out3 e3)(= out4 e4)(= out5 e5))))
		     solved)))

;; can we even satisfy given this current state
;; ie running totals given choices of buttons pressed so far 
(define can-satisfy
  (lambda (s)
    (call/cc (lambda (exit)
	      (when (> (vector-ref s 0) 61) (exit #f))
	      (when (> (vector-ref s 1) 15) (exit #f))
	      (when (> (vector-ref s 2) 50) (exit #f))
	      (when (> (vector-ref s 3) 14) (exit #f))
	      (when (> (vector-ref s 4) 45) (exit #f))
	      (when (> (vector-ref s 5) 50) (exit #f))
	      #t))))


;; functional loop instead of a macro 
(define myloop (lambda (from to fn)
		 (letrec ((foo (lambda (i)
				 (cond
				  ((and (>= i from)(<= i to)) (fn i)(foo (+ i 1)))
				  (#t #f)))))
		   (foo from))))

;;(myloop 1 10 (lambda (i) (format #t "i = ~a ~%" i)))
		  

(define copy-vector
  (lambda (v)
    (let* ((len (vector-length v))
	   (v2 (make-vector len 0)))
      (let loop ((i 0))
	(vector-set! v2 i (vector-ref v i))
	(cond
	 ((< i (- len 1)) (loop (+ i 1)))))
      v2)))


;; (0 5)        0              5
;;end state => (61 15 50 14 45 50)
(define n1
  (lambda ()
    (let* ((most (max 0 (min e0 e5))))
      (call/cc (lambda (unsat)
		 (myloop 0 most (lambda (i)
		       (let ((s (make-vector 6 0)))
			 (vector-set! s 0 (+ i (vector-ref s 0)))
			 (vector-set! s 5 (+ i (vector-ref s 5)))
			 ;; cant yet be solved
			 (when (not (can-satisfy s)) (unsat #f))
			 (n2 s i)))))))))


;;FUN> (collab-g1 (car (input)))
;;((0 5) (0 2) (0 2 3 4 5) (0 2 4 5) (1 2 3 4 5) (0 1 4 5) (1 4 5))
;;              0     2
;;end state => (61 15 50 14 45 50)
;;              n1             n1
;;              n2    n2
;; most n2 can be is either 0 or minimum of whats left over from choice of n1
(define n2
  (lambda (s n1)
    (let ((most (max 0 (min (- e0 (vector-ref s 0))
			    (- e2 (vector-ref s 2))))))
      (call/cc (lambda (unsat)
	(myloop 0 most (lambda (i)
	      (let ((s2 (copy-vector s)))    
		(vector-set! s2 0 (+ i (vector-ref s2 0)))
		(vector-set! s2 2 (+ i (vector-ref s2 2)))
		(when (not (can-satisfy s2)) (unsat #f))
		(n3  s2 n1 i)))))))))

;;((0 5) (0 2) (0 2 3 4 5) (0 2 4 5) (1 2 3 4 5) (0 1 4 5) (1 4 5))
;;             ----n3----
(define n3
  (lambda ( s n1 n2)
    (let ((most (max 0 (min (- e0  (vector-ref s 0))
			    (- e2 (vector-ref s 2))
			    (- e3 (vector-ref s 3))
			    (- e4 (vector-ref s 4))
			    (- e5 (vector-ref s 5))))))
      (call/cc (lambda (unsat)
		 (myloop 0 most (lambda (i)    
		       (let ((s2 (copy-vector s)))    
			 (vector-set! s2 0 (+ i (vector-ref s2 0)))
			 (vector-set! s2 2 (+ i (vector-ref s2 2)))
			 (vector-set! s2 3 (+ i (vector-ref s2 3)))
			 (vector-set! s2 4 (+ i (vector-ref s2 4)))
			 (vector-set! s2 5 (+ i (vector-ref s2 5)))
			 (when (not (can-satisfy s2)) (unsat #f))
			 (n4  s2 n1 n2 i)))))))))

;;((0 5) (0 2) (0 2 3 4 5) (0 2 4 5) (1 2 3 4 5) (0 1 4 5) (1 4 5))
;;                         ----n4----
(define n4
  (lambda ( s n1 n2 n3)
    (let ((most (max 0 (min (- e0 (vector-ref s 0))
			    (- e2 (vector-ref s 2))			  
			    (- e4 (vector-ref s 4))
			    (- e5 (vector-ref s 5))
			    ))))
      (call/cc (lambda (unsat)
		(myloop 0 most (lambda (i)  
		       (let ((s2 (copy-vector s)))    
			 (vector-set! s2 0 (+ i (vector-ref s2 0)))
			 (vector-set! s2 2 (+ i (vector-ref s2 2)))
			 (vector-set! s2 4 (+ i (vector-ref s2 4)))
			 (vector-set! s2 5 (+ i (vector-ref s2 5)))
			 (when (not (can-satisfy s2)) (unsat #f))
			 (n5  s2 n1 n2 n3 i)))))))))


;;((0 5) (0 2) (0 2 3 4 5) (0 2 4 5) (1 2 3 4 5) (0 1 4 5) (1 4 5))
;;   n1   n2     n3          n4       ----n5----
(define n5
  (lambda ( s n1 n2 n3 n4)
    (let ((most (max 0 (min (- e1 (vector-ref s 1))
			    (- e2 (vector-ref s 2))
			    (- e3 (vector-ref s 3))			  
			    (- e4 (vector-ref s 4))
			    (- e5 (vector-ref s 5))
			    ))))
      (call/cc (lambda (unsat)
	     (myloop 0 most (lambda (i)   
		       (let ((s2 (copy-vector s)))    
			 (vector-set! s2 1 (+ i (vector-ref s2 1)))
			 (vector-set! s2 2 (+ i (vector-ref s2 2)))
			 (vector-set! s2 3 (+ i (vector-ref s2 3)))
			 (vector-set! s2 4 (+ i (vector-ref s2 4)))
			 (vector-set! s2 5 (+ i (vector-ref s2 5)))
			 (when (not (can-satisfy s2)) (unsat #f))
			 (n6  s2 n1 n2 n3 n4 i)))))))))


;;((0 5) (0 2) (0 2 3 4 5) (0 2 4 5) (1 2 3 4 5) (0 1 4 5) (1 4 5))
;;   n1   n2     n3          n4          n5      ----n6----
(define n6
  (lambda ( s n1 n2 n3 n4 n5)
    (let ((most (max 0 (min (- e0 (vector-ref s 0))
		            (- e1 (vector-ref s 1))
			    (- e4 (vector-ref s 4))
			    (- e5 (vector-ref s 5))
			    ))))
      (call/cc (lambda (unsat)
	      (myloop 0 most (lambda (i)
		       (let ((s2 (copy-vector s)))
			 (vector-set! s2 0 (+ i (vector-ref s2 0)))
			 (vector-set! s2 1 (+ i (vector-ref s2 1)))
			 (vector-set! s2 4 (+ i (vector-ref s2 4)))
			 (vector-set! s2 5 (+ i (vector-ref s2 5)))
			 (when (not(can-satisfy s2)) (unsat #f))
			 (n7  s2 n1 n2 n3 n4 n5 i)))))))))

;;((0 5) (0 2) (0 2 3 4 5) (0 2 4 5) (1 2 3 4 5) (0 1 4 5) (1 4 5))
;;   n1   n2     n3          n4          n5         n6    ----n7----
(define n7
  (lambda ( s n1 n2 n3 n4 n5 n6)
    (let ((most (max 0 (min (- e1 (vector-ref s 1))
			    (- e4 (vector-ref s 4))
			    (- e5 (vector-ref s 5))
			    ))))
      (call/cc (lambda (unsat)
		(myloop 0 most (lambda (i)
		       (let ((s2 (copy-vector s)))    
			 (vector-set! s2 1 (+ i (vector-ref s2 1)))
			 (vector-set! s2 4 (+ i (vector-ref s2 4)))
			 (vector-set! s2 5 (+ i (vector-ref s2 5)))
			 (when (not (can-satisfy s2)) (unsat #f))
			 (final  s2 n1 n2 n3 n4 n5 n6 i)))))))))


(define final
  (lambda (s n1 n2 n3 n4 n5 n6 n7)
    (cond
     ((solved-p n1 n2 n3 n4 n5 n6 n7)
      (format #t "solved ~a ~a ~a ~a ~a ~a ~a : tot ~a : ~a ~%" n1 n2 n3 n4 n5 n6 n7 (+ n1 n2 n3 n4 n5 n6 n7) end))
     (#t #f))))

;; ====== compilation only ===========
(n1)


;; ;; functional loop use a lambda expression
;; (letrec ((foo (lambda (i fn)
;; 		(fn i)
;; 		(foo (+ i 1) fn))))
;;   (foo 1 (lambda (i) (format #t "i = ~a ~%" i))))
  



;; (defun brute ()
;;   (let ((n1 0)
;; 	(n2 0)
;; 	(n3 0)
;; 	(n4 0)
;; 	(n5 0)
;; 	(n6 0)
;; 	(n7 0))	
;;     (loop for n1 from 50 downto 0 do

;;       (loop for n2 from 
;;       (loop for n2 from 0 to 60 do
;; 	(loop for n3 from 0 to 60 do
;; 	  (loop for n4 from 0 to 60 do
;; 	    (loop for n5 from 0 to 60 do
;; 	      (loop for n6 from 0 to 60 do
;; 		(loop for n7 from 0 to 60 do





;; (defun description (buttons)
;;   (let* ((len (length buttons))
;; 	 (syms (let ((res nil))
;; 		 (loop for i from 0 to (- len 1) do
;; 		   (let ((but (nth i buttons)))
;;     len))






;; ;; simulate each input
;; (defun part-2 ()
;;   (apply #'+ (mapcar #'sim2 (input))))


