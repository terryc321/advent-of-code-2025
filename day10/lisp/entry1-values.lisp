
(ql:quickload :parseq)
;; (ql:quickload :fiveam)
;; (ql:quickload :uiop)
;; (ql:quickload :alexandria)
;; (ql:quickload :str)

(defpackage :fun
  (:use :cl :parseq)
  (:local-nicknames (:p :parseq)))
(in-package :fun)

(declaim (optimize (speed 0)(debug 3)(space 0)))

;; (defun input (filename)
;;   (with-open-file (stream filename :direction :input)
;;     (read stream)))

;; (defparameter in (coerce (input "../input.txt") 'vector))
;; (defparameter ex (coerce (input "../example.txt") 'vector))


(defrule foo () "foo")
(defrule bar () "bar")
(defrule foobar () (and foo bar))

(parseq 'foobar "foobar")

(defrule open-bracket () "[")
(defrule close-bracket () "]")
(defrule foobar () (and open-bracket close-bracket))
(parseq 'foobar "[]")

(defrule hash-char () #\#)
(defrule dot-char () #\.)
(defrule open-bracket () #\[)
(defrule close-bracket () #\])
(defrule foobar () (and open-bracket (* (or hash-char dot-char)) close-bracket))
(parseq 'foobar "[]")
(parseq 'foobar "[#]")
(parseq 'foobar "[#.#.#]")

(defrule space () #\space )
(parseq 'space " ")

(defrule open-parens () #\( )
(defrule close-parens () #\) )
(defrule comma () #\,)
(defrule foobar () (and open-parens (+ integer) (* (and comma integer)) close-parens))
(parseq 'foobar "()")
(parseq 'foobar "(123)")
(parseq 'foobar "(123,456,789)")


(defrule int () (+ p::digit)) 
(parseq 'int "123")

(defrule integer () :integer)
(parseq 'integer "123")

;; check 

(defrule foobar () comma)
(parseq 'foobar ",")

(defrule foobar () open-parens)
(parseq 'foobar "(")

(defrule foobar () close-parens)
(parseq 'foobar ")")

(defrule foobar () (and open-parens int (* (and comma int)) close-parens))
(parseq 'foobar "()")
(parseq 'foobar "(123)")
(parseq 'foobar "(123,456,789)")

(multiple-value-bind (m b) (parseq 'foobar "(123,456,789)")
  (format t "m => ~a~%" m)
  (format t "b => ~a~%" b))


(defrule foobar () (and (and open-bracket (* (or hash-char dot-char)) close-bracket)
			space
			(* (and open-parens int (* (and comma int)) close-parens space))
			))
(parseq 'foobar "[#.#.#] (1,2) (3,4,5) (6,7,8,9,10) ")
(parseq 'foobar "[#.#.#] (12,34) (56,78,910) (1112,1314,1516,1718,1920) ")


(defrule open-brace () #\{ )
(defrule close-brace () #\} )
(defrule foobar () (and open-brace int (* (and comma int)) close-brace))
(parseq 'foobar "{}")
(parseq 'foobar "{12,34,56}")

(defrule foobar () (and (and open-bracket (* (or hash-char dot-char)) close-bracket)
			space
			(* (and open-parens int (* (and comma int)) close-parens space))
			(and open-brace int (* (and comma int)) close-brace)
			))
(parseq 'foobar "[#.#.#] (12,34) (56,78,910) (1112,1314,1516,1718,1920) {12,34,56}")


(defstruct collab
  gates
  g1
  g2
  )


;; read lines
;;uiop:slurp-stream-lines stream
;;
(defun parsed (filename)
  (with-open-file (stream filename :direction :input)
    (map 'list (lambda (line) (let ((the-parse (parseq 'foobar line)))
				(list line the-parse (make-collab :gates (gates the-parse)
								  :g1 (group1 the-parse)
								  :g2 (group2 the-parse)))))
	 (uiop:slurp-stream-lines stream))))

;; [#.#.#] - ok
;; (0,5)
;; (0,2)
;; (0,2,4,5)
;; {1,2,3,4,5} curlies last


(defun gates (the-parse)
  (coerce (second (first the-parse)) 'string))


;; (#\( (#\5 #\6) ((#\, (#\7 #\8)) (#\, (#\9 #\1 #\0))) #\) #\ )
;; (defun group1 (the-parse)
;;   (let ((groups (third the-parse)))
;;     (labels ((a (x) (second x))
;; 	     (r (x) (let ((seq (cdr (cdr x))))
;; 		      (format t "seq => ~a ~%" seq)
;; 		      (mapcar (lambda (z) (second (second z))) seq))))
;;       (mapcar (lambda (y)
;; 		(format t "first => ~a ~%" (a y))
;; 		;;(format t "others => ~a ~%" (r y))
;; 		)
;; 	      groups))))


;; is X a list where all elements are digit chars -> compute integer
;; (defun digits (expr)  
;;   (dolist (v expr)
;;     (cond
;;       ((and (listp v) (not (null v)))       
;;        (digits v)))))

(defun group1 (the-parse)
  (let* ((p (third the-parse))
	 (p2 (mapcar (lambda (x) (remove-if-not #'listp x)) p)))
    (mapcar (lambda (y)
	      (let ((result nil))
		;; (format t "y => ~a ~%" y)
		(let ((y1 (car y)))
		  ;; (format t "y1 => ~a ~%" y1)
		  (mapcar (lambda (y2)
			    (let* ((y3 (mapcar #'second y2))
				   (y4 (cons y1 y3))
				   (y5 (mapcar #'all-digits y4)))
			      ;; (format t "y2 => ~a ~%" y2)
			      ;; (format t "y3 => ~a ~%" y3)
			      ;; (format t "y4 => ~a ~%" y4)
			      ;; (format t "y5 => ~a ~%" y5)
			      (setq result y5)
			      ))
			  (cdr y)))
		result))
	    p2)))


;; cannot be empty list , cannot be any non digit characters
(defun all-digits (xs)
  (cond
    ((null xs) (values nil nil))
    (t
     (catch 'not-digit
       (dolist (v xs)
	 (when (not (characterp v))
	   (throw 'not-digit (values nil nil)))
	 (when (not (member v '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))
	   (throw 'not-digit (values nil nil))))
       (values (parse-integer (coerce xs 'string)) t)))))


(all-digits '(#\1 #\2 #\3))
(all-digits '(#\a #\2 #\c))


(defun group2 (the-parse)
  (let* (;; (the-parse (parseq 'foobar "[#.#.#] (1111,222,333,444,55555555) (66666,7,8,99,100,101,102) {10,11,12}"))
	 (p (fourth the-parse))
	 (p2 (butlast (cdr p)))
	 (y1 (car p2))
	 (y2 (cdr p2))
	 (y3 (mapcar #'all-digits (cons y1 (mapcar #'second (car y2))))))
    ;;(break)
    ;; (format t "y1 => ~a ~%" (list 'y1 y1))
    ;; (format t "y2 => ~a ~%" (list 'y2 y2))
    ;; (format t "y3 => ~a ~%" (list 'y3 y3))
    y3))




(defun input ()
  (mapcar #'third (parsed "../input.txt")))

;; (0,3,4) means
;; So, a button wiring schematic like (0,3,4) means that each time you push that button,
;; the first, fourth, and fifth indicator lights would all toggle between on and off.
;;
;; If the indicator lights were [#.....], pushing the button
;;      would change them to be [...##.] instead.
;;                               123456
;;     (0,3,4) => toggle 1 4 5   *  **
;; simple as mud
;; buttons are any listed in g1
;; ignore g2 as machines not running

#|

#S(COLLAB
:GATES "....##"
:G1 ((0 5) (0 2) (0 2 3 4 5) (0 2 4 5) (1 2 3 4 5) (0 1 4 5) (1 4 5))
:G2 (61 15 50 14 45 50))

given collab - how many button presses minimum will make lights come on -
the machine lights initially off
each machine has a different number of lights
pressing a "button" toggles the relevant lights

can use a string toggle - zero based - just use :gates given - except fill with "." offs

|#

(defun toggle(ch)
  (cond
    ((char= ch #\.) #\#)
    ((char= ch #\#) #\.)
    (t (error "bad ch"))))

(defun empty-string (str)
  (let ((len (length str))
	(res ""))
    (loop for i from 1 to len do
      (setq res (concatenate 'string res ".")))
    res))

;; keep original string as many buttons want to press on it 
(defun press-button(s button)
  (let ((s2 (copy-seq s)))
    (dolist (b button)
      (setf (char s2 b) (toggle (char s2 b))))
    s2))


(defun sim (c)
  (let ((states (make-hash-table :test #'equalp))
	(buttons (collab-g1 c))
	(end-state (collab-gates c))
	(initial-state (empty-string (collab-gates c)))
	(presses 0)
	(next-states nil))
    ;; first state
    (setf (gethash initial-state states) t)
    ;; 
    (catch 'found-solution
      (loop while t do
	(setq next-states (make-hash-table :test #'equalp))
	;; for each state known - key = "#..#" val=t b=(1 3 5) 
	(maphash #'(lambda (key val)
		     (dolist (b buttons)
		       (let ((s2 (press-button key b)))
			 (setf (gethash s2 next-states) t))))
		 states)
	;; next generation
	(setq states next-states)
	(incf presses)
	;; check if any is solved ?
	(maphash #'(lambda (key val)
		     (when (equalp key end-state)
		       (throw 'found-solution presses)))
		 states)
	;; otherwise
	    ))))

;; simulate each input
(defun part-1 ()
  (apply #'+ (mapcar #'sim (input))))


;; (part-1) => 500
;; accepted answer

(defun empty-state (xs)
  (let ((len (length xs))
	(res nil))
    (loop for i from 1 to len do
      (setq res (cons 0 res)))
    res))

(defun press-button2(s button)
  (let ((s2 (copy-seq s)))
    (dolist (b button)
      (setf (char s2 b) (toggle (char s2 b))))
    s2))


;; (defun sim2 (c)
;;   (let* ((states (make-hash-table :test #'equalp))
;; 	 (buttons (collab-g1 c))
;; 	 (end-state (collab-g2 c)) ;; end state {3 4 5 7}
;; 	 (initial-state (empty-state end-state))
;; 	 (presses 0)
;; 	 (next-states nil))
;;     ;; first state
;;     (setf (gethash initial-state states) t)
;;     ;; 
;;     (catch 'found-solution
;;       (loop while t do
;; 	(setq next-states (make-hash-table :test #'equalp))
;; 	;; for each state known - key = "#..#" val=t b=(1 3 5) 
;; 	(maphash #'(lambda (key val)
;; 		     (dolist (b buttons)
;; 		       (let ((s2 (press-button2 key b)))
;; 			 (setf (gethash s2 next-states) t))))
;; 		 states)
;; 	;; next generation
;; 	(setq states next-states)
;; 	(incf presses)
;; 	;; check if any is solved ?
;; 	(maphash #'(lambda (key val)
;; 		     (when (equalp key end-state)
;; 		       (throw 'found-solution presses)))
;; 		 states)
;; 	;; otherwise
;; 	    ))))

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

(defun solved-p (n1 n2 n3 n4 n5 n6 n7)
  (let* ((e0 61)
	 (e1 15)
	 (e2 50)
	 (e3 14)
	 (e4 45)
	 (e5 50)
	 (out0 (+ n1 n2 n3 n4 n6))
	 (out1 (+ n6 n7))
	 (out2 (+ n2 n3 n4 n5))
	 (out3 (+ n3 n5))
	 (out4 (+ n3 n4 n5 n6 n7))
	 (out5 (+ n1 n3 n4 n5 n6 n7))
	 (solved (and (= out0 e0)(= out1 e1) (= out2 e2)(= out3 e3)(= out4 e4)(= out5 e5))))
    ;; (format t "(~a:~a) (~a:~a) (~a:~a) (~a:~a) (~a:~a) (~a:~a) ~%"
    ;; 	    out0 e0
    ;; 	    out1 e1
    ;; 	    out2 e2
    ;; 	    out3 e3
    ;; 	    out4 e4
    ;; 	    out5 e5)
    
    solved))

(defun cannot-satisfy (s)
  (catch 'maybe
    (when (> (aref s 0) 61) (throw 'maybe t))
    (when (> (aref s 1) 15) (throw 'maybe t))
    (when (> (aref s 2) 50) (throw 'maybe t))
    (when (> (aref s 3) 14) (throw 'maybe t))
    (when (> (aref s 4) 45) (throw 'maybe t))
    (when (> (aref s 5) 50) (throw 'maybe t))
    nil))


;; (0 5)        0              5
;;end state => (61 15 50 14 45 50)
(defun n1 ()
  (let* ((end #(61 15 50 14 45 50))
	 (most (max 0 (min (aref end 0) (aref end 5)))))
    (catch 'unsat
      (loop for i from 0 to most do
	(let ((s (make-array 6 :initial-element 0)))
	  (setf (aref s 0) (+ i (aref s 0)))
	  (setf (aref s 5) (+ i (aref s 5)))
	  ;; cant yet be solved
	  (when (cannot-satisfy s) (throw 'unsat nil))
	  (n2 end s i))))))

;;FUN> (collab-g1 (car (input)))
;;((0 5) (0 2) (0 2 3 4 5) (0 2 4 5) (1 2 3 4 5) (0 1 4 5) (1 4 5))
;;              0     2
;;end state => (61 15 50 14 45 50)
;;              n1             n1
;;              n2    n2
;; most n2 can be is either 0 or minimum of whats left over from choice of n1
(defun n2 (end s n1)
  (let ((most (max 0 (min (- (aref end 0) (aref s 0))
			  (- (aref end 2) (aref s 2))))))
    (catch 'unsat
      (loop for i from 0 to most do
	(let ((s2 (copy-seq s)))    
	  (setf (aref s2 0) (+ i (aref s2 0)))
	  (setf (aref s2 2) (+ i (aref s2 2)))
	  (when (cannot-satisfy s2) (throw 'unsat nil))
	  (n3 end s2 n1 i))))))

;;((0 5) (0 2) (0 2 3 4 5) (0 2 4 5) (1 2 3 4 5) (0 1 4 5) (1 4 5))
;;             ----n3----
(defun n3 (end s n1 n2)
  (let ((most (max 0 (min (- (aref end 0) (aref s 0))
			  (- (aref end 2) (aref s 2))
			  (- (aref end 3) (aref s 3))
			  (- (aref end 4) (aref s 4))
			  (- (aref end 5) (aref s 5))))))
    (catch 'unsat

      (loop for i from 0 to most do    
	(let ((s2 (copy-seq s)))    
	  (setf (aref s2 0) (+ i (aref s2 0)))
	  (setf (aref s2 2) (+ i (aref s2 2)))
	  (setf (aref s2 3) (+ i (aref s2 3)))
	  (setf (aref s2 4) (+ i (aref s2 4)))
	  (setf (aref s2 5) (+ i (aref s2 5)))
	  (when (cannot-satisfy s2) (throw 'unsat nil))
	  (n4 end s2 n1 n2 i))))))

;;((0 5) (0 2) (0 2 3 4 5) (0 2 4 5) (1 2 3 4 5) (0 1 4 5) (1 4 5))
;;                         ----n4----
(defun n4 (end s n1 n2 n3)
  (let ((most (max 0 (min (- (aref end 0) (aref s 0))
			  (- (aref end 2) (aref s 2))			  
			  (- (aref end 4) (aref s 4))
			  (- (aref end 5) (aref s 5))
			  ))))
    (catch 'unsat

      (loop for i from 0 to most do     
	(let ((s2 (copy-seq s)))    
	  (setf (aref s2 0) (+ i (aref s2 0)))
	  (setf (aref s2 2) (+ i (aref s2 2)))
	  (setf (aref s2 4) (+ i (aref s2 4)))
	  (setf (aref s2 5) (+ i (aref s2 5)))
	  (when (cannot-satisfy s2) (throw 'unsat nil))
	  (n5 end s2 n1 n2 n3 i))))))


;;((0 5) (0 2) (0 2 3 4 5) (0 2 4 5) (1 2 3 4 5) (0 1 4 5) (1 4 5))
;;   n1   n2     n3          n4       ----n5----
(defun n5 (end s n1 n2 n3 n4)
  (let ((most (max 0 (min (- (aref end 1) (aref s 1))
			  (- (aref end 2) (aref s 2))
			  (- (aref end 3) (aref s 3))			  
			  (- (aref end 4) (aref s 4))
			  (- (aref end 5) (aref s 5))
			  ))))
    (catch 'unsat

      (loop for i from 0 to most do         
	(let ((s2 (copy-seq s)))    
	  (setf (aref s2 1) (+ i (aref s2 1)))
	  (setf (aref s2 2) (+ i (aref s2 2)))
	  (setf (aref s2 3) (+ i (aref s2 3)))
	  (setf (aref s2 4) (+ i (aref s2 4)))
	  (setf (aref s2 5) (+ i (aref s2 5)))
	  (when (cannot-satisfy s2) (throw 'unsat nil))
	  (n6 end s2 n1 n2 n3 n4 i))))))

;;((0 5) (0 2) (0 2 3 4 5) (0 2 4 5) (1 2 3 4 5) (0 1 4 5) (1 4 5))
;;   n1   n2     n3          n4          n5      ----n6----
(defun n6 (end s n1 n2 n3 n4 n5)
  (let ((most (max 0 (min (- (aref end 0) (aref s 0))
		          (- (aref end 1) (aref s 1))
			  (- (aref end 4) (aref s 4))
			  (- (aref end 5) (aref s 5))
			  ))))
    (catch 'unsat
      (loop for i from 0 to most do         
	(let ((s2 (copy-seq s)))
	  (setf (aref s2 0) (+ i (aref s2 0)))
	  (setf (aref s2 1) (+ i (aref s2 1)))
	  (setf (aref s2 4) (+ i (aref s2 4)))
	  (setf (aref s2 5) (+ i (aref s2 5)))
	  (when (cannot-satisfy s2) (throw 'unsat nil))
	  (n7 end s2 n1 n2 n3 n4 n5 i))))))

;;((0 5) (0 2) (0 2 3 4 5) (0 2 4 5) (1 2 3 4 5) (0 1 4 5) (1 4 5))
;;   n1   n2     n3          n4          n5         n6    ----n7----
(defun n7 (end s n1 n2 n3 n4 n5 n6)
  (let ((most (max 0 (min (- (aref end 1) (aref s 1))
			  (- (aref end 4) (aref s 4))
			  (- (aref end 5) (aref s 5))
			  ))))
    (catch 'unsat
      (loop for i from 0 to most do         
	(let ((s2 (copy-seq s)))    
	  (setf (aref s2 1) (+ i (aref s2 1)))
	  (setf (aref s2 4) (+ i (aref s2 4)))
	  (setf (aref s2 5) (+ i (aref s2 5)))
	  (when (cannot-satisfy s2) (throw 'unsat nil))
	  (final end s2 n1 n2 n3 n4 n5 n6 i))))))


(defun final (end s n1 n2 n3 n4 n5 n6 n7)
  (cond
    ((solved-p n1 n2 n3 n4 n5 n6 n7)
     (format t "solved ~a ~a ~a ~a ~a ~a ~a : tot ~a : ~a ~%" n1 n2 n3 n4 n5 n6 n7 (+ n1 n2 n3 n4 n5 n6 n7) end))
    (t nil)))






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

;; generate a solver
;; ((61 15 50 14 45 50)
;;  ((0 5) (0 2) (0 2 3 4 5) (0 2 4 5) (1 2 3 4 5) (0 1 4 5) (1 4 5)))

;; generates a list of n's from 1 to n-1 
(defun the-ns (n)
  (let ((res nil))
    (loop for i from 1 to (- n 1) do
      (let ((sym (intern (format nil "N~a" i))))
	(setq res (cons sym res))))
    (nreverse res)))
;; FUN> (the-ns 5)
;; (N1 N2 N3 N4)


(defun dynamic-cannot-satisfy (expected)
  (let ((len (length expected))
	(res nil))
    (loop for i from 0 to (- len 1) do
      (setq res (cons `(when (> (aref s ,i) ,(aref expected i)) (throw 'maybe t)) res)))
    (nreverse res)))
;; FUN> (dynamic-cannot-satisfy #(61 15 50 14 45 50))
;; ((WHEN (> (AREF S 0) 61) (THROW 'MAYBE T))
;;  (WHEN (> (AREF S 1) 15) (THROW 'MAYBE T))
;;  (WHEN (> (AREF S 2) 50) (THROW 'MAYBE T))
;;  (WHEN (> (AREF S 3) 14) (THROW 'MAYBE T))
;;  (WHEN (> (AREF S 4) 45) (THROW 'MAYBE T))
;;  (WHEN (> (AREF S 5) 50) (THROW 'MAYBE T)))

(defmacro punch (x)
  `(progn (write ,x)
	  (terpri)
	  (eval ,x)))


(defun generate-solver (coll)
  (let* ((expected (coerce (collab-g2 coll) 'vector))
	 (buttons (collab-g1 coll))
	 (n 1)
	 (result nil))
    ;;(concatenate 'string
    ;; (remove-if-not #'stringp
    
    ;;(format t "buttons => ~A~%" buttons)
    (terpri)
    (punch `(defun n1 ()
			(let* ((end ,expected)
			       ;;(most (max 0 (min (aref end 0) (aref end 5)))))
			       (most (max 0 (min ,@(mapcar (lambda (e) `(aref end ,e))
							   (car buttons))))))
			  (catch 'unsat
			    (loop for i from 0 to most do
			      (let ((s (make-array ,(length buttons) :initial-element 0)))
				;; (setf (aref s 0) (+ i (aref s 0)))
				;; (setf (aref s 5) (+ i (aref s 5)))
				,@(mapcar (lambda (e) `(setf (aref s ,e) (+ i (aref s ,e))))
					  (car buttons))
				;; cant yet be solved
				(when (cannot-satisfy s) (throw 'unsat nil))
				(n2 end s i)))))))
    
    (loop while (not (null (cdr buttons))) do 
      (setq buttons (cdr buttons))
      (incf n)
      (terpri)
	       (punch `(defun ,(intern (format nil "N~a" n))  (end s ,@(the-ns n))
			  ;; (let ((most (max 0 (min (- (aref end 0) (aref s 0))
			  ;; 			(- (aref end 2) (aref s 2))))))
			  (let ((most (max 0 (min ,@(mapcar (lambda (e) `(- (aref end ,e) (aref s ,e)))
							    (car buttons))))))			     
			    (catch 'unsat
			      (loop for i from 0 to most do
				(let ((s2 (copy-seq s)))
				  ;; (setf (aref s2 0) (+ i (aref s2 0)))
				  ;; (setf (aref s2 2) (+ i (aref s2 2)))
				  ,@(mapcar (lambda (e) `(setf (aref s2 ,e) (+ i (aref s ,e))))
					    (car buttons))
				  (when (cannot-satisfy s2) (throw 'unsat nil))
				  (,(intern (format nil "N~a" (+ n 1))) end s2 ,@(the-ns n) i))))))))
    (incf n)
    ;; solved condition when end == s
    (terpri)
    (punch `(defun ,(intern (format nil "N~a" n))  (end s ,@(the-ns n))
			(cond
			  ((equalp end s) (format t "solved Ns => ~a : S => ~a : ~a~%"
						  (list ,@(the-ns n))
						  s 
						  (+ ,@(the-ns n))
						  )))))

    ;; cannot satisfy
    (terpri)
    (punch `(defun cannot-satisfy (s)
	      (catch 'maybe
		,@(dynamic-cannot-satisfy expected)
		nil)))

    
    ;; (setq result (remove-if-not #'stringp result))
    ;; (concatenate 'string result)))
    ;; result))
    t
    ))

;;(time (generate-solver (third (input))))
;;
