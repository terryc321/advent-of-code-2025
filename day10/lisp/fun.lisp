
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


