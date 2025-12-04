
(ql:quickload :uiop)
(ql:quickload :alexandria)
(ql:quickload :str)

;; make grid from input text
;; option 1,1 top left or 0,0 

(defpackage :fun
  (:use :cl))
(in-package :fun)

;; usual routine - load file data - parse then begin attack problem


(defstruct grid
  wid
  hgt
  data)

;; list of strings 
(defun new-grid (xs)
  (let ((hgt (length xs))
	(wid (length (car xs))))
    (let ((data (make-array (list (+ wid 2)(+ hgt 2)))))
      (loop for y from 1 to hgt do
	(let ((str (nth (- y 1) xs)))
	  (loop for x from 1 to wid do
	    (let ((ch (char str (- x 1))))
	      (setf (aref data x y) ch)))))
      (make-grid :wid wid
		 :hgt hgt
		 :data data))))

(defun show-grid (g)
  (let ((hgt (grid-hgt g))
	(wid (grid-wid g))
	(data (grid-data g)))
    (format t "~%")    
    (loop for y from 1 to hgt do
      (format t "~%")
      (loop for x from 1 to wid do
	(let ((ch (aref data x y)))
	  (format t "~a" ch)
	  )))))


(defun input ()
  (new-grid (str:lines (str:from-file "../input.txt"))))

(defun example ()
  (new-grid (str:lines (str:from-file "../example.txt"))))

(defun at (g x y)
  (aref (grid-data g) x y))

(defparameter in (input))
(defparameter ex (example))

(defun roll-p (ch)
  (equalp ch #\@))

(defun empty-p (ch)
  (equalp ch #\.))

;; they are square inputs 
;; example = 10 x 10
;; input = 135 x 135

(defun safe-at (g x y)
  (let ((hgt (grid-hgt g))
	(wid (grid-wid g))
	(data (grid-data g))
	(dummy #\$))
    (cond
      ((< x 1) dummy)
      ((> x wid) dummy)
      ((< y 1) dummy)
      ((> y hgt) dummy)
      (t (aref data x y)))))
  

;; how many squares have fewer than four rolls of paper in eight adjacent squares
;;
;; Y +y
;; .
;;/|\
;; | 1 2 3
;; | 4 * 5
;; | 6 7 8
;; |_______\. X +x
;;         /
(defun find-rolls (g)
  (let ((hgt (grid-hgt g))
	(wid (grid-wid g))
	(data (grid-data g))
	(dummy nil)
	(debug nil)
	(tot 0))
    (loop for y from 1 to hgt do
      (loop for x from 1 to wid do
	(let* ((squares (list (safe-at g (+ x -1) (+ y 1)) ;; 1
			      (safe-at g (+ x 0) (+ y 1)) ;; 2
   			      (safe-at g (+ x +1) (+ y 1)) ;; 3
     			      (safe-at g (+ x -1) (+ y 0)) ;; 4 
			      (safe-at g (+ x +1) (+ y 0)) ;; 5
			      (safe-at g (+ x -1) (+ y -1)) ;; 6
			      (safe-at g (+ x 0) (+ y -1)) ;; 7
			      (safe-at g (+ x +1) (+ y -1))))
	       (rolls (remove-if-not (lambda (ch) (equalp ch #\@)) squares))
	       (n-rolls (length rolls)))
	  (when debug
	    (format t "looking at square (~a,~a) ~%" x y)
	    (format t "squares => ~a~%" squares)
	    (format t "rolls => ~a~%" rolls)
	    (format t "n-rolls => ~a~%" n-rolls)
	    )
	  (cond
	    ((and (< n-rolls 4) (roll-p (at g x y)))
	     (incf tot)
	     (when debug (format t "less than 4 && roll at (~a,~a) : ~a ~%" tot x y))
	     )
	    (t
	     (when debug (format t "more than or equal to 4 : - ignored - ~%" tot))
	     (setq dummy t)
	     ))
	  (when debug (format t "~%~%"))
	  )))
    tot))


(defun part-1 ()
  (find-rolls in))


;; find all rolls of paper accessible - then remove them and keep removing them !
(defun remove-rolls (g)  (let ((hgt (grid-hgt g))	(wid (grid-wid g))	(data (grid-data g))	(dummy nil)	(debug nil)(tot-removed 0))
    (catch 'no-more-to-remove			   
    (loop while t do 
      (let ((locations nil)(tot 0))	    
	(loop for y from 1 to hgt do
	  (loop for x from 1 to wid do
	    (let* ((squares (list (safe-at g (+ x -1) (+ y 1)) ;; 1
				  (safe-at g (+ x 0) (+ y 1)) ;; 2
   				  (safe-at g (+ x +1) (+ y 1)) ;; 3
     				  (safe-at g (+ x -1) (+ y 0)) ;; 4 
				  (safe-at g (+ x +1) (+ y 0)) ;; 5
				  (safe-at g (+ x -1) (+ y -1)) ;; 6
				  (safe-at g (+ x 0) (+ y -1)) ;; 7
				  (safe-at g (+ x +1) (+ y -1))))
		   (rolls (remove-if-not (lambda (ch) (equalp ch #\@)) squares))
		   (n-rolls (length rolls)))
	      (when debug
		(format t "looking at square (~a,~a) ~%" x y)
		(format t "squares => ~a~%" squares)
		(format t "rolls => ~a~%" rolls)
		(format t "n-rolls => ~a~%" n-rolls)
		)
	      (cond
		((and (< n-rolls 4) (roll-p (at g x y)))
		 (incf tot)
		 (setq locations (cons (list x y) locations))
		 (when debug (format t "less than 4 && roll at (~a,~a) : ~a ~%" tot x y))
		 )
		(t
		 (when debug (format t "more than or equal to 4 : - ignored - ~%" tot))
		 (setq dummy t)
		 ))
	      (when debug (format t "~%~%"))
	      )))
	(cond
	  ((null locations) (format t "there are no more rolls to remove ~%")
	   (throw 'no-more-to-remove t))
	  (t
	   (format t "there are ~d rolls that can be removed ~%" (length locations))
	   (dolist (pr locations)
	     (destructuring-bind (px py) pr
	       (setf (aref data px py) #\. )
	       (incf tot-removed)
	       )))))))
			   tot-removed))
	   


	    

