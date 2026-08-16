
(ql:quickload :uiop)
(ql:quickload :alexandria)
(ql:quickload :str)




;; make grid from input text
;; option 1,1 top left or 0,0 

(defpackage :fun
  (:use :cl))
(in-package :fun)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

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
;; (defparameter ranges (multiple-value-bind (rngs nums)
;; 			 (process (input))
;; 		       (sort rngs (lambda (a b) (< (first a) (first b))))))


;; (defun process2 (xs)
;;   (multiple-value-bind (ranges nums) (process xs)
;;     (let ((spoiled nil)
;; 	  (fresh nil))
;;       (dolist (r ranges)
;; 	(destructuring-bind (lo hi) r
;; 	  (format t "lo ~a : hi ~a~%" lo hi))))))


;;wrinkle in the armour is the lo - hi ranges are extremely large to iterate over them in 
;;a one by one nature .
;; two ranges can be distinct
;; rather a range can be distinct from current known ranges - in which case that range is
;; included
;; otherwise consider how they may overlap
;; r1 and r2 overlap like this 
;; r1 ...r1
;;     r2.... r2
;;
;; r1 and r2 overlap like this 
;;         r1 .. r1 
;; r2 .......r2
;;
;; r1 completely subsumes r2 
;; r1 ...................r1
;;       r2........r2
;;
;; r2 completely subsumes r1 
;; r2...................r2
;;         r1.....r1
;;
;; (defun fix-ranges (r1lo r1hi r2lo r2hi)
;;   (assert (< r1lo r1hi))
;;   (assert (< r2lo r2hi))
;;   (cond
;;     ((and (< r1lo r2lo)(< r1hi r2lo))
;;      ;; distinct ranges r1...r1  r2.......r2
;;      (list (list r1lo r1hi) (r2lo r2hi)))
;;     ((and (> r1lo r2hi)(> r1hi r2hi))
;;      ;; distinct ranges  r2.......r2   r1 ... r1
;;      (list (list r1lo r1hi) (r2lo r2hi)))
;;     ((and (< r1lo r2lo)(> r1hi r2lo))

;; we can create a range with a low and high component,
;; we can also check for equality 
;; should we give each range an identifier ? 

(defstruct range lo hi id)
(equalp (make-range :lo 3 :hi 4 :id 1)
	(make-range :hi 4 :lo 3 :id 2))

(defparameter ranges (multiple-value-bind (rngs nums)
			 (process (input))
		       (let ((unsorted (mapcar (let ((id -1))
				 (lambda (ab)
				   (incf id)
				   (destructuring-bind (lo hi) ab
				     (make-range :lo lo :hi hi :id id))))
			       rngs)))
		      (sort unsorted (lambda (a b) (< (range-lo a) (range-lo b)))))))

;; ranges
;; lo hi id
;; id allows them to be removed from a list cleanly , we can simplify refer to component
;; save a list of id's that need to be removed or added cleanly

;; integration
(defparameter known (first ranges))
(setq ranges (cdr ranges))

;; check for consistency with ranges 
(defun consistent (rngs)
  (catch 'counterexample
    (dolist (r rngs)
      (dolist (q rngs)
	(when (not (= (range-id r) (range-id q)))
	  ;; so r and q are different ranges
	  (let ((ok (or (and (< (range-lo r) (range-lo q))
			     (< (range-hi r) (range-lo q)))
			(and (> (range-lo r) (range-hi q))
			     (> (range-hi r) (range-hi q))))))
	    (when (not ok)
	      (throw 'counterexample nil))))))
    t))

		    
#|

clearly its a tricky puzzle

definition : a range
range is defined as having low and high markers , all integers between and inclusive of
end points of the markers - define the range

lets say we have some distinct ranges , no overlap between any ranges 
  r1.... r1  r2.... r2   r3... r3

now introduce an arbitrary range r4 ... r4
...  r1.... r1   r2.... r2   r3... r3
a   b   c  d  e f  g   h   i j k  l  m

for n ranges there are multiple places r4 can interlude itself


1st  #S(RANGE :LO 13466688954434 :HI 16534508927096 :ID 66)
2nd  #S(RANGE :LO 16534508927097 :HI 16534508927097 :ID 21)

|#

(defun getid(id)
  (catch 'found
    (dolist (r ranges)
      (cond
	((= (range-id r) id) (throw 'found r))))
    (error "id not found")))

(defparameter solution (list (car ranges)))
;; (setq ranges (cdr ranges))
;; try to keep ranges sorted ?




;; can we combine k and r ??
(defun comb(kid rid)
  (let ((k (getid kid))
	(r (getid rid)))
    (assert (eq (type-of k) 'range))
    (assert (eq (type-of r) 'range))
    (cond
      ((and (< (range-lo k) (range-lo r))
	    (< (range-hi k) (range-lo r)))
       ;; k is below r -- no interference
       (format t "k is below r -- no interference~%")
       'none)
      ((and (> (range-lo k) (range-lo r))
	    (> (range-hi k) (range-lo r)))
       ;; k is above r -- no interference
       (format t "k is above r -- no interference~%")
       'none)
      ((and (> (range-lo k) (range-lo r))
	    (< (range-hi k) (range-hi r)))
       ;; k is inside r entirely -- so r subsumes k  , we can remove k
       (format t "r subsumes k -- k is redundant ~%")
       'subsumes)
      ((and (> (range-lo r) (range-lo k))
	    (< (range-hi r) (range-hi k)))
       ;; r is inside k entirely -- so r subsumes k  , we can remove k
       (format t "r subsumes k -- k is redundant ~%")
       'subsumed)
      (t 'unknown))))

      
  








;; ;; integrate each range in todo into known good ranges
;; ;; pick one off - integrate it , return a set of known good ranges
;; ;; 
;; (defun integrate(known todo)
;;   (cond
;;     ((null todo) known)
;;     (t (integrate (integrate2 known (car todo)) (cdr todo)))))


;;(defun inside-range(i r)

;; does r stretch over several ranges of known ?
;; if so those ranges are obsolete
;; then what ?
(defun integrate2(known r)
  (cond
    ((null known) (list r))
    (t  ;; known is non empty
	(format t "known is not empty ~%")
	(let ((result nil)
	      (grablo nil)
	      (grabhi nil))
	  (dolist (k known)
	    ;; check by assert that ranges do not conflict on edge cases bounds
	    ;; (assert (not (= (range-lo k) (range-lo r))))
	    ;; (assert (not (= (range-hi k) (range-hi r))))
	    ;; (assert (not (= (range-lo k) (range-hi r))))
	    ;; (assert (not (= (range-hi k) (range-lo r))))
	    ;; ok so now what ?
	    ;; rlo is inside k somewhere
	    ;; rhi is inside k somewhere
	    (let ((lowr (and (>= (range-lo r) (range-lo k))
			     (<= (range-lo r) (range-hi k))))
		  (highr (and (>= (range-hi r) (range-lo k))
			      (<= (range-hi r) (range-hi k)))))	 
	      (cond
		((and lowr highr)
		 ;; r is completely subsumed inside already existing range
		 ;; so no change necessary
		 ;; indeed no other range in known can/should be able to grab this range r
		 (setq grablo t)
		 (setq grabhi t)
		 (setq result (cons k result)))
		(lowr
		 ;; rlo is inside k
		 (format t "rlo inside k~%")
		 )
		(highr
		 ;; rhi is inside k 
		 (format t "rhi inside k~%")
		 )
		(t
		 ;; no interference
		 (format t "no interference~%")
		 (setq result (cons k result))))))
	  ;; if 
	  (cond
	    ((and grablo grabhi) nil)
	    ((or grablo grabhi) (error "grab was not finished"))
	    (t
	     ;; no grab - no range contained lo or hi of r 
	     (setq result (cons r result))))
	  result))))


(defun part2 ()
  (let ((result (list (car ranges)))
	(todos (cdr ranges)))
    (dolist (todo todos)
      ;; (terpri)
      ;; (terpri)
      (format t "before ~%~a~%" result)
      (format t "integrate2 ~a~%" todo)
      (setq result (integrate2 result todo))
      (format t "result ~%~a~%" result))
    result))




	     



;; ;; will process ranges more than once ?
;; (defun collisions ()
;;   (let ((rs ranges)
;; 	(id 0))
;;     (loop while (not (null rs)) do
;;       (let ((r (car rs))
;; 	    (qs (cdr rs)))
;; 	(dolist (q qs)
;; 	  (destructuring-bind (rlo rhi) r
;; 	    (destructuring-bind (rlo2 rhi2) q
;; 	      (cond
;; 		((and (< rlo rlo2) (< rhi rlo2)) nil)
;; 		((and (> rlo rhi2) (> rhi rhi2)) nil)	
		
;; 		(t
;; 		 (cond
;; 		   ((and (< rlo rlo2) (> rhi rhi2)) (format t "r subsumes q~%"))
;; 		   ((and (< rlo2 rlo) (> rhi2 rhi)) (format t "q subsumes r~%"))
;; 		   (t
;; 		    (incf id)
;; 		    (format t "collision ~a : ~a ~a ... ~a ~a ~%" id rlo rhi rlo2 rhi2)
;; 		    nil))
;; 		 )))))
;;       (setq rs (cdr rs))))))
	      
	  
    
      
    
