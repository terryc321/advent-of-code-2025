

(defpackage :fun
  (:use #:cl)
  (:use #:uiop)
  (:local-nicknames (#:alex #:alexandria)))

(in-package :fun)

(defun read-file (filename)
  (coerce 
   (with-open-file (stream filename :direction :input)
     (read stream))
   'vector))

(defparameter in (read-file "../input.txt"))
(defparameter ex (read-file "../example.txt"))

;; speed up lookup using a hash table for inputs maybe

;; find all paths leading from you to out
(defun find-you (xs)
  (let ((hash (make-hash-table :test #'eq))
	(start nil))
    ;; finish does not have an index - name only
    (loop for i from 0 to (+ -1 (length xs)) do
      (let* ((signpost (aref xs i))
	     (key (car signpost)))
	(setf (gethash key hash) i)))
    ;; you at index 
    (setq start (gethash 'you hash nil))
    (when (not start) (error "cannot find YOU"))
    (values start hash)))

	

;; breadth first search all places can reach from current location
;; keep track of places we have been
;; if we go over own tracks stop ?
;; depth first search ?

(defun recur (xs)
  (multiple-value-bind (start hash) (find-you xs)
    (assert (integerp start))
    (let ((npath 0))
      (labels ((recur2 (i hist)
		 (assert (integerp i))	      
		 ;; get all places on signpost from i
		 (let ((places (cdr (aref xs i))))
		   (dolist (name places)
		     (assert (symbolp name))
		     (cond
		       ((eq name 'out) ;; found exit
			(incf npath))
		       (t ;; not the exit
			(let ((index (gethash name hash nil)))
			  (when (not (integerp index))
			    (error "place not known"))
			  (when (not (member index hist))
			    (recur2 index (cons index hist))))))))))
	(recur2 start nil))
      (format t "there are ~a paths ~%" npath))))


(defun run-example ()  (recur ex))

(defun part-1 () (recur in))

;; FUN> (part-1)
;; there are 764 paths 
;; NIL

