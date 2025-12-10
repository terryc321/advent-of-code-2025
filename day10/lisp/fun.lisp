
(ql:quickload :parseq)
;; (ql:quickload :fiveam)
;; (ql:quickload :uiop)
;; (ql:quickload :alexandria)
;; (ql:quickload :str)

(defpackage :fun
  (:use :cl :parseq))
(in-package :fun)

(declaim (optimize (speed 0)(debug 3)(space 0)))

(defun input (filename)
  (with-open-file (stream filename :direction :input)
    (read stream)))

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

(defrule open-bracket () #\[)
(defrule close-bracket () #\])
(defrule foobar () (and open-bracket close-bracket))
(parseq 'foobar "[]")



