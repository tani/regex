#|
Copyright (c) 2015, TANIGUCHI Masaya.

This program is free software; you can redistribute it and/or modify
it under the terms of the Lisp Lesser General Public License version 2, as published by
the Free Software Foundation and with the following preamble:
http://opensource.franz.com/preamble.html

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Lisp Lesser General Public License for more details.
|#

(in-package :cl-user)
(defpackage regex.core
  (:use :cl)
  (:import-from :cl-cont :with-call/cc :let/cc)
  (:export
   :re/character
   :re/dot
   :re/question
   :re/star
   :re/group
   :re/head
   :re/tail
   :re/plus
   :re/brackets
   :re/brackets-not
   :re/ok
   :re/repeat
   :re/ref
   :re/or
   :match))
(in-package :regex.core)

(defstruct data
  (register nil :type list)
  (index    0   :type fixnum)
  (start    0   :type fixnum :read-only t)
  (string   ""  :type string :read-only t))

(declaim (inline sar))
(defun sar (data)
  (declare (type data data))
  (when (< (data-index data) (length (data-string data)))
    (let ((c (char (data-string data) (data-index data))))
      (unless (char= #\Newline c) c))))

(declaim (inline sdr))
(defun sdr (data)
  (declare (type data data))
  (let ((d (copy-data data)))
    (incf (data-index d)) d))

(defmacro spush (val data)
  `(setf (data-register ,data)
	 (cons ,val (data-register ,data))))

(defmacro defmatcher (name args &body body)
  `(defun ,name ,args
     (lambda (data reg cont)
       (declare (ignorable data reg cont)
		(type data data)
		(type list reg)
		(type function cont))
       ,@body)))

(defmatcher re/character (c)
  (if (eql c (sar data))
      (funcall (car reg) (sdr data) (cdr reg) cont)
      (funcall cont)))

(defmatcher re/dot ()
  (if (sar data)
      (funcall (car reg) (sdr data) (cdr reg) cont)
      (funcall cont)))

(defmatcher re/repeat (n m r)
  (cond ((zerop m) (funcall (car reg) data (cdr reg) cont))
	((zerop n)
	 (with-call/cc
	   (let/cc c (funcall r data (cons (re/repeat n (1- m) r) reg) c))
	   (funcall (car reg) data (cdr reg) cont)))
	(t (funcall r data (cons (re/repeat (1- n) (1- m) r) reg) cont))))

(defun re/question (r)
  (re/repeat 0 1 r))

(defun re/star (r)
  (re/repeat 0 most-positive-fixnum r))

(defun re/plus (r)
  (re/repeat 1 most-positive-fixnum r))

(defmatcher re/or (r1 r2)
  (with-call/cc
    (let/cc c (funcall (car r1) data (append (cdr r1) reg) c))
    (funcall (car r2) data (append (cdr r2) reg) cont)))

(defmatcher re/head ()
  (if (= (data-start data) (data-index data) 0)
      (funcall (car reg) data (cdr reg) cont)
      (funcall cont)))

(defmatcher re/tail ()
  (unless (< (data-index data) (length (data-string data)))
    (throw 'match
      (values t
	      (data-start data)
	      (data-index data)
	      (reverse (data-register data)))))
  (funcall cont))

(defmatcher re/ref (n)
  (let* ((l (coerce (nth (1- n) (reverse (data-register data))) 'list))
	 (r (mapcar #'re/character l)))
    (funcall (car r) data (append (cdr r) reg) cont)))

(defmatcher re/group (r)
  (let ((start (data-index data)))
    (labels ((re/group-1 (r-1)
	       (lambda (d e c)
		 (unless r-1
		   (spush (subseq (data-string d) start (data-index d)) d)
		   (funcall (car e) d (cdr e) c))
		 (funcall (car r-1) d (cons (re/group-1 (cdr r-1)) e) c))))
      (funcall (re/group-1 r) data reg cont))))

(defmatcher re/brackets (r)
  (when (member (sar data) r)
    (funcall (car reg) (sdr data) (cdr reg) cont))
  (funcall cont))

(defmatcher re/brackets-not (r)
  (unless (or (not (sar data)) (member (sar data) r))
    (funcall (car reg) (sdr data) (cdr reg) cont))
  (funcall cont))
  
(defmatcher re/ok ()
  (throw 'match
    (values t
	    (data-start data)
	    (data-index data)
	    (reverse (data-register data)))))

(defun match (reg str &optional (start 0) (end most-positive-fixnum))
  (catch 'match
    (loop :for i :from start :to (min end (length str))
	  :for d := (make-data :string str :start i :index i) :do
       (with-call/cc (let/cc cont (funcall (car reg) d (cdr reg) cont))))))
