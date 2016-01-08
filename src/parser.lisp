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
(defpackage regex.parser
  (:use :cl :regex.core)
  (:export :parse))
(in-package regex.parser)

(defun syntex-check (str)
  (loop :with p1 := 0 :with p2 := 0 :with p3 := 0
	:with s := (make-string-input-stream str)
	:for  c := (read-char s nil nil) :while c :do
	  (cond ((char= c #\() (incf p1))
		((char= c #\)) (decf p1))
		((char= c #\[) (incf p2))
		((char= c #\]) (decf p2))
		((char= c #\{) (incf p3))
		((char= c #\}) (decf p3))
		((char= c #\\) (read-char s nil nil)))
	:finally
	   (unless (and (zerop p1) (zerop p2) (zerop p3))
	     (error "Invalid regular expression"))))

(defun expand-a-z-1 (str)
  (let ((str-1 "") (re (list (re/group (list (re/dot)))
			     (re/character #\-)
			     (re/group (list (re/dot)))
			     (re/ok))))
    (unless (match re str) (return-from expand-a-z-1 str))
    (multiple-value-bind (bool start end ls) (match re str)
      (declare (ignore bool))
      (loop :for c :from (char-int (char (first ls) 0))
	           :to   (char-int (char (second ls) 0))
	    :collecting (code-char c) :into s
	    :finally (setf str-1 (coerce s 'string)))
      (concatenate 'string (subseq str 0 start) str-1 (subseq str end)))))

(defun expand-a-z (str)
  (if (not (member #\- (coerce str 'list))) str
      (loop :repeat (count #\- str)
	    :for s := (expand-a-z-1 str) :then (expand-a-z-1 s)
	    :finally (return s))))

(defun expand-class (str)
  (let ((re (list (re/character #\[)
		  (re/character #\:)
		  (re/group
		   (list
		    (re/plus
		     (re/brackets
		      (coerce (expand-a-z "a-z") 'list)))))
		  (re/character #\:)
		  (re/character #\])
		  (re/ok))))
    (multiple-value-bind (bool start end ls) (match re str)
      (unless bool (return-from expand-class str))
      (concatenate
       'string
       (subseq str 0 start)
       (cond ((string= "upper" (first ls))(expand-a-z "A-Z"))
	     ((string= "lower" (first ls))(expand-a-z "a-z"))
	     ((string= "alpha" (first ls))(expand-a-z "a-zA-Z"))
	     ((string= "alnum" (first ls))(expand-a-z "0-9a-zA-Z"))
	     ((string= "digit" (first ls))(expand-a-z "0-9"))
	     ((string= "xdigit"(first ls))(expand-a-z "0-9A-Fa-f"))
	     ((string= "punct" (first ls))
	      "\\]\\[!\"#$%&'()*+,./:;<=>?@\\^_`{|}~-")
	     ((string= "blank" (first ls)) (coerce '(#\space #\tab) 'string)))
       (subseq str end)))))

(defun parse-brackets (s)
  (loop :with cnt     := 1
	:with inverse := nil
	:for  i       :from 0
	:for  c       := (read-char s nil nil)
	:until (and (= cnt 1) (char= c #\]))
	:if (and (char= c #\^) (zerop i))
	  :do (setf inverse t)
	:if (char= c #\\)
	  :collecting (read-char s nil nil) :into ret
	:else :if (char= c #\[)
	  :collecting c :into ret :and :do (incf cnt)
	:else :if (char= c #\])
	  :collecting c :into ret :and :do (incf cnt)
	:else
	  :collecting c :into ret
	:finally
	   (if inverse
	       (return (re/brackets-not (coerce (expand-a-z (expand-class (coerce ret 'string))) 'list)))
	       (return (re/brackets (coerce (expand-a-z (expand-class (coerce ret 'string))) 'list))))))

(defun parse-brace (s)
  (let ((re (list (re/group (list (re/plus (re/brackets (coerce "0123456789" 'list)))))
		  (re/character #\,)
		  (re/group (list (re/star (re/brackets (coerce "0123456789" 'list)))))
		  (re/ok)))
	(str (loop :for c := (read-char s nil nil) :until (char= c #\})
		   :collecting c :into u
		   :finally (return (coerce u 'string)))))
    (multiple-value-bind (bool start end ls) (match re str)
      (declare (ignore start end))
      (if bool
	  (if (string= "" (second ls))
	      (lambda (r) (re/repeat (read-from-string (first ls)) most-positive-fixnum r))
	      (lambda (r) (re/repeat (read-from-string (first ls)) (read-from-string (second ls)) r)))
	  (lambda (r) (re/repeat (read-from-string str) (read-from-string str) r))))))

(defun parse-1 (s)
  (labels ((parse-parenthesis (s-1)
	     (loop :with cnt := 1
		   :for  c   := (read-char s-1 nil nil)
		   :while (not (and (= cnt 1) (char= c #\))))
		   :if (char= c #\\)
		     :collecting (read-char s nil nil) :into ret
		   :else :if (char= c #\()
		     :collecting c :into ret :and :do (incf cnt)
		   :else :if (char= c #\))
		     :collecting c :into ret :and :do (incf cnt)
		   :else
		     :collecting c :into ret
		   :finally (return (re/group
				     (parse-1
				      (make-string-input-stream
				       (coerce ret 'string))))))))
    (loop :with ast := nil
	  :for  c   := (read-char s nil nil) :while c :do
	    (cond ((char= c #\[) (push (parse-brackets s) ast))
		  ((char= c #\{) (push (funcall (parse-brace s) (pop ast)) ast))
		  ((char= c #\() (push (parse-parenthesis s) ast))
		  ((char= c #\.) (push (re/dot) ast))
		  ((char= c #\*) (push (re/star (pop ast)) ast))
		  ((char= c #\+) (push (re/plus (pop ast)) ast))
		  ((char= c #\?) (push (re/question (pop ast)) ast))
		  ((char= c #\$) (push (re/tail) ast))
		  ((char= c #\^) (push (re/head) ast))
		  ((char= c #\\)
		   (let ((e (read-char s nil nil)))
		     (if (member e (coerce "0123456789" 'list))
			 (push (re/ref (- (char-int e) (char-int #\0))) ast)
			 (push (re/character e) ast))))
		  ((char= c #\|) (setf ast (list (re/or ast (parse-1 s)))))
		  (t (push (re/character c) ast)))
	  :finally (return (reverse ast)))))

(defun parse (str)
  (syntex-check str)
  (append (parse-1 (make-string-input-stream str)) (list (re/ok))))
