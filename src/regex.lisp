#|
Copyright (c) 2015, asciian.

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
(defpackage regex
  (:use :cl)
  (:nicknames :re)
  (:import-from :regex.core :match)
  (:import-from :regex.parser :parse)
  (:export :scan :regex-replace :compile-regex))
(in-package :regex)

(defstruct (regex
	    (:constructor compile-regex
		(string &aux (re (parse string)))))
  string re)

(defun scan (regex target)
  (if (regex-p regex)
      (match (regex-re regex) target)
      (match (parse regex) target)))

(defun regex-replace (regex target replacement &key global)
  (multiple-value-bind (bool start end ls) (scan regex target)
    (unless bool (return-from regex-replace target))
    (concatenate 'string
      (subseq target 0 start)
      (if ls
	  (loop
	    :repeat (length ls) :for i :from 1
	    :for ret := (regex-replace "\\\\1" replacement (nth 0 ls) :global t)
	      :then (regex-replace (format nil "\\\\~d" i) ret (nth (1- i) ls) :global t)
	    :finally (return ret))
	  replacement)
      (if global
	  (regex-replace regex (subseq target end) replacement :global t)
	  (subseq target end)))))
