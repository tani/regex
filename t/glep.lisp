#!/bin/sh
#|-*- mode:lisp coding:utf-8 -*-|#
#|
exec env cl -- "$0" "$@"
|#
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

(setf (sb-ext:bytes-consed-between-gcs) (* 100 1024 1024))
(ql:quickload :regex :silent t)
(ql:quickload :cl-ppcre :silent t)

(defun glep (re file)
  (let ((ls (uiop:read-file-lines file))
	(compiled (re:compile-regex re)))
    (count-if  (lambda (s) (re:scan compiled s)) ls)))

(defun glep-interp (re file)
  (let ((ls (uiop:read-file-lines file)))
    (count-if  (lambda (s) (re:scan re s)) ls)))

(defun ppcre-glep (re file)
  (let ((ls (uiop:read-file-lines file)))
    (count-if  (lambda (s) (ppcre:scan re s)) ls)))

(sb-ext:gc)

(format t "~%|======== ppcre-glep ========|~%")
(time
 (format t "~a hits~%"
	 (loop :for f :in (nthcdr 5 sb-ext:*posix-argv*)
	       :sum (ppcre-glep (fifth sb-ext:*posix-argv*) f))))

(format t "~%|======== glep-interp ========|~%")
(time
 (format t "~a hits~%"
	 (loop :for f :in (nthcdr 5 sb-ext:*posix-argv*)
	       :sum (glep (fifth sb-ext:*posix-argv*) f))))

(format t "~%|======== glep ========|~%")
(time
 (format t "~a hits~%"
	 (loop :for f :in (nthcdr 5 sb-ext:*posix-argv*)
	       :sum (glep (fifth sb-ext:*posix-argv*) f))))

