;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2019
;;;; Free Software Foundation, Inc.

;;;; This file is part of GNU G-Golf

;;;; GNU G-Golf is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU Lesser General Public License as
;;;; published by the Free Software Foundation; either version 3 of the
;;;; License, or (at your option) any later version.

;;;; GNU G-Golf is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.

;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with GNU G-Golf.  If not, see
;;;; <https://www.gnu.org/licenses/lgpl.html>.
;;;;

;;; Commentary:

;;; Code:


(define-module (g-golf support flag)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-60)
  #:use-module (oop goops)
  #:use-module (g-golf support goops)
  #:use-module (g-golf support g-export)
  #:use-module (g-golf support utils)
  #:use-module (g-golf support keyword)
  #:use-module (g-golf support enum)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (<gi-flag>

            gi-gflags->integer
            gi-integer->gflags))


#;(g-export )


;;;
;;; GI Flag
;;;

(define-class <gi-flag> (<gi-enum>)
  )

#;(define-method (initialize (self <gi-enum>) initargs)
  (next-method)
  (let ((gi-name (get-keyword #:gi-name initargs #f)))
    (and gi-name
         (set! (!gi-name self) gi-name)
         (set! (!scm-name self)
               (g-name->scm-name gi-name)))))

(define (gi-gflags->integer gflags flags)
  (list->integer
   (reverse (map (lambda (name)
		   (if (member name flags) #t #f))
	      (enum->symbols gflags)))))

(define (gi-integer->gflags gflags n)
  (let ((symbols (enum->symbols gflags)))
    (fold-right (lambda (symbol bool result)
		  (if bool
		      (cons symbol result)
		      result))
		'()
		symbols
		(reverse (integer->list n (length symbols))))))
