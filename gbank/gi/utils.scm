;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2016
;;;; Free Software Foundation, Inc.

;;;; This file is part of GNU Gbank

;;;; GNU Gbank is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published
;;;; by the Free Software Foundation, either version 3 of the License,
;;;; or (at your option) any later version.

;;;; GNU Gbank is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; General Public License for more details.

;;;; You should have received a copy of the GNU General Public License
;;;; along with GNU Gbank.  If not, see <http://www.gnu.org/licenses/>.
;;;;

;;; Commentary:

;;; Code:


(define-module (gbank gi utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)

  #:export (%gpointer-size
	    gbank-pointer-new
	    gbank-pointer-inc
	    with-gerror
	    gchar**->scm
	    gchar*,->scm
	    gboolean->scm
	    gbank-gtype->scm
	    gbank-attribute-iter-new))


(define %gpointer-size 8)
  
(define (gbank-pointer-new)
  (bytevector->pointer (make-bytevector %gpointer-size 0)))

(define* (gbank-pointer-inc pointer #:optional (offset %gpointer-size))
  (make-pointer (+ (pointer-address pointer)
		   offset)))

(define-syntax with-gerror
  (syntax-rules ()
    ((with-gerror ?var ?body)
     (let* ((?var (gbank-pointer-new))
	    (result ?body))
       (receive (d-pointer)
	   (dereference-pointer ?var)
	 (if (null-pointer? d-pointer)
	     result
	     (match (parse-c-struct d-pointer
				    (list uint32 int8 '*))
	       ((domain code message)
		(error (pointer->string message))))))))))

(define (gchar**->scm pointer)
  (define (gchar**->scm-1 pointer result)
    (receive (d-pointer)
	(dereference-pointer pointer)
    (if (null-pointer? d-pointer)
	(reverse! result)
	(gchar**->scm-1 (gbank-pointer-inc pointer)
			(cons (pointer->string d-pointer)
			      result)))))
  (gchar**->scm-1 pointer '()))

(define (gchar*,->scm pointer)
  (string-split (pointer->string pointer)
		#\,))

(define (gboolean->scm value)
  (if (= value 0) #f #t))

(define (gbank-gtype->scm gvalue gtype)
  (case gtype
    ((gchar**) (gchar**->scm gvalue))
    ((gchar*,) (gchar*,->scm gvalue))
    ((gchar*) (pointer->string gvalue))
    ((gboolean) (gboolean->scm gvalue))
    (else
     (error "no such gtype: " gtype))))

(define (gbank-attribute-iter-new)
  (make-c-struct (list '* '* '* '*)
		 (list %null-pointer
		       %null-pointer
		       %null-pointer
		       %null-pointer)))
