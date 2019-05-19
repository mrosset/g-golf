;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2016 - 2019
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


(define-module (g-golf gi utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-60)
  #:use-module (g-golf support enum)
  #:use-module (g-golf glib mem-alloc)

  #:export (%gi-pointer-size
	    gi-pointer-new
	    gi-pointer-inc
	    gi-attribute-iter-new
	    with-gerror
	    gi->scm
            scm->gi
            gi-boolean->scm
            gi-string->scm
            gi-strings->scm
            gi-cvs-string->scm
	    gi-pointer->scm
	    gi-integer->gflags
	    gi-gflags->integer))


(define %gi-pointer-size (sizeof '*))
  
(define (gi-pointer-new)
  ;; (bytevector->pointer (make-bytevector %gi-pointer-size 0))
  ;; The above would work iif none of Glib, Gobject and GI would ever call
  ;; any of there respective *_free functions upon pointers returned by
  ;; this procedure [it segfaults - C can't free Guile's mem]. This
  ;; statement is _not_ guaranteed, hence we have to allocate using the
  ;; glib API.
  (g-malloc0 %gi-pointer-size))

(define* (gi-pointer-inc pointer
                         #:optional
                         (offset %gi-pointer-size))
  (make-pointer (+ (pointer-address pointer)
		   offset)))

(define (gi-attribute-iter-new)
  (make-c-struct (list '* '* '* '*)
		 (list %null-pointer
		       %null-pointer
		       %null-pointer
		       %null-pointer)))

(define-syntax with-gerror
  (syntax-rules ()
    ((with-gerror ?var ?body)
     (let* ((?var (gi-pointer-new))
	    (result ?body)
	    (d-pointer (dereference-pointer ?var)))
       (if (null-pointer? d-pointer)
	   (begin
	     (g-free ?var)
	     result)
	   (match (parse-c-struct d-pointer
				  (list uint32 int8 '*))
	     ((domain code message)
	      (g-free ?var)
	      (error (pointer->string message)))))))))


;;;
;;; gi->scm procedures
;;;

(define (gi->scm value type)
  (case type
    ((boolean) (gi-boolean->scm value))
    ((string) (gi-string->scm value))
    ((strings) (gi-strings->scm value))
    ((csv-string) (gi-cvs-string->scm value))
    ((pointer) (gi-pointer->scm value))
    (else
     (error "No such type: " type))))

(define (gi-boolean->scm value)
  (if (= value 0) #f #t))

(define (gi-string->scm pointer)
  (if (null-pointer? pointer)
      #f
      (pointer->string pointer)))

(define (gi-strings->scm pointer)
  (define (gi-strings->scm-1 pointer result)
    (receive (d-pointer)
	(dereference-pointer pointer)
      (if (null-pointer? d-pointer)
          (reverse! result)
          (gi-strings->scm-1 (gi-pointer-inc pointer)
                             (cons (pointer->string d-pointer)
                                   result)))))
  (gi-strings->scm-1 pointer '()))

(define (gi-csv-string->scm pointer)
  (if (null-pointer? pointer)
       #f
       (string-split (pointer->string pointer)
                     #\,)))

(define (gi-pointer->scm pointer)
  (if (null-pointer? pointer)
      #f
      pointer))


;;;
;;; gi->scm procedures
;;;

(define (scm->gi value type)
  (case type
    ((boolean) (scm->gi-boolean value))
    ((string) (scm->gi-string value))
    #;((strings) (scm->gi-strings value))
    #;((csv-string) (scm->gi-cvs-string value))
    ((pointer) (scm->gi-pointer value))
    (else
     value)))

(define (scm->gi-boolean value)
  (if value 1 0))

(define (scm->gi-string value)
  (string->pointer value))

(define (scm->gi-pointer value)
  (if value
      value
      %null-pointer))

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
