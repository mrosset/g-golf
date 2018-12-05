;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2016 - 2018
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
  #:use-module (g-golf support)
  #:use-module (g-golf glib)
  #:use-module (g-golf gobject)

  #:export (%gi-pointer-size
	    gi-pointer-new
	    gi-pointer-inc
	    gi-attribute-iter-new
	    with-gerror
	    gi->scm
            gi-boolean->scm
            gi-string->scm
            gi-strings->scm
            gi-cvs-string->scm
	    gi-pointer>scm
	    gstudly-caps-expand
	    %gtype-name-exceptions
	    gtype-name->scm-name
	    gtype-name->class-name
	    ;; gtype-class-name->method-name
	    g-golf-gflags->integer
	    g-golf-integer->gflags))


(define %gi-pointer-size 8)
  
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
;;; Name Transformation
;;;

;; Based on Guile-Gnome (gobject gw utils)

(define (gstudly-caps-expand nstr)
  ;; GStudlyCapsExpand
  (do ((idx (- (string-length nstr) 1)
	    (- idx 1)))
      ((> 1 idx)
       (string-downcase nstr))
    (cond ((and (> idx 2)
                (char-lower-case? (string-ref nstr (- idx 3)))
                (char-upper-case? (string-ref nstr (- idx 2)))
                (char-upper-case? (string-ref nstr (- idx 1)))
                (char-lower-case? (string-ref nstr idx)))
           (set! idx (- idx 1))
           (set! nstr
                 (string-append (substring nstr 0 (- idx 1))
                                "-"
                                (substring nstr (- idx 1)
                                           (string-length nstr)))))
          ((and (> idx 1)
                (char-upper-case? (string-ref nstr (- idx 1)))
                (char-lower-case? (string-ref nstr idx)))
           (set! nstr
                 (string-append (substring nstr 0 (- idx 1))
                                "-"
                                (substring nstr (- idx 1)
                                           (string-length nstr)))))
          ((and (char-lower-case? (string-ref nstr (- idx 1)))
                (char-upper-case? (string-ref nstr idx)))
           (set! nstr
                 (string-append (substring nstr 0 idx)
                                "-"
                                (substring nstr idx
                                           (string-length nstr))))))))

;; Default name transformations can be overridden, but g-golf won't
;; define exceptions for now, let's see.
(define %gtype-name-exceptions
  '(;; (GEnum . genum)
    ))

(define (gtype-name->scm-name type-name)
  (or (assoc-ref %gtype-name-exceptions type-name)
      (string-trim-right (gstudly-caps-expand
			  ;; only change _ to -
			  ;; other chars are not valid in a type name
			  (string-map (lambda (c) (if (eq? c #\_) #\- c))
				      type-name))
			 #\-)))

;; "GtkAccelGroup" => <gtk-accel-group>
;; "GSource*" => <g-source*>
(define (gtype-name->class-name type-name)
  (string->symbol (string-append "<"
				 (gstudly-caps-expand type-name)
				 ">")))

;; Not sure this is used but let's keep it as well
#;(define (gtype-class-name->method-name class-name name)
  (let ((class-string (symbol->string class-name)))
    (string->symbol
     (string-append (substring class-string 1 (1- (string-length class-string)))
                    ":" (symbol->string name)))))

(define (g-golf-gflags->integer gflags flags)
  (list->integer
   (reverse (map (lambda (name)
		   (if (member name flags) #t #f))
	      (enum->symbols gflags)))))

(define (g-golf-integer->gflags gflags n)
  (let ((symbols (enum->symbols gflags)))
    (fold-right (lambda (symbol bool result)
		  (if bool
		      (cons symbol result)
		      result))
		'()
		symbols
		(reverse (integer->list n (length symbols))))))
