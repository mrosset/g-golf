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


(define-module (g-golf gi enum-info)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (g-golf support utils)
  #:use-module (g-golf support enum)
  #:use-module (g-golf init)
  #:use-module (g-golf gi utils)
  #:use-module (g-golf gi base-info)
  #:use-module (g-golf gi registered-type-info)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (g-golf-enum-import
	    g-golf-enum-get-values

	    g-golf-ei-get-n-values
	    g-golf-ei-get-value
	    g-golf-ei-get-n-methods
	    g-golf-ei-get-method

	    g-golf-vi-get-value))


;;;
;;; Build Interface
;;;

(define (g-golf-enum-import info)
  (let* ((type-name (g-golf-rt-get-type-name info))
	 (scm-name (g-golf-gtype-name->scm-name type-name))
	 (e-vals (g-golf-enum-get-values info)))
    (make <gi-enum>
      #:gi-name type-name
      #:scm-name scm-name
      #:enum-set e-vals)))

(define (g-golf-enum-get-values info)
  (letrec ((get-enum-values
	    (lambda (info n i v-set)
	      (if (= i n)
		  (reverse! v-set)
		  (let* ((value-info (g-golf-ei-get-value info i))
			 (name (g-golf-bi-get-name value-info))
			 (value (g-golf-vi-get-value value-info)))
		    (g-golf-bi-unref value-info)
		    (get-enum-values info
				     n
				     (+ i 1)
				     (cons (cons name value)
					   v-set)))))))
    (get-enum-values info
		     (g-golf-ei-get-n-values info)
		     0
		     '())))


;;;
;;; Low level API
;;;

(define (g-golf-ei-get-n-values info)
  (g-enum-info-get-n-values info))

(define (g-golf-ei-get-value info index)
  (let ((pointer (g-enum-info-get-value info index)))
    (if (null-pointer? pointer)
	#f
	pointer)))

(define (g-golf-ei-get-n-methods info)
  (g-enum-info-get-n-methods info))

(define (g-golf-ei-get-method info index)
  (let ((pointer (g-enum-info-get-method info index)))
    (if (null-pointer? pointer)
	#f
	pointer)))

(define (g-golf-vi-get-value info)
  (g-value-info-get-value info))


;;;
;;; GI Bindings
;;;

(define g-enum-info-get-n-values
  (pointer->procedure int
                      (dynamic-func "g_enum_info_get_n_values"
				    %libgirepository)
                      (list '*)))

(define g-enum-info-get-value
  (pointer->procedure '*
                      (dynamic-func "g_enum_info_get_value"
				    %libgirepository)
                      (list '* int)))

(define g-enum-info-get-n-methods
  (pointer->procedure int
                      (dynamic-func "g_enum_info_get_n_methods"
				    %libgirepository)
                      (list '*)))

(define g-enum-info-get-method
  (pointer->procedure '*
                      (dynamic-func "g_enum_info_get_method"
				    %libgirepository)
                      (list '* int)))

(define g-value-info-get-value
  (pointer->procedure int64
                      (dynamic-func "g_value_info_get_value"
				    %libgirepository)
                      (list '*)))
