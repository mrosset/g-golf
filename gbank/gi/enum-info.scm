;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2016
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


(define-module (gbank gi enum-info)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (gbank support utils)
  #:use-module (gbank gi gobject enum-flags)
  #:use-module (gbank gi init)
  #:use-module (gbank gi utils)
  #:use-module (gbank gi base-info)
  #:use-module (gbank gi registered-type-info)

  #:export (gbank-enum-import

	    gbank-ei-get-n-values
	    gbank-ei-get-value
	    gbank-ei-get-n-methods
	    gbank-ei-get-method

	    gbank-vi-get-value))


;;;
;;; Build Interface
;;;

#;(define (gbank-enum-import info)
  (let* ((type-name (gbank-rt-get-type-name info))
	 (class-name (gbank-gtype-name->class-name type-name))
	 (e-vals (gbank-enum-get-values info)))
    (dimfi e-vals)
    (make-class (list <enum>) '()
		#:name class-name
		#:set e-vals)))

(define (gbank-enum-import info)
  (let* ((type-name (gbank-rt-get-type-name info))
	 (scm-name (gbank-gtype-name->scm-name type-name))
	 (e-vals (gbank-enum-get-values info)))
    (make <genum>
      #:type-name type-name
      #:scm-name scm-name
      #:value-set e-vals)))

(define (gbank-enum-get-values info)
  (let ((nb (gbank-ei-get-n-values info)))
    (and (> nb 0)
	 (gbank-enum-get-values-1 info nb 0 '()))))

(define (gbank-enum-get-values-1 info nb i set)
  (if (= i nb)
      (reverse! set)
      (let* ((value-info (gbank-ei-get-value info i))
	     (name (gbank-bi-get-name value-info))
	     (value (gbank-vi-get-value value-info)))
	(gbank-bi-unref value-info)
	(gbank-enum-get-values-1 info
				 nb
				 (+ i 1)
				 (cons (cons name value)
				       set)))))


;;;
;;; Low level API
;;;

(define (gbank-ei-get-n-values info)
  (g-enum-info-get-n-values info))

(define (gbank-ei-get-value info index)
  (let ((pointer (g-enum-info-get-value info index)))
    (if (null-pointer? pointer)
	#f
	pointer)))

(define (gbank-ei-get-n-methods info)
  (g-enum-info-get-n-methods info))

(define (gbank-ei-get-method info index)
  (let ((pointer (g-enum-info-get-method info index)))
    (if (null-pointer? pointer)
	#f
	pointer)))

(define (gbank-vi-get-value info)
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
