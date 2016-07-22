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


(define-module (golf gi enum-info)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (golf support utils)
  #:use-module (golf gi gobject enum-flags)
  #:use-module (golf gi init)
  #:use-module (golf gi utils)
  #:use-module (golf gi base-info)
  #:use-module (golf gi registered-type-info)

  #:export (golf-enum-import

	    golf-ei-get-n-values
	    golf-ei-get-value
	    golf-ei-get-n-methods
	    golf-ei-get-method

	    golf-vi-get-value))


;;;
;;; Build Interface
;;;

#;(define (golf-enum-import info)
  (let* ((type-name (golf-rt-get-type-name info))
	 (class-name (golf-gtype-name->class-name type-name))
	 (e-vals (golf-enum-get-values info)))
    (dimfi e-vals)
    (make-class (list <enum>) '()
		#:name class-name
		#:set e-vals)))

(define (golf-enum-import info)
  (let* ((type-name (golf-rt-get-type-name info))
	 (scm-name (golf-gtype-name->scm-name type-name))
	 (e-vals (golf-enum-get-values info)))
    (make <genum>
      #:type-name type-name
      #:scm-name scm-name
      #:value-set e-vals)))

(define (golf-enum-get-values info)
  (let ((nb (golf-ei-get-n-values info)))
    (and (> nb 0)
	 (golf-enum-get-values-1 info nb 0 '()))))

(define (golf-enum-get-values-1 info nb i set)
  (if (= i nb)
      (reverse! set)
      (let* ((value-info (golf-ei-get-value info i))
	     (name (golf-bi-get-name value-info))
	     (value (golf-vi-get-value value-info)))
	(golf-bi-unref value-info)
	(golf-enum-get-values-1 info
				 nb
				 (+ i 1)
				 (cons (cons name value)
				       set)))))


;;;
;;; Low level API
;;;

(define (golf-ei-get-n-values info)
  (g-enum-info-get-n-values info))

(define (golf-ei-get-value info index)
  (let ((pointer (g-enum-info-get-value info index)))
    (if (null-pointer? pointer)
	#f
	pointer)))

(define (golf-ei-get-n-methods info)
  (g-enum-info-get-n-methods info))

(define (golf-ei-get-method info index)
  (let ((pointer (g-enum-info-get-method info index)))
    (if (null-pointer? pointer)
	#f
	pointer)))

(define (golf-vi-get-value info)
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
