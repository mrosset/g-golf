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


(define-module (g-golf gi type-info)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (g-golf support enum)
  #:use-module (g-golf init)
  #:use-module (g-golf gi utils)
  #:use-module (g-golf gi common-types)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (g-type-tag-to-string
	    g-info-type-to-string
	    g-type-info-is-pointer
	    g-type-info-get-tag
	    g-type-info-get-param-type
	    g-type-info-get-interface
	    g-type-info-get-array-length
	    g-type-info-get-array-fixed-size
	    g-type-info-is-zero-terminated
	    g-type-info-get-array-type))


;;;
;;; Low level API
;;;

(define (g-type-tag-to-string type-tag)
  (g-golf-gtype->scm (g_type_tag_to_string type-tag)
		    'gchar*))

(define (g-info-type-to-string info-type)
  (g-golf-gtype->scm (g_info_type_to_string info-type)
		    'gchar*))

(define (g-type-info-is-pointer info-type)
  (g-golf-gtype->scm (g_type_info_is_pointer info-type)
		    'gboolean))

(define (g-type-info-get-tag info-type)
  (enum->symbol %g-common-types-type-tag
                (g_type_info_get_tag info-type)))

(define (g-type-info-get-param-type info-type n)
  (g_type_info_get_param_type info-type n))

(define (g-type-info-get-interface info-type)
  (let ((pointer (g_type_info_get_interface info-type)))
    (if (null-pointer? pointer)
	#f
	pointer)))

(define (g-type-info-get-array-length info-type)
  (g_type_info_get_array_length info-type))

(define (g-type-info-get-array-fixed-size info-type)
  (g_type_info_get_array_fixed_size info-type))

(define (g-type-info-is-zero-terminated info-type)
  (g-golf-gtype->scm (g_type_info_is_zero_terminated info-type)
		    'gboolean))

(define (g-type-info-get-array-type info-type)
  (enum->symbol %g-common-types-array-type
                (g_type_info_get_array_type info-type)))


;;;
;;; GI Bindings
;;;


(define g_type_tag_to_string
  (pointer->procedure '*
                      (dynamic-func "g_type_tag_to_string"
				    %libgirepository)
                      (list int)))

(define g_info_type_to_string
  (pointer->procedure int
                      (dynamic-func "g_info_type_to_string"
				    %libgirepository)
                      (list '*)))

(define g_type_info_is_pointer
  (pointer->procedure int
                      (dynamic-func "g_type_info_is_pointer"
				    %libgirepository)
                      (list '*)))

(define g_type_info_get_tag
  (pointer->procedure int
                      (dynamic-func "g_type_info_get_tag"
				    %libgirepository)
                      (list '*)))

(define g_type_info_get_param_type
  (pointer->procedure '*
                      (dynamic-func "g_type_info_get_param_type"
				    %libgirepository)
                      (list '* int)))

(define g_type_info_get_interface
  (pointer->procedure '*
                      (dynamic-func "g_type_info_get_interface"
				    %libgirepository)
                      (list '*)))

(define g_type_info_get_array_length
  (pointer->procedure int
                      (dynamic-func "g_type_info_get_array_length"
				    %libgirepository)
                      (list '*)))

(define g_type_info_get_array_fixed_size
  (pointer->procedure int
                      (dynamic-func "g_type_info_get_array_fixed_size"
				    %libgirepository)
                      (list '*)))

(define g_type_info_is_zero_terminated
  (pointer->procedure int
                      (dynamic-func "g_type_info_is_zero_terminated"
				    %libgirepository)
                      (list '*)))

(define g_type_info_get_array_type
  (pointer->procedure int
                      (dynamic-func "g_type_info_get_array_type"
				    %libgirepository)
                      (list '*)))
