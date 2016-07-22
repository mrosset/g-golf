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


(define-module (golf gi type-info)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (golf support enum)
  #:use-module (golf gi init)
  #:use-module (golf gi utils)
  #:use-module (golf gi types)

  #:export (golf-ti-type-tag-to-string
	    golf-ti-info-type-to-string
	    golf-ti-is-pointer
	    golf-ti-get-tag
	    golf-ti-get-param-type
	    golf-ti-get-interface
	    golf-ti-get-array-length
	    golf-ti-get-array-fixed-size
	    golf-ti-is-zero-terminated
	    golf-ti-get-array-type))


;;;
;;; Low level API
;;;

(define (golf-ti-type-tag-to-string type-tag)
  (golf-gtype->scm (g-type-tag-to-string type-tag)
		    'gchar*))

(define (golf-ti-info-type-to-string info-type)
  (golf-gtype->scm (g-info-type-to-string info-type)
		    'gchar*))

(define (golf-ti-is-pointer info-type)
  (golf-gtype->scm (g-type-info-is-pointer info-type)
		    'gboolean))

(define (golf-ti-get-tag info-type)
  (e-sym %golf-ct-type-tag
	 (g-type-info-get-tag info-type)))

(define (golf-ti-get-param-type info-type n)
  (g-type-info-get-param-type info-type n))

(define (golf-ti-get-interface info-type)
  (g-type-info-get-interface info-type))

(define (golf-ti-get-array-length info-type)
  (g-type-info-get-array-length info-type))

(define (golf-ti-get-array-fixed-size info-type)
  (g-type-info-get-array-fixed-size info-type))

(define (golf-ti-is-zero-terminated info-type)
  (golf-gtype->scm (g-type-info-is-zero-terminated info-type)
		    'gboolean))

(define (golf-ti-get-array-type info-type)
  (e-sym %golf-ct-array-type
	 (g-type-info-get-array-type info-type)))


;;;
;;; GI Bindings
;;;


(define g-type-tag-to-string
  (pointer->procedure '*
                      (dynamic-func "g_type_tag_to_string"
				    %libgirepository)
                      (list int)))

(define g-info-type-to-string
  (pointer->procedure int
                      (dynamic-func "g_info_type_to_string"
				    %libgirepository)
                      (list '*)))

(define g-type-info-is-pointer
  (pointer->procedure int
                      (dynamic-func "g_type_info_is_pointer"
				    %libgirepository)
                      (list '*)))

(define g-type-info-get-tag
  (pointer->procedure int
                      (dynamic-func "g_type_info_get_tag"
				    %libgirepository)
                      (list '*)))

(define g-type-info-get-param-type
  (pointer->procedure '*
                      (dynamic-func "g_type_info_get_param_type"
				    %libgirepository)
                      (list '* int)))

(define g-type-info-get-interface
  (pointer->procedure '*
                      (dynamic-func "g_type_info_get_interface"
				    %libgirepository)
                      (list '*)))

(define g-type-info-get-array-length
  (pointer->procedure int
                      (dynamic-func "g_type_info_get_array_length"
				    %libgirepository)
                      (list '*)))

(define g-type-info-get-array-fixed-size
  (pointer->procedure int
                      (dynamic-func "g_type_info_get_array_fixed_size"
				    %libgirepository)
                      (list '*)))

(define g-type-info-is-zero-terminated
  (pointer->procedure int
                      (dynamic-func "g_type_info_is_zero_terminated"
				    %libgirepository)
                      (list '*)))

(define g-type-info-get-array-type
  (pointer->procedure int
                      (dynamic-func "g_type_info_get_array_type"
				    %libgirepository)
                      (list '*)))
