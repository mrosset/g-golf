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


(define-module (gbank gi type-info)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (gbank support enum)
  #:use-module (gbank gi init)
  #:use-module (gbank gi utils)
  #:use-module (gbank gi types)

  #:export (gbank-ti-type-tag-to-string
	    gbank-ti-info-type-to-string
	    gbank-ti-is-pointer
	    gbank-ti-get-tag
	    gbank-ti-get-param-type
	    gbank-ti-get-interface
	    gbank-ti-get-array-length
	    gbank-ti-get-array-fixed-size
	    gbank-ti-is-zero-terminated
	    gbank-ti-get-array-type))


;;;
;;; Low level API
;;;

(define (gbank-ti-type-tag-to-string type-tag)
  (gbank-gtype->scm (g-type-tag-to-string type-tag)
		    'gchar*))

(define (gbank-ti-info-type-to-string info-type)
  (gbank-gtype->scm (g-info-type-to-string info-type)
		    'gchar*))

(define (gbank-ti-is-pointer info-type)
  (gbank-gtype->scm (g-type-info-is-pointer info-type)
		    'gboolean))

(define (gbank-ti-get-tag info-type)
  (e-sym %gbank-ct-type-tag
	 (g-type-info-get-tag info-type)))

(define (gbank-ti-get-param-type info-type n)
  (g-type-info-get-param-type info-type n))

(define (gbank-ti-get-interface info-type)
  (g-type-info-get-interface info-type))

(define (gbank-ti-get-array-length info-type)
  (g-type-info-get-array-length info-type))

(define (gbank-ti-get-array-fixed-size info-type)
  (g-type-info-get-array-fixed-size info-type))

(define (gbank-ti-is-zero-terminated info-type)
  (gbank-gtype->scm (g-type-info-is-zero-terminated info-type)
		    'gboolean))

(define (gbank-ti-get-array-type info-type)
  (e-sym %gbank-ct-array-type
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
