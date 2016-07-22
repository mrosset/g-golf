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


(define-module (golf gi base-info)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (golf support enum)
  #:use-module (golf gi init)
  #:use-module (golf gi utils)
  #:use-module (golf gi types)

  #:export (golf-bi-ref
	    golf-bi-unref
	    golf-bi-equal
	    golf-bi-get-type
	    golf-bi-get-typelib
	    golf-bi-get-namespace
	    golf-bi-get-name
	    golf-bi-get-attribute
	    golf-bi-iterate-attributes
	    golf-bi-get-container
	    golf-bi-is-deprecated))


;;;
;;; Low level API
;;;

(define (golf-bi-ref info)
  (g-base-info-ref info))

(define (golf-bi-unref info)
  (g-base-info-unref info))

(define (golf-bi-equal info1 info2)
  (golf-gtype->scm (g-base-info-equal info1 info2)
		    'gboolean))
  
(define (golf-bi-get-type info)
  (e-sym %golf-bi-info-type
	 (g-base-info-get-type info)))

(define (golf-bi-get-typelib info)
  (g-base-info-get-typelib info))

(define (golf-bi-get-namespace info)
  (golf-gtype->scm (g-base-info-get-namespace info)
		    'gchar*))

(define (golf-bi-get-name info)
  (let ((pointer (g-base-info-get-name info)))
    (if (null-pointer? pointer)
	#f
	(golf-gtype->scm pointer 'gchar*))))

(define (golf-bi-get-attribute info name)
  (let ((pointer (g-base-info-get-attribute info
					    (string->pointer name))))
    (if (null-pointer? pointer)
	#f
	(golf-gtype->scm pointer 'gchar*))))

(define (golf-bi-iterate-attributes info proc)
  (let ((iter (golf-attribute-iter-new))
	(name (golf-pointer-new))
	(value (golf-pointer-new)))
    (while (> (g-base-info-iterate-attributes info iter name value)
	      0)
      (proc name value))
    (values)))

(define (golf-bi-get-container info)
  (g-base-info-get-container info))

(define (golf-bi-is-deprecated info)
  (golf-gtype->scm (g-base-info-is-deprecated info)
		    'gboolean))


;;;
;;; GI Bindings
;;;

(define g-base-info-ref
  (pointer->procedure '*
                      (dynamic-func "g_base_info_ref"
				    %libgirepository)
                      (list '*)))

(define g-base-info-unref
  (pointer->procedure void
                      (dynamic-func "g_base_info_unref"
				    %libgirepository)
                      (list '*)))

(define g-base-info-equal
  (pointer->procedure int
                      (dynamic-func "g_base_info_equal"
				    %libgirepository)
                      (list '* '*)))

(define g-base-info-get-type
  (pointer->procedure int
                      (dynamic-func "g_base_info_get_type"
				    %libgirepository)
                      (list '*)))

(define g-base-info-get-typelib
  (pointer->procedure '*
                      (dynamic-func "g_base_info_get_typelib"
				    %libgirepository)
                      (list '*)))

(define g-base-info-get-namespace
  (pointer->procedure '*
                      (dynamic-func "g_base_info_get_namespace"
				    %libgirepository)
                      (list '*)))

(define g-base-info-get-name
  (pointer->procedure '*
                      (dynamic-func "g_base_info_get_name"
				    %libgirepository)
                      (list '*)))

(define g-base-info-get-attribute
  (pointer->procedure '*
                      (dynamic-func "g_base_info_get_attribute"
				    %libgirepository)
                      (list '* '*)))

(define g-base-info-iterate-attributes
  (pointer->procedure int
                      (dynamic-func "g_base_info_iterate_attributes"
				    %libgirepository)
                      (list '* '* '* '*)))

(define g-base-info-get-container
  (pointer->procedure '*
                      (dynamic-func "g_base_info_get_container"
				    %libgirepository)
                      (list '*)))

(define g-base-info-is-deprecated
  (pointer->procedure int
                      (dynamic-func "g_base_info_is_deprecated"
				    %libgirepository)
                      (list '*)))
