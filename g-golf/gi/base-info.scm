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


(define-module (g-golf gi base-info)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (g-golf support enum)
  #:use-module (g-golf init)
  #:use-module (g-golf gi utils)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (g-golf-bi-ref
	    g-golf-bi-unref
	    g-golf-bi-equal
	    g-golf-bi-get-type
	    g-golf-bi-get-typelib
	    g-golf-bi-get-namespace
	    g-golf-bi-get-name
	    g-golf-bi-get-attribute
	    g-golf-bi-iterate-attributes
	    g-golf-bi-get-container
	    g-golf-bi-is-deprecated))


;;;
;;; Low level API
;;;

(define (g-golf-bi-ref info)
  (g-base-info-ref info))

(define (g-golf-bi-unref info)
  (g-base-info-unref info))

(define (g-golf-bi-equal info1 info2)
  (g-golf-gtype->scm (g-base-info-equal info1 info2)
		    'gboolean))
  
(define (g-golf-bi-get-type info)
  (enum->symbol %g-golf-bi-info-type
                (g-base-info-get-type info)))

(define (g-golf-bi-get-typelib info)
  (g-base-info-get-typelib info))

(define (g-golf-bi-get-namespace info)
  (g-golf-gtype->scm (g-base-info-get-namespace info)
		    'gchar*))

(define (g-golf-bi-get-name info)
  (let ((pointer (g-base-info-get-name info)))
    (if (null-pointer? pointer)
	#f
	(g-golf-gtype->scm pointer 'gchar*))))

(define (g-golf-bi-get-attribute info name)
  (let ((pointer (g-base-info-get-attribute info
					    (string->pointer name))))
    (if (null-pointer? pointer)
	#f
	(g-golf-gtype->scm pointer 'gchar*))))

(define (g-golf-bi-iterate-attributes info proc)
  (let ((iter (g-golf-attribute-iter-new))
	(name (g-golf-pointer-new))
	(value (g-golf-pointer-new)))
    (while (> (g-base-info-iterate-attributes info iter name value)
	      0)
      (proc name value))
    (values)))

(define (g-golf-bi-get-container info)
  (let ((pointer (g-base-info-get-container info)))
    (if (null-pointer? pointer)
	#f
	pointer)))

(define (g-golf-bi-is-deprecated info)
  (g-golf-gtype->scm (g-base-info-is-deprecated info)
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


;;;
;;; Types and Values
;;;

(define %g-golf-bi-info-type
  (make <gi-enum>
    #:gi-name "GIInfoType"
    #:scm-name "gi-info-type"
    #:enum-set '(invalid
                 function
                 callback
                 struct
                 boxed
                 enum
                 flags
                 object
                 interface
                 constant
                 error-domain ;; invalid_0 - deleted
                 union
                 value
                 signal
                 vfunc
                 property
                 field
                 arg
                 type
                 unresolved)))
