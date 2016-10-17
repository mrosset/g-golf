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

;; GIObjectInfo represents a GObject. This doesn't represent a specific
;; instance of a GObject, instead this represent the object type (eg
;; class).

;;; Code:


(define-module (g-golf gi object-info)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (g-golf support utils)
  #:use-module (g-golf gi gobject enum-flags)
  #:use-module (g-golf gi init)
  #:use-module (g-golf gi utils)
  #:use-module (g-golf gi base-info)
  #:use-module (g-golf gi registered-type-info)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (g-golf-object-import

	    g-golf-oi-get-abstract
	    g-golf-oi-get-parent
	    g-golf-oi-get-type-name
	    g-golf-oi-get-n-constants
	    g-golf-oi-get-constant
	    g-golf-oi-get-n-fields
	    g-golf-oi-get-field
	    g-golf-oi-get-n-interfaces
	    g-golf-oi-get-interface
	    g-golf-oi-get-n-methods
	    g-golf-oi-get-method
	    g-golf-oi-find-method
	    g-golf-oi-get-n-properties
	    g-golf-oi-get-property
	    g-golf-oi-get-n-signals
	    g-golf-oi-get-signal
	    g-golf-oi-find-signal
	    g-golf-oi-get-n-vfuncs
	    g-golf-oi-get-vfunc
	    g-golf-oi-find-vfunc
	    g-golf-oi-get-class-struct))


;;;
;;; Import Interface
;;;


(define (g-golf-object-import info)
  ;; fixme
  #f)


;;;
;;; Low level API
;;;

(define (g-golf-oi-get-abstract info)
  (g-golf-gtype->scm (g-object-info-get-abstract info)
		     'gboolean))

(define (g-golf-oi-get-parent info)
  (g-object-info-get-parent info))

(define (g-golf-oi-get-type-name info)
  (let ((pointer (g-object-info-get-type-name info)))
    (if (null-pointer? pointer)
	#f
	(g-golf-gtype->scm pointer 'gchar*))))

(define (g-golf-oi-get-n-constants info)
  (g-object-info-get-n-constants info))

(define (g-golf-oi-get-constant info index)
  (let ((pointer (g-object-info-get-constant info index)))
    (if (null-pointer? pointer)
	#f
	pointer)))

(define (g-golf-oi-get-n-fields info)
  (g-object-info-get-n-fields info))

(define (g-golf-oi-get-field info index)
  (let ((pointer (g-object-info-get-field info index)))
    (if (null-pointer? pointer)
	#f
	pointer)))

(define (g-golf-oi-get-n-interfaces info)
  (g-object-info-get-n-interfaces info))

(define (g-golf-oi-get-interface info index)
  (let ((pointer (g-object-info-get-interface info index)))
    (if (null-pointer? pointer)
	#f
	pointer)))

(define (g-golf-oi-get-n-methods info)
  (g-object-info-get-n-methods info))

(define (g-golf-oi-get-method info index)
  (let ((pointer (g-object-info-get-method info index)))
    (if (null-pointer? pointer)
	#f
	pointer)))

(define (g-golf-oi-find-method info name)
  (let ((pointer (g-object-info-find-method info
					    (string->pointer name))))
    (if (null-pointer? pointer)
	#f
	pointer)))

(define (g-golf-oi-get-n-properties info)
  (g-object-info-get-n-properties info))

(define (g-golf-oi-get-property info index)
  (let ((pointer (g-object-info-get-property info index)))
    (if (null-pointer? pointer)
	#f
	pointer)))

(define (g-golf-oi-get-n-signals info)
  (g-object-info-get-n-signals info))

(define (g-golf-oi-get-signal info index)
  (let ((pointer (g-object-info-get-signal info index)))
    (if (null-pointer? pointer)
	#f
	pointer)))

(define (g-golf-oi-find-signal info name)
  (let ((pointer (g-object-info-find-signal info
					    (string->pointer name))))
    (if (null-pointer? pointer)
	#f
	pointer)))

(define (g-golf-oi-get-n-vfuncs info)
  (g-object-info-get-n-vfuncs info))

(define (g-golf-oi-get-vfunc info index)
  (let ((pointer (g-object-info-get-vfunc info index)))
    (if (null-pointer? pointer)
	#f
	pointer)))

(define (g-golf-oi-find-vfunc info name)
  (let ((pointer (g-object-info-find-vfunc info
					    (string->pointer name))))
    (if (null-pointer? pointer)
	#f
	pointer)))

(define (g-golf-oi-get-class-struct info)
  (let ((pointer (g-object-info-get-class-struct info)))
    (if (null-pointer? pointer)
	#f
	pointer)))


;;;
;;; GI Bindings
;;;

(define (g-object-info-get-abstract info)
  (pointer->procedure int
                      (dynamic-func "g_object_info_get_abstract"
				    %libgirepository)
                      (list '*)))

(define g-object-info-get-parent
  (pointer->procedure '*
                      (dynamic-func "g_object_info_get_parent"
				    %libgirepository)
                      (list '*)))

(define g-object-info-get-type-name
  (pointer->procedure '*
                      (dynamic-func "g_object_info_get_type_name"
				    %libgirepository)
                      (list '*)))

(define g-object-info-get-n-constants
  (pointer->procedure int
                      (dynamic-func "g_object_info_get_n_constants"
				    %libgirepository)
                      (list '*)))

(define g-object-info-get-constant
  (pointer->procedure '*
                      (dynamic-func "g_object_info_get_constant"
				    %libgirepository)
                      (list '* int)))

(define g-object-info-get-n-fields
  (pointer->procedure int
                      (dynamic-func "g_object_info_get_n_fields"
				    %libgirepository)
                      (list '*)))

(define g-object-info-get-field
  (pointer->procedure '*
                      (dynamic-func "g_object_info_get_field"
				    %libgirepository)
                      (list '* int)))

(define g-object-info-get-n-interfaces
  (pointer->procedure int
                      (dynamic-func "g_object_info_get_n_interfaces"
				    %libgirepository)
                      (list '*)))

(define g-object-info-get-interface
  (pointer->procedure '*
                      (dynamic-func "g_object_info_get_interface"
				    %libgirepository)
                      (list '* int)))

(define g-object-info-get-n-methods
  (pointer->procedure int
                      (dynamic-func "g_object_info_get_n_methods"
				    %libgirepository)
                      (list '*)))

(define g-object-info-get-method
  (pointer->procedure '*
                      (dynamic-func "g_object_info_get_method"
				    %libgirepository)
                      (list '* int)))

(define g-object-info-find-method
  (pointer->procedure '*
                      (dynamic-func "g_object_info_find_method"
				    %libgirepository)
                      (list '* '*)))

(define g-object-info-get-n-properties
  (pointer->procedure int
                      (dynamic-func "g_object_info_get_n_properties"
				    %libgirepository)
                      (list '*)))

(define g-object-info-get-property
  (pointer->procedure '*
                      (dynamic-func "g_object_info_get_property"
				    %libgirepository)
                      (list '* int)))

(define g-object-info-get-n-signals
  (pointer->procedure int
                      (dynamic-func "g_object_info_get_n_signals"
				    %libgirepository)
                      (list '*)))

(define g-object-info-get-signal
  (pointer->procedure '*
                      (dynamic-func "g_object_info_get_signal"
				    %libgirepository)
                      (list '* int)))

(define g-object-info-find-signal
  (pointer->procedure '*
                      (dynamic-func "g_object_info_find_signal"
				    %libgirepository)
                      (list '* '*)))

(define g-object-info-get-n-vfuncs
  (pointer->procedure int
                      (dynamic-func "g_object_info_get_n_vfuncs"
				    %libgirepository)
                      (list '*)))

(define g-object-info-get-vfunc
  (pointer->procedure '*
                      (dynamic-func "g_object_info_get_vfunc"
				    %libgirepository)
                      (list '* int)))

(define g-object-info-find-vfunc
  (pointer->procedure '*
                      (dynamic-func "g_object_info_find_vfunc"
				    %libgirepository)
                      (list '* '*)))

(define g-object-info-get-class-struct
  (pointer->procedure '*
                      (dynamic-func "g_object_info_get_class_struct"
				    %libgirepository)
                      (list '*)))
