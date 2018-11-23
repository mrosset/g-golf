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


(define-module (g-golf gobject gobject)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (rnrs arithmetic bitwise)
  #:use-module (g-golf init)
  #:use-module (g-golf gi utils)
  #:use-module (g-golf gi common-types)
  #:use-module (g-golf gi base-info)
  #:use-module (g-golf gi registered-type-info)
  #:use-module (g-golf gi property-info)
  #:use-module (g-golf gi type-info)
  #:use-module (g-golf support enum)
  #:use-module (g-golf gobject generic-values)
  #:use-module (g-golf gobject params-vals)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (g-object-get-property
	    g-object-set-property))


;;;
;;; GObject Low level API
;;;

(define (g-object-get-property object property)
  ;; both args, object and property, are pointers
  (let* ((name (g-base-info-get-name property))
	 (type-info (g-property-info-get-type property))
	 (type-tag (g-golf-ti-get-tag type-info))
	 (type-value (enum->value %g-common-types-type-tag type-tag))
	 (type-name (g-golf-ti-type-tag-to-string type-value))
	 (g-type (bitwise-arithmetic-shift type-value 2))
	 (g-value (g-value-init g-type)))
    (g_object_get_property object
			   (string->pointer name)
			   g-value)
    ;; FIXME!
    ;; Incorrect: it must call g-value-ref, to be defined still, but in
    ;; the ean tme, just so it compiles (it obviusly will rase an
    ;; exception if the property type is not a gfloat ...
    (g-value-get-float g-value)))

(define (g-object-set-property object name value)
  ;; ...
  #f)


;;;
;;; GObject Bindings
;;;

(define g_object_get_property
  (pointer->procedure void
                      (dynamic-func "g_object_get_property"
				    %libgobject)
                      (list '* '* '*)))
