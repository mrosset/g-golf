;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2016 - 2019
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
  #:use-module (g-golf support libg-golf)
  #:use-module (g-golf support enum)
  #:use-module (g-golf gi utils)
  #:use-module (g-golf gobject type-info)
  #:use-module (g-golf gobject generic-values)
  #:use-module (g-golf gobject params-vals)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (g-object-new
            g-object-get-property
            g-object-set-property
            g-object-type
            g-object-type-name))


;;;
;;; GObject Low level API
;;;

(define (g-object-new gtype)
  (gi->scm (g_object_new gtype %null-pointer) 'pointer))

(define* (g-object-get-property object name g-value)
  (g_object_get_property object
			 (string->pointer name)
			 g-value))

(define* (g-object-set-property object name g-value)
  (g_object_set_property object
			 (string->pointer name)
			 g-value))

(define (g-object-type-name object)
  (g-type-name (g-object-type object)))


;;;
;;; From libg-golf
;;;

(define (g-object-type object)
  (g-object-type-c object))

#!

Not working yet, see libg-golf.scm for a problem description

(define (g-object-type-name object)
  (gi->scm (g-object-type-name-c object) 'string))

!#


;;;
;;; GObject Bindings
;;;

(define g_object_new
  (pointer->procedure '*
                      (dynamic-func "g_object_new"
				    %libgobject)
                      (list unsigned-long '*)))

(define g_object_get_property
  (pointer->procedure void
                      (dynamic-func "g_object_get_property"
				    %libgobject)
                      (list '* '* '*)))

(define g_object_set_property
  (pointer->procedure void
                      (dynamic-func "g_object_set_property"
				    %libgobject)
                      (list '* '* '*)))
