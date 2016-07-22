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


(define-module (golf gi gobject type-info)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (golf gi init)
  #:use-module (golf gi utils)
  #:use-module (golf gi gobject enum-flags)

  #:export (golf-go-type-name
	    %golf-gt-fundamental-flags))


;;;
;;; GObject Low level API
;;;

(define (golf-go-type-name id)
  (golf-gtype->scm (g-type-name id)
		    'gchar*))


;;;
;;; GObject Bindings
;;;

(define g-type-name
  (pointer->procedure '*
                      (dynamic-func "g_type_name"
				    %libgobject)
                      (list int64)))


;;;
;;; Gtype*
;;;

(define %golf-gt-fundamental-flags
  (make <genum>
    #:type-name "GTypeFundamentalFlags"
    #:scm-name "g-type-fundamental-flags"
    #:value-set '(classed
		  instantiable
		  derivable
		  deep-derivable)))
