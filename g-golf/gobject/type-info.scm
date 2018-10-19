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


(define-module (g-golf gobject type-info)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (g-golf init)
  #:use-module (g-golf gi utils)
  #:use-module (g-golf gobject enum-flags)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (g-golf-go-type-name
	    %g-golf-go-fundamental-flags))


;;;
;;; GObject Low level API
;;;

(define (g-golf-go-type-name id)
  (g-golf-gtype->scm (g-type-name id)
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

(define %g-golf-go-fundamental-flags
  (make <genum>
    #:type-name "GTypeFundamentalFlags"
    #:scm-name "g-type-fundamental-flags"
    #:value-set '(classed
		  instantiable
		  derivable
		  deep-derivable)))
