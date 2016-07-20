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


(define-module (gbank gi gobject type-info)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (gbank gi init)
  #:use-module (gbank gi utils)
  #:use-module (gbank gi gobject enum-flags)

  #:export (gbank-go-type-name
	    %gbank-gt-fundamental-flags))


;;;
;;; GObject Low level API
;;;

(define (gbank-go-type-name id)
  (gbank-gtype->scm (g-type-name id)
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

(define %gbank-gt-fundamental-flags
  (make <genum>
    #:type-name "GTypeFundamentalFlags"
    #:scm-name "g-type-fundamental-flags"
    #:value-set '(classed
		  instantiable
		  derivable
		  deep-derivable)))
