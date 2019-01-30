;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2019
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


(define-module (g-golf gi struct-info)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (g-golf support utils)
  #:use-module (g-golf support enum)
  #:use-module (g-golf init)
  #:use-module (g-golf gi utils)
  #:use-module (g-golf gi base-info)
  #:use-module (g-golf gi registered-type-info)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (g-struct-info-get-n-fields
            g-struct-info-get-field))


;;;
;;; Low level API
;;;

(define (g-struct-info-get-n-fields info)
  (g_struct_info_get_n_fields info))

(define (g-struct-info-get-field info n)
  (gi->scm (g_struct_info_get_field info n) 'pointer))


;;;
;;; GI Bindings
;;;

(define g_struct_info_get_n_fields
  (pointer->procedure int
                      (dynamic-func "g_struct_info_get_n_fields"
				    %libgirepository)
                      (list '*)))

(define g_struct_info_get_field
  (pointer->procedure '*
                      (dynamic-func "g_struct_info_get_field"
				    %libgirepository)
                      (list '* int)))
