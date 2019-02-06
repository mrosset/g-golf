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
  #:use-module (g-golf support struct)
  #:use-module (g-golf init)
  #:use-module (g-golf gi utils)
  #:use-module (g-golf gi base-info)
  #:use-module (g-golf gi registered-type-info)
  #:use-module (g-golf gi field-info)
  #:use-module (g-golf gi type-info)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (gi-struct-import
            gi-struct-field-types

            g-struct-info-get-n-fields
            g-struct-info-get-field))


;;;
;;; Build Interface
;;;

(define (gi-struct-import info)
  (let* ((type-name (g-registered-type-info-get-type-name info))
	 (scm-name (gi-name->scm-name type-name))
         (field-types (gi-struct-field-types info)))
    (make <gi-struct>
      #:gi-name type-name
      #:scm-name scm-name
      #:field-types field-types)))

(define (gi-struct-field-types info)
  (letrec ((struct-field-types
	    (lambda (info n i t-set)
	      (if (= i n)
		  (reverse! t-set)
		  (let* ((field-info (g-struct-info-get-field info i))
			 (type-info (g-field-info-get-type field-info))
                         (type-tag (g-type-info-get-tag type-info)))
		    (g-base-info-unref field-info)
		    (struct-field-types info
				        n
				        (+ i 1)
				        (cons type-tag
					      t-set)))))))
    (struct-field-types info
		        (g-struct-info-get-n-fields info)
		        0
		        '())))


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
