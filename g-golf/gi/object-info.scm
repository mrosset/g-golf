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

  #:export (g-golf-object-import

	    g-golf-oi-get-type-name))


;;;
;;; Import Interface
;;;


(define (g-golf-object-import info)
  ;; fixme
  #f)


;;;
;;; Low level API
;;;

(define (g-golf-oi-get-type-name info)
  (let ((pointer (g-object-info-get-type-name info)))
    (if (null-pointer? pointer)
	#f
	(g-golf-gtype->scm pointer 'gchar*))))


;;;
;;; GI Bindings
;;;

(define g-object-info-get-type-name
  (pointer->procedure '*
                      (dynamic-func "g_object_info_get_type_name"
				    %libgirepository)
                      (list '*)))
