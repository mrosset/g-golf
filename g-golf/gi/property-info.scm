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


(define-module (g-golf gi property-info)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (g-golf support utils)
  #:use-module (g-golf gobject enum-flags)
  #:use-module (g-golf gi init)
  #:use-module (g-golf gi utils)
  #:use-module (g-golf gi base-info)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (g-golf-property-import

	    g-golf-pi-get-flags))


;;;
;;; Build Interface
;;;

(define (g-golf-property-import info)
  ;; Fixme...
  (g-golf-bi-get-name info))


;;;
;;; Low level API
;;;

(define (g-golf-pi-get-flags info)
  (g-property-info-get-flags info))


;;;
;;; GI Bindings
;;;

(define g-property-info-get-flags
  (pointer->procedure uint32
                      (dynamic-func "g_property_info_get_flags"
				    %libgirepository)
                      (list '*)))
