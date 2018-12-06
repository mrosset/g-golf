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


(define-module (g-golf gi property-info)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (g-golf support utils)
  #:use-module (g-golf support enum)
  #:use-module (g-golf init)
  #:use-module (g-golf gobject param-spec)
  #:use-module (g-golf gi utils)
  #:use-module (g-golf gi base-info)
  #:use-module (g-golf gi arg-info)
  
  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (gi-property-import

	    g-property-info-get-flags
	    g-property-info-get-ownership-transfer
	    g-property-info-get-type))


;;;
;;; Build Interface
;;;

(define (gi-property-import info)
  ;; Fixme...
  (g-base-info-get-name info))


;;;
;;; Low level API
;;;

(define (g-property-info-get-flags info)
  (gi-integer->gflags %g-param-flags
                      (g_property_info_get_flags info)))

(define (g-property-info-get-ownership-transfer info)
  (enum->symbol %gi-transfer
                (g_property_info_get_ownership_transfer info)))

(define (g-property-info-get-type info)
  (g_property_info_get_type info))


;;;
;;; GI Bindings
;;;

(define g_property_info_get_flags
  (pointer->procedure uint32
                      (dynamic-func "g_property_info_get_flags"
				    %libgirepository)
                      (list '*)))

(define g_property_info_get_ownership_transfer
  (pointer->procedure int
                      (dynamic-func "g_property_info_get_ownership_transfer"
				    %libgirepository)
                      (list '*)))

(define g_property_info_get_type
  (pointer->procedure '*
                      (dynamic-func "g_property_info_get_type"
				    %libgirepository)
                      (list '*)))
