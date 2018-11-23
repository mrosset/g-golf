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

  #:export (g-golf-property-import

	    g-golf-pi-get-flags
	    g-golf-pi-get-ownership-transfer
	    g-golf-pi-get-type))


;;;
;;; Build Interface
;;;

(define (g-golf-property-import info)
  ;; Fixme...
  (g-base-info-get-name info))


;;;
;;; Low level API
;;;

(define (g-golf-pi-get-flags info)
  (g-golf-integer->gflags %g-golf-go-param-flags
                          (g-property-info-get-flags info)))

(define (g-golf-pi-get-ownership-transfer info)
  (enum->symbol %g-golf-ai-transfer
                (g-property-info-get-ownership-transfer info)))

(define (g-golf-pi-get-type info)
  (g-property-info-get-type info))


;;;
;;; GI Bindings
;;;

(define g-property-info-get-flags
  (pointer->procedure uint32
                      (dynamic-func "g_property_info_get_flags"
				    %libgirepository)
                      (list '*)))

(define g-property-info-get-ownership-transfer
  (pointer->procedure int
                      (dynamic-func "g_property_info_get_ownership_transfer"
				    %libgirepository)
                      (list '*)))

(define g-property-info-get-type
  (pointer->procedure '*
                      (dynamic-func "g_property_info_get_type"
				    %libgirepository)
                      (list '*)))
