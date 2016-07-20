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


(define-module (gbank gi registered-type-info)
  #:use-module (system foreign)
  #:use-module (gbank gi init)
  #:use-module (gbank gi utils)
  #:use-module (gbank gi types)

  #:export (gbank-rt-get-type-name
	    gbank-rt-get-type-init
	    gbank-rt-get-g-type))


;;;
;;; Low level API
;;;

(define (gbank-rt-get-type-name info)
  (gbank-gtype->scm (g-registered-type-info-get-type-name info)
		    'gchar*))

;; this should not be called by language bindings
(define (gbank-rt-get-type-init info)
  (gbank-gtype->scm (g-registered-type-info-get-type-init info)
		    'gchar*))

(define (gbank-rt-get-g-type info)
  (g-registered-type-info-get-g-type info))


;;;
;;; GI Bindings
;;;

(define g-registered-type-info-get-type-name
  (pointer->procedure '*
                      (dynamic-func "g_registered_type_info_get_type_name"
				    %libgirepository)
                      (list '*)))

(define g-registered-type-info-get-type-init
  (pointer->procedure '*
                      (dynamic-func "g_registered_type_info_get_type_init"
				    %libgirepository)
                      (list '*)))

(define g-registered-type-info-get-g-type
  (pointer->procedure int64
                      (dynamic-func "g_registered_type_info_get_g_type"
				    %libgirepository)
                      (list '*)))
