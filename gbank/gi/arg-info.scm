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


(define-module (gbank gi arg-info)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (gbank support enum)
  #:use-module (gbank gi init)
  #:use-module (gbank gi utils)
  #:use-module (gbank gi types)

  #:export (gbank-ai-get-closure
	    gbank-ai-get-destroy
	    gbank-ai-get-direction
	    gbank-ai-get-ownership-transfer
	    gbank-ai-get-scope
	    gbank-ai-get-type
	    gbank-ai-may-be-null
	    gbank-ai-is-optional
	    gbank-ai-is-return-value))


;;;
;;; Low level API
;;;

(define (gbank-ai-get-closure info)
  (g-arg-info-get-closure info))

(define (gbank-ai-get-destroy info)
  (g-arg-info-get-destroy info))

(define (gbank-ai-get-direction info)
  (e-sym %gbank-ai-direction
	 (g-arg-info-get-direction info)))

(define (gbank-ai-get-ownership-transfer info)
  (e-sym %gbank-ai-transfer
	 (g-arg-info-get-ownership-transfer info)))

(define (gbank-ai-get-scope info)
  (e-sym %gbank-ai-scope
	 (g-arg-info-get-scope info)))

(define (gbank-ai-get-type info)
  (g-arg-info-get-type info))

(define (gbank-ai-may-be-null info)
  (gbank-gtype->scm (g-arg-info-may-be-null info)
		    'gboolean))

(define (gbank-ai-is-return-value info)
  (gbank-gtype->scm (g-arg-info-is-return-value info)
		    'gboolean))


;;;
;;; GI Bindings
;;;


(define g-arg-info-get-closure
  (pointer->procedure int
                      (dynamic-func "g_arg_info_get_closure"
				    %libgirepository)
                      (list '*)))

(define g-arg-info-get-destroy
  (pointer->procedure int
                      (dynamic-func "g_arg_info_get_destroy"
				    %libgirepository)
                      (list '*)))

(define g-arg-info-get-direction
  (pointer->procedure int
                      (dynamic-func "g_arg_info_get_direction"
				    %libgirepository)
                      (list '*)))

(define g-arg-info-get-ownership-transfer
  (pointer->procedure int
                      (dynamic-func "g_arg_info_get_ownership_transfer"
				    %libgirepository)
                      (list '*)))

(define g-arg-info-get-scope
  (pointer->procedure int
                      (dynamic-func "g_arg_info_get_scope"
				    %libgirepository)
                      (list '*)))

(define g-arg-info-get-type
  (pointer->procedure '*
                      (dynamic-func "g_arg_info_get_type"
				    %libgirepository)
                      (list '*)))

(define g-arg-info-may-be-null
  (pointer->procedure int
                      (dynamic-func "g_arg_info_may_be_null"
				    %libgirepository)
                      (list '*)))

(define g-arg-info-is-optional
  (pointer->procedure int
                      (dynamic-func "g_arg_info_is_optional"
				    %libgirepository)
                      (list '*)))

(define g-arg-info-is-return-value
  (pointer->procedure int
                      (dynamic-func "g_arg_info_is_return_value"
				    %libgirepository)
                      (list '*)))

