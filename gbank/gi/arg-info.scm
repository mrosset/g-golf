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


(define-module (golf gi arg-info)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (golf support enum)
  #:use-module (golf gi init)
  #:use-module (golf gi utils)
  #:use-module (golf gi types)

  #:export (golf-ai-get-closure
	    golf-ai-get-destroy
	    golf-ai-get-direction
	    golf-ai-get-ownership-transfer
	    golf-ai-get-scope
	    golf-ai-get-type
	    golf-ai-may-be-null
	    golf-ai-is-optional
	    golf-ai-is-return-value))


;;;
;;; Low level API
;;;

(define (golf-ai-get-closure info)
  (g-arg-info-get-closure info))

(define (golf-ai-get-destroy info)
  (g-arg-info-get-destroy info))

(define (golf-ai-get-direction info)
  (e-sym %golf-ai-direction
	 (g-arg-info-get-direction info)))

(define (golf-ai-get-ownership-transfer info)
  (e-sym %golf-ai-transfer
	 (g-arg-info-get-ownership-transfer info)))

(define (golf-ai-get-scope info)
  (e-sym %golf-ai-scope
	 (g-arg-info-get-scope info)))

(define (golf-ai-get-type info)
  (g-arg-info-get-type info))

(define (golf-ai-may-be-null info)
  (golf-gtype->scm (g-arg-info-may-be-null info)
		    'gboolean))

(define (golf-ai-is-return-value info)
  (golf-gtype->scm (g-arg-info-is-return-value info)
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

