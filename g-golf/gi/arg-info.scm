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


(define-module (g-golf gi arg-info)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (g-golf support enum)
  #:use-module (g-golf init)
  #:use-module (g-golf gi utils)
  #:use-module (g-golf gi types)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (g-golf-ai-get-closure
	    g-golf-ai-get-destroy
	    g-golf-ai-get-direction
	    g-golf-ai-get-ownership-transfer
	    g-golf-ai-get-scope
	    g-golf-ai-get-type
	    g-golf-ai-may-be-null
	    g-golf-ai-is-optional
	    g-golf-ai-is-return-value))


;;;
;;; Low level API
;;;

(define (g-golf-ai-get-closure info)
  (g-arg-info-get-closure info))

(define (g-golf-ai-get-destroy info)
  (g-arg-info-get-destroy info))

(define (g-golf-ai-get-direction info)
  (enum->symbol %g-golf-ai-direction
                (g-arg-info-get-direction info)))

(define (g-golf-ai-get-ownership-transfer info)
  (enum->symbol %g-golf-ai-transfer
                (g-arg-info-get-ownership-transfer info)))

(define (g-golf-ai-get-scope info)
  (enum->symbol %g-golf-ai-scope
                (g-arg-info-get-scope info)))

(define (g-golf-ai-get-type info)
  (g-arg-info-get-type info))

(define (g-golf-ai-may-be-null info)
  (g-golf-gtype->scm (g-arg-info-may-be-null info)
		    'gboolean))

(define (g-golf-ai-is-return-value info)
  (g-golf-gtype->scm (g-arg-info-is-return-value info)
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

