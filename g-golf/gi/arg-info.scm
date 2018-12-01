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

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (;; Procedures
            g-arg-info-get-closure
	    g-arg-info-get-destroy
	    g-arg-info-get-direction
	    g-arg-info-get-ownership-transfer
	    g-arg-info-get-scope
	    g-arg-info-get-type
	    g-arg-info-may-be-null
	    g-arg-info-is-optional
	    g-arg-info-is-return-value
            ;; Types and Values
            %gi-direction
	    %gi-scope-type
	    %gi-transfer))


;;;
;;; Low level API
;;;

(define (g-arg-info-get-closure info)
  (g_arg_info_get_closure info))

(define (g-arg-info-get-destroy info)
  (g_arg_info_get_destroy info))

(define (g-arg-info-get-direction info)
  (enum->symbol %gi-direction
                (g_arg_info_get_direction info)))

(define (g-arg-info-get-ownership-transfer info)
  (enum->symbol %gi-transfer
                (g_arg_info_get_ownership_transfer info)))

(define (g-arg-info-get-scope info)
  (enum->symbol %gi-scope-type
                (g_arg_info_get_scope info)))

(define (g-arg-info-get-type info)
  (g_arg_info_get_type info))

(define (g-arg-info-may-be-null info)
  (gi->scm (g_arg_info_may_be_null info)
           'gboolean))

(define (g-arg-info-is-return-value info)
  (gi->scm (g_arg_info_is_return_value info)
           'gboolean))


;;;
;;; GI Bindings
;;;


(define g_arg_info_get_closure
  (pointer->procedure int
                      (dynamic-func "g_arg_info_get_closure"
				    %libgirepository)
                      (list '*)))

(define g_arg_info_get_destroy
  (pointer->procedure int
                      (dynamic-func "g_arg_info_get_destroy"
				    %libgirepository)
                      (list '*)))

(define g_arg_info_get_direction
  (pointer->procedure int
                      (dynamic-func "g_arg_info_get_direction"
				    %libgirepository)
                      (list '*)))

(define g_arg_info_get_ownership_transfer
  (pointer->procedure int
                      (dynamic-func "g_arg_info_get_ownership_transfer"
				    %libgirepository)
                      (list '*)))

(define g_arg_info_get_scope
  (pointer->procedure int
                      (dynamic-func "g_arg_info_get_scope"
				    %libgirepository)
                      (list '*)))

(define g_arg_info_get_type
  (pointer->procedure '*
                      (dynamic-func "g_arg_info_get_type"
				    %libgirepository)
                      (list '*)))

(define g_arg_info_may_be_null
  (pointer->procedure int
                      (dynamic-func "g_arg_info_may_be_null"
				    %libgirepository)
                      (list '*)))

(define g_arg_info_is_optional
  (pointer->procedure int
                      (dynamic-func "g_arg_info_is_optional"
				    %libgirepository)
                      (list '*)))

(define g_arg_info_is_return_value
  (pointer->procedure int
                      (dynamic-func "g_arg_info_is_return_value"
				    %libgirepository)
                      (list '*)))



;;;
;;; Tyeps and Values
;;;

(define %gi-direction
  (make <gi-enum>
    #:gi-name "GIDirection"
    #:scm-name "gi-direction"
    #:enum-set '(in
                 out
                 inout)))

(define %gi-scope-type
  (make <gi-enum>
    #:gi-name "GIScopeType"
    #:scm-name "gi-scope-type"
    #:enum-set '(invalid
                 call
                 async
                 notified)))

(define %gi-transfer
  (make <gi-enum>
    #:gi-name "GITransfer"
    #:scm-name "gi-transfer"
    #:enum-set '(nothing
                 container
                 everything)))
