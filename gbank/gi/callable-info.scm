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


(define-module (gbank gi callable-info)
  #:use-module (system foreign)
  #:use-module (gbank support enum)
  #:use-module (gbank gi init)
  #:use-module (gbank gi utils)
  #:use-module (gbank gi types)

  #:export (gbank-ci-get-n-args
	    gbank-ci-get-arg
	    gbank-ci-get-caller-owns
	    gbank-ci-get-return-type
	    gbank-ci-may-return-null))


;;;
;;; Low level API
;;;

(define (gbank-ci-get-n-args info)
  (g-callable-info-get-n-args info))

(define (gbank-ci-get-arg info n)
  (g-callable-info-get-arg info n))

(define (gbank-ci-get-caller-owns info)
  (e-sym %gbank-ai-transfer
	 (g-callable-info-get-caller-owns info)))

(define (gbank-ci-get-return-type info)
  (g-callable-info-get-return-type info))

(define (gbank-ci-may-return-null info)
  (gbank-gtype->scm (g-callable-info-may-return-null info)
		    'gboolean))


;;;
;;; GI Bindings
;;;

(define g-callable-info-get-n-args
  (pointer->procedure int
                      (dynamic-func "g_callable_info_get_n_args"
				    %libgirepository)
                      (list '*)))

(define g-callable-info-get-arg
  (pointer->procedure '*
                      (dynamic-func "g_callable_info_get_arg"
				    %libgirepository)
                      (list '* int)))

(define g-callable-info-get-caller-owns
  (pointer->procedure int
                      (dynamic-func "g_callable_info_get_caller_owns"
				    %libgirepository)
                      (list '*)))

(define g-callable-info-get-return-type
  (pointer->procedure '*
                      (dynamic-func "g_callable_info_get_return_type"
				    %libgirepository)
                      (list '*)))
					
(define g-callable-info-may-return-null
  (pointer->procedure int
                      (dynamic-func "g_callable_info_may_return_null"
				    %libgirepository)
                      (list '*)))
