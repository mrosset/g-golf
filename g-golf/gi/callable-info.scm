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


(define-module (golf gi callable-info)
  #:use-module (system foreign)
  #:use-module (golf support enum)
  #:use-module (golf gi init)
  #:use-module (golf gi utils)
  #:use-module (golf gi types)

  #:export (golf-ci-get-n-args
	    golf-ci-get-arg
	    golf-ci-get-caller-owns
	    golf-ci-get-return-type
	    golf-ci-may-return-null))


;;;
;;; Low level API
;;;

(define (golf-ci-get-n-args info)
  (g-callable-info-get-n-args info))

(define (golf-ci-get-arg info n)
  (g-callable-info-get-arg info n))

(define (golf-ci-get-caller-owns info)
  (e-sym %golf-ai-transfer
	 (g-callable-info-get-caller-owns info)))

(define (golf-ci-get-return-type info)
  (g-callable-info-get-return-type info))

(define (golf-ci-may-return-null info)
  (golf-gtype->scm (g-callable-info-may-return-null info)
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
