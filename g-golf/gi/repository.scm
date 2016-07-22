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


(define-module (golf gi repository)
  #:use-module (system foreign)
  #:use-module (golf gi init)
  #:use-module (golf gi utils)

  #:export (golf-ir-get-default
	    golf-ir-get-dependencies
	    golf-ir-get-loaded-namespaces
	    golf-ir-get-n-infos
	    golf-ir-get-info
	    golf-ir-get-typelib-path
	    golf-ir-require
	    golf-ir-get-c-prefix
	    golf-ir-get-shared-library
	    golf-ir-get-version
	    golf-ir-find-by-gtype
	    golf-ir-find-by-name))


;;;
;;; Low level API
;;;

(define (golf-ir-get-default)
  (g-irepository-get-default))
  
(define* (golf-ir-get-dependencies namespace
				    #:key (repository %null-pointer))
  (golf-gtype->scm (g-irepository-get-dependencies repository
						    (string->pointer namespace))
		    'gchar**))

(define* (golf-ir-get-loaded-namespaces #:key (repository %null-pointer))
  (golf-gtype->scm (g-irepository-get-loaded-namespaces repository)
		    'gchar**))

(define* (golf-ir-get-n-infos namespace
			       #:key (repository %null-pointer))
  (g-irepository-get-n-infos repository
			     (string->pointer namespace)))

(define* (golf-ir-get-info namespace index
			    #:key (repository %null-pointer))
  (g-irepository-get-info repository
			  (string->pointer namespace)
			  index))

(define* (golf-ir-get-typelib-path namespace
				    #:key (repository %null-pointer))
  (let ((pointer (g-irepository-get-typelib-path repository
						 (string->pointer namespace))))
    (if (null-pointer? pointer)
	#f
	(golf-gtype->scm pointer 'gchar*))))

(define* (golf-ir-require namespace
			   #:key (version #f) (repository %null-pointer))
  (with-gerror g-error
	       (g-irepository-require repository
				      (string->pointer namespace)
				      (if version
					  (string->pointer version)
					  %null-pointer)
				      0
				      g-error)))

(define* (golf-ir-get-c-prefix namespace
				#:key (repository %null-pointer))
  (golf-gtype->scm (g-irepository-get-c-prefix repository
						(string->pointer namespace))
		    'gchar*))

(define* (golf-ir-get-shared-library namespace
				      #:key (repository %null-pointer))
  (let ((pointer (g-irepository-get-shared-library repository
						   (string->pointer namespace))))
    (if (null-pointer? pointer)
	'()
	(golf-gtype->scm pointer 'gchar*,))))

(define* (golf-ir-get-version namespace
			       #:key (repository %null-pointer))
  (golf-gtype->scm (g-irepository-get-version repository
					       (string->pointer namespace))
		    'gchar*))

#;(define* (golf-ir-find-by-gtype gtype
				 #:key (repository %null-pointer))
  (let ((pointer (g-irepository-find-by-gtype repository
					      gtype)))
    (if (null-pointer? pointer)
	#f
	pointer)))

(define* (golf-ir-find-by-gtype gtype
				 #:key (repository %null-pointer))
  (let ((pointer (g-irepository-find-by-gtype repository
					      (string->pointer gtype))))
    (if (null-pointer? pointer)
	#f
	pointer)))

(define* (golf-ir-find-by-name namespace name
				#:key (repository %null-pointer))
  (let ((pointer (g-irepository-find-by-name repository
					     (string->pointer namespace)
					     (string->pointer name))))
    (if (null-pointer? pointer)
	#f
	pointer)))


;;;
;;; GI Bindings
;;;

(define g-irepository-get-default
  (pointer->procedure '*
                      (dynamic-func "g_irepository_get_default"
				    %libgirepository)
                      '()))

(define g-irepository-get-dependencies
  (pointer->procedure '*
                      (dynamic-func "g_irepository_get_dependencies"
				    %libgirepository)
                      (list '* '*)))

(define g-irepository-get-loaded-namespaces
  (pointer->procedure '*
                      (dynamic-func "g_irepository_get_loaded_namespaces"
				    %libgirepository)
                      (list '*)))

(define g-irepository-get-n-infos
  (pointer->procedure int
                      (dynamic-func "g_irepository_get_n_infos"
				    %libgirepository)
                      (list '* '*)))

(define g-irepository-get-info
  (pointer->procedure '*
                      (dynamic-func "g_irepository_get_info"
				    %libgirepository)
                      (list '* '* int)))

(define g-irepository-get-typelib-path
  (pointer->procedure '*
                      (dynamic-func "g_irepository_get_typelib_path"
				    %libgirepository)
                      (list '* '*)))

(define g-irepository-require
  (pointer->procedure '*
                      (dynamic-func "g_irepository_require"
				    %libgirepository)
                      (list '* '* '* int '*)))

(define g-irepository-get-c-prefix
  (pointer->procedure '*
                      (dynamic-func "g_irepository_get_c_prefix"
				    %libgirepository)
                      (list '* '*)))

(define g-irepository-get-shared-library
  (pointer->procedure '*
                      (dynamic-func "g_irepository_get_shared_library"
				    %libgirepository)
                      (list '* '*)))

(define g-irepository-get-version
  (pointer->procedure '*
                      (dynamic-func "g_irepository_get_version"
				    %libgirepository)
                      (list '* '*)))

#;(define g-irepository-find-by-gtype
  (pointer->procedure '*
                      (dynamic-func "g_irepository_find_by_gtype"
				    %libgirepository)
                      (list '* int)))

(define g-irepository-find-by-gtype
  (pointer->procedure '*
                      (dynamic-func "g_irepository_find_by_gtype"
				    %libgirepository)
                      (list '* '*)))

(define g-irepository-find-by-name
  (pointer->procedure '*
                      (dynamic-func "g_irepository_find_by_name"
				    %libgirepository)
                      (list '* '* '*)))
