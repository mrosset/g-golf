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


(define-module (gbank gi repository)
  #:use-module (system foreign)
  #:use-module (gbank gi init)
  #:use-module (gbank gi utils)

  #:export (gbank-ir-get-default
	    gbank-ir-get-dependencies
	    gbank-ir-get-loaded-namespaces
	    gbank-ir-get-n-infos
	    gbank-ir-get-info
	    gbank-ir-get-typelib-path
	    gbank-ir-require
	    gbank-ir-get-c-prefix
	    gbank-ir-get-shared-library
	    gbank-ir-get-version
	    gbank-ir-find-by-gtype
	    gbank-ir-find-by-name))


;;;
;;; Low level API
;;;

(define (gbank-ir-get-default)
  (g-irepository-get-default))
  
(define* (gbank-ir-get-dependencies namespace
				    #:key (repository %null-pointer))
  (gbank-gtype->scm (g-irepository-get-dependencies repository
						    (string->pointer namespace))
		    'gchar**))

(define* (gbank-ir-get-loaded-namespaces #:key (repository %null-pointer))
  (gbank-gtype->scm (g-irepository-get-loaded-namespaces repository)
		    'gchar**))

(define* (gbank-ir-get-n-infos namespace
			       #:key (repository %null-pointer))
  (g-irepository-get-n-infos repository
			     (string->pointer namespace)))

(define* (gbank-ir-get-info namespace index
			    #:key (repository %null-pointer))
  (g-irepository-get-info repository
			  (string->pointer namespace)
			  index))

(define* (gbank-ir-get-typelib-path namespace
				    #:key (repository %null-pointer))
  (let ((pointer (g-irepository-get-typelib-path repository
						 (string->pointer namespace))))
    (if (null-pointer? pointer)
	#f
	(gbank-gtype->scm pointer 'gchar*))))

(define* (gbank-ir-require namespace
			   #:key (version #f) (repository %null-pointer))
  (with-gerror g-error
	       (g-irepository-require repository
				      (string->pointer namespace)
				      (if version
					  (string->pointer version)
					  %null-pointer)
				      0
				      g-error)))

(define* (gbank-ir-get-c-prefix namespace
				#:key (repository %null-pointer))
  (gbank-gtype->scm (g-irepository-get-c-prefix repository
						(string->pointer namespace))
		    'gchar*))

(define* (gbank-ir-get-shared-library namespace
				      #:key (repository %null-pointer))
  (let ((pointer (g-irepository-get-shared-library repository
						   (string->pointer namespace))))
    (if (null-pointer? pointer)
	'()
	(gbank-gtype->scm pointer 'gchar*,))))

(define* (gbank-ir-get-version namespace
			       #:key (repository %null-pointer))
  (gbank-gtype->scm (g-irepository-get-version repository
					       (string->pointer namespace))
		    'gchar*))

#;(define* (gbank-ir-find-by-gtype gtype
				 #:key (repository %null-pointer))
  (let ((pointer (g-irepository-find-by-gtype repository
					      gtype)))
    (if (null-pointer? pointer)
	#f
	pointer)))

(define* (gbank-ir-find-by-gtype gtype
				 #:key (repository %null-pointer))
  (let ((pointer (g-irepository-find-by-gtype repository
					      (string->pointer gtype))))
    (if (null-pointer? pointer)
	#f
	pointer)))

(define* (gbank-ir-find-by-name namespace name
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
