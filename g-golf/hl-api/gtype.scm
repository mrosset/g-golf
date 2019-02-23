;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2018 - 2019
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

;; This code is largely inspired by the Guile-Gnome module (gnome
;; gobject gtype), see:

;;   https://www.gnu.org/software/guile-gnome

;;   http://git.savannah.gnu.org/cgit/guile-gnome.git
;;     tree/glib/gnome/gobject/gtype.scm

;;; Code:


(define-module (g-golf hl-api gtype)
  #:use-module (oop goops)
  #:use-module (g-golf support)
  #:use-module (g-golf gi)
  #:use-module (g-golf glib)
  #:use-module (g-golf gobject)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)
  
  #:export     (<gtype-class>
                <gtype-instance>))


(g-export !info
          !namespace
          !gtype-id
          !gtype-name
          !scm-name
          
          !g-inst)


;;;
;;; 
;;;

;; The metaclass of all GType classes.

(define-class <gtype-class> (<class>)
  (info #:accessor !info
        #:init-keyword #:info)
  (namespace #:accessor !namespace
	     #:allocation #:virtual
	     #:slot-ref (lambda (self)
                          (and (not (boolean? (!info self)))
		               (g-base-info-get-namespace (!info self))))
	     #:slot-set! (lambda (self value)
		           (values)))
  (gtype-id #:accessor !gtype-id
	    #:allocation #:virtual
	    #:slot-ref (lambda (self)
                          (and (not (boolean? (!info self)))
		               (g-registered-type-info-get-g-type (!info self))))
	    #:slot-set! (lambda (self value)
		          (values)))
  (gtype-name #:accessor !gtype-name
	      #:allocation #:virtual
	      #:slot-ref (lambda (self)
                           (and (not (boolean? (!info self)))
		                (g-object-info-get-type-name (!info self))))
	      #:slot-set! (lambda (self value)
		            (values)))
  (scm-name #:accessor !scm-name
	    #:allocation #:virtual
	    #:slot-ref (lambda (self)
                         (and (not (boolean? (!info self)))
		              (g-name->scm-name (!gtype-name self))))
	    #:slot-set! (lambda (self value)
		          (values))))


(define-method (initialize (self <gtype-class>) initargs)
  (let ((info (or (get-keyword #:info initargs #f)
                  (error "Missing #:info initarg: " initargs))))
    (next-method)))


;; The root class of all instantiatable GType classes.

(define-class <gtype-instance> ()
  (g-inst #:accessor !g-inst)
  #:info #t
  #:metaclass <gtype-class>)

(define-method (initialize (self <gtype-instance>) initargs)
  (next-method)
  (gtype-instance-construct self initargs))

(define (gtype-instance-construct self initargs)
  ;; FIXME: this is largely incomplete, we obviously need to set the
  ;; instance properties (initargs ...)
  (let* ((class (class-of self))
         (gtype-id (!gtype-id class)))
    (set! (!g-inst self)
          (g-object-new gtype-id))))
