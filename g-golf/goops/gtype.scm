;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2018
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


(define-module (g-golf goops gtype)
  #:use-module (oop goops)
  #:use-module (g-golf support utils)
  #:use-module (g-golf support g-export)
  #:use-module (g-golf support goops)
  #:use-module (g-golf gi utils)
  #:use-module (g-golf gi base-info)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)
  
  #:export     (<gtype-class>
                <gtype-instance>))


(g-export !info
          !namespace
          #;!gtype-id
          #;!gtype-name
          #;!scm-name
          #;!class-name
          
          !ginst-ptr)


;;;
;;; 
;;;

;; The metaclass of all GType classes.

(define-class <gtype-class> (<class>)
  (info #:accessor !info #:init-keyword #:info #:init-value #f)
  (namespace #:accessor !namespace
	     #:allocation #:virtual
	     #:slot-ref (lambda (self)
		          (g-base-info-get-namespace (!info self)))
	     #:slot-set! (lambda (self value)
		           (values))))

  
(define-method (initialize (self <gtype-class>) initargs)
  (let ((info (or (get-keyword #:info initargs #f)
                  (error "Missing #:info initarg: " initargs))))
    (next-method)))


;; The root class of all instantiatable GType classes.

(define-class <gtype-instance> ()
  (ginst-ptr #:accessor ginst-ptr #:init-keyword #:ginst-ptr #:init-value #f)
  #:info #t
  #:metaclass <gtype-class>)

(define-method (initialize (self <gtype-instance>) initargs)
  (next-method)
  #;(dimfi "<gtype-class> initialize: " self " - Initargs: "initargs))
