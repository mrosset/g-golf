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
  #:use-module (ice-9 receive)
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
  
  #:export (<gtype-class>
            <gtype-instance>))


(g-export !info
          !derived
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
  (derived #:accessor !derived
           #:init-keyword #:derived #:init-value #f)
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
  ;; Init keywords for g-properties are cached at class creation time,
  ;; see (g-golg hl-api gobject).  This method needs to call next-method,
  ;; but without passing it any g-property init keyword, since these
  ;; g-property slots redefine their slot-set! method, which calls
  ;; g-object-set-property and that needs a g-inst which does not exist
  ;; yet.
  (let* ((c-name (class-name (class-of self)))
         (g-props-init-kw (gi-cache-ref 'g-props-init-kw c-name)))
    (receive (split-kw split-rest)
        (split-keyword-args g-props-init-kw initargs)
      (next-method self split-rest)
      (gtype-instance-construct self initargs)
      (gtype-instance-initialize-properties self split-kw))))

(define (gtype-instance-construct self initargs)
  (let* ((class (class-of self))
         (gtype-id (!gtype-id class)))
    (set! (!g-inst self)
          (g-object-new gtype-id))))

(define (gtype-instance-initialize-properties self g-props-init-kw)
  (for-each (lambda (slot)
              (case (slot-definition-allocation slot)
                ((#:g-property)
                 (let* ((s-name (slot-definition-name slot))
                        (i-kw (slot-definition-init-keyword slot))
                        (i-value (get-keyword i-kw g-props-init-kw)))
                   (and i-value
                        (slot-set! self s-name i-value))))))
      (class-direct-slots (class-of self))))
