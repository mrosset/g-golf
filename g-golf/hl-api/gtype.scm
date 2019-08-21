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
            <gtype-instance>
            %g-instance-cache-default-size
            %g-instance-cache
            g-instance-cache-ref
            g-instance-cache-set!
            g-instance-cache-show))


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

(define-method (make (class <gtype-class>) . initargs)
  ;; If #:g-inst is passed, we first check if the g-inst is cached, in
  ;; which case we just return the goops instance associated with it.
  (let ((g-inst (get-keyword #:g-inst initargs #f)))
    (or (and g-inst
             (g-instance-cache-ref g-inst))
        (next-method))))

(define-method (initialize (self <gtype-instance>) initargs)
  (let ((g-inst (or (get-keyword #:g-inst initargs #f)
                    (g-instance-construct (class-of self)))))
    (receive (split-kw split-rest)
        (split-keyword-args '(#:g-inst) initargs)
      (set! (!g-inst self) g-inst)
      (next-method self split-rest)
      (g-instance-cache-set! g-inst self))))

(define (g-instance-construct class)
  (g-object-new (!gtype-id class)))

;; previous initialze core code, with its comment, which I may need in
;; another context (the code).

;; Init keywords for g-properties are cached at class creation time, see
;; (g-golg hl-api gobject).  This method needs to call next-method, but
;; without passing it any g-property init keyword, because g-property
;; slots redefine their slot-set!  method to call g-object-set-property
;; and that needs a g-inst which either does not exist yet (g-inst above
;; is #f), or it does, but we don't know that goops will set the g-inst
;; slot before any other, and therefore we cn't rely ibecause

#;(let* ((c-name (class-name (class-of self)))
       (g-props-init-kw (gi-cache-ref 'g-props-init-kw c-name)))
  (receive (split-kw split-rest)
      (split-keyword-args g-props-init-kw initargs)
    (next-method self split-rest)
    (g-instance-construct self initargs)
    (g-instance-initialize-properties self split-kw)))

;; we don't need the followig anymore, but let's keep it for now.
#;(define (g-instance-initialize-properties self g-props-init-kw)
  (for-each (lambda (slot)
              (case (slot-definition-allocation slot)
                ((#:g-property)
                 (let* ((s-name (slot-definition-name slot))
                        (i-kw (slot-definition-init-keyword slot))
                        (i-value (get-keyword i-kw g-props-init-kw)))
                   (and i-value
                        (slot-set! self s-name i-value))))))
      (class-direct-slots (class-of self))))


;;;
;;; g-instace-cache
;;;

(define %g-instance-cache-default-size 1013)

(define %g-instance-cache
  (make-hash-table %g-instance-cache-default-size))

(define (g-instance-cache-ref g-inst)
  (hashq-ref %g-instance-cache
             (pointer-address g-inst)))

(define (g-instance-cache-set! g-inst inst)
  (hashq-set! %g-instance-cache
              (pointer-address g-inst)
              inst))

(define (g-instance-cache-show)
  (hash-for-each dimfi
                 %g-instance-cache))
