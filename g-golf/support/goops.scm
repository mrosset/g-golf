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

;; this file is a copy of (grip goops)
;; http://www.nongnu.org/grip/

;;; Code:


(define-module (g-golf support goops)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (g-golf support modules)

  #:export (class-direct-virtual-slots
	    class-virtual-slots
	    describe))


(eval-when (expand load eval)
  (re-export-public-interface (oop goops)))


(define-method (class-direct-virtual-slots (c <class>))
  (filter-map (lambda (slot-definition)
		(and (eq? (slot-definition-allocation slot-definition)
			  #:virtual)
		     slot-definition))
      (class-direct-slots c)))

(define-method (class-virtual-slots (c <class>))
  (filter-map (lambda (slot-definition)
		(and (eq? (slot-definition-allocation slot-definition)
			  #:virtual)
		     slot-definition))
      (class-slots c)))

(define-method (describe (self <object>))
  (format #t "~S - instance of ~A~%"
	  self
	  (class-name (class-of self)))
  (format #t "  slots and values are:~%")
  (for-each (lambda (slot)
	      (let ((name (slot-definition-name slot)))
		(format #t "    ~S = ~A~%"
			name
			(if (slot-bound? self name) 
			    (format #f "~S" (slot-ref self name))
			    "#<unbound>"))))
	    (class-slots (class-of self)))
  *unspecified*)
