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


(define-module (tests gi)
  #:use-module (oop goops)
  #:use-module (unit-test)
  #:use-module (g-golf gi))


(g-golf-ir-require "Clutter")
(g-golf-ir-require "Gtk")

(define-class <g-golf-test-gi> (<test-case>))


(define-method (test-utils (self <g-golf-test-gi>))
  (assert-equal "clutter-actor"
		(g-golf-gtype-name->scm-name "ClutterActor"))
  (assert-equal '<clutter-actor>
		(g-golf-gtype-name->class-name "ClutterActor")))


(let* ((actor (g-golf-ir-find-by-name "Clutter" "Actor"))
       (property (g-golf-oi-get-property actor 5))
       (property-flags (g-golf-pi-get-flags property))
       (align-info (g-golf-ir-find-by-name "Clutter" "ActorAlign")))

  (define-method (test-repository (self <g-golf-test-gi>))
    (assert-true actor)
    (assert-true align-info)
    (assert-exception  (g-golf-ir-require "ClutterBlue"))
    (assert-equal #f
		  (g-golf-ir-find-by-name "Gtk" "button"))
    (assert-true (g-golf-ir-find-by-name "Gtk" "Button"))
    (assert-true (g-golf-ir-get-version "Gtk")))

  (define-method (test-registered-type-info (self <g-golf-test-gi>))
    (assert-true (g-golf-rt-get-g-type align-info)))

  (define-method (test-enum-info (self <g-golf-test-gi>))
    (assert-true (g-golf-ei-get-n-values align-info))
    (assert-true (g-golf-ei-get-value align-info 0))
    (assert-true (g-golf-enum-import align-info)))

  (define-method (test-property (self <g-golf-test-gi>))
    (assert-true property)
    (assert-equal "background-color"
		  (g-golf-bi-get-name property))
    (assert-numeric-= 3 0 property-flags)
    (assert-equal '(readable writable)
		  (g-golf-integer->gflags %g-golf-go-g-param-flags
					  property-flags))))

(define-method (test-type-lib (self <g-golf-test-gi>))
  (let ((filename (g-golf-ir-get-typelib-path "Clutter")))
    (assert-equal "Clutter"
		  (call-with-input-typelib filename
					   (lambda (typelib)
					     (g-golf-tl-get-name-space typelib))))))


(exit-with-summary (run-all-defined-test-cases))
