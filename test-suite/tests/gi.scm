;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2016 - 2018
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


(define-class <g-golf-test-gi> (<test-case>))


;;;
;;; Utils
;;;

(define-method (test-utils (self <g-golf-test-gi>))
  (assert-equal "clutter-actor"
		(g-golf-gtype-name->scm-name "ClutterActor"))
  (assert-equal '<clutter-actor>
		(g-golf-gtype-name->class-name "ClutterActor")))


;;;
;;; Repository
;;;

(define-method (test-repository (self <g-golf-test-gi>))
  (assert-true (g-golf-ir-require "Clutter"))
  (assert-true (g-golf-ir-require "Gtk"))
  (assert-true (g-golf-ir-get-typelib-path "Clutter"))
  (assert-true (g-golf-ir-find-by-name "Clutter" "Actor"))
  (assert-true (g-golf-ir-find-by-name "Clutter" "ActorAlign"))
  (assert-exception (g-golf-ir-require "ClutterBlue"))
  (assert-false (g-golf-ir-find-by-name "Gtk" "button"))
  (assert-true (g-golf-ir-find-by-name "Gtk" "Button"))
  (assert-true (g-golf-ir-get-version "Gtk")))


;;;
;;; Base Info
;;;

(define-method (test-base-info (self <g-golf-test-gi>))
  (assert-true (g-golf-bi-get-name (g-golf-ir-find-by-name "Clutter" "Actor"))))


;;;
;;; Callable Info
;;;


;;;
;;; Function Info
;;;


;;;
;;; Registered Type Info
;;;

(define-method (test-registered-type-info (self <g-golf-test-gi>))
  (let ((align-info (g-golf-ir-find-by-name "Clutter" "ActorAlign")))
    (assert-true (g-golf-rt-get-g-type align-info))))


;;;
;;; Enum Info
;;;

(define-method (test-enum-info (self <g-golf-test-gi>))
  (let ((align-info (g-golf-ir-find-by-name "Clutter" "ActorAlign")))
    (assert-true (g-golf-ei-get-n-values align-info))
    (assert-true (g-golf-ei-get-value align-info 0))
    (assert-true (g-golf-enum-import align-info))))


;;;
;;; Object Info
;;;

(define-method (test-object-info (self <g-golf-test-gi>))
  (let ((actor (g-golf-ir-find-by-name "Clutter" "Actor")))
    (g-golf-oi-get-property actor 5)))


;;;
;;; Arg Info
;;;


;;;
;;; Property Info
;;;

(define-method (test-property-info (self <g-golf-test-gi>))
  (let* ((actor (g-golf-ir-find-by-name "Clutter" "Actor"))
         (property (g-golf-oi-get-property actor 5)))
    (assert-true (g-golf-pi-get-flags property))
    (assert-equal "background-color"
		  (g-golf-bi-get-name property))
    (assert-numeric-= 3 0
                      (g-golf-pi-get-flags property))
    (assert-equal '(readable writable)
		  (g-golf-integer->gflags %g-golf-go-g-param-flags
					  (g-golf-pi-get-flags property)))))


;;;
;;; Typelib
;;;

(define-method (test-type-lib (self <g-golf-test-gi>))
  (let ((filename (g-golf-ir-get-typelib-path "Clutter")))
    (assert-equal "Clutter"
		  (call-with-input-typelib filename
					   (lambda (typelib)
					     (g-golf-tl-get-name-space typelib))))))


(exit-with-summary (run-all-defined-test-cases))
