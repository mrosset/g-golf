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
  #:use-module (g-golf))


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
  (assert-true (g-irepository-require "Clutter"))
  (assert-true (g-irepository-require "Gtk"))
  (assert-true (g-irepository-get-typelib-path "Clutter"))
  (assert-true (g-irepository-find-by-name "Clutter" "Actor"))
  (assert-true (g-irepository-find-by-name "Clutter" "ActorAlign"))
  (assert-exception (g-irepository-require "ClutterBlue"))
  (assert-false (g-irepository-find-by-name "Gtk" "button"))
  (assert-true (g-irepository-find-by-name "Gtk" "Button"))
  (assert-true (g-irepository-get-version "Gtk")))


;;;
;;; Base Info
;;;

(define-method (test-base-info (self <g-golf-test-gi>))
  (assert-true (g-base-info-get-name (g-irepository-find-by-name "Clutter" "Actor"))))


;;;
;;; Callable Info
;;;


;;;
;;; Function Info
;;;

(define-method (test-function-info (self <g-golf-test-gi>))
  (let* ((actor (g-irepository-find-by-name "Clutter" "Actor"))
         (actor-m1 (g-object-info-get-method actor 0)))
    (assert-true (g-function-info-get-flags actor-m1))
    (assert-true (g-function-info-get-symbol actor-m1))))


;;;
;;; Registered Type Info
;;;

(define-method (test-registered-type-info (self <g-golf-test-gi>))
  (let ((align-info (g-irepository-find-by-name "Clutter" "ActorAlign")))
    (assert-true (g-registered-type-info-get-g-type align-info))))


;;;
;;; Enum Info
;;;

(define-method (test-enum-info (self <g-golf-test-gi>))
  (let ((align-info (g-irepository-find-by-name "Clutter" "ActorAlign")))
    (assert-true (g-enum-info-get-n-values align-info))
    (assert-true (g-enum-info-get-value align-info 0))
    (assert-true (g-golf-enum-import align-info))))


;;;
;;; Object Info
;;;

(define-method (test-object-info (self <g-golf-test-gi>))
  (let ((actor (g-irepository-find-by-name "Clutter" "Actor")))
    (assert-true (g-object-info-get-n-methods actor))
    (assert-true (g-object-info-get-method actor 0))
    (assert-true (g-object-info-get-property actor 5))))


;;;
;;; Arg Info
;;;


;;;
;;; Property Info
;;;

(define-method (test-property-info (self <g-golf-test-gi>))
  (let* ((actor (g-irepository-find-by-name "Clutter" "Actor"))
         (property (g-object-info-get-property actor 5)))
    (assert-true (g-property-info-get-type property))
    (assert-true (g-property-info-get-flags property))))


;;;
;;; Type Info
;;;

(define-method (test-type-info (self <g-golf-test-gi>))
  (let* ((actor (g-irepository-find-by-name "Clutter" "Actor"))
         (property (g-object-info-get-property actor 5))
         (type-info (g-property-info-get-type property))
         (type-tag (g-type-info-get-tag type-info))
         (interface (g-type-info-get-interface type-info))
         (i-type (g-base-info-get-type interface)))
    (assert (g-type-tag-to-string type-tag))
    (assert (g-type-tag-to-string 'interface))
    (assert-false (g-type-tag-to-string 1000))
    (assert-false (g-type-tag-to-string 'blue))
    (assert (g-info-type-to-string i-type))
    (assert (g-info-type-to-string 'struct))
    (assert-false (g-info-type-to-string 1000))
    (assert-false (g-type-tag-to-string 'fox))))


;;;
;;; Typelib
;;;

(define-method (test-type-lib (self <g-golf-test-gi>))
  (let ((filename (g-irepository-get-typelib-path "Clutter")))
    (assert-equal "Clutter"
		  (call-with-input-typelib filename
					   (lambda (typelib)
					     (g-typelib-get-name-space typelib))))))


(exit-with-summary (run-all-defined-test-cases))
