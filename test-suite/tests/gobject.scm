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


(define-module (tests gobject)
  #:use-module (oop goops)
  #:use-module (unit-test)
  #:use-module (g-golf))


(g-irepository-require "Clutter")


(define %align-info (g-irepository-find-by-name "Clutter" "ActorAlign"))
(define %gtype (g-registered-type-info-get-g-type %align-info))


(define-class <g-golf-test-gobject> (<test-case>))


(define-method (test-g-value-init (self <g-golf-test-gobject>))
  (assert (g-value-init (symbol->g-type 'float))))

(define-method (test-g-value-get-int (self <g-golf-test-gobject>))
  (let ((g-value (g-value-init (symbol->g-type 'int))))
    (assert (g-value-ref g-value))))

(define-method (test-g-value-set-int (self <g-golf-test-gobject>))
  (let ((g-value (g-value-init (symbol->g-type 'int))))
    (assert (g-value-set! g-value 5))))

(define-method (test-g-value-get-float (self <g-golf-test-gobject>))
  (let ((g-value (g-value-init (symbol->g-type 'float))))
    (assert (g-value-ref g-value))))

(define-method (test-g-value-set-float (self <g-golf-test-gobject>))
  (let ((g-value (g-value-init (symbol->g-type 'float))))
    (assert (g-value-set! g-value 5.0))))

(define-method (test-g-value-get-string (self <g-golf-test-gobject>))
  (let ((g-value (g-value-init (symbol->g-type 'string))))
    (assert (g-value-ref g-value))))

(define-method (test-g-value-set-string (self <g-golf-test-gobject>))
  (let ((g-value (g-value-init (symbol->g-type 'string))))
    (assert (g-value-set! g-value ""))
    (assert (g-value-set! g-value "Hello!"))
    (assert (g-value-set! g-value "Apresentação"))))

(define-method (test-g-type-name (self <g-golf-test-gobject>))
  (assert-equal "ClutterActorAlign" (g-type-name %gtype))
  (assert-equal "gfloat" (g-type-name 56)))


(exit-with-summary (run-all-defined-test-cases))
