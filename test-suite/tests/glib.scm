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


(define-module (tests glib)
  #:use-module (oop goops)
  #:use-module (unit-test)
  #:use-module (g-golf))


#;(g-golf-ir-require "Clutter")


(define-class <g-golf-test-glib> (<test-case>))


(define-method (test-g-malloc (self <g-golf-test-glib>))
  (assert-exception (g-malloc -1.5))
  (assert-exception (g-malloc -1))
  (assert-exception (g-malloc 1.5))
  (assert-false (g-malloc -0))
  (assert-false (g-malloc 0))
  (assert (g-malloc 1)))


(define-method (test-g-malloc0 (self <g-golf-test-glib>))
  (assert-exception (g-malloc0 -1.5))
  (assert-exception (g-malloc0 -1))
  (assert-exception (g-malloc0 1.5))
  (assert-false (g-malloc0 -0))
  (assert-false (g-malloc0 0))
  (assert (g-malloc0 1)))


(define-method (test-g-free (self <g-golf-test-glib>))
  (assert-exception (g-free 10))
  (assert (g-free %null-pointer))
  (assert (g-free (g-malloc 10))))


(define-method (test-g-memdup (self <g-golf-test-glib>))
  (let ((mem (g-malloc 10)))
    (assert-exception (g-memdup mem -1.5))
    (assert-exception (g-memdup mem -1))
    (assert-exception (g-memdup mem 1.5))
    (assert-false (g-memdup %null-pointer 10))
    (assert-false (g-memdup mem -0))
    (assert-false (g-memdup mem 0))
    (assert (g-memdup mem 1))
    (assert (g-memdup mem 10))
    (assert (g-memdup mem 20))))


(define-method (test-main-loop (self <g-golf-test-glib>))
  (assert (g-main-loop-new #f #f))
  (assert (g-idle-source-new)))


(exit-with-summary (run-all-defined-test-cases))
