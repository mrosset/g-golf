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
  #:use-module (g-golf gi))


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


(define-method (test-main-loop (self <g-golf-test-glib>))
  (assert (g-main-loop-new #f #f))
  (assert (g-idle-source-new)))


(exit-with-summary (run-all-defined-test-cases))
