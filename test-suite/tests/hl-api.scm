;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2019
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


(define-module (tests hl-api)
  #:use-module (oop goops)
  #:use-module (unit-test)
  #:use-module (g-golf))


(g-irepository-require "Clutter")


(define %grid-layout-info
  (g-irepository-find-by-name "Clutter" "GridLayout"))

(gi-import-object %grid-layout-info)

(define-class <g-golf-test-hl-api> (<test-case>))


(define-method (test-g-property-accessor (self <g-golf-test-hl-api>))
  (let ((a-grid (make <clutter-grid-layout>)))
    (assert-true (eq? (!orientation a-grid) 'horizontal))
    (assert (set! (!orientation a-grid) 'vertical))
    (assert-true (eq? (!orientation a-grid) 'vertical))))


(define-method (test-accessor-inheritance (self <g-golf-test-hl-api>))
  (let* ((module (resolve-module '(tests hl-api)))
         (c-name '<foo>)
         (c-inst (make-class (list <clutter-grid-layout>)
                             '()
                             #:name c-name))
         (dummy (module-define! module c-name c-inst))
         (a-foo (make <foo>)))
    (assert-true (eq? (!orientation a-foo) 'horizontal))
    (assert (set! (!orientation a-foo) 'vertical))
    (assert-true (eq? (!orientation a-foo) 'vertical))))


(exit-with-summary (run-all-defined-test-cases))
