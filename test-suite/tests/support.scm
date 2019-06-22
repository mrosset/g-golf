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


(define-module (tests support)
  #:use-module (oop goops)
  #:use-module (unit-test)
  #:use-module (g-golf))


(define-class <g-golf-test-support> (<test-case>))


;;;
;;; Goops
;;;

(define-class <a> () (name) (val))

(define-method (test-mslot-set! (self <g-golf-test-support>))
  (let ((inst (make <a>)))
    (assert (mslot-set! inst
                        'name 'alice
                        'val 7))
    (assert-exception (mslot-set! inst
                                  'name 'alice
                                  'val))))


;;;
;;; Enum
;;;

(define-method (test-enum (self <g-golf-test-support>))
  (let* ((a-set '((foo . 0) (bar  . 1)))
         (enum (make <enum> #:enum-set a-set)))
    (assert-exception (make <enum>))
    (assert-true (enum->value enum 'foo))
    (assert-true (enum->value enum 'bar))
    (assert-false (enum->value enum 'baz))
    (assert-true (enum->symbol enum 0))
    (assert-true (enum->symbol enum 1))
    (assert-false (enum->symbol enum 2))
    (assert-true (enum->name enum 'foo))
    (assert-true (enum->name enum 'bar))
    (assert-false (enum->name enum 'baz))
    (assert-true (enum->name enum 0))
    (assert-true (enum->name enum 1))
    (assert-false (enum->name enum 2))
    (assert (enum->names enum))))

(exit-with-summary (run-all-defined-test-cases))
