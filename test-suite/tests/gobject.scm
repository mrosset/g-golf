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


(g-golf-ir-require "Clutter")


(define %align-info (g-golf-ir-find-by-name "Clutter" "ActorAlign"))
(define %gtype (g-golf-rt-get-g-type %align-info))


(define-class <g-golf-test-gobject> (<test-case>))


(define-method (test-gobject (self <g-golf-test-gobject>))
  (assert-equal "ClutterActorAlign" (g-type-name %gtype)))


(exit-with-summary (run-all-defined-test-cases))
