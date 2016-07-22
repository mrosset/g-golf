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

(eval-when (expand load eval)
  (use-modules (oop goops))
  (default-duplicate-binding-handler
    '(merge-generics replace warn-override-core warn last)))

(define-module (golf gi)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (golf support modules)
  #:use-module (golf support goops)
  #:use-module (golf support g-export)
  #:use-module (golf support utils)
  #:use-module (golf support enum)
  #:use-module (golf support push)
  #:use-module (golf support keyword)
  #:use-module (golf gi init)
  #:use-module (golf gi glib mem-alloc)
  #:use-module (golf gi gobject type-info)
  #:use-module (golf gi gobject enum-flags)
  #:use-module (golf gi utils)
  #:use-module (golf gi types)
  #:use-module (golf gi repository)
  #:use-module (golf gi base-info)
  #:use-module (golf gi callable-info)
  #:use-module (golf gi registered-type-info)
  #:use-module (golf gi enum-info)
  #:use-module (golf gi arg-info)
  #:use-module (golf gi type-info))

(eval-when (expand load eval)
  (re-export-public-interface (oop goops)
			      (ice-9 binary-ports)
			      (rnrs bytevectors)
			      (system foreign)
			      (golf support goops)
			      (golf support g-export)
			      (golf support utils)
			      (golf support enum)
			      (golf support push)
			      (golf support keyword)
			      (golf gi init)
			      (golf gi glib mem-alloc)
			      (golf gi gobject type-info)
			      (golf gi gobject enum-flags)
			      (golf gi utils)
			      (golf gi types)
			      (golf gi repository)
  			      (golf gi base-info)
			      (golf gi callable-info)
			      (golf gi registered-type-info)
			      (golf gi enum-info)
			      (golf gi arg-info)
			      (golf gi type-info)
			      (golf gi typelib)))
