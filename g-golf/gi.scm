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

(define-module (g-golf gi)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (g-golf support modules)
  #:use-module (g-golf support goops)
  #:use-module (g-golf support g-export)
  #:use-module (g-golf support utils)
  #:use-module (g-golf support enum)
  #:use-module (g-golf support push)
  #:use-module (g-golf support keyword)
  #:use-module (g-golf gi init)
  #:use-module (g-golf gi glib mem-alloc)
  #:use-module (g-golf gi gobject type-info)
  #:use-module (g-golf gi gobject enum-flags)
  #:use-module (g-golf gi utils)
  #:use-module (g-golf gi types)
  #:use-module (g-golf gi repository)
  #:use-module (g-golf gi base-info)
  #:use-module (g-golf gi callable-info)
  #:use-module (g-golf gi registered-type-info)
  #:use-module (g-golf gi enum-info)
  #:use-module (g-golf gi arg-info)
  #:use-module (g-golf gi type-info))

(eval-when (expand load eval)
  (re-export-public-interface (oop goops)
			      (ice-9 binary-ports)
			      (rnrs bytevectors)
			      (system foreign)
			      (g-golf support goops)
			      (g-golf support g-export)
			      (g-golf support utils)
			      (g-golf support enum)
			      (g-golf support push)
			      (g-golf support keyword)
			      (g-golf gi init)
			      (g-golf gi glib mem-alloc)
			      (g-golf gi gobject type-info)
			      (g-golf gi gobject enum-flags)
			      (g-golf gi utils)
			      (g-golf gi types)
			      (g-golf gi repository)
  			      (g-golf gi base-info)
			      (g-golf gi callable-info)
			      (g-golf gi registered-type-info)
			      (g-golf gi enum-info)
			      (g-golf gi arg-info)
			      (g-golf gi type-info)
			      (g-golf gi typelib)))
