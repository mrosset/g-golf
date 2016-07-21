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

(define-module (gbank gi glib)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (gbank support modules)
  #:use-module (gbank support goops)
  #:use-module (gbank support g-export)
  #:use-module (gbank support utils)
  #:use-module (gbank support enum)
  #:use-module (gbank gi init)
  #:use-module (gbank gi glib mem-alloc))

(eval-when (expand load eval)
  (re-export-public-interface (oop goops)
			      (ice-9 binary-ports)
			      (rnrs bytevectors)
			      (system foreign)
			      (gbank support goops)
			      (gbank support g-export)
			      (gbank support utils)
			      (gbank support enum)
			      (gbank gi init)
			      (gbank gi glib mem-alloc)))
