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


(define-module (golf gi types)
  #:use-module (oop goops)  
  ;; #:use-module (golf support enum)
  #:use-module (golf gi gobject enum-flags)

  #:export (%golf-bi-info-type
	    %golf-ai-direction
	    %golf-ai-scope
	    %golf-ai-transfer
	    %golf-ct-type-tag
	    %golf-ct-array-type))


;;;
;;; Base Info
;;;

(define %golf-bi-info-type
  (make <genum>
    #:type-name "GIInfoType"
    #:scm-name "gi-info-type"
    #:value-set '(invalid
		  function
		  callback
		  struct
		  boxed
		  enum
		  flags
		  object
		  interface
		  constant
		  error-domain ;; invalid_0 - deleted
		  union
		  value
		  signal
		  vfunc
		  property
		  field
		  arg
		  type
		  unresolved)))


;;;
;;; Arg Info
;;;

(define %golf-ai-direction
  (make <genum>
    #:type-name "GIDirection"
    #:scm-name "gi-direction"
    #:value-set '(in
		  out
		  inout)))

(define %golf-ai-scope
  (make <genum>
    #:type-name "GIScopeType"
    #:scm-name "gi-scope-type"
    #:value-set '(invalid
		  call
		  async
		  notified)))

(define %golf-ai-transfer
  (make <genum>
    #:type-name "GITransfer"
    #:scm-name "gi-transfer"
    #:value-set '(nothing
		  container
		  everything)))


;;;
;;; Common Types
;;;

(define %golf-ct-type-tag
  (make <genum>
    #:type-name "GITypeTag"
    #:scm-name "gi-type-tag"
    #:value-set '(void
		  boolean
		  int8
		  uint8
		  int16
		  uint16
		  int32
		  uint32
		  int64
		  uint64
		  float
		  double
		  gtype
		  utf8
		  filename
		  array
		  interface
		  glist
		  gslist
		  ghash
		  error
		  unichar)))

(define %golf-ct-array-type
  (make <genum>
    #:type-name "GIArrayType"
    #:scm-name "gi-array-type"
    #:value-set '(c
		  array
		  ptr-array
		  byte-array)))
