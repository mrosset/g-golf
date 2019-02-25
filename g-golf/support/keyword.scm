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

;; this file is a copy of (grip optargs)
;; http://www.nongnu.org/grip/

;;; Code:


(define-module (g-golf support keyword)
  :use-module (ice-9 match)
  :use-module (ice-9 receive)
  #:use-module (g-golf support push)
  
  #:export (split-keyword-args
            strip-keyword-args))


(define (split-keyword-args keywords args)
  (let loop ((args args)
             (split-kw '())
             (split-rest '()))
    (match args
      (()
       (values (reverse! split-kw) (reverse! split-rest)))
      (((? keyword? kw) arg . rest)
       (loop rest
             (if (memq kw keywords)
                 (cons* arg kw split-kw)
                 split-kw)
             (if (memq kw keywords)
                 split-rest
                 (cons* arg kw split-rest))))
      ((head . tail)
       (loop tail split-kw (cons head split-rest))))))

(define (strip-keyword-args keywords args)
  (receive (split-kw split-rest)
      (split-keyword-args keywords args)
    split-rest))
