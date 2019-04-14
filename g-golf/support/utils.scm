;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2016 - 2019
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

;; this file is a copy of (grip utils)
;; http://www.nongnu.org/grip/

;;; Code:


(define-module (g-golf support utils)
  #:use-module (system foreign)

  #:export (storage-get
	    storage-set
	    displayln
	    dimfi
	    warning
	    abort
	    and-l
	    identities
            g-studly-caps-expand
	    %g-name-transform-exceptions
	    g-name->scm-name
	    g-name->class-name
	    #;gi-class-name->method-name
            gi-type-tag->ffi))


(define storage-get #f)
(define storage-set #f)

(let ((storage (list)))
  (set! storage-get
	(lambda (key)
	  (case key
	    ((all) storage)
	    (else
	     (assq-ref storage key)))))
  (set! storage-set
	(lambda (key value)
	  (set! storage
		(assq-set! storage key value)))))


(define* (displayln msg #:optional (port #f))
  (if port
      (begin
	(display msg port)
	(newline port))
      (begin
	(display msg)
	(newline))))

(define (dimfi . items)
  ;; if the first item is a port, we use it.
  (if (port? (car items))
      (let ((p (car items)))
	(display ";; " p)
	(for-each (lambda (item)
		    (display item p) (display " " p))
	    (cdr items))
	(display "\n" p))
      (begin
	(display ";; ")
	(for-each (lambda (item)
		    (display item) (display " " ))
	    items)
	(display "\n")))
  (car (last-pair items)))

(define* (warning what msg port #:key (msg-2 #f))
  (display (string-append "Warning: " what ": " msg) port)
  (newline port)
  (when msg-2
    (display msg-2 port)
    (newline port)))

(define* (abort what msg port #:key (msg-2 #f) (code -1))
  (display (string-append "ERROR: " what ": " msg) port)
  (newline port)
  (when msg-2
    (display msg-2 port)
    (newline port))
  (exit code))

(define (and-l ll)
  (if (null? ll)
      #t
      (if (car ll)
	  (and-l (cdr ll))
	  #f)))

(define (identities . args)
  args)


;;;
;;; Name Transformation
;;;

;; Based on Guile-Gnome (gobject gw utils)

(define (g-studly-caps-expand nstr)
  ;; GStudlyCapsExpand
  (do ((idx (- (string-length nstr) 1)
	    (- idx 1)))
      ((> 1 idx)
       (string-downcase nstr))
    (cond ((and (> idx 2)
                (char-lower-case? (string-ref nstr (- idx 3)))
                (char-upper-case? (string-ref nstr (- idx 2)))
                (char-upper-case? (string-ref nstr (- idx 1)))
                (char-lower-case? (string-ref nstr idx)))
           (set! idx (- idx 1))
           (set! nstr
                 (string-append (substring nstr 0 (- idx 1))
                                "-"
                                (substring nstr (- idx 1)
                                           (string-length nstr)))))
          ((and (> idx 1)
                (char-upper-case? (string-ref nstr (- idx 1)))
                (char-lower-case? (string-ref nstr idx)))
           (set! nstr
                 (string-append (substring nstr 0 (- idx 1))
                                "-"
                                (substring nstr (- idx 1)
                                           (string-length nstr)))))
          ((and (char-lower-case? (string-ref nstr (- idx 1)))
                (char-upper-case? (string-ref nstr idx)))
           (set! nstr
                 (string-append (substring nstr 0 idx)
                                "-"
                                (substring nstr idx
                                           (string-length nstr))))))))

;; Default name transformations can be overridden, but g-golf won't
;; define exceptions for now, let's see.
(define %g-name-transform-exceptions
  '(("BLuefox" . "bluefox") ;; to test
    ;; ("GEnum" . "genum")  ;; no sure yet
    ))

(define (g-name->scm-name type-name)
  (or (assoc-ref %g-name-transform-exceptions type-name)
      (string-trim-right (g-studly-caps-expand
			  ;; only change _ to -
			  ;; other chars are not valid in a type name
			  (string-map (lambda (c) (if (eq? c #\_) #\- c))
				      type-name))
			 #\-)))

;; "GtkAccelGroup" => <gtk-accel-group>
;; "GSource*" => <g-source*>
(define (g-name->class-name type-name)
  (string->symbol (string-append "<"
				 (g-studly-caps-expand type-name)
				 ">")))

;; Not sure this is used but let's keep it as well
#;(define (gi-class-name->method-name class-name name)
  (let ((class-string (symbol->string class-name)))
    (string->symbol
     (string-append (substring class-string 1 (1- (string-length class-string)))
                    ":" (symbol->string name)))))

(define (gi-type-tag->ffi type-tag)
  (case type-tag
    ((void) void)
    ((boolean) int)
    ((int8
      uint8
      int16
      uint16
      int32
      uint32
      int64
      uint64
      float
      double)
     (eval type-tag (current-module)))
    ((gtype) unsigned-long)
    ((utf8
      filename
      array
      interface
      glist
      gslist
      ghash
      error)
     '*) ;; pointer
    ((unichar) uint32)
    (else
     (error "No such GI type tag: " type-tag))))
