;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/AO
;;;Contents: Libao binding backend
;;;Date: Thu May 21, 2015
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
(library (vicare multimedia ao)
  (foreign-library "vicare-ao")
  (export

    ;; version numbers and strings
    vicare-ao-version-interface-current
    vicare-ao-version-interface-revision
    vicare-ao-version-interface-age
    vicare-ao-version

    ;; initialisation and shutdown
    ao-initialize
    (rename (ao-initialize ao-initialise))
    ao-shutdown

    ;; device options
    ao-option			ao-option?
    ao-option?/alive		$ao-option-alive?
    ao-option-custom-destructor	set-ao-option-custom-destructor!
    ao-option-putprop		ao-option-getprop
    ao-option-remprop		ao-option-property-list
    ao-option-hash

    ao-append-option		ao-free-options
    ao-option->alist
    ao-append-global-option

    ao-info			ao-info?
    ao-info-type
    ao-info-name
    ao-info-short-name
    ao-info-comment
    ao-info-preferred-byte-format
    ao-info-priority
    ao-info-options

    ;; device setup/playback/teardown
    ao-open-live
    ao-open-file
    ao-play
    ao-close

    ;; driver information
    ao-driver-id
    ao-default-driver-id
    ao-driver-info
    ao-driver-info-list
    ao-file-extension

    ;; miscellaneous
    ao-is-big-endian)
  (import (vicare)
    (vicare multimedia ao constants)
    (prefix (vicare multimedia ao unsafe-capi) capi.)
    (prefix (vicare ffi) ffi.)
    (prefix (vicare ffi foreign-pointer-wrapper) ffi.)
    (vicare arguments validation)
    (vicare arguments general-c-buffers)
    (prefix (vicare platform words) words.))


;;;; version functions

(define (vicare-ao-version-interface-current)
  (capi.vicare-ao-version-interface-current))

(define (vicare-ao-version-interface-revision)
  (capi.vicare-ao-version-interface-revision))

(define (vicare-ao-version-interface-age)
  (capi.vicare-ao-version-interface-age))

(define (vicare-ao-version)
  (ascii->string (capi.vicare-ao-version)))


;;;; initialisation and shutdown

(define* (ao-initialize)
  (capi.ao-initialize))

(define* (ao-shutdown)
  (capi.ao-shutdown))


;;;; device options

(ffi.define-foreign-pointer-wrapper ao-option
  (ffi.foreign-destructor capi.ao-free-options)
  (ffi.collector-struct-type #f))

(module ()
  (set-rtd-printer! (type-descriptor ao-option)
    (lambda (S port sub-printer)
      (define-syntax-rule (%display thing)
	(display thing port))
      (define-syntax-rule (%write thing)
	(write thing port))
      (%display "#[ao-option")
      (%display " pointer=")	(%display ($ao-option-pointer S))
      (%display "]"))))

(define* (ao-free-options {opt ao-option?})
  ($ao-option-finalise opt))

(define* (ao-append-option {opt (or not ao-option?/alive)} {key general-c-string?} {val general-c-string?})
  ;;When OPT is false: allocate a new  linked list of "ao_option" C structs and store
  ;;it in a newly allocated "ao-struct" Scheme struct; return the Scheme struct.
  ;;
  ;;When OPT  is a live "ao-option"  Scheme struct: append  a new node to  the linked
  ;;list of "ao_option" C structs and return OPT itself.
  ;;
  (with-general-c-strings
      ((key^ key)
       (val^ val))
    (cond ((capi.ao-append-option opt key^ val^)
	   => (lambda (rv)
		;;RV is a pointer object referencing the linked list of "ao_option" C
		;;structs.
		(or opt (make-ao-option/owner rv))))
	  (else
	   (error __who__ "unable to create alpha object")))))

(define* (ao-option->alist {opt ao-option?/alive})
  (map (lambda (entry)
	 (cons (ascii->string (car entry))
	       (ascii->string (cdr entry))))
    (capi.ao-option->alist opt)))

(define* (ao-append-global-option {key general-c-string?} {val general-c-string?})
  (with-general-c-strings
      ((key^ key)
       (val^ val))
    (capi.ao-append-global-option key^ val^)))


;;;; devicae playback and teardown

(define* (ao-open-live ctx)
  (capi.ao-open-live))

(define* (ao-open-file ctx)
  (capi.ao-open-file))

(define* (ao-play ctx)
  (capi.ao-play))

(define* (ao-close ctx)
  (capi.ao-close))


;;;; driver information

(define-struct ao-info
  (type
		;One of the exact integers: AO_TYPE_LIVE, AO_TYPE_FILE.
   name
		;A Scheme string representing the full name of the driver.
   short-name
		;A Scheme string representing the short name of the driver.
   comment
		;A Scheme string representing the driver description.
   preferred-byte-format
		;An exact  integer specifying  the preferred  ordering of  the sample
		;bytes.  Using  the driver with  this byte format usually  results in
		;slightly less  memory usage  and slightly less  CPU usage  because a
		;swap buffer will not be needed.
   priority
		;A positive exact integer ranking how likely it is for this driver to
		;be the  default.  The  default driver will  be a  functioning driver
		;with highest priority.
   options
		;A list of  strings representing the list of option  keys accepted by
		;this driver.
   ))

(module ()
  (set-rtd-printer! (type-descriptor ao-info)
    (lambda (S port sub-printer)
      (define-syntax-rule (%display thing)
	(display thing port))
      (define-syntax-rule (%write thing)
	(write thing port))
      (%display "#[ao-info")
      (%display " type=")		(%display (if (= (ao-info-type S) AO_TYPE_LIVE) 'AO_TYPE_LIVE 'AO_TYPE_FILE))
      (%display " name=")		(%write (ao-info-name S))
      (%display " short-name=")		(%write (ao-info-short-name S))
      (%display " comment=")		(%write (ao-info-comment S))
      (%display " preferred-byte-format=") (%display (ao-info-preferred-byte-format S))
      (%display " priority=")		(%display (ao-info-priority S))
      (%display " options=")		(%write (ao-info-options S))
      (%display "]"))))

(define* (ao-driver-id {short-name general-c-string?})
  (with-general-c-strings
      ((short-name^	short-name))
    (let ((rv (capi.ao-driver-id short-name^)))
      (if (positive? rv)
	  rv
	#f))))

(define* (ao-default-driver-id)
  (capi.ao-default-driver-id))

(define* (ao-driver-info id)
  (receive-and-return (rv)
      (capi.ao-driver-info id (struct-type-descriptor ao-info))
    (when rv
      (%normalise-ao-info! rv))))

(define* (ao-driver-info-list)
  (receive-and-return (L)
      (capi.ao-driver-info-list (struct-type-descriptor ao-info))
    (for-each %normalise-ao-info! L)))

(define (%normalise-ao-info! info)
  (set-ao-info-name!       info (ascii->string     (ao-info-name       info)))
  (set-ao-info-short-name! info (ascii->string     (ao-info-short-name info)))
  (set-ao-info-comment!    info (ascii->string     (ao-info-comment    info)))
  (set-ao-info-options!    info (map ascii->string (ao-info-options    info))))

(define* (ao-file-extension {id (and words.signed-int? positive?)})
  (cond ((capi.ao-file-extension id)
	 => ascii->string)
	(else #f)))


;;;; miscellaneous

(define* (ao-is-big-endian ctx)
  (capi.ao-is-big-endian))


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; eval: (put 'ffi.define-foreign-pointer-wrapper 'scheme-indent-function 1)
;; End:
