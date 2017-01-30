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
;;;Copyright (C) 2015, 2017 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (options typed-language)
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
    ao-option?/alive
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

    ;; device playback/teardown
    ao-device			ao-device?
    ao-device?/alive
    ao-device-custom-destructor	set-ao-device-custom-destructor!
    ao-device-putprop		ao-device-getprop
    ao-device-remprop		ao-device-property-list
    ao-device-hash

    ao-sample-format
    make-ao-sample-format	ao-sample-format?
    ao-sample-format-bits
    ao-sample-format-rate
    ao-sample-format-channels
    ao-sample-format-byte-format
    ao-sample-format-matrix

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
  (import (vicare (0 4 2017 1 (>= 10)))
    (prefix (vicare system structs) structs::)
    (vicare multimedia ao constants)
    (prefix (vicare multimedia ao unsafe-capi) capi::)
    (prefix (vicare ffi (or (0 4 2015 5 (>= 28))
			    (0 4 2015 (>= 6))
			    (0 4 (>= 2016))))
	    ffi::)
    (prefix (vicare ffi foreign-pointer-wrapper) ffi::)
    (vicare arguments validation)
    (vicare arguments general-c-buffers)
    (prefix (vicare platform words) words::))


;;;; version functions

(define (vicare-ao-version-interface-current)
  (capi::vicare-ao-version-interface-current))

(define (vicare-ao-version-interface-revision)
  (capi::vicare-ao-version-interface-revision))

(define (vicare-ao-version-interface-age)
  (capi::vicare-ao-version-interface-age))

(define (vicare-ao-version)
  (ascii->string (capi::vicare-ao-version)))


;;;; initialisation and shutdown

(define* (ao-initialize)
  (capi::ao-initialize))

(define* (ao-shutdown)
  (capi::ao-shutdown))


;;;; device options

(ffi::define-foreign-pointer-wrapper ao-option
  (ffi::foreign-destructor capi::ao-free-options)
  (ffi::collector-struct-type #f))

(module ()
  (structs::set-struct-type-printer! (type-descriptor ao-option)
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
    (cond ((capi::ao-append-option opt key^ val^)
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
    (capi::ao-option->alist opt)))

(define* (ao-append-global-option {key general-c-string?} {val general-c-string?})
  (with-general-c-strings
      ((key^ key)
       (val^ val))
    (capi::ao-append-global-option key^ val^)))


;;;; devicae playback and teardown

(ffi::define-foreign-pointer-wrapper ao-device
  (ffi::foreign-destructor capi::ao-close)
  (ffi::collector-struct-type #f))

(define-record-type ao-sample-format
  (fields (immutable bits)
		;Bits per sample.
	  (immutable rate)
		;Samples per second (in a single channel).
	  (immutable channels)
		;Number of audio channels.
	  (immutable byte-format)
		;Byte ordering in sample.
	  (immutable matrix)
		;Channel input matrix.
	  (immutable gen-matrix)
		;Channel input matrix as general C string.
	  #| end of FIELDS |# )
  (protocol
   (lambda (make-record)
     (lambda* ({bits		(and words::signed-int? positive?)}
	       {rate		(and words::signed-int? positive?)}
	       {channels	(and words::signed-int? positive?)}
	       {byte-format	byte-format?}
	       {matrix		(or not general-c-string?)})
       (make-record bits rate channels byte-format
		    (if matrix
			(with-general-c-strings
			    ((matrix^	matrix))
			  matrix^)
		      matrix)
		    matrix))))
  #| end of DEFINE-RECORD-TYPE |# )

(define (byte-format? obj)
  (memv obj `(,AO_FMT_LITTLE ,AO_FMT_BIG ,AO_FMT_NATIVE)))

(module ()
  (structs::set-struct-type-printer! (type-descriptor ao-device)
    (lambda (S port sub-printer)
      (define-syntax-rule (%display thing)
	(display thing port))
      (define-syntax-rule (%write thing)
	(write thing port))
      (%display "#[ao-device")
      (%display " pointer=")	(%display ($ao-device-pointer S))
      (%display "]"))))

(case-define* ao-open-live
  ((driver-id sample-format)
   (ao-open-live driver-id sample-format #f))
  (({driver-id		(and words::signed-int? non-negative?)}
    {sample-format	ao-sample-format?}
    {options		(or not ao-option?/alive)})
   (let ((rv (capi::ao-open-live driver-id sample-format options)))
     (if (pointer? rv)
	 (make-ao-device/owner rv)
       (error __who__
	 "error opening live device"
	 (cond
	  ;;No driver corresponds to driver_id.
	  ((= rv AO_ENODRIVER)		'AO_ENODRIVER)
	  ;;This driver is not a live output device.
	  ((= rv AO_ENOTLIVE)		'AO_ENOTLIVE)
	  ;;A valid option key has an invalid value.
	  ((= rv AO_EBADOPTION)		'AO_EBADOPTION)
	  ;;Cannot open  the device (for  example, if  /dev/dsp cannot be  opened for
	  ;;writing).
	  ((= rv AO_EOPENDEVICE)	'AO_EOPENDEVICE)
	  ;;Any other cause of failure.
	  ((= rv AO_EFAIL)		'AO_EFAIL)
	  (else				'unknown-code))))))
  #| end of CASE-DEFINE* |# )

(case-define* ao-open-file
  ((driver-id filename overwrite? sample-format)
   (ao-open-file driver-id filename overwrite? sample-format #f))
  (({driver-id		(and words::signed-int? non-negative?)}
    {filename		general-c-string?}
    overwrite?
    {sample-format	ao-sample-format?}
    {options		(or not ao-option?/alive)})
   (with-general-c-strings
       ((filename^	filename))
     (let ((rv (capi::ao-open-file driver-id filename^ overwrite? sample-format options)))
       (if (pointer? rv)
	   (make-ao-device/owner rv)
	 (error __who__
	   "error opening file device"
	   (cond
	    ;;No driver corresponds to driver_id.
	    ((= rv AO_ENODRIVER)	'AO_ENODRIVER)
	    ;;This driver is not a file output device.
	    ((= rv AO_ENOTFILE)		'AO_ENOTFILE)
	    ;;A valid option key has an invalid value.
	    ((= rv AO_EBADOPTION)	'AO_EBADOPTION)
	    ;;Cannot open the file.
	    ((= rv AO_EOPENFILE)	'AO_EOPENFILE)
	    ;;The  file already exists.
	    ((= rv AO_EFILEEXISTS)	'AO_EFILEEXISTS)
	    ;;Any other cause of failure.
	    ((= rv AO_EFAIL)		'AO_EFAIL)
	    (else			'unknown-code)))))))
  #| end of CASE-DEFINE* |# )

(define* (ao-play {device ao-device?/alive} {output-samples bytevector?})
  (capi::ao-play device output-samples))

(define* (ao-close {device ao-device?})
  ($ao-device-finalise device))


;;;; driver information

(structs::define-struct ao-info
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
  (structs::set-struct-type-printer! (type-descriptor ao-info)
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
    (let ((rv (capi::ao-driver-id short-name^)))
      (if (positive? rv)
	  rv
	#f))))

(define* (ao-default-driver-id)
  (capi::ao-default-driver-id))

(define* (ao-driver-info id)
  (receive-and-return (rv)
      (capi::ao-driver-info id (struct-type-descriptor ao-info))
    (when rv
      (%normalise-ao-info! rv))))

(define* (ao-driver-info-list)
  (receive-and-return (L)
      (capi::ao-driver-info-list (struct-type-descriptor ao-info))
    (for-each %normalise-ao-info! L)))

(define (%normalise-ao-info! info)
  (set-ao-info-name!       info (ascii->string     (ao-info-name       info)))
  (set-ao-info-short-name! info (ascii->string     (ao-info-short-name info)))
  (set-ao-info-comment!    info (ascii->string     (ao-info-comment    info)))
  (set-ao-info-options!    info (map ascii->string (ao-info-options    info))))

(define* (ao-file-extension {id (and words::signed-int? non-negative?)})
  (cond ((capi::ao-file-extension id)
	 => ascii->string)
	(else #f)))


;;;; miscellaneous

(define (ao-is-big-endian)
  (capi::ao-is-big-endian))


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; eval: (put 'ffi::define-foreign-pointer-wrapper 'scheme-indent-function 1)
;; End:
