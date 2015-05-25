;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/AO
;;;Contents: tests for Ao bindings
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


#!r6rs
(import (vicare)
  (prefix (vicare multimedia ao) ao.)
  (prefix (vicare multimedia ao constants) ao.)
  (prefix (vicare multimedia ao cond-expand) ao.)
  (vicare language-extensions cond-expand)
  #;(vicare arguments validation)
  (vicare numerics constants)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare Libao bindings\n")

(ao.ao-initialize)

;; These do nothing.
(ao.ao-initialise)
(ao.ao-initialise)

(define-cond-expand ao-cond-expand
  ao.vicare-ao-features)


;;;; helpers



(parametrise ((check-test-name	'version))

  (check
      (fixnum? (ao.vicare-ao-version-interface-current))
    => #t)

  (check
      (fixnum? (ao.vicare-ao-version-interface-revision))
    => #t)

  (check
      (fixnum? (ao.vicare-ao-version-interface-age))
    => #t)

  (check
      (string? (ao.vicare-ao-version))
    => #t)

  #t)


(parametrise ((check-test-name		'struct-option)
	      (struct-guardian-logger	#t))

  (check	;this will be garbage collected
      (let ((opt (ao.ao-append-option #f "client_name" "vicare-ao")))
;;;(debug-print opt)
	(ao.ao-option? opt))
    => #t)

  (check
      (ao.ao-option?/alive (ao.ao-append-option #f "client_name" "vicare-ao"))
    => #t)

  (check	;single finalisation
      (let ((opt (ao.ao-append-option #f "client_name" "vicare-ao")))
  	(ao.ao-free-options opt))
    => #f)

  (check	;double finalisation
      (let ((opt (ao.ao-append-option #f "client_name" "vicare-ao")))
  	(ao.ao-free-options opt)
  	(ao.ao-free-options opt))
    => #f)

  (check	;alive predicate after finalisation
      (let ((opt (ao.ao-append-option #f "client_name" "vicare-ao")))
  	(ao.ao-free-options opt)
  	(ao.ao-option?/alive opt))
    => #f)

;;; --------------------------------------------------------------------
;;; destructor

  (check
      (with-result
	(let ((opt (ao.ao-append-option #f "client_name" "vicare-ao")))
	  (ao.set-ao-option-custom-destructor! opt (lambda (opt)
						     (add-result 123)))
	  (ao.ao-free-options opt)))
    => '(#f (123)))

;;; --------------------------------------------------------------------
;;; hash

  (check-for-true
   (integer? (ao.ao-option-hash (ao.ao-append-option #f "client_name" "vicare-ao"))))

  (check
      (let ((A (ao.ao-append-option #f "client_name" "vicare-ao"))
	    (B (ao.ao-append-option #f "client_name" "vicare-ao"))
	    (T (make-hashtable ao.ao-option-hash eq?)))
	(hashtable-set! T A 1)
	(hashtable-set! T B 2)
	(list (hashtable-ref T A #f)
	      (hashtable-ref T B #f)))
    => '(1 2))

;;; --------------------------------------------------------------------
;;; properties

  (check
      (let ((opt (ao.ao-append-option #f "client_name" "vicare-ao")))
	(ao.ao-option-property-list opt))
    => '())

  (check
      (let ((opt (ao.ao-append-option #f "client_name" "vicare-ao")))
	(ao.ao-option-putprop opt 'ciao 'salut)
	(ao.ao-option-getprop opt 'ciao))
    => 'salut)

  (check
      (let ((opt (ao.ao-append-option #f "client_name" "vicare-ao")))
	(ao.ao-option-getprop opt 'ciao))
    => #f)

  (check
      (let ((opt (ao.ao-append-option #f "client_name" "vicare-ao")))
	(ao.ao-option-putprop opt 'ciao 'salut)
	(ao.ao-option-remprop opt 'ciao)
	(ao.ao-option-getprop opt 'ciao))
    => #f)

  (check
      (let ((opt (ao.ao-append-option #f "client_name" "vicare-ao")))
	(ao.ao-option-putprop opt 'ciao 'salut)
	(ao.ao-option-putprop opt 'hello 'ohayo)
	(list (ao.ao-option-getprop opt 'ciao)
	      (ao.ao-option-getprop opt 'hello)))
    => '(salut ohayo))

;;; --------------------------------------------------------------------
;;; appending options

  (check
      (let* ((opt (ao.ao-append-option #f "client_name" "vicare-ao"))
	     (opt (ao.ao-append-option opt  "client_name" "vicare-ao"))
	     (opt (ao.ao-append-option opt  "client_name" "vicare-ao")))
	(ao.ao-free-options opt))
    => #f)

  (check
      (let* ((opt (ao.ao-append-option #f "client_name" "vicare-ao"))
	     (opt (ao.ao-append-option opt  "alpha" "beta"))
	     (opt (ao.ao-append-option opt  "delta" "gamma")))
	(ao.ao-option->alist opt))
    => '(("client_name" . "vicare-ao")
	 ("alpha" . "beta")
	 ("delta" . "gamma")))

  (collect))


(parametrise ((check-test-name		'driver-info))

  (check-for-false
   (ao.ao-driver-id "ciao"))

  (check-for-true
   (positive-fixnum? (ao.ao-driver-id "alsa")))

  (check-for-true
   (positive-fixnum? (ao.ao-driver-id "oss")))

;;; --------------------------------------------------------------------

  (check-for-true
   (positive-fixnum? (ao.ao-default-driver-id)))

;;; --------------------------------------------------------------------

  (check
      (let ((I (ao.ao-driver-info (ao.ao-driver-id "alsa"))))
	(when #f
	  (fprintf (current-error-port) "ao-info: ~a\n" I))
	(ao.ao-info? I))
    => #t)

  (check
      (let ((I (ao.ao-driver-info (ao.ao-driver-id "oss"))))
	(when #f
	  (fprintf (current-error-port) "ao-info: ~a\n" I))
	(ao.ao-info? I))
    => #t)

  (check-for-false
   (ao.ao-driver-info 12345))

;;; --------------------------------------------------------------------

  (check-for-true
   (let ((L (ao.ao-driver-info-list)))
     (when #f
       (pretty-print L (current-error-port)))
     (for-all ao.ao-info? L)))

;;; --------------------------------------------------------------------

  (ao-cond-expand
   (ao.ao-file-extension
    (check
	(ao.ao-file-extension (ao.ao-driver-id "alsa"))
      => "wav"))
   (else
    (fprintf (current-error-port)
	     "~a: skipping test for ~a\n" (vicare-argv0-string) 'ao-file-extension)))

  #t)


(parametrise ((check-test-name		'struct-device)
	      (struct-guardian-logger	#t))

  (define-constant SAMPLE-FORMAT
    (let ((bits		16)
	  (rate		44100)
	  (channels	2)
	  (byte-format	ao.AO_FMT_NATIVE)
	  (matrix	"L,R"))
      (ao.make-ao-sample-format bits rate channels byte-format matrix)))

  (define-constant DEVICE-ID
    (ao.ao-default-driver-id))

  (define-constant DRIVER-OPTIONS
    #f)

  (define (make-device)
    (ao.ao-open-live DEVICE-ID SAMPLE-FORMAT DRIVER-OPTIONS))

  ;;NOTE We must  always close the device  either explicitly or by  running a garbage
  ;;collection, otherwise  trying to  open a  new device will  fail with  a "resource
  ;;busy" error.

;;; --------------------------------------------------------------------

  (check	;this will be garbage collected
      (let ((device (make-device)))
;;;(debug-print device)
	(ao.ao-device? device))
    => #t)

  (collect)

  (check
      (ao.ao-device?/alive (make-device))
    => #t)

  (collect)

  (check	;single finalisation
      (let ((device (make-device)))
  	(ao.ao-close device))
    => #t)

  (collect)

  (check	;double finalisation
      (let ((device (make-device)))
	(let* ((rv1 (ao.ao-close device))
	       (rv2 (ao.ao-close device)))
	  (values rv1 rv2)))
    => #t #f)

  (collect)

  (check	;alive predicate after finalisation
      (let ((device (make-device)))
  	(ao.ao-close device)
  	(ao.ao-device?/alive device))
    => #f)

;;; --------------------------------------------------------------------
;;; destructor

  (check
      (with-result
	(let ((device (make-device)))
	  (ao.set-ao-device-custom-destructor! device (lambda (device)
							(add-result 123)))
	  (ao.ao-close device)))
    => '(#t (123)))

  (collect)

;;; --------------------------------------------------------------------
;;; hash

  (check-for-true
   (integer? (ao.ao-device-hash (make-device))))

  (collect)

  (check
      (let ((A (make-device))
	    (T (make-hashtable ao.ao-device-hash eq?)))
	(hashtable-set! T A 1)
	(hashtable-ref T A #f))
    => 1)

  (collect)

;;; --------------------------------------------------------------------
;;; properties

  (check
      (let ((device (make-device)))
	(ao.ao-device-property-list device))
    => '())

  (collect)

  (check
      (let ((device (make-device)))
	(ao.ao-device-putprop device 'ciao 'salut)
	(ao.ao-device-getprop device 'ciao))
    => 'salut)

  (collect)

  (check
      (let ((device (make-device)))
	(ao.ao-device-getprop device 'ciao))
    => #f)

  (collect)

  (check
      (let ((device (make-device)))
	(ao.ao-device-putprop device 'ciao 'salut)
	(ao.ao-device-remprop device 'ciao)
	(ao.ao-device-getprop device 'ciao))
    => #f)

  (collect)

  (check
      (let ((device (make-device)))
	(ao.ao-device-putprop device 'ciao 'salut)
	(ao.ao-device-putprop device 'hello 'ohayo)
	(list (ao.ao-device-getprop device 'ciao)
	      (ao.ao-device-getprop device 'hello)))
    => '(salut ohayo))

  (collect))


(parametrise ((check-test-name		'playback))

  (check
      (let ((bits		16)
	    (rate		44100)
	    (channels		2)
	    (byte-format	ao.AO_FMT_LITTLE)
	    (matrix		"L,R"))
	(let* ((id		(ao.ao-default-driver-id))
	       (sample-format	(ao.make-ao-sample-format bits rate channels byte-format matrix))
	       (driver-options	#f)
	       (device		(ao.ao-open-live id sample-format driver-options)))
	  (fprintf (current-error-port)
		   "audio array slot size: ~a bytes\n"
		   (infix bits / 8 * channels))
	  (let* ((samples.len  (infix bits / 8 * channels * rate))
		 (samples.bv   (make-bytevector samples.len 0)))
	    (do ((i 0 (fxadd1 i)))
		((>= i rate))
	      (let* ((freq   440.0)
		     (sample (exact (floor (infix 0.75 * 32768.0 * sin(2 * greek-pi * freq * inexact(i / rate))))))
		     (j      (infix 4 * i)))
		;;Put the same  stuff in left and right channels.   The bytevector is
		;;an array of 32-bit slots, each with format:
		;;
		;;    channel 1 LSB channel 2 LSB channel 1 MSB channel 1 MSB
		;;   |-------------|-------------|-------------|-------------|
		;;
		;;where LSB stands for Least Significant Byte and MSB stands for Most
		;;Significant Byte.
		;;
		(let ((sample.lsb (infix #xFF & sample)))
		  (bytevector-u8-set! samples.bv j       sample.lsb)
		  (bytevector-u8-set! samples.bv (+ j 2) sample.lsb))
		(let ((sample.msb (infix #xFF & (sample >> 8))))
		  (bytevector-u8-set! samples.bv (infix j + 1) sample.msb)
		  (bytevector-u8-set! samples.bv (infix j + 3) sample.msb))))
	    (let* ((rv1 (ao.ao-play  device samples.bv))
		   (rv2 (ao.ao-close device)))
	      (values rv1 rv2)))))
    => #t #t)

  (collect))


(parametrise ((check-test-name		'file-output))

  (check
      (with-unwind-protection
	  (lambda (E)
	    (when (file-exists? "output.wav")
	      (delete-file "output.wav")))
	(lambda ()
	  (let ((bits		16)
		(rate		44100)
		(channels	2)
		(byte-format	ao.AO_FMT_LITTLE)
		(matrix		"L,R"))
	    (let* ((id			(ao.ao-driver-id "wav"))
		   (sample-format	(ao.make-ao-sample-format bits rate channels byte-format matrix))
		   (driver-options	#f)
		   (device		(ao.ao-open-file id "output.wav" #t sample-format driver-options)))
	      (fprintf (current-error-port)
		       "audio array slot size: ~a bytes\n"
		       (infix bits / 8 * channels))
	      (let* ((samples.len  (infix bits / 8 * channels * rate))
		     (samples.bv   (make-bytevector samples.len 0)))
		(do ((i 0 (fxadd1 i)))
		    ((>= i rate))
		  (let* ((freq   440.0)
			 (sample (exact (floor (infix 0.75 * 32768.0 * sin(2 * greek-pi * freq * inexact(i / rate))))))
			 (j      (infix 4 * i)))
		    ;;Put the same  stuff in left and right channels.   The bytevector is
		    ;;an array of 32-bit slots, each with format:
		    ;;
		    ;;    channel 1 LSB channel 2 LSB channel 1 MSB channel 1 MSB
		    ;;   |-------------|-------------|-------------|-------------|
		    ;;
		    ;;where LSB stands for Least Significant Byte and MSB stands for Most
		    ;;Significant Byte.
		    ;;
		    (let ((sample.lsb (infix #xFF & sample)))
		      (bytevector-u8-set! samples.bv j       sample.lsb)
		      (bytevector-u8-set! samples.bv (+ j 2) sample.lsb))
		    (let ((sample.msb (infix #xFF & (sample >> 8))))
		      (bytevector-u8-set! samples.bv (infix j + 1) sample.msb)
		      (bytevector-u8-set! samples.bv (infix j + 3) sample.msb))))
		(let* ((rv1 (ao.ao-play  device samples.bv))
		       (rv2 (ao.ao-close device)))
		  (values rv1 rv2)))))))
    => #t #t)

  (collect))


(parametrise ((check-test-name		'misc))

  (when #f
    (fprintf (current-error-port)
	     "big endian? ~a" (ao.ao-is-big-endian)))

  (check-for-true
   (boolean? (ao.ao-is-big-endian)))

  #t)


;;;; done

(ao.ao-shutdown)

(collect 4)
(check-report)

;;; end of file
