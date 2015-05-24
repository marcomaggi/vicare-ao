;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/AO
;;;Contents: demo for documentation
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
(program (demo)
  (import (vicare)
    (prefix (vicare multimedia ao) ao.)
    (prefix (vicare multimedia ao constants) ao.)
    (vicare numerics constants))

(ao.ao-initialise)


;;;; helpers




;;;; version functions

(internal-body

  (printf "version interface number: ~a\n" (ao.vicare-ao-version-interface-current))
  (printf "version revision number: ~a\n" (ao.vicare-ao-version-interface-revision))
  (printf "version age number: ~a\n" (ao.vicare-ao-version-interface-age))
  (printf "version string: ~a\n" (ao.vicare-ao-version))
  (flush-output-port (current-output-port))

  #t)


;;;; preamble

;;Some notes frequencies in Hertz:
;;
(define-constant A4   440.00)
(define-constant B4   493.88)
(define-constant C5   523.25)
(define-constant D5   587.33)
(define-constant E5   659.25)
(define-constant F5   698.46)
(define-constant Fs5  739.995)
(define-constant G5   783.99)
(define-constant A5   880.00)
(define-constant B5   987.77)
(define-constant C6  1046.50)

(define sample-format
  (let ((bits		16)    ;how many bits per sample
	(rate		44100) ;how many samples per second
	(channels	2)
	(byte-format	ao.AO_FMT_LITTLE)
	(matrix	"L,R"))
    (ao.make-ao-sample-format bits rate channels byte-format matrix)))


;;;; playback, single note, plain language

(internal-body

  (define device
    (let ((driver-options #f))
      (ao.ao-open-live (ao.ao-default-driver-id) sample-format driver-options)))

  (define (make-note frm freq msecs)
    ;;Build and return a samples bytevector, filled with a sinusoidal waveform.  FREQ
    ;;must be the tone frequency in Hertz.
    ;;
    (define number-of-ticks-per-second
      (infix inexact(ao.ao-sample-format-rate(frm))))
    (define number-of-ticks-per-millisecond
      (infix number-of-ticks-per-second / 1000.0))
    (define imax
      (infix exact(floor(msecs * number-of-ticks-per-millisecond))))
    (define samples.len
      (infix ao.ao-sample-format-bits(frm) / 8
	     * ao.ao-sample-format-channels(frm)
	     * imax))
    (define A      (infix 0.75 * 32768.0))
    (define omega  (infix 2 * greek-pi * freq))
    (receive-and-return (samples.bv)
	(make-bytevector samples.len 0)
      (do ((i 0 (fxadd1 i)))
	  ((>= i imax))
	(let* ((time   (infix inexact(i / number-of-ticks-per-second)))
	       (sample (exact (floor (infix A * sin(omega * time)))))
	       (j      (infix 4 * i)))
	  ;;Put the  same stuff  in left  and right channels.   The bytevector  is an
	  ;;array of 32-bit slots, each with format:
	  ;;
	  ;;    channel 1 LSB channel 2 LSB channel 1 MSB channel 2 MSB
	  ;;   |-------------|-------------|-------------|-------------|
	  ;;
	  ;;where  LSB stands  for Least  Significant Byte  and MSB  stands for  Most
	  ;;Significant Byte.
	  ;;
	  (let ((sample.lsb (infix #xFF & sample)))
	    (bytevector-u8-set! samples.bv j       sample.lsb)
	    (bytevector-u8-set! samples.bv (+ j 2) sample.lsb))
	  (let ((sample.msb (infix #xFF & (sample >> 8))))
	    (bytevector-u8-set! samples.bv (+ j 1) sample.msb)
	    (bytevector-u8-set! samples.bv (+ j 3) sample.msb))))))

  (define scale
    (map (lambda (freq)
	   (make-note sample-format freq 300))
      `(,C5 ,D5 ,E5 ,F5 ,G5 ,A5 ,B5 ,C6)))

  (for-each (lambda (samples)
	      (ao.ao-play device samples))
    scale)

  (ao.ao-close device)

  #| end of internal-body |# )


;;;; playback, melody, plain language

(internal-body

  (define device
    (let ((driver-options #f))
      (ao.ao-open-live (ao.ao-default-driver-id) sample-format driver-options)))

  (define (make-melody frm melody)
    ;;Build and return a samples bytevector, filled with a sinusoidal waveform.
    ;;
    (define number-of-ticks-per-second
      (infix inexact(ao.ao-sample-format-rate(frm))))
    (define number-of-ticks-per-millisecond
      (infix number-of-ticks-per-second / 1000.0))
    (define ticks*
      (map (lambda (note)
	     (let ((msecs (cdr note)))
	       (infix exact(floor(msecs * number-of-ticks-per-millisecond)))))
	melody))
    (define samples.len
      (let ((total-number-of-ticks (fold-left + 0 ticks*)))
	(infix ao.ao-sample-format-bits(frm) / 8
	       * ao.ao-sample-format-channels(frm)
	       * total-number-of-ticks)))
    (receive-and-return (samples.bv)
	(make-bytevector samples.len)
      (fold-left
	  (lambda (i.start note ticks)
	    (let ((freq  (car note))
		  (msecs (cdr note)))
	      (receive-and-return (i.end)
		  (+ i.start ticks)
		(fill-tone! samples.bv i.start i.end freq number-of-ticks-per-second))))
	0 melody ticks*)))

  (define (fill-tone! samples.bv i.start i.end freq number-of-ticks-per-second)
    ;;FREQ must be the tone frequency in Hertz.
    ;;
    (define A      (infix 0.75 * 32768.0))
    (define omega  (infix 2 * greek-pi * freq))
    (do ((i i.start (fxadd1 i)))
	((fx>=? i i.end))
      (let* ((time   (infix inexact(i / number-of-ticks-per-second)))
	     (sample (exact (floor (infix A * sin(omega * time)))))
	     (j      (infix 4 * i)))
	;;Put the same stuff in left and  right channels.  The bytevector is an array
	;;of 32-bit slots, each with format:
	;;
	;;    channel 1 LSB channel 2 LSB channel 1 MSB channel 2 MSB
	;;   |-------------|-------------|-------------|-------------|
	;;
	;;where  LSB stands  for  Least  Significant Byte  and  MSB  stands for  Most
	;;Significant Byte.
	;;
	(let ((sample.lsb (infix #xFF & sample)))
	  (bytevector-u8-set! samples.bv j       sample.lsb)
	  (bytevector-u8-set! samples.bv (+ j 2) sample.lsb))
	(let ((sample.msb (infix #xFF & (sample >> 8))))
	  (bytevector-u8-set! samples.bv (+ j 1) sample.msb)
	  (bytevector-u8-set! samples.bv (+ j 3) sample.msb)))))

  (define scale
    (make-melody sample-format
		 `((,C5 . 300) (,D5 . 280) (,E5 . 260)
		   (,F5 . 240) (,G5 . 220) (,A5 . 200)
		   (,B5 . 180) (,C6 . 160))))

  (ao.ao-play device scale)
  (ao.ao-close device)

  #| end of internal-body |# )


;;;; playback, iron man, plain language

(internal-body

  (define device
    (let ((driver-options #f))
      (ao.ao-open-live (ao.ao-default-driver-id) sample-format driver-options)))

  (define (make-melody frm melody)
    ;;Build and return a samples bytevector, filled with a sinusoidal waveform.
    ;;
    (define number-of-ticks-per-second
      (infix inexact(ao.ao-sample-format-rate(frm))))
    (define number-of-ticks-per-millisecond
      (infix number-of-ticks-per-second / 1000.0))
    (define ticks*
      (map (lambda (note)
	     (let ((msecs (cdr note)))
	       (infix exact(floor(msecs * number-of-ticks-per-millisecond)))))
	melody))
    (define samples.len
      (let ((total-number-of-ticks (fold-left + 0 ticks*)))
	(infix ao.ao-sample-format-bits(frm) / 8
	       * ao.ao-sample-format-channels(frm)
	       * total-number-of-ticks)))
    (receive-and-return (samples.bv)
	(make-bytevector samples.len)
      (fold-left
	  (lambda (i.start note ticks)
	    (let ((freq  (car note))
		  (msecs (cdr note)))
	      (receive-and-return (i.end)
		  (+ i.start ticks)
		(fill-tone! samples.bv i.start i.end freq number-of-ticks-per-second))))
	0 melody ticks*)))

  (define (fill-tone! samples.bv i.start i.end freq number-of-ticks-per-second)
    ;;FREQ must be the tone frequency in Hertz.
    ;;
    (define A     (infix 0.75 * 32768.0))
    (define omega (infix 2 * greek-pi * freq))
    (define sample-max (exact (floor (* A 0.20))))
    (define sample-min (- sample-max))
    (do ((i i.start (fxadd1 i)))
	((fx>=? i i.end))
      (let* ((time   (infix inexact(i / number-of-ticks-per-second)))
	     (sample (exact (floor (infix A * sin(omega * time)))))
	     (j      (infix 4 * i)))
	;;Put the same stuff in left and  right channels.  The bytevector is an array
	;;of 32-bit slots, each with format:
	;;
	;;    channel 1 LSB channel 2 LSB channel 1 MSB channel 2 MSB
	;;   |-------------|-------------|-------------|-------------|
	;;
	;;where  LSB stands  for  Least  Significant Byte  and  MSB  stands for  Most
	;;Significant Byte.
	;;
	(let ((sample.lsb (infix #xFF & sample)))
	  (bytevector-u8-set! samples.bv j       sample.lsb)
	  (bytevector-u8-set! samples.bv (+ j 2) sample.lsb))
	(let ((sample.msb (infix #xFF & (sample >> 8))))
	  (bytevector-u8-set! samples.bv (+ j 1) sample.msb)
	  (bytevector-u8-set! samples.bv (+ j 3) sample.msb)))))

  (define iron-man
    (let ((T 1500)
	  (pause '(0 . 100))
	  (pause/2 '(0 . 50)))
      (make-melody sample-format
		   `((,B4  . ,(* T 1/4)) ,pause
		     (,D5  . ,(* T 1/4)) ,pause
		     (,D5  . ,(* T 1/8)) ,pause
		     (,E5  . ,(* T 1/8)) ,pause
		     (,E5  . ,(* T 1/4)) ,pause
		     (,G5  . ,(* T 1/16)) ,pause/2
		     (,Fs5 . ,(* T 1/16)) ,pause/2
		     (,G5  . ,(* T 1/16)) ,pause/2
		     (,Fs5 . ,(* T 1/16)) ,pause/2
		     (,G5  . ,(* T 1/16)) ,pause/2
		     (,Fs5 . ,(* T 1/16)) ,pause/2
		     (,D5  . ,(* T 1/8)) ,pause
		     (,D5  . ,(* T 1/8)) ,pause
		     (,E5  . ,(* T 1/8)) ,pause
		     (,E5  . ,(* T 1/4))
		     ))))

  (ao.ao-play device iron-man)
  (ao.ao-close device)

  #| end of internal-body |# )


;;;; done

(ao.ao-shutdown)

(collect 4)

#| end of program |# )

;;; end of file
