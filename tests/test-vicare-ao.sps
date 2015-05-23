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
  (vicare arguments validation)
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
	(fprintf (current-error-port) "ao-info: ~a\n" I)
	(ao.ao-info? I))
    => #t)

  (check
      (let ((I (ao.ao-driver-info (ao.ao-driver-id "oss"))))
	(fprintf (current-error-port) "ao-info: ~a\n" I)
	(ao.ao-info? I))
    => #t)

  (check-for-false
   (ao.ao-driver-info 12345))

;;; --------------------------------------------------------------------

  (check-for-true
   (let ((L (ao.ao-driver-info-list)))
     (pretty-print L (current-error-port))
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


;;;; done

(ao.ao-shutdown)

(check-report)

;;; end of file
