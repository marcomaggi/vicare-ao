/*
  Part of: Vicare/AO
  Contents: print platform features library
  Date: Thu May 21, 2015

  Abstract



  Copyright (C) 2015 Marco Maggi <marco.maggi-ipsu@poste.it>

  This program is  free software: you can redistribute  it and/or modify
  it under the  terms of the GNU General Public  License as published by
  the Free Software Foundation, either version  3 of the License, or (at
  your option) any later version.

  This program  is distributed in the  hope that it will  be useful, but
  WITHOUT   ANY  WARRANTY;   without  even   the  implied   warranty  of
  MERCHANTABILITY  or FITNESS  FOR A  PARTICULAR PURPOSE.   See the  GNU
  General Public License for more details.

  You should  have received  a copy  of the  GNU General  Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/


#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <stdio.h>
#include <stdlib.h>


int
main (int argc, const char *const argv[])
{
  printf(";;; -*- coding: utf-8-unix -*-\n\
;;;\n\
;;;Part of: Vicare/AO\n\
;;;Contents: static platform inspection\n\
;;;Date: Thu May 21, 2015\n\
;;;\n\
;;;Abstract\n\
;;;\n\
;;;\n\
;;;\n\
;;;Copyright (C) 2015 Marco Maggi <marco.maggi-ipsu@poste.it>\n\
;;;\n\
;;;This program is free software:  you can redistribute it and/or modify\n\
;;;it under the terms of the  GNU General Public License as published by\n\
;;;the Free Software Foundation, either version 3 of the License, or (at\n\
;;;your option) any later version.\n\
;;;\n\
;;;This program is  distributed in the hope that it  will be useful, but\n\
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of\n\
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU\n\
;;;General Public License for more details.\n\
;;;\n\
;;;You should  have received a  copy of  the GNU General  Public License\n\
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.\n\
;;;\n\
\n\
\n\
#!r6rs\n\
(library (vicare multimedia ao features)\n\
  (export\n\
    ;; initialisation and shutdown features\n\
    HAVE_AO_INITIALIZE\n\
    HAVE_AO_SHUTDOWN\n\
    \n\
    ;; device setup/playback/teardown features\n\
    HAVE_AO_APPEND_GLOBAL_OPTION\n\
    HAVE_AO_APPEND_OPTION\n\
    HAVE_AO_FREE_OPTIONS\n\
    HAVE_AO_OPEN_LIVE\n\
    HAVE_AO_OPEN_FILE\n\
    HAVE_AO_PLAY\n\
    HAVE_AO_CLOSE\n\
    \n\
    ;; driver information features\n\
    HAVE_AO_DRIVER_ID\n\
    HAVE_AO_DEFAULT_DRIVER_ID\n\
    HAVE_AO_DRIVER_INFO\n\
    HAVE_AO_DRIVER_INFO_LIST\n\
    HAVE_AO_FILE_EXTENSION\n\
    \n\
    ;; miscellaneous features\n\
    HAVE_AO_IS_BIG_ENDIAN\n\
    \n\
    )\n\
  (import (rnrs))\n\
\n\
;;;; helpers\n\
\n\
(define-syntax define-inline-constant\n\
  (syntax-rules ()\n\
    ((_ ?name ?value)\n\
     (define-syntax ?name (identifier-syntax ?value)))))\n\
\n\
\n\
;;;; code\n\n");


/** --------------------------------------------------------------------
 ** initialisation and shutdown features.
 ** ----------------------------------------------------------------- */

printf("(define-inline-constant HAVE_AO_INITIALIZE %s)\n",
#ifdef HAVE_AO_INITIALIZE
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_AO_SHUTDOWN %s)\n",
#ifdef HAVE_AO_SHUTDOWN
  "#t"
#else
  "#f"
#endif
  );


/** --------------------------------------------------------------------
 ** device setup/playback/teardown features.
 ** ----------------------------------------------------------------- */

printf("(define-inline-constant HAVE_AO_APPEND_GLOBAL_OPTION %s)\n",
#ifdef HAVE_AO_APPEND_GLOBAL_OPTION
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_AO_APPEND_OPTION %s)\n",
#ifdef HAVE_AO_APPEND_OPTION
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_AO_FREE_OPTIONS %s)\n",
#ifdef HAVE_AO_FREE_OPTIONS
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_AO_OPEN_LIVE %s)\n",
#ifdef HAVE_AO_OPEN_LIVE
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_AO_OPEN_FILE %s)\n",
#ifdef HAVE_AO_OPEN_FILE
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_AO_PLAY %s)\n",
#ifdef HAVE_AO_PLAY
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_AO_CLOSE %s)\n",
#ifdef HAVE_AO_CLOSE
  "#t"
#else
  "#f"
#endif
  );


/** --------------------------------------------------------------------
 ** driver information features.
 ** ----------------------------------------------------------------- */

printf("(define-inline-constant HAVE_AO_DRIVER_ID %s)\n",
#ifdef HAVE_AO_DRIVER_ID
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_AO_DEFAULT_DRIVER_ID %s)\n",
#ifdef HAVE_AO_DEFAULT_DRIVER_ID
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_AO_DRIVER_INFO %s)\n",
#ifdef HAVE_AO_DRIVER_INFO
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_AO_DRIVER_INFO_LIST %s)\n",
#ifdef HAVE_AO_DRIVER_INFO_LIST
  "#t"
#else
  "#f"
#endif
  );

printf("(define-inline-constant HAVE_AO_FILE_EXTENSION %s)\n",
#ifdef HAVE_AO_FILE_EXTENSION
  "#t"
#else
  "#f"
#endif
  );


/** --------------------------------------------------------------------
 ** miscellaneous features.
 ** ----------------------------------------------------------------- */

printf("(define-inline-constant HAVE_AO_IS_BIG_ENDIAN %s)\n",
#ifdef HAVE_AO_IS_BIG_ENDIAN
  "#t"
#else
  "#f"
#endif
  );


  printf("\n\
;;;; done\n\
\n\
)\n\
\n\
;;; end of file\n");
  exit(EXIT_SUCCESS);
}

/* end of file */
