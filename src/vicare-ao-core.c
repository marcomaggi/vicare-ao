/*
  Part of: Vicare/AO
  Contents: Libao for Vicare
  Date: Thu May 21, 2015

  Abstract

	Core functions.

  Copyright (C) 2015 Marco Maggi <marco.maggi-ipsu@poste.it>

  This program is  free software: you can redistribute  it and/or modify
  it under the  terms of the GNU General Public  License as published by
  the Free Software Foundation, either  version 3 of the License, or (at
  your option) any later version.

  This program  is distributed in the  hope that it will  be useful, but
  WITHOUT   ANY  WARRANTY;   without  even   the  implied   warranty  of
  MERCHANTABILITY  or FITNESS  FOR A  PARTICULAR PURPOSE.   See  the GNU
  General Public License for more details.

  You  should have received  a copy  of the  GNU General  Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#include "vicare-ao-internals.h"


/** --------------------------------------------------------------------
 ** initialisation and shutdown C wrappers.
 ** ----------------------------------------------------------------- */

static int	libao_initialised = 0;

ikptr
ikrt_ao_initialize (ikpcb * pcb)
{
#ifdef HAVE_AO_INITIALIZE
  if (! libao_initialised) {
    ao_initialize();
    libao_initialised = 1;
  }
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_ao_shutdown (ikpcb * pcb)
{
#ifdef HAVE_AO_SHUTDOWN
  if (libao_initialised) {
    ao_shutdown();
    libao_initialised = 0;
  }
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Device options C wrappers.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_ao_append_option (ikptr s_option, ikptr s_key, ikptr s_val, ikpcb * pcb)
{
  /* The  key and  value  strings are  duplicated  into newly  allocated
     memory, so  the calling  function retains  ownership of  the string
     parameters. */
  const char *	key = IK_GENERALISED_C_STRING(s_key);
  const char *	val = IK_GENERALISED_C_STRING(s_val);
  int		rv;
#ifdef HAVE_AO_APPEND_OPTION
  if (IK_FALSE == s_option) {
    /* We need to create a new linked list of "ao_option" structs. */
    ao_option *	opt = NULL;
    rv = ao_append_option(&opt, key, val);
    /* Return a pointer for success and false for failure. */
    return (rv)? ika_pointer_alloc(pcb, (ikuword_t)opt) : IK_FALSE;
  } else {
    /* We need to  append a new node to an  already existent linked list
       of "ao_option" structs. */
    ikptr	s_pointer = IK_AO_OPTION_POINTER(s_option);
    if (ik_is_pointer(s_pointer)) {
      ao_option *	opt = IK_POINTER_DATA_VOIDP(s_pointer);
      rv = ao_append_option(&opt, key, val);
      /* Return a pointer object for success and false for failure. */
      if (rv) {
	/* Store the new C pointer in the Scheme pointer object. */
	IK_POINTER_SET(s_pointer, opt);
	return s_pointer;
      } else {
	return IK_FALSE;
      }
    } else {
      return IK_FALSE;
    }
  }
#else
  feature_failure(__func__);
#endif
}

ikptr
ikrt_ao_free_options (ikptr s_option, ikpcb * pcb)
/* Apply  "ao_free_options()" to  the "ao_option"  pointer, but  only if
   such  pointer  is not  already  NULL  and  the struct  referenced  by
   "s_option" is the owner of the pointer. */
{
#ifdef HAVE_AO_FREE_OPTIONS
  ikptr		s_pointer	= IK_AO_OPTION_POINTER(s_option);
  if (ik_is_pointer(s_pointer)) {
    ao_option *		option	= IK_POINTER_DATA_VOIDP(s_pointer);
    int			owner	= IK_BOOLEAN_TO_INT(IK_AO_OPTION_OWNER(s_option));
    if (option && owner) {
      ao_free_options(option);
      IK_POINTER_SET_NULL(s_pointer);
    }
  }
  /* Return false so  that the return value  of "$ao-option-finalise" is
     always false. */
  return IK_FALSE;
#else
  feature_failure(__func__);
#endif
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_ao_option_keys_and_vals (ikptr s_option, ikpcb * pcb)
/* Build and return an alist of  Scheme bytevectors from the linked list
   of "ao_option" structs. */
{
  ikptr	s_pointer = IK_AO_OPTION_POINTER(s_option);
  if (ik_is_pointer(s_pointer)) {
    ao_option *	opt	= IK_POINTER_DATA_VOIDP(s_pointer);
    ikptr	s_list, s_spine, s_pair;
    s_list = s_spine = ika_pair_alloc(pcb);
    pcb->root0 = &s_list;
    pcb->root1 = &s_spine;
    {
      while (opt) {
	/* Allocate and initialise the next alist entry. */
	s_pair = ika_pair_alloc(pcb);
	pcb->root2 = &s_pair;
	{
	  ik_signal_dirt_in_page_of_pointer(pcb, s_pair);
	  IK_ASS(IK_CAR(s_pair), ika_bytevector_from_cstring(pcb, opt->key));
	  IK_ASS(IK_CDR(s_pair), ika_bytevector_from_cstring(pcb, opt->value));
	  /* Store the entry in the spine. */
	  ik_signal_dirt_in_page_of_pointer(pcb, s_spine);
	  IK_CAR(s_spine) = s_pair;
	}
	pcb->root2 = NULL;
	if (opt->next) {
	  /* More entries to be added: allocate  a new pair for the next
	     entry, then loop. */
	  IK_ASS(IK_CDR(s_spine), ika_pair_alloc(pcb));
	  s_spine = IK_CDR(s_spine);
	  opt = opt->next;
	} else {
	  /* No more entries. */
	  IK_CDR(s_spine) = IK_NULL;
	  break;
	}
      }
    }
    pcb->root1 = NULL;
    pcb->root0 = NULL;
    return s_list;
  } else {
    return IK_FALSE;
  }
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_ao_append_global_option (ikptr s_key, ikptr s_val, ikpcb * pcb)
{
#ifdef HAVE_AO_APPEND_GLOBAL_OPTION
  const char *	key = IK_GENERALISED_C_STRING(s_key);
  const char *	val = IK_GENERALISED_C_STRING(s_val);
  int		rv;
  /* Append a node to the internal  linked list of options.  The key and
     value strings  are duplicated into  newly allocated memory,  so the
     calling function retains ownership of the string parameters.*/
  rv = ao_append_global_option(key, val);
  /* Return true for success and false for failure. */
  return (rv)? IK_TRUE : IK_FALSE;
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Device playback and teardown C wrappers.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_ao_open_live (ikpcb * pcb)
{
#ifdef HAVE_AO_OPEN_LIVE
  /* rv = ao_open_live(); */
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_ao_open_file (ikpcb * pcb)
{
#ifdef HAVE_AO_OPEN_FILE
  /* rv = ao_open_file(); */
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_ao_play (ikpcb * pcb)
{
#ifdef HAVE_AO_PLAY
  /* rv = ao_play(); */
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_ao_close (ikpcb * pcb)
{
#ifdef HAVE_AO_CLOSE
  /* rv = ao_close(); */
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** driver information C wrappers.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_ao_driver_id (ikptr s_short_name, ikpcb * pcb)
{
#ifdef HAVE_AO_DRIVER_ID
  const char *	short_name = IK_GENERALISED_C_STRING(s_short_name);
  int		rv;
  rv = ao_driver_id(short_name);
  return IK_FIX(rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_ao_default_driver_id (ikpcb * pcb)
{
#ifdef HAVE_AO_DEFAULT_DRIVER_ID
  int	rv;
  rv = ao_default_driver_id();
  return IK_FIX(rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_ao_driver_info (ikpcb * pcb)
{
#ifdef HAVE_AO_DRIVER_INFO
  /* rv = ao_driver_info(); */
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_ao_driver_info_list (ikpcb * pcb)
{
#ifdef HAVE_AO_DRIVER_INFO_LIST
  /* rv = ao_driver_info_list(); */
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_ao_file_extension (ikptr s_id, ikpcb * pcb)
{
#ifdef HAVE_AO_FILE_EXTENSION
  int		id = ik_integer_to_int(s_id);
  const char*	rv;
  rv = ao_file_extension(id);
  return (rv)? ika_bytevector_from_cstring(pcb, rv) : IK_FALSE;
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** miscellaneous C wrappers.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_ao_is_big_endian (ikpcb * pcb)
{
#ifdef HAVE_AO_IS_BIG_ENDIAN
  /* rv = ao_is_big_endian(); */
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Still to be implemented.
 ** ----------------------------------------------------------------- */

#if 0
ikptr
ikrt_ao_doit (ikpcb * pcb)
{
#ifdef HAVE_AO_DOIT
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}
#endif

/* end of file */
