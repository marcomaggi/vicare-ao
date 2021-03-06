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
#include <string.h>


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

static void
ik_ao_sample_format_from_record (ao_sample_format * format, ikptr s_sample_format)
{
  memset(format, '\0', sizeof(ao_sample_format));
  format->bits		= ik_integer_to_int(IK_AO_SAMPLE_FORMAT_BITS(s_sample_format));
  format->rate		= ik_integer_to_int(IK_AO_SAMPLE_FORMAT_RATE(s_sample_format));
  format->channels	= ik_integer_to_int(IK_AO_SAMPLE_FORMAT_CHANNELS(s_sample_format));
  format->byte_format	= ik_integer_to_int(IK_AO_SAMPLE_FORMAT_BYTE_FORMAT(s_sample_format));
  {
    ikptr	s_matrix = IK_AO_SAMPLE_FORMAT_MATRIX(s_sample_format);
    format->matrix = (IK_FALSE == s_matrix)? NULL : IK_GENERALISED_C_STRING(s_matrix);
  }
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_ao_open_live (ikptr s_device_id, ikptr s_sample_format, ikptr s_option, ikpcb * pcb)
/* Open a device for audio  playback.  When successful: return a pointer
   object referencing  an "ao_device"  C language  struct.  If  an error
   occurs: return an exact integer  representing a Libao error code (see
   the documentation for details).

   S_DEVICE_ID must be  an exact integer representing  the identifier of
   an    audio   device,    obtained   with    a   previous    call   to
   "ikrt_ao_driver_id()" or "ikrt_ao_default_driver_id".

   S_SAMPLE_FORMAT  must be  a  reference  to a  Scheme  record of  type
   "ao-sample-format".  Here it  is converted to a  C language structure
   of type "ao_sample_format".

   S_OPTION must  be false  or a  reference to a  Scheme struct  of type
   "ao-option".
*/
{
#ifdef HAVE_AO_OPEN_LIVE
  int			id;
  ao_sample_format	format;
  ao_option *		option;
  ao_device *		rv;
  id     = ik_integer_to_int(s_device_id);
  option = (IK_FALSE == s_option)? NULL : IK_AO_OPTION(s_option);
  ik_ao_sample_format_from_record(&format, s_sample_format);
  if (0) {
    fprintf(stderr, "%s: bits %d, channels %d, rate %d, byte_format %d\n",
	    __func__, format.bits, format.channels, format.rate, format.byte_format);
  }
  errno = 0;
  rv    = ao_open_live(id, &format, option);
  if (rv) {
    return ika_pointer_alloc(pcb, (ikuword_t)rv);
  } else {
    return ika_integer_from_int(pcb, errno);
  }
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_ao_open_file (ikptr s_driver_id, ikptr s_filename, ikptr s_overwrite,
		   ikptr s_sample_format, ikptr s_option, ikpcb * pcb)
{
#ifdef HAVE_AO_OPEN_FILE
  int			driver_id	= ik_integer_to_int(s_driver_id);
  const char *		filename	= IK_GENERALISED_C_STRING(s_filename);
  int			overwrite	= IK_BOOLEAN_TO_INT(s_overwrite);
  ao_sample_format	format;
  ao_option *		option		= (IK_FALSE == s_option)? NULL : IK_AO_OPTION(s_option);
  ao_device *		rv;
  ik_ao_sample_format_from_record(&format, s_sample_format);
  if (0) {
    fprintf(stderr, "%s: driver_id %d, filename %s, overwrite? %d\n",
	    __func__, driver_id, filename, overwrite);
  }
  errno = 0;
  rv = ao_open_file(driver_id, filename, overwrite, &format, option);
  if (rv) {
    return ika_pointer_alloc(pcb, (ikuword_t)rv);
  } else {
    return ika_integer_from_int(pcb, errno);
  }
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_ao_play (ikptr s_device, ikptr s_output_samples, ikpcb * pcb)
{
#ifdef HAVE_AO_PLAY
  ao_device *	device		= IK_AO_DEVICE(s_device);
  char *	output_samples	= IK_BYTEVECTOR_DATA_CHARP(s_output_samples);
  uint_32	num_bytes	= (uint_32)IK_BYTEVECTOR_LENGTH(s_output_samples);
  int		rv;
  rv = ao_play(device, output_samples, num_bytes);
  return IK_BOOLEAN_FROM_INT(rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_ao_close (ikptr s_device, ikpcb * pcb)
{
#ifdef HAVE_AO_CLOSE
  ikptr		s_pointer	= IK_AO_DEVICE_POINTER(s_device);
  int		rv		= 1;
  if (ik_is_pointer(s_pointer)) {
    ao_device *		device	= IK_POINTER_DATA_VOIDP(s_pointer);
    int			owner	= IK_BOOLEAN_TO_INT(IK_AO_DEVICE_OWNER(s_device));
    if (device && owner) {
      rv = ao_close(device);
      IK_POINTER_SET_NULL(s_pointer);
    }
  }
  return (rv)? IK_TRUE : IK_FALSE;
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** driver information C wrappers.
 ** ----------------------------------------------------------------- */

static ikptr
ao_info_to_scheme_struct (ikpcb * pcb, ao_info * info, ikptr s_ao_info_std)
/* Build  and  return a  new  Scheme  struct  object of  type  "ao-info"
   representing  the C  language  struct "ao_info"  referenced by  INFO.
   S_AO_INFO_STD is the struct type descriptor. */
{
  ikptr		s_stru = ika_struct_alloc_and_init(pcb, s_ao_info_std);
  pcb->root9 = &s_stru;
  {
    ik_signal_dirt_in_page_of_pointer(pcb, s_stru);
    /* Field "type". */
    IK_ASS(IK_FIELD(s_stru, 0), ika_integer_from_int(pcb, info->type));
    /* Field "name". */
    IK_ASS(IK_FIELD(s_stru, 1), ika_bytevector_from_cstring(pcb, info->name));
    /* Field "short-name". */
    IK_ASS(IK_FIELD(s_stru, 2), ika_bytevector_from_cstring(pcb, info->short_name));
    /* Field "comment". */
    IK_ASS(IK_FIELD(s_stru, 3), ika_bytevector_from_cstring(pcb, info->comment));
    /* Field "preferred-byte-format". */
    IK_ASS(IK_FIELD(s_stru, 4), ika_integer_from_int(pcb, info->preferred_byte_format));
    /* Field "priority". */
    IK_ASS(IK_FIELD(s_stru, 5), ika_integer_from_int(pcb, info->priority));
    /* Field "options". */
    if (0 < info->option_count) {
      ikptr	s_spine = ika_pair_alloc(pcb);
      IK_FIELD(s_stru, 6) = s_spine;
      pcb->root8 = &s_spine;
      {
	for (int i=0; i<info->option_count;) {
	  ik_signal_dirt_in_page_of_pointer(pcb, s_spine);
	  IK_ASS(IK_CAR(s_spine), ika_bytevector_from_cstring(pcb, info->options[i]));
	  ++i;
	  if (i < info->option_count) {
	    IK_ASS(IK_CDR(s_spine), ika_pair_alloc(pcb));
	    s_spine = IK_CDR(s_spine);
	  } else {
	    IK_CDR(s_spine) = IK_NULL;
	  }
	}
      }
      pcb->root8 = NULL;
    } else {
      IK_FIELD(s_stru, 6) = IK_NULL;
    }
  }
  pcb->root9 = NULL;
  return s_stru;
}

/* ------------------------------------------------------------------ */

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
ikrt_ao_driver_info (ikptr s_id, ikptr s_ao_info_std, ikpcb * pcb)
{
#ifdef HAVE_AO_DRIVER_INFO
  int		id = ik_integer_to_int(s_id);
  ao_info *	rv;
  rv = ao_driver_info(id);
  if (rv) {
    return ao_info_to_scheme_struct(pcb, rv, s_ao_info_std);
  } else {
    return IK_FALSE;
  }
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_ao_driver_info_list (ikptr s_ao_info_std, ikpcb * pcb)
{
#ifdef HAVE_AO_DRIVER_INFO_LIST
  int		info_count;
  ao_info **	infos = ao_driver_info_list(&info_count);
  if (0 < info_count) {
    ikptr	s_list, s_spine;
    s_list = s_spine = ika_pair_alloc(pcb);
    pcb->root0 = &s_list;
    pcb->root1 = &s_spine;
    {
      for (int i=0; i<info_count;) {
	ik_signal_dirt_in_page_of_pointer(pcb, s_spine);
	IK_CAR(s_spine) = ao_info_to_scheme_struct(pcb, infos[i], s_ao_info_std);
	++i;
	if (i < info_count) {
	  IK_ASS(IK_CDR(s_spine), ika_pair_alloc(pcb));
	  s_spine = IK_CDR(s_spine);
	} else {
	  IK_CDR(s_spine) = IK_NULL;
	}
      }
    }
    pcb->root1 = NULL;
    pcb->root0 = NULL;
    return s_list;
  } else {
    return IK_NULL;
  }
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
  return IK_BOOLEAN_FROM_INT(ao_is_big_endian());
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
