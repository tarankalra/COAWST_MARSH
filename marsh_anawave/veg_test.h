/*
** svn $Id: inlet_test.h 838 2008-11-17 04:22:18Z jcwarner $
*******************************************************************************
** Copyright (c) 2002-2008 The ROMS/TOMS Group                               **
**   Licensed under a MIT/X style license                                    **
**   See License_ROMS.txt                                                    **
*******************************************************************************
**
** Options for Vegetation Test Case, ROMS only.
**
** Application flag:   VEG_TEST
** Input script:       ocean_veg_test.in
**                     vegetation.in
**                     sediment_veg_test.in
*/

#define ROMS_MODEL

#define UV_VIS2
#define MIX_S_UV
#define MASKING
#define WET_DRY
#define UV_ADV
#undef  UV_COR
#define TS_MPDATA
#define DJ_GRADPS
#define SOLVE3D
#define SPLINES_VVISC
#define SPLINES_VDIFF

/*#define WEC_VF
#define BOTTOM_STREAMING
#define WDISS_WAVEMOD
#define UV_KIRBY*/

#define ANA_FSOBC
#define ANA_M2OBC
#define ANA_WWAVE 

#define VEGETATION
# ifdef VEGETATION
# define MARSH_WAVE_THRUST 
#  ifdef MARSH_WAVE_THRUST
/*#   define MARSH_SED_BEDLOAD*/
#  endif
#endif


/* define only one of the following */
#undef UV_LOGDRAG
#define SSW_BBL
#ifdef SSW_BBL
# define SSW_CALC_ZNOT
#endif

#ifdef SOLVE3D
# define GLS_MIXING
# ifdef GLS_MIXING
#  define KANTHA_CLAYSON
#  define N2S2_HORAVG
# ifdef VEG_TURB
#  undef N2S2_HORAVG
# endif
#  define RI_SPLINES
# endif
#endif

#define SEDIMENT
# ifdef SEDIMENT
#  define SUSPLOAD
#  define BEDLOAD
#  define  BEDLOAD_SOULSBY
# endif
# define ANA_SMFLUX
# define ANA_STFLUX
# define ANA_SSFLUX
# define ANA_BPFLUX
# define ANA_BTFLUX
# define ANA_BSFLUX
# define ANA_SPFLUX
# define ANA_SRFLUX

#define DIAGNOSTICS_UV
