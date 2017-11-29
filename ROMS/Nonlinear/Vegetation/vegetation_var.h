/*
** svn $Id: vegetation_var.h 439 2015-06-10 12:00:00Z arango $
*************************************************** Hernan G. Arango ***
** Copyright (c) 2002-2016 The ROMS/TOMS Group                        **
**   Licensed under a MIT/X style license                             **
**   See License_ROMS.txt                                             **
*************************************************** John C. Warner    **
*************************************************** Neil K. Ganju     **
*************************************************** Alexis Beudin     **
*************************************************** Tarandeep S. Kalra**
**                                                                    **
**  Assigns metadata indices for the vegetation module variables that **
**  are used in input and output NetCDF files.  The metadata          **
**  information is read from "varinfo.dat".                           **
**                                                                    **
**  This file is included in file "mod_ncparam.F", routine            **
**  "initialize_ncparm".                                              **
**                                                                    **
************************************************************************
*/
#if defined VEG_DRAG || defined VEG_BIOMASS  
            CASE ('idvprp(pdens)')
                idvprp(pdens)=varid
            CASE ('idvprp(pdiam)')
                idvprp(pdiam)=varid
            CASE ('idvprp(pthck)')
                idvprp(pthck)=varid
            CASE ('idvprp(phght)')
                idvprp(phght)=varid
#endif 
#if defined VEG_BIOMASS  
            CASE ('idvprp(pabbm)')
                idvprp(pabbm)=varid
            CASE ('idvprp(pbgbm)')
                idvprp(pbgbm)=varid
#endif 
#if defined VEG_STREAMING 
            CASE ('idWdvg')
              idWdvg=varid
#endif 
#if defined MARSH_WAVE_EROSION
            CASE ('idTims')
              idTims=varid
            CASE ('idTtot')
              idTtot=varid
# if defined MARSH_SED_EROSION 
            CASE ('idTmfo')
              idTmfo=varid
# endif 
# if defined MARSH_RETREAT
            CASE ('idTmmr')
              idTmmr=varid
# endif 
#endif 
