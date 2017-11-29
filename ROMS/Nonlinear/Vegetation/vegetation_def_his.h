/*
** svn $Id: vegetation_def.h 429 2009-12-20 17:30:26Z arango $
*************************************************** Hernan G. Arango ***
** Copyright (c) 2002-2016 The ROMS/TOMS Group                        **
**   Licensed under a MIT/X style license                             **
**   See License_ROMS.txt                                             **
*************************************************** John C. Warner    **
*************************************************** Neil K. Ganju     **
*************************************************** Alexis Beudin     **
*************************************************** Tarandeep S. Kalra**
**                                                                    **
**  Defines vegetation module input parameters in output NetCDF files.**
**  It is included in routine "def_his.F".                            **
**                                                                    **
************************************************************************
*/
#if defined VEG_DRAG || defined VEG_BIOMASS
!
!  Define vegetation module parameters.
!
      DO i=1,NVEGP
        IF (Hout(idvprp(i),ng)) THEN
           Vinfo( 1)=Vname(1,idvprp(i))
           Vinfo( 2)=Vname(2,idvprp(i))
           Vinfo( 3)=Vname(3,idvprp(i))
           Vinfo(14)=Vname(4,idvprp(i))
           Vinfo(16)=Vname(1,idtime)
#  if defined WRITE_WATER && defined MASKING
           Vinfo(20)='mask_rho'
#  endif
!           Vinfo(22)='coordinates'
           Aval(5)=REAL(Iinfo(1,idvprp(i),ng),r8)
           status=def_var(ng, iNLM, HIS(ng)%ncid,HIS(ng)%Vid(idvprp(i))  &
     &                   ,NF_FOUT, nvd4, v3pgrd, Aval, Vinfo, ncname)
          IF (exit_flag.ne.NoError) RETURN
        END IF
      END DO
#endif 
#if defined VEG_STREAMING 
!
!  Define wave dissipation due to vegetation. 
!
        IF (Hout(idWdvg,ng)) THEN 
          Vinfo( 1)=Vname(1,idWdvg)
          Vinfo( 2)=Vname(2,idWdvg)
          Vinfo( 3)=Vname(3,idWdvg)
          Vinfo(14)=Vname(4,idWdvg)
          Vinfo(16)=Vname(1,idWdvg)
# if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
# endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idWdvg,ng),r8)
          status=def_var(ng, iNLM, HIS(ng)%ncid, HIS(ng)%Vid(idWdvg),   &
     &                   NF_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (exit_flag.ne.NoError) RETURN
        END IF 
#endif 
#ifdef MARSH_WAVE_EROSION
!
!  Store masking marsh of marsh cells. 
!
       IF (Hout(idTims,ng)) THEN 
         Vinfo( 1)=Vname(1,idTims)
         Vinfo( 2)=Vname(2,idTims)
         Vinfo( 3)=Vname(3,idTims)
         Vinfo(14)=Vname(4,idTims)
         Vinfo(16)=Vname(1,idTims)
#  if defined WRITE_WATER && defined MASKING
         Vinfo(20)='mask_rho'
#  endif
         Vinfo(22)='coordinates'
         Aval(5)=REAL(Iinfo(1,idTims,ng),r8)
         status=def_var(ng, iNLM, HIS(ng)%ncid, HIS(ng)%Vid(idTims),   &
     &                  NF_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
         IF (exit_flag.ne.NoError) RETURN
       END IF
!
!  Total thrust from all directions due to waves.
!
        IF (Hout(idTtot,ng)) THEN 
          Vinfo( 1)=Vname(1,idTtot)
          Vinfo( 2)=Vname(2,idTtot)
          Vinfo( 3)=Vname(3,idTtot)
          Vinfo(14)=Vname(4,idTtot)
          Vinfo(16)=Vname(1,idTtot)
#  if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#  endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idTtot,ng),r8)
          status=def_var(ng, iNLM, HIS(ng)%ncid, HIS(ng)%Vid(idTtot),   &
     &                   NF_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (exit_flag.ne.NoError) RETURN
        END IF
!
# ifdef MARSH_SED_EROSION
!
!  Marsh sediment flux out from marsh cells from each sedclass type.
!
        DO i=1,NST
          IF (Hout(idTmfo(i),ng)) THEN
            Vinfo( 1)=Vname(1,idTmfo(i))
            Vinfo( 2)=Vname(2,idTmfo(i))
            Vinfo( 3)=Vname(3,idTmfo(i))
            Vinfo(14)=Vname(4,idTmfo(i))
            Vinfo(16)=Vname(1,idtime)
#  if defined WRITE_WATER && defined MASKING
            Vinfo(20)='mask_rho'
#  endif
            Vinfo(22)='coordinates'
            Aval(5)=REAL(Iinfo(1,idTmfo(i),ng))
            status=def_var(ng, iNLM, HIS(ng)%ncid,                      &
     &                     HIS(ng)%Vid(idTmfo(i)), NF_FOUT,             &
     &                     nvd3, t2dgrd, Aval, Vinfo, ncname)
            IF (exit_flag.ne.NoError) RETURN
          END IF
        END DO
# endif
!
# ifdef MARSH_RETREAT 
!
!  Amount of marsh retreat from all directions.
!
        IF (Hout(idTmmr,ng)) THEN 
          Vinfo( 1)=Vname(1,idTmmr)
          Vinfo( 2)=Vname(2,idTmmr)
          Vinfo( 3)=Vname(3,idTmmr)
          Vinfo(14)=Vname(4,idTmmr)
          Vinfo(16)=Vname(1,idTmmr)
#  if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_rho'
#  endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(Iinfo(1,idTmmr,ng),r8)
          status=def_var(ng, iNLM, HIS(ng)%ncid, HIS(ng)%Vid(idTmmr),   &
     &                   NF_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
          IF (exit_flag.ne.NoError) RETURN
        END IF
# endif
#endif 
