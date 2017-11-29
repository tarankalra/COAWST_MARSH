      SUBROUTINE read_VegPar (model, inp, out, Lwrite)
!
!=======================================================================
!                                                                      !
!  This routine reads in vegetation model parameters.                  !
!  Equivalent of read_phypar.F so it gets read in that                 !
!  This routine also outputs vegetation model parameters.              !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mod_ncparam
      USE mod_scalars
      USE mod_vegetation
!
      implicit none
!
!  Imported variable declarations
!
      logical, intent(in) :: Lwrite
      integer, intent(in) :: model, inp, out
!
!  Local variable declarations.
!
      integer :: Npts, Nval
      integer :: iveg, ised, ng, status
      integer :: decode_line, load_i, load_l, load_lbc, load_r
!
      real(r8), dimension(200) :: Rval
#if defined MARSH_SED_EROSION 
!      real(r8), allocatable :: Rmarsh(:)
      real(r8), dimension(Ngrids) :: Rmarsh
#endif 
#ifdef VEG_DRAG 
      real(r8), allocatable :: Rveg(:,:)
#endif 
!
      character (len=40 ) :: KeyWord
      character (len=256) :: line 
      character (len=256), dimension(200) :: Cval
!
!-----------------------------------------------------------------------
!  Read input parameters from vegetation.in
!-----------------------------------------------------------------------
!
      DO WHILE (.TRUE.)
        READ (inp,'(a)',ERR=10,END=20) line
        status=decode_line(line, KeyWord, Nval, Cval, Rval)
        IF (status.gt.0) THEN
          SELECT CASE (TRIM(KeyWord))
#ifdef VEG_DRAG 
            CASE ('NVEG') 
              Npts=load_i(Nval, Rval, Ngrids, NVEG)
                IF (NVEG.lt.0) THEN
                  IF (Master) WRITE (out,30) 'NVEG', ng,                &
     &              'must be greater than zero.'
                  exit_flag=5
                  RETURN
                END IF
            IF (.not.allocated(Rveg)) allocate(Rveg(NVEG,Ngrids)) 
            CASE ('CD_VEG')
              IF (.not.allocated(CD_VEG)) allocate(CD_VEG(NVEG,Ngrids)) 
              Npts=load_r(Nval, Rval, NVEG*Ngrids, Rveg)
              DO ng=1,Ngrids
                DO iveg=1,NVEG
                  CD_VEG(iveg,ng)=Rveg(iveg,ng)
                END DO 
              END DO
            CASE ('E_VEG') 
              IF (.not.allocated(E_VEG)) allocate(E_VEG(NVEG,Ngrids)) 
              Npts=load_r(Nval, Rval, NVEG*Ngrids, Rveg)
              DO ng=1,Ngrids
                DO iveg=1,NVEG
                  E_VEG(iveg,ng)=Rveg(iveg,ng)
                END DO 
              END DO
            CASE ('VEG_MASSDENS') 
              IF (.not.allocated(VEG_MASSDENS))                         &
     &                 allocate(VEG_MASSDENS(NVEG,Ngrids)) 
              Npts=load_r(Nval, Rval, NVEG*Ngrids, Rveg)
              DO ng=1,Ngrids
                DO iveg=1,NVEG
                  VEG_MASSDENS(iveg,ng)=Rveg(iveg,ng)
                END DO 
              END DO
            CASE ('VEGHMIXCOEF') 
              IF (.not.allocated(VEGHMIXCOEF))                          &
     &                 allocate(VEGHMIXCOEF(NVEG,Ngrids)) 
              Npts=load_r(Nval, Rval, NVEG*Ngrids, Rveg)
              DO ng=1,Ngrids
                DO iveg=1,NVEG
                  VEGHMIXCOEF(iveg,ng)=Rveg(iveg,ng)
                END DO 
              END DO
#endif
!            IF (.not.allocated(Rmarsh)) allocate(Rmarsh(Ngrids))
#if defined MARSH_SED_EROSION
             CASE ('KFAC_MARSH')
               IF (.not.allocated(KFAC_MARSH))                          &
     &                allocate(KFAC_MARSH(Ngrids))
               Npts=load_r(Nval, Rval, Ngrids, Rmarsh)
               DO ng=1,Ngrids
                 KFAC_MARSH(ng)=Rmarsh(ng)
               END DO
             CASE ('DCRIT_MARSH')
               IF (.not.allocated(DCRIT_MARSH))                         &
     &                allocate(DCRIT_MARSH(Ngrids))
                 Npts=load_r(Nval, Rval, Ngrids, Rmarsh)
                 DO ng=1,Ngrids
                   DCRIT_MARSH(ng)=Rmarsh(ng)
                 END DO
#endif
!
!-----------------------------------------------------------------------
!  Read output ids from vegetation.in
!-----------------------------------------------------------------------
!          
#if defined VEG_DRAG || defined VEG_BIOMASS
            CASE ('Hout(ipdens)')
              IF (idvprp(pdens).eq.0) THEN 
                IF (Master) WRITE (out,30) 'ipdens'
                exit_flag=5
                RETURN
              END IF 
              Npts=load_l(Nval, Cval, Ngrids, Hout(idvprp(pdens),:))
            CASE ('Hout(iphght)')
              IF (idvprp(phght).eq.0) THEN 
                IF (Master) WRITE (out,30) 'iphght'
                exit_flag=5
                RETURN
              END IF 
              Npts=load_l(Nval, Cval, Ngrids, Hout(idvprp(phght),:))
            CASE ('Hout(ipdiam)')
              IF (idvprp(pdiam).eq.0) THEN 
                IF (Master) WRITE (out,30) 'ipdiam'
                exit_flag=5
                RETURN
              END IF 
              Npts=load_l(Nval, Cval, Ngrids, Hout(idvprp(pdiam),:))
            CASE ('Hout(ipthck)')
              IF (idvprp(pthck).eq.0) THEN 
                IF (Master) WRITE (out,30) 'ipthck'
                exit_flag=5
                RETURN
              END IF 
              Npts=load_l(Nval, Cval, Ngrids, Hout(idvprp(pthck),:))
#endif 
#ifdef VEG_BIOMASS
            CASE ('Hout(ipupbm)')
              IF (idvprp(pupbm).eq.0) THEN 
                IF (Master) WRITE (out,30) 'ipupbm'
                exit_flag=5
                RETURN
              END IF 
              Npts=load_l(Nval, Cval, Ngrids, Hout(idvprp(pupbm),:))
            CASE ('Hout(ipdwbm)')
              IF (idvprp(pdwbm).eq.0) THEN 
                IF (Master) WRITE (out,30) 'ipdwbm'
                exit_flag=5
                RETURN
              END IF 
              Npts=load_l(Nval, Cval, Ngrids, Hout(idvprp(pdwbm),:))
#endif 
#ifdef VEG_STREAMING 
            CASE ('Hout(idWdvg)')
              IF ((idWdvg).eq.0) THEN 
                IF (Master) WRITE (out,30) 'idWdvg'
                exit_flag=5
                RETURN
              END IF 
              Npts=load_l(Nval, Cval, Ngrids, Hout(idWdvg,:))
#endif 
#ifdef MARSH_WAVE_EROSION
            CASE ('Hout(idTims)')
              IF (idTims.eq.0) THEN 
                IF (Master) WRITE (out,30) 'idTims'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Hout(idTims,:))
            CASE ('Hout(idTtot)')
              IF (idTtot.eq.0) THEN 
                IF (Master) WRITE (out,30) 'idTtot'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Hout(idTtot,:))
# ifdef MARSH_SED_EROSION
            CASE ('Hout(idTmfo)')
              DO ised=1,NST
                IF (idTmfo(ised).eq.0) THEN
                  IF (Master) WRITE (out,30) 'idTmfo'
                  exit_flag=5
                  RETURN
                END IF
                Npts=load_l(Nval, Cval, Ngrids, Hout(idTmfo(ised),:))
              END DO
# endif 
# ifdef MARSH_RETREAT
            CASE ('Hout(idTmmr)')
              IF (idTmmr.eq.0) THEN 
                IF (Master) WRITE (out,30) 'idTmmr'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Hout(idTmmr,:))
# endif 
#endif
          END SELECT
        END IF
      END DO
  10  IF (Master) WRITE (out,30) line
      exit_flag=4
      RETURN
  20  CONTINUE
!
!-----------------------------------------------------------------------
! Print/Report input parameters (values specified in vegetation.in).
!-----------------------------------------------------------------------
!
      IF (Lwrite) THEN
        DO ng=1,Ngrids
            WRITE (out,50) ng
#if defined VEG_DRAG || defined VEG_BIOMASS 
            WRITE (out,60)
            DO iveg=1,NVEG
              WRITE (out,70) NVEG, CD_VEG(iveg,ng), E_VEG(iveg,ng),     &
     &                       VEG_MASSDENS(iveg,ng), VEGHMIXCOEF(iveg,ng)
            END DO 
#endif 
#ifdef MARSH_SED_EROSION 
           WRITE (out,80) KFAC_MARSH(ng)
           WRITE (out,90) DCRIT_MARSH(ng)
#endif
        END DO 
!     END IF 
!
!-----------------------------------------------------------------------
!  Report output parameters (switched on in vegetation.in).
!-----------------------------------------------------------------------
! 
#if defined VEG_DRAG || defined VEG_BIOMASS 
!      IF (Hout(idvprp(pdens),ng)WRITE (out,90) Hout(idvprp(pdens),ng),  & 
!     &  'Hout(ipdens)',                                                 &
!     &  'Write out plant density for each veg type.'
!      IF (Hout(idvprp(pdiam),:)) WRITE (out,90) Hout(idvprp(pdiam),ng),  & 
!     &  'Hout(ipdiam)',                                                 &
!     &  'Write out plant diameter for each veg type.'
!      IF (Hout(idvprp(pthck),ng)) WRITE (out,90) Hout(idvprp(pthck),:), & 
!     &  'Hout(ipthck)',                                                 &
!     &  'Write out plant thickness for each veg type.'
#endif 
#ifdef VEG_BIOMASS
!      IF (Hout(idvprp(pupbm),ng) WRITE (out,90) Hout(idvprp(pupbm),ng), & 
!     &  'Hout(ipupbm)',                                                 &
!     &  'Write out above ground plant biomass.'
!      IF (Hout(idvprp(pdwbm),ng) WRITE (out,90) Hout(idvprp(pdwbm),ng), & 
!     &  'Hout(idwbm)',                                                  &
!     &  'Write out below ground plant biomass.'
#endif 
#ifdef VEG_STREAMING 
!      IF (Hout(idWdvg,ng) WRITE (out,90) Hout(idWdvg,ng), & 
!     &  'Hout(idWdvg).    
!     &  'Write out wave dissipation due to vegetation.'
#endif 
#ifdef V
       IF (Hout(idTims) WRITE (out,100) Hout(idTims),                   & 
      &  'Write out wave dissipation due to vegetation.'
#endif 
       END IF 
   30  FORMAT (/,' read_VegPar - variable info not yet loaded, ',a)
   40  FORMAT (/,' read_VegPar - Error while processing line: ',/,a)
   50  FORMAT (/,/,' Vegetation Parameters, Grid: ',i2.2,               &
      &        /,  ' =================================',/)
#if defined VEG_DRAG || defined VEG_BIOMASS 
   60  FORMAT (/,1x,'Nveg(unitless)',2x,'Cd_veg(unitless)',2x,          &
      &        'E_veg(N/m2)',2x,'Veg_massdens(kg/m3)',2x,'VegHMixCoef'/)
   70  FORMAT (2x,i2,2(10x,1p,e11.4),2(5x,1p,e11.4))
#endif
   80  FORMAT ('Marsh erosion coefficient (s/m) = ',e11.1)
   90  FORMAT ('Marsh critical elevation  (m)   = ',e11.1)
#ifdef MARSH_SED_EROSION 
  100  FORMAT (1x,l1,2x,a,t29,a,i2.2,':',1x,a)
#endif
!
      RETURN
      END SUBROUTINE read_VegPar

