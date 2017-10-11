#include "cppdefs.h"

      MODULE marsh_sed_bedload_mode1_mod

#if defined SEDIMENT && defined BEDLOAD && defined MARSH_WAVE_THRUST \
    && defined MARSH_WAVE_EROSION && defined MARSH_SED_BEDLOAD_MODE1 
!
!svn $Id: marsh_wave_thrust.F 429 2015-04-20 17:30:26Z arango $
!=======================================================================
!  Copyright (c) 2002-2016 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license           Hernan G. Arango   !
!    See License_ROMS.txt                   Alexander F. Shchepetkin   !
!================================================Neil K. Ganju  =======!
!==============================================Tarandeep S. Kalra======!
!==============================================John C. Warner==========!
!                                                                      ! 
!  References:                                                         !
!                                                                      !
!=======================================================================
!
      implicit none

      PRIVATE
      PUBLIC  :: marsh_sed_bedload_mode1

      CONTAINS
!
!***********************************************************************
      SUBROUTINE marsh_sed_bedload_mode1 (ng, tile)
!***********************************************************************
!
      USE mod_param
      USE mod_forces
      USE mod_grid
      USE mod_ocean
      USE mod_vegetation 
      USE mod_vegarr
      USE mod_sedbed
      USE mod_stepping
      USE marsh_wave_thrust_mod
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
!
!  Local variable declarations.
!
# include "tile.h"
!
# ifdef PROFILE
      CALL wclock_on (ng, iNLM, 16)
# endif
      CALL marsh_sed_bedload_mode1_tile (ng, tile,                      &
     &                       LBi, UBi, LBj, UBj,                        &
     &                       IminS, ImaxS, JminS, JmaxS,                &
     &                       GRID(ng) % om_r,                           &
     &                       GRID(ng) % on_r,                           &
     &                       nstp(ng), nnew(ng),                        &
     &                       VEG(ng) % Thrust_w,                        & 
     &                       VEG(ng) % Thrust_e,                        &
     &                       VEG(ng) % Thrust_n,                        &
     &                       VEG(ng) % Thrust_s,                        &
# ifdef MARSH_LAT_RETREAT
     &                       VEG(ng) % marsh_flux_e,                    &
     &                       VEG(ng) % marsh_flux_w,                    &
     &                       VEG(ng) % marsh_flux_n,                    &
     &                       VEG(ng) % marsh_flux_s,                    &
# endif 
     &                    SEDBED(ng) % bed_frac,                        &
     &                    SEDBED(ng) % bed,                             &
     &                    SEDBED(ng) % bed_mass,                        &
     &                       VEG(ng) % marsh_mask)
# ifdef PROFILE
      CALL wclock_off (ng, iNLM, 16)
# endif
      RETURN
      END SUBROUTINE marsh_sed_bedload_mode1
!
!***********************************************************************
      SUBROUTINE marsh_sed_bedload_mode1_tile (ng, tile,                &
     &                             LBi, UBi, LBj, UBj,                  &
     &                             IminS, ImaxS, JminS, JmaxS,          &
     &                             om_r, on_r,                          &
     &                             nstp, nnew,                          &
     &                             Thrust_w, Thrust_e,                  &
     &                             Thrust_n, Thrust_s,                  &
# ifdef MARSH_LAT_RETREAT
     &                             marsh_flux_e, marsh_flux_w,          &
     &                             marsh_flux_n, marsh_flux_s,          &
# endif 
     &                             bed_frac, bed, bed_mass,             &
     &                             marsh_mask)
!***********************************************************************
!
      USE mod_param
      USE mod_ncparam
      USE mod_scalars
      USE mod_vegetation 
      USE mod_vegarr
      USE mod_sediment
      USE marsh_wave_thrust_mod
!
      USE bc_2d_mod, ONLY : bc_r2d_tile
      USE bc_3d_mod, ONLY : bc_r3d_tile
# ifdef DISTRIBUTE
      USE mp_exchange_mod, ONLY : mp_exchange2d, mp_exchange4d
# endif
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
      integer, intent(in) :: nstp, nnew
!
# ifdef ASSUMED_SHAPE
      real(r8), intent(in) :: om_r(LBi:,LBj:)
      real(r8), intent(in) :: on_r(LBi:,LBj:)
!
      real(r8), intent(in) :: Thrust_w(LBi:,LBj:)
      real(r8), intent(in) :: Thrust_e(LBi:,LBj:)
      real(r8), intent(in) :: Thrust_n(LBi:,LBj:)
      real(r8), intent(in) :: Thrust_s(LBi:,LBj:)
# ifdef MARSH_LAT_RETREAT
      real(r8), intent(inout) :: marsh_flux_w(LBi:,LBj:)
      real(r8), intent(inout) :: marsh_flux_e(LBi:,LBj:)
      real(r8), intent(inout) :: marsh_flux_n(LBi:,LBj:)
      real(r8), intent(inout) :: marsh_flux_s(LBi:,LBj:)
# endif 
!
      real(r8), intent(inout) :: bed(LBi:,LBj:,:,:)
      real(r8), intent(inout) :: bed_frac(LBi:,LBj:,:,:)
      real(r8), intent(inout) :: bed_mass(LBi:,LBj:,:,:,:)
!
      real(r8), intent(inout) :: marsh_mask(LBi:,LBj:)
# else
      real(r8), intent(in) :: om_r(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: on_r(LBi:UBi,LBj:UBj)
!
      real(r8), intent(in) :: Thrust_w(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: Thrust_e(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: Thrust_n(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: Thrust_s(LBi:UBi,LBj:UBj)
# ifdef MARSH_LAT_RETREAT
      real(r8), intent(inout) :: marsh_flux_w(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: marsh_flux_e(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: marsh_flux_n(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: marsh_flux_s(LBi:UBi,LBj:UBj)
# endif 
!
      real(r8), intent(inout) :: bed(LBi:UBi,LBj:UBj,Nbed,MBEDP)
      real(r8), intent(inout) :: bed_frac(LBi:UBi,LBj:UBj,Nbed,NST)
      real(r8), intent(inout) :: bed_mass(LBi:UBi,LBj:UBj,Nbed,1:2,NST)
!
      real(r8), intent(inout) :: marsh_mask(LBi:UBi,LBj:UBj)
# endif
!
!  Local variable declarations.
!
      integer :: i, j, k, ised 
      real(r8) :: cff, cff1, cff2, cff3, cff4
      real(r8) :: Thrust_crit, Eff_thrust 
!
# include "set_bounds.h"
!
# if defined MARSH_LAT_RETREAT
!      marsh_flux_e=0.0_r8
!      marsh_flux_w=0.0_r8
!      marsh_flux_n=0.0_r8
!      marsh_flux_s=0.0_r8
# endif 
!
!-----------------------------------------------------------------------
! Critical thrust or erosion thrust value to be decided 
!-----------------------------------------------------------------------
!
      Thrust_crit=0.0_r8
!-----------------------------------------------------------------------
!
      DO ised=NCS+1,NST
        DO j=Jstr,Jend
          DO i=Istr,Iend
!
!  |           |Thrust North |           |
!--|---------- |-------------|---------  |-------------
!  |Thrust West|   Marsh     |Thrust East| 
!  |-----------|-------------|-----------|
!  |           |Thrust South |           |    
!
! 
! Thrust has units kNewton/m or kg/sec2 
! kfac needs to have the units of kg-m/sec 
! Thrust*kfac has to have units of kg 
!
! A fraction of bed mass moves from this cell to either
! of the four neighboring cells if a critical thrust exceeds 
!
!|------|-----     |   -----     | -----   |-------------
!|      |          |   marsh     |         | 
!|      |  <----   |  <----->    | ---->   |
!|      | add sed  | remove sed  | add sed |   
!|
!
!
            cff1=bed_mass(i,j,1,nnew,ised)
!
!-----------------------------------------------------------------------
! Thrust from West cell face leads to sediment addition on left cell
!-----------------------------------------------------------------------
!
            cff=on_r(i,j)*dt(ng)
            Eff_thrust=MAX((ABS(Thrust_w(i,j))-Thrust_crit),0.0_r8)
            cff2=MIN(Eff_thrust*kfac_marsh(ng)*cff*cff1,cff1)
            bed_mass(i-1,j,1,nnew,ised)=bed_mass(i-1,j,1,nnew,ised)+    &
                                                               cff2
!
# if defined MARSH_LAT_RETREAT
!
! Save the marsh flux 
!
            marsh_flux_w(i,j)=cff2+marsh_flux_w(i,j)
# endif 
!
            IF(Eff_thrust.gt.0.0_r8) THEN
              cff3=1.0_r8
              cff1=MAX(cff1-cff2*cff3,0.0_r8)
!
! Save the updated bed mass from the eroded cell
! 
              bed_mass(i,j,1,nnew,ised)=cff1
!
!  Get the updated bed thickness 
!
                CALL update_bed_thickness (ng, tile, LBi, UBi, LBj, UBj,& 
     &                                   Istr, Iend, Jstr, Jend,        &
     &                                   nnew, bed , bed_frac, bed_mass)
!
!   If the marsh cell bed thickness is equal to wet cell infront of it
!   Then make the marsh cell as a wet cell i.e. marsh_mask=0.0  
!   Then the sediment is not supplied indefinitely to a cell infront 
!
                cff4=(bed(i,j,1,ithck)-bed(i-1,j,1,ithck))*marsh_mask(i,j)
                IF (cff4.gt.0.0) THEN 
                  marsh_mask(i,j)=0.0_r8
                END IF 
              ENDIF
!
!-----------------------------------------------------------------------
! Thrust from East cell face leads to sediment addition on right cell
!-----------------------------------------------------------------------
!
            Eff_thrust=MAX((ABS(Thrust_e(i+1,j))-Thrust_crit),0.0_r8)   
            cff2=MIN(Eff_thrust*kfac_marsh(ng)*cff*cff1,cff1)
            bed_mass(i+1,j,1,nnew,ised)=bed_mass(i+1,j,1,nnew,ised)+    &
                                                               cff2
!
# if defined MARSH_LAT_RETREAT
!
! Save the marsh flux 
!
            marsh_flux_e(i+1,j)=cff2+marsh_flux_e(i+1,j)
# endif 
!
            IF(Eff_thrust.gt.0.0_r8) THEN
              cff3=1.0_r8
              cff1=MAX(cff1-cff2*cff3,0.0_r8)
!
! Save the updated bed mass from the eroded cell
! 
              bed_mass(i,j,1,nnew,ised)=cff1 
!
!  Get the updated bed thickness 
!
              CALL update_bed_thickness (ng, tile, LBi, UBi, LBj, UBj,  & 
     &                                   Istr, Iend, Jstr, Jend,        &
     &                                   nnew, bed , bed_frac, bed_mass)
!
!   If the marsh cell bed thickness is equal to wet cell infront of it
!   Then make the marsh cell as a wet cell i.e. marsh_mask=0.0  
!   Then the sediment is not supplied indefinitely to a cell infront 
!
              cff4=(bed(i+1,j,1,ithck)-bed(i,j,1,ithck))*marsh_mask(i,j)
              IF (cff4.gt.0.0) THEN 
                marsh_mask(i,j)=0.0_r8
              END IF 
            ENDIF
!
!-----------------------------------------------------------------------
! Thrust from North cell face leads to sediment addition on cell above
!-----------------------------------------------------------------------
!
            cff=om_r(i,j)*dt(ng)
!
            Eff_thrust=MAX((ABS(Thrust_n(i,j+1))-Thrust_crit),0.0_r8)
            cff2=MIN(Eff_thrust*kfac_marsh(ng)*cff*cff1,cff1)
            bed_mass(i,j+1,1,nnew,ised)=bed_mass(i,j+1,1,nnew,ised)+    &
                                                               cff2
!
# if defined MARSH_LAT_RETREAT
!
! Save the marsh flux
! 
            marsh_flux_n(i,j+1)=cff2+marsh_flux_n(i,j+1)
# endif
!
            IF(Eff_thrust.gt.0.0_r8) THEN
              cff3=1.0_r8
              cff1=MAX((cff1-cff2*cff3),0.0_r8)
!
! Save the updated bed mass from the eroded cell
! 
              bed_mass(i,j,1,nnew,ised)=cff1 
!
              CALL update_bed_thickness (ng, tile, LBi, UBi, LBj, UBj,  & 
     &                                   Istr, Iend, Jstr, Jend,        &
     &                                   nnew, bed , bed_frac, bed_mass)
!
!   If the marsh cell bed thickness is equal to wet cell infront of it
!   Then make the marsh cell as a wet cell i.e. marsh_mask=0.0  
!   Then the sediment is not supplied indefinitely to a cell infront 
!
              cff4=(bed(i,j+1,1,ithck)-bed(i,j,1,ithck))*marsh_mask(i,j)
              IF (cff4.gt.0.0) THEN 
                marsh_mask(i,j)=0.0_r8
              END IF 
!
            ENDIF
!
!-----------------------------------------------------------------------
! Thrust from South cell face leads to sediment addition on cell below
!-----------------------------------------------------------------------
!
            Eff_thrust=MAX((ABS(Thrust_s(i,j))-Thrust_crit),0.0_r8)
            cff2=MIN(Eff_thrust*kfac_marsh(ng)*cff*cff1,cff1)
            bed_mass(i,j-1,1,nnew,ised)=bed_mass(i,j-1,1,nnew,ised)+    &
                                                               cff2
!
# if defined MARSH_LAT_RETREAT
!
! Save the marsh flux
! 
            marsh_flux_s(i,j)=cff2+marsh_flux_s(i,j)
# endif 
!
            IF(Eff_thrust.gt.0.0_r8) THEN
              cff3=1.0_r8
              cff1=MAX((cff1-cff2*cff3),0.0_r8)
!
! Save the updated bed mass from the eroded cell
! 
              bed_mass(i,j,1,nnew,ised)=cff1 
!
!  Get the updated bed thickness 
!
              CALL update_bed_thickness (ng, tile, LBi, UBi, LBj, UBj,  & 
     &                                   Istr, Iend, Jstr, Jend,        &
     &                                   nnew, bed , bed_frac, bed_mass)
!
!   If the marsh cell bed thickness is equal to wet cell infront of it
!   Then make the marsh cell as a wet cell i.e. marsh_mask=0.0  
!   Then the sediment is not supplied indefinitely to a cell infront 
!
              cff4=(bed(i,j-1,1,ithck)-bed(i,j,1,ithck))*marsh_mask(i,j)
              IF (cff4.gt.0.0) THEN 
                marsh_mask(i,j)=0.0_r8
              END IF 
!
            ENDIF
!
! First criteria of converting marsh cell to wet cell 
! 
            IF (bed(i,j,1,ithck).le.dcrit_marsh(ng)) THEN 
              marsh_mask(i,j)=0.0_r8
            END IF 
!
          END DO
        END DO 
      END DO
!
!---------------------------------------------------------------------
!  Apply periodic or gradient boundary conditions for output
!  purposes only.
!---------------------------------------------------------------------
!
      CALL bc_r2d_tile (ng, tile,                                       &
     &                  LBi, UBi, LBj, UBj,                             &
     &                  marsh_mask)
      DO ised=1,NST
        CALL bc_r3d_tile (ng, tile,                                     &
     &                    LBi, UBi, LBj, UBj, 1, Nbed,                  &
     &                    bed_frac(:,:,:,ised))
        CALL bc_r3d_tile (ng, tile,                                     &
     &                    LBi, UBi, LBj, UBj, 1, Nbed,                  &
     &                    bed_mass(:,:,:,nnew,ised))
      END DO
# ifdef DISTRIBUTE
        CALL mp_exchange2d (ng, tile, iNLM, 2,                          &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        NghostPoints,                             &
     &                        EWperiodic(ng), NSperiodic(ng),           &
     &                        marsh_mask)
!
      CALL mp_exchange4d (ng, tile, iNLM, 2,                            &
     &                    LBi, UBi, LBj, UBj, 1, Nbed, 1, NST,          &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    bed_frac,                                     &
     &                    bed_mass(:,:,:,nnew,:))
# endif
      DO i=1,MBEDP
        CALL bc_r3d_tile (ng, tile,                                     &
     &                    LBi, UBi, LBj, UBj, 1, Nbed,                  &
     &                    bed(:,:,:,i))
      END DO
# ifdef DISTRIBUTE
      CALL mp_exchange4d (ng, tile, iNLM, 1,                            &
     &                    LBi, UBi, LBj, UBj, 1, Nbed, 1, MBEDP,        &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    bed) 
# endif
# if defined MARSH_LAT_RETREAT
!     CALL bc_u2d_tile (ng, tile,                                        &
!     &                  LBi, UBi, LBj, UBj,                             &
!     &                  marsh_flux_w)
!     CALL bc_u2d_tile (ng, tile,                                        &
!     &                 LBi, UBi, LBj, UBj,                              &
!     &                  marsh_flux_e)
!     CALL bc_v2d_tile (ng, tile,                                        &
!     &                 LBi, UBi, LBj, UBj,                              &
!     &                  marsh_flux_n)
!     CALL bc_v2d_tile (ng, tile,                                        &
!     &                 LBi, UBi, LBj, UBj,                              &
!     &                  marsh_flux_s)
!
#  ifdef DISTRIBUTE
      CALL mp_exchange2d (ng, tile, iNLM, 4,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    marsh_flux_w, marsh_flux_e,                   &
     &                    marsh_flux_n, marsh_flux_s)         
#  endif 
# endif
!
      RETURN
      END SUBROUTINE marsh_sed_bedload_mode1_tile
!
      SUBROUTINE update_bed_thickness (ng, tile, LBi, UBi, LBj, UBj,    & 
     &                                  Istr, Iend, Jstr, Jend,         &
     &                                 nnew, bed, bed_frac, bed_mass)
! 
      USE mod_sediment
      USE mod_sedbed
      USE mod_param
      USE mod_scalars 
!
      implicit none
!
      integer, intent(in) :: ng, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: Istr, Iend, Jstr, Jend
      integer, intent(in) :: nnew
!
#  ifdef ASSUMED_SHAPE
      real(r8), intent(inout) :: bed(LBi:,LBj:,:,:)
      real(r8), intent(inout) :: bed_frac(LBi:,LBj:,:,:)
      real(r8), intent(inout) :: bed_mass(LBi:,LBj:,:,:,:)
#  else
      real(r8), intent(inout) :: bed(LBi:UBi,LBj:UBj,Nbed,MBEDP)
      real(r8), intent(inout) :: bed_frac(LBi:UBi,LBj:UBj,Nbed,NST)
      real(r8), intent(inout) :: bed_mass(LBi:UBi,LBj:UBj,Nbed,1:2,NST)
#  endif
!
!  Local variable declarations.
!
      integer :: i, ised, j, k
      real(r8) :: cff3
      real(r8), parameter :: eps = 1.0E-14_r8 !
!
! recalculate thickness and fractions for all layers.
!
      DO j=Jstr,Jend
        DO i=Istr,Iend
          cff3=0.0_r8
          DO ised=1,NST
            cff3=cff3+bed_mass(i,j,1,nnew,ised)
          END DO
          IF (cff3.eq.0.0_r8) THEN
            cff3=eps
          END IF
          bed(i,j,1,ithck)=0.0_r8
          DO ised=1,NST
            bed_frac(i,j,1,ised)=bed_mass(i,j,1,nnew,ised)/cff3
            bed(i,j,1,ithck)=MAX(bed(i,j,1,ithck)+                      &
     &                       bed_mass(i,j,1,nnew,ised)/                 &
     &                       (Srho(ised,ng)*                            &
     &                        (1.0_r8-bed(i,j,1,iporo))),0.0_r8)
          END DO
        END DO 
      END DO 
!
      END SUBROUTINE update_bed_thickness
#endif
      END MODULE marsh_sed_bedload_mode1_mod
