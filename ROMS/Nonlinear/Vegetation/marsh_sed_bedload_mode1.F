#include "cppdefs.h"

      MODULE marsh_sed_bedload_mode1_mod

#if defined SEDIMENT && defined BEDLOAD && defined MARSH_WAVE_EROSION \
      && defined MARSH_SED_BEDLOAD_MODE1 
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
     &                       GRID(ng) % on_u,                           &
     &                       GRID(ng) % om_v,                           &
     &                       GRID(ng) % pm,                             &
     &                       GRID(ng) % pn,                             &
     &                       GRID(ng) % h,                              &
     &                       nstp(ng), nnew(ng),                        &
     &                       VEG(ng) % Thrust_e,                        &
     &                       VEG(ng) % Thrust_n,                        &
# ifdef MARSH_LAT_RETREAT
     &                       VEG(ng) % marsh_flux_out,                  &
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
     &                             on_u, om_v,                          &
     &                             pm, pn,                              &
     &                             h,                                   &
     &                             nstp, nnew,                          &
     &                             Thrust_e, Thrust_n,                  &
# ifdef MARSH_LAT_RETREAT
     &                             marsh_flux_out,                      &
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
      USE mp_exchange_mod, ONLY : mp_exchange2d, mp_exchange3d,         &
     &                            mp_exchange4d
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
      real(r8), intent(in) :: on_u(LBi:,LBj:)
      real(r8), intent(in) :: om_v(LBi:,LBj:)
      real(r8), intent(in) :: pm(LBi:,LBj:)
      real(r8), intent(in) :: pn(LBi:,LBj:)
      real(r8), intent(in) :: h(LBi:,LBj:)
!
      real(r8), intent(in) :: Thrust_e(LBi:,LBj:)
      real(r8), intent(in) :: Thrust_n(LBi:,LBj:)
# ifdef MARSH_LAT_RETREAT
      real(r8), intent(inout) :: marsh_flux_e(LBi:,LBj:)
      real(r8), intent(inout) :: marsh_flux_n(LBi:,LBj:)
      real(r8), intent(inout) :: marsh_flux_out(LBi:,LBj:,:)
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
      real(r8), intent(in) :: on_u(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: om_v(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: pm(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: pn(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: h(LBi:UBi,LBj:UBj)
!
      real(r8), intent(in) :: Thrust_e(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: Thrust_n(LBi:UBi,LBj:UBj)
# ifdef MARSH_LAT_RETREAT
      real(r8), intent(inout) :: marsh_flux_out(LBi:UBi,LBj:UBj,NST)
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
      real(r8), parameter :: half=0.5_r8
      real(r8), parameter :: eps = 1.0E-14_r8 !
      real(r8), parameter :: kN_N=1000.0_r8 ! convert Thrust from kN to N 
      real(r8) :: cff, cff1, cff2, cff3, cff4
      real(r8) :: bed_thick_old, bathy
!
# include "set_bounds.h"

# if defined MARSH_LAT_RETREAT
        marsh_flux_e=0.0_r8
        marsh_flux_n=0.0_r8
# endif 
!
        J_LOOP: DO j=Jstr,Jend
          DO i=Istr,Iend
            SED_LOOP: DO ised=NCS+1,NST
!
!---------------------------------------------------------------------
! Convert bed mass from kg/sq.m to kg in each cell.
!---------------------------------------------------------------------
!
              cff=bed_mass(i,j,1,nnew,ised)*om_r(i,j)*on_r(i,j)
!
!---------------------------------------------------------------------
! Add bed mass from sea cell providing horizontal face thrust.
!---------------------------------------------------------------------
!
              cff1=half*(1.0_r8-SIGN(1.0_r8,Thrust_e(i,j)))
              cff2=half*(1.0_r8+SIGN(1.0_r8,Thrust_e(i+1,j)))
              cff3=kN_N*kfac_marsh(ng)*dt(ng)*                          &
                            ( Thrust_e(i,j  )*on_u(i,j  )*cff1          &
                             -Thrust_e(i+1,j)*on_u(i+1,j)*cff2) 
              cff=cff+cff3
# ifdef MARSH_LAT_RETREAT
              marsh_sedflux_out(i,j,ised)=cff3
# endif 
!
!---------------------------------------------------------------------
! Subtract bed mass from marsh cell if thrust acts on horizontal face.
!---------------------------------------------------------------------
!
              cff1=half*(1.0_r8+SIGN(1.0_r8,Thrust_e(i,j)))
              cff2=half*(1.0_r8-SIGN(1.0_r8,Thrust_e(i+1,j)))
              cff3=-kN_N*kfac_marsh(ng)*dt(ng)*                         &
                            (  Thrust_e(i+1,j)*on_u(i+1,j)*cff2         &
                              -Thrust_e(i,j  )*on_u(i,j  )*cff1)
              cff=cff+cff3
!
!---------------------------------------------------------------------
! Add bed mass from sea cell if it provides vertical face thrust. 
!---------------------------------------------------------------------
!
              cff1=half*(1.0_r8-SIGN(1.0_r8,Thrust_n(i,j)))
              cff2=half*(1.0_r8+SIGN(1.0_r8,Thrust_n(i,j+1)))
              cff3=kN_N*kfac_marsh(ng)*dt(ng)*                          &
                            ( Thrust_n(i,j+1)*om_v(i,j+1)*cff2          &
                             -Thrust_n(i,j  )*om_v(i,j  )*cff1 )
              cff=cff+cff3
# ifdef MARSH_LAT_RETREAT
              marsh_sedflux_out(i,j,ised)=cff3+                         &
                                          marsh_sedflux_out(i,j,ised)
# endif 
!
!---------------------------------------------------------------------
! Subtract bed mass from marsh cell if thrust acts on vertical face. 
!---------------------------------------------------------------------
!
              cff1=half*(1.0_r8+SIGN(1.0_r8,Thrust_n(i,j  )))
              cff2=half*(1.0_r8-SIGN(1.0_r8,Thrust_n(i,j+1)))
              cff=cff-kN_N*kfac_marsh(ng)*dt(ng)*                       &
                            ( Thrust_n(i,j  )*om_v(i,j  )*cff1          &
                             -Thrust_n(i,j+1)*om_v(i,j+1)*cff2  )
!
!---------------------------------------------------------------------
! Update bed mass and convert back to kg-sq.m. 
!---------------------------------------------------------------------
!
              bed_mass(i,j,1,nnew,ised)=cff*pm(i,j)*pn(i,j)
!        
            END DO SED_LOOP
!
!---------------------------------------------------------------------
! recalculate thickness and fractions for all layers.
!---------------------------------------------------------------------
!
            bed_thick_old=bed(i,j,1,ithck)
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
              bed(i,j,1,ithck)=MAX(bed(i,j,1,ithck)+                    &
     &                       bed_mass(i,j,1,nnew,ised)/                 &
     &                       (Srho(ised,ng)*                            &
     &                       (1.0_r8-bed(i,j,1,iporo))),0.0_r8)
            END DO
!
!---------------------------------------------------------------------
! Local bathymetry calculation to change marsh mask.
!---------------------------------------------------------------------
!
          bathy=h(i,j)+bed(i,j,1,ithck)-bed_thick_old 
!
          IF(bathy.lt.DCRIT_MARSH(ng))THEN
!
!---------------------------------------------------------------------
! Reclaim marsh.
!---------------------------------------------------------------------
!
             marsh_mask(i,j)=1.0_r8
           ELSE
!
!---------------------------------------------------------------------
! Erode marsh.
!---------------------------------------------------------------------
!
             marsh_mask(i,j)=0.0_r8
           END IF
! 
        END DO
      END DO J_LOOP
!
!---------------------------------------------------------------------
!  Apply periodic or gradient boundary conditions for output
!  purposes only.
!---------------------------------------------------------------------
!
      CALL bc_r2d_tile (ng, tile,                                       &
     &                  LBi, UBi, LBj, UBj,                             &
                        marsh_mask)
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
      CALL mp_exchange3d (ng, tile, iNLM, 1,                            &
     &                    LBi, UBi, LBj, UBj, 1, NST,                   &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    marsh_flux_out)         
#  endif 
# endif
!
      RETURN 
      END SUBROUTINE marsh_sed_bedload_mode1_tile
#endif
      END MODULE marsh_sed_bedload_mode1_mod

