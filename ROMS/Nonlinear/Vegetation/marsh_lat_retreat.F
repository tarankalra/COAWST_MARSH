#include "cppdefs.h"

      MODULE marsh_lat_retreat_mod

#if defined MARSH_WAVE_EROSION && defined MARSH_WAVE_THRUST \
 && defined MARSH_SED_BEDLOAD_MODE1 || defined MARSH_SED_BEDLOAD_MODE2 \
 && defined MARSH_LAT_RETREAT 
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
      PUBLIC  :: marsh_lat_retreat

      CONTAINS
!
!***********************************************************************
      SUBROUTINE marsh_lat_retreat (ng, tile)
!***********************************************************************
!
      USE mod_grid
      USE mod_ocean
      USE mod_vegetation 
      USE mod_vegarr
      USE mod_sediment
      USE mod_sedbed
      USE marsh_sed_bedload_mode1_mod
!      USE marsh_wave_thrust_mod
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
!
!  Local variable declarations.
!
# include "tile.h"
!
      CALL marsh_lat_retreat_tile (ng, tile,                            & 
     &                       LBi, UBi, LBj, UBj,                        &
     &                       IminS, ImaxS, JminS, JmaxS,                &
     &                       GRID(ng) % om_r,                           &
     &                       GRID(ng) % on_r,                           &
     &                       GRID(ng) % h,                              &
     &                       OCEAN(ng) % zeta,                          & 
     &                       VEG(ng) % marsh_mask,                      & 
     &                       VEG(ng) % marsh_flux_e,                    &
     &                       VEG(ng) % marsh_flux_w,                    &
     &                       VEG(ng) % marsh_flux_n,                    &
     &                       VEG(ng) % marsh_flux_s,                    & 
     &                       SEDBED(ng) % bed,                          &
     &                       VEG(ng) % marsh_retreat)
      RETURN
      END SUBROUTINE marsh_lat_retreat
!
!***********************************************************************
!
      SUBROUTINE marsh_lat_retreat_tile (ng, tile,                &
     &                             LBi, UBi, LBj, UBj,                  &
     &                             IminS, ImaxS, JminS, JmaxS,          &
     &                             om_r, on_r,                          &
     &                             h, zeta,                             &
     &                             marsh_mask,                          &
     &                             marsh_flux_e, marsh_flux_w,          &
     &                             marsh_flux_n, marsh_flux_s,          & 
     &                             bed,                                 &
     &                             marsh_retreat)
!
!***********************************************************************
!
      USE mod_param
!      USE mod_ncparam
      USE mod_scalars
      USE mod_vegetation 
      USE mod_vegarr
      USE mod_sediment
      USE mod_sedbed
!      USE marsh_wave_thrust_mod
      USE marsh_sed_bedload_mode1_mod
      USE bc_2d_mod, ONLY : bc_r2d_tile
      USE bc_3d_mod, ONLY : bc_r3d_tile
!
      USE exchange_2d_mod, ONLY : exchange_r2d_tile
# ifdef DISTRIBUTE
      USE mp_exchange_mod, ONLY : mp_exchange2d, mp_exchange3d
      USE mp_exchange_mod, ONLY : mp_exchange4d
# endif
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
!      integer, intent(in) :: nstp, nnew
!
# ifdef ASSUMED_SHAPE
      real(r8), intent(in) :: om_r(LBi:,LBj:)
      real(r8), intent(in) :: on_r(LBi:,LBj:)
      real(r8), intent(in) :: h(LBi:,LBj:)
      real(r8), intent(in) :: zeta(LBi:,LBj:,:)
!
      real(r8), intent(in) :: marsh_mask(LBi:,LBj:)
      real(r8), intent(in) :: marsh_flux_w(LBi:,LBj:)
      real(r8), intent(in) :: marsh_flux_e(LBi:,LBj:)
      real(r8), intent(in) :: marsh_flux_n(LBi:,LBj:)
      real(r8), intent(in) :: marsh_flux_s(LBi:,LBj:)
! 
      real(r8), intent(in) :: bed(LBi:,LBj:,:,:)
!
      real(r8), intent(inout) :: marsh_retreat(LBi:,LBj:)
# else
      real(r8), intent(in) :: om_r(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: on_r(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: h(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: zeta(LBi:UBi,LBj:UBj,3)
!
      real(r8), intent(in) :: marsh_mask(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: marsh_flux_w(LBi:UBi,LBj:UBj) 
      real(r8), intent(in) :: marsh_flux_e(LBi:UBi,LBj:UBj) 
      real(r8), intent(in) :: marsh_flux_n(LBi:UBi,LBj:UBj) 
      real(r8), intent(in) :: marsh_flux_s(LBi:UBi,LBj:UBj)
!
      real(r8), intent(in) :: bed(LBi:UBi,LBj:UBj,Nbed,MBEDP)
!
      real(r8), intent(inout) :: marsh_retreat(LBi:UBi,LBj:UBj)
# endif
!
!  Local variable declarations.
!
      integer :: i, ised, j, k
      real(r8) :: cff, cff1
      real(r8) :: Vol_erod, scarp_height_eff
      real(r8) :: dx_e, dx_w, dy_n, dy_s 
!
# include "set_bounds.h"
!
!-----------------------------------------------------------------------
! Compute some constant of marsh sediment (May be would be input). 
!-----------------------------------------------------------------------
!     
      DO ised=NCS+1,NST
        DO j=Jstr,Jend
          DO i=Istr,Iend
! 
! Get the area and effective density to obtain eroded volume 
! 
            cff=1.0_r8/(om_r(i,j)*on_r(i,j))
            cff1=1.0_r8/(Srho(ised,ng)*(1.0_r8-bed(i,j,1,iporo)))
            scarp_height_eff=h(i,j)+zeta(i,j,3)
!
!  |           | Flux North  |         |
!--|---------- |-------------|---------|-------------
!  |Flux West  |   Marsh     |Flux East| 
!  |-----------|-------------|---------|
!  |           |Flux South   |         |    
!
!-----------------------------------------------------------------------
! West cell face erosion volume
!-----------------------------------------------------------------------
!
            Vol_erod=marsh_flux_w(i,j)*cff*cff1
            dx_e=Vol_erod/(scarp_height_eff*on_r(i,j))
!
!-----------------------------------------------------------------------
! East cell face erosion volume
!-----------------------------------------------------------------------
!
            Vol_erod=marsh_flux_e(i+1,j)*cff*cff1
            dx_w=Vol_erod/(scarp_height_eff*on_r(i,j))
!
!-----------------------------------------------------------------------
! North cell face erosion volume
!-----------------------------------------------------------------------
!
            Vol_erod=marsh_flux_n(i,j+1)*cff*cff1
            dy_n=Vol_erod/(scarp_height_eff*om_r(i,j))
!
!-----------------------------------------------------------------------
! South cell face erosion volume
!-----------------------------------------------------------------------
!
            Vol_erod=marsh_flux_s(i,j)*cff*cff1
            dy_s=Vol_erod/(scarp_height_eff*om_r(i,j))
!
            marsh_retreat(i,j)=marsh_mask(i,j)*(dx_e+dx_w+dy_n+dy_s)
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
     &                  marsh_retreat)
# ifdef DISTRIBUTE
        CALL mp_exchange2d (ng, tile, iNLM, 1,                          &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        NghostPoints,                             &
     &                        EWperiodic(ng), NSperiodic(ng),           &
     &                        marsh_retreat)
# endif
!
      RETURN
      END SUBROUTINE marsh_lat_retreat_tile
#endif
      END MODULE marsh_lat_retreat_mod
