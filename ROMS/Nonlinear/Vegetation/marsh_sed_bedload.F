#include "cppdefs.h"

      MODULE marsh_sed_bedload_mod

#if defined SEDIMENT && defined BEDLOAD && defined MARSH_WAVE_THRUST \
    && defined MARSH_SED_BEDLOAD 
!
!svn $Id: marsh_wave_thrust.F 429 2015-04-20 17:30:26Z arango $
!=======================================================================
!  Copyright (c) 2002-2016 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license           Hernan G. Arango   !
!    See License_ROMS.txt                   Alexander F. Shchepetkin   !
!================================================John C. Warner=========
!================================================Neil K. Ganju  ========
!==============================================Tarandeep S. Kalra=======
!                                                                      ! 
!  References:                                                         !
!                                                                      !
!=======================================================================
!
      implicit none

      PRIVATE
      PUBLIC  :: marsh_sed_bedload

      CONTAINS
!
!***********************************************************************
      SUBROUTINE marsh_sed_bedload (ng, tile)
!***********************************************************************
!
      USE mod_param
      USE mod_forces
      USE mod_grid
      USE mod_ocean
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
      CALL marsh_sed_bedload_tile (ng, tile,                            &
     &                       LBi, UBi, LBj, UBj,                        &
     &                       IminS, ImaxS, JminS, JmaxS,                &
     &                       GRID(ng) % om_r,                           &
     &                       GRID(ng) % on_r,                           &
     &                       nstp(ng), nnew(ng),                        &
     &                       VEG(ng) % Thrust_w,                        & 
     &                       VEG(ng) % Thrust_e,                        &
     &                       VEG(ng) % Thrust_n,                        &
     &                       VEG(ng) % Thrust_s,                        &
     &                       SEDBED(ng) % bed_mass,                     &
     &                       VEG(ng)    % marsh_mask)
# ifdef PROFILE
      CALL wclock_off (ng, iNLM, 16)
# endif
      RETURN
      END SUBROUTINE marsh_sed_bedload
!
!***********************************************************************
      SUBROUTINE marsh_sed_bedload_tile (ng, tile,                      &
     &                             LBi, UBi, LBj, UBj,                  &
     &                             IminS, ImaxS, JminS, JmaxS,          &
     &                             om_r, on_r,                          &
     &                             nstp, nnew,                          &
     &                             Thrust_w, Thrust_e,                  &
     &                             Thrust_n, Thrust_s,                  &
     &                             bed_mass,                            &
     &                             marsh_mask)
!***********************************************************************
!
      USE mod_param
      USE mod_ncparam
      USE mod_scalars
      USE mod_vegarr
      USE mod_sediment
      USE marsh_wave_thrust_mod
!
      USE bc_3d_mod, ONLY : bc_r3d_tile
# ifdef BEDLOAD
      USE exchange_2d_mod, ONLY : exchange_u2d_tile, exchange_v2d_tile
# endif
# ifdef DISTRIBUTE
      USE mp_exchange_mod, ONLY : mp_exchange3d, mp_exchange4d
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
      real(r8), intent(in) :: Thrust_w(LBi:,LBj:)
      real(r8), intent(in) :: Thrust_e(LBi:,LBj:)
      real(r8), intent(in) :: Thrust_n(LBi:,LBj:)
      real(r8), intent(in) :: Thrust_s(LBi:,LBj:)
      real(r8), intent(in) :: om_r(LBi:,LBj:)
      real(r8), intent(in) :: on_r(LBi:,LBj:)
      real(r8), intent(inout) :: bed_mass(LBi:,LBj:,:,:,:)
      real(r8), intent(inout) :: marsh_mask(LBi:,LBj:)
# else
      real(r8), intent(in) :: Thrust_w(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: Thrust_e(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: Thrust_n(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: Thrust_s(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: h(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: om_r(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: on_r(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: bed_mass(LBi:UBi,LBj:UBj,Nbed,1:2,NST)
      real(r8), intent(inout) :: marsh_mask(LBi:UBi,LBj:UBj)
# endif
!
!  Local variable declarations.
!
      integer :: i, ised, j, k
      real(r8) :: cff

      real(r8) :: upper_limit_bedmass, lower_limit_bedmass
      real(r8) :: kfac, Eff_thrust, Thrust_crit

# include "set_bounds.h"
!
!-----------------------------------------------------------------------
!  Compute bedload sediment transport.
!-----------------------------------------------------------------------
!
! Compute some constant of marsh sediment (May be would be input). 
!
!     
! kfac will depend on sediment cohesive properties 
! Critical thrust before Thrust exceeds on each cell face
! leads to marsh erosion 
!
      kfac=1.0_r8  
!
! Critical thrust or erosion thrust value to be decided 
!
      Thrust_crit=0.0_r8

      DO ised=NCS+1,NST
        DO j=Jstr,Jend
          DO i=Istr,Iend
! 
! Magnitude of bed load at rho points. Meyer-Peter Muller formulation.
! bedld has dimensions of kg m-1 s-1. Use partitions of stress
! from upwind direction, still at rho points.
! (FX_r and FE_r have dimensions of kg).
!
! Thrust has units Newton/m or kg/sec2 
! kfac needs to have the units of kg-m/sec
! Thrust*kfac has to have units of kg 
! REPLACE THIS WITH YOUR FORMULATION TAU_WX and TAU_WE 
!
! A fraction of bed mass moves from this cell to either
! of the four neighboring cells if a critical thrust exceeds 
!
!|------|-----|-----|-----|-------------
!|      |---->|marsh|<--- |
!|      |     |     |     |
!|      |added|subt.|added|
!|
! Thrust_xi and Thrust_eta always act at cell face
!           
             cff=on_r(i,j)*dt(ng)
!
             Eff_thrust=ABS(Thrust_w(i,j))-Thrust_crit
!
! Add the bed mass from the adjacent cell where thrust is acting 
!
             bed_mass(i-1,j,1,nstp,ised)=Eff_thrust*kfac*               &
      &      cff*bed_mass(i,j,1,nstp,ised)+bed_mass(i-1,j,1,nstp,ised)
! 
! Subtract the bed mass that entered the left cell from current cell
!
             bed_mass(i,j,1,nstp,ised)=bed_mass(i,j,1,nstp,ised)-       &
      &                                bed_mass(i-1,j,1,nstp,ised)
!
             Eff_thrust=ABS(Thrust_e(i+1,j))-Thrust_crit
             bed_mass(i+1,j,1,nstp,ised)=Eff_thrust*kfac*               & 
      &       cff*bed_mass(i,j,1,nstp,ised)+bed_mass(i+1,j,1,nstp,ised)
             bed_mass(i,j,1,nstp,ised)=bed_mass(i,j,1,nstp,ised)-       &
      &                                bed_mass(i+1,j,1,nstp,ised)

             cff=om_r(i,j)*dt(ng)
             Eff_thrust=ABS(Thrust_s(i,j))-Thrust_crit
!
! Add the bed mass from the adjacent cell where thrust is acting 
!
             bed_mass(i,j-1,1,nstp,ised)=Eff_thrust*kfac*               &
      &       cff*bed_mass(i,j,1,nstp,ised)+bed_mass(i,j-1,1,nstp,ised)
             bed_mass(i,j,1,nstp,ised)=bed_mass(i,j,1,nstp,ised)-       &
      &                                bed_mass(i,j-1,1,nstp,ised)
!
             Eff_thrust=ABS(Thrust_n(i,j+1))-Thrust_crit
             bed_mass(i,j+1,1,nstp,ised)=Eff_thrust*kfac*               &
      &       cff*bed_mass(i,j,1,nstp,ised)+bed_mass(i,j+1,1,nstp,ised)
             bed_mass(i,j,1,nstp,ised)=bed_mass(i,j,1,nstp,ised)-       &
      &                                bed_mass(i,j+1,1,nstp,ised)
!            
          END DO
        END DO 
! 
! If the bed mass decreases a certain value in a cell or exceeds a
! certain value in another cell, change the marsh_mask
!
      upper_limit_bedmass=1.0_r8
      lower_limit_bedmass=0.5_r8
!
          DO j=Jstr,Jend
            DO i=Istr,Iend
              bed_mass=bed_mass(i,j,1,nstp,ised)
!              cff=MAX(bed_mass,upper_limit_bedmass)
!              IF ( bed_mass > upper_limit_bedmass ) THEN
!                marsh_mask(i,j)=1.0_r8
!              ELSEIF ( bed_mass < lower_limit_bedmass ) THEN 
!                marsh_mask(i,j)=0.0_r8
!              ENDIF
            END DO 
          END DO

        END DO 
 
      RETURN

      END SUBROUTINE marsh_sed_bedload_tile
#endif
      END MODULE marsh_sed_bedload_mod
