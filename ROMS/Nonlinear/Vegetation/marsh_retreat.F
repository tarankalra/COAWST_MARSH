#include "cppdefs.h"

      MODULE marsh_lat_retreat_mod

#if defined MARSH_WAVE_EROSION && defined MARSH_RETREAT
!
!svn $Id: marsh_wave_thrust.F 429 2015-04-20 17:30:26Z arango $
!=======================================================================
!  Copyright (c) 2002-2016 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license           Hernan G. Arango   !
!    See License_ROMS.txt                   Alexander F. Shchepetkin   !
!================================================Neil K. Ganju  =======!
!==============================================Tarandeep S. Kalra======!
!==============================================John C. Warner==========!
!==============================================Julia M. Moriarty=======!
!                                                                      ! 
!  References:                                                         !
!                                                                      !
!=======================================================================
!
      implicit none

      PRIVATE
      PUBLIC  :: marsh_retreat

      CONTAINS
!
!***********************************************************************
      SUBROUTINE marsh_retreat (ng, tile)
!***********************************************************************
!
      USE mod_grid
      USE mod_ocean
      USE mod_vegetation 
      USE mod_vegarr
      USE mod_sediment
      USE mod_sedbed
      USE marsh_wave_thrust
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
!
!  Local variable declarations.
!
# include "tile.h"
!
      CALL marsh_retreat_tile (ng, tile,                                &
     &                       LBi, UBi, LBj, UBj,                        &
     &                       IminS, ImaxS, JminS, JmaxS,                &
     &                       GRID(ng) % pm,                             &
     &                       GRID(ng) % pn,                             &
     &                       GRID(ng) % h,                              &
     &                       OCEAN(ng) % zeta,                          &
     &                       VEG(ng) % marsh_retreat)
      RETURN
      END SUBROUTINE marsh_retreat
!
!***********************************************************************
!
      SUBROUTINE marsh_retreat_tile (ng, tile,                          &
     &                             LBi, UBi, LBj, UBj,                  &
     &                             IminS, ImaxS, JminS, JmaxS,          &
     &                             marsh_mask,                          &
     &                             marsh_retreat)
!
!***********************************************************************
!
      USE mod_param
      USE mod_scalars
      USE mod_vegetation 
      USE mod_vegarr
      USE mod_sediment
      USE mod_sedbed
      USE mod_marsh_wave_thrust
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
!
# ifdef ASSUMED_SHAPE
      real(r8), intent(in) :: pm(LBi:,LBj:)
      real(r8), intent(in) :: pn(LBi:,LBj:)
      real(r8), intent(in) :: h(LBi:,LBj:)
      real(r8), intent(in) :: zeta(LBi:,LBj:,:)
!
      real(r8), intent(in) :: marsh_mask(LBi:,LBj:)
      real(r8), intent(in) :: Thrust_xi(LBi:,LBj:)
      real(r8), intent(in) :: Thrust_eta(LBi:,LBj:)
      real(r8), intent(inout) :: marsh_retreat(LBi:,LBj:)
# else
      real(r8), intent(in) :: pm(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: pn(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: h(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: zeta(LBi:UBi,LBj:UBj,3)
!
      real(r8), intent(in) :: marsh_mask(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: Thrust_xi(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: Thrust_eta(LBi:UBi,LBj:UBj)
!
      real(r8), intent(inout) :: marsh_retreat(LBi:UBi,LBj:UBj)
# endif
!
!  Local variable declarations.
!
      integer :: i, ised, j, k
      real(r8) :: cff, cff1, cff2, cff3
      real(r8) :: inv_scarp_hght_eff
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: srho_mean
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: dx_xi, dy_eta
!
# include "set_bounds.h"
!
      J_LOOP : DO j=Jstr,Jend
        DO i=Istr,Iend
          cff=1.0_r8
          DO ised=NCS+1,NST
            cff=cff*Srho(ised,ng)*bed_frac(i,j,1,ised)
          END DO
          srho_mean(i,j)=MAX(cff1,1050.0_r8)
        END DO
      END DO J_LOOP
!
!----------------------------------------------------------------------
! Calculated flux out in kg/m from marsh at U-points and V-points.
!----------------------------------------------------------------------
!
      DO j=JstrR,JendR
         DO i=Istr,IendR
          cff=kN_N*kfac_marsh(ng)*dt(ng)          
          flux_out=ABS(cff*Thrust_xi(i,j)*on_u(i,j))
!
          cff1=SIGN(1.0_r8,Thrust_xi(i,j))
          cff2=half*(1.0_r8+cff1)
          cff3=half*(1.0_r8-cff1)

          inv_scarp_hght_eff=cff2*(h(i,j-1)+zeta(i,j,1  ))+             &
                             cff3*(h(i,j  )+zeta(i-1,j,1))
          dx_xi(i,j)=flux_out/inv_scarp_hght_eff
        END DO
      END DO
!
      DO j=Jstr,JendR
        DO i=IstrR,IendR
!
          cff=kN_N*kfac_marsh(ng)*dt(ng)          
          flux_out=ABS(cff*Thrust_eta(i,j)*om_v(i,j))
!
          cff1=SIGN(1.0_r8,Thrust_eta(i,j))
          cff2=half*(1.0_r8+cff1)
          cff3=half*(1.0_r8-cff1)
          inv_scarp_hght_eff=cff2*(h(i,j-1)+zeta(i,j,1  ))+             &
                             cff3*(h(i,j  )+zeta(i,j-1,1))
          dy_eta(i,j)=flux_out/(inv_scarp_hght_eff)
        END DO
      END DO
!
# ifdef DISTRIBUTE
! 
! Exchange boundary data 
!
       CALL exchange_u2d_tile (ng, tile,                                &
     &                            LBi, UBi, LBj, UBj,                  &
     &                            dx_xi(:,:))
       CALL exchange_v2d_tile (ng, tile,                                &
     &                            LBi, UBi, LBj, UBj,                  &
     &                            dy_eta(:,:))
!
 # endif
! 
      DO j=Jstr,Jend
        DO i=Istr,Iend
!
!-----------------------------------------------------------------------
! Total amount of retreat from all four directions
!-----------------------------------------------------------------------
!
          marsh_retreat(i,j)=marsh_mask(i,j)*(dx_xi(i,j)+dx_xi(i+1,j)+  &
     &                                        dy_eta(i,j)+dy_eta(i,j+1))
!
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

