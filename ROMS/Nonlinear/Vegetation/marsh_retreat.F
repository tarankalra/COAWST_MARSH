#include "cppdefs.h"

      MODULE marsh_retreat_mod
#if defined MARSH_WAVE_EROSION && defined MARSH_RETREAT
!
!svn $Id: marsh_retreat.F 429 2015-04-20 17:30:26Z arango $
!=======================================================================
!  Copyright (c) 2002-2016 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license           Hernan G. Arango   !
!    See License_ROMS.txt                   Alexander F. Shchepetkin   !
!==============================================Tarandeep S. Kalra======!
!================================================Neil K. Ganju  =======!
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
      USE mod_sedbed
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
      CALL marsh_retreat_tile (ng, tile,                                &
     &                       LBi, UBi, LBj, UBj,                        &
     &                       IminS, ImaxS, JminS, JmaxS,                &
     &                       GRID(ng) % h,                              &
     &                       GRID(ng) % on_u,                           &
     &                       GRID(ng) % om_v,                           &
     &                       SEDBED(ng) % bed_frac,                     &
     &                       SEDBED(ng) % bed,                          &
     &                       VEG(ng) % marsh_mask,                      &
     &                       VEG(ng) % marsh_retreat,                   &
     &                       VEG(ng) % Thrust_xi,                       &
     &                       VEG(ng) % Thrust_eta,                      &
     &                       OCEAN(ng) % zeta)                          
# ifdef PROFILE
      CALL wclock_off (ng, iNLM, 16)
# endif
      RETURN
      END SUBROUTINE marsh_retreat
!
!***********************************************************************
!
      SUBROUTINE marsh_retreat_tile (ng, tile,                          &
     &                             LBi, UBi, LBj, UBj,                  &
     &                             IminS, ImaxS, JminS, JmaxS,          &
     &                             h,                                   &
     &                             on_u, om_v,                          &
     &                             bed_frac,                            &
     &                             bed,                                 &
     &                             marsh_mask,                          &
     &                             marsh_retreat,                       &
     &                             Thrust_xi,                           &
     &                             Thrust_eta,                          &
     &                             zeta)
!
!***********************************************************************
!
      USE mod_param
      USE mod_scalars
      USE mod_vegetation 
      USE mod_vegarr
      USE mod_sedbed
      USE marsh_wave_thrust_mod
      USE bc_2d_mod, ONLY : bc_r2d_tile
!
      USE exchange_2d_mod
# ifdef DISTRIBUTE
      USE mp_exchange_mod, ONLY : mp_exchange2d
# endif
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
!
# ifdef ASSUMED_SHAPE
      real(r8), intent(in) :: h(LBi:,LBj:)
      real(r8), intent(in) :: on_u(LBi:,LBj:)
      real(r8), intent(in) :: om_v(LBi:,LBj:)
      real(r8), intent(in) :: zeta(LBi:,LBj:,:)
!
      real(r8), intent(in) :: marsh_mask(LBi:,LBj:)
      real(r8), intent(in) :: Thrust_xi(LBi:,LBj:)
      real(r8), intent(in) :: Thrust_eta(LBi:,LBj:)
!
      real(r8), intent(in)  :: bed_frac(LBi:,LBj:,:,:)
      real(r8), intent(in)  :: bed(LBi:,LBj:,:,:)
!
      real(r8), intent(inout) :: marsh_retreat(LBi:,LBj:)
# else
      real(r8), intent(in) :: h(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: on_u(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: om_v(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: zeta(LBi:UBi,LBj:UBj,3)
!
      real(r8), intent(in) :: marsh_mask(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: Thrust_xi(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: Thrust_eta(LBi:UBi,LBj:UBj)
!
      real(r8), intent(in) :: bed_frac(LBi:UBi,LBj:UBj,Nbed,NST)
      real(r8), intent(in) :: bed(LBi:UBi,LBj:UBj,Nbed,MBEDP)
!
      real(r8), intent(inout) :: marsh_retreat(LBi:UBi,LBj:UBj)
# endif
!
!  Local variable declarations.
!
      integer :: i, ised, j, k
      real(r8) :: cff, cff1, cff2, cff3
      real(r8) :: flux_out, scarp_hght_eff

      real(r8), parameter :: half=0.5_r8, one=1.0_r8
      real(r8), parameter :: kN_N=1000.0_r8 ! convert Thrust from kN to N 

      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: srho_mean
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: dx_xi, dy_eta
!
# include "set_bounds.h"
!
!
!----------------------------------------------------------------------
! Calculated flux out in kg/m from marsh at U-points and V-points.
!----------------------------------------------------------------------
!
      DO j=JstrR,JendR
         DO i=Istr,IendR
          cff=kN_N*kfac_marsh(ng)*dt(ng)          
          flux_out=cff*Thrust_xi(i,j)
!
          cff1=SIGN(1.0_r8,Thrust_xi(i,j))
          cff2=half*(1.0_r8+cff1)
          cff3=half*(1.0_r8-cff1)
!
          scarp_hght_eff=cff2*(h(i-1,j)+zeta(i,j,1  ))+                 &
     &                   cff3*(h(i,j  )+zeta(i-1,j,1))
          dx_xi(i,j)=MIN(ABS(flux_out/scarp_hght_eff),0.0_r8)
!
        END DO
      END DO
!
      DO j=Jstr,JendR
        DO i=IstrR,IendR
          cff=kN_N*kfac_marsh(ng)*dt(ng)          
          flux_out=cff*Thrust_eta(i,j)  !kg/(m)
!
          cff1=SIGN(1.0_r8,Thrust_eta(i,j))
          cff2=half*(1.0_r8+cff1)
          cff3=half*(1.0_r8-cff1)
          scarp_hght_eff=cff2*(h(i,j-1)+zeta(i,j,1  ))+                 &
     &                   cff3*(h(i,j  )+zeta(i,j-1,1))
!
!-----------------------------------------------------------------------
! The formulation for effective scarp height can change. 
! Min keeps infinity values out when scarp height = 0.0. 
!-----------------------------------------------------------------------
!
          dy_eta(i,j)=MIN(ABS(flux_out/scarp_hght_eff),0.0_r8)
!
        END DO
      END DO
!
# ifdef DISTRIBUTE
! 
! Exchange boundary data 
!
       CALL exchange_u2d_tile (ng, tile,                                &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            dx_xi(:,:))
       CALL exchange_v2d_tile (ng, tile,                                &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            dy_eta(:,:))
!
# endif
!
      DO j=Jstr,Jend
        DO i=Istr,Iend
!
!-----------------------------------------------------------------------
! Total amount of retreat from all four directions.
!-----------------------------------------------------------------------
!
          cff=0.0_r8
          DO ised=1,NST
            cff1=1.0_r8/(Srho(ised,ng)*(1.0_r8-bed(i,j,1,iporo)))
            cff=cff+cff1*marsh_mask(i,j)*                               &
     &                                   (dx_xi(i,j) + dx_xi(i+1,j )+   &
     &                                    dy_eta(i,j)+dy_eta(i,j+1))
          END DO 
          marsh_retreat(i,j)=cff
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
      CALL mp_exchange2d (ng, tile, iNLM, 1,                            &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        NghostPoints,                             &
     &                        EWperiodic(ng), NSperiodic(ng),           &
     &                        marsh_retreat)
# endif
!
      END SUBROUTINE marsh_retreat_tile
#endif
      END MODULE marsh_retreat_mod
