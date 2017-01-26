#include "cppdefs.h"
 
       MODULE marsh_wave_thrust_mod
#if defined VEGETATION && defined MARSH_WAVE_THRUST
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
!  This routine computes the wave thrust on marshes. Marsh thrust      !
!  values are computed with correction from the wave angle. For each   !
!  cell if one side is sheltered from other cells, that side is not    !
!  exposed to waves. Each cell has four cell normals directed towards  !
!  the center of the cell. The angle of the normals is with respect to !
!  the North and clockwise direction. For a submerged marsh,           !
!  "Tonelli mask" is used to reduce the value of the wave thrust.      !
!                                                                      !
!  References:                                                         !   
!                                                                      !
!=======================================================================
!                                                                      !
!  Tonelli, M., Fagherazzi, Sergio., and Petti., M., 2010: Modeling    !
!  wave impact on salt marsh boundaries, Journal of Geophysical        !
!  Research, 115, 0148-0227.                                           !   
!                                                                      !
!  Dean, R.G. and Dalrymple, R.A., 1991: Water Wave Mechanics for      !
!  Engineers and Scientists, World Scientific Publications             !
!                                                                      !
!=======================================================================

      implicit none

      PRIVATE
      PUBLIC  :: marsh_wave_thrust

      CONTAINS
!
!***********************************************************************
      SUBROUTINE marsh_wave_thrust (ng, tile)
!***********************************************************************
!
      USE mod_param
      USE mod_forces
      USE mod_grid
      USE mod_ocean 
      USE mod_stepping
      USE mod_vegarr
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
      CALL marsh_wave_thrust_tile  (ng, tile,                           &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        IminS, ImaxS, JminS, JmaxS,               &
     &                        nstp(ng),                                 &
     &                        GRID(ng) % h,                             &
     &                        GRID(ng) % angler,                        &
# ifdef MASKING
     &                       GRID(ng) % rmask,                          &
     &                       GRID(ng) % umask,                          &
     &                       GRID(ng) % vmask,                          &
# endif
# ifdef WET_DRY
     &                       GRID(ng) % rmask_wet,                      &
# endif
# if defined WAVES_HEIGHT 
     &                        FORCES(ng) % Hwave,                       &
# endif 
# if defined WAVES_LENGTH 
     &                        FORCES(ng) % Lwave,                       &
# endif 
# if defined WAVES_DIR 
     &                        FORCES(ng) % Dwave,                       &
# endif 
     &                        VEG(ng) % marsh_mask,                     &
     &                        VEG(ng) % mask_thrust,                    &
     &                        VEG(ng) % Thrust_x,                       &
     &                        VEG(ng) % Thrust_y,                       &
     &                        OCEAN(ng)  % zeta)
              
# ifdef PROFILE
      CALL wclock_off (ng, iNLM, 16)
# endif
      RETURN
      END SUBROUTINE marsh_wave_thrust
!
!***********************************************************************
!
      SUBROUTINE marsh_wave_thrust_tile  (ng, tile,                     &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              IminS, ImaxS, JminS, JmaxS,         &
     &                              nstp,                               &
     &                              h,angler,                           &
# ifdef MASKING
     &                             rmask, umask, vmask,                 &
# endif
# ifdef WET_DRY
     &                             rmask_wet,                           &
# endif
# if defined WAVES_HEIGHT  
     &                              Hwave,                              &
# endif 
# if defined WAVES_LENGTH  
     &                              Lwave,                              &
# endif 
# if defined WAVES_DIR  
     &                              Dwave,                              &
# endif 
     &                              marsh_mask,                         &
     &                              mask_thrust,                        &
     &                              Thrust_x, Thrust_y,                 & 
     &                              zeta)           
!
!***********************************************************************
!
      USE mod_param
      USE mod_grid
      USE mod_forces
      USE mod_ocean 
      USE mod_scalars
      USE bc_2d_mod
      USE mod_vegetation
      USE mod_vegarr
!#ifdef DISTRIBUTE
!      USE mp_exchange_mod, ONLY : mp_exchange2d
!#endif

!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
      integer, intent(in) :: nstp 
!
# ifdef ASSUMED_SHAPE
      real(r8), intent(in)  :: h(LBi:,LBj:)
      real(r8), intent(in)  :: angler(LBi:,LBj:)
#  ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:,LBj:)
      real(r8), intent(in) :: umask(LBi:,LBj:)
      real(r8), intent(in) :: vmask(LBi:,LBj:)
#  endif
#  ifdef WET_DRY
      real(r8), intent(in) :: rmask_wet(LBi:,LBj:)
#  endif
#  ifdef WAVES_HEIGHT  
      real(r8), intent(in)  :: Hwave(LBi:,LBj:)
#  endif 
#  ifdef WAVES_LENGTH
      real(r8), intent(in)  :: Lwave(LBi:,LBj:)
#  endif 
#  ifdef WAVES_DIR
      real(r8), intent(in)  :: Dwave(LBi:,LBj:)
#  endif 
      real(r8), intent(in)    :: marsh_mask(LBi:,LBj:)
      real(r8), intent(inout) :: mask_thrust(LBi:,LBj:)
      real(r8), intent(inout) :: Thrust_x(LBi:,LBj:)
      real(r8), intent(inout) :: Thrust_y(LBi:,LBj:) 
      real(r8), intent(in)    :: zeta(LBi:,LBj:,:)
# else
      real(r8), intent(in)  :: h(LBi:UBi,LBj:UBj)
      real(r8), intent(in)  :: angler(LBi:UBi,LBj:UBj)
#  ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: umask(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: vmask(LBi:UBi,LBj:UBj)
#  endif
#  ifdef WET_DRY
      real(r8), intent(in) :: rmask_wet(LBi:UBi,LBj:UBj)
#  endif 
#  ifdef WAVES_HEIGHT  
      real(r8), intent(in)  :: Hwave(LBi:UBi,LBj:UBj)
#  endif 
#  ifdef WAVES_HEIGHT  
      real(r8), intent(in)  :: Lwave(LBi:UBi,LBj:UBj)
#  endif 
#  ifdef WAVES_DIR
      real(r8), intent(in)  :: Dwave(LBi:UBi,LBj:UBj)
#  endif 
      real(r8), intent(in)    :: marsh_mask(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: mask_thrust(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: Thrust_x(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: Thrust_y(LBi:UBi,LBj:UBj) 
      real(r8), intent(in)    :: zeta(LBi:UBi,LBj:UBj,3)
# endif

!  Local variable declarations.
!
      integer :: i,j

      real(r8) :: Kw, Integral_Kp 
      real(r8) :: depth_all, mask_local_tonelli

      real(r8) :: cff, cff1, F_asl, F_bsl, F_msl

      real(r8) :: angler_deg, eft_angle
      real(r8) :: thrust_n, thrust_s, thrust_e, thrust_w

      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: umask_marsh
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: vmask_marsh
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: FX
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: FY

# include "set_bounds.h"
!
!----------------------------------------------------------------------
!  Executing the code
!----------------------------------------------------------------------
!
!----------------------------------------------------------------------
!  Marsh mask at U-points.
!----------------------------------------------------------------------
!
!

      DO j=Jstr,Jend+1
        DO i=Istr,Iend+1
          umask_marsh(i,j)=marsh_mask(i-1,j)+marsh_mask(i,j)
          IF (umask_marsh(i,j).eq.1.0_r8) THEN
            umask_marsh(i,j)=marsh_mask(i-1,j)-marsh_mask(i,j)
          ELSE
            umask_marsh(i,j)=0.0_r8 
          END IF
          write(59,*) i,j,umask_marsh(i,j)
        END DO
      END DO
!
!
!----------------------------------------------------------------------
!  Compute tonelli thrust mask and net thrust on all cell centers
!  where wave data is available at RHO points.
!----------------------------------------------------------------------
!
      DO j=Jstr-1,Jend+1
        DO i=Istr-1,Iend+1
          cff=1.5_r8*pi-Dwave(i,j)-angler(i,j)
!
!----------------------------------------------------------------------
!  Calculate Thrust below and above sea level.
!----------------------------------------------------------------------
!
          kw=2.0_r8*pi/Lwave(i,j)
          Integral_kp=sinh(kw*h(i,j))/(kw*cosh(h(i,j)*kw))

          cff1=rho0*g*Hwave(i,j)
          F_bsl=cff1*Integral_kp*0.001_r8
          F_asl=cff1*Hwave(i,j)*0.5_r8*0.001_r8
!
!---------------------------------------------------------------------
!  Total wave thrust at mean sea level.
!---------------------------------------------------------------------
!
          F_msl=F_asl+F_bsl
          FX(i,j)=F_msl*cos(cff)
          FY(i,j)=F_msl*sin(cff)
!
!---------------------------------------------------------------------
!  Compute total water column depth.
!---------------------------------------------------------------------
!
          depth_all=h(i,j)+zeta(i,j,1)
          IF (depth_all.lt.0.2_r8) THEN
            cff=1.0_r8-0.45_r8*depth_all*5.0_r8
          ELSEIF (0.2_r8.lt.depth_all.and.depth_all.lt.0.4_r8) THEN
            cff=0.55_r8*(1.0_r8-2.5_r8*(depth_all-0.2_r8))
          ELSE
            cff=0.275_r8
          ENDIF
          mask_thrust(i,j)=cff*marsh_mask(i,j)
!
! The total thrust is at cell centers if they are all wet
!
#  ifdef MASKING
#   ifdef WET_DRY
          FX(i,j)=FX(i,j)*rmask_wet(i,j)
          FY(i,j)=FY(i,j)*rmask_wet(i,j)
#   else
          FX(i,j)=FX(i,j)*rmask(i,j)
          FY(i,j)=FY(i,j)*rmask(i,j)
#   endif
#  endif
        END DO
      END DO

 
      DO j=Jstr,Jend
        DO i=Istr,Iend

          cff=SIGN(1.0_r8,FX(i-1,j))
!  Use only positive values
          cff1=MAX(cff,0.0_r8) 
          Thrust_w=umask_marsh(i,j)*FX(i-1,j)*cff1*mask_thrust(i,j)
          write(60,*)i,j,cff1,umask_marsh(i,j)
!
          cff=SIGN(1.0_r8,FX(i+1,j))
          cff1=MIN(cff,0.0_r8)  ! Use only negative values
          Thrust_e=umask_marsh(i+1,j)*FX(i+1,j)*cff1*mask_thrust(i,j)
          write(62,*)i,j,cff1,umask_marsh(i+1,j)
!
          Thrust_x(i,j)=ABS(Thrust_w)+ABS(Thrust_e)
!
!          IF(umask_marsh(i,j).eq.1.0_r8) THEN
!            Thrust_w=FX(i-1,j)*mask_thrust(i,j)
!          ELSE IF(umask_marsh(i+1,j).eq.-1.0_r8)THEN
!            Thrust_e=FX(i+1,j)*mask_thrust(i,j)
!          ELSE
!!            Thrust_w=0.0_r8
!            Thrust_e=0.0_r8
!          END IF
!
!---------------------------------------------------------------------
! This can be negative or positive (Thrust), Thrust_w,Thrust_e is at
! cell faces, so we transfer it to cell centers at end
!---------------------------------------------------------------------
!
        END DO
      END DO

   


!---------------------------------------------------------------------
!  Apply periodic or gradient boundary conditions for output
!  purposes only.
!---------------------------------------------------------------------
      CALL bc_r2d_tile (ng, tile,                                       &
     &                  LBi, UBi, LBj, UBj,                             &
     &                  mask_thrust)
      CALL bc_r2d_tile (ng, tile,                                       &
     &                  LBi, UBi, LBj, UBj,                             &
     &                  Thrust_x)
      CALL bc_r2d_tile (ng, tile,                                       &
     &                  LBi, UBi, LBj, UBj,                             &
     &                  Thrust_y)
!# ifdef DISTRIBUTE
!      CALL mp_exchange2d (ng, tile, iNLM, 4,                           &
!     &                    LBi, UBi, LBj, UBj,                          &
!     &                    NghostPoints,                                &
!     &                    EWperiodic(ng), NSperiodic(ng),              &
!     &                    mask_thrust, Thrust_x,                       & 
!     &                    Thrust_y)
!# endif
      END SUBROUTINE marsh_wave_thrust_tile
#endif
      END MODULE marsh_wave_thrust_mod
