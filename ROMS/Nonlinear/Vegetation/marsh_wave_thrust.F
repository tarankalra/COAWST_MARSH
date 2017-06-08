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
     &                        VEG(ng) % Thrust_tonelli,                 &
# ifdef MARSH_SED_BEDLOAD 
     &                        VEG(ng) % Thrust_w,                       &
     &                        VEG(ng) % Thrust_e,                       &
     &                        VEG(ng) % Thrust_n,                       &
     &                        VEG(ng) % Thrust_s,                       &
# endif 
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
     &                              Thrust_tonelli,                     & 
# ifdef MARSH_SED_BEDLOAD 
     &                              Thrust_w,                           &
     &                              Thrust_e,                           &
     &                              Thrust_n,                           &
     &                              Thrust_s,                           &
# endif 
     &                              zeta)           
!
!***********************************************************************
!
      USE mod_param
      USE mod_grid
      USE mod_forces
      USE mod_ocean 
      USE mod_scalars
      USE mod_vegetation
      USE mod_vegarr
      USE bc_2d_mod
#ifdef DISTRIBUTE
      USE mp_exchange_mod, ONLY : mp_exchange2d
#endif

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
      real(r8), intent(inout) :: Thrust_tonelli(LBi:,LBj:)
#  if defined MARSH_SED_BEDLOAD 
      real(r8), intent(inout) :: Thrust_w(LBi:,LBj:)
      real(r8), intent(inout) :: Thrust_e(LBi:,LBj:)
      real(r8), intent(inout) :: Thrust_n(LBi:,LBj:)
      real(r8), intent(inout) :: Thrust_s(LBi:,LBj:)
#  endif 
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
      real(r8), intent(inout) :: Thrust_tonelli(LBi:UBi,LBj:UBj)
#  ifdef MARSH_SED_BEDLOAD 
      real(r8), intent(inout) :: Thrust_w(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: Thrust_e(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: Thrust_n(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: Thrust_s(LBi:UBi,LBj:UBj)
#  endif 
      real(r8), intent(in)    :: zeta(LBi:UBi,LBj:UBj,3)
# endif
!
!  Local variable declarations.
!
      integer :: i,j

      real(r8), parameter :: Inival=0.0_r8
      real(r8), parameter :: half=0.5_r8
      real(r8), parameter :: eps=1.0e-12_r8

      real(r8) :: Kw, Integral_Kp 
      real(r8) :: depth_all, mask_local_tonelli

      real(r8) :: cff, cff1, F_asl, F_bsl, F_msl

      real(r8) :: mask_thrust_n, mask_thrust_s
      real(r8) :: mask_thrust_e, mask_thrust_w
      real(r8) :: thrust_loc_n, thrust_loc_s
      real(r8) :: thrust_loc_e, thrust_loc_w

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
!  Marsh mask at U-points and V-points.
!----------------------------------------------------------------------
!
      umask_marsh=Inival
      vmask_marsh=Inival
      FX=Inival
      FY=Inival
      mask_thrust_w=Inival
      mask_thrust_e=Inival
      mask_thrust_n=Inival
      mask_thrust_s=Inival
!
      DO j=JstrR,JendR
        DO i=Istr,IendR
          umask_marsh(i,j)=marsh_mask(i-1,j)+marsh_mask(i,j)
          IF (umask_marsh(i,j).eq.1.0_r8) THEN
            umask_marsh(i,j)=marsh_mask(i-1,j)-marsh_mask(i,j)
          ELSE
            umask_marsh(i,j)=0.0_r8 
          END IF
        END DO
      END DO
!
      DO j=Jstr,JendR
        DO i=IstrR,IendR
          vmask_marsh(i,j)=marsh_mask(i,j-1)+marsh_mask(i,j)
          IF (vmask_marsh(i,j).eq.1.0_r8) THEN
            vmask_marsh(i,j)=marsh_mask(i,j-1)-marsh_mask(i,j)
          ELSE
            vmask_marsh(i,j)=0.0_r8
          END IF
        END DO
      END DO
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
!
          cff1=rho0*g*Hwave(i,j)
          F_bsl=cff1*Integral_kp*0.001_r8
          F_asl=cff1*Hwave(i,j)*0.5_r8*0.001_r8
!
!---------------------------------------------------------------------
!  Total wave thrust at mean sea level.
!---------------------------------------------------------------------
!
          F_msl=F_asl+F_bsl
          FX(i,j)=F_msl*MIN(cos(cff),eps)
          FY(i,j)=F_msl*MIN(sin(cff),eps)
! 
! Find a way of making 10-16 cos,sin to zero
!
        END DO
      END DO
!
!  |           |Thrust North |           |
!--|---------- |-------------|---------  |-------------
!  |Thrust West|   Marsh     |Thrust East| 
!  |-----------|-------------|-----------|
!  |           |Thrust South |           |    
!
      DO j=Jstr,Jend
        DO i=Istr,Iend
!
!---------------------------------------------------------------------
! Use sign of thrust and use only positive values for West 
! and use only negative values for East. 
!---------------------------------------------------------------------
!
          mask_thrust_w=IniVal
          depth_all=ABS(h(i,j)+zeta(i-1,j,1))
          CALL depth_mask(depth_all,mask_thrust_w)
!
          cff=half*(1.0_r8+SIGN(1.0_r8,FX(i-1,j)))
          Thrust_loc_w=umask_marsh(i,j)*FX(i-1,j)*cff*                  &
                       mask_thrust_w
#  ifdef MARSH_SED_BEDLOAD 
          Thrust_w(i,j)=Thrust_loc_w
#  endif
!
          mask_thrust_e=IniVal
          depth_all=ABS(h(i,j)+zeta(i+1,j,1))
          CALL depth_mask(depth_all,mask_thrust_e)
!
          cff=half*(1.0_r8-SIGN(1.0_r8,FX(i+1,j)))
          Thrust_loc_e=umask_marsh(i+1,j)*FX(i+1,j)*cff*                &
                       mask_thrust_e
#  ifdef MARSH_SED_BEDLOAD 
          Thrust_e(i+1,j)=Thrust_loc_e
#  endif
!
!---------------------------------------------------------------------
! Use sign of thrust and use only positive values for South
! and use only negative values for North.
!---------------------------------------------------------------------
! 
          mask_thrust_n=IniVal
          depth_all=(h(i,j)+zeta(i,j+1,1))
          CALL depth_mask(depth_all,mask_thrust_n)
!
          cff=half*(1.0_r8-SIGN(1.0_r8,FY(i,j+1)))
          Thrust_loc_n=vmask_marsh(i,j+1)*FY(i,j+1)*cff*                &
                       mask_thrust_n
#  ifdef MARSH_SED_BEDLOAD 
          Thrust_n(i,j+1)=Thrust_loc_n
#  endif
!
          mask_thrust_s=IniVal
          depth_all=ABS(h(i,j)+zeta(i,j-1,1))
          CALL depth_mask(depth_all,mask_thrust_s)
!
          cff=half*(1.0_r8+SIGN(1.0_r8,FY(i,j-1)))
          Thrust_loc_s=vmask_marsh(i,j)*FY(i,j-1)*cff*                  &
                       mask_thrust_s
!
#  ifdef MARSH_SED_BEDLOAD 
          Thrust_s(i,j)=Thrust_loc_s
#  endif
   
!
          mask_thrust(i,j)=mask_thrust_w+mask_thrust_e+                 &
     &                     mask_thrust_n+mask_thrust_s
!  
          Thrust_tonelli(i,j)=ABS(Thrust_loc_w)+ABS(Thrust_loc_e)+      &
     &                        ABS(Thrust_loc_n)+ABS(Thrust_loc_s)
         
        END DO
      END DO
!
!---------------------------------------------------------------------
!  Apply periodic or gradient boundary conditions for output
!  purposes only.
!---------------------------------------------------------------------
      CALL bc_r2d_tile (ng, tile,                                       &
     &                  LBi, UBi, LBj, UBj,                             &
     &                  mask_thrust)
     CALL bc_r2d_tile (ng, tile,                                        &
     &                  LBi, UBi, LBj, UBj,                             &
     &                  Thrust_tonelli)
# ifdef DISTRIBUTE
! 
! Exchange boundary data 
!
      CALL mp_exchange2d (ng, tile, iNLM, 2,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    mask_thrust,                                  &
     &                    Thrust_tonelli)  
# endif
!
# ifdef MARSH_SED_BEDLOAD
     CALL bc_u2d_tile (ng, tile,                                        &
     &                  LBi, UBi, LBj, UBj,                             &
     &                  Thrust_w)
     CALL bc_u2d_tile (ng, tile,                                        &
     &                 LBi, UBi, LBj, UBj,                              &
     &                  Thrust_e)
     CALL bc_v2d_tile (ng, tile,                                        &
     &                 LBi, UBi, LBj, UBj,                              &
     &                  Thrust_n)
     CALL bc_v2d_tile (ng, tile,                                        &
     &                 LBi, UBi, LBj, UBj,                              &
     &                  Thrust_s)
#  ifdef DISTRIBUTE
! 
! Exchange boundary data 
!
      CALL mp_exchange2d (ng, tile, iNLM, 4,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    Thrust_w,                                     &
     &                    Thrust_e,                                     &
     &                    Thrust_n,                                     &
     &                    Thrust_s)  
#  endif
# endif
!
      END SUBROUTINE marsh_wave_thrust_tile

      SUBROUTINE depth_mask (depth_all, mask_thrust_eachdir)
!
!=======================================================================
! Compute marsh mask with respect to water column depth to reduce      ! 
! thrust based on depth.                                               ! 
!                                                                      !
!=======================================================================
!
      USE mod_scalars
!
!  Imported variable declarations.
!
      real(r8), intent(in) :: depth_all
      real(r8), intent(out) :: mask_thrust_eachdir
!
!  Local variable declarations.
!

      real(r8) :: cff, fac
      fac=0.60_r8
!
!---------------------------------------------------------------------
!  Constant value of masking below the mrsh
!  Exponential reduction above the marsh (depth=zeta+marsh elevation)
!---------------------------------------------------------------------
! 
      IF(depth_all.lt.0.0_r8) THEN 
        cff=1.0_r8
      ELSEIF (depth_all.ge.0.0_r8) THEN
        cff=exp(-depth_all*3.0_r8)
      ENDIF   
!
      mask_thrust_eachdir=cff*fac
!
      END SUBROUTINE depth_mask
#endif
      END MODULE marsh_wave_thrust_mod
