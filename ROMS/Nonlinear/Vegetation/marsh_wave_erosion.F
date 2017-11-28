#include "cppdefs.h"
 
       MODULE marsh_wave_erosion_mod
#if defined VEGETATION && defined MARSH_WAVE_EROSION
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
!  This routine is the main driver for marsh sediment transport and    !
!  calls three sub-routines.                                           !
!                                                                      !
!  1. Compute wave thrust on marshes.                                  !  
!     --> marsh_wave_thrust.F                                          !
!  2. Obtain sediment erosion and net sed flux out of marsh cells      !
!     --> marsh_sed_erosion.F                                          !
!  3. Get the amount of lateral retreat based on mass flux.            !
!     --> marsh_retreat.F                                              !
!                                                                      !
!  References:                                                         !   
!                                                                      !
!======================================================================! 
!                                                                      !
      implicit none

      PRIVATE
      PUBLIC  :: marsh_wave_erosion

      CONTAINS
!
!***********************************************************************
!
      SUBROUTINE marsh_wave_erosion (ng, tile)
!
!***********************************************************************
!
      USE mod_stepping 
      USE marsh_wave_thrust_mod, ONLY : marsh_wave_thrust
#  if defined MARSH_SED_EROSION
      USE marsh_sed_erosion, ONLY : marsh_sed_erosion 
#  endif 
#  if defined MARSH_RETREAT
      USE marsh_retreat_mod, ONLY : marsh_retreat
#  endif 
# endif 
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
!

# ifdef PROFILE
      CALL wclock_on (ng, iNLM, 16)
# endif
!
!***********************************************************************
! Wave induced thrust calculation on marsh boundary.
!***********************************************************************
!
      CALL marsh_wave_thrust(ng, tile)
!
#  if defined MARSH_SED_EROSION
!
!***********************************************************************
! Marsh sediment erosion based on bathy change in marsh cells.
!***********************************************************************
! 
      CALL marsh_sed_erosion(ng, tile) 
!
#  endif 
!
#  if defined MARSH_RETREAT
!
!***********************************************************************
! Compute effective lateral/horizontal retreat. 
!***********************************************************************
!
      CALL marsh_retreat(ng, tile) 
!
#  endif 
# endif 
              
# ifdef PROFILE
      CALL wclock_off (ng, iNLM, 16)
# endif
      RETURN
      END SUBROUTINE marsh_wave_erosion
#endif
      END MODULE marsh_wave_erosion_mod
