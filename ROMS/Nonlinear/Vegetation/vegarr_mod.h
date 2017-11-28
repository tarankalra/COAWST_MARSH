!                                                                      !
!svn $Id: vegarr_mod.h 429 2009-12-20 17:30:26Z arango $               !
!================================================== Hernan G. Arango ==!
!  Copyright (c) 2002-2016 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!================================================== John C. Warner ====!
!==================================================== Neil K. Ganju  ==! 
!==================================================== Alexis Beudin  ==! 
!==================================================Tarandeep S. Kalra==!
!                                                                      !
!  Vegetation Model Kernel Variables:                                  !
# if defined VEG_DRAG || defined VEG_BIOMASS 
!  plant         Vegetation variable properties:                       !
!                   plant(:,:,:,phght) => height                       !
!                   plant(:,:,:,pdens) => density                      !
!                   plant(:,:,:,pthck) => thickness                    !
!                   plant(:,:,:,pdiam) => diameter                     !
!                   plant(:,:,:,pabbm) => above ground biomass         !
!                   plant(:,:,:,pbgbm) => below ground biomass         !
# endif
# ifdef VEG_DRAG
!  ru_veg         Momentum term for x direction(takes account for all  !
!                 vegetation types)                                    !
!  rv_veg         Momentum term for x direction(takes account for all  !
!                 vegetation types)                                    !
!  ru_veg_loc     Momentum term for x direction(takes account for only !
!                 local vegetation type)                               !
!  rv_veg_loc     Momentum term for x direction(takes account for all  !
!                 local vegetation types)                              !
!  step2d_uveg    Momentum term for 2d x direction                     !
!  step2d_vveg    Momentum term for 2d y direction                     !
!  Lveg           Effective blade length                               ! 
# endif
# ifdef VEG_FLEX
!  bend           Bending for each vegetation                          !
# endif
# ifdef VEG_TURB
!  tke_veg        Turbulent kinetic energy from vegetation             !
!  gls_veg        Length scale change from vegetation                  !
# endif
# if defined VEG_SWAN_COUPLING && defined VEG_STREAMING
!  dissip_veg     Dissipation from the SWAN model due to vegetation    !
!  BWDXL_veg      Wave streaming effect due to vegetation              !
!  BWDYL_veg      Wave streaming effect due to vegetation              !
# endif
# ifdef MARSH_WAVE_EROSION 
!  marsh_mask     User input of marsh masking at MSL                   ! 
!  Thrust_xi      Wave thrust on xi marsh faces                        ! 
!  Thrust_eta     Wave thrust on eta marsh faces                       ! 
!  Thrust_total   Total magnitude of thrust on marsh edge              !
!  marsh_flux_out Total marsh flux out from a cell                     !
#  endif                                                               !  
#  if defined MARSH_RETREAT                                            !
!   marsh_flux_xi   Marsh flux from xi marsh face                      ! 
!   marsh_flux_eta  Marsh flux from North marsh face                   ! 
!   marsh_retreat   Amount of marsh retreat                            !
#  endif                                                               ! 
# endif                                                                ! 
!                                                                      !
!======================================================================!
!
      USE mod_kinds
!
      implicit none
       
      TYPE T_VEG
!
!  Nonlinear model state.
!
# if defined VEG_DRAG || defined VEG_BIOMASS 
        real(r8), pointer :: plant(:,:,:,:)
# endif 
# ifdef VEG_DRAG
!  Momentum terms go back to act as sink in rhs
        real(r8), pointer :: ru_veg(:,:,:)
        real(r8), pointer :: rv_veg(:,:,:)

!  Momentum terms feed to the turbulence model 
        real(r8), pointer :: ru_loc_veg(:,:,:,:)
        real(r8), pointer :: rv_loc_veg(:,:,:,:)
        real(r8), pointer :: step2d_uveg(:,:)
        real(r8), pointer :: step2d_vveg(:,:)
        real(r8), pointer :: Lveg(:,:,:)
# endif 
# ifdef VEG_FLEX 
        real(r8), pointer :: bend(:,:,:)
# endif         
# ifdef VEG_TURB
        real(r8), pointer :: tke_veg(:,:,:)
        real(r8), pointer :: gls_veg(:,:,:)
# endif 
# ifdef VEG_HMIXING
        real(r8), pointer :: visc2d_r_veg(:,:)
        real(r8), pointer :: visc3d_r_veg(:,:,:)
# endif 
# if defined VEG_SWAN_COUPLING && defined VEG_STREAMING
        real(r8), pointer :: dissip_veg(:,:)
        real(r8), pointer :: BWDXL_veg(:,:,:)
        real(r8), pointer :: BWDYL_veg(:,:,:)
# endif 
# ifdef MARSH_WAVE_EROSION 
        real(r8), pointer :: marsh_mask(:,:)
        real(r8), pointer :: Thrust_xi(:,:)
        real(r8), pointer :: Thrust_eta(:,:)
        real(r8), pointer :: Thrust_total(:,:)
        real(r8), pointer :: marsh_flux_out(:,:,:)
#  if defined MARSH_RETREAT 
        real(r8), pointer :: marsh_flux_xi(:,:)
        real(r8), pointer :: marsh_flux_eta(:,:)
        real(r8), pointer :: marsh_retreat(:,:)
#  endif 
# endif
!
      END TYPE T_VEG
!
      TYPE (T_VEG), allocatable :: VEG(:)
!
      CONTAINS
!
      SUBROUTINE allocate_vegarr (ng, LBi, UBi, LBj, UBj)
!
!=======================================================================
!                                                                      !
!  This routine allocates all variables in the module for all nested   !
!  grids.                                                              !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_ncparam
      USE mod_vegetation 
#   if defined MARSH_LAT_RETREAT
      USE mod_sediment 
#   endif 

      implicit none 
!                       
!  Imported variable declarations.
!
      integer, intent(in) :: ng, LBi, UBi, LBj, UBj
!
!-----------------------------------------------------------------------
!  Allocate structure variables.
!-----------------------------------------------------------------------
!
      IF (ng.eq.1) allocate ( VEG(Ngrids) )
!
!  Nonlinear model state.
!
# if defined VEG_DRAG || defined VEG_BIOMASS 
      allocate ( VEG(ng) % plant(LBi:UBi,LBj:UBj,NVEG,NVEGP) )
# endif
# ifdef VEG_DRAG
      allocate ( VEG(ng) % ru_veg(LBi:UBi,LBj:UBj,N(ng)) )
      allocate ( VEG(ng) % rv_veg(LBi:UBi,LBj:UBj,N(ng)) )
      allocate ( VEG(ng) % ru_loc_veg(LBi:UBi,LBj:UBj,N(ng),NVEG) )
      allocate ( VEG(ng) % rv_loc_veg(LBi:UBi,LBj:UBj,N(ng),NVEG) )
      allocate ( VEG(ng) % step2d_uveg(LBi:UBi,LBj:UBj) )
      allocate ( VEG(ng) % step2d_vveg(LBi:UBi,LBj:UBj) ) 
      allocate ( VEG(ng) % Lveg(LBi:UBi,LBj:UBj,N(ng)) )
#  ifdef VEG_FLEX
      allocate ( VEG(ng) % bend(LBi:UBi,LBj:UBj,NVEG) )
#  endif
#  ifdef VEG_HMIXING
      allocate ( VEG(ng) % visc2d_r_veg(LBi:UBi,LBj:UBj) )
      allocate ( VEG(ng) % visc3d_r_veg(LBi:UBi,LBj:UBj,N(ng)) )
#  endif 
#  ifdef VEG_TURB
      allocate ( VEG(ng) % tke_veg(LBi:UBi,LBj:UBj,N(ng)) )
      allocate ( VEG(ng) % gls_veg(LBi:UBi,LBj:UBj,N(ng)) )
#  endif
# endif
#  if defined VEG_SWAN_COUPLING && defined VEG_STREAMING
      allocate ( VEG(ng) % dissip_veg(LBi:UBi,LBj:UBj) )
      allocate ( VEG(ng) % BWDXL_veg(LBi:UBi,LBj:UBj,N(ng)) )
      allocate ( VEG(ng) % BWDYL_veg(LBi:UBi,LBj:UBj,N(ng)) )
#  endif
# ifdef MARSH_WAVE_EROSION
      allocate ( VEG(ng) % marsh_mask(LBi:UBi,LBj:UBj        ) )
      allocate ( VEG(ng) % Thrust_xi(LBi:UBi,LBj:UBj         ) )
      allocate ( VEG(ng) % Thrust_eta(LBi:UBi,LBj:UBj        ) )
      allocate ( VEG(ng) % Thrust_total(LBi:UBi,LBj:UBj      ) )
      allocate ( VEG(ng) % marsh_flux_out(LBi:UBi,LBj:UBj,NST) )
#  if defined MARSH_RETREAT
      allocate ( VEG(ng) % marsh_flux_xi(LBi:UBi,LBj:UBj ) )
      allocate ( VEG(ng) % marsh_flux_eta(LBi:UBi,LBj:UBj) )
      allocate ( VEG(ng) % marsh_retreat(LBi:UBi,LBj:UBj ) )
#  endif
# endif
!
!-----------------------------------------------------------------------
!  Allocate various input variables for vegetation module.
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE allocate_vegarr
!
      SUBROUTINE initialize_vegarr (ng, tile, model)
!
!=======================================================================
!                                                                      !
!  This routine initialize structure variables in the module using     !
!  first touch distribution policy. In shared-memory configuration,    !
!  this operation actually performs the propagation of the "shared     !
!  arrays" across the cluster,  unless another policy is specified     !
!  to  override the default.                                           !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_ncparam
      USE mod_vegetation 
#   if defined MARSH_LAT_RETREAT
      USE mod_sediment
#   endif 
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile, model
!
!  Local variable declarations.
!
      integer :: Imin, Imax, Jmin, Jmax
      integer :: i, j, k, iveg, ivpr
!
      real(r8), parameter :: IniVal = 0.0_r8
!
#include "set_bounds.h"
!
!  Set array initialization range.
!
#ifdef _OPENMP
      IF (DOMAIN(ng)%Western_Edge(tile)) THEN
        Imin=BOUNDS(ng)%LBi(tile)
      ELSE
        Imin=Istr
      END IF
      IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
       Imax=BOUNDS(ng)%UBi(tile)
      ELSE
        Imax=Iend
      END IF
      IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
        Jmin=BOUNDS(ng)%LBj(tile)
      ELSE
        Jmin=Jstr
      END IF
      IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
        Jmax=BOUNDS(ng)%UBj(tile)
      ELSE
        Jmax=Jend
      END IF
#else
      Imin=BOUNDS(ng)%LBi(tile)
      Imax=BOUNDS(ng)%UBi(tile)
      Jmin=BOUNDS(ng)%LBj(tile)
      Jmax=BOUNDS(ng)%UBj(tile)
#endif
!
!-----------------------------------------------------------------------
!  Initialize vegetation structure variables.
!-----------------------------------------------------------------------
!
!
      IF ((model.eq.0).or.(model.eq.iNLM)) THEN
# if defined VEG_DRAG || defined VEG_BIOMASS 
        DO ivpr=1,NVEGP
          DO iveg=1,NVEG
            DO j=Jmin,Jmax
              DO i=Imin,Imax
                VEG(ng) % plant(i,j,iveg,ivpr) = IniVal
              END DO
            END DO
          END DO 
        END DO
# endif 
!
# ifdef VEG_DRAG
        DO k=1,N(ng)
          DO j=Jmin,Jmax
            DO i=Imin,Imax
              VEG(ng) % ru_veg(i,j,k) = IniVal
              VEG(ng) % rv_veg(i,j,k) = IniVal
            END DO 
          END DO 
        END DO 
        DO k=1,N(ng)
          DO j=Jmin,Jmax
            DO i=Imin,Imax
              VEG(ng) % Lveg(i,j,k) = IniVal
            END DO 
          END DO 
        END DO 
!
        DO iveg=1,NVEG
          DO k=1,N(ng)
            DO j=Jmin,Jmax
              DO i=Imin,Imax
                VEG(ng) % ru_loc_veg(i,j,k,iveg) = IniVal
                VEG(ng) % rv_loc_veg(i,j,k,iveg) = IniVal
              END DO 
            END DO 
          END DO
        END DO
!
        DO j=Jmin,Jmax
          DO i=Imin,Imax
            VEG(ng) % step2d_uveg(i,j) = IniVal
            VEG(ng) % step2d_vveg(i,j) = IniVal
          END DO
        END DO 
!
#  ifdef VEG_FLEX 
        DO iveg=1,NVEG
          DO j=Jmin,Jmax
            DO i=Imin,Imax
              VEG(ng) % bend(i,j,iveg) = IniVal
            END DO 
          END DO 
        END DO 
#  endif 
!  
#  ifdef VEG_TURB 
        DO k=1,N(ng)
          DO j=Jmin,Jmax
            DO i=Imin,Imax
              VEG(ng) % tke_veg(i,j,k) = IniVal
              VEG(ng) % gls_veg(i,j,k) = IniVal
            END DO 
          END DO
        END DO 
#  endif
!  
#  if defined VEG_SWAN_COUPLING && defined VEG_STREAMING 
        DO j=Jmin,Jmax
          DO i=Imin,Imax
            VEG(ng) % dissip_veg(i,j) = IniVal
          END DO 
        END DO 
        DO k=1,N(ng)
          DO j=Jmin,Jmax
            DO i=Imin,Imax
              VEG(ng) % BWDXL_veg(i,j,k) = IniVal
              VEG(ng) % BWDYL_veg(i,j,k) = IniVal
            END DO 
          END DO
        END DO 
#  endif
# endif
!
# ifdef MARSH_WAVE_EROSION
   DO j=Jmin,Jmax
     DO i=Imin,Imax
       VEG(ng) % marsh_mask(i,j)   = IniVal
       VEG(ng) % Thrust_xi(i,j)    = IniVal
       VEG(ng) % Thrust_eta(i,j)   = IniVal  
       VEG(ng) % Thrust_total(i,j) = IniVal
       DO k=1,NST
         VEG(ng) % marsh_flux_out(i,j,k) = IniVal
       END DO 
     END DO 
   END DO
!
#  if defined MARSH_RETREAT
    DO j=Jmin,Jmax
      DO i=Imin,Imax
        VEG(ng) % marsh_flux_xi(i,j) = IniVal
        VEG(ng) % marsh_flux_eta(i,j) = IniVal
        VEG(ng) % marsh_retreat(i,j) = IniVal
      END DO 
    END DO
#  endif
# endif
!
      END IF
!
      RETURN   
      END SUBROUTINE initialize_vegarr
