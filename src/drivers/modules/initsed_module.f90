module initsed_module
  use precision
  implicit none

save

    real(fp)                                      :: alf1          ! calibration coefficient van Rijn (1984) [-]
    real(fp)                                      :: betam         ! power factor for adaptation of critical bottom shear stress [-]
    real(fp)                                      :: rksc          ! reference level van Rijn (1984) [m]
    real(fp),    allocatable, dimension(:)        :: pmcrit        ! critical mud fraction [-]
!    real(fp),    allocatable, dimension(:,:)      :: bfluff0       ! burial coefficient 1 [kg/m2/s]
 !   real(fp),    allocatable, dimension(:,:)      :: bfluff1       ! burial coefficient 2 [1/s]
    real(fp),     dimension(:,:), pointer       :: bfluff0       ! burial coefficient 1 [kg/m2/s]
    real(fp),     dimension(:,:), pointer       :: bfluff1       ! burial coefficient 2 [1/s]
    real(fp),    allocatable, dimension(:,:)      :: depeff        ! deposition efficiency [-]
    real(fp),    allocatable, dimension(:,:)      :: depfac        ! deposition factor (flufflayer=2) [-]
    real(fp),    allocatable, dimension(:,:)      :: eropar        ! erosion parameter for mud [kg/m2/s]
    real(fp),    allocatable, dimension(:,:)      :: parfluff0     ! erosion parameter 1 [s/m]
    real(fp),    allocatable, dimension(:,:)      :: parfluff1     ! erosion parameter 2 [ms/kg]
    real(fp),    allocatable, dimension(:,:)      :: tcrdep        ! critical bed shear stress for mud sedimentation [N/m2]
    real(fp),    allocatable, dimension(:,:)      :: tcrero        ! critical bed shear stress for mud erosion [N/m2]
    real(fp),   allocatable, dimension(:,:)      :: tcrfluff      ! critical bed shear stress for fluff layer erosion [N/m2]

contains

subroutine initsed( nmlb,   nmub,   nfrac, flufflyr) !, &
!                 & alf1,    betam,  rksc,   pmcrit , bfluff0, bfluff1,&
!                 & depeff,  depfac, eropar, parfluff0,  parfluff1, &
!                 & tcrdep,  tcrero, tcrfluff)
!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2012.
!
!  This program is free software: you can redistribute it and/or modify
!  it under the terms of the GNU General Public License as published by
!  the Free Software Foundation version 3.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D" and "Deltares"
!  are registered trademarks of Stichting Deltares, and remain the property of
!  Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------
!  $Id: initsed.f90 7697 2012-11-16 14:10:17Z boer_aj $
!  $HeadURL: https://svn.oss.deltares.nl/repos/openearthtools/trunk/programs/SandMudBedModule/03_Fortran/example/example/source/initsed.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Settings of morphological parameters
!
!!--declarations----------------------------------------------------------------

  use precision
    !
    implicit none
    !
!
! Global variables
!
    integer                                     , intent(in)   :: flufflyr      ! switch for fluff layer concept
    integer                                     , intent(in)   :: nfrac         ! number of sediment fractions
    integer                                     , intent(in)   :: nmlb          ! first cell number
    integer                                     , intent(in)   :: nmub          ! first cell number


!

    allocate (   pmcrit (nmlb:nmub)             )
    allocate (   bfluff0  (nfrac,nmlb:nmub)     )        ! burial coefficient 1 [kg/m2/s]
    allocate (   bfluff1  (nfrac,nmlb:nmub)     )        ! burial coefficient 2 [1/s]
    allocate (   depeff  (nfrac,nmlb:nmub)      )        ! deposition efficiency [-]
    allocate (   depfac  (nfrac,nmlb:nmub)      )        ! deposition factor (flufflayer=2) [-]
    allocate (   eropar  (nfrac,nmlb:nmub)      )        ! erosion parameter for mud [kg/m2/s]
    allocate (   parfluff0  (nfrac,nmlb:nmub)   )        ! erosion parameter 1 [s/m]
    allocate (   parfluff1  (nfrac,nmlb:nmub)   )        ! erosion parameter 2 [ms/kg]
    allocate (   tcrdep  (nfrac,nmlb:nmub)      )        ! critical bed shear stress for mud sedimentation [N/m2]
    allocate (   tcrero  (nfrac,nmlb:nmub)      )        ! critical bed shear stress for mud erosion [N/m2]
    allocate (   tcrfluff  (nfrac,nmlb:nmub)    )        ! critical bed shear stress for fluff layer erosion [N/m2]

!! executable statements ------------------
!
    ! ================================================================================
    !   USER INPUT
    ! ================================================================================
    !
    !   Parameters sediment
    !
    eropar      = 1.0e-2_fp     ! erosion parameter for mud [kg/m2/s]
    tcrdep      = 1000.0_fp     ! critical bed shear stress for mud sedimentation [N/m2]
    tcrero      = 1.0_fp        ! critical bed shear stress for mud erosion [N/m2]
    !
    !   Parameters fluff layer
    !
    depeff      = 0.95_fp       ! deposition efficiency [-]
    depfac      = 0.2_fp        ! deposition factor (flufflayer=2) [-]
    parfluff0   = 2.0e-1_fp     ! erosion parameter 1 [s/m]
    parfluff1   = 1.0_fp        ! erosion parameter 2 [ms/kg]
    tcrfluff    = 0.05_fp       ! critical bed shear stress for fluff layer erosion [N/m2]
    if (flufflyr==1) then
        bfluff0     = 0.0_fp    ! burial coefficient 1 [kg/m2/s]
        bfluff1     = 0.0_fp    ! burial coefficient 2 [1/s]
    endif
    !
    !   Parameters sand-mud interaction
    !
    betam       =  1.0_fp       ! power factor for adaptation of critical bottom shear stress [-]
    pmcrit      =  0.6_fp       ! critical mud fraction [-]
    !
    !   Parameters sediment transport formulation
    !
    alf1        = 2.0_fp        ! calibration coefficient [-]
    rksc        = 0.1_fp        ! reference level [m]
    !
    ! ================================================================================

end subroutine initsed

end module initsed_module
