!> @brief Implementation of a vertical macrobenthos position driver
!> @file vertical_macrobenthos_driver.F90
!!
!  self computer program is part of MOSSCO.
!> @copyright Copyright (C) 2018 Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen <carsten.lemmen@hzg.de>
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

#define RANGE1D lbnd(1):ubnd(1)
#define RANGE2D RANGE1D,lbnd(2):ubnd(2)

#ifndef KIND_R8
#define KIND_R8 selected_real_kind(13)
#endif

#ifndef KIND_I4
#define KIND_I4 selected_int_kind(5)
#endif

module vertical_macrobenthos_driver

  use solver_library!, only :: type_rhs_driver
  !> also use mossco_strings? only when no ESMF dependence!

  private
  public create_vertical_macrobenthos

  type, extends(type_rhs_driver), public :: type_vertical_macrobenthos

    real(KIND_R8), pointer :: biomass_epibenthic(:,:) => null()
    real(KIND_R8), pointer :: biomass_endobenthic(:,:) => null()
    real(KIND_R8), pointer :: biomass(:,:) => null()
    real(KIND_R8), pointer :: depth_epibenthic(:,:) => null()
    real(KIND_R8), pointer :: depth_endobenthic(:,:) => null()
    real(KIND_R8), pointer :: depth(:,:) => null()

    real(KIND_R8), pointer :: roughness_length(:,:) => null()
    real(KIND_R8), pointer :: bulk_speed(:,:) => null()
    real(KIND_R8), pointer :: poc(:,:) => null()

    real(KIND_R8) :: respiration_rate, predation_depth, bioturbation_depth
    real(KIND_R8) :: epibenthic_affinity, endobenthic_affinity, visibility
    real(KIND_R8) :: risk_amplification, drag

  contains
    procedure :: set
    procedure :: get
    procedure :: initialize
    procedure :: finalize
  end type


contains

function create_vertical_macrobenthos(inum, jnum, rc) result(self)

  implicit none

  integer(KIND_I4), intent(in)            :: inum, jnum
  integer(KIND_I4), optional, intent(out) :: rc
  type(type_vertical_macrobenthos), allocatable :: self

  if (present(rc)) rc = 0
  if (.not.allocated(self)) allocate(self)

  call self%initialize(inum, jnum, rc)

end function create_vertical_macrobenthos

subroutine initialize(self, inum, jnum, rc)

  class(type_vertical_macrobenthos) :: self
  integer(KIND_I4), intent(in)            :: inum, jnum
  integer(KIND_I4), optional, intent(out) :: rc

  if (present(rc)) rc = 0

  self%knum=1
  self%inum=inum
  self%jnum=jnum
  self%nvar=4

  allocate(self%mask(self%inum,self%jnum,self%knum))
  self%mask = .false.

  allocate(self%conc(self%inum,self%jnum,self%knum,self%nvar))
  self%conc = 0.0

  self%biomass_epibenthic  => self%conc(:,:,1,1)
  self%biomass_endobenthic => self%conc(:,:,1,2)

  allocate(self%biomass(inum,jnum))
  self%biomass = self%biomass_endobenthic + self%biomass_epibenthic

  self%depth_epibenthic  => self%conc(:,:,1,3)
  self%depth_endobenthic => self%conc(:,:,1,4)

  allocate(self%depth(inum,jnum))
  self%depth = self%depth_epibenthic + self%depth_endobenthic

  ! @todo allocate from outside?
  allocate(self%poc(inum,jnum))
  self%poc = 0.0

  allocate(self%roughness_length(inum,jnum))
  self%roughness_length = 0.01

  allocate(self%bulk_speed(inum,jnum))
  self%bulk_speed = 0.1

end subroutine initialize

subroutine finalize(self, rc)

  class(type_vertical_macrobenthos) :: self
  integer(KIND_I4), optional, intent(out) :: rc

  if (present(rc)) rc = 0

  if (associated(self%mask)) deallocate(self%mask)
  if (associated(self%conc)) deallocate(self%conc)

  nullify(self%biomass_epibenthic)
  nullify(self%biomass_endobenthic)

  if (associated(self%biomass)) deallocate(self%biomass)

  nullify(self%depth_epibenthic)
  nullify(self%depth_epibenthic)

  if (associated(self%depth)) deallocate(self%depth)
  if (associated(self%poc)) deallocate(self%poc)
  if (associated(self%roughness_length)) deallocate(self%roughness_length)
  if (associated(self%bulk_speed)) deallocate(self%bulk_speed)

end subroutine finalize

subroutine set(self)

  class(type_vertical_macrobenthos) :: self

  self%dt_min=1.d-9 ! minimum timestep
  self%relative_change_min=-0.9d0 ! minimum relative change
  self%last_min_dt=1.e20
  self%last_min_dt_grid_cell = (/-99,-99,-99,-99/)
  self%adaptive_solver_diagnostics=.false.

end subroutine set

subroutine get(self)

  class(type_vertical_macrobenthos) :: self
end subroutine get

subroutine get_rhs(self, rhs, rc)

  class(type_vertical_macrobenthos) :: self
  integer(KIND_I4), optional, intent(out) :: rc
  real(KIND_R8), intent(inout), dimension(:,:,:,:), pointer :: rhs

  real(KIND_R8)    :: dmpdz, dmbdz, dppdz, dpbdz, drpdz, drbdz
  real(KIND_R8)    :: drag, m0
  integer(KIND_I4) :: j, i

  if (present(rc)) rc = 0

  do j=1,self%jnum
    do i=1,self%inum
      if (.not.self%mask(i,j,1)) then

        drpdz = 0 ! pelagic respiration with depth
        drbdz = self%respiration_rate / self%predation_depth &
          * exp (self%depth(i,j)/self%predation_depth)

        m0 = self%visibility * (self%biomass_endobenthic(i,j) &
           * exp (-self%depth_endobenthic(i,j)/self%predation_depth) &
           + self%biomass_epibenthic(i,j))

        drag = self%drag * (self%depth_epibenthic(i,j) + self%roughness_length(i,j) &
          / 2 * (4 * exp (-self%depth_epibenthic(i,j)/self%roughness_length(i,j)) &
          -exp (-2*self%depth_epibenthic(i,j)/self%roughness_length(i,j)) - 3))

        dmpdz = -m0 * self%risk_amplification &
          * drag * exp(-self%depth_epibenthic(i,j)/self%roughness_length(i,j))
        dmbdz = -m0 * exp(-self%depth_endobenthic(i,j)/self%predation_depth)

        dppdz = self%epibenthic_affinity * self%bulk_speed(i,j)  &
          * self%poc(i,j) / self%roughness_length(i,j)  &
          * exp (-self%depth_epibenthic(i,j)/self%roughness_length(i,j))

        dpbdz = self%endobenthic_affinity * self%poc(i,j) &
          / self%bioturbation_depth &
          * exp (-self%depth_endobenthic(i,j)/self%bioturbation_depth)

        rhs(i,j,1,1) = 1.0
        rhs(i,j,1,2) = 1.0
        rhs(i,j,1,3) = dppdz - drpdz - dmpdz

        ! Change in endobenthic depth
        rhs(i,j,1,4) = dpbdz - drbdz - dmbdz
      endif
    end do
 end do

end subroutine get_rhs


end module vertical_macrobenthos_driver
