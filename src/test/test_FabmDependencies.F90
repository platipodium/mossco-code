!> @file test_FabmDependencies.F90
!! @brief list FABM dependencies
!! @author Richard Hofmeister
!!
!! lists FABM dependencies for a present fabm.nml

#define LEVEL2 write(0,*) '  ',
#define LEVEL3 write(0,*) '    ',

program test_FabmDependencies

use fabm
use fabm_driver
use fabm_config
use fabm_types

type(type_model),pointer         :: model
integer                          :: namlst=55
type (type_bulk_variable_id)     :: id
type (type_horizontal_variable_id) :: id_hz
type (type_link),       pointer :: link

allocate(driver)
model => fabm_create_model_from_file(namlst)

#if 0
  ! Report prognostic variable descriptions
  LEVEL2 'FABM pelagic state variables:'
  do i=1,size(model%state_variables)
     LEVEL3 trim(model%state_variables(i)%name), '  ', &
            trim(model%state_variables(i)%units),'  ',&
            trim(model%state_variables(i)%long_name), &
            'missing_value:',model%state_variables(i)%missing_value
  end do

  LEVEL2 'FABM benthic state variables:'
  do i=1,size(model%state_variables_ben)
     LEVEL3 trim(model%state_variables_ben(i)%name), '  ', &
            trim(model%state_variables_ben(i)%units),'  ',&
            trim(model%state_variables_ben(i)%long_name)
  end do

  ! Report diagnostic variable descriptions
  LEVEL2 'FABM diagnostic variables defined on the full model domain:'
  do i=1,size(model%diagnostic_variables)
     LEVEL3 trim(model%diagnostic_variables(i)%name), '  ', &
            trim(model%diagnostic_variables(i)%units),'  ',&
            trim(model%diagnostic_variables(i)%long_name)
  end do

  LEVEL2 'FABM diagnostic variables defined on a horizontal slice of the model domain:'
  do i=1,size(model%diagnostic_variables_hz)
     LEVEL3 trim(model%diagnostic_variables_hz(i)%name), '  ', &
            trim(model%diagnostic_variables_hz(i)%units),'  ',&
            trim(model%diagnostic_variables_hz(i)%long_name)
  end do
#endif

  !> taken from helper.F90 in pyfabm
  !! todo: discuss, if this should go into fabm.F90:
  LEVEL2 ''
  LEVEL2 'FABM environmental dependencies'
  link => model%links_postcoupling%first
  do while (associated(link))
    if (.not.link%target%read_indices%is_empty().and.link%target%state_indices%is_empty()) then
      select case (link%target%domain)
        case (domain_interior)
          if (.not.associated(model%data(link%target%read_indices%pointers(1)%p)%p) &
              .and..not.(link%target%presence==presence_internal)) then
            LEVEL3 '  interior: ',trim(link%name),' [',trim(link%target%units),']'
          end if
  case (domain_horizontal,domain_bottom,domain_surface)
          if (.not.associated(model%data_hz(link%target%read_indices%pointers(1)%p)%p) &
              .and..not.(link%target%presence==presence_internal)) then
            LEVEL3 'horizontal: ',trim(link%name),' [',trim(link%target%units),']'
          end if
        case (domain_scalar)
          if (.not.associated(model%data_scalar(link%target%read_indices%pointers(1)%p)%p) &
              .and..not.(link%target%presence==presence_internal)) then
            LEVEL3 '    global: ',trim(link%name),' [',trim(link%target%units),']'
          end if
      end select
    end if
    link => link%next
  end do

  !call fabm_check_ready(model)

end program
