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

type(type_model),pointer         :: model
integer                          :: namlst=55
type (type_bulk_variable_id)     :: id
type (type_horizontal_variable_id) :: id_hz

allocate(driver)
model => fabm_create_model_from_file(namlst)

  ! Report prognostic variable descriptions
  LEVEL2 'FABM pelagic state variables:'
  do i=1,size(model%state_variables)
     LEVEL3 trim(model%state_variables(i)%name), '  ', &
            trim(model%state_variables(i)%units),'  ',&
            trim(model%state_variables(i)%long_name)
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

  ! Report bulk dependencies descriptions
  LEVEL2 'FABM bulk dependencies:'
  do i=1,size(model%dependencies)
     id = fabm_get_bulk_variable_id(model,model%dependencies(i))
     ! list, if the variable is not a state variable
     ! and the data pointer is associated:
     if ((id%state_index == -1).and.associated(id%p)) LEVEL3 trim(model%dependencies(i)),size(id%alldata)
  end do

  ! Report bulk dependencies descriptions
  LEVEL2 'FABM horizontal dependencies:'
  do i=1,size(model%dependencies_hz)
     id_hz = fabm_get_horizontal_variable_id(model,model%dependencies(i))
     ! list, if the variable is not a state variable
     if (id_hz%state_index == -1) &
       LEVEL3 trim(model%dependencies_hz(i)),size(id%alldata)
  end do

  ! Report bulk dependencies descriptions
  LEVEL2 'FABM scalar dependencies:'
  do i=1,size(model%dependencies_scalar)
     LEVEL3 trim(model%dependencies_scalar(i))
  end do

end program
