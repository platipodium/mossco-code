module macrofauna_interface

! This module is an interface to benthos ESMF-compenent in MOSSCO project for macrofauna species.
! All benthos objects are (and should be) defined as subclasses of benthosEffect class (which is an abstract object).
! Macrofauna species are subclasses of Macrofauna class, which itself is a subclass of benthosEffect class.
! This interface has been designed to simplify extending macrofauna types, so that the developer need not
! to change anything in benthos ESMF-component.
! Each new macrofauna species muss be defined as a subclass of Macrofauna class, and
! thier methods should be called within this module.
! In the following the variable Total_Bioturbation includes the following:
!type BioturbationEffect
! real (fp)          , pointer  :: TauEffect => null()    ! critical bed shear stress
! real (fp)          , pointer  :: ErodibilityEffect => null()
! real (fp)          , pointer  :: d50=> null()
! real (fp)          , pointer  :: MudContent=> null()
!end type BioturbationEffect


! so that the effect of different macrofauna species on the critical bed shear stress and erodibility
! are multiplied linearly to get the total effect. This approach is a simplification of the preocesses,
! and not a physically based approach, but state-of-the-art (refer to Knappen et al. 2003, Borsje et al. (2008)
! and Paarlberg et al (2005).

use mbalthica_class
use biotypes , only : BioturbationEffect

implicit none
! New Macrofauna types should be declared here.

type (Mbalthica_Object),save   :: M_Balthica


contains

subroutine Macrofanua_init(Total_Bioturbation)
! All macrofauna species are initialized here.
! In case feuther features of macrofauna effect have been added, the corresponding pointers should be
! deallocated here.
implicit none

type (BioturbationEffect) :: Total_Bioturbation

allocate (Total_Bioturbation%ErodibilityEffect(1,1,1), Total_Bioturbation%TauEffect(1,1,1))

Total_Bioturbation%ErodibilityEffect= 1.0_fp
Total_Bioturbation%TauEffect=1.0_fp

! The initilize method of new species should be called here
call init_Mbalthica(M_Balthica)

end subroutine Macrofanua_init
!*********************************************************************************
subroutine Macrofanua_set()
! The set method of new species should be called here, to read and set data

call set_Mbalthica(M_Balthica)

end subroutine Macrofanua_set
!*********************************************************************************
subroutine Macrofanua_run(Total_Bioturbation)

implicit none

type (BioturbationEffect) ::Total_Bioturbation

! The run method of new species should be called here, to calculate the biological effect of
! macrofauna on sediment flux.

call run_Mbalthica(M_Balthica)

! It should be noted that currently only the biological effect on the critical bed shear stress and
! erodibiity are considered, althogh the superclass macrofauna includes many other effects.
! In case further effects such as biogenicstructures on flow, direct bio-resuspension und deposition
! and etc. are going to be activated, they should be considered in the corresponding parameters with the class.
! In case of extending the following equation to further effects such as i.e. Tellina fabula it should look like the following:
!Total_Bioturbation%TauEffect = M_Balthica%Bioturbation%TauEffect * T_fabula%Taueffect * ...

Total_Bioturbation%ErodibilityEffect = M_Balthica%Bioturbation%ErodibilityEffect
Total_Bioturbation%TauEffect = M_Balthica%Bioturbation%TauEffect

end subroutine Macrofanua_run
!*********************************************************************************
subroutine Macrofanua_fin(Total_Bioturbation)
! The finialize method of new species should be called here.
implicit none

type (BioturbationEffect)  ::Total_Bioturbation

call fin_Mbalthica(M_Balthica)

! In case feuther features of macrofauna effect have been added, the corresponding pointers should be
! deallocated here
deallocate (Total_Bioturbation%ErodibilityEffect, Total_Bioturbation%TauEffect)

end subroutine Macrofanua_fin
!*********************************************************************************
end module macrofauna_interface



