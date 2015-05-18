!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!  HAMSOM (HAMburg Shelf Ocean Model) - hydrodynamic model         !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! !INTERFACE:
   PROGRAM HAMSOM
!
! !DESCRIPTION:
!! @mainpage HAMSOM
!! The development of the HAMSOM coding goes back to the mid eighties where it emerged from a fruitful co-operation between Backhaus and
!! Maier-Reimer who later called his model 'HOPE'. From the very beginning HAMSOM was designed with the intention to allow simulations of
!! both oceanic and coastal and shelf sea dynamics. <br> <br>
!!
!! The primitive equation model with a free surface utilises two time-levels, and is defined in Z co-ordinates on the Arakawa C-grid.
!! Stability constraints for surface gravity waves and the heat conduction equation are avoided by the implementation of implicit
!! schemes. With a user defined weighting between future and presence time levels a hierarchy of implicit schemes is provided to solve
!! for the free surface problem, and for the vertical transfer of momentum and water mass properties.
!! In the time domain a scheme for the Coriolis rotation is incorporated which has second order accuracy. Time- and space-dependent
!! vertical exchange and diffusivity coefficients are determined from a simple zero-order turbulence closure scheme which has also been
!! replaced by a higher order closure scheme (GOTM). The resolution of a water column may degenerate to just one grid cell. At the seabed
!! a non-linear (implicit) friction law as well as the full kinematic boundary condition is applied. Seabed cells may deviate from an
!! undisturbed cell height to allow for a better resolution of the topography. The HAMSOM coding excludes any time-splitting, i.e. free
!! surface and internal baroclinic modes are always directly coupled. Simple upstream and more sophisticated advection schemes for both
!! momentum and matter may be run according to directives from the user.
!!
!! Successful couplings with eco-system models (ECOHAM, ERSEM), an atmospheric model (REMO), and both Lagrangian and Eulerian models for
!! sediment transport are reported in the literature. For polar applications HAMSOM was coupled with a viscous-plastic
!! thermo-hydrodynamic ice model of Hibler type. Since about 15 years in Hamburg, and overseas in more than 30 laboratories, HAMSOM is
!! already being in use as a community model.
!
! !CONVENTIONS:
!  M    = i = North South !=lat = Y = V
!  N    = j = West East   !=lon = X = U
!  ILO  = k = Top Bottom  !=km  = Z = W
!
! !USES:
   use hamsom_driver
   use CommonVar, only : EndOfIteration
   use ConfigVar, only : RunNumber, dt
   use TimeVar,   only : Year
!
   IMPLICIT NONE
!
! !LOCAL VARIABLES:
   integer           :: istep_main, nstep_main, dt_main
   character(len=4)  :: Str4

!-----------------------------------------------------------------------

!*****************************************************************
!  initialisation
!*****************************************************************
   print*,'-----------------------------'
   print*,'---      INITIALIZE       ---'
   print*,'-----------------------------'
   ! get RunID and Year as program parameter
   if (iargc() > 0) then
      call getarg(1, RunNumber)
      call getarg(2, Str4)
      read(Str4, *) Year
   else ! default settings
      RunNumber = '999'; Year = 1999
   endif

   call init_hamsom
   print*,''; print*,' ... HAMSOM component initialized !';print*,''

   dt_main    = nint(dt)       ! main timestep in seconds !!
   nstep_main = EndOfIteration ! number of iterations

!*****************************************************************
!  main time-loop
!*****************************************************************
   print*,'-----------------------------'
   print*,'---   STARTING MAINLOOP   ---'
   print*,'-----------------------------'
   do istep_main = 1, nstep_main
      call do_mainstep_hamsom(dt_main)
   enddo

!*****************************************************************
!  finish
!*****************************************************************
   print*,'-----------------------------'
   print*,'---        FINALIZE       ---'
   print*,'-----------------------------'
   call finalize_hamsom

   end ! PROGRAM HAMSOM
