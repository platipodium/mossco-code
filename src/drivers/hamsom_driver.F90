!=======================================================================
! HAMSOM - hydrodynamic model
!=======================================================================
!
! !DESCRIPTION:
!
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
!!
!!
!! @author Bernhard Mayer, Johannes Paetsch, Markus Kreus
!!
!
! !INTERFACE:
   module hamsom_driver
!
! !USES:
!    use AverageVar, only : AvrT, AvrS, AvrU, AvrV, AvrCumZ, AvrAv, AvrAh, AvrSm
!
   IMPLICIT NONE
!
! !PUBLIC MEMBER FUNCTIONS:
!!  default: all is private
   private
   public init_hamsom, do_mainstep_hamsom, finalize_hamsom
!    public AvrT, AvrS, AvrU, AvrV, AvrCumZ, AvrAv, AvrAh, AvrSm
!
!=======================================================================
   contains
!=======================================================================
!
! !INTERFACE:
   subroutine init_hamsom(RunID,strS,strE,dt_main)
!
! !DESCRIPTION:
!
! !USES:
   use CommonVar,  only : Restart
   use ConfigVar,  only : RunNumber, DayLength, dt
   use TimeVar,    only : Day, Month, Year, Hour, Minute, Second
   use TimeVar,    only : dayE, monthE, StepOfIteration, SecondOfYear
   use AverageVar, only : AverageDT
   implicit none
!
! !INPUT PARAMETERS:
  character(len=*),  intent(in), optional :: RunID
  character(len=16), intent(in), optional :: strS, strE
  real,              intent(in), optional :: dt_main
! !LOCAL VARIABLES:
  integer :: DOY, getDOY, SOY, TOY, yy, mm, dd, hh, mn, ierr
!
!-----------------------------------------------------------------------

   call ReadConfig

!=============   IMPORTANT FOR COUPLING !!  ================
   ! override namelist settings with global definitions
   if (present(RunID)) RunNumber = trim(RunID)

   ! set starttime to global definition
   if (present(strS)) then
      read(strS,'(i4.4,x,i2.2,x,i2.2,x,i2.2,x,i2.2)') Year, Month, Day, Hour, Minute
   endif
   Second = 0

   ! set endtime to global definition
   if (present(strE))  read(strE,'(4x,x,i2.2,x,i2.2,x)') monthE, dayE

   ! set coupling timestep
   if (present(dt_main)) then
      ! set averaging timestep according to coupling timestep
      AverageDT = nint(dt_main)
      ! limit HAMSOM timestep according to coupler timestep
      dt = min(dt, dt_main) ! timestep (h)
   endif
!===========================================================
   call init

   if (Restart .and. present(strS)) then
      ierr = 0
      read(strS,'(i4.4,x,i2.2,x,i2.2,x,i2.2,x,i2.2)') yy, mm, dd, hh, mn
      if (yy /= Year)  ierr = ierr+1
      if (mm /= Month) ierr = ierr+1
      if (dd /= Day)   ierr = ierr+1
      if (hh /= Hour)  ierr = ierr+1

      if (ierr/=0) then
         !print*,' FATAL: timestamp from HAMSOM-restart file is different from startdate'
         !stop 'init_hamsom # 98'
         TOY = getDOY(Day, Month, 0)
         DOY = getDOY(dd, mm, 0)
         SOY = Second + (mn + hh*60)*60 + (DOY-1)*DayLength
         print*,''
         print*,'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
         print*,'+++  WARNING: timestamp from HAMSOM-restart file differs from global startdate   +++'
         print*,'+++              -> reset HAMSOM starttimes :                                    +++'
         print*,'+++                                    Year from ',Year, ' to ',yy,           '  +++'
         print*,'+++                                   Month from ',Month,' to ',mm,           '  +++'
         print*,'+++                                     Day from ',Day,  ' to ',dd,           '  +++'
         print*,'+++                                    Hour from ',Hour, ' to ',hh,           '  +++'
         print*,'+++                               DayOfYear from ',TOY,  ' to ',DOY,          '  +++'
         print*,'+++                            SecondOfYear from ',SecondOfYear,' to ',SOY,   '  +++'
         print*,'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
         print*,''
         Year = yy; Month = mm; Day = dd; Hour = hh; SecondOfYear = SOY
      endif
   endif

   StepOfIteration = 1

   return

   end subroutine init_hamsom
!
!=======================================================================
!
! !INTERFACE:
   subroutine do_mainstep_hamsom(dt_main)
!
! !DESCRIPTION:
!
! !USES:
   use ConfigVar,   only : dt
   use TimeVar,     only : StepOfIteration
   implicit none
!
! !INPUT PARAMETERS:
   real, intent(in) :: dt_main
!
! !LOCAL VARIABLES:
   real             :: cum_dt
!
!-----------------------------------------------------------------------

   cum_dt = 0.0

   do while (cum_dt < dt_main)
      call hamsom_step
      cum_dt = cum_dt + dt
      StepOfIteration = StepOfIteration + 1
   enddo
! print*,'hamsom_driver#161 ',AvrU(61,3,45),AvrV(61,3,45),AvrT(61,3,45),AvrS(61,3,45)

   return

   end subroutine do_mainstep_hamsom
!
!=======================================================================
!
! !INTERFACE:
   subroutine finalize_hamsom
!
! !DESCRIPTION:
!
! !USES:
   implicit none
!
!-----------------------------------------------------------------------

   call finish

!    print*,'-----------------------------'
!    print*,'---     END  HAMSOM       ---'
!    print*,'-----------------------------'
   return

   end subroutine finalize_hamsom
!
!=======================================================================
   end module hamsom_driver
!=======================================================================

!-----------------------------------------------------------------------
! Copyright (C) 2014 - Markus Kreus                                    !
!-----------------------------------------------------------------------
