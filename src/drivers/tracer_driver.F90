!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!  Lagrangian trajectories of the particles drifting under         !!!
!!!       influence of current fields and (turbulent) duffisivity    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! !INTERFACE:
   module tracer_driver
!
! !DESCRIPTION:
!
! !CONVENTIONS:
!  M    = i = North South !=lat = Y = V
!  N    = j = West East   !=lon = X = U
!  ILO  = k = Top Bottom  !=km  = Z = W
!
!  Original author(s): Markus Kreus
!
! !USES:
!$ use omp_lib
   use grid
   use forcing
   use particles
   use traort4
   use output
   use utils
!
   IMPLICIT NONE
!
! !PUBLIC MEMBER FUNCTIONS:
!!  default: all is private
   private
   public init_tracer, do_mainstep_tracer, finalize_tracer
   public U_in, V_in, Salt_in, Temp_in, Av_in, Ah_in, Sm_in, Zeta_in
!
! !PUBLIC DATA MEMBERS:
   integer, public   :: ltg_max           ! max number of iterations (timesteps)
   integer, public   :: dt                ! timestep (h) for main loop (for reading forcing, particle release etc.)
!
! !LOCAL VARIABLES:
   integer           :: year,month,day,hour,doy
!    integer           :: ltg,ltg_max,nsteps
   integer           :: ltg,nsteps
!   integer           :: lp,ilp0
!   character(100)    :: file_nml
! from namelist
  integer           :: yearstart          ! starttime year
!   integer           :: daystart          ! starttime day (of year)
!   integer           :: hourstart         ! starttime hour (of startday)
!   integer           :: simdays           ! duration of simulation (days)
!   integer           :: dt                ! timestep (h) for main loop (for reading forcing, particle release etc.)
   character(16)     :: timestr           ! time as string
!   character(200)    :: path_to_griddata  ! path to grid infos
   !character(200)    :: path_to_forcing   ! path to forcing data
  logical           :: dump_warmstart    ! write warmstart file (each timestep)
!
!=======================================================================
   contains
!=======================================================================
!
! !INTERFACE:
   subroutine init_tracer(RunID,strS,strE,dt_main)
!
! !DESCRIPTION:
!
! !USES:
!
   implicit none
!
! !INPUT PARAMETERS:
  character(len=99), intent(in), optional :: RunID
  character(len=16), intent(in), optional :: strS, strE
  integer          , intent(in), optional :: dt_main ! main timestep (s)
!
! !LOCAL VARIABLES:
!   integer           :: lp,ilp0
   character(100)    :: file_nml
! from namelist
!   integer           :: yearstart         ! starttime year
   integer           :: daystart          ! starttime day (of year)
   integer           :: hourstart         ! starttime hour (of startday)
   integer           :: simdays           ! duration of simulation (days)
!    integer           :: dt                ! timestep (h) for main loop (for reading forcing, particle release etc.)
!   character(16)     :: timestr           ! time as string
   character(200)    :: path_to_griddata  ! path to grid infos
   !character(200)    :: path_to_forcing   ! path to forcing data
!ma   logical           :: dump_warmstart    ! write warmstart file (each timestep)
   integer           :: yearE, monthE, dayE, hourE

   namelist /gerneral_nml/  yearstart,daystart,hourstart,simdays,dt,AH_const, &
                            path_to_griddata,path_to_forcing,dt_forcing, &
                            dump_warmstart

!-----------------------------------------------------------------------

!  initialize defaults
   dump_warmstart=.false.
   hourstart = 0 ! default starting hour is at midnight
   !hourstart = 12 ! default starting hour is at noon (12:00 am)

!  initialize random generator
   call random_seed()

!  initialise general settings from namelist
   file_nml = 'tracer.nml'
   print*,' '
   print*,'-----------------------------'
   print*,'  read namelist: ',trim(file_nml)
   open(1,file=file_nml,action='read',status='old',err=90)
   read(1,nml=gerneral_nml,err=91)
   read(1,nml=particles_nml,err=92)
   read(1,nml=traort_nml,err=93)
   read(1,nml=output_nml,err=94)
   close(1)
   print*,'     - done -'

   doy = daystart
   call DOY2Date(doy, day, month, yearstart)

!=============   IMPORTANT FOR COUPLING !!  ================
   ! override namelist settings with global definitions
   if (present(RunID)) prefix_out = trim(RunID)

   ! set starttime to global definition
   if (present(strS)) then
      read(strS,'(i4.4,x,i2.2,x,i2.2,x,i2.2,x)') yearstart, month, day, hourstart
      daystart = GetDOY(day, month, yearstart, 0)
   endif

   ! set duration of simulation by global definitions
   if (present(strE)) then
      read(strE,'(i4.4,x,i2.2,x,i2.2,x,i2.2,x)') yearE, monthE, dayE, hourE
      simdays = GetDOY(dayE, monthE, yearE, 0) - daystart
   endif

   ! limit timestep according to coupling timestep
   if (present(dt_main)) then
     dt = min(dt, int(dt_main/3600.)) ! timestep (h)
      ! check if dt matches global timestep (dt_main)
      if (dt*3600.ne.dt_main) then
         write(*,'(a12,i3,a7)') 'tracer timestep is ',dt*3600,' seconds'
         write(*,'(a12,i3,a7)') 'global timestep is ',dt_main,' seconds.'
         write(*,'(a50)') 'Adjust either tracer or global timestep accordingly'
         stop '=================================================='
      endif
   endif
!===========================================================

   ! check if dt_out matches timestep (dt)
   if (mod(dt_out,dt).ne.0) then
      write(*,'(a12,i3,a7)') 'timestep is ',dt,' hours.'
      write(*,'(a38,i3,a7)') 'Please adjust output timestep (dt_out=',dt_out,')'
      stop '=================================================='
   endif

   !          1234567890123456
   timestr = 'YYYY-MM-DD hh:mm'
   write(timestr( 1: 4),'(i4.4)') yearstart
   write(timestr( 6: 7),'(i2.2)') month
   write(timestr( 9:10),'(i2.2)') day
   write(timestr(12:13),'(i2.2)') hourstart
   write(timestr(15:16),'(i2.2)') 0

!  Steps within in TRAORT for each timestep
   NSTEPS  = nint(dt*60*60/DTG)
   XDEFAULT =  max(1,min(N-NWEG,int(XDEFAULT)))
   YDEFAULT =  max(1,min(M-NWEG,int(YDEFAULT)))
   ZDEFAULT =  max(1,min(ILO-NWEG,int(ZDEFAULT)))

!  initialise modules
   print*,'-----------------------------'
   print*,'  initialise modules'
   print*,'       grid'
   call init_grid(path_to_griddata)
   print*,'     particles'
   call init_particles
   print*,'      forcing'
   call init_forcing(timestr,dt)
   print*,'      output'
   call init_output(timestr)
   print*,'     - done -'

!    print*,'-----------------------------'
!    print*,'---   STARTING MAINLOOP   ---'
!    print*,'-----------------------------'

   year = yearstart
   doy  = daystart     ! day of year
   hour = hourstart    ! hour of day

   ltg = 0
   ltg_max = simdays*hours_per_day/dt

   return

90 print*,'I could not open ',trim(file_nml)
   stop
91 print*,'I could not read general_nml'
   stop
92 print*,'I could not read particles_nml'
   stop
93 print*,'I could not read traort_nml'
   stop
94 print*,'I could not read output_nml'
   stop

   end subroutine init_tracer
!
!=======================================================================
!
! !INTERFACE:
   subroutine do_mainstep_tracer(dt_main)
!
! !DESCRIPTION:
!
! !USES:
!
   implicit none
!
! !INPUT PARAMETERS:
   integer, intent(in) :: dt_main
!
! !LOCAL VARIABLES:
   integer             :: lp,ilp0
   integer             :: cum_dt
!
!-----------------------------------------------------------------------

   if (.false.) print*,dt_main ! to get rid of "unused ..."

   ! skip all below, if global time is beyond tracer simtime
   if (ltg > ltg_max) return

   cum_dt = 0
!*****************************************************************
   do while (cum_dt < dt_main) !  main time-loop: do LTG = 0,LTG_max-1
!*****************************************************************
!       ltg = ltg + 1
      write(*,'(a5,i4,x,a4,i4,a5,a16,a2,i3,a17,i8)') &
               'step ',ltg,'of ',LTG_max,' --- ', timestr,   &
               ' (',doy,') --- particles: ',ilp

!*****************************************************************
!  Getting T, S, U, V, Av and Sm, calculating W
!*****************************************************************
#ifndef Forcing_Online
      if (mod(ltg*dt,dt_forcing) .eq. 0.) then
         !print*,'read_forcing',day,hour
         call read_forcing(timestr,dt)
         !print*,'-done-'
      endif
#endif
      call update_forcing

!*****************************************************************
!  Starting new particles each new day (not each timestep !!)
!*****************************************************************
      if (mod(ltg*dt,hours_per_day) .eq. 0.) then

         if((doy.ge.tstart) .and. (doy.le.tstart+tspawn-1) .and. &
            (year.eq.yearstart).and.(mod(doy-tstart,spawn_step).eq.0.)) then

            ilp0 = ilp

            !print*,'release_particles',day
            call release_particles(timestr)
            !print*,'-done-',ilp,ilp0

            if(ilp.gt.ilp0) then
               ilp0 = max(1,ilp0+1)
               do LP= ilp0, ilp

                  call get_data(XPOS(lp),YPOS(lp),ZPOS(lp))

                  call write_output(lp,timestr)

               enddo
            endif

         endif
      endif

!*****************************************************************
!   Particle loop
!*****************************************************************
      if(ilp.ge.1 .and. tmax.gt.0) then
         !if(ilp.ge.1) then

         if (dump_warmstart) then
            write(*,'(a20,2i5)') 'write_warmstart - year/day',year,day
            call write_warmstart
         endif

         !print*,'do particle loop'
!$omp parallel do
         do LP= 1, ilp
            call traort (NSTEPS,XPOS(lp),YPOS(lp),ZPOS(lp))
            traveltime(lp) = traveltime(lp) + dt/hours_per_day
         enddo
!$omp end parallel do

         !print*,'-done-'
      endif

!*****************************************************************
!   update time
!*****************************************************************
      hour = hour + dt
      if (hour .ge. hours_per_day) then
         hour = mod(hour,hours_per_day)
         doy = doy + 1

         ! adjust if simulation exceeds calendar year
         if (doy .gt. 365) then
            !print*,'simulation exceeds calendar year'
            doy  = 1
            year = year+1
            call close_forcing

            call DOY2Date(doy, day, month, year)
            !          1234567890123456
            !timestr ='YYYY-MM-DD hh:mm'
            write(timestr( 1: 4),'(i4.4)') year
            write(timestr( 6: 7),'(i2.2)') month
            write(timestr( 9:10),'(i2.2)') day
            write(timestr(12:13),'(i2.2)') hour
            write(timestr(15:16),'(i2.2)') 0

            call init_forcing(timestr,dt)
         endif
      endif

      call DOY2Date(doy, day, month, year)
      !          1234567890123456
      !timestr ='YYYY-MM-DD hh:mm'
      write(timestr( 1: 4),'(i4.4)') year
      write(timestr( 6: 7),'(i2.2)') month
      write(timestr( 9:10),'(i2.2)') day
      write(timestr(12:13),'(i2.2)') hour
      write(timestr(15:16),'(i2.2)') 0

!*****************************************************************
!   write output
!*****************************************************************
      if (ilp.ge.1 .and. tmax.gt.0) then
         if (mod((ltg+1)*dt,dt_out) .eq. 0.) then
            do LP= 1, ilp
               call get_data(XPOS(lp),YPOS(lp),ZPOS(lp))
               call write_output(lp,timestr)
            enddo
         endif
      endif

!*****************************************************************
!   remove_particles (once a day)
!*****************************************************************
      if (mod((ltg+1)*dt,hours_per_day) .eq. 0.) then
         if(ilp.ge.1)  call remove_particles
      endif

      cum_dt = cum_dt + dt*3600
      ltg = ltg + 1
      !print*,'#348 ',ltg, ltg_max, cum_dt, dt_main

!*****************************************************************
   enddo ! tracer time loop (ltg)
!*****************************************************************

   return
   end subroutine do_mainstep_tracer
!
!=======================================================================
!
! !INTERFACE:
   subroutine finalize_tracer
!
! !DESCRIPTION:
!  Reads the namelist and makes calls to the init functions of the
!  various model components.
!
! !USES:
!
!-----------------------------------------------------------------------

   !print*,'timeloop finished'

   call close_forcing
   call close_output
   print*,'-----------------------------'
   print*,'---         END           ---'
   print*,'-----------------------------'
   return

   end subroutine finalize_tracer

!=======================================================================
   end module tracer_driver
!=======================================================================

!-----------------------------------------------------------------------
! Copyright (C) 2014 - Markus Kreus                                    !
!-----------------------------------------------------------------------
