#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: getm_driver - driver routines for GETM
!
! !INTERFACE:
   module getm_driver
!
! !DESCRIPTION:
!
! !USES:
   use initialise, only: runtype,dryrun
!  these variables are needed in init_time(), but cannot be included there
!  because of name-clash with NML
   use time, only: start,stop,timestep,days_in_mon
   use time, only: jul0,secs0,juln,secsn,julianday,secondsofday,simtime
   use time, only: String2JulSecs,time_diff,write_time_string,update_time
   IMPLICIT NONE

   interface
      subroutine tracer_diffusion(f,hn,AH_method,AH_const,AH_Prt,AH_stirr_const, &
                                  phymix)
         use domain, only: imin,imax,jmin,jmax,kmax
         IMPLICIT NONE
         REALTYPE,intent(in)           :: hn(I3DFIELD)
         integer,intent(in)            :: AH_method
         REALTYPE,intent(in)           :: AH_const,AH_Prt,AH_stirr_const
         REALTYPE,intent(inout)        :: f(I3DFIELD)
         REALTYPE,intent(out),optional :: phymix(I3DFIELD)
      end subroutine tracer_diffusion
   end interface

   private
!
! !PUBLIC DATA MEMBERS:
   public preinit_model,postinit_model,init_time,time_step
   public do_transport,do_transport_3d
   public zero_gradient_3d_bdy
   character(len=64)                   :: runid
   character(len=80)                   :: title
   logical                             :: hotstart=.false.
   logical                             :: use_epoch=.false.
   logical                             :: save_initial=.false.
   character(len=PATH_MAX)             :: input_dir=''
!
! !REVISION HISTORY:
!  Original author(s): Knut Klingbeil
!
!EOP
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: preinit_model - first part of init_model
!
! !INTERFACE:
   subroutine preinit_model(dstr,tstr)
!
! !DESCRIPTION:
!  Reads the namelist and initialises parallel runs.
!
! !USES:
   use kurt_parallel, only: init_parallel
#ifdef GETM_PARALLEL
   use halo_mpi, only: init_mpi
#endif
   use parameters, only: init_parameters
   use getm_timers, only: init_getm_timers, tic, toc, TIM_INITIALIZE
   use exceptions
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   character(len=*)                    :: dstr,tstr
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
! !LOCAL VARIABLES:
   logical                   :: parallel=.false.
   character(len=PATH_MAX)   :: namlst_file=''
   namelist /param/ &
             dryrun,runid,title,parallel,runtype,  &
             hotstart,use_epoch,save_initial
!EOP
!-------------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'preinit_model() # ',Ncall
#endif
#ifndef NO_TIMERS
   call init_getm_timers()
#endif
   ! Immediately start to time (rest of) init:
   call tic(TIM_INITIALIZE)

   ! We need to pass info about the input directory
#if 0
   call getarg(1,base_dir)
   if(len_trim(base_dir) .eq. 0) then
      call getenv("base_dir",base_dir)
   end if
   if(len_trim(base_dir) .gt. 0) then
      base_dir = trim(base_dir) // '/'
   end if
#endif

!
! In parallel mode it is imperative to let the instances
! "say hello" right away. For MPI this changes the working directory,
! so that input files can be read.
!
#ifdef GETM_PARALLEL
   call init_mpi()
#endif

#ifdef INPUT_DIR
   input_dir=trim(INPUT_DIR) // '/'
   STDERR 'input_dir:'
   STDERR input_dir
#endif
#ifdef _NAMLST_FILE_
   namlst_file=trim(_NAMLST_FILE_)
#else
   namlst_file=trim(input_dir) // 'getm.inp'
#endif
!
! Open the namelist file to get basic run parameters.
!
   title='A descriptive title can be specified in the param namelist'
   open(NAMLST,status='unknown',file=namlst_file)
   read(NAMLST,NML=param)

#ifdef NO_BAROCLINIC
   if(runtype .ge. 3) then
      FATAL 'getm not compiled for baroclinic runs'
      stop 'preinit_model()'
   end if
#endif

#ifdef NO_3D
   if(runtype .ge. 2) then
      FATAL 'getm not compiled for 3D runs'
      stop 'preinit_model()'
   end if
#endif

! call all modules init_ ... routines

   if (parallel) then
#ifdef GETM_PARALLEL
      call init_parallel(runid,input_dir)
#else
      STDERR 'You must define GETM_PARALLEL and recompile'
      STDERR 'in order to run in parallel'
      stop 'preinit_model()'
#endif
   end if

   STDERR LINE
   STDERR 'getm ver. ',RELEASE,': Started on  ',dstr,' ',tstr
   STDERR LINE
   STDERR 'Initialising....'
   STDERR LINE
   LEVEL1 'the run id is: ',trim(runid)
   LEVEL1 'the title is:  ',trim(title)

   select case (runtype)
      case (1)
         LEVEL1 '2D run (hotstart=',hotstart,')'
      case (2)
         LEVEL1 '3D run - no density (hotstart=',hotstart,')'
      case (3)
         LEVEL1 '3D run - frozen density (hotstart=',hotstart,')'
      case (4)
         LEVEL1 '3D run - full (hotstart=',hotstart,')'
      case default
         FATAL 'A non valid runtype has been specified.'
         stop 'preinit_model()'
   end select

   call init_parameters()

#ifdef DEBUG
   write(debug,*) 'Leaving preinit_model()'
   write(debug,*)
#endif
   return
   end subroutine preinit_model
!EOC
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: postinit_model - second part of init_model
!
! !INTERFACE:
   subroutine postinit_model()
!
! !DESCRIPTION:
!  Makes calls to the init functions of the
!  various model components.
!
! !USES:
   use kurt_parallel, only: myid
   use output, only: init_output,do_output,restart_file,out_dir
   use input,  only: init_input
   use domain, only: init_domain
   use domain, only: kmax
   use time, only: update_time,write_time_string
   use time, only: start,timestr,timestep
   use m2d, only: init_2d,postinit_2d,depth_update
   use variables_2d, only: zo,z,D,Dvel,DU,DV
   use les, only: init_les
   use getm_timers, only: init_getm_timers, tic, toc, TIM_INITIALIZE
#ifndef NO_3D
   use m3d, only: init_3d,postinit_3d
#ifndef NO_BAROCLINIC
   use m3d, only: T,calc_temp,calc_salt
   use temperature, only: init_temperature_field
   use salinity, only: init_salinity_field
#endif
   use m3d, only: use_gotm
   use turbulence, only: init_turbulence
   use mtridiagonal, only: init_tridiagonal
   use rivers, only: init_rivers
#ifdef SPM
   use suspended_matter, only: init_spm
#endif
#ifdef _FABM_
   use getm_fabm, only: fabm_calc
   use getm_fabm, only: init_getm_fabm, postinit_getm_fabm
   use rivers, only: init_rivers_fabm
#endif
#ifdef GETM_BIO
   use bio, only: bio_calc
   use getm_bio, only: init_getm_bio
   use rivers, only: init_rivers_bio
#endif
#endif
   use meteo, only: metforcing,met_method,init_meteo,do_meteo
   use waves, only: init_waves,do_waves,waveforcing_method,NO_WAVES
   use integration,  only: MinN,MaxN
   use exceptions
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
! !LOCAL VARIABLES:
   character(len=8)          :: buf
   character(len=PATH_MAX)   :: hot_in=''
!EOP
!-------------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'postinit_model() # ',Ncall
#endif

   if(use_epoch) then
      LEVEL2 'using "',start,'" as time reference'
   end if

   call init_domain(input_dir)

   call init_meteo(hotstart)

   call init_waves(hotstart,runtype)

#ifndef NO_3D
   call init_rivers(hotstart)
#endif

   call init_2d(runtype,timestep,hotstart)

#ifndef NO_3D
   if (runtype .gt. 1) then
      call init_3d(runtype,timestep,hotstart)
      if (use_gotm) then
         call init_turbulence(60,trim(input_dir) // 'gotmturb.nml',kmax)
      end if
      call init_tridiagonal(kmax)

#ifdef SPM
      call init_spm(trim(input_dir) // 'spm.inp',runtype)
#endif
#ifdef _FABM_
      call init_getm_fabm(trim(input_dir) // 'getm_fabm.inp')
      call init_rivers_fabm
#endif
#ifdef GETM_BIO
      call init_getm_bio(trim(input_dir) // 'getm_bio.inp')
      call init_rivers_bio
#endif
   end if
#endif

   call init_les(runtype)

   call init_output(runid,title,start,runtype,dryrun,myid,MinN,MaxN,save_initial)

   close(NAMLST)

#if 0
   call init_biology(hotstart)
#endif

   if (hotstart) then
      LEVEL1 'hotstart'
      if (myid .ge. 0) then
         write(buf,'(I4.4)') myid
         buf = '.' // trim(buf) // '.in'
      else
         buf = '.in'
      end if
      hot_in = trim(out_dir) //'/'// 'restart' // trim(buf)
      call restart_file(READING,trim(hot_in),MinN,runtype,use_epoch)
      LEVEL3 'MinN adjusted to ',MinN
      call update_time(MinN)
      call write_time_string()
      LEVEL3 timestr
      MinN = MinN+1
#ifndef NO_BAROCLINIC
      if (calc_temp) then
         LEVEL2 'hotstart temperature:'
         call init_temperature_field()
      end if
      if (calc_salt) then
         LEVEL2 'hotstart salinity:'
         call init_salinity_field()
      end if
#endif
   end if

!  Note (KK): we need Dvel for do_waves()
!  KK-TODO: we would not need Dvel if we use H for WAVES_FROMWIND
   call depth_update(zo,z,D,Dvel,DU,DV)

!  Note (KK): init_input() calls do_3d_bdy_ncdf() which requires hn
   call init_input(input_dir,MinN)

   call toc(TIM_INITIALIZE)

   if (metforcing) then
      if(runtype .le. 2) then
         call do_meteo(MinN-1)
         if (met_method .eq. 2) then
            call get_meteo_data(MinN-1)
            call do_meteo(MinN-1)
         end if
#ifndef NO_BAROCLINIC
      else
         call do_meteo(MinN-1,T(:,:,kmax))
         if (met_method .eq. 2) then
            call get_meteo_data(MinN-1)
            call do_meteo(MinN-1,T(:,:,kmax))
         end if
#endif
      end if
   end if

   if (waveforcing_method .ne. NO_WAVES) then
      call do_waves(MinN-1,Dvel)
   end if

   call tic(TIM_INITIALIZE)

   call postinit_2d(runtype,timestep,hotstart,MinN)
#ifndef NO_3D
   if (runtype .gt. 1) then
      call postinit_3d(runtype,timestep,hotstart,MinN)
#ifdef _FABM_
      if (fabm_calc) call postinit_getm_fabm()
#endif
   end if
#endif

   call toc(TIM_INITIALIZE)

   if (.not. dryrun) then
      call do_output(runtype,MinN-1,timestep)
   end if

#ifdef DEBUG
   write(debug,*) 'Leaving postinit_model()'
   write(debug,*)
#endif
   return
   end subroutine postinit_model
!EOC
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: init_time - initialise the time system in getm
!
! !INTERFACE:
   subroutine init_time(MinN,MaxN,start_external,stop_external)
!
! USES:
!   use time, only: start,stop,timestep,days_in_mon
!   use time, only: jul0,secs0,juln,secsn,julianday,secondsofday,simtime
!   use time, only: String2JulSecs,time_diff,write_time_string,update_time
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
   integer, intent(out)                  :: MinN,MaxN
   character(len=19),intent(in),optional :: start_external,stop_external
!
! !DESCRIPTION:
!  Reads the namelist and makes calls to the init functions of the
!  various model components.
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
! !LOCAL VARIABLES:
   logical                   :: use_external
   logical                   :: HasRealTime=.true.
   integer                   :: timefmt=1
   integer                   :: jul1,secs1,jul2,secs2
   integer                   :: ndays,nsecs
   integer                   :: nfirst,nlast
   namelist /time/ timestep,timefmt,nlast,start,stop
!
!EOP
!-------------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'init_time() # ',Ncall
#endif
   days_in_mon(0,:) = (/31,28,31,30,31,30,31,31,30,31,30,31/)
   days_in_mon(1,:) = (/31,29,31,30,31,30,31,31,30,31,30,31/)

   use_external = ( present(start_external) .and. present(stop_external) )
!
!  Read time specific things from the namelist.
!
   LEVEL1 'init_time'
   READ(NAMLST,NML=time)

   if (use_external) then
      timefmt = 2
      start = start_external
      stop = stop_external
   end if
!
!  Calculate MaxN -> MinN is 1 if not changed by HotStart
!
   MinN = 1
   MaxN = nlast
   LEVEL2 'Time step:      ',timestep,' seconds'
   LEVEL2 'Time format:    ',timefmt
   select case (timefmt)
      case (0)
!KBK
         LEVEL2 'Hopefully we will get the time from the hot start file'
      case (1)
         HasRealTime=.false.
         LEVEL2 '# of timesteps: ',MaxN
         start='2000-01-01 00:00:00'

         call String2JulSecs(start,jul1,secs1)

         nsecs = nint(MaxN*timestep) + secs1
         ndays = nsecs/86400
         jul2  = jul1 + ndays
         secs2 = mod(nsecs,86400)
         call write_time_string(jul2,secs2,stop)

         LEVEL2 'Fake start:     ',start
         LEVEL2 'Fake stop:      ',stop
      case (2)
         LEVEL2 'Start:          ',start
         LEVEL2 'Stop:           ',stop

         call String2JulSecs(start,jul1,secs1)
         call String2JulSecs(stop,jul2,secs2)

         nsecs = time_diff(jul2,secs2,jul1,secs1)
         MaxN  = nint(nsecs/timestep)

         ndays = jul2-jul1
         if (nsecs .lt. 86400 .and. jul1 .ne. jul2) ndays = ndays-1
         nsecs = nsecs - 86400*ndays
         STDERR '        ==> ',ndays,' day(s) and ',nsecs,' seconds ==> ',MaxN,' micro time steps'
      case (3)
         LEVEL2 'Start:          ',start
         LEVEL2 '# of timesteps: ',MaxN

         call String2JulSecs(start,jul1,secs1)

         nsecs = nint(MaxN*timestep) + secs1
         ndays = nsecs/86400
         jul2  = jul1 + ndays
         secs2 = mod(nsecs,86400)

         call write_time_string(jul2,secs2,stop)
         LEVEL2 'Stop:           ',stop
      case default
         STDERR 'Fatal error: A non valid input format has been chosen'
         stop 'init_time'
   end select

   jul0  = jul1
   secs0 = secs1

   juln  = jul2
   secsn = secs2

   julianday    = jul0
   secondsofday = secs0

   simtime = timestep*(MaxN-MinN+1)

   call update_time(0)

#ifdef DEBUG
   write(debug,*) 'Leaving init_time()'
   write(debug,*)
#endif
   return
   end subroutine init_time
!EOC
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: time_step - a single time step of getm
!
! !INTERFACE:
   subroutine time_step(runtype,n)
!
! !DESCRIPTION:
!  A wrapper that calls meteo\_forcing, integrate\_2d, integrate\_3d,
!  do\_getm\_bio and output for one time step.
!
! !USES:
   use time,     only: update_time,timestep
   use domain,   only: kmax
   use meteo,    only: do_meteo,tausx,tausy,airp,fwf_method,evap,precip
   use waves,    only: do_waves,waveforcing_method,NO_WAVES
   use m2d,      only: no_2d,integrate_2d
   use variables_2d, only: fwf,fwf_int,Dvel
#ifndef NO_3D
   use m3d,      only: integrate_3d,M
#ifndef NO_BAROCLINIC
   use variables_3d, only: T
#endif
   use rivers,   only: do_rivers
#ifdef _FABM_
   use getm_fabm, only: fabm_calc,do_getm_fabm
#endif
#ifdef GETM_BIO
   use bio, only: bio_calc
   use getm_bio, only: do_getm_bio
#endif
#endif
#ifdef SPM
   use suspended_matter, only: spm_calc,do_spm
#endif
   use input,    only: do_input
   use output,   only: do_output
#ifdef TEST_NESTING
   use nesting,   only: nesting_file
#endif
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: runtype,n
!
! !REVISION HISTORY:
!
! !LOCAL VARIABLES
   logical                   :: do_3d=.false.
   integer                   :: progress=100
   character(8)              :: d_
   character(10)             :: t_
!EOP
!-----------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'time_step() # ',Ncall
#endif

      if (progress .gt. 0 .and. mod(n,progress) .eq. 0) then
         call date_and_time(date=d_,time=t_)
         LEVEL1 t_(1:2),':',t_(3:4),':',t_(5:10),' n=',n
      end if

#ifndef NO_3D
      do_3d = (runtype .ge. 2 .and. mod(n,M) .eq. 0)
#endif
      call do_input(n,do_3d)
      if(runtype .le. 2) then
         call do_meteo(n)
#ifndef NO_3D
#ifndef NO_BAROCLINIC
      else
         call do_meteo(n,T(:,:,kmax))
#endif
#endif
      end if

      if (waveforcing_method .ne. NO_WAVES) then
         call do_waves(n,Dvel)
      end if

      if (fwf_method .ge. 1) then
         fwf = evap+precip
#ifndef NO_3D
         fwf_int = fwf_int+timestep*fwf
#endif
      end if

#ifndef NO_BAROTROPIC
      if (.not. no_2d) call integrate_2d(runtype,n,tausx,tausy,airp)
#endif
#ifndef NO_3D
      call do_rivers(n,do_3d)
      if (do_3d) then
         call integrate_3d(runtype,n)
#ifdef SPM
         if (spm_calc) call do_spm()
#endif
#ifdef _FABM_
         if (fabm_calc) call do_getm_fabm(M*timestep)
#endif
#ifdef GETM_BIO
         if (bio_calc) call do_getm_bio(M*timestep)
#endif
#ifndef NO_3D
         if (fwf_method .ge. 1) then
            fwf_int = _ZERO_
         end if
#endif
      end if
#endif

#ifdef TEST_NESTING
      if (mod(n,80) .eq. 0) then
         call nesting_file(WRITING)
      end if
#endif
      call update_time(n)

      call do_output(runtype,n,timestep)
#ifdef DIAGNOSE
      call diagnose(n,MaxN,runtype)
#endif


#ifdef DEBUG
   write(debug,*) 'Leaving time_step()'
   write(debug,*)
#endif
   return
   end subroutine time_step
!EOC
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: do_transport() - transport of 2D fields
!
! !INTERFACE:
   subroutine do_transport(f)
!
! !DESCRIPTION:
!
! !USES:
   use domain      , only: imin,imax,jmin,jmax,az,H
   use m2d         , only: dtm,Uint,Vint
   use advection   , only: do_advection,HALFSPLIT,P2_PDM
   use variables_3d, only: Dn,Dun,Dvn,sseo
   use halo_zones  , only: update_2d_halo,wait_halo,H_TAG
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
!
! !INPUT/OUPUT PARAMETERS:
   REALTYPE,dimension(E2DFIELD),intent(inout) :: f
!
! !REVISION HISTORY:
!  Original Author(s): Knut Klingbeil
!
! !LOCAL VARIABLES
   REALTYPE,dimension(E2DFIELD) :: Dold
   REALTYPE,parameter           :: AH=_ZERO_
!
!EOP
!-----------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'do_transport() # ',Ncall
#endif

!  Cannot extract layer heights from grid coordinates, because these are
!  already updated.
!  For several timesteps we need to store Dires and calculate new
!  D[old|[u|v]n] based on Dires.
!  For several timesteps [U|V]int is inconsistent.
   Dold = sseo + H
   call update_2d_halo(f,f,az,imin,jmin,imax,jmax,H_TAG)
   call wait_halo(H_TAG)
   call do_advection(dtm,f,Uint,Vint,Dun,Dvn,Dold,Dn,HALFSPLIT,P2_PDM,AH,H_TAG)

#ifdef DEBUG
   write(debug,*) 'Leaving do_transport()'
   write(debug,*)
#endif
   return

   end subroutine do_transport
!EOC
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: do_transport_3d() - transport of 3D fields
!
! !INTERFACE:
   subroutine do_transport_3d(f,ws)
!
! !DESCRIPTION:
!
! !USES:
   use domain      ,only: imin,imax,jmin,jmax,kmax,az
   use m3d         ,only: dt,cnpar
   use les         ,only: les_mode,LES_BOTH,LES_TRACER
   use advection_3d,only: do_advection_3d
   use variables_3d,only: uu,vv,ww,hun,hvn,ho,hn,nuh
   use halo_zones  ,only: update_3d_halo,wait_halo,H_TAG
   use advection   ,only: HALFSPLIT
   use util        ,only: P2_PDM,NEUMANN,FLUX
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   REALTYPE,dimension(I3DFIELD),intent(in)    :: ws
!
! !INPUT/OUPUT PARAMETERS:
   REALTYPE,dimension(I3DFIELD),intent(inout) :: f
!
! !REVISION HISTORY:
!  Original Author(s): Knut Klingbeil
!
! !LOCAL VARIABLES
   REALTYPE,dimension(0:kmax) :: sour,Taur,ws1d
   integer                    :: i,j
   integer                    :: AH_method=2
   REALTYPE                   :: AH_const=1.4d-7
   REALTYPE                   :: AH_Prt=_TWO_
   REALTYPE                   :: AH_stirr_const=_ONE_
!
!EOP
!-----------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'do_transport_3d() # ',Ncall
#endif

!  see comments in do_transport()
   call update_3d_halo(f,f,az,imin,jmin,imax,jmax,kmax,H_TAG)
   call wait_halo(H_TAG)
   call do_advection_3d(dt,f,uu,vv,ww,hun,hvn,ho,hn,HALFSPLIT,P2_PDM,P2_PDM,_ZERO_,H_TAG)

   if (les_mode.eq.LES_BOTH .or. les_mode.eq.LES_TRACER) then

      call update_3d_halo(f,f,az,imin,jmin,imax,jmax,kmax,H_TAG)
      call wait_halo(H_TAG)
      call tracer_diffusion(f,hn,AH_method,AH_const,AH_Prt,AH_stirr_const)
   end if

   sour = _ZERO_
   Taur = 1.d15
   ws1d(0   ) = _ZERO_
   ws1d(kmax) = _ZERO_
   do j=jmin,jmax
      do i=imin,imax
         if (az(i,j) .eq. 1) then
!           Do advection step due to settling or rising
            ws1d(1:kmax-1) = _HALF_ * ( ws(i,j,1:kmax-1) + ws(i,j,2:kmax) )
            call adv_center(kmax,dt,hn(i,j,:),hn(i,j,:),ws1d,FLUX,FLUX, &
                            _ZERO_,_ZERO_,P2_PDM,1,f(i,j,:))
            call diff_center(kmax,dt,cnpar,1,hn(i,j,:),NEUMANN,NEUMANN, &
                             _ZERO_,_ZERO_,nuh(i,j,:),sour,sour,Taur,   &
                             f(i,j,:),f(i,j,:))
         end if
      end do
   end do

#ifdef DEBUG
   write(debug,*) 'Leaving do_transport_3d()'
   write(debug,*)
#endif
   return

   end subroutine do_transport_3d
!EOC
!-----------------------------------------------------------------------


   subroutine zero_gradient_3d_bdy(f)
   use domain, only: imin,imax,jmin,jmax,kmax,az,au,av

   REALTYPE,dimension(I3DFIELD),intent(inout) :: f
   integer :: i,j

   ! a halo update is necessary here to be fully consistent in parallel
   call update_3d_halo(f,f,az,imin,jmin,imax,jmax,kmax,H_TAG)
   call wait_halo(H_TAG)

   ! set zero-gradient in x-direction
   do j=jmin,jmax
     do i=imin,imax
       ! western boundary
       if ((au(i,j) .eq. 2) .and. (au(i-1,j) .eq. 0)) &
         f(i,j,:) = f(i+1,j,:)
       ! eastern boundary
       if ((au(i-1,j) .eq. 2) .and. (au(i,j) .eq. 0)) &
         f(i,j,:) = f(i-1,j,:)
     end do
   end do

   ! set zero-gradient in y-direction
   do j=jmin,jmax
     do i=imin,imax
       ! southern boundary
       if ((av(i,j) .eq. 2) .and. (av(i,j-1) .eq. 0)) &
         f(i,j,:) = f(i,j+1,:)
       ! northern boundary
       if ((av(i,j-1) .eq. 2) .and. (av(i,j) .eq. 0)) &
         f(i,j,:) = f(i,j-1,:)
     end do
   end do

   end subroutine zero_gradient_3d_bdy


   end module getm_driver

!-----------------------------------------------------------------------
! Copyright (C) 2013 - Hans Burchard                                   !
!-----------------------------------------------------------------------
