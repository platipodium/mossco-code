program BioDriver

! this program is a driver for calculating the biological effects of microphytobenthos and macrofauna
! on the sediment flux.
! An interface (Macrofauna_interface) has been inbroduced to handle various macrofauna species within it.
! At the moment only Macoma bathica has been implemented. In the following, microphytobenthos and macrofauan
! are initialized, set, run and finalized. The biological bioturbation effect (currently only the biological
! effects on the sediment erodibility and the critical bed shear stress are only included.
! however, many other bilogical effects including biogenic structures, direct bio-resuspension and deposition
! are included as templates in macrofauna class. They can be activated, when required.

use Macrofauna_interface
use Microphytobenthos_class

implicit none

type (microphytobenthos)  :: Micro
type (BioturbationEffect) :: Total_Bioturb
real (fp)    :: tau,tau0
real (fp)    :: Erod,Erod0

! setting up microphytobenthos effects
call Micro%initialize(1,1)
call Micro%set()
call Micro%run()

! setting up macrofauna effect (currently only Macoma balthica)
call Macrofauna_init (Total_Bioturb,1,1)
call Macrofauna_set ()
call Macrofauna_run (Total_Bioturb)

! assuming an abiotic critical bed shear stress (tau) and erodibility
tau0 = 1.9
Erod0 = 0.00006


write (*,*) 'Abitotic critical tau =' , tau0, 'Abiotic Erodibility = ', Erod0
write (*,*)

write (*,*) 'Effect of Microphytobenthos of:'
write (*,*) 'Critical tau:', Micro%TauEffect(1,1)
write (*,*) ' Erodibility:',Micro%ErodibilityEffect(1,1)
write (*,*)
write (*,*)'Effect of Macoma balthica of:'
write (*,*)'Critical tau:', Total_Bioturb%TauEffect(1,1)
write (*,*)' Erodibility:',Total_Bioturb%ErodibilityEffect(1,1)



 tau = tau0 * Micro%TauEffect(1,1)
 Erod = Erod0 * Micro%ErodibilityEffect(1,1)


write (*,*) 'critical tau (microphytobenthos) =' , tau
write (*,*)
write (*,*) 'Erodibility (microphytobenthos) =',Erod
write (*,*)

 tau0= tau0* Total_Bioturb%TauEffect(1,1)
 Erod0 = Erod0 * Total_Bioturb%ErodibilityEffect(1,1)

write (*,*) 'critical tau (M.balthica) =' , tau0
write (*,*)
write (*,*) 'Erodibility (M.balthica) =',Erod0
write (*,*)

open (unit = 12, file= 'result.out',action = 'write ', status = 'replace')

write (12,*) 'critical tau (microphytobenthos) =' ,tau
write (12,*) 'Biotic erodibility=', Micro%ErodibilityEffect
write (12,*) 'Biotic Critical bed shear stress effect= ',Micro%TauEffect


tau = tau * Total_Bioturb%TauEffect(1,1)
Erod = Erod * Total_Bioturb%ErodibilityEffect(1,1)

write (*,*) 'Critical tau (M.balthica and microphytobenthos) =' ,tau,' Erodibility (M.balthica and microphytobenthos) ='&
       &       ,Erod

write (*,*)

write (12,*) 'critical tau (macrofaunau and microphytobenthos) =' ,tau
write (12,*) 'Erodibility (macrofaunau and microphytobenthos)=', Erod

! finalizing the classes
 call Micro%finalize()
 call Macrofauna_fin(Total_Bioturb)

 close (12)

 end program BioDriver
