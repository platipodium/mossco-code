
module particle_driver

type passive_tracer
  double precision :: x,y,z ! position of particle
  double precision :: u,v,w ! advective velocity
  double precision :: Ax, Ay, Az ! diffusivities
end type passive_tracer

contains

subroutine nextPosition(self, dt)
  !> Following Visser [1997] and Hunter et al. [1993], the new position of a
  !> particle in the x-th dimension at the time t + 1 is given by
  !> x_{t+1} = x_t+u *\Delta t + u_self * \Delta t + \frac{\partial{A_x}}{\partial x}
  !>         * \Delta t + Z* \sqrt{􏰄2*A_x * \Delta t}

  class(passive_tracer) :: self
  double precision, intent(in) :: dt

!  self%x = self%x + self%u * dt + self%

end subroutine nextPosition

end module particle_driver
