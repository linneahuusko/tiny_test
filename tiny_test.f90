! Linnea Huusko 27/2-2025
module user
  use neko
  implicit none

  real(kind=rp) :: u_geo

contains

  ! Register user defined functions (see user_intf.f90)
  subroutine user_setup(u)
    type(user_t), intent(inout) :: u

    u_geo = 10

    u%initial_conditions => user_ic
    ! u%scalar_user_ic => scalar_ic
  end subroutine user_setup

  ! User defined initial condition
  subroutine user_ic(scheme_name, fields)
    character(len=*), intent(in) :: scheme_name
    type(field_list_t), intent(inout) :: fields
    type(field_t), pointer :: u, v, w, s
    type(field_t), pointer :: p
    type(dofmap_t), pointer :: dof
    integer :: i
    real(kind=rp) :: x, y, z
    real(kind=rp) :: eps, kx, ky, lx, ly, alpha, beta, gamma, delta, PI

    PI = (4.*atan(1.))

    kx  = 5
    ky  = 7
    lx  = 40
    ly  = 40
    eps = 0.4


    alpha = kx * PI / 1500
    beta  = ky * PI / 1500
    gamma = lx * PI / 1500
    delta = ly * PI / 1500
    if (scheme_name .eq. 'fluid') then
        u => fields%get("u")
        v => fields%get("v")
        w => fields%get("w")
      do i = 1, u%dof%size()
        u%x(i,1,1,1) = u_geo
        v%x(i,1,1,1) = 0
        w%x(i,1,1,1) = 0
        x = u%dof%x(i,1,1,1)
        y = u%dof%y(i,1,1,1)
        z = u%dof%z(i,1,1,1)
        if (z .le. 50) then ! Small perturbation to help get turbulence started
          u%x(i,1,1,1) = u%x(i,1,1,1) + eps*(sin(alpha*x)*sin(beta*y)) &
                      + eps*(sin(gamma*x)*sin(delta*y))
          v%x(i,1,1,1) = w%x(i,1,1,1) + eps*(sin(alpha*x)*sin(beta*y)) &
          + eps*(sin(gamma*x)*sin(delta*y))
          w%x(i,1,1,1) = v%x(i,1,1,1) - eps*(alpha * cos(alpha*x)*sin(beta*y)) &
          - eps*(gamma * cos(gamma*x)*sin(delta*y))
        endif
      end do
    else !scalar
      s => fields%get(scheme_name)
      if (scheme_name .eq. 'temperature') then
        do i = 1, s%dof%size()
          z = s%dof%z(i,1,1,1)
          if (z.le.500) then
            s%x(i,1,1,1) = 300
          elseif (z.le.550) then
            s%x(i,1,1,1) = 300 + (z - 500)*0.08
          else
            s%x(i,1,1,1) = 303.96 + (z - 550)*0.003
          endif
        end do
      endif
    endif
  end subroutine user_ic

end module user
