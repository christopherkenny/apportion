subroutine adams(n_tot, pop, out, pop__dim_1_, pop__dim_2_) bind(c)
  use iso_c_binding, only: c_double, c_int
  implicit none

  ! manifest start
  ! sizes
  integer(c_int), intent(in), value :: pop__dim_1_
  integer(c_int), intent(in), value :: pop__dim_2_

  ! args
  integer(c_int), intent(in) :: n_tot
  real(c_double), intent(in) :: pop(pop__dim_1_, pop__dim_2_)
  integer(c_int), intent(out) :: out(pop__dim_1_, pop__dim_2_)

  ! locals
  integer(c_int) :: k
  real(c_double) :: div
  real(c_double), allocatable :: apprt(:)
  real(c_double) :: rem
  integer(c_int) :: diff
  ! manifest end

  allocate(apprt(pop__dim_1_))


  out = 0_c_int
  do k = 1, size(pop, 2)
div = (aint((sum(pop(:, k)) / real(n_tot, kind=c_double))) - merge(1.0_c_double, 0.0_c_double, ((sum(pop(:, k)) / real(n_tot,&
& kind=c_double)) < aint((sum(pop(:, k)) / real(n_tot, kind=c_double))))))
    apprt = (aint((pop(:, k) / div)) + merge(1.0_c_double, 0.0_c_double, ((pop(:, k) / div) > aint((pop(:, k) / div)))))
    rem = (n_tot - sum(apprt))
    do while ((rem /= 0.0_c_double))
      diff = merge(1_c_int, -1_c_int, (rem < 0_c_int))
      div = (div + diff)
      apprt = (aint((pop(:, k) / div)) + merge(1.0_c_double, 0.0_c_double, ((pop(:, k) / div) > aint((pop(:, k) / div)))))
      rem = (n_tot - sum(apprt))
    end do
    out(:, k) = apprt
  end do
end subroutine
