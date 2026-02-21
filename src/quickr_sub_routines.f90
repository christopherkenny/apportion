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

subroutine balinski_young(n_tot, pop, apprt, pop__dim_1_, pop__dim_2_) bind(c)
  use iso_c_binding, only: c_double, c_int
  implicit none

  ! manifest start
  ! sizes
  integer(c_int), intent(in), value :: pop__dim_1_
  integer(c_int), intent(in), value :: pop__dim_2_

  ! args
  integer(c_int), intent(in) :: n_tot
  real(c_double), intent(in) :: pop(pop__dim_1_, pop__dim_2_)
  integer(c_int), intent(in out) :: apprt(pop__dim_1_, pop__dim_2_)

  ! locals
  integer(c_int) :: k
  real(c_double) :: total_pop
  integer(c_int) :: h
  real(c_double), allocatable :: v(:)
  integer(c_int) :: tmp1_
  ! manifest end

  allocate(v(pop__dim_1_))


  do k = 1, size(pop, 2)
    total_pop = sum(pop(:, k))
    do h = 1, n_tot
      v = (pop(:, k) / real(((1.0_c_double + apprt(:, k))), kind=c_double))
v(pack([(tmp1_, tmp1_=1, size((pop(:, k) < ((pop(:, k) * h) / total_pop))))], (pop(:, k) < ((pop(:, k) * h) / total_pop)))) =&
& 0.0_c_double
      apprt(maxloc(v, 1), k) = (apprt(maxloc(v, 1), k) + 1_c_int)
    end do
  end do
end subroutine

subroutine dean(n_tot, pop, out, pop__dim_1_, pop__dim_2_) bind(c)
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
  real(c_double), allocatable :: v(:)
  real(c_double), allocatable :: fc(:)
  real(c_double), allocatable :: apprt(:)
  real(c_double) :: rem
  integer(c_int) :: diff
  ! manifest end

  allocate(v(pop__dim_1_))
  allocate(fc(pop__dim_1_))
  allocate(apprt(pop__dim_1_))


  out = 0_c_int
  do k = 1, size(pop, 2)
div = (aint((sum(pop(:, k)) / real(n_tot, kind=c_double))) - merge(1.0_c_double, 0.0_c_double, ((sum(pop(:, k)) / real(n_tot,&
& kind=c_double)) < aint((sum(pop(:, k)) / real(n_tot, kind=c_double))))))
    v = (pop(:, k) / div)
fc = (((2.0_c_double * (aint(v) - merge(1.0_c_double, 0.0_c_double, (v < aint(v))))) * (aint(v) + merge(1.0_c_double,&
& 0.0_c_double, (v > aint(v))))) / (((aint(v) - merge(1.0_c_double, 0.0_c_double, (v < aint(v)))) + (aint(v) + merge(1.0_c_double,&
& 0.0_c_double, (v > aint(v)))))))
apprt = merge((aint(v) - merge(1.0_c_double, 0.0_c_double, (v < aint(v)))), (aint(v) + merge(1.0_c_double, 0.0_c_double, (v >&
& aint(v)))), (v < fc))
    rem = (n_tot - sum(apprt))
    do while ((rem /= 0.0_c_double))
      diff = merge(1_c_int, -1_c_int, (rem < 0.0_c_double))
      div = (div + diff)
      v = (pop(:, k) / div)
fc = (((2.0_c_double * (aint(v) - merge(1.0_c_double, 0.0_c_double, (v < aint(v))))) * (aint(v) + merge(1.0_c_double,&
& 0.0_c_double, (v > aint(v))))) / (((aint(v) - merge(1.0_c_double, 0.0_c_double, (v < aint(v)))) + (aint(v) + merge(1.0_c_double,&
& 0.0_c_double, (v > aint(v)))))))
apprt = merge((aint(v) - merge(1.0_c_double, 0.0_c_double, (v < aint(v)))), (aint(v) + merge(1.0_c_double, 0.0_c_double, (v >&
& aint(v)))), (v < fc))
      rem = (n_tot - sum(apprt))
    end do
    out(:, k) = apprt
  end do
end subroutine

subroutine dhondt(n_tot, pop, apprt, pop__dim_1_, pop__dim_2_) bind(c)
  use iso_c_binding, only: c_double, c_int
  implicit none

  ! manifest start
  ! sizes
  integer(c_int), intent(in), value :: pop__dim_1_
  integer(c_int), intent(in), value :: pop__dim_2_

  ! args
  integer(c_int), intent(in) :: n_tot
  real(c_double), intent(in) :: pop(pop__dim_1_, pop__dim_2_)
  integer(c_int), intent(in out) :: apprt(pop__dim_1_, pop__dim_2_)

  ! locals
  integer(c_int) :: k
  integer(c_int) :: rem
  real(c_double), allocatable :: quotient(:)
  integer(c_int) :: idx
  ! manifest end

  allocate(quotient(pop__dim_1_))


  do k = 1, size(pop, 2)
    rem = (n_tot - sum(apprt(:, k)))
    do while ((rem > 0.0_c_double))
      quotient = (pop(:, k) / real(((apprt(:, k) + 1.0_c_double)), kind=c_double))
      idx = maxloc(quotient, 1)
      apprt(idx, k) = (apprt(idx, k) + 1_c_int)
      rem = (rem - 1_c_int)
    end do
  end do
end subroutine

subroutine hamilton_vinton(n_tot, pop, out, pop__dim_1_, pop__dim_2_) bind(c)
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
  real(c_double) :: total_pop
  real(c_double) :: denom
  real(c_double), allocatable :: quotient(:)
  real(c_double), allocatable :: apprt(:)
  integer(c_int) :: tmp1_
  real(c_double) :: rem
  real(c_double), allocatable :: remainder(:)
  integer(c_int) :: idx
  ! manifest end

  allocate(quotient(pop__dim_1_))
  allocate(apprt(pop__dim_1_))
  allocate(remainder(pop__dim_1_))


  out = 0_c_int
  do k = 1, size(pop, 2)
    total_pop = sum(pop(:, k))
    denom = (total_pop / real(n_tot, kind=c_double))
    quotient = (pop(:, k) / denom)
    apprt = (aint(quotient) - merge(1.0_c_double, 0.0_c_double, (quotient < aint(quotient))))
    apprt(pack([(tmp1_, tmp1_=1, size((apprt == 0_c_int)))], (apprt == 0_c_int))) = 1_c_int
    rem = (n_tot - sum(apprt))
    remainder = (quotient - apprt)
    do while ((rem > 0.0_c_double))
      idx = maxloc(remainder, 1)
      apprt(idx) = (apprt(idx) + 1_c_int)
      remainder(idx) = 0.0_c_double
      rem = (rem - 1_c_int)
    end do
    out(:, k) = apprt
  end do
end subroutine

subroutine huntington_hill(n_tot, pop, apprt, pop__dim_1_, pop__dim_2_) bind(c)
  use iso_c_binding, only: c_double, c_int
  implicit none

  ! manifest start
  ! sizes
  integer(c_int), intent(in), value :: pop__dim_1_
  integer(c_int), intent(in), value :: pop__dim_2_

  ! args
  integer(c_int), intent(in) :: n_tot
  real(c_double), intent(in) :: pop(pop__dim_1_, pop__dim_2_)
  integer(c_int), intent(in out) :: apprt(pop__dim_1_, pop__dim_2_)

  ! locals
  integer(c_int) :: k
  logical, allocatable :: is_zero(:) ! logical
  integer(c_int) :: tmp1_
  integer(c_int) :: rem
  real(c_double), allocatable :: prios(:)
  integer(c_int) :: best
  integer(c_int) :: idx
  integer(c_int) :: j
  ! manifest end

  allocate(is_zero(pop__dim_1_))
  allocate(prios(pop__dim_1_))


  do k = 1, size(pop, 2)
    is_zero = (apprt(:, k) == 0_c_int)
    rem = (n_tot - sum(apprt(pack([(tmp1_, tmp1_=1, size((.not. is_zero)))], (.not. is_zero)), k)))
    do while ((rem > 0.0_c_double))
      prios = (pop(:, k) / real(sqrt((apprt(:, k) * ((apprt(:, k) + 1.0_c_double)))), kind=c_double))
      best = 0_c_int
      idx = 0_c_int
      do j = 1, size(prios)
        if ((.not. is_zero(j)) .and. (prios(j) > best)) then
          best = prios(j)
          idx = j
        end if
      end do
      apprt(idx, k) = (apprt(idx, k) + 1_c_int)
      rem = (rem - 1_c_int)
    end do
  end do
end subroutine

subroutine webster(n_tot, pop, apprt, pop__dim_1_, pop__dim_2_) bind(c)
  use iso_c_binding, only: c_double, c_int
  implicit none

  ! manifest start
  ! sizes
  integer(c_int), intent(in), value :: pop__dim_1_
  integer(c_int), intent(in), value :: pop__dim_2_

  ! args
  integer(c_int), intent(in) :: n_tot
  real(c_double), intent(in) :: pop(pop__dim_1_, pop__dim_2_)
  integer(c_int), intent(in out) :: apprt(pop__dim_1_, pop__dim_2_)

  ! locals
  integer(c_int) :: k
  real(c_double) :: div
  integer(c_int) :: rem
  integer(c_int) :: diff
  ! manifest end


  do k = 1, size(pop, 2)
div = (aint((sum(pop(:, k)) / real(n_tot, kind=c_double))) - merge(1.0_c_double, 0.0_c_double, ((sum(pop(:, k)) / real(n_tot,&
& kind=c_double)) < aint((sum(pop(:, k)) / real(n_tot, kind=c_double))))))
    rem = (n_tot - sum(apprt(:, k)))
    do while ((rem /= 0.0_c_double))
      diff = merge(1_c_int, -1_c_int, (rem < 0.0_c_double))
      div = (div + diff)
apprt(:, k) = (aint(((pop(:, k) / div) + 0.5_c_double)) - merge(1.0_c_double, 0.0_c_double, (((pop(:, k) / div) + 0.5_c_double) <&
& aint(((pop(:, k) / div) + 0.5_c_double)))))
      rem = (n_tot - sum(apprt(:, k)))
    end do
  end do
end subroutine
