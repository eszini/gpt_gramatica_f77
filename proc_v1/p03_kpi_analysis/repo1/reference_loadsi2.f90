! reference_loadsi2.f90
!
module reference_loadsi2
  implicit none

  integer(kind=2) :: historical_peak_hour(2,12)
  integer(kind=2) :: system_load_order(24,2,12)
  integer(kind=2) :: historical_day_count(2,12)
  integer(kind=2) :: holidays(12)
end module reference_loadsi2
!
