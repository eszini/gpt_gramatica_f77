! cls_load.f90 - comes from LAMCOM.MON
!
module cls_load
  use prod_arrays_dimensions
  implicit none

  real (kind=4) :: forecast_customers(max_load_classes)
  real (kind=4) :: historical_loads(24,2,12,max_load_classes)
  real (kind=4) :: coincident_historical_peak(2,12,max_load_classes)
  real (kind=4) :: forecast_energy(2,12,max_load_classes)
  real (kind=4) :: forecast_coincident_peak(2,12,max_load_classes)
  real (kind=4) :: class_losses(max_load_classes)
  real (kind=4) :: class_coin_factor(max_load_classes)
  real (kind=4) :: class_reserve_margin(max_load_classes)
  real (kind=4) :: class_peak_losses(max_load_classes)
end module cls_load
!
