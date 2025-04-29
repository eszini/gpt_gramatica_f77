module forecast_decs
implicit none
        real, allocatable :: fc_forecast_coincident_peak(:,:,:)
        integer (kind=2) :: fc_class=0
        logical (kind=1), allocatable :: which_classes_in_area(:,:)
        
        type t_ns_forecast_decs
        real :: dsm_peak
        real, allocatable :: energy_class_split(:,:)
        logical (kind=1) :: load_factor_adjusted
        real :: load_factor=0.
        real :: excess_energy=0.
        logical (kind=1) :: not_srp=.true.
        real :: deficient_peak
        real :: PEAK_SPLIT(12)
        logical (kind=1) :: peak_is_on_the_weekend(12)
        real :: energy_split(12)
        real :: MO_ENERGY(2)
        real :: SYSTEM_FORECAST_ENERGY(2,12)
        end type t_ns_forecast_decs
        
        type(t_ns_forecast_decs), save :: ns_forecast_decs
end module forecast_decs