! reference_loads.f90
!
    module reference_loads
          implicit none

          real :: system_historical_loads(24,2,12)
          real :: system_historical_peaks(24,2,12)
          real :: three_day_loads(24,3,12)
          real :: historical_energy_in(2,12)
    end module reference_loads
