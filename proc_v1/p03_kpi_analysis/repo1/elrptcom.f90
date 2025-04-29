      module elrptcom
! Used to be COMMON BLOCK FOR ELRPT ITEMS. Now it's a module
!
      implicit none
        integer (kind=4) :: SEASONAL_ENRG_LIMITED_CAPACITY
        integer (kind=4) :: ANNUAL_PEAK1
        integer (kind=4) :: ANNUAL_PEAK2
        integer (kind=4) :: ANNUAL_BASE1
        integer (kind=4) :: ANNUAL_BASE2
        integer (kind=4) :: MONTHLY_BEFORE_PEAK(12)
        integer (kind=4) :: MONTHLY_AFTER_PEAK(12)
        integer (kind=4) :: MONTHLY_BEFORE_BASE(12)
        integer (kind=4) :: MONTHLY_AFTER_BASE(12)
        integer (kind=4) :: ANNUAL_BEFORE_ENRG
        integer (kind=4) :: ANNUAL_AFTER_ENRG
        integer (kind=4) :: ANNUAL_DEFERENCE_ENRG
        integer (kind=4) :: MONTHLY_ENERGY_COSTS(12)
        integer (kind=4) :: MONTHLY_BEFORE_ENERGY(12)
        integer (kind=4) :: MONTHLY_AFTER_ENERGY(12)
      end module elrptcom
