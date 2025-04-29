!     Module extracted from common block.
!
      module GROUP_PROD
      use prod_arrays_dimensions
      implicit none
        REAL :: ANNUAL_ECONOMY_COST
        REAL :: ANNUAL_ECONOMY_REVENUE
        real :: GROUP_EMISSIONS(5,0:MAX_REPORTING_GROUPS)
        real :: GROUP_FUEL(0:MAX_REPORTING_GROUPS)
        real :: GROUP_VAROM(0:MAX_REPORTING_GROUPS)
        real :: GROUP_SALES_REVENUE(0:MAX_REPORTING_GROUPS)
        real :: GROUP_FIXED_OM(0:MAX_REPORTING_GROUPS)
        real :: ANNUAL_ECONOMY_BOUGHT, ANNUAL_ECONOMY_SOLD
        real :: GROUP_CAPACITY(0:MAX_REPORTING_GROUPS)
        real :: ANN_EL_CAPACITY(0:MAX_REPORTING_GROUPS)
        real :: ANN_CL_CAPACITY(0:MAX_REPORTING_GROUPS)
        real (kind=8) :: GROUP_ENERGY(0:MAX_REPORTING_GROUPS)
        real (kind=8) :: GROUP_MMBTUS(0:MAX_REPORTING_GROUPS)
        real (kind=8) :: GROUP_SALES(0:MAX_REPORTING_GROUPS)
      end module GROUP_PROD
