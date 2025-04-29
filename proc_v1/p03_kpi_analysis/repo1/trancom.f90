module trancom
implicit none
      REAL :: SLOPE(3,12)
      real :: INTERCEPT(3,12)
      real :: FUT_PEAK(3,12)
      real :: RR_SLOPE(12)
      real :: RR_INTERCEPT(12)
      
      INTEGER*2 WEEKEND_PEAK_DAYS(12)
      INTEGER (kind=4) :: PKPEAK(3,12)
      integer (kind=4) :: PKPKD(24,3,12)
end module trancom
