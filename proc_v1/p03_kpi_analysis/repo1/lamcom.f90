
module lamcom
use trancom
       use prod_arrays_dimensions
use fyregycom
implicit none
! LAMCOM time variables are in the TIM module
      integer (kind=4) :: PKPKD_LC(24,3,12)
      
end module lamcom
      module eightyeight

        implicit none
        REAL, DIMENSION(8800) :: HOURLY_RR
  
      end module eightyeight