!
      module prod2com
      use prod_arrays_dimensions
      implicit none
      
      type tworld
      	integer (kind=2) :: base_year_common
      	integer (kind=2) :: maximum_years_common
      end type tworld
      
	  type(tworld), save :: world

      type tdata_drive_location
      	character (len=2) :: data_drive
      end type tdata_drive_location
      
      

      type(tdata_drive_location) :: data_drive
      
      type tfoshyd
           real :: CAP_FRAC_OWN(MAX_CL_UNITS)
           real :: CAP_PLANNING_FAC(MAX_CL_UNITS)
      end type tfoshyd
      type(tfoshyd) :: foshyd
      end module prod2com
!
