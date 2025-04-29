! Apologies - created this module to troubleshoot a build issue.
! The fact that you're reading this, means the activity was successful.
! TODO:  Determine proper placement of module members.
module csvdat
use prod_arrays_dimensions
implicit none
logical (kind=1) :: csv_by_unit(0:avail_data_years)
end module csvdat