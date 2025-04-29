! Module was created to fix issues with max_fo_per_month and
! unsized arrays passed illegally to ENTRY routines.
module  clm_objt_pay
implicit none
integer (kind=2) :: MAX_FO_PER_MONTH=1
integer (kind=2), save, allocatable :: BEG_HR_FO(:),END_HR_FO(:)

end module clm_objt_pay
