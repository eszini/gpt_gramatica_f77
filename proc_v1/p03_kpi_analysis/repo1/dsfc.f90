module dsfc
! from dsmfncom.mon f77 include file
use dsf1
implicit none

character(len=30) :: desc(max_dsm_financial_records)
character(len=4) :: depmet(max_dsm_financial_records)
character(len=1) :: reg_treat(max_dsm_financial_records)
character(len=1) :: collect(max_dsm_financial_records)
              
end module dsfc