module dsf
! from dsfmncom.mon F77 include file.
use prod_arrays_dimensions
implicit none

real :: &
 base_prog_cap(max_dsm_financial_records), & 
 ongo_prog_cap(max_dsm_financial_records), & 
 base_prog_exp(max_dsm_financial_records), & 
 ongo_prog_exp(max_dsm_financial_records), & 
 base_cust_exp(max_dsm_financial_records), & 
 ongo_cust_exp(max_dsm_financial_records), & 
 ongo_new_cust_exp(max_dsm_financial_records), & 
 ongo_kwh_exp(max_dsm_financial_records), & 
 ongo_kw_exp(max_dsm_financial_records), & 
 base_cust_cap(max_dsm_financial_records), & 
 ongo_cust_cap(max_dsm_financial_records), & 
 ongo_new_cust_cap(max_dsm_financial_records), & 
 ongo_kwh_cap(max_dsm_financial_records), & 
 ongo_kw_cap(max_dsm_financial_records), & 
 boklf(max_dsm_financial_records), & 
 taxlf(max_dsm_financial_records), & 
 construction_period(max_dsm_financial_records), & 
 adrlife(max_dsm_financial_records), & 
 dbrate(max_dsm_financial_records), & 
 regulatory_allocator(max_dsm_financial_records), & 
 taxexp(max_dsm_financial_records), & 
 rebate_cust_exp(max_dsm_financial_records), & 
 rebate_new_cust_exp(max_dsm_financial_records), & 
 rebate_cust_cap(max_dsm_financial_records), & 
 rebate_new_cust_cap(max_dsm_financial_records), & 
 participant_cust_cost(max_dsm_financial_records), & 
 participant_new_cust_cost(max_dsm_financial_records), & 
 util_non_elec_cust_cost(max_dsm_financial_records), & 
 util_non_elec_new_cust_cost(max_dsm_financial_records), & 
 third_party_cust_cost(max_dsm_financial_records), & 
 third_party_new_cust_cost(max_dsm_financial_records), & 
 oth_participant_cust_cost(max_dsm_financial_records), & 
 oth_participant_new_cust_cost(max_dsm_financial_records)
     
end module dsf
