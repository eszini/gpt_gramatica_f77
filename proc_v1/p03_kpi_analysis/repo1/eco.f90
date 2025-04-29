module eco
use hesi_gendecs
implicit none
    real (kind=4) :: eco_puch_cost_from(max_cl_units)
    real (kind=4) :: eco_puch_enrg_from(max_cl_units)
    real (kind=4) :: eco_sales_enrg_from(max_cl_units)
    real (kind=4) :: eco_sales_rev_from(max_cl_units)
    real (kind=4) :: emission_rates(6,3,max_cl_units)
    real (kind=4) :: mon_eco_puch_cost_from(max_cl_units)
    real (kind=4) :: mon_eco_puch_enrg_from(max_cl_units)
    real (kind=4) :: mon_eco_sales_enrg_from(max_cl_units)
    real (kind=4) :: mon_eco_sales_rev_from(max_cl_units)
end module eco