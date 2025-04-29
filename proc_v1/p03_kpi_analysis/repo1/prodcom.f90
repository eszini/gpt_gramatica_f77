    module prodcom
    use prod_arrays_dimensions
    implicit none

      integer (kind=2) :: gengrp(max_cl_units)
      integer (kind=2) :: fixed_cost_escalator(max_cl_units)
      character (len=1) :: expense_assignment(max_cl_units)
      integer (kind=2) :: cl_resource_id(max_cl_units)
      integer (kind=2) :: fuel_supply_id(max_cl_units)
  
      real :: input_mw(2,max_cl_units)
      real :: mw(2,max_cl_units)
      real :: vcpmwh(max_cl_units)
      real :: coeff(3,max_cl_units)
      real :: fueladj(max_cl_units)
      real :: sbtuct(max_cl_units)
      real :: pbtuct(max_cl_units)
      real :: eavail(max_cl_units)
      real :: mnrate(max_cl_units,12)
    real :: dispadj_prod(max_cl_units)
      real :: hr_factor(max_cl_units)
      real :: efor(max_cl_units)
      real :: fixed_cost(max_cl_units)
      real :: dispadj2(max_cl_units)
      real :: fuelmx(max_cl_units)
      real :: annual_cl_fixed_cost(max_cl_units)
      real :: dispatch_mult(max_cl_units)
      real :: excess_energy_sales(max_cl_units)
      real :: fuel_adder_adjustment(max_cl_units)
     
      real :: p_so2(max_cl_units)
      real :: p_nox(max_cl_units)
      real :: p_particulates(max_cl_units)
      real :: p_emis_oth2(max_cl_units)
      real :: p_emis_oth3(max_cl_units)
      real :: p_nox_bk2(max_cl_units)
      real :: emiss_fuel_cost(max_cl_units)
      real :: emiss_blending_rate(max_cl_units)
      
      integer (kind=2) :: sec_fuel_emiss_ptr(max_cl_units)
      integer (kind=2) :: emiss_fuel_escal(max_cl_units)
      integer (kind=2) :: emiss_fuel_emiss_ptr(max_cl_units)
      integer (kind=2) :: prim_fuel_emiss_ptr(max_cl_units)
      integer (kind=2) :: annual_cl_fixed_cost_esc(max_cl_units)
      integer (kind=2) :: monthly_capacity_pointer(max_cl_units)
  
      real :: import_cap
      real :: export_cap
      real :: cl_pool_frac_own(max_cl_units)
  
      character (len=5)  :: special_hydro_unit_id(max_el_units)
      character (len=20) :: unitnm(max_cl_units)
      character (len=20) :: el_unit_name(max_el_units)
  
      character (len=1) ::  ldtype(max_cl_units)
      character (len=1) :: expense_collection(max_cl_units)
      character (len=1) :: economy_trans_type(max_cl_units)
      character (len=1) :: prim_fuel_type(max_cl_units)
      character (len=1) :: sec_fuel_type(max_cl_units)
      character (len=1) :: emiss_fuel_type(max_cl_units)
  
      logical (kind=1) :: phase_i_unit(max_cl_units)
      logical (kind=1) :: cl_cap_area_linked(max_cl_units)
  
      integer (kind=2) :: elcpts
      integer (kind=2) :: round
      integer (kind=2) :: mxunit
      integer (kind=2), save :: date1=0,date2=0,nblok=0,nblock=0
      integer (kind=4) :: syscap
  
      integer (kind=2) :: pfescr(max_cl_units)
      integer (kind=2) :: sfescr(max_cl_units)
      integer (kind=2) :: omescr(max_cl_units)
      integer (kind=2) :: disp_adder_escr(max_cl_units)
  
      character (len=1) :: hytype(max_el_units)
      character (len=1) :: hydro_expense_collection(max_el_units)
      character (len=1) :: hydro_expense_assignment(max_el_units)
      character (len=1) :: el_fuel_type(max_el_units)
      real :: in_capmo(12,max_el_units)
      real :: pscap(max_el_units)
  
      integer (kind=2) :: vomesr(max_el_units)
      integer (kind=2) :: hydro_fixed_cost_escalator(max_el_units)
      integer (kind=2) :: el_group(max_el_units)
      integer (kind=2) :: hydro_annual_fixed_cost_esc(max_el_units)
  
      real :: pseff(max_el_units)
      real :: varom(max_el_units)
      real :: planning_factor_hydro(max_el_units)
      real :: enrgmo(12,max_el_units)
      real :: pcown(max_el_units)
      real :: hydro_fixed_cost(max_el_units)
      real :: el_pool_frac_own(max_el_units)
      real :: el_so2_rate(max_el_units)
      real :: hydro_annual_fixed_cost(max_el_units)
      
      ! John added these during the merge - debugmod
      real :: DISPADJ(MAX_CL_UNITS) ! From prodcom.mon
      character (len=1) :: expense_assignment_prod(max_cl_units)
  
!  secondary emissions rates and
!  emissions blending fuel data
!
      real :: s_so2(max_cl_units)
      real :: s_nox_bk1(max_cl_units)
      real :: s_nox_bk2(max_cl_units)
      real :: s_co2(max_cl_units)
      real :: s_other2(max_cl_units)
      real :: s_other3(max_cl_units)
      real :: emiss_fuel_so2(max_cl_units)
      real :: emiss_fuel_nox_bk1(max_cl_units)
      real :: emiss_fuel_nox_bk2(max_cl_units)
      real :: emiss_fuel_co2(max_cl_units)
      real :: emiss_fuel_other2(max_cl_units)
      real :: emiss_fuel_other3(max_cl_units)

    end module prodcom
!
!
