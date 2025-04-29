      module prod_arrays_dimensions
      use hesi_gendecs
	  implicit none
      
	  INTEGER (kind=2), parameter ::  MX_RES_P_YEAR = MAX_CL_UNITS
	  ! From enrg_helper AND namescom
      INTEGER (KIND=2), PARAMETER :: adjustment_clause = 2 
      
      integer (kind=2), parameter :: max_retirement_units = max_cl_units
    integer (kind=4), parameter :: max_dispatch_blocks = 2*max_cl_units

    integer (kind=2), parameter :: max_contracts=150
    integer (kind=2), parameter :: max_reporting_groups=15
    integer (kind=2), save :: max_simulation_years = 30
    integer (kind=2), parameter :: max_load_classes = 6
    integer (kind=2), parameter :: maximum_fuel_types=200
    integer (kind=2), parameter :: convolution_points=1000
    integer (kind=2), parameter :: load_curve_points=1000
    integer (kind=2), parameter :: max_dsm_devices= 100
    integer (kind=2), parameter :: max_dsm_financial_records= 100
    integer (kind=2), parameter :: max_dsm_response_curves=100
    integer (kind=2), parameter :: max_emission_report_groups=10
    integer (kind=2), parameter :: max_emission_dispatch_groups=6
    integer (kind=2), parameter :: number_of_emission_types = 5
   integer (kind=2), parameter :: max_state_lookup_ids = 400 
    integer (kind=2), parameter :: total_emission_groups = &
        max_emission_report_groups+max_emission_dispatch_groups
        
! from prod_arrays_dimensions module, no longer in codebase:
         integer (kind = 2) :: system_class_num = 7
         integer (kind=2), parameter :: avail_data_years = 30,&
                                     max_planning_years=100,&
                                   max_financial_simulation_years=101
         logical (kind=1) :: testing_plan=.false.
         logical (kind=1) :: monthly_midas_active=.false.
         
      end module prod_arrays_dimensions

