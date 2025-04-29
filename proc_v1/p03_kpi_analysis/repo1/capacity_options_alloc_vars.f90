!
!	Change log
!
!	Changed USE capacity_options_fixed_variables
!	to      USE capacity_options_fixed_vars
!	
!
!
!
!
!
!

!--------
  MODULE CAPACITY_OPTIONS_ALLOC_VARS
  use capacity_options_fixed_vars
  use esrn_decs
  implicit none
  INTEGER (KIND=2) :: FOR_ALL_OPTIONS=0
  CHARACTER (LEN=20), ALLOCATABLE ::  UNIT_NAMES(:)
  
  type tc_resources
    CHARACTER (LEN=10), ALLOCATABLE :: RESOURCE_TYPE(:)
  end type tc_resources
  type(tc_resources), save :: ns_tc_resources
  
  CHARACTER (LEN=2), ALLOCATABLE :: FILE_SOURCE(:)
  CHARACTER (LEN=1), ALLOCATABLE :: YES_OR_NO_FILLER(:), &
                                    LOADING_TYPE(:)
  REAL (KIND=4), ALLOCATABLE :: UNIT_CAP(:), &
                                MAX_UNIT_CAPACITY(:), &
!                              
                                CONSTRUCTION_COSTS(:,:), &
                                CCR_FOR_SCREENING(:), &
                                MRX_RISK_ADJUSTMENT(:), &
                                COMPOUND_ESC(:), &
                                THRESHOLD_CAPACITY(:,:), &
                                RENEWABLE_ENERGY_PERCENT(:)
  INTEGER (KIND=2), ALLOCATABLE :: FIRST_YEAR_AVAILABLE(:), &
                                   RESOURCE_ID_NUM(:), &
                                   UNIT_IS_IDENTICAL_TO(:), &
                                   RESOURCE_GROUP_NUM(:), &
                                   LAST_YEAR_AVAILABLE(:)
  INTEGER (KIND=2), ALLOCATABLE :: OPERATION_LIFE(:), &
                                   LEAD_TIME(:), &
                                   ANNUAL_UNITS(:), &
                                   cap_PRODUCTION_DATA_POINTER(:), &
                                   INVESTMENT_DATA_POINTER(:), &
                                   CUMULATIVE_UNITS(:), &
                                   ESCALATION_VECTOR(:), &
                                   DEPEND_UNIT_NO(:), &
                                   MUT_EXC_UNIT(:), &
                                   POINTER_TO_DEPENDENT_UNIT(:), &
                                   DEPENDENT(:), &
                                   INDEPENDENT(:), &
                                   DEPEND_RESOURCE_ID_2_I(:), &
                                   INDEPENDENT_UNIT_STATUS(:), &
                                   INDPENDENT_UNIT(:), &
                                   BASE_OPTION_LIST(:), &
                                   CYCL_OPTION_LIST(:), &
                                   PEAK_OPTION_LIST(:), &
                                   LOAD_OPTION_LIST(:), &
                                   FILL_OPTION_LIST(:), &
                                   ACTIVE_OPTION_LIST(:), &
                                   HARD_WIRED_LIST(:), &
                                   HARD_WIRED_ON_LINE_YEAR(:), &
                                   SAVED_CUMULATIVE_UNITS(:), &
                                   GRX_SAVED_CUMULATIVE_UNITS(:), &
                                   ANNUAL_UNITS_LEFT(:), &
                                   TOTAL_UNITS_ADDED(:), &
                                   ADDITIONAL_INVESTMENT_POINTER(:), &
                                   OVER_CAPACITY_TABLE(:), &
                                   UNDER_CAPACITY_TABLE(:), &
                                   PRIMARY_ASSET_CLASS(:), &
                                   PRIMARY_ALLOCATION_VECTOR(:), &
                                   ADDITIONAL_ASSET_CLASS(:), &
                                   ADDITIONAL_ALLOCATION_VECTOR(:), &
                                   SAVE_NO_CL_ADDITIONS(:), &
                                   SAVE_NO_EL_ADDITIONS(:), &
                                   ADDED_LM_PROGRAMS(:), &
                                   ADDED_CONTRACT_PROGRAMS(:), &
                                   GRX_RESOURCE_LINK_ID(:)
  LOGICAL (KIND=1), ALLOCATABLE :: CONTROLS_DEPEND(:), &
                                   IS_A_HARD_WIRED_UNIT(:), &
                                   HAS_BEEN_SELECTED(:), &
                                   HARD_WIRED_UNIT_AVAILABLE(:)
  REAL (KIND=4), ALLOCATABLE :: LOWEST_CONE_COST_BY_CM(:), &
                                EAS_REVENUE_OFFSET_BY_CM(:)
  CHARACTER (LEN=40), ALLOCATABLE :: OVN_CAPITAL_COST_STOCASTIC(:)
  

contains 
    function UPDATE_SCREEN_CAP_COSTS(R_YEAR)
    use capacity_options_fixed_vars
    use debugtrace
    use cn_objt_shared
    implicit none
    integer :: UPDATE_SCREEN_CAP_COSTS, I
    integer :: file_trace_uscc=0
    integer (kind=2) :: r_year ! unused
    
       if(file_trace_uscc==0) then
        file_trace_uscc=open_trace("update_screen_cap_costs.trace", &
            rq_uscc)
        call write_trace_int2s(file_trace_uscc, "Esc. vector", &
            ESCALATION_VECTOR)
            
       end if
        call write_trace_int2(file_trace_uscc, "TOTAL_ALL_OPTIONS", &
            tot_all_options_cno)
            
        DO I = 1 , tot_all_options_cno
            
           call write_trace_int2(file_trace_uscc, "ESCALATION value", &
            ESCALATION_VECTOR(I))
           call write_trace_real4(file_trace_uscc, "< COMPOUND_ESC", &
            COMPOUND_ESC(I))
            COMPOUND_ESC(I) = get_escalated_value(COMPOUND_ESC(I), &
                ESCALATION_VECTOR(I))
           call write_trace_real4(file_trace_uscc, "> COMPOUND_ESC", &
            COMPOUND_ESC(I))
            
        ENDDO
            UPDATE_SCREEN_CAP_COSTS = tot_all_options_cno   
        RETURN
    end function UPDATE_SCREEN_CAP_COSTS
        
  END MODULE CAPACITY_OPTIONS_ALLOC_VARS
