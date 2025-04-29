!      Last change: msg 9/11/2021 10:07:35 AM


      FUNCTION MIDAS_NORMAL_SIMULATION(SCENAME)

!
!
!  VARIABLES TO SIZE THE MODEL
!
      use globecom
      use abb_capmarketrptdata
      use cl_data
      use cla_objt_arrays
      use cls_load
      use co2_cap_n_trade
      use contracts_data
      use dr_booth_modules
      use dreptcom
      use dsex_obj_interfaces
      use dsmfrcom
      use envircom
      use financial_switches_common
      use forecast
      use foshydi2com
      use grx_planning_routines
      use grxmodules
      use irec_endpoint_control
      use lamcom
      use msgoutpt
      use prod2com
      use prod_arrays_dimensions
      use prodcom
      use tim
      use rptreccontrol
      use sizecom
      use spindriftlib
      use program_state
      use nunits_shared
      use ovls_data
      use debugtrace

      implicit none
!
!  LP COAL MODEL
!
      integer (kind=2) :: imm_year
      integer :: time_mns
      integer :: file_trace_msgnorm=0
      logical (kind=1) :: COAL_MODEL_ONLY
      LOGICAL (KIND=4) :: READ_BASIN_SUPPLY_SO2_DATA, &
                          READ_PLANT_CONT_DEMAND_LNK_DATA, &
                          READ_GEN_PLANT_DEMAND_LINK_DATA, &
                          READ_COAL_ESCALATION_FILE, &
                          CoalExistingPlant,CoalGenericPlant
      REAL (KIND=4) :: TestFactor
!
!  GRX VARIABLES
!
      INTEGER (KIND=2) :: RETIRED_UNITS, &
                          RESET_ANNUAL_UNITS_LEFT,I
      REAL (KIND=4) :: INIT_ANNUAL_GROSS_MARGIN,VOID_REAL
      LOGICAL (KIND=1) :: VOID_LOGICAL,ZERO_TOTAL_EMIS_VARS, &
                          GET_CO2_RETIREMENTS_LOGIC, &
                          CO2_RETROFIT_LOGIC_ACTIVE, &
                          ADJUST_CO2_RETRO_PLAN_CAP, &
                          GRX_SAVE_DERIV_PLAN_VALUES, &
                          GRX_SAVE_RPS_PROGRAM_VALUES
      INTEGER (KIND=2) :: MAX_GRX_ITERs
      LOGICAL (KIND=4) :: FILE_IS_OPEN,OneMoreIter
!  END GRX VARIBLES
      LOGICAL*1 ASSET_ANALYST_ONLY,SET_ASSET_ANALYST_ONLY_SWITCH, &
                TRANSACT_ANALYST_ONLY,INIT_FILE_FOUND, &
                INIT_FILE_IS_ACTIVE, &
                gas_lp_active,LP_GAS_MODEL, & ! Externals
                TEMP_L, &
                GAS_PRICING_ROUTINE, & ! External
                WRITE_LNG_TO_DAT, &  ! External
                MULTI_YEAR_ENERGY_PRODUCTS ! External
      INTEGER*2 EL_UNITS_READ,CL_UNITS_READ,CONTRACTS_READ
      INTEGER*2 VOID_INT2,YEAR_LOOP,SET_ESCALATION_VECTOR_STRUCTURE
      INTEGER*2 START_YEAR,MIDAS_NORMAL_SIMULATION
      INTEGER*2 SETUP_DAY_OF_WEEK_4,DAYS_IN_EACH_MONTH,MONTH, &
                GET_PRB_SEQUENCE_NUMBER, &
                R_MONTH
      INTEGER IOS
      LOGICAL (KIND=1) :: SP_CAPEX_DISPLAY_ACTIVE
      LOGICAL*1 CL_RESET_PRICES, &
                EL_RESET_PRICES, &
                CONTRACT_RESET_OPTIONS, &
                DSM_RESET_OPTIONS, &
                MARKET_GROUPS, &
                READ_MARKET_GROUPS_DATA, &
                WVPA,IPALCO, &
                QUICKSILVER, &
                SIMSTATUS_REPORT, &
                SIMSTATUS_ACTIVE, &
                UPDATE_STATUS_REPORT, &
                GAS_MODEL_ONLY,YES_GAS_MODEL_ONLY, &
                RESET_INTO_EXTENSION_PERIOD
      logical (kind=1) :: wvpa_imm
      INTEGER*2 RESET_EL_UNITS, &
                RESET_CL_UNITS
      CHARACTER (LEN=10) :: START_SIMTIME,START_SIMDATE,START_SIMZONE, &
                            SIMTIME,SIMDATE,SIMZONE
      INTEGER :: START_SIMDT(10),SIMDT(10),SIM_REC
      SAVE      SIMSTATUS_REPORT, &
                START_SIMTIME,START_SIMDATE,START_SIMZONE, &
                SIMTIME,SIMDATE,SIMZONE, &
                START_SIMDT,SIMDT,SIM_REC

!
!  PALO ALTO ITEMS
!
      CHARACTER*1 PALO_ALTO
      PARAMETER(PALO_ALTO='P')
!
      LOGICAL*1   RUN_FUTURE_ASSETS,CAPACITY_WAS_ADDED, &
                  EMISSIONS_CREDITS, &
                  ENVIR_DATA_READ, &
                  ANNUAL_ENERGY_PRODUCTS, &
                  ANNUAL_TRANSACTION_LOADS, &
                  ANNUAL_GAS_DEMAND, &
                  CALC_ANNUAL_SUPPLY_CURVE_PRICE, &
                  WEEKLY_HYDRO_ACTIVE, &
                  MANAGE_WEEKLY_HYDRO_FORECASTS, &
                  MANAGE_RPS_REQUIREMENTS, &
                  MANAGE_RPS_PROGRAM_FORECASTS, &
                  MANAGE_CO2_PARAM_FORECASTS, &
                  RESET_WARNINGS
      CHARACTER*128 SIMLINE
      CHARACTER*256 OUTPUT_DIRECTORY
      INTEGER*2 YEARS_TO_RUN, &
                LAST_YEAR, &
                LAST_LOOP_YEAR, &
                ENDPOINTS_RUN, &
                TEMP_I2, &
                STARTING_END_POINT
      INTEGER*4 REC_LENGHT
      INTEGER*2 I2_REC_LENGHT
      INTEGER*4 SYSTEM_CAPACITY, &
                PROCOST
      REAL*4 TEMP_R,UPDATE_HH_PRICE
      REAL GET_ENERGY_EXCHANGE_ADJUSTMENT
      REAL*8 GET_TOTAL_SALES_REVENUE
!      VARIABLES FOR COST OF SERVICE
      LOGICAL*1 FIRST_END_POINT, &
                CAPACITY_PLANNING_ACTIVE, &
                CAPACITY_PLANNING_IS_ACTIVE, &
                OBJECTIVE_FUNCTION_ACTIVE, &
                READ_PRICE_FEEDBACK_DATA, &
                READ_SCENARIO_MAKER_DATA, &
                READ_LH_GLOBAL_FILE, &
                READ_REGIONAL_OUTAGES_DATA, &
                READ_REGIONAL_PARAMS_DATA, &
                MANAGE_TRAN_EXP_FORECASTS, &
                MANAGE_GAS_NODE_FORECASTS, &
                MANAGE_GAS_SUPPLY_FORECASTS, &
                MANAGE_GAS_DEMAND_FORECASTS, &
                MANAGE_GAS_LINK_FORECASTS, &
                MANAGE_GAS_STORAGE_FORECASTS, &
                FIRST_PASS,FIRST_PASS_IS_ACTIVE, &
                PRICE_PLANNING_FIRST_PASS, &
                UPDATE_PRODUCTION_PARAMETERS, &
                READ_ENERGY_PRODUCTS_DATA, &
                READ_TRANS_CONSTRAINT_DATA, &
                READ_TRANS_PATH_DATA, &
                READ_TRANS_GROUPS_DATA
      LOGICAL*4 PRODP_FILE_EXISTS,PRODUCTION_PARAMETERS_OBJECT, &
                FILE_OPENED, &
                ERROR_FILE_OPEN, &
                ERROR_FILE_EXISTS
      SAVE FIRST_PASS

!
!      TYPE DECLARATION FOR /SCREEN/
!
      LOGICAL*4 ESCAPE
      LOGICAL*1 LM_FIN_FILE_EXISTS
!      LOGICAL*4 REPORT_FILE_EXISTS
      LOGICAL*1 RUN_LOAD,RUN_PROCOST, &
                RUN_FINANCE, &
                RUN_FINASSET
!  3/1/93. GAT. HOURLY REFERENCE LOAD SHAPES
!  5/5/93. GAT. SCREENING INFORMATION.
      INTEGER*4 END_POINT_STARTING_RECORD
!      LOGICAL*1 CONVERT_ALL
      LOGICAL*1 FORECAST_REPORT
!  FILE NAMES
      CHARACTER (LEN=256) :: FILE_NAME,STATUS_FILE_NAME, &
                             CURRENT_DIRECTORY,BIP_FILE_NAME
!  DECLARATION SECTION FOR LAM PROGRAMS
!
!  COMMON BLOCKS
!


!  MONTHLY FUEL PRICE ITEMS ADDED 9/22/92
!
      LOGICAL*1 FUEL_PRICE_DATA_AVAILABLE
      

      INTEGER*4 ERROR,DEALLOCATE_FUEL_INVENTORY_ID
      integer, save :: timeshere=0
!  PARAMETER SECTION
!   COMMON BLOCK FOR ERROR MESSAGES
      LOGICAL*1 MESSAGE,DEBUG_ON
      INTEGER*2 INVERSE_VIDEO_COM
      LOGICAL*1 USER_TERMINATED
     
      COMMON /ERRORS/ INVERSE_VIDEO_COM,MESSAGE,DEBUG_ON,USER_TERMINATED
!   BEGINNING OF THE COMMON BLOCK SECTION FOR THE PRODUCTION MODULE

     

!
      
!
!   END COMMON BLOCK SECTION FOR THE PRODUCTION MODULES
!
!  NEW VARIABLES ADDED BY M.S.G. JULY 29, 1993 -
!
      LOGICAL*1 RUN_SPECS_OBJECT, &
                DETAILED_REPORTS_OBJECT, &
                CLOSE_RUN_SPECS_FILE, &
                CLOSE_DETAILED_REPORTS_FILE, &
                RETURN_RUN_SPEC_SWITCHES, &
                RETURN_DETAILED_REPORTS, &
                SET_END_OF_STUDY_FLAG_FALSE, &
                WVPA_INIT_TRACKER_DATABASE, &
                READ_ESCALATION_FILE
      LOGICAL*1 VOID_LOG1,READ_DAY_TYPE_DATA, &
                READ_USER_DAY_DATA,NERC_REGION_BASED_FORECAST
      LOGICAL*1 END_OF_STUDY_FLAG, &
                END_OF_PLAN_FLAG, &
                RATIOS_FOUND
      logical (kind=1) :: read_icap_file ! External
      INTEGER*2 RUN_YEARS
      INTEGER*2 LAST_REFERENCE_LOADS/9999/,HOURLY_LOAD_IN
      CHARACTER (len=5) :: SCENAME, &
            LD_FIN_FIL
      CHARACTER*2 CAPACITY_PLANNING_METHOD,GREEN_MRX_METHOD, &
                  ACTIVE_CAPACITY_PLANNING_METHOD
      LOGICAL*1 DEALLOCATE_TRANS_OUTAGE_DATA, &
                DEALLOCATE_ANNUAL_TRANS_LOADS, &
                DEALLOCATE_ANNUAL_GAS_LOADS
      LOGICAL*1 LAHEY_LF95
      CHARACTER (LEN=1) :: UTILITY_TYPE_CODE,UTILITY_TYPE
!  COAL MODEL
      LOGICAL (KIND=1) :: CoalLPActive, &
                          RUN_COAL_MODEL_ONLY

      INTEGER (KIND=2) :: CURRENT_YEAR
      LOGICAL (KIND=1) :: TRUE=.TRUE.,FALSE=.FALSE.
      logical (kind=1) :: logresult
      integer :: timesdo
      integer (kind=2) :: gcep
      character (len=2) :: cap_plan_method
      character (len=2) :: grn_mrx_method
      logical :: fcr, aao
      logical :: esf
      logical :: jump_to_breakpoint
      logical :: jump30


      if(file_trace_msgnorm==0) then
        file_trace_msgnorm=open_trace("msgnorm.trace", rq_msgnorm)
        call write_trace_message(file_trace_msgnorm, &
            "procost trace for msgnorm.")

      end if

      FORECAST_ENERGY = 0.

      CLASS_COIN_FACTOR = 0.
      CLASS_RESERVE_MARGIN = 0.
      CLASS_PEAK_LOSSES = 0.
      time_mns=time_mns+1
      CALL DISPLAY_FORECAST_TYPE
      RUN_LOAD = .TRUE.
      UTILITY_TYPE_CODE = UTILITY_TYPE()
      RUN_PROCOST = .TRUE.
      RUN_FINANCE = .TRUE.
      RUN_FINASSET = .TRUE.
      ENDPOINTS_RUN = 0
      call set_NUNITS(int(0,2))
      HYDRO_UNITS = 0
      NUMBER_OF_CONTRACTS = 0
      SYSCAP = 0
      CAPACITY_PLANNING_IS_ACTIVE = .FALSE.
      START_YEAR = 1
      STARTING_END_POINT = 1
      if(get_globecom_study_period()==0) then
        call end_program("msgnorm:0008 - get_globecom_study_period() is zero.")
      end if
      
      LAST_LOOP_YEAR = get_globecom_study_period() + get_EXTENSION_PERIOD()
      YEARS_TO_RUN = LAST_LOOP_YEAR
!
      TESTING_PLAN = .FALSE.
! 091416.
      UZVars = 0.0
      RXVars = 0.0
      TBVARS = 0.0
      MXVARS = 0.0
      NEWVars = 0.0
      CAPACITY_AREA_NAME = " "
!
!  IF CAPACITY PLANNING IS ACTIVE READ THE OBJECTIVE FUNCTION
!
      FIRST_END_POINT = (gc_end_point == STARTING_END_POINT)
      
      INIT_FILE_FOUND = INIT_FILE_IS_ACTIVE()  .AND. &
                                                 .NOT. SP_CAPEX_ACTIVE()
      IF(CAPACITY_PLANNING_ACTIVE()) &
            CALL POSTFIX_NOTATION_OBJECT(OBJECTIVE_FUNCTION_ACTIVE)
      ACTIVE_CAPACITY_PLANNING_METHOD = CAPACITY_PLANNING_METHOD()
      IF(ACTIVE_CAPACITY_PLANNING_METHOD == 'NE' .AND. &
                                        CAPACITY_PLANNING_ACTIVE()) THEN

      ENDIF
!
      SIMSTATUS_REPORT = SIMSTATUS_ACTIVE()
      call write_trace_bool1(file_trace_msgnorm, "SIMSTATUS_REPORT", &
        SIMSTATUS_REPORT)
      
      IF(SIMSTATUS_REPORT) THEN
         INQUIRE(UNIT=7101,OPENED=ERROR_FILE_OPEN)
         IF(ERROR_FILE_OPEN) CLOSE(7101)
         CALL CURDIR(" ",CURRENT_DIRECTORY)
         STATUS_FILE_NAME = TRIM(CURRENT_DIRECTORY)//"\SIMSTATUS.LOG"
         INQUIRE(FILE=STATUS_FILE_NAME,EXIST=ERROR_FILE_EXISTS)
         IF(ERROR_FILE_EXISTS) CALL ERASE(STATUS_FILE_NAME)
         OPEN(7101,FILE=STATUS_FILE_NAME, &
                        CARRIAGE CONTROL="FORTRAN", &
                        ACCESS= 'DIRECT',RECL=128)
         CALL DATE_AND_TIME(START_SIMDATE, &
                            START_SIMTIME, &
                            START_SIMZONE, &
                            START_SIMDT)
         SIM_REC = 1
      ENDIF
      ! TODO: put exit condition here (DO WHILE).  Currently,
      ! it checks end_of_study_flag() and calls exit right before 
      ! the ENDDO
      DO ! START OF STUDY LOOP
         timesdo=timesdo+1
         CALL SET_NEW_ASSETS_INACTIVE
!
! 090106. TEMP TEST FOR LONG RUNS
! 090606. SEEMS TO CAUSE MORE PROBLEMS. ERASE IF EXISTS AS A TEST.
!
      GRX_ITERATIONS = 0

      END_POINT_STARTING_RECORD = SAVE_END_POINT_STARTING_RECORD()
      RUN_FUTURE_ASSETS = .FALSE.
      CALL RESET_PROJECTS_IN_PROCESS_RECS
! 042811. MOVED FOR TVA STOCHASTICS.
      VOID_LOGICAL = READ_LH_GLOBAL_FILE()
      BIP_FILE_NAME = TRIM(OUTPUT_DIRECTORY())//"BIP"// &
            TRIM(SCENAME)//".BIN"
      IF(FIRST_END_POINT) THEN
         FILE_NAME = trim(OUTPUT_DIRECTORY())//"OL*.BIN"
         CALL ERASEWC(FILE_NAME)
         
         ! This is the BC_RESET call that John's interested in
         CALL BC_RESET
         gcep=gc_end_point
         

         

         CALL STARTOVL(YEARS_TO_RUN,START_YEAR,gcep, &
                       ENDPOINTS_RUN,jump_to_breakpoint, &
                       RUN_FUTURE_ASSETS)
         


         
         if (jump_to_breakpoint) then
            goto 30
         end if
!
!  FOR A ONE BRANCH TREE NEED TO UNSET THE END_OF_STUDY_FLAG
!   WHEN AUTO_OPTIM IS BEING RUN
!
         
         IF(CAPACITY_PLANNING_ACTIVE() .AND. &
                 ACTIVE_CAPACITY_PLANNING_METHOD == 'OP') &
                            VOID_LOGICAL = SET_END_OF_STUDY_FLAG_FALSE()
      ELSEIF(.NOT. END_OF_STUDY_FLAG()) THEN
         IF(CAPACITY_PLANNING_ACTIVE() .AND. &
                           ACTIVE_CAPACITY_PLANNING_METHOD == 'OP') THEN
            gc_end_point = gc_end_point + 1
            CALL SET_FIRST_FINANCIAL_ADD_TRUE
            IF(LAHEY_LF95()) THEN
               CALL RW_SET_ENDPOINTS_ON_STATUS(gc_end_point)
            ELSE

               WRITE(SCREEN_MESSAGES,"(I4)") gc_end_point
               CALL MG_CLEAR_LINE_WRITE(8,59,63,trim(SCREEN_MESSAGES), &
                                                         ALL_VERSIONS,0)
            ENDIF
         ELSE
            CALL CLOSE_FORECAST_DATA_FILES
            FILE_NAME = trim(OUTPUT_DIRECTORY())//"OL*.BIN"
            CALL ERASEWC(FILE_NAME)
            CALL BC_RESET
            CALL GETOVLS(YEARS_TO_RUN,START_YEAR,gc_end_point, &
                    ENDPOINTS_RUN,jump30,RUN_FUTURE_ASSETS)
            if(jump30) then
                goto 30
            end if
         ENDIF
      ENDIF
      GRX_SAVED_ENDPOINT = gc_end_point
      IF(gc_end_point == 3) THEN
         gc_end_point = gc_end_point
      ENDIF
!
!  code to run coal model as standalone
!
      TestFactor = .5
      PRODP_FILE_EXISTS = PRODUCTION_PARAMETERS_OBJECT(INT2(472))
      VOID_LOGICAL = UPDATE_PRODUCTION_PARAMETERS(START_YEAR)
      logresult=logical(COAL_MODEL_ONLY(),2)
      ! run_coal_mode_only.or.logresult=false/false
      ! Hit once and evaluates false
      if(get_globecom_study_period()==0) then
        call end_program("msgnorm:0009 - get_globecom_study_period() is zero.")
        
      end if
      
      IF(RUN_COAL_MODEL_ONLY() .OR. logresult) THEN
         CALL MG_LOCATE_WRITE(20,70,"Stand Alone Coal Model Running", &
                                                         ALL_VERSIONS,0)
         CoalLPActive = READ_COAL_ESCALATION_FILE(get_BASE_YEAR())
         CoalLPActive = READ_BASIN_SUPPLY_SO2_DATA()
      CoalLPActive = READ_PLANT_CONT_DEMAND_LNK_DATA(get_BASE_YEAR(), &
                               get_BASE_YEAR()+get_globecom_study_period())

         CALL InitCNWStructure(SCENAME,TRUE,TestFactor)
         CALL DISPLAY_TIME
         ! get_globecom_study_period()=asdf/asdf
         if(get_globecom_study_period()==0) then
            call end_program("msgnorm:0007 - get_globecom_study_period() is zero.")
         end if
         call write_trace_int2(file_trace_msgnorm, "STUDY_PERIOD 2.", &
            get_globecom_study_period())
            
         do globecom_YEAR = 1, get_globecom_study_period()
         
            call write_trace_message(file_trace_msgnorm, &
                "Entering study period loop.")
                
            CURRENT_YEAR = get_BASE_YEAR() + globecom_YEAR
            
            call write_trace_int2(file_trace_msgnorm, "YEAR-iter", &
                globecom_YEAR)
                
            WRITE(SCREEN_MESSAGES,"(I4,A,I4)") CURRENT_YEAR, &
                                             ' End Point: ',gc_end_point
            CALL MG_LOCATE_WRITE(6,26,TRIM(SCREEN_MESSAGES), &
                                                         ALL_VERSIONS,1)
            ! owner of interface arrays
            CALL GregsCnwRoutine(get_BASE_YEAR(),globecom_YEAR,.true.) 
            CALL COAL_MODEL_REPORTS(CURRENT_YEAR,globecom_YEAR,gc_end_point,.true.)
            CALL DISPLAY_TIME
            
            call write_trace_message(file_trace_msgnorm, &
                "Study period loop - bottom.")
                
         enddo
         FIRST_END_POINT = .FALSE.
         ENDPOINTS_RUN = ENDPOINTS_RUN + 1
         esf=end_of_study_flag() ! For debugger
         
         
         call write_trace_bool1(file_trace_msgnorm, "END_STUDY_flag", &
            END_OF_STUDY_FLAG())
            
         IF(END_OF_STUDY_FLAG()) THEN
         
         call write_trace_int2(file_trace_msgnorm, "EP=", gc_end_point)
         
            CLOSE(38,IOSTAT=IOS)
            OPEN(38,FILE=BIP_FILE_NAME, &
                                  ACCESS="TRANSPARENT",STATUS="UNKNOWN")
            WRITE(38,REC=4)gc_end_point,get_BASE_YEAR(),CURRENT_YEAR
            CLOSE(38)
            EXIT
         ENDIF
         CYCLE ! TO END OF ENDPOINT LOOP
      ENDIF

      ! at year=1:
      ! wvpa=asdf/.false.
      IF(WVPA()) then
        VOID_LOGICAL = WVPA_INIT_TRACKER_DATABASE()
      end if
      CALL MANAGE_ASSET_ALLOCATIONS(.TRUE.)
      CALL STORE_END_POINT(gc_end_point)
      TEMP_L = RESET_INTO_EXTENSION_PERIOD() ! 050709.
      IF(INIT_FILE_FOUND) THEN
         CALL OPEN_ASSET_VECTOR_FILE(82) ! 3/28/95 MAKING ONE OPEN
         ! NEED ELIMINATION CLASS B4 ASSET ANALYSIS
         CALL INIT_ASSET_CLASS_INFO  
         CALL ZERO_ASSET_CLASS_ARRAYS
         CALL READ_CLASS_INITIALIZATION_FILE
      ENDIF

!
!  THIS IS THE RECYCLE POINT FOR AUTO_OPTIM OPERATION
!
!
!  SET PROPER ESCALATION VALUES
!
!  CALL TO ESCALATION OBJECT ADDED 5/20/93 WILL REMOVE THE OTHER READS
!  LATER
!
! 0803006 MOVED FOR BURESH.
!
      VOID_LOGICAL = READ_LH_GLOBAL_FILE()
!
      VOID_LOGICAL = READ_SCENARIO_MAKER_DATA()
      CALL PROCESS_CAPITAL_RATES_FILE
      VOID_LOGICAL = READ_ESCALATION_FILE()
!
!  SET-UP MONTHLY FUEL PRICE FILE FOR THIS END POINT
!
      CALL BUILD_FUEL_PRICE_INFO(FUEL_PRICE_DATA_AVAILABLE)
!
      CALL OPEN_VAR_UNITS_FILE
!
      CALL INIT_ENDPT_SWITCHES
! 03/16/05. IN FOR BURESH.
!      RATIOS_FOUND = READ_ICAP_FILE()
!
      CALL CLS(9,10,12)
      SIMULATION_YEARS = RUN_YEARS()
      WRITE(RUN_END_POINT,100) 'End point: ',gc_end_point
      CALL CLS(16,9,36)
      CALL DISPLAY_TIME
      VOID_LOGICAL = SET_ASSET_ANALYST_ONLY_SWITCH()

      VOID_LOGICAL = DETAILED_REPORTS_OBJECT()
      CALL INIT_USER_MARKET_PRICES()
      globecom_YEAR = START_YEAR
      IF(.NOT. ASSET_ANALYST_ONLY()) THEN
         ! right is .false. at year=1
         VOID_LOGICAL = READ_PRICE_FEEDBACK_DATA()
         CALL OPEN_ASSET_VECTOR_FILE(82)
         LM_FIN_FILE_EXISTS = INDEX(LD_FIN_FIL(),'NONE') == 0

!
         VOID_LOGICAL = READ_TRANS_GROUPS_DATA()
! 070409.
         VOID_LOGICAL = MANAGE_TRAN_EXP_FORECASTS()
!
         VOID_LOGICAL = MANAGE_GAS_NODE_FORECASTS()
         VOID_LOGICAL = MANAGE_GAS_SUPPLY_FORECASTS()
         VOID_LOGICAL = MANAGE_GAS_DEMAND_FORECASTS()
         VOID_LOGICAL = MANAGE_GAS_LINK_FORECASTS()
         VOID_LOGICAL = MANAGE_GAS_STORAGE_FORECASTS()
!
! 042209.
!
         VOID_LOGICAL = MANAGE_RPS_REQUIREMENTS()
         VOID_LOGICAL = MANAGE_RPS_PROGRAM_FORECASTS()
         CALL MANAGE_RPS_STATE_DEMAND()
         CALL MANAGE_THERMAL_RETROFIT()
         VOID_LOGICAL = MANAGE_CO2_PARAM_FORECASTS()
!
         gas_lp_active = LP_GAS_MODEL()
!
         if(gas_lp_active) then
            TEMP_R = UPDATE_HH_PRICE(0_2,0_2)

            call FirstGNWCallGreg
            TEMP_L = GAS_PRICING_ROUTINE()
         endif

         VOID_LOGICAL = READ_REGIONAL_PARAMS_DATA()
!
         VOID_LOGICAL = READ_ENERGY_PRODUCTS_DATA()
! 010618. MOVED FROM PROCOST.
         TEMP_L = READ_TRANS_CONSTRAINT_DATA()
         TEMP_L = READ_TRANS_PATH_DATA()
! 090106.
         CALL RESET_FISCAL_MONTHLY_GROUP()
!
!
!  8/9/94. GAT. MOVED FROM INSIDE THE YEAR=1 LOOP
!
         CALL INIT_LAST_REFERENCE_LOADS
         IF(.NOT. NERC_REGION_BASED_FORECAST()) &
                                             CALL UPDATE_REFERENCE_LOADS
         CAPACITY_PLANNING_IS_ACTIVE = CAPACITY_PLANNING_ACTIVE()
      ELSE
         VOID_LOGICAL = READ_TRANS_GROUPS_DATA()
         CAPACITY_PLANNING_IS_ACTIVE = .FALSE.
         PRODP_FILE_EXISTS = .FALSE.
      ENDIF
!
!
      FIRST_PASS = .TRUE.
!
      CoalLPActive = .FALSE.
      DO          ! if pr planning loop
         ! Only one hit (left/right)
         TESTING_PLAN = .FALSE.
         IF(.NOT. ASSET_ANALYST_ONLY()) THEN
            IF(ACTIVE_CAPACITY_PLANNING_METHOD == 'PR' .AND. &
                            CAPACITY_PLANNING_IS_ACTIVE .AND. &
                                                        FIRST_PASS) THEN
               TESTING_PLAN = .TRUE.
            ENDIF
            call write_trace_bool1(file_trace_msgnorm, "2. SP_CAPEX_ACTIVE()", &
                SP_CAPEX_ACTIVE())
                
            IF(.NOT. SP_CAPEX_ACTIVE()) THEN
               IF(SYSTEM_BASED_FORECAST()) THEN
                  CALL SF_GET_PEAKS
               ELSEIF(.NOT. NERC_REGION_BASED_FORECAST()) THEN
                  CALL CF_GET_PEAKS
               ENDIF
               CALL MDSLDCAP
            ENDIF
!
!
            IF(FIRST_PASS .OR. (CAPACITY_PLANNING_METHOD() == 'MX' &
                                 .AND. GREEN_MRX_METHOD() == 'GX')) THEN
               if(get_globecom_study_period()==0) then
                call end_program("msgnorm:0006 - get_globecom_study_period() is zero.")
               end if
               
               TEMP_L = MULTI_YEAR_ENERGY_PRODUCTS()
               
               ! Code breaks on write error in this routine
               MARKET_GROUPS = READ_MARKET_GROUPS_DATA()
               HYDRO_UNITS = EL_UNITS_READ()
               VOID_LOG1 = READ_DAY_TYPE_DATA()
               VOID_LOG1 = READ_USER_DAY_DATA()
               

               call set_nunits(CL_UNITS_READ())
               call write_trace_int2(file_trace_msgnorm, "1. NUNITS", get_nunits())
               if(get_nunits()==0) then
                    call end_program("msgnorm:0005 - NUNITS is " // &
                        "zero after calling cl_units_read.")
               end if
               
               NUMBER_OF_CONTRACTS = CONTRACTS_READ()
               CAPACITY_PLANNING_IS_ACTIVE = CAPACITY_PLANNING_ACTIVE()
            ENDIF
         ENDIF
         IF(INIT_FILE_FOUND) THEN
            IF(FIRST_PASS) THEN
               CALL FINASSET(.FALSE.)
            ELSE
               CALL ZERO_ASSET_CLASS_ARRAYS
            ENDIF
            CALL READ_CLASS_INITIALIZATION_FILE
         ENDIF
!
         CUM_ANN_EMIS = 0.
         YEAR_LOOP = START_YEAR
         IF(.NOT. INIT_FILE_FOUND) then
           ! WRITE THE BASE YEAR TO THE BIP FILE
            CALL WRITE_BASE_YR_RESULTS() 
         endif
!
         TEMP_I2 = ENDPOINTS_RUN + 1 ! 081416.
         CALL RW_SET_ENDPOINTS_ON_STATUS(TEMP_I2) ! gc_end_point)
         GRX_SAVED_ENDPOINT = gc_end_point
         GRX_PRT_ENDPOINT = 100 * gc_end_point
         GRX_UNIT_COUNT = 0
         ! 2nd hit:
         ! year_loop=2/asdf
         ! last_loop_year=5/asdf
         ! grx_prt_endpoint=100/asdf
         ! No second hit for right!!!!!!!!!!!!!
         call write_trace_int2(file_trace_msgnorm, "NUNITS", get_NUNITS())
         call write_trace_int2(file_trace_msgnorm, "LAST_LOOP_YEAR", &
            LAST_LOOP_YEAR)
            call write_trace_int2(file_trace_msgnorm, "1. YEAR_LOOP", &
                YEAR_LOOP)         
         
         DOWHILE (YEAR_LOOP <= LAST_LOOP_YEAR)
            globecom_YEAR = YEAR_LOOP
            call write_trace_int2(file_trace_msgnorm, "2. YEAR_LOOP", &
                YEAR_LOOP)
            
            
            GRX_UNIT_COUNT(YEAR_LOOP,-1) = get_NUNITS()
            
            
            if (get_BASE_YEAR()==0) then
                er_message="msgnorm:0004 - " // &
                    "Base year is zero and should be a calendar year."
                call end_program(er_message)
            end if
                        
            MAX_GRX_ITERs = MAX_GRX_ITERATIONS(get_BASE_YEAR() + globecom_YEAR)
            GRX_ITERATIONS = 0
            VOID_LOGICAL = UPDATE_PRODUCTION_PARAMETERS(globecom_YEAR)
            !CoalLpActive=asdf/asdf
            IF(.NOT. CoalLPActive) THEN !  .AND. RUN_COAL_MODEL()) THEN
!
!  the following call order must be kept.
!
               CoalLPActive = READ_COAL_ESCALATION_FILE(get_BASE_YEAR())
               CoalLPActive = READ_BASIN_SUPPLY_SO2_DATA()
               CoalGenericPlant = &
                        READ_GEN_PLANT_DEMAND_LINK_DATA(get_BASE_YEAR())
               CoalExistingPlant = &
                      READ_PLANT_CONT_DEMAND_LNK_DATA(get_BASE_YEAR(), &
                                          get_BASE_YEAR()+get_globecom_study_period())
               IF((CoalExistingPlant .OR. CoalGenericPlant) .AND. &
                                                      CoalLPActive) THEN
                   CALL COAL_DEMAND_LINK()
                   CALL InitCNWStructure(SCENAME,TRUE,TestFactor)
                   CoalLPActive = .TRUE.
               ENDIF
             ENDIF
               call write_trace_int2(file_trace_msgnorm, &
                "CALL RW_SET_YEAR_ON_STATUS", get_BASE_YEAR()+globecom_YEAR)
                
               CALL RW_SET_YEAR_ON_STATUS(get_BASE_YEAR() + globecom_YEAR)
               IF(SP_CAPEX_ACTIVE()) THEN
                  WRITE(SCREEN_MESSAGES,"(I4,A,I4,A)") get_BASE_YEAR() &
                  + globecom_YEAR, &
                      ' End Point: ',INT(PRT_ENDPOINT()),' for SP CapEx'
               ELSE
                  WRITE(SCREEN_MESSAGES,"(I4,A,I4)") get_BASE_YEAR() &
                  + globecom_YEAR, &
                                      ' End Point: ',INT(PRT_ENDPOINT())
               ENDIF
               CALL MG_LOCATE_WRITE(6,26,trim(SCREEN_MESSAGES), &
                                                         ALL_VERSIONS,1)
               IF(QUICKSILVER()) THEN
                  OPEN(10,FILE='IamStillRunning') 
                  WRITE(10,*) trim(SCREEN_MESSAGES)
                  call flush(10)
                  CLOSE(10)
               ENDIF
            
            call write_trace_bool1(file_trace_msgnorm, "WVPA()", &
                wvpa())
            IF(WVPA()) THEN
               ! right enters first time
               CALL WVPA_STORE_CURRENT_DATE(globecom_YEAR,get_BASE_YEAR())
            ENDIF

!
            VOID_LOGICAL = RETURN_DETAILED_REPORTS(globecom_YEAR)
!
            VOID_INT2 = SET_ESCALATION_VECTOR_STRUCTURE(globecom_YEAR)
!
            VOID_INT2 = SETUP_DAY_OF_WEEK_4(get_BASE_YEAR() + globecom_YEAR)
            YES_GAS_MODEL_ONLY = GAS_MODEL_ONLY()
            GRX_NUNITS = get_NUNITS()
            write(9,*) "Before GRX Loop",get_NUNITS()
            if(get_BASE_YEAR()+globecom_YEAR == 2018) then
               GRX_NUNITS = get_nunits()
            endif
            IF(.NOT. ASSET_ANALYST_ONLY() .AND. &
                  CAPACITY_PLANNING_METHOD() == 'MX'  .AND. &
                                        GREEN_MRX_METHOD() == 'GX') THEN
              imm_year=globecom_YEAR
              TEMP_L = ADJUST_CO2_RETRO_PLAN_CAP(imm_year)
               CALL GRX_SAVE_PLANNIG_VALUES(STORE)
               CALL GRX_SAVE_CAPACITY_OPTIONS(STORE)
               CALL GRX_RETROFIT_OPTONS(STORE)
               VOID_LOGICAL = GRX_SAVE_DERIV_PLAN_VALUES(STORE,globecom_YEAR)
               VOID_LOGICAL = GRX_SAVE_RPS_PROGRAM_VALUES(STORE)
            ENDIF
!
!  TESTING THE GRX PLANNING LOOP FOR HOLDING THE CURRENT VALUE OF 
!  VARIABLES
!
          GRX_CONVERGED =.FALSE.
          OneMoreIter = .false.
          DO    ! GRX PLANNING LOOP
            INSTALLED_POINTER = 0
            aao=ASSET_ANALYST_ONLY()
            call write_trace_bool(file_trace_msgnorm, "ASSET_ANALYST_ONLY", &
                aao)
                
            ! year=2/asdf
            ! aao=.false./asdf
            IF(.NOT. aao) THEN

               
               cap_plan_method=CAPACITY_PLANNING_METHOD()
               
               call write_trace_string(file_trace_msgnorm, &
                "GREEN_MRX_METHOD", GREEN_MRX_METHOD())
                
               grn_mrx_method=GREEN_MRX_METHOD()

               IF(cap_plan_method == 'MX'  .AND. &
                                        grn_mrx_method == 'GX') THEN
                  WRITE(SCREEN_MESSAGES,"(A,I2)") &
                                   ' GRX Iteration: ',GRX_ITERATIONS
                  CALL MG_LOCATE_WRITE(6,26,trim(SCREEN_MESSAGES), &
                                                         ALL_VERSIONS,1)
               ENDIF


               fcr=forecast_report()
               ! fcr=asdf/asdf
               call write_trace_bool1(file_trace_msgnorm, "RUN_LOAD", &
                run_load)
               call write_trace_int2(file_trace_msgnorm, "YEAR_LOOP", &
                YEAR_LOOP)
               call write_trace_int2(file_trace_msgnorm, &
                "GLOBECOM_STUDY_PERIOD", get_globecom_study_period())
               call write_trace_bool(file_trace_msgnorm, &
               "FORECAST_REPORT", fcr)
               
                call write_trace_bool1(file_trace_msgnorm, "RUN_LOAD", &
                    RUN_LOAD)

               IF((RUN_LOAD .OR. fcr) .AND. &
                       (YEAR_LOOP <= get_globecom_study_period())) THEN
                  IF(YEAR_LOOP == 1 .AND. GRX_ITERATIONS == 0) THEN
!
                     CALL DISPLAY_TIME
                     call write_trace_bool1(file_trace_msgnorm, &
                     "1. SP_CAPEX_ACTIVE()", SP_CAPEX_ACTIVE())
                                          
                     IF(.NOT. SP_CAPEX_ACTIVE()) &
                        CALL MG_LOCATE_WRITE(16,9, &
                               'Analyzing forecast data',ALL_VERSIONS,1)
!
!  NEED PEAKS FOR CAPACITY PLANNING
!
                        CALL DISPLAY_TIME
                        CALL CLS(16,9,32)
!
                        WEEKLY_HYDRO_ACTIVE = &
                                         MANAGE_WEEKLY_HYDRO_FORECASTS()
!
                     ENDIF


                     IF(.NOT. SP_CAPEX_ACTIVE()) &
                                CALL MG_LOCATE_WRITE(9,15,"Markets",3,4)
                     IF(.NOT. TRANSACT_ANALYST_ONLY()) THEN
                        CALL LOAD_ANALYSIS_OBJECT()
                     ENDIF
!
!     USED TO ACCUMULATE LOADS FOR USE IN TRANSACT

                     ! 2nd time, year=2
                     ! Left gets here twice/right, once
                     VOID_LOGICAL =  ANNUAL_TRANSACTION_LOADS(globecom_YEAR)
                     VOID_LOGICAL =  ANNUAL_GAS_DEMAND(globecom_YEAR)
                     VOID_LOGICAL = &
                                  CALC_ANNUAL_SUPPLY_CURVE_PRICE(globecom_YEAR, &
                                         int(GRX_ITERATIONS,4))

                     CALL DISPLAY_TIME
                     CLOSE(29)
                  ELSE
                     IF(GRX_ITERATIONS == 0) &
                          VOID_LOGICAL =  ANNUAL_TRANSACTION_LOADS(globecom_YEAR)
                  ENDIF
                  
                  call write_trace_int2(file_trace_msgnorm, "1. YEAR", globecom_YEAR)

                  call write_trace_int2(file_trace_msgnorm, &
                    "Calling ANNUAL_RPS_STATE_DEMAND for YEAR", globecom_year)
                    
                  CALL ANNUAL_RPS_STATE_DEMAND(globecom_YEAR)
!
!  READ THE ENVIRONMENTAL FILE
!
                  ! Left gets here twice. Right, once.
                  call write_trace_message(file_trace_msgnorm, &
                    "calling envir_data_read")
                  VOID_LOGICAL = ENVIR_DATA_READ()
                  call write_trace_int2(file_trace_msgnorm, &
                   "calling ANNUAL_ENERGY_PRODUCTS for year", globecom_YEAR)
                  
                  VOID_LOGICAL = ANNUAL_ENERGY_PRODUCTS(globecom_YEAR)
                  
                  if (globecom_year==2) then
                    globecom_year=globecom_year ! debugstop
                  end if
                  


             call write_trace_string(file_trace_msgnorm, &
                "2. CAPACITY_PLANNING_METHOD", CAPACITY_PLANNING_METHOD())
                    
                  IF(((CAPACITY_PLANNING_METHOD() == 'MX' &
                           .AND.      GREEN_MRX_METHOD() == 'GX') .OR. &
                      CAPACITY_PLANNING_IS_ACTIVE) .AND. &
                                          .NOT. YES_GAS_MODEL_ONLY) THEN
                     IF(ACTIVE_CAPACITY_PLANNING_METHOD == 'NE' .OR. &
                        ACTIVE_CAPACITY_PLANNING_METHOD == 'SC' .OR. &
                        ACTIVE_CAPACITY_PLANNING_METHOD == 'PR' .OR. &
                        ACTIVE_CAPACITY_PLANNING_METHOD == 'TR' .OR. &
                        ACTIVE_CAPACITY_PLANNING_METHOD == 'MX' .OR. &
                                                globecom_YEAR == START_YEAR) THEN
                        IF(.NOT. SP_CAPEX_ACTIVE()) &
                             CALL MG_LOCATE_WRITE(10,15,"Resources",3,4)
                        timeshere=timeshere+1
                      

                        call write_trace_int2(file_trace_msgnorm, &
                        "calling CAPACITY_PLANNING_OBJECT with " // &
                        "NUNITS", get_nunits())
                        
                        CALL CAPACITY_PLANNING_OBJECT(get_NUNITS())

                        call write_trace_message(file_trace_msgnorm, &
                            "called CAPACITY_PLANNING_OBJECT")
                        
                        call write_trace_int2(file_trace_msgnorm, &
                            "YEAR=", globecom_YEAR)
!       call end_program("msgnorm:0010 - I never get here.")
                        call write_trace_int4(file_trace_msgnorm, &
                            "GRX_ITERATIONS", GRX_ITERATIONS)
                        call write_trace_bool1(file_trace_msgnorm, &
                            "CoalLPActive", CoalLPActive)
                            
                        IF(globecom_YEAR ==1 .AND. GRX_ITERATIONS == 0 .AND. &
                                                         CoalLPActive) then
                                                         
                            call write_trace_int2(file_trace_msgnorm, &
                                "Calling COAL_HARDWRIED_DEMAND_LINK with" // &
                                "NUNITS", get_nunits())
                                
                            CALL COAL_HARDWRIED_DEMAND_LINK(get_NUNITS())
                        end if
                        CAPACITY_WAS_ADDED = .TRUE.
                        CALL DISPLAY_TIME
                     ENDIF
                  ENDIF

        call write_trace_bool1(file_trace_msgnorm, "RUN_PROCOST", &
            RUN_PROCOST)
            
                  IF(RUN_PROCOST) THEN
                     IF(.NOT. SP_CAPEX_ACTIVE()) &
                            CALL MG_LOCATE_WRITE(11,15,"Operations",3,4)
                     PRICE_PLANNING_FIRST_PASS = FIRST_PASS .AND. &
                         ACTIVE_CAPACITY_PLANNING_METHOD == 'PR' .AND. &
                            CAPACITY_PLANNING_IS_ACTIVE
                     write(9,*) "Before PROCOST",globecom_YEAR,ofline(35)
                     

                    
        call write_trace_message(file_trace_msgnorm, "failure point for right.")
        call write_trace_message(file_trace_msgnorm, "calling procost")
        

                    SYSTEM_CAPACITY=PROCOST(CoalLPActive)
                     write(9,*) "After PROCOST",globecom_YEAR,ofline(35)
                     CALL CLS(11,27,32)
                     CALL DISPLAY_TIME
                  ENDIF
                  VOID_LOGICAL = EMISSIONS_CREDITS()
               ELSE ! TESTING !
                  VOID_LOGICAL =  ANNUAL_GAS_DEMAND(globecom_YEAR)
                  VOID_LOGICAL = &
                                  CALC_ANNUAL_SUPPLY_CURVE_PRICE(globecom_YEAR, &
                                              int(GRX_ITERATIONS,4))
               ENDIF ! END ASSETS ONLY
!
! 09/27/03. MOVED FROM BELOW TO ACCOMODATE LEVELIZED PLANNING
!
               IF(.NOT. (ACTIVE_CAPACITY_PLANNING_METHOD == 'PR' .AND. &
                               CAPACITY_PLANNING_IS_ACTIVE .AND. &
                                                       FIRST_PASS)) THEN
                  
                  CALL TRANSACT_GROUP_RESERVE( &
                     globecom_YEAR,.TRUE.,ASSET_ANALYST_ONLY())
               ENDIF
!
               VOID_LOGICAL = ZERO_TOTAL_EMIS_VARS()
            CALL DISPLAY_TIME
!
!  GRX PLANNING LOOP
!
            IF(.NOT. ASSET_ANALYST_ONLY() .AND. &
                    CAPACITY_PLANNING_METHOD() == 'MX'  .AND. &
                                        GREEN_MRX_METHOD() == 'GX') THEN
               write(9,*) "End GRX Loop Before Reset",get_NUNITS()
               CALL FLUSH(9)
               CALL FLUSH(4)
               write(9,*) "670 converged", GRX_CONVERGED
               GRX_UNIT_COUNT(YEAR_LOOP,GRX_ITERATIONS) = get_NUNITS()
               
               OneMoreIter = .true.
               
      call write_trace_bool(file_trace_msgnorm, "OneMoreIter", &
       OneMoreIter)
               

              IF(GRX_CONVERGED .AND. .NOT. OneMoreIter &
                           .AND. globecom_YEAR > 1 .AND. GRX_ITERATIONS > 0) THEN
                  OneMoreIter = .true.
                  GRX_CONVERGED = .FALSE.
              ELSEIF((GRX_CONVERGED .AND. GRX_ITERATIONS >= &
                  CO2_Min_GRX_Iterations(get_BASE_YEAR()+globecom_YEAR)-1) .OR. &
                   GRX_ITERATIONS >= MAX_GRX_ITERs .OR. globecom_YEAR == 1) THEN
                  ! UPDATE ALL START REC POSITIONS
                  IREC_SAVED = rptrec(REC_OPT='U')   
                  IF(globecom_YEAR > 1) &
                   CALL GRX_RETIREMENT_RETROFIT_RPT(get_BASE_YEAR(), &
                    globecom_YEAR, &
                                   INT2(get_BASE_YEAR()+LAST_LOOP_YEAR))
                  gc_end_point = GRX_SAVED_ENDPOINT
                  NEXT_CURRENT_CO2_DISPATCH_COST = 0.
                  CURRENT_CO2_DISPATCH_COST = 0.
                  EXIT
               ENDIF
               ! RESET ALL START REC POSITIONS
               IREC_SAVED = rptrec(REC_OPT='A')   
               HYDRO_UNITS = RESET_EL_UNITS()

               !RESETS THE AVAILABLE ANNUAL UNITS
               RETIRED_UNITS = RESET_ANNUAL_UNITS_LEFT() 
               call set_nunits(GRX_NUNITS)
               VOID_LOGICAL = CONTRACT_RESET_OPTIONS()
               VOID_LOGICAL = DSM_RESET_OPTIONS()

               GRX_ITERATIONS = GRX_ITERATIONS + 1
               GRX_PRT_ENDPOINT = GRX_PRT_ENDPOINT + 1

               CALL GRX_SAVE_PLANNIG_VALUES(RESTORE)
               CALL GRX_SAVE_CAPACITY_OPTIONS(RESTORE)
               CALL GRX_RETROFIT_OPTONS(RESTORE)
               VOID_LOGICAL = GRX_SAVE_DERIV_PLAN_VALUES(RESTORE,globecom_YEAR)
               VOID_LOGICAL = GRX_SAVE_RPS_PROGRAM_VALUES(RESTORE)
               write(9,*) "End GRX Loop After Reset",get_NUNITS()
             ELSE
             
               call write_trace_message(file_trace_msgnorm, &
                "Exiting GRX loop.")
                
               EXIT
             ENDIF
             
               call write_trace_message(file_trace_msgnorm, &
               "Bottom of GRX Loop.")
               
          ENDDO ! END GRX PLANNING LOOP

               IF(INIT_FILE_FOUND) THEN
                  IF(RUN_FINANCE .OR. RUN_FINASSET .OR. &
                                                LM_FIN_FILE_EXISTS) THEN
                     IF(.NOT. SP_CAPEX_ACTIVE()) THEN
                        IF(ASSET_ANALYST_ONLY()) THEN
                           CALL MG_LOCATE_WRITE(8,15,"Financial",3,4)
                        ELSE
                           CALL MG_LOCATE_WRITE(12,15,"Financial",3,4)
                        ENDIF
                     ENDIF
                     IF(CAPACITY_PLANNING_IS_ACTIVE .AND. &
                       CAPACITY_WAS_ADDED .AND. &
                         (globecom_YEAR <= get_globecom_study_period()) .AND. &
                         (globecom_YEAR == START_YEAR .OR. &
                          ACTIVE_CAPACITY_PLANNING_METHOD == 'NE' .OR. &
                          ACTIVE_CAPACITY_PLANNING_METHOD == 'TR' .OR. &
                          ACTIVE_CAPACITY_PLANNING_METHOD == 'MX' .OR. &
                        (ACTIVE_CAPACITY_PLANNING_METHOD == 'PR' .AND. &
                                                    FIRST_PASS))) THEN
                      IF(ACTIVE_CAPACITY_PLANNING_METHOD == 'NE' .OR. &
                          ACTIVE_CAPACITY_PLANNING_METHOD == 'TR' .OR. &
                          ACTIVE_CAPACITY_PLANNING_METHOD == 'SO' .OR. &
                          ACTIVE_CAPACITY_PLANNING_METHOD == 'MX' .OR. &
                        (ACTIVE_CAPACITY_PLANNING_METHOD == 'PR' .AND. &
                                                     FIRST_PASS)) THEN
                         
                         CALL FUASST(.FALSE.,.TRUE.)
                        ELSE
                           CALL FUASST(.TRUE.,.TRUE.)
                        ENDIF
                        CALL write_scroll_line_RW(' ',2)
                        CALL write_scroll_line_RW(' ',0)
                     ENDIF
                     CALL DISPLAY_TIME
                     IF(globecom_YEAR == START_YEAR) CALL ZERO_DSM_RESULTS
                     IF(LM_FIN_FILE_EXISTS) CALL DSM_FINANCIAL_COST
                  IF(ACTIVE_CAPACITY_PLANNING_METHOD == 'PR' .AND. &
                      FIRST_PASS .AND. CAPACITY_PLANNING_IS_ACTIVE) THEN
                     CALL FINANCE(globecom_YEAR,.FALSE.,LAST_LOOP_YEAR)
                  ELSE
                     CALL FINANCE(globecom_YEAR,.TRUE.,LAST_LOOP_YEAR)
                  ENDIF
                  CALL DISPLAY_TIME
               ENDIF
               IF(.NOT. ASSET_ANALYST_ONLY()) CALL COST_OF_SERVICE
             ENDIF

!
!  OUTPUT REPORTS
!
            CURRENT_YEAR = get_BASE_YEAR() + globecom_YEAR
            CALL ABB_CapacityMarketReport(CURRENT_YEAR)

            IF(ACTIVE_CAPACITY_PLANNING_METHOD == 'PR' .AND. &
                               CAPACITY_PLANNING_IS_ACTIVE .AND. &
                                                        FIRST_PASS) THEN
               CALL OUTPUT(globecom_YEAR,.FALSE.,ASSET_ANALYST_ONLY())
            ELSE
               CALL OUTPUT(globecom_YEAR,.TRUE.,ASSET_ANALYST_ONLY())
            ENDIF
            IF(CAPACITY_PLANNING_IS_ACTIVE .AND. &
                                         OBJECTIVE_FUNCTION_ACTIVE) THEN
               IF(ACTIVE_CAPACITY_PLANNING_METHOD /= 'AN') THEN
                  CALL COMPUTE_OBJECTIVE_FUNCTION
                  IF(END_OF_PLAN_FLAG()) EXIT
               ENDIF
            ENDIF
            CALL DISPLAY_TIME
! 7/1/02.
            RESET_WARNINGS = .FALSE.

            CALL FLUSH(4)
            IF(RESET_WARNINGS) THEN
               INQUIRE(UNIT=4,OPENED=FILE_OPENED)
               IF(FILE_OPENED) THEN
                  CLOSE(4)
                  FILE_NAME = TRIM(OUTPUT_DIRECTORY())//"MSG"// &
                     TRIM(SCENAME)//".ERR"
                  OPEN(4,FILE=FILE_NAME, &
                                  CARRIAGE CONTROL="FORTRAN",RECL=132, &
                                                      POSITION='APPEND')
               ENDIF
            ENDIF
!
          YEAR_LOOP = YEAR_LOOP + 1
          
      call write_trace_message(file_trace_msgnorm, "Bottom of loop 3")
      call write_trace_int2(file_trace_msgnorm, "YEAR_LOOP", YEAR_LOOP)
      
      
         ENDDO  !WHILE YEAR LOOP
!
         IF(WVPA()) THEN
            CALL WVPA_WRITE_PLANT_BAL_RPT(gc_end_point)
            CALL WVPA_CONSTRUCTION_SUMMARY_RPT(gc_end_point)
         ENDIF
         IF(MONTHLY_MIDAS_ACTIVE) THEN
            CALL BASE_YEAR_ACTUAL_INCOME_DATA()
            CALL MONTHLY_TRAILING_12_MONTHS_REVENUE_REPORT()
         ENDIF
         IF(IPALCO() .AND. MONTHLY_MIDAS_ACTIVE) THEN
            CALL IPALCO_REPORTS()
         ENDIF

!
!  RECOVERY MEMORY AT END OF ENDPOINT 7/24/00 MSG
!
         VOID_LOGICAL = DEALLOCATE_TRANS_OUTAGE_DATA()
         VOID_LOGICAL = DEALLOCATE_ANNUAL_TRANS_LOADS()
         VOID_LOGICAL = DEALLOCATE_ANNUAL_GAS_LOADS()
!
         IF(ACTIVE_CAPACITY_PLANNING_METHOD == 'PR' .AND. &
                           CAPACITY_PLANNING_IS_ACTIVE .AND. &
                                                        FIRST_PASS) THEN
            HYDRO_UNITS = RESET_EL_UNITS()
            VOID_LOGICAL = EL_RESET_PRICES()
            call set_NUNITS(RESET_CL_UNITS())
            VOID_LOGICAL = CL_RESET_PRICES()
            VOID_LOGICAL = CONTRACT_RESET_OPTIONS()
            VOID_LOGICAL = DSM_RESET_OPTIONS()
            FIRST_PASS = .FALSE.
            CAPACITY_PLANNING_IS_ACTIVE = .FALSE.

         ELSE
         
            call write_trace_message(file_trace_msgnorm, "Early Exit 2")
            EXIT
            
         ENDIF
       call write_trace_message(file_trace_msgnorm, "Bottom of loop 4")
      ENDDO  ! END PR PLANNING LOOP
      
      ! Hits here first to initialize.
      FIRST_END_POINT = .FALSE.


      IF(YEAR_LOOP > LAST_LOOP_YEAR) THEN
         ENDPOINTS_RUN = ENDPOINTS_RUN + 1

      ELSE
!
!  NEED TO RESET THE OUTPUT POINTER BECAUSE THE PLAN DIDN'T
!
         END_POINT_STARTING_RECORD = SET_OUT_REC_TO_ENDPNT_START_REC()
!
      ENDIF

!  CLEAN UP NEEDED ? IF AUTO_OPTIM NOT ACTIVE (NORMAL CLEAN UP)  THIS
!   NEEDS TO REVIEW BECAUSE IT IS 640k AND FILES = DEPENDENT
!
      CALL CLOSE_FINANCIAL_PARAMETER_FILE
      CALL CLOSE_CAPITAL_RATES_VECTOR_FILE
!
!  CLOSE THE MULTI-YEAR FINANCIAL RUN SPECIFICATIONS FILE
!
      CLOSE(472)    !PRODUCTION PARAMETERS FILE
      IF(FUEL_PRICE_DATA_AVAILABLE) CLOSE(666) !FUEL PRICE FILE
      CALL CLOSE_CLASS_RUN_SWITCH_FILE
      VOID_LOGICAL = CLOSE_DETAILED_REPORTS_FILE()
      CLOSE(50)
      CALL CLOSE_ASSET_VECTOR_FILE
      CLOSE(84)
      CLOSE(43,IOSTAT=IOS) ! ENVIRONMENTAL DATA FILE
      CLOSE(111,IOSTAT=IOS) ! FUEL INVENTORY FILE
      CLOSE(112,IOSTAT=IOS) ! FUEL INVENTORY FILE
      CLOSE(3458,IOSTAT=IOS) ! REVENUE FORECAST
      CLOSE(900,IOSTAT=IOS) ! SYSTEM FORECAST
      CALL CLOSE_FINANCIAL_OPTIONS
!
! CLOSE CPO FILE FOR THE END POINT
!
      INQUIRE(UNIT=9979,OPENED=FILE_IS_OPEN)
      IF(FILE_IS_OPEN) THEN
         IREC_SAVED = rptrec(9979_2,FILE_OPT = 'S')
      ENDIF
!
      if(gas_lp_active) then
         call LastGNWCallGreg
         TEMP_L = WRITE_LNG_TO_DAT()
      endif
! 101116.
      CALL WRITE_GRX_RPS_CAPACITY()

      IF(END_OF_STUDY_FLAG()) then
        call write_trace_message(file_trace_msgnorm, "Exit 1")
        
        EXIT
      end if
      call write_trace_message(file_trace_msgnorm, "STUDY LOOP BOTTOM")
      ENDDO ! STUDY LOOP
   30 CONTINUE
      IF(SP_CAPEX_ACTIVE()) THEN
         OPEN(10,FILE="SPCapExDONE")
         write(10,*) "SP has finished"
         CLOSE(10)
      ENDIF
! 11/10/04. FOR XML INTERFACE
      INQUIRE(UNIT=4444,OPENED=FILE_OPENED)
      IF(FILE_OPENED) THEN
         WRITE(4444,4445) '</NewStation>'
         WRITE(4444,4445) '</XMLIntegration>'
         CLOSE(4444,IOSTAT=IOS)
      ENDIF
! SEQUENTIAL FILE WRITTEN IN GRX DOESN'T NEED RE-SIZING
      CLOSE(8337,IOSTAT=IOS) ! EV GRX RETIRE/RETRO CSV REPORT
      CLOSE(8339,IOSTAT=IOS) ! EV GRX RETIRE/RETRO CSV REPORT

! THE FOLLOWING CLOSES ALL OPEN BINARY FILES.
      IREC_SAVED = rptrec(FILE_OPT = 'C')
      CALL CLOSE_FILES(SCENAME, ENDPOINTS_RUN,get_BASE_YEAR()+LAST_LOOP_YEAR)
      MESSAGE = .FALSE.
      MIDAS_NORMAL_SIMULATION = ENDPOINTS_RUN
      RETURN
4445  FORMAT(1X,4A)
!

      ENTRY UPDATE_STATUS_REPORT(R_MONTH)

            IF(SIMSTATUS_REPORT) THEN

               CLOSE(7101)
               OPEN(7101,FILE=STATUS_FILE_NAME, &
                        ACCESS= 'DIRECT',RECL=128)

               CALL DATE_AND_TIME( &
                            SIMDATE, &
                            SIMTIME, &
                            SIMZONE, &
                            SIMDT)
               TEMP_R = PRT_ENDPOINT() 
               WRITE(SIMLINE,*)  START_SIMDATE,',', &
                                 START_SIMTIME,',', &
                                 SIMDATE,',', &
                                 SIMTIME,',', &
                              cldata%SCENAME,',', &
                                 RPT_PROJECT_NAME(1:5),',', &
                                 RPT_BASE_FAMILY_NAME(1:5),',', &
                                 TEMP_R,',', &
                                 get_BASE_YEAR()+globecom_YEAR,',', &
                                 R_MONTH,',', &
                                 '"Normal Simulation"'
               SIMLINE = trim(SIMLINE)
!               WRITE(7101,'(A)') SIMLINE
               WRITE(7101,REC=1) SIMLINE
!               WRITE(7101) SIMLINE
               SIM_REC = SIM_REC + 1
               UPDATE_STATUS_REPORT = .TRUE.
            ELSE
               UPDATE_STATUS_REPORT = .FALSE.
            ENDIF
      RETURN

      ENTRY FIRST_PASS_IS_ACTIVE

         FIRST_PASS_IS_ACTIVE = FIRST_PASS
      RETURN
  100 FORMAT (A,I4)
      END

      SUBROUTINE INIT_ENDPT_SWITCHES

         CALL INIT_ENDPT_FORE_DAYS_REPORT
      RETURN
      END subroutine INIT_ENDPT_SWITCHES


