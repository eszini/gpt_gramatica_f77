
!     MSGANDEC.FOR
!     Created: 10/24/02 6:01:33 PM
!     Author : msg
!     Last change: GT 4/19/2008 8:01:10 PM


      SUBROUTINE MIDAS_ANN_DECOMP_SIMULATION(SCENAME)
      use spindriftlib
      use prod_arrays_dimensions
      use financial_switches_common
      use tim
      use dreptcom
      use sizecom
      use globecom
      use foshydi2com
      use msgoutpt
      use lamcom
      use envircom
      use dsmfrcom
      use endpoint
      use prodcom
      use prod2com
      use contracts_data
      use forecast
      use nunits_shared
      use program_state
      use debugtrace
      implicit none
      
        integer :: file_trace_msg=0
      INTEGER (kind=2) ::  EL_UNITS_READ,CL_UNITS_READ,CONTRACTS_READ
      INTEGER (kind=2) ::  PLAN_NUMBER
      INTEGER (kind=2) :: VOID_INT2,ANNUAL_DECOMPOSITION_START_UP, 
     +                  RESET_DSM_MAX_PROGRAM_NUM, 
     +            SET_ESCALATION_VECTOR_STRUCTURE
      REAL ::  OBJECTIVE_FUNCTION_VALUE,ANN_DECOMP_OBJECTIVE_FUNCTION
      REAL ::  WORST_OBJECT_FUNCTION_VALUE,ANN_DECOMP_SAVE_PLAN,
     +   BEST_PLAN_OBJECT_FUNCTION_VALUE,
     +   ANN_DECOMP_PICK_BEST_PLAN,VOID_REAL,UPDATE_NET_PLANNING_PEAK

      INTEGER (kind=2) ::  YEAR_LOOP,LAST_LOOP_YEAR
      INTEGER (kind=2) ::  ADDITIONS_BY
      INTEGER (kind=2) ::  EL_PLANNING_ADDITIONS,CL_PLANNING_ADDITIONS
      INTEGER ::  IOS
      INTEGER (kind=2) ::  HARD_WIRED_UNITS_ADDED
      INTEGER (kind=2) ::  ADD_ALL_HARD_WIRED_UNITS
      INTEGER (kind=2) ::  EXPANSION_UNITS,READ_CAPACITY_OPTIONS_FILE
      LOGICAL (kind=1) ::  RATIOS_FOUND,READ_CAP_RATIOS_FILE
!
! PALO ALTO ITEMS
!
      CHARACTER (len=1), parameter ::  PALO_ALTO='P'

!
      LOGICAL (kind=1) ::  RUN_FUTURE_ASSETS,LAST_OPTION,
     +          CHECK_RESOURCE_NEED, 
     +          ANNUAL_DECOMPOSITION_OPTION, 
     +          SET_ANNUAL_DECOMP_OPTIONS, 
     +          SAVE_RESULTS, 
     +          WEEKLY_HYDRO_ACTIVE, 
     +          MANAGE_WEEKLY_HYDRO_FORECASTS

      LOGICAL (kind=1) ::  CONTRACT_RESET_OPTIONS
      LOGICAL (kind=1) ::  PRICE_PLANNING_FIRST_PASS
      logical (kind=1) :: system_forecast
      LOGICAL (kind=1) ::  NERC_REGION_BASED_FORECAST
      INTEGER (kind=2) ::  CURRENT_PLANNING_YEAR,LAST_PLANNING_YEAR
      INTEGER (kind=2) ::  RESET_EL_UNITS,RESET_CL_UNITS
      CHARACTER (len=256) ::  OUTPUT_DIRECTORY,U50_FILE
      INTEGER (kind=2) ::  YEARS_TO_RUN,START_YEAR,
     +          LAST_YEAR, 
     +          ENDPOINTS_RUN, 
     +          STARTING_END_POINT
      INTEGER (kind=4) ::  SYSTEM_CAPACITY,
     +          PROCOST
      REAL ::  GET_ENERGY_EXCHANGE_ADJUSTMENT
      REAL (kind=8) ::  GET_TOTAL_SALES_REVENUE
!     VARIABLES FOR COST OF SERVICE
      LOGICAL (kind=1) ::  FIRST_END_POINT,
     +          CAPACITY_PLANNING_ACTIVE, 
     +          OBJECTIVE_FUNCTION_ACTIVE,
     +          EMISSIONS_CREDITS, 
     +          ENVIR_DATA_READ, 
     +          ANNUAL_TRANSACTION_LOADS




!     TYPE DECLARATION FOR /SCREEN/

      LOGICAL (kind=4) ::  ESCAPE
      LOGICAL (kind=1) ::  DETAILED_AN_DECOMP_REPORT,
     +          DETAIL_ANNUAL_DECOMP_REPORT
      LOGICAL (kind=1) ::  LM_FIN_FILE_EXISTS

      INTEGER (kind=4) ::  END_POINT_STARTING_RECORD

      CHARACTER (len=256) ::  FILE_NAME

! MONTHLY FUEL PRICE ITEMS ADDED 9/22/92
!
      LOGICAL (kind=1) ::  FUEL_PRICE_DATA_AVAILABLE
      CHARACTER (len=2) ::  LAST_FUEL_PRICE_OL_TYPE

!
! STUFF 11/11/93
!
      INTEGER (kind=4) ::  ERROR,DEALLOCATE_FUEL_INVENTORY_ID
!
! PARAMETER SECTION
!  COMMON BLOCK FOR ERROR MESSAGES
      LOGICAL (kind=1) ::  MESSAGE,DEBUG_ON
      INTEGER (kind=2) ::  INVERSE_VIDEO_COM
      LOGICAL (kind=1) ::  USER_TERMINATED
      COMMON /ERRORS/ INVERSE_VIDEO_COM,MESSAGE,DEBUG_ON,USER_TERMINATED
!  BEGINNING OF THE COMMON BLOCK SECTION FOR THE PRODUCTION MODULE
!
! VARIABLES FOR THE DETAILED REPORTS
!
!

!
! END COMMON BLOCK SECTION FOR THE PRODUCTION MODULES
!
! NEW VARIABLES ADDED BY M.S.G. JULY 29, 1993 -

      LOGICAL (kind=1) ::  VOID_LOGICAL,RUN_SPECS_OBJECT,
     +          DETAILED_REPORTS_OBJECT, 
     +          READ_SCENARIO_MAKER_DATA, 
     +          CLOSE_RUN_SPECS_FILE, 
     +          RETURN_RUN_SPEC_SWITCHES, 
     +          CLOSE_DETAILED_REPORTS_FILE, 
     +          SET_END_OF_STUDY_FLAG_TRUE, 
     +          SET_END_OF_PLAN_FLAG_FALSE, 
     +          CL_RESET_PRICES, 
     +          EL_RESET_PRICES, 
     +          DSM_RESET_OPTIONS, 
     +          AN_DECOMP_REPORT_HEADER, 
     +          READ_PRICE_FEEDBACK_DATA, 
     +          ANN_DECOMP_OPTIONS_AVAILABLE, 
     +          UPDATE_PRODUCTION_PARAMETERS, 
     +          READ_TRANS_GROUPS_DATA, 
     +          RETURN_DETAILED_REPORTS, 
     +          VOID_LOG1,READ_DAY_TYPE_DATA, 
     +          READ_ESCALATION_FILE
      LOGICAL (kind=4) ::  PRODP_FILE_EXISTS
      LOGICAL (kind=4) ::  PRODUCTION_PARAMETERS_OBJECT
      LOGICAL (kind=1) ::  END_OF_STUDY_FLAG,
     +          END_OF_PLAN_FLAG,LAHEY_LF95
      INTEGER (kind=2) ::  RUN_YEARS
      CHARACTER (len=5) ::  SCENAME, LD_FIN_FIL
      CHARACTER (len=4) ::  END_POINT_STR
      LOGICAL (kind=1) ::  DEALLOCATE_TRANS_OUTAGE_DATA, 
     +          DEALLOCATE_ANNUAL_TRANS_LOADS
      CHARACTER (LEN=1) :: UTILITY_TYPE_CODE,UTILITY_TYPE
	  logical :: j30
      logical :: jump_to_breakpoint

      if (file_trace_msg==0) then
        file_trace_msg=open_trace(
     + "MIDAS_ANN_DECOMP_SIMULATION.trace", rq_mads)
      end if
      
      ENDPOINTS_RUN = 0
      START_YEAR = 1
      STARTING_END_POINT = 1
      YEARS_TO_RUN = RUN_YEARS()
      LAST_YEAR = YEARS_TO_RUN
      UTILITY_TYPE_CODE = UTILITY_TYPE()
      TESTING_PLAN = .FALSE.


! IF CAPACITY PLANNING IS ACTIVE READ THE OBJECTIVE FUNCTION
      call write_trace_message(file_trace_msg, 
     + "msgandec.txt")
      call write_trace_message(file_trace_msg, 
     + "Tracing MIDAS_ANN_DECOMP_SIMULATION.")
     
      call write_trace_int2(file_trace_msg, "END_POINT", gc_end_point)
      call write_trace_int2(file_trace_msg, "STARTING_END_POINT",
     + STARTING_END_POINT)
     
      FIRST_END_POINT = gc_end_point == STARTING_END_POINT
      
      call write_trace_bool1(file_trace_msg, "FIRST_END_POINT", 
     + FIRST_END_POINT)
      call write_trace_bool1(file_trace_msg, 
     + "OBJECTIVE_FUNCTION_ACTIVE",OBJECTIVE_FUNCTION_ACTIVE)
     
      call write_trace_message(file_trace_msg, 
     + "calling POSTFIX_NOTATION_OBJECT")
      CALL POSTFIX_NOTATION_OBJECT(OBJECTIVE_FUNCTION_ACTIVE)

      call write_trace_message(file_trace_msg, 
     + "calling ANN_DECOMP_SCREEN")
      CALL ANN_DECOMP_SCREEN
      
      call write_trace_message(file_trace_msg, "calling DISPLAY_TIME")
      CALL DISPLAY_TIME
      call write_trace_message(file_trace_msg, 
     + "calling DISPLAY_FORECAST_TYPE")
      CALL DISPLAY_FORECAST_TYPE
      
      call write_trace_message(file_trace_msg, 
     + "calling SYSTEM_BASED_FORECAST")
      SYSTEM_FORECAST = SYSTEM_BASED_FORECAST()
      call write_trace_bool1(file_trace_msg, "SYSTEM_FORECAST", 
     + SYSTEM_FORECAST)
     

   15 CALL SET_NEW_ASSETS_INACTIVE
      call write_trace_message(file_trace_msg, 
     + "Called SET_NEW_ASSETS_INACTIVE after label 15.")
      call write_trace_message(file_trace_msg, 
     + "calling INIT_ENDPT_SWITCHES")
      CALL INIT_ENDPT_SWITCHES

      END_POINT_STARTING_RECORD = SAVE_END_POINT_STARTING_RECORD()
      call write_trace_int4(file_trace_msg, 
     + "END_POINT_STARTING_RECORD", END_POINT_STARTING_RECORD)
     
      RUN_FUTURE_ASSETS = .FALSE.
      IF(FIRST_END_POINT) THEN
         FILE_NAME = trim(OUTPUT_DIRECTORY())//"OL*.BIN"
         CALL ERASEWC(FILE_NAME)
         CALL BC_RESET
         CALL STARTOVL(YEARS_TO_RUN,START_YEAR,gc_end_point,
     +               ENDPOINTS_RUN,jump_to_breakpoint, 
     +               RUN_FUTURE_ASSETS)
         if (jump_to_breakpoint) then
            goto 30
         end if
         LAST_FUEL_PRICE_OL_TYPE = 'OL'
         FIRST_END_POINT = .FALSE.
      ELSEIF(.NOT. END_OF_STUDY_FLAG()) THEN
         CALL CLOSE_FORECAST_DATA_FILES
         FILE_NAME = trim(OUTPUT_DIRECTORY())//"OL*.BIN"
         CALL ERASEWC(FILE_NAME)
         CALL BC_RESET
         CALL ANN_DECOMP_SCREEN
         CALL DISPLAY_TIME
         CALL DISPLAY_FORECAST_TYPE
		 ! Replaced asterisk position with j30 boolean. Alternate
		 ! returns aren't supported in F90. Using them
		 ! makes program structure hard to follow and is never
		 ! a good thing to do.
         CALL GETOVLS(YEARS_TO_RUN,START_YEAR,gc_end_point,
     +              ENDPOINTS_RUN,j30, 
     +              RUN_FUTURE_ASSETS)
      ENDIF
      if(j30) then
		goto 30
	  end if
! OPEN UP THE ASSET VECTOR FILE ONCE FOR EACH ENDPOINT AND CLOSE IT AT
! THE END OF EACH ENDPOINT 3/28/95

      CALL MANAGE_ASSET_ALLOCATIONS(.TRUE.)
      VOID_LOGICAL = READ_SCENARIO_MAKER_DATA()
      CALL PROCESS_CAPITAL_RATES_FILE
      CALL OPEN_ASSET_VECTOR_FILE(82)

      CALL RESET_PROJECTS_IN_PROCESS_RECS
      CALL INIT_ASSET_CLASS_INFO
      CALL ZERO_ASSET_CLASS_ARRAYS
      CALL READ_CLASS_INITIALIZATION_FILE

         CALL FINASSET(.FALSE.)

      WRITE(SCREEN_MESSAGES,"(I4)") gc_end_point
      CALL MG_LOCATE_WRITE(25,63,trim(SCREEN_MESSAGES),
     +     ALL_VERSIONS,0)
      CALL LOCATEW(7,0,64)
      CALL PRINTW(7,END_POINT_STR)
      ANN_DECOMP_OPTIONS_AVAILABLE = ANNUAL_DECOMPOSITION_START_UP() > 0
      CALL STORE_END_POINT(gc_end_point)

      VOID_LOGICAL = READ_ESCALATION_FILE()
      CALL OPEN_VAR_UNITS_FILE
      SIMULATION_YEARS = RUN_YEARS()
      LM_FIN_FILE_EXISTS = INDEX(LD_FIN_FIL(),'NONE') == 0

! SET-UP MONTHLY FUEL PRICE FILE FOR THIS END POINT
!
      CALL BUILD_FUEL_PRICE_INFO(FUEL_PRICE_DATA_AVAILABLE)

      IF(SYSTEM_BASED_FORECAST()) THEN

         CALL SF_GET_PEAKS
      ELSEIF(.NOT. NERC_REGION_BASED_FORECAST()) THEN
         CALL CF_GET_PEAKS
      ENDIF

      PRODP_FILE_EXISTS = PRODUCTION_PARAMETERS_OBJECT(INT(472,2))
      VOID_LOGICAL = UPDATE_PRODUCTION_PARAMETERS(INT(1,2))

      VOID_LOGICAL = READ_TRANS_GROUPS_DATA()

! NEED TO INITIALIZE THE FORECAST PEAKS AND DSM PROGRAMS
! 3/28/94. GAT. MOVED FOR HISTORICAL PEAK HOUR IN MDSLDAP.

      CALL INIT_LAST_REFERENCE_LOADS
      IF(.NOT. NERC_REGION_BASED_FORECAST()) CALL UPDATE_REFERENCE_LOADS
!
      CALL MDSLDCAP
      CALL DISPLAY_TIME
      CALL OPEN_NEWPLT_FILE(INT(13,2))
      HYDRO_UNITS = EL_UNITS_READ()
      VOID_LOG1 = READ_DAY_TYPE_DATA()
      call set_nunits(CL_UNITS_READ())
      NUMBER_OF_CONTRACTS = CONTRACTS_READ()
      EXPANSION_UNITS = READ_CAPACITY_OPTIONS_FILE()
      RATIOS_FOUND = READ_CAP_RATIOS_FILE()
      IF(EXPANSION_UNITS < 1 .OR. .NOT. RATIOS_FOUND) THEN
! NEED EXIT ROUTINE
      ENDIF
      HARD_WIRED_UNITS_ADDED = ADD_ALL_HARD_WIRED_UNITS()

      !Open the Detailed Reports file
      VOID_LOGICAL = DETAILED_REPORTS_OBJECT()
      !Opens Price Feedback
      VOID_LOGICAL = READ_PRICE_FEEDBACK_DATA()
      CURRENT_PLANNING_YEAR = 0

      CALL DISPLAY_TIME


! DETAILED EXPANSION REPORTS

      DETAILED_AN_DECOMP_REPORT = DETAIL_ANNUAL_DECOMP_REPORT()

C THIS IS TEMPORARY CODE UNTIL THE EXTENSION PERIOD IS > 30


      LAST_PLANNING_YEAR = LAST_YEAR
      DOWHILE (CURRENT_PLANNING_YEAR <= LAST_PLANNING_YEAR)
         CURRENT_PLANNING_YEAR = CURRENT_PLANNING_YEAR + 1
         IF(CURRENT_PLANNING_YEAR <= LAST_PLANNING_YEAR .AND.
     +                                ANN_DECOMP_OPTIONS_AVAILABLE) THEN
            TESTING_PLAN = .TRUE.

            WRITE(SCREEN_MESSAGES,"(I4/)")
     +           get_BASE_YEAR() + CURRENT_PLANNING_YEAR
            CALL MG_LOCATE_WRITE(3,24,trim(SCREEN_MESSAGES),
     +                                                   ALL_VERSIONS,0)
            LAST_LOOP_YEAR = CURRENT_PLANNING_YEAR + 
     + get_EXTENSION_PERIOD()

            VOID_LOGICAL =RETURN_DETAILED_REPORTS(CURRENT_PLANNING_YEAR)

            VOID_LOGICAL =
     +               UPDATE_PRODUCTION_PARAMETERS(CURRENT_PLANNING_YEAR)

            IF(CHECK_RESOURCE_NEED(CURRENT_PLANNING_YEAR)) then
                CYCLE
            end if
            IF(SET_ANNUAL_DECOMP_OPTIONS(CURRENT_PLANNING_YEAR)) then
                CYCLE
            end if
         ELSE
            CURRENT_PLANNING_YEAR = LAST_PLANNING_YEAR + 1
            TESTING_PLAN = .FALSE.

         LAST_LOOP_YEAR = LAST_PLANNING_YEAR + get_EXTENSION_PERIOD()

            CALL MG_CLEAR_LINE_WRITE(3,9,74,
     +           'Year            Simulating Optimum Plan', 
     +                                      ALL_VERSIONS,0)
         ENDIF
         PLAN_NUMBER = 0
         LAST_OPTION = .FALSE.
         DOWHILE (.NOT. LAST_OPTION)
            CALL SET_OBJECT_FUNC_ACCUMULATORS

! NEED TO SET THE OBJECTIVE FUNCTION ACCUMULATOR TO ZERO

            HYDRO_UNITS = RESET_EL_UNITS()
            VOID_LOGICAL = EL_RESET_PRICES()
            call set_NUNITS(RESET_CL_UNITS())
            VOID_LOGICAL = CL_RESET_PRICES()
            VOID_LOGICAL = CONTRACT_RESET_OPTIONS()
            VOID_LOGICAL = DSM_RESET_OPTIONS()
            CALL RESET_AN_DECOMP_FINAN_OPTIONS
            IF(CURRENT_PLANNING_YEAR <= LAST_PLANNING_YEAR) THEN
               TESTING_PLAN = .TRUE.
               CALL SET_FIRST_FINANCIAL_ADD_TRUE
               LAST_OPTION =
     +                ANNUAL_DECOMPOSITION_OPTION(CURRENT_PLANNING_YEAR)
               IF(LAST_OPTION) CYCLE
               SAVE_RESULTS = .FALSE.
               PLAN_NUMBER = PLAN_NUMBER + 1

               WRITE(SCREEN_MESSAGES,"(I4/)") PLAN_NUMBER
               CALL MG_LOCATE_WRITE(3,65,trim(SCREEN_MESSAGES),
     +                                                   ALL_VERSIONS,0)
            ELSE
               TESTING_PLAN = .FALSE.
               LAST_OPTION = .TRUE.
               SAVE_RESULTS = .TRUE.
            ENDIF
            CUM_ANN_EMIS = 0.
            CALL DISPLAY_TIME

! THE FOLLOWING READS THE RESOURCE DATA

            CALL ZERO_ASSET_CLASS_ARRAYS
            CALL READ_CLASS_INITIALIZATION_FILE
            YEAR_LOOP = START_YEAR
            DOWHILE (YEAR_LOOP <= LAST_LOOP_YEAR)
               globecom_year = YEAR_LOOP

             WRITE(SCREEN_MESSAGES,"(I4/)") get_BASE_YEAR() + YEAR_LOOP
               CALL MG_LOCATE_WRITE(4,9,trim(SCREEN_MESSAGES),
     +                                                  ALL_VERSIONS,0)
               WRITE(FORECAST_YEAR,100)  'Planning Year: ',
     +          get_BASE_YEAR()+YEAR_LOOP

               VOID_LOGICAL = RETURN_DETAILED_REPORTS(YEAR_LOOP)

               VOID_LOGICAL = UPDATE_PRODUCTION_PARAMETERS(YEAR_LOOP)

               VOID_INT2 = SET_ESCALATION_VECTOR_STRUCTURE(YEAR_LOOP)

               IF(YEAR_LOOP <=
     +              MIN(CURRENT_PLANNING_YEAR,LAST_PLANNING_YEAR)) THEN

                  CALL WRITE_INVERSE_VIDEO(5,"Load",3,13)
                  
                  if (globecom_year==0) then
                call end_program("msgandec:0001 - globecom_year " //
     +                   "is zero.")
                  else
                    globecom_year=globecom_year ! Debug
                  end if

                  CALL LOAD_ANALYSIS_OBJECT

!     USED TO ACCUMULATE LOADS FOR USE IN TRANSACT
!     MOVED 7/17/98. GAT. FOR PRE-SCHEDULED MAINTENANCE IN TRANSACT.
!     MOVED TO MSGNORM/MSGANDEC. 10/20/98. GAT. FOR CAPACITY PLANNING.

                  VOID_LOGICAL = ANNUAL_TRANSACTION_LOADS(globecom_year)

                  IF(YEAR_LOOP == 1) THEN
                     WEEKLY_HYDRO_ACTIVE =
     +                                   MANAGE_WEEKLY_HYDRO_FORECASTS()
!
                  ENDIF
                  CALL WRITE_NORMAL_VIDEO(5,"Load",3,13)
                  CALL DISPLAY_TIME
                  CLOSE(29)
               ENDIF

! READ THE ENVIRONMENTAL FILE
               call write_trace_message(file_trace_msg, 
     +           "Calling envir_data_read()")
                
               VOID_LOGICAL = ENVIR_DATA_READ()

                IF(.NOT. LAHEY_LF95()) then
                    CALL DISPLAY_PROC_METHOD
                end if
               CALL WRITE_INVERSE_VIDEO(5,"Production",3,19)
               PRICE_PLANNING_FIRST_PASS = .FALSE.
               SYSTEM_CAPACITY = PROCOST(PRICE_PLANNING_FIRST_PASS)
               CALL WRITE_NORMAL_VIDEO(5,"Production",3,19)

               CALL DISPLAY_TIME

! EMISSIONS

               VOID_LOGICAL = EMISSIONS_CREDITS()

! FINANCIAL

               CALL WRITE_INVERSE_VIDEO(5,"Financial",3,31)
               IF(YEAR_LOOP == START_YEAR) THEN
                  CALL READ_CLASS_INITIALIZATION_FILE

                  CALL FUASST(.FALSE.,.TRUE.)

                  CALL ZERO_DSM_RESULTS


               ENDIF
               CALL DISPLAY_TIME
               IF(LM_FIN_FILE_EXISTS) CALL DSM_FINANCIAL_COST
               IF(UTILITY_TYPE_CODE == PALO_ALTO) THEN

               ELSE
                  CALL FINANCE(YEAR_LOOP,SAVE_RESULTS,LAST_LOOP_YEAR)
               ENDIF
               CALL WRITE_NORMAL_VIDEO(5,"Financial",3,31)
               CALL DISPLAY_TIME
!           ENDIF
               CALL WRITE_INVERSE_VIDEO(5,"Rates",3,42)
               CALL COST_OF_SERVICE
               CALL WRITE_NORMAL_VIDEO(5,"Rates",3,42)
               CALL DISPLAY_TIME

! OUTPUT REPORTS

               IF(CURRENT_PLANNING_YEAR <= LAST_PLANNING_YEAR) THEN
                  CALL OUTPUT_AN_DECOMP(int(YEAR_LOOP,2),'A',
     +                      logical(DETAILED_AN_DECOMP_REPORT,4),
     + int(CURRENT_PLANNING_YEAR+get_BASE_YEAR(),2),int(PLAN_NUMBER,2))
               ELSE
                 ! debugmod:  TODO: Added last argument to OUTPUT
                 ! routine. It could be false or true. I chose false.
                 ! (John)
       CALL OUTPUT(int(YEAR_LOOP,2),logical(SAVE_RESULTS,4), 
     +  logical(.false.,1))
               ENDIF

               OBJECTIVE_FUNCTION_VALUE =
     +                ANN_DECOMP_OBJECTIVE_FUNCTION(get_BASE_YEAR()+ 
     +   YEAR_LOOP)
               IF(END_OF_PLAN_FLAG()) EXIT

               CALL DISPLAY_TIME
               YEAR_LOOP = YEAR_LOOP + 1
            ENDDO  !WHILE YEAR LOOP

            VOID_LOGICAL = SET_END_OF_PLAN_FLAG_FALSE()
!            ERROR = DEALLOCATE_FUEL_INVENTORY_ID()
            IF(YEAR_LOOP > LAST_LOOP_YEAR .AND.
     +                 CURRENT_PLANNING_YEAR <= LAST_PLANNING_YEAR) THEN
               WORST_OBJECT_FUNCTION_VALUE = ANN_DECOMP_SAVE_PLAN()
            ENDIF
! CLEAN UP NEEDED ? IF AUTO_OPTIM NOT ACTIVE (NORMAL CLEAN UP)  THIS
!  NEEDS TO REVIEW BECAUSE IT IS 640k AND FILES = DEPENDENT
!
            CALL CLOSE_FINANCIAL_PARAMETER_FILE

            IF(FUEL_PRICE_DATA_AVAILABLE) CLOSE(666) !FUEL PRICE FILE
            INQUIRE(UNIT=50,NAME=U50_FILE)
            CLOSE(50,IOSTAT=IOS)

            INQUIRE(UNIT=84,NAME=U50_FILE)
            CLOSE(84)
            CLOSE(43,IOSTAT=IOS) ! ENVIRONMENTAL DATA FILE
            CLOSE(111,IOSTAT=IOS) ! FUEL INVENTORY FILE
            CLOSE(112,IOSTAT=IOS) ! FUEL INVENTORY FILE
            CLOSE(3458,IOSTAT=IOS) ! REVENUE FORECAST

         ENDDO ! OPTION LOOP

! ALL OPTIONS TESTED FOR CURRENT YEAR SAVE BEST ONE

         IF(CURRENT_PLANNING_YEAR > LAST_PLANNING_YEAR) THEN
            CALL CLOSE_FUTURE_ASSET_FILE
         ELSE
            BEST_PLAN_OBJECT_FUNCTION_VALUE =
     +           ANN_DECOMP_PICK_BEST_PLAN(CURRENT_PLANNING_YEAR)
         ENDIF
      ENDDO ! PLANNING YEAR LOOP
      CLOSE(900,IOSTAT=IOS) ! SYSTEM FORECAST
      CALL CLOSE_FINANCIAL_OPTIONS
      CALL CLOSE_ASSET_VECTOR_FILE
      CALL CLOSE_CLASS_RUN_SWITCH_FILE
      CALL CLOSE_CAPITAL_RATES_VECTOR_FILE
      VOID_LOGICAL = CLOSE_DETAILED_REPORTS_FILE()

! RECOVERY MEMORY AT END OF ENDPOINT 7/24/00 MSG

      VOID_LOGICAL = DEALLOCATE_TRANS_OUTAGE_DATA()
      VOID_LOGICAL = DEALLOCATE_ANNUAL_TRANS_LOADS()
      ENDPOINTS_RUN = ENDPOINTS_RUN + 1
      CALL CLOSE_NEWPLT_FILE()
      IF(.NOT. END_OF_STUDY_FLAG()) GOTO 15
   30 CALL CLOSE_FILES(ENDPOINTS_RUN,get_BASE_YEAR()+LAST_LOOP_YEAR)
      ADDITIONS_BY = EL_PLANNING_ADDITIONS()
      ADDITIONS_BY = CL_PLANNING_ADDITIONS()
      FIRST_END_POINT = .FALSE.
      RETURN
  100 FORMAT(A,I4)
      END subroutine MIDAS_ANN_DECOMP_SIMULATION

