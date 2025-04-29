!     CAP_OBJT.FOR
!     Copyright(R) M.S.Gerber & Associates 2000
!
!     Created: 6/26/2003 5:17:47 PM
!     Author : GREG TURK


!    CAPACITY PLANNING OBJECT FOR MIDAS GOLD 3A
!    COPYRIGHT (R) 1993 M.S. GERBER & ASSOCIATES, INC
!    ALL RIGHTS RESERVED
      SUBROUTINE CAPACITY_PLANNING_OBJECT(NUNITS)
!
      use SpinDriftLib
      USE CLA_OBJT_ARRAYS
      USE GRX_PLANNING_ROUTINES
      use mod_base_year
      use SpinDriftLib
      use capacity_arrays
      use expansion
      use prod_arrays_dimensions
      use sizecom
      use globecom
      use grxmodules
      use end_routine
      use debugtrace
      use program_state
      
      implicit none
      
      

      CHARACTER (len=2) ::  CAPACITY_PLANNING_METHOD
      
      CHARACTER (len=2) ::  PLANNING_TRIGGER
      CHARACTER (len=2) ::  GREEN_MRX_METHOD
      LOGICAL (kind=1) ::  RATIOS_FOUND
      LOGICAL (kind=1) ::  READ_CAP_RATIOS_FILE
      LOGICAL (kind=1) ::  READ_ICAP_FILE ! External
      LOGICAL (kind=1) ::  CANT_DO_MIX_RATIOS=.FALSE.
      LOGICAL (kind=1) ::  GET_CO2_RETIREMENTS_LOGIC
      LOGICAL (kind=1) ::  CO2_RETROFIT_LOGIC_ACTIVE
      LOGICAL (kind=1) ::  GRX_MX_INIT_NOT_SET=.TRUE.
      LOGICAL (kind=1) ::  TEMP_L1
      INTEGER (kind=2) ::   EXPANSION_UNITS,READ_CAPACITY_OPTIONS_FILE
      INTEGER (kind=2) ::   HARD_WIRED_UNITS_ADDED
      INTEGER (kind=2) ::  ADD_ALL_HARD_WIRED_UNITS
      INTEGER (kind=2) ::  VOID_INT2
      INTEGER (kind=2) ::  START_STUDY_ENDPOINT
      INTEGER (kind=2) ::  UPDATE_CL_SCREEN_COSTS
      INTEGER (kind=2) ::  NUNITS
      CHARACTER (len=1) ::   PLANNING_DECISIONS,TEMP_CHR
      logical :: gwmp
      
      character (len=2) :: cap_pln_meth

      integer :: file_trace_cp=0
      

      
      if(file_trace_cp==0) then
        file_trace_cp=open_trace("CAPACITY_PLANNING_trace.trace", rq_co_cp)
      end if
      




!
! SET-UP SECTION FOR GETTING INFORMATION
!
      
      INSTALLED_POINTER = 0
      call write_trace_int2(file_trace_cp, "NUNITS", nunits)
      call write_trace_int2(file_trace_cp, "1. YEAR", globecom_YEAR)
      
        
      EXPANSION_UNIT_LOCATION(NUNITS+1:) = -1

      CALL OPEN_NEWPLT_FILE(INT(13,2))
      call write_trace_string(file_trace_cp, "capacity_planning_method", &
        CAPACITY_PLANNING_METHOD())
       
      IF(globecom_YEAR == 1 .AND. GRX_ITERATIONS == 0) THEN
         GRX_MX_INIT_NOT_SET = .TRUE.
         EXPANSION_UNITS = READ_CAPACITY_OPTIONS_FILE()
         RATIOS_FOUND = READ_CAP_RATIOS_FILE()
         IF(EXPANSION_UNITS < 1 .OR. .NOT. RATIOS_FOUND) THEN
            CANT_DO_MIX_RATIOS = .TRUE.
         ELSE
            CANT_DO_MIX_RATIOS = .FALSE.
         ENDIF
         

! 03/16/05. OUT FOR BURESH.
         RATIOS_FOUND = READ_ICAP_FILE()
         cap_pln_meth=CAPACITY_PLANNING_METHOD()
         ! Left gets here on years 1/2, right on 1 only.
         IF(CAPACITY_PLANNING_METHOD() /= 'NE' .AND. &
                CAPACITY_PLANNING_METHOD() /= 'PR') then
                     HARD_WIRED_UNITS_ADDED = ADD_ALL_HARD_WIRED_UNITS()
         end if
!
      ENDIF

      IF(CANT_DO_MIX_RATIOS .OR. globecom_year > get_globecom_study_period()) THEN
         CALL CLOSE_NEWPLT_FILE()
         IF(.NOT.(CAPACITY_PLANNING_METHOD() == 'MX'  .AND. &
                              GREEN_MRX_METHOD() == 'GX')) then
                call write_trace_message(file_trace_cp, "3. RETURN")
             RETURN
         end if
      endif

      call write_trace_string(file_trace_cp, "CAPACITY_PLANNING_METHOD", &
        CAPACITY_PLANNING_METHOD())


      IF(CAPACITY_PLANNING_METHOD() == 'SC') THEN
      
         CALL MG_LOCATE_WRITE(10,9,'Scren',ALL_VERSIONS,0)

         IF(globecom_YEAR == 1) then
       call write_trace_message(file_trace_cp, "Calling init screening")
            CALL INIT_SCREENING_OBJECT
         end if
      call write_trace_int2(file_trace_cp, "1. Calling annual_screen for year", &
        globecom_year)
         CALL ANNUAL_SCREENING(globecom_YEAR)
         call write_trace_string(file_trace_cp, "1. cap plan meth", &
            CAPACITY_PLANNING_METHOD())
         call write_trace_string(file_trace_cp, "grrn. mrx meth.", &
            GREEN_MRX_METHOD())
            
      ELSEIF(CAPACITY_PLANNING_METHOD() == 'MX'  .AND. &
                                        GREEN_MRX_METHOD() == 'GX') THEN
         CALL MG_LOCATE_WRITE(10,9,'GRX  ',ALL_VERSIONS,0)
         gwmp=grx_with_mrx_planning()

         IF(gwmp) THEN
         call write_trace_bool(file_trace_cp, "GRX (gwmp)", gwmp)

            write(9,*) "Calling GRX with MRX Planning",gc_end_point

            IF(GRX_MX_INIT_NOT_SET) then
                CALL INIT_SCREENING_OBJECT
            end if

            CALL ANNU_GRX_WITHOUT_CAP_ADDITS(globecom_YEAR)

            GRX_MX_INIT_NOT_SET = .FALSE.
            
            if(globecom_YEAR==0) then
               call end_program("cap_objt:0061 - year must be nonzero.")
            end if
            call write_trace_message(file_trace_cp, &
                "ANNUAL_MARKET_OPTION_COST:1")
          call write_trace_int2(file_trace_cp, "2. YEAR", globecom_YEAR)
            call write_trace_message(file_trace_cp, &
                "1. Calling ANNUAL_MARKET_OPTION_COST")

            CALL ANNUAL_MARKET_OPTION_COST(globecom_YEAR)

         ELSE

            IF(GRX_MX_INIT_NOT_SET) then
                CALL INIT_MRX_GREEN_OBJECT
            end if
            write(9,*) "Calling GRX Planning",gc_end_point
            CALL ANNUAL_MRX_GREEN(globecom_YEAR)
            GRX_MX_INIT_NOT_SET = .FALSE.
         ENDIF
      ELSEIF(CAPACITY_PLANNING_METHOD() == 'MX') THEN

         CALL MG_LOCATE_WRITE(10,9,'MRX  ',ALL_VERSIONS,0)
         IF(globecom_YEAR == 1) then
            CALL INIT_SCREENING_OBJECT
         end if

            write(9,*) "Calling MRX Planning",gc_end_point
            
            
            call write_trace_message(file_trace_cp, &
                "ANNUAL_MARKET_OPTION_COST:2")
        call write_trace_int2(file_trace_cp, "3. YEAR", globecom_YEAR)
            call write_trace_message(file_trace_cp, &
                "2. Calling ANNUAL_MARKET_OPTION_COST")
         CALL ANNUAL_MARKET_OPTION_COST(globecom_YEAR)
      ELSEIF(CAPACITY_PLANNING_METHOD() == 'OP') THEN
         CALL MG_LOCATE_WRITE(10,9,'OPTIM',ALL_VERSIONS,0)
         CALL AUTO_OPTIM
      ELSEIF(CAPACITY_PLANNING_METHOD() == 'RE') THEN
         CALL MG_LOCATE_WRITE(10,9,'R Mrg',ALL_VERSIONS,0)
         CALL MIX_RATIOS
      ELSEIF(CAPACITY_PLANNING_METHOD() == 'MI') THEN
         CALL MG_LOCATE_WRITE(10,9,'Mix  ',ALL_VERSIONS,0)
         CALL MIX_RATIOS
      ELSEIF(CAPACITY_PLANNING_METHOD() == 'SO') THEN
         CALL MG_LOCATE_WRITE(10,9,'Sort ',ALL_VERSIONS,0)
         CALL MIX_RATIOS
      ELSEIF(CAPACITY_PLANNING_METHOD() == 'PR') THEN
         CALL MG_LOCATE_WRITE(10,9,'PSort',ALL_VERSIONS,0)
         CALL MIX_RATIOS
      ELSEIF(CAPACITY_PLANNING_METHOD() == 'TR') THEN
         CALL MG_LOCATE_WRITE(10,9,'T Act',ALL_VERSIONS,0)
         CALL MIX_RATIOS
      ELSEIF(CAPACITY_PLANNING_METHOD() == 'NE') THEN
         CALL MG_LOCATE_WRITE(10,9,'Next ',ALL_VERSIONS,0)
         IF(globecom_year == 1 .AND. gc_end_point == &
            START_STUDY_ENDPOINT()) THEN
            TEMP_CHR = PLANNING_DECISIONS()
         ENDIF
         CALL NEXT_DECISION_MIX_RATIOS
         CALL SET_NEW_ASSETS_ACTIVE
      ENDIF

      IF(globecom_YEAR> 1 .AND. GRX_ITERATIONS > 0) THEN
         CALL CALC_CM_RESERVE_MARGINS(globecom_YEAR)
      ENDIF
      CALL CLOSE_NEWPLT_FILE
      RETURN
      END SUBROUTINE CAPACITY_PLANNING_OBJECT


      SUBROUTINE COMPUTE_OBJECTIVE_FUNCTION
      use end_routine
      CHARACTER (len=2) ::   CAPACITY_PLANNING_METHOD,PLANNING_METHOD
!
      PLANNING_METHOD = CAPACITY_PLANNING_METHOD()
   ! IF block shortened because parts
      ! of it were empty. Compare with legacy codebase.
      IF(PLANNING_METHOD == 'O') THEN
         CALL AUTO_OPTIM_OBJECTIVE_FUNCTION

      ENDIF
      RETURN
      END


!    AUTOMATION OF THE OPTIM PROCESS
!    COPYRIGHT (C) 1993 M.S. GERBER & ASSOCIATES, INC
!    ALL RIGHTS RESERVED

!
      SUBROUTINE AUTO_OPTIM
      use end_routine
      use SpinDriftLib
      use prod_arrays_dimensions
      use mod_base_year
      use globecom
      use debugtrace

      integer :: file_trace_ao=0
      INTEGER (kind=4) ::   PLAN_POINTR=0 ,LAST_OPTION_POINTR
      REAL, allocatable :: DSM_CAP(:,:)
      
      REAL :: CURRENT_TARGET_RATIO,TARGET_RATIO
      LOGICAL (kind=1) ::   HARD_WIRED_UNITS_IN
      LOGICAL (kind=1) ::   TARGET_MET,CONTINUE_ADDING_UNITS
      INTEGER (kind=2) ::   HARD_WIRED_UNITS_ADDED,ADD_HARD_WIRED_UNIT
      INTEGER (kind=2) ::   CUMULATIVE_UNITS_ADDED
      LOGICAL (kind=1) ::   ADD_OPTIM_UNIT,UNIT_ADDED
      LOGICAL (kind=1) ::   END_OF_STUDY_FLAG,SET_END_OF_STUDY_FLAG_TRUE
      REAL ::  ANN_CAP
      REAL ::  EL_PLANNING_CAPACITY
      REAL ::  CL_PLANNING_CAPACITY
      REAL ::  CT_PLANNING_CAPACITY
      REAL ::  LM_PLANNING_CAPACITY
      REAL ::  ADJUSTMENT_CAPACITY
      REAL ::  PEAK
      REAL ::  CURRENT_CAPACITY_RATIO
      REAL ::  UPDATE_NET_PLANNING_PEAK
      REAL ::  RETURN_RESERVE_MARGIN_RATIO
!
      INTEGER (kind=2) ::   I
      INTEGER (kind=2) ::  ACTIVE_OPTIONS
      INTEGER (kind=2) ::  RETURN_OPTIM_ACTIVE_OPTIONS
      INTEGER (kind=2) ::  POINTR
      INTEGER (kind=2) ::  CURRENT_OPTION_POINTER
      INTEGER (kind=2) ::  ANNUAL_UNITS_FOR_OPTION
      INTEGER (kind=2) ::  YR
      INTEGER (kind=1) ::   STARTING_POINTER,SET_STARTING_POINTER
!
! VARIABLES FOR STORING OBJECTIVE FUNCTION VALUES
!
      LOGICAL (kind=1) ::  END_OF_PLAN_FLAG
      LOGICAL (kind=1) ::  SET_END_OF_PLAN_FLAG_FALSE
      LOGICAL (kind=1) ::  SET_END_OF_PLAN_FLAG_TRUE
      LOGICAL (kind=1) ::  EXPANSION_REPORT
      LOGICAL (kind=1) ::  WRITE_TO_EXPANSION_REPORT
      LOGICAL (kind=1) ::  END_OF_CYCLE
      INTEGER (kind=2) ::   PLAN_IDENTIFIER(:)
      REAL ::  PLAN_OBJECT_FUNCTION_VALUE(:)
      REAL ::  MAX_OBJECT_FUNCTION_VALUE=-999999.
      REAL ::  OBJECTIVE_FUNCTION_VALUE
      INTEGER (kind=4) ::  NUMBER_OF_SAVED_PLANS=0
      INTEGER (kind=4) ::  MAX_NUMBER_OF_PLANS_TO_SAVE=12
      ALLOCATABLE :: PLAN_IDENTIFIER,PLAN_OBJECT_FUNCTION_VALUE
      SAVE PLAN_IDENTIFIER,PLAN_OBJECT_FUNCTION_VALUE
      if (file_trace_ao==0) then
        file_trace_ao=open_trace("auto_optim.trace", rq_co_ao)
      end if
      
      ACTIVE_OPTIONS = RETURN_OPTIM_ACTIVE_OPTIONS()
      IF(ACTIVE_OPTIONS == 0) THEN
         END_OF_STUDY_FLAG = SET_END_OF_STUDY_FLAG_TRUE()
         RETURN
      ENDIF
      ALLOCATE(DSM_CAP(3,get_globecom_study_period()))
      CALL GET_DSM_CAP(DSM_CAP)
!
      END_OF_CYCLE = .FALSE.
      POINTR = CURRENT_OPTION_POINTER(END_OF_CYCLE)
      PLAN_POINTR = PLAN_POINTR + 1
      call write_trace_int2(file_trace_ao, "STUDY_PERIOD", &
        get_globecom_study_period())

      
      DO YR = 1, get_globecom_study_period()
         CURRENT_TARGET_RATIO = TARGET_RATIO(3,YR)
         ANN_CAP =  EL_PLANNING_CAPACITY(3,YR) + &
                    CL_PLANNING_CAPACITY(3,YR) + &
                    CT_PLANNING_CAPACITY(3,YR) + &
                    LM_PLANNING_CAPACITY(YR)   + &
                    ADJUSTMENT_CAPACITY(YR)
         call write_trace_real(file_trace_ao, "2. ANN_CAP", ANN_CAP)
            if(yr==0) then
               call end_program("cap_objt:0041 - year must be nonzero.")
            end if
         PEAK =  UPDATE_NET_PLANNING_PEAK(YR)
!
         CUMULATIVE_UNITS_ADDED = 0
         IF(HARD_WIRED_UNITS_IN(YR)) THEN
            CUMULATIVE_UNITS_ADDED=ADD_HARD_WIRED_UNIT(YR+get_BASE_YEAR(), &
                                                           ANN_CAP,PEAK)
         ENDIF
!
         TARGET_MET = &
               RETURN_RESERVE_MARGIN_RATIO(PEAK,ANN_CAP,YR) &
                                              >= CURRENT_TARGET_RATIO
         IF(TARGET_MET) then
            CYCLE
         end if
         END_OF_CYCLE = .FALSE.
         CONTINUE_ADDING_UNITS = .TRUE.
         STARTING_POINTER = SET_STARTING_POINTER()
         DO WHILE (CONTINUE_ADDING_UNITS)
!
            LAST_OPTION_POINTR = POINTR
            DO I = 1, MAX(ANNUAL_UNITS_FOR_OPTION(POINTR),INT(1,2))
               UNIT_ADDED = ADD_OPTIM_UNIT(POINTR,YR+get_BASE_YEAR(), &
                                                           ANN_CAP,PEAK)
               TARGET_MET = &
                  RETURN_RESERVE_MARGIN_RATIO(PEAK,ANN_CAP,YR) &
                                              >= CURRENT_TARGET_RATIO
               IF(TARGET_MET .OR. .NOT. UNIT_ADDED) then
                EXIT
               end if
               CUMULATIVE_UNITS_ADDED = CUMULATIVE_UNITS_ADDED + 1
            ENDDO
            DO WHILE (LAST_OPTION_POINTR == POINTR)
               POINTR = CURRENT_OPTION_POINTER(END_OF_CYCLE)
               IF(END_OF_CYCLE) then
                EXIT
               end if
            ENDDO
            CONTINUE_ADDING_UNITS = .NOT.(TARGET_MET .OR. END_OF_CYCLE)
         ENDDO
      ENDDO ! YEAR LOOP
      DEALLOCATE(DSM_CAP)
      RETURN

      ENTRY AUTO_OPTIM_OBJECTIVE_FUNCTION

      IF(PLAN_POINTR == 1 .AND. globecom_YEAR == 1) THEN
         ALLOCATE(PLAN_IDENTIFIER(0:MAX_NUMBER_OF_PLANS_TO_SAVE), &
              PLAN_OBJECT_FUNCTION_VALUE(0:MAX_NUMBER_OF_PLANS_TO_SAVE))
         PLAN_IDENTIFIER = -1
         PLAN_OBJECT_FUNCTION_VALUE = 0.
      ENDIF
      IF(globecom_YEAR == 1) THEN
         CALL SET_OBJECT_FUNC_ACCUMULATORS
         END_OF_PLAN_FLAG = SET_END_OF_PLAN_FLAG_FALSE()
         NUMBER_OF_SAVED_PLANS = NUMBER_OF_SAVED_PLANS + 1
      ENDIF
      CALL CALCULATE_OBJECTIVE_FUNCTION(globecom_YEAR+get_BASE_YEAR(), &
                                        OBJECTIVE_FUNCTION_VALUE)
      IF(NUMBER_OF_SAVED_PLANS > MAX_NUMBER_OF_PLANS_TO_SAVE) THEN
         PLAN_IDENTIFIER(0) = PLAN_POINTR
         PLAN_OBJECT_FUNCTION_VALUE(0) = OBJECTIVE_FUNCTION_VALUE
         IF(OBJECTIVE_FUNCTION_VALUE > MAX_OBJECT_FUNCTION_VALUE) THEN
            END_OF_PLAN_FLAG = SET_END_OF_PLAN_FLAG_TRUE()
            RETURN
         ENDIF
      ELSE
         PLAN_IDENTIFIER(NUMBER_OF_SAVED_PLANS) = PLAN_POINTR
         PLAN_OBJECT_FUNCTION_VALUE(NUMBER_OF_SAVED_PLANS) = &
                                                OBJECTIVE_FUNCTION_VALUE
         MAX_OBJECT_FUNCTION_VALUE = MAX(MAX_OBJECT_FUNCTION_VALUE, &
                                               OBJECTIVE_FUNCTION_VALUE)
      ENDIF
      RETURN
      END


! RATIOS FUNCTIONS


      FUNCTION RETURN_OPTIM_ACTIVE_OPTIONS()

      use SpinDriftLib
      use prod_arrays_dimensions
      use globecom
      use end_routine

      LOGICAL (kind=1) ::  ALL_OPTIONS_TESTED=.FALSE.
      LOGICAL (kind=1) ::  SET_END_OF_STUDY_FLAG_TRUE
      LOGICAL (kind=1) ::  R_END_OF_CYCLE
      INTEGER (kind=2) ::   RETURN_OPTIM_ACTIVE_OPTIONS
      INTEGER (kind=2) ::   TOTAL_ACTIVE_OPTIONS
      INTEGER (kind=2) ::  NUMBER_OF_ACTIVE_OPTIONS
      INTEGER (kind=2) ::  I
      INTEGER (kind=2) ::  OPTION_DEPTH
      INTEGER (kind=2) ::  CURRENT_OPTION_POINTER
      INTEGER (kind=2) ::  OPTIM_DEPTH
      INTEGER (kind=2) ::  START_STUDY_ENDPOINT
      INTEGER (kind=1) ::   ACTIVE_OPTIONS(:)
      INTEGER (kind=1) ::  OPTIONS_POINTER
      INTEGER (kind=1) ::  STARTING_POINTER
      INTEGER (kind=1) ::  SET_STARTING_POINTER
      ALLOCATABLE ACTIVE_OPTIONS
      INTEGER (kind=1) ::   I_TRUE
      PARAMETER(I_TRUE = 0)

      SAVE ACTIVE_OPTIONS,OPTIONS_POINTER,OPTION_DEPTH,STARTING_POINTER

      IF(ALL_OPTIONS_TESTED) THEN
         RETURN_OPTIM_ACTIVE_OPTIONS = 0
      ELSE
         OPTIONS_POINTER = 0
         ! total_active_options set here
         TOTAL_ACTIVE_OPTIONS =  NUMBER_OF_ACTIVE_OPTIONS()
         IF(gc_end_point == START_STUDY_ENDPOINT()) THEN
            OPTION_DEPTH = OPTIM_DEPTH()
            ALLOCATE(ACTIVE_OPTIONS(OPTION_DEPTH))
            ACTIVE_OPTIONS = I_TRUE
         ELSE
            DO I = 1, OPTION_DEPTH
               ACTIVE_OPTIONS(I) = ACTIVE_OPTIONS(I) + 1
               IF(ACTIVE_OPTIONS(I) <= TOTAL_ACTIVE_OPTIONS) then
                EXIT
               end if
               ACTIVE_OPTIONS(I) = 1
            ENDDO
            ALL_OPTIONS_TESTED = .TRUE.
            DO I = 1, OPTION_DEPTH
               IF(ACTIVE_OPTIONS(I) == TOTAL_ACTIVE_OPTIONS) then
                    CYCLE
               end if
               ALL_OPTIONS_TESTED = .FALSE.
               EXIT
            ENDDO
            IF(ALL_OPTIONS_TESTED) &
                       ALL_OPTIONS_TESTED = SET_END_OF_STUDY_FLAG_TRUE()
         ENDIF
         RETURN_OPTIM_ACTIVE_OPTIONS = OPTION_DEPTH
      ENDIF
      RETURN
      ENTRY SET_STARTING_POINTER
         STARTING_POINTER = OPTIONS_POINTER
         SET_STARTING_POINTER = STARTING_POINTER
      RETURN
      ENTRY CURRENT_OPTION_POINTER(R_END_OF_CYCLE)
         OPTIONS_POINTER = OPTIONS_POINTER + 1
         IF(OPTIONS_POINTER > OPTION_DEPTH) OPTIONS_POINTER = 1
         IF(STARTING_POINTER == OPTIONS_POINTER) THEN
            R_END_OF_CYCLE = .TRUE.
            OPTIONS_POINTER = OPTIONS_POINTER + 1
            IF(OPTIONS_POINTER > OPTION_DEPTH) OPTIONS_POINTER = 1
            STARTING_POINTER = OPTIONS_POINTER
         ENDIF

         CURRENT_OPTION_POINTER =  ACTIVE_OPTIONS(OPTIONS_POINTER)
      RETURN
      END

      SUBROUTINE MIX_RATIOS
      use miscmod
      use mod_base_year
      use sizecom
      use globecom
      use rangechecker
      use end_routine
      use debugtrace
      implicit none
      integer :: file_trace_mr=0 ! Mix ratios subroutine
      INTEGER (kind=2) ::   POINTER(3)
      INTEGER (kind=2) ::  YR
      INTEGER (kind=2) ::  ICAP
      INTEGER (kind=2) ::  ICAP_POINTER
      INTEGER (kind=2) ::  START_POINTER
      INTEGER (kind=2) ::  POINTR
      INTEGER (kind=2) ::  MAX_ANNUAL_UNITS
      INTEGER (kind=2) ::  ANNUAL_UNITS_FOR_OPTION
      INTEGER (kind=2) ::  L
      INTEGER (kind=2) ::  CUMULATIVE_UNITS_FOR_OPTION
      INTEGER (kind=2) ::  CAP_ORDER(3)
      INTEGER (kind=2) ::  START_YEAR
      INTEGER (kind=2) ::  END_YEAR
      INTEGER (kind=2) ::  POINTER_FOR_NEXT
      INTEGER (kind=2) ::  MO
      INTEGER (kind=2) ::  MONTH
      INTEGER (kind=2) ::  PEAK_MONTH
      REAL ::  ANN_CAP(3)
      REAL ::  EL_PLANNING_CAPACITY
      REAL ::  CL_PLANNING_CAPACITY
      REAL ::  CT_PLANNING_CAPACITY
      REAL ::  ADJUSTMENT_BASE_CAPACITY
      REAL ::  ADJUSTMENT_PEAK_CAPACITY
      REAL ::  DERIV_CAPACITY_PLANNING
      REAL ::  PEAK
      REAL ::  TEMP_PEAK
      REAL ::  UPDATE_NET_PLANNING_PEAK
      REAL ::  RETURN_RESERVE_MARGIN_RATIO
      REAL ::  GET_SCENARIO_RESERVE_MARGIN
      REAL ::  SCENARIO_RESERVE_MARGIN
      REAL ::  GET_SCENARIO_ELECTRIC_DEMAND
      REAL :: DSM_CAP(:,:),FIRST_YEAR_CAPACITY,ADD_THIS_UNIT
      ALLOCATABLE :: DSM_CAP
      REAL (kind=4) ::   TARGET_RATIO,CURRENT_TARGET_RATIO(3)
      CHARACTER (len=2) ::   CAPACITY_PLANNING_METHOD,PLANNING_METHOD
      CHARACTER (len=1) ::   CAP_TYPE(3),MOST_NEEDED_CAP
      LOGICAL (kind=1) ::  RESERVE_MARGIN_PLANNING
      LOGICAL (kind=1) ::  RESORT_RATIOS_WITHIN_YEAR
      LOGICAL (kind=1) ::  FIRST_RESERVE_ADDITION
      LOGICAL (kind=1) ::  UNIT_WAS_ADDED
      LOGICAL (kind=1) ::  EXPANSION_REPORT
      LOGICAL (kind=1) ::  TARGET_NOT_MET
      LOGICAL (kind=1) ::  FOUND_NEXT_ADDITION
      LOGICAL (kind=1) ::  HARD_WIRED_UNITS_IN
      LOGICAL (kind=1) ::  CONTINUE_ADDING_UNITS
      LOGICAL (kind=1) ::  GET_NEXT_ADDITION
      LOGICAL (kind=1) ::  ALWAYS_CYCLE=.FALSE.
!
      CHARACTER (len=1) ::   R_CAP_TYPE
      LOGICAL (kind=1) ::     THRESHOLD_EXCEEDED,FILL_OPTIONS_ACTIVE
      INTEGER (kind=1) ::     NUMBER_OF_THRESHOLD_UNITS_LEFT
      INTEGER (kind=1) ::  THRESHOLD_UNITS
      INTEGER (kind=1) ::  UPDATE_THRESHOLD
      INTEGER (kind=2) ::   NUMBER_OF_ACTIVE_FILL_OPTIONS,R_YEAR
      REAL (kind=4) ::        R_CAPACITY
      REAL (kind=4) ::        INCREMENTAL_CAPACITY,SO_PEAK(30),R_SO_PEAK
!
      INTEGER (kind=2) ::     FILL_POINTR
      INTEGER (kind=2) ::  FILL_UNITS_NEEDED
      INTEGER (kind=2) ::  GET_FILL_UNITS_NEEDED
      INTEGER (kind=2) ::  A_POINTER
      REAL (kind=4) ::        RESERVE_CAPACITY_NEEDED
      REAL (kind=4) ::        FILLER_CAPACITY_NEEDED(3)
!
      INTEGER (kind=2) ::     HARD_WIRED_UNITS_ADDED
      INTEGER (kind=2) ::  ADD_ALL_HARD_WIRED_UNIT
      INTEGER (kind=2) ::  MAX_LEAD_TIME=10
      INTEGER (kind=2) ::  GET_MAX_LEAD_TIME
      INTEGER (kind=2) ::  NUMBER_OF_CAP_TYPES=3
      INTEGER (kind=2) ::  CAP
      INTEGER (kind=2) ::  CURRENT_TYPE
      INTEGER (kind=2) ::   ANNUAL_UNITS_LEFT,REDUCE_ANNUAL_UNITS_LEFT
      INTEGER (kind=2) ::   AVAIALABLE_OPTIONS,RESET_ANNUAL_UNITS_LEFT
      INTEGER (kind=2) ::   CUMULATIVE_UNITS_ADDED
      INTEGER (kind=2) ::   PR_ADD_HARD_WIRED_UNIT
!
      SAVE POINTER,ANN_CAP,SO_PEAK
!
! END OF DATA DECLARATIONS
!
!
      if(file_trace_mr==0) then
        file_trace_mr=open_trace("mix_ratios.trace", rq_mr)
        
      end if
      
      IF(globecom_YEAR == 1) then
        MAX_LEAD_TIME = GET_MAX_LEAD_TIME()
        call write_trace_int2(file_trace_mr, "MAX_LEAD_TIME", &
            MAX_LEAD_TIME)
      end if
      
      PLANNING_METHOD = CAPACITY_PLANNING_METHOD()
      call write_trace_string(file_trace_mr, "PLANNING_METHOD", &
        PLANNING_METHOD)
        
      RESERVE_MARGIN_PLANNING = (PLANNING_METHOD == 'RE' .OR. &
                                 PLANNING_METHOD == 'SO' .OR. &
                                 PLANNING_METHOD == 'PR' .OR. &
                                 PLANNING_METHOD == 'TR' .OR. &
                                 PLANNING_METHOD == 'NE')
      call write_trace_bool1(file_trace_mr, "RESERVE_MARGIN_PLANNING", &
          RESERVE_MARGIN_PLANNING)
          
      RESORT_RATIOS_WITHIN_YEAR = (PLANNING_METHOD == 'SO' .OR. &
                                   PLANNING_METHOD == 'PR' .OR. &
                                   PLANNING_METHOD == 'TR' .OR. &
                                   PLANNING_METHOD == 'NE')

      FILL_OPTIONS_ACTIVE = NUMBER_OF_ACTIVE_FILL_OPTIONS() > 0

      IF(PLANNING_METHOD == 'PR' .OR. PLANNING_METHOD == 'TR')  THEN
         START_YEAR = globecom_YEAR
         END_YEAR = globecom_YEAR
         IF(globecom_YEAR == 1) THEN

            POINTER(1) = 1
            POINTER(2) = 1
            POINTER(3) = 1

         ENDIF
      ELSE
         START_YEAR = 1
         END_YEAR = get_globecom_study_period()

         POINTER(1) = 1
         POINTER(2) = 1
         POINTER(3) = 1

      ENDIF
      ALLOCATE(DSM_CAP(3,get_globecom_study_period()))
      CALL GET_DSM_CAP(DSM_CAP)

      CAP_TYPE(1) = 'B'
      CAP_TYPE(2) = 'C'
      CAP_TYPE(3) = 'P'

      ALWAYS_CYCLE = PLANNING_METHOD == 'SO'.OR. PLANNING_METHOD == 'TR'
      SO_PEAK = 0.0
               ! call check_bounds_r_2s(dsm_cap, "cap_objt:0044", &
            ! "dsm_cap", int(3,2), end_year)
      DO YR = START_YEAR, END_YEAR
         IF(HARD_WIRED_UNITS_IN(YR) .AND. PLANNING_METHOD == 'PR') THEN
       CUMULATIVE_UNITS_ADDED=PR_ADD_HARD_WIRED_UNIT(YR+get_BASE_YEAR())
         ENDIF

         SCENARIO_RESERVE_MARGIN = &
                                GET_SCENARIO_RESERVE_MARGIN(YR,INT(1,2))

         AVAIALABLE_OPTIONS = RESET_ANNUAL_UNITS_LEFT()
         CURRENT_TARGET_RATIO(1) = TARGET_RATIO(1,YR) * &
                                                 SCENARIO_RESERVE_MARGIN
         CURRENT_TARGET_RATIO(2) = TARGET_RATIO(2,YR) * &
                                                 SCENARIO_RESERVE_MARGIN
         CURRENT_TARGET_RATIO(3) = TARGET_RATIO(3,YR) * &
                                                 SCENARIO_RESERVE_MARGIN

         ANN_CAP(1) =  EL_PLANNING_CAPACITY(1,YR) + &
                    CL_PLANNING_CAPACITY(1,YR) + &
                    CT_PLANNING_CAPACITY(1,YR) + &
                    DSM_CAP(1,YR) + &
                    ADJUSTMENT_BASE_CAPACITY(YR)
         ANN_CAP(2) =  EL_PLANNING_CAPACITY(2,YR) + &
                    CL_PLANNING_CAPACITY(2,YR) + &
                    CT_PLANNING_CAPACITY(2,YR) + &
                    DSM_CAP(2,YR) + DSM_CAP(1,YR) + &
                    ADJUSTMENT_BASE_CAPACITY(YR)
         ANN_CAP(3) =  EL_PLANNING_CAPACITY(3,YR) + &
                    CL_PLANNING_CAPACITY(3,YR) + &
                    CT_PLANNING_CAPACITY(3,YR) + &
                    DSM_CAP(3,YR) + DSM_CAP(2,YR) + DSM_CAP(1,YR) + &
                    ADJUSTMENT_BASE_CAPACITY(YR) + &
                    ADJUSTMENT_PEAK_CAPACITY(YR) + &
                    DERIV_CAPACITY_PLANNING(YR)
            if(yr==0) then
               call end_program("cap_objt:0044 - year must be nonzero.")
            end if

         PEAK =  UPDATE_NET_PLANNING_PEAK(YR)
         IF(PLANNING_METHOD == 'SO') THEN
            PEAK = 0.0
            DO MO = 1, 12
                CALL SF_MONTHLY_PEAKS(MO,YR,TEMP_PEAK)
                TEMP_PEAK = TEMP_PEAK * &
                                  GET_SCENARIO_ELECTRIC_DEMAND(YR,MO)
                IF(TEMP_PEAK > PEAK) THEN
                   PEAK = TEMP_PEAK
                   MONTH = MO
                ENDIF
            ENDDO
            SO_PEAK(YR) = PEAK
         ENDIF

         IF(FILL_OPTIONS_ACTIVE) THEN
            CALL RESORT_CAPACITY_ORDER(YR,CURRENT_TARGET_RATIO, &
                                             ANN_CAP,PEAK,CAP_ORDER, &
                                             ICAP_POINTER,ICAP)
!
            INCREMENTAL_CAPACITY = 999.
            FILL_POINTR = 0 ! FOR NOW
!
            DO CAP = 1, NUMBER_OF_CAP_TYPES
               FILLER_CAPACITY_NEEDED(1) = &
                               PEAK*CURRENT_TARGET_RATIO(1) - ANN_CAP(1)
               FILLER_CAPACITY_NEEDED(2) = &
                               PEAK*CURRENT_TARGET_RATIO(2) - ANN_CAP(2)
               FILLER_CAPACITY_NEEDED(3) = &
                               PEAK*CURRENT_TARGET_RATIO(3) - ANN_CAP(3)
!
               IF(FILLER_CAPACITY_NEEDED(3) > 0. .AND. &
                                         INCREMENTAL_CAPACITY > 0.) THEN
!
                  CURRENT_TYPE = CAP_ORDER(CAP)
                  MOST_NEEDED_CAP = CAP_TYPE(CURRENT_TYPE)
                  FILL_POINTR = 0
!
                  DO
                     RESERVE_CAPACITY_NEEDED = &
                             MIN(FILLER_CAPACITY_NEEDED(CURRENT_TYPE), &
                                              FILLER_CAPACITY_NEEDED(3))
                     FILL_UNITS_NEEDED = &
                          GET_FILL_UNITS_NEEDED(FILL_POINTR, &
                                             YR+get_BASE_YEAR(),YR, &
                                              RESERVE_CAPACITY_NEEDED, &
                                                MOST_NEEDED_CAP, &
                                                INCREMENTAL_CAPACITY, &
                                                A_POINTER)
                     IF(FILL_UNITS_NEEDED > 0) THEN
!
                        MAX_ANNUAL_UNITS = &
                          MIN(ANNUAL_UNITS_FOR_OPTION(A_POINTER), &
                                                      FILL_UNITS_NEEDED)

                        FIRST_YEAR_CAPACITY = ADD_THIS_UNIT(A_POINTER, &
                           YR+get_BASE_YEAR())
                           
                        INCREMENTAL_CAPACITY = INCREMENTAL_CAPACITY - &
                                                     FIRST_YEAR_CAPACITY
                        CALL UPDATE_ANN_CAP(ANN_CAP,PEAK, &
                                    MOST_NEEDED_CAP,FIRST_YEAR_CAPACITY)
                        ANNUAL_UNITS_LEFT = &
                                     REDUCE_ANNUAL_UNITS_LEFT(A_POINTER)

                        FILLER_CAPACITY_NEEDED(1) = &
                               PEAK*CURRENT_TARGET_RATIO(1) - ANN_CAP(1)
                        FILLER_CAPACITY_NEEDED(2) = &
                               PEAK*CURRENT_TARGET_RATIO(2) - ANN_CAP(2)
                        FILLER_CAPACITY_NEEDED(3) = &
                               PEAK*CURRENT_TARGET_RATIO(3) - ANN_CAP(3)
                     ELSE
                        EXIT
                     ENDIF ! FILLER UNITS FOR THIS TYPE ARE FOUND
                  ENDDO
               ELSE
                  EXIT
               ENDIF ! RESERVE MARGIN EXCEEDED
            ENDDO ! EACH RESOURCE TYPE
         ENDIF
!
         IF(RESERVE_MARGIN_PLANNING) THEN
            TARGET_NOT_MET = &
               RETURN_RESERVE_MARGIN_RATIO(PEAK,ANN_CAP(3),YR) &
                                              < CURRENT_TARGET_RATIO(3)
            FIRST_RESERVE_ADDITION = .TRUE.
         ELSE
            ICAP = 1
            TARGET_NOT_MET = .TRUE.
            FIRST_RESERVE_ADDITION = .FALSE.
         ENDIF

         CONTINUE_ADDING_UNITS = .TRUE.
         UNIT_WAS_ADDED = .FALSE.


         DO WHILE (TARGET_NOT_MET .AND. CONTINUE_ADDING_UNITS)
            IF( (RESORT_RATIOS_WITHIN_YEAR .AND. UNIT_WAS_ADDED) .OR. &
                                           FIRST_RESERVE_ADDITION) &
                  CALL RESORT_CAPACITY_ORDER(YR, &
                                             CURRENT_TARGET_RATIO, &
                                             ANN_CAP,PEAK,CAP_ORDER, &
                                             ICAP_POINTER,ICAP)

            IF( (ANN_CAP(3)/PEAK >= CURRENT_TARGET_RATIO(3) .AND. &
                                         RESERVE_MARGIN_PLANNING) .OR. &
              ( ANN_CAP(ICAP)/PEAK >= CURRENT_TARGET_RATIO(ICAP) .AND. &
                               .NOT. RESERVE_MARGIN_PLANNING) ) then
            	GOTO 150
            end if
            START_POINTER = POINTER(ICAP)

   70       CONTINUE ! FIND NEXT ADDITION OF SAME LOADING TYPE

            IF(ALWAYS_CYCLE) THEN
               POINTER_FOR_NEXT = POINTER(ICAP)
            ELSE
               POINTER_FOR_NEXT = START_POINTER
            ENDIF
!
            FOUND_NEXT_ADDITION = GET_NEXT_ADDITION(POINTER(ICAP), &
                                                    CAP_TYPE(ICAP), &
                                                YR+get_BASE_YEAR(), &
                                                    POINTER_FOR_NEXT, &
                                                    YR,POINTR)

            IF(FOUND_NEXT_ADDITION) THEN
               UNIT_WAS_ADDED = .TRUE. ! ADD THIS UNIT TYPE TO THE PLAN
!
               IF( .NOT. RESORT_RATIOS_WITHIN_YEAR) THEN
                  MAX_ANNUAL_UNITS = &
                           MAX(ANNUAL_UNITS_FOR_OPTION(POINTR),INT(1,2))
               ELSE
                  MAX_ANNUAL_UNITS = 1
               ENDIF
               call write_trace_int2(file_trace_mr, "L", L)
               call write_trace_int2(file_trace_mr, "MAX_ANNUAL_UNITS", &
                MAX_ANNUAL_UNITS)
                
               DO L = 1 , MAX_ANNUAL_UNITS
                  call write_trace_int2(file_trace_mr, "L", L)
                  FIRST_YEAR_CAPACITY=ADD_THIS_UNIT(POINTR,YR+get_BASE_YEAR())
             call write_trace_real(file_trace_mr, "FIRST_YEAR_CAPACITY",  &
                FIRST_YEAR_CAPACITY)
                call write_trace_reals(file_trace_mr, "ANN_CAP", ANN_CAP)
                call write_trace_int2(file_trace_mr, "ICAP", ICAP)
                
                call write_trace_string(file_trace_mr, "CAP_TYPE", &
                    cap_type(ICAP))
                call write_trace_real(file_trace_mr, "FYC", &
                    FIRST_YEAR_CAPACITY)
                    

                  CALL UPDATE_ANN_CAP(ANN_CAP,PEAK,CAP_TYPE(ICAP), &
                                              FIRST_YEAR_CAPACITY)
                  ANNUAL_UNITS_LEFT = REDUCE_ANNUAL_UNITS_LEFT(POINTR)
!
                  IF((ANN_CAP(ICAP)/PEAK >= &
                                      CURRENT_TARGET_RATIO(ICAP)) .OR. &
                     (RESERVE_MARGIN_PLANNING .AND. &
                            ANN_CAP(3)/PEAK >= &
                                        CURRENT_TARGET_RATIO(3)) ) THEN
                     POINTER(ICAP) = POINTER(ICAP) + 1
                     GOTO 150
                  ENDIF
!
                  IF(CUMULATIVE_UNITS_FOR_OPTION(POINTR) == 0) then
                  	EXIT
                  end if
               ENDDO
!
               POINTER(ICAP) = POINTER(ICAP) + 1
               IF(POINTER(ICAP) /= START_POINTER) GOTO 70
           ELSE
              UNIT_WAS_ADDED = .FALSE. !UNIT WAS NOT FOUND SO DON'T SORT
           ENDIF ! IF FOUND_NEXT_ADDITION
!
!      MOVE-ON TO THE NEXT LOADING TYPE
!
  150       IF(RESERVE_MARGIN_PLANNING) THEN
               ICAP_POINTER = ICAP_POINTER + 1
               TARGET_NOT_MET = CURRENT_TARGET_RATIO(3) > &
                         RETURN_RESERVE_MARGIN_RATIO(PEAK,ANN_CAP(3),YR)
               FIRST_RESERVE_ADDITION = .FALSE.
               IF(TARGET_NOT_MET) THEN
                  IF(ICAP_POINTER > 3) THEN
                     IF(UNIT_WAS_ADDED) THEN
                        UNIT_WAS_ADDED = .FALSE.
                        ICAP_POINTER = 1
                        ICAP = CAP_ORDER(ICAP_POINTER)
                     ELSE
                        CONTINUE_ADDING_UNITS = .FALSE.
                     ENDIF
                  ELSE
                     ICAP = CAP_ORDER(ICAP_POINTER)
                  ENDIF
               ENDIF
            ELSE
               ICAP = ICAP + 1
               TARGET_NOT_MET = ICAP <= 3
            ENDIF
         ENDDO  ! WHILE TARGET NOT MET AND CONTINUE ADDING UNITS
      ENDDO ! YEAR LOOP
      DEALLOCATE(DSM_CAP)
      RETURN


      ENTRY REDUCE_ANN_CAP(R_CAP_TYPE,R_CAPACITY)
      IF(R_CAP_TYPE == 'B') THEN
         ANN_CAP(1) = ANN_CAP(1) - R_CAPACITY
         ANN_CAP(2) = ANN_CAP(2) - R_CAPACITY
         ANN_CAP(3) = ANN_CAP(3) - R_CAPACITY
      ELSEIF(R_CAP_TYPE == 'C') THEN
         ANN_CAP(2) = ANN_CAP(2) - R_CAPACITY
         ANN_CAP(3) = ANN_CAP(3) - R_CAPACITY
      ELSEIF(R_CAP_TYPE == 'P') THEN
         ANN_CAP(3) = ANN_CAP(3) - R_CAPACITY
      ENDIF
      RETURN


      ENTRY GET_SO_PEAK(R_YEAR,R_SO_PEAK) ! 053113

         R_SO_PEAK = SO_PEAK(R_YEAR)
      RETURN
      END


      RECURSIVE SUBROUTINE NEXT_DECISION_MIX_RATIOS
      use end_routine
      use rangechecker
      use SpinDriftLib
      use prod_arrays_dimensions
      use mod_fuel_types
      USE SIZECOM
      use globecom
      use mod_base_year
      SAVE

      INTEGER (kind=2) ::   POINTER(3)
      INTEGER (kind=2) ::  YR
      INTEGER (kind=2) ::  ICAP
      INTEGER (kind=2) ::  ICAP_POINTER
      INTEGER (kind=2) ::  START_POINTER
      INTEGER (kind=2) ::  POINTR
      INTEGER (kind=2) ::  MAX_ANNUAL_UNITS
      INTEGER (kind=2) ::  I
      INTEGER (kind=2) ::  ON_LINE_YEAR
      INTEGER (kind=2) ::  ANNUAL_UNITS_FOR_OPTION
      INTEGER (kind=2) ::  L
      INTEGER (kind=2) ::  CUMULATIVE_UNITS_FOR_OPTION
      INTEGER (kind=2) ::  CAP_ORDER(3)
      INTEGER (kind=2) ::  START_YEAR
      INTEGER (kind=2) ::  END_YEAR
      INTEGER (kind=2) ::  ADD_HARD_WIRED_UNITS_2_NE_PLAN
      REAL ::  ANN_CAP(3)
      REAL ::  EL_PLANNING_CAPACITY
      REAL ::  CL_PLANNING_CAPACITY
      REAL ::  CT_PLANNING_CAPACITY
      REAL ::  ADJUSTMENT_BASE_CAPACITY
      REAL ::  ADJUSTMENT_PEAK_CAPACITY
      REAL ::  DERIV_CAPACITY_PLANNING
      REAL ::  PEAK
      REAL ::  TEMP_PEAK
      REAL ::  UPDATE_NET_PLANNING_PEAK
      REAL ::  RETURN_RESERVE_MARGIN_RATIO
      REAL ::  CAPACITY_UNDER_CONSTRUTION(MAX_PLANNING_YEARS)
      REAL ::  R_TEMP
      REAL ::  ADD_THIS_FINANCIAL_INVESTMENT
      REAL ::  ADJUST_FINANCIAL_INVESTMENT
      INTEGER (kind=2) ::  NU_OF_UNITS_UNDER_CONST(MAX_PLANNING_YEARS)
      INTEGER (kind=2) ::  CURRENT_YEAR
      INTEGER (kind=2) ::  CURRENT_ON_LINE_YEAR
      REAL :: PEAK_GROWTH_RATE,PEAK_GROWTH_RATE_FOR,PEAK_ADJUSTMENT
      REAL :: DSM_CAP(:,:),ADD_THIS_UNIT
      ALLOCATABLE :: DSM_CAP
      REAL (kind=4) ::   TARGET_RATIO,CURRENT_TARGET_RATIO(3)
      CHARACTER (len=2) ::   CAPACITY_PLANNING_METHOD,PLANNING_METHOD
      CHARACTER (len=1) ::   CAP_TYPE(3)
      LOGICAL (kind=1) ::  RESERVE_MARGIN_PLANNING
      LOGICAL (kind=1) ::  RESORT_RATIOS_WITHIN_YEAR
      LOGICAL (kind=1) ::  FIRST_RESERVE_ADDITION
      LOGICAL (kind=1) ::  UNIT_WAS_ADDED
      LOGICAL (kind=1) ::  TARGET_NOT_MET
      LOGICAL (kind=1) ::  FOUND_NEXT_ADDITION
      LOGICAL (kind=1) ::  HARD_WIRED_UNITS_IN
      LOGICAL (kind=1) ::  CONTINUE_ADDING_UNITS
      LOGICAL (kind=1) ::  GET_NEXT_ADDITION
      INTEGER (kind=2) ::   HARD_WIRED_UNITS_ADDED
      INTEGER (kind=2) ::  ADD_ALL_HARD_WIRED_UNIT
      INTEGER (kind=2) ::  MAX_LEAD_TIME=10
      INTEGER (kind=2) ::  GET_MAX_LEAD_TIME
      INTEGER (kind=2) ::   ANNUAL_UNITS_LEFT,REDUCE_ANNUAL_UNITS_LEFT
      INTEGER (kind=2) ::   AVAIALABLE_OPTIONS,RESET_ANNUAL_UNITS_LEFT
      LOGICAL (kind=1) ::   EXPANSION_REPORT,EXPANSION_REPORT_ACTIVE
      REAL :: FIRST_YEAR_CAPACITY,GET_OPTION_CAPACITY
      INTEGER (kind=2) ::   LEAD_TIME,GET_OPTION_LEAD_TIME
      INTEGER (kind=2) ::   TEMP_CUM_UNITS,EXPANSION_PATTERN_NO
      LOGICAL (kind=1) ::   WRITE_2_EXPANSION_PATTERN_REPT,L_TEMP
      REAL :: CAP_ALREADY_ADDED(0:3,MAX_PLANNING_YEARS),CAPACITY_ADDED
      INTEGER (kind=2) ::   SAVED_POINTERS(3)
      LOGICAL (kind=1) ::   TIME_IS_AVAILABLE_2_ACCEL
      INTEGER (kind=2) ::   TIME_NEEDED_2_ACCEL
      CHARACTER (len=1) ::  OVER_CAPACITY_DECISION
      CHARACTER (len=1) ::  UNDER_CAPACITY_DECISION
      CHARACTER (len=1) ::  WHAT_DO_I_DO
      CHARACTER (len=1) ::  RETURN_CANCEL_RULE
      CHARACTER (len=1) ::  VOID_CHR
      INTEGER (kind=2) ::   UNDER_CAPACITY_TABLE_NUM
      INTEGER (kind=2) ::   OVER_CAPACITY_TABLE_NUM
      INTEGER (kind=2) ::   REMAINING_CONSTRUCTION_YRS
      INTEGER (kind=2) ::   PLANNING_FOR_YEAR
      INTEGER (kind=2) ::   PLANNING_YEAR,PLANNING_LEAD_TIME
!
      INTEGER (kind=2) ::  CANCEL_BECAUSE_OF_DELAYS
      INTEGER (kind=2) ::  CANCEL_4_DELAYS_AND_COST
      REAL :: COST_LEVEL_TO_COMPLETE
!
      REAL :: BASE_CAPACITY_ADDED
      REAL :: CYCLE_CAPACITY_ADDED,PEAK_CAPACITY_ADDED
      REAL :: NEW_ADDITIONS(3),ADD_THIS_UNIT_2_CAPACITY
!
      CHARACTER (len=1) ::   OPTION_TYPE,GET_OPTION_LOADING_TYPE
!
      INTEGER (kind=2) ::  NEXT_COMMITTED_OPTION
      INTEGER (kind=2) ::  NEXT_PLANNED_OPTION
      INTEGER (kind=2) ::  MAX_COMMITTED_OPTIONS
      INTEGER (kind=2) ::  MAX_PLANNED_OPTIONS
      INTEGER (kind=2) ::  MUST_BUILD_IT_LEAD_TIME
      LOGICAL (kind=1), allocatable ::   HAS_BEEN_ACCELERATED(:)
      LOGICAL (kind=1) ::  LOG1_FALSE
      LOGICAL (kind=1) ::  ABANDON_IT
      LOGICAL (kind=1) ::  MODIFY_RESOURCE_TIMING
      LOGICAL (kind=1), allocatable ::  TEMP_L1_ARRAY(:)
      REAL :: PERCENT_TO_COMPLETION
      REAL, allocatable ::  OPTION_PERCENT_COMPLETED(:)
      REAL, allocatable ::  TEMP_R4_ARRAY(:)
      INTEGER (kind=2), allocatable ::  COMMITTED_OPTION_POINTERS(:)
      INTEGER (kind=2), allocatable ::  COMMITTED_OPTION_START_YEAR(:)
      INTEGER (kind=2), allocatable ::  COMMITTED_OPTION_ON_LINE_YR(:)
      INTEGER (kind=2), allocatable ::  COMMITTED_OPTION_ORG_ON_LINE_YR(:)
      INTEGER (kind=2), allocatable ::  COMMITTED_OPTION_TIMES_DELAYED(:)
      INTEGER (kind=2), allocatable ::  REC_LOCATOR(:)
      INTEGER (kind=2), allocatable ::  PLANNED_OPTION_POINTERS(:)
      INTEGER (kind=2), allocatable ::  PLANNED_OPTION_START_YEAR(:)
      INTEGER (kind=2), allocatable ::  PLANNED_OPTION_ON_LINE_YR(:)
      INTEGER (kind=2), allocatable ::  TEMP_I2_ARRAY(:)
      INTEGER (kind=2), allocatable ::  DELAYED_ON_LINE_DATE(:)
      

      EXPANSION_REPORT_ACTIVE = EXPANSION_REPORT()
!
      CURRENT_YEAR = globecom_YEAR + get_BASE_YEAR()
      PLANNING_YEAR = globecom_YEAR + get_BASE_YEAR()
      PLANNING_LEAD_TIME = 0
      PLANNING_FOR_YEAR = PLANNING_YEAR + PLANNING_LEAD_TIME
      LOG1_FALSE = .FALSE.
      IF(globecom_YEAR == 1) THEN
         MAX_LEAD_TIME = GET_MAX_LEAD_TIME()
         POINTER(1) = 1
         POINTER(2) = 1
         POINTER(3) = 1
         SAVED_POINTERS(1) = 1
         SAVED_POINTERS(2) = 1
         SAVED_POINTERS(3) = 1
         NEXT_COMMITTED_OPTION = 0
         NEXT_PLANNED_OPTION = 0
         MAX_COMMITTED_OPTIONS = 100
         MAX_PLANNED_OPTIONS = 100
         IF(ALLOCATED(COMMITTED_OPTION_POINTERS)) then
                  DEALLOCATE(COMMITTED_OPTION_POINTERS, &
                             COMMITTED_OPTION_START_YEAR, &
                             COMMITTED_OPTION_ON_LINE_YR, &
                             COMMITTED_OPTION_ORG_ON_LINE_YR, &
                             COMMITTED_OPTION_TIMES_DELAYED, &
                             OPTION_PERCENT_COMPLETED, &
                             REC_LOCATOR, &
                             HAS_BEEN_ACCELERATED, &
                             PLANNED_OPTION_POINTERS, &
                             PLANNED_OPTION_START_YEAR, &
                             PLANNED_OPTION_ON_LINE_YR)
         end if
         
         ALLOCATE(COMMITTED_OPTION_POINTERS(MAX_COMMITTED_OPTIONS), &
              COMMITTED_OPTION_START_YEAR(MAX_COMMITTED_OPTIONS), &
              COMMITTED_OPTION_ON_LINE_YR(MAX_COMMITTED_OPTIONS), &
              COMMITTED_OPTION_ORG_ON_LINE_YR(MAX_COMMITTED_OPTIONS), &
              COMMITTED_OPTION_TIMES_DELAYED(MAX_COMMITTED_OPTIONS), &
              OPTION_PERCENT_COMPLETED(MAX_COMMITTED_OPTIONS), &
              REC_LOCATOR(MAX_COMMITTED_OPTIONS), &
              HAS_BEEN_ACCELERATED(MAX_COMMITTED_OPTIONS), &
              PLANNED_OPTION_POINTERS(MAX_PLANNED_OPTIONS), &
              PLANNED_OPTION_START_YEAR(MAX_PLANNED_OPTIONS), &
              PLANNED_OPTION_ON_LINE_YR(MAX_PLANNED_OPTIONS))
         COMMITTED_OPTION_TIMES_DELAYED = 0
         OPTION_PERCENT_COMPLETED = 0.
         REC_LOCATOR = 0
         HAS_BEEN_ACCELERATED = LOG1_FALSE
!
         NEXT_COMMITTED_OPTION = ADD_HARD_WIRED_UNITS_2_NE_PLAN( &
                                        COMMITTED_OPTION_POINTERS, &
                                        COMMITTED_OPTION_START_YEAR, &
                                        COMMITTED_OPTION_ON_LINE_YR, &
                                        COMMITTED_OPTION_ORG_ON_LINE_YR)
         IF(NEXT_COMMITTED_OPTION > 0 .AND. PLANNING_LEAD_TIME > 0) THEN
            DO I = 1, NEXT_COMMITTED_OPTION
               IF(COMMITTED_OPTION_START_YEAR(I) >= &
                   PLANNING_FOR_YEAR) then
                        CYCLE
                end if
               ON_LINE_YEAR = COMMITTED_OPTION_ON_LINE_YR(I)
               POINTR = COMMITTED_OPTION_POINTERS(I)
               OPTION_PERCENT_COMPLETED(I) = &
                    ADD_THIS_FINANCIAL_INVESTMENT(POINTR,ON_LINE_YEAR, &
                                            REC_LOCATOR(I),CURRENT_YEAR)
            ENDDO
         ENDIF
!
      ELSE
         POINTER(1) = SAVED_POINTERS(1)
         POINTER(2) = SAVED_POINTERS(2)
         POINTER(3) = SAVED_POINTERS(3)
      ENDIF
      PLANNING_METHOD = CAPACITY_PLANNING_METHOD()
      RESERVE_MARGIN_PLANNING = (PLANNING_METHOD == 'RE' .OR. &
                                 PLANNING_METHOD == 'SO' .OR. &
                                 PLANNING_METHOD == 'TR' .OR. &
                                 PLANNING_METHOD == 'NE')
      RESORT_RATIOS_WITHIN_YEAR = PLANNING_METHOD == 'SO' .OR. &
                                                 PLANNING_METHOD == 'TR'

      START_YEAR = globecom_YEAR + PLANNING_LEAD_TIME
      END_YEAR = MIN(START_YEAR + MAX_LEAD_TIME,get_globecom_study_period())

      ALLOCATE(DSM_CAP(3,get_globecom_study_period()))
      CALL GET_DSM_CAP(DSM_CAP)

      CAP_TYPE(1) = 'B'
      CAP_TYPE(2) = 'C'
      CAP_TYPE(3) = 'P'
      CAPACITY_ADDED = 0.

      CAP_ALREADY_ADDED = 0.
      CAPACITY_UNDER_CONSTRUTION = 0.
      NU_OF_UNITS_UNDER_CONST = 0.

      IF(NEXT_COMMITTED_OPTION > 0) THEN
         ALLOCATE(DELAYED_ON_LINE_DATE(NEXT_COMMITTED_OPTION))
         DELAYED_ON_LINE_DATE = 0
         DO I = 1, NEXT_COMMITTED_OPTION
            IF(COMMITTED_OPTION_TIMES_DELAYED(I) == 999) then
                CYCLE
            end if
            ON_LINE_YEAR = COMMITTED_OPTION_ON_LINE_YR(I) - get_BASE_YEAR()
            IF(ON_LINE_YEAR > 0 .AND. &
                             ON_LINE_YEAR <= MAX_PLANNING_YEARS) THEN
               NU_OF_UNITS_UNDER_CONST(ON_LINE_YEAR) = 1 + &
                           NU_OF_UNITS_UNDER_CONST(ON_LINE_YEAR)
            ENDIF
         ENDDO
!
! NOT DETERMINE THE STATUS OF STILL COMMITTED UNITS
!
         PEAK_GROWTH_RATE = PEAK_GROWTH_RATE_FOR(START_YEAR,PEAK, &
                                                        PEAK_ADJUSTMENT)
         YR = START_YEAR - 1
         DO WHILE(MODIFY_RESOURCE_TIMING())
            YR = YR + 1
            IF(YR > END_YEAR) then
                EXIT
            end if

            PEAK = PEAK * PEAK_GROWTH_RATE + PEAK_ADJUSTMENT
!
            ANN_CAP(3) =  EL_PLANNING_CAPACITY(3,YR) + &
                          CL_PLANNING_CAPACITY(3,YR) + &
                          CT_PLANNING_CAPACITY(3,YR) + &
                        DSM_CAP(3,YR) + DSM_CAP(2,YR) + DSM_CAP(1,YR)+ &
                          ADJUSTMENT_BASE_CAPACITY(YR) + &
                          ADJUSTMENT_PEAK_CAPACITY(YR) + &
                          DERIV_CAPACITY_PLANNING(YR) + &
                          CAPACITY_ADDED
!
            DO
               TARGET_NOT_MET = &
                       RETURN_RESERVE_MARGIN_RATIO(PEAK,ANN_CAP(3),YR) &
                                                    < TARGET_RATIO(3,YR)
               IF(TARGET_NOT_MET) THEN
                  IF(NU_OF_UNITS_UNDER_CONST(YR) /= 0) THEN
                     DO I = 1, NEXT_COMMITTED_OPTION
                        IF(COMMITTED_OPTION_TIMES_DELAYED(I)==999) then
                            CYCLE
                        end if
                        IF(COMMITTED_OPTION_ON_LINE_YR(I) == &
                                           (YR + get_BASE_YEAR())) THEN
                           POINTR = COMMITTED_OPTION_POINTERS(I)
                           IF(TARGET_NOT_MET .OR. &
                                COMMITTED_OPTION_START_YEAR(I) >= &
                                                      CURRENT_YEAR) THEN
                              FIRST_YEAR_CAPACITY = &
                                             GET_OPTION_CAPACITY(POINTR)
                              ANN_CAP(3)=ANN_CAP(3)+FIRST_YEAR_CAPACITY
                              CAPACITY_ADDED = CAPACITY_ADDED + &
                                                     FIRST_YEAR_CAPACITY
                              TARGET_NOT_MET = &
                                   RETURN_RESERVE_MARGIN_RATIO(PEAK, &
                                                    ANN_CAP(3),YR) < &
                                                      TARGET_RATIO(3,YR)
                           ELSE
                              REMAINING_CONSTRUCTION_YRS = &
                                    COMMITTED_OPTION_ON_LINE_YR(I) - &
                                                            CURRENT_YEAR
                              PERCENT_TO_COMPLETION = MAX(0.,100.- &
                                            OPTION_PERCENT_COMPLETED(I))
                              WHAT_DO_I_DO = OVER_CAPACITY_DECISION( &
                                                PERCENT_TO_COMPLETION, &
                                           REMAINING_CONSTRUCTION_YRS, &
                                        OVER_CAPACITY_TABLE_NUM(POINTR))
                              VOID_CHR = &
                                     RETURN_CANCEL_RULE( &
                                             CANCEL_4_DELAYS_AND_COST, &
                                               COST_LEVEL_TO_COMPLETE, &
                                      OVER_CAPACITY_TABLE_NUM(POINTR), &
                                               CANCEL_BECAUSE_OF_DELAYS)
                              IF(WHAT_DO_I_DO == 'D' .AND. &
                                (COMMITTED_OPTION_TIMES_DELAYED(I)+1 > &
                                         CANCEL_BECAUSE_OF_DELAYS .OR. &
                                (COMMITTED_OPTION_TIMES_DELAYED(I)+1 > &
                                        CANCEL_4_DELAYS_AND_COST .AND. &
                                     OPTION_PERCENT_COMPLETED(I) <= &
                                          COST_LEVEL_TO_COMPLETE))) THEN
!
                                 COMMITTED_OPTION_TIMES_DELAYED(I) = 99
                                 IF(COMMITTED_OPTION_START_YEAR(I) > &
                                                         CURRENT_YEAR) then 
                                     COMMITTED_OPTION_START_YEAR(I)=2300
                                 end if
                                 NU_OF_UNITS_UNDER_CONST(YR)= &
                                   NU_OF_UNITS_UNDER_CONST(YR)-1
                                 CYCLE
                              ENDIF
                              IF(WHAT_DO_I_DO == 'D' .AND. &
                                     .NOT. HAS_BEEN_ACCELERATED(I)) THEN
!
                                 DELAYED_ON_LINE_DATE(I) = 1 + &
                                                 DELAYED_ON_LINE_DATE(I)
                                 COMMITTED_OPTION_ON_LINE_YR(I) = 1 + &
                                          COMMITTED_OPTION_ON_LINE_YR(I)
                                 IF(COMMITTED_OPTION_START_YEAR(I) > &
                                                         CURRENT_YEAR) &
                                  COMMITTED_OPTION_START_YEAR(I) = 1 + &
                                          COMMITTED_OPTION_START_YEAR(I)
                                 NU_OF_UNITS_UNDER_CONST(YR)= &
                                   NU_OF_UNITS_UNDER_CONST(YR)-1
                                 NU_OF_UNITS_UNDER_CONST(YR+1)= &
                                 1+NU_OF_UNITS_UNDER_CONST(YR+1)
                              ELSEIF(WHAT_DO_I_DO == 'X') THEN
!
                                 COMMITTED_OPTION_TIMES_DELAYED(I) = 99
                                 IF(COMMITTED_OPTION_START_YEAR(I) > &
                                                         CURRENT_YEAR) &
                                     COMMITTED_OPTION_START_YEAR(I)=2300
                                 NU_OF_UNITS_UNDER_CONST(YR)= &
                                   NU_OF_UNITS_UNDER_CONST(YR)-1
                              ELSE
!
                                 FIRST_YEAR_CAPACITY = &
                                             GET_OPTION_CAPACITY(POINTR)
                                 ANN_CAP(3) = ANN_CAP(3) + &
                                                     FIRST_YEAR_CAPACITY
                                 CAPACITY_ADDED = CAPACITY_ADDED + &
                                                     FIRST_YEAR_CAPACITY
                              ENDIF
                           ENDIF
                        ENDIF
                     ENDDO
                     EXIT 
                  ELSEIF(NU_OF_UNITS_UNDER_CONST(YR+1) /= 0) THEN
!
! ADD ACCELERATION LOGIC
!
                     DO I = 1, NEXT_COMMITTED_OPTION
                        IF(.NOT. TARGET_NOT_MET) then
                            EXIT
                        end if
                      IF(COMMITTED_OPTION_TIMES_DELAYED(I) == 999 .OR. &
                                          HAS_BEEN_ACCELERATED(I)) then 
                          CYCLE
                      end if
                        IF(COMMITTED_OPTION_ON_LINE_YR(I) /= &
                                             (YR + 1 + get_BASE_YEAR())) then
                            CYCLE
                        end if
                        POINTR = COMMITTED_OPTION_POINTERS(I)
                        REMAINING_CONSTRUCTION_YRS = &
                                    COMMITTED_OPTION_ON_LINE_YR(I) - &
                                                            CURRENT_YEAR
                        PERCENT_TO_COMPLETION = MAX(0.,100.- &
                                            OPTION_PERCENT_COMPLETED(I))
                        WHAT_DO_I_DO = UNDER_CAPACITY_DECISION( &
                                             PERCENT_TO_COMPLETION, &
                                           REMAINING_CONSTRUCTION_YRS, &
                                       UNDER_CAPACITY_TABLE_NUM(POINTR))
                        IF(WHAT_DO_I_DO /= 'A') then
                            CYCLE
                        end if
                        IF(COMMITTED_OPTION_START_YEAR(I) > &
                                                 PLANNING_FOR_YEAR) THEN
                           COMMITTED_OPTION_START_YEAR(I) = -1 + &
                                          COMMITTED_OPTION_START_YEAR(I)
                        ELSE
                           HAS_BEEN_ACCELERATED(I) = .TRUE.
                        ENDIF
                        FIRST_YEAR_CAPACITY=GET_OPTION_CAPACITY(POINTR)
                        ANN_CAP(3) = ANN_CAP(3) + FIRST_YEAR_CAPACITY
                        CAPACITY_ADDED = CAPACITY_ADDED + &
                                                     FIRST_YEAR_CAPACITY
                        COMMITTED_OPTION_ON_LINE_YR(I) = -1 + &
                                          COMMITTED_OPTION_ON_LINE_YR(I)
                        NU_OF_UNITS_UNDER_CONST(YR+1) = -1 + &
                                   NU_OF_UNITS_UNDER_CONST(YR+1)
                        NU_OF_UNITS_UNDER_CONST(YR) = 1 + &
                                     NU_OF_UNITS_UNDER_CONST(YR)
                      TARGET_NOT_MET=RETURN_RESERVE_MARGIN_RATIO(PEAK, &
                                                 ANN_CAP(3),YR) < &
                                                      TARGET_RATIO(3,YR)
                     ENDDO
                  ENDIF
                  EXIT
               ELSE
                  IF(NU_OF_UNITS_UNDER_CONST(YR) /= 0) THEN
!
! ADD DELAY CANCEL LOGIC
!
                     DO I = 1, NEXT_COMMITTED_OPTION
                        IF(COMMITTED_OPTION_TIMES_DELAYED(I)==999) then
                        	CYCLE
                        end if
                        
                        IF(COMMITTED_OPTION_ON_LINE_YR(I) /= &
                                                 (YR + get_BASE_YEAR())) then
                        	CYCLE
                        end if
                        POINTR = COMMITTED_OPTION_POINTERS(I)
                        IF(COMMITTED_OPTION_START_YEAR(I) >= &
                                                      CURRENT_YEAR) THEN
                           WHAT_DO_I_DO = 'D'
                           COMMITTED_OPTION_START_YEAR(I) = 1 + &
                                          COMMITTED_OPTION_START_YEAR(I)
                        ELSE
                           REMAINING_CONSTRUCTION_YRS = &
                                    COMMITTED_OPTION_ON_LINE_YR(I) - &
                                                            CURRENT_YEAR
                           PERCENT_TO_COMPLETION = MAX(0.,100.- &
                                            OPTION_PERCENT_COMPLETED(I))
                           WHAT_DO_I_DO = OVER_CAPACITY_DECISION( &
                                             PERCENT_TO_COMPLETION, &
                                           REMAINING_CONSTRUCTION_YRS, &
                                        OVER_CAPACITY_TABLE_NUM(POINTR))
                        ENDIF
                        VOID_CHR = RETURN_CANCEL_RULE( &
                                             CANCEL_4_DELAYS_AND_COST, &
                                               COST_LEVEL_TO_COMPLETE, &
                                      OVER_CAPACITY_TABLE_NUM(POINTR), &
                                               CANCEL_BECAUSE_OF_DELAYS)
                        IF(WHAT_DO_I_DO == 'D' .AND. &
                                (COMMITTED_OPTION_TIMES_DELAYED(I)+1 > &
                                         CANCEL_BECAUSE_OF_DELAYS .OR. &
                                (COMMITTED_OPTION_TIMES_DELAYED(I)+1 > &
                                        CANCEL_4_DELAYS_AND_COST .AND. &
                                     OPTION_PERCENT_COMPLETED(I) <= &
                                          COST_LEVEL_TO_COMPLETE))) THEN
!
                           IF(COMMITTED_OPTION_START_YEAR(I) > &
                                                         CURRENT_YEAR) &
                                   COMMITTED_OPTION_START_YEAR(I) = 2300
                           COMMITTED_OPTION_TIMES_DELAYED(I) = 99
                           NU_OF_UNITS_UNDER_CONST(YR)= &
                                   NU_OF_UNITS_UNDER_CONST(YR)-1
                           CYCLE
                        ENDIF
                        IF(WHAT_DO_I_DO == 'D') THEN
                           DELAYED_ON_LINE_DATE(I) = 1 + &
                                                 DELAYED_ON_LINE_DATE(I)
                           COMMITTED_OPTION_ON_LINE_YR(I) = 1 + &
                                          COMMITTED_OPTION_ON_LINE_YR(I)
                           NU_OF_UNITS_UNDER_CONST(YR) = &
                                 NU_OF_UNITS_UNDER_CONST(YR) - 1
                           NU_OF_UNITS_UNDER_CONST(YR+1) = 1+ &
                                   NU_OF_UNITS_UNDER_CONST(YR+1)
                        ELSEIF(WHAT_DO_I_DO == 'X') THEN
                           IF(COMMITTED_OPTION_START_YEAR(I) > &
                                                         CURRENT_YEAR) &
                                   COMMITTED_OPTION_START_YEAR(I) = 2300
                           COMMITTED_OPTION_TIMES_DELAYED(I) = 99
                           NU_OF_UNITS_UNDER_CONST(YR)= &
                                   NU_OF_UNITS_UNDER_CONST(YR)-1
                        ELSE
                           FIRST_YEAR_CAPACITY = &
                                             GET_OPTION_CAPACITY(POINTR)
                           ANN_CAP(3) = ANN_CAP(3) + FIRST_YEAR_CAPACITY
                           CAPACITY_ADDED = CAPACITY_ADDED + &
                                                     FIRST_YEAR_CAPACITY
                        ENDIF
                     ENDDO
                  ENDIF
                  EXIT
               ENDIF
            ENDDO
         ENDDO

! NEED TO SET THE CAPACITY BY YEAR

         DO I = 1, NEXT_COMMITTED_OPTION
            IF(COMMITTED_OPTION_TIMES_DELAYED(I) >= 999) then
                CYCLE
            end if
            ON_LINE_YEAR = COMMITTED_OPTION_ON_LINE_YR(I) - get_BASE_YEAR()
            IF(DELAYED_ON_LINE_DATE(I) > 0) &
                 COMMITTED_OPTION_TIMES_DELAYED(I) = 1 + &
                                       COMMITTED_OPTION_TIMES_DELAYED(I)
            IF(ON_LINE_YEAR > 0 .AND. &
                             ON_LINE_YEAR <= MAX_PLANNING_YEARS) THEN
               POINTR = COMMITTED_OPTION_POINTERS(I)
               FIRST_YEAR_CAPACITY = GET_OPTION_CAPACITY(POINTR)
               OPTION_TYPE = GET_OPTION_LOADING_TYPE(POINTR)
               CAPACITY_UNDER_CONSTRUTION(ON_LINE_YEAR) = &
                            CAPACITY_UNDER_CONSTRUTION(ON_LINE_YEAR) + &
                                                     FIRST_YEAR_CAPACITY
               
               L = INDEX('BCP',OPTION_TYPE)

             CAP_ALREADY_ADDED(L,ON_LINE_YEAR) = FIRST_YEAR_CAPACITY + &
                                       CAP_ALREADY_ADDED(L,ON_LINE_YEAR)
            ENDIF
         ENDDO
         DEALLOCATE(DELAYED_ON_LINE_DATE)
      ENDIF
!
! GET CURRENT GROWTH RATE AND LAST YEAR'S ACTUAL PEAK
!
      NEW_ADDITIONS(1) = 0.
      NEW_ADDITIONS(2) = 0.
      NEW_ADDITIONS(3) = 0.
      PEAK_GROWTH_RATE = PEAK_GROWTH_RATE_FOR(START_YEAR, &
                                                   PEAK,PEAK_ADJUSTMENT)
      DO YR = START_YEAR, END_YEAR
!
         IF(YR == START_YEAR) THEN
            BASE_CAPACITY_ADDED = CAP_ALREADY_ADDED(1,YR)
            CYCLE_CAPACITY_ADDED = CAP_ALREADY_ADDED(1,YR) + &
                                                 CAP_ALREADY_ADDED(2,YR)
            PEAK_CAPACITY_ADDED = CAP_ALREADY_ADDED(1,YR) + &
                                             CAP_ALREADY_ADDED(2,YR) + &
                                                 CAP_ALREADY_ADDED(3,YR)
         ELSE
            BASE_CAPACITY_ADDED = BASE_CAPACITY_ADDED + &
                                             CAP_ALREADY_ADDED(1,YR) + &
                                               NEW_ADDITIONS(1)
            CYCLE_CAPACITY_ADDED = CYCLE_CAPACITY_ADDED + &
                                             CAP_ALREADY_ADDED(1,YR) + &
                                             CAP_ALREADY_ADDED(2,YR) + &
                                               NEW_ADDITIONS(1) + &
                                               NEW_ADDITIONS(2)
            PEAK_CAPACITY_ADDED = PEAK_CAPACITY_ADDED + &
                                             CAP_ALREADY_ADDED(1,YR) + &
                                             CAP_ALREADY_ADDED(2,YR) + &
                                             CAP_ALREADY_ADDED(3,YR) + &
                                               NEW_ADDITIONS(1) + &
                                               NEW_ADDITIONS(2) + &
                                               NEW_ADDITIONS(3)
         ENDIF

         NEW_ADDITIONS(1) = 0.
         NEW_ADDITIONS(2) = 0.
         NEW_ADDITIONS(3) = 0.
         PEAK = PEAK * PEAK_GROWTH_RATE + PEAK_ADJUSTMENT
         AVAIALABLE_OPTIONS = RESET_ANNUAL_UNITS_LEFT()
         CURRENT_TARGET_RATIO(1) = TARGET_RATIO(1,YR)
         CURRENT_TARGET_RATIO(2) = TARGET_RATIO(2,YR)
         CURRENT_TARGET_RATIO(3) = TARGET_RATIO(3,YR)

         ANN_CAP(1) =  EL_PLANNING_CAPACITY(1,YR) + &
                    CL_PLANNING_CAPACITY(1,YR) + &
                    CT_PLANNING_CAPACITY(1,YR) + &
                    DSM_CAP(1,YR) + &
                    ADJUSTMENT_BASE_CAPACITY(YR) + &
                    BASE_CAPACITY_ADDED
         ANN_CAP(2) =  EL_PLANNING_CAPACITY(2,YR) + &
                    CL_PLANNING_CAPACITY(2,YR) + &
                    CT_PLANNING_CAPACITY(2,YR) + &
                    DSM_CAP(2,YR) + DSM_CAP(1,YR) + &
                    ADJUSTMENT_BASE_CAPACITY(YR)  + &
                    CYCLE_CAPACITY_ADDED
         ANN_CAP(3) =  EL_PLANNING_CAPACITY(3,YR) + &
                    CL_PLANNING_CAPACITY(3,YR) + &
                    CT_PLANNING_CAPACITY(3,YR) + &
                    DSM_CAP(3,YR) + DSM_CAP(2,YR) + DSM_CAP(1,YR) + &
                    ADJUSTMENT_BASE_CAPACITY(YR) + &
                    ADJUSTMENT_PEAK_CAPACITY(YR) + &
                    DERIV_CAPACITY_PLANNING(YR) + &
                    PEAK_CAPACITY_ADDED
!
         TARGET_NOT_MET = &
                      RETURN_RESERVE_MARGIN_RATIO(PEAK,ANN_CAP(3),YR) &
                                              < CURRENT_TARGET_RATIO(3)
         FIRST_RESERVE_ADDITION = .TRUE.
!
         CONTINUE_ADDING_UNITS = .TRUE.
         UNIT_WAS_ADDED = .FALSE.
!
         DO WHILE (TARGET_NOT_MET .AND. CONTINUE_ADDING_UNITS)
            IF((RESORT_RATIOS_WITHIN_YEAR .AND. UNIT_WAS_ADDED) .OR. &
                                           FIRST_RESERVE_ADDITION) then
                  CALL RESORT_CAPACITY_ORDER(YR, &
                                             CURRENT_TARGET_RATIO, &
                                             ANN_CAP,PEAK,CAP_ORDER, &
                                             ICAP_POINTER,ICAP)
            end if
!
            IF((ANN_CAP(3)/PEAK >= CURRENT_TARGET_RATIO(3) .AND. &
                                         RESERVE_MARGIN_PLANNING) .OR. &
               (ANN_CAP(ICAP)/PEAK >= CURRENT_TARGET_RATIO(ICAP) .AND. &
                                .NOT. RESERVE_MARGIN_PLANNING)) GOTO 150
            START_POINTER = POINTER(ICAP)
!
   70       CONTINUE ! FIND NEXT ADDITION OF SAME LOADING TYPE
            FOUND_NEXT_ADDITION = GET_NEXT_ADDITION(POINTER(ICAP), &
                                                    CAP_TYPE(ICAP), &
                                                    YR+get_BASE_YEAR(), &
                                                    START_POINTER, &
                                                    YR-START_YEAR+1, &
                                                    POINTR)
            IF(FOUND_NEXT_ADDITION) THEN
!
               UNIT_WAS_ADDED = .TRUE. ! ADD THIS UNIT TYPE TO THE PLAN
!
               IF( .NOT. RESORT_RATIOS_WITHIN_YEAR) THEN
                  MAX_ANNUAL_UNITS = &
                           MAX(ANNUAL_UNITS_FOR_OPTION(POINTR),INT(1,2))
               ELSE
                  MAX_ANNUAL_UNITS = 1
               ENDIF
!
               FIRST_YEAR_CAPACITY = GET_OPTION_CAPACITY(POINTR)
               LEAD_TIME = GET_OPTION_LEAD_TIME(POINTR)
               TEMP_CUM_UNITS = CUMULATIVE_UNITS_FOR_OPTION(POINTR)
               DO L = 1, MAX_ANNUAL_UNITS
!
                  IF(YR-LEAD_TIME == START_YEAR) THEN
                     IF(NEXT_COMMITTED_OPTION+1 > &
                                             MAX_COMMITTED_OPTIONS) THEN
                        CALL REALLOCATE_COMMITTED_ARRAYS
                     ENDIF
                     NEXT_COMMITTED_OPTION = NEXT_COMMITTED_OPTION + 1
                     COMMITTED_OPTION_POINTERS(NEXT_COMMITTED_OPTION) &
                                                                = POINTR
                   COMMITTED_OPTION_START_YEAR(NEXT_COMMITTED_OPTION)= &
                                           YR+get_BASE_YEAR()-LEAD_TIME
                   COMMITTED_OPTION_ON_LINE_YR(NEXT_COMMITTED_OPTION)= &
                                                            YR+get_BASE_YEAR()
                     COMMITTED_OPTION_ORG_ON_LINE_YR &
                                    (NEXT_COMMITTED_OPTION)=YR+get_BASE_YEAR()
                     CALL UPDATE_ANN_CAP(CAP_ALREADY_ADDED(1,YR), &
                                                  PEAK,CAP_TYPE(ICAP), &
                                                    FIRST_YEAR_CAPACITY)
                     CALL UPDATE_ANN_CAP(NEW_ADDITIONS, &
                                                  PEAK,CAP_TYPE(ICAP), &
                                                    FIRST_YEAR_CAPACITY)
                     NU_OF_UNITS_UNDER_CONST(YR) = 1 + &
                                     NU_OF_UNITS_UNDER_CONST(YR)
                  ELSEIF(YR-LEAD_TIME > START_YEAR) THEN
                     IF(NEXT_PLANNED_OPTION+1 > &
                                               MAX_PLANNED_OPTIONS) THEN
                        CALL REALLOCATE_PLANNED_ARRAYS
                     ENDIF
                     NEXT_PLANNED_OPTION = NEXT_PLANNED_OPTION + 1
                     PLANNED_OPTION_POINTERS(NEXT_PLANNED_OPTION)=POINTR
                     PLANNED_OPTION_START_YEAR(NEXT_PLANNED_OPTION) = &
                                                  YR+get_BASE_YEAR()-LEAD_TIME
                     PLANNED_OPTION_ON_LINE_YR(NEXT_PLANNED_OPTION) = &
                                                            YR+get_BASE_YEAR()
                     CALL UPDATE_ANN_CAP(NEW_ADDITIONS, &
                                                  PEAK,CAP_TYPE(ICAP), &
                                                    FIRST_YEAR_CAPACITY)
                  ELSE
                     CYCLE
                  ENDIF
                  CALL UPDATE_ANN_CAP(ANN_CAP,PEAK,CAP_TYPE(ICAP), &
                                                    FIRST_YEAR_CAPACITY)
                  ANNUAL_UNITS_LEFT = REDUCE_ANNUAL_UNITS_LEFT(POINTR)
                  TEMP_CUM_UNITS = TEMP_CUM_UNITS - 1
!
                  IF((ANN_CAP(ICAP)/PEAK >= &
                                     CURRENT_TARGET_RATIO(ICAP)) .OR. &
                     (RESERVE_MARGIN_PLANNING .AND. &
                         ANN_CAP(3)/PEAK>=CURRENT_TARGET_RATIO(3))) THEN
                     POINTER(ICAP) = POINTER(ICAP) + 1
                     GOTO 150
                  ENDIF
!
                  IF(TEMP_CUM_UNITS <= 0) EXIT
               ENDDO
!
!      RAN OUT OF CUMULATIVE UNITS
!
               POINTER(ICAP) = POINTER(ICAP) + 1
               IF(POINTER(ICAP) /= START_POINTER) GOTO 70
            ELSE
               UNIT_WAS_ADDED = .FALSE. 
               ! UNIT WAS NOT FOUND SO DON'T SORT
            ENDIF ! IF FOUND_NEXT_ADDITION
!
!      MOVE-ON TO THE NEXT LOADING TYPE
!
  150       IF(RESERVE_MARGIN_PLANNING) THEN
               ICAP_POINTER = ICAP_POINTER + 1
               TARGET_NOT_MET = TARGET_RATIO(3,YR) > &
                         RETURN_RESERVE_MARGIN_RATIO(PEAK,ANN_CAP(3),YR)
               FIRST_RESERVE_ADDITION = .FALSE.
               IF(TARGET_NOT_MET) THEN
                  IF(ICAP_POINTER > 3) THEN
                     IF(UNIT_WAS_ADDED) THEN
                        UNIT_WAS_ADDED = .FALSE.
                        ICAP_POINTER = 1
                        ICAP = CAP_ORDER(ICAP_POINTER)
                     ELSE
                        CONTINUE_ADDING_UNITS = .FALSE.
                     ENDIF
                  ELSE
                     ICAP = CAP_ORDER(ICAP_POINTER)
                  ENDIF
               ENDIF
            ELSE
               ICAP = ICAP + 1
               TARGET_NOT_MET = ICAP <= 3
            ENDIF
         ENDDO  ! WHILE TARGET NOT MET AND CONTINUE ADDING UNITS
         IF(YR == START_YEAR) THEN
            SAVED_POINTERS(1) = POINTER(1)
            SAVED_POINTERS(2) = POINTER(2)
            SAVED_POINTERS(3) = POINTER(3)
         ENDIF
      ENDDO ! YEAR LOOP
      IF(NEXT_COMMITTED_OPTION > 0) THEN
         CALL OPEN_MODEL_ADDED_ASSET_FILE(11)
         PEAK_GROWTH_RATE = PEAK_GROWTH_RATE_FOR(START_YEAR,PEAK, &
                                                        PEAK_ADJUSTMENT)
         PEAK = PEAK * PEAK_GROWTH_RATE + PEAK_ADJUSTMENT
         DO I = 1, NEXT_COMMITTED_OPTION
            IF(COMMITTED_OPTION_TIMES_DELAYED(I) == 999) then
                CYCLE
            end if
            ON_LINE_YEAR = COMMITTED_OPTION_ON_LINE_YR(I)
            IF(ON_LINE_YEAR < CURRENT_YEAR) then
                CYCLE
            end if
            POINTR = COMMITTED_OPTION_POINTERS(I)
            IF(ON_LINE_YEAR == CURRENT_YEAR) THEN
               FIRST_YEAR_CAPACITY = &
                           ADD_THIS_UNIT_2_CAPACITY(POINTR,CURRENT_YEAR)
               IF(EXPANSION_REPORT_ACTIVE) then
                L_TEMP = &
                     WRITE_2_EXPANSION_PATTERN_REPT(POINTR,"On-line   ")
               end if
            ENDIF
            IF(COMMITTED_OPTION_START_YEAR(I) == CURRENT_YEAR .AND. &
                                               REC_LOCATOR(I) == 0) THEN
               OPTION_PERCENT_COMPLETED(I) = &
                    ADD_THIS_FINANCIAL_INVESTMENT(POINTR,ON_LINE_YEAR, &
                                            REC_LOCATOR(I),CURRENT_YEAR)
            ELSEIF(COMMITTED_OPTION_START_YEAR(I) < CURRENT_YEAR) THEN
               IF(COMMITTED_OPTION_TIMES_DELAYED(I) == 99) THEN
                  ABANDON_IT = .TRUE.
                  COMMITTED_OPTION_TIMES_DELAYED(I) = 999
                  COMMITTED_OPTION_ON_LINE_YR(I) = 2300
               ELSE
                  ABANDON_IT = .FALSE.
               ENDIF
               OPTION_PERCENT_COMPLETED(I) = &
                      ADJUST_FINANCIAL_INVESTMENT(POINTR,ON_LINE_YEAR, &
                               REC_LOCATOR(I),ABANDON_IT,CURRENT_YEAR, &
                                 COMMITTED_OPTION_TIMES_DELAYED(I))
            ENDIF
         ENDDO
         CALL CLOSE_MODEL_ADDED_ASSET_FILE
      ENDIF
      DEALLOCATE(DSM_CAP)
      RETURN


      ENTRY REALLOCATE_COMMITTED_ARRAYS

         LOG1_FALSE = .FALSE.
         MAX_COMMITTED_OPTIONS = MAX_COMMITTED_OPTIONS + 50
         ALLOCATE(TEMP_I2_ARRAY(NEXT_COMMITTED_OPTION))
         TEMP_I2_ARRAY(1:NEXT_COMMITTED_OPTION) = &
                      COMMITTED_OPTION_POINTERS(1:NEXT_COMMITTED_OPTION)
         DEALLOCATE(COMMITTED_OPTION_POINTERS)
         ALLOCATE(COMMITTED_OPTION_POINTERS(MAX_COMMITTED_OPTIONS))
         COMMITTED_OPTION_POINTERS(1:NEXT_COMMITTED_OPTION) = &
                                  TEMP_I2_ARRAY(1:NEXT_COMMITTED_OPTION)

         TEMP_I2_ARRAY(1:NEXT_COMMITTED_OPTION) = &
                 COMMITTED_OPTION_TIMES_DELAYED(1:NEXT_COMMITTED_OPTION)
         DEALLOCATE(COMMITTED_OPTION_TIMES_DELAYED)
         ALLOCATE(COMMITTED_OPTION_TIMES_DELAYED(MAX_COMMITTED_OPTIONS))
         COMMITTED_OPTION_TIMES_DELAYED = 0
         COMMITTED_OPTION_TIMES_DELAYED(1:NEXT_COMMITTED_OPTION) = &
                                  TEMP_I2_ARRAY(1:NEXT_COMMITTED_OPTION)

         TEMP_I2_ARRAY(1:NEXT_COMMITTED_OPTION) = &
                 COMMITTED_OPTION_START_YEAR(1:NEXT_COMMITTED_OPTION)
         DEALLOCATE(COMMITTED_OPTION_START_YEAR)
         ALLOCATE(COMMITTED_OPTION_START_YEAR(MAX_COMMITTED_OPTIONS))
         COMMITTED_OPTION_START_YEAR = 0
         COMMITTED_OPTION_START_YEAR(1:NEXT_COMMITTED_OPTION) = &
                                  TEMP_I2_ARRAY(1:NEXT_COMMITTED_OPTION)
!
         TEMP_I2_ARRAY(1:NEXT_COMMITTED_OPTION) = &
                 COMMITTED_OPTION_ON_LINE_YR(1:NEXT_COMMITTED_OPTION)
         DEALLOCATE(COMMITTED_OPTION_ON_LINE_YR)
         ALLOCATE(COMMITTED_OPTION_ON_LINE_YR(MAX_COMMITTED_OPTIONS))
         COMMITTED_OPTION_ON_LINE_YR = 0
         COMMITTED_OPTION_ON_LINE_YR(1:NEXT_COMMITTED_OPTION) = &
                                  TEMP_I2_ARRAY(1:NEXT_COMMITTED_OPTION)
!
         TEMP_I2_ARRAY(1:NEXT_COMMITTED_OPTION) = &
                COMMITTED_OPTION_ORG_ON_LINE_YR(1:NEXT_COMMITTED_OPTION)
         DEALLOCATE(COMMITTED_OPTION_ORG_ON_LINE_YR)
        ALLOCATE(COMMITTED_OPTION_ORG_ON_LINE_YR(MAX_COMMITTED_OPTIONS))
         COMMITTED_OPTION_ORG_ON_LINE_YR = 0
         COMMITTED_OPTION_ORG_ON_LINE_YR(1:NEXT_COMMITTED_OPTION) = &
                                  TEMP_I2_ARRAY(1:NEXT_COMMITTED_OPTION)
!
         TEMP_I2_ARRAY(1:NEXT_COMMITTED_OPTION) = &
                                    REC_LOCATOR(1:NEXT_COMMITTED_OPTION)
         DEALLOCATE(REC_LOCATOR)
         ALLOCATE(REC_LOCATOR(MAX_COMMITTED_OPTIONS))
         REC_LOCATOR = 0
         REC_LOCATOR(1:NEXT_COMMITTED_OPTION) = &
                                  TEMP_I2_ARRAY(1:NEXT_COMMITTED_OPTION)

         DEALLOCATE(TEMP_I2_ARRAY)
!
         ALLOCATE(TEMP_R4_ARRAY(NEXT_COMMITTED_OPTION))
         TEMP_R4_ARRAY(1:NEXT_COMMITTED_OPTION) = &
                       OPTION_PERCENT_COMPLETED(1:NEXT_COMMITTED_OPTION)
         DEALLOCATE(OPTION_PERCENT_COMPLETED)
         ALLOCATE(OPTION_PERCENT_COMPLETED(MAX_COMMITTED_OPTIONS))
         OPTION_PERCENT_COMPLETED = 0.
         OPTION_PERCENT_COMPLETED(1:NEXT_COMMITTED_OPTION) = &
                                  TEMP_R4_ARRAY(1:NEXT_COMMITTED_OPTION)
         DEALLOCATE(TEMP_R4_ARRAY)
!
         ALLOCATE(TEMP_L1_ARRAY(NEXT_COMMITTED_OPTION))
         TEMP_L1_ARRAY(1:NEXT_COMMITTED_OPTION) = &
                       HAS_BEEN_ACCELERATED(1:NEXT_COMMITTED_OPTION)
         DEALLOCATE(HAS_BEEN_ACCELERATED)
         ALLOCATE(HAS_BEEN_ACCELERATED(MAX_COMMITTED_OPTIONS))
         HAS_BEEN_ACCELERATED = .FALSE.
         HAS_BEEN_ACCELERATED(1:NEXT_COMMITTED_OPTION) = &
                                  TEMP_L1_ARRAY(1:NEXT_COMMITTED_OPTION)
         DEALLOCATE(TEMP_L1_ARRAY)
      RETURN


      ENTRY REALLOCATE_PLANNED_ARRAYS

         MAX_PLANNED_OPTIONS = MAX_PLANNED_OPTIONS + 50
         ALLOCATE(TEMP_I2_ARRAY(NEXT_PLANNED_OPTION))
!
         TEMP_I2_ARRAY(1:NEXT_COMMITTED_OPTION) = &
                      PLANNED_OPTION_POINTERS(1:NEXT_COMMITTED_OPTION)
         DEALLOCATE(PLANNED_OPTION_POINTERS)
         ALLOCATE(PLANNED_OPTION_POINTERS(MAX_COMMITTED_OPTIONS))
         PLANNED_OPTION_POINTERS(1:NEXT_COMMITTED_OPTION) = &
                                  TEMP_I2_ARRAY(1:NEXT_COMMITTED_OPTION)
!
         TEMP_I2_ARRAY(1:NEXT_COMMITTED_OPTION) = &
                      PLANNED_OPTION_START_YEAR(1:NEXT_COMMITTED_OPTION)
         DEALLOCATE(PLANNED_OPTION_START_YEAR)
         ALLOCATE(PLANNED_OPTION_START_YEAR(MAX_COMMITTED_OPTIONS))
         PLANNED_OPTION_START_YEAR(1:NEXT_COMMITTED_OPTION) = &
                                  TEMP_I2_ARRAY(1:NEXT_COMMITTED_OPTION)
!
         TEMP_I2_ARRAY(1:NEXT_COMMITTED_OPTION) = &
                      PLANNED_OPTION_ON_LINE_YR(1:NEXT_COMMITTED_OPTION)
         DEALLOCATE(PLANNED_OPTION_ON_LINE_YR)
         ALLOCATE(PLANNED_OPTION_ON_LINE_YR(MAX_COMMITTED_OPTIONS))
         PLANNED_OPTION_ON_LINE_YR(1:NEXT_COMMITTED_OPTION) = &
                                  TEMP_I2_ARRAY(1:NEXT_COMMITTED_OPTION)
!
         DEALLOCATE(TEMP_I2_ARRAY)
      RETURN
      END


      FUNCTION PLANNING_DECISIONS()
      use miscmod
      use end_routine
      use debugtrace
      implicit none

      CHARACTER (len=1) ::  PLANNING_DECISIONS
      CHARACTER (len=1) ::  OVER_CAPACITY_DECISION
      CHARACTER (len=1) ::  UNDER_CAPACITY_DECISION
      CHARACTER (len=1) ::  RETURN_CANCEL_RULE
      CHARACTER (len=68) ::   FILE_NAME,OVER_UNDER_TABLE_FILE_NAME
      INTEGER (kind=2) ::   I
      INTEGER (kind=2) ::   J
      INTEGER (kind=2) ::   L
      INTEGER (kind=2) ::   COST_STATE
      INTEGER (kind=2) ::   COMPLETION_STATE,R_CONSTRUCTION_YEARS
      REAL :: R_COMPLETION_PERCENT,CONSTRUCTION_YEARS
      INTEGER (kind=2) ::   OVER_NUM_OF_COST_STATES
      INTEGER (kind=2) ::   OVER_NUM_OF_COMPLETION_STATES
      INTEGER (kind=2) ::   UNDER_NUM_OF_COST_STATES
      INTEGER (kind=2) ::   UNDER_NUM_OF_COMPLETION_STATES
      LOGICAL (kind=4) ::   FILE_EXISTS
      INTEGER (kind=2) ::  CANCEL_BECAUSE_OF_DELAYS(:)
      INTEGER (kind=2) ::  CANCEL_4_DELAYS_AND_COST(:)
      REAL :: COST_LEVEL_TO_COMPLETE(:)
      ALLOCATABLE:: CANCEL_BECAUSE_OF_DELAYS, &
                    CANCEL_4_DELAYS_AND_COST, &
                    COST_LEVEL_TO_COMPLETE
      INTEGER (kind=2) ::   R_CANCEL_4_DELAYS_AND_COST,R_TABLE_NUM
      REAL :: R_COST_LEVEL_TO_COMPLETE
      REAL :: OVER_COST_LEVELS(:,:),OVER_COMPLETION_LEVELS(:,:)
      REAL :: UNDER_COST_LEVELS(:,:),UNDER_COMPLETION_LEVELS(:,:)
      INTEGER (kind=2) ::  NUMBER_OF_OVER_TABLES
      INTEGER (kind=2) ::  NUMBER_OF_UNDER_TABLES
      INTEGER (kind=2) ::  MAX_OVER_TABLE_NUM
      INTEGER (kind=2) ::  MAX_UNDER_TABLE_NUM
      INTEGER (kind=2) ::  MAX_OVER_TABLE_ROWS
      INTEGER (kind=2) ::  MAX_UNDER_TABLE_ROWS
      INTEGER (kind=2) ::  MAX_OVER_TABLE_COLS
      INTEGER (kind=2) ::  MAX_UNDER_TABLE_COLS
      INTEGER (kind=2) ::  NUMBER_OF_RECORDS
      INTEGER (kind=2) ::  UNDER_TABLE
      INTEGER (kind=2) ::  OVER_TABLE
      INTEGER (kind=2) ::  R_RETURN_CANCEL_RULE
      INTEGER :: IOS
      CHARACTER (len=1) ::  OVER_CAPACITY_ACTIONS(:,:,:)
      CHARACTER (len=1) ::  UNDER_CAPACITY_ACTIONS(:,:,:)
      INTEGER (kind=2) ::  OVER_TABLE_CROSS_REF(:)
      INTEGER (kind=2) ::  ROWS_IN_OVER_TABLE(:)
      INTEGER (kind=2) ::  COLUMNS_IN_OVER_TABLE(:)
      INTEGER (kind=2) ::  UNDER_TABLE_CROSS_REF(:)
      INTEGER (kind=2) ::  ROWS_IN_UNDER_TABLE(:)
      INTEGER (kind=2) ::  COLUMNS_IN_UNDER_TABLE(:)
      INTEGER (kind=2) ::  DELETE
      INTEGER (kind=2) ::  TABLE_NUMBER
      INTEGER (kind=2) ::  TABLE_COLUMNS
      INTEGER (kind=2) ::  TABLE_ROWS
      INTEGER (kind=2) ::  TABLE_POINTER
      CHARACTER (len=15) ::  ABSOLUTE_CANCEL_RULE
      CHARACTER (len=15) ::  TABLE_TYPE*1
      CHARACTER (len=15) ::  OVER*1
      CHARACTER (len=15) ::  COLUMN_STR*60
      CHARACTER (len=15) ::  TABLE_ROW_STR(15)*60
      PARAMETER (OVER = 'O')
      ALLOCATABLE :: OVER_COST_LEVELS,OVER_COMPLETION_LEVELS, &
                     OVER_CAPACITY_ACTIONS,UNDER_CAPACITY_ACTIONS, &
                     UNDER_COST_LEVELS,UNDER_COMPLETION_LEVELS, &
                     OVER_TABLE_CROSS_REF, &
                     ROWS_IN_OVER_TABLE, &
                     COLUMNS_IN_OVER_TABLE, &
                     UNDER_TABLE_CROSS_REF, &
                     ROWS_IN_UNDER_TABLE, &
                     COLUMNS_IN_UNDER_TABLE
      SAVE OVER_COST_LEVELS,OVER_COMPLETION_LEVELS, &
           OVER_CAPACITY_ACTIONS,UNDER_CAPACITY_ACTIONS, &
           OVER_NUM_OF_COST_STATES,OVER_NUM_OF_COMPLETION_STATES, &
           UNDER_NUM_OF_COST_STATES,UNDER_NUM_OF_COMPLETION_STATES, &
           UNDER_COST_LEVELS,UNDER_COMPLETION_LEVELS, &
           CANCEL_BECAUSE_OF_DELAYS, &
           CANCEL_4_DELAYS_AND_COST, &
           COST_LEVEL_TO_COMPLETE , &
           OVER_TABLE_CROSS_REF, &
           ROWS_IN_OVER_TABLE, &
           COLUMNS_IN_OVER_TABLE, &
           UNDER_TABLE_CROSS_REF, &
           ROWS_IN_UNDER_TABLE, &
           COLUMNS_IN_UNDER_TABLE
!
! TEMP SETUP BEFORE USING AN INPUT FILE
!
      CHARACTER (len=64) ::   TITLE
      CHARACTER (len=1024) ::   RECLN(:)
      ALLOCATABLE :: RECLN
      integer :: file_trace_pd=0
!
! THE OVER CAPACITY TABLE
!
         
         if(file_trace_pd==0) then
       file_trace_pd=open_trace("PLANNING_DECISIONS.trace", rq_co_pd)
        call write_trace_message(file_trace_pd, &
            "spooling planning_decisions.")
         end if
         
         
         PLANNING_DECISIONS = 'N'
         FILE_NAME = OVER_UNDER_TABLE_FILE_NAME()
         call write_trace_message(file_trace_pd, trim(file_name))
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         IF(.NOT. FILE_EXISTS) THEN
       call write_trace_message(file_trace_pd, "Allocating.")
            ALLOCATE(RECLN(2))
            NUMBER_OF_OVER_TABLES = 1
            NUMBER_OF_UNDER_TABLES = 1
            MAX_OVER_TABLE_NUM = 1
            MAX_UNDER_TABLE_NUM = 1
            MAX_OVER_TABLE_ROWS = 7
            MAX_UNDER_TABLE_ROWS = 7
            MAX_OVER_TABLE_COLS = 4
            MAX_UNDER_TABLE_COLS = 5
            NUMBER_OF_RECORDS = 2
            RECLN(1) ='1,"Over Capacity Table",1,"OVER","5,3,70",'// &
                      '4,"1,2,3,100",7,"10,C,C,C,C,","25,C,C,C,D",'// &
                      '"45,C,C,C,D,","60,C,D,D,D","70,C,D,D,D,",'// &
                      '"85,D,X,D,D,","100,D,X,X,D,",,,,,,,,,'
            RECLN(2) = '1,"Under Capacity Table",1,"UNDER",,5,'// &
                       '"1,2,3,5,100",7,"10,C,C,C,C,C,",'// &
                       '"25,C,A,A,A,C,","45,C,A,A,A,C,",'// &
                       '"60,C,A,A,A,C,","70,C,A,A,A,C,",'// &
                       '"90,C,A,A,A,C,","100,C,A,A,A,C,",,,,,,,,,'
         ELSE
            call write_trace_message(file_trace_pd, "opening " // &
                trim(FILE_NAME))
                
            OPEN(10,FILE=FILE_NAME)
            ALLOCATE(RECLN(200))
            NUMBER_OF_OVER_TABLES = 0
            NUMBER_OF_UNDER_TABLES = 0
            MAX_OVER_TABLE_NUM = 0
            MAX_UNDER_TABLE_NUM = 0
            MAX_OVER_TABLE_ROWS = 0
            MAX_UNDER_TABLE_ROWS = 0
            MAX_OVER_TABLE_COLS = 0
            MAX_UNDER_TABLE_COLS = 0
            READ(10,'(A)',IOSTAT=IOS) RECLN(1)
            I = 1
            DO WHILE (I <= 200)
               READ(10,'(A)',IOSTAT=IOS) RECLN(I)
               IF(IOS /= 0) then
                   EXIT
               end if
               READ(RECLN(I),*,IOSTAT=IOS) DELETE,TITLE,TABLE_NUMBER, &
                                TABLE_TYPE,ABSOLUTE_CANCEL_RULE, &
                                TABLE_COLUMNS,COLUMN_STR, &
                                TABLE_ROWS
                                
               write(file_trace_pd,*) DELETE,TITLE,TABLE_NUMBER, &
                                TABLE_TYPE,ABSOLUTE_CANCEL_RULE, &
                                TABLE_COLUMNS,COLUMN_STR, &
                                TABLE_ROWS
                         
               IF(IOS /= 0) then
                   call write_trace_message(file_trace_pd, "Exiting 2")
                   EXIT
               end if
               IF(DELETE > 7) then
                   call write_trace_message(file_trace_pd, "CYCLE 1")
                   CYCLE
               end if
               IF(TABLE_TYPE == OVER) THEN
                  MAX_OVER_TABLE_NUM = MAX(MAX_OVER_TABLE_NUM, &
                                           TABLE_NUMBER)
                  MAX_OVER_TABLE_ROWS = MAX(MAX_OVER_TABLE_ROWS, &
                                           TABLE_ROWS)
                  MAX_OVER_TABLE_COLS = MAX(MAX_OVER_TABLE_COLS, &
                                           TABLE_COLUMNS)
                  NUMBER_OF_OVER_TABLES = NUMBER_OF_OVER_TABLES + 1
               ELSE
                  MAX_UNDER_TABLE_NUM = MAX(MAX_UNDER_TABLE_NUM, &
                                           TABLE_NUMBER)
                  MAX_UNDER_TABLE_ROWS = MAX(MAX_UNDER_TABLE_ROWS, &
                                           TABLE_ROWS)
                  MAX_UNDER_TABLE_COLS = MAX(MAX_UNDER_TABLE_COLS, &
                                           TABLE_COLUMNS)
                  NUMBER_OF_UNDER_TABLES = NUMBER_OF_UNDER_TABLES + 1
               ENDIF
               I = I + 1
            ENDDO
            CLOSE(10)
            NUMBER_OF_RECORDS = I - 1
         ENDIF
         
         ALLOCATE(OVER_COST_LEVELS(MAX_OVER_TABLE_ROWS, &
                                               NUMBER_OF_OVER_TABLES), &
                  OVER_COMPLETION_LEVELS(MAX_OVER_TABLE_COLS, &
                                               NUMBER_OF_OVER_TABLES), &
                  OVER_CAPACITY_ACTIONS(MAX_OVER_TABLE_COLS, &
                                             MAX_OVER_TABLE_ROWS, &
                                               NUMBER_OF_OVER_TABLES), &
                  OVER_TABLE_CROSS_REF(MAX_OVER_TABLE_NUM), &
                  ROWS_IN_OVER_TABLE(NUMBER_OF_OVER_TABLES), &
                  CANCEL_BECAUSE_OF_DELAYS(NUMBER_OF_OVER_TABLES), &
                  CANCEL_4_DELAYS_AND_COST(NUMBER_OF_OVER_TABLES), &
                  COST_LEVEL_TO_COMPLETE(NUMBER_OF_OVER_TABLES), &
                  COLUMNS_IN_OVER_TABLE(NUMBER_OF_OVER_TABLES))
         ALLOCATE(UNDER_COST_LEVELS(MAX_UNDER_TABLE_ROWS, &
                                              NUMBER_OF_UNDER_TABLES), &
                  UNDER_COMPLETION_LEVELS(MAX_UNDER_TABLE_COLS, &
                                              NUMBER_OF_UNDER_TABLES), &
                  UNDER_CAPACITY_ACTIONS(MAX_UNDER_TABLE_COLS, &
                                             MAX_UNDER_TABLE_ROWS, &
                                              NUMBER_OF_UNDER_TABLES), &
                  UNDER_TABLE_CROSS_REF(MAX_UNDER_TABLE_NUM), &
                  ROWS_IN_UNDER_TABLE(NUMBER_OF_UNDER_TABLES), &
                  COLUMNS_IN_UNDER_TABLE(NUMBER_OF_UNDER_TABLES))
!
         UNDER_TABLE = 0
         OVER_TABLE = 0
         call write_trace_int2(file_trace_pd, "NUMBER_OF_RECORDS", &
            NUMBER_OF_RECORDS)
            
         DO I = 1, NUMBER_OF_RECORDS
            READ(RECLN(I),*) DELETE,TITLE,TABLE_NUMBER, &
                             TABLE_TYPE,ABSOLUTE_CANCEL_RULE, &
                             TABLE_COLUMNS,COLUMN_STR, &
                             TABLE_ROWS, &
                             (TABLE_ROW_STR(J),J=1,TABLE_ROWS)
            IF(TABLE_TYPE == OVER) THEN
               OVER_TABLE = OVER_TABLE + 1
               OVER_TABLE_CROSS_REF(TABLE_NUMBER) = OVER_TABLE
               ROWS_IN_OVER_TABLE(OVER_TABLE) = TABLE_ROWS
               COLUMNS_IN_OVER_TABLE(OVER_TABLE) = TABLE_COLUMNS
               READ(ABSOLUTE_CANCEL_RULE,*) &
                                 CANCEL_BECAUSE_OF_DELAYS(OVER_TABLE), &
                                 CANCEL_4_DELAYS_AND_COST(OVER_TABLE), &
                                   COST_LEVEL_TO_COMPLETE(OVER_TABLE)
               READ(COLUMN_STR,*) &
                (OVER_COMPLETION_LEVELS(J,OVER_TABLE),J=1,TABLE_COLUMNS)
               DO J = 1, TABLE_ROWS
                  READ(TABLE_ROW_STR(J),*) &
                            OVER_COST_LEVELS(J,OVER_TABLE), &
                           (OVER_CAPACITY_ACTIONS(L,J,OVER_TABLE), &
                                                    L = 1,TABLE_COLUMNS)
               ENDDO
            ELSE
               UNDER_TABLE = UNDER_TABLE + 1
               UNDER_TABLE_CROSS_REF(TABLE_NUMBER) = UNDER_TABLE
               ROWS_IN_UNDER_TABLE(UNDER_TABLE) = TABLE_ROWS
               COLUMNS_IN_UNDER_TABLE(UNDER_TABLE) = TABLE_COLUMNS
               READ(COLUMN_STR,*) &
                             (UNDER_COMPLETION_LEVELS(J,UNDER_TABLE), &
                                                      J=1,TABLE_COLUMNS)
               DO J = 1, TABLE_ROWS
                  READ(TABLE_ROW_STR(J),*) &
                            UNDER_COST_LEVELS(J,UNDER_TABLE), &
                           (UNDER_CAPACITY_ACTIONS(L,J,UNDER_TABLE), &
                                                    L = 1,TABLE_COLUMNS)
               ENDDO
            ENDIF
         ENDDO
         DEALLOCATE(RECLN)
         call write_trace_message(file_trace_pd, "exiting 1")
      RETURN


      ENTRY OVER_CAPACITY_DECISION(R_COMPLETION_PERCENT, &
                                   R_CONSTRUCTION_YEARS, &
                                   R_TABLE_NUM)

         OVER_CAPACITY_DECISION = 'C'
         COST_STATE = 0
         CONSTRUCTION_YEARS = FLOAT(R_CONSTRUCTION_YEARS)
         TABLE_POINTER = OVER_TABLE_CROSS_REF(R_TABLE_NUM)
         DO I = 1, ROWS_IN_OVER_TABLE(TABLE_POINTER)
            IF(R_COMPLETION_PERCENT <= &
                                 OVER_COST_LEVELS(I,TABLE_POINTER)) THEN
               COST_STATE = I
               EXIT
            ENDIF
         ENDDO
         COMPLETION_STATE = 0
         DO J = 1, COLUMNS_IN_OVER_TABLE(TABLE_POINTER)
            IF(CONSTRUCTION_YEARS <= &
                           OVER_COMPLETION_LEVELS(J,TABLE_POINTER)) THEN
               COMPLETION_STATE = J
               EXIT
            ENDIF
         ENDDO
         IF(COST_STATE /= 0 .AND. COMPLETION_STATE /= 0) THEN
            OVER_CAPACITY_DECISION = OVER_CAPACITY_ACTIONS( &
                              COMPLETION_STATE,COST_STATE,TABLE_POINTER)
            IF(OVER_CAPACITY_DECISION == 'F') OVER_CAPACITY_DECISION='C'
         ENDIF
      RETURN


      ENTRY UNDER_CAPACITY_DECISION(R_COMPLETION_PERCENT, &
                                    R_CONSTRUCTION_YEARS, &
                                    R_TABLE_NUM)

         UNDER_CAPACITY_DECISION = 'C'
         COST_STATE = 0
         CONSTRUCTION_YEARS = FLOAT(R_CONSTRUCTION_YEARS)
         TABLE_POINTER = UNDER_TABLE_CROSS_REF(R_TABLE_NUM)
         DO I = 1, ROWS_IN_UNDER_TABLE(TABLE_POINTER)
            IF(R_COMPLETION_PERCENT <= &
                                UNDER_COST_LEVELS(I,TABLE_POINTER)) THEN
               COST_STATE = I
               EXIT
            ENDIF
         ENDDO
         COMPLETION_STATE = 0
         DO J = 1, COLUMNS_IN_UNDER_TABLE(TABLE_POINTER)
            IF(CONSTRUCTION_YEARS <= &
                          UNDER_COMPLETION_LEVELS(J,TABLE_POINTER)) THEN
               COMPLETION_STATE = J
               EXIT
            ENDIF
         ENDDO
         IF(COST_STATE /= 0 .AND. COMPLETION_STATE /= 0) THEN
            UNDER_CAPACITY_DECISION = UNDER_CAPACITY_ACTIONS( &
                              COMPLETION_STATE,COST_STATE,TABLE_POINTER)
            IF(UNDER_CAPACITY_DECISION=='F') UNDER_CAPACITY_DECISION='C'
         ENDIF
      RETURN


      ENTRY RETURN_CANCEL_RULE(R_CANCEL_4_DELAYS_AND_COST, &
                               R_COST_LEVEL_TO_COMPLETE, &
                               R_TABLE_NUM,R_RETURN_CANCEL_RULE)

         TABLE_POINTER = OVER_TABLE_CROSS_REF(R_TABLE_NUM)
         R_CANCEL_4_DELAYS_AND_COST = &
                                 CANCEL_4_DELAYS_AND_COST(TABLE_POINTER)
         R_COST_LEVEL_TO_COMPLETE=COST_LEVEL_TO_COMPLETE(TABLE_POINTER)
         R_RETURN_CANCEL_RULE = CANCEL_BECAUSE_OF_DELAYS(TABLE_POINTER)
         RETURN_CANCEL_RULE = 'X'
      RETURN
      END


      SUBROUTINE RESORT_CAPACITY_ORDER(YR,CURRENT_TARGET_RATIO, &
                                       ANN_CAP,PEAK,CAP_ORDER, &
                                       ICAP_POINTER,ICAP)
      use end_routine

      REAL (kind=4) ::   CURRENT_TARGET_RATIO(3)
      REAL (kind=4) ::   PEAK,MIX_RATIO(3),RTEMP,ANN_CAP(3)
      INTEGER (kind=2) ::   I
      INTEGER (kind=2) ::   M,N,YR,ITEMP,CAP_ORDER(3),ICAP_POINTER,ICAP
      LOGICAL (kind=1) ::   CAPACITY_TYPE_EXISTS

      DO I = 1, 3
         MIX_RATIO(I) = CURRENT_TARGET_RATIO(I) - ANN_CAP(I)/PEAK
         CAP_ORDER(I) = I
      ENDDO

      DO I = 1, 2
         M = I
         N = I + 1
   20    IF (M > 0) THEN
            IF(MIX_RATIO(N) > MIX_RATIO(M)) THEN
               ITEMP = CAP_ORDER(N)
               CAP_ORDER(N) = CAP_ORDER(M)
               CAP_ORDER(M) = ITEMP
               RTEMP = MIX_RATIO(N)
               MIX_RATIO(N) = MIX_RATIO(M)
               MIX_RATIO(M) = RTEMP
               N = M
               M = M - 1
               GOTO 20
            ENDIF
         ENDIF
      ENDDO
      DO I = 1, 3
         ICAP_POINTER = I
         ICAP = CAP_ORDER(I)
         IF(CAPACITY_TYPE_EXISTS(ICAP)) then
            EXIT
         end if
      ENDDO
      RETURN
      END


      SUBROUTINE UPDATE_ANN_CAP(ANN_CAP,PEAK,CAP_TYPE, &
                                         FIRST_YEAR_CAPACITY)
      use end_routine
      CHARACTER (len=1) ::   CAP_TYPE
      REAL :: ANN_CAP(3),PEAK,FIRST_YEAR_CAPACITY
      IF(CAP_TYPE == 'B') THEN
         ANN_CAP(1) = ANN_CAP(1) + FIRST_YEAR_CAPACITY
         ANN_CAP(2) = ANN_CAP(2) + FIRST_YEAR_CAPACITY
         ANN_CAP(3) = ANN_CAP(3) + FIRST_YEAR_CAPACITY
      ELSEIF(CAP_TYPE == 'C') THEN
         ANN_CAP(2) = ANN_CAP(2) + FIRST_YEAR_CAPACITY
         ANN_CAP(3) = ANN_CAP(3) + FIRST_YEAR_CAPACITY
      ELSEIF(CAP_TYPE == 'P') THEN
         ANN_CAP(3) = ANN_CAP(3) + FIRST_YEAR_CAPACITY

      ENDIF
      RETURN
      END


      FUNCTION SAVE_CT_LOAD_REDUCTION(R_NOT_DENOMINATOR_REDUCTION)
      use end_routine

      INTEGER (kind=2) ::   R_YEAR
      REAL ::  RETURN_RESERVE_MARGIN_RATIO
      REAL ::  R_PEAK
      REAL ::  R_CAPACITY
      REAL ::  CT_DENOM_LOAD_REDUCTION
      LOGICAL (kind=1) ::  NOT_DENOMINATOR_REDUCTION
      LOGICAL (kind=1) ::  R_NOT_DENOMINATOR_REDUCTION
      LOGICAL (kind=1) ::  SAVE_CT_LOAD_REDUCTION
!
      SAVE NOT_DENOMINATOR_REDUCTION
         NOT_DENOMINATOR_REDUCTION = R_NOT_DENOMINATOR_REDUCTION
         SAVE_CT_LOAD_REDUCTION = .TRUE.
      RETURN

      ENTRY RETURN_RESERVE_MARGIN_RATIO(R_PEAK,R_CAPACITY,R_YEAR)
         IF(NOT_DENOMINATOR_REDUCTION) THEN
            RETURN_RESERVE_MARGIN_RATIO = R_CAPACITY/R_PEAK
         ELSE
            RETURN_RESERVE_MARGIN_RATIO = 1. + (R_CAPACITY - R_PEAK) / &
                              (R_PEAK - CT_DENOM_LOAD_REDUCTION(R_YEAR))
         ENDIF
      RETURN
      END

      SUBROUTINE SCREENING_OBJECT
      use mod_fuel_types
      use end_routine
      use SpinDriftLib
      use prod_arrays_dimensions
      use mod_fuel_types
      use rps_data
      use cla_objt_arrays
      use grxmodules
      use rptreccontrol
      use prodcom
      use cap_trans
      use mod_base_year
      use tf_decs
      USE IREC_ENDPOINT_CONTROL
      USE GRX_PLANNING_ROUTINES
      USE CAPACITY_OPTIONS_ALLOC_VARS
      USE TRANS_GROUP_VARIABLES
      USE ICAP_RESULTS  ! VARIABLE USED IN CLREPORT AND HERE
      USE ABB_CapMarketRptData
      use capacity_arrays
      USE CapOptionsFileResourceIndexes
      use pp_objt_interfaces
      use tf_objt_interfaces
      use CAPACITY_OPTIONS_ALLOC_VARS
      USE SIZECOM
      use rptreccontrol
      use grxModules
      use annual_cl_unit
      use mwunih
      use dr_booth_modules
      use rps_index_translate_mod
      use globecom
      use prodcom
      use cap_shared
      use logging
      use program_state
      use rangechecker
      use prod2com
      use debugtrace
      use program_state
      use debugtrace
      use sic_trace

      implicit none
      integer (kind=4) :: marcount=0
      integer (kind=4) :: tai_loopcount
      integer :: last_hgst, ub2
      integer :: file_trace_mxa=0 ! MX_ANNUAL report file
      integer :: file_trace_as=0
      integer :: amoc_debug_value
      integer :: file_trace_amoc=0
      integer :: file_trace_so=0
      integer :: file_trace_camp=0
      integer (kind=4) :: amoccount=0
      integer (kind=4) :: amoc2count=0
      integer :: ub
      integer (kind=4) :: write_loop_count=0
      integer (kind=2) :: co_prod_pointer
      integer (kind=2) :: GET_PA_FROM_TG ! External
      CHARACTER (LEN=2) ::  CAPACITY_PLANNING_METHOD
      CHARACTER (LEN=2) ::  GREEN_MRX_METHOD
      INTEGER (kind=2) ::   MAX_SCREENING_INTERVALS
      INTEGER (kind=2) ::   R_YEAR,upper_trans_group
      PARAMETER(MAX_SCREENING_INTERVALS = 79)
      logical :: ifres1, ifres2
      integer :: ignored_result
      LOGICAL (kind=1) ::  RESOURCE_AVAILABLE
      LOGICAL (kind=1) ::  DEPENDENT_UNIT_AVAILABLE
      LOGICAL (kind=1) ::  TEST_CAP_VALUE_W_ADD=.TRUE.
      LOGICAL (kind=1) ::  CALC_SCREEN_SLOPE_INTERCEPT
      LOGICAL (kind=1) ::  SCREENING_NOT_OPEN=.TRUE.
      LOGICAL (kind=1) ::  CAPTURED_OPTIONS_NAMES
      LOGICAL (kind=1) ::  GET_OPTION_NAMES ! External
      
      LOGICAL (kind=1) ::  RUN_MULTIAREA
      LOGICAL (kind=1) ::  RUN_PRICE_MODE
      LOGICAL (kind=1) ::  RUN_AREA_PRICE_MODE
      LOGICAL (kind=1) ::  YES_MULTI_AREA_PRICE
      LOGICAL (kind=1) ::  YES_STRICT_MARKET_PRICE
      LOGICAL (kind=1) ::  YES_RUN_MULTIAREA_TRANSACT
      LOGICAL (kind=1) ::  BUILD_TO_MARKET=.TRUE.
      LOGICAL (kind=1) ::  ANNUAL_MARKET_OPTION_SWITCH=.FALSE.
      LOGICAL (kind=1) ::  ANNUAL_RESOURCES_AVAILABLE
      LOGICAL (kind=1) ::  CX_ACTIVE=.true.
      LOGICAL (kind=1) ::  LOCAL_RESOURCES_AVAILABLE
      LOGICAL (kind=1) ::  TEST_START_STOP_YEARS
      LOGICAL (kind=1) ::  FUEL_PRICE_DATA_AVAILABLE=.FALSE.
      LOGICAL (kind=1) ::  PLANNING_PER_MWH
      LOGICAL (kind=1) ::  YES_PLAN_TO_MWH_OR_KWYR
      LOGICAL (kind=1) ::  REGIONAL_PARAMS_EXIST=.FALSE.
      LOGICAL (kind=1) ::  YES_REGIONAL_PARAMS_EXIST
      LOGICAL (kind=1) ::  MRX_SORTED_MIX
      LOGICAL (kind=1) ::  USE_EMIS_IN_MRX
      LOGICAL (kind=1) ::  YES_USE_EMIS_IN_MRX
      LOGICAL (kind=1) ::  SAME_TRANSACTION
      LOGICAL (kind=1) ::  HOUR_PATH_ACTIVE
      LOGICAL (kind=1) ::  CHRONO_MARKET_PRICES
      LOGICAL (kind=1) ::  CX_ALTER_MX_CHRONO_PRICES
      LOGICAL (kind=1) ::  CX_ANNUAL_CONTRACT
      LOGICAL (kind=1) ::  CX_DailyOperAnPumpedStorage
      LOGICAL (kind=1) ::  CX_DailyOperAnPS2
      LOGICAL (kind=1) ::  ANNUAL_STORAGE_SORT
      LOGICAL (kind=1) ::  CX_UNIT
      LOGICAL (kind=1) ::  GRX_UNIT_LINK_ACTIVE=.TRUE.
      LOGICAL (kind=1) ::  MUST_RUN_UNIT(:,:)
      LOGICAL (kind=1) ::  UPDATE_TRANS_FIXED_COSTS
      LOGICAL (kind=1) ::  OPTIMAL_ORDER
      LOGICAL (kind=1) ::  S_YES_MONTHLY_MUST_RUN
      LOGICAL (kind=1) ::  RESET_MRX_RPS_TEST_REQ
      LOGICAL (kind=1) ::  GET_RPS_NAME
      LOGICAL (kind=1) ::  RPS_CROSS_OVER
      INTEGER (kind=2) ::  RETURN_TOTAL_ALL_OPTIONS
      INTEGER (kind=2) ::  BEST_RESOURCE(MAX_SCREENING_INTERVALS)
      INTEGER (kind=2) ::  I
      INTEGER (kind=2) ::  J
      INTEGER (kind=2) ::  K
      INTEGER (kind=2) ::  L
      INTEGER (kind=2) ::  TG
      INTEGER (kind=2) ::  PA
      INTEGER (kind=2) ::  INITIALIZE_SCREEN_DATA
      INTEGER (kind=2) ::  THIS_YEAR
      INTEGER (kind=2) ::  THIS_YEAR_PLUS_LEAD_TIME(:)
      INTEGER (kind=2) ::  UPDATE_CL_SCREEN_COSTS
      INTEGER (kind=2) ::  VOID_INT2
      INTEGER (kind=2) ::  SCREENING_NO=0
      INTEGER (kind=2) ::  SCREENING_HEADER
      INTEGER (kind=2) ::  HIGHEST_OPTION
      INTEGER (kind=2) ::  NET_MARGIN_INTERVAL(:)
      INTEGER (kind=2) ::  STATE_PROVINCE(:)
      INTEGER (kind=2) ::  GET_TRANS_STATE_INDEX
      INTEGER (kind=2) ::  GET_SCREEN_STATE_INDEX
      INTEGER (kind=2) ::  LOCAL_ANNUAL_UNITS(:)
      INTEGER (kind=2) ::  LOCAL_LEAD_TIME(:)
      INTEGER (kind=2) ::  FILE_SOURCE_INDEX(:)
      INTEGER (kind=2) ::  GET_FILE_SOURCE_INDEX
      INTEGER (kind=2) ::  OPTION_POSITION(:)
      INTEGER (kind=2) ::  ACTIVE_OPTION(:)
      INTEGER (kind=2) ::  FIRST_CN(:)
      INTEGER (kind=2) ::  AO
      INTEGER (kind=2) ::  TOTAL_ACTIVE_INSTANCES
      INTEGER (kind=2) ::  AI
      INTEGER (kind=2) ::  M
      INTEGER (kind=2) ::  CX_ITER
      INTEGER (kind=2) ::  MAX_CX_ITER
      INTEGER (kind=2) ::  ACTIVE_INSTANCE(:)
      INTEGER (kind=2) ::  LAST_RESOURCE_ADDED(:)
      INTEGER (kind=2) ::  LAST_PA_RESOURCE_ADDED(:)
      INTEGER (kind=2) ::  LAST_CM_RESOURCE_ADDED(:)
      INTEGER (kind=2) ::  FIRST_CT_4_EAS_INDEX(:)
      INTEGER (kind=2) ::  LOWEST_FIXED_COST_PA(:)
      INTEGER (kind=2) ::  LAST_SORTED_POSITION=0
      INTEGER (kind=2) ::  HIGHEST_OPTION_FOR_TG(:)
      INTEGER (kind=2) ::  LSP
      INTEGER (kind=2) ::  CN
      INTEGER (kind=2) ::  ConeLink
      INTEGER (kind=2) ::  HIGHEST_INTERVAL
      INTEGER (kind=2) ::  MAX_RESOURCES_PER_YEAR_GROUP
      INTEGER (kind=2) ::  MAX_RESOURCES_PER_GROUP
      
      INTEGER (kind=2) ::  HYBRID_PROD_POINTER
      INTEGER (kind=2) ::  HYBRID_OPT_POINTER
      INTEGER (kind=2) ::  GET_TRANS_FOR_GRX_ID
      INTEGER (kind=2) ::  GET_TRANSMISSION_FOR_GRX_ID
      INTEGER (kind=2) ::  GET_GRX_RESOURCE_LINK_ID
      INTEGER (kind=2) ::  GET_TRANS_GROUP_FOR_TRANS
      INTEGER (kind=2) ::  GET_TG_FOR_TRANSMISSION_BUY
      INTEGER (kind=2) ::  GET_TG_FOR_TRANSMISSION_SELL
      INTEGER (kind=2) ::  GET_PRODUCTION_DATA_POINTER
      INTEGER (kind=2) ::  HIGHEST_J
      INTEGER (kind=2) ::  LOCAL_TIME_OF_DAY=1
      INTEGER (kind=2) ::  SELLER
      INTEGER (kind=2) ::  BUYER
      INTEGER (kind=2) ::  HR
      INTEGER (kind=2) ::  SELLER_CM
      INTEGER (kind=2) ::  BUYER_CM
      INTEGER (kind=2) ::  SELLER_PA
      INTEGER (kind=2) ::  BUYER_PA
      INTEGER (kind=2) ::  SELLER_TG
      INTEGER (kind=2) ::  BUYER_TG
      INTEGER (kind=2) ::  BEST_SELLER_TG
      INTEGER (kind=2) ::  BEST_SELLER_CM
      INTEGER (kind=2) ::  BEST_SELLER_PA
      INTEGER (kind=2) ::  BEST_BUYER_TG
      INTEGER (kind=2) ::  BEST_BUYER_CM
      INTEGER (kind=2) ::  BEST_BUYER_PA
      INTEGER (kind=2) ::  NUMBER_DIRECT_INTERCONNECTS(:)
      INTEGER (kind=2) ::  DIRECT_INTERCONNECT_TG(:,:)
      INTEGER (kind=2) ::  RETURN_SCREEN_OP_LIFE
      INTEGER (kind=2) ::  RETURN_CN_OP_LIFE
      INTEGER (kind=2) ::  OP_LIFE(:)
      integer (kind=2), allocatable :: RPS_PROGRAM_NUMBER_loc(:)
      INTEGER (kind=2) ::  GET_ICAP_SWITCH_INDEX
      INTEGER (kind=2) ::  ICAP_SWITCH_INDEX(:)
      INTEGER (kind=2) ::  U
      INTEGER (kind=2) ::  INT2_ONE=1
      INTEGER (kind=2) ::  GET_RESOURCE_ID_TO_UNIT
      INTEGER (kind=2) ::  GET_PRIMARY_MOVER_INDEX
      INTEGER (kind=2) ::  LAST_SELLER
      INTEGER (kind=2) ::  LAST_BUYER
      INTEGER (kind=2) ::  GET_TRANS_GROUP_INDEX
      INTEGER (kind=2) ::  GET_TRANS_GROUP_POSITION
      INTEGER (kind=2) ::  RETURN_S_TRANS_GROUP_ID
      INTEGER (kind=2) ::  TRANS_GROUP_4_PRICING
      INTEGER (kind=2) ::  ANNUAL_UNITS_FOR_OPTION
      INTEGER (kind=2) ::  CUMULATIVE_UNITS_FOR_OPTION
      INTEGER (kind=2) ::  MIN_UP_TIME(:)
      INTEGER (kind=2) ::  MIN_DOWN_TIME(:)
      INTEGER (kind=2) ::  MRX_RPS_MX_UNIT_CLEARED(:)
      INTEGER (kind=2) ::  GET_S_RPS_PROGRAM_NUMBER
      INTEGER (kind=2) ::  GET_TRANS_RPS_PROG_NUMBER
      INTEGER (kind=2) ::  LEAD_TIME_FOR_OPTION
      INTEGER (kind=2) ::  RPS_NO
      INTEGER (kind=2) ::  RPS_COUNT
      INTEGER (kind=2) ::  PX

      ! 6000) 080306. UP FOR NAMER
      INTEGER (kind=2) ::  HGST_OPT_INDEX(MX_RES_P_YEAR)
      INTEGER (kind=2) ::  SORTED_OPTIONS(MX_RES_P_YEAR)
      INTEGER (kind=2) ::  RPS_RESOURCE_ADDED(MX_RES_P_YEAR)
      INTEGER (kind=2) ::  LAST_SORT_INSTANCE(MX_RES_P_YEAR)
      INTEGER (kind=2) ::  TEMP_INSTANCE(MX_RES_P_YEAR)
      INTEGER (kind=2) ::  HO
      INTEGER (kind=2) ::  EU
      INTEGER (kind=2) ::  NEXT_CN
      INTEGER (kind=2) ::  LAST_HO
      INTEGER (kind=2) ::  GET_REGIONAL_CM_NAME
      INTEGER (kind=2) ::  TEMP_I2
      INTEGER (kind=2) ::  INT2_PM
      INTEGER (kind=2) ::  RPS_PM_INDEX
      INTEGER (kind=2) ::  PM
      REAL (kind=4) ::  HGST_NET_MARG_ADDED(MX_RES_P_YEAR)
      REAL (kind=4) ::  GET_S_MIN_UP_TIME
      REAL (kind=4) ::  GET_S_MIN_DOWN_TIME
      INTEGER :: SCREENING_REC
      SAVE SCREENING_REC
      REAL (kind=4) ::  ConeLinkCapacity
      REAL (kind=4) ::  CONE_AnnualFixedCosts
      REAL (kind=4) ::  BEST_SLOPE(MAX_SCREENING_INTERVALS)
      REAL (kind=4) ::  BEST_INTERCEPT(MAX_SCREENING_INTERVALS)
      REAL (kind=4) ::  LOWEST_FIXED_COST_BY_PA(:)
      REAL (kind=4) ::  LOWEST_FIXED_COST_BY_CM(:)
      REAL (kind=4) ::  MARGINAL_ICAP_PER_KWYR_BY_TG(:)
      REAL (kind=4) ::  SCREEN_SLOPE(:)
      REAL (kind=4) ::  SCREEN_INTERCEPT(:)
      REAL (kind=4) ::  SCREEN_CAPACITY(:)
      REAL (kind=4) ::  ICAP_CONE_PRICE(:)
      REAL (kind=4) ::  EQUIVALENT_CAPACITY(:)
      REAL (kind=4) ::  BLOCK_CAPACITY(:,:)
      REAL (kind=4) ::  BLOCK_HEAT_RATE(:,:)
      REAL (kind=4) ::  ANNUAL_FIXED_COST(:)
      REAL (kind=4) ::  ANNUAL_VARIABLE_COST(:)
      REAL (kind=4) ::  VARIABLE_COST_MM(:)
      REAL (kind=4) ::  FIXED_COST_MM(:)
      REAL (kind=4) ::  HIGHEST_SCREEN_CAPACITY(:)
      REAL (kind=4), save ::  NET_MARGIN(:)
      REAL (kind=4) ::  NET_MARGIN_PER_KWYR(:)
      REAL (kind=4) ::  MARKET_REVENUE(:)
      REAL (kind=4) ::  HIGHEST_ENERGY(:)
      REAL (kind=4) ::  MARKET_COST(:)
      REAL (kind=4) ::  RFT(:)
      REAL (kind=4) ::  RPM(:)
      REAL (kind=4) ::  START_UP_COSTS(:,:)
      REAL (kind=4) ::  GET_UNIT_START_UP_COSTS
      REAL (kind=4) ::  HIGHEST_RFT(:)
      REAL (kind=4) ::  HIGHEST_RPM(:)
      REAL (kind=4) ::  INTERVAL_MULTIPLIER
      REAL (kind=4) ::  R_SLOPE
      REAL (kind=4) ::  R_INTERCEPT
      REAL (kind=4) ::  SCREEN_VALUE(MAX_SCREENING_INTERVALS)
      REAL (kind=4) ::  BEST_VALUE(MAX_SCREENING_INTERVALS)
      REAL (kind=4) ::  HIGHEST_NET_MARGIN
      REAL (kind=4) ::  HIGHEST_NET_MARGIN_PER_MWH
      REAL (kind=4) ::  HIGHEST_NET_MARGIN_PER_KWYR
      REAL (kind=4) ::  HIGHEST_ICAP_DEMAND_CURVE_MULT
      REAL (kind=4) ::  HIGHEST_EAS_PROFIT_PER_KWYR
      REAL (kind=4) ::  HIGHEST_CONE_COST_PER_KWYR
      REAL (kind=4) ::  HIGHEST_CONE_DEM_ADJ_PER_KWYR
      REAL (kind=4) ::  HIGHEST_ICAP_REVENUE_PER_KWYR
      REAL (kind=4) ::  HIGHEST_CM_CAPACITY_MW
      REAL (kind=4) ::  HIGHEST_CM_RESERVE_MARGIN
      REAL (kind=4) ::  HIGHEST_CF(:)
      REAL (kind=4) ::  HIGHEST_ICAP_REVENUE
      REAL (kind=4) ::  HIGHEST_ENERGY_REVENUE
      REAL (kind=4) ::  MINIMUM_NET_MARGIN
      REAL (kind=4) ::  GET_ADDITIONS_PROFIT_PER_MWH
      REAL (kind=4) ::  GET_ANNUAL_VECTOR_VALUE
      REAL (kind=4) ::  TEMP_NET_MARGIN
      REAL (kind=4) ::  TEMP_NET_MARGIN_PER_KWYR
      REAL (kind=4) ::  TOTAL_AVERAGE_COST
      REAL (kind=4) ::  TOP_CAP_PERCENT
      REAL (kind=4) ::  GET_SCREEN_CAPACITY
      REAL (kind=4) ::  MARKET_CUM_REVENUE(:)
      REAL (kind=4) ::  MARKET_DURATION(:)
      REAL (kind=4) ::  MARKET_PRICE(:)
      REAL (kind=4) ::  STRIKE_PRICE(:,:)
      REAL (kind=4) ::  ANNUAL_LEVELIZED_CAPITAL(:)
      REAL (kind=4) ::  HIGHEST_ANN_LEVEL_CAP(:)
      REAL (kind=4) ::  ANNUAL_FUEL_COST(:)
      REAL (kind=4) ::  ANNUAL_VOM_COST
      REAL (kind=4) ::  ANNUAL_EMIS_COST
      REAL (kind=4) ::  HIGHEST_FUEL_COST(:)
      REAL (kind=4) ::  FUEL_COST_PER_MWH(:,:)
      REAL (kind=4) ::  MONTHLY_CAP_MULT(:,:)
      REAL (kind=4) ::  AVERAGE_FUEL_PRICE(:)
      REAL (kind=4) ::  VAR_OM_PER_MWH(:)
      REAL (kind=4) ::  FUEL_COST_PER_MMBTU(:)
      REAL (kind=4) ::  REVENUE_PER_MWH(:)
      REAL (kind=4) ::  HIGHEST_STRIKE_PRICE(:)
      REAL (kind=4) ::  HIGHEST_MRX_MARGIN(:)
      REAL (kind=4) ::  HIGHEST_MRX_CF(:)
      REAL (kind=4) ::  HIGHEST_MRX_ENERGY(:)
      REAL (kind=4) ::  HIGHEST_MARKET_COST(:)
      REAL (kind=4) ::  ENERGY_REVENUE_PER_MWH(:)
      REAL (kind=4) ::  HIGHEST_ENERGY_REV_PER_MWH(:)
      REAL (kind=4) ::  CAPACITY_REVENUE_PER_MWH(:)
      REAL (kind=4) ::  CAPACITY_REVENUE_MM(:)
      REAL (kind=4) ::  RPS_REVENUE_MM(:)
      REAL (kind=4) ::  RPS_REVENUE_PER_MWH(:)
      REAL (kind=4) ::  GET_RPS_REV_BY_OPTION
      REAL (kind=4) ::  PUT_RPS_PRICE_BY_OPTION
      REAL (kind=4) ::  PUT_RPS_PRICE_BY_PROGRAM
      REAL (kind=4) ::  ENERGY_REVENUE_MM(:)
      REAL (kind=4) ::  EMISSIONS_PER_MWH(:)
      REAL (kind=4) ::  FOM_PER_MWH(:)
      REAL (kind=4) ::  LEVELIZED_CAP_PER_MWH(:)
      REAL (kind=4) ::  FIXED_COST_PER_MWH(:)
      REAL (kind=4) ::  VOM_COST_MM(:)
      REAL (kind=4) ::  EMISSIONS_COST_MM(:)
      REAL (kind=4) ::  FOM_COST_MM(:)
      REAL (kind=4) ::  TRANS_GROUP_ID(:)
      REAL (kind=4) ::  TG_CAPACITY_MW(:)
      REAL (kind=4) ::  TG_PEAK_MW(:)
      REAL (kind=4) ::  FirmTransfer(:,:)
      REAL (kind=4) ::  CM_Net_Transfer(:)
      REAL (kind=4) ::  PA_Net_Transfer(:)
      REAL (kind=4) ::  TG_Net_Transfer(:)
      REAL (kind=4) ::  HIGHEST_TG_ICAP_REV_PER_KWYR(:)
      REAL (kind=4) ::  TransmissionConstraint(:,:)
      REAL (kind=4) ::  MaxTransmissionConstraint(:,:)
      REAL (kind=4) ::  GetCapTransMultiplier
      REAL (kind=4) ::  TransferValue(:,:)
      REAL (kind=4) ::  CM_INTERUPTIBLRESERVE_MARGIN
      REAL (kind=4) ::  FIRST_CT_EAS(:)
      REAL (kind=4) ::  TG_RESERVE_MARGIN(:)
      REAL (kind=4) ::  PLANNING_AREA_ID(:)
      REAL (kind=4) ::  PA_CAPACITY_MW(:)
      REAL (kind=4) ::  CM_CAPACITY_MW(:)
      REAL (kind=4) ::  PA_PEAK_MW(:)
      REAL (kind=4) ::  CM_PEAK_MW(:)
      REAL (kind=4) ::  PA_RESERVE_MARGIN(:)
      REAL (kind=4) ::  CM_RESERVE_MARGIN(:)
      REAL (kind=4) ::  CM_MAX_NET_ENRG_MARG_PER_KWYR(:)
      REAL (kind=4) ::  HEAT_RATE(:)
      REAL (kind=4) ::  HIGHEST_HEAT_RATE(:)
      REAL (kind=4) ::  HIGHEST_ANNUAL_UNITS(:)
      REAL (kind=4) ::  HIGHEST_MARKET_REVENUE(:)
      REAL (kind=4) ::  HIGHEST_MARGIN_PER_KWYR(:)
      REAL (kind=4) ::  HIGHEST_FUEL_COST_PER_MWH(:)
      REAL (kind=4) ::  HIGHEST_VAR_OM_PER_MWH(:)
      REAL (kind=4) ::  HIGHEST_FUEL_COST_PER_MMBTU(:)
      REAL (kind=4) ::  HIGHEST_EMISSIONS_PER_MWH(:)
      REAL (kind=4) ::  HIGHEST_FOM_PER_MWH(:)
      REAL (kind=4) ::  HIGHEST_LEVEL_CAP_PER_MWH(:)
      REAL (kind=4) ::  HIGHEST_VOM_COST_MM(:)
      REAL (kind=4) ::  HIGHEST_EMISSIONS_COST_MM(:)
      REAL (kind=4) ::  HIGHEST_FOM_COST_MM(:)
      REAL (kind=4) ::  FIX_OM_COST(:)
      REAL (kind=4) ::  CAPACITY_PLANNING_MW(:)
      REAL (kind=4) ::  GET_TRANS_RPS_PERCENT
      REAL (kind=4) ::  GET_S_RPS_PERCENT
      REAL (kind=4) ::  RPS_PERCENT(:)
      REAL (kind=4) ::  GET_TRANS_CAP_PLAN_FAC
      REAL (kind=4) ::  FIRST_CF(:)
      REAL (kind=4) ::  CF_FACTOR
      REAL (kind=4) ::  RETURN_OTHER_VAR_COMPONENT
      REAL (kind=4) ::  RETURN_CL_SCREEN_FIXED_COST
      REAL (kind=4) ::  GET_S_CAP_PLANNING_FAC
      REAL (kind=4) ::  MARKET_HOURS
      REAL (kind=4) ::  MARKET_AVE_REVENUE
      REAL (kind=4) ::  CF
      REAL (kind=4) ::  ANNUAL_ENERGY
      REAL (kind=4) ::  ANNUAL_STRIKES
      REAL (kind=4) ::  RETURN_RISK_ADJ_S_CAP_COST
      REAL (kind=4) ::  RETURN_CL_SCREEN_FUEL_COST
      REAL (kind=4) ::  RETURN_CL_SCREEN_EMIS_COST
      REAL (kind=4) ::  HIGHEST_TOP_CAP_PERCENT
      REAL (kind=4) ::  FIRST_YEAR_CAPACITY
      REAL (kind=4) ::  ADD_THIS_UNIT
      REAL (kind=4) ::  PEAK
      REAL (kind=4) ::  UPDATE_NET_PLANNING_PEAK
      REAL (kind=4) ::  GET_VAR
      REAL (kind=4) ::  RETURN_CL_SCREEN_EFOR
      REAL (kind=4) ::  SCENARIO_CAP_COST_MULT
      REAL (kind=4) ::  GET_SCENARIO_EXPAND_CAP_COST
      REAL (kind=4) ::  SCENARIO_RESERVE_MARGIN
      REAL (kind=4) ::  GET_SCENARIO_RESERVE_MARGIN
      REAL (kind=4) ::  GET_ICAP_REVENUE_MULT
      REAL (kind=4) ::  ANNUAL_ICAP_REVENUE
      REAL (kind=4) ::  ANNUAL_ENERGY_REVENUE(:)
      REAL (kind=4) ::  NOX_VOM
      REAL (kind=4) ::  NOX_FOM
      REAL (kind=4) ::  SOX_VOM
      REAL (kind=4) ::  SOX_FOM
      REAL (kind=4) ::  CO2_VOM
      REAL (kind=4) ::  CO2_FOM
      REAL (kind=4) ::  HG_VOM
      REAL (kind=4) ::  HG_FOM
      REAL (kind=4) ::  OTHER3_VOM
      REAL (kind=4) ::  OTHER3_FOM
      REAL (kind=4) ::  NOX_CONTROL_MULT
      REAL (kind=4) ::  SOX_CONTROL_MULT
      REAL (kind=4) ::  CO2_CONTROL_MULT
      REAL (kind=4) ::  HG_CONTROL_MULT
      REAL (kind=4) ::  OTHER3_CONTROL_MULT
      REAL (kind=4) ::  INSTALL_CAP_VALUE_LAST_PA_UNIT(:)
      REAL (kind=4) ::  INSTALL_CAP_VALUE_LAST_CM_UNIT(:)
      REAL (kind=4) ::  R_MRX_CAPACITY
      REAL (kind=4) ::  R_MRX_ENERGY
      REAL (kind=4) ::  R_MRX_TOTAL_REVENUE
      REAL (kind=4) ::  R_MRX_TOTAL_VARIABLE
      REAL (kind=4) ::  R_MRX_FUEL
      REAL (kind=4) ::  R_MRX_VOM
      REAL (kind=4) ::  R_MRX_EMISSIONS
      REAL (kind=4) ::  R_MRX_TOTAL_COST
      REAL (kind=4) ::  R_MRX_ANN_CAP_COST
      REAL (kind=4) ::  R_MRX_NET_MARGIN_MM
      REAL (kind=4) ::  R_MRX_NET_MARGIN_PER_MWH
      REAL (kind=4) ::  R_MRX_NET_MARGIN_PER_KWMO
      REAL (kind=4) ::  R_MRX_CONE_PER_KWMO
      REAL (kind=4) ::  R_MRX_EAS_REV_PER_KWMO
      REAL (kind=4) ::  R_MRX_ICAP_VALUE_POINT
      REAL (kind=4) ::  PRICE_OF_TRANSFER
      REAL (kind=4) ::  PRICE_CEILING
      REAL (kind=4) ::  QUANTITY_OF_TRANSFER
      REAL (kind=4) ::  R_TG_TRANSFER_CAPACITY
      REAL (kind=4) ::  MRX_ICAP_INDEX(:)
      REAL (kind=4) ::  LOCAL_CHRONO_MARKET_PRICES(:,:)
      REAL (kind=4) ::  RESET_CHRONO_MARKET_PRICES(:,:)
      REAL (kind=4) ::  LOCAL_CHRONO_MW_USAGE(8784)
      REAL (kind=4) ::  LOCAL_CHRONO_STRIKE_PRICE(8784)
      REAL (kind=4) ::  HIGHEST_CHRONO_MW_USAGE(8784)
      REAL (kind=4) ::  LOCAL_HYBRID_MW_CHARGE(8784)
      REAL (kind=4) ::  INDEPENDENT_MW_USAGE(8784)
      REAL (kind=4) ::  ADD_HYBRID_MW_CHARGE(8784)
      REAL (kind=4) ::  LOCAL_HYBRID_MW_DISCHARGE(8784)
      REAL (kind=4) ::  LOCAL_HYBRID_HR_MARGIN(8784)
      REAL (kind=4) ::  CONSTRAIN_HYBRID_MARGIN(8784)
      REAL (kind=4) ::  CONSTRAIN_HYBRID_MW_USAGE(8784)
      REAL (kind=4) ::  MARKET_HYBRID_MW_USAGE(8784)
      REAL (kind=4) ::  HYBRID_MARKET_MW_USAGE(8784)
      REAL (kind=4) ::  LOCAL_HYBRID_PROFIT(8784)
      REAL (kind=4) ::  INDEPENDENT_ANNUAL_ENERGY
      REAL (kind=4) ::  ANNUAL_CHARGE
      REAL (kind=4) ::  CONSTRAIN_ANNUAL_ENERGY
      REAL (kind=4) ::  CONSTRAIN_ANNUAL_CHARGE
      REAL (kind=4) ::  HYBRID_MARKET_ANNUAL_ENERGY
      REAL (kind=4) ::  HYBRID_MARKET_ANNUAL_CHARGE
      REAL (kind=4) ::  HYBRID_MARKET_ANNUAL_REVENUE
      REAL (kind=4) ::  INDEPENDENT_FUEL_COST
      REAL (kind=4) ::  INDEPENDENT_VOM_COST
      REAL (kind=4) ::  CONSTRAIN_VOM_COST
      REAL (kind=4) ::  HYBRID_MARKET_VOM_COST
      REAL (kind=4) ::  INDEPENDENT_ANNUAL_REVENUE
      REAL (kind=4) ::  CONSTRAIN_ANNUAL_REVENUE
      REAL (kind=4) ::  INDEPENDENT_EMIS_COST
      REAL (kind=4) ::  HYBRID_MARKET_ENERGY_LIM
      REAL (kind=4) ::  CALC_GRX_FIX_COST_PER_UNIT
      CHARACTER (len=5) ::   GET_SCENAME,CL_UNIQUE_RPT_STR
      CHARACTER (len=20) ::  OPTION_NAME(:)
      CHARACTER (len=20) ::  PA_NAME
      CHARACTER (len=20) ::  CX_NAME
      CHARACTER (len=20) ::  RPS_NAME*50
      CHARACTER (len=25) ::  CURVE_NAME,TEMP_CURVE_NAME
      CHARACTER (len=35) ::  MULTI_AREA_NAME(:)
      CHARACTER (len=35) ::  GET_GROUP_NAME
      CHARACTER (len=35) ::  CM_NAME
      CHARACTER (len=22) ::  LOCAL_NAME
      CHARACTER (len=256) :: TEMP_STR
      ALLOCATABLE :: SCREEN_SLOPE,SCREEN_INTERCEPT,OPTION_NAME, &
                        OPTION_POSITION, &
                        ACTIVE_OPTION, &
                        FIRST_CN, &
                        ACTIVE_INSTANCE, &
                        LAST_RESOURCE_ADDED, &
                        LAST_PA_RESOURCE_ADDED, &
                        LAST_CM_RESOURCE_ADDED, &
                        FIRST_CT_EAS, &
                        FIRST_CT_4_EAS_INDEX, &
                        LOWEST_FIXED_COST_PA, &
                        LOWEST_FIXED_COST_BY_PA, &
                        LOWEST_FIXED_COST_BY_CM, &
                        MARGINAL_ICAP_PER_KWYR_BY_TG, &
                        INSTALL_CAP_VALUE_LAST_PA_UNIT, &
                        INSTALL_CAP_VALUE_LAST_CM_UNIT, &
                        SCREEN_CAPACITY, &
                        ANNUAL_ENERGY_REVENUE, &
                        EQUIVALENT_CAPACITY, &
                        BLOCK_CAPACITY, &
                        BLOCK_HEAT_RATE, &
                        ANNUAL_FIXED_COST, &
                        ANNUAL_VARIABLE_COST, &
                        VARIABLE_COST_MM, &
                        FIXED_COST_MM, &
                        HIGHEST_SCREEN_CAPACITY, &
                        NET_MARGIN, &
                        NET_MARGIN_PER_KWYR, &
                        NET_MARGIN_INTERVAL, &
                        THIS_YEAR_PLUS_LEAD_TIME, &
                        MARKET_REVENUE, &
                        HIGHEST_ENERGY, &
                        MARKET_COST, &
                        MARKET_CUM_REVENUE, &
                        MARKET_DURATION, &
                        MARKET_PRICE, &
                        STRIKE_PRICE, &
                        STATE_PROVINCE, &
                        LOCAL_ANNUAL_UNITS, &
                        RFT, &
                        MIN_UP_TIME, &
                        MIN_DOWN_TIME, &
                        START_UP_COSTS, &
                        MUST_RUN_UNIT, &
                        RPM, &
                        HIGHEST_RFT, &
                        HIGHEST_RPM, &
                        LOCAL_LEAD_TIME, &
                        FILE_SOURCE_INDEX, &
                        ANNUAL_LEVELIZED_CAPITAL, &
                        OP_LIFE, &
                        ICAP_SWITCH_INDEX, &
                        ICAP_CONE_PRICE, &
                        HIGHEST_ANN_LEVEL_CAP, &
                        ANNUAL_FUEL_COST, &
                        HIGHEST_FUEL_COST, &
                        FUEL_COST_PER_MWH, &
                        MONTHLY_CAP_MULT, &
                        AVERAGE_FUEL_PRICE, &
                        VAR_OM_PER_MWH, &
                        FUEL_COST_PER_MMBTU, &
                        REVENUE_PER_MWH, &
                        CAPACITY_REVENUE_PER_MWH, &
                        CAPACITY_REVENUE_MM, &
                        RPS_REVENUE_MM, &
                        MRX_RPS_MX_UNIT_CLEARED, &
                        RPS_REVENUE_PER_MWH, &
                        ENERGY_REVENUE_MM, &
                        ENERGY_REVENUE_PER_MWH, &
                        HIGHEST_ENERGY_REV_PER_MWH, &
                        HIGHEST_STRIKE_PRICE, &
                        HIGHEST_MRX_MARGIN, &
                        HIGHEST_MRX_ENERGY, &
                        HIGHEST_MARKET_COST, &
                        HIGHEST_MRX_CF, &
                        EMISSIONS_PER_MWH, &
                        FOM_PER_MWH, &
                        LEVELIZED_CAP_PER_MWH, &
                        FIXED_COST_PER_MWH, &
                        MRX_ICAP_INDEX, &
                        LOCAL_CHRONO_MARKET_PRICES, &
                        RESET_CHRONO_MARKET_PRICES, &
                        VOM_COST_MM, &
                        EMISSIONS_COST_MM, &
                        FOM_COST_MM, &
                        TRANS_GROUP_ID, &
                        TG_CAPACITY_MW, &
                        TG_PEAK_MW, &
                        FirmTransfer, &
                        CM_Net_Transfer, &
                        TG_Net_Transfer, &
                        PA_Net_Transfer, &
                        NUMBER_DIRECT_INTERCONNECTS, &
                        DIRECT_INTERCONNECT_TG, &
                        HIGHEST_TG_ICAP_REV_PER_KWYR, &
                        HIGHEST_OPTION_FOR_TG, &
                        TransmissionConstraint, &
                        MaxTransmissionConstraint, &
                        TransferValue, &
                        TG_RESERVE_MARGIN, &
                        PLANNING_AREA_ID, &
                        PA_CAPACITY_MW, &
                        CM_CAPACITY_MW, &
                        PA_PEAK_MW, &
                        CM_PEAK_MW, &
                        PA_RESERVE_MARGIN, &
                        CM_RESERVE_MARGIN, &
                        CM_MAX_NET_ENRG_MARG_PER_KWYR, &
                        HIGHEST_CF, &
                        HEAT_RATE, &
                        HIGHEST_HEAT_RATE, &
                        HIGHEST_ANNUAL_UNITS, &
                        HIGHEST_MARKET_REVENUE, &
                        HIGHEST_MARGIN_PER_KWYR, &
                        HIGHEST_FUEL_COST_PER_MWH, &
                        HIGHEST_VAR_OM_PER_MWH, &
                        HIGHEST_FUEL_COST_PER_MMBTU, &
                        HIGHEST_EMISSIONS_PER_MWH, &
                        HIGHEST_FOM_PER_MWH, &
                        HIGHEST_LEVEL_CAP_PER_MWH, &
                        HIGHEST_VOM_COST_MM, &
                        HIGHEST_EMISSIONS_COST_MM, &
                        HIGHEST_FOM_COST_MM, &
                        FIX_OM_COST, &
                        CAPACITY_PLANNING_MW, &
                        RPS_PERCENT, &
                        FIRST_CF, &
                        MULTI_AREA_NAME
         SAVE &
               ANNUAL_FUEL_COST, &
               OPTION_NAME, &
               OPTION_POSITION, &
               LAST_RESOURCE_ADDED, &
               LAST_PA_RESOURCE_ADDED, &
               LAST_CM_RESOURCE_ADDED, &
               FIRST_CT_EAS, &
               FIRST_CT_4_EAS_INDEX, &
               LOWEST_FIXED_COST_PA, &
               LOWEST_FIXED_COST_BY_PA, &
               LOWEST_FIXED_COST_BY_CM, &
               MARGINAL_ICAP_PER_KWYR_BY_TG, &
               INSTALL_CAP_VALUE_LAST_PA_UNIT, &
               INSTALL_CAP_VALUE_LAST_CM_UNIT, &
               HIGHEST_NET_MARGIN, & !  3
               VARIABLE_COST_MM, & !  7
               FIXED_COST_MM, & !  8
               HIGHEST_SCREEN_CAPACITY, & !  9
               HIGHEST_ENERGY, & !  10
               HIGHEST_ANN_LEVEL_CAP, & !  12
               HIGHEST_FUEL_COST, & !  13
               MARKET_REVENUE, & !  16
               NET_MARGIN_PER_KWYR, & !  17
               VOM_COST_MM, & !  26
               EMISSIONS_COST_MM ! 27
!
      LOGICAL (kind=1) ::  MX_SUMMARY_REPORT
      LOGICAL (kind=1) ::  EXPANSION_REPORT
      LOGICAL (kind=1) ::  MX_REPORT_NOT_OPEN=.TRUE.
      LOGICAL (kind=1) ::  YES_INCLUDE_ICAP_REVENUE
      LOGICAL (kind=1) ::  INCLUDE_ICAP_REVENUE
      LOGICAL (kind=1) ::  VN_REPORT_NOT_OPEN=.TRUE.
      LOGICAL (kind=1) ::  RPS_CURVE_REPORT
      LOGICAL (kind=1) ::  YES_RPS_CURVE_REPORT
      INTEGER (kind=2) ::  MX_REPORT_VARIABLES=53
      INTEGER (kind=2) ::  MX_ANNUAL_ALT_NO=0
      INTEGER (kind=2) ::  MX_ANNUAL_HEADER
      INTEGER (kind=2) ::  VN_REPORT_VARIABLES=9
      INTEGER (kind=2) ::  VN_ANNUAL_ALT_NO=0
      INTEGER (kind=2) ::  RPS_CURVE_RPT_HEADER
      INTEGER ::  MX_ANNUAL_ALT_REC
      INTEGER ::  VN_ANNUAL_ALT_REC
      SAVE MX_ANNUAL_ALT_REC, &
                  VN_ANNUAL_ALT_REC
!
      LOGICAL (kind=1) ::  GRX_8761_REPORT
      LOGICAL (kind=1) ::  GRX_8761_REPORT_ACTIVE
      LOGICAL (kind=1) ::  GRX_8761_REPORT_NOT_OPEN=.TRUE.
      INTEGER (kind=2) ::  GRX_8761_REPORT_VARIABLES
      INTEGER (kind=2) ::  GRX_8761_ALT_NO=0
      INTEGER (kind=2) ::  GRX_8761_HEADER
      INTEGER :: GRX_8761_ALT_REC=0 ,ITER,MAX_ITER
      SAVE GRX_8761_ALT_REC
!
      LOGICAL (kind=1) ::  FUEL_POINTERS_USED
      LOGICAL (kind=1) ::  EXISTING_UNIT
      LOGICAL (kind=1) ::  IS_IN_RPS_PROGRAM
      INTEGER (kind=2) ::  N
      INTEGER (kind=2) ::  FT
      INTEGER (kind=2) ::  MO
      INTEGER (kind=2) ::  GET_NUNITS_POSITION
      INTEGER (kind=2) ::  FUEL_INVENTORY_ID(0:1024)
      INTEGER (kind=2) ::  GET_PRIMARY_MOVER
      INTEGER (kind=2) ::  GET_S_PRIMARY_MOVER
      INTEGER (kind=2) ::  GET_S_PRIMARY_MOVER_INDEX
      INTEGER (kind=2) ::  GET_DERIV_PRIM_MOVER
      INTEGER (kind=2) ::  GET_DERIV_PRIM_MOVER_INDEX
      INTEGER (kind=2) ::  CM_UNIT_NO
      INTEGER (kind=2) ::  ST
      INTEGER (kind=2) ::  NUM_RPS_REGIONS
      INTEGER (kind=2) ::  REGION
      INTEGER (kind=2) ::  RPS_PROGRAMS
      INTEGER (kind=2) ::  NUMBER_OF_RPS_PROGRAMS
      INTEGER (kind=2) ::  GET_RPS_PROGRAM_POSITION
      INTEGER (kind=2) ::  RPS_SYSTEM
      INTEGER (kind=2) ::  RPS_STATE_REGION(0:61)
      INTEGER (kind=2) ::  RPS_REGION_POSITION(0:99)
      INTEGER (kind=2) ::  OPTION_REGION
      INTEGER (kind=2) ::  D
      INTEGER (kind=2) ::  LAST_EU
      INTEGER (kind=2) ::  BID_POS
      INTEGER (kind=2) ::  ED
      INTEGER (kind=2) ::  LAST_ED
      INTEGER (kind=2) ::  LAST_EN
      INTEGER (kind=2) ::  EN
      INTEGER (kind=2) ::  FN
      REAL (kind=4) ::  GET_SCENARIO_COAL_PRICES
      REAL (kind=4) ::  GET_SCENARIO_GAS_PRICES
      REAL (kind=4) ::  GET_SCENARIO_OIL_PRICES
      REAL (kind=4) ::  GET_SCENARIO_URANIUM_PRICES
      REAL (kind=4) ::  DISP_BTU_COST(MAX_CL_UNITS)
      REAL (kind=4) ::  BLENDED_BTU_COST(MAX_CL_UNITS)
      REAL (kind=4) ::  FUEL_SCEN_MULT(5)
      REAL (kind=4) ::  RTEMP
      REAL (kind=4) ::  PUT_MRX_DELIVERY_COST
      REAL (kind=4) ::  GET_MONTHLY_FUEL_INDEX
      REAL (kind=4) ::  R_DELIVERY_COST
      REAL (kind=4) ::  VOID_R4
      REAL (kind=4) ::  MAXIMUM_ANNUAL_MRX_CAP(:)
      REAL (kind=4) ::  PA_ICAP_REVENUE_MULT(:)
      REAL (kind=4) ::  CM_ICAP_REVENUE_MULT(:)
      REAL (kind=4) ::  MINIMUM_ANNUAL_MRX_CAP(:)
      REAL (kind=4) ::  GET_MIN_CAP_TESTING_RATIO
      REAL (kind=4) ::  GET_MAX_CAP_TESTING_RATIO
      REAL (kind=4) ::  MINIMUM_TG_MRX_CAP(:)
      REAL (kind=4) ::  MAXIMUM_TG_MRX_CAP(:)
      REAL (kind=4) ::  GET_MAXIMUM_ANNUAL_MRX_CAP
      REAL (kind=4) ::  TOTAL_ANNUAL_CAPACITY_ADDED(:)
      REAL (kind=4) ::  CM_TOT_ANN_CAPACITY_ADDED(:)
      REAL (kind=4) ::  TG_TOTAL_ANNUAL_CAPACITY_ADDED(:)
      REAL (kind=4) ::  GET_CM_RETIREMENT_KW
      REAL (kind=4) ::  GET_NEW_UNIT_RETIRE_VALUE_BY_CM
      REAL (kind=4) ::  ANN_CAP
      REAL (kind=4) ::  EL_PLANNING_CAPACITY
      REAL (kind=4) ::  CL_PLANNING_CAPACITY
      REAL (kind=4) ::  CT_PLANNING_CAPACITY
      REAL (kind=4) ::  LM_PLANNING_CAPACITY
      REAL (kind=4) ::  ADJUSTMENT_CAPACITY
      REAL (kind=4) ::  DERIV_CAPACITY_PLANNING
      REAL (kind=4) ::  DERIV_CAPACITY_PLANNING_BY_TG
      REAL (kind=4) ::  DERIV_NEW_CAP_PLAN_BY_ALL
      REAL (kind=4) ::  CURRENT_TARGET_RATIO
      REAL (kind=4) ::  CURRENT_TARGET_RATIO_PA(:)
      REAL (kind=4) ::  TARGET_RATIO
      REAL (kind=4) ::  RETURN_RESERVE_MARGIN_RATIO
      REAL (kind=4) ::  MAXIMUM_RM
      REAL (kind=4) ::  MAXIMUM_RM_PA(:)
      REAL (kind=4) ::  MAX_CAPACITY_TESTING_MARGIN
      REAL (kind=4) ::  ICAP_MIN_TESTING_MARGIN
      REAL (kind=4) ::  ICAP_MAX_TESTING_MARGIN
      REAL (kind=4) ::  TOTAL_REGIONAL_CAPACITY_NEEDED
      REAL (kind=4) ::  GET_EMISS_CAP_FOR_CLASS
      REAL (kind=4) ::  CO2_EMISSIONS_CAP(:)
      REAL (kind=4) ::  CO2_EMISS_REDUCTION_REQUIRED(:)
      REAL (kind=4) ::  CO2_EMISSIONS(:)
      REAL (kind=4) ::  GET_CL_EMISS_FOR_CLASS
      REAL (kind=4) ::  CO2_UNIT_MW
      REAL (kind=4) ::  CO2_UNIT_MWH
      REAL (kind=4) ::  CO2_UNIT_CO2
      REAL (kind=4) ::  UNIT_CO2
      REAL (kind=4) ::  CO2_UNIT_PRICE
      REAL (kind=4) ::  CO2_STRIKE_PRICE
      REAL (kind=4) ::  CO2_STRIKE_PRICE_AFTER
      REAL (kind=4) ::  CO2_UNIT_MW_AFTER
      REAL (kind=4) ::  UNIT_CO2_AFTER
      REAL (kind=4) ::  GET_CL_CO2_TON_PER_MWH
      REAL (kind=4) ::  LOWEST_TRANSFER_COST
      REAL (kind=4) ::  LOWEST_TRANSFER_CAPACITY
      REAL (kind=4) ::  STATE_SURPLUS_VARS(0:7)
      REAL (kind=4) ::  SYSTEM_SURPLUS_VARS(0:7)
      REAL (kind=4) ::  RPS_PROGRAM_BALANCE
      REAL (kind=4) ::  RPS_ALT_PRICE
      REAL (kind=4) ::  RPS_ALT_CAP_PRICE
      REAL (kind=4) ::  BREAK_EVEN_REV_REQ
      REAL (kind=4) ::  RPS_REC_PRICE
      REAL (kind=4) ::  RPS_GEN_WEIGHTED_REV
      REAL (kind=4) ::  RPS_GEN_WEIGHTED_PRICE
      REAL (kind=4) ::  RPS_ACP_REV
      REAL (kind=4) ::  CAPACITY_REV
      REAL (kind=4) ::  GET_LOCAL_ENDING_BALANCE
      REAL (kind=4) ::  GET_ANNUAL_RPS_PROG_REQ
      REAL (kind=4) ::  PROG_REQ
      REAL (kind=4) ::  ZERO_BID_MWH
      REAL (kind=4) ::  GET_QUALIFYING_GEN_DB
      REAL (kind=4) ::  GET_RPS_ALT_COMPLIANCE_PRICE
      REAL (kind=4) ::  RPS_ALT_COMPLIANCE_PRICE
      REAL (kind=4) ::  MIN_BID
      REAL (kind=4) ::  MAX_BID
      REAL (kind=4) ::  NEW_BID
      REAL (kind=4) ::  EXISTING_CL_BID
      REAL (kind=4) ::  EXISTING_DV_BID
      REAL (kind=4) ::  TEMP_BID
      REAL (kind=4) ::  TEMP_REVENUE
      REAL (kind=4) ::  TEMP_COST
      REAL (kind=4) ::  TEMP_MWH
      REAL (kind=4) ::  CUMULATIVE_MWH
      REAL (kind=4) ::  PA_BID(30000)
      REAL (kind=4) ::  PA_COST(30000)
      REAL (kind=4) ::  PA_REVENUE(30000)
      REAL (kind=4) ::  PA_CUM_MWH(30000)
      CHARACTER (len=1) ::   USE_MINIMUM_RM
      LOGICAL (kind=1) ::  USE_MAXIMUM_RM_LOGIC
      LOGICAL (kind=1) ::  USE_MINIMUM_RM_LOGIC
      LOGICAL (kind=1) ::  USE_REGIONAL_MINIMUM_RM_LOGIC
      LOGICAL (kind=1) ::  USE_REGIONAL_MAXIMUM_RM_LOGIC
      LOGICAL (kind=1) ::  TARGET_MET
      LOGICAL (kind=1) ::  YES_USE_MINIMUM_RM
      LOGICAL (kind=1) ::  VOID_LOGICAL
      LOGICAL (kind=1) ::  RESOURCE_ADDITION
      LOGICAL (kind=1) ::  TEMP_L
      LOGICAL (kind=1) ::  CALC_MRX_RPS_CURVES
      LOGICAL (kind=1) ::  GET_TRANS_NAME
      LOGICAL (kind=1) ::  STORAGE_IS_ACTIVE=.TRUE.
      LOGICAL (kind=1) ::  CONTRACT_IS_STORAGE
      LOGICAL (kind=1) ::  GET_S_BLOCK_CAP_AND_HR
      LOGICAL (kind=1) ::  INIT_HOUR_PATH_LIMIT
      LOGICAL (kind=1) ::  ADJUST_CO2_RETRO_PLAN_CAP

      LOGICAL (kind=1) ::  CO2_END_LIST
      LOGICAL (kind=1) ::  GET_CO2_RETIREMENTS_LOGIC
      LOGICAL (kind=1) ::  CO2_RETIREMENTS_LOGIC
      LOGICAL (kind=1) ::  GET_NEXT_MRX_RETIRE_RETRO
      LOGICAL (kind=1) ::  RETIRE_CO2_THERMAL_UNIT
      LOGICAL (kind=1) ::  RETROFIT_CO2_THERMAL_UNIT
      LOGICAL (kind=1) ::  TARGET_IS_PA
      LOGICAL (kind=1) ::  TG_MW_TRANSFERS
      LOGICAL (kind=1) ::  GRX_RPS_MODULE_IS_ACTIVE
      LOGICAL (kind=1) ::  GRX_RPS_MODULE_ACTIVE
      LOGICAL (kind=1) ::  RPS_ONLY_SWITCH_ACTIVE
      LOGICAL (kind=1) ::  RPS_ONLY_SWITCH

!
! DECLARATION FOR RESERVE MARGIN BY TG
!
      

      INTEGER (kind=2) :: &
           GET_CM_FROM_TG, &
           TG_2_PLANNING_AREA(:), &
           TG_2_CAPACITY_MARKET(:), &
           PG,CG,CM,CM_INDEX, &
           GET_CM_INDEX_FROM_TG, &
           R_PA, &
           R_TG, &
           TEMP_PA, &
           TEMP_CM, &
           GET_NUMBER_OF_PLANNING_GROUPS, &
           GET_NUMBER_OF_CAPACITY_MARKETS, &
           CO2_PLN=3 , &
           CO2_COUNTER, &
           CO2_RETIRE_OR_RETRO, &
           CO2_NUNIT, &
           CO2_TG, &
           BASE_DATE, &
           ZERO_I2=0
           
      REAL ::  PA_PLANNING_CAPACITY(:)
      REAL ::  CM_PLANNING_CAPACITY(:)
      REAL ::  CM_IOTA(:)
      REAL ::  IOTA_BY_CN(:)
      REAL ::  TG_PLANNING_CAPACITY(:)
      REAL ::  PEAK_AFTER_INTERRUPTIBLE_TG(:)
      REAL ::  TransferConvergence=0.01
      REAL ::  SELLER_CAPACITY
      REAL ::  BUYER_CAPACITY
      REAL ::  I_CAPACITY
      REAL ::  J_CAPACITY
      REAL ::  TEMP_R
      REAL ::  MaxTransferValue
      REAL ::  HOUR_TIE_LIMIT
      REAL ::  R_RESERVE
      REAL ::  TG_PLANNING_RESERVE_MARGIN(:)
      REAL ::  PA_PEAK_AFTER_INTERRUPTIBLE(:)
      REAL ::  CM_PEAK_AFTER_INTERRUPTIBLE(:)
      REAL ::  EXISTING_PLANNING_CAPACITY(0:6)
      REAL ::  NEW_PLANNING_CAPACITY(0:6)
      REAL ::  AFTER_PEAK_CAPACITY
      REAL ::  RETIREMENT_CAPACITY
      REAL ::  GET_ANNUAL_PEAK
      REAL ::  ENRG_LIMITED_COST
      REAL ::  PA_PLANNING_RESERVE_MARGIN(:)
      REAL, allocatable ::  CM_PLANNING_RESERVE_MARGIN(:)

      REAL ::  PLANNING_RM_BEFORE_ADDITIONS
      REAL ::  PLANNING_PEAK_BEFORE_ADDITIONS
      REAL ::  GET_CL_TG_CAP
      REAL ::  GET_GROUP_PEAK_ON_PEAK_MONTH
      REAL ::  GET_EL_TG_CAP
      REAL ::  GET_CL_AFTER_PEAK
      REAL ::  GET_CL_TG_RETIRE
      REAL ::  GET_PEAK_ON_PA_PEAK_MONTH
      REAL ::  ICAP_DEMAND_CURVE_MULT_K
      REAL ::  EAS_PROFIT_PER_KWYR_K
      REAL ::  CONE_COST_PER_KWYR_K
      REAL ::  CONE_DEM_CURVE_ADJ_PER_KWYR_K
      REAL ::  ICAP_REVENUE_PER_KWYR_K
      REAL ::  CAPACITY_REVENUE_MM_K
      REAL ::  IOTA=0.0
      REAL ::  Y_BEFORE_SELLER
      REAL ::  X_BEFORE_SELLER
      REAL ::  Y_BEFORE_BUYER
      REAL ::  X_BEFORE_BUYER
      REAL ::  X_AFTER_SELLER
      REAL ::  Y_AFTER_SELLER
      REAL ::  X_AFTER_BUYER
      REAL ::  Y_AFTER_BUYER
      REAL ::  B_BEFORE
      REAL ::  A_BEFORE
      REAL ::  B_AFTER
      REAL ::  A_AFTER
      REAL ::  X_FINAL
      REAL ::  Y_FINAL
      REAL ::  X_SEARCH
      REAL ::  HALF
      ALLOCATABLE :: PA_PLANNING_RESERVE_MARGIN, &
                     PA_PEAK_AFTER_INTERRUPTIBLE, &
                     PEAK_AFTER_INTERRUPTIBLE_TG, &
                     TG_PLANNING_RESERVE_MARGIN, &
                     CM_PEAK_AFTER_INTERRUPTIBLE, &
                     PA_PLANNING_CAPACITY, &
                     CM_PLANNING_CAPACITY, &
                     CM_IOTA, &
                     IOTA_BY_CN, &
                     TG_PLANNING_CAPACITY, &
                     TOTAL_ANNUAL_CAPACITY_ADDED, &
                     CM_TOT_ANN_CAPACITY_ADDED, &
                     TG_TOTAL_ANNUAL_CAPACITY_ADDED, &
                     MAXIMUM_ANNUAL_MRX_CAP, &
                     PA_ICAP_REVENUE_MULT, &
                     CM_ICAP_REVENUE_MULT, &
                     MINIMUM_ANNUAL_MRX_CAP, &
                     MINIMUM_TG_MRX_CAP, &
                     MAXIMUM_TG_MRX_CAP, &
                     CO2_EMISSIONS_CAP, &
                     CO2_EMISS_REDUCTION_REQUIRED, &
                     CO2_EMISSIONS, &
                     TG_2_PLANNING_AREA, &
                     TG_2_CAPACITY_MARKET, &
                     CURRENT_TARGET_RATIO_PA, &
                     MAXIMUM_RM_PA
      REAL :: &
            INTERRUPTIBLE_LOAD, &
            GET_ANNUAL_INTER_CAPACITY, &
            ZERO,NEAR_ZERO, &
            CAPACITY_PLANNING_PEAK, &
            TEMP_PEAK, &
            CO2_RETIREMENT_PRICE, &
            GET_CO2_RETIREMENT_PRICE, &
            TEMP_R4, &
            BASE_CAP_REV_MM, &
            MAX_CAP_REV_MM, &
            MAX_RPS_REV_MM, &
            ENERGY_UP_LIFT_MM
      PARAMETER (ZERO=0.,NEAR_ZERO=.0000001)
      SAVE PA_PLANNING_RESERVE_MARGIN,CM_PLANNING_RESERVE_MARGIN
      REAL (kind=8) ::  CO2_COEFF
      REAL (kind=8) ::  CO2_POWER
      REAL (kind=8) ::  CO2_PERCENT_OF_CAP
      INTEGER (KIND=2) ::  MAX_ID_RESOURCE_OPTIONS
      INTEGER (KIND=2) ::  OPTIONS_MAX_RESOURCE_ID
      INTEGER (KIND=2) , ALLOCATABLE :: ResourceIDtoOptionPosPrt(:)
      INTEGER (KIND=4) :: MaxProductionProinter
      integer (kind=2) :: mpp
      integer (kind=2) :: gtgp_result, timeshere=0
      integer (kind=2) :: grx_resid_value
      logical (kind=1) :: tsty_result, ra_result, gwmp
      logical (kind=1) ::  ttsy_result
      integer (kind=2) :: tot_all_options_cap
      logical (kind=2) :: umrl

      integer :: allocstat
      
      
!
! END DATA DECLARATIONS
!


      ENTRY INIT_SCREENING_OBJECT
         if(file_trace_mxa==0) then
            file_trace_mxa=open_trace("mx_annual.trace", rq_mxa)
         end if
         if(file_trace_so==0) then
            file_trace_so=open_trace( &
                "init_screening_object.trace", rq_so)
         end if
         call write_trace_message(file_trace_so, &
            "calling fuel_price_files_active")
            
         CALL FUEL_PRICE_FILES_ACTIVE(FUEL_PRICE_DATA_AVAILABLE)
!
         LAST_SORTED_POSITION = 0
!
         UPPER_TRANS_GROUP = GET_NUMBER_OF_ACTIVE_GROUPS()
         IF(ALLOCATED(LAST_RESOURCE_ADDED)) then
                              DEALLOCATE(LAST_RESOURCE_ADDED, &
                                         LAST_PA_RESOURCE_ADDED, &
                                         LAST_CM_RESOURCE_ADDED, &
                                         FIRST_CT_EAS, &
                                         FIRST_CT_4_EAS_INDEX, &
                                         LOWEST_FIXED_COST_PA, &
                                         LOWEST_FIXED_COST_BY_PA, &
                                         LOWEST_FIXED_COST_BY_CM, &
                                         LOWEST_CONE_COST_BY_CM, &
                                         EAS_REVENUE_OFFSET_BY_CM, &
                                         MARGINAL_ICAP_PER_KWYR_BY_TG, &
                                       INSTALL_CAP_VALUE_LAST_PA_UNIT, &
                                         INSTALL_CAP_VALUE_LAST_CM_UNIT)
         end if
        call write_trace_int2(file_trace_so, "upper TG", &
            UPPER_TRANS_GROUP)
        ALLOCATE(LAST_RESOURCE_ADDED(MAX(INT(1,2),UPPER_TRANS_GROUP)), &
              LAST_PA_RESOURCE_ADDED(MAX(INT(1,2),UPPER_TRANS_GROUP)), &
              LAST_CM_RESOURCE_ADDED(MAX(INT(1,2),UPPER_TRANS_GROUP)), &
                 FIRST_CT_EAS(MAX(INT(1,2),UPPER_TRANS_GROUP)), &
                FIRST_CT_4_EAS_INDEX(MAX(INT(1,2),UPPER_TRANS_GROUP)), &
                LOWEST_FIXED_COST_PA(MAX(INT(1,2),UPPER_TRANS_GROUP)), &
             LOWEST_FIXED_COST_BY_PA(MAX(INT(1,2),UPPER_TRANS_GROUP)), &
             LOWEST_FIXED_COST_BY_CM(MAX(INT(1,2),UPPER_TRANS_GROUP)), &
              LOWEST_CONE_COST_BY_CM(MAX(INT(1,2),UPPER_TRANS_GROUP)), &
            EAS_REVENUE_OFFSET_BY_CM(MAX(INT(1,2),UPPER_TRANS_GROUP)), &
        MARGINAL_ICAP_PER_KWYR_BY_TG(MAX(INT(1,2),UPPER_TRANS_GROUP)), &
      INSTALL_CAP_VALUE_LAST_PA_UNIT(MAX(INT(1,2),UPPER_TRANS_GROUP)), &
        INSTALL_CAP_VALUE_LAST_CM_UNIT(MAX(INT(1,2),UPPER_TRANS_GROUP)))
         LAST_RESOURCE_ADDED = 0
         LAST_CM_RESOURCE_ADDED = 0
         FIRST_CT_EAS = 0.0
         FIRST_CT_4_EAS_INDEX = 0
         INSTALL_CAP_VALUE_LAST_PA_UNIT = 0.0
         INSTALL_CAP_VALUE_LAST_CM_UNIT = 0.0
         
         ! TODO: Make INITIALIZE_SCREEN_DATA a subroutine to make
         ! more obvious it's setting two values, not just one.
         tot_all_options_cap=INITIALIZE_SCREEN_DATA(MaxProductionProinter)
         call write_trace_int4(file_trace_so, "Max Prod Prtr.", &
            MaxProductionProinter)
         call write_trace_int2(file_trace_so, "total all options", &
            tot_all_options_cap)
            
         MAX_ID_RESOURCE_OPTIONS =  OPTIONS_MAX_RESOURCE_ID() ! INTEGER

       call write_trace_int2(file_trace_so, "MAX_ID_RESOURCE_OPTIONS", &
        MAX_ID_RESOURCE_OPTIONS)

         IF(ALLOCATED(OPTION_NAME)) then
            DEALLOCATE(OPTION_NAME,OPTION_POSITION, &
                                            ResourceIDtoOptionPosPrt)
         end if
         
                                            
         call write_trace_int2(file_trace_so, "TOTAL_ALL_OPTIONS", &
            tot_all_options_cap)
            
         IF(tot_all_options_cap > 0) THEN

                
            ALLOCATE(OPTION_NAME(tot_all_options_cap), stat=allocstat)
            call check_alloc("cap_objt:0071", "tot_all_options_cap", &
                allocstat)
            
            allocate (OPTION_POSITION(tot_all_options_cap), stat=allocstat)
            call check_alloc("cap_objt:0071", "tot_all_options_cap", &
                allocstat)
                
            OPTION_POSITION = -1 
            ! TODO: Can this be combined? (DRY)
            CAPTURED_OPTIONS_NAMES = GET_OPTION_NAMES(OPTION_NAME, &
                OPTION_POSITION,tot_all_options_cap)
            call write_trace_strings(file_trace_so, "OPTION_NAME", &
                OPTION_NAME(:120)) ! Arbitrary length
            if(file_trace_so/=BAD_TRACE_HANDLE) then
               write(file_trace_so, *) "CAPTURED_OPTIONS_NAMES", &
                CAPTURED_OPTIONS_NAMES
            end if
            
            
            ALLOCATE(ResourceIDtoOptionPosPrt(MaxProductionProinter))
         ENDIF

         ANNUAL_MARKET_OPTION_SWITCH = .FALSE.
         call write_trace_bool1(file_trace_so, "AMOS", &
            ANNUAL_MARKET_OPTION_SWITCH)

      RETURN


      ENTRY ANNUAL_SCREENING(R_YEAR)
         if(file_trace_as==0) then
            file_trace_as=open_trace("annual_screening.trace", &
                rq_ftas)
         end if
         call write_trace_int2(file_trace_as, "annual_screening for year",  &
                R_YEAR)
         call write_trace_int2(file_trace_as, "TOTAL_ALL_OPTIONS", &
            TOT_ALL_OPTIONS_cap)
         
         IF(tot_all_options_cap < 1) then
            call write_trace_message(file_trace_as, "2. RETURN")
            RETURN
         end if
         ALLOCATE(SCREEN_SLOPE(tot_all_options_cap), &
                  SCREEN_INTERCEPT(tot_all_options_cap))
         SCREEN_SLOPE = 9999.
         SCREEN_INTERCEPT = 9999.
!
         IF(SCREENING_NOT_OPEN) THEN
            SCREENING_NOT_OPEN = .FALSE.
            SCREENING_NO = SCREENING_HEADER(SCREENING_REC)
         ENDIF
!
         DO J = 1, MAX_SCREENING_INTERVALS
            BEST_SLOPE(J) = 9999.
            BEST_INTERCEPT(J) = 9999.
            BEST_VALUE(J) = 99999.
            BEST_RESOURCE(J) = 0
         ENDDO
         INTERVAL_MULTIPLIER = &
             1./FLOAT(MAX(MAX_SCREENING_INTERVALS,INT(2,2))-INT(1,2))
         THIS_YEAR = R_YEAR + get_BASE_YEAR()
!
         ignored_result = UPDATE_CL_SCREEN_COSTS(R_YEAR)
!
         ignored_result=UPDATE_SCREEN_CAP_COSTS(R_YEAR)

         TEMP_L = UPDATE_TRANS_FIXED_COSTS(R_YEAR)
         
         DO I = 1, tot_all_options_cap ! Issue in this loop - John
            IF( .NOT. RESOURCE_AVAILABLE(I,THIS_YEAR)) then
                call write_trace_int2(file_trace_as, "I", i)
                call write_trace_message(file_trace_as, "2. CYCLE")
                CYCLE
            end if
            IF( .NOT. &
                 CALC_SCREEN_SLOPE_INTERCEPT(I,R_YEAR, &
                                             R_SLOPE,R_INTERCEPT)) CYCLE
            SCREEN_SLOPE(I) = R_SLOPE
            SCREEN_INTERCEPT(I) = R_INTERCEPT
!
            DO J = 1, MAX_SCREENING_INTERVALS
!
               SCREEN_VALUE(J) = SCREEN_INTERCEPT(I) + &
                            FLOAT(J)*INTERVAL_MULTIPLIER*SCREEN_SLOPE(I)
!
               IF(SCREEN_VALUE(J) >= BEST_VALUE(J)) then
               call write_trace_int2(file_trace_as, "(J)", J)
               call write_trace_real4s(file_trace_as, "BEST_VALUE", &
                BEST_VALUE)
               call write_trace_message(file_trace_as, "3. CYCLE")
                CYCLE
               end if

               
               BEST_VALUE(J) = SCREEN_VALUE(J)
               BEST_SLOPE(J) = SCREEN_SLOPE(I)
               BEST_INTERCEPT(J) = SCREEN_INTERCEPT(I)
               BEST_RESOURCE(J) = I
               
               if(file_trace_as/=BAD_TRACE_HANDLE) then
                    write(file_trace_as,*) "I ", I, "J ", J, &
                    "BEST_VALUE ", BEST_VALUE(J),  &
                   "BEST_SLOPE ", BEST_SLOPE(J) , &
                   "BEST_INTERCEPT ", BEST_INTERCEPT(J), &
                   "BEST_RESOURCE ", BEST_RESOURCE(J)
               end if
            ENDDO ! SCREENING_INTERVALS
!
            if(file_trace_as/=BAD_TRACE_HANDLE) then
                WRITE(file_trace_as,*) &
                "SCREENING_REC ", SCREENING_REC, &
                "PRT_ENDPOINT ", PRT_ENDPOINT(), &
                "+YEAR+ ", FLOAT(globecom_year+get_BASE_YEAR()), &
                "OPTION_NAME ", OPTION_NAME(I), &
                "SCREEN_VALUE ", SCREEN_VALUE
            end if
            
            WRITE(SCREENING_NO,REC=SCREENING_REC) &
                     PRT_ENDPOINT(), &
                     FLOAT(globecom_year+get_BASE_YEAR()), &
                     OPTION_NAME(I), &
                     SCREEN_VALUE
            SCREENING_REC = SCREENING_REC + 1
!
         ENDDO ! OPTIONS
         call write_trace_message(file_trace_as, "after DO")
         if (file_trace_as/=BAD_TRACE_HANDLE) then
            write(file_trace_as,*) "SCREENING_REC ", SCREENING_REC, &
            "PRT_ENDPOINT ", PRT_ENDPOINT(), &
            "+YEAR+ ", FLOAT(globecom_year+get_BASE_YEAR()), &
            'Least Cost Resource ', &
             BEST_VALUE
         end if
         
         WRITE(SCREENING_NO,REC=SCREENING_REC) &
                     PRT_ENDPOINT(), &
                     FLOAT(globecom_year+get_BASE_YEAR()), &
                     'Least Cost Resource ', &
                     BEST_VALUE
         SCREENING_REC = SCREENING_REC + 1
! MAKE CALL TO SCREENING REPORT
         IF(ALLOCATED(SCREEN_SLOPE)) &
                               DEALLOCATE(SCREEN_SLOPE,SCREEN_INTERCEPT)
      RETURN ! ANNUAL_SCREENING


      ENTRY ANNUAL_MARKET_OPTION_COST(R_YEAR)

!
! THIS ROUTINE NEEDS TO BE ADDED AFTER PRICES HAVE BEEN CALCULATED
! AND BEFORE THE NEXT YEAR'S SIMULATION.
!
! THIS PROGRAM IS COPIED HEAVILY FROM ANNUAL_SCREENING ABOVE.
! THE PURPOSE OF THIS PROGRAM IS TO CACULATE THE ANNUAL COSTS OF EACH
! OPTION FOR CAPACITY FACTORS FROM 0% TO 100%.
!
! THE TOTAL COST AT EACH CAPACITY FACTOR IS TOTAL FIXED (INTERCEPT)
! PLUS THE SLOPE (FUEL + VOM) AT EACH CAPACITY FACTOR POINT.
!
! CALLS FOR THE FIXED, VARIABLE AND CAPACITY ARE MADE TO CLA_OBJT.FOR.
! THERE ARE A NUMBER OF WAYS THAT THE CODE NEEDS TO BE MADE MORE ROBUST:
!     - FUEL PRICES SHOULD BE CALLABLE FROM THE FUEL PRICE FILE
!     - COSTS OF THE RESOURCE SHOULD BE CALCULATED MONTHLY, ESP.
!       FOR GAS UNITS
!     - CAPACITIES NEED TO CHANGE MONTH/ANNUAL
!
! ALSO, TVA WANTS TO BE ABLE TO RETIRE AS WELL AS ADD RESOURCES.
!
!
!
! Both sides are called from the same place.
         amoccount=amoccount+1
         if(file_trace_amoc==0) then
            file_trace_amoc=open_trace( &
                "ANNUAL_MARKET_OPTION_COST.trace", rq_amoc)
            call write_trace_message(file_trace_amoc, &
                "First entry into the " // &
                "ANNUAL_MARKET_OPTION_COST routine.")

         end if
            call write_trace_int2(file_trace_amoc, "MX_RES_P_YEAR", &
                MX_RES_P_YEAR)
         
         cc_ANNUAL_MARKET_OPTION_COST=cc_ANNUAL_MARKET_OPTION_COST+1
         call write_trace_int(file_trace_amoc, &
            "ANNUAL_MARKET_OPTION_COST routine call", &
            cc_ANNUAL_MARKET_OPTION_COST)
        call write_trace_int2(file_trace_amoc, "4. year", R_YEAR)
            
          !see value in debugger by copying it to the amoc_debug_value variable. 
         amoc_debug_value=cc_ANNUAL_MARKET_OPTION_COST

         
         if(r_year==2) then
            r_year=r_year ! Debugstop
         end if
         MX_SUMMARY_REPORT = EXPANSION_REPORT()
         call write_trace_bool1(file_trace_amoc, "MX_SUMMARY_REPORT", &
            MX_SUMMARY_REPORT)
            
         
         GRX_8761_REPORT = GRX_8761_REPORT_ACTIVE()
         call write_trace_bool1(file_trace_amoc, "GRX_8761_REPORT", &
            GRX_8761_REPORT)
            
         
         RUN_PRICE_MODE = YES_STRICT_MARKET_PRICE()
         call write_trace_bool1(file_trace_amoc, "RUN_PRICE_MODE", &
            RUN_PRICE_MODE)
            
         
         RUN_AREA_PRICE_MODE = YES_MULTI_AREA_PRICE() ! ADDED 07/30/03.
         call write_trace_bool1(file_trace_amoc, "RUN_AREA_PRICE_MODE", &
            RUN_AREA_PRICE_MODE)
            
         
         RUN_MULTIAREA = YES_RUN_MULTIAREA_TRANSACT()
         call write_trace_bool1(file_trace_amoc, "RUN_MULTIAREA", &
            RUN_MULTIAREA)
            
         
         INCLUDE_ICAP_REVENUE = YES_INCLUDE_ICAP_REVENUE()
        call write_trace_bool1(file_trace_amoc, "INCLUDE_ICAP_REVENUE", &
            INCLUDE_ICAP_REVENUE)
            
         
         GRX_RPS_MODULE_ACTIVE = GRX_RPS_MODULE_IS_ACTIVE()
         call write_trace_bool1(file_trace_amoc, "GRX_RPS_MODULE_ACTIVE", &
            GRX_RPS_MODULE_ACTIVE)
            
         
         RPS_ONLY_SWITCH_ACTIVE = RPS_ONLY_SWITCH()
       call write_trace_bool1(file_trace_amoc, "RPS_ONLY_SWITCH_ACTIVE", &
        RPS_ONLY_SWITCH_ACTIVE)
                  
         RPS_PROGRAMS = NUMBER_OF_RPS_PROGRAMS()
        call write_trace_int2(file_trace_amoc, "NUMBER_OF_RPS_PROGRAMS", &
            NUMBER_OF_RPS_PROGRAMS())
            

         gwmp=GRX_WITH_MRX_PLANNING()
       call write_trace_bool1(file_trace_amoc, "GRX_WITH_MRX_PLANNING", &
        gwmp)
        
  
         IF(.NOT. gwmp .AND. .FALSE.) THEN
            TEMP_L = ADJUST_CO2_RETRO_PLAN_CAP(R_YEAR)
            call write_trace_bool1(file_trace_amoc, &
            "TEMP_L after calling ADJUST_CO2_RETRO_PLAN_CAP", &
                TEMP_L)
            call write_trace_int2(file_trace_amoc, "... for R_YEAR ",  &
                R_YEAR)
                
         ENDIF
!
         CO2_RETIREMENTS_LOGIC = GET_CO2_RETIREMENTS_LOGIC()

       call write_trace_bool1(file_trace_amoc, "CO2_RETIREMENTS_LOGIC", &
        CO2_RETIREMENTS_LOGIC)
        
            if(r_year==0) then
               call end_program("cap_objt:0051 - year must be nonzero.")
            end if

         PEAK =  UPDATE_NET_PLANNING_PEAK(R_YEAR)


         
         call write_trace_real(file_trace_amoc, "PEAK from " // &
            "UPDATE_NET_PLANNING_PEAK", PEAK)
            
         

         call write_trace_int2(file_trace_amoc, "1. R_YEAR", R_YEAR)
         ! First call to get_el_tg_cap (right)
         call write_trace_real(file_trace_amoc, &
            "1. EL_PLANNING_CAPACITY(3,R_YEAR)", &
            EL_PLANNING_CAPACITY(3,R_YEAR))
         
         call write_trace_real(file_trace_amoc, &
           "CL_PLANNING_CAPACITY(3,R_YEAR)", &
           CL_PLANNING_CAPACITY(3,R_YEAR))
         call write_trace_real(file_trace_amoc, &
          "CT_PLANNING_CAPACITY(3,R_YEAR)", &
          CT_PLANNING_CAPACITY(3,R_YEAR))
         call write_trace_real(file_trace_amoc, &
         "LM_PLANNING_CAPACITY(R_YEAR)", &
         LM_PLANNING_CAPACITY(R_YEAR))


         
         call write_trace_real(file_trace_amoc, &
         "ADJUSTMENT_CAPACITY(R_YEAR)", &
         ADJUSTMENT_CAPACITY(R_YEAR))
         
         call write_trace_real(file_trace_amoc, &
         "DERIV_CAPACITY_PLANNING(R_YEAR)", &
         DERIV_CAPACITY_PLANNING(R_YEAR))


         ANN_CAP =  EL_PLANNING_CAPACITY(3,R_YEAR) + &
                    CL_PLANNING_CAPACITY(3,R_YEAR) + &
                    CT_PLANNING_CAPACITY(3,R_YEAR) + &
                    LM_PLANNING_CAPACITY(R_YEAR)   + &
                    ADJUSTMENT_CAPACITY(R_YEAR)    + &
                    DERIV_CAPACITY_PLANNING(R_YEAR)

         
         
         call write_trace_real(file_trace_amoc, "1. ANN_CAP", ANN_CAP)
         call write_trace_string(file_trace_amoc, &
            "CAPACITY_PLANNING_METHOD", CAPACITY_PLANNING_METHOD())
            
         call write_trace_string(file_trace_amoc, &
            "GREEN_MRX_METHOD", GREEN_MRX_METHOD())

          

         IF(CAPACITY_PLANNING_METHOD() == 'MX'  .AND. &
                                        GREEN_MRX_METHOD() == 'GX') THEN

            call write_trace_int4(file_trace_amoc, "GRX_ITERATIONS", &
                GRX_ITERATIONS)
                
            IF(GRX_ITERATIONS == 0) THEN

               VOID_INT2 = UPDATE_CL_SCREEN_COSTS(R_YEAR)

               call write_trace_message(file_trace_amoc, &
                "Calling UPDATE_SCREEN_CAP_COSTS.")
                
             ignored_result = UPDATE_SCREEN_CAP_COSTS(R_YEAR)
             
            call write_trace_int2(file_trace_amoc, &
            "Calling UPDATE_TRANS_FIXED_COSTS for year ", R_YEAR)
            
               TEMP_L = UPDATE_TRANS_FIXED_COSTS(R_YEAR)
               
            ENDIF
         ELSE
         
            call write_trace_int2(file_trace_amoc, "Calling " // &
            "UPDATE_CL_SCREEN_COSTS for year", R_YEAR)
            
            VOID_INT2 = UPDATE_CL_SCREEN_COSTS(R_YEAR)
            
            call write_trace_int2(file_trace_amoc,  &
                "1. Calling UPDATE_TRANS_FIXED_COSTS for year", r_year)
                
            ignored_result = UPDATE_SCREEN_CAP_COSTS(R_YEAR)
            
            call write_trace_int2(file_trace_amoc, &
                "2. Calling UPDATE_TRANS_FIXED_COSTS for year", r_year)
            TEMP_L = UPDATE_TRANS_FIXED_COSTS(R_YEAR)
         ENDIF

         
         call  write_trace_int2(file_trace_amoc, "TOTAL_ALL_OPTIONS", &
            tot_all_options_cap)
         call write_trace_bool1(file_trace_amoc, "RUN_MULTIAREA", &
            RUN_MULTIAREA)
         call write_trace_bool1(file_trace_amoc, "RUN_PRICE_MODE", &
            RUN_PRICE_MODE)
         call write_trace_bool1(file_trace_amoc, "RUN_AREA_PRICE_MODE", &
            RUN_AREA_PRICE_MODE)
            
         IF(tot_all_options_cap < 1 .OR. &
                      .NOT. (RUN_MULTIAREA .OR. RUN_PRICE_MODE .OR. &
                                           RUN_AREA_PRICE_MODE) ) then
                                           
             call write_trace_message(file_trace_amoc, &
                "1. RETURN from ANNUAL_MARKET_OPTION_COST")
             RETURN
         end if
         
         HIGHEST_INTERVAL = MAX_SCREENING_INTERVALS
       call write_trace_int2(file_trace_amoc, "MAX_SCREENING_INTERVALS", &
        MAX_SCREENING_INTERVALS)
        

         IF(ALLOCATED(SCREEN_SLOPE)) &
            DEALLOCATE( &
                  SCREEN_SLOPE, &
                  SCREEN_INTERCEPT, &
                  SCREEN_CAPACITY, &
                  ANNUAL_ENERGY_REVENUE, &
                  EQUIVALENT_CAPACITY, &
                  BLOCK_CAPACITY, &
                  BLOCK_HEAT_RATE, &
                  ANNUAL_FIXED_COST, &
                  STRIKE_PRICE, &
                  ns_cap_trans%transaction_group, &
                  STATE_PROVINCE, &
                  LOCAL_ANNUAL_UNITS, &
                  RFT, &
                  ACTIVE_OPTION, &
                  FIRST_CN, &
                  ACTIVE_INSTANCE, &
                  MIN_UP_TIME, &
                  MIN_DOWN_TIME, &
                  RPS_PROGRAM_NUMBER_loc, &
                  START_UP_COSTS, &
                  MUST_RUN_UNIT, &
                  RPM, &
                  HIGHEST_RFT, &
                  HIGHEST_RPM, &
                  FILE_SOURCE_INDEX, &
                  LOCAL_LEAD_TIME, &
                  ANNUAL_LEVELIZED_CAPITAL, &
                  OP_LIFE, &
                  ICAP_SWITCH_INDEX, &
                  ICAP_CONE_PRICE, &
                  HIGHEST_ANN_LEVEL_CAP, &
                  ANNUAL_FUEL_COST, &
                  HIGHEST_FUEL_COST, &
                  FUEL_COST_PER_MWH, &
                  MONTHLY_CAP_MULT, &
                  AVERAGE_FUEL_PRICE, &
                  VAR_OM_PER_MWH, &
                  FUEL_COST_PER_MMBTU, &
                  REVENUE_PER_MWH, &
                  CAPACITY_REVENUE_PER_MWH, &
                  CAPACITY_REVENUE_MM, &
                  RPS_REVENUE_MM, &
                  MRX_RPS_MX_UNIT_CLEARED, &
                  RPS_REVENUE_PER_MWH, &
                  ENERGY_REVENUE_MM, &
                  ENERGY_REVENUE_PER_MWH, &
                  HIGHEST_ENERGY_REV_PER_MWH, &
                  HIGHEST_STRIKE_PRICE, &
                  HIGHEST_MRX_MARGIN, &
                  HIGHEST_MRX_CF, &
                  HIGHEST_MARKET_COST, &
                  HIGHEST_MRX_ENERGY, &
                  EMISSIONS_PER_MWH, &
                  FOM_PER_MWH, &
                  LEVELIZED_CAP_PER_MWH, &
                  FIXED_COST_PER_MWH, &
                  MRX_ICAP_INDEX, &
                  LOCAL_CHRONO_MARKET_PRICES, &
                  RESET_CHRONO_MARKET_PRICES, &
                  VOM_COST_MM, &
                  EMISSIONS_COST_MM, &
                  FOM_COST_MM, &
                  IOTA_BY_CN, &
                  TRANS_GROUP_ID, &
                  TG_CAPACITY_MW, &
                  TG_PEAK_MW, &
                  FirmTransfer, &
                  CM_Net_Transfer, &
                  TG_Net_Transfer, &
                  PA_Net_Transfer, &
                  NUMBER_DIRECT_INTERCONNECTS, &
                  DIRECT_INTERCONNECT_TG, &
                  HIGHEST_TG_ICAP_REV_PER_KWYR, &
                  HIGHEST_OPTION_FOR_TG, &
                  TransmissionConstraint, &
                  MaxTransmissionConstraint, &
                  TransferValue, &
                  TG_RESERVE_MARGIN, &
                  PLANNING_AREA_ID, &
                  PA_CAPACITY_MW, &
                  CM_CAPACITY_MW, &
                  PA_PEAK_MW, &
                  CM_PEAK_MW, &
                  PA_RESERVE_MARGIN, &
                  CM_RESERVE_MARGIN, &
                  CM_MAX_NET_ENRG_MARG_PER_KWYR, &
                  ICAP_DEMAND_CURVE_MULT, &
                  HIGHEST_CF, &
                  CONE_COST_PER_KWYR, &
                  CONE_DEMAND_CURVE_ADJ_PER_KWYR, &
                  EAS_PROFIT_PER_KWYR, &
                  NET_MARGIN_PER_KWYR, &
                  ICAP_REVENUE_PER_KWYR, &
                  HEAT_RATE, &
                  HIGHEST_HEAT_RATE, &
                  HIGHEST_ANNUAL_UNITS, &
                  HIGHEST_MARKET_REVENUE, &
                  HIGHEST_MARGIN_PER_KWYR, &
                  HIGHEST_FUEL_COST_PER_MWH, &
                  HIGHEST_VAR_OM_PER_MWH, &
                  HIGHEST_FUEL_COST_PER_MMBTU, &
                  HIGHEST_EMISSIONS_PER_MWH, &
                  HIGHEST_FOM_PER_MWH, &
                  HIGHEST_LEVEL_CAP_PER_MWH, &
                  HIGHEST_VOM_COST_MM, &
                  HIGHEST_EMISSIONS_COST_MM, &
                  HIGHEST_FOM_COST_MM, &
                  FIX_OM_COST, &
                  CAPACITY_PLANNING_MW, &
                  RPS_PERCENT, &
                  FIRST_CF, &
                  ANNUAL_VARIABLE_COST, &
                  VARIABLE_COST_MM, &
                  FIXED_COST_MM, &
                  HIGHEST_SCREEN_CAPACITY, &
                  MARKET_REVENUE)
         if(allocated(highest_energy)) then
            deallocate(HIGHEST_ENERGY)
         end if
         if(allocated(market_cost)) then
            deallocate(market_cost)
         end if
         if (allocated(net_margin)) then
            deallocate(net_margin)
         end if
         if (allocated(NET_MARGIN_INTERVAL)) then
            deallocate(net_margin_interval)
         end  if
        if(allocated(THIS_YEAR_PLUS_LEAD_TIME)) then
            deallocate(THIS_YEAR_PLUS_LEAD_TIME)
        endif
        if(allocated(MARKET_CUM_REVENUE)) then
            deallocate(MARKET_CUM_REVENUE)
        endif
        if(allocated(MARKET_DURATION)) then
            deallocate(market_duration)
        endif
        if(allocated(MARKET_PRICE)) then
            deallocate(market_price)
        endif

         ALLOCATE(SCREEN_SLOPE(tot_all_options_cap), &
                  SCREEN_INTERCEPT(tot_all_options_cap), &
                  SCREEN_CAPACITY(tot_all_options_cap), &
                  ANNUAL_ENERGY_REVENUE(tot_all_options_cap), &
                  EQUIVALENT_CAPACITY(tot_all_options_cap), &
                  BLOCK_CAPACITY(5,tot_all_options_cap), &
                  BLOCK_HEAT_RATE(5,tot_all_options_cap), &
                  ANNUAL_FIXED_COST(tot_all_options_cap), &
                  STRIKE_PRICE(tot_all_options_cap,13), &
                  ns_cap_trans%transaction_group(tot_all_options_cap), &
                  STATE_PROVINCE(tot_all_options_cap), &
                  LOCAL_ANNUAL_UNITS(tot_all_options_cap), &
                  RFT(tot_all_options_cap), &
                  ACTIVE_OPTION(tot_all_options_cap), &
                  FIRST_CN(tot_all_options_cap), &
                  ACTIVE_INSTANCE(MX_RES_P_YEAR), &
                  MIN_UP_TIME(tot_all_options_cap), &
                  MIN_DOWN_TIME(tot_all_options_cap), &
                  RPS_PROGRAM_NUMBER_loc(tot_all_options_cap), &
                  START_UP_COSTS(12,tot_all_options_cap), &
                  MUST_RUN_UNIT(12,tot_all_options_cap), &
                  RPM(tot_all_options_cap), &
                  LOCAL_LEAD_TIME(tot_all_options_cap), &
                  FILE_SOURCE_INDEX(tot_all_options_cap), &
                  ANNUAL_LEVELIZED_CAPITAL(tot_all_options_cap), &
                  OP_LIFE(tot_all_options_cap), &
                  ICAP_SWITCH_INDEX(0:UPPER_TRANS_GROUP), &
                  ICAP_CONE_PRICE(0:UPPER_TRANS_GROUP), &
                  HIGHEST_ANN_LEVEL_CAP(MX_RES_P_YEAR), &
                  ANNUAL_FUEL_COST(tot_all_options_cap), &
                  HIGHEST_FUEL_COST(MX_RES_P_YEAR), &
                  FUEL_COST_PER_MWH(12,tot_all_options_cap), &
                  MONTHLY_CAP_MULT(12,tot_all_options_cap), &
                  AVERAGE_FUEL_PRICE(tot_all_options_cap), &
                  VAR_OM_PER_MWH(tot_all_options_cap), &
                  FUEL_COST_PER_MMBTU(tot_all_options_cap), &
                  REVENUE_PER_MWH(MX_RES_P_YEAR), &
                  CAPACITY_REVENUE_PER_MWH(MX_RES_P_YEAR), &
                  CAPACITY_REVENUE_MM(MX_RES_P_YEAR), &
                  RPS_REVENUE_MM(MX_RES_P_YEAR), &
                  MRX_RPS_MX_UNIT_CLEARED(MX_RES_P_YEAR), &
                  RPS_REVENUE_PER_MWH(MX_RES_P_YEAR), &
                  ENERGY_REVENUE_MM(MX_RES_P_YEAR), &
                  ENERGY_REVENUE_PER_MWH(tot_all_options_cap), &
                  HIGHEST_ENERGY_REV_PER_MWH(MX_RES_P_YEAR), &
                  HIGHEST_STRIKE_PRICE(MX_RES_P_YEAR), &
                  HIGHEST_MRX_MARGIN(MX_RES_P_YEAR), &
                  HIGHEST_MRX_CF(MX_RES_P_YEAR), &
                  HIGHEST_MARKET_COST(MX_RES_P_YEAR), &
                  HIGHEST_MRX_ENERGY(MX_RES_P_YEAR), &
                  HIGHEST_RFT(MX_RES_P_YEAR), &
                  HIGHEST_RPM(MX_RES_P_YEAR), &
                  EMISSIONS_PER_MWH(tot_all_options_cap), &
                  FOM_PER_MWH(tot_all_options_cap), &
                  LEVELIZED_CAP_PER_MWH(tot_all_options_cap), &
                  FIXED_COST_PER_MWH(MX_RES_P_YEAR), &
                  MRX_ICAP_INDEX(MX_RES_P_YEAR), &
                  LOCAL_CHRONO_MARKET_PRICES(8784,UPPER_TRANS_GROUP), &
                  RESET_CHRONO_MARKET_PRICES(8784,UPPER_TRANS_GROUP), &
                  VOM_COST_MM(tot_all_options_cap), &
                  EMISSIONS_COST_MM(tot_all_options_cap), &
                  FOM_COST_MM(tot_all_options_cap), &
                  IOTA_BY_CN(tot_all_options_cap), &
                  TRANS_GROUP_ID(tot_all_options_cap), &
                  TG_CAPACITY_MW(0:UPPER_TRANS_GROUP), &
                  TG_PEAK_MW(0:UPPER_TRANS_GROUP), &
                FirmTransfer(0:UPPER_TRANS_GROUP,0:UPPER_TRANS_GROUP), &
                  CM_Net_Transfer(0:UPPER_TRANS_GROUP), &
                  TG_Net_Transfer(0:UPPER_TRANS_GROUP), &
                  PA_Net_Transfer(0:UPPER_TRANS_GROUP), &
                  NUMBER_DIRECT_INTERCONNECTS(0:UPPER_TRANS_GROUP), &
                  DIRECT_INTERCONNECT_TG(0:UPPER_TRANS_GROUP, &
                                               0:UPPER_TRANS_GROUP), &
                  HIGHEST_TG_ICAP_REV_PER_KWYR(0:UPPER_TRANS_GROUP), &
                  HIGHEST_OPTION_FOR_TG(0:UPPER_TRANS_GROUP), &
                  TransmissionConstraint(0:UPPER_TRANS_GROUP, &
                                                 0:UPPER_TRANS_GROUP), &
                  MaxTransmissionConstraint(0:UPPER_TRANS_GROUP, &
                                                 0:UPPER_TRANS_GROUP), &
                  TransferValue(0:UPPER_TRANS_GROUP, &
                                                 0:UPPER_TRANS_GROUP), &
                  TG_RESERVE_MARGIN(0:UPPER_TRANS_GROUP), &
                  PLANNING_AREA_ID(tot_all_options_cap), &
                  PA_CAPACITY_MW(0:UPPER_TRANS_GROUP), &
                  CM_CAPACITY_MW(0:UPPER_TRANS_GROUP), &
                  PA_PEAK_MW(0:UPPER_TRANS_GROUP), &
                  CM_PEAK_MW(0:UPPER_TRANS_GROUP), &
                  PA_RESERVE_MARGIN(0:UPPER_TRANS_GROUP), &
                  CM_RESERVE_MARGIN(0:UPPER_TRANS_GROUP), &
                  CM_MAX_NET_ENRG_MARG_PER_KWYR(0:UPPER_TRANS_GROUP), &
                  ICAP_DEMAND_CURVE_MULT(MX_RES_P_YEAR), &
                  HIGHEST_CF(tot_all_options_cap), &
                  CONE_COST_PER_KWYR(MX_RES_P_YEAR), &
                  CONE_DEMAND_CURVE_ADJ_PER_KWYR( &
                                                MX_RES_P_YEAR), &
                  EAS_PROFIT_PER_KWYR(MX_RES_P_YEAR), &
                  NET_MARGIN_PER_KWYR(tot_all_options_cap), &
                  ICAP_REVENUE_PER_KWYR(MX_RES_P_YEAR), &
                  HEAT_RATE(tot_all_options_cap), &
                  HIGHEST_HEAT_RATE(MX_RES_P_YEAR), &
                  HIGHEST_ANNUAL_UNITS(MX_RES_P_YEAR), &
                  HIGHEST_MARKET_REVENUE(MX_RES_P_YEAR), &
                  HIGHEST_MARGIN_PER_KWYR(MX_RES_P_YEAR), &
                  HIGHEST_FUEL_COST_PER_MWH(MX_RES_P_YEAR), &
                  HIGHEST_VAR_OM_PER_MWH(MX_RES_P_YEAR), &
                  HIGHEST_FUEL_COST_PER_MMBTU(MX_RES_P_YEAR), &
                  HIGHEST_EMISSIONS_PER_MWH(MX_RES_P_YEAR), &
                  HIGHEST_FOM_PER_MWH(MX_RES_P_YEAR), &
                  HIGHEST_LEVEL_CAP_PER_MWH(MX_RES_P_YEAR), &
                  HIGHEST_VOM_COST_MM(MX_RES_P_YEAR), &
                  HIGHEST_EMISSIONS_COST_MM(MX_RES_P_YEAR), &
                  HIGHEST_FOM_COST_MM(MX_RES_P_YEAR), &
                  FIX_OM_COST(tot_all_options_cap), &
                  CAPACITY_PLANNING_MW(tot_all_options_cap), &
                  RPS_PERCENT(tot_all_options_cap), &
                  FIRST_CF(tot_all_options_cap), &
                  ANNUAL_VARIABLE_COST(tot_all_options_cap), &
                  VARIABLE_COST_MM(MX_RES_P_YEAR), &
                  FIXED_COST_MM(MX_RES_P_YEAR), &
                  HIGHEST_SCREEN_CAPACITY(MX_RES_P_YEAR), &
                  MARKET_REVENUE(tot_all_options_cap))
                  ANNUAL_ENERGY_REVENUE=0. ! Added initialization
                  allocate(HIGHEST_ENERGY(-1:tot_all_options_cap), &
                    stat=allocstat)
                  call check_alloc("cap_objt:0066", "tot_all_options_cap", &
                  allocstat, "None")
                  HIGHEST_ENERGY=0 ! Moved from cap_objt
                  
                  allocate(MARKET_COST(tot_all_options_cap), &
                  NET_MARGIN(tot_all_options_cap), &
                  NET_MARGIN_INTERVAL(tot_all_options_cap), &
                  THIS_YEAR_PLUS_LEAD_TIME(tot_all_options_cap), &
                  MARKET_CUM_REVENUE(0:HIGHEST_INTERVAL), &
                  MARKET_DURATION(0:HIGHEST_INTERVAL), &
                  MARKET_PRICE(0:HIGHEST_INTERVAL))

         TG_Net_Transfer = 0.0 ! 091111 MOVED UP.
         SCREEN_SLOPE = 9999.
         SCREEN_INTERCEPT = 9999.
         SCREEN_CAPACITY = 0.
         EQUIVALENT_CAPACITY = 0.
         BLOCK_CAPACITY = 0.
         BLOCK_HEAT_RATE = 0.
         ANNUAL_FIXED_COST = 0.
         ANNUAL_VARIABLE_COST = 0.
         VARIABLE_COST_MM = 0.
         FIXED_COST_MM = 0.
         HIGHEST_SCREEN_CAPACITY = 0.
         STRIKE_PRICE = 9999.
         ns_cap_trans%transaction_group = 0
         STATE_PROVINCE = 0
         LOCAL_ANNUAL_UNITS = 9999
         RFT = 0.0
         START_UP_COSTS = 0.0
         MUST_RUN_UNIT = .FALSE.
         MIN_UP_TIME = 0
         MIN_DOWN_TIME = 0
         RPS_PROGRAM_NUMBER_loc = 0
         ACTIVE_OPTION = 0
         FIRST_CN = 0
         ACTIVE_INSTANCE = 0
         RPM = 0.0
         HIGHEST_RFT = 0.0
         HIGHEST_RPM = 0.0
         LOCAL_LEAD_TIME = 0
         FILE_SOURCE_INDEX = 0
         ANNUAL_LEVELIZED_CAPITAL = 0.
         OP_LIFE = 99
         ICAP_SWITCH_INDEX = 2
         ICAP_CONE_PRICE = 0.0
         HIGHEST_ANN_LEVEL_CAP = 0.
         ANNUAL_FUEL_COST = 0.
         HIGHEST_FUEL_COST = 0.
         FUEL_COST_PER_MWH = 0.
         MONTHLY_CAP_MULT = 0.
         AVERAGE_FUEL_PRICE = 0.
! 110809. TEST
         LAST_PA_RESOURCE_ADDED = 0
         LAST_CM_RESOURCE_ADDED = 0
         FIRST_CT_EAS = 0.0
         FIRST_CT_4_EAS_INDEX = 0
         LOWEST_FIXED_COST_PA = 0
         LOWEST_FIXED_COST_BY_PA = 999999.0
         LOWEST_FIXED_COST_BY_CM = 999999.0
         LOWEST_CONE_COST_BY_CM = 999999.0
         EAS_REVENUE_OFFSET_BY_CM = 0.0
         MARGINAL_ICAP_PER_KWYR_BY_TG = 999999.0
!
         VAR_OM_PER_MWH = 0.
         FUEL_COST_PER_MMBTU = 0.
         REVENUE_PER_MWH = 0.
         CAPACITY_REVENUE_PER_MWH = 0.
         CAPACITY_REVENUE_MM = 0.
         RPS_REVENUE_MM = 0.0
         RPS_REVENUE_PER_MWH = 0.0
         ENERGY_REVENUE_MM = 0.
         ENERGY_REVENUE_PER_MWH = 0.
         HIGHEST_ENERGY_REV_PER_MWH = 0.
         HIGHEST_STRIKE_PRICE = 0.
         HIGHEST_MRX_MARGIN = 0.
         HIGHEST_MRX_CF = 0.
         HIGHEST_MARKET_COST = 0.
         HIGHEST_FUEL_COST_PER_MWH = 0.
         HIGHEST_MRX_ENERGY = 0.
         EMISSIONS_PER_MWH = 0.
         FOM_PER_MWH = 0.
         LEVELIZED_CAP_PER_MWH = 0.
         FIXED_COST_PER_MWH = 0.
         MRX_ICAP_INDEX = 0.
         VOM_COST_MM = 0.
         EMISSIONS_COST_MM = 0.
         FOM_COST_MM = 0.
         TRANS_GROUP_ID = 0.
         TG_CAPACITY_MW = 0.
         TG_PEAK_MW = 0.
         TG_RESERVE_MARGIN = 0.
         PLANNING_AREA_ID = 0.
         PA_CAPACITY_MW = 0.
         CM_CAPACITY_MW = 0.
         PA_PEAK_MW = 0.
         CM_PEAK_MW = 0.
         PA_RESERVE_MARGIN = 0.
         CM_RESERVE_MARGIN = 0.
         CM_MAX_NET_ENRG_MARG_PER_KWYR = -999999.0
         ICAP_DEMAND_CURVE_MULT = 0.
         HIGHEST_CF = 0.
         CONE_COST_PER_KWYR = 0.
         CONE_DEMAND_CURVE_ADJ_PER_KWYR = 0.
         EAS_PROFIT_PER_KWYR = 0.
         ICAP_REVENUE_PER_KWYR = 0.
         HEAT_RATE = 0.
         HIGHEST_HEAT_RATE = 0.
         HIGHEST_ANNUAL_UNITS = 0.
         HIGHEST_MARKET_REVENUE = 0.
         HIGHEST_MARGIN_PER_KWYR = 0.
         HIGHEST_VAR_OM_PER_MWH = 0.
         HIGHEST_FUEL_COST_PER_MMBTU = 0.
         HIGHEST_EMISSIONS_PER_MWH = 0.
         HIGHEST_FOM_PER_MWH = 0.
         HIGHEST_LEVEL_CAP_PER_MWH = 0.
         HIGHEST_VOM_COST_MM = 0.
         HIGHEST_EMISSIONS_COST_MM = 0.
         HIGHEST_FOM_COST_MM = 0.
         FIX_OM_COST = 0.
         CAPACITY_PLANNING_MW = 0.0
         RPS_PERCENT = 1.0
         THIS_YEAR_PLUS_LEAD_TIME = 0

         CO2_RETIREMENT_PRICE = -9999.

         
!
         PG = MAX(GET_NUMBER_OF_PLANNING_GROUPS(),INT(1,2))
         CG = MAX(GET_NUMBER_OF_CAPACITY_MARKETS(),INT(1,2))
         call write_trace_int2(file_trace_amoc, "PG", PG)
         call write_trace_int2(file_trace_amoc, "CG", CG)
         
         if(file_trace_amoc/=BAD_TRACE_HANDLE) then
             write(file_trace_amoc,*) "get_number_of_(pg/cm):"
             write(file_trace_amoc,*) "PG became ", PG
             write(file_trace_amoc,*) "CG became ", CG
             ! mergfile_trace_amocllet ) 
             ! this revision not getting here for CN==3. why?
             write(file_trace_amoc,*) "2. R_YEAR=", R_YEAR
         end if
     
         SCENARIO_CAP_COST_MULT = &
                           GET_SCENARIO_EXPAND_CAP_COST(R_YEAR,INT(1,2))
         SCENARIO_RESERVE_MARGIN = &
                            GET_SCENARIO_RESERVE_MARGIN(R_YEAR,INT(1,2))
!

         
         MINIMUM_NET_MARGIN = GET_ADDITIONS_PROFIT_PER_MWH()
         MAXIMUM_RM = MAX_CAPACITY_TESTING_MARGIN(R_YEAR)
         VOID_LOGICAL = YES_USE_MINIMUM_RM(USE_MINIMUM_RM)

         PLANNING_PER_MWH = .TRUE.
!
         USE_MINIMUM_RM_LOGIC = .FALSE.
         USE_REGIONAL_MINIMUM_RM_LOGIC = .FALSE.
         USE_REGIONAL_MAXIMUM_RM_LOGIC = .FALSE.
         MRX_SORTED_MIX =  USE_MINIMUM_RM == 'X'
         IF(USE_MINIMUM_RM == 'U' .OR. USE_MINIMUM_RM == 'B') THEN
            USE_MINIMUM_RM_LOGIC = .TRUE.
         ELSEIF(USE_MINIMUM_RM == 'R' .OR. USE_MINIMUM_RM == 'X') THEN
            USE_REGIONAL_MINIMUM_RM_LOGIC = .TRUE.
         ENDIF
!
         IF(USE_MINIMUM_RM == 'A' .OR. &
                      USE_MINIMUM_RM == 'B' .OR. &
                                 USE_MINIMUM_RM == 'R' .OR. &
                                             USE_MINIMUM_RM == 'X') THEN
            USE_MAXIMUM_RM_LOGIC = .TRUE.
         ELSE
            USE_MAXIMUM_RM_LOGIC = .FALSE.
         ENDIF
!
         PLANNING_PER_MWH = YES_PLAN_TO_MWH_OR_KWYR()
         REGIONAL_PARAMS_EXIST = YES_REGIONAL_PARAMS_EXIST()
         YES_USE_EMIS_IN_MRX = USE_EMIS_IN_MRX()
         THIS_YEAR = R_YEAR + get_BASE_YEAR()
!
! FIRST, GET FIXED AND VARIABLE COST BY OPTION BY INTERVAL
!
         call write_trace_bool1(file_trace_amoc, "ANNUAL_MARKET_OPTION_SWITCH", &
            ANNUAL_MARKET_OPTION_SWITCH )
         call write_trace_int2(file_trace_amoc, "3. R_YEAR", R_YEAR)
         
         
         
         
         IF(.NOT. ANNUAL_MARKET_OPTION_SWITCH &
           .or. .not. YES_RUN_TRANSACT()) THEN
             if(yes_run_transact()) then
               ! Should go true first time.
               ANNUAL_MARKET_OPTION_SWITCH = .TRUE.
             end if
             
             
            RETURN
         ENDIF
!
! INITIALIZE RESOURCE EXPANSION OPTIONS IN THIS YEAR.
!
         TEMP_L = CALC_MRX_RPS_CURVES(R_YEAR)
         ANNUAL_RESOURCES_AVAILABLE = .FALSE.
!
         N = 0
         AO = 0
!
         ResourceIDtoOptionPosPrt = 0
         IF(THIS_YEAR >= 2018) then
           i= tot_all_options_cap
         endif
         


         call write_trace_int2(file_trace_amoc, "TOTAL_ALL_OPTIONS", &
            tot_all_options_cap)
            
         DO CN = 1, tot_all_options_cap 
         ! INDEX FROM THE CAPACITY OPTIONS FILE
         call write_trace_int2(file_trace_amoc, &
            "OPTION_POSITION(CN)", OPTION_POSITION(CN))
            
            IF(OPTION_POSITION(CN) == -1) THEN
              

               tot_all_options_cap = MAX(1,CN-1)
         call write_trace_int2(file_trace_amoc, "total_all_options+-", &
            tot_all_options_cap)
            
               
               call write_trace_message(file_trace_amoc, "miscount")

               WRITE(4,*) 'MISCOUNT IN CAPACITY OPTIONS FILE'
               
               EXIT
            ENDIF
            if(cn==4) then
                tot_all_options_cap=tot_all_options_cap ! Debugstop
            end if
!
            I = OPTION_POSITION(CN)

            call write_trace_int2(file_trace_amoc, "3. I", I)
            call write_trace_int2(file_trace_amoc, "6. CN", CN)
            FILE_SOURCE_INDEX(CN) = GET_FILE_SOURCE_INDEX(I)
            call write_trace_int2(file_trace_amoc, "file_source_index", &
                 file_source_index(cn))       
 
            THIS_YEAR_PLUS_LEAD_TIME(CN) = &
                                     THIS_YEAR + LEAD_TIME_FOR_OPTION(I)
!



        if (file_trace_amoc/=BAD_TRACE_HANDLE) then
            write(file_trace_amoc,*) "I=", i
            write(file_trace_amoc,*) " 1. CN=", CN
            write(file_trace_amoc,*) "RESOURCE_AVAILABLE elements=",&
            resource_available(i, this_year_plus_lead_time(cn))
            
            write(file_trace_amoc,*) "TEST_START_STOP_YEARS elements=",&
                test_start_stop_years(I, &
                this_year_plus_lead_time(cn))
        end if
        
            call write_trace_int2(file_trace_amoc, "1. I", I)
            call write_trace_int2(file_trace_amoc, "1. CN", CN)

            ra_result=resource_available(i, &
                this_year_plus_lead_time(cn))
            call write_trace_bool1(file_trace_amoc, "ra_result", &
                ra_result)
                

            ttsy_result=test_start_stop_years(I, &
                this_year_plus_lead_time(cn))

            call write_trace_bool1(file_trace_amoc, "ttsy_result", &
        ttsy_result)

            if(.not. ra_result .or. &
                .not. ttsy_result) then
                call write_trace_message(file_trace_amoc, "14. CYCLE")

               CYCLE
            end if
            
            SCREEN_CAPACITY(CN) = GET_SCREEN_CAPACITY(I)
            call write_trace_int2(file_trace_amoc, "CN", CN)
            call write_trace_real4(file_trace_amoc, "1. SC", &
                SCREEN_CAPACITY(CN))
            
            

            
            OP_LIFE(CN) = RETURN_CN_OP_LIFE(CN,R_YEAR)

        if (file_trace_amoc/=BAD_TRACE_HANDLE) then
            write(file_trace_amoc,*) "1. FILE_SOURCE_INDEX(CN)=", &
                FILE_SOURCE_INDEX(CN), "2. CN=",CN
        end if
                
                
                
            IF(FILE_SOURCE_INDEX(CN) == CapLimited) THEN
              
               

      call write_trace_int2(file_trace_amoc, "I", I)
      call write_trace_message(file_trace_amoc, &
        "getting production DATA pointer")

               co_prod_pointer=GET_PRODUCTION_DATA_POINTER(I)
               
      call write_trace_int2(file_trace_amoc, "PRODUCTION_DATA_POINTER", &
        co_prod_pointer)
        
               ResourceIDtoOptionPosPrt(co_prod_pointer) = CN
               
               ns_cap_trans%transaction_group(CN) = &
                   RETURN_S_TRANS_GROUP_ID(co_prod_pointer)

               STATE_PROVINCE(CN) = GET_SCREEN_STATE_INDEX(co_prod_pointer)


               
               gtgp_result=GET_TRANS_GROUP_POSITION( &
                   ns_cap_trans%transaction_group(CN))

               

               IF(gtgp_result <=0) then
               
                call write_trace_message(file_trace_amoc, "Cycling on "// &
       "GET_TRANS_GROUP_POSITION")
                call write_trace_int2(file_trace_amoc, "CN", CN)
                call write_trace_int2(file_trace_amoc, "TRANSACTION_GROUP", &
                    ns_cap_trans%transaction_group(CN))
                call write_trace_message(file_trace_amoc, "cycle.")
                
                
                CYCLE
                
               else
                gtgp_result=gtgp_result ! Debugstop
               end if

               AO = AO + 1
               ACTIVE_OPTION(AO) = CN
               call write_trace_int2(file_trace_amoc, "CN", CN)
               call write_trace_int2(file_trace_amoc, "AO", AO)
        call write_trace_int2(file_trace_amoc, "1. ACTIVE_OPTION(AO)", &
            ACTIVE_OPTION(AO))
               

               N = CN


!              Prod_pointer=737/737
               FT = GET_S_PRIMARY_MOVER(co_prod_pointer)
               !FT=/4
               FUEL_SCEN_MULT(5) = 1.0
!
               IF(FT > 4 .OR. FT < 1) then
                FT = 5
               end if
!
               RFT(CN) = &
                  FLOAT(MAX(INT(1,2), &
                      MIN(GET_S_PRIMARY_MOVER(co_prod_pointer),INT(11,2))))
               RPM(CN) = FLOAT(GET_S_PRIMARY_MOVER_INDEX(co_prod_pointer))
!
               R_DELIVERY_COST = -999999.
!
               VOID_R4 = PUT_MRX_DELIVERY_COST(R_DELIVERY_COST)
!
               MIN_UP_TIME(CN) = &
                                  INT(GET_S_MIN_UP_TIME(co_prod_pointer),2)
               MIN_DOWN_TIME(CN) = &
                                INT(GET_S_MIN_DOWN_TIME(co_prod_pointer),2)
               RPS_PERCENT(CN) = &
                              GET_S_RPS_PERCENT(co_prod_pointer,R_YEAR)
                call check_cap_prod_pointer(co_prod_pointer, &
                    "cap_objt:0025", .false.)


               RPS_PROGRAM_NUMBER_loc(CN) = &
                                  GET_S_RPS_PROGRAM_NUMBER(co_prod_pointer)
               call check_cap_prod_pointer(co_prod_pointer, "cap_objt:0027", &
               .false.)
               
!
! 12/17/04. MAKE FUEL AND VOM VARIABLES WITH A SWITCH.
!
               DO MO= 1, 12

                  START_UP_COSTS(MO,CN) = &
                         GET_UNIT_START_UP_COSTS(R_YEAR,co_prod_pointer,MO)
                  call check_cap_prod_pointer(co_prod_pointer, &
                  "cap_objt:0024", .false.)

                  MUST_RUN_UNIT(MO,CN) = &
                                 S_YES_MONTHLY_MUST_RUN(co_prod_pointer,MO)
                  IF(FUEL_PRICE_DATA_AVAILABLE) THEN
                     FUEL_SCEN_MULT(1) = &
                                  GET_SCENARIO_COAL_PRICES(R_YEAR,MO)
                     FUEL_SCEN_MULT(2) = &
                                  GET_SCENARIO_GAS_PRICES(R_YEAR,MO)
                     FUEL_SCEN_MULT(3) = &
                                  GET_SCENARIO_OIL_PRICES(R_YEAR,MO)
                     FUEL_SCEN_MULT(4) = &
                              GET_SCENARIO_URANIUM_PRICES(R_YEAR,MO)
!
                     RTEMP = GET_MONTHLY_FUEL_INDEX(N,MO)
!
! ASSUME NO FUEL INVENTORIES.
! THE TWO CRITICAL ROUTINES.  THEY ARE HOOKED-UP TO EXISTING UNITS.
! THE INDICES IN THESE TWO ROUTINES ARE FOR EXISTING UNITS.
!
                     FUEL_POINTERS_USED = .FALSE.
                     EXISTING_UNIT = .FALSE.

                     
            call write_trace_message(file_trace_amoc, &
            "calling GET_MRX_DELIVERY_COST")
            
               if (file_trace_amoc/=BAD_TRACE_HANDLE) then
                write(file_trace_amoc,*) &
                  "prod_pointer ", co_prod_pointer, &
                  "R_DELIVERY_COST ", R_DELIVERY_COST, &
                  "MO ", MO," R_YEAR ", R_YEAR, &
                  "FUEL_POINTERS_USED ", FUEL_POINTERS_USED, &
                  "EXISTING_UNIT", EXISTING_UNIT
               end if
                     CALL GET_MRX_DELIVERY_COST(co_prod_pointer, &
                                          R_DELIVERY_COST, &
                                          MO,R_YEAR, &
                                          FUEL_POINTERS_USED, &
                                          EXISTING_UNIT)
               call write_trace_message(file_trace_amoc, "<< back")

               if (file_trace_amoc/=BAD_TRACE_HANDLE) then
                write(file_trace_amoc,*) &
                  "prod_pointer ", co_prod_pointer, &
                  "R_DELIVERY_COST ", R_DELIVERY_COST, &
                  "MO ", MO," R_YEAR ", R_YEAR, &
                  "FUEL_POINTERS_USED ", FUEL_POINTERS_USED, &
                  "EXISTING_UNIT", EXISTING_UNIT
               end if
               
                     IF(FUEL_POINTERS_USED) THEN
                        VOID_R4 = PUT_MRX_DELIVERY_COST(R_DELIVERY_COST)
                     ENDIF
                  ENDIF
!
                  FUEL_COST_PER_MWH(MO,CN) = &
                       RETURN_CL_SCREEN_FUEL_COST(co_prod_pointer,R_YEAR, &
                                              HEAT_RATE(CN), &
                                            FUEL_COST_PER_MMBTU(CN)) * &
                                              FUEL_SCEN_MULT(FT) * RTEMP

!
! 03/17/05. ADDED FOR PAC AND BURESH
!
         call write_trace_bool1(file_trace_amoc, "YES_USE_EMIS_IN_MRX", &
            YES_USE_EMIS_IN_MRX)
            
                  IF(YES_USE_EMIS_IN_MRX) THEN
                  call write_trace_message(file_trace_amoc, &
                    "calling CALCULATE_NOX_SOX_VOM with...")
                    if(file_trace_amoc/=BAD_TRACE_HANDLE) then
                        write(file_trace_amoc, *) &
                        "CN", CN, &
          "prod_pointer ", co_prod_pointer,  "4. R_YEAR ", R_YEAR, &
          "MO ", MO, "NOX_VOM ", NOX_VOM, "NOX_FOM ", NOX_FOM, &
          "SOX_VOM ", SOX_VOM,  "SOX_FOM ", SOX_FOM, &
          "CO2_VOM ", CO2_VOM, "CO2_FOM ", CO2_FOM, &
          "HG_VOM ", HG_VOM, "HG_FOM ", HG_FOM, &
          "OTHER3_VOM ", OTHER3_VOM, "OTHER3_FOM ", OTHER3_FOM, &
          "NOX_CONTROL_MULT ", NOX_CONTROL_MULT, &
          "SOX_CONTROL_MULT ", SOX_CONTROL_MULT, &
          "CO2_CONTROL_MULT ", CO2_CONTROL_MULT,  "HG_CONTROL_MULT ", &
          HG_CONTROL_MULT, &
          "OTHER3_CONTROL_MULT ", OTHER3_CONTROL_MULT, &
          "transaction_group ", ns_cap_trans%transaction_group(CN)
                    end if
                    
                     CALL CALCULATE_NOX_SOX_VOM( &
                              co_prod_pointer, &
                              R_YEAR, &
                              MO, &
                              NOX_VOM, &
                              NOX_FOM, &
                              SOX_VOM, &
                              SOX_FOM, &
                              CO2_VOM, &
                              CO2_FOM, &
                              HG_VOM, &
                              HG_FOM, &
                              OTHER3_VOM, &
                              OTHER3_FOM, &
                              NOX_CONTROL_MULT, &
                              SOX_CONTROL_MULT, &
                              CO2_CONTROL_MULT, &
                              HG_CONTROL_MULT, &
                              OTHER3_CONTROL_MULT, &
                              ns_cap_trans%transaction_group(CN))

       call write_trace_message(file_trace_amoc, "1. <<back")
                    if(file_trace_amoc/=BAD_TRACE_HANDLE) then
                        write(file_trace_amoc, *) &
                        "CN", CN, &
          "prod_pointer ", co_prod_pointer,  "5. R_YEAR ", R_YEAR, &
          "MO ", MO, "NOX_VOM ", NOX_VOM, "NOX_FOM ", NOX_FOM, &
          "SOX_VOM ", SOX_VOM,  "SOX_FOM ", SOX_FOM, &
          "CO2_VOM ", CO2_VOM, "CO2_FOM ", CO2_FOM, &
          "HG_VOM ", HG_VOM, "HG_FOM ", HG_FOM, &
          "OTHER3_VOM ", OTHER3_VOM, "OTHER3_FOM ", OTHER3_FOM, &
          "NOX_CONTROL_MULT ", NOX_CONTROL_MULT, &
          "SOX_CONTROL_MULT ", SOX_CONTROL_MULT, &
          "CO2_CONTROL_MULT ", CO2_CONTROL_MULT,  "HG_CONTROL_MULT ", &
          HG_CONTROL_MULT, &
          "OTHER3_CONTROL_MULT ", OTHER3_CONTROL_MULT, &
          "transaction_group ", ns_cap_trans%transaction_group(CN)
                    end if

            call write_trace_message(file_trace_amoc, &
                "calling RETURN_CL_SCREEN_EMIS_COST")
                
                     EMISSIONS_PER_MWH(CN) = &
                            RETURN_CL_SCREEN_EMIS_COST(co_prod_pointer, &
                                                  R_YEAR,MO, &
                                                  NOX_CONTROL_MULT, &
                                                  SOX_CONTROL_MULT, &
                                                  CO2_CONTROL_MULT, &
                                                  HG_CONTROL_MULT, &
                                                  OTHER3_CONTROL_MULT, &
                                               CO2_RETIREMENT_PRICE) + &
                                           NOX_VOM + SOX_VOM + &
                                           CO2_VOM + HG_VOM + OTHER3_VOM
       call write_trace_message(file_trace_amoc, " 2. <<back")
       
       if (file_trace_amoc/=BAD_TRACE_HANDLE) then
            write(file_trace_amoc,*) &
                "EMISSIONS_PER_MWH ", EMISSIONS_PER_MWH(CN),  &
                "prod_pointer ", co_prod_pointer, "6. R_YEAR ", R_YEAR, &
                "MO ", MO, "NOX_CONTROL_MULT ", NOX_CONTROL_MULT, &
                 "SOX_CONTROL_MULT ", SOX_CONTROL_MULT, &
                 "CO2_CONTROL_MULT ", CO2_CONTROL_MULT, &
                 "HG_CONTROL_MULT ", HG_CONTROL_MULT, &
                 "OTHER3_CONTROL_MULT ", OTHER3_CONTROL_MULT, &
              "CO2_RETIREMENT_PRICE ", CO2_RETIREMENT_PRICE, &
             "NOX_VOM ", NOX_VOM, "SOX_VOM ", SOX_VOM,  &
            "CO2_VOM ", CO2_VOM, "HG_VOM ", HG_VOM, &
            "OTHER3_VOM ", OTHER3_VOM
       end if
       

                  ENDIF

               ENDDO ! MO

               MO = 7 ! FOR NOW.
               
           if(file_trace_amoc/=BAD_TRACE_HANDLE) then
               write(file_trace_amoc,*) "prod_pointer=", &
                co_prod_pointer
           end if
                
               VAR_OM_PER_MWH(CN) = &
                                RETURN_OTHER_VAR_COMPONENT(co_prod_pointer)
               call write_trace_real4(file_trace_amoc, "1. VAR_OM_PER_MWH(CN) became ", &
               VAR_OM_PER_MWH(CN))
               call write_trace_int2(file_trace_amoc, "1. (CN)", CN)
               
                                
          if(file_trace_amoc/=BAD_TRACE_HANDLE) then
          write(file_trace_amoc,*) "3. CN=", CN, "VAR_OM_PER_MWH became", &
                VAR_OM_PER_MWH(CN)
          end if
               

               EQUIVALENT_CAPACITY(CN) = SCREEN_CAPACITY(CN) * &
                    (1. - RETURN_CL_SCREEN_EFOR(co_prod_pointer,R_YEAR, &
                                             MONTHLY_CAP_MULT(1,CN)))
                                             
           if(file_trace_amoc/=BAD_TRACE_HANDLE) then
               write(file_trace_amoc,*) "Calling GET_S_BLOCK_CAP_AND_HR " // &
               "with PROD_POINTER=", co_prod_pointer, "4. CN=",CN, &
               "BLOCK_CAPACITY(1,CN)=", BLOCK_CAPACITY(1,CN), &
               "BLOCK_HEAT_RATE(1,CN)=", BLOCK_HEAT_RATE(1,CN), &
               "7.R_YEAR=", R_YEAR
           end if
               
               
               
               TEMP_L = GET_S_BLOCK_CAP_AND_HR(co_prod_pointer, &
                                               BLOCK_CAPACITY(1,CN), &
                                               BLOCK_HEAT_RATE(1,CN), &
                                               R_YEAR)
                                               
               call write_trace_bool1(file_trace_amoc, &
       "GET_S_BLOCK_CAP_AND_HR returned TEMP_L", TEMP_L)
                
               
               FIX_OM_COST(CN) = &
                               RETURN_CL_SCREEN_FIXED_COST(co_prod_pointer)
                               
        call write_trace_real4(file_trace_amoc, "FIX_OM_COST(CN)=", &
           FIX_OM_COST(CN))
        call write_trace_int2(file_trace_amoc, "CN", CN)
        call write_trace_int2(file_trace_amoc, "PROD_POINTER", &
       co_PROD_POINTER)

            
                
               CAPACITY_PLANNING_MW(CN) = SCREEN_CAPACITY(CN) * &
                             GET_S_CAP_PLANNING_FAC(co_prod_pointer,R_YEAR)
                             
            if(file_trace_amoc/=BAD_TRACE_HANDLE) then
               write(file_trace_amoc,*) "CAPACITY_PLANNING_MW(CN) set to ", &
               CAPACITY_PLANNING_MW(CN), " (CN=", CN, ")"
            end if
               
            ELSEIF(FILE_SOURCE_INDEX(CN) == Derivative) THEN
                grx_resid_value=GET_GRX_RESOURCE_LINK_ID(i)
                co_prod_pointer=grx_resid_value
                
            if(file_trace_amoc/=BAD_TRACE_HANDLE) then
                write(file_trace_amoc,*) "2. FILE_SOURCE_INDEX(CN)=", &
                    FILE_SOURCE_INDEX(CN), " (Derivative) ", &
                    "5. CN=", CN
            end if
                    



               IF(co_prod_pointer < 1) THEN
                  WRITE(4,*) 'BAD GRX RESOURCE ID LINK IN OPTIONS FILE'
                  WRITE(4,*) 'FOR ',OPTION_NAME(I)
                  CYCLE
               ENDIF
               
               co_prod_pointer=GET_TRANS_FOR_GRX_ID(co_prod_pointer)
               
               ! prod_pointer=0/0

               EMISSIONS_PER_MWH(CN) =  0.0 ! COULD ADD EMISSION COST
               IF(co_prod_pointer < 1) THEN
                  ! Error write looking at now - John
                  WRITE(4,*) &
                          'BAD GRX RESOURCE ID LINK IN DERIVATIVE FILE'
                  WRITE(4,*) 'FOR ',OPTION_NAME(I)
                  WRITE(4,*) 'RESOURCE ID LINK ', &
                                             GET_GRX_RESOURCE_LINK_ID(I)
                  WRITE(4,*) 'TOTAL ALL OPTIONS ',tot_all_options_cap
                  WRITE(4,*) 'CURRENT COUNT OF OPTIONS',CN
                  WRITE(4,*) 'CURRENT POSITION ',I
                  CYCLE
               ENDIF

               ns_cap_trans%transaction_group(CN) = &
                                 GET_TRANS_GROUP_FOR_TRANS(co_prod_pointer)
               STATE_PROVINCE(CN) = GET_TRANS_STATE_INDEX(co_prod_pointer)
               IF(ns_cap_trans%transaction_group(CN) < 1) THEN
                  WRITE(4,*) &
                          'BAD TRANSACTION GROUP IN DERIVATIVE FILE'
                  WRITE(4,*) 'FOR ',OPTION_NAME(I)
                 call write_trace_message(file_trace_amoc, "1. Cycling")
                  CYCLE
               ENDIF
               IF(GET_TRANS_GROUP_POSITION( &
                ns_cap_trans%transaction_group(CN)) <= 0) then
                call write_trace_message(file_trace_amoc, "2. Cycling")
                   CYCLE
               end if
               
               AO = AO + 1
               call write_trace_int2(file_trace_amoc, "AO++", AO)
               
               
               ACTIVE_OPTION(AO) = CN
               call write_trace_int2(file_trace_amoc, "CN", CN)
               call write_trace_int2(file_trace_amoc, "AO", AO)
        call write_trace_int2(file_trace_amoc, "2. ACTIVE_OPTION(AO)", &
            ACTIVE_OPTION(AO))

               N = CN
               EQUIVALENT_CAPACITY(CN) = SCREEN_CAPACITY(CN)
! VARIABLE COST, FIXED COST, RFT, RPM, EMISSIONS COST IF SET, TG,
! HEAT RATE
         call write_trace_real4(file_trace_amoc, "1. AE", ANNUAL_ENERGY)
               FIX_OM_COST(CN) = &
                           CALC_GRX_FIX_COST_PER_UNIT( &
                                       co_prod_pointer, &
                                       R_YEAR, &
                                       1_2, &
                                       EQUIVALENT_CAPACITY(CN), &
                                       ANNUAL_ENERGY)

!
               RFT(CN) = &
                 FLOAT(MAX(INT(1,2), &
                     MIN(GET_DERIV_PRIM_MOVER(co_prod_pointer),INT(11,2))))
               RPM(CN) = FLOAT(GET_DERIV_PRIM_MOVER_INDEX(co_prod_pointer))
                             

               
               
               CAPACITY_PLANNING_MW(CN) = SCREEN_CAPACITY(CN) * &
                             GET_TRANS_CAP_PLAN_FAC(co_prod_pointer,R_YEAR)
               RPS_PERCENT(CN) = &
                              GET_TRANS_RPS_PERCENT(co_prod_pointer,R_YEAR)
               RPS_PROGRAM_NUMBER_loc(CN) = &
                                 GET_TRANS_RPS_PROG_NUMBER(co_prod_pointer)
            ELSEIF(FILE_SOURCE_INDEX(CN) == Transmission) THEN
               co_prod_pointer=GET_GRX_RESOURCE_LINK_ID(I)

               if(co_prod_pointer<=1) then
                  WRITE(4,*) 'BAD GRX RESOURCE ID LINK IN OPTIONS FILE'
                  WRITE(4,*) 'FOR ',OPTION_NAME(I)
                  call write_trace_message(file_trace_amoc, "3. Cycling")
                  CYCLE
               end if

               co_prod_pointer=GET_TRANSMISSION_FOR_GRX_ID(co_prod_pointer)
               call write_trace_int2(file_trace_amoc, "PROD_POINTER", &
                co_prod_pointer)
                


               EMISSIONS_PER_MWH(CN) =  0.0 ! COULD ADD EMISSION COST
               IF(co_prod_pointer < 1) THEN
                  call write_trace_message(file_trace_amoc, &
              "1. BAD GRX RESOURCE ID LINK IN TRANSMISSION PATHS FILE")
                  WRITE(4,*) &
                   'BAD GRX RESOURCE ID LINK IN TRANSMISSION PATHS FILE'
                  WRITE(4,*) 'FOR ',OPTION_NAME(I)
                  WRITE(4,*) 'RESOURCE ID LINK ', &
                                             GET_GRX_RESOURCE_LINK_ID(I)
                  WRITE(4,*) 'TOTAL ALL OPTIONS ',tot_all_options_cap
                  WRITE(4,*) 'CURRENT COUNT OF OPTIONS',CN
                  WRITE(4,*) 'CURRENT POSITION ',I
                  call write_trace_message(file_trace_amoc, "4. Cycling")
                  CYCLE
               ENDIF
               call write_trace_message(file_trace_amoc, &
                ">GET_TG_FOR_TRANSMISSION_BUY")
               ns_cap_trans%transaction_group(CN) = &
                               GET_TG_FOR_TRANSMISSION_BUY(co_prod_pointer)
               call write_trace_int2s(file_trace_amoc, "trans group", &
                ns_cap_trans%transaction_group)
                
               STATE_PROVINCE(CN) = 0 ! FOR NOW
               IF(ns_cap_trans%transaction_group(CN) < 1) THEN
                  WRITE(4,*) &
                          'BAD TRANSACTION GROUP IN DERIVATIVE FILE'
                  WRITE(4,*) 'FOR ',OPTION_NAME(I)
                call write_trace_message(file_trace_amoc, "5. Cycling")
                  CYCLE
               ENDIF

               IF(GET_TRANS_GROUP_POSITION( &
               ns_cap_trans%transaction_group(CN)) <=  0) then
                    call write_trace_message(file_trace_amoc, "6. Cycling")
                    CYCLE
               end if
               AO = AO + 1
               ACTIVE_OPTION(AO) = CN
               call write_trace_int2(file_trace_amoc, "CN", CN)
               call write_trace_int2(file_trace_amoc, "AO", AO)
        call write_trace_int2(file_trace_amoc, "3. ACTIVE_OPTION(AO)", &
            ACTIVE_OPTION(AO))
               N = CN
               EQUIVALENT_CAPACITY(CN) = SCREEN_CAPACITY(CN)

! VARIABLE COST, FIXED COST, RFT, RPM, EMISSIONS COST IF SET, TG,
! HEAT RATE

               FIX_OM_COST(CN) = 0 ! FOR NOW
               RFT(CN) = 0.0
               RPM(CN) = 0.0
               CAPACITY_PLANNING_MW(CN) = SCREEN_CAPACITY(CN) ! FOR NOW
               RPS_PERCENT(CN) = 0 ! FOR NOW

            ENDIF ! THERMAL, DERIVATIVE OR TRANSMISSION
            call write_trace_message(file_trace_amoc, &
                "calling RETURN_RISK_ADJ_S_CAP_COST")
            call write_trace_int2(file_trace_amoc, "I", I)
            call write_trace_int2(file_trace_amoc, "8. R_YEAR", R_YEAR)
            
            ANNUAL_LEVELIZED_CAPITAL(CN) = &
                              RETURN_RISK_ADJ_S_CAP_COST(I,R_YEAR) * &
                                                  SCENARIO_CAP_COST_MULT
            if(file_trace_amoc/=BAD_TRACE_HANDLE) then
                write(file_trace_amoc,*) "<BACK ", &
                "I ", I, "R_YEAR ", R_YEAR, &
                "SCENARIO_CAP_COST_MULT ", SCENARIO_CAP_COST_MULT, &
                "CN ", CN, &
             "ANNUAL_LEVELIZED_CAPITAL ", ANNUAL_LEVELIZED_CAPITAL(CN)
            end if
            
            IF(SCREEN_CAPACITY(CN) <= 0.) THEN
               WRITE(4,*) "EXPANSION CAPACITY NEGATIVE IN MRX"
               WRITE(4,*) "FOR RESOURCE",OPTION_NAME(CN)
               WRITE(4,*) "OPTION CAPACITY RESET TO 100 MW"
               SCREEN_CAPACITY(CN) = 100.
            call write_trace_int2(file_trace_amoc, "CN", CN)
            call write_trace_real4(file_trace_amoc, "2. SC", &
                SCREEN_CAPACITY(CN))

            ENDIF
!
            FOM_COST_MM(CN) = FIX_OM_COST(CN)* 0.000001

            ANNUAL_FIXED_COST(CN) = &
                        1000000.*ANNUAL_LEVELIZED_CAPITAL(CN) + &
                                                         FIX_OM_COST(CN)
            call write_trace_real4(file_trace_amoc, "annl. fixed. $$", &
                ANNUAL_FIXED_COST(CN))
            call write_trace_int2(file_trace_amoc, "(CN)", CN)
            
!
! 041110 MOVED UP. CHANGED TO CM. NEED TO USE THIS FOR CAPACITY PAYMENT
!
            IF(SCREEN_CAPACITY(CN) > 0.01) THEN
               TG = GET_TRANS_GROUP_POSITION(ns_cap_trans%transaction_group(CN))
      call write_trace_int2(file_trace_amoc, "1. TG", TG)
               CM = GET_CM_FROM_TG(TG)
               TEMP_R4 = ANNUAL_FIXED_COST(CN)* 0.001/ &
                                            SCREEN_CAPACITY(CN)
!
               IF(TEMP_R4 < LOWEST_FIXED_COST_BY_CM(CM) .AND. &
                                                   OP_LIFE(CN) > 0) THEN
                  LOWEST_FIXED_COST_BY_CM(CM) = MAX(0.0,TEMP_R4)
               ENDIF
            ENDIF
!
            NET_MARGIN(CN) = -999999.
            call write_trace_int2(file_trace_amoc, "CN", CN)
        call write_trace_real4(file_trace_amoc, "7. NM", NET_MARGIN(CN))
            call write_trace_int2(file_trace_amoc, "HO", &
                HIGHEST_OPTION)

        
            NET_MARGIN_PER_KWYR(CN) = -999999.
            NET_MARGIN_INTERVAL(CN) = -1
        
!
! 03/05/04. BIG DEFINITIONAL CHANGE FOR STRIKE_PRICE
!
            DO MO = 1, 12
               AVERAGE_FUEL_PRICE(CN) = &
                     AVERAGE_FUEL_PRICE(CN) + &
                        FUEL_COST_PER_MWH(MO,CN)
            END DO
            AVERAGE_FUEL_PRICE(CN) = AVERAGE_FUEL_PRICE(CN) / 12.0
!
! 080112. CONSISTENT DEFINITION OF FUEL PRICE.
!
            IF(HEAT_RATE(CN) > 0.001) THEN
             FUEL_COST_PER_MMBTU(CN) = AVERAGE_FUEL_PRICE(CN) * 1000./ &
                                                   HEAT_RATE(CN)
            ENDIF
            ! What's special about July?
            MO = 7
            call write_trace_real4(file_trace_amoc, "SP", &
                STRIKE_PRICE(CN, MO))
                
            STRIKE_PRICE(CN,MO) = AVERAGE_FUEL_PRICE(CN) + &
                                       EMISSIONS_PER_MWH(CN) + &
                                                      VAR_OM_PER_MWH(CN)
!
            LOCAL_ANNUAL_UNITS(CN) = ANNUAL_UNITS_FOR_OPTION(I)
            call write_trace_int2(file_trace_amoc, "CN", CN)
            call write_trace_int2(file_trace_amoc, "LAN(CN)", &
                LOCAL_ANNUAL_UNITS(CN))
                
            
            LOCAL_LEAD_TIME(CN) = LEAD_TIME_FOR_OPTION(I)
            IF(LOCAL_ANNUAL_UNITS(CN) < 0) THEN 
            ! ASSUME IT IS A POINTER PER PACIFICORP.
               LOCAL_ANNUAL_UNITS(CN) = &
                     INT(GET_VAR(FLOAT(LOCAL_ANNUAL_UNITS(CN)),R_YEAR, &
                                                     OPTION_NAME(CN)),2)
            ENDIF
            LOCAL_ANNUAL_UNITS(CN) = &
                            MIN(CUMULATIVE_UNITS_FOR_OPTION(I), &
                                                 LOCAL_ANNUAL_UNITS(CN))
            IF(LOCAL_ANNUAL_UNITS(CN) > 0) THEN
               ANNUAL_RESOURCES_AVAILABLE = .TRUE.
            ENDIF
!
         ENDDO ! OPTIONS

         TOTAL_ACTIVE_OPTIONS = AO
!
! NEXT, ASSUME ALL RESOURCES ARE ANNUAL CALL OPTIONS WHERE THE VARIABLE
! COST IS THE STRIKE PRICE AND
! UTILIZE THE CALL OPTIONS WITH THE HIGHEST NET MARGIN.

         call write_trace_int2(file_trace_amoc, "N", N)
         IF(N == 0) then ! N shouldn't be zero first time through - John

             call write_trace_message(file_trace_amoc, "1. RETURN")
             RETURN
         end if

! 011920. COMBINE HYBRID: FIXED AND CAPITAL COSTS
!
         call write_trace_int2(file_trace_amoc, "16A. TG", TG)


         call write_trace_int2s(file_trace_amoc, "transaction_groups", &
            ns_cap_trans%transaction_group(1:tot_all_options_cap))
            
         DO CN = 1, tot_all_options_cap

            IF(INDEPENDENT(CN) == 0) then
                call write_trace_message(file_trace_amoc, "7. cycling")
                CYCLE
            end if
            
            HYBRID_PROD_POINTER = INDEPENDENT(CN)
            FIX_OM_COST(CN) = FIX_OM_COST(CN) + &
                                        FIX_OM_COST(HYBRID_PROD_POINTER)
            FOM_COST_MM(CN) = FOM_COST_MM(CN) + &
                                        FOM_COST_MM(HYBRID_PROD_POINTER)
            ANNUAL_FIXED_COST(CN) = ANNUAL_FIXED_COST(CN) + &
                                  ANNUAL_FIXED_COST(HYBRID_PROD_POINTER)
            ANNUAL_LEVELIZED_CAPITAL(CN) = &
                     ANNUAL_LEVELIZED_CAPITAL(CN) + &
                           ANNUAL_LEVELIZED_CAPITAL(HYBRID_PROD_POINTER)
         ENDDO
!
         UPPER_TRANS_GROUP = GET_NUMBER_OF_ACTIVE_GROUPS()
         
         DO TG = 1, UPPER_TRANS_GROUP 
            call write_trace_int2(file_trace_amoc, "13. TG", TG)
         ! INDEX FROM THE CAPACITY OPTIONS FILE
             IF(MRX_ICAP_UNIT_LINK(TG) .NE. 0) THEN
               IF(MRX_ICAP_UNIT_LINK(TG) < 0) THEN
                  ConeLink = GET_ANNUAL_VECTOR_VALUE( &
                                          MRX_ICAP_UNIT_LINK(TG),R_YEAR)
               ELSE
                  ConeLink = MRX_ICAP_UNIT_LINK(TG)
               ENDIF

               ConeLink = ResourceIDtoOptionPosPrt(ConeLink)

               IF(ConeLink == 0) then
                   call write_trace_message(file_trace_amoc, "8. cycling")
                   CYCLE
               end if

               IF( .NOT. RESOURCE_AVAILABLE(ConeLink, &
                              THIS_YEAR_PLUS_LEAD_TIME(ConeLink)) .OR. &
                  .NOT. TEST_START_STOP_YEARS(ConeLink, &
                              THIS_YEAR_PLUS_LEAD_TIME(ConeLink))) then
               call write_trace_message(file_trace_amoc, "9. cycling")
                   CYCLE
               end if
               gtgp_result=GET_TRANS_GROUP_POSITION( &
                ns_cap_trans%transaction_group(ConeLink))
              IF(gtgp_result <= 0) then
                  
                  Call write_trace_int2(file_trace_amoc, &
                  "Cycling at year", globecom_year)

                  CYCLE
              end if

               ConeLinkCapacity = SCREEN_CAPACITY(ConeLink)
!
               IF(ConeLinkCapacity > 0.01) THEN
                  CM = GET_CM_FROM_TG(TG)
                  TEMP_R4 = ANNUAL_FIXED_COST(ConeLink)* 0.001/ &
                                            SCREEN_CAPACITY(ConeLink)
                  IF(TEMP_R4 < LOWEST_CONE_COST_BY_CM(CM) .AND. &
                            RETURN_CN_OP_LIFE(ConeLink,R_YEAR) > 0) THEN
                     LOWEST_CONE_COST_BY_CM(CM) = MAX(0.0,TEMP_R4)
                   ENDIF
                ELSE
                  WRITE(4,*) 'IN MRX, THE RESOURCE LINK IS NOT FOUND'
                  WRITE(4,*) 'LINK = ',MRX_ICAP_UNIT_LINK(TG)
                  er_message='Stop requested from CAP_OBJT SIID4'
                  call end_program(er_message)
               ENDIF
            ENDIF
         ENDDO

         CURRENT_TARGET_RATIO = TARGET_RATIO(3,R_YEAR) * &
                                                 SCENARIO_RESERVE_MARGIN
         
         ! Check multi_area_name allocation
         IF(ALLOCATED(MULTI_AREA_NAME)) then
            DEALLOCATE(MULTI_AREA_NAME, &
                                           PA_PLANNING_RESERVE_MARGIN, &
                                           CM_PLANNING_RESERVE_MARGIN, &
                                        PA_PEAK_AFTER_INTERRUPTIBLE, &
                                        PEAK_AFTER_INTERRUPTIBLE_TG, &
                                        TG_PLANNING_RESERVE_MARGIN, &
                                        CM_PEAK_AFTER_INTERRUPTIBLE, &
                                        PA_PLANNING_CAPACITY, &
                                        CM_PLANNING_CAPACITY, &
                                           CM_IOTA, &
                                           TG_PLANNING_CAPACITY, &
                                           TG_2_PLANNING_AREA, &
                                           TG_2_CAPACITY_MARKET, &
                                           CURRENT_TARGET_RATIO_PA, &
                                           MAXIMUM_RM_PA, &
                                          TOTAL_ANNUAL_CAPACITY_ADDED, &
                                           CM_TOT_ANN_CAPACITY_ADDED, &
                                       TG_TOTAL_ANNUAL_CAPACITY_ADDED, &
                                           MINIMUM_ANNUAL_MRX_CAP, &
                                           MINIMUM_TG_MRX_CAP, &
                                           MAXIMUM_TG_MRX_CAP, &
                                           CO2_EMISSIONS_CAP, &
                                         CO2_EMISS_REDUCTION_REQUIRED, &
                                           CO2_EMISSIONS, &
                                           MAXIMUM_ANNUAL_MRX_CAP, &
                                           PA_ICAP_REVENUE_MULT, &
                                           CM_ICAP_REVENUE_MULT)
         end if

         ! This was previously allocated using upper_trans_group, which
         ! is always insufficient (legacy too). 

        
         allocate(CM_PLANNING_RESERVE_MARGIN(0:upper_trans_group), stat=allocstat)
         call check_alloc("cap_objt:0068", "CM_PLANNING_RESERVE_MARGIN", &
            allocstat)
         
         ! In program_state object...
         cm_planning_reserve_margin_allocated=.true.

         cm_planning_reserve_margin=0
         ALLOCATE(MULTI_AREA_NAME(0:UPPER_TRANS_GROUP), &
                      PA_PLANNING_RESERVE_MARGIN(0:UPPER_TRANS_GROUP), &
                     PA_PEAK_AFTER_INTERRUPTIBLE(0:UPPER_TRANS_GROUP), &
                     PEAK_AFTER_INTERRUPTIBLE_TG(0:UPPER_TRANS_GROUP), &
                      TG_PLANNING_RESERVE_MARGIN(0:UPPER_TRANS_GROUP), &
                     CM_PEAK_AFTER_INTERRUPTIBLE(0:UPPER_TRANS_GROUP), &
                        PA_PLANNING_CAPACITY(0:UPPER_TRANS_GROUP), &
                        CM_PLANNING_CAPACITY(0:UPPER_TRANS_GROUP), &
                        CM_IOTA(0:UPPER_TRANS_GROUP), &
                           MAXIMUM_RM_PA(0:UPPER_TRANS_GROUP), &
                         CURRENT_TARGET_RATIO_PA(0:UPPER_TRANS_GROUP), &
                            PA_ICAP_REVENUE_MULT(0:UPPER_TRANS_GROUP), &
                            CM_ICAP_REVENUE_MULT(0:UPPER_TRANS_GROUP), &
                       CM_TOT_ANN_CAPACITY_ADDED(0:UPPER_TRANS_GROUP), &
                           TG_PLANNING_CAPACITY(0:UPPER_TRANS_GROUP), &
                           TG_2_PLANNING_AREA(0:UPPER_TRANS_GROUP), &
                           TG_2_CAPACITY_MARKET(0:UPPER_TRANS_GROUP))
                           
         allocate(TOTAL_ANNUAL_CAPACITY_ADDED(0:UPPER_TRANS_GROUP), &
            stat=allocstat)
         call check_alloc("cap_objt:0063", "total_annual_capacity_added", &
            allocstat, "...was getting 'not allocated' errors elsewhere " // &
            "variable was being initialized.")
         total_annual_capacity_added=0. ! Moved from the bad place
         allocate(TG_TOTAL_ANNUAL_CAPACITY_ADDED(0:UPPER_TRANS_GROUP), &
            stat=allocstat)
            
         call check_alloc("cap_objt:0070", "tg_total_annual_capacity_added", &
            allocstat)
            tg_total_annual_capacity_added=0.
            
         allocate(MAXIMUM_ANNUAL_MRX_CAP(0:UPPER_TRANS_GROUP), &
                          MINIMUM_ANNUAL_MRX_CAP(0:UPPER_TRANS_GROUP), &
                            MINIMUM_TG_MRX_CAP(0:UPPER_TRANS_GROUP), &
                            MAXIMUM_TG_MRX_CAP(0:UPPER_TRANS_GROUP), &
                            CO2_EMISSIONS_CAP(0:UPPER_TRANS_GROUP), &
                    CO2_EMISS_REDUCTION_REQUIRED(0:UPPER_TRANS_GROUP), &
                            CO2_EMISSIONS(0:UPPER_TRANS_GROUP))
         TG_TOTAL_ANNUAL_CAPACITY_ADDED = 0.0
         MULTI_AREA_NAME(0) = 'System'
         
         DO L = 1, UPPER_TRANS_GROUP
            MULTI_AREA_NAME(L) = GET_GROUP_NAME(L)
         ENDDO
         
         call write_trace_strings(file_trace_amoc, "MULTI_AREA_NAME", &
            MULTI_AREA_NAME)
            

         HO = 0
         LAST_HO = -1

! 111507. CHANGED FOR TVA.

         HGST_NET_MARG_ADDED = MINIMUM_NET_MARGIN - 0.001 ! 0.
         call write_trace_real4(file_trace_amoc, "1. HGMA-init", &
            HGST_NET_MARG_ADDED(1))
         SORTED_OPTIONS = 0
         HGST_OPT_INDEX = 0

! SYSTEM CO2
!
         CO2_EMISSIONS_CAP = 999999999999.0
         CO2_EMISS_REDUCTION_REQUIRED = 0.0
         CO2_EMISSIONS = 0.0
         TG = 0

         call write_trace_int2(file_trace_amoc, "2. TG", TG)
         CO2_EMISSIONS_CAP(TG) = GET_EMISS_CAP_FOR_CLASS(CO2_PLN,TG)
         CO2_EMISSIONS(TG) = GET_CL_EMISS_FOR_CLASS(CO2_PLN,TG)
         CO2_EMISS_REDUCTION_REQUIRED(TG) = MAX(0.0, &
                              CO2_EMISSIONS(TG) - CO2_EMISSIONS_CAP(TG))

         RESOURCE_ADDITION = .TRUE.
!
! BEGIN CO2 RETIREMENTS
!
         IF(CO2_RETIREMENTS_LOGIC .and. .false.) THEN
            TEMP_R4 = CO2_EMISS_REDUCTION_REQUIRED(TG)
            CO2_RETIREMENT_PRICE = GET_CO2_RETIREMENT_PRICE(TEMP_R4)
            CO2_END_LIST = .FALSE.
            DO WHILE(.NOT. CO2_END_LIST .AND. &
                               CO2_EMISS_REDUCTION_REQUIRED(TG) > 0.001)
               TEMP_L = GET_NEXT_MRX_RETIRE_RETRO( &
                                          CO2_COUNTER, &
                                          CO2_RETIRE_OR_RETRO, &
                                          CO2_NUNIT, &
                                          CO2_TG, &
                                          CO2_UNIT_MW, &
                                          CO2_UNIT_MWH, &
                                          UNIT_CO2, &
                                          CO2_UNIT_PRICE, &
                                          CO2_STRIKE_PRICE, &
                                          CO2_UNIT_MW_AFTER, &
                                          CO2_STRIKE_PRICE_AFTER, &
                                          UNIT_CO2_AFTER, &
                                          CO2_END_LIST)
               IF(CO2_RETIRE_OR_RETRO == 1) THEN ! RETIRE
                  TEMP_L = RETIRE_CO2_THERMAL_UNIT( &
                                          CO2_NUNIT, &
                                          CO2_UNIT_MW, &
                                          UNIT_CO2, &
                                          CO2_UNIT_PRICE)
               ELSE
                  TEMP_L = RETROFIT_CO2_THERMAL_UNIT( &
                                          CO2_NUNIT, &
                                          CO2_UNIT_MW, &
                                          UNIT_CO2, &
                                          CO2_UNIT_PRICE, &
                                          CO2_RETIRE_OR_RETRO)
               ENDIF
               CO2_EMISS_REDUCTION_REQUIRED(0) = &
                         CO2_EMISS_REDUCTION_REQUIRED(0) - UNIT_CO2 + &
                                                          UNIT_CO2_AFTER
            ENDDO
         ENDIF
!
! END SYSTEM CO2
!
         IF(RUN_PRICE_MODE) BUILD_TO_MARKET = .FALSE.
!
            CM_PLANNING_RESERVE_MARGIN = ZERO
            CM_PEAK_AFTER_INTERRUPTIBLE = ZERO
            CM_PLANNING_CAPACITY = ZERO
            CM_ICAP_REVENUE_MULT = ZERO
            TG_2_CAPACITY_MARKET = ZERO
            CM_TOT_ANN_CAPACITY_ADDED = ZERO
!
            PA_PEAK_AFTER_INTERRUPTIBLE = ZERO
            TOTAL_ANNUAL_CAPACITY_ADDED = 0.
            MINIMUM_ANNUAL_MRX_CAP = 0.
            PA_PLANNING_CAPACITY = ZERO
!
            DO PA = 0, PG
               PA_PLANNING_RESERVE_MARGIN(PA) = ZERO

               TG_2_PLANNING_AREA(PA) = 0

               MAXIMUM_ANNUAL_MRX_CAP(PA) = 999999.
               PA_ICAP_REVENUE_MULT(PA) = 0.

!
! CURRENT_TARGET_RATIO
! STILL NEED TO PUSH THE VALUES BELOW FOR ALL MINIMUM AND MAXIMUM RM'S.
               
               IF(REGIONAL_PARAMS_EXIST) THEN
                  CURRENT_TARGET_RATIO_PA(PA) = &
                      ICAP_MIN_TESTING_MARGIN( &
                      R_YEAR,CURRENT_TARGET_RATIO,pa) * &
                      SCENARIO_RESERVE_MARGIN
!
! NEED TO CHANGE THE ORDER FOR MAX
                  
                  MAXIMUM_RM_PA(PA) = &
                           ICAP_MAX_TESTING_MARGIN( &
                           R_YEAR,MAXIMUM_RM,pa) * &
                           SCENARIO_RESERVE_MARGIN
               ELSE
                  CURRENT_TARGET_RATIO_PA(PA) = CURRENT_TARGET_RATIO
                  MAXIMUM_RM_PA(PA) = MAXIMUM_RM * &
                                                 SCENARIO_RESERVE_MARGIN
               ENDIF
            ENDDO
!
            MAXIMUM_ANNUAL_MRX_CAP(0) = GET_MAXIMUM_ANNUAL_MRX_CAP()
            TG_PLANNING_RESERVE_MARGIN = 0.0
            PEAK_AFTER_INTERRUPTIBLE_TG = 0.0
!
            DO TG = 1, UPPER_TRANS_GROUP
         call write_trace_int2(file_trace_amoc, "15. TG", TG)
               PLANNING_RM_BEFORE_ADDITIONS = ZERO
               
               TG_2_PLANNING_AREA(TG) = GET_PA_FROM_TG(TG)
               TG_2_CAPACITY_MARKET(TG) = GET_CM_FROM_TG(TG)
               PA = TG_2_PLANNING_AREA(TG)
               
               CM = TG_2_CAPACITY_MARKET(TG)
               CM_INDEX = GET_CM_INDEX_FROM_TG(CM)
!
               EXISTING_PLANNING_CAPACITY(0) = &
                           GET_CL_TG_CAP( &
                                    INT(0,2),TG,R_YEAR,INT(1,2)) + &
                           GET_EL_TG_CAP(TG,R_YEAR,INT(1,2)) + &
                             DERIV_CAPACITY_PLANNING_BY_TG(TG,R_YEAR)
!
               NEW_PLANNING_CAPACITY(0) = &
                              GET_CL_TG_CAP( &
                                    INT(0,2),TG,R_YEAR,INT(2,2)) + &
                            GET_EL_TG_CAP(TG,R_YEAR,INT(2,2)) + &
                           DERIV_NEW_CAP_PLAN_BY_ALL(ZERO_I2,TG,R_YEAR)
               PA_PLANNING_CAPACITY(PA) = PA_PLANNING_CAPACITY(PA) + &
                           EXISTING_PLANNING_CAPACITY(0) + &
                                                NEW_PLANNING_CAPACITY(0)
               CM_PLANNING_CAPACITY(CM) = CM_PLANNING_CAPACITY(CM) + &
                           EXISTING_PLANNING_CAPACITY(0) + &
                                                NEW_PLANNING_CAPACITY(0)
               IF(MRX_SORTED_MIX) THEN 
                  CAPACITY_PLANNING_PEAK = &
                                        GET_GROUP_PEAK_ON_PEAK_MONTH(TG)
               ELSEIF(USE_REGIONAL_MINIMUM_RM_LOGIC) THEN
                  CAPACITY_PLANNING_PEAK = &
                                        GET_PEAK_ON_PA_PEAK_MONTH(TG,PA)
               ELSE
                  CAPACITY_PLANNING_PEAK = &
                                        GET_GROUP_PEAK_ON_PEAK_MONTH(TG)
               ENDIF
!
               INTERRUPTIBLE_LOAD = GET_ANNUAL_INTER_CAPACITY(TG)
               PA_PEAK_AFTER_INTERRUPTIBLE(PA) = &
                     PA_PEAK_AFTER_INTERRUPTIBLE(PA) + &
                        MAX(0., &
                            CAPACITY_PLANNING_PEAK - INTERRUPTIBLE_LOAD)
               PA_PEAK_MW(PA) = PA_PEAK_AFTER_INTERRUPTIBLE(PA)
               CM_PEAK_AFTER_INTERRUPTIBLE(CM) = &
                     CM_PEAK_AFTER_INTERRUPTIBLE(CM) + &
                        MAX(0., &
                            CAPACITY_PLANNING_PEAK - INTERRUPTIBLE_LOAD)
               CM_PEAK_MW(CM) = CM_PEAK_AFTER_INTERRUPTIBLE(CM)
!
! 090707. MIN AND MAX RM BY TG
!
               TG_PLANNING_CAPACITY(TG) = &
                           EXISTING_PLANNING_CAPACITY(0) + &
                                                NEW_PLANNING_CAPACITY(0)
               PEAK_AFTER_INTERRUPTIBLE_TG(TG) = &
                        MAX(0., &
                            CAPACITY_PLANNING_PEAK - INTERRUPTIBLE_LOAD)
               IF(PEAK_AFTER_INTERRUPTIBLE_TG(TG) > NEAR_ZERO)  THEN
                  TG_PLANNING_RESERVE_MARGIN(TG) = &
                        1. + (TG_PLANNING_CAPACITY(TG) - &
                                  PEAK_AFTER_INTERRUPTIBLE_TG(TG)) / &
                                         PEAK_AFTER_INTERRUPTIBLE_TG(TG)
                  
                  MINIMUM_TG_MRX_CAP(TG) = &
                           MAX(0., &
                              (GET_MIN_CAP_TESTING_RATIO(TG)* &
                                             SCENARIO_RESERVE_MARGIN - &
                                  TG_PLANNING_RESERVE_MARGIN(TG))* &
                                        PEAK_AFTER_INTERRUPTIBLE_TG(TG))
!

                  MAXIMUM_TG_MRX_CAP(TG) = &
                           MIN(999999., &
                                (GET_MAX_CAP_TESTING_RATIO(TG)* &
                                             SCENARIO_RESERVE_MARGIN - &
                                     TG_PLANNING_RESERVE_MARGIN(TG)) * &
                                        PEAK_AFTER_INTERRUPTIBLE_TG(TG))
                  TG_PEAK_MW(TG) = PEAK_AFTER_INTERRUPTIBLE_TG(TG)
               ENDIF


            ENDDO ! TRANSACTION GROUPS

! 06/10/03. CHANGED TO PG COUNTER

         DO PA = 1, PG

               PA_PEAK_AFTER_INTERRUPTIBLE(0) = &
                  PA_PEAK_AFTER_INTERRUPTIBLE(0) + &
                     PA_PEAK_AFTER_INTERRUPTIBLE(PA)
               PA_PLANNING_CAPACITY(0) = &
                  PA_PLANNING_CAPACITY(0) + &
                     PA_PLANNING_CAPACITY(PA)
!
               IF(PA_PEAK_AFTER_INTERRUPTIBLE(PA) > NEAR_ZERO)  THEN
                  PA_PLANNING_RESERVE_MARGIN(PA) = &
                        1. + (PA_PLANNING_CAPACITY(PA) - &
                                 PA_PEAK_AFTER_INTERRUPTIBLE(PA)) / &
                                      PA_PEAK_AFTER_INTERRUPTIBLE(PA)
                  MINIMUM_ANNUAL_MRX_CAP(PA) = MAX(0., &
                        (CURRENT_TARGET_RATIO_PA(PA) - &
                           PA_PLANNING_RESERVE_MARGIN(PA))* &
                                        PA_PEAK_AFTER_INTERRUPTIBLE(PA))
!

      
                  IF(USE_MAXIMUM_RM_LOGIC) THEN
                     MAXIMUM_ANNUAL_MRX_CAP(PA) = &
                       MIN(MAXIMUM_ANNUAL_MRX_CAP(PA), &
                           MAX(0., &
                                MAXIMUM_RM_PA(PA) - &
                                     PA_PLANNING_RESERVE_MARGIN(PA)) * &
                                       PA_PEAK_AFTER_INTERRUPTIBLE(PA))
                  ENDIF
               ELSE
                  PA_PLANNING_RESERVE_MARGIN(PA) = ZERO
               ENDIF
!
! 022210. TAKEN OUT: PA TO CM
               
               if(R_YEAR==2 .and. pa==1) then
                    R_YEAR=R_YEAR ! Debugstop
               end if
               PA_ICAP_REVENUE_MULT(PA) = GET_ICAP_REVENUE_MULT( &
                               R_YEAR,pa,PA_PLANNING_RESERVE_MARGIN(PA))
         ENDDO
         IF(PA_PEAK_AFTER_INTERRUPTIBLE(0) > NEAR_ZERO)  THEN
            PA_PLANNING_RESERVE_MARGIN(0) = &
                        1. + (PA_PLANNING_CAPACITY(0) - &
                                 PA_PEAK_AFTER_INTERRUPTIBLE(0)) / &
                                      PA_PEAK_AFTER_INTERRUPTIBLE(0)
         ENDIF
!
         IF(R_YEAR >= 5) Then
           CM = CM
         Endif
         DO CM = 1, CG
               
               ICAP_SWITCH_INDEX(CM) = &
                    GET_ICAP_SWITCH_INDEX(CM, &
                                          R_YEAR, &
                                          TEMP_R4)
               ICAP_CONE_PRICE(CM) = TEMP_R4 
               ! MONTHLY TO ANNUAL IN GET_ICAP... *12.0
!
               IF(CM_PEAK_AFTER_INTERRUPTIBLE(CM) > NEAR_ZERO)  THEN
                  CM_PLANNING_RESERVE_MARGIN(CM) = &
                        1. + (CM_PLANNING_CAPACITY(CM) - &
                                 CM_PEAK_AFTER_INTERRUPTIBLE(CM)) / &
                                      CM_PEAK_AFTER_INTERRUPTIBLE(CM)
               ELSE
                  CM_PLANNING_RESERVE_MARGIN(CM) = ZERO
               ENDIF
               
               if(r_year==2 .and. CM==1) then
                    r_YEAR=r_YEAR ! Debugstop
               end if
               CM_ICAP_REVENUE_MULT(CM) = GET_ICAP_REVENUE_MULT( &
                         R_YEAR,CM,CM_PLANNING_RESERVE_MARGIN(CM))
         ENDDO
!
         FirmTransfer = 0.0
         CM_Net_Transfer = 0.0
         PA_Net_Transfer = 0.0
         NUMBER_DIRECT_INTERCONNECTS = 0
         DIRECT_INTERCONNECT_TG = 0

         TransmissionConstraint = 0.0
         TransferConvergence = 0.01
         MO = 7
         TEMP_L = INIT_HOUR_PATH_LIMIT(LOCAL_TIME_OF_DAY, &
                                       MO, &
                                       R_YEAR)
         DO I = 1, UPPER_TRANS_GROUP
            DO J = I-1,1,-1 ! DON'T CHECK DIAGONAL
               IF(HOUR_PATH_ACTIVE(I,J)) THEN
                  NUMBER_DIRECT_INTERCONNECTS(J) = &
                                      NUMBER_DIRECT_INTERCONNECTS(J) + 1
                  DIRECT_INTERCONNECT_TG(J, &
                                     NUMBER_DIRECT_INTERCONNECTS(J)) = I
                  TransmissionConstraint(I,J) = HOUR_TIE_LIMIT(I,J)  * &
                                       GetCapTransMultiplier(I,J,R_YEAR)
               ENDIF
               IF(HOUR_PATH_ACTIVE(J,I)) THEN
                  NUMBER_DIRECT_INTERCONNECTS(I) = &
                                      NUMBER_DIRECT_INTERCONNECTS(I) + 1
                  DIRECT_INTERCONNECT_TG(I, &
                                     NUMBER_DIRECT_INTERCONNECTS(I)) = J
                  TransmissionConstraint(J,I) = HOUR_TIE_LIMIT(J,I)  * &
                                       GetCapTransMultiplier(J,I,R_YEAR)
               ENDIF
            END DO
         END DO
         MaxTransmissionConstraint = TransmissionConstraint
         MaxTransferValue = 999.0
         LAST_SELLER = -1
         LAST_BUYER = -1
!
! TEST
!
         TARGET_IS_PA = .FALSE.
         TG_MW_TRANSFERS = .TRUE.
         MAX_ITER = 20 * UPPER_TRANS_GROUP
         ITER = 0
         DO
            ITER = ITER + 1
            IF(ITER == MAX_ITER) EXIT
            TransferValue = -999.0
            MaxTransferValue = -999.0

            SELLER = 0
            BUYER = 0
            call write_trace_message(file_trace_amoc, "seller/buyer=0")

!
! IDENTIFY MaxTransferValue, SELLER, BUYER
!
            DO I = 1, UPPER_TRANS_GROUP
               DO J = I-1,1,-1 ! DON'T CHECK DIAGONAL
                  IF(TG_PEAK_MW(I) > NEAR_ZERO) THEN
                     CM = TG_2_CAPACITY_MARKET(I)
                     TEMP_R = &
                           (TG_Net_Transfer(I) + &
                                             TG_PLANNING_CAPACITY(I))/ &
                                                       TG_PEAK_MW(I)
                                                       
                    call write_trace_real(file_trace_amoc, "1. TEMP_R", &
                        TEMP_R)
                        

                     TransferValue(I,J) = TEMP_R

                  ELSE
                     TransferValue(I,J) = 999.0 ! INFINITY
                  ENDIF
                  IF(TG_PEAK_MW(J) > NEAR_ZERO) THEN
                     CM = TG_2_CAPACITY_MARKET(J)
                     TEMP_R = &
                           (TG_Net_Transfer(J) + &
                                             TG_PLANNING_CAPACITY(J))/ &
                                                       TG_PEAK_MW(J)
                call write_trace_real(file_trace_amoc, "2. TEMP_R", &
                        TEMP_R)
                     TransferValue(J,I) = TEMP_R

                  ELSE
                     TransferValue(J,I) = 999.0 ! INFINITY
                  ENDIF
                  PA = TG_2_PLANNING_AREA(I)
                  IF(TARGET_IS_PA) THEN
                     TEMP_R = PA_PLANNING_RESERVE_MARGIN(PA)
                call write_trace_real(file_trace_amoc, "3. TEMP_R", &
                        TEMP_R)
                  ELSE
                     TEMP_R = PA_PLANNING_RESERVE_MARGIN(0)
                call write_trace_real(file_trace_amoc, "4. TEMP_R", &
                        TEMP_R)
                  ENDIF
                  I_CAPACITY = MAX(0.0, &
                         TG_Net_Transfer(I) + &
                         TG_PLANNING_CAPACITY(I)- &
                                     CURRENT_TARGET_RATIO_PA(PA)* &
                                                     TG_PEAK_MW(I) )

                  PA = TG_2_PLANNING_AREA(J)
                  IF(TARGET_IS_PA) THEN
                     TEMP_R = PA_PLANNING_RESERVE_MARGIN(PA)
                call write_trace_real(file_trace_amoc, "5. TEMP_R", &
                        TEMP_R)
                  ELSE
                     TEMP_R = PA_PLANNING_RESERVE_MARGIN(0)
                call write_trace_real(file_trace_amoc, "6. TEMP_R", &
                        TEMP_R)
                  ENDIF
                  J_CAPACITY = MAX(0.0, &
                         TG_Net_Transfer(J) + &
                         TG_PLANNING_CAPACITY(J)- &
                                     CURRENT_TARGET_RATIO_PA(PA)* &
                                                     TG_PEAK_MW(J) )

                  IF(I_CAPACITY > 0.1 .OR. J_CAPACITY > 0.1) THEN
                     IF(TransferValue(I,J) > TransferValue(J,I) .AND. &
                                 I_CAPACITY > 0.1 .AND. &
                                 TransmissionConstraint(I,J) > 0.1) THEN
                        TEMP_R = TransferValue(I,J) - TransferValue(J,I)
                call write_trace_real(file_trace_amoc, "7. TEMP_R", &
                        TEMP_R)
                        IF(I == LAST_SELLER .AND. &
                                          J == LAST_BUYER) THEN
                           SAME_TRANSACTION = .TRUE.
                        ELSE
                           SAME_TRANSACTION = .FALSE.
                        ENDIF
                        IF(TEMP_R > MaxTransferValue .AND. &
                                             .NOT. SAME_TRANSACTION)THEN
                           SELLER = I
                           BUYER = J
            call write_trace_int2(file_trace_amoc, "1a. SELLER", SELLER)
            call write_trace_int2(file_trace_amoc, "1a. BUYER", BUYER)
                           SELLER_CAPACITY = I_CAPACITY
                           BUYER_CAPACITY = J_CAPACITY
                       call write_trace_int2(file_trace_amoc, "1. SELLER", &
                        SELLER)
                       call write_trace_int2(file_trace_amoc, "1. BUYER", &
                        BUYER)
                        
                           MaxTransferValue = TEMP_R
                        ENDIF
                     ELSEIF(TransmissionConstraint(J,I)> 0.1 .AND. &
                                                  J_CAPACITY > 0.1) THEN
                        TEMP_R = TransferValue(J,I) - TransferValue(I,J)
                call write_trace_real(file_trace_amoc, "30. TEMP_R", &
                        TEMP_R)
                        IF(J == LAST_SELLER .AND. &
                                          I == LAST_BUYER) THEN
                           SAME_TRANSACTION = .TRUE.
                        ELSE
                           SAME_TRANSACTION = .FALSE.
                        ENDIF
                        IF(TEMP_R > MaxTransferValue .AND. &
                                             .NOT. SAME_TRANSACTION)THEN
                           SELLER = J
                           BUYER = I

                       call write_trace_int2(file_trace_amoc, "2. SELLER", &
                        SELLER)
                       call write_trace_int2(file_trace_amoc, "2. BUYER", &
                        BUYER)
                           SELLER_CAPACITY = J_CAPACITY
                           BUYER_CAPACITY = I_CAPACITY
                           MaxTransferValue = TEMP_R
                        ENDIF ! CHECK FOR IMPROVED SOLUTION
                     ENDIF ! I OR J SELLER
                  ENDIF ! CAPACITY CHECK
               END DO
            END DO

            IF(SELLER == 0 .OR. BUYER == 0 .OR. &
                            MaxTransferValue < TransferConvergence) then
                EXIT
            end if

! IDENTIFY QUANTITY TO TRANSFER

            TEMP_R = MIN( &
                         SELLER_CAPACITY, &
                         TransmissionConstraint(SELLER,BUYER))
                call write_trace_real(file_trace_amoc, "8. TEMP_R", &
                        TEMP_R)

            X_SEARCH = TEMP_R
            HALF = X_SEARCH
            Y_BEFORE_SELLER = TransferValue(SELLER,BUYER)
            X_BEFORE_SELLER = TEMP_R
            Y_BEFORE_BUYER = TransferValue(BUYER,SELLER)
            X_BEFORE_BUYER = 0.0
            DO I = 1, 10
               X_AFTER_SELLER = TEMP_R - X_SEARCH
               Y_AFTER_SELLER = &
                      (TG_Net_Transfer(SELLER) + &
                       TG_PLANNING_CAPACITY(SELLER)-X_SEARCH)/ &
                                                 TG_PEAK_MW(SELLER)
               X_AFTER_BUYER = X_SEARCH
               Y_AFTER_BUYER = &
                      (TG_Net_Transfer(BUYER) + &
                       TG_PLANNING_CAPACITY(BUYER)+X_SEARCH)/ &
                                               TG_PEAK_MW(BUYER)
               IF(ABS(Y_AFTER_SELLER - Y_AFTER_BUYER) < 0.01) THEN
                  TEMP_R = X_SEARCH
                call write_trace_real(file_trace_amoc, "9. TEMP_R", &
                        TEMP_R)
                  EXIT
               ELSE
                  HALF = HALF * 0.5
                  IF(Y_AFTER_SELLER > Y_AFTER_BUYER) THEN
                     IF(I == 1) THEN
                        TEMP_R = X_SEARCH
                call write_trace_real(file_trace_amoc, "10. TEMP_R", &
                        TEMP_R)
                        EXIT
                     ELSE ! X_SEARCH IS TOO SMALL
                        X_SEARCH = X_SEARCH + HALF
                     ENDIF
                  ELSE ! X_SEARCH IS TOO BIG
                     X_SEARCH = X_SEARCH - HALF
                  ENDIF
               ENDIF
            ENDDO


            IF(FirmTransfer(BUYER,SELLER) < 0.001) THEN
               FirmTransfer(SELLER,BUYER) = &
                     FirmTransfer(SELLER,BUYER) + TEMP_R
            ELSE
               IF(FirmTransfer(BUYER,SELLER) > TEMP_R) THEN
                  FirmTransfer(BUYER,SELLER) = &
                                     FirmTransfer(BUYER,SELLER) - TEMP_R
               ELSE
                  FirmTransfer(SELLER,BUYER) = &
                                     TEMP_R - FirmTransfer(BUYER,SELLER)
                  FirmTransfer(BUYER,SELLER) = 0.0
               ENDIF
            ENDIF
            TransmissionConstraint(SELLER,BUYER) = &
                        MaxTransmissionConstraint(SELLER,BUYER) - &
                                              FirmTransfer(SELLER,BUYER)
            TransmissionConstraint(BUYER,SELLER) = &
                        MaxTransmissionConstraint(BUYER,SELLER) - &
                                              FirmTransfer(BUYER,SELLER)

            TG_Net_Transfer(SELLER) = TG_Net_Transfer(SELLER) - TEMP_R

            TG_Net_Transfer(BUYER) = TG_Net_Transfer(BUYER) + TEMP_R
            SELLER_CM = TG_2_CAPACITY_MARKET(SELLER)
            BUYER_CM = TG_2_CAPACITY_MARKET(BUYER)
         call write_trace_int2(file_trace_amoc, "2. buyer_cm", buyer_cm)
            SELLER_PA = TG_2_PLANNING_AREA(SELLER)
            BUYER_PA = TG_2_PLANNING_AREA(BUYER)
            IF(SELLER_CM /= BUYER_CM) THEN
             CM_Net_Transfer(SELLER_CM) = CM_Net_Transfer(SELLER_CM) - &
                                                                  TEMP_R
               CM_Net_Transfer(BUYER_CM) = CM_Net_Transfer(BUYER_CM) + &
                                                                  TEMP_R
            ENDIF
            IF(SELLER_PA /= BUYER_PA) THEN
             PA_Net_Transfer(SELLER_PA) = PA_Net_Transfer(SELLER_PA) - &
                                                                  TEMP_R
               PA_Net_Transfer(BUYER_PA) = PA_Net_Transfer(BUYER_PA) + &
                                                                  TEMP_R
            ENDIF
            LAST_SELLER = SELLER
            LAST_BUYER = BUYER
         END DO
         call write_trace_bool1(file_trace_amoc, "GRX_8761_REPORT", &
            GRX_8761_REPORT)
            
         IF(GRX_8761_REPORT) THEN
!
            IF(GRX_8761_REPORT_NOT_OPEN) THEN
               GRX_8761_REPORT_NOT_OPEN = .FALSE.
               GRX_8761_REPORT_VARIABLES = UPPER_TRANS_GROUP+1
               GRX_8761_ALT_NO = GRX_8761_HEADER( &
                                  GRX_8761_REPORT_VARIABLES, &
                                  GRX_8761_ALT_REC)
!
               OPEN(10,FILE="MSG"//TRIM(GET_SCENAME())//".GP1")
               DO J = 0,  UPPER_TRANS_GROUP
                  WRITE(TEMP_STR,'(I6)') J
                  TEMP_STR = '"'//TRIM(MULTI_AREA_NAME(J))//'"'// &
                             ',V2,'//TRIM(TEMP_STR)// &
                           ',,S,,,,,,,,,"Help Message"'
                  WRITE(10,'(A)') TRIM(TEMP_STR)
               END DO
               CLOSE(10)
            ENDIF
            DO I = 0, UPPER_TRANS_GROUP
               GRX_8761_ALT_REC = rptrec(GRX_8761_ALT_NO)
               WRITE(GRX_8761_ALT_NO,REC=GRX_8761_ALT_REC) &
                        PRT_ENDPOINT(), &
                        FLOAT(THIS_YEAR), &
                        'Firm Transfer       ', &
                        MULTI_AREA_NAME(I), &
                        (FirmTransfer(I,J),J=0,UPPER_TRANS_GROUP)
               GRX_8761_ALT_REC = GRX_8761_ALT_REC + 1
!
               GRX_8761_ALT_REC = RPTREC(GRX_8761_ALT_NO)
               WRITE(GRX_8761_ALT_NO,REC=GRX_8761_ALT_REC) &
                        PRT_ENDPOINT(), &
                        FLOAT(THIS_YEAR), &
                        'Capacity Available  ', &
                        MULTI_AREA_NAME(I), &
                        (TransmissionConstraint(I,J), &
                                                  J=0,UPPER_TRANS_GROUP)
               GRX_8761_ALT_REC = GRX_8761_ALT_REC + 1
               GRX_8761_ALT_REC = RPTREC(GRX_8761_ALT_NO)
               WRITE(GRX_8761_ALT_NO,REC=GRX_8761_ALT_REC) &
                        PRT_ENDPOINT(), &
                        FLOAT(THIS_YEAR), &
                        'Max Trans Constraint', &
                        MULTI_AREA_NAME(I), &
                        (MaxTransmissionConstraint(I,J), &
                                                  J=0,UPPER_TRANS_GROUP)
               GRX_8761_ALT_REC = GRX_8761_ALT_REC + 1
               GRX_8761_ALT_REC = RPTREC(GRX_8761_ALT_NO)
               WRITE(GRX_8761_ALT_NO,REC=GRX_8761_ALT_REC) &
                        PRT_ENDPOINT(), &
                        FLOAT(THIS_YEAR), &
                        'Transfer Value      ', &
                        MULTI_AREA_NAME(I), &
                        (TransferValue(I,J),J=0,UPPER_TRANS_GROUP)
               GRX_8761_ALT_REC = GRX_8761_ALT_REC + 1
            END DO
         ENDIF


         MAX_RESOURCES_PER_GROUP = 0
         MAX_RESOURCES_PER_YEAR_GROUP = &
                                MX_RES_P_YEAR/UPPER_TRANS_GROUP
!
         HIGHEST_ENERGY = 0.

         IF(MX_SUMMARY_REPORT .AND. MX_REPORT_NOT_OPEN) THEN
            MX_REPORT_NOT_OPEN = .FALSE.
            MX_ANNUAL_ALT_NO = MX_ANNUAL_HEADER( &
                                  MX_REPORT_VARIABLES,MX_ANNUAL_ALT_REC)
         ENDIF
!
         RPS_CURVE_REPORT = YES_RPS_CURVE_REPORT()
         IF(RPS_CURVE_REPORT .AND. VN_REPORT_NOT_OPEN) THEN
            VN_REPORT_NOT_OPEN = .FALSE.
            VN_ANNUAL_ALT_NO = RPS_CURVE_RPT_HEADER( &
                                  VN_REPORT_VARIABLES,VN_ANNUAL_ALT_REC)
         ENDIF
         CX_ACTIVE = .TRUE.
         IF(.NOT. CX_ACTIVE) GOTO 1234

         LOCAL_CHRONO_MARKET_PRICES = 0.0
         DO L = 1, UPPER_TRANS_GROUP
            IF(CX_ACTIVE) THEN
            call write_trace_int2(file_trace_amoc, "1. L", L)
            call write_trace_message(file_trace_amoc, &
                "calling CX_TRANS_ANNUAL_USER_MARKET")
                
               CALL CX_TRANS_ANNUAL_USER_MARKET( &
                                      L,LOCAL_CHRONO_MARKET_PRICES(1,L))
                                      
            call write_trace_real4(file_trace_amoc,  &
       "LOCAL_CHRONO_MARKET_PRICES", LOCAL_CHRONO_MARKET_PRICES(1,L))

            ENDIF
            call write_trace_message(file_trace_amoc, "1. enddo")
         ENDDO
         call write_trace_message(file_trace_amoc, "1. exit enddo")
         
         RESET_CHRONO_MARKET_PRICES = LOCAL_CHRONO_MARKET_PRICES
         NET_MARGIN = -999999.
         
         NET_MARGIN_PER_KWYR = -999999.
         LOCAL_RESOURCES_AVAILABLE = .FALSE.

         AI = 0
         TOTAL_ACTIVE_INSTANCES = 0
         DO  AO = 1, TOTAL_ACTIVE_OPTIONS
            CN = ACTIVE_OPTION(AO)
               call write_trace_int2(file_trace_amoc, "y. CN", CN)
               call write_trace_int2(file_trace_amoc, "AO", AO)
        call write_trace_int2(file_trace_amoc, "4. ACTIVE_OPTION(AO)", &
            ACTIVE_OPTION(AO))
            IF(DEPENDENT(CN) > 0) then
                call write_trace_message(file_trace_amoc, "10. cycle")
                CYCLE ! 011920.
            end if
            call write_trace_int2(file_trace_amoc, "7. CN", CN)
            call write_trace_int2(file_trace_amoc, "LAN(CN)", &
                LOCAL_ANNUAL_UNITS(CN))

            DO J = 1, LOCAL_ANNUAL_UNITS(CN)
               AI = AI + 1
               IF(AI > MX_RES_P_YEAR) then
               call write_trace_message(file_trace_amoc, "11. exit")
                EXIT
               end if
               ACTIVE_INSTANCE(AI) = AO
            call write_trace_int2(file_trace_amoc,"1. SORTED_OPTIONS", AI)
               SORTED_OPTIONS(AI) = AI
            ENDDO
            IF(AI > MX_RES_P_YEAR) then
            call write_trace_message(file_trace_amoc, "12. exit")
                EXIT
            end if
         ENDDO

         TOTAL_ACTIVE_INSTANCES = AI
        call write_trace_int2(file_trace_amoc, "TOTAL_ACTIVE_INSTANCES", &
            TOTAL_ACTIVE_INSTANCES)
            
         LOCAL_CHRONO_MW_USAGE = 0.0
         LOCAL_CHRONO_STRIKE_PRICE = 0.0
         CX_ITER = 0

         MAX_CX_ITER = 5
         OPTIMAL_ORDER = .FALSE.
         TG_TOTAL_ANNUAL_CAPACITY_ADDED = 0.
      call write_trace_int2(file_trace_amoc, "MAX_CX_ITER", MAX_CX_ITER)
        ! Optimal_order a clue?
         DO WHILE(CX_ITER < MAX_CX_ITER .AND. .NOT. OPTIMAL_ORDER)
            call write_trace_int2(file_trace_amoc, "CX_ITER", CX_ITER)
            
            HGST_NET_MARG_ADDED = -999999. ! MAY NEED TO CHANGE
         call write_trace_real4(file_trace_amoc, "2. HGMA-init", &
            HGST_NET_MARG_ADDED(1))
            TEMP_L = RESET_MRX_RPS_TEST_REQ() ! 032319
        call write_trace_int2(file_trace_amoc, "TOTAL_ACTIVE_INSTANCES",  &
            TOTAL_ACTIVE_INSTANCES)
           call write_trace_int2s(file_trace_amoc, "99. SORTED_OPTIONS", &
           sorted_options(:TOT_ALL_OPTIONS_cap))
          amoccount=amoccount+1
          call write_trace_int4(file_trace_amoc, "amoccount", amoccount)
           
           
           
           
           call write_trace_message(file_trace_amoc, "Start TAI loop.")
            tai_loopcount=0
            DO AI = 1, TOTAL_ACTIVE_INSTANCES
               tai_loopcount=tai_loopcount+1
               call write_trace_int4(file_trace_amoc, "tai_loop", &
                tai_loopcount)
               call write_trace_int2(file_trace_amoc, "loop-AI", AI)
               
               J = TOTAL_ACTIVE_INSTANCES - AI + 1
               call write_trace_int2(file_trace_amoc, "1. J", J)

               J = SORTED_OPTIONS(J)
         call write_trace_int2(file_trace_amoc,"3. SORTED_OPTION", &
      sorted_options(J))
               call write_trace_int2(file_trace_amoc, "2. J", J)
               AO = ACTIVE_INSTANCE(J)
               call write_trace_int2(file_trace_amoc, "1. AO", AO)
               CN = ACTIVE_OPTION(AO)
               call write_trace_int2(file_trace_amoc, "1. CN", CN)
               
               I = OPTION_POSITION(CN)
               call write_trace_int2(file_trace_amoc, "2. I", I)
               
               IF( .NOT. RESOURCE_AVAILABLE(I, &
                                    THIS_YEAR_PLUS_LEAD_TIME(CN)) .OR. &
                      .NOT. TEST_START_STOP_YEARS(I, &
                                    THIS_YEAR_PLUS_LEAD_TIME(CN)) .OR. &
                               .NOT. DEPENDENT_UNIT_AVAILABLE(CN)) then
            call write_trace_message(file_trace_amoc, "13. cycle")
                   CYCLE
               end if
!
               IF(CX_ITER == 0) THEN
                  IF(FIRST_CN(CN) == 0) THEN
                     FIRST_CN(CN) = 1
                  ELSE
                  call write_trace_message(file_trace_amoc, "14. cycle")
                     CYCLE
                  ENDIF
               ENDIF
               TG = ns_cap_trans%transaction_group(CN)
               call write_trace_int2(file_trace_amoc, "3. TG", TG)
               L = GET_TRANS_GROUP_POSITION(TG)
               CM = GET_CM_FROM_TG(L)
               PA = TG_2_PLANNING_AREA(L)
               LOCAL_RESOURCES_AVAILABLE = .TRUE.
               call write_trace_int2(file_trace_amoc, "2. L", L)
               call write_trace_int2(file_trace_amoc, "2. PA", PA)
               call write_trace_int2(file_trace_amoc, "2. CM", CM)
               

! ENERGY CALCULATIONS.
       call write_trace_bool1(file_trace_amoc, "CX_ACTIVE", CX_ACTIVE)
               IF(CX_ACTIVE) THEN
                  IF(FILE_SOURCE_INDEX(CN) == Transmission) THEN
                     co_prod_pointer=GET_GRX_RESOURCE_LINK_ID(I)


                     IF(co_prod_pointer < 1) THEN
                        CYCLE ! NEED TO CLEAN UP
                     ENDIF
                     ! continue tracing here
                     co_prod_pointer= &
                        GET_TRANSMISSION_FOR_GRX_ID(co_prod_pointer)

                     BUYER = GET_TG_FOR_TRANSMISSION_BUY(co_prod_pointer)
                     SELLER = GET_TG_FOR_TRANSMISSION_SELL(co_prod_pointer)
                       call write_trace_int2(file_trace_amoc, "3. SELLER", &
                        SELLER)
                       call write_trace_int2(file_trace_amoc, "3. BUYER", &
                        BUYER)
                     IF(BUYER < 1 .OR. SELLER < 1) THEN
                        call write_trace_message(file_trace_amoc, &
                            "15. cycle")
                        CYCLE ! NEED TO CLEAN UP
                     ENDIF
                     ANNUAL_STRIKES = 0.0
                     ANNUAL_ENERGY = 0.0
                     call write_trace_message(file_trace_amoc, "AE to zero.")
                     call write_trace_real4(file_trace_amoc, "14. AE", ANNUAL_ENERGY)
                     ANNUAL_ENERGY_REVENUE(CN) = 0.0
                     call write_trace_real4(file_trace_amoc, "1. AER", &
                     ANNUAL_ENERGY_REVENUE(CN))
                     
                     DO HR = 1, 8760
                        IF(LOCAL_CHRONO_MARKET_PRICES(HR,SELLER) > &
                           LOCAL_CHRONO_MARKET_PRICES(HR,BUYER) + &
                                                               .01) THEN
                           ANNUAL_STRIKES = ANNUAL_STRIKES + 1
                           ANNUAL_ENERGY = ANNUAL_ENERGY + &
                                                    SCREEN_CAPACITY(CN)
                            call write_trace_int2(file_trace_amoc, "CN", CN)
                            call write_trace_real4(file_trace_amoc, "3. AE", ANNUAL_ENERGY)
                            call write_trace_real4(file_trace_amoc, "SC", SCREEN_CAPACITY(CN))
                           ANNUAL_ENERGY_REVENUE(CN) = &
                              ANNUAL_ENERGY_REVENUE(CN) + &
                                               SCREEN_CAPACITY(CN) * &
                              (LOCAL_CHRONO_MARKET_PRICES(HR,SELLER) - &
                               LOCAL_CHRONO_MARKET_PRICES(HR,BUYER))
           call write_trace_int2(file_trace_amoc, "CN", CN)
                     call write_trace_real4(file_trace_amoc, "2. AER", &
                     ANNUAL_ENERGY_REVENUE(CN))
                        ENDIF
                     ENDDO

                     ANNUAL_VARIABLE_COST(CN) = 0.0


                     VOM_COST_MM(CN) = ANNUAL_VOM_COST * 0.000001


                     ANNUAL_VOM_COST = ANNUAL_VARIABLE_COST(CN)
                     STRIKE_PRICE(CN,:) = VAR_OM_PER_MWH(CN) + &
                                                   EMISSIONS_PER_MWH(CN)
                     ANNUAL_EMIS_COST = 0.0
!
                  ELSEIF(FILE_SOURCE_INDEX(CN) == Derivative) THEN
                     co_prod_pointer=GET_GRX_RESOURCE_LINK_ID(I)


                     IF(co_prod_pointer < 1) THEN
                        CYCLE ! NEED TO CLEAN UP
                     ENDIF
                     co_prod_pointer=GET_TRANS_FOR_GRX_ID( &
                        co_prod_pointer)


                     IF(co_prod_pointer < 1) THEN
                        CYCLE ! NEED TO CLEAN UP
                     ENDIF
                     IF(CONTRACT_IS_STORAGE(co_prod_pointer) .AND. &
                                                 STORAGE_IS_ACTIVE) THEN
                        ANNUAL_EMIS_COST = 0.0
                        LOCAL_HYBRID_MW_DISCHARGE = 999999.0
                        LOCAL_HYBRID_HR_MARGIN = 0.0
                        LOCAL_HYBRID_MW_CHARGE = 999999.0
         call write_trace_real4(file_trace_amoc, "15. AE", ANNUAL_ENERGY)
         cx_called_from="cap_objt:cx_001"
          call write_trace_real4(file_trace_amoc, &
            "8. ANNUAL_ENERGY_REVENUE", &
            ANNUAL_ENERGY_REVENUE(CN))
                        TEMP_L = CX_DailyOperAnPS2( &
                                        co_prod_pointer, & !  TRANS
                                      LOCAL_CHRONO_MARKET_PRICES(1,L), &
                                      !  8784, TG
                                        LOCAL_CHRONO_MW_USAGE, & !  8784
                                        ANNUAL_ENERGY, &
                                        ANNUAL_CHARGE, &
                                        ANNUAL_ENERGY_REVENUE(CN), &
                                        ANNUAL_VOM_COST, &
                                        LOCAL_HYBRID_MW_CHARGE, &
                                        LOCAL_HYBRID_MW_DISCHARGE, &
                                        LOCAL_HYBRID_HR_MARGIN, &
                                        LOCAL_HYBRID_PROFIT)
         call write_trace_real4(file_trace_amoc, "16. AE", ANNUAL_ENERGY)

                     ELSE
         call write_trace_real4(file_trace_amoc, "17. AE", ANNUAL_ENERGY)

     
         cxac_called_from="cap_objt:cxac_001"
                 call write_trace_int2(file_trace_amoc, "CN", CN)

                        TEMP_L = CX_ANNUAL_CONTRACT( &
                                        co_prod_pointer, &
                                      LOCAL_CHRONO_MARKET_PRICES(1,L), &
                                        MARKET_HOURS, &
                                        ANNUAL_STRIKES, & !  NOT USED
                                        ANNUAL_ENERGY, &
                                        ANNUAL_ENERGY_REVENUE(CN), &
                                        ANNUAL_VOM_COST, &
                                        ANNUAL_EMIS_COST, &
                                        LOCAL_CHRONO_MW_USAGE, &
                                        LOCAL_CHRONO_STRIKE_PRICE, &
                                        EMISSIONS_PER_MWH(CN))
          call write_trace_real4(file_trace_amoc, &
            "9. ANNUAL_ENERGY_REVENUE", &
            ANNUAL_ENERGY_REVENUE(CN))
         call write_trace_real4(file_trace_amoc, "18. AE", ANNUAL_ENERGY)
                     ENDIF
                     ANNUAL_VARIABLE_COST(CN) = ANNUAL_VOM_COST + &
                                                        ANNUAL_EMIS_COST
                     VOM_COST_MM(CN) = ANNUAL_VOM_COST * 0.000001
                     IF(ANNUAL_ENERGY > 0.000001) THEN
                        VAR_OM_PER_MWH(CN) = &
                                          ANNUAL_VOM_COST/ ANNUAL_ENERGY
                     ENDIF

                     STRIKE_PRICE(CN,:) = VAR_OM_PER_MWH(CN) + &
                                                   EMISSIONS_PER_MWH(CN)
                  ELSEIF(FILE_SOURCE_INDEX(CN) == CapLimited) THEN
                    co_prod_pointer=GET_PRODUCTION_DATA_POINTER(I)


                     TEMP_L = .TRUE.
                     TEMP_I2 = -99
                     WRITE(CX_NAME,1011) &
                                            OPTION_NAME(CN)(1:14),CN,HO
 1011  FORMAT(A,I3,I3)
!
         call write_trace_real4(file_trace_amoc, "28. AE", ANNUAL_ENERGY)
                     TEMP_L =  CX_UNIT( &
                                        TEMP_L, & !  ANNUAL SIMULATION
                                        CX_ITER, & !  032017
                                        CX_NAME, &
                                        BLOCK_CAPACITY(1,CN), &
                                        BLOCK_HEAT_RATE(1,CN), &
                                        FUEL_COST_PER_MWH(1,CN), &
                                        VAR_OM_PER_MWH(CN), &
                                        EMISSIONS_PER_MWH(CN), &
                                        MONTHLY_CAP_MULT(1,CN), &
                                        MIN_UP_TIME(CN), &
                                        MIN_DOWN_TIME(CN), &
                                        START_UP_COSTS(1,CN), &
                                        MUST_RUN_UNIT(1,CN), &
                                        co_prod_pointer, &
                                        TEMP_I2, & !  DUMMY FOR MONTH
                                      LOCAL_CHRONO_MARKET_PRICES(1,L), &
                                        LOCAL_CHRONO_MW_USAGE, &
                                        LOCAL_CHRONO_STRIKE_PRICE, &
                                        MARKET_HOURS, &
                                        ANNUAL_STRIKES, &
                                        ANNUAL_ENERGY, &
                                        ANNUAL_ENERGY_REVENUE(CN), &
                                        ANNUAL_VARIABLE_COST(CN), &
                                        ANNUAL_FUEL_COST(CN), &
                                        ANNUAL_VOM_COST, &
                                        ANNUAL_EMIS_COST)
         call write_trace_real4(file_trace_amoc, "19. AE", ANNUAL_ENERGY)
                     TEMP_R4 = ANNUAL_VARIABLE_COST(CN) - &
                                        ANNUAL_FUEL_COST(CN) - &
                                        ANNUAL_VOM_COST - &
                                        ANNUAL_EMIS_COST

                  ENDIF ! TYPE OF AI RESOURCE
                  IF(INDEPENDENT(CN) > 0) THEN
!
! CONSTRAINED CHARGE, UNCONSTRAINED DISCHARGE.
!
                     INDEPENDENT_ANNUAL_ENERGY = ANNUAL_ENERGY
                     INDEPENDENT_ANNUAL_REVENUE = &
                                               ANNUAL_ENERGY_REVENUE(CN)
                     INDEPENDENT_MW_USAGE = LOCAL_CHRONO_MW_USAGE
                     INDEPENDENT_FUEL_COST = ANNUAL_FUEL_COST(CN)
                     INDEPENDENT_VOM_COST = ANNUAL_VOM_COST
                     INDEPENDENT_EMIS_COST = ANNUAL_EMIS_COST
! TRIPLE INDEX.
                     HYBRID_OPT_POINTER = INDEPENDENT(CN)
                     HYBRID_PROD_POINTER = &
                           GET_GRX_RESOURCE_LINK_ID(HYBRID_OPT_POINTER)
                     HYBRID_PROD_POINTER = &
                               GET_TRANS_FOR_GRX_ID(HYBRID_PROD_POINTER)
                     LOCAL_HYBRID_MW_DISCHARGE = 999999.0
                     LOCAL_HYBRID_HR_MARGIN = 0.0
                     LOCAL_HYBRID_MW_CHARGE = LOCAL_CHRONO_MW_USAGE
                     cx_called_from="cap_objt:cx_002"
                     TEMP_L = CX_DailyOperAnPS2( &
                                        HYBRID_PROD_POINTER, & !  TRANS
                                      LOCAL_CHRONO_MARKET_PRICES(1,L), &
                                      !  8784, TG
                                        LOCAL_CHRONO_MW_USAGE, & ! 8784
                                        CONSTRAIN_ANNUAL_ENERGY, &
                                        CONSTRAIN_ANNUAL_CHARGE, &
                                        CONSTRAIN_ANNUAL_REVENUE, &
                                        CONSTRAIN_VOM_COST, &
                                        LOCAL_HYBRID_MW_CHARGE, &
                                        LOCAL_HYBRID_MW_DISCHARGE, &
                                        LOCAL_HYBRID_HR_MARGIN, &
                                        LOCAL_HYBRID_PROFIT)

                     CONSTRAIN_HYBRID_MARGIN = LOCAL_HYBRID_HR_MARGIN
                     CONSTRAIN_HYBRID_MW_USAGE = LOCAL_CHRONO_MW_USAGE
! 030720.
                     IF(RENEWABLE_ENERGY_PERCENT(HYBRID_OPT_POINTER) &
                                                          > 0.0001) THEN
                        HYBRID_MARKET_ENERGY_LIM = &
                           - CONSTRAIN_ANNUAL_CHARGE * &
                                 (1.0/RENEWABLE_ENERGY_PERCENT( &
                                              HYBRID_OPT_POINTER) - 1.0)
                     ELSE
                        HYBRID_MARKET_ENERGY_LIM = 999999.0
                     ENDIF
                     IF(HYBRID_MARKET_ENERGY_LIM > 0.0001) THEN

! UNCONSTRAINED CHARGE, UNCONSTRAINED DISCHARGE.

                        LOCAL_HYBRID_MW_DISCHARGE = 999999.0
                        LOCAL_HYBRID_HR_MARGIN = 0.0
                        LOCAL_HYBRID_MW_CHARGE = 999999.0
                     cx_called_from="cap_objt:cx_003"
                        TEMP_L = CX_DailyOperAnPS2( &
                                        HYBRID_PROD_POINTER, & !  TRANS
                                      LOCAL_CHRONO_MARKET_PRICES(1,L), &
                                      !  8784, TG
                                        LOCAL_CHRONO_MW_USAGE, & ! 8784
                                        HYBRID_MARKET_ANNUAL_ENERGY, &
                                        HYBRID_MARKET_ANNUAL_CHARGE, &
                                        HYBRID_MARKET_ANNUAL_REVENUE, &
                                        HYBRID_MARKET_VOM_COST, &
                                        LOCAL_HYBRID_MW_CHARGE, &
                                        LOCAL_HYBRID_MW_DISCHARGE, &
                                        LOCAL_HYBRID_HR_MARGIN, &
                                        LOCAL_HYBRID_PROFIT)
                        HYBRID_MARKET_MW_USAGE = &
                                                   LOCAL_CHRONO_MW_USAGE
!
! FIND MOST PROFITABLE STORAGE DISCHARGE ENERGY GIVEN RENEWABLE ENERGY
! PERCENT
                        LOCAL_HYBRID_HR_MARGIN = 0.0
                        ADD_HYBRID_MW_CHARGE = 999999.0

                        TEMP_L = ANNUAL_STORAGE_SORT( &
                                        HYBRID_PROD_POINTER, &
                                        HYBRID_MARKET_ENERGY_LIM, &
                                        HYBRID_MARKET_MW_USAGE, &
                                        CONSTRAIN_HYBRID_MW_USAGE, &
                                        CONSTRAIN_HYBRID_MARGIN, &
                                        LOCAL_CHRONO_MW_USAGE)
!
! UNCONSTRAINED CHARGE, CONSTRAINED DISCHARGE.
!
                        LOCAL_HYBRID_MW_DISCHARGE = &
                                                   LOCAL_CHRONO_MW_USAGE
                        LOCAL_HYBRID_MW_CHARGE = 999999.0
                     cx_called_from="cap_objt:cx_004"
                        TEMP_L = CX_DailyOperAnPS2( &
                                        HYBRID_PROD_POINTER, & !  TRANS
                                      LOCAL_CHRONO_MARKET_PRICES(1,L), &
                                      !  8784, TG
                                        LOCAL_CHRONO_MW_USAGE, & !  8784
                                        HYBRID_MARKET_ANNUAL_ENERGY, &
                                        HYBRID_MARKET_ANNUAL_CHARGE, &
                                        HYBRID_MARKET_ANNUAL_REVENUE, &
                                        HYBRID_MARKET_VOM_COST, &
                                        LOCAL_HYBRID_MW_CHARGE, &
                                        LOCAL_HYBRID_MW_DISCHARGE, &
                                        LOCAL_HYBRID_HR_MARGIN, &
                                        LOCAL_HYBRID_PROFIT)

                        MARKET_HYBRID_MW_USAGE = LOCAL_CHRONO_MW_USAGE
!
                        CONSTRAIN_HYBRID_MW_USAGE = &
                                       CONSTRAIN_HYBRID_MW_USAGE + &
                                                  MARKET_HYBRID_MW_USAGE
                     ELSE
                        HYBRID_MARKET_ANNUAL_ENERGY = 0.0
                        HYBRID_MARKET_ANNUAL_CHARGE = 0.0
                        HYBRID_MARKET_ANNUAL_REVENUE = 0.0
                        HYBRID_MARKET_VOM_COST = 0.0
                     ENDIF ! HYBRID MARKET ENERGY > 0
!
! COMBINED HOURLY ENERGY.
!
                     LOCAL_CHRONO_MW_USAGE = INDEPENDENT_MW_USAGE + &
                                               CONSTRAIN_HYBRID_MW_USAGE
!
                     ANNUAL_ENERGY = INDEPENDENT_ANNUAL_ENERGY + &
                               CONSTRAIN_ANNUAL_ENERGY + &
                                    CONSTRAIN_ANNUAL_CHARGE + &
                                         HYBRID_MARKET_ANNUAL_ENERGY
                    call write_trace_real4(file_trace_amoc, "4. AE", &
                        ANNUAL_ENERGY)
                        

!
                     ANNUAL_ENERGY_REVENUE(CN) = &
                                     INDEPENDENT_ANNUAL_REVENUE + &
                                       CONSTRAIN_ANNUAL_REVENUE - &
                                       CONSTRAIN_VOM_COST + &
                                       HYBRID_MARKET_ANNUAL_REVENUE
                     call write_trace_int2(file_trace_amoc, "CN", CN)
                     call write_trace_real4(file_trace_amoc, "3. AER", &
                     ANNUAL_ENERGY_REVENUE(CN))
                     ANNUAL_VOM_COST = INDEPENDENT_VOM_COST + &
                                                  HYBRID_MARKET_VOM_COST
                     ANNUAL_VARIABLE_COST(CN) = ANNUAL_VOM_COST + &
                                                   INDEPENDENT_EMIS_COST
                     VOM_COST_MM(CN) = ANNUAL_VOM_COST * 0.000001
                     IF(ANNUAL_ENERGY > 0.000001) THEN
                        VAR_OM_PER_MWH(CN) = &
                                          ANNUAL_VOM_COST/ ANNUAL_ENERGY
                     ENDIF

                  ENDIF ! HYBRID RESOURCE
               ENDIF ! AI ACTIVE

               IF(SCREEN_CAPACITY(CN) > 0.0) THEN

                  CF = ANNUAL_ENERGY /(SCREEN_CAPACITY(CN)*87.60)

               ELSE
                  CF = 0.0
               ENDIF
               IF( ANNUAL_ENERGY < -.0001) THEN

                  WRITE(4,*) 'ANNUAL ENERGY WHEN NEGATIVE'
                  er_message='Stop requested from CAP_OBJT SIID5'
                  call end_program(er_message)
               END IF
               IF(TG_MW_TRANSFERS) THEN
                  CM_CAPACITY_MW(CM) = &
                           CM_PLANNING_CAPACITY(CM) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(L)  + &
                                    CM_Net_Transfer(CM)
               ELSE
                  CM_CAPACITY_MW(CM) = &
                           CM_PLANNING_CAPACITY(CM) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(L)
               ENDIF
                 call write_trace_real4(file_trace_mxa, "19. CM_CAP_MW", &
                    CM_CAPACITY_MW(CM))
               IF(CM_PEAK_MW(CM) > .01) THEN
                  CM_RESERVE_MARGIN(CM) = 100. * &
                              (CM_CAPACITY_MW(CM)/CM_PEAK_MW(CM) - 1.0)
                  CM_INTERUPTIBLRESERVE_MARGIN = 100. * &
                         (CM_CAPACITY_MW(CM) - &
                                    CM_PEAK_AFTER_INTERRUPTIBLE(CM)) / &
                                CM_PEAK_AFTER_INTERRUPTIBLE(CM)
               ELSE
                  CM_RESERVE_MARGIN(CM) = 100.
               ENDIF
               TEMP_R4 = CM_RESERVE_MARGIN(CM)*0.01 + 1.0
               

               ICAP_DEMAND_CURVE_MULT_K = &
                          GET_ICAP_REVENUE_MULT(R_YEAR,CM,TEMP_R4)

! 031019. ADD AVAILABLE RPS_REVENUE AT THIS POINT TO GIVE RPS RESOURCE
!         FULL OPPORTUNITY TO RECEIVE VALUE OF REVENUE. SHOULD BE
!         AUTOMATICALLY REDUCED BELOW IF CAPACITY REVENUES ARE AVAILABLE

               TEMP_I2 = RPS_PROGRAM_NUMBER_loc(CN)
              call write_trace_int2(file_trace_amoc, "temp_i2", temp_i2)
               IF(ABS(TEMP_I2) > 0 .AND. CX_ITER /= 0) THEN
                  IF(R_YEAR == 7 .AND. GRX_ITERATIONS > 0) THEN
                     R_YEAR = R_YEAR
                  ENDIF
                  TEMP_R4 = ANNUAL_ENERGY * RPS_PERCENT(CN)
                  RPS_REVENUE_MM(AI) = &
                              GET_RPS_REV_BY_OPTION( &
                                                 TEMP_I2,TEMP_R4,R_YEAR)
               ELSE
                  RPS_REVENUE_MM(AI) = 0.0
               ENDIF
               MARKET_REVENUE(CN) = ANNUAL_ENERGY_REVENUE(CN) + &
                                            1000000.0*RPS_REVENUE_MM(AI)
            call write_trace_real4(file_trace_amoc, "1. MARKET_REVENUE", &
                MARKET_REVENUE(CN))
        call write_trace_real4(file_trace_amoc, "ANNUAL_ENERGY_REVENUE", &
            ANNUAL_ENERGY_REVENUE(CN))
        call write_trace_int2(file_trace_amoc, "AI", AI)
        
        call write_trace_real4(file_trace_amoc, "RPS_REVENUE_MM(AI)", &
            RPS_REVENUE_MM(AI))
            
               MARKET_COST(CN) = ANNUAL_FIXED_COST(CN) + &
                                               ANNUAL_VARIABLE_COST(CN)
          call write_trace_real4(file_trace_amoc, "AE", ANNUAL_ENERGY)

               IF(ANNUAL_ENERGY > 0.0001) THEN
                  ENERGY_REVENUE_PER_MWH(CN) = &
                                       ANNUAL_ENERGY_REVENUE(CN)/ &
                                                         ANNUAL_ENERGY

                  marcount=marcount+1
                 call write_trace_int4(file_trace_amoc, "1. marcount: ", &
                    marcount)

                 
                  MARKET_AVE_REVENUE = MARKET_REVENUE(CN) / &
                         ANNUAL_ENERGY
                         
           call write_trace_int2(file_trace_amoc, "CN", CN)
           call write_trace_real4(file_trace_amoc, "market_revenue(CN)", &
            MARKET_REVENUE(CN))
           call write_trace_real4(file_trace_amoc, "ANNUAL_ENERGY", &
            ANNUAL_ENERGY)
            

                    call write_trace_real4(file_trace_amoc, "1. MAR", &
                    MARKET_AVE_REVENUE)
                  TOTAL_AVERAGE_COST = MARKET_COST(CN) / &
                                                         ANNUAL_ENERGY
                   call write_trace_real4(file_trace_amoc, "2. TAC", &
                        TOTAL_AVERAGE_COST)
                
            
               ELSE
                  ENERGY_REVENUE_PER_MWH(CN) = 0.0
                  MARKET_AVE_REVENUE = 0.0
                  TOTAL_AVERAGE_COST = 0.0
                  marcount=marcount+1
                 call write_trace_int4(file_trace_amoc, "2. marcount: ", &
                    marcount)
       call write_trace_real4(file_trace_amoc, "3. TAC", &
            TOTAL_AVERAGE_COST)
       call write_trace_real4(file_trace_amoc, "4. MAR", &
            MARKET_AVE_REVENUE)
               ENDIF
               call write_trace_real4(file_trace_amoc, "MAR", &
                MARKET_AVE_REVENUE)
               call write_trace_real4(file_trace_amoc, "TAC", &
                TOTAL_AVERAGE_COST)
                
               NET_MARGIN(CN) = MARKET_AVE_REVENUE - TOTAL_AVERAGE_COST
               call write_trace_int2(file_trace_amoc, "CN", CN)
               call write_trace_real4(file_trace_amoc, &
                "1. NM", NET_MARGIN(CN))
               HIGHEST_FUEL_COST_PER_MMBTU(AI) = &
                                     FUEL_COST_PER_MMBTU(CN)
               HIGHEST_VAR_OM_PER_MWH(AI) = &
                                     VAR_OM_PER_MWH(CN)
               HIGHEST_EMISSIONS_PER_MWH(AI) = &
                                       EMISSIONS_PER_MWH(CN)
!
               IF(SCREEN_CAPACITY(CN) > 0.) THEN
                  NET_MARGIN_PER_KWYR(CN) = &
                      .001*(MARKET_REVENUE(CN) - MARKET_COST(CN)) / &
                                                     SCREEN_CAPACITY(CN)
                  IF(CX_ITER == 0) THEN
                     CM_MAX_NET_ENRG_MARG_PER_KWYR(CM) = &
                        MAX(CM_MAX_NET_ENRG_MARG_PER_KWYR(CM), 0.001 * &
                       (ANNUAL_ENERGY_REVENUE(CN) - MARKET_COST(CN)) / &
                                                    SCREEN_CAPACITY(CN))
                  ENDIF
               ELSE
                  NET_MARGIN_PER_KWYR(CN) = -999999.0
               ENDIF

               VARIABLE_COST_MM(AI) = 0.000001 * &
                                               ANNUAL_VARIABLE_COST(CN)
               FIXED_COST_MM(AI) = 0.000001 * &
                                    ANNUAL_FIXED_COST(CN)
               HIGHEST_FOM_COST_MM(AI) = 0.000001 * FIX_OM_COST(CN)
               HIGHEST_ANN_LEVEL_CAP(AI) = ANNUAL_LEVELIZED_CAPITAL(CN)
               ENERGY_REVENUE_MM(AI) = 0.000001 * &
                                               ANNUAL_ENERGY_REVENUE(CN)
!
               HIGHEST_MARKET_REVENUE(AI) = ENERGY_REVENUE_MM(AI)
               HIGHEST_ENERGY_REV_PER_MWH(AI) = &
                                              ENERGY_REVENUE_PER_MWH(CN)
               HIGHEST_MRX_ENERGY(AI) = ANNUAL_ENERGY * 0.001
               HIGHEST_SCREEN_CAPACITY(AI) = SCREEN_CAPACITY(CN)
               HIGHEST_MRX_CF(AI) = CF
               HIGHEST_FUEL_COST(AI) = &
                              ANNUAL_FUEL_COST(CN)/1000000.0
               HIGHEST_VOM_COST_MM(AI) = 0.000001 * ANNUAL_VOM_COST
!
               HIGHEST_EMISSIONS_COST_MM(AI) = 0.000001 * &
                                                        ANNUAL_EMIS_COST
               HIGHEST_HEAT_RATE(AI) = &
                                       HEAT_RATE(CN)
               IF(ANNUAL_ENERGY > 0.01) THEN
                  HIGHEST_FOM_PER_MWH(AI) = FIX_OM_COST(CN)/ &
                                                  MAX(.01,ANNUAL_ENERGY)
                  HIGHEST_LEVEL_CAP_PER_MWH(AI) = &
                           ANNUAL_LEVELIZED_CAPITAL(CN) * 1000000.0 / &
                                                  MAX(.01,ANNUAL_ENERGY)
                 HIGHEST_FUEL_COST_PER_MWH(AI) = ANNUAL_FUEL_COST(CN)/ &
                                                  MAX(.01,ANNUAL_ENERGY)
                  FIXED_COST_PER_MWH(AI) = &
                              HIGHEST_LEVEL_CAP_PER_MWH(AI) + &
                                          HIGHEST_FOM_PER_MWH(AI)
                  REVENUE_PER_MWH(AI) = ANNUAL_ENERGY_REVENUE(CN)/ &
                                                  MAX(.01,ANNUAL_ENERGY)
               ELSE
                  HIGHEST_FUEL_COST_PER_MWH(AI) = AVERAGE_FUEL_PRICE(CN)
               ENDIF
               HIGHEST_STRIKE_PRICE(AI) = &
                        HIGHEST_FUEL_COST_PER_MWH(AI) + &
                              HIGHEST_VAR_OM_PER_MWH(AI) + &
                                     HIGHEST_EMISSIONS_PER_MWH(AI)
               HIGHEST_RPM(AI) = RPM(CN)
               HIGHEST_RFT(AI) = RFT(CN)
               HIGHEST_ANNUAL_UNITS(AI) =  FLOAT(LOCAL_ANNUAL_UNITS(CN))
               HIGHEST_MRX_MARGIN(AI) = NET_MARGIN(CN)
               HIGHEST_MARGIN_PER_KWYR(AI) = NET_MARGIN_PER_KWYR(CN)
!
               IF(CX_ITER /= 0) THEN
                  IF(PLANNING_PER_MWH) THEN
                     HGST_NET_MARG_ADDED(AI) = NET_MARGIN(CN)
         call write_trace_real4(file_trace_amoc, "3. HGMA", &
            HGST_NET_MARG_ADDED(AI))
                  ELSE
                     HGST_NET_MARG_ADDED(AI) = &
                                                 NET_MARGIN_PER_KWYR(CN)
         call write_trace_real4(file_trace_amoc, "4. HGMA", &
            HGST_NET_MARG_ADDED(AI))

                  ENDIF
               ENDIF

               HGST_OPT_INDEX(AI) = CN
          if(file_trace_camp==0) then 
         file_trace_camp=open_trace("cx_alter_mx_chrono.trace", rq_camp)
          end if
          
               IF(CX_ACTIVE .AND. CX_ITER > 0) THEN ! .AND.
            if(r_year==0) then
               call end_program("cap_objt:0047 - year must be nonzero.")
            end if
                  IF(TG == 0) THEN
                     TEMP_PEAK =  UPDATE_NET_PLANNING_PEAK(R_YEAR)
                  ELSE
                     TEMP_PEAK = TG_PEAK_MW(L) 
                  ENDIF
                  if(file_trace_camp/=BAD_TRACE_HANDLE) then
                      write(file_trace_camp,*) &
      "CX_ALTER_MX_CHRONO_PRICES-> ", &
      "L ", L,  &
      "TG_PLANNING_CAPACITY ", TG_PLANNING_CAPACITY(L), &
      "LOCAL_CHRONO_MARKET_PRICES ", LOCAL_CHRONO_MARKET_PRICES(1,L), &
      "TEMP_PEAK ", TEMP_PEAK

                  end if
                  
                  TEMP_L = CX_ALTER_MX_CHRONO_PRICES( &
                                    TG_PLANNING_CAPACITY(L), &
                                    LOCAL_CHRONO_MW_USAGE, &
                                    LOCAL_CHRONO_STRIKE_PRICE, &
                                    LOCAL_CHRONO_MARKET_PRICES(1,L), &
                                    TEMP_PEAK)
                  if(file_trace_camp/=BAD_TRACE_HANDLE) then
                      write(file_trace_camp,*) &
      "<- CX_ALTER_MX_CHRONO_PRICES ", &
      "L ", L,  &
      "TG_PLANNING_CAPACITY ", TG_PLANNING_CAPACITY(L), &
      "LOCAL_CHRONO_MARKET_PRICES ", LOCAL_CHRONO_MARKET_PRICES(1,L), &
      "TEMP_PEAK ", TEMP_PEAK

                  end if
               ELSE ! PROPAGATE SOLUTION TO OTHER INSTANCES OF SAME CN
       call write_trace_int2s(file_trace_amoc, "ACTIVE_INSTANCE", &
        ACTIVE_INSTANCE(:TOTAL_ACTIVE_INSTANCES))
                  DO J = 1, TOTAL_ACTIVE_INSTANCES
                     K = ACTIVE_INSTANCE(J)
                     IF(K == AO) THEN
                        IF(PLANNING_PER_MWH) THEN

                           HGST_NET_MARG_ADDED(J) = NET_MARGIN(CN)
         call write_trace_real4(file_trace_amoc, "5. HGMA", &
            HGST_NET_MARG_ADDED(CN))

                        ELSE
                           HGST_NET_MARG_ADDED(J) = &
                                                 NET_MARGIN_PER_KWYR(CN)
         call write_trace_real4(file_trace_amoc, "6. HGMA", &
            HGST_NET_MARG_ADDED(J))

                        ENDIF
                     ENDIF
                  ENDDO
               ENDIF
            ENDDO ! OPTIONS/INSTANCES
         call write_trace_int2s(file_trace_amoc,"6. SORTED_OPTIONS", &
      sorted_options(:TOT_ALL_OPTIONS_cap)) 
      do last_hgst=1, ub2
        if (HGST_NET_MARG_ADDED(last_hgst)==-999999.0) then
            exit
        end if
      end do
         call write_trace_real4s(file_trace_amoc, "9. HGST_NET_MARG_ADDED", &
            HGST_NET_MARG_ADDED(:last_hgst))
            call write_trace_int2(file_trace_amoc, "3. TAO", &
                TOTAL_ACTIVE_INSTANCES)
        call write_sic_trace("cap_objt:0070a", TOTAL_ACTIVE_INSTANCES)
         call write_trace_int2s(file_trace_amoc,"7. SORTED_OPTIONS", &
      sorted_options(:TOTAL_ACTIVE_INSTANCES)) 
      ub2=ubound(HGST_NET_MARG_ADDED,1)
      do last_hgst=1, ub2
        if (HGST_NET_MARG_ADDED(last_hgst)==-999999.0) then
            exit
        end if
      end do

       call write_trace_real4s(file_trace_amoc, "11. HGST_NET_MARG_ADDED", &
            HGST_NET_MARG_ADDED(:last_hgst))
      
            CALL SortIncrPos(TOTAL_ACTIVE_INSTANCES, &
                             SORTED_OPTIONS, &
                             HGST_NET_MARG_ADDED)
      do last_hgst=1, ub2
        if (HGST_NET_MARG_ADDED(last_hgst)==-999999.0) then
            exit
        end if
      end do

       call write_trace_real4s(file_trace_amoc, "10. HGST_NET_MARG_ADDED", &
            HGST_NET_MARG_ADDED(:last_hgst))
      amoc2count=amoc2count+1
      call write_trace_int4(file_trace_amoc, "5. amoc2count", amoc2count)
         call write_trace_int2s(file_trace_amoc,"5. SORTED_OPTIONS", &
      sorted_options(:TOTAL_ACTIVE_INSTANCES)) 


            IF(CX_ITER > 0) THEN
               OPTIMAL_ORDER = .TRUE.
               DO J = 1, TOTAL_ACTIVE_INSTANCES-1
                  IF(SORTED_OPTIONS(J) > SORTED_OPTIONS(J+1)) THEN
                     call write_trace_message(file_trace_amoc, "1. CYCLE")
                     CYCLE
                  ELSE
                     OPTIMAL_ORDER = .FALSE.
                     EXIT
                  ENDIF
               ENDDO
! RESTORE SORTED_OPTIONS BACK TO POSITION AND INSTANCE
               TEMP_INSTANCE = SORTED_OPTIONS
               call write_trace_int2(file_trace_amoc, "X. TAO", &
                TOTAL_ACTIVE_INSTANCES)
               DO AI = TOTAL_ACTIVE_INSTANCES, 1, -1
                  call write_trace_int2(file_trace_amoc, "AI", AI)
                  write_loop_count=write_loop_count+1
                  
                  M = TEMP_INSTANCE(AI)
                  K = TOTAL_ACTIVE_INSTANCES - M + 1
                  J = LAST_SORT_INSTANCE(K)
                  SORTED_OPTIONS(AI) = J
        
                  AO = ACTIVE_INSTANCE(J)
                  CN = ACTIVE_OPTION(AO)
                  HGST_OPT_INDEX(M) = CN
                  
         if (file_trace_amoc/=BAD_TRACE_HANDLE) then
            write(file_trace_amoc, *) &
                  "1. M ", M, "K ", K, "J ", J, "AO ", AO, "CN ", CN
         end if
         if (file_trace_mxa/=BAD_TRACE_HANDLE) then
            write(file_trace_mxa, *) &
                  "2. M ", M, "K ", K, "J ", J, "AO ", AO, "CN ", CN
         end if

                  TG = ns_cap_trans%transaction_group(CN)
                  call write_trace_int2(file_trace_amoc, "4. TG", TG)
                  L = GET_TRANS_GROUP_POSITION(TG)
                  call write_trace_int2(file_trace_mxa, "4. TG", TG)
                  call write_trace_int2(file_trace_mxa, "L", L)
                  CM = GET_CM_FROM_TG(L)
                  call write_trace_int2(file_trace_mxa, "CM", CM)
                  CM_INDEX = GET_CM_INDEX_FROM_TG(CM)
                  PA = TG_2_PLANNING_AREA(L)
!
                  IF(MX_SUMMARY_REPORT .AND. &
                           (CX_ITER == MAX_CX_ITER - 1 .OR. &
                                                   OPTIMAL_ORDER) ) THEN
!
                     IF(TG_MW_TRANSFERS) THEN
                        TG_CAPACITY_MW(L) = &
                           TG_PLANNING_CAPACITY(L) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(L) + &
                                    TG_Net_Transfer(L)
                     ELSE
                        TG_CAPACITY_MW(L) = &
                           TG_PLANNING_CAPACITY(L) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(L)! +
                     ENDIF
                     IF(TG_PEAK_MW(L) > .01) THEN
                        TG_RESERVE_MARGIN(L) = 100. * &
                              (TG_CAPACITY_MW(L)/TG_PEAK_MW(L) - 1.0)
                     ELSE
                        TG_RESERVE_MARGIN(L) = 100.
                     ENDIF
                     IF(TG_MW_TRANSFERS) THEN
                        CM_CAPACITY_MW(CM) = &
                           CM_PLANNING_CAPACITY(CM) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(L)  + &
                                    CM_Net_Transfer(CM)
                     ELSE
                        CM_CAPACITY_MW(CM) = &
                           CM_PLANNING_CAPACITY(CM) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(L) ! +
                     ENDIF
                 call write_trace_real4(file_trace_mxa, "18. CM_CAP_MW", &
                    CM_CAPACITY_MW(CM))
                     IF(CM_PEAK_MW(CM) > .01) THEN
                        CM_RESERVE_MARGIN(CM) = 100. * &
                              (CM_CAPACITY_MW(CM)/CM_PEAK_MW(CM) - 1.0)

                        CM_INTERUPTIBLRESERVE_MARGIN = 100. * &
                         (CM_CAPACITY_MW(CM) - &
                                    CM_PEAK_AFTER_INTERRUPTIBLE(CM)) / &
                                CM_PEAK_AFTER_INTERRUPTIBLE(CM)
                     ELSE
                        CM_RESERVE_MARGIN(CM) = 100.
                     ENDIF
                     TEMP_R4 = CM_RESERVE_MARGIN(CM)*0.01 + 1.0
                     TEMP_R4 = CM_INTERUPTIBLRESERVE_MARGIN*0.01 + 1.0
                     
                       if(R_YEAR==2 .and. CM==1) then
                            R_YEAR=R_YEAR ! Debugstop
                       end if
                     ICAP_DEMAND_CURVE_MULT(M) = &
                       GET_ICAP_REVENUE_MULT(R_YEAR,CM,TEMP_R4)
!
                     IF(SCREEN_CAPACITY(CN) > 0.001) THEN
!
                        IF(ICAP_SWITCH_INDEX(CM) == 2) THEN
                           CONE_COST_PER_KWYR(M) = &
                                    LOWEST_FIXED_COST_BY_CM(CM)
                        ELSE
                           CONE_COST_PER_KWYR(M) = ICAP_CONE_PRICE(CM)
                        ENDIF
                        CONE_DEMAND_CURVE_ADJ_PER_KWYR(M) = &
                                    CONE_COST_PER_KWYR(M) * &
                                    ICAP_DEMAND_CURVE_MULT(M)

                        IF(ICAP_SWITCH_INDEX(CM) == 2 .OR.   & !MARGINAL
                                        ICAP_SWITCH_INDEX(CM) == 3) THEN
                           ! CONE-MRX
                           CAPACITY_REVENUE_MM(M) = &
                              MIN(CONE_DEMAND_CURVE_ADJ_PER_KWYR(M) * &
                                  CAPACITY_PLANNING_MW(CN)*0.001, &
                                MAX(0., &
                                 (VARIABLE_COST_MM(M) + &
                                  FIXED_COST_MM(M) - &
                                       RPS_REVENUE_MM(M) - &
                                               ENERGY_REVENUE_MM(M))))
                           ICAP_REVENUE_PER_KWYR(M) = &
                              MAX(0.0,CAPACITY_REVENUE_MM(M)*1000.0/ &
                                                    SCREEN_CAPACITY(CN))
        call write_trace_int2(file_trace_amoc, "M", M)
        call write_trace_real4(file_trace_amoc, &
            "1. ICAP_REVENUE_PER_KWYR", &
            ICAP_REVENUE_PER_KWYR(M))
                        ELSE
                           ICAP_REVENUE_PER_KWYR(M) = &
                                 MIN(ICAP_CONE_PRICE(CM), &
                                     CONE_DEMAND_CURVE_ADJ_PER_KWYR(M))
                           CAPACITY_REVENUE_MM(M) = &
                              ICAP_REVENUE_PER_KWYR(M) * &
                                 CAPACITY_PLANNING_MW(CN) * 0.001
        call write_trace_int2(file_trace_amoc, "M", M)
        call write_trace_real4(file_trace_amoc, &
            "2. ICAP_REVENUE_PER_KWYR", &
            ICAP_REVENUE_PER_KWYR(M))
                        ENDIF
                        EAS_PROFIT_PER_KWYR(M) = &
                                          HIGHEST_EAS_PROFIT_PER_KWYR
                        EAS_PROFIT_PER_KWYR(M) = MAX(0.0, &
                                  CONE_DEMAND_CURVE_ADJ_PER_KWYR(M) - &
                                              ICAP_REVENUE_PER_KWYR(M))
                                              
                        HIGHEST_MARKET_REVENUE(M) = &
                                    HIGHEST_MARKET_REVENUE(M) + &
                                          RPS_REVENUE_MM(M) + &
                                                  CAPACITY_REVENUE_MM(M)
                        HIGHEST_MARGIN_PER_KWYR(M) = 1000.0 * &
                              (HIGHEST_MARKET_REVENUE(M) - &
                                 VARIABLE_COST_MM(M) - &
                                       FIXED_COST_MM(M))/ &
                                          SCREEN_CAPACITY(CN)
                     ELSE
                        EAS_PROFIT_PER_KWYR(M) = 0.0
                        CONE_COST_PER_KWYR(M) = 0.0
                        CONE_DEMAND_CURVE_ADJ_PER_KWYR(M) = 0.0
                        ICAP_REVENUE_PER_KWYR(M) = 0.0
        call write_trace_int2(file_trace_amoc, "M", M)
        call write_trace_real4(file_trace_amoc, &
            "4. ICAP_REVENUE_PER_KWYR", &
            ICAP_REVENUE_PER_KWYR(M))
                        CAPACITY_REVENUE_MM(M) = 0.0
                        CAPACITY_REVENUE_PER_MWH(M) = 0.0
                        HIGHEST_MARGIN_PER_KWYR(M) = 0.0
                     ENDIF
                     TEMP_R = HIGHEST_MRX_ENERGY(M) * &
                                       RPS_PERCENT(CN)
                call write_trace_real(file_trace_amoc, "11. TEMP_R", &
                        TEMP_R)
                     IF(TEMP_R > 0.0) THEN
                        RPS_REVENUE_PER_MWH(M) = &
                              1000.0 * RPS_REVENUE_MM(M) / TEMP_R
                     ELSE
                        RPS_REVENUE_PER_MWH(M) = 0.0
                     ENDIF
                     IF(HIGHEST_MRX_ENERGY(M) > 0.01) THEN
                        CAPACITY_REVENUE_PER_MWH(M) = &
                            CAPACITY_REVENUE_MM(M)*1000.0/ &
                                          HIGHEST_MRX_ENERGY(M)
                        HIGHEST_MRX_MARGIN(M) = 1000.0 * &
                              (HIGHEST_MARKET_REVENUE(M) - &
                                 VARIABLE_COST_MM(M) - &
                                       FIXED_COST_MM(M))/ &
                                          HIGHEST_MRX_ENERGY(M)
                        REVENUE_PER_MWH(M) = 1000.0 * &
                                   HIGHEST_MARKET_REVENUE(M)/ &
                                          HIGHEST_MRX_ENERGY(M)
                     ELSE
                        CAPACITY_REVENUE_PER_MWH(M) = 0.0
                        HIGHEST_MRX_MARGIN(M) = -999999.
                        REVENUE_PER_MWH(M) = 0.0
                     ENDIF
! 081316. END INSERT.
                     LOCAL_ANNUAL_UNITS(CN) = LOCAL_ANNUAL_UNITS(CN) - 1
                     HIGHEST_ANNUAL_UNITS(M) = &
                               FLOAT(LOCAL_ANNUAL_UNITS(CN))
                     MRX_ICAP_INDEX(M) = 0

              call write_trace_string(file_trace_amoc, &
                "1. OPTION_NAME ", OPTION_NAME(CN)(1:17))
                
                     WRITE(LOCAL_NAME,1010) OPTION_NAME(CN)(1:17),AI
                     MX_ANNUAL_ALT_REC = RPTREC(MX_ANNUAL_ALT_NO)
! Removed code that if statement guarantees won't execute. 
! (9 is never 10) :-)

              call write_trace_string(file_trace_amoc, &
                "1. LOCAL_NAME", LOCAL_NAME)
             call write_trace_int2(file_trace_mxa, "CN", CN)
             
             call write_trace_int4(file_trace_mxa, "write loop count", &
                write_loop_count)
             
             call write_trace_message(file_trace_mxa, "1. WRITE: ")
             call write_trace_int2(file_trace_mxa, "L", L)
                if (file_trace_mxa/=BAD_TRACE_HANDLE) then
                    write(file_trace_mxa,*) &
        MX_ANNUAL_ALT_REC, &
        "THIS_YEAR ", THIS_YEAR, &
        "MULTI_AREA_NAME ", MULTI_AREA_NAME(L), &
        "LOCAL_NAME ", LOCAL_NAME, &
        "HIGHEST_STRIKE_PRICE ", HIGHEST_STRIKE_PRICE(M), &
        "HIGHEST_ENERGY_REV_PER_MWH ", HIGHEST_ENERGY_REV_PER_MWH(M), & 
        "HIGHEST_MRX_MARGIN ", HIGHEST_MRX_MARGIN(M), &
        "HIGHEST_MRX_CF ", HIGHEST_MRX_CF(M), &
        "ENERGY_REVENUE_MM ", ENERGY_REVENUE_MM(M), &
        "HIGHEST_MARKET_COST ", HIGHEST_MARKET_COST(M), & !  5
        "VARIABLE_COST_MM ", VARIABLE_COST_MM(M), &
        "FIXED_COST_MM ", FIXED_COST_MM(M), &
        "SCREEN_CAPACITY ", SCREEN_CAPACITY(CN), &
        "HIGHEST_MRX_ENERGY ", HIGHEST_MRX_ENERGY(M), & !  10
        "TOTAL_AVERAGE_COST ", TOTAL_AVERAGE_COST, & 
        "HIGHEST_ANN_LEVEL_CAP ", HIGHEST_ANN_LEVEL_CAP(M), &
        "HIGHEST_FUEL_COST ", HIGHEST_FUEL_COST(M), &
        "HIGHEST_ANNUAL_UNITS ", HIGHEST_ANNUAL_UNITS(M), &
        "CAPACITY_REVENUE_MM ", CAPACITY_REVENUE_MM(M), &
        "HIGHEST_MARKET_REVENUE ", HIGHEST_MARKET_REVENUE(M), & 
        "HIGHEST_MARGIN_PER_KWYR ", HIGHEST_MARGIN_PER_KWYR(M), &
        "HIGHEST_FUEL_COST_PER_MWH ", HIGHEST_FUEL_COST_PER_MWH(M), &
        "HIGHEST_VAR_OM_PER_MWH ", HIGHEST_VAR_OM_PER_MWH(M), & !  19
       "HIGHEST_FUEL_COST_PER_MMBTU ", HIGHEST_FUEL_COST_PER_MMBTU(M), &
        "REVENUE_PER_MWH ", REVENUE_PER_MWH(M), &
        "CAPACITY_REVENUE_PER_MWH ", CAPACITY_REVENUE_PER_MWH(M), &
        "HIGHEST_EMISSIONS_PER_MWH ", HIGHEST_EMISSIONS_PER_MWH(M), &
        "HIGHEST_FOM_PER_MWH ", HIGHEST_FOM_PER_MWH(M), & !  24
        "HIGHEST_LEVEL_CAP_PER_MWH ", HIGHEST_LEVEL_CAP_PER_MWH(M), &
        "HIGHEST_VOM_COST_MM ", HIGHEST_VOM_COST_MM(M), &
        "HIGHEST_EMISSIONS_COST_MM ", HIGHEST_EMISSIONS_COST_MM(M), & 
        "HIGHEST_FOM_COST_MM ", HIGHEST_FOM_COST_MM(M), &
        "TG ", FLOAT(TG), &
        "TG_CAPACITY_MW ", TG_CAPACITY_MW(L), &
        "TG_PEAK_MW ", TG_PEAK_MW(L), &
        "TG_RESERVE_MARGIN ", TG_RESERVE_MARGIN(L), & !  32
        "CM_INDEX ", FLOAT(CM_INDEX), &
        "CM_CAPACITY_MW ", CM_CAPACITY_MW(CM), &
        "CM_PEAK_MW ", CM_PEAK_MW(CM), &
        "CM_RESERVE_MARGIN ", CM_RESERVE_MARGIN(CM), &
        "ICAP_DEMAND_CURVE_MULT ", ICAP_DEMAND_CURVE_MULT(M), &
        "CONE_COST_PER_KWYR ", CONE_COST_PER_KWYR(M), & !  38
        "CONE_DEMAND_CURVE_ADJ_PER_KWYR ", &
        CONE_DEMAND_CURVE_ADJ_PER_KWYR(M), &
        "EAS_PROFIT_PER_KWYR ", EAS_PROFIT_PER_KWYR(M), &
        "ICAP_REVENUE_PER_KWYR ", ICAP_REVENUE_PER_KWYR(M) , &
        "HIGHEST_HEAT_RATE ", HIGHEST_HEAT_RATE(M), & !  42
        "FIXED_COST_PER_MWH ", FIXED_COST_PER_MWH(M), &
        "HIGHEST_RFT ", HIGHEST_RFT(M), &
        "HIGHEST_RPM ", HIGHEST_RPM(M), &
        "TG_Net_Transfer ", TG_Net_Transfer(L), &
        "CM_Net_Transfer ", CM_Net_Transfer(CM), &
        "MRX_ICAP_INDEX ", MRX_ICAP_INDEX(M), &
        "CM_INTERUPTIBLRESERVE_MARGIN ", CM_INTERUPTIBLRESERVE_MARGIN, &
        "CM_PEAK_AFTER_INTERRUPTIBLE ", CM_PEAK_AFTER_INTERRUPTIBLE(CM), &
        "CAPACITY_PLANNING_MW ", CAPACITY_PLANNING_MW(CN), &
        "RPS_REVENUE_MM ", RPS_REVENUE_MM(M), &
        "RPS_REVENUE_PER_MWH ", RPS_REVENUE_PER_MWH(M), " "
                end if
                ! TODO: Combined writes to one in separate routine
                     WRITE(MX_ANNUAL_ALT_NO,REC=MX_ANNUAL_ALT_REC) &
                        PRT_ENDPOINT(), &
                        FLOAT(THIS_YEAR), &
                        MULTI_AREA_NAME(L), &
                        LOCAL_NAME, &
                        HIGHEST_STRIKE_PRICE(M), &
                        HIGHEST_ENERGY_REV_PER_MWH(M), & 
                        !  111509. REDEFINED.
                        HIGHEST_MRX_MARGIN(M), &
                        HIGHEST_MRX_CF(M), &
                        ENERGY_REVENUE_MM(M), &
                        HIGHEST_MARKET_COST(M), & !  5
                        VARIABLE_COST_MM(M), &
                        FIXED_COST_MM(M), &
                        SCREEN_CAPACITY(CN), &
                        HIGHEST_MRX_ENERGY(M), & !  10
                        TOTAL_AVERAGE_COST, & !  NEED DEFINITION
                        HIGHEST_ANN_LEVEL_CAP(M), &
                        HIGHEST_FUEL_COST(M), &
                        HIGHEST_ANNUAL_UNITS(M), &
                        CAPACITY_REVENUE_MM(M), &
                        HIGHEST_MARKET_REVENUE(M), & !  16 TOTAL REVENUE
                        HIGHEST_MARGIN_PER_KWYR(M), &
                        HIGHEST_FUEL_COST_PER_MWH(M), &
                        HIGHEST_VAR_OM_PER_MWH(M), & !  19
                        HIGHEST_FUEL_COST_PER_MMBTU(M), &
                        REVENUE_PER_MWH(M), &
                        CAPACITY_REVENUE_PER_MWH(M), &
                        HIGHEST_EMISSIONS_PER_MWH(M), &
                        HIGHEST_FOM_PER_MWH(M), & !  24
                        HIGHEST_LEVEL_CAP_PER_MWH(M), &
                        HIGHEST_VOM_COST_MM(M), &
                        HIGHEST_EMISSIONS_COST_MM(M), & !  27
                        HIGHEST_FOM_COST_MM(M), &
                        FLOAT(TG), &
                        TG_CAPACITY_MW(L), &
                        TG_PEAK_MW(L), &
                        TG_RESERVE_MARGIN(L), & !  32
                        FLOAT(CM_INDEX), &
                        CM_CAPACITY_MW(CM), &
                        CM_PEAK_MW(CM), &
                        CM_RESERVE_MARGIN(CM), &
                        ICAP_DEMAND_CURVE_MULT(M), &
                        CONE_COST_PER_KWYR(M), & !  38
                        CONE_DEMAND_CURVE_ADJ_PER_KWYR(M), &
                        EAS_PROFIT_PER_KWYR(M), &
                        ICAP_REVENUE_PER_KWYR(M) , &
                        HIGHEST_HEAT_RATE(M), & !  42
                        FIXED_COST_PER_MWH(M), &
                        HIGHEST_RFT(M), &
                        HIGHEST_RPM(M), &
                        TG_Net_Transfer(L), &
                        CM_Net_Transfer(CM), &
                        MRX_ICAP_INDEX(M), &
                        CM_INTERUPTIBLRESERVE_MARGIN, &
                        CM_PEAK_AFTER_INTERRUPTIBLE(CM), &
                        CAPACITY_PLANNING_MW(CN), &
                        RPS_REVENUE_MM(M), &
                        RPS_REVENUE_PER_MWH(M)
                     MX_ANNUAL_ALT_REC = MX_ANNUAL_ALT_REC + 1
! 081316.
                     TOTAL_ANNUAL_CAPACITY_ADDED(PA) = &
                           TOTAL_ANNUAL_CAPACITY_ADDED(PA) + &
                                    CAPACITY_PLANNING_MW(CN)
                     CM_TOT_ANN_CAPACITY_ADDED(CM) = &
                           CM_TOT_ANN_CAPACITY_ADDED(CM) + &
                                    CAPACITY_PLANNING_MW(CN)
                     TG_TOTAL_ANNUAL_CAPACITY_ADDED(L) = &
                           TG_TOTAL_ANNUAL_CAPACITY_ADDED(L) + &
                                    CAPACITY_PLANNING_MW(CN)
!
                  ENDIF ! IF REPORT AND OPTIMAL_ORDER
               ENDDO
            ENDIF
            LAST_SORT_INSTANCE = SORTED_OPTIONS
            LOCAL_CHRONO_MARKET_PRICES = RESET_CHRONO_MARKET_PRICES
!
! EVALUATE LIST. COMPARE WITH PREVIOUS ITERATION
!
            CX_ITER = CX_ITER + 1
         ENDDO ! ITERATE ON ACTIVE INSTANCE SOLUTION
         WRITE(9,*) 'CX_ITER = ',CX_ITER
         IF(.NOT. OPTIMAL_ORDER) THEN
            WRITE(9,*) 'NOT OPTIMAL ORDER IN ',THIS_YEAR,' AND ITER ', &
                                                          GRX_ITERATIONS
         ENDIF
!
 1234    CONTINUE
         HO = TOTAL_ACTIVE_INSTANCES
         IF(CX_ACTIVE) GOTO 5678
         DO L = 1, UPPER_TRANS_GROUP

            TG = GET_TRANS_GROUP_INDEX(L)
            call write_trace_int2(file_trace_amoc, "5. TG", TG)

            PA = TG_2_PLANNING_AREA(L) ! FOR ICAP REVENUE
            CM = TG_2_CAPACITY_MARKET(L) ! FOR ICAP REVENUE
            CM_INDEX = GET_CM_INDEX_FROM_TG(CM)
            IOTA = 0.0

            HIGHEST_OPTION = -1
       call write_trace_int2(file_trace_amoc, "1. HO", HIGHEST_OPTION)
!
! GET THE INITIAL MARKET DURATION CURVE. DO NOT CALL THIS ROUTINE AGAIN.
!
            IF(BUILD_TO_MARKET) THEN
               TRANS_GROUP_4_PRICING = L
               CALL MX_USER_PRICE_CURVE(      L, &
                                              HIGHEST_INTERVAL, &
                                              MARKET_DURATION, &
                                              MARKET_CUM_REVENUE, &
                                              MARKET_PRICE, &
                                              INT(13,2))
            ELSE
               TRANS_GROUP_4_PRICING = 0
               CALL MX_PRICE_CURVE( &
                                              HIGHEST_INTERVAL, &
                                              MARKET_DURATION, &
                                              MARKET_CUM_REVENUE, &
                                              MARKET_PRICE, &
                                              INT(13,2))
            ENDIF
!
            IF(CX_ACTIVE) THEN
               CALL CX_TRANS_ANNUAL_USER_MARKET( &
                                      L,LOCAL_CHRONO_MARKET_PRICES(1,L))

            ENDIF

            K = 1
            I = 1
!
            ANNUAL_RESOURCES_AVAILABLE = .TRUE.
!
            PA_CAPACITY_MW = 0.0
            CM_CAPACITY_MW = 0.0
!
! 9/10/02. REVERT BACK TO PREVIOUS LOGIC IF NOT REGIONAL. OTHERWISE,
!           ENSURE THAT RESOURCES ARE TESTED IN ALL REGIONS
!
            IF(USE_REGIONAL_MINIMUM_RM_LOGIC) THEN
               MAX_RESOURCES_PER_GROUP = &
                        MAX_RESOURCES_PER_GROUP + &
                                MAX_RESOURCES_PER_YEAR_GROUP
            ELSE
               MAX_RESOURCES_PER_GROUP = MX_RES_P_YEAR
            ENDIF
!
            FIRST_CF = -1.0
!
            WRITE(4,*) "MAX_RESOURCES_PER_GROUP",MAX_RESOURCES_PER_GROUP
            DO WHILE(HO < MAX_RESOURCES_PER_GROUP .AND. &
                           ANNUAL_RESOURCES_AVAILABLE .AND. &
                         (HIGHEST_NET_MARGIN > MINIMUM_NET_MARGIN .OR. &
                                             USE_MINIMUM_RM_LOGIC .OR. &
                                    USE_REGIONAL_MINIMUM_RM_LOGIC .OR. &
                                                                K == 1))
!
               IF(HO / MX_RES_P_YEAR > &
                                             L / UPPER_TRANS_GROUP) THEN
                  WRITE(4,*) "MAX RESOURCES HIT IN MRX."
                  WRITE(4,*) "RESOURCES TESTED K AND PASSED HO",K,HO
                  WRITE(4,*) "TRANSACTION GROUP ",MULTI_AREA_NAME(L)
                  EXIT
               ELSE
                  LAST_HO = HO
               ENDIF

               HIGHEST_CHRONO_MW_USAGE = 0.0
               ANNUAL_ENERGY = 0.
                    call write_trace_real4(file_trace_amoc, "5. AE", &
                        ANNUAL_ENERGY)

               HIGHEST_NET_MARGIN = -999999.
               HIGHEST_ICAP_REVENUE = -999999.
               HIGHEST_ENERGY_REVENUE = -999999.
               HIGHEST_NET_MARGIN_PER_MWH = -999999.
               HIGHEST_NET_MARGIN_PER_KWYR = -999999.
               HIGHEST_ICAP_DEMAND_CURVE_MULT = 0.0
               HIGHEST_EAS_PROFIT_PER_KWYR = 0.0
               HIGHEST_CONE_COST_PER_KWYR = 0.0
               HIGHEST_CONE_DEM_ADJ_PER_KWYR = 0.0
               HIGHEST_ICAP_REVENUE_PER_KWYR = 0.0
               HIGHEST_CM_CAPACITY_MW = 0.0
               HIGHEST_CM_RESERVE_MARGIN = 0.0

               LOCAL_RESOURCES_AVAILABLE = .FALSE.

               DO CN = 1, tot_all_options_cap

                  I = OPTION_POSITION(CN)
                  call write_trace_int2(file_trace_amoc, "16. I", I)
! 08/19/04. ADDED DEPENDENT CONDITION FOR PAC CORP DUCT FIRING.

                  IF( .NOT. RESOURCE_AVAILABLE(I, &
                                    THIS_YEAR_PLUS_LEAD_TIME(CN)) .OR. &
                      .NOT. TEST_START_STOP_YEARS(I, &
                                    THIS_YEAR_PLUS_LEAD_TIME(CN)) .OR. &
                                 ns_cap_trans%transaction_group(CN) /= TG  .OR. &
                                       LOCAL_ANNUAL_UNITS(CN) < 1 .OR. &
                               .NOT. DEPENDENT_UNIT_AVAILABLE(CN)) THEN
                      CYCLE
                   END IF
!
                  NET_MARGIN(CN) = -999999.
                  NET_MARGIN_PER_KWYR(CN) = -999999.
                  NET_MARGIN_INTERVAL(CN) = -1
!
                  DO J = HIGHEST_INTERVAL, 1, -1
                     IF(MARKET_PRICE(J) < STRIKE_PRICE(CN,MO)) then
                         EXIT
                     end if
                  ENDDO
!
                  LOCAL_RESOURCES_AVAILABLE = .TRUE.
                  IF(J < HIGHEST_INTERVAL) THEN
!
                     IF(J == 0) THEN
                        TOP_CAP_PERCENT = 0.0
                        J = 1
                     ELSEIF(MARKET_PRICE(J+1)  - &
                                 MARKET_PRICE(J) /= 0.0) THEN
                        TOP_CAP_PERCENT = &
                           (STRIKE_PRICE(CN,MO) - MARKET_PRICE(J)) / &
                                   (MARKET_PRICE(J+1) - MARKET_PRICE(J))
                     ELSE
                        TOP_CAP_PERCENT = 0.0
                     ENDIF

                     MARKET_HOURS = &
                        (TOP_CAP_PERCENT * MARKET_DURATION(J+1) + &
                        (1.-TOP_CAP_PERCENT) * MARKET_DURATION(J))

!
! ASSUMES 8784 HOURS PER YEAR.
!
                     IF(SCREEN_CAPACITY(CN) > 0.0 .AND. &
                                               FIRST_CF(CN) < -0.5) THEN
                        ANNUAL_ENERGY = MARKET_HOURS * &
                                                 EQUIVALENT_CAPACITY(CN)
                    call write_trace_real4(file_trace_amoc, "6. AE", &
                        ANNUAL_ENERGY)
                        FIRST_CF(CN) = ANNUAL_ENERGY / &
                                             (SCREEN_CAPACITY(CN)*87.84)
                     ENDIF
!
                     IF(GRX_UNIT_LINK_ACTIVE) THEN
                        U = GRX_RESOURCE_LINK_ID(CN)
                        IF(U > 0) THEN
                           U = GET_RESOURCE_ID_TO_UNIT(U,INT2_ONE)
                        ENDIF
                        IF(U > 0) THEN
                           IF(ANNUAL_CL_UNIT_CAPACITY(U) > 0.0001) THEN
!
                              ANNUAL_ENERGY = MARKET_HOURS * &
                                                 EQUIVALENT_CAPACITY(CN)
                    call write_trace_real4(file_trace_amoc, "7. AE", &
                        ANNUAL_ENERGY)
                              IF(MARKET_HOURS < -0.001) THEN
                                WRITE(4,*) 'MARKET HOURS WENT NEGATIVE'
                         er_message="Stop requested from CAP OBJTSIID6"
                                call end_program(er_message)
                              ENDIF
                              CF = ANNUAL_ENERGY / &
                                             (SCREEN_CAPACITY(CN)*87.84)
                              IF(FIRST_CF(CN) - CF > 0.001) THEN
                                 CF_FACTOR = MIN(1.0, &
                                  1.0 - (FIRST_CF(CN) - CF)*.01 )
                              ELSE
                                 CF_FACTOR = 1.0
                              ENDIF
!
                              TEMP_R = CF_FACTOR * &
                                 EQUIVALENT_CAPACITY(CN) / &
                                             ANNUAL_CL_UNIT_CAPACITY(U)
                call write_trace_real(file_trace_amoc, "12. TEMP_R", &
                        TEMP_R)
                              ANNUAL_ENERGY = &
                                 ANNUAL_CL_UNIT_ENERGY(U) * TEMP_R
                    call write_trace_real4(file_trace_amoc, "8. AE", &
                        ANNUAL_ENERGY)
                              ANNUAL_ENERGY_REVENUE = &
                                 ECO_SALES_REV_FROM(U) * TEMP_R
                                 
        call write_trace_real4s(file_trace_amoc, &
            "1. ANNUAL_ENERGY_REVENUE", &
            ANNUAL_ENERGY_REVENUE(:tot_all_options_cap))
               

            
           call write_trace_int2(file_trace_amoc, "U", U)
                call write_trace_real4(file_trace_amoc, "4. AER-init", &
                     ANNUAL_ENERGY_REVENUE(1))
                     
                              IF(ANNUAL_ENERGY == 0.) THEN
                                 ECO_SALES_REV_FROM(U) = 0.
                              ENDIF
                              ANNUAL_ENERGY_REVENUE = &
                                 ECO_SALES_REV_FROM(U) * TEMP_R
        call write_trace_real4s(file_trace_amoc, &
            "2. ANNUAL_ENERGY_REVENUE", &
            ANNUAL_ENERGY_REVENUE(:tot_all_options_cap))
           call write_trace_int2(file_trace_amoc, "U", U)
                call write_trace_real4(file_trace_amoc, "5. AER-init", &
                     ANNUAL_ENERGY_REVENUE(1))
                           ELSE
                              ANNUAL_ENERGY = MARKET_HOURS * &
                                                 EQUIVALENT_CAPACITY(CN)
                    call write_trace_real4(file_trace_amoc, "9. AE", &
                        ANNUAL_ENERGY)
                              ANNUAL_ENERGY_REVENUE = &
                                    (TOP_CAP_PERCENT * &
                                             MARKET_CUM_REVENUE(J+1) + &
                                    (1.-TOP_CAP_PERCENT) * &
                                              MARKET_CUM_REVENUE(J)) * &
                                               EQUIVALENT_CAPACITY(CN)
        call write_trace_real4s(file_trace_amoc, &
            "3. ANNUAL_ENERGY_REVENUE", &
            ANNUAL_ENERGY_REVENUE(:tot_all_options_cap))
           call write_trace_int2(file_trace_amoc, "CN", CN)
           call write_trace_int(file_trace_amoc, "J+1", J+1)
           call write_trace_real4(file_trace_amoc, "TOP_CAP_PERCENT", &
            TOP_CAP_PERCENT)
        call write_trace_real4(file_trace_amoc, "MARKET_CUM_REVENUE(J)", &
            MARKET_CUM_REVENUE(J))
      call write_trace_real4(file_trace_amoc, "MARKET_CUM_REVENUE(J+1)", &
            MARKET_CUM_REVENUE(J+1))
      call write_trace_real4(file_trace_amoc, "Equiv. Capacity", &
        EQUIVALENT_CAPACITY(CN))
        
           
           
                call write_trace_real4(file_trace_amoc, "6. AER", &
                     ANNUAL_ENERGY_REVENUE(1))
                           ENDIF
                        ELSE
                           ANNUAL_ENERGY = MARKET_HOURS * &
                                                 EQUIVALENT_CAPACITY(CN)
                    call write_trace_real4(file_trace_amoc, "10. AE", &
                        ANNUAL_ENERGY)
                           ANNUAL_ENERGY_REVENUE = &
                              (TOP_CAP_PERCENT * &
                                             MARKET_CUM_REVENUE(J+1) + &
                              (1.-TOP_CAP_PERCENT) * &
                                              MARKET_CUM_REVENUE(J)) * &
                                               EQUIVALENT_CAPACITY(CN)
        call write_trace_real4s(file_trace_amoc, &
            "4. ANNUAL_ENERGY_REVENUE", &
            ANNUAL_ENERGY_REVENUE(:tot_all_options_cap))
                        ENDIF
                     ELSE
                        ANNUAL_ENERGY = MARKET_HOURS * &
                                                 EQUIVALENT_CAPACITY(CN)
                    call write_trace_real4(file_trace_amoc, "11. AE", &
                        ANNUAL_ENERGY)
                        ANNUAL_ENERGY_REVENUE = &
                              (TOP_CAP_PERCENT * &
                                             MARKET_CUM_REVENUE(J+1) + &
                              (1.-TOP_CAP_PERCENT) * &
                                              MARKET_CUM_REVENUE(J)) * &
                                               EQUIVALENT_CAPACITY(CN)
        call write_trace_real4s(file_trace_amoc, &
            "5. ANNUAL_ENERGY_REVENUE", &
            ANNUAL_ENERGY_REVENUE(:tot_all_options_cap))
                     ENDIF
                     IF(SCREEN_CAPACITY(CN) > 0.0) THEN
                        CF = ANNUAL_ENERGY /(SCREEN_CAPACITY(CN)*87.84)
                     ELSE
                        CF = 0.0
                     ENDIF

                     IF(ANNUAL_ENERGY > 0.) THEN
                        ENERGY_REVENUE_PER_MWH(CN) = &
                                       ANNUAL_ENERGY_REVENUE(CN)/ &
                                                  MAX(.01,ANNUAL_ENERGY)
                     ELSE
                        ENERGY_REVENUE_PER_MWH(CN) = 0.
                     ENDIF
!
                     IF( ANNUAL_ENERGY < -.0001) THEN

                        WRITE(4,*) 'ANNUAL ENERGY WHEN NEGATIVE'
                        er_message='Stop requested from CAP_OBJT SIID7'
                        call end_program(er_message)
                     END IF
!
                     ANNUAL_VARIABLE_COST(CN) = STRIKE_PRICE(CN,MO) * &
                                                           ANNUAL_ENERGY
!
                     IF(TG_MW_TRANSFERS) THEN
                        CM_CAPACITY_MW(CM) = &
                           CM_PLANNING_CAPACITY(CM) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(L)  + &
                                    CM_Net_Transfer(CM)
                 call write_trace_real4(file_trace_mxa, "17. CM_CAP_MW", &
                    CM_CAPACITY_MW(CM))
                     ELSE
                        CM_CAPACITY_MW(CM) = &
                           CM_PLANNING_CAPACITY(CM) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(L)
                 call write_trace_real4(file_trace_mxa, "16. CM_CAP_MW", &
                    CM_CAPACITY_MW(CM))
                     ENDIF

                     IF(CM_PEAK_MW(CM) > .01) THEN
                        CM_RESERVE_MARGIN(CM) = 100. * &
                              (CM_CAPACITY_MW(CM)/CM_PEAK_MW(CM) - 1.0)
                        CM_INTERUPTIBLRESERVE_MARGIN = 100. * &
                         (CM_CAPACITY_MW(CM) - &
                                    CM_PEAK_AFTER_INTERRUPTIBLE(CM)) / &
                                CM_PEAK_AFTER_INTERRUPTIBLE(CM)
                     ELSE
                        CM_RESERVE_MARGIN(CM) = 100.
                     ENDIF
!
                     TEMP_R4 = CM_RESERVE_MARGIN(CM)*0.01 + 1.0
                     TEMP_R4 = CM_INTERUPTIBLRESERVE_MARGIN*0.01 + 1.0
                     
               if(R_YEAR==2 .and. CM==1) then
                    r_YEAR=r_YEAR ! Debugstop
               end if
                     ICAP_DEMAND_CURVE_MULT_K = &
                     GET_ICAP_REVENUE_MULT(R_YEAR,CM,TEMP_R4)
                     IF(SCREEN_CAPACITY(CN) > 0.01) THEN
                        EAS_PROFIT_PER_KWYR_K = 0.001 * &
                              (ANNUAL_ENERGY_REVENUE(CN) - &
                                  ANNUAL_VARIABLE_COST(CN))/ &
                                         SCREEN_CAPACITY(CN)
                        IF(EAS_PROFIT_PER_KWYR_K < -0.0001) THEN
                           EAS_PROFIT_PER_KWYR_K = EAS_PROFIT_PER_KWYR_K
                        ENDIF
                        CONE_COST_PER_KWYR_K = 0.001 * &
                                   ANNUAL_FIXED_COST(CN)/ &
                                         SCREEN_CAPACITY(CN)
                        CONE_DEM_CURVE_ADJ_PER_KWYR_K = &
                                    CONE_COST_PER_KWYR_K * &
                                  ICAP_DEMAND_CURVE_MULT_K
                        ICAP_REVENUE_PER_KWYR_K = &
                           MAX(0.0,CONE_DEM_CURVE_ADJ_PER_KWYR_K - &
                                                EAS_PROFIT_PER_KWYR_K)
                     ELSE
                        EAS_PROFIT_PER_KWYR_K = 0.0
                        CONE_COST_PER_KWYR_K = 0.0
                        CONE_DEM_CURVE_ADJ_PER_KWYR_K = 0.0
                        ICAP_REVENUE_PER_KWYR_K = 0.0
                     ENDIF
!
                     ANNUAL_ICAP_REVENUE = 1000.0 * &
                                 ICAP_REVENUE_PER_KWYR_K * &
                                                CAPACITY_PLANNING_MW(CN)
!
                     MARKET_REVENUE(CN) = ANNUAL_ENERGY_REVENUE(CN) ! +
!
                     IF(ANNUAL_ENERGY > 0.01) THEN
                        FOM_PER_MWH(CN) = FIX_OM_COST(CN)/ &
                                                  MAX(.01,ANNUAL_ENERGY)
                        LEVELIZED_CAP_PER_MWH(CN) = &
                           ANNUAL_LEVELIZED_CAPITAL(CN) * 1000000.0 / &
                                                  MAX(.01,ANNUAL_ENERGY)
                     ENDIF
                     VOM_COST_MM(CN) = VAR_OM_PER_MWH(CN) * &
                                            MAX(.01,ANNUAL_ENERGY) * &
                                                                0.000001

                     MARKET_COST(CN) = ANNUAL_FIXED_COST(CN) + &
                                               ANNUAL_VARIABLE_COST(CN)

                     ANNUAL_FUEL_COST(CN) = &
                             AVERAGE_FUEL_PRICE(CN) * ANNUAL_ENERGY
                     EMISSIONS_COST_MM(CN) = &
                          EMISSIONS_PER_MWH(CN) * ANNUAL_ENERGY*0.000001
                     MARKET_AVE_REVENUE = MARKET_REVENUE(CN) / &
                                                  MAX(.01,ANNUAL_ENERGY)
                 marcount=marcount+1
                 call write_trace_int4(file_trace_amoc, "3. marcount: ", &
                    marcount)
       call write_trace_real4(file_trace_amoc, "5. MAR", &
            MARKET_AVE_REVENUE)
                     TOTAL_AVERAGE_COST = MARKET_COST(CN) / &
                                                  MAX(.01,ANNUAL_ENERGY)
       call write_trace_real4(file_trace_amoc, "6. TAC", &
            TOTAL_AVERAGE_COST)
                     TEMP_NET_MARGIN = &
                                 MARKET_AVE_REVENUE - TOTAL_AVERAGE_COST
                     IF(EQUIVALENT_CAPACITY(CN) > 0.) THEN
                        TEMP_NET_MARGIN_PER_KWYR = &
                         .001*(MARKET_REVENUE(CN) - MARKET_COST(CN)) / &
                                                 EQUIVALENT_CAPACITY(CN)
                     ELSE
                        TEMP_NET_MARGIN_PER_KWYR = -999999.0
                     ENDIF
                  ELSE
                     ANNUAL_ENERGY_REVENUE(CN) = 0.


                        MARKET_REVENUE(CN) = 0.

                     MARKET_COST(CN) = ANNUAL_FIXED_COST(CN)
                     MARKET_AVE_REVENUE = 0.0
                 marcount=marcount+1
                 call write_trace_int4(file_trace_amoc, "4. marcount: ", &
                    marcount)
       call write_trace_real4(file_trace_amoc, "7. MAR", &
            MARKET_AVE_REVENUE)
                     CF = 0.0
                     ANNUAL_ENERGY = 0.0
                    call write_trace_real4(file_trace_amoc, "13. AE", &
                        ANNUAL_ENERGY)

                     ANNUAL_FUEL_COST(CN) = 0.0
                     ANNUAL_VARIABLE_COST(CN) = 0.0
                     TOTAL_AVERAGE_COST = ANNUAL_FIXED_COST(CN)
       call write_trace_real4(file_trace_amoc, "8. TAC", &
            TOTAL_AVERAGE_COST)
!
                     TEMP_NET_MARGIN = -999999.0
                     IF(EQUIVALENT_CAPACITY(CN) > 0.) THEN
                        TEMP_NET_MARGIN_PER_KWYR = &
                          001*(MARKET_REVENUE(CN) - MARKET_COST(CN)) / &
                                                 EQUIVALENT_CAPACITY(CN)
                     ELSE
                        TEMP_NET_MARGIN_PER_KWYR = -999999.0
                     ENDIF
                  ENDIF
       
                  IF(TEMP_NET_MARGIN > NET_MARGIN(CN)) THEN
                     NET_MARGIN(CN) =  TEMP_NET_MARGIN

                     call write_trace_int2(file_trace_amoc, "CN", CN)
                     call write_trace_real4(file_trace_amoc, "7. NM", &
                        NET_MARGIN(CN))
       
       
                     NET_MARGIN_INTERVAL(CN) = J
                     call write_trace_int2(file_trace_amoc, "J", J)
                     call write_trace_int2(file_trace_amoc, "NMI", &
                        NET_MARGIN_INTERVAL(CN))
                        
       
                  ENDIF
                  IF(TEMP_NET_MARGIN_PER_KWYR > &
                                           NET_MARGIN_PER_KWYR(CN)) THEN
                     NET_MARGIN_PER_KWYR(CN) =  TEMP_NET_MARGIN_PER_KWYR
                  ENDIF
!
                  IF(PLANNING_PER_MWH) THEN
                     IF(NET_MARGIN(CN) + .01 > HIGHEST_NET_MARGIN) THEN
                    call write_trace_int2(file_trace_amoc, "2. CN", CN)
                        call write_trace_message(file_trace_amoc, &
                            "NM > highest")
                        call write_trace_bool1(file_trace_amoc, &
                            "CX_ACTIVE", CX_ACTIVE)
                        
                        IF(CX_ACTIVE) THEN
                           HIGHEST_CHRONO_MW_USAGE = &
                                         LOCAL_CHRONO_MW_USAGE
                        ENDIF
                        HIGHEST_NET_MARGIN = NET_MARGIN(CN)
                        HIGHEST_OPTION = CN
       call write_trace_int2(file_trace_amoc, "2. HO", HIGHEST_OPTION)
                        HIGHEST_J = J
                        HIGHEST_TOP_CAP_PERCENT = TOP_CAP_PERCENT
                        HIGHEST_ENERGY(CN) = ANNUAL_ENERGY
                        HIGHEST_CF(CN) = CF
                        HIGHEST_ICAP_REVENUE = ANNUAL_ICAP_REVENUE
                        HIGHEST_ENERGY_REVENUE = &
                                               ANNUAL_ENERGY_REVENUE(CN)
                        HIGHEST_NET_MARGIN_PER_MWH = TEMP_NET_MARGIN
                        HIGHEST_NET_MARGIN_PER_KWYR = &
                                                TEMP_NET_MARGIN_PER_KWYR
                        HIGHEST_ICAP_DEMAND_CURVE_MULT = &
                                                ICAP_DEMAND_CURVE_MULT_K
                        HIGHEST_EAS_PROFIT_PER_KWYR = &
                                                EAS_PROFIT_PER_KWYR_K
                        HIGHEST_CONE_COST_PER_KWYR = &
                                                CONE_COST_PER_KWYR_K
                        HIGHEST_CONE_DEM_ADJ_PER_KWYR = &
                                          CONE_DEM_CURVE_ADJ_PER_KWYR_K
                        HIGHEST_ICAP_REVENUE_PER_KWYR = &
                                          ICAP_REVENUE_PER_KWYR_K
                        HIGHEST_CM_CAPACITY_MW = CM_CAPACITY_MW(CM)
                        HIGHEST_CM_RESERVE_MARGIN = &
                                                   CM_RESERVE_MARGIN(CM)
                     ENDIF
                  ELSE
                     IF(NET_MARGIN_PER_KWYR(CN) + .01 > &
                                                HIGHEST_NET_MARGIN) THEN
                        IF(CX_ACTIVE) THEN
                           HIGHEST_CHRONO_MW_USAGE = &
                                         LOCAL_CHRONO_MW_USAGE
                        ENDIF
                        HIGHEST_NET_MARGIN = NET_MARGIN_PER_KWYR(CN)
                        HIGHEST_OPTION = CN
       call write_trace_int2(file_trace_amoc, "3. HO", HIGHEST_OPTION)
                        HIGHEST_J = J
                        HIGHEST_TOP_CAP_PERCENT = TOP_CAP_PERCENT
                        HIGHEST_ENERGY(CN) = ANNUAL_ENERGY
                        HIGHEST_CF(CN) = CF
                        HIGHEST_ICAP_REVENUE = ANNUAL_ICAP_REVENUE
                        HIGHEST_ENERGY_REVENUE = &
                                               ANNUAL_ENERGY_REVENUE(CN)
                        HIGHEST_NET_MARGIN_PER_MWH = TEMP_NET_MARGIN
                        HIGHEST_NET_MARGIN_PER_KWYR = &
                                                TEMP_NET_MARGIN_PER_KWYR
                        HIGHEST_ICAP_DEMAND_CURVE_MULT = &
                                                ICAP_DEMAND_CURVE_MULT_K
                        HIGHEST_EAS_PROFIT_PER_KWYR = &
                                                EAS_PROFIT_PER_KWYR_K
                        HIGHEST_CONE_COST_PER_KWYR = &
                                                CONE_COST_PER_KWYR_K
                        HIGHEST_CONE_DEM_ADJ_PER_KWYR = &
                                          CONE_DEM_CURVE_ADJ_PER_KWYR_K
                        HIGHEST_ICAP_REVENUE_PER_KWYR = &
                                          ICAP_REVENUE_PER_KWYR_K
                        HIGHEST_CM_CAPACITY_MW = CM_CAPACITY_MW(CM)
                        HIGHEST_CM_RESERVE_MARGIN = &
                                                   CM_RESERVE_MARGIN(CM)
                     ENDIF
                  ENDIF
!
                  K = K + 1
!
               ENDDO ! OPTIONS
!
               IF( (RUN_MULTIAREA .OR. RUN_PRICE_MODE .OR. &
                                            RUN_AREA_PRICE_MODE) .AND. &
                     HO < MAX_RESOURCES_PER_GROUP .AND. &
                         ( (HIGHEST_NET_MARGIN > MINIMUM_NET_MARGIN &
                         .AND. HIGHEST_ENERGY(HIGHEST_OPTION)> 0 ).OR. &
                              USE_MINIMUM_RM_LOGIC .OR. &
                              USE_REGIONAL_MINIMUM_RM_LOGIC) .AND. &
                                       LOCAL_RESOURCES_AVAILABLE .AND. &
                                                HIGHEST_OPTION > 0) THEN
!
                  LOCAL_ANNUAL_UNITS(HIGHEST_OPTION) = &
                                  LOCAL_ANNUAL_UNITS(HIGHEST_OPTION) - 1
!
                  HO = HO + 1
                  IF( HO > MX_RES_P_YEAR) THEN
                     WRITE(4,*) "In the MRX Resource Expansion"
                     WRITE(4,*) "Algorithm, more than ", &
                                 MX_RES_P_YEAR," resources"
                     WRITE(4,*) "were added to meet total system"
                     WRITE(4,*) "market conditions."
                     HO = MX_RES_P_YEAR
                  ENDIF

                  HGST_OPT_INDEX(HO) = HIGHEST_OPTION
                  HGST_NET_MARG_ADDED(HO)  = HIGHEST_NET_MARGIN
         call write_trace_real4(file_trace_amoc, "12. HGMA", &
            HGST_NET_MARG_ADDED(HO))

                  SORTED_OPTIONS(HO) = HO

                  HIGHEST_RFT(HO) = RFT(HIGHEST_OPTION)
                  HIGHEST_RPM(HO) = RPM(HIGHEST_OPTION)

                  IF(MX_SUMMARY_REPORT) THEN

                     WRITE(LOCAL_NAME,1010) &
                                    OPTION_NAME(HIGHEST_OPTION)(1:17),HO
              call write_trace_string(file_trace_amoc, &
                "3. LOCAL_NAME", LOCAL_NAME)
 1017                FORMAT(A,A,I4)
 1010                FORMAT(A,I4)
!
                     HIGHEST_STRIKE_PRICE(HO) = &
                                         STRIKE_PRICE(HIGHEST_OPTION,MO)
                     HIGHEST_ENERGY_REV_PER_MWH(HO) = &
                                  ENERGY_REVENUE_PER_MWH(HIGHEST_OPTION)
                     HIGHEST_FUEL_COST_PER_MWH(HO) = &
                                      AVERAGE_FUEL_PRICE(HIGHEST_OPTION)
                     HIGHEST_MRX_ENERGY(HO) = &
                                   HIGHEST_ENERGY(HIGHEST_OPTION)/1000.0
                     IF(HIGHEST_MRX_ENERGY(HO) > 0.) THEN
                        MARKET_AVE_REVENUE = &
                              MARKET_REVENUE(HIGHEST_OPTION) / &
                                          HIGHEST_MRX_ENERGY(HO)
       call write_trace_real4(file_trace_amoc, "9. MAR", &
            MARKET_AVE_REVENUE)
                 marcount=marcount+1
                 call write_trace_int4(file_trace_amoc, "5. marcount: ", &
                    marcount)
                        TOTAL_AVERAGE_COST = &
                              MARKET_COST(HIGHEST_OPTION) / &
                                          HIGHEST_MRX_ENERGY(HO)
       call write_trace_real4(file_trace_amoc, "10. TAC", &
            TOTAL_AVERAGE_COST)
                     ELSE
                        MARKET_AVE_REVENUE = 0.
                        TOTAL_AVERAGE_COST = 0.
       call write_trace_real4(file_trace_amoc, "11. TAC/MAR", &
            0.)
                     ENDIF
!
                     IF(TG_MW_TRANSFERS) THEN
                        TG_CAPACITY_MW(L) = &
                           TG_PLANNING_CAPACITY(L) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(L) + &
                                    TG_Net_Transfer(L)
                     ELSE
                        TG_CAPACITY_MW(L) = &
                           TG_PLANNING_CAPACITY(L) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(L)! +
                     ENDIF

                     IF(TG_PEAK_MW(L) > .01) THEN
                        TG_RESERVE_MARGIN(L) = 100. * &
                              (TG_CAPACITY_MW(L)/TG_PEAK_MW(L) - 1.0)
                     ELSE
                        TG_RESERVE_MARGIN(L) = 100.
                     ENDIF
!
                     PA_CAPACITY_MW(PA) = &
                           PA_PLANNING_CAPACITY(PA) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(L) ! +
                     IF(PA_PEAK_MW(PA) > .01) THEN
                        PA_RESERVE_MARGIN(PA) = 100. * &
                              (PA_CAPACITY_MW(PA)/PA_PEAK_MW(PA) - 1.0)
                     ELSE
                        PA_RESERVE_MARGIN(PA) = 100.
                     ENDIF
!
                     IF(TG_MW_TRANSFERS) THEN
                        CM_CAPACITY_MW(CM) = &
                           CM_PLANNING_CAPACITY(CM) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(L)  + &
                                    CM_Net_Transfer(CM)
                 call write_trace_real4(file_trace_mxa, "14. CM_CAP_MW", &
                    CM_CAPACITY_MW(CM))
                     ELSE
                        CM_CAPACITY_MW(CM) = &
                           CM_PLANNING_CAPACITY(CM) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(L) 
                 call write_trace_real4(file_trace_mxa, "13. CM_CAP_MW", &
                    CM_CAPACITY_MW(CM))
                     ENDIF

                     IF(CM_PEAK_MW(CM) > .01) THEN
                        CM_RESERVE_MARGIN(CM) = 100. * &
                              (CM_CAPACITY_MW(CM)/CM_PEAK_MW(CM) - 1.0)

                        CM_INTERUPTIBLRESERVE_MARGIN = 100. * &
                         (CM_CAPACITY_MW(CM) - &
                                    CM_PEAK_AFTER_INTERRUPTIBLE(CM)) / &
                                CM_PEAK_AFTER_INTERRUPTIBLE(CM)
                     ELSE
                        CM_RESERVE_MARGIN(CM) = 100.
                     ENDIF
                     TEMP_R4 = CM_RESERVE_MARGIN(CM)*0.01 + 1.0
                     TEMP_R4 = CM_INTERUPTIBLRESERVE_MARGIN*0.01 + 1.0
                     
               if(R_YEAR==2 .and. CM==1) then
                    r_YEAR=R_YEAR ! Debugstop
               end if
                     ICAP_DEMAND_CURVE_MULT(HO) = &
                       GET_ICAP_REVENUE_MULT(R_YEAR,CM,TEMP_R4)
                     IF(SCREEN_CAPACITY(HIGHEST_OPTION) > 0.01) THEN
!
                        IF(ICAP_SWITCH_INDEX(CM) == 2) THEN
                           CONE_COST_PER_KWYR(HO) = &
                                    LOWEST_FIXED_COST_BY_CM(CM)
                        ELSE
                           CONE_COST_PER_KWYR(HO) = ICAP_CONE_PRICE(CM)
                        ENDIF
                        CONE_DEMAND_CURVE_ADJ_PER_KWYR(HO) = &
                                    CONE_COST_PER_KWYR(HO) * &
                                    ICAP_DEMAND_CURVE_MULT(HO)
!
                        IF(ICAP_SWITCH_INDEX(CM) == 2 .OR.   & !MARGINAL
                                        ICAP_SWITCH_INDEX(CM) == 3) THEN
                                        ! CONE-MRX
                           CAPACITY_REVENUE_MM(HO) = &
                              MIN(CONE_DEMAND_CURVE_ADJ_PER_KWYR(HO) * &
                                SCREEN_CAPACITY(HIGHEST_OPTION)*0.001, &
                                MAX(0.,0.000001 * &
                               (ANNUAL_VARIABLE_COST(HIGHEST_OPTION) + &
                                  ANNUAL_FIXED_COST(HIGHEST_OPTION) - &
                                               HIGHEST_ENERGY_REVENUE)))
                           ICAP_REVENUE_PER_KWYR(HO) = &
                              MAX(0.0,CAPACITY_REVENUE_MM(HO)*1000.0/ &
                                       SCREEN_CAPACITY(HIGHEST_OPTION))
        call write_trace_int2(file_trace_amoc, "HO", HO)
        call write_trace_real4(file_trace_amoc, &
            "5. ICAP_REVENUE_PER_KWYR", &
            ICAP_REVENUE_PER_KWYR(HO))
                        ELSE
                           ICAP_REVENUE_PER_KWYR(HO) = &
                                 MIN(ICAP_CONE_PRICE(CM), &
                                     CONE_DEMAND_CURVE_ADJ_PER_KWYR(HO))
                           CAPACITY_REVENUE_MM(HO) = &
                              ICAP_REVENUE_PER_KWYR(HO) * &
                                 SCREEN_CAPACITY(HIGHEST_OPTION) * 0.001
        call write_trace_int2(file_trace_amoc, "HO", HO)
        call write_trace_real4(file_trace_amoc, &
            "6. ICAP_REVENUE_PER_KWYR", &
            ICAP_REVENUE_PER_KWYR(HO))
                        ENDIF
                        EAS_PROFIT_PER_KWYR(HO) = &
                                          HIGHEST_EAS_PROFIT_PER_KWYR
                        EAS_PROFIT_PER_KWYR(HO) = MAX(0.0, &
                                  CONE_DEMAND_CURVE_ADJ_PER_KWYR(HO) - &
                                              ICAP_REVENUE_PER_KWYR(HO))
                     ELSE
                        EAS_PROFIT_PER_KWYR(HO) = 0.0
                        CONE_COST_PER_KWYR(HO) = 0.0
                        CONE_DEMAND_CURVE_ADJ_PER_KWYR(HO) = 0.0
                        ICAP_REVENUE_PER_KWYR(HO) = 0.0
        call write_trace_int2(file_trace_amoc, "HO", HO)
        call write_trace_real4(file_trace_amoc, &
            "7. ICAP_REVENUE_PER_KWYR", &
            ICAP_REVENUE_PER_KWYR(HO))
                        CAPACITY_REVENUE_MM(HO) = 0.0
                     ENDIF
!
                     ENERGY_REVENUE_MM(HO) = &
                                         HIGHEST_ENERGY_REVENUE/1000000.
                     HIGHEST_MARKET_REVENUE(HO) = &
                                ENERGY_REVENUE_MM(HO) + &
                                 RPS_REVENUE_MM(HO) + &
                                     CAPACITY_REVENUE_MM(HO)
!
! END TESTING
!
                     IF(HIGHEST_MRX_ENERGY(HO) > 0.01) THEN
                        CAPACITY_REVENUE_PER_MWH(HO) = &
                            CAPACITY_REVENUE_MM(HO)*1000.0/ &
                                          HIGHEST_MRX_ENERGY(HO)
                        NET_MARGIN(HIGHEST_OPTION) = &
                                 HIGHEST_MARKET_REVENUE(HO) / &
                                          HIGHEST_MRX_ENERGY(HO)
       call write_trace_real4(file_trace_amoc, "2. NM",  &
        NET_MARGIN(HIGHEST_OPTION))
                     ELSE
                        CAPACITY_REVENUE_PER_MWH(HO) = 0.0
                        NET_MARGIN(HIGHEST_OPTION) = -999999.
       call write_trace_real4(file_trace_amoc, "3. NM",  &
        NET_MARGIN(HIGHEST_OPTION))
                     ENDIF
                     REVENUE_PER_MWH(HO) = &
                           HIGHEST_ENERGY_REV_PER_MWH(HO) + &
                                CAPACITY_REVENUE_PER_MWH(HO)
                     VARIABLE_COST_MM(HO) = 0.000001 * &
                                    ANNUAL_VARIABLE_COST(HIGHEST_OPTION)
                     FIXED_COST_MM(HO) = 0.000001 * &
                                    ANNUAL_FIXED_COST(HIGHEST_OPTION)
                     HIGHEST_SCREEN_CAPACITY(HO) = &
                                         SCREEN_CAPACITY(HIGHEST_OPTION)
                     FIXED_COST_PER_MWH(HO) = &
                        FOM_PER_MWH(HIGHEST_OPTION) + &
                              LEVELIZED_CAP_PER_MWH(HIGHEST_OPTION)
                     HIGHEST_MRX_MARGIN(HO) = &
                              REVENUE_PER_MWH(HO) - &
                                 HIGHEST_STRIKE_PRICE(HO) - &
                                    FIXED_COST_PER_MWH(HO)
                     HIGHEST_MRX_CF(HO) = HIGHEST_CF(HIGHEST_OPTION)
                     HIGHEST_MARKET_COST(HO) = &
                                   MARKET_COST(HIGHEST_OPTION)/1000000.0
                     HIGHEST_ANN_LEVEL_CAP(HO) = &
                                ANNUAL_LEVELIZED_CAPITAL(HIGHEST_OPTION)
                     IF(HIGHEST_SCREEN_CAPACITY(HO) > 0.01) THEN
                        NET_MARGIN_PER_KWYR(HIGHEST_OPTION) = 1000.0 * &
                              (HIGHEST_MARKET_REVENUE(HO) - &
                                 VARIABLE_COST_MM(HO) - &
                                       FIXED_COST_MM(HO))/ &
                                 HIGHEST_SCREEN_CAPACITY(HO)
                     ELSE
                        NET_MARGIN_PER_KWYR(HIGHEST_OPTION) = -999999.0
                     ENDIF
!
                     HIGHEST_FUEL_COST(HO) = &
                              ANNUAL_FUEL_COST(HIGHEST_OPTION)/1000000.0
                     HIGHEST_HEAT_RATE(HO) = &
                                       HEAT_RATE(HIGHEST_OPTION)
                     HIGHEST_ANNUAL_UNITS(HO) = &
                               FLOAT(LOCAL_ANNUAL_UNITS(HIGHEST_OPTION))
                     HIGHEST_MARGIN_PER_KWYR(HO) = &
                                     NET_MARGIN_PER_KWYR(HIGHEST_OPTION)
                     HIGHEST_VAR_OM_PER_MWH(HO) = &
                                   VAR_OM_PER_MWH(HIGHEST_OPTION) ! 19
                     HIGHEST_FUEL_COST_PER_MMBTU(HO) = &
                                     FUEL_COST_PER_MMBTU(HIGHEST_OPTION)
                     HIGHEST_EMISSIONS_PER_MWH(HO) = &
                                       EMISSIONS_PER_MWH(HIGHEST_OPTION)
                     HIGHEST_FOM_PER_MWH(HO) = &
                                             FOM_PER_MWH(HIGHEST_OPTION)
                     HIGHEST_LEVEL_CAP_PER_MWH(HO) = &
                                   LEVELIZED_CAP_PER_MWH(HIGHEST_OPTION)
                     HIGHEST_VOM_COST_MM(HO) = &
                                             VOM_COST_MM(HIGHEST_OPTION)
                     HIGHEST_EMISSIONS_COST_MM(HO) = &
                                       EMISSIONS_COST_MM(HIGHEST_OPTION)
                     HIGHEST_FOM_COST_MM(HO) = &
                                             FOM_COST_MM(HIGHEST_OPTION)

                     MRX_ICAP_INDEX(HO) = 0

                     MXVars(1:13,L,26) = CONE_COST_PER_KWYR(HO)
                     MX_ANNUAL_ALT_REC = RPTREC(MX_ANNUAL_ALT_NO)
              call write_trace_string(file_trace_amoc, &
                "4. LOCAL_NAME", LOCAL_NAME)
             call write_trace_message(file_trace_mxa, "2. WRITE: ")
            if (file_trace_mxa/=BAD_TRACE_HANDLE) then
                    write(file_trace_mxa,*)  &
      "MX_ANNUAL_ALT_REC ", MX_ANNUAL_ALT_REC, &
      "Year ", FLOAT(THIS_YEAR), &
      "MULTI_AREA_NAME ", MULTI_AREA_NAME(L), &
      "LOCAL_NAME ", LOCAL_NAME, &
      "HIGHEST_STRIKE_PRICE ", HIGHEST_STRIKE_PRICE(HO), & 
      "HIGHEST_ENERGY_REV_PER_MWH ", HIGHEST_ENERGY_REV_PER_MWH(HO), & 
      "HIGHEST_MRX_MARGIN ", HIGHEST_MRX_MARGIN(HO), &
      "HIGHEST_MRX_CF ", HIGHEST_MRX_CF(HO), &
      "ENERGY_REVENUE_MM ", ENERGY_REVENUE_MM(HO), &
      "HIGHEST_MARKET_COST ", HIGHEST_MARKET_COST(HO), & 
      "VARIABLE_COST_MM ", VARIABLE_COST_MM(HO), &
      "FIXED_COST_MM ", FIXED_COST_MM(HO), &
      "HIGHEST_SCREEN_CAPACITY ", HIGHEST_SCREEN_CAPACITY(HO), &
      "HIGHEST_MRX_ENERGY ", HIGHEST_MRX_ENERGY(HO), & !
      "TOTAL_AVERAGE_COST ", TOTAL_AVERAGE_COST, & 
      "HIGHEST_ANN_LEVEL_CAP ", HIGHEST_ANN_LEVEL_CAP(HO), &
      "HIGHEST_FUEL_COST ", HIGHEST_FUEL_COST(HO), &
      "HIGHEST_ANNUAL_UNITS ", HIGHEST_ANNUAL_UNITS(HO), &
      "CAPACITY_REVENUE_MM ", CAPACITY_REVENUE_MM(HO), &
      "HIGHEST_MARKET_REVENUE ", HIGHEST_MARKET_REVENUE(HO), & 
      "HIGHEST_MARGIN_PER_KWYR ", HIGHEST_MARGIN_PER_KWYR(HO), &
      "HIGHEST_FUEL_COST_PER_MWH ", HIGHEST_FUEL_COST_PER_MWH(HO), &
      "HIGHEST_VAR_OM_PER_MWH ", HIGHEST_VAR_OM_PER_MWH(HO), & 
      "HIGHEST_FUEL_COST_PER_MMBTU ", HIGHEST_FUEL_COST_PER_MMBTU(HO), &
      "REVENUE_PER_MWH ", REVENUE_PER_MWH(HO),  & 
      "CAPACITY_REVENUE_PER_MWH ", CAPACITY_REVENUE_PER_MWH(HO), &
      "HIGHEST_EMISSIONS_PER_MWH ", HIGHEST_EMISSIONS_PER_MWH(HO), &
      "HIGHEST_FOM_PER_MWH ", HIGHEST_FOM_PER_MWH(HO), &
      "HIGHEST_LEVEL_CAP_PER_MWH ", HIGHEST_LEVEL_CAP_PER_MWH(HO), &
      "HIGHEST_VOM_COST_MM ", HIGHEST_VOM_COST_MM(HO),  & 
      "HIGHEST_EMISSIONS_COST_MM ", HIGHEST_EMISSIONS_COST_MM(HO), &
      "HIGHEST_FOM_COST_MM ", HIGHEST_FOM_COST_MM(HO), &
      "TG ", FLOAT(TG), & 
      "TG_CAPACITY_MW ", TG_CAPACITY_MW(L), &
      "TG_PEAK_MW ", TG_PEAK_MW(L),  & 
      "TG_RESERVE_MARGIN ", TG_RESERVE_MARGIN(L), &
      "CM_INDEX ", FLOAT(CM_INDEX), & 
      "CM_CAPACITY_MW ", CM_CAPACITY_MW(CM), &
      "CM_PEAK_MW ", CM_PEAK_MW(CM), &
      "CM_RESERVE_MARGIN ", CM_RESERVE_MARGIN(CM),  & 
      "ICAP_DEMAND_CURVE_MULT ", ICAP_DEMAND_CURVE_MULT(HO), &
      "CONE_COST_PER_KWYR ", CONE_COST_PER_KWYR(HO), &
      "CONE_DEMAND_CURVE_ADJ_PER_KWYR ", &
      CONE_DEMAND_CURVE_ADJ_PER_KWYR(HO), &
      "EAS_PROFIT_PER_KWYR ", EAS_PROFIT_PER_KWYR(HO), &
      "ICAP_REVENUE_PER_KWYR ", ICAP_REVENUE_PER_KWYR(HO) ,  & !  40
      "HIGHEST_HEAT_RATE ", HIGHEST_HEAT_RATE(HO), &
      "FIXED_COST_PER_MWH ", FIXED_COST_PER_MWH(HO), &
      "HIGHEST_RFT ", HIGHEST_RFT(HO), &
      "HIGHEST_RPM ", HIGHEST_RPM(HO), &
      "TG_Net_Transfer ", TG_Net_Transfer(L), & !  45
      "CM_Net_Transfer ", CM_Net_Transfer(CM), &
      "MRX_ICAP_INDEX ", MRX_ICAP_INDEX(HO), &
      "CM_INTERUPTIBLRESERVE_MARGIN ", CM_INTERUPTIBLRESERVE_MARGIN, &
      "CM_PEAK_AFTER_INTERRUPTIBLE ", CM_PEAK_AFTER_INTERRUPTIBLE(CM), &
      "CAPACITY_PLANNING_MW ", CAPACITY_PLANNING_MW(HIGHEST_OPTION), &
      "RPS_REVENUE_MM ", RPS_REVENUE_MM(HO), &
      "RPS_REVENUE_PER_MWH ", RPS_REVENUE_PER_MWH(HO), " "
            end if
                     ! TODO: Combined writes to one in separate routine
                     WRITE(MX_ANNUAL_ALT_NO,REC=MX_ANNUAL_ALT_REC) &
                        PRT_ENDPOINT(), &
                        FLOAT(THIS_YEAR), &
                        MULTI_AREA_NAME(L), &
                        LOCAL_NAME, &
                        HIGHEST_STRIKE_PRICE(HO), & !  0
                        HIGHEST_ENERGY_REV_PER_MWH(HO), & 
                        ! 111509. REDEFINED.
                        HIGHEST_MRX_MARGIN(HO), &
                        HIGHEST_MRX_CF(HO), &
                        ENERGY_REVENUE_MM(HO), &
                        HIGHEST_MARKET_COST(HO), & !  5
                        VARIABLE_COST_MM(HO), &
                        FIXED_COST_MM(HO), &
                        HIGHEST_SCREEN_CAPACITY(HO), &
                        HIGHEST_MRX_ENERGY(HO), & !
                        TOTAL_AVERAGE_COST, & !  10 NEED DEFINITION
                        HIGHEST_ANN_LEVEL_CAP(HO), &
                        HIGHEST_FUEL_COST(HO), &
                        HIGHEST_ANNUAL_UNITS(HO), &
                        CAPACITY_REVENUE_MM(HO), &
                        HIGHEST_MARKET_REVENUE(HO), & 
                        ! 15 TOTAL REVENUE
                        HIGHEST_MARGIN_PER_KWYR(HO), &
                        HIGHEST_FUEL_COST_PER_MWH(HO), &
                        HIGHEST_VAR_OM_PER_MWH(HO), & !  18
                        HIGHEST_FUEL_COST_PER_MMBTU(HO), &
                        REVENUE_PER_MWH(HO),  & !  20
                        CAPACITY_REVENUE_PER_MWH(HO), &
                        HIGHEST_EMISSIONS_PER_MWH(HO), &
                        HIGHEST_FOM_PER_MWH(HO), &
                        HIGHEST_LEVEL_CAP_PER_MWH(HO), &
                        HIGHEST_VOM_COST_MM(HO),  & !  25
                        HIGHEST_EMISSIONS_COST_MM(HO), &
                        HIGHEST_FOM_COST_MM(HO), &
                        FLOAT(TG), & !  TRANS_GROUP_ID(HIGHEST_OPTION),
                        TG_CAPACITY_MW(L), &
                        TG_PEAK_MW(L),  & !  30
                        TG_RESERVE_MARGIN(L), &
                        FLOAT(CM_INDEX), & 
                        ! PLANNING_AREA_ID(HIGHEST_OPTION),
                        CM_CAPACITY_MW(CM), &
                        CM_PEAK_MW(CM), &
                        CM_RESERVE_MARGIN(CM),  & !  35
                        ICAP_DEMAND_CURVE_MULT(HO), &
                        CONE_COST_PER_KWYR(HO), &
                        CONE_DEMAND_CURVE_ADJ_PER_KWYR(HO), &
                        EAS_PROFIT_PER_KWYR(HO), &
                        ICAP_REVENUE_PER_KWYR(HO) ,  & !  40
                        HIGHEST_HEAT_RATE(HO), &
                        FIXED_COST_PER_MWH(HO), &
                        HIGHEST_RFT(HO), &
                        HIGHEST_RPM(HO), &
                        TG_Net_Transfer(L), & !  45
                        CM_Net_Transfer(CM), &
                        MRX_ICAP_INDEX(HO), &
                        CM_INTERUPTIBLRESERVE_MARGIN, &
                        CM_PEAK_AFTER_INTERRUPTIBLE(CM), &
                        CAPACITY_PLANNING_MW(HIGHEST_OPTION), &
                        RPS_REVENUE_MM(HO), &
                        RPS_REVENUE_PER_MWH(HO)
                     MX_ANNUAL_ALT_REC = MX_ANNUAL_ALT_REC + 1
                  ENDIF
!
                  CALL ALTER_MX_PRICE_CURVE(  HIGHEST_INTERVAL, &
                                           MARKET_DURATION, &
                                           MARKET_CUM_REVENUE, &
                                           MARKET_PRICE, &
                                           EQUIVALENT_CAPACITY( &
                                                      HIGHEST_OPTION), &
                                           HIGHEST_ENERGY( &
                                                      HIGHEST_OPTION), &
                                           STRIKE_PRICE( &
                                                   HIGHEST_OPTION,MO), &
                                           HIGHEST_J, &
                                           HIGHEST_TOP_CAP_PERCENT, &
                                           MAX(HIGHEST_ENERGY_REVENUE, &
                                                                 0.0), &
                                           TRANS_GROUP_4_PRICING, &
                                           RESOURCE_ADDITION)
                  IF(CX_ACTIVE) THEN ! .AND.
            if(r_year==0) then
               call end_program("cap_objt:0053 - year must be nonzero.")
            end if
         call write_trace_int2(file_trace_amoc, "17. TG", TG)
                     IF(TG == 0) THEN
                        TEMP_PEAK =  UPDATE_NET_PLANNING_PEAK(R_YEAR)
                     ELSE
                        TEMP_PEAK = GET_GROUP_PEAK_ON_PEAK_MONTH(L)
                     ENDIF
                     TEMP_L = CX_ALTER_MX_CHRONO_PRICES( &
                                    TG_PLANNING_CAPACITY(L), &
                                    HIGHEST_CHRONO_MW_USAGE, &
                                    LOCAL_CHRONO_MARKET_PRICES(1,L), &
                                    TEMP_PEAK)
                  ENDIF
!
! 040611. NOTE THE ON FIRST_CT_EAS IS $/KWYR
!
                     U = GET_PRODUCTION_DATA_POINTER(HIGHEST_OPTION)
                     RPM = FLOAT(GET_S_PRIMARY_MOVER_INDEX(U))
                     IF(FIRST_CT_4_EAS_INDEX(CM) <= 0 .AND. &
                                    HIGHEST_RPM(HO) == 4 .AND. &
                            SCREEN_CAPACITY(HIGHEST_OPTION) > 0.01) THEN
                        FIRST_CT_4_EAS_INDEX(CM) = HO
                        FIRST_CT_EAS(CM) = 1000.* &
                                    ENERGY_REVENUE_MM(HO)/ &
                                         SCREEN_CAPACITY(HIGHEST_OPTION)
                     ENDIF
!
! FANCY DYNAMIC UPDATING.
!
                  TOTAL_ANNUAL_CAPACITY_ADDED(PA) = &
                           TOTAL_ANNUAL_CAPACITY_ADDED(PA) + &
                                         SCREEN_CAPACITY(HIGHEST_OPTION)
                  CM_TOT_ANN_CAPACITY_ADDED(CM) = &
                           CM_TOT_ANN_CAPACITY_ADDED(CM) + &
                                         SCREEN_CAPACITY(HIGHEST_OPTION)
                  TG_TOTAL_ANNUAL_CAPACITY_ADDED(L) = &
                           TG_TOTAL_ANNUAL_CAPACITY_ADDED(L) + &
                                         SCREEN_CAPACITY(HIGHEST_OPTION)
                  IF(PA_PEAK_AFTER_INTERRUPTIBLE(PA) > NEAR_ZERO)  THEN
                     PA_PLANNING_RESERVE_MARGIN(PA) = &
                        1. + (PA_PLANNING_CAPACITY(PA) + &
                           TOTAL_ANNUAL_CAPACITY_ADDED(PA) - &
                                 PA_PEAK_AFTER_INTERRUPTIBLE(PA)) / &
                                      PA_PEAK_AFTER_INTERRUPTIBLE(PA)
                  ELSE
                     PA_PLANNING_RESERVE_MARGIN(PA) = ZERO
                  ENDIF
                  
               if(R_YEAR==2 .and. pa==1) then
                    r_YEAR=r_YEAR ! Debugstop
               end if
                  PA_ICAP_REVENUE_MULT(PA) = GET_ICAP_REVENUE_MULT( &
                              R_YEAR,PA,PA_PLANNING_RESERVE_MARGIN(PA))
                              
                  IF(CM_PEAK_AFTER_INTERRUPTIBLE(CM) > NEAR_ZERO)  THEN
                     CM_PLANNING_RESERVE_MARGIN(CM) = &
                        1. + (CM_PLANNING_CAPACITY(CM) + &
                                 CM_Net_Transfer(CM) + & !  100410.
                           CM_TOT_ANN_CAPACITY_ADDED(CM) - &
                                 CM_PEAK_AFTER_INTERRUPTIBLE(CM)) / &
                                      CM_PEAK_AFTER_INTERRUPTIBLE(CM)
                  ELSE
                     CM_PLANNING_RESERVE_MARGIN(CM) = ZERO
                  ENDIF
                  
                  
               if(R_YEAR==2 .and. CM==1) then
                    R_YEAR=R_YEAR ! Debugstop
               end if
               
                  CM_ICAP_REVENUE_MULT(CM) = GET_ICAP_REVENUE_MULT( &
                         R_YEAR,CM,CM_PLANNING_RESERVE_MARGIN(CM))
!
               ENDIF
!
               ANNUAL_RESOURCES_AVAILABLE = .FALSE.
               DO CN = 1, tot_all_options_cap
                  I = OPTION_POSITION(CN)
                call write_trace_int2(file_trace_amoc, "4. I", I)
                  IF(.NOT. CX_ACTIVE .AND. &
                              FILE_SOURCE_INDEX(CN) == Derivative) then
                      CYCLE
                  end if
                  IF( .NOT. RESOURCE_AVAILABLE(I, &
                                    THIS_YEAR_PLUS_LEAD_TIME(CN)) .OR. &
                      .NOT. TEST_START_STOP_YEARS(I, &
                                    THIS_YEAR_PLUS_LEAD_TIME(CN)) .OR. &
                                 ns_cap_trans%transaction_group(CN) /= TG  .OR. &
                                       LOCAL_ANNUAL_UNITS(CN) < 1 .OR. &
                               .NOT. DEPENDENT_UNIT_AVAILABLE(CN)) then
                      CYCLE
                  end if
                  ANNUAL_RESOURCES_AVAILABLE = .TRUE.
                  EXIT
               ENDDO
!
            ENDDO ! ADD UNITS WHILE STILL MAKING MONEY
         ENDDO ! TRANSACTION GROUP
        call write_sic_trace("cap_objt:0071", HO)
         CALL SortIncrPos(                HO, &
                                          SORTED_OPTIONS, &
                                          HGST_NET_MARG_ADDED)
                                          
         call write_trace_real4s(file_trace_amoc, "13. HGMA", &
            HGST_NET_MARG_ADDED)
!
         IF(USE_MAXIMUM_RM_LOGIC) THEN
            MAXIMUM_ANNUAL_MRX_CAP(0) = &
                           MAX(0.,MIN(MAXIMUM_ANNUAL_MRX_CAP(0), &
                                      (MAXIMUM_RM-ANN_CAP/PEAK) * PEAK))
         ENDIF
!
         IF(USE_MINIMUM_RM_LOGIC) THEN
            MINIMUM_ANNUAL_MRX_CAP(0) = MAX(0., &
                        (CURRENT_TARGET_RATIO-ANN_CAP/PEAK)*PEAK)
         ELSE
            MINIMUM_ANNUAL_MRX_CAP(0) = 0.
         ENDIF
!
! USED UP ABOVE FOR ICAP REVENUES
!
         TOTAL_ANNUAL_CAPACITY_ADDED = 0.
         CM_TOT_ANN_CAPACITY_ADDED = 0.
         TG_TOTAL_ANNUAL_CAPACITY_ADDED = 0.
!
         IF(MRX_SORTED_MIX) THEN
            LSP = LAST_SORTED_POSITION
            HO = MX_RES_P_YEAR
            call write_trace_int2(file_trace_amoc, "18. HO", HO)
!
            DO J = HO, 1, -1
               DO K = 1, tot_all_options_cap
                  IF(LSP < tot_all_options_cap) THEN
                     LSP = LSP + 1
                  ELSE
                     LSP = 1
                  ENDIF
                  call write_trace_int2(file_trace_amoc, "LSP", LSP)
                  I = OPTION_POSITION(LSP)
                  call write_trace_int2(file_trace_amoc, "6. I", I)
                  IF( .NOT. RESOURCE_AVAILABLE(I, &
                                   THIS_YEAR_PLUS_LEAD_TIME(LSP)) .OR. &
                         .NOT. TEST_START_STOP_YEARS(I, &
                                   THIS_YEAR_PLUS_LEAD_TIME(LSP)) .OR. &
                              .NOT. DEPENDENT_UNIT_AVAILABLE(LSP)) CYCLE
                  co_prod_pointer=&
                    GET_PRODUCTION_DATA_POINTER(I)


                  ns_cap_trans%transaction_group(LSP) = &
                                   RETURN_S_TRANS_GROUP_ID(co_prod_pointer)
                IF(GET_TRANS_GROUP_POSITION( &
                    ns_cap_trans%transaction_group(LSP))<=0) then 
                    CYCLE
                end if
                  
!
! FOUND A GOOD OPTION
!
                  EXIT
               ENDDO
               SORTED_OPTIONS(J) = J
               HGST_OPT_INDEX(J) = LSP
            ENDDO
         ENDIF ! Sorted mix
!
! 072110. FIND CHEAPEST $/KW RESOURCE PER TRANSACTION GROUP
!
         HIGHEST_TG_ICAP_REV_PER_KWYR = -999999.0
         HIGHEST_OPTION_FOR_TG = 0
         DO I = HO, 1, -1
            NEXT_CN = SORTED_OPTIONS(I)
            HIGHEST_OPTION = HGST_OPT_INDEX(NEXT_CN)
       call write_trace_int2(file_trace_amoc, "4. HO", HIGHEST_OPTION)
!
            IF(.NOT. DEPENDENT_UNIT_AVAILABLE(HIGHEST_OPTION)) CYCLE
!
            TG = GET_TRANS_GROUP_POSITION( &
                                      ns_cap_trans%transaction_group(HIGHEST_OPTION))
            call write_trace_int2(file_trace_amoc, "6. TG", TG)
            HIGHEST_OPTION_FOR_TG(TG) = HIGHEST_OPTION
            HIGHEST_TG_ICAP_REV_PER_KWYR(TG) = &
                        MAX(HIGHEST_TG_ICAP_REV_PER_KWYR(TG), &
                                  ICAP_REVENUE_PER_KWYR(HIGHEST_OPTION))
         END DO
!
! IDENTIFY THE MW AND $/KW*YR OF ADJACENT TG'S
! IDENTIFY THE LOWEST COST TG TRANSFER SET THAT CAN MEET THE SCREEN
! CAPACITY
! USE THE MIN(LOWEST COST TRANSFER,INTERNAL COST) AS THE TRIGGER
! IF UNIT IS STILL BUILT, OK
! IF UNIT IS NOT BUILT, PERFORM AND RECORD THE TRANSFER
! ADJUST THE CAPACITIES OF THE NET TG'S AND CM'S
! NEXT
!
         DO I = 1, UPPER_TRANS_GROUP
            LOWEST_TRANSFER_COST = 999999.0
            LOWEST_TRANSFER_CAPACITY = 0.0
            DO J = 1, UPPER_TRANS_GROUP
               IF(TransmissionConstraint(J,I) < 0.01) CYCLE
               IF(LOWEST_TRANSFER_COST < &
                                  HIGHEST_TG_ICAP_REV_PER_KWYR(J)) CYCLE
               LOWEST_TRANSFER_COST = HIGHEST_TG_ICAP_REV_PER_KWYR(J)
               LOWEST_TRANSFER_CAPACITY = &
                                            TransmissionConstraint(J,I)
!
! BUYER TG AND PA CAN'T EXCEED MAX RESERVE MARGIN
! SELLER TG AND PA CAN'T EXCEED MIN RESERVE MARGIN
!
            END DO
            HIGHEST_OPTION = HIGHEST_OPTION_FOR_TG(I)
       call write_trace_int2(file_trace_amoc, "5. HO", HIGHEST_OPTION)
!
! FIND CROSS-OVER POINT
!
         END DO
!
 5678    CONTINUE
!
         IF(R_YEAR == 5 .AND. GRX_ITERATIONS == 3) THEN
            R_YEAR = R_YEAR ! Debugstop
         ENDIF
!
         RPS_REVENUE_MM = 0.0
         MRX_RPS_MX_UNIT_CLEARED = 0
         IF( (GRX_RPS_MODULE_ACTIVE .OR. RPS_ONLY_SWITCH_ACTIVE) &
                                            .AND. RPS_PROGRAMS > 0) THEN
!
! 070320. MAY WANT TO MOVE THIS UP TO SAVE TIME.
!
            IF(ALLOCATED(MRX_RPS_MX_CURVE_MARGIN)) &
                DEALLOCATE(MRX_RPS_MX_CURVE_MARGIN, &
                           MRX_RPS_MX_CURVE_REVENUE, &
                           MRX_RPS_MX_CURVE_COST, &
                           MRX_RPS_MX_CURVE_UMWH, &
                           MRX_RPS_MX_CURVE_MWH, &
                           MRX_RPS_MX_OPTION_NO, &
                           MRX_RPS_MX_CURVE_INDEX, &
                           MRX_RPS_MX_OPTION_INDEX, &
                           MRX_RPS_MX_NEXT_CN_INDEX, &
                           RPS_REC_PRICE_BY, &
                           RPS_GEN_PRICE_BY)
!
            PA = RPS_PROGRAMS
            ALLOCATE(MRX_RPS_MX_CURVE_MARGIN(HO,PA), &
                     MRX_RPS_MX_CURVE_REVENUE(HO,PA), &
                     MRX_RPS_MX_CURVE_COST(HO,PA), &
                     MRX_RPS_MX_CURVE_UMWH(HO,PA), &
                     MRX_RPS_MX_CURVE_MWH(HO,PA), &
                     MRX_RPS_MX_OPTION_NO(PA), &
                     MRX_RPS_MX_CURVE_INDEX(HO,PA), &
                     MRX_RPS_MX_OPTION_INDEX(HO,PA), &
                     MRX_RPS_MX_NEXT_CN_INDEX(HO,PA), &
                     RPS_REC_PRICE_BY(PA), &
                     RPS_GEN_PRICE_BY(PA))
!
            MRX_RPS_MX_OPTION_NO = 0
            MRX_RPS_MX_CURVE_INDEX = 0
            MRX_RPS_MX_OPTION_INDEX = 0
            MRX_RPS_MX_NEXT_CN_INDEX = 0
            MRX_RPS_MX_CURVE_MARGIN = 0
            MRX_RPS_MX_CURVE_REVENUE = 0.0
            MRX_RPS_MX_CURVE_COST = 0.0
            MRX_RPS_MX_CURVE_UMWH = 0.0
!
! 072620. RPS_REVENUE SOLVED IN CAPACITY PRICE SECTION BELOW.
!
            RPS_REC_PRICE_BY = 0.0
            RPS_GEN_PRICE_BY = 0.0
!
            DO I = 1, HO
               NEXT_CN = SORTED_OPTIONS(I)
               HIGHEST_OPTION = HGST_OPT_INDEX(NEXT_CN)
       call write_trace_int2(file_trace_amoc, "6. HO", HIGHEST_OPTION)
               PA = RPS_PROGRAM_NUMBER_loc(HIGHEST_OPTION)
               IF(PA == 0) CYCLE
               RPS_COUNT = 1
               DO
                  PX = GET_RPS_PROGRAM_POSITION(PA,RPS_COUNT)
                  IF(PX == 0) EXIT
                  RPS_COUNT = RPS_COUNT + 1
                  TEMP_R4 = 1000.0*HIGHEST_MRX_ENERGY(NEXT_CN) * &
                                             RPS_PERCENT(HIGHEST_OPTION)
                  IF(TEMP_R4 < 0.0001) CYCLE
!
                  MRX_RPS_MX_OPTION_NO(PX) = &
                                            MRX_RPS_MX_OPTION_NO(PX) + 1
                  J = MRX_RPS_MX_OPTION_NO(PX)
                  MRX_RPS_MX_CURVE_UMWH(J,PX) = TEMP_R4
                  MRX_RPS_MX_CURVE_INDEX(J,PX) = J
                  MRX_RPS_MX_OPTION_INDEX(J,PX) = HIGHEST_OPTION
                  MRX_RPS_MX_NEXT_CN_INDEX(J,PX) = NEXT_CN
                  MRX_RPS_MX_CURVE_REVENUE(J,PX) = 1000. * &
                        ENERGY_REVENUE_MM(NEXT_CN) / &
                                             HIGHEST_MRX_ENERGY(NEXT_CN)
                  MRX_RPS_MX_CURVE_COST(J,PX) = 1000. * &
                        (FIXED_COST_MM(NEXT_CN) + &
                                      VARIABLE_COST_MM(NEXT_CN))/ &
                                             HIGHEST_MRX_ENERGY(NEXT_CN)
                  MRX_RPS_MX_CURVE_MARGIN(J,PX) = &
                        MRX_RPS_MX_CURVE_REVENUE(J,PX) - &
                                             MRX_RPS_MX_CURVE_COST(J,PX)
               ENDDO
            ENDDO
            DO PA = 1, RPS_PROGRAMS
               IF(MRX_RPS_MX_OPTION_NO(PA) > 1) THEN
        call write_sic_trace("cap_objt:0073", MRX_RPS_MX_OPTION_NO(PA))
                  CALL SortIncrPos(MRX_RPS_MX_OPTION_NO(PA), &
                                MRX_RPS_MX_CURVE_INDEX(1,PA), &
                                MRX_RPS_MX_CURVE_MARGIN(1,PA))
               ENDIF
            ENDDO
            RPS_RESOURCE_ADDED = 0
            DO PA = 1, RPS_PROGRAMS
               PROG_REQ = GET_ANNUAL_RPS_PROG_REQ(PA,R_YEAR)
               TEMP_L = GET_RPS_NAME(RPS_NAME,PA)
               RPS_ALT_COMPLIANCE_PRICE = &
                        GET_RPS_ALT_COMPLIANCE_PRICE(PA,R_YEAR)
!
              ZERO_BID_MWH = GET_QUALIFYING_GEN_DB(PA,INT(5,2),INT(1,2))
               J = 0
               K = 0
               L = 0
               M = 0
               I = MRX_RPS_MX_OPTION_NO(PA)
               U = MRX_RPS_CL_UNIT_NO(PA)
               D = MRX_RPS_DV_UNIT_NO(PA)
               LAST_EN = 0
               LAST_EU = 0
               LAST_ED = 0
               TEMP_COST = 0.0
               TEMP_REVENUE = 0.0
               TEMP_MWH = 0.0
               CUMULATIVE_MWH = 0.0
               PA_BID = 0.0
               PA_COST = 0.0
               PA_REVENUE = 0.0
               PA_CUM_MWH = 0.0
               RPS_CROSS_OVER = .FALSE.
               RPS_GEN_WEIGHTED_REV = 0.0
               RPS_GEN_WEIGHTED_PRICE = 0.0
!
! NEED BIDS FOR RESOURCES BY MWH NOT KW*YR
!
               DO
                  IF(I+U+D <= 0) EXIT
                  IF(I > 0) THEN
                     M = MRX_RPS_MX_CURVE_INDEX(I,PA)
                     FN = MRX_RPS_MX_NEXT_CN_INDEX(M,PA)
                     EN = MRX_RPS_MX_OPTION_INDEX(M,PA)
                     LAST_EN = MAX(EN,LAST_EN)
                     NEW_BID = MAX(0.0, &
                                      -1.*MRX_RPS_MX_CURVE_MARGIN(M,PA))
                  ELSE
                     NEW_BID = 999999.
                  ENDIF
                  IF(U > 0) THEN
                     J = MRX_RPS_CL_CURVE_INDEX(U,PA)
                     EU = MRX_RPS_CL_UNIT_INDEX(J,PA)
                     LAST_EU = MAX(EU,LAST_EU)
                     EXISTING_CL_BID = MAX(0.0, &
                                      -1.*MRX_RPS_CL_CURVE_MARGIN(J,PA))
                  ELSE
                     EXISTING_CL_BID = 999999.
                  ENDIF
                  IF(D > 0) THEN
                     L = MRX_RPS_DV_CURVE_INDEX(D,PA)
                     ED = MRX_RPS_DV_UNIT_INDEX(L,PA)
                     LAST_ED = MAX(ED,LAST_ED)
                     EXISTING_DV_BID = MAX(0.0, &
                                      -1.*MRX_RPS_DV_CURVE_MARGIN(L,PA))
                  ELSE
                     EXISTING_DV_BID = 999999.
                  ENDIF
!
! DETERMINE ORDER OF NEW_BID, EXISTING_CL_BID, EXISTING_DV_BID,
! INCLUDING NO RESOURCES LEFT.
! 100810. I ONLY AFTER U AND D.
!
                  BID_POS = 0
                  IF(U+D > 0) THEN
                     IF( U>0 .AND. &
                         (( EXISTING_CL_BID <= EXISTING_DV_BID) .OR. &
                                                            D==0) ) THEN
                        BID_POS = 1
                     ELSE
                        BID_POS = 2
                     ENDIF

                  ELSEIF(I > 0) THEN
                     BID_POS = 3
                  ELSE
                     WRITE(9,*) 'INFEASIBLE RPS SUPPLY CURVE'
                     er_message="INFEASIBLE RPS SUPPLY CURVE"
                     call end_program(er_message)
                  ENDIF
                  K = K + 1
                  IF(K == 1) THEN
                     IF(ZERO_BID_MWH < 0.0001) CYCLE
                     CURVE_NAME = 'Zero Bid Capacity       '
                     TEMP_BID = 0.0
                     TEMP_REVENUE = 0.0
                     TEMP_COST = 0.0
                     TEMP_MWH = ZERO_BID_MWH
                     CM = 0
                     CM_INDEX = 0
                  ELSEIF( BID_POS == 3 ) THEN
                     WRITE(CURVE_NAME,1010) &
                                OPTION_NAME(EN)(1:20),I
                     TEMP_BID = NEW_BID
                     TEMP_MWH = MRX_RPS_MX_CURVE_UMWH(M,PA)
                     TEMP_REVENUE = MRX_RPS_MX_CURVE_REVENUE(M,PA)
                     TEMP_COST = MRX_RPS_MX_CURVE_COST(M,PA)
                     I = I - 1
                  ELSEIF( BID_POS == 1 ) THEN
                     cur_called_from="coccf:1"
                     CURVE_NAME = UNITNM(EU)//CL_UNIQUE_RPT_STR(EU)
                     TEMP_BID = EXISTING_CL_BID
                     TEMP_MWH = MRX_RPS_CL_CURVE_MWH(J,PA)
                     TEMP_REVENUE = 1000. * &
                                         MRX_RPS_CL_CURVE_REVENUE(EU)/ &
                                             MRX_RPS_CL_CURVE_MWH(J,PA)
                     TEMP_COST = 1000. * MRX_RPS_CL_CURVE_COST(EU)/ &
                                             MRX_RPS_CL_CURVE_MWH(J,PA)
                     U = U - 1
                  ELSE
                     TEMP_L = GET_TRANS_NAME(ED,TEMP_CURVE_NAME)
                     CURVE_NAME = TEMP_CURVE_NAME
                     TEMP_BID = EXISTING_DV_BID
                     TEMP_MWH = MRX_RPS_DV_CURVE_MWH(L,PA)
                     TEMP_REVENUE = 1000. * &
                                         MRX_RPS_DV_CURVE_REVENUE(ED)/ &
                                             MRX_RPS_DV_CURVE_MWH(L,PA)
                     TEMP_COST = 1000. * MRX_RPS_DV_CURVE_COST(ED)/ &
                                             MRX_RPS_DV_CURVE_MWH(L,PA)
                     D = D - 1
                  ENDIF
                  CUMULATIVE_MWH = CUMULATIVE_MWH + TEMP_MWH
                  PA_BID(K) = TEMP_BID
                  PA_COST(K) = TEMP_COST
                  PA_REVENUE(K) = TEMP_REVENUE
                  PA_CUM_MWH(K) = CUMULATIVE_MWH
                  RPS_GEN_WEIGHTED_REV = RPS_GEN_WEIGHTED_REV + &
                                                    TEMP_BID * TEMP_MWH
                  IF(CUMULATIVE_MWH > 0.001) THEN
                     RPS_GEN_WEIGHTED_PRICE = RPS_GEN_WEIGHTED_REV/ &
                                                      CUMULATIVE_MWH
                  ELSE
                     RPS_GEN_WEIGHTED_PRICE = 0.0
                  ENDIF
                  IF(K == 1) THEN
                     RPS_REC_PRICE = 0.0
                  ELSEIF(RPS_CROSS_OVER) THEN
                     RPS_REC_PRICE = RPS_REC_PRICE
                  ELSEIF(CUMULATIVE_MWH > 0.001) THEN
                     RPS_REC_PRICE = PA_BID(K)

                  ELSE
                     RPS_REC_PRICE = PA_BID(K)
                  ENDIF
                  IF(.NOT. RPS_CROSS_OVER) THEN
                     RPS_REC_PRICE_BY(PA) = RPS_REC_PRICE
                     RPS_GEN_PRICE_BY(PA) = RPS_GEN_WEIGHTED_PRICE
                  ENDIF
                  IF(.NOT. RPS_CROSS_OVER .AND. BID_POS == 3 ) THEN
                     MRX_RPS_MX_UNIT_CLEARED(FN) = 1
                  ENDIF
                  IF(CUMULATIVE_MWH >= PROG_REQ .AND. &
                                              .NOT. RPS_CROSS_OVER) THEN
                     RPS_CROSS_OVER = .TRUE.
                     IF(RPS_CURVE_REPORT) THEN
                        VN_ANNUAL_ALT_REC = RPTREC(VN_ANNUAL_ALT_NO)
                        WRITE(VN_ANNUAL_ALT_NO,REC=VN_ANNUAL_ALT_REC) &
                           PRT_ENDPOINT(), &
                           FLOAT(THIS_YEAR), &
                           RPS_NAME, &
                           'REC PRICE                ', &
                           PA_BID(K), &
                           PA_CUM_MWH(K), &
                           PA_REVENUE(K), &
                           PA_COST(K), &
                           PROG_REQ, &
                           FLOAT(K), &
                           TEMP_MWH, &
                           RPS_REC_PRICE, &
                           RPS_GEN_WEIGHTED_PRICE
                        VN_ANNUAL_ALT_REC = VN_ANNUAL_ALT_REC + 1
                     ENDIF ! YES_RPS_CURVE_REPORT
                  ENDIF
                  IF(RPS_CURVE_REPORT) THEN
                     VN_ANNUAL_ALT_REC = RPTREC(VN_ANNUAL_ALT_NO)
                     WRITE(VN_ANNUAL_ALT_NO,REC=VN_ANNUAL_ALT_REC) &
                        PRT_ENDPOINT(), &
                        FLOAT(THIS_YEAR), &
                        RPS_NAME, &
                        CURVE_NAME, &
                        PA_BID(K), &
                        PA_CUM_MWH(K), &
                        PA_REVENUE(K), &
                        PA_COST(K), &
                        PROG_REQ, &
                        FLOAT(K), &
                        TEMP_MWH, &
                        RPS_REC_PRICE, &
                        RPS_GEN_WEIGHTED_PRICE
                     VN_ANNUAL_ALT_REC = VN_ANNUAL_ALT_REC + 1
                  ENDIF ! YES_RPS_CURVE_REPORT
                  IF(I+U+D < 1 .AND. .NOT. RPS_CROSS_OVER) THEN
                     RPS_REC_PRICE = RPS_ALT_COMPLIANCE_PRICE
                     IF(RPS_CURVE_REPORT) THEN
                        VN_ANNUAL_ALT_REC = RPTREC(VN_ANNUAL_ALT_NO)
                        WRITE(VN_ANNUAL_ALT_NO,REC=VN_ANNUAL_ALT_REC) &
                           PRT_ENDPOINT(), &
                           FLOAT(THIS_YEAR), &
                           RPS_NAME, &
                           'REC PRICE                ', &
                           PA_BID(K), &
                           PA_CUM_MWH(K), &
                           PA_REVENUE(K), &
                           PA_COST(K), &
                           PROG_REQ, &
                           FLOAT(K), &
                           TEMP_MWH, &
                           RPS_REC_PRICE, &
                           RPS_GEN_WEIGHTED_PRICE
                        VN_ANNUAL_ALT_REC = VN_ANNUAL_ALT_REC + 1
                     ENDIF ! YES_RPS_CURVE_REPORT
                     EXIT
                  ENDIF
               ENDDO ! I OR U OR D
            ENDDO ! RPS_PROGRAM
         ENDIF ! RPS ONLY SWITCH)
!
!
! 062120. END RPS CURVE ROUTINE HERE.
!
!
         IF(R_YEAR == 4) THEN
            R_YEAR = R_YEAR
         ENDIF
         CM_IOTA = 0.0
         CM_TOT_ANN_CAPACITY_ADDED = 0.
         IF(CX_ACTIVE) THEN
           call write_trace_int2s(file_trace_amoc,&
           "8. SORTED_OPTIONS", sorted_options(:TOTAL_ACTIVE_INSTANCES)) 
           
            SORTED_OPTIONS = TEMP_INSTANCE
         ENDIF
!
         IF( (GRX_RPS_MODULE_ACTIVE .OR. RPS_ONLY_SWITCH_ACTIVE) &
                                            .AND. RPS_PROGRAMS > 0) THEN
            DO I = HO, 1, -1
               NEXT_CN = SORTED_OPTIONS(I)
               HIGHEST_OPTION = HGST_OPT_INDEX(NEXT_CN)
       call write_trace_int2(file_trace_amoc, "7. HO", HIGHEST_OPTION)
!
               IF(.NOT. DEPENDENT_UNIT_AVAILABLE(HIGHEST_OPTION)) CYCLE
               RPS_NO = RPS_PROGRAM_NUMBER_loc(HIGHEST_OPTION)
               IF(RPS_NO /= 0) THEN
                  RPS_COUNT = 1
                  RPS_NO = GET_RPS_PROGRAM_POSITION(RPS_NO,RPS_COUNT)
                  IF(RPS_NO /= 0) THEN
                     RPS_REVENUE_MM(NEXT_CN) = &
                           RPS_REC_PRICE_BY(RPS_NO) * &
                                0.001 * HIGHEST_MRX_ENERGY(NEXT_CN) * &
                                             RPS_PERCENT(HIGHEST_OPTION)
                     IF(SCREEN_CAPACITY(HIGHEST_OPTION) > 0.01) THEN
                        HGST_NET_MARG_ADDED(NEXT_CN) = &
                             1000.0 * &
                                (ENERGY_REVENUE_MM(NEXT_CN) + &
                                   RPS_REVENUE_MM(NEXT_CN) + &
                                     CAPACITY_REVENUE_MM(NEXT_CN) - &
                                      VARIABLE_COST_MM(NEXT_CN) - &
                                       FIXED_COST_MM(NEXT_CN))/ &
                                         SCREEN_CAPACITY(HIGHEST_OPTION)
         call write_trace_real4(file_trace_amoc, "14. HGMA", &
            HGST_NET_MARG_ADDED(NEXT_CN))
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
        call write_sic_trace("cap_objt:0074", HO)
            CALL SortIncrPos(             HO, &
                                          SORTED_OPTIONS, &
                                          HGST_NET_MARG_ADDED)
         call write_trace_real4s(file_trace_amoc, "16. HGMA", &
            HGST_NET_MARG_ADDED)
         ENDIF ! RPS REVENUE
!
         TEMP_L = RESET_MRX_RPS_TEST_REQ()
!
         TOTAL_ANNUAL_CAPACITY_ADDED = 0.
         CM_TOT_ANN_CAPACITY_ADDED = 0.
         TG_TOTAL_ANNUAL_CAPACITY_ADDED = 0.
         call write_trace_int2s(file_trace_amoc, "SORTED_OPTIONS", &
            SORTED_OPTIONS(:HO))
         DO I = HO, 1, -1
!
! MAXIMUM RESERVE MARGIN CONDITION
!
            NEXT_CN = SORTED_OPTIONS(I)
            HIGHEST_OPTION = HGST_OPT_INDEX(NEXT_CN)
       call write_trace_int2(file_trace_amoc, "8. HO", HIGHEST_OPTION)
!
            IF(.NOT. DEPENDENT_UNIT_AVAILABLE(HIGHEST_OPTION)) CYCLE

!
            TG = GET_TRANS_GROUP_POSITION( &
                                      ns_cap_trans%transaction_group(HIGHEST_OPTION))
            call write_trace_int2(file_trace_amoc, "7. TG", TG)
            PA = TG_2_PLANNING_AREA(TG)
            CM = TG_2_CAPACITY_MARKET(TG)
            CM_INDEX = GET_CM_INDEX_FROM_TG(CM)
!
! 042710. RECALCULATE highest net margin added BASED ON CURRENT ICAP
! 021819. TEST ADDING RPS_REVENUE
!
           IF(TG_MW_TRANSFERS) THEN
               TG_CAPACITY_MW(TG) = &
                           TG_PLANNING_CAPACITY(TG) + &
                             TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG) + & ! -
                                          TG_Net_Transfer(TG)
            ELSE
               TG_CAPACITY_MW(TG) = &
                           TG_PLANNING_CAPACITY(TG) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG)
            ENDIF
            IF(TG_PEAK_MW(TG) > .01) THEN
               TG_RESERVE_MARGIN(TG) = 100. * &
                              (TG_CAPACITY_MW(TG)/TG_PEAK_MW(TG) - 1.0)
            ELSE
               TG_RESERVE_MARGIN(TG) = 100.
            ENDIF
            IF(TEST_CAP_VALUE_W_ADD) THEN
               TEMP_R4 = CAPACITY_PLANNING_MW(HIGHEST_OPTION)
            ELSE
               TEMP_R4 = 0.0
            ENDIF
            IF(TG_MW_TRANSFERS) THEN
               CM_CAPACITY_MW(CM) = &
                           CM_PLANNING_CAPACITY(CM) + &
                                 CM_TOT_ANN_CAPACITY_ADDED(CM) + &
                                    CM_Net_Transfer(CM) + &
                                       TEMP_R4
                 call write_trace_real4(file_trace_mxa, "12. CM_CAP_MW", &
                    CM_CAPACITY_MW(CM))
            ELSE
               CM_CAPACITY_MW(CM) = &
                           CM_PLANNING_CAPACITY(CM) + &
                                 CM_TOT_ANN_CAPACITY_ADDED(CM) + &
                                       TEMP_R4
                 call write_trace_real4(file_trace_mxa, "11. CM_CAP_MW", &
                    CM_CAPACITY_MW(CM))
            ENDIF
!
            IF(CM_PEAK_MW(CM) > .01) THEN
               CM_RESERVE_MARGIN(CM) = 100. * &
                              (CM_CAPACITY_MW(CM)/CM_PEAK_MW(CM) - 1.0)
               CM_INTERUPTIBLRESERVE_MARGIN = 100. * &
                         (CM_CAPACITY_MW(CM) - &
                                    CM_PEAK_AFTER_INTERRUPTIBLE(CM)) / &
                                CM_PEAK_AFTER_INTERRUPTIBLE(CM)
            ELSE
               CM_RESERVE_MARGIN(CM) = 100.
            ENDIF
            TEMP_R4 = CM_RESERVE_MARGIN(CM)*0.01 + 1.0
            TEMP_R4 = CM_INTERUPTIBLRESERVE_MARGIN*0.01 + 1.0
            
               if(R_YEAR==2 .and. CM==1) then
                    r_YEAR=r_YEAR ! Debugstop
               end if
            ICAP_DEMAND_CURVE_MULT(NEXT_CN) = &
               GET_ICAP_REVENUE_MULT(R_YEAR,CM,TEMP_R4)
!
            IF(SCREEN_CAPACITY(HIGHEST_OPTION) > 0.01) THEN
              IF(ICAP_SWITCH_INDEX(CM) == 2) THEN
                 CONE_COST_PER_KWYR(NEXT_CN) = &
                                             LOWEST_FIXED_COST_BY_CM(CM)
              ELSE
                 CONE_COST_PER_KWYR(NEXT_CN) = ICAP_CONE_PRICE(CM)
              ENDIF
              CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN) = &
                      CONE_COST_PER_KWYR(NEXT_CN) * &
                                    ICAP_DEMAND_CURVE_MULT(NEXT_CN)

               IF(ICAP_SWITCH_INDEX(CM) == 2 .OR.   & !  MARGINAL
                                        ICAP_SWITCH_INDEX(CM) == 3) THEN
                                        ! CONE-MRX
                  CAPACITY_REVENUE_MM(NEXT_CN) = &
                         MIN(CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN) * &
                           CAPACITY_PLANNING_MW(HIGHEST_OPTION)*0.001, &
                                MAX(0., &
                                 FIXED_COST_MM(NEXT_CN) + &
                                    VARIABLE_COST_MM(NEXT_CN) - &
                                       RPS_REVENUE_MM(NEXT_CN) - &
                                         ENERGY_REVENUE_MM(NEXT_CN)))
                  ICAP_REVENUE_PER_KWYR(NEXT_CN) = &
                          MAX(0.0,CAPACITY_REVENUE_MM(NEXT_CN)*1000.0/ &
                                        SCREEN_CAPACITY(HIGHEST_OPTION))
        call write_trace_int2(file_trace_amoc, "NEXT_CN", NEXT_CN)
        call write_trace_real4(file_trace_amoc, &
            "8. ICAP_REVENUE_PER_KWYR", &
            ICAP_REVENUE_PER_KWYR(NEXT_CN))
               ELSE
                  ICAP_REVENUE_PER_KWYR(NEXT_CN) = &
                           MIN(ICAP_CONE_PRICE(CM), &
                                CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN))
        call write_trace_int2(file_trace_amoc, "NEXT_CN", NEXT_CN)
        call write_trace_real4(file_trace_amoc, &
            "9. ICAP_REVENUE_PER_KWYR", &
            ICAP_REVENUE_PER_KWYR(NEXT_CN))
                  CAPACITY_REVENUE_MM(NEXT_CN) = &
                       ICAP_REVENUE_PER_KWYR(NEXT_CN) * &
                            CAPACITY_PLANNING_MW(HIGHEST_OPTION)*0.001
               ENDIF
!
               EAS_PROFIT_PER_KWYR(NEXT_CN) = 0.001 * &
                        1000000. * (ENERGY_REVENUE_MM(NEXT_CN) - &
                                           VARIABLE_COST_MM(NEXT_CN))/ &
                                         SCREEN_CAPACITY(HIGHEST_OPTION)
               EAS_PROFIT_PER_KWYR(NEXT_CN) = MAX(0.0, &
                             CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN) - &
                                         ICAP_REVENUE_PER_KWYR(NEXT_CN))
            ELSE
               EAS_PROFIT_PER_KWYR(NEXT_CN) = 0.0
               CONE_COST_PER_KWYR(NEXT_CN) = 0.0
               CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN) = 0.0
               ICAP_REVENUE_PER_KWYR(NEXT_CN) = 0.0
        call write_trace_int2(file_trace_amoc, "NEXT_CN", NEXT_CN)
        call write_trace_real4(file_trace_amoc, &
            "10. ICAP_REVENUE_PER_KWYR", &
            ICAP_REVENUE_PER_KWYR(NEXT_CN))
               CAPACITY_REVENUE_MM(NEXT_CN) = 0.0
               RPS_REVENUE_MM(NEXT_CN) = 0.0
            ENDIF
            HIGHEST_MARKET_REVENUE(NEXT_CN) = &
                                ENERGY_REVENUE_MM(NEXT_CN) + &
                                   RPS_REVENUE_MM(NEXT_CN) + &
                                     CAPACITY_REVENUE_MM(NEXT_CN)
            NET_MARGIN_PER_KWYR(HIGHEST_OPTION) = 1000.0 * &
                              (HIGHEST_MARKET_REVENUE(NEXT_CN) - &
                                 VARIABLE_COST_MM(NEXT_CN) - &
                                       FIXED_COST_MM(NEXT_CN))/ &
                                         SCREEN_CAPACITY(HIGHEST_OPTION)
!
            IF( ABS(NET_MARGIN_PER_KWYR(HIGHEST_OPTION)) < 0.01) THEN
               HGST_NET_MARG_ADDED(NEXT_CN) = 0.0
               HGST_NET_MARG_ADDED(NEXT_CN) = &
                      NET_MARGIN_PER_KWYR(HIGHEST_OPTION) + &
                                                   CM_IOTA(CM)
         call write_trace_real4(file_trace_amoc, "17. HGMA", &
            HGST_NET_MARG_ADDED(NEXT_CN))
               CM_IOTA(CM) = CM_IOTA(CM) - 0.001
            ELSE
               HGST_NET_MARG_ADDED(NEXT_CN) = &
                      NET_MARGIN_PER_KWYR(HIGHEST_OPTION)
         call write_trace_real4(file_trace_amoc, "18. HGMA", &
            HGST_NET_MARG_ADDED(NEXT_CN))
            ENDIF
            TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG) = &
                           TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG) + &
                                    CAPACITY_PLANNING_MW(HIGHEST_OPTION)
            IF(CM > 0) THEN
               CM_TOT_ANN_CAPACITY_ADDED(CM) = &
                           CM_TOT_ANN_CAPACITY_ADDED(CM) + &
                                    CAPACITY_PLANNING_MW(HIGHEST_OPTION)
            ENDIF
!
            HIGHEST_MARGIN_PER_KWYR(NEXT_CN) = &
                                       HGST_NET_MARG_ADDED(NEXT_CN)
!
            IF(MARGINAL_ICAP_PER_KWYR_BY_TG(TG) > 999998.0) THEN
               MARGINAL_ICAP_PER_KWYR_BY_TG(TG) = &
                                          ICAP_REVENUE_PER_KWYR(NEXT_CN)
            ENDIF
!
            TEMP_R = HIGHEST_MRX_ENERGY(NEXT_CN)
                call write_trace_real(file_trace_amoc, "13. TEMP_R", &
                        TEMP_R)
            IF( TEMP_R > 0.0) THEN
               REVENUE_PER_MWH(NEXT_CN) = &
                          HIGHEST_MARKET_REVENUE(NEXT_CN)*1000.0/TEMP_R
               CAPACITY_REVENUE_PER_MWH(NEXT_CN) = &
                             CAPACITY_REVENUE_MM(NEXT_CN)*1000.0/TEMP_R
               HIGHEST_MRX_MARGIN(NEXT_CN) = 1000.0 * &
                              (HIGHEST_MARKET_REVENUE(NEXT_CN) - &
                                 VARIABLE_COST_MM(NEXT_CN) - &
                                        FIXED_COST_MM(NEXT_CN))/TEMP_R
               RPS_REVENUE_PER_MWH(NEXT_CN) = &
                               1000.0 * RPS_REVENUE_MM(NEXT_CN)/TEMP_R
            ELSE
               REVENUE_PER_MWH(NEXT_CN) = 0.0
               CAPACITY_REVENUE_PER_MWH(NEXT_CN) = 0.0
               HIGHEST_MRX_MARGIN(NEXT_CN) = 0.0
            ENDIF
!
            IF(MX_SUMMARY_REPORT) THEN
               TEMP_I2 = GET_REGIONAL_CM_NAME(CM,CM_NAME)
               call write_trace_string(file_trace_amoc, "CAPName", &
                "CAP_//" // CM_NAME(1:31))
                
               WRITE(CM_NAME,1010) 'CAP_'//CM_NAME(1:31)
              
               WRITE(LOCAL_NAME,1010) &
                            OPTION_NAME(HIGHEST_OPTION)(1:17),HO - I + 1
                            
              call write_trace_string(file_trace_amoc, &
                "5. LOCAL_NAME", LOCAL_NAME)


               TEMP_R4 = 20
               MX_ANNUAL_ALT_REC = RPTREC(MX_ANNUAL_ALT_NO)
             call write_trace_message(file_trace_mxa, "3. WRITE: ")
            if (file_trace_mxa/=BAD_TRACE_HANDLE) then
                    write(file_trace_mxa,*) &
      "MX_ANNUAL_ALT_REC ", MX_ANNUAL_ALT_REC, &
      "THIS_YEAR ", FLOAT(THIS_YEAR), &
      "CM_NAME ", CM_NAME, &
      "LOCAL_NAME ", LOCAL_NAME, &
      "HIGHEST_STRIKE_PRICE ", HIGHEST_STRIKE_PRICE(NEXT_CN), &
      "HIGHEST_ENERGY_REV_PER_MWH ", &
        HIGHEST_ENERGY_REV_PER_MWH(NEXT_CN), & 
      "HIGHEST_MRX_MARGIN ", HIGHEST_MRX_MARGIN(NEXT_CN), &
      "HIGHEST_MRX_CF ", HIGHEST_MRX_CF(NEXT_CN), &
      "ENERGY_REVENUE_MM ", ENERGY_REVENUE_MM(NEXT_CN), &
      "HIGHEST_MARKET_COST ", HIGHEST_MARKET_COST(NEXT_CN), & !  6
      "VARIABLE_COST_MM ", VARIABLE_COST_MM(NEXT_CN), &
      "FIXED_COST_MM ", FIXED_COST_MM(NEXT_CN), &
      "SCREEN_CAPACITY ", SCREEN_CAPACITY(HIGHEST_OPTION), &
      "HIGHEST_MRX_ENERGY ", HIGHEST_MRX_ENERGY(NEXT_CN), & !  10
      "TOTAL_AVERAGE_COST ", TOTAL_AVERAGE_COST, & !  NEED DEFINITION
      "HIGHEST_ANN_LEVEL_CAP ", HIGHEST_ANN_LEVEL_CAP(NEXT_CN), &
      "HIGHEST_FUEL_COST ", HIGHEST_FUEL_COST(NEXT_CN), &
      "HIGHEST_ANNUAL_UNITS ", HIGHEST_ANNUAL_UNITS(NEXT_CN), &
      "CAPACITY_REVENUE_MM ", CAPACITY_REVENUE_MM(NEXT_CN), &
      "HIGHEST_MARKET_REVENUE ", HIGHEST_MARKET_REVENUE(NEXT_CN), & 
      "HIGHEST_MARGIN_PER_KWYR ", HIGHEST_MARGIN_PER_KWYR(NEXT_CN), &
      "HIGHEST_FUEL_COST_PER_MWH ",HIGHEST_FUEL_COST_PER_MWH(NEXT_CN), &
      "HIGHEST_VAR_OM_PER_MWH ", &
      HIGHEST_VAR_OM_PER_MWH(NEXT_CN), & !  19
      "HIGHEST_FUEL_COST_PER_MMBTU ", &
      HIGHEST_FUEL_COST_PER_MMBTU(NEXT_CN), &
      "REVENUE_PER_MWH ", REVENUE_PER_MWH(NEXT_CN), &
      "CAPACITY_REVENUE_PER_MWH ", CAPACITY_REVENUE_PER_MWH(NEXT_CN), &
      "HIGHEST_EMISSIONS_PER_MWH ", HIGHEST_EMISSIONS_PER_MWH(NEXT_CN), &
      "HIGHEST_FOM_PER_MWH ", HIGHEST_FOM_PER_MWH(NEXT_CN), & !  24
      "HIGHEST_LEVEL_CAP_PER_MWH ", HIGHEST_LEVEL_CAP_PER_MWH(NEXT_CN), &
      "HIGHEST_VOM_COST_MM ", HIGHEST_VOM_COST_MM(NEXT_CN), &
      "HIGHEST_EMISSIONS_COST_MM ", HIGHEST_EMISSIONS_COST_MM(NEXT_CN), & !  27
      "HIGHEST_FOM_COST_MM ", HIGHEST_FOM_COST_MM(NEXT_CN), &
      "TRANSACTION_GROUP ", FLOAT(ns_cap_trans%transaction_group(HIGHEST_OPTION)), &
      "TG_CAPACITY_MW ", TG_CAPACITY_MW(TG), &
      "TG_PEAK_MW ", TG_PEAK_MW(TG), &
      "TG_RESERVE_MARGIN ", TG_RESERVE_MARGIN(TG), &
      "CM_INDEX ", FLOAT(CM_INDEX), & 
      "CM_CAPACITY_MW ", CM_CAPACITY_MW(CM), &
      "CM_PEAK_MW ", CM_PEAK_MW(CM), &
      "CM_RESERVE_MARGIN ", CM_RESERVE_MARGIN(CM), &
      "ICAP_DEMAND_CURVE_MULT ", ICAP_DEMAND_CURVE_MULT(NEXT_CN), &
      "CONE_COST_PER_KWYR ", CONE_COST_PER_KWYR(NEXT_CN), & !  38
      "CONE_DEMAND_CURVE_ADJ_PER_KWYR ", &
      CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN), &
      "EAS_PROFIT_PER_KWYR ", EAS_PROFIT_PER_KWYR(NEXT_CN), &
      "ICAP_REVENUE_PER_KWYR ", ICAP_REVENUE_PER_KWYR(NEXT_CN), &
      "HIGHEST_HEAT_RATE ", HIGHEST_HEAT_RATE(NEXT_CN), & !  42
      "FIXED_COST_PER_MWH ", FIXED_COST_PER_MWH(NEXT_CN), &
      "HIGHEST_RFT ", HIGHEST_RFT(NEXT_CN), &
      "HIGHEST_RPM ", HIGHEST_RPM(NEXT_CN), &
      "TG_Net_Transfer ", TG_Net_Transfer(TG), &
      "CM_Net_Transfer ", CM_Net_Transfer(CM), &
      "TEMP_R4 ", TEMP_R4, & !  MRX_ICAP_INDEX(NEXT_CN),
      "CM_INTERUPTIBLRESERVE_MARGIN ", CM_INTERUPTIBLRESERVE_MARGIN, &
      "CM_PEAK_AFTER_INTERRUPTIBLE ", CM_PEAK_AFTER_INTERRUPTIBLE(CM), &
      "CAPACITY_PLANNING_MW ", CAPACITY_PLANNING_MW(HIGHEST_OPTION), &
      "RPS_REVENUE_MM ", RPS_REVENUE_MM(NEXT_CN), &
      "RPS_REVENUE_PER_MWH ", RPS_REVENUE_PER_MWH(NEXT_CN), " "
            end if
                     ! TODO: Combined writes to one in separate routine
               WRITE(MX_ANNUAL_ALT_NO,REC=MX_ANNUAL_ALT_REC) &
                        PRT_ENDPOINT(), &
                        FLOAT(THIS_YEAR), &
                        CM_NAME, &
                        LOCAL_NAME, &
                        HIGHEST_STRIKE_PRICE(NEXT_CN), &
                        HIGHEST_ENERGY_REV_PER_MWH(NEXT_CN), & 
                        ! 110309. REDEFINED.
                        HIGHEST_MRX_MARGIN(NEXT_CN), &
                        HIGHEST_MRX_CF(NEXT_CN), &
                        ENERGY_REVENUE_MM(NEXT_CN), &
                        HIGHEST_MARKET_COST(NEXT_CN), & !  6
                        VARIABLE_COST_MM(NEXT_CN), &
                        FIXED_COST_MM(NEXT_CN), &
                        SCREEN_CAPACITY(HIGHEST_OPTION), &
                        HIGHEST_MRX_ENERGY(NEXT_CN), & !  10
                        TOTAL_AVERAGE_COST, & !  NEED DEFINITION
                        HIGHEST_ANN_LEVEL_CAP(NEXT_CN), &
                        HIGHEST_FUEL_COST(NEXT_CN), &
                        HIGHEST_ANNUAL_UNITS(NEXT_CN), &
                        CAPACITY_REVENUE_MM(NEXT_CN), &
                        HIGHEST_MARKET_REVENUE(NEXT_CN), & 
                        ! 16 TOTAL REVENUE
                        HIGHEST_MARGIN_PER_KWYR(NEXT_CN), &
                        HIGHEST_FUEL_COST_PER_MWH(NEXT_CN), &
                        HIGHEST_VAR_OM_PER_MWH(NEXT_CN), & !  19
                        HIGHEST_FUEL_COST_PER_MMBTU(NEXT_CN), &
                        REVENUE_PER_MWH(NEXT_CN), &
                        CAPACITY_REVENUE_PER_MWH(NEXT_CN), &
                        HIGHEST_EMISSIONS_PER_MWH(NEXT_CN), &
                        HIGHEST_FOM_PER_MWH(NEXT_CN), & !  24
                        HIGHEST_LEVEL_CAP_PER_MWH(NEXT_CN), &
                        HIGHEST_VOM_COST_MM(NEXT_CN), &
                        HIGHEST_EMISSIONS_COST_MM(NEXT_CN), & !  27
                        HIGHEST_FOM_COST_MM(NEXT_CN), &
                        FLOAT(ns_cap_trans%transaction_group(HIGHEST_OPTION)), &
                        TG_CAPACITY_MW(TG), &
                        TG_PEAK_MW(TG), &
                        TG_RESERVE_MARGIN(TG), &
                        FLOAT(CM_INDEX), & !  PLANNING_AREA_ID(NEXT_CN),
                        CM_CAPACITY_MW(CM), &
                        CM_PEAK_MW(CM), &
                        CM_RESERVE_MARGIN(CM), &
                        ICAP_DEMAND_CURVE_MULT(NEXT_CN), &
                        CONE_COST_PER_KWYR(NEXT_CN), & !  38
                        CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN), &
                        EAS_PROFIT_PER_KWYR(NEXT_CN), &
                        ICAP_REVENUE_PER_KWYR(NEXT_CN), &
                        HIGHEST_HEAT_RATE(NEXT_CN), & !  42
                        FIXED_COST_PER_MWH(NEXT_CN), &
                        HIGHEST_RFT(NEXT_CN), &
                        HIGHEST_RPM(NEXT_CN), &
                        TG_Net_Transfer(TG), &
                        CM_Net_Transfer(CM), &
                        TEMP_R4, & !  MRX_ICAP_INDEX(NEXT_CN),
                        CM_INTERUPTIBLRESERVE_MARGIN, &
                        CM_PEAK_AFTER_INTERRUPTIBLE(CM), &
                        CAPACITY_PLANNING_MW(HIGHEST_OPTION), &
                        RPS_REVENUE_MM(NEXT_CN), &
                        RPS_REVENUE_PER_MWH(NEXT_CN)
               MX_ANNUAL_ALT_REC = MX_ANNUAL_ALT_REC + 1
!
            ENDIF
         ENDDO ! HO LIST ! 051210. BIG TEST
        call write_sic_trace("cap_objt:0075", HO)

         CALL SortIncrPos(                HO, &
                                          SORTED_OPTIONS, &
                                          HGST_NET_MARG_ADDED)
         call write_trace_real4s(file_trace_amoc, "19. HGMA", &
            HGST_NET_MARG_ADDED)
!
         TOTAL_ANNUAL_CAPACITY_ADDED = 0.
         CM_TOT_ANN_CAPACITY_ADDED = 0.
         TG_TOTAL_ANNUAL_CAPACITY_ADDED = 0.
!
! 082716. NEW RPS BUILD SECTION
! STATE_SURPLUS_VARS 0:6 WHERE ZERO IS TOTAL
! USED TO TRACK RPS ADDITIONS

      ! TODO: Remove code in IF block
      IF(1 == 2) THEN
      RPS_RESOURCE_ADDED = 0
      IF(GRX_RPS_MODULE_ACTIVE) THEN
         CALL GET_ANNUAL_STATE_SURPLUS_DATA(ZERO,SYSTEM_SURPLUS_VARS)
         CALL GET_ANNUAL_REGION_DATA( &
                                      NUM_RPS_REGIONS, &
                                      RPS_STATE_REGION, &
                                      RPS_REGION_POSITION)
         ST = 62
         REGION = 0
         RPS_SYSTEM = 0
         DO
            IF(ST > 1) THEN
               ST = ST - 1
               IF(RPS_STATE_REGION(ST)> 0) THEN
                  CYCLE
               ENDIF
               CALL GET_ANNUAL_STATE_SURPLUS_DATA(ST,STATE_SURPLUS_VARS)
            ELSE
               ST = 0
               IF(REGION < NUM_RPS_REGIONS) THEN
                  REGION = REGION + 1
                  CALL GET_ANNUAL_STATE_SURPLUS_DATA(-REGION, &
                                                     STATE_SURPLUS_VARS)
               ELSEIF(RPS_SYSTEM < 1) THEN
                  REGION = NUM_RPS_REGIONS + 1
                  RPS_SYSTEM = 1
                  STATE_SURPLUS_VARS = SYSTEM_SURPLUS_VARS
               ELSE
                  EXIT
               ENDIF
            ENDIF

            DO J = 7, 0, -1 ! DEFICIT RPS TO PRIM MOVER LIST (ZERO=ALL)
               IF(STATE_SURPLUS_VARS(J) > -0.0001) CYCLE ! SURPLUS
               RPS_PM_INDEX = J + 1
               RPS_PM_INDEX = RPS_INDEX_TRANSLATE(RPS_PM_INDEX)
               DO I = HO, 1, -1
                  IF(STATE_SURPLUS_VARS(J) > -0.0001) EXIT ! SURPLUS
                  NEXT_CN = SORTED_OPTIONS(I)
                  PM = HIGHEST_RPM(NEXT_CN)
                  HIGHEST_OPTION = HGST_OPT_INDEX(NEXT_CN)
       call write_trace_int2(file_trace_amoc, "9. HO", HIGHEST_OPTION)
                  OPTION_REGION = RPS_STATE_REGION( &
                                         STATE_PROVINCE(HIGHEST_OPTION))
                  OPTION_REGION = RPS_REGION_POSITION(OPTION_REGION)
                  IF( (J > 0 .AND. ABS(RPS_PM_INDEX - PM) > 0 ) .OR. &
                       (ST > 0 .AND. &
                    ABS(STATE_PROVINCE(HIGHEST_OPTION) - ST) > 0) .OR. &
                     (REGION > 0 .AND. REGION <= NUM_RPS_REGIONS .AND. &
                      ABS(OPTION_REGION - REGION) > 0) .OR. &
                       HIGHEST_MRX_ENERGY(NEXT_CN) * &
                              RPS_PERCENT(HIGHEST_OPTION) < 0.001 .OR. &
                                  RPS_RESOURCE_ADDED(NEXT_CN) > 0 .OR. &
                                     .NOT. IS_A_VALID_RPS_PM(PM) ) CYCLE
                  TG = GET_TRANS_GROUP_POSITION( &
                                      ns_cap_trans%transaction_group(HIGHEST_OPTION))
                  call write_trace_int2(file_trace_amoc, "8. TG", TG)
                  PA = TG_2_PLANNING_AREA(TG)
                  CM = TG_2_CAPACITY_MARKET(TG)
                  CM_INDEX = GET_CM_INDEX_FROM_TG(CM)
                  IF(TG_MW_TRANSFERS) THEN
                     TG_CAPACITY_MW(TG) = &
                           TG_PLANNING_CAPACITY(TG) + &
                              TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG) + & ! -
                                          TG_Net_Transfer(TG)
                  ELSE
                     TG_CAPACITY_MW(TG) = &
                           TG_PLANNING_CAPACITY(TG) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG)
                  ENDIF
                  IF(TG_PEAK_MW(TG) > .01) THEN
                     TG_RESERVE_MARGIN(TG) = 100. * &
                              (TG_CAPACITY_MW(TG)/TG_PEAK_MW(TG) - 1.0)
                  ELSE
                     TG_RESERVE_MARGIN(TG) = 100.
                  ENDIF
!
                  IF(TEST_CAP_VALUE_W_ADD) THEN
                     TEMP_R4 = CAPACITY_PLANNING_MW(HIGHEST_OPTION)
                  ELSE
                     TEMP_R4 = 0.0
                  ENDIF
                  IF(TG_MW_TRANSFERS) THEN
                     CM_CAPACITY_MW(CM) = &
                           CM_PLANNING_CAPACITY(CM) + &
                                 CM_TOT_ANN_CAPACITY_ADDED(CM) + &
                                    CM_Net_Transfer(CM) + &
                                       TEMP_R4
                 call write_trace_real4(file_trace_mxa, "9. CM_CAP_MW", &
                    CM_CAPACITY_MW(CM))
                  ELSE
                     CM_CAPACITY_MW(CM) = &
                           CM_PLANNING_CAPACITY(CM) + &
                              CM_TOT_ANN_CAPACITY_ADDED(CM) + &
                                       TEMP_R4
                 call write_trace_real4(file_trace_mxa, "8. CM_CAP_MW", &
                    CM_CAPACITY_MW(CM))
                  ENDIF
                  IF(CM_PEAK_MW(CM) > .01) THEN
                     CM_RESERVE_MARGIN(CM) = 100. * &
                              (CM_CAPACITY_MW(CM)/CM_PEAK_MW(CM) - 1.0)
                     CM_INTERUPTIBLRESERVE_MARGIN = 100. * &
                         (CM_CAPACITY_MW(CM) - &
                                    CM_PEAK_AFTER_INTERRUPTIBLE(CM)) / &
                                CM_PEAK_AFTER_INTERRUPTIBLE(CM)
                  ELSE
                     CM_RESERVE_MARGIN(CM) = 100.
                  ENDIF
!
                  FIRST_YEAR_CAPACITY = &
                     ADD_THIS_UNIT( &
                           OPTION_POSITION(HIGHEST_OPTION),THIS_YEAR + &
                                        LOCAL_LEAD_TIME(HIGHEST_OPTION))
         call write_trace_real(file_trace_amoc, "FIRST_YEAR_CAPACITY", &
      first_year_capacity)    
!
                  IF(INDEPENDENT(HIGHEST_OPTION) > 0) THEN
                     HYBRID_PROD_POINTER = INDEPENDENT(HIGHEST_OPTION)
                     FIRST_YEAR_CAPACITY = &
                        ADD_THIS_UNIT( &
                      OPTION_POSITION(HYBRID_PROD_POINTER),THIS_YEAR + &
                                   LOCAL_LEAD_TIME(HYBRID_PROD_POINTER))
         call write_trace_real(file_trace_amoc, "2. FIRST_YEAR_CAPACITY", &
      first_year_capacity)    
                  ENDIF
!
                  RPS_RESOURCE_ADDED(NEXT_CN) = 1
                  IF(MX_SUMMARY_REPORT) THEN
                     TEMP_I2 = GET_REGIONAL_CM_NAME(CM,CM_NAME)
                     IF(INDEPENDENT(HIGHEST_OPTION) > 0) THEN


                        WRITE(LOCAL_NAME,1017) &
                           OPTION_NAME(HIGHEST_OPTION)(1:15), &
                                                         ' H',HO - I + 1
              call write_trace_string(file_trace_amoc, &
                "6. LOCAL_NAME", LOCAL_NAME)                                                         
                                                         
                     ELSE


                        WRITE(LOCAL_NAME,1010) &
                            OPTION_NAME(HIGHEST_OPTION)(1:17),HO - I + 1
             call write_trace_string(file_trace_amoc, &
                "7. LOCAL_NAME", LOCAL_NAME)
                            
                     ENDIF
                     IF(ST == 0) THEN
                        MRX_ICAP_INDEX(NEXT_CN) = 10
                     ELSE
                        MRX_ICAP_INDEX(NEXT_CN) = 11
                     ENDIF
                     MX_ANNUAL_ALT_REC = RPTREC(MX_ANNUAL_ALT_NO)
             call write_trace_message(file_trace_mxa, "4. WRITE: ")
             if (file_trace_mxa/=BAD_TRACE_HANDLE) then
                    write(file_trace_mxa,*)  &
                   "MX_ANNUAL_ALT_REC ", MX_ANNUAL_ALT_REC, &
      "PRT_ENDPOINT ", PRT_ENDPOINT(), &
      "YEAR ", FLOAT(THIS_YEAR), &
      "CM_NAME ", CM_NAME, &
      "LOCAL_NAME ", LOCAL_NAME, &
      "HIGHEST_STRIKE_PRICE ", HIGHEST_STRIKE_PRICE(NEXT_CN), &
      "HIGHEST_ENERGY_REV_PER_MWH ",HIGHEST_ENERGY_REV_PER_MWH(NEXT_CN), & 
      "HIGHEST_MRX_MARGIN ", HIGHEST_MRX_MARGIN(NEXT_CN), &
      "HIGHEST_MRX_CF ", HIGHEST_MRX_CF(NEXT_CN), &
      "ENERGY_REVENUE_MM ", ENERGY_REVENUE_MM(NEXT_CN), &
      "HIGHEST_MARKET_COST ", HIGHEST_MARKET_COST(NEXT_CN), & !  6
      "VARIABLE_COST_MM ", VARIABLE_COST_MM(NEXT_CN), &
      "FIXED_COST_MM ", FIXED_COST_MM(NEXT_CN), &
      "SCREEN_CAPACITY ", SCREEN_CAPACITY(HIGHEST_OPTION), &
      "HIGHEST_MRX_ENERGY ", HIGHEST_MRX_ENERGY(NEXT_CN), & !  10
      "TOTAL_AVERAGE_COST ", TOTAL_AVERAGE_COST, & !  NEED DEFINITION
      "HIGHEST_ANN_LEVEL_CAP ", HIGHEST_ANN_LEVEL_CAP(NEXT_CN), &
      "HIGHEST_FUEL_COST ", HIGHEST_FUEL_COST(NEXT_CN), &
      "HIGHEST_ANNUAL_UNITS ", HIGHEST_ANNUAL_UNITS(NEXT_CN), &
      "CAPACITY_REVENUE_MM ", CAPACITY_REVENUE_MM(NEXT_CN), &
      "HIGHEST_MARKET_REVENUE ", HIGHEST_MARKET_REVENUE(NEXT_CN), & 
      "HIGHEST_MARGIN_PER_KWYR ", HIGHEST_MARGIN_PER_KWYR(NEXT_CN), &
      "HIGHEST_FUEL_COST_PER_MWH ",HIGHEST_FUEL_COST_PER_MWH(NEXT_CN), &
      "HIGHEST_VAR_OM_PER_MWH ", HIGHEST_VAR_OM_PER_MWH(NEXT_CN), & !  19
      "HIGHEST_FUEL_COST_PER_MMBTU ", &
        HIGHEST_FUEL_COST_PER_MMBTU(NEXT_CN), &
      "REVENUE_PER_MWH ", REVENUE_PER_MWH(NEXT_CN), &
      "CAPACITY_REVENUE_PER_MWH ", CAPACITY_REVENUE_PER_MWH(NEXT_CN), &
      "HIGHEST_EMISSIONS_PER_MWH ", HIGHEST_EMISSIONS_PER_MWH(NEXT_CN), &
      "HIGHEST_FOM_PER_MWH ", HIGHEST_FOM_PER_MWH(NEXT_CN), & !  24
      "HIGHEST_LEVEL_CAP_PER_MWH ", HIGHEST_LEVEL_CAP_PER_MWH(NEXT_CN), &
      "HIGHEST_VOM_COST_MM ", HIGHEST_VOM_COST_MM(NEXT_CN), &
      "HIGHEST_EMISSIONS_COST_MM ", HIGHEST_EMISSIONS_COST_MM(NEXT_CN), & 
      "HIGHEST_FOM_COST_MM ", HIGHEST_FOM_COST_MM(NEXT_CN), &
      "TRANSACTION_GROUP ", &
      FLOAT(ns_cap_trans%transaction_group(HIGHEST_OPTION)), &
      "TG_CAPACITY_MW ", TG_CAPACITY_MW(TG), &
      "TG_PEAK_MW ", TG_PEAK_MW(TG), &
      "TG_RESERVE_MARGIN ", TG_RESERVE_MARGIN(TG), &
      "CM_INDEX ", FLOAT(CM_INDEX), &
      "CM_CAPACITY_MW ", CM_CAPACITY_MW(CM), &
      "CM_PEAK_MW ", CM_PEAK_MW(CM), &
      "CM_RESERVE_MARGIN ", CM_RESERVE_MARGIN(CM), &
      "ICAP_DEMAND_CURVE_MULT ", ICAP_DEMAND_CURVE_MULT(NEXT_CN), &
      "CONE_COST_PER_KWYR ", CONE_COST_PER_KWYR(NEXT_CN), & !  38
      "CONE_DEMAND_CURVE_ADJ_PER_KWYR ", &
      CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN), &
      "EAS_PROFIT_PER_KWYR ", EAS_PROFIT_PER_KWYR(NEXT_CN), &
      "ICAP_REVENUE_PER_KWYR ", ICAP_REVENUE_PER_KWYR(NEXT_CN), &
      "HIGHEST_HEAT_RATE ", HIGHEST_HEAT_RATE(NEXT_CN), & !  42
      "FIXED_COST_PER_MWH ", FIXED_COST_PER_MWH(NEXT_CN), &
      "HIGHEST_RFT ", HIGHEST_RFT(NEXT_CN), &
      "HIGHEST_RPM ", HIGHEST_RPM(NEXT_CN), &
      "TG_Net_Transfer ", TG_Net_Transfer(TG), &
      "CM_Net_Transfer ", CM_Net_Transfer(CM), &
      "MRX_ICAP_INDEX ", MRX_ICAP_INDEX(NEXT_CN), &
      "CM_INTERUPTIBLRESERVE_MARGIN ", CM_INTERUPTIBLRESERVE_MARGIN, &
      "CM_PEAK_AFTER_INTERRUPTIBLE ", CM_PEAK_AFTER_INTERRUPTIBLE(CM), &
      "CAPACITY_PLANNING_MW ", CAPACITY_PLANNING_MW(HIGHEST_OPTION), &
      "RPS_REVENUE_MM ", RPS_REVENUE_MM(NEXT_CN), &
      "RPS_REVENUE_PER_MWH ", RPS_REVENUE_PER_MWH(NEXT_CN), " "
             end if
                     ! TODO: Combined writes to one in separate routine
                     WRITE(MX_ANNUAL_ALT_NO,REC=MX_ANNUAL_ALT_REC) &
                        PRT_ENDPOINT(), &
                        FLOAT(THIS_YEAR), &
                        CM_NAME, &
                        LOCAL_NAME, &
                        HIGHEST_STRIKE_PRICE(NEXT_CN), &
                        HIGHEST_ENERGY_REV_PER_MWH(NEXT_CN), & 
                        ! 110309. REDEFINED.
                        HIGHEST_MRX_MARGIN(NEXT_CN), &
                        HIGHEST_MRX_CF(NEXT_CN), &
                        ENERGY_REVENUE_MM(NEXT_CN), &
                        HIGHEST_MARKET_COST(NEXT_CN), & !  6
                        VARIABLE_COST_MM(NEXT_CN), &
                        FIXED_COST_MM(NEXT_CN), &
                        SCREEN_CAPACITY(HIGHEST_OPTION), &
                        HIGHEST_MRX_ENERGY(NEXT_CN), & !  10
                        TOTAL_AVERAGE_COST, & !  NEED DEFINITION
                        HIGHEST_ANN_LEVEL_CAP(NEXT_CN), &
                        HIGHEST_FUEL_COST(NEXT_CN), &
                        HIGHEST_ANNUAL_UNITS(NEXT_CN), &
                        CAPACITY_REVENUE_MM(NEXT_CN), &
                        HIGHEST_MARKET_REVENUE(NEXT_CN), & 
                        ! 16 TOTAL REVENUE
                        HIGHEST_MARGIN_PER_KWYR(NEXT_CN), &
                        HIGHEST_FUEL_COST_PER_MWH(NEXT_CN), &
                        HIGHEST_VAR_OM_PER_MWH(NEXT_CN), & !  19
                        HIGHEST_FUEL_COST_PER_MMBTU(NEXT_CN), &
                        REVENUE_PER_MWH(NEXT_CN), &
                        CAPACITY_REVENUE_PER_MWH(NEXT_CN), &
                        HIGHEST_EMISSIONS_PER_MWH(NEXT_CN), &
                        HIGHEST_FOM_PER_MWH(NEXT_CN), & !  24
                        HIGHEST_LEVEL_CAP_PER_MWH(NEXT_CN), &
                        HIGHEST_VOM_COST_MM(NEXT_CN), &
                        HIGHEST_EMISSIONS_COST_MM(NEXT_CN), & !  27
                        HIGHEST_FOM_COST_MM(NEXT_CN), &
                        FLOAT(ns_cap_trans%transaction_group(HIGHEST_OPTION)), &
                        TG_CAPACITY_MW(TG), &
                        TG_PEAK_MW(TG), &
                        TG_RESERVE_MARGIN(TG), &
                        FLOAT(CM_INDEX), & !  PLANNING_AREA_ID(NEXT_CN),
                        CM_CAPACITY_MW(CM), &
                        CM_PEAK_MW(CM), &
                        CM_RESERVE_MARGIN(CM), &
                        ICAP_DEMAND_CURVE_MULT(NEXT_CN), &
                        CONE_COST_PER_KWYR(NEXT_CN), & !  38
                        CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN), &
                        EAS_PROFIT_PER_KWYR(NEXT_CN), &
                        ICAP_REVENUE_PER_KWYR(NEXT_CN), &
                        HIGHEST_HEAT_RATE(NEXT_CN), & !  42
                        FIXED_COST_PER_MWH(NEXT_CN), &
                        HIGHEST_RFT(NEXT_CN), &
                        HIGHEST_RPM(NEXT_CN), &
                        TG_Net_Transfer(TG), &
                        CM_Net_Transfer(CM), &
                        MRX_ICAP_INDEX(NEXT_CN), &
                        CM_INTERUPTIBLRESERVE_MARGIN, &
                        CM_PEAK_AFTER_INTERRUPTIBLE(CM), &
                        CAPACITY_PLANNING_MW(HIGHEST_OPTION), &
                        RPS_REVENUE_MM(NEXT_CN), &
                        RPS_REVENUE_PER_MWH(NEXT_CN)
                     MX_ANNUAL_ALT_REC = MX_ANNUAL_ALT_REC + 1
                  ENDIF
!
!                 RECORD THREE CAPACITY ADDED VARIABLES
!
                  IF(PA> 0) THEN
                     TOTAL_ANNUAL_CAPACITY_ADDED(PA) = &
                           TOTAL_ANNUAL_CAPACITY_ADDED(PA) + &
                                    CAPACITY_PLANNING_MW(HIGHEST_OPTION)
                  ENDIF
                  TOTAL_ANNUAL_CAPACITY_ADDED(0) = &
                           TOTAL_ANNUAL_CAPACITY_ADDED(0) + &
                                    CAPACITY_PLANNING_MW(HIGHEST_OPTION)
                  IF(CM > 0) THEN
                     CM_TOT_ANN_CAPACITY_ADDED(CM) = &
                           CM_TOT_ANN_CAPACITY_ADDED(CM) + &
                                    CAPACITY_PLANNING_MW(HIGHEST_OPTION)
                  ENDIF
                  TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG) = &
                           TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG) + &
                                    CAPACITY_PLANNING_MW(HIGHEST_OPTION)
!
!                 OTHER VARIABLES
!
                  LAST_SORTED_POSITION = HIGHEST_OPTION
                  LAST_RESOURCE_ADDED(TG) = HIGHEST_OPTION
                  LAST_PA_RESOURCE_ADDED(PA) = NEXT_CN
                  LAST_CM_RESOURCE_ADDED(CM) = NEXT_CN
                  INSTALL_CAP_VALUE_LAST_PA_UNIT(PA) = &
                                HIGHEST_ANN_LEVEL_CAP(HIGHEST_OPTION)
                  INSTALL_CAP_VALUE_LAST_CM_UNIT(CM) = &
                                HIGHEST_ANN_LEVEL_CAP(HIGHEST_OPTION)
                  STATE_SURPLUS_VARS(J) = STATE_SURPLUS_VARS(J) + &
                                         HIGHEST_MRX_ENERGY(NEXT_CN)* &
                                           RPS_PERCENT(HIGHEST_OPTION)
                  IF(J > 0) THEN
                     STATE_SURPLUS_VARS(0) = STATE_SURPLUS_VARS(0) + &
                                          HIGHEST_MRX_ENERGY(NEXT_CN)* &
                                           RPS_PERCENT(HIGHEST_OPTION)
                  ENDIF
                  IF(ST > 0 .OR. (REGION > 0 .AND. &
                                       REGION <= NUM_RPS_REGIONS) ) THEN
                     SYSTEM_SURPLUS_VARS(J) = SYSTEM_SURPLUS_VARS(J) + &
                                          HIGHEST_MRX_ENERGY(NEXT_CN)* &
                                           RPS_PERCENT(HIGHEST_OPTION)
                     IF(J > 0) THEN
                        SYSTEM_SURPLUS_VARS(0) = &
                                  SYSTEM_SURPLUS_VARS(0) + &
                                          HIGHEST_MRX_ENERGY(NEXT_CN)* &
                                           RPS_PERCENT(HIGHEST_OPTION)
                     ENDIF
                  ENDIF
               ENDDO ! RESOURCE ADDITIONS
            ENDDO ! RPS TYPE
         ENDDO ! STATE AND ZERO
      ENDIF ! RPS MODULE ACTIVE
!
      ELSE
!
      IF(R_YEAR == 7) THEN
         J = J
      ENDIF
!
      RPS_RESOURCE_ADDED = 0
!
! 080120. RPS ONLY MUTUALLY EXCLUSIVE FROM GRX_RPS_MODULE_ACTIVE
! 081620. PER 8/14/20 MEETING, PASS SUPPLY CURVE INFORMATION.
!
      IF(RPS_ONLY_SWITCH_ACTIVE) THEN

         RPS_PROGRAMS = NUMBER_OF_RPS_PROGRAMS()
!
         DO J = 1, RPS_PROGRAMS
            RPS_REC_PRICE = RPS_REC_PRICE_BY(J)
            RPS_GEN_WEIGHTED_PRICE = RPS_GEN_PRICE_BY(J)
            TEMP_R = PUT_RPS_PRICE_BY_PROGRAM(J,RPS_REC_PRICE, &
                                            RPS_GEN_WEIGHTED_PRICE)
                call write_trace_real(file_trace_amoc, "14. TEMP_R", &
                        TEMP_R)
         ENDDO
         DO J = 1, RPS_PROGRAMS

            RPS_PROGRAM_BALANCE = &
                        GET_LOCAL_ENDING_BALANCE(R_YEAR,J,RPS_ALT_PRICE)
            DO I = HO, 1, -1

               NEXT_CN = SORTED_OPTIONS(I)
               PM = HIGHEST_RPM(NEXT_CN)
               HIGHEST_OPTION = HGST_OPT_INDEX(NEXT_CN)
       call write_trace_int2(file_trace_amoc, "10. HO", HIGHEST_OPTION)
               RPS_NO = RPS_PROGRAM_NUMBER_loc(HIGHEST_OPTION)
!
! 081920. REC PRICE FROM RPS SUPPLY CURVE FOR RPS ONLY.
! 081920. BEGIN
!
                  IF( .NOT. IS_IN_RPS_PROGRAM(J,RPS_NO)) CYCLE
                  IF(MRX_RPS_MX_UNIT_CLEARED(NEXT_CN) < 1 .OR. &
                                 RPS_RESOURCE_ADDED(NEXT_CN) == 1) THEN
                     CYCLE
                  ENDIF
! 013021.
                  IF(RPS_NO /= 0) THEN
                     RPS_COUNT = 1
                     RPS_NO = GET_RPS_PROGRAM_POSITION(RPS_NO,RPS_COUNT)
                     IF(RPS_NO /= 0) THEN
                        RPS_REC_PRICE = RPS_REC_PRICE_BY(RPS_NO)
                        RPS_GEN_WEIGHTED_PRICE = &
                                                RPS_GEN_PRICE_BY(RPS_NO)
                     ENDIF
                  ENDIF
                  RPS_REVENUE_PER_MWH(NEXT_CN) = RPS_REC_PRICE
                  RPS_REVENUE_MM(NEXT_CN) = 0.001 * RPS_REC_PRICE * &
                           HIGHEST_MRX_ENERGY(NEXT_CN) * &
                              RPS_PERCENT(HIGHEST_OPTION)
! 081920. END
                  TG = GET_TRANS_GROUP_POSITION( &
                     ns_cap_trans%transaction_group(HIGHEST_OPTION))
            call write_trace_int2(file_trace_amoc, "9. TG", TG)
                  PA = TG_2_PLANNING_AREA(TG)
                  CM = TG_2_CAPACITY_MARKET(TG)
                  CM_INDEX = GET_CM_INDEX_FROM_TG(CM)
                  IF(TG_MW_TRANSFERS) THEN
                     TG_CAPACITY_MW(TG) = &
                           TG_PLANNING_CAPACITY(TG) + &
                              TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG) + & ! -
                                          TG_Net_Transfer(TG)
                  ELSE
                     TG_CAPACITY_MW(TG) = &
                           TG_PLANNING_CAPACITY(TG) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG)
                  ENDIF
                  IF(TG_PEAK_MW(TG) > .01) THEN
                     TG_RESERVE_MARGIN(TG) = 100. * &
                              (TG_CAPACITY_MW(TG)/TG_PEAK_MW(TG) - 1.0)
                  ELSE
                     TG_RESERVE_MARGIN(TG) = 100.
                  ENDIF
!
                  IF(TEST_CAP_VALUE_W_ADD) THEN
                     TEMP_R4 = CAPACITY_PLANNING_MW(HIGHEST_OPTION)
                  ELSE
                     TEMP_R4 = 0.0
                  ENDIF
                  IF(TG_MW_TRANSFERS) THEN
                     CM_CAPACITY_MW(CM) = &
                           CM_PLANNING_CAPACITY(CM) + &
                                 CM_TOT_ANN_CAPACITY_ADDED(CM) + &
                                    CM_Net_Transfer(CM) + &
                                       TEMP_R4
                 call write_trace_real4(file_trace_mxa, "7. CM_CAP_MW", &
                    CM_CAPACITY_MW(CM))
                  ELSE
                     CM_CAPACITY_MW(CM) = &
                           CM_PLANNING_CAPACITY(CM) + &
                              CM_TOT_ANN_CAPACITY_ADDED(CM) + &
                                       TEMP_R4
                 call write_trace_real4(file_trace_mxa, "6. CM_CAP_MW", &
                    CM_CAPACITY_MW(CM))
                  ENDIF
                  IF(CM_PEAK_MW(CM) > .01) THEN
                     CM_RESERVE_MARGIN(CM) = 100. * &
                              (CM_CAPACITY_MW(CM)/CM_PEAK_MW(CM) - 1.0)
                     CM_INTERUPTIBLRESERVE_MARGIN = 100. * &
                         (CM_CAPACITY_MW(CM) - &
                                    CM_PEAK_AFTER_INTERRUPTIBLE(CM)) / &
                                CM_PEAK_AFTER_INTERRUPTIBLE(CM)
                  ELSE
                     CM_RESERVE_MARGIN(CM) = 100.
                  ENDIF
!
                  FIRST_YEAR_CAPACITY = &
                     ADD_THIS_UNIT( &
                           OPTION_POSITION(HIGHEST_OPTION),THIS_YEAR + &
                                        LOCAL_LEAD_TIME(HIGHEST_OPTION))
       call write_trace_real(file_trace_amoc, "3. FIRST_YEAR_CAPACITY", &
      first_year_capacity)    
                  RPS_RESOURCE_ADDED(NEXT_CN) = 1
!
                  TEMP_I2 = RPS_PROGRAM_NUMBER_loc(HIGHEST_OPTION)
                  INT2_PM = INT(HIGHEST_RPM(NEXT_CN),2)
                  TEMP_R = PUT_RPS_PRICE_BY_OPTION( &
                                 TEMP_I2, &
                                 RPS_REC_PRICE, &
                                 1000.0*HIGHEST_MRX_ENERGY(NEXT_CN), &
                                 SCREEN_CAPACITY(HIGHEST_OPTION), &
                                 INT2_PM, &
                                 R_YEAR, &
                                 GRX_RPS_MODULE_ACTIVE, &
                                 RPS_GEN_WEIGHTED_PRICE)
                call write_trace_real(file_trace_amoc, "29. TEMP_R", &
                        TEMP_R)
                  HIGHEST_MARKET_REVENUE(NEXT_CN) = &
                                ENERGY_REVENUE_MM(NEXT_CN) + &
                                   RPS_REVENUE_MM(NEXT_CN) + &
                                     CAPACITY_REVENUE_MM(NEXT_CN)
                  HIGHEST_MARGIN_PER_KWYR(NEXT_CN) = 1000.0 * &
                              (HIGHEST_MARKET_REVENUE(NEXT_CN) - &
                                 VARIABLE_COST_MM(NEXT_CN) - &
                                       FIXED_COST_MM(NEXT_CN))/ &
                                   SCREEN_CAPACITY(HIGHEST_OPTION)
                  TEMP_R = HIGHEST_MRX_ENERGY(NEXT_CN) * &
                              RPS_PERCENT(HIGHEST_OPTION)
                call write_trace_real(file_trace_amoc, "15. TEMP_R", &
                        TEMP_R)
                  IF( TEMP_R > 0.0) THEN
                     REVENUE_PER_MWH(NEXT_CN) = &
                          HIGHEST_MARKET_REVENUE(NEXT_CN)*1000.0/TEMP_R
                     CAPACITY_REVENUE_PER_MWH(NEXT_CN) = &
                             CAPACITY_REVENUE_MM(NEXT_CN)*1000.0/TEMP_R
                     HIGHEST_MRX_MARGIN(NEXT_CN) = 1000.0 * &
                              (HIGHEST_MARKET_REVENUE(NEXT_CN) - &
                                 VARIABLE_COST_MM(NEXT_CN) - &
                                        FIXED_COST_MM(NEXT_CN))/TEMP_R
                  ELSE
                     REVENUE_PER_MWH(NEXT_CN) = 0.0
                     CAPACITY_REVENUE_PER_MWH(NEXT_CN) = 0.0
                     HIGHEST_MRX_MARGIN(NEXT_CN) = 0.0
                  ENDIF
                  IF(MX_SUMMARY_REPORT) THEN
                     TEMP_I2 = GET_REGIONAL_CM_NAME(CM,CM_NAME)

                     WRITE(LOCAL_NAME,1010) &
                            OPTION_NAME(HIGHEST_OPTION)(1:17),HO - I + 1
                            
              call write_trace_string(file_trace_amoc, &
                "8. LOCAL_NAME", LOCAL_NAME)                            
                            

                        MRX_ICAP_INDEX(NEXT_CN) = 10
                     MX_ANNUAL_ALT_REC = RPTREC(MX_ANNUAL_ALT_NO)
             call write_trace_message(file_trace_mxa, "5. WRITE: ")
             if (file_trace_mxa/=BAD_TRACE_HANDLE) then
                    write(file_trace_mxa,*)  &
                   "MX_ANNUAL_ALT_REC ", MX_ANNUAL_ALT_REC, &
      "PRT_ENDPOINT ", PRT_ENDPOINT(), &
      "YEAR ", FLOAT(THIS_YEAR), &
      "CM_NAME ", CM_NAME, &
      "LOCAL_NAME ", LOCAL_NAME, &
      "HIGHEST_STRIKE_PRICE ", HIGHEST_STRIKE_PRICE(NEXT_CN), &
      "HIGHEST_ENERGY_REV_PER_MWH ", &
      HIGHEST_ENERGY_REV_PER_MWH(NEXT_CN), & 
      "HIGHEST_MRX_MARGIN ", HIGHEST_MRX_MARGIN(NEXT_CN), &
      "HIGHEST_MRX_CF ", HIGHEST_MRX_CF(NEXT_CN), &
      "ENERGY_REVENUE_MM ", ENERGY_REVENUE_MM(NEXT_CN), &
      "HIGHEST_MARKET_COST ", HIGHEST_MARKET_COST(NEXT_CN), & !  6
      "VARIABLE_COST_MM ", VARIABLE_COST_MM(NEXT_CN), &
      "FIXED_COST_MM ", FIXED_COST_MM(NEXT_CN), &
      "SCREEN_CAPACITY ", SCREEN_CAPACITY(HIGHEST_OPTION), &
      "HIGHEST_MRX_ENERGY ", HIGHEST_MRX_ENERGY(NEXT_CN), & !  10
      "TOTAL_AVERAGE_COST ", TOTAL_AVERAGE_COST, & !  NEED DEFINITION
      "HIGHEST_ANN_LEVEL_CAP ", HIGHEST_ANN_LEVEL_CAP(NEXT_CN), &
      "HIGHEST_FUEL_COST ", HIGHEST_FUEL_COST(NEXT_CN), &
      "HIGHEST_ANNUAL_UNITS ", HIGHEST_ANNUAL_UNITS(NEXT_CN), &
      "CAPACITY_REVENUE_MM ", CAPACITY_REVENUE_MM(NEXT_CN), &
      "HIGHEST_MARKET_REVENUE ", HIGHEST_MARKET_REVENUE(NEXT_CN) , & 
      "HIGHEST_MARGIN_PER_KWYR ", HIGHEST_MARGIN_PER_KWYR(NEXT_CN), &
      "HIGHEST_FUEL_COST_PER_MWH ", HIGHEST_FUEL_COST_PER_MWH(NEXT_CN), &
      "HIGHEST_VAR_OM_PER_MWH ", HIGHEST_VAR_OM_PER_MWH(NEXT_CN),  &
      "HIGHEST_FUEL_COST_PER_MMBTU ", &
      HIGHEST_FUEL_COST_PER_MMBTU(NEXT_CN), &
      "REVENUE_PER_MWH ", REVENUE_PER_MWH(NEXT_CN), &
      "CAPACITY_REVENUE_PER_MWH ", CAPACITY_REVENUE_PER_MWH(NEXT_CN), &
      "HIGHEST_EMISSIONS_PER_MWH ", HIGHEST_EMISSIONS_PER_MWH(NEXT_CN), &
      "HIGHEST_FOM_PER_MWH ", HIGHEST_FOM_PER_MWH(NEXT_CN), &
      "HIGHEST_LEVEL_CAP_PER_MWH ", HIGHEST_LEVEL_CAP_PER_MWH(NEXT_CN), &
      "HIGHEST_VOM_COST_MM ", HIGHEST_VOM_COST_MM(NEXT_CN), &
      "HIGHEST_EMISSIONS_COST_MM ", HIGHEST_EMISSIONS_COST_MM(NEXT_CN), & !  27
      "HIGHEST_FOM_COST_MM ", HIGHEST_FOM_COST_MM(NEXT_CN), &
      "TRANSACTION_GROUP ", FLOAT(ns_cap_trans%transaction_group(HIGHEST_OPTION)), &
      "TG_CAPACITY_MW ", TG_CAPACITY_MW(TG), &
      "TG_PEAK_MW ", TG_PEAK_MW(TG), &
      "TG_RESERVE_MARGIN ", TG_RESERVE_MARGIN(TG), &
      "CM_INDEX ", FLOAT(CM_INDEX), &
      "CM_CAPACITY_MW ", CM_CAPACITY_MW(CM), &
      "CM_PEAK_MW ", CM_PEAK_MW(CM), &
      "CM_RESERVE_MARGIN ", CM_RESERVE_MARGIN(CM), &
      "ICAP_DEMAND_CURVE_MULT ", ICAP_DEMAND_CURVE_MULT(NEXT_CN), &
      "CONE_COST_PER_KWYR ", CONE_COST_PER_KWYR(NEXT_CN), & !  38
      "CONE_DEMAND_CURVE_ADJ_PER_KWYR ", &
      CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN), &
      "EAS_PROFIT_PER_KWYR ", EAS_PROFIT_PER_KWYR(NEXT_CN), &
      "ICAP_REVENUE_PER_KWYR ", ICAP_REVENUE_PER_KWYR(NEXT_CN), &
      "HIGHEST_HEAT_RATE ", HIGHEST_HEAT_RATE(NEXT_CN), & 
      "FIXED_COST_PER_MWH ", FIXED_COST_PER_MWH(NEXT_CN), &
      "HIGHEST_RFT ", HIGHEST_RFT(NEXT_CN), &
      "HIGHEST_RPM ", HIGHEST_RPM(NEXT_CN), &
      "TG_Net_Transfer ", TG_Net_Transfer(TG), &
      "CM_Net_Transfer ", CM_Net_Transfer(CM), &
      "MRX_ICAP_INDEX ", MRX_ICAP_INDEX(NEXT_CN), &
      "CM_INTERUPTIBLRESERVE_MARGIN ", CM_INTERUPTIBLRESERVE_MARGIN, &
      "CM_PEAK_AFTER_INTERRUPTIBLE ", CM_PEAK_AFTER_INTERRUPTIBLE(CM), &
      "CAPACITY_PLANNING_MW ", CAPACITY_PLANNING_MW(HIGHEST_OPTION), &
      "RPS_REVENUE_MM ", RPS_REVENUE_MM(NEXT_CN), &
      "RPS_REVENUE_PER_MWH ", RPS_REVENUE_PER_MWH(NEXT_CN), " "
             end if
                     ! TODO: Combined writes to one in separate routine
                     WRITE(MX_ANNUAL_ALT_NO,REC=MX_ANNUAL_ALT_REC) &
                        PRT_ENDPOINT(), &
                        FLOAT(THIS_YEAR), &
                        CM_NAME, &
                        LOCAL_NAME, &
                        HIGHEST_STRIKE_PRICE(NEXT_CN), &
                        HIGHEST_ENERGY_REV_PER_MWH(NEXT_CN), & 
                        ! 110309. REDEFINED.
                        HIGHEST_MRX_MARGIN(NEXT_CN), &
                        HIGHEST_MRX_CF(NEXT_CN), &
                        ENERGY_REVENUE_MM(NEXT_CN), &
                        HIGHEST_MARKET_COST(NEXT_CN), & !  6
                        VARIABLE_COST_MM(NEXT_CN), &
                        FIXED_COST_MM(NEXT_CN), &
                        SCREEN_CAPACITY(HIGHEST_OPTION), &
                        HIGHEST_MRX_ENERGY(NEXT_CN), & !  10
                        TOTAL_AVERAGE_COST, & !  NEED DEFINITION
                        HIGHEST_ANN_LEVEL_CAP(NEXT_CN), &
                        HIGHEST_FUEL_COST(NEXT_CN), &
                        HIGHEST_ANNUAL_UNITS(NEXT_CN), &
                        CAPACITY_REVENUE_MM(NEXT_CN), &
                        HIGHEST_MARKET_REVENUE(NEXT_CN), & 
                        ! 16 TOTAL REVENUE
                        HIGHEST_MARGIN_PER_KWYR(NEXT_CN), &
                        HIGHEST_FUEL_COST_PER_MWH(NEXT_CN), &
                        HIGHEST_VAR_OM_PER_MWH(NEXT_CN), & !  19
                        HIGHEST_FUEL_COST_PER_MMBTU(NEXT_CN), &
                        REVENUE_PER_MWH(NEXT_CN), &
                        CAPACITY_REVENUE_PER_MWH(NEXT_CN), &
                        HIGHEST_EMISSIONS_PER_MWH(NEXT_CN), &
                        HIGHEST_FOM_PER_MWH(NEXT_CN), & !  24
                        HIGHEST_LEVEL_CAP_PER_MWH(NEXT_CN), &
                        HIGHEST_VOM_COST_MM(NEXT_CN), &
                        HIGHEST_EMISSIONS_COST_MM(NEXT_CN), & !  27
                        HIGHEST_FOM_COST_MM(NEXT_CN), &
                        FLOAT(ns_cap_trans%transaction_group(HIGHEST_OPTION)), &
                        TG_CAPACITY_MW(TG), &
                        TG_PEAK_MW(TG), &
                        TG_RESERVE_MARGIN(TG), &
                        FLOAT(CM_INDEX), & !  PLANNING_AREA_ID(NEXT_CN),
                        CM_CAPACITY_MW(CM), &
                        CM_PEAK_MW(CM), &
                        CM_RESERVE_MARGIN(CM), &
                        ICAP_DEMAND_CURVE_MULT(NEXT_CN), &
                        CONE_COST_PER_KWYR(NEXT_CN), & !  38
                        CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN), &
                        EAS_PROFIT_PER_KWYR(NEXT_CN), &
                        ICAP_REVENUE_PER_KWYR(NEXT_CN), &
                        HIGHEST_HEAT_RATE(NEXT_CN), & !  42
                        FIXED_COST_PER_MWH(NEXT_CN), &
                        HIGHEST_RFT(NEXT_CN), &
                        HIGHEST_RPM(NEXT_CN), &
                        TG_Net_Transfer(TG), &
                        CM_Net_Transfer(CM), &
                        MRX_ICAP_INDEX(NEXT_CN), &
                        CM_INTERUPTIBLRESERVE_MARGIN, &
                        CM_PEAK_AFTER_INTERRUPTIBLE(CM), &
                        CAPACITY_PLANNING_MW(HIGHEST_OPTION), &
                        RPS_REVENUE_MM(NEXT_CN), &
                        RPS_REVENUE_PER_MWH(NEXT_CN)
                     MX_ANNUAL_ALT_REC = MX_ANNUAL_ALT_REC + 1
                  ENDIF
!
!                 RECORD THREE CAPACITY ADDED VARIABLES
!
                  IF(PA> 0) THEN
                     TOTAL_ANNUAL_CAPACITY_ADDED(PA) = &
                           TOTAL_ANNUAL_CAPACITY_ADDED(PA) + &
                                    CAPACITY_PLANNING_MW(HIGHEST_OPTION)
                  ENDIF
                  TOTAL_ANNUAL_CAPACITY_ADDED(0) = &
                           TOTAL_ANNUAL_CAPACITY_ADDED(0) + &
                                    CAPACITY_PLANNING_MW(HIGHEST_OPTION)
                  IF(CM > 0) THEN
                     CM_TOT_ANN_CAPACITY_ADDED(CM) = &
                           CM_TOT_ANN_CAPACITY_ADDED(CM) + &
                                    CAPACITY_PLANNING_MW(HIGHEST_OPTION)
                  ENDIF
                  TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG) = &
                           TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG) + &
                                    CAPACITY_PLANNING_MW(HIGHEST_OPTION)
!
!                 OTHER VARIABLES
!
                  LAST_SORTED_POSITION = HIGHEST_OPTION
                  LAST_RESOURCE_ADDED(TG) = HIGHEST_OPTION
                  LAST_PA_RESOURCE_ADDED(PA) = NEXT_CN
                  LAST_CM_RESOURCE_ADDED(CM) = NEXT_CN
                  INSTALL_CAP_VALUE_LAST_PA_UNIT(PA) = &
                                HIGHEST_ANN_LEVEL_CAP(HIGHEST_OPTION)
                  INSTALL_CAP_VALUE_LAST_CM_UNIT(CM) = &
                                HIGHEST_ANN_LEVEL_CAP(HIGHEST_OPTION)

!
                  RPS_PROGRAM_BALANCE = RPS_PROGRAM_BALANCE + &
                                 1000.0 * HIGHEST_MRX_ENERGY(NEXT_CN)* &
                                           RPS_PERCENT(HIGHEST_OPTION)
               ENDDO ! RESOURCE ADDITIONS
         ENDDO ! RPS_PROGRAM
      ENDIF ! RPS MODULE ACTIVE
      ENDIF ! NEW OR OLD RPS_ROUTINE
!
         IF(.NOT. RPS_ONLY_SWITCH_ACTIVE) THEN
            TEMP_L = RESET_MRX_RPS_TEST_REQ() ! 022019.
         ENDIF
         DO I = HO, 1, -1
!
! MAXIMUM RESERVE MARGIN CONDITION
!
               NEXT_CN = SORTED_OPTIONS(I)
            HIGHEST_OPTION = HGST_OPT_INDEX(NEXT_CN)
       call write_trace_int2(file_trace_amoc, "11. HO", HIGHEST_OPTION)
!
            IF(RPS_RESOURCE_ADDED(NEXT_CN) > 0) CYCLE
            IF(.NOT. DEPENDENT_UNIT_AVAILABLE(HIGHEST_OPTION)) THEN
               MRX_ICAP_INDEX(NEXT_CN) = 9
               CYCLE
            ENDIF
!
            TG = GET_TRANS_GROUP_POSITION( &
                ns_cap_trans%transaction_group(HIGHEST_OPTION))
            call write_trace_int2(file_trace_amoc, "10. TG", TG)
            PA = TG_2_PLANNING_AREA(TG)
            CM = TG_2_CAPACITY_MARKET(TG)
            CM_INDEX = GET_CM_INDEX_FROM_TG(CM)
!
            IF(TEST_CAP_VALUE_W_ADD) THEN
               TEMP_R4 = CAPACITY_PLANNING_MW(HIGHEST_OPTION)
            ELSE
               TEMP_R4 = 0.0
            ENDIF
            IF(TG_MW_TRANSFERS) THEN
               CM_CAPACITY_MW(CM) = &
                           CM_PLANNING_CAPACITY(CM) + &
                                 CM_TOT_ANN_CAPACITY_ADDED(CM) + &
                                    CM_Net_Transfer(CM) + &
                                       TEMP_R4
                 call write_trace_real4(file_trace_mxa, "4. CM_CAP_MW", &
                    CM_CAPACITY_MW(CM))
            ELSE
               CM_CAPACITY_MW(CM) = &
                           CM_PLANNING_CAPACITY(CM) + &
                              CM_TOT_ANN_CAPACITY_ADDED(CM) + &
                                       TEMP_R4
                 call write_trace_real4(file_trace_mxa, "5. CM_CAP_MW", &
                    CM_CAPACITY_MW(CM))
            ENDIF
!
            IF(CM_PEAK_MW(CM) > .01) THEN
               CM_RESERVE_MARGIN(CM) = 100. * &
                              (CM_CAPACITY_MW(CM)/CM_PEAK_MW(CM) - 1.0)
               CM_INTERUPTIBLRESERVE_MARGIN = 100. * &
                         (CM_CAPACITY_MW(CM) - &
                                    CM_PEAK_AFTER_INTERRUPTIBLE(CM)) / &
                                CM_PEAK_AFTER_INTERRUPTIBLE(CM)
            ELSE
               CM_RESERVE_MARGIN(CM) = 100.
            ENDIF
!
            TEMP_R4 = CM_RESERVE_MARGIN(CM)*0.01 + 1.0
            TEMP_R4 = CM_INTERUPTIBLRESERVE_MARGIN*0.01 + 1.0
            
               if(R_YEAR==2 .and. CM==1) then
                    r_YEAR=r_YEAR ! Debugstop
               end if
            ICAP_DEMAND_CURVE_MULT(NEXT_CN) = &
                GET_ICAP_REVENUE_MULT(R_YEAR,CM,TEMP_R4)
            IF(SCREEN_CAPACITY(HIGHEST_OPTION) > 0.01) THEN
               IF(ICAP_SWITCH_INDEX(CM) == 2) THEN
                  CONE_COST_PER_KWYR(NEXT_CN) = &
                                   LOWEST_FIXED_COST_BY_CM(CM)
               ELSE
                  CONE_COST_PER_KWYR(NEXT_CN) = ICAP_CONE_PRICE(CM)
               ENDIF
               CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN) = &
                                    CONE_COST_PER_KWYR(NEXT_CN) * &
                                    ICAP_DEMAND_CURVE_MULT(NEXT_CN)
               IF(ICAP_SWITCH_INDEX(CM) == 2 .OR.   & !  MARGINAL
                                        ICAP_SWITCH_INDEX(CM) == 3) THEN
                                        ! CONE-MRX
                  CAPACITY_REVENUE_MM(NEXT_CN) = &
                         MIN(CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN) * &
                           CAPACITY_PLANNING_MW(HIGHEST_OPTION)*0.001, &
                                MAX(0., &
                                 FIXED_COST_MM(NEXT_CN) + &
                                    VARIABLE_COST_MM(NEXT_CN) - &
                                         ENERGY_REVENUE_MM(NEXT_CN)))
                  ICAP_REVENUE_PER_KWYR(NEXT_CN) = &
                          MAX(0.0,CAPACITY_REVENUE_MM(NEXT_CN)*1000.0/ &
                                       SCREEN_CAPACITY(HIGHEST_OPTION))
        call write_trace_int2(file_trace_amoc, "NEXT_CN", NEXT_CN)
        call write_trace_real4(file_trace_amoc, &
            "11. ICAP_REVENUE_PER_KWYR", &
            ICAP_REVENUE_PER_KWYR(NEXT_CN))
               ELSE
                  ICAP_REVENUE_PER_KWYR(NEXT_CN) = &
                           MIN(ICAP_CONE_PRICE(CM), &
                                CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN))
                  CAPACITY_REVENUE_MM(NEXT_CN) = &
                       ICAP_REVENUE_PER_KWYR(NEXT_CN) * &
                            CAPACITY_PLANNING_MW(HIGHEST_OPTION) * 0.001
               ENDIF

               PRICE_OF_TRANSFER = ICAP_REVENUE_PER_KWYR(NEXT_CN)
               QUANTITY_OF_TRANSFER = 0.0

               BEST_BUYER_TG = 0
               BEST_BUYER_CM = 0
               BEST_BUYER_PA = 0

               DO K = 1, NUMBER_DIRECT_INTERCONNECTS(TG)
                  BUYER_TG = DIRECT_INTERCONNECT_TG(TG,K)
                  BUYER_CM = TG_2_CAPACITY_MARKET(BUYER_TG)
         call write_trace_int2(file_trace_amoc, "6. buyer_cm", buyer_cm)
                  BUYER_PA = TG_2_PLANNING_AREA(BUYER_TG)
                  if(file_trace_amoc/=BAD_TRACE_HANDLE) then
                    write(file_trace_amoc, *) "B/S: ", &
                  "BUYER_TG ", BUYER_TG, &
                  "BUYER_CM ", BUYER_CM, &
                  "BUYER_PA ", BUYER_PA
                  end if
                  
                  IF(CM == BUYER_CM .OR. CM == 0) then
                   call write_trace_message(file_trace_amoc, "1. cycle")
                    CYCLE
                  end if

                  IF(TransmissionConstraint(TG,BUYER_TG) < 0.01) then
                 call write_trace_message(file_trace_amoc, "2. cycle")
                    CYCLE
                  end if
!
                  QUANTITY_OF_TRANSFER = MIN( &
                                TransmissionConstraint(TG,BUYER_TG), &
                                   CAPACITY_PLANNING_MW(HIGHEST_OPTION))
!
          if(file_trace_mxa/=BAD_TRACE_HANDLE) then
            write(file_trace_mxa,*) &
                "BUYER_CM ", BUYER_CM, &
                "CM_PLANNING_CAPACITY ", CM_PLANNING_CAPACITY(BUYER_CM),  &
                "CM_TOT_ANN_CAPACITY_ADDED ", CM_TOT_ANN_CAPACITY_ADDED(BUYER_CM), &
                "CM_Net_Transfer ", CM_Net_Transfer(BUYER_CM), &
                "TQY ", QUANTITY_OF_TRANSFER
          end if
          
                  CM_CAPACITY_MW(BUYER_CM) = &
                           CM_PLANNING_CAPACITY(BUYER_CM) + &
                                CM_TOT_ANN_CAPACITY_ADDED(BUYER_CM) + &
                                    CM_Net_Transfer(BUYER_CM) - &
                                       QUANTITY_OF_TRANSFER
                 call write_trace_int2(file_trace_mxa, "1. buyer_cm", &
                    buyer_cm)
                    
                 call write_trace_real4(file_trace_mxa, "3. CM_CAP_MW", &
                    CM_CAPACITY_MW(BUYER_CM))
                  IF(CM_PEAK_MW(BUYER_CM) > .01) THEN
                     CM_RESERVE_MARGIN(BUYER_CM) = 100. * &
                         (CM_CAPACITY_MW(BUYER_CM)/ &
                                            CM_PEAK_MW(BUYER_CM) - 1.0)
                     CM_INTERUPTIBLRESERVE_MARGIN = 100. * &
                         (CM_CAPACITY_MW(BUYER_CM) - &
                              CM_PEAK_AFTER_INTERRUPTIBLE(BUYER_CM)) / &
                                CM_PEAK_AFTER_INTERRUPTIBLE(BUYER_CM)
                  ELSE
                     CM_RESERVE_MARGIN(BUYER_CM) = 100.
                  ENDIF
                  TEMP_R4 = CM_RESERVE_MARGIN(BUYER_CM)*0.01 + 1.0
                  TEMP_R4 = CM_INTERUPTIBLRESERVE_MARGIN*0.01 + 1.0

                  call write_trace_int2(file_trace_amoc, "1. CM", CM)
               if(R_YEAR==2 .and. CM==1) then
                    r_YEAR=r_YEAR ! Debugstop
               end if
                  MARGINAL_ICAP_PER_KWYR_BY_TG(BUYER_TG) = &
                     MIN(MARGINAL_ICAP_PER_KWYR_BY_TG(BUYER_TG), &
                       GET_ICAP_REVENUE_MULT(R_YEAR,CM,TEMP_R4)* &
                                     LOWEST_FIXED_COST_BY_CM(BUYER_CM))
!
                  IF(PRICE_OF_TRANSFER >= &
                           MARGINAL_ICAP_PER_KWYR_BY_TG(BUYER_TG))CYCLE
!
                  PRICE_OF_TRANSFER = &
                                 MARGINAL_ICAP_PER_KWYR_BY_TG(BUYER_TG)
                  BEST_BUYER_TG = BUYER_TG
                  BEST_BUYER_CM = BUYER_CM
         call write_trace_int2(file_trace_amoc, "4. buyer_cm", buyer_cm)
         
                  BEST_BUYER_PA = BUYER_PA
                  
                  if(file_trace_amoc/=BAD_TRACE_HANDLE) then
                    write (file_trace_amoc, *) "Best buyers: ", &
                    "BEST_BUYER_TG ", BEST_BUYER_TG, &
                    "BEST_BUYER_CM ", BEST_BUYER_CM, &
                    "BEST_BUYER_PA ", BEST_BUYER_PA

                  end if
                  
               END DO
               
               ! TODO: Remove code in IF block
               IF(1 == 2 .AND. PRICE_OF_TRANSFER > &
                       ICAP_REVENUE_PER_KWYR(NEXT_CN) .AND. &
                                 QUANTITY_OF_TRANSFER > 0.10) THEN
                  PRICE_CEILING = PRICE_OF_TRANSFER
               ELSE
                  PRICE_CEILING = &
                                 CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN)
                  QUANTITY_OF_TRANSFER = 0.0
               ENDIF
!
               PRICE_CEILING = &
                              CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN)

               IF(ICAP_SWITCH_INDEX(CM) == 2 .OR. &  ! MARGINAL
                    ICAP_SWITCH_INDEX(CM) == 3) THEN ! CONE-MRX
                  CAPACITY_REVENUE_MM(NEXT_CN) = &
                           MIN(PRICE_CEILING * &
                           CAPACITY_PLANNING_MW(HIGHEST_OPTION)*0.001, &
                                MAX(0., &
                                 FIXED_COST_MM(NEXT_CN) + &
                                    VARIABLE_COST_MM(NEXT_CN) - &
                                       RPS_REVENUE_MM(NEXT_CN) - &
                                         ENERGY_REVENUE_MM(NEXT_CN)))
                  ICAP_REVENUE_PER_KWYR(NEXT_CN) = &
                          MAX(0.0,CAPACITY_REVENUE_MM(NEXT_CN)*1000.0/ &
                                       SCREEN_CAPACITY(HIGHEST_OPTION))
        call write_trace_int2(file_trace_amoc, "NEXT_CN", NEXT_CN)
        call write_trace_real4(file_trace_amoc, &
            "12. ICAP_REVENUE_PER_KWYR", &
            ICAP_REVENUE_PER_KWYR(NEXT_CN))
               ELSE
                  ICAP_REVENUE_PER_KWYR(NEXT_CN) = &
                                 MIN(ICAP_CONE_PRICE(CM),PRICE_CEILING)
        call write_trace_int2(file_trace_amoc, "NEXT_CN", NEXT_CN)
        call write_trace_real4(file_trace_amoc, &
            "13. ICAP_REVENUE_PER_KWYR", &
            ICAP_REVENUE_PER_KWYR(NEXT_CN))
                  CAPACITY_REVENUE_MM(NEXT_CN) = &
                       ICAP_REVENUE_PER_KWYR(NEXT_CN) * &
                            CAPACITY_PLANNING_MW(HIGHEST_OPTION)*0.001
               ENDIF

               EAS_PROFIT_PER_KWYR(NEXT_CN) = 0.001 * &
                        1000000. * (ENERGY_REVENUE_MM(NEXT_CN) - &
                                           VARIABLE_COST_MM(NEXT_CN))/ &
                                         SCREEN_CAPACITY(HIGHEST_OPTION)
               EAS_PROFIT_PER_KWYR(NEXT_CN) = MAX(0.0, &
                             CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN) - &
                                         ICAP_REVENUE_PER_KWYR(NEXT_CN))
               MARGINAL_ICAP_PER_KWYR_BY_TG(TG) = &
                                          ICAP_REVENUE_PER_KWYR(NEXT_CN)
            ELSE
               EAS_PROFIT_PER_KWYR(NEXT_CN) = 0.0
               CONE_COST_PER_KWYR(NEXT_CN) = 0.0
               CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN) = 0.0
               ICAP_REVENUE_PER_KWYR(NEXT_CN) = 0.0
        call write_trace_int2(file_trace_amoc, "NEXT_CN", NEXT_CN)
        call write_trace_real4(file_trace_amoc, &
            "14. ICAP_REVENUE_PER_KWYR", &
            ICAP_REVENUE_PER_KWYR(NEXT_CN))
               CAPACITY_REVENUE_MM(NEXT_CN) = 0.
               RPS_REVENUE_MM(NEXT_CN)= 0.0
            ENDIF
            HIGHEST_MARKET_REVENUE(NEXT_CN) = &
                                ENERGY_REVENUE_MM(NEXT_CN) + &
                                  RPS_REVENUE_MM(NEXT_CN) + &
                                     CAPACITY_REVENUE_MM(NEXT_CN)
            NET_MARGIN_PER_KWYR(HIGHEST_OPTION) = 1000.0 * &
                              (HIGHEST_MARKET_REVENUE(NEXT_CN) - &
                                 VARIABLE_COST_MM(NEXT_CN) - &
                                       FIXED_COST_MM(NEXT_CN))/ &
                                 SCREEN_CAPACITY(HIGHEST_OPTION)
            HGST_NET_MARG_ADDED(NEXT_CN) = &
                                     NET_MARGIN_PER_KWYR(HIGHEST_OPTION)
         call write_trace_real4(file_trace_amoc, "20. HGMA", &
            HGST_NET_MARG_ADDED(NEXT_CN))

            IF(PA_PEAK_AFTER_INTERRUPTIBLE(PA) > NEAR_ZERO)  THEN
               PA_PLANNING_RESERVE_MARGIN(PA) = &
                        1. + (PA_PLANNING_CAPACITY(PA) &
                                    + PA_Net_Transfer(PA) - &
                                 PA_PEAK_AFTER_INTERRUPTIBLE(PA)) / &
                                      PA_PEAK_AFTER_INTERRUPTIBLE(PA)
               MINIMUM_ANNUAL_MRX_CAP(PA) = MAX(0., &
                        (CURRENT_TARGET_RATIO_PA(PA) - &
                           PA_PLANNING_RESERVE_MARGIN(PA))* &
                                        PA_PEAK_AFTER_INTERRUPTIBLE(PA))
!
            ELSE
               PA_PLANNING_RESERVE_MARGIN(PA) = ZERO
            ENDIF
            IF(PEAK_AFTER_INTERRUPTIBLE_TG(TG) > NEAR_ZERO)  THEN
               TG_PLANNING_RESERVE_MARGIN(TG) = &
                        1. + (TG_PLANNING_CAPACITY(TG) &
                                    + TG_Net_Transfer(TG) - &
                                  PEAK_AFTER_INTERRUPTIBLE_TG(TG)) / &
                                         PEAK_AFTER_INTERRUPTIBLE_TG(TG)
               MINIMUM_TG_MRX_CAP(TG) = &
                           MAX(0., &
                              (GET_MIN_CAP_TESTING_RATIO(TG)* &
                                             SCENARIO_RESERVE_MARGIN - &
                                  TG_PLANNING_RESERVE_MARGIN(TG))* &
                                        PEAK_AFTER_INTERRUPTIBLE_TG(TG))
               TG_PEAK_MW(TG) = PEAK_AFTER_INTERRUPTIBLE_TG(TG)
            ELSE
               MINIMUM_TG_MRX_CAP(TG) = ZERO
            ENDIF
!
            IF(USE_REGIONAL_MINIMUM_RM_LOGIC) THEN
               IF(TOTAL_ANNUAL_CAPACITY_ADDED(0) + &
                      CAPACITY_PLANNING_MW(HIGHEST_OPTION) > &
                                       MAXIMUM_ANNUAL_MRX_CAP(0) .OR. &
                     TOTAL_ANNUAL_CAPACITY_ADDED(PA) + &
                           CAPACITY_PLANNING_MW(HIGHEST_OPTION) > &
                                        MAXIMUM_ANNUAL_MRX_CAP(PA)) THEN
!
                  if(TOTAL_ANNUAL_CAPACITY_ADDED(0) + &
                      CAPACITY_PLANNING_MW(HIGHEST_OPTION) > &
                                       MAXIMUM_ANNUAL_MRX_CAP(0)) then
                     MRX_ICAP_INDEX(NEXT_CN) = 4
                  else
                     MRX_ICAP_INDEX(NEXT_CN) = 5
                  endif
                  TOTAL_REGIONAL_CAPACITY_NEEDED = 0.
                  DO PA = 1, PG
                     TOTAL_REGIONAL_CAPACITY_NEEDED = &
                          TOTAL_REGIONAL_CAPACITY_NEEDED + &
                              MAX(0.,MINIMUM_ANNUAL_MRX_CAP(PA) - &
                                        TOTAL_ANNUAL_CAPACITY_ADDED(PA))
                  ENDDO
!
                  IF(TOTAL_REGIONAL_CAPACITY_NEEDED > 0.) THEN
                     CYCLE
                  ELSE
                     EXIT
                  ENDIF
               ENDIF
            ELSE
               IF(TOTAL_ANNUAL_CAPACITY_ADDED(0) + &
                      CAPACITY_PLANNING_MW(HIGHEST_OPTION) > &
                                  MAX(MAXIMUM_ANNUAL_MRX_CAP(0), &
                                       MAXIMUM_ANNUAL_MRX_CAP(PA))) THEN
                  MRX_ICAP_INDEX(NEXT_CN) = 4
                  EXIT
               ENDIF
            ENDIF
!
            IF(TOTAL_ANNUAL_CAPACITY_ADDED(PA) + &
                      CAPACITY_PLANNING_MW(HIGHEST_OPTION) > &
                                       MAXIMUM_ANNUAL_MRX_CAP(PA)) THEN
               MRX_ICAP_INDEX(NEXT_CN) = 6
               CYCLE
            ENDIF
!
! 090709. ADDITIONAL MAXIMUM TG CONSTRAINT.
!
            IF(TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG) + &
                      CAPACITY_PLANNING_MW(HIGHEST_OPTION) > &
                                       MAXIMUM_TG_MRX_CAP(TG)) THEN
               MRX_ICAP_INDEX(NEXT_CN) = 7
               CYCLE
            ENDIF
!
            IF(USE_MINIMUM_RM_LOGIC) THEN
               IF(TOTAL_ANNUAL_CAPACITY_ADDED(0)  >= &
                                         MINIMUM_ANNUAL_MRX_CAP(0)) THEN
                  IF(HGST_NET_MARG_ADDED(NEXT_CN)+ 0.01<= &
                                           MINIMUM_NET_MARGIN .OR. &
                                            RPS_ONLY_SWITCH_ACTIVE) THEN
                     MRX_ICAP_INDEX(NEXT_CN) = 4
                     EXIT
                  ENDIF
               ELSE ! HAVE NOT ADDED ENOUGH.
               ENDIF
            ELSEIF(USE_REGIONAL_MINIMUM_RM_LOGIC) THEN
!
! I'VE ALREADY ADDED ENOUGH
!
               IF(TOTAL_ANNUAL_CAPACITY_ADDED(PA)  >= &
                  MINIMUM_ANNUAL_MRX_CAP(PA) .AND. &
                     TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG)  >= & ! 090709.
                      MINIMUM_TG_MRX_CAP(TG)) THEN
!
                  IF(HGST_NET_MARG_ADDED(NEXT_CN)+ 0.01<= &
                                           MINIMUM_NET_MARGIN .OR. &
                                            RPS_ONLY_SWITCH_ACTIVE) THEN
!
! MUST CYCLE THROUGH BECAUSE OF MULTIPLE PLANNING AREAS
!
                     MRX_ICAP_INDEX(NEXT_CN) = 8
                     CYCLE
                  ENDIF
               ELSE ! HAVE NOT ADDED ENOUGH.
!
! I NEED MORE TO MEET MINIMUM RESERVE. LET THIS ALWAYS FALL THROUGH TO 
! BUILD THE UNIT.
               ENDIF
!
            ELSEIF(HGST_NET_MARG_ADDED(NEXT_CN)+0.01 <= &
                                           MINIMUM_NET_MARGIN .OR. &
                                            RPS_ONLY_SWITCH_ACTIVE) THEN
!
               MRX_ICAP_INDEX(NEXT_CN) = 8
               EXIT
!
            ENDIF
!
! PASSED ALL TESTS.  ADD THE UNIT
!
            IF(PA > 0) THEN
               TOTAL_ANNUAL_CAPACITY_ADDED(PA) = &
                           TOTAL_ANNUAL_CAPACITY_ADDED(PA) + &
                                    CAPACITY_PLANNING_MW(HIGHEST_OPTION)
!
               TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG) = &
                           TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG) + &
                                    CAPACITY_PLANNING_MW(HIGHEST_OPTION)
            ENDIF
            IF(CM > 0) THEN
               CM_TOT_ANN_CAPACITY_ADDED(CM) = &
                           CM_TOT_ANN_CAPACITY_ADDED(CM) + &
                                    CAPACITY_PLANNING_MW(HIGHEST_OPTION)
            ENDIF
!
            TOTAL_ANNUAL_CAPACITY_ADDED(0) = &
                           TOTAL_ANNUAL_CAPACITY_ADDED(0) + &
                                    CAPACITY_PLANNING_MW(HIGHEST_OPTION)
!
            LAST_SORTED_POSITION = HIGHEST_OPTION
            LAST_RESOURCE_ADDED(TG) = HIGHEST_OPTION
!
            LAST_PA_RESOURCE_ADDED(PA) = NEXT_CN
            LAST_CM_RESOURCE_ADDED(CM) = NEXT_CN
            INSTALL_CAP_VALUE_LAST_PA_UNIT(PA) = &
                                HIGHEST_ANN_LEVEL_CAP(HIGHEST_OPTION)
            INSTALL_CAP_VALUE_LAST_CM_UNIT(CM) = &
                                HIGHEST_ANN_LEVEL_CAP(HIGHEST_OPTION)
!
            FIRST_YEAR_CAPACITY = &
                   ADD_THIS_UNIT( &
                           OPTION_POSITION(HIGHEST_OPTION),THIS_YEAR + &
                                        LOCAL_LEAD_TIME(HIGHEST_OPTION))
       call write_trace_real(file_trace_amoc, "4. FIRST_YEAR_CAPACITY", &
      first_year_capacity)    
!
            IF(INDEPENDENT(HIGHEST_OPTION) > 0) THEN
               HYBRID_PROD_POINTER = INDEPENDENT(HIGHEST_OPTION)
               FIRST_YEAR_CAPACITY = &
                   ADD_THIS_UNIT( &
                      OPTION_POSITION(HYBRID_PROD_POINTER),THIS_YEAR + &
                                   LOCAL_LEAD_TIME(HYBRID_PROD_POINTER))
       call write_trace_real(file_trace_amoc, "5. FIRST_YEAR_CAPACITY", &
      first_year_capacity)    
            ENDIF
!
            IF(MX_SUMMARY_REPORT) THEN
!
               TEMP_I2 = GET_REGIONAL_CM_NAME(CM,CM_NAME)
               IF(INDEPENDENT(HIGHEST_OPTION) > 0) THEN

                  WRITE(LOCAL_NAME,1017) &
                           OPTION_NAME(HIGHEST_OPTION)(1:15), &
                                                         ' H',HO - I + 1
              call write_trace_string(file_trace_amoc, &
                "9. LOCAL_NAME", LOCAL_NAME)
                                                         
               ELSE
              call write_trace_string(file_trace_amoc, &
                "10. LOCAL_NAME", LOCAL_NAME)
                  WRITE(LOCAL_NAME,1010) &
                           OPTION_NAME(HIGHEST_OPTION)(1:17),HO - I + 1
               ENDIF
!
               MARKET_AVE_REVENUE = 0.
               TOTAL_AVERAGE_COST = 0.
       call write_trace_real4(file_trace_amoc, "12. TAC/MAR", &
            0.)
!
! 032811. TOOK OUT SCREEN CAPACITY PER NAN
!
               IF(TG_MW_TRANSFERS) THEN
                  TG_CAPACITY_MW(TG) = &
                           TG_PLANNING_CAPACITY(TG) + &
                              TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG) + & ! -
                                          TG_Net_Transfer(TG)
               ELSE
                  TG_CAPACITY_MW(TG) = &
                           TG_PLANNING_CAPACITY(TG) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG) ! -
               ENDIF
!
               IF(TG_PEAK_MW(TG) > .01) THEN
                  TG_RESERVE_MARGIN(TG) = 100. * &
                              (TG_CAPACITY_MW(TG)/TG_PEAK_MW(TG) - 1.0)
               ELSE
                  TG_RESERVE_MARGIN(TG) = 100.
               ENDIF
!
               PA_CAPACITY_MW(PA) = &
                           PA_PLANNING_CAPACITY(PA) + &
                                 TOTAL_ANNUAL_CAPACITY_ADDED(PA)
               IF(PA_PEAK_MW(PA) > .01) THEN
                  PA_RESERVE_MARGIN(PA) = 100. * &
                              (PA_CAPACITY_MW(PA)/PA_PEAK_MW(PA) - 1.0)
               ELSE
                  PA_RESERVE_MARGIN(PA) = 100.
               ENDIF
               TEMP_R4 = PA_RESERVE_MARGIN(PA)*0.01 + 1.0
!
               IF(TG_MW_TRANSFERS) THEN
                  CM_CAPACITY_MW(CM) = &
                           CM_PLANNING_CAPACITY(CM) + &
                                 CM_TOT_ANN_CAPACITY_ADDED(CM) + &
                                          CM_Net_Transfer(CM) + &
                                              QUANTITY_OF_TRANSFER
                 
       if(file_trace_mxa/=BAD_TRACE_HANDLE) then
        write(file_trace_mxa, *) "x. CM", CM, &
           "CM_PLANNING_CAPACITY ", CM_PLANNING_CAPACITY(CM), &
           "CM_TOT_ANN_CAPACITY_ADDED ", CM_TOT_ANN_CAPACITY_ADDED(CM), &
           "CM_Net_Transfer ", CM_Net_Transfer(CM), &
           "QUANTITY_OF_TRANSFER ", QUANTITY_OF_TRANSFER
       end if
       
                 call write_trace_real4(file_trace_mxa, "2. CM_CAP_MW", &
                    CM_CAPACITY_MW(CM))
                    
               ELSE
                  CM_CAPACITY_MW(CM) = &
                           CM_PLANNING_CAPACITY(CM) + &
                                 CM_TOT_ANN_CAPACITY_ADDED(CM)
                 call write_trace_real4(file_trace_mxa, "1. CM_CAP_MW", &
                    CM_CAPACITY_MW(CM))
                    
               ENDIF
               IF(CM_PEAK_MW(CM) > .01) THEN
                  CM_RESERVE_MARGIN(CM) = 100. * &
                              (CM_CAPACITY_MW(CM)/CM_PEAK_MW(CM) - 1.0)
                  CM_INTERUPTIBLRESERVE_MARGIN = 100. * &
                         (CM_CAPACITY_MW(CM) - &
                                    CM_PEAK_AFTER_INTERRUPTIBLE(CM)) / &
                                CM_PEAK_AFTER_INTERRUPTIBLE(CM)
               ELSE
                  CM_RESERVE_MARGIN(CM) = 100.
               ENDIF
!
               TEMP_R4 = CM_RESERVE_MARGIN(CM)*0.01 + 1.0
               TEMP_R4 = CM_INTERUPTIBLRESERVE_MARGIN*0.01 + 1.0
               
               if(R_YEAR==2 .and. CM==1) then
                    r_YEAR=r_YEAR ! Debugstop
               end if
               ICAP_DEMAND_CURVE_MULT(NEXT_CN) = &
                         GET_ICAP_REVENUE_MULT(R_YEAR,CM,TEMP_R4)
               IF(SCREEN_CAPACITY(HIGHEST_OPTION) > 0.01) THEN
                  EAS_PROFIT_PER_KWYR(NEXT_CN) = 0.001 * &
                        1000000. * (ENERGY_REVENUE_MM(NEXT_CN) - &
                                           VARIABLE_COST_MM(NEXT_CN))/ &
                                       SCREEN_CAPACITY(HIGHEST_OPTION)
!
                  IF(ICAP_SWITCH_INDEX(CM) == 2) THEN
                     CONE_COST_PER_KWYR(NEXT_CN) = &
                                             LOWEST_FIXED_COST_BY_CM(CM)
                  ELSE
                     CONE_COST_PER_KWYR(NEXT_CN) = ICAP_CONE_PRICE(CM)
                  ENDIF
!
                  BASE_CAP_REV_MM = -0.001 * &
                                CAPACITY_PLANNING_MW(HIGHEST_OPTION) * &
                                       CM_MAX_NET_ENRG_MARG_PER_KWYR(CM)
                  MAX_CAP_REV_MM = 0.001 * &
                                CAPACITY_PLANNING_MW(HIGHEST_OPTION) * &
                                 CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN)
                  MAX_RPS_REV_MM = RPS_REVENUE_MM(NEXT_CN)
                  ENERGY_UP_LIFT_MM = &
                          MIN(MAX_CAP_REV_MM, &
                               MAX(0.0,(FIXED_COST_MM(NEXT_CN) + &
                                    VARIABLE_COST_MM(NEXT_CN) - &
                                          ENERGY_REVENUE_MM(NEXT_CN))))
                  MAX_RPS_REV_MM = &
                    MAX(0.0, &
                       MIN(ENERGY_UP_LIFT_MM - BASE_CAP_REV_MM, &
                                                       MAX_RPS_REV_MM))
                  TEMP_R4 = &
                     MAX(0.0, &
                        MIN(ENERGY_UP_LIFT_MM - MAX_RPS_REV_MM, &
                                                        MAX_CAP_REV_MM))
                  TEMP_R4 = TEMP_R4 * 1000.0 / &
                            CAPACITY_PLANNING_MW(HIGHEST_OPTION)
!
                  IF(ICAP_SWITCH_INDEX(CM) == 2 .OR.   & !  MARGINAL
                                        ICAP_SWITCH_INDEX(CM) == 3) THEN
                                        ! CONE-MRX
                     ICAP_REVENUE_PER_KWYR(NEXT_CN) = &
                        MAX(0.0,MIN(PRICE_CEILING,TEMP_R4))
        call write_trace_int2(file_trace_amoc, "NEXT_CN", NEXT_CN)
        call write_trace_real4(file_trace_amoc, &
            "15. ICAP_REVENUE_PER_KWYR", &
            ICAP_REVENUE_PER_KWYR(NEXT_CN))
                  ELSE
                     ICAP_REVENUE_PER_KWYR(NEXT_CN) = &
                                                     ICAP_CONE_PRICE(CM)
        call write_trace_int2(file_trace_amoc, "NEXT_CN", NEXT_CN)
        call write_trace_real4(file_trace_amoc, &
            "16. ICAP_REVENUE_PER_KWYR", &
            ICAP_REVENUE_PER_KWYR(NEXT_CN))
                  ENDIF
               ELSE
                  EAS_PROFIT_PER_KWYR(NEXT_CN) = 0.0
                  CONE_COST_PER_KWYR(NEXT_CN) = 0.0
                  CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN) = &
                                                                  0.0
                  ICAP_REVENUE_PER_KWYR(NEXT_CN) = 0.0
        call write_trace_int2(file_trace_amoc, "NEXT_CN", NEXT_CN)
        call write_trace_real4(file_trace_amoc, &
            "17. ICAP_REVENUE_PER_KWYR", &
            ICAP_REVENUE_PER_KWYR(NEXT_CN))
               ENDIF
!
               CAPACITY_REVENUE_MM(NEXT_CN) = 0.001 * &
                                 ICAP_REVENUE_PER_KWYR(NEXT_CN) * &
                            CAPACITY_PLANNING_MW(HIGHEST_OPTION)
!
               RPS_REVENUE_MM(NEXT_CN) = &
                           MIN(RPS_REVENUE_MM(NEXT_CN), &
                                MAX(0., &
                                 FIXED_COST_MM(NEXT_CN) + &
                                    VARIABLE_COST_MM(NEXT_CN) - &
                                       CAPACITY_REVENUE_MM(NEXT_CN) - &
                                          ENERGY_REVENUE_MM(NEXT_CN)))
               TEMP_R = HIGHEST_MRX_ENERGY(NEXT_CN) * &
                                       RPS_PERCENT(HIGHEST_OPTION)
                call write_trace_real(file_trace_amoc, "16. TEMP_R", &
                        TEMP_R)
               IF(TEMP_R > 0.0) THEN
                  RPS_REVENUE_PER_MWH(NEXT_CN) = &
                              1000.0 * RPS_REVENUE_MM(NEXT_CN) / TEMP_R
               ELSE
                  RPS_REVENUE_PER_MWH(M) = 0.0
               ENDIF
               HIGHEST_MARKET_REVENUE(NEXT_CN) = &
                                ENERGY_REVENUE_MM(NEXT_CN) + &
                                  RPS_REVENUE_MM(NEXT_CN) + &
                                     CAPACITY_REVENUE_MM(NEXT_CN)
               IF(HIGHEST_MRX_ENERGY(NEXT_CN) > 0.01) THEN
                  CAPACITY_REVENUE_PER_MWH(NEXT_CN) = &
                            CAPACITY_REVENUE_MM(NEXT_CN) * 1000.0/ &
                                            HIGHEST_MRX_ENERGY(NEXT_CN)
                  REVENUE_PER_MWH(NEXT_CN) = &
                           HIGHEST_ENERGY_REV_PER_MWH(NEXT_CN) + &
                              RPS_REVENUE_PER_MWH(NEXT_CN) + &
                                CAPACITY_REVENUE_PER_MWH(NEXT_CN)
                  FIXED_COST_PER_MWH(NEXT_CN) = &
                        HIGHEST_FOM_PER_MWH(NEXT_CN) + &
                              HIGHEST_LEVEL_CAP_PER_MWH(NEXT_CN)
                  HIGHEST_MRX_MARGIN(NEXT_CN) = &
                              REVENUE_PER_MWH(NEXT_CN) - &
                                 HIGHEST_STRIKE_PRICE(NEXT_CN) - &
                                    FIXED_COST_PER_MWH(NEXT_CN)
               ELSE
                  CAPACITY_REVENUE_PER_MWH(NEXT_CN) = 0.0
                  REVENUE_PER_MWH(NEXT_CN) = 0.0
                  FIXED_COST_PER_MWH(NEXT_CN) = 0.0
                  HIGHEST_MRX_MARGIN(NEXT_CN) = 0.0
               ENDIF
!
               IF(SCREEN_CAPACITY(HIGHEST_OPTION) > 0.01) THEN
                  HIGHEST_MARGIN_PER_KWYR(NEXT_CN) = 1000.0 * &
                              (HIGHEST_MARKET_REVENUE(NEXT_CN) - &
                                 VARIABLE_COST_MM(NEXT_CN) - &
                                       FIXED_COST_MM(NEXT_CN))/ &
                              SCREEN_CAPACITY(HIGHEST_OPTION)
               ELSE
                  HIGHEST_MARGIN_PER_KWYR(NEXT_CN) = -999999.0
               ENDIF

               IF(RPS_REVENUE_MM(NEXT_CN) > 0.0) THEN
                  MRX_ICAP_INDEX(NEXT_CN) = 10
                  TEMP_I2 = RPS_PROGRAM_NUMBER_loc(HIGHEST_OPTION)
                  RPS_COUNT = 1
                  RPS_NO = GET_RPS_PROGRAM_POSITION(TEMP_I2,RPS_COUNT)
                  INT2_PM = INT(HIGHEST_RPM(NEXT_CN),2)
                  IF(ABS(TEMP_I2) > 0) THEN
                     TEMP_R = PUT_RPS_PRICE_BY_OPTION( &
                                 TEMP_I2, &
                                 RPS_REVENUE_PER_MWH(NEXT_CN), &
                                 1000.0*HIGHEST_MRX_ENERGY(NEXT_CN), &
                                 SCREEN_CAPACITY(HIGHEST_OPTION), &
                                 INT2_PM, &
                                 R_YEAR, &
                                 GRX_RPS_MODULE_ACTIVE, &
                                 RPS_GEN_PRICE_BY(RPS_NO))
                call write_trace_real(file_trace_amoc, "17. TEMP_R", &
                        TEMP_R)
                  ENDIF
               ELSE
                  MRX_ICAP_INDEX(NEXT_CN) = 1
               ENDIF
!
               MX_ANNUAL_ALT_REC = RPTREC(MX_ANNUAL_ALT_NO)
              call write_trace_string(file_trace_amoc, &
                "11. LOCAL_NAME", LOCAL_NAME)
             call write_trace_message(file_trace_mxa, "6. WRITE: ")
             if (file_trace_mxa/=BAD_TRACE_HANDLE) then
                    write(file_trace_mxa,*)  &
                   "NEXT_CN ", NEXT_CN, " ",  &
                   "THIS_YEAR ", THIS_YEAR, " ", &
                   "HIGHEST_OPTION ", HIGHEST_OPTION, " " , &
                   "TG ", TG, " " , &
                   "CM_INDEX ", CM_INDEX, " " , &
                   "MX_ANNUAL_ALT_REC ", MX_ANNUAL_ALT_REC, &
      "PRT_ENDPOINT ", PRT_ENDPOINT(), &
      "THIS_YEAR ", FLOAT(THIS_YEAR), &
      "CM_NAME ", CM_NAME, &
      "LOCAL_NAME ", LOCAL_NAME, &
      "HIGHEST_STRIKE_PRICE ", HIGHEST_STRIKE_PRICE(NEXT_CN), &
      "HIGHEST_ENERGY_REV_PER_MWH ", HIGHEST_ENERGY_REV_PER_MWH(NEXT_CN), & 
      "HIGHEST_MRX_MARGIN ", HIGHEST_MRX_MARGIN(NEXT_CN), &
      "HIGHEST_MRX_CF ", HIGHEST_MRX_CF(NEXT_CN), &
      "ENERGY_REVENUE_MM ", ENERGY_REVENUE_MM(NEXT_CN), &
      "HIGHEST_MARKET_COST ", HIGHEST_MARKET_COST(NEXT_CN), & !  6
      "VARIABLE_COST_MM ", VARIABLE_COST_MM(NEXT_CN), &
      "FIXED_COST_MM ", FIXED_COST_MM(NEXT_CN), &
      "SCREEN_CAPACITY ", SCREEN_CAPACITY(HIGHEST_OPTION), &
      "HIGHEST_MRX_ENERGY ", HIGHEST_MRX_ENERGY(NEXT_CN), & !  10
      "TOTAL_AVERAGE_COST ", TOTAL_AVERAGE_COST, & !  NEED DEFINITION
      "HIGHEST_ANN_LEVEL_CAP ", HIGHEST_ANN_LEVEL_CAP(NEXT_CN), &
      "HIGHEST_FUEL_COST ", HIGHEST_FUEL_COST(NEXT_CN), &
      "6. HIGHEST_ANNUAL_UNITS ", HIGHEST_ANNUAL_UNITS(NEXT_CN), &
      "CAPACITY_REVENUE_MM ", CAPACITY_REVENUE_MM(NEXT_CN), &
      "HIGHEST_MARKET_REVENUE ", HIGHEST_MARKET_REVENUE(NEXT_CN), & 
      "HIGHEST_MARGIN_PER_KWYR ", HIGHEST_MARGIN_PER_KWYR(NEXT_CN), &
      "HIGHEST_FUEL_COST_PER_MWH ", HIGHEST_FUEL_COST_PER_MWH(NEXT_CN), &
      "HIGHEST_VAR_OM_PER_MWH ", HIGHEST_VAR_OM_PER_MWH(NEXT_CN), & 
      "HIGHEST_FUEL_COST_PER_MMBTU ", HIGHEST_FUEL_COST_PER_MMBTU(NEXT_CN), &
      "REVENUE_PER_MWH ", REVENUE_PER_MWH(NEXT_CN), &
      "CAPACITY_REVENUE_PER_MWH ", CAPACITY_REVENUE_PER_MWH(NEXT_CN), &
      "HIGHEST_EMISSIONS_PER_MWH ", HIGHEST_EMISSIONS_PER_MWH(NEXT_CN), &
      "HIGHEST_FOM_PER_MWH ", HIGHEST_FOM_PER_MWH(NEXT_CN), & 
      "HIGHEST_LEVEL_CAP_PER_MWH ", HIGHEST_LEVEL_CAP_PER_MWH(NEXT_CN), &
      "HIGHEST_VOM_COST_MM ", HIGHEST_VOM_COST_MM(NEXT_CN), &
      "HIGHEST_EMISSIONS_COST_MM ", HIGHEST_EMISSIONS_COST_MM(NEXT_CN), & 
      "HIGHEST_FOM_COST_MM ", HIGHEST_FOM_COST_MM(NEXT_CN), & 
      "TRANSACTION_GROUP ", &
      FLOAT(ns_cap_trans%transaction_group(HIGHEST_OPTION)), &
      "TG_CAPACITY_MW ", TG_CAPACITY_MW(TG), &
      "TG_PEAK_MW ", TG_PEAK_MW(TG), &
      "TG_RESERVE_MARGIN ", TG_RESERVE_MARGIN(TG), &
      "CM_INDEX ", FLOAT(CM_INDEX), &
      "CM_CAPACITY_MW ", CM_CAPACITY_MW(CM), &
      "CM_PEAK_MW ", CM_PEAK_MW(CM), &
      "CM_RESERVE_MARGIN ", CM_RESERVE_MARGIN(CM), &
      "ICAP_DEMAND_CURVE_MULT ", ICAP_DEMAND_CURVE_MULT(NEXT_CN), &
      "CONE_COST_PER_KWYR ", CONE_COST_PER_KWYR(NEXT_CN), & !  38
      "CONE_DEMAND_CURVE_ADJ_PER_KWYR ", CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN), &
      "EAS_PROFIT_PER_KWYR ", EAS_PROFIT_PER_KWYR(NEXT_CN), &
      "ICAP_REVENUE_PER_KWYR ", ICAP_REVENUE_PER_KWYR(NEXT_CN), &
      "HIGHEST_HEAT_RATE ", HIGHEST_HEAT_RATE(NEXT_CN), & !  42
      "FIXED_COST_PER_MWH ", FIXED_COST_PER_MWH(NEXT_CN), &
      "HIGHEST_RFT ", HIGHEST_RFT(NEXT_CN), &
      "HIGHEST_RPM ", HIGHEST_RPM(NEXT_CN), &
      "TG_Net_Transfer ", TG_Net_Transfer(TG), &
      "CM_Net_Transfer ", CM_Net_Transfer(CM), &
      "MRX_ICAP_INDEX ", MRX_ICAP_INDEX(NEXT_CN), &
      "CM_INTERUPTIBLRESERVE_MARGIN ", CM_INTERUPTIBLRESERVE_MARGIN, &
      "CM_PEAK_AFTER_INTERRUPTIBLE ", CM_PEAK_AFTER_INTERRUPTIBLE(CM), &
      "CAPACITY_PLANNING_MW ", CAPACITY_PLANNING_MW(HIGHEST_OPTION), &
      "RPS_REVENUE_MM ", RPS_REVENUE_MM(NEXT_CN), &
      "RPS_REVENUE_PER_MWH ", RPS_REVENUE_PER_MWH(NEXT_CN), linefeed
             end if
                     ! TODO: Combined writes to one in separate routine
               WRITE(MX_ANNUAL_ALT_NO,REC=MX_ANNUAL_ALT_REC) &
                        PRT_ENDPOINT(), &
                        FLOAT(THIS_YEAR), &
                        CM_NAME, &
                        LOCAL_NAME, &
                        HIGHEST_STRIKE_PRICE(NEXT_CN), &
                        HIGHEST_ENERGY_REV_PER_MWH(NEXT_CN), & 
                        ! 110309. REDEFINED.
                        HIGHEST_MRX_MARGIN(NEXT_CN), &
                        HIGHEST_MRX_CF(NEXT_CN), &
                        ENERGY_REVENUE_MM(NEXT_CN), &
                        HIGHEST_MARKET_COST(NEXT_CN), & !  6
                        VARIABLE_COST_MM(NEXT_CN), &
                        FIXED_COST_MM(NEXT_CN), &
                        SCREEN_CAPACITY(HIGHEST_OPTION), &
                        HIGHEST_MRX_ENERGY(NEXT_CN), & !  10
                        TOTAL_AVERAGE_COST, & !  NEED DEFINITION
                        HIGHEST_ANN_LEVEL_CAP(NEXT_CN), &
                        HIGHEST_FUEL_COST(NEXT_CN), &
                        HIGHEST_ANNUAL_UNITS(NEXT_CN), &
                        CAPACITY_REVENUE_MM(NEXT_CN), &
                        HIGHEST_MARKET_REVENUE(NEXT_CN), & 
                        ! 16 TOTAL REVENUE
                        HIGHEST_MARGIN_PER_KWYR(NEXT_CN), &
                        HIGHEST_FUEL_COST_PER_MWH(NEXT_CN), &
                        HIGHEST_VAR_OM_PER_MWH(NEXT_CN), & !  19
                        HIGHEST_FUEL_COST_PER_MMBTU(NEXT_CN), &
                        REVENUE_PER_MWH(NEXT_CN), &
                        CAPACITY_REVENUE_PER_MWH(NEXT_CN), &
                        HIGHEST_EMISSIONS_PER_MWH(NEXT_CN), &
                        HIGHEST_FOM_PER_MWH(NEXT_CN), & !  24
                        HIGHEST_LEVEL_CAP_PER_MWH(NEXT_CN), &
                        HIGHEST_VOM_COST_MM(NEXT_CN), &
                        HIGHEST_EMISSIONS_COST_MM(NEXT_CN), & !  27
                        HIGHEST_FOM_COST_MM(NEXT_CN), &
                        FLOAT(ns_cap_trans%transaction_group(HIGHEST_OPTION)), &
                        TG_CAPACITY_MW(TG), &
                        TG_PEAK_MW(TG), &
                        TG_RESERVE_MARGIN(TG), &
                        FLOAT(CM_INDEX), & !  PLANNING_AREA_ID(NEXT_CN),
                        CM_CAPACITY_MW(CM), &
                        CM_PEAK_MW(CM), &
                        CM_RESERVE_MARGIN(CM), &
                        ICAP_DEMAND_CURVE_MULT(NEXT_CN), &
                        CONE_COST_PER_KWYR(NEXT_CN), & !  38
                        CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN), &
                        EAS_PROFIT_PER_KWYR(NEXT_CN), &
                        ICAP_REVENUE_PER_KWYR(NEXT_CN), &
                        HIGHEST_HEAT_RATE(NEXT_CN), & !  42
                        FIXED_COST_PER_MWH(NEXT_CN), &
                        HIGHEST_RFT(NEXT_CN), &
                        HIGHEST_RPM(NEXT_CN), &
                        TG_Net_Transfer(TG), &
                        CM_Net_Transfer(CM), &
                        MRX_ICAP_INDEX(NEXT_CN), &
                        CM_INTERUPTIBLRESERVE_MARGIN, &
                        CM_PEAK_AFTER_INTERRUPTIBLE(CM), &
                        CAPACITY_PLANNING_MW(HIGHEST_OPTION), &
                        RPS_REVENUE_MM(NEXT_CN), &
                        RPS_REVENUE_PER_MWH(NEXT_CN)
               MX_ANNUAL_ALT_REC = MX_ANNUAL_ALT_REC + 1
            ENDIF
!
            IF(PRICE_OF_TRANSFER > &
                       ICAP_REVENUE_PER_KWYR(NEXT_CN) .AND. &
                                       QUANTITY_OF_TRANSFER > 0.10) THEN
!
! RECORD TRANSACTION AND TRY TO RESET THE LOOP
!
               TransmissionConstraint(TG,BEST_BUYER_TG) = &
                        TransmissionConstraint(TG,BEST_BUYER_TG) - &
                                                    QUANTITY_OF_TRANSFER
               CM_Net_Transfer(CM) = CM_Net_Transfer(CM) + &
                                                    QUANTITY_OF_TRANSFER
               CM_Net_Transfer(BEST_BUYER_CM) = &
                                 CM_Net_Transfer(BEST_BUYER_CM) - &
                                                    QUANTITY_OF_TRANSFER
               PA_Net_Transfer(PA) = PA_Net_Transfer(PA) + &
                                                    QUANTITY_OF_TRANSFER
               PA_Net_Transfer(BEST_BUYER_PA) = &
                                 PA_Net_Transfer(BEST_BUYER_PA) - &
                                                    QUANTITY_OF_TRANSFER
               TG_Net_Transfer(TG) = TG_Net_Transfer(TG) + &
                                                    QUANTITY_OF_TRANSFER
               TG_Net_Transfer(BEST_BUYER_TG) = &
                                 TG_Net_Transfer(BEST_BUYER_TG) - &
                                                    QUANTITY_OF_TRANSFER
            ELSE
            ENDIF
!
            DO L = I-1, 1, -1
               NEXT_CN = SORTED_OPTIONS(L)
               HIGHEST_OPTION = HGST_OPT_INDEX(NEXT_CN)
       call write_trace_int2(file_trace_amoc, "12. HO", HIGHEST_OPTION)
               NEXT_CN = I-1 - L + 1
!
! 042717. ADDED THIS LINE TO GET THE RIGHT VALUE FOR CAPACITY
!
               HIGHEST_OPTION = HGST_OPT_INDEX(NEXT_CN)
       call write_trace_int2(file_trace_amoc, "13. HO", HIGHEST_OPTION)
!
               IF(.NOT. DEPENDENT_UNIT_AVAILABLE(HIGHEST_OPTION)) then
                 call write_trace_message(file_trace_amoc, "1A. CYCLE")
               	 CYCLE
               end if
               BEST_SELLER_TG = GET_TRANS_GROUP_POSITION( &
                                 ns_cap_trans%transaction_group(HIGHEST_OPTION))
               IF(BEST_SELLER_TG /= TG) then
                  call write_trace_message(file_trace_amoc, "20. CYCLE")
                  CYCLE
               end if
               call write_trace_int2(file_trace_amoc, "BEST_SELLER", &
                BEST_SELLER_TG)
                
               IF(TG_MW_TRANSFERS) THEN
                  TG_CAPACITY_MW(TG) = &
                           TG_PLANNING_CAPACITY(TG) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG) - &
                                CAPACITY_PLANNING_MW(HIGHEST_OPTION) + &
                                          TG_Net_Transfer(TG)
               ELSE
                  TG_CAPACITY_MW(TG) = &
                           TG_PLANNING_CAPACITY(TG) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG) - &
                                    CAPACITY_PLANNING_MW(HIGHEST_OPTION)
               ENDIF
               IF(TG_PEAK_MW(TG) > .01) THEN
                  TG_RESERVE_MARGIN(TG) = 100. * &
                              (TG_CAPACITY_MW(TG)/TG_PEAK_MW(TG) - 1.0)
               ELSE
                  TG_RESERVE_MARGIN(TG) = 100.
               ENDIF
               PA_CAPACITY_MW(PA) = &
                           PA_PLANNING_CAPACITY(PA) + &
                                 TOTAL_ANNUAL_CAPACITY_ADDED(PA)
               IF(PA_PEAK_MW(PA) > .01) THEN
                  PA_RESERVE_MARGIN(PA) = 100. * &
                              (PA_CAPACITY_MW(PA)/PA_PEAK_MW(PA) - 1.0)
               ELSE
                  PA_RESERVE_MARGIN(PA) = 100.
               ENDIF
               TEMP_R4 = PA_RESERVE_MARGIN(PA)*0.01 + 1.0
               IF(TG_MW_TRANSFERS) THEN
                  CM_CAPACITY_MW(CM) = &
                           CM_PLANNING_CAPACITY(CM) + &
                                 CM_TOT_ANN_CAPACITY_ADDED(CM) - &
                                CAPACITY_PLANNING_MW(HIGHEST_OPTION) + &
                                          CM_Net_Transfer(CM)
               ELSE
                  CM_CAPACITY_MW(CM) = &
                           CM_PLANNING_CAPACITY(CM) + &
                                 CM_TOT_ANN_CAPACITY_ADDED(CM) - &
                                    CAPACITY_PLANNING_MW(HIGHEST_OPTION)
               ENDIF
               IF(CM_PEAK_MW(CM) > .01) THEN
                  CM_RESERVE_MARGIN(CM) = 100. * &
                              (CM_CAPACITY_MW(CM)/CM_PEAK_MW(CM) - 1.0)
               ELSE
                  CM_RESERVE_MARGIN(CM) = 100.
               ENDIF
!
               TEMP_R4 = CM_RESERVE_MARGIN(CM)*0.01 + 1.0
               
               if(R_YEAR==2 .and. CM==1) then
                    r_YEAR=r_YEAR ! Debugstop
               end if
               ICAP_DEMAND_CURVE_MULT(NEXT_CN) = &
                 GET_ICAP_REVENUE_MULT(R_YEAR,CM,TEMP_R4)
               IF(SCREEN_CAPACITY(HIGHEST_OPTION) > 0.01) THEN
                  EAS_PROFIT_PER_KWYR(NEXT_CN) = 0.001 * &
                        1000000. * (ENERGY_REVENUE_MM(NEXT_CN) - &
                                           VARIABLE_COST_MM(NEXT_CN))/ &
                                       SCREEN_CAPACITY(HIGHEST_OPTION)
                  IF(ICAP_SWITCH_INDEX(CM) == 2) THEN
                     CONE_COST_PER_KWYR(NEXT_CN) = &
                                             LOWEST_FIXED_COST_BY_CM(CM)
                  ELSE
                     CONE_COST_PER_KWYR(NEXT_CN) = ICAP_CONE_PRICE(CM)
                  ENDIF
                  CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN) = &
                                    CONE_COST_PER_KWYR(NEXT_CN) * &
                                  ICAP_DEMAND_CURVE_MULT(NEXT_CN)
                  TEMP_R4 = 1000.0 * &
                               (FIXED_COST_MM(NEXT_CN) + &
                                    VARIABLE_COST_MM(NEXT_CN) - &
                                       RPS_REVENUE_MM(NEXT_CN) - &
                                          ENERGY_REVENUE_MM(NEXT_CN))/ &
                                     SCREEN_CAPACITY(HIGHEST_OPTION)
                  IF(ICAP_SWITCH_INDEX(CM) == 2 .OR.   & !  MARGINAL
                                        ICAP_SWITCH_INDEX(CM) == 3) THEN
                                        ! CONE-MRX
                     ICAP_REVENUE_PER_KWYR(NEXT_CN) = &
                        MAX(0.0, &
                        MIN(CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN), &
                                                           TEMP_R4))
        call write_trace_int2(file_trace_amoc, "NEXT_CN", NEXT_CN)
        call write_trace_real4(file_trace_amoc, &
            "18. ICAP_REVENUE_PER_KWYR", &
            ICAP_REVENUE_PER_KWYR(NEXT_CN))
                  ELSE
                     ICAP_REVENUE_PER_KWYR(NEXT_CN) = &
                                                     ICAP_CONE_PRICE(CM)
        call write_trace_int2(file_trace_amoc, "NEXT_CN", NEXT_CN)
        call write_trace_real4(file_trace_amoc, &
            "19. ICAP_REVENUE_PER_KWYR", &
            ICAP_REVENUE_PER_KWYR(NEXT_CN))
                  ENDIF
               ELSE
                  EAS_PROFIT_PER_KWYR(NEXT_CN) = 0.0
                  CONE_COST_PER_KWYR(NEXT_CN) = 0.0
                  CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN) = &
                                                                  0.0
                  ICAP_REVENUE_PER_KWYR(NEXT_CN) = 0.0
        call write_trace_int2(file_trace_amoc, "NEXT_CN", NEXT_CN)
        call write_trace_real4(file_trace_amoc, &
            "20. ICAP_REVENUE_PER_KWYR", &
            ICAP_REVENUE_PER_KWYR(NEXT_CN))
               ENDIF
               MARGINAL_ICAP_PER_KWYR_BY_TG(TG) = &
                                          ICAP_REVENUE_PER_KWYR(NEXT_CN)
               EXIT
            ENDDO

         ENDDO ! MAIN ADD_THIS_UNIT LOOP WITH CONSTRAINTS
         
         ! TODO: Remove code in IF block
         if(1==2) then
            MaxTransmissionConstraint = TransmissionConstraint
            MaxTransferValue = 999.0
            LAST_SELLER = -1
            LAST_BUYER = -1
!
            TARGET_IS_PA = .FALSE.
            TG_MW_TRANSFERS = .TRUE.
            MAX_ITER = 20 * UPPER_TRANS_GROUP
            ITER = 0
            DO
               ITER = ITER + 1
               IF(ITER == MAX_ITER) then
               	EXIT
               end if
               TransferValue = -999.0
               MaxTransferValue = -999.0
               SELLER = 0
               BUYER = 0
                call write_trace_int2(file_trace_amoc, "4. SELLER", &
                    SELLER)
                   call write_trace_int2(file_trace_amoc, "4. BUYER", &
                       BUYER)
! IDENTIFY MaxTransferValue, SELLER, BUYER
               DO I = 1, UPPER_TRANS_GROUP
                  DO J = I-1,1,-1 ! DON'T CHECK DIAGONAL
                     IF(TG_PEAK_MW(I) > NEAR_ZERO) THEN
                        CM = TG_2_CAPACITY_MARKET(I)
                        TEMP_R = &
                           (TG_Net_Transfer(I) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(I) + &
                                             TG_PLANNING_CAPACITY(I))/ &
                                                       TG_PEAK_MW(I)
                call write_trace_real(file_trace_amoc, "18. TEMP_R", &
                        TEMP_R)
                        TransferValue(I,J) = TEMP_R
                     ELSE
                        TransferValue(I,J) = 999.0 ! INFINITY
                     ENDIF
                     IF(TG_PEAK_MW(J) > NEAR_ZERO) THEN
                        CM = TG_2_CAPACITY_MARKET(J)
                        TEMP_R = &
                           (TG_Net_Transfer(J) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(J) + &
                                             TG_PLANNING_CAPACITY(J))/ &
                                                       TG_PEAK_MW(J)
                call write_trace_real(file_trace_amoc, "19. TEMP_R", &
                        TEMP_R)
                        TransferValue(J,I) = TEMP_R
!
                     ELSE
                        TransferValue(J,I) = 999.0 ! INFINITY
                     ENDIF
                     IF(GRX_ITERATIONS == 3 .AND. &
                                               THIS_YEAR == 2014)THEN
                        THIS_YEAR = THIS_YEAR
                     ENDIF
                     PA = TG_2_PLANNING_AREA(I)
                     IF(TARGET_IS_PA) THEN
                        TEMP_R = PA_PLANNING_RESERVE_MARGIN(PA)
                call write_trace_real(file_trace_amoc, "20. TEMP_R", &
                        TEMP_R)
                     ELSE
                        TEMP_R = PA_PLANNING_RESERVE_MARGIN(0)
                call write_trace_real(file_trace_amoc, "21. TEMP_R", &
                        TEMP_R)
                     ENDIF
                     I_CAPACITY = MAX(0.0, &
                         TG_Net_Transfer(I) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(I) + &
                         TG_PLANNING_CAPACITY(I)- &
                                     CURRENT_TARGET_RATIO_PA(PA)* &
                                                     TG_PEAK_MW(I) )
                     PA = TG_2_PLANNING_AREA(J)
                     IF(TARGET_IS_PA) THEN
                        TEMP_R = PA_PLANNING_RESERVE_MARGIN(PA)
                call write_trace_real(file_trace_amoc, "22. TEMP_R", &
                        TEMP_R)
                     ELSE
                        TEMP_R = PA_PLANNING_RESERVE_MARGIN(0)
                call write_trace_real(file_trace_amoc, "23. TEMP_R", &
                        TEMP_R)
                     ENDIF
                     J_CAPACITY = MAX(0.0, &
                         TG_Net_Transfer(J) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(J) + &
                         TG_PLANNING_CAPACITY(J)- &
                                     CURRENT_TARGET_RATIO_PA(PA)* &
                                                     TG_PEAK_MW(J) )
                     IF(I_CAPACITY > 0.1 .OR. J_CAPACITY > 0.1) THEN
                      IF(TransferValue(I,J) > TransferValue(J,I) .AND. &
                                 I_CAPACITY > 0.1 .AND. &
                                 TransmissionConstraint(I,J) > 0.1) THEN
                           TEMP_R = &
                              TransferValue(I,J) - TransferValue(J,I)
                call write_trace_real(file_trace_amoc, "24. TEMP_R", &
                        TEMP_R)
                           IF(I == LAST_SELLER .AND. &
                                          J == LAST_BUYER) THEN
                              SAME_TRANSACTION = .TRUE.
                           ELSE
                              SAME_TRANSACTION = .FALSE.
                           ENDIF
                           IF(TEMP_R > MaxTransferValue .AND. &
                                             .NOT. SAME_TRANSACTION)THEN
                              SELLER = I
                              BUYER = J
                call write_trace_int2(file_trace_amoc, "5. SELLER", &
                    SELLER)
                   call write_trace_int2(file_trace_amoc, "5. BUYER", &
                       BUYER)
                              SELLER_CAPACITY = I_CAPACITY
                              BUYER_CAPACITY = J_CAPACITY
                              MaxTransferValue = TEMP_R
                           ENDIF
                        ELSEIF(TransmissionConstraint(J,I)> 0.1 .AND. &
                                                  J_CAPACITY > 0.1) THEN
                           TEMP_R = &
                              TransferValue(J,I) - TransferValue(I,J)
                call write_trace_real(file_trace_amoc, "25. TEMP_R", &
                        TEMP_R)
                           IF(J == LAST_SELLER .AND. &
                                          I == LAST_BUYER) THEN
                              SAME_TRANSACTION = .TRUE.
                           ELSE
                              SAME_TRANSACTION = .FALSE.
                           ENDIF
                           IF(TEMP_R > MaxTransferValue .AND. &
                                             .NOT. SAME_TRANSACTION)THEN
                              SELLER = J
                              BUYER = I
                call write_trace_int2(file_trace_amoc, "6. SELLER", &
                    SELLER)
                   call write_trace_int2(file_trace_amoc, "6. BUYER", &
                       BUYER)
                              SELLER_CAPACITY = J_CAPACITY
                              BUYER_CAPACITY = I_CAPACITY
                              MaxTransferValue = TEMP_R
                           ENDIF ! CHECK FOR IMPROVED SOLUTION
                        ENDIF ! I OR J SELLER
                     ENDIF ! CAPACITY CHECK
                  END DO
               END DO
!
               IF(SELLER == 0 .OR. BUYER == 0 .OR. &
                            MaxTransferValue < TransferConvergence) EXIT
!
! IDENTIFY QUANTITY TO TRANSFER
!
               TEMP_R = MIN( &
                         SELLER_CAPACITY, &
                         TransmissionConstraint(SELLER,BUYER))
                call write_trace_real(file_trace_amoc, "26. TEMP_R", &
                        TEMP_R)
! 081110.   WHAT IS THE CAPACITY THAT MAKES THE TWO RESERVE MARGINS
!
               X_SEARCH = TEMP_R
               HALF = X_SEARCH
               Y_BEFORE_SELLER = TransferValue(SELLER,BUYER)
               X_BEFORE_SELLER = TEMP_R
               Y_BEFORE_BUYER = TransferValue(BUYER,SELLER)
               X_BEFORE_BUYER = 0.0
               DO I = 1, 10
                  X_AFTER_SELLER = TEMP_R - X_SEARCH
                  Y_AFTER_SELLER = &
                      (TG_Net_Transfer(SELLER) + &
                              TG_TOTAL_ANNUAL_CAPACITY_ADDED(SELLER) + &
                       TG_PLANNING_CAPACITY(SELLER)-X_SEARCH)/ &
                                                 TG_PEAK_MW(SELLER)
                  X_AFTER_BUYER = X_SEARCH
                  Y_AFTER_BUYER = &
                      (TG_Net_Transfer(BUYER) + &
                              TG_TOTAL_ANNUAL_CAPACITY_ADDED(BUYER) + &
                       TG_PLANNING_CAPACITY(BUYER)+X_SEARCH)/ &
                                               TG_PEAK_MW(BUYER)
                  IF(ABS(Y_AFTER_SELLER - Y_AFTER_BUYER) < 0.01) THEN
                     TEMP_R = X_SEARCH
                call write_trace_real(file_trace_amoc, "27. TEMP_R", &
                        TEMP_R)
                     EXIT
                  ELSE
                     HALF = HALF * 0.5
                     IF(Y_AFTER_SELLER > Y_AFTER_BUYER) THEN
                        IF(I == 1) THEN
                           TEMP_R = X_SEARCH
                call write_trace_real(file_trace_amoc, "28. TEMP_R", &
                        TEMP_R)
                           EXIT
                        ELSE ! X_SEARCH IS TOO SMALL
                           X_SEARCH = X_SEARCH + HALF
                        ENDIF
                     ELSE ! X_SEARCH IS TOO BIG
                        X_SEARCH = X_SEARCH - HALF
                     ENDIF
                  ENDIF
               ENDDO
!
               IF(FirmTransfer(BUYER,SELLER) < 0.001) THEN
                  FirmTransfer(SELLER,BUYER) = &
                     FirmTransfer(SELLER,BUYER) + TEMP_R
               ELSE
                  IF(FirmTransfer(BUYER,SELLER) > TEMP_R) THEN
                     FirmTransfer(BUYER,SELLER) = &
                                     FirmTransfer(BUYER,SELLER) - TEMP_R
                  ELSE
                     FirmTransfer(SELLER,BUYER) = &
                                     TEMP_R - FirmTransfer(BUYER,SELLER)
                     FirmTransfer(BUYER,SELLER) = 0.0
                  ENDIF
               ENDIF
               TransmissionConstraint(SELLER,BUYER) = &
                        MaxTransmissionConstraint(SELLER,BUYER) - &
                                              FirmTransfer(SELLER,BUYER)
               TransmissionConstraint(BUYER,SELLER) = &
                        MaxTransmissionConstraint(BUYER,SELLER) - &
                                              FirmTransfer(BUYER,SELLER)
!
               TG_Net_Transfer(SELLER) = &
                                       TG_Net_Transfer(SELLER) - TEMP_R
               TG_Net_Transfer(BUYER) = TG_Net_Transfer(BUYER) + TEMP_R
               SELLER_CM = TG_2_CAPACITY_MARKET(SELLER)
               BUYER_CM = TG_2_CAPACITY_MARKET(BUYER)
         call write_trace_int2(file_trace_amoc, "3. buyer_cm", buyer_cm)
               SELLER_PA = TG_2_PLANNING_AREA(SELLER)
               BUYER_PA = TG_2_PLANNING_AREA(BUYER)
               IF(SELLER_CM /= BUYER_CM) THEN
                  CM_Net_Transfer(SELLER_CM) = &
                     CM_Net_Transfer(SELLER_CM) - TEMP_R
                  CM_Net_Transfer(BUYER_CM) = &
                     CM_Net_Transfer(BUYER_CM) +  TEMP_R
               ENDIF
               IF(SELLER_PA /= BUYER_PA) THEN
                  PA_Net_Transfer(SELLER_PA) = &
                     PA_Net_Transfer(SELLER_PA) - TEMP_R
                  PA_Net_Transfer(BUYER_PA) = &
                     PA_Net_Transfer(BUYER_PA) +  TEMP_R
               ENDIF
               LAST_SELLER = SELLER
               LAST_BUYER = BUYER
            END DO
!
! 040611. TO RECALCULATE EXISTING UNITS CAPACITY.
!
            DO CM = 1, CG
               IF(LAST_CM_RESOURCE_ADDED(CM) < 1) then
               	CYCLE
               end if
               DO I = HO, 1, -1
                  NEXT_CN = SORTED_OPTIONS(I)
                  HIGHEST_OPTION = HGST_OPT_INDEX(NEXT_CN)
       call write_trace_int2(file_trace_amoc, "14. HO", HIGHEST_OPTION)
                  IF(MRX_ICAP_INDEX(NEXT_CN) /= 1 .AND. &
                                    MRX_ICAP_INDEX(NEXT_CN) /= 10)then
                      CYCLE
                  end if
                  IF(.NOT. &
                         DEPENDENT_UNIT_AVAILABLE(HIGHEST_OPTION)) then
                     CYCLE
                   end if

                  TG = GET_TRANS_GROUP_POSITION( &
                       ns_cap_trans%transaction_group(HIGHEST_OPTION))
                  call write_trace_int2(file_trace_amoc, "11. TG", TG)
                  TEMP_CM = TG_2_CAPACITY_MARKET(TG)
!
                  IF(CM /= TEMP_CM) then
                      CYCLE
                  end if
!
                  TEMP_I2 = GET_REGIONAL_CM_NAME(CM,CM_NAME)
                  WRITE(LOCAL_NAME,1010) &
                            OPTION_NAME(HIGHEST_OPTION)(1:17),HO - I + 1
              call write_trace_string(file_trace_amoc, &
                "12. LOCAL_NAME", LOCAL_NAME)

                  MARKET_AVE_REVENUE = 0.
                  TOTAL_AVERAGE_COST = 0.
       call write_trace_real4(file_trace_amoc, "13. TAC/MAR", &
            0.)
                  IF(TG_MW_TRANSFERS) THEN
                     TG_CAPACITY_MW(TG) = &
                           TG_PLANNING_CAPACITY(TG) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG) + &
                                    TG_Net_Transfer(TG)
                  ELSE
                     TG_CAPACITY_MW(TG) = &
                           TG_PLANNING_CAPACITY(TG) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG)
                  ENDIF
                  IF(TG_PEAK_MW(TG) > .01) THEN
                     TG_RESERVE_MARGIN(TG) = 100. * &
                              (TG_CAPACITY_MW(TG)/TG_PEAK_MW(TG) - 1.0)
                  ELSE
                     TG_RESERVE_MARGIN(TG) = 100.
                  ENDIF
!
                  PA_CAPACITY_MW(CM) = &
                           PA_PLANNING_CAPACITY(CM) + &
                                 TOTAL_ANNUAL_CAPACITY_ADDED(CM)
                  IF(PA_PEAK_MW(CM) > .01) THEN
                     PA_RESERVE_MARGIN(CM) = 100. * &
                              (PA_CAPACITY_MW(CM)/PA_PEAK_MW(CM) - 1.0)
                  ELSE
                     PA_RESERVE_MARGIN(CM) = 100.
                  ENDIF
!
                  IF(TG_MW_TRANSFERS) THEN
                     CM_CAPACITY_MW(CM) = &
                           CM_PLANNING_CAPACITY(CM) + &
                                 CM_TOT_ANN_CAPACITY_ADDED(CM) + &
                                    CM_Net_Transfer(CM)
                  ELSE
                     CM_CAPACITY_MW(CM) = &
                           CM_PLANNING_CAPACITY(CM) + &
                                 CM_TOT_ANN_CAPACITY_ADDED(CM)
                  ENDIF
                  IF(CM_PEAK_MW(CM) > .01) THEN
                     CM_RESERVE_MARGIN(CM) = 100. * &
                              (CM_CAPACITY_MW(CM)/CM_PEAK_MW(CM) - 1.0)
                  ELSE
                     CM_RESERVE_MARGIN(CM) = 100.
                  ENDIF
!
                  TEMP_R4 = CM_RESERVE_MARGIN(CM)*0.01 + 1.0
                  
               if(R_YEAR==2 .and. CM==1) then
                    r_YEAR=r_YEAR ! Debugstop
               end if
                  ICAP_DEMAND_CURVE_MULT(NEXT_CN) = &
                     GET_ICAP_REVENUE_MULT(R_YEAR,CM,TEMP_R4)
                  IF(SCREEN_CAPACITY(HIGHEST_OPTION) > 0.01) THEN
                     EAS_PROFIT_PER_KWYR(NEXT_CN) = 0.001 * &
                        1000000. * (ENERGY_REVENUE_MM(NEXT_CN) - &
                                           VARIABLE_COST_MM(NEXT_CN))/ &
                                       SCREEN_CAPACITY(HIGHEST_OPTION)
                     IF(ICAP_SWITCH_INDEX(CM) == 2) THEN
                        CONE_COST_PER_KWYR(NEXT_CN) = &
                                             LOWEST_FIXED_COST_BY_CM(CM)
                     ELSE
                        CONE_COST_PER_KWYR(NEXT_CN) =ICAP_CONE_PRICE(CM)
                     ENDIF
                     CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN) = &
                                    CONE_COST_PER_KWYR(NEXT_CN) * &
                                  ICAP_DEMAND_CURVE_MULT(NEXT_CN)
!
! 111509. REDEFINED PER NORM.
!
                     TEMP_R4 = 1000.0 * &
                               (FIXED_COST_MM(NEXT_CN) + &
                                    VARIABLE_COST_MM(NEXT_CN) - &
                                       RPS_REVENUE_MM(NEXT_CN) - &
                                          ENERGY_REVENUE_MM(NEXT_CN))/ &
                                     SCREEN_CAPACITY(HIGHEST_OPTION)
!
! 101010. TAKING OUT THE VRR DEMAND CURVE MULT FROM THIS CALC
!         TO SMOOTH THE CURVES.
!
                     IF(ICAP_SWITCH_INDEX(CM) == 2 .OR.   & !  MARGINAL
                                        ICAP_SWITCH_INDEX(CM) == 3) THEN
                                        ! CONE-MRX
                        ICAP_REVENUE_PER_KWYR(NEXT_CN) = &
                           MAX(0.0, &
                          MIN(CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN), &
                                                           TEMP_R4))
        call write_trace_int2(file_trace_amoc, "NEXT_CN", NEXT_CN)
        call write_trace_real4(file_trace_amoc, &
            "21. ICAP_REVENUE_PER_KWYR", &
            ICAP_REVENUE_PER_KWYR(NEXT_CN))
                     ELSE
                        ICAP_REVENUE_PER_KWYR(NEXT_CN) = &
                                                     ICAP_CONE_PRICE(CM)
        call write_trace_int2(file_trace_amoc, "NEXT_CN", NEXT_CN)
        call write_trace_real4(file_trace_amoc, &
            "22. ICAP_REVENUE_PER_KWYR", &
            ICAP_REVENUE_PER_KWYR(NEXT_CN))
                     ENDIF
                  ELSE
                     EAS_PROFIT_PER_KWYR(NEXT_CN) = 0.0
                     CONE_COST_PER_KWYR(NEXT_CN) = 0.0
                     CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN) = &
                                                                  0.0
                     ICAP_REVENUE_PER_KWYR(NEXT_CN) = 0.0
        call write_trace_int2(file_trace_amoc, "NEXT_CN", NEXT_CN)
        call write_trace_real4(file_trace_amoc, &
            "23. ICAP_REVENUE_PER_KWYR", &
            ICAP_REVENUE_PER_KWYR(NEXT_CN))
                  ENDIF
                  CAPACITY_REVENUE_MM(NEXT_CN) = 0.001 * &
                                 ICAP_REVENUE_PER_KWYR(NEXT_CN) * &
                            CAPACITY_PLANNING_MW(HIGHEST_OPTION)
                  IF(HIGHEST_MRX_ENERGY(NEXT_CN) > 0.01) THEN
                     CAPACITY_REVENUE_PER_MWH(NEXT_CN) = &
                            CAPACITY_REVENUE_MM(NEXT_CN) * 1000.0/ &
                                            HIGHEST_MRX_ENERGY(NEXT_CN)
                  ELSE
                     CAPACITY_REVENUE_PER_MWH(NEXT_CN) = 0.0
                  ENDIF
                  HIGHEST_MARKET_REVENUE(NEXT_CN) = &
                                ENERGY_REVENUE_MM(NEXT_CN) + &
                                 RPS_REVENUE_MM(NEXT_CN) + &
                                     CAPACITY_REVENUE_MM(NEXT_CN)
                  REVENUE_PER_MWH(NEXT_CN) = &
                           HIGHEST_ENERGY_REV_PER_MWH(NEXT_CN) + &
                                CAPACITY_REVENUE_PER_MWH(NEXT_CN)
                  FIXED_COST_PER_MWH(NEXT_CN) = &
                        HIGHEST_FOM_PER_MWH(NEXT_CN) + &
                              HIGHEST_LEVEL_CAP_PER_MWH(NEXT_CN)
                  HIGHEST_MRX_MARGIN(NEXT_CN) = &
                              REVENUE_PER_MWH(NEXT_CN) - &
                                 HIGHEST_STRIKE_PRICE(NEXT_CN) - &
                                    FIXED_COST_PER_MWH(NEXT_CN)
                  IF(SCREEN_CAPACITY(HIGHEST_OPTION) > 0.01) THEN
                     HIGHEST_MARGIN_PER_KWYR(NEXT_CN) = 1000.0 * &
                              (HIGHEST_MARKET_REVENUE(NEXT_CN) - &
                                 VARIABLE_COST_MM(NEXT_CN) - &
                                       FIXED_COST_MM(NEXT_CN))/ &
                                 SCREEN_CAPACITY(HIGHEST_OPTION)
                  ELSE
                     HIGHEST_MARGIN_PER_KWYR(NEXT_CN) = -999999.0
                  ENDIF
               ENDDO ! HO COUNTER
            ENDDO ! CM
!
! 040511. RECONSTRUCT ICAP CALC FOR UNITS THAT HAVE BEEN BUILD
!
         endif
!
! END OF TEST THIS SECTION
!
         call write_trace_int2(file_trace_amoc, "CG", CG)
         
         DO CM = 1, CG
            CM_INDEX = GET_CM_INDEX_FROM_TG(CM)
            call write_trace_int2(file_trace_amoc, "CM_INDEX", CM_INDEX)
            call write_trace_int2(file_trace_amoc, "LAStRES ADDED", &
                LAST_CM_RESOURCE_ADDED(CM))
            call write_trace_bool1(file_trace_amoc, "MX Summary rpt.", &
                MX_SUMMARY_REPORT)

            
            IF(LAST_CM_RESOURCE_ADDED(CM) < 1 .AND. &
                                                 MX_SUMMARY_REPORT) THEN
!
               call write_trace_int2(file_trace_amoc, "16. HO", HO)
               DO I = HO, 1, -1
               call write_trace_int2(file_trace_amoc, "17. HO", HO)
! MAXIMUM RESERVE MARGIN CONDITION
!
                  NEXT_CN = SORTED_OPTIONS(I)
                  HIGHEST_OPTION = &
                                 HGST_OPT_INDEX(NEXT_CN)
       call write_trace_int2(file_trace_amoc, "15. HO", HIGHEST_OPTION)
!
                  IF(.NOT. &
                         DEPENDENT_UNIT_AVAILABLE(HIGHEST_OPTION)) then
                  call write_trace_message(file_trace_amoc, "20. cycle")
                          CYCLE
                  end if
!
                  TG = GET_TRANS_GROUP_POSITION( &
                      ns_cap_trans%transaction_group(HIGHEST_OPTION))
                  call write_trace_int2(file_trace_amoc, "12. TG", TG)
                  TEMP_CM = TG_2_CAPACITY_MARKET(TG)
                  IF(CM /= TEMP_CM) then
                    CYCLE
                  end if
                  EXIT
               ENDDO

               IF(CM /= TEMP_CM) then
                CYCLE
               end if
               
               LAST_CM_RESOURCE_ADDED(CM) = NEXT_CN ! HIGHEST_OPTION
               
               TEMP_I2 = GET_REGIONAL_CM_NAME(CM,CM_NAME)
               WRITE(LOCAL_NAME,1010) &
                            OPTION_NAME(HIGHEST_OPTION)(1:17),HO - I + 1
              call write_trace_string(file_trace_amoc, &
                "14. LOCAL_NAME", LOCAL_NAME)
!
               MARKET_AVE_REVENUE = 0.
               TOTAL_AVERAGE_COST = 0.
       call write_trace_real4(file_trace_amoc, "14. TAC/MAR", &
            0.)
!
               IF(TG_MW_TRANSFERS) THEN
                  TG_CAPACITY_MW(TG) = &
                           TG_PLANNING_CAPACITY(TG) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG) + &
                                    TG_Net_Transfer(TG)
               ELSE
                  TG_CAPACITY_MW(TG) = &
                           TG_PLANNING_CAPACITY(TG) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG)
               ENDIF
!
               IF(TG_PEAK_MW(TG) > .01) THEN
                  TG_RESERVE_MARGIN(TG) = 100. * &
                              (TG_CAPACITY_MW(TG)/TG_PEAK_MW(TG) - 1.0)
               ELSE
                  TG_RESERVE_MARGIN(TG) = 100.
               ENDIF
!
               PA_CAPACITY_MW(CM) = &
                           PA_PLANNING_CAPACITY(CM) + &
                                 TOTAL_ANNUAL_CAPACITY_ADDED(CM)
               IF(PA_PEAK_MW(CM) > .01) THEN
                  PA_RESERVE_MARGIN(CM) = 100. * &
                              (PA_CAPACITY_MW(CM)/PA_PEAK_MW(CM) - 1.0)
               ELSE
                  PA_RESERVE_MARGIN(CM) = 100.
               ENDIF
!
               IF(TG_MW_TRANSFERS) THEN
                  CM_CAPACITY_MW(CM) = &
                           CM_PLANNING_CAPACITY(CM) + &
                                 CM_TOT_ANN_CAPACITY_ADDED(CM) + &
                                    CM_Net_Transfer(CM)
               ELSE
                  CM_CAPACITY_MW(CM) = &
                           CM_PLANNING_CAPACITY(CM) + &
                                 CM_TOT_ANN_CAPACITY_ADDED(CM)
               ENDIF
               IF(CM_PEAK_MW(CM) > .01) THEN
                  CM_RESERVE_MARGIN(CM) = 100. * &
                              (CM_CAPACITY_MW(CM)/CM_PEAK_MW(CM) - 1.0)
                  CM_INTERUPTIBLRESERVE_MARGIN = 100. * &
                         (CM_CAPACITY_MW(CM) - &
                                    CM_PEAK_AFTER_INTERRUPTIBLE(CM)) / &
                                CM_PEAK_AFTER_INTERRUPTIBLE(CM)
               ELSE
                  CM_RESERVE_MARGIN(CM) = 100.
               ENDIF
!
               TEMP_R4 = CM_RESERVE_MARGIN(CM)*0.01 + 1.0
               TEMP_R4 = CM_INTERUPTIBLRESERVE_MARGIN*0.01 + 1.0
               
               if(R_YEAR==2 .and. CM==1) then
                    r_YEAR=r_YEAR ! Debugstop
               end if
               ICAP_DEMAND_CURVE_MULT(NEXT_CN) = &
                 GET_ICAP_REVENUE_MULT(R_YEAR,CM,TEMP_R4)
               IF(SCREEN_CAPACITY(HIGHEST_OPTION) > 0.01) THEN
                  EAS_PROFIT_PER_KWYR(NEXT_CN) = 0.001 * &
                        1000000. * (ENERGY_REVENUE_MM(NEXT_CN) - &
                                           VARIABLE_COST_MM(NEXT_CN))/ &
                                       SCREEN_CAPACITY(HIGHEST_OPTION)
!
                  IF(ICAP_SWITCH_INDEX(CM) == 2) THEN
                     CONE_COST_PER_KWYR(NEXT_CN) = &
                                             LOWEST_FIXED_COST_BY_CM(CM)
                  ELSE
                     CONE_COST_PER_KWYR(NEXT_CN) = ICAP_CONE_PRICE(CM)
                  ENDIF
!
                  CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN) = &
                                    CONE_COST_PER_KWYR(NEXT_CN) * &
                                  ICAP_DEMAND_CURVE_MULT(NEXT_CN)
!
! 111509. REDEFINED PER NORM.
!
                  TEMP_R4 = 1000.0 * &
                               (FIXED_COST_MM(NEXT_CN) + &
                                    VARIABLE_COST_MM(NEXT_CN) - &
                                       RPS_REVENUE_MM(NEXT_CN) - &
                                          ENERGY_REVENUE_MM(NEXT_CN))/ &
                                     SCREEN_CAPACITY(HIGHEST_OPTION)
!
! 101010. TAKING OUT THE VRR DEMAND CURVE MULT FROM THIS CALC
!         TO SMOOTH THE CURVES.
!
                  IF(ICAP_SWITCH_INDEX(CM) == 2 .OR.   & !  MARGINAL
                                        ICAP_SWITCH_INDEX(CM) == 3) THEN
                                        ! CONE-MRX
                     ICAP_REVENUE_PER_KWYR(NEXT_CN) = &
                           MAX(0.0, &
                          MIN(CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN), &
                                                           TEMP_R4))
        call write_trace_int2(file_trace_amoc, "NEXT_CN", NEXT_CN)
        call write_trace_real4(file_trace_amoc, &
            "24. ICAP_REVENUE_PER_KWYR", &
            ICAP_REVENUE_PER_KWYR(NEXT_CN))
                  ELSE
                     ICAP_REVENUE_PER_KWYR(NEXT_CN) = &
                                                     ICAP_CONE_PRICE(CM)
                     ICAP_REVENUE_PER_KWYR(NEXT_CN) = &
                                 MIN(ICAP_CONE_PRICE(CM), &
                                CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN))
        call write_trace_int2(file_trace_amoc, "CM", CM)
        call write_trace_real4(file_trace_amoc, "ICAP_CONE_PRICE", &
            icap_cone_price(CM))
            
        call write_trace_int2(file_trace_amoc, "NEXT_CN", NEXT_CN)
        call write_trace_real4(file_trace_amoc, &
            "25. ICAP_REVENUE_PER_KWYR", &
            ICAP_REVENUE_PER_KWYR(NEXT_CN))
                  ENDIF

               ELSE
                  EAS_PROFIT_PER_KWYR(NEXT_CN) = 0.0
                  CONE_COST_PER_KWYR(NEXT_CN) = 0.0
                  CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN) = &
                                                                  0.0
                  ICAP_REVENUE_PER_KWYR(NEXT_CN) = 0.0
        call write_trace_int2(file_trace_amoc, "NEXT_CN", NEXT_CN)
        call write_trace_real4(file_trace_amoc, &
            "26. ICAP_REVENUE_PER_KWYR", &
            ICAP_REVENUE_PER_KWYR(NEXT_CN))
               ENDIF
!
! 081719. REMOVED RETIREMENT ICAP.
! 091210. ADDED NEW UNIT
!
         call write_trace_real4(file_trace_amoc, "Rev/KW/YR", &
            ICAP_REVENUE_PER_KWYR(NEXT_CN))
          
               IF(ICAP_REVENUE_PER_KWYR(NEXT_CN) < 0.001) THEN
      call write_trace_real4(file_trace_amoc, "ICAP_REVENUE_PER_KWYR<<", &
        ICAP_REVENUE_PER_KWYR(NEXT_CN))
           

        call write_trace_int2(file_trace_amoc, "(NEXT_CN)", next_cn)
                  MRX_ICAP_INDEX(NEXT_CN) = 3
            call write_trace_real4(file_trace_amoc, &
          "1. ICAP_DEMAND_CURVE_MULT", ICAP_DEMAND_CURVE_MULT(NEXT_CN))

            
                  IF(ICAP_DEMAND_CURVE_MULT(NEXT_CN) < 0.1) THEN
                     IF(ICAP_SWITCH_INDEX(CM) == 2 .OR.   & !  MARGINAL
                                        ICAP_SWITCH_INDEX(CM) == 3) THEN
                                        ! CONE-MRX
                        ICAP_REVENUE_PER_KWYR(NEXT_CN) = &
                           MAX(0.0, &
                                    GET_CM_RETIREMENT_KW(CM,CM_UNIT_NO))
        call write_trace_int2(file_trace_amoc, "NEXT_CN", NEXT_CN)
        call write_trace_real4(file_trace_amoc, &
            "27. ICAP_REVENUE_PER_KWYR", &
            ICAP_REVENUE_PER_KWYR(NEXT_CN))
                     ELSE
                        ICAP_REVENUE_PER_KWYR(NEXT_CN) = &
                                                     ICAP_CONE_PRICE(CM)
        call write_trace_int2(file_trace_amoc, "NEXT_CN", NEXT_CN)
        call write_trace_real4(file_trace_amoc, &
            "28. ICAP_REVENUE_PER_KWYR", &
            ICAP_REVENUE_PER_KWYR(NEXT_CN))
                     ENDIF
                  ELSE
                     IF(ICAP_SWITCH_INDEX(CM) == 2 .OR.   & !  MARGINAL
                                        ICAP_SWITCH_INDEX(CM) == 3) THEN
                                        ! CONE-MRX
                        ICAP_REVENUE_PER_KWYR(NEXT_CN) = &
                           MAX(0.0, &
                             MIN(ICAP_REVENUE_PER_KWYR(NEXT_CN), &
                                GET_CM_RETIREMENT_KW(CM,CM_UNIT_NO)))
        call write_trace_int2(file_trace_amoc, "NEXT_CN", NEXT_CN)
        call write_trace_real4(file_trace_amoc, &
            "29. ICAP_REVENUE_PER_KWYR", &
            ICAP_REVENUE_PER_KWYR(NEXT_CN))
                     ELSE
                        ICAP_REVENUE_PER_KWYR(NEXT_CN) = &
                                                     ICAP_CONE_PRICE(CM)
        call write_trace_int2(file_trace_amoc, "NEXT_CN", NEXT_CN)
        call write_trace_real4(file_trace_amoc, &
            "30. ICAP_REVENUE_PER_KWYR", &
            ICAP_REVENUE_PER_KWYR(NEXT_CN))
                     ENDIF
                  ENDIF

                  IF(CM_UNIT_NO > 0) THEN
                     WRITE(4,*) 'MARGINAL ICAP RETIREMENT UNIT', &
                                    UNITNM(CM_UNIT_NO),'ICAP = ', &
                                    ICAP_REVENUE_PER_KWYR(NEXT_CN), &
                                    'Capacity Market =',CM_NAME
                  ELSE
                    WRITE(4,*) 'NO MARGINAL ICAP RETIREMENT UNIT FOR ', &
                                'CAPACITY MARKET ',CM_NAME
                     IF(ICAP_SWITCH_INDEX(CM) == 2) THEN
                        ICAP_REVENUE_PER_KWYR(NEXT_CN) = &
                                 CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN)
        call write_trace_int2(file_trace_amoc, "NEXT_CN", NEXT_CN)
        call write_trace_real4(file_trace_amoc, &
            "31. ICAP_REVENUE_PER_KWYR", &
            ICAP_REVENUE_PER_KWYR(NEXT_CN))
                     ELSE
                        ICAP_REVENUE_PER_KWYR(NEXT_CN) = &
                                                     ICAP_CONE_PRICE(CM)
        call write_trace_int2(file_trace_amoc, "NEXT_CN", NEXT_CN)
        call write_trace_real4(file_trace_amoc, &
            "32. ICAP_REVENUE_PER_KWYR", &
            ICAP_REVENUE_PER_KWYR(NEXT_CN))
                     ENDIF
                  ENDIF
               ELSE
                  if(MRX_ICAP_INDEX(NEXT_CN) == 0) &
                                    MRX_ICAP_INDEX(NEXT_CN)= 2

               ENDIF
!
! 110709. ADDED THIS IN THIS SECTION.
!
               CAPACITY_REVENUE_MM(NEXT_CN) = 0.001 * &
                                 ICAP_REVENUE_PER_KWYR(NEXT_CN) * &
                            CAPACITY_PLANNING_MW(HIGHEST_OPTION)
               HIGHEST_MARKET_REVENUE(NEXT_CN) = &
                                ENERGY_REVENUE_MM(NEXT_CN) + &
                                 RPS_REVENUE_MM(NEXT_CN) + &
                                     CAPACITY_REVENUE_MM(NEXT_CN)
               IF(HIGHEST_MRX_ENERGY(NEXT_CN) > 0.01) THEN
                  CAPACITY_REVENUE_PER_MWH(NEXT_CN) = &
                            CAPACITY_REVENUE_MM(NEXT_CN) * 1000.0/ &
                                            HIGHEST_MRX_ENERGY(NEXT_CN)
                  REVENUE_PER_MWH(NEXT_CN) = &
                           HIGHEST_ENERGY_REV_PER_MWH(NEXT_CN) + &
                                CAPACITY_REVENUE_PER_MWH(NEXT_CN)
!
                  FIXED_COST_PER_MWH(NEXT_CN) = &
                        HIGHEST_FOM_PER_MWH(NEXT_CN) + &
                              HIGHEST_LEVEL_CAP_PER_MWH(NEXT_CN)
                  HIGHEST_MRX_MARGIN(NEXT_CN) = &
                              REVENUE_PER_MWH(NEXT_CN) - &
                                 HIGHEST_STRIKE_PRICE(NEXT_CN) - &
                                    FIXED_COST_PER_MWH(NEXT_CN)
               ELSE
                  REVENUE_PER_MWH(NEXT_CN) = 0.0
                  FIXED_COST_PER_MWH(NEXT_CN) = 0.0
                  HIGHEST_MRX_MARGIN(NEXT_CN) = 0.0
                  CAPACITY_REVENUE_PER_MWH(NEXT_CN) = 0.0
               ENDIF
               IF(SCREEN_CAPACITY(HIGHEST_OPTION) > 0.01) THEN
                  HIGHEST_MARGIN_PER_KWYR(NEXT_CN) = 1000.0 * &
                              (HIGHEST_MARKET_REVENUE(NEXT_CN) - &
                                 VARIABLE_COST_MM(NEXT_CN) - &
                                       FIXED_COST_MM(NEXT_CN))/ &
                                 SCREEN_CAPACITY(HIGHEST_OPTION)
               ELSE
                  HIGHEST_MARGIN_PER_KWYR(NEXT_CN) = -999999.0
               ENDIF
               MX_ANNUAL_ALT_REC = RPTREC(MX_ANNUAL_ALT_NO)
               call write_trace_int2(file_trace_amoc, &
                   "Writing to MX_ANNUAL_ALT_NO", &
                   MX_ANNUAL_ALT_NO)

               if(file_trace_amoc/=BAD_TRACE_HANDLE) then
                    write(file_trace_amoc,*) &
                    "NEXT_CN ", NEXT_CN, &
                    "HIGHEST_OPTION ", HIGHEST_OPTION, &
                    "TG ", TG, "CM ", CM, &
                    "MX_ANNUAL_ALT_REC ", MX_ANNUAL_ALT_REC, &
                        "PRT_ENDPOINT ", PRT_ENDPOINT(), &
                        "THIS_YEAR ", FLOAT(THIS_YEAR), &
                        "CM_NAME ", CM_NAME, &
                        "LOCAL_NAME ", LOCAL_NAME, &
       "HIGHEST_STRIKE_PRICE ", HIGHEST_STRIKE_PRICE(NEXT_CN), &
       "HIGHEST_ENERGY_REV_PER_MWH ", &
       HIGHEST_ENERGY_REV_PER_MWH(NEXT_CN), & 
       "HIGHEST_MRX_MARGIN ", HIGHEST_MRX_MARGIN(NEXT_CN), &
          "HIGHEST_MRX_CF ", HIGHEST_MRX_CF(NEXT_CN), &
        "ENERGY_REVENUE_MM ", ENERGY_REVENUE_MM(NEXT_CN), &
        "HIGHEST_MARKET_COST ", HIGHEST_MARKET_COST(NEXT_CN), & !  6
         "VARIABLE_COST_MM ", VARIABLE_COST_MM(NEXT_CN), &
         "FIXED_COST_MM ", FIXED_COST_MM(NEXT_CN), &
         "SCREEN_CAPACITY ", SCREEN_CAPACITY(HIGHEST_OPTION), &
         "HIGHEST_MRX_ENERGY ", HIGHEST_MRX_ENERGY(NEXT_CN), & !  10
         "TOTAL_AVERAGE_COST ", TOTAL_AVERAGE_COST, & 
          "HIGHEST_ANN_LEVEL_CAP ", HIGHEST_ANN_LEVEL_CAP(NEXT_CN), &
          "HIGHEST_FUEL_COST ", HIGHEST_FUEL_COST(NEXT_CN), &
          "HIGHEST_ANNUAL_UNITS ", HIGHEST_ANNUAL_UNITS(NEXT_CN), &
          "CAPACITY_REVENUE_MM ", CAPACITY_REVENUE_MM(NEXT_CN), &
          "HIGHEST_MARKET_REVENUE ", HIGHEST_MARKET_REVENUE(NEXT_CN), & 
          "HIGHEST_MARGIN_PER_KWYR ", HIGHEST_MARGIN_PER_KWYR(NEXT_CN), &
       "HIGHEST_FUEL_COST_PER_MWH ", HIGHEST_FUEL_COST_PER_MWH(NEXT_CN), &
          "HIGHEST_VAR_OM_PER_MWH ", HIGHEST_VAR_OM_PER_MWH(NEXT_CN), & !  19
          "HIGHEST_FUEL_COST_PER_MMBTU ", &
          HIGHEST_FUEL_COST_PER_MMBTU(NEXT_CN), &
          "REVENUE_PER_MWH ", REVENUE_PER_MWH(NEXT_CN), &
         "CAPACITY_REVENUE_PER_MWH ", CAPACITY_REVENUE_PER_MWH(NEXT_CN), &
       "HIGHEST_EMISSIONS_PER_MWH ", HIGHEST_EMISSIONS_PER_MWH(NEXT_CN), &
          "HIGHEST_FOM_PER_MWH ", HIGHEST_FOM_PER_MWH(NEXT_CN), & !  24
      "HIGHEST_LEVEL_CAP_PER_MWH ", HIGHEST_LEVEL_CAP_PER_MWH(NEXT_CN), &
          "HIGHEST_VOM_COST_MM ", HIGHEST_VOM_COST_MM(NEXT_CN), &
      "HIGHEST_EMISSIONS_COST_MM ", HIGHEST_EMISSIONS_COST_MM(NEXT_CN), & !  27
          "HIGHEST_FOM_COST_MM ", HIGHEST_FOM_COST_MM(NEXT_CN), &
          "transaction_group ", &
          FLOAT(ns_cap_trans%transaction_group(HIGHEST_OPTION)), &
          "TG_CAPACITY_MW ", TG_CAPACITY_MW(TG), &
          "TG_PEAK_MW ", TG_PEAK_MW(TG), &
          "TG_RESERVE_MARGIN ", TG_RESERVE_MARGIN(TG), &
          "CM_INDEX ", FLOAT(CM_INDEX), & 
          "CM_CAPACITY_MW ", CM_CAPACITY_MW(CM), &
          "CM_PEAK_MW ", CM_PEAK_MW(CM), &
          "CM_RESERVE_MARGIN ", CM_RESERVE_MARGIN(CM), &
          "ICAP_DEMAND_CURVE_MULT ", ICAP_DEMAND_CURVE_MULT(NEXT_CN), &
          "CONE_COST_PER_KWYR ", CONE_COST_PER_KWYR(NEXT_CN), & !  38
          "CONE_DEMAND_CURVE_ADJ_PER_KWYR ", &
            CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN), &
          "EAS_PROFIT_PER_KWYR ", EAS_PROFIT_PER_KWYR(NEXT_CN), &
          "ICAP_REVENUE_PER_KWYR ", ICAP_REVENUE_PER_KWYR(NEXT_CN), &
          "HIGHEST_HEAT_RATE ", HIGHEST_HEAT_RATE(NEXT_CN), & !  42
          "FIXED_COST_PER_MWH ", FIXED_COST_PER_MWH(NEXT_CN), &
          "HIGHEST_RFT ", HIGHEST_RFT(NEXT_CN), &
          "HIGHEST_RPM ", HIGHEST_RPM(NEXT_CN), &
          "TG_Net_Transfer ", TG_Net_Transfer(TG), &
          "CM_Net_Transfer ", CM_Net_Transfer(CM), &
          "MRX_ICAP_INDEX ", MRX_ICAP_INDEX(NEXT_CN), &
      "CM_INTERUPTIBLRESERVE_MARGIN ", CM_INTERUPTIBLRESERVE_MARGIN, &
      "CM_PEAK_AFTER_INTERRUPTIBLE ", CM_PEAK_AFTER_INTERRUPTIBLE(CM), &
       "CAPACITY_PLANNING_MW ", CAPACITY_PLANNING_MW(HIGHEST_OPTION), &
          "RPS_REVENUE_MM ", RPS_REVENUE_MM(NEXT_CN), &
          "RPS_REVENUE_PER_MWH ", RPS_REVENUE_PER_MWH(NEXT_CN)
               end if

                     ! TODO: Combined writes to one in separate routine
               WRITE(MX_ANNUAL_ALT_NO,REC=MX_ANNUAL_ALT_REC) &
                        PRT_ENDPOINT(), &
                        FLOAT(THIS_YEAR), &
                        CM_NAME, &
                        LOCAL_NAME, &
                        HIGHEST_STRIKE_PRICE(NEXT_CN), &
                        HIGHEST_ENERGY_REV_PER_MWH(NEXT_CN), & 
                        ! 110309. REDEFINED.
                        HIGHEST_MRX_MARGIN(NEXT_CN), &
                        HIGHEST_MRX_CF(NEXT_CN), &
                        ENERGY_REVENUE_MM(NEXT_CN), &
                        HIGHEST_MARKET_COST(NEXT_CN), & !  6
                        VARIABLE_COST_MM(NEXT_CN), &
                        FIXED_COST_MM(NEXT_CN), &
                        SCREEN_CAPACITY(HIGHEST_OPTION), &
                        HIGHEST_MRX_ENERGY(NEXT_CN), & !  10
                        TOTAL_AVERAGE_COST, & !  NEED DEFINITION
                        HIGHEST_ANN_LEVEL_CAP(NEXT_CN), &
                        HIGHEST_FUEL_COST(NEXT_CN), &
                        HIGHEST_ANNUAL_UNITS(NEXT_CN), &
                        CAPACITY_REVENUE_MM(NEXT_CN), &
                        HIGHEST_MARKET_REVENUE(NEXT_CN), & 
                        ! 16 TOTAL REVENUE
                        HIGHEST_MARGIN_PER_KWYR(NEXT_CN), &
                        HIGHEST_FUEL_COST_PER_MWH(NEXT_CN), &
                        HIGHEST_VAR_OM_PER_MWH(NEXT_CN), & !  19
                        HIGHEST_FUEL_COST_PER_MMBTU(NEXT_CN), &
                        REVENUE_PER_MWH(NEXT_CN), &
                        CAPACITY_REVENUE_PER_MWH(NEXT_CN), &
                        HIGHEST_EMISSIONS_PER_MWH(NEXT_CN), &
                        HIGHEST_FOM_PER_MWH(NEXT_CN), & !  24
                        HIGHEST_LEVEL_CAP_PER_MWH(NEXT_CN), &
                        HIGHEST_VOM_COST_MM(NEXT_CN), &
                        HIGHEST_EMISSIONS_COST_MM(NEXT_CN), & !  27
                        HIGHEST_FOM_COST_MM(NEXT_CN), &
                        FLOAT(ns_cap_trans%transaction_group(HIGHEST_OPTION)), &
                        TG_CAPACITY_MW(TG), &
                        TG_PEAK_MW(TG), &
                        TG_RESERVE_MARGIN(TG), &
                        FLOAT(CM_INDEX), & !  PLANNING_AREA_ID(NEXT_CN),
                        CM_CAPACITY_MW(CM), &
                        CM_PEAK_MW(CM), &
                        CM_RESERVE_MARGIN(CM), &
                        ICAP_DEMAND_CURVE_MULT(NEXT_CN), &
                        CONE_COST_PER_KWYR(NEXT_CN), & !  38
                        CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN), &
                        EAS_PROFIT_PER_KWYR(NEXT_CN), &
                        ICAP_REVENUE_PER_KWYR(NEXT_CN), &
                        HIGHEST_HEAT_RATE(NEXT_CN), & !  42
                        FIXED_COST_PER_MWH(NEXT_CN), &
                        HIGHEST_RFT(NEXT_CN), &
                        HIGHEST_RPM(NEXT_CN), &
                        TG_Net_Transfer(TG), &
                        CM_Net_Transfer(CM), &
                        MRX_ICAP_INDEX(NEXT_CN), &
                        CM_INTERUPTIBLRESERVE_MARGIN, &
                        CM_PEAK_AFTER_INTERRUPTIBLE(CM), &
                        CAPACITY_PLANNING_MW(HIGHEST_OPTION), &
                        RPS_REVENUE_MM(NEXT_CN), &
                        RPS_REVENUE_PER_MWH(NEXT_CN)
               MX_ANNUAL_ALT_REC = MX_ANNUAL_ALT_REC + 1
            ENDIF

            PA = CM
            IF(PA_PEAK_AFTER_INTERRUPTIBLE(PA) > NEAR_ZERO)  THEN
               PA_PLANNING_RESERVE_MARGIN(PA) = &
                        1. + (PA_PLANNING_CAPACITY(PA) + &
                               TOTAL_ANNUAL_CAPACITY_ADDED(PA) - &
                                 PA_PEAK_AFTER_INTERRUPTIBLE(PA)) / &
                                      PA_PEAK_AFTER_INTERRUPTIBLE(PA)
            ENDIF
            IF(CM_PEAK_AFTER_INTERRUPTIBLE(CM) > NEAR_ZERO)  THEN
               CM_PLANNING_RESERVE_MARGIN(CM) = & !  100410.
                                        CM_RESERVE_MARGIN(CM)*0.01 + 1.0
            ENDIF
            IF(MINIMUM_ANNUAL_MRX_CAP(PA)  <= .1 + &
                                  TOTAL_ANNUAL_CAPACITY_ADDED(PA)) CYCLE
            WRITE(4,*) "MINIMUM PLANNING AREA RESERVE NOT MET",PA
         ENDDO
!
! ADD ADDITIONS HIGHEST TO LOWEST WITH CAPACITY AND RM CONSTRAINTS
!
      RETURN ! END ENTRY ANNUAL_MARKET_OPTION_COST


      ENTRY GET_TG_TRANSFER_CAPACITY(R_TG,R_TG_TRANSFER_CAPACITY)!082610

         IF(ALLOCATED(TG_Net_Transfer) .AND. R_TG > 0) THEN
            R_TG_TRANSFER_CAPACITY =  TG_Net_Transfer(R_TG)
         ELSE
            R_TG_TRANSFER_CAPACITY =  0.0
         ENDIF
      RETURN


      ENTRY GET_MRX_ICAP_REPORT_VALUES( R_PA, &
                                        R_MRX_CAPACITY, &
                                        R_MRX_ENERGY, &
                                        R_MRX_TOTAL_REVENUE, &
                                        R_MRX_TOTAL_VARIABLE, &
                                        R_MRX_FUEL, &
                                        R_MRX_VOM, &
                                        R_MRX_EMISSIONS, &
                                        R_MRX_TOTAL_COST, &
                                        R_MRX_ANN_CAP_COST, &
                                        R_MRX_NET_MARGIN_MM, &
                                        R_MRX_NET_MARGIN_PER_MWH, &
                                        R_MRX_NET_MARGIN_PER_KWMO, &
                                        R_MRX_CONE_PER_KWMO, &
                                        R_MRX_EAS_REV_PER_KWMO, &
                                        R_MRX_ICAP_VALUE_POINT)
!
         IF(ALLOCATED(INSTALL_CAP_VALUE_LAST_CM_UNIT) .AND. &
                                             R_PA > 0 .AND. &
                                  LAST_CM_RESOURCE_ADDED(R_PA) > 0) THEN
!
            NEXT_CN = LAST_CM_RESOURCE_ADDED(R_PA)
            R_MRX_CAPACITY = HIGHEST_SCREEN_CAPACITY(NEXT_CN)
            R_MRX_ENERGY = HIGHEST_MRX_ENERGY(NEXT_CN)
            R_MRX_TOTAL_REVENUE = HIGHEST_MARKET_REVENUE(NEXT_CN)
            R_MRX_TOTAL_VARIABLE = VARIABLE_COST_MM(NEXT_CN)
            R_MRX_FUEL = HIGHEST_FUEL_COST(NEXT_CN)
            R_MRX_VOM = HIGHEST_VOM_COST_MM(NEXT_CN)
            R_MRX_EMISSIONS = HIGHEST_EMISSIONS_COST_MM(NEXT_CN)
            R_MRX_TOTAL_COST = VARIABLE_COST_MM(NEXT_CN) + &
                                           FIXED_COST_MM(NEXT_CN)
            R_MRX_ANN_CAP_COST = &
                                HIGHEST_ANN_LEVEL_CAP(NEXT_CN)
            R_MRX_NET_MARGIN_MM = HIGHEST_MARKET_REVENUE(NEXT_CN) - &
                                     VARIABLE_COST_MM(NEXT_CN) - &
                                           FIXED_COST_MM(NEXT_CN)
            R_MRX_NET_MARGIN_PER_MWH = HIGHEST_MRX_MARGIN(NEXT_CN)
            R_MRX_NET_MARGIN_PER_KWMO = &
                                     HIGHEST_MARGIN_PER_KWYR(NEXT_CN)
            R_MRX_CONE_PER_KWMO = CONE_COST_PER_KWYR(NEXT_CN)
            R_MRX_EAS_REV_PER_KWMO = EAS_PROFIT_PER_KWYR(NEXT_CN)
            R_MRX_ICAP_VALUE_POINT = ICAP_REVENUE_PER_KWYR(NEXT_CN)
         ELSE
            R_MRX_CAPACITY = 0.0
            R_MRX_ENERGY = 0.0
            R_MRX_TOTAL_REVENUE = 0.0
            R_MRX_TOTAL_VARIABLE = 0.0
            R_MRX_FUEL = 0.0
            R_MRX_VOM = 0.0
            R_MRX_EMISSIONS = 0.0
            R_MRX_TOTAL_COST = 0.0
            R_MRX_ANN_CAP_COST = 0.0
            R_MRX_NET_MARGIN_MM = 0.0
            R_MRX_NET_MARGIN_PER_MWH = 0.0
            R_MRX_NET_MARGIN_PER_KWMO = 0.0
            R_MRX_CONE_PER_KWMO = 0.0
            R_MRX_EAS_REV_PER_KWMO = 0.0
            R_MRX_ICAP_VALUE_POINT = 0.0
         ENDIF
      RETURN


      ENTRY GET_INSTALL_CAP_VALUE_LAST_PA(R_PA,R_RESERVE)

         IF(ALLOCATED(INSTALL_CAP_VALUE_LAST_PA_UNIT)) THEN
            R_RESERVE = &
                  INSTALL_CAP_VALUE_LAST_PA_UNIT(R_PA)
         ELSE
            R_RESERVE = 0.0
         ENDIF
      RETURN


      ENTRY GET_INSTALL_CAP_VALUE_LAST_CM(R_PA,R_RESERVE)

         IF(ALLOCATED(INSTALL_CAP_VALUE_LAST_CM_UNIT)) THEN
            R_RESERVE = &
                  INSTALL_CAP_VALUE_LAST_CM_UNIT(R_PA)
         ELSE
            R_RESERVE = 0.0
         ENDIF
      RETURN


      ENTRY GET_PLANNING_RESERVE_MARGIN(R_PA,R_RESERVE)

         IF(ALLOCATED(PA_PLANNING_RESERVE_MARGIN)) THEN
            R_RESERVE = PA_PLANNING_RESERVE_MARGIN(R_PA)
         ELSE
            R_RESERVE = -999.0
         ENDIF
      RETURN


      ENTRY GET_CAPACITY_MARKET_MARGIN(R_PA,R_RESERVE)
         ! TODO: Something in program state
         ! should say whether data is grabbed from the array or not,
         ! NOT allocation status.
         IF(ALLOCATED(CM_PLANNING_RESERVE_MARGIN)) THEN
            ub=ubound(CM_PLanning_RESERVE_MARGIN,1)
            if(r_pa>ub) then
                er_message="cap_objt:0064 - exceeding bounds " // &
                  "on cm_planning_reserve_margin. " // &
                  "ubound=" // trim(itos(ub)) // ", " // &
                  "index=" // trim(itos(int(r_pa)))
               
                call end_program(er_message)
                
            end if
            
            R_RESERVE =  CM_PLANNING_RESERVE_MARGIN(R_PA)
         ELSE
            R_RESERVE = -999.0
         ENDIF
      RETURN


      ENTRY CALC_CM_RESERVE_MARGINS(R_YEAR)

         UPPER_TRANS_GROUP = GET_NUMBER_OF_ACTIVE_GROUPS()
         PG = MAX(GET_NUMBER_OF_PLANNING_GROUPS(),INT(1,2))
         CG = MAX(GET_NUMBER_OF_CAPACITY_MARKETS(),INT(1,2))
         PA_PLANNING_CAPACITY = 0.
         CM_PLANNING_CAPACITY = 0.
         CM_PEAK_AFTER_INTERRUPTIBLE = 0.
         PA_PEAK_AFTER_INTERRUPTIBLE = 0.
         IF(.NOT. ALLOCATED(TG_2_PLANNING_AREA)) RETURN
         DO TG = 1, UPPER_TRANS_GROUP
         call write_trace_int2(file_trace_amoc, "18. TG", TG)
            PLANNING_RM_BEFORE_ADDITIONS = ZERO
!
            TG_2_PLANNING_AREA(TG) = GET_PA_FROM_TG(TG)
            TG_2_CAPACITY_MARKET(TG) = GET_CM_FROM_TG(TG)
            PA = TG_2_PLANNING_AREA(TG)
            CM = TG_2_CAPACITY_MARKET(TG)
            CM_INDEX = GET_CM_INDEX_FROM_TG(CM)
!
            EXISTING_PLANNING_CAPACITY(0) = &
                        GET_CL_TG_CAP( &
                                 INT(0,2),TG,R_YEAR,INT(1,2)) + &
                        GET_EL_TG_CAP(TG,R_YEAR,INT(1,2)) + &
                          DERIV_CAPACITY_PLANNING_BY_TG(TG,R_YEAR)
!
            NEW_PLANNING_CAPACITY(0) = &
                           GET_CL_TG_CAP( &
                                 INT(0,2),TG,R_YEAR,INT(2,2)) + &
                         GET_EL_TG_CAP(TG,R_YEAR,INT(2,2))
            PA_PLANNING_CAPACITY(PA) = PA_PLANNING_CAPACITY(PA) + &
                        EXISTING_PLANNING_CAPACITY(0) + &
                                             NEW_PLANNING_CAPACITY(0)
            CM_PLANNING_CAPACITY(CM) = CM_PLANNING_CAPACITY(CM) + &
                        EXISTING_PLANNING_CAPACITY(0) + &
                                             NEW_PLANNING_CAPACITY(0)
            IF(MRX_SORTED_MIX) THEN ! I THINK THIS IS WHAT STEVE WANTS.
               CAPACITY_PLANNING_PEAK = &
                                     GET_GROUP_PEAK_ON_PEAK_MONTH(TG)
            ELSEIF(USE_REGIONAL_MINIMUM_RM_LOGIC) THEN
               CAPACITY_PLANNING_PEAK = &
                                     GET_PEAK_ON_PA_PEAK_MONTH(TG,PA)
            ELSE
               CAPACITY_PLANNING_PEAK = &
                                     GET_GROUP_PEAK_ON_PEAK_MONTH(TG)
            ENDIF
!
            INTERRUPTIBLE_LOAD = GET_ANNUAL_INTER_CAPACITY(TG)
            PA_PEAK_AFTER_INTERRUPTIBLE(PA) = &
                  PA_PEAK_AFTER_INTERRUPTIBLE(PA) + &
                     MAX(0., &
                         CAPACITY_PLANNING_PEAK - INTERRUPTIBLE_LOAD)
            PA_PEAK_MW(PA) = PA_PEAK_AFTER_INTERRUPTIBLE(PA)
            CM_PEAK_AFTER_INTERRUPTIBLE(CM) = &
                  CM_PEAK_AFTER_INTERRUPTIBLE(CM) + &
                     MAX(0., &
                         CAPACITY_PLANNING_PEAK - INTERRUPTIBLE_LOAD)
            CM_PEAK_MW(CM) = CM_PEAK_AFTER_INTERRUPTIBLE(CM)
!
! 090707. MIN AND MAX RM BY TG
!
            TG_PLANNING_CAPACITY(TG) = &
                        EXISTING_PLANNING_CAPACITY(0) + &
                                             NEW_PLANNING_CAPACITY(0)
            PEAK_AFTER_INTERRUPTIBLE_TG(TG) = &
                     MAX(0., &
                         CAPACITY_PLANNING_PEAK - INTERRUPTIBLE_LOAD)
            IF(PEAK_AFTER_INTERRUPTIBLE_TG(TG) > NEAR_ZERO)  THEN
               TG_PLANNING_RESERVE_MARGIN(TG) = &
                     1. + (TG_PLANNING_CAPACITY(TG) - &
                               PEAK_AFTER_INTERRUPTIBLE_TG(TG)) / &
                                      PEAK_AFTER_INTERRUPTIBLE_TG(TG)
               MINIMUM_TG_MRX_CAP(TG) = &
                        MAX(0., &
                           (GET_MIN_CAP_TESTING_RATIO(TG)* &
                                            SCENARIO_RESERVE_MARGIN - &
                               TG_PLANNING_RESERVE_MARGIN(TG))* &
                                     PEAK_AFTER_INTERRUPTIBLE_TG(TG))
!
               MAXIMUM_TG_MRX_CAP(TG) = &
                        MIN(999999., &
                             (GET_MAX_CAP_TESTING_RATIO(TG)* &
                                            SCENARIO_RESERVE_MARGIN - &
                                  TG_PLANNING_RESERVE_MARGIN(TG)) * &
                                     PEAK_AFTER_INTERRUPTIBLE_TG(TG))
               TG_PEAK_MW(TG) = PEAK_AFTER_INTERRUPTIBLE_TG(TG)
            ENDIF
         ENDDO ! TRANSACTION GROUPS
!
         DO PA = 1, PG
            PA_PEAK_AFTER_INTERRUPTIBLE(0) = &
               PA_PEAK_AFTER_INTERRUPTIBLE(0) + &
                  PA_PEAK_AFTER_INTERRUPTIBLE(PA)
            PA_PLANNING_CAPACITY(0) = &
               PA_PLANNING_CAPACITY(0) + &
                  PA_PLANNING_CAPACITY(PA)
!
            IF(PA_PEAK_AFTER_INTERRUPTIBLE(PA) > NEAR_ZERO)  THEN
               PA_PLANNING_RESERVE_MARGIN(PA) = &
                     1. + (PA_PLANNING_CAPACITY(PA) - &
                              PA_PEAK_AFTER_INTERRUPTIBLE(PA)) / &
                                   PA_PEAK_AFTER_INTERRUPTIBLE(PA)
               MINIMUM_ANNUAL_MRX_CAP(PA) = MAX(0., &
                     (CURRENT_TARGET_RATIO_PA(PA) - &
                        PA_PLANNING_RESERVE_MARGIN(PA))* &
                                     PA_PEAK_AFTER_INTERRUPTIBLE(PA))
!
               IF(USE_MAXIMUM_RM_LOGIC) THEN
                  MAXIMUM_ANNUAL_MRX_CAP(PA) = &
                    MIN(MAXIMUM_ANNUAL_MRX_CAP(PA), &
                        MAX(0., &
                             MAXIMUM_RM_PA(PA) - &
                                   PA_PLANNING_RESERVE_MARGIN(PA)) * &
                                    PA_PEAK_AFTER_INTERRUPTIBLE(PA))
               ENDIF
            ELSE
               PA_PLANNING_RESERVE_MARGIN(PA) = ZERO
            ENDIF
!
! 022210. TAKEN OUT: PA TO CM
            
               if(R_YEAR==2 .and. pa==1) then
                    R_YEAR=R_YEAR ! Debugstop
               end if
            PA_ICAP_REVENUE_MULT(PA) = GET_ICAP_REVENUE_MULT( &
                      R_YEAR,pa,PA_PLANNING_RESERVE_MARGIN(PA))
         ENDDO
         IF(PA_PEAK_AFTER_INTERRUPTIBLE(0) > NEAR_ZERO)  THEN
            PA_PLANNING_RESERVE_MARGIN(0) = &
                        1. + (PA_PLANNING_CAPACITY(0) - &
                                 PA_PEAK_AFTER_INTERRUPTIBLE(0)) / &
                                      PA_PEAK_AFTER_INTERRUPTIBLE(0)
         ENDIF
         DO CM = 1, CG
!
! 082412.
            
            ICAP_SWITCH_INDEX(CM) = &
                 GET_ICAP_SWITCH_INDEX(CM, &
                                       R_YEAR, &
                                       TEMP_R4)
            ICAP_CONE_PRICE(CM) = TEMP_R4
!
            IF(CM_PEAK_AFTER_INTERRUPTIBLE(CM) > NEAR_ZERO)  THEN
               CM_PLANNING_RESERVE_MARGIN(CM) = &
                     1. + (CM_PLANNING_CAPACITY(CM) - &
                              CM_PEAK_AFTER_INTERRUPTIBLE(CM)) / &
                                   CM_PEAK_AFTER_INTERRUPTIBLE(CM)
            ELSE
               CM_PLANNING_RESERVE_MARGIN(CM) = ZERO
            ENDIF
!
! 022210. PA TO CM
            
               if(R_YEAR==2 .and. CM==1) then
                    R_YEAR=R_YEAR ! Debugstop
               end if
            CM_ICAP_REVENUE_MULT(CM) = GET_ICAP_REVENUE_MULT( &
                         R_YEAR,CM,CM_PLANNING_RESERVE_MARGIN(CM))
         ENDDO
      RETURN
      END subroutine screening_object


      SUBROUTINE CALCULATE_NOX_SOX_VOM( &
                              R_PROD_POINTER, &
                              R_YEAR, &
                              R_ISEAS, &
                              RTEMP, &
                              RTEMP_2, &
                              RTEMP_3, &
                              RTEMP_4, &
                              RTEMP_5,RTEMP_6,RTEMP_7, &
                              RTEMP_8,RTEMP_9,RTEMP_10, &
                              NOX_CONTROL_MULT, &
                              SOX_CONTROL_MULT, &
                              CO2_CONTROL_MULT, &
                              HG_CONTROL_MULT, &
                              OTHER3_CONTROL_MULT, &
                              R_TRANS_GROUP)
!
         use capacity_arrays
         use params
         use esrn_decs
         use sizecom
         use globecom
         use mod_base_year
         use cap_trans
         use end_routine
         implicit none

!
      CHARACTER (len=1) ::   NOX_SEASON_REGION
      CHARACTER (len=1) ::   GET_TRANS_GROUP_NOX_SEASON
      LOGICAL (kind=1) ::  NOX_CONTROL_FOR_UNIT
      LOGICAL (kind=1) ::  CALCULATE_NOX
      LOGICAL (kind=1) ::  SOX_CONTROL_FOR_UNIT
      LOGICAL (kind=1) ::  CO2_CONTROL_FOR_UNIT
      LOGICAL (kind=1) ::  HG_CONTROL_FOR_UNIT
      LOGICAL (kind=1) ::  OTHER3_CONTROL_FOR_UNIT
      LOGICAL (kind=1) ::  PHASE_I_UNIT_OR_NOT_USA
      LOGICAL (kind=1) ::  GET_SCREEN_NOX_CONTROL_FOR_UNIT
      LOGICAL (kind=1) ::  GET_SCREEN_SOX_CONTROL_FOR_UNIT
      LOGICAL (kind=1) ::  GET_SCREEN_CO2_CONTROL_FOR_UNIT
      LOGICAL (kind=1) ::  GET_SCREEN_HG_CONTROL_FOR_UNIT
      LOGICAL (kind=1) ::  GET_SCREEN_OTHER3_CTRL_FOR_UNIT
      LOGICAL (kind=1) ::  SEASON_IS_NOX_SEASON
      LOGICAL (kind=1) ::  NOX_ACTIVE_IN_TG
      LOGICAL (kind=1) ::  NOX_YEAR_ACTIVE_IN_TG
      INTEGER (kind=2) ::  R_YEAR 
      INTEGER (kind=2) ::  R_PROD_POINTER
      INTEGER (kind=2) ::  I
      INTEGER (kind=2) ::  R_ISEAS
      INTEGER (kind=2) ::  TG
      INTEGER (kind=2) ::  MAX_TRANS_GROUP_NUMBER
      INTEGER (kind=2) ::  GET_MAX_TRANS_GROUP_NUMBER
      INTEGER (kind=2) ::  BASE_DATE
      INTEGER (kind=2) ::  CURRENT_YEAR
      INTEGER (kind=2) ::  NOX_YEAR
      INTEGER (kind=2) ::  END_NOX_YEAR
      INTEGER (kind=2) ::  GET_TRANS_GROUP_POSITION
      INTEGER (kind=2) ::  GET_TRANS_GROUP_NOX_YEAR
      INTEGER (kind=2) ::  GET_TRANS_GROUP_END_NOX_YEAR
      INTEGER (kind=2) ::  R_TRANS_GROUP
      INTEGER (kind=2) ::   ISEAS
      REAL (kind=4) ::  RTEMP
      REAL (kind=4) ::  RTEMP_2
      REAL (kind=4) ::  RTEMP_3
      REAL (kind=4) ::  RTEMP_4
      REAL (kind=4) ::  RTEMP_5
      REAL (kind=4) ::  RTEMP_6
      REAL (kind=4) ::  RTEMP_7
      REAL (kind=4) ::  RTEMP_8
      REAL (kind=4) ::  RTEMP_9
      REAL (kind=4) ::  RTEMP_10
      REAL (kind=4) ::  ESCALATED_MONTHLY_VALUE
      REAL (kind=4) ::  GET_SCREEN_NOX_VOM
      REAL (kind=4) ::  GET_SCREEN_NOX_FOM
      REAL (kind=4) ::  GET_SCREEN_SOX_VOM
      REAL (kind=4) ::  GET_SCREEN_SOX_FOM
      REAL (kind=4) ::  GET_SCREEN_CO2_VOM
      REAL (kind=4) ::  GET_SCREEN_CO2_FOM
      REAL (kind=4) ::  GET_SCREEN_HG_VOM
      REAL (kind=4) ::  GET_SCREEN_HG_FOM
      REAL (kind=4) ::  GET_SCREEN_OTHER3_VOM
      REAL (kind=4) ::  GET_SCREEN_OTHER3_FOM
      REAL (kind=4) ::  NOX_CONTROL_MULT
      REAL (kind=4) ::  SOX_CONTROL_MULT
      REAL (kind=4) ::  CO2_CONTROL_MULT
      REAL (kind=4) ::  HG_CONTROL_MULT
      REAL (kind=4) ::  OTHER3_CONTROL_MULT
      

!
! END DATA DECLARATIONS
!
            I = R_PROD_POINTER
            ISEAS = R_ISEAS
            CURRENT_YEAR = R_YEAR+get_BASE_YEAR()
            BASE_DATE = (CURRENT_YEAR - 1900) * 100
!
            NOX_CONTROL_FOR_UNIT =  GET_SCREEN_NOX_CONTROL_FOR_UNIT( &
                                             I, &
                                             ISEAS+BASE_DATE, &
                                             NOX_CONTROL_MULT)
            SOX_CONTROL_FOR_UNIT =  GET_SCREEN_SOX_CONTROL_FOR_UNIT( &
                                             I, &
                                             ISEAS+BASE_DATE, &
                                             SOX_CONTROL_MULT)
            CO2_CONTROL_FOR_UNIT =  GET_SCREEN_CO2_CONTROL_FOR_UNIT( &
                                             I, &
                                             ISEAS+BASE_DATE, &
                                             CO2_CONTROL_MULT)
            HG_CONTROL_FOR_UNIT =  GET_SCREEN_HG_CONTROL_FOR_UNIT( &
                                             I, &
                                             ISEAS+BASE_DATE, &
                                             HG_CONTROL_MULT)
            OTHER3_CONTROL_FOR_UNIT = &
                                    GET_SCREEN_OTHER3_CTRL_FOR_UNIT( &
                                             I, &
                                             ISEAS+BASE_DATE, &
                                             OTHER3_CONTROL_MULT)
!
! TG IS POSITION
!
            TG = R_TRANS_GROUP

            TG = GET_TRANS_GROUP_POSITION(TG)

            NOX_SEASON_REGION = GET_TRANS_GROUP_NOX_SEASON(TG) 
            ! TRIGGERED OFF OF TG #, NOT INDEX
            NOX_YEAR = GET_TRANS_GROUP_NOX_YEAR(TG)
            END_NOX_YEAR = GET_TRANS_GROUP_END_NOX_YEAR(TG)
            IF( (NOX_SEASON_REGION == 'T' .AND. &
                                 CURRENT_YEAR >= NOX_YEAR.AND. &
                                 CURRENT_YEAR <= END_NOX_YEAR) &
                                  .OR. NOX_SEASON_REGION == 'M') THEN
               NOX_ACTIVE_IN_TG = .TRUE.
               NOX_YEAR_ACTIVE_IN_TG = .TRUE.
            ELSE
               NOX_ACTIVE_IN_TG = .FALSE.
!
               IF(NOX_SEASON_REGION == 'T' .AND. &
                                       CURRENT_YEAR < NOX_YEAR ) THEN
                  IF(CURRENT_YEAR > END_NOX_YEAR) THEN
                     NOX_YEAR_ACTIVE_IN_TG = .TRUE.
                  ELSE
                     NOX_YEAR_ACTIVE_IN_TG = .FALSE.
                  ENDIF
               ELSE
                  NOX_YEAR_ACTIVE_IN_TG = .TRUE.
               ENDIF
            ENDIF

            RTEMP = 0.0
            RTEMP_2 = 0.0

            IF(ISEAS > 4 .AND. ISEAS < 10 ) THEN
               SEASON_IS_NOX_SEASON = .TRUE.
            ELSE
               SEASON_IS_NOX_SEASON = .FALSE.
            ENDIF
            TG = R_TRANS_GROUP

            MAX_TRANS_GROUP_NUMBER = GET_MAX_TRANS_GROUP_NUMBER()

            IF(TG > 0 .AND. TG <= MAX_TRANS_GROUP_NUMBER) THEN
               IF(NOX_ACTIVE_IN_TG) THEN
                  CALCULATE_NOX = SEASON_IS_NOX_SEASON
               ELSEIF(.NOT. NOX_YEAR_ACTIVE_IN_TG) THEN
                  CALCULATE_NOX = .FALSE.
               ELSE
                  CALCULATE_NOX = .TRUE.
               ENDIF
            ELSE
               WRITE(4,*) 'CANNOT FIND TRANSACTION GROUP FOR'
               WRITE(4,*) 'MRX UNIT',I,'AND TRANS GROUP',TG
            ENDIF

            IF(NOX_CONTROL_FOR_UNIT .AND. CALCULATE_NOX) THEN

               RTEMP = GET_SCREEN_NOX_VOM(I)

               IF(RTEMP < -0.1) THEN
                  RTEMP=ABS(RTEMP)
                  RTEMP = &
                     ESCALATED_MONTHLY_VALUE(RTEMP, &
                                                 INT(RTEMP,2), &
                                                 globecom_year,ISEAS,INT(1,2))
               ENDIF
!
               RTEMP_2 = GET_SCREEN_NOX_FOM(I)
               IF(RTEMP_2 < -0.1) THEN
                  RTEMP_2 = ABS(RTEMP_2)
                  RTEMP_2 = &
                     ESCALATED_MONTHLY_VALUE(RTEMP_2, &
                                                 INT(RTEMP_2,2), &
                                                 globecom_YEAR,ISEAS,INT(1,2))
               ENDIF
            ENDIF
            RTEMP_3 = 0.0
            RTEMP_4 = 0.0
            IF(SOX_CONTROL_FOR_UNIT) THEN
               RTEMP_3 = GET_SCREEN_SOX_VOM(I)

               IF(RTEMP_3 < -0.1) THEN
                  RTEMP_3=ABS(RTEMP_3)
                  RTEMP_3 = &
                     ESCALATED_MONTHLY_VALUE(RTEMP_3, &
                                                 INT(RTEMP_3,2), &
                                                 globecom_YEAR,ISEAS,INT(1,2))
               ENDIF
!
               RTEMP_4 = GET_SCREEN_SOX_FOM(I)
               IF(RTEMP_4 < -0.1) THEN
                  RTEMP_4 = ABS(RTEMP_4)
                  RTEMP_4 = &
                     ESCALATED_MONTHLY_VALUE(RTEMP_4, &
                                                 INT(RTEMP_4,2), &
                                                 globecom_YEAR,ISEAS,INT(1,2))
               ENDIF
            ENDIF
!
            RTEMP_5 = 0.0
            RTEMP_6 = 0.0

            IF(CO2_CONTROL_FOR_UNIT) THEN
               RTEMP_5 = GET_SCREEN_CO2_VOM(I)

               IF(RTEMP_5 < -0.1) THEN
                  RTEMP_5=ABS(RTEMP_5)
                  RTEMP_5 = &
                     ESCALATED_MONTHLY_VALUE(RTEMP_5, &
                                                 INT(RTEMP_5,2), &
                                                 globecom_YEAR,ISEAS,INT(1,2))
               ENDIF
!
               RTEMP_6 = GET_SCREEN_CO2_FOM(I)
               IF(RTEMP_6 < -0.1) THEN
                  RTEMP_6 = ABS(RTEMP_6)
                  RTEMP_6 = &
                     ESCALATED_MONTHLY_VALUE(RTEMP_6, &
                                                 INT(RTEMP_6,2), &
                                                 globecom_YEAR,ISEAS,INT(1,2))
               ENDIF
            ENDIF

            RTEMP_7 = 0.0
            RTEMP_8 = 0.0
            IF(HG_CONTROL_FOR_UNIT) THEN
               RTEMP_7 = GET_SCREEN_HG_VOM(I)

               IF(RTEMP_7 < -0.1) THEN
                  RTEMP_7=ABS(RTEMP_7)
                  RTEMP_7 = &
                     ESCALATED_MONTHLY_VALUE(RTEMP_7, &
                                                 INT(RTEMP_7,2), &
                                                 globecom_YEAR,ISEAS,INT(1,2))
               ENDIF
!
               RTEMP_8 = GET_SCREEN_HG_FOM(I)
!
               IF(RTEMP_8 < -0.1) THEN
                  RTEMP_8 = ABS(RTEMP_8)
                  RTEMP_8 = &
                     ESCALATED_MONTHLY_VALUE(RTEMP_8, &
                                                 INT(RTEMP_8,2), &
                                                 globecom_YEAR,ISEAS,INT(1,2))
               ENDIF
            ENDIF

            RTEMP_9 = 0.0
            RTEMP_10 = 0.0

            IF(OTHER3_CONTROL_FOR_UNIT) THEN
               RTEMP_9 = GET_SCREEN_OTHER3_VOM(I)

               IF(RTEMP_9 < -0.1) THEN
                  RTEMP_9=ABS(RTEMP_9)
                  RTEMP_9 = &
                     ESCALATED_MONTHLY_VALUE(RTEMP_9, &
                                                 INT(RTEMP_9,2), &
                                                 globecom_year,ISEAS,INT(1,2))
               ENDIF

               RTEMP_10 = GET_SCREEN_OTHER3_FOM(I)
               IF(RTEMP_10 < -0.1) THEN
                  RTEMP_10 = ABS(RTEMP_10)
                  RTEMP_10 = &
                     ESCALATED_MONTHLY_VALUE(RTEMP_10, &
                                                 INT(RTEMP_10,2), &
                                                 globecom_YEAR,ISEAS,INT(1,2))
               ENDIF
            ENDIF
      END SUBROUTINE


      SUBROUTINE ALTER_MX_PRICE_CURVE(R_HIGHEST_INTERVAL, &
                                      R_MARKET_DURATION, &
                                      R_MARKET_CUM_REVENUE, &
                                      R_MARKET_PRICE, &
                                      R_SCREEN_CAPACITY, &
                                      R_ANNUAL_ENERGY, &
                                      R_STRIKE_PRICE, &
                                      R_HIGHEST_J, &
                                      R_HIGHEST_TOP_CAP_PERCENT, &
                                      R_MARKET_REVENUE, &
                                      R_TRANS_GROUP_4_PRICING, &
                                      R_RESOURCE_ADDITION)

      USE SIZECOM
      use globecom
      use end_routine
      implicit none

      LOGICAL (kind=1) ::   R_RESOURCE_ADDITION
      INTEGER (kind=2) ::  R_HIGHEST_J
      INTEGER (kind=2) ::  R_HIGHEST_INTERVAL
      INTEGER (kind=2) ::  I
      INTEGER (kind=2) ::  J
      INTEGER (kind=2) ::  K
      INTEGER (kind=2) ::  R_TRANS_GROUP_4_PRICING
      REAL (kind=4) ::  R_MARKET_DURATION(0:R_HIGHEST_INTERVAL)
      REAL (kind=4) ::  R_MARKET_CUM_REVENUE(0:R_HIGHEST_INTERVAL)
      REAL (kind=4) ::  R_MARKET_PRICE(0:R_HIGHEST_INTERVAL)
      REAL (kind=4) ::  R_SCREEN_CAPACITY
      REAL (kind=4) ::  R_ANNUAL_ENERGY
      REAL (kind=4) ::  R_STRIKE_PRICE
      REAL (kind=4) ::  R_HIGHEST_TOP_CAP_PERCENT
      REAL (kind=4) ::  R_MARKET_REVENUE
      REAL (kind=4) ::  RESOURCE_HOURS
      REAL (kind=4) ::  RESOURCE_ELASTICITY
      REAL (kind=4) ::  REGIONAL_PEAK
      REAL (kind=4) ::  MARKET_FACTOR
      REAL (kind=4) ::  PRICE_DIFF
      REAL (kind=4) ::  PRICE_REDUCTION
      REAL (kind=4) ::  PEAK
      REAL (kind=4) ::  UPDATE_NET_PLANNING_PEAK
      REAL (kind=4) ::  MRX_VOLATILITY_MULT
      REAL (kind=4) ::  GET_MRX_VOLATILITY_MULT
      REAL (kind=4) ::  GET_GROUP_PEAK_ON_PEAK_MONTH
!
! MODIFY THE PRICE CURVE STARTING AT HIGHEST_J UNTIL THE END OF THE 
! CURVE
! MAKE AN ASSUMPTION HOW MANY HOURS THE RESOURCE IS RUNNING BASED ON 
! ENERGY.
! THEN MAKE AN ASSUMPTION OF THE SIZE OF THE SUPPLY AND ELASTICITY OF 
! DEMAND IN THAT HOUR.
! FINALLY, MAKE AN ASSUMPTION OF HOW PRICES WILL BE SHIFTED AS A RESULT 
! OF THE RESOURCE.
!
! ASSUME IT ALWAYS OPERATES AT FULL LOAD
!
! SHOULD BE A FUNCITON OF THE SIZE OF THE RESOURCE IN THE LOCAL AND 
! REGIONAL MARKETPLACE.


         MRX_VOLATILITY_MULT = 1.0 ! MOVED INTO MX_USER_PRICE_CURVE

         IF(R_TRANS_GROUP_4_PRICING == 0) THEN
            if(globecom_YEAR==0) then
               call end_program("cap_objt:0054 - year must be nonzero.")
            end if
            PEAK =  UPDATE_NET_PLANNING_PEAK(globecom_YEAR)

         ELSE
            PEAK = GET_GROUP_PEAK_ON_PEAK_MONTH(R_TRANS_GROUP_4_PRICING)

         ENDIF

         RESOURCE_ELASTICITY = &
                            MIN(1.,12.*R_SCREEN_CAPACITY / MAX(1.,PEAK))
!
         DO J = R_HIGHEST_INTERVAL, 1, -1

            IF(J > R_HIGHEST_J) THEN
               IF(R_RESOURCE_ADDITION) THEN
                  PRICE_DIFF = R_MARKET_PRICE(J) - R_STRIKE_PRICE
               ELSE ! RETIREMENT
                  PRICE_DIFF = R_STRIKE_PRICE - R_MARKET_PRICE(J)
               ENDIF
               PRICE_REDUCTION = PRICE_DIFF * &
                                     RESOURCE_ELASTICITY * &
                                     MRX_VOLATILITY_MULT

               R_MARKET_PRICE(J) = R_MARKET_PRICE(J) - PRICE_REDUCTION
               IF(R_MARKET_PRICE(J) < 0.) THEN
                  WRITE(4,*) "NON-ECONOMIC RESOURCES ADDED IN MRX."
                  WRITE(4,*) "PRICE SHIFT BEYOND 100%."
               ENDIF
            ENDIF

            IF(J < R_HIGHEST_INTERVAL) THEN
! DURATION IS DECREASING
               IF(R_MARKET_PRICE(J) > 100000.) THEN
                  WRITE(4,*) "PRICES TOO HIGH IN MRX",R_MARKET_PRICE(J)
                  WRITE(4,*) "TRANS GROUP ",R_TRANS_GROUP_4_PRICING
                  WRITE(4,*) "POINT ON CURVE ",J
                  write(4,*) "YEAR",globecom_YEAR
                  write(4,*) "EndPoint",gc_end_point
                  R_MARKET_PRICE(J) = 100000.
               ELSEIF(R_MARKET_PRICE(J) < 0.) THEN
                  WRITE(4,*) "PRICES TOO LOW IN MRX",R_MARKET_PRICE(J)
                  R_MARKET_PRICE(J) = 0.
               ENDIF
               R_MARKET_CUM_REVENUE(J) = R_MARKET_CUM_REVENUE(J+1) + &
                                R_MARKET_PRICE(J) * &
                         (R_MARKET_DURATION(J) - R_MARKET_DURATION(J+1))
            ELSE
               R_MARKET_CUM_REVENUE(J) = 0.
            ENDIF
!
         ENDDO
!
      RETURN
      END


      SUBROUTINE MRX_GREEN_OBJECT
      use end_routine
      use cap_shared
      use capacity_options_alloc_vars
      use capacity_options_fixed_vars
      USE GRX_PLANNING_ROUTINES
      USE CLA_OBJT_ARRAYS
      USE CO2_CAP_N_TRADE
      use mod_fuel_types
      use esrn_decs
      use grxmodules
      use mod_base_year
      use cap_trans
      use tf_decs
      use tf_objt_interfaces
      use mwunih
      USE SIZECOM
      use prod2com
      use prodcom
      use flusher
      use debugtrace
      use sic_trace

      implicit none
      integer :: file_trace_mga=0 ! annual_mx_green trace
      logical (kind=1) :: yes_rt
      integer :: file_trace_mg=0
      
      REAL (KIND=4) :: GRX_CO2_MARKET_TONS,GRX_CO2_MARKET_PRICE
      LOGICAL (KIND=1) ::  CO2_RETIREMENTS_LOGIC
      LOGICAL (KIND=1) ::  CO2_RETOFIT_LOGIC
      LOGICAL (KIND=1) ::  OPTIONS_ARE_AVAILABLE
      LOGICAL (KIND=1) ::  ADJUST_GRX_CO2_RETRO_PLAN_CAP
      INTEGER (kind=2) ::   MAX_SCREENING_INTERVALS,R_YEAR
      PARAMETER(MAX_SCREENING_INTERVALS = 79)
      LOGICAL (kind=1) ::  RESOURCE_AVAILABLE
      LOGICAL (kind=1) ::  DEPENDENT_UNIT_AVAILABLE
      LOGICAL (kind=1) ::  CALC_SCREEN_SLOPE_INTERCEPT
      LOGICAL (kind=1) ::  SCREENING_NOT_OPEN=.TRUE.
      LOGICAL (kind=1) ::  CAPTURED_OPTIONS_NAMES
      LOGICAL (kind=1) ::  GET_OPTION_NAMES
      LOGICAL (kind=1) ::  RUN_MULTIAREA
      LOGICAL (kind=1) ::  RUN_PRICE_MODE
      LOGICAL (kind=1) ::  RUN_AREA_PRICE_MODE
      LOGICAL (kind=1) ::  YES_MULTI_AREA_PRICE
      LOGICAL (kind=1) ::  YES_STRICT_MARKET_PRICE
      LOGICAL (kind=1) ::  YES_RUN_MULTIAREA_TRANSACT
      LOGICAL (kind=1) ::  YES_RUN_TRANSACT
      LOGICAL (kind=1) ::  BUILD_TO_MARKET=.TRUE.
      LOGICAL (kind=1) ::  ANNUAL_MARKET_OPTION_SWITCH=.FALSE.
      LOGICAL (kind=1) ::  ANNUAL_RESOURCES_AVAILABLE
      LOGICAL (kind=1) ::  LOCAL_RESOURCES_AVAILABLE
      LOGICAL (kind=1) ::  TEST_START_STOP_YEARS
      LOGICAL (kind=1) ::  FUEL_PRICE_DATA_AVAILABLE=.FALSE.
      LOGICAL (kind=1) ::  PLANNING_PER_MWH
      LOGICAL (kind=1) ::  YES_PLAN_TO_MWH_OR_KWYR
      LOGICAL (kind=1) ::  REGIONAL_PARAMS_EXIST=.FALSE.
      LOGICAL (kind=1) ::  YES_REGIONAL_PARAMS_EXIST
      LOGICAL (kind=1) ::  MRX_SORTED_MIX
      LOGICAL (kind=1) ::  USE_EMIS_IN_MRX
      LOGICAL (kind=1) ::  YES_USE_EMIS_IN_MRX
      LOGICAL (kind=1) ::  RETIRE_CO2_THERMAL_UNIT
      LOGICAL (kind=1) ::  RETROFIT_CO2_THERMAL_UNIT
      LOGICAL (kind=1) ::  TEMP_L1
      LOGICAL (kind=1) ::  GET_ESC_NRG_PRICE
      LOGICAL (kind=1) ::  YES_GAS_MODEL_DATA_AVAILABLE
      LOGICAL (kind=1) ::  GAS_MODEL_DATA_AVAILABLE
      LOGICAL (kind=1) ::  RETROFIT_PROJECT_ACTIVE
      INTEGER (kind=2) ::  RETURN_TOTAL_ALL_OPTIONS
      INTEGER (kind=2) ::  BEST_RESOURCE(MAX_SCREENING_INTERVALS)
      INTEGER (kind=2) ::  I
      INTEGER (kind=2) ::  J
      INTEGER (kind=2) ::  K
      INTEGER (kind=2) ::  L
      INTEGER (kind=2) ::  TG
      INTEGER (kind=2) ::  PA
      INTEGER (kind=2) ::  INITIALIZE_SCREEN_DATA
      INTEGER (kind=2) ::  THIS_YEAR
      INTEGER (kind=2) ::  TRANS
      INTEGER (kind=2) ::  GSP
      INTEGER (kind=2) ::  GET_SCRN_UNIT_GAS_REGION_INDEX
      INTEGER (kind=2) ::  MAX_GAS_REGION_NO=500
      INTEGER (kind=2) ::  THIS_YEAR_PLUS_LEAD_TIME(:)
      INTEGER (kind=2) ::  UPDATE_CL_SCREEN_COSTS
      INTEGER (kind=2) ::  VOID_INT2
      INTEGER (kind=2) ::  SCREENING_NO=0
      INTEGER (kind=2) ::  SCREENING_HEADER
      INTEGER (kind=2) ::  HIGHEST_OPTION
      INTEGER (kind=2) ::  NET_MARGIN_INTERVAL(:)
      INTEGER (kind=2) ::  STATE_PROVINCE(:)
      INTEGER (kind=2) ::  GET_TRANS_STATE_INDEX
      INTEGER (kind=2) ::  GET_SCREEN_STATE_INDEX
      INTEGER (kind=2) ::  LOCAL_ANNUAL_UNITS(:)
      INTEGER (kind=2) ::  RESOURCE_COUNTER(:)
      INTEGER (kind=2) ::  LOCAL_LEAD_TIME(:)
      INTEGER (kind=2) ::  OPTION_POSITION(:)
      INTEGER (kind=2) ::  LAST_RESOURCE_ADDED(:)
      INTEGER (kind=2) ::  FILE_SOURCE_INDEX(:)
      INTEGER (kind=2) ::  RESOURCE_TYPE_INDEX(:)
      INTEGER (kind=2) ::  LOCAL_RESOURCE_INDEX(:)
      INTEGER (kind=2) ::  GET_FILE_SOURCE_INDEX
      INTEGER (kind=2) ::  GET_RESOURCE_TYPE_INDEX
      INTEGER (kind=2) ::  GET_TRANS_GROUP_FOR_TRANS
      INTEGER (kind=2) ::  GET_TRANS_FOR_GRX_ID
      INTEGER (kind=2) ::  THERM_EXP=1
      INTEGER (kind=2) ::  HYDRO_EXP=2
      INTEGER (kind=2) ::  DERIV_EXP=3
      INTEGER (kind=2) ::  RPS_EXP=4
      INTEGER (kind=2) ::  RETIRE_RES=5
      INTEGER (kind=2) ::  RETRO_RES=6
      INTEGER (kind=2) ::  LAST_SORTED_POSITION=0
      INTEGER (kind=2) ::  LSP
      INTEGER (kind=2) ::  CN
      INTEGER (kind=2) ::  HIGHEST_INTERVAL
      INTEGER (kind=2) ::  MAX_RESOURCES_PER_YEAR_GROUP
      INTEGER (kind=2) ::  MAX_RESOURCES_PER_GROUP
      integer (kind=2) :: co_prod_pointer
      INTEGER (kind=2) ::  GET_PRODUCTION_DATA_POINTER
      INTEGER (kind=2) ::  HIGHEST_J
      INTEGER (kind=2) ::  UPPER_TRANS_GROUP=0
      INTEGER (kind=2) ::  GET_TRANS_GROUP_INDEX
      INTEGER (kind=2) ::  GET_TRANS_GROUP_POSITION
      INTEGER (kind=2) ::  RETURN_S_TRANS_GROUP_ID
      INTEGER (kind=2) ::  TRANS_GROUP_4_PRICING
      INTEGER (kind=2) ::  ANNUAL_UNITS_FOR_OPTION
      INTEGER (kind=2) ::  CUMULATIVE_UNITS_FOR_OPTION
      INTEGER (kind=2) ::  LEAD_TIME_FOR_OPTION
      INTEGER (kind=2) ::  GET_MRX_ICAP_UNIT_LINK
      INTEGER (kind=2) ::  MRX_ICAP_UNIT_LINK(:)

      INTEGER (kind=2) ::  HGST_OPT_INDEX(MX_RES_P_YEAR)
      INTEGER (kind=2) ::  SORTED_OPTIONS(MX_RES_P_YEAR)
      INTEGER (kind=2) ::  HO
      INTEGER (kind=2) ::  LAST_HO
      INTEGER (kind=2) ::  RPS_USER_PATTERN
      INTEGER (kind=2) ::  GET_TRANS_RPS_USER_PATTERN
      INTEGER (kind=2) ::  RPS_OPTION(:)
      INTEGER (kind=2) ::  RPS_RESOURCE_COUNT
      REAL (kind=4) ::  HGST_NET_MARG_ADDED(MX_RES_P_YEAR)
      REAL (kind=4) ::  CALC_GRX_FIX_COST_PER_UNIT
      REAL (kind=4) ::  RPS_ENERGY(:)
      REAL (kind=4) ::  RPS_HOURS(:)
      REAL (kind=4) ::  RPS_MULT(:)
      REAL (kind=4) ::  GET_SUM_USER_DAY_DATA
      REAL (kind=4) ::  RETRO_FITTED_CAP
      INTEGER :: SCREENING_REC

      SAVE SCREENING_REC
      REAL (kind=4) ::  BEST_SLOPE(MAX_SCREENING_INTERVALS)
      REAL (kind=4) ::  BEST_INTERCEPT(MAX_SCREENING_INTERVALS)
      REAL (kind=4) ::  SCREEN_SLOPE(:)
      REAL (kind=4) ::  SCREEN_INTERCEPT(:)
      REAL (kind=4) ::  SCREEN_CAPACITY(:)
      REAL (kind=4) ::  EQUIVALENT_CAPACITY(:)
      REAL (kind=4) ::  ANNUAL_FIXED_COST(:)
      REAL (kind=4) ::  ANNUAL_VARIABLE_COST(:)
      REAL (kind=4) ::  NET_MARGIN(:)
      REAL (kind=4) ::  NET_MARGIN_PER_KWYR(:)
      REAL (kind=4) ::  MARKET_REVENUE(:)
      REAL (kind=4) ::  HIGHEST_ENERGY(:)
      REAL (kind=4) ::  MARKET_COST(:)
      REAL (kind=4) ::  INTERVAL_MULTIPLIER
      REAL (kind=4) ::  R_SLOPE
      REAL (kind=4) ::  R_INTERCEPT
      REAL (kind=4) ::  SCREEN_VALUE(MAX_SCREENING_INTERVALS)
      REAL (kind=4) ::  BEST_VALUE(MAX_SCREENING_INTERVALS)
      REAL (kind=4) ::  HIGHEST_NET_MARGIN
      REAL (kind=4) ::  HIGHEST_NET_MARGIN_PER_MWH
      REAL (kind=4) ::  HIGHEST_NET_MARGIN_PER_KWYR
      REAL (kind=4) ::  HIGHEST_CF
      REAL (kind=4) ::  HIGHEST_ICAP_REVENUE
      REAL (kind=4) ::  HIGHEST_ENERGY_REVENUE
      REAL (kind=4) ::  MINIMUM_NET_MARGIN
      REAL (kind=4) ::  GET_ADDITIONS_PROFIT_PER_MWH
      REAL (kind=4) ::  TEMP_NET_MARGIN
      REAL (kind=4) ::  TEMP_NET_MARGIN_PER_KWYR
      REAL (kind=4) ::  TOTAL_AVERAGE_COST
      REAL (kind=4) ::  TOP_CAP_PERCENT
      REAL (kind=4) ::  GET_SCREEN_CAPACITY
      REAL (kind=4) ::  MARKET_CUM_REVENUE(:,:)
      REAL (kind=4) ::  MARKET_DURATION(:,:)
      REAL (kind=4) ::  MARKET_PRICE(:,:)
      REAL (kind=4) ::  STRIKE_PRICE(:,:)
      REAL (kind=4) ::  ANNUAL_LEVELIZED_CAPITAL(:)
      REAL (kind=4) ::  ANNUAL_FUEL_COST(:)
      REAL (kind=4) ::  FUEL_COST_PER_MWH(:,:)
      REAL (kind=4) ::  EXPANSION_CO2(:)
      REAL (kind=4) ::  VAR_OM_PER_MWH(:)
      REAL (kind=4) ::  FIX_OM_COST(:)
      REAL (kind=4) ::  GET_MONTHLY_GAS_PRICE_BY_STATE
      REAL (kind=4) ::  RETURN_OTHER_VAR_COMPONENT
      REAL (kind=4) ::  RETURN_CL_SCREEN_FIXED_COST
      REAL (kind=4) ::  MARKET_HOURS
      REAL (kind=4) ::  MARKET_AVE_REVENUE
      REAL (kind=4) ::  CF
      REAL (kind=4) ::  ANNUAL_ENERGY(:)
      REAL (kind=4) ::  RETURN_SCREEN_CAP_COST
      REAL (kind=4) ::  RETURN_RISK_ADJ_S_CAP_COST
      REAL (kind=4) ::  RETURN_CL_SCREEN_FUEL_COST
      REAL (kind=4) ::  HEAT_RATE
      REAL (kind=4) ::  FUEL_COST_PER_MMBTU
      REAL (kind=4) ::  RETURN_CL_SCREEN_EMIS_COST
      REAL (kind=4) ::  HIGHEST_TOP_CAP_PERCENT
      REAL (kind=4) ::  FIRST_YEAR_CAPACITY
      REAL (kind=4) ::  ADD_THIS_UNIT
      REAL (kind=4) ::  PEAK
      REAL (kind=4) ::  UPDATE_NET_PLANNING_PEAK
      REAL (kind=4) ::  GET_VAR
      REAL (kind=4) ::  RETURN_CL_SCREEN_EFOR
      REAL (kind=4) ::  SCENARIO_CAP_COST_MULT
      REAL (kind=4) ::  GET_SCENARIO_EXPAND_CAP_COST
      REAL (kind=4) ::  SCENARIO_RESERVE_MARGIN
      REAL (kind=4) ::  GET_SCENARIO_RESERVE_MARGIN
      REAL (kind=4) ::  GET_ICAP_REVENUE_MULT
      REAL (kind=4) ::  ANNUAL_ICAP_REVENUE
      REAL (kind=4) ::  ANNUAL_ENERGY_REVENUE
      REAL (kind=4) ::  NOX_VOM
      REAL (kind=4) ::  NOX_FOM
      REAL (kind=4) ::  SOX_VOM
      REAL (kind=4) ::  SOX_FOM
      REAL (kind=4) ::  CO2_VOM
      REAL (kind=4) ::  CO2_FOM
      REAL (kind=4) ::  HG_VOM
      REAL (kind=4) ::  HG_FOM
      REAL (kind=4) ::  OTHER3_VOM
      REAL (kind=4) ::  OTHER3_FOM
      REAL (kind=4) ::  NOX_CONTROL_MULT
      REAL (kind=4) ::  SOX_CONTROL_MULT
      REAL (kind=4) ::  CO2_CONTROL_MULT
      REAL (kind=4) ::  HG_CONTROL_MULT
      REAL (kind=4) ::  OTHER3_CONTROL_MULT
      REAL (kind=4) ::  CO2_STRIKE_PRICE
      REAL (kind=4) ::  CO2_UNIT_MW_AFTER
      REAL (kind=4) ::  CO2_STRIKE_PRICE_AFTER
      REAL (kind=4) ::  CO2_MARKET_REVENUE
      CHARACTER (len=20) ::     OPTION_NAME(:)
      CHARACTER (len=35) ::  MULTI_AREA_NAME(:)
      CHARACTER (len=35) ::  GET_GROUP_NAME
      CHARACTER (len=22) ::     LOCAL_NAME
      ALLOCATABLE :: SCREEN_SLOPE,SCREEN_INTERCEPT,OPTION_NAME, &
                        OPTION_POSITION, &
                        LAST_RESOURCE_ADDED, &
                        FILE_SOURCE_INDEX, &
                        RESOURCE_TYPE_INDEX, &
                        LOCAL_RESOURCE_INDEX, &
                        MRX_ICAP_UNIT_LINK, &
                        SCREEN_CAPACITY, &
                        EQUIVALENT_CAPACITY, &
                        RPS_HOURS, &
                        RPS_OPTION, &
                        RPS_ENERGY, &
                        RPS_MULT, &
                        ANNUAL_FIXED_COST, &
                        ANNUAL_VARIABLE_COST, &
                        NET_MARGIN, &
                        NET_MARGIN_PER_KWYR, &
                        NET_MARGIN_INTERVAL, &
                        THIS_YEAR_PLUS_LEAD_TIME, &
                        MARKET_REVENUE, &
                        HIGHEST_ENERGY, &
                        MARKET_COST, &
                        MARKET_CUM_REVENUE, &
                        MARKET_DURATION, &
                        MARKET_PRICE, &
                        STRIKE_PRICE, &
                        STATE_PROVINCE, &
                        LOCAL_ANNUAL_UNITS, &
                        RESOURCE_COUNTER, &
                        LOCAL_LEAD_TIME, &
                        ANNUAL_LEVELIZED_CAPITAL, &
                        ANNUAL_FUEL_COST, &
                        FUEL_COST_PER_MWH, &
                        EXPANSION_CO2, &
                        VAR_OM_PER_MWH, &
                        FIX_OM_COST, &
                        MULTI_AREA_NAME, &
                        ANNUAL_ENERGY
         SAVE &
               OPTION_NAME, &
               OPTION_POSITION, &
               LAST_RESOURCE_ADDED, &
               FILE_SOURCE_INDEX, &
               RESOURCE_TYPE_INDEX, &
               LOCAL_RESOURCE_INDEX, &
               MRX_ICAP_UNIT_LINK
!
! DETAILED REPORTING
!
      LOGICAL (kind=1) ::  GRX_SUMMARY_REPORT
      LOGICAL (kind=1) ::  EXPANSION_REPORT
      LOGICAL (kind=1) ::  GRX_REPORT_NOT_OPEN=.TRUE.
      LOGICAL (kind=1) ::  YES_INCLUDE_ICAP_REVENUE
      LOGICAL (kind=1) ::  INCLUDE_ICAP_REVENUE
      INTEGER (kind=2) ::  GRX_REPORT_VARIABLES
      INTEGER (kind=2) ::  GRX_ANNUAL_ALT_NO=0
      INTEGER (kind=2) ::  GRX_ANNUAL_HEADER
      INTEGER :: GRX_ANNUAL_ALT_REC
      SAVE GRX_ANNUAL_ALT_REC
!
      LOGICAL (kind=1) ::  FUEL_POINTERS_USED
      LOGICAL (kind=1) ::  EXISTING_UNIT

      INTEGER (kind=2) ::  N
      INTEGER (kind=2) ::  FT
      INTEGER (kind=2) ::  MO
      INTEGER (kind=2) ::  GET_NUNITS_POSITION
      INTEGER (kind=2) ::  FUEL_INVENTORY_ID(0:1024)
      INTEGER (kind=2) ::  GET_PRIMARY_MOVER
      INTEGER (kind=2) ::  GET_S_PRIMARY_MOVER
      INTEGER (kind=4) ::   CO2_INT4=3
      REAL (kind=4) ::  GET_SCENARIO_COAL_PRICES
      REAL (kind=4) ::  GET_SCENARIO_GAS_PRICES
      REAL (kind=4) ::  GET_SCENARIO_OIL_PRICES
      REAL (kind=4) ::  GET_SCENARIO_URANIUM_PRICES
      REAL (kind=4) ::  FUEL_SCEN_MULT(5)
      REAL (kind=4) ::  RTEMP
      REAL (kind=4) ::  PUT_MRX_DELIVERY_COST
      REAL (kind=4) ::  GET_MONTHLY_FUEL_INDEX
      REAL (kind=4) ::  R_DELIVERY_COST
      REAL (kind=4) ::  VOID_R4
      REAL (kind=4) ::  MAXIMUM_ANNUAL_MRX_CAP(:)
      REAL (kind=4) ::  ICAP_REVENUE_MULT(:)
      REAL (kind=4) ::  MINIMUM_ANNUAL_MRX_CAP(:)
      REAL (kind=4) ::  GET_MIN_CAP_TESTING_RATIO
      REAL (kind=4) ::  GET_MAX_CAP_TESTING_RATIO
      REAL (kind=4) ::  MINIMUM_TG_MRX_CAP(:)
      REAL (kind=4) ::  MAXIMUM_TG_MRX_CAP(:)
      REAL (kind=4) ::  GET_MAXIMUM_ANNUAL_MRX_CAP
      REAL (kind=4) ::  TOTAL_ANNUAL_CAPACITY_ADDED(:)
      REAL (kind=4) ::  TG_TOTAL_ANNUAL_CAPACITY_ADDED(:)
      REAL (kind=4) ::  ANN_CAP
      REAL (kind=4) ::  EL_PLANNING_CAPACITY
      REAL (kind=4) ::  CL_PLANNING_CAPACITY
      REAL (kind=4) ::  CT_PLANNING_CAPACITY
      REAL (kind=4) ::  LM_PLANNING_CAPACITY
      REAL (kind=4) ::  ADJUSTMENT_CAPACITY
      REAL (kind=4) ::  DERIV_CAPACITY_PLANNING
      REAL (kind=4) ::  DERIV_CAPACITY_PLANNING_BY_TG
      REAL (kind=4) ::  CURRENT_TARGET_RATIO
      REAL (kind=4) ::  CURRENT_TARGET_RATIO_PA(:)
      REAL (kind=4) ::  TARGET_RATIO
      REAL (kind=4) ::  RETURN_RESERVE_MARGIN_RATIO
      REAL (kind=4) ::  MAXIMUM_RM
      REAL (kind=4) ::  MAXIMUM_RM_PA(:)
      REAL (kind=4) ::  MAX_CAPACITY_TESTING_MARGIN
      REAL (kind=4) ::  ICAP_MIN_TESTING_MARGIN
      REAL (kind=4) ::  ICAP_MAX_TESTING_MARGIN
      REAL (kind=4) ::  TOTAL_REGIONAL_CAPACITY_NEEDED
      REAL (kind=4) ::  GET_EMISS_CAP_FOR_CLASS
      REAL (kind=4) ::  CO2_EMISSIONS_CAP(:)
      REAL (kind=4) ::  CO2_EMISS_REDUCTION_REQUIRED(:)
      REAL (kind=4) ::  CO2_EMISSIONS(:)
      REAL (kind=4) ::  GET_CL_EMISS_FOR_CLASS
      REAL (kind=4) ::  CO2_UNIT_MW
      REAL (kind=4) ::  CO2_UNIT_MWH
      REAL (kind=4) ::  UNIT_CO2
      REAL (kind=4) ::  UNIT_CO2_AFTER
      REAL (kind=4) ::  CO2_UNIT_PRICE
      REAL (kind=4) ::  GET_CL_CO2_TON_PER_MWH
      CHARACTER (len=1) ::   USE_MINIMUM_RM
      LOGICAL (kind=1) ::  USE_MAXIMUM_RM_LOGIC
      LOGICAL (kind=1) ::  USE_MINIMUM_RM_LOGIC
      LOGICAL (kind=1) ::  USE_REGIONAL_MINIMUM_RM_LOGIC
      LOGICAL (kind=1) ::  USE_REGIONAL_MAXIMUM_RM_LOGIC
      LOGICAL (kind=1) ::  TARGET_MET
      LOGICAL (kind=1) ::  YES_USE_MINIMUM_RM
      LOGICAL (kind=1) ::  VOID_LOGICAL
      LOGICAL (kind=1) ::  RESOURCE_ADDITION
      LOGICAL (kind=1) ::  TEMP_L
      LOGICAL (kind=1) ::  INIT_ANNUAL_RPS_SUPPLY_CURVES
      LOGICAL (kind=1) ::  PUT_TRANS_MRX_DISP_EMIS_ADDER
      LOGICAL (kind=1) ::  PUT_GRX_DISP_EMIS_ADDER

      LOGICAL (kind=1) ::  GET_NEXT_MRX_RETIRE_RETRO
      LOGICAL (kind=1) ::  BUILD_RPS_SUPPLY_CURVES
      LOGICAL (kind=1) ::  CO2_END_LIST
      LOGICAL (kind=1) ::  GET_CO2_RETIREMENTS_LOGIC
      LOGICAL (kind=1) ::  CO2_RETROFIT_LOGIC_ACTIVE
      LOGICAL (kind=1) ::  TRANS_MRX_LINK
!
! DECLARATION FOR RESERVE MARGIN BY TG
!
      INTEGER (kind=2) :: &
           GET_PA_FROM_TG, &
           GET_PA_VALUE_FROM_TG, &
           LOCAL_PA, &
           TG_2_PLANNING_AREA(:), &
           PG, &
           R_PA, &
           GET_NUMBER_OF_PLANNING_GROUPS, &
           CO2_PLN=3 , &
           CO2_COUNTER, &
           CO2_RETIRE_OR_RETRO_OR_MARKET, &
           CO2_NUNIT, &
           CO2_TG, &
           BASE_DATE, &
           SYS=0 , &
           LINK_CN, &
           LINK_I
      REAL ::  PLANNING_CAPACITY(:)
      REAL ::  TG_PLANNING_CAPACITY
      REAL ::  PEAK_AFTER_INTERRUPTIBLE_TG
      REAL ::  R_RESERVE
      REAL ::  TG_PLANNING_RESERVE_MARGIN
      REAL ::  PEAK_AFTER_INTERRUPTIBLE(:)
      REAL ::  EXISTING_PLANNING_CAPACITY(0:6)
      REAL ::  NEW_PLANNING_CAPACITY(0:6)
      REAL ::  AFTER_PEAK_CAPACITY
      REAL ::  RETIREMENT_CAPACITY
      REAL ::  GET_ANNUAL_PEAK
      REAL ::  ENRG_LIMITED_COST
      REAL ::  PLANNING_RESERVE_MARGIN(:)
      REAL ::  PLANNING_RM_BEFORE_ADDITIONS
      REAL ::  PLANNING_PEAK_BEFORE_ADDITIONS
      REAL ::  GET_CL_TG_CAP
      REAL ::  GET_GROUP_PEAK_ON_PEAK_MONTH
      REAL ::  GET_EL_TG_CAP
      REAL ::  GET_CL_AFTER_PEAK
      REAL ::  GET_CL_TG_RETIRE
      REAL ::  GET_PEAK_ON_PA_PEAK_MONTH
      ALLOCATABLE :: PLANNING_RESERVE_MARGIN, &
                     PEAK_AFTER_INTERRUPTIBLE, &
                     PLANNING_CAPACITY, &
                     TOTAL_ANNUAL_CAPACITY_ADDED, &
                     TG_TOTAL_ANNUAL_CAPACITY_ADDED, &
                     MAXIMUM_ANNUAL_MRX_CAP, &
                     ICAP_REVENUE_MULT, &
                     MINIMUM_ANNUAL_MRX_CAP, &
                     MINIMUM_TG_MRX_CAP, &
                     MAXIMUM_TG_MRX_CAP, &
                     CO2_EMISSIONS_CAP, &
                     CO2_EMISS_REDUCTION_REQUIRED, &
                     CO2_EMISSIONS, &
                     TG_2_PLANNING_AREA, &
                     CURRENT_TARGET_RATIO_PA, &
                     MAXIMUM_RM_PA
      REAL :: &
            INTERRUPTIBLE_LOAD, &
            GET_ANNUAL_INTER_CAPACITY, &
            ZERO,NEAR_ZERO, &
            CAPACITY_PLANNING_PEAK, &
            CO2_RETIREMENT_PRICE, &
            CO2_RETIRE_RETRO_PRICE, &
            INIT_CO2_RETIRE_RETRO_PRICE, &
            GET_CO2_RETIREMENT_PRICE, &
            GET_CO2_RETIRE_RETRO_PRICE, &
            CO2_NET_MARGIN, &
            TEMP_R4
      PARAMETER (ZERO=0.,NEAR_ZERO=.0000001)
      SAVE PLANNING_RESERVE_MARGIN
      REAL (kind=8) ::  CO2_COEFF
      REAL (kind=8) ::  CO2_POWER
      REAL (kind=8) ::  CO2_PERCENT_OF_CAP
      LOGICAL (kind=1) ::  CO2_CAP_NOT_MET
      LOGICAL (kind=1) ::  CO2_RESOURCES_AVAIL
      LOGICAL (kind=1) ::  RESOURCES_PROFITABLE
      LOGICAL (kind=1) ::  RESERVES_NOT_MET
      LOGICAL (kind=1) ::  ENRG_RESOURCES_AVAIL
      LOGICAL (kind=1) ::  RPS_CAPS_NOT_MET
      LOGICAL (kind=1) ::  RPS_EXPOURCES_AVAIL
      LOGICAL (kind=1) ::  UNIT_PROFITABLE
      LOGICAL (kind=1) ::  VIOLATES_MAXIMUM_TG_RESERVE
      LOGICAL (kind=1) ::  NEED_FOR_MINIMUM_RESERVE
      LOGICAL (kind=1) ::  VIOLATES_MAXIMUM_RESERVE
      LOGICAL (kind=1) ::  RESOURCES_ADDED
      INTEGER (kind=2) ::  R
      INTEGER (kind=2) ::  ALL_RESOURCES
      INTEGER (kind=2) ::  CO2_RETIREMENT_OPTIONS
      INTEGER (kind=2) ::  CO2_RETROFIT_OPTIONS
      real :: ignored_result
      integer (kind=2) :: tot_all_options_cap
      
!
! END DATA DECLARATIONS
!


      ENTRY INIT_MRX_GREEN_OBJECT

         if (file_trace_mg==0) then
            file_trace_mg=open_trace("MRX_GREEN.trace", rq_co_mg)
         end if
         CALL FUEL_PRICE_FILES_ACTIVE(FUEL_PRICE_DATA_AVAILABLE)

         LAST_SORTED_POSITION = 0

         UPPER_TRANS_GROUP = GET_NUMBER_OF_ACTIVE_GROUPS()
         IF(ALLOCATED(LAST_RESOURCE_ADDED)) &
                                         DEALLOCATE(LAST_RESOURCE_ADDED)
         ALLOCATE(LAST_RESOURCE_ADDED(MAX(INT(1,2),UPPER_TRANS_GROUP)))
         LAST_RESOURCE_ADDED = 0

         tot_all_options_cap = &
                        INITIALIZE_SCREEN_DATA()
         IF(ALLOCATED(OPTION_NAME)) &
                        DEALLOCATE(OPTION_NAME,OPTION_POSITION)
         IF(tot_all_options_cap > 0) THEN
            ALLOCATE(OPTION_NAME(tot_all_options_cap), &
                                     OPTION_POSITION(tot_all_options_cap))
            ! TODO: Can this be combined? (DRY)
            CAPTURED_OPTIONS_NAMES = GET_OPTION_NAMES(OPTION_NAME, &
                                      OPTION_POSITION,tot_all_options_cap)
            call write_trace_strings(file_trace_mga, "OPTIONs", &
                OPTION_NAME)
            call write_trace_bool1(file_trace_mga, "Captured", &  
                CAPTURED_OPTIONS_NAMES)
                
                
         ENDIF
         ANNUAL_MARKET_OPTION_SWITCH = .FALSE.
      RETURN


      ENTRY ANNUAL_MRX_GREEN(R_YEAR)

! THIS ROUTINE NEEDS TO BE ADDED AFTER PRICES HAVE BEEN CALCULATED
! AND BEFORE THE NEXT YEAR'S SIMULATION.

! THIS PROGRAM IS COPIED HEAVILY FROM ANNUAL_SCREENING ABOVE.
! THE PURPOSE OF THIS PROGRAM IS TO CACULATE THE ANNUAL COSTS OF EACH
! OPTION FOR CAPACITY FACTORS FROM 0% TO 100%.

! THE TOTAL COST AT EACH CAPACITY FACTOR IS TOTAL FIXED (INTERCEPT)
! PLUS THE SLOPE (FUEL + VOM) AT EACH CAPACITY FACTOR POINT.

! CALLS FOR THE FIXED, VARIABLE AND CAPACITY ARE MADE TO CLA_OBJT.FOR.
! THERE ARE A NUMBER OF WAYS THAT THE CODE NEEDS TO BE MADE MORE ROBUST:
!     - FUEL PRICES SHOULD BE CALLABLE FROM THE FUEL PRICE FILE
!     - COSTS OF THE RESOURCE SHOULD BE CALCULATED MONTHLY, ESP. FOR
! GAS UNITS
!     - CAPACITIES NEED TO CHANGE MONTH/ANNUAL

! ALSO, TVA WANTS TO BE ABLE TO RETIRE AS WELL AS ADD RESOURCES.

         if(file_trace_mga/=0) then
            file_trace_mga=open_trace("annual_mrx_green.txt", rq_mga)
            
         end if
         
         call write_trace_message(file_trace_mga, "+ANNUAL_MRX_GREEN")
         RUN_PRICE_MODE = YES_STRICT_MARKET_PRICE()
         RUN_AREA_PRICE_MODE = YES_MULTI_AREA_PRICE() ! ADDED 07/30/03.
         RUN_MULTIAREA = YES_RUN_MULTIAREA_TRANSACT()
         INCLUDE_ICAP_REVENUE = YES_INCLUDE_ICAP_REVENUE()
         GAS_MODEL_DATA_AVAILABLE = YES_GAS_MODEL_DATA_AVAILABLE()
            if(globecom_year==0) then
               call end_program("cap_objt:0047 - year must be nonzero.")
            end if

         PEAK =  UPDATE_NET_PLANNING_PEAK(globecom_year)
         call write_trace_real(file_trace_mga, "PEAK", PEAK)
         
         ANN_CAP =  EL_PLANNING_CAPACITY(3,R_YEAR) + &
                    CL_PLANNING_CAPACITY(3,R_YEAR) + &
                    CT_PLANNING_CAPACITY(3,R_YEAR) + &
                    LM_PLANNING_CAPACITY(R_YEAR)   + &
                    ADJUSTMENT_CAPACITY(R_YEAR)    + &
                    DERIV_CAPACITY_PLANNING(R_YEAR)
         call write_trace_real(file_trace_mga, "2. ANN_CAP", ANN_CAP)
!
         IF(GRX_ITERATIONS == 0) THEN
            VOID_INT2 = UPDATE_CL_SCREEN_COSTS(R_YEAR)

            ignored_result = UPDATE_SCREEN_CAP_COSTS(R_YEAR)
         ENDIF
!
         IF(globecom_year > 1 .AND. GRX_ITERATIONS == 0) THEN 

            BALANCE_WITH_CO2_PRICE = .FALSE.
            IF(GET_CO2_RETIREMENTS_LOGIC() .OR. &
                                      CO2_RETROFIT_LOGIC_ACTIVE()) THEN
               write(9,*) "CO2 Price for next iter", &
                           CURRENT_CO2_DISPATCH_COST
               CALL flush_unit(INT(9,2))
               CALL flush_unit(INT(4,2))
!
               INIT_CO2_RETIRE_RETRO_PRICE = &
                             MAX(0.0,CURRENT_CO2_DISPATCH_COST * 0.0005)
               TEMP_L = PUT_GRX_DISP_EMIS_ADDER(CO2_INT4, &
                                          0_2, &
                                          INIT_CO2_RETIRE_RETRO_PRICE)
            ENDIF
            call write_trace_message(file_trace_mga, &
            "Setting CO2_RETIREMENTS_LOGIC/CO2_RETOFIT_LOGIC False")
            
            CO2_RETIREMENTS_LOGIC = .FALSE.
            CO2_RETOFIT_LOGIC = .FALSE.   !MSG AUG 09

         ELSE
            CO2_RETIREMENTS_LOGIC = GET_CO2_RETIREMENTS_LOGIC()
         call write_trace_bool1(file_trace_mga, "CO2_RETIREMENTS_LOGIC", &
            CO2_RETIREMENTS_LOGIC)
            
            CO2_RETOFIT_LOGIC = CO2_RETROFIT_LOGIC_ACTIVE() !MSG AUG 09
        call write_trace_bool1(file_trace_mga, "CO2_RETOFIT_LOGIC", &
            CO2_RETOFIT_LOGIC)
            
         ENDIF
!
         IF(.NOT.(CO2_RETIREMENTS_LOGIC .OR. CO2_RETOFIT_LOGIC)) THEN
            IF(tot_all_options_cap < 1 .OR. &
                      .NOT. (RUN_MULTIAREA .OR. RUN_PRICE_MODE .OR. &
                                           RUN_AREA_PRICE_MODE) ) then
       call write_trace_message(file_trace_mga, "1. -ANNUAL_MRX_GREEN")
                RETURN
            end if
         ENDIF
         HIGHEST_INTERVAL = MAX_SCREENING_INTERVALS
         call write_trace_int2(file_trace_mga, "HIGHEST_INTERVAL", &
            HIGHEST_INTERVAL)
            
         IF(ALLOCATED(SCREEN_SLOPE)) then 
            DEALLOCATE( &
                  SCREEN_SLOPE, &
                  SCREEN_INTERCEPT, &
                  SCREEN_CAPACITY, &
                  EQUIVALENT_CAPACITY, &
                  RPS_HOURS, &
                  RPS_OPTION, &
                  RPS_ENERGY, &
                  RPS_MULT, &
                  ANNUAL_FIXED_COST, &
                  STRIKE_PRICE, &
                  STATE_PROVINCE, &
                  FILE_SOURCE_INDEX, &
                  RESOURCE_TYPE_INDEX, &
                  LOCAL_RESOURCE_INDEX, &
                  MRX_ICAP_UNIT_LINK, &
                  LOCAL_ANNUAL_UNITS, &
                  RESOURCE_COUNTER, &
                  LOCAL_LEAD_TIME, &
                  ANNUAL_LEVELIZED_CAPITAL, &
                  ANNUAL_FUEL_COST, &
                  FUEL_COST_PER_MWH, &
                  EXPANSION_CO2, &
                  VAR_OM_PER_MWH, &
                  FIX_OM_COST, &
                  ANNUAL_ENERGY, &
                  ANNUAL_VARIABLE_COST, &
                  MARKET_REVENUE, &
                  HIGHEST_ENERGY, &
                  MARKET_COST, &
                  NET_MARGIN, &
                  NET_MARGIN_PER_KWYR, &
                  NET_MARGIN_INTERVAL, &
                  THIS_YEAR_PLUS_LEAD_TIME, &
                  MARKET_CUM_REVENUE, &
                  MARKET_DURATION, &
                  MARKET_PRICE)
         end if
         ALLOCATE(SCREEN_SLOPE(tot_all_options_cap), &
                  SCREEN_INTERCEPT(tot_all_options_cap), &
                  SCREEN_CAPACITY(tot_all_options_cap), &
                  EQUIVALENT_CAPACITY(tot_all_options_cap), &
                  RPS_HOURS(tot_all_options_cap), &
                  RPS_OPTION(tot_all_options_cap), &
                  RPS_ENERGY(tot_all_options_cap), &
                  RPS_MULT(tot_all_options_cap), &
                  ANNUAL_FIXED_COST(tot_all_options_cap), &
                  STRIKE_PRICE(tot_all_options_cap,13), &
                  ns_cap_trans%transaction_group(tot_all_options_cap), &
                  STATE_PROVINCE(tot_all_options_cap), &
                  FILE_SOURCE_INDEX(tot_all_options_cap), &
                  RESOURCE_TYPE_INDEX(tot_all_options_cap), &
                  LOCAL_RESOURCE_INDEX(tot_all_options_cap), &
                  LOCAL_ANNUAL_UNITS(tot_all_options_cap), &
                  RESOURCE_COUNTER(tot_all_options_cap), &
                  LOCAL_LEAD_TIME(tot_all_options_cap), &
                  ANNUAL_LEVELIZED_CAPITAL(tot_all_options_cap), &
                  ANNUAL_FUEL_COST(tot_all_options_cap), &
                  FUEL_COST_PER_MWH(tot_all_options_cap,13), &
                  EXPANSION_CO2(tot_all_options_cap), &
                  VAR_OM_PER_MWH(tot_all_options_cap), &
                  FIX_OM_COST(tot_all_options_cap), &
                  ANNUAL_ENERGY(tot_all_options_cap), &
                  ANNUAL_VARIABLE_COST(tot_all_options_cap), &
                  MARKET_REVENUE(tot_all_options_cap), &
                  HIGHEST_ENERGY(tot_all_options_cap), &
                  MARKET_COST(tot_all_options_cap), &
                  NET_MARGIN(tot_all_options_cap), &
                  NET_MARGIN_PER_KWYR(tot_all_options_cap), &
                  NET_MARGIN_INTERVAL(tot_all_options_cap), &
                  THIS_YEAR_PLUS_LEAD_TIME(tot_all_options_cap), &
                  MARKET_CUM_REVENUE(0:HIGHEST_INTERVAL, &
                                                   UPPER_TRANS_GROUP), &
                  MARKET_DURATION(0:HIGHEST_INTERVAL, &
                                                   UPPER_TRANS_GROUP), &
                  MARKET_PRICE(0:HIGHEST_INTERVAL, &
                                                   UPPER_TRANS_GROUP), &
                  MRX_ICAP_UNIT_LINK(0:UPPER_TRANS_GROUP))
         SCREEN_SLOPE = 9999.
         SCREEN_INTERCEPT = 9999.
         SCREEN_CAPACITY = 0.
            call write_trace_real4(file_trace_mga, "3. SC init", &
                SCREEN_CAPACITY(1))
         EQUIVALENT_CAPACITY = 0.
         RPS_HOURS = 0.
         RPS_OPTION = 0
         RPS_RESOURCE_COUNT = 0
         RPS_ENERGY = 0.
         RPS_MULT = 0.
         ANNUAL_FIXED_COST = 0.
         ANNUAL_VARIABLE_COST = 0.
         ANNUAL_ENERGY = 0.
        call write_trace_real4(file_trace_mga, "15. AE-init", ANNUAL_ENERGY(1))
         STRIKE_PRICE = 9999.
         ns_cap_trans%transaction_group = 0
         STATE_PROVINCE = 0
         FILE_SOURCE_INDEX = 0
         RESOURCE_TYPE_INDEX = 0
         LOCAL_RESOURCE_INDEX = 0
         LOCAL_ANNUAL_UNITS = 9999
         RESOURCE_COUNTER = 0
         LOCAL_LEAD_TIME = 0
         ANNUAL_LEVELIZED_CAPITAL = 0.
         ANNUAL_FUEL_COST = 0.
         FUEL_COST_PER_MWH = 0.
         EXPANSION_CO2 = 0.
!
         MRX_ICAP_UNIT_LINK = 0
!
         VAR_OM_PER_MWH = 0.
         FIX_OM_COST = 0.
         THIS_YEAR_PLUS_LEAD_TIME = 0
         CO2_CAP_INCREASE_FROM_MARKET = 0.
         CO2_CAP_EXPENDITURES = 0.
         GRX_CO2_MARKET_PTS_USED = 0

         CO2_RETIRE_RETRO_PRICE = -9999.
         INIT_CO2_RETIRE_RETRO_PRICE = -9999.
!
         PG = MAX(GET_NUMBER_OF_PLANNING_GROUPS(),INT(1,2))
         call write_trace_int2(file_trace_mga, "PG", PG)
         
!
         SCENARIO_CAP_COST_MULT = &
                            GET_SCENARIO_EXPAND_CAP_COST(R_YEAR,1_2)
       call write_trace_real4(file_trace_mga, "SCENARIO_CAP_COST_MULT", &
        SCENARIO_CAP_COST_MULT)
        
         SCENARIO_RESERVE_MARGIN = &
                             GET_SCENARIO_RESERVE_MARGIN(R_YEAR,1_2)

         GRX_SUMMARY_REPORT = EXPANSION_REPORT()
      call write_trace_bool1(file_trace_mga, "GRX_SUMMARY_REPORT", &
        GRX_SUMMARY_REPORT)
        

         MINIMUM_NET_MARGIN = GET_ADDITIONS_PROFIT_PER_MWH()
      
         MAXIMUM_RM = MAX_CAPACITY_TESTING_MARGIN(R_YEAR)
         VOID_LOGICAL = YES_USE_MINIMUM_RM(USE_MINIMUM_RM)
         ANNUAL_RESOURCES_AVAILABLE = .FALSE.

         PLANNING_PER_MWH = .TRUE.

         USE_MINIMUM_RM_LOGIC = .FALSE.
         USE_REGIONAL_MINIMUM_RM_LOGIC = .FALSE.
         USE_REGIONAL_MAXIMUM_RM_LOGIC = .FALSE.
         MRX_SORTED_MIX =  USE_MINIMUM_RM == 'X'
         IF(USE_MINIMUM_RM == 'U' .OR. USE_MINIMUM_RM == 'B') THEN
            USE_MINIMUM_RM_LOGIC = .TRUE.
         ELSEIF(USE_MINIMUM_RM == 'R' .OR. USE_MINIMUM_RM == 'X') THEN
            USE_REGIONAL_MINIMUM_RM_LOGIC = .TRUE.
         ENDIF

         IF(USE_MINIMUM_RM == 'A' .OR. &
                      USE_MINIMUM_RM == 'B' .OR. &
                                 USE_MINIMUM_RM == 'R' .OR. &
                                             USE_MINIMUM_RM == 'X') THEN
            USE_MAXIMUM_RM_LOGIC = .TRUE.
         ELSE
            USE_MAXIMUM_RM_LOGIC = .FALSE.
         ENDIF

         PLANNING_PER_MWH = YES_PLAN_TO_MWH_OR_KWYR()

         REGIONAL_PARAMS_EXIST = YES_REGIONAL_PARAMS_EXIST()


         YES_USE_EMIS_IN_MRX = USE_EMIS_IN_MRX()


         THIS_YEAR = R_YEAR + get_BASE_YEAR()

! FIRST, GET FIXED AND VARIABLE COST BY OPTION BY INTERVAL


! FOR NOW, DO NOT BUILD IN THE FIRST YEAR.

         IF(.NOT. ANNUAL_MARKET_OPTION_SWITCH .OR. &
                                          .NOT. YES_RUN_TRANSACT()) THEN

            yes_rt=YES_RUN_TRANSACT()

            
            IF(yes_rt) THEN
               ANNUAL_MARKET_OPTION_SWITCH = .TRUE.
            ENDIF

            IF(globecom_year == 1) then
       call write_trace_message(file_trace_mga, "2. -ANNUAL_MRX_GREEN")
                RETURN
            end if
         ENDIF
         DO CN = 1, tot_all_options_cap
            I = OPTION_POSITION(CN)
            call write_trace_int2(file_trace_mga, "10. I", I)
            IF(GET_RESOURCE_TYPE_INDEX(I) == 4 .AND. &
         .NOT. CO2_RETIREMENTS_LOGIC) then
                CYCLE !MSG AUG 09
            end if
            IF(GET_RESOURCE_TYPE_INDEX(I) == 5 .AND. &
               .NOT. CO2_RETOFIT_LOGIC) then
              CYCLE !MSG AUG 09
            end if
            FILE_SOURCE_INDEX(CN) = GET_FILE_SOURCE_INDEX(I)
            RESOURCE_TYPE_INDEX(CN) = GET_RESOURCE_TYPE_INDEX(I)

! COMMON TO ALL OPTIONS.

            NET_MARGIN(CN) = -999999.
       call write_trace_int2(file_trace_mga, "CN", CN)
       call write_trace_real4(file_trace_mga, "4. NM",  &
        NET_MARGIN(CN))
            NET_MARGIN_PER_KWYR(CN) = -999999.
            NET_MARGIN_INTERVAL(CN) = -1

            LOCAL_ANNUAL_UNITS(CN) = ANNUAL_UNITS_FOR_OPTION(I)
            LOCAL_LEAD_TIME(CN) = LEAD_TIME_FOR_OPTION(I)
            IF(LOCAL_ANNUAL_UNITS(CN) < 0) THEN 
            ! ASSUME IT IS A POINTER PER PACIFICORP.
               LOCAL_ANNUAL_UNITS(CN) = &
                     INT(GET_VAR(FLOAT(LOCAL_ANNUAL_UNITS(CN)),R_YEAR, &
                                                     OPTION_NAME(CN)),2)
            ENDIF
            LOCAL_ANNUAL_UNITS(CN) = &
                            MIN(CUMULATIVE_UNITS_FOR_OPTION(I), &
                                                 LOCAL_ANNUAL_UNITS(CN))
            IF(LOCAL_ANNUAL_UNITS(CN) > 0) THEN
               ANNUAL_RESOURCES_AVAILABLE = .TRUE.
            ENDIF
            THIS_YEAR_PLUS_LEAD_TIME(CN) = &
                                     THIS_YEAR + LEAD_TIME_FOR_OPTION(I)
         ENDDO
         CO2_RETIREMENTS_LOGIC = CO2_RETIREMENTS_LOGIC .OR. &
                                         CO2_RETOFIT_LOGIC     ! AUG 09
!
! 072209. RENEWABLE, EFFICIENCY. CAN BE IN THE SAME LOOP LATER.
! 083009. CHANGED TO FILE_SOURCE_INDEX
!
         MO = 7
         DO CN = 1, tot_all_options_cap 

            IF(FILE_SOURCE_INDEX(CN) /= 3) then
                call write_trace_message(file_trace_mga, "16. CYCLE")
                CYCLE ! aug 09
            end if
!
            I = OPTION_POSITION(CN)
            call write_trace_int2(file_trace_mga, "OPTION_POSITION", I)
            
            IF( .NOT. RESOURCE_AVAILABLE(I, &
                                    THIS_YEAR_PLUS_LEAD_TIME(CN)) .OR. &
                  .NOT. TEST_START_STOP_YEARS(I, &
                                    THIS_YEAR_PLUS_LEAD_TIME(CN))) then
                call write_trace_message(file_trace_mga, "2. CYCLE")
                CYCLE
            end if
            co_prod_pointer=GET_PRODUCTION_DATA_POINTER(I)
            

            LOCAL_RESOURCE_INDEX(CN) =  & !  TRANS RECORD POSITION
                                      GET_TRANS_FOR_GRX_ID(co_prod_pointer)
            TRANS = LOCAL_RESOURCE_INDEX(CN)
            call write_trace_int2(file_trace_mga, "TRANS", TRANS)
            ns_cap_trans%transaction_group(CN) = GET_TRANS_GROUP_FOR_TRANS(TRANS)
            call write_trace_int2(file_trace_mga, "XactionGp", &
                ns_cap_trans%transaction_group(CN))
                
            IF(GET_TRANS_GROUP_POSITION(ns_cap_trans%transaction_group(CN)) <= 0) then
                call write_trace_message(file_trace_mga, "3. CYCLE")
                CYCLE
            end if
            STATE_PROVINCE(CN) = GET_TRANS_STATE_INDEX(TRANS)
            call write_trace_int2(file_trace_mga, &
                "State", STATE_PROVINCE(CN))
            
            IF(RESOURCE_TYPE_INDEX(CN) == 2) THEN
               IF(STATE_PROVINCE(CN) > 0) then
                RPS_OPTION(CN) = 1
               end if
       
               RPS_RESOURCE_COUNT = RPS_RESOURCE_COUNT + 1
       call write_trace_int2(file_trace_mga, "RC", RPS_RESOURCE_COUNT)
            ENDIF

            TEMP_L1 = GET_ESC_NRG_PRICE(R_YEAR, &
                                      MO, &
                                      TRANS, &
                                      TEMP_R4)
            VAR_OM_PER_MWH(CN) = TEMP_R4
            call write_trace_real4(file_trace_mga, "2. VAR_OM_PER_MWH(CN)", &
                VAR_OM_PER_MWH(CN))
            call write_trace_int2(file_trace_mga, "2. (CN)", CN)
            

            SCREEN_CAPACITY(CN) = GET_SCREEN_CAPACITY(I)
            call write_trace_int2(file_trace_mga, "CN", CN)
            call write_trace_real4(file_trace_mga, "4. SC", &
                SCREEN_CAPACITY(CN))
            EQUIVALENT_CAPACITY(CN) = SCREEN_CAPACITY(CN)

            IF(SCREEN_CAPACITY(CN) <= 0.) THEN
               WRITE(4,*) "EXPANSION CAPACITY NEGATIVE IN MRX"
               WRITE(4,*) "FOR RESOURCE",OPTION_NAME(CN)
               WRITE(4,*) "OPTION CAPACITY RESET TO 100 MW"
               SCREEN_CAPACITY(CN) = 100.
            call write_trace_int2(file_trace_mga, "CN", CN)
            call write_trace_real4(file_trace_mga, "5. SC", &
                SCREEN_CAPACITY(CN))
            ENDIF
            RPS_USER_PATTERN = GET_TRANS_RPS_USER_PATTERN(TRANS)
            call write_trace_int2(file_trace_mga, "USER_PATTERN", &
                RPS_USER_PATTERN)
                
            IF(RPS_USER_PATTERN > 0) THEN
!
! ESTIMATED ENERGY BASED ON SUM OF LOAD PATTERN * ANNUAL CAPACITY
!
               RPS_HOURS(CN) = 4.345 * &
                         GET_SUM_USER_DAY_DATA(0_2,RPS_USER_PATTERN)
               RPS_ENERGY(CN) = RPS_HOURS(CN) * &
                                  SCREEN_CAPACITY(CN)
            ELSE
               RPS_ENERGY(CN) = 0.0
            ENDIF
            FIX_OM_COST(CN) = &
                   CALC_GRX_FIX_COST_PER_UNIT(TRANS, &
                                             R_YEAR, &
                                             1_2, &
                                             SCREEN_CAPACITY(CN), &
                                             RPS_ENERGY(CN))
            ANNUAL_LEVELIZED_CAPITAL(CN) = &
                              RETURN_RISK_ADJ_S_CAP_COST(I,R_YEAR) * &
                                                  SCENARIO_CAP_COST_MULT
!
            ANNUAL_FIXED_COST(CN) = &
                        1000000.*ANNUAL_LEVELIZED_CAPITAL(CN) + &
                                                         FIX_OM_COST(CN)
            STRIKE_PRICE(CN,MO) = & !  FUEL_COST_PER_MWH(CN,MO) +
                                                      VAR_OM_PER_MWH(CN)
          ENDDO
!
! INITIALIZE RESOURCE EXPANSION OPTIONS IN THIS YEAR.
!
         N = 0
         DO CN = 1, tot_all_options_cap 
         ! INDEX FROM THE CAPACITY OPTIONS FILE
            IF(FILE_SOURCE_INDEX(CN) /= 1) then
                call write_trace_message(file_trace_mga, "4. CYCLE")
                CYCLE
            end if
!
! 072209. CN IS THE LOCAL INDEX. I IS THE CAPACITY_OPTIONS INDEX.
!         PROD_POINTER = VARIABLE IN CAPACITY_OPTIONS FOR A GIVEN I
!
            I = OPTION_POSITION(CN)
       call write_trace_int2(file_trace_mga, "11. I", I)
            IF( .NOT. RESOURCE_AVAILABLE(I, &
                                    THIS_YEAR_PLUS_LEAD_TIME(CN)) .OR. &
                  .NOT. TEST_START_STOP_YEARS(I, &
                                    THIS_YEAR_PLUS_LEAD_TIME(CN))) then
                call write_trace_message(file_trace_mga, "5. CYCLE")
                cycle
            end if

            co_prod_pointer=&
                GET_PRODUCTION_DATA_POINTER(I)


!
            ns_cap_trans%transaction_group(CN) = &
               RETURN_S_TRANS_GROUP_ID(co_prod_pointer)
!
            IF(GET_TRANS_GROUP_POSITION(ns_cap_trans%transaction_group(CN)) <= &
                                                                0) then
                call write_trace_message(file_trace_mga, "5. CYCLE")
                CYCLE
            end if 
            call write_trace_int2(file_trace_mga, "Trans Group", &
                ns_cap_trans%transaction_group(CN))
                
            STATE_PROVINCE(CN) = GET_SCREEN_STATE_INDEX(co_prod_pointer)
      call write_trace_int2(file_trace_mga, "STATE", STATE_PROVINCE(CN))
            ANNUAL_RESOURCES_AVAILABLE = .TRUE.

            IF(RESOURCE_TYPE_INDEX(CN) == 2) THEN
               IF(STATE_PROVINCE(CN) > 0) then
                   RPS_OPTION(CN) = 1
               end if
               RPS_RESOURCE_COUNT = RPS_RESOURCE_COUNT + 1
            ENDIF

            N = CN
      call write_trace_int2(file_trace_mga, "Prod Ptr", co_prod_POINTER)
            FT = GET_S_PRIMARY_MOVER(co_prod_pointer)
      call write_trace_int2(file_trace_mga, "FT", FT)


            FUEL_SCEN_MULT(5) = 1.0

            IF(FT > 4 .OR. FT < 1) then
                FT = 5
                call write_trace_int2(file_trace_mga, "FT->", FT)
            end if

            R_DELIVERY_COST = -999999.

            VOID_R4 = PUT_MRX_DELIVERY_COST(R_DELIVERY_COST)

! 12/17/04. MAKE FUEL AND VOM VARIABLES WITH A SWITCH.

            DO MO= 1, 12
               IF(FUEL_PRICE_DATA_AVAILABLE) THEN

                  FUEL_SCEN_MULT(1) = &
                                  GET_SCENARIO_COAL_PRICES(R_YEAR,MO)
                  FUEL_SCEN_MULT(2) = &
                                  GET_SCENARIO_GAS_PRICES(R_YEAR,MO)
                  FUEL_SCEN_MULT(3) = &
                                  GET_SCENARIO_OIL_PRICES(R_YEAR,MO)
                  FUEL_SCEN_MULT(4) = &
                              GET_SCENARIO_URANIUM_PRICES(R_YEAR,MO)

                  RTEMP = GET_MONTHLY_FUEL_INDEX(CN,MO)

! ASSUME NO FUEL INVENTORIES.

! THE TWO CRITICAL ROUTINES.  THEY ARE HOOKED-UP TO EXISTING UNITS.
! THE INDICES IN THESE TWO ROUTINES ARE FOR EXISTING UNITS.

                  FUEL_POINTERS_USED = .FALSE.
                  EXISTING_UNIT = .FALSE.
                  if(co_prod_pointer<=1) then
                      call end_program("cap_objt:0002 - " // &
                     "Prod_pointer=0 before calling get_mrx_delivery_cost.")
                  end if
                  CALL GET_MRX_DELIVERY_COST(co_prod_pointer, &
                                          R_DELIVERY_COST, &
                                          MO,R_YEAR, &
                                          FUEL_POINTERS_USED, &
                                          EXISTING_UNIT)
                 if (file_trace_mga/=BAD_TRACE_HANDLE) then
                    write(file_trace_mga, *) &
                    "prod_pointer", co_prod_pointer, &
                     "R_DELIVERY_COST", R_DELIVERY_COST, &
                     "MO", MO,"R_YEAR", R_YEAR, &
                     "FUEL_POINTERS_USED", FUEL_POINTERS_USED, &
                     "EXISTING_UNIT", EXISTING_UNIT
                 end if
                 


                  IF(FUEL_POINTERS_USED) THEN
                    call write_trace_real4(file_trace_mga, "putting " // &
                      "R_DELIVERY_COST", R_DELIVERY_COST)
                      
                    VOID_R4 = PUT_MRX_DELIVERY_COST(R_DELIVERY_COST)
                  ENDIF
               ENDIF
! 082309.
               IF(GAS_MODEL_DATA_AVAILABLE .AND. FT == 2) THEN 
                  call write_trace_message(file_trace_mga, "Gas model")
                  GSP = MIN(MAX(INT(0,2), &
                        GET_SCRN_UNIT_GAS_REGION_INDEX(co_prod_pointer)), &
                                                      MAX_GAS_REGION_NO)
                  FUEL_COST_PER_MWH(CN,MO) = &
                                  GET_MONTHLY_GAS_PRICE_BY_STATE(GSP,MO)
               ELSE
       call write_trace_message(file_trace_mga, "no gas or FT!=2")
                  FUEL_COST_PER_MWH(CN,MO) = &
                       RETURN_CL_SCREEN_FUEL_COST(co_prod_pointer,R_YEAR, &
                                              HEAT_RATE, &
                                              FUEL_COST_PER_MMBTU) * &
                                              FUEL_SCEN_MULT(FT) * RTEMP
               ENDIF

! 03/17/05. ADDED FOR PAC AND BURESH
               call write_trace_bool1(file_trace_mga, "use_emis", &
                YES_USE_EMIS_IN_MRX)
               IF(YES_USE_EMIS_IN_MRX) THEN
                  CALL CALCULATE_NOX_SOX_VOM( &
                              co_prod_pointer, &
                              R_YEAR, &
                              MO, &
                              NOX_VOM, &
                              NOX_FOM, &
                              SOX_VOM, &
                              SOX_FOM, &
                              CO2_VOM, &
                              CO2_FOM, &
                              HG_VOM, &
                              HG_FOM, &
                              OTHER3_VOM, &
                              OTHER3_FOM, &
                              NOX_CONTROL_MULT, &
                              SOX_CONTROL_MULT, &
                              CO2_CONTROL_MULT, &
                              HG_CONTROL_MULT, &
                              OTHER3_CONTROL_MULT, &
                              ns_cap_trans%transaction_group(CN))
                 FUEL_COST_PER_MWH(CN,MO) = FUEL_COST_PER_MWH(CN,MO) + &
                    RETURN_CL_SCREEN_EMIS_COST(co_prod_pointer,R_YEAR,MO, &
                                               NOX_CONTROL_MULT, &
                                               SOX_CONTROL_MULT, &
                                               CO2_CONTROL_MULT, &
                                               HG_CONTROL_MULT, &
                                               OTHER3_CONTROL_MULT, &
                                             CO2_RETIRE_RETRO_PRICE) + &
                                           NOX_VOM + SOX_VOM + &
                                           CO2_VOM + HG_VOM + OTHER3_VOM
               ENDIF
            ENDDO

            MO = 7 ! FOR NOW.
            VAR_OM_PER_MWH(CN) = &
                                RETURN_OTHER_VAR_COMPONENT(co_prod_pointer)

            SCREEN_CAPACITY(CN) = GET_SCREEN_CAPACITY(I)
            call write_trace_int2(file_trace_mga, "CN", CN)
            call write_trace_real4(file_trace_mga, "6. SC", &
                SCREEN_CAPACITY(CN))
            EQUIVALENT_CAPACITY(CN) = SCREEN_CAPACITY(CN) * &
                       (1. - RETURN_CL_SCREEN_EFOR(co_prod_pointer,R_YEAR))
            IF(SCREEN_CAPACITY(CN) <= 0.) THEN
               WRITE(4,*) "EXPANSION CAPACITY NEGATIVE IN MRX"
               WRITE(4,*) "FOR RESOURCE",OPTION_NAME(CN)
               WRITE(4,*) "OPTION CAPACITY RESET TO 100 MW"
               SCREEN_CAPACITY(CN) = 100.
            call write_trace_int2(file_trace_mga, "CN", CN)
            call write_trace_real4(file_trace_mga, "7. SC", &
                SCREEN_CAPACITY(CN))
            ENDIF
!
            ANNUAL_LEVELIZED_CAPITAL(CN) = &
                              RETURN_RISK_ADJ_S_CAP_COST(I,R_YEAR) * &
                                                  SCENARIO_CAP_COST_MULT

            FIX_OM_COST(CN) = RETURN_CL_SCREEN_FIXED_COST(co_prod_pointer)

            ANNUAL_FIXED_COST(CN) = &
                        1000000.*ANNUAL_LEVELIZED_CAPITAL(CN) + &
                                                         FIX_OM_COST(CN)

! 03/05/04. BIG DEFINITIONAL CHANGE FOR STRIKE_PRICE

            STRIKE_PRICE(CN,MO) = FUEL_COST_PER_MWH(CN,MO) + &
                                                      VAR_OM_PER_MWH(CN)

         ENDDO ! OPTIONS

! NEXT, ASSUME ALL RESOURCES ARE ANNUAL CALL OPTIONS WHERE THE VARIABLE
! COST IS THE STRIKE PRICE AND
! UTILIZE THE CALL OPTIONS WITH THE HIGHEST NET MARGIN.

! 072209. RETURN CONDITION IS IF THERE ARE NO
!         EXPANSIONS OR CONTRACTION RESOURCES.

         UPPER_TRANS_GROUP = GET_NUMBER_OF_ACTIVE_GROUPS()

         CURRENT_TARGET_RATIO = TARGET_RATIO(3,R_YEAR) * &
                                                 SCENARIO_RESERVE_MARGIN

         IF(ALLOCATED(MULTI_AREA_NAME)) then
            DEALLOCATE(MULTI_AREA_NAME, &
                     PLANNING_RESERVE_MARGIN, &
                     PEAK_AFTER_INTERRUPTIBLE, &
                     PLANNING_CAPACITY, &
                     TG_2_PLANNING_AREA, &
                     CURRENT_TARGET_RATIO_PA, &
                     MAXIMUM_RM_PA, &
                    TOTAL_ANNUAL_CAPACITY_ADDED, &
                 TG_TOTAL_ANNUAL_CAPACITY_ADDED, &
                     MINIMUM_ANNUAL_MRX_CAP, &
                     MINIMUM_TG_MRX_CAP, &
                     MAXIMUM_TG_MRX_CAP, &
                     CO2_EMISSIONS_CAP, &
                   CO2_EMISS_REDUCTION_REQUIRED, &
                     CO2_EMISSIONS, &
                     MAXIMUM_ANNUAL_MRX_CAP, &
                     ICAP_REVENUE_MULT) 
         end if
         call write_trace_int2(file_trace_mga, "UTG", UPPER_TRANS_GROUP)
         ALLOCATE(MULTI_AREA_NAME(0:UPPER_TRANS_GROUP), &
                   PLANNING_RESERVE_MARGIN(0:UPPER_TRANS_GROUP), &
                   PEAK_AFTER_INTERRUPTIBLE(0:UPPER_TRANS_GROUP), &
                   PLANNING_CAPACITY(0:UPPER_TRANS_GROUP), &
                   TG_2_PLANNING_AREA(0:UPPER_TRANS_GROUP), &
                   CURRENT_TARGET_RATIO_PA(0:UPPER_TRANS_GROUP), &
                   MAXIMUM_RM_PA(0:UPPER_TRANS_GROUP), &
                   TOTAL_ANNUAL_CAPACITY_ADDED(0:UPPER_TRANS_GROUP), &
                  TG_TOTAL_ANNUAL_CAPACITY_ADDED(0:UPPER_TRANS_GROUP), &
                   MAXIMUM_ANNUAL_MRX_CAP(0:UPPER_TRANS_GROUP), &
                   ICAP_REVENUE_MULT(0:UPPER_TRANS_GROUP), &
                   MINIMUM_ANNUAL_MRX_CAP(0:UPPER_TRANS_GROUP), &
                   MINIMUM_TG_MRX_CAP(0:UPPER_TRANS_GROUP), &
                   MAXIMUM_TG_MRX_CAP(0:UPPER_TRANS_GROUP), &
                   CO2_EMISSIONS_CAP(0:UPPER_TRANS_GROUP), &
                   CO2_EMISS_REDUCTION_REQUIRED(0:UPPER_TRANS_GROUP), &
                   CO2_EMISSIONS(0:UPPER_TRANS_GROUP))
         
         TG_TOTAL_ANNUAL_CAPACITY_ADDED = 0.0
         IF(RUN_PRICE_MODE) then
            BUILD_TO_MARKET = .FALSE.
         call write_trace_bool1(file_trace_mga, "BTM", BUILD_TO_MARKET)
         end if
         MULTI_AREA_NAME(0) = 'System'
         DO L = 1, UPPER_TRANS_GROUP
            MULTI_AREA_NAME(L) = GET_GROUP_NAME(L)

! 060309. MOVED UP. GET THE INITIAL MARKET DURATION CURVE.
!         LOAD ALL PRICES AHEAD OF TIME.

            IF(BUILD_TO_MARKET) THEN
               TRANS_GROUP_4_PRICING = L
               CALL MX_USER_PRICE_CURVE(      L, &
                                              HIGHEST_INTERVAL, &
                                              MARKET_DURATION(0,L), &
                                              MARKET_CUM_REVENUE(0,L), &
                                              MARKET_PRICE(0,L), &
                                              13_2)
            ELSE
               TRANS_GROUP_4_PRICING = 0
               CALL MX_PRICE_CURVE( &
                                              HIGHEST_INTERVAL, &
                                              MARKET_DURATION(0,L), &
                                              MARKET_CUM_REVENUE(0,L), &
                                              MARKET_PRICE(0,L), &
                                              13_2)
            ENDIF
!
            MRX_ICAP_UNIT_LINK(L) = GET_MRX_ICAP_UNIT_LINK(L)
            call write_trace_int2(file_trace_mga, "Link unit",  &
                MRX_ICAP_UNIT_LINK(L))
!
         ENDDO
         HO = 0
         LAST_HO = -1
!
! 111507. CHANGED FOR TVA.
!
         HGST_NET_MARG_ADDED = MINIMUM_NET_MARGIN - 0.001 ! 0.
         call write_trace_real4(file_trace_mga, "21. HGMA - init", &
            HGST_NET_MARG_ADDED(1))
         SORTED_OPTIONS = 0
         HGST_OPT_INDEX = 0
!
! SYSTEM CO2
!
         CO2_EMISSIONS_CAP = 999999999999.0
         CO2_EMISS_REDUCTION_REQUIRED = 0.0
         CO2_EMISSIONS = 0.0
         RETRO_FITTED_CAP = 0.
         TG = 0

         CO2_EMISSIONS_CAP(TG) = GET_EMISS_CAP_FOR_CLASS(CO2_PLN,TG)
         CO2_EMISSIONS(TG) = GET_CL_EMISS_FOR_CLASS(CO2_PLN,TG)
         CALL GRX_CO2_REDUCTION_CURVE(TG) 
         ! STARTS CO2 REDUCTION CURVE ROUTINES
         IF(CO2_RETIREMENTS_LOGIC .AND. &
                              (GRX_ITERATIONS  > 2 .OR. &
                                           BALANCE_WITH_CO2_PRICE)) THEN
            CURRENT_CO2_DISPATCH_COST = NEXT_CURRENT_CO2_DISPATCH_COST 

         ENDIF
         CURRENT_CO2_DISPATCH_COST = NEXT_CURRENT_CO2_DISPATCH_COST 
         !GRX_CO2_PRICE_ADJ(0_2)
         CO2_EMISS_REDUCTION_REQUIRED(TG) = &
                               CO2_EMISSIONS(TG) - CO2_EMISSIONS_CAP(TG)
         IF(GRX_ITERATIONS <= 1) THEN
            GRX_CO2_EMISS_REDUCT_REQUIRED(TG) = &
                                        CO2_EMISS_REDUCTION_REQUIRED(TG)
            WRITE(9,*) "Saved CO2 req", &
                        GRX_CO2_EMISS_REDUCT_REQUIRED(TG)
               CALL flush_unit(INT(9,2))
               CALL flush_unit(INT(4,2))
         ELSE
            CO2_EMISS_REDUCTION_REQUIRED(TG) = &
                                    GRX_CO2_EMISS_REDUCT_REQUIRED(TG)
         ENDIF

         WRITE(9,*) "TOP OF GRX YEAR ITERATION",THIS_YEAR,gc_end_point, &
                    "ITER",GRX_ITERATIONS, &
                    "CO2_EMISSIONS_CAP", &
                            GRX_ALL_SECTOR_CO2_EMISS_CAP(THIS_YEAR), &
                    "CO2_EMISSIONS    ",CO2_EMISSIONS(TG), &
                    "CO2_EMISS_REDUCTION_REQUIRED", &
                     CO2_EMISS_REDUCTION_REQUIRED(TG), &
                    "CURRENT PRICE",CURRENT_CO2_DISPATCH_COST

         CALL flush_unit(INT(9,2))
         CALL flush_unit(INT(4,2))
         CO2_COEFF = 6E-28
         CO2_POWER = 13.618

         RESOURCE_ADDITION = .TRUE.

! CODE FOR RESERVE MARGIN BY TG

         CURRENT_TARGET_RATIO = TARGET_RATIO(3,R_YEAR) * &
                                                 SCENARIO_RESERVE_MARGIN
            call write_trace_int2(file_trace_mga, "Iterating on PG", PG)
            DO PA = 0, PG
               PLANNING_RESERVE_MARGIN(PA) = ZERO
               PEAK_AFTER_INTERRUPTIBLE(PA) = ZERO
               PLANNING_CAPACITY(PA) = ZERO
               TG_2_PLANNING_AREA(PA) = 0
               TOTAL_ANNUAL_CAPACITY_ADDED(PA) = 0.
               MAXIMUM_ANNUAL_MRX_CAP(PA) = 999999.
               ICAP_REVENUE_MULT(PA) = 0.
               MINIMUM_ANNUAL_MRX_CAP(PA) = 0.

! 9/11/02. FOR RDI.
! CURRENT_TARGET_RATIO
! STILL NEED TO PUSH THE VALUES BELOW FOR ALL MINIMUM AND MAXIMUM RM'S.

               IF(REGIONAL_PARAMS_EXIST) THEN
                  CURRENT_TARGET_RATIO_PA(PA) = &
                           ICAP_MIN_TESTING_MARGIN( &
                                     R_YEAR,CURRENT_TARGET_RATIO,PA) * &
                                                 SCENARIO_RESERVE_MARGIN

! NEED TO CHANGE THE ORDER FOR MAX
                  
                  MAXIMUM_RM_PA(PA) = &
                           ICAP_MAX_TESTING_MARGIN( &
                           R_YEAR,MAXIMUM_RM,pa) * &
                           SCENARIO_RESERVE_MARGIN
               ELSE
                  CURRENT_TARGET_RATIO_PA(PA) = CURRENT_TARGET_RATIO
                  MAXIMUM_RM_PA(PA) = MAXIMUM_RM * &
                                                 SCENARIO_RESERVE_MARGIN
               ENDIF
!
            ENDDO
!
            MAXIMUM_ANNUAL_MRX_CAP(0) = GET_MAXIMUM_ANNUAL_MRX_CAP()
!
            IF(USE_MINIMUM_RM_LOGIC .AND. PEAK > 0.00001) THEN
               MINIMUM_ANNUAL_MRX_CAP(0) = MAX(0., &
                        (CURRENT_TARGET_RATIO-ANN_CAP/PEAK)*PEAK)
            ELSE
               MINIMUM_ANNUAL_MRX_CAP(0) = 0.
            ENDIF

            DO TG = 1, UPPER_TRANS_GROUP

               PLANNING_RM_BEFORE_ADDITIONS = ZERO

               TG_2_PLANNING_AREA(TG) = GET_PA_FROM_TG(TG)
               PA = TG_2_PLANNING_AREA(TG)

               EXISTING_PLANNING_CAPACITY(0) = &
                           GET_CL_TG_CAP( &
                                    0_2,TG,R_YEAR,1_2) + &
                           GET_EL_TG_CAP(TG,R_YEAR,1_2) + &
                             DERIV_CAPACITY_PLANNING_BY_TG(TG,R_YEAR)

               NEW_PLANNING_CAPACITY(0) = &
                              GET_CL_TG_CAP( &
                                    0_2,TG,R_YEAR,2_2) + &
                            GET_EL_TG_CAP(TG,R_YEAR,2_2)
               PLANNING_CAPACITY(PA) = PLANNING_CAPACITY(PA) + &
                           EXISTING_PLANNING_CAPACITY(0) + &
                                                NEW_PLANNING_CAPACITY(0)
               IF(MRX_SORTED_MIX) THEN 

                  CAPACITY_PLANNING_PEAK = &
                                        GET_GROUP_PEAK_ON_PEAK_MONTH(TG)
               ELSEIF(USE_REGIONAL_MINIMUM_RM_LOGIC) THEN
                  CAPACITY_PLANNING_PEAK = &
                                        GET_PEAK_ON_PA_PEAK_MONTH(TG,PA)
               ELSE
                  CAPACITY_PLANNING_PEAK = &
                                        GET_GROUP_PEAK_ON_PEAK_MONTH(TG)
               ENDIF
!
               INTERRUPTIBLE_LOAD = GET_ANNUAL_INTER_CAPACITY(TG)
               PEAK_AFTER_INTERRUPTIBLE(PA) = &
                     PEAK_AFTER_INTERRUPTIBLE(PA) + &
                        MAX(0., &
                            CAPACITY_PLANNING_PEAK - INTERRUPTIBLE_LOAD)
!
! 090707. MIN AND MAX RM BY TG
!
               TG_PLANNING_CAPACITY = &
                           EXISTING_PLANNING_CAPACITY(0) + &
                                                NEW_PLANNING_CAPACITY(0)
               PEAK_AFTER_INTERRUPTIBLE_TG = &
                        MAX(0., &
                            CAPACITY_PLANNING_PEAK - INTERRUPTIBLE_LOAD)
               IF(PEAK_AFTER_INTERRUPTIBLE_TG > NEAR_ZERO)  THEN
                  TG_PLANNING_RESERVE_MARGIN = &
                        1. + (TG_PLANNING_CAPACITY - &
                                      PEAK_AFTER_INTERRUPTIBLE_TG) / &
                                             PEAK_AFTER_INTERRUPTIBLE_TG
                  MINIMUM_TG_MRX_CAP(TG) = &
                           MAX(0., &
                              (GET_MIN_CAP_TESTING_RATIO(TG)* &
                                             SCENARIO_RESERVE_MARGIN - &
                                     TG_PLANNING_RESERVE_MARGIN)* &
                                            PEAK_AFTER_INTERRUPTIBLE_TG)

                  MAXIMUM_TG_MRX_CAP(TG) = &
                           MIN(999999., &
                                (GET_MAX_CAP_TESTING_RATIO(TG)* &
                                             SCENARIO_RESERVE_MARGIN - &
                                     TG_PLANNING_RESERVE_MARGIN) * &
                                            PEAK_AFTER_INTERRUPTIBLE_TG)
               ENDIF
            ENDDO ! TRANSACTION GROUPS
!
         DO PA = 1, PG
               IF(PEAK_AFTER_INTERRUPTIBLE(PA) > NEAR_ZERO)  THEN
                  PLANNING_RESERVE_MARGIN(PA) = &
                        1. + (PLANNING_CAPACITY(PA) - &
                                    PEAK_AFTER_INTERRUPTIBLE(PA)) / &
                                         PEAK_AFTER_INTERRUPTIBLE(PA)
                  MINIMUM_ANNUAL_MRX_CAP(PA) = MAX(0., &
                        (CURRENT_TARGET_RATIO_PA(PA) - &
                           PLANNING_RESERVE_MARGIN(PA))* &
                                           PEAK_AFTER_INTERRUPTIBLE(PA))
!
                  IF(USE_MAXIMUM_RM_LOGIC) THEN
                     MAXIMUM_ANNUAL_MRX_CAP(PA) = &
                       MIN(MAXIMUM_ANNUAL_MRX_CAP(PA), &
                           MAX(0., &
                                MAXIMUM_RM_PA(PA) - &
                                      PLANNING_RESERVE_MARGIN(PA)) * &
                                          PEAK_AFTER_INTERRUPTIBLE(PA))
                  ENDIF
               ELSE
                  PLANNING_RESERVE_MARGIN(PA) = ZERO
               ENDIF
               
               if(R_YEAR==2 .and. pa==1) then
                    R_YEAR=R_YEAR ! Debugstop
               end if
               ICAP_REVENUE_MULT(PA) = GET_ICAP_REVENUE_MULT( &
                          R_YEAR,pa,PLANNING_RESERVE_MARGIN(PA))
         ENDDO
!
! 080309. DEVELOP RPS SUPPLY CURVES
!
         TEMP_L = INIT_ANNUAL_RPS_SUPPLY_CURVES( &
                                          tot_all_options_cap,   & 
                                          !  R_RESOURCE_NUM,
                                          STATE_PROVINCE,      & 
                                          !  R_RESOURCE_STATE,
                                          RESOURCE_TYPE_INDEX, & 
                                          !  R_RESOURCE_TYPE,
                                          RPS_OPTION, &
                                          RPS_RESOURCE_COUNT, &
                                          OPTION_NAME)

         MAX_RESOURCES_PER_GROUP = 0
         MAX_RESOURCES_PER_YEAR_GROUP = &
                                MX_RES_P_YEAR/UPPER_TRANS_GROUP
         HIGHEST_ENERGY = 0.
!
! 061309. BEGIN GRX ALGORITHM.
!
         CO2_CAP_NOT_MET = .TRUE.
         CO2_RESOURCES_AVAIL = .TRUE.
         RESOURCES_PROFITABLE = .TRUE.
         RESERVES_NOT_MET = .TRUE.
         ENRG_RESOURCES_AVAIL = .TRUE.
         RPS_CAPS_NOT_MET = .TRUE.
         RPS_EXPOURCES_AVAIL = .TRUE.

         UNITS_RETROFITTED = 0
         CO2_RETIREMENT_OPTIONS = 0
         CO2_RETROFIT_OPTIONS = 0
         ALL_RESOURCES = tot_all_options_cap + &
                           CO2_RETIREMENT_OPTIONS + &
                              CO2_RETROFIT_OPTIONS

         BASE_DATE = (R_YEAR + get_BASE_YEAR() - 1900) * 100 + 7
         call write_trace_int2(file_trace_mga, "BASE_DATE", BASE_DATE)
         
         TOTAL_ANNUAL_CAPACITY_ADDED = 0.
         TG_TOTAL_ANNUAL_CAPACITY_ADDED = 0.

         CO2_COUNTER = 1

         OPTIONS_ARE_AVAILABLE = .FALSE.
         RESOURCES_ADDED = CO2_RETIREMENTS_LOGIC .OR. &
                        SUM(ns_cap_trans%transaction_group(1:ALL_RESOURCES)) >0
         call write_trace_bool1(file_trace_mga, "1. RES Added", &
            RESOURCES_ADDED)
         
         DO WHILE(RESOURCES_ADDED)
             RESOURCES_ADDED = CO2_RETIREMENTS_LOGIC
         call write_trace_bool1(file_trace_mga, "2. RES Added", &
            RESOURCES_ADDED)

            ANNUAL_ENERGY = 0.
        call write_trace_real4(file_trace_mga, "16. AE-init", ANNUAL_ENERGY(1))
            HIGHEST_NET_MARGIN = -999999.
            HIGHEST_CF = 0.
            HIGHEST_ICAP_REVENUE = -999999.
            HIGHEST_ENERGY_REVENUE = -999999.
            HIGHEST_NET_MARGIN_PER_MWH = -999999.
            HIGHEST_NET_MARGIN_PER_KWYR = -999999.

            HGST_NET_MARG_ADDED = -999999.

            HIGHEST_OPTION = -1
            

            LOCAL_RESOURCES_AVAILABLE = .FALSE.

            MO = 7 ! FOR NOW.

! CO2 RETIREMENTS PRICE

            IF(CO2_RETIREMENTS_LOGIC) THEN
               TEMP_R4 = CO2_EMISS_REDUCTION_REQUIRED(SYS)

! 062709. REPLACES THE RETIREMENT PRICE ROUTINE.

               CO2_RETIRE_RETRO_PRICE = &
                                     GET_CO2_RETIRE_RETRO_PRICE(TEMP_R4)
               INIT_CO2_RETIRE_RETRO_PRICE = CO2_RETIRE_RETRO_PRICE
            ENDIF


            DO CN = 1, ALL_RESOURCES

               R = CN ! R = ALL RESOURCES (ADD RETIRE AND RETRO)

               L = ns_cap_trans%transaction_group(CN)

               IF(L <= 0) then
               	CYCLE 
               end if

               L = GET_TRANS_GROUP_POSITION(L)
! DOUBLE INDEX ON L (L=TG OF TG = 1, UPPER_TRANS_GROUP)
               PA = TG_2_PLANNING_AREA(L)

               I = OPTION_POSITION(CN)
               call write_trace_int2(file_trace_mga, "12. I", I)
               co_prod_pointer=GET_PRODUCTION_DATA_POINTER(I)



               IF( .NOT. RESOURCE_AVAILABLE(I, &
                                    THIS_YEAR_PLUS_LEAD_TIME(CN)) .OR. &
                      .NOT. TEST_START_STOP_YEARS(I, &
                                    THIS_YEAR_PLUS_LEAD_TIME(CN)) .OR. &
                                       LOCAL_ANNUAL_UNITS(CN) < 1 .OR. &
                               .NOT. DEPENDENT_UNIT_AVAILABLE(CN))then
                   CYCLE
               end if
!
               TRANS_MRX_LINK = .TRUE.   ! FALSE in Greg's code

               IF(TRANS_MRX_LINK) THEN
                  LINK_CN = MRX_ICAP_UNIT_LINK(L)
           call write_trace_int2(file_trace_mga, "LINK_CN", LINK_CN)
                  IF(LINK_CN > 0) THEN
                     LINK_I = OPTION_POSITION(LINK_CN)
           call write_trace_int2(file_trace_mga, "LINK_I", LINK_I)
                  ELSE
                     LINK_I = I
           call write_trace_int2(file_trace_mga, "2. LINK_I", LINK_I)
                  ENDIF
               ELSE
                  LINK_I = I
           call write_trace_int2(file_trace_mga, "3. LINK_I", LINK_I)
               ENDIF
               ANNUAL_ICAP_REVENUE = &
                     ICAP_REVENUE_MULT(PA) * &
                     ( 1000000. * &
                        RETURN_SCREEN_CAP_COST(LINK_I,R_YEAR) * &
                        SCENARIO_CAP_COST_MULT  + &
                                                 FIX_OM_COST(CN))
!
               IF(CO2_RETIREMENTS_LOGIC .AND. &
                           FILE_SOURCE_INDEX(CN) /= 3 .AND. &
                                      FILE_SOURCE_INDEX(CN) /= 4) THEN
                  N = 0
                  I = OPTION_POSITION(CN)
                  call write_trace_int2(file_trace_mga, "14. I", I)
                  co_prod_pointer=GET_PRODUCTION_DATA_POINTER(I)

                  FT = GET_S_PRIMARY_MOVER(co_prod_pointer)
                  call write_trace_int2(file_trace_mga, "FT", FT)
                  FUEL_SCEN_MULT(5) = 1.0
                  IF(FT > 4 .OR. FT < 1) FT = 5
                  R_DELIVERY_COST = -999999.
                  VOID_R4 = PUT_MRX_DELIVERY_COST(R_DELIVERY_COST)
                  DO MO= 1, 12
                     IF(FUEL_PRICE_DATA_AVAILABLE) THEN
                        FUEL_SCEN_MULT(1) = &
                                  GET_SCENARIO_COAL_PRICES(R_YEAR,MO)
                        FUEL_SCEN_MULT(2) = &
                                  GET_SCENARIO_GAS_PRICES(R_YEAR,MO)
                        FUEL_SCEN_MULT(3) = &
                                  GET_SCENARIO_OIL_PRICES(R_YEAR,MO)
                        FUEL_SCEN_MULT(4) = &
                              GET_SCENARIO_URANIUM_PRICES(R_YEAR,MO)
                        RTEMP = GET_MONTHLY_FUEL_INDEX(CN,MO)
                        FUEL_POINTERS_USED = .FALSE.
                        EXISTING_UNIT = .FALSE.


                        CALL GET_MRX_DELIVERY_COST(co_prod_pointer, &
                                          R_DELIVERY_COST, &
                                          MO,R_YEAR, &
                                          FUEL_POINTERS_USED, &
                                          EXISTING_UNIT)
                        IF(FUEL_POINTERS_USED) THEN
                           VOID_R4 = &
                                  PUT_MRX_DELIVERY_COST(R_DELIVERY_COST)
                        ENDIF
                     ENDIF ! FUEL PRICE DATA AVAILABLE
! 082309.
                     IF(GAS_MODEL_DATA_AVAILABLE .AND. &
                      FT == FT_GAS) THEN
                        GSP = MIN(MAX(INT(0,2), &
                        GET_SCRN_UNIT_GAS_REGION_INDEX(co_prod_pointer)), &
                                                      MAX_GAS_REGION_NO)
                        FUEL_COST_PER_MWH(CN,MO) = &
                                  GET_MONTHLY_GAS_PRICE_BY_STATE(GSP,MO)
                     ELSE
                        FUEL_COST_PER_MWH(CN,MO) = &
                              RETURN_CL_SCREEN_FUEL_COST( &
                                              co_prod_pointer, &
                                                  R_YEAR, &
                                                  HEAT_RATE, &
                                                FUEL_COST_PER_MMBTU) * &
                                   FUEL_SCEN_MULT(FT) * RTEMP
                     ENDIF
                     
                     
                     
                     
                     IF(YES_USE_EMIS_IN_MRX) THEN
                        CALL CALCULATE_NOX_SOX_VOM( &
                              co_prod_pointer, &
                              R_YEAR, &
                              MO, &
                              NOX_VOM, &
                              NOX_FOM, &
                              SOX_VOM, &
                              SOX_FOM, &
                              CO2_VOM, &
                              CO2_FOM, &
                              HG_VOM, &
                              HG_FOM, &
                              OTHER3_VOM, &
                              OTHER3_FOM, &
                              NOX_CONTROL_MULT, &
                              SOX_CONTROL_MULT, &
                              CO2_CONTROL_MULT, &
                              HG_CONTROL_MULT, &
                              OTHER3_CONTROL_MULT, &
                              ns_cap_trans%transaction_group(CN))
                        FUEL_COST_PER_MWH(CN,MO) = &
                              FUEL_COST_PER_MWH(CN,MO) + &
                                    RETURN_CL_SCREEN_EMIS_COST( &
                                    co_prod_pointer,R_YEAR,MO, &
                                            NOX_CONTROL_MULT, &
                                            SOX_CONTROL_MULT, &
                                            CO2_CONTROL_MULT, &
                                            HG_CONTROL_MULT, &
                                            OTHER3_CONTROL_MULT, &
                                            CO2_RETIRE_RETRO_PRICE) + &
                                            NOX_VOM + SOX_VOM + &
                                            CO2_VOM + HG_VOM + &
                                            OTHER3_VOM
                     ENDIF ! YES_USE_EMIS_IN_MRX
                     STRIKE_PRICE(CN,MO) = FUEL_COST_PER_MWH(CN,MO) + &
                                                      VAR_OM_PER_MWH(CN)
          call write_trace_real4(file_trace_mga, "Strike price", &
            STRIKE_PRICE(CN,MO))
            
                  ENDDO ! MONTH FOR FUEL CALC
                  MO = 7 ! FOR NOW.
!
               ENDIF ! CO2_RETIREMENTS_LOGIC
!
! IDENTIFY THE TYPE OF RESOURCE
! UPDATE ALL COST COMPONENTS OF THE RESOURCE
! IDENTIFY THE STRIKE PRICE OF THE RESOURCE
! GET THE CF OF THE RESOURCE
!
               DO J = HIGHEST_INTERVAL, 1, -1
                  IF(MARKET_PRICE(J,L) < STRIKE_PRICE(CN,MO)) then
                      EXIT
                  end if
               ENDDO
!
! DOES PRICE FALL WITHIN THE PRICE DURATION CURVE?
!
               IF(J < HIGHEST_INTERVAL) THEN
!
! 041209. FUEL PRICES MUST REFLECT CHANGING EMISSION PRICES.
!
                  IF(J == 0) THEN
                     TOP_CAP_PERCENT = 0.0
                     J = 1
                  ELSEIF(MARKET_PRICE(J+1,L)  - &
                                 MARKET_PRICE(J,L) /= 0.0) THEN
                     TOP_CAP_PERCENT = &
                           (STRIKE_PRICE(CN,MO) - MARKET_PRICE(J,L)) / &
                               (MARKET_PRICE(J+1,L) - MARKET_PRICE(J,L))
                  ELSE
                     TOP_CAP_PERCENT = 0.0
                  ENDIF
                  MARKET_HOURS = &
                        (TOP_CAP_PERCENT * MARKET_DURATION(J+1,L) + &
                        (1.-TOP_CAP_PERCENT) * MARKET_DURATION(J,L))

! ASSUMES 8784 HOURS PER YEAR.

                  IF(RPS_HOURS(CN) > 0.01) THEN
                     IF(MARKET_HOURS > 0.0) THEN
                        RPS_MULT(CN) = RPS_HOURS(CN) / MARKET_HOURS
                     ELSE
                        RPS_MULT(CN) = 0.0
                     ENDIF
                  ELSE
                     RPS_MULT(CN) = 1.0
                  ENDIF
                  CF = MARKET_HOURS/87.84
                  ANNUAL_ENERGY(CN) = MARKET_HOURS * RPS_MULT(CN) * &
                                                 EQUIVALENT_CAPACITY(CN)

! CO2 EMISSIONS

                  IF(FILE_SOURCE_INDEX(CN) == 1) THEN
                     EXPANSION_CO2(CN) = &
                           ANNUAL_ENERGY(CN) * &
                               GET_CL_CO2_TON_PER_MWH(R_YEAR, &
                                                 co_prod_pointer, &
                                                      BASE_DATE)
                  ENDIF
! ENERGY REVENUE
                  ANNUAL_ENERGY_REVENUE = RPS_MULT(CN) * &
                        (TOP_CAP_PERCENT * MARKET_CUM_REVENUE(J+1,L) + &
                     (1.-TOP_CAP_PERCENT) * MARKET_CUM_REVENUE(J,L)) * &
                                               EQUIVALENT_CAPACITY(CN)
                                               
        call write_trace_real4(file_trace_mga, &
            "6. ANNUAL_ENERGY_REVENUE", &
            ANNUAL_ENERGY_REVENUE)
            
                  IF(INCLUDE_ICAP_REVENUE) THEN
                     MARKET_REVENUE(CN) = ANNUAL_ENERGY_REVENUE + &
                                                     ANNUAL_ICAP_REVENUE
                  ELSE
                     MARKET_REVENUE(CN) = ANNUAL_ENERGY_REVENUE
                  ENDIF
                  ANNUAL_VARIABLE_COST(CN) = STRIKE_PRICE(CN,MO) * &
                                                       ANNUAL_ENERGY(CN)
                  MARKET_COST(CN) = ANNUAL_FIXED_COST(CN) + &
                                               ANNUAL_VARIABLE_COST(CN)
                  ANNUAL_FUEL_COST(CN) = &
                             FUEL_COST_PER_MWH(CN,MO) * &
                                                       ANNUAL_ENERGY(CN)
                  MARKET_AVE_REVENUE = MARKET_REVENUE(CN) / &
                                              MAX(.01,ANNUAL_ENERGY(CN))
       call write_trace_real4(file_trace_mga, "15. MAR", &
            MARKET_AVE_REVENUE)
                  TOTAL_AVERAGE_COST = MARKET_COST(CN) / &
                                              MAX(.01,ANNUAL_ENERGY(CN))
       call write_trace_real4(file_trace_mga, "16. TAC", &
            TOTAL_AVERAGE_COST)
                  TEMP_NET_MARGIN = &
                                 MARKET_AVE_REVENUE - TOTAL_AVERAGE_COST
                  IF(EQUIVALENT_CAPACITY(CN) > 0.) THEN
                     TEMP_NET_MARGIN_PER_KWYR = &
                         .001*(MARKET_REVENUE(CN) - MARKET_COST(CN)) / &
                                                 EQUIVALENT_CAPACITY(CN)
                  ELSE
                     TEMP_NET_MARGIN_PER_KWYR = -999999.0
                  ENDIF
               ELSE ! RESOURCE DOES NOT FALL WITHIN THE PRICE 
                    ! DURATION CURVE
                  ANNUAL_ENERGY_REVENUE = 0.
               call write_trace_message(file_trace_mga, "AER zeroed.")
                  IF(INCLUDE_ICAP_REVENUE) THEN
                     MARKET_REVENUE(CN) = ANNUAL_ICAP_REVENUE
                  ELSE
                     MARKET_REVENUE(CN) = 0.
                  ENDIF
                  MARKET_COST(CN) = ANNUAL_FIXED_COST(CN)
                  MARKET_AVE_REVENUE = 0.0
       call write_trace_real4(file_trace_mga, "17. MAR", &
            MARKET_AVE_REVENUE)
                  CF = 0.0
                  ANNUAL_ENERGY(CN) = 0.0
                  ANNUAL_FUEL_COST(CN) = 0.0
                  ANNUAL_VARIABLE_COST(CN) = 0.0
                  TOTAL_AVERAGE_COST = ANNUAL_FIXED_COST(CN)
       call write_trace_real4(file_trace_mga, "18. TAC", &
            TOTAL_AVERAGE_COST)

! SEEMS TO ALLOW TOO MANY RESOURCES IF = 0.0
! TODO: Understand why ^^ and fix if appropriate.

                  TEMP_NET_MARGIN = -999999.0
                  IF(EQUIVALENT_CAPACITY(CN) > 0.) THEN
                     TEMP_NET_MARGIN_PER_KWYR = &
                         .001*(MARKET_REVENUE(CN) - MARKET_COST(CN)) / &
                                                 EQUIVALENT_CAPACITY(CN)
                  ELSE
                     TEMP_NET_MARGIN_PER_KWYR = -999999.0
                  ENDIF
               ENDIF

               IF(TEMP_NET_MARGIN > NET_MARGIN(CN)) THEN
                  NET_MARGIN(CN) =  TEMP_NET_MARGIN
       call write_trace_real4(file_trace_mga, "5. NM",  &
        NET_MARGIN(CN))
                  NET_MARGIN_INTERVAL(CN) = J
               ENDIF
               IF(TEMP_NET_MARGIN_PER_KWYR > &
                                           NET_MARGIN_PER_KWYR(CN)) THEN
                  NET_MARGIN_PER_KWYR(CN) =  TEMP_NET_MARGIN_PER_KWYR
               ENDIF

               IF( (PLANNING_PER_MWH .AND. &
                      NET_MARGIN(CN) + .01 > &
                                       HGST_NET_MARG_ADDED(L)) .OR. &
                   (.NOT. PLANNING_PER_MWH .AND. &
                              NET_MARGIN_PER_KWYR(CN) + .01 > &
                                     HGST_NET_MARG_ADDED(L)) ) THEN

                  IF(PLANNING_PER_MWH) THEN
                     HGST_NET_MARG_ADDED(L) = NET_MARGIN(CN)
         call write_trace_real4(file_trace_mga, "21. HGMA", &
            HGST_NET_MARG_ADDED(L))
                  ELSE
                     HGST_NET_MARG_ADDED(L) = &
                                                 NET_MARGIN_PER_KWYR(CN)
         call write_trace_real4(file_trace_mga, "22. HGMA", &
            HGST_NET_MARG_ADDED(L))
                  ENDIF
                  HIGHEST_OPTION = CN
                  HIGHEST_J = J
                  HIGHEST_TOP_CAP_PERCENT = TOP_CAP_PERCENT
                  HIGHEST_ENERGY(CN) = ANNUAL_ENERGY(CN)
                  HIGHEST_CF = CF * RPS_MULT(CN)
                  HIGHEST_ICAP_REVENUE = ANNUAL_ICAP_REVENUE
                  HIGHEST_ENERGY_REVENUE = ANNUAL_ENERGY_REVENUE
                  HIGHEST_NET_MARGIN_PER_MWH = TEMP_NET_MARGIN
                  HIGHEST_NET_MARGIN_PER_KWYR = &
                                                TEMP_NET_MARGIN_PER_KWYR

                  HGST_OPT_INDEX(L) = HIGHEST_OPTION

                  SORTED_OPTIONS(L) = L
                  IF(L > 0) OPTIONS_ARE_AVAILABLE = .TRUE.
               ENDIF ! PLANNING CRITERIA

! ESTIMATE THE TOTAL COST OF THE RESOURCE
! ESTIMATE THE COST PER MWH AND KW OF THE RESOURCE
! COMPARE THE RESULTS OF THE RESOURCE TO ALL OTHER RESOURCES
! SAVE THE NECESSARY PARAMETERS OF THE CURRENT RESOURCE

            ENDDO ! RESOURCES LOOP
! RPS
            TEMP_L = BUILD_RPS_SUPPLY_CURVES(RPS_ENERGY, &
                                            NET_MARGIN)
! RETIREMENT AND RETROFIT
            IF(.NOT. CO2_END_LIST .AND. &
                     CO2_RETIREMENTS_LOGIC) THEN

               TEMP_L = GET_NEXT_MRX_RETIRE_RETRO( &
                                          CO2_COUNTER, &
                                        CO2_RETIRE_OR_RETRO_OR_MARKET, &
                                          CO2_NUNIT, &
                                          CO2_TG, &
                                          CO2_UNIT_MW, &
                                          CO2_UNIT_MWH, &
                                          UNIT_CO2, &
                                          CO2_UNIT_PRICE, &
                                          CO2_STRIKE_PRICE, &
                                          CO2_UNIT_MW_AFTER, &
                                          CO2_STRIKE_PRICE_AFTER, &
                                          UNIT_CO2_AFTER, &
                                          CO2_END_LIST)
               IF(CO2_UNIT_PRICE > CURRENT_CO2_DISPATCH_COST) THEN
                  RESOURCES_ADDED = .FALSE.
                  call write_trace_message(file_trace_mga, "6. CYCLE")
                  CYCLE  ! MSG 09
               ENDIF
               PA = TG_2_PLANNING_AREA(CO2_TG)
               CO2_NET_MARGIN = CO2_RETIRE_RETRO_PRICE - CO2_UNIT_PRICE
               IF(PLANNING_PER_MWH .AND. CO2_UNIT_MWH > 0.0) THEN
                  CO2_NET_MARGIN = CO2_NET_MARGIN/CO2_UNIT_MWH
               ELSEIF(.NOT. PLANNING_PER_MWH .AND. &
                                                 CO2_UNIT_MW > 0.0) THEN
                  CO2_NET_MARGIN = CO2_NET_MARGIN/(CO2_UNIT_MW*1000.0)
               ELSE
                  CO2_NET_MARGIN = -999999.0
               ENDIF
!
! 070209. TEMP TO CREATE RETIREMENT SCHEDULE
!
               RESOURCES_ADDED = .TRUE.
               IF(CO2_RETIRE_OR_RETRO_OR_MARKET == 1) THEN ! RETIRE
                  TEMP_L = RETIRE_CO2_THERMAL_UNIT( &
                                          CO2_NUNIT, &
                                          CO2_UNIT_MW, &
                                          UNIT_CO2, &
                                          CO2_UNIT_PRICE)
               ELSEIF(CO2_RETIRE_OR_RETRO_OR_MARKET == 2) THEN
                  IF(RETRO_FITTED_CAP > 15000.) then
              call write_trace_message(file_trace_mga, "7. CYCLE")
                    CYCLE
                  end if
                  TEMP_L = RETROFIT_CO2_THERMAL_UNIT( &
                                          CO2_NUNIT, &
                                          CO2_UNIT_MW, &
                                          UNIT_CO2, &
                                          CO2_UNIT_PRICE, &
                                          CO2_RETIRE_OR_RETRO_OR_MARKET)

                  RETRO_FITTED_CAP = RETRO_FITTED_CAP + &
                  (CO2_UNIT_MW - CO2_UNIT_MW_AFTER)
                  call write_trace_message(file_trace_mga, "8. CYCLE")
                  cycle ! to remove market from abatement using units
               ENDIF
               CO2_EMISS_REDUCTION_REQUIRED(0) = &
                        CO2_EMISS_REDUCTION_REQUIRED(0) - UNIT_CO2 + &
                                                          UNIT_CO2_AFTER
               IF(PA > 0) THEN
                  TOTAL_ANNUAL_CAPACITY_ADDED(PA) = &
                           TOTAL_ANNUAL_CAPACITY_ADDED(PA) - &
                                        CO2_UNIT_MW + CO2_UNIT_MW_AFTER
                  TG_TOTAL_ANNUAL_CAPACITY_ADDED(CO2_TG) = &
                           TG_TOTAL_ANNUAL_CAPACITY_ADDED(CO2_TG) - & 
                                        CO2_UNIT_MW + CO2_UNIT_MW_AFTER
               ENDIF
!
               TOTAL_ANNUAL_CAPACITY_ADDED(0) = &
                           TOTAL_ANNUAL_CAPACITY_ADDED(0) - &
                                        CO2_UNIT_MW + CO2_UNIT_MW_AFTER
!
! 070209. MAKE A LIST OF ELEMENTS THAT I NEED TO ADD:
!         STRIKE_PRICE
!
               DO J = HIGHEST_INTERVAL, 1, -1

                  IF(MARKET_PRICE(J,CO2_TG) < CO2_STRIKE_PRICE) then
                    EXIT
                  end if

               ENDDO
               IF(J < HIGHEST_INTERVAL) THEN


! 041209. FUEL PRICES MUST REFLECT CHANGING EMISSION PRICES.


                  IF(J == 0) THEN
                     TOP_CAP_PERCENT = 0.0
                     J = 1
                  ELSEIF(MARKET_PRICE(J+1,CO2_TG)  - &
                                 MARKET_PRICE(J,CO2_TG) /= 0.0) THEN
                     TOP_CAP_PERCENT = &
                        (CO2_STRIKE_PRICE - MARKET_PRICE(J,CO2_TG)) / &
                           (MARKET_PRICE(J+1,CO2_TG) - &
                                                 MARKET_PRICE(J,CO2_TG))
                  ELSE
                     TOP_CAP_PERCENT = 0.0
                  ENDIF
                  MARKET_HOURS = &
                        (TOP_CAP_PERCENT * MARKET_DURATION(J+1,CO2_TG) &
                        +(1.-TOP_CAP_PERCENT)*MARKET_DURATION(J,CO2_TG))
                        ! AUG 09

                  CF = MARKET_HOURS/87.84 ! ASSUMES 8784 HOURS PER YEAR
                  RESOURCE_ADDITION = .FALSE.
                  CALL ALTER_MX_PRICE_CURVE(HIGHEST_INTERVAL, &
                                           MARKET_DURATION(0,CO2_TG), &
                                         MARKET_CUM_REVENUE(0,CO2_TG), &
                                           MARKET_PRICE(0,CO2_TG), &
                                           CO2_UNIT_MW, &
                                           CO2_UNIT_MWH, &
                                           CO2_STRIKE_PRICE, &
                                           J, &
                                           TOP_CAP_PERCENT, &
                                           CO2_MARKET_REVENUE, &
                                           TRANS_GROUP_4_PRICING, &
                                           RESOURCE_ADDITION)
               ENDIF
               ! TODO: Remove code in IF block
               IF(CO2_RETIRE_OR_RETRO_OR_MARKET == 2 .AND. 1 == 2) THEN
                  DO J = HIGHEST_INTERVAL, 1, -1

                     IF(MARKET_PRICE(J,CO2_TG) < &
               CO2_STRIKE_PRICE_AFTER) then 
                                            EXIT
                                        end if

                  ENDDO
                  IF(J < HIGHEST_INTERVAL) THEN


! 041209. FUEL PRICES MUST REFLECT CHANGING EMISSION PRICES.


                     IF(J == 0) THEN
                        TOP_CAP_PERCENT = 0.0
                        J = 1
                     ELSEIF(MARKET_PRICE(J+1,CO2_TG)  - &
                                 MARKET_PRICE(J,CO2_TG) /= 0.0) THEN
                        TOP_CAP_PERCENT = &
                           (CO2_STRIKE_PRICE_AFTER - &
                                             MARKET_PRICE(J,CO2_TG)) / &
                           (MARKET_PRICE(J+1,CO2_TG) - &
                                                 MARKET_PRICE(J,CO2_TG))
                     ELSE
                        TOP_CAP_PERCENT = 0.0
                     ENDIF
                     MARKET_HOURS = &
                        (TOP_CAP_PERCENT * MARKET_DURATION(J+1,L) + &
                        (1.-TOP_CAP_PERCENT) * MARKET_DURATION(J,L))
                     CF = MARKET_HOURS/87.84 
                     ! ASSUMES 8784 HOURS PER YEAR
                     RESOURCE_ADDITION = .TRUE.
                     CALL ALTER_MX_PRICE_CURVE(HIGHEST_INTERVAL, &
                                           MARKET_DURATION(0,CO2_TG), &
                                         MARKET_CUM_REVENUE(0,CO2_TG), &
                                           MARKET_PRICE(0,CO2_TG), &
                                           CO2_UNIT_MW_AFTER, &
                                           CO2_UNIT_MWH, &
                                           CO2_STRIKE_PRICE_AFTER, &
                                           J, &
                                           TOP_CAP_PERCENT, &
                                           CO2_MARKET_REVENUE, &
                                           TRANS_GROUP_4_PRICING, &
                                           RESOURCE_ADDITION)
                  ENDIF
               ENDIF ! RETROFIT ONLY
            ENDIF ! RETIREMENT OR RETROFIT

! HIGHEST_OPTION IS POSITION WITHIN THE ALL_RESOURCES LIST OF THE BEST
! OPTION

! SORT ADDITIONS HIGHEST TO LOWEST

            IF(OPTIONS_ARE_AVAILABLE) THEN
        call write_sic_trace("cap_objt:0076", UPPER_TRANS_GROUP)
               CALL SortIncrPos( UPPER_TRANS_GROUP, & 
                                 SORTED_OPTIONS, &
                                 HGST_NET_MARG_ADDED)
         call write_trace_real4s(file_trace_mga, "23. HGMA", &
            HGST_NET_MARG_ADDED)
               HO = UPPER_TRANS_GROUP
            ELSE
               HO = 0
            ENDIF

! DETERMINE WHETHER TO ADD THE RESOURCE
! IDENTIFY PROFITABILITY
! IDENTIFY RESERVE MARGIN NEED
! IDENFITY CO2 NEED (RETIREMENT/RETROFIT)
! IDENTIFY RPS NEED
! IF ADD RESOURCE, THEN HAVE IT IMPACT THE PROPER ROUTINE

! HO IS THE MOST PROFITABLE UNIT.


            DO I = HO, 1, -1

! MAXIMUM RESERVE MARGIN CONDITION
!
               HIGHEST_OPTION = HGST_OPT_INDEX(SORTED_OPTIONS(I))
!
               IF(.NOT. DEPENDENT_UNIT_AVAILABLE(HIGHEST_OPTION)) then
                CYCLE
               end if
!
               TG = GET_TRANS_GROUP_POSITION( &
                 ns_cap_trans%transaction_group(HIGHEST_OPTION))

               PA = TG_2_PLANNING_AREA(TG)
! PROFITABILITY TEST
               IF(HGST_NET_MARG_ADDED(HIGHEST_OPTION) + .01 > &
                                                MINIMUM_NET_MARGIN) THEN
                  UNIT_PROFITABLE = .TRUE.
               ELSE
                  UNIT_PROFITABLE = .FALSE.
               ENDIF

! RESERVE TESTS

               IF(USE_REGIONAL_MINIMUM_RM_LOGIC) THEN
! MAXIMUM RESERVES
                  IF(TOTAL_ANNUAL_CAPACITY_ADDED(0) + &
                                SCREEN_CAPACITY(HIGHEST_OPTION) > &
                                       MAXIMUM_ANNUAL_MRX_CAP(0) .OR. &
                        TOTAL_ANNUAL_CAPACITY_ADDED(PA) + &
                                SCREEN_CAPACITY(HIGHEST_OPTION) > &
                                        MAXIMUM_ANNUAL_MRX_CAP(PA)) THEN
                     VIOLATES_MAXIMUM_RESERVE = .TRUE.
                  ELSE
                     VIOLATES_MAXIMUM_RESERVE = .FALSE.
                  ENDIF
! MINIMUM RESERVES
                  IF(TOTAL_ANNUAL_CAPACITY_ADDED(PA)  >= &
                                      MINIMUM_ANNUAL_MRX_CAP(PA) .AND. &
                     TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG)  >= & !  090709.
                                           MINIMUM_TG_MRX_CAP(TG)) THEN
!
                     NEED_FOR_MINIMUM_RESERVE = .FALSE.
                  ELSE
                     NEED_FOR_MINIMUM_RESERVE = .TRUE.
                  ENDIF
               ELSE ! IMPLICITLY USE_MINIMUM_RM_LOGIC
! MAXIMUM RESERVES
                  IF(TOTAL_ANNUAL_CAPACITY_ADDED(0) + &
                           SCREEN_CAPACITY(HIGHEST_OPTION) > &
                                  MAX(MAXIMUM_ANNUAL_MRX_CAP(0), &
                                       MAXIMUM_ANNUAL_MRX_CAP(PA))) THEN
                     VIOLATES_MAXIMUM_RESERVE = .TRUE.
                  ELSE
                     VIOLATES_MAXIMUM_RESERVE = .FALSE.
                  ENDIF
! MINIMUM RESERVES
                  IF(TOTAL_ANNUAL_CAPACITY_ADDED(0)  >= &
                                         MINIMUM_ANNUAL_MRX_CAP(0)) THEN
                     NEED_FOR_MINIMUM_RESERVE = .FALSE.
                  ELSE
                     NEED_FOR_MINIMUM_RESERVE = .TRUE.
                  ENDIF
               ENDIF

! MAXIMUM TG CONSTRAINT

               IF(TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG) + &
                      SCREEN_CAPACITY(HIGHEST_OPTION) > &
                                       MAXIMUM_TG_MRX_CAP(TG)) THEN
                  VIOLATES_MAXIMUM_TG_RESERVE = .TRUE.
               ELSE
                  VIOLATES_MAXIMUM_TG_RESERVE = .FALSE.
               ENDIF

! TEST WHETHER TO ADD INCREMENTAL RESOURCE

               IF(UNIT_PROFITABLE .OR. &
                     (.NOT. VIOLATES_MAXIMUM_TG_RESERVE .AND. &
                           .NOT. VIOLATES_MAXIMUM_RESERVE  .AND. &
                                        NEED_FOR_MINIMUM_RESERVE) ) THEN
                  IF(FILE_SOURCE_INDEX(HIGHEST_OPTION) == &
                                                        THERM_EXP) THEN
                     FIRST_YEAR_CAPACITY = &
                        ADD_THIS_UNIT( &
                           OPTION_POSITION(HIGHEST_OPTION),THIS_YEAR + &
                                        LOCAL_LEAD_TIME(HIGHEST_OPTION))
      call write_trace_real(file_trace_mga, "1. FIRST_YEAR_CAPACITY", &
      FIRST_YEAR_CAPACITY)
                     RESOURCE_ADDITION = .TRUE.
                     CALL ALTER_MX_PRICE_CURVE(  HIGHEST_INTERVAL, &
                                           MARKET_DURATION(0,TG), &
                                           MARKET_CUM_REVENUE(0,TG), &
                                           MARKET_PRICE(0,TG), &
                                           EQUIVALENT_CAPACITY( &
                                                      HIGHEST_OPTION), &
                                           HIGHEST_ENERGY( &
                                                      HIGHEST_OPTION), &
                                           STRIKE_PRICE( &
                                                   HIGHEST_OPTION,MO), &
                                           HIGHEST_J, &
                                           HIGHEST_TOP_CAP_PERCENT, &
                                           MARKET_REVENUE( &
                                                      HIGHEST_OPTION), &
                                           TRANS_GROUP_4_PRICING, &
                                           RESOURCE_ADDITION)
                  ELSEIF(FILE_SOURCE_INDEX(HIGHEST_OPTION) == &
                                                         DERIV_EXP) THEN
                     FIRST_YEAR_CAPACITY = &
                        ADD_THIS_UNIT( &
                           OPTION_POSITION(HIGHEST_OPTION),THIS_YEAR + &
                                        LOCAL_LEAD_TIME(HIGHEST_OPTION))
       call write_trace_real(file_trace_mga, "2. FIRST_YEAR_CAPACITY",  &
      FIRST_YEAR_CAPACITY)
                     RESOURCE_ADDITION = .TRUE.
                     TEMP_R4 = EQUIVALENT_CAPACITY(HIGHEST_OPTION) * &
                                                RPS_MULT(HIGHEST_OPTION)
                     CALL ALTER_MX_PRICE_CURVE(  HIGHEST_INTERVAL, &
                                           MARKET_DURATION(0,TG), &
                                           MARKET_CUM_REVENUE(0,TG), &
                                           MARKET_PRICE(0,TG), &
                                           TEMP_R4, &
                                           HIGHEST_ENERGY( &
                                                      HIGHEST_OPTION), &
                                           STRIKE_PRICE( &
                                                   HIGHEST_OPTION,MO), &
                                           HIGHEST_J, &
                                           HIGHEST_TOP_CAP_PERCENT, &
                                           MARKET_REVENUE( &
                                                      HIGHEST_OPTION), &
                                           TRANS_GROUP_4_PRICING, &
                                           RESOURCE_ADDITION)
                  ELSEIF(FILE_SOURCE_INDEX(HIGHEST_OPTION) == &
                                                        RETIRE_RES) THEN
                     RESOURCE_ADDITION = .FALSE.
                     TEMP_L = RETIRE_CO2_THERMAL_UNIT( &
                                          CO2_NUNIT, &
                                          CO2_UNIT_MW, &
                                          UNIT_CO2, &
                                          CO2_UNIT_PRICE)
                     CALL ALTER_MX_PRICE_CURVE(  HIGHEST_INTERVAL, &
                                           MARKET_DURATION(0,TG), &
                                           MARKET_CUM_REVENUE(0,TG), &
                                           MARKET_PRICE(0,TG), &
                                           EQUIVALENT_CAPACITY( &
                                                      HIGHEST_OPTION), &
                                           HIGHEST_ENERGY( &
                                                      HIGHEST_OPTION), &
                                           STRIKE_PRICE( &
                                                   HIGHEST_OPTION,MO), &
                                           HIGHEST_J, &
                                           HIGHEST_TOP_CAP_PERCENT, &
                                           MARKET_REVENUE( &
                                                      HIGHEST_OPTION), &
                                           TRANS_GROUP_4_PRICING, &
                                           RESOURCE_ADDITION)
                  ELSEIF(FILE_SOURCE_INDEX(HIGHEST_OPTION) == &
                                                         RETRO_RES) THEN
! FIRST RETIRE THE ORIGINAL CHARACTERISTICS
                     RESOURCE_ADDITION = .FALSE.
                     CALL ALTER_MX_PRICE_CURVE(  HIGHEST_INTERVAL, &
                                           MARKET_DURATION(0,TG), &
                                           MARKET_CUM_REVENUE(0,TG), &
                                           MARKET_PRICE(0,TG), &
                                           EQUIVALENT_CAPACITY( &
                                                      HIGHEST_OPTION), &
                                           HIGHEST_ENERGY( &
                                                      HIGHEST_OPTION), &
                                           STRIKE_PRICE( &
                                                   HIGHEST_OPTION,MO), &
                                           HIGHEST_J, &
                                           HIGHEST_TOP_CAP_PERCENT, &
                                           MARKET_REVENUE( &
                                                      HIGHEST_OPTION), &
                                           TRANS_GROUP_4_PRICING, &
                                           RESOURCE_ADDITION)
! THEN ADD THE NEW CHARACTERISTICS
                     RESOURCE_ADDITION = .TRUE.
                     CALL ALTER_MX_PRICE_CURVE(  HIGHEST_INTERVAL, &
                                           MARKET_DURATION(0,TG), &
                                           MARKET_CUM_REVENUE(0,TG), &
                                           MARKET_PRICE(0,TG), &
                                           EQUIVALENT_CAPACITY( &
                                                      HIGHEST_OPTION), &
                                           HIGHEST_ENERGY( &
                                                      HIGHEST_OPTION), &
                                           STRIKE_PRICE( &
                                                   HIGHEST_OPTION,MO), &
                                           HIGHEST_J, &
                                           HIGHEST_TOP_CAP_PERCENT, &
                                           MARKET_REVENUE( &
                                                      HIGHEST_OPTION), &
                                           TRANS_GROUP_4_PRICING, &
                                           RESOURCE_ADDITION)

                  ENDIF ! FILE_SOURCE_INDEX
!
! MAKE THE CHANGE TO THE PRICE CURVE, RESERVE MARGINS, CO2 MARKET, 
! REC MARKET, OTHER MARKETS
! CREDIT THE CHANGE TO ANY LIMITS ON ANNUAL, SCENARIO AND CHECK FOR 
! MUTUALLY DEPENDENT UNITS
!
! CREDIT RESERVES AND UPDATE CAPACITY MARKET
!
                  IF(PA > 0) THEN
                     TOTAL_ANNUAL_CAPACITY_ADDED(PA) = &
                           TOTAL_ANNUAL_CAPACITY_ADDED(PA) + &
                                         SCREEN_CAPACITY(HIGHEST_OPTION)
!
                     TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG) = &
                           TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG) + &
                                         SCREEN_CAPACITY(HIGHEST_OPTION)
! UPDATING CAPACITY MARKETS
                     IF(PEAK_AFTER_INTERRUPTIBLE(PA) > NEAR_ZERO)  THEN
                        PLANNING_RESERVE_MARGIN(PA) = &
                              1. + (PLANNING_CAPACITY(PA) + &
                                    TOTAL_ANNUAL_CAPACITY_ADDED(PA) - &
                                    PEAK_AFTER_INTERRUPTIBLE(PA)) / &
                                         PEAK_AFTER_INTERRUPTIBLE(PA)
                     ELSE
                        PLANNING_RESERVE_MARGIN(PA) = ZERO
                     ENDIF
                     

                     ICAP_REVENUE_MULT(PA) = GET_ICAP_REVENUE_MULT( &
                            R_YEAR,pa,PLANNING_RESERVE_MARGIN(PA))
!
                  ENDIF
!
                  TOTAL_ANNUAL_CAPACITY_ADDED(0) = &
                           TOTAL_ANNUAL_CAPACITY_ADDED(0) + &
                                         SCREEN_CAPACITY(HIGHEST_OPTION)
! UPDATING UNIT-BASED CONSTRAINTED
                  LOCAL_ANNUAL_UNITS(HIGHEST_OPTION) = &
                                  LOCAL_ANNUAL_UNITS(HIGHEST_OPTION) - 1
! UPDATING CO2 EMISSIONS CONSTRAINT
               CO2_EMISS_REDUCTION_REQUIRED(SYS) = &
                              CO2_EMISS_REDUCTION_REQUIRED(SYS)

                  RESOURCES_ADDED = .TRUE.
!
! SUMMARY REPORT
!
                  IF(GRX_SUMMARY_REPORT) THEN
!
                     IF(GRX_REPORT_NOT_OPEN) THEN
                        GRX_REPORT_NOT_OPEN = .FALSE.
                        GRX_REPORT_VARIABLES = 26
                        GRX_ANNUAL_ALT_NO = GRX_ANNUAL_HEADER( &
                                GRX_REPORT_VARIABLES,GRX_ANNUAL_ALT_REC)
                     ENDIF
!
                     RESOURCE_COUNTER(HIGHEST_OPTION) = &
                                    RESOURCE_COUNTER(HIGHEST_OPTION) + 1
!
                     WRITE(LOCAL_NAME,'(A,I3)') &
                                    OPTION_NAME(HIGHEST_OPTION)(1:19), &
                                        RESOURCE_COUNTER(HIGHEST_OPTION)

                     IF(HIGHEST_ENERGY(HIGHEST_OPTION) > 0.) THEN
                        MARKET_AVE_REVENUE = &
                              MARKET_REVENUE(HIGHEST_OPTION) / &
                                          HIGHEST_ENERGY(HIGHEST_OPTION)
       call write_trace_real4(file_trace_mga, "19. MAR", &
            MARKET_AVE_REVENUE)
                        TOTAL_AVERAGE_COST = &
                              MARKET_COST(HIGHEST_OPTION) / &
                                          HIGHEST_ENERGY(HIGHEST_OPTION)
       call write_trace_real4(file_trace_mga, "19. TAC", &
            TOTAL_AVERAGE_COST)
                     ELSE
                        MARKET_AVE_REVENUE = 0.
                        TOTAL_AVERAGE_COST = 0.
       call write_trace_real4(file_trace_mga, "20. TAC/MAR", &
            0.)
                     ENDIF
                     TG = ns_cap_trans%transaction_group(HIGHEST_OPTION)

                     L = GET_TRANS_GROUP_POSITION(TG)
!
                     LOCAL_PA = GET_PA_VALUE_FROM_TG(PA)
       if (file_trace_mga/=BAD_TRACE_HANDLE) then
        write (file_trace_mga, *) &
        "PRT_ENDPOINT", PRT_ENDPOINT(), &
        "YEAR", FLOAT(THIS_YEAR), &
        "MULTI_AREA_NAME", MULTI_AREA_NAME(L), &
        "LOCAL_NAME", LOCAL_NAME, &
        "iterations", FLOAT(GRX_ITERATIONS), &
        "STRIKE_PRICE", STRIKE_PRICE(HIGHEST_OPTION,MO), &
        "MARKET_AVE_REVENUE", MARKET_AVE_REVENUE, &
        "HIGHEST_NET_MARGIN_PER_MWH", HIGHEST_NET_MARGIN_PER_MWH, &
        "HIGHEST_CF", HIGHEST_CF, &
        "HIGHEST_REVENUE", HIGHEST_ENERGY_REVENUE/1000000., &
        "MARKET_COST", MARKET_COST(HIGHEST_OPTION)/1000000., &
        "AV cost", ANNUAL_VARIABLE_COST(HIGHEST_OPTION)/1000000., &
        "AF cost", ANNUAL_FIXED_COST(HIGHEST_OPTION)/1000000., &
        "SCREEN_CAPACITY", SCREEN_CAPACITY(HIGHEST_OPTION), &
        "HIGHEST_ENERGY", HIGHEST_ENERGY(HIGHEST_OPTION)/1000., &
        "TOTAL_AVERAGE_COST", TOTAL_AVERAGE_COST, & ! NEED DEFINITION
        "ALC", ANNUAL_LEVELIZED_CAPITAL(HIGHEST_OPTION), &
        "AFC", ANNUAL_FUEL_COST(HIGHEST_OPTION)/1000000., &
        "LAU", FLOAT(LOCAL_ANNUAL_UNITS(HIGHEST_OPTION)), &
        "HIGHEST_ICAP_REVENUE", HIGHEST_ICAP_REVENUE /1000000., &
        "MARKET_REVENUE", MARKET_REVENUE(HIGHEST_OPTION)/1000000., &
        "HIGHEST_NET_MARGIN_PER_KWYR", HIGHEST_NET_MARGIN_PER_KWYR, &
        "FUEL_COST_PER_MWH", FUEL_COST_PER_MWH(HIGHEST_OPTION,MO), &
        "VAR_OM_PER_MWH", VAR_OM_PER_MWH(HIGHEST_OPTION), &
        "LOCAL_PA", FLOAT(LOCAL_PA), &
        "TG", FLOAT(TG), &
        "CO2_UNIT_PRICE", CO2_UNIT_PRICE, &
        "REC", GRX_ANNUAL_ALT_REC
       end if
       
                     WRITE(GRX_ANNUAL_ALT_NO,REC=GRX_ANNUAL_ALT_REC) &
                        PRT_ENDPOINT(), &
                        FLOAT(THIS_YEAR), &
                        MULTI_AREA_NAME(L), &
                        LOCAL_NAME, &
                        FLOAT(GRX_ITERATIONS), &
                        STRIKE_PRICE(HIGHEST_OPTION,MO), &
                        MARKET_AVE_REVENUE, &
                        HIGHEST_NET_MARGIN_PER_MWH, &
                        HIGHEST_CF, &
                        HIGHEST_ENERGY_REVENUE/1000000., &
                        MARKET_COST(HIGHEST_OPTION)/1000000., &
                        ANNUAL_VARIABLE_COST(HIGHEST_OPTION)/1000000., &
                        ANNUAL_FIXED_COST(HIGHEST_OPTION)/1000000., &
                        SCREEN_CAPACITY(HIGHEST_OPTION), &
                        HIGHEST_ENERGY(HIGHEST_OPTION)/1000., &
                        TOTAL_AVERAGE_COST, & ! NEED DEFINITION
                        ANNUAL_LEVELIZED_CAPITAL(HIGHEST_OPTION), &
                        ANNUAL_FUEL_COST(HIGHEST_OPTION)/1000000., &
                        FLOAT(LOCAL_ANNUAL_UNITS(HIGHEST_OPTION)), &
                        HIGHEST_ICAP_REVENUE /1000000., &
                        MARKET_REVENUE(HIGHEST_OPTION)/1000000., &
                        HIGHEST_NET_MARGIN_PER_KWYR, &
                        FUEL_COST_PER_MWH(HIGHEST_OPTION,MO), &
                        VAR_OM_PER_MWH(HIGHEST_OPTION), &
                        FLOAT(LOCAL_PA), &
                        FLOAT(TG), &
                        CO2_UNIT_PRICE
!
                     GRX_ANNUAL_ALT_REC = GRX_ANNUAL_ALT_REC + 1
                  ENDIF
!
               ENDIF ! ADD THE RESOURCE
            ENDDO ! BEST RESOURCE IN EACH MARKET
            IF(CO2_RETIREMENTS_LOGIC .AND. &
                        CO2_UNIT_PRICE > CURRENT_CO2_DISPATCH_COST) THEN
!
               RESOURCES_ADDED = .FALSE.
            ENDIF
         ENDDO ! RESOURCE CONSTRAINTS (CO2, PROFIT, RESERVES, RPS)
!
         DO PA = 1, PG
            IF(PEAK_AFTER_INTERRUPTIBLE(PA) > NEAR_ZERO)  THEN
               PLANNING_RESERVE_MARGIN(PA) = &
                        1. + (PLANNING_CAPACITY(PA) + &
                               TOTAL_ANNUAL_CAPACITY_ADDED(PA) - &
                                    PEAK_AFTER_INTERRUPTIBLE(PA)) / &
                                         PEAK_AFTER_INTERRUPTIBLE(PA)
            ENDIF
!
            IF(MINIMUM_ANNUAL_MRX_CAP(PA) <= .1 + &
                                  TOTAL_ANNUAL_CAPACITY_ADDED(PA)) then
                   call write_trace_message(file_trace_mga, "9. CYCLE")
                CYCLE
            end if
            WRITE(4,*) "MINIMUM PLANNING AREA RESERVE NOT MET IN GRX",PA
         ENDDO
!
         IF(CO2_RETIREMENTS_LOGIC .AND. &
                              INIT_CO2_RETIRE_RETRO_PRICE > -9998.) THEN
!
          write(9,*) "BOTTOM OF GRX"
          write(9,*) "CO2 Price for next iter",CURRENT_CO2_DISPATCH_COST
!
            INIT_CO2_RETIRE_RETRO_PRICE = &
                             MAX(0.0,CURRENT_CO2_DISPATCH_COST * 0.0005)
            TEMP_L = PUT_GRX_DISP_EMIS_ADDER( &
                                          CO2_INT4, &
                                          SYS, &
                                          INIT_CO2_RETIRE_RETRO_PRICE)
         ENDIF
       call write_trace_message(file_trace_mga, "3. -ANNUAL_MRX_GREEN")
       RETURN ! ANNUAL_MRX_GREEN


      ENTRY ANNU_GRX_WITHOUT_CAP_ADDITS(R_YEAR)

!


         CO2_RETIREMENTS_LOGIC = GET_CO2_RETIREMENTS_LOGIC()

         CO2_RETOFIT_LOGIC = CO2_RETROFIT_LOGIC_ACTIVE()   !MSG AUG 09

         CO2_RETIREMENTS_LOGIC = CO2_RETIREMENTS_LOGIC .OR. &
                                         CO2_RETOFIT_LOGIC     ! AUG 09

         CURRENT_CO2_DISPATCH_COST = NEXT_CURRENT_CO2_DISPATCH_COST 



            IF(CO2_Price_Evaluation(R_YEAR+get_BASE_YEAR())) THEN

               CURRENT_CO2_DISPATCH_COST = &
                            CO2_Price_Forecast(get_BASE_YEAR()+globecom_year)

            ELSE
               CURRENT_CO2_DISPATCH_COST = &
                                    MAX(CURRENT_CO2_DISPATCH_COST, &
                                        CO2_Min_Price(R_YEAR+get_BASE_YEAR()))
            ENDIF

               INIT_CO2_RETIRE_RETRO_PRICE = &
                          MAX(0.0,CURRENT_CO2_DISPATCH_COST * 0.0005)
               TEMP_L = PUT_GRX_DISP_EMIS_ADDER( &
                                          CO2_INT4, &
                                          SYS, &
                                          INIT_CO2_RETIRE_RETRO_PRICE)
         IF(GRX_ITERATIONS <= 0 .OR. .NOT. CO2_RETIREMENTS_LOGIC) then
            return
         end if
         IF(.NOT. CO2_RETIREMENTS_LOGIC) then
            RETURN
         end if
!
        IF(ALLOCATED(CO2_EMISSIONS_CAP)) DEALLOCATE(CO2_EMISSIONS_CAP, &
                                         CO2_EMISS_REDUCTION_REQUIRED, &
                                          CO2_EMISSIONS)
         ALLOCATE(CO2_EMISSIONS_CAP(0:UPPER_TRANS_GROUP), &
                  CO2_EMISS_REDUCTION_REQUIRED(0:UPPER_TRANS_GROUP), &
                  CO2_EMISSIONS(0:UPPER_TRANS_GROUP))
         GRX_CO2_MARKET_PTS_USED = 0


         GRX_SUMMARY_REPORT = EXPANSION_REPORT()

         UPPER_TRANS_GROUP = GET_NUMBER_OF_ACTIVE_GROUPS()

! SYSTEM CO2

         CO2_EMISSIONS_CAP = 999999999999.0
         CO2_EMISS_REDUCTION_REQUIRED = 0.0
         CO2_EMISSIONS = 0.0
         RETRO_FITTED_CAP = 0.
         TG = 0

         CO2_EMISSIONS_CAP(TG) = GET_EMISS_CAP_FOR_CLASS(CO2_PLN,TG)
         CO2_EMISSIONS(TG) = GET_CL_EMISS_FOR_CLASS(CO2_PLN,TG)
         ! STARTS CO2 REDUCTION CURVE ROUTINES
         CALL GRX_CO2_REDUCTION_CURVE(TG) 
         CURRENT_CO2_DISPATCH_COST = NEXT_CURRENT_CO2_DISPATCH_COST 
         
         CO2_EMISS_REDUCTION_REQUIRED(TG) = &
                               CO2_EMISSIONS(TG) - CO2_EMISSIONS_CAP(TG)
         IF(GRX_ITERATIONS <= 1) THEN
            GRX_CO2_EMISS_REDUCT_REQUIRED(TG) = &
                                        CO2_EMISS_REDUCTION_REQUIRED(TG)
            WRITE(9,*) "Saved CO2 req", &
                        GRX_CO2_EMISS_REDUCT_REQUIRED(TG)
         ELSE
            CO2_EMISS_REDUCTION_REQUIRED(TG) = &
                                    GRX_CO2_EMISS_REDUCT_REQUIRED(TG)
         ENDIF
!
         WRITE(9,*) "TOP OF GRX YEAR ITERATION",THIS_YEAR,gc_end_point, &
                    "ITER",GRX_ITERATIONS, &
                    "CO2_EMISSIONS_CAP", &
                            GRX_ALL_SECTOR_CO2_EMISS_CAP(THIS_YEAR), &
                    "CO2_EMISSIONS    ",CO2_EMISSIONS(TG), &
                    "CO2_EMISS_REDUCTION_REQUIRED", &
                     CO2_EMISS_REDUCTION_REQUIRED(TG), &
                    "CURRENT PRICE",CURRENT_CO2_DISPATCH_COST
!
         CO2_COEFF = 6E-28
         CO2_POWER = 13.618
         RESOURCE_ADDITION = .TRUE.



         MAX_RESOURCES_PER_GROUP = 0
         MAX_RESOURCES_PER_YEAR_GROUP = &
                                MX_RES_P_YEAR/UPPER_TRANS_GROUP
         
         ! Removed HIGHEST_ENERGY initialization because the
         ! array has been deallocated by this point.  Moved
         ! initialization to allocation point.
         

! 061309. BEGIN GRX ALGORITHM.

         CO2_CAP_NOT_MET = .TRUE.
         CO2_RESOURCES_AVAIL = .TRUE.
         RESOURCES_PROFITABLE = .TRUE.
         RESERVES_NOT_MET = .TRUE.
         ENRG_RESOURCES_AVAIL = .TRUE.
         RPS_CAPS_NOT_MET = .TRUE.
         RPS_EXPOURCES_AVAIL = .TRUE.

         UNITS_RETROFITTED = 0
         CO2_RETIREMENT_OPTIONS = 0
         CO2_RETROFIT_OPTIONS = 0
         UNIT_Decision_CO2_Price = 0.


         BASE_DATE = (R_YEAR + get_BASE_YEAR() - 1900) * 100 + 7

         
          


         OPTIONS_ARE_AVAILABLE = .FALSE.
         RESOURCES_ADDED = CO2_RETIREMENTS_LOGIC
         CO2_END_LIST = .FALSE.
         CO2_COUNTER = 1
         DO WHILE(RESOURCES_ADDED .AND. RETIRE_RETRO_COUNTER > 0)

! CO2 RETIREMENTS PRICE
            IF(CO2_RETIREMENTS_LOGIC .AND. .FALSE.) THEN
               TEMP_R4 = CO2_EMISS_REDUCTION_REQUIRED(SYS)

! 062709. REPLACES THE RETIREMENT PRICE ROUTINE.

               CO2_RETIRE_RETRO_PRICE = &
                                     GET_CO2_RETIRE_RETRO_PRICE(TEMP_R4)
               INIT_CO2_RETIRE_RETRO_PRICE = CO2_RETIRE_RETRO_PRICE
            ENDIF


! RPS
! RETIREMENT AND RETROFIT

            IF(.NOT. CO2_END_LIST .AND. &
                     CO2_RETIREMENTS_LOGIC) THEN
               TEMP_L = GET_NEXT_MRX_RETIRE_RETRO( &
                                          CO2_COUNTER, &
                                        CO2_RETIRE_OR_RETRO_OR_MARKET, &
                                          CO2_NUNIT, &
                                          CO2_TG, &
                                          CO2_UNIT_MW, &
                                          CO2_UNIT_MWH, &
                                          UNIT_CO2, &
                                          CO2_UNIT_PRICE, &
                                          CO2_STRIKE_PRICE, &
                                          CO2_UNIT_MW_AFTER, &
                                          CO2_STRIKE_PRICE_AFTER, &
                                          UNIT_CO2_AFTER, &
                                          CO2_END_LIST)

               CO2_UNIT_PRICE = ECON_RETIRE_MARGIN(CO2_NUNIT,0)
               CO2_UNIT_PRICE = RETIRE_RETRO_CO2_PRICE(CO2_COUNTER-1)
!
               IF(HWRetrofitProjectThisYear(CO2_NUNIT)) THEN
                  TEMP_L = RETROFIT_CO2_THERMAL_UNIT( &
                                                   CO2_NUNIT, &
                                                   CO2_UNIT_MW, &
                                                   UNIT_CO2, &
                                                   CO2_UNIT_PRICE, &
                                          CO2_RETIRE_OR_RETRO_OR_MARKET)
                  UNIT_Decision_CO2_Price(CO2_NUNIT) = CO2_UNIT_PRICE
                  RETRO_FITTED_CAP = RETRO_FITTED_CAP &
                                     + (CO2_UNIT_MW - CO2_UNIT_MW_AFTER)
               ELSE
                  IF(CO2_RETIRE_OR_RETRO_OR_MARKET == 1) THEN ! RETIRE
                     IF(.NOT. RETIRE_THIS_UNIT(CO2_NUNIT)) then
                        CYCLE
                     end if

                     TEMP_L = RETIRE_CO2_THERMAL_UNIT( &
                                             CO2_NUNIT, &
                                             CO2_UNIT_MW, &
                                             UNIT_CO2, &
                                             CO2_UNIT_PRICE)

                  ELSEIF(CO2_RETIRE_OR_RETRO_OR_MARKET == 2) THEN
                     IF(RETRO_FITTED_CAP > 15000.) then
                        CYCLE
                     end if

                     IF(CO2_UNIT_PRICE>= &
           RETIREMENT_CO2_COST_PER_TON(CO2_NUNIT)) then
                                                            CYCLE
                                                    end if


                     TEMP_L = RETROFIT_CO2_THERMAL_UNIT( &
                                                   CO2_NUNIT, &
                                                   CO2_UNIT_MW, &
                                                   UNIT_CO2, &
                                                   CO2_UNIT_PRICE, &
                                          CO2_RETIRE_OR_RETRO_OR_MARKET)
                     UNIT_Decision_CO2_Price(CO2_NUNIT) = CO2_UNIT_PRICE
                     TEMP_L = ADJUST_GRX_CO2_RETRO_PLAN_CAP(globecom_year, &
                                                            CO2_NUNIT)
                     RETRO_FITTED_CAP = RETRO_FITTED_CAP &
                                     + (CO2_UNIT_MW - CO2_UNIT_MW_AFTER)
                  ELSE ! A MARKET HAS BEEN FOUND
                     cycle ! to remove market from abatement using units
                  ENDIF
                  CO2_EMISS_REDUCTION_REQUIRED(0) = &
                        CO2_EMISS_REDUCTION_REQUIRED(0) - UNIT_CO2 + &
                                                          UNIT_CO2_AFTER

               ENDIF ! RETIREMENT OR RETROFIT

             ENDIF ! ADD THE RESOURCE

             IF(CO2_END_LIST) then
                EXIT
             end if
         ENDDO ! RESOURCE CONSTRAINTS (CO2, PROFIT, RESERVES, RPS)

         IF(CO2_RETIREMENTS_LOGIC .and. .false.) THEN

          write(9,*) "BOTTOM OF GRX"
          write(9,*) "CO2 Price for next iter",CURRENT_CO2_DISPATCH_COST

               INIT_CO2_RETIRE_RETRO_PRICE = &
                             MAX(0.0,CURRENT_CO2_DISPATCH_COST * 0.0005)
               TEMP_L = PUT_GRX_DISP_EMIS_ADDER(CO2_INT4, &
                                            SYS, &
                                            INIT_CO2_RETIRE_RETRO_PRICE)
         ENDIF
       RETURN


      ENTRY GET_MRX_GREEN_PLANNING_MARGIN(R_PA,R_RESERVE)

         IF(ALLOCATED(PLANNING_RESERVE_MARGIN)) THEN
            R_RESERVE = PLANNING_RESERVE_MARGIN(R_PA)
         ELSE
            R_RESERVE = -999.0
         ENDIF
      RETURN

      ENTRY GET_MRX_GREEN_CAP_PLAN_MARGIN(R_PA,R_RESERVE)

! NEEDS TO INCORPORATE CM
         IF(ALLOCATED(PLANNING_RESERVE_MARGIN)) THEN
            R_RESERVE = PLANNING_RESERVE_MARGIN(R_PA)
         ELSE
            R_RESERVE = -999.0
         ENDIF
      RETURN
      END

















































































































































































































































































































































































































































































































































































































































































