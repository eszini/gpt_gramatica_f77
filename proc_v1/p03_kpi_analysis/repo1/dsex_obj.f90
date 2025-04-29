
!     Dsex_obj.for
!     Copyright(c)  2000
!
!     Created: 6/23/2003 2:14:00 PM
!     Author : MARK S GERBER
!     Last change: msg 11/21/2021 12:54:01 PM


! Last change: MSG 6/23/2003 2:12:08 PM
      FUNCTION START_UP(R_DEBUG_ON,R_BASE_FILE_DEFINITION, &
                        POINTS_IN_LOAD_CURVE,R_BASE_FILE_FAMILY)

! ISOLATE START_UP VARIABLES

      use filemod
      use logging
      use startup_tasks
      USE SERVICE_ROUTINES
      USE prod_arrays_dimensions
      use cl_data
      use dsex_data
      use dreptcom
      implicit none
      
      CHARACTER (len=*) :: R_BASE_FILE_DEFINITION,R_BASE_FILE_FAMILY
      LOGICAL (kind=1) ::  SET_DELTA_X_IN_LOAD_CURVE
      REAL (kind=4) ::  DELTA_X_FOR_LOAD_CURVE
      CHARACTER (len=256) ::  TEMP_BASE_FILE_DEF,TEMP_BASE_FAMILY_NAME
      CHARACTER (len=256) ::  BASE_FILE_DIRECTORY,RESULTS_DIRECTORY, &
                    FILE_NAME,GET_RESULTS_DIRECTORY
      CHARACTER (len=256) ::  BASE_FILE_DEFINITION
      INTEGER (kind=2) ::  LENGHT,tagPOS,POINTS_IN_LOAD_CURVE
      LOGICAL (kind=1) ::  NO_WARNING_WINDOW
      LOGICAL (kind=1) ::  DONT_SUPPRESS_WARNING_WINDOW, &
                DEBUG_ON,DEBUG_ON_ACTIVE,R_DEBUG_ON, &
                ITERATION_ACTIVE,ITERATION_ON, &
                MULTI_AREA_TRANC,MULTI_AREA_TRANC_ACTIVE, &
                SIMSTATUS,SIMSTATUS_ACTIVE, &
                NUCLEAR_FUEL_REPORT_ON,NUCLEAR_FUEL_REPORT_ACTIVE, &
                MONTAUK_ACTIVE,MONTAUK_ACTIVE_FLAG, &
                CREDITS_IN_INCOME_STATEMENT,CREDITS_IN_INCOME_FLAG, &
                OVERLAY_BOOK_ACTIVE,USE_OVERLAY_BOOK, &
                START_UP, &
                SET_POINTS_IN_LOAD_CURVE, &
                VOID_LOGICAL
      LOGICAL (kind=1) :: &
                SET_DELTA_X_VALUE, &
                CD_LINE_ASSET_ANALYST_ACTIVE, &
                CL_ASSET_ANALYST_ACTIVE, &
                QUICKSILVER,QUICKSILVER_ACTIVE, &
                NO_AUTO_INDEXING,NO_AUTO_INDEXING_ACTIVE, &
                SP_CAPEX_ACTIVE, &
                SET_SP_CAPEX_ACTIVE_WO_DISPLAY, &
                SET_SP_CAPEX_ACTIVE_W_DISPLAY, &
                SP_CAPEX_DISPLAY_ACTIVE, &
                SET_DEBUG_ON, &
                COAL_MODEL_ONLY_ACTIVE,RUN_COAL_MODEL_ONLY
      SAVE NUCLEAR_FUEL_REPORT_ACTIVE, &
           USE_OVERLAY_BOOK, &
           MONTAUK_ACTIVE_FLAG, &
           DEBUG_ON_ACTIVE, &
           ITERATION_ACTIVE, &
           MULTI_AREA_TRANC, &
           SIMSTATUS, &
           CREDITS_IN_INCOME_FLAG, &
           DONT_SUPPRESS_WARNING_WINDOW, &
           CL_ASSET_ANALYST_ACTIVE, &
           QUICKSILVER_ACTIVE, &
           NO_AUTO_INDEXING_ACTIVE, &
           COAL_MODEL_ONLY_ACTIVE
      LOGICAL (KIND=1) , SAVE :: RUN_SP_CAPEX=.FALSE., &
                                DISPLAY_SPCAPEX=.TRUE.
      INTEGER (KIND=2) , SAVE :: ENDING_ENDPOINT,STARTING_ENDPOINT, &
                                ITERATION_NUMBER
      INTEGER (KIND=2) :: START_STUDY_ENDPOINT,LAST_STUDY_ENDPOINT, &
                          GET_ITERATION_NUMBER
      integer :: idx, idx2, ic
      character (len=256) :: tempcl
      character (len=5) :: col1, col2, col3


        LENGHT=0 ! Avoid spurious lahey "not initialized" warning

        if(len_trim(cldata%command_line)<2) then
            call end_program("dsex_obj:0001 - Command line not set.")
        endif
        call startup_dsex(TEMP_BASE_FAMILY_NAME)
!
! SET COMMAND LOGIC SWITCHES
!
         tempcl=cldata%command_line
         MONTHLY_MIDAS_ACTIVE = (INDEX(tempcl,'/ACTMONTHY') /= 0)
         DEBUG_ON_ACTIVE = (INDEX(tempcl,'/DF') /= 0)
         MULTI_AREA_TRANC = (INDEX(tempcl,'/MULTI_TRANC') /= 0)
         SIMSTATUS = (INDEX(tempcl,'/SIMSTATUS') /= 0)
         R_DEBUG_ON = DEBUG_ON_ACTIVE
         NUCLEAR_FUEL_REPORT_ACTIVE = (INDEX(tempcl,'/NF') /= 0)
         DONT_SUPPRESS_WARNING_WINDOW =(INDEX(tempcl,'/BAT') == 0)
         USE_OVERLAY_BOOK = .TRUE. ! INDEX(tempcl,'/OVLBOOK') /= 0
         MONTAUK_ACTIVE_FLAG = INDEX(tempcl,'/MONTAUK') /= 0
         CREDITS_IN_INCOME_FLAG = INDEX(tempcl,'/CREDIT') /= 0
         CL_ASSET_ANALYST_ACTIVE = INDEX(tempcl,'/AACTIVE') /= 0
         QUICKSILVER_ACTIVE = INDEX(tempcl,'/QUICKSILVER') /= 0
         NO_AUTO_INDEXING_ACTIVE = INDEX(tempcl,'/NOINDX') /= 0
         COAL_MODEL_ONLY_ACTIVE=INDEX(tempcl,'/COALMODELONLY') /=0
         IF(QUICKSILVER_ACTIVE .OR. RUN_SP_CAPEX) then
             DONT_SUPPRESS_WARNING_WINDOW = .FALSE.
        endif
!
         if(len_trim(tempcl)<20) then
            call end_program("dsex:0004 - Command-line not set " // &
       "in startup_dsex.")

         endif
         ITERATION_ACTIVE = .FALSE.
         IF(INDEX(tempcl,'/ITER') /= 0) THEN
            tagPOS = INDEX(tempcl,'/ITER') + 5
            READ(tempcl(tagPOS:),*) ITERATION_NUMBER
            ITERATION_ACTIVE = .TRUE.
         ENDIF

         ENDING_ENDPOINT = 30000
         STARTING_ENDPOINT = 1
         IF(INDEX(tempcl,'/EPS') /= 0) THEN
            tagPOS = INDEX(tempcl,'/EPS') + 4
            READ(tempcl(tagPOS:),*) STARTING_ENDPOINT
            STARTING_ENDPOINT = MAX(INT(1,2),STARTING_ENDPOINT)
         ENDIF
         IF(INDEX(tempcl,'/EPE') /= 0) THEN
            tagPOS = INDEX(tempcl,'/EPE') + 4
            READ(tempcl(tagPOS:),*) ENDING_ENDPOINT
            ENDING_ENDPOINT = MAX(INT(1,2),ENDING_ENDPOINT, &
                              STARTING_ENDPOINT)
         ENDIF
         SET_POINTS_IN_LOAD_CURVE = INDEX(tempcl,'/PS') /= 0
         IF(SET_POINTS_IN_LOAD_CURVE) THEN
            tagPOS = INDEX(tempcl,'/PS') + 3
            READ(tempcl(tagPOS:),*) POINTS_IN_LOAD_CURVE
         ELSE
            POINTS_IN_LOAD_CURVE = 79
         ENDIF
         SET_DELTA_X_IN_LOAD_CURVE = INDEX(tempcl,'/DX') /= 0
         IF(SET_DELTA_X_IN_LOAD_CURVE) THEN
            tagPOS = INDEX(tempcl,'/DX') + 3
            READ(tempcl(tagPOS:),*) DELTA_X_FOR_LOAD_CURVE
         ELSE
            DELTA_X_FOR_LOAD_CURVE = -100.
         ENDIF
         VOID_LOGICAL = SET_DELTA_X_VALUE(DELTA_X_FOR_LOAD_CURVE)
!
! STRIP THE COMMAND LINE OF THE PROJECT NAME AND BASE FAmILY
!


! STRIP PROJECT/DSF FILE NAME
!

         tempcl = trim(tempcl)//",,,"
         TEMP_BASE_FILE_DEF = ""

!         Read TEMP_BASE_FAMILY_NAME from command line
         idx=index(tempcl, ",")
         call read_cols_from_tempcl(tempcl, col1, col2, col3)
         temp_base_file_def=col1
         if(len_trim(temp_base_file_def)<2) then
            call end_program("dsex:99 - " // &
       "Could not read base file definition from command line.")
         endif
         BASE_FILE_DEFINITION=trim(col2)
         TEMP_BASE_FAMILY_NAME=trim(col3)



         if(len_trim(base_file_definition)<2) then
             call end_program("dsex:0001 - " // &
                 "base_file_definition not set.")
         endif
!
! STRIP BASE FAMILY NAME
!

       call write_log_entry("dsex:00020", &
       "TEMP_BASE_FILE_DEF=" // trim(TEMP_BASE_FILE_DEF) // &
       ", " // "TEMP_BASE_FAMILY_NAME=" // &
       trim(TEMP_BASE_FAMILY_NAME))

         R_BASE_FILE_FAMILY = 'NONE'

         IF(trim(TEMP_BASE_FAMILY_NAME) /= ' ') then
             call write_log_entry("dsex:00018", &
               "Setting R_BASE_FILE_FAMILY=" // &
                trim(TEMP_BASE_FAMILY_NAME))

             R_BASE_FILE_FAMILY = TEMP_BASE_FAMILY_NAME

         endif

         R_BASE_FILE_DEFINITION = BASE_FILE_DEFINITION

         RPT_BASE_FAMILY_NAME = R_BASE_FILE_FAMILY
         RPT_PROJECT_NAME = BASE_FILE_DEFINITION
         cldata%command_line=tempcl
         START_UP = .TRUE.
      RETURN

      ENTRY GET_ITERATION_NUMBER
         GET_ITERATION_NUMBER = ITERATION_NUMBER
      RETURN

      ENTRY START_STUDY_ENDPOINT
         START_STUDY_ENDPOINT = STARTING_ENDPOINT
      RETURN

      ENTRY LAST_STUDY_ENDPOINT
         LAST_STUDY_ENDPOINT = ENDING_ENDPOINT
      RETURN

      ENTRY MONTAUK_ACTIVE
         MONTAUK_ACTIVE = MONTAUK_ACTIVE_FLAG
      RETURN

      ENTRY CREDITS_IN_INCOME_STATEMENT
         CREDITS_IN_INCOME_STATEMENT = CREDITS_IN_INCOME_FLAG
      RETURN

      ENTRY OVERLAY_BOOK_ACTIVE
         OVERLAY_BOOK_ACTIVE = USE_OVERLAY_BOOK
      RETURN

      ENTRY DEBUG_ON
         DEBUG_ON = DEBUG_ON_ACTIVE
      RETURN

      ENTRY SET_DEBUG_ON
         DEBUG_ON_ACTIVE = .TRUE.
         SET_DEBUG_ON = DEBUG_ON_ACTIVE
      RETURN

      ENTRY ITERATION_ON
         ITERATION_ON = ITERATION_ACTIVE
      RETURN

      ENTRY MULTI_AREA_TRANC_ACTIVE
         MULTI_AREA_TRANC_ACTIVE = MULTI_AREA_TRANC
      RETURN

      ENTRY SIMSTATUS_ACTIVE
         SIMSTATUS_ACTIVE = SIMSTATUS
      RETURN

      ENTRY NO_WARNING_WINDOW
         NO_WARNING_WINDOW = DONT_SUPPRESS_WARNING_WINDOW
      RETURN

      ENTRY QUICKSILVER
         QUICKSILVER = QUICKSILVER_ACTIVE
      RETURN

      ENTRY SP_CAPEX_ACTIVE
         SP_CAPEX_ACTIVE = RUN_SP_CAPEX
      RETURN

      ENTRY SP_CAPEX_DISPLAY_ACTIVE
         SP_CAPEX_DISPLAY_ACTIVE = DISPLAY_SPCAPEX
      RETURN

      ENTRY SET_SP_CAPEX_ACTIVE_W_DISPLAY()
         RUN_SP_CAPEX = .true.
         DISPLAY_SPCAPEX = .TRUE.
         SET_SP_CAPEX_ACTIVE_W_DISPLAY = DISPLAY_SPCAPEX
      RETURN

      ENTRY SET_SP_CAPEX_ACTIVE_WO_DISPLAY()
         RUN_SP_CAPEX = .true.
         DISPLAY_SPCAPEX = .FALSE.
         SET_SP_CAPEX_ACTIVE_WO_DISPLAY = RUN_SP_CAPEX
      RETURN

      ENTRY NO_AUTO_INDEXING
         NO_AUTO_INDEXING = NO_AUTO_INDEXING_ACTIVE
      RETURN

      ENTRY RUN_COAL_MODEL_ONLY
         RUN_COAL_MODEL_ONLY  = COAL_MODEL_ONLY_ACTIVE
      RETURN

      ENTRY NUCLEAR_FUEL_REPORT_ON
         NUCLEAR_FUEL_REPORT_ON = NUCLEAR_FUEL_REPORT_ACTIVE
      RETURN

      ENTRY CD_LINE_ASSET_ANALYST_ACTIVE
         CD_LINE_ASSET_ANALYST_ACTIVE =  CL_ASSET_ANALYST_ACTIVE
      RETURN
      END function START_UP
!

      FUNCTION DSEXEC_OBJECT(POINTS_IN_LOAD_CURVE)
      use end_routine, only: end_program, er_message
      use params
      use logging
      use miscmod
      use dsex_data
      use cl_data
      use filemod
      use spindriftlib
      use prod_arrays_dimensions
      use kepcocom
      use sizecom
      use iostatmsg
      use dreptcom
      use forecast
      use mod_base_year
      use msgoutpt
      implicit none
      
      CHARACTER (len=2048) ::  DSEXEC_LINE
      character (len=152) :: ERR_MESSAGE
      CHARACTER (len=1) ::  ESC_DISP_ADDER_CHR,SHORT_FORM_CHR
      LOGICAL (kind=1) ::  SHORT_FORM_ACTIVE=.FALSE.
      CHARACTER (len=12) ::  DES_FILE_NAME
      INTEGER (kind=2) ::  DELETE

      INTEGER (kind=4) ::  IOS
      INTEGER (kind=2) ::  DIREC_LEN
      INTEGER (kind=2) ::  I,ACTIVE_VARIABLES
      INTEGER (kind=2) ::  STORED_PROCMETH
      INTEGER (kind=2) ::  SET_PRODUCTION_METHOD,DIMENSIONS
      SAVE STORED_PROCMETH
      CHARACTER (len=256) ::  BASE_FILE_DIRECTORY
      CHARACTER (len=256) ::  FILE_NAME,RESULTS_DIRECTORY, &
                     LDE_FILE_DIRECTORY, &
                     LDG_FILE_DIRECTORY, &
                     ENRG_PATTERN_FILE_DIRECTORY, &
                     WORKING_FILE_DIRECTORY, &
                     GET_WORKING_FILE_DIRECTORY, &
                     PRB_FILE_DIRECTORY, &
                     SHB_FILE_DIRECTORY, &
                     PRJ_FILE_DIRECTORY, &
                     XML_FILE_DIRECTORY, &
                     COMMAND_LINE,GET_RESULTS_DIRECTORY
      INTEGER (kind=2) ::  PRODUCTION_PERIODS_IN
      INTEGER (kind=2) ::  ENDING_MONTHS(4)=0,ISERROR
      LOGICAL (kind=1) ::  ESC_DISP_ADDER_ACTIVE



      INTEGER (kind=4) ::  R_START_RECORD
      CHARACTER (len=28) ::  COMPANY_NAME_STR
      CHARACTER (len=28) ::  TITLE_STR*40,STUDY_DESCRIPTION*40
      CHARACTER (len=1) ::  PRODUCTION_COST_METHOD_CHR,UTILITY_TYPE_CHR, &
                  UTILITY_TYPE
      CHARACTER (len=10) ::  UTILITY_TYPE_STR,TEMP_STR_10
      

      INTEGER (kind=4) ::  REC_LENGHT
      LOGICAL (kind=4) ::  FILE_EXISTS
      CHARACTER (len=18) ::  DATE_TIME,SYM_SYS_DATE_TIME

! CONVERSION FACTOR DEFININTIONS
!
      REAL ::  GET_COST_CONVERSION
      REAL ::  GET_HEAT_CONVERSION,GET_TONS_CONVERSION
      REAL ::  COST_CONVERSION,HEAT_CONVERSION,TONS_CONVERSION
      SAVE COST_CONVERSION,HEAT_CONVERSION,TONS_CONVERSION
!
! FUNCTION DEFINITIONS
!
      CHARACTER (len=1) ::  START_PRODUCTION_COST_METHOD
      INTEGER (kind=2) ::  LAST_VARIABLE
      CHARACTER (len=1) ::  DSEXEC_OBJECT
      INTEGER (kind=2) ::  PRODUCTION_PERIODS
      INTEGER (kind=4) ::  EXE_DATE
      INTEGER (kind=2) ::  GET_NUM_END_POINTS_IN_OVL_FILE,END_POINTS
      CHARACTER (len=16) ::  SET_RUN_DATE_TIME
      CHARACTER (len=16) ::  RUN_DATE_TIME,GET_RUN_DATE_TIME
      CHARACTER (len=28) ::  COMPANY_NAME,TITLE*40
      LOGICAL (kind=1) ::  VOID_LOGICAL,SET_DELTA_X_VALUE
      LOGICAL (kind=1) ::  SHORT_FORM
      CHARACTER (len=1) ::  FORECAST_TYPE,COUNTRY
      INTEGER (kind=4) ::  NUM_ARGS
      CHARACTER (len=256) ::  SHORT_FORM_FILE_NAME
      CHARACTER (len=64) ::  ARGUMENTS(11)
      CHARACTER (len=32) ::  SHORT_FORM_TITLE=' '
      CHARACTER (len=8) ::  SHORT_FORM_NAME ! CHANGED 12/6/93.GAT
      CHARACTER (len=12) ::  SHORT_FORM_DES_NAME
!
      SAVE ESC_DISP_ADDER_ACTIVE, &
           COMPANY_NAME_STR,TITLE_STR, &
           PRODUCTION_COST_METHOD_CHR,UTILITY_TYPE_CHR, &
           RESULTS_DIRECTORY, &
           END_POINTS,REC_LENGHT, &
!    +     STORED_PERIOD & !     +     STORED_PERIOD,
           PRODUCTION_PERIODS_IN, &
           RUN_DATE_TIME
!
! PLANNING SWITCH INFORMATION
!
      CHARACTER (len=4) ::  STR_BASE_YEAR
      CHARACTER (len=4) ::  STR_END_YEAR,STR_EXTENSION_PERIOD
      CHARACTER (len=1) ::  STR_PRODUCTION_PERIODS=" ", &
                  START_PRODUCTION_PERIODS
      CHARACTER (len=64) ::  OBJECTIVE_FUNC_POINTER
      SAVE SHORT_FORM_NAME
      CHARACTER (len=20) ::  SIMULATION_MODEL
      CHARACTER (len=5) ::  VERSION
!
! ISOLATE THIS ROUTINES VARIABLES
!
      REAL ::  VOID_REAL,SET_HEAT_FACTORS_CLASS_GROWTH
      LOGICAL (kind=1) ::  SAVE_EXTERNAL_PRODUCTION_COST
      LOGICAL (kind=1) ::  SET_STUDY_PARAMS_LOGICAL
      INTEGER (kind=2) ::  POINTS_IN_LOAD_CURVE
      LOGICAL (kind=1) ::  S_UTILITY_IDENTIFIER
      LOGICAL (kind=1) ::  ORLANDO_UC,SALT_RIVER_PROJECT
      CHARACTER (len=256) ::  OUTPUT_DIRECTORY
      CHARACTER (len=2) ::  ACTIVE_DATA_DRIVE
      CHARACTER (len=1) ::  VOID_CHR1,CHECK_CAP_PLANNING_SWTCHES
      LOGICAL (kind=1) ::  LAHEY_LF95
      INTEGER (kind=1) ::  F7=7! 7H == z"f7" Hex/Dec 7 same value
      CHARACTER (len=256) ::  IOS_ERROR_MESSAGE
      LOGICAL (KIND=1) :: SP_CAPEX_ACTIVE
      integer (kind=2) :: by, loc_ext_period
         DSEXEC_OBJECT = 'X'
         SIMULATION_MODEL = 'MIDAS Gold '//VERSION()
         COUNTRY_CHR = 'U'
         SHORT_FORM_CHR = 'F'
         STUDY_DESCRIPTION = ' '
!
! DETERMINE OUTPUT DIRECTORY
!
         CALL CURDIR(" ",RESULTS_DIRECTORY)
         DIREC_LEN = LEN(trim(RESULTS_DIRECTORY))
         IF(DIREC_LEN >= 3 .AND. &
                    RESULTS_DIRECTORY(DIREC_LEN:DIREC_LEN) /= '\') &
                      RESULTS_DIRECTORY = trim(RESULTS_DIRECTORY)//'\'
!
         FILE_NAME = trim(RESULTS_DIRECTORY)//'MSGPARAM.VAL'
         if(.not. does_file_exist(trim(file_name)))then
            er_message="dsex_obj:0002 - file " // trim(file_name) // &
               " does not exist."
            call end_program(er_message)
         end if
         OPEN(10,FILE=FILE_NAME,ACCESS="SEQUENTIAL",STATUS="OLD", &
                                                             IOSTAT=IOS)
         IF(IOS /= 0) THEN
            CALL iostatmsg_unit(IOS,IOS_ERROR_MESSAGE)
            CALL CRITICAL_ERROR_MSG(2) !
         ENDIF
         READ(10,"(A)") DSEXEC_LINE
         call close_unit(int(10,2))
         IF(DSEXEC_LINE(1:1) /= '9') THEN
            STR_EXTENSION_PERIOD = '0000'
            DSEXEC_LINE = trim(DSEXEC_LINE)//',,,,,,,,,,,,,,,,,,,,,,,'
            READ(DSEXEC_LINE,*,ERR=9000) COMPANY_NAME_STR,TITLE_STR, &
                                         STR_BASE_YEAR, &
                                         PRODUCTION_COST_METHOD_CHR, &
                                         STR_END_YEAR, &
                                         STR_PRODUCTION_PERIODS, &
                                         FORECAST_TYPE_CHR, &
                                         BASE_FILE_DIRECTORY, &
                                         UTILITY_TYPE_STR, &
                                         COUNTRY_CHR, &
                                         ESC_DISP_ADDER_CHR, &
                                         SHORT_FORM_CHR, &
                                         SHORT_FORM_NAME, &
                                         STR_EXTENSION_PERIOD, &
                                         SHORT_FORM_TITLE, &
                                         STUDY_DESCRIPTION, &
                                         LDE_FILE_DIRECTORY, &
                                         PRB_FILE_DIRECTORY, &
                                         SHB_FILE_DIRECTORY, &
                                         PRJ_FILE_DIRECTORY, &
                                         XML_FILE_DIRECTORY, &
                                         ENRG_PATTERN_FILE_DIRECTORY, &
                                         WORKING_FILE_DIRECTORY, &
                                         LDG_FILE_DIRECTORY

            IF(trim(STUDY_DESCRIPTION) /= ' ') &
                                           TITLE_STR = STUDY_DESCRIPTION
!
! PROCESS AND ADJUST THE READ INFORMATION
!
            IF(trim(STR_BASE_YEAR) /= ' ') THEN
               READ(STR_BASE_YEAR,*) by
               if(by/=0) then
                call set_base_year(by)
               end if
            ELSE
               call set_base_year(int(2002,2))
            ENDIF
            IF(trim(STR_END_YEAR) /= ' ') THEN
               READ(STR_END_YEAR,*) stored_end_year
            ELSE
               stored_end_year = 2010
            ENDIF
            ! stored_end_year=2027/2027
            ! Stored_Base_Year=2022/asdf
            call set_globecom_study_period( &
                int(MAX(1,stored_end_year-get_BASE_YEAR()),2))

   
            IF(trim(STR_EXTENSION_PERIOD) /= ' ') THEN
               READ(trim(STR_EXTENSION_PERIOD),*) loc_ext_period
               call set_extension_period(loc_ext_period)
            ELSE
               call set_EXTENSION_PERIOD(int(0,2))
            ENDIF
            IF(COUNTRY_CHR == 'M') THEN
               COUNTRY_CHR = 'C'
            ELSE
               COUNTRY_CHR = 'U'
            ENDIF
!
! SET THE UTILITY ID INFORMATION 10/15/00 MSG
!
            VOID_LOGICAL = S_UTILITY_IDENTIFIER(UTILITY_TYPE_STR, &
                                                  UTILITY_TYPE_CHR)
!
! CALCULATE AND PASS THE LOGICAL FUNCTION PARAMETERS
!
            SAVE_EXTERNAL_PRODUCTION_COST=UTILITY_TYPE_CHR == 'T' .AND. &
                                              UTILITY_TYPE_STR(5:5)=='P'
            ESC_DISP_ADDER_ACTIVE=.NOT.((ESC_DISP_ADDER_CHR == 'N').OR. &
                                        (ESC_DISP_ADDER_CHR == 'n'))
            VOID_LOGICAL = SET_STUDY_PARAMS_LOGICAL(FORECAST_TYPE_CHR, &
                                          SAVE_EXTERNAL_PRODUCTION_COST, &
                                          ESC_DISP_ADDER_ACTIVE)
            VOID_REAL =  SET_HEAT_FACTORS_CLASS_GROWTH(COUNTRY_CHR)

             
            STORED_PROCMETH = &
                       SET_PRODUCTION_METHOD(PRODUCTION_COST_METHOD_CHR)
           
            ! At last call, extension_period set to large negative value.
            if(get_extension_period()<0) then
                ! mergissue
                call end_program("dsex_obj:0008 - extension_period " // &
                "was set to large-tiny value inside SET_PRODUCTION_METHOD " // &
                "routine when that routine has no code to change it.")
            end if
            
            PRODUCTION_PERIODS_IN = 12
            IF(STR_PRODUCTION_PERIODS == 'A') then
                PRODUCTION_PERIODS_IN = 1
            end if
            
            call SET_PRODUCTION_PERIODS(STR_PRODUCTION_PERIODS)
!
! ADDED 3/9/93
!
            IF(REALLY_KEPCO) THEN
               NUMBER_OF_AREAS = 5
            ELSEIF(WABASH_VALLEY) THEN
               NUMBER_OF_AREAS = 6
            ELSE
               NUMBER_OF_AREAS = 6
            ENDIF
! END UTILTY ID
! MOVED 3/17/94. GAT.

            IF(get_EXTENSION_PERIOD() > stored_end_year) THEN
                gc_last_extension_year = &
                       MIN(get_EXTENSION_PERIOD(), &
                                 get_base_year() + MAX_PLANNING_YEARS)
                call set_EXTENSION_PERIOD( &
                    gc_last_extension_year - stored_end_year)
                    
            ELSEIF(get_EXTENSION_PERIOD() >= 1900) THEN
                gc_last_extension_year = get_base_year()
                call set_EXTENSION_PERIOD(int(0,2))
            ELSE
               call set_extension_period(MIN(get_EXTENSION_PERIOD(), &
                                   MAX_PLANNING_YEARS-get_globecom_study_period()))
                
                gc_last_extension_year = stored_end_year + get_EXTENSION_PERIOD()
            ENDIF
!
! 01/04/07 STOP CHECK ON THE EXTENSION PERIOD STARTING BEFORE THE 30TH STUDY YEAR.
! DrG
!
            IF(get_EXTENSION_PERIOD() > 0 .AND. get_globecom_study_period() < 30) THEN
               CALL write_scroll_line_RW( &
                     "In order to use the Extension Period "// &
                     "the Study Period MUST BE 30 years.",5)
               WRITE(IOS_ERROR_MESSAGE,'(A,I2,A)') &
                    "The Study Period length is ",get_globecom_study_period()," years."
               CALL write_scroll_line_RW(IOS_ERROR_MESSAGE,5)

               er_message='Stop requested from Dsex_obj SIID78'
               call end_program(er_message)
            ENDIF
!
! CHECK FOR SHORT FORM FILE
!
            SHORT_FORM_NAME = 'MSG'//trim(SHORT_FORM_NAME)
            SHORT_FORM_FILE_NAME = trim(RESULTS_DIRECTORY)// &
                                         trim(SHORT_FORM_NAME)//'.VRS'
            SHORT_FORM_DES_NAME = trim(SHORT_FORM_NAME)//'.DSS'
            INQUIRE(FILE=SHORT_FORM_FILE_NAME,EXIST=FILE_EXISTS)
         ELSE
            CALL CRITICAL_ERROR_MSG(4) ! MSGPARAM.VAL doesn't exist
         ENDIF
!
! CHECK FOR VALID BASE_FILE_DIRECTORY
!
         cldata%Scename=check_scename()

         CALL VERIFY_BASE_FILE_DIRECTORY(BASE_FILE_DIRECTORY, &
                                         RESULTS_DIRECTORY, &
                                         LDE_FILE_DIRECTORY, &
                                         PRB_FILE_DIRECTORY, &
                                         SHB_FILE_DIRECTORY, &
                                         XML_FILE_DIRECTORY, &
                                         LDG_FILE_DIRECTORY)
!
         END_POINTS = GET_NUM_END_POINTS_IN_OVL_FILE()
!
         IF(PRODUCTION_PERIODS_IN < 1 .OR. PRODUCTION_PERIODS_IN > 12) &
                                              PRODUCTION_PERIODS_IN = 12

         IF(get_base_year() >= stored_end_year) &
                                              CALL CRITICAL_ERROR_MSG(5)
         gc_max_years = MAX_SIMULATION_YEARS + 1

         stored_end_year = get_base_year() + get_globecom_study_period()
         gc_last_study_year = stored_end_year
!
! THE FOLLOWING REPLACES THE CHECK_DISC_SPACE SUBROUTINE
!
!
! PASSED ALL CRITICAL TEST EXCEPT DISC SPACE
!
! DETERMINE LAST VARIABLE
!
         IF(SHORT_FORM_ACTIVE) THEN
            ACTIVE_VARIABLES = SET_SHORT_FORM_VARIABLES( &
                                                   SHORT_FORM_FILE_NAME)
            DES_FILE_NAME = trim(SHORT_FORM_DES_NAME)
         ELSE
            ACTIVE_VARIABLES = get_last_variable() + 1
            DES_FILE_NAME = 'DSSBIFP.DES'
         ENDIF
         CALL SAVE_SHORT_FORM_ACTIVE(SHORT_FORM_ACTIVE)
!
         VOID_CHR1 = CHECK_CAP_PLANNING_SWTCHES()
!
! OPEN WARNING MESSAGE FILE AND CLEAR BEFORE CHECKING DISK SPACE
!
         IF(.NOT. SP_CAPEX_ACTIVE()) &
          CALL DELETE_RESULTS_OF_PREVIOUS_RUN()
!
! OPEN ASCII REPORTS FILES
!
         FILE_NAME=" "
         FILE_NAME = trim(RESULTS_DIRECTORY)//"MSG"// &
             trim(cldata%SCENAME)//".RNS"
       call write_log_entry("dsex:0023", &
       "Opening " // trim(file_name) // "...")
        ! TODO: Resolve carriage control warnings
         OPEN(9,FILE=FILE_NAME,CARRIAGECONTROL="FORTRAN",RECL=132, &
                                                       STATUS='REPLACE')
         WRITE(9,*) ' '
!
         FILE_NAME = trim(RESULTS_DIRECTORY)//"MSG"// &
              trim(cldata%SCENAME)//".ERR"
       call write_log_entry("dsex:0024", &
       "Opening " // trim(file_name) // "...")
         ! TODO: Resolve carriage control warnings
         OPEN(4,FILE=FILE_NAME,CARRIAGECONTROL="FORTRAN", &
                                               STATUS='REPLACE')
         WRITE(4,*) ' '
!
! FIND SIMULATION MODEL NAME AND DATE TIME STAMP
!
         CALL MYNAME(FILE_NAME)
         SYM_SYS_DATE_TIME = DATE_TIME(FILE_NAME)
         RUN_DATE_TIME = SET_RUN_DATE_TIME()
!
! OPEN RESULT FILE AND WRITE HEADER DATA
!
         REC_LENGHT = MAX((ACTIVE_VARIABLES+3)*4,256)
         FILE_NAME = trim(RESULTS_DIRECTORY)//"BIP"// &
            trim(cldata%SCENAME)//".BIN"
         IF(LAHEY_LF95()) THEN
            OPEN(38,FILE=FILE_NAME,ACCESS="DIRECT",STATUS='REPLACE', &
                                                        RECL=REC_LENGHT)
            WRITE(38,REC=1) F7,INT(REC_LENGHT,2), &
                            END_POINTS,get_base_year(), &
                            gc_last_extension_year, &
                            COMPANY_NAME_STR,TITLE_STR,RUN_DATE_TIME, &
                            UTILITY_TYPE_CHR, &
                            STORED_PROCMETH, &
                            FORECAST_TYPE_CHR, &
                            PRODUCTION_PERIODS_IN, &
                            COUNTRY_CHR, &
                            SHORT_FORM_ACTIVE, &
                            DES_FILE_NAME, &
                            ENDING_MONTHS, &
                            ESC_DISP_ADDER_CHR, &
                            SIMULATION_MODEL, &
                            SYM_SYS_DATE_TIME, &
                            ACTIVE_VARIABLES-1
         ELSE
            OPEN(38,FILE=FILE_NAME,ACCESS="DIRECT",STATUS="REPLACE", &
                                                        RECL=REC_LENGHT)
            call close_unit(int(38,2))

            OPEN(38,FILE=FILE_NAME,ACCESS="TRANSPARENT",STATUS="OLD")
            WRITE(38,REC=4) END_POINTS,get_base_year(), &
                            gc_last_extension_year, &
                            COMPANY_NAME_STR,TITLE_STR,RUN_DATE_TIME, &
                            UTILITY_TYPE_CHR, &
                            STORED_PROCMETH, &
                            FORECAST_TYPE_CHR, &
                            PRODUCTION_PERIODS_IN, &
                            COUNTRY_CHR, &
                            SHORT_FORM_ACTIVE, &
                            DES_FILE_NAME, &
                            ENDING_MONTHS, &
                            ESC_DISP_ADDER_CHR, &
                            SIMULATION_MODEL, &
                            SYM_SYS_DATE_TIME, &
                            ACTIVE_VARIABLES-1
            call close_unit(int(38,2))
            OPEN(38,FILE=FILE_NAME,ACCESS="DIRECT",STATUS="OLD", &
                                                        RECL=REC_LENGHT)
         ENDIF
!
! OPEN LOAD PROB FILE
!
         OPEN(8800,FILE="MIDAS880.BIN",ACCESS="DIRECT",STATUS='REPLACE', &
                                           RECL=40+12*LOAD_CURVE_POINTS)
      RETURN

      ENTRY GET_RUN_DATE_TIME

         GET_RUN_DATE_TIME = RUN_DATE_TIME
      RETURN

      ENTRY COMPANY_NAME
         COMPANY_NAME = COMPANY_NAME_STR
      RETURN

      ENTRY TITLE
         TITLE = TITLE_STR
      RETURN

      ENTRY UTILITY_TYPE
         UTILITY_TYPE = UTILITY_TYPE_CHR
      RETURN

      ENTRY FORECAST_TYPE
         FORECAST_TYPE = FORECAST_TYPE_CHR
      RETURN

      ENTRY COUNTRY
         COUNTRY = COUNTRY_CHR
      RETURN

      ENTRY ACTIVE_DATA_DRIVE
         ACTIVE_DATA_DRIVE = RESULTS_DIRECTORY(1:1)//':'
      RETURN

      ENTRY OUTPUT_DIRECTORY
         OUTPUT_DIRECTORY = RESULTS_DIRECTORY
      RETURN

      ENTRY GET_WORKING_FILE_DIRECTORY
         GET_WORKING_FILE_DIRECTORY = WORKING_FILE_DIRECTORY
      RETURN

      ENTRY START_PRODUCTION_COST_METHOD
         START_PRODUCTION_COST_METHOD = PRODUCTION_COST_METHOD_CHR
      RETURN
 9000 CALL CRITICAL_ERROR_MSG(4) ! BAD MSGPARAM.VAL FILE
      END

      FUNCTION DATE_TIME(R_FILE_NAME)
      use logging
      
      use dreptcom
      LOGICAL (kind=1) ::  LAHEY_LF95
      CHARACTER (len=18) ::  DATE_TIME
      CHARACTER (len=18) ::  SYM_SYS_DATE_TIME,S_SYM_SYS_DATE_TIME
      CHARACTER (len=5) ::  MONTH_DAY
      CHARACTER (len=256) ::  R_FILE_NAME,RECLN
      CHARACTER (len=120) ::  SYSTEM_CMD
      LOGICAL (kind=4) ::  FILE_EXISTS
      INTEGER (kind=4) ::  R_EXE_DATE,EXE_TIME,SIZE
      INTEGER (kind=4) ::  VOID_INT4,STORE_SIM_SYSTEM_DATE
      INTEGER (kind=4) ::  HOURS,MINUTES,MONTH,DAY,YEAR,IOS,tagPOS
      CHARACTER (len=9) ::  TIME,DATE
      SAVE R_EXE_DATE,S_SYM_SYS_DATE_TIME
      CHARACTER (len=16) ::  SET_RUN_DATE_TIME
      CHARACTER (len=2) ::  MONTH_STR,DAY_STR, &
                  LEFT_JUSTIFY_I2_IN_STR*15
      CHARACTER (len=8) ::  DAT,STORED_RUN_DATE,STORED_RUN_TIME
      CHARACTER (len=11) ::  CLK

      INTEGER (kind=2) ::  HOUR
      CHARACTER (len=1) ::  STR_PRODUCTION_PERIODS=" ", &
                  START_PRODUCTION_PERIODS
      SAVE STORED_RUN_DATE
!
         CALL FILE_INQUIRE(R_FILE_NAME,FILE_EXISTS, &
                                               R_EXE_DATE,EXE_TIME,SIZE)
         IF(LAHEY_LF95()) THEN
            CALL SEC2DATE(R_EXE_DATE,YEAR,MONTH,DAY) 
            IF(YEAR >= 2000) YEAR = YEAR - 2000
            IF(YEAR >= 1900) YEAR = YEAR - 1900
            TIME = ' '
         ELSE
            HOURS = IAND(63488,EXE_TIME)/2048
            MINUTES = IAND(2016,EXE_TIME)/32
            MONTH = IAND(480,R_EXE_DATE)/32
            DAY = IAND(31,R_EXE_DATE)
            YEAR = IAND(65024,R_EXE_DATE)/512 + 80
            IF(YEAR >= 100) YEAR = YEAR - 100
            IF(HOURS < 12) THEN
               IF(HOURS == 0) HOURS = 12
               WRITE(TIME,'(1X,I2,":",I2.2," am")') HOURS,MINUTES
            ELSE
               IF(HOURS /= 12) HOURS = HOURS - 12
               WRITE(TIME,'(1X,I2,":",I2.2," pm")') HOURS,MINUTES
            ENDIF
         ENDIF
         WRITE(DATE,'(1X,I2.2,"/",I2.2,"/",I2.2)') MONTH,DAY,YEAR
         S_SYM_SYS_DATE_TIME = DATE//TIME
         DATE_TIME = DATE//TIME
         VOID_INT4 = STORE_SIM_SYSTEM_DATE(R_EXE_DATE)
      RETURN

      ENTRY MONTH_DAY(R_FILE_NAME)

         CALL FILE_INQUIRE(R_FILE_NAME,FILE_EXISTS, &
                                               R_EXE_DATE,EXE_TIME,SIZE)
         IF(LAHEY_LF95()) THEN
            CALL SEC2DATE(R_EXE_DATE,YEAR,MONTH,DAY) 
         ELSE
            MONTH = IAND(480,R_EXE_DATE)/32
            DAY = IAND(31,R_EXE_DATE)
         ENDIF
         DAY_STR = LEFT_JUSTIFY_I2_IN_STR(INT(DAY,2))
         MONTH_STR = LEFT_JUSTIFY_I2_IN_STR(INT(MONTH,2))
         MONTH_DAY = trim(MONTH_STR)//"/"//trim(DAY_STR)
      RETURN

      ENTRY SYM_SYS_DATE_TIME

         SYM_SYS_DATE_TIME = S_SYM_SYS_DATE_TIME
      RETURN

      ENTRY SET_RUN_DATE_TIME

         STORED_RUN_DATE = DAT()
         IF(STORED_RUN_DATE(1:1) == '0') STORED_RUN_DATE(1:1) = " "
         STORED_RUN_TIME(1:5) = CLK()
         READ(STORED_RUN_TIME,'(I2)') HOUR
         IF(HOUR < 12) THEN
            IF(HOUR == 0) HOUR = 12
            STORED_RUN_TIME = STORED_RUN_TIME(1:5)//' am'
         ELSE
            IF(HOUR /= 12) HOUR = HOUR - 12
            WRITE(STORED_RUN_TIME(1:2),'(I2)') HOUR
            STORED_RUN_TIME = STORED_RUN_TIME(1:5)//' pm'
         ENDIF
         IF(STORED_RUN_TIME(1:1) == '0') STORED_RUN_TIME(1:1) = " "
         SET_RUN_DATE_TIME = STORED_RUN_DATE//STORED_RUN_TIME
         RUN_DATE = STORED_RUN_DATE
         RUN_TIME = STORED_RUN_TIME
      RETURN

      ENTRY START_PRODUCTION_PERIODS
         START_PRODUCTION_PERIODS = STR_PRODUCTION_PERIODS
      RETURN


      END

      FUNCTION EXE_DATE()
      use logging
      use filename_tracker

!
      INTEGER ::  EXE_DATE,R_EXE_DATE,S_EXE_DATE,STORE_SIM_SYSTEM_DATE
      SAVE S_EXE_DATE
      INTEGER (kind=2) ::  GET_NUM_END_POINTS_IN_OVL_FILE
      INTEGER (kind=2) ::  END_POINTS,DELETE
      INTEGER (kind=4) ::  IOS
      LOGICAL (kind=4) ::  FILE_EXISTS
      CHARACTER (len=256) ::  FILE_NAME,GET_RESULTS_DIRECTORY
      CHARACTER (len=5) ::  GET_SCENAME
!     INTEGER*2 STORED_PERIOD(2,5)=0,PERIOD
      INTEGER (kind=2) ::  BEGIN_END,SEASON,GET_NUM_OF_END_POINTS, &
                LAST_STUDY_ENDPOINT,START_STUDY_ENDPOINT
      SAVE END_POINTS
!
         EXE_DATE = S_EXE_DATE
      RETURN
      ENTRY STORE_SIM_SYSTEM_DATE(R_EXE_DATE)
         S_EXE_DATE = R_EXE_DATE
         STORE_SIM_SYSTEM_DATE = 1234
      RETURN

!      ENTRY PERIOD(BEGIN_END,SEASON)
!         PERIOD = STORED_PERIOD(BEGIN_END,SEASON)
!      RETURN

      ENTRY GET_NUM_END_POINTS_IN_OVL_FILE
         END_POINTS = 1
         FILE_NAME = get_scn_ovl_filename()

         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         IF(FILE_EXISTS) THEN
            OPEN(10,FILE=FILE_NAME,ACCESS='SEQUENTIAL')
            END_POINTS = -9999
            READ(10,'(A)') FILE_NAME
            FILE_NAME = trim(FILE_NAME)//',,,,,,,'
            READ(FILE_NAME,*) DELETE,END_POINTS
            IF(END_POINTS < 1) THEN
               IOS = 0
               DO WHILE (IOS == 0)
                  READ(10,*,IOSTAT=IOS) DELETE,END_POINTS
               ENDDO
            ENDIF
            call close_unit(int(10,2))
            END_POINTS = MAX(END_POINTS,INT(1,2))
            END_POINTS = MIN(END_POINTS, &
              LAST_STUDY_ENDPOINT() - START_STUDY_ENDPOINT() + INT(1,2))
         ENDIF
         GET_NUM_END_POINTS_IN_OVL_FILE = END_POINTS

      RETURN

      ENTRY GET_NUM_OF_END_POINTS

         GET_NUM_OF_END_POINTS = END_POINTS
      RETURN
      END

      FUNCTION CHECK_CAP_PLANNING_SWTCHES()


! GLOBAL VARIABLES

      use logging
      use globecom
      use mod_base_year
      implicit none
      CHARACTER (len=256) ::  FILE_NAME
      CHARACTER (len=40) ::  REPORT_TITLE
      CHARACTER (len=1) ::  TRIGGER_METHOD_VALUE='X', &
                  PLANNING_TRIGGER, &
                  CAPACITY_PLANNING_SWTCHES, &
                  CHECK_CAP_PLANNING_SWTCHES
      CHARACTER (len=2) ::  CAPACITY_PLANNING_METHOD, &
                  CAPACITY_PLANNING_METHOD_VALUE='XX', &
                  GREEN_MRX_METHOD, &
                  GREEN_MRX_METHOD_VALUE='NO'
      CHARACTER (len=60) ::  CAPACITY_PLANNING_METHOD_STR,TEMP_STR_60
      INTEGER (kind=4) ::  IOS
      INTEGER (kind=2) ::  DELETE
      INTEGER (kind=2) ::  VOID_INT2,SET_INTEGER_PLANNING_SWITCHES
      INTEGER (kind=2) ::  OPTIM_DEPTH_VALUE=1, &
                OPTIM_DEPTH, &
                MAX_PLANS_TO_SAVE_VALUE=1, &
                MAX_PLANS_TO_SAVE, &
                ISERROR
      CHARACTER (len=4) ::  STR_MIN_MAX_VALUE='MIN ', &
                  OBJ_FUNCTION_MIN_MAX
      LOGICAL (kind=1) ::  CAPACITY_PLANNING_ACTIVE
      CHARACTER (len=50) ::  OBJ_FUNCTION_DESC, &
                   OBJ_FUNCTION_DESC_STR=" "
      CHARACTER (len=256) :: OBJ_FUNCTION_FILE_NAME_STR=" ", &
                    OBJ_FUNCTION_FILE_NAME
      CHARACTER (len=8) ::  PEAK_GROWTH_RATE_METHOD_VALUE='Forecast', &
                  PEAK_GROWTH_RATE_METHOD
!
! LOCAL VARIABLES
!
      CHARACTER (len=128) ::  SWITCH_LINE
      CHARACTER (len=1) ::  AN_DECOMP_DEBUG_STR,AN_DECOMP_DEBUG_SWITCH
      SAVE AN_DECOMP_DEBUG_STR
!
! FUNCTION TYPES
!
      CHARACTER (len=1) ::  FORECAST_TYPE,COUNTRY,UTILITY_TYPE
!
!         CALL FINDFILE('MSGPLSWT.DAT',FILE_NAME)
         OPEN(10,FILE='MSGPLSWT.DAT',ACCESS="SEQUENTIAL",STATUS="OLD", &
                                                             IOSTAT=IOS)
         IF(IOS /= 0) THEN
            REPORT_TITLE = 'Planning not active.'
            CAPACITY_PLANNING_METHOD_VALUE = 'RE'
            TRIGGER_METHOD_VALUE = 'R'
            OPTIM_DEPTH_VALUE = 1
            STR_MIN_MAX_VALUE = 'MIN'
            MAX_PLANS_TO_SAVE_VALUE = 1
            gc_last_extension_year = stored_end_year

            OBJ_FUNCTION_FILE_NAME_STR = 'NONE'
            OBJ_FUNCTION_DESC_STR = 'Default PV Revenues at 10%'
         ELSE

            READ(10,*)
            DO
               READ(10,"(A)") SWITCH_LINE
               SWITCH_LINE = trim(SWITCH_LINE)//',,,,,,,,,,,,,,,,,'
               READ(SWITCH_LINE,*,IOSTAT=IOS) DELETE, &
                                   TEMP_STR_60, &
                                   TRIGGER_METHOD_VALUE, &
                                   OPTIM_DEPTH_VALUE, &
                                   STR_MIN_MAX_VALUE, &
                                   MAX_PLANS_TO_SAVE_VALUE, &
                                   OBJ_FUNCTION_FILE_NAME_STR, &
                                   OBJ_FUNCTION_DESC_STR, &
                                   AN_DECOMP_DEBUG_STR
               IF(IOS /=0) EXIT
               IF(DELETE < 8) EXIT
            ENDDO
            call close_unit(int(10,2))
            CALL UPC(TEMP_STR_60,CAPACITY_PLANNING_METHOD_STR)
            CAPACITY_PLANNING_METHOD_VALUE = &
                                       CAPACITY_PLANNING_METHOD_STR(1:2)
! 010609. GREATLY SIMPLIFIES BRANCHING
            IF(CAPACITY_PLANNING_METHOD_VALUE == 'GX') THEN
               GREEN_MRX_METHOD_VALUE = 'GX'
               CAPACITY_PLANNING_METHOD_VALUE = 'MX'
            ELSE
               GREEN_MRX_METHOD_VALUE = 'NO'
            ENDIF
!
            PEAK_GROWTH_RATE_METHOD_VALUE = 'Forecast'
            IF(CAPACITY_PLANNING_METHOD_VALUE == 'NE') THEN
               IF(INDEX(CAPACITY_PLANNING_METHOD_STR,'FILE') /= 0) &
                                PEAK_GROWTH_RATE_METHOD_VALUE = 'Ratios'
               IF(INDEX(CAPACITY_PLANNING_METHOD_STR,'AVERAGE') /= 0) &
                               PEAK_GROWTH_RATE_METHOD_VALUE = 'Average'
            ENDIF

         ENDIF
         IF (get_globecom_study_period()==0) then
            call end_program("dsex_obj:0011 - get_globecom_study_period() is zero.")
         end if
         
         VOID_INT2 =  SET_INTEGER_PLANNING_SWITCHES(get_globecom_study_period(), &
                                                OPTIM_DEPTH_VALUE, &
                                                MAX_PLANS_TO_SAVE_VALUE, &
                                            get_EXTENSION_PERIOD(), &
                                                stored_end_year, &
                                                get_base_year())
         CHECK_CAP_PLANNING_SWTCHES = CAPACITY_PLANNING_METHOD_VALUE
      RETURN
!


      ENTRY AN_DECOMP_DEBUG_SWITCH

         AN_DECOMP_DEBUG_SWITCH = AN_DECOMP_DEBUG_STR
      RETURN

      ENTRY CAPACITY_PLANNING_SWTCHES

         CAPACITY_PLANNING_SWTCHES = CAPACITY_PLANNING_METHOD_VALUE
      RETURN

      ENTRY PEAK_GROWTH_RATE_METHOD

         PEAK_GROWTH_RATE_METHOD = PEAK_GROWTH_RATE_METHOD_VALUE
      RETURN

      ENTRY GREEN_MRX_METHOD

         GREEN_MRX_METHOD = GREEN_MRX_METHOD_VALUE
      RETURN

      ENTRY CAPACITY_PLANNING_METHOD

         CAPACITY_PLANNING_METHOD = CAPACITY_PLANNING_METHOD_VALUE
      RETURN

      ENTRY OBJ_FUNCTION_FILE_NAME

         OBJ_FUNCTION_FILE_NAME = OBJ_FUNCTION_FILE_NAME_STR
      RETURN

      ENTRY OBJ_FUNCTION_DESC

         OBJ_FUNCTION_DESC = OBJ_FUNCTION_DESC_STR
      RETURN

      ENTRY OBJ_FUNCTION_MIN_MAX

         OBJ_FUNCTION_MIN_MAX = STR_MIN_MAX_VALUE
      RETURN

!     ENTRY PLANNING_TRIGGER ! THIS VALUE IS SPECIFIED IN RUN SPECS

!        PLANNING_TRIGGER = TRIGGER_METHOD_VALUE
!     RETURN
      END

      FUNCTION SET_INTEGER_PLANNING_SWITCHES(R_STUDY_PERIOD, &
                                             R_OPTIM_DEPTH_VALUE, &
                                             R_MAX_PLANS_TO_SAVE_VALUE, &
                                             R_EXTENSION_PERIOD, &
                                             R_STORED_END_YEAR, &
                                             R_STORED_BASE_YEAR)

!
      use logging
      use mod_base_year
      use globecom
      INTEGER (kind=2) ::  SET_INTEGER_PLANNING_SWITCHES
      INTEGER (kind=2) ::  EXTENSION_PERIOD_START
      INTEGER (kind=2) ::  RUN_YEARS,EXTENSION_YEARS,EXTENSION_PERIOD

      INTEGER (kind=2) ::  get_endyr
      INTEGER (kind=2) ::  OPTIM_DEPTH,OPTIM_DEPTH_VALUE
      INTEGER (kind=2) ::  MAX_PLANS_TO_SAVE,MAX_PLANS_TO_SAVE_VALUE
      INTEGER (kind=2) ::  R_STUDY_PERIOD, &
                R_OPTIM_DEPTH_VALUE, &
                R_MAX_PLANS_TO_SAVE_VALUE, &
                R_EXTENSION_PERIOD, &
                R_STORED_END_YEAR, &
                R_STORED_BASE_YEAR
      SAVE OPTIM_DEPTH_VALUE,MAX_PLANS_TO_SAVE_VALUE, &
           EXTENSION_PERIOD
           
           
         call set_globecom_study_period(int(R_STUDY_PERIOD,2))

         
         OPTIM_DEPTH_VALUE = R_OPTIM_DEPTH_VALUE
         MAX_PLANS_TO_SAVE_VALUE = R_MAX_PLANS_TO_SAVE_VALUE
         EXTENSION_PERIOD = R_EXTENSION_PERIOD
         stored_end_year = R_STORED_END_YEAR
         call set_base_year(R_STORED_BASE_YEAR)
         
         
      RETURN

      ENTRY EXTENSION_PERIOD_START
         EXTENSION_PERIOD_START = MIN(31,get_globecom_study_period()+1)
         EXTENSION_PERIOD_START = get_globecom_study_period()+1  ! 11/09/06
      RETURN

      ENTRY OPTIM_DEPTH

         OPTIM_DEPTH = OPTIM_DEPTH_VALUE
      RETURN

      ENTRY MAX_PLANS_TO_SAVE

         MAX_PLANS_TO_SAVE = MAX_PLANS_TO_SAVE_VALUE
      RETURN



      ENTRY get_endyr

         get_endyr = stored_end_year
      RETURN

      ENTRY RUN_YEARS

         RUN_YEARS = get_globecom_study_period()
      RETURN

      ENTRY EXTENSION_YEARS()

         EXTENSION_YEARS = EXTENSION_PERIOD
      RETURN
      END

      FUNCTION SET_STUDY_PARAMS_LOGICAL(R_FORECAST_TYPE_CHR, &
                                        R_SAVE_EXTERNAL_PRODUCTION_COST, &
                                        R_ESC_DISP_ADDER_ACTIVE)

!
      use logging
      use forecast
      use mod_base_year
      implicit none
      LOGICAL (kind=1) ::  SET_STUDY_PARAMS_LOGICAL
      LOGICAL (kind=1) ::  SET_RUN_FUTURE_ASSETS, &
                DETAIL_ANNUAL_DECOMP_REPORT, &
                FIXED_COSTS_IN_ADJ_CLAUSE
      LOGICAL (kind=1) ::  &
                NERC_REGION_BASED_FORECAST, &
                POOLING_TRANSACTIONS, &
                CONTROL_AREA_FORECAST, &
                CLASS_BASED_FORECAST
      LOGICAL (kind=1) ::  TURN_OFF_SYSTEM_BASE_FORECAST
      LOGICAL (kind=1) ::  ESC_DISP_ADDER,SHORT_FORM, &
                USE_EXTERNAL_PRODUCTION_COST
      CHARACTER (len=1) ::  R_FORECAST_TYPE_CHR, &
                  AN_DECOMP_DEBUG_SWITCH
      LOGICAL (kind=1) ::  MIN_MAX_OBJECTIVE_FUNC
      LOGICAL (kind=1) ::  R_SAVE_EXTERNAL_PRODUCTION_COST, &
                R_ESC_DISP_ADDER_ACTIVE
      LOGICAL (kind=1) ::  LAST_ELDC_REPORT,MARGINAL_COST_SWITCH
      CHARACTER (len=1) ::  UTILITY_TYPE
      CHARACTER (len=4) ::  OBJ_FUNCTION_MIN_MAX
      INTEGER (kind=2) ::  GET_NUM_OF_END_POINTS
!
! SAVED VARAIABLES THAT MAKE THE FUNCTIONS WORK
!
      LOGICAL (kind=1) ::  SHORT_FORM_ACTIVE=.FALSE., &
                ESC_DISP_ADDER_ACTIVE, &
                SAVE_EXTERNAL_PRODUCTION_COST=.FALSE.
      SAVE ESC_DISP_ADDER_ACTIVE
!
        FORECAST_TYPE_CHR = R_FORECAST_TYPE_CHR
        SAVE_EXTERNAL_PRODUCTION_COST = R_SAVE_EXTERNAL_PRODUCTION_COST
        ESC_DISP_ADDER_ACTIVE = R_ESC_DISP_ADDER_ACTIVE
        SET_STUDY_PARAMS_LOGICAL = .TRUE.
      RETURN

      ENTRY USE_EXTERNAL_PRODUCTION_COST

         USE_EXTERNAL_PRODUCTION_COST = SAVE_EXTERNAL_PRODUCTION_COST
      RETURN

      ENTRY SHORT_FORM

         SHORT_FORM = SHORT_FORM_ACTIVE ! .FALSE.
      RETURN

      ENTRY ESC_DISP_ADDER

         ESC_DISP_ADDER = ESC_DISP_ADDER_ACTIVE
      RETURN

      ENTRY MIN_MAX_OBJECTIVE_FUNC

         MIN_MAX_OBJECTIVE_FUNC=INDEX(OBJ_FUNCTION_MIN_MAX(),'MIN') /= 0
      RETURN

      ENTRY FIXED_COSTS_IN_ADJ_CLAUSE
         FIXED_COSTS_IN_ADJ_CLAUSE = .TRUE.
      RETURN



      ENTRY TURN_OFF_SYSTEM_BASE_FORECAST

         SYSTEM_FORECAST_FILE_IN_USE = .FALSE.
         TURN_OFF_SYSTEM_BASE_FORECAST = .FALSE.
      RETURN

      ENTRY NERC_REGION_BASED_FORECAST

         NERC_REGION_BASED_FORECAST = ( &
               (FORECAST_TYPE_CHR == 'S' .OR. &
                              FORECAST_TYPE_CHR == 'A') .AND. &
                                      .NOT. SYSTEM_FORECAST_FILE_IN_USE)
      RETURN

      ENTRY POOLING_TRANSACTIONS

         POOLING_TRANSACTIONS  = (FORECAST_TYPE_CHR == 'P')
      RETURN

      ENTRY CONTROL_AREA_FORECAST

         CONTROL_AREA_FORECAST  = (FORECAST_TYPE_CHR == 'A')
      RETURN

      ENTRY CLASS_BASED_FORECAST

         CLASS_BASED_FORECAST = (FORECAST_TYPE_CHR == 'C')
      RETURN

      ENTRY DETAIL_ANNUAL_DECOMP_REPORT

         DETAIL_ANNUAL_DECOMP_REPORT = AN_DECOMP_DEBUG_SWITCH() == 'Y'
      RETURN

      ENTRY SET_RUN_FUTURE_ASSETS

         SET_RUN_FUTURE_ASSETS = GET_NUM_OF_END_POINTS() > 1
      RETURN

      ENTRY LAST_ELDC_REPORT ! TEMPORARY UNTIL WE PUT INTO RUN SPEC'S

         LAST_ELDC_REPORT = UTILITY_TYPE() == 'T' .AND. &
                            MARGINAL_COST_SWITCH()
      RETURN
      END

      FUNCTION SET_HEAT_FACTORS_CLASS_GROWTH(COUNTRY_CHR)

!
      use logging
      CHARACTER (len=1) ::  COUNTRY_CHR
      REAL ::  GET_COST_CONVERSION,COST_CONVERSION, &
           GET_HEAT_CONVERSION,HEAT_CONVERSION, &
           GET_TONS_CONVERSION,TONS_CONVERSION
      REAL ::  SET_HEAT_FACTORS_CLASS_GROWTH
      REAL ::  SYS_GROWTH_TRIGGER,SYS_GROWTH_TRIGGER_VALUE=20.
      REAL ::  CLASS_GROWTH_TRIGGER,CLASS_GROWTH_TRIGGER_VALUE=20.
      LOGICAL (kind=1) ::  ORLANDO_UC,SALT_RIVER_PROJECT
      SAVE COST_CONVERSION, &
           HEAT_CONVERSION, &
           TONS_CONVERSION
!
         SET_HEAT_FACTORS_CLASS_GROWTH = 0.
!
! SET CONVERSION FACTORS
!
         IF(COUNTRY_CHR == 'C') THEN
            COST_CONVERSION = 10.
            HEAT_CONVERSION = 1.
            TONS_CONVERSION = 1000.  ! KG/TON METRIC
         ELSE
            COST_CONVERSION = .01
            HEAT_CONVERSION = 1000.
            TONS_CONVERSION = 2000.   ! LBS/TON
         ENDIF
!
! LOWER CLASS GROWTH RATES FOR SMALLER COMPANYS
!
         IF(ORLANDO_UC()) THEN
            CLASS_GROWTH_TRIGGER_VALUE = 4.
          ELSEIF(SALT_RIVER_PROJECT()) THEN
            CLASS_GROWTH_TRIGGER_VALUE = 4.
          ENDIF
      RETURN
!
      ENTRY GET_COST_CONVERSION
         GET_COST_CONVERSION = COST_CONVERSION
      RETURN

      ENTRY GET_HEAT_CONVERSION

         GET_HEAT_CONVERSION = HEAT_CONVERSION
      RETURN

      ENTRY GET_TONS_CONVERSION

         GET_TONS_CONVERSION = TONS_CONVERSION
      RETURN

      ENTRY SYS_GROWTH_TRIGGER

         SYS_GROWTH_TRIGGER = SYS_GROWTH_TRIGGER_VALUE
      RETURN

      ENTRY CLASS_GROWTH_TRIGGER

         CLASS_GROWTH_TRIGGER = CLASS_GROWTH_TRIGGER_VALUE
      RETURN
      END

      LOGICAL (kind=1)   FUNCTION S_UTILITY_IDENTIFIER(UTILITY_TYPE_STR, &
                                              UTILITY_TYPE_CHR)

! end of planning switch information
      USE spindriftlib
      USE prod_arrays_dimensions
      use kepcocom
      use sizecom
      use msgoutpt
      implicit none
      
      LOGICAL (kind=1) ::  SALT_RIVER_ACTIVE=.FALSE.
      LOGICAL (kind=1) ::  SALT_RIVER_PROJECT, &
                ECITY_ACTIVE=.FALSE.,ECITY_COMPANY, &
                DUKE_ACTIVE=.FALSE.,DUKE, &
                IMPA,IMPA_ACTIVE=.FALSE., &
                CIPSCO_ACTIVE=.FALSE.,CIPSCO, &
                SOUTHERN_COMPANY_ACTIVE=.FALSE.,SOUTHERN_COMPANY, &
                KCPL_ACTIVE=.FALSE.,KCPL, &
                LGE_ACTIVE=.FALSE.,LGandE, &
                AMEREN_ACTIVE=.FALSE.,AMEREN, &
                PACIFICORP_ACTIVE=.FALSE.,PACIFICORP, &
                FirstEnergy_ACTIVE=.FALSE.,FirstEnergy, &
                ODEC_ACTIVE=.FALSE.,ODEC
      LOGICAL (kind=1) ::  IP_ACTIVE=.FALSE.,UTILITY_IS_IP
      LOGICAL (kind=1) ::  ST_JOE_ACTIVE=.FALSE.,ST_JOE
      LOGICAL (kind=1) ::  OUC_ACTIVE=.FALSE.,ORLANDO_UC
      LOGICAL (kind=1) ::  EMPIRE_ACTIVE=.FALSE.,EMPIRE
      LOGICAL (kind=1) ::  UI_ACTIVE=.FALSE.,UI
      LOGICAL (kind=1) ::  BANGOR_ACTIVE=.FALSE.,BANGOR
      LOGICAL (kind=1) ::  APS_DQE_ACTIVE=.FALSE.,APS_DQE, &
                CHANGE_CPL_STATUS_TO_ACTIVE,TEMPL1
      LOGICAL (kind=1) ::  CCN_ACTIVE=.FALSE.,CCN
      CHARACTER (len=10) ::  UTILITY_TYPE_STR,TEMP_STR_10
      CHARACTER (len=1) ::  UTILITY_TYPE_CHR
      logical (kind=1) :: wkp_active
      LOGICAL (kind=1) ::  MPS_ACTIVE=.FALSE.,MPS
      LOGICAL (kind=1) ::  WVPA,WABASH_VALLEY_FIN=.FALSE.
      LOGICAL (kind=1) ::  IPALCO,IPALCO_ACTIVE=.FALSE.
      LOGICAL (kind=1) ::  GreatRiver,GRE_ACTIVE=.FALSE.
!
         WABASH_VALLEY = .FALSE.
         REALLY_KEPCO = .FALSE.
         TEMP_STR_10 = UTILITY_TYPE_STR
         CALL UPC(TEMP_STR_10,UTILITY_TYPE_STR)
         IF(INDEX(UTILITY_TYPE_STR,'WABASH') /= 0) THEN
            WABASH_VALLEY = .TRUE.
            WABASH_VALLEY_FIN = .TRUE.
            UTILITY_TYPE_CHR = 'R'
         ELSEIF(INDEX(UTILITY_TYPE_STR,'GRE') /= 0) THEN
            GRE_ACTIVE = .TRUE.
            UTILITY_TYPE_CHR = 'R'
         ELSEIF(INDEX(UTILITY_TYPE_STR,'SALT') /= 0 .OR. &
                                INDEX(UTILITY_TYPE_STR,'SRP') /= 0) THEN
            SALT_RIVER_ACTIVE = .TRUE.
            UTILITY_TYPE_CHR = 'R'
         ELSEIF(INDEX(UTILITY_TYPE_STR,'SOUT') /= 0) THEN
             UTILITY_TYPE_CHR = 'I'
            SOUTHERN_COMPANY_ACTIVE = .TRUE.
            CALL TELL_DISPATCH_SOUTHERN_STATUS
         ELSEIF(INDEX(UTILITY_TYPE_STR,'STJP&L') /= 0) THEN
             UTILITY_TYPE_CHR = 'I'
            ST_JOE_ACTIVE = .TRUE.
         ELSEIF(INDEX(UTILITY_TYPE_STR,'KCP&L') /= 0) THEN
             UTILITY_TYPE_CHR = 'I'
            KCPL_ACTIVE = .TRUE.
         ELSEIF(INDEX(UTILITY_TYPE_STR,'LG&E') /= 0) THEN
             UTILITY_TYPE_CHR = 'I'
            LGE_ACTIVE = .TRUE.
         ELSEIF(INDEX(UTILITY_TYPE_STR,'FIRSTE') /= 0) THEN
             UTILITY_TYPE_CHR = 'I'
            FirstEnergy_ACTIVE = .TRUE.
         ELSEIF(INDEX(UTILITY_TYPE_STR,'EMPIRE') /= 0) THEN
             UTILITY_TYPE_CHR = 'I'
            EMPIRE_ACTIVE = .TRUE.
         ELSEIF(INDEX(UTILITY_TYPE_STR,'UI') /= 0) THEN
             UTILITY_TYPE_CHR = 'I'
            UI_ACTIVE = .TRUE.
         ELSEIF(INDEX(UTILITY_TYPE_STR,'BGR') /= 0) THEN
             UTILITY_TYPE_CHR = 'I'
            BANGOR_ACTIVE = .TRUE.
         ELSEIF(INDEX(UTILITY_TYPE_STR,'MPS') /= 0) THEN
             UTILITY_TYPE_CHR = 'I'
            MPS_ACTIVE = .TRUE.
         ELSEIF(INDEX(UTILITY_TYPE_STR,'APS') /= 0) THEN
             UTILITY_TYPE_CHR = 'I'
            APS_DQE_ACTIVE = .TRUE.
         ELSEIF(INDEX(UTILITY_TYPE_STR,'IPALCO') /= 0 .OR. &
                                INDEX(UTILITY_TYPE_STR,'IPL') /= 0) THEN
             UTILITY_TYPE_CHR = 'I'
            IPALCO_ACTIVE = .TRUE.
         ELSEIF(INDEX(UTILITY_TYPE_STR,'IP') /= 0) THEN
             UTILITY_TYPE_CHR = 'I'
            IP_ACTIVE = .TRUE.
         ELSEIF(INDEX(UTILITY_TYPE_STR,'CP&L') /= 0) THEN
             UTILITY_TYPE_CHR = 'I'
            TEMPL1 = CHANGE_CPL_STATUS_TO_ACTIVE()
         ELSEIF(INDEX(UTILITY_TYPE_STR,'CCN') /= 0) THEN
             UTILITY_TYPE_CHR = 'I'
            CCN_ACTIVE = .TRUE.
         ELSEIF(INDEX(UTILITY_TYPE_STR,'OUC') /= 0) THEN
             UTILITY_TYPE_CHR = 'X'
            OUC_ACTIVE = .TRUE.
         ELSEIF(INDEX(UTILITY_TYPE_STR,'CIPS') /= 0) THEN
             UTILITY_TYPE_CHR = 'I'
            CIPSCO_ACTIVE = .TRUE.
         ELSEIF(INDEX(UTILITY_TYPE_STR,'AMEREN') /= 0) THEN
             UTILITY_TYPE_CHR = 'I'
            AMEREN_ACTIVE = .TRUE.
         ELSEIF(INDEX(UTILITY_TYPE_STR,'PACIFI') /= 0) THEN
             UTILITY_TYPE_CHR = 'I'
            PACIFICORP_ACTIVE = .TRUE.
         ELSEIF(INDEX(UTILITY_TYPE_STR,'DUKE') /= 0) THEN
             UTILITY_TYPE_CHR = 'I'
            DUKE_ACTIVE = .TRUE.
         ELSEIF(INDEX(UTILITY_TYPE_STR,'IMPA') /= 0) THEN
             UTILITY_TYPE_CHR = 'I'
            IMPA_ACTIVE = .TRUE.
         ELSEIF(INDEX(UTILITY_TYPE_STR,'ECITY') /= 0) THEN
             UTILITY_TYPE_CHR = 'I'
            ECITY_ACTIVE = .TRUE.
         ELSEIF(INDEX(UTILITY_TYPE_STR,'KEPCO') /= 0) THEN
             UTILITY_TYPE_CHR = 'R'
            REALLY_KEPCO = .TRUE.
         ELSEIF(INDEX(UTILITY_TYPE_STR,'ODEC') /= 0) THEN
             UTILITY_TYPE_CHR = 'I'
            ODEC_ACTIVE = .TRUE.
         ELSE
             UTILITY_TYPE_CHR = UTILITY_TYPE_STR(1:1)
         ENDIF
         IF(UTILITY_TYPE_CHR == 'M') UTILITY_TYPE_CHR = 'P'
         WKP_ACTIVE = STORE_WKP_ACTIVE(UTILITY_TYPE_CHR, WKP_ACTIVE)
      RETURN

      ENTRY ODEC

         ODEC = ODEC_ACTIVE
      RETURN

      ENTRY IPALCO

         IPALCO = IPALCO_ACTIVE
      RETURN

      ENTRY GreatRiver

         GreatRiver = GRE_ACTIVE
      RETURN

      ENTRY WVPA
         WVPA = WABASH_VALLEY_FIN
      RETURN

      ENTRY AMEREN

         AMEREN = AMEREN_ACTIVE
      RETURN

      ENTRY PACIFICORP

         PACIFICORP = PACIFICORP_ACTIVE
      RETURN

      ENTRY CIPSCO

         CIPSCO = CIPSCO_ACTIVE
      RETURN

      ENTRY DUKE

         DUKE = DUKE_ACTIVE
      RETURN

      ENTRY IMPA

         IMPA = IMPA_ACTIVE
      RETURN

      ENTRY ECITY_COMPANY

         ECITY_COMPANY = ECITY_ACTIVE
      RETURN

      ENTRY UTILITY_IS_IP

         UTILITY_IS_IP = IP_ACTIVE
      RETURN

      ENTRY SOUTHERN_COMPANY

         SOUTHERN_COMPANY = SOUTHERN_COMPANY_ACTIVE
      RETURN

      ENTRY KCPL

         KCPL = KCPL_ACTIVE
      RETURN

      ENTRY LGandE

         LGandE = LGE_ACTIVE
      RETURN

      ENTRY FirstEnergy

         FirstEnergy = FirstEnergy_ACTIVE
      RETURN

      ENTRY SALT_RIVER_PROJECT

         SALT_RIVER_PROJECT = SALT_RIVER_ACTIVE
      RETURN

      ENTRY APS_DQE

         APS_DQE = APS_DQE_ACTIVE
      RETURN

      ENTRY ST_JOE

         ST_JOE = ST_JOE_ACTIVE
      RETURN

      ENTRY EMPIRE

         EMPIRE = EMPIRE_ACTIVE
      RETURN

      ENTRY UI

         UI = .true. ! UI_ACTIVE
      RETURN

      ENTRY BANGOR
         BANGOR = BANGOR_ACTIVE
      RETURN

      ENTRY MPS
         MPS = MPS_ACTIVE
      RETURN

      ENTRY ORLANDO_UC
         ORLANDO_UC = OUC_ACTIVE
      RETURN

      ENTRY CCN
         CCN = CCN_ACTIVE
      RETURN
      END

!
! MOVED FROM MIDAS.FOR 8/3/93
!
      SUBROUTINE VERIFY_BASE_FILE_DIRECTORY(BASE_FILE_DIRECTORY, &
                                            RESULTS_DIRECTORY, &
                                            LDE_FILE_DIRECTORY, &
                                            PRB_FILE_DIRECTORY, &
                                            SHB_FILE_DIRECTORY, &
                                            XML_FILE_DIRECTORY, &
                                            LDG_FILE_DIRECTORY)

!
      use logging
      use dsex_data
      USE spindriftlib
      USE prod_arrays_dimensions
      CHARACTER (len=*) :: BASE_FILE_DIRECTORY,RESULTS_DIRECTORY
      CHARACTER (len=2) ::  BASE_FILE_DRIVE
      CHARACTER (len=2) ::  LDE_FILE_DRIVE,PRB_FILE_DRIVE, &
                  SHB_FILE_DRIVE,XML_FILE_DRIVE,LDG_FILE_DRIVE
      CHARACTER (len=256) ::  FILE_NAME,SEARCH_FILE
      CHARACTER (len=256) ::  RETURN_CHAR,STORE_BASE_FILE_DIRECTORY, &
                                         LDE_FILE_DIRECTORY, &
                                         LDG_FILE_DIRECTORY, &
                                         PRB_FILE_DIRECTORY, &
                                         SHB_FILE_DIRECTORY, &
                                         XML_FILE_DIRECTORY
!
      INTEGER (kind=2) ::  I
      INTEGER (kind=2) ::  ISERROR,ERROR
      INTEGER (kind=4) ::  IOS
!
! BASE FILE DIRECTORY SET-UP
!
      cldata%SCENAME=check_scename()
! TODO: Resolve `NBLANK is not standard-conforming` warnings.
      I = NBLANK(BASE_FILE_DIRECTORY)
      IF(I == 0) THEN
         BASE_FILE_DIRECTORY = RESULTS_DIRECTORY
         I = NBLANK(BASE_FILE_DIRECTORY)
      ELSEIF(I==1 .AND. BASE_FILE_DIRECTORY(1:1) /= '\') THEN
         BASE_FILE_DIRECTORY = trim(BASE_FILE_DIRECTORY)//':'
         I = 2
      ENDIF
      IF(BASE_FILE_DIRECTORY(I:I) /= '\') THEN
         BASE_FILE_DIRECTORY = trim(BASE_FILE_DIRECTORY)//'\'
      ELSE
         I = I - 1
      ENDIF
      OPEN(10,FILE=trim(BASE_FILE_DIRECTORY)//'TESTDIR',IOSTAT=IOS)
      IF(IOS /= 0) THEN
         BASE_FILE_DIRECTORY = RESULTS_DIRECTORY
      ELSE
         close(10,STATUS='DELETE')
      ENDIF

      I = NBLANK(BASE_FILE_DIRECTORY)
      IF(BASE_FILE_DIRECTORY(I:I) /= '\') &
                  BASE_FILE_DIRECTORY = trim(BASE_FILE_DIRECTORY)//'\'
      IF(INDEX(BASE_FILE_DIRECTORY,':') == 2) THEN
         BASE_FILE_DRIVE = BASE_FILE_DIRECTORY(1:2)
      ELSE
         BASE_FILE_DRIVE = RESULTS_DIRECTORY(1:1)//':'
         BASE_FILE_DIRECTORY = BASE_FILE_DRIVE//BASE_FILE_DIRECTORY
      ENDIF
!
! LDE FILE DIRECTORY SET-UP
!
      IF(trim(LDE_FILE_DIRECTORY) == ' ') THEN
         LDE_FILE_DIRECTORY = RESULTS_DIRECTORY
      ELSE
         I = NBLANK(LDE_FILE_DIRECTORY)
         IF(I == 0 .OR. I == 256) THEN
            LDE_FILE_DIRECTORY = RESULTS_DIRECTORY
            I = NBLANK(LDE_FILE_DIRECTORY)
         ELSEIF(I==1 .AND. LDE_FILE_DIRECTORY(1:1) /= '\') THEN
            LDE_FILE_DIRECTORY = trim(LDE_FILE_DIRECTORY)//':'
            I = 2
         ENDIF
         IF(LDE_FILE_DIRECTORY(I:I) /= '\') THEN
            LDE_FILE_DIRECTORY = trim(LDE_FILE_DIRECTORY)//'\'
         ELSE
            I = I - 1
         ENDIF
         OPEN(10,FILE=trim(LDE_FILE_DIRECTORY)//'TESTDIR',IOSTAT=IOS)
         IF(IOS > 0) THEN
            LDE_FILE_DIRECTORY = RESULTS_DIRECTORY
         ELSE
            CLOSE(10,STATUS='DELETE')
         ENDIF

         I = NBLANK(LDE_FILE_DIRECTORY)
         IF(LDE_FILE_DIRECTORY(I:I) /= '\') &
                    LDE_FILE_DIRECTORY = trim(LDE_FILE_DIRECTORY)//'\'
         IF(INDEX(LDE_FILE_DIRECTORY,':') == 2) THEN
            LDE_FILE_DRIVE = LDE_FILE_DIRECTORY(1:2)
         ELSE
            LDE_FILE_DRIVE = RESULTS_DIRECTORY(1:1)//':'
            LDE_FILE_DIRECTORY = LDE_FILE_DRIVE//LDE_FILE_DIRECTORY
         ENDIF
      ENDIF
!
! LDE FILE DIRECTORY SET-UP
!
      IF(trim(LDG_FILE_DIRECTORY) == ' ') THEN
         LDG_FILE_DIRECTORY = RESULTS_DIRECTORY
      ELSE
         I = NBLANK(LDG_FILE_DIRECTORY)
         IF(I == 0 .OR. I == 256) THEN
            LDG_FILE_DIRECTORY = RESULTS_DIRECTORY
            I = NBLANK(LDG_FILE_DIRECTORY)
         ELSEIF(I==1 .AND. LDG_FILE_DIRECTORY(1:1) /= '\') THEN
            LDG_FILE_DIRECTORY = trim(LDG_FILE_DIRECTORY)//':'
            I = 2
         ENDIF
         IF(LDG_FILE_DIRECTORY(I:I) /= '\') THEN
            LDG_FILE_DIRECTORY = trim(LDG_FILE_DIRECTORY)//'\'
         ELSE
            I = I - 1
         ENDIF
         OPEN(10,FILE=trim(LDG_FILE_DIRECTORY)//'TESTDIR',IOSTAT=IOS)
         IF(IOS > 0) THEN
            LDG_FILE_DIRECTORY = RESULTS_DIRECTORY
         ELSE
            CLOSE(10,STATUS='DELETE')
         ENDIF

         I = NBLANK(LDG_FILE_DIRECTORY)
         IF(LDG_FILE_DIRECTORY(I:I) /= '\') &
                    LDG_FILE_DIRECTORY = trim(LDG_FILE_DIRECTORY)//'\'
         IF(INDEX(LDG_FILE_DIRECTORY,':') == 2) THEN
            LDG_FILE_DRIVE = LDG_FILE_DIRECTORY(1:2)
         ELSE
            LDG_FILE_DRIVE = RESULTS_DIRECTORY(1:1)//':'
            LDG_FILE_DIRECTORY = LDG_FILE_DRIVE//LDG_FILE_DIRECTORY
         ENDIF
      ENDIF
!
! PRB FILE DIRECTORY SET-UP
!
      IF(trim(PRB_FILE_DIRECTORY) == ' ') THEN
         PRB_FILE_DIRECTORY = RESULTS_DIRECTORY
      ELSE
         I = NBLANK(PRB_FILE_DIRECTORY)
         IF(I == 0 .OR. I == 256) THEN
            PRB_FILE_DIRECTORY = RESULTS_DIRECTORY
            I = NBLANK(PRB_FILE_DIRECTORY)
         ELSEIF(I==1 .AND. PRB_FILE_DIRECTORY(1:1) /= '\') THEN
            PRB_FILE_DIRECTORY = trim(PRB_FILE_DIRECTORY)//':'
            I = 2
         ENDIF
         IF(PRB_FILE_DIRECTORY(I:I) /= '\') THEN
            PRB_FILE_DIRECTORY = trim(PRB_FILE_DIRECTORY)//'\'
         ELSE
            I = I - 1
         ENDIF
         OPEN(10,FILE=trim(PRB_FILE_DIRECTORY)//'TESTDIR',IOSTAT=IOS)
         IF(IOS > 0) THEN
            PRB_FILE_DIRECTORY = RESULTS_DIRECTORY
         ELSE
            CLOSE(10,STATUS='DELETE')
         ENDIF

         I = NBLANK(PRB_FILE_DIRECTORY)
         IF(PRB_FILE_DIRECTORY(I:I) /= '\') &
                    PRB_FILE_DIRECTORY = trim(PRB_FILE_DIRECTORY)//'\'
         IF(INDEX(PRB_FILE_DIRECTORY,':') == 2) THEN
            PRB_FILE_DRIVE = PRB_FILE_DIRECTORY(1:2)
         ELSE
            PRB_FILE_DRIVE = RESULTS_DIRECTORY(1:1)//':'
            PRB_FILE_DIRECTORY = PRB_FILE_DRIVE//PRB_FILE_DIRECTORY
         ENDIF
      ENDIF
!
! SHB FILE DIRECTORY SET-UP
!
      IF(trim(SHB_FILE_DIRECTORY) == ' ') THEN
         SHB_FILE_DIRECTORY = RESULTS_DIRECTORY
      ELSE
         I = NBLANK(SHB_FILE_DIRECTORY)
         IF(I == 0 .OR. I == 256) THEN
            SHB_FILE_DIRECTORY = RESULTS_DIRECTORY
            I = NBLANK(SHB_FILE_DIRECTORY)
         ELSEIF(I==1 .AND. SHB_FILE_DIRECTORY(1:1) /= '\') THEN
            SHB_FILE_DIRECTORY = trim(SHB_FILE_DIRECTORY)//':'
            I = 2
         ENDIF
         IF(SHB_FILE_DIRECTORY(I:I) /= '\') THEN
            SHB_FILE_DIRECTORY = trim(SHB_FILE_DIRECTORY)//'\'
         ELSE
            I = I - 1
         ENDIF
         OPEN(10,FILE=trim(SHB_FILE_DIRECTORY)//'TESTDIR',IOSTAT=IOS)
         IF(IOS > 0) THEN
            SHB_FILE_DIRECTORY = RESULTS_DIRECTORY
         ELSE
            CLOSE(10,STATUS='DELETE')
         ENDIF
!         SEARCH_FILE = trim(SHB_FILE_DIRECTORY)//'SHB*.*'
!         CALL FINDFRST(SEARCH_FILE,FILE_NAME)
!         ERROR = ISERROR()
!         IF(ERROR > 0) THEN
!            SHB_FILE_DIRECTORY = RESULTS_DIRECTORY
!         ENDIF
         I = NBLANK(SHB_FILE_DIRECTORY)
         IF(SHB_FILE_DIRECTORY(I:I) /= '\') &
                    SHB_FILE_DIRECTORY = trim(SHB_FILE_DIRECTORY)//'\'
         IF(INDEX(SHB_FILE_DIRECTORY,':') == 2) THEN
            SHB_FILE_DRIVE = SHB_FILE_DIRECTORY(1:2)
         ELSE
            SHB_FILE_DRIVE = RESULTS_DIRECTORY(1:1)//':'
            SHB_FILE_DIRECTORY = SHB_FILE_DRIVE//SHB_FILE_DIRECTORY
         ENDIF
      ENDIF
!
! XML FILE DIRECTORY SET-UP
!
      IF(trim(XML_FILE_DIRECTORY) == ' ') THEN
         XML_FILE_DIRECTORY = RESULTS_DIRECTORY
      ELSE
         I = NBLANK(XML_FILE_DIRECTORY)
         IF(I == 0 .OR. I == 256) THEN
            XML_FILE_DIRECTORY = RESULTS_DIRECTORY
            I = NBLANK(XML_FILE_DIRECTORY)
         ELSEIF(I==1 .AND. XML_FILE_DIRECTORY(1:1) /= '\') THEN
            XML_FILE_DIRECTORY = trim(XML_FILE_DIRECTORY)//':'
            I = 2
         ENDIF
         IF(XML_FILE_DIRECTORY(I:I) /= '\') THEN
            XML_FILE_DIRECTORY = trim(XML_FILE_DIRECTORY)//'\'
         ELSE
            I = I - 1
         ENDIF
         OPEN(10,FILE=trim(XML_FILE_DIRECTORY)//'TESTDIR',IOSTAT=IOS)
         IF(IOS > 0) THEN
            XML_FILE_DIRECTORY = RESULTS_DIRECTORY
         ELSE
            CLOSE(10,STATUS='DELETE')
         ENDIF

         I = NBLANK(XML_FILE_DIRECTORY)
         IF(XML_FILE_DIRECTORY(I:I) /= '\') &
                    XML_FILE_DIRECTORY = trim(XML_FILE_DIRECTORY)//'\'
         IF(INDEX(XML_FILE_DIRECTORY,':') == 2) THEN
            XML_FILE_DRIVE = XML_FILE_DIRECTORY(1:2)
         ELSE
            XML_FILE_DRIVE = RESULTS_DIRECTORY(1:1)//':'
            XML_FILE_DIRECTORY = XML_FILE_DRIVE//XML_FILE_DIRECTORY
         ENDIF
      ENDIF
!
      RETURN_CHAR = STORE_BASE_FILE_DIRECTORY(BASE_FILE_DIRECTORY, &
                                              RESULTS_DIRECTORY, &
                                              cldata%SCENAME, &
                                              LDE_FILE_DIRECTORY, &
                                              PRB_FILE_DIRECTORY, &
                                              SHB_FILE_DIRECTORY, &
                                              XML_FILE_DIRECTORY, &
                                              LDG_FILE_DIRECTORY)
      RETURN
      END
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      FUNCTION NEW_DETAILED_REPORTS()
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
      use rptreccontrol
      USE DRILLING_REPT_PARAMETERS
      USE IREC_ENDPOINT_CONTROL
      use logging
      use miscmod
      use rptreccontrol
      use dreptcom
      use midasmod_decs
      use mod_base_year
      use tf_objt_interfaces
      implicit none



      integer, save :: timeshere=0
      CHARACTER (LEN=2) :: CAPACITY_PLANNING_METHOD,GREEN_MRX_METHOD
      LOGICAL (kind=1) ::  NEW_DETAILED_REPORTS
      INTEGER (kind=2) ::  CPL_EMC_CAP_REVS_RPT_HEADER, &
                CPL_PAEN_RPT_HEADER, &
                CPL_DEFERRED_FUEL_RPT_HEADER, &
                DAY_WEEK_RPT_HEADER, &
                TRANS_TIE_RPT_HEADER, &
                DAILY_PRODUCTS_RPT_HEADER, &
                SCENARIO_MAKER_RPT_HEADER, &
                REGIONAL_MAKER_RPT_HEADER, &
                MONTHLY_FUEL_DERIV_RPT_HEADER, &
                TRANS_DEL_RPT_HEADER,TRANS_ANN_RPT_HEADER, &
                MON_TRANC_RPT_HEADER, &
                MON_POSIT_RPT_HEADER, &
                TRANS_PRI_RPT_HEADER,TRANS_LDS_RPT_HEADER, &
                TRANS_LBH_RPT_HEADER, &
                TRANS_INTR_RPT_HEADER, &
                TRANS_SCA_RPT_HEADER, &
                TRANS_LAS_RPT_HEADER, &
                TRANS_MCB_RPT_HEADER, &
                MARKET_AREA_RPT_HEADER, &
                GAS_MARKET_AREA_RPT_HEADER, &
                GAS_MARKET_TRANS_RPT_HEADER, &
                DAILY_GAS_SELL_BUY_HEADER, &
                MONTHLY_GAS_SELL_BUY_HEADER, &
                MONTHLY_GAS_LINK_HEADER, &
                MONTHLY_GAS_RESERVE_HEADER, &
                DAILY_GAS_LINK_HEADER, &
                DAILY_GAS_LINK_LIMIT_HEADER, &
                DAILY_GAS_LINK_PRICE_HEADER, &
                DAILY_GAS_LINK_CF_HEADER, &
                DAILY_GAS_AVE_PRICE_HEADER, &
                DAILY_GAS_SUR_DEF_HEADER, &
                MONTHLY_GAS_LINK_PRICE_HEADER, &
                DAILY_GAS_PRICE_HEADER, &
                DAILY_GAS_NODE_VOLUME_HEADER, &
                DAILY_GAS_STORAGE_HEADER, &
                MONTHLY_GAS_PRICE_HEADER, &
                MONTHLY_GAS_DIFFER_HEADER, &
                DAILY_GAS_DIFFER_HEADER, &
                DAILY_GAS_UNSERVED_HEADER, &
                DAILY_GAS_SUPPLY_HEADER, &
                TRANS_HMP_RPT_HEADER, &
                TRANS_MRK_RPT_HEADER, &
                TRANS_PB4_RPT_HEADER, &
                TRANS_PAF_RPT_HEADER, &
                TRANS_CAP_RPT_HEADER, &
                TRANS_DER_RPT_HEADER, &
                TRANS_EUE_RPT_HEADER, &
                TRANS_INC_RPT_HEADER, &
                TRANS_LMD_RPT_HEADER, &
                TRANS_TRM_RPT_HEADER, &
                TRANS_EXC_RPT_HEADER, &
                HOURLY_ECAP_RPT_HEADER, &
                HOUR_DIAG_RPT_HEADER, &
                HOUR_JDA_RPT_HEADER, &
                DEPTH_MARKET_RPT_HEADER, &
                TRANS_DEPTH_MARKET_RPT_HEADER, &
                C_DEPTH_MARKET_RPT_HEADER, &
                C_DEPTH_MARGIN_RPT_HEADER, &
                DYNAMIC_JURIS_RPT_HEADER, &
                TRANSACT_FUEL_RPT_HEADER, &
                TRANSACT_STATE_RPT_HEADER, &
                STATE_RPS_RPT_HEADER, &
                CO2_ABATEMENT_RPT_HEADER, &
                RPS_PROGRAMS_RPT_HEADER, &
                TRANSACT_PROD_RPT_HEADER, &
                TRAN_PROD_FUEL_RPT_HEADER, &
                MARGINAL_UNIT_TECH_HEADER, &
                TRANSACT_PROD_PROD_RPT_HEADER, &
                TRANSACT_PROD_MW_RPT_HEADER, &
                RPS_CURVE_RPT_HEADER, &
                HOURLY_STORAGE_REPORT_HEADER, &
                TRANSACT_PROD_FUEL_RPT_HEADER, &
                NUC_FUEL_REGION_RPT, &
                EXISTING_ASSET_RPT_HEADER, &
                WVPA_ASSET_RPT_HEADER, &
                WVPA_WORKORDER_CONTROL_RPT, &
                WVPA_WORKORDER_EXPENSES_RPT, &
                WVPA_WORKORDER_SUMMARY_RPT, &
                WVPA_DEBT_FILE_RPT_HEADER, &
                WVPA_MORTGAGE_SUMMARY_RPT_HEADR, &
                WVPA_PLNT_BALANCES_RPT_HEADR, &
                WVPA_CONSTR_SUMY_RPT_HEADR, &
                WVPA_COMP_CONSTR_SUMY_RPT_HEADR, &
                WVPA_CUM_DEP_RPT_HEADR, &
                WVPA_RATE_TRACKER_HEADR, &
                FUTURE_ASSET_RPT_HEADER, &
                TRANS_CONTRIBUTION_RPT_HEADER, &
                TRANS_TRN_RPT_HEADER, &
                ASSET_PROD_RPT_HEADER, &
                ANNUAL_SERVICE_TRANS_HEADER, &
                MON_CL_TRANS_UNIT_HEADER, &
                MON_DV_TRANS_UNIT_HEADER, &
                MON_NU_TRANS_UNIT_HEADER, &
                TECH_SCAR_HEADER, &
                ECITY_OBJ_HEADER, &
                MARKET_DURATION_HEADER, &
                USER_DURATION_HEADER, &
                TRANS_CLASS_SUM_HEADER, &
                MONTHLY_GAS_DEMAND_HEADER, &
                MONTHLY_GAS_BALANCE_HEADER, &
                MONTHLY_GAS_SUPPLY_HEADER, &
                MONTHLY_GAS_BASIN_HEADER, &
                WH_HOURLY_HEADER, &
                DEBT_FILE_RPT_HEADER, &
                DEBIT_FILE_RPT_HEADER, &
                CPL_SALES_GEN_HEADER, &
                MONTH_GROUP_HEADER, &
                MX_ANNUAL_HEADER, &
                GRX_ANNUAL_HEADER, &
                GRX_8761_HEADER, &
                GAS_PIPE_EXPANSION_HEADER, &
                MX_ICAP_ANNUAL_HEADER, &
                CX_DAILY_HEADER, &
                UU_DAILY_HEADER, &
                UK_DAILY_HEADER, &
                HX_HOURLY_HEADER, &
                AJ_HOURLY_HEADER, &
                AK_HOURLY_HEADER, &
                UV_HOURLY_HEADER, &
                XX_HOURLY_HEADER, &
                KX_HOURLY_HEADER, &
                QX_HOURLY_HEADER, &
                WX_HOURLY_HEADER, &
                HOURLY_FLOW_ANALYST_HEADER, &
                HOURLY_FLOW_CONSTRAINT_HEADER, &
                C_HOURLY_COST_HEADER, &
                C_HOURLY_MWH_COST_HEADER, &
                C_HOURLY_SUMMARY_HEADER, &
                C_TG_HOURLY_SUMMARY_HEADER, &
                GY_HOURLY_HEADER, &
                DX_HOURLY_HEADER, &
                OX_HOURLY_HEADER, &
                HEC_HOURLY_HEADER, &
                HYDRO_AG_HEADER, &
                RC_REVENUE_FORECAST_HEADER, &
                TRAILING_12_MOS_RPT_HEADER, &
!               IPL regulated revenues report header ->
                IPL_REGULATED_REVENUES_RPT_H, &
                FE_MONTHLY_PCA_LEGAL_HEADER
      INTEGER (kind=2) ::  TRANS_DISPATCH_RPT_HEADER, &
                CAPEX_THERMAL_RPT_HEADER, &
                NEW_SUPPLY_CURVE_HEADER
      INTEGER (kind=2) ::  LM_COST_REPORT_HEADER, &
                DSM_RATE_REPORT_HEADER, &
                FORECASTED_DAYS_REPORT_HEADER, &
                FORE_MONTH_REPORT_HEADER, &
                ANN_CL_UNIT_HEADER, &
                MON_CL_UNIT_HEADER, &
                MON_LOAD_BY_BLOCK_HEADER, &
                HOURLY_CUST_MARGIN_HEADER, &
                IPL_FAC_HEADER, &
                MON_CT_UNIT_HEADER, &
                ANN_CL_SUMMARY_HEADER, &
                ANN_POOL_SUMMARY_HEADER, &
                MON_CL_SUMMARY_HEADER, &
                LDC_REPORT_HEADER, &
                LAST_ELDC_HEADER, &
                LOLP_CURVE_HEADER, &
                LOLP_MONTHLY_HEADER, &
                ANN_EL_UNIT_HEADER, &
                MON_EL_UNIT_HEADER, &
                CLASS_REV_ALLOC_HEADER, &
                REV_FUNC_HEADER, &
                DISPATCH_ORDER_HEADER, &
                CATAWBA_HEADER, &
                FUEL_INVENTORY_HEADER, &
                RESERVE_MARGIN_HEADER, &
                TG_RESERVE_MARGIN_HEADER, &
                EXPANSION_PATTERN_HEADER, &
                MON_DSM_PATTERN_HEADER, &
                WABASH_VALLEY_POWER_COST_HEADER, &
                WABASH_VALLEY_POWER_DSM_HEADER, &
                WVPA_COOP_REVENUE_HEADER, &
                WABASH_VALLEY_POWER_COST_NO, &
                WV_AREA_REPORT_HEADER, &
                MAINTENANCE_HEADER, &
                CM_MAINT_HEADER, &
                R_MW_MAINT_NO, &
                SCREENING_HEADER, &
                MARGINAL_COST_HEADER, &
                MARGINAL_DATA_BASE_HEADER, &
                ECONOMY_INTER_HEADER, &
                GET_EXT_ECONOMY_INTER_NO, &
                SERVICE_TRANS_HEADER, &
                   FUTURE_ASSET_REPT_HEADER, &
                   GET_SCENAME_AND_EXT_PROD_NUM, &
                GET_SCENAME_AND_EXT_AI_NUM, &
                   R_EXTERNAL_AI_NUM, &
                FLEX_PLANNING_ACTION_HEADER, &
                ASSET_CLASS_INCOME_HEADER, &
                FE_PCA_LEGAL_HEADER, &
                AN_DECOMP_REPORT_HEADER, &
                RESOURCES_TESTED_RPT_HEADER
      INTEGER (kind=2) ::  OVERHEAD_LENGTH
      INTEGER (kind=2) ::  VARIABLE_NUMBER,RECORD_LENGTH, &
                RPT_UNIT_NUM,ESTABLISH_DETAIL_REPORT_FILE, &
                DIMENSIONS, &
                R_VARIABLE_NUMBER,NEXT_REC, &
                ESTABLISH_DETAIL_REPORT_FILE_WO
      INTEGER ::  R_RECORD_LENGTH,R_START_RECORD
      CHARACTER (len=256) ::  FILE_NAME,GET_RESULTS_DIRECTORY
      CHARACTER (len=5) ::  GET_SCENAME
      CHARACTER (len=28) ::  COMPANY_NAME,TITLE*40
      SAVE FILE_NAME,OVERHEAD_LENGTH,VARIABLE_NUMBER,DIMENSIONS, &
           RPT_UNIT_NUM
      LOGICAL (kind=1) ::  UNIT_1500_IS_OPEN=.FALSE.
      CHARACTER (len=5) ::  R_SCENAME
      CHARACTER (len=18) ::  DATE_TIME,SYM_SYS_DATE_TIME
      LOGICAL (kind=1) ::  AN_DECOMP_ACTIVE,DETAIL_ANNUAL_DECOMP_REPORT
      INTEGER (kind=2) ::  GET_NUM_OF_END_POINTS
      LOGICAL ::  FILE_EXISTS
      CHARACTER (len=12) ::  DES_FILE_NAME
      INTEGER (kind=2) ::  WRITE_OPTIONS_REPORT_HEADER
      CHARACTER (len=32) ::  SHORT_FORM_TITLE=' '
      CHARACTER (len=8) ::  SHORT_FORM_NAME=' ' ! CHANGED 12/6/93.GAT
      INTEGER (kind=2) ::  ACTIVE_VARIABLES,LAST_VARIABLE,get_endyr
      CHARACTER (len=16) ::  SET_RUN_DATE_TIME,RUN_DATE_TIME
      INTEGER (kind=4) ::  R_NEXT_REC, &
                R_CL_NEXT_REC, &
                R_MW_NEXT_REC
      CHARACTER (len=1) ::  ACTIVE_DIMENSION
      LOGICAL (kind=1) ::  ADD_MONTHLY_DIMENSION

!
! END DATA DECLARATIONS
!                               NEW_DETAILED_REPORTS = .TRUE.
      RETURN
! Beginning of report headers

      ENTRY GET_SCENAME_AND_EXT_PROD_NUM(R_SCENAME)

        R_SCENAME = GET_SCENAME()
        GET_SCENAME_AND_EXT_PROD_NUM = EXTERNAL_PRODUCTION_NUM
      RETURN

      ENTRY GET_SCENAME_AND_EXT_AI_NUM(R_SCENAME)

        R_SCENAME = GET_SCENAME()
        GET_SCENAME_AND_EXT_AI_NUM = EXTERNAL_AI_NUM
      RETURN

      ENTRY AN_DECOMP_REPORT_HEADER(R_NEXT_REC)

!
! OPEN BIFP OUTPUT FILE
!
         AN_DECOMP_REPORT_HEADER = 1500
         IF(DETAIL_ANNUAL_DECOMP_REPORT() .AND. &
                                            .NOT.UNIT_1500_IS_OPEN) THEN

            ACTIVE_VARIABLES = get_last_variable() + 1
            DES_FILE_NAME = trim(SHORT_FORM_NAME)//'.DSS'
            CALL COPY(DES_FILE_NAME,'MSGANDEC.RPT')
            DES_FILE_NAME = 'MSGANDEC.RPT'
!
            FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                           trim(GET_SCENAME())//".ARD"
            OVERHEAD_LENGTH = 150
            VARIABLE_NUMBER = ACTIVE_VARIABLES+5
            RPT_UNIT_NUM = INT(1500,2)
            DIMENSIONS = 4
            RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE_WO(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!

            WRITE(1500,REC=NEXT_REC) 'End Point           ', &
                                                       'N',INT(4,2),'V'
            WRITE(1500,REC=NEXT_REC+1) 'Planning Year       ', &
                                                       'N',INT(4,2),'V'
            WRITE(1500,REC=NEXT_REC+2) 'Plan Number         ', &
                                                       'N',INT(4,2),'V'
            WRITE(1500,REC=NEXT_REC+3) 'Simulation Year     ', &
                                                       'N',INT(4,2),'V'
            R_NEXT_REC = NEXT_REC + 4
            IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
            UNIT_1500_IS_OPEN = .TRUE.
         ENDIF
      RETURN

      ENTRY RESOURCES_TESTED_RPT_HEADER(R_NEXT_REC)

!
! OPEN THE RESOURCE TESTED FILE
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                           trim(GET_SCENAME())//".AOD"
         OVERHEAD_LENGTH = 128
         VARIABLE_NUMBER = 0
         RPT_UNIT_NUM = INT(1501,2)
         DIMENSIONS = 3
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE_WO(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(1501,REC=NEXT_REC)   'End Point           ', &
                                                       'N',INT(4,2),'V'
         WRITE(1501,REC=NEXT_REC+1) 'Planning Year       ', &
                                                       'N',INT(4,2),'V'
         WRITE(1501,REC=NEXT_REC+2) 'Plan Order          ', &
                                                       'N',INT(4,2),'V'
         R_NEXT_REC = NEXT_REC + 3
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         RESOURCES_TESTED_RPT_HEADER = 1501
      RETURN
!

      ENTRY WRITE_OPTIONS_REPORT_HEADER(R_NEXT_REC)

!
! OPEN THE RESOURCE OPTIONS FILE
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                           trim(GET_SCENAME())//".ALD"
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 0
         RPT_UNIT_NUM = INT(1502,2)
         DIMENSIONS = 2
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE_WO(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(1502,REC=NEXT_REC) 'End Point           ','N', &
                                  INT(4,2),'V'
         WRITE(1502,REC=NEXT_REC+1) 'Name                ', &
                                                        'C', &
                                                      INT(20,2),'V'
         R_NEXT_REC = NEXT_REC + 2
!
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         WRITE_OPTIONS_REPORT_HEADER = 1502
      RETURN

      ENTRY LM_COST_REPORT_HEADER(R_NEXT_REC)

         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                           trim(GET_SCENAME())//".DCD"
         OVERHEAD_LENGTH = 68
         VARIABLE_NUMBER = 24
         DIMENSIONS = 4
         RPT_UNIT_NUM = LM_COST_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'LM Financial Account','C', &
                                                    INT(30,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Cost Component      ','C', &
                                                      INT(30,2),'F','D'
         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         LM_COST_REPORT_HEADER = LM_COST_NO
      RETURN

      ENTRY MON_DSM_PATTERN_HEADER(R_NEXT_REC)

         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                           trim(GET_SCENAME())//".MDD"
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 25
         DIMENSIONS = 5
         RPT_UNIT_NUM = MON_DSM_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Load Manage. Device ','C', &
                                                       INT(32,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                             'Day Type            ','C',INT(8,2),'F','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MON_DSM_PATTERN_HEADER = MON_DSM_NO
      RETURN

      ENTRY DSM_RATE_REPORT_HEADER(R_NEXT_REC)

         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                           trim(GET_SCENAME())//".DTD"
         OVERHEAD_LENGTH = 60
         VARIABLE_NUMBER = 24
         DIMENSIONS = 4
         RPT_UNIT_NUM = DSM_RATE_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Customer Class      ','C', &
                                                     INT(20,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Load Manage. Device ','C', &
                                                     INT(32,2),'V','D'
!
         R_NEXT_REC = NEXT_REC + 2
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         DSM_RATE_REPORT_HEADER = DSM_RATE_NO
      RETURN

      ENTRY REV_FUNC_HEADER(R_NEXT_REC)

         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                           trim(GET_SCENAME())//".FAD"
         OVERHEAD_LENGTH = 16
         VARIABLE_NUMBER = 5
         DIMENSIONS = 3
         RPT_UNIT_NUM = REV_FUNC_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Functional Allocator','C',INT(8,2),'V','D'
         R_NEXT_REC = NEXT_REC + 1
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         REV_FUNC_HEADER = REV_FUNC_NO
      RETURN

      ENTRY CLASS_REV_ALLOC_HEADER(R_NEXT_REC)

         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                           trim(GET_SCENAME())//".RAD"
         OVERHEAD_LENGTH = 28
         VARIABLE_NUMBER = 10
         DIMENSIONS = 3
         RPT_UNIT_NUM = REV_ALLOC_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Customer Class      ','C', &
                                                      INT(20,2),'V','D'
!
         R_NEXT_REC = NEXT_REC + 1
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         CLASS_REV_ALLOC_HEADER = REV_ALLOC_NO
      RETURN

      ENTRY  FORECASTED_DAYS_REPORT_HEADER(R_NEXT_REC)

         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                           trim(GET_SCENAME())//".FDD"
         OVERHEAD_LENGTH = 25
         VARIABLE_NUMBER = 24
         DIMENSIONS = 4
         RPT_UNIT_NUM = FORE_DAYS_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day Type            ','C',INT(8,2),'F','D'
         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         FORECASTED_DAYS_REPORT_HEADER = FORE_DAYS_NO
      RETURN

      ENTRY FORE_MONTH_REPORT_HEADER(R_NEXT_REC)

         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                           trim(GET_SCENAME())//".FQD"
         OVERHEAD_LENGTH = 25
         VARIABLE_NUMBER = 4
         DIMENSIONS = 4
         RPT_UNIT_NUM = FORE_MONTH_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day Type            ','C',INT(8,2),'F','D'
         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         FORE_MONTH_REPORT_HEADER = FORE_MONTH_NO
      RETURN

      ENTRY LDC_REPORT_HEADER(R_NEXT_REC)

         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                           trim(GET_SCENAME())//".LDD"
         OVERHEAD_LENGTH = 27
         VARIABLE_NUMBER = 2
         DIMENSIONS = 5
         RPT_UNIT_NUM = LDC_REPORT_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Before/After EL     ','C',INT(6,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                             'Load Position       ','N',INT(4,2),'F','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         LDC_REPORT_HEADER = LDC_REPORT_NO
      RETURN

      ENTRY LAST_ELDC_HEADER

! THIS IS A FORMATED ASCII FILE
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                           trim(GET_SCENAME())//".LED"
         OPEN(LAST_ELDC_NO,FILE=FILE_NAME)
!
         LAST_ELDC_HEADER = LAST_ELDC_NO
      RETURN
!

      ENTRY LOLP_MONTHLY_HEADER(R_NEXT_REC)

         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                           trim(GET_SCENAME())//".LOD"
         OVERHEAD_LENGTH = 21
         VARIABLE_NUMBER = 13
         DIMENSIONS = 2
         RPT_UNIT_NUM = LOLP_REPORT_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         R_NEXT_REC = NEXT_REC
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         LOLP_MONTHLY_HEADER = LOLP_REPORT_NO
      RETURN

      ENTRY LOLP_CURVE_HEADER

! THIS IS A FORMATTED ASCII FILE
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                           trim(GET_SCENAME())//".LCD"
         OPEN(LOLP_CURVE_NO,FILE=FILE_NAME)
!
         LOLP_CURVE_HEADER = LOLP_CURVE_NO
      RETURN

      ENTRY CATAWBA_HEADER(R_NEXT_REC)

         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                           trim(GET_SCENAME())//".CAD"
         OVERHEAD_LENGTH = 13
         VARIABLE_NUMBER = 26
         DIMENSIONS = 3
         RPT_UNIT_NUM = CATAWBA_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Company             ','C',INT(5,2),'V','D'
         R_NEXT_REC = NEXT_REC + 1
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         CATAWBA_HEADER = CATAWBA_NO
      RETURN

      ENTRY RESERVE_MARGIN_HEADER(R_NEXT_REC)

         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                           trim(GET_SCENAME())//".RMD"
         OVERHEAD_LENGTH = 8
! debugmod - not sure whether variable_number should be increased here.
! TODO: increase variable number? John did that last time but it's 
! unclear whether that's appropriate. Ask Greg.  variable_number_dsex
! is used to determine the length of the record, and it's a count, not
! an ID

! TODO: Update for new vars - debugmod
         VARIABLE_NUMBER = 24
         DIMENSIONS = 2
         RPT_UNIT_NUM = RESERVE_MARGIN_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         R_NEXT_REC = NEXT_REC
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         RESERVE_MARGIN_HEADER = RESERVE_MARGIN_NO
      RETURN

      ENTRY TG_RESERVE_MARGIN_HEADER(R_NEXT_REC)

         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                           trim(GET_SCENAME())//TG_RESERVE_MARGIN_CODE
         OVERHEAD_LENGTH = 64
         
         ! ** Previously set to 51 by JTR for OW, HB, H2, CS but 
         ! believe that was a mistake.
         ! debugmod - check this value.
         VARIABLE_NUMBER = 43

         
         DIMENSIONS = 3
         IF(CAPACITY_PLANNING_METHOD() == 'MX'  .AND. &
                              GREEN_MRX_METHOD() == 'GX' .AND. &
                                   .NOT. HORIZONS_ACTIVE) THEN
            DIMENSIONS = 4
            OVERHEAD_LENGTH = 64
         ENDIF
         RPT_UNIT_NUM = TG_RESERVE_MARGIN_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         IF(CAPACITY_PLANNING_METHOD() == 'MX'  .AND. &
                                 GREEN_MRX_METHOD() == 'GX' .AND. &
                                             .NOT. HORIZONS_ACTIVE) THEN
            WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                              'Iteration           ','N', &
                                                       INT(4,2),'F','D'
            NEXT_REC = NEXT_REC + 1
         ENDIF
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                            'Transaction Group   ','C',INT(35,2),'V','D'
         R_NEXT_REC = NEXT_REC + 1

          IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)

         TG_RESERVE_MARGIN_HEADER = TG_RESERVE_MARGIN_NO
      RETURN

      ENTRY EXPANSION_PATTERN_HEADER(R_NEXT_REC)

         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                           trim(GET_SCENAME())//".EPD"
         OVERHEAD_LENGTH = 64 !46
         VARIABLE_NUMBER = 24
         DIMENSIONS = 4
         RPT_UNIT_NUM = EXPANSION_PATTERN_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE_WO(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         ! TODO: Put these write statements in a loop for DRY.
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Endpoint            ','N',INT(4,2),'V','D'
         next_rec=next_rec+1
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Current Year        ','N',INT(4,2),'V','D'
         next_rec=next_rec+1
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Start Year          ','N',INT(4,2),'V','D'
         next_rec=next_rec+1
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                            'Resource Name/Status','C',INT(35,2),'V','D'
         next_rec=next_rec+1
         

         R_NEXT_REC = NEXT_REC
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         EXPANSION_PATTERN_HEADER = EXPANSION_PATTERN_NO
      RETURN

      ENTRY FUTURE_ASSET_REPT_HEADER(R_NEXT_REC)

         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                           trim(GET_SCENAME())//".NAD"
         OVERHEAD_LENGTH = 68
         VARIABLE_NUMBER = 31
         DIMENSIONS = 4
         RPT_UNIT_NUM = FUTURE_ASSETS_UNIT_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE_WO(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Endpoint            ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Type                ','C',INT(6,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Asset Name          ','C',INT(38,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+3) &
                             'Result Year         ','N',INT(4,2),'V','D'
         R_NEXT_REC = NEXT_REC + 4
!
         FUTURE_ASSET_REPT_HEADER = FUTURE_ASSETS_UNIT_NO
      RETURN

      ENTRY FLEX_PLANNING_ACTION_HEADER(R_NEXT_REC)

         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                           trim(GET_SCENAME())//".FXD"
         OVERHEAD_LENGTH = 64 !54
         VARIABLE_NUMBER = 4
         DIMENSIONS = 3
         RPT_UNIT_NUM = FLEX_PLANNING_ACTION_UNIT_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE_WO(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Endpoint            ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                            'Asset Name          ','C',INT(38,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Action              ','C',INT(12,2),'V','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         FLEX_PLANNING_ACTION_HEADER = FLEX_PLANNING_ACTION_UNIT_NO
      RETURN

      ENTRY FUEL_INVENTORY_HEADER(R_NEXT_REC)

! IN CL_REPORT
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                           trim(GET_SCENAME())//".FID"
         OVERHEAD_LENGTH = 64 !41
         VARIABLE_NUMBER = 2
         DIMENSIONS = 5
         RPT_UNIT_NUM = FUEL_INVEN_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Fuel Source         ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                             'Generating Unit     ','C',INT(20,2),'V', &
                                                                     'D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         FUEL_INVENTORY_HEADER = FUEL_INVEN_NO
      RETURN

      ENTRY ANN_CL_UNIT_HEADER(R_NEXT_REC)

! IN CLREPORT
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                           trim(GET_SCENAME())//".TAD"
         OVERHEAD_LENGTH = 64 !28
         VARIABLE_NUMBER = 16
         DIMENSIONS = 3
         IF(CAPACITY_PLANNING_METHOD() == 'MX'  .AND. &
                              GREEN_MRX_METHOD() == 'GX') DIMENSIONS = 4
         RPT_UNIT_NUM = ANN_CL_UNIT_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                            'Generating Unit     ','C',INT(20,2),'V','D'
         R_NEXT_REC = NEXT_REC + 1
         IF(CAPACITY_PLANNING_METHOD() == 'MX'  .AND. &
                                        GREEN_MRX_METHOD() == 'GX') THEN
            WRITE(RPT_UNIT_NUM,REC=R_NEXT_REC) &
                             'Iteration           ','N',INT(4,2),'F','D'
            R_NEXT_REC = R_NEXT_REC + 1
         ENDIF
!
         ANN_CL_UNIT_HEADER = ANN_CL_UNIT_NO
      RETURN

      ENTRY MON_CL_UNIT_HEADER(R_NEXT_REC)

! CLREPORT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                           trim(GET_SCENAME())//".TMD"
         OVERHEAD_LENGTH = 64 !39
         VARIABLE_NUMBER = 14
         DIMENSIONS = 4
         RPT_UNIT_NUM = MON_CL_UNIT_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                            'Unit Segment        ','C',INT(22,2),'V','D'
         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MON_CL_UNIT_HEADER = MON_CL_UNIT_NO
      RETURN

      ENTRY MON_LOAD_BY_BLOCK_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)

! CLREPORT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                               trim(GET_SCENAME())//LOAD_BY_BLOCK_CODE
         OVERHEAD_LENGTH = 64 !48
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 5
         RPT_UNIT_NUM = LOAD_BY_BLOCK_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                            'Unit Segment        ','C',INT(22,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                             'Component           ','C',INT(9,2),'V','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MON_LOAD_BY_BLOCK_HEADER = LOAD_BY_BLOCK_NO
      RETURN

      ENTRY HOURLY_CUST_MARGIN_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)

! 02/25/04.
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                          trim(GET_SCENAME())//HOURLY_CUST_MARGIN_CODE
         OVERHEAD_LENGTH = 64 !30
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = HOURLY_CUST_MARGIN_NO
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Hour                ','N',INT(4,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                             'Variable            ','C',INT(9,2),'F','D'
!
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         HOURLY_CUST_MARGIN_HEADER = HOURLY_CUST_MARGIN_NO
      RETURN

      ENTRY HOURLY_STORAGE_REPORT_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)

! 02/25/04.
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                              trim(GET_SCENAME())//HOURLY_STORAGE_CODE
         OVERHEAD_LENGTH = 64 !30
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = HOURLY_STORAGE_UNIT
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day of Month        ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Transaction Group   ','C',INT(35,2),'V','D'
!
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         HOURLY_STORAGE_REPORT_HEADER = HOURLY_STORAGE_UNIT
      RETURN

      ENTRY IPL_FAC_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)

! CLREPORT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                               trim(GET_SCENAME())//IPL_FAC_CODE
         OVERHEAD_LENGTH = 64 !26
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 4
         RPT_UNIT_NUM = IPL_FAC_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Component           ','C',INT(9,2),'V','D'
         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         IPL_FAC_HEADER = IPL_FAC_NO
      RETURN

      ENTRY ANN_CL_SUMMARY_HEADER(R_NEXT_REC)

! CLREPORT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                           trim(GET_SCENAME())//".TSD"
         OVERHEAD_LENGTH = 8
         VARIABLE_NUMBER = 27
         DIMENSIONS = 2
         RPT_UNIT_NUM = ANN_CL_SUM_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         R_NEXT_REC = NEXT_REC
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         ANN_CL_SUMMARY_HEADER = ANN_CL_SUM_NO
      RETURN

      ENTRY MON_CT_UNIT_HEADER(R_NEXT_REC)

! CLREPORT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                           trim(GET_SCENAME())//".CMD"
         OVERHEAD_LENGTH = 37
         VARIABLE_NUMBER = 13
         DIMENSIONS = 4
         RPT_UNIT_NUM = MON_CT_UNIT_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                            'Contract            ','C',INT(20,2),'V','D'
         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MON_CT_UNIT_HEADER = MON_CT_UNIT_NO
      RETURN

      ENTRY MON_CL_TRANS_UNIT_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER, &
                                     R_RECORD_LENGTH)

! CLREPORT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                trim(GET_SCENAME())//MON_CL_TRANS_CODE
         OVERHEAD_LENGTH = 64 !42
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
!         VARIABLE_NUMBER = 36
         DIMENSIONS = 4
         RPT_UNIT_NUM = MON_CL_TRANS_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                  'Generating Unit     ','C',INT(25,2),'V','D',INT(25,2)
         R_NEXT_REC = NEXT_REC + 2
!
         R_RECORD_LENGTH = RECORD_LENGTH
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MON_CL_TRANS_UNIT_HEADER = MON_CL_TRANS_NO
      RETURN

      ENTRY MON_DV_TRANS_UNIT_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER, &
                                     R_RECORD_LENGTH)

! CLREPORT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                trim(GET_SCENAME())//MON_DV_TRANS_CODE
         OVERHEAD_LENGTH = 64 !42
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
!         VARIABLE_NUMBER = 36
         DIMENSIONS = 4
         RPT_UNIT_NUM = MON_DV_TRANS_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                  'Generating Unit     ','C',INT(25,2),'V','D',INT(25,2)
         R_NEXT_REC = NEXT_REC + 2
!
         R_RECORD_LENGTH = RECORD_LENGTH
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MON_DV_TRANS_UNIT_HEADER = MON_DV_TRANS_NO
      RETURN

      ENTRY MON_NU_TRANS_UNIT_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER, &
                                     R_RECORD_LENGTH)

! CLREPORT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                trim(GET_SCENAME())//MON_NU_TRANS_CODE
         OVERHEAD_LENGTH = 64 !42
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
!         VARIABLE_NUMBER = 36
         DIMENSIONS = 4
         RPT_UNIT_NUM = MON_NU_TRANS_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         call write_log_entry("dsex_obj:0012", "Establish_detail_report_file(" // &
            "Filename: " // trim(file_name) // ")")
            
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                  'Generating Unit     ','C',INT(25,2),'V','D',INT(25,2)
         R_NEXT_REC = NEXT_REC + 2
!
         R_RECORD_LENGTH = RECORD_LENGTH
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MON_NU_TRANS_UNIT_HEADER = MON_NU_TRANS_NO
      RETURN

      ENTRY ANN_POOL_SUMMARY_HEADER(R_NEXT_REC)

! CLREPORT
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                           trim(GET_SCENAME())//".PSD"
         OVERHEAD_LENGTH = 14
         VARIABLE_NUMBER = 12
         DIMENSIONS = 3
         RPT_UNIT_NUM = ANN_POOL_SUM_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Member              ','C',INT(6,2),'F','D'
         R_NEXT_REC = NEXT_REC + 1
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         ANN_POOL_SUMMARY_HEADER = ANN_POOL_SUM_NO
      RETURN

      ENTRY MON_CL_SUMMARY_HEADER(R_NEXT_REC)

! CLREPORT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                           trim(GET_SCENAME())//".TTD"
         OVERHEAD_LENGTH = 17
         VARIABLE_NUMBER = 31
         DIMENSIONS = 3
         RPT_UNIT_NUM = MON_CL_SUM_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'
         R_NEXT_REC = NEXT_REC + 1
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MON_CL_SUMMARY_HEADER = MON_CL_SUM_NO
      RETURN

      ENTRY TRANSACT_FUEL_RPT_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! CLREPORT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                   trim(GET_SCENAME())//TRANS_FUL_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 4
         RPT_UNIT_NUM = TRANS_FUL_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                            'Transaction Group   ','C',INT(35,2),'V','D'
         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANSACT_FUEL_RPT_HEADER = TRANS_FUL_UNIT
      RETURN

      ENTRY TRANSACT_STATE_RPT_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! CLREPORT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                 trim(GET_SCENAME())//TRANS_STATE_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 4
         RPT_UNIT_NUM = TRANS_STATE_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                            'State               ','C',INT(20,2),'V','D'
         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANSACT_STATE_RPT_HEADER = TRANS_STATE_UNIT
      RETURN

      ENTRY STATE_RPS_RPT_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! CLREPORT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                   trim(GET_SCENAME())//STATE_RPS_CODE
         OVERHEAD_LENGTH = 37
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 4
         RPT_UNIT_NUM = STATE_RPS_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                            'State or Province   ','C',INT(20,2),'V','D'
         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         STATE_RPS_RPT_HEADER = STATE_RPS_UNIT
      RETURN

      ENTRY RPS_PROGRAMS_RPT_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! CLREPORT NOT A RESET REPORT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                               trim(GET_SCENAME())//RPS_PROGRAMS_CODE
         OVERHEAD_LENGTH = 58
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 3
         RPT_UNIT_NUM = RPS_PROGRAMS_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                            'Program             ','C',INT(50,2),'V','D'
         R_NEXT_REC = NEXT_REC + 1
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
!
         RPS_PROGRAMS_RPT_HEADER = RPS_PROGRAMS_UNIT
      RETURN

      ENTRY CO2_ABATEMENT_RPT_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! CLREPORT NOT A RESET REPORT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                               trim(GET_SCENAME())//CO2_ABATEMENT_CODE
         OVERHEAD_LENGTH = 60
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 3
         IF(CAPACITY_PLANNING_METHOD() == 'MX'  .AND. &
                              GREEN_MRX_METHOD() == 'GX') DIMENSIONS = 4
         RPT_UNIT_NUM = CO2_ABATEMENT_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                            'Unit                ','C',INT(48,2),'V','D'
         R_NEXT_REC = NEXT_REC + 1
         IF(CAPACITY_PLANNING_METHOD() == 'MX'  .AND. &
                                        GREEN_MRX_METHOD() == 'GX') THEN
            WRITE(RPT_UNIT_NUM,REC=R_NEXT_REC) &
                             'Iteration           ','N',INT(4,2),'F','D'
            R_NEXT_REC = R_NEXT_REC + 1
         ENDIF
!
         CO2_ABATEMENT_RPT_HEADER = CO2_ABATEMENT_UNIT
      RETURN

      ENTRY MARGINAL_UNIT_TECH_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! MARGNOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                          trim(GET_SCENAME())//MARGINAL_UNIT_TECH_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 4
         RPT_UNIT_NUM = MARGINAL_UNIT_TECH_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                            'Transaction Group   ','C',INT(35,2),'V','D'
         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MARGINAL_UNIT_TECH_HEADER = MARGINAL_UNIT_TECH_UNIT
      RETURN

      ENTRY TRANSACT_PROD_RPT_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! CLREPORT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                  trim(GET_SCENAME())//TRANS_PROD_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 4
         RPT_UNIT_NUM = TRANS_PROD_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                            'Transaction Group   ','C',INT(35,2),'V','D'
         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANSACT_PROD_RPT_HEADER = TRANS_PROD_UNIT
      RETURN

      ENTRY TRAN_PROD_FUEL_RPT_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! CLREPORT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                              trim(GET_SCENAME())//TRAN_PROD_FUEL_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 4
         RPT_UNIT_NUM = TRAN_PROD_FUEL_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                            'Transaction Group   ','C',INT(35,2),'V','D'
         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRAN_PROD_FUEL_RPT_HEADER = TRAN_PROD_FUEL_UNIT
      RETURN

      ENTRY TRANSACT_PROD_PROD_RPT_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! CLREPORT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                             trim(GET_SCENAME())//TRANS_PROD_PROD_CODE
         OVERHEAD_LENGTH = 80
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 5
         RPT_UNIT_NUM = TRANS_PROD_PROD_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                            'Transaction Group   ','C',INT(35,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Product Name        ','C',INT(20,2),'V','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANSACT_PROD_PROD_RPT_HEADER = TRANS_PROD_PROD_UNIT
      RETURN

      ENTRY TRANSACT_PROD_MW_RPT_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! CLREPORT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                             trim(GET_SCENAME())//TRANS_PROD_MW_CODE
         OVERHEAD_LENGTH = 80 !57
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 5
         RPT_UNIT_NUM = TRANS_PROD_MW_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                            'Transaction Group   ','C',INT(35,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Product Name        ','C',INT(20,2),'V','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANSACT_PROD_MW_RPT_HEADER = TRANS_PROD_MW_UNIT
      RETURN

      ENTRY RPS_CURVE_RPT_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! CLREPORT
!
         FILE_NAME = TRIM(GET_RESULTS_DIRECTORY())//"MSG"// &
                                  TRIM(GET_SCENAME())//RPS_CURVE_CODE
         OVERHEAD_LENGTH = 83 ! 64
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 4
         RPT_UNIT_NUM = RPS_CURVE_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                            'Program             ','C',INT(50,2),'V','D'
!         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
!     +                       'Iteration           ','N',INT2(4),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                            'Resource            ','C',INT(25,2),'V','D'
         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         RPS_CURVE_RPT_HEADER = RPS_CURVE_UNIT
      RETURN

      ENTRY TRANSACT_PROD_FUEL_RPT_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! CLREPORT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                             trim(GET_SCENAME())//TRANS_PROD_FUEL_CODE
         OVERHEAD_LENGTH = 80!57
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 5
         RPT_UNIT_NUM = TRANS_PROD_FUEL_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                            'Transaction Group   ','C',INT(35,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Product Name        ','C',INT(20,2),'V','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANSACT_PROD_FUEL_RPT_HEADER = TRANS_PROD_FUEL_UNIT
      RETURN

      ENTRY SERVICE_TRANS_HEADER(R_NEXT_REC)

! SERVICAC FILE
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                           trim(GET_SCENAME())//".STD"
         OVERHEAD_LENGTH = 56
         VARIABLE_NUMBER = 7
         DIMENSIONS = 6
         RPT_UNIT_NUM = TRANS_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                            'Description         ','C',INT(20,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                             'Revenue/Expense     ','C',INT(7,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+3) &
                            'Type                ','C',INT(12,2),'V','D'
         R_NEXT_REC = NEXT_REC + 4
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         SERVICE_TRANS_HEADER = TRANS_NO
      RETURN

      ENTRY ANN_EL_UNIT_HEADER(R_NEXT_REC)

! NOT FOUND IN ANY ROUTINE

         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                           trim(GET_SCENAME())//".EAD"
         OVERHEAD_LENGTH = 28
         VARIABLE_NUMBER = 16
         DIMENSIONS = 3
         RPT_UNIT_NUM = ANN_EL_UNIT_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                            'Generating Unit     ','C',INT(20,2),'V','D'
         R_NEXT_REC = NEXT_REC + 1
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         ANN_EL_UNIT_HEADER = ANN_EL_UNIT_NO
!
      RETURN

      ENTRY MON_EL_UNIT_HEADER(R_NEXT_REC)


         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                           trim(GET_SCENAME())//".EMD"
         OVERHEAD_LENGTH = 40
         VARIABLE_NUMBER = 11
         DIMENSIONS = 4
         RPT_UNIT_NUM = MON_EL_UNIT_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                            'Energy Limited Unit ','C',INT(20,2),'V','D'
         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MON_EL_UNIT_HEADER = MON_EL_UNIT_NO
!
      RETURN

      ENTRY ASSET_PROD_RPT_HEADER(R_NEXT_REC)

! ENRGLIMT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                  trim(GET_SCENAME())//ASSET_PROD_CODE
         OVERHEAD_LENGTH = 38
         VARIABLE_NUMBER = 8
         DIMENSIONS = 3
         RPT_UNIT_NUM = ASSET_PRODUCTION_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                            'Asset Class Name    ','C',INT(30,2),'V','D'
         R_NEXT_REC = NEXT_REC + 1
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         ASSET_PROD_RPT_HEADER = ASSET_PRODUCTION_UNIT
      RETURN

      ENTRY SCREENING_HEADER(R_NEXT_REC)

! CAP_OBJT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                           trim(GET_SCENAME())//".SCD"
         OVERHEAD_LENGTH = 28
         VARIABLE_NUMBER = 11
         DIMENSIONS = 3
         RPT_UNIT_NUM = SCREENING_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                            'Expansion Option    ','C',INT(20,2),'V','D'
         R_NEXT_REC = NEXT_REC + 1
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         SCREENING_HEADER = SCREENING_NO
      RETURN

      ENTRY MX_ANNUAL_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! CAP_OBJT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                               trim(GET_SCENAME())//MX_ANNUAL_ALT_CODE
         OVERHEAD_LENGTH = 70 !50
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = MX_ANNUAL_ALT_NO
         DIMENSIONS = 4
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                            'Transaction Group   ','C',INT(35,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                            'Resource            ','C',INT(22,2),'F','D'
         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MX_ANNUAL_HEADER = MX_ANNUAL_ALT_NO
      RETURN

      ENTRY GRX_ANNUAL_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! CAP_OBJT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                       trim(GET_SCENAME())//GRX_DETAILED_RESOURCE_CODE
         OVERHEAD_LENGTH = 76
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = GRX_DETAILED_RESOURCE_UNIT
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                            'Transaction Group   ','C',INT(35,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                            'Resource            ','C',INT(22,2),'F','D'
         R_NEXT_REC = NEXT_REC + 2
         WRITE(RPT_UNIT_NUM,REC=R_NEXT_REC) &
                             'Iteration           ','N',INT(4,2),'F','D'
         R_NEXT_REC = R_NEXT_REC + 1
!
         GRX_ANNUAL_HEADER = GRX_DETAILED_RESOURCE_UNIT
      RETURN

      ENTRY GRX_8761_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! CAP_OBJT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                    trim(GET_SCENAME())//GRX_8761_CODE
         OVERHEAD_LENGTH = 63
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = GRX_8761_UNIT
         DIMENSIONS = 4
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                            'Variable            ','C',INT(20,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                            'Selling Trans Group ','C',INT(35,2),'F','D'
         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
!
         GRX_8761_HEADER = GRX_8761_UNIT
      RETURN

      ENTRY GAS_PIPE_EXPANSION_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! CAP_OBJT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                          trim(GET_SCENAME())//GAS_PIPE_EXPANSION_CODE
         OVERHEAD_LENGTH = 58
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = GAS_PIPE_EXPANSION_UNIT
         DIMENSIONS = 3
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                            'Pipeline Expansion  ','C',INT(50,2),'F','D'
         R_NEXT_REC = NEXT_REC + 1
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         GAS_PIPE_EXPANSION_HEADER = GAS_PIPE_EXPANSION_UNIT
      RETURN

      ENTRY MX_ICAP_ANNUAL_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! CAP_OBJT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                          trim(GET_SCENAME())//MX_ICAP_ANNUAL_ALT_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = MX_ICAP_ANNUAL_ALT_NO
         DIMENSIONS = 4
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                            'Transaction Group   ','C',INT(35,2),'V','D'
         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MX_ICAP_ANNUAL_HEADER = MX_ICAP_ANNUAL_ALT_NO
      RETURN

      ENTRY CM_MAINT_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! NEW CLM_OBJT AVAILABILITY REPORT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                    trim(GET_SCENAME())//CM_MAINT_CODE
         OVERHEAD_LENGTH = 34
!         OVERHEAD_LENGTH = 28
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 3
         RPT_UNIT_NUM = CM_MAINT_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                            'Generating Unit     ','C',INT(25,2),'V','D'

         R_NEXT_REC = NEXT_REC + 1
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         CM_MAINT_HEADER = CM_MAINT_NO
      RETURN

      ENTRY MAINTENANCE_HEADER(R_MW_MAINT_NO, &
                               R_CL_NEXT_REC, &
                               R_MW_NEXT_REC)

! CLA_OBJT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                           trim(GET_SCENAME())//".UMD"
         OVERHEAD_LENGTH = 28
         VARIABLE_NUMBER = 39
         DIMENSIONS = 3
         RPT_UNIT_NUM = CL_MAINT_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                            'Generating Unit     ','C',INT(20,2),'V','D'
         R_CL_NEXT_REC = NEXT_REC + 1
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_CL_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MAINTENANCE_HEADER = CL_MAINT_NO
!
! OPENS GROUP MAINTENANCE HEADER

         R_MW_MAINT_NO = MW_MAINT_NO
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                           trim(GET_SCENAME())//".WMD"
         OVERHEAD_LENGTH = 28
         VARIABLE_NUMBER = 39
         DIMENSIONS = 3
         RPT_UNIT_NUM = MW_MAINT_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Group Name          ','C',INT(9,2),'V','D'
         R_MW_NEXT_REC = NEXT_REC + 1
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_MW_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
      RETURN

      ENTRY MARGINAL_COST_HEADER(R_NEXT_REC)

! MARGNOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                           trim(GET_SCENAME())//".MCD"
         OVERHEAD_LENGTH = 21
         VARIABLE_NUMBER = 4
         DIMENSIONS = 4
         RPT_UNIT_NUM = MAR_COST_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Load Segment        ','N',INT(4,2),'V','D'
         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MARGINAL_COST_HEADER = MAR_COST_NO
      RETURN

      ENTRY TRANS_CONTRIBUTION_RPT_HEADER(R_NEXT_REC)

! MARGNOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                trim(GET_SCENAME())//TRANS_CON_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 5
         DIMENSIONS = 5
         RPT_UNIT_NUM = TRANS_CON_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Hour of the Month   ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Transaction Group   ','C',INT(35,2),'V','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_CONTRIBUTION_RPT_HEADER = TRANS_CON_UNIT
      RETURN

      ENTRY MARGINAL_DATA_BASE_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! MARGNOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                           trim(GET_SCENAME())//".MGD"
         OVERHEAD_LENGTH = 20
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RECORD_LENGTH = MAX(1000,OVERHEAD_LENGTH + 4*VARIABLE_NUMBER)
         OPEN(MAR_DATA_NO,FILE=FILE_NAME, &
                    ACCESS="DIRECT",STATUS="REPLACE",RECL=RECORD_LENGTH)
!
! 1\8\96 END_POINTS MAY NOT BE DEFINED.
         WRITE(MAR_DATA_NO,REC=1) GET_NUM_OF_END_POINTS(), &
                                  INT(get_endyr()-get_base_year(),2)
         R_NEXT_REC = 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MARGINAL_DATA_BASE_HEADER = MAR_DATA_NO
      RETURN

      ENTRY ECONOMY_INTER_HEADER(R_NEXT_REC)

! ECONINTR
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                           trim(GET_SCENAME())//".ECD"
         OVERHEAD_LENGTH = 21
         VARIABLE_NUMBER = 12
         DIMENSIONS = 4
         RPT_UNIT_NUM = ECONOMY_INTER_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Transaction Point   ','N',INT(4,2),'V','D'
         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         ECONOMY_INTER_HEADER = ECONOMY_INTER_NO
      RETURN

      ENTRY GET_EXT_ECONOMY_INTER_NO

! ECONINTR
!
        GET_EXT_ECONOMY_INTER_NO = EXTERNAL_ECONOMY_INTER_NO
      RETURN

      ENTRY DISPATCH_ORDER_HEADER(R_NEXT_REC)

! PICADISP
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                           trim(GET_SCENAME())//".DOD"
         OVERHEAD_LENGTH = 39
         VARIABLE_NUMBER = 40
         DIMENSIONS = 4
         RPT_UNIT_NUM = DISP_ORDER_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                            'Capacity Segment    ','C',INT(22,2),'V','D'
         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         DISPATCH_ORDER_HEADER = DISP_ORDER_NO
      RETURN

      ENTRY TRANS_DISPATCH_RPT_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! PICADISP
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                        trim(GET_SCENAME())//MONTHLY_DISPATCH_RPT_CODE
         OVERHEAD_LENGTH = 80
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = MONTHLY_DISPATCH_RPT_NO
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                            'Tranaction Group    ','C',INT(40,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Period              ','C',INT(9,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Capacity Segment    ','C',INT(22,2),'V','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_DISPATCH_RPT_HEADER = RPT_UNIT_NUM
      RETURN

      ENTRY NEW_SUPPLY_CURVE_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! PICADISP
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                        trim(GET_SCENAME())//NEW_SUPPLY_CURVE_CODE
         OVERHEAD_LENGTH = 72
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = NEW_SUPPLY_CURVE_UNIT
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                            'Transaction Group   ','C',INT(35,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Generating Resource ','C',INT(20,2),'V','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         NEW_SUPPLY_CURVE_HEADER = RPT_UNIT_NUM
      RETURN

      ENTRY CAPEX_THERMAL_RPT_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! IN PICADISP
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                          trim(GET_SCENAME())//".XCD"
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = 3247 ! MONTHLY_DISPATCH_RPT_NO
         DIMENSIONS = 4
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','N',INT(4,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                            'Unit Name           ','C',INT(48,2),'V','D'
         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         CAPEX_THERMAL_RPT_HEADER = RPT_UNIT_NUM
      RETURN

      ENTRY WABASH_VALLEY_POWER_COST_HEADER(R_NEXT_REC)

! REAENRG
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                    trim(GET_SCENAME())//WABASH_VALLEY_POWER_COST_CODE
         OVERHEAD_LENGTH = 38
         VARIABLE_NUMBER = 30 ! 03/19/04 ! ADDED TRACKERS 20
         DIMENSIONS = 4
         RPT_UNIT_NUM = WABASH_VALLEY_POWER_COST_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                            'Resource            ','C',INT(20,2),'V','D'
         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         WABASH_VALLEY_POWER_COST_HEADER = WABASH_VALLEY_POWER_COST_UNIT
      RETURN

      ENTRY WABASH_VALLEY_POWER_COST_NO

         WABASH_VALLEY_POWER_COST_NO = WABASH_VALLEY_POWER_COST_UNIT
      RETURN

      ENTRY WABASH_VALLEY_POWER_DSM_HEADER(R_NEXT_REC)

! REAENRG
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                     trim(GET_SCENAME())//WABASH_VALLEY_POWER_DSM_CODE
         OVERHEAD_LENGTH = 32
         VARIABLE_NUMBER = 3
         DIMENSIONS = 4
         RPT_UNIT_NUM = WABASH_VALLEY_POWER_DSM_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                            'Area                ','C',INT(15,2),'V','D'
         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         WABASH_VALLEY_POWER_DSM_HEADER = WABASH_VALLEY_POWER_DSM_UNIT
      RETURN

      ENTRY WV_AREA_REPORT_HEADER(R_NEXT_REC)

! REAENRG
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                     trim(GET_SCENAME())//WV_AREA_CODE
         OVERHEAD_LENGTH = 32
         VARIABLE_NUMBER = 13
         DIMENSIONS = 4
         RPT_UNIT_NUM = WV_AREA_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                            'Area                ','C',INT(15,2),'V','D'
         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         WV_AREA_REPORT_HEADER = WV_AREA_UNIT
      RETURN

      ENTRY ASSET_CLASS_INCOME_HEADER(R_START_RECORD,R_VARIABLE_NUMBER)

! ASSET_CLASS_ANALYSIS
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                           trim(GET_SCENAME())//".AID"
         OVERHEAD_LENGTH = 46
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 3
         RPT_UNIT_NUM = ASSET_INCOME_STATEMENT_NUM
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                            'Asset Class Name    ','C',INT(38,2),'F','D'
!
         R_START_RECORD = NEXT_REC + 1 !3 + DIMENSIONS
         ASSET_CLASS_INCOME_HEADER = ASSET_INCOME_STATEMENT_NUM
      RETURN

      ENTRY FE_PCA_LEGAL_HEADER()

! ASSET_CLASS_ANALYSIS
!
         FILE_NAME = TRIM(GET_RESULTS_DIRECTORY())//"MSG"// &
                                           TRIM(GET_SCENAME())//".FED"
         OVERHEAD_LENGTH = 46
         VARIABLE_NUMBER = 300
         DIMENSIONS = 3
         RPT_UNIT_NUM = 2710
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                            'Asset Class Name    ','C',INT(38,2),'F','D'
!
         FE_PCA_LEGAL_HEADER = RPT_UNIT_NUM
      RETURN

      ENTRY FE_MONTHLY_PCA_LEGAL_HEADER()

! ASSET_CLASS_ANALYSIS
!
         FILE_NAME = TRIM(GET_RESULTS_DIRECTORY())//"MSG"// &
                                           TRIM(GET_SCENAME())//".MFD"
         OVERHEAD_LENGTH = 46
         VARIABLE_NUMBER = 300
         DIMENSIONS = 4
         RPT_UNIT_NUM = 6742
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                            'Asset Class Name    ','C',INT(38,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                           'Period              ','C',INT(10,2),'V','D'
!
         FE_MONTHLY_PCA_LEGAL_HEADER = RPT_UNIT_NUM
      RETURN

      ENTRY CPL_EMC_CAP_REVS_RPT_HEADER(R_NEXT_REC)

! CAT2OBJT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                    trim(GET_SCENAME())//CPL_ECAP_CODE
         OVERHEAD_LENGTH = 17
         VARIABLE_NUMBER = 21
         DIMENSIONS = 3
         RPT_UNIT_NUM = CPL_ECAP_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         R_NEXT_REC = NEXT_REC + 1
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         CPL_EMC_CAP_REVS_RPT_HEADER = CPL_ECAP_UNIT
      RETURN

      ENTRY CPL_PAEN_RPT_HEADER(R_NEXT_REC)

! CAT2OBJT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                    trim(GET_SCENAME())//CPL_PAEN_CODE
         OVERHEAD_LENGTH = 37
         VARIABLE_NUMBER = 10
         DIMENSIONS = 4
         RPT_UNIT_NUM = CPL_PAEN_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                            'PA Resource         ','C',INT(20,2),'V','D'
         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         CPL_PAEN_RPT_HEADER = CPL_PAEN_UNIT
      RETURN

      ENTRY HOURLY_ECAP_RPT_HEADER(R_NEXT_REC)

! CAT2OBJT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                 trim(GET_SCENAME())//HOURLY_ECAP_CODE
         OVERHEAD_LENGTH = 21
         VARIABLE_NUMBER = 25
         DIMENSIONS = 4
         RPT_UNIT_NUM = HOURLY_ECAP_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Hour of Month       ','N',INT(4,2),'V','D'
         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         HOURLY_ECAP_RPT_HEADER = HOURLY_ECAP_UNIT
      RETURN

      ENTRY CPL_SALES_GEN_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! CAT2OBJT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                               trim(GET_SCENAME())//CPL_SALES_GEN_CODE
         OVERHEAD_LENGTH = 68
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = CPL_SALES_GEN_UNIT
         DIMENSIONS = 4
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                            'Asset Class Name    ','C',INT(38,2),'F','D'
         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         CPL_SALES_GEN_HEADER = CPL_SALES_GEN_UNIT
      RETURN

      ENTRY CPL_DEFERRED_FUEL_RPT_HEADER(R_NEXT_REC)

! CPLDEFUL
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"CPL"// &
                           trim(GET_SCENAME())//CPL_DEFERRED_FUEL_CODE
         OVERHEAD_LENGTH = 47
         VARIABLE_NUMBER = 50
         DIMENSIONS = 3
         RPT_UNIT_NUM = CPL_DEFERRRED_FUEL_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(6,2),'F','D'
         R_NEXT_REC = NEXT_REC + 1
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         CPL_DEFERRED_FUEL_RPT_HEADER = CPL_DEFERRRED_FUEL_UNIT
      RETURN

      ENTRY TRANS_TIE_RPT_HEADER(R_NEXT_REC)

! TRANSOBJ
!

         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                   trim(GET_SCENAME())//TRANS_TIE_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 24
         DIMENSIONS = 5
         RPT_UNIT_NUM = TRANS_TIE_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day of Month        ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Transaction Group   ','C',INT(35,2),'V','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_TIE_RPT_HEADER = TRANS_TIE_UNIT
      RETURN

      ENTRY DAILY_PRODUCTS_RPT_HEADER(R_NEXT_REC)

! TRANSOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                  trim(GET_SCENAME())//DAILY_PROD_CODE
         OVERHEAD_LENGTH = 90
         VARIABLE_NUMBER = 9 ! 8
         DIMENSIONS = 5
         RPT_UNIT_NUM = DAILY_PRODUCTS_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ', &
                                                    'C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Transaction Group   ','C', &
                                                       INT(35,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                             'Type of Product     ', &
                                                   'C',INT(20,2),'V','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         DAILY_PRODUCTS_RPT_HEADER = DAILY_PRODUCTS_UNIT
      RETURN

      ENTRY SCENARIO_MAKER_RPT_HEADER(R_NEXT_REC)

! TRANSOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                              trim(GET_SCENAME())//SCENARIO_MAKER_CODE
         OVERHEAD_LENGTH =  42
         VARIABLE_NUMBER = 16
         DIMENSIONS = 4
         RPT_UNIT_NUM = SCENARIO_MAKER_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)

         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Time Frame          ','C', &
                                                        INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Uncertainty Variable', &
                                                   'C',INT(25,2),'V','D'
         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         SCENARIO_MAKER_RPT_HEADER = SCENARIO_MAKER_UNIT
      RETURN

      ENTRY REGIONAL_MAKER_RPT_HEADER(R_NEXT_REC)

! TRANSOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                              trim(GET_SCENAME())//REGIONAL_MAKER_CODE
         OVERHEAD_LENGTH =  46
         VARIABLE_NUMBER = 16
         DIMENSIONS = 5
         RPT_UNIT_NUM = REGIONAL_MAKER_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)

         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Time Frame          ','C', &
                                                        INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Uncertainty Variable', &
                                                   'C',INT(25,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                             'Transaction Group   ','N', &
                                                        INT(4,2),'V','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         REGIONAL_MAKER_RPT_HEADER = REGIONAL_MAKER_UNIT
      RETURN

      ENTRY MONTHLY_FUEL_DERIV_RPT_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)

! TRANSOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                          trim(GET_SCENAME())//MONTHLY_FUEL_DERIV_CODE
         OVERHEAD_LENGTH =  37
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 4
         RPT_UNIT_NUM = MONTHLY_FUEL_DERIV_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ', &
                                                    'C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Fuel Derivative     ','C', &
                                                       INT(20,2),'V','D'
         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MONTHLY_FUEL_DERIV_RPT_HEADER = MONTHLY_FUEL_DERIV_UNIT
      RETURN

      ENTRY C_DEPTH_MARKET_RPT_HEADER(R_NEXT_REC)

! TRANSOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                              trim(GET_SCENAME())//C_DEPTH_MARKET_CODE
         OVERHEAD_LENGTH = 72
         VARIABLE_NUMBER = 4
         DIMENSIONS = 5
         RPT_UNIT_NUM = C_DEPTH_MARKET_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ', &
                                                    'C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Transaction Group   ','C', &
                                                       INT(35,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                             'Type of Product     ', &
                                                   'C',INT(20,2),'V','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         C_DEPTH_MARKET_RPT_HEADER = C_DEPTH_MARKET_UNIT
      RETURN

      ENTRY C_DEPTH_MARGIN_RPT_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)

! TRANSOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                              trim(GET_SCENAME())//C_DEPTH_MARGIN_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 5
         RPT_UNIT_NUM = C_DEPTH_MARGIN_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ', &
                                                    'C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Transaction Group   ','C', &
                                                       INT(35,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                             'Depth Interval      ', &
                                                    'N',INT(4,2),'V','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         C_DEPTH_MARGIN_RPT_HEADER = C_DEPTH_MARGIN_UNIT
      RETURN

      ENTRY TRANS_DEL_RPT_HEADER(R_NEXT_REC)

! TRANSOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                   trim(GET_SCENAME())//TRANS_DEL_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 24
         DIMENSIONS = 5
         RPT_UNIT_NUM = TRANS_DEL_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ', &
                                                    'C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day of Month        ', &
                                                    'N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                             'Transaction Group   ','C', &
                                                       INT(35,2),'V','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_DEL_RPT_HEADER = TRANS_DEL_UNIT
      RETURN

      ENTRY TRANS_PRI_RPT_HEADER(R_NEXT_REC)

! TRANSOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                   trim(GET_SCENAME())//TRANS_PRI_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 24
         DIMENSIONS = 5
         RPT_UNIT_NUM = TRANS_PRI_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day of Month        ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Transaction Group   ','C',INT(35,2),'V','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_PRI_RPT_HEADER = TRANS_PRI_UNIT
      RETURN

      ENTRY TRANS_SCA_RPT_HEADER(R_NEXT_REC)

! TRANSOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                   trim(GET_SCENAME())//TRANS_SCA_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 24
         DIMENSIONS = 5
         RPT_UNIT_NUM = TRANS_SCA_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day of Month        ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Transaction Group   ','C',INT(35,2),'V','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_SCA_RPT_HEADER = TRANS_SCA_UNIT
      RETURN

      ENTRY TRANS_MCB_RPT_HEADER(R_NEXT_REC)

! TRANSOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                   trim(GET_SCENAME())//TRANS_MCB_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 24
         DIMENSIONS = 5
         RPT_UNIT_NUM = TRANS_MCB_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day of Month        ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Transaction Group   ','C',INT(35,2),'V','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_MCB_RPT_HEADER = TRANS_MCB_UNIT
      RETURN

      ENTRY MARKET_AREA_RPT_HEADER(R_NEXT_REC)

! TRANSOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                 trim(GET_SCENAME())//MARKET_AREA_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 24 ! 30
         DIMENSIONS = 5
         RPT_UNIT_NUM = MARKET_AREA_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day of Month        ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Market Area         ','C',INT(20,2),'V','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MARKET_AREA_RPT_HEADER = MARKET_AREA_UNIT
      RETURN

      ENTRY GAS_MARKET_AREA_RPT_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)

! TRANSOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                             trim(GET_SCENAME())//DAILY_CUST_GAS_CODE
         OVERHEAD_LENGTH = 21
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 4
         RPT_UNIT_NUM = DAILY_CUST_GAS_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day of Month        ','N',INT(4,2),'V','D'

         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         GAS_MARKET_AREA_RPT_HEADER = DAILY_CUST_GAS_UNIT
      RETURN

      ENTRY GAS_MARKET_TRANS_RPT_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)

! TRANSOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                             trim(GET_SCENAME())//DAILY_TRANS_GAS_CODE
         OVERHEAD_LENGTH = 21
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 4
         RPT_UNIT_NUM = DAILY_TRANS_GAS_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day of Month        ','N',INT(4,2),'V','D'

         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         GAS_MARKET_TRANS_RPT_HEADER = DAILY_TRANS_GAS_UNIT
      RETURN

      ENTRY DAILY_GAS_SELL_BUY_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)

! TRANSOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                          trim(GET_SCENAME())//DAILY_GAS_SELL_BUY_CODE
         OVERHEAD_LENGTH = 41
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 5
         RPT_UNIT_NUM = DAILY_GAS_SELL_BUY_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day of Month        ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Gas Region          ','C',INT(20,2),'V','D'
         R_NEXT_REC = NEXT_REC + 3
!         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         DAILY_GAS_SELL_BUY_HEADER = DAILY_GAS_SELL_BUY_UNIT
      RETURN

      ENTRY DAILY_GAS_LINK_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)

! TRANSOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                          trim(GET_SCENAME())//DAILY_GAS_LINK_CODE
         OVERHEAD_LENGTH = 21
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 4
         RPT_UNIT_NUM = DAILY_GAS_LINK_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day of Month        ','N',INT(4,2),'V','D'

         R_NEXT_REC = NEXT_REC + 2

         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         DAILY_GAS_LINK_HEADER = DAILY_GAS_LINK_UNIT
      RETURN

      ENTRY DAILY_GAS_LINK_LIMIT_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)

! TRANSOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                        trim(GET_SCENAME())//DAILY_GAS_LINK_LIMIT_CODE
         OVERHEAD_LENGTH = 21
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 4
         RPT_UNIT_NUM = DAILY_GAS_LINK_LIMIT_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day of Month        ','N',INT(4,2),'V','D'

         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         DAILY_GAS_LINK_LIMIT_HEADER = DAILY_GAS_LINK_LIMIT_UNIT
      RETURN

      ENTRY DAILY_GAS_LINK_PRICE_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)

! GAS_OBJT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                        trim(GET_SCENAME())//DAILY_GAS_LINK_PRICE_CODE
         OVERHEAD_LENGTH = 21
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 4
         RPT_UNIT_NUM = DAILY_GAS_LINK_PRICE_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day of Month        ','N',INT(4,2),'V','D'
         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         DAILY_GAS_LINK_PRICE_HEADER = DAILY_GAS_LINK_PRICE_UNIT
      RETURN

      ENTRY DAILY_GAS_LINK_CF_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)

! GAS_OBJT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                        trim(GET_SCENAME())//DAILY_GAS_LINK_CF_CODE
         OVERHEAD_LENGTH = 21
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 4
         RPT_UNIT_NUM = DAILY_GAS_LINK_CF_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day of Month        ','N',INT(4,2),'V','D'
         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         DAILY_GAS_LINK_CF_HEADER = DAILY_GAS_LINK_CF_UNIT
      RETURN

      ENTRY MONTHLY_GAS_SELL_BUY_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)

! TRANSOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                        trim(GET_SCENAME())//MONTHLY_GAS_SELL_BUY_CODE
         OVERHEAD_LENGTH = 37
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 4
         RPT_UNIT_NUM = MONTHLY_GAS_SELL_BUY_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
!         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
!     +                       'Day of Month        ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                            'Gas Region          ','C',INT(20,2),'V','D'
         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MONTHLY_GAS_SELL_BUY_HEADER = MONTHLY_GAS_SELL_BUY_UNIT
      RETURN


      ENTRY DAILY_GAS_PRICE_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)

! TRANSOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                          trim(GET_SCENAME())//DAILY_GAS_PRICE_CODE
         OVERHEAD_LENGTH = 21
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 4
         RPT_UNIT_NUM = DAILY_GAS_PRICE_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day of Month        ','N',INT(4,2),'V','D'

         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         DAILY_GAS_PRICE_HEADER = DAILY_GAS_PRICE_UNIT
      RETURN

      ENTRY DAILY_GAS_NODE_VOLUME_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)

! TRANSOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                       trim(GET_SCENAME())//DAILY_GAS_NODE_VOLUME_CODE
         OVERHEAD_LENGTH = 21
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 4
         RPT_UNIT_NUM = DAILY_GAS_NODE_VOLUME_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day of Month        ','N',INT(4,2),'V','D'

         R_NEXT_REC = NEXT_REC + 2

         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         DAILY_GAS_NODE_VOLUME_HEADER = DAILY_GAS_NODE_VOLUME_UNIT
      RETURN

      ENTRY DAILY_GAS_STORAGE_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)

! TRANSOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                           trim(GET_SCENAME())//DAILY_GAS_STORAGE_CODE
         OVERHEAD_LENGTH = 21
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 4
         RPT_UNIT_NUM = DAILY_GAS_STORAGE_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day of Month        ','N',INT(4,2),'V','D'

         R_NEXT_REC = NEXT_REC + 2

         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         DAILY_GAS_STORAGE_HEADER = DAILY_GAS_STORAGE_UNIT
      RETURN

      ENTRY DAILY_GAS_SUR_DEF_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)

! TRANSOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                   trim(GET_SCENAME())//DAILY_SURPLUS_DEFICIT_CODE
         OVERHEAD_LENGTH = 21
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 4
         RPT_UNIT_NUM = DAILY_SURPLUS_DEFICIT_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day of Month        ','N',INT(4,2),'V','D'
         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         DAILY_GAS_SUR_DEF_HEADER = DAILY_SURPLUS_DEFICIT_UNIT
      RETURN

      ENTRY DAILY_GAS_AVE_PRICE_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)

! TRANSOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                         trim(GET_SCENAME())//DAILY_GAS_AVE_PRICE_CODE
         OVERHEAD_LENGTH = 21
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 4
         RPT_UNIT_NUM = DAILY_GAS_AVE_PRICE_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day of Month        ','N',INT(4,2),'V','D'

         R_NEXT_REC = NEXT_REC + 2

         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         DAILY_GAS_AVE_PRICE_HEADER = DAILY_GAS_AVE_PRICE_UNIT
      RETURN

      ENTRY DAILY_GAS_DIFFER_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)

! TRANSOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                          trim(GET_SCENAME())//DAILY_GAS_DIFFER_CODE
         OVERHEAD_LENGTH = 21
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 4
         RPT_UNIT_NUM = DAILY_GAS_DIFFER_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day of Month        ','N',INT(4,2),'V','D'

         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         DAILY_GAS_DIFFER_HEADER = DAILY_GAS_DIFFER_UNIT
      RETURN

      ENTRY MONTHLY_GAS_PRICE_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)

! TRANSOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                          trim(GET_SCENAME())//MONTHLY_GAS_PRICE_CODE
         OVERHEAD_LENGTH = 17
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 3
         IF(CAPACITY_PLANNING_METHOD() == 'MX'  .AND. &
                              GREEN_MRX_METHOD() == 'GX' .AND. &
                                   .NOT. HORIZONS_ACTIVE) THEN
            DIMENSIONS = 4
            OVERHEAD_LENGTH = 21
         ENDIF
         RPT_UNIT_NUM = MONTHLY_GAS_PRICE_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         IF(CAPACITY_PLANNING_METHOD() == 'MX'  .AND. &
                                 GREEN_MRX_METHOD() == 'GX' .AND. &
                                             .NOT. HORIZONS_ACTIVE) THEN
            WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Iteration           ','N',INT(4,2),'F','D'
            NEXT_REC = NEXT_REC + 1
         ENDIF
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'

         R_NEXT_REC = NEXT_REC + 1

         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MONTHLY_GAS_PRICE_HEADER = MONTHLY_GAS_PRICE_UNIT
      RETURN

      ENTRY MONTHLY_GAS_DIFFER_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)

! TRANSOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                          trim(GET_SCENAME())//MONTHLY_GAS_DIFFER_CODE
         OVERHEAD_LENGTH = 17
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 3
         RPT_UNIT_NUM = MONTHLY_GAS_DIFFER_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'

         R_NEXT_REC = NEXT_REC + 1

         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MONTHLY_GAS_DIFFER_HEADER = MONTHLY_GAS_DIFFER_UNIT
      RETURN


      ENTRY TRANS_HMP_RPT_HEADER(R_NEXT_REC)

! TRANSOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                   trim(GET_SCENAME())//TRANS_HMP_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 24 ! 30
         DIMENSIONS = 5
         RPT_UNIT_NUM = TRANS_HMP_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day of Month        ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Transaction Group   ','C',INT(35,2),'V','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_HMP_RPT_HEADER = TRANS_HMP_UNIT
      RETURN

      ENTRY TRANS_LDS_RPT_HEADER(R_NEXT_REC)

! TRANSOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                   trim(GET_SCENAME())//TRANS_LDS_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 24
         DIMENSIONS = 5
         RPT_UNIT_NUM = TRANS_LDS_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day of Month        ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Transaction Group   ','C',INT(35,2),'V','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_LDS_RPT_HEADER = TRANS_LDS_UNIT
      RETURN

      ENTRY TRANS_LAS_RPT_HEADER(R_NEXT_REC)

! TRANSOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                   trim(GET_SCENAME())//TRANS_LAS_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 24
         DIMENSIONS = 5
         RPT_UNIT_NUM = TRANS_LAS_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day of Month        ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Transaction Group   ','C',INT(35,2),'V','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         if(.not. is_unit_in_use(int(trans_las_unit))) then
            call end_program("dsex_obj:0007 - TRANS_LAS_UNIT (" // &
                trim(itos(int(trans_las_unit))) // ") " // &
                "has not been opened.")
         end if
         TRANS_LAS_RPT_HEADER = TRANS_LAS_UNIT
      RETURN

      ENTRY TRANS_MRK_RPT_HEADER(R_NEXT_REC)

! TRANSOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                   trim(GET_SCENAME())//TRANS_MRK_CODE
         OVERHEAD_LENGTH = 21
         VARIABLE_NUMBER = 24
         DIMENSIONS = 4
         RPT_UNIT_NUM = TRANS_MRK_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day of Month        ','N',INT(4,2),'V','D'
         R_NEXT_REC = NEXT_REC + 2

         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_MRK_RPT_HEADER = TRANS_MRK_UNIT
      RETURN

      ENTRY TRANS_PB4_RPT_HEADER(R_NEXT_REC)

! TRANSOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                   trim(GET_SCENAME())//TRANS_PB4_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 24
         DIMENSIONS = 5
         RPT_UNIT_NUM = TRANS_PB4_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day of Month        ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Transaction Group   ','C',INT(35,2),'V','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_PB4_RPT_HEADER = TRANS_PB4_UNIT
      RETURN

      ENTRY HOUR_DIAG_RPT_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! TRANSOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                   trim(GET_SCENAME())//HOUR_DIAG_CODE
         OVERHEAD_LENGTH = 25
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 5
         RPT_UNIT_NUM = HOUR_DIAG_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Hour of Month       ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                             'Transaction Number  ','N',INT(4,2),'V','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         HOUR_DIAG_RPT_HEADER = HOUR_DIAG_UNIT
      RETURN

      ENTRY HOUR_JDA_RPT_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! TRANSOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                   trim(GET_SCENAME())//HOUR_JDA_CODE
         OVERHEAD_LENGTH = 25
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 5
         RPT_UNIT_NUM = HOUR_JDA_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day of Month        ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                             'Hour of Month       ','N',INT(4,2),'V','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         HOUR_JDA_RPT_HEADER = HOUR_JDA_UNIT
      RETURN

      ENTRY DEPTH_MARKET_RPT_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! TRANSOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                trim(GET_SCENAME())//DEPTH_MARKET_CODE
         OVERHEAD_LENGTH = 25
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 5
         RPT_UNIT_NUM = DEPTH_MARKET_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day of Month        ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                             'Market Interval     ','N',INT(4,2),'V','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         DEPTH_MARKET_RPT_HEADER = DEPTH_MARKET_UNIT
      RETURN

      ENTRY TRANS_DEPTH_MARKET_RPT_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! TRANSOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                          trim(GET_SCENAME())//TRANS_DEPTH_MARKET_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 6
         RPT_UNIT_NUM = TRANS_DEPTH_MARKET_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day of Month        ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Transaction Group   ','C',INT(35,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+3) &
                             'Market Interval     ','C',INT(6,2),'V','D'
         R_NEXT_REC = NEXT_REC + 4
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_DEPTH_MARKET_RPT_HEADER = TRANS_DEPTH_MARKET_UNIT
      RETURN

      ENTRY TRANS_PAF_RPT_HEADER(R_NEXT_REC)

! TRANSOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                   trim(GET_SCENAME())//TRANS_PAF_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 24
         DIMENSIONS = 5
         RPT_UNIT_NUM = TRANS_PAF_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day of Month        ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Transaction Group   ','C',INT(35,2),'V','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_PAF_RPT_HEADER = TRANS_PAF_UNIT
      RETURN

      ENTRY TRANS_CAP_RPT_HEADER(R_NEXT_REC)

! TRANSOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                   trim(GET_SCENAME())//TRANS_CAP_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 24
         DIMENSIONS = 5
         RPT_UNIT_NUM = TRANS_CAP_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day of Month        ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Transaction Group   ','C',INT(35,2),'V','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_CAP_RPT_HEADER = TRANS_CAP_UNIT
      RETURN

      ENTRY TRANS_DER_RPT_HEADER(R_NEXT_REC)

! TRANSOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                   trim(GET_SCENAME())//TRANS_DER_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 24
         DIMENSIONS = 5
         RPT_UNIT_NUM = TRANS_DER_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day of Month        ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Transaction Group   ','C',INT(35,2),'V','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_DER_RPT_HEADER = TRANS_DER_UNIT
      RETURN

      ENTRY TRANS_EUE_RPT_HEADER(R_NEXT_REC)

! TRANSOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                   trim(GET_SCENAME())//TRANS_EUE_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 24
         DIMENSIONS = 5
         RPT_UNIT_NUM = TRANS_EUE_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day of Month        ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Transaction Group   ','C',INT(35,2),'V','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_EUE_RPT_HEADER = TRANS_EUE_UNIT
      RETURN

      ENTRY TRANS_TRM_RPT_HEADER(R_NEXT_REC)

! TRANSOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                   trim(GET_SCENAME())//TRANS_TRM_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 24
         DIMENSIONS = 5
         RPT_UNIT_NUM = TRANS_TRM_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day of Month        ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Transaction Group   ','C',INT(35,2),'V','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_TRM_RPT_HEADER = TRANS_TRM_UNIT
      RETURN

      ENTRY TRANS_EXC_RPT_HEADER(R_NEXT_REC)

! TRANSOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                   trim(GET_SCENAME())//TRANS_EXC_CODE
         OVERHEAD_LENGTH = 64
         ! SET TO THE MAXIMUM TRANSACTION GROUPS
         VARIABLE_NUMBER = 1 + GET_NUMBER_OF_ACTIVE_GROUPS() 
!
         DIMENSIONS = 4
         RPT_UNIT_NUM = TRANS_EXC_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                            'Seller              ','C',INT(35,2),'V','D'
         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_EXC_RPT_HEADER = TRANS_EXC_UNIT
      RETURN

      ENTRY TRANS_INC_RPT_HEADER(R_NEXT_REC)

! TRANSOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                   trim(GET_SCENAME())//TRANS_INC_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 24
         DIMENSIONS = 5
         RPT_UNIT_NUM = TRANS_INC_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day of Month        ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Transaction Group   ','C',INT(35,2),'V','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_INC_RPT_HEADER = TRANS_INC_UNIT
      RETURN

      ENTRY TRANS_LMD_RPT_HEADER(R_NEXT_REC)

! TRANSOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                   trim(GET_SCENAME())//TRANS_LMD_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 24
         DIMENSIONS = 5
         RPT_UNIT_NUM = TRANS_LMD_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day of Month        ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Transaction Group   ','C',INT(35,2),'V','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_LMD_RPT_HEADER = TRANS_LMD_UNIT
      RETURN

      ENTRY TRANS_ANN_RPT_HEADER(R_NEXT_REC)

! TRANSOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                   trim(GET_SCENAME())//TRANS_ANN_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 27
         DIMENSIONS = 4
         RPT_UNIT_NUM = TRANS_ANN_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                            'Transaction Group   ','C',INT(35,2),'V','D'
         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_ANN_RPT_HEADER = TRANS_ANN_UNIT
      RETURN

      ENTRY MON_TRANC_RPT_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! TRANSOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                   trim(GET_SCENAME())//MON_TRANC_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 4
         RPT_UNIT_NUM = MON_TRANC_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                            'Transaction Group   ','C',INT(35,2),'V','D'
         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MON_TRANC_RPT_HEADER = MON_TRANC_UNIT
      RETURN

      ENTRY MON_POSIT_RPT_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! TRANSOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                   trim(GET_SCENAME())//MON_POSIT_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 5
         RPT_UNIT_NUM = MON_POSIT_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                            'Transaction Group   ','C',INT(35,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Product             ','C',INT(10,2),'V','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MON_POSIT_RPT_HEADER = MON_POSIT_UNIT
      RETURN

      ENTRY TRANS_TRN_RPT_HEADER(R_NEXT_REC)

! TRANSOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                   trim(GET_SCENAME())//TRANS_TRN_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 1 + GET_NUMBER_OF_ACTIVE_GROUPS() 
         DIMENSIONS = 6
         

         RPT_UNIT_NUM = TRANS_TRN_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day of the Month    ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Transaction Group   ','C',INT(35,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+3) &
                             'Hour of the Day     ','N',INT(4,2),'V','D'
         R_NEXT_REC = NEXT_REC + 4

         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_TRN_RPT_HEADER = TRANS_TRN_UNIT

      RETURN

      ENTRY USER_DURATION_HEADER(R_NEXT_REC)

! TRANSOBJ
! TODO: Understand exactly what is meant by "TRANSOBJ", above comment.

         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                             trim(GET_SCENAME())//USER_DURATION_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 4
         DIMENSIONS = 5
         RPT_UNIT_NUM = USER_DURATION_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                            'Transaction Group   ','C',INT(35,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                             'Position            ','N',INT(4,2),'V','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         USER_DURATION_HEADER = USER_DURATION_NO
      RETURN

      ENTRY TECH_SCAR_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! TRANSOBJ
! TODO: Understand exactly what is meant by "TRANSOBJ", above comment.
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                   trim(GET_SCENAME())//TECH_SCAR_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 5
         RPT_UNIT_NUM = TECH_SCAR_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                            'Transaction Group   ','C',INT(35,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                             'Position            ','N',INT(4,2),'V','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TECH_SCAR_HEADER = TECH_SCAR_NO
      RETURN

      ENTRY ECITY_OBJ_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! TRANSOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                             trim(GET_SCENAME())//ECITY_OBJ_CODE
         OVERHEAD_LENGTH = 21
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 4
         RPT_UNIT_NUM = ECITY_OBJ_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Asset Class Number  ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Month               ','C',INT(9,2),'F','D'
         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         ECITY_OBJ_HEADER = ECITY_OBJ_NO
      RETURN

      ENTRY MARKET_DURATION_HEADER(R_NEXT_REC)

! TRANSOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                             trim(GET_SCENAME())//MARKET_DURATION_CODE
         OVERHEAD_LENGTH = 21
         VARIABLE_NUMBER = 4
         DIMENSIONS = 4
         RPT_UNIT_NUM = MARKET_DURATION_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)

         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'

         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Position            ','N',INT(4,2),'V','D'
         R_NEXT_REC = NEXT_REC + 2

         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MARKET_DURATION_HEADER = MARKET_DURATION_NO
      RETURN

      ENTRY DAY_WEEK_RPT_HEADER(R_NEXT_REC)

! TRANSOBJ
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                    trim(GET_SCENAME())//DAY_WEEK_CODE
         OVERHEAD_LENGTH = 17
         VARIABLE_NUMBER = 31
         DIMENSIONS = 3
         RPT_UNIT_NUM = DAY_WEEK_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) 'Period              ','C', &
                                                        INT(9,2),'F','D'
         R_NEXT_REC = NEXT_REC + 1
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         DAY_WEEK_RPT_HEADER = DAY_WEEK_UNIT
      RETURN

      ENTRY NUC_FUEL_REGION_RPT(R_NEXT_REC)

! FINNUCFL MSGMMNFL
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                          trim(GET_SCENAME())//NUC_FUEL_REPORTING_CODE
         OVERHEAD_LENGTH = 45
         VARIABLE_NUMBER = 15
         DIMENSIONS = 4
         RPT_UNIT_NUM = NUC_REPORTING_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                            'Account Name        ','C',INT(34,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Batch/Load          ','C',INT(7,2),'V','D'
         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         NUC_FUEL_REGION_RPT = NUC_REPORTING_UNIT
      RETURN

      ENTRY TRANS_LBH_RPT_HEADER(R_NEXT_REC)

! TF_OBJT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                   trim(GET_SCENAME())//TRANS_LBH_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 24 ! 30
         DIMENSIONS = 5
         RPT_UNIT_NUM = TRANS_LBH_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day of Month        ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Transaction Group   ','C',INT(35,2),'V','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_LBH_RPT_HEADER = TRANS_LBH_UNIT
      RETURN

      ENTRY TRANS_INTR_RPT_HEADER(R_NEXT_REC)

! TF_OBJT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                  trim(GET_SCENAME())//TRANS_INTR_CODE
         OVERHEAD_LENGTH = 51
         VARIABLE_NUMBER = 24
         DIMENSIONS = 5
         RPT_UNIT_NUM = TRANS_INTR_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day of Month        ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Customer Group      ','C',INT(30,2),'V','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_INTR_RPT_HEADER = TRANS_INTR_UNIT
      RETURN

      ENTRY WH_HOURLY_HEADER(R_NEXT_REC)

! TF_OBJT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                    trim(GET_SCENAME())//TRANS_WH_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 24
         DIMENSIONS = 5
         RPT_UNIT_NUM = TRANS_WH_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day of Month        ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Transaction Group   ','C',INT(35,2),'V','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         WH_HOURLY_HEADER = TRANS_WH_UNIT
      RETURN

      ENTRY HYDRO_AG_HEADER(R_NEXT_REC)

! TF_OBJT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                             trim(GET_SCENAME())//HYDRO_AG_HOURLY_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 24
         DIMENSIONS = 5
         RPT_UNIT_NUM = HYDRO_AG_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day of Month        ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Transaction Group   ','C',INT(35,2),'V','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         HYDRO_AG_HEADER = HYDRO_AG_NO
      RETURN

      ENTRY TRANS_CLASS_SUM_HEADER(R_NEXT_REC)

! TF_OBJT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                            trim(GET_SCENAME())//MON_CUSTOMER_SUM_CODE
         OVERHEAD_LENGTH = 47
         VARIABLE_NUMBER = 30 ! 100907 ! 3/19/02
         DIMENSIONS = 4
         RPT_UNIT_NUM = TRANS_CLASS_SUM_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                            'Customer Name       ','C',INT(30,2),'V','D'
         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_CLASS_SUM_HEADER = TRANS_CLASS_SUM_UNIT
      RETURN

      ENTRY MONTHLY_GAS_DEMAND_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! GAS_OBJT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                          trim(GET_SCENAME())//MONTHLY_GAS_DEMAND_CODE
         OVERHEAD_LENGTH = 47
         VARIABLE_NUMBER = R_VARIABLE_NUMBER ! 12 ! 3/19/02
         DIMENSIONS = 4
         RPT_UNIT_NUM = MONTHLY_GAS_DEMAND_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                            'Customer Name       ','C',INT(30,2),'V','D'
         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MONTHLY_GAS_DEMAND_HEADER = MONTHLY_GAS_DEMAND_UNIT
      RETURN

      ENTRY MONTHLY_GAS_BALANCE_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)

! GAS_OBJT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                         trim(GET_SCENAME())//MONTHLY_GAS_BALANCE_CODE
         OVERHEAD_LENGTH = 23
         VARIABLE_NUMBER = R_VARIABLE_NUMBER ! 12 ! 3/19/02
         DIMENSIONS = 4
         RPT_UNIT_NUM = MONTHLY_GAS_BALANCE_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Country             ','C',INT(6,2),'V','D'
         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MONTHLY_GAS_BALANCE_HEADER = MONTHLY_GAS_BALANCE_UNIT
      RETURN

      ENTRY MONTHLY_GAS_SUPPLY_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! GAS_OBJT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                          trim(GET_SCENAME())//MONTHLY_GAS_SUPPLY_CODE
         OVERHEAD_LENGTH = 47
         VARIABLE_NUMBER = R_VARIABLE_NUMBER ! 12 ! 3/19/02
         DIMENSIONS = 4
         RPT_UNIT_NUM = MONTHLY_GAS_SUPPLY_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                            'Supply Cost Curve   ','C',INT(30,2),'V','D'
         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MONTHLY_GAS_SUPPLY_HEADER = MONTHLY_GAS_SUPPLY_UNIT
      RETURN

      ENTRY MONTHLY_GAS_BASIN_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)

! GAS_OBJT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                          trim(GET_SCENAME())//MONTHLY_GAS_BASIN_CODE
         OVERHEAD_LENGTH = 17
         VARIABLE_NUMBER = R_VARIABLE_NUMBER ! 12 ! 3/19/02
         DIMENSIONS = 3
         RPT_UNIT_NUM = MONTHLY_GAS_BASIN_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'
!         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
!     +                       'BASIN Cost Curve   ','C',INT2(30),'V','D'
         R_NEXT_REC = NEXT_REC + 1
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MONTHLY_GAS_BASIN_HEADER = MONTHLY_GAS_BASIN_UNIT
      RETURN

      ENTRY MONTHLY_GAS_LINK_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)

! GAS_OBJT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                            trim(GET_SCENAME())//MONTHLY_GAS_LINK_CODE
         OVERHEAD_LENGTH = 17
         VARIABLE_NUMBER = R_VARIABLE_NUMBER ! 12 ! 3/19/02
         DIMENSIONS = 3
         RPT_UNIT_NUM = MONTHLY_GAS_LINK_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'
!         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
!     +                       'LINK Cost Curve   ','C',INT2(30),'V','D'
         R_NEXT_REC = NEXT_REC + 1
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MONTHLY_GAS_LINK_HEADER = MONTHLY_GAS_LINK_UNIT
      RETURN

      ENTRY MONTHLY_GAS_LINK_PRICE_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)

! GAS_OBJT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                      trim(GET_SCENAME())//MONTHLY_GAS_LINK_PRICE_CODE
         OVERHEAD_LENGTH = 17
         VARIABLE_NUMBER = R_VARIABLE_NUMBER ! 12 ! 3/19/02
         DIMENSIONS = 3
         RPT_UNIT_NUM = MONTHLY_GAS_LINK_PRICE_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'
!         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
!     +                       'LINK Cost Curve   ','C',INT2(30),'V','D'
         R_NEXT_REC = NEXT_REC + 1
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MONTHLY_GAS_LINK_PRICE_HEADER = MONTHLY_GAS_LINK_PRICE_UNIT
      RETURN

      ENTRY MONTHLY_GAS_RESERVE_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)

! GAS_OBJT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                         trim(GET_SCENAME())//MONTHLY_GAS_RESERVE_CODE
         OVERHEAD_LENGTH = 17
         VARIABLE_NUMBER = R_VARIABLE_NUMBER ! 12 ! 3/19/02
         DIMENSIONS = 3
         RPT_UNIT_NUM = MONTHLY_GAS_RESERVE_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)

         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'

         R_NEXT_REC = NEXT_REC + 1

         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MONTHLY_GAS_RESERVE_HEADER = MONTHLY_GAS_RESERVE_UNIT
      RETURN

      ENTRY CX_DAILY_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! TF_OBJT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                    trim(GET_SCENAME())//CX_DAILY_CODE
         OVERHEAD_LENGTH = 41
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = CX_DAILY_NO
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day                 ','N',INT(4,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Resource            ','C',INT(20,2),'F','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         CX_DAILY_HEADER = CX_DAILY_NO
      RETURN

      ENTRY UU_DAILY_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! TF_OBJT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                          trim(GET_SCENAME())//DAILY_THERMAL_UNIT_CODE
         OVERHEAD_LENGTH = 46
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = DAILY_THERMAL_UNIT_NO
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day                 ','N',INT(4,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Resource            ','C',INT(25,2),'F','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         UU_DAILY_HEADER = DAILY_THERMAL_UNIT_NO
      RETURN

      ENTRY UK_DAILY_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! TF_OBJT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                 trim(GET_SCENAME())//DAILY_TG_PM_CODE
         OVERHEAD_LENGTH = 56
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = DAILY_TG_PM_NO
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day                 ','N',INT(4,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Transaction Group   ','C',INT(35,2),'F','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         UK_DAILY_HEADER = DAILY_TG_PM_NO
      RETURN

      ENTRY UV_HOURLY_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! TF_OBJT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                   trim(GET_SCENAME())//UV_HOURLY_CODE
         OVERHEAD_LENGTH = 46
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = UV_HOURLY_NO
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day                 ','N',INT(4,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Resource            ','C',INT(25,2),'F','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         UV_HOURLY_HEADER = UV_HOURLY_NO
      RETURN

      ENTRY HX_HOURLY_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! TF_OBJT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                   trim(GET_SCENAME())//HX_HOURLY_CODE
         OVERHEAD_LENGTH = 41
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = HX_HOURLY_NO
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day                 ','N',INT(4,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Resource            ','C',INT(20,2),'F','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         HX_HOURLY_HEADER = HX_HOURLY_NO
      RETURN

      ENTRY AJ_HOURLY_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! TF_OBJT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                   trim(GET_SCENAME())//AJ_HOURLY_CODE
         OVERHEAD_LENGTH = 41
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = AJ_HOURLY_NO
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day                 ','N',INT(4,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Resource            ','C',INT(20,2),'F','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         AJ_HOURLY_HEADER = AJ_HOURLY_NO
      RETURN

      ENTRY AK_HOURLY_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! TF_OBJT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                   trim(GET_SCENAME())//AK_HOURLY_CODE
         OVERHEAD_LENGTH = 41
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = AK_HOURLY_NO
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                            'Resource            ','C',INT(20,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                             'Hour                ','N',INT(4,2),'F','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         AK_HOURLY_HEADER = AK_HOURLY_NO
      RETURN

      ENTRY XX_HOURLY_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! TF_OBJT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                   trim(GET_SCENAME())//XX_HOURLY_CODE
         OVERHEAD_LENGTH = 41
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = XX_HOURLY_NO
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day                 ','N',INT(4,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Resource            ','C',INT(20,2),'F','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         XX_HOURLY_HEADER = XX_HOURLY_NO
      RETURN

      ENTRY KX_HOURLY_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! TF_OBJT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                   trim(GET_SCENAME())//KX_HOURLY_CODE
         OVERHEAD_LENGTH = 33
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = KX_HOURLY_NO
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day                 ','N',INT(4,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Resource ID         ','C',INT(12,2),'F','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         KX_HOURLY_HEADER = KX_HOURLY_NO
      RETURN

      ENTRY QX_HOURLY_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! TF_OBJT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                   trim(GET_SCENAME())//QX_HOURLY_CODE
         OVERHEAD_LENGTH = 33
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = QX_HOURLY_NO
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day                 ','N',INT(4,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Resource ID         ','C',INT(12,2),'F','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         QX_HOURLY_HEADER = QX_HOURLY_NO
      RETURN

      ENTRY HOURLY_FLOW_ANALYST_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! TF_OBJT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                         trim(GET_SCENAME())//HOURLY_FLOW_ANALYST_CODE
         OVERHEAD_LENGTH = 90
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = HOURLY_FLOW_ANALYST_NO
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day                 ','N',INT(4,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Resource            ','C',INT(65,2),'F','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         HOURLY_FLOW_ANALYST_HEADER = HOURLY_FLOW_ANALYST_NO
      RETURN

      ENTRY WX_HOURLY_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! TF_OBJT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                   trim(GET_SCENAME())//WX_HOURLY_CODE
         OVERHEAD_LENGTH = 41
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = WX_HOURLY_NO
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day                 ','N',INT(4,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Resource            ','C',INT(20,2),'F','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         WX_HOURLY_HEADER = WX_HOURLY_NO
      RETURN

      ENTRY HOURLY_FLOW_CONSTRAINT_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! TF_OBJT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                      trim(GET_SCENAME())//HOURLY_FLOW_CONSTRAINT_CODE
         OVERHEAD_LENGTH = 90
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = HOURLY_FLOW_CONSTRAINT_NO
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day                 ','N',INT(4,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Resource            ','C',INT(65,2),'F','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         HOURLY_FLOW_CONSTRAINT_HEADER = HOURLY_FLOW_CONSTRAINT_NO
      RETURN

      ENTRY C_HOURLY_COST_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! TF_OBJT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                               trim(GET_SCENAME())//C_HOURLY_COST_CODE
         OVERHEAD_LENGTH = 41
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = C_HOURLY_COST_UNIT
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day                 ','N',INT(4,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Resource            ','C',INT(20,2),'F','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         C_HOURLY_COST_HEADER = C_HOURLY_COST_UNIT
      RETURN

      ENTRY C_HOURLY_MWH_COST_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! TF_OBJT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                           trim(GET_SCENAME())//C_HOURLY_MWH_COST_CODE
         OVERHEAD_LENGTH = 41
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = C_HOURLY_MWH_COST_UNIT
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day                 ','N',INT(4,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Resource            ','C',INT(20,2),'F','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         C_HOURLY_MWH_COST_HEADER = C_HOURLY_MWH_COST_UNIT
      RETURN

      ENTRY C_HOURLY_SUMMARY_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! TF_OBJT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                            trim(GET_SCENAME())//C_HOURLY_SUMMARY_CODE
         OVERHEAD_LENGTH = 41
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = C_HOURLY_SUMMARY_UNIT
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day                 ','N',INT(4,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Resource            ','C',INT(20,2),'F','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         C_HOURLY_SUMMARY_HEADER = C_HOURLY_SUMMARY_UNIT
      RETURN

      ENTRY C_TG_HOURLY_SUMMARY_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! TF_OBJT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                         trim(GET_SCENAME())//C_TG_HOURLY_SUMMARY_CODE
         OVERHEAD_LENGTH = 80
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = C_TG_HOURLY_SUMMARY_UNIT
         DIMENSIONS = 6
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                            'Transaction Group   ','C',INT(35,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                             'Day                 ','N',INT(4,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+3) &
                            'Resource            ','C',INT(20,2),'F','D'
         R_NEXT_REC = NEXT_REC + 4
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         C_TG_HOURLY_SUMMARY_HEADER = C_TG_HOURLY_SUMMARY_UNIT
      RETURN

      ENTRY DX_HOURLY_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! TF_OBJT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                   trim(GET_SCENAME())//DX_HOURLY_CODE
         OVERHEAD_LENGTH = 41
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = DX_HOURLY_NO
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day                 ','N',INT(4,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Resource            ','C',INT(20,2),'F','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         DX_HOURLY_HEADER = DX_HOURLY_NO
      RETURN

      ENTRY GY_HOURLY_HEADER(R_NEXT_REC)

! TF_OBJT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                         trim(GET_SCENAME())//DEPTH_HOURLY_LAMBDA_CODE
         OVERHEAD_LENGTH = 21
         VARIABLE_NUMBER = 24
         RPT_UNIT_NUM = DEPTH_HOURLY_LAMBDA_UNIT
         DIMENSIONS = 4
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day                 ','N',INT(4,2),'F','D'

         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         GY_HOURLY_HEADER = DEPTH_HOURLY_LAMBDA_UNIT
      RETURN

      ENTRY OX_HOURLY_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

!
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                   trim(GET_SCENAME())//OX_HOURLY_CODE
         OVERHEAD_LENGTH = 41
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = OX_HOURLY_NO
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day                 ','N',INT(4,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Resource            ','C',INT(20,2),'F','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         OX_HOURLY_HEADER = OX_HOURLY_NO
      RETURN

      ENTRY RC_REVENUE_FORECAST_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

!
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                             trim(GET_SCENAME())//".RCD" ! RC_REV_FORECAST_CODE
         OVERHEAD_LENGTH = 60
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = RC_REV_FORECAST_NO
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                      OVERHEAD_LENGTH, &
                                                      VARIABLE_NUMBER, &
                                                      DIMENSIONS, &
                                                      RPT_UNIT_NUM, &
                                                      NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Account Type        ','C',INT(8,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                  'Account Name        ','C',INT(40,2),'V','D',INT(36,2)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                             'Period              ','C',INT(6,2),'V','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         RC_REVENUE_FORECAST_HEADER = RC_REV_FORECAST_NO
      RETURN

      ENTRY HEC_HOURLY_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! CREATED 3/28/01.
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                  trim(GET_SCENAME())//HEC_HOURLY_CODE
         OVERHEAD_LENGTH = 41
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = HEC_HOURLY_NO
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Day                 ','N',INT(4,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Resource            ','C',INT(20,2),'F','D'
         R_NEXT_REC = NEXT_REC + 3
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         HEC_HOURLY_HEADER = HEC_HOURLY_NO
      RETURN

      ENTRY EXISTING_ASSET_RPT_HEADER(R_NEXT_REC)

! MSGMMEA FINACEA
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                trim(GET_SCENAME())//EA_REPORTING_CODE
         OVERHEAD_LENGTH = 45
         VARIABLE_NUMBER = 15
         DIMENSIONS = 3
         RPT_UNIT_NUM = EA_REPORTING_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE_WO(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Endpoint            ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                  'Account Name        ','C',INT(34,2),'V','D',INT(30,2)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                             'Year                ','N',INT(4,2),'V','D'
         R_NEXT_REC = NEXT_REC + 3
!
         EXISTING_ASSET_RPT_HEADER = EA_REPORTING_UNIT
      RETURN

      ENTRY WVPA_ASSET_RPT_HEADER(R_NEXT_REC)

! MSGMMEA FINACEA
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"WVP"// &
                                trim(GET_SCENAME())//EA_REPORTING_CODE
         OVERHEAD_LENGTH = 45
         VARIABLE_NUMBER = 30
         DIMENSIONS = 5
         RPT_UNIT_NUM = EA_REPORTING_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE_WO(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Endpoint            ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Year                ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                    'Department Name     ','C',INT(5,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+3) &
                  'Primary Account     ','C',INT(40,2),'V','D',INT(36,2)

         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+4) &
                             'Period              ','C',INT(6,2),'F','D'
         R_NEXT_REC = NEXT_REC + 5

         WVPA_ASSET_RPT_HEADER = EA_REPORTING_UNIT
      RETURN

      ENTRY WVPA_COOP_REVENUE_HEADER(R_NEXT_REC)


         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"WVP"// &
                                           trim(GET_SCENAME())//".RCD" ! RC_REV_FORECAST_CODE
         OVERHEAD_LENGTH = 60
         VARIABLE_NUMBER = 30
         RPT_UNIT_NUM = RC_REV_FORECAST_NO
         DIMENSIONS = 4
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                      OVERHEAD_LENGTH, &
                                                      VARIABLE_NUMBER, &
                                                      DIMENSIONS, &
                                                      RPT_UNIT_NUM, &
                                                      NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                  'Member Name         ','C',INT(40,2),'V','D',INT(36,2)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Period              ','C',INT(6,2),'V','D'
         R_NEXT_REC = NEXT_REC + 2
!
         WVPA_COOP_REVENUE_HEADER = RC_REV_FORECAST_NO
      RETURN

      ENTRY WVPA_WORKORDER_CONTROL_RPT(R_NEXT_REC)

! MSGMMEA FINACEA
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"WVP"// &
                           trim(GET_SCENAME())//WVPA_CONTROL_REPT_CODE
         OVERHEAD_LENGTH = 45
         VARIABLE_NUMBER = 30
         DIMENSIONS = 3
         RPT_UNIT_NUM = WVPA_CONTROL_REPT_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE_WO(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                    'Endpoint            ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                    'Department Head     ','C',INT(15,2),'V','D'

         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                    'Work Order          ','C', &
                              INT(DrillingAccountNameWidth,2),'V','D', &
                              INT(DrillingAccountNameWidth-4,2)
         R_NEXT_REC = NEXT_REC + 3
!
         WVPA_WORKORDER_CONTROL_RPT = WVPA_CONTROL_REPT_UNIT
      RETURN

      ENTRY WVPA_WORKORDER_EXPENSES_RPT(R_NEXT_REC)

! MSGMMEA FINACEA
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"WVP"// &
                       trim(GET_SCENAME())//WVPA_WO_EXPENSES_REPT_CODE
         OVERHEAD_LENGTH = 45
         VARIABLE_NUMBER = 30
         DIMENSIONS = 4
         RPT_UNIT_NUM = WVPA_WO_EXPENSES_REPT_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE_WO(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                    'Endpoint            ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                    'Department Head     ','C',INT(15,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                    'Year                ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+3) &
                    'Work Order          ','C', &
                              INT(DrillingAccountNameWidth,2),'V','D', &
                              INT(DrillingAccountNameWidth-4,2)
         R_NEXT_REC = NEXT_REC + 4
!
         WVPA_WORKORDER_EXPENSES_RPT = WVPA_WO_EXPENSES_REPT_UNIT
      RETURN

      ENTRY WVPA_WORKORDER_SUMMARY_RPT(R_NEXT_REC)

! MSGMMEA FINACEA
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"WVP"// &
                        trim(GET_SCENAME())//WVPA_WO_SUMMARY_REPT_CODE
         OVERHEAD_LENGTH = 45
         VARIABLE_NUMBER = 30
         DIMENSIONS = 5
         RPT_UNIT_NUM = WVPA_WO_SUMMARY_REPT_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE_WO(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                    'Endpoint            ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                    'Year                ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                    'Department Head     ','C',INT(15,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+3) &
                    'Work Order          ','C', &
                              INT(DrillingAccountNameWidth,2),'V','D', &
                              INT(DrillingAccountNameWidth-4,2)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+4) &
                    'Month Ending        ','C',INT(6,2),'F','D'
         R_NEXT_REC = NEXT_REC + DIMENSIONS
!
         WVPA_WORKORDER_SUMMARY_RPT = WVPA_WO_SUMMARY_REPT_UNIT
      RETURN

      ENTRY WVPA_DEBT_FILE_RPT_HEADER(R_NEXT_REC)

! MSGMMDB FINACDBT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"WVP"// &
                                trim(GET_SCENAME())//DB_REPORTING_CODE
         OVERHEAD_LENGTH = 46
         VARIABLE_NUMBER = 20
         DIMENSIONS = 4
         RPT_UNIT_NUM = DB_REPORTING_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE_WO(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Endpoint            ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Year                ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                  'Issue Name          ','C',INT(34,2),'V','D',INT(30,2)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+3) &
                             'Period              ','C',INT(6,2),'F','D'
         R_NEXT_REC = NEXT_REC + 4
!
         WVPA_DEBT_FILE_RPT_HEADER = DB_REPORTING_UNIT
      RETURN

      ENTRY WVPA_MORTGAGE_SUMMARY_RPT_HEADR(R_NEXT_REC)

! MSGMMDB FINACDBT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"WVP"// &
                    trim(GET_SCENAME())//WVPA_MORTGAGE_SUMRY_REPT_CODE
         OVERHEAD_LENGTH = 80
         VARIABLE_NUMBER = 10
         DIMENSIONS = 5
         RPT_UNIT_NUM = WVPA_MORTGAGE_SUMRY_REPT_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE_WO(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Endpoint            ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Year                ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                  'Issue Name          ','C',INT(34,2),'V','D',INT(30,2)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+3) &
                  'Mortgage Debt Type  ','C',INT(30,2),'V','D',INT(30,2)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+4) &
                             'Period              ','C',INT(6,2),'F','D'
         R_NEXT_REC = NEXT_REC + 5
!
         WVPA_MORTGAGE_SUMMARY_RPT_HEADR = WVPA_MORTGAGE_SUMRY_REPT_UNIT
      RETURN

      ENTRY WVPA_PLNT_BALANCES_RPT_HEADR(R_NEXT_REC)

! MSGMMDB FINACDBT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"WVP"// &
                                          trim(GET_SCENAME())//'.WPD' ! WVPA_PLNT_BALANCES_RPT_CODE
         OVERHEAD_LENGTH = 80
         VARIABLE_NUMBER = 200
         DIMENSIONS = 5
         RPT_UNIT_NUM = 5286 ! WVPA_PLNT_BALANCES_RPT_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE_WO(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                            'Endpoint            ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                            'Year                ','N',INT(4,2),'V','D'
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Period              ','C',INT(6,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+3) &
                            'Company             ','C',INT(10,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+4) &
                            'Plant Values        ','C',INT(30,2),'V','D'
         R_NEXT_REC = NEXT_REC + DIMENSIONS
!
         WVPA_PLNT_BALANCES_RPT_HEADR = RPT_UNIT_NUM
      RETURN


      ENTRY WVPA_COMP_CONSTR_SUMY_RPT_HEADR(R_NEXT_REC)

! MSGMMDB FINACDBT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"WVP"// &
                                           trim(GET_SCENAME())//'.CCD' ! WVPA_PLNT_BALANCES_RPT_CODE
         OVERHEAD_LENGTH = 80
         VARIABLE_NUMBER = 30
         DIMENSIONS = 5
         RPT_UNIT_NUM = 5291 ! WVPA_PLNT_BALANCES_RPT_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE_WO(FILE_NAME, &
                                                        OVERHEAD_LENGTH, &
                                                        VARIABLE_NUMBER, &
                                                        DIMENSIONS, &
                                                        RPT_UNIT_NUM, &
                                                        NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Endpoint            ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Year                ','N',INT(4,2),'V','D'
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                             'Period              ','C',INT(6,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+3) &
                            'Company             ','C',INT(10,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+4) &
                            'Account-Work Order  ','C',INT(40,2),'V','D'
         R_NEXT_REC = NEXT_REC + DIMENSIONS
!
         WVPA_COMP_CONSTR_SUMY_RPT_HEADR = RPT_UNIT_NUM
      RETURN

      ENTRY WVPA_CONSTR_SUMY_RPT_HEADR(R_NEXT_REC)

! MSGMMDB FINACDBT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"WVP"// &
                                          trim(GET_SCENAME())//'.WAD' ! WVPA_PLNT_BALANCES_RPT_CODE
         OVERHEAD_LENGTH = 80
         VARIABLE_NUMBER = 30
         DIMENSIONS = 5
         RPT_UNIT_NUM = 5288 ! WVPA_PLNT_BALANCES_RPT_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE_WO(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                            'Endpoint            ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                            'Year                ','N',INT(4,2),'V','D'
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                            'Period              ','C',INT(6,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+3) &
                            'Account Name        ','C',INT(20,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+4) &
                            'Work Order          ','C',INT(34,2),'V','D'
         R_NEXT_REC = NEXT_REC + DIMENSIONS
!
         WVPA_CONSTR_SUMY_RPT_HEADR = RPT_UNIT_NUM
      RETURN

      ENTRY WVPA_CUM_DEP_RPT_HEADR(R_NEXT_REC)

! MSGMMDB FINACDBT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"WVP"// &
            trim(GET_SCENAME())//'.WDD' ! WVPA_PLNT_BALANCES_RPT_CODE
         OVERHEAD_LENGTH = 80
         VARIABLE_NUMBER = 200
         DIMENSIONS = 5
         RPT_UNIT_NUM = 5287 ! WVPA_PLNT_BALANCES_RPT_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE_WO(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                            'Endpoint            ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                            'Year                ','N',INT(4,2),'V','D'
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                             'Period              ','C',INT(6,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+3) &
                            'Company             ','C',INT(10,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+4) &
                            'Plant Values        ','C',INT(30,2),'V','D'
         R_NEXT_REC = NEXT_REC + DIMENSIONS
!
         WVPA_CUM_DEP_RPT_HEADR = RPT_UNIT_NUM
      RETURN

      ENTRY WVPA_RATE_TRACKER_HEADR(R_NEXT_REC)

! MSGMMDB FINACDBT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"WVP"// &
                                          trim(GET_SCENAME())//'.RTD' ! WVPA_PLNT_BALANCES_RPT_CODE
         OVERHEAD_LENGTH = 80
         VARIABLE_NUMBER = 200
         DIMENSIONS = 3
         RPT_UNIT_NUM = 5289 ! WVPA_PLNT_BALANCES_RPT_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE_WO(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                            'Endpoint            ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                            'Year                ','N',INT(4,2),'V','D'
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                             'Period              ','C',INT(6,2),'F','D'
         R_NEXT_REC = NEXT_REC + 3
!
         WVPA_RATE_TRACKER_HEADR = RPT_UNIT_NUM
      RETURN

      ENTRY FUTURE_ASSET_RPT_HEADER(R_NEXT_REC,ACTIVE_DIMENSION)

! FUTURE ASSETS
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                trim(GET_SCENAME())//FA_REPORTING_CODE
         IF(ACTIVE_DIMENSION == 'M') THEN
            DIMENSIONS = 4
         ELSE
            DIMENSIONS = 3
         ENDIF
         OVERHEAD_LENGTH = 46

         VARIABLE_NUMBER =30
         RPT_UNIT_NUM = FA_REPORTING_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE_WO(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Endpoint            ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                    'Account Name        ','C', &
                              INT(DrillingAccountNameWidth,2),'V','D', &
                              INT(DrillingAccountNameWidth-4,2)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                             'Year                ','N',INT(4,2),'V','D'
         IF(ACTIVE_DIMENSION == 'M') THEN
            WRITE(RPT_UNIT_NUM,REC=NEXT_REC+3) &
                            'Period              ','C',INT(10,2),'V','D'
            R_NEXT_REC = NEXT_REC + 4
         ELSE
            R_NEXT_REC = NEXT_REC + 3
         ENDIF
!
         FUTURE_ASSET_RPT_HEADER = FA_REPORTING_UNIT
      RETURN

      ENTRY DEBIT_FILE_RPT_HEADER(R_NEXT_REC)

! MSGMMDFD FINACDB
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                trim(GET_SCENAME())//DD_REPORTING_CODE
         OVERHEAD_LENGTH = 46
         VARIABLE_NUMBER = 25
         DIMENSIONS = 3
         RPT_UNIT_NUM = DD_REPORTING_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE_WO(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Endpoint            ','N',INT(4,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &

                  'Account Name        ','C',INT(34,2),'V','D',INT(30,2)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &

                             'Year                ','N',INT(4,2),'V','D'
          R_NEXT_REC = NEXT_REC + 3

         DEBIT_FILE_RPT_HEADER = DD_REPORTING_UNIT
      RETURN

      ENTRY TRAILING_12_MOS_RPT_HEADER(R_NEXT_REC)

! MSGMMDFD FINACDB
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                             trim(GET_SCENAME())//T12MO_REPORTING_CODE
         OVERHEAD_LENGTH = 15
         VARIABLE_NUMBER = 300
         DIMENSIONS = 3
         RPT_UNIT_NUM = T12MO_REPORTING_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             '12 Months Ending    ','C',INT(7,2),'V','D'
         R_NEXT_REC = NEXT_REC + 1
!
         TRAILING_12_MOS_RPT_HEADER = T12MO_REPORTING_UNIT
      RETURN

      ENTRY IPL_REGULATED_REVENUES_RPT_H(R_NEXT_REC)

! MSGMMDFD FINACDB
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"IPL"// &
                             trim(GET_SCENAME())//IPL_REG_REVENUE_CODE
         OVERHEAD_LENGTH = 78
         VARIABLE_NUMBER = 30
         DIMENSIONS = 5
         RPT_UNIT_NUM = IPL_REG_REVENUE_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE_WO(FILE_NAME, &
                                                        OVERHEAD_LENGTH, &
                                                        VARIABLE_NUMBER, &
                                                        DIMENSIONS, &
                                                        RPT_UNIT_NUM, &
                                                        NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                            'Company             ','C',INT(38,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) 'Endpoint            ','N', &
                                                        INT(4,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) 'Year                ','N', &
                                                        INT(4,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+3) &
                             '12 Months Ending    ','C',INT(7,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+4) &
                            'Columns             ','C',INT(25,2),'V','D'
         R_NEXT_REC = NEXT_REC + 5
!        IPL regulated revenues report header (unit)
         IPL_REGULATED_REVENUES_RPT_H = IPL_REG_REVENUE_UNIT
      RETURN

      ENTRY DEBT_FILE_RPT_HEADER(R_NEXT_REC,ADD_MONTHLY_DIMENSION)

! MSGMMDB FINACDBT
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                trim(GET_SCENAME())//DB_REPORTING_CODE
         OVERHEAD_LENGTH = 46
         VARIABLE_NUMBER = 40
! 013114. COPIED FROM OLDER SOURCE TO REMOVE DIMENSION FOR ODEC.
         DIMENSIONS = 3
         IF(ADD_MONTHLY_DIMENSION) DIMENSIONS = 4
         RPT_UNIT_NUM = DB_REPORTING_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE_WO(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Endpoint            ','N',INT(4,2),'V','D'
         IF(ADD_MONTHLY_DIMENSION) THEN
            WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Year                ','N',INT(4,2),'V','D'
            WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                  'Account Name        ','C',INT(34,2),'V','D',INT(30,2)
            WRITE(RPT_UNIT_NUM,REC=NEXT_REC+3) &
                             'Period              ','C',INT(6,2),'F','D'
            R_NEXT_REC = NEXT_REC + 4
         ELSE
            WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                  'Account Name        ','C',INT(34,2),'V','D',INT(30,2)
            WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) &
                             'Year                ','N',INT(4,2),'V','D'
            R_NEXT_REC = NEXT_REC + 3
         ENDIF
!
         DEBT_FILE_RPT_HEADER = DB_REPORTING_UNIT
      RETURN

      ENTRY ANNUAL_SERVICE_TRANS_HEADER(R_NEXT_REC)

!
! SERVICAC FILE
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                         trim(GET_SCENAME())//ANNUAL_TRANSACTIONS_CODE
         OVERHEAD_LENGTH = 40
         VARIABLE_NUMBER = 10
         DIMENSIONS = 4
         RPT_UNIT_NUM = ANNUAL_TRANSACTIONS_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                            'Description         ','C',INT(25,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                             'Revenue/Expense     ','C',INT(7,2),'V','D'
         R_NEXT_REC = NEXT_REC + 2
!
         ANNUAL_SERVICE_TRANS_HEADER = ANNUAL_TRANSACTIONS_UNIT
      RETURN

      ENTRY DYNAMIC_JURIS_RPT_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! MSGMMREV
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                               trim(GET_SCENAME())//DYNAMIC_JURIS_CODE
         OVERHEAD_LENGTH = 8
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 2
         RPT_UNIT_NUM = DYNAMIC_JURIS_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
         R_NEXT_REC = NEXT_REC
!
         DYNAMIC_JURIS_RPT_HEADER = DYNAMIC_JURIS_UNIT
      RETURN

      ENTRY MONTH_GROUP_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)

! DR_BOOTH
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                                 trim(GET_SCENAME())//MONTH_GROUP_CODE
         OVERHEAD_LENGTH = 37
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = MONTH_GROUP_UNIT
         DIMENSIONS = 4
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                                  OVERHEAD_LENGTH, &
                                                  VARIABLE_NUMBER, &
                                                  DIMENSIONS, &
                                                  RPT_UNIT_NUM, &
                                                  NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) &
                             'Month               ','C',INT(9,2),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) &
                            'Group Name          ','C',INT(20,2),'F','D'
         R_NEXT_REC = NEXT_REC + 2
!
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC, &
                           REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MONTH_GROUP_HEADER = MONTH_GROUP_UNIT
      RETURN
      END
!
! END DETAILED REPORTS
!

      FUNCTION ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME, &
                                            OVERHEAD_LENGTH, &
                                            VARIABLE_NUMBER, &
                                            DIMENSIONS, &
                                            RPT_UNIT_NUM, &
                                            NEXT_REC)
       use logging
       use mod_base_year

      INTEGER (kind=2) ::  ESTABLISH_DRILLING_REPORT_FILE,RECORD_LENGTH, &
                VARIABLE_NUMBER,DIMENSIONS, &
                RPT_UNIT_NUM,OVERHEAD_LENGTH, &
                NEXT_REC,ESTABLISH_DETAIL_REPORT_FILE, &
                get_endyr,EXTENSION_YEARS, &
                GET_NUM_OF_END_POINTS
      CHARACTER (len=*) :: FILE_NAME
      CHARACTER (len=40) ::  TITLE
      CHARACTER (len=28) ::  COMPANY_NAME
      INTEGER (kind=1) ::  F7=7 ! 7H == z"f7"
      LOGICAL (kind=1) ::  LAHEY_LF95
      LOGICAL ::  FILE_EXISTS
      integer :: ios
!
         RECORD_LENGTH = MAX(64,OVERHEAD_LENGTH + 4*VARIABLE_NUMBER)
         if (rpt_unit_num==1936) then
             rpt_unit_num=rpt_unit_num ! Debugstop
         end if

         OPEN(RPT_UNIT_NUM,FILE=FILE_NAME,ACCESS="DIRECT", &
                    STATUS="REPLACE",RECL=RECORD_LENGTH, IOSTAT=ios)
         call check_ios(ios, "dsex_obj:0006", FILE_NAME)
         IF(LAHEY_LF95()) THEN
            WRITE(RPT_UNIT_NUM,REC=1) F7,RECORD_LENGTH, &
                                      DIMENSIONS, &
                                      get_base_year(), &
                                      get_endyr()+EXTENSION_YEARS(), &
                                      GET_NUM_OF_END_POINTS()
            NEXT_REC = 2
         ELSE
            call close_unit(RPT_UNIT_NUM)
!
            OPEN(RPT_UNIT_NUM,FILE=FILE_NAME, &
                                      ACCESS="TRANSPARENT",STATUS="OLD")
            WRITE(RPT_UNIT_NUM,REC=4) DIMENSIONS, &
                                      get_base_year(), &
                                      get_endyr()+EXTENSION_YEARS(), &
                                      GET_NUM_OF_END_POINTS()
            call close_unit(RPT_UNIT_NUM)
!
            OPEN(RPT_UNIT_NUM,FILE=FILE_NAME,ACCESS="DIRECT", &
                                        STATUS="OLD",RECL=RECORD_LENGTH)
            NEXT_REC = 1
         ENDIF
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) COMPANY_NAME()
         NEXT_REC = NEXT_REC+1
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) TITLE()
         NEXT_REC = NEXT_REC+1
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) 'Endpoint            ','N', &
                                                        INT(4,2),'F','D'
         NEXT_REC = NEXT_REC+1
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) 'Year                ','N', &
                                                        INT(4,2),'F','D'
         NEXT_REC = NEXT_REC+1
         ESTABLISH_DETAIL_REPORT_FILE = RECORD_LENGTH
      RETURN
      END

      FUNCTION ESTABLISH_DETAIL_REPORT_FILE_WO(FILE_NAME, &
                                               OVERHEAD_LENGTH, &
                                               VARIABLE_NUMBER, &
                                               DIMENSIONS, &
                                               RPT_UNIT_NUM, &
                                               NEXT_REC)
      use logging
      use mod_base_year
      implicit none

      INTEGER (kind=2) ::  ESTABLISH_DRILLING_REPORT_FILE,RECORD_LENGTH, &
                VARIABLE_NUMBER,DIMENSIONS, &
                RPT_UNIT_NUM,OVERHEAD_LENGTH, &
                NEXT_REC,ESTABLISH_DETAIL_REPORT_FILE_WO, &
                get_endyr,EXTENSION_YEARS, &
                GET_NUM_OF_END_POINTS
      CHARACTER (len=*) :: FILE_NAME
      CHARACTER (len=40) ::  TITLE
      CHARACTER (len=28) ::  COMPANY_NAME
      INTEGER (kind=1) ::  F7=7 ! 7H == z"f7"
      LOGICAL (kind=1) ::  LAHEY_LF95
!
          RECORD_LENGTH = MAX(64,OVERHEAD_LENGTH + 4*VARIABLE_NUMBER)
         OPEN(RPT_UNIT_NUM,FILE=FILE_NAME,ACCESS="DIRECT", &
                                    STATUS="REPLACE",RECL=RECORD_LENGTH)
!
         IF(LAHEY_LF95()) THEN

            WRITE(RPT_UNIT_NUM,REC=1) F7,RECORD_LENGTH, &
                                      DIMENSIONS, &
                                      get_base_year(), &
                                      get_endyr()+EXTENSION_YEARS(), &
                                      GET_NUM_OF_END_POINTS()
            NEXT_REC = 2
         ELSE
            call close_unit(RPT_UNIT_NUM)
!
            OPEN(RPT_UNIT_NUM,FILE=FILE_NAME, &
                                      ACCESS="TRANSPARENT",STATUS="OLD")
            WRITE(RPT_UNIT_NUM,REC=4) DIMENSIONS, &
                                      get_base_year(), &
                                      get_endyr()+EXTENSION_YEARS(), &
                                      GET_NUM_OF_END_POINTS()
            call close_unit(RPT_UNIT_NUM)
!
            OPEN(RPT_UNIT_NUM,FILE=FILE_NAME,ACCESS="DIRECT", &
                                        STATUS="OLD",RECL=RECORD_LENGTH)
            NEXT_REC = 1
         ENDIF
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) COMPANY_NAME()
         NEXT_REC = NEXT_REC+1
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) TITLE()
         NEXT_REC = NEXT_REC+1
         ESTABLISH_DETAIL_REPORT_FILE_WO = RECORD_LENGTH
      RETURN
      END
      
! END DETAILED REPORTS

