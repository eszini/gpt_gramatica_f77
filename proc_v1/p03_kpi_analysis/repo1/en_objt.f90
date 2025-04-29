    
!     EN_OBJT.FOR
!     Created: 10/24/02 11:23:58 AM
!     Author : msg
!     Last change: MSG 11/17/2011 3:52:28 PM
    
!
      SUBROUTINE EN_OBJECT()
      use end_routine, only: end_program, er_message
      use SpinDriftLib
      use prod_arrays_dimensions
      USE SIZECOM
      use envircom
      use enrglimit
      use miscmod
      implicit none
      CHARACTER (len=32) ::  class_name_loc
      CHARACTER (LEN=3) :: CO2_PUSH_OK,SO2_PUSH_OK
      LOGICAL (kind=1) ::  INIT_CREATE_CLASS_LIST
      INTEGER (kind=2) ::  CREATE_CLASS_LIST,VOID_INT2,TABLE
      INTEGER (kind=2) ::  MAX_CLASS_IN_TABLE,PASS_TABLE_POINTER
      INTEGER (kind=2) ::  I
      INTEGER (kind=2) ::  IREC,INUNIT,DELETE,YEAR,EMIS_TYPE,LRECL=200
      INTEGER (kind=4) ::  IOS,IOS_BASE
      LOGICAL (kind=4) ::  FILE_EXISTS,ENVIR_FILE_EXISTS=.FALSE. ,   &
                R_ENVIR_FILE_EXISTS
      CHARACTER (len=5) ::  ENV_COM_FIL,OVERLAY_FAMILY_NAME
      CHARACTER (len=50) ::  COMMENT,EMISS_COMMENT
      CHARACTER (len=256) ::  FILE_NAME
      CHARACTER (len=256) ::  BASE_FILE_DIRECTORY,OUTPUT_DIRECTORY
      CHARACTER (len=1024) ::  RECLN
      CHARACTER (len=14) ::  FILE_TYPE='Emissions data'
      LOGICAL (kind=1) ::  FIRST_RECORD_OF_TABLE
      CHARACTER (len=2) ::  GET_ENV_COM_OL
      INTEGER (kind=2) ::  R_ENVIR_UNIT,R_MAX_TABLE_NUM
!
! MULTI-TABLE ADDITIONS
!
      CHARACTER (len=2) ::  ENV_COM_OL='BC'
      INTEGER (kind=2) ::  BC_TABLE_POSITION_FOR(:),   &
                OL_TABLE_POSITION_FOR(:)
      ALLOCATABLE :: BC_TABLE_POSITION_FOR,   &
                     OL_TABLE_POSITION_FOR
      INTEGER (kind=2) ::  MAX_OL_CLASS_NUMBER,MAX_BC_CLASS_NUMBER,   &
                R_TABLE_POINTR(0:*),DATA_RECORDS_IN_TABLE,   &
                R_MAX_CLASS_NUMBER
      
      LOGICAL (kind=1) ::  LAHEY_LF95
      LOGICAL (kind=1) ::  TRANSACT_ANALYST_ONLY,INIT_FILE_IS_ACTIVE
      CHARACTER (len=30) ::  SCREEN_OUTPUT
      INTEGER (kind=2) ::  CLASS_NUM
      INTEGER (kind=2) ::  MAX_BC_TABLE_NUMBER,MAX_OL_TABLE_NUMBER
      integer :: times_eno=0
      logical :: ifa
      SAVE BC_TABLE_POSITION_FOR,MAX_BC_CLASS_NUMBER,MAX_BC_TABLE_NUMBER
      SAVE OL_TABLE_POSITION_FOR,MAX_OL_CLASS_NUMBER,MAX_OL_TABLE_NUMBER
!
      RETURN

    
!
!          SUBROUTINE TO CONVERT METAFILE FILES TO DIRECT ACESS BINARY
!          COPYRIGHT (C) 1983, 84, 85  M.S. GERBER & ASSOCIATES, INC.
!
    
!
! CONVERT THE ENVIRONMENTAL FILE
      ENTRY EN_MAKEBIN
      times_eno=times_eno+1
      MAX_BC_CLASS_NUMBER = 0
      MAX_BC_TABLE_NUMBER = 0
      MAX_OL_CLASS_NUMBER = 0
      MAX_OL_TABLE_NUMBER = 0
      FILE_NAME = trim(BASE_FILE_DIRECTORY())//   &
                                    "ENB"//trim(ENV_COM_FIL())//".DAT"

      INQUIRE(FILE=FILE_NAME,EXIST=ENVIR_FILE_EXISTS)
      IF(ENVIR_FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//ENV_COM_FIL()
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_LOCATE_WRITE(16,30,ENV_COM_FIL(),ALL_VERSIONS,0)
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         ENDIF
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCENVCOM.BIN",   &
                            ACCESS="DIRECT",STATUS="REPLACE",RECL=LRECL)
! INITIALIZE VARIABLES
         EPA_EAS = 0.
         EPA_SETASIDE_RATE = 0.
         CLASS_NUM = 0
         DO EMIS_TYPE = 1, NUMBER_OF_EMISSION_TYPES
            EMIS_DISPATCH_ADDER(EMIS_TYPE) = 0.
            EMIS_CAP(EMIS_TYPE) = 0.
            PURCH_CRED_PRICE(EMIS_TYPE) = 0.
            SELL_CRED_PRICE(EMIS_TYPE) = 0.
            EMIS_STRAT(EMIS_TYPE) = "N"
            MIN_BANK(EMIS_TYPE) = 0.
            MAX_BANK(EMIS_TYPE) = 0.
         ENDDO
         FIRST_RECORD_OF_TABLE = INIT_CREATE_CLASS_LIST(INT(0,2))
!
         READ(10,*,IOSTAT=IOS) DELETE
         IREC = 0
         DO
            FIRST_RECORD_OF_TABLE = .TRUE.
            DATA_RECORDS_IN_TABLE = 0
            CO2_PUSH_OK = 'No'
            SO2_PUSH_OK = 'No'
            DO

               READ(10,1000,IOSTAT=IOS) RECLN
               IF(IOS /=0) EXIT
               IF(RECLN(1:1) == '7') EXIT
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,'//   &
                     ',,,,,,,,,,,,,,,,,,,,,,,,'
                     
 
               READ(RECLN,*,ERR=200) DELETE,YEAR,   &
                 (EMIS_DISPATCH_ADDER(I),I=1,3),(EMIS_CAP(I),I=1,3),   &
                 (PURCH_CRED_PRICE(I),I=1,3),(SELL_CRED_PRICE(I),I=1,3),   &
                 COMMENT,(EMIS_STRAT(I),I=1,3),(MIN_BANK(I),I=1,3),   &
                 (MAX_BANK(I),I=1,3),EPA_EAS,EPA_SETASIDE_RATE,   &
                 (EMIS_DISPATCH_ADDER(I),I=4,5),(EMIS_CAP(I),I=4,5),   &
                 (PURCH_CRED_PRICE(I),I=4,5),(SELL_CRED_PRICE(I),I=4,5),   &
                 (EMIS_STRAT(I),I=4,5),(MIN_BANK(I),I=4,5),   &
                 (MAX_BANK(I),I=4,5),class_name_loc,CLASS_NUM,   &
                  (EMISS_COMMENT,I=1,5),   &
                  CO2_PUSH_OK,   &
                  SO2_PUSH_OK
                  
               if (times_eno==1 .and. class_num==0) then
                    er_message="en_objt:0009 - " // &
                    "CLASS_NUM is zero when it should be 281."
                    call end_program(er_message)

               end if
               
               IF(INDEX(CO2_PUSH_OK,'Yes') == 0) CO2_PUSH_OK = 'No'
               IF(INDEX(SO2_PUSH_OK,'Yes') == 0) SO2_PUSH_OK = 'No'
               

               if(.not. init_file_is_active() .and. times_eno==1) then
                er_message="en_objt:0010 - " // &
                "init_file_is_active should return .true. the first " // &
                "time through, but it returns .false."
                call end_program(er_message)
               end if
               ! Check results of init_file_is_active()
               ifa=INIT_FILE_IS_ACTIVE()
               IF(.NOT. INIT_FILE_IS_ACTIVE()) then
                CLASS_NUM = 0
               end if
               IREC = IREC + 1
               FILE_NAME=get_filename_from_unit(11)
               
               if(class_num==0) then
               er_message="en_objt:0011 - CLASS_NUM is zero before " // &
               "write to " // trim(get_filename_from_unit(11)) // "."
               call  end_program(er_message)
               end if

               WRITE(11,REC=IREC) DELETE,YEAR,   &
                   EMIS_DISPATCH_ADDER,   &
                   EMIS_CAP,PURCH_CRED_PRICE,SELL_CRED_PRICE,EMIS_STRAT,   &
                   MIN_BANK,MAX_BANK,EPA_EAS,EPA_SETASIDE_RATE,   &
                   CO2_PUSH_OK,SO2_PUSH_OK,class_name_loc,CLASS_NUM
               DATA_RECORDS_IN_TABLE = DATA_RECORDS_IN_TABLE + 1
!
               IF(FIRST_RECORD_OF_TABLE) THEN
                  VOID_INT2 = CREATE_CLASS_LIST(CLASS_NUM)
                  FIRST_RECORD_OF_TABLE = .FALSE.
               ENDIF
            ENDDO
            IF(DATA_RECORDS_IN_TABLE < AVAIL_DATA_YEARS) THEN
               DO WHILE (DATA_RECORDS_IN_TABLE < AVAIL_DATA_YEARS)
                  IREC = IREC + 1
                  WRITE(11,REC=IREC) DELETE,YEAR,   &
                            EMIS_DISPATCH_ADDER,   &
                            EMIS_CAP,PURCH_CRED_PRICE,   &
                            SELL_CRED_PRICE,EMIS_STRAT,   &
                            MIN_BANK,MAX_BANK,EPA_EAS,EPA_SETASIDE_RATE,   &
                            CO2_PUSH_OK,SO2_PUSH_OK,class_name_loc,CLASS_NUM
                  DATA_RECORDS_IN_TABLE = DATA_RECORDS_IN_TABLE + 1
               ENDDO
            ENDIF
            IF(IOS /= 0) EXIT
         ENDDO
         CLOSE(10)
         CLOSE(11)
         MAX_BC_CLASS_NUMBER = MAX_CLASS_IN_TABLE()
         ALLOCATE(BC_TABLE_POSITION_FOR(0:MAX_BC_CLASS_NUMBER))
         MAX_BC_TABLE_NUMBER = PASS_TABLE_POINTER(BC_TABLE_POSITION_FOR)
      ELSE IF(INDEX(ENV_COM_FIL(),'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "EN_OBJT:1111")
      ENDIF
      RETURN

! OVERLAY THE ENVIRONMENTAL FILE
    
      ENTRY EN_MAKEOVL(OVERLAY_FAMILY_NAME)
    
!
      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      FILE_NAME=trim(OUTPUT_DIRECTORY())//"ENO"//   &
                                     trim(OVERLAY_FAMILY_NAME)//".DAT"
      OPEN(10,FILE=FILE_NAME)
      INUNIT = 12
      IF(ENV_COM_OL == 'BC') THEN
         INUNIT = 11
         FILE_NAME = trim(OUTPUT_DIRECTORY())//"BCENVCOM.BIN"
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         IF(FILE_EXISTS) THEN
            OPEN(11,FILE= FILE_NAME,ACCESS="DIRECT",RECL=LRECL)
         ELSE
            CALL STOP_NO_BASE_FILE(FILE_TYPE)
         ENDIF
      ENDIF
      OPEN(12,FILE=trim(OUTPUT_DIRECTORY())//"OLENVCOM.BIN",   &
                            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      FIRST_RECORD_OF_TABLE = INIT_CREATE_CLASS_LIST(INT(0,2))
      IREC = 0
      TABLE = 0
      READ(10,*,IOSTAT=IOS) DELETE
      DO
         FIRST_RECORD_OF_TABLE = .TRUE.
         TABLE = TABLE + 1
         DO
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(RECLN(1:1) == '7') EXIT
            IREC = IREC + 1
            READ(INUNIT,REC=IREC,IOSTAT=IOS_BASE) DELETE,YEAR,   &
                         EMIS_DISPATCH_ADDER,EMIS_CAP,   &
                         PURCH_CRED_PRICE,SELL_CRED_PRICE,EMIS_STRAT,   &
                         MIN_BANK,MAX_BANK,EPA_EAS,EPA_SETASIDE_RATE,   &
                         CO2_PUSH_OK,SO2_PUSH_OK,class_name_loc,CLASS_NUM
            IF(IOS_BASE /= 0) EXIT
            IF(IOS == 0) THEN
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=200) DELETE,YEAR,   &
                  (EMIS_DISPATCH_ADDER(I),I=1,3),(EMIS_CAP(I),I=1,3),   &
                  (PURCH_CRED_PRICE(I),I=1,3),   &
                  (SELL_CRED_PRICE(I),I=1,3),   &
                  COMMENT,(EMIS_STRAT(I),I=1,3),(MIN_BANK(I),I=1,3),   &
                  (MAX_BANK(I),I=1,3),EPA_EAS,EPA_SETASIDE_RATE,   &
                  (EMIS_DISPATCH_ADDER(I),I=4,5),(EMIS_CAP(I),I=4,5),   &
                  (PURCH_CRED_PRICE(I),I=4,5),   &
                  (SELL_CRED_PRICE(I),I=4,5),   &
                  (EMIS_STRAT(I),I=4,5),(MIN_BANK(I),I=4,5),   &
                  (MAX_BANK(I),I=4,5),class_name_loc,CLASS_NUM,   &
                  (EMISS_COMMENT,I=1,5),   &
                   CO2_PUSH_OK,SO2_PUSH_OK
            ENDIF
            WRITE(12,REC=IREC) DELETE,YEAR,   &
                         EMIS_DISPATCH_ADDER,EMIS_CAP,   &
                         PURCH_CRED_PRICE,SELL_CRED_PRICE,EMIS_STRAT,   &
                         MIN_BANK,MAX_BANK,EPA_EAS,EPA_SETASIDE_RATE,   &
                         CO2_PUSH_OK,SO2_PUSH_OK,class_name_loc,CLASS_NUM
            IF(FIRST_RECORD_OF_TABLE) THEN
               VOID_INT2 = CREATE_CLASS_LIST(CLASS_NUM)
               FIRST_RECORD_OF_TABLE = .FALSE.
            ENDIF
         ENDDO
         IF(IOS_BASE /= 0) EXIT
         IREC = AVAIL_DATA_YEARS * TABLE
      ENDDO
      CLOSE(10)
      CLOSE(12)
      IF(ALLOCATED(OL_TABLE_POSITION_FOR))   &
                                       DEALLOCATE(OL_TABLE_POSITION_FOR)
      MAX_OL_CLASS_NUMBER = MAX_CLASS_IN_TABLE()
      ALLOCATE(OL_TABLE_POSITION_FOR(0:MAX_OL_CLASS_NUMBER))
      MAX_OL_TABLE_NUMBER = PASS_TABLE_POINTER(OL_TABLE_POSITION_FOR)
      IF(ENV_COM_OL == 'BC') CLOSE(11)
      ENV_COM_OL = 'OL'
      RETURN

  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from EN_OBJT SIID95'
      call end_program(er_message)
    
      ENTRY RESET_ENV_COM_OL
    
         ENV_COM_OL = 'BC'
      RETURN

    
      ENTRY OPEN_ENVIR_TABLES(R_ENVIR_UNIT,R_ENVIR_FILE_EXISTS)
    
         FILE_NAME=trim(OUTPUT_DIRECTORY())//ENV_COM_OL//"ENVCOM.BIN"
         if(index(file_name, "BCENVCOM.BIN")>0) then
            FILE_NAME=FILE_NAME ! Debugstop
         end if
         INQUIRE(FILE=FILE_NAME,EXIST=ENVIR_FILE_EXISTS)
         IF(ENVIR_FILE_EXISTS) OPEN(R_ENVIR_UNIT,FILE=FILE_NAME,   &
                                             ACCESS='DIRECT',RECL=LRECL)
         R_ENVIR_FILE_EXISTS = ENVIR_FILE_EXISTS
      RETURN

    
      ENTRY RETURN_ENVIR_FILE_SIZE(R_MAX_CLASS_NUMBER)
    
         IF(ENV_COM_OL == 'OL') THEN
            R_MAX_CLASS_NUMBER = MAX_OL_CLASS_NUMBER
         ELSE
            R_MAX_CLASS_NUMBER = MAX_BC_CLASS_NUMBER
         ENDIF
      RETURN
    
      ENTRY RETURN_ENVIR_FILE_CLASS_INFO(R_TABLE_POINTR,R_MAX_TABLE_NUM)
    
         IF(ENV_COM_OL == 'OL') THEN
            R_MAX_TABLE_NUM = MAX_OL_TABLE_NUMBER
            DO I = 0, MAX_OL_CLASS_NUMBER
               IF(ENVIR_FILE_EXISTS) THEN
                  R_TABLE_POINTR(I) = OL_TABLE_POSITION_FOR(I)
               ELSE
                  R_TABLE_POINTR(I) = 0
               ENDIF
            ENDDO
         ELSE
            R_MAX_TABLE_NUM = MAX_BC_TABLE_NUMBER
            DO I = 0, MAX_BC_CLASS_NUMBER
               IF(ENVIR_FILE_EXISTS) THEN
                  R_TABLE_POINTR(I) = BC_TABLE_POSITION_FOR(I)
               ELSE
                  R_TABLE_POINTR(I) = 0
               ENDIF
            ENDDO
         ENDIF
      RETURN
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END

    
!              ROUTINE FOR READING ENVIRONMENTAL EMISSIONS DATA
!              FOR FIVE EMISSIONS GROUPS
!
!              COPYRIGHT (C) 1991, 1995 M.S. GERBER & ASSOCIATES, INC.
!                         ALL RIGHTS RESERVED
!
    
      FUNCTION ENVIR_DATA_READ()
      use end_routine, only: end_program, er_message
      use mod_base_year
      use SpinDriftLib
      use prod_arrays_dimensions
      USE CO2_CAP_N_TRADE
      use globecom
      USE SIZECOM
      use envircom
      use enrglimit
      use string
      use miscmod
      use program_state
      use debugtrace
      
      CHARACTER (len=32) ::  class_name_loc
      CHARACTER (LEN=2) :: CAPACITY_PLANNING_METHOD,   &
                           GREEN_MRX_METHOD
      LOGICAL (kind=1) ::  USE_TABLE=.FALSE.
      REAL ::  EMISS_PURCH_CRED_PRICE,EMISS_SELL_CRED_PRICE
      INTEGER (kind=2) ::  DELETE
      INTEGER (kind=2) ::  YR,EMIS_TYPE,TABLE,IREC,R_CLASS,R_VECTOR
      INTEGER (kind=2) ::  MAX_CLASS_NUM,   &
                MAX_TABLE_NUM,   &
                CLASS,ITEM
      INTEGER (kind=4) ::  IOS
      INTEGER (kind=4) ::  R_INT4_EMIS_TYPE
      INTEGER (kind=2) ::  R_EMIS_TYPE,MO
      SAVE MAX_CLASS_NUM,MAX_TABLE_NUM,CLASS,TABLE
      INTEGER (kind=2) ::  ENVIR_CLASS_2_TABLE(:)
      ALLOCATABLE :: ENVIR_CLASS_2_TABLE
      SAVE ENVIR_CLASS_2_TABLE
      LOGICAL (kind=4) ::  FILE_EXISTS=.FALSE.
      INTEGER (kind=2) ::  ENVIR_UNIT,SO2,   &
                L_CLASS,GET_EMISSION_MARKET_LINK,R_UNIT
      PARAMETER(ENVIR_UNIT = 43,SO2=1)
      LOGICAL (kind=1) ::  ENVIR_DATA_READ,CLASS_EMISSIONS_CREDITS,   &
                REPORT_CLASS_EMISSIONS_INFO,ZERO_TOTAL_EMIS_VARS,   &
                EMISSIONS_CREDITS,   &
                DEFINED_CLASS(:),   &
                EMPIRE,EMPIRE_IS_ACTIVE=.FALSE. ,   &
                PUT_TRANS_MRX_DISP_EMIS_ADDER,   &
                PUT_GRX_DISP_EMIS_ADDER,   &
                PUT_CoalLP_SO2_DISP_EMIS_ADDER
      
      CHARACTER (LEN=3) :: CO2_PUSH_OK,SO2_PUSH_OK
      INTEGER (kind=2) ::  CLASS_NUM,   &
                I,   &
                LOCAL_VALUE,   &
                GET_MRX_EMISSION_MARKET_LINK
      REAL ::   DISPATCH_EMIS_ADDER,DISPATCH_EMIS_CLASS_ADDER,   &
            VECTOR_ASSET_CLASS_ID(AVAIL_DATA_YEARS),   &
            ESCALATED_MONTHLY_VALUE,RTEMP,GET_VAR
      INTEGER (kind=4) ::  VALUES_2_SET
!
      REAL ::  R_CLASS_INC_EMIS_EXPENSE,R_CLASS_INC_EMIS_REVENUE
      REAL ::  CLASS_INC_EMIS_EXPENSE,CLASS_INC_EMIS_REVENUE,   &
           CLASS_INC_NET_EMIS_EXPENSE
      SAVE CLASS_INC_EMIS_EXPENSE,CLASS_INC_EMIS_REVENUE,   &
           CLASS_INC_NET_EMIS_EXPENSE
      REAL ::  CLASS_EPA_EAS(:),   &
           CLASS_EPA_SETASIDE_RATE(:),   &
           CLASS_EMIS_DISPATCH_ADDER(:,:),   &
           MONTH_EMIS_DISPATCH_ADDER(:,:,:),   &
           CLASS_EMIS_CAP(:,:),   &
           CLASS_PURCH_CRED_PRICE(:,:),   &
           CLASS_SELL_CRED_PRICE(:,:),   &
           MONTH_PURCH_CRED_PRICE(:,:,:),   &
           MONTH_SELL_CRED_PRICE(:,:,:),   &
           CLASS_MIN_BANK(:,:),   &
           CLASS_MAX_BANK(:,:),   &
           CLASS_EMISSION_CAP(:)
      LOGICAL (KIND=1) , ALLOCATABLE, SAVE :: ACCEPT_CO2_PUSH(:),   &
                                             ACCEPT_SO2_PUSH(:)

      CHARACTER (len=1) ::  CLASS_EMIS_STRAT(:,:),ASSET_DATA_TYPE
      CHARACTER (len=20) ::   &
          EMISSIONS_DATA_POINTER='EMISSION DATA POINTR'
      ALLOCATABLE :: CLASS_EPA_EAS,CLASS_EMIS_DISPATCH_ADDER,   &
                     MONTH_EMIS_DISPATCH_ADDER,   &
                     CLASS_EMIS_CAP,CLASS_EPA_SETASIDE_RATE,   &
                     CLASS_PURCH_CRED_PRICE,CLASS_SELL_CRED_PRICE,   &
                     MONTH_PURCH_CRED_PRICE,MONTH_SELL_CRED_PRICE,   &
                     CLASS_EMIS_STRAT,CLASS_MIN_BANK,CLASS_MAX_BANK,   &
                     CLASS_EMISSION_CAP,   &
                     DEFINED_CLASS
      SAVE CLASS_EPA_EAS,CLASS_EMIS_DISPATCH_ADDER,   &
           MONTH_EMIS_DISPATCH_ADDER,   &
           CLASS_EMIS_CAP,CLASS_EPA_SETASIDE_RATE,   &
           CLASS_PURCH_CRED_PRICE,CLASS_SELL_CRED_PRICE,   &
           MONTH_PURCH_CRED_PRICE,MONTH_SELL_CRED_PRICE,   &
           CLASS_EMIS_STRAT,CLASS_MIN_BANK,CLASS_MAX_BANK,   &
           CLASS_EMISSION_CAP,   &
           DEFINED_CLASS
      REAL ::  TOTAL_EL_SO2_ANNUAL,CLASS_EPA_SETASIDE_REVENUE,   &
           CLASS_EPA_SETASIDE_CREDITS,ACTIVE_EMISSION_TABLES
      SAVE TOTAL_EL_SO2_ANNUAL,CLASS_EPA_SETASIDE_REVENUE,   &
           CLASS_EPA_SETASIDE_CREDITS,ACTIVE_EMISSION_TABLES
!
!     ENVIRONMENTAL VARIABLES
!

!
! 1 = SOX, 2 = NOX, 3 = CO2 1 4 = OTHER 2 5 = OTHER 3
!
      REAL ::  CLASS_NET_ANN_EMIS(:),   &
           CLASS_EMIS_EXPENSE(:),   &
           CLASS_EMIS_REVENUE(:),   &
           CLASS_EMIS_SELL(:),   &
           CLASS_EMIS_PURCH(:),   &
           CLASS_CUM_ANN_EMIS(:,:),   &
           AVE_PURCH_CRED_PRICE(:),   &
           AVE_SELL_CRED_PRICE(:)

      REAL ::   CLASS_CL_EMISSIONS(:),   &
            CLASS_EMISSIONS(:),   &
            RPT_CUM_ANN_EMIS(:),   &
            CLASS_EL_SO2_ANNUAL,CLASS_CT_SO2_ANNUAL,CLASS_CL_SO2_ANNUAL,   &
            GET_CL_EMISSIONS,   &
            SCEN_EMIS_MULT,   &
            GET_SCENARIO_SO2_PRICES,   &
            GET_SCENARIO_NOX_PRICES,   &
            GET_SCENARIO_CO2_PRICES,   &
            GET_SCENARIO_HG_PRICES,   &
            GET_SCENARIO_ANN_NOX_PRICES,   &
            TRANS_DISPATCH_EMIS_ADDER,   &
            TRANS_MRX_DISPATCH_EMIS_ADDER,   &
            R_PRICE,   &
            GET_EMISS_CAP_FOR_CLASS

      SAVE CLASS_EL_SO2_ANNUAL,CLASS_CT_SO2_ANNUAL
      REAL ::  R_VARIABLES(0:*)
      ALLOCATABLE :: CLASS_CUM_ANN_EMIS,   &
               CLASS_NET_ANN_EMIS,CLASS_EMIS_EXPENSE,CLASS_EMIS_REVENUE,   &
               CLASS_EMIS_SELL,CLASS_EMIS_PURCH,   &
               CLASS_CL_EMISSIONS,   &
               CLASS_EMISSIONS,   &
               RPT_CUM_ANN_EMIS,   &
               AVE_PURCH_CRED_PRICE,   &
               AVE_SELL_CRED_PRICE
      SAVE CLASS_CUM_ANN_EMIS,   &
           CLASS_NET_ANN_EMIS,CLASS_EMIS_EXPENSE,CLASS_EMIS_REVENUE,   &
           CLASS_EMIS_SELL,CLASS_EMIS_PURCH,   &
           CLASS_CL_EMISSIONS,   &
           CLASS_EMISSIONS,   &
           RPT_CUM_ANN_EMIS,   &
           AVE_PURCH_CRED_PRICE,   &
           AVE_SELL_CRED_PRICE
!
      REAL (KIND=4) , SAVE, ALLOCATABLE :: EMIS_DISPATCH_ADDER_ESC(:,:),   &
                                          PURCH_CRED_PRICE_ESC(:,:),   &
                                          SELL_CRED_PRICE_ESC(:,:)
      INTEGER (kind=2) ::  MAX_INIT_CLASS_NUM
      INTEGER (kind=2) ::  NUM_OF_INIT_CLASSES,R_YEAR,R_MONTH
      integer :: file_trace_endr=0
      integer :: times_edr=0
      real :: GVResult
      character (len=512) :: fname
      SAVE MAX_INIT_CLASS_NUM
!   
! END OF DATA DECLARATIONS
!
       if(file_trace_endr==0) then
          file_trace_endr=open_trace("en_data_read.trace", rq_edr)

          call write_trace_message(file_trace_endr, "Tracing " // &
            "function en_data_read from en_objt.")
            
      end if
      times_edr=times_edr+1
      call write_trace_message(file_trace_endr, "ENVIR_DATA_READ()")
      call write_trace_int2(file_trace_endr, "YEAR", globecom_YEAR)
      
      IF(globecom_year == 1) THEN

         EMPIRE_IS_ACTIVE = EMPIRE()
         call write_trace_bool1(file_trace_endr, "EMPIRE_IS_ACTIVE", &
            EMPIRE_IS_ACTIVE)
         CALL RETURN_ENVIR_FILE_SIZE(MAX_CLASS_NUM)
         call write_trace_int2(file_trace_endr, "MAX_CLASS_NUM", &
            MAX_CLASS_NUM)
            
         CALL RETURN_INITIALIZATION_CLASSES(NUM_OF_INIT_CLASSES,   &
                                            MAX_INIT_CLASS_NUM)
        call write_trace_int2(file_trace_endr,  "NUM_OF_INIT_CLASSES", &
            NUM_OF_INIT_CLASSES)
            
         IF(ALLOCATED(ENVIR_CLASS_2_TABLE))   &
                                         DEALLOCATE(ENVIR_CLASS_2_TABLE)
         ALLOCATE(ENVIR_CLASS_2_TABLE(0:MAX_CLASS_NUM))
         CALL RETURN_ENVIR_FILE_CLASS_INFO(ENVIR_CLASS_2_TABLE,   &
                                                          MAX_TABLE_NUM)
       call write_trace_int2(file_trace_endr, "ENVIR_UNIT", ENVIR_UNIT)
         CALL OPEN_ENVIR_TABLES(ENVIR_UNIT,FILE_EXISTS)
         
         IF(ALLOCATED(CLASS_EPA_EAS)) DEALLOCATE(CLASS_EPA_EAS,   &
                                           DEFINED_CLASS,   &
                                           CLASS_EPA_SETASIDE_RATE,   &
                                           CLASS_EMIS_DISPATCH_ADDER,   &
                                           MONTH_EMIS_DISPATCH_ADDER,   &
                                           CLASS_EMIS_CAP,   &
                                           CLASS_PURCH_CRED_PRICE,   &
                                           CLASS_SELL_CRED_PRICE,   &
                                           MONTH_PURCH_CRED_PRICE,   &
                                           MONTH_SELL_CRED_PRICE,   &
                                           CLASS_EMIS_STRAT,   &
                                           CLASS_MIN_BANK,   &
                                           CLASS_MAX_BANK,   &
                                           CLASS_EMISSION_CAP,   &
                                           EMIS_DISPATCH_ADDER_ESC,   &
                                           PURCH_CRED_PRICE_ESC,   &
                                           SELL_CRED_PRICE_ESC,   &
                                           CLASS_NET_ANN_EMIS,   &
                                           CLASS_EMIS_EXPENSE,   &
                                           CLASS_EMIS_REVENUE,   &
                                           CLASS_CL_EMISSIONS,   &
                                           CLASS_EMISSIONS,   &
                                           RPT_CUM_ANN_EMIS,   &
                                           CLASS_EMIS_SELL,   &
                                           CLASS_EMIS_PURCH,   &
                                           CLASS_CUM_ANN_EMIS,   &
                                           AVE_PURCH_CRED_PRICE,   &
                                           AVE_SELL_CRED_PRICE,   &
                                           ACCEPT_CO2_PUSH,   &
                                           ACCEPT_SO2_PUSH)

         ALLOCATE(CLASS_EPA_EAS(0:MAX_TABLE_NUM),   &
             DEFINED_CLASS(0:9999),   &
             CLASS_EPA_SETASIDE_RATE(0:MAX_TABLE_NUM),   &
             CLASS_EMIS_DISPATCH_ADDER(NUMBER_OF_EMISSION_TYPES,   &
                                                       0:MAX_TABLE_NUM),   &
             MONTH_EMIS_DISPATCH_ADDER(NUMBER_OF_EMISSION_TYPES,   &
                                                  0:MAX_TABLE_NUM,0:12),   &
             CLASS_EMIS_CAP(NUMBER_OF_EMISSION_TYPES,0:MAX_TABLE_NUM),   &
             CLASS_PURCH_CRED_PRICE(NUMBER_OF_EMISSION_TYPES,   &
                                                       0:MAX_TABLE_NUM),   &
             CLASS_SELL_CRED_PRICE(NUMBER_OF_EMISSION_TYPES,   &
                                                       0:MAX_TABLE_NUM),   &
             MONTH_PURCH_CRED_PRICE(NUMBER_OF_EMISSION_TYPES,   &
                                                  0:MAX_TABLE_NUM,0:12),   &
             MONTH_SELL_CRED_PRICE(NUMBER_OF_EMISSION_TYPES,   &
                                                  0:MAX_TABLE_NUM,0:12),   &
             CLASS_EMIS_STRAT(NUMBER_OF_EMISSION_TYPES,0:MAX_TABLE_NUM),   &
             CLASS_MIN_BANK(NUMBER_OF_EMISSION_TYPES,0:MAX_TABLE_NUM),   &
             CLASS_MAX_BANK(NUMBER_OF_EMISSION_TYPES,0:MAX_TABLE_NUM),   &
             EMIS_DISPATCH_ADDER_ESC(NUMBER_OF_EMISSION_TYPES,   &
                                                       0:MAX_TABLE_NUM),   &
             PURCH_CRED_PRICE_ESC(NUMBER_OF_EMISSION_TYPES,   &
                                                       0:MAX_TABLE_NUM),   &
             SELL_CRED_PRICE_ESC(NUMBER_OF_EMISSION_TYPES,   &
                                                       0:MAX_TABLE_NUM),   &
             ACCEPT_CO2_PUSH(0:MAX_TABLE_NUM),   &
             ACCEPT_SO2_PUSH(0:MAX_TABLE_NUM))
!
! CLASS RESULTS VALUES
!
         ALLOCATE(CLASS_NET_ANN_EMIS(NUMBER_OF_EMISSION_TYPES),   &
             CLASS_EMIS_EXPENSE(NUMBER_OF_EMISSION_TYPES),   &
             CLASS_EMIS_REVENUE(NUMBER_OF_EMISSION_TYPES),   &
             CLASS_EMISSION_CAP(NUMBER_OF_EMISSION_TYPES),   &
             CLASS_CL_EMISSIONS(NUMBER_OF_EMISSION_TYPES),   &
             CLASS_EMISSIONS(NUMBER_OF_EMISSION_TYPES),   &
             RPT_CUM_ANN_EMIS(NUMBER_OF_EMISSION_TYPES),   &
             CLASS_EMIS_SELL(NUMBER_OF_EMISSION_TYPES),   &
             CLASS_EMIS_PURCH(NUMBER_OF_EMISSION_TYPES),   &
             AVE_PURCH_CRED_PRICE(NUMBER_OF_EMISSION_TYPES),   &
             AVE_SELL_CRED_PRICE(NUMBER_OF_EMISSION_TYPES),   &
             CLASS_CUM_ANN_EMIS(NUMBER_OF_EMISSION_TYPES,   &
                                                  0:MAX_INIT_CLASS_NUM))
!
! INITIALIZE VALUES
!
!
         CLASS_EMISSION_CAP = 0.
! 11/19/02.
         DEFINED_CLASS = .FALSE.
!
         CLASS_EPA_EAS = 0.
         CLASS_EPA_SETASIDE_RATE = 0.
         CLASS_EMIS_DISPATCH_ADDER = 0.
         CLASS_EMIS_CAP = 0.
         CLASS_PURCH_CRED_PRICE = 0.
         CLASS_SELL_CRED_PRICE = 0.
         CLASS_MIN_BANK = 0.
         CLASS_MAX_BANK = 0.
         CLASS_CUM_ANN_EMIS = 0.
         CLASS_EMIS_STRAT = "N"
         EMIS_DISPATCH_ADDER_ESC = 0.
         PURCH_CRED_PRICE_ESC = 0.
         SELL_CRED_PRICE_ESC = 0.
!
      ENDIF
!
! 083006
!
      ENVIR_DATA_READ = FILE_EXISTS
      call write_trace_bool1(file_trace_endr, "1. ENVIR_DATA_READ", &
        ENVIR_DATA_READ)
      call write_trace_bool4(file_trace_endr, "FILE_EXISTS", &
        FILE_EXISTS)
      call write_trace_int2(file_trace_endr, "AVAIL_DATA_YEARS", &
        AVAIL_DATA_YEARS)
      
      
      IF(FILE_EXISTS) THEN
!
         IF(globecom_year <= AVAIL_DATA_YEARS) THEN
            MONTH_EMIS_DISPATCH_ADDER = 0.
            MONTH_PURCH_CRED_PRICE = 0.
            MONTH_SELL_CRED_PRICE = 0.
!
! ZERO ANNUAL RESULTS VARIABLES
!
            AVE_PURCH_CRED_PRICE = 0.
            AVE_SELL_CRED_PRICE = 0.
            TOTAL_EL_SO2_ANNUAL = 0.
            INC_EMIS_EXPENSE = 0.
            INC_EMIS_REVENUE = 0.
            EPA_SETASIDE_CREDITS = 0.
            EPA_SETASIDE_REVENUE = 0.
            ACTIVE_EMISSION_TABLES = 0.
!
! READ THE TABLES INTEGER*2 THE ENVIRONMENT DATA FILE
!
            IREC = MIN(globecom_year,AVAIL_DATA_YEARS) 
            call write_trace_int2(file_trace_endr, "IREC", IREC)
            fname=get_filename_from_unit(43)
            call write_trace_string(file_trace_endr, "fname", trim(fname))
            DO TABLE = 1, MAX_TABLE_NUM
               call write_trace_int2(file_trace_endr, "READ REC", IREC)

! en_objt:0012. Write is at en_objt:0013
                      
               READ(43,REC=IREC,IOSTAT=IOS) DELETE,YR,   &
                         (CLASS_EMIS_DISPATCH_ADDER(EMIS_TYPE,TABLE),   &
                               EMIS_TYPE = 1, NUMBER_OF_EMISSION_TYPES),   &
                         (CLASS_EMIS_CAP(EMIS_TYPE,TABLE),   &
                               EMIS_TYPE = 1, NUMBER_OF_EMISSION_TYPES),   &
                         (CLASS_PURCH_CRED_PRICE(EMIS_TYPE,TABLE),   &
                               EMIS_TYPE = 1, NUMBER_OF_EMISSION_TYPES),   &
                         (CLASS_SELL_CRED_PRICE(EMIS_TYPE,TABLE),   &
                               EMIS_TYPE = 1, NUMBER_OF_EMISSION_TYPES),   &
                         (CLASS_EMIS_STRAT(EMIS_TYPE,TABLE),   &
                               EMIS_TYPE = 1, NUMBER_OF_EMISSION_TYPES),   &
                         (CLASS_MIN_BANK(EMIS_TYPE,TABLE),   &
                               EMIS_TYPE = 1, NUMBER_OF_EMISSION_TYPES),   &
                         (CLASS_MAX_BANK(EMIS_TYPE,TABLE),   &
                               EMIS_TYPE = 1, NUMBER_OF_EMISSION_TYPES),   &
                          CLASS_EPA_EAS(TABLE),   &
                          CLASS_EPA_SETASIDE_RATE(TABLE),   &
                          CO2_PUSH_OK,   &
                          SO2_PUSH_OK,   &
                          CLASS_NAME_loc,   &
                          CLASS_NUM ! bad
                          

               
               ACCEPT_CO2_PUSH(TABLE) = CO2_PUSH_OK(1:1) == 'Y'
               ACCEPT_SO2_PUSH(TABLE) = SO2_PUSH_OK(1:1) == 'Y'
               
               call write_trace_bool1(file_trace_endr, "ACCEPT_CO2_PUSH(TABLE)", &
                ACCEPT_CO2_PUSH(TABLE))
               call write_trace_bool1(file_trace_endr, "ACCEPT_SO2_PUSH(TABLE)", &
                ACCEPT_SO2_PUSH(TABLE))
               call write_trace_int2(file_trace_endr, "TABLE", TABLE)
                
               

               call write_trace_int2(file_trace_endr, "CLASS_NUM", &
                CLASS_NUM)
               IF(CLASS_NUM >= 0 .AND. CLASS_NUM <= 999) THEN


                  
                  DEFINED_CLASS(CLASS_NUM) = .TRUE.
                  call write_trace_bool1(file_trace_endr, &
                   "DEFINED_CLASS(CLASS_NUM)", DEFINED_CLASS(CLASS_NUM))
!
! 04/08/03.
!
               ELSEIF(CLASS_NUM < 0) THEN
                  CALL GET_ASSET_VAR(ABS(CLASS_NUM),ASSET_DATA_TYPE,   &
                                                  VECTOR_ASSET_CLASS_ID)
                  DO I = 1, AVAIL_DATA_YEARS
                     LOCAL_VALUE = VECTOR_ASSET_CLASS_ID(I)
                     IF(LOCAL_VALUE > 0 .AND.   &
                                   LOCAL_VALUE <= 999) THEN
                        DEFINED_CLASS(LOCAL_VALUE) = .TRUE.
                       ENVIR_CLASS_2_TABLE(LOCAL_VALUE+1) = TABLE
                     ELSEIF(LOCAL_VALUE == 0) THEN
                    call write_trace_message(file_trace_endr, "1. EXIT")
                        EXIT
                     ENDIF
                  ENDDO
               ENDIF

               IF(IOS /= 0) THEN
                  fname=get_filename_from_unit(43)
               call write_trace_string(file_trace_endr, "fname", fname)
                  WRITE(4,*) 'Error reading Emissions Data file at'//   &
                             ' record ',IREC,' for year',globecom_YEAR
                  WRITE(4,*) '*** line 546 EN_OBJT.FOR ***'
                  
                  er_message="EN_OBJT:0001 - " // &
                   "Error " // trim(itos(int(ios))) // & 
                   " reading Emissions Data file " // &
                   trim(fname) // " at " //   &
                   ' record '//trim(itos(int(irec))) // &
                   ' for year ' // trim(itos(int(globecom_year))) // &
                   " times_edr="//trim(itos(times_edr))
                   
                  call end_program(er_message)
                  
               ENDIF
               IREC = IREC + AVAIL_DATA_YEARS
            ENDDO
!
! 01/31/06. CONVERSION FROM ANNUAL TO MONTHLY.
!

        call write_trace_message(file_trace_endr, "Iterating over tables.")
        call write_trace_int2(file_trace_endr, "YEAR", globecom_year)
        call write_trace_int2(file_trace_endr, "IREC", IREC)
            

            

            
            DO TABLE = 1, MAX_TABLE_NUM
               DO EMIS_TYPE = 1, NUMBER_OF_EMISSION_TYPES
                  DO MO = 1, 12
                  
                 call write_trace_int2(file_trace_endr, "EMIS_TYPE",  &
                        EMIS_TYPE)
                    call write_trace_int2(file_trace_endr, "TABLE", &
                    TABLE)
                    call write_trace_int2(file_trace_endr, "MO", MO)
                    

                    
                     IF(CLASS_EMIS_DISPATCH_ADDER(EMIS_TYPE,TABLE) >=   &
                                                                0.) THEN
                        MONTH_EMIS_DISPATCH_ADDER(EMIS_TYPE,TABLE,MO) =   &
                             CLASS_EMIS_DISPATCH_ADDER(EMIS_TYPE,TABLE)/   &
                                                                  2000.
                     ELSE
                        RTEMP =   &
                              CLASS_EMIS_DISPATCH_ADDER(EMIS_TYPE,TABLE)

        if(file_trace_endr/= BAD_TRACE_HANDLE) then
                            write(file_trace_endr,*) &
                                "RTEMP 2. ", RTEMP, &
                                "MO", MO, "EMISSIONS_DATA_POINTER", &
                                "......", &
                                "EMIS_TYPE", EMIS_TYPE, &
                                "TABLE", TABLE
              get_var_called_from="en_objt:554"
              write(file_trace_endr,*) "1. Calling GET_VAR."
             write(file_trace_endr,*) & 
             "RTEMP 1. ", RTEMP, &
             "MO", MO, "EMISSIONS_DATA_POINTER", &
             "......",  &
             "EMIS_TYPE", EMIS_TYPE,  &
              "TABLE", TABLE
      end if
      
          GVResult=GET_VAR(RTEMP,MO,EMISSIONS_DATA_POINTER)/2000.
          call write_trace_real(file_trace_endr, "GVResult", GVResult)


             MONTH_EMIS_DISPATCH_ADDER(EMIS_TYPE,TABLE,MO) =   &
                               GVResult
                     ENDIF
                     IF(CLASS_PURCH_CRED_PRICE(EMIS_TYPE,TABLE) >=   &
                                                                0.) THEN
                        MONTH_PURCH_CRED_PRICE(EMIS_TYPE,TABLE,MO) =   &
                              CLASS_PURCH_CRED_PRICE(EMIS_TYPE,TABLE)
                     ELSE
                        RTEMP = CLASS_PURCH_CRED_PRICE(EMIS_TYPE,TABLE)
                        ! EN_OBJT:0013
                        ! Bullet - should call at get_var 362271
                        get_var_called_from="en_objt:772"
                        MONTH_PURCH_CRED_PRICE(EMIS_TYPE,TABLE,MO) =   &
                                GET_VAR(RTEMP,MO,EMISSIONS_DATA_POINTER)
                     ENDIF
!
                     IF(CLASS_SELL_CRED_PRICE(EMIS_TYPE,TABLE) >=   &
                                                                0.) THEN
                        MONTH_SELL_CRED_PRICE(EMIS_TYPE,TABLE,MO) =   &
                              CLASS_SELL_CRED_PRICE(EMIS_TYPE,TABLE)
                     ELSE
                        get_var_called_from="en_objt:782"
                        RTEMP =   &
                              CLASS_SELL_CRED_PRICE(EMIS_TYPE,TABLE)
                        MONTH_SELL_CRED_PRICE(EMIS_TYPE,TABLE,MO) =   &
                                GET_VAR(RTEMP,MO,EMISSIONS_DATA_POINTER)
                     ENDIF
!
                  ENDDO

                  MONTH_EMIS_DISPATCH_ADDER(EMIS_TYPE,TABLE,0) = SUM(   &
                    MONTH_EMIS_DISPATCH_ADDER(EMIS_TYPE,TABLE,1:12))/12.
                    
                  call write_trace_real(file_trace_endr, &
                  "MONTH_EMIS_DISPATCH_ADDER(EMIS_TYPE,TABLE,0)", &
                  MONTH_EMIS_DISPATCH_ADDER(EMIS_TYPE,TABLE,0))
                  
                  MONTH_PURCH_CRED_PRICE(EMIS_TYPE,TABLE,0) = SUM(   &
                    MONTH_PURCH_CRED_PRICE(EMIS_TYPE,TABLE,1:12))/12.
                  MONTH_SELL_CRED_PRICE(EMIS_TYPE,TABLE,0) =SUM(   &
                        MONTH_SELL_CRED_PRICE(EMIS_TYPE,TABLE,1:12))/12.
                  IF(globecom_YEAR == AVAIL_DATA_YEARS-1) THEN  ! SAVE THIS YEAR FOR ESCALATIONS
                     EMIS_DISPATCH_ADDER_ESC(EMIS_TYPE,TABLE) =   &
                            MONTH_EMIS_DISPATCH_ADDER(EMIS_TYPE,TABLE,0)
!
                     PURCH_CRED_PRICE_ESC(EMIS_TYPE,TABLE) =   &
                               MONTH_PURCH_CRED_PRICE(EMIS_TYPE,TABLE,0)
!
                     SELL_CRED_PRICE_ESC(EMIS_TYPE,TABLE) =   &
                                MONTH_SELL_CRED_PRICE(EMIS_TYPE,TABLE,0)
                  ENDIF
            IF(globecom_YEAR == AVAIL_DATA_YEARS) THEN  ! CALCULATE IMPLIED ESCALATIONS
               call write_trace_message(file_trace_endr, "calculating " // &
                  "implied escalations.")
                  
                     CALL IMPLIED_ESC(   &
                           EMIS_DISPATCH_ADDER_ESC(EMIS_TYPE,TABLE),   &
                           MONTH_EMIS_DISPATCH_ADDER(EMIS_TYPE,TABLE,0))
!
                     CALL IMPLIED_ESC(   &
                              PURCH_CRED_PRICE_ESC(EMIS_TYPE,TABLE),   &
                              MONTH_PURCH_CRED_PRICE(EMIS_TYPE,TABLE,0))
!
                     CALL IMPLIED_ESC(   &
                               SELL_CRED_PRICE_ESC(EMIS_TYPE,TABLE),   &
                               MONTH_SELL_CRED_PRICE(EMIS_TYPE,TABLE,0))
                  ENDIF
               ENDDO
            ENDDO
         ELSE
            DO TABLE = 1, MAX_TABLE_NUM
               DO EMIS_TYPE = 1, NUMBER_OF_EMISSION_TYPES
                  MONTH_EMIS_DISPATCH_ADDER(EMIS_TYPE,TABLE,0:12) =   &
                      MONTH_EMIS_DISPATCH_ADDER(EMIS_TYPE,TABLE,0:12) *   &
                                EMIS_DISPATCH_ADDER_ESC(EMIS_TYPE,TABLE)
                  MONTH_PURCH_CRED_PRICE(EMIS_TYPE,TABLE,0:12) =   &
                      MONTH_PURCH_CRED_PRICE(EMIS_TYPE,TABLE,0:12) *   &
                                   PURCH_CRED_PRICE_ESC(EMIS_TYPE,TABLE)
                  MONTH_SELL_CRED_PRICE(EMIS_TYPE,TABLE,0:12) =   &
                      MONTH_SELL_CRED_PRICE(EMIS_TYPE,TABLE,0:12) *   &
                                    SELL_CRED_PRICE_ESC(EMIS_TYPE,TABLE)
               ENDDO
            ENDDO
         ENDIF
!
      ENDIF
      TABLE = TABLE
      RETURN

    
      ENTRY GET_EMISS_CAP_FOR_CLASS(R_EMIS_TYPE,R_CLASS)
    
         L_CLASS = ABS(R_CLASS)
!
         IF(EMPIRE_IS_ACTIVE .AND. L_CLASS+1 < 1) THEN
            TABLE = 1
         ELSEIF(MAX_TABLE_NUM == 0 .OR. (L_CLASS+1 < 1 .AND.   &
                                          L_CLASS < MAX_CLASS_NUM)) THEN
            TABLE = 0
         ELSEIF(.NOT. DEFINED_CLASS(L_CLASS)) THEN
            TABLE = 0
         ELSE
            TABLE = ENVIR_CLASS_2_TABLE(L_CLASS+1)
         ENDIF
         GET_EMISS_CAP_FOR_CLASS = CLASS_EMIS_CAP(R_EMIS_TYPE,TABLE)
      RETURN

    
      ENTRY TRANS_DISPATCH_EMIS_ADDER(R_INT4_EMIS_TYPE,R_MONTH,   &
                                                          R_YEAR,R_UNIT)
    
         TRANS_DISPATCH_EMIS_ADDER = 0.
         IF(R_INT4_EMIS_TYPE == 1) THEN
            SCEN_EMIS_MULT = GET_SCENARIO_SO2_PRICES(R_YEAR,R_MONTH)
         ELSEIF(R_INT4_EMIS_TYPE == 2) THEN
            SCEN_EMIS_MULT = GET_SCENARIO_NOX_PRICES(R_YEAR,R_MONTH)
         ELSEIF(R_INT4_EMIS_TYPE == 3) THEN
            IF(R_MONTH == 0) THEN
               SCEN_EMIS_MULT = 0.
               DO MO = 1, 12
                  SCEN_EMIS_MULT = SCEN_EMIS_MULT   &
                                   + GET_SCENARIO_CO2_PRICES(R_YEAR,MO)
               ENDDO
               SCEN_EMIS_MULT = SCEN_EMIS_MULT/12.
            ELSE
               SCEN_EMIS_MULT = GET_SCENARIO_CO2_PRICES(R_YEAR,R_MONTH)
            ENDIF
         ELSEIF(R_INT4_EMIS_TYPE == 4) THEN
            SCEN_EMIS_MULT = GET_SCENARIO_HG_PRICES(R_YEAR,R_MONTH)
         ELSEIF(R_INT4_EMIS_TYPE == 5) THEN
            SCEN_EMIS_MULT = GET_SCENARIO_ANN_NOX_PRICES(R_YEAR,R_MONTH)
         ELSE
            SCEN_EMIS_MULT = 1.0
         ENDIF
!
! 112706. ADDED ABS() FOR AQUILA.
!
         L_CLASS = ABS(GET_EMISSION_MARKET_LINK(R_UNIT))
         IF(EMPIRE_IS_ACTIVE .AND. L_CLASS+1 < 1) THEN
            TABLE = 1
         ELSEIF(MAX_TABLE_NUM == 0 .OR. (L_CLASS+1 < 1 .AND.   &
                                          L_CLASS < MAX_CLASS_NUM)) THEN
            TABLE = 0
         ELSEIF(.NOT. DEFINED_CLASS(L_CLASS)) THEN
            TABLE = 0
         ELSE
            TABLE = ENVIR_CLASS_2_TABLE(L_CLASS+1)
         ENDIF
         TRANS_DISPATCH_EMIS_ADDER =   &
             MONTH_EMIS_DISPATCH_ADDER(R_INT4_EMIS_TYPE,TABLE,R_MONTH) *   &
                                                          SCEN_EMIS_MULT
      RETURN
!
    
      ENTRY TRANS_MRX_DISPATCH_EMIS_ADDER(R_INT4_EMIS_TYPE,R_MONTH,   &
                                                         R_YEAR,R_CLASS)
    
         TRANS_MRX_DISPATCH_EMIS_ADDER = 0.
         IF(.NOT. ALLOCATED(MONTH_EMIS_DISPATCH_ADDER)) RETURN
         IF(R_INT4_EMIS_TYPE == 1) THEN
            SCEN_EMIS_MULT = GET_SCENARIO_SO2_PRICES(R_YEAR,R_MONTH)
         ELSEIF(R_INT4_EMIS_TYPE == 2) THEN
            SCEN_EMIS_MULT = GET_SCENARIO_NOX_PRICES(R_YEAR,R_MONTH)
         ELSEIF(R_INT4_EMIS_TYPE == 3) THEN
            SCEN_EMIS_MULT = GET_SCENARIO_CO2_PRICES(R_YEAR,R_MONTH)
         ELSEIF(R_INT4_EMIS_TYPE == 4) THEN
            SCEN_EMIS_MULT = GET_SCENARIO_HG_PRICES(R_YEAR,R_MONTH)
         ELSEIF(R_INT4_EMIS_TYPE == 5) THEN
            SCEN_EMIS_MULT = GET_SCENARIO_ANN_NOX_PRICES(R_YEAR,R_MONTH)
         ELSE
            SCEN_EMIS_MULT = 1.0
         ENDIF
!
! 112706. ADDED ABS() FOR AQUILA.
         L_CLASS = ABS(R_CLASS)
!
         IF(EMPIRE_IS_ACTIVE .AND. L_CLASS+1 < 1) THEN
            TABLE = 1
         ELSEIF(MAX_TABLE_NUM == 0 .OR. (L_CLASS+1 < 1 .AND.   &
                                          L_CLASS < MAX_CLASS_NUM)) THEN
            TABLE = 0
         ELSEIF(.NOT. DEFINED_CLASS(L_CLASS)) THEN
            TABLE = 0
         ELSE
            TABLE = ENVIR_CLASS_2_TABLE(L_CLASS+1)
         ENDIF
         TRANS_MRX_DISPATCH_EMIS_ADDER =   &
             MONTH_EMIS_DISPATCH_ADDER(R_INT4_EMIS_TYPE,TABLE,R_MONTH) *   &
                                                          SCEN_EMIS_MULT
      RETURN

! 01/31/06. USED TO WRITE THE ANNUAL SBU EMISSIONS REPORT
    
      ENTRY PUT_TRANS_MRX_DISP_EMIS_ADDER(R_INT4_EMIS_TYPE,R_MONTH,   &
                                          R_YEAR,R_CLASS,R_PRICE)
    
!
         PUT_TRANS_MRX_DISP_EMIS_ADDER = .FALSE.
         IF(R_PRICE <= -999.) RETURN
!
         IF(R_INT4_EMIS_TYPE == 1) THEN
            SCEN_EMIS_MULT = GET_SCENARIO_SO2_PRICES(R_YEAR,R_MONTH)
         ELSEIF(R_INT4_EMIS_TYPE == 2) THEN
            SCEN_EMIS_MULT = GET_SCENARIO_NOX_PRICES(R_YEAR,R_MONTH)
         ELSEIF(R_INT4_EMIS_TYPE == 3) THEN
            SCEN_EMIS_MULT = GET_SCENARIO_CO2_PRICES(R_YEAR,R_MONTH)
         ELSEIF(R_INT4_EMIS_TYPE == 4) THEN
            SCEN_EMIS_MULT = GET_SCENARIO_HG_PRICES(R_YEAR,R_MONTH)
         ELSEIF(R_INT4_EMIS_TYPE == 5) THEN
            SCEN_EMIS_MULT = GET_SCENARIO_ANN_NOX_PRICES(R_YEAR,R_MONTH)
         ELSE
            SCEN_EMIS_MULT = 1.0
         ENDIF
!
! 112706. ADDED ABS() FOR AQUILA.
         L_CLASS = ABS(R_CLASS)
!
         IF(EMPIRE_IS_ACTIVE .AND. L_CLASS+1 < 1) THEN
            TABLE = 1
         ELSEIF(MAX_TABLE_NUM == 0 .OR. (L_CLASS+1 < 1 .AND.   &
                                          L_CLASS < MAX_CLASS_NUM)) THEN
            TABLE = 0
         ELSEIF(.NOT. DEFINED_CLASS(L_CLASS)) THEN
            TABLE = 0
         ELSE
            TABLE = ENVIR_CLASS_2_TABLE(L_CLASS+1)
         ENDIF
         MONTH_EMIS_DISPATCH_ADDER(R_INT4_EMIS_TYPE,TABLE,R_MONTH) =   &
                                                                 R_PRICE
         PUT_TRANS_MRX_DISP_EMIS_ADDER = .TRUE.
      RETURN

! 01/31/06. USED TO WRITE THE ANNUAL SBU EMISSIONS REPORT
    
      ENTRY PUT_GRX_DISP_EMIS_ADDER(R_INT4_EMIS_TYPE,   &
                                    R_CLASS,   &
                                    R_PRICE)
    
!
! FOR SYSTEM CO2:
!    R_INT4_EMIS_TYPE = 3
!    R_MONTH = 1, 12
!    R_YEAR = NEXT YEAR OF SIMULATION
!    R_CLASS = 0 FOR SYSTEM
!    R_PRICE = MODEL DERIVED
!
         PUT_GRX_DISP_EMIS_ADDER = .FALSE.
         IF(.NOT. ALLOCATED(MONTH_EMIS_DISPATCH_ADDER)) RETURN
         IF(R_PRICE <= -999.) RETURN
!
! 112706. ADDED ABS() FOR AQUILA.
         L_CLASS = ABS(R_CLASS)
!
         IF(EMPIRE_IS_ACTIVE .AND. L_CLASS+1 < 1) THEN
            TABLE = 1
         ELSEIF(MAX_TABLE_NUM == 0 .OR. (L_CLASS+1 < 1 .AND.   &
                                          L_CLASS < MAX_CLASS_NUM)) THEN
            TABLE = 0
         ELSEIF(.NOT. DEFINED_CLASS(L_CLASS)) THEN
            TABLE = 0
         ELSE
            TABLE = ENVIR_CLASS_2_TABLE(L_CLASS+1)
         ENDIF
         MONTH_EMIS_DISPATCH_ADDER(R_INT4_EMIS_TYPE,0,0:12) = R_PRICE
         IF(POKE_CURRENT_CO2_DISPATCH_COST(globecom_YEAR+get_BASE_YEAR())) THEN
            MONTH_EMIS_DISPATCH_ADDER(R_INT4_EMIS_TYPE,   &
                                         1:MAX_TABLE_NUM,0:12) = R_PRICE
         ELSE
            DO TABLE = 1, MAX_TABLE_NUM
               IF(ACCEPT_CO2_PUSH(TABLE))   &
                MONTH_EMIS_DISPATCH_ADDER(R_INT4_EMIS_TYPE,TABLE,0:12) =   &
                                                                 R_PRICE
            END DO
         ENDIF
         PUT_GRX_DISP_EMIS_ADDER = .TRUE.
      RETURN

    
      ENTRY PUT_CoalLP_SO2_DISP_EMIS_ADDER(R_INT4_EMIS_TYPE,   &
                                           R_CLASS,   &
                                           R_PRICE)
    
!
! FOR SYSTEM CO2:
!    R_INT4_EMIS_TYPE = 3
!    R_MONTH = 1, 12
!    R_YEAR = NEXT YEAR OF SIMULATION
!    R_CLASS = 0 FOR SYSTEM
!    R_PRICE = MODEL DERIVED
!
         PUT_CoalLP_SO2_DISP_EMIS_ADDER = .FALSE.
         IF(.NOT. ALLOCATED(MONTH_EMIS_DISPATCH_ADDER)) RETURN
         IF(R_PRICE <= -999.) RETURN
!
! 112706. ADDED ABS() FOR AQUILA.
         L_CLASS = ABS(R_CLASS)
!
         IF(EMPIRE_IS_ACTIVE .AND. L_CLASS+1 < 1) THEN
            TABLE = 1
         ELSEIF(MAX_TABLE_NUM == 0 .OR. (L_CLASS+1 < 1 .AND.   &
                                          L_CLASS < MAX_CLASS_NUM)) THEN
            TABLE = -1
         ELSEIF(.NOT. DEFINED_CLASS(L_CLASS)) THEN
            TABLE = -1
         ELSE
            TABLE = ENVIR_CLASS_2_TABLE(L_CLASS+1)
         ENDIF
         IF(TABLE >= 0) THEN
            IF(ACCEPT_SO2_PUSH(TABLE)) THEN
               MONTH_EMIS_DISPATCH_ADDER(R_INT4_EMIS_TYPE,TABLE,0:12) =   &
                                                                 R_PRICE
               PUT_CoalLP_SO2_DISP_EMIS_ADDER = .TRUE.
            ELSE
               R_PRICE =   &
                     MONTH_EMIS_DISPATCH_ADDER(R_INT4_EMIS_TYPE,TABLE,1)
            ENDIF
         ELSE
            R_PRICE = 0.
         ENDIF
      RETURN

! 01/31/06. USED TO WRITE THE ANNUAL SBU EMISSIONS REPORT
    
      ENTRY EMISS_PURCH_CRED_PRICE(R_EMIS_TYPE)
    
         IF(MAX_TABLE_NUM == 0) THEN
            EMISS_PURCH_CRED_PRICE = 0.
         ELSEIF(EMIS_PURCH(R_EMIS_TYPE) /= 0.) THEN
            EMISS_PURCH_CRED_PRICE = EMIS_EXPENSE(R_EMIS_TYPE)/   &
                                               EMIS_PURCH(R_EMIS_TYPE)
         ELSEIF(ACTIVE_EMISSION_TABLES == 0.) THEN
            EMISS_PURCH_CRED_PRICE =   &
                                 MONTH_PURCH_CRED_PRICE(R_EMIS_TYPE,1,0)
         ELSE
            EMISS_PURCH_CRED_PRICE = AVE_PURCH_CRED_PRICE(R_EMIS_TYPE)/   &
                                                ACTIVE_EMISSION_TABLES
         ENDIF
      RETURN

    
      ENTRY EMISS_SELL_CRED_PRICE(R_EMIS_TYPE)
    
         IF(MAX_TABLE_NUM == 0) THEN
            EMISS_SELL_CRED_PRICE = 0.
         ELSEIF(EMIS_SELL(R_EMIS_TYPE) /= 0.) THEN
            EMISS_SELL_CRED_PRICE = EMIS_REVENUE(R_EMIS_TYPE)/   &
                                                EMIS_SELL(R_EMIS_TYPE)
         ELSEIF(ACTIVE_EMISSION_TABLES == 0.) THEN
            EMISS_SELL_CRED_PRICE =   &
                                  MONTH_SELL_CRED_PRICE(R_EMIS_TYPE,1,0)

         ELSE
            EMISS_SELL_CRED_PRICE = AVE_SELL_CRED_PRICE(R_EMIS_TYPE)/   &
                                                ACTIVE_EMISSION_TABLES
         ENDIF
      RETURN

    
      ENTRY ZERO_TOTAL_EMIS_VARS
    
         CUM_ANN_EMIS = 0.
         EMIS_PURCH = 0.
         EMIS_SELL = 0.
         EMIS_EXPENSE = 0.
         EMIS_REVENUE = 0.
         NET_ANN_EMIS = 0.
         EMIS_CAP = 0.
         ANNUAL_EMIS = 0.
         ZERO_TOTAL_EMIS_VARS = .TRUE.
      RETURN

    
      ENTRY CLASS_EMISSIONS_CREDITS(R_CLASS,R_CLASS_INC_EMIS_EXPENSE,   &
                                    R_CLASS_INC_EMIS_REVENUE,   &
                                    R_VARIABLES)
    
!
      CLASS_EMISSIONS_CREDITS = .TRUE.
      CLASS = R_CLASS
      CLASS_INC_EMIS_EXPENSE = 0.
      CLASS_INC_EMIS_REVENUE = 0.
      CALL GET_EL_SO2_EMISSIONS(CLASS,CLASS_EL_SO2_ANNUAL)
      CLASS_CT_SO2_ANNUAL = 0.
      CLASS_CL_SO2_ANNUAL = GET_CL_EMISSIONS(CLASS,CLASS_CL_EMISSIONS)
      IF(CLASS > MAX_CLASS_NUM) THEN
         TABLE = 0
      ELSE
         TABLE = ENVIR_CLASS_2_TABLE(CLASS)
      ENDIF
      USE_TABLE = .FALSE.
      IF(TABLE == 0) THEN
!
         DO EMIS_TYPE = 1, NUMBER_OF_EMISSION_TYPES
            IF(CLASS_CL_EMISSIONS(EMIS_TYPE) /= 0.) THEN
               TABLE = 1
               USE_TABLE = .TRUE.
               EXIT
            ENDIF
         ENDDO
         IF(.NOT. USE_TABLE .OR. MAX_TABLE_NUM == 0) THEN
            TABLE = 0
            R_CLASS_INC_EMIS_EXPENSE = 0.
            R_CLASS_INC_EMIS_REVENUE = 0.
            CLASS_EPA_SETASIDE_CREDITS = 0.
            CLASS_EPA_SETASIDE_REVENUE = 0.
            CLASS_INC_EMIS_EXPENSE = 0.
            CLASS_INC_EMIS_REVENUE = 0.
            CLASS_INC_NET_EMIS_EXPENSE = 0.
            DO EMIS_TYPE = 1, NUMBER_OF_EMISSION_TYPES
               CLASS_EMIS_PURCH(EMIS_TYPE) = 0.
               CLASS_EMIS_SELL(EMIS_TYPE) = 0.
               CLASS_EMIS_EXPENSE(EMIS_TYPE) = 0.
               CLASS_EMIS_REVENUE(EMIS_TYPE) = 0.
               CLASS_NET_ANN_EMIS(EMIS_TYPE) = 0.
               CLASS_EMISSION_CAP(EMIS_TYPE) = 0.
               RPT_CUM_ANN_EMIS(EMIS_TYPE) = 0.
               IF(EMIS_TYPE == SO2) THEN
                  CLASS_EMISSIONS(SO2) = CLASS_CL_SO2_ANNUAL   &
                                         + CLASS_EL_SO2_ANNUAL   &
                                         + CLASS_CT_SO2_ANNUAL
               ELSE
                  CLASS_EMISSIONS(EMIS_TYPE) =   &
                                           CLASS_CL_EMISSIONS(EMIS_TYPE)
               ENDIF
               ANNUAL_EMIS(EMIS_TYPE) = ANNUAL_EMIS(EMIS_TYPE)   &
                                        + CLASS_EMISSIONS(EMIS_TYPE)
            ENDDO
!
            RETURN
         ENDIF
      ENDIF
      CLASS_EPA_SETASIDE_CREDITS = 0.
      CLASS_EPA_SETASIDE_REVENUE = 0.
!
! CALCULATE EPA SETASIDE CREDITS AND REVENUES FOR SO2
! 4/21/99. GAT. TESTING FOR JIM LAMBETH AT DUKE.
!
      IF(.NOT. USE_TABLE) THEN
         IF(CLASS_EPA_SETASIDE_RATE(TABLE) >= 0.) THEN
            CLASS_EMISSION_CAP(SO2) = CLASS_EMIS_CAP(SO2,TABLE)   &
                                      + CLASS_EPA_EAS(TABLE)
         ELSE
            CLASS_EMISSION_CAP(SO2) = (CLASS_EMIS_CAP(SO2,TABLE)   &
                                       + CLASS_EPA_EAS(TABLE))/   &
                         (1. - ABS(CLASS_EPA_SETASIDE_RATE(TABLE))/100.)
         ENDIF
         CLASS_EPA_SETASIDE_CREDITS = CLASS_EMISSION_CAP(SO2) *   &
                                ABS(CLASS_EPA_SETASIDE_RATE(TABLE))/100.
         CLASS_EMISSION_CAP(SO2) = CLASS_EMISSION_CAP(SO2)   &
                                   - CLASS_EPA_SETASIDE_CREDITS
      ENDIF
      CLASS_EPA_SETASIDE_REVENUE = CLASS_EPA_SETASIDE_REVENUE   &
                               + MONTH_SELL_CRED_PRICE(SO2,TABLE,0)
      EPA_SETASIDE_CREDITS = EPA_SETASIDE_CREDITS   &
                             + CLASS_EPA_SETASIDE_CREDITS
      EPA_SETASIDE_REVENUE = EPA_SETASIDE_REVENUE   &
                             + CLASS_EPA_SETASIDE_REVENUE
!
! ADD EL SO2 TO SO2 EMISSION TYPE 1
!
      TOTAL_EL_SO2_ANNUAL = TOTAL_EL_SO2_ANNUAL + CLASS_EL_SO2_ANNUAL
      CLASS_EMISSIONS(SO2) = CLASS_CL_SO2_ANNUAL   &
                             + CLASS_EL_SO2_ANNUAL   &
                             + CLASS_CT_SO2_ANNUAL
!
! CALCULATE ANNUAL SURPLUS / DEFICIT
!
      DO EMIS_TYPE = 1, NUMBER_OF_EMISSION_TYPES
!
!
         ITEM = 10*(EMIS_TYPE-1)
!
! ADDED 8/2/00. GAT/TS.
!
         IF(EMIS_TYPE == 1) THEN
            SCEN_EMIS_MULT = GET_SCENARIO_SO2_PRICES(globecom_YEAR,INT(7,2))
         ELSEIF(EMIS_TYPE == 2) THEN
            SCEN_EMIS_MULT = GET_SCENARIO_NOX_PRICES(globecom_YEAR,INT(7,2))
         ELSEIF(EMIS_TYPE == 3) THEN
            SCEN_EMIS_MULT = GET_SCENARIO_CO2_PRICES(globecom_YEAR,INT(7,2))
         ELSEIF(EMIS_TYPE == 4) THEN
            SCEN_EMIS_MULT = GET_SCENARIO_HG_PRICES(globecom_YEAR,INT(7,2))
         ELSEIF(EMIS_TYPE == 5) THEN
            SCEN_EMIS_MULT = GET_SCENARIO_ANN_NOX_PRICES(globecom_YEAR,INT(7,2))
         ELSE
            SCEN_EMIS_MULT = 1.0
         ENDIF
!
         CLASS_EMIS_PURCH(EMIS_TYPE) = 0.
         CLASS_EMIS_EXPENSE(EMIS_TYPE) = 0.
         CLASS_NET_ANN_EMIS(EMIS_TYPE) = 0.
         IF(EMIS_TYPE == SO2) THEN
            CLASS_EMIS_SELL(EMIS_TYPE) = CLASS_EPA_SETASIDE_CREDITS
            CLASS_EMIS_REVENUE(EMIS_TYPE) = CLASS_EPA_SETASIDE_REVENUE
         ELSE
            CLASS_EMIS_SELL(EMIS_TYPE) = 0.
            CLASS_EMIS_REVENUE(EMIS_TYPE) = 0.
            CLASS_EMISSION_CAP(EMIS_TYPE) = 0.
            IF(.NOT. USE_TABLE) THEN
               CLASS_EMISSION_CAP(EMIS_TYPE) =   &
                                         CLASS_EMIS_CAP(EMIS_TYPE,TABLE)
            ENDIF
            CLASS_EMISSIONS(EMIS_TYPE) = CLASS_CL_EMISSIONS(EMIS_TYPE)
         ENDIF
!
! EMISSIONS ROLL-UP. EMISSIONS CAPS DON'T ROLL-UP.
!
         CLASS_NET_ANN_EMIS(EMIS_TYPE) =   &
                        CLASS_EMISSION_CAP(EMIS_TYPE)   &
                        - R_VARIABLES(159+ITEM)   &
                        - CLASS_EMISSIONS(EMIS_TYPE)
         CLASS_CUM_ANN_EMIS(EMIS_TYPE,CLASS) =   &
                                     CLASS_CUM_ANN_EMIS(EMIS_TYPE,CLASS)   &
                                     + CLASS_NET_ANN_EMIS(EMIS_TYPE)
         NET_ANN_EMIS(EMIS_TYPE) = NET_ANN_EMIS(EMIS_TYPE)   &
                                   + CLASS_NET_ANN_EMIS(EMIS_TYPE)
!
         IF(.NOT. USE_TABLE) THEN
         IF(CLASS_EMIS_STRAT(EMIS_TYPE,TABLE) == 'S') THEN
!
! THIS ALLOWS FOR THE POSSIBILITY OF SWITCHING FROM
! EITHER BANKING OR NOTHING TO SELLING.
!
            IF(CLASS_CUM_ANN_EMIS(EMIS_TYPE,CLASS) >= 0.) THEN
               CLASS_EMIS_SELL(EMIS_TYPE) =   &
                                     CLASS_CUM_ANN_EMIS(EMIS_TYPE,CLASS)
               CLASS_EMIS_REVENUE(EMIS_TYPE)=CLASS_EMIS_SELL(EMIS_TYPE)*   &
                                          SCEN_EMIS_MULT*   &
                                MONTH_SELL_CRED_PRICE(EMIS_TYPE,TABLE,0)
            ELSE
               CLASS_EMIS_PURCH(EMIS_TYPE) =   &
                                    -CLASS_CUM_ANN_EMIS(EMIS_TYPE,CLASS)
               CLASS_EMIS_EXPENSE(EMIS_TYPE) =   &
                              MONTH_PURCH_CRED_PRICE(EMIS_TYPE,TABLE,0)*   &
                                          SCEN_EMIS_MULT*   &
                                             CLASS_EMIS_PURCH(EMIS_TYPE)
            ENDIF
            CLASS_CUM_ANN_EMIS(EMIS_TYPE,CLASS) = 0.
         ELSEIF(CLASS_EMIS_STRAT(EMIS_TYPE,TABLE) == 'B') THEN
! BANKING OPTION:
            IF(CLASS_CUM_ANN_EMIS(EMIS_TYPE,CLASS) <   &
                                   CLASS_MIN_BANK(EMIS_TYPE,TABLE)) THEN
! BUY EMISSIONS TO MEET MINIMUM.
               CLASS_EMIS_PURCH(EMIS_TYPE) =   &
                                   (CLASS_MIN_BANK(EMIS_TYPE,TABLE)-   &
                                    CLASS_CUM_ANN_EMIS(EMIS_TYPE,CLASS))
               CLASS_EMIS_EXPENSE(EMIS_TYPE) =   &
                              MONTH_PURCH_CRED_PRICE(EMIS_TYPE,TABLE,0)*   &
                                          SCEN_EMIS_MULT*   &
                                             CLASS_EMIS_PURCH(EMIS_TYPE)
               CLASS_CUM_ANN_EMIS(EMIS_TYPE,CLASS) =   &
                                         CLASS_MIN_BANK(EMIS_TYPE,TABLE)
            ENDIF
            IF(CLASS_CUM_ANN_EMIS(EMIS_TYPE,CLASS) >   &
                                   CLASS_MAX_BANK(EMIS_TYPE,TABLE)) THEN
! SELL EMISSIONS TO MEET MAXIMUM.
               CLASS_EMIS_SELL(EMIS_TYPE) =   &
                                   CLASS_CUM_ANN_EMIS(EMIS_TYPE,CLASS) -   &
                                        CLASS_MAX_BANK(EMIS_TYPE,TABLE)
               CLASS_EMIS_REVENUE(EMIS_TYPE) =   &
                              MONTH_SELL_CRED_PRICE(EMIS_TYPE,TABLE,0) *   &
                                          SCEN_EMIS_MULT*   &
                                              CLASS_EMIS_SELL(EMIS_TYPE)
               CLASS_CUM_ANN_EMIS(EMIS_TYPE,CLASS) =   &
                                         CLASS_MAX_BANK(EMIS_TYPE,TABLE)
            ENDIF
         ENDIF
         ENDIF ! .NOT. USE_TABLE
!
!
         AVE_PURCH_CRED_PRICE(EMIS_TYPE) =   &
                             AVE_PURCH_CRED_PRICE(EMIS_TYPE)   &
                             + SCEN_EMIS_MULT *   &
                               MONTH_PURCH_CRED_PRICE(EMIS_TYPE,TABLE,0)
         AVE_SELL_CRED_PRICE(EMIS_TYPE) =   &
                             AVE_SELL_CRED_PRICE(EMIS_TYPE)   &
                             + SCEN_EMIS_MULT *   &
                                MONTH_SELL_CRED_PRICE(EMIS_TYPE,TABLE,0)
         ANNUAL_EMIS(EMIS_TYPE) = ANNUAL_EMIS(EMIS_TYPE)   &
                                  + CLASS_EMISSIONS(EMIS_TYPE)
         EMIS_PURCH(EMIS_TYPE) = EMIS_PURCH(EMIS_TYPE)   &
                                 + CLASS_EMIS_PURCH(EMIS_TYPE)
         EMIS_SELL(EMIS_TYPE) = EMIS_SELL(EMIS_TYPE)   &
                                + CLASS_EMIS_SELL(EMIS_TYPE)
         EMIS_EXPENSE(EMIS_TYPE) = EMIS_EXPENSE(EMIS_TYPE)   &
                                   + CLASS_EMIS_EXPENSE(EMIS_TYPE)
         EMIS_REVENUE(EMIS_TYPE) = EMIS_REVENUE(EMIS_TYPE)   &
                                   + CLASS_EMIS_REVENUE(EMIS_TYPE)
         EMIS_CAP(EMIS_TYPE) = EMIS_CAP(EMIS_TYPE)   &
                               + CLASS_EMISSION_CAP(EMIS_TYPE)
         CUM_ANN_EMIS(EMIS_TYPE) = CUM_ANN_EMIS(EMIS_TYPE)   &
                                   + CLASS_CUM_ANN_EMIS(EMIS_TYPE,CLASS)
         CLASS_INC_EMIS_EXPENSE = CLASS_INC_EMIS_EXPENSE   &
                                  + CLASS_EMIS_EXPENSE(EMIS_TYPE)
         CLASS_INC_EMIS_REVENUE = CLASS_INC_EMIS_REVENUE   &
                                  + CLASS_EMIS_REVENUE(EMIS_TYPE)
         RPT_CUM_ANN_EMIS(EMIS_TYPE)=CLASS_CUM_ANN_EMIS(EMIS_TYPE,CLASS)
!
      ENDDO
      ACTIVE_EMISSION_TABLES = ACTIVE_EMISSION_TABLES + 1.
      R_CLASS_INC_EMIS_EXPENSE = CLASS_INC_EMIS_EXPENSE/1000000.
      R_CLASS_INC_EMIS_REVENUE = CLASS_INC_EMIS_REVENUE/1000000.
      CLASS_INC_NET_EMIS_EXPENSE = (CLASS_INC_EMIS_EXPENSE -   &
                                        CLASS_INC_EMIS_REVENUE)/1000000.
      INC_EMIS_EXPENSE = INC_EMIS_EXPENSE + CLASS_INC_EMIS_EXPENSE
      INC_EMIS_REVENUE = INC_EMIS_REVENUE + CLASS_INC_EMIS_REVENUE
      RETURN

    
      ENTRY REPORT_CLASS_EMISSIONS_INFO(R_VARIABLES)
    
! 083006. TEMP !!! NOT DEFINED !!!
         TABLE = 1
!
         R_VARIABLES(155) = R_VARIABLES(155)+CLASS_EL_SO2_ANNUAL
         R_VARIABLES(156) = R_VARIABLES(156)+CLASS_CT_SO2_ANNUAL
         R_VARIABLES(157) = R_VARIABLES(157)+CLASS_CL_EMISSIONS(SO2)
         R_VARIABLES(158) = R_VARIABLES(158)+CLASS_INC_NET_EMIS_EXPENSE
         DO EMIS_TYPE = 1, NUMBER_OF_EMISSION_TYPES
            ITEM = 10*(EMIS_TYPE-1)
!
! CREDIT PURCHASE PRICE DIRECT OR AVERAGE
!
            R_VARIABLES(162+ITEM) = R_VARIABLES(162+ITEM) +   &
                                             CLASS_EMIS_PURCH(EMIS_TYPE)
            R_VARIABLES(164+ITEM) = R_VARIABLES(164+ITEM) +   &
                                  CLASS_EMIS_EXPENSE(EMIS_TYPE)/1000000.
            IF(R_VARIABLES(162+ITEM) /= 0.) THEN
               R_VARIABLES(163+ITEM) = 10.**6 * R_VARIABLES(164+ITEM)/   &
                                           R_VARIABLES(162+ITEM)
            ELSE
               IF(TABLE == 0) THEN
                  R_VARIABLES(163+ITEM) = 0. ! PURCHASE PRICE
               ELSE
                  R_VARIABLES(163+ITEM) =   &
                               MONTH_PURCH_CRED_PRICE(EMIS_TYPE,TABLE,0)
               ENDIF
            ENDIF
!
! CREDIT SELLING PRICE DIRECT OR AVERAGE
!
            R_VARIABLES(165+ITEM) = R_VARIABLES(165+ITEM) +   &
                                              CLASS_EMIS_SELL(EMIS_TYPE)
            R_VARIABLES(167+ITEM) = R_VARIABLES(167+ITEM) +   &
                                  CLASS_EMIS_REVENUE(EMIS_TYPE)/1000000.
            IF(R_VARIABLES(165+ITEM) /= 0.) THEN
               R_VARIABLES(166+ITEM) = 10.**6 * R_VARIABLES(167+ITEM)/   &
                                           R_VARIABLES(165+ITEM)
            ELSE
               IF(TABLE == 0) THEN
                  R_VARIABLES(166+ITEM) = 0. ! SELL PRICE
               ELSE
                  R_VARIABLES(166+ITEM) =   &
                                MONTH_SELL_CRED_PRICE(EMIS_TYPE,TABLE,0)
               ENDIF
            ENDIF
!
            R_VARIABLES(159+ITEM) = R_VARIABLES(159+ITEM) +   &
                                              CLASS_EMISSIONS(EMIS_TYPE)
!
            IF(USE_TABLE) THEN
               R_VARIABLES(160+ITEM) = R_VARIABLES(160+ITEM) +   &
                                           CLASS_EMISSION_CAP(EMIS_TYPE)
               R_VARIABLES(161+ITEM) = R_VARIABLES(161+ITEM) +   &
                                           CLASS_NET_ANN_EMIS(EMIS_TYPE)
               R_VARIABLES(168+ITEM) = R_VARIABLES(168+ITEM) +   &
                                             RPT_CUM_ANN_EMIS(EMIS_TYPE)
            ELSE
               R_VARIABLES(160+ITEM) = CLASS_EMISSION_CAP(EMIS_TYPE)
               R_VARIABLES(161+ITEM) = R_VARIABLES(160+ITEM) -   &
                                                   R_VARIABLES(159+ITEM)
               R_VARIABLES(168+ITEM) = RPT_CUM_ANN_EMIS(EMIS_TYPE)
            ENDIF
         ENDDO
         REPORT_CLASS_EMISSIONS_INFO = .TRUE.
      RETURN

    
      ENTRY EMISSIONS_CREDITS
    
         EMISSIONS_CREDITS = .FALSE.
      RETURN
      END

    
      SUBROUTINE CAL_SUMMED_VALUES(VARIABLE)
    
!
      use SpinDriftLib
      use prod_arrays_dimensions
      USE SIZECOM

      INTEGER (kind=2) ::  EMIS_TYPE,ITEM
      REAL ::  VARIABLE(0:*)
      REAL ::  EMISS_PURCH_CRED_PRICE,EMISS_SELL_CRED_PRICE
      REAL ::  NOT_AVAIL
      REAL (kind=4) ::  EFFECTIVE_TAX_RATE
      PARAMETER(NOT_AVAIL=-999999.)
!
         DO EMIS_TYPE = 1, NUMBER_OF_EMISSION_TYPES
            ITEM = 10*(EMIS_TYPE-1)
            IF(VARIABLE(162+ITEM) /= 0.) THEN
               VARIABLE(163+ITEM) = (1000000.*VARIABLE(164+ITEM))/  & !  EMIS_EXPENSE
                                       VARIABLE(162+ITEM) ! EMIS_PURCH
            ELSE
               VARIABLE(163+ITEM) = EMISS_PURCH_CRED_PRICE(EMIS_TYPE) ! PURCHASE PRICE
            ENDIF
            IF(VARIABLE(165+ITEM) /= 0.) THEN
               VARIABLE(166+ITEM) = (1000000.*VARIABLE(167+ITEM))/ & !   EMIS_REVENUE
                                       VARIABLE(165+ITEM) ! EMIS_SELL
            ELSE
               VARIABLE(166+ITEM) = EMISS_SELL_CRED_PRICE(EMIS_TYPE)
            ENDIF
         ENDDO
!
! ANNUALIZED COST VALUES
!
      ENTRY CAL_ANNUALIZED_CAPTIAL_COSTS(VARIABLE)
         VARIABLE(223) = 0. ! EMBED_COST_CAPITAL_W_STD
         VARIABLE(224) = 0. ! EMBED_COST_CAPITAL_WO_STD
         EFFECTIVE_TAX_RATE = (VARIABLE(213)/100. *   &
                                (100.-VARIABLE(141))+VARIABLE(141))/100.
         IF(VARIABLE(82)+VARIABLE(84) /= 0.) THEN
            VARIABLE(223) = 100.*(VARIABLE(226) +  & !  ANNUALIZED_EQUITY_COST
                                  VARIABLE(221) +  & !  ANNUALIZED_PS_DIVIDENDS
                                  VARIABLE(222) +  & !  ANNUALIZED_LTD_INTEREST
                                  VARIABLE(225))/  & !  ANNUALIZED_STD_INTEREST
                                  (VARIABLE(82)+VARIABLE(84)) ! TOTAL_CAPITIAL_BAL + STD BALANCE
            VARIABLE(396) = 100.*(VARIABLE(226) +   & !  ANNUALIZED_EQUITY_COST
                                  VARIABLE(221) +   & !  ANNUALIZED_PS_DIVIDENDS
              (1.-EFFECTIVE_TAX_RATE)*(VARIABLE(222) +  & !  ANNUALIZED_LTD_INTEREST
                                  VARIABLE(225)))/  & !  ANNUALIZED_STD_INTEREST
                                  (VARIABLE(82)+VARIABLE(84)) ! TOTAL_CAPITIAL_BAL + STD BALANCE
         ENDIF
         IF(VARIABLE(82) /= 0.) THEN
            VARIABLE(224) = 100.*(VARIABLE(226) +  & !  ANNUALIZED_EQUITY_COST
                                  VARIABLE(221) +  & !  ANNUALIZED_PS_DIVIDENDS
                                  VARIABLE(222))/  & !  ANNUALIZED_LTD_INTEREST
                                    VARIABLE(82)   ! TOTAL_CAPITIAL_BAL
            VARIABLE(397) = 100.*(VARIABLE(226) +  & !  ANNUALIZED_EQUITY_COST
                                  VARIABLE(221) +  & !  ANNUALIZED_PS_DIVIDENDS
              (1.-EFFECTIVE_TAX_RATE)*VARIABLE(222))/  & !  ANNUALIZED_LTD_INTEREST
                                    VARIABLE(82)   ! TOTAL_CAPITIAL_BAL
         ENDIF
      RETURN
      END

    
      SUBROUTINE STOP_NO_BASE_FILE(FILE_TYPE)
      use end_routine, only: end_program, er_message
    
!
         CHARACTER (len=14) ::  FILE_TYPE
!
         WRITE(4,*) 'The base file for ',trim(FILE_TYPE),   &
                                                        ' was not found'
         WRITE(4,*) 'for one or more overlay files. Run check'
         WRITE(4,*) 'overlays to determine overlay names.'
         er_message='See WARNING MESSAGES -EN_OBJT.FOR-2'
         call end_program(er_message)
      END
!
!
