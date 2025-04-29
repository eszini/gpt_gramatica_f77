
!     ESRN_OBJ.FOR
!     Created: 11/11/02 10:23:26 AM


      SUBROUTINE ES_OBJECT
      use end_routine, only: end_program, er_message
      use miscmod
      use SpinDriftLib
      USE PROD_ARRAYS_DIMENSIONS
      USE ESCALATION_VARIABLES
      USE SIZECOM
      SAVE
      INTEGER (KIND=2) :: VECTOR,RUN_YEAR,R_RUN_YEAR
      LOGICAL (KIND=1) :: WEPCO_ESCAL=.FALSE.
      CHARACTER (LEN=16) :: FILE_TYPE='Escalation Rates'
      INTEGER (KIND=2) :: I,J,INUNIT,IREC,DELETE
      INTEGER :: IOS
      CHARACTER (LEN=5) :: ESCALFL,OVERLAY_FAMILY_NAME
      CHARACTER (LEN=50) :: COMMENT
      CHARACTER (LEN=256) :: FILE_NAME,FILE_NAME2
      CHARACTER (LEN=256) :: BASE_FILE_DIRECTORY
      LOGICAL (KIND=4) :: FILEXIST
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER (LEN=1024) :: RECLN
      CHARACTER (LEN=256) :: OUTPUT_DIRECTORY
! DECLARATION FOR ESCALATION RATES FILE
      INTEGER (KIND=2) :: YEAR
!
!
      INTEGER (KIND=2) :: BC_REC,UNIT_NO,VECTOR_NO,VECTOR_POS_HOLDER, &
      MIN_VECTOR_NUM=9999
      CHARACTER (LEN=2) :: VECTOR_TYPE
      INTEGER (KIND=2) :: GROWTH_RATE_YEAR
      INTEGER (KIND=2) :: LAST_INPUT_RECORD=0
      CHARACTER (LEN=1) :: UTILITY_TYPE
      CHARACTER (LEN=4) :: ITER_CHR
! DECLARATION FOR GENERATING UNIT PARAMETERS
      REAL :: VALUES_30(0:AVAIL_DATA_YEARS)
      CHARACTER (LEN=40) :: DESCRIPTION
      INTEGER (KIND=2) :: START_MONTH,END_MONTH
      INTEGER (KIND=2) :: TABLE_NO,TABLE_POS
      CHARACTER (LEN=30) :: SCREEN_OUTPUT
!
! VARIABLE ADDITIONS AND CHANGES 01/17/07 DR.G
!
      INTEGER (KIND=2) :: FILE_NO
      INTEGER (KIND=4) :: R_FILE_NO
      CHARACTER (LEN=5) :: ESC_FILE_BASE_NAMES(0:MAX_ESC_FILES-1),VOID_CHR
!
! FOR N5 LNG DELIVERIES
!
      LOGICAL (KIND=1) :: ITERATION_ON
      INTEGER (KIND=2) :: TEMP_I2,GET_ITERATION_NUMBER
      CHARACTER (LEN=3) :: ITER_STRING

!
!          ROUTINE TO CONVERT METAFILE FILES TO DIRECT-ACESS BINARY
!          COPYRIGHT (C) 1983, 84, 85  M.S. GERBER & ASSOCIATES, INC.
!          COPYRIGHT (c) 2007 DrG-Solutions, LLC
!

!
!

      ENTRY ES_MAKEBIN

!

         ESCAL_OL = 'BC'
         ESCAL_BASE_FILE_ACTIVE = .FALSE.
         BASE_INPUT_RECORDS = 0.
         OVERLAY_INPUT_RECORDS = 0.
         VOID_CHR = ESCALFL(ESC_FILE_BASE_NAMES)
         DO FILE_NO = 0, MAX_ESC_FILES-1
            IF(FILE_NO == 5 .AND. ITERATION_ON()) THEN
!
               TEMP_I2 = GET_ITERATION_NUMBER()
               WRITE(ITER_CHR,'(I4)') TEMP_I2
               SCREEN_MESSAGES = "Iter #"//TRIM(ITER_CHR)
               CALL MG_LOCATE_WRITE(18,70,TRIM(SCREEN_MESSAGES),3,2)
! NOTE: FIRST ITERATION USES THE DSF AS THE FILE NAME
               IF(TEMP_I2 > 999 .OR. TEMP_I2 <= 1) THEN
                   FILE_NAME = TRIM(BASE_FILE_DIRECTORY())// &
                       TRIM(ESC_FILE_CODES(FILE_NO))//"B"// &
                             TRIM(ESC_FILE_BASE_NAMES(FILE_NO))//".DAT"
               ELSE
! SECOND-TO-N ITERATION USES THE PREVIOUS ITERATION LNG DELIVERIES
                  TEMP_I2 = TEMP_I2 - 1
!
                  IF(TEMP_I2 < 10) THEN
                     WRITE(ITER_STRING,'(I1)') TEMP_I2
                     ITER_STRING = '00'//ITER_STRING
                  ELSEIF(TEMP_I2 < 100) THEN
                     WRITE(ITER_STRING,'(I2)') TEMP_I2
                     ITER_STRING = '0'//ITER_STRING
                  ELSE
                     WRITE(ITER_STRING,'(I3)') TEMP_I2
                  ENDIF
!
                   FILE_NAME = TRIM(BASE_FILE_DIRECTORY())// &
                       TRIM(ESC_FILE_CODES(FILE_NO))//"BZZ"// &
                       TRIM(ITER_STRING)//".DAT"
               ENDIF
            ELSE
                FILE_NAME = TRIM(BASE_FILE_DIRECTORY())// &
                    TRIM(ESC_FILE_CODES(FILE_NO))//"B"// &
                             TRIM(ESC_FILE_BASE_NAMES(FILE_NO))//".DAT"
            ENDIF
            INQUIRE(FILE=FILE_NAME,EXIST=FILEXIST)
            IF(.NOT. FILEXIST) CYCLE
            SCREEN_OUTPUT = TRIM(FILE_TYPE)//'-'// &
            	ESC_FILE_BASE_NAMES(FILE_NO)
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
            OPEN(10,FILE=FILE_NAME)
            OPEN(11,FILE=TRIM(OUTPUT_DIRECTORY())//"BC"// &
            TRIM(ESC_FILE_BINARY_NAMES(FILE_NO))//".BIN", &
                        ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL_ES)
            ESCAL_BASE_FILE_ACTIVE(FILE_NO) = .TRUE.
            IREC = 0
            TABLE_NO = 0
            READ(10,*) DELETE
            DO
               TABLE_NO = TABLE_NO + 1
               TABLE_POS = 0
               DO
                  READ(10,1000,IOSTAT=IOS) RECLN
                  IF(IOS /= 0) EXIT
                  IF(RECLN(1:1) == '7') EXIT
                  RECLN = TRIM(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                  ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,' &
                   //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                    ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,' &
                  //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                    ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
                  VALUES_30 = -9999.
                  VALUES_30(0) = 0.
                  START_MONTH = 0
                  END_MONTH = 0
                  VECTOR_NO = 10001
                  READ(RECLN,*,ERR=200) DELETE,DESCRIPTION,VECTOR_NO, &
				  VECTOR_TYPE,GROWTH_RATE_YEAR,VALUES_30,COMMENT,START_MONTH, &
                            END_MONTH
                  TABLE_POS = TABLE_POS + 1
                  IF(VECTOR_NO < -1) THEN
                     WRITE(4,*) 'Escalation vector number',VECTOR_NO, &
					 'is less than -1 which is the smallest',' allowed value.'
                     WRITE(4,*)'This bad vector number was found at ', &
					 'record',IREC,' in escalation file ', &
                                 TRIM(ESC_FILE_BASE_NAMES(FILE_NO))
                     WRITE(4,*) '*** line 140 ESRN_OBJ.FOR ***'
                     er_message='See WARNING MESSAGES -ESRN_OBJ.FOR-1'
                     call end_program(er_message)
                  ENDIF

                  IF(VECTOR_NO > 30000) THEN
                     WRITE(4,*) 'The vector ',TRIM(DESCRIPTION), &
					 ' located in table',TABLE_NO,' at column',TABLE_POS, &
                                '   ','is defined by rippling.'
                     WRITE(4,*) 'ALL ESCALATION VECTOR NUMBERS MUST BE ' &
					 //      'ENTERED BY THE USER!        '
                     WRITE(4,*) '*** line 149 ESRN_OBJ.FOR ***'
                     er_message='See WARNING MESSAGES -ESRN_OBJ.FOR-2'
                     call end_program(er_message)
                  ENDIF
                  MIN_VECTOR_NUM = MIN(MIN_VECTOR_NUM,VECTOR_NO)
                  MAX_VECTOR_NUM = MAX(MAX_VECTOR_NUM,VECTOR_NO)
                  IREC = IREC + 1
                  DO I = 1, AVAIL_DATA_YEARS
                     IF(VALUES_30(I) == -9999.) THEN
                     	VALUES_30(I)=VALUES_30(I-1)
                     END IF
                  ENDDO
                  WRITE(11,REC=IREC) DELETE,VECTOR_NO,VECTOR_TYPE, &
				  GROWTH_RATE_YEAR,VALUES_30,START_MONTH,END_MONTH
               ENDDO
               IF(IOS /= 0) EXIT
            ENDDO
            CLOSE(10)
            CLOSE(11)
            BASE_INPUT_RECORDS(FILE_NO) = IREC
            MIN_VECTOR_NUM = MIN(MIN_VECTOR_NUM,int(1,2))
         ENDDO
      RETURN
!
! OVERLAY THE GENERATING-UNIT PARAMETER FILE
!

      ENTRY ES_MAKEOVL(OVERLAY_FAMILY_NAME,R_FILE_NO)

!
! 063008. MOVED TO RESET ROUTINE BELOW.
!
!      OVERLAY_INPUT_RECORDS = 0.
      IF(.NOT. ESCAL_BASE_FILE_ACTIVE(R_FILE_NO)) RETURN
      SCREEN_OUTPUT = TRIM(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
      CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      INUNIT = 12
      IF(ESCAL_OL(R_FILE_NO) == 'BC') THEN
         FILE_NAME = TRIM(OUTPUT_DIRECTORY())//"BC"// &
		 TRIM(ESC_FILE_BINARY_NAMES(R_FILE_NO))//".BIN"
         FILE_NAME2 = TRIM(OUTPUT_DIRECTORY())//"OL"// &
		 TRIM(ESC_FILE_BINARY_NAMES(R_FILE_NO))//".BIN"
         CALL COPY_FILE_2_FILE(FILE_NAME,FILE_NAME2)
         OPEN(11,FILE=TRIM(OUTPUT_DIRECTORY())//"BC"// &
		 TRIM(ESC_FILE_BINARY_NAMES(R_FILE_NO))//".BIN", &
		 ACCESS="DIRECT",RECL=LRECL_ES)
         LAST_INPUT_RECORD = BASE_INPUT_RECORDS(R_FILE_NO)
         INUNIT = 11
      ELSE ! 063008. ADDED FOR OVERLAY PROBLEM
         LAST_INPUT_RECORD = OVERLAY_INPUT_RECORDS(R_FILE_NO)
      ENDIF
      OPEN(12,FILE=TRIM(OUTPUT_DIRECTORY())//"OL"// &
	  TRIM(ESC_FILE_BINARY_NAMES(R_FILE_NO))//".BIN", &
                       ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL_ES)
!
      OPEN(10,FILE=TRIM(OUTPUT_DIRECTORY())// &
	  TRIM(ESC_FILE_CODES(R_FILE_NO))//"O"//TRIM(OVERLAY_FAMILY_NAME)//".DAT")
      READ(10,*) DELETE
!
      BC_REC = 0
!
      DO
         DO
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(IOS /=0) EXIT
            IF(RECLN(1:1) == '7') EXIT
            RECLN = TRIM(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
			',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,' &
            //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
			',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,' &
            //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
			',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
! 072908. ALLOW OVERLAY TO HAVE BLANK VALUE FOR BURESH AND HI.
            VECTOR_NO = -29999
            READ(RECLN,*,ERR=200) DELETE,DESCRIPTION,VECTOR_NO
            IF(VECTOR_NO == -29999 .OR. (VECTOR_NO >= MIN_VECTOR_NUM &
				.AND. VECTOR_NO <= MAX_VECTOR_NUM) ) THEN
               BC_REC = BC_REC + 1
               IF(BC_REC == 0) THEN
                  VALUES_30 = -9999.
                  VALUES_30(0) = 0.
                  READ(RECLN,*,ERR=200) DELETE,DESCRIPTION,DELETE, &
				  VECTOR_TYPE,GROWTH_RATE_YEAR,VALUES_30,COMMENT,START_MONTH, &
                                        END_MONTH
                  LAST_INPUT_RECORD = LAST_INPUT_RECORD + 1
                  BC_REC = LAST_INPUT_RECORD
                  DO I = 1, AVAIL_DATA_YEARS
                     IF(VALUES_30(I)==-9999.)VALUES_30(I)=VALUES_30(I-1)
                  ENDDO
               ELSE
                  READ(INUNIT,REC=BC_REC) DELETE,VECTOR_POS_HOLDER, &
				  VECTOR_TYPE,GROWTH_RATE_YEAR,VALUES_30,START_MONTH,END_MONTH
                  READ(RECLN,*,ERR=200) DELETE,DESCRIPTION,IREC, &
				  VECTOR_TYPE,GROWTH_RATE_YEAR,VALUES_30,COMMENT,START_MONTH, &
				  END_MONTH
               ENDIF
               IF(VECTOR_NO == -29999) VECTOR_NO = VECTOR_POS_HOLDER
               WRITE(12,REC=BC_REC) DELETE,VECTOR_NO,VECTOR_TYPE, &
			   GROWTH_RATE_YEAR,VALUES_30,START_MONTH,END_MONTH
            ELSE
               IF(DELETE > 9 .OR. DELETE < 1) THEN
                  WRITE(SCREEN_MESSAGES,'(A,I4)')'The overlay file '// &
				  TRIM(OVERLAY_FAMILY_NAME)// &
                       ' is bad at or near the vector that overlays record', &
					   VECTOR_NO
                  CALL MG_CLEAR_LINE_WRITE(22,0,79,TRIM(SCREEN_MESSAGES), &
				  ALL_VERSIONS,0)
                  CALL MG_CLEAR_LINE_WRITE(23,0,79,'and has the description ' &
				  //TRIM(DESCRIPTION),ALL_VERSIONS,0)
                  er_message='stop requested from ESRN_OBJ SIID105'
                  call end_program(er_message)
               ELSE IF(VECTOR_NO > MAX_VECTOR_NUM .OR. VECTOR_NO < &
				MIN_VECTOR_NUM) THEN
                  WRITE(4,*)'While overlaying the Escalation file ', &
				  'the following problem was found:'
                  WRITE(4,*) 'In overlay file ',OVERLAY_FAMILY_NAME, &
				  ' in the vector with the description ',TRIM(DESCRIPTION)
                  WRITE(4,*)'the vector being referenced to overlay in', &
				  ' the base file',VECTOR_NO,' is not in the '
                  WRITE(4,*) 'base file.  The maximum record in ', &
				  'the base file is ',MAX_VECTOR_NUM
                  WRITE(4,*)
               ENDIF
            ENDIF
         ENDDO
         IF(IOS /= 0) EXIT
      ENDDO
      OVERLAY_INPUT_RECORDS(R_FILE_NO) = LAST_INPUT_RECORD
      CLOSE(10)
      CLOSE(12)
      IF(ESCAL_OL(R_FILE_NO) == 'BC') CLOSE(11)
      ESCAL_OL(R_FILE_NO) = 'OL'
      RETURN
!
  200 CALL MG_LOCATE_WRITE(20,0,TRIM(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from ESRN_OBJ SIID106'
      call end_program(er_message)
!
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END
!

      SUBROUTINE RESET_ESCAL_OL

      USE ESCALATION_VARIABLES
         ESCAL_OL = 'BC'
         OVERLAY_INPUT_RECORDS = 0.
      RETURN
      END

      FUNCTION GET_ESCAL_OL()

      USE ESCALATION_VARIABLES
      INTEGER (KIND=2) :: FILE_NO
      LOGICAL (KIND=1) :: GET_ESCAL_OL
!
         GET_ESCAL_OL = .FALSE.
         DO FILE_NO = 0, MAX_ESC_FILES-1
            IF(ESCAL_OL(FILE_NO) == 'OL') THEN
               GET_ESCAL_OL = .TRUE.
               EXIT
            ENDIF
         ENDDO
      RETURN
      END

      FUNCTION READ_ESCALATION_FILE()
      use end_routine, only: end_program, er_message

!
      use SpinDriftLib
      USE PROD_ARRAYS_DIMENSIONS
      USE ESCALATION_VARIABLES
      USE GRX_PLANNING_ROUTINES
      use esrn_decs
      USE SIZECOM
      use error_central
      use mod_base_year
      LOGICAL (KIND=1) :: READ_ESCALATION_FILE,SET_RNV_ESCALATION_VECTORS
      LOGICAL (KIND=4) :: R_VECTOR_TYPE
      REAL (KIND=4) :: ESCALATED_VECTOR_OR_RETURN_VAL
      INTEGER (KIND=4) :: NUMBER_OF_RNV_VECTORS ! 093008.
      INTEGER (KIND=2) :: MAXIMUM_RNV_VECTOR_NUM
      INTEGER (KIND=2) :: GROWTH_RATE_YEAR,GROWTH_RATE_YR(:),VECTOR_NO,DELETE
      INTEGER (KIND=4) :: I,J
      INTEGER :: IOS
      INTEGER (KIND=2) :: BASE_YR,YR,FILE_NO
      INTEGER (KIND=2) :: R_RUN_YEAR,R_VECTOR_NO,RETURN_VALUES_FOR_A_YEAR, &
	  POSITION_IN_VECTOR_MAP
      REAL :: R_VALUE,R_CURRENT_YEAR_VALUES(*),R_VECTOR_FOR_ALL_YEARS(*), &
	  ESCALATED_MONTHLY_VALUE,ESCALATED_MONTHLY_VAL, &
              ESCALATED_FUEL_MONTHLY_VALUE,R_VALUE_V, &
			  START_UP_COST_ESCALAT_MTHLY_VAL,GET_ANNUAL_VECTOR_VALUE, &
              RET_VAL_FROM_ESCALATION_VECTOR, &
              RET_SNG_VALUE_ESCALATION_VECTOR
      LOGICAL (KIND=1) :: R_VALUE_TYPES(*),RETURN_A_VECTOR_FOR_ALL_YEARS
      REAL :: VALUES(0:AVAIL_DATA_YEARS)
      INTEGER (KIND=4) :: RETURN_ALL_VECTORS_ALL_YEARS, &
	  RETURN_VALUE_TYPES_FOR_A_YEAR !093008.
      REAL :: R_ESCALATION_VECTORS(AVAIL_DATA_YEARS,*)

      INTEGER (KIND=2) :: R_YEAR
      INTEGER (KIND=2) :: R_VECTOR_MAP(*)
      REAL (KIND=4) :: VECTOR_VALUES(:,:),CUMULATIVE_VALUES(:)
      CHARACTER (LEN=2) :: DATA_TYPE
      LOGICAL (KIND=1) :: WEPCO_ESCAL=.FALSE.
      CHARACTER (LEN=256) :: OUTPUT_DIRECTORY,FILE_NAME
      LOGICAL (KIND=1) :: LOGICAL1_FALSE
      PARAMETER(LOGICAL1_FALSE = .FALSE.)
      INTEGER (KIND=2) :: ESCALATION_RATE_IS_REAL(:),REF_NO
      LOGICAL (KIND=1) ::   POSITION_IS_ANNUAL_VECTOR(:)
!
      INTEGER (KIND=2) :: START_MONTH,END_MONTH,MO,R_YR,R_SEASON, &
	  R_UPDATE_SEASON,R_UNITS,RECALCULATE_FUEL_COSTS,R_START_MONTH, &
                          R_END_MONTH,ESCALATE_ALL_MONTHLY_VALUES, &
						  MONTHLY_FUEL_COST_CALC,EXP_YR
      REAL :: SEASONS,TEMP_VALUE

      !TODO: Create HasEscalationRates file routine, which
      ! checks (currently) whether VECTOR_PERIOD_IS_ANNUAL
      ! has been allocated.
      LOGICAL (KIND=1) :: VECTOR_PERIOD_IS_ANNUAL(:),LOGICAL1_TRUE, &
	  MONTHLY_ESCALATIONS_USED,SAVE_MONTHLY_ESCALATIONS=.FALSE., &
                          RESET_MONTHLY_ESCALATIONS_USED
      PARAMETER(LOGICAL1_TRUE = .TRUE.)
!
      ALLOCATABLE :: VECTOR_VALUES,CUMULATIVE_VALUES,GROWTH_RATE_YR, &
	  ESCALATION_RATE_IS_REAL,POSITION_IS_ANNUAL_VECTOR, &
                     VECTOR_PERIOD_IS_ANNUAL
!
      INTEGER (KIND=2) :: ESCALATE_ALL_VALUES,SET_ESCALATION_VECTOR_STRUCTURE, &
	  ESCALATE_DISP_ADDER_VALUES, &
                          ESCALATE_FUEL_ADDER_VALUES,ESCALATE_ONE_VALUE, &
						  R_I,ESCALATE_ONE_VALUE_FOR_ADDS
      REAL :: TEMP_RATE,R_DISPATCH_1(*),R_DISPATCH_2(*),R_FUEL_ADD(*)
      LOGICAL (KIND=1) :: VECTOR_IS_VALUE,RESET_INTO_EXTENSION_PERIOD
      REAL :: R_VALUE_ARRAY(*)
      INTEGER (KIND=2) :: R_VECTOR_NO_ARRAY(*),NUM_VALS
      CHARACTER (LEN=*) :: RECORD_NAME(*)
      CHARACTER (LEN=20) :: ONE_RECORD_NAME
      SAVE  VECTOR_VALUES,CUMULATIVE_VALUES,GROWTH_RATE_YR, &
      VECTOR_PERIOD_IS_ANNUAL

      INTEGER :: VALUES_TO_SET
      LOGICAL (KIND=4) :: FILE_EXISTS
      INTEGER (KIND=4) :: RECORDS_IN_FILE(0:MAX_ESC_FILES-1)
!
         RECORDS_IN_FILE = 0
         DO FILE_NO = 0, MAX_ESC_FILES-1
            IF(ESCAL_OL(FILE_NO) == 'OL') THEN
               RECORDS_IN_FILE(FILE_NO) = OVERLAY_INPUT_RECORDS(FILE_NO)
            ELSE
               RECORDS_IN_FILE(FILE_NO) = BASE_INPUT_RECORDS(FILE_NO)
            ENDIF
         ENDDO
         MAX_VECTORS = SUM(RECORDS_IN_FILE(:))
         MAXIMUM_VECTOR_NUM = MAX_VECTOR_NUM
         MAXIMUM_VECTORS = MAX_VECTORS
         IF(ALLOCATED(I4_VECTOR_MAP)) THEN
            DEALLOCATE(I4_VECTOR_MAP,VECTOR_VALUES,CUMULATIVE_VALUES, &
			VECTOR_TYPE_IS_VALUE,GROWTH_RATE_YR, &
                        I4_MONTHLY_VECTOR_MAP,VECTOR_PERIOD_IS_ANNUAL)
         ENDIF
         ALLOCATE(I4_VECTOR_MAP(-1:MAX_VECTOR_NUM), &
		 VECTOR_VALUES(AVAIL_DATA_YEARS+1,-1:MAX_VECTORS), &
		 I4_REFERENCE_VECTOR_MAP(0:9), &
                  CUMULATIVE_VALUES(-1:MAX_VECTORS), &
				  VECTOR_TYPE_IS_VALUE(-1:MAX_VECTORS), &
				  GROWTH_RATE_YR(-1:MAX_VECTORS), &
                  ESCALATION_RATE_IS_REAL(-1:MAX_VECTORS), &
				  POSITION_IS_ANNUAL_VECTOR(-1:MAX_VECTORS), &
                  I4_MONTHLY_VECTOR_MAP(-1:MAX_VECTOR_NUM,12), &
				  VECTOR_PERIOD_IS_ANNUAL(-1:MAX_VECTOR_NUM))
!
         BASE_YR = get_base_year()
         I4_REFERENCE_VECTOR_MAP = 0
         I4_VECTOR_MAP = 0
         I4_MONTHLY_VECTOR_MAP = -2
         ESCALATION_RATE_IS_REAL = -1
         GROWTH_RATE_YR = 999
         VECTOR_TYPE_IS_VALUE = .FALSE.
         VECTOR_PERIOD_IS_ANNUAL = .TRUE.
         POSITION_IS_ANNUAL_VECTOR = .TRUE.
         VECTOR_VALUES = 0.
         I4 = 0
         DO FILE_NO = 0, MAX_ESC_FILES-1
            FILE_NAME = TRIM(OUTPUT_DIRECTORY())//ESCAL_OL(FILE_NO)// &
			TRIM(ESC_FILE_BINARY_NAMES(FILE_NO))//".BIN"
            INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
            IF(.NOT. FILE_EXISTS) CYCLE
            OPEN(10,FILE=FILE_NAME,ACCESS="DIRECT",RECL=LRECL_ES)
            DO J = 1, RECORDS_IN_FILE(FILE_NO)
               READ(10,REC=J) DELETE,VECTOR_NO,DATA_TYPE,GROWTH_RATE_YEAR, &
			   VALUES,START_MONTH,END_MONTH
               IF(DELETE > 7) CYCLE
               I4 = I4 + 1
               IF(VECTOR_NO == 8118) THEN
                  I4 = I4
               ENDIF
               IF(START_MONTH < 0 .OR. START_MONTH > 12) THEN
                  WRITE(4,*) "WHEN READING ESCALATION RATES FILE, ",FILE_NO, &
				  " , VECTOR ",VECTOR_NO," HAD A START MONTH ", &
                              START_MONTH," WHICH IS OUT OF RANGE."
                  er_message='Stop requested from ESRN_OBJ SIID108'
                  call end_program(er_message)
               ELSEIF(END_MONTH < 0 .OR. END_MONTH > 12) THEN
                  WRITE(4,*) "WHEN READING ESCALATION RATES FILE, ",FILE_NO, &
				  " , VECTOR ",VECTOR_NO," HAD AN END MONTH ", &
                             END_MONTH," WHICH IS OUT OF RANGE."
                  er_message='Stop requested from ESRN_OBJ SIID109'
                  call end_program(er_message)
               ENDIF
               IF(START_MONTH == 0 .OR. END_MONTH == 0) THEN
                  I4_VECTOR_MAP(VECTOR_NO) = I4
               ELSE
                  MO = START_MONTH
                  DO
                     I4_MONTHLY_VECTOR_MAP(VECTOR_NO,MO) = I4
                     IF(MO == END_MONTH) EXIT
                     MO = MO + 1
                     IF(MO > 12) MO = 1
                  ENDDO
                  VECTOR_PERIOD_IS_ANNUAL(VECTOR_NO) = .FALSE.
                  POSITION_IS_ANNUAL_VECTOR(I4) = .FALSE.
               ENDIF
               IF(INDEX(DATA_TYPE,'V') /= 0) THEN
                  VECTOR_TYPE_IS_VALUE(I4) = .TRUE.
                  GROWTH_RATE_YEAR = GROWTH_RATE_YEAR - BASE_YR
                  GROWTH_RATE_YR(I4) = GROWTH_RATE_YEAR
                  IF(GROWTH_RATE_YEAR > 0 .AND. GROWTH_RATE_YEAR < &
					AVAIL_DATA_YEARS + 1) THEN
                     VECTOR_VALUES(AVAIL_DATA_YEARS+1,I4) = &
						VALUES(AVAIL_DATA_YEARS)/100.
!
! 6/11/08. TMS. ADDED ARRAY UPDATE TO INSIDE DO LOOP
!
                     DO YR = GROWTH_RATE_YEAR, AVAIL_DATA_YEARS
                        VALUES(YR) = VALUES(YR-1)*(1.+VALUES(YR)/100.)
                     ENDDO
                  ELSE
                     IF(VALUES(AVAIL_DATA_YEARS-1) /= 0.) THEN
                        VECTOR_VALUES(AVAIL_DATA_YEARS+1,I4) = -1. + &
							VALUES(AVAIL_DATA_YEARS)/ &
							VALUES(AVAIL_DATA_YEARS-1)
                     ELSE
                        VECTOR_VALUES(AVAIL_DATA_YEARS+1,I4) = 0.
                     ENDIF
                  ENDIF

                     VECTOR_VALUES(1:30,I4) = VALUES(1:30)
!                  ENDDO
               ELSEIF( .NOT. VECTOR_PERIOD_IS_ANNUAL(VECTOR_NO) ) THEN
                  VECTOR_VALUES(AVAIL_DATA_YEARS+1,I4) = &
					VALUES(AVAIL_DATA_YEARS)/100.
!
!
! 9/25/98. GAT. ADDED TO ALLOW FOR MONTHLY NOMINAL AND REAL ESCALATIONS
!           DOES IT CREATE NEW PROBLEMS? POSSIBLY WITH A CONNECTION TO
!           FUEL PRICE FILE?
!
                  VALUES(0) = 1.0
!
                  DO YR = 1, AVAIL_DATA_YEARS
                     VALUES(YR) = VALUES(YR-1)*(1.+VALUES(YR)/100.)
                  ENDDO
!
                  IF(INDEX(DATA_TYPE,'R') /= 0) then
					ESCALATION_RATE_IS_REAL(I4) = &
						INDEX('0123456789',DATA_TYPE(2:2))-1
				  end if
!
                  VECTOR_VALUES(1:AVAIL_DATA_YEARS,I4) = &
					VALUES(1:AVAIL_DATA_YEARS)
               ELSE
                  VECTOR_VALUES(1:AVAIL_DATA_YEARS,I4) = &
					VALUES(1:AVAIL_DATA_YEARS)/100.
                  VECTOR_VALUES(AVAIL_DATA_YEARS+1,I4) = &
					VECTOR_VALUES(AVAIL_DATA_YEARS,I4)
                  IF(INDEX(DATA_TYPE,'R') /= 0) THEN
                     ESCALATION_RATE_IS_REAL(I4) = &
						INDEX('0123456789',DATA_TYPE(2:2))-1
                  ELSEIF(INDEX(DATA_TYPE,'X') /= 0) THEN
                     REF_NO = INDEX('0123456789',DATA_TYPE(2:2)) - 1
                     I4_REFERENCE_VECTOR_MAP(REF_NO) = I4
                     IF(REF_NO == 0) THEN
                        I4_VECTOR_MAP(-1) = I4
                        DO YR = 1, AVAIL_DATA_YEARS+1
                           VECTOR_VALUES(YR,-1) = VECTOR_VALUES(YR,I4)
                        ENDDO
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO ! VECTORS
            CLOSE(10,IOSTAT=IOS)
         ENDDO ! ESCALATION FILES
!
         DO I4 = 1, MAX_VECTORS
            IF(ESCALATION_RATE_IS_REAL(I4) == -1) CYCLE
            I4_REF = I4_REFERENCE_VECTOR_MAP(ESCALATION_RATE_IS_REAL(I4))
            IF(POSITION_IS_ANNUAL_VECTOR(I4_REF)) THEN
               IF(WEPCO_ESCAL) THEN
                  DO YR = 1, AVAIL_DATA_YEARS
                     VECTOR_VALUES(YR,I4_REF) = (1 + &
						VECTOR_VALUES(YR,I4_REF)) * &
						(1 + VECTOR_VALUES(YR,I4_REF)) - 1.
                  ENDDO
               ELSE
                  DO YR = 1, AVAIL_DATA_YEARS
                     VECTOR_VALUES(YR,I4_REF) = VECTOR_VALUES(YR,I4_REF) &
						+ VECTOR_VALUES(YR,I4_REF)
                  ENDDO
               ENDIF
            ELSE
               IF(WEPCO_ESCAL) THEN
                  DO YR = 1, AVAIL_DATA_YEARS
                     VECTOR_VALUES(YR,I4_REF) = VECTOR_VALUES(YR,I4_REF) &
						* (1. + VECTOR_VALUES(YR,I4_REF))
                  ENDDO
               ELSE
!
! This should be changed to increase the dimension of VECTOR_VALUES to
! include Year = 0 (Base Year Value).
!
                  VECTOR_VALUES(1,I4_REF) = VECTOR_VALUES(1,I4_REF) * &
					(1. + VECTOR_VALUES(1,I4_REF))
!
                  DO YR = 2, AVAIL_DATA_YEARS
                     VECTOR_VALUES(YR,I4_REF) = VECTOR_VALUES(YR,I4_REF) + &
						VECTOR_VALUES(YR-1,I4_REF) * VECTOR_VALUES(YR,I4_REF)
                  ENDDO
               ENDIF
            ENDIF
         ENDDO ! VECTORS
!
         DEALLOCATE(ESCALATION_RATE_IS_REAL,I4_REFERENCE_VECTOR_MAP, &
		 POSITION_IS_ANNUAL_VECTOR)
         READ_ESCALATION_FILE = .TRUE.
      RETURN
! 050709. RESET AT THE BEGINNING OF EACH ENDPOINT. FOUND BY WPS.

      ENTRY RESET_INTO_EXTENSION_PERIOD()

         INTO_EXTENSION_PERIOD = .FALSE.
         RESET_INTO_EXTENSION_PERIOD = INTO_EXTENSION_PERIOD
      RETURN

      ENTRY SET_ESCALATION_VECTOR_STRUCTURE(R_YEAR)

         SET_ESCALATION_VECTOR_STRUCTURE = R_YEAR
         YR = MIN(R_YEAR,int(AVAIL_DATA_YEARS,2)+int(1,2))
         INTO_EXTENSION_PERIOD = R_YEAR >= AVAIL_DATA_YEARS+1
         IF(ALLOCATED(ACTIVE_VECTOR_VALUES)) then
         	DEALLOCATE(ACTIVE_VECTOR_VALUES)
         end if
         ALLOCATE(ACTIVE_VECTOR_VALUES(-1:MAXIMUM_VECTORS))
         DO I4 = -1, MAXIMUM_VECTORS
            IF(VECTOR_TYPE_IS_VALUE(I4)) THEN
               IF(INTO_EXTENSION_PERIOD) THEN
                  ACTIVE_VECTOR_VALUES(I4) = 1. + VECTOR_VALUES(YR,I4)
               ELSE
                  ACTIVE_VECTOR_VALUES(I4) = VECTOR_VALUES(YR,I4)
               ENDIF
            ELSE
               ACTIVE_VECTOR_VALUES(I4) = 1. + VECTOR_VALUES(YR,I4)
            ENDIF
         ENDDO
      RETURN

      ! TODO:  This is a function and should be a subroutine. But it
      ! cannot become a subroutine until this file is a module.
      ENTRY ESCALATE_ALL_VALUES(R_VALUE_ARRAY,R_VECTOR_NO_ARRAY,NUM_VALS, &
		RECORD_NAME)

! THIS ASSUMES THAT R_VALUE HAS BEEN ESCALATED TO THE PREVIOUS YEAR
! THE number of values is returned and ignored (should be a subroutine).
! The new value(s) are set in R_VALUE_ARRAY directly.
!
         ! It doesn't matter what is returned from this routine. It
         ! returns a value because it's an `ENTRY` and not a separate
         ! subroutine. Return value is meaningless and ignored everywhere
         ! it is encountered.
         ESCALATE_ALL_VALUES = -1

         DO I = 1, NUM_VALS
            POSITION_IN_VECTOR_MAP = R_VECTOR_NO_ARRAY(I)
            IF(POSITION_IN_VECTOR_MAP < -1 .OR. POSITION_IN_VECTOR_MAP > &
				MAXIMUM_VECTOR_NUM) CALL BAD_ESC_VECTOR_NUMBER( &
                  POSITION_IN_VECTOR_MAP,MAXIMUM_VECTOR_NUM,RECORD_NAME(I))
            I4 = I4_VECTOR_MAP(POSITION_IN_VECTOR_MAP)
            IF(VECTOR_TYPE_IS_VALUE(I4)) THEN
               IF(INTO_EXTENSION_PERIOD) THEN
                  IF(GRX_ITERATIONS == 0 .OR. I > GRX_NUNITS) then
					R_VALUE_ARRAY(I) = R_VALUE_ARRAY(I) * &
						ACTIVE_VECTOR_VALUES(I4)
				  end if
               ELSE
                  R_VALUE_ARRAY(I) = ACTIVE_VECTOR_VALUES(I4)
               ENDIF
            ELSEIF(GRX_ITERATIONS == 0 .OR. I > GRX_NUNITS) THEN
               R_VALUE_ARRAY(I) = R_VALUE_ARRAY(I) * ACTIVE_VECTOR_VALUES(I4)
            ENDIF
         ENDDO
      RETURN

      ENTRY ESCALATE_ONE_VALUE(R_VALUE_V,R_I,R_VECTOR_NO,ONE_RECORD_NAME,R_YEAR)

! THIS ASSUMES THAT R_VALUE HAS BEEN ESCALATED TO THE PREVIOUS YEAR
! THE NEW VALUE IS RETURNED
!
         ESCALATE_ONE_VALUE = R_I

         POSITION_IN_VECTOR_MAP = R_VECTOR_NO
         IF(POSITION_IN_VECTOR_MAP < -1 .OR. POSITION_IN_VECTOR_MAP > &
			MAXIMUM_VECTOR_NUM) CALL BAD_ESC_VECTOR_NUMBER( &
                   POSITION_IN_VECTOR_MAP,MAXIMUM_VECTOR_NUM,ONE_RECORD_NAME)
         I4 = I4_VECTOR_MAP(POSITION_IN_VECTOR_MAP)
         IF(VECTOR_TYPE_IS_VALUE(I4)) THEN ! NEVER IN EXTENSION
            R_VALUE_V = VECTOR_VALUES(R_YEAR,I4)
         ELSE
            DO YR = 1, R_YEAR
               R_VALUE_V = R_VALUE_V * (1. + VECTOR_VALUES(YR,I4))
            ENDDO
         ENDIF
      RETURN

      ENTRY ESCALATE_ONE_VALUE_FOR_ADDS(R_VALUE_V,R_I,R_VECTOR_NO, &
		ONE_RECORD_NAME,R_YEAR)

! THIS ASSUMES THAT R_VALUE HAS BEEN ESCALATED TO THE PREVIOUS YEAR
! THE NEW VALUE IS RETURNED
!
         ESCALATE_ONE_VALUE_FOR_ADDS = R_I
         POSITION_IN_VECTOR_MAP = R_VECTOR_NO
         IF(POSITION_IN_VECTOR_MAP < -1 .OR. POSITION_IN_VECTOR_MAP > &
			MAXIMUM_VECTOR_NUM) CALL BAD_ESC_VECTOR_NUMBER( &
                  POSITION_IN_VECTOR_MAP,MAXIMUM_VECTOR_NUM,ONE_RECORD_NAME)
         I4 = I4_VECTOR_MAP(POSITION_IN_VECTOR_MAP)
         IF(VECTOR_TYPE_IS_VALUE(I4)) THEN ! NEVER IN EXTENSION
            R_VALUE_V = VECTOR_VALUES(R_YEAR,I4)
         ELSE
            DO YR = 1, R_YEAR
               R_VALUE_V = R_VALUE_V * (1. + VECTOR_VALUES(YR,I4))
            ENDDO
         ENDIF
      RETURN

      ENTRY ESCALATE_DISP_ADDER_VALUES(R_YEAR,R_DISPATCH_1,R_DISPATCH_2, &
		R_VECTOR_NO_ARRAY,NUM_VALS,RECORD_NAME)

! THIS ASSUMES THAT R_VALUE HAS BEEN ESCALATED TO THE PREVIOUS YEAR
! THE NEW VALUE IS RETURNED
!
         ESCALATE_DISP_ADDER_VALUES = NUM_VALS

         DO I = 1, NUM_VALS
            IF(R_DISPATCH_1(I) == 0. .AND. R_DISPATCH_2(I) == 0.) CYCLE
            POSITION_IN_VECTOR_MAP = R_VECTOR_NO_ARRAY(I)
            IF(POSITION_IN_VECTOR_MAP < -1 .OR. POSITION_IN_VECTOR_MAP > &
				MAXIMUM_VECTOR_NUM) CALL BAD_ESC_VECTOR_NUMBER( &
                   POSITION_IN_VECTOR_MAP,MAXIMUM_VECTOR_NUM,RECORD_NAME(I))
            I4 = I4_VECTOR_MAP(POSITION_IN_VECTOR_MAP)
            IF(.NOT. VECTOR_TYPE_IS_VALUE(I4) .OR. INTO_EXTENSION_PERIOD) THEN
                  TEMP_RATE = ACTIVE_VECTOR_VALUES(I4)
            ELSE
               TEMP_RATE = 1.
               IF(R_YEAR == 1) THEN
                  IF(VECTOR_VALUES(1,I4) /= 0.) THEN
                     TEMP_RATE = 1. + (VECTOR_VALUES(2,I4) - &
						VECTOR_VALUES(1,I4))/ VECTOR_VALUES(1,I4)
                  ENDIF
               ELSE
                  IF(VECTOR_VALUES(R_YEAR-1,I4) /= 0.) THEN
                     TEMP_RATE = 1. + (VECTOR_VALUES(R_YEAR,I4) - &
						VECTOR_VALUES(R_YEAR-1,I4))/ VECTOR_VALUES(R_YEAR-1,I4)
                  ENDIF
               ENDIF
            ENDIF
            IF(GRX_ITERATIONS == 0 .OR. I > GRX_NUNITS) THEN
               R_DISPATCH_1(I) = R_DISPATCH_1(I) * TEMP_RATE
               R_DISPATCH_2(I) = R_DISPATCH_2(I) * TEMP_RATE
            ENDIF
         ENDDO
      RETURN

      ENTRY ESCALATE_FUEL_ADDER_VALUES(R_YEAR,R_FUEL_ADD, &
	  R_VECTOR_NO_ARRAY,NUM_VALS,RECORD_NAME)

! THIS ASSUMES THAT R_VALUE HAS BEEN ESCALATED TO THE PREVIOUS YEAR
! THE NEW VALUE IS RETURNED
!
         ESCALATE_FUEL_ADDER_VALUES = NUM_VALS
!         IF(ITERATE_GRX_PLANNING() .AND. GRX_ITERATIONS > 1) RETURN REMOVED 3/4/11
         DO I = 1, NUM_VALS
            IF(R_FUEL_ADD(I) == 0.) CYCLE
            POSITION_IN_VECTOR_MAP = R_VECTOR_NO_ARRAY(I)
            IF(POSITION_IN_VECTOR_MAP < -1 .OR. POSITION_IN_VECTOR_MAP > &
				MAXIMUM_VECTOR_NUM) CALL BAD_ESC_VECTOR_NUMBER( &
                   POSITION_IN_VECTOR_MAP,MAXIMUM_VECTOR_NUM,RECORD_NAME(I))
            I4 = I4_VECTOR_MAP(POSITION_IN_VECTOR_MAP)
            IF(.NOT. VECTOR_TYPE_IS_VALUE(I4) .OR. INTO_EXTENSION_PERIOD) THEN
                  TEMP_RATE = ACTIVE_VECTOR_VALUES(I4)
            ELSE
               TEMP_RATE = 1.
               IF(R_YEAR == 1) THEN
                  IF(VECTOR_VALUES(1,I4) /= 0.) THEN
                     TEMP_RATE = 1. + (VECTOR_VALUES(2,I4) - &
						VECTOR_VALUES(1,I4))/ VECTOR_VALUES(1,I4)
                  ENDIF
               ELSE
                  IF(VECTOR_VALUES(R_YEAR-1,I4) /= 0.) THEN
                     TEMP_RATE = 1. + (VECTOR_VALUES(R_YEAR,I4) - &
						VECTOR_VALUES(R_YEAR-1,I4))/ VECTOR_VALUES(R_YEAR-1,I4)
                  ENDIF
               ENDIF
            ENDIF
            IF(GRX_ITERATIONS == 0 .OR. I > GRX_NUNITS) THEN
               R_FUEL_ADD(I) = R_FUEL_ADD(I) * TEMP_RATE
            ENDIF
         ENDDO
      RETURN

      ENTRY ESCALATE_ALL_MONTHLY_VALUES(R_VALUE_ARRAY,R_VECTOR_NO_ARRAY, &
		NUM_VALS,RECORD_NAME,R_YR,R_START_MONTH,R_END_MONTH)

! THIS ASSUMES THAT R_VALUE HAS BEEN ESCALATED TO THE PREVIOUS YEAR
! THE NEW VALUE IS RETURNED
!
         ESCALATE_ALL_MONTHLY_VALUES = NUM_VALS

         SEASONS = FLOAT(R_END_MONTH - R_START_MONTH + 1)
         DO I = 1, NUM_VALS
            POSITION_IN_VECTOR_MAP = R_VECTOR_NO_ARRAY(I)
            IF(POSITION_IN_VECTOR_MAP < -1 .OR. POSITION_IN_VECTOR_MAP >  &
				MAXIMUM_VECTOR_NUM) CALL BAD_ESC_VECTOR_NUMBER( &
                  POSITION_IN_VECTOR_MAP,MAXIMUM_VECTOR_NUM,RECORD_NAME(I))
            IF(VECTOR_PERIOD_IS_ANNUAL(POSITION_IN_VECTOR_MAP)) THEN
               I4 = I4_VECTOR_MAP(POSITION_IN_VECTOR_MAP)
               IF(R_START_MONTH > 0) CYCLE
               IF(VECTOR_TYPE_IS_VALUE(I4)) THEN
                  IF(INTO_EXTENSION_PERIOD) THEN
                     IF(GRX_ITERATIONS == 0 .OR. I > GRX_NUNITS) THEN
                        R_VALUE_ARRAY(I) = R_VALUE_ARRAY(I) * &
							ACTIVE_VECTOR_VALUES(I4)
                     ENDIF
                  ELSE
                     R_VALUE_ARRAY(I) = ACTIVE_VECTOR_VALUES(I4)
                  ENDIF
               ELSEIF(GRX_ITERATIONS == 0 .OR. I > GRX_NUNITS) THEN
                  R_VALUE_ARRAY(I) = R_VALUE_ARRAY(I) * ACTIVE_VECTOR_VALUES(I4)
               ENDIF
            ELSE ! MONTHLY REPRESENTATION
               SAVE_MONTHLY_ESCALATIONS = .TRUE.
               IF(R_START_MONTH < 1) CYCLE
               TEMP_VALUE = 0.
               IF(INTO_EXTENSION_PERIOD) THEN
                  DO MO = R_START_MONTH, R_END_MONTH
                     I4 = I4_MONTHLY_VECTOR_MAP(POSITION_IN_VECTOR_MAP,MO)
                     IF(GRX_ITERATIONS == 0 .OR. I > GRX_NUNITS) THEN
                        TEMP_VALUE = TEMP_VALUE + R_VALUE_ARRAY(I4) * &
							VECTOR_VALUES(AVAIL_DATA_YEARS+1,I4)
                     ELSE
                       TEMP_VALUE = TEMP_VALUE + R_VALUE_ARRAY(I4)
                     ENDIF
                  ENDDO
               ELSE
                  DO MO = R_START_MONTH, R_END_MONTH
                     I4 = I4_MONTHLY_VECTOR_MAP(POSITION_IN_VECTOR_MAP,MO)
                     TEMP_VALUE = TEMP_VALUE+VECTOR_VALUES(R_YR,I4)
                  ENDDO
               ENDIF
               R_VALUE_ARRAY(I4) = TEMP_VALUE / SEASONS
            ENDIF
         ENDDO
      RETURN

      ENTRY MONTHLY_ESCALATIONS_USED()

         MONTHLY_ESCALATIONS_USED = SAVE_MONTHLY_ESCALATIONS
      RETURN

      ENTRY MONTHLY_FUEL_COST_CALC(R_UNITS)

         MONTHLY_FUEL_COST_CALC = RECALCULATE_FUEL_COSTS(R_UNITS)
      RETURN

      ENTRY RESET_MONTHLY_ESCALATIONS_USED()

         SAVE_MONTHLY_ESCALATIONS = .FALSE.
         RESET_MONTHLY_ESCALATIONS_USED = SAVE_MONTHLY_ESCALATIONS
      RETURN

      ENTRY MAXIMUM_RNV_VECTOR_NUM

         MAXIMUM_RNV_VECTOR_NUM = MAXIMUM_VECTOR_NUM
      RETURN

      ENTRY NUMBER_OF_RNV_VECTORS

         NUMBER_OF_RNV_VECTORS = MAXIMUM_VECTORS
      RETURN

      ENTRY ESCALATED_VECTOR_OR_RETURN_VAL(R_VECTOR_TYPE,R_VECTOR_NO,R_YR, &
	  R_SEASON,R_UPDATE_SEASON)

         IF(ABS(R_VECTOR_NO) < -1 .OR. R_VECTOR_NO > MAXIMUM_VECTOR_NUM) THEN
            I4 = 0
         ELSE
            I4 = R_VECTOR_NO
         ENDIF
         IF(.NOT. ALLOCATED(VECTOR_PERIOD_IS_ANNUAL)) THEN 
			! NO ESCALATION RATES FILE.
            ESCALATED_VECTOR_OR_RETURN_VAL = 1.
            R_VECTOR_TYPE = .TRUE.  ! RETURN VALUE IS ESCALATED.
            RETURN
         ENDIF
         IF(VECTOR_PERIOD_IS_ANNUAL(I4)) THEN
            I4 = I4_VECTOR_MAP(I4)
            IF(VECTOR_TYPE_IS_VALUE(I4)) THEN
               R_VECTOR_TYPE = .FALSE.  ! RETURN VECTOR IS VALUE
               IF(INTO_EXTENSION_PERIOD) THEN
                  EXP_YR = R_YR - AVAIL_DATA_YEARS
                  IF(R_SEASON >= R_UPDATE_SEASON) THEN
                     ESCALATED_MONTHLY_VAL = &
						VECTOR_VALUES(AVAIL_DATA_YEARS,I4) * &
							ACTIVE_VECTOR_VALUES(I4)**EXP_YR
                  ELSEIF(EXP_YR > 1) THEN
                     ESCALATED_MONTHLY_VAL = &
						VECTOR_VALUES(AVAIL_DATA_YEARS,I4) * &
							ACTIVE_VECTOR_VALUES(I4)**(EXP_YR-1)
                  ELSE
                     ESCALATED_MONTHLY_VAL = VECTOR_VALUES(AVAIL_DATA_YEARS,I4)
                  ENDIF
               ELSE
                  ESCALATED_MONTHLY_VAL = ACTIVE_VECTOR_VALUES(I4)
               ENDIF
            ELSE
               R_VECTOR_TYPE = .TRUE.  ! RETURN VALUE IS ESCALATED.
               ESCALATED_MONTHLY_VAL = 1.
               DO EXP_YR = 1, R_YR-1
                  ESCALATED_MONTHLY_VAL = ESCALATED_MONTHLY_VAL * &
					(1.+VECTOR_VALUES(EXP_YR,I4))
               ENDDO
               IF(R_SEASON >= R_UPDATE_SEASON) ESCALATED_MONTHLY_VAL = &
				ESCALATED_MONTHLY_VAL * (1.+VECTOR_VALUES(R_YR,I4))
            ENDIF
            ESCALATED_VECTOR_OR_RETURN_VAL = ESCALATED_MONTHLY_VAL
         ENDIF
      RETURN

      ! TODO: Make into module function
      ENTRY ESCALATED_MONTHLY_VALUE(R_VALUE,R_VECTOR_NO,R_YR, &
		R_SEASON,R_UPDATE_SEASON)

! THIS ASSUMES THAT R_VALUE HAS BEEN ESCALATED TO THE PREVIOUS YEAR
! THE NEW VALUE IS RETURNED
!
         ESCALATED_MONTHLY_VALUE = R_VALUE


         ! TODO: R_VECTOR_NO>MAX should be an error condition. Fix.
         IF(R_VECTOR_NO > MAXIMUM_VECTOR_NUM) THEN
            I4 = 0
         ELSE
            I4 = R_VECTOR_NO
         ENDIF
         ! TODO: Determine whether allocation check is required.  Else
         ! block should be IF for positive allocation.  Result was
         ! set to R_Value at the beginning of the routine so that
         ! is redundant as well.
         
         IF(.NOT. ALLOCATED(VECTOR_PERIOD_IS_ANNUAL)) THEN
		     ! NO ESCALATION RATES FILE.

            ! TODO: Create HasEscalationRates file routine, which
            ! checks (currently) whether VECTOR_PERIOD_IS_ANNUAL
            ! has been allocated. (^^)
            ESCALATED_MONTHLY_VALUE = R_VALUE
         ELSEIF(VECTOR_PERIOD_IS_ANNUAL(I4)) THEN
            I4 = I4_VECTOR_MAP(I4)
            IF(VECTOR_TYPE_IS_VALUE(I4)) THEN
               IF(INTO_EXTENSION_PERIOD) THEN
                  EXP_YR = R_YR - AVAIL_DATA_YEARS
                  IF(R_SEASON >= R_UPDATE_SEASON) THEN
                     ESCALATED_MONTHLY_VALUE = &
						VECTOR_VALUES(AVAIL_DATA_YEARS,I4) * &
						ACTIVE_VECTOR_VALUES(I4)**EXP_YR
                  ELSEIF(EXP_YR > 1) THEN
                     ESCALATED_MONTHLY_VALUE = &
						VECTOR_VALUES(AVAIL_DATA_YEARS,I4) * &
						ACTIVE_VECTOR_VALUES(I4)**(EXP_YR-1)
                  ELSE
                     ESCALATED_MONTHLY_VALUE = VECTOR_VALUES(AVAIL_DATA_YEARS,I4)
                  ENDIF
               ELSE
                  ESCALATED_MONTHLY_VALUE = ACTIVE_VECTOR_VALUES(I4)
               ENDIF
            ELSEIF(R_UPDATE_SEASON == R_SEASON) THEN
               ESCALATED_MONTHLY_VALUE = R_VALUE * ACTIVE_VECTOR_VALUES(I4)
            ELSE
               ESCALATED_MONTHLY_VALUE = R_VALUE
            ENDIF
         ELSE ! MONTHLY REPRESENTATION ALWAYS REPLACES VARIABLE VALUE
            I4 = I4_MONTHLY_VECTOR_MAP(I4,R_SEASON)
            IF(INTO_EXTENSION_PERIOD) THEN
               EXP_YR = R_YR - AVAIL_DATA_YEARS
               IF(R_SEASON >= R_UPDATE_SEASON) THEN
                  ESCALATED_MONTHLY_VALUE = VECTOR_VALUES(AVAIL_DATA_YEARS,I4) &
					* (1+VECTOR_VALUES(AVAIL_DATA_YEARS+1,I4))**EXP_YR
               ELSEIF(EXP_YR > 1) THEN
                  ESCALATED_MONTHLY_VALUE = VECTOR_VALUES(AVAIL_DATA_YEARS,I4) &
					* (1+VECTOR_VALUES(AVAIL_DATA_YEARS+1,I4))** &
                                                             (EXP_YR-1)
               ELSE
                  ESCALATED_MONTHLY_VALUE = VECTOR_VALUES(AVAIL_DATA_YEARS,I4)
               ENDIF

            ELSE

               IF(VECTOR_TYPE_IS_VALUE(I4)) THEN
                  ESCALATED_MONTHLY_VALUE = VECTOR_VALUES(R_YR,I4)
               ELSEIF(R_UPDATE_SEASON == R_SEASON) THEN

                  ESCALATED_MONTHLY_VALUE = R_VALUE * VECTOR_VALUES(R_YR,I4)
               ELSE
                  ESCALATED_MONTHLY_VALUE = R_VALUE
               ENDIF
            ENDIF
         ENDIF
      RETURN
! END ENTRY ESCALATED_MONTHLY_VALUE

      ENTRY START_UP_COST_ESCALAT_MTHLY_VAL(R_VECTOR_NO,R_YR)

! THIS ASSUMES THAT R_VALUE HAS BEEN ESCALATED TO THE PREVIOUS YEAR
! THE NEW VALUE IS RETURNED
!
         IF(ABS(R_VECTOR_NO) < -1 .OR. ABS(R_VECTOR_NO) > MAXIMUM_VECTOR_NUM) THEN
            I = 0
         ELSE
            I = ABS(R_VECTOR_NO)
         ENDIF
         START_UP_COST_ESCALAT_MTHLY_VAL = 0.
         IF(.NOT. ALLOCATED(VECTOR_PERIOD_IS_ANNUAL) ) then
			RETURN ! NO ESCALATION RATES FILE.
		end if

         IF(VECTOR_PERIOD_IS_ANNUAL(I)) THEN
            I = I4_VECTOR_MAP(I)
            IF(R_YR > AVAIL_DATA_YEARS) THEN
               START_UP_COST_ESCALAT_MTHLY_VAL = &
			   VECTOR_VALUES(AVAIL_DATA_YEARS,I) * ACTIVE_VECTOR_VALUES(I)** &
                (R_YR-AVAIL_DATA_YEARS)
            ELSE
               START_UP_COST_ESCALAT_MTHLY_VAL = ACTIVE_VECTOR_VALUES(I)
            ENDIF
         ENDIF
      RETURN

      ENTRY GET_ANNUAL_VECTOR_VALUE(R_VECTOR_NO,R_YR)

! THIS ASSUMES THAT R_VALUE HAS BEEN ESCALATED TO THE PREVIOUS YEAR
! THE NEW VALUE IS RETURNED
!
         GET_ANNUAL_VECTOR_VALUE = 0
         IF(ABS(R_VECTOR_NO) < -1 .OR. ABS(R_VECTOR_NO) > MAXIMUM_VECTOR_NUM) THEN
            I = 0
         ELSE
            I = ABS(R_VECTOR_NO)
         ENDIF
         IF(.NOT. ALLOCATED(VECTOR_PERIOD_IS_ANNUAL) ) then
			RETURN ! NO ESCALATION RATES FILE.
		end if
         IF(VECTOR_PERIOD_IS_ANNUAL(I)) THEN
            I = I4_VECTOR_MAP(I)
            GET_ANNUAL_VECTOR_VALUE = VECTOR_VALUES(MIN(AVAIL_DATA_YEARS,R_YR),I)
         ENDIF
      RETURN

      ENTRY RET_VAL_FROM_ESCALATION_VECTOR(R_YEAR,R_VECTOR_NO, &
		VECTOR_IS_VALUE,R_SEASON)

! THIS ASSUMES THAT R_VALUE HAS BEEN ESCALATED TO THE PREVIOUS YEAR
! THE NEW VALUE IS RETURNED
!
         IF(ABS(R_VECTOR_NO) < -1 .OR. ABS(R_VECTOR_NO) > MAXIMUM_VECTOR_NUM) THEN
            I = 0
         ELSE
            I = ABS(R_VECTOR_NO)
         ENDIF
         RET_VAL_FROM_ESCALATION_VECTOR = 0.
         VECTOR_IS_VALUE = .FALSE.
         YR = MIN(R_YEAR,AVAIL_DATA_YEARS)
         IF(ALLOCATED(VECTOR_PERIOD_IS_ANNUAL)) THEN ! NO ESCALATION RATES FILE.

! MONTHLY REPRESENTATION ALWAYS REPLACES VARIABLE VALUE
! 092707. REPLACE VALUE IF VECTOR IS NOT VALUE OR IN EXTENSION PERIOD

            IF(VECTOR_PERIOD_IS_ANNUAL(I)) THEN
               I4 = I4_VECTOR_MAP(I)
               TEMP_RATE = 100.
            ELSE
               I4 = I4_MONTHLY_VECTOR_MAP(I,R_SEASON)
               TEMP_RATE = 1.
            ENDIF

            IF(YR >= GROWTH_RATE_YR(I4)) THEN
               VECTOR_IS_VALUE = .FALSE.
            ELSE
               VECTOR_IS_VALUE = VECTOR_TYPE_IS_VALUE(I4)
            ENDIF
            RET_VAL_FROM_ESCALATION_VECTOR = VECTOR_VALUES(YR,I4)*TEMP_RATE

         ENDIF
      RETURN

      ENTRY RET_SNG_VALUE_ESCALATION_VECTOR(R_VECTOR_NO,R_YEAR,R_SEASON)

! THIS ASSUMES THAT R_VALUE HAS BEEN ESCALATED TO THE PREVIOUS YEAR
! THE NEW VALUE IS RETURNED
!
         IF(ABS(R_VECTOR_NO) < -1 .OR. ABS(R_VECTOR_NO) > MAXIMUM_VECTOR_NUM) THEN
            I = 0
         ELSE
            I = ABS(R_VECTOR_NO)
         ENDIF
         RET_SNG_VALUE_ESCALATION_VECTOR = 0.
         YR = MIN(R_YEAR,AVAIL_DATA_YEARS)
         IF(ALLOCATED(VECTOR_PERIOD_IS_ANNUAL)) THEN ! NO ESCALATION RATES FILE.
            I4 = I4_MONTHLY_VECTOR_MAP(I,R_SEASON)
            IF(I4 == -2) I4 = I4_VECTOR_MAP(I)
            RET_SNG_VALUE_ESCALATION_VECTOR = VECTOR_VALUES(YR,I4)
         ENDIF
      RETURN

      ENTRY ESCALATED_FUEL_MONTHLY_VALUE(R_VALUE,R_VECTOR_NO,R_YR, &
		R_SEASON,R_UPDATE_SEASON)

! THIS ASSUMES THAT R_VALUE HAS BEEN ESCALATED TO THE PREVIOUS YEAR
! THE NEW VALUE IS RETURNED
!
! SAME AS ESCALATED_MONTHLY_VALUE, EXCEPT
! ACTIVE_VECTOR_VALUES REPLACE WITH VECTOR_VALUES
!
         ESCALATED_FUEL_MONTHLY_VALUE = R_VALUE

         IF(R_VECTOR_NO < -1 .OR. R_VECTOR_NO > MAXIMUM_VECTOR_NUM) THEN
            I = 0
         ELSE
            I = R_VECTOR_NO
         ENDIF
         IF(.NOT. ALLOCATED(VECTOR_PERIOD_IS_ANNUAL)) THEN
            ESCALATED_FUEL_MONTHLY_VALUE = R_VALUE
         ELSEIF(VECTOR_PERIOD_IS_ANNUAL(I)) THEN
            I = I4_VECTOR_MAP(I)
            IF(INTO_EXTENSION_PERIOD) THEN
               ESCALATED_FUEL_MONTHLY_VALUE = R_VALUE * &
				VECTOR_VALUES(AVAIL_DATA_YEARS+1,I)
            ELSEIF(VECTOR_TYPE_IS_VALUE(I)) THEN
               ESCALATED_FUEL_MONTHLY_VALUE = VECTOR_VALUES(R_YR,I)
            ELSE
               ESCALATED_FUEL_MONTHLY_VALUE = R_VALUE * VECTOR_VALUES(R_YR,I)
            ENDIF
         ELSE ! MONTHLY REPRESENTATION ALWAYS REPLACES VARIABLE VALUE
            I4 = I4_MONTHLY_VECTOR_MAP(I,R_SEASON)
            IF(INTO_EXTENSION_PERIOD) THEN
               ESCALATED_FUEL_MONTHLY_VALUE = R_VALUE * &
				VECTOR_VALUES(AVAIL_DATA_YEARS+1,I4)
            ELSE
               ESCALATED_FUEL_MONTHLY_VALUE = VECTOR_VALUES(R_YR,I4)
            ENDIF
         ENDIF
      RETURN

      ENTRY RETURN_VALUES_FOR_A_YEAR(R_CURRENT_YEAR_VALUES,R_RUN_YEAR)

! THE VALUES FOR A YEAR ARE RETURNED ORDERED BY VECTOR NUMBER
!
         YR = MIN(R_RUN_YEAR,int(AVAIL_DATA_YEARS,2)+int(1,2))
         DO VECTOR_NO = 1, MAXIMUM_VECTORS
            I4 = I4_VECTOR_MAP(VECTOR_NO)
            IF(I4 == 0) THEN
               R_CURRENT_YEAR_VALUES(VECTOR_NO) = 0.
            ELSE
               R_CURRENT_YEAR_VALUES(VECTOR_NO) = VECTOR_VALUES(YR,I4)
            ENDIF
         ENDDO
         RETURN_VALUES_FOR_A_YEAR = MAXIMUM_VECTORS
      RETURN

      ENTRY RETURN_VALUE_TYPES_FOR_A_YEAR(R_VALUE_TYPES,R_RUN_YEAR)

! A LOGICAL ARRAY THAT IS TRUE IF THE VALUE IS A VALUE NOT AN ESCALATION
! RATE IS RETURNED ORDERED BY VECTOR NUMBER.
! NOTE: ALL VALUES ARE ESCALATIONS RATE AFTER THE 30TH YEAR.
!
         IF(R_RUN_YEAR >= AVAIL_DATA_YEARS+1) THEN
            DO VECTOR_NO = 1, MAXIMUM_VECTORS
               R_VALUE_TYPES(VECTOR_NO) = .FALSE.
            ENDDO
         ELSE
            DO VECTOR_NO = 1, MAXIMUM_VECTORS
               I4 = I4_VECTOR_MAP(VECTOR_NO)
               IF(I4 == 0) THEN
                  R_VALUE_TYPES(VECTOR_NO) = .FALSE.
               ELSE
                  R_VALUE_TYPES(VECTOR_NO) = VECTOR_TYPE_IS_VALUE(I4)
               ENDIF
            ENDDO
         ENDIF
         RETURN_VALUE_TYPES_FOR_A_YEAR = MAXIMUM_VECTORS
      RETURN

      ENTRY RETURN_A_VECTOR_FOR_ALL_YEARS(R_VECTOR_FOR_ALL_YEARS,R_VECTOR_NO)

! THE VALUES FOR A VECTOR ARE RETURN FOR THE AVAILABLE DATA YEARS
!
           IF(R_VECTOR_NO <= MAXIMUM_VECTOR_NUM) THEN
            I4 = I4_VECTOR_MAP(R_VECTOR_NO)
            RETURN_A_VECTOR_FOR_ALL_YEARS = VECTOR_TYPE_IS_VALUE(I4)
            IF(VECTOR_TYPE_IS_VALUE(I4)) THEN
               DO YR = 1, AVAIL_DATA_YEARS
                  R_VECTOR_FOR_ALL_YEARS(YR) = VECTOR_VALUES(YR,I4)
               ENDDO
            ELSE
               DO YR = 1, AVAIL_DATA_YEARS
                  R_VECTOR_FOR_ALL_YEARS(YR) = 1. + VECTOR_VALUES(YR,I4)
               ENDDO
            ENDIF
         ELSE
            call call_error("Escalation vector")
            WRITE(4,*) "Escalation vector",R_VECTOR_NO,' does not exist.'
            WRITE(4,*) '*** line 1183 ESRN_OBJ.FOR ***'
            er_message='See WARNING MESSAGES -ESRN_OBJ.FOR-3'
            call end_program(er_message)
         ENDIF
      RETURN

      ENTRY RETURN_ALL_VECTORS_ALL_YEARS(R_ESCALATION_VECTORS, &
		R_VALUE_TYPES,R_VECTOR_MAP)

! VECTORS, VECTOR TYPES AND THEIR MAPPING TO VECTOR NUMBER ARE RETURNED
         DO I = 1, MAXIMUM_VECTORS
            R_VALUE_TYPES(I) = VECTOR_TYPE_IS_VALUE(I)
            R_VECTOR_MAP(I) = I4_VECTOR_MAP(I)
            DO YR = 1, AVAIL_DATA_YEARS
               R_ESCALATION_VECTORS(I,YR) = VECTOR_VALUES(YR,I)
            ENDDO
         ENDDO
         RETURN_ALL_VECTORS_ALL_YEARS = MAXIMUM_VECTORS
      RETURN
      END

      SUBROUTINE BAD_ESC_VECTOR_NUMBER(INPUT_VECTOR_NUM,MAX_VECTOR_NUM,RECORD_NAME)
      use end_routine, only: end_program, er_message

      INTEGER (KIND=2) :: INPUT_VECTOR_NUM,MAX_VECTOR_NUM
      CHARACTER (LEN=20) :: RECORD_NAME

         WRITE(4,*) " "
         WRITE(4,*) "A Bad Escalation Vector Reference was Found"
         WRITE(4,*) "in Record ",RECORD_NAME,", and Referencing ", &
			INPUT_VECTOR_NUM,"."
         WRITE(4,*) "Minimum Vector Number = -1,"
         WRITE(4,*) "Maximum Vector Number (from file) =",MAX_VECTOR_NUM
         WRITE(4,*) " "

         WRITE(4,*) '*** line 1210 ESRN_OBJ.FOR ***'
         er_message='See WARNING MESSAGES -ESRN_OBJ.FOR-4'
         call end_program(er_message)

      END
