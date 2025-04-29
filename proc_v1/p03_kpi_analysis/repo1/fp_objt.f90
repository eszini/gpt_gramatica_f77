!     Copyright(c) M.S. Gerber & Associates 2000
!     Created: 4/30/2003 5:56:30 PM
!     Author : MARK S GERBER
!     Last change: MSG 1/29/2007 3:53:38 PM


!   PRICE ELASTICITY OBJECT
!
      SUBROUTINE FP_OBJECT
      use end_routine, only: end_program, er_message
      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM

      INTEGER (kind=2) ::  gc_max_years
      INTEGER (kind=2) ::  I,L,IREC,INUNIT,DELETE,LRECL=88
      INTEGER ::  IOS
      LOGICAL (kind=1) ::  PRICE_FEEDBACK_ACTIVE,YES_PRICE_FEEDBACK
      LOGICAL (kind=4) ::  FILE_EXISTS
      CHARACTER (len=2) ::  ELASTICITY_FILE_OL='BC'
      CHARACTER (len=2) ::  R_ELASTICITY_FILE_OL
      CHARACTER (len=5) ::  ELASTICITY_FILE
      CHARACTER (len=5) ::  OVERLAY_FAMILY_NAME,BASE_FILE_NAME
      CHARACTER (len=14) ::  FILE_TYPE='Price Feedback'
      CHARACTER (len=256) ::  FILE_NAME
      CHARACTER (len=256) ::  BASE_FILE_DIRECTORY
      CHARACTER (len=256) ::  DATA_DRIVE,OUTPUT_DIRECTORY
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER (len=1024) ::  RECLN
! DECLARATION FOR GENERATING UNIT PARAMETERS
      CHARACTER (len=40) ::  DESCRIPTION
! DECLARATION FOR REAL VALUES
      INTEGER (kind=2) ::  YEAR
      REAL ::  VALUES(21),VALUES_7(7),VALUES_14(14)
      EQUIVALENCE (VALUES(1),VALUES_14(1)), &
                  (VALUES(15),VALUES_7(1))
      LOGICAL (kind=1) ::  LAHEY_LF95
      CHARACTER (len=30) ::  SCREEN_OUTPUT


!          SUBROUTINE TO CONVERT METAFILE FILES TO DIRECT ACESS BINARY
!          COPYRIGHT (C) 1983, 84, 85  M.S. GERBER & ASSOCIATES, INC.

! CONVERT ELASTICITY FILE
      ENTRY FP_MAKEBIN
      BASE_FILE_NAME = ELASTICITY_FILE()
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = trim(BASE_FILE_DIRECTORY())//   &
                                "FPB"//trim(BASE_FILE_NAME)//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         ENDIF
         PRICE_FEEDBACK_ACTIVE = YES_PRICE_FEEDBACK(.TRUE.)
         OPEN(10,FILE=FILE_NAME)

         OPEN(11,FILE=trim(DATA_DRIVE)//"BCELAST.BIN",ACCESS="DIRECT",   &
                                            STATUS="UNKNOWN",RECL=LRECL)
         IREC = 1
         VALUES_14 = 0.
         VALUES_7 = -999.
         READ(10,*) DELETE
         DO
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(IOS /= 0) EXIT

            READ(RECLN,*,ERR=200) DELETE,YEAR,VALUES_14,   &
                                  DESCRIPTION,VALUES_7
            L = 2
            DO I = 15, 21
               IF(VALUES(I) == -999.) VALUES(I) = VALUES (L)
               L = L + 2
            ENDDO
            WRITE(11,REC=IREC) DELETE,YEAR,VALUES
            IREC = IREC + 1
         ENDDO

         DO WHILE (IREC <= gc_max_years() - 1)
            WRITE(11,REC=IREC) DELETE,YEAR,VALUES
            IREC = IREC + 1
         ENDDO
         CLOSE(10)
         CLOSE(11)
      ELSEIF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,BASE_FILE_NAME, "fp_objt:1111")
      ELSE
         PRICE_FEEDBACK_ACTIVE = YES_PRICE_FEEDBACK(.FALSE.)
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCELAST.BIN",ACCESS="DIRECT",   &
                                            STATUS="UNKNOWN",RECL=LRECL)


         CLOSE(11)
      ENDIF
      RETURN
!

! OVERLAY THE PRICE ELASTICITY FILE

!
      ENTRY FP_MAKEOVL(OVERLAY_FAMILY_NAME)
      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      DATA_DRIVE = OUTPUT_DIRECTORY()

      FILE_NAME = trim(DATA_DRIVE)//"FPO"//   &
                                     trim(OVERLAY_FAMILY_NAME)//".DAT"
      OPEN(10,FILE=FILE_NAME)
      READ(10,*) DELETE
      INUNIT = 12
      IF(ELASTICITY_FILE_OL == 'BC') THEN

         OPEN(11,FILE=trim(DATA_DRIVE)//"BCELAST.BIN",   &
                                             ACCESS="DIRECT",RECL=LRECL)
         INUNIT = 11
      ENDIF
      OPEN(12,FILE=trim(DATA_DRIVE)//"OLELAST.BIN",ACCESS="DIRECT",   &
                                            STATUS="UNKNOWN",RECL=LRECL)
      IREC = 0
      DO
         IREC = IREC + 1
         READ(INUNIT,REC=IREC,IOSTAT=IOS) DELETE,YEAR,VALUES
         IF(IOS /= 0) EXIT
         READ(10,1000,IOSTAT=IOS) RECLN
         IF(IOS == 0) THEN
            RECLN = trim(RECLN)//',,,,,,,,,,,,,'

            READ(RECLN,*,ERR=200) DELETE,YEAR,VALUES_14,   &
                                                    DESCRIPTION,VALUES_7
         ENDIF
         WRITE(12,REC=IREC) DELETE,YEAR,VALUES
      ENDDO
      CLOSE(10)
      CLOSE(12)
      IF(ELASTICITY_FILE_OL == 'BC') CLOSE(11)
      ELASTICITY_FILE_OL = 'OL'
      RETURN
!
      ENTRY RESET_ELASTICITY_FILE_OL
         ELASTICITY_FILE_OL = 'BC'
      RETURN
      ENTRY GET_FP_OL(R_ELASTICITY_FILE_OL)
         R_ELASTICITY_FILE_OL = ELASTICITY_FILE_OL
      RETURN
!

  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from fp_objt SIID132'
      call end_program(er_message)
 1000 FORMAT(A)
 1010 FORMAT('&',4A,I4)
      END


      FUNCTION PRICE_FEEDBACK_OBJECT()
      use spindriftlib
      use prod_arrays_dimensions
      use lamcom
      use retslcom
      USE SIZECOM
!
      INTEGER (kind=2) ::  I
      INTEGER (kind=2) ::  DELETE
      INTEGER (kind=2) ::  CLASS,MAX_YEARS_1,THIS_YEAR,MO,R_CLASS,   &
                STUDY_PERIOD=30
      INTEGER ::  IOS

      LOGICAL (kind=1) ::    PRICE_FEEDBACK,READ_PRICE_FEEDBACK_DATA,   &
                  PRICE_FEEDBACK_OBJECT,SAVE_PRICE_FEEDBACK,   &
                  CALCULATE_PRICE_FEEDBACK,VOID_LOGICAL,   &
                  CALCULATE_TOTAL_PRICE_ADJUSTER,YES_PRICE_FEEDBACK
      LOGICAL (kind=4) ::  FILE_EXISTS,R_SAVE_PRICE_FEEDBACK

      REAL (KIND=4) :: CLASS_TOTAL_ADJUSTER(SYSTEM_CLASS_NUM),   &
             CLASS_PRICE(SYSTEM_CLASS_NUM,MAX_SIMULATION_YEARS),   &
             LT_CLASS_ELASTICITY(SYSTEM_CLASS_NUM,MAX_SIMULATION_YEARS),   &
             ST_CLASS_ELASTICITY(SYSTEM_CLASS_NUM,MAX_SIMULATION_YEARS),   &
             Q_INPUT(SYSTEM_CLASS_NUM),Q_OUTPUT(SYSTEM_CLASS_NUM)

      REAL (kind=4) ::  CLASS_PRICE_ADJUSTER,CLASS_QUANTITY_ADJUSTER,   &
             QUANTITY_EXPONENT,CLASS_RATES,   &
             CLASS_FORECAST_ENERGY(2,12),CLASS_FORECAST_PEAK(2,12),   &
             SYSTEM_FORECAST_ENERGY(2,12),SYSTEM_FORECAST_PEAK(2,12),   &
             CAPACITY_PEAKS,PLANNING_PEAK,UPDATE_PEAK_AFTER_FEEDBACK,   &
             UPDATE_NET_PLANNING_PEAK,CLASS_ENERGY_MULT,   &
             CLASS_DEMAND_MULT,CLASS_ANNUAL_ENERGY,PASS_PEAK
      CHARACTER (len=2) ::  ELASTICITY_FILE_OL
      CHARACTER (len=256) ::  FILE_NAME,DATA_DRIVE,OUTPUT_DIRECTORY
!
! INFO FOR CONSUMER SURPLUS CALC
!
      INTEGER (kind=2) ::  YR,R_YR
      REAL ::   SURPLUS_DELTA,   &
            R_ADJUSTER,R_PRICE,   &
            DELTA_PRICE,SUM_DEMAND,R_SURPLUS
      LOGICAL (kind=1) ::  SAVE_SURPLUS_DELTA,   &

                  RETURN_CONSUMER_SURPLUS_VALUES
      SAVE SURPLUS_DELTA
!
         PRICE_FEEDBACK_OBJECT = .TRUE.
      RETURN

      ENTRY YES_PRICE_FEEDBACK(R_SAVE_PRICE_FEEDBACK)

         SAVE_PRICE_FEEDBACK = R_SAVE_PRICE_FEEDBACK
         YES_PRICE_FEEDBACK = SAVE_PRICE_FEEDBACK
      RETURN


      ENTRY READ_PRICE_FEEDBACK_DATA()


!
         IF(.NOT. SAVE_PRICE_FEEDBACK) RETURN
         DATA_DRIVE = OUTPUT_DIRECTORY()
         CALL GET_FP_OL(ELASTICITY_FILE_OL)
         FILE_NAME = trim(DATA_DRIVE)//ELASTICITY_FILE_OL//"ELAST.BIN"
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         IF(FILE_EXISTS) THEN

            OPEN(10,FILE=FILE_NAME,ACCESS="DIRECT",STATUS="OLD") ! ,RECL=88)
            I = 1
            IOS = 0
            MAX_YEARS_1 = MAX_SIMULATION_YEARS - 1
            DO WHILE ((I <= MAX_SIMULATION_YEARS) .AND. (IOS == 0))
               READ(10,REC=I,IOSTAT=IOS) DELETE,DELETE,   &
                  CLASS_PRICE(SYSTEM_CLASS_NUM,I),   &
                  ST_CLASS_ELASTICITY(SYSTEM_CLASS_NUM,I),   &
                  (CLASS_PRICE(CLASS,I),ST_CLASS_ELASTICITY(CLASS,I),   &
                                              CLASS=1,MAX_LOAD_CLASSES),   &
                  LT_CLASS_ELASTICITY(SYSTEM_CLASS_NUM,I),   &
                  (LT_CLASS_ELASTICITY(CLASS,I),   &


                                              CLASS=1,MAX_LOAD_CLASSES)
               I = I + 1
            ENDDO
            IF(I == 2 .AND. IOS /= 0) THEN
               DO CLASS = 1, SYSTEM_CLASS_NUM
                  Q_INPUT(CLASS) = 0.
                  DO I = 1, MAX_YEARS_1
                     CLASS_PRICE(CLASS,I) = 0.
                     ST_CLASS_ELASTICITY(CLASS,I) = 0.
                     LT_CLASS_ELASTICITY(CLASS,I) = 0.
                  ENDDO
               ENDDO
               SAVE_PRICE_FEEDBACK = .FALSE.
            ENDIF
            CLOSE(10)
         ELSE
            SAVE_PRICE_FEEDBACK = .FALSE.
         ENDIF
         READ_PRICE_FEEDBACK_DATA = .TRUE.
      RETURN



      ENTRY PRICE_FEEDBACK()

         PRICE_FEEDBACK = SAVE_PRICE_FEEDBACK
      RETURN


      ENTRY CALCULATE_TOTAL_PRICE_ADJUSTER(R_CLASS,THIS_YEAR, &
                                                            CLASS_RATES)


         CALCULATE_TOTAL_PRICE_ADJUSTER = SAVE_PRICE_FEEDBACK
         IF(.NOT. SAVE_PRICE_FEEDBACK) RETURN
         IF(THIS_YEAR > STUDY_PERIOD) RETURN
         CLASS = R_CLASS
         CLASS_TOTAL_ADJUSTER(CLASS) = 1.
         QUANTITY_EXPONENT = 0.
!

         IF(CLASS_PRICE(CLASS,THIS_YEAR) > 0. .AND. &
                                                CLASS_RATES > 0.) THEN
            IF(ST_CLASS_ELASTICITY(CLASS,THIS_YEAR)/=0) THEN
               CLASS_PRICE_ADJUSTER = (CLASS_RATES/ &
                    CLASS_PRICE(CLASS,THIS_YEAR))** &


                        ST_CLASS_ELASTICITY(CLASS,THIS_YEAR)
            ELSE
               CLASS_PRICE_ADJUSTER = 1.
            ENDIF

            IF(LT_CLASS_ELASTICITY(CLASS,THIS_YEAR) /= 0. .AND.   &
                                                     THIS_YEAR/= 1) THEN
               QUANTITY_EXPONENT = 1. -   &
                  ST_CLASS_ELASTICITY(CLASS,THIS_YEAR) /   &
                  LT_CLASS_ELASTICITY(CLASS,THIS_YEAR)
               CLASS_QUANTITY_ADJUSTER = (Q_OUTPUT(CLASS)/   &
                                     Q_INPUT(CLASS))**QUANTITY_EXPONENT
               CLASS_TOTAL_ADJUSTER(CLASS) =   &
                           CLASS_QUANTITY_ADJUSTER*CLASS_PRICE_ADJUSTER
            ELSEIF(ST_CLASS_ELASTICITY(CLASS,THIS_YEAR) /= 0.)  THEN
               CLASS_TOTAL_ADJUSTER(CLASS) = CLASS_PRICE_ADJUSTER
            ELSEIF(ST_CLASS_ELASTICITY(CLASS,THIS_YEAR) == 0. .AND.   &
                   LT_CLASS_ELASTICITY(CLASS,THIS_YEAR) == 0. .AND.   &
                                             THIS_YEAR /= 1) THEN
               CLASS_TOTAL_ADJUSTER(CLASS) =   &
                                         Q_OUTPUT(CLASS)/Q_INPUT(CLASS)
            ENDIF
         ENDIF
         IF(CLASS == SYSTEM_CLASS_NUM) VOID_LOGICAL =   &
                  SAVE_SURPLUS_DELTA(CLASS_TOTAL_ADJUSTER(CLASS),   &
                  CLASS_RATES - CLASS_PRICE(SYSTEM_CLASS_NUM,THIS_YEAR),   &
                  Q_INPUT(CLASS)*(1.0+CLASS_TOTAL_ADJUSTER(CLASS)) -   &
                  2.0*DELTA_CLASS_SALES_FROM_DSM(SYSTEM_CLASS_NUM),   &
                  SURPLUS_DELTA)

      RETURN


      ENTRY CALCULATE_PRICE_FEEDBACK(THIS_YEAR,   &
                                       CLASS_FORECAST_ENERGY,   &
                                       CLASS_ENERGY_MULT,   &
                                       CLASS_FORECAST_PEAK,   &
                                       CLASS_DEMAND_MULT,   &
                                       SYSTEM_FORECAST_ENERGY,   &
                                       SYSTEM_FORECAST_PEAK,   &
                                       CLASS_ANNUAL_ENERGY,   &
                                       R_CLASS)


         CALCULATE_PRICE_FEEDBACK = SAVE_PRICE_FEEDBACK
         IF(.NOT. SAVE_PRICE_FEEDBACK) RETURN
         CLASS = R_CLASS
         Q_INPUT(CLASS) = CLASS_ANNUAL_ENERGY
         IF(THIS_YEAR == 1) RETURN
!
         PLANNING_PEAK = CAPACITY_PEAKS(THIS_YEAR)
         IF(THIS_YEAR .LE. MAX_SIMULATION_YEARS)  THEN

            Q_OUTPUT(CLASS) = CLASS_TOTAL_ADJUSTER(CLASS) *&
                                                         Q_INPUT(CLASS)
            IF(CLASS_TOTAL_ADJUSTER(CLASS) /= 1.) THEN
               CLASS_ANNUAL_ENERGY = Q_OUTPUT(CLASS)
!
               IF(CLASS == SYSTEM_CLASS_NUM) THEN

                  PLANNING_PEAK = CLASS_TOTAL_ADJUSTER(CLASS) *   &
                                                           PLANNING_PEAK
                  PASS_PEAK =   &

                     UPDATE_PEAK_AFTER_FEEDBACK(THIS_YEAR,PLANNING_PEAK)
                  PASS_PEAK = UPDATE_NET_PLANNING_PEAK(THIS_YEAR)
               ENDIF
!
               DO MO = 1, 12
!
                  IF(CLASS < SYSTEM_CLASS_NUM) THEN
!

                     SYSTEM_FORECAST_ENERGY(1,MO) =   &
                        SYSTEM_FORECAST_ENERGY(1,MO) +   &
                                      CLASS_FORECAST_ENERGY(1,MO) *   &
                                      CLASS_ENERGY_MULT*   &
                                 (CLASS_TOTAL_ADJUSTER(CLASS) - 1.)
                     SYSTEM_FORECAST_ENERGY(2,MO) =   &
                        SYSTEM_FORECAST_ENERGY(2,MO) +   &
                                      CLASS_FORECAST_ENERGY(2,MO) *   &
                                      CLASS_ENERGY_MULT*   &
                                 (CLASS_TOTAL_ADJUSTER(CLASS) - 1.)
                     SYSTEM_FORECAST_PEAK(1,MO) =   &
                        SYSTEM_FORECAST_PEAK(1,MO) +   &
                                      CLASS_FORECAST_PEAK(1,MO) *   &
                                      CLASS_DEMAND_MULT*   &
                                 (CLASS_TOTAL_ADJUSTER(CLASS) - 1.)
                     SYSTEM_FORECAST_PEAK(2,MO) =   &
                        SYSTEM_FORECAST_PEAK(2,MO) +   &
                                      CLASS_FORECAST_PEAK(2,MO) *   &
                                      CLASS_DEMAND_MULT*   &
                                 (CLASS_TOTAL_ADJUSTER(CLASS) - 1.)
                  ENDIF
!
                  CLASS_FORECAST_ENERGY(1,MO) =   &
                            CLASS_TOTAL_ADJUSTER(CLASS) *   &
                                         CLASS_FORECAST_ENERGY(1,MO)
                  CLASS_FORECAST_ENERGY(2,MO) =   &
                           CLASS_TOTAL_ADJUSTER(CLASS) *   &
                                         CLASS_FORECAST_ENERGY(2,MO)
                  CLASS_FORECAST_PEAK(1,MO) =   &
                           CLASS_TOTAL_ADJUSTER(CLASS) *   &
                                           CLASS_FORECAST_PEAK(1,MO)
                  CLASS_FORECAST_PEAK(2,MO) =   &
                           CLASS_TOTAL_ADJUSTER(CLASS) *   &


                                           CLASS_FORECAST_PEAK(2,MO)
!
               ENDDO ! MONTH COUNTER
            ENDIF ! PRICE FEEDBACK IMPACT
         ENDIF ! CHECK FOR LAST FORECAST YEAR
      RETURN ! CALCULATE SYSTEM PRICE FEEDBACK
!
!

      ENTRY RETURN_CONSUMER_SURPLUS_VALUES(R_SURPLUS,R_ADJUSTER,   &

                                                     R_PRICE,R_YR)
         YR = R_YR
         IF(SAVE_PRICE_FEEDBACK) THEN
            R_PRICE = CLASS_PRICE(SYSTEM_CLASS_NUM,MIN(YR,STUDY_PERIOD))
            R_SURPLUS= SURPLUS_DELTA
            R_ADJUSTER = CLASS_TOTAL_ADJUSTER(SYSTEM_CLASS_NUM)
         ELSE
            R_PRICE = 0.
            R_SURPLUS = 0.
            R_ADJUSTER = 1.
         ENDIF
         RETURN_CONSUMER_SURPLUS_VALUES = SAVE_PRICE_FEEDBACK
      RETURN
      END ! PRICE FEEDBACK OBJECT



      FUNCTION SAVE_SURPLUS_DELTA(R_ADJUSTER,DELTA_PRICE,   &
                                  SUM_DEMAND,SURPLUS_DELTA)

!
      LOGICAL (kind=1) ::  SAVE_SURPLUS_DELTA
      REAL ::  R_ADJUSTER,DELTA_PRICE,SUM_DEMAND,SURPLUS_DELTA


         SURPLUS_DELTA = -.5 * DELTA_PRICE * SUM_DEMAND/1000000.
         SAVE_SURPLUS_DELTA = .TRUE.
      RETURN
      END
