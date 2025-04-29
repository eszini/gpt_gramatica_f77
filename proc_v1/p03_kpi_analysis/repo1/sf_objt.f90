
!     SF_OBJT.FOR
!     Created: 10/24/02 11:45:48 AM
!     Author : msg
!     Last change: gt 5/28/2013 2:40:03 PM



      SUBROUTINE SF_OBJECT
      use end_routine, only: end_program, er_message

!
      USE spindriftlib
      USE prod_arrays_dimensions
      USE SIZECOM
      USE iostatmsg
      use mod_base_year
      

      INTEGER(kind=1) :: FORECAST_GROWTH_YEARS=AVAIL_DATA_YEARS
      CHARACTER(len=1) :: EXTENSION_PERIOD_GROWTH_SWITCH='X'
      INTEGER(kind=2) :: INUNIT,IREC,DELETE,LRECL=256
      INTEGER :: IOS
      CHARACTER(len=5) :: SYSFRC,OVERLAY_FAMILY_NAME
      CHARACTER(len=256) :: FILE_NAME
      CHARACTER(len=256) :: OUTPUT_DIRECTORY
      CHARACTER(len=256) :: BASE_FILE_DIRECTORY
      LOGICAL(kind=1) :: SYSTEM_FILE_EXISTS=.FALSE.,R_SYSTEM_FILE_EXISTS
      CHARACTER(len=152) :: MESSAGE
      LOGICAL(kind=4) :: FILE_EXISTS
      INTEGER(kind=4) :: UNIT_NUMBER
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER(len=1024) :: RECLN
! DECLARATION FOR SYSTEM FORECAST VARIABLES
      INTEGER(kind=2) :: YEAR
      REAL :: SYS_FC_DATA(4,12),MONTHLY_SYSTEM_PEAKS(12,30),R_PEAK
      REAL :: MARKET_RATES_CUSTOMERS(4) !3/26/95 MARKET PRICES AT SYSTEM LEVEL MSG
      CHARACTER(len=50) :: COMMENT
      CHARACTER(len=15) :: FILE_TYPE='System Forecast'
      CHARACTER(len=2) :: SYSFRC_OL='BC',R_SYSFRC_OL
!
      LOGICAL(kind=1) :: LAHEY_LF95
      CHARACTER(len=30) :: SCREEN_OUTPUT
      INTEGER(kind=2) :: SF_YEAR,STUDY_BASE_YEAR,MO,MO_OFFSET,R_MO,R_YR
!
      SAVE STUDY_BASE_YEAR
!

!
!          ROUTINE TO CONVERT METAFILE FILES TO DIRECT-ACESS BINARY
!          COPYRIGHT (C) 1983, 84, 85  M.S. GERBER & ASSOCIATES, INC.
!

!
! CONVERT THE SYSTEM-FORECAST FILE
!
!

      ENTRY SF_MAKEBIN

      STUDY_BASE_YEAR = get_BASE_YEAR()
      SF_YEAR = 0
      FILE_NAME = trim(BASE_FILE_DIRECTORY())//   &
                                         "SFB"//trim(SYSFRC())//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      IF(EXTENSION_PERIOD_GROWTH_SWITCH == 'F')   &
                                FORECAST_GROWTH_YEARS = AVAIL_DATA_YEARS
      SYSTEM_FILE_EXISTS = FILE_EXISTS
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//SYSFRC()
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
            CALL MG_LOCATE_WRITE(16,30,SYSFRC(),ALL_VERSIONS,0)
         ENDIF
         SYS_FC_DATA = 0.
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCSYSFC.BIN",   &
                            ACCESS="DIRECT",STATUS="REPLACE",RECL=LRECL)
         IREC = 0
         READ(10,*) DELETE
         DO WHILE (IREC < FORECAST_GROWTH_YEARS)
            IREC = IREC + 1
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(IOS == 0) THEN
               SF_YEAR = SF_YEAR + 1
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=200) DELETE,YEAR,SYS_FC_DATA,COMMENT,   &
                                     MARKET_RATES_CUSTOMERS
            ENDIF
!
            WRITE(11,REC=IREC) SF_YEAR,SYS_FC_DATA,   &
                                                  MARKET_RATES_CUSTOMERS
            CALL CALCULATE_SYSTEM_PEAKS(SYS_FC_DATA,SF_YEAR)
         ENDDO
         CLOSE(10)
      ELSE
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCSYSFC.BIN",   &
                            ACCESS="DIRECT",STATUS="REPLACE",RECL=LRECL)
      ENDIF

      CLOSE(11)
      RETURN


!
!          ROUTINE TO CREATE OVERLAY FILES
!          COPYRIGHT (C) 1984-88  M.S. GERBER & ASSOCIATES, INC.
!          COPYRIGHT (C) 1991-92  M.S. GERBER & ASSOCIATES, INC.
!

!
! OVERLAY THE SYSTEM-FORECAST FILE

      ENTRY SF_MAKEOVL(OVERLAY_FAMILY_NAME)

!
      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      FILE_NAME=trim(OUTPUT_DIRECTORY())//"SFO"//   &
                                     trim(OVERLAY_FAMILY_NAME)//".DAT"
      OPEN(10,FILE=FILE_NAME)
      READ(10,*) DELETE
      SF_YEAR = 0
      INUNIT = 12
      IF(SYSFRC_OL == 'BC') THEN
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCSYSFC.BIN",   &
                                             ACCESS="DIRECT",RECL=LRECL)
         INUNIT = 11
      ENDIF
!
      FILE_NAME = trim(OUTPUT_DIRECTORY())//"OLSYSFC.BIN"
      OPEN(12,FILE=FILE_NAME,ACCESS="DIRECT",   &
                                 STATUS="UNKNOWN",RECL=LRECL,IOSTAT=IOS)
      IF(IOS /= 0) THEN
         CALL iostatmsg_unit(IOS,MESSAGE)
         WRITE(4,*) trim(MESSAGE)
         er_message='134 in SF_OFJT.FOR see WARNINGS messages'
         call end_program(er_message)
      ENDIF
      IREC = 0
      DELETE = 1
      DO
         IREC = IREC + 1
         READ(INUNIT,REC=IREC,IOSTAT=IOS) YEAR,SYS_FC_DATA,   &
                                          MARKET_RATES_CUSTOMERS
         IF(IOS /= 0) EXIT
         READ(10,1000,IOSTAT=IOS) RECLN
         IF(IOS == 0) THEN
            SF_YEAR = SF_YEAR + 1
            RECLN = trim(RECLN)//',,,,,,,,,,,,,'
            READ(RECLN,*,ERR=200) DELETE,YEAR,SYS_FC_DATA,COMMENT,   &
                                                  MARKET_RATES_CUSTOMERS
         ENDIF
         IF(SF_YEAR /= YEAR - STUDY_BASE_YEAR) THEN
            WRITE(4,*) 'The Overlay System Forecast File  ',   &
                                           OVERLAY_FAMILY_NAME,','
            WRITE(4,*) 'Record',SF_YEAR,'  and Year',YEAR
            WRITE(4,*) 'is inconsistent with the Base Year',   &
                                                    STUDY_BASE_YEAR
            WRITE(4,*) 'in Set Parameters. Either reset Base '
            WRITE(4,*) 'Year in Set Parameters or the Year in'
            WRITE(4,*) 'System Forecast.'
            WRITE(4,*) ' '
         ENDIF
         WRITE(12,REC=IREC) SF_YEAR,SYS_FC_DATA,MARKET_RATES_CUSTOMERS
         CALL CALCULATE_SYSTEM_PEAKS(SYS_FC_DATA,SF_YEAR)
      ENDDO
      CLOSE(10)
      CLOSE(12)
      IF(SYSFRC_OL == 'BC') CLOSE(11)
      SYSFRC_OL = 'OL'
      RETURN


      ENTRY SF_GET_PEAKS

!
      OPEN(10,FILE=trim(OUTPUT_DIRECTORY())//SYSFRC_OL//   &
                                 "SYSFC.BIN",ACCESS="DIRECT",RECL=LRECL)
      IREC = 0
      MONTHLY_SYSTEM_PEAKS = 0.0
      DO
         IREC = IREC + 1
         READ(10,REC=IREC,IOSTAT=IOS) YEAR,SYS_FC_DATA
         IF(IOS /= 0) EXIT
         CALL CALCULATE_SYSTEM_PEAKS(SYS_FC_DATA,YEAR)
         DO MO = 1, 12
            MONTHLY_SYSTEM_PEAKS(MO,YEAR) = SYS_FC_DATA(2,MO)
         END DO
      ENDDO
      CLOSE(10)
      RETURN

  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from SF_OBJT SIID267'
      call end_program(er_message)


      ENTRY SF_MONTHLY_PEAKS(R_MO,R_YR,R_PEAK)

        R_PEAK = MONTHLY_SYSTEM_PEAKS(R_MO,R_YR)
      RETURN


      ENTRY RESET_SYSFRC_OL

         SYSFRC_OL = 'BC'
      RETURN


      ENTRY RETURN_SYSFRC_OL(R_SYSFRC_OL)

         R_SYSFRC_OL = SYSFRC_OL
      RETURN


      ENTRY SYSTEM_FORECAST_FILE_EXISTS(R_SYSTEM_FILE_EXISTS)

         R_SYSTEM_FILE_EXISTS = SYSTEM_FILE_EXISTS
      RETURN
!
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END


      FUNCTION STORE_ANNUAL_PEAK_AND_MONTH(R_MO,R_YR,R_PEAK)

!
      USE spindriftlib
      USE prod_arrays_dimensions
      USE SIZECOM
      use globecom
      use end_routine

      SAVE
      integer(kind=2) :: imm_year
      INTEGER(kind=2) :: YR,R_YR,R_MO
      REAL :: PEAK,STORE_ANNUAL_PEAK_AND_MONTH,R_PEAK
      REAL :: PLANNING_PEAK(MAX_PLANNING_YEARS),CAPACITY_PEAKS,   &
            SAVE_LM_RESOURCE_ADJUSTMENT,   &
            SAVE_LM_PLANNED_ADJUSTMENT,   &
            LM_AS_RESOURCE(MAX_PLANNING_YEARS),S_LM_AS_RESOURCE,   &
            LM_AS_PLANNED_CAPACITY(MAX_PLANNING_YEARS),   &
            S_LM_AS_PLANNED_CAPACITY,   &
            GET_INPUT_SYSTEM_PEAK,   &
            LM_RESOURCE_LOAD_CREDIT,LM_PLANNED_LOAD_CREDIT,   &
            PLANNING_PEAK_AFTER_FEEDBACK(MAX_PLANNING_YEARS),   &
            S_INIT_PLANNING_PEAK(3),INIT_PLANNING_PEAK(3),   &
            X1,X2,PEAK_GROWTH_RATE_FOR,RETURN_PEAK,   &
            RETURN_PEAK_ADJUSTMENT,ADJUSTMENT_CAPACITY,   &
            PLANNING_PEAK_GROWTH_RATE
      REAL :: SAVE_DSM_ADJUSTED_PLANNING_PEAK
      REAL ::  NET_PLANNING_PEAK,   &
            SAVE_NET_PLANNING_PEAK(MAX_PLANNING_YEARS),   &
            ADJUSTMENT_PEAK,CONTRACT_PEAK_ADJUSTMENT,   &
            PEAK_ADJ_OFF_SYSTEM_SALES,UPDATE_NET_PLANNING_PEAK ,   &
            UPDATE_PEAK_AFTER_FEEDBACK,   &
            CL_PLANNING_LOAD_REDUCTION
      INTEGER(kind=2) :: PLANNING_PEAK_MONTH_IS(MAX_PLANNING_YEARS)= 7
      INTEGER(kind=2) :: PEAK_MONTH,UPDATE_PLANNING_PEAK_MONTH
      LOGICAL(kind=1) :: STORE_INIT_PLANNING_PEAKS
      CHARACTER(len=8) :: PEAK_GROWTH_RATE_METHOD
      CHARACTER(len=1) :: CHR_PEAK_GROWTH_RATE_METHOD

         YR = R_YR
         LM_AS_RESOURCE(YR) = 0.
         LM_AS_PLANNED_CAPACITY(YR) = 0.
         PLANNING_PEAK(YR) = R_PEAK
         PLANNING_PEAK_AFTER_FEEDBACK(YR) = R_PEAK
         SAVE_NET_PLANNING_PEAK(YR) = R_PEAK
         PLANNING_PEAK_MONTH_IS(YR) = R_MO

            
         STORE_ANNUAL_PEAK_AND_MONTH = PLANNING_PEAK(YR)
      RETURN


      ENTRY CAPACITY_PEAKS(R_YR)
         if(get_globecom_study_period()==0) then
            call end_program("sf_objt:0005 - get_globecom_study_period()==0")
         end if
         
         IF(R_YR > get_globecom_study_period()) THEN
            CAPACITY_PEAKS = PLANNING_PEAK_AFTER_FEEDBACK(get_globecom_study_period())
         ELSE
            CAPACITY_PEAKS = PLANNING_PEAK_AFTER_FEEDBACK(R_YR)
         ENDIF
      RETURN


      ENTRY UPDATE_PEAK_AFTER_FEEDBACK(R_YR,R_PEAK)

         PLANNING_PEAK_AFTER_FEEDBACK(R_YR) = R_PEAK
         UPDATE_PEAK_AFTER_FEEDBACK = R_PEAK
      RETURN


      ENTRY PEAK_MONTH(R_YR)
         if(r_yr==0) then
            call end_program("sf_objt:0002 - R_YR argument to " // &
            "sf_objt:peak_month is zero.")
         end if
         if(get_globecom_study_period()==0) then
            call end_program("sf_objt:0004 - get_globecom_study_period() is zero.")
         end if
         
         IF(R_YR > get_globecom_study_period()) THEN
            PEAK_MONTH = PLANNING_PEAK_MONTH_IS(get_globecom_study_period())
         ELSE
            PEAK_MONTH = PLANNING_PEAK_MONTH_IS(R_YR)
         ENDIF
      RETURN


      ENTRY UPDATE_PLANNING_PEAK_MONTH(R_YR,R_MO)

         PLANNING_PEAK_MONTH_IS(R_YR) = R_MO
         UPDATE_PLANNING_PEAK_MONTH = R_MO
      RETURN


      ENTRY SAVE_LM_PLANNED_ADJUSTMENT(R_YR,S_LM_AS_PLANNED_CAPACITY)

         YR = R_YR
         LM_AS_PLANNED_CAPACITY(YR) = LM_AS_PLANNED_CAPACITY(YR) +   &
                                            S_LM_AS_PLANNED_CAPACITY
         IF(YR > get_globecom_study_period()) THEN
            SAVE_LM_PLANNED_ADJUSTMENT =   &
                              PLANNING_PEAK_AFTER_FEEDBACK(get_globecom_study_period())   &
                              + LM_AS_PLANNED_CAPACITY(YR)
         ELSE
            SAVE_LM_PLANNED_ADJUSTMENT =   &
                                        PLANNING_PEAK_AFTER_FEEDBACK(YR)   &
                                        + LM_AS_PLANNED_CAPACITY(YR)
         ENDIF
      RETURN


      ENTRY LM_PLANNED_LOAD_CREDIT(R_YR)

         LM_PLANNED_LOAD_CREDIT = LM_AS_PLANNED_CAPACITY(R_YR)
      RETURN


      ENTRY SAVE_LM_RESOURCE_ADJUSTMENT(R_YR,S_LM_AS_RESOURCE)

         YR = R_YR
         LM_AS_RESOURCE(YR) = LM_AS_RESOURCE(YR) + S_LM_AS_RESOURCE
         IF(YR > get_globecom_study_period()) THEN
            SAVE_LM_PLANNED_ADJUSTMENT =   &
                              PLANNING_PEAK_AFTER_FEEDBACK(get_globecom_study_period())
         ELSE
            SAVE_LM_PLANNED_ADJUSTMENT =   &
                                        PLANNING_PEAK_AFTER_FEEDBACK(YR)
         ENDIF
         SAVE_LM_RESOURCE_ADJUSTMENT = SAVE_LM_PLANNED_ADJUSTMENT +   &
                  LM_AS_RESOURCE(YR) + LM_AS_PLANNED_CAPACITY(YR)
      RETURN


      ENTRY LM_RESOURCE_LOAD_CREDIT(R_YR)

         LM_RESOURCE_LOAD_CREDIT = LM_AS_RESOURCE(R_YR)
      RETURN


      ENTRY UPDATE_NET_PLANNING_PEAK(R_YR)
         if(r_yr==0) then
            call end_program ("sf_objt:0001 - R_YR argument must be " // &
              "nonzero.")
         end if
         if(r_yr==5) then
            r_yr=r_yr ! Debugstop
         end if
         
         YR = R_YR
         IF(YR > get_globecom_study_period()) THEN
            SAVE_NET_PLANNING_PEAK(YR) =   &
                              PLANNING_PEAK_AFTER_FEEDBACK(get_globecom_study_period())
         ELSE
            SAVE_NET_PLANNING_PEAK(YR) =   &
                                        PLANNING_PEAK_AFTER_FEEDBACK(YR)
         ENDIF
         
         imm_year=int(YR,2)
         SAVE_NET_PLANNING_PEAK(YR) =   &
                                 MAX(1.,SAVE_NET_PLANNING_PEAK(YR)   &
                                 + LM_AS_PLANNED_CAPACITY(YR)   &
                                 + LM_AS_RESOURCE(YR)   &
                                 + ADJUSTMENT_PEAK(YR)   &
                                 - CONTRACT_PEAK_ADJUSTMENT(YR)   &
                           - CL_PLANNING_LOAD_REDUCTION(imm_year)   &
                                 + PEAK_ADJ_OFF_SYSTEM_SALES(YR))
         UPDATE_NET_PLANNING_PEAK = SAVE_NET_PLANNING_PEAK(YR)
      RETURN


      ENTRY GET_INPUT_SYSTEM_PEAK(R_YR)

         IF(R_YR > get_globecom_study_period()) THEN
            GET_INPUT_SYSTEM_PEAK = PLANNING_PEAK(get_globecom_study_period())
         ELSE
            GET_INPUT_SYSTEM_PEAK = PLANNING_PEAK(R_YR)
         ENDIF
      RETURN


      ENTRY NET_PLANNING_PEAK(R_YR)

         IF(R_YR > get_globecom_study_period()) THEN
            NET_PLANNING_PEAK = SAVE_NET_PLANNING_PEAK(get_globecom_study_period())
         ELSE
            NET_PLANNING_PEAK = SAVE_NET_PLANNING_PEAK(R_YR)
         ENDIF
      RETURN


      ENTRY STORE_INIT_PLANNING_PEAKS(S_INIT_PLANNING_PEAK)

         INIT_PLANNING_PEAK(1) = S_INIT_PLANNING_PEAK(1)
         INIT_PLANNING_PEAK(2) = S_INIT_PLANNING_PEAK(2)
         INIT_PLANNING_PEAK(3) = S_INIT_PLANNING_PEAK(3)
         STORE_INIT_PLANNING_PEAKS = .TRUE.
      RETURN


      ENTRY PEAK_GROWTH_RATE_FOR(R_YR,RETURN_PEAK,   &
                                                 RETURN_PEAK_ADJUSTMENT)

         IF(R_YR > get_globecom_study_period()) THEN
            YR = get_globecom_study_period()
         ELSE
            YR = R_YR
         ENDIF
         RETURN_PEAK_ADJUSTMENT = ADJUSTMENT_CAPACITY(R_YR)
         IF(YR > 1) THEN
            RETURN_PEAK = SAVE_NET_PLANNING_PEAK(YR-1)
         ELSE
            RETURN_PEAK = INIT_PLANNING_PEAK(1)
         ENDIF
         CHR_PEAK_GROWTH_RATE_METHOD = PEAK_GROWTH_RATE_METHOD()
         IF(CHR_PEAK_GROWTH_RATE_METHOD == 'R') THEN
            PEAK_GROWTH_RATE_FOR = PLANNING_PEAK_GROWTH_RATE(R_YR)
            RETURN
         ENDIF
         IF(YR > 3) THEN
            PEAK_GROWTH_RATE_FOR = SQRT(SAVE_NET_PLANNING_PEAK(YR-1)/   &
                                           SAVE_NET_PLANNING_PEAK(YR-3))
         ELSEIF(YR == 3) THEN
            PEAK_GROWTH_RATE_FOR = SQRT(SAVE_NET_PLANNING_PEAK(YR-1)/   &
                                                  INIT_PLANNING_PEAK(1))
         ELSEIF(YR == 2) THEN
            IF(INIT_PLANNING_PEAK(2) <= 15.) THEN
               PEAK_GROWTH_RATE_FOR = 1. + INIT_PLANNING_PEAK(2)/100.
            ELSE
               PEAK_GROWTH_RATE_FOR = SQRT(SAVE_NET_PLANNING_PEAK(YR-1)/   &
                                                  INIT_PLANNING_PEAK(2))
            ENDIF
         ELSEIF(YR == 1) THEN
            IF(INIT_PLANNING_PEAK(1) == -9999. .OR.   &
                        INIT_PLANNING_PEAK(2) == -9999. .OR.   &
                                   INIT_PLANNING_PEAK(3) == -9999.) THEN
               INIT_PLANNING_PEAK(1) = SAVE_NET_PLANNING_PEAK(1)
               INIT_PLANNING_PEAK(2) = SAVE_NET_PLANNING_PEAK(1)
               INIT_PLANNING_PEAK(3) = SAVE_NET_PLANNING_PEAK(1)
            ENDIF
            IF(INIT_PLANNING_PEAK(3) <= 15.) THEN
               PEAK_GROWTH_RATE_FOR = 1. + INIT_PLANNING_PEAK(3)/100.
            ELSE
               PEAK_GROWTH_RATE_FOR = SQRT(INIT_PLANNING_PEAK(1)/   &
                                               INIT_PLANNING_PEAK(3))
            ENDIF
         ENDIF
      RETURN


      ENTRY SAVE_DSM_ADJUSTED_PLANNING_PEAK(R_YR,R_PEAK)

!
         SAVE_DSM_ADJUSTED_PLANNING_PEAK = R_PEAK
         SAVE_NET_PLANNING_PEAK(R_YR) = R_PEAK
      RETURN
      END

!
! FUNCTION FOR READING THE SYSTEM LOAD FILE BY YEAR AND PASSING IT TO
! THE MODULE THAT CALCULATES 8760 LOAD POINTS
!

      SUBROUTINE GET_SYSTEM_LOAD_FORECAST(YEAR,   &
                                          SYSTEM_ENRGY,   &
                                          SYSTEM_PEAKS)

!
      USE spindriftlib
      USE prod_arrays_dimensions

      REAL :: SYSTEM_ENRGY(12,2),SYSTEM_PEAKS(12,2)
      REAL :: VOID,SET_PEAK_SPLIT,SPLIT_ENERGY,SET_ENERGY_SPLIT,   &
           SPLIT_PEAKS
      INTEGER(kind=2) :: YEAR,CURRENT_YEAR,MO,   &
                EXTENSION_PERIOD_START
      INTEGER ::  IOS
      CHARACTER(len=256) :: OUTPUT_DIRECTORY
      CHARACTER(len=2) :: SYSFRC_OL
!
      IF(YEAR == 1) THEN
         CALL RETURN_SYSFRC_OL(SYSFRC_OL)
         OPEN(1222,FILE=trim(OUTPUT_DIRECTORY())//   &
                        SYSFRC_OL//'SYSFC.BIN',ACCESS='DIRECT',RECL=256)
         READ(1222,REC=1,IOSTAT=IOS) CURRENT_YEAR,   &
                                  (SYSTEM_ENRGY(1,MO),   &
                                   SYSTEM_PEAKS(1,MO),   &
                                   SYSTEM_ENRGY(2,MO),   &
                                   SYSTEM_PEAKS(2,MO),MO = 1,12)
         DO MO = 1, 12
            IF(SYSTEM_ENRGY(2,MO) == 0.) THEN
               SYSTEM_ENRGY(2,MO) = SPLIT_ENERGY(MO,SYSTEM_ENRGY(1,MO))
            ELSE
               VOID = SET_ENERGY_SPLIT(MO,SYSTEM_ENRGY(1,MO))
            ENDIF
            IF(SYSTEM_PEAKS(2,MO) == 0.) THEN
               SYSTEM_PEAKS(2,MO) = SPLIT_PEAKS(MO,SYSTEM_PEAKS(1,MO))
            ELSE
               VOID = SET_PEAK_SPLIT(MO,SYSTEM_PEAKS(1,MO))
            ENDIF
         ENDDO
         RETURN
      ELSEIF (YEAR >= EXTENSION_PERIOD_START()) THEN
         SYSTEM_ENRGY = 0.
         SYSTEM_PEAKS = 0.
      ELSE
         READ(1222,REC=YEAR,IOSTAT=IOS) CURRENT_YEAR,   &
                                  (SYSTEM_ENRGY(1,MO),   &
                                   SYSTEM_PEAKS(1,MO),   &
                                   SYSTEM_ENRGY(2,MO),   &
                                   SYSTEM_PEAKS(2,MO),MO = 1,12)
      ENDIF
      RETURN
      END

!
! ENERGY SPLIT INFORMATION
!

      FUNCTION ENERGY_AND_PEAK_SPLIT()

      REAL :: ENERGY_AND_PEAK_SPLIT,SPLIT_ENERGY,   &
           SET_ENERGY_SPLIT,SPLIT_PEAKS,SET_PEAK_SPLIT
      INTEGER(kind=2) :: MO
      REAL :: VALUE_TO_SPLIT,SET_SPLIT_VALUES(2)
      REAL :: ENERGY_SPLIT_RATIO(12)=0.,   &
           PEAK_SPLIT_RATIO(12)=0.
!
      ENERGY_AND_PEAK_SPLIT = 1.
      RETURN


      ENTRY SPLIT_ENERGY(MO,VALUE_TO_SPLIT)

         SPLIT_ENERGY = VALUE_TO_SPLIT * ENERGY_SPLIT_RATIO(MO)
      RETURN


      ENTRY SET_ENERGY_SPLIT(MO,SET_SPLIT_VALUES)

         ENERGY_SPLIT_RATIO(MO) = SET_SPLIT_VALUES(2)/   &
                       (SET_SPLIT_VALUES(1) + SET_SPLIT_VALUES(2))
         SET_ENERGY_SPLIT = ENERGY_SPLIT_RATIO(MO)
      RETURN


      ENTRY SPLIT_PEAKS(MO,VALUE_TO_SPLIT)

         SPLIT_PEAKS = VALUE_TO_SPLIT * PEAK_SPLIT_RATIO(MO)
      RETURN


      ENTRY SET_PEAK_SPLIT(MO,SET_SPLIT_VALUES)

         PEAK_SPLIT_RATIO(MO) = SET_SPLIT_VALUES(2)/SET_SPLIT_VALUES(1)
         SET_PEAK_SPLIT = PEAK_SPLIT_RATIO(MO)
      RETURN
      END

