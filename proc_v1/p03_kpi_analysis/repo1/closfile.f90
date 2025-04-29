 
!     CLOSFILE.FOR
!     Copyright(c)  2000
!
!     Created: 8/26/2003 4:19:58 PM
!     Author : MARK S GERBER
!     Last change: MSG 1/20/2011 1:13:54 PM
 

 
!
!     SUBROUTINE TO RECORD EXECUTION TIME AND CLOSE OUTPUT FILES
!          COPYRIGHT (C) 1986  M.S. GERBER & ASSOCIATES, INC.
!
 
!
      SUBROUTINE CLOSE_FILES(SCENAME_arg, END_POINTS,LAST_LOOP_YEAR)
      use dsex_data
      use dreptcom
      use run_period_data

      use production
      use mod_base_year
      implicit none
      character (len=5) :: SCENAME_arg
      CHARACTER (len=152) ::  ERR_MESSAGE
      CHARACTER (len=9) ::  CLOCK_TIME,TOTAL_TIME_STR,OVERHEAD_TIME_STR, &
                  EXECUTION_TIME_STR,INDEXING_TIME_STR
      CHARACTER (len=40) ::  TITLE
      CHARACTER (len=256) ::  OUTPUT_DIRECTORY
      INTEGER (kind=2) ::  END_POINTS,PROCMETH, &
                LAST_LOOP_YEAR,EXTENSION_YEARS
      INTEGER ::  IOS
      INTEGER (kind=4) ::  SIMULATION_YEARS
      INTEGER (kind=4) ::  TOTAL_TIME
      INTEGER (kind=4) ::  OVERHEAD_TIME,EXECUTION_TIME,INDEXING_TIME
!     INTEGER*4 HOURS,MINUTES,SECONDS,TOTIME,DUMMY
      CHARACTER (len=20) ::  PC_METHOD_TITLE(5),PERIODS_TITLE
      CHARACTER (len=80) ::  PRODUCTION_METHOD_TITLE
      CHARACTER (len=256) ::  SIM_FILE_NAME
      CHARACTER (len=256) ::  LC_SIM_FILE_NAME,DATE_TIME*18, &
                    PROJECT_NAME
      LOGICAL (kind=1) ::  COAL_MODEL_ONLY,GAS_MODEL_ONLY
!
      IF(END_POINTS == 0) THEN
         CLOSE(38,STATUS='DELETE')
         CLOSE(9,STATUS='DELETE')
         RETURN
      ENDIF
!
      PC_METHOD_TITLE(1) = 'Booth'
      PC_METHOD_TITLE(2) = 'DRate'
      PC_METHOD_TITLE(3) = 'Booth/DRate'
      PC_METHOD_TITLE(4) = 'Energy Balance'
      PC_METHOD_TITLE(5) = 'Facet'
      PERIODS_TITLE = 'Monthly'
      IF(get_PRODUCTION_PERIODS_in() == 1) THEN
         PERIODS_TITLE = 'Annual'
      ELSE IF(get_PRODUCTION_PERIODS_in() /= 12) THEN
         PERIODS_TITLE = 'Seasonal'
      ENDIF

! CLOSE RESULTS FILE THEN UPDATE END POINTS

      CLOSE(38)
      OPEN(38,FILE=trim(OUTPUT_DIRECTORY())//"BIP"//  &
         trim(SCENAME_arg)//".BIN",ACCESS="TRANSPARENT",STATUS="OLD")
      WRITE(38,REC=4) END_POINTS
      WRITE(38,REC=8) LAST_LOOP_YEAR
      CLOSE(38)
! REPORTS FILES
!
! OVERHEAD TIME
!
      CALL RETURN_TOTAL_TIME(TOTAL_TIME,EXECUTION_TIME,OVERHEAD_TIME, &
                             INDEXING_TIME)
      WRITE(9,"('1',A//)") BANNER_LINE
      WRITE(9,1010) " Summary of Execution Specifications"
      WRITE(9,1010) '0',UTILITY_NAME
      IF(trim(TITLE()) .NE. ' ') WRITE(9,1010) ' Title: ',TITLE()
      WRITE(9,1010) ' Project Name: ',trim(PROJECT_NAME())
      WRITE(9,1010) ' Study Name: ',SCENAME_arg
      WRITE(9,1010) ' Base File Family Name: ',  &
                                            trim(RPT_BASE_FAMILY_NAME)
      WRITE(9,1010) ' Study Date: '//RUN_DATE//' '//RUN_TIME
      WRITE(9,1020) ' Study Period: ',get_BASE_YEAR()+1,'-',get_endyr()
      IF(COAL_MODEL_ONLY()) THEN
         WRITE(9,1010) ' Operating Model: Coal LP Model Only'
      ELSEIF(GAS_MODEL_ONLY()) THEN
         WRITE(9,1010) ' Operating Model: Gas LP Model Only'
      ELSE
         WRITE(9,1010) ' Operating Model: Production Model'
         IF(EXTENSION_YEARS() > 0)                                       &
                   WRITE(9,1020) ' Extension Period to: ',LAST_LOOP_YEAR
         IF(PROCMETH() > 0 .AND. PROCMETH() < 5) THEN
         WRITE(9,1010) ' Production Cost Method: ',                      &
                                       trim(PRODUCTION_METHOD_TITLE())
         ENDIF
         WRITE(9,1010) ' Production Periods: ',PERIODS_TITLE
      ENDIF
      WRITE(9,1020) ' Endpoints: ',END_POINTS
      SIMULATION_YEARS = INT(LAST_LOOP_YEAR - get_BASE_YEAR())
      IF(END_POINTS > 1) THEN
         WRITE(9,"(A,I6)") ' Simulation Years per Endpoint: ',           &
                                                        SIMULATION_YEARS
         SIMULATION_YEARS = INT(END_POINTS) * SIMULATION_YEARS
         WRITE(9,"(A,F5.1)") ' Execution Time per Endpoint (secs): ',    &
                           DBLE(EXECUTION_TIME)/(FLOAT(END_POINTS)*100.)
         WRITE(9,"(A,I6)") ' Total Simulation Years: ',SIMULATION_YEARS
      ELSE
         WRITE(9,"(A,I6)") ' Simulation Years: ',SIMULATION_YEARS
      ENDIF
      IF(SIMULATION_YEARS > 1) WRITE(9,"(A,F6.2)")                       &
        ' Execution Time per Year (secs): ',DBLE(EXECUTION_TIME)/        &
                                       (FLOAT(SIMULATION_YEARS)*100.)
!
! OVERHEAD TIME
!

      OVERHEAD_TIME_STR = CLOCK_TIME(OVERHEAD_TIME)
      WRITE(9,1010) " Overhead Time:           ",OVERHEAD_TIME_STR
!
! EXECUTION TIME
!
      EXECUTION_TIME_STR = CLOCK_TIME(EXECUTION_TIME)
      WRITE(9,1010) " Simulation Time:         ",EXECUTION_TIME_STR

!
! TOTAL TIME
!
      TOTAL_TIME_STR = CLOCK_TIME(TOTAL_TIME)
      WRITE(9,1010) " Total Run Time:          ",TOTAL_TIME_STR
!
! TIME ON INDEXING
!
      IF(INDEXING_TIME > 0) THEN
         INDEXING_TIME_STR = CLOCK_TIME(INDEXING_TIME)
         WRITE(9,1010)" File Indexing Time:          ",INDEXING_TIME_STR
      ENDIF
!
      CALL MYNAME(SIM_FILE_NAME)
      CALL LWC(SIM_FILE_NAME,LC_SIM_FILE_NAME)
      WRITE(9,1010) " Simulation Model:        ",                        &
                                                trim(LC_SIM_FILE_NAME)
      LC_SIM_FILE_NAME = trim(DATE_TIME(SIM_FILE_NAME))
      WRITE(9,1010) "  created:                  ",                      &
                                                trim(LC_SIM_FILE_NAME)
      WRITE(9,"('1')")

      CLOSE(9,IOSTAT=IOS)
      IF(IOS /= 0) THEN
         ERR_MESSAGE="" ! Bypass spurious lahey warning
         ! TODO:  This is not a standard call. This routine
         ! can be replaced fairly easily in a few hours 
         ! using API calls. Create replacment routine
         ! and update the 20+ codebase usages.
         call IOSTAT_MSG(int(IOS),ERR_MESSAGE)
         ! TODO end
         WRITE(4,*) ERR_MESSAGE
      ENDIF
      CALL DISPLAY_TIME

      RETURN
 1000 FORMAT("&",A,A,1X,I4)
 1010 FORMAT(A,A)
 1020 FORMAT(A,I4,A1,I4)
      END
 
!
!     SUBROUTINE TO CONVERT TOTAL TIME TO HOURS, MINUTES & SECONDS AND
!        CREATE A PRINT STRING OF THE TIME.
!          COPYRIGHT (C) 1986  M.S. GERBER & ASSOCIATES, INC.
!
 
!
      FUNCTION CLOCK_TIME(TOTAL_TIME)
!
      INTEGER (kind=4) ::  TOTAL_TIME_IN_SECONDS,HOURS,MINUTES,SECONDS,  &
                TOTAL_TIME
      INTEGER (kind=4) ::  FRAC_SECONDS
      CHARACTER (len=9) ::  TOTAL_TIME_STRING,CLOCK_TIME
!
      TOTAL_TIME_IN_SECONDS = TOTAL_TIME/100
      FRAC_SECONDS =MAX(TOTAL_TIME - 100*TOTAL_TIME_IN_SECONDS,0)
      SECONDS = MOD(TOTAL_TIME_IN_SECONDS,60)
      MINUTES = TOTAL_TIME_IN_SECONDS-SECONDS
      MINUTES = MOD(MINUTES,3600)/60
      HOURS = (TOTAL_TIME_IN_SECONDS-SECONDS-MINUTES*60)/3600
      IF(HOURS == 0) THEN
         WRITE(TOTAL_TIME_STRING,"(1X,I2,':',I2.2,'.',I2.2)")            &
                                         MINUTES,SECONDS,FRAC_SECONDS
      ELSE
         WRITE(TOTAL_TIME_STRING,"(I3,':',I2.2,':',I2.2)")               &
                                                HOURS,MINUTES,SECONDS
      ENDIF
      CLOCK_TIME = TOTAL_TIME_STRING
      RETURN
      END


