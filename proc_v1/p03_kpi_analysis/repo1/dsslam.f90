
!     DSSLAM.FOR
!     Copyright(c)  2000
!
!     Created: 11/8/2006 2:02:15 PM
!     Author : MARK S GERBER
!     Last change: MSG 1/10/2010 2:55:38 PM



!
!                       DSS LAM CONTROL MODULE
! COPYRIGHT (C) 1987 M.S. GERBER & ASSOCIATES, INC. ALL RIGHTS RESERVED
! COPYRIGHT (C) 1992 M.S. GERBER & ASSOCIATES, INC. ALL RIGHTS RESERVED
!

!
! Todo: Name this routine what it does.
      SUBROUTINE LODANAL
      use spindriftlib
      use prod_arrays_dimensions
      use reference_loads
      use tran2
      use cls_load
      use fyregycom
      use reference_loadsi2
      use trancom
      use globecom
      use prodcom
      use retslcom
      use sizecom
      use eightyeight
      use lamcom
      use fyregycom
      use forecast
      use production
      use month_extra
      use enrg_helper
	  use cls_load
	  use fyregycom, only: fyregy
      implicit none
      
      LOGICAL (kind=1) ::  CLASS_EXISTS_loc(MAX_LOAD_CLASSES),LDMGT_ACTIVE, &
        R_LDMGT_ACTIVE
      INTEGER (kind=2) ::  PSMO,PEMO,ISEAS,HOURS,HOURS_INCREMENT, I


      REAL, allocatable ::  FORECAST_LOADS(:,:,:,:)



      INTEGER (kind=2) ::  SYSTEM_POINTER,CLASS,SEASON,DAY,HOUR
      REAL ::  PEAK_LOADS(2,12),NUMBER_OF_DAYS
      REAL ::  FRCLOD(:)
      ALLOCATABLE :: FRCLOD
      REAL ::  MONTHLY_ENERGY(12)
      REAL ::  PEAK_BEFORE_DSM(12),BASE_BEFORE_DSM(12),PEAK_AFTER_DSM(12), &
        BASE_AFTER_DSM(12)
      REAL ::  PEAK_BEFORE,PEAK_AFTER,BASE_BEFORE,BASE_AFTER
      REAL (kind=8) ::  TENRG
      REAL (kind=8) ::  SUM_CLASS_ENERGY
      REAL ::  SYSTEM_LOSSES
      REAL ::  SYSTEM_FORECAST_ENERGY(2,12),SYSTEM_FORECAST_PEAK(2,12)
      REAL ::  SUM,HIST_DAY_COUNT
      LOGICAL (kind=1) ::  SYSTEM_FORECAST_DATA
      LOGICAL (kind=1) ::  CLASS_BASED_FORECAST,POOLING_TRANSACTIONS, &
        CONTROL_AREA_FORECAST

! DECLARATION FOR WORLD VARIABLES FOR FUTURE ASSETS
      LOGICAL (kind=1) ::  FORECAST_REPORT
      SAVE LDMGT_ACTIVE
!
      ALLOCATE(FRCLOD(8800), &
          FORECAST_LOADS(24,3,12,SYSTEM_CLASS_NUM))
!
      FORECAST_LOADS = 0.
!
      class_exists_loc=.false.
      CALL GET_CLASS_EXISTS(CLASS_EXISTS_loc)
      SYSTEM_POINTER = SYSTEM_CLASS_NUM
      DO CLASS = 1, SYSTEM_CLASS_NUM
         RETAIL_SALES_BY_CLASS(CLASS) = 0.
      ENDDO
      SYSTEM_FORECAST_DATA = SYSTEM_BASED_FORECAST() .OR. &
                             POOLING_TRANSACTIONS() .OR. &
                             CONTROL_AREA_FORECAST()
!
! BEGIN BY BUFFERING SYSTEM VALUES INTO ARRAYS
!
         DO SEASON = 1, 12
            DO DAY = 1, 2
               PEAK_LOADS(DAY,SEASON) = SYSTEM_HISTORICAL_PEAKS( &
                HISTORICAL_PEAK_HOUR(DAY,SEASON),DAY,SEASON)
            ENDDO
         ENDDO
!
! NOW CALL SUBROUTINE TO FORECAST DAY TYPES
!
!
!    CALL FORECAST TYPICAL DAY LOADS
!
      IF(SYSTEM_FORECAST_DATA) THEN
         IF(SYSTEM_BASED_FORECAST()) THEN
            CALL RETURN_ENERGY_PEAK_SYSTEM(SYSTEM_FORECAST_ENERGY, &
                SYSTEM_FORECAST_PEAK)
         ELSE
            CALL RETURN_ENERGY_PEAK_CLASS(SYSTEM_FORECAST_ENERGY, &
                SYSTEM_FORECAST_PEAK)
         ENDIF
         CALL SYSTEM_PEAK_FORECAST(SYSTEM_FORECAST_ENERGY, &
                 SYSTEM_FORECAST_PEAK,SYSTEM_HISTORICAL_LOADS, &
                 PEAK_LOADS,SYSTEM_CLASS_NUM,globecom_YEAR, &
                 gc_end_point,THREE_DAY_LOADS,FORECAST_LOADS, &
                 IMPORT_CAP,EXPORT_CAP)
      ELSE
         DO CLASS = 1, MAX_LOAD_CLASSES
            IF(CLASS_EXISTS_loc(CLASS)) &
              CALL SYSTEM_PEAK_FORECAST(FORECAST_ENERGY(1,1,CLASS), &
                    FORECAST_COINCIDENT_PEAK(1,1,CLASS), &
                    HISTORICAL_LOADS(1,1,1,CLASS), &
                    COINCIDENT_HISTORICAL_PEAK(1,1,CLASS), &
                    CLASS,globecom_YEAR, &
                    gc_end_point, &
                    THREE_DAY_CLASS_LOADS(1,1,1,CLASS),FORECAST_LOADS, &
                    IMPORT_CAP,EXPORT_CAP)
         ENDDO
!
         CALL SYSTEM_PEAK_FORECAST(SYSTEM_FORECAST_ENERGY, &
                 SYSTEM_FORECAST_PEAK,SYSTEM_HISTORICAL_LOADS, &
                 PEAK_LOADS,SYSTEM_CLASS_NUM,globecom_YEAR, &
                 gc_end_point, &
                 THREE_DAY_CLASS_LOADS(1,1,1,SYSTEM_CLASS_NUM), &
                 FORECAST_LOADS,IMPORT_CAP,EXPORT_CAP)

         FYREGY(globecom_year) = 0.
         DO SEASON = 1 , 12
            DO DAY = 1 , 3
               IF(DAY == 3) THEN
                  NUMBER_OF_DAYS = 4
               ELSE
                  HIST_DAY_COUNT=FLOAT(HISTORICAL_DAY_COUNT(DAY,SEASON))
                  IF(DAY == 1) NUMBER_OF_DAYS = HIST_DAY_COUNT - 4 + &
                    WEEKEND_PEAK_DAYS(SEASON)
                  IF(DAY == 2) NUMBER_OF_DAYS = HIST_DAY_COUNT - &
                    WEEKEND_PEAK_DAYS(SEASON)
               ENDIF
               SUM = 0.
               DO HOUR = 1, 24
                  SUM = SUM + FORECAST_LOADS(HOUR,DAY,SEASON,SYSTEM_CLASS_NUM)
               ENDDO
               FYREGY(globecom_YEAR) = FYREGY(globecom_YEAR) + SUM * &
                 NUMBER_OF_DAYS
            ENDDO !DAY
         ENDDO !SEASON
      ENDIF
!
!     NOW GENERATE FORECAST YEARLY LOAD VALUES
!
      CALL SYNSIZ(globecom_YEAR,LDMGT_ACTIVE, &
                  SYSTEM_FORECAST_DATA,gc_end_point, &
                  FORECAST_LOADS,FRCLOD, &
                  PEAK_BEFORE_DSM,BASE_BEFORE_DSM, &
                  PEAK_AFTER_DSM,BASE_AFTER_DSM, &
                  MONTHLY_ENERGY)
!
! CALCULATE RETAIL SALES
!
      IF(SYSTEM_BASED_FORECAST()) THEN
         CALL GET_SYSTEM_SALES(RETAIL_SALES_BY_CLASS(SYSTEM_CLASS_NUM))
      ELSE
         CALL GET_CLASS_AND_SYSTEM_SALES(RETAIL_SALES_BY_CLASS)
      ENDIF

      HOURS = 1
      DO ISEAS = 1 , get_PRODUCTION_PERIODS_in()
         IF(get_PRODUCTION_PERIODS_in() == 12) THEN
            PSMO = ISEAS
            PEMO = ISEAS
         ELSE
            PSMO = 1
            PEMO = 12
         ENDIF
         HOURS_INCREMENT = 0
         PEAK_BEFORE = 0.
         PEAK_AFTER = 0.
         BASE_BEFORE = 999999.
         BASE_AFTER = 999999.
         TENRG = 0.
         DO I = PSMO, PEMO
            HOURS_INCREMENT = HOURS_INCREMENT + get_HOURS_IN_PERIOD(I)
            TENRG = TENRG + MONTHLY_ENERGY(I)
            PEAK_BEFORE = MAX(PEAK_BEFORE,PEAK_BEFORE_DSM(I))
            PEAK_AFTER = MAX(PEAK_AFTER,PEAK_AFTER_DSM(I))
            BASE_BEFORE = MIN(BASE_BEFORE,BASE_BEFORE_DSM(I))
            BASE_AFTER = MIN(BASE_AFTER,BASE_AFTER_DSM(I))
         ENDDO
         CALL LOADPROB(HOURS,HOURS_INCREMENT,FRCLOD,TENRG, &
                       PEAK_BEFORE,BASE_BEFORE, &
                       PEAK_AFTER,BASE_AFTER, &
                       LDMGT_ACTIVE,ISEAS)
      ENDDO

!     END OF LOADPROB-MOVE EFFECTS

!     IF TYPICAL CURVE PLOTS DESIRED

      IF(FORECAST_REPORT() .AND. .NOT. TESTING_PLAN) THEN
         IF(.NOT. SYSTEM_FORECAST_DATA) THEN
            DO CLASS = 1, MAX_LOAD_CLASSES
               IF(CLASS_EXISTS_loc(CLASS)) &
                     CALL TYPLOT(FORECAST_LOADS,THREE_DAY_LOADS,globecom_YEAR, &
                                 THREE_DAY_CLASS_LOADS, &
                                 CLASS, &
                                 HISTORICAL_DAY_COUNT)
            ENDDO
            CALL TYPLOT(FORECAST_LOADS, &
                  THREE_DAY_CLASS_LOADS(1,1,1,SYSTEM_CLASS_NUM), &
                  globecom_YEAR,HISTORICAL_LOADS, &
                  SYSTEM_CLASS_NUM, &
                  HISTORICAL_DAY_COUNT)
         ELSE
            CALL TYPLOT(FORECAST_LOADS, &
                  THREE_DAY_LOADS,globecom_YEAR,HISTORICAL_LOADS, &
                  SYSTEM_CLASS_NUM, &
                  HISTORICAL_DAY_COUNT)
         ENDIF
      ENDIF
      DEALLOCATE(FRCLOD,FORECAST_LOADS)
      RETURN


      ENTRY STORE_LDMGT_ACTIVE(R_LDMGT_ACTIVE)

         LDMGT_ACTIVE = R_LDMGT_ACTIVE
      RETURN


      ENTRY GET_LDMGT_ACTIVE(R_LDMGT_ACTIVE)

          R_LDMGT_ACTIVE = LDMGT_ACTIVE
      RETURN
      END


      SUBROUTINE TYPLOT(FORECAST_LOADS,SYSLOD,yr, &
                        CLSLOD,CLASS, &
                        HDYCNT,endpt)

      use spindriftlib
      use prod_arrays_dimensions
      use grx_planning_routines
      use trans_peak_info
      use trancom
      use sizecom
      use lamcom
      use mod_base_year
      use endpoint
      use forecast
      
      implicit none
      integer (kind=2) :: endpt
      INTEGER (kind=2) ::  HR,CLASS,SEASON,DAY_TYPE, &
               HDYCNT(2,12),END_POINT_loc,yr
      REAL ::  HIST_ENERGY_DAILY,FUTURE_ENERGY_DAILY,HIST_ENERGY_MONTHLY, &
        FUTURE_ENERGY_MONTHLY,HIST_DAYS(4,12)
      REAL ::  FORECAST_LOADS(24,3,12,*), &
               SYSLOD(24,3,12), &
               CLSLOD(24,3,12,*)
      CHARACTER (len=20) ::  LOADS_MONTH_NAME*9,WRITE_MONTH_NAME(12)*9, &
        DAY_TITLE(4)*8
      ! Appears not to be used, but leaving commented for now.
      !CHARACTER*15 CLASS_NAME(7)
      INTEGER (kind=2) ::  FORECASTED_DAYS_REPORT_HEADER,FORE_DAYS_NO, &
        FORE_MONTH_NO,FORE_MONTH_REPORT_HEADER
      INTEGER ::  FORE_DAYS_REC,FORE_MONTH_REC
      SAVE FORE_DAYS_REC,FORE_MONTH_REC
      LOGICAL (kind=1) ::  FORECASTED_DAYS_NOT_OPEN=.TRUE., &
        FIRST_CALL_FOR_THIS_END_POINT
      integer (kind=2), save :: LOCAL_BASE_YEAR
      SAVE  FORE_DAYS_NO,WRITE_MONTH_NAME, &
            FIRST_CALL_FOR_THIS_END_POINT,HIST_DAYS, &
            FORE_MONTH_NO

! PLOT FOR BASE YEAR TYPICAL DAYS AND FORECAST TYPICAL DAYS
! BY SYSTEM OF CLASS

      DAY_TITLE(1) = 'Weekdays'
      DAY_TITLE(2) = 'Weekends'
      DAY_TITLE(3) = 'Peakdays'
      DAY_TITLE(4) = 'All Days'

      IF(FORECASTED_DAYS_NOT_OPEN) THEN
         FORE_DAYS_NO = FORECASTED_DAYS_REPORT_HEADER(FORE_DAYS_REC)
         FORE_MONTH_NO = FORE_MONTH_REPORT_HEADER(FORE_MONTH_REC)
         FORECASTED_DAYS_NOT_OPEN = .FALSE.
         LOCAL_BASE_YEAR = get_base_year()
         DO SEASON = 1 , 12
            WRITE_MONTH_NAME(SEASON) = LOADS_MONTH_NAME(SEASON)
         ENDDO
      ENDIF

      IF(FIRST_CALL_FOR_THIS_END_POINT) THEN
         DO SEASON = 1 , 12
               HIST_DAYS(1,SEASON) = FLOAT(HDYCNT(1,SEASON) -4)
               HIST_DAYS(2,SEASON) = FLOAT(HDYCNT(2,SEASON))
               HIST_DAYS(3,SEASON) = 4.
               HIST_DAYS(4,SEASON) = FLOAT(HDYCNT(1,SEASON) + HDYCNT(2,SEASON))
               HIST_ENERGY_MONTHLY = 0.
            DO DAY_TYPE = 1 , 3
               HIST_ENERGY_DAILY = 0.0
               DO HR = 1 , 24
                  HIST_ENERGY_DAILY = HIST_ENERGY_DAILY + &
                    SYSLOD(HR,DAY_TYPE,SEASON)
               ENDDO
!
               WRITE(FORE_DAYS_NO,REC=FORE_DAYS_REC) PRT_ENDPOINT(), &
                     FLOAT(LOCAL_BASE_YEAR), &
                     WRITE_MONTH_NAME(SEASON),DAY_TITLE(DAY_TYPE), &
                     (SYSLOD(HR,DAY_TYPE,SEASON),HR=1,24)
               FORE_DAYS_REC = FORE_DAYS_REC + 1
!
               WRITE(FORE_MONTH_NO,REC=FORE_MONTH_REC) PRT_ENDPOINT(), &
                     FLOAT(LOCAL_BASE_YEAR), &
                     WRITE_MONTH_NAME(SEASON),DAY_TITLE(DAY_TYPE), &
                     HIST_DAYS(DAY_TYPE,SEASON), &
                     HIST_ENERGY_DAILY,HIST_ENERGY_DAILY* &
                        HIST_DAYS(DAY_TYPE,SEASON), &
                     FLOAT(PKPEAK(DAY_TYPE,SEASON))
               FORE_MONTH_REC = FORE_MONTH_REC + 1
               HIST_ENERGY_MONTHLY = HIST_ENERGY_MONTHLY +  &
                HIST_DAYS(DAY_TYPE,SEASON)*HIST_ENERGY_DAILY
!
            ENDDO
!
            WRITE(FORE_DAYS_NO,REC=FORE_DAYS_REC) PRT_ENDPOINT(), &
                  FLOAT(LOCAL_BASE_YEAR), &
                  WRITE_MONTH_NAME(SEASON),DAY_TITLE(4), &
                  ((SYSLOD(HR,1,SEASON)*HIST_DAYS(1,SEASON) + &
                    SYSLOD(HR,2,SEASON)*HIST_DAYS(2,SEASON) + &
                    SYSLOD(HR,3,SEASON)*HIST_DAYS(3,SEASON))/ &
                                        HIST_DAYS(4,SEASON),HR=1,24)
            FORE_DAYS_REC = FORE_DAYS_REC + 1
!
            WRITE(FORE_MONTH_NO,REC=FORE_MONTH_REC) PRT_ENDPOINT(), &
                  FLOAT(LOCAL_BASE_YEAR), &
                  WRITE_MONTH_NAME(SEASON),DAY_TITLE(4), &
                  HIST_DAYS(4,SEASON), &
                  HIST_ENERGY_MONTHLY/MAX(HIST_DAYS(4,SEASON),real(1)), &
                  HIST_ENERGY_MONTHLY, &
                  FLOAT(MAX(PKPEAK(2,SEASON),PKPEAK(3,SEASON)))
            FORE_MONTH_REC = FORE_MONTH_REC + 1

         ENDDO
         FIRST_CALL_FOR_THIS_END_POINT = .FALSE.
      ENDIF

      DO SEASON = 1, 12
         IF(CLASS == SYSTEM_CLASS_NUM) THEN

            FUTURE_ENERGY_MONTHLY = 0.
            DO DAY_TYPE = 1 , 3
               FUTURE_ENERGY_DAILY = 0.0
               DO HR = 1 , 24

                  FUTURE_ENERGY_DAILY = FUTURE_ENERGY_DAILY + &
                    FORECAST_LOADS(HR,DAY_TYPE,SEASON,CLASS)

               ENDDO

               WRITE(FORE_DAYS_NO,REC=FORE_DAYS_REC) PRT_ENDPOINT(), &
                     FLOAT(yr+LOCAL_BASE_YEAR), &
                     WRITE_MONTH_NAME(SEASON),DAY_TITLE(DAY_TYPE), &
                     (FORECAST_LOADS(HR,DAY_TYPE,SEASON,7),HR=1,24)
               FORE_DAYS_REC = FORE_DAYS_REC + 1
!
               WRITE(FORE_MONTH_NO,REC=FORE_MONTH_REC) PRT_ENDPOINT(), &
                     FLOAT(yr+LOCAL_BASE_YEAR), &
                     WRITE_MONTH_NAME(SEASON),DAY_TITLE(DAY_TYPE), &
                     HIST_DAYS(DAY_TYPE,SEASON), &
                     FUTURE_ENERGY_DAILY,FUTURE_ENERGY_DAILY* &
                        HIST_DAYS(DAY_TYPE,SEASON), &
                     FUT_PEAK(DAY_TYPE,SEASON)
               FORE_MONTH_REC = FORE_MONTH_REC + 1
!
               FUTURE_ENERGY_MONTHLY = FUTURE_ENERGY_MONTHLY + &
                FUTURE_ENERGY_DAILY*HIST_DAYS(DAY_TYPE,SEASON)
!
            ENDDO
!
            WRITE(FORE_DAYS_NO,REC=FORE_DAYS_REC) PRT_ENDPOINT(), &
               FLOAT(yr+LOCAL_BASE_YEAR), &
               WRITE_MONTH_NAME(SEASON),DAY_TITLE(4), &
               ((FORECAST_LOADS(HR,1,SEASON,7)*HIST_DAYS(1,SEASON) + &
                FORECAST_LOADS(HR,2,SEASON,7)*HIST_DAYS(2,SEASON) + &
                FORECAST_LOADS(HR,3,SEASON,7)*HIST_DAYS(3,SEASON))/ &
                                     HIST_DAYS(4,SEASON),HR=1,24)
            FORE_DAYS_REC = FORE_DAYS_REC + 1
!
            WRITE(FORE_MONTH_NO,REC=FORE_MONTH_REC) PRT_ENDPOINT(), &
                     FLOAT(yr+LOCAL_BASE_YEAR), &
                     WRITE_MONTH_NAME(SEASON),DAY_TITLE(4), &
                     HIST_DAYS(4,SEASON), &
                     FUTURE_ENERGY_MONTHLY/MAX(HIST_DAYS(4,SEASON),real(1)), &
                     FUTURE_ENERGY_MONTHLY, &
                     MAX(FUT_PEAK(2,SEASON),FUT_PEAK(3,SEASON))
            FORE_MONTH_REC = FORE_MONTH_REC + 1

         ENDIF

         IF(CLASS < 7) CYCLE

      ENDDO
      RETURN
      ENTRY INIT_ENDPT_FORE_DAYS_REPORT
         FIRST_CALL_FOR_THIS_END_POINT = .TRUE.
      RETURN
      END subroutine TYPLOT
