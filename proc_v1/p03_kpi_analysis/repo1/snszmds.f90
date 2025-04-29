!     SNSZMDS.FOR
!     Copyright(c)  2000
!     Created: 11/18/2009 10:28:43 AM
!     Author : MARK S GERBER
!     Last change: MSG 11/20/2009 11:53:49 AM
      SUBROUTINE SYNSIZ(year_ord,LDMGT_ACTIVE, &
       system_based_forecast_loc,endpt_ord, &
       FORECAST_LOADS,FRCLOD, &
       PEAK_BEFORE_DSM,BASE_BEFORE_DSM, &
       PEAK_AFTER_DSM,BASE_AFTER_DSM, &
       MONTHLY_ENERGY)

!
!  FORMS SYSTEM FORECAST HOURLY LOAD VALUES
!
      USE SIZECOM
      use SpinDriftLib
      use eightyeight
      use forecast
      use kepcocom
      use lamcom
      use mod_base_year
      use month_extra
      use pp_objt_interfaces
      use prod_arrays_dimensions
      use reference_loads
      use reference_loadsi2
      use retslcom
      use sizecom
      use spindriftlib
      use string
      use tran2
      use trans_peak_info
      use logging
      use sic_trace
      implicit none
      integer :: index_dbg
      character (len=1024) :: string_dbg
      logical(kind=1) :: CLASS_BASED_FORECAST
      LOGICAL (kind=1) ::  SYSTEM_BASED_FORECAST_loc,LDMGT_ACTIVE, &
                USE_EXTERNAL_HOURLY_LOADS,USE_EXT_LOAD_SOURCE, &
                SAVE_YES_RUN_TRANSACT=.FALSE.
      INTEGER (kind=2) ::  YEAR_ord,MONTH
      INTEGER (kind=2) ::  CUM_HOURS_AT_THE_END_OF


!     2/10/93. GAT.

      INTEGER (kind=2) ::  SYSTEM_POINTER, &
                SEASON,NUMBER_OF_HISTORICAL_LOADS,I,DAY,HOUR, &
                CURRENT_HOUR,FIRST_SEASON_HOUR
      integer (kind=2) :: endpt_ord,METHOD,PK_DAY,OFF_DAY, &
                DAY_OF_WEEK_OUT(31,12),KILL_DAY_TYPE, &
                KILL_DAY_POSITION,ADD_DAY_TYPE,REDUCED_WEEKDAYS, &
                LEAP_DAY,R_DAY_IN_MONTH,R_DAY_OF_WEEK, &
                DAYWEK
      LOGICAL (kind=1) ::    ADD_DAY
      INTEGER (kind=2) ::  DAY_OF_MONTH(31,3),PKHRSIP(3), &
               DAY_OF_WEEK(31),COUNTER(3),STARTING_(3),HOUR_IN_FCLOD, &
               DAY_TYPE,POS_OF_HOURLY_FILE, &
               CURRENT_YEAR,HOURLY_LOAD_OUT,CURRENT_DAY, &
               CAL_WEEK_DAYS,CAL_PEAK_DAYS,CAL_WEEK_END_DAYS, &
               CHANGE_IN_WEEK_DAYS,CHANGE_IN_WEEKEND_DAYS, &
               CAL_DAY_OF_WEEK(31),CAL_DAYS_PER_MONTH,HOUR_IN_MONTH 
      INTEGER (kind=4) ::  AFTER_DSM_DAILY_LOADS(24)
      LOGICAL (kind=1) ::  CONTROL_AREA_FORECAST
      INTEGER (kind=2) ::  DAYS_PER_MONTH,DAYS_IN_EACH_MONTH
      REAL ::   PEAK_MAX,BASE_MIN,ENERGY_YEAR,REMAIN,LEAP_SLOPE, &
            LEAP_INTERCEPT,HOURLY_ERROR,LAST_ERROR,ADDED_ENERGY, &
            LOST_ENERGY,LEAP_ENERGY,HOURS_IN_SEASON
      LOGICAL (kind=1) ::  APPLY_LEAP_YEAR
      LOGICAL (kind=1) ::  WRITE_TO_DSM_DATA_BASE,CALENDER_CORRECT, &
                  LEAP_MONTH,YES_CALANDER_CORRECT
      CHARACTER (len=8) ::  EEICODE
      INTEGER (kind=2) ::  TIMZON,TEMPER,DELTMP
      REAL ::  HISTORICAL_ENERGY, &
            HIST_VALUE, &
            TEMPSLOPE,TEMPINTERCEPT,SYS_HIST_PK,LDMAX, &
            HIST_CHECK,PEAK_TEST,DSM_CHECK1,DSM_CHECK2, &
            DSM_CHECK3, &
            PEAK_BEFORE_DSM(12),BASE_BEFORE_DSM(12), &
            PEAK_AFTER_DSM(12),BASE_AFTER_DSM(12), &
            LOAD_BEFORE_DSM,LOAD_AFTER_DSM, &
            TEMP_ARRAY(3,MAX_LOAD_CLASSES)
      INTEGER (kind=2) ::  PEAK_DAY_AFTER_DSM,PEAK_HOUR_AFTER_DSM
      REAL ::  FORE_CHECK
      REAL ::  EQ1_P_EQ21,EQ22_P_EQ3_M_5
      CHARACTER (len=20) ::  MONTH_NAME
      CHARACTER (len=9) ::  DAY_NAME
!
! INNCLUDE COMMON BLOCK OF ALL FORECAST TYPICAL CURVES FOR THIS YEAR_ord
!
      REAL ::  FORECAST_LOADS(24,3,12,SYSTEM_CLASS_NUM)
!
!
      INTEGER (kind=2) :: THIS_YEAR_IS
! INCLUDE LAM COMMON BLOCKS
!
!
! REQUIRED BUFFER ARRAYS
!
!
      INTEGER (kind=2) ::    CHRONOLOGICAL_HOUR(8800),HR, &
                  R_HOURS,R_HOURS_INCREMENT,CHRONO_HOURS_PER_MONTH(12), &
                  R_CHRONO_HOURS_PER_MONTH, &
                  CUM_CHRONO_HOURS_PER_MONTH(12), &
                  TEMP_HOURLY_INDEX(*), &
                  TEMP_HOURS,TEMP_CUM_HOURS,R_MONTH,R_CUM_HOURS
      REAL (kind=4) ::       CHRONOLOGICAL_LOAD(8800)
      REAL (kind=4) ::       TEMP_CHRONO_LOAD(:)
      SAVE  CHRONOLOGICAL_HOUR,CHRONO_HOURS_PER_MONTH, &
            CUM_CHRONO_HOURS_PER_MONTH,DAY_OF_WEEK_OUT
      ALLOCATABLE :: TEMP_CHRONO_LOAD



      REAL ::  MONTHLY_ENERGY(12),FRCLOD(8800)
      INTEGER (kind=4) ::  HISTORICAL_LOAD_BUFFER(31)
      integer (kind=2) :: hla_returned
      REAL ::  LODDIF(24,3,3)
      REAL (kind=4) ::  OPC_LOSS_MULT
      LOGICAL (kind=1) ::  LAHEY_LF95
!
! END DATA DECLARATIONS
!
!
!  READ IN HISTORICAL LOAD VALUES BY TIME-BLOCK, DAY TYPE
!  ( TYPE 1 = WEEKDAYS, TYPE 2 = WEEKENDS+HOLIDAYS), & SEASON.
!  GROW LOAD VALUES IN HISTORICAL BLOCK BY RATIO OF FORECAST SYSTEM
!  TO HISTORICAL SYSTEM TYPICAL DAY CURVE VALUES THIS BLOCK
!
! 2/15/93. GAT. ADDED TO KEEP TRACK OF HOURLY DSM
!

      CHRONO_HOURS_PER_MONTH = 0
      CUM_CHRONO_HOURS_PER_MONTH = 0
      DAY_OF_WEEK_OUT = 0
!
      SAVE_YES_RUN_TRANSACT = YES_RUN_TRANSACT()
!
      IF(YES_CALANDER_CORRECT()) THEN
         APPLY_LEAP_YEAR = .FALSE. ! TEMPORARY
         CALENDER_CORRECT = .TRUE.
      ELSE
         APPLY_LEAP_YEAR = .FALSE.
         CALENDER_CORRECT = .FALSE.
      ENDIF
!

      CALL CHECK_WRITE_TO_DSM_DATA_BASE(WRITE_TO_DSM_DATA_BASE)
      REWIND(26)
      CURRENT_HOUR=0
      THIS_YEAR_IS = year_ord + get_base_year()

      USE_EXTERNAL_HOURLY_LOADS = USE_EXT_LOAD_SOURCE()
      IF(LDMGT_ACTIVE .AND. USE_EXTERNAL_HOURLY_LOADS) THEN
         WRITE(4,*) "Load Management is ignored if"
         WRITE(4,*) "using Reference Loads for"
         WRITE(4,*) "for the forecast."
         WRITE(4,*) " "
      ENDIF

      OPC_LOSS_MULT = 1.0 ! FOR LOADS FROM TRANSACT

      DO MONTH = 1 , 12
         MONTHLY_ENERGY(MONTH) = 0.
         PEAK_BEFORE_DSM(MONTH) = 0.
         BASE_BEFORE_DSM(MONTH) = 9999999.
         PEAK_AFTER_DSM(MONTH) = 0.
         PEAK_DAY_AFTER_DSM = 0.
         PEAK_HOUR_AFTER_DSM = 0.
         BASE_AFTER_DSM(MONTH) = 9999999.
         DO DAY = 1 , 3
            FUT_PEAK(DAY,MONTH) = 0.0
         ENDDO
         FIRST_SEASON_HOUR = CURRENT_HOUR + 1
         IF(MONTH == 1 .OR. LDMGT_ACTIVE) LODDIF = 0.
!
! IF LOAD MANAGEMENT OR TIME-OF-DAY SCENARIO DESIRED THIS RUN, CALL.
!
         IF(LDMGT_ACTIVE) THEN
            IF(WABASH_VALLEY) THEN
               CALL WABASH_LODMGT(LODDIF,MONTH,FORECAST_LOADS)
            ELSE
               CALL LODMGT(LODDIF,MONTH,FORECAST_LOADS)
            ENDIF
            IF(MONTH == 12) CALL UPDATE_DSM_RESPONSE_CURVES
         ENDIF
!
         DO HOUR = 1 , 24
!
            DO DAY = 1 , 3
!
!**********************************************************************
!              THIS SECTION CALCULATES THE CONTRIBUTION
!              OF DSM LOADS OF ANY OF THREE OPERATING METHODS
!
!              EQ1 => E (CONSERVATION METHOD)
!              EQ21, EQ22 => I (INTERMEDIATE,CYCLING METHOD)
!              EQ3 => P (PEAK METHOD)
!
!              THE I METHOD IS JUST A LINEAR INTERPOLATION OF
!              OF THE E AND P METHODS. IT WILL PRODUCE THE SAME
!              PEAK REDUCTIONS AS P AND E AND WILL PRODUCE ENERGY
!              REDUCTIONS HALF WAY BETWEEN P AND E.
!
!**********************************************************************
!
               IF(LDMGT_ACTIVE) THEN
                  LDMAX = (PKPKD(HOUR,DAY,MONTH)-  &
                                INTERCEPT(DAY,MONTH))/SLOPE(DAY,MONTH)
                  EQ1_P_EQ21 = LODDIF(HOUR,DAY,1)+LODDIF(HOUR,DAY,2)/2.

                  EQ22_P_EQ3_M_5 = 1 + (LODDIF(HOUR,DAY,2)/2. + &
                                               LODDIF(HOUR,DAY,3))/LDMAX
                  FORECAST_LOADS(HOUR,DAY,MONTH,SYSTEM_CLASS_NUM) = &
                      EQ1_P_EQ21 + EQ22_P_EQ3_M_5 * &
                        FORECAST_LOADS(HOUR,DAY,MONTH,SYSTEM_CLASS_NUM)
               ENDIF
!

!
               IF(.NOT.system_based_forecast_loc) THEN


                  IF(DAY < 3) THEN
                     SYS_HIST_PK = SYSTEM_HISTORICAL_PEAKS(HOUR,DAY,MONTH)
                  ELSE
                     SYS_HIST_PK = &
                                  SYSTEM_HISTORICAL_PEAKS(HOUR,1,MONTH)
                  ENDIF

                  TEMPSLOPE = (SYS_HIST_PK - &
                                      THREE_DAY_LOADS(HOUR,DAY,MONTH))/ &
                      (SYS_HIST_PK - &
                               THREE_DAY_CLASS_LOADS(HOUR,DAY,MONTH,7))
                  TEMPINTERCEPT = SYS_HIST_PK*(1.0-TEMPSLOPE)
               ENDIF
!
               READ(26) NUMBER_OF_HISTORICAL_LOADS,HISTORICAL_ENERGY, &
                           (HISTORICAL_LOAD_BUFFER(I), &
                                       I = 1, NUMBER_OF_HISTORICAL_LOADS)
!
! TRANSFORM HOURLY LOADS WITH LINEAR EQUATION PREVIOUSLY
! DERIVED FOR THIS MONTH AND DAY_TYPE.
!
               DO I = 1 , NUMBER_OF_HISTORICAL_LOADS
                  CURRENT_HOUR = CURRENT_HOUR + 1
!
                  IF(system_based_forecast_loc) THEN
                     HIST_VALUE = HISTORICAL_LOAD_BUFFER(I)
                  ELSE
                     HIST_VALUE = &
                            (HISTORICAL_LOAD_BUFFER(I) - TEMPINTERCEPT)/ &
                                            TEMPSLOPE
                  ENDIF
!
!                 THIS EQUATION WILL FIRST TRANSFORM EACH HISTORICAL
!                 OBSERVATION TO ITS FORECASTED VALUE AND THEN INCLUDE
!                 LOAD CHANGES FOR ANY COMBINATION OF THE THREE DSM
!                 OPERATING METHODS.
!

                  IF(USE_EXTERNAL_HOURLY_LOADS) THEN
                     LOAD_BEFORE_DSM = HIST_VALUE
                  ELSE
                     LOAD_BEFORE_DSM = &
                        (HIST_VALUE - INTERCEPT(DAY,MONTH))/ &
                                                        SLOPE(DAY,MONTH)
                  ENDIF
                  IF(LOAD_BEFORE_DSM .GT. 0.) THEN
                     IF(LOAD_BEFORE_DSM .GT. PEAK_BEFORE_DSM(MONTH)) &
                               PEAK_BEFORE_DSM(MONTH) = LOAD_BEFORE_DSM
                     IF(LOAD_BEFORE_DSM .LT. BASE_BEFORE_DSM(MONTH)) &
                               BASE_BEFORE_DSM(MONTH) = LOAD_BEFORE_DSM
                     IF(LDMGT_ACTIVE) THEN
                        IF(USE_EXTERNAL_HOURLY_LOADS) THEN
                           LOAD_AFTER_DSM = HIST_VALUE
                        ELSE

                           LOAD_AFTER_DSM = EQ1_P_EQ21 + &
                                        EQ22_P_EQ3_M_5 * LOAD_BEFORE_DSM
                        ENDIF
                        IF(LOAD_AFTER_DSM .GT. &
                                             PEAK_AFTER_DSM(MONTH)) THEN

                           PEAK_AFTER_DSM(MONTH) = LOAD_AFTER_DSM
                           PEAK_HOUR_AFTER_DSM = HOUR
                           PEAK_DAY_AFTER_DSM = DAY
                        ENDIF
                        IF(LOAD_AFTER_DSM .LT. BASE_AFTER_DSM(MONTH)) &
                               BASE_AFTER_DSM(MONTH) = LOAD_AFTER_DSM
                        FRCLOD(CURRENT_HOUR) = LOAD_AFTER_DSM
                     ELSE
                        IF(USE_EXTERNAL_HOURLY_LOADS) THEN
                           FRCLOD(CURRENT_HOUR) = HIST_VALUE * &
                                                           OPC_LOSS_MULT
                        ELSE
                           FRCLOD(CURRENT_HOUR) = LOAD_BEFORE_DSM
                        ENDIF
                     ENDIF
                     MONTHLY_ENERGY(MONTH) = MONTHLY_ENERGY(MONTH) + &
                                                    FRCLOD(CURRENT_HOUR)
                     IF(FRCLOD(CURRENT_HOUR) .GT. FUT_PEAK(DAY,MONTH)) &
                              FUT_PEAK(DAY,MONTH) = FRCLOD(CURRENT_HOUR)
                  ELSE
                     FRCLOD(CURRENT_HOUR) = 0.
                  ENDIF
!
                  IF(RR_SLOPE(MONTH) /= 0.) THEN
                     HOURLY_RR(CURRENT_HOUR) = MAX(0.,10.* (HIST_VALUE - &
                                 RR_INTERCEPT(MONTH))/RR_SLOPE(MONTH))
                  ELSE ! ASSUME NOT USING ECONOMY_INTERCHANGE MODULE
                     ! THIS MUST BE FIXED
                     HOURLY_RR(CURRENT_HOUR) = HIST_VALUE
                  ENDIF
               ENDDO
!
            ENDDO
!
! IF LOAD MANAGEMENT NOT ACTIVE SET SEASONAL PEAK AND BASE
!
            IF(.NOT. LDMGT_ACTIVE) THEN
               PEAK_AFTER_DSM(MONTH) = PEAK_BEFORE_DSM(MONTH)
               BASE_AFTER_DSM(MONTH) = BASE_BEFORE_DSM(MONTH)
            ENDIF
         ENDDO
!
!           WARNINGS SECTION
!
            HIST_CHECK = PKPEAK(3,MONTH) - PKPEAK(2,MONTH)
            FORE_CHECK = DAY_TYPE_PEAK_DELTA(MONTH)
            PEAK_TEST = HIST_CHECK * FORE_CHECK
            DSM_CHECK1 = FUT_PEAK(3,MONTH) - FUT_PEAK(2,MONTH)
            DSM_CHECK2 = FUT_PEAK(3,MONTH) - FUT_PEAK(1,MONTH)
            DSM_CHECK3 = FUT_PEAK(2,MONTH) - FUT_PEAK(1,MONTH)


!        WARN IF LOAD FORECAST SHIFTS PEAK


            IF( PEAK_TEST.LT.0. ) THEN
               IF(HIST_CHECK.LT.0) THEN
                  PK_DAY = 2
                  OFF_DAY = 3
               ELSE
                  PK_DAY = 3
                  OFF_DAY = 2
               ENDIF

               WRITE(4,*) 'In the month of ', &
               trim(MONTH_NAME(MONTH)), &
                 ' in year_ord',THIS_YEAR_IS,','
               WRITE(4,*) 'for end point',endpt_ord

               WRITE(4,*) 'the system forecast shifted peak from '
               WRITE(4,*) trim(DAY_NAME(PK_DAY)),' to ', &
                  trim(DAY_NAME(OFF_DAY))
               WRITE(4,*) 'Check the peak forecast ', &
                  'in the System Forcast file'
               WRITE(4,*) ' '
            ENDIF

!        WARN IF LOAD MANAGEMENT SHIFTS PEAK

            IF(FORE_CHECK > 0.) THEN

!              IF THE SYSTEM FORECAST PEAK OCCURS ON THE PEAK DAY

               IF(DSM_CHECK1.LT.0) THEN
                  PK_DAY = 3
                  OFF_DAY = 2
                  CALL DSM_WARN(year_ord, &
                      PK_DAY,OFF_DAY,MONTH,endpt_ord,DSM_CHECK1)
               ENDIF
               IF(DSM_CHECK2.LT.0) THEN
                  PK_DAY = 3
                  OFF_DAY = 1
                  CALL DSM_WARN(year_ord, &
                       PK_DAY,OFF_DAY,MONTH,endpt_ord,DSM_CHECK2)
               ENDIF
!
            ELSEIF(FORE_CHECK < 0.) THEN
!
!              IF THE SYSTEM FORECAST PEAK OCCURS ON THE WEEKEND
!
               IF(DSM_CHECK1.GT.0) THEN
                  PK_DAY = 2
                  OFF_DAY = 3
                  DSM_CHECK1 = -1*DSM_CHECK1
                  CALL DSM_WARN(year_ord, &
                      PK_DAY,OFF_DAY,MONTH,endpt_ord,DSM_CHECK1)
               ENDIF
               IF(DSM_CHECK3.LT.0) THEN
                  PK_DAY = 2
                  OFF_DAY = 1
                  CALL DSM_WARN(year_ord, &
                      PK_DAY,OFF_DAY,MONTH,endpt_ord,DSM_CHECK3)
               ENDIF
            ENDIF
!
         IF(HOURLY_LOAD_OUT() > 0 .OR. SAVE_YES_RUN_TRANSACT) THEN

!           2/11/93. GAT. THIS MOVES INITIALIZE INSIDE THE WRITE Q.

            IF(MONTH == 1 ) THEN
               CALL SAVE_HOLIDAYS(THIS_YEAR_IS,HOLIDAYS)
               HOURLY_ERROR = 0.0
               CURRENT_YEAR = THIS_YEAR_IS-1900
               IF(CURRENT_YEAR > 99) CURRENT_YEAR = CURRENT_YEAR - 100

! 3/1/93. GAT. TAKES PLACE OF A SINGLE LARGE FILE.

               IF(LAHEY_LF95()) THEN
                  POS_OF_HOURLY_FILE = 1
               ELSE
                  POS_OF_HOURLY_FILE = 0
               ENDIF
               PEAK_MAX = 0.0
               BASE_MIN = 9999999.0
               ENERGY_YEAR = 0.0
               READ(27) EEICODE,TIMZON,TEMPER,DELTMP
            ENDIF
            DAYS_PER_MONTH = HISTORICAL_DAY_COUNT(1,MONTH)+ &
                                           HISTORICAL_DAY_COUNT(2,MONTH)
            CAL_DAYS_PER_MONTH = DAYS_PER_MONTH
            HOURS_IN_SEASON = 24.*FLOAT(DAYS_PER_MONTH)

            LEAP_SLOPE = 1.0
            LEAP_INTERCEPT = 0.0
            LEAP_ENERGY = 0.0
            ADDED_ENERGY = 0.0
            LOST_ENERGY = 0.0
            LEAP_MONTH = .FALSE.
            LEAP_DAY = 0
            DO DAY = 1 , 3

               READ(27) PKHRSIP(DAY),(DAY_OF_MONTH(I,DAY), &
                                                  I = 1 , PKHRSIP(DAY))

               DO I = PKHRSIP(DAY)+1 , 31
                  DAY_OF_MONTH(I,DAY) = 0
               ENDDO
            ENDDO
            READ(27) (DAY_OF_WEEK(DAY), DAY = 1 , DAYS_PER_MONTH)
            IF(MONTH == 2) THEN
               IF(DAY_OF_WEEK(28) < 7) THEN
                  DAY_OF_WEEK(29) = DAY_OF_WEEK(28)+1
               ELSE
                  DAY_OF_WEEK(29) = 1
               ENDIF
               REMAIN = MOD(THIS_YEAR_IS-1964.,4.)

               IF( REMAIN < .001 .AND. APPLY_LEAP_YEAR) then
                                                LEAP_MONTH = .TRUE.
               end if
               IF(LEAP_MONTH .AND. .NOT. CALENDER_CORRECT) THEN
                  LEAP_ENERGY = 0.0
                  IF(DAY_OF_WEEK(29) > 0 .AND. DAY_OF_WEEK(29) < 6) THEN
                     DAY_TYPE = 1
                  ELSE
                     DAY_TYPE = 2
                  ENDIF
                  DO HOUR = 1 , 24
                     LEAP_ENERGY = LEAP_ENERGY + &
                              FORECAST_LOADS(HOUR,DAY_TYPE,2,7)
                  ENDDO
               ENDIF
            ENDIF
            COUNTER(1) = 1
            COUNTER(2) = 1
            COUNTER(3) = 1
            STARTING_(1) = 0
            STARTING_(2) = PKHRSIP(1)
            STARTING_(3) = STARTING_(2)+PKHRSIP(2)
            CHANGE_IN_WEEK_DAYS = 0
            CHANGE_IN_WEEKEND_DAYS = 0
            IF( CALENDER_CORRECT ) THEN
               IF(LEAP_MONTH) THEN
                  CAL_DAYS_PER_MONTH = 29
                  HOURS_IN_SEASON = 696
                  LEAP_DAY = 1
               ENDIF
               DO DAY = 1 , CAL_DAYS_PER_MONTH
                  CALL CALENDER_DAY_OF_WEEK(MONTH, &
                       DAY,THIS_YEAR_IS,CAL_DAY_OF_WEEK(DAY))
               ENDDO

               CALL DAYS_BY_DAY_TYPE(THIS_YEAR_IS, &
                        MONTH,CAL_DAYS_PER_MONTH,HOLIDAYS(MONTH), &
                        CAL_WEEK_DAYS,CAL_PEAK_DAYS,CAL_WEEK_END_DAYS)
               CHANGE_IN_WEEK_DAYS = CAL_WEEK_DAYS - &
                                          HISTORICAL_DAY_COUNT(1,MONTH)
               CHANGE_IN_WEEKEND_DAYS = CAL_WEEK_END_DAYS - &
                  HISTORICAL_DAY_COUNT(2,MONTH) - HOLIDAYS(MONTH)

               REDUCED_WEEKDAYS  = 0
               ADDED_ENERGY = 0.0
               LOST_ENERGY = 0.0
               IF(CHANGE_IN_WEEK_DAYS < 0) THEN ! REDUCE WEEK DAYS
                  REDUCED_WEEKDAYS  = ABS(CHANGE_IN_WEEK_DAYS)+LEAP_DAY
                  ADD_DAY_TYPE = 2
                  KILL_DAY_TYPE = 1
                  KILL_DAY_POSITION = PKHRSIP(1) + 1
                  I = CAL_DAYS_PER_MONTH + 1
                  DAY = 0
                  DO WHILE( DAY < ABS(CHANGE_IN_WEEK_DAYS))
                     I = I - 1

                     IF( DAY_OF_WEEK(I) > 5 .OR. &
                                    DAY_OF_MONTH(1,3) == I .OR. &
                                    DAY_OF_MONTH(2,3) == I .OR. &
                                    DAY_OF_MONTH(3,3) == I .OR. &
                                    DAY_OF_MONTH(4,3) == I) then
                          CYCLE
                     end if

                     DAY = DAY + 1
                     KILL_DAY_POSITION = KILL_DAY_POSITION - 1
                     DO HOUR = 1 , 24
                        IF(.NOT. (LEAP_MONTH .AND. DAY == 1)) THEN

                           HOUR_IN_FCLOD= &
                              STARTING_(KILL_DAY_TYPE) + &
                              KILL_DAY_POSITION + &
                              DAYS_PER_MONTH*(HOUR-1) + &
                              FIRST_SEASON_HOUR - 1 
                           LOST_ENERGY = LOST_ENERGY + &
                                                FRCLOD(HOUR_IN_FCLOD)

                        ENDIF
                        ADDED_ENERGY = ADDED_ENERGY + &
                                       FORECAST_LOADS(HOUR,2,MONTH,7)
                     ENDDO
                  ENDDO
               ELSEIF(CHANGE_IN_WEEKEND_DAYS < 0) THEN
                  ! REDUCE WEEKEND DAYS               
                  ADD_DAY_TYPE = 1
                  KILL_DAY_TYPE = 2
                  KILL_DAY_POSITION = PKHRSIP(2) + 1
                  I = CAL_DAYS_PER_MONTH + 1
                  DAY = 0
                  DO WHILE( DAY < ABS(CHANGE_IN_WEEK_DAYS))
                     I = I - 1
                     IF(DAY_OF_WEEK(I)<6 .OR. &
                           DAY_OF_WEEK(I) > 7) then
                         CYCLE
                     end if
                     DAY = DAY + 1
                     KILL_DAY_POSITION = KILL_DAY_POSITION - 1
                     DO HOUR = 1 , 24
                        IF(.NOT. (LEAP_MONTH .AND. DAY == 1)) THEN

                           HOUR_IN_FCLOD= &
                              STARTING_(KILL_DAY_TYPE) + &
                              KILL_DAY_POSITION + &
                              DAYS_PER_MONTH*(HOUR-1) + &
                              FIRST_SEASON_HOUR - 1
                           LOST_ENERGY = LOST_ENERGY + &
                                          FRCLOD(HOUR_IN_FCLOD)

                        ENDIF
                        ADDED_ENERGY = ADDED_ENERGY + &
                             FORECAST_LOADS(HOUR,1,MONTH,7)
                     ENDDO
                  ENDDO
               ELSEIF(CHANGE_IN_WEEKEND_DAYS > 0) THEN ! ADD WEEKEND DAY
                  ADD_DAY_TYPE = 2
                  DO HOUR = 1 , 24
                     ADDED_ENERGY = ADDED_ENERGY + &
                                         FORECAST_LOADS(HOUR,2,MONTH,7)
                  ENDDO
               ELSEIF(CHANGE_IN_WEEK_DAYS > 0) THEN ! ADD WEEK DAY
                  ADD_DAY_TYPE = 1
                  DO HOUR = 1 , 24
                     ADDED_ENERGY = ADDED_ENERGY + &
                                         FORECAST_LOADS(HOUR,1,MONTH,7)
                  ENDDO
               ENDIF ! CHANGE = 0 => DO NOT CHANGE DAYS
            ENDIF ! CALENDER_CORRECT

            IF( (CALENDER_CORRECT  .AND. CHANGE_IN_WEEK_DAYS /= 0) &
                           .OR. (MONTH == 2 .AND. LEAP_MONTH) ) THEN
               LEAP_SLOPE = (FUT_PEAK(3,MONTH) - &
                  MONTHLY_ENERGY(MONTH)/ &
                  HOURS_IN_SEASON)/ &
                  (FUT_PEAK(3,MONTH) - (MONTHLY_ENERGY(MONTH) + &
                  LEAP_ENERGY+ADDED_ENERGY-LOST_ENERGY)/ &
                  HOURS_IN_SEASON)

               LEAP_INTERCEPT = FUT_PEAK(3,MONTH)*(1.0-LEAP_SLOPE)
            ENDIF
            CURRENT_DAY = 1
            ADD_DAY = .FALSE.
            DO DAY = 1 , CAL_DAYS_PER_MONTH
               IF(CALENDER_CORRECT) THEN
                  IF(CAL_DAY_OF_WEEK(DAY) < 6) THEN
                     IF( (DAY_OF_MONTH(COUNTER(3),3) <= DAY .OR. &
                           COUNTER(1) + REDUCED_WEEKDAYS > &
                                          PKHRSIP(1)) .AND. &
                                          COUNTER(3) <= PKHRSIP(3)) THEN
                        DAY_TYPE = 3
                        COUNTER(3) = COUNTER(3) + 1
                     ELSE
                        IF(COUNTER(1) <= PKHRSIP(1)) THEN
                           DAY_TYPE = 1
                           COUNTER(1) = COUNTER(1) + 1
                        ELSE
                           ADD_DAY = .TRUE.
                        ENDIF
                     ENDIF
                  ELSE
                     IF(COUNTER(2) <= PKHRSIP(2)) THEN
                        DAY_TYPE = 2
                        COUNTER(2) = COUNTER(2) + 1
                     ELSE
                        ADD_DAY = .TRUE.
                     ENDIF
                  ENDIF
                  DAY_OF_WEEK_OUT(DAY,MONTH) = CAL_DAY_OF_WEEK(DAY)
!
! CHECK FOR HOLIDAY
!
                  DAYWEK = DAY_OF_WEEK_OUT(DAY,MONTH)
                  CALL NERC_CALENDER_DAY_OF_WEEK(MONTH,DAY, &
                        THIS_YEAR_IS,DAYWEK)
                  IF(DAYWEK == 8) THEN
                     DAY_OF_WEEK_OUT(DAY,MONTH) = DAYWEK
                  ELSEIF(DAY_OF_WEEK_OUT(DAY,MONTH) > 7) THEN

                     DAY_OF_WEEK_OUT(DAY,MONTH) = &
                                 DAY_OF_WEEK_OUT(MAX(1,DAY-1),MONTH) + 1

                  ENDIF
               ELSE
                  CURRENT_DAY = DAY
                  IF(CURRENT_DAY == DAY_OF_MONTH(COUNTER(1),1)) THEN
                     DAY_TYPE = 1
                     COUNTER(1) = COUNTER(1) + 1
                  ELSEIF(CURRENT_DAY == DAY_OF_MONTH(COUNTER(2),2)) THEN
                     DAY_TYPE = 2
                     COUNTER(2) = COUNTER(2) + 1
                  ELSE
                     DAY_TYPE = 3
                     COUNTER(3) = COUNTER(3) + 1
                  ENDIF
                  DAY_OF_WEEK_OUT(DAY,MONTH) = DAY_OF_WEEK(CURRENT_DAY)
               ENDIF
               POS_OF_HOURLY_FILE = POS_OF_HOURLY_FILE + 1
               DO HOUR = 1 , 24
                  

                  HOUR_IN_FCLOD= STARTING_(DAY_TYPE) + &
                        COUNTER(DAY_TYPE) -1 + DAYS_PER_MONTH*(HOUR-1) + &
                                                FIRST_SEASON_HOUR - 1


                  IF(.NOT. ADD_DAY) THEN
                     AFTER_DSM_DAILY_LOADS(HOUR) =  NINT(LEAP_SLOPE* &
                                FRCLOD(HOUR_IN_FCLOD) + LEAP_INTERCEPT)

                     LAST_ERROR = FLOAT(AFTER_DSM_DAILY_LOADS(HOUR)) - &
                        LEAP_SLOPE*FRCLOD(HOUR_IN_FCLOD)-LEAP_INTERCEPT
                  ELSE

                     AFTER_DSM_DAILY_LOADS(HOUR) = NINT(LEAP_SLOPE* &
                        FORECAST_LOADS(HOUR,ADD_DAY_TYPE,MONTH,7) + &
                        LEAP_INTERCEPT)
                     LAST_ERROR = FLOAT(AFTER_DSM_DAILY_LOADS(HOUR)) - &
                        LEAP_SLOPE* &
                        FORECAST_LOADS(HOUR,ADD_DAY_TYPE,MONTH,7) - &
                        LEAP_INTERCEPT
                  ENDIF
                  HOURLY_ERROR = HOURLY_ERROR + LAST_ERROR
                  IF(ABS(HOURLY_ERROR) > 1.0 .AND. ABS(LAST_ERROR)>.35 &
                     .AND. HOURLY_ERROR * LAST_ERROR > 0.0 ) THEN
                     IF(FRCLOD(HOUR_IN_FCLOD) < PEAK_AFTER_DSM(MONTH) &
                         .AND. FRCLOD(HOUR_IN_FCLOD) > &
                                          BASE_AFTER_DSM(MONTH)) THEN
                        IF(HOURLY_ERROR > 0.0 ) THEN
                           AFTER_DSM_DAILY_LOADS(HOUR) = &
                              AFTER_DSM_DAILY_LOADS(HOUR) - 1
                           HOURLY_ERROR = HOURLY_ERROR - 1
                        ELSE
                           AFTER_DSM_DAILY_LOADS(HOUR) = &
                              AFTER_DSM_DAILY_LOADS(HOUR) + 1

                           HOURLY_ERROR = HOURLY_ERROR + 1
                        ENDIF
                     ENDIF
                  ENDIF


                  HOUR_IN_MONTH = &
                                HOUR + 24*(DAY-1) + FIRST_SEASON_HOUR-1



       

       CHRONO_HOURS_PER_MONTH(MONTH) = &
        CHRONO_HOURS_PER_MONTH(MONTH) + 1

       CHRONOLOGICAL_HOUR(HOUR_IN_MONTH) =  HOUR_IN_MONTH - &
                                        FIRST_SEASON_HOUR + 1

       CHRONOLOGICAL_LOAD(HOUR_IN_MONTH) = &
        AFTER_DSM_DAILY_LOADS(HOUR)



               ENDDO ! END OF WRITES-TO-HOURLY_OUT

               hla_returned=hourly_load_out()
               
               IF(hla_returned > 0) then
                  WRITE(29,REC=POS_OF_HOURLY_FILE) MONTH,DAY, &
                     CURRENT_YEAR, &
                     EEICODE,DAY_OF_WEEK_OUT(DAY,MONTH),TIMZON,TEMPER, &
                     DELTMP,AFTER_DSM_DAILY_LOADS
               end if
               ADD_DAY = .FALSE.
            ENDDO ! END OF DAILY WRITES-TO-DSM

            IF(MONTH == 2 .AND. .NOT. &
                        (CALENDER_CORRECT .AND. LEAP_MONTH) ) THEN

               IF(DAY_OF_WEEK(29) > 0 .AND. DAY_OF_WEEK(29) < 6) THEN
                  DAY_TYPE = 1
               ELSE
                  DAY_TYPE = 2
               ENDIF
               POS_OF_HOURLY_FILE = POS_OF_HOURLY_FILE + 1
               DO HOUR = 1 , 24
                  AFTER_DSM_DAILY_LOADS(HOUR) = NINT(LEAP_SLOPE* &
                     FORECAST_LOADS(HOUR,DAY_TYPE,2,7) + LEAP_INTERCEPT)
               ENDDO
               IF(DAY_OF_WEEK_OUT(DAY,MONTH) > 6) THEN
                  DAY_OF_WEEK_OUT(DAY,MONTH) = 1
               ELSE
                  DAY_OF_WEEK_OUT(DAY,MONTH) = &
                                         DAY_OF_WEEK_OUT(DAY,MONTH) + 1
               ENDIF
               IF(HOURLY_LOAD_OUT() > 0) &
                  WRITE(29,REC=POS_OF_HOURLY_FILE) MONTH,DAY, &
                     CURRENT_YEAR, &
                     EEICODE,DAY_OF_WEEK_OUT(DAY,MONTH),TIMZON,TEMPER, &
                     DELTMP,AFTER_DSM_DAILY_LOADS
            ELSEIF(MONTH == 12) THEN
               DO I = 1 ,12
                  PEAK_MAX = MAX(PEAK_MAX,PEAK_AFTER_DSM(I))
                  BASE_MIN = MIN(BASE_MIN,BASE_AFTER_DSM(I))
                  ENERGY_YEAR = ENERGY_YEAR + MONTHLY_ENERGY(I)
               ENDDO
               IF(HOURLY_LOAD_OUT() > 0) THEN

                  WRITE(29,REC = POS_OF_HOURLY_FILE+1) &
                                      PEAK_AFTER_DSM,PEAK_MAX
                  WRITE(29,REC = POS_OF_HOURLY_FILE+2) &
                                      BASE_AFTER_DSM,BASE_MIN
                  WRITE(29,REC = POS_OF_HOURLY_FILE+3) &
                                      MONTHLY_ENERGY,ENERGY_YEAR

               ENDIF
               POS_OF_HOURLY_FILE = POS_OF_HOURLY_FILE + 3
               REWIND(27)
            ENDIF ! MONTH ==2 OR MONTH == 12
         ENDIF ! END IF WRITE-TO-DSM STUFF
!
         IF(MONTH == 1) THEN

            CUM_CHRONO_HOURS_PER_MONTH(MONTH) = &
                                           CHRONO_HOURS_PER_MONTH(MONTH)

         ELSE
            CUM_CHRONO_HOURS_PER_MONTH(MONTH) = &
                  CUM_CHRONO_HOURS_PER_MONTH(MONTH-1) + &
                                           CHRONO_HOURS_PER_MONTH(MONTH)
          ENDIF
!

         IF(WRITE_TO_DSM_DATA_BASE) then
           CALL STORE_CLASS_MW_RESERVE_ALLOC(MONTH, &
                  PEAK_HOUR_AFTER_DSM,PEAK_DAY_AFTER_DSM, &
                  PEAK_BEFORE_DSM(MONTH))
         end if
         !  CHRONO_HOURS_PER_MONTH(1) = 744/
         if(chrono_hours_per_month(month)>744) then
            chrono_hours_per_month=chrono_hours_per_month
         end if
            
      ENDDO ! END MONTHLY LOOP ON THE LOADS
!
      IF((CONTROL_AREA_FORECAST() .OR. CLASS_BASED_FORECAST())  .AND. &
                             LDMGT_ACTIVE) CALL STORE_CLASS_PEAK_NET_DSM
      IF(WRITE_TO_DSM_DATA_BASE) CALL FREE_UP_DSM_DATA_BASE
      RETURN


!
      ENTRY GET_CHRONO_HOUR_DISTRIBUTION(R_MONTH,TEMP_HOURLY_INDEX)
!



!
         TEMP_HOURS = CHRONO_HOURS_PER_MONTH(R_MONTH)
         IF(R_MONTH == 1) THEN
            TEMP_CUM_HOURS = 1
         ELSE
            TEMP_CUM_HOURS = CUM_CHRONO_HOURS_PER_MONTH(R_MONTH-1) + 1
         ENDIF
         ALLOCATE (TEMP_CHRONO_LOAD(TEMP_HOURS))

! REPLACE THE CMORE WITH BELOW 9/6/01

         DO HR = 1, TEMP_HOURS
            TEMP_CHRONO_LOAD(HR) = &
                                CHRONOLOGICAL_LOAD(TEMP_CUM_HOURS+HR-1)
            TEMP_HOURLY_INDEX(HR) = &
                                CHRONOLOGICAL_HOUR(TEMP_CUM_HOURS+HR-1)
         ENDDO
         call write_sic_trace("snszmds",temp_hours)
         CALL SortIncrPos(TEMP_HOURS,TEMP_HOURLY_INDEX,TEMP_CHRONO_LOAD)

         IF(ALLOCATED(TEMP_CHRONO_LOAD)) DEALLOCATE(TEMP_CHRONO_LOAD)
      RETURN



      ENTRY GET_CUM_HOURS_N_HOURS_IN_MONTH(R_MONTH,R_HOURS,R_CUM_HOURS)
         ! DEBUG
         string_dbg=" "
         do index_dbg=1,ubound(chrono_hours_per_month,1)
            string_dbg=trim(string_dbg) // &
            trim(itos(int(chrono_hours_per_month(index_dbg)))) 
            if(index_dbg<ubound(chrono_hours_per_month,1)) then
                string_dbg=trim(string_dbg)//","
            end if

         end do
         ! END DEBUG
         string_dbg="[" // trim(string_dbg) // "]"
         

         R_HOURS = CHRONO_HOURS_PER_MONTH(R_MONTH)

         if(r_hours>744) then
            er_message="snszmds:0001 - R_Hours value of " // &
            trim(itos(int(r_hours))) // " is invalid."
            
            call end_program(er_message)
         end if
         IF(R_HOURS <= 0) THEN
            R_HOURS = get_HOURS_IN_PERIOD(R_MONTH)
            IF(R_MONTH == 1) THEN
               R_CUM_HOURS = 1
            ELSE
               R_CUM_HOURS = CUM_HOURS_AT_THE_END_OF(R_MONTH-1) + 1
            ENDIF
         ELSE
            IF(R_MONTH == 1) THEN
               R_CUM_HOURS = 1
            ELSE
               R_CUM_HOURS = CUM_CHRONO_HOURS_PER_MONTH(R_MONTH-1) + 1
            ENDIF
         ENDIF
      RETURN

      ENTRY GET_CHRONO_HOURS_PER_MONTH(R_MONTH,R_CHRONO_HOURS_PER_MONTH)

         R_CHRONO_HOURS_PER_MONTH = CHRONO_HOURS_PER_MONTH(R_MONTH)
            if(r_chrono_hours_per_month>744) then
                call end_program ("snszmds:002 - r_chrono_hours/month " // &
      "value of " // trim(itos(int(r_chrono_hours_per_month))) // &
      " is invalid.")
            end if

      RETURN


      ENTRY GET_DAY_OF_WEEK_IN_MONTH(R_DAY_IN_MONTH,R_MONTH, &
                                                          R_DAY_OF_WEEK)

         R_DAY_OF_WEEK = DAY_OF_WEEK_OUT(R_DAY_IN_MONTH,R_MONTH)
      RETURN
      END


      SUBROUTINE READ_CALENDAR_FROM_REF_FILE(R_LDE_EXISTS)
      implicit none
      
      INTEGER (kind=2) ::  HOURLY_LOAD_IN,REC_LENGTH
      CHARACTER (len=2) ::  LOAD_FILE_CHAR_EXT
      CHARACTER (len=5) ::  BSYRLOAD
      CHARACTER (len=256) ::  REF_FILE_NAME,BASE_FILE_DIRECTORY, &
                    REFERENCE_LOAD_FULL_NAME
      LOGICAL (kind=4) ::  LDE_EXISTS
      LOGICAL (kind=1) ::  R_LDE_EXISTS,LAHEY_LF95,NEW_LDE_FORMAT
      INTEGER (kind=4) ::  REC_START

      INTEGER (kind=4) ::  IOS,REC
      INTEGER (kind=2) ::  MO,DA,CURRENT_YEAR,DAYWEK,DAY(12,31)
      CHARACTER (len=8) ::  EEICODE


      SAVE DAY
!
         IF(HOURLY_LOAD_IN() == 0) THEN
            REF_FILE_NAME = trim(BSYRLOAD())//".BIN"
         ELSE
            REF_FILE_NAME = trim(BSYRLOAD())&
                //".B"//LOAD_FILE_CHAR_EXT(HOURLY_LOAD_IN())
         ENDIF
         REF_FILE_NAME = trim(REFERENCE_LOAD_FULL_NAME())//"LDE"// &
            trim(REF_FILE_NAME)
         INQUIRE(FILE=REF_FILE_NAME,EXIST=LDE_EXISTS)
         R_LDE_EXISTS = LDE_EXISTS
         IF(LDE_EXISTS) THEN
            OPEN(10,FILE=REF_FILE_NAME,ACCESS="TRANSPARENT",STATUS="OLD")
            READ(10,REC=2) REC_LENGTH
            CLOSE(10)
            OPEN(10,FILE=REF_FILE_NAME,ACCESS="DIRECT",STATUS="OLD", &
                RECL=REC_LENGTH)
            IF(LAHEY_LF95()) THEN
               REC_START = 2
            ELSE
               REC_START = 1
            ENDIF
            NEW_LDE_FORMAT = REC_LENGTH > 80
            DO REC = REC_START, 366+REC_START-1
               IF(NEW_LDE_FORMAT) THEN
                  READ(10,REC=REC,IOSTAT=IOS)MO,DA,CURRENT_YEAR,EEICODE, &
                                             DAYWEK
               ELSE
                  READ(10,REC=REC,IOSTAT=IOS)MO,DA,CURRENT_YEAR,EEICODE, &
                                             DAYWEK
               ENDIF
               IF(IOS /= 0) EXIT
               IF(MO > 0 .AND. MO < 13 .AND. DA > 0 .AND. DA < 32) THEN
                  DAY(MO,DA) = DAYWEK
               ENDIF
            ENDDO
            CLOSE(10)
            R_LDE_EXISTS = MO == 12 .AND. DA == 13
         ENDIF
      RETURN

      END SUBROUTINE READ_CALENDAR_FROM_REF_FILE



      SUBROUTINE DSM_WARN(year_ord,PK_DAY,OFF_DAY,MONTH,endpt_ord,MWDIF)
      use mod_base_year
      implicit none
      INTEGER (kind=2) ::  INT_MWDIF
      REAL ::  MWDIF
      INTEGER (kind=2) :: year_ord,PK_DAY,OFF_DAY,MONTH,endpt_ord
      CHARACTER (len=20) ::  MONTH_NAME
      CHARACTER (len=9) ::  DAY_NAME
!
!
               INT_MWDIF = -1*NINT(MWDIF)
               WRITE(4,*) 'In the month of ', &
               trim(MONTH_NAME(MONTH)), &
                ' in YEAR_ord',get_BASE_YEAR()+year_ord,','
               WRITE(4,*) 'for end point',endpt_ord


               WRITE(4,*) 'the system forecast shifted peak from '
               WRITE(4,*) trim(DAY_NAME(PK_DAY)),' to ', &
                  trim(DAY_NAME(OFF_DAY)),' by ',INT_MWDIF, ' MWs '
               WRITE(4,*)  'due to load management modifications.'
               WRITE(4,*) 'Check the level of load reductions in the '
               WRITE(4,*) 'Load Acceptance and Load Response files.'
               WRITE(4,*) ' '

      RETURN
      END


      SUBROUTINE DAYWEEK(MONTH,DAY,year_ord,DAYWEK)

      INTEGER (kind=2) :: MONTH,DAY,year_ord
      INTEGER (kind=2) ::  DAYWEK,DAYS(12),DAY_ADJ,OFFSET,ITEMP
      DATA DAYS/0,31,60,91,121,152,182,213,244,274,305,335/
      DAY_ADJ = 0
      OFFSET = 4
      IF(MOD(int(year_ord,2),int(4,2)) .NE. 0 .AND. MONTH .GT. 2) then
        DAY_ADJ = 1
      end if
      IF(MOD(int(year_ord,2),int(4,2)) .EQ. 0) then
        OFFSET  = 3
      end if
      ITEMP = INT((year_ord-1980)/4)
      ITEMP = ITEMP+year_ord-1983+DAYS(MONTH)-DAY_ADJ+DAY+OFFSET
      DAYWEK = MOD(ITEMP,int(7,2)) + 1


      RETURN
      END


      SUBROUTINE CALENDER_DAY_OF_WEEK(R_MONTH,R_DAY,R_YEAR,DAYWEK)
      INTEGER (kind=2) ::    NEW_YEARS
      INTEGER (kind=2) ::    PRESIDENTS,MEMORIAL,FORTH,LABOR,COLUMBUS, &
                     VETERANS,THANKS_GIVING,CHRISTMAS,MARTIN_LUTHER_KING
      INTEGER (kind=2) ::    MONTH
      INTEGER (kind=2) ::    R_MONTH
      INTEGER (kind=2) ::    DAY,R_DAY,YEAR_ord,R_YEAR,DAYWEK,R_DAYWEK, &
                  MONTHLY_HOLIDAYS(12),HOLIDAYS(12)
      SAVE        NEW_YEARS,PRESIDENTS,MEMORIAL,FORTH,LABOR,COLUMBUS, &
                  VETERANS,THANKS_GIVING,CHRISTMAS,MONTHLY_HOLIDAYS, &
                  MARTIN_LUTHER_KING
         MONTH = R_MONTH
         DAY = R_DAY
         year_ord = R_YEAR
         CALL DAYWEEK(MONTH,DAY,year_ord,DAYWEK)
         IF(MONTHLY_HOLIDAYS(1) >= 1 .AND. &
               MONTH == 1 .AND. DAY == NEW_YEARS) DAYWEK = 8
         IF(MONTHLY_HOLIDAYS(2) >= 1 .AND. &
               MONTH == 2 .AND. DAY == PRESIDENTS) DAYWEK = 8
         IF(MONTHLY_HOLIDAYS(5) >= 1 .AND. &
               MONTH == 5 .AND. DAY == MEMORIAL) DAYWEK = 8
         IF(MONTHLY_HOLIDAYS(7) >= 1 .AND. &
               MONTH == 7 .AND. DAY == FORTH) DAYWEK = 8
         IF(MONTHLY_HOLIDAYS(9) >= 1 .AND. &
               MONTH == 9 .AND. DAY == LABOR) DAYWEK = 8
         IF(MONTHLY_HOLIDAYS(10) >= 1 .AND. &
               MONTH == 10 .AND. DAY == COLUMBUS) DAYWEK = 8
         IF(MONTHLY_HOLIDAYS(11) >= 2 .AND. &
               MONTH == 11 .AND. DAY == VETERANS) DAYWEK = 8
         IF(MONTHLY_HOLIDAYS(11) >= 1 .AND. &
               MONTH == 11 .AND. DAY == THANKS_GIVING) DAYWEK = 8
         IF(MONTHLY_HOLIDAYS(12) >= 1 .AND. &
               MONTH == 12 .AND. DAY == CHRISTMAS) DAYWEK = 8

      RETURN


      ENTRY NERC_CALENDER_DAY_OF_WEEK(R_MONTH,R_DAY,R_YEAR,DAYWEK)

         MONTH = R_MONTH
         DAY = R_DAY

         year_ord = R_YEAR
         IF(MONTH == 1 .AND. DAY == NEW_YEARS) DAYWEK = 8
!
         IF(MONTH == 5 .AND. DAY == MEMORIAL) DAYWEK = 8
         IF(MONTH == 7 .AND. DAY == FORTH) DAYWEK = 8
         IF(MONTH == 9 .AND. DAY == LABOR) DAYWEK = 8
         IF(MONTH == 11 .AND. DAY == THANKS_GIVING) DAYWEK = 8

         IF(MONTH == 12 .AND. DAY == CHRISTMAS) DAYWEK = 8
      RETURN


      ENTRY SAVE_HOLIDAYS(R_YEAR,HOLIDAYS)

         DO MONTH = 1 , 12
            MONTHLY_HOLIDAYS(MONTH) = HOLIDAYS(MONTH)
         ENDDO

      ENTRY SET_HOLIDAYS(R_YEAR)


         year_ord = R_YEAR
! FIND NEW YEARS

         MONTH = 1
         DAY = 1
         CALL DAYWEEK(MONTH,DAY,year_ord,R_DAYWEK)
         IF(R_DAYWEK.LT.6) NEW_YEARS = 1
         IF(R_DAYWEK.EQ.6) NEW_YEARS = 3
         IF(R_DAYWEK.EQ.7) NEW_YEARS = 2
! FIND CHRISTMAS
         MONTH = 12
         DAY = 25
         CALL DAYWEEK(MONTH,DAY,year_ord,R_DAYWEK)
         IF(R_DAYWEK.LT.6) CHRISTMAS = 25
         IF(R_DAYWEK.EQ.6) CHRISTMAS = 27
         IF(R_DAYWEK.EQ.7) CHRISTMAS = 26
! FIND THE FORTH OF JULY
         MONTH = 7
         DAY = 4
         CALL DAYWEEK(MONTH,DAY,year_ord,R_DAYWEK)
         IF(R_DAYWEK.LT.6) FORTH = 4
         IF(R_DAYWEK.EQ.6) FORTH = 6
         IF(R_DAYWEK.EQ.7) FORTH = 5
! FIND VETERANS DAY ALWAYS 11/11
         MONTH = 11
         DAY = 11
         CALL DAYWEEK(MONTH,DAY,year_ord,R_DAYWEK)
         IF(R_DAYWEK.LT.6) VETERANS = 11
         IF(R_DAYWEK.EQ.6) VETERANS = 13
         IF(R_DAYWEK.EQ.7) VETERANS = 12
! FIND PRESIDENTS DAY THE MONDAY BEFORE OR ON THE 22
         MONTH = 2
         DAY = 1
         CALL DAYWEEK(MONTH,DAY,year_ord,R_DAYWEK)
         PRESIDENTS = 23 - R_DAYWEK
         IF(R_DAYWEK.EQ.1) PRESIDENTS = 15
! FIND COLUMBUS DAY
         MONTH = 10
         DAY = 1
         CALL DAYWEEK(MONTH,DAY,year_ord,R_DAYWEK)
         COLUMBUS = 16 - R_DAYWEK
         IF(R_DAYWEK.EQ.1) COLUMBUS = 8
! FIND MEMORIAL DAY THE LAST MONDAY IN MAY
         MONTH = 5
         DAY = 1
         CALL DAYWEEK(MONTH,DAY,year_ord,R_DAYWEK)
         IF(R_DAYWEK.LT.6) MEMORIAL = 30 - R_DAYWEK
         IF(R_DAYWEK.EQ.6) MEMORIAL = 31
         IF(R_DAYWEK.EQ.7) MEMORIAL = 30
! FIND LABOR DAY THE FIRST MONDAY IN SEPTEMBER
         MONTH = 9
         DAY = 1
         CALL DAYWEEK(MONTH,DAY,year_ord,R_DAYWEK)
         IF(R_DAYWEK.GT.1) LABOR = 9 - R_DAYWEK
         IF(R_DAYWEK.EQ.1) LABOR = 1
! FIND THANKSGIVING DAY THE FORTH THURSDAY IN NOVEMBER
         MONTH = 11
         DAY = 1
         CALL DAYWEEK(MONTH,DAY,year_ord,R_DAYWEK)
         IF(R_DAYWEK.LT.5) THANKS_GIVING = 26 - R_DAYWEK
         IF(R_DAYWEK.EQ.5) THANKS_GIVING = 28
         IF(R_DAYWEK.EQ.6) THANKS_GIVING = 27
         IF(R_DAYWEK.EQ.7) THANKS_GIVING = 26
      RETURN
      END

      FUNCTION SETUP_DAY_OF_WEEK_4(YEAR_ord)
      use mod_base_year
      use forecast
      INTEGER (kind=2) ::  GET_DAY_OF_WEEK_4
      INTEGER (kind=2) :: MONTH,DAY,year_ord,CALANDER_DAY_OF_WEEK
      INTEGER (kind=2) ::  CALENDAR_YEAR,MO,DA,SETUP_DAY_OF_WEEK_4
      LOGICAL (kind=1) ::  YES_CALANDER_CORRECT
      INTEGER (kind=2) ::  DAY_WEEK_CALENDAR(12,31),R_DAY_OF_WEEK
      LOGICAL (kind=1) ::  NERC_REGION_BASED_FORECAST
!


         IF(YES_CALANDER_CORRECT()) THEN
            CALENDAR_YEAR = year_ord
         ELSE
            CALENDAR_YEAR = get_base_year()
         ENDIF
         CALL SET_HOLIDAYS(CALENDAR_YEAR)
         DO MO = 1, 12
            DO DA = 1, 31
               CALL DAYWEEK(MO,DA,CALENDAR_YEAR,CALANDER_DAY_OF_WEEK)
               CALL NERC_CALENDER_DAY_OF_WEEK(MO,DA,CALENDAR_YEAR, &
                    CALANDER_DAY_OF_WEEK)
               DAY_WEEK_CALENDAR(MO,DA) = CALANDER_DAY_OF_WEEK
            ENDDO
         ENDDO
         SETUP_DAY_OF_WEEK_4 = CALENDAR_YEAR
      RETURN

      ENTRY GET_DAY_OF_WEEK_4(MONTH,DAY)

         IF(SYSTEM_BASED_FORECAST() .OR. &
              .NOT. NERC_REGION_BASED_FORECAST()) THEN
            CALL GET_DAY_OF_WEEK_IN_MONTH(DAY,MONTH,R_DAY_OF_WEEK)
            GET_DAY_OF_WEEK_4 = R_DAY_OF_WEEK
         ELSE
            GET_DAY_OF_WEEK_4 = DAY_WEEK_CALENDAR(MONTH,DAY)
         ENDIF
      END


!
      SUBROUTINE DAYS_BY_DAY_TYPE(year_ord,MONTH,DAYS_PER_MONTH, &
      HOLIDAYS, WEEK_DAYS,PEAK_DAYS,WEEK_END_DAYS)


      INTEGER (kind=2) :: year_ord,MONTH,WEEK_DAYS,PEAK_DAYS, &
      WEEK_END_DAYS, HOLIDAYS, DAYS_PER_MONTH
      
      PEAK_DAYS = 4
      CALL WEEKEND_DAYS(year_ord,MONTH,DAYS_PER_MONTH,WEEK_END_DAYS)
      WEEK_END_DAYS = WEEK_END_DAYS + HOLIDAYS ! CONSISTENT DEFINITION
      WEEK_DAYS = DAYS_PER_MONTH - WEEK_END_DAYS ! CONSISTENT DEFINITION
      RETURN
      END

      SUBROUTINE WEEKEND_DAYS(year_ord,MONTH,DAYS_PER_MONTH, &
      WEEK_END_DAYS)
         INTEGER (kind=2) :: WEEK_END_DAYS,year_ord,DAYS_PER_MONTH,MONTH, &
             DAYWEK
         CALL DAYWEEK(MONTH,INT(1,2),year_ord,DAYWEK)
         WEEK_END_DAYS = 8
         IF(MONTH .EQ. 2) THEN
            IF(DAYWEK > 5 .AND. DAYS_PER_MONTH .EQ. 29) WEEK_END_DAYS=9
         ELSE
            IF(DAYWEK == 6) WEEK_END_DAYS = 10
            IF(DAYWEK == 7) WEEK_END_DAYS = 9
            IF(DAYWEK == 5) WEEK_END_DAYS = 8 + DAYS_PER_MONTH - 29
            IF(DAYWEK == 4) WEEK_END_DAYS = 8 + DAYS_PER_MONTH - 30
         ENDIF
      RETURN
      END


      subroutine SortIncrPos(iSup,iPtr,a) 
      ! this is adopted from facalg with a sort index
      use end_routine, only: end_program, er_message
      use debugtrace
      implicit none


! sorts iSup items into ascending order based on values in key array a;
! Shell sort adapted from the source on page 110 of
! SOFTWARE TOOLS IN PASCAL, by Kernighan & Plauger, Addison-Wesley 1981

      integer (kind=2) ::  iSup,i,j,k,Gap,iPtrj,iPtr(*)
      real (kind=4) ::  a(*)
      integer :: file_trace_sip=0
      
      if(file_trace_sip==0) then
        file_trace_sip=open_trace("snszmds.trace", rq_sip)
      end if
      call write_trace_message(file_trace_sip, "IN...")
      call write_trace_int2(file_trace_sip, "isup", isup)
      
      Gap=iSup/2
      do while(Gap>0)
         do i=Gap,iSup
            j=i-Gap
            do while(j>0)
               k=j+Gap
               iPtrj=iPtr(j)
               if(iPtrj>iSup .or. iPtrj<1) then
                  WRITE(4,*) "Inside mc sort, bad value is",iPtrj
                  WRITE(4,*) '*** line 1055 SNSZMDS.FOR ***'
                  write(4,*) "iPtr        a"
                  do j = 1, iSup
                     Write(4,*) iPtr(j),a(j)
                  enddo
                  call SEE_WARNING_MESSAGES()
                  er_message='Stop requested from SNSZMDS SIID270'
                  call end_program(er_message)
               endif
               if(a(iPtrj)<=a(iPtr(k))) then ! exit the j-loop
                  j=0
               else
                  iPtr(j)=iPtr(k)
                  iPtr(k)=iPtrj
               end if
               j=j-Gap
            end do ! j loop
         end do ! i loop
         Gap=Gap/2
      end do ! GAP>0
      
      call write_trace_message(file_trace_sip, "OUT...")
      call write_trace_int2(file_trace_sip, "isup", isup)
      
      return
      end ! subroutine SortIncr






























