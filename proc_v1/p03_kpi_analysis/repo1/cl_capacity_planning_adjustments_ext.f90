      FUNCTION CL_CAPACITY_PLANNING_adj(R_YEAR,R_UNIT_NO, &
                                                ST,SUBTRACT_IT) &
                             RESULT(R_CL_CAPACITY_PLANNING_ADJUSTMENTS)
      use end_routine, only: end_program, er_message
      use logging
      use foshydi2com
      use forecast
      use globecom
      USE CLA_OBJT_ARRAYS
      use up_objt_decs
      use SpinDriftLib
      use prod_arrays_dimensions
      use sizecom
      use prodcom
      use prod2com
      use mod_base_year
      use tf_objt_interfaces
      use offline_trace
      SAVE


      
      CHARACTER (LEN=2) :: GREEN_MRX_METHOD
      REAL (KIND=4) :: R_CL_CAPACITY_PLANNING_ADJUSTMENTS
      REAL (KIND=4) :: UNIT_CAPACITY,CAP_MW,VOID_REAL4,R_POINTR, &
                       MONTH_MULT,FIRST_YEAR_CAPACITY
      INTEGER (KIND=2) :: UNIT_NO,R_UNIT_NO,YEAR_START,YEAR_END, &
        R_YEAR, &
                          ON_LINE_MONTH,TG,FT, &
                          UNIT_OFF_YEAR,PT_YR, &
                          LOCAL_POINTER,NG
      integer (kind=2) :: imm_year ! required
      LOGICAL (KIND=4) :: SUBTRACT_IT
      CHARACTER (LEN=20) :: MONTH_NAME,LOCAL_NAME
      LOGICAL*1 RESURRECT_RETROFIT_UNIT
      INTEGER (KIND=4) :: ST
      INTEGER (KIND=2), PARAMETER :: MAX_NEWGEN_INDEX = 10

      INTEGER (KIND=2) :: PEAK_MONTH,GET_TRANS_GROUP_POSITION
      integer (kind=2) :: pm

      REAL (KIND=4) :: PLAN_FACTOR,GET_VAR

! BEGINNING OF THE CAPACITY PLANNING SECTION

      if(r_year==0) then
        call end_program("CL_CAPACITY_PLANNING_adjustments_ext:0002 - " &
        // "r_year is zero.")
      end if
      
      ON_LINE_MONTH = ONLINE(R_UNIT_NO) - &
                                        100*INT(ONLINE(R_UNIT_NO)/100.)
      IF(.NOT. SUBTRACT_IT) THEN
         YEAR_START = R_YEAR - get_BASE_YEAR()
         IF(YEAR_START < 1) THEN
            YEAR_START = 1
            ON_LINE_MONTH = 1
         ENDIF
      if(year_start<1) then
        call end_program("CL_CAPACITY_PLANNING_adjustments_ext:0006 - " // &
            "year_start is invalid (zero).")
        
      end if
      if(get_globecom_study_period()==0) then
        call end_program("CL_CAPACITY_PLANNING_adjustments_ext:0009 - " // &
            "get_globecom_study_period() is zero.")
      end if
      IF(ON_LINE_MONTH > PEAK_MONTH(YEAR_START)) THEN
               WRITE(4,*) 'The on-line month, ', &
                          TRIM(MONTH_NAME(ON_LINE_MONTH)), &
                          ', for CL unit, ',TRIM(UNITNM(R_UNIT_NO))
              WRITE(4,"('&',A,I4,A)") ' is after the peak month in ', &
                           R_YEAR,' of '// &
                 TRIM(MONTH_NAME(PEAK_MONTH(YEAR_START)))//'.'
               WRITE(4,*) "The capacity of this unit is not in ", &
                          "the planning capacity for its on-line year."
               WRITE(4,*)


               TG = TRANSACTION_GROUP_ID(R_UNIT_NO)
               TG = GET_TRANS_GROUP_POSITION(TG)
               FT= PRIMARY_MOVER(R_UNIT_NO)
               UNIT_CAPACITY = INPUT_MW(2,R_UNIT_NO)
               CAP_MW = UNIT_CAPACITY
               PT_YR = 1
               UNIT_NO = R_UNIT_NO

               IF(CAP_MW < 0.) THEN
                  

                  CAP_MW = GET_CL_MW_FROM_POINTR(CAP_MW,PT_YR)
                  IF(CAP_MW < 0.) &
                      CAP_MW = &
                        GET_CL_MW_FROM_POINTR(CAP_MW,PEAK_MONTH(PT_YR))
                  CAP_MW = MAX(0.,CAP_MW)
               ELSE
                  CAP_MW = MAX(0.,CAP_MW)
               ENDIF
               if(get_globecom_study_period()==0) then
                call end_program( &
                    "CL_CAPACITY_PLANNING_adjustments_ext:0008 - " // &
                    "get_globecom_study_period() is zero.")
               end if
               
              ! pt_yr=asdf/0
              if(peak_month(pt_yr)==0) then

                call end_program( &
                    "CL_CAPACITY_PLANNING_adjustments_ext:0007 - " // &
                    "peak_month(pt_yr) cannot be zero.")
              end if

               
               IF(MONTHLY_CAPACITY_POINTER(UNIT_NO) /= 0 .AND. &
                                .NOT. CL_CAP_AREA_LINKED(UNIT_NO)) THEN
                  R_POINTR = &
                          FLOAT(ABS(MONTHLY_CAPACITY_POINTER(UNIT_NO)))
                  if(peak_month(pt_yr)==0) then
                    call end_program( &
                        "CL_CAPACITY_PLANNING_adjustments_ext:0005 - " // &
                        "peak_month(pt_yr) cannot be zero.")
                  end if
                  
                  MONTH_MULT = GET_CL_MW_FROM_POINTR(R_POINTR, &
                                                 PEAK_MONTH(PT_YR))
                  IF(MONTH_MULT > 2. .AND. CAP_MW /= 0.) THEN
                     CAP_MW = MONTH_MULT
                  ELSE
                     CAP_MW = MONTH_MULT * CAP_MW
                  ENDIF
               ELSEIF(MONTHLY_CAPACITY_POINTER(UNIT_NO) < 0 .AND. &
                                      CL_CAP_AREA_LINKED(UNIT_NO)) THEN
                  LOCAL_NAME = UNITNM(UNIT_NO)
                 LOCAL_POINTER = ABS(MONTHLY_CAPACITY_POINTER(UNIT_NO))
                  CAP_MW = MIN(GET_CLASS_PEAK_NET_DSM_FOR_CAP( &
                                PT_YR,LOCAL_NAME,LOCAL_POINTER),CAP_MW)
               ENDIF
	       ! Johncheck
               IF(foshyd%CAP_PLANNING_FAC(R_UNIT_NO) < 0.) THEN
             PLAN_FACTOR = GET_VAR(foshyd%CAP_PLANNING_FAC(R_UNIT_NO), &
                                                 PT_YR, &
                                                 UNITNM(R_UNIT_NO))
               ELSE
                  PLAN_FACTOR = foshyd%CAP_PLANNING_FAC(R_UNIT_NO)
               ENDIF
               CAP_MW = CAP_MW * foshyd%CAP_FRAC_OWN(UNIT_NO) * &
                                                    PLAN_FACTOR * 0.01

               IF (FT > 0 .AND. FT < 7) THEN
                  CL_TG_AFTER_PEAK(TG,YEAR_START) =      &
                               CL_TG_AFTER_PEAK(TG,YEAR_START) + CAP_MW
                  CL_TG_AFTER_PEAK(0,YEAR_START) = &
                                CL_TG_AFTER_PEAK(0,YEAR_START) + CAP_MW
               ENDIF
               YEAR_START = YEAR_START + 1
            ENDIF

         IF(OFF_LINE_YEAR(R_UNIT_NO) > gc_last_study_year) THEN
            YEAR_END = gc_last_study_year - get_BASE_YEAR()
         ELSE
            YEAR_END = OFF_LINE_YEAR(R_UNIT_NO) - get_BASE_YEAR()
            IF(YEAR_END < 1 .OR. R_UNIT_NO < 1 .OR. R_UNIT_NO &
                                                   > MAX_CL_UNITS) THEN
               WRITE(4,*) "The unit ",UNITNM(R_UNIT_NO)
               WRITE(4,*) "has an off-line year of ", &
                                               OFF_LINE_YEAR(R_UNIT_NO)
               WRITE(4,*) "while the base year is ",get_BASE_YEAR()
               WRITE(4,*) "First, check to make sure that the "
               WRITE(4,*) "Unit Production pointers in your "
               WRITE(4,*) "Capacity Options file are valid."
               WRITE(4,*) "If running Transact, make sure that the"
               WRITE(4,*) "resource options in your Expansion Plant"
               WRITE(4,*) "Operations file all have active"
               WRITE(4,*) "Transact Groups."
               WRITE(4,*) '*** line 3318 CLA_OBJT.FOR ***'
               er_message='See WARNING MESSAGES-cla_objt-2'
               call end_program(er_message)
            ENDIF
            IF(OFF_LINE_MONTH(R_UNIT_NO) < PEAK_MONTH(YEAR_END)) THEN
               WRITE(4,*) 'The off-line month, ', &
                         TRIM(MONTH_NAME(OFF_LINE_MONTH(R_UNIT_NO))), &
                          ', for CL unit, ',TRIM(UNITNM(R_UNIT_NO))
             WRITE(4,"('&',A,I4,A)")', is before the peak month in ', &
                           OFF_LINE_YEAR(R_UNIT_NO),' of '// &
             TRIM(MONTH_NAME(PEAK_MONTH(OFF_LINE_YEAR(R_UNIT_NO))))// &
                     '.'
               WRITE(4,*) "The capacity of this unit is not in ", &
                        "the planning capacity for its off-line year."
               WRITE(4,*)
               YEAR_END = YEAR_END - 1
            ENDIF
         ENDIF
         UNIT_CAPACITY = INPUT_MW(2,R_UNIT_NO)
         UNIT_OFF_YEAR = OFF_LINE_YEAR(R_UNIT_NO) - get_BASE_YEAR()

         UNIT_NO = R_UNIT_NO ! 4/11/95. GAT.
      ELSE !   GOTO 10
         UNIT_NO = R_UNIT_NO
         UNIT_CAPACITY = INPUT_MW(2,UNIT_NO)
         YEAR_START = R_YEAR - get_BASE_YEAR()
         YEAR_END=MIN(gc_last_study_year,OFF_LINE_YEAR(UNIT_NO))-get_BASE_YEAR()
         IF(.NOT. GREEN_MRX_METHOD() == 'GX') &
              OFLINE(UNIT_NO)=100*(R_YEAR-1900)+OFF_LINE_MONTH(UNIT_NO)
              
                    call write_offline_trace( &
         "CL_CAPACITY_PLANNING_adjustments_ext:0010", ofline(R_UNIT_NO))
      ENDIF ! 10 CONTINUE
         DO PT_YR = YEAR_START, YEAR_END

               CAP_MW = UNIT_CAPACITY


            IF(CAP_MW < 0.) THEN
               CAP_MW = GET_CL_MW_FROM_POINTR(CAP_MW,PT_YR)
! ADDED PER JONES. 3/13/98. GAT.
               IF(CAP_MW < 0.) &
                      CAP_MW = &
                        GET_CL_MW_FROM_POINTR(CAP_MW,PEAK_MONTH(PT_YR))

               CAP_MW = MAX(0.,CAP_MW)
            ELSE

! IF NO POINTER HAS BEEN SPECIFIED FOR THE PEAK MONTH

               CAP_MW = MAX(0.,CAP_MW)
            ENDIF

            IF(MONTHLY_CAPACITY_POINTER(UNIT_NO) /= 0 .AND. &
                                .NOT. CL_CAP_AREA_LINKED(UNIT_NO)) THEN
               R_POINTR = FLOAT(ABS(MONTHLY_CAPACITY_POINTER(UNIT_NO)))
               imm_year=PT_YR
               MONTH_MULT = GET_CL_MW_FROM_POINTR(R_POINTR, &
                                                 PEAK_MONTH(imm_year))
               IF(MONTH_MULT > 2. .AND. CAP_MW /= 0.) THEN
                  CAP_MW = MONTH_MULT
               ELSE
                  CAP_MW = MONTH_MULT * CAP_MW
               ENDIF
            ELSEIF(MONTHLY_CAPACITY_POINTER(UNIT_NO) < 0 .AND. &
                                      CL_CAP_AREA_LINKED(UNIT_NO)) THEN
               LOCAL_NAME = UNITNM(UNIT_NO)
               LOCAL_POINTER = ABS(MONTHLY_CAPACITY_POINTER(UNIT_NO))
               CAP_MW = MIN(GET_CLASS_PEAK_NET_DSM_FOR_CAP( &
                                PT_YR,LOCAL_NAME,LOCAL_POINTER),CAP_MW)
            ENDIF

! EFFECTIVE MW'S OF PLANNING CAPACITY
            !Johncheck
            IF(foshyd%CAP_PLANNING_FAC(R_UNIT_NO) < 0.) THEN
            PLAN_FACTOR = GET_VAR(foshyd%CAP_PLANNING_FAC(R_UNIT_NO), &
                                                      PT_YR, &
                                                     UNITNM(R_UNIT_NO))
            ELSE
               PLAN_FACTOR = foshyd%CAP_PLANNING_FAC(R_UNIT_NO)
            ENDIF
            CAP_MW = CAP_MW * foshyd%CAP_FRAC_OWN(UNIT_NO) * &
                                                    PLAN_FACTOR * 0.01

            IF(SUBTRACT_IT) CAP_MW = -CAP_MW

! THIS SECTION ADDS MW'S OF PLANNING CAPACITY

            IF(LDTYPE(UNIT_NO)=='M' .OR. &
                 LDTYPE(UNIT_NO)=='B' .OR. LDTYPE(UNIT_NO)=='N') THEN
               CL_ANN_CAP(1,PT_YR,ST) = CL_ANN_CAP(1,PT_YR,ST) + CAP_MW
               CL_ANN_CAP(2,PT_YR,ST) = CL_ANN_CAP(2,PT_YR,ST) + CAP_MW
               CL_ANN_CAP(3,PT_YR,ST) = CL_ANN_CAP(3,PT_YR,ST) + CAP_MW
            ELSE IF(LDTYPE(UNIT_NO) == 'C') THEN
               CL_ANN_CAP(2,PT_YR,ST) = CL_ANN_CAP(2,PT_YR,ST) + CAP_MW
               CL_ANN_CAP(3,PT_YR,ST) = CL_ANN_CAP(3,PT_YR,ST) + CAP_MW
            ELSE IF(LDTYPE(UNIT_NO) == 'D') THEN
               CL_ANNUAL_LOAD_REDUCTION(PT_YR) = &
                              CL_ANNUAL_LOAD_REDUCTION(PT_YR) + CAP_MW
            ELSE
               CL_ANN_CAP(3,PT_YR,ST) = CL_ANN_CAP(3,PT_YR,ST) + CAP_MW
            ENDIF
! 01/11/02. FOR NEW TG RESERVE MARGIN REPORT.
            TG=TRANSACTION_GROUP_ID(UNIT_NO)
            TG = GET_TRANS_GROUP_POSITION(TG)
            FT= PRIMARY_MOVER(UNIT_NO)
            NG= NEWGEN_INDEX(UNIT_NO)
         IF(TG < 1 .OR. TG > GET_NUMBER_OF_ACTIVE_GROUPS() ) THEN
              WRITE(4,*) "BAD TRANSACTION GROUP ID IN THERMAL UNITS", &
                                                        UNITNM(UNIT_NO)
            ENDIF
            IF(NG > 10) NG = 1 ! 070609. PER DOUG.
            IF (FT > 0 .AND. FT < 7) THEN
               CL_TG_CAP(FT,TG,PT_YR,ST) = &
                                     CL_TG_CAP(FT,TG,PT_YR,ST) + CAP_MW
               CL_TG_CAP(0,TG,PT_YR,ST) = &
                                      CL_TG_CAP(0,TG,PT_YR,ST) + CAP_MW
            ELSEIF(FT > 6) THEN ! 110807 FOR PAC: .OR. FT < 1) THEN
               CL_TG_CAP(6,TG,PT_YR,ST) = &
                                      CL_TG_CAP(6,TG,PT_YR,ST) + CAP_MW
               CL_TG_CAP(0,TG,PT_YR,ST) = &
                                      CL_TG_CAP(0,TG,PT_YR,ST) + CAP_MW
            ENDIF
            IF (NG > 0 .AND. NG < MAX_NEWGEN_INDEX) THEN
               NEWGEN_CAP_BY_INDEX(NG,TG,PT_YR) = &
                              NEWGEN_CAP_BY_INDEX(NG,TG,PT_YR) + CAP_MW
            ENDIF

            IF(PT_YR == YEAR_START) FIRST_YEAR_CAPACITY = ABS(CAP_MW)
         ENDDO
         IF(  (UNIT_OFF_YEAR <= get_globecom_study_period() .AND. &
            OFF_LINE_MONTH(UNIT_NO) < PEAK_MONTH(UNIT_OFF_YEAR)) .OR. &
               (UNIT_OFF_YEAR+1 <= get_globecom_study_period() .AND. &
                   OFF_LINE_MONTH(UNIT_NO) > &
                                       PEAK_MONTH(UNIT_OFF_YEAR))) THEN
            IF (FT > 0 .AND. FT < 10) THEN ! 7) THEN
               IF(OFF_LINE_MONTH(UNIT_NO) > &
                                        PEAK_MONTH(UNIT_OFF_YEAR)) THEN
                  CL_TG_RETIRE(TG,UNIT_OFF_YEAR+1) = &
                              CL_TG_RETIRE(TG,UNIT_OFF_YEAR+1) + CAP_MW
                  CL_TG_RETIRE(0,UNIT_OFF_YEAR+1) = &
                               CL_TG_RETIRE(0,UNIT_OFF_YEAR+1) + CAP_MW
               ELSE
                  CL_TG_RETIRE(TG,UNIT_OFF_YEAR) = &
                                CL_TG_RETIRE(TG,UNIT_OFF_YEAR) + CAP_MW
                  CL_TG_RETIRE(0,UNIT_OFF_YEAR) = &
                                 CL_TG_RETIRE(0,UNIT_OFF_YEAR) + CAP_MW
               ENDIF
            ENDIF
         ENDIF
         R_CL_CAPACITY_PLANNING_ADJUSTMENTS = FIRST_YEAR_CAPACITY
      RETURN

      ENTRY RESURRECT_RETROFIT_UNIT(R_YEAR,R_UNIT_NO)


! IMPLICITLY ASSUMES 25 YEAR LIFE

         OFLINE(R_UNIT_NO) = 100*(R_YEAR + 25 - 1900) + &
                                              OFF_LINE_MONTH(R_UNIT_NO)
         ONLINE(R_UNIT_NO) = 100*INT(ONLINE(R_UNIT_NO)/100.) + &
                  CO2_CONTROL_DATE(R_UNIT_NO) - &
                            100*INT(CO2_CONTROL_DATE(R_UNIT_NO)/100.)
         RESURRECT_RETROFIT_UNIT = .TRUE.
      RETURN
      END function CL_CAPACITY_PLANNING_adj

