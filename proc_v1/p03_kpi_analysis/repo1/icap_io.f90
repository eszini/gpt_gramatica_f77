
      ! Function moved from cn_objt.f90
      FUNCTION READ_ICAP_FILE()
      use end_routine, only: end_program, er_message
      USE CAPACITY_OPTIONS_ALLOC_VARS
      USE ICAP_RESULTS  ! VARIABLE USED IN CLREPORT AND HERE
      USE SIZECOM
      use globecom
	  use mod_base_year
	  use string
      use program_state
      use miscmod
      use capacity_arrays
      use debugtrace
	  implicit none
      integer :: file_trace_rif=0
      logical :: called_get_icap_rv=.false.
      integer :: times_marginal_icap
	  
      
    
      INTEGER (kind=2) :: delete_loc,I,R_YEAR, &
                  loc_year, &
                  R_PA, &
                  MAX_PA, &
                  CM, &
                  MAX_CM, &
                  K, &
                  GET_PLANNING_AREA_POSITION, &
                  GET_CAPACITY_MARKET_POSITION, &
                  GET_REGIONAL_PA_NAME, &
                  GET_REGIONAL_CM_NAME, &
                  GET_ICAP_SWITCH_INDEX, &
                  R_MRX_MARGINAL_ICAP, &
                  REG_PARAM_ID(9999)
      integer (kind=2), save :: PA_loc=0
      integer (kind=2) :: pa_was
      integer (kind=2) :: pa_tries=0
      INTEGER (kind=4) ::  IOS,ICAP
      INTEGER (kind=1) ::  J
      LOGICAL (kind=1) ::  READ_ICAP_FILE,ICAP_FILE_ACTIVE=.FALSE. , &
                YES_REGIONAL_PARAMS_EXIST, &
                ACTIVE_PA(:), &
                ACTIVE_CM(:), &
               GET_ICAP_REPORT_VALUES, &
               CALC_ANN_ICAP_VALUES, &
               GREEN_MRX=.FALSE. , &
               MARGINAL_ICAP
      CHARACTER (len=2) ::  GREEN_MRX_METHOD
      REAL ::  ADJUSTMENT_PEAK,TARGET_RATIO
      REAL ::  ADJUSTMENT_BASE_CAPACITY,ADJUSTMENT_PEAK_CAPACITY
      REAL ::  ADJ_PK2,ADJ_PK1,PLANNING_PEAK_GROWTH_RATE
      REAL ::  ICAP_MIN_TESTING_MARGIN,ICAP_MAX_TESTING_MARGIN, &
           LOCAL_ICAP_CAPACITY_COST(10),LOCAL_ICAP_CAPACITY_RATIO(10), &
           GET_ICAP_REVENUE_MULT,LOCAL_RATIO,R_RESERVE_MARGIN, &
           LOCAL_WEIGHT,MAX_TESTING_MARGIN,MIN_TESTING_MARGIN, &
           R_CURRENT_TARGET_RATIO,PEAK_COIN_FACTOR, &
           LEVELIZED_ICAP_MARKET,LOCAL_MINIMUM_TESTING_RM, &
           LOCAL_MAXIMUM_TESTING_RM,R_ICAP_CONE_VALUE, &
           R_INSTALLED_CAPACITY_VALUE,R_EAS_REVENUE_OFFSET, &
           R_MRX_EAS,R_ADJUSTED_CAPACITY_VALUE,R_ICAP_MAX_MRX_OR_CONE, &
           R_ICAP_MRX_CONE,R_ICAP_RESERVE_MARGIN,R_ICAP_DEMAND_POINT, &
           R_ICAP_VALUE_POINT,RESERVE,RATIO, &
           LOWER_X,LOWER_Y,INCREMENT_ABOVE_LOWER,DELTA_X,DELTA_Y, &
           DOWN_FROM_LOWER_Y,Local_EAS_Revenue_Input,TEMP_R4
!
      REAL ::  TARGETS(:,:),ADJ_BASE_CAP(:),ADJ_PEAK_CAP(:), &
           PEAK_ADJUSTMENTS(:),ADJUSTMENT_CAPACITY, &
           PEAK_GROWTH_RATE(:), &
           MINIMUM_TESTING_RM(:,:), &
           MAXIMUM_TESTING_RM(:,:), &
           ICAP_CONE_VALUE(:,:), &
           EAS_Revenue_Input(:,:)
      CHARACTER (len=10) ::  Local_Type_of_New_Entry, tne, &
                   Local_Type_of_EAS_Revenue, &
                   Type_of_New_Entry(:,:), &
                   Type_of_EAS_Revenue(:,:)

      CHARACTER (len=20) ::  REGIONAL_PA_NAME(:), &
                   LOCAL_REGIONAL_PA_NAME, &
                   R_REGIONAL_PA_NAME
      CHARACTER (len=35) ::  & !  092011.
                   REGIONAL_CM_NAME(:), &
                   R_REGIONAL_CM_NAME
      ALLOCATABLE :: TARGETS,ADJ_BASE_CAP,ADJ_PEAK_CAP,PEAK_ADJUSTMENTS, &
           PEAK_GROWTH_RATE,MINIMUM_TESTING_RM,MAXIMUM_TESTING_RM, &
           ICAP_CONE_VALUE, &
           ACTIVE_PA, &
           ACTIVE_CM, &
           REGIONAL_PA_NAME, &
           REGIONAL_CM_NAME, &
           Type_of_New_Entry, &
           Type_of_EAS_Revenue, &
           EAS_Revenue_Input
      SAVE TARGETS,ADJ_BASE_CAP,ADJ_PEAK_CAP,PEAK_ADJUSTMENTS, &
           PEAK_GROWTH_RATE,MINIMUM_TESTING_RM,MAXIMUM_TESTING_RM, &
           ICAP_CONE_VALUE, &
           Type_of_New_Entry, &
           Type_of_EAS_Revenue, &
           EAS_Revenue_Input, &
           ACTIVE_PA, &
           ACTIVE_CM, &
           REGIONAL_PA_NAME, &
           REGIONAL_CM_NAME
!
!  REGIONAL PLANNING AND ICAP INFORMATION
!  ONE REGION FOR NOW
!
      integer :: lb_1, lb_2, ub_1, ub_2
      logical :: is_valid_pa_was ! External
      integer (kind=2) :: lpa, pap
      
      INTEGER (kind=2) ::  REGIONAL_PLANNING_AREA_ID, &
                GET_NUMBER_OF_PLANNING_GROUPS, &
                GET_NUMBER_OF_CAPACITY_MARKETS, &
                PG=1 ,CG=1 
      REAL ::  MIN_CAP2PEAK_RATIO(:,:),MAX_CAP2PEAK_RATIO(:,:), &
           ICAP_CAPACITY_COST(:,:,:), &
           ICAP_CAPACITY_RATIO(:,:,:), &
           ICAP_ANN_RESERVE(:), &
           ICAP_ANN_DEMAND(:)
      ALLOCATABLE :: MIN_CAP2PEAK_RATIO,MAX_CAP2PEAK_RATIO, &
           ICAP_CAPACITY_COST, &
           ICAP_CAPACITY_RATIO, &
           ICAP_ANN_RESERVE, &
           ICAP_ANN_DEMAND
      SAVE MIN_CAP2PEAK_RATIO,MAX_CAP2PEAK_RATIO, &
           ICAP_CAPACITY_COST, &
           ICAP_CAPACITY_RATIO, &
           ICAP_ANN_RESERVE, &
           ICAP_ANN_DEMAND, &
           MAX_PA
      
      integer (kind=2) :: by
      character (len=1024) :: filename

!
! END DATA DECLARATIONS
!
         if(file_trace_rif==0) then
            file_trace_rif=open_trace("read_icap_file.trace", rq_rif)

         end if
         
         
         CALL OPEN_CAPACITY_ICAP_FILE(10,ICAP_FILE_ACTIVE)
         MAX_PA = 0
         IF(.NOT. ICAP_FILE_ACTIVE) THEN
            READ_ICAP_FILE = .FALSE.
         ELSE
!
            PG = MAX(GET_NUMBER_OF_PLANNING_GROUPS(),INT(1,2))
            CG = 3* MAX(GET_NUMBER_OF_CAPACITY_MARKETS(),INT(1,2)) 
!
            IF(ALLOCATED(TARGETS)) THEN
               DEALLOCATE(TARGETS,ADJ_BASE_CAP,ADJ_PEAK_CAP, &
                          PEAK_ADJUSTMENTS,PEAK_GROWTH_RATE, &
                          MINIMUM_TESTING_RM,MAXIMUM_TESTING_RM, &
                          ICAP_CONE_VALUE, &
                          Type_of_New_Entry, &
                          Type_of_EAS_Revenue, &
                          EAS_Revenue_Input, &
                          MIN_CAP2PEAK_RATIO,MAX_CAP2PEAK_RATIO, &
                          ICAP_CAPACITY_COST,ICAP_CAPACITY_RATIO, &
                          ICAP_ANN_RESERVE,ICAP_ANN_DEMAND, &
                          ACTIVE_PA, &
                          ACTIVE_CM, &
                          REGIONAL_PA_NAME, &
                          REGIONAL_CM_NAME)
            ENDIF
            ALLOCATE(TARGETS(3,get_globecom_study_period()), &
             ADJ_BASE_CAP(get_globecom_study_period()),ADJ_PEAK_CAP(get_globecom_study_period()), &
             PEAK_ADJUSTMENTS(get_globecom_study_period()), &
             PEAK_GROWTH_RATE(get_globecom_study_period()), &
             MINIMUM_TESTING_RM(get_globecom_study_period(),PG), &
             MAXIMUM_TESTING_RM(get_globecom_study_period(),PG),REGIONAL_PA_NAME(PG), &
             ! ONE PLANNING AREA FOR NOW
             MIN_CAP2PEAK_RATIO(PG,get_globecom_study_period()), & 
             MAX_CAP2PEAK_RATIO(PG,get_globecom_study_period()),ACTIVE_PA(PG), &
             ICAP_CONE_VALUE(get_globecom_study_period(),CG), &
             Type_of_New_Entry(get_globecom_study_period(),CG), &
             Type_of_EAS_Revenue(get_globecom_study_period(),CG), &
             EAS_Revenue_Input(get_globecom_study_period(),CG), &
             ICAP_CAPACITY_COST(CG,10,get_globecom_study_period()), & 
             ICAP_CAPACITY_RATIO(CG,10,get_globecom_study_period()), &
             ICAP_ANN_RESERVE(CG),ICAP_ANN_DEMAND(CG),ACTIVE_CM(CG), &
             REGIONAL_CM_NAME(CG))
            K = 0
!
            ICAP_CAPACITY_COST = 0.
            ICAP_CAPACITY_RATIO = 0.
            REG_PARAM_ID = 0
!
!
            DO I = 1, PG
               ACTIVE_PA(I) = .FALSE.
            ENDDO
            DO I = 1, CG
               ACTIVE_CM(I) = .FALSE.
            END DO
!
            CG_COUNT = 0
            
        call write_trace_int2(file_trace_rif, "1. CG_COUNT", CG_COUNT)
!
!
            DO
               
               pa_tries=pa_tries+1
               K = K + 1
               filename=get_filename_from_unit(10) ! For debugging
               
               READ(10,REC=K,IOSTAT=IOS) delete_loc, &
                                        REGIONAL_PLANNING_AREA_ID, &
                                        LOCAL_MINIMUM_TESTING_RM, &
                                        LOCAL_MAXIMUM_TESTING_RM, &
                                        LOCAL_ICAP_CAPACITY_COST, &
                                        LOCAL_ICAP_CAPACITY_RATIO, &
                                        PEAK_COIN_FACTOR, &
                                        LEVELIZED_ICAP_MARKET, &
                                        LOCAL_REGIONAL_PA_NAME, &
                                        Local_Type_of_New_Entry, &
                                        Local_EAS_Revenue_Input, &
                                        Local_Type_of_EAS_Revenue


               if(regional_planning_area_id/=127) then
                regional_planning_area_id=regional_planning_area_id 

                
               end if 
               IF(IOS /= 0) then

                EXIT
               end if

               I = MAX(INT(1,2),delete_loc - get_BASE_YEAR())

               IF(I > get_globecom_study_period()) then
                   CYCLE
               end if
               if(regional_planning_area_id==4) then
                
               end if

               if(REGIONAL_PLANNING_AREA_ID==0) then
                call end_program("icap_io:0018 - REGIONAL_PLANNING_AREA_ID " // &
                "is zero.")
               end if
               pa_was=PA_loc
               

               if(REGIONAL_PLANNING_AREA_ID==4) then

                REGIONAL_PLANNING_AREA_ID=REGIONAL_PLANNING_AREA_ID ! Debugstop
               end if
               
               PA_loc = REGIONAL_PLANNING_AREA_ID
               by=get_base_year()


               if(regional_planning_area_id==4) then
                REGIONAL_PLANNING_AREA_ID=REGIONAL_PLANNING_AREA_ID ! Debugstop
               end if

               pa_was=pa_loc
               PA_loc=GET_PLANNING_AREA_POSITION(PA_loc)

               

               IF(I == 1) THEN
                  IF(REGIONAL_PLANNING_AREA_ID > 0 .AND. &
                      REGIONAL_PLANNING_AREA_ID < 10000) THEN
                     IF(REG_PARAM_ID(REGIONAL_PLANNING_AREA_ID) > 0) THEN
                        WRITE(4,*) 'IN THE REGIONAL PLANNING PARAMETERS'
                        WRITE(4,*) 'FILE, THERE IS A REDUNDANT PLANNING'
                        WRITE(4,*) 'AREA ID',REGIONAL_PLANNING_AREA_ID
                     ENDIF
                     REG_PARAM_ID(REGIONAL_PLANNING_AREA_ID) = &
                         REG_PARAM_ID(REGIONAL_PLANNING_AREA_ID) + 1
                  ELSE
                        WRITE(4,*) 'IN THE REGIONAL PLANNING PARMETERS'
                        WRITE(4,*) 'FILE, THERE IS AN INVALID PLANNING'
                        WRITE(4,*) 'AREA ID',REGIONAL_PLANNING_AREA_ID
                        er_message='Stop requested from CN_OBJT SIID40'
                        call end_program(er_message)
                  ENDIF
               ENDIF
               IF(PA_loc > 0 .AND. PA_loc <= PG) THEN
                  ACTIVE_PA(PA_loc) = .TRUE.
!               
! DOUBLE INDEX MATCHES CALLING ROUTINES.
                  MAX_PA = MAX(MAX_PA,PA_loc)
                  ! Max_pa=1/1

                  IF(I == 1) THEN ! INDEX ON THE FIRST ENCOUNTER
                     REGIONAL_PA_NAME(PA_loc) = LOCAL_REGIONAL_PA_NAME
                  ENDIF
                  MINIMUM_TESTING_RM(I,PA_loc) = LOCAL_MINIMUM_TESTING_RM
                  MAXIMUM_TESTING_RM(I,PA_loc) = LOCAL_MAXIMUM_TESTING_RM

                  IF(MINIMUM_TESTING_RM(I,PA_loc) > 99999.) &
                      MINIMUM_TESTING_RM(I,PA_loc) = TARGETS(3,I)
                  IF(MAXIMUM_TESTING_RM(I,PA_loc) > 99999.) &
                      MAXIMUM_TESTING_RM(I,PA_loc) = 1.4*TARGETS(3,I)
               ENDIF
!
               CM = REGIONAL_PLANNING_AREA_ID
               CM = GET_CAPACITY_MARKET_POSITION(CM)
               IF(CM > 0 .AND. CM <= CG) THEN
                  ACTIVE_CM(CM) = .TRUE.
!
! DOUBLE INDEX MATCHES CALLING ROUTINES.
!
                  MAX_CM = MAX(MAX_CM,CM)
!
                  IF(I == 1) THEN ! INDEX ON THE FIRST ENCOUNTER
                     REGIONAL_CM_NAME(CM) = LOCAL_REGIONAL_PA_NAME
                     CG_COUNT = CG_COUNT + 1
        call write_trace_int2(file_trace_rif, "2. CG_COUNT", CG_COUNT)
                  ENDIF
!
                  DO J = 1, 10
                     ICAP_CAPACITY_COST(CM,J,I) = LOCAL_ICAP_CAPACITY_COST(J)
                     ICAP_CAPACITY_RATIO(CM,J,I) = LOCAL_ICAP_CAPACITY_RATIO(J)
                  ENDDO
!
                  ICAP_CONE_VALUE(I,CM) = LEVELIZED_ICAP_MARKET
          Local_Type_of_New_Entry=ucase(Local_Type_of_New_Entry)

                  Type_of_New_Entry(I,CM) = Local_Type_of_New_Entry
                  Type_of_EAS_Revenue(I,CM) = Local_Type_of_EAS_Revenue
                  EAS_Revenue_Input(I,CM) = Local_EAS_Revenue_Input
               ENDIF ! CM

            ENDDO

            CLOSE(10)
            GREEN_MRX =  GREEN_MRX_METHOD() == 'GX'
            READ_ICAP_FILE = I == get_globecom_study_period()
         ENDIF

      RETURN

      ENTRY CALC_ANN_ICAP_VALUES(R_YEAR)

         IF(ALLOCATED(ICAP_CONE_VALUE)) THEN
        call write_trace_int2(file_trace_rif, "3. CG_COUNT", CG_COUNT)
            DO CM = 1, CG_COUNT ! CG

                  CALL GET_CAPACITY_MARKET_MARGIN(CM,RESERVE)

               ICAP_ANN_RESERVE(CM) = RESERVE
               IF(RESERVE < -998.0) THEN ! PER BURESH. 04/30/08.
                  ICAP_ANN_DEMAND(CM) = 1.0
               ELSEIF(RESERVE <= ICAP_CAPACITY_RATIO(CM,1,R_YEAR)) THEN
                  J = 1
                  ICAP_ANN_DEMAND(CM) = ICAP_CAPACITY_COST(CM,J,R_YEAR)
               ELSEIF(RESERVE >= ICAP_CAPACITY_RATIO(CM,10,R_YEAR)) THEN
                  J = 10
                  ICAP_ANN_DEMAND(CM) = ICAP_CAPACITY_COST(CM,J,R_YEAR)
               ELSE
                  DO J = 2, 10
                     IF(RESERVE > ICAP_CAPACITY_RATIO(CM,J,R_YEAR)) CYCLE
                     DELTA_Y =  ICAP_CAPACITY_COST(CM,J,R_YEAR) - &
                         ICAP_CAPACITY_COST(CM,J-1,R_YEAR)
                     IF(ABS(DELTA_Y) < .0001) THEN
                        ICAP_ANN_DEMAND(CM) = ICAP_CAPACITY_COST(CM,J,R_YEAR)
                        EXIT
                     ENDIF
!
                     LOWER_X = ICAP_CAPACITY_RATIO(CM,J-1,R_YEAR)
                     LOWER_Y = ICAP_CAPACITY_COST(CM,J-1,R_YEAR)
                     INCREMENT_ABOVE_LOWER = RESERVE - LOWER_X
! NEED MONOTONIC X
                     DELTA_X =  ICAP_CAPACITY_RATIO(CM,J,R_YEAR) - &
                         ICAP_CAPACITY_RATIO(CM,J-1,R_YEAR)
                     IF(DELTA_X > 0.) THEN
                        DOWN_FROM_LOWER_Y = &
                            INCREMENT_ABOVE_LOWER*DELTA_Y/DELTA_X

                        ICAP_ANN_DEMAND(CM) = DOWN_FROM_LOWER_Y + LOWER_Y
                        EXIT
                     ELSE
                        WRITE(4,*) 'ICAP RESERVE FUNCTION NOT MONOTONIC'
                        ICAP_ANN_DEMAND(CM) = 0.0
                     ENDIF
!
                  ENDDO
                  IF(J == 11) THEN
                     WRITE(4,*) 'ICAP PRICING DID NOT FIND A VALID POINT'
                     er_message='Stop requested from CN_OBJT SIID41'
                     call end_program(er_message)
                  ENDIF
               ENDIF
            END DO
            CALC_ANN_ICAP_VALUES = .TRUE.
         ELSE
!
            CALC_ANN_ICAP_VALUES = .FALSE.
         ENDIF

      RETURN

      ENTRY GET_ICAP_SWITCH_INDEX(  R_PA,R_YEAR, &
          R_INSTALLED_CAPACITY_VALUE)

         IF(ALLOCATED(ICAP_CONE_VALUE).AND. &
             R_PA > 0 .AND. R_PA <= MAX_CM) THEN
            If(INDEX(Type_of_New_Entry(R_YEAR,R_PA),'CONE')/= 0) Then
               GET_ICAP_SWITCH_INDEX = 1
               If(INDEX(Type_of_New_Entry(R_YEAR,R_PA),'MRX') /= 0) Then
                  GET_ICAP_SWITCH_INDEX = 3
                  ICAP_CONE_VALUE(R_YEAR,R_PA) = LOWEST_CONE_COST_BY_CM(R_PA)/12.
               endif
               R_INSTALLED_CAPACITY_VALUE = 12. * ICAP_CONE_VALUE(R_YEAR,R_PA)
            Elseif(TRIM(Type_of_New_Entry(R_YEAR,R_PA)) == 'NONE') Then
               GET_ICAP_SWITCH_INDEX = 0
               R_INSTALLED_CAPACITY_VALUE = 0.0
            Else ! ASSUME CALCULATE BASED ON MRX PRICE
               GET_ICAP_SWITCH_INDEX = 2
               R_INSTALLED_CAPACITY_VALUE = 0.0
            Endif
         ELSE
            GET_ICAP_SWITCH_INDEX = 2
            R_INSTALLED_CAPACITY_VALUE = 0.0
         ENDIF
         if(PA_loc==0) then
            call end_program("icap_io:0029 - PA_loc is invalid.")
         end if
      RETURN

      ENTRY GET_ICAP_REPORT_VALUES(R_PA, &
                                   R_YEAR, &
                                   R_ICAP_CONE_VALUE, &
                                   R_ICAP_MRX_CONE, &
                                   R_ICAP_RESERVE_MARGIN, &
                                   R_ICAP_DEMAND_POINT, &
                                   R_ICAP_VALUE_POINT, &
                                   R_INSTALLED_CAPACITY_VALUE, &
                                   R_EAS_REVENUE_OFFSET, &
                                   R_ADJUSTED_CAPACITY_VALUE, &
                                   R_MRX_EAS, &
                                   R_MRX_MARGINAL_ICAP)
          if(r_year==0) then
            call end_program("icap_io:0036 - r_year argument must " // &
            " be nonzero here.")
          end if
          

          GET_ICAP_REPORT_VALUES_called=.true.

         GET_ICAP_REPORT_VALUES = .FALSE.

         PA_loc=r_pa


         if(PA_loc==0) then
             call end_program("icap_io:0022- PA_loc is zero.")
         end if
         R_MRX_MARGINAL_ICAP = 0

         IF(ALLOCATED(ICAP_CONE_VALUE) .AND. &
             PA_loc > 0 .AND. PA_loc <= MAX_CM) THEN


            IF(GREEN_MRX) THEN
               tne=Type_of_New_Entry(R_YEAR,PA_loc)
               If(INDEX(tne,'CONE') /= 0) Then
                  If(INDEX(tne,'MRX') /= 0) Then
                     ICAP_CONE_VALUE(R_YEAR,PA_loc)  = &
                                        LOWEST_CONE_COST_BY_CM(PA_loc)/12.

! 042617.

                     R_MRX_MARGINAL_ICAP = 1
                  ELSE
                     R_EAS_REVENUE_OFFSET = 0.0
                  endif
                  R_INSTALLED_CAPACITY_VALUE = &
                                              ICAP_CONE_VALUE(R_YEAR,PA_loc)
!
! 042617. NEEDS TO BE SAVED BY CM.
!
                  R_ICAP_CONE_VALUE = ICAP_CONE_VALUE(R_YEAR,PA_loc)
                


                  ! 1s to be used in the demand adjustment curve.
                  R_ICAP_DEMAND_POINT = ICAP_ANN_DEMAND(PA_loc) 
                  R_ADJUSTED_CAPACITY_VALUE = R_INSTALLED_CAPACITY_VALUE

                  R_ICAP_VALUE_POINT = R_ICAP_DEMAND_POINT * &
                      R_ADJUSTED_CAPACITY_VALUE
               Elseif(INDEX(Type_of_New_Entry(R_YEAR,PA_loc),'MARGINAL')&
                    /= 0) Then
                  if(PA_loc==4) then
                      r_icap_value_point=r_icap_value_point ! Debugstop
                  end if
                  CALL GET_INSTALL_CAP_VALUE_LAST_CM(PA_loc,TEMP_R4)
                  ! pa=2/asdf
                  ! temp_r4=0
                  if(PA_loc==4) then
                      r_icap_value_point=r_icap_value_point ! Debugstop
                      ! type_of_new_entry(1,4)=marginal/asdf
                  end if
                  R_INSTALLED_CAPACITY_VALUE = TEMP_R4
                  R_MRX_MARGINAL_ICAP = 1
                  R_EAS_REVENUE_OFFSET = R_MRX_EAS
                  R_ADJUSTED_CAPACITY_VALUE = MAX(0.0, &
                     R_INSTALLED_CAPACITY_VALUE - R_EAS_REVENUE_OFFSET)
                  R_ICAP_CONE_VALUE = ICAP_CONE_VALUE(R_YEAR,PA_loc)
                  R_ICAP_DEMAND_POINT = ICAP_ANN_DEMAND(PA_loc)
                  R_ICAP_VALUE_POINT = R_ICAP_DEMAND_POINT * &
                      R_ADJUSTED_CAPACITY_VALUE
! ONLY THREE OPTIONS
               Else 
                  R_INSTALLED_CAPACITY_VALUE = 0.0
                  R_EAS_REVENUE_OFFSET = 0.0
                  R_ADJUSTED_CAPACITY_VALUE = 0.0
                  R_ICAP_CONE_VALUE = ICAP_CONE_VALUE(R_YEAR,PA_loc)
                  R_ICAP_DEMAND_POINT = ICAP_ANN_DEMAND(PA_loc)
                  R_ICAP_VALUE_POINT = 0.0
               Endif
               IF(ICAP_ANN_RESERVE(PA_loc) > 0.001) THEN
                  R_ICAP_RESERVE_MARGIN = (ICAP_ANN_RESERVE(PA_loc) - 1.0)*100.0
               ELSE
                  R_ICAP_RESERVE_MARGIN = 0.0
               ENDIF
!
            ELSE ! NOT GRX
               If(INDEX(Type_of_New_Entry(R_YEAR,PA_loc),'CONE')/=0) Then
                  If(INDEX(Type_of_New_Entry(R_YEAR,PA_loc),'MRX') /= 0) Then
                     ICAP_CONE_VALUE(R_YEAR,PA_loc)  = &
                        LOWEST_CONE_COST_BY_CM(PA_loc)/12.
                  endif
                  R_INSTALLED_CAPACITY_VALUE = ICAP_CONE_VALUE(R_YEAR,PA_loc)
               Elseif(TRIM(Type_of_New_Entry(R_YEAR,PA_loc)) == 'MRX') Then
                  R_INSTALLED_CAPACITY_VALUE = R_ICAP_MRX_CONE
               Elseif(TRIM(Type_of_New_Entry(R_YEAR,PA_loc)) == &
                   'MAXIMUM') Then
                  R_INSTALLED_CAPACITY_VALUE = max(R_ICAP_MRX_CONE, &
                      ICAP_CONE_VALUE(R_YEAR,PA_loc))
               Elseif(TRIM(Type_of_New_Entry(R_YEAR,PA_loc)) ==  &
                   'MINIMUM') Then
                  R_INSTALLED_CAPACITY_VALUE = min(R_ICAP_MRX_CONE, &
                  ICAP_CONE_VALUE(R_YEAR,PA_loc))
               Elseif(TRIM(Type_of_New_Entry(R_YEAR,PA_loc)) == &
                   'AVERAGE') Then
                  R_INSTALLED_CAPACITY_VALUE = (R_ICAP_MRX_CONE + &
                      ICAP_CONE_VALUE(R_YEAR,PA_loc))* 0.5
               Elseif(TRIM(Type_of_New_Entry(R_YEAR,PA_loc)) == &
                    'MARGINAL') Then


                  CALL GET_INSTALL_CAP_VALUE_LAST_CM(PA_loc,TEMP_R4)

                  R_INSTALLED_CAPACITY_VALUE = TEMP_R4
                  R_MRX_MARGINAL_ICAP = 1
               Elseif(TRIM(Type_of_New_Entry(R_YEAR,PA_loc)) == 'NONE') Then
                  R_INSTALLED_CAPACITY_VALUE = 0.0
               Endif
!
               If(TRIM(Type_of_EAS_Revenue(R_YEAR,PA_loc)) == 'EAS') Then
                  R_EAS_REVENUE_OFFSET = EAS_Revenue_Input(R_YEAR,PA_loc)
               Elseif(TRIM(Type_of_EAS_Revenue(R_YEAR,PA_loc)) == 'MRX') Then
                  R_EAS_REVENUE_OFFSET = R_MRX_EAS
               Elseif(TRIM(Type_of_EAS_Revenue(R_YEAR,PA_loc)) == 'MAXIMUM') Then
                  R_EAS_REVENUE_OFFSET = &
                    max(R_MRX_EAS,ICAP_CONE_VALUE(R_YEAR,PA_loc))
               Elseif(TRIM(Type_of_EAS_Revenue(R_YEAR,PA_loc)) == 'MINIMUM') Then
                  R_EAS_REVENUE_OFFSET = &
                    min(R_MRX_EAS,ICAP_CONE_VALUE(R_YEAR,PA_loc))
               Elseif(TRIM(Type_of_EAS_Revenue(R_YEAR,PA_loc)) == 'AVERAGE') Then
                  R_EAS_REVENUE_OFFSET = (R_MRX_EAS + &
                    ICAP_CONE_VALUE(R_YEAR,PA_loc))* 0.5
               Elseif(TRIM(Type_of_EAS_Revenue(R_YEAR,PA_loc)) == 'NONE') Then
                  R_EAS_REVENUE_OFFSET = 0.0
               Endif
               R_ADJUSTED_CAPACITY_VALUE = MAX(0.0, &
                   R_INSTALLED_CAPACITY_VALUE - R_EAS_REVENUE_OFFSET)
               R_ICAP_CONE_VALUE = ICAP_CONE_VALUE(R_YEAR,PA_loc)

               R_ICAP_DEMAND_POINT = ICAP_ANN_DEMAND(PA_loc)
               R_ICAP_VALUE_POINT = R_ICAP_DEMAND_POINT * &
                   R_ADJUSTED_CAPACITY_VALUE

! FIND THE INTERVAL FOR THE CAP RATIO
               IF(ICAP_ANN_RESERVE(PA_loc) > 0.001) THEN
                  R_ICAP_RESERVE_MARGIN = &
                      (ICAP_ANN_RESERVE(PA_loc) - 1.0)*100.0
               ELSE
                  R_ICAP_RESERVE_MARGIN = 0.0
               ENDIF
            ENDIF
         ELSE
            R_ICAP_CONE_VALUE = 0.0
            R_ICAP_CONE_VALUE = MAX(R_ICAP_CONE_VALUE,0.0)
            IF(RESERVE > 0.001 .AND. ALLOCATED(ICAP_ANN_RESERVE)) THEN
               R_ICAP_RESERVE_MARGIN = &
                   (ICAP_ANN_RESERVE(PA_loc) - 1.0)*100.0
            ELSE
               R_ICAP_RESERVE_MARGIN = 0.0
            ENDIF
            R_ICAP_DEMAND_POINT = 0.0
            R_ICAP_VALUE_POINT = 0.0
         ENDIF
         if(PA_loc==0) then
            call end_program("icap_io:0030 - PA_loc is invalid.")
         end if
      RETURN

      ENTRY MARGINAL_ICAP(R_YEAR,R_PA)

! NOW LOOPS ON CM NOT PA_loc
         times_marginal_icap=times_marginal_icap+1
         IF(ALLOCATED(ICAP_CONE_VALUE) .AND. &
             R_PA > 0 .AND. R_PA <= MAX_CM) THEN
             if(r_pa==1) then
                r_pa=r_pa ! Debugstop

             end if

             
             PA_loc=r_pa


            if(times_marginal_icap==4) then
                ! R_YEAR=1/asdf
                ! R_PA=4/asdf
                r_pa=r_pa ! Debugstop
            end if
           if(pa_loc==4) then
            times_marginal_icap=times_marginal_icap ! Debugstop
           end if
         if(PA_loc==0) then
             call end_program("icap_io:0023- PA is zero.")
         end if

            if(TRIM(Type_of_New_Entry(R_YEAR,PA_loc)) == 'MARGINAL' .OR. &
                 (INDEX(Type_of_New_Entry(R_YEAR,PA_loc),'CONE') /= 0 .AND. &
                   INDEX(Type_of_New_Entry(R_YEAR,PA_loc),'MRX') /= 0)) Then
               MARGINAL_ICAP = .TRUE.
            Else
               MARGINAL_ICAP = .FALSE.
            Endif
         ENDIF
         if(PA_loc==0) then
            call end_program("icap_io:0031 - PA_loc is invalid.")
         end if
      RETURN

      ENTRY GET_REGIONAL_PA_NAME(R_PA,R_REGIONAL_PA_NAME)
      

         if(.not. GET_ICAP_REPORT_VALUES_called) then
            er_message="icap_io:0035 - GET_ICAP_REPORT_VALUES() " // &
                "has not been called."
            call end_program(er_message)
         end if
         
         IF(R_PA > 0 .AND. R_PA <= MAX_PA) THEN


            IF(REGIONAL_PA_NAME(R_PA)(1:6) /= 'AREA  ') THEN
               GET_REGIONAL_PA_NAME = 1
               R_REGIONAL_PA_NAME = REGIONAL_PA_NAME(R_PA)
               ! pa_tre/pa_tre
            ELSE
               GET_REGIONAL_PA_NAME = 0
            ENDIF
         ELSE
            GET_REGIONAL_PA_NAME = 0
            R_REGIONAL_PA_NAME = 'INVALID PA_loc NAME'
         ENDIF
         ! get_pa=4/adsf first time through
        if(PA_loc==0) then
           ! Smoking gun
           call end_program("icap_io:0032 - PA_loc is invalid.")
        end if
      RETURN

      ENTRY GET_REGIONAL_CM_NAME(R_PA,R_REGIONAL_CM_NAME)

         IF(R_PA > 0 .AND. R_PA <= MAX_CM) THEN
            IF(REGIONAL_CM_NAME(R_PA)(1:6) /= 'AREA  ') THEN
! Regional CM Name initialized to "AREA"
               GET_REGIONAL_CM_NAME = 1
               R_REGIONAL_CM_NAME = REGIONAL_CM_NAME(R_PA)
            ELSE
               GET_REGIONAL_CM_NAME = 0
            ENDIF
         ELSE
            GET_REGIONAL_CM_NAME = 0
            R_REGIONAL_CM_NAME = 'INVALID CM NAME'
         ENDIF

      RETURN

      ENTRY GET_ICAP_REVENUE_MULT(R_YEAR,R_PA,R_RESERVE_MARGIN)

! NOW COMES IN AS CM (CAPACITY MARKET)
         GET_ICAP_REVENUE_MULT = 0.

         IF(ICAP_FILE_ACTIVE .AND. R_PA > 0 .AND. R_PA <= MAX_CM) THEN

            

           

            
            lb_1=lbound(Type_of_New_Entry,1)
            ub_1=ubound(Type_of_New_Entry,1)
            lb_2=lbound(Type_of_New_Entry,2)
            ub_2=ubound(Type_of_New_Entry,2)
            

            
            If(TRIM(Type_of_New_Entry(R_YEAR,PA_loc)) /= 'NONE') Then
               IF(R_YEAR > get_globecom_study_period()) THEN
                  loc_year = get_globecom_study_period()
               ELSE
                  loc_year = R_YEAR
               ENDIF
               LOCAL_RATIO = R_RESERVE_MARGIN
               IF(LOCAL_RATIO < &
                 ICAP_CAPACITY_RATIO(R_PA,1,loc_year)) THEN
                  GET_ICAP_REVENUE_MULT = &
                      ICAP_CAPACITY_COST(R_PA,1,loc_year)
               ELSE IF(LOCAL_RATIO > &
                   ICAP_CAPACITY_RATIO(R_PA,10,loc_year)) THEN
                  GET_ICAP_REVENUE_MULT = &
                      ICAP_CAPACITY_COST(R_PA,10,loc_year)
               ELSE
                  DO I = 1, 9
                     IF(LOCAL_RATIO >= &
                         ICAP_CAPACITY_RATIO(R_PA,I+1,loc_year)) then
                             CYCLE
                     end if
                     
                     IF(ICAP_CAPACITY_RATIO(R_PA,I+1,loc_year) /= &
                         ICAP_CAPACITY_RATIO(R_PA,I,loc_year))THEN
                        LOCAL_WEIGHT = &
                        (LOCAL_RATIO - &
                            ICAP_CAPACITY_RATIO(R_PA,I,loc_year)) / &
                             (ICAP_CAPACITY_RATIO(R_PA,I+1,loc_year) - &
                                 ICAP_CAPACITY_RATIO(R_PA,I,loc_year))
                        ELSE
                           LOCAL_WEIGHT = 0.
                     ENDIF
                     GET_ICAP_REVENUE_MULT = (1.- LOCAL_WEIGHT) * &
                        ICAP_CAPACITY_COST(R_PA,I,loc_year) + LOCAL_WEIGHT * &
                                ICAP_CAPACITY_COST(R_PA,I+1,loc_year)
                     EXIT
                  ENDDO
               ENDIF
            endif ! type == 'None'
!
!
         ENDIF
         if(PA_loc==0) then

            call end_program("icap_io:0034 - PA_loc is invalid.")
           
         end if
      RETURN

      ENTRY YES_REGIONAL_PARAMS_EXIST

         YES_REGIONAL_PARAMS_EXIST = ICAP_FILE_ACTIVE

      RETURN

      ENTRY ICAP_MAX_TESTING_MARGIN(R_YEAR,R_CURRENT_TARGET_RATIO,R_PA)

         IF(ICAP_FILE_ACTIVE .AND. R_PA > 0 .AND. R_PA <= MAX_PA) THEN
            IF(ACTIVE_PA(R_PA)) THEN
               IF(R_YEAR <= get_globecom_study_period()) THEN
                  ICAP_MAX_TESTING_MARGIN = MAXIMUM_TESTING_RM(R_YEAR,R_PA)
               ELSE
                  ICAP_MAX_TESTING_MARGIN = MAXIMUM_TESTING_RM(get_globecom_study_period(),R_PA)
               ENDIF
            ELSE
               ICAP_MAX_TESTING_MARGIN = R_CURRENT_TARGET_RATIO
            ENDIF
         ELSE
            ICAP_MAX_TESTING_MARGIN = R_CURRENT_TARGET_RATIO
         ENDIF
      RETURN

      ENTRY ICAP_MIN_TESTING_MARGIN(R_YEAR,R_CURRENT_TARGET_RATIO,R_PA)

         IF(ICAP_FILE_ACTIVE .AND. R_PA > 0 .AND. R_PA <= MAX_PA) THEN
            IF(ACTIVE_PA(R_PA)) THEN
               IF(R_YEAR <= get_globecom_study_period()) THEN
                  ICAP_MIN_TESTING_MARGIN = MINIMUM_TESTING_RM(R_YEAR,R_PA)
               ELSE
                  ICAP_MIN_TESTING_MARGIN = MINIMUM_TESTING_RM(get_globecom_study_period(),R_PA)
               ENDIF
            ELSE
               ICAP_MIN_TESTING_MARGIN = R_CURRENT_TARGET_RATIO 
            ENDIF
         ELSE
            ICAP_MIN_TESTING_MARGIN = R_CURRENT_TARGET_RATIO
         ENDIF


      END function READ_ICAP_FILE

