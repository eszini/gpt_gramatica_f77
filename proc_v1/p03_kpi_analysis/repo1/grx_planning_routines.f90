
      MODULE GRX_PLANNING_ROUTINES
      USE PROD_ARRAYS_DIMENSIONS
      use grx_data
      use grx_extend
      use co2_data
	  use miscmod
	  use flusher
	  use cl_data
      implicit none

      
      REAL (KIND=4), save :: &
                         MRX_RPS_CL_CURVE_GROSS_MARGIN(MAX_CL_UNITS), &
                          MRX_RPS_CL_CURVE_REVENUE(MAX_CL_UNITS), &
                          MRX_RPS_CL_CURVE_COST(MAX_CL_UNITS), &
                          MRX_RPS_CL_CURVE_UMWH(MAX_CL_UNITS)
      REAL (KIND=4), SAVE, ALLOCATABLE :: &
                                   MRX_RPS_CL_CURVE_MWH(:,:), &
                                   MRX_RPS_CL_CURVE_CUM_MWH(:,:), &
                                   MRX_RPS_CL_CURVE_MARGIN(:,:), &
                                   MRX_RPS_CL_CURVE_CUM_MARGIN(:,:), &
                                   MRX_RPS_DV_CURVE_MWH(:,:), &
                                   MRX_RPS_DV_CURVE_CUM_MWH(:,:), &
                                   MRX_RPS_DV_CURVE_MARGIN(:,:), &
                                   MRX_RPS_DV_CURVE_CUM_MARGIN(:,:), &
! 062020. ALLOCATED IN CLREPORT.
                                   MRX_RPS_DV_CURVE_GROSS_MARGIN(:), &
                                   MRX_RPS_DV_CURVE_REVENUE(:), &
                                   MRX_RPS_DV_CURVE_COST(:), &
                                   MRX_RPS_DV_CURVE_UMWH(:),&
                                   MRX_RPS_MX_CURVE_MARGIN(:,:),&
                                   MRX_RPS_MX_CURVE_REVENUE(:,:),&
                                   MRX_RPS_MX_CURVE_COST(:,:), &
                                   MRX_RPS_MX_CURVE_UMWH(:,:), &
                                   MRX_RPS_MX_CURVE_MWH(:,:), &
                                   RPS_REC_PRICE_BY(:), &
                                   RPS_GEN_PRICE_BY(:)
    INTEGER (KIND=2),SAVE,ALLOCATABLE :: MRX_RPS_CL_CURVE_INDEX(:,:), &
                                           MRX_RPS_CL_UNIT_NO(:), &
                                         MRX_RPS_CL_UNIT_INDEX(:,:), &
                                         MRX_RPS_DV_CURVE_INDEX(:,:), &
                                           MRX_RPS_DV_UNIT_NO(:), &
                                          MRX_RPS_DV_UNIT_INDEX(:,:), &
                                           MRX_RPS_MX_OPTION_NO(:), &
                                         MRX_RPS_MX_CURVE_INDEX(:,:), &
                                        MRX_RPS_MX_OPTION_INDEX(:,:), &
                                          MRX_RPS_MX_NEXT_CN_INDEX(:,:)
 REAL (KIND=4),SAVE, ALLOCATABLE :: GRX_CO2_EMISS_REDUCT_REQUIRED(:)
      LOGICAL (KIND=4), PARAMETER :: STORE=.TRUE.,RESTORE=.FALSE.
      REAL (KIND=4),SAVE, ALLOCATABLE :: GRX_ITER_CO2_PRICE(:,:), &
                                           GRX_ITER_TG_CO2_EMISS(:,:)
      INTEGER (KIND=2), SAVE :: TG_MAX,GRX_NUNITS
      
      INTEGER (KIND=2), SAVE :: GRX_MAXIMUM_ITERATIONS
      LOGICAL (KIND=1), SAVE :: BALANCE_WITH_CO2_PRICE/.FALSE./
      INTEGER (KIND=8), SAVE :: H2_UNIT_ID_NUM(MAX_CL_UNITS)
      LOGICAL (KIND=1), ALLOCATABLE :: GRX_BOP_RETROFIT_ACTIVE(:)
      INTEGER (KIND=2), SAVE :: RETROFIT_LIST(MAX_CL_UNITS), &
        UNITS_RETROFITTED=0,RETROFIT_TYPE_FIXED_GRX(MAX_CL_UNITS)
      REAL (KIND=4), SAVE :: RETROFIT_CO2_DECISION_COST(MAX_CL_UNITS)
      
      REAL (KIND=4), PARAMETER :: CO2_ERROR_TRAGET=.02
      INTEGER (KIND=2), SAVE :: RETIRE_RETRO_COUNTER
      

      INTEGER (KIND=2) :: GRX_CO2_MARKET_PTS_USED/0/, &
        GRX_CO2_MARKET_PTS_USED_SAVED/0/
      
      REAL (KIND=4) :: LAST_CO2_MARKET_PRICE/0./
      INTEGER (KIND=2) :: GRX_UNIT_COUNT(100,-1:20)
      INTEGER (KIND=2), SAVE :: RETRO_POINTER=0
CONTAINS
      REAL FUNCTION NEWTON_MRKT_PRICE_SEARCH(MIN_POINT,MAX_POINT, &
                                             MIN_CO2,MAX_CO2,CO2_CAP) &
                                                     RESULT(NEW_PRICE)
 REAL (KIND=4), OPTIONAL :: MIN_POINT,MAX_POINT,MIN_CO2,MAX_CO2,CO2_CAP
         REAL (KIND=4), SAVE :: MIN_PRICE/0./,MAX_PRICE/0./, &
                      MIN_CO2_EMISS/0./,MAX_CO2_EMISS/0./,EMISS_CAP/0./
         IF(PRESENT(MAX_POINT)) MAX_PRICE = MAX_POINT
         IF(PRESENT(MIN_POINT)) MIN_PRICE = MIN_POINT
         IF(PRESENT(MIN_CO2)) MIN_CO2_EMISS = MIN_CO2
         IF(PRESENT(MAX_CO2)) MAX_CO2_EMISS = MAX_CO2
         IF(PRESENT(CO2_CAP)) EMISS_CAP = CO2_CAP
         IF(MIN_CO2_EMISS-MAX_CO2_EMISS == 0.) THEN
            NEW_PRICE = (MAX_PRICE + MIN_PRICE)/2.
         ELSE
        NEW_PRICE = (EMISS_CAP-MIN_CO2_EMISS)*(MIN_PRICE-MAX_PRICE)/ &
                         (MIN_CO2_EMISS-MAX_CO2_EMISS)+ MIN_PRICE
         ENDIF
      END FUNCTION NEWTON_MRKT_PRICE_SEARCH
      
      FUNCTION UNITS_BEFORE_ADDITIONS()
      INTEGER (KIND=2) :: UNITS_BEFORE_ADDITIONS
         UNITS_BEFORE_ADDITIONS = GRX_NUNITS 
      END FUNCTION UNITS_BEFORE_ADDITIONS

!--------
        FUNCTION ITERATE_GRX_PLANNING()
        LOGICAL (KIND=4) :: ITERATE_GRX_PLANNING
        CHARACTER (LEN=2) :: GREEN_MRX_METHOD
           ITERATE_GRX_PLANNING = GREEN_MRX_METHOD() == 'GX'
        END FUNCTION
!--------
        FUNCTION MAX_GRX_ITERATIONS(YR)
        USE CO2_CAP_N_TRADE
        INTEGER (KIND=2):: MAX_GRX_ITERATIONS,YR
        integer :: lb, ub
           
           
           MAX_GRX_ITERATIONS = 10
           IF(ALLOCATED(CO2_Max_GRX_Iterations)) THEN
            lb=lbound(CO2_Max_GRX_Iterations,1)
            ub=ubound(CO2_Max_GRX_Iterations,1)
              MAX_GRX_ITERATIONS = CO2_Max_GRX_Iterations(YR)
           ENDIF
           GRX_MAXIMUM_ITERATIONS = MAX_GRX_ITERATIONS
        END FUNCTION
!--------
        SUBROUTINE GRX_CAPACITY_RETIRE_OPTIONS(R_UNIT_NO,GROSS_MARGIN)
         USE CLA_OBJT_ARRAYS
         USE PROD_ARRAYS_DIMENSIONS
         REAL (KIND=4) :: GROSS_MARGIN
         INTEGER (KIND=2) :: R_UNIT_NO
!
         INTEGER (KIND=2) :: I
         INTEGER (KIND=2), SAVE :: &
         TEMP_OPTIONS_RETIRED_UNIT_NO(MAX_CL_UNITS)
         REAL (KIND=4), SAVE :: &
         TEMP_OPTIONS_RETIREMENTS_CO2(max_retirement_units), &
         TEMP_OPTIONS_RETIREMENTS_CO2_COST(max_retirement_units), &
         TEMP_OPTIONS_RETIREMENTS_CAP_COST(max_retirement_units), &
         TEMP_OPTIONS_RETIREMENTS_MW(max_retirement_units)
         
         REAL (KIND=4), SAVE :: &
            TEMP_OPTIONS_RETIREMENTS_MWH(max_retirement_units), &
            TEMP_OPTIONS_RETIRE_GROSS_MARGIN(max_retirement_units)

          IF(GRX_ITERATIONS == 0) THEN
           RETIRED_OPTIONS_PNTR = 1 + RETIRED_OPTIONS_PNTR
         TEMP_OPTIONS_RETIRED_UNIT_NO(R_UNIT_NO) = RETIRED_OPTIONS_PNTR
           TEMP_OPTIONS_RETIREMENTS_CO2(RETIRED_OPTIONS_PNTR) = &
           RETIREMENTS_CO2(R_UNIT_NO)
           TEMP_OPTIONS_RETIREMENTS_CO2_COST(RETIRED_OPTIONS_PNTR) = &
           RETIREMENTS_CO2_COST(R_UNIT_NO)
           TEMP_OPTIONS_RETIREMENTS_CAP_COST(RETIRED_OPTIONS_PNTR) = &
           RETIREMENTS_CAP_COST(R_UNIT_NO)
           TEMP_OPTIONS_RETIREMENTS_MW(RETIRED_OPTIONS_PNTR) = &
           RETIREMENTS_MW(R_UNIT_NO)
           TEMP_OPTIONS_RETIREMENTS_MWH(RETIRED_OPTIONS_PNTR) = &
           RETIREMENTS_MWH(R_UNIT_NO)
           TEMP_OPTIONS_RETIRE_GROSS_MARGIN(RETIRED_OPTIONS_PNTR) = &
           GROSS_MARGIN

          ELSE
            I = TEMP_OPTIONS_RETIRED_UNIT_NO(R_UNIT_NO)
            IF(I > 0) THEN
               RETIREMENTS_CO2(R_UNIT_NO) = &
               TEMP_OPTIONS_RETIREMENTS_CO2(I)
               RETIREMENTS_CO2_COST(R_UNIT_NO) = &
               TEMP_OPTIONS_RETIREMENTS_CO2_COST(I)
               RETIREMENTS_CAP_COST(R_UNIT_NO) = &
               TEMP_OPTIONS_RETIREMENTS_CAP_COST(I)
               RETIREMENTS_MW(R_UNIT_NO) = &
               TEMP_OPTIONS_RETIREMENTS_MW(I)
               RETIREMENTS_MWH(R_UNIT_NO) = &
               TEMP_OPTIONS_RETIREMENTS_MWH(I)
               GROSS_MARGIN = TEMP_OPTIONS_RETIRE_GROSS_MARGIN(I)
            ENDIF
          ENDIF
         END SUBROUTINE
!--------
        SUBROUTINE GRX_SAVE_PLANNIG_VALUES(R_SAVE_VALUES)
        USE CLA_OBJT_ARRAYS
        use globecom
        use cl_data, only: saved_cl_ann_cap
        use tf_objt_interfaces


        LOGICAL (KIND=4) :: SAVE_VALUES,R_SAVE_VALUES
        INTEGER (KIND=2), SAVE :: UPPER_TRANS_GROUP

           SAVE_VALUES = R_SAVE_VALUES
           IF(.NOT. ALLOCATED(SAVED_CL_ANN_CAP)) THEN
               ALLOCATE(SAVED_CL_ANN_CAP(3,get_globecom_study_period(),2))
               SAVE_VALUES = .TRUE.
           ENDIF
           IF(.NOT. ALLOCATED(SAVED_CL_TG_CAP)) THEN
              UPPER_TRANS_GROUP = GET_NUMBER_OF_ACTIVE_GROUPS()
              ALLOCATE(SAVED_CL_TG_CAP(0:6,&
              MAX(1,UPPER_TRANS_GROUP),get_globecom_study_period(),2))
              SAVE_VALUES = .TRUE.
           ENDIF
           IF(SAVE_VALUES) THEN
              SAVED_CL_ANN_CAP(1:3,1:get_globecom_study_period(),1:2) = &
                   CL_ANN_CAP(1:3,1:get_globecom_study_period(),1:2)
              SAVED_CL_TG_CAP &
              (0:6,1:UPPER_TRANS_GROUP,1:get_globecom_study_period(),1:2) = &
              CL_TG_CAP(0:6,1:UPPER_TRANS_GROUP,1:get_globecom_study_period(),1:2)
           ELSE
              CL_ANN_CAP(1:3,1:get_globecom_study_period(),1:2) = &
                            SAVED_CL_ANN_CAP(1:3,1:get_globecom_study_period(),1:2)
              CL_TG_CAP(0:6,1:UPPER_TRANS_GROUP,1:get_globecom_study_period(),1:2) = &
            SAVED_CL_TG_CAP(0:6,1:UPPER_TRANS_GROUP,1:get_globecom_study_period(),1:2)
           ENDIF
        END SUBROUTINE
!-------
        SUBROUTINE GRX_SAVE_CAPACITY_OPTIONS(R_SAVE_VALUES)
        USE CAPACITY_OPTIONS_ALLOC_VARS
        USE CAPACITY_OPTIONS_FIXED_VARS
        LOGICAL (KIND=4) :: R_SAVE_VALUES
           IF(.NOT. ALLOCATED(GRX_SAVED_CUMULATIVE_UNITS)) RETURN
           IF(R_SAVE_VALUES) THEN
              GRX_SAVED_CUMULATIVE_UNITS(1:FOR_ALL_OPTIONS) =  &
                                  CUMULATIVE_UNITS(1:FOR_ALL_OPTIONS)
           ELSE
              CUMULATIVE_UNITS(1:FOR_ALL_OPTIONS) =  &
                         GRX_SAVED_CUMULATIVE_UNITS(1:FOR_ALL_OPTIONS)
 ANNUAL_UNITS_LEFT(1:FOR_ALL_OPTIONS) = ANNUAL_UNITS(1:FOR_ALL_OPTIONS)
           ENDIF
        END SUBROUTINE
!-------
        SUBROUTINE GRX_RETROFIT_OPTONS(R_SAVE_VALUES)
        USE PROD_ARRAYS_DIMENSIONS
        USE CLA_OBJT_ARRAYS
        LOGICAL (KIND=4) :: SAVE_VALUES,R_SAVE_VALUES
           SAVE_VALUES = R_SAVE_VALUES
           IF(.NOT. ALLOCATED(GRX_BOP_RETROFIT_ACTIVE)) THEN
              ALLOCATE(GRX_BOP_RETROFIT_ACTIVE(MAX_CL_UNITS))
              SAVE_VALUES = .TRUE.
           ENDIF
           IF(SAVE_VALUES) THEN
              GRX_BOP_RETROFIT_ACTIVE(1:MAX_CL_UNITS) = &
                                    RETROFIT_ACTIVE(1:MAX_CL_UNITS)
           ELSE
              RETROFIT_ACTIVE(1:MAX_CL_UNITS) = &
                       GRX_BOP_RETROFIT_ACTIVE(1:MAX_CL_UNITS) 
           ENDIF
        END SUBROUTINE
!-------
        SUBROUTINE GRX_CO2_REDUCTION_CURVE(TG)
            use globecom
            use tf_objt_interfaces
        

        
   INTEGER (KIND=2) :: TG,I,GRX_MAX_ITERS
        IF(GRX_ITERATIONS == 0) THEN
           TG_MAX = GET_NUMBER_OF_ACTIVE_GROUPS()
           GRX_MAX_ITERS = GRX_MAXIMUM_ITERATIONS
      IF(ALLOCATED(GRX_ITER_CO2_PRICE)) DEALLOCATE (GRX_ITER_CO2_PRICE)
           ALLOCATE(GRX_ITER_CO2_PRICE(0:GRX_MAX_ITERS,0:TG_MAX))
           GRX_ITER_CO2_PRICE = 0.
IF(ALLOCATED(GRX_ITER_TG_CO2_EMISS)) DEALLOCATE (GRX_ITER_TG_CO2_EMISS)
           ALLOCATE(GRX_ITER_TG_CO2_EMISS(0:GRX_MAX_ITERS,0:TG_MAX))
           GRX_ITER_TG_CO2_EMISS = 0.
           IF(ALLOCATED(GRX_CO2_EMISS_REDUCT_REQUIRED)) &
                          DEALLOCATE(GRX_CO2_EMISS_REDUCT_REQUIRED)
           ALLOCATE(GRX_CO2_EMISS_REDUCT_REQUIRED(0:TG_MAX))
           GRX_CO2_EMISS_REDUCT_REQUIRED = 0.
           IF(globecom_YEAR == 1) then
			CURRENT_CO2_DISPATCH_COST = 0.
		   end if
           NEXT_CURRENT_CO2_DISPATCH_COST = 0.
           RETURN
        ELSE
           TG_MAX = GET_NUMBER_OF_ACTIVE_GROUPS()
           GRX_MAX_ITERS = GRX_MAXIMUM_ITERATIONS
           IF(.NOT. ALLOCATED(GRX_ITER_CO2_PRICE)) THEN
              ALLOCATE(GRX_ITER_CO2_PRICE(0:GRX_MAX_ITERS,0:TG_MAX))
              GRX_ITER_CO2_PRICE = 0.
           ENDIF
           IF(.NOT. ALLOCATED(GRX_ITER_TG_CO2_EMISS)) THEN
              ALLOCATE(GRX_ITER_TG_CO2_EMISS(0:GRX_MAX_ITERS,0:TG_MAX))
              GRX_ITER_TG_CO2_EMISS = 0.
           ENDIF
           IF(.NOT. ALLOCATED(GRX_CO2_EMISS_REDUCT_REQUIRED)) THEN
              ALLOCATE(GRX_CO2_EMISS_REDUCT_REQUIRED(0:TG_MAX))
              GRX_CO2_EMISS_REDUCT_REQUIRED = 0.
           ENDIF
        ENDIF
    GRX_ITER_CO2_PRICE(GRX_ITERATIONS,TG) = CURRENT_CO2_DISPATCH_COST
  GRX_ITER_TG_CO2_EMISS(GRX_ITERATIONS,TG) = GET_CO2_EMISS_FOR_SYSTEM()

           WRITE(9,*) "CO2 REDUCTION CURVE"
           DO I = 1, GRX_ITERATIONS
              WRITE(9,*) I,GRX_ITER_CO2_PRICE(I,TG), &
              GRX_ITER_TG_CO2_EMISS(I,TG)
           ENDDO

        END SUBROUTINE
!-------
        FUNCTION GRX_CO2_PRICE_ADJ(TG)
        USE co2_market_abatement_curves
        REAL (KIND=4) :: GRX_CO2_PRICE_ADJ
        REAL (KIND=4) :: GET_EMISS_CAP_FOR_CLASS,X,SLOPE,INTERCEPT
        INTEGER (KIND=2) :: TG,PT1,PT2
        LOGICAL (KIND=1) :: CO2_EMISSIONS_GT_CAP
         IF(IS_CO2_PRICE_MRKT_CAPPED) THEN
            GRX_CO2_PRICE_ADJ = LAST_CO2_MARKET_PRICE
            WRITE(9,*) &
            "MRKT PRICE CONTROLS SLOPE BASED PRICE",GRX_CO2_PRICE_ADJ
            IS_CO2_PRICE_MRKT_CAPPED = .FALSE.
            LAST_CO2_MARKET_PRICE = 0.
         ELSE
            PT2 = GRX_ITERATIONS
            PT1 = GRX_ITERATIONS - 1
       SLOPE = (GRX_ITER_CO2_PRICE(PT2,TG)-GRX_ITER_CO2_PRICE(PT1,TG))
            X = GET_EMISS_CAP_FOR_CLASS(3_2,TG)
            IF(GRX_CO2_MARKET_PTS_USED > 0) THEN
       X = X + 10**6 * SUM(CO2_ABATE_VOLUMS(1:GRX_CO2_MARKET_PTS_USED))
            ENDIF
            CO2_EMISSIONS_GT_CAP = GRX_ITER_TG_CO2_EMISS(PT2,TG) > X
           IF(SLOPE == 0. .AND. GRX_ITER_CO2_PRICE(PT2,TG) == 0.) THEN
               GRX_CO2_PRICE_ADJ = 20.
            ELSEIF(SLOPE == 0.) THEN
               IF(CO2_EMISSIONS_GT_CAP) THEN
                  GRX_CO2_PRICE_ADJ = 1.2* GRX_ITER_CO2_PRICE(PT2,TG)
               ELSE
                  GRX_CO2_PRICE_ADJ = 0.8 * GRX_ITER_CO2_PRICE(PT2,TG)
               ENDIF
            ELSE
               SLOPE = SLOPE/ &
        (GRX_ITER_TG_CO2_EMISS(PT2,TG) - GRX_ITER_TG_CO2_EMISS(PT1,TG))
               INTERCEPT = GRX_ITER_CO2_PRICE(PT2,TG) - SLOPE * &
               GRX_ITER_TG_CO2_EMISS(PT2,TG)
               GRX_CO2_PRICE_ADJ = SLOPE * X + INTERCEPT
            ENDIF
            WRITE(9,*) "SLOPE BASED PRICE",GRX_CO2_PRICE_ADJ
            WRITE(9,*) "SLOPE",SLOPE
            WRITE(9,*) "CAP USED",X
            WRITE(9,*) "CO2 PRICE 1",GRX_ITER_CO2_PRICE(PT2,TG)
            WRITE(9,*) "CO2 PRICE 2",GRX_ITER_CO2_PRICE(PT1,TG)
         ENDIF
         BALANCE_WITH_CO2_PRICE = .TRUE.
         call flush_unit(int(9,2))
        END FUNCTION
!-------
        FUNCTION GRX_CO2_LEAST_SQUARED_PRICE_ADJ(TG)
        REAL (KIND=4) :: GRX_CO2_LEAST_SQUARED_PRICE_ADJ
        REAL (KIND=4) :: GET_EMISS_CAP_FOR_CLASS,X,SLOPE,&
                         INTERCEPT,SUMX,SUMY,SUM_PROD_XY,SUM_X2
        INTEGER (KIND=2) :: TG,I,ISTART
        SUMX = 0
        SUMY = 0
        SUM_PROD_XY = 0.
        SUM_X2 = 0.
        ISTART = MAX(1,GRX_ITERATIONS-3)
        DO I = 1, GRX_ITERATIONS
            SUMX = SUMX + GRX_ITER_TG_CO2_EMISS(I,TG)
            SUM_X2 = SUM_X2 + GRX_ITER_TG_CO2_EMISS(I,TG)**2
            SUMY = SUMY + GRX_ITER_CO2_PRICE(I,TG)
            SUM_PROD_XY = SUM_PROD_XY + &
            GRX_ITER_TG_CO2_EMISS(I,TG) * GRX_ITER_CO2_PRICE(I,TG)
        ENDDO
        SLOPE = (FLOAT(GRX_ITERATIONS)* SUM_PROD_XY - SUMX * SUMY)/ &
                     (FLOAT(GRX_ITERATIONS)* SUM_X2 - SUMX**2)
        INTERCEPT = (SUMY - SLOPE * SUMX)/FLOAT(GRX_ITERATIONS)
        X = (1.-CO2_ERROR_TRAGET) * GET_EMISS_CAP_FOR_CLASS(3_2,TG)
        GRX_CO2_LEAST_SQUARED_PRICE_ADJ = SLOPE * X + INTERCEPT
        BALANCE_WITH_CO2_PRICE = .TRUE.
         WRITE(9,*) "LEAST SQR BASED PRICE",&
         GRX_CO2_LEAST_SQUARED_PRICE_ADJ
         WRITE(9,*) "SLOPE",SLOPE
         WRITE(9,*) "INTERCEPT",INTERCEPT
         call flush_unit(int(9,2))
        END FUNCTION
!--------
        SUBROUTINE GRX_CAPACITY_TEMP_RETRO_UNIT(R_UNIT_NO)
         USE PROD_ARRAYS_DIMENSIONS
         USE CLA_OBJT_ARRAYS
         REAL (KIND=4), SAVE :: TEMP_RETRO_CO2(max_retirement_units), &
                                TEMP_RETRO_MW(max_retirement_units)
         REAL (KIND=4), SAVE :: TEMP_RETRO_MWH(max_retirement_units), &
                            TEMP_RETRO_CO2_PRICE(max_retirement_units)
         INTEGER (KIND=2) :: R_YEAR,R_UNIT_NO, &
         CL_CAPACITY_TEMP_RETRO_UNIT
         INTEGER (KIND=2), SAVE :: TEMP_RETRO_UNIT_PTR(MAX_CL_UNITS)
         CHARACTER (LEN=1) :: TEMP_RETRO_SWITCH(max_retirement_units)
         INTEGER (KIND=2) :: RETRO_PTR
!
          IF(GRX_ITERATIONS == 0) THEN
            RETRO_POINTER = 1 + RETRO_POINTER
            TEMP_RETRO_UNIT_PTR(R_UNIT_NO) = RETRO_POINTER

            TEMP_RETRO_CO2(RETRO_POINTER) = RETIREMENTS_CO2(R_UNIT_NO)
            TEMP_RETRO_MW(RETRO_POINTER) = RETIREMENTS_MW(R_UNIT_NO)
            TEMP_RETRO_MWH(RETRO_POINTER) = RETIREMENTS_MWH(R_UNIT_NO)
            TEMP_RETRO_CO2_PRICE(RETRO_POINTER) = &
            RETIREMENTS_CO2_PRICE(R_UNIT_NO)
            write(9,*) "GRX Retros Saved"
         ELSE
            RETRO_PTR = TEMP_RETRO_UNIT_PTR(R_UNIT_NO)
            IF(RETRO_PTR > 0) THEN

               RETIREMENTS_CO2(R_UNIT_NO) = TEMP_RETRO_CO2(RETRO_PTR)
               RETIREMENTS_MW(R_UNIT_NO) = TEMP_RETRO_MW(RETRO_PTR)
               RETIREMENTS_MWH(R_UNIT_NO) = TEMP_RETRO_MWH(RETRO_PTR)
               RETIREMENTS_CO2_PRICE(R_UNIT_NO) = &
               TEMP_RETRO_CO2_PRICE(RETRO_PTR)
            ENDIF
            write(9,*) "GRX Retros Restored"
         ENDIF
         write(9,*) RETRO_PTR, &
                    R_UNIT_NO, &
                    RETIREMENT_CANDIDATE(R_UNIT_NO), &
                    RETIREMENTS_CO2(R_UNIT_NO), &
                    RETIREMENTS_MW(R_UNIT_NO), &
                    RETIREMENTS_MWH(R_UNIT_NO), &
                    RETIREMENTS_CO2_PRICE(R_UNIT_NO), &
                    FIRST_RETIREMENT_YEAR(R_UNIT_NO)

         CL_CAPACITY_TEMP_RETRO_UNIT = RETRO_PTR
         END SUBROUTINE
!-------
        SUBROUTINE GRX_ITER_RESULTS(TG,&
        CONVER_CREDITS_USED,CONVER_CREDITS_COST)
        USE co2_market_abatement_curves
        use globecom
        use mod_base_year
        
        REAL (KIND=4), OPTIONAL :: &
        CONVER_CREDITS_USED,CONVER_CREDITS_COST

         INTEGER (KIND=4) :: NEXT_REC=0
         CHARACTER (LEN=40) :: TITLE
         CHARACTER (LEN=28) :: COMPANY_NAME
         ! Lahey won't support the following Fortran-standard construct:
         ! integer :: F7=int(Z'00F7', kind(F7))
         ! Used calc.exe (programmer calculator mode) to get decimal
         ! value 247.
         INTEGER :: F7=247
         LOGICAL (KIND=4) FILE_EXISTS
         INTEGER (KIND=2) :: OVERHEAD_LENGTH,DIMENSIONS, &
                             VARIABLE_NUMBER,RECORD_LENGTH, &
                             GET_NUM_OF_END_POINTS, &
                             TG
         CHARACTER (LEN=256) :: FILE_NAME,GET_RESULTS_DIRECTORY
         CHARACTER (LEN=5) :: GET_SCENAME
         CHARACTER (LEN=35) :: GET_GROUP_NAME
         REAL (KIND=4) :: CAP_ERROR,GET_EMISS_CAP_FOR_CLASS, &
                          CO2_CREDITS_FROM_MARKET, &
                          CO2_CREDIT_EXPENDITURES, &
                          CREDITS_PURCHASED_AT_LAST_PRICE
         REAL (KIND=4) :: CO2_EMISS_CAP,CO2_EMISSIONS,UNCOSTED_CREDITS
         INTEGER (KIND=2) :: I
         IF(NEXT_REC == 0) THEN
            FILE_NAME = TRIM(GET_RESULTS_DIRECTORY())//"MSG"// &
                           TRIM(GET_SCENAME())//".XGD"
            OVERHEAD_LENGTH = 64
            VARIABLE_NUMBER = 20 ! 08/24/05. ADDED FORECAST VARIABLE
            DIMENSIONS = 4
            RECORD_LENGTH = MAX(64,OVERHEAD_LENGTH + 4*VARIABLE_NUMBER)
            OPEN(1337,FILE=FILE_NAME,ACCESS="DIRECT", &
                                   STATUS="REPLACE",RECL=RECORD_LENGTH)
!
            WRITE(1337,REC=1) F7,RECORD_LENGTH, &
                                      DIMENSIONS, &
                                      get_BASE_YEAR(), &
                                      gc_endyr+get_EXTENSION_PERIOD(), &
                                      GET_NUM_OF_END_POINTS()
            WRITE(1337,REC=2) COMPANY_NAME()
            WRITE(1337,REC=3) TITLE()
           WRITE(1337,REC=4) 'Endpoint            ','N',INT2(4),'F','D'
           WRITE(1337,REC=5) 'Year                ','N',INT2(4),'F','D'
          WRITE(1337,REC=6) 'Transaction Group   ','C',INT2(35),'V','D'
           WRITE(1337,REC=7) 'Iteration           ','N',INT2(4),'F','D'
            NEXT_REC = 7
         ENDIF
         NEXT_REC = NEXT_REC + 1
         IF(GRX_CO2_MARKET_PTS_USED == 0) THEN
            CAP_ERROR = -100.*(GET_EMISS_CAP_FOR_CLASS(3_2,TG) - &
            GET_CO2_EMISS_FOR_SYSTEM()) &
                                   /GET_EMISS_CAP_FOR_CLASS(3_2,TG)
      WRITE(1337,REC=NEXT_REC) PRT_ENDPOINT(), &
		FLOAT(get_BASE_YEAR()+globecom_YEAR), &
                                  GET_GROUP_NAME(TG), &
                                  FLOAT(GRX_ITERATIONS), &
                                  CURRENT_CO2_DISPATCH_COST, & 
                                GET_CO2_EMISS_FOR_SYSTEM()/1000000. , &
                                  CAP_ERROR, &
                           GET_EMISS_CAP_FOR_CLASS(3_2,TG)/1000000., &
                           0., & 
                                  0., & !CO2_CAP_EXPENDITURES, &
                                  0., &
                                  0., &
                                  0., &
                               GET_CO2_EMISS_FOR_SYSTEM()/1000000., &
                                  0., &
                                  0.
         ELSE
            CO2_EMISS_CAP = GET_EMISS_CAP_FOR_CLASS(3_2,TG)/1000000.
            CO2_EMISSIONS = GET_CO2_EMISS_FOR_SYSTEM()/1000000.
            IF(PRESENT(CONVER_CREDITS_USED)) THEN
               CO2_CREDITS_FROM_MARKET = CONVER_CREDITS_USED/1000000.
               IF(GRX_CO2_MARKET_PTS_USED > 1) THEN
            CREDITS_PURCHASED_AT_LAST_PRICE= CO2_CREDITS_FROM_MARKET &
                   - SUM(CO2_ABATE_VOLUMS(1:GRX_CO2_MARKET_PTS_USED-1))
               ELSE
              CREDITS_PURCHASED_AT_LAST_PRICE = CO2_CREDITS_FROM_MARKET
               ENDIF
            ELSE
               CO2_CREDITS_FROM_MARKET = &
               SUM(CO2_ABATE_VOLUMS(1:GRX_CO2_MARKET_PTS_USED))
               CREDITS_PURCHASED_AT_LAST_PRICE= &
               CO2_ABATE_VOLUMS(GRX_CO2_MARKET_PTS_USED)
            ENDIF
            IF(PRESENT(CONVER_CREDITS_COST)) THEN
               CO2_CAP_EXPENDITURES = CONVER_CREDITS_COST
            ELSE
               CO2_CAP_EXPENDITURES = 0.
               UNCOSTED_CREDITS = CO2_CREDITS_FROM_MARKET
               DO I = 1, GRX_CO2_MARKET_PTS_USED
                  CO2_CAP_EXPENDITURES = CO2_CAP_EXPENDITURES + &
                          CO2_ABATE_PRICES(I) * &
                          MIN(CO2_ABATE_VOLUMS(I),UNCOSTED_CREDITS)
                  UNCOSTED_CREDITS = UNCOSTED_CREDITS - &
                  CO2_ABATE_VOLUMS(I)
               ENDDO
            ENDIF
            CAP_ERROR = -100.*(CO2_EMISS_CAP - &
            (CO2_EMISSIONS - CO2_CREDITS_FROM_MARKET))/ &
                                         CO2_EMISS_CAP
            WRITE(1337,REC=NEXT_REC) PRT_ENDPOINT(),&
            FLOAT(get_BASE_YEAR()+globecom_YEAR), &
                                  GET_GROUP_NAME(TG), &
                                  FLOAT(GRX_ITERATIONS), &
                                  CURRENT_CO2_DISPATCH_COST, & 
                                  CO2_EMISSIONS, &
                                  CAP_ERROR, &
                                  CO2_EMISS_CAP, &
                                  CO2_CREDITS_FROM_MARKET, &
                                  CO2_CAP_EXPENDITURES, &
                                  FLOAT(GRX_CO2_MARKET_PTS_USED), &
                           CO2_ABATE_PRICES(GRX_CO2_MARKET_PTS_USED), &
                           CO2_ABATE_VOLUMS(GRX_CO2_MARKET_PTS_USED), &
                           (CO2_EMISSIONS - CO2_CREDITS_FROM_MARKET), &
                   SUM(CO2_ABATE_VOLUMS(1:GRX_CO2_MARKET_PTS_USED)), &
                                  CREDITS_PURCHASED_AT_LAST_PRICE

         ENDIF
         call flush_unit(int(1337,2))
      END SUBROUTINE


      END MODULE GRX_PLANNING_ROUTINES
