      MODULE CL_UNITS_READ_DATA
         USE PROD_ARRAYS_DIMENSIONS
         use mthnmcom
         REAL (KIND=4) :: LOCAL_CL_POOL_FRAC_OWN(MAX_CL_UNITS)
         REAL (KIND=8) :: ThermalDemandPreviousIter(MAX_CL_UNITS)

      REAL (KIND=4) :: MN_RATE,TEMP_REAL
      
      REAL (KIND=4), ALLOCATABLE :: RETIRE_RETRO_UPLIFT_PER_TON(:), &
             RETIRE_RETRO_CUM_CO2(:), &
             NEW_UNIT_RETIRE_VALUE_BY_CM(:), &
             NUC_FUEL_LEASED_BURN_BY_CLASS(:), &
             NUC_FUEL_OWNED_BURN_BY_CLASS(:), &
             NUCLEAR_MWH_BY_CLASS(:), &
             NUCLEAR_MMBTU_BY_CLASS(:), &
             INTRA_COMPANY_NF_BURN(:), &
             ANNUAL_CL_MAINT_MW(:), &
             Mtnc_DAYS(:), &
             MAINTENANCE_DAYS(:), &
             MAINT_INDEX_MW(:), &
             MAINT_INDEX_DAYS(:), &
             ASSET_CLASS_LIST(:), &
             ASSET_ALLOCATION_LIST(:), &
             ANNUAL_CL_MAINTENANCE(:,:), &
            MON_MDS_CL_UNIT_CAPACITY(:,:), &
            MON_MDS_CL_UNIT_ENERGY(:,:), &
            MON_MDS_CL_UNIT_FUEL_COST(:,:), &
            MON_MDS_CL_UNIT_VAR_COST(:,:), &
            MON_MDS_CL_UNIT_FIXED_COST(:,:), &
            MON_MDS_NUC_FUEL_ADDER_COST(:,:), &
            MON_MDS_ECO_SALES_REV_FROM(:,:), &
            MON_MDS_ECO_SALES_ENRG_FROM(:,:), &
            MON_MDS_ECO_PUCH_COST_FROM(:,:), &
            MON_MDS_ECO_PUCH_ENRG_FROM(:,:), &
            MON_MDS_ICAP_REVENUES(:,:), &
            CL_ANN_CLASS_FUEL_COST(:,:), &
           CL_ANN_MULT_FUEL_COST(:,:), &
           FE_ANN_MULT_FUEL_COST(:,:), &
           CL_ANN_MULT_VAR_COST(:,:), &
           CL_ANN_MULT_FIXED_COST(:,:), &
           CL_ANN_MULT_REVENUE(:,:), &
           CL_ANN_MULT_CAPACITY(:,:), &
           CL_ANN_MULT_PURCHASES(:,:), &
           CL_ANN_MULT_ENERGY(:,:), &
           NF_FUEL_LEASED_BY_CLASS(:,:), &
           NF_FUEL_OWNED_BY_CLASS(:,:), &
           CL_ANN_CLASS_VAR_COST(:,:), &
           CL_ANN_CLASS_FIXED_COST(:,:), &
           CL_ANN_CLASS_REVENUE(:,:), &
           CL_ANN_CLASS_CAPACITY(:,:), &
           CL_ANN_CLASS_PURCHASES(:,:), &
           CL_ANN_CLASS_ENERGY(:,:), &
           CL_ANN_CLASS_EMISSIONS(:,:), &
           INTRA_COMPANY_REVENUES(:,:), &
           MON_MDS_NUC_FUEL_LEASE_BURN_BC(:,:), &
           MON_MDS_NUC_FUEL_OWNED_BURN_BC(:,:), &
           MON_MDS_NUCLEAR_MWH_BY_CLASS(:,:), &
           MON_MDS_NUCLEAR_MMBTU_BY_CLASS(:,:), &
           MONTHLY_AC_WHOLESALE_PROD_COST(:,:), &
           MONTHLY_AC_ECITY_VAR_PROD_COST(:,:), &
           MONTHLY_AC_ECITY_NEW_FIX_COST(:,:), &
           MAINT_INDEX_MONTHLY_MW(:,:), &
           ANNUAL_CL_CAPACITY(:,:,:), &
           MON_MDS_CL_UNIT_EMISSIONS_COST(:,:,:), &
           MON_MDS_CL_UNIT_EMISSIONS(:,:,:), &
           MON_MDS_CL_MULT_FUEL_COST(:,:,:), &
           MON_MDS_CL_MULT_VAR_COST(:,:,:), &
           MON_MDS_CL_MULT_FIXED_COST(:,:,:), &
           MON_MDS_CL_MULT_CAPACITY(:,:,:), &
           MON_MDS_CL_MULT_ENERGY(:,:,:), &
           MON_MDS_CL_MULT_PURCHASES(:,:,:), &
           MON_MDS_CL_CLASS_VAR_COST(:,:,:), &
           MON_MDS_CL_CLASS_FIXED_COST(:,:,:), &
           MON_MDS_CL_CLASS_CAPACITY(:,:,:), &
           MON_MDS_CL_CLASS_ENERGY(:,:,:), &
           MON_MDS_CL_CLASS_EMISSIONS(:,:,:), &
           MON_MDS_CL_CLASS_EMISSIONS_COST(:,:,:), &
           MON_MDS_INTRA_COMPANY_REVENUES(:,:,:), &
           MON_MDS_INTRA_COMPANY_NF_BURN(:,:)

! FIX TO MAINTENANCE SCHEDULER. 3/8/1.

      REAL (KIND=8),ALLOCATABLE :: MON_MDS_CL_UNIT_MMBTUS(:,:)





      INTEGER (KIND=2), ALLOCATABLE :: TG_2_PLANNING_AREA(:), &
                RETIRE_RETRO_INDEX(:), &
                TRANS_GROUP_FOR_DATA_BASE(:), &
                RETIRE_RETRO_POSITION(:), &
                RETIRE_RETRO_ABATE_INDEX(:), &
                PURCHASE_ASSET_CLASS_ID(:), &
                MARKET_RESOURCE_INDEX(:), &
                MARKET_RESOURCE_COUNTER(:), &
                DAY_TYPE_TRANS_GROUP_PAIR(:,:), &
                DATA_BASE_POSITION(:,:), &
                MRX_TG_CAP_CURVE_INDEX(:,:), &
                MRX_TG_CAP_UNIT_NO(:), &
                MRX_TG_CAP_UNIT_INDEX(:)

      LOGICAL (KIND=1) :: RETROFIT_UNIT_IS_ACTIVE(MAX_CL_UNITS), &
                          TEMP_L1

      INTEGER (KIND=2) :: CAP_LIMITED_CLASS_POINTER(1024), &
                          TOP,BOTTOM,HALF, &
                          CM_INDEX,GET_CM_INDEX_FROM_TG, &
                          UnitTG,UnitCG
      CHARACTER (LEN=1), ALLOCATABLE :: PURCHASE_POWER_ASSIGN(:)
      CHARACTER (LEN=1)INTRA_CO_transaction_loc(MAX_CL_UNITS)

      LOGICAL (KIND=1), ALLOCATABLE :: MAINT_DIVISIBLE(:)
      CHARACTER (len=20), save :: SPL_unit_id_loc(MAX_CL_UNITS)
      INTEGER (KIND=4) :: DA_ERR,CPO_REC,RESET_REC
      LOGICAL (KIND=4) :: temp_grx_converged,GRX_CO2_MARKET_LOGIC
      REAL (KIND=4) :: ITER_0_ECON_RETIRE_MARGIN(MAX_CL_UNITS), &
                       LAST_ITER_GROSS_MARGIN(MAX_CL_UNITS)
      REAL (KIND=4) :: UNIT_ANNUAL_GROSS_MARGIN,RETIREMENT_MEASURE, &
                       GET_MRX_TG_CAP_CURVE_PRICE, &
                       GET_MRX_CM_CAP_CURVE_PRICE
      CHARACTER (LEN=48) :: RPT_UNIT_NAME
      REAL (KIND=4) :: RPT_RETIREMENTS_MW,RPT_RETIREMENTS_MWH, &
                       RPT_RETIREMENTS_CO2,GRX_CO2_MARKET_TONS, &
                       GRX_CO2_MARKET_PRICE

      


      

      
      INTEGER*2 MAX_INTRA_CLASS, &
             CM_RETIREMENT_UNIT(0:1000)
      REAL INTRA_FOSSIL_FUEL_EXPENSES, &
           INTRA_LEASED_NUC_FUEL_EXPENSES, &
           INTRA_OWNED_NUC_FUEL_EXPENSES, &
           INTRA_NUC_OWN_BURN, &
           INTRA_NUC_LEASE_BURN, &
           INTRA_PURCHASE_EXPENSES









      INTEGER (kind=2) :: MARKETSYM_UNIT, &
                COUNTER, &
                I2_RETURN_RESULT










      INTEGER*2   LOCAL_MONTH, &
                  MAX_RESOURCE_ID_NO, &
                  FT, & !  PRIMARY MOVER
                  TT,  & !  TRANSFER TYPE
                  NG, & !  NEWGEN INDEX
                  YEAR_OR_SCEN, &
                  MAX_PLANNING_AREAS, &
                  MAX_MAINT_PLANNING_AREAS, &
                  MAX_NEWGEN_INDEX
      PARAMETER(  MAX_RESOURCE_ID_NO=9999, &
                  MAX_NEWGEN_INDEX=100)
      
      INTEGER*4   VALUES_2_ZERO, &
                  INT4_EMIS_TYPE



      INTEGER*2   CL_RECORDS,EMISS_TYPE, &
                  RDI_RECORDS,RDI_CL_RECORDS,RDI_REC,RDI_CL_REC, &
                  RDI_IN_UNIT,RDI_OUT_UNIT, &
                  NEWGEN_COUNTER(MAX_NEWGEN_INDEX), & !  082309.
                  START_UP_INDEX(MAX_CL_UNITS), &
                  MONTHLY_MUST_RUN_vctr(MAX_CL_UNITS), &
                  MONTHLY_dcmt_VECOT(MAX_CL_UNITS), &
                  State_TG_Index(MAX_CL_UNITS), &
                  thermal_State_Index(MAX_CL_UNITS), &
                  EMISSION_MARKET_LINK(MAX_CL_UNITS), &
                  TOTAL_START_UP_UNITS/0/, &
                  LOCAL_RESOURCE_ID, &
                  RESOURCE_ID_TO_UNIT(0:MAX_RESOURCE_ID_NO), &
                  NUMBER_OF_RESOURCE_ID(0:MAX_RESOURCE_ID_NO), &
                  MAX_FO_PER_MONTH/1/, &
                  NOX_SsN_DATE(MAX_CL_UNITS), &
                  POINTER_FOR_NEW_CL_UNIT(MAX_CL_UNITS), &
                  RETIREMENTS_INDEX(MAX_CL_UNITS), &
                  RETIRE_RETRO_COUNT_AFTER_SORT, &
                  LAST_COUNTER, &
                  RETIRE_RETRO_USED(MAX_CL_UNITS), &
                  CO2_ABATEMENT_INDEX(MAX_CL_UNITS), &
                  CO2_RETROFIT_ABATEMENT_INDEX(MAX_CL_UNITS), &
                  CO2_COUNTER, &
                  ANN_ECON_RETIRE_COUNTER, &
                  NEXT_MRX_RETIREMENT, &
                  NEXT_MRX_RETROFIT, &
                  U, &
                  MONTH



      INTEGER(kind=2), save :: DELETE,ON_LINE_MONTH,ON_LINE_YEAR, &
                M,IREC, &
                R_LAST_NUNITS, &
                BASE_CL_UNITS/0/, &
                BASE_PLUS_HARDWIRED_CL_UNITS/0/, &
                AVAILABLE_CL_UNITS/0/, &
                CO2_ABATEMENT_NO, &
                CO2_ABATEMENT_VAR_NUM



      REAL R_POINTR,MONTH_MULT,VOID_REAL, &
           RDI_MONTH_GENERATION(12),RDI_MONTH_CAPACITY(12), &
           RANDOM_NUMBER
      LOGICAL*4 FILE_EXISTS

      INTEGER*2 TEMP_RPS_NO, &
                TEMP_PM
      REAL*4    TEMP_ENRG, &
                TEMP_CAP
      LOGICAL*4 NEW_CL_UNIT_OPENED,RDI_FILE_EXISTS, &
       temp_l4
       



!     VARIABLES FOR CAPACITY PLANNING
           real ::  &
           Min_Cap_Change, &
           Max_Cap_Change, &
           Ave_Heat_Rate_Mult, &
           CO2_Cntl_Percent

      CHARACTER*2 CL_LOAD_TYPE
      CHARACTER*1 unit_is_active, &
                  UTILITY_OWNED_loc, &
                  SAVE_IGNORE_NON_UTILITY/'F'/, &
                  STARTup_LOGIC(MAX_CL_UNITS), &
                  CONTRIBUTES_2_SPIN(MAX_CL_UNITS), &
                  APPLY_Nx_SEASON_DATE(MAX_CL_UNITS), &
                  MkT_RESOURCE(MAX_CL_UNITS), &
                  FOR_SEED_OPTIONS, &
                  USE_PLY_HEAT_RATES, &
                  wvpa_rt_tckr(MAX_CL_UNITS), &
                  wvpa_res_trkr(MAX_CL_UNITS), &
                  WVPA_fl_TCKR_loc(MAX_CL_UNITS), &
                  monte_or_frq(MAX_CL_UNITS), &
                  WVPA_MM_TRACKER(MAX_CL_UNITS), &
                  ALLOW_DcmT(MAX_CL_UNITS), &
                  prim_fuelcat(MAX_CL_UNITS), &
                  scndary_fuel_cat(MAX_CL_UNITS), &
                  EMSSNS_FUEL_CATEGORY(MAX_CL_UNITS), &
                  THrm_AGGREGATED_UNIT


      CHARACTER*6    Bc_PLANT_ID(MAX_CL_UNITS), &
                     bcuid, &
                     Bc_MARKET_AREA_ID(MAX_CL_UNITS), &
                     BC_TRANS_AREA_ID, &
                     BASECASE_PLANT_NERC_SUBID, &
                     BC_PLANT_OWNER_ID, &
                     PRIM_MOVER_STR(MAX_CL_UNITS)

      INTEGER*2 &
      ! Now using ns_cla_decs->capacity_market_pointer_acl - johncheck
                     CAP_MARKET_MONTH_NO(MAX_CL_UNITS), &
                     LOCAL_COLLECTION, &
                     RETROFIT_PROJ_ID(MAX_CL_UNITS), &
                     RETROFIT_UNIT_INDEX(MAX_CL_UNITS), &
                     RETROFIT_COUNTER,RETIREMENT_COUNTER, &
                     ANN_RETROFIT_COUNTER

      CHARACTER*5  MARKET_ID, &
                     CAP_MARKET_TYPE(MAX_CL_UNITS), &
                     CAPACITY_MARKET_MTH(MAX_CL_UNITS), &
                     CAP_MARKET_EXP_COLLECT(MAX_CL_UNITS)
      CHARACTER*20   CAP_MARKET_COST_ASSIGN(MAX_CL_UNITS),&
                     STATE_PROVince_NAME
      CHARACTER*36 GUID
      REAL*4 &
               MON_CAP_SALE_MW_FROM(MAX_CL_UNITS), &
               MON_CAP_SALE_REV_FROM(MAX_CL_UNITS), &
               MON_CAP_PUCH_MW_FROM(MAX_CL_UNITS), &
               MON_CAP_PUCH_COST_FROM(MAX_CL_UNITS), &
               RPS_CONTRIBUTION_PERCENT(MAX_CL_UNITS), &
               CO2_PPLN_DISTANCE(MAX_CL_UNITS), &
               CO2_RTRO_HEAT_MULT(MAX_CL_UNITS), &

               CO2_RTRO_CAP_MULT(MAX_CL_UNITS)


      CHARACTER*20 cnty,NG_UNIT_STATUS
      CHARACTER*25 CL_TRANS_NAME



      INTEGER*2 ST
      
      CHARACTER*6 st_pvnc, &
                  TEMP_STATE, &
                  STATE_PROVINCE_NAMES(0:MAX_STATE_LOOKUP_IDS)


                    
                    
      INTEGER*2 THIS_YEAR, &
                STATE_PROVINCE_INDEX(-1:MAX_STATE_LOOKUP_IDS), &
                STATE_PROVINCE_ADDRESS(MAX_STATE_LOOKUP_IDS), &
                UNIT_STATE_PROVINCE_INDEX(MAX_CL_UNITS), &
                UNIT_GAS_REGION_INDEX(MAX_CL_UNITS), &
                MAX_STATE_PROVINCE_NO




! ADDL EXPANSION



                
      INTEGER*2 , &
                dTYPE_ID(MAX_CL_UNITS), &
                MAX_TRANS_DATA_BASES/0/, &
                MAX_DAY_TYPES/0/,MAX_TRANS_GROUPS,DAY_ID,TRANS_ID, &
                DAY_TYPE_COUNTER, &
                MAX_TRANS_ID_USED/1/, &
                UPPER_TRANS_GROUP/0/, &
                TG_ASSET_CLASS,TG, &
                CM,CG,PG, &
                RPS_COUNT,PX, &
                NUM_TRANS, &
                MONTHLY_FUEL_Idx(MAX_CL_UNITS), &
                UNIT_BEG_FO_HR(12),UNIT_END_FO_HR(12), &
                & ! Johncheck
                & ! moved to module CLA_OBJECT_ARRAYS
                &    ! CO2_CONTROL_DATE(MAX_CL_UNITS),
                HG_CTL_DATE(MAX_CL_UNITS), &
                OTH3_CONTROL_DATE(MAX_CL_UNITS), &
                NOX_CTL_DATE(MAX_CL_UNITS), &
                SOX_CTL_DATE(MAX_CL_UNITS), &
                SOX_CONTROL_DATE_TEMP(MAX_CL_UNITS)



      REAL FUEL_MIX_PTR(MAX_CL_UNITS), &
           PBTUCT_SAVE(MAX_CL_UNITS), &
           SBTUCT_SAVE(MAX_CL_UNITS), &
           PRIM_HEAT_CONTENT(MAX_CL_UNITS), &
           FUELADJ_SAVE(MAX_CL_UNITS), &
           VCPMWH_IN(MAX_CL_UNITS), &
           FIXED_COST_IN(MAX_CL_UNITS), &
           VCPMWH_IN_SAVE(MAX_CL_UNITS), &
           FIXED_COST_SAVE(MAX_CL_UNITS), &
           ANNUAL_CL_FIXED_COST_SAVE(MAX_CL_UNITS), &
           DISPADJ_SAVE(MAX_CL_UNITS), &
           DISPADJ2_SAVE(MAX_CL_UNITS), &
           CL_AI_CAPACITY_RATE_SAVE(MAX_CL_UNITS), &
           CL_AI_ENERGY_RATE_SAVE(MAX_CL_UNITS),  &
           EMISS_FUEL_COST_SAVE(MAX_CL_UNITS), &
           DISPATCH_MULT_SAVE(MAX_CL_UNITS), &
           EXCESS_ENERGY_SALES_SAVE(MAX_CL_UNITS), &
           CL_AI_REMAINING_LIFE_SAVE(MAX_CL_UNITS), &
           MIN_CAP_FACTOR(MAX_CL_UNITS), &
           MAX_CAP_FACTOR(MAX_CL_UNITS), &
           STARTUP_COSTS(MAX_CL_UNITS), &
           STARTUP_COSTS_ESCALATION(MAX_CL_UNITS),  & !  212
           RAMP_RATE_array(MAX_CL_UNITS), &
           RAMP_DOWN_RATEs(MAX_CL_UNITS), &
           MIN_DOWN_TIMEs(MAX_CL_UNITS), &
           MINimum_UP_TIME(MAX_CL_UNITS), &
           FOR_FREQUENCies(MAX_CL_UNITS), &
           FOR_DURAtn(MAX_CL_UNITS), &
           CUBIC_HEAT_CURVE(0:3,MAX_CL_UNITS), &
           & !Johncheck
           MIN_SPIN_Cpcty(MAX_CL_UNITS), &
           MAX_SPN_CAP(MAX_CL_UNITS), &
           TRANS_ANNUAL_UNSERVED_COST, &
           WHOLESALE_MARKET_COST, &
           ANNUAL_GROSS_MARGIN(MAX_CL_UNITS,0:3), &
           ECON_CO2_RETIRE_PRICE(MAX_CL_UNITS), &
           ECON_CO2_RETRO_PRICE(MAX_CL_UNITS), &
           TEMP_R4, &
           WINTER_TOTAL_CAPACITY, &
           R_FUEL_DELIVERY_1, &
           R_FUEL_DELIVERY_2, &
           R_FUEL_DELIVERY_3, &
           NOX_CNTRL_PERCENT(MAX_CL_UNITS), &
           SOX_CNTRL_PERCENT(MAX_CL_UNITS), &
           SOX_CONTROL_PERCENT_TEMP(MAX_CL_UNITS), &
           EMRGCY_CAPACITY(MAX_CL_UNITS), &
           HG_CNTRL_PERCENT(MAX_CL_UNITS), &
           OTHER3_CNTRL_PERCENT(MAX_CL_UNITS), &
           EMRGENCY_HEATRATE(MAX_CL_UNITS)




      INTEGER*2 J,BASE_DATE,GET_YR,LAST_ACTIVE_UNIT, &
        DATE_CHECK, &
                BASE_YEAR_DATE,LOCAL_YEAR




      INTEGER*2 GGI,MONTHLY_INTEGER(12)
      INTEGER*2 ALLOCATION_VECTOR
      REAL*4 ALLOCATION_VALUE(AVAIL_DATA_YEARS)
      REAL*4 TOTAL_MONTHLY_MAINTENANCE(0:12), &
             GROUP_MW_MAINTENANCE(0:12,0:MAX_REPORTING_GROUPS)

      LOGICAL (kind=1) :: FALSE_BYTE=.FALSE., &
                STARTS_ONLY=.FALSE.

! ASSET CLASS DATA

      

! NUM_FUEL_CATEGORIES   1 = COAL
!                       2 = GAS
!                       3 = OIL
!                       4 = NUCLEAR
!                       5 = WATER
!                       6 = OTHER
!                       7-9 = RESERVED
!                       10 =  WHOLESALE SALES
!                       11 =  WHOLESALE PURCHASES
      CHARACTER*20 SPECIAL_ID_NAME



      


      INTEGER*2   CL_MAINT_NO,BEGIN_DATE
      INTEGER CL_MAINT_REC
      REAL        DAYS_PER_MONTH(13)/31.,28.,31.,30.,31.,30.,31., &
                                    31.,30.,31.,30.,31.,365./

      INTEGER*2 CURRENT_YEAR,CURRENT_YEAR_COMPARISON,UNUM, &
                CLASS_POINTER,COLLECTION,EXPENSE_GROUP
      CHARACTER*1 DUMMY_TYPE


      REAL*4 EL_BASE_RATE,EL_PEAK_RATE, &
             BASE_MARKET_ENRG_SALES, &
             BASE_MARKET_REVENUES, &
             PEAK_MARKET_ENRG_SALES, &
             PEAK_MARKET_REVENUES

      REAL ASSET_ALLOCATOR
      


      LOGICAL*1 SHADOW_UNITS_ACTIVE
      INTEGER*2 MO,MY_SHADOW,NUM_OF_NUCLEAR_UNITS

      
      REAL*4 TEMP_CL_MAINTENANCE,MAINTENANCE_CAPACITY
      INTEGER*2 MW_MAINT_NO
      INTEGER MW_MAINT_REC
      CHARACTER*9 GROUP_NAME(0:MAX_REPORTING_GROUPS)/'Group 0', &
                 'Group 1','Group 2','Group 3','Group 4','Group 5', &
                 'Group 6','Group 7','Group 8','Group 9','Group 10', &
                 'Group 11','Group 12','Group 13','Group 14', &
        'Group 15'/

! DOE AND DECOMMISSIONING VARIABLES






      INTEGER*2 FUEL_ID,UNITNO
      character (len=6) :: TEMP_PRIM_FUEL_TYPE_STR
      CHARACTER*6 PRIM_ft_STR

      CHARACTER*20      RDI_COMPANY_NAME, &
                        RDI_NERC_REGION, &
                        RDI_SUB_REGION, &
                        RDI_PM_ABBREV, &
                        RDI_PRIME_FUEL
      CHARACTER*30      RDI_DESC,CO2_BASIN_NAME
      REAL*4      RDI_DEMONSTRATED_CAPACITY_MW, &
                  RDI_INCREMENTAL_FUEL_COST_MWH, &
                  RDI_AVERAGE_HEAT_RATE


! GADS DATA BASE FOR FOR'S AND MOR'S


      REAL*4 GADS_FOR(8,5), GADS_MOR(8,5)

! UNIT SIZES (MW) 1-99 100-199 200-299 300-399 400-599 600-799 800-1000
! 1000+

          DATA GADS_FOR/0.1045,0.1086,0.1240,0.1472,0.1444, &
                    0.1254,0.1253,0.1555, &
                    0.0758,0.1260,0.1061,0.1465,0.1423, &
                    0.1663,0.1154,0.1154, &
                    0.1450,0.1450,0.1450,0.1450,0.1450, &
                    0.1450,0.1585,0.2106, &
                    0.0641,0.1052,0.1241,0.1706,0.1322, &
                    0.1697,0.0804,0.0804, &
                    0.1241,0.1241,0.1241,0.1241,0.1241, &
                    0.1241,0.1241,0.1241/

          data GADS_MOR/ 1.1,1.3,1.3,1.3,1.3,0.7,0.6,1.4, &
                    0.8,1.0,1.0,2.3,1.3,1.2,0.7,0.7, &
                    6.7,6.7,6.7,6.7,6.7,6.7,6.7,6.7, &
                    1.3,1.6,1.5,2.0,1.8,1.6,1.5,1.5, &
                    0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9/ ! LIGNITE

      LOGICAL*1 BASE_EL_REVENUE_CASE, &
                   PEAK_EL_REVENUE_CASE



      INTEGER     & !  H2_UNIT_ID_NUM,
                  POWERDAT_PLANT_ID(MAX_CL_UNITS), &
                  I4,HIGHEST_ID


      
      INTEGER*2 ON_LINE_MONTH_TEMP

! 06/13/03. FOR THE ECITY REPORT




! MULTI-FILE VARIABLES

      LOGICAL*1 PROCESSING_FIRST_DATA_FILE
      INTEGER*2 MAX_CL_FILES/36/
      INTEGER   FILE_ID
      INTEGER*4 UNIQUE_ID_NUM,CL_UNIT_UNIQUE_RPT_ID(MAX_CL_UNITS)

      LOGICAL*1 TRANSACT_ACTIVE_IN_ENDPOINT


! FE P&L VARIABLES



      LOGICAL (KIND=4) :: EFORS0=.FALSE., SORS0=.FALSE.
! SPCapEx Additions Nov. 2005

         CHARACTER (LEN=1) :: PHASE_I_STR
         INTEGER (KIND=2) :: ID,UNIT_NAME_COUNT(MAX_CL_UNITS)
         CHARACTER (LEN=46) :: UNIT_NAME
         CHARACTER (LEN=48) :: TEMP_UNIT_NAME
         REAL (KIND=4) :: CL_CAPACITY_PLANNING_REMOVALS
         
! END SPCapEx Additions

! ADDITONS TO CREATE CL FILE OF RESOURCES ADDTIONS 12/06/06 DR.G

      
      CHARACTER (LEN=256) :: BASE_FILE_DIRECTORY
      INTEGER (KIND=2) :: YEAR_CHECK
      INTEGER (KIND=2) :: END_POINT_CHECK=-999, &
                          SEQU_NO

      LOGICAL (KIND=1) :: MRX_TEXT_FILE_NOT_OPEN=.TRUE., &
                          VECTOR_IS_VALUE
      LOGICAL (KIND=4) :: MRX_TEXT_FILE_OPEN=.FALSE.
      CHARACTER (LEN=2) :: UNIT_TYPE,UNIT_TYPE_CATEGORY
      CHARACTER (LEN=18) :: TEMP_EXPENSE_ASSIGNMENT, &
                            TEMP_EXPENSE_COLLECTION
      CHARACTER (LEN=10) :: TEMP_SEC_FUEL_TYPE, &
                            TEMP_EMISS_FUEL_TYPE
      CHARACTER (LEN=3) :: TEMP_INTRA_COMPANY_TRANSACTION
      CHARACTER (LEN=15) :: TEMP_UTILITY_OWNED
      CHARACTER (LEN=6) :: TEMP_MARKET_RESOURCE

      CHARACTER (LEN=9) :: TEMP_START_UP_LOGIC, &
                           TEMP_EMISSION_DATA_UNITS
      CHARACTER (LEN=5) :: TEMP_UNIT_ACTIVE, &
                           TEMP_REPORT_THIS_UNIT, &
                           TEMP_CONTRIBUTES_TO_SPIN, &
                           TEMP_APPLY_NOX_SEASON_DATE, &
                           TEMP_ALLOW_DECOMMIT, &
                           TEMP_USE_POLY_HEAT_RATES, &
                           TEMP_AGGREGATE_THIS_UNIT, &
                           TEMP_LINKED_BETTERMENT_OPTION
      CHARACTER (LEN=20) :: TEMP_WVPA_RATE_TRACKER, &
                            TEMP_WVPA_RES_TRACKER, &
                            TEMP_WVPA_FUEL_TRACKER, &
                            TEMP_MONTE_OR_FREQ, &
                            TEMP_WVPA_MEM_TRACKER, &
                            TEMP_PRIMARY_FUEL_CATEGORY, &
                            TEMP_SECONDARY_FUEL_CATEGORY, &
                            TEMP_EMISSIONS_FUEL_CATEGORY, &
                            TEMP_RETIREMENT_CANDIDATE, &
                            TEMP_RETROFIT_CANDIDATE
      REAL (KIND=4) :: ESCAL_RATE
      CHARACTER*30 SCREEN_OUTPUT
      character (len=1024) :: name_of_file
      END MODULE