module screen_interface_data
use foshydi2com
implicit none

      real (kind=4) :: MINIMUM_CAPACITY_FACTOR,MAXIMUM_CAPACITY_FACTOR
      integer (kind=2) :: PROD_PNTR_BY_CL_UNITS(:)
      integer (kind=2) :: SUNITS
      CHARACTER(len=2) ::     &
                     UNIT_TYPE, &
                     UNIT_TYPE_CATEGORY
      
      real (kind=4) :: max_spin_cap=0., min_spin_cap=0.





!
!  DECLARATIONS FOR THE EXPANSION PLANT OPERATIONS FILE
!
      CHARACTER*20   S_UNITNM(:)


      CHARACTER*6  PRIM_FUEL_TYPE


      INTEGER*2      GENGRP, &
                     ONLIMO,OFLIMO, &
                     S_PFESCR(:), &
                     S_SFESCR(:), &
                     S_OMESCR(:), &
                     S_FIXED_COST_ESCALATOR(:), &
                     FUEL_SUPPLY_ID, &
                     EMISS_FUEL_EMISS_PTR, &
                     PRIM_FUEL_EMISS_PTR, &
                     PROD_PNTR/5000/, & !  030310.
                     TIE_CONSTRAINT_GROUP, & !  INT*2
                     S_MONTHLY_CAPACITY_POINTER(:), & !  INT*2
                     S_ANNUAL_CL_FIXED_COST_ESC(:), & !  INT*2
                     SEC_FUEL_EMISS_PTR, &
                     EMISS_FUEL_ESCAL, &
                     NOX_SEASON_DATE(:), &
                     FIRST_RETIREMENT_YEAR, &
                     S_RPS_PROGRAM_NUMBER(:), &
                     R_RPS_PROGRAM




      real (kind=4) :: cl_ai_capacity_rate_ord
      REAL*4 :: S_FUELMX(:),S_PBTUCT(:), &
                     S_SBTUCT(:), &
                     S_FUELADJ_IN(:),S_EFOR(:), &
                     S_FUELADJ(:), &
                     S_MNRATE(:,:), &
                     S_VCPMWH_IN(:), &
                     S_FIXED_COST_IN(:), &
                     S_HR_FACTOR(:), &
                     S_INPUT_MW(:,:), &
                     S_CAP_PLANNING_FAC(:), &
                     S_COEFF(:,:), &
                     P_PARTICULATES(:), &
                     CL_POOL_FRAC_OWN, &
                     P_EMIS_OTH2(:), &
                     P_EMIS_OTH3(:), &
                     P_NOX_BK2, &
                     EMISS_FUEL_COST, &
                     EMISS_BLENDING_RATE, &
                     S_ANNUAL_CL_FIXED_COST(:), &
                     MAINT_DAYS, & 
                     NOX_VOM(:), &
                     NOX_FOM(:), &
                     SOX_VOM(:), &
                     SOX_FOM(:), &
                     CO2_VOM(:), &
                     CO2_FOM(:), &
                     HG_VOM(:), &
                     HG_FOM(:), &
                     OTHER3_VOM(:), &
                     OTHER3_FOM(:), &
                     LATITUDE, &
                     LONGITUDE, &
                     MW_INSIDE_FENCE, &
                     S_INTER_BLOCKS(:,:), &
                     MARKET_FLOOR, &
                     MARKET_CEILING, &
                     START_UP_COSTS_ESCALATION(:), &
                     A_HR,B_HR,INC_CAP

      INTEGER (kind=2) :: &
                     CAPACITY_MARKET_POINTER_ocl, &
                     RETROFIT_PROJECT_ID
       REAL (kind=4) :: CAPACITY_MARKET_COIN_ADJ_FACT, &
                     S_RPS_PERCENT(:), &
                     CO2_PIPELINE_DISTANCE, &
                     CO2_RETRO_HEAT_MULT, &
                     CO2_RETRO_CAP_MULT
      CHARACTER*5 &
                     CAPACITY_MARKET_TYPE, &
                     CAPACITY_MARKET_MONTH, &
                     CAPACITY_MARKET_EXP_COLLECT
      CHARACTER*20   CAPACITY_MARKET_COST_ASSIGN, &
                     STATE_PROV_NAME
      character (len=20) :: NEWGEN_UNIT_STATUS
      CHARACTER*36 THERMAL_GUID
      CHARACTER*1 APPLY_NOX_SEASON_DATE(:), &
                   THERMAL_AGGREGATED_UNIT
      CHARACTER*18 EXPENSE_ASSIGNMENT,EXPENSE_COLLECTION
      CHARACTER*10 SEC_FUEL_TYPE,EMISS_FUEL_TYPE
      character (len=3) ::  INTRA_COMPANY_TRANSACTION
      character (len=20) :: SPECIAL_UNIT_ID
      character (len=5) :: UNIT_ACTIVE
      character (len=15) :: UTILITY_OWNED
      character (len=9) :: START_UP_LOGIC
      character (len=5) :: CONTRIBUTES_TO_SPIN
      character (len=6) :: MARKET_RESOURCE
      character (len=5) :: USE_POLY_HEAT_RATES
      
      character (len=20) :: WVPA_RATE_TRACKER, &
                           WVPA_RES_TRACKER, &
                           WVPA_FUEL_TRACKER, &
                           MONTE_OR_FREQ, &
                           WVPA_MEM_TRACKER
      character (len=5) :: ALLOW_DECOMMIT
      character (len=20) :: PRIMARY_FUEL_CATEGORY, &
                           SECONDARY_FUEL_CATEGORY, &
                           EMISSIONS_FUEL_CATEGORY, &
                           RETIREMENT_CANDIDATE, &
                           RETROFIT_CANDIDATE
      CHARACTER*6 &
                           BASECASE_PLANT_ID, &
                           BASECASE_UNIT_ID, &
                           BASECASE_TRANS_AREA_ID, &
                           BASECASE_PLANT_NERC_SUB_ID, &
                           BASECASE_PLANT_OWNER_ID, &
                           PRIMARY_MOVER_STR(:), &
                           PRIM_FUEL_TYPE_STR
      CHARACTER*20 COUNTY
      CHARACTER*6 STATE_PROVINCE




      INTEGER*2 &
                           AI_CL_REMAINING_LIFE_ocl, &
                           ASSET_CLASS_NUM, &
                           ASSET_CLASS_VECTOR, &
                           INTRA_COMPANY_CLASS_ID, &
                           S_TRANSACTION_GROUP_ID(:), &
                           MONTHLY_FUEL_INDEX, &
                           DAY_TYPE_ID, &
                           S_PRIMARY_MOVER(:), &
                           S_UNIT_GAS_REGION_INDEX(:), &
                           State_Index(:), &
                           NOX_CONTROL_DATE(:), &
                           SOX_CONTROL_DATE(:), &
                           CO2_CONTROL_DATE(:), &
                           HG_CONTROL_DATE(:), &
                           OTHER3_CONTROL_DATE(:), &
                           MONTHLY_MUST_RUN_VECTOR(:), &
                           MONTHLY_DECOMMIT_VECTOR, &
                           S_EMISSION_MARKET_LINK(:), &
                           PM, &
                           ST_TG, &
                           BASE_PN_RECORDS






      REAL*4 :: &
                           P_FUEL_DELIVERY(:), &
                           P_FUEL_DELIVERY_2(:), &
                           P_FUEL_DELIVERY_3(:), &
                           START_UP_COSTS(:), &
                           MIN_DOWN_TIME(:), &
                           MIN_UP_TIME(:), &
                           S_FUEL_DELIVERY(:), &
                           S_FUEL_DELIVERY_2(:), &
                           S_FUEL_DELIVERY_3(:), &
                           NOX_CONTROL_PERCENT(:), &
                           SOX_CONTROL_PERCENT(:), &
                           CO2_CONTROL_PERCENT(:), &
                           HG_CONTROL_PERCENT(:), &
                           OTHER3_CONTROL_PERCENT(:)


!  DECLARATIONS FOR SCREENING VARIABLES
!
      real (kind=4), allocatable :: AHR(:)
      real (kind=4) :: HEAT_RATE_FACTOR=0.
!
!  OPERATIONS FILE
      ALLOCATABLE :: S_FUELMX,S_PBTUCT,S_PFESCR,S_SBTUCT, &
                     S_RPS_PROGRAM_NUMBER, &
                     S_RPS_PERCENT, &
                     P_FUEL_DELIVERY, &
                     P_FUEL_DELIVERY_2, &
                     P_FUEL_DELIVERY_3, &
                     START_UP_COSTS, &
                     START_UP_COSTS_ESCALATION, &
                     MIN_DOWN_TIME, &
                     MIN_UP_TIME, &
                     S_FUEL_DELIVERY, &
                     S_FUEL_DELIVERY_2, &
                     S_FUEL_DELIVERY_3, &
                     APPLY_NOX_SEASON_DATE, &
                     S_PRIMARY_MOVER, &
                     S_UNIT_GAS_REGION_INDEX, &
                     State_Index, &
                     NOX_SEASON_DATE, &
                     NOX_CONTROL_PERCENT, &
                     PRIMARY_MOVER_STR, &
                     CO2_CONTROL_PERCENT, &
                     HG_CONTROL_PERCENT, &
                     OTHER3_CONTROL_PERCENT, &
                     NOX_CONTROL_DATE, &
                     CO2_CONTROL_DATE, &
                     HG_CONTROL_DATE, &
                     MONTHLY_MUST_RUN_VECTOR, &
                     OTHER3_CONTROL_DATE, &
                     SOX_CONTROL_PERCENT, &
                     SOX_CONTROL_DATE, &
                     SOX_VOM, &
                     SOX_FOM, &
                     NOX_VOM, &
                     NOX_FOM, &
                     CO2_VOM, &
                     CO2_FOM, &
                     HG_VOM, &
                     HG_FOM, &
                     OTHER3_VOM, &
                     OTHER3_FOM
      ALLOCATABLE :: S_SFESCR,S_FUELADJ_IN,S_FUELADJ, &
                     S_VCPMWH_IN,S_OMESCR,S_EFOR, &
                     S_MNRATE, &
                     S_INTER_BLOCKS
      ALLOCATABLE :: S_FIXED_COST_IN,S_FIXED_COST_ESCALATOR, &
                     S_MONTHLY_CAPACITY_POINTER
      ALLOCATABLE :: S_HR_FACTOR,S_INPUT_MW
      ALLOCATABLE :: S_CAP_PLANNING_FAC,S_COEFF, &
                     P_PARTICULATES, &
                     P_EMIS_OTH2, &
                     P_EMIS_OTH3
      ALLOCATABLE :: PROD_PNTR_BY_CL_UNITS
      ALLOCATABLE :: S_UNITNM
      ALLOCATABLE :: S_ANNUAL_CL_FIXED_COST, &
                     S_TRANSACTION_GROUP_ID, &
                     S_EMISSION_MARKET_LINK
      ALLOCATABLE :: S_ANNUAL_CL_FIXED_COST_ESC
! OPERATIONS FIL & !  OPERATIONS FILE
      SAVE &
            S_FUELMX,S_PBTUCT,S_PFESCR,S_SBTUCT, &
            S_RPS_PROGRAM_NUMBER, &
            S_RPS_PERCENT, &
            P_FUEL_DELIVERY, &
            P_FUEL_DELIVERY_2, &
            P_FUEL_DELIVERY_3, &
            START_UP_COSTS, &
            START_UP_COSTS_ESCALATION, &
            MIN_DOWN_TIME, &
            MIN_UP_TIME, &
            S_FUEL_DELIVERY, &
            S_FUEL_DELIVERY_2, &
            S_FUEL_DELIVERY_3, &
            APPLY_NOX_SEASON_DATE, &
            S_PRIMARY_MOVER, &
            S_UNIT_GAS_REGION_INDEX, &
            State_Index, &
            NOX_SEASON_DATE, &
            NOX_CONTROL_PERCENT, &
            PRIMARY_MOVER_STR, &
            CO2_CONTROL_PERCENT, &
            HG_CONTROL_PERCENT, &
            OTHER3_CONTROL_PERCENT, &
            NOX_CONTROL_DATE, &
            CO2_CONTROL_DATE, &
            HG_CONTROL_DATE, &
            OTHER3_CONTROL_DATE, &
            MONTHLY_MUST_RUN_VECTOR, &
            SOX_CONTROL_PERCENT, &
            SOX_CONTROL_DATE, &
            SOX_VOM, &
            SOX_FOM, &
            NOX_VOM, &
            NOX_FOM, &
            CO2_VOM, &
            CO2_FOM, &
            HG_VOM, &
            HG_FOM, &
            OTHER3_VOM, &
            OTHER3_FOM, &
            S_SFESCR,S_FUELADJ_IN,S_FUELADJ, &
            S_MNRATE, &
            S_INTER_BLOCKS, &
            S_VCPMWH_IN,S_OMESCR,S_EFOR, &
            S_FIXED_COST_IN,S_FIXED_COST_ESCALATOR, &
            S_MONTHLY_CAPACITY_POINTER, &
            S_HR_FACTOR,S_INPUT_MW, &
            S_CAP_PLANNING_FAC,S_COEFF,AHR,S_UNITNM, &
            P_PARTICULATES, &
            P_EMIS_OTH2, &
            P_EMIS_OTH3, &
            S_ANNUAL_CL_FIXED_COST, &
            S_TRANSACTION_GROUP_ID, &
            S_EMISSION_MARKET_LINK, &
            S_ANNUAL_CL_FIXED_COST_ESC, &
            & ! LOCAL VARIABLE & 
            PROD_PNTR_BY_CL_UNITS,SUNITS
!  SPCapEx additions Nov. 2005
      
      LOGICAL (KIND=1) :: SPCapExOption
      CHARACTER (LEN=15),DIMENSION(155) :: CL_VARIABLES
      character (len=5) :: report_this_unit_ord=" "
      character (len=5) :: aggregate_this_unit_ord=" "


      

end module screen_interface_data
