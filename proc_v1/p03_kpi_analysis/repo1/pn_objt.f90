!      Last change: msg 3/17/2022 11:56:39 AM
!			MSG 5/22/2016 5:36:58 PM
      SUBROUTINE PN_OBJECT
      use end_routine, only: end_program, er_message
      use p_fuel_annl
      use spindriftlib
      use sizecom
      use up_objt_decs


      CHARACTER(len=20) :: R_LOCAL_UNIT_NAME
      INTEGER(kind=2) :: R_LOCAL_DATA_POINTER, &
                     R_LOCAL_TRANS_ID
      REAL :: R_LOCAL_UNIT_CAP
!
      INTEGER(kind=2) :: IREC,INUNIT,DELETE,LRECL=1628, & 
                 BASE_PN_RECORDS,R_BASE_PN_RECORDS
      SAVE BASE_PN_RECORDS
      INTEGER(kind=4) :: IOS,IOS_BASE
      INTEGER (KIND=2) :: AvailableGenericCoalUnits,AvailCoal
      INTEGER(kind=2) :: LAST_UNIT_NUM_OPENED=10,UNIT_NUM
      CHARACTER(len=5) :: BASE_FILE_NAME,OVERLAY_FAMILY_NAME, &
                          NWCAP_PROD_FIL
      CHARACTER(len=20) :: STATE_PROV_NAME
      CHARACTER(len=30) :: DESC,CO2_BASIN_NAME
      CHARACTER(len=36) :: THERMAL_GUID
      CHARACTER(len=256) :: FILE_NAME
      CHARACTER(len=256) :: BASE_FILE_DIRECTORY,OUTPUT_DIRECTORY
      LOGICAL(kind=4) :: FILE_EXISTS,NEW_CL_UNIT_OPENED
!  DECLARATION FOR DBREAD COMMON BLOCK
!      CHARACTER(len=1024 RECLN
      CHARACTER(len=2048) :: RECLN ! 011809
!  DECLARATION FOR /FOSSIL FILE/
      INTEGER(kind=2) :: GENGRP,ONLIMO,OFLIMO,PRESCR,SDESCR,VRESCR, &
                AI_CAPACITY_ESCALATOR,AI_ENERGY_ESCALATOR, &
                TIE_GROUP,P_FUEL_SUPPLY_ID, &
                FIXED_COST_ESCALATOR, &
                MONTHLY_CAPACITY_POINTER, &
                ASSET_CLASS_NUM, &
                ASSET_CLASS_VECTOR, &
                INTRA_COMPANY_CLASS_ID,TRANSACTION_GROUP_ID, &
                MONTHLY_FUEL_INDEX, &
                NOX_SEASON_DATE, &
                NOX_CONTROL_DATE, &
                SOX_CONTROL_DATE, &
                CO2_CONTROL_DATE, &
                HG_CONTROL_DATE, &
                OTHER3_CONTROL_DATE, &
                MONTHLY_MUST_RUN_VECTOR, &
                MONTHLY_DECOMMIT_VECTOR, &
                EMISSION_MARKET_LINK, &
                RETROFIT_PROJECT_ID, &
                FIRST_RETIREMENT_YEAR,RPS_PROGRAM_NUMBER
      INTEGER(kind=8) :: HESI_UNIT_ID_NUM, &
                TRANS_BUS_ID, &
                THERMAL_PARENT_ID, &
                H2_UNIT_ID_NUM
      INTEGER :: & !  H2_UNIT_ID_NUM,
                POWERDAT_PLANT_ID
!
      REAL :: PLANNING_FACTOR,ANNUAL_CL_FIXED_COST,AI_TAX_LIFE, &
           AI_ADR_TAX_LIFE,START_UP_COSTS, &
           RAMP_RATE,RAMP_DOWN_RATE,MIN_DOWN_TIME,MIN_UP_TIME, &
           FOR_FREQUENCY,FOR_DURATION, &
           WINTER_TOTAL_CAPACITY,NOX_CONTROL_PERCENT, &
           SOX_CONTROL_PERCENT,CO2_CONTROL_PERCENT, &
           HG_CONTROL_PERCENT,OTHER3_CONTROL_PERCENT, &
           EMERGENCY_CAPACITY,EMERGENCY_HEATRATE,NOX_VOM,NOX_FOM, &
           SOX_VOM,SOX_FOM,CO2_VOM,CO2_FOM,HG_VOM,HG_FOM,OTHER3_VOM, &
           OTHER3_FOM,LATITUDE,LONGITUDE,MW_INSIDE_FENCE,CONSTANT_POLY, &
           FIRST_POLY,SECOND_POLY,THIRD_POLY,MIN_SPIN_CAP, &
           MAX_SPIN_CAP,INTER_BLOCKS(3),MARKET_FLOOR,MARKET_CEILING, &
           RPS_CONTRIBUTION_PERCENT,CO2_PIPELINE_DISTANCE, &
           CO2_RETRO_HEAT_MULT,CO2_RETRO_CAP_MULT

      INTEGER(kind=2) :: ANNUAL_CL_FIXED_COST_ESC
      REAL(kind=4) :: DISPATCH_MULT,MAINT_DAYS,EXCESS_ENERGY_SALES
      REAL :: MW(2)
      INTEGER(kind=2) :: RESOURCE_ID,FUEL_SUPPLY_ID
!  END 5/14/92 CHANGES
      REAL :: FRAOWN,FUELMX,PRFCST,SEFCST,EFOR,FUELADJ,MAINRT(12), &
            VARCST,DISPADJ,HRFACT,COEFF(3), &
            AI_CAPACITY_RATE,AI_ENERGY_RATE,POOL_FRAC_OWN, &
            MINIMUM_CAPACITY_FACTOR,MAXIMUM_CAPACITY_FACTOR, &
            P_FuelDev_0, &
            P_FuelDev_2, &
            P_FuelDev_3, &
            S_FuelDev_0, &
            S_FuelDev_2, &
            S_FuelDev_3
      INTEGER(kind=2) :: AI_REMAINING_LIFE
!  DECLARATION FOR EMISSION STUFF
      REAL :: P_SO2,P_NOX,P_PARTICULATES, &
           P_NOX_BK1,P_CO2,FIXED_COST,DISP_ADJ2_OUT,P_NOX_BK2_OUT, &

           P_OTHER2,P_OTHER3,DISPADJ2, &

           P_NOX_BK2,EMISS_FUEL_COST,EMISS_BLENDING_RATE
      INTEGER(kind=2) :: SEC_EMISS_PTR,EMISS_FUEL_ESCAL, &
                EMISS_FUEL_EMISS_PTR,PRIM_FUEL_EMISS_PTR
      CHARACTER(len=20) :: UNIT_NAME
      CHARACTER(len=26) :: FILE_TYPE='Expansion-Production Units'
      CHARACTER(len=2) :: NEWPLT_PROD_OL='BC'
      CHARACTER(len=6) :: PRIM_FUEL_TYPES
      CHARACTER (LEN=2) :: CL_LOAD_TYPE,UNIT_TYPE,UNIT_TYPE_CATEGORY

!
!  OPTIONS CHECKING
!
      INTEGER(kind=2) :: I,R_UNITS_2_CHECK, &
                R_DATA_POINTER(*)
      REAL :: R_UNIT_CAP(*)
      CHARACTER(len=*) :: R_UNIT_NAME(*),R_RESOURCE_TYPE(*)
      REAL :: CAP_MW, POINTR,MONTH_MULT
      INTEGER(kind=2) :: YEAR_START,PEAK_MONTH,DAY_TYPE_ID
      CHARACTER (LEN=18) :: EXPENSE_ASSIGNMENT, &
                            EXPENSE_COLLECTION
      CHARACTER (LEN=10) :: FUEL_TYPES(2)
      CHARACTER (LEN=15) :: UTILITY_OWNED
      CHARACTER (LEN=9) :: START_UP_LOGIC
      CHARACTER (LEN=3) :: INTRA_COMPANY_TRANSACTION
      CHARACTER (LEN=5) :: UNIT_ACTIVE, &
                           REPORT_THIS_UNIT, &
                           CONTRIBUTES_TO_SPIN, &
                           APPLY_NOX_SEASON_DATE, &
                           ALLOW_DECOMMIT, &
                           USE_POLY_HEAT_RATES
      CHARACTER (LEN=5) :: AGGREGATE_THIS_UNIT="False", &
                           LINKED_BETTERMENT_OPTION="False"
      CHARACTER(len=1) :: THERMAL_AGGREGATED_UNIT
      CHARACTER (LEN=9) :: EMISSION_DATA_UNITS="lbs/mmBTU"
      CHARACTER (LEN=6) :: MARKET_RESOURCE
      CHARACTER (LEN=20) :: WVPA_RATE_TRACKER, &
                            WVPA_RES_TRACKER, &
                            WVPA_FUEL_TRACKER, &
                            MONTE_OR_FREQ, &
                            WVPA_MEM_TRACKER, &
                            PRIMARY_FUEL_CATEGORY, &
                            R_PRIMARY_FUEL_CATEGORY, &
                            SECONDARY_FUEL_CATEGORY, &
                            EMISSIONS_FUEL_CATEGORY, &
                            RETIREMENT_CANDIDATE, &
                            RETROFIT_CANDIDATE
      CHARACTER (LEN=20) :: SPECIAL_UNIT_ID

      CHARACTER(len=6) :: BASECASE_PLANT_ID, &
                     BASECASE_UNIT_ID, &
                     BASECASE_MARKET_AREA_ID, &
                     BASECASE_TRANS_AREA_ID, &
                     BASECASE_PLANT_NERC_SUB_ID, &
                     BASECASE_PLANT_OWNER_ID, &
                     PRIMARY_MOVER_STR
      CHARACTER(len=20) :: COUNTY,NEWGEN_UNIT_STATUS
      CHARACTER(len=6) :: STATE_PROVINCE
      LOGICAL(kind=1) :: LAHEY_LF95
      CHARACTER(len=50) :: SCREEN_OUTPUT
!  SPCapEx additions Nov. 2005
      CHARACTER (LEN=20)  :: tch_type
      LOGICAL (KIND=1) :: SPCapExOption
      CHARACTER (LEN=15),DIMENSION(155) :: CL_VARIABLES
      REAL (KIND=4) :: FIRST_YEAR_DECOMM_AVAIALABLE
      REAL (KIND=4) :: DECOMMISSIONING_BASE_YR_COST, &
                       DECOMMISSIONING_COST_ESCALATION, &
                       ANNUAL_ENERGY_PLANNING_FACTOR, &
                       NAME_PLATE_CAPACITY, &
                       FuelRatio_pn(5), &
                       BlendableFuelsPtr(4), & !  167
                       FuelTransportationCost(4), &
                       BlendedEnthalpyUp, &
                       BlendedEnthalpyLo, &
                       BlendedSO2Up,                  & !  177
                       BettermentProjectID, &
                       DECOM_CONTINUING_COST, &
                       DECOM_CONT_COST_ESCALATION, &
                       EnrgPatternPointer, & !  181
                       EmissRedRate(5), &
                       EmissMaxRate(5), &
                       MaxEnergyLimit(3), &
                       START_UP_COSTS_ESCALATION
      CHARACTER(len=10) :: FUEL_BLENDING_IS
      INTEGER(kind=2) :: capacity_market_pointer_pno
      REAL(kind=4) :: CAPACITY_MARKET_COIN_ADJ_FACT
      CHARACTER(len=5) :: &
                     CAPACITY_MARKET_TYPE, &
                     CAPACITY_MARKET_MONTH, &
                     CAPACITY_MARKET_EXP_COLLECT
      CHARACTER(len=20) ::   CAPACITY_MARKET_COST_ASSIGN
!

!
!           ROUTINE TO CONVERT METAFILE FILES TO DIRECT-ACESS BINARY
!           COPYRIGHT (C) 1983, 84, 85  M.S. GERBER & ASSOCIATES, INC.
!

!
!  CONVERT THE EXPANSION-PLANT FILE

      ENTRY PN_MAKEBIN

      BASE_FILE_NAME = NWCAP_PROD_FIL()
      FILE_NAME = trim(BASE_FILE_DIRECTORY())// &
                                   "PNB"//trim(BASE_FILE_NAME)//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
            CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
         ENDIF
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCPNWCP.BIN", &
                            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         IREC = 1
         READ(10,*) DELETE
         DO
            UNIT_NAME = 'Default Value Set'
            GENGRP = 0
            FRAOWN = 100.
            ONLIMO = 1
            OFLIMO = 12
            FUELMX = 1.0
            PRFCST = 0.
            PRESCR = 0
            SEFCST = 0.
            SDESCR = 0
            FUELADJ = 0.
            EFOR = 0.
            VARCST = 0.
            VRESCR = 0
            AI_CAPACITY_RATE = 0.
            AI_CAPACITY_ESCALATOR = 0.
            AI_ENERGY_RATE = 0.
            AI_ENERGY_ESCALATOR = 0.
            AI_REMAINING_LIFE = 0.
            P_SO2 = 0.
            P_NOX_BK1 = 0.
            P_CO2 = 0.
            P_OTHER2 = 0.
            P_OTHER3 = 0.
            POOL_FRAC_OWN = 100.
            DISPADJ2 = -99999.
            RESOURCE_ID = 0
            P_FUEL_SUPPLY_ID = 0
            P_NOX_BK2 = -99999.
            SEC_EMISS_PTR = -1
            EMISS_FUEL_COST = 0.
            EMISS_FUEL_ESCAL = 0
            EMISS_FUEL_EMISS_PTR = 0
            EMISS_BLENDING_RATE = 0.
            PRIM_FUEL_EMISS_PTR = 0
            PRIM_FUEL_TYPES = ' '
            FUEL_TYPES(1) = ' '
            FUEL_TYPES(2) = ' '
            TIE_GROUP = 0
            MONTHLY_CAPACITY_POINTER = 0
            ANNUAL_CL_FIXED_COST = 0.
            ANNUAL_CL_FIXED_COST_ESC = 0
            MAINT_DAYS = 0.
            DISPATCH_MULT = 1.
            EXCESS_ENERGY_SALES = 0.
            AI_TAX_LIFE = 99.
            AI_ADR_TAX_LIFE = 99.
            ASSET_CLASS_NUM = 0
            ASSET_CLASS_VECTOR = 0
            INTRA_COMPANY_CLASS_ID = -1
            INTRA_COMPANY_TRANSACTION = 'No'
            MINIMUM_CAPACITY_FACTOR = 0.
            MAXIMUM_CAPACITY_FACTOR = 1.
            TRANSACTION_GROUP_ID = 1
! ADDED 3/9/98. GAT.
            DAY_TYPE_ID = 0
            UNIT_ACTIVE = 'True'
            REPORT_THIS_UNIT = 'True'
            BASECASE_PLANT_ID = 'BLANK ' ! CHAR*6
            BASECASE_UNIT_ID = 'BLANK ' ! CHAR*6
            BASECASE_MARKET_AREA_ID = 'BLANK ' ! CHAR*6
            BASECASE_TRANS_AREA_ID = 'BLANK ' ! CHAR*6
            BASECASE_PLANT_NERC_SUB_ID = 'BLANK ' ! CHAR*6
            BASECASE_PLANT_OWNER_ID = 'BLANK ' ! CHAR*6
            UTILITY_OWNED = 'UTILITY'
            MONTHLY_FUEL_INDEX = 0
            START_UP_COSTS = 0.
            START_UP_LOGIC = 'False'
            RAMP_RATE = 999999.
            RAMP_DOWN_RATE = -999999.
            MIN_DOWN_TIME = 0.
            MIN_UP_TIME = 0.
! Merge2issue: P_FUEL_DELIVERY renamed to P_FUEL_DELIV (left)
! and P_FuelDev (right).  Going with Right choice. Probably ok.
            P_FuelDev_0 = 0.0
            P_FuelDev_2 = 0.0
            P_FuelDev_3 = 0.0
            S_FuelDev_0 = 0.0
            S_FuelDev_2 = 0.0
            S_FuelDev_3 = 0.0
            HESI_UNIT_ID_NUM = 0
            TRANS_BUS_ID = 0
            FOR_FREQUENCY = 0.
            FOR_DURATION = 0.
            WINTER_TOTAL_CAPACITY = 0.
            CONTRIBUTES_TO_SPIN = 'False'
            APPLY_NOX_SEASON_DATE = 'False'
            MARKET_RESOURCE = 'False'
            NOX_SEASON_DATE = 0150
            NOX_CONTROL_PERCENT = 0.
            SOX_CONTROL_PERCENT = 0.
            CO2_CONTROL_PERCENT = 0.
            HG_CONTROL_PERCENT = 0.
            OTHER3_CONTROL_PERCENT = 0.
            NOX_CONTROL_DATE = 5001
            SOX_CONTROL_DATE = 5001
            CO2_CONTROL_DATE = 5001
            HG_CONTROL_DATE = 5001
            OTHER3_CONTROL_DATE = 5001
!
            EMERGENCY_CAPACITY = 0.
            EMERGENCY_HEATRATE = 0.
!
            PRIMARY_MOVER_STR = 'PMNONE'
            COUNTY = 'BLANK'
            STATE_PROVINCE = '#N/A'
!
            NOX_VOM = 0.
            NOX_FOM = 0.
            SOX_VOM = 0.
            SOX_FOM = 0.
            CO2_VOM = 0.
            CO2_FOM = 0.
            HG_VOM = 0.
            HG_FOM = 0.
            OTHER3_VOM = 0.
            OTHER3_FOM = 0.
            LATITUDE = 0.
            LONGITUDE = 0.
            MW_INSIDE_FENCE = 0.
!
            MARKET_FLOOR = 0.
            MARKET_CEILING = 0.
!
            INTER_BLOCKS(1) = -99.
            INTER_BLOCKS(2) = -99.
            INTER_BLOCKS(3) = -99.
!
            USE_POLY_HEAT_RATES = 'False'
            NEWGEN_UNIT_STATUS = '05-Proposed'
            MONTHLY_MUST_RUN_VECTOR = 0
            MONTHLY_DECOMMIT_VECTOR = 0
            EMISSION_MARKET_LINK = -9999
            WVPA_RATE_TRACKER = 'Both IN & PSCR'
            WVPA_RES_TRACKER = 'Long_term'
            WVPA_FUEL_TRACKER = 'Internal'
            MONTE_OR_FREQ = 'Global_Assign'
            WVPA_MEM_TRACKER = 'Member'
            ALLOW_DECOMMIT = 'True'
            EXPENSE_ASSIGNMENT = 'Base Rates'
            EXPENSE_COLLECTION = 'Fossil Fuel'
!
            MIN_SPIN_CAP = 0.
            MAX_SPIN_CAP = 9999.
!
            SPECIAL_UNIT_ID = "None"
!
            FuelRatio_pn = 0.
            BlendableFuelsPtr = 0.
            FuelTransportationCost = 0.
            BlendedEnthalpyUp = 0.
            BlendedEnthalpyLo = 0.
            BlendedSO2Up = 0.
            BettermentProjectID = 0.
            DECOM_CONTINUING_COST = 0.
            DECOM_CONT_COST_ESCALATION = 0.
            EnrgPatternPointer = 0.
            EmissRedRate = 0.
            EmissMaxRate = 0.
            MaxEnergyLimit= 0.
            tch_type = " "
            FUEL_BLENDING_IS = 'Primary'
            FUELMX = 1. ! 070806.
            CAPACITY_MARKET_TYPE = 'NONE'
            CAPACITY_MARKET_COIN_ADJ_FACT = 1.0
            PRIMARY_FUEL_CATEGORY = ' '
            SECONDARY_FUEL_CATEGORY = ' '
            EMISSIONS_FUEL_CATEGORY = ' '
            RPS_CONTRIBUTION_PERCENT = 0.0
            RETIREMENT_CANDIDATE = 'F'
            RETROFIT_CANDIDATE = 'F'
!
            THERMAL_GUID = 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
            THERMAL_PARENT_ID = 123456789
            THERMAL_AGGREGATED_UNIT = 'F'
            FIRST_RETIREMENT_YEAR = 1901
            RPS_PROGRAM_NUMBER = 0
            AvailableGenericCoalUnits = 0
!
            CO2_RETRO_HEAT_MULT = 1.0
            CO2_RETRO_CAP_MULT = 1.0

!
            DO
               READ(10,1000,IOSTAT=IOS) RECLN
               IF(IOS /=0) EXIT
               IF(RECLN(1:1) == '7') EXIT
               RECLN = trim(RECLN)// &
                     ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                     ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                     ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
!
!  VARIABLES THAT DO NOT PROPAGATE LEFT TO RIGHT
!
!
               READ(RECLN,*) &
                     DELETE,UNIT_NAME,CL_LOAD_TYPE, &
                     EXPENSE_ASSIGNMENT,EXPENSE_COLLECTION, &
                     GENGRP,FRAOWN,ONLIMO,OFLIMO,FUELMX,PRFCST,PRESCR, &
                     SEFCST,SDESCR,FUELADJ,EFOR,MAINRT,VARCST,VRESCR, &
                     FIXED_COST,FIXED_COST_ESCALATOR, &
                     DISPADJ,HRFACT,MW,PLANNING_FACTOR,COEFF,DESC, &
                     P_SO2,P_NOX_BK1,P_CO2,POOL_FRAC_OWN, &
                     P_OTHER2,P_OTHER3,DISPADJ2, &
                     RESOURCE_ID,P_FUEL_SUPPLY_ID,P_NOX_BK2, &
                     SEC_EMISS_PTR,EMISS_FUEL_COST,EMISS_FUEL_ESCAL, &
                     EMISS_FUEL_EMISS_PTR,EMISS_BLENDING_RATE, &
                     PRIM_FUEL_EMISS_PTR, &
                     PRIM_FUEL_TYPES,FUEL_TYPES,TIE_GROUP, &
                     MONTHLY_CAPACITY_POINTER, &
                     AI_CAPACITY_RATE,AI_CAPACITY_ESCALATOR, &
                     AI_ENERGY_RATE,AI_ENERGY_ESCALATOR, &
                     ANNUAL_CL_FIXED_COST, &
                     ANNUAL_CL_FIXED_COST_ESC, &
                     MAINT_DAYS, &
                     DISPATCH_MULT, &
                     EXCESS_ENERGY_SALES, &
                     AI_REMAINING_LIFE, &
                     AI_TAX_LIFE, &
                     AI_ADR_TAX_LIFE, &
                     ASSET_CLASS_NUM, &
                     ASSET_CLASS_VECTOR, &
                     INTRA_COMPANY_CLASS_ID, &
                     INTRA_COMPANY_TRANSACTION, &
                     SPECIAL_UNIT_ID, &
                     MINIMUM_CAPACITY_FACTOR, &
                     MAXIMUM_CAPACITY_FACTOR, &
                     TRANSACTION_GROUP_ID, &
                     DAY_TYPE_ID, &
                     UNIT_ACTIVE, &
                     BASECASE_PLANT_ID, &
                     BASECASE_UNIT_ID, &
                     BASECASE_MARKET_AREA_ID, &
                     BASECASE_TRANS_AREA_ID, &
                     BASECASE_PLANT_NERC_SUB_ID, &
                     BASECASE_PLANT_OWNER_ID, &
                     UTILITY_OWNED, &
                     MONTHLY_FUEL_INDEX, &
                     START_UP_COSTS, &
                     START_UP_LOGIC, &
                     RAMP_RATE, &
                     MIN_DOWN_TIME, &
                     REPORT_THIS_UNIT, &
                     P_FuelDev_0, &
                     P_FuelDev_2, &
                     P_FuelDev_3, &
                     MIN_UP_TIME, &
                     HESI_UNIT_ID_NUM, &
                     FOR_FREQUENCY, &
                     FOR_DURATION, &
                     WINTER_TOTAL_CAPACITY, &
                     CONTRIBUTES_TO_SPIN, &
                     H2_UNIT_ID_NUM, &
                     POWERDAT_PLANT_ID, &
                     APPLY_NOX_SEASON_DATE, &
                     NOX_SEASON_DATE, &
                     RAMP_DOWN_RATE, &
                     NOX_CONTROL_PERCENT, &
                     NOX_CONTROL_DATE, &
                     EMERGENCY_CAPACITY, &
                     EMERGENCY_HEATRATE, &
                     MARKET_RESOURCE, &
                     PRIMARY_MOVER_STR, &
                     COUNTY, &
                     STATE_PROVINCE, &
                     NOX_VOM, &
                     NOX_FOM, &
                     S_FuelDev_0, &
                     S_FuelDev_2, &
                     S_FuelDev_3, &
                     CONSTANT_POLY, &
                     FIRST_POLY, &
                     SECOND_POLY, &
                     THIRD_POLY, &
                     USE_POLY_HEAT_RATES, &
                     NEWGEN_UNIT_STATUS, &
                     MONTHLY_MUST_RUN_VECTOR, &
                     EMISSION_MARKET_LINK, &
                     WVPA_RATE_TRACKER, &
                     ALLOW_DECOMMIT, &
                     MIN_SPIN_CAP, &
                     MAX_SPIN_CAP, &
                     WVPA_RES_TRACKER, &
                     WVPA_FUEL_TRACKER, &
                     MONTE_OR_FREQ, &
                     WVPA_MEM_TRACKER, &
                     MONTHLY_DECOMMIT_VECTOR, &
                     TRANS_BUS_ID, &
                     SOX_CONTROL_PERCENT, &
                     SOX_CONTROL_DATE, &
                     SOX_VOM, &
                     SOX_FOM, &
                     LATITUDE, &
                     LONGITUDE, &
                     MW_INSIDE_FENCE, &
                     INTER_BLOCKS, &
                     FIRST_YEAR_DECOMM_AVAIALABLE,     & !  152
                     DECOMMISSIONING_BASE_YR_COST,            & !  153
                     DECOMMISSIONING_COST_ESCALATION, &
                     ANNUAL_ENERGY_PLANNING_FACTOR, &
                     AGGREGATE_THIS_UNIT,              & !  156
                     NAME_PLATE_CAPACITY, &
                     FUEL_BLENDING_IS, &
                     FuelRatio_pn,         & !  159-163
                     BlendableFuelsPtr, & !  164-167
                     FuelTransportationCost, & !  168-171
                     BlendedEnthalpyUp, & !  172
                     BlendedEnthalpyLo, &
                     BlendedSO2Up,                  & !  174
                     BettermentProjectID, &
                     DECOM_CONTINUING_COST, &
                     DECOM_CONT_COST_ESCALATION, &
                     EnrgPatternPointer, & !  184
                     EMISSION_DATA_UNITS, &
                     EmissRedRate,       & !  180-184
                     EmissMaxRate, &
                     MaxEnergyLimit, &
                     tch_type, &
                     LINKED_BETTERMENT_OPTION, & !  194
                     CO2_CONTROL_PERCENT, &
                     HG_CONTROL_PERCENT, &
                     OTHER3_CONTROL_PERCENT, &
                     CO2_CONTROL_DATE, &
                     HG_CONTROL_DATE, &
                     OTHER3_CONTROL_DATE, &
                     CO2_VOM, &
                     CO2_FOM, &
                     HG_VOM, &
                     HG_FOM, &
                     OTHER3_VOM, &
                     OTHER3_FOM, &
                     MARKET_FLOOR, & !  207
                     MARKET_CEILING, & !  208
                     START_UP_COSTS_ESCALATION,  & !  209
                     CAPACITY_MARKET_TYPE, &
                     CAPACITY_MARKET_MONTH, &
                     capacity_market_pointer_pno, &
                     CAPACITY_MARKET_COST_ASSIGN, &
                     CAPACITY_MARKET_EXP_COLLECT, &
                     CAPACITY_MARKET_COIN_ADJ_FACT, &
                     PRIMARY_FUEL_CATEGORY, &
                     SECONDARY_FUEL_CATEGORY, & !
                     EMISSIONS_FUEL_CATEGORY, &
                     RPS_CONTRIBUTION_PERCENT, & !  219
                     RETIREMENT_CANDIDATE, &
                     RETROFIT_CANDIDATE, &
                     RETROFIT_PROJECT_ID, &
                     CO2_BASIN_NAME, &
                     CO2_PIPELINE_DISTANCE, &
                     STATE_PROV_NAME, &
                     CO2_RETRO_HEAT_MULT, &
                     CO2_RETRO_CAP_MULT, &
                     THERMAL_GUID, &
                     THERMAL_PARENT_ID, & !  229
                     THERMAL_AGGREGATED_UNIT, &
                     FIRST_RETIREMENT_YEAR, & !  231
                     UNIT_TYPE, &
                     UNIT_TYPE_CATEGORY, &
                     RPS_PROGRAM_NUMBER ! 234
               IF(RAMP_DOWN_RATE == -999999.) THEN
                  RAMP_DOWN_RATE = RAMP_RATE
               ENDIF
!               IF(EMISSION_MARKET_LINK == -9999) THEN
!                  EMISSION_MARKET_LINK = ASSET_CLASS_NUM
!               ENDIF
               IF(DISPADJ2 .EQ. -99999.) THEN
                  DISP_ADJ2_OUT = DISPADJ
               ELSE
                  DISP_ADJ2_OUT = DISPADJ2
               ENDIF
               IF(P_NOX_BK2 == -99999.) THEN
                  P_NOX_BK2_OUT = P_NOX_BK1
               ELSE
                  P_NOX_BK2_OUT = P_NOX_BK2
               ENDIF

               IF(PRIMARY_FUEL_CATEGORY=='C') THEN
                  AvailableGenericCoalUnits=AvailableGenericCoalUnits+1
               ENDIF
               ! MW_INSIDE_FENCE written here
               WRITE(11,REC=IREC) DELETE,UNIT_NAME,CL_LOAD_TYPE, &
                     EXPENSE_ASSIGNMENT,EXPENSE_COLLECTION, &
                     GENGRP,FRAOWN,ONLIMO,OFLIMO,FUELMX,PRFCST,PRESCR, &
                     SEFCST,SDESCR,FUELADJ,EFOR,MAINRT,VARCST,VRESCR, &
                     FIXED_COST,FIXED_COST_ESCALATOR, &
                     DISPADJ,HRFACT,MW,PLANNING_FACTOR,COEFF, &
                     AI_CAPACITY_RATE,AI_CAPACITY_ESCALATOR, &
                     AI_ENERGY_RATE,AI_ENERGY_ESCALATOR, &
                     P_SO2,P_NOX_BK1,P_CO2,POOL_FRAC_OWN, &
                     P_OTHER2,P_OTHER3,DISP_ADJ2_OUT, &
                     RESOURCE_ID,P_FUEL_SUPPLY_ID,P_NOX_BK2_OUT, &
                     SEC_EMISS_PTR,EMISS_FUEL_COST,EMISS_FUEL_ESCAL, &
                     EMISS_FUEL_EMISS_PTR,EMISS_BLENDING_RATE, &
                     PRIM_FUEL_EMISS_PTR, &
                     PRIM_FUEL_TYPES,FUEL_TYPES,TIE_GROUP, &
                     MONTHLY_CAPACITY_POINTER, &
                     ANNUAL_CL_FIXED_COST, &
                     ANNUAL_CL_FIXED_COST_ESC, &
                     MAINT_DAYS, &
                     DISPATCH_MULT, &
                     EXCESS_ENERGY_SALES, &
                     AI_REMAINING_LIFE, &
                     AI_TAX_LIFE, &
                     AI_ADR_TAX_LIFE, &
                     ASSET_CLASS_NUM, &
                     ASSET_CLASS_VECTOR, &
                     INTRA_COMPANY_CLASS_ID, &
                     INTRA_COMPANY_TRANSACTION, &
                     SPECIAL_UNIT_ID, &
                     MINIMUM_CAPACITY_FACTOR, &
                     MAXIMUM_CAPACITY_FACTOR, &
                     TRANSACTION_GROUP_ID, &
                     DAY_TYPE_ID, &
                     UNIT_ACTIVE, &
                     BASECASE_PLANT_ID, &
                     BASECASE_UNIT_ID, &
                     BASECASE_MARKET_AREA_ID, &
                     BASECASE_TRANS_AREA_ID, &
                     BASECASE_PLANT_NERC_SUB_ID, &
                     BASECASE_PLANT_OWNER_ID, &
                     UTILITY_OWNED, &
                     MONTHLY_FUEL_INDEX, &
                     START_UP_COSTS, &
                     START_UP_LOGIC, &
                     RAMP_RATE, &
                     MIN_DOWN_TIME, &
                     REPORT_THIS_UNIT, &
                     P_FuelDev_0, &
                     P_FuelDev_2, &
                     P_FuelDev_3, &
                     MIN_UP_TIME, &
                     HESI_UNIT_ID_NUM, &
                     FOR_FREQUENCY, &
                     FOR_DURATION, &
                     WINTER_TOTAL_CAPACITY, &
                     CONTRIBUTES_TO_SPIN, &
                     H2_UNIT_ID_NUM, &
                     POWERDAT_PLANT_ID, &
                     APPLY_NOX_SEASON_DATE, &
                     NOX_SEASON_DATE, &
                     RAMP_DOWN_RATE, &
                     NOX_CONTROL_PERCENT, &
                     NOX_CONTROL_DATE, &
                     EMERGENCY_CAPACITY, &
                     EMERGENCY_HEATRATE, &
                     MARKET_RESOURCE, &
                     PRIMARY_MOVER_STR, &
                     COUNTY, &
                     STATE_PROVINCE, &
                     NOX_VOM, &
                     NOX_FOM, &
                     S_FuelDev_0, &
                     S_FuelDev_2, &
                     S_FuelDev_3, &
                     CONSTANT_POLY, &
                     FIRST_POLY, &
                     SECOND_POLY, &
                     THIRD_POLY, &
                     USE_POLY_HEAT_RATES, &
                     NEWGEN_UNIT_STATUS, &
                     MONTHLY_MUST_RUN_VECTOR, &
                     EMISSION_MARKET_LINK, &
                     WVPA_RATE_TRACKER, &
                     ALLOW_DECOMMIT, &
                     MIN_SPIN_CAP, &
                     MAX_SPIN_CAP, &
                     WVPA_RES_TRACKER, &
                     WVPA_FUEL_TRACKER, &
                     MONTE_OR_FREQ, &
                     WVPA_MEM_TRACKER, &
                     MONTHLY_DECOMMIT_VECTOR, &
                     TRANS_BUS_ID, &
                     SOX_CONTROL_PERCENT, &
                     SOX_CONTROL_DATE, &
                     SOX_VOM, &
                     SOX_FOM, &
                     LATITUDE, &
                     LONGITUDE, &
                     MW_INSIDE_FENCE, &
                     INTER_BLOCKS, &
                     FIRST_YEAR_DECOMM_AVAIALABLE,     & !  152
                     DECOMMISSIONING_BASE_YR_COST,            & !  153
                     DECOMMISSIONING_COST_ESCALATION, &
                     ANNUAL_ENERGY_PLANNING_FACTOR, &
                     AGGREGATE_THIS_UNIT,              & !  156
                     NAME_PLATE_CAPACITY, &
                     FUEL_BLENDING_IS, &
                     FuelRatio_pn,         & !  159-163
                     BlendableFuelsPtr, & !  164-167
                     FuelTransportationCost, & !  168-171
                     BlendedEnthalpyUp, & !  172
                     BlendedEnthalpyLo, &
                     BlendedSO2Up,                  & !  174
                     BettermentProjectID, &
                     DECOM_CONTINUING_COST, &
                     DECOM_CONT_COST_ESCALATION, &
                     EnrgPatternPointer, & !  184
                     EMISSION_DATA_UNITS, &
                     EmissRedRate,       & !  180-184
                     EmissMaxRate, &
                     MaxEnergyLimit, &
                     tch_type, &
                     LINKED_BETTERMENT_OPTION, & !  194
                     CO2_CONTROL_PERCENT, &
                     HG_CONTROL_PERCENT, &
                     OTHER3_CONTROL_PERCENT, &
                     CO2_CONTROL_DATE, &
                     HG_CONTROL_DATE, &
                     OTHER3_CONTROL_DATE, &
                     CO2_VOM, &
                     CO2_FOM, &
                     HG_VOM, &
                     HG_FOM, &
                     OTHER3_VOM, &
                     OTHER3_FOM, &
                     MARKET_FLOOR,             & !  207
                     MARKET_CEILING, & !  208
                     START_UP_COSTS_ESCALATION,  & !  209
                     CAPACITY_MARKET_TYPE, &
                     CAPACITY_MARKET_MONTH, &
                     capacity_market_pointer_pno, &
                     CAPACITY_MARKET_COST_ASSIGN, &
                     CAPACITY_MARKET_EXP_COLLECT, &
                     CAPACITY_MARKET_COIN_ADJ_FACT, &
                     PRIMARY_FUEL_CATEGORY, &
                     SECONDARY_FUEL_CATEGORY, & !  220
                     EMISSIONS_FUEL_CATEGORY, &
                     RPS_CONTRIBUTION_PERCENT, &
                     RETIREMENT_CANDIDATE, &
                     RETROFIT_CANDIDATE, &
                     RETROFIT_PROJECT_ID, &
                     CO2_BASIN_NAME, &
                     CO2_PIPELINE_DISTANCE, &
                     STATE_PROV_NAME, &
                     CO2_RETRO_HEAT_MULT, &
                     CO2_RETRO_CAP_MULT, &
                     THERMAL_GUID, &
                     THERMAL_PARENT_ID, & !  232
                     THERMAL_AGGREGATED_UNIT, &
                     FIRST_RETIREMENT_YEAR, &
                     UNIT_TYPE, &
                     UNIT_TYPE_CATEGORY, &
                     RPS_PROGRAM_NUMBER
               IREC = IREC + 1
            ENDDO
            IF(IOS /= 0) EXIT
         ENDDO
         CLOSE(10)

         CLOSE(11)
         BASE_PN_RECORDS = IREC - 1
!         WRITE(4,*) "BASE EXP PLANT REC = ",BASE_PN_RECORDS
      ELSE IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "pn_objt:1111")
      ENDIF
      RETURN
!

!
!           ROUTINE TO CREATE OVERLAY FILES
!           COPYRIGHT (C) 1984-88  M.S. GERBER & ASSOCIATES, INC.
!           COPYRIGHT (C) 1991-92  M.S. GERBER & ASSOCIATES, INC.
!

!
!  OVERLAY THE EXPANSION-PLANT FILE

      ENTRY PN_MAKEOVL(OVERLAY_FAMILY_NAME)

      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      FILE_NAME=trim(OUTPUT_DIRECTORY())//"PNO"// &
                                     trim(OVERLAY_FAMILY_NAME)//".DAT"
      OPEN(10,FILE=FILE_NAME)
      READ(10,*) DELETE
      INUNIT = 12
      IF(NEWPLT_PROD_OL == 'BC') THEN
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCPNWCP.BIN", &
                                             ACCESS="DIRECT",RECL=LRECL)
         INUNIT = 11
      ENDIF
      OPEN(12,FILE=trim(OUTPUT_DIRECTORY())//"OLPNWCP.BIN", &
                            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      IREC = 0
      DO
         DO
            READ(10,'(A)',IOSTAT=IOS) RECLN
            IF(RECLN(1:1) == '7') EXIT
            IREC = IREC + 1
            READ(INUNIT,REC=IREC,IOSTAT=IOS_BASE) DELETE,UNIT_NAME, &
               CL_LOAD_TYPE,EXPENSE_ASSIGNMENT,EXPENSE_COLLECTION, &
               GENGRP,FRAOWN,ONLIMO,OFLIMO,FUELMX,PRFCST, &
               PRESCR,SEFCST,SDESCR,FUELADJ,EFOR,MAINRT,VARCST,VRESCR, &
               FIXED_COST,FIXED_COST_ESCALATOR, &
               DISPADJ,HRFACT,MW,PLANNING_FACTOR,COEFF, &
               AI_CAPACITY_RATE,AI_CAPACITY_ESCALATOR, &
               AI_ENERGY_RATE,AI_ENERGY_ESCALATOR, &
               P_SO2,P_NOX,P_PARTICULATES,POOL_FRAC_OWN, &
               P_OTHER2,P_OTHER3,DISPADJ2, &
               RESOURCE_ID,FUEL_SUPPLY_ID, &
               P_NOX_BK2,SEC_EMISS_PTR,EMISS_FUEL_COST,EMISS_FUEL_ESCAL, &
               EMISS_FUEL_EMISS_PTR,EMISS_BLENDING_RATE, &
               PRIM_FUEL_EMISS_PTR, &
               PRIM_FUEL_TYPES,FUEL_TYPES,TIE_GROUP, &
               MONTHLY_CAPACITY_POINTER, &
               ANNUAL_CL_FIXED_COST, &
               ANNUAL_CL_FIXED_COST_ESC, &
               MAINT_DAYS, &
               DISPATCH_MULT, &
               EXCESS_ENERGY_SALES, &
               AI_REMAINING_LIFE, &
               AI_TAX_LIFE, &
               AI_ADR_TAX_LIFE, &
               ASSET_CLASS_NUM, &
               ASSET_CLASS_VECTOR, &
               INTRA_COMPANY_CLASS_ID, &
               INTRA_COMPANY_TRANSACTION, &
               SPECIAL_UNIT_ID, &
               MINIMUM_CAPACITY_FACTOR, &
               MAXIMUM_CAPACITY_FACTOR, &
               TRANSACTION_GROUP_ID, &
               DAY_TYPE_ID, &
               UNIT_ACTIVE, &
               BASECASE_PLANT_ID, &
               BASECASE_UNIT_ID, &
               BASECASE_MARKET_AREA_ID, &
               BASECASE_TRANS_AREA_ID, &
               BASECASE_PLANT_NERC_SUB_ID, &
               BASECASE_PLANT_OWNER_ID, &
               UTILITY_OWNED, &
               MONTHLY_FUEL_INDEX, &
               START_UP_COSTS, &
               START_UP_LOGIC, &
               RAMP_RATE, &
               MIN_DOWN_TIME, &
               REPORT_THIS_UNIT, &
               P_FuelDev_0, &
               P_FuelDev_2, &
               P_FuelDev_3, &
               MIN_UP_TIME, &
               HESI_UNIT_ID_NUM, &
               FOR_FREQUENCY, &
               FOR_DURATION, &
               WINTER_TOTAL_CAPACITY, &
               CONTRIBUTES_TO_SPIN, &
               H2_UNIT_ID_NUM, &
               POWERDAT_PLANT_ID, &
               APPLY_NOX_SEASON_DATE, &
               NOX_SEASON_DATE, &
               RAMP_DOWN_RATE, &
               NOX_CONTROL_PERCENT, &
               NOX_CONTROL_DATE, &
               EMERGENCY_CAPACITY, &
               EMERGENCY_HEATRATE, &
               MARKET_RESOURCE, &
               PRIMARY_MOVER_STR, &
               COUNTY, &
               STATE_PROVINCE, &
               NOX_VOM, &
               NOX_FOM, &
               S_FuelDev_0, &
               S_FuelDev_2, &
               S_FuelDev_3, &
               CONSTANT_POLY, &
               FIRST_POLY, &
               SECOND_POLY, &
               THIRD_POLY, &
               USE_POLY_HEAT_RATES, &
               NEWGEN_UNIT_STATUS, &
               MONTHLY_MUST_RUN_VECTOR, &
               EMISSION_MARKET_LINK, &
               WVPA_RATE_TRACKER, &
               ALLOW_DECOMMIT, &
               MIN_SPIN_CAP, &
               MAX_SPIN_CAP, &
               WVPA_RES_TRACKER, &
               WVPA_FUEL_TRACKER, &
               MONTE_OR_FREQ, &
               WVPA_MEM_TRACKER, &
               MONTHLY_DECOMMIT_VECTOR, &
               TRANS_BUS_ID, &
               SOX_CONTROL_PERCENT, &
               SOX_CONTROL_DATE, &
               SOX_VOM, &
               SOX_FOM, &
               LATITUDE, &
               LONGITUDE, &
               MW_INSIDE_FENCE, &
                     INTER_BLOCKS, &
                     FIRST_YEAR_DECOMM_AVAIALABLE,     & !  152
                     DECOMMISSIONING_BASE_YR_COST,            & !  153
                     DECOMMISSIONING_COST_ESCALATION, &
                     ANNUAL_ENERGY_PLANNING_FACTOR, &
                     AGGREGATE_THIS_UNIT,              & !  156
                     NAME_PLATE_CAPACITY, &
                     FUEL_BLENDING_IS, &
                     FuelRatio_pn,         & !  159-163
                     BlendableFuelsPtr, & !  164-167
                     FuelTransportationCost, & !  168-171
                     BlendedEnthalpyUp, & !  172
                     BlendedEnthalpyLo, &
                     BlendedSO2Up,                  & !  174
                     BettermentProjectID, &
                     DECOM_CONTINUING_COST, &
                     DECOM_CONT_COST_ESCALATION, &
                     EnrgPatternPointer, & !  184
                     EMISSION_DATA_UNITS, &
                     EmissRedRate,       & !  180-184
                     EmissMaxRate, &
                     MaxEnergyLimit, &
                     tch_type, &
                     LINKED_BETTERMENT_OPTION, & !  194
                     CO2_CONTROL_PERCENT, &
                     HG_CONTROL_PERCENT, &
                     OTHER3_CONTROL_PERCENT, &
                     CO2_CONTROL_DATE, &
                     HG_CONTROL_DATE, &
                     OTHER3_CONTROL_DATE, &
                     CO2_VOM, &
                     CO2_FOM, &
                     HG_VOM, &
                     HG_FOM, &
                     OTHER3_VOM, &
                     OTHER3_FOM, &
                     MARKET_FLOOR,             & !  207
                     MARKET_CEILING, & !  208
                     START_UP_COSTS_ESCALATION,  & !  209
                     CAPACITY_MARKET_TYPE, &
                     CAPACITY_MARKET_MONTH, &
                     capacity_market_pointer_pno, &
                     CAPACITY_MARKET_COST_ASSIGN, &
                     CAPACITY_MARKET_EXP_COLLECT, &
                     CAPACITY_MARKET_COIN_ADJ_FACT, &
                     PRIMARY_FUEL_CATEGORY, &
                     SECONDARY_FUEL_CATEGORY, & !  220
                     EMISSIONS_FUEL_CATEGORY, &
                     RPS_CONTRIBUTION_PERCENT, &
                     RETIREMENT_CANDIDATE, &
                     RETROFIT_CANDIDATE, &
                     RETROFIT_PROJECT_ID, &
                     CO2_BASIN_NAME, &
                     CO2_PIPELINE_DISTANCE, &
                     STATE_PROV_NAME, &
                     CO2_RETRO_HEAT_MULT, &
                     CO2_RETRO_CAP_MULT, &
                     THERMAL_GUID, &
                     THERMAL_PARENT_ID, & !  232
                     THERMAL_AGGREGATED_UNIT, &
                     FIRST_RETIREMENT_YEAR, &
                     UNIT_TYPE, &
                     UNIT_TYPE_CATEGORY, &
                     RPS_PROGRAM_NUMBER
            IF(IOS_BASE /= 0) EXIT
            IF(IOS == 0) THEN
               RECLN = trim(RECLN)// &
                     ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                     ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                     ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=200) DELETE,UNIT_NAME,CL_LOAD_TYPE, &
                     EXPENSE_ASSIGNMENT,EXPENSE_COLLECTION, &
                     GENGRP,FRAOWN,ONLIMO,OFLIMO,FUELMX,PRFCST, &
                     PRESCR,SEFCST,SDESCR,FUELADJ,EFOR,MAINRT,VARCST, &
                     VRESCR,FIXED_COST,FIXED_COST_ESCALATOR, &
                     DISPADJ,HRFACT,MW,PLANNING_FACTOR,COEFF,DESC, &
                     P_SO2,P_NOX,P_PARTICULATES,POOL_FRAC_OWN, &
                     P_OTHER2,P_OTHER3,DISPADJ2, &
                     RESOURCE_ID,FUEL_SUPPLY_ID, &
                     P_NOX_BK2,SEC_EMISS_PTR,EMISS_FUEL_COST, &
                     EMISS_FUEL_ESCAL,EMISS_FUEL_EMISS_PTR, &
                     EMISS_BLENDING_RATE,PRIM_FUEL_EMISS_PTR, &
                     PRIM_FUEL_TYPES,FUEL_TYPES,TIE_GROUP, &
                     MONTHLY_CAPACITY_POINTER, &
                     AI_CAPACITY_RATE,AI_CAPACITY_ESCALATOR, &
                     AI_ENERGY_RATE,AI_ENERGY_ESCALATOR, &
                     ANNUAL_CL_FIXED_COST, &
                     ANNUAL_CL_FIXED_COST_ESC, &
                     MAINT_DAYS, &
                     DISPATCH_MULT, &
                     EXCESS_ENERGY_SALES, &
                     AI_REMAINING_LIFE, &
                     AI_TAX_LIFE, &
                     AI_ADR_TAX_LIFE, &
                     ASSET_CLASS_NUM, &
                     ASSET_CLASS_VECTOR, &
                     INTRA_COMPANY_CLASS_ID, &
                     INTRA_COMPANY_TRANSACTION, &
                     SPECIAL_UNIT_ID, &
                     MINIMUM_CAPACITY_FACTOR, &
                     MAXIMUM_CAPACITY_FACTOR, &
                     TRANSACTION_GROUP_ID, &
                     DAY_TYPE_ID, &
                     UNIT_ACTIVE, &
                     BASECASE_PLANT_ID, &
                     BASECASE_UNIT_ID, &
                     BASECASE_MARKET_AREA_ID, &
                     BASECASE_TRANS_AREA_ID, &
                     BASECASE_PLANT_NERC_SUB_ID, &
                     BASECASE_PLANT_OWNER_ID, &
                     UTILITY_OWNED, &
                     MONTHLY_FUEL_INDEX, &
                     START_UP_COSTS, &
                     START_UP_LOGIC, &
                     RAMP_RATE, &
                     MIN_DOWN_TIME, &
                     REPORT_THIS_UNIT, &
                     P_FuelDev_0, &
                     P_FuelDev_2, &
                     P_FuelDev_3, &
                     MIN_UP_TIME, &
                     HESI_UNIT_ID_NUM, &
                     FOR_FREQUENCY, &
                     FOR_DURATION, &
                     WINTER_TOTAL_CAPACITY, &
                     CONTRIBUTES_TO_SPIN, &
                     H2_UNIT_ID_NUM, &
                     POWERDAT_PLANT_ID, &
                     APPLY_NOX_SEASON_DATE, &
                     NOX_SEASON_DATE, &
                     RAMP_DOWN_RATE, &
                     NOX_CONTROL_PERCENT, &
                     NOX_CONTROL_DATE, &
                     EMERGENCY_CAPACITY, &
                     EMERGENCY_HEATRATE, &
                     MARKET_RESOURCE, &
                     PRIMARY_MOVER_STR, &
                     COUNTY, &
                     STATE_PROVINCE, &
                     NOX_VOM, &
                     NOX_FOM, &
                     S_FuelDev_0, &
                     S_FuelDev_2, &
                     S_FuelDev_3, &
                     CONSTANT_POLY, &
                     FIRST_POLY, &
                     SECOND_POLY, &
                     THIRD_POLY, &
                     USE_POLY_HEAT_RATES, &
                     NEWGEN_UNIT_STATUS, &
                     MONTHLY_MUST_RUN_VECTOR, &
                     EMISSION_MARKET_LINK, &
                     WVPA_RATE_TRACKER, &
                     ALLOW_DECOMMIT, &
                     MIN_SPIN_CAP, &
                     MAX_SPIN_CAP, &
                     WVPA_RES_TRACKER, &
                     WVPA_FUEL_TRACKER, &
                     MONTE_OR_FREQ, &
                     WVPA_MEM_TRACKER, &
                     MONTHLY_DECOMMIT_VECTOR, &
                     TRANS_BUS_ID, &
                     SOX_CONTROL_PERCENT, &
                     SOX_CONTROL_DATE, &
                     SOX_VOM, &
                     SOX_FOM, &
                     LATITUDE, &
                     LONGITUDE, &
                     MW_INSIDE_FENCE, &
                     INTER_BLOCKS, &
                     FIRST_YEAR_DECOMM_AVAIALABLE,     & !  152
                     DECOMMISSIONING_BASE_YR_COST,            & !  153
                     DECOMMISSIONING_COST_ESCALATION, &
                     ANNUAL_ENERGY_PLANNING_FACTOR, &
                     AGGREGATE_THIS_UNIT,              & !  156
                     NAME_PLATE_CAPACITY, &
                     FUEL_BLENDING_IS, &
                     FuelRatio_pn,         & !  159-163
                     BlendableFuelsPtr, & !  164-167
                     FuelTransportationCost, & !  168-171
                     BlendedEnthalpyUp, & !  172
                     BlendedEnthalpyLo, &
                     BlendedSO2Up,                  & !  174
                     BettermentProjectID, &
                     DECOM_CONTINUING_COST, &
                     DECOM_CONT_COST_ESCALATION, &
                     EnrgPatternPointer, & !  184
                     EMISSION_DATA_UNITS, &
                     EmissRedRate,       & !  180-184
                     EmissMaxRate, &
                     MaxEnergyLimit, &
                     tch_type, &
                     LINKED_BETTERMENT_OPTION, & !  194
                     CO2_CONTROL_PERCENT, &
                     HG_CONTROL_PERCENT, &
                     OTHER3_CONTROL_PERCENT, &
                     CO2_CONTROL_DATE, &
                     HG_CONTROL_DATE, &
                     OTHER3_CONTROL_DATE, &
                     CO2_VOM, &
                     CO2_FOM, &
                     HG_VOM, &
                     HG_FOM, &
                     OTHER3_VOM, &
                     OTHER3_FOM, &
                     MARKET_FLOOR,             & !  207
                     MARKET_CEILING, & !  208
                     START_UP_COSTS_ESCALATION,  & !  209
                     CAPACITY_MARKET_TYPE, &
                     CAPACITY_MARKET_MONTH, &
                     capacity_market_pointer_pno, &
                     CAPACITY_MARKET_COST_ASSIGN, &
                     CAPACITY_MARKET_EXP_COLLECT, &
                     CAPACITY_MARKET_COIN_ADJ_FACT, &
                     PRIMARY_FUEL_CATEGORY, &
                     SECONDARY_FUEL_CATEGORY, & !  220
                     EMISSIONS_FUEL_CATEGORY, &
                     RPS_CONTRIBUTION_PERCENT, &
                     RETIREMENT_CANDIDATE, &
                     RETROFIT_CANDIDATE, &
                     RETROFIT_PROJECT_ID, &
                     CO2_BASIN_NAME, &
                     CO2_PIPELINE_DISTANCE, &
                     STATE_PROV_NAME, &
                     CO2_RETRO_HEAT_MULT, &
                     CO2_RETRO_CAP_MULT, &
                     THERMAL_GUID, &
                     THERMAL_PARENT_ID, & !  232
                     THERMAL_AGGREGATED_UNIT, &
                     FIRST_RETIREMENT_YEAR, &
                     UNIT_TYPE, &
                     UNIT_TYPE_CATEGORY, &
                     RPS_PROGRAM_NUMBER
            ENDIF
            WRITE(12,REC=IREC) DELETE,UNIT_NAME,CL_LOAD_TYPE, &
               EXPENSE_ASSIGNMENT,EXPENSE_COLLECTION, &
               GENGRP,FRAOWN,ONLIMO,OFLIMO,FUELMX,PRFCST, &
               PRESCR,SEFCST,SDESCR,FUELADJ,EFOR,MAINRT,VARCST,VRESCR, &
               FIXED_COST,FIXED_COST_ESCALATOR, &
               DISPADJ,HRFACT,MW,PLANNING_FACTOR,COEFF, &
               AI_CAPACITY_RATE,AI_CAPACITY_ESCALATOR, &
               AI_ENERGY_RATE,AI_ENERGY_ESCALATOR, &
               P_SO2,P_NOX,P_PARTICULATES,POOL_FRAC_OWN, &
               P_OTHER2,P_OTHER3,DISPADJ2, &
               RESOURCE_ID,FUEL_SUPPLY_ID, &
               P_NOX_BK2,SEC_EMISS_PTR,EMISS_FUEL_COST,EMISS_FUEL_ESCAL, &
               EMISS_FUEL_EMISS_PTR,EMISS_BLENDING_RATE, &
               PRIM_FUEL_EMISS_PTR, &
               PRIM_FUEL_TYPES,FUEL_TYPES,TIE_GROUP, &
               MONTHLY_CAPACITY_POINTER, &
               ANNUAL_CL_FIXED_COST, &
               ANNUAL_CL_FIXED_COST_ESC, &
               MAINT_DAYS, &
               DISPATCH_MULT, &
               EXCESS_ENERGY_SALES, &
               AI_REMAINING_LIFE, &
               AI_TAX_LIFE, &
               AI_ADR_TAX_LIFE, &
               ASSET_CLASS_NUM, &
               ASSET_CLASS_VECTOR, &
               INTRA_COMPANY_CLASS_ID, &
               INTRA_COMPANY_TRANSACTION, &
               SPECIAL_UNIT_ID, &
               MINIMUM_CAPACITY_FACTOR, &
               MAXIMUM_CAPACITY_FACTOR, &
               TRANSACTION_GROUP_ID, &
               DAY_TYPE_ID, &
               UNIT_ACTIVE, &
               BASECASE_PLANT_ID, &
               BASECASE_UNIT_ID, &
               BASECASE_MARKET_AREA_ID, &
               BASECASE_TRANS_AREA_ID, &
               BASECASE_PLANT_NERC_SUB_ID, &
               BASECASE_PLANT_OWNER_ID, &
               UTILITY_OWNED, &
               MONTHLY_FUEL_INDEX, &
               START_UP_COSTS, &
               START_UP_LOGIC, &
               RAMP_RATE, &
               MIN_DOWN_TIME, &
               REPORT_THIS_UNIT, &
               P_FuelDev_0, &
               P_FuelDev_2, &
               P_FuelDev_3, &
               MIN_UP_TIME, &
               HESI_UNIT_ID_NUM, &
               FOR_FREQUENCY, &
               FOR_DURATION, &
               WINTER_TOTAL_CAPACITY, &
               CONTRIBUTES_TO_SPIN, &
               H2_UNIT_ID_NUM, &
               POWERDAT_PLANT_ID, &
               APPLY_NOX_SEASON_DATE, &
               NOX_SEASON_DATE, &
               RAMP_DOWN_RATE, &
               NOX_CONTROL_PERCENT, &
               NOX_CONTROL_DATE, &
               EMERGENCY_CAPACITY, &
               EMERGENCY_HEATRATE, &
               MARKET_RESOURCE, &
               PRIMARY_MOVER_STR, &
               COUNTY, &
               STATE_PROVINCE, &
               NOX_VOM, &
               NOX_FOM, &
               S_FuelDev_0, &
               S_FuelDev_2, &
               S_FuelDev_3, &
               CONSTANT_POLY, &
               FIRST_POLY, &
               SECOND_POLY, &
               THIRD_POLY, &
               USE_POLY_HEAT_RATES, &
               NEWGEN_UNIT_STATUS, &
               MONTHLY_MUST_RUN_VECTOR, &
               EMISSION_MARKET_LINK, &
               WVPA_RATE_TRACKER, &
               ALLOW_DECOMMIT, &
               MIN_SPIN_CAP, &
               MAX_SPIN_CAP, &
               WVPA_RES_TRACKER, &
               WVPA_FUEL_TRACKER, &
               MONTE_OR_FREQ, &
               WVPA_MEM_TRACKER, &
               MONTHLY_DECOMMIT_VECTOR, &
               TRANS_BUS_ID, &
               SOX_CONTROL_PERCENT, &
               SOX_CONTROL_DATE, &
               SOX_VOM, &
               SOX_FOM, &
               LATITUDE, &
               LONGITUDE, &
               MW_INSIDE_FENCE, &
                     INTER_BLOCKS, &
                     FIRST_YEAR_DECOMM_AVAIALABLE,     & !  152
                     DECOMMISSIONING_BASE_YR_COST,            & !  153
                     DECOMMISSIONING_COST_ESCALATION, &
                     ANNUAL_ENERGY_PLANNING_FACTOR, &
                     AGGREGATE_THIS_UNIT,              & !  156
                     NAME_PLATE_CAPACITY, &
                     FUEL_BLENDING_IS, &
                     FuelRatio_pn,         & !  159-163
                     BlendableFuelsPtr, & !  164-167
                     FuelTransportationCost, & !  168-171
                     BlendedEnthalpyUp, & !  172
                     BlendedEnthalpyLo, &
                     BlendedSO2Up,                  & !  174
                     BettermentProjectID, &
                     DECOM_CONTINUING_COST, &
                     DECOM_CONT_COST_ESCALATION, &
                     EnrgPatternPointer, & !  184
                     EMISSION_DATA_UNITS, &
                     EmissRedRate,       & !  180-184
                     EmissMaxRate, &
                     MaxEnergyLimit, &
                     tch_type, &
                     LINKED_BETTERMENT_OPTION, & !  194
                     CO2_CONTROL_PERCENT, &
                     HG_CONTROL_PERCENT, &
                     OTHER3_CONTROL_PERCENT, &
                     CO2_CONTROL_DATE, &
                     HG_CONTROL_DATE, &
                     OTHER3_CONTROL_DATE, &
                     CO2_VOM, &
                     CO2_FOM, &
                     HG_VOM, &
                     HG_FOM, &
                     OTHER3_VOM, &
                     OTHER3_FOM, &
                     MARKET_FLOOR,             & !  207
                     MARKET_CEILING, & !  208
                     START_UP_COSTS_ESCALATION,  & !  209
                     CAPACITY_MARKET_TYPE, &
                     CAPACITY_MARKET_MONTH, &
                     capacity_market_pointer_pno, &
                     CAPACITY_MARKET_COST_ASSIGN, &
                     CAPACITY_MARKET_EXP_COLLECT, &
                     CAPACITY_MARKET_COIN_ADJ_FACT, &
                     PRIMARY_FUEL_CATEGORY, &
                     SECONDARY_FUEL_CATEGORY, & !  220
                     EMISSIONS_FUEL_CATEGORY, &
                     RPS_CONTRIBUTION_PERCENT, &
                     RETIREMENT_CANDIDATE, &
                     RETROFIT_CANDIDATE, &
                     RETROFIT_PROJECT_ID, &
                     CO2_BASIN_NAME, &
                     CO2_PIPELINE_DISTANCE, &
                     STATE_PROV_NAME, &
                     CO2_RETRO_HEAT_MULT, &
                     CO2_RETRO_CAP_MULT, &
                     THERMAL_GUID, &
                     THERMAL_PARENT_ID, & !  232
                     THERMAL_AGGREGATED_UNIT, &
                     FIRST_RETIREMENT_YEAR, &
                     UNIT_TYPE, &
                     UNIT_TYPE_CATEGORY, &
                     RPS_PROGRAM_NUMBER
         ENDDO
         IF(IOS_BASE /= 0) EXIT
      ENDDO
      CLOSE(10)
      CLOSE(12)
      IF(NEWPLT_PROD_OL == 'BC') CLOSE(11)
      NEWPLT_PROD_OL = 'OL'
      RETURN
!

  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from Pn_objt SIID254'
      call end_program(er_message)
!

      ENTRY RESET_NEWPLT_PROD_OL

         NEWPLT_PROD_OL = 'BC'
      RETURN
!

      ENTRY GET_BASE_PN_RECORDS(R_BASE_PN_RECORDS)

         R_BASE_PN_RECORDS = BASE_PN_RECORDS
      RETURN

      ENTRY OPEN_NEWPLT_FILE(UNIT_NUM)

         INQUIRE(UNIT=UNIT_NUM,OPENED=NEW_CL_UNIT_OPENED)
         IF(.NOT. NEW_CL_UNIT_OPENED) THEN
            OPEN(UNIT_NUM,FILE=trim(OUTPUT_DIRECTORY())// &
               NEWPLT_PROD_OL// &
                "PNWCP.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
            LAST_UNIT_NUM_OPENED = UNIT_NUM
         ELSE
            WRITE(4,*) "READ ERROR IN EXPANSION OPERATIONS FILE"
         ENDIF
      RETURN
!

      ENTRY CLOSE_NEWPLT_FILE

         CLOSE(LAST_UNIT_NUM_OPENED)
      RETURN

      ENTRY CL_CAPACITY_CHECK(R_UNITS_2_CHECK,R_UNIT_NAME, &
                              R_DATA_POINTER,R_UNIT_CAP, &
                              R_RESOURCE_TYPE)

!  2/23/95. GAT. OUT BECAUSE CLOSED TWICE
!          CALL OPEN_NEWPLT_FILE(10)
!
         DO I = 1, R_UNITS_2_CHECK
            IF(R_RESOURCE_TYPE(I) /= 'CL') then
            	CYCLE
            end if
            IREC = R_DATA_POINTER(I)
            IF(IREC <= 0) THEN
               WRITE(4,*) trim(R_UNIT_NAME(I))// &
                    ', a capacity-limited expansion option, does not '// &
                    'have a valid production pointer.'
               CYCLE
            ENDIF
            READ(LAST_UNIT_NUM_OPENED,REC=IREC,IOSTAT=IOS) DELETE, &
               UNIT_NAME, &
               CL_LOAD_TYPE,EXPENSE_ASSIGNMENT,EXPENSE_COLLECTION, &
               GENGRP,FRAOWN,ONLIMO,OFLIMO,FUELMX,PRFCST, &
               PRESCR,SEFCST,SDESCR,FUELADJ,EFOR,MAINRT,VARCST,VRESCR, &
               FIXED_COST,FIXED_COST_ESCALATOR, &
               DISPADJ,HRFACT,MW,PLANNING_FACTOR,COEFF, &
               AI_CAPACITY_RATE,AI_CAPACITY_ESCALATOR, &
               AI_ENERGY_RATE,AI_ENERGY_ESCALATOR, &
               P_SO2,P_NOX,P_PARTICULATES,POOL_FRAC_OWN, &
               P_OTHER2,P_OTHER3,DISPADJ2, &
               RESOURCE_ID,FUEL_SUPPLY_ID, &
               P_NOX_BK2,SEC_EMISS_PTR,EMISS_FUEL_COST,EMISS_FUEL_ESCAL, &
               EMISS_FUEL_EMISS_PTR,EMISS_BLENDING_RATE, &
               PRIM_FUEL_EMISS_PTR, &
               PRIM_FUEL_TYPES,FUEL_TYPES,TIE_GROUP, &
               MONTHLY_CAPACITY_POINTER, &
               ANNUAL_CL_FIXED_COST, &
               ANNUAL_CL_FIXED_COST_ESC, &
               MAINT_DAYS, &
               DISPATCH_MULT, &
               EXCESS_ENERGY_SALES, &
               AI_REMAINING_LIFE, &
               AI_TAX_LIFE, &
               AI_ADR_TAX_LIFE, &
               ASSET_CLASS_NUM, &
               ASSET_CLASS_VECTOR, &
               INTRA_COMPANY_CLASS_ID, &
               INTRA_COMPANY_TRANSACTION, &
               SPECIAL_UNIT_ID, &
               MINIMUM_CAPACITY_FACTOR, &
               MAXIMUM_CAPACITY_FACTOR, &
               TRANSACTION_GROUP_ID, &
               DAY_TYPE_ID, &
               UNIT_ACTIVE, &
               BASECASE_PLANT_ID, &
               BASECASE_UNIT_ID, &
               BASECASE_MARKET_AREA_ID, &
               BASECASE_TRANS_AREA_ID, &
               BASECASE_PLANT_NERC_SUB_ID, &
               BASECASE_PLANT_OWNER_ID, &
               UTILITY_OWNED, &
               MONTHLY_FUEL_INDEX, &
               START_UP_COSTS, &
               START_UP_LOGIC, &
               RAMP_RATE, &
               MIN_DOWN_TIME, &
               REPORT_THIS_UNIT, &
               P_FuelDev_0, &
               P_FuelDev_2, &
               P_FuelDev_3, &
               MIN_UP_TIME, &
               HESI_UNIT_ID_NUM, &
               FOR_FREQUENCY, &
               FOR_DURATION, &
               WINTER_TOTAL_CAPACITY, &
               CONTRIBUTES_TO_SPIN, &
               H2_UNIT_ID_NUM, &
               POWERDAT_PLANT_ID, &
               APPLY_NOX_SEASON_DATE, &
               NOX_SEASON_DATE, &
               RAMP_DOWN_RATE, &
               NOX_CONTROL_PERCENT, &
               NOX_CONTROL_DATE, &
               EMERGENCY_CAPACITY, &
               EMERGENCY_HEATRATE, &
               MARKET_RESOURCE, &
               PRIMARY_MOVER_STR, &
               COUNTY, &
               STATE_PROVINCE, &
               NOX_VOM, &
               NOX_FOM, &
               S_FuelDev_0, &
               S_FuelDev_2, &
               S_FuelDev_3, &
               CONSTANT_POLY, &
               FIRST_POLY, &
               SECOND_POLY, &
               THIRD_POLY, &
               USE_POLY_HEAT_RATES, &
               NEWGEN_UNIT_STATUS, &
               MONTHLY_MUST_RUN_VECTOR, &
               EMISSION_MARKET_LINK, &
               WVPA_RATE_TRACKER, &
               ALLOW_DECOMMIT, &
               MIN_SPIN_CAP, &
               MAX_SPIN_CAP, &
               WVPA_RES_TRACKER, &
               WVPA_FUEL_TRACKER, &
               MONTE_OR_FREQ, &
               WVPA_MEM_TRACKER, &
               MONTHLY_DECOMMIT_VECTOR, &
               TRANS_BUS_ID, &
               SOX_CONTROL_PERCENT, &
               SOX_CONTROL_DATE, &
               SOX_VOM, &
               SOX_FOM, &
               LATITUDE, &
               LONGITUDE, &
               MW_INSIDE_FENCE, &
                     INTER_BLOCKS, &
                     FIRST_YEAR_DECOMM_AVAIALABLE,     & !  152
                     DECOMMISSIONING_BASE_YR_COST,            & !  153
                     DECOMMISSIONING_COST_ESCALATION, &
                     ANNUAL_ENERGY_PLANNING_FACTOR, &
                     AGGREGATE_THIS_UNIT,              & !  156
                     NAME_PLATE_CAPACITY, &
                     FUEL_BLENDING_IS, &
                     FuelRatio_pn,         & !  159-163
                     BlendableFuelsPtr, & !  164-167
                     FuelTransportationCost, & !  168-171
                     BlendedEnthalpyUp, & !  172
                     BlendedEnthalpyLo, &
                     BlendedSO2Up,                  & !  174
                     BettermentProjectID, &
                     DECOM_CONTINUING_COST, &
                     DECOM_CONT_COST_ESCALATION, &
                     EnrgPatternPointer, & !  184
                     EMISSION_DATA_UNITS, &
                     EmissRedRate,       & !  180-184
                     EmissMaxRate, &
                     MaxEnergyLimit, &
                     tch_type, &
                     LINKED_BETTERMENT_OPTION, & !  194
                     CO2_CONTROL_PERCENT, &
                     HG_CONTROL_PERCENT, &
                     OTHER3_CONTROL_PERCENT, &
                     CO2_CONTROL_DATE, &
                     HG_CONTROL_DATE, &
                     OTHER3_CONTROL_DATE, &
                     CO2_VOM, &
                     CO2_FOM, &
                     HG_VOM, &
                     HG_FOM, &
                     OTHER3_VOM, &
                     OTHER3_FOM, &
                     MARKET_FLOOR,             & !  207
                     MARKET_CEILING, & !  208
                     START_UP_COSTS_ESCALATION,  & !  209
                     CAPACITY_MARKET_TYPE, &
                     CAPACITY_MARKET_MONTH, &
                     capacity_market_pointer_pno, &
                     CAPACITY_MARKET_COST_ASSIGN, &
                     CAPACITY_MARKET_EXP_COLLECT, &
                     CAPACITY_MARKET_COIN_ADJ_FACT, &
                     PRIMARY_FUEL_CATEGORY, &
                     SECONDARY_FUEL_CATEGORY, & !  220
                     EMISSIONS_FUEL_CATEGORY, &
                     RPS_CONTRIBUTION_PERCENT, &
                     RETIREMENT_CANDIDATE, &
                     RETROFIT_CANDIDATE, &
                     RETROFIT_PROJECT_ID, &
                     CO2_BASIN_NAME, &
                     CO2_PIPELINE_DISTANCE, &
                     STATE_PROV_NAME, &
                     CO2_RETRO_HEAT_MULT, &
                     CO2_RETRO_CAP_MULT, &
                     THERMAL_GUID, &
                     THERMAL_PARENT_ID, & !  232
                     THERMAL_AGGREGATED_UNIT, &
                     FIRST_RETIREMENT_YEAR, &
                     UNIT_TYPE, &
                     UNIT_TYPE_CATEGORY, &
                     RPS_PROGRAM_NUMBER
!
            CAP_MW = MW(2)

            YEAR_START = 1

            IF(CAP_MW < 0.) THEN
               CAP_MW = GET_CL_MW_FROM_POINTR(CAP_MW,YEAR_START)
               CAP_MW = MAX(0.,CAP_MW)
            ELSE
!
!  IF NO POINTER HAS BEEN SPECIFIED FOR THE PEAK MONTH
!
               CAP_MW = MAX(0.,CAP_MW)
            ENDIF

            IF(MONTHLY_CAPACITY_POINTER > 0) THEN
               POINTR = FLOAT(MONTHLY_CAPACITY_POINTER)
               MONTH_MULT = GET_CL_MW_FROM_POINTR(POINTR, &
                                                 PEAK_MONTH(YEAR_START))
               IF(MONTH_MULT > 2. .AND. CAP_MW /= 0.) THEN
                  CAP_MW = MONTH_MULT
               ELSE
                  CAP_MW = MONTH_MULT * CAP_MW
               ENDIF

            ENDIF

!  EFFECTIVE MW'S OF PLANNING CAPACITY

            CAP_MW = CAP_MW * FRAOWN * PLANNING_FACTOR/100.
            IF(ABS(R_UNIT_CAP(I) - CAP_MW) > 1.) THEN
               WRITE(4,*) trim(R_UNIT_NAME(I))// &
                ', a capacity-limited expansion option, has a '// &
                'capacity value in the options file that is '// &
                'inconsistent with the capacity in the operations file.'
            ENDIF
         ENDDO

      RETURN

      ENTRY CL_CAPACITY_AND_TRANS_CHECK(R_LOCAL_UNIT_NAME, &
                                        R_LOCAL_DATA_POINTER, &
                                        R_LOCAL_UNIT_CAP, &
                                        R_LOCAL_TRANS_ID, &
                                        R_PRIMARY_FUEL_CATEGORY)
!

!
         IREC = R_LOCAL_DATA_POINTER
         IF(IREC <= 0) THEN
            WRITE(4,*) trim(R_LOCAL_UNIT_NAME)// &
                    ', a capacity-limited expansion option, does not '// &
                    'have a valid production pointer.'
         ENDIF
         READ(LAST_UNIT_NUM_OPENED,REC=IREC,IOSTAT=IOS) DELETE, &
          UNIT_NAME, &
          CL_LOAD_TYPE,EXPENSE_ASSIGNMENT,EXPENSE_COLLECTION, &
          GENGRP,FRAOWN,ONLIMO,OFLIMO,FUELMX,PRFCST, &
          PRESCR,SEFCST,SDESCR,FUELADJ,EFOR,MAINRT,VARCST,VRESCR, &
          FIXED_COST,FIXED_COST_ESCALATOR, &
          DISPADJ,HRFACT,MW,PLANNING_FACTOR,COEFF, &
          AI_CAPACITY_RATE,AI_CAPACITY_ESCALATOR, &
          AI_ENERGY_RATE,AI_ENERGY_ESCALATOR, &
          P_SO2,P_NOX,P_PARTICULATES,POOL_FRAC_OWN, &
          P_OTHER2,P_OTHER3,DISPADJ2, &
          RESOURCE_ID,FUEL_SUPPLY_ID, &
          P_NOX_BK2,SEC_EMISS_PTR,EMISS_FUEL_COST,EMISS_FUEL_ESCAL, &
          EMISS_FUEL_EMISS_PTR,EMISS_BLENDING_RATE, &
          PRIM_FUEL_EMISS_PTR, &
          PRIM_FUEL_TYPES,FUEL_TYPES,TIE_GROUP, &
          MONTHLY_CAPACITY_POINTER, &
          ANNUAL_CL_FIXED_COST, &
          ANNUAL_CL_FIXED_COST_ESC, &
          MAINT_DAYS, &
          DISPATCH_MULT, &
          EXCESS_ENERGY_SALES, &
          AI_REMAINING_LIFE, &
          AI_TAX_LIFE, &
          AI_ADR_TAX_LIFE, &
          ASSET_CLASS_NUM, &
          ASSET_CLASS_VECTOR, &
          INTRA_COMPANY_CLASS_ID, &
          INTRA_COMPANY_TRANSACTION, &
          SPECIAL_UNIT_ID, &
          MINIMUM_CAPACITY_FACTOR, &
          MAXIMUM_CAPACITY_FACTOR, &
          TRANSACTION_GROUP_ID, &
          DAY_TYPE_ID, &
          UNIT_ACTIVE, &
          BASECASE_PLANT_ID, &
          BASECASE_UNIT_ID, &
          BASECASE_MARKET_AREA_ID, &
          BASECASE_TRANS_AREA_ID, &
          BASECASE_PLANT_NERC_SUB_ID, &
          BASECASE_PLANT_OWNER_ID, &
          UTILITY_OWNED, &
          MONTHLY_FUEL_INDEX, &
          START_UP_COSTS, &
          START_UP_LOGIC, &
          RAMP_RATE, &
          MIN_DOWN_TIME, &
          REPORT_THIS_UNIT, &
          P_FuelDev_0, &
          P_FuelDev_2, &
          P_FuelDev_3, &
          MIN_UP_TIME, &
          HESI_UNIT_ID_NUM, &
          FOR_FREQUENCY, &
          FOR_DURATION, &
          WINTER_TOTAL_CAPACITY, &
          CONTRIBUTES_TO_SPIN, &
          H2_UNIT_ID_NUM, &
          POWERDAT_PLANT_ID, &
          APPLY_NOX_SEASON_DATE, &
          NOX_SEASON_DATE, &
          RAMP_DOWN_RATE, &
          NOX_CONTROL_PERCENT, &
          NOX_CONTROL_DATE, &
          EMERGENCY_CAPACITY, &
          EMERGENCY_HEATRATE, &
          MARKET_RESOURCE, &
          PRIMARY_MOVER_STR, &
          COUNTY, &
          STATE_PROVINCE, &
          NOX_VOM, &
          NOX_FOM, &
          S_FuelDev_0, &
          S_FuelDev_2, &
          S_FuelDev_3, &
          CONSTANT_POLY, &
          FIRST_POLY, &
          SECOND_POLY, &
          THIRD_POLY, &
          USE_POLY_HEAT_RATES, &
          NEWGEN_UNIT_STATUS, &
          MONTHLY_MUST_RUN_VECTOR, &
          EMISSION_MARKET_LINK, &
          WVPA_RATE_TRACKER, &
          ALLOW_DECOMMIT, &
          MIN_SPIN_CAP, &
          MAX_SPIN_CAP, &
          WVPA_RES_TRACKER, &
          WVPA_FUEL_TRACKER, &
          MONTE_OR_FREQ, &
          WVPA_MEM_TRACKER, &
          MONTHLY_DECOMMIT_VECTOR, &
          TRANS_BUS_ID, &
          SOX_CONTROL_PERCENT, &
          SOX_CONTROL_DATE, &
          SOX_VOM, &
          SOX_FOM, &
          LATITUDE, &
          LONGITUDE, &
          MW_INSIDE_FENCE, &
                INTER_BLOCKS, &
                     FIRST_YEAR_DECOMM_AVAIALABLE,     & !  152
                     DECOMMISSIONING_BASE_YR_COST,            & !  153
                     DECOMMISSIONING_COST_ESCALATION, &
                     ANNUAL_ENERGY_PLANNING_FACTOR, &
                     AGGREGATE_THIS_UNIT,              & !  156
                     NAME_PLATE_CAPACITY, &
                     FUEL_BLENDING_IS, &
                     FuelRatio_pn,         & !  159-163
                     BlendableFuelsPtr, & !  164-167
                     FuelTransportationCost, & !  168-171
                     BlendedEnthalpyUp, & !  172
                     BlendedEnthalpyLo, &
                     BlendedSO2Up,                  & !  174
                     BettermentProjectID, &
                     DECOM_CONTINUING_COST, &
                     DECOM_CONT_COST_ESCALATION, &
                     EnrgPatternPointer, & !  184
                     EMISSION_DATA_UNITS, &
                     EmissRedRate,       & !  180-184
                     EmissMaxRate, &
                     MaxEnergyLimit, &
                     tch_type, &
                     LINKED_BETTERMENT_OPTION, & !  194
                     CO2_CONTROL_PERCENT, &
                     HG_CONTROL_PERCENT, &
                     OTHER3_CONTROL_PERCENT, &
                     CO2_CONTROL_DATE, &
                     HG_CONTROL_DATE, &
                     OTHER3_CONTROL_DATE, &
                     CO2_VOM, &
                     CO2_FOM, &
                     HG_VOM, &
                     HG_FOM, &
                     OTHER3_VOM, &
                     OTHER3_FOM, &
                     MARKET_FLOOR,             & !  207
                     MARKET_CEILING, & !  208
                     START_UP_COSTS_ESCALATION,  & !  209
                     CAPACITY_MARKET_TYPE, &
                     CAPACITY_MARKET_MONTH, &
                     capacity_market_pointer_pno, &
                     CAPACITY_MARKET_COST_ASSIGN, &
                     CAPACITY_MARKET_EXP_COLLECT, &
                     CAPACITY_MARKET_COIN_ADJ_FACT, &
                     R_PRIMARY_FUEL_CATEGORY, &
                     SECONDARY_FUEL_CATEGORY, & !  218
                     EMISSIONS_FUEL_CATEGORY, &
                     RPS_CONTRIBUTION_PERCENT, & !  220
                     RETIREMENT_CANDIDATE, &
                     RETROFIT_CANDIDATE, &
                     RETROFIT_PROJECT_ID, &
                     CO2_BASIN_NAME, &
                     CO2_PIPELINE_DISTANCE, &
                     STATE_PROV_NAME, &
                     CO2_RETRO_HEAT_MULT, &
                     CO2_RETRO_CAP_MULT, &
                     THERMAL_GUID, &
                     THERMAL_PARENT_ID, & !  230
                     THERMAL_AGGREGATED_UNIT, &
                     FIRST_RETIREMENT_YEAR, &
                     UNIT_TYPE, &
                     UNIT_TYPE_CATEGORY, &
                     RPS_PROGRAM_NUMBER ! 235
!
         CAP_MW = MW(2)
         YEAR_START = 1
!
         IF(CAP_MW < 0.) THEN
            CAP_MW = GET_CL_MW_FROM_POINTR(CAP_MW,YEAR_START)
            CAP_MW = MAX(0.,CAP_MW)
         ELSE
!
!  IF NO POINTER HAS BEEN SPECIFIED FOR THE PEAK MONTH
!
            CAP_MW = MAX(0.,CAP_MW)
         ENDIF
!
         IF(MONTHLY_CAPACITY_POINTER > 0) THEN
            POINTR = FLOAT(MONTHLY_CAPACITY_POINTER)
            MONTH_MULT = GET_CL_MW_FROM_POINTR(POINTR, &
                                                 PEAK_MONTH(YEAR_START))
            IF(MONTH_MULT > 2. .AND. CAP_MW /= 0.) THEN
               CAP_MW = MONTH_MULT
            ELSE
               CAP_MW = MONTH_MULT * CAP_MW
            ENDIF
         ENDIF
!
!  EFFECTIVE MW'S OF PLANNING CAPACITY
!
         CAP_MW = CAP_MW * FRAOWN * PLANNING_FACTOR/100.
         IF(ABS(R_LOCAL_UNIT_CAP - CAP_MW) > 1.) THEN
            WRITE(4,*) trim(R_LOCAL_UNIT_NAME)// &
                ', a capacity-limited expansion option, has a '// &
                'capacity value in the options file that is '// &
                'inconsistent with the capacity in the operations file.'
         ENDIF
!
         R_LOCAL_TRANS_ID = TRANSACTION_GROUP_ID
!
      RETURN
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END


