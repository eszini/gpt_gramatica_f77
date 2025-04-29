
!     BFIL_OBJ.FOR
!     Copyright(c) M.S. Gerber & Associates 2000
!
!     Created: 1/14/2003 8:07:52 PM
!     Author : MARK S GERBER
!     Last change: msg 11/11/2018 12:05:14 PM
            FUNCTION BASE_CASE_FILES(R_BASE_FILE_DEFINITION,   &
                               R_BASE_FILE_FAMILY)
!

      USE BASE_FILE_NAMES_FROM_BFIL_OBJ  ! SOURCE IN MODULES95
      use spindriftlib
      use end_routine
      use cl_data
      use SpinDriftLib
      use prod_arrays_dimensions
      use sizecom
      use forecast
      use enrg_helper
      use forecast
	  implicit none

      CHARACTER (len=256) ::  FILE_NAME
      CHARACTER (len=*) :: R_BASE_FILE_DEFINITION
      character (len=*) :: R_BASE_FILE_FAMILY
      CHARACTER (len=5) ::  VERSION,PUT_REFERENCE_LOAD
      LOGICAL (kind=4) ::  FILE_EXISTS

      LOGICAL (kind=1) ::  TURN_OFF_SYSTEM_BASE_FORECAST,   &
                VOID_LOG1,CONTROL_AREA_FORECAST

      CHARACTER (len=2048) ::  DSEXEC_LINE
      INTEGER (kind=2) ::  DELETE,POSbs
      INTEGER (kind=4) ::  IOS,I
      CHARACTER (len=4) ::  NONE
      PARAMETER(NONE='NONE')
      CHARACTER (len=256) ::   DIRECTORY_FOR_BASE_FILES,   &
                     DIRECTORY_FOR_RESULT_FILES,   &
                     DIRECTORY_FOR_LDE_FILES,   &
                     DIRECTORY_FOR_LDG_FILES,   &
                     DIRECTORY_FOR_PRB_FILES,   &
                     DIRECTORY_FOR_SHB_FILES,   &
                     DIRECTORY_FOR_XML_FILES,   &
                     SOFTWARE_DIRECTORY,   &
                     PathName
      CHARACTER (len=*) ::  S_BASE_FILE_DIRECTORY,   &
                     S_RESULTS_DIRECTORY,   &
                     S_SCENAME,   &
                     S_LDE_FILE_DIRECTORY,   &
                     S_LDG_FILE_DIRECTORY,   &
                     S_PRB_FILE_DIRECTORY,   &
                     S_SHB_FILE_DIRECTORY,   &
                     S_XML_FILE_DIRECTORY

      CHARACTER (len=5) ::  GET_SCENAME
      CHARACTER (len=256) ::  GET_RESULTS_DIRECTORY


!
! FUNCTION TYPE DEFINITIONS
!
      CHARACTER (len=5) ::  BASE_CASE_FILES,REFERENCE_LOAD_FILE
      CHARACTER (len=256) ::  TEST_NAME,BASE_FAMILY_NAME
      CHARACTER (len=256) ::   BASE_FILE_DIRECTORY,   &
                     STORE_BASE_FILE_DIRECTORY,   &
                     LDE_FILE_DIRECTORY,   &
                     LDG_FILE_DIRECTORY,   &
                     PRB_FILE_DIRECTORY,   &
                     SHB_FILE_DIRECTORY,   &
                     XML_FILE_DIRECTORY,   &
                     PROJECT_NAME,PROJECT_NAME_SAVE=NONE

      CHARACTER (LEN=256) :: WVPA_ACTUAL_PURCHASES_FILE,   &
                             TRANS_EXP_FILE,   &
                             IPALCO_ACTUAL_REGULATED_REVENUES_FILE,   &
                             ProSym_Interface_File,   &
                             ProSymBaseDataFile='NONE',   &
                             ESCALATION_FILE_1='NONE',   &
                             ESCALATION_FILE_2='NONE',   &
                             ESCALATION_FILE_3='NONE',   &
                             ESCALATION_FILE_4='NONE',   & !  316
                             ESCALATION_FILE_5='NONE',   & !  344
                             UPE_FILE_U1='NONE',   &
                             UPE_FILE_U2='NONE',   &
                             UPE_FILE_U3='NONE',   &
                             UPE_FILE_U4='NONE', & !  320
                             UPE_FILE_U5='NONE', & !  345
                             UPE_FILE_U6='NONE',   &
                             UPE_FILE_U7='NONE',   &
                             UPE_FILE_U8='NONE',   &
                             UPE_FILE_U9='NONE',  & !  349
                             CORES_AND_MACHINES_FILE='NONE', & !  351
                             RPS_REQUIREMENTS='NONE', & !  352
                             GQB_GAS_THERMAL='NONE'  ! 353

      CHARACTER (LEN=1) :: FUASTFIL
      CHARACTER (len=5) ::  STUDY_NAME=NONE ,EXASTFIL,DEBITFIL,   &
            DEBTFIL,NUCFLFIL,EXPENFIL,PARMASET,PARMFIN,INITFIL,   &
            TAXES_FILE,INTRSTFL,FINEXEC_FILE,ESCALFL,FOSSILFL,HYDROFL,   &
            UNIT_PARMS,SYSFRC,COMFRC,RESFRC,INDFRC,OTH1FRC,OTH2FRC,   &
            OTH3FRC,CLSHAPE,CLSHIST,LDMGT,LMGLS,ELASTICITY_FILE,   &
            NWCAP_PROD_FIL,NWCAP_FIN_FIL,CAP_STACK_FILE,CAP_RATIOS_FILE,   &
            ALLOCATOR_FILE,BSYRLOAD,LD_FIN_FIL,ENV_COM_FIL,ECON_INT_FIL,   &
            ASSET_RATE_FILE,ASSET_VECTOR_FILE,PUR_CONTRACT,   &
            PROD_PARM_FILE,AREA_ALLOCATORS_FILE,FUEL_INVENTORY_FILE,   &
            FUEL_PRICE_FILE,CAP_ENRG_TRANSACTIONS_FILE,   &
            FUEL_EMISSIONS_FILE,   &
            ASSET_SWITCHES_BASE_FILE,   &
            CLASS_SALES_BASE_FILE,   &
            ELIM_FILE,DETAILED_REPORT_FILE,   &
            OVER_UNDER_TABLE_FILE_NAME*68,   &
            CATAWBA2_CONTRACT_FILE,   &
            UNIT_OUTAGE_FILE,   &
            MONTHLY_PAYABLES_FILE,   &
            TRANSACT_TIE_FILE,DAY_TYPE_FILE,ENERGY_PRODUCTS_FILE,   &
            SCENARIO_MAKER_FILE,   &
            REG_OUT_PARAM_FILE,   &
            MARKET_PRICE_FILE,CPL_CONTRACT_FILE,   &
            RDI_POWERDAT_FILE,RDI_BASECASE_FILE,TRANS_FORECAST_FILE,   &
            TAX_INFO_FILE,ADDENDUM_FILE,TRANS_GROUPS_FILE,   &
            TRANS_PATHS_FILE,TRANS_CONSTRAINT_FILE,   &
            CPL_DEFERRED_FUEL_FILE,NEW_MARKET_PRICE_FILE,   &
            SCENARIO_PARAMETER_FILE,   &
            WEEKLY_HYDRO_FILE,   &
            WEATHER_DEMAND_FILE,   &
            SCENARIO_HOURLY_FILE,   &
            FORWARD_OPTION_FILE,   &
            FUEL_PRICE_FILE_F1,   &
            FUEL_PRICE_FILE_F2,   &
            FUEL_PRICE_FILE_F3,   &
            FUEL_PRICE_FILE_F4,   &
            FUEL_PRICE_FILE_F5,   &
            FUEL_PRICE_FILE_F6,   &
            FUEL_PRICE_FILE_F7,   &
            FUEL_PRICE_FILE_F8,   &
            FUEL_PRICE_FILE_F9,   &
            FUEL_PRICE_FILE_F0,   &
            MARKET_GROUPS_FILE,   &
            REGIONAL_OUTAGES_FILE,   &
            SCENARIO_VECTOR_FILE,   &
            USER_DAY_FILE,   &
            ICAP_FILE,   &
            REGIONAL_PARAMETER_FILE,   &
            CAPITAL_RATES_FILE,   &
            FUEL_DERIVATIVES_FILE,   &
            WVPA_RATE_STRUCTURES_FILE,   &
            WVPA_ADJUSTMENT_TRACKER_FILE,   &
            WVPA_COOP_REV_FORECAST_FILE,   &
            LH_GLOBAL_FILE,   &
            ELECT_HOUR_MULT_FILE,   &
            GAS_HOUR_MULT_FILE,   &
            OIL_HOUR_MULT_FILE,   &
            GAS_NODE_FILE,   &
            GAS_STORAGE_FILE,   &
            GAS_WEATHER_DEMAND_FILE,   &
            GAS_TRANSPORT_FILE,   &
            GAS_DEMAND_FORECAST_FILE,   &
            GAS_SUPPLY_FORECAST_FILE,   &
            RPS_REQUIREMENTS_FILE,   &
            RPS_STATE_DEMAND_FILE,   &
            THERMAL_RETROFIT_FILE,   &
            RPS_PROGRAM_FILE
!
      CHARACTER (LEN=256) :: FXE_FILE=NONE ,   & !  213 CAPEX EXPANSION FUTURE ASSETS
                             CL_UNITS_CS=NONE ,   &
                             CL_UNITS_CU=NONE ,   &
                             CL_UNITS_CV=NONE ,   &
                             CL_UNITS_CW=NONE ,   &
                             CL_UNITS_CX=NONE ,   &
                             CL_UNITS_CY=NONE ,   &
                             CO2_PARAM=NONE ,          & !  362
                             CO2_PARAM_FILE,   &
                             EPE_FILE=NONE ,   &
                             EXPENSE_FILE_X1=NONE ,   &
                             EXPENSE_FILE_X2=NONE ,   &
                             EXPENSE_FILE_X3=NONE ,   &
                             EXPENSE_FILE_X4=NONE ,   &
                             EXPENSE_FILE_X5=NONE   ! 174

      CHARACTER (len=5) ::  EAE_FILE=NONE ,DDE_FILE=NONE ,   &
                  DBE_FILE=NONE ,FAE_FILE=NONE ,   &
                  NFE_FILE=NONE ,   &
                  PRF_FILE=NONE ,   &
                  INE_FILE=NONE ,TAX_FILE=NONE ,   &
                  INT_FILE=NONE ,FIN_FILE=NONE ,   &
                  OLD_FIN_FILE=NONE ,   &
                  ESC_FILE=NONE ,FOS_FILE=NONE ,   &
                  HYD_FILE=NONE ,UPE_FILE=NONE ,   &
                  SFC_FILE=NONE ,CFC_FILE=NONE ,   &
                  RFC_FILE=NONE ,IFC_FILE=NONE ,   &
                  O1F_FILE=NONE ,O2F_FILE=NONE ,   &
                  O3F_FILE=NONE ,CLS_FILE=NONE ,   &
                  CLH_FILE=NONE ,LMG_FILE=NONE ,   &
                  LMS_FILE=NONE ,PFD_FILE=NONE ,   &
                  PNP_FILE=NONE ,FNP_FILE=NONE ,   &
                  CNP_FILE=NONE ,CRA_FILE=NONE ,   &
                  COS_FILE=NONE ,LFI_FILE=NONE ,   &
                  ENV_FILE=NONE ,ECO_FILE=NONE ,   &
                  ASTRT_FILE=NONE ,ASTVEC_FILE=NONE ,   &
                  CNT_FILE=NONE ,PRODP_FILE=NONE ,   &
                  AREA_FILE=NONE ,FUEL_INVENT_FILE=NONE ,   &
                  FUEL_PRICES_FILE=NONE ,   &
                  SERVICE_TRANSACTIONS_FILE=NONE ,   &
                  FUEL_EMISSIONS_DATA_FILE=NONE ,   &
                  BASE_YEAR_LOAD=NONE ,   &
                  OVER_UNDER_DECISIONS_TABLES=NONE ,   &
                  ASSET_CLASS_RUN_SWITCHES=NONE ,   &
                  ASSET_CLASS_SALES_FORECAST=NONE ,   &
                  ASSET_CLASS_ELIMINATIONS=NONE ,   &
                  DETAILED_REPORTS=NONE ,   &
                  CATAWBA2_CONTRACT=NONE ,   &
                  UNIT_OUTAGE=NONE ,   &
                  TRANSACT_TIE=NONE ,   &
                  MONTHLY_PAYABLES=NONE ,   &
                  DAY_TYPE=NONE ,   &
                  SCENARIO_MAKER=NONE ,   &
                  ENERGY_PRODUCTS=NONE ,   &
                  MARKET_PRICE=NONE ,   &
                  CPL_CONTRACT=NONE ,   &
                  CPL_DEFERRED_FUEL=NONE ,   &
                  RDI_POWERDAT=NONE ,   &
                  RDI_BASECASE=NONE ,   &
                  TRANS_FORECAST=NONE ,   &
                  TAX_INFO=NONE , & !  USING FOR CP&L TAX 9/13/98
                  ADDENDUM_INFO=NONE ,   &
                  TRANS_GROUPS_INFO=NONE ,   &
                  TRANS_PATHS_INFO=NONE ,   &
                  TRANS_CONSTRAINT_INFO=NONE ,   &
                  SCENARIO_PARAMETER=NONE ,   &
                  NEW_MARKET_PRICE=NONE ,   &
                  WEEKLY_HYDRO=NONE ,   &
                  WEATHER_DEMAND=NONE ,   &
                  SCENARIO_HOURLY=NONE ,   &
                  FORWARD_OPTION=NONE ,   &
                  PLACE_HOLDER=NONE ,  & !  75-83
                  MARKET_GROUPS=NONE , & !  84
                  FUEL_PRICE_F1=NONE ,   &
                  FUEL_PRICE_F2=NONE ,   &
                  FUEL_PRICE_F3=NONE ,   &
                  FUEL_PRICE_F4=NONE ,   &
                  FUEL_PRICE_F5=NONE ,   &
                  FUEL_PRICE_F6=NONE ,   &
                  FUEL_PRICE_F7=NONE ,   &
                  FUEL_PRICE_F8=NONE ,   &
                  FUEL_PRICE_F9=NONE , & !  93
                  FUEL_PRICE_F0=NONE , & !  335
                  RDI_PLACE_HOLDER=NONE ,   &
                  WVPA_ACTUAL_PURCHASES=NONE ,   &
                  CL_UNITS_C1=NONE ,   &
                  CL_UNITS_C2=NONE ,   &
                  CL_UNITS_C3=NONE ,   &
                  CL_UNITS_C4=NONE ,   &
                  CL_UNITS_C5=NONE ,   &
                  CL_UNITS_C6=NONE , & ! 99
                  CL_UNITS_C7=NONE , & ! 100
                  CL_UNITS_C8=NONE , & !  130
                  CL_UNITS_C9=NONE ,   &
                  CL_UNITS_C0=NONE ,   &
                  CL_UNITS_CA=NONE ,   &
                  CL_UNITS_CB=NONE ,   &
                  CL_UNITS_CC=NONE ,   &
                  CL_UNITS_CD=NONE , & !  136
                  EL_UNITS_E1=NONE , & !  137
                  EL_UNITS_E2=NONE ,   &
                  EL_UNITS_E3=NONE ,   &
                  EL_UNITS_E4=NONE , & !  140
                  EL_UNITS_E5=NONE ,   &
                  EL_UNITS_E6=NONE ,   &
                  EL_UNITS_E7=NONE ,   &
                  EL_UNITS_E8=NONE ,   &
                  EL_UNITS_E9=NONE , & !  145
                  EL_UNITS_E0=NONE ,   &
                  EL_UNITS_EB=NONE ,   &
                  EL_UNITS_ED=NONE ,   &
                  EL_UNITS_EE=NONE ,   &
                  EL_UNITS_EF=NONE , & !  150
                  TF_UNITS_T1=NONE , & !  151
                  TF_UNITS_T2=NONE ,   &
                  TF_UNITS_T3=NONE ,   &
                  TF_UNITS_T4=NONE ,   &
                  TF_UNITS_T5=NONE , & !  155
                  TF_UNITS_T6=NONE ,   &
                  TF_UNITS_T7=NONE ,   &
                  TF_UNITS_T8=NONE ,   &
                  TF_UNITS_T9=NONE ,   &
                  TF_UNITS_T0=NONE , & !  160
                  TF_UNITS_TB=NONE ,   &
                  TF_UNITS_TD=NONE ,   &
                  TF_UNITS_TH=NONE ,   &
                  TF_UNITS_TI=NONE ,  & !  164
                  TF_UNITS_TA=NONE ,  & !  377
                  TF_UNITS_TC=NONE ,  & !  378
                  CL_UNITS_CE=NONE ,  & !  165
                  CL_UNITS_CG=NONE ,  & !  166
                  EL_UNITS_EG=NONE ,  & !  167
                  EL_UNITS_EH=NONE ,   & !  168
                  EL_UNITS_EI=NONE ,   & !  211
                  EL_UNITS_EO=NONE ,   & !  381
                  EL_UNITS_EP=NONE ,   & !  382
                  REGIONAL_OUTAGES=NONE , & !  169
                  SCENARIO_VECTOR=NONE ,  & !  175
                  SM_UNITS_S1=NONE ,   &
                  SM_UNITS_S2=NONE ,   &
                  SM_UNITS_S3=NONE ,   &
                  SM_UNITS_S4=NONE ,   &
                  SM_UNITS_S5=NONE ,   &
                  SM_UNITS_S6=NONE ,   &
                  SM_UNITS_S7=NONE ,   &
                  SM_UNITS_S8=NONE ,   &
                  SM_UNITS_S9=NONE ,   &
                  USER_DAY=NONE , & !  185
                  ICAP=NONE ,   &
                  REGIONAL_PARAMETER=NONE , & ! 187
                  FUEL_DERIVATIVES=NONE ,   &
                  FINANCIAL_HISTORY=NONE , & !  189
                  MONTHLY_CAPITIAL_RATES=NONE , & !  190
                  WVPA_RATES_STRUCTURES=NONE ,   &
                  WVPA_TRACKER_FILE=NONE ,  & !  192
                  WVPA_COOP_FORECAST_FILE=NONE ,  & !  193
                  RM_UNITS_R1=NONE , & !  194
                  RM_UNITS_R2=NONE , & ! 195
                  RM_UNITS_R3=NONE ,   & !  196
                  LH_GLOBAL=NONE ,   &
                  ELECT_HOUR_MULT=NONE ,   &
                  GAS_HOUR_MULT=NONE ,   &
                  OIL_HOUR_MULT=NONE , & !  200
                  ENERGY_PRODUCTS_P2=NONE , & !  201
                  ENERGY_PRODUCTS_P3=NONE ,   &
                  ENERGY_PRODUCTS_P4=NONE ,   &
                  ENERGY_PRODUCTS_P5=NONE ,  & !  204
                  ENERGY_PRODUCTS_P6=NONE ,  & !  212
                  ENERGY_PRODUCTS_P0=NONE ,  & !  385
                  ENERGY_PRODUCTS_P1=NONE ,  & !  386
                  TRANS_EXP=NONE ,         & !  258
                  GAS_NODE=NONE ,          & !  290
                  GAS_STORAGE=NONE ,         & !  334
                  GAS_TRANSPORT=NONE ,       & !  291
                  GAS_DEMAND_FORECAST=NONE , & !  292
                  GAS_SUPPLY_FORECAST=NONE , & !  293
                  GF_UNITS_T1=NONE , & !  294
                  GF_UNITS_T2=NONE ,   &
                  GF_UNITS_T3=NONE ,   &
                  GF_UNITS_T4=NONE ,   &
                  GF_UNITS_T5=NONE , & ! 
                  GF_UNITS_T6=NONE ,   &
                  GF_UNITS_T7=NONE ,   &
                  GF_UNITS_T8=NONE ,   &
                  GF_UNITS_T9=NONE , & !  302
                  GAS_WEATHER_DEMAND=NONE , & !  303
                  CF_UNITS_T1=NONE ,          & !  358
                  RPS_STATE_DEMAND=NONE ,     & !  359
                  THERMAL_RETROFIT=NONE ,     & !  360
                  CL_UNITS_CI=NONE ,   &
                  CL_UNITS_CJ=NONE ,   &
                  CL_UNITS_CK=NONE ,   &
                  CL_UNITS_CM=NONE ,   &
                  CL_UNITS_CP=NONE ,   &
                  CL_UNITS_CQ=NONE , & !  210
                  CL_UNITS_OPTIONS_CX=NONE , & !  263
                  USER_DAY_D1=NONE ,      & !  363
                  USER_DAY_D2=NONE ,      & !  364
                  USER_DAY_D3=NONE ,      & !  365
                  USER_DAY_D4=NONE ,      & !  366
                  USER_DAY_D5=NONE ,      & !  367
                  USER_DAY_D6=NONE ,      & !  368
                  USER_DAY_D7=NONE ,      & !  369
                  USER_DAY_D8=NONE ,      & !  370
                  USER_DAY_D9=NONE ,       & !  371
                  CL_UNITS_EJ=NONE ,      & !  379
                  CL_UNITS_EK=NONE ,      & !  380
                  CL_UNITS_ER=NONE ,      & !  383
                  CL_UNITS_ET=NONE ,      & !  384
                  CL_UNITS_MX=NONE ,      & !  387
                  CL_UNITS_MY=NONE ,      & !  388
                  RPS_PROGRAM=NONE       ! 376

      SAVE           DIRECTORY_FOR_BASE_FILES,   &
                     DIRECTORY_FOR_RESULT_FILES,   &
                     DIRECTORY_FOR_LDE_FILES,   &
                     DIRECTORY_FOR_LDG_FILES,   &
                     DIRECTORY_FOR_PRB_FILES,   &
                     DIRECTORY_FOR_SHB_FILES,   &
                     DIRECTORY_FOR_XML_FILES

      CHARACTER (len=*) :: R_BASE_FILE_NAMES(0:*)
      INTEGER (KIND=4) :: FILE_NUMBER
      CHARACTER (len=256) , SAVE :: BASE_FILE_NAMES(400)
      CHARACTER (LEN=256) :: RETURN_BASE_FILE_NAME
      character (len=1024) :: current_path
!
      FILE_NAME = "DSF"//trim(R_BASE_FILE_DEFINITION)//".DAT"
      call write_log_entry("bfil_obj:0002", &
      "base_file_definition=" // trim(R_BASE_FILE_DEFINITION))
      
!       debugmod -- getcwd is telling me we're in the debug area.

      call getcwd(current_path)
      call write_log_entry("bfil_objt:0003", &
        "getcwd returned " // trim(current_path) // ".")
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      
      IF(.NOT. FILE_EXISTS) THEN
        er_message="bfil_obj:0001 - " // &
        "File " // trim(file_name) // " does not exist."
        call end_program(er_message)
      else
        call write_log_entry("bfil_obj:0004", &
        trim(file_name) // " exists.")
      end if
      OPEN(10,FILE=FILE_NAME,ACCESS='SEQUENTIAL')
      call write_log_entry("bfil_obj:0005", &
      "opened " // trim(file_name) //".")
      READ(10,*)
      DO
         READ(10,"(A)",IOSTAT=IOS) DSEXEC_LINE
         IF(IOS /= 0) THEN

            CALL MG_LOCATE_WRITE(14,35,   &
                                   'Base Case Definition is missing or',   &
                                                         ALL_VERSIONS,1)
            CALL MG_LOCATE_WRITE(15,35,'error reading definition file!',   &
                                                         ALL_VERSIONS,1)
            er_message="bfil.0001 - Base case definition missing or "   &
       //"error reading definition file."
            call end_program(er_message)

         ENDIF
         DSEXEC_LINE = trim(DSEXEC_LINE)//',,,,,,,,,,,,,,,,,,,,,,,,,,'   &
                                          //',,,,,,,,,,,,,,,,,,,,,,,,,,'   &
                                          //',,,,,,,,,,,,,,,,,,,,,,,,,,'   &
                                          //',,,,,,,,,,,,,,,,,,,,,,,,,,'   &
                                          //',,,,,,,,,,,,,,,,,,,,,,,,,,'   &
                                          //',,,,,,,,,,,,,,,,,,,,,,,,,,'   &
                                          //',,,,,,,,,,,,,,,,,,,,,,,,,,'   &
                                          //',,,,,,,,,,,,,,,,,,,,,,,,,,'   &
                                          //',,,,,,,,,,,,,,,,,,,,,,,,,,'   &
                                          //',,,,,,,,,,,,,,,,,,,,,,,,,,'   &
                                          //',,,,,,,,,,,,,,,,,,,,,,,,,,'   &
                                          //',,,,,,,,,,,,,,,,,,,,,,,,,,'   &
                                          //',,,,,,,,,,,,,,,,,,,,,,,,,,'   &
                                          //',,,,,,,,,,,,,,,,,,,,,,,,,,'   &
                                          //',,,,,,,,,,,,,,,,,,,,,,,,,,'   &
                                          //',,,,,,,,,,,,,,,,,,,,,,,,,,'   &
                                          //',,,,,,,,,,,,,,,,,,,,,,,,,,'   &
                                          //',,,,,,,,,,,,,,,,,,,,,,,,,,'   &
                                          //',,,,,,,,,,,,,,,,,,,,,,,,,,'   &
                                          //',,,,,,,,,,,,,,,,,,,,,,,,,,'   &
                                          //',,,,,,,,,,,,,,,,,,,,,,,,,,'   &
                                          //',,,,,,,,,,,,,,,,,,,,,,,,,,'   &
                                          //',,,,,,,,,,,,,,,,,,,,,,,,,,'   &
                                          //',,,,,,,,,,,,,,,,,,,,,,,,,,'   &
                                          //',,,,,,,,,,,,,,,,,,,,,,,,,,'   &
                                          //',,,,,,,,,,,,,,,,,,,,,,,,,,'   &
                                          //',,,,,,,,,,,,,,,,,,,,,,,,,,'   &
                                          //',,,,,,,,,,,,,,,,,,,,,,,,,,'   &
                                          //',,,,,,,,,,,,,,,,,,,,,,,,,,'   &
                                          //',,,,,,,,,,,,,,,,,,,,,,,,,,'   &
                                          //',,,,,,,,,,,,,,,,,,,,,,,,,,'   &
                                          //',,,,,,,,,,,,,,,,,,,,,,,,,,'
!
! CODES TO READ(5, FROM A FAMILY OF BASE FILES
!
         READ(DSEXEC_LINE,*) DELETE,TEST_NAME
         CALL UPC(TEST_NAME,BASE_FAMILY_NAME)
         IF((INDEX(BASE_FAMILY_NAME,R_BASE_FILE_FAMILY) /= 0 .AND.   &
                  INDEX(R_BASE_FILE_FAMILY,BASE_FAMILY_NAME) /= 0) .OR.   &
                                      R_BASE_FILE_FAMILY == 'NONE') THEN
            BASE_FILE_NAMES = "NONE"
            ProSymBaseDataFile = "NONE"

            READ(DSEXEC_LINE,*) DELETE,STUDY_NAME,   &
                             EAE_FILE,DDE_FILE,DBE_FILE,FAE_FILE,   &
                             NFE_FILE,EPE_FILE,   &
                             EXPENSE_NAMES_RESERVED,PRF_FILE,   &
                             INE_FILE,TAX_FILE,INT_FILE,OLD_FIN_FILE,   &
                             ESC_FILE,FOS_FILE,HYD_FILE,UPE_FILE,   &
                             SFC_FILE,CFC_FILE,RFC_FILE,IFC_FILE,   &
                             O1F_FILE,O2F_FILE,O3F_FILE,CLS_FILE,   &
                             CLH_FILE,LMG_FILE,LMS_FILE,PFD_FILE,   &
                             PNP_FILE,FNP_FILE,CNP_FILE,CRA_FILE,   &
                             COS_FILE,BASE_YEAR_LOAD,   &
                             LFI_FILE,ENV_FILE,ECO_FILE,   &
                             ASTRT_FILE,ASTVEC_FILE,CNT_FILE,PRODP_FILE,   &
                             AREA_FILE,FUEL_INVENT_FILE,   &
                             FUEL_PRICES_FILE,SERVICE_TRANSACTIONS_FILE,   &
                             FUEL_EMISSIONS_DATA_FILE,   &
                             OVER_UNDER_DECISIONS_TABLES,   &
                             ASSET_CLASS_RUN_SWITCHES,   &
                             ASSET_CLASS_SALES_FORECAST,   &
                             ASSET_CLASS_ELIMINATIONS,   &
                             DETAILED_REPORTS,   &
                             FIN_FILE,   &
                             CATAWBA2_CONTRACT,   &
                             UNIT_OUTAGE,   &
                             TRANSACT_TIE,   &
                             MONTHLY_PAYABLES,   &
                             DAY_TYPE,   &
                             SCENARIO_MAKER,   &
                             ENERGY_PRODUCTS,   &
                             MARKET_PRICE,   &
                             CPL_CONTRACT,   &
                             CPL_DEFERRED_FUEL,   & !  FILE 70 MSG 3/7/99
                             RDI_BASECASE,   &
                             TRANS_FORECAST,   &
                             TAX_INFO,  & !  USING FOR CP&L TAX 9/13/98
                             ADDENDUM_INFO,   &
                             TRANS_GROUPS_INFO,   &
                             TRANS_PATHS_INFO,   &
                             TRANS_CONSTRAINT_INFO,   &
                             SCENARIO_PARAMETER,   &
                             NEW_MARKET_PRICE,   &
                             WEEKLY_HYDRO,   &
                             SCENARIO_HOURLY,   &
                             FORWARD_OPTION, & !  74
                             PLACE_HOLDER, & !  75
                             PLACE_HOLDER, & !  76
                             PLACE_HOLDER, & !  77
                             WEATHER_DEMAND, & !  78
                             PLACE_HOLDER, & !  79
                             PLACE_HOLDER, & !  80
                             PLACE_HOLDER, & !  81
                             PLACE_HOLDER, & !  82
                             PLACE_HOLDER, & !  83
                             MARKET_GROUPS, & !  84
                             FUEL_PRICE_F1, & !  85
                             FUEL_PRICE_F2,   &
                             FUEL_PRICE_F3,   &
                             FUEL_PRICE_F4,   &
                             FUEL_PRICE_F5,   &
                             FUEL_PRICE_F6,   &
                             FUEL_PRICE_F7,   &
                             FUEL_PRICE_F8,   &
                             FUEL_PRICE_F9, & !  93
                             CL_UNITS_C1,   &
                             CL_UNITS_C2,   &
                             CL_UNITS_C3,   &
                             CL_UNITS_C4,   &
                             CL_UNITS_C5,   &
                             CL_UNITS_C6, & ! 99
                             CL_UNITS_C7, & ! 100
                             ProSymBaseDataFile, & !  101
                             (BASE_FILE_NAMES(I), I=102,129),   &
                             CL_UNITS_C8, & !  130
                             CL_UNITS_C9,   &
                             CL_UNITS_C0,   &
                             CL_UNITS_CA,   &
                             CL_UNITS_CB,   &
                             CL_UNITS_CC,   &
                             CL_UNITS_CD,  & !  136
                             EL_UNITS_E1, & !  137
                             EL_UNITS_E2,   &
                             EL_UNITS_E3,   &
                             EL_UNITS_E4, & !  140
                             EL_UNITS_E5,   &
                             EL_UNITS_E6,   &
                             EL_UNITS_E7,   &
                             EL_UNITS_E8,   &
                             EL_UNITS_E9, & !  145
                             EL_UNITS_E0,   &
                             EL_UNITS_EB,   &
                             EL_UNITS_ED,   &
                             EL_UNITS_EE,   &
                             EL_UNITS_EF, & !  150
                             TF_UNITS_T1, & !  151
                             TF_UNITS_T2,   &
                             TF_UNITS_T3,   &
                             TF_UNITS_T4,   &
                             TF_UNITS_T5, & !  155
                             TF_UNITS_T6,   &
                             TF_UNITS_T7,   &
                             TF_UNITS_T8,   &
                             TF_UNITS_T9,   &
                             TF_UNITS_T0, & !  160
                             TF_UNITS_TB,   &
                             TF_UNITS_TD,   &
                             TF_UNITS_TH,   &
                             TF_UNITS_TI,  & !  164
                             CL_UNITS_CE,  & !  165
                             CL_UNITS_CG,  & !  166
                             EL_UNITS_EG,  & !  167
                             EL_UNITS_EH,   & !  168
                             REGIONAL_OUTAGES, & !  169
                             EXPENSE_FILE_X1,   &
                             EXPENSE_FILE_X2,   &
                             EXPENSE_FILE_X3,   &
                             EXPENSE_FILE_X4,   &
                             EXPENSE_FILE_X5, & !  174
                             SCENARIO_VECTOR,   &
                             SM_UNITS_S1,   &
                             SM_UNITS_S2,   &
                             SM_UNITS_S3,   &
                             SM_UNITS_S4,   &
                             SM_UNITS_S5,   &
                             SM_UNITS_S6,   &
                             SM_UNITS_S7,   &
                             SM_UNITS_S8,   &
                             SM_UNITS_S9, & !  184
                             USER_DAY,   &
                             ICAP,   &
                             REGIONAL_PARAMETER, & !  187
                             FUEL_DERIVATIVES,   &
                             FINANCIAL_HISTORY,   &
                             MONTHLY_CAPITIAL_RATES, & !  190
                             WVPA_RATES_STRUCTURES,   &
                             WVPA_TRACKER_FILE,        & !  192
                             WVPA_COOP_FORECAST_FILE,  & !  193
                             RM_UNITS_R1,   &
                             RM_UNITS_R2,   &
                             RM_UNITS_R3,         & !  196
                             LH_GLOBAL,   &
                             ELECT_HOUR_MULT,   &
                             GAS_HOUR_MULT,   &
                             OIL_HOUR_MULT,  & !  200
                             ENERGY_PRODUCTS_P2,   &
                             ENERGY_PRODUCTS_P3,   &
                             ENERGY_PRODUCTS_P4,   &
                             ENERGY_PRODUCTS_P5, & !  204
                             CL_UNITS_CI,   &
                             CL_UNITS_CJ,   &
                             CL_UNITS_CK,   &
                             CL_UNITS_CM,   &
                             CL_UNITS_CP,   &
                             CL_UNITS_CQ, & !  210
                             EL_UNITS_EI, & !  211
                             ENERGY_PRODUCTS_P6, & !  212
                             FXE_FILE,      & !  213 CAPEX EXPANSION FUTURE ASSETS
                             (BASE_FILE_NAMES(I), I=214,289), & !  261-289 are SPCapEx Files
                             GAS_NODE,                       & !  290-299 Reserved for Gas Model
                             GAS_TRANSPORT,   &
                             GAS_DEMAND_FORECAST,   &
                             GAS_SUPPLY_FORECAST,   &
                             GF_UNITS_T1,   &
                             GF_UNITS_T2,   &
                             GF_UNITS_T3,   &
                             GF_UNITS_T4,   &
                             GF_UNITS_T5,   &
                             GF_UNITS_T6,   &
                             GF_UNITS_T7,   &
                             GF_UNITS_T8,   &
                             GF_UNITS_T9,   &
                             GAS_WEATHER_DEMAND,   &
                             (BASE_FILE_NAMES(I), I=304,306), & !  These are special LHC Files not used by AOD/MIDAS
                             CL_UNITS_CS,  & !  307
                             CL_UNITS_CU,   &
                             CL_UNITS_CV,   &
                             CL_UNITS_CW,  & !  310
                             CL_UNITS_CX,   &
                             CL_UNITS_CY,   & !  312
                             ESCALATION_FILE_1,   &
                             ESCALATION_FILE_2,   &
                             ESCALATION_FILE_3,   &
                             ESCALATION_FILE_4,   & !  316
                             UPE_FILE_U1,   &
                             UPE_FILE_U2,   &
                             UPE_FILE_U3,   &
                             UPE_FILE_U4,      & !  320
                             PLACE_HOLDER,     & !  321
                             PLACE_HOLDER,     & !  322
                             PLACE_HOLDER,     & !  323
                             PLACE_HOLDER,     & !  324
                             PLACE_HOLDER,     & !  325
                             PLACE_HOLDER,     & !  326
                             PLACE_HOLDER,     & !  327
                             PLACE_HOLDER,     & !  328
                             PLACE_HOLDER,     & !  329
                             PLACE_HOLDER,     & !  330
                             PLACE_HOLDER,     & !  331
                             PLACE_HOLDER,     & !  332
                             PLACE_HOLDER,     & !  333
                             GAS_STORAGE,      & !  334
                             FUEL_PRICE_F0,    & !  335
                             PLACE_HOLDER,     & !  336
                             PLACE_HOLDER,     & !  337
                             PLACE_HOLDER,     & !  338
                             PLACE_HOLDER,     & !  339
                             PLACE_HOLDER,     & !  340
                             PLACE_HOLDER,     & !  341
                             PLACE_HOLDER,     & !  342
                             PLACE_HOLDER,     & !  343
                             ESCALATION_FILE_5, & !  344
                             UPE_FILE_U5, & !  345
                             UPE_FILE_U6,   &
                             UPE_FILE_U7,   &
                             UPE_FILE_U8,   &
                             UPE_FILE_U9, & !  349
                             PLACE_HOLDER,     & !  350 LDG
                             CORES_AND_MACHINES_FILE, & !  351
                             RPS_REQUIREMENTS, & !  352
                             GQB_GAS_THERMAL,  & !  353
                             COAL_NODE,  & !  354
                             COAL_TRANSPORT,       & !  355
                             COAL_SUPPLY_FORECAST, & !  356
                             PLANT_DEMAND_FORECAST, & !  357
                             CF_UNITS_T1,          & !  358
                             RPS_STATE_DEMAND, & !  359
                             THERMAL_RETROFIT,  & !  360
                             COAL_CONTRACTS,  & !  361
                             CO2_PARAM,        & !  362
                             USER_DAY_D1,      & !  363
                             USER_DAY_D2,      & !  364
                             USER_DAY_D3,      & !  365
                             USER_DAY_D4,      & !  366
                             USER_DAY_D5,      & !  367
                             USER_DAY_D6,      & !  368
                             USER_DAY_D7,      & !  369
                             USER_DAY_D8,      & !  370
                             USER_DAY_D9,      & !  371
                             COAL_MODEL_POINTER_FILE,      & !  372
                             COAL_MODEL_SO2_INFO_FILE,      & !  373
                             COAL_MODEL_GENE_TRANS_FILE,  & !  374
                             COAL_MODEL_GENERIC_DEMAND_FILE, & !  375
                             RPS_PROGRAM,     & !  376
                             TF_UNITS_TA,      & !  377
                             TF_UNITS_TC,      & !  378
                             CL_UNITS_EJ,      & !  379
                             CL_UNITS_EK,      & !  380
                             EL_UNITS_EO,      & !  381
                             EL_UNITS_EP,      & !  382
                             CL_UNITS_ER,      & !  383
                             CL_UNITS_ET,      & !  384
                             ENERGY_PRODUCTS_P0,  & !  385
                             ENERGY_PRODUCTS_P1,  & !  386
                             CL_UNITS_MX,      & !  387
                             CL_UNITS_MY       ! 388

            WVPA_ACTUAL_PURCHASES = BASE_FILE_NAMES(107)
            TRANS_EXP = BASE_FILE_NAMES(258)
            EXIT
         ENDIF
      ENDDO
      CLOSE(10)
!
      IF((INDEX(SFC_FILE,NONE) /= 0) .AND.   &
               (SYSTEM_BASED_FORECAST()  .OR.   &
                                         CONTROL_AREA_FORECAST()) ) THEN
          VOID_LOG1 = TURN_OFF_SYSTEM_BASE_FORECAST()
      ENDIF

      STUDY_NAME = ClData%SCENAME
      PROJECT_NAME_SAVE = R_BASE_FILE_DEFINITION
      BASE_CASE_FILES = ClData%SCENAME
      HYDRO_FILES_ARE_ACTIVE_SAVED = (INDEX(HYD_FILE,'NONE') == 0) .OR.   &
                                   (INDEX(EL_UNITS_E1,'NONE') == 0) .OR.   &
                                   (INDEX(EL_UNITS_E2,'NONE') == 0) .OR.   &
                                   (INDEX(EL_UNITS_E3,'NONE') == 0) .OR.   &
                                   (INDEX(EL_UNITS_E4,'NONE') == 0) .OR.   &
                                   (INDEX(EL_UNITS_E5,'NONE') == 0) .OR.   &
                                   (INDEX(EL_UNITS_E6,'NONE') == 0) .OR.   &
                                   (INDEX(EL_UNITS_E7,'NONE') == 0) .OR.   &
                                   (INDEX(EL_UNITS_E8,'NONE') == 0) .OR.   &
                                   (INDEX(EL_UNITS_E9,'NONE') == 0) .OR.   &
                                   (INDEX(EL_UNITS_E0,'NONE') == 0) .OR.   &
                                   (INDEX(EL_UNITS_EB,'NONE') == 0) .OR.   &
                                   (INDEX(EL_UNITS_ED,'NONE') == 0) .OR.   &
                                   (INDEX(EL_UNITS_EE,'NONE') == 0) .OR.   &
                                   (INDEX(EL_UNITS_EF,'NONE') == 0) .OR.   &
                                   (INDEX(EL_UNITS_EG,'NONE') == 0) .OR.   &
                                   (INDEX(EL_UNITS_EH,'NONE') == 0) .OR.   &
                                   (INDEX(EL_UNITS_EO,'NONE') == 0) .OR.   &
                                   (INDEX(EL_UNITS_EP,'NONE') == 0) .OR.   &
                                   (INDEX(EL_UNITS_EI,'NONE') == 0)
      RETURN

       ENTRY EXASTFIL
         EXASTFIL = EAE_FILE
        RETURN
       ENTRY DEBITFIL
         DEBITFIL = DDE_FILE
      RETURN
       ENTRY DEBTFIL
         DEBTFIL = DBE_FILE
      RETURN
       ENTRY FUASTFIL(R_BASE_FILE_NAMES)
         R_BASE_FILE_NAMES(0) = FAE_FILE
         R_BASE_FILE_NAMES(1) = FXE_FILE
         FUASTFIL = "T"
         IF(INDEX(FAE_FILE,NONE) == 0 .OR. INDEX(FXE_FILE,NONE) == 0) then
                  FUASTFIL = "N"
         end if
      RETURN
       ENTRY NUCFLFIL
         NUCFLFIL = NFE_FILE
      RETURN
       ENTRY EXPENFIL(R_BASE_FILE_NAMES)
         R_BASE_FILE_NAMES(0) = EPE_FILE
         R_BASE_FILE_NAMES(1) = EXPENSE_FILE_X1
         R_BASE_FILE_NAMES(2) = EXPENSE_FILE_X2
         R_BASE_FILE_NAMES(3) = EXPENSE_FILE_X3
         R_BASE_FILE_NAMES(4) = EXPENSE_FILE_X4
         R_BASE_FILE_NAMES(5) = EXPENSE_FILE_X5
         EXPENSE_FILE_X1 = "NONE"
         IF(INDEX(EPE_FILE,NONE) == 0 .OR.   &
                        INDEX(EXPENSE_FILE_X1,NONE) == 0 .OR.   &
                        INDEX(EXPENSE_FILE_X2,NONE) == 0  .OR.   &
                        INDEX(EXPENSE_FILE_X3,NONE) == 0 .OR.   &
                        INDEX(EXPENSE_FILE_X4,NONE) == 0 .OR.   &
                        INDEX(EXPENSE_FILE_X5,NONE) == 0)   &
          EXPENFIL = 'AAAAA' ! EPE_FILE
      RETURN
       ENTRY PARMFIN
         PARMFIN = PRF_FILE
      RETURN
       ENTRY INITFIL
         INITFIL = INE_FILE
      RETURN
       ENTRY TAXES_FILE
         TAXES_FILE = TAX_FILE
      RETURN
       ENTRY TAX_INFO_FILE      ! USING FOR CP&L TAX 9/13/98
         TAX_INFO_FILE = TAX_INFO
      RETURN
      ENTRY ADDENDUM_FILE
         ADDENDUM_FILE = ADDENDUM_INFO
      RETURN
       ENTRY INTRSTFL
         INTRSTFL = INT_FILE
      RETURN
       ENTRY FINEXEC_FILE
         FINEXEC_FILE = FIN_FILE
      RETURN
       ENTRY DETAILED_REPORT_FILE
         DETAILED_REPORT_FILE = DETAILED_REPORTS
      RETURN
       ENTRY CATAWBA2_CONTRACT_FILE
         CATAWBA2_CONTRACT_FILE = CATAWBA2_CONTRACT
      RETURN
      ENTRY UNIT_OUTAGE_FILE
         UNIT_OUTAGE_FILE = UNIT_OUTAGE
      RETURN
      ENTRY MONTHLY_PAYABLES_FILE
         MONTHLY_PAYABLES_FILE = MONTHLY_PAYABLES
      RETURN
      ENTRY TRANS_CONSTRAINT_FILE
         TRANS_CONSTRAINT_FILE = TRANS_CONSTRAINT_INFO
      RETURN
      ENTRY TRANS_PATHS_FILE
         TRANS_PATHS_FILE = TRANS_PATHS_INFO
      RETURN
      ENTRY TRANS_GROUPS_FILE
         TRANS_GROUPS_FILE = TRANS_GROUPS_INFO
      RETURN
       ENTRY TRANSACT_TIE_FILE
         TRANSACT_TIE_FILE = TRANSACT_TIE
      RETURN
       ENTRY DAY_TYPE_FILE
         DAY_TYPE_FILE = DAY_TYPE
      RETURN
      ENTRY USER_DAY_FILE(R_BASE_FILE_NAMES)
         R_BASE_FILE_NAMES(0) = USER_DAY
         R_BASE_FILE_NAMES(1) = USER_DAY_D1     ! 363
         R_BASE_FILE_NAMES(2) = USER_DAY_D2     ! 364
         R_BASE_FILE_NAMES(3) = USER_DAY_D3     ! 365
         R_BASE_FILE_NAMES(4) = USER_DAY_D4     ! 366
         R_BASE_FILE_NAMES(5) = USER_DAY_D5     ! 367
         R_BASE_FILE_NAMES(6) = USER_DAY_D6     ! 368
         R_BASE_FILE_NAMES(7) = USER_DAY_D7     ! 369
         R_BASE_FILE_NAMES(8) = USER_DAY_D8     ! 370
         R_BASE_FILE_NAMES(9) = USER_DAY_D9     ! 371
         USER_DAY_FILE = USER_DAY
      RETURN
      ENTRY ICAP_FILE
         ICAP_FILE = ICAP
      RETURN

!!!! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ENTRY REGIONAL_OUTAGES_FILE
         REGIONAL_OUTAGES_FILE = REGIONAL_OUTAGES
      RETURN
      ENTRY REGIONAL_PARAMETER_FILE
         REGIONAL_PARAMETER_FILE = REGIONAL_PARAMETER
      RETURN
      ENTRY REG_OUT_PARAM_FILE(R_BASE_FILE_NAMES)
         R_BASE_FILE_NAMES(0) = REGIONAL_PARAMETER
         R_BASE_FILE_NAMES(1) = RM_UNITS_R1
         R_BASE_FILE_NAMES(2) = RM_UNITS_R2
         R_BASE_FILE_NAMES(3) = RM_UNITS_R3
         R_BASE_FILE_NAMES(4) = REGIONAL_OUTAGES
         REG_OUT_PARAM_FILE = REGIONAL_PARAMETER
      RETURN

!!!! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ENTRY FUEL_DERIVATIVES_FILE
         FUEL_DERIVATIVES_FILE = FUEL_DERIVATIVES
      RETURN
       ENTRY ENERGY_PRODUCTS_FILE(R_BASE_FILE_NAMES)
         ENERGY_PRODUCTS_FILE = ENERGY_PRODUCTS
         R_BASE_FILE_NAMES(0) = ENERGY_PRODUCTS
         R_BASE_FILE_NAMES(1) = ENERGY_PRODUCTS_P0
         R_BASE_FILE_NAMES(2) = ENERGY_PRODUCTS_P1
         R_BASE_FILE_NAMES(3) = ENERGY_PRODUCTS_P2
         R_BASE_FILE_NAMES(4) = ENERGY_PRODUCTS_P3
         R_BASE_FILE_NAMES(5) = ENERGY_PRODUCTS_P4
         R_BASE_FILE_NAMES(6) = ENERGY_PRODUCTS_P5
         R_BASE_FILE_NAMES(7) = ENERGY_PRODUCTS_P6
      RETURN
!
!!!! ! GAS MODEL
!
      ENTRY GAS_NODE_FILE
         GAS_NODE_FILE = GAS_NODE
      RETURN
!
! 090907.
!
      ENTRY GAS_STORAGE_FILE
         GAS_STORAGE_FILE = GAS_STORAGE
      RETURN
!
      ENTRY GAS_WEATHER_DEMAND_FILE
         GAS_WEATHER_DEMAND_FILE = GAS_WEATHER_DEMAND
      RETURN
! 070906
      ENTRY GAS_TRANSPORT_FILE
         GAS_TRANSPORT_FILE = GAS_TRANSPORT
      RETURN
!
      ENTRY TRANS_EXP_FILE
         TRANS_EXP_FILE = TRANS_EXP
      RETURN
!
      ENTRY GAS_DEMAND_FORECAST_FILE(R_BASE_FILE_NAMES)
         R_BASE_FILE_NAMES(0) = GAS_DEMAND_FORECAST
         R_BASE_FILE_NAMES(1) = GF_UNITS_T1
         R_BASE_FILE_NAMES(2) = GF_UNITS_T2
         R_BASE_FILE_NAMES(3) = GF_UNITS_T3
         R_BASE_FILE_NAMES(4) = GF_UNITS_T4
         R_BASE_FILE_NAMES(5) = GF_UNITS_T5
         R_BASE_FILE_NAMES(6) = GF_UNITS_T6
         R_BASE_FILE_NAMES(7) = GF_UNITS_T7
         R_BASE_FILE_NAMES(8) = GF_UNITS_T8
         R_BASE_FILE_NAMES(9) = GF_UNITS_T9
      RETURN
      ENTRY GAS_SUPPLY_FORECAST_FILE
         GAS_SUPPLY_FORECAST_FILE = GAS_SUPPLY_FORECAST
      RETURN
       ENTRY MARKET_PRICE_FILE
         MARKET_PRICE_FILE = MARKET_PRICE
      RETURN
       ENTRY NEW_MARKET_PRICE_FILE
         NEW_MARKET_PRICE_FILE = NEW_MARKET_PRICE
      RETURN
      ENTRY ELECT_HOUR_MULT_FILE
         ELECT_HOUR_MULT_FILE = ELECT_HOUR_MULT
      RETURN
      ENTRY LH_GLOBAL_FILE
         LH_GLOBAL_FILE = LH_GLOBAL
      RETURN
      ENTRY RPS_PROGRAM_FILE
         RPS_PROGRAM_FILE = RPS_PROGRAM
      RETURN
      ENTRY GAS_HOUR_MULT_FILE
         GAS_HOUR_MULT_FILE = GAS_HOUR_MULT
      RETURN
      ENTRY OIL_HOUR_MULT_FILE
         OIL_HOUR_MULT_FILE = OIL_HOUR_MULT
      RETURN
       ENTRY SCENARIO_HOURLY_FILE
         SCENARIO_HOURLY_FILE = SCENARIO_HOURLY
      RETURN
       ENTRY WEEKLY_HYDRO_FILE
         WEEKLY_HYDRO_FILE = WEEKLY_HYDRO
      RETURN
       ENTRY RPS_REQUIREMENTS_FILE
         RPS_REQUIREMENTS_FILE = RPS_REQUIREMENTS
      RETURN
       ENTRY RPS_STATE_DEMAND_FILE
         RPS_STATE_DEMAND_FILE = RPS_STATE_DEMAND
      RETURN
       ENTRY THERMAL_RETROFIT_FILE
         THERMAL_RETROFIT_FILE = THERMAL_RETROFIT
      RETURN
       ENTRY CO2_PARAM_FILE
         CO2_PARAM_FILE = CO2_PARAM
      RETURN
       ENTRY WEATHER_DEMAND_FILE
         WEATHER_DEMAND_FILE = WEATHER_DEMAND
      RETURN
       ENTRY FORWARD_OPTION_FILE
         FORWARD_OPTION_FILE = FORWARD_OPTION
      RETURN
       ENTRY MARKET_GROUPS_FILE
         MARKET_GROUPS_FILE = MARKET_GROUPS
      RETURN
       ENTRY CPL_CONTRACT_FILE
         CPL_CONTRACT_FILE = CPL_CONTRACT
      RETURN
      ENTRY CPL_DEFERRED_FUEL_FILE
         CPL_DEFERRED_FUEL_FILE = CPL_DEFERRED_FUEL
      RETURN
       ENTRY RDI_POWERDAT_FILE
         RDI_POWERDAT_FILE = RDI_POWERDAT
      RETURN
       ENTRY RDI_BASECASE_FILE
         RDI_BASECASE_FILE = RDI_BASECASE
      RETURN
       ENTRY TRANS_FORECAST_FILE(R_BASE_FILE_NAMES)
         R_BASE_FILE_NAMES(0) = TRANS_FORECAST
         R_BASE_FILE_NAMES(1) = TF_UNITS_T1
         R_BASE_FILE_NAMES(2) = TF_UNITS_T2
         R_BASE_FILE_NAMES(3) = TF_UNITS_T3
         R_BASE_FILE_NAMES(4) = TF_UNITS_T4
         R_BASE_FILE_NAMES(5) = TF_UNITS_T5
         R_BASE_FILE_NAMES(6) = TF_UNITS_T6
         R_BASE_FILE_NAMES(7) = TF_UNITS_T7
         R_BASE_FILE_NAMES(8) = TF_UNITS_T8
         R_BASE_FILE_NAMES(9) = TF_UNITS_T9
         R_BASE_FILE_NAMES(10) = TF_UNITS_T0
         R_BASE_FILE_NAMES(11) = TF_UNITS_TB
         R_BASE_FILE_NAMES(12) = TF_UNITS_TD
         R_BASE_FILE_NAMES(13) = TF_UNITS_TH
         R_BASE_FILE_NAMES(14) = TF_UNITS_TI
         R_BASE_FILE_NAMES(15) = TF_UNITS_TA  ! 377
         R_BASE_FILE_NAMES(16) = TF_UNITS_TC  ! 378
         TRANS_FORECAST_FILE = TRANS_FORECAST
      RETURN

      ENTRY SCENARIO_MAKER_FILE(R_BASE_FILE_NAMES)
         R_BASE_FILE_NAMES(0) = SCENARIO_MAKER
         R_BASE_FILE_NAMES(1) = SM_UNITS_S1
         R_BASE_FILE_NAMES(2) = SM_UNITS_S2
         R_BASE_FILE_NAMES(3) = SM_UNITS_S3
         R_BASE_FILE_NAMES(4) = SM_UNITS_S4
         R_BASE_FILE_NAMES(5) = SM_UNITS_S5
         R_BASE_FILE_NAMES(6) = SM_UNITS_S6
         R_BASE_FILE_NAMES(7) = SM_UNITS_S7
         R_BASE_FILE_NAMES(8) = SM_UNITS_S8
         R_BASE_FILE_NAMES(9) = SM_UNITS_S9
         SCENARIO_MAKER_FILE = SCENARIO_MAKER
      RETURN

       ENTRY ESCALFL(R_BASE_FILE_NAMES)
         R_BASE_FILE_NAMES(0) = ESC_FILE
         R_BASE_FILE_NAMES(1) = ESCALATION_FILE_1
         R_BASE_FILE_NAMES(2) = ESCALATION_FILE_2
         R_BASE_FILE_NAMES(3) = ESCALATION_FILE_3
         R_BASE_FILE_NAMES(4) = ESCALATION_FILE_4
         R_BASE_FILE_NAMES(5) = ESCALATION_FILE_5
         ESCALFL = ESC_FILE
      RETURN

       ENTRY FOSSILFL(R_BASE_FILE_NAMES)
         R_BASE_FILE_NAMES(0) = FOS_FILE     ! 14
         R_BASE_FILE_NAMES(1) = CL_UNITS_C1  ! 94
         R_BASE_FILE_NAMES(2) = CL_UNITS_C2  ! 95
         R_BASE_FILE_NAMES(3) = CL_UNITS_C3  ! 96
         R_BASE_FILE_NAMES(4) = CL_UNITS_C4  ! 97
         R_BASE_FILE_NAMES(5) = CL_UNITS_C5  ! 98
         R_BASE_FILE_NAMES(6) = CL_UNITS_C6  ! 99
         R_BASE_FILE_NAMES(7) = CL_UNITS_C7  ! 100
         R_BASE_FILE_NAMES(8) =  CL_UNITS_C8 ! 130
         R_BASE_FILE_NAMES(9) =  CL_UNITS_C9 ! 131
         R_BASE_FILE_NAMES(10) = CL_UNITS_C0 ! 132
         R_BASE_FILE_NAMES(11) = CL_UNITS_CA ! 133
         R_BASE_FILE_NAMES(12) = CL_UNITS_CB ! 134
         R_BASE_FILE_NAMES(13) = CL_UNITS_CC ! 135
         R_BASE_FILE_NAMES(14) = CL_UNITS_CD ! 136
         R_BASE_FILE_NAMES(15) = CL_UNITS_CE ! 165
         R_BASE_FILE_NAMES(16) = CL_UNITS_CG ! 166
         R_BASE_FILE_NAMES(17) = CL_UNITS_CI ! 205
         R_BASE_FILE_NAMES(18) = CL_UNITS_CJ ! 206
         R_BASE_FILE_NAMES(19) = CL_UNITS_CK ! 207
         R_BASE_FILE_NAMES(20) = CL_UNITS_CM ! 208
         R_BASE_FILE_NAMES(21) = CL_UNITS_CP ! 209
         R_BASE_FILE_NAMES(22) = CL_UNITS_CQ ! 210
         R_BASE_FILE_NAMES(23) = BASE_FILE_NAMES(263) ! SPCapEx
         R_BASE_FILE_NAMES(24) = CL_UNITS_CS ! 307
         R_BASE_FILE_NAMES(25) = CL_UNITS_CU ! 308
         R_BASE_FILE_NAMES(26) = CL_UNITS_CV ! 309
         R_BASE_FILE_NAMES(27) = CL_UNITS_CW ! 310
         R_BASE_FILE_NAMES(28) = CL_UNITS_CX ! 311
         R_BASE_FILE_NAMES(29) = CL_UNITS_CY ! 312
         R_BASE_FILE_NAMES(30) = CL_UNITS_EJ ! 379
         R_BASE_FILE_NAMES(31) = CL_UNITS_EK ! 380
         R_BASE_FILE_NAMES(32) = CL_UNITS_ER ! 383
         R_BASE_FILE_NAMES(33) = CL_UNITS_ET ! 384
         R_BASE_FILE_NAMES(34) = CL_UNITS_MX ! 387
         R_BASE_FILE_NAMES(35) = CL_UNITS_MY ! 388
         FOSSILFL = FOS_FILE
      RETURN

       ENTRY HYDROFL(R_BASE_FILE_NAMES)
         R_BASE_FILE_NAMES(0) = HYD_FILE
         R_BASE_FILE_NAMES(1) = EL_UNITS_E1
         R_BASE_FILE_NAMES(2) = EL_UNITS_E2
         R_BASE_FILE_NAMES(3) = EL_UNITS_E3
         R_BASE_FILE_NAMES(4) = EL_UNITS_E4
         R_BASE_FILE_NAMES(5) = EL_UNITS_E5
         R_BASE_FILE_NAMES(6) = EL_UNITS_E6
         R_BASE_FILE_NAMES(7) = EL_UNITS_E7
         R_BASE_FILE_NAMES(8) = EL_UNITS_E8
         R_BASE_FILE_NAMES(9) = EL_UNITS_E9
         R_BASE_FILE_NAMES(10) = EL_UNITS_E0
         R_BASE_FILE_NAMES(11) = EL_UNITS_EB
         R_BASE_FILE_NAMES(12) = EL_UNITS_ED
         R_BASE_FILE_NAMES(13) = EL_UNITS_EE
         R_BASE_FILE_NAMES(14) = EL_UNITS_EF
         R_BASE_FILE_NAMES(15) = EL_UNITS_EG
         R_BASE_FILE_NAMES(16) = EL_UNITS_EH
         R_BASE_FILE_NAMES(17) = EL_UNITS_EI
         R_BASE_FILE_NAMES(18) = EL_UNITS_EO      ! 381
         R_BASE_FILE_NAMES(19) = EL_UNITS_EP      ! 382
         HYDROFL = HYD_FILE
      RETURN


       ENTRY UNIT_PARMS(R_BASE_FILE_NAMES)
         R_BASE_FILE_NAMES(0) = UPE_FILE
         R_BASE_FILE_NAMES(1) = UPE_FILE_U1
         R_BASE_FILE_NAMES(2) = UPE_FILE_U2
         R_BASE_FILE_NAMES(3) = UPE_FILE_U3
         R_BASE_FILE_NAMES(4) = UPE_FILE_U4
         R_BASE_FILE_NAMES(5) = UPE_FILE_U5
         R_BASE_FILE_NAMES(6) = UPE_FILE_U6
         R_BASE_FILE_NAMES(7) = UPE_FILE_U7
         R_BASE_FILE_NAMES(8) = UPE_FILE_U8
         R_BASE_FILE_NAMES(9) = UPE_FILE_U9
         UNIT_PARMS = UPE_FILE
      RETURN

       ENTRY SYSFRC
         SYSFRC = SFC_FILE
      RETURN
       ENTRY COMFRC
         COMFRC = CFC_FILE
      RETURN
       ENTRY RESFRC
         RESFRC = RFC_FILE
      RETURN
       ENTRY INDFRC
         INDFRC = IFC_FILE
      RETURN
       ENTRY OTH1FRC
         OTH1FRC = O1F_FILE
      RETURN
       ENTRY OTH2FRC
         OTH2FRC = O2F_FILE
      RETURN
       ENTRY OTH3FRC
         OTH3FRC = O3F_FILE
      RETURN
       ENTRY CLSHAPE
         CLSHAPE = CLS_FILE
      RETURN
       ENTRY CLSHIST
         CLSHIST = CLH_FILE
      RETURN
       ENTRY LDMGT
         LDMGT = LMG_FILE
      RETURN
       ENTRY LMGLS
         LMGLS = LMS_FILE
      RETURN
       ENTRY ELASTICITY_FILE
         ELASTICITY_FILE = PFD_FILE
      RETURN
       ENTRY NWCAP_PROD_FIL
         NWCAP_PROD_FIL = PNP_FILE
      RETURN
       ENTRY NWCAP_FIN_FIL
         NWCAP_FIN_FIL = FNP_FILE
      RETURN
       ENTRY CAP_STACK_FILE
         CAP_STACK_FILE = CNP_FILE
      RETURN
       ENTRY CAP_RATIOS_FILE
         CAP_RATIOS_FILE = CRA_FILE
      RETURN
       ENTRY ALLOCATOR_FILE
         ALLOCATOR_FILE = COS_FILE
      RETURN
       ENTRY LD_FIN_FIL
         LD_FIN_FIL = LFI_FILE
      RETURN
       ENTRY ENV_COM_FIL
         ENV_COM_FIL = ENV_FILE
      RETURN
       ENTRY ECON_INT_FIL
         ECON_INT_FIL = ECO_FILE
      RETURN
       ENTRY ASSET_RATE_FILE
         ASSET_RATE_FILE = ASTRT_FILE
      RETURN
       ENTRY ASSET_VECTOR_FILE
         ASSET_VECTOR_FILE = ASTVEC_FILE
      RETURN
       ENTRY CAPITAL_RATES_FILE
         CAPITAL_RATES_FILE = MONTHLY_CAPITIAL_RATES
      RETURN
       ENTRY PUR_CONTRACT
         PUR_CONTRACT = CNT_FILE
      RETURN
       ENTRY PROD_PARM_FILE
         PROD_PARM_FILE = PRODP_FILE
      RETURN
       ENTRY AREA_ALLOCATORS_FILE
         AREA_ALLOCATORS_FILE = AREA_FILE
      RETURN
       ENTRY FUEL_INVENTORY_FILE
         FUEL_INVENTORY_FILE = FUEL_INVENT_FILE
      RETURN
       ENTRY FUEL_PRICE_FILE
         FUEL_PRICE_FILE = FUEL_PRICES_FILE
      RETURN
      ENTRY FUEL_PRICE_FILE_F1
         FUEL_PRICE_FILE_F1 = FUEL_PRICE_F1
      RETURN
      ENTRY FUEL_PRICE_FILE_F2
         FUEL_PRICE_FILE_F2 = FUEL_PRICE_F2
      RETURN
      ENTRY FUEL_PRICE_FILE_F3
         FUEL_PRICE_FILE_F3 = FUEL_PRICE_F3
      RETURN
      ENTRY FUEL_PRICE_FILE_F4
         FUEL_PRICE_FILE_F4 = FUEL_PRICE_F4
      RETURN
      ENTRY FUEL_PRICE_FILE_F5
         FUEL_PRICE_FILE_F5 = FUEL_PRICE_F5
      RETURN
      ENTRY FUEL_PRICE_FILE_F6
         FUEL_PRICE_FILE_F6 = FUEL_PRICE_F6
      RETURN
      ENTRY FUEL_PRICE_FILE_F7
         FUEL_PRICE_FILE_F7 = FUEL_PRICE_F7
      RETURN
      ENTRY FUEL_PRICE_FILE_F8
         FUEL_PRICE_FILE_F8 = FUEL_PRICE_F8
      RETURN
      ENTRY FUEL_PRICE_FILE_F9
         FUEL_PRICE_FILE_F9 = FUEL_PRICE_F9
      RETURN
      ENTRY FUEL_PRICE_FILE_F0
         FUEL_PRICE_FILE_F0 = FUEL_PRICE_F0
      RETURN
       ENTRY CAP_ENRG_TRANSACTIONS_FILE
         CAP_ENRG_TRANSACTIONS_FILE = SERVICE_TRANSACTIONS_FILE
      RETURN
       ENTRY FUEL_EMISSIONS_FILE
         FUEL_EMISSIONS_FILE = FUEL_EMISSIONS_DATA_FILE
      RETURN
      ENTRY OVER_UNDER_TABLE_FILE_NAME
         OVER_UNDER_TABLE_FILE_NAME = trim(DIRECTORY_FOR_BASE_FILES)//   &
                      'DAB'//trim(OVER_UNDER_DECISIONS_TABLES)//'.DAT'
      RETURN
      ENTRY ASSET_SWITCHES_BASE_FILE
         ASSET_SWITCHES_BASE_FILE = ASSET_CLASS_RUN_SWITCHES
      RETURN
      ENTRY CLASS_SALES_BASE_FILE
         CLASS_SALES_BASE_FILE = ASSET_CLASS_SALES_FORECAST
      RETURN
      ENTRY ELIM_FILE
         ELIM_FILE = ASSET_CLASS_ELIMINATIONS
      RETURN
!

      ENTRY REFERENCE_LOAD_FILE
         REFERENCE_LOAD_FILE = BASE_YEAR_LOAD
      RETURN
      ENTRY WVPA_RATE_STRUCTURES_FILE
         WVPA_RATE_STRUCTURES_FILE = WVPA_RATES_STRUCTURES
      RETURN
      ENTRY WVPA_ADJUSTMENT_TRACKER_FILE
         WVPA_ADJUSTMENT_TRACKER_FILE = WVPA_TRACKER_FILE  ! 192
      RETURN
      ENTRY WVPA_COOP_REV_FORECAST_FILE
         WVPA_COOP_REV_FORECAST_FILE = WVPA_COOP_FORECAST_FILE  ! 193
      RETURN
      ENTRY WVPA_ACTUAL_PURCHASES_FILE
         WVPA_ACTUAL_PURCHASES_FILE = TRIM(DIRECTORY_FOR_BASE_FILES)//   &
                              'APB'//TRIM(WVPA_ACTUAL_PURCHASES)//'.DAT'
      RETURN
      ENTRY ProSym_Interface_File
         ProSym_Interface_File = ProSymBaseDataFile
      RETURN

      ENTRY IPALCO_ACTUAL_REGULATED_REVENUES_FILE
         IPALCO_ACTUAL_REGULATED_REVENUES_FILE =   &
                   TRIM(DIRECTORY_FOR_BASE_FILES)//   &
                              'ILB'//TRIM(WVPA_ACTUAL_PURCHASES)//'.DAT'
      RETURN
      ENTRY RETURN_BASE_FILE_NAME(FILE_NUMBER)
         RETURN_BASE_FILE_NAME = BASE_FILE_NAMES(FILE_NUMBER)
      RETURN

      ENTRY STORE_BASE_FILE_DIRECTORY(S_BASE_FILE_DIRECTORY,   &
                                      S_RESULTS_DIRECTORY,   &
                                      S_SCENAME,   &
                                      S_LDE_FILE_DIRECTORY,   &
                                      S_PRB_FILE_DIRECTORY,   &
                                      S_SHB_FILE_DIRECTORY,   &
                                      S_XML_FILE_DIRECTORY,   &
                                      S_LDG_FILE_DIRECTORY)

         DIRECTORY_FOR_BASE_FILES = S_BASE_FILE_DIRECTORY
         DIRECTORY_FOR_RESULT_FILES = S_RESULTS_DIRECTORY
         DIRECTORY_FOR_LDE_FILES = S_LDE_FILE_DIRECTORY
         DIRECTORY_FOR_PRB_FILES = S_PRB_FILE_DIRECTORY
         DIRECTORY_FOR_SHB_FILES = S_SHB_FILE_DIRECTORY
         DIRECTORY_FOR_XML_FILES = S_XML_FILE_DIRECTORY
         DIRECTORY_FOR_LDG_FILES = S_LDG_FILE_DIRECTORY
         STUDY_NAME = S_SCENAME
         STORE_BASE_FILE_DIRECTORY = 'T'
      RETURN

      ENTRY BASE_FILE_DIRECTORY

         BASE_FILE_DIRECTORY = DIRECTORY_FOR_BASE_FILES
      RETURN

      ENTRY LDE_FILE_DIRECTORY

         LDE_FILE_DIRECTORY = DIRECTORY_FOR_LDE_FILES
      RETURN

      ENTRY LDG_FILE_DIRECTORY

         LDG_FILE_DIRECTORY = DIRECTORY_FOR_LDG_FILES
      RETURN

      ENTRY PRB_FILE_DIRECTORY

         PRB_FILE_DIRECTORY = DIRECTORY_FOR_PRB_FILES
      RETURN

      ENTRY SHB_FILE_DIRECTORY

         SHB_FILE_DIRECTORY = DIRECTORY_FOR_SHB_FILES
      RETURN

      ENTRY XML_FILE_DIRECTORY

         XML_FILE_DIRECTORY = DIRECTORY_FOR_XML_FILES
      RETURN



      ENTRY PROJECT_NAME

         PROJECT_NAME = PROJECT_NAME_SAVE
      RETURN

      ENTRY GET_SCENAME

        GET_SCENAME = STUDY_NAME
      RETURN

      ENTRY GET_RESULTS_DIRECTORY

         GET_RESULTS_DIRECTORY = DIRECTORY_FOR_RESULT_FILES
      RETURN

      ENTRY SOFTWARE_DIRECTORY

         CALL MyName(PathName) ! path & name of current .exe-file
         POSbs = INDEX(PathName,'\',.TRUE.)
         SOFTWARE_DIRECTORY = PathName(1:POSbs)
      RETURN
      END


      FUNCTION RUN_FINASSET()

      LOGICAL (kind=1) ::  RUN_FINASSET,RUN_FINANCE
      CHARACTER (len=5) ::  EXASTFIL,DEBITFIL,DEBTFIL,   &
                  NUCFLFIL,EXPENFIL,ASSET_RATE_FILE,   &
                  ASSET_VECTOR_FILE,PARMFIN,INITFIL,TAXES_FILE,   &
                  FILE_NAMES(0:5)
      CHARACTER (len=1) ::  FUASTFIL
      CHARACTER (len=4) ::  NONE
      PARAMETER(NONE='NONE')
!
         RUN_FINASSET = INDEX(EXASTFIL(),NONE) == 0 .OR.   &
                        INDEX(DEBITFIL(),NONE) == 0 .OR.   &
                        INDEX(DEBTFIL(),NONE) == 0  .OR.   &
                        FUASTFIL(FILE_NAMES) == 'T' .OR.   &
                        INDEX(NUCFLFIL(),NONE) == 0 .OR.   &
                        INDEX(EXPENFIL(FILE_NAMES),NONE) == 0 .OR.   &
                        INDEX(ASSET_RATE_FILE(),NONE) == 0 .OR.   &
                        INDEX(ASSET_VECTOR_FILE(),NONE) == 0
      RETURN
      ENTRY RUN_FINANCE()
         RUN_FINANCE =  INDEX(PARMFIN(),NONE) == 0 .OR.   &
                        INDEX(INITFIL(),NONE) == 0 .OR.   &
                        INDEX(TAXES_FILE(),NONE) == 0
      RETURN
      END
!
!
