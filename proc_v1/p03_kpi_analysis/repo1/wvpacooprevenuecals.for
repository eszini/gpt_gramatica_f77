
!     WVPACoopRevenueCals.FOR
!     Copyright(c) M.S. Gerber & Associates 2000

!     Created: 5/27/2003 5:11:46 PM
!     Author : MARK S GERBER
!     Last change: MSG 1/10/2010 2:53:46 PM



      RECURSIVE SUBROUTINE INIT_WVPA_SALES_REVENUE_BY_COOP

      use paramscom
      use spindriftlib
      use prod_arrays_dimensions
      use irec_endpoint_control
      use grx_planning_routines
      use sizecom
      use globecom
      use class_run_switchesc
      use class_run_switchesl1
      use class_run_switchesl4
      use mthnmcom
	  use namescom
      use mod_base_year
      use endpoint
      use namescom
      
      SAVE
      
      
!
! WVPA ADDED VARIABLES
!
      REAL(kind=4) :: FUEL_ADJ_RATES(12),   


     +       NONFUEL_RATE(12), 
     +       SCR_RATES(12),
     +       NON_FUEL_BASE_RATE(12),
     +       FUEL_RATE_IN_ENRG_RATE(12),
     +       DEMAND_ADJUSTMENTS(12),
     +       ENERGY_ADJUSTMENTS(12)
      CHARACTER(len=1) :: ENERGY_ADJUSTMENT_METHOD
      CHARACTER(len=1) :: DEMAND_ADJUSTMENT_METHOD  
      CHARACTER(len=1) :: DEMAND_PRICING_SWITCH,
     +            ACCOUNT_ACTIVE(:)
      INTEGER(kind=4) :: RATE_CODE(:) 
      CHARACTER(len=10) :: RATE_TRACKER(:),
     +             RATE_OPTION(:)
      LOGICAL(kind=1) :: RETURN_WVPA_RATES
!
!
      CHARACTER(len=30) :: COMMENT
      INTEGER(kind=2) :: ASSET_CLASS_POINTER(:),MAX_ASSET_CLASS_NUM,

     +          NUM_OF_ASSET_CLASSES,R_MONTH,RATE_YEAR
      ALLOCATABLE :: ASSET_CLASS_POINTER
      INTEGER(kind=2) :: LAG_MO,MO1
      INTEGER(kind=2) :: MO,I,IREC,R_YEAR
      INTEGER(kind=4) :: IOS
      CHARACTER(len=32) :: RATE_CLASS_NAME(:)
      REAL(kind=4) :: R_TOTAL_BASE_REVENUE,TOTAL_BASE_REVENUE
      REAL(kind=4) :: R_CLASS_REVENUES(0:*),ALLOCATED_REVENUES,
     +       TWO_YR_CASH_DISTRIBUTION(24),
     +       MONTHS_FORECAST_ACTIVE(24)
      REAL (KIND=4), ALLOCATABLE :: CLASS_ENERGY(:,:),
     +       CLASS_DEMAND(:,:),
     +       CLASS_CUSTOMERS(:,:),
     +       CUSTOMER_PRICE(:,:),
     +       MONTHLY_SALES_ENERGY(:,:)
      CHARACTER(LEN=4),DIMENSION(:),ALLOCATABLE :: ENERGY_ACTUAL_MONTH,  ! 76
     +                                             CAPACITY_ACTUAL_MONTH ! 77
      REAL(kind=4) :: ASSET_CLASS_LIST(:)
      REAL(kind=4) :: ASSET_ALLOCATION_LIST(:),ASSET_ALLOCATOR
      ALLOCATABLE :: ASSET_CLASS_LIST,ASSET_ALLOCATION_LIST
      REAL(kind=4) :: MARKET_ENERGY_PRICE,
     +     MARKET_DEMAND_PRICE,MARKET_CUSTOMER_PRICE,
     +     ENERGY_PRICE(12),
     +     DEMAND_PRICE(12),
     +     CUSTOMERS(12),
     +     MONTHLY_DOLLARS(0:12),MONTHLY_ENERGY(0:12),
     +     TRANS_ENERGY(0:12),
     +     TRANS_PEAK(0:12),
     +     TRANS_CUSTOMERS(0:12),
     +     TEMP_TRANS_ENERGY(0:12),
     +     TEMP_TRANS_PEAK(0:12),
     +     TEMP_TRANS_CUSTOMERS(0:12)
      REAL(kind=4) :: MONTHLY_ENERGY_DOLLARS(0:12),
     +       MONTHLY_CUSTOMER_DOLLARS(0:12),
     +       MONTHLY_DEMAND_DOLLARS(0:12),
     +       BILLING_DEMAND(0:12),
     +       MONTHLY_BASE_FUEL_DOLLARS(0:12),
     +       MONTHLY_FUEL_ADJ_DOLLARS(0:12),
     +       MONTHLY_NONFUEL_DOLLARS(0:12),
     +       MONTHLY_BASE_NONFUEL_DOLLARS(0:12),
     +       MONTHLY_SCR_DOLLARS(0:12),
     +       R_MONTHLY_VALUES(0:12)
      LOGICAL(kind=1) :: GET_TG_CG2_DATA,DATA_FOUND,TEMP_DATA_FOUND
      REAL(kind=4) :: ANNUAL_PEAK_DEMAND,ANNUAL_ENERGY
      INTEGER(kind=2) :: FILE_TABLES
      INTEGER(kind=2) :: ASSET_CLASS,CLASS_POINTER,SALES_UNIT=3458,
     +          DELETE,READ_ASSET_CLASS(:),
     +          ASSET_ALLOCATION_VECTOR(:)
      CHARACTER(len=1) :: DUMMY_TYPE
      CHARACTER(len=40) :: REVENUE_CLASSIFICATION(:),
     +             R_REVENUE_CLASSIFICATION
      CHARACTER(len=1) :: USE_TRANSACT_FILE ! 113
      INTEGER(kind=2), ALLOCATABLE :: TRANSACT_FORECAST_GROUP(:), ! 110
     +                          TRANSACT_CUSTOMER_GROUP(:) ! 111
      CHARACTER(len=3) :: ENERGY_UNITS
      CHARACTER(len=1) :: CASH_TREATMENT(:),
     +            R_CASH_TREATMENT
      CHARACTER(len=30) :: LAG_PERIOD(:),
     +             CASH_RECEIVABLE_PAYABLE(:)
      ALLOCATABLE :: RATE_CLASS_NAME,
     +               RATE_CODE, 
     +               RATE_TRACKER,
     +               RATE_OPTION,
     +               ACCOUNT_ACTIVE,
     +               READ_ASSET_CLASS,
     +               ASSET_ALLOCATION_VECTOR,
     +               REVENUE_CLASSIFICATION,
     +               CASH_TREATMENT,
     +               LAG_PERIOD,
     +               CASH_RECEIVABLE_PAYABLE

      REAL(kind=4) :: REVENUES_MONTHLY(:,:,:),


     +       CASH_AMOUNTS_RECEIVABLE(:,:,:),
     +       CASH_CARRY_OVER_RECEIVABLE(:,:,:),
     +       ENERGY_MONTHLY(:,:,:)
      ALLOCATABLE :: REVENUES_MONTHLY,
     +               CASH_AMOUNTS_RECEIVABLE,
     +               CASH_CARRY_OVER_RECEIVABLE,
     +               ENERGY_MONTHLY
      INTEGER(kind=2) :: RUN_YEAR
      INTEGER(kind=2) :: REV_TYPE
      REAL(kind=4) :: LAG_BY_MONTH(24),ANNUAL_CASH_TOTAL,
     +       TOTAL_BOOKED_AMOUNT,TOTAL_CASH_AMOUNT,
     +       ACCRUED_CASH_BY_MONTH(24)
     

! REVENUE SECTION

      INTEGER(kind=2) :: R_CLASS
      REAL(kind=4) :: R_ADJUSTMENT_CLAUSE_REVENUES,

     +     R_BASE_RATES_REVENUES,
     +     R_SECONDARY_SALES_REVENUES,
     +     R_OTHER_REVENUES,
     +     R_BTL_REVENUES,
     +     R_GAS_REVENUES,
     +     R_CAT_REVENUES,
     +     R_RESIDENTIAL_MWH,
     +     R_COMMERCIAL_MWH,
     +     R_INDUSTRIAL_MWH,
     +     R_PUBLIC_STREET_HIGHWAY_MWH,
     +     R_OTHER_PUBLIC_MWH,
     +     R_WHOLESALE_MWH,
     +     R_OTHER_MWH,
     +     R_GAS_ADJUSTMENT_CLAUSE_REVENUE,
     +     R_COMPETITIVE_SALES_REVENUE
      LOGICAL(kind=1) :: CLASS_LINKED_2_PARENT
!     
      INTEGER(kind=2) :: REPORTING_UNIT
      LOGICAL(kind=1) :: REPORT_HEADER_OPEN=.FALSE.
      CHARACTER(len=1) :: USE_BUDGET_FORECAST_IN(7)
! ADDITIONAL SECTION
      REAL, DIMENSION(:), ALLOCATABLE :: ENERGY_LOSS_FACTOR,
     +                                   DEMAND_LOSS_FACTOR
     

      INTEGER(kind=2) :: INCOME_STATEMENT_POSITION
!
      INTEGER(kind=2) :: ALLOCATION_VECTOR
      REAL(kind=4) :: ALLOCATION_VALUE(AVAIL_DATA_YEARS)
      LOGICAL(kind=1) :: VOID_LOGICAL,RETURN_ASSET_CLASS_LISTS,


     +          WVPA_STORE_TRACKER_INFO,
     +          WVPA_STORE_TRACKER_SALES_INFO
      REAL :: AVERAGE_ANNUAL_POWER_RATE,WVPA_QUARTERLY_SYSTEM_RATES,
     +     IN_FUEL_ADJUSTMENT_FACTOR,WVPA_IN_FUEL_ADJUSTMENT_CAL,
     +     FUEL_TRACKER_ADJ,WVPA_IN_FUEL_TRACKER_CALC,
     +     NON_FUEL_TRACKER_ADJ,WVPA_NON_FUEL_TRACKER_CALC,
     +     NON_FUEL_ADJUSTMENT_FACTOR,WVPA_NON_FUEL_ADJUSTMENT_CAL,
     +     PSCR_TRACKER_ADJ,WVPA_PSCR_TRACKER_CALUCLATION,
     +     PSCR_ADJUSTMENT_FACTOR,WVPA_PSCR_ADJUSTMENT_CAL

      INTEGER(kind=2) :: R_RUN_YEAR
      CHARACTER(len=30) :: R_LAG_PERIOD,R_CASH_RECEIVABLE_PAYABLE
      CHARACTER(len=40) :: LAGGED_PATTERN
      REAL(kind=4) :: R_MONTHLY_REVENUES(0:12),

     +       R_ASSET_ALLOCATOR,
     +       R_MONTHLY_ENERGY(0:12) 
      REAL(kind=4) :: MONTHLY_ALLOCATED_REVENUES(0:12),
     +       MONTHLY_ALLOCATED_ENERGY
      REAL(kind=4) :: CURRENT_AMOUNT,LAGGED_AMOUNT
      REAL(kind=4) :: R_REVENUE_ELIM
      REAL(kind=4) :: R_MONTHLY_EXPENSES(0:12),
     +       MONTHLY_ALLOCATED_EXPENSES(0:12)
      LOGICAL(kind=1) :: EXPENSE_TYPE_IS_CASH
      REAL(kind=4) :: R_EXPENSES_ELIM
      INTEGER(kind=2) :: PERIOD
      REAL(kind=4) :: R_RESIDENTIAL_REVENUES,
     +       R_COMMERCIAL_REVENUES,
     +       R_INDUSTRIAL_REVENUES,
     +       R_LIGHTING_REVENUES,
     +       R_BULK_POWER_REVENUES,
     +       R_CAPACITY_SALES,
     +       R_GOVERNMENT_SALES,
     +       R_ADJ_EXP

      REAL(kind=4) :: R_MON_CHANGE_ACCTS_RECEIVABLE(0:12),
     +       R_MON_CHANGE_ACCTS_PAYABLE(0:12)
      INTEGER(kind=2) :: DATA_POS,J
!
!
      INTEGER(kind=2) :: YR
      INTEGER(kind=2) :: WVPA_COOP_REVENUE_HEADER
!
      CHARACTER(len=6) :: SHORT_MONTH_NAMES
!
      CHARACTER(len=40) :: EXT_RATE_CLASS_NAME
      CHARACTER(len=20) :: VARIABLE_TYPE_NAME(0:18)
      REAL(kind=4) :: VARIABLE_VALUE(0:23)
      LOGICAL(kind=1) :: RC_RPT_HEADER_NOT_OPEN=.TRUE.
      INTEGER(kind=4) :: NEXT_REC=0
      INTEGER(kind=2) :: RC_UNIT_NO=0
      REAL(kind=4) :: THE_RATIO_OF_A_TO_B
      INTEGER(kind=2) :: GET_MONTH_NUMBER 
      INTEGER(kind=2) :: USE_ENERGY_DATA_MONTH,


     +          USE_DEMAND_DATA_MONTH
      ! Renamed. Was transfer_transact_analyst_results.
      LOGICAL (KIND=1) :: TRANSFER_TRANSACT_ANL_RESULTS
!     
! END DATA DECLARATIONS           


         MAX_ASSET_CLASS_NUM = -99
         CALL RETURN_WVPA_COOP_TABLE_NUM(FILE_TABLES)
         IF(FILE_TABLES > 0) THEN
            IF(ALLOCATED(ASSET_CLASS_LIST))
     +                          DEALLOCATE(ASSET_CLASS_LIST,
     +                                     ASSET_ALLOCATION_LIST,
     +                                     CLASS_ENERGY,
     +                                     CLASS_DEMAND,
     +                                     MONTHLY_SALES_ENERGY,
     +                                     CLASS_CUSTOMERS,
     +                                     CUSTOMER_PRICE,
     +                                     ENERGY_LOSS_FACTOR,
     +                                     DEMAND_LOSS_FACTOR,
     +                                     RATE_CLASS_NAME,
     +                                     RATE_CODE, 
     +                                     RATE_TRACKER,
     +                                     RATE_OPTION,
     +                                     ACCOUNT_ACTIVE,
     +                                     READ_ASSET_CLASS,
     +                                     ASSET_ALLOCATION_VECTOR,
     +                                     REVENUE_CLASSIFICATION,
     +                                     CASH_TREATMENT,
     +                                     LAG_PERIOD,
     +                                     ENERGY_ACTUAL_MONTH,  ! 76
     +                                     CAPACITY_ACTUAL_MONTH, ! 77
     +                                     TRANSACT_FORECAST_GROUP, ! 15
     +                                     TRANSACT_CUSTOMER_GROUP, ! 16
     +                                     CASH_RECEIVABLE_PAYABLE)
            ALLOCATE(ASSET_CLASS_LIST(AVAIL_DATA_YEARS),
     +               ASSET_ALLOCATION_LIST(AVAIL_DATA_YEARS),
     +               CLASS_ENERGY(0:12,FILE_TABLES),
     +               MONTHLY_SALES_ENERGY(0:12,FILE_TABLES),
     +               CLASS_DEMAND(0:12,FILE_TABLES),
     +               CLASS_CUSTOMERS(0:12,FILE_TABLES),
     +               CUSTOMER_PRICE(0:12,FILE_TABLES),
     +               ENERGY_LOSS_FACTOR(FILE_TABLES),
     +               DEMAND_LOSS_FACTOR(FILE_TABLES),
     +               RATE_CLASS_NAME(FILE_TABLES),
     +               RATE_CODE(FILE_TABLES), 
     +               RATE_TRACKER(FILE_TABLES),
     +               RATE_OPTION(FILE_TABLES),
     +               ACCOUNT_ACTIVE(FILE_TABLES),
     +               READ_ASSET_CLASS(FILE_TABLES),
     +               ASSET_ALLOCATION_VECTOR(FILE_TABLES),
     +               REVENUE_CLASSIFICATION(FILE_TABLES),
     +               CASH_TREATMENT(FILE_TABLES),
     +               LAG_PERIOD(FILE_TABLES),
     +               ENERGY_ACTUAL_MONTH(FILE_TABLES),  ! 76
     +               CAPACITY_ACTUAL_MONTH(FILE_TABLES), ! 77
     +               TRANSACT_FORECAST_GROUP(FILE_TABLES), ! 15
     +               TRANSACT_CUSTOMER_GROUP(FILE_TABLES), ! 16
     +               CASH_RECEIVABLE_PAYABLE(FILE_TABLES))

            CLASS_ENERGY = 0.
            MONTHLY_SALES_ENERGY = 0.
            CLASS_DEMAND = 0.
            CLASS_CUSTOMERS = 0.
            CUSTOMER_PRICE = 0.

! ASSET CLASS EXPENSE AND REVENUE INFORMATION

            CALL RETURN_NUM_WVPA_COOPS(NUM_OF_ASSET_CLASSES,
     +                                 MAX_ASSET_CLASS_NUM)
            IF(ALLOCATED(ASSET_CLASS_POINTER))
     +                                   DEALLOCATE(ASSET_CLASS_POINTER)
            IF(MAX_ASSET_CLASS_NUM > 0) THEN
               ALLOCATE(ASSET_CLASS_POINTER(MAX_ASSET_CLASS_NUM))
               CALL RETURN_WVPA_COOP_POINTERS(ASSET_CLASS_POINTER,
     +                                        MAX_ASSET_CLASS_NUM)
            ENDIF
            CALL REVENUE_FORECAST_DATABASE(NUM_OF_ASSET_CLASSES,
     +                                     MAX_ASSET_CLASS_NUM,
     +                                     ASSET_CLASS_POINTER)

! REVENUE SECTION

            IF(ALLOCATED(REVENUES_MONTHLY)) DEALLOCATE(REVENUES_MONTHLY,
     +                                          CASH_AMOUNTS_RECEIVABLE,
     +                                       CASH_CARRY_OVER_RECEIVABLE,
     +                                       ENERGY_MONTHLY)
            ALLOCATE(REVENUES_MONTHLY(0:12,-1:NUM_OF_ASSET_CLASSES,
     +                                                LAST_INCOME_LINE),
     +             CASH_AMOUNTS_RECEIVABLE(0:12,-1:NUM_OF_ASSET_CLASSES,
     +                                                LAST_INCOME_LINE),
     +               CASH_CARRY_OVER_RECEIVABLE(1:12,
     +                                          -1:NUM_OF_ASSET_CLASSES,
     +                                                LAST_INCOME_LINE),
     +               ENERGY_MONTHLY(0:12,-1:NUM_OF_ASSET_CLASSES,
     +                                                LAST_INCOME_LINE))

            REVENUES_MONTHLY = 0.
            CASH_AMOUNTS_RECEIVABLE = 0.

            ENERGY_MONTHLY = 0.

            CASH_CARRY_OVER_RECEIVABLE = 0.

            CALL OPEN_WVPA_COOP_SALES_FILE(SALES_UNIT)

! SET REPORTING TITLES

            VARIABLE_TYPE_NAME(0) = "Energy Dollars"
            VARIABLE_TYPE_NAME(1) = "Demand Dollars"
            VARIABLE_TYPE_NAME(2) = "Customer Dollars"
            VARIABLE_TYPE_NAME(3) = "Total Dollars"
            VARIABLE_TYPE_NAME(4) = "Energy Rate"
            VARIABLE_TYPE_NAME(5) = "Demand Rate"
            VARIABLE_TYPE_NAME(6) = "Customer Rate"
            VARIABLE_TYPE_NAME(7) = "Energy Amount"
            VARIABLE_TYPE_NAME(8) = "Demand"
            VARIABLE_TYPE_NAME(9) = "Customers"
            VARIABLE_TYPE_NAME(10) = "Billing Demand"
            VARIABLE_TYPE_NAME(11) = "Fuel Rate"
            VARIABLE_TYPE_NAME(12) = "Non-fuel Rate"
            VARIABLE_TYPE_NAME(13) = "SCR Rate"
            VARIABLE_TYPE_NAME(14) = "Fuel Dollars"
            VARIABLE_TYPE_NAME(15) = "Non-fuel Dollars"
            VARIABLE_TYPE_NAME(16) = "SCR Dollars"
            VARIABLE_TYPE_NAME(17) = "Base Non-fuel Dollars"
            VARIABLE_TYPE_NAME(18) = "Base Non-fuel Rate"
         ENDIF
      RETURN

      ENTRY CALCULATE_WVPA_COOP_REVENUES(R_YEAR,R_CLASS_REVENUES)

         IF(FILE_TABLES == 0) RETURN
         RUN_YEAR = MIN(AVAIL_DATA_YEARS,R_YEAR)

! set up report


         IF(RC_RPT_HEADER_NOT_OPEN) THEN
            RC_RPT_HEADER_NOT_OPEN = .FALSE.
            RC_UNIT_NO = WVPA_COOP_REVENUE_HEADER(NEXT_REC)
         ENDIF


! ZERO ARRAYS

         REVENUES_MONTHLY = 0.
         CASH_AMOUNTS_RECEIVABLE = 0.
         ENERGY_MONTHLY = 0.

! SHIFT MATRIX FOR SUBSTITUTING ACTUALS


         MONTHS_FORECAST_ACTIVE = 1.
         DO MO = 1, 12
            IF(.NOT. TRANSFER_TRANSACT_ANL_RESULTS(MO))
     +                                   MONTHS_FORECAST_ACTIVE(MO) = 0.
         ENDDO

         IF(RUN_YEAR /= 1) THEN
            DO ASSET_CLASS = -1, NUM_OF_ASSET_CLASSES
               DO REV_TYPE = 1, LAST_INCOME_LINE
                  DO MO = 1, 12 
                     CASH_AMOUNTS_RECEIVABLE(MO,ASSET_CLASS,REV_TYPE) =
     +                  CASH_CARRY_OVER_RECEIVABLE(MO,
     +                                             ASSET_CLASS,REV_TYPE)
                     CASH_CARRY_OVER_RECEIVABLE(MO,
     +                                        ASSET_CLASS,REV_TYPE) = 0.
                  ENDDO
               ENDDO
            ENDDO
         ENDIF

! READ SALES FILE


         IREC = RUN_YEAR - AVAIL_DATA_YEARS
         DO I = 1, FILE_TABLES
            IREC = IREC + AVAIL_DATA_YEARS
            READ(SALES_UNIT,REC=IREC,IOSTAT=IOS) DELETE,RATE_YEAR,
     +                               RATE_CLASS_NAME(I),
     +                               RATE_CODE(I), 
     +                               RATE_TRACKER(I),
     +                               RATE_OPTION(I),
     +                               ACCOUNT_ACTIVE(I),
     +                               READ_ASSET_CLASS(I),
     +                               ASSET_ALLOCATION_VECTOR(I),
     +                               REVENUE_CLASSIFICATION(I),
     +                               CASH_TREATMENT(I),
     +                               LAG_PERIOD(I),
     +                               CASH_RECEIVABLE_PAYABLE(I),
     +                               COMMENT,
     +                               USE_TRANSACT_FILE,
     +                               TRANSACT_FORECAST_GROUP(I), ! 15
     +                               TRANSACT_CUSTOMER_GROUP(I), ! 16
     +                               ENERGY_LOSS_FACTOR(I),
     +                               DEMAND_LOSS_FACTOR(I),
     +                               DEMAND_PRICING_SWITCH, ! 19
     +                               ENERGY_ADJUSTMENT_METHOD, ! 26
     +                               ENERGY_ADJUSTMENTS, ! 27-38
     +                               DEMAND_ADJUSTMENT_METHOD, ! 39
     +                               DEMAND_ADJUSTMENTS, ! 40-51
     +                               CUSTOMERS,               !52-63
     +                               CUSTOMER_PRICE(1:,I),    ! 64-75
     +                               ENERGY_ACTUAL_MONTH(I),  ! 76
     +                               CAPACITY_ACTUAL_MONTH(I) ! 77


            IF(IOS /= 0) EXIT
            IF(DELETE >= 8) ACCOUNT_ACTIVE(I) = 'N'
            IF(ACCOUNT_ACTIVE(I) == 'N') CYCLE
            DATA_FOUND = .FALSE.
            ANNUAL_PEAK_DEMAND = 0.
            ANNUAL_ENERGY = 0.
            IF(USE_TRANSACT_FILE == 'Y') THEN
               TRANS_ENERGY = 0.
               TRANS_PEAK = 0.
               TRANS_CUSTOMERS = 0.
               IF(TRANSACT_CUSTOMER_GROUP(I) < 0) THEN
                  ASSET_CLASS_LIST = -99.
                  CALL GET_ASSET_VAR(ABS(TRANSACT_CUSTOMER_GROUP),
     +                                   DUMMY_TYPE,
     +                                   ASSET_CLASS_LIST)
                  CLASS_POINTER = 1
                  DO
                     TRANSACT_CUSTOMER_GROUP =
     +                                   ASSET_CLASS_LIST(CLASS_POINTER)
                     TEMP_DATA_FOUND =
     +                       GET_TG_CG2_DATA(TRANSACT_FORECAST_GROUP(I),
     +                                       TRANSACT_CUSTOMER_GROUP(I),
     +                                       TEMP_TRANS_ENERGY,
     +                                       TEMP_TRANS_PEAK,
     +                                       TEMP_TRANS_CUSTOMERS)
                     DATA_FOUND = DATA_FOUND .OR. TEMP_DATA_FOUND
                     IF(TEMP_DATA_FOUND) THEN
!                        DO MO = 1, 12
                        TRANS_ENERGY(:) = TRANS_ENERGY(:)
     +                                    + TEMP_TRANS_ENERGY(:)
                        TRANS_PEAK(:) = TRANS_PEAK(:)
     +                                  + TEMP_TRANS_PEAK(:)
                        TRANS_CUSTOMERS(:) = TRANS_CUSTOMERS(:)
     +                                       + TEMP_TRANS_CUSTOMERS(:)
!                        ENDDO
                     ENDIF
                     CLASS_POINTER = CLASS_POINTER + 1
                     IF(CLASS_POINTER > AVAIL_DATA_YEARS) EXIT
                     IF(ASSET_CLASS_LIST(CLASS_POINTER) == 0. .OR.
     +                     ASSET_CLASS_LIST(CLASS_POINTER) == -99.) EXIT
                  ENDDO
               ELSE
                  DATA_FOUND=GET_TG_CG2_DATA(TRANSACT_FORECAST_GROUP(I),
     +                                       TRANSACT_CUSTOMER_GROUP(I),
     +                                       TRANS_ENERGY,
     +                                       TRANS_PEAK,
     +                                       TRANS_CUSTOMERS)
               ENDIF

! ADJUST THE DATA OR USE THE DATA IN THE COOP TABLES


               USE_ENERGY_DATA_MONTH = 
     +                          GET_MONTH_NUMBER(ENERGY_ACTUAL_MONTH(I))
               USE_DEMAND_DATA_MONTH =
     +                        GET_MONTH_NUMBER(CAPACITY_ACTUAL_MONTH(I))

               DO MO = 1, 12
                  IF(MO <= USE_ENERGY_DATA_MONTH) THEN
                     CLASS_ENERGY(MO,I) = ENERGY_ADJUSTMENTS(MO)
                  ELSEIF(ENERGY_ADJUSTMENT_METHOD == 'A') THEN
                     CLASS_ENERGY(MO,I) = TRANS_ENERGY(MO)
     +                                    + ENERGY_ADJUSTMENTS(MO)
                  ELSEIF(ENERGY_ADJUSTMENT_METHOD == 'P') THEN
                     CLASS_ENERGY(MO,I) = TRANS_ENERGY(MO)
     +                                    * ENERGY_ADJUSTMENTS(MO)/100.
                  ELSEIF(ENERGY_ADJUSTMENT_METHOD == 'M') THEN
                     CLASS_ENERGY(MO,I) = TRANS_ENERGY(MO)
     +                                    * ENERGY_ADJUSTMENTS(MO)
                  ELSE
                     CLASS_ENERGY(MO,I) = TRANS_ENERGY(MO)
                  ENDIF
                  ANNUAL_ENERGY = ANNUAL_ENERGY + CLASS_ENERGY(MO,I)
                  IF(MO <= USE_DEMAND_DATA_MONTH) THEN
                     CLASS_DEMAND(MO,I) = DEMAND_ADJUSTMENTS(MO)
                  ELSEIF(DEMAND_ADJUSTMENT_METHOD == 'A') THEN
                     CLASS_DEMAND(MO,I) = TRANS_PEAK(MO)
     +                                    + DEMAND_ADJUSTMENTS(MO)
                  ELSEIF(DEMAND_ADJUSTMENT_METHOD == 'P') THEN
                     CLASS_DEMAND(MO,I) = TRANS_PEAK(MO)
     +                                    * DEMAND_ADJUSTMENTS(MO)/100.
                  ELSEIF(DEMAND_ADJUSTMENT_METHOD == 'M') THEN
                     CLASS_DEMAND(MO,I) = TRANS_PEAK(MO)
     +                                    * DEMAND_ADJUSTMENTS(MO)
                  ELSE
                     CLASS_DEMAND(MO,I) = TRANS_PEAK(MO)
                  ENDIF
                  ANNUAL_PEAK_DEMAND = MAX(ANNUAL_PEAK_DEMAND,
     +                                               CLASS_DEMAND(MO,I))
                  CLASS_CUSTOMERS(MO,I) = CUSTOMERS(MO)
               ENDDO

            ELSE
               DO MO = 1, 12
                  IF(ENERGY_ADJUSTMENTS(MO) < 1. .AND. RUN_YEAR >1 .AND.
     +                                ENERGY_ADJUSTMENTS(MO) /= 0.) THEN
                     CLASS_ENERGY(MO,I) = CLASS_ENERGY(MO,I) *
     +                                       (1.+ENERGY_ADJUSTMENTS(MO))
                  ELSE
                     CLASS_ENERGY(MO,I) = ENERGY_ADJUSTMENTS(MO) ! /1000.
                  ENDIF
                  ANNUAL_ENERGY = ANNUAL_ENERGY + CLASS_ENERGY(MO,I)
                  IF(DEMAND_ADJUSTMENTS(MO) < 1. .AND. RUN_YEAR >1 .AND.
     +                                DEMAND_ADJUSTMENTS(MO) /= 0.) THEN
                     CLASS_DEMAND(MO,I) = CLASS_DEMAND(MO,I) *
     +                                       (1.+DEMAND_ADJUSTMENTS(MO))
                  ELSE
                     CLASS_DEMAND(MO,I) = DEMAND_ADJUSTMENTS(MO)
                  ENDIF
                  ANNUAL_PEAK_DEMAND = MAX(ANNUAL_PEAK_DEMAND,
     +                                               CLASS_DEMAND(MO,I))
                  IF(CUSTOMERS(MO) < 1. .AND. RUN_YEAR > 1) THEN
                     CLASS_CUSTOMERS(MO,I) = CLASS_CUSTOMERS(MO,I) *
     +                                                (1.+CUSTOMERS(MO))
                  ELSE
                     CLASS_CUSTOMERS(MO,I) = CUSTOMERS(MO)
                  ENDIF
               ENDDO
            ENDIF
            CLASS_ENERGY(0,I) = SUM(CLASS_ENERGY(1:,I))
            CLASS_DEMAND(0,I) = SUM(CLASS_DEMAND(1:,I))
            IF(ENERGY_LOSS_FACTOR(I) < 100.) THEN
               MONTHLY_SALES_ENERGY(:,I) = CLASS_ENERGY(:,I) * 
     +                                   (1.-ENERGY_LOSS_FACTOR(I)/100.)
               VOID_LOGICAL = WVPA_STORE_TRACKER_SALES_INFO(
     +                                        RATE_TRACKER(I),
     +                                        RATE_CODE(I), 
     +                                        MONTHLY_SALES_ENERGY(0,I))
            ENDIF
         ENDDO ! REVENUE FORCAST TABLES

! CALCULATE QUARTERLY POWER RATES

         AVERAGE_ANNUAL_POWER_RATE=WVPA_QUARTERLY_SYSTEM_RATES(RUN_YEAR)
         DO MO = 1, 12
            IF(MOD(MO,int(3,2)) == 1) THEN  ! UPDATE ADJ FACTORS
               IN_FUEL_ADJUSTMENT_FACTOR =
     +                                   WVPA_IN_FUEL_ADJUSTMENT_CAL(MO)
               NON_FUEL_ADJUSTMENT_FACTOR =
     +                                  WVPA_NON_FUEL_ADJUSTMENT_CAL(MO)
            ENDIF 
            DO I = 1, FILE_TABLES
               IF(ACCOUNT_ACTIVE(I) == 'N') CYCLE
               DATA_FOUND  = RETURN_WVPA_RATES(RATE_CODE(I),
     +                                         ENERGY_PRICE,   
     +                                         DEMAND_PRICE, 
     +                                         FUEL_ADJ_RATES,   
     +                                         NONFUEL_RATE, 
     +                                         SCR_RATES,
     +                                         NON_FUEL_BASE_RATE,
     +                                         FUEL_RATE_IN_ENRG_RATE)
               IF(.NOT. DATA_FOUND) CYCLE
               IF(ENERGY_LOSS_FACTOR(I) < 100.) THEN
                  MONTHLY_ENERGY(MO) = CLASS_ENERGY(MO,I) * 
     +                                   (1.-ENERGY_LOSS_FACTOR(I)/100.)
               ELSE
                  MONTHLY_ENERGY(MO) = 0.
               ENDIF
               IF (DEMAND_LOSS_FACTOR(I) < 100.) THEN
                  CLASS_DEMAND(MO,I) = CLASS_DEMAND(MO,I) * 
     +                                 (1. - DEMAND_LOSS_FACTOR(I)/100.)
               ELSE
                  CLASS_DEMAND(MO,I) = 0.
               ENDIF
               MONTHLY_ENERGY_DOLLARS(MO) =
     +                         ENERGY_PRICE(MO)*MONTHLY_ENERGY(MO)/1000.
               MONTHLY_BASE_FUEL_DOLLARS(MO) =
     +                                   FUEL_RATE_IN_ENRG_RATE(MO) *
     +                                          MONTHLY_ENERGY(MO)/1000.
!               IF(RATE_CODE(I) > 200000 .AND. RATE_CODE(I) < 300000)THEN ! was for pscs 5/5/04
!                  MONTHLY_FUEL_ADJ_DOLLARS(MO) = 0.
!               ELSE
                  MONTHLY_FUEL_ADJ_DOLLARS(MO) =
     +                       FUEL_ADJ_RATES(MO)*MONTHLY_ENERGY(MO)/1000.
!               ENDIF
               MONTHLY_NONFUEL_DOLLARS(MO) =
     +                         NONFUEL_RATE(MO)*MONTHLY_ENERGY(MO)/1000.
               MONTHLY_BASE_NONFUEL_DOLLARS(MO) =
     +                   NON_FUEL_BASE_RATE(MO)*MONTHLY_ENERGY(MO)/1000.
               MONTHLY_SCR_DOLLARS(MO) =
     +                            SCR_RATES(MO)*MONTHLY_ENERGY(MO)/1000.
               IF(DEMAND_PRICING_SWITCH == 'M') THEN
                  MONTHLY_DEMAND_DOLLARS(MO) =
     +                       + DEMAND_PRICE(MO)*CLASS_DEMAND(MO,I)/1000.
                  BILLING_DEMAND(MO) = CLASS_DEMAND(MO,I)
               ELSE
                  MONTHLY_DEMAND_DOLLARS(MO) =
     +                       + DEMAND_PRICE(MO)*ANNUAL_PEAK_DEMAND/1000.
                  BILLING_DEMAND(MO) = ANNUAL_PEAK_DEMAND
               ENDIF

! STORE TRACKER INFO


               VOID_LOGICAL = WVPA_STORE_TRACKER_INFO(MO,
     +                                 RATE_TRACKER(I),
     +                                 RATE_CODE(I),
     +                                 MONTHLY_BASE_FUEL_DOLLARS(MO),
     +                                 MONTHLY_FUEL_ADJ_DOLLARS(MO),
     +                                 MONTHLY_NONFUEL_DOLLARS(MO),
     +                                 MONTHLY_BASE_NONFUEL_DOLLARS(MO),
     +                                 MONTHLY_SCR_DOLLARS(MO))
            ENDDO
            FUEL_TRACKER_ADJ = WVPA_IN_FUEL_TRACKER_CALC(MO)
            NON_FUEL_TRACKER_ADJ = WVPA_NON_FUEL_TRACKER_CALC(MO)
         ENDDO

! PSCR TRACKIER


         PSCR_ADJUSTMENT_FACTOR = WVPA_PSCR_ADJUSTMENT_CAL()






! CALCULATE REVENUES BY CUSTOMER FOR REPORTING AND ASSET CLASS NEEDS


         DO I = 1, FILE_TABLES
            IF(ACCOUNT_ACTIVE(I) == 'N') CYCLE
            DATA_FOUND  = RETURN_WVPA_RATES(RATE_CODE(I),
     +                                         ENERGY_PRICE,   
     +                                         DEMAND_PRICE, 
     +                                         FUEL_ADJ_RATES,   
     +                                         NONFUEL_RATE, 
     +                                         SCR_RATES,
     +                                         NON_FUEL_BASE_RATE,
     +                                         FUEL_RATE_IN_ENRG_RATE)
            IF(.NOT. DATA_FOUND) THEN
               ENERGY_PRICE = 0.   
               DEMAND_PRICE = 0. 
               FUEL_ADJ_RATES = 0.
               NONFUEL_RATE = 0.
               NON_FUEL_BASE_RATE = 0.
               SCR_RATES = 0.
            ENDIF
            DO MO = 1, 12
               IF(ENERGY_LOSS_FACTOR(I) < 100.) THEN
                  MONTHLY_ENERGY(MO) = CLASS_ENERGY(MO,I) * 
     +                                   (1.-ENERGY_LOSS_FACTOR(I)/100.)
               ELSE
                  MONTHLY_ENERGY(MO) = 0.
               ENDIF
               MONTHLY_ENERGY_DOLLARS(MO) =
     +                         ENERGY_PRICE(MO)*MONTHLY_ENERGY(MO)/1000.
               MONTHLY_BASE_FUEL_DOLLARS(MO) =
     +                                   FUEL_RATE_IN_ENRG_RATE(MO) *
     +                                          MONTHLY_ENERGY(MO)/1000.
               MONTHLY_FUEL_ADJ_DOLLARS(MO) =
     +                       FUEL_ADJ_RATES(MO)*MONTHLY_ENERGY(MO)/1000.
               MONTHLY_NONFUEL_DOLLARS(MO) =
     +                         NONFUEL_RATE(MO)*MONTHLY_ENERGY(MO)/1000.
               MONTHLY_BASE_NONFUEL_DOLLARS(MO) =
     +                   NON_FUEL_BASE_RATE(MO)*MONTHLY_ENERGY(MO)/1000.
               MONTHLY_SCR_DOLLARS(MO) =
     +                            SCR_RATES(MO)*MONTHLY_ENERGY(MO)/1000.
               MONTHLY_CUSTOMER_DOLLARS(MO) =
     +                        CUSTOMER_PRICE(MO,I)*CLASS_CUSTOMERS(MO,I)
               IF(DEMAND_PRICING_SWITCH == 'M') THEN
                  MONTHLY_DEMAND_DOLLARS(MO) =
     +                       + DEMAND_PRICE(MO)*CLASS_DEMAND(MO,I)/1000.
                  BILLING_DEMAND(MO) = CLASS_DEMAND(MO,I)
               ELSE
                  MONTHLY_DEMAND_DOLLARS(MO) =
     +                       + DEMAND_PRICE(MO)*ANNUAL_PEAK_DEMAND/1000.
                  BILLING_DEMAND(MO) = ANNUAL_PEAK_DEMAND
               ENDIF
!               VOID_LOGICAL = WVPA_STORE_TRACKER_INFO(MO,
!     +                                 RATE_TRACKER(I),
!     +                                 RATE_CODE(I),
!     +                                 MONTHLY_BASE_FUEL_DOLLARS(MO),
!     +                                 MONTHLY_FUEL_ADJ_DOLLARS(MO),
!     +                                 MONTHLY_NONFUEL_DOLLARS(MO),
!     +                                 MONTHLY_BASE_NONFUEL_DOLLARS(MO),
!     +                                 MONTHLY_SCR_DOLLARS(MO))
            ENDDO
!            MONTHLY_DOLLARS(0) = SUM(MONTHLY_DOLLARS(1:))
            MONTHLY_ENERGY_DOLLARS(0) =SUM(MONTHLY_ENERGY_DOLLARS(1:))
            MONTHLY_DEMAND_DOLLARS(0) = SUM(MONTHLY_DEMAND_DOLLARS(1:))
            MONTHLY_BASE_FUEL_DOLLARS(0) =
     +                                SUM(MONTHLY_BASE_FUEL_DOLLARS(1:))
            MONTHLY_FUEL_ADJ_DOLLARS(0) =
     +                                 SUM(MONTHLY_FUEL_ADJ_DOLLARS(1:))
            MONTHLY_NONFUEL_DOLLARS(0) =
     +                                  SUM(MONTHLY_NONFUEL_DOLLARS(1:))
            MONTHLY_BASE_NONFUEL_DOLLARS(0) =
     +                             SUM(MONTHLY_BASE_NONFUEL_DOLLARS(1:))
            MONTHLY_SCR_DOLLARS(0) = SUM(MONTHLY_SCR_DOLLARS(1:))
            MONTHLY_CUSTOMER_DOLLARS(0) =
     +                                 SUM(MONTHLY_CUSTOMER_DOLLARS(1:))
            BILLING_DEMAND(0) = SUM(BILLING_DEMAND(1:))
            CLASS_DEMAND(0,I) = SUM(CLASS_DEMAND(1:,I))
            CLASS_CUSTOMERS(0,I) = SUM(CLASS_CUSTOMERS(1:,I))
            MONTHLY_DOLLARS = MONTHLY_ENERGY_DOLLARS
     +                        + MONTHLY_DEMAND_DOLLARS
     +                        + MONTHLY_CUSTOMER_DOLLARS
     +                        + MONTHLY_FUEL_ADJ_DOLLARS
     +                        + MONTHLY_NONFUEL_DOLLARS
     +                        + MONTHLY_SCR_DOLLARS

            VOID_LOGICAL = RETURN_ASSET_CLASS_LISTS(READ_ASSET_CLASS(I),
     +                                       ASSET_CLASS_LIST,
     +                                       ASSET_ALLOCATION_VECTOR(I),
     +                                       ASSET_ALLOCATION_LIST)
            CLASS_POINTER = 1
            DO
               ASSET_CLASS = ASSET_CLASS_LIST(CLASS_POINTER)
               CALL CHECK_IF_CLASS_DEFINED(ASSET_CLASS)
               ASSET_CLASS = ASSET_CLASS + 1
               IF(ASSET_CLASS > 0) ASSET_CLASS = 
     +                                  ASSET_CLASS_POINTER(ASSET_CLASS)
               ASSET_ALLOCATOR = ASSET_ALLOCATION_LIST(CLASS_POINTER)
               IF(ASSET_ALLOCATOR < 0.) THEN
                  ALLOCATION_VECTOR = ABS(ASSET_ALLOCATOR)
                  CALL GET_ASSET_VAR(ALLOCATION_VECTOR,
     +                                      DUMMY_TYPE,ALLOCATION_VALUE)
                  ASSET_ALLOCATOR = ALLOCATION_VALUE(RUN_YEAR)
               ENDIF

               ASSET_ALLOCATOR = ASSET_ALLOCATOR/100.


               IF(ASSET_CLASS <= MAX_ASSET_CLASS_NUM) THEN
                  CALL WVPA_REVENUE_ALLOCATION(ASSET_CLASS,
     +                                       MONTHLY_DOLLARS,
     +                                       MONTHLY_ENERGY,
     +                                       ASSET_ALLOCATOR,
     +                                       REVENUE_CLASSIFICATION(I),
     +                                       RUN_YEAR,
     +                                       CASH_TREATMENT(I),
     +                                       LAG_PERIOD(I),
     +                                       CASH_RECEIVABLE_PAYABLE(I))
!                  
                  ALLOCATED_REVENUES = ASSET_ALLOCATOR *
     +                                                MONTHLY_DOLLARS(0)
                  R_CLASS_REVENUES(ASSET_CLASS) = ALLOCATED_REVENUES
     +                                   + R_CLASS_REVENUES(ASSET_CLASS)
                  R_CLASS_REVENUES(0) = R_CLASS_REVENUES(0) +
     +                                                ALLOCATED_REVENUES
               ENDIF
               CLASS_POINTER = CLASS_POINTER + 1
               IF(CLASS_POINTER > AVAIL_DATA_YEARS) EXIT
               IF(ASSET_CLASS_LIST(CLASS_POINTER) == 0. .OR.
     +                     ASSET_CLASS_LIST(CLASS_POINTER) == -99.) EXIT
            ENDDO

! WRITE OUTPUT

            IF(.TRUE.) THEN
               WRITE(EXT_RATE_CLASS_NAME,'(I6)') RATE_CODE(I)
               EXT_RATE_CLASS_NAME = trim(RATE_CLASS_NAME(I))//'  '//
     +                                               EXT_RATE_CLASS_NAME
               MONTHLY_ENERGY(0) = SUM(MONTHLY_ENERGY(1:))
               DO MO = 0, 12
                  VARIABLE_VALUE(0) = MONTHLY_ENERGY_DOLLARS(MO)
                  VARIABLE_VALUE(1) = MONTHLY_DEMAND_DOLLARS(MO)
                  VARIABLE_VALUE(2) = MONTHLY_CUSTOMER_DOLLARS(MO)
                  VARIABLE_VALUE(14) = MONTHLY_FUEL_ADJ_DOLLARS(MO)
                  VARIABLE_VALUE(15) = MONTHLY_NONFUEL_DOLLARS(MO)
                  VARIABLE_VALUE(16) = MONTHLY_SCR_DOLLARS(MO)
                  VARIABLE_VALUE(17) = MONTHLY_BASE_NONFUEL_DOLLARS(MO)
                  VARIABLE_VALUE(3) = MONTHLY_DOLLARS(MO)
                  VARIABLE_VALUE(7) = MONTHLY_ENERGY(MO)/1000.
                  VARIABLE_VALUE(20) = MONTHLY_BASE_FUEL_DOLLARS(MO)
                  VARIABLE_VALUE(21) = RATE_CODE(I)
                  VARIABLE_VALUE(22) = TRANSACT_CUSTOMER_GROUP(I)
                  VARIABLE_VALUE(23) = TRANSACT_FORECAST_GROUP(I)
                  IF(MO == 0) THEN
                     VARIABLE_VALUE(4) = THE_RATIO_OF_A_TO_B(
     +                  MONTHLY_ENERGY_DOLLARS(MO),VARIABLE_VALUE(7))
                     VARIABLE_VALUE(5) = 1000.*THE_RATIO_OF_A_TO_B(
     +                    MONTHLY_DEMAND_DOLLARS(MO),BILLING_DEMAND(MO))
                     VARIABLE_VALUE(6) = THE_RATIO_OF_A_TO_B(
     +                      MONTHLY_CUSTOMER_DOLLARS(MO),
     +                                            CLASS_CUSTOMERS(MO,I))
                     VARIABLE_VALUE(11) = THE_RATIO_OF_A_TO_B(
     +                   MONTHLY_FUEL_ADJ_DOLLARS(MO),VARIABLE_VALUE(7))
                     VARIABLE_VALUE(19) = THE_RATIO_OF_A_TO_B( 
     +                  MONTHLY_BASE_FUEL_DOLLARS(MO),VARIABLE_VALUE(7))
                     VARIABLE_VALUE(12) = THE_RATIO_OF_A_TO_B(
     +                    MONTHLY_NONFUEL_DOLLARS(MO),VARIABLE_VALUE(7))
                     VARIABLE_VALUE(13) = THE_RATIO_OF_A_TO_B(
     +                        MONTHLY_SCR_DOLLARS(MO),VARIABLE_VALUE(7))
                     VARIABLE_VALUE(18) = THE_RATIO_OF_A_TO_B(
     +                                 MONTHLY_BASE_NONFUEL_DOLLARS(MO),
     +                                                VARIABLE_VALUE(7))
                     VARIABLE_VALUE(8) = CLASS_DEMAND(MO,I)/12.
                     VARIABLE_VALUE(9) = CLASS_CUSTOMERS(MO,I)/12.
                     VARIABLE_VALUE(10) = BILLING_DEMAND(MO)/12.
                  ELSE
                     VARIABLE_VALUE(4) = ENERGY_PRICE(MO)
                     VARIABLE_VALUE(5) = DEMAND_PRICE(MO)
                     VARIABLE_VALUE(6) = CUSTOMER_PRICE(MO,I)
                     VARIABLE_VALUE(8) = CLASS_DEMAND(MO,I)
                     VARIABLE_VALUE(9) = CLASS_CUSTOMERS(MO,I)
                     VARIABLE_VALUE(10) = BILLING_DEMAND(MO)
                     VARIABLE_VALUE(11) = FUEL_ADJ_RATES(MO)
                     VARIABLE_VALUE(12) = NONFUEL_RATE(MO)
                     VARIABLE_VALUE(13) = SCR_RATES(MO)
                     VARIABLE_VALUE(18) = NON_FUEL_BASE_RATE(MO)
                     VARIABLE_VALUE(19) = FUEL_RATE_IN_ENRG_RATE(MO)
                  ENDIF

                  WRITE(RC_UNIT_NO,REC=NEXT_REC) PRT_ENDPOINT(),
     +                                float(get_BASE_YEAR()+R_YEAR),
     +                                          EXT_RATE_CLASS_NAME,
     +                                          SHORT_MONTH_NAMES(MO),
     +                                          VARIABLE_VALUE
                  NEXT_REC = NEXT_REC + 1
               ENDDO
            ENDIF
         ENDDO ! READ RECORDS LOOP

! FINISH TRACKER

         PSCR_TRACKER_ADJ = WVPA_PSCR_TRACKER_CALUCLATION()

! SUM EACH CASH VARIABLE TO AN ANNUAL AMOUNT


         DO ASSET_CLASS = -1, NUM_OF_ASSET_CLASSES
            DO REV_TYPE = 1, LAST_INCOME_LINE
               CASH_AMOUNTS_RECEIVABLE(0,ASSET_CLASS,REV_TYPE) = 0.
               DO MO = 1, 12
                  CASH_AMOUNTS_RECEIVABLE(0,ASSET_CLASS,REV_TYPE) =
     +                CASH_AMOUNTS_RECEIVABLE(0,ASSET_CLASS,REV_TYPE)
     +                + CASH_AMOUNTS_RECEIVABLE(MO,ASSET_CLASS,REV_TYPE)
               ENDDO
            ENDDO
         ENDDO
         CALL SAVE_REVENUE_DATA_FORECASTS(NUM_OF_ASSET_CLASSES,
     +                                    REVENUES_MONTHLY,
     +                                    CASH_AMOUNTS_RECEIVABLE,
     +                                    ENERGY_MONTHLY)
      RETURN

      ENTRY WVPA_REVENUE_ALLOCATION(R_CLASS,
     +                              R_MONTHLY_REVENUES,
     +                              R_MONTHLY_ENERGY,
     +                              R_ASSET_ALLOCATOR,
     +                              R_REVENUE_CLASSIFICATION,
     +                              R_RUN_YEAR,
     +                              R_CASH_TREATMENT,
     +                              R_LAG_PERIOD,
     +                              R_CASH_RECEIVABLE_PAYABLE)

         REV_TYPE = INCOME_STATEMENT_POSITION(R_REVENUE_CLASSIFICATION)
         IF(REV_TYPE >= 1 .AND. REV_TYPE <= LAST_INCOME_LINE) THEN
            MONTHLY_ALLOCATED_REVENUES(1:) = R_ASSET_ALLOCATOR *
     +                                      R_MONTHLY_REVENUES(1:) *
     +                                      MONTHS_FORECAST_ACTIVE(1:12)
            ENERGY_MONTHLY(:,R_CLASS,REV_TYPE) =
     +                          ENERGY_MONTHLY(:,R_CLASS,REV_TYPE)
     +                          + R_MONTHLY_ENERGY(:)*R_ASSET_ALLOCATOR

            REVENUES_MONTHLY(1:12,R_CLASS,REV_TYPE) =
     +                         MONTHLY_ALLOCATED_REVENUES(1:12)
     +                         + REVENUES_MONTHLY(1:12,R_CLASS,REV_TYPE)
            REVENUES_MONTHLY(0,R_CLASS,REV_TYPE) =
     +                        SUM(REVENUES_MONTHLY(1:,R_CLASS,REV_TYPE))
            ACCRUED_CASH_BY_MONTH = 0.
            TWO_YR_CASH_DISTRIBUTION = 0.
            IF(R_RUN_YEAR == 1) THEN
               LAGGED_PATTERN = TRIM(R_CASH_RECEIVABLE_PAYABLE)//
     +                                 ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(LAGGED_PATTERN,*) ACCRUED_CASH_BY_MONTH

               TWO_YR_CASH_DISTRIBUTION = R_ASSET_ALLOCATOR
     +                                    * ACCRUED_CASH_BY_MONTH
     +                                    * MONTHS_FORECAST_ACTIVE

            ENDIF
            LAG_BY_MONTH = 0.
            IF(R_CASH_TREATMENT == 'B') THEN
               LAG_BY_MONTH(1) = 100.
            ELSE ! IF(R_CASH_TREATMENT == 'L') THEN
               LAGGED_PATTERN = 
     +               TRIM(R_LAG_PERIOD)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(LAGGED_PATTERN,*) LAG_BY_MONTH
            ENDIF
            IF(R_CASH_TREATMENT /= 'N') THEN
               MONTHLY_ALLOCATED_REVENUES(1:) = R_ASSET_ALLOCATOR
     +                                          * R_MONTHLY_REVENUES(1:)

               DO MO = 1, 12
                  LAG_MO = 1
                  DO MO1 = MO, 24
                     TWO_YR_CASH_DISTRIBUTION(MO1) = 
     +                                  TWO_YR_CASH_DISTRIBUTION(MO1) 
     +                                  + MONTHLY_ALLOCATED_REVENUES(MO)
     +                                    * MONTHS_FORECAST_ACTIVE(MO1)
     +                                       * LAG_BY_MONTH(LAG_MO)/100.
                     LAG_MO = LAG_MO + 1
                  ENDDO
               ENDDO

               CASH_AMOUNTS_RECEIVABLE(1:12,R_CLASS,REV_TYPE) =
     +                    CASH_AMOUNTS_RECEIVABLE(1:12,R_CLASS,REV_TYPE)
     +                    + TWO_YR_CASH_DISTRIBUTION(1:12)
     +
               CASH_AMOUNTS_RECEIVABLE(0,R_CLASS,REV_TYPE) =
     +                       CASH_AMOUNTS_RECEIVABLE(0,R_CLASS,REV_TYPE)
     +                       + SUM(TWO_YR_CASH_DISTRIBUTION(1:12))
! STORE END OF YEAR CARRY OVER
               CASH_CARRY_OVER_RECEIVABLE(1:12,R_CLASS,REV_TYPE) =
     +                 CASH_CARRY_OVER_RECEIVABLE(1:12,R_CLASS,REV_TYPE)
     +                 + TWO_YR_CASH_DISTRIBUTION(13:24)
            ENDIF
         ENDIF
      RETURN
      END

      SUBROUTINE REVENUE_FORECAST_DATABASE(R_NUM_OF_ASSET_CLASSES,
     +                                     R_MAX_ASSET_CLASS_NUM,
     +                                     R_ASSET_CLASS_POINTER)

      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM
      use globecom
      use class_run_switchesc
      use class_run_switchesl1
      use class_run_switchesl4
      use mthnmcom
      use paramscom
      use enrg_helper
      use namescom

      INTEGER(kind=2) :: NUM_OF_ASSET_CLASSES,R_NUM_OF_ASSET_CLASSES
      INTEGER(kind=2) :: MAX_ASSET_CLASS_NUM,R_MAX_ASSET_CLASS_NUM
      INTEGER(kind=2) :: R_ASSET_CLASS_POINTER(R_MAX_ASSET_CLASS_NUM)
      INTEGER(kind=2) :: R_MONTH,I,REV_TYPE,MO

      
      REAL (KIND=4), SAVE, ALLOCATABLE :: REVENUES_MONTHLY(:,:,:),
     +                                   EXPENSES_MONTHLY(:,:,:),
     +                                   CASH_AMOUNTS_RECEIVABLE(:,:,:),
     +                                   CASH_AMOUNTS_PAYABLE(:,:,:),
     +                                   ENERGY_MONTHLY(:,:,:)
      SAVE NUM_OF_ASSET_CLASSES,
     +     MAX_ASSET_CLASS_NUM
     
      REAL(kind=4) :: R_REVENUES_MONTHLY(0:12,-1:R_NUM_OF_ASSET_CLASSES,


     +                                                LAST_INCOME_LINE),
     +       R_CASH_AMOUNTS_RECEIVABLE(0:12,-1:R_NUM_OF_ASSET_CLASSES,
     +                                                LAST_INCOME_LINE),
     +       R_EXPENSES_MONTHLY(0:12,-1:R_NUM_OF_ASSET_CLASSES,
     +                                               LAST_EXPENSE_ITEM),
     +       R_CASH_AMOUNTS_PAYABLE(0:12,-1:R_NUM_OF_ASSET_CLASSES,
     +                                               LAST_EXPENSE_ITEM),
     +       R_ENERGY_MONTHLY(0:12,-1:R_NUM_OF_ASSET_CLASSES,
     +                                                LAST_INCOME_LINE),
     +       R_MONTHLY_VALUES(0:12),
     +       ENERGY_SALES_BY_CLASS(0:12,LAST_INCOME_LINE),
     +       REVENUES_BY_CLASS(0:12,LAST_INCOME_LINE)

! REVENUE SECTION

      INTEGER(kind=2) :: R_CLASS
      REAL(kind=4) :: R_ADJUSTMENT_CLAUSE_REVENUES,


     +     R_BASE_RATES_REVENUES,
     +     R_SECONDARY_SALES_REVENUES,
     +     R_OTHER_REVENUES,
     +     R_BTL_REVENUES,
     +     R_GAS_REVENUES,
     +     R_CAT_REVENUES,
     +     R_RESIDENTIAL_MWH,
     +     R_COMMERCIAL_MWH,
     +     R_INDUSTRIAL_MWH,
     +     R_PUBLIC_STREET_HIGHWAY_MWH,
     +     R_OTHER_PUBLIC_MWH,
     +     R_WHOLESALE_MWH,
     +     R_OTHER_MWH,
     +     R_GAS_ADJUSTMENT_CLAUSE_REVENUE,
     +     R_COMPETITIVE_SALES_REVENUE,
     +     FE_Competitive_Unit_Sales,
     +     FE_Intra_Company_Utility_Sales
      REAL(kind=4) :: R_COMPETITIVE_SALES(9),
     +       R_COMPETITIVE_SALES_QUANT(9),
     +       R_COMPETITIVE_LOSS(9)
      REAL(kind=4) :: R_FUEXP,R_PREXP,R_OPEXP,R_MNEXP,R_OTHER1,R_OTHER2,
     +     R_OTHER3,R_NFOWN,R_NFLEASE,
     +     R_DSM_EXPENSE,R_DSM_REBATE,R_BTL_EXPENSE,
     +     R_ATL_LEASE_EXP,R_BTL_LEASE_EXP,R_SERVICE_TRANSACTIONS
      REAL(kind=4) :: R_MONTHLY_REVENUES(0:12),R_ASSET_ALLOCATOR,
     +       R_MONTHLY_ENERGY(0:12) 
      REAL(kind=4) :: MONTHLY_ALLOCATED_REVENUES(0:12),
     +       MONTHLY_ALLOCATED_ENERGY
      REAL(kind=4) :: CURRENT_AMOUNT,LAGGED_AMOUNT
      REAL(kind=4) :: R_REVENUE_ELIM
      REAL(kind=4) :: R_MONTHLY_EXPENSES(0:12),
     +       MONTHLY_ALLOCATED_EXPENSES(0:12)
      REAL(kind=4) :: R_EXPENSES_ELIM
      INTEGER(kind=2) :: PERIOD
      REAL(kind=4) :: R_RESIDENTIAL_REVENUES,
     +       R_COMMERCIAL_REVENUES,
     +       R_INDUSTRIAL_REVENUES,
     +       R_LIGHTING_REVENUES,
     +       R_BULK_POWER_REVENUES,
     +       R_CAPACITY_SALES,
     +       R_GOVERNMENT_SALES,
     +       R_ADJ_EXP

      REAL(kind=4) :: MONTH_VARS(0:12,1:*)
      REAL(kind=4) :: R_MON_CHANGE_ACCTS_RECEIVABLE(0:12),
     +       R_MON_CHANGE_ACCTS_PAYABLE(0:12)
      INTEGER(kind=2) :: DATA_POS,J

      REAL(kind=4) :: R_CHANGE_IN_ACCOUNTS_RECEIVABLE,
     +       R_CHANGE_IN_ACCOUNTS_PAYABLE


            NUM_OF_ASSET_CLASSES = R_NUM_OF_ASSET_CLASSES
            MAX_ASSET_CLASS_NUM = R_MAX_ASSET_CLASS_NUM


            IF(ALLOCATED(REVENUES_MONTHLY)) DEALLOCATE(REVENUES_MONTHLY,
     +                                       EXPENSES_MONTHLY,
     +                                       CASH_AMOUNTS_RECEIVABLE,
     +                                       CASH_AMOUNTS_PAYABLE,
     +                                       ENERGY_MONTHLY)
            ALLOCATE(REVENUES_MONTHLY(0:12,-1:NUM_OF_ASSET_CLASSES,
     +                                                LAST_INCOME_LINE),
     +             CASH_AMOUNTS_RECEIVABLE(0:12,-1:NUM_OF_ASSET_CLASSES,
     +                                                LAST_INCOME_LINE),
     +               EXPENSES_MONTHLY(0:12,-1:NUM_OF_ASSET_CLASSES,
     +                                               LAST_EXPENSE_ITEM),
     +               CASH_AMOUNTS_PAYABLE(0:12,-1:NUM_OF_ASSET_CLASSES,
     +                                               LAST_EXPENSE_ITEM),
     +               ENERGY_MONTHLY(0:12,-1:NUM_OF_ASSET_CLASSES,
     +                                                LAST_INCOME_LINE))

         REVENUES_MONTHLY = 0.
         CASH_AMOUNTS_RECEIVABLE = 0.
         EXPENSES_MONTHLY = 0.
         CASH_AMOUNTS_PAYABLE = 0.
         ENERGY_MONTHLY = 0.

         IF(ALLOCATED(ASSET_CLASS_POINTER))
     +                                   DEALLOCATE(ASSET_CLASS_POINTER)
         ALLOCATE(ASSET_CLASS_POINTER(MAX_ASSET_CLASS_NUM))
         ASSET_CLASS_POINTER = R_ASSET_CLASS_POINTER
      RETURN

      ENTRY SAVE_REVENUE_DATA_FORECASTS(R_NUM_OF_ASSET_CLASSES,
     +                                  R_REVENUES_MONTHLY,
     +                                  R_CASH_AMOUNTS_RECEIVABLE,
     +                                  R_ENERGY_MONTHLY)

         REVENUES_MONTHLY = R_REVENUES_MONTHLY
         CASH_AMOUNTS_RECEIVABLE = R_CASH_AMOUNTS_RECEIVABLE
         ENERGY_MONTHLY = R_ENERGY_MONTHLY
      RETURN

      ENTRY SAVE_EXPENSE_DATA_FORECASTS(R_NUM_OF_ASSET_CLASSES,
     +                                  R_EXPENSES_MONTHLY,
     +                                  R_CASH_AMOUNTS_PAYABLE)

         EXPENSES_MONTHLY = R_EXPENSES_MONTHLY
         CASH_AMOUNTS_PAYABLE = R_CASH_AMOUNTS_PAYABLE
      RETURN

      ENTRY GET_REV_FORECAST_SALES(R_MONTH,
     +                             R_RESIDENTIAL_MWH,
     +                             R_COMMERCIAL_MWH,
     +                             R_INDUSTRIAL_MWH,
     +                             R_PUBLIC_STREET_HIGHWAY_MWH,
     +                             R_OTHER_PUBLIC_MWH,
     +                             R_WHOLESALE_MWH,
     +                             R_OTHER_MWH)

         R_RESIDENTIAL_MWH = 0.
         R_COMMERCIAL_MWH = 0.
         R_INDUSTRIAL_MWH = 0.
         R_PUBLIC_STREET_HIGHWAY_MWH = 0.
         R_OTHER_PUBLIC_MWH = 0.
         R_WHOLESALE_MWH = 0.
         R_OTHER_MWH = 0.
!         
         DO I = 0, NUM_OF_ASSET_CLASSES
            R_RESIDENTIAL_MWH = R_RESIDENTIAL_MWH +
     +                    ENERGY_MONTHLY(R_MONTH,I,Residential)
            R_COMMERCIAL_MWH = R_COMMERCIAL_MWH +
     +                    ENERGY_MONTHLY(R_MONTH,I,Commercial)
            R_INDUSTRIAL_MWH = R_INDUSTRIAL_MWH +
     +                    ENERGY_MONTHLY(R_MONTH,I,Industrial)
            R_PUBLIC_STREET_HIGHWAY_MWH = R_PUBLIC_STREET_HIGHWAY_MWH +
     +                      ENERGY_MONTHLY(R_MONTH,I,Lighting)

            R_OTHER_PUBLIC_MWH = R_OTHER_PUBLIC_MWH +
     +                    ENERGY_MONTHLY(R_MONTH,I,Government)
            R_WHOLESALE_MWH = R_WHOLESALE_MWH +
! Secondary Sales named secondary_sales on left, and
! SecondarySales on right.  Keeping SecondarySales. Merge2Issue
     +                    ENERGY_MONTHLY(R_MONTH,I,SecondarySales)


            R_OTHER_MWH = R_OTHER_MWH + 
     +                    ENERGY_MONTHLY(R_MONTH,I,BulkPower)
         ENDDO
!         
      RETURN

      ENTRY GET_ENERGY_SALES_BY_CLASS(R_CLASS,
     +                                REVENUES_BY_CLASS,
     +                                ENERGY_SALES_BY_CLASS)

         IF(R_CLASS <= MAX_ASSET_CLASS_NUM .AND.
     +                                     MAX_ASSET_CLASS_NUM > 0) THEN
            IF(R_CLASS <=0) THEN
               ASSET_CLASS = R_CLASS
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS <= 0) THEN
               ENERGY_SALES_BY_CLASS(:,:) =
     +                                   ENERGY_MONTHLY(:,ASSET_CLASS,:)
               REVENUES_BY_CLASS(:,:)=REVENUES_MONTHLY(:,ASSET_CLASS,:)
            ENDIF
         ENDIF

      RETURN

      ENTRY RC_EXPENSE_REVENUE_INFO(R_CLASS,
     +                              R_FUEXP,R_PREXP,R_OPEXP,
     +                              R_MNEXP,R_OTHER1,R_OTHER2,
     +                              R_OTHER3,R_NFOWN,R_NFLEASE,
     +                              R_ADJ_EXP,
     +                              R_DSM_EXPENSE,
     +                              R_DSM_REBATE,
     +                              R_ADJUSTMENT_CLAUSE_REVENUES,
     +                              R_BASE_RATES_REVENUES,
     +                              R_SECONDARY_SALES_REVENUES,
     +                              R_OTHER_REVENUES,
     +                              R_BTL_REVENUES,
     +                              R_BTL_EXPENSE,
     +                              R_ATL_LEASE_EXP,         
     +                              R_BTL_LEASE_EXP,
     +                              R_SERVICE_TRANSACTIONS,
     +                              R_GAS_REVENUES,
     +                              R_CAT_REVENUES,
     +                              R_RESIDENTIAL_REVENUES,
     +                              R_COMMERCIAL_REVENUES,
     +                              R_INDUSTRIAL_REVENUES,
     +                              R_LIGHTING_REVENUES,
     +                              R_BULK_POWER_REVENUES,
     +                              R_CAPACITY_SALES,
     +                              R_GOVERNMENT_SALES,
     +                              R_GAS_ADJUSTMENT_CLAUSE_REVENUE,
     +                              R_COMPETITIVE_SALES_REVENUE,
     +                              FE_Competitive_Unit_Sales,
     +                              FE_Intra_Company_Utility_Sales)

         R_COMPETITIVE_SALES_REVENUE = 0.
         IF(R_CLASS <= MAX_ASSET_CLASS_NUM .AND.
     +                                     MAX_ASSET_CLASS_NUM > 0) THEN
            IF(R_CLASS <=0) THEN
               ASSET_CLASS = R_CLASS
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS <= 0) THEN
               PERIOD = 0
               R_ADJ_EXP = R_ADJ_EXP 
               R_FUEXP = R_FUEXP + EXPENSES_MONTHLY(PERIOD,ASSET_CLASS,
     +                                                      FossilFuel) ! = 11,
               R_PREXP = R_PREXP + EXPENSES_MONTHLY(PERIOD,ASSET_CLASS,
     +                                                  PurchasedPower) ! = 12,
               R_OPEXP = R_OPEXP + EXPENSES_MONTHLY(PERIOD,ASSET_CLASS,
     +                                                   VariableOandM) ! = 13,
               R_MNEXP = R_MNEXP + EXPENSES_MONTHLY(PERIOD,ASSET_CLASS,
     +                                                      FixedOandM) ! = 14,
               R_OTHER1 = R_OTHER1 +
     +                              EXPENSES_MONTHLY(PERIOD,ASSET_CLASS,
     +                                                      OtherOandM) ! = 15,
               R_OTHER2 = R_OTHER2 +
     +                              EXPENSES_MONTHLY(PERIOD,ASSET_CLASS,
     +                                                    PurchasedGas) ! = 16,
               R_OTHER3 = R_OTHER3 +  
     +                              EXPENSES_MONTHLY(PERIOD,ASSET_CLASS,
     +                                                            Other) ! = 17,
               R_NFOWN = R_NFOWN + EXPENSES_MONTHLY(PERIOD,ASSET_CLASS,
     +                                               OwnedNuclearFuel) ! = 18,
               R_NFLEASE = R_NFLEASE + 
     +                              EXPENSES_MONTHLY(PERIOD,ASSET_CLASS,
     +                                              LeasedNuclearFuel) ! = 19,
               R_DSM_EXPENSE = R_DSM_EXPENSE +  
     +                              EXPENSES_MONTHLY(PERIOD,ASSET_CLASS,
     +                                                      DSMExpense) ! = 20,
               R_DSM_REBATE = R_DSM_REBATE +
     +                              EXPENSES_MONTHLY(PERIOD,ASSET_CLASS,
     +                                                       DSMRebate) ! = 21,
               R_BTL_EXPENSE = R_BTL_EXPENSE +  
     +                              EXPENSES_MONTHLY(PERIOD,ASSET_CLASS,
     +                                                     BTLExpenses) ! = 28
               R_ATL_LEASE_EXP = R_ATL_LEASE_EXP +  
     +                              EXPENSES_MONTHLY(PERIOD,ASSET_CLASS,
     +                                           ATLBookLeaseExpense) ! = 22,
               R_BTL_LEASE_EXP = R_BTL_LEASE_EXP +
     +                              EXPENSES_MONTHLY(PERIOD,ASSET_CLASS,
     +                                                   BTLLeaseCash)
               R_SERVICE_TRANSACTIONS = R_SERVICE_TRANSACTIONS +
     +                              EXPENSES_MONTHLY(PERIOD,ASSET_CLASS,

     +                                             ServiceTransactions) ! = 23,

               R_ADJUSTMENT_CLAUSE_REVENUES =
     +                 R_ADJUSTMENT_CLAUSE_REVENUES +
     +                 REVENUES_MONTHLY(0,ASSET_CLASS,adjustment_clause)


               R_GAS_ADJUSTMENT_CLAUSE_REVENUE =
     +                  R_GAS_ADJUSTMENT_CLAUSE_REVENUE
     +                  + REVENUES_MONTHLY(0,ASSET_CLASS,PGAAdjustment)
               R_BASE_RATES_REVENUES = R_BASE_RATES_REVENUES +
     +                        REVENUES_MONTHLY(0,ASSET_CLASS,BaseRates)
               R_SECONDARY_SALES_REVENUES = R_SECONDARY_SALES_REVENUES +

     +                   REVENUES_MONTHLY(0,ASSET_CLASS,SecondarySales)

               R_OTHER_REVENUES = R_OTHER_REVENUES +
     +                     REVENUES_MONTHLY(0,ASSET_CLASS,OtherRevenue)
               R_BTL_REVENUES = R_BTL_REVENUES +
     +                      REVENUES_MONTHLY(0,ASSET_CLASS,BTLRevenues)
               R_GAS_REVENUES = R_GAS_REVENUES +
     +                      REVENUES_MONTHLY(0,ASSET_CLASS,GasRevenues)
               R_CAT_REVENUES = R_CAT_REVENUES +
     +                  REVENUES_MONTHLY(0,ASSET_CLASS,CatawbaRevenues)
               R_RESIDENTIAL_REVENUES = R_RESIDENTIAL_REVENUES +
     +                       REVENUES_MONTHLY(0,ASSET_CLASS,Residential)
               R_COMMERCIAL_REVENUES = R_COMMERCIAL_REVENUES +
     +                        REVENUES_MONTHLY(0,ASSET_CLASS,Commercial)
               R_INDUSTRIAL_REVENUES = R_INDUSTRIAL_REVENUES +
     +                        REVENUES_MONTHLY(0,ASSET_CLASS,Industrial)
               R_LIGHTING_REVENUES = R_LIGHTING_REVENUES +
     +                          REVENUES_MONTHLY(0,ASSET_CLASS,Lighting)
               R_BULK_POWER_REVENUES = R_BULK_POWER_REVENUES +
     +                        REVENUES_MONTHLY(0,ASSET_CLASS,BulkPower)
               R_CAPACITY_SALES = R_CAPACITY_SALES +
     +                    REVENUES_MONTHLY(0,ASSET_CLASS,CapacitySales)
               R_GOVERNMENT_SALES = R_GOVERNMENT_SALES +
     +                        REVENUES_MONTHLY(0,ASSET_CLASS,Government)
               R_COMPETITIVE_SALES_REVENUE =
     +                              R_COMPETITIVE_SALES_REVENUE
     +                              + REVENUES_MONTHLY(0,ASSET_CLASS,21) ! 21) ! competitive sales 12/12/02 was 20
               FE_Intra_Company_Utility_Sales =
     +                              FE_Intra_Company_Utility_Sales
     +                              + REVENUES_MONTHLY(0,ASSET_CLASS,20) ! Utility Sales
               DO REV_TYPE = 22, 30 ! FE Competitive sales
                  FE_Competitive_Unit_Sales =
     +                        FE_Competitive_Unit_Sales
     +                        + REVENUES_MONTHLY(0,ASSET_CLASS,REV_TYPE)
               ENDDO
            ENDIF
         ENDIF
      RETURN

      ENTRY RETURN_MONTHY_RC_REVENUES(R_CLASS,MONTH_VARS)

         IF(R_CLASS <= MAX_ASSET_CLASS_NUM .AND.
     +                                     MAX_ASSET_CLASS_NUM > 0) THEN
            IF(R_CLASS <= 0) THEN
               ASSET_CLASS = R_CLASS
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS <= 0) THEN
               DO MO = 0, 12
                  DO DATA_POS = 1, LAST_INCOME_LINE
                     IF(DATA_POS > EXP_OFFSET_LINE) THEN
                     ELSE
                        MONTH_VARS(MO,DATA_POS)=MONTH_VARS(MO,DATA_POS) 
     +                       + REVENUES_MONTHLY(MO,ASSET_CLASS,DATA_POS)
                     ENDIF
                  ENDDO
               ENDDO
            ENDIF
         ENDIF
      RETURN

      ENTRY RETURN_MONTHY_RC_CASH_REVENUES(R_CLASS,MONTH_VARS)

         IF(R_CLASS <= MAX_ASSET_CLASS_NUM .AND.
     +                                     MAX_ASSET_CLASS_NUM > 0) THEN
            IF(R_CLASS <= 0) THEN
               ASSET_CLASS = R_CLASS
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS <= 0) THEN
               DO MO = 0, 12
                  DO DATA_POS = Cash_Base_Rates, Cash_Government
                     MONTH_VARS(MO,DATA_POS) = MONTH_VARS(MO,DATA_POS) +
     +                  CASH_AMOUNTS_RECEIVABLE(MO,ASSET_CLASS,DATA_POS)
                  ENDDO
               ENDDO
            ENDIF  
         ENDIF
      RETURN

      ENTRY RETURN_MONTHY_RC_CASH_EXPENSES(R_CLASS,MONTH_VARS,
     +                                     R_MONTHLY_VALUES)

         IF(R_CLASS <= MAX_ASSET_CLASS_NUM .AND.
     +                                     MAX_ASSET_CLASS_NUM > 0) THEN
            IF(R_CLASS <= 0) THEN
               ASSET_CLASS = R_CLASS
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS <= 0) THEN
               DO MO = 0, 12
                  DO DATA_POS = Cash_BTL_Lease_Cash, Cash_Other
                     IF(DATA_POS == Cash_Purchased_Power) THEN
                        R_MONTHLY_VALUES(MO) =
     +                  CASH_AMOUNTS_PAYABLE(MO,ASSET_CLASS,DATA_POS-30) !THE 30 IS AN OFFSET TO THE INCOME POSITIONS
                     ELSE
                        MONTH_VARS(MO,DATA_POS)=MONTH_VARS(MO,DATA_POS)+
     +                  CASH_AMOUNTS_PAYABLE(MO,ASSET_CLASS,DATA_POS-30) !THE 30 IS AN OFFSET TO THE INCOME POSITIONS
                     ENDIF
                  ENDDO
                  DO DATA_POS = Cash_Leased_Nuclear_Fuel,
     +                                               Cash_AG_Maintenance
                     MONTH_VARS(MO,DATA_POS) = MONTH_VARS(MO,DATA_POS) +
     +                  CASH_AMOUNTS_PAYABLE(MO,ASSET_CLASS,DATA_POS-30)
                  ENDDO

!                 DO DATA_POS = 1, LAST_INCOME_LINE
!                    MONTH_VARS(MO,DATA_POS) = MONTH_VARS(MO,DATA_POS) +
!    +                  CASH_AMOUNTS_PAYABLE(MO,ASSET_CLASS,DATA_POS)
!                 ENDDO
               ENDDO
            ENDIF  
         ENDIF
      RETURN

      ENTRY RETURN_MONTHY_RC_EXPENSES(R_CLASS,MONTH_VARS,
     +                                R_MONTHLY_VALUES)

         IF(R_CLASS <= MAX_ASSET_CLASS_NUM .AND.
     +                                     MAX_ASSET_CLASS_NUM > 0) THEN
            IF(R_CLASS <= 0) THEN
               ASSET_CLASS = R_CLASS
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS <= 0) THEN
               DO MO = 0, 12
                  DO DATA_POS = 1, LAST_EXPENSE_ITEM
                     IF(DATA_POS == PurchasedPower) THEN
                        R_MONTHLY_VALUES(MO) =
     +                         EXPENSES_MONTHLY(MO,ASSET_CLASS,DATA_POS)
                     ELSE
                        MONTH_VARS(MO,DATA_POS) =MONTH_VARS(MO,DATA_POS) 
     +                       + EXPENSES_MONTHLY(MO,ASSET_CLASS,DATA_POS)
                     ENDIF
                  ENDDO
               ENDDO
            ENDIF
         ENDIF
      RETURN

      ENTRY CHANGE_IN_RECD_PAYS_4_REV_FORC(R_CLASS,
     +                                  R_CHANGE_IN_ACCOUNTS_RECEIVABLE,
     +                                  R_CHANGE_IN_ACCOUNTS_PAYABLE)

         IF(R_CLASS <= MAX_ASSET_CLASS_NUM .AND.
     +                                     MAX_ASSET_CLASS_NUM > 0) THEN
            IF(R_CLASS <= 0) THEN
               ASSET_CLASS = R_CLASS
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS <= 0) THEN
               DO DATA_POS = Cash_Base_Rates, Cash_Gas_Revenues
                  R_CHANGE_IN_ACCOUNTS_RECEIVABLE =
     +                             R_CHANGE_IN_ACCOUNTS_RECEIVABLE +
     +                   REVENUES_MONTHLY(0,ASSET_CLASS,DATA_POS) -
     +                   CASH_AMOUNTS_RECEIVABLE(0,ASSET_CLASS,DATA_POS)
               ENDDO
               DO DATA_POS = Cash_Relationship_Revenues,
     +                                         Cash_Operating_Method_Adj
                  R_CHANGE_IN_ACCOUNTS_RECEIVABLE =
     +                             R_CHANGE_IN_ACCOUNTS_RECEIVABLE +
     +                   REVENUES_MONTHLY(0,ASSET_CLASS,DATA_POS) -
     +                   CASH_AMOUNTS_RECEIVABLE(0,ASSET_CLASS,DATA_POS)
               ENDDO
               DO DATA_POS = BTLLeaseCash, Other
                  R_CHANGE_IN_ACCOUNTS_PAYABLE =
     +                             R_CHANGE_IN_ACCOUNTS_PAYABLE +
     +                   EXPENSES_MONTHLY(0,ASSET_CLASS,DATA_POS) -
     +                   CASH_AMOUNTS_PAYABLE(0,ASSET_CLASS,DATA_POS)
               ENDDO
               DO DATA_POS = TotalNuclearFuel, AGMaintenance
                  R_CHANGE_IN_ACCOUNTS_PAYABLE =
     +                             R_CHANGE_IN_ACCOUNTS_PAYABLE +
     +                   EXPENSES_MONTHLY(0,ASSET_CLASS,DATA_POS) -
     +                   CASH_AMOUNTS_PAYABLE(0,ASSET_CLASS,DATA_POS)
               ENDDO
            ENDIF
         ENDIF
      RETURN

      ENTRY MON_REV_FORC_RECEI_PAYABLES(R_CLASS,
     +                                 R_MON_CHANGE_ACCTS_RECEIVABLE,
     +                                 R_MON_CHANGE_ACCTS_PAYABLE)

         IF(R_CLASS <= MAX_ASSET_CLASS_NUM .AND.
     +                                     MAX_ASSET_CLASS_NUM > 0) THEN
            IF(R_CLASS <= 0) THEN
               ASSET_CLASS = R_CLASS
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS <= 0) THEN
               DO MO = 0, 12
                  DO DATA_POS = Cash_Base_Rates, Cash_Gas_Revenues
                     R_MON_CHANGE_ACCTS_RECEIVABLE(MO) =
     +                             R_MON_CHANGE_ACCTS_RECEIVABLE(MO) +
     +                  REVENUES_MONTHLY(MO,ASSET_CLASS,DATA_POS) -
     +                  CASH_AMOUNTS_RECEIVABLE(MO,ASSET_CLASS,DATA_POS)
                  ENDDO
                  DO DATA_POS = Cash_Relationship_Revenues,
     +                                         Cash_Operating_Method_Adj
                     R_MON_CHANGE_ACCTS_RECEIVABLE (MO) =
     +                             R_MON_CHANGE_ACCTS_RECEIVABLE(MO) +
     +                  REVENUES_MONTHLY(MO,ASSET_CLASS,DATA_POS) -
     +                  CASH_AMOUNTS_RECEIVABLE(MO,ASSET_CLASS,DATA_POS)
                  ENDDO
                  DO DATA_POS = BTLLeaseCash, Other
                     R_MON_CHANGE_ACCTS_PAYABLE(MO) =
     +                             R_MON_CHANGE_ACCTS_PAYABLE(MO) +
     +                    EXPENSES_MONTHLY(MO,ASSET_CLASS,DATA_POS) -
     +                    CASH_AMOUNTS_PAYABLE(MO,ASSET_CLASS,DATA_POS)
                  ENDDO
                  DO DATA_POS = TotalNuclearFuel, AGMaintenance
                     R_MON_CHANGE_ACCTS_PAYABLE(MO) =
     +                             R_MON_CHANGE_ACCTS_PAYABLE(MO) +
     +                   EXPENSES_MONTHLY(MO,ASSET_CLASS,DATA_POS) -
     +                   CASH_AMOUNTS_PAYABLE(MO,ASSET_CLASS,DATA_POS)
                  ENDDO
               ENDDO
            ENDIF
         ENDIF
      RETURN
      END      
!
!
