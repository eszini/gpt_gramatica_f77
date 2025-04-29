      

   MODULE BASE_FILE_NAMES_FROM_BFIL_OBJ  ! SOURCE IN MODULES95
    CHARACTER (LEN=256) :: COAL_NODE='NONE',              & ! 354
                           COAL_TRANSPORT='NONE',         & ! 355
                           COAL_SUPPLY_FORECAST='NONE',   & ! 356
                           PLANT_DEMAND_FORECAST='NONE',   & ! 357
                           COAL_CONTRACTS='NONE',          & ! 361
                           COAL_MODEL_POINTER_FILE='NONE',  & ! 372
                           COAL_MODEL_SO2_INFO_FILE='NONE', & ! 373
                      COAL_MODEL_GENE_TRANS_FILE='NONE', & ! 374
                        COAL_MODEL_GENERIC_DEMAND_FILE='NONE',  & ! 375
                           EXPENSE_NAMES_RESERVED='NONE' ! 7
   END MODULE BASE_FILE_NAMES_FROM_BFIL_OBJ



!--------
      MODULE PROCSAVE_COMMON
         REAL (KIND=8), ALLOCATABLE, SAVE :: ENERGY(:), &
                                             TENRG(:), &
                                             TMMBTUS(:), &
                                             FUELCOST(:), &
                                             VARCOST(:)

      END MODULE PROCSAVE_COMMON

      MODULE FINANCIAL_SWITCHES_COMMON
!
! WAS A COMMON BLOCK FOR FINANCIAL RUN SWITCHES
!
         CHARACTER (LEN=1), SAVE :: OPMETH,DVMETH,SRCNPV,ACMETH,TXMETHF,TXMETHS,&
                              PRICE_SOURCE,P_EQUITY_DEFINITION,  &
                              DEBT_ISSUE_TYPE
         INTEGER (KIND=2), SAVE :: OPYEAR,RBMETH
         LOGICAL (KIND=1), SAVE :: CALCULATE_BTL_TAXES
         INTEGER (KIND=2), SAVE:: COVERAGE_RATIO=5,PROPERTY_TAX_METHOD
      END MODULE FINANCIAL_SWITCHES_COMMON


      MODULE LH_GLOBAL_VARIABLES
         CHARACTER (LEN=1) :: SAVE_SCENARIO_MRX_PLANS='N'
         CHARACTER (LEN=2) :: MRX_EXPANSION_PLAN_FILE_CODE="CP",&
                              MRX_EXPANSION_PLAN_FILE_NAME="LH"
         INTEGER (KIND=2) ::  MRX_SEQUENCE_NUMBER=1
      END MODULE LH_GLOBAL_VARIABLES
      MODULE TRANS_GROUP_VARIABLES
         REAL (KIND=4), SAVE, ALLOCATABLE :: PRICE_ESCALATION_RATE(:), &
                                       MINIMUM_CAPACITY_TESTING_RATIO(:),& ! 72
                                       MAXIMUM_CAPACITY_TESTING_RATIO(:) ! 73
         INTEGER (KIND=2), ALLOCATABLE :: TGYr2UseExtnsionPriceEscalation(:)
         CHARACTER (LEN=1), SAVE, ALLOCATABLE :: SAVE_MRX_EXPANSION_PLAN(:)
         CHARACTER (LEN=60), SAVE, ALLOCATABLE :: LONG_TRANS_GROUP_NAME(:)
         INTEGER (KIND=2), SAVE, ALLOCATABLE :: MRX_ICAP_UNIT_LINK(:) ! 78
         CHARACTER (LEN=50), SAVE, ALLOCATABLE :: TRANS_GROUP_FULL_NAME(:)
         CHARACTER (LEN=6), SAVE, ALLOCATABLE :: TG_BASECASE_MARKET_AREA_ID(:) ! MOVED FROM READ_TRANS_GROUPS_DATA
     END MODULE TRANS_GROUP_VARIABLES
     MODULE ANNUAL_MARG_COST_ALLOCAT_ARRAYS
! ARRAYS IN DEALLOC_ANNUAL
      INTEGER (KIND=4), ALLOCATABLE, SAVE ::             &
              OUTAGE_BLOCK_N_DB(:,:),                    &
              OUTAGE_BLOCK_REVERSE_INDEX(:,:),           &
              COST_CURVE_POINTER_BY(:,:)                  
      INTEGER (KIND=2), ALLOCATABLE, SAVE ::             &
              OUTAGE_BLOCKS_4_DB(:)
      REAL (KIND=4), ALLOCATABLE, SAVE ::                &
              BlockIncrCost(:),                          &
              TOT_EMBED_COST_BY_POINT(:,:),              &
              CURRENT_UNIT_UTIL(:,:),                    &
              CUM_UNIT_UTIL(:,:),                        &
              CUM_UNIT_UTIL_zero(:,:),                   &
              CURRENT_UNIT_UTIL_zero(:,:)
! ARRAYS IN DEALLO_MONTH_MARG_COST_ARRAYS
      INTEGER (KIND=2), ALLOCATABLE, SAVE ::  &
              SAVE_TRANS_FOR_DATA_BASE(:),    &
              OUTAGE_UNIT_INDEX(:),           &
              OUTAGE_BLOCK_INDEX(:),          &
              START_UP_UNIT_BY_TG(:),         &
              OUTAGE_DB_USED(:),              &
              COMMITMENT_DB(:),               &
              CURRENT(:),                     &
              NEXT(:)
      integer (kind=2), allocatable, save :: POS(:)
      
      integer (kind=2), allocatable, save :: &
              NATIVE_POS(:),                  &
              LAST_DB_POINT(:),               &
              CumDbDx(:),                     &
              NEW_POINT_COUNT(:,:),           &
              NEW_BLOCK_NUMBER(:,:,:),        & 
              BLOCK_B4_MARKET(:,:),           & 
              CUM_MUST_RUN_POSITION(:),       & 
              ACTIVE_UNIT(:),                 &
              START_UP_POSITION(:,:),         &
              UNIT_SAVE(:),BLKNO_SAVE(:)
     LOGICAL (KIND=1), ALLOCATABLE, SAVE ::    &
              ACTIVE_DATA_BASE(:),             &
              DETAILED_OUTAGE_DATA_BASE(:)      
     REAL (KIND=4), ALLOCATABLE, SAVE ::       &
              LODDUR_FOR_MC(:,:),              &
              LPROB_FOR_MC(:,:,:),             &
              CUM_CAP(:),                      &
              DB_ADJ_TOTAL_CAP(:),             &
              DB_CAP_AFTER_OUTAGES(:),         &
              DB_DX(:),                        &
              PREVIOUS_INTERVAL(:),            &
              I_CORRECT(:),                    &
              NEW_LODDUR(:,:),                 &
              NEW_MARGINAL_DATA_BASE(:,:,:),   &
              EXPECTED_MARGINAL_COST(:,:),     &
              CUM_COST(:),                     &
              CUM_PROB(:),                     &
              NATIVE_POS_PERCENT(:),           &
              REMAINING_CONTRIBUTION_MW(:,:),  &
              SALES_B4_AFTER(:,:,:),           &
              WHOLESALE_REV_AND_EXP(:,:,:),    &
              WHOLESALE_SAL_AND_PUR(:,:,:),    &
              RETAIL_REV_AND_EXP(:,:,:),       &
              RETAIL_SAL_AND_PUR(:,:,:),       &
              CUM_SALES_AFTER(:),              &
              CUM_INTERVAL_CAPACITY(:,:),      &
              CUM_MUST_RUN_MW(:),              & 
              HOURLY_OUTAGE_STATE(:,:),        &
              TRANS_BLOCK_CAPACITY(:),         &
              SCARCITY_MULT(:,:),              &
              CUM_TC_UNIT_MWH(:),              &
              UNIT_DISP_COST(:,:),             &
              OUTAGE_ENERGY(:),                &
              UNIT_ENERGY_B4_AFTER(:,:,:),     &
              FIRST_ECONOMY_COST(:),           &
              MW_AFTER_OUTAGES(:,:),           &
              MW_DIFF(:,:),                    &
              MARGINAL_UNIT_BY_TECH(:,:)
     REAL (KIND=8), ALLOCATABLE, SAVE ::    &
          SAVE_LOAD(:),                     &
          TRANSITION_MARGINAL_COST(:,:,:),  &
          CAPACITY_GIVEN_MARKET(:,:)
! ARRAYS NOT DEALLOCATED BY ANNUAL OR MONTHLY
    INTEGER (KIND=2), ALLOCATABLE, SAVE ::   &
              TEMP_TRANSFER_ARRAY1(:),         &
              TEMP_TRANSFER_ARRAY2(:)
     REAL (KIND=4), ALLOCATABLE, SAVE ::           &
              OUTAGE_SINGULAR(:),                  &
              ADJUST_BLOCK(:),                     &
              DERATE_BLOCK(:),                     &
              OUTAGE_TYPE(:),                      &
              HOURLY_OUTAGE_TYPE(:,:),             &
              HOURLY_LAST_STATE(:),                &
              HOURLY_LAST_DERATE(:),               &
              HOURLY_LAST_TYPE(:),                 &
              DEPTH_PRICE(:,:),                    &
              TRANS_DEPTH_PRICE(:,:,:),            &
              BLOCK_DISP_COST(:),                  &
              OUTAGE_WHOLESALE_ENERGY(:,:),        &
              OUTAGE_WHOLESALE_REVENUE(:,:),       &
              MUST_RUN_COST(:),                          &
              TEMP_TRANSFER_ARRAY3(:)
     END MODULE ANNUAL_MARG_COST_ALLOCAT_ARRAYS

!
! 072119. NEW CODE FOR HOURLY LOAD SHAPE.
!
!***************************************************************
      FUNCTION StoreHourlyProductFiles(ProductHourlyRefName,    &
                                       ProductHourlyRefNumber,  &
                                       ScenarioLoadPattern) &
                                                         RESULT(FilePrt)   
         USE HourlyProductsData                                        
         use end_routine, only: end_program, er_message 
         CHARACTER (LEN=5) :: ProductHourlyRefName,TempHourlyRefName
         CHARACTER (LEN=2) :: ScenarioType,ScenarioLoadPattern 
         INTEGER (KIND=2) :: ProductHourlyRefNumber,FilePrt,I,J
         CHARACTER (LEN=2) :: LOAD_FILE_CHAR_EXT
!
         CALL UPC(ProductHourlyRefName,ProductHourlyRefName)
         CALL UPC(ScenarioLoadPattern,ScenarioType)
         IF(ScenarioType == 'WI' .AND. .NOT. UseWindLSYears) ScenarioType = 'HO'
         IF(ScenarioType == 'SO' .AND. .NOT. UseSolarLSYears) ScenarioType = 'HO'
         IF(ScenarioType == 'LM' .AND. .NOT. UseLMP_LSYears) ScenarioType = 'HO'
         IF(ScenarioType == 'LO' .AND. .NOT. UseLoadLSYears) ScenarioType = 'HO'
         DO I = 1, ActiveProdFiles
            TempHourlyRefName = TRIM(HourlyProdFileNames(I))
            IF(TRIM(TempHourlyRefName) == TRIM(ProductHourlyRefName)) THEN
!            IF(INDEX(TRIM(HourlyProdFileNames(I)),TRIM(ProductHourlyRefName))/=0) THEN
               IF(ScenarioType == 'HO') THEN
                  DO J= 1,30 
                     IF(HourlyProdRefNumber(I,J) /= ProductHourlyRefNumber) EXIT
                  ENDDO
                  IF(J < 31) CYCLE
                  EXIT
               ENDIF
               IF(RefUsed(I) == ScenarioType) EXIT
            ENDIF
         ENDDO
! 060520. TEST LIMIT.
         IF(ActiveProdFiles > 31000) THEN
            WRITE(9,*) "Active hourly files exceed limit ",ActiveProdFiles
            call end_program("Active hourly files exceed limit. MF1")
         ENDIF
         IF(I > ActiveProdFiles) THEN
            ActiveProdFiles = ActiveProdFiles + 1
            HourlyProdFileNames(I)= TRIM(ProductHourlyRefName)
            HourlyProdRefNumber(I,1:31) = ProductHourlyRefNumber
            RefUsed(I) = ScenarioType
            IF(ScenarioType == 'WI') THEN   
                  HourlyProdRefNumber(I,1:31) = WindLoadShapeYears(1:31)
            ELSEIF(ScenarioType == 'SO') THEN
                  HourlyProdRefNumber(I,1:31) = SolarLoadShapeYears(1:31)
            ELSEIF(ScenarioType == 'LM') THEN
                  HourlyProdRefNumber(I,1:31) = LMP_LoadShapeYears(1:31)
            ELSEIF(ScenarioType == 'LO') THEN
                  HourlyProdRefNumber(I,1:31) = SMLoadShapeYears(1:31)
            ENDIF
            
         ENDIF  
         FilePrt = I
      END FUNCTION
!***************************************************************
      SUBROUTINE READ_HOURLY_PRODUCT_FILES(Year)
         USE ArrayAllocationInterface
         USE HourlyProductsData
         use mod_base_year
		 use calendar
         INTEGER (KIND=2) :: Files,RefYr,Yr

         CHARACTER (LEN=256) :: FileName,SHB_FILE_DIRECTORY
         INTEGER (KIND=2) :: DA,RefDaysInMonth(12),Month,CurrentHr,  &
                             ALINE_LOAD_DATA,Hr,Year, &
                             TimeZone,Temp,DeltaTemp,Mo,  &
                             DaOfWeek,I,Day,Calendar_Year, &
                             WIND_SHAPE_NUMBER,SOLAR_SHAPE_NUMBER
         LOGICAL (KIND=4) :: FILE_EXISTS
         CHARACTER (LEN=2) :: LOAD_FILE_CHAR_EXT
         CHARACTER (LEN=8) :: EEICODE=" "
         INTEGER (KIND=4) :: IREC
!         INTEGER (KIND=4) :: Values(24)
         REAL (KIND=4) :: Values(24),SumOfValuesFor(12), &
                          ValuesByMoDa(24,12,31), &
                          CurValuesByMoDa(24,12,31),ScaleFactor, &
                          GET_SCENARIO_WIND_SHAPE, &
                          GET_SCENARIO_SOLAR_SHAPE
! Calendar correct variable         
         LOGICAL (KIND=1) :: YES_CALANDER_CORRECT, &
                             ADJUST_FOR_LEAP_YEAR, &
                             MakeCalendarAdjustment
         LOGICAL (KIND=1) :: WIND_SHAPE_ACTIVE
         LOGICAL (KIND=1) :: SOLAR_SHAPE_ACTIVE
         INTEGER (KIND=4) :: LREC
         INTEGER (KIND=2) :: RefDay,RefMo,RefDoWk,CurDoWk, &

                             CurDaysInMonth,MON=1,TempExtension

!
! NOTE:
! In the original version of this source, those variables were not set.
!
         WIND_SHAPE_ACTIVE  = .false.
         SOLAR_SHAPE_ACTIVE = .false.
         WIND_SHAPE_NUMBER  = 0
         SOLAR_SHAPE_NUMBER = 0

         IF(ActiveProdFiles <= 0) RETURN
!
! 022015.

         CALL AllocateArray(HourlyProductValue,1,744,1,12,   &
                                                     1,ActiveProdFiles)   ! months,hours,files
         HourlyProductValue = 0.
         I = 1
         Calendar_Year = get_base_year() + Year
         DO Files = 1, ActiveProdFiles 
            IF(WIND_SHAPE_ACTIVE .AND. RefUsed(Files) == "WI") THEN
               TempExtension = WIND_SHAPE_NUMBER
            ELSEIF(SOLAR_SHAPE_ACTIVE .AND. RefUsed(Files) == "SO") THEN
               TempExtension = SOLAR_SHAPE_NUMBER
            ELSE
               TempExtension = HourlyProdRefNumber(Files,Year)
            ENDIF
            FileName = TRIM(SHB_FILE_DIRECTORY())//"SHB"//   &
                          TRIM(HourlyProdFileNames(Files))//".S"// &
                              LOAD_FILE_CHAR_EXT(TempExtension)
            INQUIRE(FILE=FileName,EXIST=FILE_EXISTS)
            IF(FILE_EXISTS) THEN
               OPEN(UNIT=2801,FILE=FileName,RECL=118,ACCESS="DIRECT",STATUS="OLD")
!
! ASSUMES 369 RECORD CONVENTION
!     
               IREC = 2
               READ(2801,REC=IREC) Mo,Da,RefYr,EEICODE,DaOfWeek,  &
                                   TimeZone,Temp,DeltaTemp,  &
                                   Values
               MakeCalendarAdjustment = &
                     RefYr /= Calendar_Year .AND. YES_CALANDER_CORRECT()
               IF(MakeCalendarAdjustment) THEN
                  ValuesByMoDa = 0. 
               ENDIF
               DO Month = 1, 12
                  IREC = ALINE_LOAD_DATA(I,Month) 
                  RefDaysInMonth(Month) = &
                                    ALINE_LOAD_DATA(I,Month+1_2) - IREC
                  CurrentHr = 1
                  DO Day = 1, RefDaysInMonth(Month)
                     READ(2801,REC=IREC) Mo,Da,Yr,EEICODE,DaOfWeek,  &
                                         TimeZone,Temp,DeltaTemp,  &
                                         Values
                     IREC = IREC + 1
                     IF(MakeCalendarAdjustment) THEN
                        ValuesByMoDa(1:24,Month,Day) = Values(1:24)
                     ENDIF
                     HourlyProductValue( &
                               CurrentHr:CurrentHr+23,Month,Files) =  &
                                                            Values(1:24)
                     CurrentHr = CurrentHr + 24
                  ENDDO
                  SumOfValuesFor(Month) = &
                                  SUM(HourlyProductValue(:,Month,Files))
               ENDDO
               CLOSE(2801)
               IF(MakeCalendarAdjustment) THEN
                  CALL MakingCalendarAdjustments(Calendar_Year,RefYr, &
                                       HourlyProductValue(:,:,Files), &
                                       ValuesByMoDa,.FALSE.)
               ! TODO: Remove code in IF block
               IF(.FALSE.) THEN
                  HourlyProductValue = 0.
                  ADJUST_FOR_LEAP_YEAR = &
                                      MOD(Calendar_Year-1964.,4.) < .001
                  CALL DAYWEEK(1_2,2_2,RefYr,RefDoWk)
                  CALL DAYWEEK(1_2,2_2,Calendar_Year,CurDoWk)
                  IF(RefDoWk > CurDoWk) THEN
                     RefDay = 7- (RefDoWk - CurDoWk) + 2
                  ELSE
                     RefDay = CurDoWk - RefDoWk + 2
                  ENDIF
                  RefMo = 1
                  DO Month = 1, 12
                     CurrentHr = 1
                     IF(Month == 2) THEN
                        CurDaysInMonth = 28
                        IF(ADJUST_FOR_LEAP_YEAR) CurDaysInMonth = 29
                     ELSE
                        CurDaysInMonth = RefDaysInMonth(Month)
                     ENDIF
                     DO Day = 1, CurDaysInMonth
                        IF(Month == 1 .AND. Day == 1) THEN
                           CurValuesByMoDa(1:24,1,1) = &
                                                  ValuesByMoDa(1:24,1,1)
                           HourlyProductValue(1:24,Month,Files) = &
                                               CurValuesByMoDa(1:24,1,1)
                           CurrentHr = 25
                           CYCLE
                        ENDIF
                        CurValuesByMoDa(1:24,Month,Day) = &
                                         ValuesByMoDa(1:24,RefMo,RefDay)
                        RefDay = RefDay + 1
                        IF(RefDay > RefDaysInMonth(RefMo)) THEN
                           IF(RefMo+1 > 12) THEN
                              RefMo = 12 
                              RefDay = RefDay - 7 ! need to realine Days
                           ELSE
                              RefMo = RefMo + 1
                              RefDay = 1
                           ENDIF
                        ENDIF
                        HourlyProductValue( &
                                CurrentHr:CurrentHr+23,Month,Files) =  &
                                         CurValuesByMoDa(1:24,Month,Day)
                        CurrentHr = CurrentHr + 24
                     ENDDO
                     ScaleFactor = SumOfValuesFor(Month)/ &
                            SUM(HourlyProductValue(:,Month,Files))
                                           
                     HourlyProductValue(:,Month,Files) = ScaleFactor * &
                                       HourlyProductValue(:,Month,Files)
                     ScaleFactor = &
                                  SUM(HourlyProductValue(:,Month,Files))
                  ENDDO
              ENDIF
               ENDIF ! MakeCalendarAdjustment
            ENDIF ! FILE EXISTS
         ENDDO      
         
      END SUBROUTINE

                      


!***************************************************************
      FUNCTION GET_HOURLY_PRODUTION_DATA( &
                     ProductPos,Month,Hour,CFactor) RESULT(Hourly_Value)
         USE HourlyProductsData
         REAL (KIND=4) :: Hourly_Value
         INTEGER (KIND=2) :: Month,Hour,FilePrt,ProductPos,CFactor
         IF(ActiveProdFiles <= 0) THEN
            Hourly_Value = 1.
         ELSEIF(CFactor > 0) THEN
            FilePrt = HourlyProdPtrToCFLoc(ProductPos)
            Hourly_Value = HourlyCFProductValues(Hour,FilePrt)
         ELSE
            FilePrt = HourlyProductFilePtr(ProductPos)
            Hourly_Value = HourlyProductValue(Hour,Month,FilePrt)   
         ENDIF         
      END FUNCTION
!***************************************************************
      FUNCTION GET_DAILY_PRODUTION_DATA( &
                              ProductPos,Day,Month,CFactor,Daily_Values)
         USE HourlyProductsData
         LOGICAL (KIND=1) GET_DAILY_PRODUTION_DATA
         REAL (KIND=4) :: Daily_Values(24)
         INTEGER (KIND=2) :: Day,Month,Hour,FilePrt, &
                             ProductPos,CFactor,HourInMonth
         HourInMonth = (Day-1)*24
         GET_DAILY_PRODUTION_DATA = .TRUE.
         IF(ActiveProdFiles <= 0) THEN
            Daily_Values = 1.
         ELSEIF(CFactor > 0) THEN
            FilePrt = HourlyProdPtrToCFLoc(ProductPos)
            DO Hour = 1, Hour + 23
               Daily_Values(Hour) = HourlyCFProductValues(Hour,FilePrt)
            ENDDO
         ELSE
            FilePrt = HourlyProductFilePtr(ProductPos)
            DO Hour = 1, 24
               Daily_Values(Hour) = &
                      HourlyProductValue(HourInMonth+Hour,Month,FilePrt)
            ENDDO
         ENDIF
      END FUNCTION
      FUNCTION ResetActiveHourlyProducts()
         USE HourlyProductsData
         INTEGER (KIND=2) :: ResetActiveHourlyProducts
         ActiveProdFiles = 0
         ResetActiveHourlyProducts = 0
      END FUNCTION

