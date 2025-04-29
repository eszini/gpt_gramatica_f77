!     ******************************************************************
!     COAL_MODULES.F95
!     Copyright(c)  2000
!
!     Created: 12/9/2010 10:54:40 AM
!     Author : MARK S GERBER
!     Last change: MSG 1/24/2012 10:37:21 AM
!     ******************************************************************


!
!        PROGRAM TO READ MULTI-TAB INFORMATION FOR THE COAL MODEL
!                  AND CONVERT TO BINARY FORMAT
!                        COPYRIGHT (C) 2006
!       ALL RIGHTS RESERVED GLOBAL ENERGY DECISIONS
!

!
!
      SUBROUTINE COAL_SUPPLY_MAKEBIN(BASE_FILE_NAME)  ! ENTERY NAME HERE
      use mod_base_year
      use coal_supply_inputs
      use coal_cnw_files_read_input
      use end_routine
      use logging
!
! SIMULATION VARIABLES
!
         INTEGER (KIND=2) ::   UNIT_NUM=10,IREC,DELETE
         INTEGER (KIND=4) :: IOS
         CHARACTER (LEN=256) :: BASE_FILE_NAME,OVERLAY_FAMILY_NAME, &
                                FILE_NAME,OUTPUT_DIRECTORY, &
                                BASE_FILE_DIRECTORY,SCREEN_OUTPUT
!         CHARACTER (LEN=50) :: COMMENT
         CHARACTER (LEN=1) :: Active="Y"
! DECLARATION FOR DBREAD COMMON BLOCK
         CHARACTER(LEN=1024) :: RECLN
!
!
      SCREEN_OUTPUT = TRIM(FILE_TYPE)//'-'//BASE_FILE_NAME

      FILE_NAME = TRIM(BASE_FILE_DIRECTORY())// &
                      DATA_FILE_CODE//"B"//TRIM(BASE_FILE_NAME )//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=COAL_SUPPLY_DATA_FILE_EXISTS)
      call write_log_entry("coal_main_prg:0001", "coal_supply_makebin " // &
        "BASE_FILE_NAME=" // trim(base_file_name) // ", FILE_TYPE= " // &
        trim(file_type) // ", SCREEN_OUTPUT=" // trim(SCREEN_OUTPUT))
        
      IF(COAL_SUPPLY_DATA_FILE_EXISTS) THEN
!
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=TRIM(OUTPUT_DIRECTORY())//"BC"// &
                                   TRIM(COAL_SUPPLY_BINARY_FILE_NAME), &
                ACCESS="DIRECT",STATUS="REPLACE",RECL=COAL_SUPPLY_LRECL)
!
!
         IREC = 0
         READ(10,*) DELETE
         DO ! TABLES
            DO ! TABLE RECORDS
               READ(10,'(A)',IOSTAT=IOS) RECLN
!
               IF(IOS /= 0) EXIT ! END OF FILE
!
               IF(RECLN(1:1) == '7') THEN ! END OF TABLE
                  EXIT ! GO TO NEXT TABLE
               ENDIF
!
               RECLN = TRIM(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,IOSTAT=IOS)   DELETE, &
                                       SupplyDescription, &
                                       Active, &
                                       BasinID, &
                                       FuelTypeID, &
                                       HeatContent, &
                                       SO2Rate, &
                                       CO2Rate, &
                                       QuantityConsumedBaseYear, &
                                       MinimumAnnualProduction, &
                                       MaximumAnnualProduction, &
                                       SupplyCurvePoints, &
                                       ProductionEscalation, &
                                       ProductionCostEscalation, &
                                       SupplyProduction, &
                                       SupplyCost, &
                                       LastSupplyCost
!
               IF(IOS /= 0) THEN
                  WRITE(4,*) TRIM(RECLN)
                  call end_program("Stop: Coal_Main_Prg IOSCMP1")
               ENDIF
               MAX_SUPPLY_BASIN_ID_BASEFILE = MAX(INT(BasinID,4), &
                                           MAX_SUPPLY_BASIN_ID_BASEFILE)
               IF(Active /= "N" .AND. DELETE < 8) THEN
                  ACTIVE_FUEL_SUPPLY_BASEFILE = &
                                         ACTIVE_FUEL_SUPPLY_BASEFILE + 1
               ENDIF
               IREC = IREC + 1
               WRITE(11,REC=IREC) DELETE, &
                                  SupplyDescription, &
                                  Active, &
                                  BasinID, &
                                  FuelTypeID, &
                                  HeatContent, &
                                  SO2Rate, &
                                  CO2Rate, &
                                  QuantityConsumedBaseYear, &
                                  MinimumAnnualProduction, &
                                  MaximumAnnualProduction, &
                                  SupplyCurvePoints, &
                                  ProductionEscalation, &
                                  ProductionCostEscalation, &
                                  SupplyProduction, &
                                  SupplyCost, &
                                  LastSupplyCost
!
            ENDDO ! TABLE RECORDS
            IF(IOS /= 0) EXIT
         ENDDO ! READ TABLES
         CLOSE(10)
         CLOSE(11)
      ENDIF
      RETURN
      END
!

!
!        PROGRAM TO READ MULTI-TAB INFORMATION FOR THE COAL MODEL
!                  AND CONVERT TO BINARY FORMAT
!                        COPYRIGHT (C) 2006
!       ALL RIGHTS RESERVED GLOBAL ENERGY DECISIONS
!

!
!
      SUBROUTINE COAL_BASIN_MAKEBIN(BASE_FILE_NAME)  ! ENTERY NAME HERE
!

!
!
      USE COAL_BASIN_MAKEBIN_INPUTS
      USE COAL_cnw_FILES_READ_INPUT
      use end_routine
!
! SIMULATION VARIABLES
!
         INTEGER (KIND=2) ::   UNIT_NUM=10,IREC,DELETE
         INTEGER (KIND=4) :: IOS
         CHARACTER (LEN=256) :: BASE_FILE_NAME, &
                                FILE_NAME,OUTPUT_DIRECTORY, &
                                BASE_FILE_DIRECTORY,SCREEN_OUTPUT
!         CHARACTER (LEN=50) :: COMMENT
         CHARACTER (LEN=1) :: Active="Y"
! DECLARATION FOR DBREAD COMMON BLOCK
         CHARACTER (LEN=1024) :: RECLN


!
      SCREEN_OUTPUT = TRIM(FILE_TYPE)//'-'//BASE_FILE_NAME

      FILE_NAME = TRIM(BASE_FILE_DIRECTORY())// &
                      DATA_FILE_CODE//"B"//TRIM(BASE_FILE_NAME )//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=BASIN_DATA_FILE_EXISTS)
      IF(BASIN_DATA_FILE_EXISTS) THEN
!
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=TRIM(OUTPUT_DIRECTORY())//"BC"// &
                                      TRIM(BASIN_BINARY_FILE_NAME), &
                      ACCESS="DIRECT",STATUS="REPLACE",RECL=BASIN_LRECL)
!
!
         IREC = 0
         READ(10,*) DELETE
         DO ! TABLES
            DO ! TABLE RECORDS
               READ(10,'(A)',IOSTAT=IOS) RECLN
!
               IF(IOS /= 0) EXIT ! END OF FILE
!
               IF(RECLN(1:1) == '7') THEN ! END OF TABLE
                  EXIT ! GO TO NEXT TABLE
               ENDIF
!
               RECLN = TRIM(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,IOSTAT=IOS) DELETE, &
                                        BasinAbbrev, &
                                        Active, &
                                        BasinName, &
                                        BasinHIID, &
                                        AnnualBaseYrQuantity, &
                                        MinimumAnnualQuantity, &
                                        MaximumAnnualQuantity
!
               IF(IOS /= 0) THEN
                  WRITE(4,*) TRIM(RECLN)
                  call end_program("Stop:Coal_Main_Prg IOSCMP2")
               ENDIF
               IREC = IREC + 1
               IF(Active /= "N" .AND. DELETE < 8) &
                           ACTIVE_BASINS_IN_BASEFILE = &
                                           ACTIVE_BASINS_IN_BASEFILE + 1
               MAX_BASIN_ID_BASEFILE = MAX(BasinHIID, &
                                           MAX_BASIN_ID_BASEFILE)
               WRITE(11,REC=IREC) DELETE, &
                                  BasinAbbrev, &
                                  Active, &
                                  BasinName, &
                                  BasinHIID, &
                                  AnnualBaseYrQuantity, &
                                  MinimumAnnualQuantity, &
                                  MaximumAnnualQuantity
!
            ENDDO ! TABLE RECORDS
            IF(IOS /= 0) EXIT
         ENDDO ! READ TABLES
         CLOSE(10)
         CLOSE(11)
      ENDIF
      RETURN
      END

!
!
      SUBROUTINE COAL_TRANSPORT_LINKS_MAKEBIN(BASE_FILE_NAME, &
                                                   FILE_CODE)
!

!
!
!      USE COAL_cnw_FILES_READ_INPUT
      USE COAL_TRANSPORT_MAKE_INPUTS
       use end_routine
!
! SIMULATION VARIABLES
!
         INTEGER (KIND=2) ::   UNIT_NUM=10,IREC,DELETE,FILE_POS
         INTEGER (KIND=4) :: IOS
         CHARACTER (LEN=256) :: BASE_FILE_NAME, &
                                FILE_NAME,OUTPUT_DIRECTORY, &
                                BASE_FILE_DIRECTORY,SCREEN_OUTPUT
!         CHARACTER (LEN=50) :: COMMENT
         CHARACTER (LEN=1) :: Active="Y"
         CHARACTER (LEN=2) :: FILE_CODE
! DECLARATION FOR DBREAD COMMON BLOCK
         CHARACTER (LEN=1024) :: RECLN
         LOGICAL (KIND=4) :: FILE_EXISTS

!
      SCREEN_OUTPUT = TRIM(FILE_TYPE)//'-'//BASE_FILE_NAME

      FILE_NAME = TRIM(BASE_FILE_DIRECTORY())// &
                           FILE_CODE//"B"//TRIM(BASE_FILE_NAME )//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      IF(FILE_EXISTS) THEN
!
         OPEN(10,FILE=FILE_NAME)
         IF(FILE_CODE == 'ZD') THEN
            FILE_POS = 2
            GENERIC_LINK_DATA_FILE_EXISTS = .TRUE.
            FILE_NAME =  TRIM(OUTPUT_DIRECTORY())//"BC"// &
                                    TRIM(COAL_LINK_BINARY_FILE_NAME(2))
         ELSE
            FILE_POS = 1
            COAL_LINK_DATA_FILE_EXISTS = .TRUE.
            FILE_NAME =  TRIM(OUTPUT_DIRECTORY())//"BC"// &
                                    TRIM(COAL_LINK_BINARY_FILE_NAME(1))
         ENDIF
         OPEN(11,FILE=FILE_NAME, &
                  ACCESS="DIRECT",STATUS="REPLACE",RECL=COAL_LINK_LRECL)
!
!
         IREC = 0
         READ(10,*) DELETE
         DO ! TABLES
            DO ! TABLE RECORDS
               READ(10,'(A)',IOSTAT=IOS) RECLN
!
               IF(IOS /= 0) EXIT ! END OF FILE
!
               IF(RECLN(1:1) == '7') THEN ! END OF TABLE
                  EXIT ! GO TO NEXT TABLE
               ENDIF
!
! TODO: Remove spaces in variable names.
               RECLN = TRIM(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,IOSTAT=IOS) DELETE, &
                                        Basinto , &
                                          Plant, &
                                        Active, &
                                        BasinHIID, &
                                        PlantEVID, &
                                        TransportPrice, &
                                        TransportPriceEscalation, &
                                        AnnualQuantityLimit, &
                                        TransportVehicles, &
                                        TransportQuantityEscalation
!
               IF(IOS /= 0) THEN
                  WRITE(4,*) TRIM(RECLN)
                 call end_program('Error in transport file CMP6')
               ENDIF
               IREC = IREC + 1
               IF(Active /= "N" .AND. DELETE < 8) &
                       ACTIVE_COAL_LINK_IN_BASEFILE(FILE_POS) = &
                              ACTIVE_COAL_LINK_IN_BASEFILE(FILE_POS) + 1
               MAX_EV_PLANT_ID_BASEFILE(FILE_POS) = &
                     MAX(MAX_EV_PLANT_ID_BASEFILE(FILE_POS),PlantEVID)
               MIN_EV_PLANT_ID_BASEFILE(FILE_POS) = &
                     MIN(MIN_EV_PLANT_ID_BASEFILE(FILE_POS),PlantEVID)
               WRITE(11,REC=IREC) DELETE, &
                                  Basinto , &
                                    Plant, &
                                  Active, &
                                  BasinHIID, &
                                  PlantEVID, &
                                  TransportPrice, &
                                  TransportPriceEscalation, &
                                  AnnualQuantityLimit, &
                                  TransportVehicles, &
                                  TransportQuantityEscalation
!
            ENDDO ! TABLE RECORDS
            IF(IOS /= 0) EXIT
         ENDDO ! READ TABLES
         COAL_TRANSPORT_RECORDS(FILE_POS) = IREC
         CLOSE(10)
         CLOSE(11)
      ENDIF
      RETURN
      END

!
!
      SUBROUTINE PLANT_DEMAND_MAKEBIN(BASE_FILE_NAME)  ! ENTER NAME HERE
!

!
!
      USE PLANT_DEMAND_MAKEBIN_INPUTS
      USE COAL_cnw_FILES_READ_INPUT
      use end_routine
!
! SIMULATION VARIABLES
!
         INTEGER (KIND=2) ::   UNIT_NUM=10,INUNIT,IREC,DELETE
         INTEGER (KIND=4) :: IOS
         CHARACTER (LEN=256) :: BASE_FILE_NAME,OVERLAY_FAMILY_NAME, &
                                FILE_NAME,OUTPUT_DIRECTORY, &
                                BASE_FILE_DIRECTORY,SCREEN_OUTPUT
         CHARACTER (LEN=50) :: COMMENT
         CHARACTER (LEN=1) :: Active="Y"
! DECLARATION FOR DBREAD COMMON BLOCK
         CHARACTER(LEN=1024) :: RECLN

!
      SCREEN_OUTPUT = TRIM(FILE_TYPE)//'-'//BASE_FILE_NAME

      FILE_NAME = TRIM(BASE_FILE_DIRECTORY())// &
                      DATA_FILE_CODE//"B"//TRIM(BASE_FILE_NAME )//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=PLANT_DEMAND_DATA_FILE_EXISTS)
      IF(PLANT_DEMAND_DATA_FILE_EXISTS) THEN
!
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=TRIM(OUTPUT_DIRECTORY())//"BC"// &
                                  TRIM(PLANT_DEMAND_BINARY_FILE_NAME), &
                      ACCESS="DIRECT",STATUS="REPLACE", &
                      RECL=PLANT_DEMAND_LRECL)
!
!
         IREC = 0
         READ(10,*) DELETE
         DO ! TABLES
            DO ! TABLE RECORDS
               READ(10,'(A)',IOSTAT=IOS) RECLN
!
               IF(IOS /= 0) EXIT ! END OF FILE
!
               IF(RECLN(1:1) == '7') THEN ! END OF TABLE
                  EXIT ! GO TO NEXT TABLE
               ENDIF
!
               RECLN = TRIM(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,IOSTAT=IOS) DELETE, &
                                        PlantName, &
                                        Active, &
                                        UnitName, &
                                        EVPlantID, &
                                        EVUnitID, &
                                        HeatContentBaseYear, &
                                        PercentIncrease, &
                                        PercentDecrease, &
                                        HeatLowerBound, &
                                        HeatUpperBound, &
                                        AnnualDemand, &
                                        FractionUnscrubbed, &
                                        SO2BaseYear, &
                                        SO2PercentIncrease, &
                                        SO2PercentDecrease, &
                                        SO2LowerBound, &
                                        SO2UpperBound, &
                                        OnLineMo,OnLineYr, &
                                        OffLineMo,OffLineYr, &
                                        Trans_Group_ID, &
                                        Trans_Group_Name, &
                                        Unit_Capacity, &
                                        SOx_Control_Percent, &
                                        SOx_Control_Date, &
                                        SOx_Control_Capital_Cost, &
                                        SOx_Control_Carrying_Charge, &
                                        SOx_Control_Var_Cost, &
                                        SOx_Control_Fixed_Cost, &
                                        Average_Heatrate, &
                                        State_Location, &
                                        Escalation_SOx_Capital_Cost, &
                                       Escalation_SOx_Carrying_Charge, &
                                        Escalation_SOx_Var_Cost, &
                                        Escalation_SOx_Fixed_Cost
!
               IF(IOS /= 0) THEN
                  WRITE(4,*) TRIM(RECLN)
                  call end_program("Stop: Coal_Main_Prg IOSCMP3")
               ENDIF
               IREC = IREC + 1
               IF(Active /= "N" .AND. DELETE < 8) THEN
                  ACTIVE_PLANT_DEMAND_IN_BASEFILE = &
                                     ACTIVE_PLANT_DEMAND_IN_BASEFILE + 1
               ENDIF
               WRITE(11,REC=IREC) DELETE, &
                                  PlantName, &
                                  Active, &
                                  UnitName, &
                                  EVPlantID, &
                                  EVUnitID, &
                                  HeatContentBaseYear, &
                                  PercentIncrease, &
                                  PercentDecrease, &
                                  HeatLowerBound, &
                                  HeatUpperBound, &
                                  AnnualDemand, &
                                  FractionUnscrubbed, &
                                  SO2BaseYear, &
                                  SO2PercentIncrease, &
                                  SO2PercentDecrease, &
                                  SO2LowerBound, &
                                  SO2UpperBound, &
                                  OnLineMo,OnLineYr, &
                                  OffLineMo,OffLineYr, &
                                  Trans_Group_ID, &
                                  Trans_Group_Name, &
                                  Unit_Capacity, &
                                  SOx_Control_Percent, &
                                  SOx_Control_Date, &
                                  SOx_Control_Capital_Cost, &
                                  SOx_Control_Carrying_Charge, &
                                  SOx_Control_Var_Cost, &
                                  SOx_Control_Fixed_Cost, &
                                  Average_Heatrate, &
                                  State_Location, &
                                  Escalation_SOx_Capital_Cost, &
                                  Escalation_SOx_Carrying_Charge, &
                                  Escalation_SOx_Var_Cost, &
                                  Escalation_SOx_Fixed_Cost

            ENDDO ! TABLE RECORDS
            IF(IOS /= 0) EXIT
         ENDDO ! READ TABLES
         CLOSE(10)
         CLOSE(11)
      ENDIF
      RETURN
      END

!
!
      SUBROUTINE GENERIC_DEMAND_MAKEBIN(BASE_FILE_NAME)
      ! ENTERY NAME HERE
!

!
!
      USE GENERIC_DEMAND_MAKEBIN_INPUTS
      USE COAL_cnw_FILES_READ_INPUT
      use end_routine
!
! SIMULATION VARIABLES
!
         INTEGER (KIND=2) ::   UNIT_NUM=10,INUNIT,IREC,DELETE
         INTEGER (KIND=4) :: IOS
         CHARACTER (LEN=256) :: BASE_FILE_NAME,OVERLAY_FAMILY_NAME, &
                                FILE_NAME,OUTPUT_DIRECTORY, &
                                BASE_FILE_DIRECTORY,SCREEN_OUTPUT
         CHARACTER (LEN=50) :: COMMENT
         CHARACTER (LEN=1) :: Active="Y"
! DECLARATION FOR DBREAD COMMON BLOCK
         CHARACTER(LEN=1024) :: RECLN

!
      SCREEN_OUTPUT = TRIM(FILE_TYPE)//'-'//BASE_FILE_NAME

      FILE_NAME = TRIM(BASE_FILE_DIRECTORY())// &
                      DATA_FILE_CODE//"B"//TRIM(BASE_FILE_NAME )//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=GENERIC_DEMAND_DATA_FILE_EXISTS)
      IF(GENERIC_DEMAND_DATA_FILE_EXISTS) THEN
!
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=TRIM(OUTPUT_DIRECTORY())//"BC"// &
                                TRIM(GENERIC_DEMAND_BINARY_FILE_NAME), &
                      ACCESS="DIRECT",STATUS="REPLACE", &
                      RECL=GENERIC_DEMAND_LRECL)
!
!
         IREC = 0
         READ(10,*) DELETE
         DO ! TABLES
            DO ! TABLE RECORDS
               READ(10,'(A)',IOSTAT=IOS) RECLN
!
               IF(IOS /= 0) EXIT ! END OF FILE
!
               IF(RECLN(1:1) == '7') THEN
                  EXIT ! GO TO NEXT TABLE
               ENDIF
!
               RECLN = TRIM(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,IOSTAT=IOS) DELETE, &
                                        UnitName, &
                                        Active, &
                                        Trans_Group_ID, &
                                        Trans_Group_Name, &
                                        State_Location, &
                                        Unit_Capacity, &
                                        Average_Heatrate, &
                                        HeatContent, &
                                        PercentIncrease, &
                                        PercentDecrease, &
                                        HeatLowerBound, &
                                        HeatUpperBound, &
                                        FractionUnscrubbed, &
                                        SO2Emissions, &
                                        SO2PercentIncrease, &
                                        SO2PercentDecrease, &
                                        SO2LowerBound, &
                                        SO2UpperBound, &
                                        AnnualDemand
!
               IF(IOS /= 0) THEN
                  WRITE(4,*) TRIM(RECLN)
                  call end_program("Stop: Coal_Main_Prg IOSCMP4")
               ENDIF
               IREC = IREC + 1
               IF(Active /= "N" .AND. DELETE < 8) &
                  ACTIVE_PLANT_DEMAND_IN_BASEFILE = &
                               ACTIVE_PLANT_DEMAND_IN_BASEFILE + 1
               WRITE(11,REC=IREC) DELETE, &
                                  UnitName, &
                                  Active, &
                                  Trans_Group_ID, &
                                  Trans_Group_Name, &
                                  State_Location, &
                                  Unit_Capacity, &
                                  Average_Heatrate, &
                                  HeatContent, &
                                  PercentIncrease, &
                                  PercentDecrease, &
                                  HeatLowerBound, &
                                  HeatUpperBound, &
                                  FractionUnscrubbed, &
                                  SO2Emissions, &
                                  SO2PercentIncrease, &
                                  SO2PercentDecrease, &
                                  SO2LowerBound, &
                                  SO2UpperBound, &
                                  AnnualDemand

            ENDDO ! TABLE RECORDS
            IF(IOS /= 0) EXIT
         ENDDO ! READ TABLES
         CLOSE(10)
         CLOSE(11)
      ENDIF
      RETURN
      END

!
!
      ! ENTERY NAME HERE
      SUBROUTINE COAL_CONTRACTS_MAKEBIN(BASE_FILE_NAME)
!

!
!
      USE COAL_CONTRACTS_MAKEBIN_INPUTS
      USE COAL_cnw_FILES_READ_INPUT
      use end_routine
!
! SIMULATION VARIABLES
!
         INTEGER (KIND=2) ::   UNIT_NUM=10,INUNIT,IREC,DELETE
         INTEGER (KIND=4) :: IOS
         CHARACTER (LEN=256) :: BASE_FILE_NAME,OVERLAY_FAMILY_NAME, &
                                FILE_NAME,OUTPUT_DIRECTORY, &
                                BASE_FILE_DIRECTORY,SCREEN_OUTPUT
         CHARACTER (LEN=50) :: COMMENT
         CHARACTER (LEN=1) :: Active="Y"
! DECLARATION FOR DBREAD COMMON BLOCK
         CHARACTER(LEN=1024) :: RECLN

!
      SCREEN_OUTPUT = TRIM(FILE_TYPE)//'-'//BASE_FILE_NAME

      FILE_NAME = TRIM(BASE_FILE_DIRECTORY())// &
                      DATA_FILE_CODE//"B"//TRIM(BASE_FILE_NAME )//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=COAL_CONTRACTS_DATA_FILE_EXISTS)
      IF(COAL_CONTRACTS_DATA_FILE_EXISTS) THEN
!
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=TRIM(OUTPUT_DIRECTORY())//"BC"// &
                                TRIM(COAL_CONTRACTS_BINARY_FILE_NAME), &
                                  ACCESS="DIRECT",STATUS="REPLACE", &
                                  RECL=COAL_CONTRACTS_LRECL)
!
!
         IREC = 0
         READ(10,*) DELETE
         DO ! TABLES
            DO ! TABLE RECORDS
               READ(10,'(A)',IOSTAT=IOS) RECLN
!
               IF(IOS /= 0) EXIT ! END OF FILE
!
               IF(RECLN(1:1) == '7') THEN
                  EXIT ! GO TO NEXT TABLE
               ENDIF
!
               RECLN = TRIM(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,IOSTAT=IOS) DELETE, &
                                        ContractName, &
                                        ContractID, &
                                        Active, &
                                        MineName, &
                                        MineID, &
                                        MineBasinName, &
                                        CoalType, &
                                        PlantID, &
                                        NthContract, &
                                        AnnualQuantity, &
                                        AnnualPrice, &
                                        Comment, &
                                        BasinID, &
                                        ContractType
!
               IF(IOS /= 0) THEN
                  WRITE(4,*) TRIM(RECLN)
              call end_program("Stop requested from Coal_Main_Prg CMP7")
               ENDIF
               IREC = IREC + 1
               IF(Active /= "N" .AND. DELETE < 8) &
                      ACTIVE_GENERIC_DEMAND_IN_BASEFILE = &
                                   ACTIVE_GENERIC_DEMAND_IN_BASEFILE + 1
               WRITE(11,REC=IREC) DELETE, &
                                  ContractName, &
                                  ContractID, &
                                  Active, &
                                  MineName, &
                                  MineID, &
                                  MineBasinName, &
                                  CoalType, &
                                  PlantID, &
                                  NthContract, &
                                  AnnualQuantity, &
                                  AnnualPrice, &
                                  BasinID, &
                                  ContractType

            ENDDO ! TABLE RECORDS
            IF(IOS /= 0) EXIT
         ENDDO ! READ TABLES
         CLOSE(10)
         CLOSE(11)
      ENDIF
      RETURN
      END

!
!
      SUBROUTINE COAL_SO2_INFO_MAKEBIN(BASE_FILE_NAME)

      USE COAL_SO2_INFO_MAKEBIN_INPUTS
      USE COAL_cnw_FILES_READ_INPUT
      use end_routine
      use SO2_Scrubber_COAL_MODEL_DATA
!
! SIMULATION VARIABLES
!
         INTEGER (KIND=2) ::   UNIT_NUM=10,INUNIT,IREC,DELETE
         INTEGER (KIND=4) :: IOS
         CHARACTER (LEN=256) :: BASE_FILE_NAME,OVERLAY_FAMILY_NAME, &
                                FILE_NAME,OUTPUT_DIRECTORY, &
                                BASE_FILE_DIRECTORY,SCREEN_OUTPUT
         CHARACTER (LEN=50) :: COMMENT
         CHARACTER (LEN=1) :: Active="Y"
! DECLARATION FOR DBREAD COMMON BLOCK
         CHARACTER(LEN=1024) :: RECLN

!
      SCREEN_OUTPUT = TRIM(FILE_TYPE)//'-'//BASE_FILE_NAME

      FILE_NAME = TRIM(BASE_FILE_DIRECTORY())// &
                      DATA_FILE_CODE//"B"//TRIM(BASE_FILE_NAME )//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=COAL_SO2_Scrubber_FILE_EXISTS)
      IF(COAL_SO2_Scrubber_FILE_EXISTS) THEN
!
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=TRIM(OUTPUT_DIRECTORY())//"BC"// &
                               TRIM(COAL_SO2_Scrubber_BINARY_FILE), &
                               ACCESS="DIRECT",STATUS="REPLACE", &
                               RECL=COAL_SO2_Scrubber_LRECL)
!
!
         IREC = 0
         READ(10,*) DELETE
         DO ! TABLES
            DO ! TABLE RECORDS
               READ(10,'(A)',IOSTAT=IOS) RECLN
!
               IF(IOS /= 0) EXIT ! END OF FILE
!
!              END OF TABLE ! EXIT AT BOTTOM OF IF
               IF(RECLN(1:1) == '7') THEN
                  EXIT ! GO TO NEXT TABLE
               ENDIF
!
               RECLN = TRIM(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,IOSTAT=IOS) DELETE, &
                                        InputType, &
                                        Active, &
                                        sulphur_market_area, &
                                        sulphur_credit_cost, &
                                       sulphur_credit_cost_escalation, &
                                        SulphurEmissionsLimit, &
                                        scrubber_capital_cost, &
                                        capital_cost_escalation, &
                                        scrubber_fixed_om_cost, &
                                        fixed_om_escalation, &
                                        scrubber_variable_om_cost, &
                                        variable_om_escalation, &
                                        SOxGroupAssignment, &
                                        StateCapVolatility
!
               IF(IOS /= 0) THEN
                  WRITE(4,*) TRIM(RECLN)
                  call end_program("Stop: Coal_Main_Prg CMP9")
               ENDIF
               IREC = IREC + 1
               IF(Active /= "N" .AND. DELETE < 8) &
                      ACTIVE_SO2_Scrubber_BASEFILE = &
                                        ACTIVE_SO2_Scrubber_BASEFILE + 1
               WRITE(11,REC=IREC) DELETE, &
                                  InputType, &
                                  Active, &
                                  sulphur_market_area, &
                                  sulphur_credit_cost, &
                                  sulphur_credit_cost_escalation, &
                                  sulphur_emissions_limit, &
                                  scrubber_capital_cost, &
                                  capital_cost_escalation, &
                                  scrubber_fixed_om_cost, &
                                  fixed_om_escalation, &
                                  scrubber_variable_om_cost, &
                                  variable_om_escalation, &
                                  SOxGroupAssignment, &
                                  StateCapVolatility

            ENDDO ! TABLE RECORDS
            IF(IOS /= 0) EXIT
         ENDDO ! READ TABLES
         CLOSE(10)
         CLOSE(11)
      ENDIF
      RETURN
      END
!***********************************************************************
      SUBROUTINE RESET_COAL_PRICE_MODEL_OL_CODES()
!***********************************************************************
         USE COAL_cnw_FILES_READ_INPUT
         PROCESSING_COALCONTRACTS_FILEOL='BC'
         ACTIVE_COALCONTRACTS_IN_OVL = 0
!
         PROCESSING_PLANT_DEMAND_FILE_OL='BC'
         ACTIVE_PLANT_DEMAND_IN_OVERLAY=0
!
         PROCESSING_COAL_LINK_FILE_OL='BC'
         ACTIVE_COAL_LINK_IN_OVERLAY=0
!
         PROCESSING_BASIN_FILE_OL='BC'
         ACTIVE_BASINS_IN_OVERLAY=0
!
         PROCESSING_COAL_SUPPLY_FILE_OL='BC'
         ACTIVE_FUEL_SUPPLY_OVERLAY=0
!
         PROCESSING_COAL_SO2Scrbr_FILEOL='BC' ! Scrber == Scrubber
         ACTIVE_SO2_Scrubber_BASEFILE=0
         CALL RESET_VECTORS_OL()
      RETURN
      END SUBROUTINE RESET_COAL_PRICE_MODEL_OL_CODES
!***********************************************************************
      LOGICAL FUNCTION READ_BASIN_SUPPLY_SO2_DATA()
!***********************************************************************
      USE COAL_cnw_FILES_READ_INPUT
      use SO2_Scrubber_COAL_MODEL_DATA
      USE COAL_MODEL_INPUT_TO_cnw
      USE ArrayAllocationInterface
      USE CoalModelVectorFileInterface
      USE SO2_Scrubber_COAL_MODEL_DATA
      USE CONVERT_TO_ANNUAL_VALUES
      
      CHARACTER (LEN=256) :: OUTPUT_DIRECTORY
      CHARACTER (LEN=1) :: Active
      INTEGER (KIND=4) :: IOS
      integer (kind=2) :: iArray,DELETE,IREC,nArray,Yr
      REAL (KIND=4) :: temp_array(20)
      LOGICAL (KIND=4) :: RETURN_COAL_VECTOR
      CHARACTER (LEN=50) :: Basinto=" ",Plant=" "
      CHARACTER (LEN=256) :: FILE_NAME
      LOGICAL (KIND=4) :: FILE_EXISTS
      INTEGER (KIND=4) :: FILE_NO
      REAL (KIND=4) :: T_SulphurCreditCost(0:30), &
                       T_SulphurEmissionsLimit(0:30), &
                       T_ScrubberCapitalCost(0:30), &
                       T_ScrubberFixedOMCost(0:30), &
                       T_ScrubberVariableOMCost(0:30), &
                       T_StateCapVolatility

!***********************************************************************
! READS ALL THE COAL DATA
         READ_BASIN_SUPPLY_SO2_DATA = .FALSE.
! CHECK FOR ALL NECESSARY FILES NEED TO ADD ERROR MESSAGES
         IF(.NOT. (BASIN_DATA_FILE_EXISTS .AND. &
                                   COAL_SUPPLY_DATA_FILE_EXISTS)) RETURN
         READ_BASIN_SUPPLY_SO2_DATA = .TRUE.
! READ BASIN INPUT
         IF(PROCESSING_BASIN_FILE_OL == 'OL') THEN
            nArray = ACTIVE_BASINS_IN_OVERLAY
            nMaxBasinID = MAX(MAX_SUPPLY_BASIN_ID_OVLFILE, &
                              MAX_BASIN_ID_OVLFILE)
         ELSE
            nArray = ACTIVE_BASINS_IN_BASEFILE
            nMaxBasinID = MAX(MAX_SUPPLY_BASIN_ID_BASEFILE, &
                              MAX_BASIN_ID_BASEFILE)
         ENDIF
! ALLOCATE ARRAYS
         CALL AllocateArray(Basin_Abbrev,nArray)
         CALL AllocateArray(Basin_Name,nArray)
         CALL AllocateArray(Basin_ID,nArray)
         CALL AllocateArray(Basin_QtyLB,nArray)
         CALL AllocateArray(Basin_QtyUB,nArray)
         CALL AllocateArray(Basin_Annual_BaseYr_Quantity,nArray)
         CALL AllocateArray(BasinAvailable,nMaxBasinID)
         CALL AllocateArray(BasinPointer,nMaxBasinID)
         CALL AllocateArray(Basin_QtyLBByYr,0,30,1,INT(nArray,4))
         CALL AllocateArray(Basin_QtyUBByYr,0,30,1,INT(nArray,4))
         BasinAvailable = .FALSE.
         BasinPointer = -1

         OPEN(12,FILE=TRIM(OUTPUT_DIRECTORY())// &
              PROCESSING_BASIN_FILE_OL//TRIM(BASIN_BINARY_FILE_NAME), &
                      ACCESS="DIRECT",STATUS="OLD",RECL=BASIN_LRECL)
         IREC = 0
         nBasin = 0
         iArray = 1
         DO
            IREC = IREC + 1
            READ(12,REC=IREC,IOSTAT=IOS) DELETE, &
                                  Basin_Abbrev(iArray), &
                                  Active, &
                                  Basin_Name(iArray), &
                                  Basin_ID(iArray), &
                                 Basin_Annual_BaseYr_Quantity(iArray), &
                                  Basin_QtyLB(iArray), &
                                  Basin_QtyUB(iArray)
            IF(IOS /= 0) EXIT
            IF(Active == "N" .OR. DELETE > 7) CYCLE
            BasinAvailable(Basin_ID(iArray)) = .TRUE.
            Basin_QtyLBByYr(0:30,iArray) = &
                                 FILL_ANNUAL_ARRAYS(Basin_QtyLB(iArray))
            Basin_QtyUBByYr(0:30,iArray) = &
                                 FILL_ANNUAL_ARRAYS(Basin_QtyUB(iArray))
            nBasin = nBasin + 1
            iArray = iArray + 1
            IF(iArray > nArray) EXIT
         ENDDO

!         CLOSE(10)
! SO2 & Scrubber INPUT
         StateHasLimits(1:) = .FALSE.
         StateHasLimits(0) = .TRUE.
         IF(.NOT. COAL_SO2_Scrubber_FILE_EXISTS) THEN
            SulphurCreditCost = 0.
            SulphurEmissionsLimit = 99999999.
            ScrubberCapitalCost = 0.
            ScrubberFixedOMCost = 0.
            ScrubberVariableOMCost = 0.
            nStateSO2Limits = 0
            StateCapVolatilityByYear = 0.
            StateAssignedToGroup = 0
         ELSE
            OPEN(10,FILE=TRIM(OUTPUT_DIRECTORY())// &
                 PROCESSING_COAL_SO2Scrbr_FILEOL// &
                             TRIM(COAL_SO2_Scrubber_BINARY_FILE), &
                                  ACCESS="DIRECT",STATUS="OLD", &
                                           RECL=COAL_SO2_Scrubber_LRECL)
            IREC = 0
            iArray = 0
            nStateSO2Limits = -1
            StateAssignedToGroup = -1
            DO
               IREC = IREC + 1
               READ(10,REC=IREC,IOSTAT=IOS) DELETE, &
                                    SO2InputType(iArray), &
                                    Active, &
                                    SulphurMarketArea(iArray), &
                                    T_SulphurCreditCost(0), &
                                  SulphurCreditCostEscalation(iArray), &
                                    T_SulphurEmissionsLimit(0), &
                                    T_ScrubberCapitalCost(0), &
                                    CapitalCostEscalation(iArray), &
                                    T_ScrubberFixedOMCost(0), &
                                    FixedOMEscalation(iArray), &
                                    T_ScrubberVariableOMCost(0), &
                                    VariableOMEscalation(iArray), &
                                    StateAssignedToGroup(iArray), &
                                    T_StateCapVolatility

               IF(IOS /= 0) EXIT
               IF(Active /= "Y" .OR. DELETE > 8) CYCLE
               nStateSO2Limits = nStateSO2Limits + 1
               StateCapVolatilityByYear(0:30,iArray) = &
                                FILL_ANNUAL_ARRAYS(T_StateCapVolatility)
               CALL SO2_ESCALATIONS(T_ScrubberCapitalCost, &
                                    CapitalCostEscalation(iArray))
               ScrubberCapitalCost(0:30,iArray) = &
                                             T_ScrubberCapitalCost(0:30)
               CALL SO2_ESCALATIONS(T_ScrubberFixedOMCost, &
                                    FixedOMEscalation(iArray))
               ScrubberFixedOMCost(0:30,iArray) = &
                                             T_ScrubberFixedOMCost(0:30)
               CALL SO2_ESCALATIONS(T_ScrubberVariableOMCost, &
                                    VariableOMEscalation(iArray))
               ScrubberVariableOMCost(0:30,iArray) = &
                                          T_ScrubberVariableOMCost(0:30)
               CALL SO2_ESCALATIONS(T_SulphurCreditCost, &
                                    SulphurCreditCostEscalation(iArray))
               SulphurCreditCost(0:30,iArray) = &
                                               T_SulphurCreditCost(0:30)
               CALL SO2_ESCALATIONS(T_SulphurEmissionsLimit)
               SulphurEmissionsLimit(0:30,iArray) = &
                                           T_SulphurEmissionsLimit(0:30)
               iArray = iArray + 1
            ENDDO
            CLOSE(10)

         ENDIF
!  END READ COAL SUPPLY  INPUT
         RETURN
      END FUNCTION
!***********************************************************************
      LOGICAL FUNCTION READ_PLANT_CONT_DEMAND_LNK_DATA(BaseYr, &
                                                            EndYr_loc)
!***********************************************************************
      use SO2_Scrubber_COAL_MODEL_DATA
      USE COAL_cnw_FILES_READ_INPUT
      USE COAL_MODEL_INPUT_TO_cnw
      USE ArrayAllocationInterface
      USE CoalModelVectorFileInterface
      USE SO2_Scrubber_COAL_MODEL_DATA
      USE CONVERT_TO_ANNUAL_VALUES
      CHARACTER (LEN=256) :: OUTPUT_DIRECTORY
      CHARACTER (LEN=1) :: Active
      INTEGER (KIND=4) :: IOS
      integer (kind=2) :: iArray,DELETE,IREC,nArray,Yr,BaseYr,EndYr_loc, &
                          iCurPnts,BasinSize,GetMaxGenericCoalBuilds, &
                          nState,iPlnt,ScrubberYr
      INTEGER (KIND=4) :: VectorNum
      REAL (KIND=4) :: PriceEscalation,QuantityEscalation
      REAL (KIND=4) :: temp_array(20),Vector_Values(30), &
                       Escalation_SOx_Capital_Cost, &
                       Escalation_SOx_Carrying_Charge, &
                       Escalation_SOx_Var_Cost, &
                       Escalation_SOx_Fixed_Cost, &
                       PercentIncreaseSO2, &
                       PercentDecreaseSO2, &
                       MaxSO2OfBlend, &
                       SO2LowerBound, &
                       MaxSO2OfnAnyCoal
      LOGICAL (KIND=4) :: RETURN_COAL_VECTOR,VectorContainsValues

      INTEGER (KIND=2) :: OnLineMo,OnLineYr,OffLineMo,OffLineYr,iZero
      CHARACTER (LEN=50) :: Basinto=" ",Plant=" ", &
                            TransportVehicles
      CHARACTER (LEN=256) :: FILE_NAME
      LOGICAL (KIND=4) :: FILE_EXISTS,PLANT_ACTIVE_IN_IM
      INTEGER (KIND=4) :: FILE_NO,BasinHIID,PlantEVID
      CHARACTER (LEN=50), ALLOCATABLE :: T_Route_Name(:), &
                                         T_Transport_Vehicles(:)
      INTEGER (KIND=4), ALLOCATABLE :: T_Basin_HI_ID(:), &
                                       T_Plant_EV_ID(:)
      REAL (KIND=4), ALLOCATABLE :: Transport_Price_By_Year(:,:), &
                                   Transport_Quant_Limit_By_Year(:,:)
!
         READ_PLANT_CONT_DEMAND_LNK_DATA = .FALSE.
         IF(.NOT. (COAL_LINK_DATA_FILE_EXISTS .AND. &
                      (PLANT_DEMAND_DATA_FILE_EXISTS .OR. &
                               COAL_CONTRACTS_DATA_FILE_EXISTS))) RETURN
         READ_PLANT_CONT_DEMAND_LNK_DATA = .TRUE.
!
!***********************************************************************
! FIRST PASS READ OF COAL TRANSPORTATION LINKS INPUT
         nTransportLinks = 0
         FILE_NO = 1
         IF(PROCESSING_COAL_LINK_FILE_OL(1) == 'OL') THEN
            nArray = ACTIVE_COAL_LINK_IN_OVERLAY(1)
            EVPlantIDMax = MAX_EV_PLANT_ID_OVLFILE(1)
            EVPlantIDMin = MIN_EV_PLANT_ID_OVLFILE(1)
         ELSE
            nArray = ACTIVE_COAL_LINK_IN_BASEFILE(1)
            EVPlantIDMax = MAX_EV_PLANT_ID_BASEFILE(1)
            EVPlantIDMin = MIN_EV_PLANT_ID_BASEFILE(1)
         ENDIF
! ALLOCATE ARRAYS
         CALL AllocateArray(PlantToBasinLinkActive, &
                                              EVPlantIDMin,EVPlantIDMax)
         PlantToBasinLinkActive = .FALSE.
         FILE_NAME = TRIM(OUTPUT_DIRECTORY())// &
                       PROCESSING_COAL_LINK_FILE_OL(FILE_NO)// &
                            TRIM(COAL_LINK_BINARY_FILE_NAME(FILE_NO))
         OPEN(11,FILE=FILE_NAME, &
               ACCESS="DIRECT",STATUS="OLD",RECL=COAL_LINK_LRECL)
         nTransportLinks = nArray
         iArray = 0
         IREC = 0
         DO
            IREC = IREC + 1
            IF(IREC > COAL_TRANSPORT_RECORDS(FILE_NO)) EXIT
            READ(11,REC=IREC,IOSTAT=IOS) DELETE, &
                               Basinto , &
                                 Plant, &
                               Active, &
                               BasinHIID, &
                               PlantEVID

            IF(IOS /= 0) EXIT
            IF(Active == "N" .OR. DELETE > 7) CYCLE
            IF( .NOT. BasinAvailable(BasinHIID)) CYCLE
            IF(.NOT. (PlantEVID >= EVPlantIDMin .AND. &
                                     PlantEVID <= EVPlantIDMax)) CYCLE
            PlantToBasinLinkActive(PlantEVID) = .TRUE.
            iArray = iArray + 1
            IF(iArray > nArray) EXIT
         ENDDO
         nTransportLinks = iArray

! END READ COAL TRANSPORTTION LINKS INPUT
!***********************************************************************
! READ PLANT DEMAND INPUT
         IF(PLANT_DEMAND_DATA_FILE_EXISTS) THEN
            IF(PROCESSING_PLANT_DEMAND_FILE_OL == 'OL') THEN
               nMaxExistingCoalDemand = ACTIVE_PLANT_DEMAND_IN_OVERLAY
            ELSE
               nMaxExistingCoalDemand = ACTIVE_PLANT_DEMAND_IN_BASEFILE
            ENDIF

            nMaxGenericCoalDemand = nGenericCoalDemand
            nArray = nMaxExistingCoalDemand + nMaxGenericCoalDemand
! ALLOCATE ARRAYS
            CALL AllocateArray(AddScrubber,nArray)
            CALL AllocateArray(Plant_Name,nArray)
            CALL AllocateArray(Plant_Unit_Name,nArray)
            CALL AllocateArray(EV_Plant_ID,nArray)
            CALL AllocateArray(EV_Unit_ID,nArray)
            CALL AllocateArray(Plant_Heat_Lower_Bound,nArray)
            CALL AllocateArray(Plant_Heat_Upper_Bound,nArray)
            CALL AllocateArray(Plant_Annual_Demand,nArray)
            CALL AllocateArray(Plant_Fraction_Unscrubbed,nArray)
            CALL AllocateArray(Plant_Heat_Content_Base_Year,nArray)
            CALL AllocateArray(Plant_Percent_Increase,nArray)
            CALL AllocateArray(Plant_Percent_Decrease,nArray)
            CALL AllocateArray( &
                       Plant_Annual_Demand_By_Year,0,30,1,INT(nArray,4))
            CALL AllocateArray( &
                              MaxSO2InCoalBlend,0,30,1,INT(nArray,4))
            CALL AllocateArray( &
                          MaxSO2ForAnyCoalInBlend,0,30,1,INT(nArray,4))
           CALL AllocateArray(SOxCapitalCostByYear,0,30,1,INT(nArray,4))
            CALL AllocateArray( &
                           SOxCarryingChargeByYear,0,30,1,INT(nArray,4))
            CALL AllocateArray(SOxVarCostByYear,0,30,1,INT(nArray,4))
            CALL AllocateArray(SOxFixedCostByYear,0,30,1,INT(nArray,4))
            CALL AllocateArray(CoalDemandLinkToCLPlants,nArray)
            CALL AllocateArray(TransGroupID,nArray)
            CALL AllocateArray(TransGroupName,nArray)
            CALL AllocateArray(UnitCapacity,nArray)
            CALL AllocateArray(UnitIMEnergy,nArray)
            UnitIMEnergy = 0.
            CALL AllocateArray(AverageHeatrate,nArray)
            CALL AllocateArray(SOxControlPercent,nArray)
            CALL AllocateArray(SOxControlDate,nArray)
            CALL AllocateArray(SOxControlVarCost,nArray)
            CALL AllocateArray(SOxControlFixedCost,nArray)
            CALL AllocateArray(OnLineDate,nArray)
            CALL AllocateArray(OffLineDate,nArray)
            CALL AllocateArray(SOxControlCapitalCost,nArray)
            CALL AllocateArray(SOxControlCarryingCharge,nArray)
            CALL AllocateArray(StateLocation,nArray)
            CALL AllocateArray(LinkToPlantLinkActive, &
                                              EVPlantIDMin,EVPlantIDMax)
            CALL AllocateArray(Plant_ID,nArray)
            CALL AllocateArray(UnitToPlantPtr,nArray)
            CALL AllocateArray(PlantListName,nArray)
            CALL AllocateArray(SO2MaxInBlend,nArray)
            CALL AllocateArray(SO2MaxInAnyCoal,nArray)
            CALL AllocateArray(OptUnScrubbedFrac,nArray)
            CALL AllocateArray( &
                     PlantFractionUnscrubbedByYear,0,30,1,INT(nArray,4))
            CALL AllocateArray(ExportNonUtilityDemand,nArray)
            CALL AllocateArray(UnitLinkToState,nArray)
            CALL AllocateArray(EmissionTablePointer,nArray)

            LinkToPlantLinkActive = .FALSE.
            ExportNonUtilityDemand = .FALSE.
            UnitLinkToState = -1

            OPEN(10,FILE=TRIM(OUTPUT_DIRECTORY())// &
                 PROCESSING_PLANT_DEMAND_FILE_OL// &
                                  TRIM(PLANT_DEMAND_BINARY_FILE_NAME), &
                                    ACCESS="DIRECT",STATUS="OLD", &
                                    RECL=PLANT_DEMAND_LRECL)
            IREC = 0
            nCoalDemand = 0
            nExistingPlant = 0
            iArray = 1
            DO
               IREC = IREC + 1
               READ(10,REC=IREC,IOSTAT=IOS) DELETE, &
                                 Plant_Name(iArray), &
                                 Active, &
                                 Plant_Unit_Name(iArray), &
                               EV_Plant_ID(iArray),EV_Unit_ID(iArray), &
                                 Plant_Heat_Content_Base_Year(iArray), &
                                 Plant_Percent_Increase(iArray), &
                                 Plant_Percent_Decrease(iArray), &
                                 Plant_Heat_Lower_Bound(iArray), &
                                 Plant_Heat_Upper_Bound(iArray), &
                                 Plant_Annual_Demand(iArray), &
                                 Plant_Fraction_Unscrubbed(iArray), &
                                 MaxSO2OfBlend, &
                                 PercentIncreaseSO2, &
                                 PercentDecreaseSO2, &
                                 SO2LowerBound, &
                                 MaxSO2OfnAnyCoal, &
                                 OnLineMo,OnLineYr, &
                                 OffLineMo,OffLineYr, &
                                 TransGroupID(iArray), &
                                 TransGroupName(iArray), &
                                 UnitCapacity(iArray), &
                                 SOxControlPercent(iArray), &
                                 SOxControlDate(iArray), &
                                 SOxControlCapitalCost(iArray), &
                                 SOxControlCarryingCharge(iArray), &
                                 SOxControlVarCost(iArray), &
                                 SOxControlFixedCost(iArray), &
                                 AverageHeatrate(iArray), &
                                 StateLocation(iArray), &
                                 Escalation_SOx_Capital_Cost, &
                                 Escalation_SOx_Carrying_Charge, &
                                 Escalation_SOx_Var_Cost, &
                                 Escalation_SOx_Fixed_Cost

               IF(IOS /= 0) EXIT
               IF(IREC > 1199) THEN
                  IREC = IREC
               ENDIF
               IF(Active == "N" .OR. DELETE > 7) CYCLE
               IF(.NOT. (EV_Plant_ID(iArray)>=EVPlantIDMin .AND. &
                               EV_Plant_ID(iArray)<=EVPlantIDMax)) CYCLE
               IF(.NOT. &
                     (PlantToBasinLinkActive(EV_Plant_ID(iArray))))CYCLE
! This unit isn't in the active units in IM then by pass.
              IF(INDEX(TransGroupName(iArray),'Non Utility') /= 0 .OR. &
                       INDEX(TransGroupName(iArray),'Export') /= 0) THEN
                  ExportNonUtilityDemand(iArray) = .TRUE.
               ELSE
                  IF(.NOT. PLANT_ACTIVE_IN_IM(EV_Unit_ID(iArray))) CYCLE
               ENDIF
               ! ACTIVE PLANT IDS
               LinkToPlantLinkActive(EV_Plant_ID(iArray)) = .TRUE.
! create a list of plants that has a pointer from unit to plant
               DO iPlnt = 1, nExistingPlant
                  IF(Plant_ID(iPlnt) == EV_Plant_ID(iArray)) EXIT
               ENDDO
               IF(iPlnt > nExistingPlant) THEN
                  nExistingPlant = nExistingPlant + 1
                  Plant_ID(nExistingPlant) = EV_Plant_ID(iArray)
                  UnitToPlantPtr(iArray) = nExistingPlant
                  PlantListName(nExistingPlant) = Plant_Name(iArray)
               ELSE
                  UnitToPlantPtr(iArray) = iPlnt
               ENDIF
!
               DO nState= 1, nStateSO2Limits
                  IF(StateLocation(iArray) == &
                                         SulphurMarketArea(nState)) THEN
                     StateHasLimits(nState) = .TRUE.
                     UnitLinkToState(iArray) = nState
                     EXIT
                  ENDIF
               ENDDO
!
               ScrubberYr = SOxControlDate(iArray)/100 + 2000
           OptUnScrubbedFrac(iArray) = .FALSE. ! use unscrubbed fraction
      IF(ScrubberYr > BaseYr+30) THEN ! condition added 20110719 by AGT
OptUnScrubbedFrac(iArray) = .TRUE. ! allow change to unscrubbed fraction
                  PlantFractionUnscrubbedByYear(:,iArray) = 1.
                  Plant_Fraction_Unscrubbed(iArray) = 1.
               ELSEIF(ScrubberYr > EndYr_loc) THEN
                  PlantFractionUnscrubbedByYear(:,iArray) = 1.
                  Plant_Fraction_Unscrubbed(iArray) = 1.
               ELSEIF(ScrubberYr <= BaseYr) THEN
                  PlantFractionUnscrubbedByYear(:,iArray) = &
                                       Plant_Fraction_Unscrubbed(iArray)
               ELSE ! to reach here the scrubber is installed
               !in the first year or the last year.
   ! the array is false up to the year before the scrubber is installed.
                  Yr = min(ScrubberYr - BaseYr - 1,29)
                  PlantFractionUnscrubbedByYear(0:Yr,iArray) = 1.
                  PlantFractionUnscrubbedByYear(Yr+1:,iArray) = &
                                       Plant_Fraction_Unscrubbed(iArray)
               ENDIF
               IF(ExportNonUtilityDemand(iArray)) &
         OptUnScrubbedFrac(iArray) = .FALSE. ! use unscrubbed fraction
!
               OnLineDate(iArray) = 100*(OnLineYr-1900) + OnLineMo
               OffLineDate(iArray) = 100*(OffLineYr-1900) + OffLineMo
!
               MaxSO2InCoalBlend(0:30,iArray) = &
                                  FILL_ANNUAL_ARRAYS(MaxSO2OfBlend, &
                                                     PercentIncreaseSO2)
               MaxSO2ForAnyCoalInBlend(0:30,iArray) = &
                               FILL_ANNUAL_ARRAYS(MaxSO2OfnAnyCoal, &
                                                     PercentDecreaseSO2)
               Plant_Annual_Demand_By_Year(0:30,iArray) = &
                         FILL_ANNUAL_ARRAYS(Plant_Annual_Demand(iArray))
               SOxCapitalCostByYear(0:30,iArray) = &
                     FILL_ANNUAL_ARRAYS(SOxControlCapitalCost(iArray), &
                                          Escalation_SOx_Capital_Cost)
               SOxCarryingChargeByYear(0:30,iArray) = &
                  FILL_ANNUAL_ARRAYS(SOxControlCarryingCharge(iArray), &
                                       Escalation_SOx_Carrying_Charge)
               SOxVarCostByYear(0:30,iArray) = &
                         FILL_ANNUAL_ARRAYS(SOxControlVarCost(iArray), &
                                            Escalation_SOx_Var_Cost)
               SOxFixedCostByYear(0:30,iArray) = &
                       FILL_ANNUAL_ARRAYS(SOxControlFixedCost(iArray), &
                                            Escalation_SOx_Fixed_Cost)

               IF(OnLineYr > BaseYr+1) THEN
                  iZero = MIN(30,OnLineYr-(BaseYr+1))
                  Plant_Annual_Demand_By_Year(1:iZero,iArray) = 0.
               ENDIF
               IF(OffLineYr < BaseYr+30) THEN
                  iZero = MIN(30,OffLineYr+1-BaseYr)
                  Plant_Annual_Demand_By_Year(iZero:30,iArray) = 0.
               ENDIF
               nCoalDemand = nCoalDemand + 1
               iArray = iArray + 1
               IF(iArray > nMaxExistingCoalDemand) EXIT
            ENDDO
            CLOSE(10)
            nCoalDemandExistingUnits = nCoalDemand
            nExistingPlantPlusAdds = nExistingPlant
         ENDIF
!  END READ PLANT DEMAND INPUT
!***********************************************************************
! SECOND PASS READ OF TRANSPORTATION LINKS
! triming the transportation links.
! ALLOCATE ARRAYS
!          IF(PROCESSING_COAL_LINK_FILE_OL(1) == 'OL') THEN
!             nGenericTransLinks = ACTIVE_COAL_LINK_IN_OVERLAY(1)
!          ELSE
!             nGenericTransLinks = ACTIVE_COAL_LINK_IN_BASEFILE(1)
!          ENDIF
         nArray = nTransportLinks
         CALL AllocateArray(T_Route_Name,nArray)
         CALL AllocateArray(T_Basin_HI_ID,nArray)
         CALL AllocateArray(T_Plant_EV_ID,nArray)
         CALL AllocateArray(T_Transport_Vehicles,nArray)
         CALL AllocateArray(BasinWithLinkstoPlant,nMaxBasinID)
         CALL AllocateArray( &
                   Transport_Quant_Limit_By_Year,1,INT(nArray,4),0,30)
       CALL AllocateArray(Transport_Price_By_Year,1,INT(nArray,4),0,30)
         BasinWithLinkstoPlant = .FALSE.
         nTransportLinks = 0
         iArray = 1
         IREC = 0
         DO
            IREC = IREC + 1
            IF(IREC > COAL_TRANSPORT_RECORDS(1)) EXIT
            READ(11,REC=IREC,IOSTAT=IOS) DELETE, &
                             Basinto , &
                                Plant, &
                             Active, &
                             T_Basin_HI_ID(iArray), &
                             T_Plant_EV_ID(iArray), &
                             Transport_Price_By_Year(iArray,0), &
                             PriceEscalation, &
                           Transport_Quant_Limit_By_Year(iArray,0), &
                             T_Transport_Vehicles(iArray), &
                             QuantityEscalation
            IF(IOS /= 0) EXIT
            IF(Active == "N" .OR. DELETE > 7) CYCLE
            IF( .NOT. BasinAvailable(T_Basin_HI_ID(iArray))) CYCLE
            IF(.NOT. (T_Plant_EV_ID(iArray)>=EVPlantIDMin .AND. &
                             T_Plant_EV_ID(iArray)<=EVPlantIDMax)) CYCLE
            IF(.NOT. PlantToBasinLinkActive(T_Plant_EV_ID(iArray)))CYCLE
            IF(.NOT. LinkToPlantLinkActive(T_Plant_EV_ID(iArray))) CYCLE
            BasinWithLinkstoPlant(T_Basin_HI_ID(iArray)) = .TRUE.
            T_Route_Name(iArray) = TRIM(Basinto)//"-"//TRIM(Plant)
! quantity changes by year
            CALL ROUTE_ESCALATIONS(iArray, &
                                   QuantityEscalation, &
                                   Transport_Quant_Limit_By_Year)
! transportation price
            CALL ROUTE_ESCALATIONS(iArray, &
                                   PriceEscalation, &
                                   Transport_Price_By_Year)
            nTransportLinks = nTransportLinks + 1
            iArray = iArray + 1
            IF(iArray > nArray) EXIT
         ENDDO
         CLOSE(11,IOSTAT=IOS)
         nTransportLinksExistingUnits = nTransportLinks
         IF(PROCESSING_COAL_LINK_FILE_OL(2) == 'OL') THEN
            nGenericTransLinks = ACTIVE_COAL_LINK_IN_OVERLAY(2)
         ELSE
            nGenericTransLinks = ACTIVE_COAL_LINK_IN_BASEFILE(2)
         ENDIF
         iArray = nTransportLinks
         nArray = nTransportLinks + nGenericTransLinks
         CALL AllocateArray(Route_Name,nArray)
         CALL AllocateArray(Basin_HI_ID,nArray)
         CALL AllocateArray(Plant_EV_ID,nArray)
         CALL AllocateArray(Transport_Vehicles,nArray)
         CALL AllocateArray(Coal_Link_ID,nArray)
         CALL AllocateArray(Transport_Price,nArray)
         CALL AllocateArray(Annual_Quantity_Limit,nArray)
         CALL AllocateArray( &
                   Route_Quantity_Limit_By_Year,1,INT(nArray,4),0,30)
         CALL AllocateArray(Route_Price_By_Year,1,INT(nArray,4),0,30)
         Route_Name(1:iArray) = T_Route_Name(1:iArray)
         Basin_HI_ID(1:iArray) = T_Basin_HI_ID(1:iArray)
         Plant_EV_ID(1:iArray) = T_Plant_EV_ID(1:iArray)
         Transport_Vehicles(1:iArray) = T_Transport_Vehicles(1:iArray)
         Route_Quantity_Limit_By_Year(1:nArray,0:30) = &
                         Transport_Quant_Limit_By_Year(1:nArray,0:30)
         Route_Price_By_Year(1:nArray,0:30) = &
                                  Transport_Price_By_Year(1:nArray,0:30)
         DEALLOCATE(T_Route_Name, &
                    T_Transport_Vehicles, &
                    T_Basin_HI_ID, &
                    T_Plant_EV_ID, &
                    Transport_Quant_Limit_By_Year, &
                    Transport_Price_By_Year)
!***********************************************************************
! READ COAL CONTRACTS  INPUT
         IF(COAL_CONTRACTS_DATA_FILE_EXISTS) THEN
            IF(PROCESSING_COALCONTRACTS_FILEOL == 'OL') THEN
               nCoalContracts = ACTIVE_COALCONTRACTS_IN_OVL
            ELSE
               nCoalContracts = ACTIVE_COALCONTRACTS_IN_BF
            ENDIF
            nArray = nCoalContracts
! ALLOCATE ARRAYS
            CALL AllocateArray(Contract_Name,nArray)
            CALL AllocateArray(Contract_Mine_Name,nArray)
            CALL AllocateArray(Contract_Mine_Basin_Name,nArray)
            CALL AllocateArray(Contract_Coal_Type,nArray)
            CALL AllocateArray(Contract_ID,nArray)
            CALL AllocateArray(Contract_Mine_ID,nArray)
            CALL AllocateArray(Nth_Contract,nArray)
            CALL AllocateArray(Contract_Plant_ID,nArray)
            CALL AllocateArray(Contract_Annual_Quantity,30_2,nArray)
            CALL AllocateArray(Contract_Annual_Price,30_2,nArray)
            CALL AllocateArray(Contract_Basin_ID,nArray)
            CALL AllocateArray(Contract_Type,nArray)

            OPEN(10,FILE=TRIM(OUTPUT_DIRECTORY())// &
                 PROCESSING_COALCONTRACTS_FILEOL// &
                                TRIM(COAL_CONTRACTS_BINARY_FILE_NAME), &
                                  ACCESS="DIRECT",STATUS="OLD", &
                                              RECL=COAL_CONTRACTS_LRECL)
            IREC = 0
            nCoalContracts = 0
            iArray = 1
            DO
               IREC = IREC + 1
               READ(10,REC=IREC,IOSTAT=IOS) DELETE, &
                                     Contract_Name(iArray), &
                                     Contract_ID(iArray), &
                                     Active, &
                                     Contract_Mine_Name(iArray), &
                                     Contract_Mine_ID(iArray), &
                                     Contract_Mine_Basin_Name(iArray), &
                                     Contract_Coal_Type(iArray), &
                                     Contract_Plant_ID(iArray), &
                                     Nth_Contract(iArray), &
                                   Contract_Annual_Quantity(:,iArray), &
                                     Contract_Annual_Price(:,iArray), &
                                     Contract_Basin_ID(iArray), &
                                     Contract_Type(iArray)

               IF(IOS /= 0) EXIT
               IF(Active == "N" .OR. DELETE > 7) CYCLE
               IF(.NOT. (Contract_Plant_ID(iArray)>=EVPlantIDMin .AND. &
                         Contract_Plant_ID(iArray)<=EVPlantIDMax)) CYCLE
             IF(.NOT.LinkToPlantLinkActive(Contract_Plant_ID(iArray))) &
                                                                   CYCLE
               nCoalContracts = nCoalContracts + 1
               iArray = iArray + 1
               IF(iArray > nArray) EXIT
            ENDDO

            CLOSE(10)
         ENDIF

! Trim basins
         BasinSize = SIZE(BasinAvailable)
         IREC = 0
         nBasin = 0
         iArray = 1
         BasinPointer = -1
         DO
            IREC = IREC + 1
            READ(12,REC=IREC,IOSTAT=IOS) DELETE, &
                                  Basin_Abbrev(iArray), &
                                  Active, &
                                  Basin_Name(iArray), &
                                  Basin_ID(iArray), &
                                 Basin_Annual_BaseYr_Quantity(iArray), &
                                  Basin_QtyLB(iArray), &
                                  Basin_QtyUB(iArray)
            IF(IOS /= 0) EXIT
            IF(Active == "N" .OR. DELETE > 7) CYCLE
            IF( .NOT. BasinWithLinkstoPlant(Basin_ID(iArray))) CYCLE
            BasinPointer(Basin_ID(iArray)) = iArray
            Basin_QtyLBByYr(0:30,iArray) = &
                                 FILL_ANNUAL_ARRAYS(Basin_QtyLB(iArray))
            Basin_QtyUBByYr(0:30,iArray) = &
                                 FILL_ANNUAL_ARRAYS(Basin_QtyUB(iArray))
            nBasin = nBasin + 1
            iArray = iArray + 1
            IF(iArray > BasinSize) EXIT
         ENDDO

         CLOSE(12)
!***********************************************************************
! READ COAL SUPPLY  INPUT
         IF(PROCESSING_COAL_SUPPLY_FILE_OL == 'OL') THEN
            nArray = ACTIVE_FUEL_SUPPLY_OVERLAY
         ELSE
            nArray = ACTIVE_FUEL_SUPPLY_BASEFILE
         ENDIF
! ALLOCATE ARRAYS
         IF(ALLOCATED(Supply_Production_By_Year)) THEN
            IF(SIZE(Supply_Production_By_Year)==20*nArray*31) &
                     DEALLOCATE (Supply_Production_By_Year, &
                                 Supply_Cost_By_Year)
         ENDIF
         IF(.NOT. ALLOCATED(Supply_Production_By_Year)) THEN
            ALLOCATE (Supply_Production_By_Year(20,nArray,0:30), &
                      Supply_Cost_By_Year(20,nArray,0:30))
         ENDIF
         CALL AllocateArray(Supply_Description,nArray)
         CALL AllocateArray(Supply_Basin_ID,nArray)
         CALL AllocateArray(Supply_Fuel_Type_ID,nArray)
         CALL AllocateArray(Supply_Heat_Content,nArray)
         CALL AllocateArray(SupplyLastCost,nArray)
         CALL AllocateArray(Supply_SO2_Rate,nArray)
         CALL AllocateArray(Supply_CO2_Rate,nArray)
         CALL AllocateArray(Supply_Quantity_Consumed_BaseYr,nArray)
         CALL AllocateArray(Supply_Min_Annual_Production,nArray)
         CALL AllocateArray(Supply_Max_Annual_Production,nArray)
         CALL AllocateArray(Supply_Curve_Points,nArray)
         CALL AllocateArray(Supply_Production,20_2,nArray)
         CALL AllocateArray(Supply_Cost,20_2,nArray)
         CALL AllocateArray(Supply_Production_Escalation,nArray)
         CALL AllocateArray(Supply_Prod_Cost_Escalation,nArray)
         OPEN(10,FILE=TRIM(OUTPUT_DIRECTORY())// &
              PROCESSING_COAL_SUPPLY_FILE_OL// &
                                   TRIM(COAL_SUPPLY_BINARY_FILE_NAME), &
                ACCESS="DIRECT",STATUS="OLD",RECL=COAL_SUPPLY_LRECL)
         IREC = 0
         nCoalSupply = 0
         iArray = 1
         DO
            Supply_Production(:,iArray) = 0.
            Supply_Cost(:,iArray) = 0.
            IREC = IREC + 1
            READ(10,REC=IREC,IOSTAT=IOS) DELETE, &
                            Supply_Description(iArray), &
                            Active, &
                            Supply_Basin_ID(iArray), &
                            Supply_Fuel_Type_ID(iArray), &
                            Supply_Heat_Content(iArray), &
                            Supply_SO2_Rate(iArray), &
                            Supply_CO2_Rate(iArray), &
                            Supply_Quantity_Consumed_BaseYr(iArray), &
                            Supply_Min_Annual_Production(iArray), &
                            Supply_Max_Annual_Production(iArray), &
                            Supply_Curve_Points(iArray), &
                            Supply_Production_Escalation(iArray), &
                            Supply_Prod_Cost_Escalation(iArray), &
                            Supply_Production(:,iArray), &
                            Supply_Cost(:,iArray), &
                            SupplyLastCost(iArray)

            IF(IOS /= 0) EXIT
            IF(Active == "N" .OR. DELETE > 7) CYCLE
            IF( .NOT. BasinAvailable(Supply_Basin_ID(iArray))) CYCLE
            IF( .NOT. BasinWithLinkstoPlant(Supply_Basin_ID(iArray))) &
                                                                   CYCLE
! added 4/78/11 to eliminate supply limits
         ! TODO: Remove code in IF block
         if(.false.) then
            IF(Supply_Curve_Points(iArray) == 20) THEN
               Supply_Production(20,iArray) = &
                                Supply_Max_Annual_Production(iArray)
               Supply_Cost(20,iArray) = SupplyLastCost(iArray)
            ELSE
               iCurPnts = MIN(Supply_Curve_Points(iArray) + 1,20)
               Supply_Production(iCurPnts,iArray) = &
                                Supply_Max_Annual_Production(iArray)
                Supply_Cost(iCurPnts,iArray) = SupplyLastCost(iArray)
                Supply_Curve_Points(iArray) = iCurPnts
            ENDIF
          endif
! end of eliminating supply limits
            nCoalSupply = nCoalSupply + 1
            iArray = iArray + 1
            IF(iArray > nArray) EXIT
         ENDDO
         CLOSE(10)
!
! Escalate quantities
!
         DO iArray = 1, nCoalSupply
            Supply_Production_By_Year(:,iArray,:) = 0.
            temp_array(:) = 0
            iCurPnts = Supply_Curve_Points(iArray)
            temp_array(1:iCurPnts)=Supply_Production(1:iCurPnts,iArray)
            Supply_Production_By_Year(:,iArray,0) =  temp_array(:)
            IF(Supply_Production_Escalation(iArray) >= 0.) THEN
               DO Yr = 1, 30
                  Supply_Production_By_Year(1:iCurPnts,iArray,Yr) = &
                    (1.+ Supply_Production_Escalation(iArray)/100.) &
                     * Supply_Production_By_Year(1:iCurPnts,iArray,Yr-1)
               ENDDO
            ELSE  ! A VECTOR OF ESCALATION RATES IS BEING USED
               VectorNum = ABS(Supply_Production_Escalation(iArray))
               IF(RETURN_COAL_VECTOR(Vector_Values,VectorNum)) THEN
!                 The vector is a list of pointers to a vector that has
!                 20 values in it
               ENDIF
                  DO Yr = 1, 30
                     Supply_Production_By_Year(1:iCurPnts,iArray,Yr) = &
        Vector_Values(Yr) &  ! 1 has been added to the percentage &
                      *Supply_Production_By_Year(1:iCurPnts,iArray,Yr-1)
                  ENDDO
            ENDIF
         ENDDO
!
! Escalate costs!
         DO iArray = 1, nCoalSupply
            Supply_Cost_By_Year(:,iArray,:) = 0.
            temp_array(:) = 0
            iCurPnts = Supply_Curve_Points(iArray)
            temp_array(1:iCurPnts) = Supply_Cost(1:iCurPnts,iArray)
            Supply_Cost_By_Year(:,iArray,0) =  temp_array(:)
            IF(Supply_Prod_Cost_Escalation(iArray) >= 0.) THEN
               DO Yr = 1, 30
                  Supply_Cost_By_Year(1:iCurPnts,iArray,Yr) = &
                    (1.+ Supply_Prod_Cost_Escalation(iArray)/100.) &
                           * Supply_Cost_By_Year(1:iCurPnts,iArray,Yr-1)
               ENDDO
            ELSE  ! A VECTOR IS BEING USED
               VectorNum =ABS(Supply_Prod_Cost_Escalation(iArray))
               IF(RETURN_COAL_VECTOR(Vector_Values,VectorNum)) THEN
!                 The vector is a list of pointers to a vector that has
!                 20 values in it
               ENDIF
                  DO Yr = 1, 30
                     Supply_Cost_By_Year(1:iCurPnts,iArray,Yr) = &
             Vector_Values(Yr) &  ! 1 has been added to the percnetage &
                           * Supply_Cost_By_Year(1:iCurPnts,iArray,Yr-1)
                  ENDDO
            ENDIF
         ENDDO
!  END READ COAL INPUTS  INPUT
         RETURN
      END FUNCTION
!***********************************************************************
      LOGICAL FUNCTION READ_GEN_PLANT_DEMAND_LINK_DATA(BaseYr)
!***********************************************************************
      USE COAL_cnw_FILES_READ_INPUT
      USE COAL_MODEL_INPUT_TO_cnw
      USE ArrayAllocationInterface
      USE CoalModelVectorFileInterface
      USE CONVERT_TO_ANNUAL_VALUES
      CHARACTER (LEN=256) :: OUTPUT_DIRECTORY
      CHARACTER (LEN=1) :: Active
      INTEGER (KIND=4) :: IOS
      integer (kind=2) :: iArray,DELETE,IREC,nArray,Yr,BaseYr, &
                          GetMaxGenericCoalBuilds,TransGrpID,PathPos
      INTEGER (KIND=4) :: GenPlantID,VectorNum
      REAL (KIND=4) :: temp_array(20),Vector_Values(30)
      LOGICAL (KIND=4) :: RETURN_COAL_VECTOR

      INTEGER (KIND=2) :: OnLineMo,OnLineYr,OffLineMo,OffLineYr,iZero
      CHARACTER (LEN=50) :: Basinto=" ",Plant=" ", &
                            TransportVehicles
      CHARACTER (LEN=256) :: FILE_NAME
      LOGICAL (KIND=4) :: FILE_EXISTS
      INTEGER (KIND=4) :: FILE_NO
      REAL (KIND=4) :: PriceEscalation,QuantityEscalation, &
                       MaxSO2OfBlend, &
                       PercentIncreaseSO2, &
                       PercentDecreaseSO2, &
                       SO2LowerBound, &
                       MaxSO2OfnAnyCoal
!
         READ_GEN_PLANT_DEMAND_LINK_DATA = .FALSE.
         nGenericCoalOptions = 0
         nGenericTransLinks = 0
         IF(.NOT. (GENERIC_LINK_DATA_FILE_EXISTS .AND. &
                                GENERIC_DEMAND_DATA_FILE_EXISTS)) RETURN
         READ_GEN_PLANT_DEMAND_LINK_DATA = .TRUE.
!
! READ COAL TRANSPORTTION LINKS INPUT
         FILE_NO = 2
         IF(PROCESSING_COAL_LINK_FILE_OL(2) == 'OL') THEN
            nGenericTransLinks = ACTIVE_COAL_LINK_IN_OVERLAY(FILE_NO)
         ELSE
            nGenericTransLinks = ACTIVE_COAL_LINK_IN_BASEFILE(FILE_NO)
         ENDIF
         nArray = nGenericTransLinks
! ALLOCATE ARRAYS
         CALL AllocateArray(GenericLinkUsed,0,int(nGenericTransLinks,4))
         GenericLinkUsed = .FALSE.
         CALL AllocateArray(GenericLinkUsedTemp,0, &
                                             int(nGenericTransLinks,4))
         GenericLinkUsedTemp = .FALSE.
         CALL AllocateArray(Generic_Route_Name,nArray)
         CALL AllocateArray(Generic_Coal_Link_ID,nArray)
         CALL AllocateArray(Generic_Basin_HI_ID,nArray)
         CALL AllocateArray(Generic_Plant_EV_ID,nArray)
         CALL AllocateArray(GenericTransportVehicles,nArray)
         CALL AllocateArray( &
                 Generic_Route_Quantity_By_Year,1,INT(nArray,4),0,30)
         CALL AllocateArray( &
                    Generic_Route_Price_By_Year,1,INT(nArray,4),0,30)

!
         IF(.NOT. Allocated(GenericTransportPtrToTG)) THEN
ALLOCATE(GenericTransportPtrToTG(0:256,20)) ! TG,1 = paths TG,>1 pointer
         ENDIF
         GenericTransportPtrToTG = -1
         GenericTransportPtrToTG(0:256,1) = 0
         iArray = 1
         FILE_NAME = TRIM(OUTPUT_DIRECTORY())// &
                       PROCESSING_COAL_LINK_FILE_OL(FILE_NO)// &
                            TRIM(COAL_LINK_BINARY_FILE_NAME(FILE_NO))
         OPEN(10,FILE=FILE_NAME, &
               ACCESS="DIRECT",STATUS="OLD",RECL=COAL_LINK_LRECL)
         IREC = 0
         DO
            IREC = IREC + 1
            IF(IREC > COAL_TRANSPORT_RECORDS(FILE_NO)) EXIT
            READ(10,REC=IREC,IOSTAT=IOS) DELETE, &
                           Basinto , &
                             Plant, &
                           Active, &
                           Generic_Basin_HI_ID(iArray), &
                           GenPlantID, &
                           Generic_Route_Price_By_Year(iArray,0), &
                           PriceEscalation, &
                           Generic_Route_Quantity_By_Year(iArray,0), &
                           GenericTransportVehicles(iArray), &
                           QuantityEscalation
!
            IF(IOS /= 0) EXIT
            IF(Active == "N" .OR. DELETE > 7) CYCLE
            Generic_Plant_EV_ID(iArray) = GenPlantID
            GenericTransportPtrToTG(GenPlantID,1) = 1 + &
                                   GenericTransportPtrToTG(GenPlantID,1)
            PathPos = GenericTransportPtrToTG(GenPlantID,1) + 1
            GenericTransportPtrToTG(GenPlantID,PathPos) = iArray
            Generic_Route_Name(iArray)=TRIM(Basinto)//"-"//TRIM(Plant)
            CALL ROUTE_ESCALATIONS(iArray, &
                                   QuantityEscalation, &
                                   Generic_Route_Quantity_By_Year)
! transportation price
            CALL ROUTE_ESCALATIONS(iArray, &
                                   PriceEscalation, &
                                   Generic_Route_Price_By_Year)
            iArray = iArray + 1
            IF(iArray > nArray) EXIT
         ENDDO

         CLOSE(10)
! END READ COAL TRANSPORTTION LINKS INPUT
!***********************************************************************
! READ PLANT DEMAND INPUT
         IF(GENERIC_DEMAND_DATA_FILE_EXISTS) THEN
            IF(PROCESSING_PLANT_DEMAND_FILE_OL == 'OL') THEN
               nGenericCoalDemand = ACTIVE_GENERICDEMAND_IN_OVERLAY
            ELSE
               nGenericCoalDemand = ACTIVE_GENERIC_DEMAND_IN_BASEFILE
            ENDIF
            nArray = nGenericCoalDemand
! ALLOCATE ARRAYS
            CALL AllocateArray(Generic_Plant_Name,nArray)
            CALL AllocateArray(Generic_Plant_ID,nArray)
            CALL AllocateArray(Generic_Plant_Heat_Lower_Bound,nArray)
            CALL AllocateArray(Generic_Plant_Heat_Upper_Bound,nArray)
            CALL AllocateArray(Generic_Plant_Annual_Demand,nArray)
            CALL AllocateArray(Generic_Plant_Fract_Unscrubbed,nArray)
            CALL AllocateArray(Generic_Plant_Heat_Content,nArray)
            CALL AllocateArray(Generic_Plant_Percent_Increase,nArray)
            CALL AllocateArray(Generic_Plant_Percent_Decrease,nArray)
            CALL AllocateArray( &
                  Generic_Plant_ForecastDemand,0,30,1,INT(nArray,4))
            CALL AllocateArray( &
                          Generic_Plant_CoalDemanCLPlants,nArray)
            CALL AllocateArray(Generic_Plant_TransGroupID,nArray)
            CALL AllocateArray(Generic_Plant_TransGroupName,nArray)
            CALL AllocateArray(Generic_Plant_Capacity,nArray)
            CALL AllocateArray(Generic_Plant_AverageHeatrate,nArray)
            CALL AllocateArray(Generic_Plant_OnLineDate,nArray)
            CALL AllocateArray(Generic_Plant_OffLineDate,nArray)
            CALL AllocateArray(Generic_Plant_StateLocation,nArray)
            CALL AllocateArray( &
                         Generic_MaxSO2InCoalBlend,0,30,1,INT(nArray,4))
            CALL AllocateArray( &
                   Generic_MaxSO2ForAnyCoalInBlend,0,30,1,INT(nArray,4))
            IF(.NOT. ALLOCATED(GenericPlantPtrByTG)) THEN
               ALLOCATE(GenericPlantPtrByTG(0:256))
            ENDIF
            GenericPlantPtrByTG(0:256) = -1

            OPEN(10,FILE=TRIM(OUTPUT_DIRECTORY())// &
                 PROCESSING_GENERIC_DEMAND_FILE_OL// &
                                TRIM(GENERIC_DEMAND_BINARY_FILE_NAME), &
                                  ACCESS="DIRECT",STATUS="OLD", &
                                  RECL=GENERIC_DEMAND_LRECL)
            OnLineYr = 2000
            OnLineMo = 1
            OffLineYr = 2100
            OffLineMo = 12
            IREC = 0
            iArray = 1
            DO
               IREC = IREC + 1
               READ(10,REC=IREC,IOSTAT=IOS) DELETE, &
                             Generic_Plant_Name(iArray), &
                             Active, &
                             TransGrpID, &
                             Generic_Plant_TransGroupName(iArray), &
                             Generic_Plant_StateLocation(iArray), &
                             Generic_Plant_Capacity(iArray), &
                             Generic_Plant_AverageHeatrate(iArray), &
                             Generic_Plant_Heat_Content(iArray), &
                             Generic_Plant_Percent_Increase(iArray), &
                             Generic_Plant_Percent_Decrease(iArray), &
                             Generic_Plant_Heat_Lower_Bound(iArray), &
                             Generic_Plant_Heat_Upper_Bound(iArray), &
                             Generic_Plant_Fract_Unscrubbed(iArray), &
                             MaxSO2OfBlend, &
                             PercentIncreaseSO2, &
                             PercentDecreaseSO2, &
                             SO2LowerBound, &
                             MaxSO2OfnAnyCoal, &
                             Generic_Plant_Annual_Demand(iArray)

               IF(IOS /= 0) EXIT
               IF(Active == "N" .OR. DELETE > 7) CYCLE
               Generic_Plant_TransGroupID(iArray) = TransGrpID
               GenericPlantPtrByTG(TransGrpID) = iArray
               Generic_Plant_ForecastDemand(0,iArray) = &
                                     Generic_Plant_Annual_Demand(iArray)
               Generic_MaxSO2InCoalBlend(0:30,iArray) = &
                                  FILL_ANNUAL_ARRAYS(MaxSO2OfBlend, &
                                                     PercentIncreaseSO2)
               Generic_MaxSO2ForAnyCoalInBlend(0:30,iArray) = &
                               FILL_ANNUAL_ARRAYS(MaxSO2OfnAnyCoal, &
                                                     PercentDecreaseSO2)
               IF(Generic_Plant_Annual_Demand(iArray) >= 0.) THEN
                  Generic_Plant_ForecastDemand(1:30,iArray) = &
                                     Generic_Plant_Annual_Demand(iArray)
               ELSE  ! A VECTOR IS BEING USED
                  VectorNum = ABS(Generic_Plant_Annual_Demand(iArray))
                  IF(RETURN_COAL_VECTOR(Vector_Values,VectorNum)) THEN
                     Generic_Plant_ForecastDemand(1:30,iArray) = &
                                                        Vector_Values(:)
                  ELSE
                  ENDIF
               ENDIF
               iArray = iArray + 1
               IF(iArray > nArray) EXIT
            ENDDO
            CLOSE(10)
         ENDIF
!  END READ PLANT DEMAND INPUT
         RETURN
         END FUNCTION

         SUBROUTINE ROUTE_ESCALATIONS(iArray,EscalRateVector, &
                                      ResultByYr)
         LOGICAL (KIND=4) :: RETURN_COAL_VECTOR,VectorContainsValues
         REAL (KIND=4) :: Vector_Values(1:30)
         REAL (KIND=4) :: ResultByYr(:,0:),EscalRateVector
         INTEGER (KIND=2) :: iArray,Yr
         INTEGER (KIND=4) :: VectorNum
!
            VectorContainsValues = .FALSE.
            IF(EscalRateVector < 0.) THEN
               VectorNum=ABS(EscalRateVector)
               IF(RETURN_COAL_VECTOR(Vector_Values,VectorNum)) THEN
                  ResultByYr(iArray,1:30) = Vector_Values(1:30)
                  VectorContainsValues = .TRUE.
               ENDIF
            ELSE
               Vector_Values(:) = 1.+ EscalRateVector/100.
            ENDIF
            IF(.NOT. VectorContainsValues) THEN
               DO Yr = 1, 30
                  ResultByYr(iArray,Yr) = Vector_Values(Yr) * &
                                              ResultByYr(iArray,Yr-1)
               ENDDO
            ENDIF
         END SUBROUTINE

      SUBROUTINE COAL_MODEL_REPORTS(CurYr,Yr,EndPt,WriteReport)
         USE CoalModelOutputsFromCNW
         USE COAL_MODEL_INPUT_TO_cnw
         USE ArrayAllocationInterface
         USE  conversion_routines
         INTEGER (KIND=2) :: CurYr,EndPt,Yr,nSO2
         INTEGER (KIND=4) :: iDNod,iSNod,BasinPrt,nLink, &
                             RoutID,iLink
         INTEGER (KIND=4), SAVE :: IRecCZ,UnitNoCZ=2700
         INTEGER (KIND=4), SAVE :: IRecSS,UnitNoSS=2702
         INTEGER (KIND=4), SAVE :: IRecFS,UnitNoFS=2704
         CHARACTER (LEN=55) :: NodeNameEVCode
         INTEGER (KIND=4) :: COAL_REPORT_CZ,COAL_REPORT_SS, &
                             COAL_REPORT_FS
         REAL (KIND=4) :: CoalCostByBTU(1:6)
         CHARACTER (LEN=10) :: EV_UNIT_ID_STR
         REAL (KIND=4) :: CoalQuant,HeatQuant,SO2lbs, &
                          DelieveredSO2lbs,DelieveredHeat,MaxSO2InBlend
         CHARACTER (LEN=256) :: FILE_NAME,OUTPUT_DIRECTORY
         CHARACTER (LEN=5) :: GET_SCENAME
         CHARACTER (LEN=1024) :: RECLN
         LOGICAL (KIND=1) :: ReportFilesOpen=.FALSE.
         LOGICAL (KIND=4) :: WriteReport
!         REAL (KIND=4), ALLOCATABLE :: SO2BlendedRate(:)

         IF(.NOT.ReportFilesOpen) THEN
            ReportFilesOpen = .TRUE.
            IRecCZ = COAL_REPORT_CZ(30,UnitNoCZ,"CZ",4)
            IRecSS = COAL_REPORT_SS(20,UnitNoSS,"SS",3)
            IRecFS = COAL_REPORT_FS(20,UnitNoFS,"FS",3)

! CSV
            FILE_NAME = TRIM(OUTPUT_DIRECTORY())//"CoalLPGH-"// &
                                             TRIM(GET_SCENAME())//".CSV"
            OPEN(2701,FILE=FILE_NAME,STATUS='REPLACE')
            RECLN =  "Simulation Year,"// &
                     "Plant Name,"// &
                     "Unit Name,"// &
                     "EV PlantID,"// &
                     "EV Unit ID,"// &
                     "Transaction Group Name,"// &
                     "Transaction Group ID,"// &
                     "State Located in,"// &
                     "Total Capacity ,"// &
                     "Average Heat Rate,"// &
                     "SO2 Control Percent,"// &
                     "SO2 Control Date,"// &
                     "Annual Demand (MMBtu),"// &
                     "Coal Quantity (ktons),"// &
                     "Basin Name,"// &
                     "Basin Abbrev,"// &
                     "Heat Content (Btus/lb),"// &
                     "SO2 Rate (lbs/mmBtus),"// &
                     "Average Supply Cost ($/ton),"// &
                     "Marginal Supply Cost ($/ton),"// &
                     "Average Delivered Cost ($/ton),"// &
                     "Marginal Delivered Cost ($/ton),"// &
                     "Average Transportaion Cost ($/ton),"// &
                     "SO2 Cost ($/ton SO2 emitted),"// &
                     "Supply Type,"
             WRITE(2701,'(1X,A)') TRIM(RECLN)
! transportation report
            FILE_NAME = TRIM(OUTPUT_DIRECTORY())//"CoalLPRoutesGH-"// &
                                             TRIM(GET_SCENAME())//".CSV"
            OPEN(17337,FILE=FILE_NAME,STATUS='REPLACE')
            RECLN = &
                     "Simulation Year,"// &
                     "Plant Name,"// &
                     "Basin Name,"// &
                     "Basin Abbrev,"// &
                     "Heat Content (Btus/lb),"// &
                     "SO2 Rate (lbs/mmBtus),"// &
                     "Route_Name,"// &
                     "Transport_Price ($/ton),"// &
                     "Annual_Quantity_Limit (kiloTons),"// &
                     "Transport_Vehicles,"
             WRITE(17337,'(1X,A)') TRIM(RECLN)
         ENDIF
!
         CALL AllocateArray(UnitCoalCostByBTU,0,6,1,INT(nCoalDemand,4))
         CALL AllocateArray(SO2BlendedRate,INT(nCoalDemand,4))
         SO2BlendedRate = 0.
         UnitCoalCostByBTU = 0.
         AddScrubber = .FALSE.
         DO iDNod=1,nCoalDemand
            DelieveredHeat = 0.
            DelieveredSO2lbs = 0.
            MaxSO2InBlend = 0.
            IF(OptUnScrubbedFrac(iDNod)) THEN
               IF(fUnscNode(1,iDNod) >=0. .AND. &
                       fUnscNode(1,iDNod) < 0.1) THEN
                     ! scrubber installed by LP
                  PlantFractionUnscrubbedByYear(Yr:30,iDNod) = &
                                                      fUnscNode(1,iDNod)
                  OptUnScrubbedFrac(iDNod) = .FALSE.
                  AddScrubber(iDNod) = .TRUE.
               ENDIF
            ELSE
               AddScrubber(iDNod) = &
                        PlantFractionUnscrubbedByYear(Yr,iDNod) /= &
                               PlantFractionUnscrubbedByYear(Yr-1,iDNod)
            ENDIF
            DO iSNod=1,nCoalSupply
               if(.not.(StoD_QyAfMfAdMdTrEmCst(0,iSNod,iDNod)>0.1))cycle
     CoalQuant = 1000.*StoD_QyAfMfAdMdTrEmCst(0,iSNod,iDNod) ! kilotons
               HeatQuant = 2000.*CoalQuant* &
                      Supply_Heat_Content(iSNod)/1000000. ! # OF mmBTUs
               SO2lbs = Supply_SO2_Rate(iSNod) * HeatQuant
               MaxSO2InBlend = MAX(MaxSO2InBlend,Supply_SO2_Rate(iSNod))
               DelieveredHeat = DelieveredHeat + HeatQuant
               DelieveredSO2lbs = DelieveredSO2lbs + SO2lbs
               CoalCostByBTU(1) = CoalQuant * &
                   StoD_QyAfMfAdMdTrEmCst(1,iSNod,iDNod) ! $ Average FOB
               CoalCostByBTU(2) = CoalQuant * &
                  StoD_QyAfMfAdMdTrEmCst(2,iSNod,iDNod) ! $ Margin FOB
               CoalCostByBTU(5) = CoalQuant * &
                  StoD_QyAfMfAdMdTrEmCst(5,iSNod,iDNod) ! $ Transportion
               CoalCostByBTU(6) = CoalQuant * &
                   StoD_QyAfMfAdMdTrEmCst(6,iSNod,iDNod) ! $ Emissisons
                   
               ! Averaged Delivered Cost
               CoalCostByBTU(3) = CoalCostByBTU(1) + CoalCostByBTU(5) 
               
               ! Marginal Delivered Cost
               CoalCostByBTU(4) = CoalCostByBTU(2) + CoalCostByBTU(5) 
               UnitCoalCostByBTU(0,iDNod) = UnitCoalCostByBTU(0,iDNod) &
                                            + HeatQuant
               UnitCoalCostByBTU(1,iDNod) = UnitCoalCostByBTU(1,iDNod) &
                                            + CoalCostByBTU(1)
               UnitCoalCostByBTU(2,iDNod) = UnitCoalCostByBTU(2,iDNod) &
                                            + CoalCostByBTU(2)
               UnitCoalCostByBTU(5,iDNod) = UnitCoalCostByBTU(5,iDNod) &
                                            + CoalCostByBTU(5)
               UnitCoalCostByBTU(6,iDNod) = UnitCoalCostByBTU(6,iDNod) &
                                            + CoalCostByBTU(6)
               UnitCoalCostByBTU(3,iDNod) = UnitCoalCostByBTU(3,iDNod) &
                                            + CoalCostByBTU(3)
               UnitCoalCostByBTU(4,iDNod) = UnitCoalCostByBTU(4,iDNod) &
                                            + CoalCostByBTU(4)
               IF(.NOT. WriteReport) CYCLE
               WRITE(EV_UNIT_ID_STR,*) EV_Unit_ID(iDNod)
               NodeNameEVCode = TRIM(Plant_Name(iDNod))//' '// &
                                                          EV_UNIT_ID_STR
               IF(HeatQuant /= 0.) THEN
                 write(UnitNoCZ,REC=IRecCZ) FLOAT(EndPt),FLOAT(CurYr), &
                        NodeNameEVCode, &
                        SNodName(iSNod)(1:50), &
           StoD_QyAfMfAdMdTrEmCst(0:6,iSNod,iDNod), & ! (kT) and ($/T) 
            CoalCostByBTU(1:6)/1000000., &  ! total costs millions 
                        Supply_Heat_Content(iSNod), &   ! 13 &
                        Supply_SO2_Rate(iSNod), &
                        Supply_CO2_Rate(iSNod), &
                        FLOAT(Supply_Basin_ID(iSNod)), &
                        & ! 17   ! unit's heat demand &
                        SNGL(Plant_Annual_Demand_By_Year(Yr,iDNod)), &  
                        HeatQuant, &    ! heat in mmBTUs &
                        CoalCostByBTU(1:6)/HeatQuant, &
                        SO2lbs/HeatQuant
               ELSE
                 write(UnitNoCZ,REC=IRecCZ) FLOAT(EndPt),FLOAT(CurYr), &
                        NodeNameEVCode, &
                        SNodName(iSNod)(1:50), &
        StoD_QyAfMfAdMdTrEmCst(0:6,iSNod,iDNod), & ! (kT) and ($/T) &
           CoalCostByBTU(1:6)/1000000., &  ! total costs millions &
                        Supply_Heat_Content(iSNod), &
                        Supply_SO2_Rate(iSNod), &
                        Supply_CO2_Rate(iSNod), &
                        FLOAT(Supply_Basin_ID(iSNod)), &
SNGL(Plant_Annual_Demand_By_Year(Yr,iDNod)), &!17 ! unit's heat demand 
                        HeatQuant, & ! heat in mmBTUs 
                        CoalCostByBTU(1:6), &
                        0.
               ENDIF
               IRecCZ = IRecCZ + 1
! CSV Report
               BasinPrt = BasinPointer(Supply_Basin_ID(iSNod))
               RECLN = TRIM(CONVERT_2_STR(CurYr))//','// &
                       TRIM(CONVERT_2_STR(Plant_Name(iDNod)))//','// &
                       TRIM(CONVERT_2_STR( &
                                       Plant_Unit_Name(iDNod)))//','// &
                       TRIM(CONVERT_2_STR(EV_Plant_ID(iDNod)))//','// &
                       TRIM(CONVERT_2_STR(EV_Unit_ID(iDNod)))//','// &
                       TRIM(CONVERT_2_STR( &
                                        TransGroupName(iDNod)))//','// &
                       TRIM(CONVERT_2_STR(TransGroupID(iDNod)))//','// &
                      TRIM(CONVERT_2_STR(StateLocation(iDNod)))//','// &
                       TRIM(CONVERT_2_STR(UnitCapacity(iDNod)))//','// &
                       TRIM(CONVERT_2_STR( &
                                       AverageHeatrate(iDNod)))//','// &
                       TRIM(CONVERT_2_STR( &
                                     SOxControlPercent(iDNod)))//','// &
                     TRIM(CONVERT_2_STR(SOxControlDate(iDNod)))//','// &
                       TRIM(CONVERT_2_STR( &
                        Plant_Annual_Demand_By_Year(Yr,iDNod)))//','// &
                       TRIM(CONVERT_2_STR(CoalQuant/1000.))//','// &
                      TRIM(CONVERT_2_STR(Basin_Name(BasinPrt)))//','// &
                    TRIM(CONVERT_2_STR(Basin_Abbrev(BasinPrt)))//','// &
                       TRIM(CONVERT_2_STR( &
                                   Supply_Heat_Content(iSNod)))//','// &
                       TRIM(CONVERT_2_STR( &
                                     Supply_SO2_Rate(iSNod)))//','// &
!    +                 TRIM(CONVERT_2_STR(HeatQuant))//','// &
                       TRIM(CONVERT_2_STR( &
                        StoD_QyAfMfAdMdTrEmCst(1,iSNod,iDNod)))//','// &
                       TRIM(CONVERT_2_STR( &
                        StoD_QyAfMfAdMdTrEmCst(2,iSNod,iDNod)))//','// &
                       TRIM(CONVERT_2_STR( &
                        StoD_QyAfMfAdMdTrEmCst(3,iSNod,iDNod)))//','// &
                       TRIM(CONVERT_2_STR( &
                        StoD_QyAfMfAdMdTrEmCst(4,iSNod,iDNod)))//','// &
                       TRIM(CONVERT_2_STR( &
                        StoD_QyAfMfAdMdTrEmCst(5,iSNod,iDNod)))//','// &
                       TRIM(CONVERT_2_STR( &
                        StoD_QyAfMfAdMdTrEmCst(6,iSNod,iDNod)))//','// &
                       TRIM(CONVERT_2_STR( &
                                        Supply_Description(iSNod)))//','
                  WRITE(2701,'(1X,A)') TRIM(RECLN)
            ENDDO
            IF(DelieveredHeat > 0.) THEN
               SO2BlendedRate(iDNod) = DelieveredSO2lbs/DelieveredHeat
            ENDIF
         ENDDO
         IF(.NOT. WriteReport) RETURN
         IF(ALLOCATED(GenericLinkUsed)) &
                                   GenericLinkUsed = GenericLinkUsedTemp
         nCoalDemandExistingUnits = nCoalDemand
         nTransportLinksExistingUnits = nTransportLinks
         nExistingPlant = nExistingPlantPlusAdds


! SO2 Report
         DO nSO2 = 0, nSO2Limits
            write(UnitNoSS,REC=IRecSS) FLOAT(EndPt),FLOAT(CurYr), &
                                      SO2MarketCodes(nSO2), &
                                      SO2EmissionLimits(nSO2), &
                                      SO2CreditPrice(nSO2), &
                                      SO2ResultsEmissions(0,nSO2), &
                                    SO2ResultsEmissions(1,nSO2)/1000., &
                                    SO2ResultsEmissions(2,nSO2)/1000., &
                                      StateGroupAssignment(nSO2)
            IRecSS = IRecSS + 1
         ENDDO
         DO iDNod=1,nCoalDemand
            IF(UnitCoalCostByBTU(0,iDNod) > 0.0) THEN
               HeatQuant =  UnitCoalCostByBTU(0,iDNod)
               WRITE(EV_UNIT_ID_STR,*) EV_Unit_ID(iDNod)
               NodeNameEVCode = TRIM(Plant_Name(iDNod))//' '// &
                                                          EV_UNIT_ID_STR
               write(UnitNoFS,REC=IRecFS) FLOAT(EndPt),FLOAT(CurYr), &
                          NodeNameEVCode, &
                        100*PlantFractionUnscrubbedByYear(Yr-1,iDNod), &
                          100*fUnscNode(1,iDNod), &! 1 &
                          HeatQuant, &             ! 2 
 UnitCoalCostByBTU(1:6,iDNod)/HeatQuant, & ! 3-8   total costs millions
           UnitCoalCostByBTU(1:6,iDNod)/1000000., &   ! 9-14  $/mmBTU &
                          SO2BlendedRate(iDNod)   ! 15 lbs/mmBTU
               IRecFS = IRecFS + 1
            ENDIF
         ENDDO
! transportation route results
         nLink = MaxIndexSDRnOfLk
         DO iLink=1,nLink
            IF(SDRnOfLk(2,iLink) == 0) CYCLE
            iDNod = SDRnOfLk(1,iLink)
            iSNod = SDRnOfLk(0,iLink)
            if(.not.(StoD_QyAfMfAdMdTrEmCst(0,iSNod,iDNod)>0.1))cycle
            RoutID = SDRnOfLk(2,iLink)
            BasinPrt = BasinPointer(Supply_Basin_ID(iSNod))
            RECLN = &

               TRIM(CONVERT_2_STR(CurYr))//','// &
               TRIM(CONVERT_2_STR(Plant_Name(iDNod)))//','// &
               TRIM(CONVERT_2_STR(Basin_Name(BasinPrt)))//','// &
               TRIM(CONVERT_2_STR(Basin_Abbrev(BasinPrt)))//','// &
               TRIM(CONVERT_2_STR( &
                             Supply_Heat_Content(iSNod)))//','// &
               TRIM(CONVERT_2_STR( &
                             Supply_SO2_Rate(iSNod)))//','
               IF(RoutID <= nTransportLinks) THEN
                  RECLN = TRIM(RECLN)// &
                     TRIM(CONVERT_2_STR(Route_Name(RoutID) ))//','// &
                   TRIM(CONVERT_2_STR(Transport_Price(RoutID)))//','// &
                     TRIM(CONVERT_2_STR( &
                               Annual_Quantity_Limit(RoutID)))//','// &
                     TRIM(CONVERT_2_STR(Transport_Vehicles(RoutID)))
               ELSE
                 RECLN=TRIM(RECLN)//"RoutID exceeds number of routes " &
                             //TRIM(CONVERT_2_STR(nTransportLinks))//','
               ENDIF
            write(17337,'(1X,A)') TRIM(RECLN)
         ENDDO

      END SUBROUTINE
!***********************************************************************
      INTEGER (KIND=2) FUNCTION SO2_MARKETS_YR_BASED_VALUES(Yr)
      use mod_base_year
      USE COAL_MODEL_INPUT_TO_cnw
      USE SO2_Scrubber_COAL_MODEL_DATA
      USE ArrayAllocationInterface
      USE  conversion_routines
      USE CoalModelOutputsFromCNW
      INTEGER (KIND=2) :: Yr,nS,iS,iUnit,iPlnt,iRoute,nPlant
      REAL (KIND=4) :: PlantKiloTonDemand,RoutemmBTUDelivered,RouteUtils
      CHARACTER (LEN=256) :: RECLN
      LOGICAL (KIND=1), SAVE :: HeaderWriten=.FALSE.
      CHARACTER (LEN=256) :: FILE_NAME,OUTPUT_DIRECTORY
      CHARACTER (LEN=5) :: GET_SCENAME
      INTEGER (KIND=2) :: iArray,ActivePlants
      INTEGER (KIND=2) :: RETURN_UNIT_ADDITIONS
!
         nS = -1
         DO iS = 0, nStateSO2Limits
            IF(.NOT. StateHasLimits(iS) .OR. &
                               SulphurEmissionsLimit(Yr,iS) == 0.) CYCLE
            nS = nS + 1
            SO2MarketCodes(nS) = SulphurMarketArea(iS)
            SO2EmissionLimits(nS) = SulphurEmissionsLimit(Yr,iS)
            SO2CreditPrice(nS) = SulphurCreditCost(Yr,iS)
            StateCapVolatility (nS) = StateCapVolatilityByYear(Yr,iS)
            StateGroupAssignment(nS) = StateAssignedToGroup(iS)
         ENDDO
         SO2_MARKETS_YR_BASED_VALUES = nS
!
         DO iArray = 1, nBasin
            Basin_QtyLB(iArray)= Basin_QtyLBByYr(Yr,iArray)
            Basin_QtyUB(iArray)= Basin_QtyUBByYr(Yr,iArray)
         ENDDO
!
! pass current SO2 costs to arrays used by LP
!
         DO iArray = 1, nCoalDemand  ! size(SOxControlCapitalCost)
            SOxControlCapitalCost(iArray) = &
                                         SOxCapitalCostByYear(Yr,iArray)
            SOxControlCarryingCharge(iArray)= &
                                      SOxCarryingChargeByYear(Yr,iArray)
            SOxControlVarCost(iArray) = SOxVarCostByYear(Yr,iArray)
            SOxControlFixedCost(iArray) = SOxFixedCostByYear(Yr,iArray)
         ENDDO
!
         Transport_Price(1:nTransportLinks) = &
                               Route_Price_By_Year(1:nTransportLinks,Yr)
         Annual_Quantity_Limit(1:nTransportLinks) = &
                      Route_Quantity_Limit_By_Year(1:nTransportLinks,Yr)
! thermal demand by plant
         nPlant = MAX(nExistingPlant,nExistingPlantPlusAdds)
         CALL AllocateArray(PlantThermalDemand,nPlant)
         CALL AllocateArray(PlantTotalCapacity,nPlant)
         CALL AllocateArray(PlantTotalEnergy,nPlant)
         CALL AllocateArray(PlantAverageHeatRate,nPlant)
         PlantThermalDemand = 0
         PlantTotalCapacity = 0
         PlantTotalEnergy = 0.
         PlantAverageHeatRate = 0.
         ActivePlants = 0
         DO iUnit = 1, nCoalDemand
            SO2MaxInBlend(iUnit) = MaxSO2InCoalBlend(Yr,iUnit)
            SO2MaxInAnyCoal(iUnit)= MaxSO2ForAnyCoalInBlend(Yr,iUnit)
            Plant_Fraction_Unscrubbed(iUnit) = &
                                 PlantFractionUnscrubbedByYear(Yr,iUnit)
            iPlnt = UnitToPlantPtr(iUnit)
            if(iPlnt == 0) cycle
            ActivePlants = ActivePlants + 1
            PlantThermalDemand(iPlnt) = PlantThermalDemand(iPlnt) &
                                 + Plant_Annual_Demand_By_Year(Yr,iUnit)
            PlantTotalCapacity(iPlnt) = PlantTotalCapacity(iPlnt) &
                                        + UnitCapacity(iUnit)
            PlantTotalEnergy(iPlnt) = PlantTotalEnergy(iPlnt) &
                                        + UnitIMEnergy(iUnit)
            IF(PlantTotalEnergy(iPlnt) > 0.) THEN
               PlantAverageHeatRate(iPlnt) = &
                                      1000.*PlantThermalDemand(iPlnt)/ &
                                              PlantTotalEnergy(iPlnt)
            ENDIF
         ENDDO
! plant delivery limits
         CALL AllocateArray(RouteLimitToPlant,nPlant)
         RouteLimitToPlant = 0
         DO iRoute = 1, nTransportLinks
            DO iPlnt = 1, nPlant
               IF(Plant_ID(iPlnt) == Plant_EV_ID(iRoute)) THEN
                  RouteLimitToPlant(iPlnt) = RouteLimitToPlant(iPlnt) &
                                         + Annual_Quantity_Limit(iRoute)
                  EXIT
               ENDIF
            ENDDO
         ENDDO
! assume 10000btu/lb of coal
         IF(.NOT. HeaderWriten) THEN
            FILE_NAME = TRIM(OUTPUT_DIRECTORY())//"CoalRouteLimits-"// &
                                             TRIM(GET_SCENAME())//".CSV"
            OPEN(17373,FILE=FILE_NAME,STATUS='REPLACE')
            RECLN =  "Year,"// &
                     "Plant Name,"// &
                     "Plant Capacity,"// &
                     "Plant Thermal mmBTU,"// &
                     "Route kilotons in mmBTU,"// &
                     "Route kiloTon Delivery,"// &
                     "Plant mmBUTs in kilotons,"// &
                     "Route Limit/Plant Demand,"
            HeaderWriten = .TRUE.
            WRITE(17373,'(1X,A)') TRIM(RECLN)
         ENDIF

         DO iPlnt = 1, nPlant
            PlantKiloTonDemand = PlantThermalDemand(iPlnt)/20000.
            RoutemmBTUDelivered = 20000.*RouteLimitToPlant(iPlnt)
            RouteUtils =  0.
            IF(PlantThermalDemand(iPlnt) > 0.) &
              RouteUtils = RoutemmBTUDelivered/PlantThermalDemand(iPlnt)

            RECLN = TRIM(CONVERT_2_STR(Yr+get_BASE_YEAR()))//','// &
                  TRIM(CONVERT_2_STR(PlantListName(iPlnt)))//','// &
                 TRIM(CONVERT_2_STR(PlantTotalCapacity(iPlnt)))//','// &
                 TRIM(CONVERT_2_STR(PlantThermalDemand(iPlnt)))//','// &
                  TRIM(CONVERT_2_STR(RoutemmBTUDelivered))//','// &
                  TRIM(CONVERT_2_STR(RouteLimitToPlant(iPlnt)))//','// &
                  TRIM(CONVERT_2_STR(PlantKiloTonDemand))//','// &
                  TRIM(CONVERT_2_STR(RouteUtils,2))//','
            WRITE(17373,'(1X,A)') TRIM(RECLN)
         ENDDO
      END FUNCTION

      INTEGER FUNCTION COAL_REPORT_CZ(NumVars,UnitNum,FileCode,Dims)

         INTEGER (KIND=4) :: UnitNum,NumVars,Dims
         INTEGER (KIND=4) :: COAL_REPORT_HEADER,iRec
         CHARACTER (LEN=*) :: FileCode
         iRec = COAL_REPORT_HEADER(NumVars,UnitNum,FileCode,Dims)
         WRITE(UnitNum,REC=iRec) &
                            'Plant Name Unit EVID','C',INT(55,2),'F','D'
         WRITE(UnitNum,REC=iRec+1) &
                            'Coal Basin, Heat, SO','C',INT(50,2),'F','D'

         COAL_REPORT_CZ = iRec + 2
      END FUNCTION

      INTEGER FUNCTION COAL_REPORT_FS(NumVars,UnitNum,FileCode,Dims)

         INTEGER (KIND=4) :: UnitNum,NumVars,Dims
         INTEGER (KIND=4) :: COAL_REPORT_HEADER,iRec
         CHARACTER (LEN=*) :: FileCode
         iRec = COAL_REPORT_HEADER(NumVars,UnitNum,FileCode,Dims)
         WRITE(UnitNum,REC=iRec) &
                            'Plant Name Unit EVID','C',INT(55,2),'F','D'
         COAL_REPORT_FS = iRec + 1
      END FUNCTION

      INTEGER FUNCTION COAL_REPORT_SS(NumVars,UnitNum,FileCode,Dims)

         INTEGER (KIND=4) :: UnitNum,NumVars,Dims
         INTEGER (KIND=4) :: COAL_REPORT_HEADER,iRec
         CHARACTER (LEN=*) :: FileCode
         iRec = COAL_REPORT_HEADER(NumVars,UnitNum,FileCode,Dims)
         WRITE(UnitNum,REC=iRec) &
                             'State Code          ','C',INT(2,2),'F','D'

         COAL_REPORT_SS = iRec + 1
      END FUNCTION

      FUNCTION COAL_REPORT_HEADER(NumVars,UnitNum,FileCode,Dims)
         use mod_base_year
         implicit none

         INTEGER (KIND=4) :: COAL_REPORT_HEADER
         INTEGER (KIND=4) :: UnitNum,NumVars,Dims
         CHARACTER (LEN=256) :: FILE_NAME,OUTPUT_DIRECTORY
         CHARACTER (LEN=5) :: GET_SCENAME
         INTEGER (KIND=2) :: DIMENSIONS,RECORD_LENGTH,ENDYR_loc
         INTEGER (KIND=2) :: OVERHEAD_LENGTH, &
                             GET_NUM_OF_END_POINTS
         CHARACTER(LEN=40) :: TITLE
         CHARACTER(LEN=28) :: COMPANY_NAME
         INTEGER (KIND=1) :: F7=Z'F7'
         CHARACTER (LEN=*) :: FileCode
!
!
         FILE_NAME = TRIM(OUTPUT_DIRECTORY())//"MSG"// &
                           TRIM(GET_SCENAME())//"."//TRIM(FileCode)//"D"
         OVERHEAD_LENGTH = 128
         DIMENSIONS = Dims
         RECORD_LENGTH = MAX(256,OVERHEAD_LENGTH + 4*NumVars)
         OPEN(UnitNum,FILE=FILE_NAME,ACCESS="DIRECT", &
                                    STATUS="REPLACE",RECL=RECORD_LENGTH)
!
         WRITE(UnitNum,REC=1) F7,RECORD_LENGTH, &
                               DIMENSIONS, &
                               get_BASE_YEAR(), &
                               ENDYR_loc, &
                               GET_NUM_OF_END_POINTS()
!
         WRITE(UnitNum,REC=2) COMPANY_NAME()
         WRITE(UnitNum,REC=3) TITLE()
         WRITE(UnitNum,REC=4) 'Endpoint            ','N', &
                                                        INT(4,2),'F','D'
         WRITE(UnitNum,REC=5) 'Year                ','N', &
                                                        INT(4,2),'F','D'
         COAL_REPORT_HEADER = 6
!
         RETURN
      END FUNCTION

