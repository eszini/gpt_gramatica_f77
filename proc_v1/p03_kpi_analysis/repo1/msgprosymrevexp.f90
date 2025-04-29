    
!     MSGProSymRevExp.for
!     Copyright(c) M.S. Gerber & Associates 2004
!
!     Created: 1/13/2003 4:07:27 PM
!     Author : MARK S GERBER
!     Last change: MSG 10/4/2004 3:45:01 PM


      module ProSymModule
         TYPE DES_FILE_CONTENT
            SEQUENCE
            CHARACTER (LEN=50) :: TITLE
            CHARACTER (LEN=20) :: DEFAULT_VALUE            
            CHARACTER (LEN=5)  :: TYPE            
            INTEGER (KIND=2)   :: LENGTH 
            INTEGER (KIND=2)   :: POS
            CHARACTER (LEN=256) :: LIST

         END TYPE DES_FILE_CONTENT  
         TYPE RESOURCE_LIST
            SEQUENCE
            CHARACTER (LEN=50) :: RESOURCE_NAME
            CHARACTER (LEN=6)  :: ASSET_CLASS_ID
            CHARACTER (LEN=1)  :: ACTIVE            
         END TYPE RESOURCE_LIST  

      end module ProSymModule

    
      RECURSIVE SUBROUTINE ProSymInputData(Process_Base_File, &
        OverlayName)
      use end_routine, only: end_program, er_message
      use filename_tracker
      use sizecom
      USE ProSymModule
      use mthnmcom
	  use mod_base_year
      implicit none
      INTEGER(kind=2), PARAMETER :: ProSymDataItems=20, &
                              MCP_Revenue=1, &    ! revenues 1-10
                              Revenue_Pool_Energy=2, &
                              Revenue_Ancillary_Service=3, &
                              Revenue_Availability=4, &
                              Revenue_Capacity=5, &
                              Revenue_Pumping_Cost=6, &
                              Revenue_Energy=7, &
!
                              Total_Fuel_Cost=11, &
                              Total_Start_Cost=12, &
                              Total_Variable_OM_Cost=13, &
                              Total_Fixed_OM_Cost=14, &
                              NOx_Emissions_Cost=15, & ! expenses 11-20
                              SO2_Emissions_Cost=16 
      INTEGER (KIND=2) :: ProSymVarPos(ProSymDataItems)
      CHARACTER (LEN=30), PARAMETER, DIMENSION(ProSymDataItems) :: &
                   ProSymTitles = (/"                              ", & ! 1
       "REVENUEFROMPOOLENERGY         ", &          ! 2
       "REVENUEFROMANCILLARYSERVICE   ", &          ! 3
       "REVENUEFROMAVAILABILITY       ", &          ! 4
       "REVENUEFROMCAPACITY           ", &          ! 5
       "REVENUEFROMPUMPINGCOST        ", &          ! 6 
       "                              ", & 
       "                              ", &
       "                              ", &
       "                              ", &
       "TOTALFUELCOST                 ", &          ! 11
       "TOTALSTARTCOST                ", &          ! 12
       "TOTALVARIABLEO                ", &          ! 13
       "TOTALFIXEDO                   ", &          ! 14
       "                              ", &
       "                              ", &
       "                              ", &
       "                              ", &
       "                              ", &
       "                              "/)
      INTEGER (KIND=2), PARAMETER :: DataItemsInProSymFile=29

      REAL (KIND=4), SAVE, ALLOCATABLE :: ProSymRevAndExps(:,:,:,:)
      REAL (KIND=4), SAVE, ALLOCATABLE :: VALUES(:,:)
      CHARACTER (LEN=*), OPTIONAL :: OverlayName
      LOGICAL (KIND=4) :: Process_Base_File,FILE_EXISTS
      CHARACTER (LEN=256) :: FILE_NAME,ProSym_Interface_File, &
                             BASE_FILE_DIRECTORY,DES_FILE_NAME, &
                             GET_RESULTS_DIRECTORY, &
                             SOFTWARE_DIRECTORY,RETURN_BASE_FILE_NAME
      INTEGER (KIND=2), SAVE :: NUM_OF_ASSET_CLASSES,MAX_ASSET_CLASS_NUM
      INTEGER (KIND=2), SAVE :: BaseStartYr,BaseEndYr,StartYr,EndYr, &
                                YearsInFile,DataVariables, &
                                BaseDataVariables, &
                                CLASS_ID,MO
      INTEGER (KIND=2), SAVE, ALLOCATABLE :: ASSET_CLASS_POINTER(:)
      CHARACTER (LEN=50) :: SCREEN_OUTPUT
      CHARACTER (LEN=1024) :: RECLN
      INTEGER (KIND=4) :: IOS
      INTEGER (KIND=2) :: pos,DELETE,TAB_POSITIONS_READ,VARS, &
                          YR,I,ASSET_CLASS,R_YEAR,R_ASSET_CLASS,R_CLASS, &
                          ResourcesInFile
      CHARACTER (LEN=30) :: STATION_NAME
      REAL (KIND=4) :: R_FUEL_EXPENSE, &
                       R_FIXED_OM, &
                       R_VARIABLE_OM, &
                       R_EMISSION_EXPENSES, &
                       R_PROSYM_TOTAL_AMOUNT, &
                       R_SECONDARY_SALES_REVENUES, &
                       R_CAPACITY_REVENUES, &
                       R_OTHER_REVENUES, &
                       R_RELATIONSHIP_REVENUES 
      REAL (KIND=4) ::  INC_MONTH_VARS(0:12,1:*), &
                        CASH_MONTH_VARS(0:12,1:*)
      CHARACTER (LEN=256) :: DES_FileName
      TYPE (DES_FILE_CONTENT) :: TEMP_DES_INFO
      CHARACTER (LEN=30),ALLOCATABLE,SAVE,DIMENSION(:) :: ResourceList
      INTEGER (KIND=2),ALLOCATABLE,SAVE,DIMENSION(:) :: ResourceListIDs
      TYPE (RESOURCE_LIST) :: TEMP_RESOURCE_LIST
      TYPE (RESOURCE_LIST), ALLOCATABLE, SAVE :: RESOURCE_LIST_W_IDs(:)
!
!
      IF(ALLOCATED(ProSymRevAndExps)) DEALLOCATE(ProSymRevAndExps)
!
      IF(Process_Base_File) THEN
		
         FILE_NAME = get_psb_filename()
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         IF(.NOT. FILE_EXISTS) RETURN
!
! SHOW FILE NAME ON SCREEN
!
         SCREEN_OUTPUT = 'ProSym File'//'-'//ProSym_Interface_File()
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
! PROCESS BASE FILE
         OPEN(10,FILE=FILE_NAME)
         READ(10,'(A)',IOSTAT=IOS) RECLN ! READ HEADER RECORD
         CLOSE(10)
         pos = INDEX(RECLN,'EndYear=') + 8
         READ(RECLN(pos:),'(I4)') EndYr
         pos = INDEX(RECLN,'BegYear=') + 8
         READ(RECLN(pos:),'(I4)') StartYr
         YearsInFile = EndYr - StartYr + 1
!                                    provides for mismatch of run years
         StartYr = MIN(StartYr,get_base_year()+int(1,2))  
         BaseStartYr = StartYr
         BaseEndYr = EndYr
         pos = INDEX(RECLN,'Variables=') + 10
         READ(RECLN(pos:),'(I4)') DataVariables
         BaseDataVariables = DataVariables
         pos = INDEX(RECLN,'DES=') + 4
         READ(RECLN(pos:),'(A)') DES_FileName
         CALL ProSymVariableMapToMIDAS()
      ELSE  ! CALL FROM OVERLAY FILE FIND
         FILE_NAME = get_pso_filename(OverlayName)
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         IF(.NOT. FILE_EXISTS) THEN
            SCREEN_OUTPUT = 'ProSym File'//'-'//ProSym_Interface_File()
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
            CALL RETURN_INITIALIZATION_CLASSES(NUM_OF_ASSET_CLASSES, &
                                               MAX_ASSET_CLASS_NUM)
            IF(ALLOCATED(ProSymRevAndExps)) DEALLOCATE(ProSymRevAndExps)
            ALLOCATE(ProSymRevAndExps(0:12,BaseStartYr:BaseEndYr, &
                                      -1:NUM_OF_ASSET_CLASSES, &
                                      ProSymDataItems))
            CALL PROSYM_READ_BASE_FILE()
            RETURN
         ENDIF
! PROCESS OVERLAY FILE
         SCREEN_OUTPUT = 'ProSym File'//'-'//OverlayName
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         OPEN(10,FILE=FILE_NAME)
         READ(10,'(A)',IOSTAT=IOS) RECLN ! READ HEADER RECORD
         CLOSE(10)
         pos = INDEX(RECLN,'EndYear=') + 8
         READ(RECLN(pos:),'(I4)') EndYr
         pos = INDEX(RECLN,'BegYear=') + 8
         READ(RECLN(pos:),'(I4)') StartYr
         YearsInFile = EndYr - StartYr + 1
         StartYr = MIN(StartYr,get_base_year()+int(1,2)) 
         pos = INDEX(RECLN,'Variables=') + 10
         READ(RECLN(pos:),'(I4)') DataVariables
      ENDIF
!
!
!
      CALL RETURN_INITIALIZATION_CLASSES(NUM_OF_ASSET_CLASSES, &
		MAX_ASSET_CLASS_NUM)
      IF(MAX_ASSET_CLASS_NUM <= 0) RETURN
      IF(ALLOCATED(ASSET_CLASS_POINTER)) DEALLOCATE(ASSET_CLASS_POINTER)
      ALLOCATE(ASSET_CLASS_POINTER(MAX_ASSET_CLASS_NUM))
      CALL RETURN_INITIALIZATION_POINTER(ASSET_CLASS_POINTER)
      ALLOCATE(ProSymRevAndExps(0:12,StartYr:EndYr, &
                                -1:NUM_OF_ASSET_CLASSES, &
                                ProSymDataItems))
      ProSymRevAndExps = 0.
!
! PROCESS
!
      ALLOCATE(VALUES(DataVariables,StartYr:EndYr))
      VALUES = 0.
      TAB_POSITIONS_READ = 0
      OPEN(10,FILE=FILE_NAME)
      READ(10,'(A)',IOSTAT=IOS) RECLN ! READ HEADER RECORD
      DO
         VALUES = 0.
         TAB_POSITIONS_READ = TAB_POSITIONS_READ + 1
         DO
            READ(10,'(A)',IOSTAT=IOS) RECLN
            IF(IOS /=0) EXIT
            IF(RECLN(1:1) == '7') EXIT
            READ(RECLN,*,IOSTAT=IOS) DELETE,STATION_NAME,CLASS_ID,MO, &
                                       (YR,VALUES(:,YR),I=1,YearsInFile)
            IF(IOS /=0) THEN
               SCREEN_OUTPUT='ERROR Reading ProSym File'//'-'//FILE_NAME
               CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,1)
               SCREEN_OUTPUT='at the tab after '//RECLN(3:)
               CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,1)
               er_message='Stop requested from MSGProSymRevExp SIID248'
               call end_program(er_message)
            ENDIF
            IF(DELETE > 7) CYCLE
            CALL ProSymDataToAssetClasses() ! CLASS_ID,MO,VALUES)
         ENDDO
         IF(IOS /=0) EXIT
      ENDDO
      CLOSE(10)
      ProSymRevAndExps = ProSymRevAndExps/1000000.
      DEALLOCATE(VALUES)
      IF(Process_Base_File) CALL PROSYM_SAVE_BASE_FILE()
      RETURN
    
      ENTRY PROSYM_SAVE_BASE_FILE()
    
         FILE_NAME = trim(BASE_FILE_DIRECTORY())//'GEDProSymProductionData.BIN'
         OPEN(10,FILE=FILE_NAME,FORM='UNFORMATTED',STATUS='REPLACE')
         WRITE(10) ProSymRevAndExps
         CLOSE(10)
      RETURN
    
      ENTRY PROSYM_READ_BASE_FILE()
    
         FILE_NAME = trim(BASE_FILE_DIRECTORY())//'GEDProSymProductionData.BIN'
         OPEN(10,FILE=FILE_NAME,FORM='UNFORMATTED',STATUS='OLD')
         READ(10) ProSymRevAndExps
         CLOSE(10)
      RETURN
    
      ENTRY ProSymVariableMapToMIDAS()
    
         DES_FILE_NAME = TRIM(GET_RESULTS_DIRECTORY())//DES_FileName
         INQUIRE(FILE=DES_FILE_NAME,EXIST=FILE_EXISTS)
         IF(.NOT. FILE_EXISTS) THEN
            DES_FILE_NAME = TRIM(SOFTWARE_DIRECTORY())//'DES\'//DES_FileName
            IF(.NOT. FILE_EXISTS) THEN
               DES_FILE_NAME = TRIM(SOFTWARE_DIRECTORY())//DES_FileName
            ENDIF
         ENDIF
         IF(.NOT. FILE_EXISTS) RETURN
         ProSymVarPos = -999
         OPEN(10,FILE=DES_FILE_NAME)
         READ(10,*) ! FIRST RECORD IS NOT NEEDED
         DO
            READ(10,'(A)',IOSTAT=IOS) RECLN
            IF(IOS /= 0) THEN
               EXIT
            ENDIF
            RECLN = TRIM(RECLN)//',,,,,,'               
!
            READ(RECLN,*,IOSTAT=IOS) TEMP_DES_INFO 
            IF(IOS /= 0) THEN
               EXIT
            ENDIF
        IF(TEMP_DES_INFO%TYPE(1:1) == 'T' .OR. &
			TEMP_DES_INFO%TYPE(1:1) == 'B') then
            CYCLE
        end if
            IF(ABS(TEMP_DES_INFO%POS)-4 > DataVariables) EXIT
            DO VARS = 1, ProSymDataItems
               IF(INDEX(TEMP_DES_INFO%TITLE,ProSymTitles(VARS))/=0) THEN
                  I = ABS(TEMP_DES_INFO%POS)-4
                  ProSymVarPos(VARS) = I
               ENDIF
            ENDDO
         ENDDO
         CLOSE(10)
      RETURN
    
      ENTRY ProSymDataToAssetClasses() ! CLASS_ID,MO,VALUES)
    
         IF(CLASS_ID+1 >= 0) ASSET_CLASS=ASSET_CLASS_POINTER(CLASS_ID+1)
         DO Yr = StartYr, EndYr
!
! Revenue Section
!
            DO VARS = 1, 10
               IF(ProSymVarPos(VARS) <= 0) CYCLE
               ProSymRevAndExps(MO,Yr,ASSET_CLASS,VARS) = &
                                ProSymRevAndExps(MO,Yr,ASSET_CLASS,VARS) &
                                + VALUES(ProSymVarPos(VARS),Yr)
            ENDDO 
!
! Expense Section
!
            DO VARS = 11, 20
               IF(ProSymVarPos(VARS) <= 0) CYCLE
               ProSymRevAndExps(MO,Yr,ASSET_CLASS,VARS) = &
                                ProSymRevAndExps(MO,Yr,ASSET_CLASS,VARS) &
                                + VALUES(ProSymVarPos(VARS),Yr)
            ENDDO 
         ENDDO
      RETURN
    
      ENTRY FindProSymDataAssetClasses() ! CLASS_ID,MO,VALUES)
    
         OPEN(10,file=get_rlb_filename(RETURN_BASE_FILE_NAME(103))//'.DAT')
         READ(10,'(A)') RECLN
         pos = INDEX(RECLN,'Resources=') + 10
         READ(RECLN(pos:),*) ResourcesInFile
         IF(ALLOCATED(ResourceList)) DEALLOCATE(ResourceList)
         ALLOCATE(ResourceList(ResourcesInFile))     
         IF(ALLOCATED(ResourceListIDs)) DEALLOCATE(ResourceListIDs)
         ALLOCATE(ResourceListIDs(ResourcesInFile))
         I = 0     
         DO
            READ(10,'(A)',IOSTAT=IOS) RECLN
            IF(IOS /= 0) EXIT
            IF(RECLN(1:1) == '7') CYCLE
            READ(RECLN,*,IOSTAT=IOS) TEMP_RESOURCE_LIST
            IF(IOS /= 0) THEN
               FILE_NAME = 'ERROR-Reading resource list in '//'RLB'// &
				TRIM(RETURN_BASE_FILE_NAME(103))//'.DAT'               
               er_message='ERROR-Reading resource list.'
               call end_program(er_message)
            ENDIF
            I = I + 1
            RESOURCE_LIST_W_IDs(I) = TEMP_RESOURCE_LIST 
         ENDDO
      RETURN
    
      ENTRY RETURN_ProSymAnnualExpenses(R_YEAR,R_ASSET_CLASS, &
                                        R_PROSYM_TOTAL_AMOUNT, &
                                        R_FUEL_EXPENSE, &
                                        R_FIXED_OM, &
                                        R_VARIABLE_OM, &
                                        R_EMISSION_EXPENSES)
    
!
         Yr = get_base_year() + R_YEAR
         IF(.NOT. ALLOCATED(ProSymRevAndExps) .OR. Yr > EndYr) RETURN
         ASSET_CLASS = R_ASSET_CLASS
         ProSymRevAndExps(0,Yr,ASSET_CLASS,11:20) = &
			SUM(ProSymRevAndExps(1:,Yr,ASSET_CLASS,11:20),dim=1)
         R_PROSYM_TOTAL_AMOUNT = R_PROSYM_TOTAL_AMOUNT + &
			SUM(ProSymRevAndExps(0,Yr,ASSET_CLASS,11:20))
!
! ADD EXPENSES
!
         R_FUEL_EXPENSE = R_FUEL_EXPENSE &
               + ProSymRevAndExps(0,Yr,ASSET_CLASS,Total_Fuel_Cost) &
               + ProSymRevAndExps(0,Yr,ASSET_CLASS,Total_Start_Cost)
         R_FIXED_OM = R_FIXED_OM + ProSymRevAndExps(0, &
			Yr,ASSET_CLASS,Total_Fixed_OM_Cost)
         R_VARIABLE_OM = R_VARIABLE_OM + ProSymRevAndExps(0, &
			Yr,ASSET_CLASS,Total_Variable_OM_Cost)
         R_EMISSION_EXPENSES = R_EMISSION_EXPENSES &
               + ProSymRevAndExps(0,Yr,ASSET_CLASS,NOx_Emissions_Cost) &
                 + ProSymRevAndExps(0,Yr,ASSET_CLASS,SO2_Emissions_Cost)
      RETURN
    
      ENTRY RETURN_ProSymAnnualRevenues(R_YEAR,R_ASSET_CLASS, &
                                        R_PROSYM_TOTAL_AMOUNT, &
                                        R_SECONDARY_SALES_REVENUES, &
                                        R_CAPACITY_REVENUES, &
                                        R_OTHER_REVENUES, &
                                        R_RELATIONSHIP_REVENUES)
    
!
         Yr = get_base_year() + R_YEAR
         IF(.NOT. ALLOCATED(ProSymRevAndExps) .OR. Yr > EndYr) then
			RETURN
		end if
!
         ASSET_CLASS = R_ASSET_CLASS
         ProSymRevAndExps(0,Yr,ASSET_CLASS,1:10) = &
			SUM(ProSymRevAndExps(1:,Yr,ASSET_CLASS,1:10),dim=1)
         R_PROSYM_TOTAL_AMOUNT = R_PROSYM_TOTAL_AMOUNT + &
			SUM(ProSymRevAndExps(0,Yr,ASSET_CLASS,1:10))
!
! ADD REVENUES
!
         R_SECONDARY_SALES_REVENUES = R_SECONDARY_SALES_REVENUES &
                   + ProSymRevAndExps(0,Yr,ASSET_CLASS,Revenue_Energy) &
                   + ProSymRevAndExps(0,Yr,ASSET_CLASS,MCP_Revenue) &
                   + ProSymRevAndExps(0,Yr,ASSET_CLASS,Revenue_Pool_Energy) &
                   + ProSymRevAndExps(0,Yr,ASSET_CLASS,Revenue_Pumping_Cost) 
         R_OTHER_REVENUES = R_OTHER_REVENUES &
                   + ProSymRevAndExps(0,Yr,ASSET_CLASS,Revenue_Ancillary_Service)
         R_RELATIONSHIP_REVENUES = R_RELATIONSHIP_REVENUES &
                   + ProSymRevAndExps(0,Yr,ASSET_CLASS,Revenue_Availability)
         R_CAPACITY_REVENUES = R_CAPACITY_REVENUES &
                   + ProSymRevAndExps(0,Yr,ASSET_CLASS,Revenue_Capacity)
      RETURN
    
      ENTRY RETURN_ProSymMnthlyRevsExpsBook(R_YEAR, &
                                            R_ASSET_CLASS, &
                                            INC_MONTH_VARS)
    
!
         Yr = get_base_year() + R_YEAR
         IF(.NOT. ALLOCATED(ProSymRevAndExps) .OR. Yr > EndYr) RETURN
!
         ASSET_CLASS = R_ASSET_CLASS
!
! ADD REVENUES
!
         INC_MONTH_VARS(:,monthly_secondary_sales) = &
                   INC_MONTH_VARS(:,monthly_secondary_sales) &
                   + ProSymRevAndExps(:,Yr,ASSET_CLASS,Revenue_Energy) &
                   + ProSymRevAndExps(:,Yr,ASSET_CLASS,MCP_Revenue) &
                   + ProSymRevAndExps(:,Yr,ASSET_CLASS,Revenue_Pool_Energy) &
                   + ProSymRevAndExps(:,Yr,ASSET_CLASS,Revenue_Pumping_Cost)
         INC_MONTH_VARS(:,monthly_other_revenue) = &
                   INC_MONTH_VARS(:,monthly_other_revenue) &
                   + ProSymRevAndExps(:,Yr,ASSET_CLASS,Revenue_Ancillary_Service)
         INC_MONTH_VARS(:,monthly_relationship_revenues) = &
                   INC_MONTH_VARS(:,monthly_relationship_revenues) &
                   + ProSymRevAndExps(:,Yr,ASSET_CLASS,Revenue_Availability)
         INC_MONTH_VARS(:,monthly_capacity_sales) = &
                   INC_MONTH_VARS(:,monthly_capacity_sales) &
                   + ProSymRevAndExps(:,Yr,ASSET_CLASS,Revenue_Capacity)
!
! ADD EXPENSES
!
         INC_MONTH_VARS(:,Monthly_Fossil_Fuel) = &
                   INC_MONTH_VARS(:,Monthly_Fossil_Fuel) &
                   + ProSymRevAndExps(:,Yr,ASSET_CLASS,Total_Fuel_Cost) &
                   + ProSymRevAndExps(:,Yr,ASSET_CLASS,Total_Start_Cost)
         INC_MONTH_VARS(:,monthly_fixed_oandm) = &
                INC_MONTH_VARS(:,monthly_fixed_oandm) &
                + ProSymRevAndExps(:,Yr,ASSET_CLASS,Total_Fixed_OM_Cost)
         INC_MONTH_VARS(:,monthly_variable_oandm) = &
             INC_MONTH_VARS(:,monthly_variable_oandm) &
             + ProSymRevAndExps(:,Yr,ASSET_CLASS,Total_Variable_OM_Cost)
         INC_MONTH_VARS(:,MonthlyEmissionCredits) = &
                 INC_MONTH_VARS(:,MonthlyEmissionCredits) &
                 + ProSymRevAndExps(:,Yr,ASSET_CLASS,NOx_Emissions_Cost) &
                 + ProSymRevAndExps(:,Yr,ASSET_CLASS,SO2_Emissions_Cost)
      RETURN
    
      ENTRY RETURN_ProSymMnthlyRevsExpsCash(R_YEAR, &
                                             R_CLASS, &
                                             CASH_MONTH_VARS)
    

         Yr = get_base_year() + R_YEAR
         IF(.NOT. ALLOCATED(ProSymRevAndExps) .OR. Yr > EndYr) RETURN

         IF(.NOT. R_CLASS <= MAX_ASSET_CLASS_NUM) RETURN
         IF(R_CLASS == 0) THEN
            ASSET_CLASS = 0
         ELSEIF(R_CLASS == -1) THEN
            ASSET_CLASS = -1
         ELSE
            ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
         ENDIF
         IF(ASSET_CLASS > 0 .OR. R_CLASS == 0 .OR. R_CLASS == -1) THEN
!
! ADD REVENUES
!
            CASH_MONTH_VARS(:,CashSecondarySales) = &
                   CASH_MONTH_VARS(:,CashSecondarySales) &
                   + ProSymRevAndExps(:,Yr,ASSET_CLASS,Revenue_Energy) &
                   + ProSymRevAndExps(:,Yr,ASSET_CLASS,MCP_Revenue) &
                   + ProSymRevAndExps(:,Yr,ASSET_CLASS,Revenue_Pool_Energy) &
                   + ProSymRevAndExps(:,Yr,ASSET_CLASS,Revenue_Pumping_Cost)
            CASH_MONTH_VARS(:,Cash_Other_Revenue) = &
                   CASH_MONTH_VARS(:,Cash_Other_Revenue) &
                   + ProSymRevAndExps(:,Yr,ASSET_CLASS,Revenue_Ancillary_Service)
            CASH_MONTH_VARS(:,Cash_Relationship_Revenues) = &
                   CASH_MONTH_VARS(:,monthly_relationship_revenues) &
                   + ProSymRevAndExps(:,Yr,ASSET_CLASS,Revenue_Availability)
            CASH_MONTH_VARS(:,CashCapacitySales) = &
                   CASH_MONTH_VARS(:,monthly_capacity_sales) &
                   + ProSymRevAndExps(:,Yr,ASSET_CLASS,Revenue_Capacity)
!
! ADD EXPENSES
!
            CASH_MONTH_VARS(:,Cash_Fossil_Fuel) = &
                   CASH_MONTH_VARS(:,Cash_Fossil_Fuel) &
                   + ProSymRevAndExps(:,Yr,ASSET_CLASS,Total_Fuel_Cost) &
                   + ProSymRevAndExps(:,Yr,ASSET_CLASS,Total_Start_Cost)
            CASH_MONTH_VARS(:,Cash_Fixed_OandM) = &
                CASH_MONTH_VARS(:,Cash_Fixed_OandM) &
                + ProSymRevAndExps(:,Yr,ASSET_CLASS,Total_Fixed_OM_Cost)
            CASH_MONTH_VARS(:,Cash_Variable_OandM) = &
             CASH_MONTH_VARS(:,Cash_Variable_OandM) &
             + ProSymRevAndExps(:,Yr,ASSET_CLASS,Total_Variable_OM_Cost)
            CASH_MONTH_VARS(:,CashEmissionCredits) = &
                 CASH_MONTH_VARS(:,CashEmissionCredits) &
                 + ProSymRevAndExps(:,Yr,ASSET_CLASS,NOx_Emissions_Cost) &
                 + ProSymRevAndExps(:,Yr,ASSET_CLASS,SO2_Emissions_Cost)
         ENDIF
      RETURN
      END SUBROUTINE ProSymInputData