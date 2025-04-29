!     ******************************************************************
!     CPL_OBJT.FOR
!     Copyright(c) Global Energy Decisions 2000
!
!     Created: 11/7/2006 9:58:04 AM
!     Author : Tom Sweet
!     Editor : Mateo Bilbao
!     Last change: TS 11/7/2006 10:46:53 AM
!     ******************************************************************

! ***********************************************************************
!
!                 ROUTINE TO CONVERT CPL CONTRACT FILE
!
!                              COPYRIGHT (C) 1996
!                         M.S. GERBER & ASSOCIATES, INC.
!                              ALL RIGHTS RESERVED
!
! ***********************************************************************
!
      SUBROUTINE CPL_OBJECT
      use end_routine, only: end_program, er_message
      use filename_tracker
      USE SIZECOM
      use spindriftlib
      use prod_arrays_dimensions

!!! RECORD LENGTH UPDATED WITH VARIABLE CHANGES
      LOGICAL(kind=1) ::   CPL_ACTIVE
      INTEGER(kind=2) ::   DELETE,INUNIT,IREC,LRECL=208,YEAR
      INTEGER :: IOS
!
      INTEGER(kind=2) ::   NUMBER_OF_BC_CLASSES=0, &
                  MAX_BC_CLASS_ID_NUM=0
      INTEGER(kind=2) ::   NUMBER_OF_OL_CLASSES=0, &
                  MAX_OL_CLASS_ID_NUM=0
      INTEGER(kind=2) ::   UNIT_NUM=10,YEAR_OF_RECORD
      INTEGER(kind=2) :: &
                  ASSET_CLASS_ID, &
                  ASSET_ALLOCATION_VECTOR
      REAL(kind=4) :: &
                  SOR_A_RATE, &
                  SOR_B_RATE, &
                  SOR_C_RATE, &
                  SOR_D_RATE, &
                  SOR_E_RATE, &
                  EMC_MONTHLY_FACTOR(12), &
                  EMC_SEPA_CAPACITY, &
                  EMC_RESOURCE_CAPACITY, &
                  SOR_A_RESOURCE, &
                  SOR_D_RESOURCE, &
                  SOR_E_RESOURCE, &
                  PA_SEPA_CAPACITY, &
                  FIRM_PURCHASE_CAPACITY, &
                  MAYO_BUYBACK_RATE, &
                  HARRIS_BUYBACK_RATE, &
                  PA_RESERVE_RATE, &
                  PA_SUPPLEMENTAL_RATE, &
                  PA_TRANSMISSION_RATE, &
                  HARRIS_RETAINED_CAPACITY, &
                  BURNSWICK1_RETAINED_CAPACITY, &
                  BURNSWICK2_RETAINED_CAPACITY, &
                  MAYO_RETAINED_CAPACITY, &
                  ROXBORO4_RETAINED_CAPACITY, &
                  EMC_TRANSMISSION_RATE, &
                  PA_ANN_AVE_PROD, &
                  EMC_ANN_AVE_PROD, &
                  PA_MONTHLY_FACTOR(12)
!
      CHARACTER(len=5) :: BASE_FILE_NAME,OVERLAY_FAMILY_NAME, &
                  CPL_CONTRACT_FILE,CPL_DEFERRED_FUEL_FILE
      CHARACTER(len=1) :: EMC_PEAK,PA_ENERGY_PRICE_SWITCH, &
                  EMC_ENERGY_PRICE_SWITCH,PA_SWITCH
!
      CHARACTER(len=256) :: FILE_NAME
      CHARACTER(len=256) :: BASE_FILE_DIRECTORY
      CHARACTER(len=256) :: DATA_DRIVE,OUTPUT_DIRECTORY
      LOGICAL(kind=4) ::   FILE_EXISTS,CPL_FILE_EXISTS=.FALSE., &
                  R_CPL_FILE_EXISTS

!  DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER(len=1024) :: RECLN
!  DECLARATION FOR CPL CONTRACT DETERMINANTS
      CHARACTER(len=16) :: FILE_TYPE='CPL Contract'
      CHARACTER(len=2) :: CPL_OL='BC'
      INTEGER(kind=2) ::   BC_ASSET_CLASS_POINTER(:), &
                  OL_ASSET_CLASS_POINTER(:), &
                  TEMP_ASSET_CLASS_POINTER(:)
      ALLOCATABLE :: BC_ASSET_CLASS_POINTER, &
                     OL_ASSET_CLASS_POINTER, &
                     TEMP_ASSET_CLASS_POINTER
      SAVE        BC_ASSET_CLASS_POINTER,OL_ASSET_CLASS_POINTER
      INTEGER(kind=2) ::   R_NUM_OF_CLASSES,R_MAX_CLASS_NUM, &
	                       R_CLASS_POINTERS(*)
!
      REAL(kind=4) :: SC_CUMULATIVE_FUEL_COST, &
             SC_CUMULATIVE_ENERGY_SALES, &
             SC_CUMULATIVE_JURIS_SALES, &
             SC_BASE_FUEL_FACTOR, &
             SC_DEFERRED_FUEL_BALANCE
      REAL(kind=4) :: NC_CUMULATIVE_FUEL_COST, &
             NC_CUMULATIVE_ENERGY_SALES, &
             NC_CUMULATIVE_JURIS_SALES, &
             NC_BASE_FUEL_FACTOR, &
             NC_EMF_FACTOR , &
             NC_DEFERRED_FUEL_BALANCE, &
             NC_EMF_FUEL_BALANCE
      REAL(kind=4) :: SC_FUEL_REV_CASH_JAN, &
             NC_FUEL_REV_CASH_JAN, &
             BU_FUEL_REV_CASH_JAN
      INTEGER(kind=2) :: CPL_DEFERRED_FUEL_RPT_CLASS_ID, &
                NC_DEFERRED_FUEL_RPT_CLASS_ID, &
                SC_DEFERRED_FUEL_RPT_CLASS_ID
      LOGICAL(kind=1) :: LAHEY_LF95
      CHARACTER(len=30) :: SCREEN_OUTPUT
!  CONVERT THE CPL CONTRACT FILE
! ***********************************************************************
      ENTRY CPL_MAKEBIN
! ***********************************************************************
      FILE_TYPE = 'CP&L Contract'
      BASE_FILE_NAME = CPL_CONTRACT_FILE()
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = get_pab_filename(base_file_name)
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      CPL_FILE_EXISTS = FILE_EXISTS
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
            CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
         ENDIF
         ALLOCATE(TEMP_ASSET_CLASS_POINTER(1024))
         TEMP_ASSET_CLASS_POINTER = 0
         NUMBER_OF_OL_CLASSES = 0
         MAX_OL_CLASS_ID_NUM = 0
         ASSET_CLASS_ID = 0
         ASSET_ALLOCATION_VECTOR = 0
!
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCCPL.BIN",ACCESS="DIRECT", &
                                            STATUS="UNKNOWN",RECL=LRECL)
         IREC = 1
!
         PA_SWITCH = 'T'
         READ(10,*) DELETE
         DO YEAR_OF_RECORD = 1, 30
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(IOS /= 0) EXIT
            RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
            READ(RECLN,*,ERR=200) DELETE, &
                  YEAR, &
                  ASSET_CLASS_ID, &
                  ASSET_ALLOCATION_VECTOR, &
                  SOR_A_RATE, &
                  SOR_B_RATE, &
                  SOR_C_RATE, &
                  SOR_D_RATE, &
                  SOR_E_RATE, &
                  EMC_PEAK, & !  CHARACTER 9
                  EMC_MONTHLY_FACTOR, &
                  EMC_SEPA_CAPACITY, & !  21
                  EMC_RESOURCE_CAPACITY, &
                  SOR_A_RESOURCE, &
                  SOR_D_RESOURCE, &
                  SOR_E_RESOURCE, &
                  PA_SEPA_CAPACITY, &
                  FIRM_PURCHASE_CAPACITY, &
                  MAYO_BUYBACK_RATE, &
                  HARRIS_BUYBACK_RATE, &
                  PA_RESERVE_RATE, &
                  PA_SUPPLEMENTAL_RATE, &
                  PA_TRANSMISSION_RATE, &
                  HARRIS_RETAINED_CAPACITY, &
                  BURNSWICK1_RETAINED_CAPACITY, &
                  BURNSWICK2_RETAINED_CAPACITY, &
                  MAYO_RETAINED_CAPACITY, &
                  ROXBORO4_RETAINED_CAPACITY, &
                  EMC_TRANSMISSION_RATE, &
                  PA_ENERGY_PRICE_SWITCH, &
                  EMC_ENERGY_PRICE_SWITCH, &
                  PA_ANN_AVE_PROD, &
                  EMC_ANN_AVE_PROD, &
                  PA_MONTHLY_FACTOR, &
                  PA_SWITCH
!
!
!  TRACK ASSET CLASS INFO
!
	      CALL SET_ASSET_CLASSES(ASSET_CLASS_ID, &
                                   NUMBER_OF_BC_CLASSES, &
      	                          MAX_BC_CLASS_ID_NUM, &
                                   TEMP_ASSET_CLASS_POINTER)
!
            WRITE(11,REC=IREC) DELETE, &
                  YEAR, &
                  ASSET_CLASS_ID, &
                  ASSET_ALLOCATION_VECTOR, &
                  SOR_A_RATE, &
                  SOR_B_RATE, &
                  SOR_C_RATE, &
                  SOR_D_RATE, &
                  SOR_E_RATE, &
                  EMC_PEAK, & !  CHARACTER
                  EMC_MONTHLY_FACTOR, &
                  EMC_SEPA_CAPACITY, &
                  EMC_RESOURCE_CAPACITY, &
                  SOR_A_RESOURCE, &
                  SOR_D_RESOURCE, &
                  SOR_E_RESOURCE, &
                  PA_SEPA_CAPACITY, &
                  FIRM_PURCHASE_CAPACITY, &
                  MAYO_BUYBACK_RATE, &
                  HARRIS_BUYBACK_RATE, &
                  PA_RESERVE_RATE, &
                  PA_SUPPLEMENTAL_RATE, &
                  PA_TRANSMISSION_RATE, &
                  HARRIS_RETAINED_CAPACITY, &
                  BURNSWICK1_RETAINED_CAPACITY, &
                  BURNSWICK2_RETAINED_CAPACITY, &
                  MAYO_RETAINED_CAPACITY, &
                  ROXBORO4_RETAINED_CAPACITY, &
                  EMC_TRANSMISSION_RATE, &
                  PA_ENERGY_PRICE_SWITCH, &
                  EMC_ENERGY_PRICE_SWITCH, &
                  PA_ANN_AVE_PROD, &
                  EMC_ANN_AVE_PROD, &
                  PA_MONTHLY_FACTOR, &
                  PA_SWITCH
            IREC = IREC + 1
         ENDDO ! YEAR
         CLOSE(10)
         CLOSE(11)
         IF(MAX_BC_CLASS_ID_NUM > 0) THEN
            ALLOCATE(BC_ASSET_CLASS_POINTER(MAX_BC_CLASS_ID_NUM))
            BC_ASSET_CLASS_POINTER = &
                         TEMP_ASSET_CLASS_POINTER(1:MAX_BC_CLASS_ID_NUM)
         ENDIF
         DEALLOCATE(TEMP_ASSET_CLASS_POINTER)
      ELSEIF(CPL_ACTIVE()) THEN
         WRITE(4,*) '*** line 226 CPL_OBJT.FOR ***'
         WRITE(4,*) "Custom file is required for execution of CPL."
         er_message='See WARNING MESSAGES-cpl_objt-1'
         call end_program(er_message)
      ELSE IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "cpl_objt:0002")
      ENDIF
      RETURN



!  OVERLAY THE CPL CONTRACT FILE
! ***********************************************************************
      ENTRY CPL_MAKEOVL(OVERLAY_FAMILY_NAME)
! ***********************************************************************
      FILE_TYPE = 'CP&L Contract'
      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = get_pao_filename(data_drive, overlay_family_name)
      OPEN(10,FILE=FILE_NAME)
      READ(10,*) DELETE
      INUNIT = 12
      IF(CPL_OL == 'BC') THEN
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCCPL.BIN",ACCESS="DIRECT")
         INUNIT = 11
      ENDIF
      OPEN(12,FILE=trim(DATA_DRIVE)//"OLCPL.BIN",ACCESS="DIRECT", &
                                            STATUS="UNKNOWN",RECL=LRECL)
      ALLOCATE(TEMP_ASSET_CLASS_POINTER(1024))
      TEMP_ASSET_CLASS_POINTER = 0
      IREC = 0
      DO
         IREC = IREC + 1
         READ(INUNIT,REC=IREC,IOSTAT=IOS) DELETE, &
                  YEAR, &
                  ASSET_CLASS_ID, &
                  ASSET_ALLOCATION_VECTOR, &
                  SOR_A_RATE, &
                  SOR_B_RATE, &
                  SOR_C_RATE, &
                  SOR_D_RATE, &
                  SOR_E_RATE, &
                  EMC_PEAK, & !  CHARACTER
                  EMC_MONTHLY_FACTOR, &
                  EMC_SEPA_CAPACITY, &
                  EMC_RESOURCE_CAPACITY, &
                  SOR_A_RESOURCE, &
                  SOR_D_RESOURCE, &
                  SOR_E_RESOURCE, &
                  PA_SEPA_CAPACITY, &
                  FIRM_PURCHASE_CAPACITY, &
                  MAYO_BUYBACK_RATE, &
                  HARRIS_BUYBACK_RATE, &
                  PA_RESERVE_RATE, &
                  PA_SUPPLEMENTAL_RATE, &
                  PA_TRANSMISSION_RATE, &
                  HARRIS_RETAINED_CAPACITY, &
                  BURNSWICK1_RETAINED_CAPACITY, &
                  BURNSWICK2_RETAINED_CAPACITY, &
                  MAYO_RETAINED_CAPACITY, &
                  ROXBORO4_RETAINED_CAPACITY, &
                  EMC_TRANSMISSION_RATE, &
                  PA_ENERGY_PRICE_SWITCH, &
                  EMC_ENERGY_PRICE_SWITCH, &
                  PA_ANN_AVE_PROD, &
                  EMC_ANN_AVE_PROD, &
                  PA_MONTHLY_FACTOR, &
                  PA_SWITCH
         IF(IOS /= 0) EXIT
         READ(10,1000,IOSTAT=IOS) RECLN
         IF(IOS == 0) THEN
            RECLN = trim(RECLN)//',,,,,,,,,,,,,'
            READ(RECLN,*,ERR=300) DELETE, &
                  YEAR, &
                  ASSET_CLASS_ID, &
                  ASSET_ALLOCATION_VECTOR, &
                  SOR_A_RATE, &
                  SOR_B_RATE, &
                  SOR_C_RATE, &
                  SOR_D_RATE, &
                  SOR_E_RATE, &
                  EMC_PEAK, & !  CHARACTER
                  EMC_MONTHLY_FACTOR, &
                  EMC_SEPA_CAPACITY, &
                  EMC_RESOURCE_CAPACITY, &
                  SOR_A_RESOURCE, &
                  SOR_D_RESOURCE, &
                  SOR_E_RESOURCE, &
                  PA_SEPA_CAPACITY, &
                  FIRM_PURCHASE_CAPACITY, &
                  MAYO_BUYBACK_RATE, &
                  HARRIS_BUYBACK_RATE, &
                  PA_RESERVE_RATE, &
                  PA_SUPPLEMENTAL_RATE, &
                  PA_TRANSMISSION_RATE, &
                  HARRIS_RETAINED_CAPACITY, &
                  BURNSWICK1_RETAINED_CAPACITY, &
                  BURNSWICK2_RETAINED_CAPACITY, &
                  MAYO_RETAINED_CAPACITY, &
                  ROXBORO4_RETAINED_CAPACITY, &
                  EMC_TRANSMISSION_RATE, &
                  PA_ENERGY_PRICE_SWITCH, &
                  EMC_ENERGY_PRICE_SWITCH, &
                  PA_ANN_AVE_PROD, &
                  EMC_ANN_AVE_PROD, &
                  PA_MONTHLY_FACTOR, &
                  PA_SWITCH
         ENDIF
!
	   CALL SET_ASSET_CLASSES(ASSET_CLASS_ID, &
                                NUMBER_OF_OL_CLASSES, &
      	                       MAX_OL_CLASS_ID_NUM, &
                                TEMP_ASSET_CLASS_POINTER)
!
         WRITE(12,REC=IREC) DELETE, &
                  YEAR, &
                  ASSET_CLASS_ID, &
                  ASSET_ALLOCATION_VECTOR, &
                  SOR_A_RATE, &
                  SOR_B_RATE, &
                  SOR_C_RATE, &
                  SOR_D_RATE, &
                  SOR_E_RATE, &
                  EMC_PEAK, & !  CHARACTER
                  EMC_MONTHLY_FACTOR, &
                  EMC_SEPA_CAPACITY, &
                  EMC_RESOURCE_CAPACITY, &
                  SOR_A_RESOURCE, &
                  SOR_D_RESOURCE, &
                  SOR_E_RESOURCE, &
                  PA_SEPA_CAPACITY, &
                  FIRM_PURCHASE_CAPACITY, &
                  MAYO_BUYBACK_RATE, &
                  HARRIS_BUYBACK_RATE, &
                  PA_RESERVE_RATE, &
                  PA_SUPPLEMENTAL_RATE, &
                  PA_TRANSMISSION_RATE, &
                  HARRIS_RETAINED_CAPACITY, &
                  BURNSWICK1_RETAINED_CAPACITY, &
                  BURNSWICK2_RETAINED_CAPACITY, &
                  MAYO_RETAINED_CAPACITY, &
                  ROXBORO4_RETAINED_CAPACITY, &
                  EMC_TRANSMISSION_RATE, &
                  PA_ENERGY_PRICE_SWITCH, &
                  EMC_ENERGY_PRICE_SWITCH, &
                  PA_ANN_AVE_PROD, &
                  EMC_ANN_AVE_PROD, &
                  PA_MONTHLY_FACTOR, &
                  PA_SWITCH
      ENDDO
      CLOSE(10)
      CLOSE(12)
      IF(CPL_OL == 'BC') CLOSE(11)
      CPL_OL = 'OL'
      IF(ALLOCATED(OL_ASSET_CLASS_POINTER)) &
                                   DEALLOCATE(OL_ASSET_CLASS_POINTER)
      IF(MAX_OL_CLASS_ID_NUM > 0) THEN
         ALLOCATE(OL_ASSET_CLASS_POINTER(MAX_OL_CLASS_ID_NUM))
         OL_ASSET_CLASS_POINTER = &
                         TEMP_ASSET_CLASS_POINTER(1:MAX_OL_CLASS_ID_NUM)
      ENDIF
      DEALLOCATE(TEMP_ASSET_CLASS_POINTER)
      RETURN

!   200 CALL LOCATE(20,0)
!       WRITE(6,1010) trim(RECLN)
  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from CPL_OBJT SIID70'
      call end_program(er_message)
!   300 CALL LOCATE(20,0)
!       WRITE(6,1010) trim(RECLN)
!       WRITE(6,1010) 'Error reading the above record.  Look for',
!      +              ' a "," in a character name.'
  300 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      CALL MG_LOCATE_WRITE(20,0, &
                    'Error reading Custom Contract record.  Look for'// &
                           ' a "," in a character name.', &
                           ALL_VERSIONS,1)
      er_message='stop requested from CPL_OBJT SIID71'
      call end_program(er_message)
!
! ***********************************************************************
      ENTRY RESET_CPL_OL
! ***********************************************************************
         CPL_OL = 'BC'
      RETURN
!
! ***********************************************************************
      ENTRY OPEN_CPL_ALLOCATOR_FILE
! ***********************************************************************
         OPEN(UNIT_NUM,FILE=trim(OUTPUT_DIRECTORY())//CPL_OL// &
                "CPL.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      RETURN
!
! ***********************************************************************
      ENTRY CLOSE_CPL_ALLOCATOR_FILE
! ***********************************************************************
         CLOSE(UNIT_NUM)
      RETURN
! ***********************************************************************
      ENTRY DOES_CPL_FILE_EXIST(R_CPL_FILE_EXISTS)
! ***********************************************************************
         R_CPL_FILE_EXISTS = CPL_FILE_EXISTS
      RETURN
!
! ***********************************************************************
      ENTRY RETURN_NUM_CPL_CLASSES(R_NUM_OF_CLASSES, &
                                       R_MAX_CLASS_NUM)
! ***********************************************************************
         IF(CPL_OL == 'OL') THEN
            R_NUM_OF_CLASSES = NUMBER_OF_OL_CLASSES
            R_MAX_CLASS_NUM = MAX_OL_CLASS_ID_NUM
         ELSE
            R_NUM_OF_CLASSES = NUMBER_OF_BC_CLASSES
            R_MAX_CLASS_NUM = MAX_BC_CLASS_ID_NUM
         ENDIF
      RETURN
! ***********************************************************************
      ENTRY fill_CPL_POINTERS(R_CLASS_POINTERS)
! ***********************************************************************
         IF(CPL_OL == 'OL') THEN
            R_CLASS_POINTERS(1:MAX_OL_CLASS_ID_NUM) = &
                           OL_ASSET_CLASS_POINTER(1:MAX_OL_CLASS_ID_NUM)
         ELSE
            R_CLASS_POINTERS(1:MAX_BC_CLASS_ID_NUM) = &
                           BC_ASSET_CLASS_POINTER(1:MAX_BC_CLASS_ID_NUM)
         ENDIF
      RETURN
! ***********************************************************************
      ENTRY CPL_DEFERRED_FUEL_MAKN
! ***********************************************************************

!
         FILE_TYPE = "CP&L Def'd Fuel"
         BASE_FILE_NAME = CPL_DEFERRED_FUEL_FILE()
         DATA_DRIVE = OUTPUT_DIRECTORY()
         FILE_NAME = get_dfb_filename(BASE_FILE_NAME)
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         IF(FILE_EXISTS) THEN
!
            IF(LAHEY_LF95()) THEN
               SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
               CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
            ELSE
               CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE, &
                                                         ALL_VERSIONS,0)
               CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
            ENDIF
            OPEN(10,FILE=FILE_NAME)
            OPEN(11,FILE=trim(DATA_DRIVE)//"BCCPLDFL.BIN", &
                            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
            IREC = 1
!
            READ(10,*) DELETE
            DO
               READ(10,1000,IOSTAT=IOS) RECLN
               IF(IOS /= 0) EXIT
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=200) DELETE, &
                                     CPL_DEFERRED_FUEL_RPT_CLASS_ID, &
                                     SC_DEFERRED_FUEL_RPT_CLASS_ID, &
                                     SC_CUMULATIVE_JURIS_SALES, &
                                     SC_BASE_FUEL_FACTOR, &
                                     SC_DEFERRED_FUEL_BALANCE, &
                                     NC_DEFERRED_FUEL_RPT_CLASS_ID, &
                                     NC_CUMULATIVE_FUEL_COST, &
                                     NC_CUMULATIVE_ENERGY_SALES, &
                                     NC_CUMULATIVE_JURIS_SALES, &
                                     NC_BASE_FUEL_FACTOR, &
                                     NC_EMF_FACTOR, &
                                     NC_DEFERRED_FUEL_BALANCE, &
                                     NC_EMF_FUEL_BALANCE, &
                                     SC_FUEL_REV_CASH_JAN, &
                                     NC_FUEL_REV_CASH_JAN
!
               WRITE(11,REC=IREC) DELETE, &
                                  CPL_DEFERRED_FUEL_RPT_CLASS_ID, &
                                  SC_DEFERRED_FUEL_RPT_CLASS_ID, &
                                  SC_CUMULATIVE_JURIS_SALES, &
                                  SC_BASE_FUEL_FACTOR, &
                                  SC_DEFERRED_FUEL_BALANCE, &
                                  NC_DEFERRED_FUEL_RPT_CLASS_ID, &
                                  NC_CUMULATIVE_FUEL_COST, &
                                  NC_CUMULATIVE_ENERGY_SALES, &
                                  NC_CUMULATIVE_JURIS_SALES, &
                                  NC_BASE_FUEL_FACTOR, &
                                  NC_EMF_FACTOR, &
                                  NC_DEFERRED_FUEL_BALANCE, &
                                  NC_EMF_FUEL_BALANCE, &
                                  SC_FUEL_REV_CASH_JAN, &
                                  NC_FUEL_REV_CASH_JAN
               IREC = IREC + 1
            ENDDO ! RECORDS
            CLOSE(10)
            CLOSE(11)
         ELSE IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
            CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "cpl_objt:0001")
         ENDIF
      RETURN
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END

