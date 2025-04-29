
!     CAT2OBJT.FOR
!     Copyright(c)  2000
!
!     Created: 6/20/2003 3:37:14 PM
!     Author : MARK S GERBER
!     Last change: MSG 1/10/2010 3:02:24 PM



!
!                ROUTINE TO CONVERT CATAWBA CONTRACT FILE
!
!                             COPYRIGHT (C) 1996
!                        M.S. GERBER & ASSOCIATES, INC.
!                             ALL RIGHTS RESERVED
!

!
      SUBROUTINE CAT2_OBJECT
      use end_routine, only: end_program, er_message
      use SpinDriftLib
      use prod_arrays_dimensions
      use logging
      use filename_tracker
      use procost_data
      use sizecom
	  use globecom
	  use forecast
	  use lamcom



      INTEGER (kind=2) ::  DELETE,INUNIT,IREC,LRECL=72 ,YEAR_loc
      INTEGER ::  IOS
      INTEGER (kind=2) ::  NUMBER_OF_BC_CLASSES=0
      INTEGER (kind=2) ::  MAX_BC_CLASS_ID_NUM=0
      INTEGER (kind=2) ::  NUMBER_OF_OL_CLASSES=0
      INTEGER (kind=2) ::  MAX_OL_CLASS_ID_NUM=0
      INTEGER (kind=2) ::  UNIT_NUM=10 ,YEAR_OF_RECORD
      INTEGER (kind=2) ::  ASSET_CLASS_NUM
      INTEGER (kind=2) ::  ASSET_CLASS_VECTOR
      REAL (kind=4) ::  SUPPLEMENTAL_CAPACITY_REV
      REAL (kind=4) ::  RESERVE_CAPACITY_REV
      REAL (kind=4) ::  WHEELING_REV
      REAL (kind=4) ::  OEI_REV
      REAL (kind=4) ::  PKR_REV
      REAL (kind=4) ::  SUPPLEMENTAL_ENERGY_REV
      REAL (kind=4) ::  EXCHANGE_CAPACITY_EXP
      REAL (kind=4) ::  PURCHASED_ENERGY_EXP
      REAL (kind=4) ::  SURPLUS_ENERGY_EXP
      REAL (kind=4) ::  EXCHANGE_ENERGY_EXP
      REAL (kind=4) ::  PURCHASED_CAP_PAYMENTS
      REAL (kind=4) ::  LEVEL_PURCHASED_CAP_PAYMENTS
      REAL (kind=4) ::  OTHER_NET_REV_RETURN
      REAL (kind=4) ::  PURCHASED_CAP_DEFERRED_DEBITS
      REAL (kind=4) ::  DEFERRED_TAXES_PURCHASED_CAP
      REAL (kind=4) ::  IS_DEFERRED_TAXES
!
      CHARACTER (len=5) ::  BASE_FILE_NAME
      CHARACTER (len=5) ::  OVERLAY_FAMILY_NAME
      CHARACTER (len=5) ::  CATAWBA2_CONTRACT_FILE
      CHARACTER (len=256) ::  FILE_NAME
      CHARACTER (len=256) ::  BASE_FILE_DIRECTORY
      CHARACTER (len=256) ::  DATA_DRIVE,OUTPUT_DIRECTORY
      LOGICAL (kind=4) ::  FILE_EXISTS=.FALSE.
      LOGICAL (kind=4) ::  R_CATAWBA2_FILE_EXISTS

! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER (len=1024) ::  RECLN
! DECLARATION FOR CATAWBA CONTRACT DETERMINANTS
      CHARACTER (len=16) ::  FILE_TYPE='Catawba Contract'
      CHARACTER (len=2) ::  CATAW2_OL='BC'
      INTEGER (kind=2) ::  BC_ASSET_CLASS_POINTER(:)
      INTEGER (kind=2) ::  OL_ASSET_CLASS_POINTER(:)
      INTEGER (kind=2) ::  TEMP_ASSET_CLASS_POINTER(:)
      ALLOCATABLE :: BC_ASSET_CLASS_POINTER, &
                     OL_ASSET_CLASS_POINTER, &
                     TEMP_ASSET_CLASS_POINTER
      SAVE BC_ASSET_CLASS_POINTER,OL_ASSET_CLASS_POINTER
      INTEGER (kind=2) ::  R_NUM_OF_CLASSES
      INTEGER (kind=2) ::  R_MAX_CLASS_NUM,R_CLASS_POINTERS(*)
      LOGICAL (kind=1) ::  LAHEY_LF95
      CHARACTER (len=30) ::  SCREEN_OUTPUT
!
! CONVERT THE CATAWBA CONTRACT FILE

      ENTRY CAT2_MAKEBIN

      BASE_FILE_NAME = CATAWBA2_CONTRACT_FILE()
      DATA_DRIVE = OUTPUT_DIRECTORY()

       file_name=get_c2b_filename()

      INQUIRE(FILE=FILE_NAME,EXIST=file_exists)
      CATAWBA2_FILE_EXISTS = file_exists

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
         ASSET_CLASS_NUM = 0
         ASSET_CLASS_VECTOR = 0

         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCCATA2.BIN",ACCESS="DIRECT", &
                                            STATUS="UNKNOWN",RECL=LRECL)
         IREC = 1
!
         READ(10,*) DELETE
         DO YEAR_OF_RECORD = 1, 30
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(IOS /= 0) EXIT
            RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
            READ(RECLN,*,ERR=200) DELETE, &
                  year_loc, &
                  SUPPLEMENTAL_CAPACITY_REV, &
                  RESERVE_CAPACITY_REV, &
                  WHEELING_REV, &
                  OEI_REV, &
                  PKR_REV, &
                  SUPPLEMENTAL_ENERGY_REV, &
                  EXCHANGE_CAPACITY_EXP, &
                  PURCHASED_ENERGY_EXP, &
                  SURPLUS_ENERGY_EXP, &
                  EXCHANGE_ENERGY_EXP, &
                  PURCHASED_CAP_PAYMENTS, &
                  LEVEL_PURCHASED_CAP_PAYMENTS, &
                  OTHER_NET_REV_RETURN, &
                  PURCHASED_CAP_DEFERRED_DEBITS, &
                  DEFERRED_TAXES_PURCHASED_CAP, &
                  IS_DEFERRED_TAXES, &
                  ASSET_CLASS_NUM, &
                  ASSET_CLASS_VECTOR
!
!
! TRACK ASSET CLASS INFO
!
          CALL SET_ASSET_CLASSES(ASSET_CLASS_NUM, &
                                   NUMBER_OF_BC_CLASSES, &
                                    MAX_BC_CLASS_ID_NUM, &
                                   TEMP_ASSET_CLASS_POINTER)
!
            WRITE(11,REC=IREC) DELETE, &
                  year_loc, &
                  SUPPLEMENTAL_CAPACITY_REV, &
                  RESERVE_CAPACITY_REV, &
                  WHEELING_REV, &
                  OEI_REV, &
                  PKR_REV, &
                  SUPPLEMENTAL_ENERGY_REV, &
                  EXCHANGE_CAPACITY_EXP, &
                  PURCHASED_ENERGY_EXP, &
                  SURPLUS_ENERGY_EXP, &
                  EXCHANGE_ENERGY_EXP, &
                  PURCHASED_CAP_PAYMENTS, &
                  LEVEL_PURCHASED_CAP_PAYMENTS, &
                  OTHER_NET_REV_RETURN, &
                  PURCHASED_CAP_DEFERRED_DEBITS, &
                  DEFERRED_TAXES_PURCHASED_CAP, &
                  IS_DEFERRED_TAXES, &
                  ASSET_CLASS_NUM, &
                  ASSET_CLASS_VECTOR
            IREC = IREC + 1
         ENDDO ! YEAR
         CLOSE(10)
         CLOSE(11)
         IF(MAX_BC_CLASS_ID_NUM > 0) THEN
            ALLOCATE(BC_ASSET_CLASS_POINTER(MAX_BC_CLASS_ID_NUM))
            BC_ASSET_CLASS_POINTER(1:MAX_BC_CLASS_ID_NUM) = &
                         TEMP_ASSET_CLASS_POINTER(1:MAX_BC_CLASS_ID_NUM)
         ENDIF
         DEALLOCATE(TEMP_ASSET_CLASS_POINTER)
      ELSE IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "cat2objt:0001")
      ENDIF
      RETURN



! OVERLAY THE CATAWBA CONTRACT FILE

      ENTRY CAT2_MAKEOVL(OVERLAY_FAMILY_NAME)

      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = trim(DATA_DRIVE)//"C2O"// &
         trim(OVERLAY_FAMILY_NAME)//".DAT"
      OPEN(10,FILE=FILE_NAME)
      READ(10,*) DELETE
      INUNIT = 12
      IF(CATAW2_OL == 'BC') THEN
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCCATA2.BIN", &
                                             ACCESS="DIRECT",RECL=LRECL)
         INUNIT = 11
      ENDIF
      OPEN(12,FILE=trim(DATA_DRIVE)//"OLCATA2.BIN",ACCESS="DIRECT", &
                                            STATUS="UNKNOWN",RECL=LRECL)
      ALLOCATE(TEMP_ASSET_CLASS_POINTER(1024))
      TEMP_ASSET_CLASS_POINTER = 0
      IREC = 0
      DO
         IREC = IREC + 1
         READ(INUNIT,REC=IREC,IOSTAT=IOS) DELETE, &
                  year_loc, &
                  SUPPLEMENTAL_CAPACITY_REV, &
                  RESERVE_CAPACITY_REV, &
                  WHEELING_REV, &
                  OEI_REV, &
                  PKR_REV, &
                  SUPPLEMENTAL_ENERGY_REV, &
                  EXCHANGE_CAPACITY_EXP, &
                  PURCHASED_ENERGY_EXP, &
                  SURPLUS_ENERGY_EXP, &
                  EXCHANGE_ENERGY_EXP, &
                  PURCHASED_CAP_PAYMENTS, &
                  LEVEL_PURCHASED_CAP_PAYMENTS, &
                  OTHER_NET_REV_RETURN, &
                  PURCHASED_CAP_DEFERRED_DEBITS, &
                  DEFERRED_TAXES_PURCHASED_CAP, &
                  IS_DEFERRED_TAXES, &
                  ASSET_CLASS_NUM, &
                  ASSET_CLASS_VECTOR
         IF(IOS /= 0) EXIT
         READ(10,1000,IOSTAT=IOS) RECLN
         IF(IOS == 0) THEN
            RECLN = trim(RECLN)//',,,,,,,,,,,,,'
            READ(RECLN,*,ERR=300) DELETE, &
                  year_loc, &
                  SUPPLEMENTAL_CAPACITY_REV, &
                  RESERVE_CAPACITY_REV, &
                  WHEELING_REV, &
                  OEI_REV, &
                  PKR_REV, &
                  SUPPLEMENTAL_ENERGY_REV, &
                  EXCHANGE_CAPACITY_EXP, &
                  PURCHASED_ENERGY_EXP, &
                  SURPLUS_ENERGY_EXP, &
                  EXCHANGE_ENERGY_EXP, &
                  PURCHASED_CAP_PAYMENTS, &
                  LEVEL_PURCHASED_CAP_PAYMENTS, &
                  OTHER_NET_REV_RETURN, &
                  PURCHASED_CAP_DEFERRED_DEBITS, &
                  DEFERRED_TAXES_PURCHASED_CAP, &
                  IS_DEFERRED_TAXES, &
                  ASSET_CLASS_NUM, &
                  ASSET_CLASS_VECTOR
         ENDIF
!
       CALL SET_ASSET_CLASSES(ASSET_CLASS_NUM, &
                                NUMBER_OF_OL_CLASSES, &
                                 MAX_OL_CLASS_ID_NUM, &
                                TEMP_ASSET_CLASS_POINTER)
!
         WRITE(12,REC=IREC) DELETE, &
                  year_loc, &
                  SUPPLEMENTAL_CAPACITY_REV, &
                  RESERVE_CAPACITY_REV, &
                  WHEELING_REV, &
                  OEI_REV, &
                  PKR_REV, &
                  SUPPLEMENTAL_ENERGY_REV, &
                  EXCHANGE_CAPACITY_EXP, &
                  PURCHASED_ENERGY_EXP, &
                  SURPLUS_ENERGY_EXP, &
                  EXCHANGE_ENERGY_EXP, &
                  PURCHASED_CAP_PAYMENTS, &
                  LEVEL_PURCHASED_CAP_PAYMENTS, &
                  OTHER_NET_REV_RETURN, &
                  PURCHASED_CAP_DEFERRED_DEBITS, &
                  DEFERRED_TAXES_PURCHASED_CAP, &
                  IS_DEFERRED_TAXES, &
                  ASSET_CLASS_NUM, &
                  ASSET_CLASS_VECTOR
      ENDDO
      CLOSE(10)
      CLOSE(12)
      IF(CATAW2_OL == 'BC') CLOSE(11)
      CATAW2_OL = 'OL'
      IF(ALLOCATED(OL_ASSET_CLASS_POINTER)) &
                                   DEALLOCATE(OL_ASSET_CLASS_POINTER)
      IF(MAX_OL_CLASS_ID_NUM > 0) THEN
         ALLOCATE(OL_ASSET_CLASS_POINTER(MAX_OL_CLASS_ID_NUM))
         OL_ASSET_CLASS_POINTER(1:MAX_OL_CLASS_ID_NUM) = &
                         TEMP_ASSET_CLASS_POINTER(1:MAX_OL_CLASS_ID_NUM)
      ENDIF
      DEALLOCATE(TEMP_ASSET_CLASS_POINTER)
      RETURN


  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from CAT2OBJT SIID7'
      call end_program(er_message)

  300 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      CALL MG_LOCATE_WRITE(20,0, &
                     'Error reading Custom Contract record.  Look for'// &
                           ' a "," in a character name.', &
                           ALL_VERSIONS,1)
      er_message='stop requested from CAT2OBJT SIID8'
      call end_program(er_message)
!

      ENTRY RESET_CATAW2_OL

         CATAW2_OL = 'BC'
      RETURN
!

      ENTRY OPEN_CATAW2_ALLOCATOR_FILE

         OPEN(UNIT_NUM,FILE=trim(OUTPUT_DIRECTORY())//CATAW2_OL// &
                "CATA2.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      RETURN
!

      ENTRY CLOSE_CATAW2_ALLOCATOR_FILE

         CLOSE(UNIT_NUM)
      RETURN

      ENTRY DOES_CATAWBA2_FILE_EXIST(R_CATAWBA2_FILE_EXISTS)

         R_CATAWBA2_FILE_EXISTS = CATAWBA2_FILE_EXISTS
      RETURN
!

      ENTRY RETURN_NUM_CATAWBA_CLASSES(R_NUM_OF_CLASSES, &
                                       R_MAX_CLASS_NUM)

         IF(CATAW2_OL == 'OL') THEN
            R_NUM_OF_CLASSES = NUMBER_OF_OL_CLASSES
            R_MAX_CLASS_NUM = MAX_OL_CLASS_ID_NUM
         ELSE
            R_NUM_OF_CLASSES = NUMBER_OF_BC_CLASSES
            R_MAX_CLASS_NUM = MAX_BC_CLASS_ID_NUM
         ENDIF
      RETURN

      ENTRY RETURN_CATAWBA_POINTERS(R_CLASS_POINTERS)

         IF(CATAW2_OL == 'OL') THEN
            R_CLASS_POINTERS(1:MAX_OL_CLASS_ID_NUM) = &
                           OL_ASSET_CLASS_POINTER(1:MAX_OL_CLASS_ID_NUM)
         ELSE
            R_CLASS_POINTERS(1:MAX_BC_CLASS_ID_NUM) = &
                           BC_ASSET_CLASS_POINTER(1:MAX_BC_CLASS_ID_NUM)
         ENDIF
      RETURN
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END

      FUNCTION UPDATE_CATAWBA_CONTRACT_DATA()

!
      use SpinDriftLib
      use prod_arrays_dimensions
      use miscmod
      USE SIZECOM
      use globecom


      LOGICAL (kind=1) ::  UPDATE_CATAWBA_CONTRACT_DATA,CATAWBA_INFO
      INTEGER (kind=2) ::  DELETE
      INTEGER (kind=2) ::  S_YEAR
      INTEGER (kind=2) ::  NUMBER_OF_PARTIES
      INTEGER ::  IOS
      PARAMETER(NUMBER_OF_PARTIES=4)
      LOGICAL (kind=4) ::  CATAWBA_FILE_EXISTS
      SAVE CATAWBA_FILE_EXISTS

      REAL (kind=4) ::  S_SUPPLEMENTAL_CAPACITY_REV=0.
      REAL (kind=4) ::  S_RESERVE_CAPACITY_REV=0.
      REAL (kind=4) ::  S_WHEELING_REV=0.
      REAL (kind=4) ::  S_OEI_REV=0.
      REAL (kind=4) ::  S_PKR_REV=0.
      REAL (kind=4) ::  S_SUPPLEMENTAL_ENERGY_REV=0.
      REAL (kind=4) ::  S_EXCHANGE_CAPACITY_EXP=0.
      REAL (kind=4) ::  S_PURCHASED_ENERGY_EXP=0.
      REAL (kind=4) ::  S_SURPLUS_ENERGY_EXP=0.
      REAL (kind=4) ::  S_EXCHANGE_ENERGY_EXP=0.
      REAL (kind=4) ::  S_PURCHASED_CAP_PAYMENTS=0.
      REAL (kind=4) ::  S_LEVEL_PURCHASED_CAP_PAYMENTS=0.
      REAL (kind=4) ::  S_OTHER_NET_REV_RETURN=0.
      REAL (kind=4) ::  S_PURCHASED_CAP_DEFERRED_DEBITS=0.
      REAL (kind=4) ::  S_DEFERRED_TAXES_PURCHASED_CAP=0.
      REAL (kind=4) ::  S_IS_DEFERRED_TAXES=0.
      INTEGER (kind=2) ::  ASSET_CLASS=0
      INTEGER (kind=2) ::  ASSET_ALLOCATION_VECTOR=0
!
!
      REAL (kind=4) ::  CATAWBA_REVENUES(:,:)
      REAL (kind=4) ::  CATAWBA_EXPENSES(:,:)
      REAL (kind=4) ::  CATAWBA_CAPACITY_PAYMENTS(:,:)
      REAL (kind=4) ::  CATAWBA_LEVEL_CAP_PAYMENTS(:,:)
      REAL (kind=4) ::  CATAWBA_OTHER_NET_REVENUES(:,:)
      REAL (kind=4) ::  CATAWBA_DEFERRED_DEBITS(:,:)
      REAL (kind=4) ::  CATAWBA_DEFERRED_TAXES_BAL_CR(:,:)
      REAL (kind=4) ::  CATAWBA_DEFERRED_TAXES_CR(:,:)
      ALLOCATABLE :: CATAWBA_REVENUES, &
                     CATAWBA_EXPENSES, &
                     CATAWBA_CAPACITY_PAYMENTS, &
                     CATAWBA_LEVEL_CAP_PAYMENTS, &
                     CATAWBA_OTHER_NET_REVENUES, &
                     CATAWBA_DEFERRED_DEBITS, &
                     CATAWBA_DEFERRED_TAXES_BAL_CR, &
                     CATAWBA_DEFERRED_TAXES_CR
      SAVE CATAWBA_REVENUES, &
           CATAWBA_EXPENSES, &
           CATAWBA_CAPACITY_PAYMENTS, &
           CATAWBA_LEVEL_CAP_PAYMENTS, &
           CATAWBA_OTHER_NET_REVENUES, &
           CATAWBA_DEFERRED_DEBITS, &
           CATAWBA_DEFERRED_TAXES_BAL_CR, &
           CATAWBA_DEFERRED_TAXES_CR
      INTEGER (kind=2) ::  ASSET_CLASS_POINTER(:)
      INTEGER (kind=2) ::  MAX_ASSET_CLASS_NUM
      INTEGER (kind=2) ::  NUM_OF_ASSET_CLASSES
      ALLOCATABLE :: ASSET_CLASS_POINTER
      SAVE ASSET_CLASS_POINTER,MAX_ASSET_CLASS_NUM,NUM_OF_ASSET_CLASSES
      INTEGER (kind=4) ::  VALUES_TO_ZERO
      CHARACTER (len=1) ::  DUMMY_TYPE
      REAL (kind=4) ::  ASSET_CLASS_LIST(:),ASSET_ALLOCATION_LIST(:)
      ALLOCATABLE :: ASSET_CLASS_LIST,ASSET_ALLOCATION_LIST
      REAL (kind=4) ::  TOTAL_CATAWBA_REVENUES,TOTAL_CATAWBA_EXPENSES
      INTEGER (kind=2) ::  YR,R_YR
      REAL (kind=4) ::  R_CATAWBA_REVENUES
      REAL (kind=4) ::  R_CATAWBA_EXPENSES
      REAL (kind=4) ::  R_CATAWBA_CAPACITY_PAYMENTS
      REAL (kind=4) ::  R_CATAWBA_LEVEL_CAP_PAYMENTS
      REAL (kind=4) ::  R_CATAWBA_OTHER_NET_REVENUES
      REAL (kind=4) ::  R_CATAWBA_DEFERRED_DEBITS
      REAL (kind=4) ::  R_CATAWBA_DEFER_TAXES_BAL_CR
      REAL (kind=4) ::  R_CATAWBA_DEFERRED_TAXES_CR
      INTEGER (kind=2) ::  CLASS_POINTER,R_CLASS
      REAL (kind=4) ::  ASSET_ALLOCATOR
      REAL (kind=4) ::  S_CATAWBA_DEF_TAX_BY_BAL
      REAL (kind=4) ::  S_CATAWBA_DEF_DEBT_BY_BAL
      SAVE S_CATAWBA_DEF_TAX_BY_BAL, &
           S_CATAWBA_DEF_DEBT_BY_BAL
      REAL (kind=4) ::  CATAWBA_BY_DEBITS_BALANCE
      REAL (kind=4) ::  CATAWBA_BY_DEF_TAX_BALANCE
      REAL (kind=4) ::  CATAWBA_DEFERRED_TAXES
      REAL (kind=4) ::  CATAWBA_TOTAL_OTHER_NET_REVS
      REAL (kind=4) ::  CATAWBA_TOTAL_LEVEL_CAP_PAYMTS
      REAL (kind=4) ::  CATAWBA_TOTAL_CAP_PAYMENTS
      REAL (kind=4) ::  CATAWBA_TOTAL_EXPENSES
      REAL (kind=4) ::  CATAWBA_TOTAL_REVENUES
!
! END DATA DECLARATIONS
!
         CALL DOES_CATAWBA2_FILE_EXIST(CATAWBA_FILE_EXISTS)
         UPDATE_CATAWBA_CONTRACT_DATA = CATAWBA_FILE_EXISTS
         IF(.NOT. CATAWBA_FILE_EXISTS) RETURN
!
!
! ASSET CLASS EXPENSE AND REVENUE INFORMATION
!
         CALL RETURN_NUM_CATAWBA_CLASSES(NUM_OF_ASSET_CLASSES, &
                                         MAX_ASSET_CLASS_NUM)
         IF(ALLOCATED(ASSET_CLASS_POINTER)) &
                                         DEALLOCATE(ASSET_CLASS_POINTER)
         IF(MAX_ASSET_CLASS_NUM > 0) THEN
            ALLOCATE(ASSET_CLASS_POINTER(MAX_ASSET_CLASS_NUM))
            CALL RETURN_CATAWBA_POINTERS(ASSET_CLASS_POINTER)
         ENDIF
         IF(ALLOCATED(CATAWBA_REVENUES)) THEN
            DEALLOCATE(CATAWBA_REVENUES, &
                       CATAWBA_EXPENSES, &
                       CATAWBA_CAPACITY_PAYMENTS, &
                       CATAWBA_LEVEL_CAP_PAYMENTS, &
                       CATAWBA_OTHER_NET_REVENUES, &
                       CATAWBA_DEFERRED_DEBITS, &
                       CATAWBA_DEFERRED_TAXES_BAL_CR, &
                       CATAWBA_DEFERRED_TAXES_CR)
         ENDIF
         ALLOCATE(CATAWBA_REVENUES(0:NUM_OF_ASSET_CLASSES, &
                                                      AVAIL_DATA_YEARS), &
                  CATAWBA_EXPENSES(0:NUM_OF_ASSET_CLASSES, &
                                                      AVAIL_DATA_YEARS), &
                  CATAWBA_CAPACITY_PAYMENTS(0:NUM_OF_ASSET_CLASSES, &
                                                      AVAIL_DATA_YEARS), &
                  CATAWBA_LEVEL_CAP_PAYMENTS(0:NUM_OF_ASSET_CLASSES, &
                                                      AVAIL_DATA_YEARS), &
                  CATAWBA_OTHER_NET_REVENUES(0:NUM_OF_ASSET_CLASSES, &
                                                      AVAIL_DATA_YEARS), &
                  CATAWBA_DEFERRED_TAXES_CR(0:NUM_OF_ASSET_CLASSES, &
                                                      AVAIL_DATA_YEARS), &
                  CATAWBA_DEFERRED_DEBITS(0:NUM_OF_ASSET_CLASSES, &
                                                    0:AVAIL_DATA_YEARS), &
                  CATAWBA_DEFERRED_TAXES_BAL_CR(0:NUM_OF_ASSET_CLASSES, &
                                                    0:AVAIL_DATA_YEARS))
!
         CATAWBA_REVENUES = 0.
         CATAWBA_EXPENSES = 0.
         CATAWBA_CAPACITY_PAYMENTS = 0.
         CATAWBA_LEVEL_CAP_PAYMENTS = 0.
         CATAWBA_OTHER_NET_REVENUES = 0.
         CATAWBA_DEFERRED_TAXES_CR = 0.
         CATAWBA_DEFERRED_DEBITS = 0.
         CATAWBA_DEFERRED_TAXES_BAL_CR = 0.
         ALLOCATE(ASSET_CLASS_LIST(AVAIL_DATA_YEARS), &
                  ASSET_ALLOCATION_LIST(AVAIL_DATA_YEARS))
         CALL OPEN_CATAW2_ALLOCATOR_FILE
         DO YR = 1, AVAIL_DATA_YEARS
            READ(10,IOSTAT=IOS) DELETE,S_YEAR, &
                           S_SUPPLEMENTAL_CAPACITY_REV, &
                           S_RESERVE_CAPACITY_REV, &
                           S_WHEELING_REV, &
                           S_OEI_REV, &
                           S_PKR_REV, &
                           S_SUPPLEMENTAL_ENERGY_REV, &
                           S_EXCHANGE_CAPACITY_EXP, &
                           S_PURCHASED_ENERGY_EXP, &
                           S_SURPLUS_ENERGY_EXP, &
                           S_EXCHANGE_ENERGY_EXP, &
                           S_PURCHASED_CAP_PAYMENTS, &
                           S_LEVEL_PURCHASED_CAP_PAYMENTS, &
                           S_OTHER_NET_REV_RETURN, &
                           S_PURCHASED_CAP_DEFERRED_DEBITS, &
                           S_DEFERRED_TAXES_PURCHASED_CAP, &
                           S_IS_DEFERRED_TAXES, &
                           ASSET_CLASS, &
                           ASSET_ALLOCATION_VECTOR
            IF(IOS /= 0) EXIT
            IF(YR == 1) THEN
               S_CATAWBA_DEF_TAX_BY_BAL=S_DEFERRED_TAXES_PURCHASED_CAP + &
                                        S_IS_DEFERRED_TAXES
               S_CATAWBA_DEF_DEBT_BY_BAL = &
                                       S_PURCHASED_CAP_DEFERRED_DEBITS - &
                                       S_LEVEL_PURCHASED_CAP_PAYMENTS - &
                                       S_OTHER_NET_REV_RETURN
            ENDIF
!
! CALCULATIONS
!
            TOTAL_CATAWBA_REVENUES = S_SUPPLEMENTAL_CAPACITY_REV + &
                                     S_RESERVE_CAPACITY_REV + &
                                     S_WHEELING_REV + &
                                     S_OEI_REV + &
                                     S_PKR_REV + &
                                     S_SUPPLEMENTAL_ENERGY_REV
            TOTAL_CATAWBA_EXPENSES = S_EXCHANGE_CAPACITY_EXP + &
                                     S_PURCHASED_ENERGY_EXP + &
                                     S_SURPLUS_ENERGY_EXP + &
                                     S_EXCHANGE_ENERGY_EXP
!
! ALLOCATE TO TOTAL COMPANY AND ASSET CLASSES
!
            IF(ASSET_CLASS < 0.) THEN
               CALL GET_ASSET_VAR(ABS(ASSET_CLASS), &
                                            DUMMY_TYPE,ASSET_CLASS_LIST)
               CALL GET_ASSET_VAR(ABS(ASSET_ALLOCATION_VECTOR), &
                                       DUMMY_TYPE,ASSET_ALLOCATION_LIST)
            ELSE
               ASSET_CLASS_LIST(1) = ASSET_CLASS
               ASSET_CLASS_LIST(2) = 0
               ASSET_ALLOCATION_LIST(1) = 100.
               ASSET_ALLOCATION_LIST(2) = 0.
            ENDIF
!
            CLASS_POINTER = 1
            DO
               ASSET_CLASS = ASSET_CLASS_LIST(CLASS_POINTER) + 1
               IF(ASSET_CLASS > 0) ASSET_CLASS = &
                                        ASSET_CLASS_POINTER(ASSET_CLASS)
               ASSET_ALLOCATOR=ASSET_ALLOCATION_LIST(CLASS_POINTER)/100.
!
               IF(YR == 1) THEN
                  CATAWBA_DEFERRED_DEBITS(ASSET_CLASS,0) = &
                             CATAWBA_DEFERRED_DEBITS(ASSET_CLASS,0) + &
                             ASSET_ALLOCATOR * S_CATAWBA_DEF_DEBT_BY_BAL
                  CATAWBA_DEFERRED_TAXES_BAL_CR(ASSET_CLASS,0) = &
                          CATAWBA_DEFERRED_TAXES_BAL_CR(ASSET_CLASS,0) + &
                          ASSET_ALLOCATOR * S_CATAWBA_DEF_TAX_BY_BAL
               ENDIF
               CATAWBA_REVENUES(ASSET_CLASS,YR) = &
                                CATAWBA_REVENUES(ASSET_CLASS,YR) + &
                                ASSET_ALLOCATOR * TOTAL_CATAWBA_REVENUES
               CATAWBA_EXPENSES(ASSET_CLASS,YR) = &
                                CATAWBA_EXPENSES(ASSET_CLASS,YR) + &
                                ASSET_ALLOCATOR * TOTAL_CATAWBA_EXPENSES
               CATAWBA_CAPACITY_PAYMENTS(ASSET_CLASS,YR) = &
                              CATAWBA_CAPACITY_PAYMENTS(ASSET_CLASS,YR)+ &
                              ASSET_ALLOCATOR * S_PURCHASED_CAP_PAYMENTS
               CATAWBA_LEVEL_CAP_PAYMENTS(ASSET_CLASS,YR) = &
                        CATAWBA_LEVEL_CAP_PAYMENTS(ASSET_CLASS,YR) + &
                        ASSET_ALLOCATOR * S_LEVEL_PURCHASED_CAP_PAYMENTS
               CATAWBA_OTHER_NET_REVENUES(ASSET_CLASS,YR) = &
                             CATAWBA_OTHER_NET_REVENUES(ASSET_CLASS,YR)+ &
                             ASSET_ALLOCATOR * S_OTHER_NET_REV_RETURN
               CATAWBA_DEFERRED_DEBITS(ASSET_CLASS,YR) = &
                       CATAWBA_DEFERRED_DEBITS(ASSET_CLASS,YR) + &
                       ASSET_ALLOCATOR * S_PURCHASED_CAP_DEFERRED_DEBITS
               CATAWBA_DEFERRED_TAXES_BAL_CR(ASSET_CLASS,YR) = &
                        CATAWBA_DEFERRED_TAXES_BAL_CR(ASSET_CLASS,YR) + &
                        ASSET_ALLOCATOR * S_DEFERRED_TAXES_PURCHASED_CAP
               CATAWBA_DEFERRED_TAXES_CR(ASSET_CLASS,YR) = &
                              CATAWBA_DEFERRED_TAXES_CR(ASSET_CLASS,YR)+ &
                              ASSET_ALLOCATOR * S_IS_DEFERRED_TAXES
               CLASS_POINTER = CLASS_POINTER + 1
               IF(CLASS_POINTER > AVAIL_DATA_YEARS) EXIT
               IF(ASSET_CLASS_LIST(CLASS_POINTER) == 0. .OR. &
                            ASSET_CLASS_LIST(CLASS_POINTER) ==-99.) then
                    EXIT
               end if
            ENDDO
            DO ASSET_CLASS = 1, NUM_OF_ASSET_CLASSES
               CATAWBA_REVENUES(0,YR) = CATAWBA_REVENUES(0,YR) + &
                                        CATAWBA_REVENUES(ASSET_CLASS,YR)
               CATAWBA_EXPENSES(0,YR) = CATAWBA_EXPENSES(0,YR) + &
                                        CATAWBA_EXPENSES(ASSET_CLASS,YR)
               CATAWBA_CAPACITY_PAYMENTS(0,YR) = &
                               CATAWBA_CAPACITY_PAYMENTS(0,YR) + &
                               CATAWBA_CAPACITY_PAYMENTS(ASSET_CLASS,YR)
               CATAWBA_LEVEL_CAP_PAYMENTS(0,YR) = &
                              CATAWBA_LEVEL_CAP_PAYMENTS(0,YR) + &
                              CATAWBA_LEVEL_CAP_PAYMENTS(ASSET_CLASS,YR)
               CATAWBA_OTHER_NET_REVENUES(0,YR) = &
                              CATAWBA_OTHER_NET_REVENUES(0,YR) + &
                              CATAWBA_OTHER_NET_REVENUES(ASSET_CLASS,YR)
               CATAWBA_DEFERRED_TAXES_CR(0,YR) = &
                               CATAWBA_DEFERRED_TAXES_CR(0,YR) + &
                               CATAWBA_DEFERRED_TAXES_CR(ASSET_CLASS,YR)
               CATAWBA_DEFERRED_DEBITS(0,YR) = &
                                 CATAWBA_DEFERRED_DEBITS(0,YR) + &
                                 CATAWBA_DEFERRED_DEBITS(ASSET_CLASS,YR)
               CATAWBA_DEFERRED_TAXES_BAL_CR(0,YR) = &
                           CATAWBA_DEFERRED_TAXES_BAL_CR(0,YR) + &
                           CATAWBA_DEFERRED_TAXES_BAL_CR(ASSET_CLASS,YR)
            ENDDO
            IF(YR == 1) THEN
               DO ASSET_CLASS = 1, NUM_OF_ASSET_CLASSES
                  CATAWBA_DEFERRED_DEBITS(0,0) = &
                                  CATAWBA_DEFERRED_DEBITS(0,0) + &
                                  CATAWBA_DEFERRED_DEBITS(ASSET_CLASS,0)
                  CATAWBA_DEFERRED_TAXES_BAL_CR(0,0) = &
                            CATAWBA_DEFERRED_TAXES_BAL_CR(0,0) + &
                            CATAWBA_DEFERRED_TAXES_BAL_CR(ASSET_CLASS,0)
               ENDDO
            ENDIF
         ENDDO
!
         DEALLOCATE(ASSET_CLASS_LIST, &
                    ASSET_ALLOCATION_LIST)
         CALL CLOSE_CATAW2_ALLOCATOR_FILE
!
! SINCE THIS ROUTINE IS CALLED AFTER THE INIT FILE IS READ THE CATAWBA
! DEFERRED DEBIT AND DEFERRED TAX BALANCE MUST BE PASSED TO THE CONSOLIDATE
! BALANCE SHEET INFORMATION.
!
      RETURN
!

      ENTRY CATAWBA_BY_DEBITS_BALANCE(R_CLASS)

         CATAWBA_BY_DEBITS_BALANCE = 0.
         IF(R_CLASS<=MAX_ASSET_CLASS_NUM .AND. CATAWBA_FILE_EXISTS) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
               CATAWBA_BY_DEBITS_BALANCE = &
                                  CATAWBA_DEFERRED_DEBITS(ASSET_CLASS,0)
            ENDIF
         ENDIF
      RETURN

      ENTRY CATAWBA_BY_DEF_TAX_BALANCE(R_CLASS)

         CATAWBA_BY_DEF_TAX_BALANCE = 0.
         IF(R_CLASS<=MAX_ASSET_CLASS_NUM .AND. CATAWBA_FILE_EXISTS) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
               CATAWBA_BY_DEF_TAX_BALANCE = &
                            CATAWBA_DEFERRED_TAXES_BAL_CR(ASSET_CLASS,0)
            ENDIF
         ENDIF
      RETURN

      ENTRY CATAWBA_DEFERRED_TAXES(R_YR)

!
         S_YEAR = MIN(R_YR,AVAIL_DATA_YEARS)
         IF(CATAWBA_FILE_EXISTS) THEN
            CATAWBA_DEFERRED_TAXES = CATAWBA_DEFERRED_TAXES_CR(0,S_YEAR)
         ELSE
            CATAWBA_DEFERRED_TAXES = 0.
         ENDIF
      RETURN

      ENTRY CATAWBA_TOTAL_REVENUES(R_YR)

!
         S_YEAR = MIN(R_YR,AVAIL_DATA_YEARS)
         IF(CATAWBA_FILE_EXISTS) THEN
            CATAWBA_TOTAL_REVENUES = CATAWBA_REVENUES(0,S_YEAR)
         ELSE
            CATAWBA_TOTAL_REVENUES = 0.
         ENDIF
      RETURN

      ENTRY CATAWBA_TOTAL_EXPENSES(R_YR)

!
         S_YEAR = MIN(R_YR,AVAIL_DATA_YEARS)
         IF(CATAWBA_FILE_EXISTS) THEN
            CATAWBA_TOTAL_EXPENSES = CATAWBA_EXPENSES(0,S_YEAR) + &
                                  CATAWBA_CAPACITY_PAYMENTS(0,S_YEAR) - &
                                  CATAWBA_LEVEL_CAP_PAYMENTS(0,S_YEAR)
         ELSE
            CATAWBA_TOTAL_EXPENSES = 0.
         ENDIF
      RETURN

      ENTRY CATAWBA_TOTAL_CAP_PAYMENTS(R_YR)

!
         S_YEAR = MIN(R_YR,AVAIL_DATA_YEARS)
         IF(CATAWBA_FILE_EXISTS) THEN
            CATAWBA_TOTAL_CAP_PAYMENTS = &
                                     CATAWBA_CAPACITY_PAYMENTS(0,S_YEAR)
         ELSE
            CATAWBA_TOTAL_CAP_PAYMENTS = 0.
         ENDIF
      RETURN

      ENTRY CATAWBA_TOTAL_LEVEL_CAP_PAYMTS(R_YR)

!
         S_YEAR = MIN(R_YR,AVAIL_DATA_YEARS)
         IF(CATAWBA_FILE_EXISTS) THEN
            CATAWBA_TOTAL_LEVEL_CAP_PAYMTS = &
                                    CATAWBA_LEVEL_CAP_PAYMENTS(0,S_YEAR)
         ELSE
            CATAWBA_TOTAL_LEVEL_CAP_PAYMTS = 0.
         ENDIF
      RETURN

      ENTRY CATAWBA_TOTAL_OTHER_NET_REVS(R_YR)

!
         S_YEAR = MIN(R_YR,AVAIL_DATA_YEARS)
         IF(CATAWBA_FILE_EXISTS) THEN
            CATAWBA_TOTAL_OTHER_NET_REVS = &
                                    CATAWBA_OTHER_NET_REVENUES(0,S_YEAR)
         ELSE
            CATAWBA_TOTAL_OTHER_NET_REVS = 0.
         ENDIF
      RETURN

      ENTRY CATAWBA_INFO(R_YR,R_CLASS, &
                         R_CATAWBA_REVENUES, &
                         R_CATAWBA_EXPENSES, &
                         R_CATAWBA_CAPACITY_PAYMENTS, &
                         R_CATAWBA_LEVEL_CAP_PAYMENTS, &
                         R_CATAWBA_OTHER_NET_REVENUES, &
                         R_CATAWBA_DEFERRED_DEBITS, &
                         R_CATAWBA_DEFER_TAXES_BAL_CR, &
                         R_CATAWBA_DEFERRED_TAXES_CR)

!
         R_CATAWBA_REVENUES = 0.
         R_CATAWBA_EXPENSES = 0.
         R_CATAWBA_CAPACITY_PAYMENTS = 0.
         R_CATAWBA_LEVEL_CAP_PAYMENTS = 0.
         R_CATAWBA_OTHER_NET_REVENUES = 0.
         R_CATAWBA_DEFERRED_DEBITS = 0.
         R_CATAWBA_DEFER_TAXES_BAL_CR = 0.
         R_CATAWBA_DEFERRED_TAXES_CR = 0.
         CATAWBA_INFO = CATAWBA_FILE_EXISTS
!
         S_YEAR = MIN(R_YR,AVAIL_DATA_YEARS)
!
         IF(R_CLASS<=MAX_ASSET_CLASS_NUM .AND. CATAWBA_FILE_EXISTS) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
               R_CATAWBA_REVENUES = CATAWBA_REVENUES(ASSET_CLASS,S_YEAR)
               R_CATAWBA_EXPENSES = CATAWBA_EXPENSES(ASSET_CLASS,S_YEAR)
               R_CATAWBA_CAPACITY_PAYMENTS = &
                           CATAWBA_CAPACITY_PAYMENTS(ASSET_CLASS,S_YEAR)
               R_CATAWBA_LEVEL_CAP_PAYMENTS = &
                          CATAWBA_LEVEL_CAP_PAYMENTS(ASSET_CLASS,S_YEAR)
               R_CATAWBA_OTHER_NET_REVENUES = &
                          CATAWBA_OTHER_NET_REVENUES(ASSET_CLASS,S_YEAR)
               R_CATAWBA_DEFERRED_DEBITS = &
                             CATAWBA_DEFERRED_DEBITS(ASSET_CLASS,S_YEAR)
               R_CATAWBA_DEFER_TAXES_BAL_CR = &
                       CATAWBA_DEFERRED_TAXES_BAL_CR(ASSET_CLASS,S_YEAR)
               R_CATAWBA_DEFERRED_TAXES_CR = &
                           CATAWBA_DEFERRED_TAXES_CR(ASSET_CLASS,S_YEAR)
            ENDIF
         ENDIF
      RETURN
      END







! CALLED ANNUALLY AT THE BEGINNING OF PRO_COST.
! $ MILLIONS
!
      SUBROUTINE READ_CPL_DATA(R_YEAR) 

      use SpinDriftLib
      use prod_arrays_dimensions
      use logging
      USE SIZECOM

      CHARACTER (len=1) ::  DUMMY_TYPE
      LOGICAL (kind=1) ::    R_USE_COIN_PEAK,R_PA_SWITCH
      INTEGER (kind=2) ::  R_YEAR
      INTEGER (kind=2) ::  I
      INTEGER (kind=2) ::  CLASS_POINTER
      INTEGER (kind=2) ::  DELETE
      INTEGER (kind=2) ::  YR
      INTEGER (kind=2) ::  ASSET_CLASS
      INTEGER (kind=2) ::  ASSET_CLASS_ID
      INTEGER (kind=2) ::  ASSET_ALLOCATION_VECTOR
      INTEGER (kind=2) ::  I2TEMP
      INTEGER (kind=2) ::  SAVE_CPL_READ_VARIABLES
      INTEGER ::  IOS
!
      REAL (kind=4) ::       ASSET_ALLOCATOR
      REAL (kind=4) :: SOR_A_RATE,SOR_B_RATE,SOR_C_RATE,SOR_D_RATE, &
              SOR_E_RATE,EMC_MONTHLY_FACTOR(12),EMC_SEPA_CAPACITY, &
              EMC_RESOURCE_CAPACITY,SOR_A_RESOURCE,SOR_D_RESOURCE, &
              SOR_E_RESOURCE,PA_SEPA_CAPACITY,FIRM_PURCHASE_CAPACITY, &
              MAYO_BUYBACK_RATE,HARRIS_BUYBACK_RATE,PA_RESERVE_RATE, &
              PA_SUPPLEMENTAL_RATE,PA_TRANSMISSION_RATE, &
              HARRIS_RETAINED_CAPACITY,BURNSWICK1_RETAINED_CAPACITY, &
              BURNSWICK2_RETAINED_CAPACITY,MAYO_RETAINED_CAPACITY, &
              ROXBORO4_RETAINED_CAPACITY,EMC_TRANSMISSION_RATE, &
              R_SOR_A_RATE,R_SOR_B_RATE,R_SOR_C_RATE,R_SOR_D_RATE, &
              R_SOR_E_RATE,R_EMC_MONTHLY_FACTOR(12), &
              R_EMC_SEPA_CAPACITY,R_EMC_RESOURCE_CAPACITY, &
              R_SOR_A_RESOURCE,R_SOR_D_RESOURCE,R_SOR_E_RESOURCE, &
              R_FIRM_PURCHASE_CAPACITY,R_PA_SEPA_CAPACITY, &
              R_HARRIS_RETAINED_CAPACITY,R_BURNSWICK1_RETAINED_CAPACITY, &
              R_BURNSWICK2_RETAINED_CAPACITY,R_MAYO_RETAINED_CAPACITY, &
              R_ROXBORO4_RETAINED_CAPACITY,R_PA_RESERVE_RATE, &
              R_PA_SUPPLEMENTAL_RATE,R_PA_TRANSMISSION_RATE, &
              R_MAYO_BUYBACK_RATE,R_HARRIS_BUYBACK_RATE, &
              R_EMC_TRANSMISSION_RATE,R_TOTAL_RETAINED
      REAL (kind=4) :: PA_ANN_AVE_PROD,EMC_ANN_AVE_PROD, &
                       PA_MONTHLY_FACTOR(12)
      CHARACTER (len=1) ::  EMC_PEAK
      CHARACTER (len=1) ::  PA_ENERGY_PRICE_SWITCH
      CHARACTER (len=1) ::  EMC_ENERGY_PRICE_SWITCH
      CHARACTER (len=1) ::  PA_SWITCH
!
! DECLARATION FOR DBREAD COMMON BLOCK
      INTEGER (kind=2) ::  ASSET_CLASS_POINTER(:)
      INTEGER (kind=2) ::  MAX_ASSET_CLASS_NUM
      INTEGER (kind=2) ::  NUM_OF_ASSET_CLASSES
      ALLOCATABLE :: ASSET_CLASS_POINTER
      SAVE ASSET_CLASS_POINTER,MAX_ASSET_CLASS_NUM,NUM_OF_ASSET_CLASSES
      REAL (kind=4) ::  ASSET_CLASS_LIST(:),ASSET_ALLOCATION_LIST(:)
      ALLOCATABLE :: ASSET_CLASS_LIST,ASSET_ALLOCATION_LIST
      SAVE ASSET_CLASS, ASSET_ALLOCATION_VECTOR,SOR_A_RATE,SOR_B_RATE, &
            SOR_C_RATE,SOR_D_RATE,SOR_E_RATE,EMC_PEAK, &
            EMC_MONTHLY_FACTOR,EMC_SEPA_CAPACITY, &
            EMC_RESOURCE_CAPACITY,SOR_A_RESOURCE,SOR_D_RESOURCE, &
            SOR_E_RESOURCE,PA_SEPA_CAPACITY,FIRM_PURCHASE_CAPACITY, &
            MAYO_BUYBACK_RATE,HARRIS_BUYBACK_RATE,PA_RESERVE_RATE, &
            PA_SUPPLEMENTAL_RATE,PA_TRANSMISSION_RATE, &
            HARRIS_RETAINED_CAPACITY,BURNSWICK1_RETAINED_CAPACITY, &
            BURNSWICK2_RETAINED_CAPACITY,MAYO_RETAINED_CAPACITY, &
            ROXBORO4_RETAINED_CAPACITY,EMC_TRANSMISSION_RATE, &
            PA_ENERGY_PRICE_SWITCH,EMC_ENERGY_PRICE_SWITCH, &
            PA_SWITCH,PA_ANN_AVE_PROD,EMC_ANN_AVE_PROD,PA_MONTHLY_FACTOR
!
      INTEGER (kind=2) ::    R_NUM_OF_CLASSES,R_MAX_CLASS_NUM
!
! END OF DATA DECLARATIONS
!
         CALL RETURN_NUM_CPL_CLASSES(NUM_OF_ASSET_CLASSES, &
                                         MAX_ASSET_CLASS_NUM)
         IF(ALLOCATED(ASSET_CLASS_POINTER)) &
                                         DEALLOCATE(ASSET_CLASS_POINTER)
!
         IF(MAX_ASSET_CLASS_NUM > 0) THEN
            ALLOCATE(ASSET_CLASS_POINTER(MAX_ASSET_CLASS_NUM))
            CALL fill_CPL_POINTERS(ASSET_CLASS_POINTER)
         ENDIF
!
         CALL OPEN_CPL_ALLOCATOR_FILE
         ALLOCATE(ASSET_CLASS_LIST(AVAIL_DATA_YEARS), &
                  ASSET_ALLOCATION_LIST(AVAIL_DATA_YEARS))
         READ(10,REC=R_YEAR,IOSTAT=IOS) DELETE,YR,ASSET_CLASS, &
                  ASSET_ALLOCATION_VECTOR,SOR_A_RATE,SOR_B_RATE, &
                  SOR_C_RATE,SOR_D_RATE,SOR_E_RATE,EMC_PEAK, &
                  EMC_MONTHLY_FACTOR,EMC_SEPA_CAPACITY, &
                  EMC_RESOURCE_CAPACITY,SOR_A_RESOURCE, &
                  SOR_D_RESOURCE,SOR_E_RESOURCE, &
                  PA_SEPA_CAPACITY,FIRM_PURCHASE_CAPACITY, &
                  MAYO_BUYBACK_RATE,HARRIS_BUYBACK_RATE, &
                  PA_RESERVE_RATE,PA_SUPPLEMENTAL_RATE, &
                  PA_TRANSMISSION_RATE,HARRIS_RETAINED_CAPACITY, &
                  BURNSWICK1_RETAINED_CAPACITY, &
                  BURNSWICK2_RETAINED_CAPACITY, &
                  MAYO_RETAINED_CAPACITY,ROXBORO4_RETAINED_CAPACITY, &
                  EMC_TRANSMISSION_RATE,PA_ENERGY_PRICE_SWITCH, &
                  EMC_ENERGY_PRICE_SWITCH,PA_ANN_AVE_PROD, &
                  EMC_ANN_AVE_PROD,PA_MONTHLY_FACTOR,PA_SWITCH
!
         IF(ASSET_ALLOCATION_VECTOR /= 0) THEN
            WRITE(4,*) "CPL EMC / PA file currently does not"
            WRITE(4,*) "support a vector of asset classes"
            WRITE(4,*) "MIDAS will reset asset class to 1."
            ASSET_CLASS = 1
         ENDIF
         ASSET_CLASS_ID = ASSET_CLASS + 1
!
         I2TEMP = SAVE_CPL_READ_VARIABLES(ASSET_CLASS_ID, &
                                       EMC_ANN_AVE_PROD, &
                                       PA_ANN_AVE_PROD, &
                                       EMC_ENERGY_PRICE_SWITCH, &
                                       PA_ENERGY_PRICE_SWITCH)
!

         DEALLOCATE(ASSET_CLASS_LIST, &
                    ASSET_ALLOCATION_LIST)
         CALL CLOSE_CPL_ALLOCATOR_FILE
      RETURN

      ENTRY GET_PA_SWITCH(R_PA_SWITCH)

         R_PA_SWITCH = PA_SWITCH == 'T'
      RETURN

      ENTRY GET_EMC_RATES(R_SOR_A_RATE,R_SOR_B_RATE,R_SOR_C_RATE, &
                                       R_SOR_D_RATE,R_SOR_E_RATE)

         R_SOR_A_RATE = SOR_A_RATE
         R_SOR_B_RATE = SOR_B_RATE
         R_SOR_C_RATE = SOR_C_RATE
         R_SOR_D_RATE = SOR_D_RATE
         R_SOR_E_RATE = SOR_E_RATE
      RETURN

      ENTRY GET_EMC_PEAK_AND_LOAD_SHAPES(R_USE_COIN_PEAK, &
                                         R_EMC_MONTHLY_FACTOR)

         R_USE_COIN_PEAK = EMC_PEAK == 'C'
         DO I = 1, 12
            R_EMC_MONTHLY_FACTOR(I) = EMC_MONTHLY_FACTOR(I)
         ENDDO
      RETURN

      ENTRY GET_EMC_RESOURCES(R_EMC_SEPA_CAPACITY, &
                              R_EMC_RESOURCE_CAPACITY, &
                              R_SOR_A_RESOURCE, &
                              R_SOR_D_RESOURCE, &
                              R_SOR_E_RESOURCE)

         R_EMC_SEPA_CAPACITY = EMC_SEPA_CAPACITY
         R_EMC_RESOURCE_CAPACITY = EMC_RESOURCE_CAPACITY
         R_SOR_A_RESOURCE = SOR_A_RESOURCE
         R_SOR_D_RESOURCE = SOR_D_RESOURCE
         R_SOR_E_RESOURCE = SOR_E_RESOURCE
      RETURN

      ENTRY GET_PA_FIRM_N_SEPA(  R_FIRM_PURCHASE_CAPACITY, &
                                 R_PA_SEPA_CAPACITY)

         R_FIRM_PURCHASE_CAPACITY = FIRM_PURCHASE_CAPACITY
         R_PA_SEPA_CAPACITY = PA_SEPA_CAPACITY
      RETURN

      ENTRY GET_PA_SEPA_CAPACITY(R_PA_SEPA_CAPACITY)

         R_PA_SEPA_CAPACITY = PA_SEPA_CAPACITY
      RETURN

      ENTRY GET_EMC_SEPA_CAPACITY(R_EMC_SEPA_CAPACITY)

         R_EMC_SEPA_CAPACITY = EMC_SEPA_CAPACITY
      RETURN

      ENTRY GET_PA_RESOURCE_CAPACITY(R_FIRM_PURCHASE_CAPACITY)

         R_FIRM_PURCHASE_CAPACITY = FIRM_PURCHASE_CAPACITY
      RETURN

      ENTRY GET_UNIT_RETAINED( &
                                    R_BURNSWICK1_RETAINED_CAPACITY, &
                                    R_BURNSWICK2_RETAINED_CAPACITY, &
                                    R_HARRIS_RETAINED_CAPACITY, &
                                    R_ROXBORO4_RETAINED_CAPACITY, &
                                    R_MAYO_RETAINED_CAPACITY)

         R_BURNSWICK1_RETAINED_CAPACITY = BURNSWICK1_RETAINED_CAPACITY
         R_BURNSWICK2_RETAINED_CAPACITY = BURNSWICK2_RETAINED_CAPACITY
         R_HARRIS_RETAINED_CAPACITY = HARRIS_RETAINED_CAPACITY
         R_ROXBORO4_RETAINED_CAPACITY = ROXBORO4_RETAINED_CAPACITY
         R_MAYO_RETAINED_CAPACITY = MAYO_RETAINED_CAPACITY
      RETURN

      ENTRY GET_TOTAL_RETAINED(R_TOTAL_RETAINED)

         R_TOTAL_RETAINED          =   BURNSWICK1_RETAINED_CAPACITY + &
                                       BURNSWICK2_RETAINED_CAPACITY + &
                                       HARRIS_RETAINED_CAPACITY + &
                                       ROXBORO4_RETAINED_CAPACITY + &
                                       MAYO_RETAINED_CAPACITY
      RETURN

      ENTRY GET_PA_CAPACITY_RATES(     R_PA_RESERVE_RATE, &
                                       R_PA_SUPPLEMENTAL_RATE, &
                                       R_PA_TRANSMISSION_RATE)

         R_PA_RESERVE_RATE = PA_RESERVE_RATE
         R_PA_SUPPLEMENTAL_RATE = PA_SUPPLEMENTAL_RATE
         R_PA_TRANSMISSION_RATE = PA_TRANSMISSION_RATE
      RETURN

      ENTRY GET_PA_TRANSMISSION_RATE(R_PA_TRANSMISSION_RATE)

         R_PA_TRANSMISSION_RATE = PA_TRANSMISSION_RATE
      RETURN

      ENTRY GET_EMC_TRANSMISSION_RATE(R_EMC_TRANSMISSION_RATE)

         R_EMC_TRANSMISSION_RATE = EMC_TRANSMISSION_RATE
      RETURN

      ENTRY GET_BUYBACK_CAPACITY(   R_MAYO_BUYBACK_RATE, &
                                    R_HARRIS_BUYBACK_RATE)

         R_MAYO_BUYBACK_RATE = MAYO_BUYBACK_RATE
         R_HARRIS_BUYBACK_RATE = HARRIS_BUYBACK_RATE
      RETURN
!
      END

      FUNCTION SAVE_CPL_READ_VARIABLES(CLASS_FROM_READ_CPL, &
                                       R_EMC_ANN_AVE_PROD, &
                                       R_PA_ANN_AVE_PROD, &
                                       R_EMC_ENERGY_PRICE_SWITCH, &
                                       R_PA_ENERGY_PRICE_SWITCH)

      use logging
      LOGICAL (kind=1) ::    USE_EMC_ENERGY_PRICE,USE_PA_ENERGY_PRICE
      CHARACTER (len=1) :: &
                  EMC_ENERGY_PRICE_SWITCH_SAVE=' ' , &
                  PA_ENERGY_PRICE_SWITCH_SAVE=' ' , &
                  R_EMC_ENERGY_PRICE_SWITCH, &
                  R_PA_ENERGY_PRICE_SWITCH
      INTEGER (kind=2) ::  EMC_PA_CLASS
      INTEGER (kind=2) ::  ANNUAL_EMC_PA_CLASS=0
      INTEGER (kind=2) ::  CLASS_FROM_READ_CPL
      INTEGER (kind=2) ::  SAVE_CPL_READ_VARIABLES
      REAL (kind=4) ::  EMC_ANN_AVE_PROD
      REAL (kind=4) ::  PA_ANN_AVE_PROD
      REAL (kind=4) ::  R_EMC_ANN_AVE_PROD
      REAL (kind=4) ::  R_PA_ANN_AVE_PROD
      REAL (kind=4) ::  EMC_ANN_AVE_PROD_SAVE=0.
      REAL (kind=4) ::  PA_ANN_AVE_PROD_SAVE=0.

         EMC_ANN_AVE_PROD_SAVE = R_EMC_ANN_AVE_PROD
         PA_ANN_AVE_PROD_SAVE = R_PA_ANN_AVE_PROD
         EMC_ENERGY_PRICE_SWITCH_SAVE = R_EMC_ENERGY_PRICE_SWITCH
         PA_ENERGY_PRICE_SWITCH_SAVE = R_PA_ENERGY_PRICE_SWITCH
         ANNUAL_EMC_PA_CLASS = CLASS_FROM_READ_CPL
         SAVE_CPL_READ_VARIABLES = ANNUAL_EMC_PA_CLASS
      RETURN
!
      ENTRY EMC_PA_CLASS()
         EMC_PA_CLASS = ANNUAL_EMC_PA_CLASS
      RETURN
!
      ENTRY EMC_ANN_AVE_PROD()
         EMC_ANN_AVE_PROD = EMC_ANN_AVE_PROD_SAVE
      RETURN
!
      ENTRY PA_ANN_AVE_PROD()
         PA_ANN_AVE_PROD = PA_ANN_AVE_PROD_SAVE
      RETURN
!
      ENTRY USE_EMC_ENERGY_PRICE()
         USE_EMC_ENERGY_PRICE = EMC_ENERGY_PRICE_SWITCH_SAVE == 'I'
      RETURN
!
      ENTRY USE_PA_ENERGY_PRICE()
         USE_PA_ENERGY_PRICE = PA_ENERGY_PRICE_SWITCH_SAVE == 'I'
      RETURN
!
      END
!
!
!

!
! CALLED ANNUALLY AT THE END OF PRO_COST.
! $ MILLIONS
!
      SUBROUTINE GET_CPL_EMC_CAP_REVS ! PER SKIP SEEKAMP MEMO: 11/11/96.
!

      use mod_base_year
      use SpinDriftLib
      use prod_arrays_dimensions
      use irec_endpoint_control
      use logging
      use grx_planning_routines
      use sizecom
      use globecom
      use lamcom
      use endpoint
      use mwunih
      use production
      use nunits_shared
	  use cls_load

!
      INTEGER (kind=2) ::  CURRENT_YEAR
      INTEGER (kind=2) ::  CURRENT_MONTH
      INTEGER (kind=2) ::  EMC_CLASS
      INTEGER (kind=2) ::  R_MONTH
      INTEGER (kind=2) ::  R_CURRENT_YEAR
      PARAMETER (EMC_CLASS=5) ! (OTHER 2) IN THE DATA BASE
      REAL (kind=4) ::    EMC_ENERGY_REVENUE
      REAL (kind=4) ::  ANN_EMC_ENERGY_REVENUE=0.
      REAL (kind=4) ::  R_EMC_RATE
      REAL (kind=4) ::  ANN_EMC_ENERGY_SALES=0.
      REAL (kind=4) ::  EMC_SALES
      REAL (kind=4) ::  NON_COIN_PEAK
      REAL (kind=4) ::  ANN_NON_COIN_PEAK=0.
      REAL (kind=4) ::  R_ANN_EMC_ENERGY_SALES
      REAL (kind=4) ::  R_ANN_NON_COIN_PEAK
      REAL (kind=4) ::  R_ANN_EMC_ENERGY_REVENUE
      REAL (kind=4) ::  R_ANN_EMC_CAP_REVENUE
      REAL (kind=4) ::  R_ANN_EMC_TRAN_REVENUE
      REAL (kind=4) ::  ANN_SUM_NON_COIN_PEAK=0.
      REAL (kind=4) ::  EMC_TRANS_RATE
      REAL (kind=4) ::  R_ANN_SUM_TRAN_PEAK
      REAL (kind=4) :: &
         EMC_RESOURCE_CAP=1012. , & !  CALC
         EMC_EXCESS_OR_DEFICIT, & !  CALC
         NCEMC_COIN_PEAK, & !  AREA FORECASTS
         NCEMC_NON_COIN_PEAK, & !  AREA FORECASTS
         CPS_N_NCPS, & !  CALC
         CP_DIVIDED_BY_NCP(12), & ! DATA
!
         SEPA_ALLOCATION=44.885 , & !  PER SEEKAMP FAX.
         NCEMC_SUPP_BILLING_CPS ! CALC
! SERVICE OBLIGATION RESOURCES
      REAL (kind=4) :: SOR_A_RESOURCE, & !  PER CONTRACT
         SOR_B_RESOURCE, & !  PER CONTRACT
         SOR_C_RESOURCE, & !  PER CONTRACT
         SOR_D_RESOURCE, & !  PER CONTRACT
         SOR_E_RESOURCE, & !  PER CONTRACT
         TOTAL_SERVED_BY_CPL, & !  CALC
         LOAD_POSS_SERVE_BY_OTHERS, & ! CALC
!
         ANN_CPS_N_NCPS=0. , & !  ACCUMULATED MONTHLY
         ANN_NCEMC_SUPP_BILLING_CPS=0. , & !  ACCUMULATED MONTHLY
         ANN_SOR_A_RESOURCE=0. , & !  ACCUMULATED MONTHLY
         ANN_SOR_B_RESOURCE=0. , & !  ACCUMULATED MONTHLY
         ANN_SOR_C_RESOURCE=0. , & !  ACCUMULATED MONTHLY
         ANN_SOR_D_RESOURCE=0. , & !  ACCUMULATED MONTHLY
         ANN_SOR_E_RESOURCE=0. , & !  ACCUMULATED MONTHLY
         ANN_SEPA_ALLOCATION=0. , & !  ???
         ANN_TOTAL_SERVED_BY_CPL=0. , & !  ACCUMULATED MONTHLY
         ANN_LOAD_POSS_SERVE_BY_OTHERS=0.  ! ACCUMULATED MONTHLY
! SERVICE OBLIGATION RESOURCE RATES
      REAL (kind=4) :: SOR_A_RATE=0. , & !  PER CONTRACT
         SOR_B_RATE=0. , & !  PER CONTRACT
         SOR_C_RATE=0. , & !  PER CONTRACT
         SOR_D_RATE=0. , & !  PER CONTRACT
         SOR_E_RATE=0.  ! PER CONTRACT
! SERVICE OBLIGATION RESOURCE REVENUES
      REAL (kind=4) :: SOR_A_REVENUE, & !  CALC
         SOR_B_REVENUE, & !  CALC
         SOR_C_REVENUE, & !  CALC
         SOR_D_REVENUE, & !  CALC
         SOR_E_REVENUE, & !  CALC
         TOTAL_EMC_CAP_REVENUE, & ! CALC
 !
         ANN_SOR_A_REVENUE=0. , & !  ACCUMULATED MONTHLY
         ANN_SOR_B_REVENUE=0. , & !  ACCUMULATED MONTHLY
         ANN_SOR_C_REVENUE=0. , & !  ACCUMULATED MONTHLY
         ANN_SOR_D_REVENUE=0. , & !  ACCUMULATED MONTHLY
         ANN_SOR_E_REVENUE=0. , & !  ACCUMULATED MONTHLY
         ANN_TOTAL_EMC_CAP_REVENUE=0.  ! CALC
      DATA CP_DIVIDED_BY_NCP &
                  /0.967327,0.933236,0.891418,0.91152,0.890577,0.882076, &
                  0.931185,0.947181,0.936194,0.881214,0.888394,0.899382/
!
! DETAILED REPORT OVERHEAD

      LOGICAL (kind=1) ::    ECAP_REPORT_NOT_OPEN=.TRUE.
      LOGICAL (kind=1) ::  ECAP_REPORT_ACTIVE
      LOGICAL (kind=1) ::  USE_COIN_PEAK
      INTEGER (kind=2) ::  LAST_SEASON
      INTEGER (kind=2) ::  CPL_EMC_CAP_REVS_RPT_HEADER
      INTEGER (kind=2) ::  CPL_ECAP_UNIT
      INTEGER ::  CPL_ECAP_REC
      SAVE CPL_ECAP_REC
      CHARACTER (len=9) ::  CL_MONTH_NAME(13),MONTH_NAME*20
      SAVE CL_MONTH_NAME,CPL_ECAP_UNIT
!
! END DATA DECLARATIONS
!
      CALL GET_EMC_RATES(SOR_A_RATE,SOR_B_RATE,SOR_C_RATE, &
                                    SOR_D_RATE,SOR_E_RATE)
      CALL GET_EMC_PEAK_AND_LOAD_SHAPES(USE_COIN_PEAK, &
                                         CP_DIVIDED_BY_NCP)
      CALL GET_EMC_RESOURCES( SEPA_ALLOCATION, &
                              EMC_RESOURCE_CAP, &
                              SOR_A_RESOURCE, &
                              SOR_D_RESOURCE, &
                              SOR_E_RESOURCE)
!
      ECAP_REPORT_ACTIVE = .TRUE. ! UNTIL ADDED TO DETAILED REPORTS
      IF(ECAP_REPORT_NOT_OPEN .AND. &
                             ECAP_REPORT_ACTIVE .AND. &
                                               .NOT. TESTING_PLAN ) THEN
         ECAP_REPORT_NOT_OPEN = .FALSE.
         CPL_ECAP_UNIT = CPL_EMC_CAP_REVS_RPT_HEADER(CPL_ECAP_REC)
         LAST_SEASON = get_PRODUCTION_PERIODS_in()
         DO CURRENT_MONTH = 1, LAST_SEASON
            CL_MONTH_NAME(CURRENT_MONTH) = MONTH_NAME(CURRENT_MONTH)
         ENDDO
         CL_MONTH_NAME(LAST_SEASON+1) = 'Annual'
      ENDIF

      CURRENT_YEAR = get_BASE_YEAR() + globecom_year
      DO CURRENT_MONTH = 1, 12

         NCEMC_NON_COIN_PEAK = &
                MAX(FORECAST_COINCIDENT_PEAK(1,CURRENT_MONTH, EMC_CLASS), &
                    FORECAST_COINCIDENT_PEAK(2,CURRENT_MONTH, EMC_CLASS))

         NCEMC_COIN_PEAK = &
                              NCEMC_NON_COIN_PEAK * &
                              CP_DIVIDED_BY_NCP(CURRENT_MONTH)
         IF(USE_COIN_PEAK) THEN
            CPS_N_NCPS =  NCEMC_COIN_PEAK
         ELSE
            CPS_N_NCPS = NCEMC_NON_COIN_PEAK
         ENDIF

!
         NCEMC_SUPP_BILLING_CPS = &
                              CPS_N_NCPS - &
                              SEPA_ALLOCATION
!
         EMC_EXCESS_OR_DEFICIT = &
                              EMC_RESOURCE_CAP - &
                              NCEMC_SUPP_BILLING_CPS

         IF(SOR_B_RATE == 0.) THEN
               SOR_B_RESOURCE = 0.
         ELSEIF(CURRENT_YEAR >= 1996 .AND. CURRENT_YEAR < 2100) THEN  ! 2004
            IF(EMC_EXCESS_OR_DEFICIT > 0.) THEN
               SOR_B_RESOURCE = &
                              NCEMC_SUPP_BILLING_CPS - &
                              SOR_A_RESOURCE - &
                              SOR_D_RESOURCE - &
                              SOR_E_RESOURCE
            ELSE
               SOR_B_RESOURCE = &
                              EMC_RESOURCE_CAP - &
                              SOR_A_RESOURCE - &
                              SOR_D_RESOURCE - &
                              SOR_E_RESOURCE
            ENDIF
         ELSE
            SOR_B_RESOURCE = 0.
         ENDIF
!
         IF(SOR_C_RATE == 0.) THEN
            SOR_C_RESOURCE = 0.
         ELSEIF(CURRENT_YEAR >= 1996 .AND. CURRENT_YEAR < 2100) THEN    !2001
            SOR_C_RESOURCE = -1* MIN(0.,EMC_EXCESS_OR_DEFICIT) ! C GETS DEFICIT
         ELSE
            SOR_C_RESOURCE = 0.
         ENDIF
!
         TOTAL_SERVED_BY_CPL = &
                              SOR_A_RESOURCE + &
                              SOR_B_RESOURCE + &
                              SOR_C_RESOURCE + &
                              SOR_D_RESOURCE + &
                              SOR_E_RESOURCE
         LOAD_POSS_SERVE_BY_OTHERS = &
                              MAX(0.,NCEMC_SUPP_BILLING_CPS - &
                              TOTAL_SERVED_BY_CPL)
!
! SERVICE OBLIGATION RESOURCE REVENUES
!
         SOR_A_REVENUE = &
                              SOR_A_RESOURCE * &
                              SOR_A_RATE/1000.
         SOR_B_REVENUE = &
                              SOR_B_RESOURCE * &
                              SOR_B_RATE/1000.
         SOR_C_REVENUE = &
                              SOR_C_RESOURCE * &
                              SOR_C_RATE/1000.
         SOR_D_REVENUE = &
                              SOR_D_RESOURCE * &
                              SOR_D_RATE/1000.
         SOR_E_REVENUE = &
                              SOR_E_RESOURCE * &
                              SOR_E_RATE/1000.
         TOTAL_EMC_CAP_REVENUE = &
                              SOR_A_REVENUE + &
                              SOR_B_REVENUE + &
                              SOR_C_REVENUE + &
                              SOR_D_REVENUE + &
                              SOR_E_REVENUE
!
! ACCUMULATE ANNUAL VALUES
!
         ANN_CPS_N_NCPS = &
                              ANN_CPS_N_NCPS + &
                              CPS_N_NCPS
         ANN_NCEMC_SUPP_BILLING_CPS = &
                              ANN_NCEMC_SUPP_BILLING_CPS + &
                              NCEMC_SUPP_BILLING_CPS
         ANN_SOR_A_RESOURCE = ANN_SOR_A_RESOURCE + &
                              SOR_A_RESOURCE
         ANN_SOR_B_RESOURCE = ANN_SOR_B_RESOURCE + &
                              SOR_B_RESOURCE
         ANN_SOR_C_RESOURCE = ANN_SOR_C_RESOURCE + &
                              SOR_C_RESOURCE
         ANN_SOR_D_RESOURCE = ANN_SOR_D_RESOURCE + &
                              SOR_D_RESOURCE
         ANN_SOR_E_RESOURCE = ANN_SOR_E_RESOURCE + &
                              SOR_E_RESOURCE
         ANN_LOAD_POSS_SERVE_BY_OTHERS = ANN_LOAD_POSS_SERVE_BY_OTHERS + &
                              LOAD_POSS_SERVE_BY_OTHERS
         ANN_SEPA_ALLOCATION = &
                              ANN_SEPA_ALLOCATION + &
                              SEPA_ALLOCATION
         ANN_SOR_A_REVENUE =  ANN_SOR_A_REVENUE + &
                              SOR_A_REVENUE
         ANN_SOR_B_REVENUE =  ANN_SOR_B_REVENUE + &
                              SOR_B_REVENUE
         ANN_SOR_C_REVENUE =  ANN_SOR_C_REVENUE + &
                              SOR_C_REVENUE
         ANN_SOR_D_REVENUE =  ANN_SOR_D_REVENUE + &
                              SOR_D_REVENUE
         ANN_SOR_E_REVENUE =  ANN_SOR_E_REVENUE + &
                              SOR_E_REVENUE
         ANN_TOTAL_EMC_CAP_REVENUE = &
                              ANN_TOTAL_EMC_CAP_REVENUE + &
                              TOTAL_EMC_CAP_REVENUE
!
!
! MONTHLY WRITE
!
         WRITE(CPL_ECAP_UNIT,REC=CPL_ECAP_REC) &
                     PRT_ENDPOINT(),FLOAT(CURRENT_YEAR), &
                     CL_MONTH_NAME(CURRENT_MONTH),CPS_N_NCPS, &
                     SEPA_ALLOCATION,NCEMC_SUPP_BILLING_CPS, &
                     SOR_A_RESOURCE,SOR_B_RESOURCE,SOR_C_RESOURCE, &
                     SOR_D_RESOURCE,SOR_E_RESOURCE,TOTAL_SERVED_BY_CPL, &
                     LOAD_POSS_SERVE_BY_OTHERS,SOR_A_RATE, &
                     SOR_B_RATE,SOR_C_RATE,SOR_D_RATE,SOR_E_RATE, &
                     SOR_A_REVENUE,SOR_B_REVENUE,SOR_C_REVENUE, &
                     SOR_D_REVENUE,SOR_E_REVENUE,TOTAL_EMC_CAP_REVENUE
         CPL_ECAP_REC = CPL_ECAP_REC + 1
!
      ENDDO ! CURRENT_MONTH
!
! ANNUAL WRITE
!
      WRITE(CPL_ECAP_UNIT,REC=CPL_ECAP_REC) &
                     PRT_ENDPOINT(),FLOAT(CURRENT_YEAR), &
                     CL_MONTH_NAME(LAST_SEASON+1),ANN_CPS_N_NCPS, &
                     ANN_SEPA_ALLOCATION,ANN_NCEMC_SUPP_BILLING_CPS, &
                     ANN_SOR_A_RESOURCE,ANN_SOR_B_RESOURCE, &
                     ANN_SOR_C_RESOURCE,ANN_SOR_D_RESOURCE, &
                     ANN_SOR_E_RESOURCE,ANN_TOTAL_SERVED_BY_CPL, &
                     ANN_LOAD_POSS_SERVE_BY_OTHERS, &
                     SOR_A_RATE, &
                     SOR_B_RATE, &
                     SOR_C_RATE, &
                     SOR_D_RATE, &
                     SOR_E_RATE, &
                     ANN_SOR_A_REVENUE, &
                     ANN_SOR_B_REVENUE, &
                     ANN_SOR_C_REVENUE, &
                     ANN_SOR_D_REVENUE, &
                     ANN_SOR_E_REVENUE, &
                     ANN_TOTAL_EMC_CAP_REVENUE
      CPL_ECAP_REC = CPL_ECAP_REC + 1

      RETURN ! CALLED ANNUALLY


      ENTRY INIT_CPL_EMC_ANN_CAP


         ANN_NCEMC_SUPP_BILLING_CPS = 0.
         ANN_CPS_N_NCPS = 0.
         ANN_SOR_A_RESOURCE = 0.
         ANN_SOR_B_RESOURCE = 0.
         ANN_SOR_C_RESOURCE = 0.
         ANN_SOR_D_RESOURCE = 0.
         ANN_SOR_E_RESOURCE = 0.
         ANN_TOTAL_SERVED_BY_CPL = 0.
         ANN_LOAD_POSS_SERVE_BY_OTHERS = 0.
         ANN_SEPA_ALLOCATION = 0.
         ANN_SOR_A_REVENUE = 0.
         ANN_SOR_B_REVENUE = 0.
         ANN_SOR_C_REVENUE = 0.
         ANN_SOR_D_REVENUE = 0.
         ANN_SOR_E_REVENUE = 0.
         ANN_TOTAL_EMC_CAP_REVENUE = 0.

         ANN_EMC_ENERGY_REVENUE = 0.
         ANN_EMC_ENERGY_SALES = 0.
         ANN_NON_COIN_PEAK = 0.
         ANN_SUM_NON_COIN_PEAK = 0.

      RETURN

      ENTRY GET_CPL_EMC_MON_ENER_REVS(R_MONTH,R_EMC_RATE, &
               EMC_SALES,NON_COIN_PEAK,EMC_ENERGY_REVENUE)

         EMC_SALES =          FORECAST_ENERGY(1,R_MONTH,EMC_CLASS) + &
                              FORECAST_ENERGY(2,R_MONTH,EMC_CLASS)
         NON_COIN_PEAK = &
                MAX(FORECAST_COINCIDENT_PEAK(1,R_MONTH,EMC_CLASS), &
                    FORECAST_COINCIDENT_PEAK(2,R_MONTH,EMC_CLASS))
         ANN_NON_COIN_PEAK = MAX(ANN_NON_COIN_PEAK,NON_COIN_PEAK)
         ANN_EMC_ENERGY_SALES =     ANN_EMC_ENERGY_SALES + &
                                    EMC_SALES
         EMC_ENERGY_REVENUE = EMC_SALES * R_EMC_RATE
         ANN_EMC_ENERGY_REVENUE =   ANN_EMC_ENERGY_REVENUE + &
                                    EMC_ENERGY_REVENUE
!
         ANN_SUM_NON_COIN_PEAK = ANN_SUM_NON_COIN_PEAK + &
                                                       ANN_NON_COIN_PEAK
!
      RETURN
!
!

      ENTRY GET_CPL_EMC_ANN_CAP_REVS( R_ANN_EMC_ENERGY_SALES, &
                                       R_ANN_NON_COIN_PEAK, &
                                       R_ANN_EMC_CAP_REVENUE)

         R_ANN_EMC_ENERGY_SALES = 0.
         R_ANN_NON_COIN_PEAK = ANN_NON_COIN_PEAK
         R_ANN_EMC_CAP_REVENUE  =  1000000.*ANN_TOTAL_EMC_CAP_REVENUE

      RETURN

      ENTRY GET_CPL_EMC_ANN_ENER_REVS( R_ANN_EMC_ENERGY_SALES, &
                                       R_ANN_NON_COIN_PEAK, &
                                       R_ANN_EMC_ENERGY_REVENUE)

         R_ANN_EMC_ENERGY_SALES = ANN_EMC_ENERGY_SALES
         R_ANN_NON_COIN_PEAK = ANN_NON_COIN_PEAK
         R_ANN_EMC_ENERGY_REVENUE  =  ANN_EMC_ENERGY_REVENUE
!
      RETURN
!
!

      ENTRY GET_CPL_EMC_ANN_TRAN_REVS( R_ANN_SUM_TRAN_PEAK, &
                                       R_ANN_EMC_TRAN_REVENUE, &
                                       R_CURRENT_YEAR)

         CALL GET_EMC_TRANSMISSION_RATE(EMC_TRANS_RATE)
         R_ANN_SUM_TRAN_PEAK = ANN_SUM_NON_COIN_PEAK - 539.
         R_ANN_EMC_TRAN_REVENUE =   1000. * &
                                    R_ANN_SUM_TRAN_PEAK * EMC_TRANS_RATE
      RETURN
!
!
!
      END
      FUNCTION CPL_ACTIVE()
      use logging
      LOGICAL (kind=1) ::  SAVE_CPL_STATUS=.FALSE.
      LOGICAL (kind=1) ::  CPL_ACTIVE
      LOGICAL (kind=1) ::  CHANGE_CPL_STATUS_TO_ACTIVE
!
         CPL_ACTIVE = SAVE_CPL_STATUS
      RETURN
      ENTRY CHANGE_CPL_STATUS_TO_ACTIVE()
         SAVE_CPL_STATUS = .TRUE.
         CHANGE_CPL_STATUS_TO_ACTIVE = SAVE_CPL_STATUS
      RETURN
      END
!
!

!
      RECURSIVE SUBROUTINE CPL_PA_DISPATCH(LPROB2,SYSTEM_LODDUR, &
                                 HOURS_INCREMENT, &
                                 LAST_POINT,MAINTENANCE_RATE, &
                                 SYSTEM_DEMAND,TEMPEA,SYSTEM_PEAK,ISEAS, &
                                 BLOCK_FUEL_COST,SYSTEM_ENERGY, &
                                 SYS_EFFECTIVE_CAPACITY, &
                                 PERIOD_CPL_GEN_COST_FOSSIL, &
                                 PERIOD_CPL_GENERATION_COST_NUC, &
                                 PERIOD_CPL_PURCHASE_POWER_COST)
      
      use forecast
      use end_routine
      use interg82
      use logging
      use capacity_arrays
      use grx_planning_routines
      use shared_vars_interg82
      use prod_arrays_dimensions
      use prod2com
      use production
      use prodcom
      use poolcom
      use cla_decs
      use mwunih
      use endpoint
      use lamcom
      use nunits_shared
      use spindriftlib
      use irec_endpoint_control
      use sizecom
      use foshydi2com
	  use cls_load
      use energy_trace
      use sic_trace
      
      save

!
!     NO_PA_ACCTS = 7: 1 = Mayo 1 Replacement
!                      2 = Harris 1 Replacement
!                      3 = Total Replacement
!                      4 = Surplus
!                      5 = Harris 1 Buyback
!                      6 = Mayo 1 Buyback
!                      7 = Total Buyback

      
!
      LOGICAL (kind=1) ::  CL_UNITS_ACTIVE
      LOGICAL (kind=1) ::  USE_PA_ENERGY_PRICE,USE_EMC_ENERGY_PRICE
      INTEGER (kind=2) ::  NO_PA_UNITS=0
      INTEGER (kind=2) ::  I
      INTEGER (kind=2) ::  J
      INTEGER (kind=2) ::  HOURS_INCREMENT
      INTEGER (kind=2) ::  UNITNO
      INTEGER (kind=2) ::  LAST_POINT
      INTEGER (kind=2) ::  PA_UNIT_INDEX(:)
      INTEGER (kind=2) ::  NBLOK2
      INTEGER (kind=2) ::  BLKNUM
      INTEGER (kind=2) ::  CURRENT
      INTEGER (kind=2) ::  PAUNITNO
      INTEGER (kind=2) ::  ISEAS
      INTEGER (kind=2) ::  ISTART
      INTEGER (kind=2) ::  NO_PA_RESOURCES
      INTEGER (kind=2) ::  NO_PA_CONTRACTS=5
      INTEGER (kind=2) ::  CURRENT_CPL_CONTRACT
      INTEGER (kind=2) ::  NO_PA_ACCTS=7
      INTEGER (kind=2) ::  OFF_SYSTEM_SALES_CLASS
      INTEGER (kind=2) ::  R_ISEAS
      INTEGER (kind=2) ::  ReserveCapacity
      INTEGER (kind=2) ::  UnusedSupplemental
      INTEGER (kind=2) ::  SupplementalCap
      INTEGER (kind=2) ::  PAResource
      INTEGER (kind=2) ::  DeficiencyCapacity
      INTEGER (kind=2) ::  Mayo1Replacement
      INTEGER (kind=2) ::  Roxboro4Replacement
      INTEGER (kind=2) ::  ReplacementEnergy
      INTEGER (kind=2) ::  SurplusEnergy
      INTEGER (kind=2) ::  HarrisBuybackEnerg
      INTEGER (kind=2) ::  Mayo1BuybackEnergy
      INTEGER (kind=2) ::  BuybackEnergy
      PARAMETER (OFF_SYSTEM_SALES_CLASS=2, & !  Residental is 2 was 1
                  ReserveCapacity = 6, &
                  UnusedSupplemental = 7, &
                  SupplementalCap = 8, &
                  PAResource = 9, &
                  DeficiencyCapacity = 10, &
                  Mayo1Replacement = 6, &
                  Roxboro4Replacement = 7, &
                  ReplacementEnergy = 8, &
                  SurplusEnergy = 9, &
                  HarrisBuybackEnerg = 10, &
                  Mayo1BuybackEnergy = 11, &
                  BuybackEnergy = 12)
      REAL (kind=4) ::  LPROB2(CONVOLUTION_POINTS)
      REAL (kind=4) ::  LPROB(CONVOLUTION_POINTS,2)
      REAL (kind=4) ::  SYSTEM_LODDUR(CONVOLUTION_POINTS)
      REAL (kind=4) ::  PA_DX
      REAL (kind=4) ::  PA_LODDUR(CONVOLUTION_POINTS)
      REAL (kind=4) ::  LEFT(:)
      REAL (kind=4) ::  RIGHT(:)
      REAL (kind=4) ::  EA
      REAL (kind=4) ::  A
      REAL (kind=4) ::  B
      REAL (kind=4) ::  ENRG
      REAL (kind=4) ::  PA_BASE
      REAL (kind=4) ::  PA_PEAK
      REAL (kind=4) ::  SYSTEM_PEAK
      REAL (kind=4) ::  LIMR
      REAL (kind=4) ::  PA_CAPSYS
      REAL (kind=4) ::  SEAS_HOURS=0
      REAL (kind=4) ::  REMAINING_ENERGY
      REAL (kind=4) ::  CAPON
      REAL (kind=4) ::  CAPBLK
      REAL (kind=4) ::  PA_ENERGY(:,:)
      REAL (kind=4) ::  EFFECTIVE_CAPACITY(:,:)
      REAL (kind=4) ::  CUMCAP
      REAL (kind=4) ::  BLK_MW
      REAL (kind=4) ::  MAINTENANCE_RATE(MAX_CL_UNITS)
      REAL (kind=4) ::  UNIT_ENRG
      REAL (kind=4) ::  TEMPEA(2,MAX_CL_UNITS)
      REAL (kind=4) ::  RESERVE_CAPACITY
      REAL (kind=4) ::  RESERVE_PERCENT
      REAL (kind=4) ::  UNUSED_SUPP_CAPACITY
      REAL (kind=4) ::  SUPPLEMENTAL_CAPACITY
      REAL (kind=4) ::  PA_EXP_CAPSYS
      REAL (kind=4) ::  PA_CONTRACT_CAPACITY
      REAL (kind=4) ::  PA_RESOURCE_ENERGY_COST
      REAL (kind=4) ::  SEPA_ENERGY(12)
      REAL (kind=4) ::  SEPA_ENERGY_PER_ECAP(12)
      REAL (kind=4) ::  OFF_SYSTEM_SALES_ENERGY(0:12)
      REAL (kind=4) ::  R_CPL_GEN_COST
      REAL (kind=4) ::  R_CPL_PA_ENTITLE
      REAL (kind=4) ::  R_CPL_PUR_POWER
      REAL (kind=4) ::  R_CPL_OFF_SYS_INC_COST
      REAL (kind=4) ::  R_CPL_PA_MWH_COST
      REAL (kind=4) ::  CPL_FUEL_GENERATION(0:12)
      REAL (kind=4) ::  CPL_NUC_GENERATION(0:12)
      REAL (kind=4) ::  CPL_PUR_POWER_ENERGY_COST(0:12)
      REAL (kind=4) ::  CPL_NCEMPA_ENTITLEMENT(0:12)
      REAL (kind=4) ::  CPL_NCEMPA_ENTITLEMENT_MWH(0:12)
      REAL (kind=4) ::  CPL_NCEMPA_MWH_SUPPLIED_BY_CPL(0:12)
      REAL (kind=4) ::  CPL_NCEMPA_COST_MWH_SUP_BY_CPL(0:12)
      REAL (kind=4) ::  CPL_OFF_SYSTEM_INCR_COST(0:12)
      REAL (kind=8) ::  PERIOD_CPL_GEN_COST_FOSSIL
      REAL (kind=8) ::  PERIOD_CPL_PURCHASE_POWER_COST
      REAL (kind=4) ::    PERIOD_CPL_GENERATION_COST_NUC
      CHARACTER (len=20) ::  CONTRACT_NAMES(5),PA_ACCT_NAMES(7)
      DATA CONTRACT_NAMES/ 'Reserve Capacity    ', &
                           'Unused Supplemental ', &
                           'Supplemental Cap    ', &
                           'PA Resource         ', &
                           'Deficiency Capacity '/, &
           PA_ACCT_NAMES/ &
                     'Mayo 1 Replacement  ', &
                    'Roxboro4 Replacement', &
                     'Replacement Energy  ', &
                     'Surplus Energy      ', &
                     'Harris Buyback Energ', &
                    'Mayo1 Buyback Energy', &
                     'Buyback Energy      '/, &
            SEPA_ENERGY/18798,17789,18606,18257,17805,14226, &
                        13128,12690,11392,12757,12327,14104/, &
            SEPA_ENERGY_PER_ECAP/2616,1712,1862,1807, &
                                 1820,1700,2503,2505, &
                                 2425,1751,1689,2519/
      REAL (kind=8) ::  SYSTEM_DEMAND,PA_DEMAND,MMBTUS
      ALLOCATABLE :: PA_UNIT_INDEX,PA_ENERGY,EFFECTIVE_CAPACITY, &
                     LEFT,RIGHT
!
! ENERGY RECONCILE CALCULATIONS
!
      REAL (kind=4) ::  SYSTEM_ENERGY(2,MAX_CL_UNITS)
      REAL (kind=4) ::  SYS_EFFECTIVE_CAPACITY(2,MAX_CL_UNITS)
      REAL (kind=4) ::  SURPLUS_ENERGY
      REAL (kind=4) ::  REPLACEMENT_ENERGY
      REAL (kind=4) ::  WRITE_ENERGY
      REAL (kind=4) ::  BUYBACK_ENERGY
      REAL (kind=4) ::  MAYO1_BUYBACK_PERCENT=.0334
      REAL (kind=4) ::  HARRIS1_BUYBACK_PERCENT=.3333
      REAL (kind=4) ::  MAYO1_BUYBACK
      REAL (kind=4) ::  HARRIS_BUYBACK
      REAL (kind=4) ::  RETAINED_PERCENT
      REAL (kind=4) ::  MAYO1_REPLACEMENT
      REAL (kind=4) ::  ROX4_REPLACEMENT
      REAL (kind=4) ::  TOTAL_ENT_ENERGY=0.
      REAL (kind=4) ::  TOTAL_ENT_CAPACITY=0.
      REAL (kind=4) ::  RESOURCE_PERCENT
!
! REVENUE CALCULATIONS
!
      REAL (kind=4) ::  WRITE_FUEL_COST
      REAL (kind=4) ::  AVERAGE_FUEL_COST
      REAL (kind=4) ::  AVERAGE_PRODUCTION_COSTS
      REAL (kind=4) ::  SYS_FUEL_COST
      REAL (kind=4) ::  SYS_ENERGY
      REAL (kind=4) ::  BLOCK_FUEL_COST(2,MAX_CL_UNITS)
      REAL (kind=4) ::  WRITE_PRODUCTION_COST
      REAL (kind=4) ::  TOTAL_ENT_FUEL_COST=0.
      REAL (kind=4) ::  TOTAL_ENT_PRODUCTION_COST=0.
      REAL (kind=4) ::  MAYO1_RPL_PROD_COST
      REAL (kind=4) ::  MAYO1_RPL_AVE_COST
      REAL (kind=4) ::  ROX4_RPL_PROD_COST
      REAL (kind=4) ::  ROX4_RPL_AVE_COST
      REAL (kind=4) ::  REPLACEMENT_PROD_COST
      REAL (kind=4) ::  REPLACEMENT_AVE_COST
      REAL (kind=4) ::  SURPLUS_PROD_COST
      REAL (kind=4) ::  SURPLUS_AVE_COST
      REAL (kind=4) ::  HARRIS_BUYBACK_PROD_COST
      REAL (kind=4) ::  HARRIS_BUYBACK_AVE_COST
      REAL (kind=4) ::  MAYO1_BUYBACK_PROD_COST
      REAL (kind=4) ::  MAYO1_BUYBACK_AVE_COST
      REAL (kind=4) ::  BUYBACK_PROD_COST
      REAL (kind=4) ::  BUYBACK_AVE_COST
      REAL (kind=4) ::  TRANSFER_COST=0.
      REAL (kind=4) ::  TRANSFER_GEN=0.
      REAL (kind=4) ::  TRANSFER_SALES
      REAL (kind=4) ::   TRANSFER_PRICE
      REAL (kind=4) ::  PA_ANN_AVE_PROD
      REAL (kind=4) ::  EMC_ANN_AVE_PROD
      REAL (kind=4) ::  EMC_COST
      REAL (kind=4) ::  EMC_GEN
      REAL (kind=4) ::  EMC_PRICE
      REAL (kind=4) ::  TOTAL_PA_PRODUCTION_COST
      REAL (kind=4) ::  DEFICIT_ENERGY_COST
      REAL (kind=4) ::  ANN_RESERVE_CAPACITY
!
! ANNUAL CALCULATIONS
!
      REAL (kind=4) ::  ANN_PA_ENERGY(:)
      REAL (kind=4) ::  ANN_SYS_ENERGY(:)
      REAL (kind=4) ::  MON_SYS_ENERGY(:,:)
      REAL (kind=4) ::  MON_SYS_COST(:,:)
      REAL (kind=4) ::  MON_PA_ENERGY(:,:)
      REAL (kind=4) ::  MON_PA_COST(:,:)
      REAL (kind=4) ::  MON_PA_CAP(:,:)
      REAL (kind=4) ::  ANN_PA_CAP(:)
      REAL (kind=4) ::  ANN_SYS_CAP(:)
      REAL (kind=4) ::  ANN_PA_COST(:)
      REAL (kind=4) ::  ANN_SYS_COST(:)
      REAL (kind=4) ::  PA_HOURLY_LOADS(:)
      REAL (kind=4) ::  CPL_ENERGY_REVENUE_FROM_PA(0:12)
      REAL (kind=4) ::  CPL_ENERGY_TO_PA(0:12)
      REAL (kind=4) ::  CPL_EMC_REVENUE(0:12)
      REAL (kind=4) ::  R_CPL_EMC_REVENUE
      REAL (kind=4) ::  CPL_ENERGY_EXPENSE_TO_PA(0:12)
      REAL (kind=4) ::  CPL_ENERGY_FROM_PA(0:12)
      REAL (kind=4) ::  CPL_ANN_EXPENSE_TO_PA=0.
      REAL (kind=4) ::  CPL_ANN_REVENUE_FROM_PA=0.
      REAL (kind=4) ::  R_CPL_ENERGY_REVENUE_FROM_PA
      REAL (kind=4) ::  R_CPL_ENERGY_TO_PA
      REAL (kind=4) ::  R_CPL_ENERGY_EXPENSE_TO_PA
      REAL (kind=4) ::  R_CPL_ENERGY_FROM_PA
      REAL (kind=4) ::  PA_RESERVE_RATE
      REAL (kind=4) ::  PA_SUPPLEMENTAL_RATE
      REAL (kind=4) ::  PA_TRANSMISSION_RATE
      REAL (kind=4) ::  PA_MAYO_BUYBACK
      REAL (kind=4) ::  PA_HARRIS_BUYBACK
      REAL (kind=4) ::  R_BUYBACK
      REAL (kind=4) ::  S_CATAWBA_REVENUES
      REAL (kind=4) ::  S_CATAWBA_EXPENSES
      REAL (kind=4) ::  S_CPL_ENERGY_TO_PA
      REAL (kind=4) ::  S_CPL_ENERGY_FROM_PA
      INTEGER (kind=2) ::  SORTED_HOUR(:),PA_SEPA_HOUR(:)
      ALLOCATABLE :: &
               ANN_PA_ENERGY,ANN_SYS_ENERGY, &
               MON_PA_ENERGY,MON_PA_COST, &
               MON_SYS_ENERGY,MON_SYS_COST,MON_PA_CAP, &
               ANN_PA_CAP,ANN_SYS_CAP,ANN_PA_COST,ANN_SYS_COST, &
               PA_HOURLY_LOADS,SORTED_HOUR,PA_SEPA_HOUR
!
!
! DETAILED REPORT OVERHEAD
!

      LOGICAL (kind=1) ::  PAEN_REPORT_NOT_OPEN=.TRUE.
      LOGICAL (kind=1) ::  PAEN_REPORT_ACTIVE
      INTEGER (kind=2) ::  LAST_SEASON=0
      INTEGER (kind=2) ::  CPL_PAEN_RPT_HEADER
      INTEGER (kind=2) ::  CPL_PAEN_UNIT
      INTEGER (kind=2) ::  CURRENT_year_loc
      INTEGER (kind=2) ::  CURRENT_MONTH
      INTEGER (kind=2) ::  ANNUAL_COUNTER=0
      INTEGER (kind=2) ::  REMAINING_SYSTEM_CLASS=4
      INTEGER ::  CPL_PAEN_REC
      REAL (kind=4) ::  WRITE_CAPACITY
      REAL (kind=4) ::  CF
      REAL (kind=4) ::  TOTAL_WRITE_ENERGY
      REAL (kind=4) ::  TOTAL_WRITE_CAPACITY
      REAL (kind=4) ::  RS_CLASS_LOSSES
      CHARACTER (len=9) ::  CL_MONTH_NAME(13)
      CHARACTER (len=20) ::    MONTH_NAME,WRITE_UNIT_NAME
!
      LOGICAL (kind=1) ::    ECAP_REPORT_NOT_OPEN=.TRUE.
      LOGICAL (kind=1) ::  ECAP_REPORT_ACTIVE
      LOGICAL (kind=1) ::  HOURLY_ECAP_REPORT
      LOGICAL (kind=1) ::  HOURLY_ECAP_ACTIVE
      LOGICAL (kind=1) ::  PA_SWITCH
      INTEGER (kind=2) ::    HOURLY_ECAP_RPT_HEADER,HOURLY_ECAP_UNIT
      INTEGER ::  HOURLY_ECAP_REC
!
! SONG'S DATA
!
      REAL (kind=4) :: &
                  PAGENCY_PEAK, &
                  PAGENCY_LOAD, &
                  SEPA_CAP, &
                  NEW_RESOURCE_CAP, &
                  CONTRACT_CAP, &
                  BRUNSW1_CAP, &
                  BRUNSW2_CAP, &
                  HARRIS1_CAP, &
                  MAYO1_CAP, &
                  ROXB4_CAP, &
                  UNUSED_ROXB4_CAP, &
                  UNUSED_MAYO1_CAP, &
                  UNUSED_HARRIS1_CAP, &
                  UNUSED_BRUNSW2_CAP, &
                  UNUSED_BRUNSW1_CAP, &
                  TOT_RETAINED_CAP, &
                  TOTAL_RETAINED_CAP_BASE, &
                  TOTAL_RETAINED_AFTER_OUT, &
                  RES_CAP
      REAL (kind=4) :: &
                  RES_CAP_FACTOR, &
                  UNUSED_SUPP_CAP, &
                  SUPP_CAP, &
                  UNMET_DEMAND, &
                  MONTHLY_SEPA_CAP, &
                  MONTHLY_CONTRACT_CAP, &
                  MONTHLY_TOT_RETAINED_CAP, &
                  MONTHLY_SUPP_CAP, &
                  MONTHLY_RES_CAP, &
                  MONTHLY_UNMET_DEMAND, &
                  MONTHLY_UNUSED_SUPP_CAP, &
                  MONTHLY_UNUSED_ENTITLED_CAP, &
                  AVA_RES_CAP, &
                  AVG_SEPA_CAP, &
                  AVG_CONTRACT_CAP, &
                  AVG_TOT_RETAINED_CAP, &
                  AVG_SUPP_CAP, &
                  AVG_AVA_RES_CAP, &
                  AVG_RES_CAP
      REAL (kind=4) :: &
                  AVG_UNMET_DEMAND, &
                  AVG_UNUSED_SUPP_CAP, &
                  AVG_UNUSED_ENTITLED_CAP, &
                  PAGENCY_ENERGY, &
                  SQ_SEPA_ENERGY, &
                  NEW_RESOURCE_ENERGY, &
                  BRUNSW1_ENERGY, &
                  BRUNSW2_ENERGY, &
                  HARRIS1_ENERGY, &
                  MAYO1_ENERGY, &
                  ROXB4_ENERGY, &
                  MONTHLY_PA_ENERGY, &
                  MONTHLY_BRUNSW1_ENERGY, &
                  MONTHLY_BRUNSW2_ENERGY, &
                  MONTHLY_HARRIS1_ENERGY, &
                  MONTHLY_MAYO1_ENERGY, &
                  MONTHLY_ROXB4_ENERGY, &
                  TOTAL_ENTITLE_ENERGY, &
                  RES_ENERGY
      REAL (kind=4) :: &
                  UNUSED_SUPP_ENERGY, &
                  SUPP_ENERGY, &
                  UNSERVED_ENERGY, &
                  CONTRACT_ENERGY, &
                  UNUSED_CONTRACT_ENERGY, &
                  UNUSED_ENERGY_COEF, &
                  UNUSED_ENTITLED_CAP, &
                  TEMP, &
                  DEFICIENCY, &
                  CONTRACT_SURPLUS, &
                  CONTRACT_REPLACEMENT, &
                  MONTHLY_AVA_RES_CAP
      REAL (kind=4) :: &
                  MONTH_TOTAL_ENTITLE_ENERGY, &
                  PA_UNIT_DISPATCH_CAP(10), &
                  TEMP_UNUSED_CAP, &
                  TEMP_UNIT_CAP, &
                  MONTH_RES_ENERGY, &
                  MONTH_UNUSED_SUPP_ENERGY, &
                  MONTH_SUPP_ENERGY, &
                  MONTH_UNSERVED_ENERGY, &
                  MONTH_CONTRACT_ENERGY, &
                  MONTH_UNUSED_CONTRACT_ENGY, &
                  MONTH_DEFICIENCY, &
                  MONTH_CONTRACT_SURPLUS, &
                  MONTH_CONTRACT_REPLACEMENT, &
                  MONTH_SEPA_ENERGY
      INTEGER (kind=2) :: &
                  PA_UNIT_DISPATCH_INDEX(10), &
                  MAX_PA_DISPATCH_BLOCKS,L,MAX_SEPA_HOUR
      REAL (kind=4) ::  R_CPL_PA_MWH,R_CPL_OFF_SYS_ENRG,R_CPL_NUC_COST
      REAL (kind=4) ::  R_CATAWBA_REVENUES(0:12)
      REAL (kind=4) ::  R_CATAWBA_EXPENSES(0:12)
      INTEGER (kind=2) ::  MO


      IF(ISEAS == 1) THEN
         CPL_FUEL_GENERATION = 0.
         CPL_NUC_GENERATION = 0.
         CPL_PUR_POWER_ENERGY_COST = 0.
         CPL_NCEMPA_ENTITLEMENT = 0.
         CPL_NCEMPA_ENTITLEMENT_MWH = 0.
         CPL_NCEMPA_MWH_SUPPLIED_BY_CPL = 0.
         CPL_NCEMPA_COST_MWH_SUP_BY_CPL = 0.
         CPL_OFF_SYSTEM_INCR_COST = 0.
         OFF_SYSTEM_SALES_ENERGY = 0.
!
         CPL_ENERGY_REVENUE_FROM_PA = 0.
         CPL_EMC_REVENUE = 0.
         CPL_ENERGY_TO_PA = 0.
         CPL_ENERGY_EXPENSE_TO_PA = 0.
         CPL_ENERGY_FROM_PA = 0.
         CPL_ANN_EXPENSE_TO_PA = 0.
         CPL_ANN_REVENUE_FROM_PA = 0.
      ENDIF
!
! PULLED IN FROM DR_BOOTH
!
      CPL_NUC_GENERATION(ISEAS) = CPL_NUC_GENERATION(ISEAS) &
                                  + PERIOD_CPL_GENERATION_COST_NUC
      CPL_NUC_GENERATION(0) = CPL_NUC_GENERATION(0) &
                              + PERIOD_CPL_GENERATION_COST_NUC
      CPL_FUEL_GENERATION(ISEAS) = CPL_FUEL_GENERATION(ISEAS) &
                                   + PERIOD_CPL_GEN_COST_FOSSIL
      CPL_FUEL_GENERATION(0) = CPL_FUEL_GENERATION(0) &
                               + PERIOD_CPL_GEN_COST_FOSSIL
      CPL_PUR_POWER_ENERGY_COST(ISEAS) = &
                                  CPL_PUR_POWER_ENERGY_COST(ISEAS) + &
                                          PERIOD_CPL_PURCHASE_POWER_COST
      CPL_PUR_POWER_ENERGY_COST(0) = &
                                  CPL_PUR_POWER_ENERGY_COST(0) + &
                                          PERIOD_CPL_PURCHASE_POWER_COST
!
! 8/5/99. GAT.
!
      CALL GET_PA_SWITCH(PA_SWITCH)
      IF(.NOT. PA_SWITCH) THEN
!
         TRANSFER_COST = &
            (P_CLASS_ASSIGNED_COST(1) + &
             P_CLASS_ASS_ECON_COST(1) - P_CLASS_ASS_ECON_REV(1) + &
             P_CLASS_ASSIGNED_COST(2) + &
             P_CLASS_ASS_ECON_COST(2) - P_CLASS_ASS_ECON_REV(2))
         TRANSFER_GEN = &
            (P_CLASS_ASSIGNED_ENERGY(1) + P_CLASS_ASSIGNED_ENERGY(2) - &
             P_CLASS_ASS_ECON_SELL(1) + P_CLASS_ASS_ECON_BUY(1) - &
             P_CLASS_ASS_ECON_SELL(2) + P_CLASS_ASS_ECON_BUY(2) )
!
         CALL RETURN_CLASS_LOSSES(RS_CLASS_LOSSES, &
                                                 REMAINING_SYSTEM_CLASS)
!
!
! EMC ENERGY REVENUES
!
         EMC_COST = TRANSFER_COST
         TRANSFER_SALES = TRANSFER_GEN * &
                       (1. - MIN(RS_CLASS_LOSSES,.9999)) - &
                                                      SEPA_ENERGY(ISEAS)
         EMC_GEN = TRANSFER_SALES
         IF(USE_EMC_ENERGY_PRICE()) THEN
             EMC_PRICE = EMC_ANN_AVE_PROD()
         ELSEIF(EMC_GEN > 0.) THEN
            EMC_PRICE = EMC_COST / EMC_GEN
         ELSE
            EMC_PRICE = 0.
         ENDIF
         CALL GET_CPL_EMC_MON_ENER_REVS(ISEAS,EMC_PRICE, &
                                          WRITE_ENERGY,WRITE_CAPACITY, &
                                          WRITE_PRODUCTION_COST)
!
         RETURN
!
      ENDIF
!
      PAEN_REPORT_ACTIVE = .TRUE. ! UNTIL ADDED TO DETAILED REPORTS
      CURRENT_year_loc = world%base_year_common + globecom_year
      IF(PAEN_REPORT_NOT_OPEN .AND. &
                             PAEN_REPORT_ACTIVE .AND. &
                                               .NOT. TESTING_PLAN ) THEN
         PAEN_REPORT_NOT_OPEN = .FALSE.
         CPL_PAEN_UNIT = CPL_PAEN_RPT_HEADER(CPL_PAEN_REC)
         LAST_SEASON = get_PRODUCTION_PERIODS_in()
         DO CURRENT_MONTH = 1, LAST_SEASON
            CL_MONTH_NAME(CURRENT_MONTH) = MONTH_NAME(CURRENT_MONTH)
         ENDDO
         CL_MONTH_NAME(LAST_SEASON+1) = 'Annual'
         ANNUAL_COUNTER = LAST_SEASON + 1
      ENDIF
      IF(CURRENT_year_loc < 2008) THEN
         HARRIS1_BUYBACK_PERCENT = 0.3333
      ELSE
         HARRIS1_BUYBACK_PERCENT = 0.0000
      ENDIF
!
! IDENTIFY UNITS PARTIALLY OWNED BY PA
!
      NO_PA_UNITS = 0
      PA_RESOURCE_ENERGY_COST = 0.
      SEAS_HOURS = FLOAT(HOURS_INCREMENT)
      DO UNITNO = 1, get_nunits()
         IF(CL_POOL_FRAC_OWN(UNITNO) < 100. .AND. &
                                     CL_POOL_FRAC_OWN(UNITNO) > 0.) THEN
            NO_PA_UNITS = NO_PA_UNITS + 1
            IF(NO_PA_UNITS == 1) THEN
               IF(CURRENT_year_loc <= 1997) THEN
                  MAYO1_BUYBACK = &
                     (SYSTEM_ENERGY(1,UNITNO) + &
                            SYSTEM_ENERGY(2,UNITNO)) * &
                                    (1.-CL_POOL_FRAC_OWN(UNITNO)/100.) * &
                                                 MAYO1_BUYBACK_PERCENT * &
                                                              SEAS_HOURS
                  BUYBACK_ENERGY = BUYBACK_ENERGY + MAYO1_BUYBACK
               ELSE
                  MAYO1_BUYBACK = 0.
               ENDIF
            ELSEIF(NO_PA_UNITS == 5) THEN
               IF(CURRENT_year_loc <= 2007) THEN
                  HARRIS_BUYBACK = &
                     (SYSTEM_ENERGY(1,UNITNO) + &
                            SYSTEM_ENERGY(2,UNITNO)) * &
                                    (1.-CL_POOL_FRAC_OWN(UNITNO)/100.) * &
                                               HARRIS1_BUYBACK_PERCENT * &
                                                              SEAS_HOURS
               ENDIF
            ENDIF
         ELSEIF(CL_POOL_FRAC_OWN(UNITNO) == 0.) THEN
            SYS_ENERGY = (SYSTEM_ENERGY(1,UNITNO) + &
                                   SYSTEM_ENERGY(2,UNITNO)) * SEAS_HOURS
            SYS_FUEL_COST = BLOCK_FUEL_COST(1,UNITNO) + &
                            BLOCK_FUEL_COST(2,UNITNO)
            IF(SYS_ENERGY > 0.) THEN
               PA_RESOURCE_ENERGY_COST = SYS_FUEL_COST/SYS_ENERGY + &
                                                         VCPMWH(UNITNO)
            ELSE
               PA_RESOURCE_ENERGY_COST = VCPMWH(UNITNO)
            ENDIF
         ENDIF
      ENDDO
      IF(NO_PA_UNITS < 5) THEN
         WRITE(4,*) "Less than 5 PA Units were detected when"
         WRITE(4,*) "PA was false. Please check PA switch in the"
         WRITE(4,*) "CPL file and percent ownership in the CL file"
         er_message='See WARNING MESSAGES-cat2objt.1'
         call end_program(er_message)
      ENDIF
      NO_PA_RESOURCES = NO_PA_UNITS + NO_PA_CONTRACTS
!
!
!
! REPLACE CPL LOAD CURVE WITH TRANSFORMED LOAD CURVE
!
!      SEAS_HOURS = FLOAT(HOURS_INCREMENT) MOVED UP. 9/10/97. GAT.
      BUYBACK_ENERGY = MAYO1_BUYBACK + HARRIS_BUYBACK
!
!
!
      IF(ALLOCATED(PA_HOURLY_LOADS)) DEALLOCATE(PA_HOURLY_LOADS, &
                                               SORTED_HOUR,PA_SEPA_HOUR)
      ALLOCATE(PA_HOURLY_LOADS(HOURS_INCREMENT), &
               SORTED_HOUR(HOURS_INCREMENT), &
               PA_SEPA_HOUR(HOURS_INCREMENT))
      CALL  CREATE_CPL_LDC(      ISEAS, &
                                 SYSTEM_PEAK, &
                                 SYSTEM_DEMAND, &
                                 SYSTEM_LODDUR, &
                                 PA_LODDUR, &
                                 LPROB2, &
                                 SEAS_HOURS, &
                                 PA_PEAK, &
                                 PA_DEMAND, &
                                 PA_DX, &
                                 BUYBACK_ENERGY, &
                                 PA_HOURLY_LOADS)
!
!

         IF(ALLOCATED(PA_ENERGY)) DEALLOCATE(PA_ENERGY,PA_UNIT_INDEX)
         ALLOCATE(PA_UNIT_INDEX(NO_PA_RESOURCES), &
                  PA_ENERGY(2,NO_PA_RESOURCES), &
                  EFFECTIVE_CAPACITY(2,NO_PA_RESOURCES), &
                  LEFT(NO_PA_RESOURCES), &
                  RIGHT(NO_PA_RESOURCES))
!
!
         IF(ISEAS == 1 .AND. NO_PA_UNITS > 0) THEN
            IF(ALLOCATED(ANN_PA_ENERGY)) &
                           DEALLOCATE( ANN_PA_ENERGY,ANN_SYS_ENERGY, &
                                       ANN_PA_CAP,ANN_SYS_CAP, &
                                       ANN_PA_COST,ANN_SYS_COST, &
                                       MON_SYS_ENERGY,MON_SYS_COST, &
                                       MON_PA_CAP, &
                                       MON_PA_ENERGY, &
                                       MON_PA_COST)
            ALLOCATE(ANN_PA_ENERGY(0:NO_PA_RESOURCES+1), &
                     ANN_SYS_ENERGY(NO_PA_UNITS+NO_PA_ACCTS), &
                     ANN_PA_CAP(0:NO_PA_RESOURCES+1), &
                     ANN_SYS_CAP(NO_PA_UNITS+NO_PA_ACCTS), &
                     ANN_PA_COST(0:NO_PA_RESOURCES+1), &
                     ANN_SYS_COST(NO_PA_UNITS+NO_PA_ACCTS), &
                     MON_SYS_ENERGY(NO_PA_UNITS+NO_PA_ACCTS,0:12), &
                     MON_SYS_COST(NO_PA_UNITS+NO_PA_ACCTS,0:12), &
                     MON_PA_CAP(0:NO_PA_RESOURCES+1,0:12), &
                     MON_PA_ENERGY(0:NO_PA_RESOURCES+1,0:12), &
                     MON_PA_COST(NO_PA_UNITS+NO_PA_ACCTS,0:12))

            ANN_PA_ENERGY = 0.
            ANN_PA_CAP = 0.
            ANN_SYS_ENERGY = 0.
            ANN_SYS_CAP = 0.
            ANN_PA_COST = 0.
            ANN_SYS_COST = 0.
            MON_SYS_ENERGY = 0.
            MON_SYS_COST = 0.
            MON_PA_CAP = 0.
            MON_PA_ENERGY = 0.
            MON_PA_COST = 0.
         ELSEIF(NO_PA_UNITS == 0.) THEN
            WRITE(4,*) "NO PA OWNED UNITS FOUND"
            WRITE(4,*) "CPL DATA BASE MUST HAVE OWNERSHIP SHARES"
            WRITE(4,*) " "
         ENDIF
         I = 1
         PA_CAPSYS = 0.
         PA_EXP_CAPSYS = 0.
         DO UNITNO = 1, get_nunits()
            IF(CL_POOL_FRAC_OWN(UNITNO) < 100. .AND. &
                                     CL_POOL_FRAC_OWN(UNITNO) > 0.) THEN
!
               PA_UNIT_INDEX(I) = UNITNO
               PA_ENERGY(1,I) = 0.
               PA_ENERGY(2,I) = 0.
               EFFECTIVE_CAPACITY(1,I) = 0.
               EFFECTIVE_CAPACITY(2,I) = 0.
               LEFT(I) = 0.
               RIGHT(I) = 0.
!
               CALL GET_PA_RETAINED_PERCENT(I,RESOURCE_PERCENT)
!
               PA_CAPSYS = PA_CAPSYS + MW(2,UNITNO) * &
                                    RESOURCE_PERCENT
               PA_EXP_CAPSYS = PA_EXP_CAPSYS + MW(2,UNITNO) * &
                                    RESOURCE_PERCENT * &
                                    EAVAIL(UNITNO) * &
                                    (1.-MAINTENANCE_RATE(UNITNO))
!
               I = I + 1
            ENDIF
         ENDDO
         CALL GET_PA_RESERVE_CAPACITY(RESERVE_CAPACITY,MAINTENANCE_RATE, &
                                      EAVAIL,PA_UNIT_INDEX, &
                                      PA_CAPSYS,PA_EXP_CAPSYS, &
                                      RESERVE_PERCENT)
         CALL GET_UNUSED_SUPP_CAPACITY(UNUSED_SUPP_CAPACITY)
         CALL GET_SUPPLEMENTAL_CAPACITY(SUPPLEMENTAL_CAPACITY,PA_PEAK)
         CALL GET_PA_RESOURCE_CAPACITY(PA_CONTRACT_CAPACITY)
         DO UNITNO = 1, NO_PA_CONTRACTS ! LAST ENERGY SOURCE IN CL FILE
!
               I = NO_PA_UNITS + UNITNO
               PA_UNIT_INDEX(I) = UNITNO
               PA_ENERGY(1,I) = 0.
               PA_ENERGY(2,I) = 0.
               EFFECTIVE_CAPACITY(1,I) = 0.
               EFFECTIVE_CAPACITY(2,I) = 0.
               LEFT(I) = 0.
               RIGHT(I) = 0.
               IF(UNITNO == 1) THEN !
                  PA_CAPSYS = PA_CAPSYS + RESERVE_CAPACITY
               ELSEIF(UNITNO == 2) THEN
                  PA_CAPSYS = PA_CAPSYS + UNUSED_SUPP_CAPACITY
               ELSEIF(UNITNO == 3) THEN
                  PA_CAPSYS = PA_CAPSYS + SUPPLEMENTAL_CAPACITY
               ELSEIF(UNITNO == 4) THEN
                  PA_CAPSYS = PA_CAPSYS + PA_CONTRACT_CAPACITY
               ENDIF
!
         ENDDO
!
         DO J = 1, LAST_POINT
            LPROB(J,1) = LPROB2(J)
            LPROB(J,2) = 0.
         ENDDO
         LPROB(1,2) = 1.
!
         CURRENT = 1
         PA_BASE = PA_LODDUR(1)
         NBLOK2 = NBLOCK
         REMAINING_ENERGY = SNGL(PA_DEMAND)/SEAS_HOURS
         A = 0.
         B = 0.
         CAPON = 0.
!
! MAIN ROUTINE
!
         I = 0
         CURRENT_CPL_CONTRACT = 0
         CL_UNITS_ACTIVE = .TRUE.
!
         TOTAL_RETAINED_CAP_BASE = 0.
         TOTAL_RETAINED_AFTER_OUT = 0.
         BRUNSW1_CAP= 0.0
         BRUNSW2_CAP = 0.0
         HARRIS1_CAP = 0.0
         MAYO1_CAP = 0.0
         ROXB4_CAP = 0.0
         MAX_PA_DISPATCH_BLOCKS = 0
!
         ECAP_REPORT_ACTIVE = HOURLY_ECAP_REPORT()
         HOURLY_ECAP_ACTIVE = ECAP_REPORT_ACTIVE
         IF(ECAP_REPORT_NOT_OPEN .AND. &
                             ECAP_REPORT_ACTIVE .AND. &
                                               .NOT. TESTING_PLAN ) THEN
            ECAP_REPORT_NOT_OPEN = .FALSE.
            HOURLY_ECAP_UNIT = HOURLY_ECAP_RPT_HEADER(HOURLY_ECAP_REC)
            LAST_SEASON = get_PRODUCTION_PERIODS_in()
            DO CURRENT_MONTH = 1, LAST_SEASON
               CL_MONTH_NAME(CURRENT_MONTH) = MONTH_NAME(CURRENT_MONTH)
            ENDDO
            CL_MONTH_NAME(LAST_SEASON+1) = 'Annual'
            ANNUAL_COUNTER = LAST_SEASON + 1
         ENDIF
!
         DO
!
            IF(I == NBLOK2) CL_UNITS_ACTIVE = .FALSE.
!
            IF(CL_UNITS_ACTIVE) THEN
               I = I + 1 ! UNIT SEGMENT COUNTER
               J = 1
               DO WHILE(UNIT(I) /= PA_UNIT_INDEX(J) .AND. &
                                                      J <= NO_PA_UNITS )
                  J = J + 1
                  CYCLE
               ENDDO
               IF(J > NO_PA_UNITS) CYCLE ! THIS IS NOT A PA UNIT
!
! THIS IS A VALID PA UNIT
!
               PAUNITNO = J ! POSITION OF PA UNIT INSIDE PA ARRAY
!
               UNITNO = UNIT(I)
               BLKNUM = MAX(INT(1,2),BLKNO(I))
!
               IF(CL_POOL_FRAC_OWN(UNITNO) < 100. .AND. &
                                     CL_POOL_FRAC_OWN(UNITNO) > 0.) THEN
                  IF(J == 5) THEN
                    BLK_MW = MWBLOK(I) * (1.-HARRIS1_BUYBACK_PERCENT) * &
                                      (1.-CL_POOL_FRAC_OWN(UNITNO)/100.)
                  ELSE
                     BLK_MW = MWBLOK(I) * &
                                      (1.-CL_POOL_FRAC_OWN(UNITNO)/100.)
                  ENDIF
               ELSE
                  WRITE(4,*) '*** CPL_PA_DISPATCH ***'
                  WRITE(4,*) UNITNM(UNITNO),"IS NOT OWNED BY POOL."
                  WRITE(4,*) "JOINT UNITS MUST BE PART OWNED BY POOL"
                  WRITE(4,*) "TO PERFORM JOINT DISPATCH."
                  er_message='See WARNING MESSAGES-cat2objt-2'
                  call end_program(er_message)
               ENDIF
!
               EA = TEMPEA(BLKNUM,UNITNO) ! ASSUMES BOOTH ALGORITHM
               CAPBLK = BLK_MW * (1. - MAINTENANCE_RATE(UNITNO))
!
!
!
               MAX_PA_DISPATCH_BLOCKS = MAX_PA_DISPATCH_BLOCKS + 1
               PA_UNIT_DISPATCH_CAP(MAX_PA_DISPATCH_BLOCKS) = CAPBLK*EA
               PA_UNIT_DISPATCH_INDEX(MAX_PA_DISPATCH_BLOCKS) = J
!
               TOTAL_RETAINED_CAP_BASE = TOTAL_RETAINED_CAP_BASE + &
                                                                  BLK_MW
               TOTAL_RETAINED_AFTER_OUT = TOTAL_RETAINED_AFTER_OUT + &
                                                             BLK_MW * EA
               IF(J == 1) THEN
                  MAYO1_CAP = MAYO1_CAP + CAPBLK * EA
               ELSEIF(J == 2) THEN
                  ROXB4_CAP = ROXB4_CAP + CAPBLK * EA
               ELSEIF(J == 3) THEN
                  BRUNSW1_CAP= BRUNSW1_CAP + CAPBLK * EA
               ELSEIF(J == 4) THEN
                  BRUNSW2_CAP= BRUNSW2_CAP + CAPBLK * EA
               ELSEIF(J == 5) THEN
                  HARRIS1_CAP = HARRIS1_CAP + CAPBLK * EA
               ENDIF
!
!
            ELSEIF(CURRENT_CPL_CONTRACT < NO_PA_CONTRACTS) THEN
               CURRENT_CPL_CONTRACT = CURRENT_CPL_CONTRACT + 1
               PAUNITNO = NO_PA_UNITS + CURRENT_CPL_CONTRACT
!
               IF(CURRENT_CPL_CONTRACT == NO_PA_CONTRACTS) THEN
!
! IN GENERAL MULTI-AREA DISPATCH, THE MODEL WILL RELY ON 'L' UNIT
!
                  PA_ENERGY(1,PAUNITNO) = MAX(0.,REMAINING_ENERGY)
                  REMAINING_ENERGY = 0.
                  EXIT
               ELSE
                  IF(CURRENT_CPL_CONTRACT == 1) THEN !
                     BLK_MW = RESERVE_CAPACITY
                  ELSEIF(CURRENT_CPL_CONTRACT == 2) THEN
                     BLK_MW = UNUSED_SUPP_CAPACITY
                  ELSEIF(CURRENT_CPL_CONTRACT == 3) THEN
                     BLK_MW = SUPPLEMENTAL_CAPACITY
                  ELSEIF(CURRENT_CPL_CONTRACT == 4) THEN
                     BLK_MW = PA_CONTRACT_CAPACITY
                  ENDIF
!
!                 UNIT INDEXED ARRAY DON'T APPLY
!
                  BLKNUM = 1
                  EA = 1.0 ! 100% AVAILABLE PER UPM CODE
                  CAPBLK = BLK_MW ! 100% AVAILABLE PER UPM CODE
!
               ENDIF
!
            ELSE
               WRITE(4,*) '*** line 2387 CPL_PA_DISPATCH ***'
               WRITE(4,*) "MULTI-AREA DISPATCH EXCEEDS AVAILABLE"
               WRITE(4,*) "RESOURCES.  LAST RESOURCE NOT FOUND"
               er_message='See WARNING MESSAGES-cat2objt-6'
               call end_program(er_message)
            ENDIF
            IF(BLKNUM <= 1) THEN
               EFFECTIVE_CAPACITY(1,PAUNITNO) = CAPBLK
            ELSE
               EFFECTIVE_CAPACITY(2,PAUNITNO) = CAPBLK
            ENDIF
!
!
! VALID UNITS FOR DISPATCH
!
            IF( .NOT. HOURLY_ECAP_ACTIVE) THEN
               IF(BLK_MW == 0.0) then
                CYCLE
               end if
               
               call write_energy_trace_message("cpl_pa_dispatch", &
                "cat2objt:0002", "Zeroing out ENRG")
               call write_energy_trace("clp_pa_dispatch", "cat2objt:0002", &
                ENRG)
                
               ENRG = 0.0
               call write_energy_trace("clp_pa_dispatch", "cat2objt:0003 - to:", &
                ENRG)
                
               A = B


               PA_CAPSYS = PA_CAPSYS - BLK_MW + CAPBLK

               IF(CAPBLK > 0. .AND. REMAINING_ENERGY > 0.) THEN
                  B = A + CAPBLK
                  IF(A < PA_CAPSYS) THEN
                     IF(B < PA_BASE) THEN
                        ENRG = CAPBLK
                        call write_energy_trace("clp_pa_dispatch", "cat2objt:0004", &
                            ENRG)
                            
                        IF(BLKNUM <= 1) THEN
                           PA_ENERGY(1,PAUNITNO) = ENRG
                           EFFECTIVE_CAPACITY(1,PAUNITNO) = ENRG
                        ELSE
                           PA_ENERGY(2,PAUNITNO) = ENRG
                           EFFECTIVE_CAPACITY(2,PAUNITNO) = ENRG
                           LEFT(PAUNITNO) = 1.0
                           RIGHT(PAUNITNO) = 1.0
                        ENDIF
                     ELSE
                        CALL CALENRG( PA_LODDUR,LPROB(1,CURRENT), &
                                    A,B,ENRG, &
                                    PA_CAPSYS,PA_DX,LEFT(PAUNITNO), &
                                    RIGHT(PAUNITNO),ISTART)
                        ENRG = MIN(ENRG,REMAINING_ENERGY/EA)
          call write_energy_trace("clp_pa_dispatch", "cat2objt:0005", &
                ENRG)
                        IF(BLKNUM <= 1) THEN
                           PA_ENERGY(1,PAUNITNO) = ENRG
                           EFFECTIVE_CAPACITY(1,PAUNITNO) = CAPBLK
                        ELSE
                           PA_ENERGY(2,PAUNITNO) = ENRG
                           EFFECTIVE_CAPACITY(2,PAUNITNO) = CAPBLK
                        ENDIF
                     ENDIF
                  ENDIF
                  REMAINING_ENERGY = REMAINING_ENERGY - EA*ENRG
                  IF((REMAINING_ENERGY > 0.) .AND. I < NBLOK2 ) THEN
                     CAPON = CAPON + CAPBLK
                     IF(UNIT(I+1) /= UNITNO) THEN
                        IF(EA <= .999 .AND. EA >= .001) THEN
                           CUMCAP = CUMCAP + CAPON
                           LIMR = MIN(PA_PEAK+CUMCAP,PA_CAPSYS) + PA_DX
                           CALL CONVOL(CAPON,EA,CURRENT,LIMR, &
                                            LPROB,PA_LODDUR,LAST_POINT)
                        ELSEIF(EA < .001) THEN
                           B = A
                        ENDIF
                        CAPON = 0.
                     ENDIF
                  ENDIF
               ENDIF !END BOOTH REMAINING ENERGY <> 0
            ENDIF
         ENDDO ! DISPATCHING LOOP (CONTRACTS OR CL UNITS ACTIVE)





! CAPACITY CALCULATIONS FOR CPL_PA CONTRACT
!
         IF( HOURLY_ECAP_ACTIVE) THEN

            SEPA_CAP = 29.2
            NEW_RESOURCE_CAP = 116.0

            RES_CAP_FACTOR = .12


            CONTRACT_CAP = SEPA_CAP + NEW_RESOURCE_CAP
            TOT_RETAINED_CAP = BRUNSW1_CAP + BRUNSW2_CAP + HARRIS1_CAP + &
                                            MAYO1_CAP + ROXB4_CAP

            TOTAL_RETAINED_CAP_BASE = TOTAL_RETAINED_AFTER_OUT




! 3/14/99. PER SKIP SPREADSHEET

            SUPP_CAP = PA_PEAK - CONTRACT_CAP - TOTAL_RETAINED_CAP_BASE

! OVERWRITTEN BELOW.  CALCULATED IN PA_... ROUTINE


            AVA_RES_CAP = RES_CAP
            RES_CAP = MIN(TOTAL_RETAINED_CAP_BASE - &
                                     TOT_RETAINED_CAP,RESERVE_CAPACITY )

            IF (RES_CAP == AVA_RES_CAP) THEN
               UNUSED_SUPP_CAP = TOTAL_RETAINED_CAP_BASE - &
                                             TOT_RETAINED_CAP - RES_CAP
            ELSE
               UNUSED_SUPP_CAP = 0.
            ENDIF
!
            MONTH_RES_ENERGY = 0.
            MONTH_UNUSED_SUPP_ENERGY = 0.
            MONTH_SUPP_ENERGY = 0.
            MONTH_UNSERVED_ENERGY = 0.
            MONTH_CONTRACT_ENERGY = 0.
            MONTH_UNUSED_CONTRACT_ENGY = 0.
            MONTH_DEFICIENCY = 0.
            MONTH_CONTRACT_SURPLUS = 0.
            MONTH_CONTRACT_REPLACEMENT = 0.
            MONTH_SEPA_ENERGY = 0.
            MONTHLY_SEPA_CAP = 0.
            MONTHLY_CONTRACT_CAP = 0.
            MONTHLY_TOT_RETAINED_CAP = 0.
            MONTHLY_SUPP_CAP = 0.
            MONTHLY_RES_CAP = 0.
            MONTHLY_UNMET_DEMAND = 0.
            MONTHLY_UNUSED_SUPP_CAP = 0.
            MONTHLY_UNUSED_ENTITLED_CAP = 0.
!
            MONTHLY_AVA_RES_CAP = 0.
            MONTH_TOTAL_ENTITLE_ENERGY = 0.


            MONTHLY_PA_ENERGY = 0.
            MONTHLY_BRUNSW1_ENERGY= 0.
            MONTHLY_BRUNSW2_ENERGY = 0.
            MONTHLY_HARRIS1_ENERGY = 0.
            MONTHLY_MAYO1_ENERGY = 0.
            MONTHLY_ROXB4_ENERGY = 0.
!
!
!
!
!
!
!
! GET THIS WORKING. THEN, SPEED IT UP.
!
            MAX_SEPA_HOUR = INT(SEPA_ENERGY_PER_ECAP(ISEAS)/SEPA_CAP)

            DO I = 1, HOURS_INCREMENT
               SORTED_HOUR(I) = I
            ENDDO

            PA_SEPA_HOUR = 0
        call write_sic_trace("cat2obj:0001", HOURS_INCREMENT)
            CALL SortIncrPos(HOURS_INCREMENT, &
                                            SORTED_HOUR,PA_HOURLY_LOADS)

            DO I = HOURS_INCREMENT, HOURS_INCREMENT - MAX_SEPA_HOUR,-1
               PA_SEPA_HOUR(SORTED_HOUR(I)) = I
            ENDDO

!

            DO I = 1, HOURS_INCREMENT
!
! ENERGY CALCULATIONS FOR CPL_PA CONTRACT
!
!
!
               IF(PA_SEPA_HOUR(I) > &
                                   HOURS_INCREMENT - MAX_SEPA_HOUR) THEN
                  SQ_SEPA_ENERGY = SEPA_CAP
               ELSEIF(PA_SEPA_HOUR(I) == &
                                   HOURS_INCREMENT - MAX_SEPA_HOUR) THEN
                  SQ_SEPA_ENERGY = SEPA_ENERGY_PER_ECAP(ISEAS) - &
                                              (MAX_SEPA_HOUR) * SEPA_CAP
               ELSE
                  SQ_SEPA_ENERGY = 0.
               ENDIF
!
               BRUNSW1_ENERGY= 0.
               BRUNSW2_ENERGY =0.
               HARRIS1_ENERGY = 0.
               MAYO1_ENERGY = 0.
               ROXB4_ENERGY = 0.
               MONTHLY_PA_ENERGY = MONTHLY_PA_ENERGY + &
                                                      PA_HOURLY_LOADS(I)
!
               IF (PA_HOURLY_LOADS(I) < TOT_RETAINED_CAP) THEN
                  UNUSED_ENTITLED_CAP = TOT_RETAINED_CAP - &
                                                      PA_HOURLY_LOADS(I)
               ELSE
                  UNUSED_ENTITLED_CAP = 0.
               ENDIF

               TEMP_UNUSED_CAP = UNUSED_ENTITLED_CAP
               L = MAX_PA_DISPATCH_BLOCKS
               DO L = MAX_PA_DISPATCH_BLOCKS,1,-1
                  IF(TEMP_UNUSED_CAP > 0. .AND. L > 3) THEN
                     IF(TEMP_UNUSED_CAP < PA_UNIT_DISPATCH_CAP(L) ) THEN
                        TEMP_UNIT_CAP = &
                               PA_UNIT_DISPATCH_CAP(L) - TEMP_UNUSED_CAP
                        TEMP_UNUSED_CAP = 0.
                     ELSE
                        TEMP_UNUSED_CAP = TEMP_UNUSED_CAP - &
                                                 PA_UNIT_DISPATCH_CAP(L)
                        TEMP_UNIT_CAP = 0.
                     ENDIF
!
                     IF(L == 0 .AND. TEMP_UNUSED_CAP > .0001) THEN
                        WRITE(4,*) '*** line 2635 CPL_PA_DISPATCH ***'
                        WRITE(4,*) " INSUFFICIENT RETAINED CAPACITY"
                        WRITE(4,*) " IN THE PA CALCULATION"
                        er_message='See WARNING MESSAGES-cat2objt-5'
                        call end_program(er_message)
                     ENDIF
                  ELSE
                     TEMP_UNIT_CAP = PA_UNIT_DISPATCH_CAP(L)
                  ENDIF
                  IF(PA_UNIT_DISPATCH_INDEX(L) == 1) THEN
                     MAYO1_ENERGY = MAYO1_ENERGY + TEMP_UNIT_CAP
                     MONTHLY_MAYO1_ENERGY = &
                                    MONTHLY_MAYO1_ENERGY + TEMP_UNIT_CAP
                  ELSEIF(PA_UNIT_DISPATCH_INDEX(L) == 2) THEN
                     ROXB4_ENERGY = ROXB4_ENERGY + TEMP_UNIT_CAP
                     MONTHLY_ROXB4_ENERGY = &
                                    MONTHLY_ROXB4_ENERGY + TEMP_UNIT_CAP
                  ELSEIF(PA_UNIT_DISPATCH_INDEX(L) == 3) THEN
                     BRUNSW1_ENERGY = BRUNSW1_ENERGY + TEMP_UNIT_CAP
                     MONTHLY_BRUNSW1_ENERGY = &
                                  MONTHLY_BRUNSW1_ENERGY + TEMP_UNIT_CAP
                  ELSEIF(PA_UNIT_DISPATCH_INDEX(L) == 4) THEN
                     BRUNSW2_ENERGY = BRUNSW2_ENERGY + TEMP_UNIT_CAP
                     MONTHLY_BRUNSW2_ENERGY = &
                                  MONTHLY_BRUNSW2_ENERGY + TEMP_UNIT_CAP
                  ELSEIF(PA_UNIT_DISPATCH_INDEX(L) == 5) THEN
                     HARRIS1_ENERGY = HARRIS1_ENERGY + TEMP_UNIT_CAP
                     MONTHLY_HARRIS1_ENERGY = &
                                  MONTHLY_HARRIS1_ENERGY + TEMP_UNIT_CAP
                  ENDIF
               ENDDO
!
! Entitlement Done.
!
               TOTAL_ENTITLE_ENERGY = BRUNSW1_ENERGY + BRUNSW2_ENERGY + &
                            HARRIS1_ENERGY + MAYO1_ENERGY + ROXB4_ENERGY

! RES_ENERGY. GAT. 3/3/99.



               RES_ENERGY = MAX(0.,MIN(RES_CAP, &
                             (PA_HOURLY_LOADS(I)-TOTAL_ENTITLE_ENERGY)))


!
! Reserve Done. GAT. 3/15/99.
!
               SUPP_ENERGY = &
                  MAX( MIN( (PA_PEAK - CONTRACT_CAP), &
                           (PA_HOURLY_LOADS(I) - SQ_SEPA_ENERGY) ), &
                                             TOTAL_RETAINED_CAP_BASE ) - &
                                                 TOTAL_RETAINED_CAP_BASE
!
! Supplemental Done.
!
               CONTRACT_ENERGY = &
                   MAX( (PA_HOURLY_LOADS(I) - SUPP_ENERGY - &
                                                        SQ_SEPA_ENERGY), &
                                             TOTAL_RETAINED_CAP_BASE ) - &
                                                 TOTAL_RETAINED_CAP_BASE

!
! Contract energy done.
!
               IF ((PA_HOURLY_LOADS(I) - SQ_SEPA_ENERGY) > &
                     (TOTAL_ENTITLE_ENERGY + RES_ENERGY) ) THEN
                  UNUSED_ENERGY_COEF = 1
               ELSE
                  UNUSED_ENERGY_COEF = 0
               ENDIF

!
! Every thing but UnusedSupplemental and deficiency has been calculated
!


               TEMP =  SQ_SEPA_ENERGY + CONTRACT_ENERGY + SUPP_ENERGY + &
                                       RES_ENERGY + TOTAL_ENTITLE_ENERGY
! GAT. ADDED ZERO CONDITION. 3/3/99. GAT.
               UNUSED_SUPP_ENERGY = MAX(0., &
                                       UNUSED_ENERGY_COEF * &
                                       MIN((PA_HOURLY_LOADS(I) - TEMP), &
                                       (SUPP_CAP - SUPP_ENERGY)))

! SET TO ZERO PER SKIP'S SPREADSHEET. 3/14/99.



               UNUSED_CONTRACT_ENERGY = 0.


               DEFICIENCY = MAX(0., &
                            PA_HOURLY_LOADS(I) - TEMP - &
                            UNUSED_SUPP_ENERGY - UNUSED_CONTRACT_ENERGY)
!
               CONTRACT_SURPLUS = MAX( 0., &
                        CONTRACT_CAP - CONTRACT_ENERGY - &
                                                UNUSED_CONTRACT_ENERGY )
               CONTRACT_REPLACEMENT = MAX( 0., &
                        CONTRACT_ENERGY - UNUSED_CONTRACT_ENERGY - &
                                                          CONTRACT_CAP )
               UNMET_DEMAND = PA_HOURLY_LOADS(I) - &
                  SQ_SEPA_ENERGY - &
                      CONTRACT_ENERGY - &
                           TOTAL_ENTITLE_ENERGY - &
                                 RES_ENERGY - &
                                    DEFICIENCY - &
                                         UNUSED_SUPP_ENERGY - &
                                             SUPP_ENERGY
!
!
!
               WRITE(HOURLY_ECAP_UNIT,REC=HOURLY_ECAP_REC) &
                PRT_ENDPOINT(),FLOAT(CURRENT_year_loc),CL_MONTH_NAME(ISEAS), &
                FLOAT(I),PA_PEAK,PA_HOURLY_LOADS(I),CONTRACT_CAP, &
                TOT_RETAINED_CAP,SUPP_CAP,AVA_RES_CAP,RES_CAP, &
                UNMET_DEMAND,UNUSED_SUPP_CAP,TOTAL_ENTITLE_ENERGY, &
                UNUSED_ENTITLED_CAP,MAYO1_ENERGY,ROXB4_ENERGY, &
                BRUNSW1_ENERGY,BRUNSW2_ENERGY,HARRIS1_ENERGY, &
                RES_ENERGY,SUPP_ENERGY,CONTRACT_ENERGY, &
                UNUSED_ENERGY_COEF,TEMP,UNUSED_SUPP_ENERGY, &
                UNUSED_CONTRACT_ENERGY,DEFICIENCY,SQ_SEPA_ENERGY
               HOURLY_ECAP_REC = HOURLY_ECAP_REC + 1
!
!              CALCULATE MONTHLY TOTAL CAPACITIES
!
               MONTHLY_SEPA_CAP = MONTHLY_SEPA_CAP + SEPA_CAP
               MONTHLY_CONTRACT_CAP = MONTHLY_CONTRACT_CAP + &
                                                    CONTRACT_CAP
               MONTHLY_TOT_RETAINED_CAP = MONTHLY_TOT_RETAINED_CAP + &
                                                   TOT_RETAINED_CAP
               MONTHLY_SUPP_CAP = MONTHLY_SUPP_CAP + SUPP_CAP
               MONTHLY_AVA_RES_CAP = MONTHLY_AVA_RES_CAP + &
                                                      AVA_RES_CAP
               MONTHLY_RES_CAP = MONTHLY_RES_CAP + RES_CAP
               MONTHLY_UNMET_DEMAND = MONTHLY_UNMET_DEMAND + &
                                                    UNMET_DEMAND
               MONTHLY_UNUSED_SUPP_CAP = MONTHLY_UNUSED_SUPP_CAP + &
                                                    UNUSED_SUPP_CAP
               MONTHLY_UNUSED_ENTITLED_CAP = &
                  MONTHLY_UNUSED_ENTITLED_CAP + UNUSED_ENTITLED_CAP


!
!              CALCULATE MONTHLY TOTAL ENERGY
!
               MONTH_TOTAL_ENTITLE_ENERGY = MONTH_TOTAL_ENTITLE_ENERGY+ &
                                                  TOTAL_ENTITLE_ENERGY
               MONTH_RES_ENERGY = MONTH_RES_ENERGY + RES_ENERGY
               MONTH_UNUSED_SUPP_ENERGY = MONTH_UNUSED_SUPP_ENERGY + &
                                                   UNUSED_SUPP_ENERGY
               MONTH_SUPP_ENERGY = MONTH_SUPP_ENERGY + SUPP_ENERGY
               UNSERVED_ENERGY = 0. ! DONE TO ELIMINATE THE WARNING BELOW MSG 8/3/01
               MONTH_UNSERVED_ENERGY = MONTH_UNSERVED_ENERGY + &
                                                      UNSERVED_ENERGY ! THIS USED BUT NEVER SET TO A VALUE
               MONTH_CONTRACT_ENERGY = MONTH_CONTRACT_ENERGY + &
                                                      CONTRACT_ENERGY
               MONTH_UNUSED_CONTRACT_ENGY = MONTH_UNUSED_CONTRACT_ENGY + &
                                                UNUSED_CONTRACT_ENERGY
               MONTH_DEFICIENCY = MONTH_DEFICIENCY + DEFICIENCY
               MONTH_CONTRACT_SURPLUS = MONTH_CONTRACT_SURPLUS  + &
                                                        CONTRACT_SURPLUS
               MONTH_CONTRACT_REPLACEMENT = MONTH_CONTRACT_REPLACEMENT + &
                                                    CONTRACT_REPLACEMENT
               MONTH_SEPA_ENERGY = MONTH_SEPA_ENERGY + SQ_SEPA_ENERGY
!
            ENDDO
!              CALCULATE AVERAGE MONTHLY CAPACITY

            AVG_SEPA_CAP = MONTHLY_SEPA_CAP/HOURS_INCREMENT
            AVG_CONTRACT_CAP = MONTHLY_CONTRACT_CAP/HOURS_INCREMENT
            AVG_TOT_RETAINED_CAP = MONTHLY_TOT_RETAINED_CAP/ &
                                                   HOURS_INCREMENT
            AVG_SUPP_CAP = MONTHLY_SUPP_CAP/HOURS_INCREMENT
            AVG_AVA_RES_CAP = MONTHLY_AVA_RES_CAP/HOURS_INCREMENT
            AVG_RES_CAP = MONTHLY_RES_CAP/HOURS_INCREMENT
            AVG_UNMET_DEMAND = MONTHLY_UNMET_DEMAND/HOURS_INCREMENT
            AVG_UNUSED_SUPP_CAP = MONTHLY_UNUSED_SUPP_CAP/ &
                                                   HOURS_INCREMENT
            AVG_UNUSED_ENTITLED_CAP = MONTHLY_UNUSED_ENTITLED_CAP/ &
                                                   HOURS_INCREMENT
!
            I = 750
!
            WRITE(HOURLY_ECAP_UNIT,REC=HOURLY_ECAP_REC) &
                PRT_ENDPOINT(),FLOAT(CURRENT_year_loc),CL_MONTH_NAME(ISEAS), &
                FLOAT(I),PA_PEAK,MONTHLY_PA_ENERGY,CONTRACT_CAP, &
                TOT_RETAINED_CAP,SUPP_CAP,AVA_RES_CAP,RES_CAP, &
                MONTHLY_UNMET_DEMAND,UNUSED_SUPP_CAP, &
                MONTH_TOTAL_ENTITLE_ENERGY,UNUSED_ENTITLED_CAP, &
                MONTHLY_MAYO1_ENERGY,MONTHLY_ROXB4_ENERGY, &
                MONTHLY_BRUNSW1_ENERGY,MONTHLY_BRUNSW2_ENERGY, &
                MONTHLY_HARRIS1_ENERGY,MONTH_RES_ENERGY, &
                MONTH_SUPP_ENERGY,MONTH_CONTRACT_ENERGY, &
                UNUSED_ENERGY_COEF,TEMP,MONTH_UNUSED_SUPP_ENERGY, &
                MONTH_UNUSED_CONTRACT_ENGY,MONTH_DEFICIENCY, &
                MONTH_SEPA_ENERGY
            HOURLY_ECAP_REC = HOURLY_ECAP_REC + 1
!
!
            I = PA_UNIT_INDEX(1)
            IF(TEMPEA(2,I) == 0.) THEN
               PA_ENERGY(1,1) = 0.
            ELSE
               PA_ENERGY(1,1) = MONTHLY_MAYO1_ENERGY/ &
                                                (SEAS_HOURS*TEMPEA(2,I))
            ENDIF
!
            I = PA_UNIT_INDEX(2)
            IF(TEMPEA(2,I) == 0.) THEN
               PA_ENERGY(1,2) = 0.
            ELSE
               PA_ENERGY(1,2) = MONTHLY_ROXB4_ENERGY/ &
                                                (SEAS_HOURS*TEMPEA(2,I))
            ENDIF
!
            I = PA_UNIT_INDEX(3)
            IF(TEMPEA(2,I) == 0.) THEN
               PA_ENERGY(1,3) = 0.
            ELSE
               PA_ENERGY(1,3) = MONTHLY_BRUNSW1_ENERGY/ &
                                                (SEAS_HOURS*TEMPEA(2,I))
            ENDIF
!
            I = PA_UNIT_INDEX(4)
            IF(TEMPEA(2,I) == 0.) THEN
               PA_ENERGY(1,4) = 0.
            ELSE
               PA_ENERGY(1,4) = MONTHLY_BRUNSW2_ENERGY/ &
                                                (SEAS_HOURS*TEMPEA(2,I))
            ENDIF
!
            I = PA_UNIT_INDEX(5)
            IF(TEMPEA(2,I) == 0.) THEN
               PA_ENERGY(1,5) = 0.
            ELSE
               PA_ENERGY(1,5) = MONTHLY_HARRIS1_ENERGY/ &
                                                (SEAS_HOURS*TEMPEA(2,I))
            ENDIF
            PA_ENERGY(1,6) = MONTH_RES_ENERGY/(SEAS_HOURS)
            PA_ENERGY(1,7) = MONTH_UNUSED_SUPP_ENERGY/(SEAS_HOURS)
            PA_ENERGY(1,8) = MONTH_SUPP_ENERGY/(SEAS_HOURS)
            PA_ENERGY(1,9) = MONTH_CONTRACT_ENERGY/(SEAS_HOURS)
            PA_ENERGY(1,10) = MONTH_DEFICIENCY/(SEAS_HOURS)
         ENDIF ! HOURLY ECAP LOGIC
!
!
! PA REPORTING
!
!
         TRANSFER_COST = &
            (P_CLASS_ASSIGNED_COST(1) + &
             P_CLASS_ASS_ECON_COST(1) - P_CLASS_ASS_ECON_REV(1) + &
             P_CLASS_ASSIGNED_COST(2) + &
             P_CLASS_ASS_ECON_COST(2) - P_CLASS_ASS_ECON_REV(2))
         TRANSFER_GEN = &
            (P_CLASS_ASSIGNED_ENERGY(1) + P_CLASS_ASSIGNED_ENERGY(2) - &
             P_CLASS_ASS_ECON_SELL(1) + P_CLASS_ASS_ECON_BUY(1) - &
             P_CLASS_ASS_ECON_SELL(2) + P_CLASS_ASS_ECON_BUY(2) )
!
         CALL RETURN_CLASS_LOSSES(RS_CLASS_LOSSES, &
                                                 REMAINING_SYSTEM_CLASS)
!
         IF(USE_PA_ENERGY_PRICE()) THEN
            TRANSFER_PRICE = PA_ANN_AVE_PROD()
         ELSEIF(TRANSFER_GEN > 0.) THEN
            TRANSFER_PRICE = TRANSFER_COST / TRANSFER_GEN
         ELSE
            TRANSFER_PRICE = 0.
         ENDIF

         DEFICIT_ENERGY_COST = TRANSFER_PRICE * 1.25

         TOTAL_WRITE_ENERGY = 0.
         TOTAL_WRITE_CAPACITY = 0.
         TOTAL_PA_PRODUCTION_COST = 0.
         DO J = 0, NO_PA_RESOURCES + 1
!        INCLUDE TOTALS FOR SUPPLY AND DEMAND
            IF(J == 0) THEN
               WRITE_UNIT_NAME = 'PA Demand           '
               WRITE_CAPACITY = PA_PEAK
!
               WRITE_ENERGY = SNGL(PA_DEMAND)
               WRITE_PRODUCTION_COST = 0.
               AVERAGE_PRODUCTION_COSTS = 0.
               ANN_PA_ENERGY(J) = ANN_PA_ENERGY(J) + WRITE_ENERGY
               MON_PA_ENERGY(J,ISEAS) = MON_PA_ENERGY(J,ISEAS) + &
                                                            WRITE_ENERGY
               MON_PA_ENERGY(J,0) = MON_PA_ENERGY(J,0) + &
                                                            WRITE_ENERGY
               ANN_PA_CAP(J) = ANN_PA_CAP(J) + WRITE_CAPACITY
               MON_PA_CAP(J,ISEAS) = &
                                    MON_PA_CAP(J,ISEAS) + WRITE_CAPACITY
               MON_PA_CAP(J,0) = MON_PA_CAP(J,0) + WRITE_CAPACITY
            ELSEIF(J > 0 .AND. J <= NO_PA_UNITS) THEN
               I = PA_UNIT_INDEX(J)
               UNIT_ENRG = (  PA_ENERGY(1,J) * TEMPEA(1,I) + &
                              PA_ENERGY(2,J) * TEMPEA(2,I))
               WRITE_UNIT_NAME = UNITNM(I)(1:18)//'PA'
               WRITE_CAPACITY = &
                       (EFFECTIVE_CAPACITY(1,J)+EFFECTIVE_CAPACITY(2,J))
               IF(UNIT_ENRG > 0.) THEN
                  CALL CALHEAT(PA_ENERGY(1,J),LEFT(J),RIGHT(J), &
                        COEFF(1,I), &
                         MW(1,I),MMBTUS,EFFECTIVE_CAPACITY(1,J), &
                         TEMPEA(1,I),TEMPEA(2,I))
               ENDIF
               TOTAL_WRITE_ENERGY = TOTAL_WRITE_ENERGY + UNIT_ENRG
               TOTAL_WRITE_CAPACITY = TOTAL_WRITE_CAPACITY + &
                                                          WRITE_CAPACITY
               ANN_PA_ENERGY(J) = ANN_PA_ENERGY(J) + UNIT_ENRG &
                                                            * SEAS_HOURS
               MON_PA_ENERGY(J,ISEAS) = MON_PA_ENERGY(J,ISEAS) + &
                                                    UNIT_ENRG*SEAS_HOURS
               MON_PA_ENERGY(J,0) = MON_PA_ENERGY(J,0) + &
                                                    UNIT_ENRG*SEAS_HOURS
               ANN_PA_CAP(J) = ANN_PA_CAP(J) + WRITE_CAPACITY
               MON_PA_CAP(J,ISEAS) = &
                                    MON_PA_CAP(J,ISEAS) + WRITE_CAPACITY
               MON_PA_CAP(J,0) = MON_PA_CAP(J,0) + WRITE_CAPACITY
!
!
!
               WRITE_ENERGY = UNIT_ENRG*SEAS_HOURS
               IF(NINT(WRITE_ENERGY) > 0) THEN
!
                  SYS_ENERGY = (SYSTEM_ENERGY(1,I) + &
                                        SYSTEM_ENERGY(2,I)) * SEAS_HOURS
                  SYS_FUEL_COST = &
                             BLOCK_FUEL_COST(1,I) + BLOCK_FUEL_COST(2,I)
                  IF(SYS_ENERGY /= 0.) THEN
                     AVERAGE_FUEL_COST = SYS_FUEL_COST / SYS_ENERGY
                  ELSE
                     AVERAGE_FUEL_COST = SYS_FUEL_COST
                  ENDIF
                  WRITE_PRODUCTION_COST = &
                                       (AVERAGE_FUEL_COST + VCPMWH(I)) * &
                                                            WRITE_ENERGY
                  TOTAL_PA_PRODUCTION_COST = TOTAL_PA_PRODUCTION_COST + &
                                                   WRITE_PRODUCTION_COST
!
                  AVERAGE_PRODUCTION_COSTS = AVERAGE_FUEL_COST + &
                                                               VCPMWH(I)
               ELSE
                  WRITE_PRODUCTION_COST = 0.
                  AVERAGE_PRODUCTION_COSTS = 0.
               ENDIF
!
               ANN_PA_COST(J) = ANN_PA_COST(J) + WRITE_PRODUCTION_COST
               MON_PA_COST(J,ISEAS) = MON_PA_COST(J,ISEAS) + &
                                                   WRITE_PRODUCTION_COST
               MON_PA_COST(J,0) = MON_PA_COST(J,0) + &
                                                   WRITE_PRODUCTION_COST
!
            ELSEIF(J <= NO_PA_RESOURCES) THEN
               I = J - NO_PA_UNITS
               UNIT_ENRG = PA_ENERGY(1,J) + PA_ENERGY(2,J)
               WRITE_UNIT_NAME = CONTRACT_NAMES(I)
               IF(I == 1) THEN
                  WRITE_CAPACITY = RESERVE_CAPACITY
               ELSEIF(I== 2) THEN
                  WRITE_CAPACITY = UNUSED_SUPP_CAPACITY
               ELSEIF(I== 3) THEN
                  WRITE_CAPACITY = SUPPLEMENTAL_CAPACITY
               ELSEIF(I== 4) THEN
                  WRITE_CAPACITY = PA_CONTRACT_CAPACITY
               ELSEIF(I== 5) THEN
                  WRITE_CAPACITY = 0.
               ELSE
                  WRITE(4,*) '*** line 3050 CPL_PA_DISPATCH ***'
                  WRITE(4,*) "UNSUPPORTED PA CONTRACT TYPE"
                  er_message='See WARNING MESSAGES-cat2objt-4'
                  call end_program(er_message)
               ENDIF



               WRITE_ENERGY = UNIT_ENRG*SEAS_HOURS
               IF(NINT(WRITE_ENERGY) > 0) THEN

                  IF(I < 4) THEN
                     AVERAGE_PRODUCTION_COSTS = TRANSFER_PRICE
                     WRITE_PRODUCTION_COST = WRITE_ENERGY * &
                                                          TRANSFER_PRICE
                  ELSEIF(I == 4) THEN
                     AVERAGE_PRODUCTION_COSTS = PA_RESOURCE_ENERGY_COST
                     WRITE_PRODUCTION_COST = WRITE_ENERGY * &
                                                 PA_RESOURCE_ENERGY_COST
                  ELSEIF(I == 5) THEN
                     AVERAGE_PRODUCTION_COSTS = DEFICIT_ENERGY_COST
                     WRITE_PRODUCTION_COST = WRITE_ENERGY * &
                                                     DEFICIT_ENERGY_COST
                  ENDIF
!
               ELSE
                  WRITE_PRODUCTION_COST = 0.
                  AVERAGE_PRODUCTION_COSTS = 0.
               ENDIF
               TOTAL_PA_PRODUCTION_COST = TOTAL_PA_PRODUCTION_COST + &
                                                   WRITE_PRODUCTION_COST
!
               TOTAL_WRITE_ENERGY = TOTAL_WRITE_ENERGY + UNIT_ENRG
               TOTAL_WRITE_CAPACITY = TOTAL_WRITE_CAPACITY + &
                                                          WRITE_CAPACITY
               ANN_PA_ENERGY(J) = ANN_PA_ENERGY(J) + UNIT_ENRG &
                                                            * SEAS_HOURS
               MON_PA_ENERGY(J,ISEAS) = MON_PA_ENERGY(J,ISEAS) + &
                                                    UNIT_ENRG*SEAS_HOURS
               MON_PA_ENERGY(J,0) = MON_PA_ENERGY(J,0) + &
                                                    UNIT_ENRG*SEAS_HOURS
               ANN_PA_CAP(J) = ANN_PA_CAP(J) + WRITE_CAPACITY
               MON_PA_CAP(J,ISEAS) = &
                                    MON_PA_CAP(J,ISEAS) + WRITE_CAPACITY
               MON_PA_CAP(J,0) = MON_PA_CAP(J,0) + WRITE_CAPACITY
!
               ANN_PA_COST(J) = ANN_PA_COST(J) + WRITE_PRODUCTION_COST
               MON_PA_COST(J,ISEAS) = MON_PA_COST(J,ISEAS) + &
                                                   WRITE_PRODUCTION_COST
               MON_PA_COST(J,0) = MON_PA_COST(J,0) + &
                                                   WRITE_PRODUCTION_COST
!
            ELSE
               WRITE_UNIT_NAME = 'PA Supply           '
               WRITE_CAPACITY = TOTAL_WRITE_CAPACITY
               WRITE_ENERGY = TOTAL_WRITE_ENERGY * SEAS_HOURS
               IF(WRITE_ENERGY > 0.) THEN
                  WRITE_PRODUCTION_COST = TOTAL_PA_PRODUCTION_COST
                  AVERAGE_PRODUCTION_COSTS = TOTAL_PA_PRODUCTION_COST / &
                                                            WRITE_ENERGY
               ELSE
                  WRITE_PRODUCTION_COST = 0.
                  AVERAGE_PRODUCTION_COSTS = 0.
               ENDIF
               ANN_PA_ENERGY(J) = ANN_PA_ENERGY(J) + WRITE_ENERGY
               MON_PA_ENERGY(J,ISEAS) = MON_PA_ENERGY(J,ISEAS) + &
                                                            WRITE_ENERGY
               MON_PA_ENERGY(J,0) = MON_PA_ENERGY(J,0) + WRITE_ENERGY
               ANN_PA_CAP(J) = ANN_PA_CAP(J) + WRITE_CAPACITY
               MON_PA_CAP(J,ISEAS) = &
                                    MON_PA_CAP(J,ISEAS) + WRITE_CAPACITY
               MON_PA_CAP(J,0) = MON_PA_CAP(J,0) + WRITE_CAPACITY
               ANN_PA_COST(J) = ANN_PA_COST(J) + &
                                                TOTAL_PA_PRODUCTION_COST
               MON_PA_COST(J,ISEAS) = MON_PA_COST(J,ISEAS) + &
                                                   WRITE_PRODUCTION_COST
               MON_PA_COST(J,0) = MON_PA_COST(J,0) + &
                                                   WRITE_PRODUCTION_COST
            ENDIF
!
            CF = WRITE_CAPACITY*SEAS_HOURS
            IF(CF > 0.) THEN
               CF = 100.*WRITE_ENERGY/CF
            ELSE
               CF = 0.
            ENDIF
!
            WRITE(CPL_PAEN_UNIT,REC=CPL_PAEN_REC) &
                     PRT_ENDPOINT(), &
                     FLOAT(CURRENT_year_loc), &
                     CL_MONTH_NAME(ISEAS), &
                     WRITE_UNIT_NAME, &
                     WRITE_CAPACITY, &
                     WRITE_ENERGY, &
                     CF, &
                     WRITE_PRODUCTION_COST/1000., &
                     AVERAGE_PRODUCTION_COSTS
            CPL_PAEN_REC = CPL_PAEN_REC + 1
         ENDDO
!
!
         DEALLOCATE(EFFECTIVE_CAPACITY,LEFT,RIGHT)


         IF(NO_PA_UNITS <= 0) RETURN
         TOTAL_ENT_CAPACITY = 0.
         TOTAL_ENT_ENERGY = 0.
         TOTAL_ENT_FUEL_COST = 0.
         TOTAL_ENT_PRODUCTION_COST = 0.
         REPLACEMENT_ENERGY = 0.
         SURPLUS_ENERGY = 0.
         SURPLUS_PROD_COST = 0.
         MAYO1_REPLACEMENT = 0.
         ROX4_REPLACEMENT = 0.
         MAYO1_BUYBACK = 0.
         HARRIS_BUYBACK = 0.
         BUYBACK_ENERGY = 0.
         DO J = 1, NO_PA_UNITS
            I = PA_UNIT_INDEX(J)
            WRITE_PRODUCTION_COST = 0.
            WRITE_FUEL_COST = 0.
            IF(J == 1) THEN
               IF(CURRENT_year_loc <= 1997) THEN
                  RETAINED_PERCENT = .1563
               ELSE
                  RETAINED_PERCENT = .1617
               ENDIF
            ELSEIF(J < 5) THEN
               RETAINED_PERCENT =  (1.-CL_POOL_FRAC_OWN(I)/100.)
            ELSEIF(J == 5) THEN
               IF(CURRENT_year_loc <= 2007) THEN
                  RETAINED_PERCENT = .1078
               ELSE
                  RETAINED_PERCENT = .1617
               ENDIF
            ENDIF
!
            SYS_ENERGY = (SYSTEM_ENERGY(1,I) + &
                                        SYSTEM_ENERGY(2,I)) * SEAS_HOURS
            WRITE_ENERGY = SYS_ENERGY * RETAINED_PERCENT
!
            IF(NINT(SYS_ENERGY) > 0) THEN
!
               SYS_FUEL_COST = &
                             BLOCK_FUEL_COST(1,I) + BLOCK_FUEL_COST(2,I)
               WRITE_FUEL_COST = SYS_FUEL_COST * RETAINED_PERCENT
               WRITE_PRODUCTION_COST = WRITE_FUEL_COST + VCPMWH(I) * &
                                                            WRITE_ENERGY
!
               AVERAGE_FUEL_COST = SYS_FUEL_COST / SYS_ENERGY
               AVERAGE_PRODUCTION_COSTS =  AVERAGE_FUEL_COST + VCPMWH(I)
            ELSE
               WRITE_FUEL_COST = 0.
               AVERAGE_FUEL_COST = 0.
               AVERAGE_PRODUCTION_COSTS = 0.
            ENDIF
            WRITE_FUEL_COST = WRITE_FUEL_COST + WRITE_ENERGY * &
                                                FUEL_ADDER_ADJUSTMENT(I)
!
!
!
            WRITE_CAPACITY = (SYS_EFFECTIVE_CAPACITY(1,I) + &
                              SYS_EFFECTIVE_CAPACITY(2,I))* &
                                                        RETAINED_PERCENT
            TOTAL_ENT_ENERGY = TOTAL_ENT_ENERGY + WRITE_ENERGY
            TOTAL_ENT_CAPACITY = TOTAL_ENT_CAPACITY + WRITE_CAPACITY
!
            TOTAL_ENT_FUEL_COST = TOTAL_ENT_FUEL_COST + WRITE_FUEL_COST
            TOTAL_ENT_PRODUCTION_COST = TOTAL_ENT_PRODUCTION_COST + &
                                                   WRITE_PRODUCTION_COST
!
            UNIT_ENRG = (PA_ENERGY(1,J) + PA_ENERGY(2,J)) * SEAS_HOURS
!
            IF(J == 1) THEN
               ! THIS NEEDS TO BE REPLACED WITH SPECIAL UNIT ID !!!
               IF(UNIT_ENRG > WRITE_ENERGY ) THEN
                  MAYO1_REPLACEMENT =  UNIT_ENRG - WRITE_ENERGY
                  REPLACEMENT_ENERGY = REPLACEMENT_ENERGY + &
                                                       MAYO1_REPLACEMENT
                  MAYO1_RPL_PROD_COST = WRITE_PRODUCTION_COST
                  MAYO1_RPL_AVE_COST = AVERAGE_PRODUCTION_COSTS
               ELSE
                  SURPLUS_ENERGY = SURPLUS_ENERGY + &
                                                WRITE_ENERGY - UNIT_ENRG
                  SURPLUS_PROD_COST = SURPLUS_PROD_COST + &
                                            (WRITE_ENERGY - UNIT_ENRG) * &
                                                AVERAGE_PRODUCTION_COSTS
!
                  MAYO1_RPL_PROD_COST = 0.
                  MAYO1_RPL_AVE_COST = 0.
               ENDIF
!
               IF(CURRENT_year_loc <= 1997) THEN
                  MAYO1_BUYBACK = &
                     (SYSTEM_ENERGY(1,I) + &
                            SYSTEM_ENERGY(2,I)) * &
                                      (1.-CL_POOL_FRAC_OWN(I)/100.) * &
                                                 MAYO1_BUYBACK_PERCENT * &
                                                              SEAS_HOURS
                  BUYBACK_ENERGY = BUYBACK_ENERGY + MAYO1_BUYBACK
               ELSE
                  MAYO1_BUYBACK = 0.
               ENDIF
!
               MAYO1_BUYBACK_PROD_COST = MAYO1_BUYBACK * &
                                                AVERAGE_PRODUCTION_COSTS
               IF(MAYO1_BUYBACK > 0.) THEN
                  MAYO1_BUYBACK_AVE_COST = AVERAGE_PRODUCTION_COSTS
               ELSE
                  MAYO1_BUYBACK_AVE_COST = 0.
               ENDIF
            ELSEIF(J == 2) THEN
               IF(UNIT_ENRG > WRITE_ENERGY ) THEN
                  ROX4_REPLACEMENT =  UNIT_ENRG - WRITE_ENERGY
                  REPLACEMENT_ENERGY = REPLACEMENT_ENERGY + &
                                                        ROX4_REPLACEMENT
                  ROX4_RPL_PROD_COST = ROX4_REPLACEMENT * &
                                                AVERAGE_PRODUCTION_COSTS
                  ROX4_RPL_AVE_COST = AVERAGE_PRODUCTION_COSTS
               ELSE
                  SURPLUS_ENERGY = SURPLUS_ENERGY + &
                                                WRITE_ENERGY - UNIT_ENRG
                  SURPLUS_PROD_COST = SURPLUS_PROD_COST + &
                                            (WRITE_ENERGY - UNIT_ENRG) * &
                                                AVERAGE_PRODUCTION_COSTS
!
                  ROX4_RPL_PROD_COST = 0.
                  ROX4_RPL_AVE_COST = 0.
               ENDIF
!
            ELSEIF(J == 5) THEN
               IF(CURRENT_year_loc <= 2007) THEN
                  HARRIS_BUYBACK = &
                     (SYSTEM_ENERGY(1,I) + &
                            SYSTEM_ENERGY(2,I)) * &
                                      (1.-CL_POOL_FRAC_OWN(I)/100.) * &
                                               HARRIS1_BUYBACK_PERCENT * &
                                                              SEAS_HOURS
               ELSE
                  HARRIS_BUYBACK = 0.
               ENDIF
               BUYBACK_ENERGY = BUYBACK_ENERGY + HARRIS_BUYBACK
               HARRIS_BUYBACK_PROD_COST = HARRIS_BUYBACK * &
                                                AVERAGE_PRODUCTION_COSTS
               IF(HARRIS_BUYBACK > 0.) THEN
                  HARRIS_BUYBACK_AVE_COST = AVERAGE_PRODUCTION_COSTS
               ELSE
                  HARRIS_BUYBACK_AVE_COST = 0.
               ENDIF
            ENDIF
            ANN_SYS_ENERGY(J) = ANN_SYS_ENERGY(J) + WRITE_ENERGY
            MON_SYS_ENERGY(J,ISEAS) = MON_SYS_ENERGY(J,ISEAS) + &
                                                            WRITE_ENERGY
            MON_SYS_ENERGY(J,0) = MON_SYS_ENERGY(J,0) + WRITE_ENERGY
            ANN_SYS_CAP(J) = ANN_SYS_CAP(J) + WRITE_CAPACITY
            ANN_SYS_COST(J) = ANN_SYS_COST(J) + WRITE_PRODUCTION_COST
            MON_SYS_COST(J,ISEAS) = MON_SYS_COST(J,ISEAS) + &
                                                   WRITE_PRODUCTION_COST
            MON_SYS_COST(J,0) = MON_SYS_COST(J,0) + &
                                                   WRITE_PRODUCTION_COST
!
            WRITE_UNIT_NAME = UNITNM(I)(1:17)//'ENT'
            CF = WRITE_CAPACITY*SEAS_HOURS
            IF(CF > 0.) THEN
               CF = 100.*WRITE_ENERGY/CF
            ELSE
               CF = 0.
            ENDIF
!!
            WRITE(CPL_PAEN_UNIT,REC=CPL_PAEN_REC) &
                     PRT_ENDPOINT(), &
                     FLOAT(CURRENT_year_loc), &
                     CL_MONTH_NAME(ISEAS), &
                     WRITE_UNIT_NAME, &
                     WRITE_CAPACITY, &
                     WRITE_ENERGY, &
                     CF, &
                     WRITE_PRODUCTION_COST/1000., &
                     AVERAGE_PRODUCTION_COSTS
            CPL_PAEN_REC = CPL_PAEN_REC + 1
!
            IF(J == 4 .OR. J == 7) THEN
               CPL_NCEMPA_MWH_SUPPLIED_BY_CPL(ISEAS) = &
                           CPL_NCEMPA_MWH_SUPPLIED_BY_CPL(ISEAS) + &
                                                   WRITE_PRODUCTION_COST
               CPL_NCEMPA_MWH_SUPPLIED_BY_CPL(0) = &
                           CPL_NCEMPA_MWH_SUPPLIED_BY_CPL(0) + &
                                                   WRITE_PRODUCTION_COST
               CPL_NCEMPA_COST_MWH_SUP_BY_CPL(ISEAS) = &
                    CPL_NCEMPA_COST_MWH_SUP_BY_CPL(ISEAS) + WRITE_ENERGY
               CPL_NCEMPA_COST_MWH_SUP_BY_CPL(0) = &
                        CPL_NCEMPA_COST_MWH_SUP_BY_CPL(0) + WRITE_ENERGY
            ENDIF
!
         ENDDO ! J = 1, NO_PA_UNITS
!
         CF = TOTAL_ENT_CAPACITY*SEAS_HOURS
         IF(CF > 0.) THEN
            CF = 100.*TOTAL_ENT_ENERGY/CF
         ELSE
            CF = 0.
         ENDIF
         IF(TOTAL_ENT_ENERGY > 0.) THEN
            AVERAGE_PRODUCTION_COSTS = TOTAL_ENT_PRODUCTION_COST/ &
                                                        TOTAL_ENT_ENERGY
         ELSE
            AVERAGE_PRODUCTION_COSTS = 0.
         ENDIF
!
         CPL_NCEMPA_ENTITLEMENT(ISEAS) = CPL_NCEMPA_ENTITLEMENT(ISEAS) + &
                                               TOTAL_ENT_PRODUCTION_COST
!
         CPL_NCEMPA_ENTITLEMENT(0) = CPL_NCEMPA_ENTITLEMENT(0) + &
                                               TOTAL_ENT_PRODUCTION_COST
!
         CPL_NCEMPA_ENTITLEMENT_MWH(ISEAS) = &
                    CPL_NCEMPA_ENTITLEMENT_MWH(ISEAS) + TOTAL_ENT_ENERGY
         CPL_NCEMPA_ENTITLEMENT_MWH(0) = &
                    CPL_NCEMPA_ENTITLEMENT_MWH(0) + TOTAL_ENT_ENERGY
!
         OFF_SYSTEM_SALES_ENERGY(ISEAS) = &
                       FORECAST_ENERGY(1,ISEAS,OFF_SYSTEM_SALES_CLASS) &
                       + FORECAST_ENERGY(2,ISEAS,OFF_SYSTEM_SALES_CLASS)
         OFF_SYSTEM_SALES_ENERGY(0) = OFF_SYSTEM_SALES_ENERGY(0) + &
                                          OFF_SYSTEM_SALES_ENERGY(ISEAS)
         CPL_OFF_SYSTEM_INCR_COST(ISEAS) = &
                  CPL_OFF_SYSTEM_INCR_COST(ISEAS) + &
               OFF_SYSTEM_SALES_ENERGY(ISEAS) * AVERAGE_PRODUCTION_COSTS
         CPL_OFF_SYSTEM_INCR_COST(0) = &
                  CPL_OFF_SYSTEM_INCR_COST(0) + &
               OFF_SYSTEM_SALES_ENERGY(ISEAS) * AVERAGE_PRODUCTION_COSTS
!
         WRITE(CPL_PAEN_UNIT,REC=CPL_PAEN_REC) &
                     PRT_ENDPOINT(), &
                     FLOAT(CURRENT_year_loc), &
                     CL_MONTH_NAME(ISEAS), &
                     'Total Entitlement   ', &
                     TOTAL_ENT_CAPACITY, &
                     TOTAL_ENT_ENERGY, &
                     CF, &
                     TOTAL_ENT_PRODUCTION_COST/1000., &
                     AVERAGE_PRODUCTION_COSTS
         CPL_PAEN_REC = CPL_PAEN_REC + 1
!
         J = NO_PA_UNITS
         J = J + 1
         ANN_SYS_ENERGY(J) = ANN_SYS_ENERGY(J) + MAYO1_REPLACEMENT
         ANN_SYS_COST(J) = ANN_SYS_COST(J) + MAYO1_RPL_PROD_COST
!
         MON_SYS_ENERGY(J,ISEAS) = MON_SYS_ENERGY(J,ISEAS) + &
                                                       MAYO1_REPLACEMENT
         MON_SYS_COST(J,ISEAS) = MON_SYS_COST(J,ISEAS) + &
                                                     MAYO1_RPL_PROD_COST
         MON_SYS_ENERGY(J,0) = MON_SYS_ENERGY(J,0) + &
                                                       MAYO1_REPLACEMENT
         MON_SYS_COST(J,0) = MON_SYS_COST(J,0) +  MAYO1_RPL_PROD_COST
!
!
         WRITE(CPL_PAEN_UNIT,REC=CPL_PAEN_REC) &
                     PRT_ENDPOINT(), &
                     FLOAT(CURRENT_year_loc), &
                     CL_MONTH_NAME(ISEAS), &
                     PA_ACCT_NAMES(1), &
                     0., &
                     MAYO1_REPLACEMENT, &
                     0., &
                     MAYO1_RPL_PROD_COST/1000., &
                     MAYO1_RPL_AVE_COST
         CPL_PAEN_REC = CPL_PAEN_REC + 1
!
         J = J + 1
         ANN_SYS_ENERGY(J) = ANN_SYS_ENERGY(J) + ROX4_REPLACEMENT
         ANN_SYS_COST(J) = ANN_SYS_COST(J) + ROX4_RPL_PROD_COST
!
         MON_SYS_ENERGY(J,ISEAS) = MON_SYS_ENERGY(J,ISEAS) + &
                                                        ROX4_REPLACEMENT
         MON_SYS_COST(J,ISEAS) = MON_SYS_COST(J,ISEAS) + &
                                                      ROX4_RPL_PROD_COST
         MON_SYS_ENERGY(J,0) = MON_SYS_ENERGY(J,0) + &
                                                        ROX4_REPLACEMENT
         MON_SYS_COST(J,0) = MON_SYS_COST(J,0) + ROX4_RPL_PROD_COST
!
         WRITE(CPL_PAEN_UNIT,REC=CPL_PAEN_REC) &
                     PRT_ENDPOINT(), &
                     FLOAT(CURRENT_year_loc), &
                     CL_MONTH_NAME(ISEAS), &
                     PA_ACCT_NAMES(2), &
                     0., &
                     ROX4_REPLACEMENT, &
                     0., &
                     ROX4_RPL_PROD_COST/1000., &
                     ROX4_RPL_AVE_COST
         CPL_PAEN_REC = CPL_PAEN_REC + 1
!
         REPLACEMENT_PROD_COST = MAYO1_RPL_PROD_COST + &
                                                      ROX4_RPL_PROD_COST
         IF(REPLACEMENT_ENERGY > 0.) THEN
            REPLACEMENT_AVE_COST = REPLACEMENT_PROD_COST / &
                                                      REPLACEMENT_ENERGY
         ELSE
            REPLACEMENT_AVE_COST = 0.
         ENDIF
!
!
         J = J + 1
         ANN_SYS_ENERGY(J) = ANN_SYS_ENERGY(J) + REPLACEMENT_ENERGY
         ANN_SYS_COST(J) = ANN_SYS_COST(J) + REPLACEMENT_PROD_COST
!
         MON_SYS_ENERGY(J,ISEAS) = MON_SYS_ENERGY(J,ISEAS) + &
                                                      REPLACEMENT_ENERGY
         MON_SYS_COST(J,ISEAS) = MON_SYS_COST(J,ISEAS) + &
                                                   REPLACEMENT_PROD_COST
         MON_SYS_ENERGY(J,0) = MON_SYS_ENERGY(J,0) + &
                                                      REPLACEMENT_ENERGY
         MON_SYS_COST(J,0) = MON_SYS_COST(J,0) + REPLACEMENT_PROD_COST
!
         WRITE(CPL_PAEN_UNIT,REC=CPL_PAEN_REC) &
                     PRT_ENDPOINT(), &
                     FLOAT(CURRENT_year_loc), &
                     CL_MONTH_NAME(ISEAS), &
                     PA_ACCT_NAMES(3), &
                     0., &
                     REPLACEMENT_ENERGY, &
                     0., &
                     REPLACEMENT_PROD_COST/1000., &
                     REPLACEMENT_AVE_COST
         CPL_PAEN_REC = CPL_PAEN_REC + 1
!
         IF(SURPLUS_ENERGY > 0.) THEN
            SURPLUS_AVE_COST = SURPLUS_PROD_COST / SURPLUS_ENERGY
         ELSE
            SURPLUS_AVE_COST = 0.
         ENDIF
!
!
         J = J + 1
         ANN_SYS_ENERGY(J) = ANN_SYS_ENERGY(J) + SURPLUS_ENERGY
         ANN_SYS_COST(J) = ANN_SYS_COST(J) + SURPLUS_PROD_COST
!
         MON_SYS_ENERGY(J,ISEAS) = MON_SYS_ENERGY(J,ISEAS) + &
                                                          SURPLUS_ENERGY
         MON_SYS_COST(J,ISEAS) = MON_SYS_COST(J,ISEAS) + &
                                                       SURPLUS_PROD_COST
         MON_SYS_ENERGY(J,0) = MON_SYS_ENERGY(J,0) + SURPLUS_ENERGY
         MON_SYS_COST(J,0) = MON_SYS_COST(J,0) + SURPLUS_PROD_COST
!
         WRITE(CPL_PAEN_UNIT,REC=CPL_PAEN_REC) &
                     PRT_ENDPOINT(), &
                     FLOAT(CURRENT_year_loc), &
                     CL_MONTH_NAME(ISEAS), &
                     PA_ACCT_NAMES(4), &
                     0., &
                     SURPLUS_ENERGY, &
                     0., &
                     SURPLUS_PROD_COST/1000., &
                     SURPLUS_AVE_COST
         CPL_PAEN_REC = CPL_PAEN_REC + 1
!
!
         J = J + 1
         ANN_SYS_ENERGY(J) = ANN_SYS_ENERGY(J) + HARRIS_BUYBACK
         ANN_SYS_COST(J) = ANN_SYS_COST(J) + HARRIS_BUYBACK_PROD_COST
!
         MON_SYS_ENERGY(J,ISEAS) = MON_SYS_ENERGY(J,ISEAS) + &
                                                          HARRIS_BUYBACK
         MON_SYS_COST(J,ISEAS) = MON_SYS_COST(J,ISEAS) + &
                                                HARRIS_BUYBACK_PROD_COST
         MON_SYS_ENERGY(J,0) = MON_SYS_ENERGY(J,0) + HARRIS_BUYBACK
         MON_SYS_COST(J,0) = MON_SYS_COST(J,0) + &
                                                HARRIS_BUYBACK_PROD_COST
!
         WRITE(CPL_PAEN_UNIT,REC=CPL_PAEN_REC) &
                     PRT_ENDPOINT(), &
                     FLOAT(CURRENT_year_loc), &
                     CL_MONTH_NAME(ISEAS), &
                     PA_ACCT_NAMES(5), &
                     0., &
                     HARRIS_BUYBACK, &
                     0., &
                     HARRIS_BUYBACK_PROD_COST/1000., &
                     HARRIS_BUYBACK_AVE_COST
         CPL_PAEN_REC = CPL_PAEN_REC + 1
!
!
         J = J + 1
         ANN_SYS_ENERGY(J) = ANN_SYS_ENERGY(J) + MAYO1_BUYBACK
         ANN_SYS_COST(J) = ANN_SYS_COST(J) + MAYO1_BUYBACK_PROD_COST
!
         MON_SYS_ENERGY(J,ISEAS) = MON_SYS_ENERGY(J,ISEAS) + &
                                                           MAYO1_BUYBACK
         MON_SYS_COST(J,ISEAS) = MON_SYS_COST(J,ISEAS) + &
                                                 MAYO1_BUYBACK_PROD_COST
         MON_SYS_ENERGY(J,0) = MON_SYS_ENERGY(J,0) + MAYO1_BUYBACK
         MON_SYS_COST(J,0) = MON_SYS_COST(J,0) + MAYO1_BUYBACK_PROD_COST
!
         WRITE(CPL_PAEN_UNIT,REC=CPL_PAEN_REC) &
                     PRT_ENDPOINT(), &
                     FLOAT(CURRENT_year_loc), &
                     CL_MONTH_NAME(ISEAS), &
                     PA_ACCT_NAMES(6), &
                     0., &
                     MAYO1_BUYBACK, &
                     0., &
                     MAYO1_BUYBACK_PROD_COST/1000., &
                     MAYO1_BUYBACK_AVE_COST
         CPL_PAEN_REC = CPL_PAEN_REC + 1
!
         BUYBACK_PROD_COST = MAYO1_BUYBACK_PROD_COST + &
                                                HARRIS_BUYBACK_PROD_COST
         IF(BUYBACK_ENERGY > 0.) THEN
            BUYBACK_AVE_COST = BUYBACK_PROD_COST / BUYBACK_ENERGY
         ELSE
            BUYBACK_AVE_COST = 0.
         ENDIF
!
         J = J + 1
         ANN_SYS_ENERGY(J) = ANN_SYS_ENERGY(J) + BUYBACK_ENERGY
         ANN_SYS_COST(J) = ANN_SYS_COST(J) + BUYBACK_PROD_COST
!
         MON_SYS_ENERGY(J,ISEAS) = MON_SYS_ENERGY(J,ISEAS) + &
                                                          BUYBACK_ENERGY
         MON_SYS_COST(J,ISEAS) = MON_SYS_COST(J,ISEAS) + &
                                                       BUYBACK_PROD_COST
         MON_SYS_ENERGY(J,0) = MON_SYS_ENERGY(J,0) + BUYBACK_ENERGY
         MON_SYS_COST(J,0) = MON_SYS_COST(J,0) + BUYBACK_PROD_COST
!
         WRITE(CPL_PAEN_UNIT,REC=CPL_PAEN_REC) &
                     PRT_ENDPOINT(), &
                     FLOAT(CURRENT_year_loc), &
                     CL_MONTH_NAME(ISEAS), &
                     PA_ACCT_NAMES(7), &
                     0., &
                     BUYBACK_ENERGY, &
                     0., &
                     BUYBACK_PROD_COST/1000., &
                     BUYBACK_AVE_COST
         CPL_PAEN_REC = CPL_PAEN_REC + 1
!
! EMC ENERGY REVENUES
!
         EMC_COST = TRANSFER_COST - TOTAL_ENT_PRODUCTION_COST
         TRANSFER_SALES = TRANSFER_GEN * &
                       (1. - MIN(RS_CLASS_LOSSES,.9999)) - &
                                                      SEPA_ENERGY(ISEAS)
         EMC_GEN = TRANSFER_SALES - TOTAL_ENT_ENERGY
         IF(USE_EMC_ENERGY_PRICE()) THEN
             EMC_PRICE = EMC_ANN_AVE_PROD()
         ELSEIF(EMC_GEN > 0.) THEN
            EMC_PRICE = EMC_COST / EMC_GEN
         ELSE
            EMC_PRICE = 0.
         ENDIF
         CALL GET_CPL_EMC_MON_ENER_REVS(ISEAS,EMC_PRICE, &
                                          WRITE_ENERGY,WRITE_CAPACITY, &
                                          WRITE_PRODUCTION_COST)
!
!
         ! TRANSFER_PRICE WILL BE MODIFIED
         AVERAGE_PRODUCTION_COSTS = EMC_PRICE
!
         CF = WRITE_CAPACITY*SEAS_HOURS
         IF(CF > 0.) THEN
            CF = 100.*WRITE_ENERGY/CF
         ELSE
            CF = 0.
         ENDIF
!
         WRITE(CPL_PAEN_UNIT,REC=CPL_PAEN_REC) &
                     PRT_ENDPOINT(), &
                     FLOAT(CURRENT_year_loc), &
                     CL_MONTH_NAME(ISEAS), &
                     'EMC Energy Revenues ', &
                     WRITE_CAPACITY, &
                     WRITE_ENERGY, &
                     CF, &
                     WRITE_PRODUCTION_COST/1000., &
                     AVERAGE_PRODUCTION_COSTS
         CPL_PAEN_REC = CPL_PAEN_REC + 1
!
         CALL CPL_MON_ENERGY_REV_AND_EXP(ISEAS, &
                                         S_CATAWBA_REVENUES, &
                                         S_CPL_ENERGY_TO_PA, &
                                         S_CATAWBA_EXPENSES, &
                                         S_CPL_ENERGY_FROM_PA)
!
!
! EMC ENERGY REVENUES
!
         WRITE_UNIT_NAME = 'MWHs Supplied by CPL'
         WRITE_CAPACITY = 0.
         CF = 0.
         WRITE_ENERGY = S_CPL_ENERGY_TO_PA
         WRITE_PRODUCTION_COST = S_CATAWBA_REVENUES
         IF(WRITE_ENERGY > 0.) THEN
            AVERAGE_PRODUCTION_COSTS = WRITE_PRODUCTION_COST/ &
                                           WRITE_ENERGY
         ELSE
            AVERAGE_PRODUCTION_COSTS = 0.
         ENDIF
         WRITE(CPL_PAEN_UNIT,REC=CPL_PAEN_REC) &
                     PRT_ENDPOINT(), &
                     FLOAT(CURRENT_year_loc), &
                     CL_MONTH_NAME(ISEAS), &
                     WRITE_UNIT_NAME, &
                     WRITE_CAPACITY, &
                     WRITE_ENERGY, &
                     CF, &
                     WRITE_PRODUCTION_COST/1000., &
                     AVERAGE_PRODUCTION_COSTS
         CPL_PAEN_REC = CPL_PAEN_REC + 1
!
         WRITE_UNIT_NAME = 'MWHs Supplied to CPL'
         WRITE_CAPACITY = 0.
         CF = 0.
!
         WRITE_ENERGY = MON_SYS_ENERGY(4+NO_PA_UNITS,ISEAS) + &
                                     MON_SYS_ENERGY(7+NO_PA_UNITS,ISEAS)
!
         WRITE_PRODUCTION_COST = S_CATAWBA_EXPENSES
         IF(WRITE_ENERGY > 0.) THEN
            AVERAGE_PRODUCTION_COSTS = WRITE_PRODUCTION_COST / &
                                                            WRITE_ENERGY
         ELSE
            AVERAGE_PRODUCTION_COSTS = 0.
         ENDIF
         WRITE(CPL_PAEN_UNIT,REC=CPL_PAEN_REC) &
                     PRT_ENDPOINT(), &
                     FLOAT(CURRENT_year_loc), &
                     CL_MONTH_NAME(ISEAS), &
                     WRITE_UNIT_NAME, &
                     WRITE_CAPACITY, &
                     WRITE_ENERGY, &
                     CF, &
                     WRITE_PRODUCTION_COST/1000., &
                     AVERAGE_PRODUCTION_COSTS
         CPL_PAEN_REC = CPL_PAEN_REC + 1
      RETURN

!
      ENTRY CPL_SC_RETAIL(R_ISEAS, &
                          R_CPL_GEN_COST, &
                          R_CPL_NUC_COST, &
                          R_CPL_PA_ENTITLE, &
                          R_CPL_PUR_POWER, &
                          R_CPL_PA_MWH_COST, &
                          R_CPL_PA_MWH, &
                          R_CPL_OFF_SYS_ENRG, &
                          R_CPL_OFF_SYS_INC_COST)
!
! FOR MONTHLY MIDAS



         R_CPL_GEN_COST = CPL_FUEL_GENERATION(R_ISEAS)
         R_CPL_NUC_COST = CPL_NUC_GENERATION(R_ISEAS)
         R_CPL_PUR_POWER = CPL_PUR_POWER_ENERGY_COST(R_ISEAS)
         R_CPL_OFF_SYS_ENRG = OFF_SYSTEM_SALES_ENERGY(R_ISEAS)

! 8/5/99. GAT.

      CALL GET_PA_SWITCH(PA_SWITCH)
      IF(.NOT. PA_SWITCH) THEN

         R_CPL_PA_ENTITLE = 0.

         R_CPL_OFF_SYS_INC_COST = 0.

         R_CPL_PA_MWH_COST = 0.

         R_CPL_PA_MWH = 0.

      ELSE
         R_CPL_PA_ENTITLE = CPL_NCEMPA_ENTITLEMENT(R_ISEAS)
         IF(MON_PA_ENERGY(SupplementalCap,R_ISEAS) > 0.) THEN
            R_CPL_OFF_SYS_INC_COST = &
                              MON_PA_COST(SupplementalCap,R_ISEAS)/ &
                                 MON_PA_ENERGY(SupplementalCap,R_ISEAS)
         ELSE
            R_CPL_OFF_SYS_INC_COST = 0.
         ENDIF
!
         R_CPL_PA_MWH_COST = MON_PA_COST(ReserveCapacity,R_ISEAS) + &
                             MON_PA_COST(SupplementalCap,R_ISEAS) + &
                             MON_PA_COST(PAResource,R_ISEAS) + &
                             MON_PA_COST(DeficiencyCapacity,R_ISEAS) + &
                             MON_SYS_COST(Mayo1Replacement,R_ISEAS) + &
                             MON_SYS_COST(Roxboro4Replacement,R_ISEAS)
!
         R_CPL_PA_MWH = MON_PA_ENERGY(ReserveCapacity,R_ISEAS) + &
                        MON_PA_ENERGY(SupplementalCap,R_ISEAS) + &
                        MON_PA_ENERGY(PAResource,R_ISEAS) + &
                        MON_PA_ENERGY(DeficiencyCapacity,R_ISEAS) + &
                        MON_SYS_ENERGY(Mayo1Replacement,R_ISEAS) + &
                        MON_SYS_ENERGY(Roxboro4Replacement,R_ISEAS)
      ENDIF
      RETURN

!
      ENTRY CPL_PA_ENTITLEMENT_MWH( R_ISEAS, &
                                    R_CPL_PA_ENTITLE, &
                                    R_BUYBACK)


      CALL GET_PA_SWITCH(PA_SWITCH)
      IF(.NOT. PA_SWITCH) THEN
         R_CPL_PA_ENTITLE = 0.
         R_BUYBACK = 0.
      ELSE
         R_CPL_PA_ENTITLE = CPL_NCEMPA_ENTITLEMENT_MWH(R_ISEAS)/1000.
         I = 12
         R_BUYBACK = MON_SYS_ENERGY(I,R_ISEAS)/1000.
      ENDIF


      RETURN

      ENTRY CPL_PA_ANN_CAPACITY ! CURRENTLY HARD-CODED


!        CAPACITY REVENUE AND EXPENSE CALC'S

         CALL GET_PA_SWITCH(PA_SWITCH)
         IF(.NOT. PA_SWITCH) THEN


! EMC STUFF
!
            CALL GET_CPL_EMC_ANN_ENER_REVS( WRITE_ENERGY, &
                                         WRITE_CAPACITY, &
                                         WRITE_PRODUCTION_COST)
            CPL_EMC_REVENUE(0) = CPL_EMC_REVENUE(0) + &
                                                   WRITE_PRODUCTION_COST
!
            CALL GET_CPL_EMC_ANN_TRAN_REVS(  WRITE_CAPACITY, &
                                          WRITE_PRODUCTION_COST, &
                                          CURRENT_year_loc)
            CPL_EMC_REVENUE(0) = CPL_EMC_REVENUE(0) + &
                                                   WRITE_PRODUCTION_COST
!
            CALL GET_CPL_EMC_ANN_CAP_REVS(  WRITE_ENERGY, &
                                         WRITE_CAPACITY, &
                                         WRITE_PRODUCTION_COST)
            CPL_EMC_REVENUE(0) = CPL_EMC_REVENUE(0) + &
                                                   WRITE_PRODUCTION_COST
!
            RETURN
!
         ENDIF
!
         CALL GET_PA_CAPACITY_RATES(   PA_RESERVE_RATE, &
                                       PA_SUPPLEMENTAL_RATE, &
                                       PA_TRANSMISSION_RATE)
         DO J = 1, 3, 2
            WRITE_CAPACITY = ANN_PA_CAP(J+NO_PA_UNITS)
            WRITE_UNIT_NAME = CONTRACT_NAMES(J)(1:17)//'Cap'
            WRITE_ENERGY = 0.
            CF = 0.
!
            IF(J == 1) THEN
               CALL GET_ANN_RESERVE_CAPACITY(ANN_RESERVE_CAPACITY)
               WRITE_CAPACITY = ANN_RESERVE_CAPACITY
               WRITE_PRODUCTION_COST = WRITE_CAPACITY * PA_RESERVE_RATE &
                                                                 * 1000.
               CPL_ENERGY_REVENUE_FROM_PA(0) = &
                   CPL_ENERGY_REVENUE_FROM_PA(0) + WRITE_PRODUCTION_COST
               CPL_ANN_REVENUE_FROM_PA = CPL_ANN_REVENUE_FROM_PA + &
                                                   WRITE_PRODUCTION_COST
!
            ELSEIF(J == 3) THEN
               WRITE_PRODUCTION_COST = WRITE_CAPACITY * &
                                            PA_SUPPLEMENTAL_RATE * 1000.
               CPL_ENERGY_REVENUE_FROM_PA(0) = &
                   CPL_ENERGY_REVENUE_FROM_PA(0) + WRITE_PRODUCTION_COST
            ENDIF
!
            IF(WRITE_CAPACITY > 0.) THEN
               AVERAGE_PRODUCTION_COSTS = .001 * WRITE_PRODUCTION_COST / &
                                                          WRITE_CAPACITY
            ELSE
               AVERAGE_PRODUCTION_COSTS = 0.
            ENDIF
            WRITE(CPL_PAEN_UNIT,REC=CPL_PAEN_REC) &
                     PRT_ENDPOINT(), &
                     FLOAT(CURRENT_year_loc), &
                     CL_MONTH_NAME(ANNUAL_COUNTER), &
                     WRITE_UNIT_NAME, &
                     WRITE_CAPACITY, &
                     WRITE_ENERGY, &
                     CF, &
                     WRITE_PRODUCTION_COST/1000., &
                     AVERAGE_PRODUCTION_COSTS
            CPL_PAEN_REC = CPL_PAEN_REC + 1
         ENDDO
!
         CALL GET_BUYBACK_CAPACITY(PA_MAYO_BUYBACK, &
                                   PA_HARRIS_BUYBACK)
         DO J = 1, 2
            WRITE_CAPACITY = 0.
            WRITE_ENERGY = 0.
            CF = 0.
!
            IF(J == 1) THEN
               WRITE_UNIT_NAME = PA_ACCT_NAMES(5)(1:14)//'   Cap'
               WRITE_PRODUCTION_COST = PA_HARRIS_BUYBACK
               CPL_ENERGY_EXPENSE_TO_PA(0) = &
                     CPL_ENERGY_EXPENSE_TO_PA(0) + &
                                                   WRITE_PRODUCTION_COST
               CPL_ANN_EXPENSE_TO_PA = CPL_ANN_EXPENSE_TO_PA + &
                                                   WRITE_PRODUCTION_COST
            ELSEIF(J == 2) THEN
               WRITE_UNIT_NAME = PA_ACCT_NAMES(6)(1:14)//'   Cap'
               WRITE_PRODUCTION_COST = PA_MAYO_BUYBACK
               CPL_ENERGY_EXPENSE_TO_PA(0) = &
                     CPL_ENERGY_EXPENSE_TO_PA(0) + &
                                                   WRITE_PRODUCTION_COST
               CPL_ANN_EXPENSE_TO_PA = CPL_ANN_EXPENSE_TO_PA + &
                                                   WRITE_PRODUCTION_COST
            ENDIF
!
            WRITE(CPL_PAEN_UNIT,REC=CPL_PAEN_REC) &
                     PRT_ENDPOINT(), &
                     FLOAT(CURRENT_year_loc), &
                     CL_MONTH_NAME(ANNUAL_COUNTER), &
                     WRITE_UNIT_NAME, &
                     WRITE_CAPACITY, &
                     WRITE_ENERGY, &
                     CF, &
                     WRITE_PRODUCTION_COST/1000., &
                     0.
            CPL_PAEN_REC = CPL_PAEN_REC + 1
         ENDDO
!
!
!
         WRITE_CAPACITY = ANN_PA_CAP(0)
         WRITE_ENERGY = 0.
         WRITE_PRODUCTION_COST = 1000. * &
                                   WRITE_CAPACITY * PA_TRANSMISSION_RATE
!
         CF = 0.
!
         IF(WRITE_CAPACITY > 0.) THEN
            AVERAGE_PRODUCTION_COSTS = WRITE_PRODUCTION_COST/ &
                                                          WRITE_CAPACITY
         ELSE
            AVERAGE_PRODUCTION_COSTS = 0.
         ENDIF
!
         CPL_ENERGY_REVENUE_FROM_PA(0) = &
                          CPL_ENERGY_REVENUE_FROM_PA(0) + &
                                                   WRITE_PRODUCTION_COST
!
!
         WRITE(CPL_PAEN_UNIT,REC=CPL_PAEN_REC) &
                     PRT_ENDPOINT(), &
                     FLOAT(CURRENT_year_loc), &
                     CL_MONTH_NAME(ANNUAL_COUNTER), &
                     'PA Trans Revenues   ', &
                     WRITE_CAPACITY, &
                     WRITE_ENERGY, &
                     CF, &
                     WRITE_PRODUCTION_COST/1000., &
                     AVERAGE_PRODUCTION_COSTS
         CPL_PAEN_REC = CPL_PAEN_REC + 1
!
! EMC STUFF
!
         CALL GET_CPL_EMC_ANN_ENER_REVS( WRITE_ENERGY, &
                                         WRITE_CAPACITY, &
                                         WRITE_PRODUCTION_COST)
!
!
         IF(WRITE_ENERGY > 0.) THEN
            AVERAGE_PRODUCTION_COSTS = WRITE_PRODUCTION_COST/ &
                                                            WRITE_ENERGY
         ELSE
            AVERAGE_PRODUCTION_COSTS = 0.
         ENDIF
!
         CF = WRITE_CAPACITY*8760.
         IF(CF > 0.) THEN
            CF = 100.*WRITE_ENERGY/CF
         ELSE
            CF = 0.
         ENDIF
!
         CPL_EMC_REVENUE(0) = CPL_EMC_REVENUE(0) + &
                                                   WRITE_PRODUCTION_COST
!
!
!
         CPL_ANN_REVENUE_FROM_PA = CPL_ANN_REVENUE_FROM_PA + &
                                                   WRITE_PRODUCTION_COST
!
         WRITE(CPL_PAEN_UNIT,REC=CPL_PAEN_REC) &
                     PRT_ENDPOINT(), &
                     FLOAT(CURRENT_year_loc), &
                     CL_MONTH_NAME(ANNUAL_COUNTER), &
                     'EMC Energy Revenues ', &
                     WRITE_CAPACITY, &
                     WRITE_ENERGY, &
                     CF, &
                     WRITE_PRODUCTION_COST/1000., &
                     AVERAGE_PRODUCTION_COSTS
         CPL_PAEN_REC = CPL_PAEN_REC + 1
!
         CALL GET_CPL_EMC_ANN_TRAN_REVS(  WRITE_CAPACITY, &
                                          WRITE_PRODUCTION_COST, &
                                          CURRENT_year_loc)
         IF(WRITE_CAPACITY > 0.) THEN
            AVERAGE_PRODUCTION_COSTS = WRITE_PRODUCTION_COST/ &
                                                          WRITE_CAPACITY
         ELSE
            AVERAGE_PRODUCTION_COSTS = 0.
         ENDIF
!
         WRITE_ENERGY = 0.
         CF = 0.
!
!
!
!
         CPL_EMC_REVENUE(0) = CPL_EMC_REVENUE(0) + &
                                                   WRITE_PRODUCTION_COST
         CPL_ANN_REVENUE_FROM_PA = CPL_ANN_REVENUE_FROM_PA + &
                                                   WRITE_PRODUCTION_COST
!
!
         WRITE(CPL_PAEN_UNIT,REC=CPL_PAEN_REC) &
                     PRT_ENDPOINT(), &
                     FLOAT(CURRENT_year_loc), &
                     CL_MONTH_NAME(ANNUAL_COUNTER), &
                     'EMC Trans Revenues  ', &
                     WRITE_CAPACITY, &
                     WRITE_ENERGY, &
                     CF, &
                     WRITE_PRODUCTION_COST/1000., &
                     AVERAGE_PRODUCTION_COSTS
         CPL_PAEN_REC = CPL_PAEN_REC + 1

         CALL GET_CPL_EMC_ANN_CAP_REVS(  WRITE_ENERGY, &
                                         WRITE_CAPACITY, &
                                         WRITE_PRODUCTION_COST)
         IF(WRITE_CAPACITY > 0.) THEN
            AVERAGE_PRODUCTION_COSTS = WRITE_PRODUCTION_COST/ &
                                                          WRITE_CAPACITY
         ELSE
            AVERAGE_PRODUCTION_COSTS = 0.
         ENDIF
!
         WRITE_ENERGY = 0.
         CF = 0.
!
         CPL_EMC_REVENUE(0) = CPL_EMC_REVENUE(0) + &
                                                   WRITE_PRODUCTION_COST
!
!
!
         CPL_ANN_REVENUE_FROM_PA = CPL_ANN_REVENUE_FROM_PA + &
                                                   WRITE_PRODUCTION_COST
!
!
         WRITE(CPL_PAEN_UNIT,REC=CPL_PAEN_REC) &
                     PRT_ENDPOINT(), &
                     FLOAT(CURRENT_year_loc), &
                     CL_MONTH_NAME(ANNUAL_COUNTER), &
                     'EMC Capacity Revenue', &
                     WRITE_CAPACITY, &
                     WRITE_ENERGY, &
                     CF, &
                     WRITE_PRODUCTION_COST/1000., &
                     AVERAGE_PRODUCTION_COSTS
         CPL_PAEN_REC = CPL_PAEN_REC + 1
!
         WRITE_CAPACITY = 0.
         WRITE_PRODUCTION_COST = CPL_ENERGY_REVENUE_FROM_PA(0) + &
                                                      CPL_EMC_REVENUE(0)
         IF(WRITE_CAPACITY > 0.) THEN
            AVERAGE_PRODUCTION_COSTS = WRITE_PRODUCTION_COST/ &
                                                          WRITE_CAPACITY
         ELSE
            AVERAGE_PRODUCTION_COSTS = 0.
         ENDIF
!
         WRITE_ENERGY = 0.
         CF = 0.
!
         WRITE(CPL_PAEN_UNIT,REC=CPL_PAEN_REC) &
                     PRT_ENDPOINT(), &
                     FLOAT(CURRENT_year_loc), &
                     CL_MONTH_NAME(ANNUAL_COUNTER), &
                     'EMC and Pa Revenues ', &
                     WRITE_CAPACITY, &
                     WRITE_ENERGY, &
                     CF, &
                     WRITE_PRODUCTION_COST/1000., &
                     AVERAGE_PRODUCTION_COSTS
         CPL_PAEN_REC = CPL_PAEN_REC + 1
      RETURN

      ENTRY CPL_PA_ANN_ENERGY ! INCLUDES EXPENSE AND REVENUE CALC'S


!        TOTAL REVENUE AND ENERGY CALC'S

         CPL_ENERGY_REVENUE_FROM_PA(0) = 0.
         CPL_ENERGY_TO_PA(0) = 0.
         CPL_ENERGY_EXPENSE_TO_PA(0) = 0.
         CPL_ENERGY_FROM_PA(0) = 0.

         ! INCLUDES DEMAND, PA, RESERVE, UNUSED, SUPP, CONTRACT,
         ! DEFICIENCY, TOTAL PA
         DO I = 0, NO_PA_RESOURCES + 1
            IF(I == 0) THEN
               WRITE_UNIT_NAME = 'PA Demand           '
            ELSEIF(I <= NO_PA_UNITS) THEN
               J = PA_UNIT_INDEX(I)
               WRITE_UNIT_NAME = UNITNM(J)(1:18)//'PA'
            ELSEIF(I <= NO_PA_RESOURCES) THEN
               J = I - NO_PA_UNITS
               WRITE_UNIT_NAME = CONTRACT_NAMES(J)
               IF( J == 1 .OR. J == 3 .OR. J == 5) THEN
                  CPL_ENERGY_REVENUE_FROM_PA(0) = &
                          CPL_ENERGY_REVENUE_FROM_PA(0) + ANN_PA_COST(I)
                  CPL_ENERGY_TO_PA(0) = CPL_ENERGY_TO_PA(0) + &
                                                        ANN_PA_ENERGY(I)
               ELSEIF(J == 2) THEN ! NET UNUSED AGAINST SUPPLEMENTAL
                  CPL_ENERGY_REVENUE_FROM_PA(0) = &
                          CPL_ENERGY_REVENUE_FROM_PA(0) - ANN_PA_COST(I)
                  CPL_ENERGY_TO_PA(0) = CPL_ENERGY_TO_PA(0) - &
                                                        ANN_PA_ENERGY(I)
               ELSE
!                 CONTRACT I THINK IS TREATED LIKE ENTITLEMENT
               ENDIF
            ELSE
               WRITE_UNIT_NAME = 'PA Supply           '
            ENDIF
            WRITE_ENERGY = ANN_PA_ENERGY(I)
            WRITE_CAPACITY = ANN_PA_CAP(I)/12.
            CF = WRITE_CAPACITY*8760.
            IF(CF > 0.) THEN
               CF = 100.*WRITE_ENERGY/CF
            ELSE
               CF = 0.
            ENDIF
            WRITE_PRODUCTION_COST = ANN_PA_COST(I)
            IF(WRITE_ENERGY > 0.) THEN
               AVERAGE_PRODUCTION_COSTS = WRITE_PRODUCTION_COST / &
                                                            WRITE_ENERGY
            ELSE
               AVERAGE_PRODUCTION_COSTS = 0.
            ENDIF
            WRITE(CPL_PAEN_UNIT,REC=CPL_PAEN_REC) &
                     PRT_ENDPOINT(), &
                     FLOAT(CURRENT_year_loc), &
                     CL_MONTH_NAME(ANNUAL_COUNTER), &
                     WRITE_UNIT_NAME, &
                     WRITE_CAPACITY, &
                     WRITE_ENERGY, &
                     CF, &
                     WRITE_PRODUCTION_COST/1000., &
                     AVERAGE_PRODUCTION_COSTS
            CPL_PAEN_REC = CPL_PAEN_REC + 1
         ENDDO



         TOTAL_ENT_CAPACITY = 0.
         TOTAL_ENT_ENERGY = 0.
         TOTAL_ENT_PRODUCTION_COST = 0.

         DO I = 1, NO_PA_UNITS ! ENTITLEMENT UNITS ONLY
            J = PA_UNIT_INDEX(I)
            WRITE_UNIT_NAME = UNITNM(J)(1:17)//'ENT'
            WRITE_ENERGY = ANN_SYS_ENERGY(I)
            WRITE_CAPACITY = ANN_SYS_CAP(I)/12.
            CF = WRITE_CAPACITY*8760.
            IF(CF > 0.) THEN
               CF = 100.*WRITE_ENERGY/CF
            ELSE
               CF = 0.
            ENDIF
            WRITE_PRODUCTION_COST = ANN_SYS_COST(I)
!
            TOTAL_ENT_CAPACITY = TOTAL_ENT_CAPACITY + WRITE_CAPACITY
            TOTAL_ENT_ENERGY = TOTAL_ENT_ENERGY + WRITE_ENERGY
            TOTAL_ENT_PRODUCTION_COST = TOTAL_ENT_PRODUCTION_COST + &
                                                   WRITE_PRODUCTION_COST
!
            IF(WRITE_ENERGY > 0.) THEN
               AVERAGE_PRODUCTION_COSTS = WRITE_PRODUCTION_COST / &
                                                            WRITE_ENERGY
            ELSE
               AVERAGE_PRODUCTION_COSTS = 0.
            ENDIF
            WRITE(CPL_PAEN_UNIT,REC=CPL_PAEN_REC) &
                     PRT_ENDPOINT(), &
                     FLOAT(CURRENT_year_loc), &
                     CL_MONTH_NAME(ANNUAL_COUNTER), &
                     WRITE_UNIT_NAME, &
                     WRITE_CAPACITY, &
                     WRITE_ENERGY, &
                     CF, &
                     WRITE_PRODUCTION_COST/1000., &
                     AVERAGE_PRODUCTION_COSTS
            CPL_PAEN_REC = CPL_PAEN_REC + 1
         ENDDO

         WRITE_ENERGY = TOTAL_ENT_ENERGY
         WRITE_CAPACITY = TOTAL_ENT_CAPACITY
         WRITE_PRODUCTION_COST = TOTAL_ENT_PRODUCTION_COST

         CF = WRITE_CAPACITY*8760.
         IF(CF > 0.) THEN
            CF = 100.*WRITE_ENERGY/CF
         ELSE
            CF = 0.
         ENDIF
!
         WRITE(CPL_PAEN_UNIT,REC=CPL_PAEN_REC) &
                     PRT_ENDPOINT(), &
                     FLOAT(CURRENT_year_loc), &
                     CL_MONTH_NAME(ANNUAL_COUNTER), &
                     'Total Entitlement   ', &
                     WRITE_CAPACITY, &
                     WRITE_ENERGY, &
                     CF, &
                     WRITE_PRODUCTION_COST/1000., &
                     AVERAGE_PRODUCTION_COSTS
         CPL_PAEN_REC = CPL_PAEN_REC + 1
!
!
         DO J = 1, NO_PA_ACCTS
            I = J + NO_PA_UNITS
            WRITE_UNIT_NAME = PA_ACCT_NAMES(J)
            WRITE_ENERGY = ANN_SYS_ENERGY(I)
            WRITE_CAPACITY = 0.
            CF = 0.
            WRITE_PRODUCTION_COST = ANN_SYS_COST(I)

            IF(WRITE_ENERGY > 0.) THEN
               AVERAGE_PRODUCTION_COSTS = WRITE_PRODUCTION_COST / &
                                                            WRITE_ENERGY
            ELSE
               AVERAGE_PRODUCTION_COSTS = 0.
            ENDIF
            WRITE(CPL_PAEN_UNIT,REC=CPL_PAEN_REC) &
                     PRT_ENDPOINT(), &
                     FLOAT(CURRENT_year_loc), &
                     CL_MONTH_NAME(ANNUAL_COUNTER), &
                     WRITE_UNIT_NAME, &
                     WRITE_CAPACITY, &
                     WRITE_ENERGY, &
                     CF, &
                     WRITE_PRODUCTION_COST/1000., &
                     AVERAGE_PRODUCTION_COSTS
            CPL_PAEN_REC = CPL_PAEN_REC + 1
!
            IF(J == 3) THEN
!
               CPL_ENERGY_REVENUE_FROM_PA(0) = &
                         CPL_ENERGY_REVENUE_FROM_PA(0) + ANN_SYS_COST(I)
               CPL_ENERGY_TO_PA(0) = CPL_ENERGY_TO_PA(0) + &
                                                       ANN_SYS_ENERGY(I)
            ELSEIF(J == 4 .OR. J == 7) THEN
!
               CPL_ENERGY_EXPENSE_TO_PA(0) = &
                           CPL_ENERGY_EXPENSE_TO_PA(0) + ANN_SYS_COST(I)
               CPL_ENERGY_FROM_PA(0) = CPL_ENERGY_FROM_PA(0) + &
                                                       ANN_SYS_ENERGY(I)
            ENDIF
         ENDDO
!
         WRITE_UNIT_NAME = 'MWHs Supplied by CPL'
         WRITE_CAPACITY = 0.
         CF = 0.
         WRITE_ENERGY = CPL_ENERGY_TO_PA(0)
         WRITE_PRODUCTION_COST = CPL_ENERGY_REVENUE_FROM_PA(0)
         IF(WRITE_ENERGY > 0.) THEN
            AVERAGE_PRODUCTION_COSTS = WRITE_PRODUCTION_COST / &
                                                            WRITE_ENERGY
         ELSE
            AVERAGE_PRODUCTION_COSTS = 0.
         ENDIF
         WRITE(CPL_PAEN_UNIT,REC=CPL_PAEN_REC) &
                     PRT_ENDPOINT(), &
                     FLOAT(CURRENT_year_loc), &
                     CL_MONTH_NAME(ANNUAL_COUNTER), &
                     WRITE_UNIT_NAME, &
                     WRITE_CAPACITY, &
                     WRITE_ENERGY, &
                     CF, &
                     WRITE_PRODUCTION_COST/1000., &
                     AVERAGE_PRODUCTION_COSTS
         CPL_PAEN_REC = CPL_PAEN_REC + 1
!
         WRITE_UNIT_NAME = 'MWHs Supplied to CPL'
         WRITE_CAPACITY = 0.
         CF = 0.
         WRITE_ENERGY = CPL_ENERGY_FROM_PA(0)
         WRITE_PRODUCTION_COST = CPL_ENERGY_EXPENSE_TO_PA(0)
         IF(WRITE_ENERGY > 0.) THEN
            AVERAGE_PRODUCTION_COSTS = WRITE_PRODUCTION_COST / &
                                                            WRITE_ENERGY
         ELSE
            AVERAGE_PRODUCTION_COSTS = 0.
         ENDIF
         WRITE(CPL_PAEN_UNIT,REC=CPL_PAEN_REC) &
                     PRT_ENDPOINT(), &
                     FLOAT(CURRENT_year_loc), &
                     CL_MONTH_NAME(ANNUAL_COUNTER), &
                     WRITE_UNIT_NAME, &
                     WRITE_CAPACITY, &
                     WRITE_ENERGY, &
                     CF, &
                     WRITE_PRODUCTION_COST/1000., &
                     AVERAGE_PRODUCTION_COSTS
         CPL_PAEN_REC = CPL_PAEN_REC + 1
      RETURN

      ENTRY CPL_ENERGY_REV_AND_EXP(R_CPL_ENERGY_REVENUE_FROM_PA, &
                                   R_CPL_ENERGY_TO_PA, &
                                   R_CPL_ENERGY_EXPENSE_TO_PA, &
                                   R_CPL_ENERGY_FROM_PA, &
                                   R_CPL_EMC_REVENUE)

        R_CPL_ENERGY_REVENUE_FROM_PA = CPL_ENERGY_REVENUE_FROM_PA(0)/ &
                                                                1000000.
        R_CPL_ENERGY_TO_PA  = CPL_ENERGY_TO_PA(0)
        R_CPL_ENERGY_EXPENSE_TO_PA = CPL_ENERGY_EXPENSE_TO_PA(0)/ &
                                                                1000000.
        R_CPL_ENERGY_FROM_PA = CPL_ENERGY_FROM_PA(0)
!
        R_CPL_EMC_REVENUE = CPL_EMC_REVENUE(0)/1000000.
!
      RETURN
!

      ENTRY CPL_MONTHLY_REV_AND_EXP(R_CATAWBA_REVENUES, &
                                    R_CATAWBA_EXPENSES, &
                                    R_CPL_ENERGY_TO_PA, &
                                    R_CPL_ENERGY_FROM_PA)

!
         R_CPL_ENERGY_TO_PA  = 0.
         R_CPL_ENERGY_FROM_PA = 0.
!
         DO MO = 0, 12
!
            IF(MO == 0) THEN
               R_CATAWBA_EXPENSES(MO) = CPL_ANN_EXPENSE_TO_PA
               R_CATAWBA_REVENUES(MO) = CPL_ANN_REVENUE_FROM_PA
            ELSE
               R_CATAWBA_EXPENSES(MO) = CPL_ANN_EXPENSE_TO_PA/12.
               R_CATAWBA_REVENUES(MO) = CPL_ANN_REVENUE_FROM_PA/12.
            ENDIF
!
! EXPENSE
!
            DO J = 4, 7, 3
               I = J + NO_PA_UNITS
               R_CATAWBA_EXPENSES(MO) = R_CATAWBA_EXPENSES(MO) &
                                        + MON_SYS_COST(I,MO)
               R_CPL_ENERGY_FROM_PA = R_CPL_ENERGY_FROM_PA &
                                      + MON_SYS_ENERGY(I,MO)
            ENDDO
!
! REVENUE
!

! CAPACITY
            CALL GET_PA_CAPACITY_RATES(PA_RESERVE_RATE, &
                                       PA_SUPPLEMENTAL_RATE, &
                                       PA_TRANSMISSION_RATE)

            J = 3
            I = J + NO_PA_UNITS
            R_CATAWBA_REVENUES(MO) = R_CATAWBA_REVENUES(MO) + &
                         MON_PA_CAP(I,MO) * PA_SUPPLEMENTAL_RATE * 1000.
            I = 0
            R_CATAWBA_REVENUES(MO) = R_CATAWBA_REVENUES(MO) + &
                         MON_PA_CAP(I,MO) * PA_TRANSMISSION_RATE * 1000.
! ENERGY
            DO I = NO_PA_UNITS+1, NO_PA_RESOURCES
               J = I - NO_PA_UNITS
               IF(J == 1 .OR. J == 3 .OR. J == 5) THEN
                  R_CATAWBA_REVENUES(MO) = R_CATAWBA_REVENUES(MO) &
                                           + MON_PA_COST(I,MO)
                  R_CPL_ENERGY_TO_PA = R_CPL_ENERGY_TO_PA &
                                       + MON_PA_ENERGY(I,MO)
               ELSEIF(J == 2) THEN ! NET UNUSED AGAINST SUPPLEMENTAL
                  R_CATAWBA_REVENUES(MO) = R_CATAWBA_REVENUES(MO) &
                                           - MON_PA_COST(I,MO)
                  R_CPL_ENERGY_TO_PA = R_CPL_ENERGY_TO_PA &
                                       - MON_PA_ENERGY(I,MO)
               ELSE
!                 CONTRACT I THINK IS TREATED LIKE ENTITLEMENT
               ENDIF
!
               IF(J == 3) THEN
!
                  R_CATAWBA_REVENUES(MO) = R_CATAWBA_REVENUES(MO) &
                                           + MON_SYS_COST(I,MO)
                  R_CPL_ENERGY_TO_PA = R_CPL_ENERGY_TO_PA &
                                       + MON_SYS_ENERGY(I,MO)
!

               ENDIF
            ENDDO
            R_CATAWBA_EXPENSES(MO) = R_CATAWBA_EXPENSES(MO)/1000000.
            R_CATAWBA_REVENUES(MO) = R_CATAWBA_REVENUES(MO)/1000000.
         ENDDO
      RETURN
!

      ENTRY CPL_MON_ENERGY_REV_AND_EXP(R_ISEAS, &
                                   R_CPL_ENERGY_REVENUE_FROM_PA, &
                                   R_CPL_ENERGY_TO_PA, &
                                   R_CPL_ENERGY_EXPENSE_TO_PA, &
                                   R_CPL_ENERGY_FROM_PA)

!
         R_CPL_ENERGY_TO_PA  = 0.
!
         R_CPL_ENERGY_FROM_PA = 0.
!
         IF(R_ISEAS == 0) THEN
            R_CPL_ENERGY_EXPENSE_TO_PA = CPL_ANN_EXPENSE_TO_PA
            R_CPL_ENERGY_REVENUE_FROM_PA = CPL_ANN_REVENUE_FROM_PA
         ELSE
            R_CPL_ENERGY_EXPENSE_TO_PA = CPL_ANN_EXPENSE_TO_PA/12.
            R_CPL_ENERGY_REVENUE_FROM_PA = CPL_ANN_REVENUE_FROM_PA/12.
         ENDIF
!
         CALL GET_PA_SWITCH(PA_SWITCH)
         IF(.NOT. PA_SWITCH) RETURN
!
! EXPENSE
!
         DO J = 4, 7, 3
            I = J + NO_PA_UNITS
            R_CPL_ENERGY_EXPENSE_TO_PA = R_CPL_ENERGY_EXPENSE_TO_PA + &
                                                 MON_SYS_COST(I,R_ISEAS)
            R_CPL_ENERGY_FROM_PA = R_CPL_ENERGY_FROM_PA + &
                                               MON_SYS_ENERGY(I,R_ISEAS)
         ENDDO
!
! REVENUE
!

! CAPACITY
         CALL GET_PA_CAPACITY_RATES(   PA_RESERVE_RATE, &
                                       PA_SUPPLEMENTAL_RATE, &
                                       PA_TRANSMISSION_RATE)

         J = 3
         I = J + NO_PA_UNITS
         R_CPL_ENERGY_REVENUE_FROM_PA = &
                  R_CPL_ENERGY_REVENUE_FROM_PA + &
                    MON_PA_CAP(I,R_ISEAS) * PA_SUPPLEMENTAL_RATE * 1000.
         I = 0
         R_CPL_ENERGY_REVENUE_FROM_PA = &
                  R_CPL_ENERGY_REVENUE_FROM_PA + &
                    MON_PA_CAP(I,R_ISEAS) * PA_TRANSMISSION_RATE * 1000.
! ENERGY
         DO I = NO_PA_UNITS+1, NO_PA_RESOURCES
            J = I - NO_PA_UNITS
            IF( J == 1 .OR. J == 3 .OR. J == 5) THEN
               R_CPL_ENERGY_REVENUE_FROM_PA = &
                          R_CPL_ENERGY_REVENUE_FROM_PA + &
                                                  MON_PA_COST(I,R_ISEAS)
               R_CPL_ENERGY_TO_PA = R_CPL_ENERGY_TO_PA + &
                                                MON_PA_ENERGY(I,R_ISEAS)
            ELSEIF(J == 2) THEN ! NET UNUSED AGAINST SUPPLEMENTAL
               R_CPL_ENERGY_REVENUE_FROM_PA = &
                          R_CPL_ENERGY_REVENUE_FROM_PA - &
                                                  MON_PA_COST(I,R_ISEAS)
               R_CPL_ENERGY_TO_PA = R_CPL_ENERGY_TO_PA - &
                                                MON_PA_ENERGY(I,R_ISEAS)
            ELSE
!                 CONTRACT I THINK IS TREATED LIKE ENTITLEMENT
            ENDIF
!
            IF(J == 3) THEN
!
               R_CPL_ENERGY_REVENUE_FROM_PA = &
                          R_CPL_ENERGY_REVENUE_FROM_PA + &
                                                 MON_SYS_COST(I,R_ISEAS)
               R_CPL_ENERGY_TO_PA = R_CPL_ENERGY_TO_PA + &
                                               MON_SYS_ENERGY(I,R_ISEAS)
            ELSEIF(J == 4 .OR. J == 7) THEN
!
               R_CPL_ENERGY_EXPENSE_TO_PA = &
                        R_CPL_ENERGY_EXPENSE_TO_PA + &
                                                 MON_SYS_COST(I,R_ISEAS)
               R_CPL_ENERGY_FROM_PA = R_CPL_ENERGY_FROM_PA + &
                                               MON_SYS_ENERGY(I,R_ISEAS)
            ENDIF
         ENDDO
      RETURN
!
      END subroutine CPL_PA_DISPATCH
!
!
!

      SUBROUTINE CREATE_CPL_LDC( ISEAS, &
                                 SYSTEM_PEAK, &
                                 SYSTEM_DEMAND, &
                                 SYSTEM_LODDUR, &
                                 PA_LODDUR, &
                                 LPROB2, &
                                 SEAS_HOURS, &
                                 PA_PEAK, &
                                 PA_DEMAND, &
                                 PA_DX, &
                                 BUYBACK_ENERGY, &
                                 R_PA_HOURLY_LOADS)
      use end_routine, only: end_program, er_message
      use logging

!
!     TAKEN FROM LDCTRANS.FOR 4-2-91
!     ASSUMES:
!
!        NO DSM FOR THE PA SYSTEM
!
!
!     DECLARE VARIABLES
!
      use SpinDriftLib
      use prod_arrays_dimensions
      USE SIZECOM
      use lamcom
	  use cls_load
      
      implicit none

      REAL (kind=4) ::    LPROB2(*)
      REAL (kind=4) ::  SYSTEM_LODDUR(CONVOLUTION_POINTS)
      REAL (kind=4) ::  SYSTEM_PEAK
      REAL (kind=4) ::  PA_LODDUR(CONVOLUTION_POINTS)
      REAL (kind=4) ::  PA_PEAK
      REAL (kind=4) ::  LODDUR_SLOPE
      REAL (kind=4) ::  LODDUR_INTERCEPT
      REAL (kind=4) ::  SEAS_HOURS
      REAL (kind=4) ::  PA_DX
      REAL (kind=4) ::  BUYBACK_ENERGY
      REAL (kind=4) ::  R_PA_HOURLY_LOADS(*)
      REAL (kind=8) ::  SYSTEM_DEMAND
      REAL (kind=8) ::  PA_DEMAND
      REAL (kind=8) ::  PA_DEMAND_TEST
      REAL (kind=8) ::  SYSTEM_AVE_ENERGY
      REAL (kind=8) ::  PA_AVE_ENERGY
      INTEGER (kind=2) ::    I
      INTEGER (kind=2) ::    PA_CLASS=6
      INTEGER (kind=2) ::  ISEAS
      INTEGER (kind=2) ::  IMAX
      INTEGER (kind=2) ::  IREC
      INTEGER (kind=2) ::  ALINE_LOAD_DATA
      INTEGER (kind=2) ::  DAYS_IN_MONTH
      INTEGER (kind=2) ::  CURRENT_HR
      INTEGER (kind=2) ::  LOCAL_HOURS
      INTEGER (kind=2) ::  DA
      INTEGER (kind=2) ::  LDE_DAY
      INTEGER (kind=2) ::  LDE_MONTH
      INTEGER (kind=2) ::  LDE_YEAR
      INTEGER (kind=2) ::  DAY_WEEK
      INTEGER (kind=2) ::  HR
      CHARACTER (len=8) ::  EEICODE
      INTEGER (kind=2) ::  TIMZON,TEMPER,DELTMP,LOCAL_LOAD_POINTS
      PARAMETER (LOCAL_LOAD_POINTS=79)
      REAL (kind=4) ::  MONTHLY_PA_LOADS(:)
      REAL (kind=4) ::  PA_HIST_BASE
      REAL (kind=4) ::  PA_HIST_PEAK
      REAL (kind=4) ::  LOCAL_LPROB(LOCAL_LOAD_POINTS)
      REAL (kind=4) ::  LOCAL_LODDUR(LOCAL_LOAD_POINTS)
      REAL (kind=8) ::  PA_HIST_ENERGY
      INTEGER (kind=4) ::  LOCAL_PA_LOADS(24)
      ALLOCATABLE :: MONTHLY_PA_LOADS
      CHARACTER (len=256) ::  FILE_NAME,BASE_FILE_DIRECTORY
      LOGICAL (kind=4) ::  FILE_EXISTS
!
! END DATA DECLARATIONS
!
!
!     INITIALIZE VARIABLES
!
!
         LOCAL_HOURS = INT(SEAS_HOURS)
         ALLOCATE(MONTHLY_PA_LOADS(LOCAL_HOURS))
!
!
! CASE 2 WITH BINARY MARKET FILES
!
!
!
         FILE_NAME = trim(BASE_FILE_DIRECTORY())//"LDEPAT95.BIN"
         INQUIRE(FILE=FILE_NAME,EXIST=file_exists)
         IF(file_exists) THEN
            OPEN(UNIT=2801,FILE=FILE_NAME, &
                                           ACCESS="DIRECT",STATUS="OLD")
            I = 1
            IREC = ALINE_LOAD_DATA(I,ISEAS)
            DAYS_IN_MONTH = LOCAL_HOURS/24
!
            CURRENT_HR = 0
!
            PA_HIST_BASE = 999999.
            PA_HIST_PEAK = 0.
            PA_HIST_ENERGY = 0.
!
            DO DA = 1, DAYS_IN_MONTH
               READ(2801,REC=IREC) LDE_DAY,LDE_MONTH,LDE_YEAR, &
                                     EEICODE,DAY_WEEK, &
                                     TIMZON,TEMPER, &
                                     DELTMP, &
                                     (LOCAL_PA_LOADS(HR),HR=1,24)
               IREC = IREC + 1
               DO HR = 1, 24
                  CURRENT_HR = CURRENT_HR + 1
                  MONTHLY_PA_LOADS(CURRENT_HR) = LOCAL_PA_LOADS(HR)
               PA_HIST_BASE = MIN(PA_HIST_BASE,REAL(LOCAL_PA_LOADS(HR)))
               PA_HIST_PEAK = MAX(PA_HIST_PEAK,REAL(LOCAL_PA_LOADS(HR)))
                  PA_HIST_ENERGY = PA_HIST_ENERGY + LOCAL_PA_LOADS(HR)
               ENDDO
            ENDDO
            CLOSE(2801)

         ELSE
            WRITE(4,*) '*** line 4589 CPL_PA_DISPATCH ***'
            WRITE(4,*) "CANNOT FIND PA HOURLY LOAD FILE"
            WRITE(4,*) "NAMED LDEPAT95.BIN. THIS FILE IS NEEDED"
            WRITE(4,*) "TO RUN THE CUSTOM CPL/PA LOGIC"
            er_message='See WARNING MESSAGES-cat2obj-3'
            call end_program(er_message)
         ENDIF

         PA_PEAK = MAX(FORECAST_COINCIDENT_PEAK(1,ISEAS,PA_CLASS), &
                    FORECAST_COINCIDENT_PEAK(2,ISEAS,PA_CLASS))
         PA_DEMAND = FORECAST_ENERGY(1,ISEAS,PA_CLASS) + &
                              FORECAST_ENERGY(2,ISEAS,PA_CLASS)


         SYSTEM_AVE_ENERGY = PA_HIST_ENERGY / SEAS_HOURS
         PA_AVE_ENERGY = PA_DEMAND / SEAS_HOURS
         LODDUR_SLOPE = (PA_PEAK - PA_AVE_ENERGY) / &
                                      (PA_HIST_PEAK - SYSTEM_AVE_ENERGY)
         LODDUR_INTERCEPT = PA_AVE_ENERGY - LODDUR_SLOPE* &
                                                       SYSTEM_AVE_ENERGY
!
! NOTE: REUSING MONTHLY_PA_LOADS TO BUILD TO FORECASTED VALUES
!
!
         PA_HIST_BASE = 999999.
         PA_HIST_PEAK = 0.
         PA_HIST_ENERGY = 0.
!
         DO HR = 1, LOCAL_HOURS
            MONTHLY_PA_LOADS(HR) = LODDUR_INTERCEPT + &
                                     LODDUR_SLOPE * MONTHLY_PA_LOADS(HR)
            PA_HIST_BASE = MIN(PA_HIST_BASE,MONTHLY_PA_LOADS(HR))
            PA_HIST_PEAK = MAX(PA_HIST_PEAK,MONTHLY_PA_LOADS(HR))
            PA_HIST_ENERGY = PA_HIST_ENERGY + MONTHLY_PA_LOADS(HR)
!
            R_PA_HOURLY_LOADS(HR) = MONTHLY_PA_LOADS(HR)
!
         ENDDO
         CALL GENERIC_LOAD_PROB(    LOCAL_HOURS, &
                                    MONTHLY_PA_LOADS, &
                                    PA_HIST_ENERGY, &
                                    PA_HIST_PEAK, &
                                    PA_HIST_BASE, &
                                    LOCAL_LPROB, &
                                    LOCAL_LODDUR)
!
         DEALLOCATE(MONTHLY_PA_LOADS)
!
!
!
      IMAX = LOCAL_LOAD_POINTS
      DO I = 1, LOCAL_LOAD_POINTS


         LPROB2(I) = LOCAL_LPROB(I)
         PA_LODDUR(I) = LOCAL_LODDUR(I)
      ENDDO
      IF(IMAX == 9999) IMAX = CONVOLUTION_POINTS
      IF(IMAX > 2) THEN
         PA_DX = MAX(1.,(PA_LODDUR(3) - PA_LODDUR(1))/2.)
      ELSE
         PA_DX = 1.
      ENDIF
!
!     TEST LINEAR TRANSFORM
!
      CALL INTEG8(PA_DEMAND_TEST,PA_LODDUR,LPROB2,IMAX, &
                                           INT(SEAS_HOURS),PA_LODDUR(1))
      RETURN
      END
!
!
!

!
!     ROUTINE TO CALCULATE THE TRANS LOAD PROBABILITY
!                 CURVE ON AN ODD SPACED GRID
!

!
      SUBROUTINE GENERIC_LOAD_PROB( HOURS_INCREMENT, &
                                    TRANS_LOAD, &
                                    SUM_TRANS_LOADS, &
                                    MAX_TRANS_LOAD, &
                                    MIN_TRANS_LOAD, &
                                    LPROB,LODDUR)
!
      use SpinDriftLib
      use prod_arrays_dimensions
      use logging
      use grx_planning_routines
      USE SIZECOM
      use globecom
      use mod_base_year


      INTEGER (kind=2) ::  LOAD_POINTS
      PARAMETER (LOAD_POINTS=79)
      INTEGER (kind=2) ::  I
      INTEGER (kind=2) ::  HR
      INTEGER (kind=2) ::  IMAX
      INTEGER (kind=2) ::  HOURS_INCREMENT
      INTEGER (kind=2) ::  IPEAK
      INTEGER (kind=2) ::  COUNT
      INTEGER (kind=2) ::  INTERVALS
      INTEGER (kind=2) ::  COUNTER
      INTEGER (kind=2) ::  POINTS_IN_CURVE
      INTEGER (kind=2) ::  R_LOAD_POINTS
      INTEGER (kind=2) ::  R_MAX_TRANS_LOAD_GROUPS
      INTEGER (kind=2) ::  SAVE_MAX_TRANS_LOAD_GROUPS=0
      INTEGER (kind=2) ::  SAVE_TARGET_TRANS_GROUP=0
      INTEGER (kind=2) ::  TG_FROM_TRANS_LOAD_GROUP
      REAL ::  BASE
      REAL ::  OBS(LOAD_POINTS)
      REAL ::  DELTA_PROB
      REAL ::  AREA
      REAL ::  MIN_LPROB
      REAL ::  OBSERVATIONS
      REAL :: &
           LPROB(LOAD_POINTS), &
           DX, &
           R_DX, &
           PEAK, &
           R_PEAK, &
           R_BASE, &
           LODDUR(LOAD_POINTS), &
           TRANS_LOAD(*), &
           AVE_ENERGY, &
           MAX_TRANS_LOAD, &
           MIN_TRANS_LOAD,LOAD_VAL,PEAK_DX,BASE_DX, &
           BASE_ADJUSTMENT,INTERVAL_HOURS
      REAL (kind=8) ::  SUM_TRANS_LOADS
      REAL (kind=8) ::  DEMAND
      REAL (kind=8) ::  R_DEMAND
      REAL (kind=8) ::  ENERGY(LOAD_POINTS)
      REAL (kind=8) ::  ALPHA
      REAL (kind=8) ::  PRECISN
!
      SAVE DX,PEAK,DEMAND
!
      REAL (kind=4) ::  TEMP_INTERVALS,TEMP_VALUE
      LOGICAL (kind=1) ::  SET_POINTS
!
      INTEGER (kind=2) ::  CURRENT_MONTH
      INTEGER (kind=2) ::  LAST_SEASON=0
      INTEGER (kind=2) ::  get_PRODUCTION_PERIODS
      INTEGER (kind=2) ::  CURRENT_YEAR
      CHARACTER (len=9) ::  CL_MONTH_NAME(13)
      CHARACTER (len=20) ::  MONTH_NAME
!
!     END OF DATA DECLARATIONS
!
!
!
      CURRENT_YEAR = get_BASE_YEAR() + globecom_YEAR
!
!
      BASE_ADJUSTMENT = -999.
      INTERVALS = LOAD_POINTS/2 + 1
      DX = &
                 MAX((MAX_TRANS_LOAD-MIN_TRANS_LOAD)/FLOAT(INTERVALS-1), &
                                                                   0.01)
      POINTS_IN_CURVE = LOAD_POINTS
      COUNT = LOAD_POINTS
!
      PEAK = MAX_TRANS_LOAD
      BASE = MIN_TRANS_LOAD
!
      DO I = 1, LOAD_POINTS
         ENERGY(I)= 0.
         OBS(I) = 0.
         LPROB(I) = 0.
      ENDDO
      LPROB(1) = 1.
!
      LODDUR(1) = BASE
      COUNT = 1
      DO WHILE (LODDUR(COUNT) < &
                          PEAK .AND. COUNT + 2 <= LOAD_POINTS )
         COUNT = COUNT + 2
         LODDUR(COUNT) = LODDUR(COUNT-2) + DX
      ENDDO
      IF(LODDUR(COUNT) < PEAK) THEN
         TEMP_VALUE = LODDUR(COUNT) - PEAK
      ENDIF
      IF(PEAK - LODDUR(COUNT-2) < .0001*DX) &
                                                      COUNT = COUNT - 2
      LODDUR(COUNT) = PEAK
      POINTS_IN_CURVE = COUNT
      IMAX = COUNT
      INTERVALS = COUNT/2 + 1
      IPEAK = INTERVALS - 1
      DO I = COUNT + 1, LOAD_POINTS
         LODDUR(I) = LODDUR(I-1) + DX
      ENDDO
!
!     PLACES TRANS LOADS INTO LOAD_POINTS/2 - 1 INTERVALS
!
      DO HR = 1,HOURS_INCREMENT
         IF(TRANS_LOAD(HR) .GE. BASE) THEN
            I = AINT((TRANS_LOAD(HR) +.0001 - BASE)/DX) + 1
            IF(I.GT.IPEAK) I = IPEAK
            OBS(I) = OBS(I) + 1.
            ENERGY(I) = ENERGY(I) + TRANS_LOAD(HR)
         ENDIF
      ENDDO


      MIN_LPROB = 1./FLOAT(HOURS_INCREMENT)
      OBSERVATIONS = 0.
      DO I = 2 , INTERVALS
         COUNT = 2*(I) - 2
         OBSERVATIONS = OBSERVATIONS + OBS(I-1)
         DELTA_PROB = 1. - OBSERVATIONS/FLOAT(HOURS_INCREMENT)



         IF(OBS(I-1) .LE. 0.) THEN
            LPROB(COUNT+1) = LPROB(COUNT-1)
            LPROB(COUNT) = LPROB(COUNT-1)
            LODDUR(COUNT) = &
                 (LODDUR(COUNT+1)+LODDUR(COUNT-1))/2.
         ELSE
            LODDUR(COUNT) = ENERGY(I-1)/OBS(I-1)
            LPROB(COUNT+1) = DELTA_PROB
            IF(LPROB(COUNT+1) < &
                                  MIN_LPROB) LPROB(COUNT+1) = 0.
            AREA = (ENERGY(I-1)-LODDUR(COUNT-1)*OBS(I-1))/ &
               FLOAT(HOURS_INCREMENT) + &
               LPROB(COUNT+1)* &
                       (LODDUR(COUNT+1)-LODDUR(COUNT-1))
            LPROB(COUNT) = (2*AREA - &
               (LPROB(COUNT-1)* &
                       (LODDUR(COUNT)-LODDUR(COUNT-1)) + &
               LPROB(COUNT+1)* &
                   (LODDUR(COUNT+1)-LODDUR(COUNT)))  ) / &
               (LODDUR(COUNT+1)-LODDUR(COUNT-1))
!
            IF( ENERGY(I-1)/(OBS(I-1) * LODDUR(COUNT+1)) &
                                        > .999999 ) THEN
               LODDUR(COUNT) = LODDUR(COUNT+1)
               IF(LODDUR(COUNT+1) < 10000) THEN
                  LODDUR(COUNT+1) = &
                                          LODDUR(COUNT+1) + .001
               ELSE
                  LODDUR(COUNT+1) = &
                                           LODDUR(COUNT+1) + .01
               ENDIF
            ENDIF
            IF(LPROB(COUNT) .GT. LPROB(COUNT-1)) THEN
               LPROB(COUNT) = LPROB(COUNT-1)
            ELSEIF(LPROB(COUNT) .LT. &
                                           LPROB(COUNT+1) ) THEN
               LPROB(COUNT) = LPROB(COUNT+1)
            ENDIF
         ENDIF
      ENDDO
!
!     THIS SECTION TAKES THE ROUNDING ERROR FROM THE
!     PREVIOUS ALGORITHM (SINGLE PRECISION CALC.) AND
!     DISTRIBUTES IT EVENLY ACROSS LOAD_POINTS-1 POINTS.
!
      COUNTER = 0.
      BASE = LODDUR(1)
   50 CALL INTEG8(DEMAND, &
                  LODDUR(1),LPROB(1),IMAX, &
                  HOURS_INCREMENT,BASE)
      IF(BASE_ADJUSTMENT == -999.) THEN
         ALPHA = LODDUR(2) * FLOAT(HOURS_INCREMENT)
         ALPHA = (SUM_TRANS_LOADS-ALPHA)/(DEMAND-ALPHA)
      ELSE
         ALPHA = 1.
      ENDIF
      DO I = 2,IMAX
         LPROB(I) = LPROB(I)*ALPHA
      ENDDO
      LPROB(IMAX) = 0.
      BASE = LODDUR(1)
      CALL INTEG8(DEMAND, &
         LODDUR(1),LPROB(1),IMAX, &
                            HOURS_INCREMENT,BASE)
      PRECISN = 1.
      IF(SUM_TRANS_LOADS .GT. DEMAND) THEN
         PRECISN = DEMAND/SUM_TRANS_LOADS
      ELSE
         PRECISN = SUM_TRANS_LOADS/DEMAND
      ENDIF
      COUNTER = COUNTER + 1
      IF(PRECISN .LT. .999999 .AND. COUNTER .LT. 3) GOTO 50
!
!     WRITE THE RESULTS TO BE READ BY PROCOST
!
      I = POINTS_IN_CURVE
      DO WHILE (LPROB(I) == 0.)
         I = I - 1
      ENDDO
      POINTS_IN_CURVE = I + 1
      DX = (LODDUR(POINTS_IN_CURVE)-BASE)/ &
                                                FLOAT(POINTS_IN_CURVE-1)
      DX = MAX(DX,0.01)

      RETURN
      END

!
      SUBROUTINE CPL_PA_CAP_CALCS
      use spindriftlib
      use prod_arrays_dimensions
      use logging
      use mwunih
      use sizecom
      use lamcom
      use prodcom
      use prod2com
      use globecom
	  
	  implicit none

      INTEGER (kind=2) ::    R_PA_UNIT_INDEX(*)
      REAL (kind=4) ::  R_RESERVE_CAPACITY
      REAL (kind=4) ::  R_UNUSED_SUPP_CAPACITY
      REAL (kind=4) ::  R_SUPPLEMENTAL_CAPACITY
      REAL (kind=4) ::  LAST_YEAR_CPL_N_PA_CAP=0.
      REAL (kind=4) ::  SEPA_CAPACITY
      REAL (kind=4) ::  RESERVE_PERCENT
      REAL (kind=4) ::  R_RESERVE_PERCENT
      REAL (kind=4) ::  THIS_YEAR_PEAK
      REAL (kind=4) ::  THIS_YEAR_CPL_N_PA_CAP=0.
      REAL (kind=4) ::  SAVE_UNUSED_SUPP_CAPACITY=0.
      REAL (kind=4) ::  PA_RETAINED_CAPACITY=0.
      REAL (kind=4) ::  LAST_YEAR_PEAK
      REAL (kind=4) ::  SAVE_RESERVE_CAPACITY=0.
      REAL (kind=4) ::  R_ANN_RESERVE_CAPACITY
      REAL (kind=4) ::  R_PA_EXP_CAPSYS
      REAL (kind=4) ::  R_PA_CAPSYS
      REAL (kind=4) ::  R_PA_PEAK
      REAL (kind=4) ::  PA_FIRM_PURCHASE
      REAL (kind=4) ::  R_MAINTENANCE_RATE(*)
      REAL (kind=4) ::  R_EAVAIL(*)
      REAL (kind=4) ::  PA_EXP_RETAINED_CAPACITY
      REAL (kind=4) ::  UPDATE_NET_PLANNING_PEAK
      REAL (kind=4) ::  CL_PLANNING_CAPACITY
      REAL (kind=4) ::  EL_PLANNING_CAPACITY
      REAL (kind=4) ::  LM_PLANNING_CAPACITY
      REAL (kind=4) ::  CT_PLANNING_CAPACITY
      REAL (kind=4) ::  ADJUSTMENT_CAPACITY
      REAL (kind=4) ::  R_PA_FIRM_PURCHASE
!
!     END DATA DECLARATIONS
!
!     CALLED MONTHLY IN DR_BOOTH
!
      RETURN

      ! CALLED ANNUALLY IN PROCOST BEFORE DR_BOOTH
      ENTRY CPL_CALC_RESERVE_PERCENT 

         THIS_YEAR_PEAK =  UPDATE_NET_PLANNING_PEAK(globecom_year)
!

         THIS_YEAR_CPL_N_PA_CAP = &
                           CL_PLANNING_CAPACITY(3,globecom_year) + &
                           EL_PLANNING_CAPACITY(3,globecom_year) + &
                           LM_PLANNING_CAPACITY(globecom_year)   + &
                           CT_PLANNING_CAPACITY(3,globecom_year) + &
                           ADJUSTMENT_CAPACITY(globecom_year)
!
         IF(globecom_year == 1) THEN
            LAST_YEAR_PEAK = THIS_YEAR_PEAK
            LAST_YEAR_CPL_N_PA_CAP = THIS_YEAR_CPL_N_PA_CAP
         ENDIF
!
         CALL GET_PA_SEPA_CAPACITY(SEPA_CAPACITY)
         RESERVE_PERCENT = (LAST_YEAR_CPL_N_PA_CAP - LAST_YEAR_PEAK)/ &
                                        (LAST_YEAR_PEAK - SEPA_CAPACITY)
!
!
      RETURN

      ENTRY GET_PA_RESERVE_CAPACITY(R_RESERVE_CAPACITY, &
                                      R_MAINTENANCE_RATE, &
                                      R_EAVAIL,R_PA_UNIT_INDEX, &
                                      R_PA_CAPSYS,R_PA_EXP_CAPSYS, &
                                      R_RESERVE_PERCENT)

!
!
         CALL GET_PA_RETAINED_CAPACITY(PA_RETAINED_CAPACITY, &
                                       PA_EXP_RETAINED_CAPACITY, &
                                       R_MAINTENANCE_RATE,R_EAVAIL, &
                                       R_PA_UNIT_INDEX)
                                       
         ! ANNUAL CALC
         SAVE_RESERVE_CAPACITY = PA_RETAINED_CAPACITY * RESERVE_PERCENT
!
! 27/11/96. GAT. TAKEN-OUT FOR NOW. 4/12/96. BACK IN PER KING.
!
         IF(R_PA_EXP_CAPSYS + SAVE_RESERVE_CAPACITY > &
                                              PA_RETAINED_CAPACITY) THEN
            R_RESERVE_CAPACITY = MIN(  MAX(0., &
                                         PA_RETAINED_CAPACITY - &
                                         R_PA_EXP_CAPSYS), &
                                         SAVE_RESERVE_CAPACITY)
         ELSE
            R_RESERVE_CAPACITY = SAVE_RESERVE_CAPACITY
         ENDIF
!
         SAVE_UNUSED_SUPP_CAPACITY = MAX(0.,PA_RETAINED_CAPACITY - &
                                   (R_PA_EXP_CAPSYS+R_RESERVE_CAPACITY))
      RETURN

      ENTRY GET_ANN_RESERVE_CAPACITY(R_ANN_RESERVE_CAPACITY)

         R_ANN_RESERVE_CAPACITY = SAVE_RESERVE_CAPACITY * 12.
      RETURN

      ENTRY GET_UNUSED_SUPP_CAPACITY(R_UNUSED_SUPP_CAPACITY)

         R_UNUSED_SUPP_CAPACITY = SAVE_UNUSED_SUPP_CAPACITY
      RETURN

      ENTRY GET_SUPPLEMENTAL_CAPACITY(R_SUPPLEMENTAL_CAPACITY,R_PA_PEAK)


         CALL GET_PA_FIRM_N_SEPA(PA_FIRM_PURCHASE,SEPA_CAPACITY)

         R_SUPPLEMENTAL_CAPACITY = &
                               MAX(0.,R_PA_PEAK - PA_RETAINED_CAPACITY - &
                                       SEPA_CAPACITY - PA_FIRM_PURCHASE)
      RETURN

!      ENTRY GET_PA_RESOURCE_CAPACITY(R_PA_FIRM_PURCHASE)



      ENTRY UPDATE_CPL_CAP_CALCS
         ! Routine CALLED ANNUALLY AFTER MONTH LOOP IN PROCOST

         LAST_YEAR_PEAK = THIS_YEAR_PEAK
         LAST_YEAR_CPL_N_PA_CAP = THIS_YEAR_CPL_N_PA_CAP
      RETURN
      END
!


!
      SUBROUTINE GET_PA_RETAINED_CAPACITY(R_PA_RETAINED_CAPACITY, &
                                       R_PA_EXP_RETAINED_CAPACITY, &
                                       R_MAINTENANCE_RATE,R_EAVAIL, &
                                       R_PA_UNIT_INDEX)
!

!
      use logging
      INTEGER (kind=2) ::    CURRENT_YEAR
      INTEGER (kind=2) ::  ROUTINE_BASE_YEAR=1995
      INTEGER (kind=2) ::  YEAR_INDEX
      INTEGER (kind=2) ::  R_PA_UNIT_INDEX(*)
      INTEGER (kind=2) ::  RESOURCE_NO
      REAL (kind=4) ::    R_PA_RETAINED_CAPACITY
      REAL (kind=4) ::  R_PA_EXP_RETAINED_CAPACITY
      REAL (kind=4) ::  BRUNS1_RETAINED=144.8
      REAL (kind=4) ::  BRUNS2_RETAINED=144.8
      REAL (kind=4) ::  HARRIS1_RETAINED
      REAL (kind=4) ::  ROX4_RETAINED=90.6
      REAL (kind=4) ::  MAYO1_RETAINED
      REAL (kind=4) ::  R_MAINTENANCE_RATE(*)
      REAL (kind=4) ::  R_EAVAIL(*)
      REAL (kind=4) ::  RESOURCE_PERCENT

         CALL GET_UNIT_RETAINED(BRUNS1_RETAINED,BRUNS2_RETAINED, &
                                       HARRIS1_RETAINED,ROX4_RETAINED, &
                                       MAYO1_RETAINED)
         R_PA_RETAINED_CAPACITY = BRUNS1_RETAINED + BRUNS2_RETAINED + &
                                  HARRIS1_RETAINED + &
                                  ROX4_RETAINED + &
                                  MAYO1_RETAINED
         R_PA_EXP_RETAINED_CAPACITY = &
                  BRUNS1_RETAINED * &
                        R_EAVAIL(R_PA_UNIT_INDEX(3)) * &
                           (1.-R_MAINTENANCE_RATE(R_PA_UNIT_INDEX(3))) + &
                  BRUNS2_RETAINED * &
                        R_EAVAIL(R_PA_UNIT_INDEX(4)) * &
                           (1.-R_MAINTENANCE_RATE(R_PA_UNIT_INDEX(4))) + &
                  HARRIS1_RETAINED * &
                        R_EAVAIL(R_PA_UNIT_INDEX(5)) * &
                           (1.-R_MAINTENANCE_RATE(R_PA_UNIT_INDEX(5))) + &
                  ROX4_RETAINED * &
                        R_EAVAIL(R_PA_UNIT_INDEX(2)) * &
                           (1.-R_MAINTENANCE_RATE(R_PA_UNIT_INDEX(2))) + &
                  MAYO1_RETAINED * &
                        R_EAVAIL(R_PA_UNIT_INDEX(1)) * &
                           (1.-R_MAINTENANCE_RATE(R_PA_UNIT_INDEX(1)))
      RETURN
!
      ENTRY GET_PA_RETAINED_PERCENT(RESOURCE_NO,RESOURCE_PERCENT)
!
         CALL GET_UNIT_RETAINED(BRUNS1_RETAINED,BRUNS2_RETAINED, &
                                       HARRIS1_RETAINED,ROX4_RETAINED, &
                                       MAYO1_RETAINED)
         IF(RESOURCE_NO == 1) THEN
            RESOURCE_PERCENT = MAYO1_RETAINED/745.
         ELSEIF(RESOURCE_NO == 2) THEN
            RESOURCE_PERCENT = .1294
         ELSEIF(RESOURCE_NO == 3) THEN
            RESOURCE_PERCENT = .1833
         ELSEIF(RESOURCE_NO == 4) THEN
            RESOURCE_PERCENT = .1833
         ELSEIF(RESOURCE_NO == 5) THEN
            RESOURCE_PERCENT = HARRIS1_RETAINED/860.
         ENDIF
      RETURN
      END
!

!
      SUBROUTINE CPL_MON_SALE_AND_GEN_REPORT(R_YEAR)

      use SpinDriftLib
      use prod_arrays_dimensions
      USE IREC_ENDPOINT_CONTROL
      use logging
      use grx_planning_routines
      USE SIZECOM
      use globecom
      use endpoint
      use lamcom
      use enrglimit
	  use cls_load
      
      implicit none

!
         INTEGER (kind=2) ::    MO
      INTEGER (kind=2) ::    year_loc
      INTEGER (kind=2) ::  CPL_MON_SALE_GEN_HEADER
      INTEGER (kind=2) ::  R_YEAR
      INTEGER (kind=2) ::  EMC_CLASS=5
      INTEGER (kind=2) ::  VOID_I2, &
      	GET_CL_ENERGY_BY_TYPE

         REAL (kind=4) :: &
               RESIDENTIAL_MWH(0:12), &
               COMMERCIAL_MWH(0:12), &
               INDUSTRIAL_MWH(0:12), &
               PUBLIC_STREET_HIGHWAY(0:12), &
               OTHER_PUBLIC(0:12), &
               TOTAL_RETAIL(0:12), &
               NCEMPA_SPECIAL_CONTRACT(0:12), &
               NCEMC_SUPPLEMENTAL(0:12), &
               WHOLESALE_INCL_UNBILLED(0:12), &
               SALES_TO_OTHER_UTILITIES(0:12), &
               TOTAL_SALES_ENERGY(0:12), &
               ENERGY_USED_BY_COMPANY(0:12), &
               ENERGY_UNACCOUNTED_FOR(0:12), &
               TOTAL_SYSTEM_INPUT_NET(0:12)
        REAL (kind=4) :: &
               NET_GENERATION_STEAM(0:12), &
               NET_GENERATION_NUCLEAR(0:12), &
               NET_GENERATION_HYDRO(0:12), &
               NET_GENERATION_OTHER(0:12), &
               TOTAL_GENERATION(0:12), &
               PURCHASE_POWER(0:12), &
               RECEIVED_FOR_SEPA_CUST(0:12), &
               DELIVERD_FOR_SEPA_CUST(0:12), &
               TOTAL_SYSTEM_GEN_NET(0:12), &
               POWER_AGENCY_ENTITLEMENT(0:12), &
               CPL_ENERGY_REVENUE_FROM_PA, &
               CPL_ENERGY_EXPENSE_TO_PA, &
               CPL_ENERGY_FROM_PA, &
               BUYBACK_MWH
         CHARACTER (len=9) ::  CL_MONTH_NAME(0:12)
         CHARACTER (len=20) ::    WRITE_UNIT_NAME
         SAVE CL_MONTH_NAME ! ,CPL_PAEN_UNIT,CURRENT_YEAR
!
      LOGICAL (kind=1) ::  CPL_SALES_GEN_NOT_OPEN=.TRUE.
      LOGICAL (kind=1) ::  CPL_SALES_GEN_REPORT_ACTIVE
         INTEGER (kind=2) ::    CPL_SALES_GEN_HEADER
      INTEGER (kind=2) ::  CPL_SALES_GEN_UNIT=0
      INTEGER (kind=2) ::  VARIABLE_NUMBER
         INTEGER ::  CPL_SALES_GEN_REC
         SAVE CPL_SALES_GEN_REC
         CHARACTER (len=38) :: &
          ASSET_CLASS_NAME='Parent                      '


! END DATA DECLARATIONS

         CPL_SALES_GEN_REPORT_ACTIVE = .TRUE. ! FOR NOW
         IF(CPL_SALES_GEN_NOT_OPEN .AND. &
                                       CPL_SALES_GEN_REPORT_ACTIVE) THEN
!
            CPL_SALES_GEN_NOT_OPEN = .FALSE.
            VARIABLE_NUMBER = 24
            CPL_SALES_GEN_UNIT = CPL_SALES_GEN_HEADER(VARIABLE_NUMBER, &
                                                      CPL_SALES_GEN_REC)
!
            CL_MONTH_NAME(0) = 'Annual   '
            CL_MONTH_NAME(1) = 'January  '
            CL_MONTH_NAME(2) = 'February '
            CL_MONTH_NAME(3) = 'March    '
            CL_MONTH_NAME(4) = 'April    '
            CL_MONTH_NAME(5) = 'May      '
            CL_MONTH_NAME(6) = 'June     '
            CL_MONTH_NAME(7) = 'July     '
            CL_MONTH_NAME(8) = 'August   '
            CL_MONTH_NAME(9) = 'September'
            CL_MONTH_NAME(10) = 'October  '
            CL_MONTH_NAME(11) = 'November '
            CL_MONTH_NAME(12) = 'December '
!
            DELIVERD_FOR_SEPA_CUST(0) = 170.79965
            DELIVERD_FOR_SEPA_CUST(1) = 16.67063
            DELIVERD_FOR_SEPA_CUST(2) = 16.96104
            DELIVERD_FOR_SEPA_CUST(3) = 18.73088
            DELIVERD_FOR_SEPA_CUST(4) = 15.66725
            DELIVERD_FOR_SEPA_CUST(5) = 13.93029
            DELIVERD_FOR_SEPA_CUST(6) = 14.68544
            DELIVERD_FOR_SEPA_CUST(7) = 13.86657
            DELIVERD_FOR_SEPA_CUST(8) = 13.97898
            DELIVERD_FOR_SEPA_CUST(9) = 13.79550
            DELIVERD_FOR_SEPA_CUST(10) = 10.71280
            DELIVERD_FOR_SEPA_CUST(11) = 10.68182
            DELIVERD_FOR_SEPA_CUST(12) = 11.11846
         ENDIF
!
         TOTAL_RETAIL(0) = 0.
         TOTAL_SALES_ENERGY(0)  = 0.
         TOTAL_SYSTEM_INPUT_NET(0)  = 0.
         TOTAL_GENERATION(0)  = 0.
         TOTAL_SYSTEM_GEN_NET(0)  = 0.
         year_loc = R_YEAR + get_BASE_YEAR()
      DO MO = 0, 12

         PURCHASE_POWER(MO) = 0.
         NET_GENERATION_HYDRO(MO) = 0.
         WHOLESALE_INCL_UNBILLED(MO) = 0.

         CALL GET_REV_FORECAST_SALES(  MO, &
                                       RESIDENTIAL_MWH(MO), &
                                       COMMERCIAL_MWH(MO), &
                                       INDUSTRIAL_MWH(MO), &
                                       PUBLIC_STREET_HIGHWAY(MO), &
                                       OTHER_PUBLIC(MO), &
                                       WHOLESALE_INCL_UNBILLED(MO), &
                                       SALES_TO_OTHER_UTILITIES(MO))
         VOID_I2 = GET_CL_ENERGY_BY_TYPE( MO, &
                                          NET_GENERATION_NUCLEAR(MO), &
                                          NET_GENERATION_STEAM(MO), &
                                          PURCHASE_POWER(MO), &
                                          NET_GENERATION_OTHER(MO))


         CALL GET_EL_ENERGY_BY_TYPE(   MO, &
                                       NET_GENERATION_HYDRO(MO), &
                                       RECEIVED_FOR_SEPA_CUST(MO), &
                                       WHOLESALE_INCL_UNBILLED(MO))
!
         CALL CPL_PA_ENTITLEMENT_MWH(  MO, &
                                       POWER_AGENCY_ENTITLEMENT(MO), &
                                       BUYBACK_MWH)

         CALL CPL_MON_ENERGY_REV_AND_EXP(MO, &
                                       CPL_ENERGY_REVENUE_FROM_PA, &
                                       NCEMPA_SPECIAL_CONTRACT(MO), &
                                       CPL_ENERGY_EXPENSE_TO_PA, &
                                       CPL_ENERGY_FROM_PA)
         NCEMPA_SPECIAL_CONTRACT(MO) = NCEMPA_SPECIAL_CONTRACT(MO)/1000.
         PURCHASE_POWER(MO) = PURCHASE_POWER(MO) + BUYBACK_MWH - &
                                       RECEIVED_FOR_SEPA_CUST(MO)

         IF(MO == 0) THEN
!
            NCEMC_SUPPLEMENTAL(MO) = &
                     FORECAST_ENERGY(1,1,EMC_CLASS) + &
                        FORECAST_ENERGY(2,1,EMC_CLASS) + &
                     FORECAST_ENERGY(1,2,EMC_CLASS) + &
                        FORECAST_ENERGY(2,2,EMC_CLASS) + &
                     FORECAST_ENERGY(1,3,EMC_CLASS) + &
                        FORECAST_ENERGY(2,3,EMC_CLASS) + &
                     FORECAST_ENERGY(1,4,EMC_CLASS) + &
                        FORECAST_ENERGY(2,4,EMC_CLASS) + &
                     FORECAST_ENERGY(1,5,EMC_CLASS) + &
                        FORECAST_ENERGY(2,5,EMC_CLASS) + &
                     FORECAST_ENERGY(1,6,EMC_CLASS) + &
                        FORECAST_ENERGY(2,6,EMC_CLASS) + &
                     FORECAST_ENERGY(1,7,EMC_CLASS) + &
                        FORECAST_ENERGY(2,7,EMC_CLASS) + &
                     FORECAST_ENERGY(1,8,EMC_CLASS) + &
                        FORECAST_ENERGY(2,8,EMC_CLASS) + &
                     FORECAST_ENERGY(1,9,EMC_CLASS) + &
                        FORECAST_ENERGY(2,9,EMC_CLASS) + &
                     FORECAST_ENERGY(1,10,EMC_CLASS) + &
                        FORECAST_ENERGY(2,10,EMC_CLASS) + &
                     FORECAST_ENERGY(1,11,EMC_CLASS) + &
                        FORECAST_ENERGY(2,11,EMC_CLASS) + &
                     FORECAST_ENERGY(1,12,EMC_CLASS) + &
                        FORECAST_ENERGY(2,12,EMC_CLASS)
!
         ELSE
            NCEMC_SUPPLEMENTAL(MO) = &
                     FORECAST_ENERGY(1,MO,EMC_CLASS) + &
                        FORECAST_ENERGY(2,MO,EMC_CLASS)
         ENDIF
         NCEMC_SUPPLEMENTAL(MO) = NCEMC_SUPPLEMENTAL(MO) / 1000.

         TOTAL_RETAIL(MO) =         RESIDENTIAL_MWH(MO) + &
                                    INDUSTRIAL_MWH(MO) + &
                                    COMMERCIAL_MWH(MO) + &
                                    PUBLIC_STREET_HIGHWAY(MO) + &
                                    OTHER_PUBLIC(MO)



         TOTAL_SALES_ENERGY(MO) =   TOTAL_RETAIL(MO) + &
                                    NCEMPA_SPECIAL_CONTRACT(MO) + &
                                    NCEMC_SUPPLEMENTAL(MO) + &
                                    WHOLESALE_INCL_UNBILLED(MO) + &
                                    SALES_TO_OTHER_UTILITIES(MO)


         ENERGY_USED_BY_COMPANY(MO) = 0. ! THIS IS NOT SET PER LF95
         TOTAL_SYSTEM_INPUT_NET(MO) = &
                                    TOTAL_SALES_ENERGY(MO) + &
                                    ENERGY_USED_BY_COMPANY(MO) ! THIS IS NOT SET


         TOTAL_GENERATION(MO) = &
                                    NET_GENERATION_STEAM(MO) + &
                                    NET_GENERATION_NUCLEAR(MO) + &
                                    NET_GENERATION_HYDRO(MO) + &
                                    NET_GENERATION_OTHER(MO)

         TOTAL_SYSTEM_GEN_NET(MO) = &
                                    TOTAL_GENERATION(MO)  + &
                                    PURCHASE_POWER(MO) + &
                                    RECEIVED_FOR_SEPA_CUST(MO) - &
                                    DELIVERD_FOR_SEPA_CUST(MO)

         ENERGY_UNACCOUNTED_FOR (MO) = TOTAL_SYSTEM_GEN_NET(MO) - &
                                              TOTAL_SYSTEM_INPUT_NET(MO)
         TOTAL_SYSTEM_INPUT_NET(MO) = TOTAL_SYSTEM_INPUT_NET(MO) + &
                                             ENERGY_UNACCOUNTED_FOR (MO)
         WRITE(CPL_SALES_GEN_UNIT,REC=CPL_SALES_GEN_REC) &
            PRT_ENDPOINT(),FLOAT(year_loc),CL_MONTH_NAME(MO), &
            ASSET_CLASS_NAME, & !  HARD CODED TO PARENT
            RESIDENTIAL_MWH(MO),COMMERCIAL_MWH(MO),INDUSTRIAL_MWH(MO), &
            PUBLIC_STREET_HIGHWAY(MO),OTHER_PUBLIC(MO),TOTAL_RETAIL(MO), &
            NCEMPA_SPECIAL_CONTRACT(MO),NCEMC_SUPPLEMENTAL(MO), &
            WHOLESALE_INCL_UNBILLED (MO),SALES_TO_OTHER_UTILITIES (MO), &
            TOTAL_SALES_ENERGY (MO),ENERGY_USED_BY_COMPANY (MO), &
            ENERGY_UNACCOUNTED_FOR (MO), &
            TOTAL_SYSTEM_INPUT_NET (MO), &
            NET_GENERATION_STEAM (MO), &
            NET_GENERATION_NUCLEAR (MO), &
            NET_GENERATION_HYDRO (MO), &
            NET_GENERATION_OTHER (MO), &
            TOTAL_GENERATION (MO), &
            PURCHASE_POWER (MO), &
            RECEIVED_FOR_SEPA_CUST(MO), &
            -1*DELIVERD_FOR_SEPA_CUST(MO), &
            TOTAL_SYSTEM_GEN_NET(MO), &
            POWER_AGENCY_ENTITLEMENT(MO)
         CPL_SALES_GEN_REC = CPL_SALES_GEN_REC + 1
      ENDDO !MO
      RETURN
      END


