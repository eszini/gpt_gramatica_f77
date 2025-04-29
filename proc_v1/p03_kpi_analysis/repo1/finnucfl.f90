
!     FINNUCFL.FOR
!     Copyright(c) M.S. Gerber & Associates 2000
!
!     Created: 4/21/2003 8:56:12 AM
!     Author : MARK S GERBER
!     Last change: MSG 1/10/2010 2:59:37 PM


!   FILE NUCFUEL.FOR 2/16/84

!
!           SUBROUTINE TO CALCULATE NUCLEAR FUEL VALUES
!           COPYRIGHT (C) 1983 M.S. GERBER & ASSOCIATES, INC.
!                      ALL RIGHTS RESERVED
!
!           MIDSA GOLD CAPABILITIES ADDED JUNE, 1991
!           COPYRIGHT (C) 1991 M.S. GERBER & ASSOCIATES, INC.
!                      ALL RIGHTS RESERVED
!

!
      SUBROUTINE NF_OBJT
      use end_routine, only: end_program, er_message
      use grx_planning_routines
      use finnucfl_decs

      use SpinDriftLib
      use prod_arrays_dimensions
      use sizecom
      implicit none
      INTEGER(kind=2) :: IREC,AFDC_CAP_VECTOR,DELETE,ACCTNO,INUNIT, &
                         LRECL=512
      INTEGER :: IOS
      CHARACTER(len=5) :: BASE_FILE_NAME,OVERLAY_FAMILY_NAME,NUCFLFIL
      INTEGER(kind=2) :: UNIT_NUM=10, &
                ASSET_CLASS_NUM,ASSET_CLASS_VECTOR
      REAL :: MARCS_TAX_LIFE,MARCS_DB_RATE,LEASOR_WRITE_OFF_PERIOD
      INTEGER(kind=2) :: R_NUM_OF_CLASSES,R_MAX_CLASS_NUM, &
                R_CLASS_POINTERS(*)
      REAL :: ASSET_CLASS_LIST(:)
      ALLOCATABLE ASSET_CLASS_LIST
      INTEGER(kind=2) :: NUM_OF_OL_ASSET_CLASSES=0, &
                MAX_OL_NUC_FUEL_ID_NUM=0
      INTEGER(kind=2) :: NUM_OF_BC_ASSET_CLASSES=0, &
                MAX_BC_NUC_FUEL_ID_NUM=0
      INTEGER(kind=2) :: R_UNIT_NUM
      INTEGER(kind=2) :: BC_NUC_FUEL_CLASS_POINTER(:), &
                OL_NUC_FUEL_CLASS_POINTER(:), &
                TEMP_ASSET_CLASS_POINTER(:)
      ALLOCATABLE :: BC_NUC_FUEL_CLASS_POINTER, &
                     OL_NUC_FUEL_CLASS_POINTER, &
                     TEMP_ASSET_CLASS_POINTER
      SAVE BC_NUC_FUEL_CLASS_POINTER, &
           OL_NUC_FUEL_CLASS_POINTER
      CHARACTER(len=30) :: DESC,COMMENT
      CHARACTER(len=256) :: FILE_NAME
      CHARACTER(len=256) :: BASE_FILE_DIRECTORY,OUTPUT_DIRECTORY
      CHARACTER(len=256) :: DATA_DRIVE
      LOGICAL(kind=4) :: NF_FILE_EXISTS=.FALSE.
!  DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER(len=1024) :: RECLN
!  DECLARATION FOR /FUTURE ASSETS FILE/
      CHARACTER(len=1) :: ABACCT,DATA_TYPE
      REAL :: CONSTRUCTION_PERIOD,PV_PLANT_COST, &
           REGULATORY_ALLOCATOR
      REAL :: AFUDC1,AFDCB1,BOKLF,ABTAX,WOYRS,PLANT(15),CASH(15)
      INTEGER(kind=2) :: AFDCSW,ABYEAR,ABMETH,FIRSTYR, &
           CONSTRUCTION_ESCALATION_VECTOR,AFDCPERIODS
!      ADDED MIDAS GOLD 7/24/91
      INTEGER(kind=2) :: CASH_VECT,PLANT_VECT
!  DECLARATION FOR INPUT VARIABLES FOR NUCLEAR ASSETS
      REAL :: NFIP1,NFIPRB,NFIS1,AMORT1
      CHARACTER(len=30) :: FUEL_OWNERSHIP
      CHARACTER(len=21) :: FILE_TYPE='Nuclear Fuel Accounts'
      CHARACTER(len=2) :: NFUEL_OL='BC',R_NFUEL_OL
!
!  7/15/96 VARIABLES ADDED FOR OE
!
      INTEGER(kind=2) :: GENERATOR_ID
      REAL(kind=4) :: REGION_BALANCE(4),REGION_AMORT_RATE(4), &
             REGION_CAP_ALLOCATION(4)
      REAL(kind=4) :: LEASOR_MARK_UP,ALLOCATION_TO_OWNER
      LOGICAL(kind=1) :: R_NUC_FUEL_VERS_5,NUC_FUEL_VERS_5=.FALSE.
      LOGICAL(kind=4) :: FILE_OPENED
      CHARACTER(len=20) :: FORM_TYPE
      CHARACTER(len=40) :: LAG_STR,CASH_STR
      LOGICAL(kind=1) :: LAHEY_LF95
      CHARACTER(len=30) :: SCREEN_OUTPUT
      CHARACTER(len=1) :: ACCOUNT_ACTIVE

!
!           ROUTINE TO CONVERT METAFILE FILES TO DIRECT-ACESS BINARY
!           COPYRIGHT (C) 1983, 84, 85  M.S. GERBER & ASSOCIATES, INC.
!

!
!  CONVERT THE NUCLEAR FUEL FILE
      ENTRY NF_MAKEBIN
      BASE_FILE_NAME = NUCFLFIL()
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME=trim(BASE_FILE_DIRECTORY())// &
                                   "NFB"//trim(BASE_FILE_NAME)//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=NF_FILE_EXISTS)
      IF(NF_FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
            CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
         ENDIF
         ALLOCATE(TEMP_ASSET_CLASS_POINTER(1024), &
                  ASSET_CLASS_LIST(AVAIL_DATA_YEARS))
         TEMP_ASSET_CLASS_POINTER = 0
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCNFUEL.BIN",ACCESS="DIRECT", &
                                            STATUS="UNKNOWN",RECL=LRECL)
         IREC = 1
         DATA_TYPE = 'D'
         AFDC_CAP_VECTOR = 0
         PLANT_VECT = 0
         CASH_VECT = 0
         ASSET_CLASS_NUM = 0
         ASSET_CLASS_VECTOR = 0
         MARCS_TAX_LIFE = 99.
         MARCS_DB_RATE = 0.
         LEASOR_WRITE_OFF_PERIOD = 99.
         FIRSTYR = 1995
         REGION_BALANCE(1) = 0.
         REGION_BALANCE(2) = 0.
         REGION_BALANCE(3) = 0.
         REGION_BALANCE(4) = 0.
         REGION_AMORT_RATE(1) = 0.
         REGION_AMORT_RATE(2) = 0.
         REGION_AMORT_RATE(3) = 0.
         REGION_AMORT_RATE(4) = 0.
         REGION_CAP_ALLOCATION(1) = 0.
         REGION_CAP_ALLOCATION(2) = 0.
         REGION_CAP_ALLOCATION(3) = 0.
         REGION_CAP_ALLOCATION(4) = 0.
         LEASOR_MARK_UP = 0.
         ALLOCATION_TO_OWNER = 100.
         LAG_STR = "100"
         CASH_STR = "0"
         ACCOUNT_ACTIVE = 'A'
!
         READ(10,*) DELETE
         DO
            GENERATOR_ID = 0
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(IOS /=0) EXIT
            RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
            PLANT = 0.
            CASH = 0.
            READ(RECLN,*,ERR=200) DELETE,ACCTNO,NFIP1,NFIPRB,NFIS1, &
                 AMORT1,FUEL_OWNERSHIP,AFUDC1,AFDCB1,AFDCSW, &
                 CONSTRUCTION_PERIOD,AFDCPERIODS,BOKLF,ABYEAR, &
                 ABMETH,ABACCT,ABTAX,WOYRS,FIRSTYR, &
                 REGULATORY_ALLOCATOR,CASH,PLANT,DESC,COMMENT, &
                 DATA_TYPE,PV_PLANT_COST,CONSTRUCTION_ESCALATION_VECTOR, &
                 PLANT_VECT,CASH_VECT,AFDC_CAP_VECTOR, &
                 ASSET_CLASS_NUM,ASSET_CLASS_VECTOR, &
                 MARCS_TAX_LIFE,MARCS_DB_RATE,LEASOR_WRITE_OFF_PERIOD, &
                 GENERATOR_ID, &
                 REGION_BALANCE, &
                 REGION_AMORT_RATE, &
                 REGION_CAP_ALLOCATION, &
                 LEASOR_MARK_UP, &
                 ALLOCATION_TO_OWNER, &
                 LAG_STR,CASH_STR, &
                 ACCOUNT_ACTIVE
!
          IF(.NOT. (DELETE >= 8 .OR. ACCOUNT_ACTIVE == 'N')) THEN
             CALL SET_ASSET_CLASSES(ASSET_CLASS_NUM, &
                                   NUM_OF_BC_ASSET_CLASSES, &
                                    MAX_BC_NUC_FUEL_ID_NUM, &
                                   TEMP_ASSET_CLASS_POINTER)
               NUC_FUEL_VERS_5 = NUC_FUEL_VERS_5 .OR. GENERATOR_ID > 0
            ENDIF
!
            WRITE(11,REC=IREC) DELETE,ACCTNO,NFIP1,NFIPRB,NFIS1, &
               AMORT1,FUEL_OWNERSHIP,AFUDC1,AFDCB1,AFDCSW, &
               CONSTRUCTION_PERIOD,AFDCPERIODS,BOKLF,ABYEAR, &
               ABMETH,ABACCT,ABTAX,WOYRS,FIRSTYR, &
               REGULATORY_ALLOCATOR,CASH,PLANT,DATA_TYPE,PV_PLANT_COST, &
               CONSTRUCTION_ESCALATION_VECTOR, &
               PLANT_VECT,CASH_VECT,AFDC_CAP_VECTOR, &
               ASSET_CLASS_NUM,ASSET_CLASS_VECTOR, &
               MARCS_TAX_LIFE,MARCS_DB_RATE,LEASOR_WRITE_OFF_PERIOD, &
               GENERATOR_ID, &
               REGION_BALANCE, &
               REGION_AMORT_RATE, &
               REGION_CAP_ALLOCATION, &
               LEASOR_MARK_UP, &
               ALLOCATION_TO_OWNER, &
               LAG_STR,CASH_STR, &
               ACCOUNT_ACTIVE, &
               DESC
            IREC = IREC + 1
         ENDDO
         CLOSE(10)

         CLOSE(11)
         IF(MAX_BC_NUC_FUEL_ID_NUM > 0) THEN
            ALLOCATE(BC_NUC_FUEL_CLASS_POINTER(MAX_BC_NUC_FUEL_ID_NUM))
            BC_NUC_FUEL_CLASS_POINTER = &
                      TEMP_ASSET_CLASS_POINTER(1:MAX_BC_NUC_FUEL_ID_NUM)
         ENDIF
         DEALLOCATE(TEMP_ASSET_CLASS_POINTER,ASSET_CLASS_LIST)
      ELSE IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "finnucfl:1111")
      ENDIF
      RETURN



!           ROUTINE TO CREATE OVERLAY FILES
!           COPYRIGHT (C) 1984-88  M.S. GERBER & ASSOCIATES, INC.
!           COPYRIGHT (C) 1991-92  M.S. GERBER & ASSOCIATES, INC.



!  OVERLAY THE NUCLEAR FUEL FILE
      ENTRY NF_MAKEOVL(OVERLAY_FAMILY_NAME)

      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME=trim(DATA_DRIVE)//"NFO"// &
                                     trim(OVERLAY_FAMILY_NAME)//".DAT"
      OPEN(10,FILE=FILE_NAME)
      READ(10,*) DELETE
      INUNIT = 12
      IF(NFUEL_OL == 'BC') THEN
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCNFUEL.BIN", &
                                             ACCESS="DIRECT",RECL=LRECL)
         INUNIT = 11
      ENDIF
      OPEN(12,FILE=trim(DATA_DRIVE)//"OLNFUEL.BIN",ACCESS="DIRECT", &
                                            STATUS="UNKNOWN",RECL=LRECL)
      ALLOCATE(TEMP_ASSET_CLASS_POINTER(1024), &
                  ASSET_CLASS_LIST(AVAIL_DATA_YEARS))
      TEMP_ASSET_CLASS_POINTER = 0
      NUM_OF_OL_ASSET_CLASSES = 0
      MAX_OL_NUC_FUEL_ID_NUM = 0
      IREC = 0
      DO
         IREC = IREC + 1
         READ(INUNIT,REC=IREC,IOSTAT=IOS) DELETE,ACCTNO,NFIP1,NFIPRB, &
            NFIS1,AMORT1,FUEL_OWNERSHIP,AFUDC1,AFDCB1,AFDCSW, &
            CONSTRUCTION_PERIOD,AFDCPERIODS,BOKLF,ABYEAR, &
            ABMETH,ABACCT,ABTAX,WOYRS,FIRSTYR, &
            REGULATORY_ALLOCATOR,CASH,PLANT,DATA_TYPE,PV_PLANT_COST, &
            CONSTRUCTION_ESCALATION_VECTOR, &
            PLANT_VECT,CASH_VECT,AFDC_CAP_VECTOR, &
            ASSET_CLASS_NUM,ASSET_CLASS_VECTOR, &
            MARCS_TAX_LIFE,MARCS_DB_RATE,LEASOR_WRITE_OFF_PERIOD, &
            GENERATOR_ID, &
            REGION_BALANCE, &
            REGION_AMORT_RATE, &
            REGION_CAP_ALLOCATION, &
            LEASOR_MARK_UP, &
            ALLOCATION_TO_OWNER, &
            LAG_STR,CASH_STR, &
            ACCOUNT_ACTIVE, &
            DESC
         IF(IOS /= 0) EXIT
         READ(10,1000,IOSTAT=IOS) RECLN
         IF(IOS == 0) THEN

            RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                                   ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
            READ(RECLN,*,ERR=200) DELETE,ACCTNO,NFIP1,NFIPRB,NFIS1, &
               AMORT1,FUEL_OWNERSHIP,AFUDC1,AFDCB1,AFDCSW, &
               CONSTRUCTION_PERIOD,AFDCPERIODS,BOKLF,ABYEAR, &
               ABMETH,ABACCT,ABTAX,WOYRS,FIRSTYR, &
               REGULATORY_ALLOCATOR,CASH,PLANT,DESC,COMMENT, &
               DATA_TYPE,PV_PLANT_COST, &
               CONSTRUCTION_ESCALATION_VECTOR, &
               PLANT_VECT,CASH_VECT,AFDC_CAP_VECTOR, &
               ASSET_CLASS_NUM,ASSET_CLASS_VECTOR, &
               MARCS_TAX_LIFE,MARCS_DB_RATE,LEASOR_WRITE_OFF_PERIOD, &
               GENERATOR_ID, &
               REGION_BALANCE, &
               REGION_AMORT_RATE, &
               REGION_CAP_ALLOCATION, &
               LEASOR_MARK_UP, &
               ALLOCATION_TO_OWNER, &
               LAG_STR,CASH_STR, &
               ACCOUNT_ACTIVE
         ENDIF
!
         IF(.NOT. (DELETE >= 8 .OR. ACCOUNT_ACTIVE == 'N')) THEN
          CALL SET_ASSET_CLASSES(ASSET_CLASS_NUM, &
                                   NUM_OF_OL_ASSET_CLASSES, &
                                  MAX_OL_NUC_FUEL_ID_NUM, &
                                   TEMP_ASSET_CLASS_POINTER)
         ENDIF
!
         WRITE(12,REC=IREC) DELETE,ACCTNO,NFIP1,NFIPRB,NFIS1, &
            AMORT1,FUEL_OWNERSHIP,AFUDC1,AFDCB1,AFDCSW, &
            CONSTRUCTION_PERIOD,AFDCPERIODS,BOKLF,ABYEAR, &
            ABMETH,ABACCT,ABTAX,WOYRS,FIRSTYR, &
            REGULATORY_ALLOCATOR,CASH,PLANT,DATA_TYPE,PV_PLANT_COST, &
            CONSTRUCTION_ESCALATION_VECTOR, &
            PLANT_VECT,CASH_VECT,AFDC_CAP_VECTOR, &
            ASSET_CLASS_NUM,ASSET_CLASS_VECTOR, &
            MARCS_TAX_LIFE,MARCS_DB_RATE,LEASOR_WRITE_OFF_PERIOD, &
            GENERATOR_ID, &
            REGION_BALANCE, &
            REGION_AMORT_RATE, &
            REGION_CAP_ALLOCATION, &
            LEASOR_MARK_UP, &
            ALLOCATION_TO_OWNER, &
            LAG_STR,CASH_STR, &
            ACCOUNT_ACTIVE, &
            DESC
      ENDDO
      CLOSE(10)
      CLOSE(12)
      IF(ALLOCATED(OL_NUC_FUEL_CLASS_POINTER)) &
                                   DEALLOCATE(OL_NUC_FUEL_CLASS_POINTER)
      IF(MAX_OL_NUC_FUEL_ID_NUM > 0) THEN
         ALLOCATE(OL_NUC_FUEL_CLASS_POINTER(MAX_OL_NUC_FUEL_ID_NUM))
         OL_NUC_FUEL_CLASS_POINTER = &
                      TEMP_ASSET_CLASS_POINTER(1:MAX_OL_NUC_FUEL_ID_NUM)
      ENDIF
      DEALLOCATE(TEMP_ASSET_CLASS_POINTER,ASSET_CLASS_LIST)
      IF(NFUEL_OL == 'BC') CLOSE(11)
      NFUEL_OL= 'OL'
      RETURN

  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from FINNUCFL SIID128'
      call end_program(er_message)


      ENTRY RESET_NFUEL_OL

         NFUEL_OL = 'BC'
      RETURN


      ENTRY GET_NFUEL_OL(R_NFUEL_OL)

         R_NFUEL_OL = NFUEL_OL
      RETURN


      ENTRY OPEN_NF_OUT_FILE(R_UNIT_NUM)

         OPEN(R_UNIT_NUM,FILE=trim(OUTPUT_DIRECTORY())//NFUEL_OL// &
                                  'NF_AST.BIN',ACCESS='DIRECT',RECL=128)
      RETURN

      ENTRY OPEN_NUCLEAR_FUEL_FILE(R_UNIT_NUM)

         UNIT_NUM = R_UNIT_NUM
         FILE_NAME=trim(OUTPUT_DIRECTORY())//NFUEL_OL//"NFUEL.BIN"
         IF(NF_FILE_EXISTS) THEN
            OPEN(UNIT_NUM,FILE=FILE_NAME,ACCESS="DIRECT", &
                                            STATUS="UNKNOWN",RECL=LRECL)
         ENDIF
      RETURN


      ENTRY OPEN_NF_BASE_CASE_FILE(R_UNIT_NUM)

      FILE_NAME = ' '
      INQUIRE(UNIT=R_UNIT_NUM,OPENED=FILE_OPENED,NAME=FILE_NAME)
      FILE_NAME = trim(OUTPUT_DIRECTORY())//'BC_NFUEL.BIN'
      INQUIRE(FILE=FILE_NAME,OPENED=FILE_OPENED,FORM=FORM_TYPE)
      OPEN(R_UNIT_NUM,FILE=FILE_NAME,FORM='UNFORMATTED')

      RETURN

      ENTRY CLOSE_NUCLEAR_FUEL_FILE

      INQUIRE(UNIT=UNIT_NUM,NAME=FILE_NAME)
         IF(NF_FILE_EXISTS) THEN
            CLOSE(UNIT_NUM,IOSTAT=IOS)
         ENDIF
      RETURN

      ENTRY RETURN_NUM_OF_NUC_FUEL_CLASSES(R_NUM_OF_CLASSES, &
                                               R_MAX_CLASS_NUM)

         IF(NFUEL_OL == 'OL') THEN
            R_NUM_OF_CLASSES = NUM_OF_OL_ASSET_CLASSES
            R_MAX_CLASS_NUM = MAX_OL_NUC_FUEL_ID_NUM
         ELSE
            R_NUM_OF_CLASSES = NUM_OF_BC_ASSET_CLASSES
            R_MAX_CLASS_NUM = MAX_BC_NUC_FUEL_ID_NUM
         ENDIF
      RETURN

      ENTRY RETURN_NUC_FUEL_POINTER(R_CLASS_POINTERS)

         IF(NFUEL_OL == 'OL') THEN
            R_CLASS_POINTERS(1:MAX_OL_NUC_FUEL_ID_NUM) = &
                     OL_NUC_FUEL_CLASS_POINTER(1:MAX_OL_NUC_FUEL_ID_NUM)
         ELSE
            R_CLASS_POINTERS(1:MAX_BC_NUC_FUEL_ID_NUM) = &
                     BC_NUC_FUEL_CLASS_POINTER(1:MAX_BC_NUC_FUEL_ID_NUM)
         ENDIF
      RETURN

      ENTRY RETURN_FILE_VERSION(R_NUC_FUEL_VERS_5)


         R_NUC_FUEL_VERS_5 = NUC_FUEL_VERS_5
      RETURN
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END


      RECURSIVE SUBROUTINE NUFUEL(SAVE_BASE_CASE)


!  NOTE: 6/30/94 AFDCRT,AFDCBR,INTEREST_CAP_RATE,
!  CURRENT_INTEREST_CAP_RATE ARE ONLY PASSED TO AFUDC

      use dsf2
      use endpoint
      use finnucfl_decs
      use globecom
      use grx_planning_routines
      use irec_endpoint_control
      use mod_base_year
      use prod_arrays_dimensions
      use prodcom
      use sizecom
      use spindriftlib
      implicit none

      SAVE
      
!
      LOGICAL(kind=1) :: SAVE_BASE_CASE,VOID_LOGICAL, &
                         RETURN_ASSET_CLASS_LISTS
      INTEGER(kind=2) :: FINANCIAL_SIMULATION_YEARS=0, &
                         RUN_YEARS,EXTENSION_YEARS
      INTEGER(kind=4) :: VALUES_TO_ZERO
!
      REAL, ALLOCATABLE :: ASSET_CLASS_LIST(:)
      REAL, ALLOCATABLE :: ASSET_ALLOCATION_LIST(:)
!
      INTEGER(kind=2) :: RUN_YEAR,R_CLASS
      REAL :: R_NFCE,R_NFIP,R_RBNFIP,R_NFES,R_NFIS,R_NNFV, &
           R_RBNF,R_NFAFC1,R_NAFC1C,R_NFAFC2,R_NAFC2C,R_NTXEXP,R_NFDDB, &
           R_NFDDRB,R_NAMTEX,R_NEXEXP,R_NAFEXP,R_ADJAFC,R_NWODFT, &
           R_NAFDCB,R_NAFDCD,R_LEASED_NFCE,R_NFCAPINRST, &
!  NUCLEAR FUEL RATEBASE ADDERS
           RB_NFCAPINRST,RB_NFTXEXP,RB_NFDBAMR,NF_RBCWIP_AFDCMETH2, &
           RB_NFAFC1,R_LEASOR_NF_SL_TAX_DEPRECIATION
      REAL :: R_TOTAL_NF_TAX_DEP,R_OWNED_NF_TAX_DEP, &
           R_LEASOR_NF_DEFERRED_TAX_BASIS, &
           R_LEASOR_FUEL_DEPRECIATION,R_LEASOR_NF_TAX_DEP

!
      INTEGER(kind=2) :: R_ABMETH,R_ABYEAR
      LOGICAL(kind=1) :: R_CLASS_EXISTS
      REAL :: R_TAXWO,R_PCLEASED,R_REGULATORY_ALLOCATOR,R_ABTAX
!
      REAL :: R_BOKBL,R_BOKDAL,R_BOKWO,R_AFCBL,R_AFCDAL,R_AFCWO,R_NRTXWO
      REAL :: TEMP1,TEMP2,TEMP3,CAFDCD,ALLOCATOR
      INTEGER(kind=2) :: LASTYR
!
      INTEGER(kind=2) :: I,J,IREC,IEND,IOFFSET,DELETE,TACCTS, &
        SERVICEMO_loc=7,AFDCPERIODS,CONSTRUCTION_ESCALATION_VECTOR, &
                AFDC_CAP_VECTOR,CASH_VECT,PLANT_VECT,IVEC, &
                LAST_YEAR,START_YEARS,FIRSTYR_VEC, &
                FIRST_YEARS_VEC(:)
      INTEGER :: IOS
      REAL :: CURRENT_INTEREST_CAP(:), &
           AFDC2B(:), &
           VECTOR_DATA_CASH(:), &
           VECTOR_DATA_PLNT(:), &
           VECTOR_DATA(AVAIL_DATA_YEARS)
      ALLOCATABLE :: CURRENT_INTEREST_CAP, &
                     AFDC2B,FIRST_YEARS_VEC, &
                     VECTOR_DATA_CASH, &
                     VECTOR_DATA_PLNT
!
      REAL :: TAXWO
      REAL :: NRTXWO,BOKBL,BOKLF,BOKWO,BOKDAL, &
           AFCBL,AFCWO,AFCDAL, &
           PV_PLANT_COST,FV_PLANT_COST,CURRENT_VALUE,TEMP_NFIP
      CHARACTER*1 DATA_TYPE,DUMMY_TYPE
      CHARACTER(len=1) :: ABACCT
      INTEGER(kind=2) :: ACCTNO,AFDCSW,ABYEAR,ABMETH,FIRSTYR
      REAL :: NFIS1,AMORT1,PCLEASED,NFIP1,NFIPRB,AFUDC1,AFDCB1

      REAL :: AFDCFC,ABTAX,WOYRS,CASH(15),PLANT(15),FUEL_EXPENSE(15), &
           REGULATORY_ALLOCATOR
      INTEGER(kind=2) :: BASEYEAR,CLASS,CLASS_POINTER, &
                ASSET_CLASS,ASSET_CLASS_VECTOR, &
                R_ASSET_CLASS,R_ASSET_CLASS_VECTOR


!      DECLARATION FOR INPUT VARIABLES FOR NUCLEAR ASSETS
      
      
      

      REAL :: NF_CLASS_TAX_LIFE
      CHARACTER(len=30) :: FUEL_OWNERSHIP
      REAL :: MARCS_TAX_LIFE,MARCS_DB_RATE,LEASOR_WRITE_OFF_PERIOD
      INTEGER(kind=4) :: VALUES_2_ZERO
!
      REAL :: NF_TAX_VALUE(:), &
           NF_SL_TAX_VALUE(:), &
           NF_BOOK_CAPITIALIZED(:), &
           NF_SL_TAX_DEPRECIATION(:)
      ALLOCATABLE :: NF_TAX_VALUE, &
                     NF_SL_TAX_VALUE, &
                     NF_BOOK_CAPITIALIZED, &
                     NF_SL_TAX_DEPRECIATION
!
      LOGICAL(kind=1) :: VOID_LOG,RETURN_A_VECTOR_FOR_ALL_YEARS
      REAL :: ESCALATION_RATES(:),ESCALATION_MULTIPLIER
      ALLOCATABLE ::  ESCALATION_RATES
      CHARACTER(len=30) :: DESC
!
      INTEGER(kind=2) :: NUM_OF_NUC_UNITS,R_NUM_OF_NUC_UNITS
      INTEGER(kind=2) :: ID_POINTER(:)
      REAL(kind=4) :: GENERATION_BY_INSERVICE_YEAR(:,:),GET_VAR
      ALLOCATABLE :: ID_POINTER,GENERATION_BY_INSERVICE_YEAR
      LOGICAL(kind=1) :: OFFSET_MAINTENANCE_VECTORS, &
                         OFFSET_MAINTENANCE_ACTIVE
      INTEGER(kind=2) :: DAYS_IN_EACH_MONTH,NUC_UNITS_FOUND,YR,MO, &
                DATE,GET_YR,RELOAD_YEAR
      INTEGER(kind=2) :: R_ON_LINE,R_OFF_LINE
      INTEGER(kind=2) :: R_UNIT_ID
      REAL(kind=4) :: R_UNIT_CAPACITY,R_EFOR,R_MAINTENANCE_RATE(12), &
                      R_FRAC_OWNED
      REAL(kind=4) :: MAINTENANCE_RATE(:,:,:), &
                      ENRG_BY_FISCAL_YEAR(:,:,:), &
             HOURS_GENERATION,ANNUAL_EFOR,ANNUAL_CAPACITY
      ALLOCATABLE :: MAINTENANCE_RATE,ENRG_BY_FISCAL_YEAR
      REAL(kind=4) :: MW_CAPACITY,MONTHLY_MW,MONTHLY_UNIT_CAPACITY(:,:), &
             FRAC_OWNED
      LOGICAL(kind=1) :: FUEL_PRICE_FROM_NF_FILE(:)
      ALLOCATABLE :: MONTHLY_UNIT_CAPACITY,FUEL_PRICE_FROM_NF_FILE
      INTEGER(kind=2) :: R_MONTHLY_CAPACITY_POINTR
      CHARACTER(len=*) R_UNIT_NAME
      LOGICAL(kind=1) :: REMAINING_GENERATION_FOUND
      INTEGER(kind=2) :: NUC_FUEL_BURN_YEARS=0,REGIONS_IN_CORE=0
      REAL(kind=4) :: ENRG_GENERATION
      INTEGER(kind=2) :: POS
!
!  7/15/96 VARIABLES ADDED FOR OE
!
      INTEGER(kind=2) :: GENERATOR_ID,BUNDLE_POINTER
      REAL(kind=4) :: OPENING_REGION_BALANCE(4),REGION_AMORT_RATE(4), &
             REGION_CAP_ALLOCATION(4), &
             LEASOR_MARK_UP, &
             ALLOCATION_TO_OWNER, &
             R_ALLOCATION_TO_OWNER(*), &
             OWNER_ALLOCATION(MAX_FINANCIAL_SIMULATION_YEARS)
      REAL(kind=4):: NUC_FUEL_COST(:,:,:),CURRENT_RATE,CAPITIALIZED_FUEL
      REAL(kind=4) :: ANNUAL_LEASOR_MARK_UP(:,:),R_MARK_UP
      ALLOCATABLE :: NUC_FUEL_COST,ANNUAL_LEASOR_MARK_UP
      INTEGER(kind=2) :: GEN_UNIT,MO_START
      LOGICAL(kind=1) :: CALUCLATE_NUC_FUEL_PRICES, &
                USE_NUC_FUEL_FILE_PRICES,NUC_FUEL_VERS_5, &
                R_NUC_GEN_UNIT_FOUND
!
      CHARACTER(len=4) :: YEAR_STR,LOAD_NAME(4)*7
      LOGICAL(kind=1) :: NUC_FUEL_REPORTING,NUCLEAR_FUEL_REPORT, &
                REPORT_ALL_ACCOUNTS
      INTEGER(kind=2) :: NUC_REPORTING_UNIT,NUC_FUEL_REGION_RPT
      INTEGER NUC_REPORTING_REC
      LOGICAL(kind=1) :: REPORT_HEADER_OPEN=.FALSE.
      REAL(kind=4) :: PRE_REGION_BURN(4),PRE_LOAD_BURN(4), &
             PRE_LOAD_CLOSE(4),FUELING_START_MONTH, &
             POST_REGION_AMORT_RATE(4),POST_REGION_BURN(4), &
             POST_REGION_BALANCE(4)
      REAL(kind=4) :: TOTAL_ENRG,AVERAGE_BURN_RATE, &
             AFUDC_IN_CWIP(MAX_FINANCIAL_SIMULATION_YEARS)
      LOGICAL(kind=1) :: HALF_YEAR_BOOK_DEP
      REAL(kind=4) :: MMBTU_PRICE
      INTEGER(kind=2) :: R_UNIT_POS,R_MO,R_YR
      CHARACTER(len=6) :: R_FUEL_PRICE_SOURCE
      REAL(kind=4) :: R_OWNED_NF_SL_TAX_DEP
      REAL(kind=4) :: ALLOCATION_VALUE(AVAIL_DATA_YEARS)
      REAL(kind=4) :: CLASS_ALLOCATOR
      INTEGER(kind=2) :: ALLOCATION_VECTOR
      CHARACTER(len=40) :: LAG_STR,CASH_STR
      CHARACTER(len=1) :: ACCOUNT_ACTIVE
      LOGICAL(kind=1) :: LF95,LAHEY_LF95
      REAL (KIND=4) :: MONTHLY_UNIT_EFOR(12)
      LF95 = LAHEY_LF95()
      HALF_YEAR_BOOK_DEP = .FALSE.
      CALL SET_UP_NUC_FUEL_ARRAYS
!
      CALL INIT_NUC_FUEL_ARRAYS
!
      ALLOCATE(FIRST_YEARS_VEC(AVAIL_DATA_YEARS), &
               VECTOR_DATA_CASH(AVAIL_DATA_YEARS), &
               VECTOR_DATA_PLNT(AVAIL_DATA_YEARS), &
               ESCALATION_RATES(AVAIL_DATA_YEARS))
!
      ALLOCATE(NFCE(MAX_FINANCIAL_SIMULATION_YEARS), &
             NFES(MAX_FINANCIAL_SIMULATION_YEARS), &
             AFDC1(MAX_FINANCIAL_SIMULATION_YEARS), &
             CWIP(MAX_FINANCIAL_SIMULATION_YEARS), &
             AFDC2(MAX_FINANCIAL_SIMULATION_YEARS), &
             RBCWIP(MAX_FINANCIAL_SIMULATION_YEARS), &
             AFDC1B(MAX_FINANCIAL_SIMULATION_YEARS), &
             AFDC2B(MAX_FINANCIAL_SIMULATION_YEARS), &
             CURRENT_INTEREST_CAP(MAX_FINANCIAL_SIMULATION_YEARS), &
             CAPINRST(MAX_FINANCIAL_SIMULATION_YEARS), &
             PCAPINRST(MAX_FINANCIAL_SIMULATION_YEARS), &
             NF_TAX_DEP(MAX_FINANCIAL_SIMULATION_YEARS), &
             NF_DEFERRED_TAX_BASIS(MAX_FINANCIAL_SIMULATION_YEARS), &
             NF_BOOK_DEPRECIATION(MAX_FINANCIAL_SIMULATION_YEARS))
      ALLOCATE(NFIS(FINANCIAL_SIMULATION_YEARS), &
             NNFV(FINANCIAL_SIMULATION_YEARS), &
             TAXEXP(FINANCIAL_SIMULATION_YEARS), &
             AFDC1C(FINANCIAL_SIMULATION_YEARS), &
             DDB(FINANCIAL_SIMULATION_YEARS), &
             RBDDB(FINANCIAL_SIMULATION_YEARS), &
             AFCEXP(FINANCIAL_SIMULATION_YEARS), &
             AJAFDC(FINANCIAL_SIMULATION_YEARS), &
             EXEXP(FINANCIAL_SIMULATION_YEARS), &
             DBAMRT(FINANCIAL_SIMULATION_YEARS), &
             NFRB(FINANCIAL_SIMULATION_YEARS), &
             AFDC2C(FINANCIAL_SIMULATION_YEARS), &
             WODFTX(FINANCIAL_SIMULATION_YEARS), &
             AFDCDF(FINANCIAL_SIMULATION_YEARS), &
             NF_TAX_VALUE(FINANCIAL_SIMULATION_YEARS), &
             NF_SL_TAX_VALUE(FINANCIAL_SIMULATION_YEARS), &
             NF_BOOK_CAPITIALIZED(FINANCIAL_SIMULATION_YEARS), &
             NF_SL_TAX_DEPRECIATION(FINANCIAL_SIMULATION_YEARS))
!
      AFDCDF = 0.
!
!  READ A DATA RECORD
!
      IF(ALLOCATED(ASSET_CLASS_LIST)) DEALLOCATE(ASSET_CLASS_LIST, &
                                                  ASSET_ALLOCATION_LIST)
      ALLOCATE(ASSET_CLASS_LIST(AVAIL_DATA_YEARS), &
                                ASSET_ALLOCATION_LIST(AVAIL_DATA_YEARS))
      CALL OPEN_NUCLEAR_FUEL_FILE(INT(11,2))
      IF(ALLOCATED(NUC_FUEL_COST)) DEALLOCATE(NUC_FUEL_COST, &
                                              ANNUAL_LEASOR_MARK_UP)
      ALLOCATE( &
             NUC_FUEL_COST(12,0:NUC_FUEL_BURN_YEARS,0:NUM_OF_NUC_UNITS), &
        ANNUAL_LEASOR_MARK_UP(0:NUC_FUEL_BURN_YEARS,0:NUM_OF_NUC_UNITS))
!
      NUC_FUEL_COST = 0.
      ANNUAL_LEASOR_MARK_UP = 0.
!
      TACCTS = 0
      VALUES_2_ZERO = INT(FINANCIAL_SIMULATION_YEARS)
      BASEYEAR = get_base_year()
      IREC = 1
      CALL RETURN_FILE_VERSION(NUC_FUEL_VERS_5)
      CALUCLATE_NUC_FUEL_PRICES = NUC_FUEL_VERS_5 .AND. &
                                                   ALLOCATED(ID_POINTER)
      NUC_FUEL_REPORTING = NUCLEAR_FUEL_REPORT(REPORT_ALL_ACCOUNTS).AND. &
                           CALUCLATE_NUC_FUEL_PRICES .AND. &
                           .NOT. SAVE_BASE_CASE
      IF(NUC_FUEL_REPORTING .AND. .NOT. REPORT_HEADER_OPEN) THEN
         NUC_REPORTING_UNIT = NUC_FUEL_REGION_RPT(NUC_REPORTING_REC)
         REPORT_HEADER_OPEN = .TRUE.
      ENDIF
      IF(LF95) THEN
         WRITE(SCREEN_MESSAGES,"(A)") "Nuclear Fuel Accounts"
         CALL MG_LOCATE_WRITE(18,70,trim(SCREEN_MESSAGES),3,2)
      ENDIF
!
      DO
         CASH = 0.
         PLANT = 0.
         FUEL_EXPENSE = 0.
         NF_CLASS_TAX_LIFE = 3.
!
         READ(11,REC=IREC,IOSTAT=IOS) DELETE,ACCTNO,NFIP1,NFIPRB,NFIS1, &
                AMORT1,FUEL_OWNERSHIP,AFUDC1,AFDCB1,AFDCSW,AFDCFC, &
                AFDCPERIODS, &
                BOKLF,ABYEAR,ABMETH,ABACCT,ABTAX,WOYRS,FIRSTYR_VEC, &
                REGULATORY_ALLOCATOR,CASH,PLANT,DATA_TYPE,PV_PLANT_COST, &
                CONSTRUCTION_ESCALATION_VECTOR, &
                PLANT_VECT,CASH_VECT,AFDC_CAP_VECTOR, &
                ASSET_CLASS,ASSET_CLASS_VECTOR, &
                MARCS_TAX_LIFE,MARCS_DB_RATE,LEASOR_WRITE_OFF_PERIOD, &
                GENERATOR_ID, &
                OPENING_REGION_BALANCE, &
                REGION_AMORT_RATE, &
                REGION_CAP_ALLOCATION, &
                LEASOR_MARK_UP, &
                ALLOCATION_TO_OWNER, &
                LAG_STR,CASH_STR, &
                ACCOUNT_ACTIVE, &
                DESC
         IF(IOS /= 0) EXIT
         IREC = IREC + 1
         IF(DELETE > 7 .OR. ACCOUNT_ACTIVE == 'N') CYCLE
         TACCTS = TACCTS + 1
         IF(LF95) THEN
            WRITE(SCREEN_MESSAGES,"(I4,A)") TACCTS,"-"//DESC
            CALL MG_LOCATE_WRITE(19,71,trim(SCREEN_MESSAGES),3,0)
         ELSE
            WRITE(SCREEN_MESSAGES,"(I3)") TACCTS
            CALL MG_LOCATE_WRITE(19,71,trim(SCREEN_MESSAGES), &
                                                      ALL_VERSIONS,0)
         ENDIF
         REGIONS_IN_CORE = 3
         IF(REGION_CAP_ALLOCATION(4) /= 0.) REGIONS_IN_CORE = 4
         IF(ABYEAR > gc_last_study_year) ABYEAR = 3050
         IF(ALLOCATION_TO_OWNER < 0) THEN
            IVEC = ABS(ALLOCATION_TO_OWNER)
            CALL GET_ASSET_VAR(IVEC,DUMMY_TYPE,VECTOR_DATA)
            OWNER_ALLOCATION(1) = VECTOR_DATA(1)/100.
            DO I = 2, FINANCIAL_SIMULATION_YEARS
               IF(I <= AVAIL_DATA_YEARS+1) THEN
                  OWNER_ALLOCATION(I) = VECTOR_DATA(I-1)/100.
               ELSE
                  OWNER_ALLOCATION(I)=VECTOR_DATA(AVAIL_DATA_YEARS)/100.
               ENDIF
            ENDDO
         ELSE
            DO I = 1, FINANCIAL_SIMULATION_YEARS
               OWNER_ALLOCATION(I) = ALLOCATION_TO_OWNER/100.
            ENDDO
         ENDIF
         IF(FIRSTYR_VEC < 0) THEN
            IVEC = ABS(FIRSTYR_VEC)
            CALL GET_ASSET_VAR(IVEC,DUMMY_TYPE,VECTOR_DATA)
            LAST_YEAR = 0
            DO WHILE ((VECTOR_DATA(LAST_YEAR+1) > 1900) .AND. &
                                                       (LAST_YEAR < 30))
               IF(VECTOR_DATA(LAST_YEAR+1) - BASEYEAR <= 30) THEN
                  LAST_YEAR = LAST_YEAR + 1
                  FIRST_YEARS_VEC(LAST_YEAR) = VECTOR_DATA(LAST_YEAR+1)
               ENDIF
            ENDDO
            IF(LAST_YEAR == 0) CYCLE
         ELSE
            LAST_YEAR = 1
            FIRST_YEARS_VEC(1) = FIRSTYR_VEC
         ENDIF
         IF(CASH_VECT /= 0) THEN
            IVEC = ABS(CASH_VECT)
            CALL GET_ASSET_VAR(IVEC,DUMMY_TYPE,VECTOR_DATA_CASH)
         ENDIF
         IF(PLANT_VECT /= 0) THEN
            IVEC = ABS(PLANT_VECT)
            CALL GET_ASSET_VAR(IVEC,DUMMY_TYPE,VECTOR_DATA_PLNT)
         ENDIF
         PCLEASED = 0.
         IF(INDEX(FUEL_OWNERSHIP,"Leased") /= 0 .OR. &
                                  INDEX(FUEL_OWNERSHIP,"100") /= 0) THEN
            PCLEASED = 1.
            NFIPRB = 0.0
            AFDCSW = 1
            AFDCFC = 0.0
            AFDCPERIODS = 0
            ABYEAR = 0
            REGULATORY_ALLOCATOR = 0.
         ELSE
            NF_CLASS_TAX_LIFE = MARCS_TAX_LIFE
         ENDIF
         ABTAX  = ABTAX/100.
         REGULATORY_ALLOCATOR = REGULATORY_ALLOCATOR/100.
         AFDCFC = AFDCFC/2.
         DO START_YEARS = 1, LAST_YEAR
            NFES = 0.
            NFCE = 0.
            FIRSTYR = FIRST_YEARS_VEC(START_YEARS)
            IOFFSET = MIN(MAX_FINANCIAL_SIMULATION_YEARS, &
                          MAX(FIRSTYR - BASEYEAR,INT(1,2)))
            IF(IOFFSET >= MAX_FINANCIAL_SIMULATION_YEARS) EXIT
            IEND = MAX(INT(0,2),MIN(INT(15,2), &
                       MAX_FINANCIAL_SIMULATION_YEARS-IOFFSET))
            IF(CASH_VECT /= 0) THEN
               I = 1
               J = 1
               DOWHILE ((I+IOFFSET) <= MAX_FINANCIAL_SIMULATION_YEARS)
                  IF(J > AVAIL_DATA_YEARS) EXIT
                  NFCE(I+IOFFSET) = VECTOR_DATA_CASH(J)
                  I = I + 1
                  J = J + 1
               ENDDO
            ENDIF
            IF(PLANT_VECT /= 0) THEN
               I = 1
               J = 1
               DOWHILE ((I+IOFFSET) <= MAX_FINANCIAL_SIMULATION_YEARS)
                  IF(J > AVAIL_DATA_YEARS) EXIT
                  NFES(I+IOFFSET) = VECTOR_DATA_PLNT(J)
                  I = I + 1
                  J = J + 1
               ENDDO
            ENDIF
            IF(DATA_TYPE == 'P' .OR. DATA_TYPE == 'B') THEN
!
               FV_PLANT_COST = PV_PLANT_COST
!
!  NEED TO HAVE A MEANS TO HAVE A PLANT UNDER CONSTRUCTION
!
               IF(CASH_VECT /= 0) THEN
                  DO I = 1, MAX_FINANCIAL_SIMULATION_YEARS
                     NFCE(I)  = NFCE(I) * FV_PLANT_COST/100.
                  ENDDO
               ELSE
                  DO I = 1, IEND
                     NFCE(I+IOFFSET)  = CASH(I) * FV_PLANT_COST / 100.
                  ENDDO
               ENDIF
               IF(PLANT_VECT /= 0) THEN
                  DO I = 1, MAX_FINANCIAL_SIMULATION_YEARS
                     NFES(I)  = NFES(I)/100.
                  ENDDO
               ELSE
                  DO I = 1, IEND
                     NFES(I+IOFFSET) = PLANT(I) / 100.
                  ENDDO
               ENDIF
               VOID_LOG=RETURN_A_VECTOR_FOR_ALL_YEARS(ESCALATION_RATES, &
                                         CONSTRUCTION_ESCALATION_VECTOR)
               CURRENT_VALUE = 1.
               DO I = 2, MAX_FINANCIAL_SIMULATION_YEARS
                  IF(I <= AVAIL_DATA_YEARS + 1) THEN
                     ESCALATION_MULTIPLIER = ESCALATION_RATES(I-1)
                  ENDIF
                  CURRENT_VALUE = CURRENT_VALUE * ESCALATION_MULTIPLIER
                  NFCE(I)  = CURRENT_VALUE * NFCE(I)
               ENDDO
               TEMP_NFIP = NFIP1
               DO I = 2, MAX_FINANCIAL_SIMULATION_YEARS
                  TEMP_NFIP = TEMP_NFIP + NFCE(I)
                  NFES(I)  = MIN(NFES(I),1.) * TEMP_NFIP
                  TEMP_NFIP = TEMP_NFIP - NFES(I)
               ENDDO
               IF(DATA_TYPE == 'P') AFDCSW = 2
!
            ELSE
               TEMP_NFIP = NFIP1
               IF(CASH_VECT == 0) THEN
                  DO I = 1, IEND
                     NFCE(I+IOFFSET)  = CASH(I)
                  ENDDO
               ENDIF
               IF(PLANT_VECT == 0) THEN
                  DO I = 1, IEND
                     NFES(I+IOFFSET)  = PLANT(I)
                  ENDDO
               ENDIF
               DO I = 2, MAX_FINANCIAL_SIMULATION_YEARS
                  TEMP_NFIP  = TEMP_NFIP + NFCE(I)
                  IF(DATA_TYPE == 'R') THEN
                     NFES(I)   = MIN(NFES(I),100.)*TEMP_NFIP/100.
                  ELSE
                     NFES(I) = MIN(NFES(I),TEMP_NFIP)
                  ENDIF
                  TEMP_NFIP  = TEMP_NFIP - NFES(I)
               ENDDO
            ENDIF
            IF(AFDCSW > 2) AFDCPERIODS = 0

        CALL AFUDC(AFUDC1,NFIPRB,AFDCB1,AFDCFC,NFIP1,AFDCSW, &
               AFDCPERIODS,NFCE,NFES,AFDC1,AFDC2,CWIP,RBCWIP, &
               AFDC1B,AFDC2B,SERVICEMO_loc,CAPINRST,'ACRS', &
               PCAPINRST,AFDC_CAP_VECTOR,FIRSTYR, &
               CURRENT_INTEREST_CAP,'NF', &
               MAX_FINANCIAL_SIMULATION_YEARS,NF_CLASS_TAX_LIFE, &
               AFUDC_IN_CWIP)

! CALCULATE BOOK ITEMS FOR NUCLEAR FUEL
!   INITIALIZE VALUES

            NFIS(1)   = NFIS1
            AFDC1C(1) = AFDC1(1)
            AFDC2C(1) = AFDC2(1)
            NNFV(1)   = NFIS1 + CWIP(1) + AFDC1(1) - AMORT1
            NFRB(1)   = REGULATORY_ALLOCATOR * (NFIS1 - AMORT1)
            TAXEXP(1) = 0.0
            NF_TAX_VALUE(1) = 0.
            NF_SL_TAX_VALUE(1) = 0.
!
            DO I = 2,FINANCIAL_SIMULATION_YEARS
               TAXEXP(I) = 0.0
               NFIS(I) = NFIS(I-1) + NFES(I)
               AFDC1C(I) = AFDC1C(I-1) + AFDC1(I)
               AFDC2C(I) = AFDC2C(I-1) + AFDC2(I)
               NNFV(I) = NFIS(I) + CWIP(I) + AFDC1C(I) - AMORT1
               NFRB(I) = REGULATORY_ALLOCATOR*(NFIS(I)+AFDC2C(I)-AMORT1)
               NF_TAX_VALUE(I) = NFES(I) + CAPINRST(I)
               NF_SL_TAX_VALUE(I) = NFES(I) + CURRENT_INTEREST_CAP(I)
               NF_BOOK_CAPITIALIZED(I) = NFES(I) + AFDC2(I)
            ENDDO
!
!  OWNED OR LEASOR FUEL NEED TAX DEP AND BOOK DEP
!
            NF_TAX_DEP = 0.
            IF(INDEX(FUEL_OWNERSHIP,"Owned") /= 0) THEN
               CALL MARCS_TAX_DEPRECIATION(MARCS_TAX_LIFE, &
                                           MARCS_DB_RATE/100., &
                                           NF_TAX_DEP,NF_TAX_VALUE, &
                                           FINANCIAL_SIMULATION_YEARS)

               IF(INDEX(FUEL_OWNERSHIP,"Leasor") /= 0) THEN
                  IF(LEASOR_WRITE_OFF_PERIOD <= 99.) THEN

                     CALL DEPBOK(LEASOR_WRITE_OFF_PERIOD, &
                                 NF_BOOK_DEPRECIATION, &
                                 NF_BOOK_CAPITIALIZED, &
                                 1.,SERVICEMO_loc,FIRSTYR, &
                                 0., &
                                 FINANCIAL_SIMULATION_YEARS, &
                                 HALF_YEAR_BOOK_DEP)
                     CALL DEPBOK(LEASOR_WRITE_OFF_PERIOD, &
                                 NF_SL_TAX_DEPRECIATION, &
                                 NF_SL_TAX_VALUE, &
                                 1.,SERVICEMO_loc,FIRSTYR, &
                                 0., &
                                 FINANCIAL_SIMULATION_YEARS, &
                                 HALF_YEAR_BOOK_DEP)
                     DO I = 2, FINANCIAL_SIMULATION_YEARS
                        NF_DEFERRED_TAX_BASIS(I) = NF_TAX_DEP(I) - &
                                               NF_SL_TAX_DEPRECIATION(I)
                     ENDDO
                  ELSE
                  ENDIF
               ELSE
                  NF_DEFERRED_TAX_BASIS = 0.
                  NF_BOOK_DEPRECIATION = 0.
                  NF_SL_TAX_DEPRECIATION = 0.
                  IF(LEASOR_WRITE_OFF_PERIOD /= 0.) THEN
                     CALL DEPBOK(LEASOR_WRITE_OFF_PERIOD, &
                                 NF_SL_TAX_DEPRECIATION, &
                                 NF_SL_TAX_VALUE, &
                                 1.,SERVICEMO_loc,FIRSTYR, &
                                 0., &
                                 FINANCIAL_SIMULATION_YEARS, &
                                 HALF_YEAR_BOOK_DEP)
                  ELSE
                     DO I = 2, FINANCIAL_SIMULATION_YEARS
                        NF_SL_TAX_DEPRECIATION(I) = NF_TAX_DEP(I)
                     ENDDO
                  ENDIF
               ENDIF
            ELSE
               NF_DEFERRED_TAX_BASIS = 0.
               NF_BOOK_DEPRECIATION = 0.
               NF_SL_TAX_DEPRECIATION = 0.
            ENDIF
!
!      CALCULATE THE CHANGES FROM AN ABANDONMENT
!
            IF(ABMETH > 0) THEN
               CALL NFABND(ABYEAR,ABMETH,ABTAX,TAXWO,BOKBL,BOKDAL, &
                           BOKWO,AFCBL,AFCDAL,AFCWO,NRTXWO)
               CALL WRITE_OFF(ABYEAR,ABMETH,WOYRS,DDB,RBDDB,AFCEXP, &
                              AJAFDC,EXEXP,DBAMRT,BOKBL,BOKDAL,BOKWO, &
                              AFCBL,AFCDAL,AFCWO,ABACCT,WODFTX,NRTXWO, &
                              FINANCIAL_SIMULATION_YEARS)
            ENDIF
!
!  Calculate NF monthly fuel prices in $/MWh 7/18/96
!
            IF(CALUCLATE_NUC_FUEL_PRICES .AND. GENERATOR_ID > 0) THEN
               CALL NUCLEAR_FUEL_MONTLY_PRICES(GENERATOR_ID)
            ENDIF
!
!      LIST INDIVIDUAL ACCOUNT INFORMATION TO DISK FILE
!
            CALL NFTOTL(ABMETH,ABYEAR,TAXWO,PCLEASED, &
                        REGULATORY_ALLOCATOR,ABTAX, &
                        ASSET_CLASS,ASSET_CLASS_VECTOR, &
                        OWNER_ALLOCATION)
         ENDDO ! START_YEARS
      ENDDO ! GOTO 10
      CALL CLOSE_NUCLEAR_FUEL_FILE
      IF(LF95 .AND. TACCTS > 0) THEN
         WRITE(SCREEN_MESSAGES,"(I3,A)") TACCTS,'-Nuclear Fuel Accounts'
         CALL MG_LOCATE_WRITE(19,71,trim(SCREEN_MESSAGES), &
                                                   ALL_VERSIONS,1)
      ENDIF
      DEALLOCATE(ESCALATION_RATES)
      DEALLOCATE(CURRENT_INTEREST_CAP,AFDC2B, &
                 FIRST_YEARS_VEC, &
                 VECTOR_DATA_CASH, &
                 VECTOR_DATA_PLNT)
      DEALLOCATE(CWIP,NFIS,NNFV,TAXEXP,NFCE,NFES,AFDC1, &
                 AFDC1C,AFDC2,RBCWIP,DDB,RBDDB,AFCEXP, &
                 AJAFDC,EXEXP,DBAMRT,NFRB,AFDC2C,WODFTX, &
                 AFDC1B,AFDCDF,CAPINRST,PCAPINRST, &
                 NF_TAX_DEP,NF_DEFERRED_TAX_BASIS,NF_BOOK_DEPRECIATION, &
                 NF_TAX_VALUE, &
                 NF_SL_TAX_VALUE, &
                 NF_BOOK_CAPITIALIZED, &
                 NF_SL_TAX_DEPRECIATION)
      DEALLOCATE(ASSET_CLASS_LIST,ASSET_ALLOCATION_LIST)
!
!      WRITE THE CUMULATED RESULTS TO A FILE
!
      DO CLASS = 1, NUM_OF_ASSET_CLASSES
         DO I = 1,FINANCIAL_SIMULATION_YEARS
            TNFCE(I,0) = TNFCE(I,0) + TNFCE(I,CLASS)
            TNFIP(I,0) = TNFIP(I,0) + TNFIP(I,CLASS)
            TRBFIP(I,0) = TRBFIP(I,0) + TRBFIP(I,CLASS)
            TNFES(I,0) = TNFES(I,0) + TNFES(I,CLASS)
            TNFIS(I,0) = TNFIS(I,0) + TNFIS(I,CLASS)
            TNNFV(I,0) = TNNFV(I,0) + TNNFV(I,CLASS)
            TNFRB(I,0) = TNFRB(I,0) + TNFRB(I,CLASS)
            TAFDC1(I,0) = TAFDC1(I,0) + TAFDC1(I,CLASS)
            TAFC1C(I,0) = TAFC1C(I,0) + TAFC1C(I,CLASS)
            TAFDC2(I,0) = TAFDC2(I,0) + TAFDC2(I,CLASS)
            TAFC2C(I,0) = TAFC2C(I,0) + TAFC2C(I,CLASS)
            TTXEXP(I,0) = TTXEXP(I,0) + TTXEXP(I,CLASS)
            TDDB(I,0) = TDDB(I,0) + TDDB(I,CLASS)
            TRBDDB(I,0) = TRBDDB(I,0) + TRBDDB(I,CLASS)
            TDBAMR(I,0) = TDBAMR(I,0) + TDBAMR(I,CLASS)
            TEXEXP(I,0) = TEXEXP(I,0) + TEXEXP(I,CLASS)
            TAFEXP(I,0) = TAFEXP(I,0) + TAFEXP(I,CLASS)
            TAFCAJ(I,0) = TAFCAJ(I,0) + TAFCAJ(I,CLASS)
            TWODFT(I,0) = TWODFT(I,0) + TWODFT(I,CLASS)
            TAFDCB(I,0) = TAFDCB(I,0) + TAFDCB(I,CLASS)
            TAFDCD(I,0) = TAFDCD(I,0) + TAFDCD(I,CLASS)
            TNFCE_LEASED(I,0) = TNFCE_LEASED(I,0)+TNFCE_LEASED(I,CLASS)
            TPCAPINRST(I,0) = TPCAPINRST(I,0) + TPCAPINRST(I,CLASS)
            OWNED_NF_TAX_DEP(I,0) = OWNED_NF_TAX_DEP(I,0) + &
                                               OWNED_NF_TAX_DEP(I,CLASS)
            LEASOR_NF_TAX_DEP(I,0) = LEASOR_NF_TAX_DEP(I,0) + &
                                              LEASOR_NF_TAX_DEP(I,CLASS)
            LEASOR_NF_DEFERRED_TAX_BASIS(I,0) = &
                                   LEASOR_NF_DEFERRED_TAX_BASIS(I,0) + &
                                   LEASOR_NF_DEFERRED_TAX_BASIS(I,CLASS)
            LEASOR_NF_BOOK_DEPRECIATION(I,0) = &
                                    LEASOR_NF_BOOK_DEPRECIATION(I,0) + &
                                    LEASOR_NF_BOOK_DEPRECIATION(I,CLASS)
            LEASOR_NF_SL_TAX_DEPRECIATION(I,0) = &
                                  LEASOR_NF_SL_TAX_DEPRECIATION(I,0) + &
                                  LEASOR_NF_SL_TAX_DEPRECIATION(I,CLASS)
         ENDDO
      ENDDO
      IF(SAVE_BASE_CASE) THEN
         CALL OPEN_NF_BASE_CASE_FILE(INT(10,2))
         WRITE(10) TNFCE,TNFIP,TRBFIP,TNFES,TNFIS,TNNFV,TNFRB, &
                   TAFDC1,TAFC1C,TAFDC2,TAFC2C,TTXEXP,TDDB, &
                   TRBDDB,TDBAMR,TEXEXP,TAFEXP,TAFCAJ,TWODFT, &
                   TAFDCB,TAFDCD,TNFCE_LEASED,TPCAPINRST, &
                   JPCAPINRST,JTXEXP,JDBAMR,RBCWIP_AFDC_METH2, &
                   RB_AFDC1, &
                   OWNED_NF_TAX_DEP, &
                   OWNED_NF_SL_TAX_DEPRECIATION, &
                   LEASOR_NF_DEFERRED_TAX_BASIS, &
                   LEASOR_NF_BOOK_DEPRECIATION, &
                   LEASOR_NF_SL_TAX_DEPRECIATION, &
                   LEASOR_NF_TAX_DEP
         CLOSE(10)
      ENDIF
      RETURN

      ENTRY SETUP_GENERATING_INFORMATION(R_NUM_OF_NUC_UNITS)

!
         NUM_OF_NUC_UNITS =   MAX(INT(200,2),R_NUM_OF_NUC_UNITS)
         NUC_FUEL_BURN_YEARS = MIN(AVAIL_DATA_YEARS, &
                                        RUN_YEARS() + EXTENSION_YEARS())
         IF(ALLOCATED(ID_POINTER)) DEALLOCATE(ID_POINTER, &
                                           GENERATION_BY_INSERVICE_YEAR, &
                                           ENRG_BY_FISCAL_YEAR, &
                                           MAINTENANCE_RATE, &
                                           MONTHLY_UNIT_CAPACITY, &
                                           FUEL_PRICE_FROM_NF_FILE)
         ALLOCATE(ID_POINTER(0:10000), &
                  GENERATION_BY_INSERVICE_YEAR(0:NUC_FUEL_BURN_YEARS, &
                                                      NUM_OF_NUC_UNITS), &
                  ENRG_BY_FISCAL_YEAR(NUC_FUEL_BURN_YEARS,2, &
                                                      NUM_OF_NUC_UNITS), &
                  MAINTENANCE_RATE(12,NUC_FUEL_BURN_YEARS+2, &
                                                      NUM_OF_NUC_UNITS), &
                  MONTHLY_UNIT_CAPACITY(12,NUC_FUEL_BURN_YEARS+2), &
                  FUEL_PRICE_FROM_NF_FILE(NUM_OF_NUC_UNITS))
         ID_POINTER = -1
         GENERATION_BY_INSERVICE_YEAR = 0.
         ENRG_BY_FISCAL_YEAR = 0.
         MAINTENANCE_RATE = 0.
         MONTHLY_UNIT_CAPACITY = 0.
         OFFSET_MAINTENANCE_ACTIVE = OFFSET_MAINTENANCE_VECTORS()
!
!  TRANSACT
!
      ENTRY RESET_NUCLEAR_UNIT_COUNTER()
         NUC_UNITS_FOUND = 0
      RETURN

      ENTRY CAL_GENERATING_INFORMATION(R_UNIT_ID, &
                                       R_FRAC_OWNED, &
                                       R_UNIT_CAPACITY, &
                                       R_MONTHLY_CAPACITY_POINTR, &
                                       R_EFOR, &
                                       R_MAINTENANCE_RATE, &
                                       R_ON_LINE, &
                                       R_OFF_LINE, &
                                       R_UNIT_NAME, &
                                       R_FUEL_PRICE_SOURCE)

!
         IF(R_UNIT_ID > 10000) RETURN
         NUC_UNITS_FOUND = NUC_UNITS_FOUND + 1
         FUEL_PRICE_FROM_NF_FILE(NUC_UNITS_FOUND) = &
                                    INDEX(R_FUEL_PRICE_SOURCE,'NF') /= 0
         IF(R_UNIT_ID >= 0) THEN
            ID_POINTER(R_UNIT_ID) = NUC_UNITS_FOUND
         ENDIF
         DO YR = 1, NUC_FUEL_BURN_YEARS+2
            DATE = (YR+get_base_year()-1900)*100
            IF(R_OFF_LINE < DATE) EXIT
            IF(R_UNIT_CAPACITY < 0.) THEN
               GET_YR = MIN(YR,AVAIL_DATA_YEARS)
               ANNUAL_CAPACITY = GET_VAR(R_UNIT_CAPACITY, &
                                                     GET_YR,R_UNIT_NAME)
               IF(ANNUAL_CAPACITY < 0.) THEN
                  DO MO = 1, 12
                     MONTHLY_UNIT_CAPACITY(MO,YR) = &
                                 GET_VAR(ANNUAL_CAPACITY,MO,R_UNIT_NAME)
                  ENDDO
               ELSE
                  MONTHLY_UNIT_CAPACITY(:,YR) = ANNUAL_CAPACITY
               ENDIF
            ELSE
               MONTHLY_UNIT_CAPACITY(:,YR) = R_UNIT_CAPACITY
            ENDIF
            DO MO = 1, 12
               IF(DATE+MO >= R_ON_LINE .AND. DATE+MO <= R_OFF_LINE) THEN
                  IF(R_MAINTENANCE_RATE(MO) < 0.) THEN
                     IF(OFFSET_MAINTENANCE_ACTIVE .AND. &
                                    R_ON_LINE/100+1900 > get_base_year()) THEN
                        GET_YR = YR-(R_ON_LINE/100+1900 - get_base_year())+1
                     ELSE
                        GET_YR = YR
                     ENDIF
                     GET_YR = MIN(GET_YR,AVAIL_DATA_YEARS)
                     MAINTENANCE_RATE(MO,YR,NUC_UNITS_FOUND) = &
                                         GET_VAR(R_MAINTENANCE_RATE(MO), &
                                                     GET_YR,R_UNIT_NAME)
!
                  ELSE
                     MAINTENANCE_RATE(MO,YR,NUC_UNITS_FOUND) = &
                                                  R_MAINTENANCE_RATE(MO)
                  ENDIF
                  IF(R_MONTHLY_CAPACITY_POINTR /= 0) THEN
                     MW_CAPACITY = &
                               GET_VAR(FLOAT(R_MONTHLY_CAPACITY_POINTR), &
                                                         MO,R_UNIT_NAME)
                     IF(MW_CAPACITY <= 2.) THEN
                        MONTHLY_UNIT_CAPACITY(MO,YR) = MW_CAPACITY * &
                                            MONTHLY_UNIT_CAPACITY(MO,YR)
                     ELSE
                        MONTHLY_UNIT_CAPACITY(MO,YR) = MW_CAPACITY
                     ENDIF
                  ENDIF
               ELSE
                  MAINTENANCE_RATE(MO,YR,NUC_UNITS_FOUND) = 0.
                  MONTHLY_UNIT_CAPACITY(MO,YR) = 0.
               ENDIF
            ENDDO
         ENDDO
         HOURS_GENERATION = 0.
         REMAINING_GENERATION_FOUND = .FALSE.
         RELOAD_YEAR = 1

!  NOTE: 7/11/96 NEED A LAST LOAD VALUE THAT'S BETTER THAN
!                THE MIN() BELOW.

         FRAC_OWNED = R_FRAC_OWNED/100.
         DO YR = 1, MIN(AVAIL_DATA_YEARS,NUC_FUEL_BURN_YEARS+INT(2,2))
            IF(RELOAD_YEAR > NUC_FUEL_BURN_YEARS) then
            	EXIT
            end if
            DATE = (YR+get_base_year()-1900)*100
            IF(R_ON_LINE > DATE+12) CYCLE
            IF(R_OFF_LINE < DATE) EXIT
            IF(R_EFOR < 0.) THEN
               ANNUAL_EFOR =  GET_VAR(R_EFOR,YR,R_UNIT_NAME)
               IF(ANNUAL_EFOR < 0.) THEN
                  DO MO = 1, 12
                     MONTHLY_UNIT_EFOR(MO) = &
                             1.-GET_VAR(ANNUAL_EFOR,MO,R_UNIT_NAME)/100.
                  ENDDO
               ELSE
                  MONTHLY_UNIT_EFOR(:) = 1.-ANNUAL_EFOR/100.
               ENDIF
            ELSE
               MONTHLY_UNIT_EFOR(:) = 1.-R_EFOR/100.
            ENDIF
            DO MO = 1, 12
               IF(DATE+MO < R_ON_LINE) CYCLE
               ANNUAL_CAPACITY = FRAC_OWNED*MONTHLY_UNIT_CAPACITY(MO,YR)
               IF(DATE+MO > R_OFF_LINE) THEN
                  GENERATION_BY_INSERVICE_YEAR(RELOAD_YEAR, &
                                                      NUC_UNITS_FOUND) = &
                             GENERATION_BY_INSERVICE_YEAR(RELOAD_YEAR, &
                                                      NUC_UNITS_FOUND) + &
                                        (24.* HOURS_GENERATION)/1000000.
                  EXIT
               ENDIF
               IF(MAINTENANCE_RATE(MO,YR,NUC_UNITS_FOUND) > 0.) THEN
                  HOURS_GENERATION = HOURS_GENERATION + &
                      (1.-MAINTENANCE_RATE(MO,YR,NUC_UNITS_FOUND)/100.)* &
                                     MONTHLY_UNIT_EFOR(MO) * &
                                     FLOAT(DAYS_IN_EACH_MONTH(MO)) * &
                                     ANNUAL_CAPACITY
                  IF(.NOT. REMAINING_GENERATION_FOUND) THEN
                     REMAINING_GENERATION_FOUND = .TRUE.
                     GENERATION_BY_INSERVICE_YEAR(0,NUC_UNITS_FOUND) = &
                                        (24.* HOURS_GENERATION)/1000000.
                  ELSE
                     GENERATION_BY_INSERVICE_YEAR(RELOAD_YEAR, &
                                                      NUC_UNITS_FOUND) = &
                             GENERATION_BY_INSERVICE_YEAR(RELOAD_YEAR, &
                                                      NUC_UNITS_FOUND) + &
                                        (24.* HOURS_GENERATION)/1000000.
                     IF(YR > NUC_FUEL_BURN_YEARS) EXIT
                  ENDIF
                  RELOAD_YEAR = YR
                  HOURS_GENERATION = 0.
               ELSE
                  HOURS_GENERATION = HOURS_GENERATION + &
                                        MONTHLY_UNIT_EFOR(MO) * &
                                        FLOAT(DAYS_IN_EACH_MONTH(MO)) * &
                                        ANNUAL_CAPACITY
               ENDIF
            ENDDO
            IF(YR <= NUC_FUEL_BURN_YEARS) THEN
               ENRG_GENERATION = 0.
               POS = 1
               DO MO = 1, 12
                  IF(DATE+MO < R_ON_LINE) CYCLE
                  ANNUAL_CAPACITY = FRAC_OWNED * &
                                            MONTHLY_UNIT_CAPACITY(MO,YR)
                  IF(DATE+MO > R_OFF_LINE) THEN
                     ENRG_BY_FISCAL_YEAR(YR,POS,NUC_UNITS_FOUND) = &
                           ENRG_BY_FISCAL_YEAR(YR,POS,NUC_UNITS_FOUND) + &
                                         (24.* ENRG_GENERATION)/1000000.
                     EXIT
                  ENDIF
                  IF(MAINTENANCE_RATE(MO,YR,NUC_UNITS_FOUND) > 0.) THEN
                     ENRG_GENERATION = ENRG_GENERATION + &
                      (1.-MAINTENANCE_RATE(MO,YR,NUC_UNITS_FOUND)/100.)* &
                                     MONTHLY_UNIT_EFOR(MO) * &
                                     FLOAT(DAYS_IN_EACH_MONTH(MO)) * &
                                     ANNUAL_CAPACITY
                     ENRG_BY_FISCAL_YEAR(YR,POS,NUC_UNITS_FOUND) = &
                           ENRG_BY_FISCAL_YEAR(YR,POS,NUC_UNITS_FOUND) + &
                                         (24.* ENRG_GENERATION)/1000000.
                     ENRG_GENERATION = 0.
                     POS = 2
                  ELSE
                     ENRG_GENERATION = ENRG_GENERATION + &
                                         MONTHLY_UNIT_EFOR(MO) * &
                                         FLOAT(DAYS_IN_EACH_MONTH(MO)) * &
                                         ANNUAL_CAPACITY
                  ENDIF
               ENDDO
               ENRG_BY_FISCAL_YEAR(YR,POS,NUC_UNITS_FOUND) = &
                           ENRG_BY_FISCAL_YEAR(YR,POS,NUC_UNITS_FOUND) + &
                                         (24.* ENRG_GENERATION)/1000000.
            ENDIF
         ENDDO
      RETURN

      ENTRY NUCLEAR_FUEL_MONTLY_PRICES(R_UNIT_ID)

!

         GEN_UNIT = ID_POINTER(R_UNIT_ID)
         IF(GEN_UNIT < 1) RETURN
         IF(.NOT. FUEL_PRICE_FROM_NF_FILE(GEN_UNIT)) RETURN
         CURRENT_RATE = REGION_AMORT_RATE(1) + &
                        REGION_AMORT_RATE(2) + &
                        REGION_AMORT_RATE(3) + &
                        REGION_AMORT_RATE(4)
         REGION_CAP_ALLOCATION(1) = REGION_CAP_ALLOCATION(1)/100.
         REGION_CAP_ALLOCATION(2) = REGION_CAP_ALLOCATION(2)/ &
                                     (REGION_CAP_ALLOCATION(2) + &
                                       REGION_CAP_ALLOCATION(3) + &
                                        REGION_CAP_ALLOCATION(4))
         IF(REGION_CAP_ALLOCATION(3) == 0.) THEN
            REGION_CAP_ALLOCATION(3) = 1.
         ELSE
            REGION_CAP_ALLOCATION(3) = REGION_CAP_ALLOCATION(3)/ &
                                         (REGION_CAP_ALLOCATION(3) + &
                                           REGION_CAP_ALLOCATION(4))
         ENDIF
         REGION_CAP_ALLOCATION(4) = 1.
         IF(LEASOR_MARK_UP < 0.) THEN
            IVEC = ABS(LEASOR_MARK_UP)
            CALL GET_ASSET_VAR(IVEC,DUMMY_TYPE,VECTOR_DATA)
         ENDIF
         WRITE(YEAR_STR,'(I4)') BASEYEAR
         LOAD_NAME(1) = 'L'//YEAR_STR//'-1'
         LOAD_NAME(2) = 'L'//YEAR_STR//'-2'
         LOAD_NAME(3) = 'L'//YEAR_STR//'-3'
         LOAD_NAME(4) = 'L'//YEAR_STR//'-4'
         DO YR = 1, NUC_FUEL_BURN_YEARS
            IF(LEASOR_MARK_UP < 0.) THEN
               ANNUAL_LEASOR_MARK_UP(YR,GEN_UNIT) = VECTOR_DATA(YR)
            ELSE
               ANNUAL_LEASOR_MARK_UP(YR,GEN_UNIT) = LEASOR_MARK_UP
            ENDIF
            CAPITIALIZED_FUEL = NFES(YR+1) + AFDC2(YR+1)
            IF(CAPITIALIZED_FUEL == 0.) THEN
               DO MO = 1, 12
                  NUC_FUEL_COST(MO,YR,GEN_UNIT) = CURRENT_RATE
               ENDDO
               DO I = 4,1,-1
                  PRE_LOAD_BURN(I) = MIN(OPENING_REGION_BALANCE(I), &
                                       REGION_AMORT_RATE(I) * &
                                     ENRG_BY_FISCAL_YEAR(YR,1,GEN_UNIT))
                  IF(NUC_FUEL_REPORTING .AND. &
                                   OPENING_REGION_BALANCE(I) /= 0.) THEN
                     WRITE(NUC_REPORTING_UNIT,REC=NUC_REPORTING_REC) &
                                  PRT_ENDPOINT(), &
                                  FLOAT(BASEYEAR+YR), &
                                  DESC,'    ', &
                                  LOAD_NAME(I), &
                                  OPENING_REGION_BALANCE(I), &
                                  1000.* &
                                     ENRG_BY_FISCAL_YEAR(YR,1,GEN_UNIT), &
                                  REGION_AMORT_RATE(I), &
                                  PRE_LOAD_BURN(I), &
                                  0.,0.,0., &
                                  0.,  & !  REGION_AMORT_RATE(I),
                                  0., &
                                  PRE_LOAD_BURN(I), &
                                  OPENING_REGION_BALANCE(I) - &
                                                       PRE_LOAD_BURN(I), &
                                  1000.* &
                                     ENRG_BY_FISCAL_YEAR(YR,1,GEN_UNIT), &
                                  REGION_AMORT_RATE(I)
                     NUC_REPORTING_REC = NUC_REPORTING_REC + 1
                  ENDIF
                  OPENING_REGION_BALANCE(I) = OPENING_REGION_BALANCE(I)- &
                                                        PRE_LOAD_BURN(I)
               ENDDO
               CYCLE
            ENDIF
            DO MO = 1, 12
               NUC_FUEL_COST(MO,YR,GEN_UNIT) = CURRENT_RATE
               IF(MAINTENANCE_RATE(MO,YR,GEN_UNIT) > 0.) THEN
                  DO I = 1, 4
                     PRE_LOAD_BURN(I) = MIN(OPENING_REGION_BALANCE(I), &
                                     REGION_AMORT_RATE(I) * &
                                     ENRG_BY_FISCAL_YEAR(YR,1,GEN_UNIT))
                     PRE_LOAD_CLOSE(I) = OPENING_REGION_BALANCE(I) - &
                                                        PRE_LOAD_BURN(I)
                  ENDDO
                  FUELING_START_MONTH = MO
                  EXIT
               ENDIF
            ENDDO
!
            POST_REGION_AMORT_RATE(1) = CAPITIALIZED_FUEL * &
                                   REGION_CAP_ALLOCATION(1)/ &
                               GENERATION_BY_INSERVICE_YEAR(YR,GEN_UNIT)
            DO I = 2, 4
               POST_REGION_AMORT_RATE(I) = PRE_LOAD_CLOSE(I-1) * &
                                   REGION_CAP_ALLOCATION(I)/ &
                               GENERATION_BY_INSERVICE_YEAR(YR,GEN_UNIT)
            ENDDO
            CURRENT_RATE = POST_REGION_AMORT_RATE(1) + &
                           POST_REGION_AMORT_RATE(2) + &
                           POST_REGION_AMORT_RATE(3) + &
                           POST_REGION_AMORT_RATE(4)
            MO_START = MO + 1
            DO MO = MO_START, 12
               NUC_FUEL_COST(MO,YR,GEN_UNIT) = CURRENT_RATE
            ENDDO
            POST_REGION_BURN(1) = MIN(CAPITIALIZED_FUEL, &
                                       POST_REGION_AMORT_RATE(1) * &
                                     ENRG_BY_FISCAL_YEAR(YR,2,GEN_UNIT))
            POST_REGION_BALANCE(1) = CAPITIALIZED_FUEL - &
                                                     POST_REGION_BURN(1)
            DO I = 2,4
               POST_REGION_BURN(I) = MIN(PRE_LOAD_CLOSE(I-1), &
                                       POST_REGION_AMORT_RATE(I) * &
                                     ENRG_BY_FISCAL_YEAR(YR,2,GEN_UNIT))
               POST_REGION_BALANCE(I) = PRE_LOAD_CLOSE(I-1) - &
                                                     POST_REGION_BURN(I)
            ENDDO
            IF(NUC_FUEL_REPORTING) THEN
               IF(OPENING_REGION_BALANCE(4) > 0.) THEN
                  WRITE(NUC_REPORTING_UNIT,REC=NUC_REPORTING_REC) &
                                     PRT_ENDPOINT(), &
                                     FLOAT(BASEYEAR+YR), &
                                     DESC,'    ', &
                                     LOAD_NAME(4), &
                                     OPENING_REGION_BALANCE(4), &
                                  1000.* &
                                     ENRG_BY_FISCAL_YEAR(YR,1,GEN_UNIT), &
                                     REGION_AMORT_RATE(4), &
                                     PRE_LOAD_BURN(4), &
                                     0.,0.,0., &
                                     0., & !  REGION_AMORT_RATE(I),
                                     0., &
                                     PRE_LOAD_BURN(4), &
                                     OPENING_REGION_BALANCE(4) - &
                                                       PRE_LOAD_BURN(4), &
                                  1000.* &
                                     ENRG_BY_FISCAL_YEAR(YR,1,GEN_UNIT), &
                                     REGION_AMORT_RATE(4)
                  NUC_REPORTING_REC = NUC_REPORTING_REC + 1
               ENDIF
               DO I = 3,1,-1
                  IF(OPENING_REGION_BALANCE(I) > 0.) THEN
                     TOTAL_ENRG = ENRG_BY_FISCAL_YEAR(YR,1,GEN_UNIT)+ &
                                      ENRG_BY_FISCAL_YEAR(YR,2,GEN_UNIT)
                     IF(TOTAL_ENRG /= 0.) THEN
                        AVERAGE_BURN_RATE = (PRE_LOAD_BURN(I) + &
                                       POST_REGION_BURN(I+1))/TOTAL_ENRG
                     ELSE
                        AVERAGE_BURN_RATE = 0.
                     ENDIF
                     WRITE(NUC_REPORTING_UNIT,REC=NUC_REPORTING_REC) &
                                     PRT_ENDPOINT(), &
                                     FLOAT(BASEYEAR+YR), &
                                     DESC,'    ', &
                                     LOAD_NAME(I), &
                                     OPENING_REGION_BALANCE(I), &
                                  1000.* &
                                     ENRG_BY_FISCAL_YEAR(YR,1,GEN_UNIT), &
                                     REGION_AMORT_RATE(I), &
                                     PRE_LOAD_BURN(I), &
                                     FUELING_START_MONTH, &
                                     PRE_LOAD_CLOSE(I), &
                                  1000.* &
                                     ENRG_BY_FISCAL_YEAR(YR,2,GEN_UNIT), &
                                     POST_REGION_AMORT_RATE(I+1), &
                                     POST_REGION_BURN(I+1), &
                                     PRE_LOAD_BURN(I) + &
                                                  POST_REGION_BURN(I+1), &
                                     POST_REGION_BALANCE(I+1), &
                                     1000.*TOTAL_ENRG, &
                                     AVERAGE_BURN_RATE
                     NUC_REPORTING_REC = NUC_REPORTING_REC + 1
                  ENDIF
               ENDDO
            ENDIF
            IF(REGION_CAP_ALLOCATION(3) == 1.) THEN
               OPENING_REGION_BALANCE(4) = 0.
            ELSE
               OPENING_REGION_BALANCE(4) = POST_REGION_BALANCE(4)
            ENDIF
            OPENING_REGION_BALANCE(3) = POST_REGION_BALANCE(3)
            OPENING_REGION_BALANCE(2) = POST_REGION_BALANCE(2)
            OPENING_REGION_BALANCE(1) = POST_REGION_BALANCE(1)

            LOAD_NAME(4) = LOAD_NAME(3)
            LOAD_NAME(3) = LOAD_NAME(2)
            LOAD_NAME(2) = LOAD_NAME(1)
            WRITE(YEAR_STR,'(I4)') BASEYEAR+YR
            LOAD_NAME(1) = 'L'//YEAR_STR

            IF(NUC_FUEL_REPORTING) THEN
               WRITE(NUC_REPORTING_UNIT,REC=NUC_REPORTING_REC) &
                                     PRT_ENDPOINT(), &
                                     FLOAT(BASEYEAR+YR), &
                                     DESC,'    ', &
                                     LOAD_NAME(1), &
                                     0., & !  OPENING_REGION_BALANCE(I),
                                     0., & !  ENRG_BY_FISCAL_YEAR(YR,1,GEN_UNIT),
                                     0., & !  REGION_AMORT_RATE(I),
                                     0., & !  PRE_LOAD_BURN(I),
                                     FUELING_START_MONTH, &
                                     CAPITIALIZED_FUEL, &
                                  1000.* &
                                     ENRG_BY_FISCAL_YEAR(YR,2,GEN_UNIT), &
                                     POST_REGION_AMORT_RATE(1), &
                                     POST_REGION_BURN(1), &
                                     POST_REGION_BURN(1), &
                                     POST_REGION_BALANCE(1), &
                                  1000.* &
                                     ENRG_BY_FISCAL_YEAR(YR,2,GEN_UNIT), &
                                     POST_REGION_AMORT_RATE(1)
               NUC_REPORTING_REC = NUC_REPORTING_REC + 1
            ENDIF

               REGION_AMORT_RATE(:) = POST_REGION_AMORT_RATE(:)

         ENDDO
      RETURN

      ENTRY RETURN_NUC_FUEL_PRICES(R_UNIT_POS,R_MO,R_YR,R_MARK_UP, &
                                   R_NUC_GEN_UNIT_FOUND)

!
         R_NUC_GEN_UNIT_FOUND = .FALSE.
         R_MARK_UP = 0.
         GEN_UNIT = CL_RESOURCE_ID(R_UNIT_POS)
         GEN_UNIT = ID_POINTER(GEN_UNIT)
         IF(GEN_UNIT > 0 .AND. CALUCLATE_NUC_FUEL_PRICES) THEN
            R_MARK_UP = ANNUAL_LEASOR_MARK_UP(R_YR,GEN_UNIT) * &
                                  NUC_FUEL_COST(R_MO,R_YR,GEN_UNIT)/100.
            MMBTU_PRICE = 1000. * NUC_FUEL_COST(R_MO,R_YR,GEN_UNIT)/ &
                                          COEFF(1,R_UNIT_POS)
            PBTUCT(R_UNIT_POS) = MMBTU_PRICE
            R_NUC_GEN_UNIT_FOUND = .TRUE.
         ENDIF
      RETURN

      ENTRY RETURN_TOTAL_NUCLEAR_FUEL(RUN_YEAR,R_NFCE,R_NFIP, &
                                      R_RBNFIP,R_NFES,R_NFIS,R_NNFV, &
                                      R_RBNF,R_NFAFC1,R_NAFC1C,R_NFAFC2, &
                                      R_NAFC2C,R_NTXEXP,R_NFDDB, &
                                      R_NFDDRB,R_NAMTEX,R_NEXEXP, &
                                      R_NAFEXP,R_ADJAFC,R_NWODFT, &
                                      R_NAFDCB,R_NAFDCD,R_LEASED_NFCE, &
                                      R_NFCAPINRST, &
! NUCLEAR FUEL RATEBASE ADDER & !  NUCLEAR FUEL RATEBASE ADDERS
                                      RB_NFCAPINRST,RB_NFTXEXP, &
                                      RB_NFDBAMR,NF_RBCWIP_AFDCMETH2, &
                                      RB_NFAFC1, &
                                      R_OWNED_NF_TAX_DEP, &
                                      R_LEASOR_NF_DEFERRED_TAX_BASIS, &
                                      R_LEASOR_FUEL_DEPRECIATION, &
                                      R_LEASOR_NF_SL_TAX_DEPRECIATION, &
                                      R_LEASOR_NF_TAX_DEP)

          R_NFCE = TNFCE(RUN_YEAR,0)
          R_NFIP = TNFIP(RUN_YEAR,0)
          R_RBNFIP = TRBFIP(RUN_YEAR,0)
          R_NFES = TNFES(RUN_YEAR,0)
          R_NFIS = TNFIS(RUN_YEAR,0)
          R_NNFV = TNNFV(RUN_YEAR,0)
          R_RBNF = TNFRB(RUN_YEAR,0)
          R_NFAFC1 = TAFDC1(RUN_YEAR,0)
          R_NAFC1C = TAFC1C(RUN_YEAR,0)
          R_NFAFC2 = TAFDC2(RUN_YEAR,0)
          R_NAFC2C = TAFC2C(RUN_YEAR,0)
          R_NTXEXP = TTXEXP(RUN_YEAR,0)
          R_NFDDB = TDDB(RUN_YEAR,0)
          R_NFDDRB = TRBDDB(RUN_YEAR,0)
          R_NAMTEX = TDBAMR(RUN_YEAR,0)
          R_NEXEXP = TEXEXP(RUN_YEAR,0)
          R_NAFEXP = TAFEXP(RUN_YEAR,0)
          R_ADJAFC = TAFCAJ(RUN_YEAR,0)
          R_NWODFT = TWODFT(RUN_YEAR,0)
          R_NAFDCB = TAFDCB(RUN_YEAR,0)
          R_NAFDCD = TAFDCD(RUN_YEAR,0)
          R_LEASED_NFCE = TNFCE_LEASED(RUN_YEAR,0)
          R_NFCAPINRST = TPCAPINRST(RUN_YEAR,0)
          R_OWNED_NF_TAX_DEP = OWNED_NF_TAX_DEP(RUN_YEAR,0)
          R_LEASOR_NF_TAX_DEP = LEASOR_NF_TAX_DEP(RUN_YEAR,0)
          R_LEASOR_NF_DEFERRED_TAX_BASIS = &
                                LEASOR_NF_DEFERRED_TAX_BASIS(RUN_YEAR,0)
          R_LEASOR_FUEL_DEPRECIATION = &
                                 LEASOR_NF_BOOK_DEPRECIATION(RUN_YEAR,0)
          R_LEASOR_NF_SL_TAX_DEPRECIATION = &
                               LEASOR_NF_SL_TAX_DEPRECIATION(RUN_YEAR,0)
!
          RB_NFCAPINRST = JPCAPINRST(RUN_YEAR)
          RB_NFTXEXP = JTXEXP(RUN_YEAR)
          RB_NFDBAMR = JDBAMR(RUN_YEAR)
          NF_RBCWIP_AFDCMETH2 = RBCWIP_AFDC_METH2(RUN_YEAR)
          RB_NFAFC1 = RB_AFDC1(RUN_YEAR)
      RETURN

      ENTRY NUCLEAR_FUEL_BY_INFO(R_CLASS,R_NNFV,R_RBNF,R_NFIS)

         R_NNFV = 0.
         R_RBNF = 0.
         R_NFIS = 0.
         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
               R_NNFV = TNNFV(1,ASSET_CLASS)
               R_RBNF = TRBFIP(1,ASSET_CLASS) + TNFRB(1,ASSET_CLASS)
               R_NFIS = TNFIS(1,ASSET_CLASS)
            ENDIF
         ENDIF
      RETURN

      ENTRY NUCLEAR_FUEL_INFO(RUN_YEAR,R_CLASS,R_CLASS_EXISTS, &
                              R_NFCE,R_NFIP,R_NFES,R_NFIS, &
                              R_NNFV,R_RBNF,R_NFAFC1,R_NFAFC2, &
                              R_NTXEXP,R_NFDDB, &
                              R_NFDDRB,R_NAMTEX,R_NEXEXP, &
                              R_NAFEXP,R_ADJAFC,R_NWODFT, &
                              R_NAFDCB,R_NAFDCD,R_LEASED_NFCE, &
                              R_NFCAPINRST, &
                              R_OWNED_NF_TAX_DEP, &
                              R_OWNED_NF_SL_TAX_DEP, &
                              R_LEASOR_NF_DEFERRED_TAX_BASIS, &
                              R_LEASOR_FUEL_DEPRECIATION, &
                              R_LEASOR_NF_SL_TAX_DEPRECIATION, &
                              R_LEASOR_NF_TAX_DEP)

         R_CLASS_EXISTS = .FALSE.
         R_NFCE = 0.
         R_NFIP = 0.
         R_NFES = 0.
         R_NFIS = 0.
         R_NNFV = 0.
         R_RBNF = 0.
         R_NFAFC1 = 0.
         R_NFAFC2 = 0.
         R_NTXEXP = 0.
         R_NFDDB = 0.
         R_NEXEXP = 0.
         R_NAFEXP = 0.
         R_ADJAFC = 0.
         R_NWODFT = 0.
         R_NAFDCB = 0.
         R_NAFDCD = 0.
         R_LEASED_NFCE = 0.
         R_LEASOR_NF_TAX_DEP = 0.
         R_OWNED_NF_TAX_DEP = 0.
         R_LEASOR_NF_SL_TAX_DEPRECIATION = 0.
!
         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
               R_CLASS_EXISTS = .TRUE.
               R_NFCE = TNFCE(RUN_YEAR,ASSET_CLASS)
               R_NFIP = TNFIP(RUN_YEAR,ASSET_CLASS) + &
                        TAFC1C(RUN_YEAR,ASSET_CLASS) - &
                        TAFC2C(RUN_YEAR,ASSET_CLASS)
               R_NFES = TNFES(RUN_YEAR,ASSET_CLASS)
               R_NFIS = TNFIS(RUN_YEAR,ASSET_CLASS)
               R_NNFV = TNNFV(RUN_YEAR,ASSET_CLASS)
               R_RBNF = TNFRB(RUN_YEAR,ASSET_CLASS) + &
                                            TRBFIP(RUN_YEAR,ASSET_CLASS)
               R_NFAFC1 = TAFDC1(RUN_YEAR,ASSET_CLASS)
               R_NFAFC2 = TAFDC2(RUN_YEAR,ASSET_CLASS)
               R_NTXEXP = TTXEXP(RUN_YEAR,ASSET_CLASS)
               R_NFDDB = TDDB(RUN_YEAR,ASSET_CLASS)
               R_NFDDRB = R_NFDDRB + TRBDDB(RUN_YEAR,ASSET_CLASS)
               R_NAMTEX = R_NAMTEX + TDBAMR(RUN_YEAR,ASSET_CLASS)
               R_NEXEXP = TEXEXP(RUN_YEAR,ASSET_CLASS)
               R_NAFEXP = TAFEXP(RUN_YEAR,ASSET_CLASS)
               R_ADJAFC = TAFCAJ(RUN_YEAR,ASSET_CLASS)
               R_NWODFT = TWODFT(RUN_YEAR,ASSET_CLASS)
               R_NAFDCB = TAFDCB(RUN_YEAR,ASSET_CLASS)
               R_NAFDCD = TAFDCD(RUN_YEAR,ASSET_CLASS)
               R_LEASED_NFCE = TNFCE_LEASED(RUN_YEAR,ASSET_CLASS)
               R_NFCAPINRST = R_NFCAPINRST + &
                                        TPCAPINRST(RUN_YEAR,ASSET_CLASS)
               R_OWNED_NF_TAX_DEP = &
                                  OWNED_NF_TAX_DEP(RUN_YEAR,ASSET_CLASS)
               R_OWNED_NF_SL_TAX_DEP = &
                      OWNED_NF_SL_TAX_DEPRECIATION(RUN_YEAR,ASSET_CLASS)
               R_LEASOR_NF_TAX_DEP = &
                                 LEASOR_NF_TAX_DEP(RUN_YEAR,ASSET_CLASS)
               R_LEASOR_NF_DEFERRED_TAX_BASIS = &
                      R_LEASOR_NF_DEFERRED_TAX_BASIS + &
                      LEASOR_NF_DEFERRED_TAX_BASIS(RUN_YEAR,ASSET_CLASS)
               R_LEASOR_FUEL_DEPRECIATION = R_LEASOR_FUEL_DEPRECIATION + &
                       LEASOR_NF_BOOK_DEPRECIATION(RUN_YEAR,ASSET_CLASS)
               R_LEASOR_NF_SL_TAX_DEPRECIATION = &
                     LEASOR_NF_SL_TAX_DEPRECIATION(RUN_YEAR,ASSET_CLASS)
            ENDIF
         ENDIF
      RETURN

      ENTRY READ_NUC_FUEL_BASE_CASE

         CALL SET_UP_NUC_FUEL_ARRAYS
         CALL OPEN_NF_BASE_CASE_FILE(INT(10,2))
         READ(10) TNFCE,TNFIP,TRBFIP,TNFES,TNFIS,TNNFV,TNFRB, &
                  TAFDC1,TAFC1C,TAFDC2,TAFC2C,TTXEXP,TDDB, &
                  TRBDDB,TDBAMR,TEXEXP,TAFEXP,TAFCAJ,TWODFT, &
                  TAFDCB,TAFDCD,TNFCE_LEASED,TPCAPINRST, &
                  JPCAPINRST,JTXEXP,JDBAMR,RBCWIP_AFDC_METH2, &
                  RB_AFDC1, &
                  OWNED_NF_TAX_DEP, &
                  OWNED_NF_SL_TAX_DEPRECIATION, &
                  LEASOR_NF_DEFERRED_TAX_BASIS, &
                  LEASOR_NF_BOOK_DEPRECIATION, &
                  LEASOR_NF_SL_TAX_DEPRECIATION, &
                  LEASOR_NF_TAX_DEP
         CLOSE(10)
      RETURN

      ENTRY SET_UP_NUC_FUEL_ARRAYS

         FINANCIAL_SIMULATION_YEARS = RUN_YEARS() + EXTENSION_YEARS()+1
         CALL RETURN_NUM_OF_NUC_FUEL_CLASSES(NUM_OF_ASSET_CLASSES, &
                                                    MAX_ASSET_CLASS_NUM)
         IF(ALLOCATED(ASSET_CLASS_POINTER)) &
                                         DEALLOCATE(ASSET_CLASS_POINTER)
         IF(MAX_ASSET_CLASS_NUM > 0) THEN
            ALLOCATE(ASSET_CLASS_POINTER(MAX_ASSET_CLASS_NUM))
            CALL RETURN_NUC_FUEL_POINTER(ASSET_CLASS_POINTER)
         ENDIF
         IF(ALLOCATED(TNNFV)) THEN
            DEALLOCATE(TNNFV,TDDB,TDBAMR,TRBDDB,TNFRB, &
                       TNFIP,TRBFIP,TNFCE,TAFC1C,TNFIS,TAFC2C,TNFES, &
                       TAFDC2,TAFDC1,TEXEXP,TTXEXP,TAFEXP,TAFCAJ, &
                       TWODFT,TAFDCB,TAFDCD,TNFCE_LEASED,TPCAPINRST, &
                       JDBAMR,JPCAPINRST,JTXEXP, &
                       RBCWIP_AFDC_METH2,RB_AFDC1, &
                       OWNED_NF_TAX_DEP,LEASOR_NF_TAX_DEP, &
                       OWNED_NF_SL_TAX_DEPRECIATION, &
                       LEASOR_NF_DEFERRED_TAX_BASIS, &
                       LEASOR_NF_BOOK_DEPRECIATION, &
                       LEASOR_NF_SL_TAX_DEPRECIATION)
         ENDIF
         ALLOCATE(TNNFV(FINANCIAL_SIMULATION_YEARS, &
                                                0:NUM_OF_ASSET_CLASSES), &
                  TDDB(FINANCIAL_SIMULATION_YEARS, &
                                                0:NUM_OF_ASSET_CLASSES), &
                  TDBAMR(FINANCIAL_SIMULATION_YEARS, &
                                                0:NUM_OF_ASSET_CLASSES), &
                  TRBDDB(FINANCIAL_SIMULATION_YEARS, &
                                                0:NUM_OF_ASSET_CLASSES), &
                  TNFRB(FINANCIAL_SIMULATION_YEARS, &
                                                0:NUM_OF_ASSET_CLASSES), &
                  TNFIP(FINANCIAL_SIMULATION_YEARS, &
                                                0:NUM_OF_ASSET_CLASSES), &
                  TRBFIP(FINANCIAL_SIMULATION_YEARS, &
                                                0:NUM_OF_ASSET_CLASSES), &
                  TNFCE(FINANCIAL_SIMULATION_YEARS, &
                                                0:NUM_OF_ASSET_CLASSES), &
                  TAFC1C(FINANCIAL_SIMULATION_YEARS, &
                                                0:NUM_OF_ASSET_CLASSES), &
                  TNFIS(FINANCIAL_SIMULATION_YEARS, &
                                                0:NUM_OF_ASSET_CLASSES), &
                  TAFC2C(FINANCIAL_SIMULATION_YEARS, &
                                                0:NUM_OF_ASSET_CLASSES), &
                  TNFES(FINANCIAL_SIMULATION_YEARS, &
                                                0:NUM_OF_ASSET_CLASSES), &
                  TAFDC2(FINANCIAL_SIMULATION_YEARS, &
                                                0:NUM_OF_ASSET_CLASSES), &
                  TAFDC1(FINANCIAL_SIMULATION_YEARS, &
                                                0:NUM_OF_ASSET_CLASSES), &
                  TEXEXP(FINANCIAL_SIMULATION_YEARS, &
                                                0:NUM_OF_ASSET_CLASSES), &
                  TTXEXP(FINANCIAL_SIMULATION_YEARS, &
                                                0:NUM_OF_ASSET_CLASSES), &
                  TAFEXP(FINANCIAL_SIMULATION_YEARS, &
                                                0:NUM_OF_ASSET_CLASSES), &
                  TAFCAJ(FINANCIAL_SIMULATION_YEARS, &
                                                0:NUM_OF_ASSET_CLASSES), &
                  TWODFT(FINANCIAL_SIMULATION_YEARS, &
                                                0:NUM_OF_ASSET_CLASSES), &
                  TAFDCB(FINANCIAL_SIMULATION_YEARS, &
                                                0:NUM_OF_ASSET_CLASSES), &
                  TAFDCD(FINANCIAL_SIMULATION_YEARS, &
                                                0:NUM_OF_ASSET_CLASSES), &
                  TNFCE_LEASED(FINANCIAL_SIMULATION_YEARS, &
                                                0:NUM_OF_ASSET_CLASSES), &
                  TPCAPINRST(FINANCIAL_SIMULATION_YEARS, &
                                                0:NUM_OF_ASSET_CLASSES), &
                  OWNED_NF_TAX_DEP(FINANCIAL_SIMULATION_YEARS, &
                                                0:NUM_OF_ASSET_CLASSES), &
                OWNED_NF_SL_TAX_DEPRECIATION(FINANCIAL_SIMULATION_YEARS, &
                                                0:NUM_OF_ASSET_CLASSES), &
                  LEASOR_NF_TAX_DEP(FINANCIAL_SIMULATION_YEARS, &
                                                0:NUM_OF_ASSET_CLASSES), &
                LEASOR_NF_DEFERRED_TAX_BASIS(FINANCIAL_SIMULATION_YEARS, &
                                                0:NUM_OF_ASSET_CLASSES), &
                 LEASOR_NF_BOOK_DEPRECIATION(FINANCIAL_SIMULATION_YEARS, &
                                                0:NUM_OF_ASSET_CLASSES), &
               LEASOR_NF_SL_TAX_DEPRECIATION(FINANCIAL_SIMULATION_YEARS, &
                                                0:NUM_OF_ASSET_CLASSES), &
                  JDBAMR(FINANCIAL_SIMULATION_YEARS), &
                  JPCAPINRST(FINANCIAL_SIMULATION_YEARS), &
                  JTXEXP(FINANCIAL_SIMULATION_YEARS), &
                  RBCWIP_AFDC_METH2(FINANCIAL_SIMULATION_YEARS), &
                  RB_AFDC1(FINANCIAL_SIMULATION_YEARS))
!
      RETURN

      ENTRY INIT_NUC_FUEL_ARRAYS

!
         TNNFV = 0.
         TDDB = 0.
         TDBAMR = 0.
         TRBDDB = 0.
         TNFRB = 0.
         TNFIP = 0.
         TRBFIP = 0.
         TNFCE = 0.
         TAFC1C = 0.
         TNFIS = 0.
         TAFC2C = 0.
         TNFES = 0.
         TAFDC2 = 0.
         TAFDC1 = 0.
         TEXEXP = 0.
         TTXEXP = 0.
         TAFEXP = 0.
         TAFCAJ = 0.
         TWODFT = 0.
         TAFDCB = 0.
         TAFDCD = 0.
         TNFCE_LEASED = 0.
         TPCAPINRST = 0.
         OWNED_NF_TAX_DEP = 0.
         OWNED_NF_SL_TAX_DEPRECIATION = 0.
         LEASOR_NF_TAX_DEP = 0.
         LEASOR_NF_DEFERRED_TAX_BASIS = 0.
         LEASOR_NF_SL_TAX_DEPRECIATION = 0.
         LEASOR_NF_BOOK_DEPRECIATION = 0.
!
         JDBAMR = 0.
         JPCAPINRST = 0.
         JTXEXP = 0.
         RBCWIP_AFDC_METH2 = 0.
         RB_AFDC1 = 0.
      RETURN

! *                                                                     *
! *                           N F T O T L                               *
! *                                                                     *
! *          COPYRIGHT (C) 1983    M.S. GERBER & ASSOCIATES, INC.       *
! *                         ALL RIGHTS RESERVED                         *
! *                                                                     *

!                                                                       *
      ENTRY NFTOTL(R_ABMETH,R_ABYEAR,R_TAXWO,R_PCLEASED, &
                   R_REGULATORY_ALLOCATOR,R_ABTAX, &
                   R_ASSET_CLASS,R_ASSET_CLASS_VECTOR, &
                   R_ALLOCATION_TO_OWNER)
!
!  ALLOCATE TO TOTAL COMPANY AND ASSET CLASSES
!
      VOID_LOGICAL = RETURN_ASSET_CLASS_LISTS(R_ASSET_CLASS, &
                                          ASSET_CLASS_LIST, &
                                          R_ASSET_CLASS_VECTOR, &
                                          ASSET_ALLOCATION_LIST)
      CLASS_POINTER = 1
!
      DO
         CLASS = ASSET_CLASS_LIST(CLASS_POINTER)
         CALL CHECK_IF_CLASS_DEFINED(CLASS)
         CLASS = CLASS + 1
         IF(CLASS > 0) CLASS = ASSET_CLASS_POINTER(CLASS)

!
         IF(ASSET_ALLOCATION_LIST(CLASS_POINTER) < 0.) THEN
            ALLOCATION_VECTOR = &
                               ABS(ASSET_ALLOCATION_LIST(CLASS_POINTER))
            CALL GET_ASSET_VAR(ALLOCATION_VECTOR, &
                                            DUMMY_TYPE,ALLOCATION_VALUE)
         ELSE
            ALLOCATOR = ASSET_ALLOCATION_LIST(CLASS_POINTER)
!
            DO I = 1, AVAIL_DATA_YEARS
               ALLOCATION_VALUE(I) = ALLOCATOR
            ENDDO
         ENDIF
!
         CLASS_ALLOCATOR = ALLOCATION_VALUE(1)/100.
         DO I = 1, FINANCIAL_SIMULATION_YEARS
            ALLOCATOR = R_ALLOCATION_TO_OWNER(I) * CLASS_ALLOCATOR
            TNFCE(I,CLASS)  = TNFCE(I,CLASS)  + NFCE(I) * ALLOCATOR
            TNFCE_LEASED(I,CLASS) = TNFCE_LEASED(I,CLASS) + &
                                        ALLOCATOR * NFCE(I) * R_PCLEASED
            TNFIP(I,CLASS)  = TNFIP(I,CLASS)  + CWIP(I) * ALLOCATOR
            TNFES(I,CLASS)  = TNFES(I,CLASS)  + NFES(I) * ALLOCATOR
            TNFIS(I,CLASS)  = TNFIS(I,CLASS)  + NFIS(I) * ALLOCATOR
            TAFDC1(I,CLASS) = TAFDC1(I,CLASS) + AFDC1(I) * ALLOCATOR
            TAFDCB(I,CLASS) = TAFDCB(I,CLASS) + AFDC1B(I) * ALLOCATOR
            TAFDCD(I,CLASS) = TAFDCD(I,CLASS) + AFDCDF(I) * ALLOCATOR
            TAFC1C(I,CLASS) = TAFC1C(I,CLASS) + AFDC1C(I) * ALLOCATOR
            TAFDC2(I,CLASS) = TAFDC2(I,CLASS) + AFDC2(I) * ALLOCATOR
            TAFC2C(I,CLASS) = TAFC2C(I,CLASS) + AFDC2C(I) * ALLOCATOR
            TNNFV(I,CLASS)  = TNNFV(I,CLASS)  + NNFV(I) * ALLOCATOR
            TRBFIP(I,CLASS) = TRBFIP(I,CLASS) + RBCWIP(I) * ALLOCATOR
            TNFRB(I,CLASS)  = TNFRB(I,CLASS)  + NFRB(I) * ALLOCATOR
            TTXEXP(I,CLASS) = TTXEXP(I,CLASS) + TAXEXP(I) * ALLOCATOR
            TPCAPINRST(I,CLASS) = TPCAPINRST(I,CLASS) + &
                                                PCAPINRST(I) * ALLOCATOR
            IF(INDEX(FUEL_OWNERSHIP,"Leasor") /= 0) THEN

               LEASOR_NF_TAX_DEP(I,CLASS) = LEASOR_NF_TAX_DEP(I,CLASS) + &
                                               NF_TAX_DEP(I) * ALLOCATOR
               LEASOR_NF_DEFERRED_TAX_BASIS(I,CLASS) = &
                                 LEASOR_NF_DEFERRED_TAX_BASIS(I,CLASS) + &
                                    NF_DEFERRED_TAX_BASIS(I) * ALLOCATOR
               LEASOR_NF_SL_TAX_DEPRECIATION(I,CLASS) = &
                                LEASOR_NF_SL_TAX_DEPRECIATION(I,CLASS) + &
                                   NF_SL_TAX_DEPRECIATION(I) * ALLOCATOR
               LEASOR_NF_BOOK_DEPRECIATION(I,CLASS) = &
                                  LEASOR_NF_BOOK_DEPRECIATION(I,CLASS) + &
                                     NF_BOOK_DEPRECIATION(I) * ALLOCATOR
            ELSEIF(INDEX(FUEL_OWNERSHIP,"Owned") /= 0) THEN
               OWNED_NF_TAX_DEP(I,CLASS) = OWNED_NF_TAX_DEP(I,CLASS) + &
                                               NF_TAX_DEP(I) * ALLOCATOR
               OWNED_NF_SL_TAX_DEPRECIATION(I,CLASS) = &
                                OWNED_NF_SL_TAX_DEPRECIATION(I,CLASS) + &
                                   NF_SL_TAX_DEPRECIATION(I) * ALLOCATOR
            ENDIF
            IF(CLASS_POINTER == 1) THEN
               JPCAPINRST(I) = JPCAPINRST(I) + PCAPINRST(I) * &
                                                  R_REGULATORY_ALLOCATOR
               RBCWIP_AFDC_METH2(I) = RBCWIP_AFDC_METH2(I) + &
                                        (CWIP(I)+AFDC1C(I)-AFDC2C(I)) * &
                                                  R_REGULATORY_ALLOCATOR
               RB_AFDC1(I) = RB_AFDC1(I)+AFDC1(I)*R_REGULATORY_ALLOCATOR
            ENDIF
!
            IF(R_ABMETH > 0) THEN

               IF(I == R_ABYEAR - BASEYEAR + 1) THEN
                  TTXEXP(I,CLASS) = TTXEXP(I,CLASS)+R_TAXWO * ALLOCATOR
                  IF(R_ABMETH /= 6 .AND. CLASS_POINTER == 1) &
                          JTXEXP(I) = JTXEXP(I) + R_TAXWO * &
                                        R_REGULATORY_ALLOCATOR * R_ABTAX
               ENDIF
               IF(I >= R_ABYEAR - BASEYEAR + 1) THEN
                  TDDB(I,CLASS)   = TDDB(I,CLASS)  +DDB(I)*ALLOCATOR
                  TRBDDB(I,CLASS) = TRBDDB(I,CLASS)+RBDDB(I)*ALLOCATOR
                  TDBAMR(I,CLASS) = TDBAMR(I,CLASS)+DBAMRT(I)*ALLOCATOR
                  TEXEXP(I,CLASS) = TEXEXP(I,CLASS)+EXEXP(I)*ALLOCATOR
                  TAFEXP(I,CLASS) = TAFEXP(I,CLASS)+AFCEXP(I)*ALLOCATOR
                  TAFCAJ(I,CLASS) = TAFCAJ(I,CLASS)+AJAFDC(I)*ALLOCATOR
                  TWODFT(I,CLASS) = TWODFT(I,CLASS)+WODFTX(I)*ALLOCATOR
                  IF(CLASS_POINTER == 1) JDBAMR(I)=JDBAMR(I)+DBAMRT(I) * &
                                                  R_REGULATORY_ALLOCATOR
               ENDIF
            ENDIF
            IF(I <= AVAIL_DATA_YEARS) CLASS_ALLOCATOR = &
                                                ALLOCATION_VALUE(I)/100.
         ENDDO
!
         CLASS_POINTER = CLASS_POINTER + 1
         IF(CLASS_POINTER > AVAIL_DATA_YEARS) EXIT
         IF(ASSET_CLASS_LIST(CLASS_POINTER) == -99 .OR. &
                             ASSET_CLASS_LIST(CLASS_POINTER) == 0.) EXIT
      ENDDO
      RETURN

! *                                                                     *
! *                           N F A B N D                               *
! *                                                                     *
!*          COPYRIGHT (C) 1983    M.S. GERBER & ASSOCIATES, INC        *
! *                         ALL RIGHTS RESERVED                         *
! *                                                                     *

!                                                                       *
      ENTRY NFABND(R_ABYEAR,R_ABMETH,R_ABTAX,R_TAXWO, &
                   R_BOKBL,R_BOKDAL, &
                   R_BOKWO,R_AFCBL,R_AFCDAL,R_AFCWO,R_NRTXWO)
!
      LASTYR = R_ABYEAR - BASEYEAR
      IF(LASTYR >= FINANCIAL_SIMULATION_YEARS .OR. LASTYR < 1) THEN
         R_ABMETH = 0
         RETURN
      ENDIF
      IF(R_ABTAX == 0.) R_ABMETH = 6
      CAFDCD = 0.0
      DO J = 2, LASTYR
         CAFDCD = CAFDCD + AFDCDF(J)
      ENDDO
!
!     WRITE OFF  EFFECTS ARE COMPUTED IN NEXT SEVEN LINES:
!
      LASTYR = LASTYR + 1
!      TAX DEDCUTION
      R_TAXWO  = NNFV(LASTYR) - AFDC1C(LASTYR)
!      AMOUNT OF TAX AMORTIZED IN A DEFERRED TAX ACCOUNT
      R_NRTXWO = R_TAXWO
!      TOTAL BOOK WO
      R_BOKBL  = NNFV(LASTYR) - AFDC1C(LASTYR)
!      BOOK WO BELOW THE LINE
      R_BOKDAL = R_BOKBL * (1.-R_ABTAX)
!      BOOK WO ABOVE THE LINE
      R_BOKWO  = R_BOKBL - R_BOKDAL
!      TOTAL AFUDC WO
      R_AFCBL  = AFDC1C(LASTYR)
!      AFUDC WO BELOW THE LINE
      R_AFCDAL = R_AFCBL * (1.-R_ABTAX)
!      AFUDC WO ABOVE THE LINE
      R_AFCWO  = R_AFCBL - R_AFCDAL
!
!
!      THEN ABANDONMENT ITEMS ARE ZEROED
!
      TEMP1 = NFCE(LASTYR)
      TEMP2 = AFDC1(LASTYR)
      TEMP3 = AFDC1B(LASTYR)
      DO J = LASTYR, FINANCIAL_SIMULATION_YEARS
         NFCE(J)   = 0.0
         NFES(J)   = 0.0
         CWIP(J)   = 0.0
         RBCWIP(J) = 0.0
         NFIS(J)   = 0.0
         AFDC1(J)  = 0.0
         AFDC1C(J) = 0.0
         AFDC2(J)  = 0.0
         AFDC2C(J) = 0.0
         AFDCDF(J) = 0.0
         AFDC1B(J) = 0.0
         NNFV(J)   = 0.0
         TAXEXP(J) = 0.0
         NFRB(J)   = 0.0
      ENDDO
!
!      ADJUSTMENTS ARE MADE TO THE PLANT PARAMETERS
!      TO REFLECT THE WRITE-OFF AMOUNTS
!
      NFCE(LASTYR)   = TEMP1
      AFDC1(LASTYR)  = TEMP2
      AFDC1B(LASTYR) = TEMP3
      AFDCDF(LASTYR) = -CAFDCD
      AFDC2(LASTYR)  = -AFDC2C(LASTYR-1)
      RETURN
      END

      SUBROUTINE MARCS_TAX_DEPRECIATION(TAXLF,DBRATE,TAXDP,CEPTX, &
                                        FINANCIAL_SIMULATION_YEARS)

!
      use SpinDriftLib
      use prod_arrays_dimensions
      USE SIZECOM

      REAL TAXLF,DBRATE,SL_OFFSET
      INTEGER(kind=2) :: FINANCIAL_SIMULATION_YEARS
      LOGICAL(kind=1) :: NO_SWITCH,DUMP_AT_TAX_LIFE
      INTEGER(kind=2) :: J,L,M,IVEC
      REAL :: TAXDPC,GPVF,DEPDB,DEPSL
      REAL :: NPVFT
      REAL(kind=4) :: VECTOR_VALUES(AVAIL_DATA_YEARS)
      CHARACTER(len=1) :: DUMMY_TYPE
!      EXTERNAL VARIABLES
      REAL :: TAXDP(FINANCIAL_SIMULATION_YEARS), &
           CEPTX(FINANCIAL_SIMULATION_YEARS)
      REAL :: DEP_AMOUNT
!
         IF(TAXLF > 98.0) RETURN
         IF(TAXLF <= 1.0) THEN
            IF(TAXLF >= 0.) THEN
              DO L = 2, FINANCIAL_SIMULATION_YEARS
                  TAXDP(L) = CEPTX(L)
               ENDDO
            ELSE
               IVEC = ABS(TAXLF)
               CALL GET_ASSET_VAR(IVEC,DUMMY_TYPE,VECTOR_VALUES)
               DO J=1,MIN(FINANCIAL_SIMULATION_YEARS-INT(1,2), &
                          AVAIL_DATA_YEARS)
                  TAXDP(J+1) = VECTOR_VALUES(J)
               ENDDO
          ENDIF
            RETURN
         ENDIF
         SL_OFFSET = .5
         DUMP_AT_TAX_LIFE = .FALSE.
         NO_SWITCH = .FALSE.
         DO J = 2, FINANCIAL_SIMULATION_YEARS
            IF(CEPTX(J) /= 0.) THEN
               NPVFT = CEPTX(J) / 2.
               TAXDPC = 0.
               GPVF = CEPTX(J)
               DO L = J, FINANCIAL_SIMULATION_YEARS
                  DEPDB = DBRATE * NPVFT/TAXLF
                  IF(TAXLF - FLOAT(L - J) + SL_OFFSET > 0) THEN
                     DEPSL = NPVFT/(TAXLF - FLOAT(L - J) + SL_OFFSET)
                  ELSEIF(DUMP_AT_TAX_LIFE) THEN
                     TAXDP(L) = TAXDP(L) + NPVFT
                     EXIT
                  ELSE
                     DEPSL = DEPDB
                  ENDIF
                  IF(NO_SWITCH .OR. DEPDB >= DEPSL) THEN
!                     THEN TAKE THE DB VALUE FOR THE EXPENSE
                     TAXDP(L) = TAXDP(L) + DEPDB
                     TAXDPC =  TAXDPC + DEPDB
                     NPVFT = GPVF - TAXDPC
!                  ELSE TAKE THE STRAIGHT LINE UNTIL NPV IS ZERO
                  ELSE
                     DO M = L, FINANCIAL_SIMULATION_YEARS
                        IF(NPVFT < DEPSL) THEN
                           TAXDP(M) = TAXDP(M) + NPVFT
                           EXIT
                        ENDIF
                        TAXDP(M) = TAXDP(M) + DEPSL
                        NPVFT = NPVFT - DEPSL
                        IF(NPVFT < .0001) NPVFT = 0.
                     ENDDO
                     EXIT
                  ENDIF
               ENDDO
            ENDIF
         ENDDO
         RETURN
      END

