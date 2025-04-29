!     Last change: MSG 1/4/2013 8:20:40 AM

!
!                    CONVERT THE SERVICE-TRANSACTIONS FILE
!                             COPYRIGHT (C) 1992
!                        M.S. GERBER & ASSOCIATES, INC.
!                             ALL RIGHTS RESERVED
!

      SUBROUTINE TR_OBJECT
      use end_routine, only: end_program, er_message
      use filename_tracker
      use service_decs
      use wvpartrk_interfaces


      use SpinDriftLib
      use prod_arrays_dimensions
      use sizecom
      implicit none
      INTEGER (kind=2) ::  DELETE,IREC,INUNIT,LRECL/270/
      INTEGER (kind=4) ::  IOS,IOS_BASE
      INTEGER (kind=2) ::  UNIT_NUM/10/
      INTEGER (kind=2) ::  R_NUM_SERVICE_TRANS
      INTEGER (kind=4) ::  R_UNIT_NUM
      CHARACTER (len=5) ::  BASE_FILE_NAME,OVERLAY_FAMILY_NAME,

     +            CAP_ENRG_TRANSACTIONS_FILE
      CHARACTER (len=20) ::  REVENUE_CLASSIFICATION
      CHARACTER (len=20) ::  EXPENSE_CLASSIFICATION
      CHARACTER (len=50) ::  COMMENT
      CHARACTER (len=256) ::  FILE_NAME
      CHARACTER (len=256) ::  BASE_FILE_DIRECTORY
      CHARACTER (len=256) ::  DATA_DRIVE,OUTPUT_DIRECTORY
      LOGICAL (kind=4) ::  FILE_EXISTS/.FALSE./
      LOGICAL (kind=4) ::  R_SERVICE_TRANSACTIONS_ACTIVE
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER (len=1024) ::  RECLN
! DECLARATION FOR THE SERVICE TRANSACTIONS FILE
      INTEGER (kind=2) ::  SERVICE_REPORTING_GROUP,
     +          MONTH_SERVICE_AVAILABLE,YEAR_SERVICE_AVAILABLE,
     +          MONTH_SERVICE_ENDS,YEAR_SERVICE_ENDS,UPDATE_MONTH,
     +          RESOURCE_POINTER,CAP_ESCALATION_VECTOR,
     +          ENRG_ESCALATION_VECTOR

      INTEGER (kind=4) ::  SERVICE_ID_NUMBER
      REAL (kind=4) ::  ANNUAL_CAPACITY,CAPACITY_DISTRIBUTION_PATTERN,
     + CAPACITY_CHARGE_ord,ANNUAL_ENERGY,ENERGY_DISTRIBUTION_PATTERN,
     + ENERGY_CHARGE_ord
      CHARACTER (len=20) ::  SERVICE_NAME_loc
      CHARACTER (len=20) ::  FILE_TYPE = 'Service Transactions'
      CHARACTER (len=1) ::  TYPE_OF_SERVICE_ord,COST_ASSIGNMENT,

     +            INTRA_COMPANY_TRANSACTION,
     +            SERVICE_ACTIVE,
     +            WVPA_RATE_TRACKER,
     +            WVPA_RES_TRACKER,
     +            WVPA_FUEL_TRACKER,
     +            WVPA_MEM_TRACKER

      INTEGER (kind=2) ::  INTRA_ASSET_CLASS_ID
      INTEGER (kind=2) ::  INTRA_ASSET_CLASS_ALLOC_VECTOR
      CHARACTER (len=20) ::  INTRA_ACCOUNT_CLASSIFICATION
      CHARACTER (len=3) ::  INTRA_EXPENSE_COLLECTION,EXPENSE_COLLECTION
      CHARACTER (len=2) ::  SERVICE_TRANS_OL/'BC'/
      CHARACTER (len=5) ::  LINK_TYPE
      CHARACTER (len=1) ::  ENERGY_TO_USE_ord
! ASSET CLASS VARIABLES
      INTEGER (kind=2) ::  ASSET_CLASS_NUM,ASSET_CLASS_VECTOR
      LOGICAL (kind=1) ::  LAHEY_LF95
      CHARACTER (len=30) ::  SCREEN_OUTPUT
! END OF DATA DECLARATIONS

      ENTRY TR_MAKEBIN

!

      BASE_FILE_NAME = CAP_ENRG_TRANSACTIONS_FILE()
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = get_trb_filename(base_file_name)
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      ns_service_decs%NUM_SERVICE_TRANS = 0
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
            CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
         ENDIF
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCTRANS.BIN",ACCESS="DIRECT",
     +                                     STATUS="UNKNOWN",RECL=LRECL)
         IREC = 0
         READ(10,*) DELETE

         DO
! ASSET CLASS INITIALIZE
            ASSET_CLASS_NUM = 0
            ASSET_CLASS_VECTOR = 0
            REVENUE_CLASSIFICATION = ' '
            INTRA_COMPANY_TRANSACTION = 'N'
            INTRA_ASSET_CLASS_ID = 0
            INTRA_ASSET_CLASS_ALLOC_VECTOR = 0
            INTRA_ACCOUNT_CLASSIFICATION = ' '
            INTRA_EXPENSE_COLLECTION = ' '
            EXPENSE_CLASSIFICATION = 'Service'
            TYPE_OF_SERVICE_ord = 'T' ! ADDED 9/4/97. GAT.
            UPDATE_MONTH = 1
            MONTH_SERVICE_AVAILABLE = 1
            YEAR_SERVICE_AVAILABLE = 1990
            MONTH_SERVICE_ENDS = 12
            YEAR_SERVICE_ENDS = 2100
            SERVICE_ACTIVE = 'T'
            energy_to_use_ord = 'B'
            WVPA_RATE_TRACKER = 'N'
            WVPA_RES_TRACKER = 'N'
            WVPA_FUEL_TRACKER = 'N'
            WVPA_MEM_TRACKER = 'M'

            DO
               READ(10,1000,IOSTAT=IOS) RECLN
               IF(IOS .NE. 0) EXIT
               IF(RECLN(1:1) == '7') EXIT
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
              READ(RECLN,*,ERR=200) DELETE,SERVICE_NAME_LOC,
     +                            SERVICE_ID_NUMBER,
     +                            TYPE_OF_SERVICE_ord,COST_ASSIGNMENT,
     +                            EXPENSE_COLLECTION,
     +                            SERVICE_REPORTING_GROUP,
     +                            MONTH_SERVICE_AVAILABLE,
     +                            YEAR_SERVICE_AVAILABLE,
     +                            MONTH_SERVICE_ENDS,YEAR_SERVICE_ENDS,
     +                            UPDATE_MONTH,
     +                            LINK_TYPE,
     +                            RESOURCE_POINTER,
     +                            ANNUAL_CAPACITY,
     +                            CAPACITY_DISTRIBUTION_PATTERN,
     +                            capacity_charge_ord,
     +                            CAP_ESCALATION_VECTOR,
     +                            ANNUAL_ENERGY,
     +                            ENERGY_DISTRIBUTION_PATTERN,
     +                            ENERGY_CHARGE_ord,
     +                            ENRG_ESCALATION_VECTOR,
     +                            COMMENT,
     +                            ASSET_CLASS_NUM,
     +                            ASSET_CLASS_VECTOR,
     +                            REVENUE_CLASSIFICATION,
     +                            INTRA_COMPANY_TRANSACTION, ! 26
     +                            INTRA_ASSET_CLASS_ID,
     +                            INTRA_ASSET_CLASS_ALLOC_VECTOR,
     +                            INTRA_ACCOUNT_CLASSIFICATION, ! 29
     +                            INTRA_EXPENSE_COLLECTION,
     +                            EXPENSE_CLASSIFICATION, !31
     +                            SERVICE_ACTIVE,
     +                            energy_to_use_ord,
     +                            WVPA_RATE_TRACKER,
     +                            WVPA_RES_TRACKER,
     +                            WVPA_FUEL_TRACKER,
     +                            WVPA_MEM_TRACKER
               IREC = IREC + 1
              WRITE(11,REC=IREC) DELETE,SERVICE_NAME_LOC,
     + SERVICE_ID_NUMBER,
     +                         TYPE_OF_SERVICE_ord,COST_ASSIGNMENT,
     +                         EXPENSE_COLLECTION,
     +                         SERVICE_REPORTING_GROUP,
     +                         MONTH_SERVICE_AVAILABLE,
     +                         YEAR_SERVICE_AVAILABLE,
     +                         MONTH_SERVICE_ENDS,YEAR_SERVICE_ENDS,
     +                         UPDATE_MONTH,
     +                         LINK_TYPE,
     +                         RESOURCE_POINTER,
     +                         ANNUAL_CAPACITY,
     +                         CAPACITY_DISTRIBUTION_PATTERN,
     +                         capacity_charge_ord,
     +                         CAP_ESCALATION_VECTOR,
     +                         ANNUAL_ENERGY,
     +                         ENERGY_DISTRIBUTION_PATTERN,
     +                         ENERGY_CHARGE_ord,
     +                         ENRG_ESCALATION_VECTOR,
     +                         ASSET_CLASS_NUM,
     +                         ASSET_CLASS_VECTOR,
     +                         REVENUE_CLASSIFICATION,
     +                         INTRA_COMPANY_TRANSACTION,
     +                         INTRA_ASSET_CLASS_ID,
     +                         INTRA_ASSET_CLASS_ALLOC_VECTOR,
     +                         INTRA_ACCOUNT_CLASSIFICATION, ! 29
     +                         INTRA_EXPENSE_COLLECTION, !30
     +                         EXPENSE_CLASSIFICATION, !31
     +                         SERVICE_ACTIVE,
     +                         energy_to_use_ord,
     +                         WVPA_RATE_TRACKER,
     +                         WVPA_RES_TRACKER,
     +                         WVPA_FUEL_TRACKER,
     +                         WVPA_MEM_TRACKER
            ENDDO
            IF(IOS /= 0) EXIT
         ENDDO
         CLOSE(10)
         CLOSE(11)
         ns_service_decs%NUM_SERVICE_TRANS = IREC
      ELSE IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "servicac:1111")
      ENDIF
      RETURN

! OVERLAY THE SERVICE-TRANSACTIONS FILE

      ENTRY TR_MAKEOVL(OVERLAY_FAMILY_NAME)


      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = trim(DATA_DRIVE)//"TRO"//
     +                               trim(OVERLAY_FAMILY_NAME)//".DAT"
      OPEN(10,FILE=FILE_NAME)
      READ(10,*) DELETE
      INUNIT = 12
      IF(SERVICE_TRANS_OL == 'BC') THEN
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCTRANS.BIN",
     +                                      ACCESS="DIRECT",RECL=LRECL)
         INUNIT = 11
      ENDIF
      OPEN(12,FILE=trim(DATA_DRIVE)//"OLTRANS.BIN",ACCESS="DIRECT",
     +                                     STATUS="UNKNOWN",RECL=LRECL)
      IREC = 0
      DO
         DO
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(RECLN(1:1) == '7') EXIT
            IREC = IREC + 1
            READ(INUNIT,REC=IREC,IOSTAT=IOS_BASE) DELETE,
     + SERVICE_NAME_LOC,
     +                         SERVICE_ID_NUMBER,
     +                         TYPE_OF_SERVICE_ord,COST_ASSIGNMENT,
     +                         EXPENSE_COLLECTION,
     +                         SERVICE_REPORTING_GROUP,
     +                         MONTH_SERVICE_AVAILABLE,
     +                         YEAR_SERVICE_AVAILABLE,
     +                         MONTH_SERVICE_ENDS,YEAR_SERVICE_ENDS,
     +                         UPDATE_MONTH,
     +                         LINK_TYPE,
     +                         RESOURCE_POINTER,
     +                         ANNUAL_CAPACITY,
     +                         CAPACITY_DISTRIBUTION_PATTERN,
     +                         capacity_charge_ord,
     +                         CAP_ESCALATION_VECTOR,
     +                         ANNUAL_ENERGY,
     +                         ENERGY_DISTRIBUTION_PATTERN,
     +                         ENERGY_CHARGE_ord,
     +                         ENRG_ESCALATION_VECTOR,
     +                         ASSET_CLASS_NUM,
     +                         ASSET_CLASS_VECTOR,
     +                         REVENUE_CLASSIFICATION,
     +                         INTRA_COMPANY_TRANSACTION,
     +                         INTRA_ASSET_CLASS_ID,
     +                         INTRA_ASSET_CLASS_ALLOC_VECTOR,
     +                         INTRA_ACCOUNT_CLASSIFICATION, ! 29
     +                         INTRA_EXPENSE_COLLECTION, !30
     +                         EXPENSE_CLASSIFICATION, !31
     +                         SERVICE_ACTIVE,
     +                         energy_to_use_ord,
     +                         WVPA_RATE_TRACKER,
     +                         WVPA_RES_TRACKER,
     +                         WVPA_FUEL_TRACKER,
     +                         WVPA_MEM_TRACKER
            IF(IOS_BASE /= 0) EXIT
            IF(IOS == 0) THEN
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
             READ(RECLN,*,ERR=200) DELETE,SERVICE_NAME_LOC,
     +                            SERVICE_ID_NUMBER,
     +                            TYPE_OF_SERVICE_ord,COST_ASSIGNMENT,
     +                            EXPENSE_COLLECTION,
     +                            SERVICE_REPORTING_GROUP,
     +                            MONTH_SERVICE_AVAILABLE,
     +                            YEAR_SERVICE_AVAILABLE,
     +                            MONTH_SERVICE_ENDS,
     +                            YEAR_SERVICE_ENDS,
     +                            UPDATE_MONTH,
     +                            LINK_TYPE,
     +                            RESOURCE_POINTER,
     +                            ANNUAL_CAPACITY,
     +                            CAPACITY_DISTRIBUTION_PATTERN,
     +                            capacity_charge_ord,
     +                            CAP_ESCALATION_VECTOR,
     +                            ANNUAL_ENERGY,
     +                            ENERGY_DISTRIBUTION_PATTERN,
     +                            ENERGY_CHARGE_ord,
     +                            ENRG_ESCALATION_VECTOR,
     +                            COMMENT,
     +                            ASSET_CLASS_NUM,
     +                            ASSET_CLASS_VECTOR,
     +                            REVENUE_CLASSIFICATION,
     +                            INTRA_COMPANY_TRANSACTION,
     +                            INTRA_ASSET_CLASS_ID,
     +                            INTRA_ASSET_CLASS_ALLOC_VECTOR,
     +                            INTRA_ACCOUNT_CLASSIFICATION, ! 29
     +                            INTRA_EXPENSE_COLLECTION, !30
     +                            EXPENSE_CLASSIFICATION, !31
     +                            SERVICE_ACTIVE,
     +                            energy_to_use_ord,
     +                            WVPA_RATE_TRACKER,
     +                            WVPA_RES_TRACKER,
     +                            WVPA_FUEL_TRACKER,
     +                            WVPA_MEM_TRACKER
            ENDIF
            WRITE(12,REC=IREC) DELETE,
     + SERVICE_NAME_LOC,SERVICE_ID_NUMBER,
     +                      TYPE_OF_SERVICE_ord,COST_ASSIGNMENT,
     +                      EXPENSE_COLLECTION,
     +                      SERVICE_REPORTING_GROUP,
     +                      MONTH_SERVICE_AVAILABLE,
     +                      YEAR_SERVICE_AVAILABLE,
     +                      MONTH_SERVICE_ENDS,YEAR_SERVICE_ENDS,
     +                      UPDATE_MONTH,
     +                      LINK_TYPE,
     +                      RESOURCE_POINTER,
     +                      ANNUAL_CAPACITY,
     +                      CAPACITY_DISTRIBUTION_PATTERN,
     +                      capacity_charge_ord,
     +                      CAP_ESCALATION_VECTOR,
     +                      ANNUAL_ENERGY,
     +                      ENERGY_DISTRIBUTION_PATTERN,
     +                      ENERGY_CHARGE_ord,
     +                      ENRG_ESCALATION_VECTOR,
     +                      ASSET_CLASS_NUM,
     +                      ASSET_CLASS_VECTOR,
     +                      REVENUE_CLASSIFICATION,
     +                      INTRA_COMPANY_TRANSACTION,
     +                      INTRA_ASSET_CLASS_ID,
     +                      INTRA_ASSET_CLASS_ALLOC_VECTOR,
     +                      INTRA_ACCOUNT_CLASSIFICATION, ! 29
     +                      INTRA_EXPENSE_COLLECTION, !30
     +                      EXPENSE_CLASSIFICATION, !31
     +                      SERVICE_ACTIVE,
     +                      energy_to_use_ord,
     +                      WVPA_RATE_TRACKER,
     +                      WVPA_RES_TRACKER,
     +                      WVPA_FUEL_TRACKER,
     +                      WVPA_MEM_TRACKER
         ENDDO
         IF(IOS_BASE /= 0) EXIT
      ENDDO
      CLOSE(10)
      CLOSE(12)
      IF(SERVICE_TRANS_OL == 'BC') CLOSE(11)
      SERVICE_TRANS_OL = 'OL'
      RETURN

  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from Servicac SIID265'
      call end_program(er_message)
!

      ENTRY RESET_SERVICE_TRANS_OL

         SERVICE_TRANS_OL = 'BC'
      RETURN
!

      ENTRY OPEN_SERVICE_TRANS_FILE(R_UNIT_NUM,
     +                              R_SERVICE_TRANSACTIONS_ACTIVE,
     +                              R_NUM_SERVICE_TRANS)


         UNIT_NUM = R_UNIT_NUM
         R_NUM_SERVICE_TRANS = ns_service_decs%NUM_SERVICE_TRANS
         IF(ns_service_decs%NUM_SERVICE_TRANS > 0) THEN
            R_SERVICE_TRANSACTIONS_ACTIVE = FILE_EXISTS
            IF(FILE_EXISTS) OPEN(UNIT_NUM,
     +                           FILE=trim(OUTPUT_DIRECTORY())//
     +                                   SERVICE_TRANS_OL//"TRANS.BIN",
     +                                      ACCESS="DIRECT",RECL=LRECL)
         ELSE
            R_SERVICE_TRANSACTIONS_ACTIVE = .FALSE.
         ENDIF
      RETURN


      ENTRY CLOSE_SERVICE_TRANS_FILE

         IF(FILE_EXISTS) CLOSE(UNIT_NUM)
      RETURN


      ENTRY GET_NUMBER_OF_SERVICES (R_NUM_SERVICE_TRANS)

         R_NUM_SERVICE_TRANS = ns_service_decs%NUM_SERVICE_TRANS
      RETURN

 1000 FORMAT(A)
 1010 FORMAT('&',A,I4)
      END SUBROUTINE TR_OBJECT

!
!                         SERVICE TRANSACTIONS MODULE
!                              COPYRIGHT (C) 1992
!                        M.S. GERBER & ASSOCIATES, INC.
!                              ALL RIGHTS RESERVED
!

!
      RECURSIVE SUBROUTINE READ_SERVICE_TRANS(nunits_loc,
     +                                     SERVICE_TRANSACTIONS_ACTIVE)

      use mod_base_year
      use res_decs
      use irec_endpoint_control
      use prod_arrays_dimensions
      use sizecom
      use servcom
      use globecom
      use enrg_helper
      use service_decs
      use grx_planning_routines
      use spindriftlib
      use eco
      use lamcom
      use forecast
      use production
      use namescom
      use month_extra
      use wvpartrk_interfaces
      use mthnmcom
      
      ! service_defs holds data shared between this routine (servicac)
      ! and ENTRY routines moved to service_decs. Only service_decs
      ! and servicac can have knowledge of this module.
      use service_defs 
      use cls_load
      implicit none
      SAVE
	  real (kind=4) :: dbgvalue
	  REAL (kind=4) :: MONTH_VARS(0:12,*)
      integer (kind=2) :: class_pointer
      integer (kind=2) :: nunits_loc
      
      integer :: ub
      integer (kind=2) :: WVPA_TRACKING_TYPE ! external
      integer (kind=2), allocatable :: res_id_loc(:)
      logical(kind=1) :: kpcval

      LOGICAL (KIND=1) :: INTO_EXTENSION_PERIOD
      INTEGER (kind=4) ::  VALUES_2_ZERO
      INTEGER (kind=2) ::  EXP_TYPE,REV_TYPE,INCOME_STATEMENT_POSITION,
     +            R_TRANS_NO

      REAL (kind=4) ::  TRANSACTION_AMOUNT
      CHARACTER (len=24) ::  SERVICE_TITLE(MAX_SERVICE_ITEMS)
      REAL (kind=4) ::  SUM_OF_SERVICE(0:MAX_SERVICE_GROUPS)
      REAL (kind=4) ::  TOTAL,
     +      RPT_ENERGY_CHARGE,
     +      RPT_CAPACITY_CHARGE,

     +       R_RES_TRACKER,
     +       R_FUEL_TRACKER,
     +       R_RATE_TRACKER,
     +       R_MEM_TRACKER

      LOGICAL (kind=1) ::    WABASH_VALLEY_ACTIVE=.FALSE.
      LOGICAL (kind=1) ::   
     +            FIRST_EL_SERVICE_WRITE=.TRUE.,
     +            FIRST_CL_SERVICE_WRITE=.TRUE.,
     +            FIRST_CT_SERVICE_WRITE=.TRUE.,
     +            FIRST_OT_SERVICE_WRITE=.TRUE.,
     +            WVPA

      LOGICAL (kind=4) ::  SERVICE_TRANSACTIONS_ACTIVE
      INTEGER (kind=2) ::  GROUPS

      INTEGER ::  EL_RESOURCE_ID ! ALTERED 07/14/03.
      
      REAL (kind=4) ::  ST_REVENUE,ST_EXPENSE
!
      CHARACTER (len=1) ::  CLASS_CHR,UTILITY_TYPE
      INTEGER (kind=2) ::  DELETE
      INTEGER (kind=2) ::  IREC,SERVICE_TRANS_loc,I,YR,CLASS,DATE1,DATE2
      INTEGER (kind=4) ::  IOS
      

      REAL (kind=4) ::  get_escalated_value,ESCALATED_MONTHLY_VALUE
      REAL (kind=4) ::  GET_VAR
! 
      INTEGER (kind=2) ::   MONTH_SERVICE_AVAILABLE,
     +           YEAR_SERVICE_AVAILABLE,
     +           MONTH_SERVICE_ENDS,
     +           YEAR_SERVICE_ENDS
      REAL (kind=4) ::  ANNUAL_PEAK

      
      REAL (kind=4) ::  ANNUAL_ENERGY,ANNUAL_CAPACITY
      REAL (kind=4) ::  CLASS_FORECAST
!
      INTEGER (kind=2) ::  nr_NUNITS,ISEAS
      REAL*4 ENERGY(2*nr_NUNITS),CAPACITY(nr_NUNITS)
      INTEGER (kind=2) ::  L
      REAL (kind=4) ::  
     +     PEAK_RESERVE,ENER_RESERVE,
     +     GET_CUST_GROUP_ENERGY,GET_CUST_GROUP_PEAK
!
! MONTHLY SERVICE REPORT VARIABLES
!
      LOGICAL (kind=1) ::  MONTHLY_SERVICE_REPORT_ACTIVE/.FALSE./,
     +          LOGICAL1_TRUE/.TRUE./,
     +          INCLUDE_FIXED_COSTS_ADJ_CLAUSE,
     +          FIXED_COSTS_IN_ADJ_CLAUSE

      CHARACTER (len=1) ::  R_COST_ASSIGN(R_SERVICE_TRANS),
     +            R_TRAN_TYPE(R_SERVICE_TRANS),
     +            R_EXPENSE_ASSIGN(R_SERVICE_TRANS)
      CHARACTER (len=20) ::  R_NAME(R_SERVICE_TRANS)
      integer (kind=2), allocatable :: SERVICE_AVAILABLE(:)
      integer (kind=2), allocatable :: SERVICE_ENDS(:)
	   
      integer (kind=2), allocatable :: SERVICE_UPDATE_SEASON(:)
      integer (kind=2), allocatable :: SERVICE_RESOURCE_PONTR(:)
      integer (kind=2), allocatable :: SERVICE_CAP_ESCALATION_VECTOR(:)
      integer (kind=2), allocatable :: SERVICE_ENRG_ESCALATION_VECTOR(:)
      INTEGER (kind=4) ::  SERVICE_ID_NUMBER(:)
      REAL (kind=4) ::  SERVICE_ANNUAL_CAPACITY(:)
      REAL (kind=4) ::  SERVICE_CAPACITY_PATTERN(:),
     +     SERVICE_CAPACITY_CHARGE(:),SERVICE_ANNUAL_ENERGY(:),
     +     SERVICE_ENERGY_PATTERN(:),SERVICE_ENERGY_CHARGE(:)


      REAL (KIND=4), ALLOCATABLE :: SERVICE_FUEL_CHARGE(:),
     +                              SERVICE_FUEL_ESCALATION_VECTOR(:)
      CHARACTER (len=20) ::  SERVICE_REVENUE_CLASSIFICATION(:)
      character (len=1), allocatable :: 
     + SERVICE_INTRA_COMPY_TRANSACTION(:),
     +            ENERGY_TO_USE(:),
     +            WVPA_RATE_TRACKER(:)
     
      character (len=1) :: 
     +            SERVICE_ACTIVE,
     +            WVPA_RES_TRACKER,
     +            WVPA_FUEL_TRACKER,
     +            WVPA_MEM_TRACKER
     
      INTEGER (kind=2) ::  INTRA_ASSET_CLASS_ID(:),
     +          INTRA_ASSET_CLASS_ALLOC_VECTOR(:)

      CHARACTER (len=20) ::  INTRA_ACCOUNT_CLASSIFICATION(:)
      CHARACTER (len=3) ::  INTRA_EXPENSE_COLLECTION(:)
      ALLOCATABLE ::
     +            SERVICE_ID_NUMBER,
     +            SERVICE_ANNUAL_CAPACITY,
     +            SERVICE_CAPACITY_PATTERN,
     +            SERVICE_CAPACITY_CHARGE,
     +            SERVICE_ANNUAL_ENERGY,
     +            SERVICE_ENERGY_PATTERN,
     +            SERVICE_ENERGY_CHARGE,
     +            SERVICE_REVENUE_CLASSIFICATION,
     +            INTRA_ASSET_CLASS_ID,
     +            INTRA_ASSET_CLASS_ALLOC_VECTOR,
     +            INTRA_ACCOUNT_CLASSIFICATION,
     +            INTRA_EXPENSE_COLLECTION
!
      INTEGER (kind=2) ::  SEASON_HOURS(12),HOURS_IN_PERIOD
!
! ASSET ALLOCATION STUFF
!
      INTEGER (kind=2) ::  NUMBER_OF_SERVICE_CLASSES,UNIT_NO,
     +          SERVICE_ASSET_CLASS_POINTER(:)
      ALLOCATABLE :: SERVICE_ASSET_CLASS_POINTER

! ASSET ALLOCATION MANAGER STUFF

      REAL (kind=4) ::  VAR_COST,ST_FIXED_COST,TRANS_ENERGY

      REAL (kind=4) ::  TRANS_CAPACITY,ST_SO2_EMIS

      INTEGER (kind=2) ::  NUMBER_OF_SERVICES

      INTEGER (kind=2) ::  CURRENT_YEAR,CURRENT_YEAR_COMPARISON,
     +          UNIT,CLASS_NUM
      REAL (kind=4) ::  LAST_SEASON
      REAL (kind=4) ::  ANNUAL_ST_CAPACITY(:),ANNUAL_ST_ENERGY(:),

     +     ANNUAL_ST_VAR_COST(:),ANNUAL_ST_FIXED_COST(:),
     +     ANNUAL_ST_SO2_EMIS(:)
      ALLOCATABLE :: ANNUAL_ST_CAPACITY,ANNUAL_ST_ENERGY,
     +               ANNUAL_ST_VAR_COST,ANNUAL_ST_FIXED_COST,
     +               ANNUAL_ST_SO2_EMIS
      REAL (kind=4) ::  MONTHLY_ST_CAPACITY(:,:),MONTHLY_ST_ENERGY(:,:),
     +     MONTHLY_ST_VAR_COST(:,:),MONTHLY_ST_FIXED_COST(:,:),
     +     MONTHLY_ST_SO2_EMIS(:,:)
      ALLOCATABLE :: MONTHLY_ST_CAPACITY,MONTHLY_ST_ENERGY,
     +               MONTHLY_ST_VAR_COST,MONTHLY_ST_FIXED_COST,
     +               MONTHLY_ST_SO2_EMIS
!
! ASSET ALLOCATION STUFF
!
      CHARACTER (len=1) ::  DUMMY_TYPE
      INTEGER (kind=2) ::  ASSET_ALLOCATION_VECTOR
      INTEGER (kind=2) ::   J
      REAL (kind=4) ::  ASSET_CLASS_LIST(:),ASSET_ALLOCATION_LIST(:)
      REAL (kind=4) ::  ST_ANN_CLASS_ATL_EXPENSE(:,:),
     +     ST_ANN_CLASS_BTL_EXPENSE(:,:),
     +     ST_NF_RATEBASE(:,:),
     +     ST_ANN_CLASS_EXPENSE_CAPACITY(:,:),
     +     ST_ANN_CLASS_EXPENSE_ENERGY(:,:),
     +     ST_ANN_CLASS_REVENUE_ENERGY(:,:),
     +     ST_ANN_CLASS_REVENUE_CAPACITY(:,:),
     +     ST_ANN_CLASS_REVENUE(:,:,:)
      
       
      

      REAL (kind=4) ::  ASSET_ALLOCATOR
      ! NO NEED FOR ALLOCATION
      INTEGER (kind=2) ::  ASSET_CLASS_NUM(:),ASSET_CLASS_VECTOR(:)

      ALLOCATABLE :: ASSET_CLASS_NUM,ASSET_CLASS_VECTOR,
     +               ASSET_CLASS_LIST,
     +               ASSET_ALLOCATION_LIST,
     +               ST_ANN_CLASS_ATL_EXPENSE,
     +               ST_ANN_CLASS_BTL_EXPENSE,
     +               ST_NF_RATEBASE,
     +               ST_ANN_CLASS_EXPENSE_CAPACITY,
     +               ST_ANN_CLASS_EXPENSE_ENERGY,
     +               ST_ANN_CLASS_REVENUE_ENERGY,
     +               ST_ANN_CLASS_REVENUE_CAPACITY,
     +               ST_ANN_CLASS_REVENUE

!
! EXPENSE TRANSFER TO ASSET CLASS MODULE
!
      INTEGER (kind=2) ::  R_CLASS
      REAL (kind=4) ::  R_ATL_SERVICE_TRANSACTIONS_EXP,

     +     R_BTL_SERVICE_TRANSACTIONS_EXP,
     +     R_EXPENSE_COLLECTED_ADJ_CLAUSE,
     +     R_BTL_SALES_REVENUE,
     +     R_BASE_REVENUES,
     +     R_SECONDARY_SALES_REVENUE,
     +     R_CAPACITY_REVENUES,
     +     R_ADJ_CLAUSE_REVENUE,
     +     R_OTHER_REVENUE
      INTEGER (kind=2) ::  ATL,BTL
      CHARACTER (len=1) ::  REVENUE
      PARAMETER(ATL=1,BTL=2,REVENUE='R')

      INTEGER (kind=2) ::  CURRENT_MONTH
!
!
      INTEGER*2   BEGIN_TEST_DATE,
     +            END_TEST_DATE

      LOGICAL (kind=1) ::  TRANSACTION_FOUND
      INTEGER (kind=2) ::  HYDRO_TRANS_GROUP
      INTEGER (kind=2) ::  R_ISEAS
      LOGICAL (kind=1) ::  SERVICE_TRANS_REPORT_NOT_OPEN/.TRUE./
      INTEGER (kind=2) ::  TRANS_RPT_NO/0/,ANNUAL_SERVICE_TRANS_HEADER
      INTEGER ::  TRANS_RPT_REC
      CHARACTER (len=4) ::  LEFT_JUSTIFY_I2_IN_STR*15
      CHARACTER (len=25) :: TRANSACTION_DESCRIPTION
      character (len=7) ::  COST_ASSIGNMENT_NAME
      REAL (kind=4) ::  UNIT_ENERGY_COST,
     +       UNIT_CAPACITY_COST
      INTEGER (kind=2) ::  ALLOCATION_VECTOR
      REAL (kind=4) ::  ALLOCATION_VALUE(AVAIL_DATA_YEARS)
      
      REAL (kind=4) ::  R_FUEXP,R_PREXP,R_OPEXP,
     +       R_MNEXP,R_OTHER1,R_OTHER2,
     +       R_OTHER3,R_NFOWN,R_NFLEASE,
     +       R_ADJ_EXP,
     +       R_NF_RATEBASE,R_DSM_EXPENSE,
     +       R_DSM_REBATE,
     +       R_ADJUSTMENT_CLAUSE_REVENUES,
     +       R_BASE_RATES_REVENUES,
     +       R_SECONDARY_SALES_REVENUES,
     +       R_OTHER_REVENUES,
     +       R_BTL_REVENUES,
     +       R_BTL_EXPENSE,
     +       R_ATL_LEASE_EXP,
     +       R_BTL_LEASE_EXP,
     +       R_SERVICE_TRANSACTIONS,
     +       R_EMISSION_CREDITS,
     +       R_DOE_DISPOSAL,
     +       R_DOE_DECOMMISSIONING,
     +       R_CATAWBA_REVENUES,
     +       R_CATAWBA_EXPENSES

      LOGICAL (KIND=1) :: TRANSFER_TRANSACT_ANL_RESULTS

      LAST_SEASON = real(get_PRODUCTION_PERIODS_in(),4)

      CALL OPEN_SERVICE_TRANS_FILE(10,SERVICE_TRANSACTIONS_ACTIVE,
     +                             ns_service_decs%NUM_SERVICE_TRANS)
      IF(.NOT. SERVICE_TRANSACTIONS_ACTIVE) RETURN



! END OF DATA DECLARATIONS

C ASSET CLASS INITIALIZATION. 6/16/95. GAT.

      IF(ALLOCATED(SERVICE_ASSET_CLASS_POINTER))
     +       DEALLOCATE(SERVICE_ASSET_CLASS_POINTER,ASSET_CLASS_NUM,
     +                                              ASSET_CLASS_VECTOR)
      ALLOCATE(SERVICE_ASSET_CLASS_POINTER(1024),
     +         ASSET_CLASS_NUM(ns_service_decs%NUM_SERVICE_TRANS),
     +         ASSET_CLASS_VECTOR(ns_service_decs%NUM_SERVICE_TRANS))
      ASSET_CLASS_NUM = 0
      ASSET_CLASS_VECTOR = 0
      SERVICE_ASSET_CLASS_POINTER = 0
      NUMBER_OF_SERVICE_CLASSES = 0
      MAX_SERVICE_CLASS_ID_NUM = 0

      BEGIN_TEST_DATE = 100*(get_BASE_YEAR()+1 - 1900) + 1
      END_TEST_DATE = 100*(gc_last_study_year - 1900) + 12

      IF(ALLOCATED(ns_service_decs%SERVICE_NAME)) then
          CALL CLOSE_SERVICE_TRANSACTIONS
      end if
      ALLOCATE(ns_service_decs%SERVICE_NAME(
     + ns_service_decs%NUM_SERVICE_TRANS))

      allocate(SERVICE_ID_NUMBER(ns_service_decs%NUM_SERVICE_TRANS),
     + ns_service_decs%TYPE_OF_SERVICE(
     + ns_service_decs%NUM_SERVICE_TRANS),
     + ns_service_decs%SERVICE_COST_ASSIGNMENT(
     + ns_service_decs%NUM_SERVICE_TRANS),
     + ns_service_decs%SERVICE_EXPENSE_COLLECTION(
     + ns_service_decs%NUM_SERVICE_TRANS),
     + ns_service_decs%SERVICE_REPORTING_GROUP(
     + ns_service_decs%NUM_SERVICE_TRANS),
     + SERVICE_AVAILABLE(ns_service_decs%NUM_SERVICE_TRANS),
     + SERVICE_ENDS(ns_service_decs%NUM_SERVICE_TRANS),
     + SERVICE_UPDATE_SEASON(ns_service_decs%NUM_SERVICE_TRANS),
     + ns_service_decs%SERVICE_LINK_TYPE(
     + ns_service_decs%NUM_SERVICE_TRANS),
     + SERVICE_RESOURCE_PONTR(ns_service_decs%NUM_SERVICE_TRANS),
     + SERVICE_ANNUAL_CAPACITY(ns_service_decs%NUM_SERVICE_TRANS),
     + SERVICE_CAPACITY_PATTERN(ns_service_decs%NUM_SERVICE_TRANS),
     + SERVICE_CAPACITY_CHARGE(ns_service_decs%NUM_SERVICE_TRANS),
     + SERVICE_CAP_ESCALATION_VECTOR(ns_service_decs%NUM_SERVICE_TRANS),
     + SERVICE_ANNUAL_ENERGY(ns_service_decs%NUM_SERVICE_TRANS),
     + SERVICE_ENERGY_PATTERN(ns_service_decs%NUM_SERVICE_TRANS),
     + SERVICE_ENERGY_CHARGE(ns_service_decs%NUM_SERVICE_TRANS),
     + SERVICE_ENRG_ESCALATION_VECTOR(
     + ns_service_decs%NUM_SERVICE_TRANS),
     + SERVICE_REVENUE_CLASSIFICATION(
     + ns_service_decs%NUM_SERVICE_TRANS),
     + SERVICE_INTRA_COMPY_TRANSACTION(
     + ns_service_decs%NUM_SERVICE_TRANS),
     + ENERGY_TO_USE(ns_service_decs%NUM_SERVICE_TRANS),
     + WVPA_RATE_TRACKER(ns_service_decs%NUM_SERVICE_TRANS),
     + ns_service_decs%WVPA_RATE_TRACKER_INDEX(
     + ns_service_decs%NUM_SERVICE_TRANS),
     + ns_service_decs%WVPA_RES_TRACKER_INDEX(
     + ns_service_decs%NUM_SERVICE_TRANS),
     + ns_service_decs%WVPA_FUEL_TRACKER_INDEX(
     + ns_service_decs%NUM_SERVICE_TRANS),
     + ns_service_decs%WVPA_MEM_TRACKER_INDEX(
     + ns_service_decs%NUM_SERVICE_TRANS),
     + INTRA_ASSET_CLASS_ID(ns_service_decs%NUM_SERVICE_TRANS),
     + INTRA_ASSET_CLASS_ALLOC_VECTOR(
     + ns_service_decs%NUM_SERVICE_TRANS),
     + INTRA_ACCOUNT_CLASSIFICATION(ns_service_decs%NUM_SERVICE_TRANS),
     + INTRA_EXPENSE_COLLECTION(ns_service_decs%NUM_SERVICE_TRANS),
     + ns_service_decs%SERVICE_EXPENSE_CLASSIFICATION(
     + ns_service_decs%NUM_SERVICE_TRANS),
     + SERVICE_FUEL_CHARGE(ns_service_decs%NUM_SERVICE_TRANS),
     + SERVICE_FUEL_ESCALATION_VECTOR(
     + ns_service_decs%NUM_SERVICE_TRANS),
     + STAT=IOS)

      ns_service_decs%WVPA_RATE_TRACKER_INDEX = 0.
!
! 11/20/03. NOTE USING LF95 INITIALIZATION.
!
      ns_service_decs%WVPA_RES_TRACKER_INDEX = 0.
      ns_service_decs%WVPA_FUEL_TRACKER_INDEX = 0.
      ns_service_decs%WVPA_MEM_TRACKER_INDEX = 0.

      ns_service_decs%SERVICE_TRANS = 1


      IREC = 0
      DOWHILE (ns_service_decs%SERVICE_TRANS <= 
     + ns_service_decs%NUM_SERVICE_TRANS)
         IREC = IREC + 1
         READ(10,REC=IREC,IOSTAT=IOS) DELETE,
     +    ns_service_decs%SERVICE_NAME(ns_service_decs%SERVICE_TRANS),
     +    SERVICE_ID_NUMBER(ns_service_decs%SERVICE_TRANS),
     +   ns_service_decs%TYPE_OF_SERVICE(ns_service_decs%SERVICE_TRANS),
     +     ns_service_decs%SERVICE_COST_ASSIGNMENT(
     + ns_service_decs%SERVICE_TRANS),
     +        ns_service_decs%SERVICE_EXPENSE_COLLECTION(
     + ns_service_decs%SERVICE_TRANS),
     +         ns_service_decs%SERVICE_REPORTING_GROUP(
     + ns_service_decs%SERVICE_TRANS),
     + MONTH_SERVICE_AVAILABLE,
     + YEAR_SERVICE_AVAILABLE,
     + MONTH_SERVICE_ENDS,
     + YEAR_SERVICE_ENDS,
     + SERVICE_UPDATE_SEASON(ns_service_decs%SERVICE_TRANS),
     + ns_service_decs%SERVICE_LINK_TYPE(ns_service_decs%SERVICE_TRANS),
     + SERVICE_RESOURCE_PONTR(ns_service_decs%SERVICE_TRANS),
     + SERVICE_ANNUAL_CAPACITY(ns_service_decs%SERVICE_TRANS),
     + SERVICE_CAPACITY_PATTERN(ns_service_decs%SERVICE_TRANS),
     + SERVICE_CAPACITY_CHARGE(ns_service_decs%SERVICE_TRANS),
     +     SERVICE_CAP_ESCALATION_VECTOR(ns_service_decs%SERVICE_TRANS),
     +         SERVICE_ANNUAL_ENERGY(ns_service_decs%SERVICE_TRANS),
     +         SERVICE_ENERGY_PATTERN(ns_service_decs%SERVICE_TRANS),
     +         SERVICE_ENERGY_CHARGE(ns_service_decs%SERVICE_TRANS),
     +    SERVICE_ENRG_ESCALATION_VECTOR(ns_service_decs%SERVICE_TRANS),
     +   ASSET_CLASS_NUM(ns_service_decs%SERVICE_TRANS),
     +   ASSET_CLASS_VECTOR(ns_service_decs%SERVICE_TRANS),
     +   SERVICE_REVENUE_CLASSIFICATION(ns_service_decs%SERVICE_TRANS),
     +   SERVICE_INTRA_COMPY_TRANSACTION(ns_service_decs%SERVICE_TRANS),
     +   INTRA_ASSET_CLASS_ID(ns_service_decs%SERVICE_TRANS),
     +   INTRA_ASSET_CLASS_ALLOC_VECTOR(ns_service_decs%SERVICE_TRANS),
     +   INTRA_ACCOUNT_CLASSIFICATION(ns_service_decs%SERVICE_TRANS),
     +   INTRA_EXPENSE_COLLECTION(ns_service_decs%SERVICE_TRANS),
     +   ns_service_decs%SERVICE_EXPENSE_CLASSIFICATION(
     + ns_service_decs%SERVICE_TRANS),
     +         SERVICE_ACTIVE,
     +         ENERGY_TO_USE(ns_service_decs%SERVICE_TRANS),
     +         WVPA_RATE_TRACKER(ns_service_decs%SERVICE_TRANS),
     +         WVPA_RES_TRACKER,
     +         WVPA_FUEL_TRACKER,
     +         WVPA_MEM_TRACKER

         IF(IOS /= 0) then
            EXIT
        end if
         IF(DELETE >= 8 .OR. SERVICE_ACTIVE == 'F') then
            CYCLE
        end if

         dbgvalue= MONTH_SERVICE_AVAILABLE + 100 * 
     +     (YEAR_SERVICE_AVAILABLE - 1900)
         
         service_available(ns_service_decs%SERVICE_TRANS)=0

         SERVICE_AVAILABLE(ns_service_decs%SERVICE_TRANS) = 
     + MONTH_SERVICE_AVAILABLE + 100 * (YEAR_SERVICE_AVAILABLE - 1900)
     
      SERVICE_ENDS(ns_service_decs%SERVICE_TRANS) = MONTH_SERVICE_ENDS +
     +                                 100 * (YEAR_SERVICE_ENDS - 1900)
         IF(SERVICE_AVAILABLE(ns_service_decs%SERVICE_TRANS) > 
     + END_TEST_DATE .OR.
     + SERVICE_ENDS(ns_service_decs%SERVICE_TRANS) < 
     + BEGIN_TEST_DATE) then
            CYCLE
         end if

         CLASS_NUM = ASSET_CLASS_NUM(ns_service_decs%SERVICE_TRANS)

         CALL SET_ASSET_CLASSES(CLASS_NUM,
     +                          NUMBER_OF_SERVICE_CLASSES,
     +                           MAX_SERVICE_CLASS_ID_NUM,
     +                          SERVICE_ASSET_CLASS_POINTER)
         IF(SERVICE_INTRA_COMPY_TRANSACTION(
     + ns_service_decs%SERVICE_TRANS) == 'Y') THEN

       CLASS_NUM = INTRA_ASSET_CLASS_ID(ns_service_decs%SERVICE_TRANS)

            CALL SET_ASSET_CLASSES(CLASS_NUM,
     +                             NUMBER_OF_SERVICE_CLASSES,
     +                              MAX_SERVICE_CLASS_ID_NUM,
     +                             SERVICE_ASSET_CLASS_POINTER)
         ENDIF

         ns_service_decs%WVPA_RATE_TRACKER_INDEX(
     + ns_service_decs%SERVICE_TRANS) =
     +      FLOAT(WVPA_TRACKING_TYPE(WVPA_RATE_TRACKER(
     + ns_service_decs%SERVICE_TRANS)))
! 11/20/03. FOR REPORTING PURPOSES (NOTE: R*4)
         ns_service_decs%WVPA_RES_TRACKER_INDEX(
     + ns_service_decs%SERVICE_TRANS) =
     +             FLOAT(WVPA_RESOURCE_TRACKING_TYPE(WVPA_RES_TRACKER))
         ns_service_decs%WVPA_FUEL_TRACKER_INDEX(
     + ns_service_decs%SERVICE_TRANS) =
     +                FLOAT(WVPA_FUEL_TRACKING_TYPE(WVPA_FUEL_TRACKER))
         ns_service_decs%WVPA_MEM_TRACKER_INDEX(
     + ns_service_decs%SERVICE_TRANS) =
     +                 FLOAT(WVPA_MEM_TRACKING_TYPE(WVPA_MEM_TRACKER))

         IF(SERVICE_ENERGY_CHARGE(ns_service_decs%SERVICE_TRANS) <= 0.)
     + SERVICE_ENRG_ESCALATION_VECTOR(ns_service_decs%SERVICE_TRANS) = 0
        IF(SERVICE_CAPACITY_CHARGE(ns_service_decs%SERVICE_TRANS) <= 0.)
     +  SERVICE_CAP_ESCALATION_VECTOR(ns_service_decs%SERVICE_TRANS) = 0
       ns_service_decs%SERVICE_TRANS = ns_service_decs%SERVICE_TRANS + 1
      ENDDO
      CALL CLOSE_SERVICE_TRANS_FILE
      ns_service_decs%SERVICE_TRANS = MIN(
     + ns_service_decs%SERVICE_TRANS-1,
     + ns_service_decs%NUM_SERVICE_TRANS)
      ALLOCATE(ns_service_decs%CAPACITY_CHARGE(
     + ns_service_decs%SERVICE_TRANS),
     + ns_service_decs%ENERGY_CHARGE(ns_service_decs%SERVICE_TRANS),
     + ns_service_decs%ANNUAL_CAPACITY_CHARGE(
     + ns_service_decs%SERVICE_TRANS),
     + ns_service_decs%ANNUAL_ENERGY_CHARGE(
     + ns_service_decs%SERVICE_TRANS),
     + ns_service_decs%CAPACITY_MULTIPLIER(
     + ns_service_decs%SERVICE_TRANS),
     + ns_service_decs%ENERGY_MULTIPLIER(ns_service_decs%SERVICE_TRANS),
     + STAT=IOS)
      DO I = 1, 12
         SEASON_HOURS(I) = get_HOURS_IN_PERIOD(I)
      ENDDO

! ESTABLISH THE CURRENT PRICE
      DO I = 1, ns_service_decs%SERVICE_TRANS
         IF(SERVICE_CAPACITY_CHARGE(I) < 0.) THEN
            ns_service_decs%CAPACITY_CHARGE(I) =
     + GET_VAR(SERVICE_CAPACITY_CHARGE(I),
     +   INT2(1),ns_service_decs%SERVICE_NAME(I))

! PROB WITH USING A POINTER FOR THE COST AND HAVE THE UPDATE MONTH NOT
! BEING JANUARY.


         ELSE
         ns_service_decs%CAPACITY_CHARGE(I) = SERVICE_CAPACITY_CHARGE(I)
         ENDIF
         IF(SERVICE_ENERGY_CHARGE(I) < 0.) THEN
           ns_service_decs%ENERGY_CHARGE(I) =
     + GET_VAR(SERVICE_ENERGY_CHARGE(I),INT2(1),
     +   ns_service_decs%SERVICE_NAME(I))
         ELSE
            ns_service_decs%ENERGY_CHARGE(I) = SERVICE_ENERGY_CHARGE(I)
         ENDIF
      ENDDO

      WABASH_VALLEY_ACTIVE = UTILITY_TYPE() == 'R'
      IF(.NOT. WABASH_VALLEY_ACTIVE) THEN
         FIRST_EL_SERVICE_WRITE = .FALSE.
         FIRST_CL_SERVICE_WRITE = .FALSE.
         FIRST_CT_SERVICE_WRITE = .FALSE.
         FIRST_OT_SERVICE_WRITE = .FALSE. ! OTHER SERVICES
      ENDIF

      CALL SET_UP_ST_CLASS_ARRAYS
!
      RETURN

      ENTRY INITIALIZE_SERVICE_VARS(YR)


         TRANSMISSION_CHARGES = 0.
         WHEELING_CHARGES = 0.
         STAND_BY_TRANS_CHARGES= 0.
         DISPATCHING_CHARGES = 0.
         OTHER_SERVICE_CHARGES = 0.
         SERVICE_REVENUES = 0.
         SERVICE_EXPENSES = 0.
         SERVICE_ADJ_CLAUSE_EXPENSES = 0.
         SERVICE_BASE_RATE_EXPENSES = 0.
         SUM_OF_SERVICE(0) = 0.
         DO CLASS = 1, MAX_SERVICE_GROUPS
            ANNUAL_COINCIDENT_PEAK(CLASS) = 0.
            ANNUAL_NONCOINCIDENT_PEAK(CLASS) = 0.
            SUM_OF_SERVICE(CLASS) = 0.
            DO I = 0, MAX_SERVICE_ITEMS
               SERVICE_GROUP_COSTS(I,CLASS) = 0.
            ENDDO
            DO I = 1, 12
              CLASS_FORECAST = MAX(FORECAST_COINCIDENT_PEAK(1,I,CLASS),
     +                           FORECAST_COINCIDENT_PEAK(2,I,CLASS))
               ANNUAL_COINCIDENT_PEAK(CLASS) = MAX(CLASS_FORECAST *
     +                                        CLASS_COIN_FACTOR(CLASS),
     +                                ANNUAL_NONCOINCIDENT_PEAK(CLASS))
               ANNUAL_NONCOINCIDENT_PEAK(CLASS) = MAX(CLASS_FORECAST,
     +                                ANNUAL_NONCOINCIDENT_PEAK(CLASS))
            ENDDO
         ENDDO
         DO I = 1, ns_service_decs%SERVICE_TRANS
            ns_service_decs%ANNUAL_CAPACITY_CHARGE(I) = 0.
            ns_service_decs%ANNUAL_ENERGY_CHARGE(I) = 0.
         ENDDO
      RETURN ! INITIALIZE_SERVICE_VARS
! UPDATE SERVICE CHARGES AND CAPACITY AND ENERGY PATTERN DISTRIBUTIONS
      ENTRY SERVICE_UPDATE_COSTS(ISEAS,YR)

      DO I = 1, ns_service_decs%SERVICE_TRANS
         IF(SERVICE_CAPACITY_PATTERN(I) < 0.) THEN
            ns_service_decs%CAPACITY_MULTIPLIER(I)=
     + GET_VAR(SERVICE_CAPACITY_PATTERN(I),
     +          ISEAS,ns_service_decs%SERVICE_NAME(I))
         ELSEIF(SERVICE_CAPACITY_PATTERN(I) > 0.) THEN
            ns_service_decs%CAPACITY_MULTIPLIER(I) =
     + SERVICE_CAPACITY_PATTERN(I)
         ELSE
            ns_service_decs%CAPACITY_MULTIPLIER(I) = 1.
         ENDIF
         IF(SERVICE_ENERGY_PATTERN(I) < 0.) THEN
            ns_service_decs%ENERGY_MULTIPLIER(I) =
     + GET_VAR(SERVICE_ENERGY_PATTERN(I),
     +       ISEAS,ns_service_decs%SERVICE_NAME(I))
         ELSEIF(SERVICE_ENERGY_PATTERN(I) > 0.) THEN
            ns_service_decs%ENERGY_MULTIPLIER(I) =
     + SERVICE_ENERGY_PATTERN(I)
         ELSE
            ns_service_decs%ENERGY_MULTIPLIER(I) = 1.
         ENDIF

! 10/21/98. GAT. RE-WROTE FOR WVPA

         IF(ISEAS == SERVICE_UPDATE_SEASON(I) .OR.
     +                                      YR > AVAIL_DATA_YEARS) THEN
            IF(SERVICE_CAPACITY_CHARGE(I) < 0.) THEN

               ns_service_decs%CAPACITY_CHARGE(I) =

     +                           GET_VAR(SERVICE_CAPACITY_CHARGE(I),YR,
     + ns_service_decs%SERVICE_NAME(I))
            ELSE
         ns_service_decs%CAPACITY_CHARGE(I) = SERVICE_CAPACITY_CHARGE(I)
            ENDIF
            IF(SERVICE_ENERGY_CHARGE(I) < 0.) THEN
       ns_service_decs%ENERGY_CHARGE(I) =
     + GET_VAR(SERVICE_ENERGY_CHARGE(I),YR,
     +                          ns_service_decs%SERVICE_NAME(I))
            ELSE
           ns_service_decs%ENERGY_CHARGE(I) = SERVICE_ENERGY_CHARGE(I)
            ENDIF
         ENDIF

         ns_service_decs%CAPACITY_CHARGE(I) =

     +               ESCALATED_MONTHLY_VALUE(
     +                              ns_service_decs%CAPACITY_CHARGE(I),
     +                              SERVICE_CAP_ESCALATION_VECTOR(I),
     +                              YR,ISEAS,SERVICE_UPDATE_SEASON(I))


         ns_service_decs%ENERGY_CHARGE(I) =
     +               ESCALATED_MONTHLY_VALUE(
     +                              ns_service_decs%ENERGY_CHARGE(I),
     +                              SERVICE_ENRG_ESCALATION_VECTOR(I),
     +                              YR,ISEAS,SERVICE_UPDATE_SEASON(I))

      ENDDO
      RETURN


! ENERGY LIMITED RESOURCE CALCULATIONS


      ENTRY EL_SERVICE_TRANS_CALCULATIONS(nr_NUNITS,ENERGY,
     +                                    CAPACITY,
     +                                    DATE1,DATE2,ISEAS)

         DO I = 1, ns_service_decs%SERVICE_TRANS
           IF(INDEX(ns_service_decs%SERVICE_LINK_TYPE(I),'EL') /= 0 .OR.
     +    trim(ns_service_decs%SERVICE_LINK_TYPE(I)) == 'TG') THEN
               IF(FIRST_EL_SERVICE_WRITE .AND.
     +                          MONTHLY_SERVICE_REPORT_ACTIVE) THEN
C TAKE ALL EL SERVICES FOR FIRST WRITE
               ELSEIF(  .NOT. WABASH_VALLEY_ACTIVE .AND.
     +                  (SERVICE_AVAILABLE(I) > DATE2 .OR.
     +                                  SERVICE_ENDS(I) < DATE1)) THEN
                  CYCLE
               ENDIF
               DO L = 1, nr_NUNITS
         IF((trim(ns_service_decs%SERVICE_LINK_TYPE(I)) == 'EL' .AND.
     +              INT(EL_RESOURCE_ID(L)) ==
     +                                  SERVICE_RESOURCE_PONTR(I)) .OR.
     +  (INDEX(ns_service_decs%SERVICE_LINK_TYPE(I),'TG') /= 0 .AND.
     +            HYDRO_TRANS_GROUP(L)==SERVICE_RESOURCE_PONTR(I)))THEN

                     IF(SERVICE_AVAILABLE(I) > DATE2 .OR.
     +                                    SERVICE_ENDS(I) < DATE1) THEN
                        ns_service_decs%SEASONAL_ENERGY = 0.
                        ns_service_decs%SEASONAL_CAPACITY = 0.
                        ns_service_decs%SEASONAL_ENERGY_CHARGE = 0.
                        ns_service_decs%SEASONAL_CAPACITY_CHARGE = 0.
                ELSE
        ns_service_decs%SEASONAL_ENERGY = ENERGY(L)*
     + ns_service_decs%ENERGY_MULTIPLIER(I)
                   ns_service_decs%SEASONAL_CAPACITY = CAPACITY(L) *
     +                         ns_service_decs%CAPACITY_MULTIPLIER(I)
       ns_service_decs%SEASONAL_ENERGY_CHARGE = 
     + ns_service_decs%ENERGY_CHARGE(I) *
     +                       ns_service_decs%SEASONAL_ENERGY
         ns_service_decs%SEASONAL_CAPACITY_CHARGE = 
     + ns_service_decs%CAPACITY_CHARGE(I) *
     +                            ns_service_decs%SEASONAL_CAPACITY
                     ENDIF

      IF(ns_service_decs%TYPE_OF_SERVICE(I) == 'T') THEN



        CALL TRANSACTION_MANAGER(int(ISEAS,2),
     + I,
     +     ns_service_decs%SERVICE_COST_ASSIGNMENT(I),
     +     ns_service_decs%TYPE_OF_SERVICE(I),
     +     ns_service_decs%SERVICE_NAME(I),
     +     ns_service_decs%SEASONAL_ENERGY,
     +     ns_service_decs%SEASONAL_ENERGY_CHARGE/1000.,
     +     ns_service_decs%ENERGY_CHARGE(I),
     +     ns_service_decs%SEASONAL_CAPACITY,
     +     ns_service_decs%SEASONAL_CAPACITY_CHARGE,
     +     ns_service_decs%CAPACITY_CHARGE(I),
     +     ns_service_decs%SERVICE_EXPENSE_COLLECTION(I),
     +     ns_service_decs%SERVICE_REPORTING_GROUP(I),
     +     MONTHLY_SERVICE_REPORT_ACTIVE,
     +     LOGICAL1_TRUE,
     +     ns_service_decs%WVPA_RATE_TRACKER_INDEX(I),
     +     ns_service_decs%WVPA_RES_TRACKER_INDEX(I),
     +     ns_service_decs%WVPA_FUEL_TRACKER_INDEX(I),
     +     ns_service_decs%WVPA_MEM_TRACKER_INDEX(I),
     +   ns_service_decs%SERVICE_EXPENSE_CLASSIFICATION(I))

      ENDIF


                CALL STORE_ST_COST_AND_REVENUE_DATA(I,
     +                  ns_service_decs%SEASONAL_ENERGY_CHARGE/1000.,
     +                   ns_service_decs%SEASONAL_CAPACITY_CHARGE,
     +                   ns_service_decs%SEASONAL_ENERGY,
     +       ns_service_decs%SEASONAL_CAPACITY)
                 ENDIF
               ENDDO ! EL UNITS COUNTER
            ENDIF ! YES EL UNIT
         ENDDO ! EL SERVICES COUNTER
         IF(FIRST_EL_SERVICE_WRITE) then
            FIRST_EL_SERVICE_WRITE = .FALSE.
         endif
      RETURN

! CAPACITY LIMITED RESOURCE CALCULATIONS




      ENTRY CT_SERVICE_TRANS_CALCULATIONS(nunits_loc,ENERGY,
     +                                    CAPACITY,
     +                                    DATE1,DATE2,ISEAS)

      DO I = 1, ns_service_decs%SERVICE_TRANS
         IF(INDEX(ns_service_decs%SERVICE_LINK_TYPE(I),'CT')/= 0) THEN

            IF(FIRST_CT_SERVICE_WRITE .AND.
     +                              MONTHLY_SERVICE_REPORT_ACTIVE) THEN
! TAKE ALL CT SERVICES FOR FIRST WRITE
            ELSEIF(  .NOT. WABASH_VALLEY_ACTIVE .AND.
     +                  (SERVICE_AVAILABLE(I) > DATE2 .OR.
     +                                   SERVICE_ENDS(I) < DATE1)) THEN
               CYCLE
            ENDIF

            ! Avoid spurious Lahey warning
            res_id_loc=res_id_loc

            DO L = 1, nr_NUNITS
               IF(res_id_loc(L) ==
     +                          SERVICE_RESOURCE_PONTR(I)) THEN
                  IF(SERVICE_AVAILABLE(I) > DATE2 .OR.
     +                                    SERVICE_ENDS(I) < DATE1) THEN
                     ns_service_decs%SEASONAL_ENERGY = 0.
                     ns_service_decs%SEASONAL_CAPACITY = 0.
                     ns_service_decs%SEASONAL_ENERGY_CHARGE = 0.
                     ns_service_decs%SEASONAL_CAPACITY_CHARGE = 0.
                  ELSE
       ns_service_decs%SEASONAL_ENERGY =ENERGY(L) *
     + ns_service_decs%ENERGY_MULTIPLIER(I) *
     +                                 SEASON_HOURS(ISEAS)
        ns_service_decs%SEASONAL_CAPACITY = CAPACITY(L) *
     +                        ns_service_decs%CAPACITY_MULTIPLIER(I)
            ns_service_decs%SEASONAL_ENERGY_CHARGE = 
     + ns_service_decs%ENERGY_CHARGE(I) *
     +                               ns_service_decs%SEASONAL_ENERGY
         ns_service_decs%SEASONAL_CAPACITY_CHARGE = 
     + ns_service_decs%CAPACITY_CHARGE(I) *
     +                               ns_service_decs%SEASONAL_CAPACITY
                  ENDIF



                  IF(ns_service_decs%TYPE_OF_SERVICE(I) == 'T') THEN
                     CALL TRANSACTION_MANAGER(ISEAS,I,
     +        ns_service_decs%SERVICE_COST_ASSIGNMENT(I),
     +        ns_service_decs%TYPE_OF_SERVICE(I),
     +        ns_service_decs%SERVICE_NAME(I),
     +        ns_service_decs%SEASONAL_ENERGY,
     +        ns_service_decs%SEASONAL_ENERGY_CHARGE/1000.,
     +        ns_service_decs%ENERGY_CHARGE(I),
     +        ns_service_decs%SEASONAL_CAPACITY,
     +        ns_service_decs%SEASONAL_CAPACITY_CHARGE,
     +        ns_service_decs%CAPACITY_CHARGE(I),
     +       ns_service_decs%SERVICE_EXPENSE_COLLECTION(I),
     +        ns_service_decs%SERVICE_REPORTING_GROUP(I),
     +       MONTHLY_SERVICE_REPORT_ACTIVE,
     +        LOGICAL1_TRUE,
     +        ns_service_decs%WVPA_RATE_TRACKER_INDEX(I),
     +        ns_service_decs%WVPA_RES_TRACKER_INDEX(I),
     +        ns_service_decs%WVPA_FUEL_TRACKER_INDEX(I),
     +        ns_service_decs%WVPA_MEM_TRACKER_INDEX(I),
     +   ns_service_decs%SERVICE_EXPENSE_CLASSIFICATION(I))
                  ENDIF
                  CALL STORE_ST_COST_AND_REVENUE_DATA(I,
     +               ns_service_decs%SEASONAL_ENERGY_CHARGE/1000.,
     +                ns_service_decs%SEASONAL_CAPACITY_CHARGE,
     +                ns_service_decs%SEASONAL_ENERGY,
     +   ns_service_decs%SEASONAL_CAPACITY)
               ENDIF
            ENDDO ! CONTRACTS COUNTER
         ENDIF ! YES CONTRACT
      ENDDO ! TRANSACTIONS COUNTER
      IF(FIRST_CT_SERVICE_WRITE) FIRST_CT_SERVICE_WRITE = .FALSE.
      RETURN


! CALCULATE MONTHLY SERVICE COSTS FOR NON RESOURCE SPECIFIC REFERENCES


      ENTRY SERVICE_TRANS_CALCULATIONS(ISEAS,YR,DATE1,DATE2)


      DO I = 1, ns_service_decs%SERVICE_TRANS

C CAPACITY COSTING

         ! ONLY STORE VALUES IF TRANSACTION TYPE FOUND
         TRANSACTION_FOUND = .FALSE.
         ! ONLY WRITE NON-SPECIFIC TRANSACTIONS
         ns_service_decs%WRITE_IT_MONTHLY = .FALSE.
         IF(FIRST_OT_SERVICE_WRITE .AND.
     +                              MONTHLY_SERVICE_REPORT_ACTIVE) THEN
! TAKE ALL OT SERVICES FOR FIRST WRITE
         ELSEIF(  .NOT. WABASH_VALLEY_ACTIVE .AND.
     +                  (SERVICE_AVAILABLE(I) > DATE2 .OR.
     +                                   SERVICE_ENDS(I) < DATE1)) THEN
            CYCLE
         ENDIF

         ns_service_decs%SEASONAL_CAPACITY_CHARGE = 0.
         ns_service_decs%SEASONAL_ENERGY_CHARGE = 0.
         ns_service_decs%SEASONAL_ENERGY = 0.
         ns_service_decs%SEASONAL_CAPACITY = 0.
         SELECT CASE (trim(ns_service_decs%SERVICE_LINK_TYPE(I)))

         CASE ('CG')

            IF(SERVICE_AVAILABLE(I) > DATE2 .OR.
     +                                    SERVICE_ENDS(I) < DATE1) THEN
               ns_service_decs%SEASONAL_ENERGY = 0.
               ns_service_decs%SEASONAL_CAPACITY = 0.
            ELSE
               ns_service_decs%SEASONAL_ENERGY = 
     + ns_service_decs%ENERGY_MULTIPLIER(I) *
     +           GET_CUST_GROUP_ENERGY(SERVICE_RESOURCE_PONTR(I),ISEAS)

            ns_service_decs%SEASONAL_CAPACITY =
     + ns_service_decs%CAPACITY_MULTIPLIER(I) *
     +             GET_CUST_GROUP_PEAK(SERVICE_RESOURCE_PONTR(I),ISEAS)

            ENDIF

         ns_service_decs%SEASONAL_CAPACITY_CHARGE = 
     + ns_service_decs%CAPACITY_CHARGE(I) *
     +                                ns_service_decs%SEASONAL_CAPACITY
           ns_service_decs%SEASONAL_ENERGY_CHARGE = 
     + ns_service_decs%ENERGY_CHARGE(I) *
     + ns_service_decs%SEASONAL_ENERGY
            ns_service_decs%WRITE_IT_MONTHLY = .TRUE.
            TRANSACTION_FOUND = .TRUE.
         CASE ('D1','D2','D3','D4','D5','D6')
            CLASS_CHR = ns_service_decs%SERVICE_LINK_TYPE(I)(2:2)
            CLASS = INDEX('123456',CLASS_CHR)
            CALL DSM_PEAK_ENER_RESERVE_ALLOC(PEAK_RESERVE,
     +                                        ENER_RESERVE,CLASS,ISEAS)
C
            IF(SERVICE_AVAILABLE(I) > DATE2 .OR.
     +                                    SERVICE_ENDS(I) < DATE1) THEN
               ns_service_decs%SEASONAL_ENERGY = 0.
               ns_service_decs%SEASONAL_CAPACITY = 0.
            ELSE
               ns_service_decs%SEASONAL_ENERGY = 
     + ns_service_decs%ENERGY_MULTIPLIER(I) *
     +                               (FORECAST_ENERGY(1,ISEAS,CLASS) +
     +                                 FORECAST_ENERGY(2,ISEAS,CLASS) +
     +                                                    ENER_RESERVE)
            ns_service_decs%SEASONAL_CAPACITY = 
     + ns_service_decs%CAPACITY_MULTIPLIER(I) *
     +                   (MAX(FORECAST_COINCIDENT_PEAK(1,ISEAS,CLASS),
     +                       FORECAST_COINCIDENT_PEAK(2,ISEAS,CLASS)) +
     +                                                    PEAK_RESERVE)
            ENDIF

            ns_service_decs%SEASONAL_ENERGY_CHARGE = 
     + ns_service_decs%ENERGY_CHARGE(I)
     +  * ns_service_decs%SEASONAL_ENERGY
         ns_service_decs%SEASONAL_CAPACITY_CHARGE = 
     + ns_service_decs%CAPACITY_CHARGE(I) *
     +                                ns_service_decs%SEASONAL_CAPACITY
            ns_service_decs%WRITE_IT_MONTHLY = .TRUE.
            TRANSACTION_FOUND = .TRUE.
         CASE ('C1','C2','C3','C4','C5','C6',
     +                                   'P1','P2','P3','P4','P5','P6')
            IF( .NOT. WABASH_VALLEY_ACTIVE .AND.
     +                  (SERVICE_AVAILABLE(I) > DATE2 .OR.
     +                                  SERVICE_ENDS(I) < DATE1)) CYCLE
            CLASS_CHR = ns_service_decs%SERVICE_LINK_TYPE(I)(2:2)
            CLASS = INDEX('123456',CLASS_CHR)
            CLASS_CHR = ns_service_decs%SERVICE_LINK_TYPE(I)(1:1)
            IF(CLASS_CHR == 'C') THEN
        ns_service_decs%SEASONAL_CAPACITY_CHARGE = 
     + ns_service_decs%CAPACITY_CHARGE(I) *
     +                        ns_service_decs%CAPACITY_MULTIPLIER(I) *
     +                   MAX(FORECAST_COINCIDENT_PEAK(1,ISEAS,CLASS),
     +                       FORECAST_COINCIDENT_PEAK(2,ISEAS,CLASS))
            ELSE
               ns_service_decs%ANNUAL_CAPACITY_CHARGE(I) = 
     + ns_service_decs%ANNUAL_CAPACITY_CHARGE(I) +
     +                         ns_service_decs%CAPACITY_MULTIPLIER(I) *
     +                              ns_service_decs%CAPACITY_CHARGE(I)
            ENDIF
            ns_service_decs%SEASONAL_ENERGY_CHARGE = 
     + ns_service_decs%ENERGY_CHARGE(I) *
     +                        ns_service_decs%ENERGY_MULTIPLIER(I) *
     +                              (FORECAST_ENERGY(1,ISEAS,CLASS) +
     +                                FORECAST_ENERGY(2,ISEAS,CLASS))
            TRANSACTION_FOUND = .TRUE.
         CASE ('SF')

            IF(SERVICE_AVAILABLE(I) > DATE2 .OR.
     +                                    SERVICE_ENDS(I) < DATE1) THEN
               ns_service_decs%SEASONAL_ENERGY = 0.
               ns_service_decs%SEASONAL_CAPACITY = 0.
            ELSE
               ns_service_decs%SEASONAL_ENERGY = 
     + ns_service_decs%ENERGY_MULTIPLIER(I) *
     +                                      SEASON_SYSTEM_ENERGY(ISEAS)
            ns_service_decs%SEASONAL_CAPACITY = 
     + ns_service_decs%CAPACITY_MULTIPLIER(I) *
     +                                       SEASON_SYSTEM_PEAK(ISEAS)
            ENDIF


         ns_service_decs%SEASONAL_CAPACITY_CHARGE = 
     + ns_service_decs%CAPACITY_CHARGE(I) *
     +       ns_service_decs%SEASONAL_CAPACITY
            ns_service_decs%SEASONAL_ENERGY_CHARGE = 
     + ns_service_decs%ENERGY_CHARGE(I)
     + * ns_service_decs%SEASONAL_ENERGY
            ns_service_decs%WRITE_IT_MONTHLY = .TRUE.

            TRANSACTION_FOUND = .TRUE.
         CASE ('PA')
            IF(SERVICE_AVAILABLE(I) > DATE2 .OR.
     +                                   SERVICE_ENDS(I) < DATE1) CYCLE
            ns_service_decs%ANNUAL_CAPACITY_CHARGE(I) = 
     + ns_service_decs%ANNUAL_CAPACITY_CHARGE(I) +
     +                         ns_service_decs%CAPACITY_MULTIPLIER(I) *
     +                            ns_service_decs%CAPACITY_CHARGE(I)
            ns_service_decs%SEASONAL_ENERGY_CHARGE = 
     + ns_service_decs%ENERGY_CHARGE(I) *
     +                   ns_service_decs%ENERGY_MULTIPLIER(I) *
     +                                    SEASON_SYSTEM_ENERGY(ISEAS)


        CASE ('None','N','No')
            IF(SERVICE_ANNUAL_CAPACITY(I) < 0.) THEN
                  ANNUAL_CAPACITY = GET_VAR(SERVICE_ANNUAL_CAPACITY(I),
     +                           YR,ns_service_decs%SERVICE_NAME(I))
            ELSE
               ANNUAL_CAPACITY = SERVICE_ANNUAL_CAPACITY(I)
            ENDIF
            IF(SERVICE_ANNUAL_ENERGY(I) < 0.) THEN
               ANNUAL_ENERGY = GET_VAR(SERVICE_ANNUAL_ENERGY(I),YR,
     +        ns_service_decs%SERVICE_NAME(I))
            ELSE
               ANNUAL_ENERGY = SERVICE_ANNUAL_ENERGY(I)
            ENDIF

!
            IF(SERVICE_AVAILABLE(I) > DATE2 .OR.
     +                                    SERVICE_ENDS(I) < DATE1) THEN
               ns_service_decs%SEASONAL_ENERGY = 0.
               ns_service_decs%SEASONAL_CAPACITY = 0.
            ELSE
               ns_service_decs%SEASONAL_ENERGY =ANNUAL_ENERGY * 
     + ns_service_decs%ENERGY_MULTIPLIER(I)/12.
               ns_service_decs%SEASONAL_CAPACITY=
     + ANNUAL_CAPACITY*ns_service_decs%CAPACITY_MULTIPLIER(I)
            ENDIF

            ns_service_decs%SEASONAL_ENERGY_CHARGE = 
     + ns_service_decs%ENERGY_CHARGE(I)
     + * ns_service_decs%SEASONAL_ENERGY
         ns_service_decs%SEASONAL_CAPACITY_CHARGE = 
     + ns_service_decs%CAPACITY_CHARGE(I) *
     +                               ns_service_decs%SEASONAL_CAPACITY
            ns_service_decs%WRITE_IT_MONTHLY = .TRUE.
            TRANSACTION_FOUND = .TRUE.
         END SELECT

! force in WVPA actual values

         RPT_ENERGY_CHARGE = ns_service_decs%ENERGY_CHARGE(I)
         RPT_CAPACITY_CHARGE = ns_service_decs%CAPACITY_CHARGE(I)


c force in WVPA actual values


         

         IF(ns_service_decs%TYPE_OF_SERVICE(I) == 'T') THEN
            CALL TRANSACTION_MANAGER(ISEAS,I,
     +   ns_service_decs%SERVICE_COST_ASSIGNMENT(I),
     +   ns_service_decs%TYPE_OF_SERVICE(I),
     +   ns_service_decs%SERVICE_NAME(I),
     +   ns_service_decs%SEASONAL_ENERGY,
     +   real(ns_service_decs%SEASONAL_ENERGY_CHARGE/1000.,4),
     +   ns_service_decs%ENERGY_CHARGE(I),
     +   ns_service_decs%SEASONAL_CAPACITY,
     +   ns_service_decs%SEASONAL_CAPACITY_CHARGE,
     +   ns_service_decs%CAPACITY_CHARGE(I),
     +   ns_service_decs%SERVICE_EXPENSE_COLLECTION(I),
     +   ns_service_decs%SERVICE_REPORTING_GROUP(I),
     +   ns_service_decs%MONTHLY_SERVICE_REPORT_ACTIVE,
     +   ns_service_decs%WRITE_IT_MONTHLY,
     +   ns_service_decs%WVPA_RATE_TRACKER_INDEX(I),
     +   ns_service_decs%WVPA_RES_TRACKER_INDEX(I),
     +   ns_service_decs%WVPA_FUEL_TRACKER_INDEX(I),
     +   ns_service_decs%WVPA_MEM_TRACKER_INDEX(I),
     +   ns_service_decs%SERVICE_EXPENSE_CLASSIFICATION(I))
         ENDIF
         IF(TRANSACTION_FOUND) THEN
            CALL STORE_ST_COST_AND_REVENUE_DATA(I,
     +                    ns_service_decs%SEASONAL_ENERGY_CHARGE/1000.,
     +     ns_service_decs%SEASONAL_CAPACITY_CHARGE,
     +              ns_service_decs%SEASONAL_ENERGY,
     +         ns_service_decs%SEASONAL_CAPACITY)
         ENDIF
      ENDDO ! TRANSACTIONS COUNTER
      IF(FIRST_OT_SERVICE_WRITE) FIRST_OT_SERVICE_WRITE = .FALSE.
      RETURN

      ENTRY GET_SERVICE_TRANS_TRACKERS(R_TRANS_NO,
     +                                 R_RES_TRACKER,
     +                                 R_FUEL_TRACKER,
     +                                 R_RATE_TRACKER,
     +                                 R_MEM_TRACKER)

         R_RES_TRACKER =
     + ns_service_decs%WVPA_RES_TRACKER_INDEX(R_TRANS_NO)
         R_FUEL_TRACKER = 
     + ns_service_decs%WVPA_FUEL_TRACKER_INDEX(R_TRANS_NO)
         R_MEM_TRACKER = 
     + ns_service_decs%WVPA_MEM_TRACKER_INDEX(R_TRANS_NO)
      RETURN

! CALCULATE ANNUAL PEAK COSTS AND NON-LINKED COSTS


      ENTRY ANNUAL_SERVICE_TRANSACTIONS(YR,ANNUAL_PEAK,DATE1,DATE2)


! CALCULATE THE ANNUAL SERVICE TRANSACTION COSTS

      DO I = 1, ns_service_decs%SERVICE_TRANS
        IF(SERVICE_AVAILABLE(I)>DATE2 .OR. SERVICE_ENDS(I)<DATE1) CYCLE
         SELECT CASE (ns_service_decs%SERVICE_LINK_TYPE(I))
         CASE ('PA','P1','P2','P3','P4','P5','P6')
            IF(ns_service_decs%SERVICE_LINK_TYPE(I)(1:2) == 'PA') THEN
               ns_service_decs%SEASONAL_CAPACITY_CHARGE = 
     + ns_service_decs%ANNUAL_CAPACITY_CHARGE(I) *  ANNUAL_PEAK
               ANNUAL_ENERGY = 0.
         ELSEIF(ns_service_decs%SERVICE_LINK_TYPE(I)(1:1) == 'P') THEN
               CLASS_CHR = ns_service_decs%SERVICE_LINK_TYPE(I)(2:2)
               CLASS = INDEX('123456',CLASS_CHR)
               ns_service_decs%SEASONAL_CAPACITY_CHARGE = 
     + ns_service_decs%ANNUAL_CAPACITY_CHARGE(I) *
     +               ANNUAL_COINCIDENT_PEAK(CLASS)
               ANNUAL_ENERGY = 0.
            ENDIF

            IF(ns_service_decs%TYPE_OF_SERVICE(I) == 'T') THEN
               MO_sd = 0
               CALL TRANSACTION_MANAGER(MO_sd,I,
     +      ns_service_decs%SERVICE_COST_ASSIGNMENT(I),
     +      ns_service_decs%TYPE_OF_SERVICE(I),
     +      ns_service_decs%SERVICE_NAME(I),
     +      ns_service_decs%SEASONAL_ENERGY,
     +      ns_service_decs%SEASONAL_ENERGY_CHARGE/1000.,
     +      ns_service_decs%ENERGY_CHARGE(I),
     +      ns_service_decs%SEASONAL_CAPACITY,
     +      ns_service_decs%SEASONAL_CAPACITY_CHARGE,
     +      ns_service_decs%CAPACITY_CHARGE(I),
     +      ns_service_decs%SERVICE_EXPENSE_COLLECTION(I),
     +      ns_service_decs%SERVICE_REPORTING_GROUP(I),
     +      MONTHLY_SERVICE_REPORT_ACTIVE,
     +      LOGICAL1_TRUE,
     +      ns_service_decs%WVPA_RATE_TRACKER_INDEX(I),
     +      ns_service_decs%WVPA_RES_TRACKER_INDEX(I),
     +      ns_service_decs%WVPA_FUEL_TRACKER_INDEX(I),
     +      ns_service_decs%WVPA_MEM_TRACKER_INDEX(I),
     +   ns_service_decs%SERVICE_EXPENSE_CLASSIFICATION(I))
            ENDIF
            CALL STORE_ANNUAL_ST_COST_DATA(I,
     +               ns_service_decs%SEASONAL_ENERGY_CHARGE/1000.,
     + ns_service_decs%SEASONAL_CAPACITY_CHARGE,
     +       ns_service_decs%SEASONAL_ENERGY,
     +    ns_service_decs%SEASONAL_CAPACITY)
         END SELECT
      ENDDO
      RETURN


! END POINT FINISHED DEALLOCATE ARRAYS


      ENTRY CLOSE_SERVICE_TRANSACTIONS

      DEALLOCATE(ns_service_decs%CAPACITY_CHARGE,
     +           ns_service_decs%ENERGY_CHARGE,
     +           ns_service_decs%ANNUAL_CAPACITY_CHARGE,
     +           ns_service_decs%ANNUAL_ENERGY_CHARGE,
     +           ns_service_decs%CAPACITY_MULTIPLIER,
     +           ns_service_decs%ENERGY_MULTIPLIER,
     +           STAT=IOS)
      DEALLOCATE(ns_service_decs%SERVICE_NAME,
     +           SERVICE_ID_NUMBER,
     +           ns_service_decs%TYPE_OF_SERVICE,
     +           ns_service_decs%SERVICE_COST_ASSIGNMENT,
     +           ns_service_decs%SERVICE_EXPENSE_COLLECTION,
     +           ns_service_decs%SERVICE_REPORTING_GROUP,
C
     +           SERVICE_AVAILABLE,
     +           SERVICE_ENDS,

     +           SERVICE_UPDATE_SEASON,
     +           SERVICE_ANNUAL_CAPACITY,
     +           ns_service_decs%SERVICE_LINK_TYPE,
     +           SERVICE_RESOURCE_PONTR,
     +           SERVICE_CAPACITY_PATTERN,
     +           SERVICE_CAPACITY_CHARGE,
     +           SERVICE_CAP_ESCALATION_VECTOR,
     +           SERVICE_ANNUAL_ENERGY,
     +           SERVICE_ENERGY_PATTERN,
     +           SERVICE_ENERGY_CHARGE,
     +           SERVICE_ENRG_ESCALATION_VECTOR,
     +           SERVICE_REVENUE_CLASSIFICATION,
     +           SERVICE_INTRA_COMPY_TRANSACTION,
     +           ENERGY_TO_USE,
     +           WVPA_RATE_TRACKER,
     +           ns_service_decs%WVPA_RATE_TRACKER_INDEX,
     +           ns_service_decs%WVPA_RES_TRACKER_INDEX,
     +           ns_service_decs%WVPA_FUEL_TRACKER_INDEX,
     +           ns_service_decs%WVPA_MEM_TRACKER_INDEX,
     +           INTRA_ASSET_CLASS_ID,
     +           INTRA_ASSET_CLASS_ALLOC_VECTOR,
     +           INTRA_ACCOUNT_CLASSIFICATION,
     +           INTRA_EXPENSE_COLLECTION,
     +           ns_service_decs%SERVICE_EXPENSE_CLASSIFICATION,
     +           SERVICE_FUEL_CHARGE,
     +           SERVICE_FUEL_ESCALATION_VECTOR,
     +           STAT=IOS)
      RETURN

! ANNUAL SERVICE COST REPORT
      ENTRY SERVICE_COST_ANNUAL_REPORT(KPCVAL)


      WRITE(9,'(//38X,A/)')
     +                'Transmission, Wheeling, Dispatching, etc. Costs'
      IF(kpcval) THEN
         WRITE(9,1100) '   Service   ',
     +                 '   Total',
     +                 '     WPE',
     +                 '    KP&L',
     +                 '    KG&E',
     +                 '   KCP&L',
     +                 '     EDE',
     +                 '     SFE'
         GROUPS = 6
      ELSE
         WRITE(9,1100) '   Service   ',
     +                 '   Total',
     +                 ' Group 1',
     +                 ' Group 2',
     +                 ' Group 3',
     +                 ' Group 4',
     +                 ' Group 5',
     +                 ' Group 6'
         GROUPS = MAX_SERVICE_GROUPS
      ENDIF
      SERVICE_TITLE(1) = 'Transmission'
      SERVICE_TITLE(2) = 'Stand-by Transmission'
      SERVICE_TITLE(3) = 'Wheeling'
      SERVICE_TITLE(4) = 'Dispatching'
      SERVICE_TITLE(5) = 'Other'
      DO I = 1, MAX_SERVICE_ITEMS
         TOTAL = 0.
         DO CLASS = 1, MAX_SERVICE_GROUPS
            TOTAL = TOTAL + SERVICE_GROUP_COSTS(I,CLASS)
           SERVICE_GROUP_COSTS(I,CLASS) = SERVICE_GROUP_COSTS(I,CLASS)/
     +                                                1000.
            SUM_OF_SERVICE(CLASS) = SUM_OF_SERVICE(CLASS) +
     +                                     SERVICE_GROUP_COSTS(I,CLASS)
         ENDDO
         TOTAL = TOTAL/1000.
         SUM_OF_SERVICE(0) = SUM_OF_SERVICE(0) + TOTAL
         WRITE(9,1000) SERVICE_TITLE(I),TOTAL,
     +                 (SERVICE_GROUP_COSTS(I,CLASS),CLASS=1,GROUPS)
      ENDDO
      WRITE(9,1000) '      Total of Services',
     +                           (SUM_OF_SERVICE(CLASS),CLASS=0,GROUPS)
      RETURN
 1000 FORMAT(1X,A24,10F8.2)
 1100 FORMAT(1X,A20,T26,10(A))

     



      ENTRY STORE_ST_COST_AND_REVENUE_DATA(UNIT_NO,
     +                                     VAR_COST,
     +                                     ST_FIXED_COST,
     +                                     TRANS_ENERGY,
     +                                     TRANS_CAPACITY)

         ST_SO2_EMIS = 0.
         IF(.NOT. ALLOCATED(ANNUAL_ST_ENERGY)) THEN
            CALL GET_NUMBER_OF_SERVICES (NUMBER_OF_SERVICES)
            ALLOCATE(ANNUAL_ST_ENERGY(NUMBER_OF_SERVICES),
     +               ANNUAL_ST_CAPACITY(NUMBER_OF_SERVICES),
     +               ANNUAL_ST_VAR_COST(NUMBER_OF_SERVICES),
     +               ANNUAL_ST_FIXED_COST(NUMBER_OF_SERVICES),
     +               ANNUAL_ST_SO2_EMIS(NUMBER_OF_SERVICES))
            ANNUAL_ST_ENERGY = 0.
            ANNUAL_ST_CAPACITY = 0.
            ANNUAL_ST_VAR_COST = 0.
            ANNUAL_ST_SO2_EMIS = 0.
            ANNUAL_ST_FIXED_COST = 0.
         ENDIF
         ANNUAL_ST_ENERGY(UNIT_NO) = ANNUAL_ST_ENERGY(UNIT_NO) +
     +                                                     TRANS_ENERGY
         ANNUAL_ST_CAPACITY(UNIT_NO) = ANNUAL_ST_CAPACITY(UNIT_NO) +
     +                                       TRANS_CAPACITY/LAST_SEASON
         ANNUAL_ST_VAR_COST(UNIT_NO) = ANNUAL_ST_VAR_COST(UNIT_NO) +
     +                                                         VAR_COST
        ANNUAL_ST_FIXED_COST(UNIT_NO) = ANNUAL_ST_FIXED_COST(UNIT_NO) +
     +                                                    ST_FIXED_COST
         ANNUAL_ST_SO2_EMIS(UNIT_NO) = ANNUAL_ST_SO2_EMIS(UNIT_NO) +
     +                                                      ST_SO2_EMIS

! MONTHLY VALUES

         IF(.NOT. ALLOCATED(MONTHLY_ST_ENERGY)) THEN
            ALLOCATE(MONTHLY_ST_ENERGY(0:12,NUMBER_OF_SERVICES),
     +               MONTHLY_ST_CAPACITY(0:12,NUMBER_OF_SERVICES),
     +               MONTHLY_ST_VAR_COST(0:12,NUMBER_OF_SERVICES),
     +               MONTHLY_ST_FIXED_COST(0:12,NUMBER_OF_SERVICES),
     +               MONTHLY_ST_SO2_EMIS(0:12,NUMBER_OF_SERVICES))
            VALUES_2_ZERO = INT(13*NUMBER_OF_SERVICES)
            MONTHLY_ST_ENERGY = 0.
            MONTHLY_ST_CAPACITY = 0.
            MONTHLY_ST_VAR_COST = 0.
            MONTHLY_ST_SO2_EMIS = 0.
            MONTHLY_ST_FIXED_COST = 0.
         ENDIF
         MO_sd = CURRENT_MONTH

            MONTHLY_ST_ENERGY(MO_sd,UNIT_NO) =
     +               MONTHLY_ST_ENERGY(MO_sd,UNIT_NO)
     +                + TRANS_ENERGY
            MONTHLY_ST_CAPACITY(MO_sd,UNIT_NO) =
     +            MONTHLY_ST_CAPACITY(MO_sd,UNIT_NO)
     +             + TRANS_CAPACITY/LAST_SEASON
            MONTHLY_ST_VAR_COST(MO_sd,UNIT_NO) =
     +         MONTHLY_ST_VAR_COST(MO_sd,UNIT_NO)
     +          + VAR_COST
            MONTHLY_ST_FIXED_COST(MO_sd,UNIT_NO) =
     +      MONTHLY_ST_FIXED_COST(MO_sd,UNIT_NO)
     +        + ST_FIXED_COST
            MONTHLY_ST_SO2_EMIS(MO_sd,UNIT_NO) =
     +               MONTHLY_ST_SO2_EMIS(MO_sd,UNIT_NO)
     +                + ST_SO2_EMIS
            IF(CURRENT_MONTH /= 0) THEN
               MO_sd = 0 ! KEEP A RUNNING ANNUAL TOTAL
               MONTHLY_ST_ENERGY(MO_sd,UNIT_NO) =
     +  MONTHLY_ST_ENERGY(MO_sd,UNIT_NO)
     +   + TRANS_ENERGY
               MONTHLY_ST_CAPACITY(MO_sd,UNIT_NO) =
     + MONTHLY_ST_CAPACITY(MO_sd,UNIT_NO)
     +  + TRANS_CAPACITY/LAST_SEASON
               MONTHLY_ST_VAR_COST(MO_sd,UNIT_NO) =
     +     MONTHLY_ST_VAR_COST(MO_sd,UNIT_NO)
     +      + VAR_COST
               MONTHLY_ST_FIXED_COST(MO_sd,UNIT_NO) =
     +           MONTHLY_ST_FIXED_COST(MO_sd,UNIT_NO)
     +             + ST_FIXED_COST
               MONTHLY_ST_SO2_EMIS(MO_sd,UNIT_NO) =
     +          MONTHLY_ST_SO2_EMIS(MO_sd,UNIT_NO)
     +           + ST_SO2_EMIS
            ENDIF
      RETURN


      ENTRY STORE_ANNUAL_ST_COST_DATA(UNIT_NO,
     +                                VAR_COST,
     +                                ST_FIXED_COST,
     +                                TRANS_ENERGY,
     +                                TRANS_CAPACITY)

         ST_SO2_EMIS = 0.
         IF(.NOT. ALLOCATED(ANNUAL_ST_ENERGY)) THEN
            CALL GET_NUMBER_OF_SERVICES (NUMBER_OF_SERVICES)
            ALLOCATE(ANNUAL_ST_ENERGY(NUMBER_OF_SERVICES),
     +               ANNUAL_ST_CAPACITY(NUMBER_OF_SERVICES),
     +               ANNUAL_ST_VAR_COST(NUMBER_OF_SERVICES),
     +               ANNUAL_ST_FIXED_COST(NUMBER_OF_SERVICES),
     +               ANNUAL_ST_SO2_EMIS(NUMBER_OF_SERVICES))
            ANNUAL_ST_ENERGY = 0.
            ANNUAL_ST_CAPACITY = 0.
            ANNUAL_ST_VAR_COST = 0.
            ANNUAL_ST_SO2_EMIS = 0.
            ANNUAL_ST_FIXED_COST = 0.
         ENDIF
         ANNUAL_ST_ENERGY(UNIT_NO) = ANNUAL_ST_ENERGY(UNIT_NO) +
     +                                                     TRANS_ENERGY
         ANNUAL_ST_CAPACITY(UNIT_NO) = ANNUAL_ST_CAPACITY(UNIT_NO) +
     +                                       TRANS_CAPACITY/LAST_SEASON
         ANNUAL_ST_VAR_COST(UNIT_NO) = ANNUAL_ST_VAR_COST(UNIT_NO) +
     +                                                         VAR_COST
        ANNUAL_ST_FIXED_COST(UNIT_NO) = ANNUAL_ST_FIXED_COST(UNIT_NO) +
     +                                                    ST_FIXED_COST
         ANNUAL_ST_SO2_EMIS(UNIT_NO) = ANNUAL_ST_SO2_EMIS(UNIT_NO) +
     +                                                      ST_SO2_EMIS

! MONTHLY VALUES

         IF(.NOT. ALLOCATED(MONTHLY_ST_ENERGY)) THEN
            ALLOCATE(MONTHLY_ST_ENERGY(0:12,NUMBER_OF_SERVICES),
     +               MONTHLY_ST_CAPACITY(0:12,NUMBER_OF_SERVICES),
     +               MONTHLY_ST_VAR_COST(0:12,NUMBER_OF_SERVICES),
     +               MONTHLY_ST_FIXED_COST(0:12,NUMBER_OF_SERVICES),
     +               MONTHLY_ST_SO2_EMIS(0:12,NUMBER_OF_SERVICES))
            MONTHLY_ST_ENERGY = 0.
            MONTHLY_ST_CAPACITY = 0.
            MONTHLY_ST_VAR_COST = 0.
            MONTHLY_ST_SO2_EMIS = 0.
            MONTHLY_ST_FIXED_COST = 0.
         ENDIF
         MO_sd = 0
         MONTHLY_ST_ENERGY(MO_sd,UNIT_NO) = 
     +      MONTHLY_ST_ENERGY(MO_sd,UNIT_NO)
     +                       + TRANS_ENERGY
         MONTHLY_ST_CAPACITY(MO_sd,UNIT_NO) =
     +               MONTHLY_ST_CAPACITY(MO_sd,UNIT_NO)
     +               + TRANS_CAPACITY/LAST_SEASON
         MONTHLY_ST_VAR_COST(MO_sd,UNIT_NO) =
     +         MONTHLY_ST_VAR_COST(MO_sd,UNIT_NO)
     +          + VAR_COST
         MONTHLY_ST_FIXED_COST(MO_sd,UNIT_NO) =
     +          MONTHLY_ST_FIXED_COST(MO_sd,UNIT_NO)
     +            + ST_FIXED_COST
         MONTHLY_ST_SO2_EMIS(MO_sd,UNIT_NO) =
     +          MONTHLY_ST_SO2_EMIS(MO_sd,UNIT_NO)
     +           + ST_SO2_EMIS
         DO MO_sd = 1, 12
            MONTHLY_ST_ENERGY(MO_sd,UNIT_NO)=
     +  MONTHLY_ST_ENERGY(MO_sd,UNIT_NO)
     +               + TRANS_ENERGY/12.
            MONTHLY_ST_CAPACITY(MO_sd,UNIT_NO) =
     +      MONTHLY_ST_CAPACITY(MO_sd,UNIT_NO)
     +     + TRANS_CAPACITY/(12.*LAST_SEASON)
            MONTHLY_ST_VAR_COST(MO_sd,UNIT_NO) =
     +            MONTHLY_ST_VAR_COST(MO_sd,UNIT_NO)
     +             + VAR_COST/12.
            MONTHLY_ST_FIXED_COST(MO_sd,UNIT_NO) =
     +        MONTHLY_ST_FIXED_COST(MO_sd,UNIT_NO)
     +          + ST_FIXED_COST/12.
            MONTHLY_ST_SO2_EMIS(MO_sd,UNIT_NO) =
     +                  MONTHLY_ST_SO2_EMIS(MO_sd,UNIT_NO)
     +                   + ST_SO2_EMIS/12.
         ENDDO

      RETURN


      ENTRY STORE_SERVICE_MONTH(R_ISEAS)


         CURRENT_MONTH = R_ISEAS
      RETURN

      ENTRY CALC_ST_ANN_ASSET_CLASS

         IF(ns_service_decs%SERVICE_TRANS <= 0) then
            RETURN
        end if

         IF(ns_service_decs%SERVICE_TRANS <= 0) then
             RETURN
         end if
         IF(.NOT. ALLOCATED(ST_ANN_CLASS_ATL_EXPENSE)) RETURN
         IF(.NOT. ALLOCATED(MONTHLY_ST_ENERGY)) RETURN
         CURRENT_YEAR = globecom_YEAR+get_BASE_YEAR()
         CURRENT_YEAR_COMPARISON = (CURRENT_YEAR-1900)*100

         ST_ANN_CLASS_EXPENSE_CAPACITY = 0.
         ST_ANN_CLASS_EXPENSE_ENERGY = 0.
         ST_ANN_CLASS_REVENUE_ENERGY = 0.
         ST_ANN_CLASS_REVENUE_CAPACITY = 0.

         ST_ANN_CLASS_ATL_EXPENSE = 0.
         ST_ANN_CLASS_BTL_EXPENSE = 0.
         ST_ANN_CLASS_ADJ_CLAUSE = 0.
         ST_BTL_LEASE_PAYMENT = 0.
         ST_NF_RATEBASE = 0.

         ST_ANN_CLASS_REVENUE = 0.
         ST_ANN_CLASS_EXPENSE = 0.

         IF(MONTHLY_SERVICE_REPORT_ACTIVE .AND.
     +                              SERVICE_TRANS_REPORT_NOT_OPEN) THEN
            TRANS_RPT_NO = ANNUAL_SERVICE_TRANS_HEADER(TRANS_RPT_REC)
            SERVICE_TRANS_REPORT_NOT_OPEN = .FALSE.
         ENDIF

         INCLUDE_FIXED_COSTS_ADJ_CLAUSE = FIXED_COSTS_IN_ADJ_CLAUSE()

         MO_sd = 0
         DO UNIT = 1, ns_service_decs%SERVICE_TRANS

          IF(SERVICE_AVAILABLE(UNIT) - CURRENT_YEAR_COMPARISON>12 .OR.
     +         CURRENT_YEAR_COMPARISON - SERVICE_ENDS(UNIT) > 12) CYCLE


            IF(MONTHLY_SERVICE_REPORT_ACTIVE) THEN
               TRANSACTION_DESCRIPTION =
     +                       trim(LEFT_JUSTIFY_I2_IN_STR(UNIT))//' '//
     +  ns_service_decs%SERVICE_NAME(UNIT)
       IF(ns_service_decs%SERVICE_COST_ASSIGNMENT(UNIT) == 'R') THEN
          COST_ASSIGNMENT_NAME = 'Revenue'
       ELSE
          COST_ASSIGNMENT_NAME = 'Expense'
       ENDIF
       UNIT_ENERGY_COST = 0.
       UNIT_CAPACITY_COST = 0.
                IF(ANNUAL_ST_ENERGY(UNIT) /= 0.) THEN
          UNIT_ENERGY_COST = ANNUAL_ST_VAR_COST(UNIT)/
     +                                           ANNUAL_ST_ENERGY(UNIT)
               ENDIF
               IF(ANNUAL_ST_CAPACITY(UNIT) /= 0.) THEN
                  UNIT_CAPACITY_COST = ANNUAL_ST_FIXED_COST(UNIT)/
     +                                         ANNUAL_ST_CAPACITY(UNIT)
               ENDIF

               WRITE(TRANS_RPT_NO,REC=TRANS_RPT_REC)
     +                     PRT_ENDPOINT(),
     +                     FLOAT(CURRENT_YEAR),
     +                     TRANSACTION_DESCRIPTION,
     +                     COST_ASSIGNMENT_NAME,
     +                     ANNUAL_ST_ENERGY(UNIT),
     +                     1000.*UNIT_ENERGY_COST,
     +                     ANNUAL_ST_VAR_COST(UNIT),
     +                     ANNUAL_ST_CAPACITY(UNIT),
     +                     UNIT_CAPACITY_COST/12.,
     +                     ANNUAL_ST_FIXED_COST(UNIT),
     +                     ANNUAL_ST_VAR_COST(UNIT) +
     +                                       ANNUAL_ST_FIXED_COST(UNIT)
               TRANS_RPT_REC = TRANS_RPT_REC + 1
            ENDIF

             IF(SERVICE_INTRA_COMPY_TRANSACTION(UNIT) == 'Y') THEN
! ALLOCATE TO TOTAL COMPANY AND ASSET CLASSES
               TRANSACTION_AMOUNT = MONTHLY_ST_VAR_COST(MO_sd,UNIT) +
     +           MONTHLY_ST_FIXED_COST(MO_sd,UNIT)
       IF(ns_service_decs%SERVICE_COST_ASSIGNMENT(UNIT) 
     +  == REVENUE) THEN
                   REV_TYPE = INCOME_STATEMENT_POSITION(
     +                            SERVICE_REVENUE_CLASSIFICATION(UNIT))
                    EXP_TYPE = INCOME_STATEMENT_POSITION(
     +                              INTRA_ACCOUNT_CLASSIFICATION(UNIT))
                 IF(INDEX(INTRA_EXPENSE_COLLECTION(UNIT),'BTL')/=0)THEN
                     IF(EXP_TYPE == 22) THEN ! Lease Expense
       ST_BTL_LEASE_PAYMENT(MO_sd,-1) = TRANSACTION_AMOUNT
     +                   + ST_BTL_LEASE_PAYMENT(MO_sd,-1)

                     ! Nuc Fuel Expense


                     ELSEIF(EXP_TYPE == 17 .OR. EXP_TYPE == 18) THEN
                        ST_NF_RATEBASE(MO_sd,-1) = TRANSACTION_AMOUNT +
     +                              ST_NF_RATEBASE(MO_sd,-1)
                     ENDIF
                     EXP_TYPE = 28
                  ENDIF
                ELSE
                   REV_TYPE = INCOME_STATEMENT_POSITION(
     +                              INTRA_ACCOUNT_CLASSIFICATION(UNIT))
                     EXP_TYPE = INCOME_STATEMENT_POSITION(
     +      ns_service_decs%SERVICE_EXPENSE_CLASSIFICATION(UNIT))
                  IF(INDEX(
     + ns_service_decs%SERVICE_EXPENSE_COLLECTION(UNIT),'BTL')
     +                                                       /= 0) THEN
                     IF(EXP_TYPE == 22) THEN
       ST_BTL_LEASE_PAYMENT(MO_sd,-1) = TRANSACTION_AMOUNT
     +                     + ST_BTL_LEASE_PAYMENT(MO_sd,-1)

                     ! Nuc Fuel Expense

                     ELSEIF(EXP_TYPE == 17 .OR. EXP_TYPE == 18) THEN
                        ST_NF_RATEBASE(MO_sd,-1) = TRANSACTION_AMOUNT
     +                    + ST_NF_RATEBASE(MO_sd,-1)
                     ENDIF
                     EXP_TYPE = 28
                  ENDIF
                ENDIF
        ST_ANN_CLASS_REVENUE(MO_sd,-1,REV_TYPE) = TRANSACTION_AMOUNT
     +                     + ST_ANN_CLASS_REVENUE(MO_sd,-1,REV_TYPE)
        ST_ANN_CLASS_EXPENSE(MO_sd,-1,EXP_TYPE) = TRANSACTION_AMOUNT
     +                     + ST_ANN_CLASS_EXPENSE(MO_sd,-1,EXP_TYPE)

                IF(INTRA_ASSET_CLASS_ID(UNIT) < 0.) THEN
                   CALL GET_ASSET_VAR(ABS(INTRA_ASSET_CLASS_ID(UNIT)),
     +                                      DUMMY_TYPE,ASSET_CLASS_LIST)
                   CALL GET_ASSET_VAR(
     +                        ABS(INTRA_ASSET_CLASS_ALLOC_VECTOR(UNIT)),
     +                                 DUMMY_TYPE,ASSET_ALLOCATION_LIST)
                ELSE
                   ASSET_CLASS_LIST(1) = INTRA_ASSET_CLASS_ID(UNIT)
                   ASSET_CLASS_LIST(2) = 0
                   ASSET_ALLOCATION_LIST(1) = 100.
                   ASSET_ALLOCATION_LIST(2) = 0.
                ENDIF

                CLASS_POINTER = 1
                DO
                   ASSET_CLASS_sd = ASSET_CLASS_LIST(CLASS_POINTER)
                   CALL CHECK_IF_CLASS_DEFINED(ASSET_CLASS_sd)
                   ASSET_CLASS_sd = ASSET_CLASS_sd + 1

                 ASSET_ALLOCATOR = ASSET_ALLOCATION_LIST(CLASS_POINTER)
                  IF(ASSET_ALLOCATOR < 0.) THEN
                     ALLOCATION_VECTOR = ABS(ASSET_ALLOCATOR)
                     CALL GET_ASSET_VAR(ALLOCATION_VECTOR,
     +                                     DUMMY_TYPE,ALLOCATION_VALUE)
                     ASSET_ALLOCATOR =
     +      ALLOCATION_VALUE(MIN(AVAIL_DATA_YEARS,globecom_YEAR))
                  ENDIF

                  ASSET_ALLOCATOR = ASSET_ALLOCATOR/100.
        TRANSACTION_AMOUNT = (MONTHLY_ST_VAR_COST(MO_sd,UNIT) +
     +                       MONTHLY_ST_FIXED_COST(MO_sd,UNIT))*
     +                                        ASSET_ALLOCATOR

       IF(ns_service_decs%SERVICE_COST_ASSIGNMENT(UNIT) == REVENUE) THEN
         ST_ANN_CLASS_EXPENSE(MO_sd,ASSET_CLASS_sd,EXP_TYPE) =
     + ST_ANN_CLASS_EXPENSE(MO_sd,ASSET_CLASS_sd,EXP_TYPE)
     +                     + TRANSACTION_AMOUNT
                     IF(INDEX(INTRA_EXPENSE_COLLECTION(UNIT),'Adj')
     +                                                      /= 0) THEN
           IF(INCLUDE_FIXED_COSTS_ADJ_CLAUSE) THEN
                 ST_ANN_CLASS_ADJ_CLAUSE(MO_sd,ASSET_CLASS_sd) =
     +          ST_ANN_CLASS_ADJ_CLAUSE(MO_sd,ASSET_CLASS_sd)
     +                           + TRANSACTION_AMOUNT
           ELSE
         ST_ANN_CLASS_ADJ_CLAUSE(MO_sd,ASSET_CLASS_sd) =
     +     ST_ANN_CLASS_ADJ_CLAUSE(MO_sd,ASSET_CLASS_sd)
     +          + ASSET_ALLOCATOR *
     +                    MONTHLY_ST_VAR_COST(MO_sd,UNIT)
                        ENDIF
                     ENDIF
                   ELSE
          ST_ANN_CLASS_REVENUE(MO_sd,ASSET_CLASS_sd,REV_TYPE) =
     +      ST_ANN_CLASS_REVENUE(MO_sd,ASSET_CLASS_sd,REV_TYPE)
     +          + TRANSACTION_AMOUNT
                   ENDIF

                   CLASS_POINTER = CLASS_POINTER + 1
                   IF(ASSET_CLASS_LIST(CLASS_POINTER) == 0 .OR.
     +                           CLASS_POINTER > AVAIL_DATA_YEARS) then
                    EXIT
                   endif
                ENDDO
             ENDIF ! END INTEA-COMPANY STUFF



C
            ASSET_CLASS_sd = ASSET_CLASS_NUM(UNIT)
            ASSET_ALLOCATION_VECTOR = ASSET_CLASS_VECTOR(UNIT)

            IF(ASSET_CLASS_sd < 0) THEN
               CALL GET_ASSET_VAR(ABS(ASSET_CLASS_sd),
     +                                 DUMMY_TYPE,ASSET_CLASS_LIST)
               CALL GET_ASSET_VAR(ABS(ASSET_ALLOCATION_VECTOR),
     +                                DUMMY_TYPE,ASSET_ALLOCATION_LIST)
            ELSE
               ASSET_CLASS_LIST(1) = ASSET_CLASS_sd
               ASSET_CLASS_LIST(2) = 0.
               ASSET_ALLOCATION_LIST(1) = 100.
               ASSET_ALLOCATION_LIST(2) = 0.
            ENDIF
            CLASS_POINTER = 1

            DO
               ASSET_CLASS_sd = ASSET_CLASS_LIST(CLASS_POINTER)
               CALL CHECK_IF_CLASS_DEFINED(ASSET_CLASS_sd)
               ASSET_CLASS_sd = ASSET_CLASS_sd + 1

               ASSET_ALLOCATOR = ASSET_ALLOCATION_LIST(CLASS_POINTER)
               IF(ASSET_ALLOCATOR < 0.) THEN
                  ALLOCATION_VECTOR = ABS(ASSET_ALLOCATOR)
                  CALL GET_ASSET_VAR(ALLOCATION_VECTOR,
     +                                     DUMMY_TYPE,ALLOCATION_VALUE)
                  ASSET_ALLOCATOR =
     +         ALLOCATION_VALUE(MIN(AVAIL_DATA_YEARS,globecom_YEAR))
               ENDIF

               ASSET_ALLOCATOR = ASSET_ALLOCATOR/100.

               DO MO_sd = 1, 12
                  IF(.NOT. TRANSFER_TRANSACT_ANL_RESULTS(MO_sd)
     +                                              .AND. WVPA()) then
                      CYCLE
                  end if


       IF(ns_service_decs%SERVICE_COST_ASSIGNMENT(UNIT) == REVENUE) 
     + THEN
          ST_ANN_CLASS_REVENUE_CAPACITY(MO_sd,ASSET_CLASS_sd) =
     +      ST_ANN_CLASS_REVENUE_CAPACITY(MO_sd,ASSET_CLASS_sd)
     +      + ASSET_ALLOCATOR * MONTHLY_ST_CAPACITY(MO_sd,UNIT)
          ST_ANN_CLASS_REVENUE_ENERGY(MO_sd,ASSET_CLASS_sd) =
     +        ST_ANN_CLASS_REVENUE_ENERGY(MO_sd,ASSET_CLASS_sd)
     +        + ASSET_ALLOCATOR * MONTHLY_ST_ENERGY(MO_sd,UNIT)
       
          ST_REVENUE = ASSET_ALLOCATOR *
     +                       (MONTHLY_ST_VAR_COST(MO_sd,UNIT) +
     +                       MONTHLY_ST_FIXED_COST(MO_sd,UNIT))
           REV_TYPE = INCOME_STATEMENT_POSITION(
     +                 SERVICE_REVENUE_CLASSIFICATION(UNIT))
C
              ST_ANN_CLASS_REVENUE(MO_sd,ASSET_CLASS_sd,REV_TYPE) =
     +          ST_ANN_CLASS_REVENUE(MO_sd,ASSET_CLASS_sd,REV_TYPE)
     +              + ST_REVENUE
                  ELSE
!
           ST_ANN_CLASS_EXPENSE_CAPACITY(MO_sd,ASSET_CLASS_sd) =
     +     ST_ANN_CLASS_EXPENSE_CAPACITY(MO_sd,ASSET_CLASS_sd)
     +       + ASSET_ALLOCATOR * MONTHLY_ST_CAPACITY(MO_sd,UNIT)
           ST_ANN_CLASS_EXPENSE_ENERGY(MO_sd,ASSET_CLASS_sd) =
     +        ST_ANN_CLASS_EXPENSE_ENERGY(MO_sd,ASSET_CLASS_sd)
     +         + ASSET_ALLOCATOR * MONTHLY_ST_ENERGY(MO_sd,UNIT)
           ST_EXPENSE = ASSET_ALLOCATOR *
     +                       (MONTHLY_ST_VAR_COST(MO_sd,UNIT) +
     +                        MONTHLY_ST_FIXED_COST(MO_sd,UNIT))
             EXP_TYPE = INCOME_STATEMENT_POSITION(
     +             ns_service_decs%SERVICE_EXPENSE_CLASSIFICATION(UNIT))
                     IF(INDEX('BTL,X,N',
     +      ns_service_decs%SERVICE_EXPENSE_COLLECTION(UNIT)) /= 0) THEN
                   IF(EXP_TYPE == 22) THEN
                      ST_BTL_LEASE_PAYMENT(MO_sd,ASSET_CLASS_sd) =
     +                     ST_BTL_LEASE_PAYMENT(MO_sd,ASSET_CLASS_sd)
     +                         +  ST_EXPENSE
                   ! Nuc Fuel Expense
                   ELSEIF(EXP_TYPE == 17 .OR. EXP_TYPE == 18) THEN
                      ST_NF_RATEBASE(MO_sd,ASSET_CLASS_sd) =
     +                           ST_NF_RATEBASE(MO_sd,ASSET_CLASS_sd)
     +                               + TRANSACTION_AMOUNT
                   ENDIF
                        EXP_TYPE = 28
                     ENDIF

                     SELECT CASE(
     +           trim(ns_service_decs%SERVICE_EXPENSE_COLLECTION(UNIT)))
                  CASE ('BTL','X','N')
                     IF(EXP_TYPE == 22) THEN
                      ST_BTL_LEASE_PAYMENT(MO_sd,ASSET_CLASS_sd) =
     +                    ST_BTL_LEASE_PAYMENT(MO_sd,ASSET_CLASS_sd)
     +                        + ST_EXPENSE
                     ENDIF
                     EXP_TYPE = 28
                  CASE ('Adj','A')
                     IF(INCLUDE_FIXED_COSTS_ADJ_CLAUSE) THEN
                     ST_ANN_CLASS_ADJ_CLAUSE(MO_sd,ASSET_CLASS_sd) =
     +                ST_ANN_CLASS_ADJ_CLAUSE(MO_sd,ASSET_CLASS_sd)
     +                     + TRANSACTION_AMOUNT
                     ELSE
                    ST_ANN_CLASS_ADJ_CLAUSE(MO_sd,ASSET_CLASS_sd) =
     +                 ST_ANN_CLASS_ADJ_CLAUSE(MO_sd,ASSET_CLASS_sd)
     +                     + ASSET_ALLOCATOR *
     +                               MONTHLY_ST_VAR_COST(MO_sd,UNIT)
                     ENDIF
               END SELECT
               ST_ANN_CLASS_EXPENSE(MO_sd,ASSET_CLASS_sd,EXP_TYPE) =
     +          ST_ANN_CLASS_EXPENSE(MO_sd,ASSET_CLASS_sd,EXP_TYPE)
     +               + ST_EXPENSE
                  ENDIF
               ENDDO

               CLASS_POINTER = CLASS_POINTER + 1
               IF(ASSET_CLASS_LIST(CLASS_POINTER) == 0. .OR.
     +                           CLASS_POINTER > AVAIL_DATA_YEARS) EXIT
            ENDDO ! ASSET CLASSES

         ENDDO ! SERVICE RESOURCES
         IF(ALLOCATED(ANNUAL_ST_ENERGY))
     +         DEALLOCATE(ANNUAL_ST_ENERGY,
     +                    ANNUAL_ST_CAPACITY,
     +                    ANNUAL_ST_VAR_COST,
     +                    ANNUAL_ST_FIXED_COST,
     +                    ANNUAL_ST_SO2_EMIS)
         IF(ALLOCATED(MONTHLY_ST_ENERGY))
     +          DEALLOCATE(MONTHLY_ST_ENERGY,
     +                     MONTHLY_ST_CAPACITY,
     +                     MONTHLY_ST_VAR_COST,
     +                     MONTHLY_ST_FIXED_COST,
     +                     MONTHLY_ST_SO2_EMIS)

! SUM TO ANNUAL TOTALS

         ST_ANN_CLASS_ATL_EXPENSE(0,:) =
     +                        SUM(ST_ANN_CLASS_ATL_EXPENSE(1:,:),DIM=1)
         ST_ANN_CLASS_BTL_EXPENSE(0,:) =
     +                        SUM(ST_ANN_CLASS_BTL_EXPENSE(1:,:),DIM=1)
         ST_ANN_CLASS_ADJ_CLAUSE(0,:) =
     +                         SUM(ST_ANN_CLASS_ADJ_CLAUSE(1:,:),DIM=1)
         ST_BTL_LEASE_PAYMENT(0,:) =
     +                            SUM(ST_BTL_LEASE_PAYMENT(1:,:),DIM=1)
         ST_NF_RATEBASE(0,:) = SUM(ST_NF_RATEBASE(1:,:),DIM=1)
         ST_ANN_CLASS_REVENUE_CAPACITY(0,:) =
     +                   SUM(ST_ANN_CLASS_REVENUE_CAPACITY(1:,:),DIM=1)
         ST_ANN_CLASS_REVENUE_ENERGY(0,:) =
     +                     SUM(ST_ANN_CLASS_REVENUE_ENERGY(1:,:),DIM=1)
         ST_ANN_CLASS_EXPENSE_CAPACITY(0,:) =
     +                   SUM(ST_ANN_CLASS_EXPENSE_CAPACITY(1:,:),DIM=1)
         ST_ANN_CLASS_EXPENSE_ENERGY(0,:) =
     +                     SUM(ST_ANN_CLASS_EXPENSE_ENERGY(1:,:),DIM=1)

         ST_ANN_CLASS_REVENUE(0,:,:) =
     +                          SUM(ST_ANN_CLASS_REVENUE(1:,:,:),DIM=1)
         ST_ANN_CLASS_EXPENSE(0,:,11:) =
     +                        SUM(ST_ANN_CLASS_EXPENSE(1:,:,11:),DIM=1)

! SCALE

         ST_ANN_CLASS_BTL_EXPENSE = ST_ANN_CLASS_BTL_EXPENSE/1000.
         ST_ANN_CLASS_ATL_EXPENSE = ST_ANN_CLASS_ATL_EXPENSE/1000.
         ST_ANN_CLASS_ADJ_CLAUSE = ST_ANN_CLASS_ADJ_CLAUSE/1000.
         ST_BTL_LEASE_PAYMENT = ST_BTL_LEASE_PAYMENT/1000.
         ST_NF_RATEBASE = ST_NF_RATEBASE/1000.
         ST_ANN_CLASS_REVENUE = ST_ANN_CLASS_REVENUE/1000.
         ST_ANN_CLASS_EXPENSE = ST_ANN_CLASS_EXPENSE/1000.

      RETURN ! CALC_ST_ANN_ASSET_CLASS

      ENTRY MONTHLY_SERVICE_TRANS_REVENUES(R_CLASS,
     +                                     MONTH_VARS)


         IF(ns_service_decs%SERVICE_TRANS <= 0) RETURN

         IF(R_CLASS <= MAX_SERVICE_CLASS_ID_NUM) THEN
            ASSET_CLASS_sd = R_CLASS
            IF(ASSET_CLASS_sd > 0 .OR. R_CLASS == -1) THEN
!               DO MO_sd = 1, 12
!
! REVENUES
!
                  MONTH_VARS(:,BaseRates) =
     +                    MONTH_VARS(:,BaseRates) +
     +                         ST_ANN_CLASS_REVENUE(:,ASSET_CLASS_sd,1)
     
                  MONTH_VARS(:,adjustment_clause) =
     +                    MONTH_VARS(:,adjustment_clause) +
     +                         ST_ANN_CLASS_REVENUE(:,ASSET_CLASS_sd,2)
     
                  MONTH_VARS(:,SecondarySales) =
     +                    MONTH_VARS(:,SecondarySales) +
     +                         ST_ANN_CLASS_REVENUE(:,ASSET_CLASS_sd,3)
     
                  MONTH_VARS(:,OtherRevenue) =
     +                    MONTH_VARS(:,OtherRevenue) +
     +                         ST_ANN_CLASS_REVENUE(:,ASSET_CLASS_sd,4)
     
                  MONTH_VARS(:,BTLRevenues) =
     +                    MONTH_VARS(:,BTLRevenues) +
     +                            ST_ANN_CLASS_REVENUE(:,ASSET_CLASS,5)
     
                  MONTH_VARS(:,CatawbaRevenues) =
     +                    MONTH_VARS(:,CatawbaRevenues) +
     +                         ST_ANN_CLASS_REVENUE(:,ASSET_CLASS_sd,6)
     
                  MONTH_VARS(:,GasRevenues) =
     +                    MONTH_VARS(:,GasRevenues) +
     +                         ST_ANN_CLASS_REVENUE(:,ASSET_CLASS_sd,7)
     
                  MONTH_VARS(:,UnbilledRevenues) =
     +                    MONTH_VARS(:,UnbilledRevenues) +
     +                         ST_ANN_CLASS_REVENUE(:,ASSET_CLASS_sd,8)
     
                  MONTH_VARS(:,DeferredRevenues) =
     +                    MONTH_VARS(:,DeferredRevenues) +
     +                         ST_ANN_CLASS_REVENUE(:,ASSET_CLASS_sd,9)
     
                  MONTH_VARS(:,RelationshipRevenues) =
     +                    MONTH_VARS(:,RelationshipRevenues) +
     +                        ST_ANN_CLASS_REVENUE(:,ASSET_CLASS_sd,10)
     
                  MONTH_VARS(:,Residential) =
     +                    MONTH_VARS(:,Residential) +
     +                   ST_ANN_CLASS_REVENUE(:,ASSET_CLASS_sd,11)
     
                  MONTH_VARS(:,Commercial) =
     +                    MONTH_VARS(:,Commercial) +
     +                   ST_ANN_CLASS_REVENUE(:,ASSET_CLASS_sd,12)
     
                  MONTH_VARS(:,Industrial) =
     +                    MONTH_VARS(:,Industrial) +
     +                   ST_ANN_CLASS_REVENUE(:,ASSET_CLASS_sd,13)
     
                  MONTH_VARS(:,Lighting) =
     +                    MONTH_VARS(:,Lighting) +
     +                   ST_ANN_CLASS_REVENUE(:,ASSET_CLASS_sd,14)
                  MONTH_VARS(:,BulkPower) =
     +                    MONTH_VARS(:,BulkPower) +
     +                   ST_ANN_CLASS_REVENUE(:,ASSET_CLASS_sd,15)
                  MONTH_VARS(:,NetofTaxBTLRevenues) =
     +                    MONTH_VARS(:,NetofTaxBTLRevenues) +
     +                   ST_ANN_CLASS_REVENUE(:,ASSET_CLASS_sd,16)
                  MONTH_VARS(:,CapacitySales) =
     +                    MONTH_VARS(:,CapacitySales) +
     +                   ST_ANN_CLASS_REVENUE(:,ASSET_CLASS_sd,17)
                  MONTH_VARS(:,Government) =
     +                    MONTH_VARS(:,Government) +
     +                   ST_ANN_CLASS_REVENUE(:,ASSET_CLASS_sd,18)

            ENDIF
         ENDIF
      RETURN

      ENTRY MONTHLY_SERVICE_TRANS_CASH(R_CLASS,
     +                                 MONTH_VARS)


         IF(ns_service_decs%SERVICE_TRANS <= 0) RETURN

         CALL MONTHLY_SERVICE_TRANS_REVENUES(R_CLASS,MONTH_VARS)
!
         IF(R_CLASS <= MAX_SERVICE_CLASS_ID_NUM) THEN
            ASSET_CLASS_sd = R_CLASS
            IF(ASSET_CLASS_sd > 0 .OR. R_CLASS == -1) THEN

! EXPENSES

                  MONTH_VARS(:,Cash_Fossil_Fuel) =
     +                    MONTH_VARS(:,Cash_Fossil_Fuel) +
     +                ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS_sd,11)
!
                  MONTH_VARS(:,Cash_Purchased_Power) =
     +                MONTH_VARS(:,Cash_Purchased_Power) +
     +                ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS_sd,12)

!
                  MONTH_VARS(:,Cash_Variable_OandM) =
     +                     MONTH_VARS(:,Cash_Variable_OandM) +
     +                ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS_sd,13)
!
                  MONTH_VARS(:,Cash_Fixed_OandM) =
     +                   MONTH_VARS(:,Cash_Fixed_OandM) +
     +                ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS_sd,14)
!
                  MONTH_VARS(:,Cash_Other_OandM) =
     +                    MONTH_VARS(:,Cash_Other_OandM) +
     +                ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS_sd,15)
!
                  MONTH_VARS(:,CashPurchasedGas) =
     +                    MONTH_VARS(:,CashPurchasedGas) +
     +                ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS_sd,16)
!
                  MONTH_VARS(:,Cash_Other) =
     +                    MONTH_VARS(:,Cash_Other) +
     +                ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS_sd,17)
!
                  MONTH_VARS(:,Cash_Leased_Nuclear_Fuel) =
     +                    MONTH_VARS(:,Cash_Leased_Nuclear_Fuel) +
     +                ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS_sd,19)
!
                  MONTH_VARS(:,CashDSMExpense) =
     +                    MONTH_VARS(:,CashDSMExpense) +
     +                ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS_sd,20)
!
                  MONTH_VARS(:,CashDSMRebate) =
     +                    MONTH_VARS(:,CashDSMRebate) +
     +                ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS_sd,21)
!
                  MONTH_VARS(:,CashLeaseExpense) =
     +                    MONTH_VARS(:,CashLeaseExpense) +
     +                ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS_sd,22)
!
                  MONTH_VARS(:,CashServiceTransactions) =
     +                    MONTH_VARS(:,CashServiceTransactions) +
     +                 ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS_sd,23)
!
                  MONTH_VARS(:,CashEmissionCredits) =
     +                    MONTH_VARS(:,CashEmissionCredits) +
     +                 ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS_sd,24)
!
                  MONTH_VARS(:,CashDOEDecommissioning) =
     +                    MONTH_VARS(:,CashDOEDecommissioning) +
     +                           ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS,25)
!
                  MONTH_VARS(:,CashDOEDisposal) =
     +                    MONTH_VARS(:,CashDOEDisposal) +
     +                 ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS_sd,26)
!
                  MONTH_VARS(:,cash_catawba_expenses) =
     +                    MONTH_VARS(:,cash_catawba_expenses) +
     +                 ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS_sd,27)
!
                  MONTH_VARS(:,CashBTLExpenses) =
     +                   MONTH_VARS(:,CashBTLExpenses) +
     +                           ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS,28)
!
                  MONTH_VARS(:,Cash_BTL_Lease_Cash) =
     +                    MONTH_VARS(:,Cash_BTL_Lease_Cash) +
     +                         ST_BTL_LEASE_PAYMENT(:,ASSET_CLASS_sd)

            ENDIF
         ENDIF
      RETURN

      ENTRY SERVICE_TRANS_INFO(R_CLASS,
     +                         R_FUEXP,R_PREXP,R_OPEXP,
     +                         R_MNEXP,R_OTHER1,R_OTHER2,
     +                         R_OTHER3,R_NFOWN,R_NFLEASE,
     +                         R_ADJ_EXP,
     +                         R_NF_RATEBASE,R_DSM_EXPENSE,
     +                         R_DSM_REBATE,
     +                         R_ADJUSTMENT_CLAUSE_REVENUES,
     +                         R_BASE_RATES_REVENUES,
     +                         R_SECONDARY_SALES_REVENUES,
     +                         R_OTHER_REVENUES,
     +                         R_BTL_REVENUES,
     +                         R_BTL_EXPENSE,
     +                         R_ATL_LEASE_EXP,
     +                         R_BTL_LEASE_EXP,
     +                         R_SERVICE_TRANSACTIONS,
     +                         R_EMISSION_CREDITS,
     +                         R_DOE_DISPOSAL,
     +                         R_DOE_DECOMMISSIONING,
     +                         R_CATAWBA_REVENUES,
     +                         R_CATAWBA_EXPENSES,
     +                         R_CAPACITY_REVENUES)


         IF(ns_service_decs%SERVICE_TRANS <= 0) RETURN

         IF(R_CLASS <= MAX_SERVICE_CLASS_ID_NUM) THEN
            ASSET_CLASS_sd = R_CLASS
            IF(ASSET_CLASS_sd > 0 .OR. R_CLASS == -1) THEN
               MO_sd = 0

C REVENUES

        R_BASE_RATES_REVENUES = R_BASE_RATES_REVENUES +
     +                    ST_ANN_CLASS_REVENUE(MO_sd,ASSET_CLASS_sd,1)
        R_ADJUSTMENT_CLAUSE_REVENUES =
     +                            R_ADJUSTMENT_CLAUSE_REVENUES +
     +                    ST_ANN_CLASS_REVENUE(MO_sd,ASSET_CLASS_sd,2)
        R_SECONDARY_SALES_REVENUES = R_SECONDARY_SALES_REVENUES +
     +                    ST_ANN_CLASS_REVENUE(MO_sd,ASSET_CLASS_sd,3)
        R_OTHER_REVENUES = R_OTHER_REVENUES +
     +                    ST_ANN_CLASS_REVENUE(MO_sd,ASSET_CLASS_sd,4)
        R_BTL_REVENUES = R_BTL_REVENUES +
     +                    ST_ANN_CLASS_REVENUE(MO_sd,ASSET_CLASS_sd,5)
        R_CATAWBA_REVENUES = R_CATAWBA_REVENUES +
     +                    ST_ANN_CLASS_REVENUE(MO_sd,ASSET_CLASS_sd,6)
        R_CAPACITY_REVENUES = R_CAPACITY_REVENUES +
     +                   ST_ANN_CLASS_REVENUE(MO_sd,ASSET_CLASS_sd,17)

C EXPENSES

               R_FUEXP = R_FUEXP +
     +                  ST_ANN_CLASS_EXPENSE(MO_sd,ASSET_CLASS_sd,11)
       R_PREXP = R_PREXP +
     +                  ST_ANN_CLASS_EXPENSE(MO_sd,ASSET_CLASS_sd,12)
       R_OPEXP = R_OPEXP +
     +                  ST_ANN_CLASS_EXPENSE(MO_sd,ASSET_CLASS_sd,13)
       R_MNEXP = R_MNEXP +
     +                  ST_ANN_CLASS_EXPENSE(MO_sd,ASSET_CLASS_sd,14)
       R_OTHER1 = R_OTHER1 +
     +                  ST_ANN_CLASS_EXPENSE(MO_sd,ASSET_CLASS_sd,15)
       R_OTHER2 = R_OTHER2 +
     +                  ST_ANN_CLASS_EXPENSE(MO_sd,ASSET_CLASS_sd,16)
       R_OTHER3 = R_OTHER3 +
     +                  ST_ANN_CLASS_EXPENSE(MO_sd,ASSET_CLASS_sd,17)
       R_NFOWN = R_NFOWN +
     +                  ST_ANN_CLASS_EXPENSE(MO_sd,ASSET_CLASS_sd,18)
       R_NFLEASE = R_NFLEASE +
     +                  ST_ANN_CLASS_EXPENSE(MO_sd,ASSET_CLASS_sd,19)
       R_DSM_EXPENSE = R_DSM_EXPENSE +
     +                  ST_ANN_CLASS_EXPENSE(MO_sd,ASSET_CLASS_sd,20)
       R_DSM_REBATE =  R_DSM_REBATE +
     +                  ST_ANN_CLASS_EXPENSE(MO_sd,ASSET_CLASS_sd,21)
       R_ATL_LEASE_EXP = R_ATL_LEASE_EXP +
     +                  ST_ANN_CLASS_EXPENSE(MO_sd,ASSET_CLASS_sd,22)
       R_SERVICE_TRANSACTIONS = R_SERVICE_TRANSACTIONS +
     +                  ST_ANN_CLASS_EXPENSE(MO_sd,ASSET_CLASS_sd,23)
       R_EMISSION_CREDITS = R_EMISSION_CREDITS +
     +                  ST_ANN_CLASS_EXPENSE(MO_sd,ASSET_CLASS_sd,24)
       R_DOE_DECOMMISSIONING = R_DOE_DECOMMISSIONING +
     +                  ST_ANN_CLASS_EXPENSE(MO_sd,ASSET_CLASS_sd,25)
       R_DOE_DISPOSAL =  R_DOE_DISPOSAL +
     +                  ST_ANN_CLASS_EXPENSE(MO_sd,ASSET_CLASS_sd,26)
       R_CATAWBA_EXPENSES = R_CATAWBA_EXPENSES +
     +                  ST_ANN_CLASS_EXPENSE(MO_sd,ASSET_CLASS_sd,27)
       R_BTL_EXPENSE = R_BTL_EXPENSE +
     +                  ST_ANN_CLASS_EXPENSE(MO_sd,ASSET_CLASS_sd,28)


! SPECIAL EXPENSE ITEMS

               R_ADJ_EXP = R_ADJ_EXP +
     +                   ST_ANN_CLASS_ADJ_CLAUSE(MO_sd,ASSET_CLASS_sd)
        R_BTL_LEASE_EXP = R_BTL_LEASE_EXP +
     +                      ST_BTL_LEASE_PAYMENT(MO_sd,ASSET_CLASS_sd)
        R_NF_RATEBASE = R_NF_RATEBASE +
     +                            ST_NF_RATEBASE(MO_sd,ASSET_CLASS_sd)
            ENDIF
         ENDIF
      RETURN

      ENTRY SET_UP_ST_CLASS_ARRAYS


         IF(ALLOCATED(ST_ANN_CLASS_ATL_EXPENSE))
     +                     DEALLOCATE(ST_ANN_CLASS_ATL_EXPENSE,
     +                                ST_ANN_CLASS_BTL_EXPENSE,
     +                                ST_ANN_CLASS_ADJ_CLAUSE,
     +                                ST_BTL_LEASE_PAYMENT,
     +                                ST_NF_RATEBASE,
     +                                ST_ANN_CLASS_EXPENSE_CAPACITY,
     +                                ST_ANN_CLASS_EXPENSE_ENERGY,
     +                                ST_ANN_CLASS_REVENUE_ENERGY,
     +                                ST_ANN_CLASS_REVENUE_CAPACITY,
     +                                ST_ANN_CLASS_REVENUE,
     +                                ST_ANN_CLASS_EXPENSE)
         ALLOCATE(ST_ANN_CLASS_ATL_EXPENSE(0:12,
     +                                    -1:MAX_SERVICE_CLASS_ID_NUM),
     +        ST_ANN_CLASS_BTL_EXPENSE(0:12,
     +                                    -1:MAX_SERVICE_CLASS_ID_NUM),
     +       ST_ANN_CLASS_ADJ_CLAUSE(0:12,-1:MAX_SERVICE_CLASS_ID_NUM),
     +        ST_BTL_LEASE_PAYMENT(0:12,-1:MAX_SERVICE_CLASS_ID_NUM),
     +        ST_NF_RATEBASE(0:12,-1:MAX_SERVICE_CLASS_ID_NUM),
     +        ST_ANN_CLASS_EXPENSE_CAPACITY(0:12,
     +                                     0:MAX_SERVICE_CLASS_ID_NUM),
     +        ST_ANN_CLASS_EXPENSE_ENERGY(0:12,
     +                                     0:MAX_SERVICE_CLASS_ID_NUM),
     +        ST_ANN_CLASS_REVENUE_ENERGY(0:12,
     +                                     0:MAX_SERVICE_CLASS_ID_NUM),
     +        ST_ANN_CLASS_REVENUE_CAPACITY(0:12,
     +                                     0:MAX_SERVICE_CLASS_ID_NUM),
     +        ST_ANN_CLASS_REVENUE(0:12,-1:MAX_SERVICE_CLASS_ID_NUM,
     +                                               LAST_INCOME_LINE),
     +        ST_ANN_CLASS_EXPENSE(0:12,-1:MAX_SERVICE_CLASS_ID_NUM,
     +                                              LAST_EXPENSE_ITEM))

         IF(ALLOCATED(ASSET_CLASS_LIST))
     +               DEALLOCATE(ASSET_CLASS_LIST,ASSET_ALLOCATION_LIST)
         ALLOCATE(ASSET_CLASS_LIST(AVAIL_DATA_YEARS),
     +                         ASSET_ALLOCATION_LIST(AVAIL_DATA_YEARS))
      RETURN

      END subroutine READ_SERVICE_TRANS

!
!                   SERVICE TRANSACTIONS ALLOCATION MODULE
!                              COPYRIGHT (C) 1992
!                        M.S. GERBER & ASSOCIATES, INC.
!                              ALL RIGHTS RESERVED
!

!
      SUBROUTINE ALLOCATE_SERVICE_COSTS(TYPE_OF_SERVICE,SERVICE_CHARGE,
     + SERVICE_COST_ASSIGNMENT,
     + SERVICE_EXPENSE_cln_loc,
     + SERVICE_REPORTING_GROUP)

      use servcom
      implicit none
      CHARACTER (len=1) ::  TYPE_OF_SERVICE,
     +            SERVICE_COST_ASSIGNMENT
      CHARACTER (len=3) :: SERVICE_EXPENSE_cln_loc
      INTEGER (kind=2) ::  SERVICE_REPORTING_GROUP,SERVICE_ITEM
      REAL (kind=4) ::  SERVICE_CHARGE

      IF(SERVICE_COST_ASSIGNMENT == 'R') THEN
         SERVICE_REVENUES = SERVICE_REVENUES + SERVICE_CHARGE
         IF(INDEX(SERVICE_EXPENSE_cln_loc,'Bas') /= 0) THEN
            SERVICE_BASE_REVENUE_OFFSET = SERVICE_BASE_REVENUE_OFFSET +
     +                                                   SERVICE_CHARGE
         ENDIF
         IF(INDEX(SERVICE_EXPENSE_cln_loc,'Adj') /= 0) THEN
            SERVICE_ADJ_CLAUSE_OFFSET = SERVICE_ADJ_CLAUSE_OFFSET +
     +                                                   SERVICE_CHARGE
         ENDIF
      ELSE

         SELECT CASE (TYPE_OF_SERVICE)
         CASE ('T')
           TRANSMISSION_CHARGES = TRANSMISSION_CHARGES + SERVICE_CHARGE
         CASE ('S')
           STAND_BY_TRANS_CHARGES=STAND_BY_TRANS_CHARGES+SERVICE_CHARGE
         CASE ('W')
            WHEELING_CHARGES = WHEELING_CHARGES + SERVICE_CHARGE
         CASE ('D')
            DISPATCHING_CHARGES = DISPATCHING_CHARGES + SERVICE_CHARGE
         CASE ('O')
           OTHER_SERVICE_CHARGES = OTHER_SERVICE_CHARGES+SERVICE_CHARGE
         END SELECT

!
         SERVICE_EXPENSES = SERVICE_EXPENSES + SERVICE_CHARGE
         IF(INDEX(SERVICE_EXPENSE_cln_loc,'Adj') /= 0) THEN
           SERVICE_ADJ_CLAUSE_EXPENSES = SERVICE_ADJ_CLAUSE_EXPENSES +
     +                                                   SERVICE_CHARGE
         ENDIF
         IF(INDEX(SERVICE_EXPENSE_cln_loc,'Bas') /= 0) THEN
             SERVICE_BASE_RATE_EXPENSES = SERVICE_BASE_RATE_EXPENSES +
     +                                                   SERVICE_CHARGE
         ENDIF
      ENDIF
      SERVICE_ITEM = INDEX('TSWDO',TYPE_OF_SERVICE)
      IF(SERVICE_ITEM /= 0 .AND.
     +       SERVICE_REPORTING_GROUP > 0  .AND.
     +              SERVICE_REPORTING_GROUP <= MAX_SERVICE_GROUPS) THEN
         SERVICE_GROUP_COSTS(SERVICE_ITEM,SERVICE_REPORTING_GROUP) =
     +         SERVICE_CHARGE +
     +        SERVICE_GROUP_COSTS(SERVICE_ITEM,SERVICE_REPORTING_GROUP)

         SERVICE_GROUP_COSTS(0,SERVICE_REPORTING_GROUP) =
     +                SERVICE_CHARGE +
     +                   SERVICE_GROUP_COSTS(0,SERVICE_REPORTING_GROUP)
      ENDIF
      RETURN

      END SUBROUTINE ALLOCATE_SERVICE_COSTS
      ! Transaction_manager subroutine is now in service_decs module

