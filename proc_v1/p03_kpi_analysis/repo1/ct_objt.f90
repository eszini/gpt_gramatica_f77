    
!     CT_OBJT.FOR
!     Copyright(c)  2000
!
!     Created: 1/29/2007 3:53:38 PM
!     Author : MARK S GERBER
!     Last change: MSG 1/29/2007 4:03:28 PM
    

      SUBROUTINE CT_OBJECT
      use end_routine,  only: end_program,  er_message
      use program_state
      use debugtrace
      use SpinDriftLib
      use prod_arrays_dimensions
      use sizecom
      use ft_interfaces
      use string
      use filemod
      integer :: file_trace_ct=0
      logical :: r_active_ct_record_initialized=.false.
      INTEGER(kind=2) :: IREC, INUNIT, DELETE, LRECL=256,  &
                ACTIVE_CT_RECORDS=0,  &
                R_ACTIVE_CT_RECORDS, R_MAX_CT_ID_NUM,  &
                MAX_CT_ID_NUM=0, MAX_BASE_CT_ID_NUM=0
      INTEGER :: IOS, UNIT_NO
      LOGICAL(kind=4) :: FILE_EXISTS
      CHARACTER(len=5) :: PUR_CONTRACT, OVERLAY_FAMILY_NAME
      CHARACTER(len=50) :: COMMENT
      CHARACTER(len=256) :: FILE_NAME
      CHARACTER(len=256) :: BASE_FILE_DIRECTORY, OUTPUT_DIRECTORY
!  DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER(len=1024) :: RECLN
!  DECLARATION FOR CONTRACTS DATA
      REAL ::  MINIMUM_ENERGY, MAXIMUM_ENERGY, MINIMUM_CAPACITY,  &
            MAXIMUM_CAPACITY, CONTRACT_VARIABLE_COST,  &
            MIN_CONTRACT_FIXED_COST, CONTRACT_FIXED_COST,  &
            CNTR_OWN, CNTR_POOL,  &
            MAX_RATCHET_PATTERN, CNTR_SO2, CNTR_CAP_PLAN_FACTOR,  &
            MIN_RATCHET_PATTERN, MAX_ANNUAL_ENERGY, MAX_ANNUAL_CAPACITY,  &
            SECOND_ENERGY_PRICE,  &
            ENERGY_COST_WEIGHTING_FACTOR,  &
            ENERGY_COST_ADDER,  &
            CT_ANNUAL_FIXED_COST
      INTEGER(kind=2) :: CNTR_ONLI_MO, CNTR_ONLI_YR,  &
            CNTR_OFLI_MO, CNTR_OFYR,  &
            CNTR_ENER_ESC, CNTR_MIN_CAP_ESC, CNTR_MAX_CAP_ESC,  &
            UPDATE_MONTH, CNTR_GROUP,  &
            SECOND_ENERGY_ESCALATOR,  &
            ENERGY_COST_ADDER_ESC_VECTOR,  &
            CT_ANNUAL_FIXED_COST_ESC
      INTEGER(kind=2) :: res_id_loc, ENERGY_PATTERN_VECTOR
      CHARACTER(len=1) :: CNTRTYPE, CNTR_EXP_ASSIGN,  &
                  CNTR_CAPACITY_SWITCH, CNTR_ENERGY_SWITCH
      CHARACTER(len=18) :: CNTR_EXP_COLLECT
      CHARACTER(len=20) :: INTRA_REVENUE_CLASS
      CHARACTER(len=20) :: CNTRNM
      CHARACTER(len=18) :: FILE_TYPE='Purchase Contracts'
      CHARACTER(len=2) :: CONTRACT_OL='BC'
      INTEGER(kind=2) :: ASSET_CLASS_NUM,  &
                ASSET_CLASS_VECTOR,  &
                INTRA_ASSET_CLASS_ID,  &
                INTRA_ASSET_CLASS_ALLOCATION
      CHARACTER(len=1) :: INTRA_COMPANY_TRANSACTION
      LOGICAL(kind=1) :: LAHEY_LF95
      CHARACTER(len=30) :: SCREEN_OUTPUT
      
       ! This routine is never called because reasons
       if(file_trace_ct==0) then
        file_trace_ct=open_trace("ct_objt.trace", rq_ct_objt)
        call write_trace_message(file_trace_ct, "tracing ct_object")
       end if
    
!
!           SUBROUTINE TO CONVERT METAFILE FILES TO DIRECT ACESS BINARY
!           COPYRIGHT (C) 1983,  84,  85  M.S. GERBER & ASSOCIATES,  INC.
!
    
!
!  CONVERT THE PURCHASE-CONTRACTS FILE
      ENTRY CT_MAKEBIN
!
       if(file_trace_ct==0) then
        file_trace_ct=open_trace("ct_objt.trace", rq_ct_objt)
        call write_trace_message(file_trace_ct, "tracing ct_object")
        call write_trace_message(file_trace_ct, "Opened in ct_makebin")
       end if
      FILE_NAME = trim(BASE_FILE_DIRECTORY())// &
                                   "CTB"//trim(PUR_CONTRACT())//".DAT"
      IREC = 0
      INQUIRE(FILE=FILE_NAME, EXIST=FILE_EXISTS)
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//PUR_CONTRACT()
            CALL MG_LOCATE_WRITE(16, 30, SCREEN_OUTPUT, ALL_VERSIONS, 0)
         ELSE
            CALL MG_CLEAR_LINE_WRITE(17, 9, 36, FILE_TYPE, ALL_VERSIONS, 0)
            CALL MG_LOCATE_WRITE(16, 30, PUR_CONTRACT(), ALL_VERSIONS, 0)
         ENDIF
         OPEN(10, FILE=FILE_NAME)
         OPEN(11, FILE=trim(OUTPUT_DIRECTORY())//"BCCNTCT.BIN",  &
                            ACCESS="DIRECT", STATUS="UNKNOWN", RECL=LRECL)
         MIN_RATCHET_PATTERN = 0
         MAX_RATCHET_PATTERN = 0
         MAX_ANNUAL_ENERGY = 99999999.
         UPDATE_MONTH = 1
         CNTR_GROUP = 0
         MAX_ANNUAL_CAPACITY = 9999.
         CNTR_ENERGY_SWITCH = 'M'

         ENERGY_PATTERN_VECTOR = 0
         SECOND_ENERGY_PRICE = 0.
         SECOND_ENERGY_ESCALATOR = 0
         ENERGY_COST_WEIGHTING_FACTOR = 0.
         ENERGY_COST_ADDER = 0.
         ENERGY_COST_ADDER_ESC_VECTOR = 0
         CT_ANNUAL_FIXED_COST = 0.
         CT_ANNUAL_FIXED_COST_ESC = 0
         CNTRNM = 'Contract File'
         CNTR_ONLI_YR = 2105
         ASSET_CLASS_NUM = 0
         ASSET_CLASS_VECTOR = 0
         INTRA_COMPANY_TRANSACTION = 'N'
         INTRA_ASSET_CLASS_ID = 0
         INTRA_ASSET_CLASS_ALLOCATION = 0
         INTRA_REVENUE_CLASS = 'Base Rates'

         READ(10, *) DELETE
         DO
            READ(10, 1000, IOSTAT=IOS) RECLN
            res_id_loc = IREC + 1
            IF(IOS /=0) EXIT
            RECLN = trim(RECLN)//', , , , , , , , , , , , , , , , '
            ! issue_102 - operative read
            READ(RECLN, *, ERR=200) DELETE, CNTRNM, CNTRTYPE,  &
               CNTR_EXP_ASSIGN, CNTR_EXP_COLLECT, CNTR_OWN, CNTR_POOL,  &
               CNTR_ONLI_MO, CNTR_ONLI_YR, CNTR_OFLI_MO, CNTR_OFYR,  &
               CNTR_SO2, CONTRACT_VARIABLE_COST, CNTR_ENER_ESC,  &
               MINIMUM_ENERGY, MAXIMUM_ENERGY, MIN_CONTRACT_FIXED_COST,  &
               CNTR_MIN_CAP_ESC, CONTRACT_FIXED_COST, CNTR_MAX_CAP_ESC,  &
               MINIMUM_CAPACITY, MAXIMUM_CAPACITY, CNTR_CAPACITY_SWITCH,  &
               MAX_RATCHET_PATTERN, CNTR_CAP_PLAN_FACTOR, COMMENT,  &
               MIN_RATCHET_PATTERN, MAX_ANNUAL_ENERGY, UPDATE_MONTH,  &
               CNTR_GROUP, MAX_ANNUAL_CAPACITY, CNTR_ENERGY_SWITCH,  &
               res_id_loc, ENERGY_PATTERN_VECTOR, SECOND_ENERGY_PRICE,  &
               SECOND_ENERGY_ESCALATOR, ENERGY_COST_WEIGHTING_FACTOR,  &
               ENERGY_COST_ADDER, ENERGY_COST_ADDER_ESC_VECTOR,  &
               CT_ANNUAL_FIXED_COST, CT_ANNUAL_FIXED_COST_ESC,  &
               ASSET_CLASS_NUM, ASSET_CLASS_VECTOR,  &
               INTRA_COMPANY_TRANSACTION, INTRA_ASSET_CLASS_ID,  &
               INTRA_ASSET_CLASS_ALLOCATION, INTRA_REVENUE_CLASS

!
            IREC = IREC + 1
            ! issue_102 - operative write
            WRITE(11, REC=IREC) DELETE, CNTRNM, CNTRTYPE, CNTR_EXP_ASSIGN,  &
               CNTR_EXP_COLLECT, CNTR_OWN, CNTR_POOL, CNTR_ONLI_MO,  &
               CNTR_ONLI_YR, CNTR_OFLI_MO, CNTR_OFYR, CNTR_SO2,  &
               CONTRACT_VARIABLE_COST, CNTR_ENER_ESC, MINIMUM_ENERGY,  &
               MAXIMUM_ENERGY, MIN_CONTRACT_FIXED_COST, CNTR_MIN_CAP_ESC, &
               CONTRACT_FIXED_COST, CNTR_MAX_CAP_ESC, MINIMUM_CAPACITY,  &
               MAXIMUM_CAPACITY, CNTR_CAPACITY_SWITCH,  &
               MAX_RATCHET_PATTERN, CNTR_CAP_PLAN_FACTOR,  &
               MIN_RATCHET_PATTERN, MAX_ANNUAL_ENERGY, UPDATE_MONTH,  &
               CNTR_GROUP, MAX_ANNUAL_CAPACITY, CNTR_ENERGY_SWITCH,  &
               res_id_loc, ENERGY_PATTERN_VECTOR, SECOND_ENERGY_PRICE,  &
               SECOND_ENERGY_ESCALATOR, ENERGY_COST_WEIGHTING_FACTOR,  &
               ENERGY_COST_ADDER, ENERGY_COST_ADDER_ESC_VECTOR,  &
               CT_ANNUAL_FIXED_COST, CT_ANNUAL_FIXED_COST_ESC,  &
               ASSET_CLASS_NUM, ASSET_CLASS_VECTOR,  &
               INTRA_COMPANY_TRANSACTION, INTRA_ASSET_CLASS_ID,  &
               INTRA_ASSET_CLASS_ALLOCATION, INTRA_REVENUE_CLASS
!
            MAX_CT_ID_NUM = MAX(MAX_CT_ID_NUM, res_id_loc)

         ENDDO
         CLOSE(10)
         CLOSE(11)
      ELSE IF(INDEX(PUR_CONTRACT(), 'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE, FILE_NAME,  "CT_OBJT:0001")
      ENDIF
      ACTIVE_CT_RECORDS = IREC
      MAX_BASE_CT_ID_NUM = MAX_CT_ID_NUM
      RETURN
!  OVERLAY THE PURCHASE-CONTRACTS FILE
      ENTRY CT_MAKEOVL(OVERLAY_FAMILY_NAME)
             if(file_trace_ct==0) then
        file_trace_ct=open_trace("ct_objt.trace", rq_ct_objt)
        call write_trace_message(file_trace_ct, "tracing ct_object")
        call write_trace_message(file_trace_ct, "Opened in ct_makeovl")
       end if
      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16, 30, SCREEN_OUTPUT, ALL_VERSIONS, 0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17, 9, 36, FILE_TYPE, ALL_VERSIONS, 0)
         CALL LOCATE(10, 51)
      ENDIF
      FILE_NAME=trim(OUTPUT_DIRECTORY())//"CTO"// &
                                     trim(OVERLAY_FAMILY_NAME)//".DAT"
      OPEN(10, FILE=FILE_NAME)
      READ(10, *) DELETE
      INUNIT = 12
      IF(CONTRACT_OL == 'BC' ) THEN
         OPEN(11, FILE=trim(OUTPUT_DIRECTORY())//"BCCNTCT.BIN",  &
                                             ACCESS="DIRECT", RECL=LRECL)
         INUNIT = 11
      ENDIF
      OPEN(12, FILE=trim(OUTPUT_DIRECTORY())//"OLCNTCT.BIN",  &
                            ACCESS='DIRECT', STATUS="UNKNOWN", RECL=LRECL)
      IREC = 0
      MAX_CT_ID_NUM = 0
      DO
         IREC = IREC + 1
         ! issue_102 - operative read
         READ(INUNIT, REC=IREC, IOSTAT=IOS) DELETE, CNTRNM, CNTRTYPE,  &
             CNTR_EXP_ASSIGN, CNTR_EXP_COLLECT, CNTR_OWN, CNTR_POOL,  &
             CNTR_ONLI_MO, CNTR_ONLI_YR, CNTR_OFLI_MO, CNTR_OFYR,  &
             CNTR_SO2, CONTRACT_VARIABLE_COST, CNTR_ENER_ESC,  &
             MINIMUM_ENERGY, MAXIMUM_ENERGY, MIN_CONTRACT_FIXED_COST,  &
             CNTR_MIN_CAP_ESC, CONTRACT_FIXED_COST, CNTR_MAX_CAP_ESC,  &
             MINIMUM_CAPACITY, MAXIMUM_CAPACITY, CNTR_CAPACITY_SWITCH,  &
             MAX_RATCHET_PATTERN, CNTR_CAP_PLAN_FACTOR,  &
             MIN_RATCHET_PATTERN, MAX_ANNUAL_ENERGY, UPDATE_MONTH,  &
             CNTR_GROUP, MAX_ANNUAL_CAPACITY, CNTR_ENERGY_SWITCH,  &
             res_id_loc, ENERGY_PATTERN_VECTOR, SECOND_ENERGY_PRICE,  &
             SECOND_ENERGY_ESCALATOR, ENERGY_COST_WEIGHTING_FACTOR,  &
             ENERGY_COST_ADDER, ENERGY_COST_ADDER_ESC_VECTOR,  &
             CT_ANNUAL_FIXED_COST, CT_ANNUAL_FIXED_COST_ESC ,  &
             ASSET_CLASS_NUM, ASSET_CLASS_VECTOR,  &
             INTRA_COMPANY_TRANSACTION, INTRA_ASSET_CLASS_ID,  &
             INTRA_ASSET_CLASS_ALLOCATION, INTRA_REVENUE_CLASS
         IF(IOS /= 0) EXIT

         READ(10, 1000, IOSTAT=IOS) RECLN
         IF(IOS == 0) THEN
            RECLN = trim(RECLN)//', , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , '
            ! issue_102 - operative read
            READ(RECLN, *, ERR=200) DELETE, CNTRNM, CNTRTYPE,  &
               CNTR_EXP_ASSIGN, CNTR_EXP_COLLECT, CNTR_OWN, CNTR_POOL,  &
               CNTR_ONLI_MO, CNTR_ONLI_YR, CNTR_OFLI_MO, CNTR_OFYR,  &
               CNTR_SO2, CONTRACT_VARIABLE_COST, CNTR_ENER_ESC,  &
               MINIMUM_ENERGY, MAXIMUM_ENERGY,  MIN_CONTRACT_FIXED_COST,  &
               CNTR_MIN_CAP_ESC, CONTRACT_FIXED_COST, CNTR_MAX_CAP_ESC,  &
               MINIMUM_CAPACITY, MAXIMUM_CAPACITY, CNTR_CAPACITY_SWITCH,  &
               MAX_RATCHET_PATTERN, CNTR_CAP_PLAN_FACTOR, COMMENT,  &
               MIN_RATCHET_PATTERN, MAX_ANNUAL_ENERGY, UPDATE_MONTH,  &
               CNTR_GROUP, MAX_ANNUAL_CAPACITY, CNTR_ENERGY_SWITCH,  &
               res_id_loc, ENERGY_PATTERN_VECTOR, SECOND_ENERGY_PRICE,  &
               SECOND_ENERGY_ESCALATOR, ENERGY_COST_WEIGHTING_FACTOR,  &
               ENERGY_COST_ADDER, ENERGY_COST_ADDER_ESC_VECTOR,  &
               CT_ANNUAL_FIXED_COST, CT_ANNUAL_FIXED_COST_ESC ,  &
               ASSET_CLASS_NUM, ASSET_CLASS_VECTOR,  &
               INTRA_COMPANY_TRANSACTION, INTRA_ASSET_CLASS_ID,  &
               INTRA_ASSET_CLASS_ALLOCATION, INTRA_REVENUE_CLASS


         ENDIF
         ! issue_102 - operative write
         
         WRITE(12, REC=IREC) DELETE, CNTRNM, CNTRTYPE, CNTR_EXP_ASSIGN,  &
            CNTR_EXP_COLLECT, CNTR_OWN, CNTR_POOL, CNTR_ONLI_MO,  &
            CNTR_ONLI_YR, CNTR_OFLI_MO, CNTR_OFYR, CNTR_SO2,  &
            CONTRACT_VARIABLE_COST, CNTR_ENER_ESC, MINIMUM_ENERGY,  &
            MAXIMUM_ENERGY, MIN_CONTRACT_FIXED_COST, CNTR_MIN_CAP_ESC,  &
            CONTRACT_FIXED_COST, CNTR_MAX_CAP_ESC, MINIMUM_CAPACITY,  &
            MAXIMUM_CAPACITY, CNTR_CAPACITY_SWITCH, MAX_RATCHET_PATTERN,  &
            CNTR_CAP_PLAN_FACTOR, MIN_RATCHET_PATTERN, MAX_ANNUAL_ENERGY, &
            UPDATE_MONTH, CNTR_GROUP, MAX_ANNUAL_CAPACITY,  &
            CNTR_ENERGY_SWITCH, res_id_loc, ENERGY_PATTERN_VECTOR,  &
            SECOND_ENERGY_PRICE, SECOND_ENERGY_ESCALATOR,  &
            ENERGY_COST_WEIGHTING_FACTOR, ENERGY_COST_ADDER,  &
            ENERGY_COST_ADDER_ESC_VECTOR, CT_ANNUAL_FIXED_COST,  &
            CT_ANNUAL_FIXED_COST_ESC , ASSET_CLASS_NUM,  &
            ASSET_CLASS_VECTOR, INTRA_COMPANY_TRANSACTION,  &
            INTRA_ASSET_CLASS_ID, INTRA_ASSET_CLASS_ALLOCATION,  &
            INTRA_REVENUE_CLASS

            MAX_CT_ID_NUM = MAX(MAX_CT_ID_NUM, res_id_loc)
            

          
      ENDDO
      CLOSE(10)
      CLOSE(12)
      IF(CONTRACT_OL == 'BC') CLOSE(11)
      CONTRACT_OL = 'OL'
      RETURN

  200 CALL MG_LOCATE_WRITE(20, 0, trim(RECLN), ALL_VERSIONS, 1)
      er_message='stop requested from CT_OBJT SIID72'
      call end_program(er_message)

      ENTRY RESET_CONTRACT_OL
       if(file_trace_ct==0) then
        file_trace_ct=open_trace("ct_objt.trace", rq_ct_objt)
        call write_trace_message(file_trace_ct, "tracing ct_object")
        call write_trace_message(file_trace_ct, "reset_contract_ol")
       end if
         CONTRACT_OL = 'BC'
         MAX_CT_ID_NUM = MAX_BASE_CT_ID_NUM
      RETURN

      ENTRY OPEN_CT_FILE(UNIT_NO, R_ACTIVE_CT_RECORDS, R_MAX_CT_ID_NUM)
       if(file_trace_ct==0) then
        file_trace_ct=open_trace("ct_objt.trace", rq_ct_objt)
        call write_trace_message(file_trace_ct, "tracing ct_object")
        call write_trace_message(file_trace_ct, "Opened in open_ct_file")
       end if

         ! Initialize on first call
         if(.not. r_active_ct_record_initialized) then
            r_active_ct_record_initialized=.true.
            R_ACTIVE_CT_RECORDS=0
            R_MAX_CT_ID_NUM=0
         endif
         call write_trace_int2(file_trace_ct, "R_ACTIVE_CT_RECORDS", &
            R_ACTIVE_CT_RECORDS)
         call write_trace_int2(file_trace_ct, "R_MAX_CT_ID_NUM", &
            R_MAX_CT_ID_NUM)
         call write_trace_int2(file_trace_ct, "ACTIVE_CT_RECORDS", &
            ACTIVE_CT_RECORDS)
            
         IF(ACTIVE_CT_RECORDS > 0) THEN
            OPEN(UNIT_NO, FILE=trim(OUTPUT_DIRECTORY())// &
                    CONTRACT_OL//"CNTCT.BIN", ACCESS="DIRECT", RECL=LRECL)
         ENDIF
         R_ACTIVE_CT_RECORDS = MAX(ACTIVE_CT_RECORDS, INT(0, 2))
         R_MAX_CT_ID_NUM = MAX(MAX_CT_ID_NUM, INT(0, 2))
         call write_trace_int2(file_trace_ct, "1. R_ACTIVE_CT_RECORDS", &
            R_ACTIVE_CT_RECORDS)
            
      RETURN
!
 1000 FORMAT(A)
 1010 FORMAT('&', A)
      END subroutine CT_OBJECT
    
!
!     READS PURCHASE CONTRACTS BINARY DATA FILES
      RECURSIVE FUNCTION CONTRACTS_READ()
      use end_routine,  only: end_program,  er_message
      use annual_contracts
      use contracts_data
      use globecom
      USE SIZECOM
      use SpinDriftLib
      use prod_arrays_dimensions
      use kootenay
      use mod_base_year
      use contracts_data
      use string
      use filemod
      use program_state
      use debugtrace
      implicit none
      SAVE

      integer  :: file_trace_cr=0
      INTEGER(kind=2) :: I, IREC, DELETE, CONTRACTS_READ, YEAR_START,  &
                        YEAR_END
      INTEGER :: IOS, R_CAP_TYPE
      INTEGER(kind=2) :: R_YEAR
      REAL :: CT_PLANNING_CAPACITY, CT_DENOM_LOAD_REDUCTION
      INTEGER(kind=2) :: CT_CAPACITY_PLANNING_ADDITIONS, R_I, VOID_I2
      LOGICAL(kind=1) :: CT_OPTION_CHECK, RESOURCE_DOES_NOT_REDUCE_LOAD, &
                SAVE_CT_LOAD_REDUCTION
      INTEGER(kind=2) :: R_CONTRACT_NUM
!      CHARACTER(len=20) :: MONTH_NAME
!
      INTEGER(kind=2) :: ADDED_CONTRACT_PROGRAMS(*),  &
                CONTRACT_OPTIONS_ADDED
!
!      VARIABLES FOR CAPACITY PLANNING
!
      INTEGER(kind=2) :: YR
      INTEGER(kind=2) :: PEAK_MONTH, CT_RECORDS, MAX_CT_ID_NUM
      REAL :: EN_MW, GET_CT_MW_FROM_POINTR
      INTEGER(kind=2) :: ON_LINE_MONTH(:), ON_LINE_YEAR(:),  &
                OFF_LINE_MONTH(:), OFF_LINE_YEAR(:),  &
                CT_ID_TO_RECORD_POINTER(:)
      INTEGER(kind=2) :: R_CONTRACT_START_YEAR, R_CONTRACT_PERIOD,  &
                ACCEPTANCE_NUM, R_CT_ID_NUM
      REAL :: CT_ANN_CAP(:, :), CT_ANNUAL_LOAD_REDUCTION(:)
      ALLOCATABLE :: CT_ANN_CAP, CT_ANNUAL_LOAD_REDUCTION,  &
                     ON_LINE_MONTH, ON_LINE_YEAR,  &
                     OFF_LINE_MONTH, OFF_LINE_YEAR
      REAL :: CONTRACT_PEAK_ADJUSTMENT
      REAL :: CONTRACT_ADJUSTMENTS_TO_PEAK(:)
      ALLOCATABLE :: CONTRACT_ADJUSTMENTS_TO_PEAK,  &
                     CT_ID_TO_RECORD_POINTER
      INTEGER(kind=2) :: RESET_CONTRACT_ADDITIONS,  &
                RESET_CONTRACT_COSTS
      REAL :: ADD_NEW_CT_UNIT
!
      REAL :: SAVE_CT_FIRST_ENERGY_PRICE(:),  &
           SAVE_MIN_CONTRACT_FIXED_COST(:),  &
           SAVE_CONTRACT_FIXED_COST(:),  &
           SAVE_CT_SECOND_ENERGY_PRICE(:),  &
           SAVE_CT_ENERGY_COST_ADDER(:),  &
           SAVE_CT_ANNUAL_FIXED_COST(:)
      ALLOCATABLE :: SAVE_CT_FIRST_ENERGY_PRICE,  &
                     SAVE_MIN_CONTRACT_FIXED_COST,  &
                     SAVE_CONTRACT_FIXED_COST,  &
                     SAVE_CT_SECOND_ENERGY_PRICE,  &
                     SAVE_CT_ENERGY_COST_ADDER,  &
                     SAVE_CT_ANNUAL_FIXED_COST
!
      INTEGER(kind=2) :: ASSET_CLASS_NUM(:),  &
                ASSET_CLASS_VECTOR(:),  &
                INTRA_ASSET_CLASS_ID(:),  &
                INTRA_ASSET_CLASS_ALLOCATION(:)
      CHARACTER(len=20) :: INTRA_REVENUE_CLASS(:)
      CHARACTER(len=1) :: INTRA_COMPANY_TRANSACTION(:)
      ALLOCATABLE :: ASSET_CLASS_NUM,  &
                     ASSET_CLASS_VECTOR,  &
                     INTRA_ASSET_CLASS_ID,  &
                     INTRA_ASSET_CLASS_ALLOCATION,  &
                     INTRA_COMPANY_TRANSACTION,  &
                     INTRA_REVENUE_CLASS
!
!  ASSET ALLOCATION STUFF
!
      INTEGER(kind=2) :: CLASS_NUM
      INTEGER(kind=2) :: NUMBER_OF_CONTRACT_CLASSES,  &
      	       MAX_CONTRACT_CLASS_ID_NUM,  &
                CONTRACT_ASSET_CLASS_POINTER(:)
      ALLOCATABLE :: CONTRACT_ASSET_CLASS_POINTER
      REAL(kind=4) :: CONTRACT_BUY_CAPACITY(:),  &
             CONTRACT_BUY_ENERGY(:),  &
             CONTRACT_BUY_SO2_EMISSIONS(:),  &
             CONTRACT_BUY_FIXED_COST(:),  &
             CONTRACT_BUY_VARIABLE_COST(:),  &
             CONTRACT_INTRA_CAPACITY(:),  &
             CONTRACT_INTRA_ENERGY(:),  &
             CONTRACT_INTRA_SO2_EMISSIONS(:),  &
             CONTRACT_SELLING_REVENUES(:, :),  &
             CONTRACT_BTL_EXPENSES(:),  &
             CONTRACT_PURCHASE_EXPENSES(:),  &
             CONTRACT_ADJ_CLAUSE_COLLECTIONS(:),  &
             CONTRACT_SECONDARY_SALES_REVS(:)
      ALLOCATABLE :: CONTRACT_BUY_CAPACITY,  &
                     CONTRACT_BUY_ENERGY,  &
                     CONTRACT_BUY_SO2_EMISSIONS,  &
                     CONTRACT_BUY_FIXED_COST,  &
                     CONTRACT_BUY_VARIABLE_COST,  &
                     CONTRACT_INTRA_CAPACITY,  &
                     CONTRACT_INTRA_ENERGY,  &
                     CONTRACT_INTRA_SO2_EMISSIONS,  &
                     CONTRACT_SELLING_REVENUES,  &
                     CONTRACT_BTL_EXPENSES,  &
                     CONTRACT_PURCHASE_EXPENSES,  &
                     CONTRACT_ADJ_CLAUSE_COLLECTIONS,  &
                     CONTRACT_SECONDARY_SALES_REVS
      INTEGER(kind=4) :: VALUES_2_ZERO
      CHARACTER(len=1) :: DUMMY_TYPE
      INTEGER(kind=2) :: ASSET_CLASS_LIST(AVAIL_DATA_YEARS),  &
                CLASS_POINTER, ASSET_CLASS
      REAL(kind=4) :: ASSET_ALLOCATION_LIST(AVAIL_DATA_YEARS),  &
                ASSET_ALLOCATOR
      LOGICAL(kind=1) :: CALC_CONTRACT_CLASS_INFO, REALLY_KEPCO
      INTEGER(kind=2) :: REV_TYPE, INCOME_STATEMENT_POSITION
      REAL(kind=4) :: CNTR_WEIGHTED_CAPACITY(*)
!
      REAL(kind=4) :: R_FIXED_COST,  &
             R_VAR_COST, R_PURCHASE_COST, R_BTL_EXPENSES,  &
             R_ADJ_CLAUSE_COLLECTION, R_SECONDAY_SALES_REVENUE
      INTEGER(kind=2) :: RETURN_CONTRACT_CLASS_INFO, R_CLASS_NUM
      LOGICAL(kind=1) :: RETURN_CONTRACT_INTRA_INFO
      

      CALL OPEN_CT_FILE(10, CT_RECORDS, MAX_CT_ID_NUM)
      IF(CT_RECORDS < 1) THEN
         CONTRACTS_READ = 0
         RETURN
      ENDIF
      IF(ALLOCATED(CONTRACT_ADJUSTMENTS_TO_PEAK)) THEN
         DEALLOCATE(CONTRACT_ADJUSTMENTS_TO_PEAK,  &
                    ON_LINE_MONTH, ON_LINE_YEAR,  &
                    OFF_LINE_MONTH, OFF_LINE_YEAR,  &
                    CT_ANN_CAP,  &
                    CT_ID_TO_RECORD_POINTER,  &
                    SAVE_CT_FIRST_ENERGY_PRICE,  &
                    SAVE_MIN_CONTRACT_FIXED_COST,  &
                    SAVE_CONTRACT_FIXED_COST,  &
                    SAVE_CT_SECOND_ENERGY_PRICE,  &
                    SAVE_CT_ENERGY_COST_ADDER,  &
                    SAVE_CT_ANNUAL_FIXED_COST,  &
                    CT_ANNUAL_LOAD_REDUCTION,  &
                    ASSET_CLASS_NUM,  &
                    ASSET_CLASS_VECTOR,  &
                    INTRA_ASSET_CLASS_ID,  &
                    INTRA_ASSET_CLASS_ALLOCATION,  &
                    INTRA_REVENUE_CLASS,  &
                    INTRA_COMPANY_TRANSACTION,  &
                    CONTRACT_ASSET_CLASS_POINTER)
      ENDIF
      ALLOCATE(CONTRACT_ADJUSTMENTS_TO_PEAK(get_globecom_study_period()),  &
               ON_LINE_MONTH(CT_RECORDS), ON_LINE_YEAR(CT_RECORDS),  &
               OFF_LINE_MONTH(CT_RECORDS), OFF_LINE_YEAR(CT_RECORDS),  &
               CT_ANN_CAP(3, get_globecom_study_period()),  &
               CT_ID_TO_RECORD_POINTER(MAX_CT_ID_NUM),  &
               SAVE_CT_FIRST_ENERGY_PRICE(CT_RECORDS),  &
               SAVE_MIN_CONTRACT_FIXED_COST(CT_RECORDS),  &
               SAVE_CONTRACT_FIXED_COST(CT_RECORDS),  &
               SAVE_CT_SECOND_ENERGY_PRICE(CT_RECORDS),  &
               SAVE_CT_ENERGY_COST_ADDER(CT_RECORDS),  &
               SAVE_CT_ANNUAL_FIXED_COST(CT_RECORDS),  &
               CT_ANNUAL_LOAD_REDUCTION(get_globecom_study_period()),  &
               ASSET_CLASS_NUM(CT_RECORDS),  &
               ASSET_CLASS_VECTOR(CT_RECORDS),  &
               INTRA_ASSET_CLASS_ID(CT_RECORDS),  &
               INTRA_ASSET_CLASS_ALLOCATION(CT_RECORDS),  &
               INTRA_COMPANY_TRANSACTION(CT_RECORDS),  &
               INTRA_REVENUE_CLASS(CT_RECORDS),  &
               CONTRACT_ASSET_CLASS_POINTER(1024))
               
      if (file_trace_cr==0) then
        file_trace_cr=open_trace("CONTRACTS_READ.trace", rq_contract_read)
        call write_trace_message(file_trace_cr, &
            "tracing contracts_read routine.")
      end if
      CONTRACT_ADJUSTMENTS_TO_PEAK = 0.
      CT_ANN_CAP = 0.
      CT_ANNUAL_LOAD_REDUCTION = 0.
      CT_ID_TO_RECORD_POINTER = 0
      CONTRACT_ASSET_CLASS_POINTER = 0
!
      CT_CUM_1ST_ENERGY_COST = 1.
!
      RESOURCE_DOES_NOT_REDUCE_LOAD = SAVE_CT_LOAD_REDUCTION(.TRUE.)
      NUMBER_OF_CONTRACT_CLASSES = 0
      MAX_CONTRACT_CLASS_ID_NUM = 0
      IREC = 0
      I = 1
      DOWHILE (I <= CT_RECORDS)
         IREC = IREC + 1

         READ(10, REC=IREC, IOSTAT=IOS) DELETE, CNTRNM(I), CNTRTYPE(I),  &
          CNTR_EXP_ASSIGN(I), CNTR_EXP_COLLECT(I), CNTR_OWN(I),  &
          CNTR_POOL(I), ON_LINE_MONTH(I), ON_LINE_YEAR(I),  &
          OFF_LINE_MONTH(I), OFF_LINE_YEAR(I),  CNTR_SO2(I),  &
          CT_FIRST_ENERGY_PRICE(I), CNTR_ENER_ESC(I), MIN_ENERGY(I),  &
          MAX_ENERGY(I), MIN_CONTRACT_FIXED_COST(I), CNTR_MIN_CAP_ESC(I), &
          CONTRACT_FIXED_COST(I), CNTR_CAP_ESC(I), MIN_CAPACITY(I),  &
          MAX_CAPACITY(I), CNTR_CAPACITY_SWITCH(I),  &
          MAX_RATCHET_PATTERN(I), CNTR_CAP_PLAN_FACTOR(I),  &
          MIN_RATCHET_PATTERN(I), MAX_ANNUAL_ENERGY(I),  &
          UPDATE_MONTH(I), CNTR_GROUP(I), MAX_ANNUAL_CAPACITY(I),  &
          CNTR_ENERGY_SWITCH(I), CT_RESOURCE_ID(I),  &
          CT_ENERGY_PATTERN_VECTOR(I), CT_SECOND_ENERGY_PRICE(I),  &
          CT_SECOND_ENERGY_ESCALATOR(I),  &
          CT_ENERGY_COST_WEIGHTING_FACTOR(I), CT_ENERGY_COST_ADDER(I),  &
          CT_ENERGY_COST_ADDER_ESC_VECTOR(I), CT_ANNUAL_FIXED_COST(I),  &
          CT_ANNUAL_FIXED_COST_ESC(I), ASSET_CLASS_NUM(I),  &
          ASSET_CLASS_VECTOR(I), INTRA_COMPANY_TRANSACTION(I),  &
          INTRA_ASSET_CLASS_ID(I), INTRA_ASSET_CLASS_ALLOCATION(I),  &
          INTRA_REVENUE_CLASS(I)
         IF(IOS /= 0) EXIT

          
          
         IF(DELETE > 7 .OR. OFF_LINE_YEAR(I) <= get_BASE_YEAR()) CYCLE
!
         CLASS_NUM = ASSET_CLASS_NUM(I)
         CALL SET_ASSET_CLASSES(CLASS_NUM,  &
                                NUMBER_OF_CONTRACT_CLASSES,  &
      	                       MAX_CONTRACT_CLASS_ID_NUM,  &
                                CONTRACT_ASSET_CLASS_POINTER)
         IF(INTRA_COMPANY_TRANSACTION(I) == 'Y') THEN

            CLASS_NUM = INTRA_ASSET_CLASS_ID(I)
            CALL SET_ASSET_CLASSES(CLASS_NUM,  &
                                   NUMBER_OF_CONTRACT_CLASSES,  &
      	                          MAX_CONTRACT_CLASS_ID_NUM,  &
                                   CONTRACT_ASSET_CLASS_POINTER)
         ENDIF
         SAVE_CT_FIRST_ENERGY_PRICE(I) = CT_FIRST_ENERGY_PRICE(I)
         SAVE_MIN_CONTRACT_FIXED_COST(I) = MIN_CONTRACT_FIXED_COST(I)
         SAVE_CONTRACT_FIXED_COST(I) = CONTRACT_FIXED_COST(I)
         SAVE_CT_SECOND_ENERGY_PRICE(I) = CT_SECOND_ENERGY_PRICE(I)
         SAVE_CT_ENERGY_COST_ADDER(I) = CT_ENERGY_COST_ADDER(I)
         SAVE_CT_ANNUAL_FIXED_COST(I) = CT_ANNUAL_FIXED_COST(I)
         CT_ID_TO_RECORD_POINTER(CT_RESOURCE_ID(I)) = I
         ON_LINE_YEAR(I) = &
               MIN(MAX(ON_LINE_YEAR(I), INT(1901, 2)), INT(2200, 2))
         OFF_LINE_YEAR(I) =  MIN(OFF_LINE_YEAR(I), INT(2200, 2))
         CNTR_ON_LI(I) = 100*(ON_LINE_YEAR(I)-1900) + ON_LINE_MONTH(I)
         CNTR_OFF_LI(I) = 100*(OFF_LINE_YEAR(I)-1900)+OFF_LINE_MONTH(I)
         IF(CT_FIRST_ENERGY_PRICE(I) >= 0.) &
            CT_MONTHLY_1ST_ENERGY_PRICE(I) = CT_FIRST_ENERGY_PRICE(I)
         IF(CNTR_OWN(I) > 1.) CNTR_OWN(I) = CNTR_OWN(I)/100.
         IF(CNTR_GROUP(I) < 0 .OR. &
             CNTR_GROUP(I) > MAX_REPORTING_GROUPS) CNTR_GROUP(I) = 0
!
         IF((CNTR_CAPACITY_SWITCH(I) == "R" .AND. &
                               MAX_RATCHET_PATTERN(I) >= 0) .OR. &
            (CNTR_CAPACITY_SWITCH(I) /= "R" .AND. &
                                     MAX_RATCHET_PATTERN(I) < 0. ) )THEN
            WRITE(4, *) "Contract ",  trim(CNTRNM(I)),  &
                       " must have both an 'R' capacity switch and a",  &
                       " negative pointer for the MAX RATCHET PATTERN."
         ENDIF
         IF((CNTR_CAPACITY_SWITCH(I) == "N" .AND. &
                                  MIN_RATCHET_PATTERN(I) >= 0) .OR. &
                 (CNTR_CAPACITY_SWITCH(I) /= "N" .AND. &
                                     MIN_RATCHET_PATTERN(I) < 0. ) )THEN

         ENDIF

         IF(ON_LINE_YEAR(I) <= OFF_LINE_YEAR(I)   .AND. &
               ON_LINE_YEAR(I) - get_BASE_YEAR() <= get_globecom_study_period() .AND. &
                                      OFF_LINE_YEAR(I) > get_BASE_YEAR()) THEN
            YEAR_START = ON_LINE_YEAR(I) - get_BASE_YEAR()
            IF(YEAR_START < 1) THEN
               YEAR_START = 1
            ELSE
               IF(ON_LINE_MONTH(I) > PEAK_MONTH(YEAR_START)) THEN
                  YEAR_START = YEAR_START + 1
               ENDIF
            ENDIF
            IF(OFF_LINE_YEAR(I) > gc_last_study_year) THEN
               YEAR_END = gc_last_study_year - get_BASE_YEAR()
            ELSE
               YEAR_END = OFF_LINE_YEAR(I) - get_BASE_YEAR()
               IF(OFF_LINE_MONTH(I) < PEAK_MONTH(YEAR_END)) THEN
                  YEAR_END = YEAR_END - 1
               ENDIF
            ENDIF

            DO YR = YEAR_START,  YEAR_END

               EN_MW = MAX_CAPACITY(I)
               call write_trace_real(file_trace_cr, "1. EN_MW", EN_MW)
               
               IF(EN_MW < 0.) THEN
                  EN_MW = GET_CT_MW_FROM_POINTR(EN_MW, YR)
                  call write_trace_int2(file_trace_cr, "YR", YR)
                 call write_trace_real(file_trace_cr, "2. EN_MW", EN_MW)

               ELSE
!
!  IF NO POINTER HAS BEEN SPECIFIED FOR THE PEAK MONTH
!
                  EN_MW = MAX(0., EN_MW)
                 call write_trace_real(file_trace_cr, "2. EN_MW", EN_MW)
               ENDIF
!
!  EFFECTIVE MW'S OF PLANNING CAPACITY
!
               EN_MW = EN_MW * CNTR_OWN(I) * CNTR_CAP_PLAN_FACTOR(I)
               call write_trace_real(file_trace_cr, "3. EN_MW", EN_MW)
!
!  THIS SECTION ADDS MW'S TO THE SYSTEM
               call write_trace_int2(file_trace_cr, "I", I)
               call write_trace_string(file_trace_cr, &
               "CNTRTYPE(I)", CNTRTYPE(I))
               
               IF(EN_MW < 0.) THEN
                  call write_trace_message(file_trace_cr, &
                    "setting contract adjustments to peak.")
                    
                  CONTRACT_ADJUSTMENTS_TO_PEAK(YR) = ABS(EN_MW) + &
                                     CONTRACT_ADJUSTMENTS_TO_PEAK(YR)
               ELSEIF(CNTRTYPE(I) == 'B' ) THEN 

                  CT_ANN_CAP(1, YR) = CT_ANN_CAP(1, YR) + EN_MW
                  CT_ANN_CAP(2, YR) = CT_ANN_CAP(2, YR) + EN_MW
                  CT_ANN_CAP(3, YR) = CT_ANN_CAP(3, YR) + EN_MW
               ELSEIF(CNTRTYPE(I) == 'C' ) THEN
                  CT_ANN_CAP(2, YR) = CT_ANN_CAP(2, YR) + EN_MW
                  CT_ANN_CAP(3, YR) = CT_ANN_CAP(3, YR) + EN_MW
               ELSEIF(CNTRTYPE(I) == 'P') THEN
                  CT_ANN_CAP(3, YR) = CT_ANN_CAP(3, YR) + EN_MW
               ELSEIF(CNTRTYPE(I) == 'D') THEN
                  RESOURCE_DOES_NOT_REDUCE_LOAD = &
                              SAVE_CT_LOAD_REDUCTION(.FALSE.)
                  CT_ANNUAL_LOAD_REDUCTION(YR) = &
                              CT_ANNUAL_LOAD_REDUCTION(YR) + EN_MW
               ENDIF
            ENDDO
!
!  END OF CAPACITY PLANNING SECTION
!
         ENDIF
!
!
         call write_trace_int2(file_trace_cr, "2. I", I)
         call write_trace_int2(file_trace_cr, "MAX_CONTRACTS", &
            MAX_CONTRACTS)
            
         I = I + 1
         IF(I > MAX_CONTRACTS) THEN

           WRITE(SCREEN_MESSAGES, '(A, I2, A)')'More than ', MAX_CONTRACTS, &
                                              ' purchase contracts'
           CALL MG_LOCATE_WRITE(20, 0, trim(SCREEN_MESSAGES),  &
                                 ALL_VERSIONS, 1)
           CALL MG_LOCATE_WRITE(21, 0,  &
                  'are in the purchase contracts file.', ALL_VERSIONS, 1)
           er_message='stop requested from CT_OBJT SIID73'
           call end_program(er_message)
         ENDIF
      ENDDO
      CLOSE(10)
      I = I - 1
      CONTRACTS_READ = MIN(I, CT_RECORDS)
      call write_trace_int2(file_trace_cr, "CONTRACTS_READ", &
        CONTRACTS_READ)

      IF(ALLOCATED(CONTRACT_BUY_CAPACITY)) &
                           DEALLOCATE(CONTRACT_BUY_CAPACITY,  &
                                      CONTRACT_BUY_ENERGY,  &
                                      CONTRACT_BUY_SO2_EMISSIONS,  &
                                      CONTRACT_BUY_FIXED_COST,  &
                                      CONTRACT_BUY_VARIABLE_COST,  &
                                      CONTRACT_INTRA_CAPACITY,  &
                                      CONTRACT_INTRA_ENERGY,  &
                                      CONTRACT_INTRA_SO2_EMISSIONS,  &
                                      CONTRACT_SELLING_REVENUES,  &
                                      CONTRACT_BTL_EXPENSES,  &
                                      CONTRACT_PURCHASE_EXPENSES,  &
                                      CONTRACT_ADJ_CLAUSE_COLLECTIONS,  &
                                      CONTRACT_SECONDARY_SALES_REVS)
      ALLOCATE(CONTRACT_BUY_CAPACITY(0:MAX_CONTRACT_CLASS_ID_NUM),  &
            CONTRACT_BUY_ENERGY(0:MAX_CONTRACT_CLASS_ID_NUM),  &
            CONTRACT_BUY_SO2_EMISSIONS(0:MAX_CONTRACT_CLASS_ID_NUM),  &
            CONTRACT_BUY_FIXED_COST(-1:MAX_CONTRACT_CLASS_ID_NUM),  &
            CONTRACT_BUY_VARIABLE_COST(-1:MAX_CONTRACT_CLASS_ID_NUM),  &
            CONTRACT_INTRA_CAPACITY(0:MAX_CONTRACT_CLASS_ID_NUM),  &
            CONTRACT_INTRA_ENERGY(0:MAX_CONTRACT_CLASS_ID_NUM),  &
            CONTRACT_INTRA_SO2_EMISSIONS(0:MAX_CONTRACT_CLASS_ID_NUM), &
            CONTRACT_SELLING_REVENUES(-1:MAX_CONTRACT_CLASS_ID_NUM, 20), &
            CONTRACT_BTL_EXPENSES(-1:MAX_CONTRACT_CLASS_ID_NUM),  &
            CONTRACT_PURCHASE_EXPENSES(-1:MAX_CONTRACT_CLASS_ID_NUM),  &
            CONTRACT_ADJ_CLAUSE_COLLECTIONS &
                                        (-1:MAX_CONTRACT_CLASS_ID_NUM), &
            CONTRACT_SECONDARY_SALES_REVS &
                                         (-1:MAX_CONTRACT_CLASS_ID_NUM))
           call write_trace_message(file_trace_cr, "1. RETURN")
      RETURN
    
      ENTRY CT_CAPACITY_PLANNING_ADDITIONS(R_I)
    
!
!  BEGINING OF THE CAPACITY PLANNING SECTION
         call write_trace_message(file_trace_cr, &
            "CT_CAPACITY_PLANNING_ADDITIONS()")
         call write_trace_int2(file_trace_cr, "3. I", I)
         call write_trace_int2(file_trace_cr, "R_I", R_I)
         
         I = CT_ID_TO_RECORD_POINTER(R_I)
         CT_CAPACITY_PLANNING_ADDITIONS = 0
         call write_trace_int2(file_trace_cr, "ON_LINE_YEAR(I))", &
            ON_LINE_YEAR(I))
            
         IF(ON_LINE_YEAR(I) <= OFF_LINE_YEAR(I)   .AND. &
               ON_LINE_YEAR(I) - get_BASE_YEAR() <= get_globecom_study_period() .AND. &
                                      OFF_LINE_YEAR(I) > get_BASE_YEAR()) THEN
            YEAR_START = ON_LINE_YEAR(I) - get_BASE_YEAR()
            IF(YEAR_START < 1) THEN
               YEAR_START = 1
            ELSE
               IF(ON_LINE_MONTH(I) > PEAK_MONTH(YEAR_START)) THEN
                  YEAR_START = YEAR_START + 1
               ENDIF
            ENDIF
            IF(OFF_LINE_YEAR(I) > gc_last_study_year) THEN
               YEAR_END = gc_last_study_year - get_BASE_YEAR()
            ELSE
               YEAR_END = OFF_LINE_YEAR(I) - get_BASE_YEAR()
               IF(OFF_LINE_MONTH(I) < PEAK_MONTH(YEAR_END)) THEN
                  YEAR_END = YEAR_END - 1
               ENDIF
            ENDIF
            DO YR = YEAR_START,  YEAR_END
               EN_MW = MAX_CAPACITY(I)
               call write_trace_real(file_trace_cr, "4. EN_MW", EN_MW)
               IF(EN_MW < 0.) THEN
                  EN_MW = GET_CT_MW_FROM_POINTR(EN_MW,YR)
               call write_trace_real(file_trace_cr, "5. EN_MW", EN_MW)
               ELSE
!
!  IF NO POINTER HAS BEEN SPECIFIED FOR THE PEAK MONTH
                 call write_trace_real(file_trace_cr, "6. EN_MW", EN_MW)
                  EN_MW = MAX(0.,EN_MW)
                  call write_trace_real(file_trace_cr, "6b. EN_MW", EN_MW)
               ENDIF
!
!  EFFECTIVE MW'S OF PLANNING CAPACITY
!
               EN_MW = EN_MW * CNTR_OWN(I) * CNTR_CAP_PLAN_FACTOR(I)
               call write_trace_real(file_trace_cr, "7. EN_MW", EN_MW)
               IF(YR == YEAR_START) CT_CAPACITY_PLANNING_ADDITIONS = &
                                                     MIN(EN_MW, 30000.)
!
!  THIS SECTION ADDS MW'S TO THE SYSTEM
               call write_trace_real(file_trace_cr, "8. EN_MW", EN_MW)
               IF(EN_MW < 0.) THEN
                  CONTRACT_ADJUSTMENTS_TO_PEAK(YR) = ABS(EN_MW) + &
                                     CONTRACT_ADJUSTMENTS_TO_PEAK(YR)
               ELSEIF(CNTRTYPE(I) == 'B' ) THEN
                  CT_ANN_CAP(1, YR) = CT_ANN_CAP(1, YR) + EN_MW
                  CT_ANN_CAP(2, YR) = CT_ANN_CAP(2, YR) + EN_MW
                  CT_ANN_CAP(3, YR) = CT_ANN_CAP(3, YR) + EN_MW
               ELSEIF(CNTRTYPE(I) == 'C' ) THEN
                  CT_ANN_CAP(2, YR) = CT_ANN_CAP(2, YR) + EN_MW
                  CT_ANN_CAP(3, YR) = CT_ANN_CAP(3, YR) + EN_MW
               ELSEIF(CNTRTYPE(I) == 'P') THEN
                  CT_ANN_CAP(3, YR) = CT_ANN_CAP(3, YR) + EN_MW
               ELSEIF(CNTRTYPE(I) == 'D') THEN
                  RESOURCE_DOES_NOT_REDUCE_LOAD = &
                              SAVE_CT_LOAD_REDUCTION(.FALSE.)
                  CT_ANNUAL_LOAD_REDUCTION(YR) = &
                              CT_ANNUAL_LOAD_REDUCTION(YR) + EN_MW
               ENDIF
            ENDDO
!
!  END OF CAPACITY PLANNING SECTION
!
         ENDIF
      RETURN
    
      ENTRY CONTRACT_PEAK_ADJUSTMENT(R_YEAR)
    
         IF(CT_RECORDS == 0) THEN
           CONTRACT_PEAK_ADJUSTMENT = 0.0
         ELSEIF(R_YEAR > get_globecom_study_period()) THEN
           CONTRACT_PEAK_ADJUSTMENT = &
                              CONTRACT_ADJUSTMENTS_TO_PEAK(get_globecom_study_period())
         ELSE
           CONTRACT_PEAK_ADJUSTMENT = &
                                    CONTRACT_ADJUSTMENTS_TO_PEAK(R_YEAR)
         ENDIF
      RETURN
    
      ENTRY CT_PLANNING_CAPACITY(R_CAP_TYPE, R_YEAR)
    
         IF(CT_RECORDS == 0) THEN
            CT_PLANNING_CAPACITY = 0.0
         ELSEIF(R_YEAR > get_globecom_study_period()) THEN
            CT_PLANNING_CAPACITY = CT_ANN_CAP(R_CAP_TYPE, get_globecom_study_period())
         ELSE
            CT_PLANNING_CAPACITY = CT_ANN_CAP(R_CAP_TYPE, R_YEAR)
         ENDIF
      RETURN
    
      ENTRY CT_DENOM_LOAD_REDUCTION(R_YEAR)
    
         IF(CT_RECORDS == 0 .OR. RESOURCE_DOES_NOT_REDUCE_LOAD) THEN
            CT_DENOM_LOAD_REDUCTION = 0.0
         ELSEIF(R_YEAR > get_globecom_study_period()) THEN
            CT_DENOM_LOAD_REDUCTION = &
                        CT_ANNUAL_LOAD_REDUCTION(get_globecom_study_period())
         ELSE
            CT_DENOM_LOAD_REDUCTION = &
                        CT_ANNUAL_LOAD_REDUCTION(R_YEAR)
         ENDIF
      RETURN
    
      ENTRY ADD_NEW_CT_UNIT(R_CONTRACT_START_YEAR, R_CT_ID_NUM,  &
                            R_CONTRACT_PERIOD)
    
        I = CT_ID_TO_RECORD_POINTER(R_CT_ID_NUM)
        ON_LINE_YEAR(I) = &
           MIN(MAX(R_CONTRACT_START_YEAR, INT(1901, 2)), INT(2200, 2))
        OFF_LINE_YEAR(I) = MIN(R_CONTRACT_START_YEAR+R_CONTRACT_PERIOD, &
                                                           INT(2200, 2))
        CNTR_ON_LI(I) = 100*(ON_LINE_YEAR(I)-1900) + ON_LINE_MONTH(I)
        CNTR_OFF_LI(I) = 100*(OFF_LINE_YEAR(I)-1900)+OFF_LINE_MONTH(I)
        ADD_NEW_CT_UNIT = ABS(MAX_CAPACITY(I))
      RETURN
    
      ENTRY RESET_CONTRACT_ADDITIONS(ADDED_CONTRACT_PROGRAMS,  &
                               CONTRACT_OPTIONS_ADDED)
    
         DO I = 1,  CONTRACT_OPTIONS_ADDED
            ACCEPTANCE_NUM = ADDED_CONTRACT_PROGRAMS(I)
            IF(ACCEPTANCE_NUM < 1) EXIT
            ACCEPTANCE_NUM = CT_ID_TO_RECORD_POINTER(ACCEPTANCE_NUM)
            ON_LINE_YEAR(ACCEPTANCE_NUM) = 2200
            OFF_LINE_YEAR(ACCEPTANCE_NUM) = 2200
            CNTR_ON_LI(ACCEPTANCE_NUM) = 30000
            CNTR_OFF_LI(ACCEPTANCE_NUM) = 30000
         ENDDO
         RESET_CONTRACT_ADDITIONS = 0
    
      ENTRY RESET_CONTRACT_COSTS
    
         DO I = 1,  CT_RECORDS
            CT_FIRST_ENERGY_PRICE(I) = SAVE_CT_FIRST_ENERGY_PRICE(I)
            MIN_CONTRACT_FIXED_COST(I) = SAVE_MIN_CONTRACT_FIXED_COST(I)
            CONTRACT_FIXED_COST(I) = SAVE_CONTRACT_FIXED_COST(I)
            CT_SECOND_ENERGY_PRICE(I) = SAVE_CT_SECOND_ENERGY_PRICE(I)
            CT_ENERGY_COST_ADDER(I) = SAVE_CT_ENERGY_COST_ADDER(I)
            CT_ANNUAL_FIXED_COST(I) = SAVE_CT_ANNUAL_FIXED_COST(I)
         ENDDO
         RESET_CONTRACT_COSTS = 0
      RETURN
    
      ENTRY CT_OPTION_CHECK(R_CONTRACT_NUM)
    
        CT_OPTION_CHECK = .FALSE.
        IF(R_CONTRACT_NUM > 0 .AND. &
                               R_CONTRACT_NUM <= MAX_CT_ID_NUM) THEN
           IF(CT_ID_TO_RECORD_POINTER(R_CONTRACT_NUM) > 0) THEN
              IF(ON_LINE_YEAR(CT_ID_TO_RECORD_POINTER(R_CONTRACT_NUM))>&
                                                  gc_last_study_year) THEN
                 CT_OPTION_CHECK = .TRUE.
              ENDIF
           ENDIF
        ENDIF
      RETURN
    
      ENTRY CALC_CONTRACT_CLASS_INFO(CNTR_WEIGHTED_CAPACITY,  &
                                                           REALLY_KEPCO)
    
!
!

         CALC_CONTRACT_CLASS_INFO = .TRUE.
         IF(NUMBER_OF_CONTRACTS == 0) RETURN
!
         CONTRACT_BUY_CAPACITY = 0.
         CONTRACT_BUY_ENERGY = 0.
         CONTRACT_BUY_SO2_EMISSIONS = 0.
         CONTRACT_INTRA_CAPACITY = 0.
         CONTRACT_INTRA_ENERGY = 0.
         CONTRACT_INTRA_SO2_EMISSIONS = 0.
!
         CONTRACT_BUY_FIXED_COST = 0.
         CONTRACT_BUY_VARIABLE_COST = 0.
         CONTRACT_BTL_EXPENSES = 0.
         CONTRACT_PURCHASE_EXPENSES = 0.
         CONTRACT_ADJ_CLAUSE_COLLECTIONS = 0.
         CONTRACT_SECONDARY_SALES_REVS = 0.
!
         CONTRACT_SELLING_REVENUES = 0.
!
         DO I = 1,  NUMBER_OF_CONTRACTS
            IF(AINT(CNTR_ON_LI(I)/100.)+1900-get_BASE_YEAR() <= globecom_YEAR .AND. &
                  AINT(CNTR_OFF_LI(I)/100.)+1900-get_BASE_YEAR() >= globecom_YEAR) THEN
!
               IF(INTRA_COMPANY_TRANSACTION(I) == 'Y') THEN
                  ASSET_CLASS = INTRA_ASSET_CLASS_ID(I)
         	      IF(ASSET_CLASS < 0.) THEN
         	         CALL GET_ASSET_VAR(ABS(ASSET_CLASS),  &
         	                                DUMMY_TYPE, ASSET_CLASS_LIST)
         	         CALL GET_ASSET_VAR( &
              	                 ABS(INTRA_ASSET_CLASS_ALLOCATION(I)),  &
         	                           DUMMY_TYPE, ASSET_ALLOCATION_LIST)
         	      ELSE
         	         ASSET_CLASS_LIST(1) = ASSET_CLASS
         	         ASSET_CLASS_LIST(2) = 0
         	         ASSET_ALLOCATION_LIST(1) = 100.
         	         ASSET_ALLOCATION_LIST(2) = 0.
         	      ENDIF
!
         	      CLASS_POINTER = 1
                  REV_TYPE = &
                       INCOME_STATEMENT_POSITION(INTRA_REVENUE_CLASS(I))
         	      DO
         	       ASSET_CLASS = ASSET_CLASS_LIST(CLASS_POINTER)
         	       CALL CHECK_IF_CLASS_DEFINED(ASSET_CLASS)
         	       ASSET_CLASS = ASSET_CLASS + 1
                   ASSET_ALLOCATOR = &
                             ASSET_ALLOCATION_LIST(CLASS_POINTER)/100.
!
                   CONTRACT_INTRA_CAPACITY(ASSET_CLASS) = &
                           CNTR_WEIGHTED_CAPACITY(I)*ASSET_ALLOCATOR + &
                                  CONTRACT_INTRA_CAPACITY(ASSET_CLASS)
                   CONTRACT_INTRA_ENERGY(ASSET_CLASS) = &
                           ANNUAL_CONTRACT_ENERGY(I)*ASSET_ALLOCATOR + &
                                    CONTRACT_INTRA_ENERGY(ASSET_CLASS)
                   CONTRACT_INTRA_SO2_EMISSIONS(ASSET_CLASS) = &
                     ANNUAL_CONTRACT_VARIABLE_COST(I)*ASSET_ALLOCATOR+ &
                             CONTRACT_INTRA_SO2_EMISSIONS(ASSET_CLASS)
                   CONTRACT_SELLING_REVENUES(ASSET_CLASS, REV_TYPE) = &
                     CONTRACT_SELLING_REVENUES(ASSET_CLASS, REV_TYPE) + &
                              ASSET_ALLOCATOR * &
                                 (ANNUAL_CONTRACT_VARIABLE_COST(I) + &
                                        ANNUAL_CONTRACT_FIXED_COST(I))
                   CONTRACT_SELLING_REVENUES(-1, REV_TYPE) = &
                              CONTRACT_SELLING_REVENUES(-1, REV_TYPE) + &
                              ASSET_ALLOCATOR * &
                                 (ANNUAL_CONTRACT_VARIABLE_COST(I) + &
                                        ANNUAL_CONTRACT_FIXED_COST(I))
!
                   IF(CNTR_EXP_COLLECT(I) == 'N') THEN
                        CONTRACT_BTL_EXPENSES(-1) = &
                               CONTRACT_BTL_EXPENSES(-1) + &
                               ASSET_ALLOCATOR * &
                                   (ANNUAL_CONTRACT_VARIABLE_COST(I) + &
                                          ANNUAL_CONTRACT_FIXED_COST(I))
                     ELSEIF(CNTR_EXP_ASSIGN(I) == 'P') THEN
                        CONTRACT_PURCHASE_EXPENSES(-1) = &
                               CONTRACT_PURCHASE_EXPENSES(-1) + &
                               ASSET_ALLOCATOR * &
                                   (ANNUAL_CONTRACT_VARIABLE_COST(I) + &
                                          ANNUAL_CONTRACT_FIXED_COST(I))
                     ELSE
                      CONTRACT_BUY_FIXED_COST(-1) = ASSET_ALLOCATOR * &
                                       ANNUAL_CONTRACT_FIXED_COST(I) + &
                                       CONTRACT_BUY_FIXED_COST(-1)
                      CONTRACT_BUY_VARIABLE_COST(-1)=ASSET_ALLOCATOR * &
                                    ANNUAL_CONTRACT_VARIABLE_COST(I) + &
                                    CONTRACT_BUY_VARIABLE_COST(-1)
                     ENDIF
!
         	         CLASS_POINTER = CLASS_POINTER + 1
         	         IF(ASSET_CLASS_LIST(CLASS_POINTER) <= 0 .OR. &
         	                      CLASS_POINTER > AVAIL_DATA_YEARS) EXIT
                  ENDDO
!
               ENDIF ! END INTRA COMPANY TRANSACTION
!
               ASSET_CLASS = ASSET_CLASS_NUM(I)
!
               IF(ASSET_CLASS < 0) THEN
                  CALL GET_ASSET_VAR(ABS(ASSET_CLASS),  &
                                       DUMMY_TYPE, ASSET_CLASS_LIST)
                  CALL GET_ASSET_VAR(ABS(ASSET_CLASS_VECTOR(I)),  &
                                       DUMMY_TYPE, ASSET_ALLOCATION_LIST)
               ELSE
                  ASSET_CLASS_LIST(1) = ASSET_CLASS
                  ASSET_CLASS_LIST(2) = 0.
                  ASSET_ALLOCATION_LIST(1) = 100.
                  ASSET_ALLOCATION_LIST(2) = 0.
               ENDIF
               CLASS_POINTER = 1
!
               DO
                  ASSET_CLASS = ASSET_CLASS_LIST(CLASS_POINTER)
                  CALL CHECK_IF_CLASS_DEFINED(ASSET_CLASS)
                  ASSET_CLASS = ASSET_CLASS + 1
                  ASSET_ALLOCATOR = &
                               ASSET_ALLOCATION_LIST(CLASS_POINTER)/100.
!
                  IF(INDEX('S', CNTRTYPE(I)) /= 0 .OR. &
                      (.NOT. get_WEST_KOOTENAY_POWER_active() .AND. &
                                      INDEX('X', CNTRTYPE(I)) /= 0)) THEN
                     CONTRACT_SECONDARY_SALES_REVS(ASSET_CLASS) = &
                         CONTRACT_SECONDARY_SALES_REVS(ASSET_CLASS) + &
                               ASSET_ALLOCATOR * &
                                   (ANNUAL_CONTRACT_VARIABLE_COST(I))

                  ELSEIF(REALLY_KEPCO .AND. &
                                       INDEX('M', CNTRTYPE(I)) /= 0) THEN
!                     FOR KEPCO MAINTENANCE ENERGY IS IN WC MAINTENANCE
                  ELSE
                   CONTRACT_BUY_CAPACITY(ASSET_CLASS) = &
                           CNTR_WEIGHTED_CAPACITY(I)*ASSET_ALLOCATOR + &
                                    CONTRACT_BUY_CAPACITY(ASSET_CLASS)
                   CONTRACT_BUY_ENERGY(ASSET_CLASS) = &
                           ANNUAL_CONTRACT_ENERGY(I)*ASSET_ALLOCATOR + &
                                      CONTRACT_BUY_ENERGY(ASSET_CLASS)
                   CONTRACT_BUY_SO2_EMISSIONS(ASSET_CLASS) = &
                     ANNUAL_CONTRACT_VARIABLE_COST(I)*ASSET_ALLOCATOR+ &
                               CONTRACT_BUY_SO2_EMISSIONS(ASSET_CLASS)
!
                   IF(CNTR_EXP_COLLECT(I)(1:1) == 'N' .OR. &
                           INDEX(CNTR_EXP_COLLECT(I), 'BTL') /= 0) THEN
                      CONTRACT_BTL_EXPENSES(ASSET_CLASS) = &
                             CONTRACT_BTL_EXPENSES(ASSET_CLASS) + &
                             ASSET_ALLOCATOR * &
                                 (ANNUAL_CONTRACT_VARIABLE_COST(I) + &
                                        ANNUAL_CONTRACT_FIXED_COST(I))
                   ELSE
                      IF(CNTR_EXP_ASSIGN(I) == 'P') THEN
                         CONTRACT_PURCHASE_EXPENSES(ASSET_CLASS) = &
                             CONTRACT_PURCHASE_EXPENSES(ASSET_CLASS) + &
                             ASSET_ALLOCATOR * &
                                 (ANNUAL_CONTRACT_VARIABLE_COST(I) + &
                                        ANNUAL_CONTRACT_FIXED_COST(I))
                      ELSE
                         CONTRACT_BUY_FIXED_COST(ASSET_CLASS) = &
                            ASSET_ALLOCATOR * &
                              ANNUAL_CONTRACT_FIXED_COST(I) + &
                                  CONTRACT_BUY_FIXED_COST(ASSET_CLASS)
                         CONTRACT_BUY_VARIABLE_COST(ASSET_CLASS) = &
                            ASSET_ALLOCATOR * &
                              ANNUAL_CONTRACT_VARIABLE_COST(I) + &
                               CONTRACT_BUY_VARIABLE_COST(ASSET_CLASS)
                      ENDIF
                      IF(CNTR_EXP_COLLECT(I)(1:1) == 'A') THEN
                         CONTRACT_ADJ_CLAUSE_COLLECTIONS(ASSET_CLASS)= &
                         CONTRACT_ADJ_CLAUSE_COLLECTIONS(ASSET_CLASS)+ &
                             ASSET_ALLOCATOR * &
                                 (ANNUAL_CONTRACT_VARIABLE_COST(I) + &
                                        ANNUAL_CONTRACT_FIXED_COST(I))
                      ENDIF
                   ENDIF
                  ENDIF
!
                  CLASS_POINTER = CLASS_POINTER + 1
                  IF(CLASS_POINTER > AVAIL_DATA_YEARS) EXIT
                  IF(ASSET_CLASS_LIST(CLASS_POINTER) == 0. .OR. &
                            ASSET_CLASS_LIST(CLASS_POINTER) ==-99.) EXIT
               ENDDO ! ASSET CLASSES
            ENDIF ! CONTRACT ACTIVE
         ENDDO
      RETURN
    
      ENTRY RETURN_CONTRACT_CLASS_INFO(R_CLASS_NUM,  &
                                       R_FIXED_COST,  &
                                       R_VAR_COST,  &
                                       R_PURCHASE_COST,  &
                                       R_BTL_EXPENSES,  &
                                       R_ADJ_CLAUSE_COLLECTION,  &
                                       R_SECONDAY_SALES_REVENUE)
    
!
         RETURN_CONTRACT_CLASS_INFO = NUMBER_OF_CONTRACTS
         IF(NUMBER_OF_CONTRACTS == 0) RETURN
!
         IF(R_CLASS_NUM <= MAX_CONTRACT_CLASS_ID_NUM .AND. &
                                                  R_CLASS_NUM >= 0) THEN
            R_FIXED_COST = R_FIXED_COST + &
                                    CONTRACT_BUY_FIXED_COST(R_CLASS_NUM)
            R_VAR_COST = R_VAR_COST + &
                                 CONTRACT_BUY_VARIABLE_COST(R_CLASS_NUM)
            R_PURCHASE_COST = R_PURCHASE_COST + &
                                 CONTRACT_PURCHASE_EXPENSES(R_CLASS_NUM)
            R_BTL_EXPENSES = R_BTL_EXPENSES + &
                                      CONTRACT_BTL_EXPENSES(R_CLASS_NUM)
            R_ADJ_CLAUSE_COLLECTION = R_ADJ_CLAUSE_COLLECTION + &
                            CONTRACT_ADJ_CLAUSE_COLLECTIONS(R_CLASS_NUM)
            R_SECONDAY_SALES_REVENUE = R_SECONDAY_SALES_REVENUE + &
                              CONTRACT_SECONDARY_SALES_REVS(R_CLASS_NUM)
         ENDIF
      RETURN
    
      ENTRY RETURN_CONTRACT_INTRA_INFO(R_FIXED_COST,  &
                                       R_VAR_COST,  &
                                       R_PURCHASE_COST,  &
                                       R_BTL_EXPENSES)
    
!
!
         RETURN_CONTRACT_INTRA_INFO = .TRUE.
         IF(NUMBER_OF_CONTRACTS == 0) RETURN
!
         R_FIXED_COST = R_FIXED_COST + CONTRACT_BUY_FIXED_COST(-1)
         R_VAR_COST = R_VAR_COST + CONTRACT_BUY_VARIABLE_COST(-1)
         R_PURCHASE_COST = R_PURCHASE_COST + &
                                          CONTRACT_PURCHASE_EXPENSES(-1)
         R_BTL_EXPENSES = R_BTL_EXPENSES + CONTRACT_BTL_EXPENSES(-1)
         RETURN
      END function CONTRACTS_READ

