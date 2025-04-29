
!     CO_OBJT.FOR
!     Copyright(c)  2000
!
!     Created: 11/8/2006 2:10:56 PM
!     Author : MARK S GERBER
!     Last change: MSG 1/10/2010 3:01:47 PM


      RECURSIVE SUBROUTINE CO_OBJECT
      use end_routine, only: end_program, er_message

      use grx_planning_routines
      USE SIZECOM
      use spindriftlib
      use prod_arrays_dimensions
            use end_routine, only: end_program, er_message
      use arrays1
      use prod_arrays_dimensions
      use sizecom
      use lamcom
      use spindriftlib
      use mod_base_year
      implicit none
      SAVE ! All variables static
!
      INTEGER (kind=2) ::  IREC,DELETE,LRECL=20 ,YEAR
      INTEGER ::  IOS
      LOGICAL (kind=4) ::  FILE_EXISTS
      CHARACTER (len=5) ::  ALLOCATOR_FILE,OVERLAY_FAMILY_NAME
      CHARACTER (len=15) ::  FILE_TYPE='Cost Allocators' 
      CHARACTER (len=256) ::  FILE_NAME,COST_ALLOC_OL*2='BC' 
      CHARACTER (len=256) ::  BASE_FILE_DIRECTORY,OUTPUT_DIRECTORY
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER (len=1024) ::  RECLN
      INTEGER (kind=2) ::  MAX_YRS_loc,YR,I
! DECLARATION FOR ALLOCATION VARIABLES
      REAL ::  DEMAND_ALLOCATOR(:)
      REAL ::  ENERGY_ALLOCATOR(:)
      REAL ::  CUSTOMER_ALLOCATOR(:)
      REAL ::  SYSTEM_LOSSES(:)
      REAL ::  R_DEMAND_ALLOCATOR
      REAL ::  R_ENERGY_ALLOCATOR
      REAL ::  R_CUSTOMER_ALLOCATOR
      REAL ::  R_SYSTEM_LOSSES
      ALLOCATABLE :: DEMAND_ALLOCATOR,ENERGY_ALLOCATOR,
     +               CUSTOMER_ALLOCATOR,SYSTEM_LOSSES
      LOGICAL (kind=1) ::  LAHEY_LF95
      CHARACTER (len=30) ::  SCREEN_OUTPUT


!
!         Routine TO CONVERT METAFILE FILES TO DIRECT ACESS BINARY
!          COPYRIGHT (C) 1983, 84, 85  M.S. GERBER & ASSOCIATES, INC.

! CONVERT COST ALLOCATION FILE
      ENTRY CO_MAKEBIN
         allocate(DEMAND_ALLOCATOR(AVAIL_DATA_YEARS), stat=ios)
         call check_alloc("co_objt:0002", "demand_allocator", ios)

         DEMAND_ALLOCATOR=0
         
         ALLOCATE(
     +            ENERGY_ALLOCATOR(AVAIL_DATA_YEARS),
     +            CUSTOMER_ALLOCATOR(AVAIL_DATA_YEARS),
     +            SYSTEM_LOSSES(AVAIL_DATA_YEARS))
         FILE_NAME = trim(BASE_FILE_DIRECTORY())//
     +                           "COB"//trim(ALLOCATOR_FILE())//".DAT"
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         IF(FILE_EXISTS) THEN
            OPEN(10,FILE=FILE_NAME)
            READ(10,*) DELETE
         ELSEIF(INDEX(ALLOCATOR_FILE(),'NONE') == 0) THEN
            CALL STOP_NOFILE(FILE_TYPE,ALLOCATOR_FILE(), "co_objt:0001")
         ENDIF
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCCSTAL.BIN",
     +                     ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         IREC = 1
         DEMAND_ALLOCATOR(1) = 0.
         ENERGY_ALLOCATOR(1) = 100.
         CUSTOMER_ALLOCATOR(1) = 0.
         SYSTEM_LOSSES(1) = 0.
         IF(FILE_EXISTS) THEN
            IF(LAHEY_LF95()) THEN
               SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//ALLOCATOR_FILE()
               CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
            ELSE
               CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,
     +                                                  ALL_VERSIONS,0)
               CALL MG_LOCATE_WRITE(16,30,ALLOCATOR_FILE(),
     +                                                  ALL_VERSIONS,0)
            ENDIF
!
            DO
               READ(10,1000,IOSTAT=IOS) RECLN
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,'
               IF(IOS /= 0) EXIT
               READ(RECLN,*,ERR=200) DELETE,YEAR,DEMAND_ALLOCATOR(1),
     +                   ENERGY_ALLOCATOR(1),CUSTOMER_ALLOCATOR(1),
     +                   SYSTEM_LOSSES(1)
               IF(DELETE > 7) CYCLE
               WRITE(11,REC=IREC) DEMAND_ALLOCATOR(1),
     +                            ENERGY_ALLOCATOR(1),
     +                            CUSTOMER_ALLOCATOR(1),
     +                            SYSTEM_LOSSES(1)
               IREC = IREC + 1
            ENDDO
            CLOSE(10)
         ENDIF
         DOWHILE (IREC <= AVAIL_DATA_YEARS)
            WRITE(11,REC=IREC) DEMAND_ALLOCATOR(1),
     +                         ENERGY_ALLOCATOR(1),
     +                         CUSTOMER_ALLOCATOR(1),
     +                         SYSTEM_LOSSES(1)
            IREC = IREC + 1
         ENDDO
         CLOSE(11)
         CALL COST_SERVICE_ALLOCATORS
      RETURN
!
! OVERLAY THE COST ALLOCATION FILE
!
      ENTRY CO_MAKEOVL(OVERLAY_FAMILY_NAME)
!
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
            CALL LOCATE(10,51)
         ENDIF
         FILE_NAME = trim(OUTPUT_DIRECTORY())//"COO"//
     +                               trim(OVERLAY_FAMILY_NAME)//".DAT"
         OPEN(10,FILE=FILE_NAME)
         READ(10,*) DELETE
         DO I = 1, MAX_YRS_loc-1
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(IOS /= 0) EXIT
            RECLN = trim(RECLN)//',,,,,,,,,,,,,'
            READ(RECLN,*,ERR=200) DELETE,YEAR,DEMAND_ALLOCATOR(I),
     +                    ENERGY_ALLOCATOR(I),CUSTOMER_ALLOCATOR(I),
     +                    SYSTEM_LOSSES(I)
         ENDDO
         CLOSE(10)
         COST_ALLOC_OL = 'OL'
      RETURN

  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from CO_OBJT SIID68'
      call end_program(er_message)
!
      ENTRY RESET_COST_ALLOC_OL

         COST_ALLOC_OL = 'BC'
      RETURN



!*       COST_SERVICE_ALLOCATORS READS AND STORES THE REVENUE          *
!*       ALLOCATION FACTORS FOR DEMAND, ENERGY, CUSTOMERS              *




      ENTRY COST_SERVICE_ALLOCATORS

      FILE_NAME = trim(OUTPUT_DIRECTORY())//"BCCSTAL.BIN"
      OPEN(10,FILE=FILE_NAME,ACCESS="DIRECT",RECL=LRECL)
      DO I = 1, AVAIL_DATA_YEARS
         READ(10,REC=I) DEMAND_ALLOCATOR(I),ENERGY_ALLOCATOR(I),
     +                  CUSTOMER_ALLOCATOR(I),SYSTEM_LOSSES(I)
      ENDDO
      CLOSE(10)
      RETURN
!
      ENTRY RETURN_COST_SERVICE_ALLOCATORS(YR,R_DEMAND_ALLOCATOR,
     +                                        R_ENERGY_ALLOCATOR,
     +                                        R_CUSTOMER_ALLOCATOR)
!
         IF(YR > AVAIL_DATA_YEARS) THEN
            R_DEMAND_ALLOCATOR = DEMAND_ALLOCATOR(AVAIL_DATA_YEARS)
            R_ENERGY_ALLOCATOR = ENERGY_ALLOCATOR(AVAIL_DATA_YEARS)
            R_CUSTOMER_ALLOCATOR = CUSTOMER_ALLOCATOR(AVAIL_DATA_YEARS)
         ELSE
            R_DEMAND_ALLOCATOR = DEMAND_ALLOCATOR(YR)
            R_ENERGY_ALLOCATOR = ENERGY_ALLOCATOR(YR)
            R_CUSTOMER_ALLOCATOR = CUSTOMER_ALLOCATOR(YR)
         ENDIF
         IF(R_DEMAND_ALLOCATOR > 1.0) R_DEMAND_ALLOCATOR =
     +                                          R_DEMAND_ALLOCATOR/100.
         IF(R_ENERGY_ALLOCATOR > 1.0) R_ENERGY_ALLOCATOR =
     +                                          R_ENERGY_ALLOCATOR/100.
         IF(R_CUSTOMER_ALLOCATOR > 1.0) R_CUSTOMER_ALLOCATOR =
     +                                        R_CUSTOMER_ALLOCATOR/100.
      RETURN
!
      ENTRY RETURN_SYSTEM_LOSSES(YR,R_SYSTEM_LOSSES)
         IF(YR > AVAIL_DATA_YEARS) THEN
            R_SYSTEM_LOSSES = SYSTEM_LOSSES(AVAIL_DATA_YEARS)
         ELSE
            R_SYSTEM_LOSSES = SYSTEM_LOSSES(YR)
         ENDIF
         IF(R_SYSTEM_LOSSES > 1.0) R_SYSTEM_LOSSES=R_SYSTEM_LOSSES/100.
      RETURN
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END

      SUBROUTINE COST_OF_SERVICE
      use spindriftlib
      use prod_arrays_dimensions
      USE IREC_ENDPOINT_CONTROL
      USE FINANCIAL_SWITCHES_COMMON
      use grx_planning_routines
      use cls_load
      USE SIZECOM
      use globecom, only: globecom_year
      use mod_base_year
      use spindriftlib
      use prod_arrays_dimensions
      use irec_endpoint_control
      use financial_switches_common
      use sizecom
      use arrays1
      use lamcom
      use endpoint
      use parms
      use forecast
      use retslcom
      use enrg_helper
      use retslcom

      
!
      LOGICAL (kind=1) ::  REVENUES_FROM_CLASS_PRICES
      REAL ::  REVENUE=0. ,R_REVENUE,DENOM
      REAL ::  CLASS_PEAK(SYSTEM_CLASS_NUM)
      REAL ::  ANNUAL_PEAK
      REAL ::  GET_INPUT_SYSTEM_PEAK
      REAL ::  R_CLASS_RATES(*)
      REAL ::  TOTAL_PEAK
      REAL ::  TOTAL_MWH
      REAL ::  TOTAL_CUSTOMERS
      REAL ::  DEMAND_ALLOCATOR
      REAL ::  ENERGY_ALLOCATOR
      REAL ::  CUSTOMER_ALLOCATOR
      REAL ::  CLASS_COSTS_DEMAND(SYSTEM_CLASS_NUM)
      REAL ::  CLASS_COSTS_ENERGY(SYSTEM_CLASS_NUM)
      REAL ::  CLASS_COSTS_CUSTOMER(SYSTEM_CLASS_NUM)
      REAL ::  ALLOC_ENERGY
      REAL ::  ALLOC_DEMAND
      REAL ::  ALLOC_CUSTOMER
      REAL ::  DELTA_CLASS_MW_FROM_DSM(SYSTEM_CLASS_NUM)

      REAL (KIND=8), SAVE, ALLOCATABLE :: CLASS_MWH(:)
      REAL(KIND=4), SAVE, ALLOCATABLE :: CLASS_COSTS_TOTAL(:)
      REAL (KIND=4), SAVE, ALLOCATABLE :: CLASS_RATES(:)
      REAL (KIND=8) :: R_CLASS_MWH(*)
      
      INTEGER (kind=2) ::  CLASS
      INTEGER (kind=2) ::  PRIMARY_CLASSES
      INTEGER (kind=2) ::  PEAK_MONTH
      LOGICAL (kind=1) ::  ALLOCATION_REPORT

      LOGICAL (kind=1) ::  POOLING_TRANSACTIONS
      LOGICAL (kind=1) ::  PRICE_FEEDBACK_ACTIVE
      LOGICAL (kind=1) ::  CALCULATE_TOTAL_PRICE_ADJUSTER
!
      INTEGER (kind=2) ::  REV_ALLOC_NO
      INTEGER (kind=2) ::  CLASS_REV_ALLOC_HEADER
      INTEGER (kind=2) ::  REV_FUNC_NO
      INTEGER (kind=2) ::  REV_FUNC_HEADER
      INTEGER (kind=4) ::  REV_FUNC_REC,REV_ALLOC_REC
      LOGICAL (kind=1) ::  REV_ALLOC_REPORT_NOT_OPEN=.TRUE. 

      SAVE REV_ALLOC_NO,REV_FUNC_NO,REV_FUNC_REC,REV_ALLOC_REC
!
      LOGICAL (kind=1) ::  CLASS_EXISTS_loc(MAX_LOAD_CLASSES)
      REAL (kind=4) ::  R_BASE_REVENUE,R_ADJUSTMENT_REVENUE

      REVENUES_FROM_CLASS_PRICES = OPMETH=='P' .AND. PRICE_SOURCE=='C'
      CALL RETURN_COST_SERVICE_ALLOCATORS(globecom_YEAR,
     + DEMAND_ALLOCATOR,
     + ENERGY_ALLOCATOR,
     + CUSTOMER_ALLOCATOR)
      CALL GET_CLASS_EXISTS(CLASS_EXISTS)
      IF(POOLING_TRANSACTIONS()) THEN
         PRIMARY_CLASSES = MAX_LOAD_CLASSES-1
      ELSE
         PRIMARY_CLASSES = MAX_LOAD_CLASSES
      ENDIF

      IF(ALLOCATED(CLASS_MWH)) DEALLOCATE(CLASS_MWH)
      IF(ALLOCATED(CLASS_COSTS_TOTAL)) DEALLOCATE(CLASS_COSTS_TOTAL)
      IF(ALLOCATED(CLASS_RATES)) DEALLOCATE(CLASS_RATES)
      ALLOCATE(CLASS_MWH(SYSTEM_CLASS_NUM))
      ALLOCATE(CLASS_COSTS_TOTAL(SYSTEM_CLASS_NUM))
      ALLOCATE(CLASS_RATES(SYSTEM_CLASS_NUM))
      CLASS_MWH = 0.
      CLASS_PEAK = 0.
      CLASS_RATES = 0.
      TOTAL_CUSTOMERS  = 0.0
      CLASS_COSTS_TOTAL(SYSTEM_CLASS_NUM) = REVENUE

      CLASS_COSTS_DEMAND(SYSTEM_CLASS_NUM) = 
     + ns_arrays1%BASE_REVENUE(2) *
     + DEMAND_ALLOCATOR
     
      CLASS_COSTS_ENERGY(SYSTEM_CLASS_NUM) = 
     + ns_arrays1%BASE_REVENUE(2) *
     + ENERGY_ALLOCATOR + ns_arrays1%ADJ_CLAUSE_REVENUE(2)
      CLASS_COSTS_CUSTOMER(SYSTEM_CLASS_NUM) = 
     + ns_arrays1%BASE_REVENUE(2) *
     + CUSTOMER_ALLOCATOR

      CALL GET_CLASS_MW_FROM_DSM(globecom_YEAR,DELTA_CLASS_MW_FROM_DSM)
      IF(SYSTEM_BASED_FORECAST()) THEN
         CLASS_MWH(SYSTEM_CLASS_NUM) =
     +         RETAIL_SALES_BY_CLASS(SYSTEM_CLASS_NUM) -
     +                      DELTA_CLASS_SALES_FROM_DSM(SYSTEM_CLASS_NUM)

         ANNUAL_PEAK = GET_INPUT_SYSTEM_PEAK(globecom_YEAR) -
     +                     DELTA_CLASS_MW_FROM_DSM(SYSTEM_CLASS_NUM)
         CLASS_PEAK(SYSTEM_CLASS_NUM) = ANNUAL_PEAK
         IF(CLASS_MWH(SYSTEM_CLASS_NUM) /= 0.0)
     +             CLASS_RATES(SYSTEM_CLASS_NUM) = 1000000.*REVENUE/
     +                                      CLASS_MWH(SYSTEM_CLASS_NUM)
         TOTAL_PEAK = ANNUAL_PEAK
         TOTAL_MWH  = CLASS_MWH(SYSTEM_CLASS_NUM)
         PRICE_FEEDBACK_ACTIVE =
     +      CALCULATE_TOTAL_PRICE_ADJUSTER(SYSTEM_CLASS_NUM,
     + globecom_YEAR,CLASS_RATES(SYSTEM_CLASS_NUM))
      ELSE
         TOTAL_PEAK = 0.0
         TOTAL_MWH  = 0.0
         DO CLASS = 1, PRIMARY_CLASSES
            IF(.NOT. CLASS_EXISTS_loc(CLASS)) CYCLE
            CLASS_MWH(CLASS) = RETAIL_SALES_BY_CLASS(CLASS) -
     +                                DELTA_CLASS_SALES_FROM_DSM(CLASS)
            TOTAL_MWH  = TOTAL_MWH  + CLASS_MWH(CLASS)
            CLASS_PEAK(CLASS) =
     + MAX(FORECAST_COINCIDENT_PEAK(1,PEAK_MONTH(globecom_YEAR),CLASS),
     + FORECAST_COINCIDENT_PEAK(2,PEAK_MONTH(globecom_YEAR),CLASS)) -
     + DELTA_CLASS_MW_FROM_DSM(CLASS)
            TOTAL_PEAK = TOTAL_PEAK + CLASS_PEAK(CLASS)
            TOTAL_CUSTOMERS = TOTAL_CUSTOMERS+FORECAST_CUSTOMERS(CLASS)
         ENDDO
         CLASS_MWH(SYSTEM_CLASS_NUM) = TOTAL_MWH
         CLASS_PEAK(SYSTEM_CLASS_NUM) = TOTAL_PEAK
         CLASS_RATES(SYSTEM_CLASS_NUM) = 0.0
         IF(CLASS_MWH(SYSTEM_CLASS_NUM) /= 0.0)
     +           CLASS_RATES(SYSTEM_CLASS_NUM) = 1000000.*REVENUE/
     +                                      CLASS_MWH(SYSTEM_CLASS_NUM)
         IF(TOTAL_PEAK > 0 .AND. TOTAL_MWH > 0
     +                               .AND. TOTAL_CUSTOMERS > 0.0)THEN
            ALLOC_ENERGY =(ns_arrays1%BASE_REVENUE(2)*ENERGY_ALLOCATOR +
     +                       ns_arrays1%ADJ_CLAUSE_REVENUE(2))/TOTAL_MWH
           ALLOC_DEMAND = ns_arrays1%BASE_REVENUE(2) * 
     + DEMAND_ALLOCATOR/TOTAL_PEAK
       ALLOC_CUSTOMER = ns_arrays1%BASE_REVENUE(2) * CUSTOMER_ALLOCATOR/
     +                                TOTAL_CUSTOMERS
            IF(REVENUES_FROM_CLASS_PRICES) THEN
               CALL GET_CLASS_REVENUES_BY_TYPE(CLASS_COSTS_ENERGY,
     +                                         CLASS_COSTS_DEMAND,
     +                                         CLASS_COSTS_CUSTOMER)
               ALLOC_ENERGY = ns_arrays1%ADJ_CLAUSE_REVENUE(2)/TOTAL_MWH
               CLASS_COSTS_DEMAND(SYSTEM_CLASS_NUM) = 0.
               CLASS_COSTS_ENERGY(SYSTEM_CLASS_NUM) = 0.
               CLASS_COSTS_CUSTOMER(SYSTEM_CLASS_NUM) = 0.
            ENDIF
            DO CLASS = 1, PRIMARY_CLASSES
               IF(.NOT. CLASS_EXISTS_loc(CLASS) .OR.
     +                                    CLASS_MWH(CLASS) <= 0.) CYCLE
               IF(REVENUES_FROM_CLASS_PRICES) THEN
                 CLASS_COSTS_ENERGY(CLASS) = CLASS_COSTS_ENERGY(CLASS)+
     +                                  CLASS_MWH(CLASS) * ALLOC_ENERGY
                  CLASS_COSTS_DEMAND(SYSTEM_CLASS_NUM) =
     +                           CLASS_COSTS_DEMAND(SYSTEM_CLASS_NUM) +
     +                                        CLASS_COSTS_DEMAND(CLASS)
                  CLASS_COSTS_ENERGY(SYSTEM_CLASS_NUM) =
     +                           CLASS_COSTS_ENERGY(SYSTEM_CLASS_NUM) +
     +                                        CLASS_COSTS_ENERGY(CLASS)
                  CLASS_COSTS_CUSTOMER(SYSTEM_CLASS_NUM) =
     +                         CLASS_COSTS_CUSTOMER(SYSTEM_CLASS_NUM) +
     +                                      CLASS_COSTS_CUSTOMER(CLASS)
               ELSE
                  CLASS_COSTS_DEMAND(CLASS) = CLASS_PEAK(CLASS) *
     +                                                     ALLOC_DEMAND
                  CLASS_COSTS_CUSTOMER(CLASS) = ALLOC_CUSTOMER *
     +                                        FORECAST_CUSTOMERS(CLASS)
                  CLASS_COSTS_ENERGY(CLASS) = CLASS_MWH(CLASS) *
     +                                                     ALLOC_ENERGY

               ENDIF
               CLASS_COSTS_TOTAL(CLASS) = CLASS_COSTS_DEMAND(CLASS) +
     +                                    CLASS_COSTS_CUSTOMER(CLASS) +
     +                                    CLASS_COSTS_ENERGY(CLASS)
               CLASS_RATES(CLASS) = 1000000*CLASS_COSTS_TOTAL(CLASS)/
     +                                         CLASS_MWH(CLASS)
               PRICE_FEEDBACK_ACTIVE =
     +            CALCULATE_TOTAL_PRICE_ADJUSTER(CLASS,globecom_YEAR,
     +                                              CLASS_RATES(CLASS))
            ENDDO ! CLASS LOOP
            IF(REVENUES_FROM_CLASS_PRICES .AND.
     +                           ns_arrays1%BASE_REVENUE(2) /= 0.) THEN
               DEMAND_ALLOCATOR = CLASS_COSTS_DEMAND(SYSTEM_CLASS_NUM)/
     +                                     ns_arrays1%BASE_REVENUE(2)
              ENERGY_ALLOCATOR = (CLASS_COSTS_ENERGY(SYSTEM_CLASS_NUM)-
     + ns_arrays1%ADJ_CLAUSE_REVENUE(2))/
     +                                ns_arrays1%BASE_REVENUE(2)
               CUSTOMER_ALLOCATOR =
     + CLASS_COSTS_CUSTOMER(SYSTEM_CLASS_NUM)/ns_arrays1%BASE_REVENUE(2)
            ENDIF
         ENDIF  ! END TOTALS NE ZERO
      ENDIF
!
      IF(ALLOCATION_REPORT() .AND. .NOT. SYSTEM_BASED_FORECAST()) THEN
         DO CLASS = 1, PRIMARY_CLASSES
            IF(.NOT. CLASS_EXISTS_loc(CLASS) ) CYCLE
            CALL WRITE_DSM_RATE_REPORT(CLASS,
     +            CLASS_RATES(CLASS),
     +            CLASS_COSTS_TOTAL(CLASS),CLASS_COSTS_DEMAND(CLASS),
     +           CLASS_COSTS_ENERGY(CLASS),CLASS_COSTS_CUSTOMER(CLASS),
     +            CLASS_PEAK(CLASS),CLASS_MWH(CLASS),
     +            FORECAST_CUSTOMERS(CLASS))
         ENDDO
      ENDIF
      IF(ALLOCATION_REPORT() .AND. .NOT. TESTING_PLAN) THEN
         IF(REV_ALLOC_REPORT_NOT_OPEN) THEN
            REV_ALLOC_NO = CLASS_REV_ALLOC_HEADER(REV_ALLOC_REC)
            REV_FUNC_NO = REV_FUNC_HEADER(REV_FUNC_REC)
            REV_ALLOC_REPORT_NOT_OPEN = .FALSE.
         ENDIF
!
         IF(REVENUE /= 0.) THEN
            DENOM = REVENUE
         ELSE
            DENOM = 1.
         ENDIF
         WRITE(REV_FUNC_NO,REC=REV_FUNC_REC) PRT_ENDPOINT(),
     +         FLOAT(globecom_YEAR+get_BASE_YEAR()),'Demand  ',
     +         ns_arrays1%BASE_REVENUE(2)*DEMAND_ALLOCATOR,
     +         100.*DEMAND_ALLOCATOR,
     +         -999999.,
     +         ns_arrays1%BASE_REVENUE(2)*DEMAND_ALLOCATOR,
     +         100.*ns_arrays1%BASE_REVENUE(2)*DEMAND_ALLOCATOR/DENOM
         REV_FUNC_REC = REV_FUNC_REC + 1
         WRITE(REV_FUNC_NO,REC=REV_FUNC_REC) PRT_ENDPOINT(),
     +         FLOAT(globecom_YEAR+get_BASE_YEAR()),
     +         'Energy  ',
     +         ns_arrays1%BASE_REVENUE(2)*ENERGY_ALLOCATOR,
     +         100.*ENERGY_ALLOCATOR,
     +         ns_arrays1%ADJ_CLAUSE_REVENUE(2),
     +        ns_arrays1%BASE_REVENUE(2)*ENERGY_ALLOCATOR + 
     + ns_arrays1%ADJ_CLAUSE_REVENUE(2),
     +         100.*(ns_arrays1%BASE_REVENUE(2)*ENERGY_ALLOCATOR +
     +                           ns_arrays1%ADJ_CLAUSE_REVENUE(2))/DENOM
         REV_FUNC_REC = REV_FUNC_REC + 1
         WRITE(REV_FUNC_NO,REC=REV_FUNC_REC) PRT_ENDPOINT(),
     +         FLOAT(globecom_YEAR+get_BASE_YEAR()),
     +         'Customer',
     +         ns_arrays1%BASE_REVENUE(2)*CUSTOMER_ALLOCATOR,
     +         100.*CUSTOMER_ALLOCATOR,
     +         -999999.,
     +         ns_arrays1%BASE_REVENUE(2)*CUSTOMER_ALLOCATOR,
     +         100.*ns_arrays1%BASE_REVENUE(2)*CUSTOMER_ALLOCATOR/DENOM
         REV_FUNC_REC = REV_FUNC_REC + 1

         IF(.NOT. SYSTEM_BASED_FORECAST()) THEN
            DO CLASS = 1, PRIMARY_CLASSES
               IF(.NOT. CLASS_EXISTS_loc(CLASS)) CYCLE
               WRITE(REV_ALLOC_NO,REC=REV_ALLOC_REC) PRT_ENDPOINT(),
     +            FLOAT(globecom_year+get_BASE_YEAR()),
     +            CLASS_NAME(CLASS),CLASS_RATES(CLASS),
     +            CLASS_COSTS_TOTAL(CLASS),CLASS_COSTS_DEMAND(CLASS),
     +           CLASS_COSTS_ENERGY(CLASS),CLASS_COSTS_CUSTOMER(CLASS),
     +            CLASS_PEAK(CLASS),
     +            DELTA_CLASS_MW_FROM_DSM(CLASS),
     +            SNGL(CLASS_MWH(CLASS))/1000.,
     +            DELTA_CLASS_SALES_FROM_DSM(CLASS)/1000.,
     +            FORECAST_CUSTOMERS(CLASS)
               REV_ALLOC_REC = REV_ALLOC_REC + 1
            ENDDO
         ENDIF
         WRITE(REV_ALLOC_NO,REC=REV_ALLOC_REC) PRT_ENDPOINT(),
     +                 FLOAT(globecom_year+get_BASE_YEAR()),
     +                 CLASS_NAME(SYSTEM_CLASS_NUM),
     +                 CLASS_RATES(SYSTEM_CLASS_NUM),
     +                 CLASS_COSTS_TOTAL(SYSTEM_CLASS_NUM),
     +                 CLASS_COSTS_DEMAND(SYSTEM_CLASS_NUM),
     +                 CLASS_COSTS_ENERGY(SYSTEM_CLASS_NUM),
     +                 CLASS_COSTS_CUSTOMER(SYSTEM_CLASS_NUM),
     +                 CLASS_PEAK(SYSTEM_CLASS_NUM),
     +                 DELTA_CLASS_MW_FROM_DSM(SYSTEM_CLASS_NUM),
     +                 SNGL(CLASS_MWH(SYSTEM_CLASS_NUM))/1000.,
     +                 DELTA_CLASS_SALES_FROM_DSM(SYSTEM_CLASS_NUM)
     +                                                          /1000.,
     +                 MAX(1.,TOTAL_CUSTOMERS)
         REV_ALLOC_REC = REV_ALLOC_REC + 1
      ENDIF
      RETURN

      ENTRY GET_CLASS_MWH_RATES(R_CLASS_MWH,R_CLASS_RATES)

         IF(ALLOCATED(CLASS_MWH)) THEN
            R_CLASS_MWH(1:SYSTEM_CLASS_NUM) =
     +                                    CLASS_MWH(1:SYSTEM_CLASS_NUM)
            R_CLASS_RATES(1:SYSTEM_CLASS_NUM) =
     +                                  CLASS_RATES(1:SYSTEM_CLASS_NUM)
         ELSE
            R_CLASS_MWH(1:SYSTEM_CLASS_NUM) = 0.
            R_CLASS_RATES(1:SYSTEM_CLASS_NUM) = 0.
         ENDIF
      RETURN

      ENTRY GET_CLASS_RATES(R_CLASS_RATES)

         IF(ALLOCATED(CLASS_RATES)) THEN
            R_CLASS_RATES(1:SYSTEM_CLASS_NUM) =
     +                                  CLASS_RATES(1:SYSTEM_CLASS_NUM)
         ELSE
            R_CLASS_RATES(1:SYSTEM_CLASS_NUM) = 0.
         ENDIF
      RETURN

      ENTRY STORE_REVENUE(R_BASE_REVENUE,R_ADJUSTMENT_REVENUE)

         REVENUE = R_BASE_REVENUE + R_ADJUSTMENT_REVENUE
         ns_arrays1%BASE_REVENUE(2) = R_BASE_REVENUE
         ns_arrays1%ADJ_CLAUSE_REVENUE(2) = R_ADJUSTMENT_REVENUE
      RETURN
 1000 FORMAT(1X,I3,4X,F6.2,3(1X,F8.2),2X,F8.2,3X,F7.1,1X,F8.1,1X,I9)
 1010 FORMAT(1X,'System',1X,F6.2,3(1X,F8.2),2X,F8.2,3X,F7.1,1X,F8.1,
     +       1X,I9)
 1020 FORMAT(1X,'Base Costs (M$) ',F8.2,2(2X,F8.2),4X,I8,3X,F6.2,F8.2,
     +       2X,I8,3X,F6.2)
 1030 FORMAT(1X,'Factor (%)      ',F8.2,2(4X,F6.2))
 1040 FORMAT(1X,A,10X,F8.2,25X,F6.2,21X,F6.2)
 1050 FORMAT(1X,'Total Costs (M$)',F8.2,2(2X,F8.2),15X,F6.2,21X,F6.2)
      END
!
!
