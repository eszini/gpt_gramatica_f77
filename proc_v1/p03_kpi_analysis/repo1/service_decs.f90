module service_decs
use hesi_gendecs
use eco
use cap_trans
use end_routine
use namescom
implicit none
    interface
        function service_report()
        character (len=1) :: service_report
        end function service_report
    end interface

    type t_ns_service_decs
        INTEGER (kind=2) :: NUM_SERVICE_TRANS=0
        logical (kind=1) :: monthly_service_report_active=.false.
        logical (kind=1) :: first_cl_service_write=.true.
        integer (kind=2) :: service_trans=0
        character (len=5), allocatable :: service_link_type(:)
        logical (kind=1) :: wabash_valley_active=.false.

        real (kind=4) :: seasonal_capacity
        real (kind=4) :: seasonal_capacity_charge
        real (kind=4) :: SEASONAL_ENERGY
        real (kind=4) :: SEASONAL_ENERGY_CHARGE


        character(len=1), allocatable ::  ENERGY_TO_USE(:)
        character (len=1)  :: revenue='r' ! parameter
        integer(kind=2) :: season_hours(12)
        character(len=1), allocatable :: service_cost_assignment(:)
        character(len=1), allocatable :: type_of_service(:)
        real (kind=4), allocatable :: ENERGY_MULTIPLIER(:)
        integer (kind=2), allocatable :: service_ends(:)
        integer (kind=2), allocatable ::service_available(:)
        real (kind=4), allocatable :: capacity_multiplier(:)
        integer (kind=2), allocatable :: SERVICE_RESOURCE_PONTR(:)
        real (kind=4), allocatable :: CAPACITY_CHARGE(:)
        integer(kind=2), allocatable :: R_ASSET_CLASS_VECTOR(:)

        integer(kind=2), allocatable :: asset_class_num(:)
        CHARACTER(len=20), allocatable :: &
            SERVICE_EXPENSE_CLASSIFICATION(:)
        real (kind=4), allocatable :: WVPA_MEM_TRACKER_INDEX(:)
        character (len=3), allocatable :: SERVICE_EXPENSE_COLLECTION(:)
        real (kind=4), allocatable :: &
            WVPA_FUEL_TRACKER_INDEX(:), WVPA_RES_TRACKER_INDEX(:)
        real (kind=4), allocatable :: WVPA_RATE_TRACKER_INDEX(:)
        integer (kind=2), allocatable :: SERVICE_REPORTING_GROUP(:)
        real (kind=4), allocatable :: ENERGY_CHARGE(:)
        character (len=20), allocatable :: SERVICE_NAME(:)
        real (kind=4), allocatable :: ANNUAL_COSTS(:,:)
        integer (kind=2), allocatable :: ASSET_CLASS_VECTOR(:)
        integer (kind=2), allocatable :: INTRA_COMPANY_CLASS_ID(:)
        integer (kind=2), allocatable :: R_ASSET_CLASS_NUM(:)
        integer (kind=2), allocatable :: R_INTRA_COMPANY_CLASS_ID(:)
        real(kind=4) :: enrgy_charge_ord
        real(kind=4), allocatable :: ANNUAL_CAPACITY_CHARGE(:)
        real (kind=4), allocatable :: annual_energy_charge(:)
        real :: cl_pool_frac_own(max_cl_units)
        logical (kind=1) :: WRITE_IT_MONTHLY=.false.

    end type t_ns_service_decs
    integer (kind=2) :: R_SERVICE_TRANS=0

    type(t_ns_service_decs), save :: ns_service_decs
contains
    subroutine CL_SERVICE_TRANS_CALCULATIONS(nunits, energy, &
        on__line,off__line, &
        DATE1_loc,DATE2_loc,ISEAS, &
        REVENUE_GENERATING_CAPACITY, &
        EXPENSE_GENERATING_CAPACITY)
        use prodcom
        use res_decs
        use mod_base_year
        implicit none
        integer (kind=2) :: get_TRANSACTION_GROUP ! External
        integer (kind=2), intent(in) :: nunits
        real :: ENERGY(2,MAX_CL_UNITS)
        logical (kind=1), parameter :: LOGICAL1_TRUE=.true.
        integer (kind=2) :: I, iseas, L
        integer (kind=2) :: on__line(:), off__line(:), DATE1_loc, &
            DATE2_loc
        real :: revenue_generating_capacity(max_cl_units)
        real :: expense_generating_capacity(:,:)

        real (kind=4) :: seasonal_energy
        real (kind=4) :: SEASONAL_ENERGY_CHARGE


        call allocate_resource_ids_array(max_cl_units)




     DO I = 1, ns_service_decs%SERVICE_TRANS
        IF(INDEX(ns_service_decs%SERVICE_LINK_TYPE(I),'CL') /= 0 .OR. &
               trim(ns_service_decs%SERVICE_LINK_TYPE(I)) == 'TG') THEN
       IF(INDEX(ns_service_decs%SERVICE_LINK_TYPE(I),'CL') /= 0 .AND. &
                   ns_service_decs%FIRST_CL_SERVICE_WRITE .AND. &
                     ns_service_decs%MONTHLY_SERVICE_REPORT_ACTIVE) THEN

               ! TAKE ALL CL SERVICES FOR FIRST WRITE
           ELSEIF(  .NOT. ns_service_decs%wabash_valley_active .AND. &
              (ns_service_decs%service_available(I) > DATE2_loc .OR. &
                    ns_service_decs%service_ends(I) < DATE1_loc)) THEN

              CYCLE
           ENDIF

           DO L = 1, NUNITS
          IF((trim(ns_service_decs%SERVICE_LINK_TYPE(I)) == 'CL' .AND. &
    resource_id(L) == ns_service_decs%SERVICE_RESOURCE_PONTR(I)) .OR. &
         (INDEX(ns_service_decs%SERVICE_LINK_TYPE(I),'TG') /= 0 .AND. &
  get_TRANSACTION_GROUP(L)== &
    ns_service_decs%SERVICE_RESOURCE_PONTR(I)))THEN
                 IF(.NOT. ns_service_decs%wabash_valley_active .AND. &
              (on__line(L) > DATE2_loc .OR. off__line(L) < DATE1_loc)) CYCLE
              IF(ns_service_decs%service_available(I) > DATE2_loc .OR. &
                     ns_service_decs%service_ends(I) < DATE1_loc) THEN

                    seasonal_energy = 0.
                    ns_service_decs%SEASONAL_CAPACITY = 0.
                    SEASONAL_ENERGY_CHARGE = 0.
                    ns_service_decs%seasonal_capacity_charge = 0.
                 ELSE

          ! A hack was used when this routine was external.
          ! Energy argument was (2:*) but was defined in this routine
          ! as (1) but twice the length of the array passed, which
          ! looks like this: (:,:). So source array is (:,:), while
          ! argument array is (:) but is twice as long. I'm commenting
          ! out this code until it can be tested. Replacement code is
          ! below it.
!              seasonal_energy = &
!        (ENERGY(2*L-1)+ENERGY(2*L)) * &
!                              ns_service_decs%SEASON_HOURS(ISEAS)
       seasonal_energy = &
        (ENERGY(2,L-1)+ENERGY(2,L)) * &
                              ns_service_decs%SEASON_HOURS(ISEAS)

     IF(ns_service_decs%energy_to_use(I) == 'R') THEN
        seasonal_energy = &
            seasonal_energy &
                          - MON_ECO_SALES_ENRG_FROM(L)
     ELSEIF(ns_service_decs%energy_to_use(I) == 'W') THEN
        seasonal_energy = MON_ECO_SALES_ENRG_FROM(L)
     ENDIF

 seasonal_energy = seasonal_energy * &
                   ns_service_decs%ENERGY_MULTIPLIER(I) * &
                   CL_POOL_FRAC_OWN(L)/100.
 IF(ns_service_decs%SERVICE_COST_ASSIGNMENT(I) == &
    ns_service_decs%revenue) THEN
    ns_service_decs%SEASONAL_CAPACITY = &
        ns_service_decs%capacity_multiplier(I) * &
                     REVENUE_GENERATING_CAPACITY(L)
 ELSE
    ns_service_decs%SEASONAL_CAPACITY = &
    ns_service_decs%capacity_multiplier(I) * &
                   EXPENSE_GENERATING_CAPACITY(2,L)
 ENDIF
 ns_service_decs%SEASONAL_CAPACITY = &
    ns_service_decs%SEASONAL_CAPACITY * &
                           CL_POOL_FRAC_OWN(L)/100.
    SEASONAL_ENERGY_CHARGE = &
    ns_service_decs%ENERGY_CHARGE(I) * &
                                    seasonal_energy
 ns_service_decs%seasonal_capacity_charge = &
    ns_service_decs%CAPACITY_CHARGE(I) * &
                                  ns_service_decs%SEASONAL_CAPACITY
                 ENDIF

                 IF(ns_service_decs%type_of_service(I) == 'T') &
     THEN
                    CALL TRANSACTION_MANAGER(ISEAS,I, &
           ns_service_decs%SERVICE_COST_ASSIGNMENT(I), & !3
           ns_service_decs%type_of_service(I), & !4
           ns_service_decs%SERVICE_NAME(I), & !5
           seasonal_energy, & !6
           real(SEASONAL_ENERGY_CHARGE/1000.,4), & !7
           ns_service_decs%ENERGY_CHARGE(I), & !8
           ns_service_decs%SEASONAL_CAPACITY, & !9
           ns_service_decs%seasonal_capacity_charge, & !10
           ns_service_decs%CAPACITY_CHARGE(I), & !11
           ns_service_decs%SERVICE_EXPENSE_COLLECTION(I), &
           ns_service_decs%service_reporting_group(i), &
           ns_service_decs%MONTHLY_SERVICE_REPORT_ACTIVE, &
           LOGICAL1_TRUE, & !14
           ns_service_decs%WVPA_RATE_TRACKER_INDEX(I), &
           ns_service_decs%WVPA_RES_TRACKER_INDEX(I), &
           ns_service_decs%WVPA_FUEL_TRACKER_INDEX(I), &
           ns_service_decs%WVPA_MEM_TRACKER_INDEX(I), &
      ns_service_decs%SERVICE_EXPENSE_CLASSIFICATION(I))
    ENDIF
       CALL STORE_ST_COST_AND_REVENUE_DATA(I, &
      SEASONAL_ENERGY_CHARGE/1000., &
       ns_service_decs%seasonal_capacity_charge, &
       seasonal_energy, &
       ns_service_decs%SEASONAL_CAPACITY)

              ENDIF
           ENDDO ! CL UNITS COUNTER
        ENDIF ! YES CL UNIT
     ENDDO ! TRANSACTIONS COUNTER
     IF(ns_service_decs%FIRST_CL_SERVICE_WRITE) then
        ns_service_decs%FIRST_CL_SERVICE_WRITE = .FALSE.
    endif

    end subroutine CL_SERVICE_TRANS_CALCULATIONS

     function GET_NO_OF_SERVICE_TRANS()
         integer(kind=2) :: RST, GET_NO_OF_SERVICE_TRANS
         RST = ns_service_decs%SERVICE_TRANS
         GET_NO_OF_SERVICE_TRANS=RST
     end function GET_NO_OF_SERVICE_TRANS

      SUBROUTINE TRANSACTION_MANAGER(R_SEASON, & !verified
         R_TRANS_NO, & !verified
         R_COST_ASSIGNMENT, & ! verified
         R_TRANSACTION_TYPE, & ! Verified
         R_TRANSACTION_DESCRIPTION, & !Verified
         R_ENERGY, & ! verified
         R_ENERGY_CHARGE, & ! verified
         R_UNIT_ENERGY_COST, & ! verified
         R_CAPACITY, & ! verified
         R_CAPACITY_CHARGE, & ! verified
         R_UNIT_CAPACITY_COST, & ! verified
         R_EXPENSE_COLLECTION, & ! verified
         R_REPORTING_GROUP, & ! verified
         R_MONTHLY_REPORT_ACTIVE, & ! verified
         R_WRITE_IT_MONTHLY, & ! verified
         RATE_TRACKER, & ! verified
         RES_TRACKER, &
         FUEL_TRACKER, &
         MEM_TRACKER, &
         R_SERVICE_EXPENSE_CLASS)
! *************************************************************

      use IREC_ENDPOINT_CONTROL
      use production
      use SpinDriftLib
      use prod_arrays_dimensions
      use sizecom
      use globecom
      use servcom
      use grx_planning_routines
      use mod_base_year
      implicit none

         LOGICAL*1   SERVICE_TRANS_REPORT_NOT_OPEN/.TRUE./, &
                     TRANSACTION_ACTIVE(:),FALSE_BYTE, &
                     MONTHLY_REPORT_ACTIVE,R_MONTHLY_REPORT_ACTIVE, &
                     R_WRITE_IT_MONTHLY
        INTEGER*2   SEASON,R_SEASON,TRANS_RPT_NO,SERVICE_TRANS_HEADER, &
                     LAST_SEASON_loc,I, &
                     NO_TRANS_TYPES/5/,TRANS_TYPE_NO,COST_NO, &
                     TRANS_NO,R_TRANS_NO, &
                     TOTAL_ANNUAL_RECORDS,COST_POSITION, &
                     REPORTING_GROUP,R_REPORTING_GROUP, &
                     TYPE, &
                     TRACKER, &
                     R_TYPE, &
                     R_TRACKER_TYPE, &
                     R_COST

         integer (kind=2) :: NO_OF_TRANS=0

         REAL*4 RES_TRACKER,FUEL_TRACKER,RATE_TRACKER,MEM_TRACKER
         INTEGER TRANS_RPT_REC
         CHARACTER*1 COST_ASSIGNMENT,R_COST_ASSIGNMENT, &
                     TRANSACTION_TYPE,R_TRANSACTION_TYPE
         CHARACTER*3 R_EXPENSE_COLLECTION,EXPENSE_COLLECTION
         CHARACTER*7 COST_ASSIGNMENT_NAME, &
                ASSIGNMENT_NAME(2)/'revenue','Expense'/

         CHARACTER*9 CL_MONTH_NAME(13)
         CHARACTER*12 TRANS_TYPE,TRANS_TYPE_NAME(5)/ &
                           'Transmission','Stand-By','Wheeling', &
                                          'Dispatching','Other'/
         CHARACTER*1 TRAN_TYPE(:),COST_ASSIGN(:),EXPENSE_ASSIGN(:)
         character (len=20) :: R_SERVICE_EXPENSE_CLASS
       CHARACTER*20 TRANSACTION_DESCRIPTION,R_TRANSACTION_DESCRIPTION, &
                        MONTH_NAME,TRANS_NAME(:), &
                        WVPA_COST_ASSIGN(2)/'Total Service Rev.  ', &
                                            'Total Service Cost  '/
         REAL (kind=4) :: ENERGY_CHARGE_loc=0.
         real (kind=4) :: R_ENERGY_CHARGE
         real (kind=4) :: VARIABLE_OM_CHARGE
         real (kind=4) :: CAPACITY_CHARGE,R_CAPACITY_CHARGE, &
               UNIT_ENERGY_COST,R_UNIT_ENERGY_COST, &
               UNIT_CAPACITY_COST,R_UNIT_CAPACITY_COST, &
               IMPUTTED_ENERGY,IMPUTTED_CAPACITY, &
              SEASONAL_TYPE_ELEMENTS(4,5,2),SEASONAL_COST_ELEMENTS(6), &
               TEMP_R, &
             ENERGY,R_ENERGY,CAPACITY,R_CAPACITY,SUM_REPORT_ITEMS(20), &
               SEASONAL_FUEL_ENERGY, seas_fuel_cost_loc=0, &
               ANNUAL_FUEL_ENERGY,ANNUAL_FUEL_COST, &
               R_MM_DB_BY_SEASON(0:12,0:5)
!
         ALLOCATABLE::  TRANS_NAME,COST_ASSIGN,TRAN_TYPE, &
                        TRANSACTION_ACTIVE,EXPENSE_ASSIGN

         SAVE  TRANS_RPT_NO,CL_MONTH_NAME,SEASONAL_TYPE_ELEMENTS, &
               LAST_SEASON_loc,TRANS_NAME,TRAN_TYPE, &
               COST_ASSIGN,TOTAL_ANNUAL_RECORDS,TRANSACTION_ACTIVE, &
               EXPENSE_ASSIGN, &
               SEASONAL_FUEL_ENERGY, &
               ANNUAL_FUEL_ENERGY,ANNUAL_FUEL_COST
         SAVE TRANS_RPT_REC


         CHARACTER*1 WABASH_POWER_COST_RPT
         LOGICAL*1   PRINT_WABASH_COST_REPORT/.FALSE./, &
                     WABASH_REPORT_NOT_USED/.TRUE./, &
                     WVPA
         INTEGER*2   WABASH_REPORT_UNIT_NO/0/, &
                     WABASH_VALLEY_POWER_COST_NO
         INTEGER     WABASH_REC_COUNTER
        REAL*4     KW_CAPACITY,CAP_FACTOR,AVE_VAR_COST,AVE_TOTAL_COST, &
                     RPT_CAPACITY,RPT_ENERGY,FUEL_CHARGE_RATE, &
                     FUEL_COST
         INTEGER*2 BASE_DATE,HISTORICAL_PRODUCTION_DATE

         real (kind=4) :: accumulator

! END DATA DECLARATIONS

!  STANDARD RESULTS MODULE STUFF

         BASE_DATE = (get_BASE_YEAR() + globecom_YEAR - 1900) * 100
         SEASON = R_SEASON
         TRANS_NO = R_TRANS_NO
         COST_ASSIGNMENT = R_COST_ASSIGNMENT
         UNIT_ENERGY_COST = R_UNIT_ENERGY_COST
         UNIT_CAPACITY_COST = R_UNIT_CAPACITY_COST
         energy_charge_loc = R_ENERGY_CHARGE
         CAPACITY_CHARGE = R_CAPACITY_CHARGE
         ENERGY = R_ENERGY
         CAPACITY = R_CAPACITY
         TRANSACTION_TYPE = R_TRANSACTION_TYPE
         EXPENSE_COLLECTION = R_EXPENSE_COLLECTION
         TRANSACTION_DESCRIPTION = R_TRANSACTION_DESCRIPTION
         REPORTING_GROUP = R_REPORTING_GROUP
         MONTHLY_REPORT_ACTIVE = R_MONTHLY_REPORT_ACTIVE
         ns_service_decs%WRITE_IT_MONTHLY = R_WRITE_IT_MONTHLY




         CALL ALLOCATE_SERVICE_COSTS(TRANSACTION_TYPE, &
                                     ENERGY_CHARGE_loc &
                                    + CAPACITY_CHARGE, &
                                     COST_ASSIGNMENT, &
                                     EXPENSE_COLLECTION, &
                                     REPORTING_GROUP)

         IF(.NOT. MONTHLY_REPORT_ACTIVE .OR. &
                     .NOT. ns_service_decs%WRITE_IT_MONTHLY) then
                     RETURN
         end if

         IF(WABASH_REPORT_NOT_USED) THEN
            WABASH_REPORT_UNIT_NO = WABASH_VALLEY_POWER_COST_NO()
            WABASH_REPORT_NOT_USED = .FALSE.
         ENDIF

         PRINT_WABASH_COST_REPORT = WVPA() .AND. &
                              INDEX('M,B,A',WABASH_POWER_COST_RPT())/=0

         FALSE_BYTE = .FALSE.
         IF(COST_ASSIGNMENT == 'R') THEN
            COST_NO = 1
         ELSE
            COST_NO = 2
         ENDIF
         COST_ASSIGNMENT_NAME = ASSIGNMENT_NAME(COST_NO)
!
         IF(SERVICE_TRANS_REPORT_NOT_OPEN) THEN
            TRANS_RPT_NO = SERVICE_TRANS_HEADER(TRANS_RPT_REC)
            SERVICE_TRANS_REPORT_NOT_OPEN = .FALSE.

            LAST_SEASON_loc = get_PRODUCTION_PERIODS_in()
            DO I = 1, LAST_SEASON_loc
               CL_MONTH_NAME(I) = MONTH_NAME(I)
            ENDDO
            CL_MONTH_NAME(LAST_SEASON_loc+1) = 'Annual'
!
            NO_OF_TRANS=GET_NO_OF_SERVICE_TRANS()

         ENDIF

         TRANSACTION_ACTIVE(TRANS_NO) = .TRUE.


         IF(UNIT_CAPACITY_COST == 0.) THEN
            RPT_CAPACITY = 0.
         ELSE
            RPT_CAPACITY = CAPACITY
         ENDIF
         IF(UNIT_ENERGY_COST == 0.) THEN
            RPT_ENERGY = 0.
         ELSE
            RPT_ENERGY = ENERGY
         ENDIF

         IF(INDEX(EXPENSE_COLLECTION,'Adj') /= 0) THEN
            SEASONAL_FUEL_ENERGY = SEASONAL_FUEL_ENERGY + RPT_ENERGY
            seas_fuel_cost_loc = seas_fuel_cost_loc + &
                                 ENERGY_CHARGE_loc

            ANNUAL_FUEL_ENERGY = ANNUAL_FUEL_ENERGY + RPT_ENERGY
            ANNUAL_FUEL_COST = ANNUAL_FUEL_COST + ENERGY_CHARGE_loc
            ns_service_decs%ANNUAL_COSTS(5,TRANS_NO) = &
            ns_service_decs%ANNUAL_COSTS(5,TRANS_NO) + RPT_ENERGY

            ns_service_decs%ANNUAL_COSTS(6,TRANS_NO) = &
                ns_service_decs%ANNUAL_COSTS(6,TRANS_NO) &
                         + ENERGY_CHARGE_loc

         ELSE
            ns_service_decs%ANNUAL_COSTS(1,TRANS_NO) = &
            ns_service_decs%ANNUAL_COSTS(1,TRANS_NO) &
                                       + RPT_ENERGY
            ns_service_decs%ANNUAL_COSTS(2,TRANS_NO) = &
            ns_service_decs%ANNUAL_COSTS(2,TRANS_NO) &
                                      + ENERGY_CHARGE_loc

         ENDIF
         ns_service_decs%ANNUAL_COSTS(3,TRANS_NO) = &
         ns_service_decs%ANNUAL_COSTS(3,TRANS_NO) &
                                    + RPT_CAPACITY
         ns_service_decs%ANNUAL_COSTS(4,TRANS_NO) = &

         ns_service_decs%ANNUAL_COSTS(4,TRANS_NO) &
                                    + CAPACITY_CHARGE

         TRANS_TYPE_NO = 0
         SELECT CASE (TRANSACTION_TYPE)
         CASE ('T')
            TRANS_TYPE_NO = 1
         CASE ('S')
            TRANS_TYPE_NO = 2
         CASE ('W')
            TRANS_TYPE_NO = 3
         CASE ('D')
            TRANS_TYPE_NO = 4
         CASE ('O')
            TRANS_TYPE_NO = 5
         END SELECT
         IF(TRANS_TYPE_NO > 0 .AND. TRANS_TYPE_NO < 6) THEN
            SEASONAL_TYPE_ELEMENTS(1,TRANS_TYPE_NO,COST_NO) = &
               SEASONAL_TYPE_ELEMENTS(1,TRANS_TYPE_NO,COST_NO) + &
                                                             RPT_ENERGY
            SEASONAL_TYPE_ELEMENTS(2,TRANS_TYPE_NO,COST_NO) = &
                     SEASONAL_TYPE_ELEMENTS(2,TRANS_TYPE_NO,COST_NO) &
                         + ENERGY_CHARGE_loc
            SEASONAL_TYPE_ELEMENTS(3,TRANS_TYPE_NO,COST_NO) = &
               SEASONAL_TYPE_ELEMENTS(3,TRANS_TYPE_NO,COST_NO) + &
                                                           RPT_CAPACITY
            SEASONAL_TYPE_ELEMENTS(4,TRANS_TYPE_NO,COST_NO) = &
               SEASONAL_TYPE_ELEMENTS(4,TRANS_TYPE_NO,COST_NO) + &
                               CAPACITY_CHARGE
            TRANS_TYPE = TRANS_TYPE_NAME(TRANS_TYPE_NO)
         ELSE
            TRANS_TYPE = ' '
         ENDIF

! 05/20/03. NOTE THE SUMS.

         IF(INDEX(EXPENSE_COLLECTION,'Adj') /= 0) THEN
               FUEL_COST = ENERGY_CHARGE_loc
               VARIABLE_OM_CHARGE = 0.
            ELSE
               FUEL_CHARGE_RATE = 0.
               VARIABLE_OM_CHARGE = ENERGY_CHARGE_loc
            ENDIF

         TYPE = 4 ! SERVICE TRANSACTION TYPE
!
!         CALL GET_STRANS_TRACKER_INDEX(R_TRANS_NO,TRACKER)

         IF(BASE_DATE + R_SEASON >= HISTORICAL_PRODUCTION_DATE() .AND. &
               INDEX(R_SERVICE_EXPENSE_CLASS, &
                                        'Service') /= 0 .AND. &
                                             COST_ASSIGNMENT /= 'R') &
             CALL WVPA_STORE_CL_TRACKER_DATE_BASE(R_SEASON, &
               TYPE, &
         INT2(FUEL_TRACKER), &
               1000.*FUEL_COST, &
               1000.*VARIABLE_OM_CHARGE, &
               1000.*CAPACITY_CHARGE, &
               INT2(MEM_TRACKER))
!
         IF(SEASON < 13 .AND. PRINT_WABASH_COST_REPORT) THEN
            IF(UNIT_CAPACITY_COST == 0.) THEN
               RPT_CAPACITY = 0.
            ELSE
               RPT_CAPACITY = CAPACITY
            ENDIF
            IF(UNIT_ENERGY_COST == 0.) THEN
               RPT_ENERGY = 0.
            ELSE
               RPT_ENERGY = ENERGY
            ENDIF
            KW_CAPACITY = RPT_CAPACITY*1000.
            CAP_FACTOR = 0.
            IF(RPT_ENERGY > 0.) THEN
               AVE_VAR_COST = 1000.* &
                ENERGY_CHARGE_loc/RPT_ENERGY
               AVE_TOTAL_COST = 1000.* &
                 (ENERGY_CHARGE_loc &
                 +CAPACITY_CHARGE) &
               /RPT_ENERGY
            ELSE
               AVE_VAR_COST = 0.
               AVE_TOTAL_COST = 0.
            ENDIF
            IF(INDEX(EXPENSE_COLLECTION,'Adj') /= 0) THEN
               FUEL_CHARGE_RATE = UNIT_ENERGY_COST
               FUEL_COST = ENERGY_CHARGE_loc
               UNIT_ENERGY_COST = 0.
               ENERGY_CHARGE_loc = 0.
            ELSE
               FUEL_CHARGE_RATE = 0.
               FUEL_COST = 0.
            ENDIF
            INQUIRE(UNIT=WABASH_REPORT_UNIT_NO, &
                                            NEXTREC=WABASH_REC_COUNTER)
            WRITE(WABASH_REPORT_UNIT_NO,REC=WABASH_REC_COUNTER) &
         PRT_ENDPOINT(), &
         FLOAT(globecom_YEAR+get_BASE_YEAR()), &
         CL_MONTH_NAME(SEASON), &
         TRANSACTION_DESCRIPTION, &
         KW_CAPACITY, &
         RPT_ENERGY, &
         0., &
         KW_CAPACITY, &
         RPT_ENERGY, &
         UNIT_CAPACITY_COST, &
         CAPACITY_CHARGE, &
         FUEL_CHARGE_RATE, &
         FUEL_COST, &
         UNIT_ENERGY_COST, &
         ENERGY_CHARGE_loc, &
         FUEL_COST+ENERGY_CHARGE_loc+&
            CAPACITY_CHARGE, &
         KW_CAPACITY, &
         KW_CAPACITY, &
         0., &
         CAP_FACTOR, &
         AVE_TOTAL_COST, &
         AVE_VAR_COST, &
         RES_TRACKER, &
         FUEL_TRACKER, &
         RATE_TRACKER, &
         MEM_TRACKER, &
         0.,  & !  EMISSION AMOUNT
         0.   ! AVERAGE EMISSION RATE
         ENDIF
         IF(SEASON < 13) THEN
            WRITE(TRANS_RPT_NO,REC=TRANS_RPT_REC) &
  PRT_ENDPOINT(),FLOAT(globecom_YEAR+get_BASE_YEAR()), &
  CL_MONTH_NAME(SEASON), &
  TRANSACTION_DESCRIPTION, &
  COST_ASSIGNMENT_NAME, &
  TRANS_TYPE, &
  ENERGY, &
  UNIT_ENERGY_COST,ENERGY_CHARGE_loc, &
  CAPACITY, &
  UNIT_CAPACITY_COST,CAPACITY_CHARGE, &
  ENERGY_CHARGE_loc+CAPACITY_CHARGE
            TRANS_RPT_REC = TRANS_RPT_REC + 1
         ENDIF
      RETURN


      ENTRY INIT_TRANSACTION_MANAGER

! 11/6/01. GAT. MOVED FOR MCKEE.

            NO_OF_TRANS=GET_NO_OF_SERVICE_TRANS()
            IF(NO_OF_TRANS > 0) THEN
               IF(ALLOCATED(ns_service_decs%ANNUAL_COSTS)) then
                DEALLOCATE(ns_service_decs%ANNUAL_COSTS)
               endif
               TOTAL_ANNUAL_RECORDS = &
                                   NO_OF_TRANS + 2*(NO_TRANS_TYPES + 1)
          ALLOCATE(ns_service_decs%ANNUAL_COSTS(6,TOTAL_ANNUAL_RECORDS))
               ns_service_decs%ANNUAL_COSTS = 0.
!
               IF(ALLOCATED(TRANS_NAME)) &
                           DEALLOCATE(TRANS_NAME,COST_ASSIGN, &
                                       EXPENSE_ASSIGN,TRAN_TYPE, &
                                       TRANSACTION_ACTIVE)
               ALLOCATE(TRANS_NAME(NO_OF_TRANS), &
                     COST_ASSIGN(NO_OF_TRANS), &
                     EXPENSE_ASSIGN(NO_OF_TRANS), &
                     TRAN_TYPE(NO_OF_TRANS), &
                     TRANSACTION_ACTIVE(NO_OF_TRANS))
               CALL GET_TRANS_VARIABLES(NO_OF_TRANS, TRANS_NAME, &
                          COST_ASSIGN,EXPENSE_ASSIGN,TRAN_TYPE)
               TRANSACTION_ACTIVE = FALSE_BYTE

! 05/20/03.

!
            ENDIF
!
      RETURN

      ENTRY INIT_TRANS_SEASON_TYPE

         SEASONAL_TYPE_ELEMENTS = 0.
         SEASONAL_FUEL_ENERGY = 0.
         seas_fuel_cost_loc = 0.
      RETURN

      ENTRY WRITE_TRANS_SEASON_TYPE(R_SEASON)

         IF(SERVICE_TRANS_REPORT_NOT_OPEN) RETURN
         SEASON = R_SEASON
! 11/26/03. MOVED TO ACCOMODATE REVENUES
         SEASONAL_COST_ELEMENTS(1) = 0.
         SEASONAL_COST_ELEMENTS(2) = 0.
         SEASONAL_COST_ELEMENTS(3) = 0.
         SEASONAL_COST_ELEMENTS(4) = 0.
         SEASONAL_COST_ELEMENTS(5) = SEASONAL_FUEL_ENERGY
         SEASONAL_COST_ELEMENTS(6) = seas_fuel_cost_loc
!
         DO COST_NO = 1 , 2
            IF(COST_NO == 1) THEN
               TEMP_R = -1.0
            ELSE
               TEMP_R =  1.0
            ENDIF
            DO I = 1 , NO_TRANS_TYPES

               IF(SEASONAL_TYPE_ELEMENTS(1,I,COST_NO) > 0.) THEN
                  IMPUTTED_ENERGY = &
                        1000.*SEASONAL_TYPE_ELEMENTS(2,I,COST_NO)/ &
                                    SEASONAL_TYPE_ELEMENTS(1,I,COST_NO)
               ELSE
                  IMPUTTED_ENERGY = 0.
               ENDIF
               IF(SEASONAL_TYPE_ELEMENTS(3,I,COST_NO) > 0.) THEN
                  IMPUTTED_CAPACITY = &
                        SEASONAL_TYPE_ELEMENTS(4,I,COST_NO)/ &
                                    SEASONAL_TYPE_ELEMENTS(3,I,COST_NO)
               ELSE
                  IMPUTTED_CAPACITY = 0.
               ENDIF
               WRITE(TRANS_RPT_NO,REC=TRANS_RPT_REC) PRT_ENDPOINT(), &
                        FLOAT(globecom_YEAR+get_BASE_YEAR()), &
                        CL_MONTH_NAME(SEASON), &
                        'Total               ', &
                        ASSIGNMENT_NAME(COST_NO), &
                        TRANS_TYPE_NAME(I), &
                        SEASONAL_TYPE_ELEMENTS(1,I,COST_NO), &
                        IMPUTTED_ENERGY, &
                        SEASONAL_TYPE_ELEMENTS(2,I,COST_NO), &
                        SEASONAL_TYPE_ELEMENTS(3,I,COST_NO), &
                        IMPUTTED_CAPACITY, &
                        SEASONAL_TYPE_ELEMENTS(4,I,COST_NO), &
                        SEASONAL_TYPE_ELEMENTS(2,I,COST_NO)+ &
                                    SEASONAL_TYPE_ELEMENTS(4,I,COST_NO)
               TRANS_RPT_REC = TRANS_RPT_REC + 1
               SEASONAL_COST_ELEMENTS(1) = &
                          SEASONAL_COST_ELEMENTS(1) + &
                             SEASONAL_TYPE_ELEMENTS(1,I,COST_NO)*TEMP_R
               SEASONAL_COST_ELEMENTS(2) = &
                          SEASONAL_COST_ELEMENTS(2) + &
                             SEASONAL_TYPE_ELEMENTS(2,I,COST_NO)*TEMP_R
               SEASONAL_COST_ELEMENTS(3) = &
                          SEASONAL_COST_ELEMENTS(3) + &
                             SEASONAL_TYPE_ELEMENTS(3,I,COST_NO)*TEMP_R
               SEASONAL_COST_ELEMENTS(4) = &
                          SEASONAL_COST_ELEMENTS(4) + &
                             SEASONAL_TYPE_ELEMENTS(4,I,COST_NO)*TEMP_R
            ENDDO ! TRANSACTIONS COUNTER
            IF(ABS(SEASONAL_COST_ELEMENTS(1) + &
                                       SEASONAL_COST_ELEMENTS(5)) &
                                                           > 0.01) THEN
               IMPUTTED_ENERGY = 1000.* &
                     (SEASONAL_COST_ELEMENTS(2) + &
                                         SEASONAL_COST_ELEMENTS(6))/ &
                            (SEASONAL_COST_ELEMENTS(1) + &
                                             SEASONAL_COST_ELEMENTS(5))
            ELSE
               IMPUTTED_ENERGY = 0.
            ENDIF
            IF( ABS(SEASONAL_COST_ELEMENTS(3)) > 0.01) THEN
               IMPUTTED_CAPACITY = &
                    SEASONAL_COST_ELEMENTS(4)/SEASONAL_COST_ELEMENTS(3)
            ELSE
               IMPUTTED_CAPACITY = 0.
            ENDIF
            WRITE(TRANS_RPT_NO,REC=TRANS_RPT_REC) PRT_ENDPOINT(), &
                        FLOAT(globecom_YEAR+get_BASE_YEAR()), &
                        CL_MONTH_NAME(SEASON), &
                        'Total               ', &
                        ASSIGNMENT_NAME(COST_NO), &
                        'Total       ', &
                        SEASONAL_COST_ELEMENTS(1), &
                        IMPUTTED_ENERGY, &
                        SEASONAL_COST_ELEMENTS(2), &
                        SEASONAL_COST_ELEMENTS(3), &
                        IMPUTTED_CAPACITY, &
                        SEASONAL_COST_ELEMENTS(4), &
                        SEASONAL_COST_ELEMENTS(2)+ &
                                     SEASONAL_COST_ELEMENTS(4)
            TRANS_RPT_REC = TRANS_RPT_REC + 1
            IF(PRINT_WABASH_COST_REPORT .AND. COST_NO == 2) THEN
               KW_CAPACITY =  SEASONAL_COST_ELEMENTS(3)*1000.
               IF(KW_CAPACITY > 0.) THEN
                  UNIT_CAPACITY_COST = SEASONAL_COST_ELEMENTS(4)/ &
                                              SEASONAL_COST_ELEMENTS(3)
               ELSE
                  UNIT_CAPACITY_COST = 0.
               ENDIF
               CAP_FACTOR = 0.
               IF( SEASONAL_COST_ELEMENTS(1) > 0.) THEN
                  AVE_VAR_COST = 1000.* SEASONAL_COST_ELEMENTS(2)/ &
                                              SEASONAL_COST_ELEMENTS(1)
                  AVE_TOTAL_COST = 1000.* &
                                  (SEASONAL_COST_ELEMENTS(2) + &
                                       SEASONAL_COST_ELEMENTS(4))/ &
                                             SEASONAL_COST_ELEMENTS(1)
               ELSE
                  AVE_VAR_COST = 0.
                  AVE_TOTAL_COST = 0.
               ENDIF
               IF( SEASONAL_COST_ELEMENTS(5) > 0.) THEN
                  FUEL_CHARGE_RATE = 1000.* SEASONAL_COST_ELEMENTS(6)/ &
                                              SEASONAL_COST_ELEMENTS(5)
               ELSE
                  FUEL_CHARGE_RATE = 0.
               ENDIF
               IF(SEASONAL_COST_ELEMENTS(1) - &
                                   SEASONAL_COST_ELEMENTS(5) > 0.) THEN
                  UNIT_ENERGY_COST = (SEASONAL_COST_ELEMENTS(2) - &
                                    SEASONAL_COST_ELEMENTS(6)) / &
                                (SEASONAL_COST_ELEMENTS(1) - &
                                             SEASONAL_COST_ELEMENTS(5))
               ELSE
                  UNIT_ENERGY_COST = 0.
               ENDIF

! TOTAL TRANSACTIONS BY SEASON
!
               INQUIRE(UNIT=WABASH_REPORT_UNIT_NO, &
                                            NEXTREC=WABASH_REC_COUNTER)
               WRITE(WABASH_REPORT_UNIT_NO,REC=WABASH_REC_COUNTER) &
                           PRT_ENDPOINT(), &
                           FLOAT(globecom_YEAR+get_BASE_YEAR()), &
                           CL_MONTH_NAME(SEASON), &
                     WVPA_COST_ASSIGN(COST_NO),   & ! TOTAL SERVICE COST
                           KW_CAPACITY, &
                           SEASONAL_COST_ELEMENTS(1), &
                           0., &
                           KW_CAPACITY, &
                           SEASONAL_COST_ELEMENTS(1), &
                           UNIT_CAPACITY_COST, &
                     SEASONAL_COST_ELEMENTS(4), & !  TOTAL CAPACITY COST
                           FUEL_CHARGE_RATE, & !  FUEL $/MWH
                           SEASONAL_COST_ELEMENTS(6), & !  FUEL COST $
                           UNIT_ENERGY_COST, & !  ENERGY CHARGE $/MWH
                           SEASONAL_COST_ELEMENTS(2) - &
                               SEASONAL_COST_ELEMENTS(6), & !  ENERGY $
                           SEASONAL_COST_ELEMENTS(2) + &
                             SEASONAL_COST_ELEMENTS(4), & !  TOTAL COST
                           KW_CAPACITY, &
                           KW_CAPACITY, &
                           0., &
                           CAP_FACTOR, &
                           AVE_TOTAL_COST, &
                           AVE_VAR_COST, &
                           0., & !  RES_TRACKER,
                           0., & !  FUEL_TRACKER
                           0.,  & !  RATE_TRACKER
                           0.,  & !  MEM_TRACKER
                           0.,  & !  EMISSION AMOUNT
                           0.   ! AVERAGE EMISSION RATE
            ENDIF
         ENDDO
         IF(PRINT_WABASH_COST_REPORT) THEN
            CALL RETURN_SUM_REPORT_ITEMS(SUM_REPORT_ITEMS)
            IF(SUM_REPORT_ITEMS(1) > 0.) THEN
               UNIT_CAPACITY_COST = 1000.*(SUM_REPORT_ITEMS(7) + &
                         SEASONAL_COST_ELEMENTS(4))/SUM_REPORT_ITEMS(1)
            ELSE
               UNIT_CAPACITY_COST = 0.
            ENDIF
            IF(SUM_REPORT_ITEMS(5) > 0.) THEN
               UNIT_ENERGY_COST = &
                           1000.*(SEASONAL_COST_ELEMENTS(2) + &
                                            SUM_REPORT_ITEMS(11))/ &
                                                    SUM_REPORT_ITEMS(5)
               AVE_VAR_COST = 1000.*(SEASONAL_COST_ELEMENTS(2) + &
                      SUM_REPORT_ITEMS(9) + SUM_REPORT_ITEMS(11))/ &
                                                    SUM_REPORT_ITEMS(2)
               AVE_TOTAL_COST = &
                           1000.*(SEASONAL_COST_ELEMENTS(2) + &
                                 SEASONAL_COST_ELEMENTS(4) + &
                                      SUM_REPORT_ITEMS(12))/ &
                                                    SUM_REPORT_ITEMS(2)
            ELSE
               UNIT_ENERGY_COST = 0.
               AVE_VAR_COST = 0.
               AVE_TOTAL_COST = 0.
            ENDIF
            INQUIRE(UNIT=WABASH_REPORT_UNIT_NO, &
                                            NEXTREC=WABASH_REC_COUNTER)
            WRITE(WABASH_REPORT_UNIT_NO,REC=WABASH_REC_COUNTER) &
                           PRT_ENDPOINT(), &
                           FLOAT(globecom_YEAR+get_BASE_YEAR()), &
                           CL_MONTH_NAME(SEASON), &
                           'Total Cost          ', &
                           SUM_REPORT_ITEMS(1),             & ! (1)
                           SUM_REPORT_ITEMS(2),             & ! (2)
                           SUM_REPORT_ITEMS(3),             & ! (3)
                           SUM_REPORT_ITEMS(4),             & ! (4)
                           SUM_REPORT_ITEMS(5),             & ! (5)
                           UNIT_CAPACITY_COST,      &        !(6  !
                           SEASONAL_COST_ELEMENTS(4) + &
                                    SUM_REPORT_ITEMS(7),    & ! (7)
                  SUM_REPORT_ITEMS(8),             & ! (8) ! FUEL $/MWH
                 SEASONAL_COST_ELEMENTS(6) + SUM_REPORT_ITEMS(9), &
                UNIT_ENERGY_COST,   & ! (10)! ENERGY $/MWH
                           SEASONAL_COST_ELEMENTS(2) - &
                              SEASONAL_COST_ELEMENTS(6) + &
                              SUM_REPORT_ITEMS(11),   & ! (11)! ENERGY $
                           SEASONAL_COST_ELEMENTS(2) + &
                                SEASONAL_COST_ELEMENTS(4) + &
                                SUM_REPORT_ITEMS(12),  & ! (12)! TOTAL $
                           SUM_REPORT_ITEMS(13),            & ! (13)
                           SUM_REPORT_ITEMS(14),            & ! (14)
                           SUM_REPORT_ITEMS(15),            & ! (15)
                           SUM_REPORT_ITEMS(16),            & ! (16)
                           AVE_TOTAL_COST,            &      !(17  !
                           AVE_VAR_COST,              &      !(18  !
                           0., & !  RES_TRACKER,
                           0., & !  FUEL_TRACKER
                           0.,  & !  RATE_TRACKER
                           0.,  & !  MEM_TRACKER
                   SUM_REPORT_ITEMS(19),            & !  EMISSION AMOUNT
               SUM_REPORT_ITEMS(20)             ! AVERAGE EMISSION RATE
         ENDIF
      RETURN
!

      ! CALLED AT END OF PRODUCTION LOOP IN PROCOST
      ENTRY WRITE_TRANS_ANNUAL_TYPE

         IF(SERVICE_TRANS_REPORT_NOT_OPEN) RETURN
         SEASON = LAST_SEASON_loc
         DO TRANS_NO = 1 , NO_OF_TRANS
            IF(.NOT. TRANSACTION_ACTIVE(TRANS_NO)) CYCLE
            IF( (ns_service_decs%ANNUAL_COSTS(1,TRANS_NO) &
            +ns_service_decs%ANNUAL_COSTS(5,TRANS_NO)) &
                                                           > 0.) THEN
               IMPUTTED_ENERGY = 1000. * &
               (ns_service_decs%ANNUAL_COSTS(2,TRANS_NO) + &
               ns_service_decs%ANNUAL_COSTS(6,TRANS_NO))/ &
                  (ns_service_decs%ANNUAL_COSTS(1,TRANS_NO) + &
                  ns_service_decs%ANNUAL_COSTS(5,TRANS_NO))
            ELSE
               IMPUTTED_ENERGY = 0.
            ENDIF
            IF(ns_service_decs%ANNUAL_COSTS(3,TRANS_NO) > 0.) THEN
               IMPUTTED_CAPACITY = &
                     ns_service_decs%ANNUAL_COSTS(4,TRANS_NO)/ &
                     ns_service_decs%ANNUAL_COSTS(3,TRANS_NO)
            ELSE
               IMPUTTED_CAPACITY = 0.
            ENDIF
            IF(COST_ASSIGN(TRANS_NO) == 'R') THEN
               COST_NO = 1
            ELSE
               COST_NO = 2
            ENDIF
            SELECT CASE (TRAN_TYPE(TRANS_NO))
            CASE ('T')
               TRANS_TYPE_NO = 1
            CASE ('S')
               TRANS_TYPE_NO = 2
            CASE ('W')
               TRANS_TYPE_NO = 3
            CASE ('D')
               TRANS_TYPE_NO = 4
            CASE ('O')
               TRANS_TYPE_NO = 5
            END SELECT
            TRANS_TYPE = TRANS_TYPE_NAME(TRANS_TYPE_NO)

            IF(COST_NO == 1) THEN
               COST_POSITION = NO_OF_TRANS+TRANS_TYPE_NO
            ELSE
               COST_POSITION=NO_OF_TRANS+NO_TRANS_TYPES+TRANS_TYPE_NO
            ENDIF
            ns_service_decs%ANNUAL_COSTS(1,COST_POSITION) = &
            ns_service_decs%ANNUAL_COSTS(1,COST_POSITION) + &
                         ns_service_decs%ANNUAL_COSTS(1,TRANS_NO)
            ns_service_decs%ANNUAL_COSTS(2,COST_POSITION) = &
                     ns_service_decs%ANNUAL_COSTS(2,COST_POSITION) + &
                        ns_service_decs%ANNUAL_COSTS(2,TRANS_NO)
            ns_service_decs%ANNUAL_COSTS(3,COST_POSITION) = &
                    ns_service_decs%ANNUAL_COSTS(3,COST_POSITION) + &
                             ns_service_decs%ANNUAL_COSTS(3,TRANS_NO)
            ns_service_decs%ANNUAL_COSTS(4,COST_POSITION) = &
                   ns_service_decs%ANNUAL_COSTS(4,COST_POSITION) + &
                            ns_service_decs%ANNUAL_COSTS(4,TRANS_NO)
            ns_service_decs%ANNUAL_COSTS(5,COST_POSITION) = &
                     ns_service_decs%ANNUAL_COSTS(5,COST_POSITION) + &
                           ns_service_decs%ANNUAL_COSTS(5,TRANS_NO)
            ns_service_decs%ANNUAL_COSTS(6,COST_POSITION) = &
                    ns_service_decs%ANNUAL_COSTS(6,COST_POSITION) + &
                            ns_service_decs%ANNUAL_COSTS(6,TRANS_NO)
!
            COST_ASSIGNMENT_NAME = ASSIGNMENT_NAME(COST_NO)
! WRITE EACH TRANSACTION
            WRITE(TRANS_RPT_NO,REC=TRANS_RPT_REC) PRT_ENDPOINT(), &
                        FLOAT(globecom_year+get_BASE_YEAR()), &
                        CL_MONTH_NAME(SEASON+1), &
                        TRANS_NAME(TRANS_NO), &
                        COST_ASSIGNMENT_NAME, &
                        TRANS_TYPE, &
                        ns_service_decs%ANNUAL_COSTS(1,TRANS_NO), &
                        IMPUTTED_ENERGY, &
                        ns_service_decs%ANNUAL_COSTS(2,TRANS_NO), &
                        ns_service_decs%ANNUAL_COSTS(3,TRANS_NO), &
                        IMPUTTED_CAPACITY, &
                        ns_service_decs%ANNUAL_COSTS(4,TRANS_NO), &
                        ns_service_decs%ANNUAL_COSTS(2,TRANS_NO)+ &
                          ns_service_decs%ANNUAL_COSTS(4,TRANS_NO)
            TRANS_RPT_REC = TRANS_RPT_REC + 1

            IF(PRINT_WABASH_COST_REPORT) THEN

               IF(ns_service_decs%ANNUAL_COSTS(4,TRANS_NO) == 0.) THEN
                  RPT_CAPACITY = 0.
               ELSE
                 RPT_CAPACITY = ns_service_decs%ANNUAL_COSTS(3,TRANS_NO)
               ENDIF
               IF(ns_service_decs%ANNUAL_COSTS(2,TRANS_NO) + &
                   ns_service_decs%ANNUAL_COSTS(6,TRANS_NO) == 0.) THEN
                  RPT_ENERGY = 0.
               ELSE
               RPT_ENERGY = ns_service_decs%ANNUAL_COSTS(1,TRANS_NO) + &
                             ns_service_decs%ANNUAL_COSTS(5,TRANS_NO)
               ENDIF
               KW_CAPACITY = RPT_CAPACITY*1000.
               CAP_FACTOR = 0.
               IF(RPT_ENERGY > 0.) THEN
                  AVE_VAR_COST = 1000.* &
                  (ns_service_decs%ANNUAL_COSTS(2,TRANS_NO)+&
                  ns_service_decs%ANNUAL_COSTS(6,TRANS_NO))/ &
                                                             RPT_ENERGY
  AVE_TOTAL_COST = 1000.*(ns_service_decs%ANNUAL_COSTS(2,TRANS_NO) + &
                    ns_service_decs%ANNUAL_COSTS(6,TRANS_NO) + &
                   ns_service_decs%ANNUAL_COSTS(4,TRANS_NO))/RPT_ENERGY
               ELSE
                  AVE_VAR_COST = 0.
                  AVE_TOTAL_COST = 0.
               ENDIF
               IF(EXPENSE_ASSIGN(TRANS_NO) == 'A') THEN
                  FUEL_CHARGE_RATE = IMPUTTED_ENERGY
                  FUEL_COST = ns_service_decs%ANNUAL_COSTS(6,TRANS_NO)
                  UNIT_ENERGY_COST = 0.
                  ENERGY_CHARGE_loc = 0.
               ELSE
                  UNIT_ENERGY_COST = IMPUTTED_ENERGY
                ENERGY_CHARGE_loc = &
                ns_service_decs%ANNUAL_COSTS(2,TRANS_NO)
                  FUEL_CHARGE_RATE = 0.
                  FUEL_COST = 0.
               ENDIF

               CALL GET_SERVICE_TRANS_TRACKERS(TRANS_NO, &
                                                   RES_TRACKER, &
                                                   FUEL_TRACKER, &
                                                   RATE_TRACKER, &
                                                   MEM_TRACKER)
!
               INQUIRE(UNIT=WABASH_REPORT_UNIT_NO, &
                                            NEXTREC=WABASH_REC_COUNTER)
               WRITE(WABASH_REPORT_UNIT_NO,REC=WABASH_REC_COUNTER) &
                           PRT_ENDPOINT(), &
                           FLOAT(globecom_year+get_BASE_YEAR()), &
                           CL_MONTH_NAME(SEASON+1), &
                           TRANS_NAME(TRANS_NO), &
                           KW_CAPACITY, &
                           RPT_ENERGY, &
                           0., &
                           KW_CAPACITY, &
                           RPT_ENERGY, &
                           IMPUTTED_CAPACITY, &
                           ns_service_decs%ANNUAL_COSTS(4,TRANS_NO), &
                           FUEL_CHARGE_RATE, &
                           FUEL_COST, &
                           UNIT_ENERGY_COST, &
                           ENERGY_CHARGE_loc, &
                           ns_service_decs%ANNUAL_COSTS(2,TRANS_NO) + &
                     ns_service_decs%ANNUAL_COSTS(4,TRANS_NO) + &
                   ns_service_decs%ANNUAL_COSTS(6,TRANS_NO), &
                           KW_CAPACITY, &
                           KW_CAPACITY, &
                           0., &
                           CAP_FACTOR, &
                           AVE_TOTAL_COST, &
                           AVE_VAR_COST, &
                           RES_TRACKER, &
                           FUEL_TRACKER, &
                           RATE_TRACKER, &
                           MEM_TRACKER, &
                           0.,  & !  EMISSION AMOUNT
                           0.   ! AVERAGE EMISSION RATE
            ENDIF ! PRINT_WABASH_COST_REPORT
         ENDDO ! TRANSACTIONS LOOP


! 11/26/03. MOVED UP
!
         SEASONAL_COST_ELEMENTS(1) = 0.
         SEASONAL_COST_ELEMENTS(2) = 0.
         SEASONAL_COST_ELEMENTS(3) = 0.
         SEASONAL_COST_ELEMENTS(4) = 0.
!         IF(COST_NO == 2) THEN
            SEASONAL_COST_ELEMENTS(5) = ANNUAL_FUEL_ENERGY
            SEASONAL_COST_ELEMENTS(6) = ANNUAL_FUEL_COST

         DO COST_NO = 1 , 2
            IF(COST_NO == 1) THEN
               TEMP_R = -1.0
            ELSE
               TEMP_R =  1.0
            ENDIF
            DO TRANS_TYPE_NO = 1 , NO_TRANS_TYPES
               IF(COST_NO == 1) THEN
                  COST_POSITION = NO_OF_TRANS+TRANS_TYPE_NO
               ELSE
                  COST_POSITION = NO_OF_TRANS + NO_TRANS_TYPES + &
                                                          TRANS_TYPE_NO
               ENDIF

               IF(ns_service_decs%ANNUAL_COSTS(1,COST_POSITION) + &
        ns_service_decs%ANNUAL_COSTS(5,COST_POSITION) > 0.) THEN
   IMPUTTED_ENERGY = &
         1000.* (ns_service_decs%ANNUAL_COSTS(2,COST_POSITION) + &
                     ns_service_decs%ANNUAL_COSTS(6,COST_POSITION))/ &
                (ns_service_decs%ANNUAL_COSTS(1,COST_POSITION) + &
                          ns_service_decs%ANNUAL_COSTS(5,COST_POSITION))
               ELSE
                  IMPUTTED_ENERGY = 0.
                  ns_service_decs%ANNUAL_COSTS(1,COST_POSITION) = 0.
               ENDIF
            IF(ns_service_decs%ANNUAL_COSTS(3,COST_POSITION) > 0.) THEN
                  IMPUTTED_CAPACITY = &
                        ns_service_decs%ANNUAL_COSTS(4,COST_POSITION)/ &
                          ns_service_decs%ANNUAL_COSTS(3,COST_POSITION)
               ELSE
                  IMPUTTED_CAPACITY = 0.
                  ns_service_decs%ANNUAL_COSTS(3,COST_POSITION) = 0.
               ENDIF
! WRITE TOTAL BY TRANSACTION TYPE
               WRITE(TRANS_RPT_NO,REC=TRANS_RPT_REC) PRT_ENDPOINT(), &
                        FLOAT(globecom_year+get_BASE_YEAR()), &
                        CL_MONTH_NAME(SEASON+1), &
                        'Total               ', &
                        ASSIGNMENT_NAME(COST_NO), &
                        TRANS_TYPE_NAME(TRANS_TYPE_NO), &
                        ns_service_decs%ANNUAL_COSTS(1,COST_POSITION), &
                        IMPUTTED_ENERGY, &
                        ns_service_decs%ANNUAL_COSTS(2,COST_POSITION), &
                        ns_service_decs%ANNUAL_COSTS(3,COST_POSITION), &
                        IMPUTTED_CAPACITY, &
                        ns_service_decs%ANNUAL_COSTS(4,COST_POSITION), &
                      ns_service_decs%ANNUAL_COSTS(2,COST_POSITION) + &
         ns_service_decs%ANNUAL_COSTS(4,COST_POSITION)
               TRANS_RPT_REC = TRANS_RPT_REC + 1
               SEASONAL_COST_ELEMENTS(1) = &
                         SEASONAL_COST_ELEMENTS(1) + &
                   ns_service_decs%ANNUAL_COSTS(1,COST_POSITION)*TEMP_R
               SEASONAL_COST_ELEMENTS(2) = &
                          SEASONAL_COST_ELEMENTS(2) + &
                ns_service_decs%ANNUAL_COSTS(2,COST_POSITION)*TEMP_R
               SEASONAL_COST_ELEMENTS(3) = &
                         SEASONAL_COST_ELEMENTS(3) + &
                ns_service_decs%ANNUAL_COSTS(3,COST_POSITION)*TEMP_R
               SEASONAL_COST_ELEMENTS(4) = &
                          SEASONAL_COST_ELEMENTS(4) + &
                ns_service_decs%ANNUAL_COSTS(4,COST_POSITION)*TEMP_R
            ENDDO ! TRANSACTION_TYPES LOOP
            IF(SEASONAL_COST_ELEMENTS(1) > 0.) THEN
               IMPUTTED_ENERGY = 1000.* &
                    SEASONAL_COST_ELEMENTS(2)/SEASONAL_COST_ELEMENTS(1)
            ELSE
               IMPUTTED_ENERGY = 0.
            ENDIF
            IF(SEASONAL_COST_ELEMENTS(3) > 0.) THEN
               IMPUTTED_CAPACITY = &
                    SEASONAL_COST_ELEMENTS(4)/SEASONAL_COST_ELEMENTS(3)
            ELSE
               IMPUTTED_CAPACITY = 0.
            ENDIF
! WRITE TOTAL BY COST TYPE
            WRITE(TRANS_RPT_NO,REC=TRANS_RPT_REC) PRT_ENDPOINT(), &
                        FLOAT(globecom_year+get_BASE_YEAR()), &
                        CL_MONTH_NAME(SEASON+1), &
                        'Total               ', &
                        ASSIGNMENT_NAME(COST_NO), &
                        'Total       ', &
                        SEASONAL_COST_ELEMENTS(1), &
                        IMPUTTED_ENERGY, &
                        SEASONAL_COST_ELEMENTS(2), &
                        SEASONAL_COST_ELEMENTS(3), &
                        IMPUTTED_CAPACITY, &
                        SEASONAL_COST_ELEMENTS(4), &
                        SEASONAL_COST_ELEMENTS(2)+ &
                                     SEASONAL_COST_ELEMENTS(4)
            TRANS_RPT_REC = TRANS_RPT_REC + 1

            IF(PRINT_WABASH_COST_REPORT .AND. COST_NO == 2) THEN
               IF(SEASONAL_COST_ELEMENTS(4) == 0.) THEN
                  RPT_CAPACITY = 0.
               ELSE
                  RPT_CAPACITY = SEASONAL_COST_ELEMENTS(3)
               ENDIF
               KW_CAPACITY = RPT_CAPACITY*1000.
               CAP_FACTOR = 0.
               IF( SEASONAL_COST_ELEMENTS(1) > 0.) THEN
                  AVE_VAR_COST = 1000.* SEASONAL_COST_ELEMENTS(2)/ &
                                              SEASONAL_COST_ELEMENTS(1)
                  AVE_TOTAL_COST = 1000.* &
                                  (SEASONAL_COST_ELEMENTS(2) + &
                                       SEASONAL_COST_ELEMENTS(4))/ &
                                             SEASONAL_COST_ELEMENTS(1)
               ELSE
                  AVE_VAR_COST = 0.
                  AVE_TOTAL_COST = 0.
               ENDIF
               IF( SEASONAL_COST_ELEMENTS(5) > 0.) THEN
                  FUEL_CHARGE_RATE = 1000.* SEASONAL_COST_ELEMENTS(6)/ &
                                              SEASONAL_COST_ELEMENTS(5)
               ELSE
                  FUEL_CHARGE_RATE = 0.
               ENDIF
               IF(SEASONAL_COST_ELEMENTS(1) - &
                                   SEASONAL_COST_ELEMENTS(5) > 0.) THEN
                  UNIT_ENERGY_COST = (SEASONAL_COST_ELEMENTS(2) - &
                                    SEASONAL_COST_ELEMENTS(6)) / &
                                (SEASONAL_COST_ELEMENTS(1) - &
                                             SEASONAL_COST_ELEMENTS(5))
               ELSE
                  UNIT_ENERGY_COST = 0.
               ENDIF
               INQUIRE(UNIT=WABASH_REPORT_UNIT_NO, &
                                            NEXTREC=WABASH_REC_COUNTER)
               WRITE(WABASH_REPORT_UNIT_NO,REC=WABASH_REC_COUNTER) &
                           PRT_ENDPOINT(), &
                           FLOAT(globecom_year+get_BASE_YEAR()), &
                           CL_MONTH_NAME(SEASON+1), &
                           WVPA_COST_ASSIGN(COST_NO), &
                           KW_CAPACITY, &
                           SEASONAL_COST_ELEMENTS(1), &
                           0., &
                           KW_CAPACITY, &
                           SEASONAL_COST_ELEMENTS(1), &
                           IMPUTTED_CAPACITY, &
                           SEASONAL_COST_ELEMENTS(4), &
                           FUEL_CHARGE_RATE, &
                           SEASONAL_COST_ELEMENTS(6), &
                           UNIT_ENERGY_COST, &
                           SEASONAL_COST_ELEMENTS(2), &
                           SEASONAL_COST_ELEMENTS(2) + &
                                    SEASONAL_COST_ELEMENTS(6) + &
             SEASONAL_COST_ELEMENTS(4), &
                           KW_CAPACITY, &
                           KW_CAPACITY, &
                           0., &
                           CAP_FACTOR, &
                           AVE_TOTAL_COST, &
                           AVE_VAR_COST, &
                           0., & !  RES_TRACKER,
                           0., & !  FUEL_TRACKER
                           0., & !  RATE_TRACKER
                           0.,  & !  MEM_TRACKER
                           0.,  & !  EMISSION AMOUNT
                           0.   ! AVERAGE EMISSION RATE
            ENDIF ! PRINT_WABASH_COST_REPORT .AND. COST_NO == 2
         ENDDO ! EXPENSE OR ns_service_decs%revenue
!
         IF(PRINT_WABASH_COST_REPORT) THEN
            CALL RETURN_ANN_SUM_REPORT_ITEMS(SUM_REPORT_ITEMS)
            IF(SUM_REPORT_ITEMS(1) > 0.) THEN
               UNIT_CAPACITY_COST = 1000.*(SUM_REPORT_ITEMS(7) + &
                         SEASONAL_COST_ELEMENTS(4))/SUM_REPORT_ITEMS(1)
            ELSE
               UNIT_CAPACITY_COST = 0.
            ENDIF
            IF(SUM_REPORT_ITEMS(5) > 0.) THEN
               UNIT_ENERGY_COST = &
                           1000.*(SEASONAL_COST_ELEMENTS(2) + &
                                            SUM_REPORT_ITEMS(11))/ &
                                                    SUM_REPORT_ITEMS(5)
               AVE_VAR_COST = 1000.*(SEASONAL_COST_ELEMENTS(2) + &
                      SUM_REPORT_ITEMS(9) + SUM_REPORT_ITEMS(11))/ &
                                                    SUM_REPORT_ITEMS(2)
               AVE_TOTAL_COST = &
                           1000.*(SEASONAL_COST_ELEMENTS(2) + &
                                 SEASONAL_COST_ELEMENTS(4) + &
                                      SUM_REPORT_ITEMS(12))/ &
                                                    SUM_REPORT_ITEMS(2)
            ELSE
               UNIT_ENERGY_COST = 0.
               AVE_VAR_COST = 0.
               AVE_TOTAL_COST = 0.
            ENDIF
            INQUIRE(UNIT=WABASH_REPORT_UNIT_NO, &
                                            NEXTREC=WABASH_REC_COUNTER)
            WRITE(WABASH_REPORT_UNIT_NO,REC=WABASH_REC_COUNTER) &
                           PRT_ENDPOINT(), &
                           FLOAT(globecom_year+get_BASE_YEAR()), &
                           CL_MONTH_NAME(SEASON+1), &
                           'Total Cost          ', &
                           SUM_REPORT_ITEMS(1),             & ! (1)
                           SUM_REPORT_ITEMS(2),             & ! (2)
                           SUM_REPORT_ITEMS(3),             & ! (3)
                           SUM_REPORT_ITEMS(4),             & ! (4)
                           SUM_REPORT_ITEMS(5),             & ! (5)
                           UNIT_CAPACITY_COST,             & !(6  !
                           SEASONAL_COST_ELEMENTS(4) + &
                                    SUM_REPORT_ITEMS(7),    & ! (7)
                           SUM_REPORT_ITEMS(8),             & ! (8)
                           SUM_REPORT_ITEMS(9) + &
                SEASONAL_COST_ELEMENTS(6), & !(9)
               UNIT_ENERGY_COST,                & ! (10)! ENERGY $/MWH
                           SEASONAL_COST_ELEMENTS(2) + &
                            SUM_REPORT_ITEMS(11),   & ! (11)! ENERGY $
                           SEASONAL_COST_ELEMENTS(2) + &
                               SEASONAL_COST_ELEMENTS(4) + &
                                   SEASONAL_COST_ELEMENTS(6) + &
                               SUM_REPORT_ITEMS(12),  & ! (12)! TOTAL $
                           SUM_REPORT_ITEMS(13),            & ! (13)
                           SUM_REPORT_ITEMS(14),            & ! (14)
                           SUM_REPORT_ITEMS(15),            & ! (15)
                           SUM_REPORT_ITEMS(16),            & ! (16)
                           AVE_TOTAL_COST,                &  !(17  !
                           AVE_VAR_COST,                   &  !(18  !
                           0., & !  RES_TRACKER,
                           0., & !  FUEL_TRACKER
                           0., & !  RATE_TRACKER
                           0.,  & !  MEM_TRACKER
                           SUM_REPORT_ITEMS(19),  & !  EMISSION AMOUNT
                         SUM_REPORT_ITEMS(20)   ! AVERAGE EMISSION RATE
         ENDIF ! PRINT_WABASH_COST_REPORT
!
      RETURN


      ENTRY INIT_TRANS_ANNUAL_COSTS

         IF(.NOT. ALLOCATED(ns_service_decs%ANNUAL_COSTS)) RETURN
         ns_service_decs%ANNUAL_COSTS = 0.
         TRANSACTION_ACTIVE = FALSE_BYTE
         ANNUAL_FUEL_ENERGY = 0.
         ANNUAL_FUEL_COST = 0.
      RETURN
END subroutine transaction_manager

subroutine GET_TRANS_VARIABLES(NumTrans, R_NAME,R_COST_ASSIGN, &
                         R_EXPENSE_ASSIGN,R_TRAN_TYPE)
  use string
  implicit none
  integer (kind=2) :: NumTrans, ub
  CHARACTER (len=20) :: R_NAME(NumTrans)
  CHARACTER (len=1) :: R_COST_ASSIGN(NumTrans)
  character (len=1) :: R_EXPENSE_ASSIGN(NumTrans)
  character (len=1) :: R_TRAN_TYPE(NumTrans)
  integer :: I
  integer (kind=2) :: dbgNumTrans

         dbgNumTrans=ns_service_decs%NUM_SERVICE_TRANS


         if(.not. allocated(ns_service_decs%SERVICE_NAME)) then
           call end_program("servicac:0007 - " // &
       "Error: ns_service_decs%SERVICE_NAME is not allocated.")

         end if

         ub=ubound(ns_service_decs%SERVICE_NAME, 1)
         if(ns_service_decs%SERVICE_TRANS>ub) then
           call end_program("servicac:0003 - " // &
       "Error: About to enter a loop that will access element " // &
       trim(itos(int(ns_service_decs%SERVICE_TRANS))) // " " // &
       "of the service_name array, but there are " // &
       "only " // trim(itos(int(ub))) // " elements allocated.")
         end if

         ub=ubound(ns_service_decs%Service_Cost_assignment,1)

         if(ns_service_decs%SERVICE_TRANS>ub) then
           call end_program("servicac:0004 - " // &
       "Error: About to enter a loop that will access element " // &
       trim(itos(int(ns_service_decs%SERVICE_TRANS))) // " " // &
       "of the service_cost_assignment array, but there are " // &
       "only " // trim(itos(int(ub))) // " elements allocated.")
         end if

        ub=ubound(ns_service_decs%SERVICE_EXPENSE_COLLECTION,1)

         if(ns_service_decs%SERVICE_TRANS>ub) then
           call end_program("servicac:0005 - " // &
       "Error: About to enter a loop that will access element " // &
       trim(itos(int(ns_service_decs%SERVICE_TRANS))) // " " // &
       "of the SERVICE_EXPENSE_COLLECTION array, but there are " // &
       "only " // trim(itos(int(ub))) // " elements allocated.")
         end if

       ub=ubound(ns_service_decs%TYPE_OF_SERVICE,1)

         if(ns_service_decs%SERVICE_TRANS>ub) then
           call end_program("servicac:0006 - " // &
       "Error: About to enter a loop that will access element " // &
       trim(itos(int(ns_service_decs%SERVICE_TRANS))) // " " // &
       "of the TYPE_OF_SERVICE array, but there are " // &
       "only " // trim(itos(int(ub))) // " elements allocated.")
         end if

         DO I = 1, ns_service_decs%SERVICE_TRANS
            R_NAME(I) = ns_service_decs%SERVICE_NAME(I)
           R_COST_ASSIGN(I) = ns_service_decs%SERVICE_COST_ASSIGNMENT(I)
            R_EXPENSE_ASSIGN(I) = &
       ns_service_decs%SERVICE_EXPENSE_COLLECTION(I)
            R_TRAN_TYPE(I) = ns_service_decs%TYPE_OF_SERVICE(I)
         ENDDO

end subroutine GET_TRANS_VARIABLES
subroutine  UPDATE_SERVICE_REPORT_SWITCH()
use prod_arrays_dimensions

     implicit none
         IF(.NOT. TESTING_PLAN) THEN
               if (SERVICE_REPORT() /= 'F') then
                ns_service_decs%MONTHLY_SERVICE_REPORT_ACTIVE = .true.
               else
                ns_service_decs%MONTHLY_SERVICE_REPORT_ACTIVE=.false.
               end if
         ELSE
            ns_service_decs%MONTHLY_SERVICE_REPORT_ACTIVE = .FALSE.
         ENDIF
end subroutine UPDATE_SERVICE_REPORT_SWITCH


subroutine MONTHLY_SERVICE_TRANS_EXPENSES(R_CLASS, &
                                           MONTH_VARS)
       use service_defs
       implicit none
       integer (kind=2) :: r_class
       REAL (kind=4) ::  MONTH_VARS(0:12,*)
	   

         IF(ns_service_decs%SERVICE_TRANS <= 0) then
            RETURN
        end if

         IF(R_CLASS <= MAX_SERVICE_CLASS_ID_NUM) THEN
            ASSET_CLASS_sd = R_CLASS
            IF(ASSET_CLASS_sd > 0 .OR. R_CLASS == -1) THEN
               DO MO_sd = 0, 12

! EXPENSES

                  MONTH_VARS(MO_sd,FossilFuel) = &
                          MONTH_VARS(MO_sd,FossilFuel) + &
                 ST_ANN_CLASS_EXPENSE(MO_sd,ASSET_CLASS_sd,11)
!
                  MONTH_VARS(MO_sd,PurchasedPower) = &
                          MONTH_VARS(MO_sd,PurchasedPower) + &
                   ST_ANN_CLASS_EXPENSE(MO_sd,ASSET_CLASS_sd,12)
!
                  MONTH_VARS(MO_sd,VariableOandM) = &
                           MONTH_VARS(MO_sd,VariableOandM) + &
                   ST_ANN_CLASS_EXPENSE(MO_sd,ASSET_CLASS_sd,13)
!
                  MONTH_VARS(MO_sd,FixedOandM) = &
                         MONTH_VARS(MO_sd,FixedOandM) + &
                    ST_ANN_CLASS_EXPENSE(MO_sd,ASSET_CLASS_sd,14)
!
                  MONTH_VARS(MO_sd,OtherOandM) = &
                          MONTH_VARS(MO_sd,OtherOandM) + &
                   ST_ANN_CLASS_EXPENSE(MO_sd,ASSET_CLASS_sd,15)
!
                  MONTH_VARS(MO_sd,PurchasedGas) = &
                          MONTH_VARS(MO_sd,PurchasedGas) + &
                   ST_ANN_CLASS_EXPENSE(MO_sd,ASSET_CLASS_sd,16)
!
                  MONTH_VARS(MO_sd,Other) = &
                          MONTH_VARS(MO_sd,Other) + &
               ST_ANN_CLASS_EXPENSE(MO_sd,ASSET_CLASS_sd,17)
!
                  MONTH_VARS(MO_sd,OwnedNuclearFuel) = &
                          MONTH_VARS(MO_sd,OwnedNuclearFuel) + &
           ST_ANN_CLASS_EXPENSE(MO_sd,ASSET_CLASS_sd,18)
!
                  MONTH_VARS(MO_sd,LeasedNuclearFuel) = &
                          MONTH_VARS(MO_sd,LeasedNuclearFuel) + &
                ST_ANN_CLASS_EXPENSE(MO_sd,ASSET_CLASS_sd,19)
!
                  MONTH_VARS(MO_sd,DSMExpense) = &
                          MONTH_VARS(MO_sd,DSMExpense) + &
                    ST_ANN_CLASS_EXPENSE(MO_sd,ASSET_CLASS_sd,20)
!
                  MONTH_VARS(MO_sd,DSMRebate) = &
                          MONTH_VARS(MO_sd,DSMRebate) + &
                    ST_ANN_CLASS_EXPENSE(MO_sd,ASSET_CLASS_sd,21)
!
                  MONTH_VARS(MO_sd,ATLBookLeaseExpense) = &
                          MONTH_VARS(MO_sd,ATLBookLeaseExpense) + &
                     ST_ANN_CLASS_EXPENSE(MO_sd,ASSET_CLASS_sd,22)
!
                  MONTH_VARS(MO_sd,ServiceTransactions) = &
                          MONTH_VARS(MO_sd,ServiceTransactions) + &
                      ST_ANN_CLASS_EXPENSE(MO_sd,ASSET_CLASS_sd,23)
!
                  MONTH_VARS(MO_sd,EmissionCredits) = &
                          MONTH_VARS(MO_sd,EmissionCredits) + &
                   ST_ANN_CLASS_EXPENSE(MO_sd,ASSET_CLASS_sd,24)
!
                  MONTH_VARS(MO_sd,DOEDecommissioning) = &
                          MONTH_VARS(MO_sd,DOEDecommissioning) + &
                  ST_ANN_CLASS_EXPENSE(MO_sd,ASSET_CLASS_sd,25)
!
                  MONTH_VARS(MO_sd,DOEDisposal) = &
                          MONTH_VARS(MO_sd,DOEDisposal) + &
                  ST_ANN_CLASS_EXPENSE(MO_sd,ASSET_CLASS_sd,26)
!
                  MONTH_VARS(MO_sd,CatawbaExpenses) = &
                          MONTH_VARS(MO_sd,CatawbaExpenses) + &
                  ST_ANN_CLASS_EXPENSE(MO_sd,ASSET_CLASS_sd,27)
!
                  MONTH_VARS(MO_sd,BTLExpenses) = &
                         MONTH_VARS(MO_sd,BTLExpenses) + &
                       ST_ANN_CLASS_EXPENSE(MO_sd,ASSET_CLASS_sd,28)
!
                  MONTH_VARS(MO_sd,BTLLeaseCash) = &
                          MONTH_VARS(MO_sd,BTLLeaseCash) + &
                           ST_BTL_LEASE_PAYMENT(MO_sd,ASSET_CLASS_sd)
!
                  MONTH_VARS(MO_sd,Exp_Collection_in_Adj_Clause) = &
                          MONTH_VARS(MO_sd,Exp_Collection_in_Adj_Clause) + &
                       ST_ANN_CLASS_ADJ_CLAUSE(MO_sd,ASSET_CLASS_sd)



               ENDDO
            ENDIF
         ENDIF
end subroutine MONTHLY_SERVICE_TRANS_EXPENSES


end module service_decs
