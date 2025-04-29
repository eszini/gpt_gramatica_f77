
!     REAENRG.FOR
!     Copyright(c)  2000
!
!     Created: 6/25/2003 2:50:17 PM
!     Author : MARK S GERBER
!     Last change: gt 11/15/2013 10:12:29 AM


      SUBROUTINE REA_ENRG_CAP(ISEAS,ENERGY,CAPACITY,
     +                          res_ids,nunits_loc, UNITNM,SEAS_HOURS,
     +                          RESOURCE_FILE_TYPE,REPORT,
     +                          RESOURCE_TYPE,YR,
     +                          RESOURCE_ANNUAL_AREA_ENRG,
     +                          RESOURCE_ANNUAL_AREA_CAP,
     +                          ANNUAL_EMERGENCY_ENERGY,
     +                          ANNUAL_MAINTENANCE_ENERGY,
     +                          ANNUAL_AREA_ENERGY_LOSSES,
     +                          online_loc,ofline_loc,
     +                          DATE1,DATE2,
     +                          EA,REMAINING_ENERGY)

      use SpinDriftLib
      use prod_arrays_dimensions
      use production
	  use kepcocom
      use sizecom
      use res_decs
      use cls_load
	  

      implicit none
      integer (kind=2) :: nunits_loc
      integer (kind=2) :: res_ids(NUNITS_loc)
      INTEGER (kind=2) :: ISEAS,YR
      INTEGER (kind=2) :: online_loc(NUNITS_loc),ofline_loc(NUNITS_loc),
     +          DATE1,DATE2
      REAL ::  ENERGY(2*NUNITS_loc),CAPACITY(NUNITS_loc)
      REAL ::  EA(NUNITS_loc),REMAINING_ENERGY
      REAL ::  RESOURCE_ANNUAL_AREA_ENRG(6,NUNITS_loc)
      REAL ::  RESOURCE_ANNUAL_AREA_CAP(6,NUNITS_loc)
      REAL ::  ANNUAL_EMERGENCY_ENERGY(NUNITS_loc)
      REAL ::  ANNUAL_MAINTENANCE_ENERGY(NUNITS_loc)
      REAL ::  ANNUAL_AREA_ENERGY_LOSSES(NUNITS_loc)
      REAL ::  PERIOD_MAINTENANCE_ENERGY,PERIOD_EMERGENCY_ENERGY
      REAL ::  UNIT_ENERGY,UNIT_CAPACITY,SEAS_HOURS
      REAL ::  TOTAL_CAP_ALLOCATED,TOTAL_ENRG_ALLOCATED
      REAL ::  GET_VAR,RESOURCE_MAKE_UP_ENERGY,RESOURCE_LOSSES
      REAL ::  TOTAL_RESOURCE_ENRG_IN_AREA_1,
     +     TOTAL_RESOURCE_CAP_IN_AREA_1,
     +     R_TOTAL_RESOURCE_CAP_IN_AREA_1,
     +     CAPACITY_AFTER_LOSSES

      CHARACTER (len=20) ::  UNITNM(NUNITS_loc)
      character (len=2) :: RESOURCE_FILE_TYPE
      CHARACTER (len=1) ::  RESOURCE_TYPE(NUNITS_loc)
      INTEGER (kind=2) ::  ARRAY_POINTR
      INTEGER (kind=1) ::  AREA,I,LAST_AREA
      CHARACTER (len=20) ::  MONTH_NAME
      LOGICAL (kind=1) ::  CL_RESOURCES,CT_RESOURCES,REPORT,CONTROL_AREA
      REAL ::  RESOURCE_ENRG(6),RESOURCE_CAP(6)
      REAL ::  CAP_ALLOCATOR,ENRG_ALLOCATOR
      REAL ::  RESOURCE_ENRG_IN_AREA_1,
     +     RESOURCE_CAP_IN_AREA_1
      SAVE TOTAL_RESOURCE_ENRG_IN_AREA_1,TOTAL_RESOURCE_CAP_IN_AREA_1

      ! Added by John during merge
      call allocate_resource_ids_array(NUNITS_loc)
      CL_RESOURCES =  INDEX(RESOURCE_FILE_TYPE,'CL') /= 0
      CT_RESOURCES =  INDEX(RESOURCE_FILE_TYPE,'CT') /= 0
      CAPACITY_ALLOCATION_ABSOLUTE = .FALSE.
      ENERGY_ALLOCATION_ABSOLUTE = .FALSE.
      IF(REALLY_KEPCO) THEN
         ENERGY_ALLOCATION_ABSOLUTE = .TRUE.
         CAPACITY_ALLOCATION_ABSOLUTE = .TRUE.
      ENDIF
      IF(REPORT .AND. PRINT_HEADER) THEN
         TOTAL_RESOURCE_ENRG_IN_AREA_1 = 0.
         TOTAL_RESOURCE_CAP_IN_AREA_1 = 0.
         PRINT_HEADER = .FALSE.
         CALL DETAILED_REPORT_HEADER
         WRITE(9,'("&",2X,A21,20X,A/)') MONTH_NAME(ISEAS),
     +                                 'Control Area Allocations'
         IF(REALLY_KEPCO) THEN
            WRITE(9,'(1X,A20,T26,10(A))')    'Resource     ',
     +                                  '   Total',
     +                                  '     WPE',
     +                                  '    KP&L',
     +                                  '    KG&E',
     +                                  '   KCP&L',
     +                                  '     EDE'
            IF(NUMBER_OF_AREAS == 6) WRITE(9,'("&",A)') '  Area 6'
            WRITE(9,'("&",A)') '  Losses',
     +                         '  Emerg ',
     +                         '  Maint '
         ELSEIF(WABASH_VALLEY) THEN
            IF(WABASH_IM_NIPSCO_PSI) THEN
               WRITE(9,'(1X,T37,A)')
     +                   '---------- Area 1 -----------  Area 2  Area 3'
            ELSE
               WRITE(9,'(1X,T37,A)')
     +                   '------- Area 1 ------  Area 2  Area 3  Area 4'
            ENDIF
            WRITE(9,'(1X,A20,T26,A,9(A))') 'Resource     ',
     +                               '   Total',
     +                               '   Total ',
     +                               '    I&M ',
     +                               '    NIP',
     +                               '     PSI',
     +                               '      GM',
     +                               '    IP&L',
     +                               '  Losses',
     +                               '   Emerg',
     +                               '   Maint'
         ELSE
            WRITE(9,'(1X,A20,T26,A,6(A,I1),3(A))') 'Resource     ',
     +                               '   Total',
     +                              ('  Area ',I,I= 1, 6),
     +                               '  Losses',
     +                               '  Emerg ',
     +                               '  Maint '
         ENDIF

      ENDIF

      DO I = 1, NUNITS_loc
         IF(online_loc(I) > DATE2 .OR. ofline_loc(I) < DATE1) CYCLE
         TOTAL_ENRG_ALLOCATED = 0.
         TOTAL_CAP_ALLOCATED = 0.
         IF(CL_RESOURCES) THEN
            UNIT_ENERGY = ENERGY(2*I-1) + ENERGY(2*I)
            CONTROL_AREA = .FALSE.
            IF(I == GIBSON_BACKUP_POINTER) THEN
               ARRAY_POINTR =

     +                   CL_ALLOCATE_POINTR(res_ids(GIBSON_POINTER))

            ELSE
               ARRAY_POINTR = CL_ALLOCATE_POINTR(res_ids(I))
            ENDIF
         ELSEIF(CT_RESOURCES) THEN
            UNIT_ENERGY = ENERGY(I)
            CONTROL_AREA=INDEX('123456EMSRXKL',RESOURCE_TYPE(I)) /= 0
            ARRAY_POINTR = CT_ALLOCATE_POINTR(res_ids(I))
         ENDIF
         RESOURCE_MAKE_UP_ENERGY = -UNIT_ENERGY
         RESOURCE_LOSSES = 0.
         IF(.NOT. CONTROL_AREA) THEN
            DO AREA = 1, NUMBER_OF_AREAS
!
! ALLOCATE CAPACITY FIRST
!
               UNIT_CAPACITY = CAPACITY(I) *
     +                (1.-AREA_CAPACITY_LOSSES(AREA,ISEAS,ARRAY_POINTR))
               CAP_ALLOCATOR =
     +                      AREA_CAP_ALLOCATORS(AREA,ISEAS,ARRAY_POINTR)
               IF(CAP_ALLOCATOR < 0.) CAP_ALLOCATOR =
     +                               GET_VAR(CAP_ALLOCATOR,YR,UNITNM(I))
               IF(CAP_ALLOCATOR <= 1. .AND.
     +                          .NOT. CAPACITY_ALLOCATION_ABSOLUTE) THEN
                  CAP_ALLOCATOR = CAP_ALLOCATOR * UNIT_CAPACITY
               ENDIF
               RESOURCE_CAP(AREA) = CAP_ALLOCATOR
!
! ALLOCATE ENERGY
!
               ENRG_ALLOCATOR =
     +                     AREA_ENRG_ALLOCATORS(AREA,ISEAS,ARRAY_POINTR)

               IF(WABASH_VALLEY .AND. I == GIBSON_BACKUP_POINTER) THEN
                  RESOURCE_ENRG(AREA) = ENRG_ALLOCATOR
               ELSE
                  IF(ENRG_ALLOCATOR < 0.) ENRG_ALLOCATOR =
     +                              GET_VAR(ENRG_ALLOCATOR,YR,UNITNM(I))
                  IF(ENRG_ALLOCATOR <= 2. .AND.
     +                            .NOT. ENERGY_ALLOCATION_ABSOLUTE) THEN
                     ENRG_ALLOCATOR = ENRG_ALLOCATOR * UNIT_CAPACITY *
     +                                                        SEAS_HOURS
                  ELSEIF(ENRG_ALLOCATOR <= SEAS_HOURS)THEN
                     ENRG_ALLOCATOR = ENRG_ALLOCATOR * UNIT_CAPACITY
                  ENDIF
                  RESOURCE_ENRG(AREA) = ENRG_ALLOCATOR
               ENDIF
!
! TOTALS
!
               TOTAL_ENRG_ALLOCATED = TOTAL_ENRG_ALLOCATED +
     +                                               RESOURCE_ENRG(AREA)
               TOTAL_CAP_ALLOCATED = TOTAL_CAP_ALLOCATED +
     +                                            RESOURCE_CAP(AREA)
            ENDDO ! AREA
!
! ADJUST FOR RELATIVE ALLOCATIONS
!
            DO AREA = 1, NUMBER_OF_AREAS
               UNIT_CAPACITY = CAPACITY(I) *
     +                (1.-AREA_CAPACITY_LOSSES(AREA,ISEAS,ARRAY_POINTR))
               IF(.NOT. CAPACITY_ALLOCATION_ABSOLUTE .AND.
     +                                   TOTAL_CAP_ALLOCATED /= 0.) THEN
                  RESOURCE_CAP(AREA) = UNIT_CAPACITY *
     +                          (RESOURCE_CAP(AREA)/TOTAL_CAP_ALLOCATED)
               ENDIF
               AREA_CAPACITY(AREA,ISEAS) = AREA_CAPACITY(AREA,ISEAS) +
     +                                         RESOURCE_CAP(AREA)
               RESOURCE_ANNUAL_AREA_CAP(AREA,I) = MAX(
     +                  RESOURCE_ANNUAL_AREA_CAP(AREA,I),
     +                                               RESOURCE_CAP(AREA))
!
               IF(ENERGY_ALLOCATION_ABSOLUTE) THEN
                  RESOURCE_ENRG(AREA) = RESOURCE_ENRG(AREA)/SEAS_HOURS
               ELSEIF(TOTAL_ENRG_ALLOCATED /= 0.) THEN
                  RESOURCE_ENRG(AREA) = UNIT_ENERGY *
     +                        (RESOURCE_ENRG(AREA)/TOTAL_ENRG_ALLOCATED)
               ENDIF
               RESOURCE_MAKE_UP_ENERGY = RESOURCE_MAKE_UP_ENERGY +
     +                                               RESOURCE_ENRG(AREA)
               IF(WABASH_VALLEY) THEN
                  AREA_ENERGY(AREA,ISEAS) = AREA_ENERGY(AREA,ISEAS) +
     +                                               RESOURCE_ENRG(AREA)
                  RESOURCE_LOSSES = RESOURCE_LOSSES+RESOURCE_ENRG(AREA)*
     +                  AREA_RESOURCE_LOSES(AREA,ISEAS,ARRAY_POINTR)/
     +                (1.-AREA_RESOURCE_LOSES(AREA,ISEAS,ARRAY_POINTR))
                  RESOURCE_ANNUAL_AREA_ENRG(AREA,I) =
     +                               RESOURCE_ANNUAL_AREA_ENRG(AREA,I) +
     +                                  SEAS_HOURS * RESOURCE_ENRG(AREA)
               ELSE
                  AREA_ENERGY(AREA,ISEAS) = AREA_ENERGY(AREA,ISEAS) +
     +             (1. - AREA_RESOURCE_LOSES(AREA,ISEAS,ARRAY_POINTR)) *
     +                                               RESOURCE_ENRG(AREA)
                  RESOURCE_LOSSES = RESOURCE_LOSSES +
     +                    AREA_RESOURCE_LOSES(AREA,ISEAS,ARRAY_POINTR) *
     +                                               RESOURCE_ENRG(AREA)
                  RESOURCE_ANNUAL_AREA_ENRG(AREA,I) =
     +                  RESOURCE_ANNUAL_AREA_ENRG(AREA,I) +
     +             (1. - AREA_RESOURCE_LOSES(AREA,ISEAS,ARRAY_POINTR)) *
     +                                  SEAS_HOURS * RESOURCE_ENRG(AREA)
               ENDIF
            ENDDO ! AREA
            IF(EA(I) > 0. .AND. CL_RESOURCES .AND. REALLY_KEPCO) THEN
               PERIOD_EMERGENCY_ENERGY = UNIT_ENERGY * (1.-EA(I))/EA(I)
               PERIOD_MAINTENANCE_ENERGY = RESOURCE_MAKE_UP_ENERGY -
     +                                           PERIOD_EMERGENCY_ENERGY
            ELSE

               PERIOD_EMERGENCY_ENERGY = REMAINING_ENERGY
               REMAINING_ENERGY = 0.
               PERIOD_MAINTENANCE_ENERGY = 0.
            ENDIF
            EMERGENCY_ENERGY = EMERGENCY_ENERGY +
     +                              SEAS_HOURS * PERIOD_EMERGENCY_ENERGY
            ANNUAL_EMERGENCY_ENERGY(I) = ANNUAL_EMERGENCY_ENERGY(I) +
     +                              SEAS_HOURS * PERIOD_EMERGENCY_ENERGY
            MAINTENANCE_ENERGY = MAINTENANCE_ENERGY +
     +                            SEAS_HOURS * PERIOD_MAINTENANCE_ENERGY
            ANNUAL_MAINTENANCE_ENERGY(I) = ANNUAL_MAINTENANCE_ENERGY(I)+
     +                            SEAS_HOURS * PERIOD_MAINTENANCE_ENERGY
            AREA_ENERGY_LOSSES = AREA_ENERGY_LOSSES +
     +                                      SEAS_HOURS * RESOURCE_LOSSES
            ANNUAL_AREA_ENERGY_LOSSES(I) = ANNUAL_AREA_ENERGY_LOSSES(I)+
     +                                      SEAS_HOURS * RESOURCE_LOSSES
            IF(REPORT .AND. .NOT. CONTROL_AREA .AND.
     +                                         .NOT. WABASH_VALLEY) THEN
               IF(CL_RESOURCES) THEN
                  WRITE(9,1000)trim(UNITNM(I))//' (GWh)',
     +                      RESOURCE_FILE_TYPE,
     +                      SEAS_HOURS*UNIT_ENERGY/1000.,
     +                     (SEAS_HOURS*RESOURCE_ENRG(AREA)*
     +        (1. - AREA_RESOURCE_LOSES(AREA,ISEAS,ARRAY_POINTR))/1000.,
     +                                       AREA = 1, NUMBER_OF_AREAS),
     +                     SEAS_HOURS * RESOURCE_LOSSES/1000.,
     +                     SEAS_HOURS * PERIOD_EMERGENCY_ENERGY/1000.,
     +                     SEAS_HOURS * PERIOD_MAINTENANCE_ENERGY/1000.
               ELSE
                  WRITE(9,1000)trim(UNITNM(I))//' (GWh)',
     +                      RESOURCE_FILE_TYPE,
     +                      SEAS_HOURS*UNIT_ENERGY/1000.,
     +                     (SEAS_HOURS*RESOURCE_ENRG(AREA)*
     +        (1. - AREA_RESOURCE_LOSES(AREA,ISEAS,ARRAY_POINTR))/1000.,
     +                                       AREA = 1, NUMBER_OF_AREAS),
     +                     SEAS_HOURS * RESOURCE_LOSSES/1000.
               ENDIF
               WRITE(9,1000) '(MW)',' ',CAPACITY(I),
     +                    (RESOURCE_CAP(AREA),AREA = 1, NUMBER_OF_AREAS)
            ENDIF
            IF(REPORT .AND. .NOT. CONTROL_AREA .AND. WABASH_VALLEY) THEN
               RESOURCE_ENRG_IN_AREA_1= 0.
               RESOURCE_CAP_IN_AREA_1 = 0.
               CAPACITY_AFTER_LOSSES = 0.

               DO AREA = 1, MAX_LOAD_CLASSES
                  IF(CLASS_CONTROL_AREA(AREA) == 1) THEN
                     RESOURCE_ENRG_IN_AREA_1 = RESOURCE_ENRG_IN_AREA_1 +
     +                                  SEAS_HOURS * RESOURCE_ENRG(AREA)

                     RESOURCE_CAP_IN_AREA_1 = RESOURCE_CAP_IN_AREA_1 +
     +                                                RESOURCE_CAP(AREA)
                  ENDIF
                  CAPACITY_AFTER_LOSSES = CAPACITY_AFTER_LOSSES +
     +                                                RESOURCE_CAP(AREA)
               ENDDO

               TOTAL_RESOURCE_ENRG_IN_AREA_1 = RESOURCE_ENRG_IN_AREA_1 +
     +                                     TOTAL_RESOURCE_ENRG_IN_AREA_1
               TOTAL_RESOURCE_CAP_IN_AREA_1 = RESOURCE_CAP_IN_AREA_1 +
     +                                      TOTAL_RESOURCE_CAP_IN_AREA_1
               WRITE(9,1000)trim(UNITNM(I))//' (GWh)',
     +                   RESOURCE_FILE_TYPE,
     +                   SEAS_HOURS*(UNIT_ENERGY+RESOURCE_LOSSES)/1000.,
     +                   RESOURCE_ENRG_IN_AREA_1/1000.,
     +                  (SEAS_HOURS*RESOURCE_ENRG(AREA)/1000.,
     +                                       AREA = 1, NUMBER_OF_AREAS),
     +                   SEAS_HOURS * RESOURCE_LOSSES/1000.
               IF(CL_RESOURCES)
     +            WRITE(9,'("&",2F8.1)')
     +                      SEAS_HOURS * PERIOD_EMERGENCY_ENERGY/1000.,
     +                      SEAS_HOURS * PERIOD_MAINTENANCE_ENERGY/1000.
               WRITE(9,1000) '(MW)',' ',CAPACITY(I),
     +                    RESOURCE_CAP_IN_AREA_1,
     +                    (RESOURCE_CAP(AREA),AREA=1, NUMBER_OF_AREAS),
     +                    MAX(0.,CAPACITY(I) - CAPACITY_AFTER_LOSSES)
            ENDIF
         ENDIF
         IF(RESOURCE_LOSSES /= 0. .AND. WABASH_VALLEY) THEN
            IF(CL_RESOURCES) THEN
               IF(ENERGY(2*I-1) == 0.) THEN
                  ENERGY(2*I) = ENERGY(2*I) + RESOURCE_LOSSES
               ELSE
                  UNIT_ENERGY=ENERGY(2*I-1)/UNIT_ENERGY*RESOURCE_LOSSES
                  ENERGY(2*I-1) = ENERGY(2*I-1) + UNIT_ENERGY
                  ENERGY(2*I) = ENERGY(2*I)+RESOURCE_LOSSES-UNIT_ENERGY
               ENDIF
            ELSE
               ENERGY(I) = ENERGY(I) + RESOURCE_LOSSES
            ENDIF
         ENDIF
      ENDDO
      IF(MAINTENANCE_ENERGY > 0.) FIRST_MAINTENANCE_PERIOD = MIN(ISEAS,
     +                                         FIRST_MAINTENANCE_PERIOD)
      RETURN

      ENTRY PRINT_CONTROL_AREA_TOTALS

         WRITE(9,1010) 'Area Resources (GWh)',
     +                               TOTAL_RESOURCE_ENRG_IN_AREA_1/1000.
         WRITE(9,1010) 'Area Resources (MW)',
     +                                      TOTAL_RESOURCE_CAP_IN_AREA_1
      RETURN

      ENTRY RETURN_AREA_1_CAPACITY(R_TOTAL_RESOURCE_CAP_IN_AREA_1)

         R_TOTAL_RESOURCE_CAP_IN_AREA_1 = TOTAL_RESOURCE_CAP_IN_AREA_1
      RETURN
 1000 FORMAT(1X,A20,2X,A2,12F8.1)
 1010 FORMAT(1X,A20,12X,7F8.1)
      END subroutine REA_ENRG_CAP

      SUBROUTINE REA_LAST_RESOURCE(ISEAS,SEAS_HOURS,REPORT,
     +                               CONTRACT_CAPACITY,CONTRACT_ENERGY,
     +                               RESOURCE_TYPE,
     +                               MAXIMUM_CAPACITY,
     +                               MAXIMUM_ENERGY,
     +                               MINIMUM_CAPACITY,
     +                               DEMAND,PEAK)

	  use kepcocom

      use contracts_data
      use endpoint
      use globecom
      use grx_planning_routines
      use irec_endpoint_control
      use kepcocom
      use mod_base_year
      use prod_arrays_dimensions
      use prodcom
      use sizecom
      use spindriftlib
	  use production
	  use enrg_helper
	  use lamcom
      use cls_load
	  use forecast

      implicit none
      
      INTEGER (kind=2) ::  ISEAS,CLASSES_IN_AREA_1
      INTEGER (kind=1) ::  AREA,CONTRACT
      REAL ::  AREA_FORECAST,SEAS_HOURS
      REAL ::  TOTAL_LAST_ENERGY
      REAL ::  TOTAL_LAST_CAPACITY,TOTAL_SURPLUS_ENERGY
      REAL ::  TOTAL_AREA_ENERGY,TOTAL_AREA_COIN_PEAK
      REAL ::  CONTROL_AREA_ENRG,CONTROL_AREA_CAPACITY
      real :: forecast_coin_peak_after_dsm(max_load_classes)
      REAL ::  RESOURCE_ENRG_IN_AREA_1,

     +     RESOURCE_CAP_IN_AREA_1,
     +     FORECAST_ENRG_IN_AREA_1,
     +     FORECAST_CO_PEAK_IN_AREA_1,
     +     FORECAST_NONCO_PEAK_IN_AREA_1

      REAL ::  AREA_FORECAST_ENRG(MAX_LOAD_CLASSES),

     +     AREA_FORECAST_CAP(MAX_LOAD_CLASSES),PEAK
      REAL ::  RESERVE_MARGIN_CAPACITY,AREA_CAPACITY_NEED,
     +     RESERVE_MARGIN_CAPACITY_BAL

      REAL ::  DSM_PEAK_RESERVE_ALLOCATION(3,MAX_LOAD_CLASSES)
!
! ADDED 8/29/92 BY MSG TO HANDLE SEASONAL PRICING ON AREA CONTRACTS
!
      INTEGER (kind=2) ::  I

      REAL ::  ENRG_LAST_RESOURCE_BAL(MAX_LOAD_CLASSES),

     +     CAPACITY_LAST_RESOURCE_BAL(MAX_LOAD_CLASSES)
      REAL ::  EMERGENCY_ENERGY_BAL,MAINTENANCE_ENERGY_BAL,
     +     TOTAL_SURPLUS_ENERGY_BAL,CONTROL_AREA_ENRG_FORECAST
      CHARACTER (len=1) ::  RESOURCE_TYPE(NUMBER_OF_CONTRACTS)
      REAL ::  MAXIMUM_CAPACITY(NUMBER_OF_CONTRACTS),
     +     MAXIMUM_ENERGY(NUMBER_OF_CONTRACTS),
!    +     MAX_RATCHET_PATTERN(NUMBER_OF_CONTRACTS),
     +     MINIMUM_CAPACITY(NUMBER_OF_CONTRACTS)
! END 8/29/92
      REAL ::  CONTRACT_CAPACITY(NUMBER_OF_CONTRACTS),
     +     CONTRACT_ENERGY(NUMBER_OF_CONTRACTS)
      LOGICAL (kind=1) ::  REPORT
      REAL (kind=8) ::  DEMAND
      REAL ::  RESERVE_MARGIN_NEEDED,
     +     NEEDED_RESERVE_MARGIN_CAPACITY(MAX_LOAD_CLASSES),
     +     ALL_CLASSES_RESERVE_MARGIN
      REAL ::  TOTAL_AREA_DSM_ENERGY,FORECAST_DSM_ENRG_IN_AREA_1,
     +     FORECAST_DSM_ENERGY(MAX_LOAD_CLASSES),
     +     TOTAL_DSM_TO_RESERVE_MARGIN,DSM_TO_RESERVE_MARGIN_AREA1,
     +     DSM_TO_RESERVE_MARGIN_BY(MAX_LOAD_CLASSES),
     +     DSM_TO_LOAD_REDUCTION_BY(MAX_LOAD_CLASSES),
     +     TOTAL_AREA_PEAK_DSM,FORECAST_PEAK_DSM_IN_AREA_1,
     +     TOTAL_RESERVE_MARGIN_PURCHASES,
     +     RESERVE_MARGIN_PURCHASES_AREA1,
     +     AREA_RESERVE_MARGIN_PURCHASES(MAX_LOAD_CLASSES),
     +     TOTAL_AREA_1_COIN_PEAK,
     +     AREA_COINCIDENT_PEAK(MAX_LOAD_CLASSES),
     +     ALL_CLASSES_SURPLUS_CAPACITY,
     +     AREA1_SURPLUS_CAPACITY,
     +     TOTAL_RESOURCE_CAP_IN_AREA_1,
     +     SYSTEM_NON_COIN_PEAK,
     +     SYSTEM_NON_COIN_PEAK_LESS_DSM,
     +     SYSTEM_CAPACITY,
     +     LOCAL_CLASS_RESERVE_MARGIN,GET_VAR
      INTEGER (kind=2) ::  LAST_AREA
!
!     DETAILED REPORT DECLARATIONS
!
      LOGICAL (kind=1) ::  WV_AREA_REPORT_NOT_OPEN/.TRUE./
      INTEGER (kind=2) ::  WV_AREA_NO,WV_AREA_REPORT_HEADER,LAST_SEASON,
     +      WABASH_VALLEY_POWER_DSM_HEADER,WABASH_VALLEY_POWER_DSM_UNIT,
     +      PRODUCTION_PERIODS,CLASS
      INTEGER ::  WV_AREA_REC,WABASH_VALLEY_POWER_DSM_REC
      SAVE WV_AREA_REC,WABASH_VALLEY_POWER_DSM_REC
      CHARACTER (len=9) ::  CL_MONTH_NAME(13)
      CHARACTER (len=15) ::  AREA_NAME(MAX_LOAD_CLASSES)
      CHARACTER (len=15) ::  CONTROL_AREA_NAME
      CHARACTER (len=20) ::  MONTH_NAME
      REAL ::  N_A/-999999./
      SAVE  WV_AREA_NO,CL_MONTH_NAME,AREA_NAME,
     +      WABASH_VALLEY_POWER_DSM_UNIT,LAST_SEASON
!
!      REAL  SUM_ANNUAL_REPORT_ITEMS(13,7)
      REAL ::   SUM_ANNUAL_REPORT_ITEMS(:,:)
      ALLOCATABLE :: SUM_ANNUAL_REPORT_ITEMS
      SAVE  SUM_ANNUAL_REPORT_ITEMS
      INTEGER (kind=2) ::  SUMMARY_POS
      SAVE SUMMARY_POS

      LOGICAL ::  wabash_hoosier =.FALSE.
      LOGICAL (kind=1) ::  FOUND_CLASS_FORECAST
      
      REAL (kind=4) ::   CLASS_PEAK_AFTER_LOSSES(2,MAX_LOAD_CLASSES),
     +        CLASS_ENERGY_AFTER_LOSSES(2,MAX_LOAD_CLASSES)

! END OF VARIABLE DECLARATIONS
!
      IF(ISEAS == 1) THEN
         IF(ALLOCATED(SUM_ANNUAL_REPORT_ITEMS))
     +                               DEALLOCATE(SUM_ANNUAL_REPORT_ITEMS)
         ALLOCATE(SUM_ANNUAL_REPORT_ITEMS(13,NUMBER_OF_AREAS+2))
         SUM_ANNUAL_REPORT_ITEMS = 0.
         SUMMARY_POS = NUMBER_OF_AREAS+2
      ENDIF
!
      TOTAL_LAST_ENERGY = 0.
      TOTAL_SURPLUS_ENERGY = 0.
      TOTAL_AREA_ENERGY = 0.
      TOTAL_LAST_CAPACITY = 0.
      TOTAL_AREA_COIN_PEAK = 0.
      RESERVE_MARGIN_CAPACITY = 0.
      CALL RETURN_AREA_1_CAPACITY(TOTAL_RESOURCE_CAP_IN_AREA_1)
      CALL GET_CLASSES_PEAK_AFTER_DSM(ISEAS,
     +                                     Forecast_coin_peak_after_dsm)

      FOUND_CLASS_FORECAST = GET_PEAK_ENERGY_AFTER_LOSSES(ISEAS,
     +                                          CLASS_PEAK_AFTER_LOSSES,
     +                                        CLASS_ENERGY_AFTER_LOSSES)
!
      IF(REPORT .AND. WV_AREA_REPORT_NOT_OPEN .AND.
     +                                    .NOT. TESTING_PLAN) THEN
         WV_AREA_REPORT_NOT_OPEN = .FALSE.
         WV_AREA_NO = WV_AREA_REPORT_HEADER(WV_AREA_REC)
         WABASH_VALLEY_POWER_DSM_UNIT = WABASH_VALLEY_POWER_DSM_HEADER(
     +                                      WABASH_VALLEY_POWER_DSM_REC)
         LAST_SEASON = get_PRODUCTION_PERIODS_in()
         DO I = 1, LAST_SEASON
            CL_MONTH_NAME(I) = MONTH_NAME(I)
         ENDDO
         CL_MONTH_NAME(LAST_SEASON+1) = 'Annual'

         CALL GET_CLASS_NAME(AREA_NAME, class)

!         AREA_NAME(1) = 'I&M w/o GM'
!         AREA_NAME(2) = 'NIPSCo    '
!         AREA_NAME(3) = 'PSI       '
!         AREA_NAME(4) = 'GM        '
!         AREA_NAME(5) = 'IP&L      '
!         AREA_NAME(6) = 'Hoosier   '
!         AREA_NAME(7) = 'System    '

      ENDIF
      DO AREA = 1, NUMBER_OF_AREAS
         AREA_FORECAST = CLASS_ENERGY_AFTER_LOSSES(1,AREA) +
     +                   CLASS_ENERGY_AFTER_LOSSES(2,AREA)
         TOTAL_AREA_ENERGY = TOTAL_AREA_ENERGY + AREA_FORECAST
         ANNUAL_AREA_ENERGY(AREA) = ANNUAL_AREA_ENERGY(AREA) +
     +                              AREA_FORECAST
         IF(AREA_FORECAST > 0.) THEN
            ENRG_LAST_RESOURCE(AREA,ISEAS) = MAX(AREA_FORECAST -
     +                           SEAS_HOURS*AREA_ENERGY(AREA,ISEAS), 0.)
            AREA_SURPLUS(AREA,ISEAS) =
     +          MAX(SEAS_HOURS*AREA_ENERGY(AREA,ISEAS)-AREA_FORECAST,0.)
         ELSE
            ENRG_LAST_RESOURCE(AREA,ISEAS) = 0.
            AREA_SURPLUS(AREA,ISEAS) = 0.
         ENDIF
         ANNUAL_AREA_SURPLUS_ENERGY(AREA) = AREA_SURPLUS(AREA,ISEAS) +
     +                                  ANNUAL_AREA_SURPLUS_ENERGY(AREA)
         ENRG_LAST_RESOURCE_BAL(AREA) = ENRG_LAST_RESOURCE(AREA,ISEAS)/
     +                                       SEAS_HOURS
         TOTAL_LAST_ENERGY = TOTAL_LAST_ENERGY +
     +                                    ENRG_LAST_RESOURCE(AREA,ISEAS)
         TOTAL_SURPLUS_ENERGY = TOTAL_SURPLUS_ENERGY +
     +                                          AREA_SURPLUS(AREA,ISEAS)
         AREA_FORECAST_ENRG(AREA) = AREA_FORECAST
! FINISHED ENERGY

! 7/31/98. GAT. PER STEININGER.
!
         IF(CLASS_RESERVE_MARGIN(AREA) >= 0.) THEN
            LOCAL_CLASS_RESERVE_MARGIN = CLASS_RESERVE_MARGIN(AREA)/100.
         ELSE
            LOCAL_CLASS_RESERVE_MARGIN =
     +         GET_VAR(CLASS_RESERVE_MARGIN(AREA),
     +                                     ISEAS,MONTH_NAME(ISEAS))/100.
         ENDIF
!
         IF(WABASH_VALLEY) THEN
            AREA_FORECAST = Forecast_coin_peak_after_dsm(AREA) *
     +                                           CLASS_COIN_FACTOR(AREA)
            AREA_FORECAST_CAP(AREA) = AREA_FORECAST

            NEEDED_RESERVE_MARGIN_CAPACITY(AREA) =
     +              LOCAL_CLASS_RESERVE_MARGIN *
     +                 CLASS_COIN_FACTOR(AREA) *
     +                     MAX(CLASS_PEAK_AFTER_LOSSES(1,AREA),
     +                           CLASS_PEAK_AFTER_LOSSES(2,AREA))
            ANNUAL_COINCIDENT_PEAK(AREA)=
     +                   MAX(ANNUAL_COINCIDENT_PEAK(AREA),AREA_FORECAST)
            ANNUAL_NONCOINCIDENT_PEAK(AREA) =
     +                       MAX(ANNUAL_NONCOINCIDENT_PEAK(AREA),
     +                           Forecast_coin_peak_after_dsm(AREA))
         ELSE
!
! 3/6/94 MODIFIED TO USE PEAK AFTER DSM
!
            AREA_FORECAST = MAX(CLASS_PEAK_AFTER_LOSSES(1,AREA),
     +                      CLASS_PEAK_AFTER_LOSSES(2,AREA)) *
     +                      CLASS_COIN_FACTOR(AREA)
            AREA_FORECAST_CAP(AREA) = AREA_FORECAST
            NEEDED_RESERVE_MARGIN_CAPACITY(AREA) =
     +                        AREA_FORECAST * LOCAL_CLASS_RESERVE_MARGIN
            ANNUAL_COINCIDENT_PEAK(AREA)=
     +                   MAX(ANNUAL_COINCIDENT_PEAK(AREA),AREA_FORECAST)
            ANNUAL_NONCOINCIDENT_PEAK(AREA) =
     +                       MAX(ANNUAL_NONCOINCIDENT_PEAK(AREA),
     +                           CLASS_PEAK_AFTER_LOSSES(1,AREA),
     +                           CLASS_PEAK_AFTER_LOSSES(2,AREA))
         ENDIF
         TOTAL_AREA_COIN_PEAK = TOTAL_AREA_COIN_PEAK + AREA_FORECAST
         IF(AREA_FORECAST > 0.) THEN
            CAPACITY_LAST_RESOURCE(AREA,ISEAS) = MAX(AREA_FORECAST -
     +                                    AREA_CAPACITY(AREA,ISEAS), 0.)
            AREA_SURPLUS_CAPACITY(AREA,ISEAS) =
     +                MAX(AREA_CAPACITY(AREA,ISEAS) - AREA_FORECAST, 0.)
         ELSE
            CAPACITY_LAST_RESOURCE(AREA,ISEAS) = 0.
            AREA_SURPLUS_CAPACITY(AREA,ISEAS) = 0.
         ENDIF
         CAPACITY_LAST_RESOURCE_BAL(AREA) =
     +                                CAPACITY_LAST_RESOURCE(AREA,ISEAS)
         TOTAL_LAST_CAPACITY = TOTAL_LAST_CAPACITY +
     +                                CAPACITY_LAST_RESOURCE(AREA,ISEAS)
      ENDDO ! AREA
!
! ASSIGN CAPACITY AND ENERGY NEEDS TO AREA RESOURCE OF LAST RESORT
!
      IF(WABASH_VALLEY) THEN
         CONTROL_AREA_ENRG = AREA_ENERGY(1,ISEAS)
         CONTROL_AREA_CAPACITY = AREA_CAPACITY(1,ISEAS)
         CONTROL_AREA_ENRG_FORECAST = AREA_FORECAST_ENRG(1)
         PEAK = AREA_FORECAST_CAP(1)
         AREA_CAPACITY_NEED = AREA_FORECAST_CAP(1)
         RESERVE_MARGIN_NEEDED = NEEDED_RESERVE_MARGIN_CAPACITY(1)
         DO CLASS = 2, MAX_LOAD_CLASSES
            PEAK = PEAK + AREA_FORECAST_CAP(CLASS)
            IF(CLASS_CONTROL_AREA(CLASS) /= 1) THEN
               DEMAND = DEMAND + AREA_FORECAST_ENRG(CLASS)
            ELSE
               CONTROL_AREA_ENRG = CONTROL_AREA_ENRG +
     +                                          AREA_ENERGY(CLASS,ISEAS)
               CONTROL_AREA_CAPACITY = CONTROL_AREA_CAPACITY +
     +                                        AREA_CAPACITY(CLASS,ISEAS)
               CONTROL_AREA_ENRG_FORECAST = CONTROL_AREA_ENRG_FORECAST +
     +                                         AREA_FORECAST_ENRG(CLASS)
               ENRG_LAST_RESOURCE(CLASS,ISEAS) = 0.
               CAPACITY_LAST_RESOURCE(CLASS,ISEAS) = 0.
               CAPACITY_LAST_RESOURCE_BAL(CLASS) = 0.
               AREA_CAPACITY_NEED = AREA_CAPACITY_NEED +
     +                                          AREA_FORECAST_CAP(CLASS)
               RESERVE_MARGIN_NEEDED = RESERVE_MARGIN_NEEDED +
     +                             NEEDED_RESERVE_MARGIN_CAPACITY(CLASS)
            ENDIF
         ENDDO
         CONTROL_AREA_ENRG = SEAS_HOURS * CONTROL_AREA_ENRG
         ENRG_LAST_RESOURCE(1,ISEAS) = MAX(0.,
     +                     CONTROL_AREA_ENRG_FORECAST-CONTROL_AREA_ENRG)
         ENRG_LAST_RESOURCE_BAL(1) = ENRG_LAST_RESOURCE(1,ISEAS)/
     +                                                        SEAS_HOURS
         RESERVE_MARGIN_CAPACITY = MAX(0.,RESERVE_MARGIN_NEEDED -
     +                                    MAX(0.,CONTROL_AREA_CAPACITY -
     +                                              AREA_CAPACITY_NEED))
         CAPACITY_LAST_RESOURCE(1,ISEAS) = MAX(0.,AREA_CAPACITY_NEED-
     +                                            CONTROL_AREA_CAPACITY)
         CAPACITY_LAST_RESOURCE_BAL(1) = CAPACITY_LAST_RESOURCE(1,ISEAS)

      IF(wabash_hoosier) THEN
         IF(WABASH_IM_NIPSCO_PSI) THEN
            DEMAND = DEMAND + AREA_FORECAST_ENRG(4) +
     +                        AREA_FORECAST_ENRG(5) +
     +                        AREA_FORECAST_ENRG(6)
            CONTROL_AREA_ENRG = SEAS_HOURS*(AREA_ENERGY(1,ISEAS) +
     +                                      AREA_ENERGY(2,ISEAS) +
     +                                      AREA_ENERGY(3,ISEAS))
            CONTROL_AREA_CAPACITY = AREA_CAPACITY(1,ISEAS) +
     +                              AREA_CAPACITY(2,ISEAS) +
     +                              AREA_CAPACITY(3,ISEAS)
            ENRG_LAST_RESOURCE(2,ISEAS) = 0.
            CAPACITY_LAST_RESOURCE(2,ISEAS) = 0.
            ENRG_LAST_RESOURCE(3,ISEAS) = 0.
            CAPACITY_LAST_RESOURCE(3,ISEAS) = 0.
            ENRG_LAST_RESOURCE(1,ISEAS) = MAX(0.,AREA_FORECAST_ENRG(1) +
     +                                           AREA_FORECAST_ENRG(2) +
     +                                           AREA_FORECAST_ENRG(3) -
     +                                           CONTROL_AREA_ENRG)
            ENRG_LAST_RESOURCE_BAL(1) = ENRG_LAST_RESOURCE(1,ISEAS)/
     +                                            SEAS_HOURS
            AREA_CAPACITY_NEED = AREA_FORECAST_CAP(1) +
     +                           AREA_FORECAST_CAP(2) +
     +                           AREA_FORECAST_CAP(3)

            PEAK = AREA_FORECAST_CAP(1) +
     +             AREA_FORECAST_CAP(2) +
     +             AREA_FORECAST_CAP(3) +
     +             AREA_FORECAST_CAP(4) +
     +             AREA_FORECAST_CAP(5) +
     +             AREA_FORECAST_CAP(6)
            RESERVE_MARGIN_NEEDED = NEEDED_RESERVE_MARGIN_CAPACITY(1) +
     +                              NEEDED_RESERVE_MARGIN_CAPACITY(2) +
     +                              NEEDED_RESERVE_MARGIN_CAPACITY(3)
            RESERVE_MARGIN_CAPACITY = MAX(0.,RESERVE_MARGIN_NEEDED -
     +                                    MAX(0.,CONTROL_AREA_CAPACITY -
     +                                              AREA_CAPACITY_NEED))
            CAPACITY_LAST_RESOURCE(1,ISEAS) = MAX(0.,AREA_CAPACITY_NEED-
     +                                            CONTROL_AREA_CAPACITY)
            CAPACITY_LAST_RESOURCE_BAL(2) = 0.
            CAPACITY_LAST_RESOURCE_BAL(3) = 0.
            CAPACITY_LAST_RESOURCE_BAL(1) =
     +                                   CAPACITY_LAST_RESOURCE(1,ISEAS)
         ELSE
            DEMAND = DEMAND + AREA_FORECAST_ENRG(3) +
     +                        AREA_FORECAST_ENRG(4) +
     +                        AREA_FORECAST_ENRG(5) +
     +                        AREA_FORECAST_ENRG(6)
            CONTROL_AREA_ENRG = SEAS_HOURS*(AREA_ENERGY(1,ISEAS) +
     +                                      AREA_ENERGY(2,ISEAS))
            CONTROL_AREA_CAPACITY = AREA_CAPACITY(1,ISEAS) +
     +                              AREA_CAPACITY(2,ISEAS)
            ENRG_LAST_RESOURCE(2,ISEAS) = 0.
            CAPACITY_LAST_RESOURCE(2,ISEAS) = 0.
            ENRG_LAST_RESOURCE(1,ISEAS) = MAX(0.,AREA_FORECAST_ENRG(1) +
     +                                           AREA_FORECAST_ENRG(2) -
     +                                           CONTROL_AREA_ENRG)
            ENRG_LAST_RESOURCE_BAL(1) = ENRG_LAST_RESOURCE(1,ISEAS)/
     +                                            SEAS_HOURS
            AREA_CAPACITY_NEED = AREA_FORECAST_CAP(1) +
     +                           AREA_FORECAST_CAP(2)
            PEAK = PEAK + AREA_FORECAST_CAP(3) +
     +                    AREA_FORECAST_CAP(4) +
     +                    AREA_FORECAST_CAP(5) +
     +                    AREA_FORECAST_CAP(6)
            RESERVE_MARGIN_NEEDED = NEEDED_RESERVE_MARGIN_CAPACITY(1) +
     +                              NEEDED_RESERVE_MARGIN_CAPACITY(2)
            RESERVE_MARGIN_CAPACITY = MAX(0.,RESERVE_MARGIN_NEEDED -
     +                                    MAX(0.,CONTROL_AREA_CAPACITY -
     +                                              AREA_CAPACITY_NEED))
            CAPACITY_LAST_RESOURCE(1,ISEAS) = MAX(0.,AREA_CAPACITY_NEED-
     +                                            CONTROL_AREA_CAPACITY)
            CAPACITY_LAST_RESOURCE_BAL(2) = 0.
            CAPACITY_LAST_RESOURCE_BAL(1) =
     +                                   CAPACITY_LAST_RESOURCE(1,ISEAS)
         ENDIF
         ENDIF ! $endif
         FORECAST_DSM_ENERGY = 0.
         CALL GET_CLASS_DSM_ENERGY_ALLOC(ISEAS,FORECAST_DSM_ENERGY)
         CALL GET_CLASS_MW_RESERVE_ALLOC(ISEAS,
     +                                  DSM_PEAK_RESERVE_ALLOCATION)
         ALL_CLASSES_RESERVE_MARGIN = 0.
         TOTAL_DSM_TO_RESERVE_MARGIN = 0.
         DSM_TO_RESERVE_MARGIN_AREA1 = 0.
         FORECAST_NONCO_PEAK_IN_AREA_1 = 0.
         FORECAST_PEAK_DSM_IN_AREA_1 = 0.
         TOTAL_DSM_TO_RESERVE_MARGIN = 0.
         DSM_TO_RESERVE_MARGIN_AREA1 = 0.
         TOTAL_RESERVE_MARGIN_PURCHASES = 0.
         RESERVE_MARGIN_PURCHASES_AREA1 = 0.
         TOTAL_AREA_PEAK_DSM = 0.
         TOTAL_AREA_COIN_PEAK = 0.
         TOTAL_AREA_1_COIN_PEAK = 0.
         ALL_CLASSES_SURPLUS_CAPACITY = 0.
         AREA1_SURPLUS_CAPACITY = 0.
         TOTAL_AREA_DSM_ENERGY = 0.
         FORECAST_DSM_ENRG_IN_AREA_1 = 0.

         DO AREA = 1, NUMBER_OF_AREAS
            TOTAL_AREA_DSM_ENERGY = TOTAL_AREA_DSM_ENERGY +
     +                                         FORECAST_DSM_ENERGY(AREA)
            AREA_COINCIDENT_PEAK(AREA) = CLASS_COIN_FACTOR(AREA) *
     +                   (MAX(CLASS_PEAK_AFTER_LOSSES(1,AREA),
     +                       CLASS_PEAK_AFTER_LOSSES(2,AREA)) +
     +                             DSM_PEAK_RESERVE_ALLOCATION(1,AREA))
            TOTAL_AREA_COIN_PEAK = TOTAL_AREA_COIN_PEAK +
     +                                        AREA_COINCIDENT_PEAK(AREA)
            ALL_CLASSES_RESERVE_MARGIN = ALL_CLASSES_RESERVE_MARGIN +
     +                              NEEDED_RESERVE_MARGIN_CAPACITY(AREA)
            IF(NEEDED_RESERVE_MARGIN_CAPACITY(AREA) <
     +                 DSM_PEAK_RESERVE_ALLOCATION(2,AREA) +
     +                         DSM_PEAK_RESERVE_ALLOCATION(3,AREA)) THEN
               IF(NEEDED_RESERVE_MARGIN_CAPACITY(AREA) <
     +                         DSM_PEAK_RESERVE_ALLOCATION(3,AREA)) THEN
                  DSM_TO_RESERVE_MARGIN_BY(AREA) =
     +                               DSM_PEAK_RESERVE_ALLOCATION(3,AREA)
                  DSM_TO_LOAD_REDUCTION_BY(AREA) =
     +                             DSM_PEAK_RESERVE_ALLOCATION(1,AREA) +
     +                             DSM_PEAK_RESERVE_ALLOCATION(2,AREA)
               ELSE
                  DSM_TO_RESERVE_MARGIN_BY(AREA) =
     +                              NEEDED_RESERVE_MARGIN_CAPACITY(AREA)
                  DSM_TO_LOAD_REDUCTION_BY(AREA) =
     +                    DSM_PEAK_RESERVE_ALLOCATION(1,AREA) +
     +                       DSM_PEAK_RESERVE_ALLOCATION(2,AREA) +
     +                          DSM_PEAK_RESERVE_ALLOCATION(3,AREA) -
     +                              NEEDED_RESERVE_MARGIN_CAPACITY(AREA)
               ENDIF
            ELSE
               DSM_TO_LOAD_REDUCTION_BY(AREA) =
     +                               DSM_PEAK_RESERVE_ALLOCATION(1,AREA)
               DSM_TO_RESERVE_MARGIN_BY(AREA) =
     +                     DSM_PEAK_RESERVE_ALLOCATION(2,AREA) +
     +                               DSM_PEAK_RESERVE_ALLOCATION(3,AREA)
            ENDIF

            TOTAL_DSM_TO_RESERVE_MARGIN =
     +                                    TOTAL_DSM_TO_RESERVE_MARGIN +
     +                                    DSM_TO_RESERVE_MARGIN_BY(AREA)
            TOTAL_AREA_PEAK_DSM = TOTAL_AREA_PEAK_DSM +
     +                                    DSM_TO_LOAD_REDUCTION_BY(AREA)

            ALL_CLASSES_SURPLUS_CAPACITY =
     +                                 ALL_CLASSES_SURPLUS_CAPACITY+
     +                                 AREA_SURPLUS_CAPACITY(AREA,ISEAS)
            AREA_RESERVE_MARGIN_PURCHASES(AREA) = MAX(0.,
     +                            NEEDED_RESERVE_MARGIN_CAPACITY(AREA) +
     +                                   DSM_TO_RESERVE_MARGIN_BY(AREA))
            TOTAL_RESERVE_MARGIN_PURCHASES =
     +                               TOTAL_RESERVE_MARGIN_PURCHASES  +
     +                               AREA_RESERVE_MARGIN_PURCHASES(AREA)

            IF(CLASS_CONTROL_AREA(AREA) == 1) THEN
               FORECAST_DSM_ENRG_IN_AREA_1 =
     +                                 FORECAST_DSM_ENRG_IN_AREA_1 +
     +                                         FORECAST_DSM_ENERGY(AREA)
               DSM_TO_RESERVE_MARGIN_AREA1 =
     +                                    DSM_TO_RESERVE_MARGIN_AREA1 +
     +                                    DSM_TO_RESERVE_MARGIN_BY(AREA)
               FORECAST_NONCO_PEAK_IN_AREA_1 =
     +                                FORECAST_NONCO_PEAK_IN_AREA_1 +
     +                 MAX(CLASS_PEAK_AFTER_LOSSES(1,AREA),
     +                           CLASS_PEAK_AFTER_LOSSES(2,AREA))
               FORECAST_PEAK_DSM_IN_AREA_1 =
     +                           FORECAST_PEAK_DSM_IN_AREA_1 +
     +                               DSM_PEAK_RESERVE_ALLOCATION(1,AREA)
               RESERVE_MARGIN_PURCHASES_AREA1 =
     +                            RESERVE_MARGIN_PURCHASES_AREA1 +
     +                               AREA_RESERVE_MARGIN_PURCHASES(AREA)
               TOTAL_AREA_1_COIN_PEAK = TOTAL_AREA_1_COIN_PEAK +
     +                                        AREA_COINCIDENT_PEAK(AREA)
               AREA1_SURPLUS_CAPACITY = AREA1_SURPLUS_CAPACITY +
     +                                 AREA_SURPLUS_CAPACITY(AREA,ISEAS)
            ENDIF
         ENDDO
         RESERVE_MARGIN_CAPACITY = MAX(0.,
     +                          TOTAL_RESERVE_MARGIN_PURCHASES -
     +                          MAX(0.,TOTAL_RESOURCE_CAP_IN_AREA_1 -
     +                                          TOTAL_AREA_1_COIN_PEAK))
         TOTAL_LAST_ENERGY = 0.
         TOTAL_LAST_CAPACITY = 0.
         DO AREA = 1, NUMBER_OF_AREAS
            TOTAL_LAST_ENERGY = TOTAL_LAST_ENERGY +
     +                                    ENRG_LAST_RESOURCE(AREA,ISEAS)
            TOTAL_LAST_CAPACITY = TOTAL_LAST_CAPACITY +
     +                                CAPACITY_LAST_RESOURCE(AREA,ISEAS)
         ENDDO
!
         RESOURCE_ENRG_IN_AREA_1= 0.
         RESOURCE_CAP_IN_AREA_1 = 0.
         FORECAST_ENRG_IN_AREA_1 = 0.
         FORECAST_CO_PEAK_IN_AREA_1 = 0.
         FORECAST_NONCO_PEAK_IN_AREA_1 = 0.
         CLASSES_IN_AREA_1 = 2
         IF(WABASH_IM_NIPSCO_PSI) CLASSES_IN_AREA_1 = 3
         DO AREA = 1, NUMBER_OF_AREAS
            IF(CLASS_CONTROL_AREA(AREA) /= 1) CYCLE
            RESOURCE_ENRG_IN_AREA_1 = RESOURCE_ENRG_IN_AREA_1 +
     +                                    ENRG_LAST_RESOURCE(AREA,ISEAS)
            RESOURCE_CAP_IN_AREA_1 = RESOURCE_CAP_IN_AREA_1 +
     +                                CAPACITY_LAST_RESOURCE(AREA,ISEAS)
            FORECAST_ENRG_IN_AREA_1 =FORECAST_ENRG_IN_AREA_1 +
     +                               CLASS_ENERGY_AFTER_LOSSES(1,AREA) +
     +                               CLASS_ENERGY_AFTER_LOSSES(2,AREA)
            FORECAST_CO_PEAK_IN_AREA_1 = FORECAST_CO_PEAK_IN_AREA_1 +
     +                 CLASS_COIN_FACTOR(AREA) *
     +                       MAX(CLASS_PEAK_AFTER_LOSSES(1,AREA),
     +                           CLASS_PEAK_AFTER_LOSSES(2,AREA))
            FORECAST_NONCO_PEAK_IN_AREA_1=FORECAST_NONCO_PEAK_IN_AREA_1+
     +                       MAX(CLASS_PEAK_AFTER_LOSSES(1,AREA),
     +                           CLASS_PEAK_AFTER_LOSSES(2,AREA))
         ENDDO
         TOTAL_SURPLUS_ENERGY =
     +           MAX(0.,RESOURCE_ENRG_IN_AREA_1-FORECAST_ENRG_IN_AREA_1)
         DO AREA = 2, NUMBER_OF_AREAS
            IF(CLASS_CONTROL_AREA(AREA) == 1) CYCLE
            TOTAL_SURPLUS_ENERGY = TOTAL_SURPLUS_ENERGY +
     +                                          AREA_SURPLUS(AREA,ISEAS)
         ENDDO
      ENDIF
      EMERGENCY_ENERGY_BAL = EMERGENCY_ENERGY/SEAS_HOURS
      MAINTENANCE_ENERGY_BAL = MAINTENANCE_ENERGY/SEAS_HOURS
      TOTAL_SURPLUS_ENERGY_BAL = TOTAL_SURPLUS_ENERGY/SEAS_HOURS
      RESERVE_MARGIN_CAPACITY_BAL = RESERVE_MARGIN_CAPACITY
      DO I = 1, NUMBER_OF_CONTRACTS
         IF(CNTR_ON_LI(I) > DATE2 .OR. CNTR_OFF_LI(I) < DATE1) CYCLE
         AREA = INDEX('123456',RESOURCE_TYPE(I))
         IF(AREA > 0) THEN
            CONTRACT_ENERGY(I) = MIN(MAXIMUM_ENERGY(I)/SEAS_HOURS,
     +                               ENRG_LAST_RESOURCE_BAL(AREA))
            ENRG_LAST_RESOURCE_BAL(AREA) = MAX(0.,
     +                                ENRG_LAST_RESOURCE_BAL(AREA) -
     +                                CONTRACT_ENERGY(I))

            IF(MAXIMUM_ENERGY(I) /= 0. .AND.
     +                      CAPACITY_LAST_RESOURCE_BAL(AREA) /= 0.) THEN
               IF(MAX_RATCHET_PATTERN(I) /= 0) THEN
                  CONTRACT_CAPACITY(I)=CAPACITY_LAST_RESOURCE_BAL(AREA)
                  MAXIMUM_CAPACITY(I) = MAX(MAXIMUM_CAPACITY(I),
     +                                      CONTRACT_CAPACITY(I),
     +                                      MINIMUM_CAPACITY(I))
               ELSE IF(CNTR_CAPACITY_SWITCH(I) == 'V') THEN
                  CONTRACT_CAPACITY(I)=CAPACITY_LAST_RESOURCE_BAL(AREA)
                  MAXIMUM_CAPACITY(I) = CONTRACT_CAPACITY(I)
               ELSE
                  CONTRACT_CAPACITY(I) = MIN(MAXIMUM_CAPACITY(I),
     +                                 CAPACITY_LAST_RESOURCE_BAL(AREA))
               ENDIF
               CAPACITY_LAST_RESOURCE_BAL(AREA) = MAX(0.,
     +                                CAPACITY_LAST_RESOURCE_BAL(AREA) -
     +                                CONTRACT_CAPACITY(I))
            ENDIF
         ELSEIF(RESOURCE_TYPE(I) == 'E') THEN
            CONTRACT_CAPACITY(I) = 0.
            CONTRACT_ENERGY(I) = MIN(MAXIMUM_ENERGY(I)/SEAS_HOURS,
     +                               EMERGENCY_ENERGY_BAL)
            EMERGENCY_ENERGY_BAL = MAX(0.,EMERGENCY_ENERGY_BAL -
     +                                               CONTRACT_ENERGY(I))
         ELSEIF(RESOURCE_TYPE(I) == 'M') THEN
            CONTRACT_CAPACITY(I) = 0.
            CONTRACT_ENERGY(I) = MIN(MAXIMUM_ENERGY(I)/SEAS_HOURS,
     +                               MAINTENANCE_ENERGY_BAL)
            MAINTENANCE_ENERGY_BAL = MAX(0.,MAINTENANCE_ENERGY_BAL -
     +                                               CONTRACT_ENERGY(I))
         ELSEIF(RESOURCE_TYPE(I) == 'S') THEN
            CONTRACT_CAPACITY(I) = 0.
            CONTRACT_ENERGY(I) = MIN(MAXIMUM_ENERGY(I)/SEAS_HOURS,
     +                               TOTAL_SURPLUS_ENERGY_BAL)
            TOTAL_SURPLUS_ENERGY_BAL = MAX(0.,TOTAL_SURPLUS_ENERGY_BAL -
     +                                               CONTRACT_ENERGY(I))
         ELSEIF(RESOURCE_TYPE(I) == 'R') THEN
            CONTRACT_ENERGY(I) = 0.
            CONTRACT_CAPACITY(I) = MIN(RESERVE_MARGIN_CAPACITY_BAL,
     +                                              MAXIMUM_CAPACITY(I))
            RESERVE_MARGIN_CAPACITY_BAL =
     +        MAX(0.,RESERVE_MARGIN_CAPACITY_BAL - CONTRACT_CAPACITY(I))
         ENDIF
      ENDDO

! CHECK FOR UNSERVED AREA ENERGY

      IF(.NOT. WABASH_VALLEY) THEN
         DO AREA = 1, NUMBER_OF_AREAS
            IF(ENRG_LAST_RESOURCE_BAL(AREA) /= 0.) THEN
               WRITE(4,*) 'Area',AREA,' in',ISEAS,
     +                 ' has unserved energy',
     +                   ENRG_LAST_RESOURCE_BAL(AREA)
            ENDIF
            IF(CAPACITY_LAST_RESOURCE_BAL(AREA) > 0.) THEN
               WRITE(4,*) 'Area',AREA,' in',ISEAS,
     +                 ' has unserved capacity needs',
     +                 CAPACITY_LAST_RESOURCE_BAL(AREA)
            ENDIF
         ENDDO
      ENDIF

!
! EMERGENCY AND MAINTENANCE ENERGY BALANCE ASSIGNED TO LAST
! EMERGENCY AND MAINTENANCE CONTRACT FOUND IN THE CONTRACTS FILE
!

      IF(EMERGENCY_ENERGY_CONTRACT > 0) THEN
         CONTRACT_CAPACITY(EMERGENCY_ENERGY_CONTRACT) = 0.
         CONTRACT_ENERGY(EMERGENCY_ENERGY_CONTRACT) =
     +                      CONTRACT_ENERGY(EMERGENCY_ENERGY_CONTRACT) +
     +                                              EMERGENCY_ENERGY_BAL
      ENDIF
      IF(MAINTENANCE_ENERGY_CONTRACT > 0) THEN
         CONTRACT_CAPACITY(MAINTENANCE_ENERGY_CONTRACT) = 0.
         CONTRACT_ENERGY(MAINTENANCE_ENERGY_CONTRACT) =
     +                    CONTRACT_ENERGY(MAINTENANCE_ENERGY_CONTRACT) +
     +                                            MAINTENANCE_ENERGY_BAL
      ENDIF

      IF(REPORT.AND. .NOT. WABASH_VALLEY) THEN
         WRITE(9,1000) 'Area Utilities (GWh)',TOTAL_LAST_ENERGY/1000.,
     +                (ENRG_LAST_RESOURCE(AREA,ISEAS)/1000.,
     +                                        AREA = 1, NUMBER_OF_AREAS)
         WRITE(9,1000) 'Capacity (MW) ',TOTAL_LAST_CAPACITY,
     +         (CAPACITY_LAST_RESOURCE(AREA,ISEAS),
     +                                        AREA = 1, NUMBER_OF_AREAS)
         WRITE(9,1000)'Surplus Energy (GWh)',TOTAL_SURPLUS_ENERGY/1000.,
     +        (AREA_SURPLUS(AREA,ISEAS)/1000.,AREA = 1, NUMBER_OF_AREAS)
         WRITE(9,1000) 'Area Forecasts (GWh)',TOTAL_AREA_ENERGY/1000.,
     +         ((FORECAST_ENERGY(1,ISEAS,AREA) +
     +                   FORECAST_ENERGY(2,ISEAS,AREA))/1000.,
     +                                        AREA = 1, NUMBER_OF_AREAS)
         WRITE(9,1010) 'non-coincident peak',
     +         (MAX(FORECAST_COINCIDENT_PEAK(1,ISEAS,AREA),
     +                       FORECAST_COINCIDENT_PEAK(2,ISEAS,AREA)),
     +                                        AREA = 1, NUMBER_OF_AREAS)
         WRITE(9,1000) 'coincident peak',TOTAL_AREA_COIN_PEAK,
     +                 (CLASS_COIN_FACTOR(AREA) *
     +                      MAX(FORECAST_COINCIDENT_PEAK(1,ISEAS,AREA),
     +                          FORECAST_COINCIDENT_PEAK(2,ISEAS,AREA)),
     +                                        AREA = 1, NUMBER_OF_AREAS)
         WRITE(9,*)
         WRITE(9,1022) 'Emergency Energy (GWh)',
     +                                     EMERGENCY_ENERGY/1000.,
     +                 'Maintenance Energy (GWh)',
     +                                   MAINTENANCE_ENERGY/1000.,
     +                 'Energy Losses (GWh)',
     +                                   AREA_ENERGY_LOSSES/1000.
      ENDIF
      IF(REPORT .AND. WABASH_VALLEY) THEN
         WRITE(9,1000) 'Surplus Energy (GWh)',
     +                  TOTAL_SURPLUS_ENERGY/1000.,
     +                  MAX(0.,RESOURCE_ENRG_IN_AREA_1 -
     +                                    FORECAST_ENRG_IN_AREA_1)/1000.
         IF(WABASH_IM_NIPSCO) THEN
            WRITE(9,1005) (AREA_SURPLUS(AREA,ISEAS)/1000.,
     +                      AREA = CLASSES_IN_AREA_1+1, NUMBER_OF_AREAS)
         ELSE
            WRITE(9,1007) (AREA_SURPLUS(AREA,ISEAS)/1000.,
     +                      AREA = CLASSES_IN_AREA_1+1, NUMBER_OF_AREAS)
         ENDIF
         WRITE(9,1000) 'Area Utilities (GWh)',TOTAL_LAST_ENERGY/1000.,
     +                  RESOURCE_ENRG_IN_AREA_1/1000.,
     +                 (ENRG_LAST_RESOURCE(AREA,ISEAS)/1000.,
     +                                        AREA = 1, NUMBER_OF_AREAS)
         WRITE(9,1000) 'Capacity (MW) ',TOTAL_LAST_CAPACITY,
     +                  RESOURCE_CAP_IN_AREA_1,
     +                 (CAPACITY_LAST_RESOURCE(AREA,ISEAS),
     +                                        AREA = 1, NUMBER_OF_AREAS)
! TODO: Extract to area_energy_report routine
! AREA ENERGY REPORt
         WRITE(9,*)
         WRITE(9,1000) 'Area Forecast (GWh)',TOTAL_AREA_ENERGY/1000.,
     +         FORECAST_ENRG_IN_AREA_1/1000.,
     +         ((FORECAST_ENERGY(1,ISEAS,AREA) +
     +                   FORECAST_ENERGY(2,ISEAS,AREA))/1000.,
     +                                        AREA = 1, NUMBER_OF_AREAS)
         WRITE(9,1000) 'Area DSM (GWh)',TOTAL_AREA_DSM_ENERGY/1000.,
     +         FORECAST_DSM_ENRG_IN_AREA_1/1000.,
     +         (FORECAST_DSM_ENERGY(AREA)/1000., AREA=1,NUMBER_OF_AREAS)
         WRITE(9,1000) 'Area Energy After DSM',
     +      (TOTAL_AREA_ENERGY-TOTAL_AREA_DSM_ENERGY)/1000.,
     +      (FORECAST_ENRG_IN_AREA_1-FORECAST_DSM_ENRG_IN_AREA_1)/1000.,
     +      ((FORECAST_ENERGY(1,ISEAS,AREA) +
     +           FORECAST_ENERGY(2,ISEAS,AREA) -
     +              FORECAST_DSM_ENERGY(AREA))/1000.,
     +                                        AREA = 1, NUMBER_OF_AREAS)
! TODO: Extract to area_peak_report routine
! AREA PEAK REPORT
!
         WRITE(9,1010) 'Non-coincident peak',
     +                  FORECAST_NONCO_PEAK_IN_AREA_1,
     +                 (MAX(FORECAST_COINCIDENT_PEAK(1,ISEAS,AREA),
     +                       FORECAST_COINCIDENT_PEAK(2,ISEAS,AREA)),
     +                                        AREA = 1, NUMBER_OF_AREAS)
         WRITE(9,1000) 'Area DSM (MW)',
     +                  -TOTAL_AREA_PEAK_DSM,
     +                  -FORECAST_PEAK_DSM_IN_AREA_1,
     +                 (-DSM_PEAK_RESERVE_ALLOCATION(1,AREA),
     +                                        AREA = 1, NUMBER_OF_AREAS)
         WRITE(9,1010) 'Area Peak after DSM',
     +                  FORECAST_NONCO_PEAK_IN_AREA_1 +
     +                                      FORECAST_PEAK_DSM_IN_AREA_1,
     +                 ((MAX(FORECAST_COINCIDENT_PEAK(1,ISEAS,AREA),
     +                       FORECAST_COINCIDENT_PEAK(2,ISEAS,AREA)) +
     +                             DSM_PEAK_RESERVE_ALLOCATION(1,AREA)),
     +                                        AREA = 1, NUMBER_OF_AREAS)
         WRITE(9,1000) 'coincident peak',TOTAL_AREA_COIN_PEAK,
     +                 TOTAL_AREA_1_COIN_PEAK,
     +                 (AREA_COINCIDENT_PEAK(AREA),
     +                                        AREA = 1, NUMBER_OF_AREAS)

         WRITE(9,1000) 'Reserve Requirement (MW)',
     +                  ALL_CLASSES_RESERVE_MARGIN,
     +                  RESERVE_MARGIN_NEEDED,
     +                 (NEEDED_RESERVE_MARGIN_CAPACITY(AREA),
     +                                        AREA = 1, NUMBER_OF_AREAS)
         WRITE(9,1000) ' DSM Portion (MW)',
     +                 -TOTAL_DSM_TO_RESERVE_MARGIN,
     +                 -DSM_TO_RESERVE_MARGIN_AREA1,
     +                 (-DSM_TO_RESERVE_MARGIN_BY(AREA),
     +                                        AREA = 1, NUMBER_OF_AREAS)
         WRITE(9,1000) ' Reserves after DSM (MW)',
     +                  TOTAL_RESERVE_MARGIN_PURCHASES,
     +                  RESERVE_MARGIN_PURCHASES_AREA1,
     +                  (AREA_RESERVE_MARGIN_PURCHASES(AREA),
     +                                        AREA = 1, NUMBER_OF_AREAS)
         WRITE(9,1000) 'Area Surplus Capacity   ',
     +                  MAX(0.,TOTAL_RESOURCE_CAP_IN_AREA_1 -
     +                                           TOTAL_AREA_1_COIN_PEAK)
         WRITE(9,1000) 'Purchases Reserves (MW) ',
     +               MAX(0.,TOTAL_RESERVE_MARGIN_PURCHASES-
     +                  MAX(0.,TOTAL_RESOURCE_CAP_IN_AREA_1 -
     +                                          TOTAL_AREA_1_COIN_PEAK))
         WRITE(9,*)
         WRITE(9,1022) 'Emergency Energy (GWh)',
     +                                     EMERGENCY_ENERGY/1000.,
     +                 'Maintenance Energy (GWh)',
     +                                   MAINTENANCE_ENERGY/1000.,
     +                 'Energy Losses (GWh)',
     +                                   AREA_ENERGY_LOSSES/1000.
         IF( .NOT. TESTING_PLAN) THEN
            SYSTEM_NON_COIN_PEAK = 0.
            SYSTEM_NON_COIN_PEAK_LESS_DSM = 0.
            SYSTEM_CAPACITY = 0.
            DO AREA = 1 , NUMBER_OF_AREAS
               SYSTEM_NON_COIN_PEAK = SYSTEM_NON_COIN_PEAK +
     +               MAX(CLASS_PEAK_AFTER_LOSSES(1,AREA),
     +                          CLASS_PEAK_AFTER_LOSSES(2,AREA))
               SYSTEM_NON_COIN_PEAK_LESS_DSM =
     +               SYSTEM_NON_COIN_PEAK_LESS_DSM +
     +                  MAX(CLASS_PEAK_AFTER_LOSSES(1,AREA),
     +                       CLASS_PEAK_AFTER_LOSSES(2,AREA)) +
     +                             DSM_PEAK_RESERVE_ALLOCATION(1,AREA)
               SYSTEM_CAPACITY = SYSTEM_CAPACITY +
     +                                       AREA_CAPACITY(AREA,ISEAS)

               IF(TOTAL_AREA_COIN_PEAK >
     +                      SUM_ANNUAL_REPORT_ITEMS(4,SUMMARY_POS)) THEN
                  SUM_ANNUAL_REPORT_ITEMS(1,AREA) =
     +               MAX(CLASS_PEAK_AFTER_LOSSES(1,AREA),
     +                           CLASS_PEAK_AFTER_LOSSES(2,AREA))
                  SUM_ANNUAL_REPORT_ITEMS(2,AREA) =
     +               -DSM_TO_LOAD_REDUCTION_BY(AREA)
                  SUM_ANNUAL_REPORT_ITEMS(3,AREA) =

     +               MAX(CLASS_PEAK_AFTER_LOSSES(1,AREA),
     +                       CLASS_PEAK_AFTER_LOSSES(2,AREA)) +
     +                               DSM_PEAK_RESERVE_ALLOCATION(1,AREA)
                  SUM_ANNUAL_REPORT_ITEMS(4,AREA) =

     +                                        AREA_COINCIDENT_PEAK(AREA)
                  SUM_ANNUAL_REPORT_ITEMS(5,AREA) =
     +                              NEEDED_RESERVE_MARGIN_CAPACITY(AREA)
                  SUM_ANNUAL_REPORT_ITEMS(6,AREA) =
     +                                         AREA_CAPACITY(AREA,ISEAS)
                  SUM_ANNUAL_REPORT_ITEMS(7,AREA) =
     +               -DSM_TO_RESERVE_MARGIN_BY(AREA)
                  SUM_ANNUAL_REPORT_ITEMS(8,AREA) =
     +               AREA_RESERVE_MARGIN_PURCHASES(AREA)
                  SUM_ANNUAL_REPORT_ITEMS(9,AREA) =

     +               CAPACITY_LAST_RESOURCE(AREA,ISEAS)
               ENDIF
               SUM_ANNUAL_REPORT_ITEMS(10,AREA) =
     +               SUM_ANNUAL_REPORT_ITEMS(10,AREA) +
     +               (CLASS_ENERGY_AFTER_LOSSES(1,AREA) +
     +                   CLASS_ENERGY_AFTER_LOSSES(2,AREA))/1000.
               SUM_ANNUAL_REPORT_ITEMS(11,AREA) =
     +               SUM_ANNUAL_REPORT_ITEMS(11,AREA) +
     +               FORECAST_DSM_ENERGY(AREA)/1000.
               SUM_ANNUAL_REPORT_ITEMS(12,AREA) =
     +               SUM_ANNUAL_REPORT_ITEMS(12,AREA) +
     +               (CLASS_ENERGY_AFTER_LOSSES(1,AREA) +
     +               CLASS_ENERGY_AFTER_LOSSES(2,AREA) +
     +               FORECAST_DSM_ENERGY(AREA))/1000.
               SUM_ANNUAL_REPORT_ITEMS(13,AREA) =
     +               SUM_ANNUAL_REPORT_ITEMS(13,AREA) +
     +               MAX(CLASS_PEAK_AFTER_LOSSES(1,AREA),
     +                       CLASS_PEAK_AFTER_LOSSES(2,AREA)) +
     +                               DSM_PEAK_RESERVE_ALLOCATION(1,AREA)
!
               WRITE(WV_AREA_NO,REC=WV_AREA_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(globecom_YEAR+get_BASE_YEAR()),
     +               CL_MONTH_NAME(ISEAS),
     +               AREA_NAME(AREA),
     +               MAX(CLASS_PEAK_AFTER_LOSSES(1,AREA),
     +                          CLASS_PEAK_AFTER_LOSSES(2,AREA)),
     +               -DSM_TO_LOAD_REDUCTION_BY(AREA),
     +               MAX(CLASS_PEAK_AFTER_LOSSES(1,AREA),
     +                       CLASS_PEAK_AFTER_LOSSES(2,AREA)) +
     +                             DSM_PEAK_RESERVE_ALLOCATION(1,AREA),
     +               AREA_COINCIDENT_PEAK(AREA),
     +               NEEDED_RESERVE_MARGIN_CAPACITY(AREA),
     +               AREA_CAPACITY(AREA,ISEAS),
     +               -DSM_TO_RESERVE_MARGIN_BY(AREA),
     +               AREA_RESERVE_MARGIN_PURCHASES(AREA),
     +               CAPACITY_LAST_RESOURCE(AREA,ISEAS),
     +               (CLASS_ENERGY_AFTER_LOSSES(1,AREA) +
     +                   CLASS_ENERGY_AFTER_LOSSES(2,AREA))/1000.,
     +               FORECAST_DSM_ENERGY(AREA)/1000.,
     +               (CLASS_ENERGY_AFTER_LOSSES(1,AREA) +
     +               CLASS_ENERGY_AFTER_LOSSES(2,AREA) +
     +               FORECAST_DSM_ENERGY(AREA))/1000.,
     +               MAX(CLASS_PEAK_AFTER_LOSSES(1,AREA),
     +                        CLASS_PEAK_AFTER_LOSSES(2,AREA)) +
     +                               DSM_PEAK_RESERVE_ALLOCATION(1,AREA)
               WV_AREA_REC = WV_AREA_REC + 1

               WRITE(WABASH_VALLEY_POWER_DSM_UNIT,
     +                                  REC=WABASH_VALLEY_POWER_DSM_REC)
     +                  PRT_ENDPOINT(),
     +                  FLOAT(globecom_YEAR+get_BASE_YEAR()),
     +                  CL_MONTH_NAME(ISEAS),
     +                  AREA_NAME(AREA),
     +                  -1*DSM_PEAK_RESERVE_ALLOCATION(1,AREA),
     +                  -1*DSM_PEAK_RESERVE_ALLOCATION(2,AREA),
     +                  -1*DSM_PEAK_RESERVE_ALLOCATION(3,AREA)
               WABASH_VALLEY_POWER_DSM_REC=WABASH_VALLEY_POWER_DSM_REC+1
            ENDDO
            CONTROL_AREA_NAME = 'Area 1'
            WRITE(WV_AREA_NO,REC=WV_AREA_REC)
     +                PRT_ENDPOINT(),
     +                FLOAT(globecom_YEAR+get_BASE_YEAR()),
     +                CL_MONTH_NAME(ISEAS),
     +                CONTROL_AREA_NAME,
     +                FORECAST_NONCO_PEAK_IN_AREA_1,
     +                -FORECAST_PEAK_DSM_IN_AREA_1,
     +                FORECAST_NONCO_PEAK_IN_AREA_1 +
     +                                      FORECAST_PEAK_DSM_IN_AREA_1,
     +                TOTAL_AREA_1_COIN_PEAK,
     +                RESERVE_MARGIN_NEEDED,
     +                CONTROL_AREA_CAPACITY,
     +                -DSM_TO_RESERVE_MARGIN_AREA1,
     +                RESERVE_MARGIN_PURCHASES_AREA1,
     +                RESOURCE_CAP_IN_AREA_1,
     +                FORECAST_ENRG_IN_AREA_1/1000.,
     +                FORECAST_DSM_ENRG_IN_AREA_1/1000.,
     +                (FORECAST_ENRG_IN_AREA_1 +
     +                               FORECAST_DSM_ENRG_IN_AREA_1)/1000.,
     +                FORECAST_NONCO_PEAK_IN_AREA_1 +
     +                                       FORECAST_PEAK_DSM_IN_AREA_1
            WV_AREA_REC = WV_AREA_REC + 1
            CONTROL_AREA_NAME = 'System'
            WRITE(WV_AREA_NO,REC=WV_AREA_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(globecom_YEAR+get_BASE_YEAR()),
     +               CL_MONTH_NAME(ISEAS),
     +               CONTROL_AREA_NAME,
     +               SYSTEM_NON_COIN_PEAK,
     +               -TOTAL_AREA_PEAK_DSM,
     +               SYSTEM_NON_COIN_PEAK_LESS_DSM,
     +               TOTAL_AREA_COIN_PEAK,
     +               ALL_CLASSES_RESERVE_MARGIN,
     +               SYSTEM_CAPACITY,
     +               -TOTAL_DSM_TO_RESERVE_MARGIN,
     +               TOTAL_RESERVE_MARGIN_PURCHASES,
     +               TOTAL_LAST_CAPACITY,
     +               TOTAL_AREA_ENERGY/1000.,
     +               TOTAL_AREA_DSM_ENERGY/1000.,
     +               (TOTAL_AREA_ENERGY+TOTAL_AREA_DSM_ENERGY)/1000.,
     +               SYSTEM_NON_COIN_PEAK_LESS_DSM
               WV_AREA_REC = WV_AREA_REC + 1
               AREA = NUMBER_OF_AREAS + 1
               IF(TOTAL_AREA_COIN_PEAK >
     +                      SUM_ANNUAL_REPORT_ITEMS(4,SUMMARY_POS)) THEN
                  SUM_ANNUAL_REPORT_ITEMS(1,AREA) =

     +                                     FORECAST_NONCO_PEAK_IN_AREA_1
                  SUM_ANNUAL_REPORT_ITEMS(2,AREA) =
     +                                      -FORECAST_PEAK_DSM_IN_AREA_1
                  SUM_ANNUAL_REPORT_ITEMS(3,AREA) =
     +                FORECAST_NONCO_PEAK_IN_AREA_1 +
     +                                      FORECAST_PEAK_DSM_IN_AREA_1
                  SUM_ANNUAL_REPORT_ITEMS(4,AREA) =
     +                                            TOTAL_AREA_1_COIN_PEAK
                  SUM_ANNUAL_REPORT_ITEMS(5,AREA) =
     +                                             RESERVE_MARGIN_NEEDED
                  SUM_ANNUAL_REPORT_ITEMS(6,AREA) =
     +                                             CONTROL_AREA_CAPACITY
                  SUM_ANNUAL_REPORT_ITEMS(7,AREA) =
     +                                      -DSM_TO_RESERVE_MARGIN_AREA1
                  SUM_ANNUAL_REPORT_ITEMS(8,AREA) =
     +                                    RESERVE_MARGIN_PURCHASES_AREA1
                  SUM_ANNUAL_REPORT_ITEMS(9,AREA) =

     +                                            RESOURCE_CAP_IN_AREA_1
               ENDIF
               SUM_ANNUAL_REPORT_ITEMS(10,AREA) =
     +               SUM_ANNUAL_REPORT_ITEMS(10,AREA) +
     +                                     FORECAST_ENRG_IN_AREA_1/1000.
               SUM_ANNUAL_REPORT_ITEMS(11,AREA) =
     +               SUM_ANNUAL_REPORT_ITEMS(11,AREA) +
     +                                 FORECAST_DSM_ENRG_IN_AREA_1/1000.
               SUM_ANNUAL_REPORT_ITEMS(12,AREA) =
     +               SUM_ANNUAL_REPORT_ITEMS(12,AREA) +
     +                (FORECAST_ENRG_IN_AREA_1 +
     +                                FORECAST_DSM_ENRG_IN_AREA_1)/1000.
               SUM_ANNUAL_REPORT_ITEMS(13,AREA) =
     +                 SUM_ANNUAL_REPORT_ITEMS(13,AREA) +
     +                          FORECAST_NONCO_PEAK_IN_AREA_1 +
     +                                      FORECAST_PEAK_DSM_IN_AREA_1
               AREA = NUMBER_OF_AREAS + 2
               IF(TOTAL_AREA_COIN_PEAK >
     +                      SUM_ANNUAL_REPORT_ITEMS(4,SUMMARY_POS)) THEN
                  SUM_ANNUAL_REPORT_ITEMS(1,AREA) =

     +                                              SYSTEM_NON_COIN_PEAK
                  SUM_ANNUAL_REPORT_ITEMS(2,AREA) =
     +                                              -TOTAL_AREA_PEAK_DSM
                  SUM_ANNUAL_REPORT_ITEMS(3,AREA) =
     +                                     SYSTEM_NON_COIN_PEAK_LESS_DSM
                  SUM_ANNUAL_REPORT_ITEMS(4,AREA) =
     +                                              TOTAL_AREA_COIN_PEAK
                  SUM_ANNUAL_REPORT_ITEMS(5,AREA) =
     +                                        ALL_CLASSES_RESERVE_MARGIN
                  SUM_ANNUAL_REPORT_ITEMS(6,AREA) =
     +                                                   SYSTEM_CAPACITY
                  SUM_ANNUAL_REPORT_ITEMS(7,AREA) =
     +                                      -TOTAL_DSM_TO_RESERVE_MARGIN
                  SUM_ANNUAL_REPORT_ITEMS(8,AREA) =
     +                                    TOTAL_RESERVE_MARGIN_PURCHASES
                  SUM_ANNUAL_REPORT_ITEMS(9,AREA) =

     +                                               TOTAL_LAST_CAPACITY
               ENDIF
               SUM_ANNUAL_REPORT_ITEMS(10,AREA) =
     +               SUM_ANNUAL_REPORT_ITEMS(10,AREA) +
     +                                           TOTAL_AREA_ENERGY/1000.
               SUM_ANNUAL_REPORT_ITEMS(11,AREA) =
     +               SUM_ANNUAL_REPORT_ITEMS(11,AREA) +
     +                                       TOTAL_AREA_DSM_ENERGY/1000.
               SUM_ANNUAL_REPORT_ITEMS(12,AREA) =
     +               SUM_ANNUAL_REPORT_ITEMS(12,AREA) +
     +                   (TOTAL_AREA_ENERGY+TOTAL_AREA_DSM_ENERGY)/1000.
               SUM_ANNUAL_REPORT_ITEMS(13,AREA) =
     +               SUM_ANNUAL_REPORT_ITEMS(13,AREA) +
     +                                     SYSTEM_NON_COIN_PEAK_LESS_DSM
            IF(ISEAS == LAST_SEASON) THEN
               DO AREA = 1 , NUMBER_OF_AREAS + 2
                  IF(AREA <= NUMBER_OF_AREAS) THEN
                     CONTROL_AREA_NAME = AREA_NAME(AREA)
                  ELSEIF(AREA == NUMBER_OF_AREAS+1) THEN
                     CONTROL_AREA_NAME = 'Area 1'
                  ELSE
                     CONTROL_AREA_NAME = 'System'
                  ENDIF
                  WRITE(WV_AREA_NO,REC=WV_AREA_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(globecom_YEAR+get_BASE_YEAR()),
     +               CL_MONTH_NAME(ISEAS+1),
     +               CONTROL_AREA_NAME,
     +               (SUM_ANNUAL_REPORT_ITEMS(I,AREA),I=1,13)
                  WV_AREA_REC = WV_AREA_REC + 1
               ENDDO
            ENDIF ! WRITE ANNUAL RESOURCE REPORT
         ENDIF ! NOT TESTING PLAN AND WRITE THE REPORT
      ENDIF
      RETURN
 1000 FORMAT(1X,A24,8F8.1)
 1005 FORMAT("&",16X,8F8.1)
 1007 FORMAT("&",24X,8F8.1)
 1010 FORMAT(1X,A20,12X,7F8.1)
 1015 FORMAT(1X,A24,8X,7F8.1)
 1020 FORMAT(1X,A24,F8.1)
 1022 FORMAT(1X,3(A24,F8.1,2X))
      END

      SUBROUTINE ANNUAL_AREA_REPORT(RESOURCE_FILE_TYPE,NUNITS,
     +                              UNITNM,UNIT_ENERGY,CAPACITY,
     +                              RESOURCE_TYPE,
     +                              RESOURCE_ANNUAL_AREA_ENRG,
     +                              RESOURCE_ANNUAL_AREA_CAP,
     +                              ANNUAL_EMERGENCY_ENERGY,
     +                              ANNUAL_MAINTENANCE_ENERGY,
     +                              ANNUAL_AREA_ENERGY_LOSSES,
     +                              online_loc,ofline_loc,
     +                              BASE_DATE)

      use SpinDriftLib
      use prod_arrays_dimensions
      use kepcocom
      use grx_planning_routines
      USE SIZECOM
      integer (kind=2) :: online_loc(NUNITS), ofline_loc(NUNITS)
      INTEGER (kind=2) ::  NUNITS
      INTEGER (kind=2) ::  ONLINE(NUNITS),OFLINE(NUNITS),BASE_DATE
      REAL ::  TOTAL_AREA_SALES
      REAL ::  UNIT_ENERGY(NUNITS),CAPACITY(2*NUNITS)
      REAL ::  ANNUAL_EMERGENCY_ENERGY(NUNITS)
      REAL ::  ANNUAL_MAINTENANCE_ENERGY(NUNITS)
      REAL ::  ANNUAL_AREA_ENERGY_LOSSES(NUNITS)
      REAL ::  RESOURCE_ANNUAL_AREA_ENRG(6,NUNITS)
      REAL ::  RESOURCE_ANNUAL_AREA_CAP(6,NUNITS)
      REAL ::  UNIT_CAPACITY,TOTAL_AREA_ENERGY,TOTAL_SURPLUS_ENERGY,

     +     SURPLUS_ENERGY_AREA1
      CHARACTER (len=20) ::  UNITNM(NUNITS),RESOURCE_FILE_TYPE*2
      CHARACTER (len=1) ::  RESOURCE_TYPE(NUNITS)
      INTEGER (kind=1) ::  AREA,I,LAST_AREA
      LOGICAL (kind=1) ::  CL_RESOURCES
      LOGICAL (kind=1) ::  CT_RESOURCES,EL_RESOURCES,CONTROL_AREA
      REAL ::  CONTROL_AREA_ENERGY(6),CONTROL_AREA_CAPACITY(6)
      REAL ::  AREA_TOTAL_CAPACITY
      REAL ::  AREA_TOTAL_ENERGY,KEPCO_TOTAL_DUMP_ENERGY
      REAL ::  ENRG_FORC_IN_AREA_1,
     +     NON_CO_FORC_IN_AREA_1,
     +     CO_FORC_IN_AREA_1
      INTEGER (kind=1) ::  AREA_POINTR
      REAL ::  RESOURCE_ENRG_IN_AREA_1,RESOURCE_CAP_IN_AREA_1
      REAL ::  TOTAL_RESOURCE_ENRG_IN_AREA_1
      REAL ::  TOTAL_RESOURCE_CAP_IN_AREA_1
      SAVE TOTAL_RESOURCE_ENRG_IN_AREA_1,TOTAL_RESOURCE_CAP_IN_AREA_1
!
      CL_RESOURCES =  INDEX(RESOURCE_FILE_TYPE,'CL') /= 0
      CT_RESOURCES =  INDEX(RESOURCE_FILE_TYPE,'CT') /= 0
      EL_RESOURCES =  INDEX(RESOURCE_FILE_TYPE,'EL') /= 0
      AREA_TOTAL_ENERGY = 0.
      DO I = 1, 6
         CONTROL_AREA_ENERGY(I) = 0.
         CONTROL_AREA_CAPACITY(I) = 0.
      ENDDO
      IF(PRINT_HEADER) THEN
         PRINT_HEADER = .FALSE.
         CALL DETAILED_REPORT_HEADER
         WRITE(9,'("&",38X,A/)') 'Annual Control Area Allocations'
         TOTAL_RESOURCE_ENRG_IN_AREA_1 = 0.
         TOTAL_RESOURCE_CAP_IN_AREA_1 = 0.
         IF(REALLY_KEPCO) THEN
            WRITE(9,'(1X,A20,T26,10(A))')    'Resource     ',
     +                                  '   Total',
     +                                  '     WPE',
     +                                  '    KP&L',
     +                                  '    KG&E',
     +                                  '   KCP&L',
     +                                  '     EDE'
            IF(NUMBER_OF_AREAS == 6) WRITE(9,'("&",A)') '    SEPC'
            WRITE(9,'("&",3A)')  '  Losses',
     +                           '  Emerg ',
     +                           '  Maint '
         ELSEIF(WABASH_VALLEY) THEN
            IF(WABASH_IM_NIPSCO_PSI) THEN
               WRITE(9,'(1X,T37,A)')
     +                   '---------- Area 1 -----------  Area 2  Area 3'
            ELSE
               WRITE(9,'(1X,T37,A)')
     +                   '------- Area 1 ------  Area 2  Area 3  Area 4'
            ENDIF
            WRITE(9,'(1X,A20,T26,A,9(A))') 'Resource     ',
     +                               '   Total',
     +                               '   Total ',
     +                               '    I&M ',
     +                               '    NIP',
     +                               '     PSI',
     +                               '      GM',
     +                               '    IP&L',
     +                               '  Losses',
     +                               '   Emerg',
     +                               '   Maint'
         ELSE
            WRITE(9,'(1X,A20,T26,A,6(A,I1),3(A))') 'Resource     ',
     +                               '   Total',
     +                              ('  Area ',I,I= 1, 6),
     +                               '  Losses',
     +                               '  Emerg ',
     +                               '  Maint '
         ENDIF
      ENDIF
      DO I = 1, NUNITS
      IF(online_loc(I) > BASE_DATE+12 .OR. ofline_loc(I) 
     + < BASE_DATE+1) then
        CYCLE
      endif
         IF(CL_RESOURCES) THEN
           CONTROL_AREA = .FALSE.
           UNIT_CAPACITY =  CAPACITY(2*I)
         ELSE
           UNIT_CAPACITY =  CAPACITY(I)
           CONTROL_AREA = INDEX('123456EMSRX',RESOURCE_TYPE(I)) /= 0
         ENDIF
         IF(CONTROL_AREA) THEN
            AREA_POINTR = INDEX('123456',RESOURCE_TYPE(I))
            IF(AREA_POINTR > 0) THEN
               CONTROL_AREA_ENERGY(AREA_POINTR) = UNIT_ENERGY(I) +
     +                                  CONTROL_AREA_ENERGY(AREA_POINTR)
               CONTROL_AREA_CAPACITY(AREA_POINTR) = MAX(UNIT_CAPACITY,
     +                               CONTROL_AREA_CAPACITY(AREA_POINTR))
               AREA_TOTAL_ENERGY = AREA_TOTAL_ENERGY + UNIT_ENERGY(I)
            ENDIF
         ELSE
            IF(WABASH_VALLEY) THEN
               RESOURCE_ENRG_IN_AREA_1= 0.
               RESOURCE_CAP_IN_AREA_1 = 0.

               DO AREA = 1, MAX_LOAD_CLASSES
                  IF(CLASS_CONTROL_AREA(AREA) /= 1) CYCLE
                  RESOURCE_ENRG_IN_AREA_1 = RESOURCE_ENRG_IN_AREA_1 +
     +                                 RESOURCE_ANNUAL_AREA_ENRG(AREA,I)
                  RESOURCE_CAP_IN_AREA_1 = RESOURCE_CAP_IN_AREA_1 +
     +                                  RESOURCE_ANNUAL_AREA_CAP(AREA,I)
               ENDDO
               TOTAL_RESOURCE_ENRG_IN_AREA_1 = RESOURCE_ENRG_IN_AREA_1 +
     +                                     TOTAL_RESOURCE_ENRG_IN_AREA_1
               TOTAL_RESOURCE_CAP_IN_AREA_1 = RESOURCE_CAP_IN_AREA_1 +
     +                                      TOTAL_RESOURCE_CAP_IN_AREA_1
               WRITE(9,1000) trim(UNITNM(I))//' (GWh)',
     +                 RESOURCE_FILE_TYPE,
     +                 UNIT_ENERGY(I)/1000.,
     +                 RESOURCE_ENRG_IN_AREA_1/1000.,
     +                 (RESOURCE_ANNUAL_AREA_ENRG(AREA,I)/1000.,
     +                                AREA = 1, NUMBER_OF_AREAS),
     +                 ANNUAL_AREA_ENERGY_LOSSES(I)/1000.,
     +                 ANNUAL_EMERGENCY_ENERGY(I)/1000.,
     +                 ANNUAL_MAINTENANCE_ENERGY(I)/1000.
               WRITE(9,1000) '(MW)',' ',UNIT_CAPACITY,
     +                       RESOURCE_CAP_IN_AREA_1,
     +                      (RESOURCE_ANNUAL_AREA_CAP(AREA,I),
     +                                        AREA = 1, NUMBER_OF_AREAS)
            ELSE
               WRITE(9,1000) trim(UNITNM(I))//' (GWh)',
     +                 RESOURCE_FILE_TYPE,
     +                 UNIT_ENERGY(I)/1000.,
     +                 (RESOURCE_ANNUAL_AREA_ENRG(AREA,I)/1000.,
     +                                AREA = 1, NUMBER_OF_AREAS),
     +                 ANNUAL_AREA_ENERGY_LOSSES(I)/1000.,
     +                 ANNUAL_EMERGENCY_ENERGY(I)/1000.,
     +                 ANNUAL_MAINTENANCE_ENERGY(I)/1000.
               WRITE(9,1000) '(MW)',' ',UNIT_CAPACITY,
     +             (RESOURCE_ANNUAL_AREA_CAP(AREA,I),
     +                                 AREA = 1, NUMBER_OF_AREAS)
            ENDIF
         ENDIF
      ENDDO
      IF(CT_RESOURCES) THEN
         IF(WABASH_VALLEY) THEN
            RESOURCE_ENRG_IN_AREA_1= 0.
            RESOURCE_CAP_IN_AREA_1 = 0.
            ENRG_FORC_IN_AREA_1 = 0.
            NON_CO_FORC_IN_AREA_1 = 0.
            CO_FORC_IN_AREA_1 = 0.
            LAST_AREA = 2
            IF(WABASH_IM_NIPSCO_PSI) LAST_AREA = 3
            DO AREA = 1, MAX_LOAD_CLASSES
               IF(CLASS_CONTROL_AREA(AREA) == 1) THEN
                  RESOURCE_ENRG_IN_AREA_1 = RESOURCE_ENRG_IN_AREA_1 +
     +                                         CONTROL_AREA_ENERGY(AREA)
                  RESOURCE_CAP_IN_AREA_1 = RESOURCE_CAP_IN_AREA_1 +
     +                                       CONTROL_AREA_CAPACITY(AREA)
                  ENRG_FORC_IN_AREA_1 = ENRG_FORC_IN_AREA_1 +
     +                                         ANNUAL_AREA_ENERGY(AREA)
                  NON_CO_FORC_IN_AREA_1 = NON_CO_FORC_IN_AREA_1 +
     +                                   ANNUAL_NONCOINCIDENT_PEAK(AREA)
                  CO_FORC_IN_AREA_1 = CO_FORC_IN_AREA_1 +
     +                                      ANNUAL_COINCIDENT_PEAK(AREA)
               ENDIF
            ENDDO
            WRITE(9,1010) 'Area Resources (GWh)',
     +                               TOTAL_RESOURCE_ENRG_IN_AREA_1/1000.
            WRITE(9,1010) 'Area Resources (MW)',
     +                                      TOTAL_RESOURCE_CAP_IN_AREA_1
            SURPLUS_ENERGY_AREA1=MAX(0.,TOTAL_RESOURCE_ENRG_IN_AREA_1-
     +                                              ENRG_FORC_IN_AREA_1)
            TOTAL_SURPLUS_ENERGY = SURPLUS_ENERGY_AREA1
            DO AREA = 1, MAX_LOAD_CLASSES
               IF(CLASS_CONTROL_AREA(AREA) /= 1) THEN
                  TOTAL_SURPLUS_ENERGY = TOTAL_SURPLUS_ENERGY +
     +                                  ANNUAL_AREA_SURPLUS_ENERGY(AREA)
               ENDIF
            ENDDO
            WRITE(9,1005) 'Surplus Energy (GWh)',
     +                     TOTAL_SURPLUS_ENERGY/1000.,
     +                     SURPLUS_ENERGY_AREA1/1000.
            IF(WABASH_IM_NIPSCO) THEN
               WRITE(9,1030) (ANNUAL_AREA_SURPLUS_ENERGY(AREA)/1000.,
     +                              AREA = LAST_AREA+1, NUMBER_OF_AREAS)
            ELSE
               WRITE(9,1031) (ANNUAL_AREA_SURPLUS_ENERGY(AREA)/1000.,
     +                              AREA = LAST_AREA+1, NUMBER_OF_AREAS)
            ENDIF
            WRITE(9,1005) 'Area Utilities (GWh)',
     +                 AREA_TOTAL_ENERGY/1000.,
     +                 RESOURCE_ENRG_IN_AREA_1/1000.,
     +                (CONTROL_AREA_ENERGY(AREA)/1000.,
     +                                        AREA = 1, NUMBER_OF_AREAS)
            AREA_TOTAL_CAPACITY = 0.
            DO AREA = 1, NUMBER_OF_AREAS
               AREA_TOTAL_CAPACITY = AREA_TOTAL_CAPACITY +
     +                                       CONTROL_AREA_CAPACITY(AREA)
            ENDDO
            WRITE(9,1005) 'Capacity (MW)',AREA_TOTAL_CAPACITY,
     +                    RESOURCE_CAP_IN_AREA_1,
     +           (CONTROL_AREA_CAPACITY(AREA),AREA = 1, NUMBER_OF_AREAS)
            TOTAL_AREA_ENERGY = 0.
            DO AREA = 1, NUMBER_OF_AREAS
               TOTAL_AREA_ENERGY = TOTAL_AREA_ENERGY +
     +                                          ANNUAL_AREA_ENERGY(AREA)
               TOTAL_SURPLUS_ENERGY = TOTAL_SURPLUS_ENERGY +
     +                                  ANNUAL_AREA_SURPLUS_ENERGY(AREA)
            ENDDO
            WRITE(9,1005) 'Area Forecasts (GWh)',
     +                        TOTAL_AREA_ENERGY/1000.,
     +                        ENRG_FORC_IN_AREA_1/1000.,
     +                       (ANNUAL_AREA_ENERGY(AREA)/1000.,
     +                                        AREA = 1, NUMBER_OF_AREAS)
            WRITE(9,1010) 'Non-coincident peak (MW)',
     +                     NON_CO_FORC_IN_AREA_1,
     +                    (ANNUAL_NONCOINCIDENT_PEAK(AREA),
     +                                        AREA = 1, NUMBER_OF_AREAS)
            WRITE(9,1005) 'Coincident peak (MW)',
     +                    (ANNUAL_COINCIDENT_PEAK(1) +
     +                      ANNUAL_COINCIDENT_PEAK(2) +
     +                       ANNUAL_COINCIDENT_PEAK(3) +
     +                        ANNUAL_COINCIDENT_PEAK(4) +
     +                         ANNUAL_COINCIDENT_PEAK(5)),
     +                    CO_FORC_IN_AREA_1,
     +                    (ANNUAL_COINCIDENT_PEAK(AREA),
     +                                        AREA = 1, NUMBER_OF_AREAS)
         ELSE
            WRITE(9,1000) 'Area Utilities (GWh)',' ',
     +                 AREA_TOTAL_ENERGY/1000.,
     +                (CONTROL_AREA_ENERGY(AREA)/1000.,
     +                                        AREA = 1, NUMBER_OF_AREAS)
            AREA_TOTAL_CAPACITY = 0.
            DO AREA = 1, NUMBER_OF_AREAS
               AREA_TOTAL_CAPACITY = AREA_TOTAL_CAPACITY +
     +                                       CONTROL_AREA_CAPACITY(AREA)
            ENDDO
            WRITE(9,1000) 'Capacity (MW)',' ',AREA_TOTAL_CAPACITY,
     +           (CONTROL_AREA_CAPACITY(AREA),AREA = 1, NUMBER_OF_AREAS)
            TOTAL_AREA_ENERGY = 0.
            TOTAL_SURPLUS_ENERGY = 0.
            DO AREA = 1, NUMBER_OF_AREAS
               TOTAL_AREA_ENERGY = TOTAL_AREA_ENERGY +
     +                                          ANNUAL_AREA_ENERGY(AREA)
               TOTAL_SURPLUS_ENERGY = TOTAL_SURPLUS_ENERGY +
     +                                  ANNUAL_AREA_SURPLUS_ENERGY(AREA)
            ENDDO
            IF(REALLY_KEPCO) THEN
               TOTAL_AREA_SALES = 0.
               KEPCO_TOTAL_DUMP_ENERGY = 0.
               DO AREA = 1, NUMBER_OF_AREAS
                  TOTAL_AREA_SALES = TOTAL_AREA_SALES +
     +                                     KEPCO_ANNUAL_AREA_SALES(AREA)
                  KEPCO_TOTAL_DUMP_ENERGY = KEPCO_TOTAL_DUMP_ENERGY +
     +                                    KEPCO_ANNUAL_DUMP_ENERGY(AREA)
               ENDDO
               WRITE(9,1000) 'Area Sales (GWh)',' ',
     +                      TOTAL_AREA_SALES/1000.,
     +                      (KEPCO_ANNUAL_AREA_SALES(AREA)/1000.,
     +                                        AREA = 1, NUMBER_OF_AREAS)
               WRITE(9,1000) 'Area Losses (GWh)',' ',
     +                  (TOTAL_AREA_ENERGY - TOTAL_AREA_SALES -
     +                                   KEPCO_TOTAL_DUMP_ENERGY)/1000.,
     +                 ((ANNUAL_AREA_ENERGY(AREA) -
     +                        KEPCO_ANNUAL_DUMP_ENERGY(AREA) -
     +                             KEPCO_ANNUAL_AREA_SALES(AREA))/1000.,
     +                                        AREA = 1, NUMBER_OF_AREAS)
               WRITE(9,1000) 'Area Other Sales (GWh)',' ',
     +                     KEPCO_TOTAL_DUMP_ENERGY/1000.,
     +                     (KEPCO_ANNUAL_DUMP_ENERGY(AREA)/1000.,
     +                                        AREA = 1, NUMBER_OF_AREAS)
               WRITE(9,1000) 'Area Purchases (GWh)',' ',
     +                  TOTAL_AREA_ENERGY/1000.,
     +                 (ANNUAL_AREA_ENERGY(AREA)/1000.,
     +                                        AREA = 1, NUMBER_OF_AREAS)
            ELSE
               WRITE(9,1000) 'Area Forecasts (GWh)',' ',
     +                     TOTAL_AREA_ENERGY/1000.,
     +                    (ANNUAL_AREA_ENERGY(AREA)/1000.,
     +                                        AREA = 1, NUMBER_OF_AREAS)
            ENDIF
            WRITE(9,1010) 'Coincident peak (MW)',
     +                    (ANNUAL_COINCIDENT_PEAK(AREA),
     +                                        AREA = 1, NUMBER_OF_AREAS)
            WRITE(9,1000) 'Surplus Energy (GWh)',' ',
     +                    TOTAL_SURPLUS_ENERGY/1000.,
     +                   (ANNUAL_AREA_SURPLUS_ENERGY(AREA)/1000.,
     +                                        AREA = 1, NUMBER_OF_AREAS)
            WRITE(9,1010) 'Non-coincident peak (MW)',
     +                    (ANNUAL_NONCOINCIDENT_PEAK(AREA),
     +                                        AREA = 1, NUMBER_OF_AREAS)
         ENDIF
         WRITE(9,*)
         WRITE(9,1020) 'Emergency Energy (GWh)',
     +                                 TOTAL_EMERGENCY_ENERGY/1000.
         WRITE(9,1020) 'Maintenance Energy (GWh)',
     +                                 TOTAL_MAINTENANCE_ENERGY/1000.
         WRITE(9,1020) 'Energy Losses (GWh)',
     +                                 TOTAL_AREA_RESOURCE_LOSSES/1000.
      ENDIF
      RETURN
 1000 FORMAT(1X,A20,2X,A2,12F8.1)
 1005 FORMAT(1X,A24,12F8.1)
 1010 FORMAT(1X,A20,12X,10F8.1)
 1020 FORMAT(1X,A24,F8.1)
 1030 FORMAT("&",16X,8F8.1)
 1031 FORMAT("&",24X,8F8.1)
      END
      SUBROUTINE KEPCO_DEFERRED_MAINTENANCE(MO,MAINTENANCE_EXPENSE,
     +                                      CURRENT_MAINTENANCE_EXPENSE)
      use SpinDriftLib
      use prod_arrays_dimensions
      use kepcocom
      use sizecom

      REAL ::  MAINTENANCE_EXPENSE,CURRENT_MAINTENANCE_EXPENSE
      REAL ::  AMORT_AMOUNT
      INTEGER (kind=2) ::  MO
      INTEGER (kind=2) ::  AMORT_MONTHS
      INTEGER (kind=2) ::  AMORT_START_MONTH,CURRENT_YEAR_MONTHS,

     +          NEXT_YEAR_MONTHS,YEAR_AFTER_MONTHS,YEAR
      REAL ::  NEXT_YEAR_AMORT,YEAR_AFTER_AMORT
      SAVE NEXT_YEAR_AMORT,YEAR_AFTER_AMORT
!
      CURRENT_MAINTENANCE_EXPENSE = MIN(MAINTENANCE_EXPENSE,
     +                                      CURRENT_MAINTENANCE_EXPENSE)
      MAINTENANCE_EXPENSE = MAINTENANCE_EXPENSE -
     +                                       CURRENT_MAINTENANCE_EXPENSE
      IF(CURRENT_MAINTENANCE_EXPENSE > .005)
     +   KEPCO_WC_CURENT_MAINT_ENRG_COST = CURRENT_MAINTENANCE_EXPENSE +
     +                                   KEPCO_WC_CURENT_MAINT_ENRG_COST
      IF(MAINTENANCE_EXPENSE <= .005) RETURN
      KEPCO_WC_DEF_MAINT_ENRG_COST = KEPCO_WC_DEF_MAINT_ENRG_COST +
     +                               MAINTENANCE_EXPENSE
      AMORT_MONTHS = 18
      AMORT_START_MONTH = 13
      AMORT_AMOUNT = MAINTENANCE_EXPENSE/FLOAT(AMORT_MONTHS)
      CURRENT_YEAR_MONTHS = AMORT_START_MONTH - MO
      NEXT_YEAR_MONTHS = MIN(12,AMORT_MONTHS-(AMORT_START_MONTH - MO))
      YEAR_AFTER_MONTHS = AMORT_MONTHS - CURRENT_YEAR_MONTHS -
     +                    NEXT_YEAR_MONTHS
      KEPCO_WC_DEF_MAINT_ENRG_AMORT = KEPCO_WC_DEF_MAINT_ENRG_AMORT +
     +                                CURRENT_YEAR_MONTHS * AMORT_AMOUNT
      NEXT_YEAR_AMORT = NEXT_YEAR_AMORT +
     +                  NEXT_YEAR_MONTHS * AMORT_AMOUNT
      YEAR_AFTER_AMORT = YEAR_AFTER_AMORT +
     +                   YEAR_AFTER_MONTHS * AMORT_AMOUNT
      RETURN

      ENTRY INIT_WC_DEF_MAINTENANCE(YEAR)

      IF(YEAR == 1) THEN
         KEPCO_WC_DEF_MAINT_ENRG_AMORT = 0.
         NEXT_YEAR_AMORT = 0.
      ELSE
         KEPCO_WC_DEF_MAINT_ENRG_AMORT = NEXT_YEAR_AMORT
         NEXT_YEAR_AMORT = YEAR_AFTER_AMORT
      ENDIF
      YEAR_AFTER_AMORT = 0.
      KEPCO_WC_DEF_MAINT_ENRG_COST = 0.
      KEPCO_WC_CURENT_MAINT_ENRG_COST = 0.
      RETURN
      END

      SUBROUTINE WABASH_LOSSES(YR,ISEAS,ENERGY,CAPACITY,
     +                         res_ids,NUNITS,SEAS_HOURS,
     +                         RESOURCE_FILE_TYPE,
     +                         RESOURCE_TYPE,
     +                         online_loc,ofline_loc,
     +                         DATE1,DATE2,UNITNM,UNIT_LOSSES)

      use SpinDriftLib
      use prod_arrays_dimensions
      use kepcocom
      USE sizecom

!
      INTEGER (kind=2) ::  ISEAS,NUNITS,YR,I
      REAL ::  SEAS_HOURS
      CHARACTER (len=20) ::  UNITNM(NUNITS)
      INTEGER*2 online_loc(NUNITS),ofline_loc(NUNITS),
     +          DATE1,DATE2,ARRAY_POINTR,AREA
      CHARACTER (len=2) ::  RESOURCE_FILE_TYPE,RESOURCE_TYPE(NUNITS)
      INTEGER*2 res_ids(NUNITS)
      REAL ::  ENERGY(2*NUNITS)
      REAL ::  UNIT_ENERGY,RESOURCE_LOSSES,ENRG_ALLOCATOR
      REAL ::  GET_VAR
      REAL ::  CAPACITY(NUNITS),UNIT_LOSSES(NUNITS),UNIT_CAPACITY,

     +         GIBSON_5_BACKUP_LOSSES
      REAL ::  RESOURCE_ENRG(6),TOTAL_ENRG_ALLOCATED
      LOGICAL (kind=1) ::  CL_RESOURCES,CT_RESOURCES,CONTROL_AREA
!
      CL_RESOURCES =  INDEX(RESOURCE_FILE_TYPE,'CL') /= 0
      CT_RESOURCES =  INDEX(RESOURCE_FILE_TYPE,'CT') /= 0
      DO I = 1, NUNITS
         IF(online_loc(I) > DATE2 .OR. ofline_loc(I) < DATE1) CYCLE
         UNIT_CAPACITY = CAPACITY(I)
         TOTAL_ENRG_ALLOCATED = 0.
         IF(CL_RESOURCES) THEN
            CONTROL_AREA = .FALSE.
            UNIT_ENERGY = ENERGY(2*I-1) + ENERGY(2*I)
            IF(I == GIBSON_BACKUP_POINTER) THEN
               ARRAY_POINTR =
     +                   CL_ALLOCATE_POINTR(res_ids(GIBSON_POINTER))
               UNIT_CAPACITY = CAPACITY(GIBSON_POINTER)
               GIBSON_5_BACKUP_LOSSES = 0.
               IF(UNIT_CAPACITY <= 0.0001) THEN
                  DO AREA = 1, NUMBER_OF_AREAS
                     GIBSON_5_BACKUP_LOSSES = GIBSON_5_BACKUP_LOSSES +
     +                      AREA_RESOURCE_LOSES(AREA,ISEAS,ARRAY_POINTR)
                  ENDDO
                  IF(GIBSON_5_BACKUP_LOSSES < 1.0) THEN
                     GIBSON_5_BACKUP_LOSSES =
     +                     1./(1.-GIBSON_5_BACKUP_LOSSES) - 1.
                  ENDIF
               ENDIF
            ELSE
               ARRAY_POINTR = CL_ALLOCATE_POINTR(res_ids(I))
            ENDIF
         ELSEIF(CT_RESOURCES) THEN
            UNIT_ENERGY = ENERGY(I)
            CONTROL_AREA=INDEX('123456EMSR',RESOURCE_TYPE(I)) /= 0
            ARRAY_POINTR = CT_ALLOCATE_POINTR(res_ids(I))
         ENDIF
         RESOURCE_LOSSES = 0.
         IF(CONTROL_AREA) CYCLE
         DO AREA = 1, NUMBER_OF_AREAS
            UNIT_CAPACITY = UNIT_CAPACITY *
     +                (1.-AREA_CAPACITY_LOSSES(AREA,ISEAS,ARRAY_POINTR))

! ALLOCATE ENERGY

            ENRG_ALLOCATOR =
     +                     AREA_ENRG_ALLOCATORS(AREA,ISEAS,ARRAY_POINTR)
            IF(ENRG_ALLOCATOR < 0.) ENRG_ALLOCATOR =
     +                              GET_VAR(ENRG_ALLOCATOR,YR,UNITNM(I))
            IF(ENRG_ALLOCATOR <= 2. .AND.
     +                            .NOT. ENERGY_ALLOCATION_ABSOLUTE) THEN
               ENRG_ALLOCATOR=ENRG_ALLOCATOR*UNIT_CAPACITY*SEAS_HOURS
            ELSEIF(ENRG_ALLOCATOR<=SEAS_HOURS)THEN
               ENRG_ALLOCATOR = ENRG_ALLOCATOR * UNIT_CAPACITY
            ENDIF
            RESOURCE_ENRG(AREA) = ENRG_ALLOCATOR
            TOTAL_ENRG_ALLOCATED = TOTAL_ENRG_ALLOCATED +
     +                                               RESOURCE_ENRG(AREA)
         ENDDO ! AREA
!
! ADJUST FOR RELATIVE ALLOCATIONS
!
         DO AREA = 1, NUMBER_OF_AREAS
!
            IF(ENERGY_ALLOCATION_ABSOLUTE) THEN
               RESOURCE_ENRG(AREA) = RESOURCE_ENRG(AREA)/SEAS_HOURS
            ELSEIF(TOTAL_ENRG_ALLOCATED /= 0.) THEN
               RESOURCE_ENRG(AREA) = UNIT_ENERGY *
     +                        (RESOURCE_ENRG(AREA)/TOTAL_ENRG_ALLOCATED)
            ENDIF
            RESOURCE_LOSSES = RESOURCE_LOSSES+RESOURCE_ENRG(AREA)*
     +                  AREA_RESOURCE_LOSES(AREA,ISEAS,ARRAY_POINTR)/
     +                (1.-AREA_RESOURCE_LOSES(AREA,ISEAS,ARRAY_POINTR))
         ENDDO ! AREA
! 07/30/04.

! NEED A BETTER CALCULATION OF GIBSON CAPACITY WHEN 100% OUT ON 
! MAINTENANCE

!
         IF(I == GIBSON_BACKUP_POINTER .AND. UNIT_CAPACITY < .0001) THEN
            UNIT_LOSSES(I) = GIBSON_5_BACKUP_LOSSES * UNIT_ENERGY
         ELSE
            IF(RESOURCE_LOSSES /= 0.) UNIT_LOSSES(I) = RESOURCE_LOSSES
         ENDIF
      ENDDO ! UNITS
      RETURN
      END

      SUBROUTINE WABASH_CT_LOSSES(  YR,ISEAS,ENERGY,CAPACITY,
     +                              res_ids,NUNITS,SEAS_HOURS,
     +                              RESOURCE_FILE_TYPE,
     +                              RESOURCE_TYPE,
     +                              online_loc,ofline_loc,
     +                              DATE1,DATE2,UNITNM,UNIT_LOSSES)

      use SpinDriftLib
      use prod_arrays_dimensions
      use kepcocom
      USE SIZECOM

!
      INTEGER (kind=2) ::  ISEAS,NUNITS,YR,I
      REAL ::  SEAS_HOURS
      CHARACTER (len=20) ::  UNITNM(NUNITS)
      INTEGER*2 online_loc(NUNITS),ofline_loc(NUNITS),
     +          DATE1,DATE2,ARRAY_POINTR,AREA
      CHARACTER (len=2) ::  RESOURCE_FILE_TYPE
      CHARACTER (len=1) ::  RESOURCE_TYPE(NUNITS)
      INTEGER*2 res_ids(NUNITS)
      REAL ::  ENERGY(2*NUNITS)
      REAL ::  UNIT_ENERGY,RESOURCE_LOSSES,ENRG_ALLOCATOR
      REAL ::  GET_VAR
      REAL ::  CAPACITY(NUNITS),UNIT_LOSSES(NUNITS),UNIT_CAPACITY
      REAL ::  RESOURCE_ENRG(6),TOTAL_ENRG_ALLOCATED
      LOGICAL (kind=1) ::  CL_RESOURCES,CT_RESOURCES,CONTROL_AREA


      CT_RESOURCES =  INDEX(RESOURCE_FILE_TYPE,'CT') /= 0
      DO I = 1, NUNITS
         IF(online_loc(I) > DATE2 .OR. ofline_loc(I) < DATE1) CYCLE
         UNIT_CAPACITY = CAPACITY(I)
         TOTAL_ENRG_ALLOCATED = 0.
!
         UNIT_ENERGY = ENERGY(I)
         CONTROL_AREA=INDEX('123456EMSR',RESOURCE_TYPE(I)) /= 0
         ARRAY_POINTR = CT_ALLOCATE_POINTR(res_ids(I))
!
         RESOURCE_LOSSES = 0.
         IF(CONTROL_AREA) CYCLE
         DO AREA = 1, NUMBER_OF_AREAS
            ENRG_ALLOCATOR =
     +                      AREA_RESOURCE_LOSES(AREA,ISEAS,ARRAY_POINTR)
            UNIT_LOSSES(I) = UNIT_LOSSES(I) + ENERGY(I) * ENRG_ALLOCATOR
         ENDDO ! AREA
      ENDDO ! UNITS
      RETURN
      END


      SUBROUTINE WABASH_POWER_COST_REPORT(R_CL_UNITS,R_CONTRACTS,
     +                                    R_EL_UNITS)
      use energy_trace
      use mod_base_year
      USE IREC_ENDPOINT_CONTROL
      use SpinDriftLib
      use prod_arrays_dimensions
      use kepcocom
      use endpoint
      use sizecom
	  use production
      use globecom
      use contracts_data
      use pp_objt_interfaces
      use tf_objt_interfaces
!
      LOGICAL (kind=1) ::    GET_MONTHLY_TRANS_VARIABLES,
     +            VOID_LOGICAL,
     +            TRANS_NOT_ACTIVE
      INTEGER (kind=2) ::       TRANS,
     +               NUM_TRANSACTIONS,
     +               GET_NUM_SCENARIO_TRANSACTIONS,
     +               NUM_TRANS/0/,
     +               GET_NUM_ANNUAL_TRANSACTIONS,
     +               SUM_ANNUAL/0/,
     +               LOCAL_YEAR
      REAL (kind=4) :: 

     +                  TRANS_CAP,
     +                  TRANS_VAR_EXP,
     +                  TRANS_VAR_MWH,
     +                  TRANS_FIX_EXP,
     +                  TRANS_REV,
     +                  TRANS_REV_MWH,
     +                  TRANS_HOURS,
     +                  PRODUCT_HOURS,
     +                  TRANS_STRIKES,
     +                  TRANS_ON_LINE,
     +                  TRANS_OFF_LINE,
     +                  TRANS_UNIT_ID,
     +                  TRANS_ASSET_CLASS,
     +                  TEMP_YEAR
!

      INTEGER (kind=2) ::  ISEAS,YR,I,TRANS_GROUP,TEMP_I2,TG,PM,FT
      INTEGER (kind=2) ::    CL_UNITS,CONTRACTS,EL_UNITS,PEAK_MONTH,

     +            SAVE_PEAK_MONTH/0/
      INTEGER (kind=2) ::    R_CL_UNITS,R_CONTRACTS,R_EL_UNITS,
     +            TYPE,
     +            R_TYPE,
     +            R_COST,
     +            R_TRACKER_TYPE,
     +            TRACKER,
     +            GET_THERMAL_TRACKER_INDEX,
     +            GET_P_DERIV_TRACKER_INDEX,
     +            GET_P_DERIV_MEM_TRK_INDEX,
     +            GET_THERMAL_RES_TRACKER_INDEX,
     +            GET_THERMAL_FUEL_TRACKER_INDEX,
     +            GET_THERMAL_MEM_TRACKER_INDEX,

     +            UPPER_TRANS_GROUP/1/
      REAL ::  CL_ENERGY(2,*),SEAS_HOURS,BLOCK_FUEL_COST(2,*),

     +     LOSSES(*),UNIT_CAP(*),UNIT_CAP_AFTER_LOSSES(*),
     +     UNIT_MONTHLY_FIXED_COST(*),VARIABLE_OM_CHARGE,
     +     TEMP_VAR_OM_CHARGE,
     +     MONTHLY_EMISSIONS_COST,
     +     TEMP_VOM_RATE,
     +     VARIABLE_OM_RATE(*),FUEL_COST_ADJ(*),
     +     MONTHLY_ECONOMY_BOUGHT,MONTHLY_ECONOMY_COST,
     +     MONTHLY_ECONOMY_SOLD,MONTHLY_ECONOMY_REVENUE,
     +     UNSERVED_COST,
     +     UNSERVED_ENERGY,
     +     TEMP_R1,
     +     TEMP_R2,
     +     RES_TRACKER,
     +     FUEL_TRACKER,
     +     MEM_TRACKER,
     +     RATE_TRACKER,
     +     TEMP_R
      REAL ::  CT_ENERGY(*),CT_CAPACITY(*),UNIT_CAPACITY,
     +     UNIT_CAPACITY_AFTER_LOSSES,CT_LOSSES,MONTHLY_FIXED_COST
      REAL ::  ENRG,FUEL_COST,DISPATCHED_ENERGY,
     +     ANNUAL_VARIABLE_OM_RATE,POWER_LOSSES,SALES_MULT
      REAL ::  LOSS_RATE,FIXED_CHARGE_RATE,FUEL_CHARGE_RATE
      REAL ::  TOTAL_ALL_COSTS,CAPACITY_FACTOR,ANNUAL_HOURS/8760./
      INTEGER (kind=2) ::  WABASH_REPORT_UNIT_NO/0/
      INTEGER (kind=2) ::  WABASH_VALLEY_POWER_COST_HEADER
      INTEGER ::  WABASH_REPORT_UNIT_REC
      SAVE WABASH_REPORT_UNIT_REC
      INTEGER (kind=2) ::  LAST_SEASON,PRODUCTION_PERIODS
      CHARACTER (len=9) ::  RPT_MONTH_NAME(13)
      CHARACTER (len=20) ::  MONTH_NAME
      character (len=20) :: UNIT_NAMES(:)
      CHARACTER (len=20) ::  UNIT_NAME,DERIV_NAME
      LOGICAL (kind=1) ::  WABASH_REPORT_NOT_OPEN/.TRUE./,PRINT_REPORT,
     +            FIRST_CL_WRITE/.TRUE./,FIRST_CT_WRITE/.TRUE./,
     +            YES_REPORT_PRODUCT,
     +            EXPENSE_ASSIGNMENT_IS_PURCHASE
      CHARACTER (len=1) ::  WABASH_POWER_COST_RPT
!
      REAL (KIND=4) ::  SUM_REPORT_ITEMS(20),AVERAGE_TOTAL_COST,
     +                  AVERAGE_VARIABLE_COST,
     +                  R_SUM_REPORT_ITEMS(20),
     +                  ANN_SUM_REPORT_ITEMS(20),
     +                  MD_SUM_REPORT_ITEMS(20)
      REAL (kind=8) ::    OUTPUT_ANN_FUEL
      REAL (kind=8) ::    OUTPUT_ANN_VARI,OUTPUT_ANN_TOTAL
      REAL (kind=4) ::    OUTPUT_ANN_FIX
      REAL (kind=4) ::    OUTPUT_ANN_AVE_TOTAL,OUTPUT_ANN_PURCHASE
      SAVE  SUM_REPORT_ITEMS,ANN_SUM_REPORT_ITEMS
!
      REAL (kind=4) :: 
     +      ANNUAL_PURCHASE_POWER_COST/0./,
     +      PURCHASE_POWER_COST/0./,
     +      ANNUAL_POWER_PURCHASED_MWH/0./,
     +      POWER_PURCHASED_MWH/0./,
     +      ANNUAL_COST_OF_POWER_SOLD/0./,
     +      COST_OF_POWER_SOLD/0./,
     +      ANNUAL_FUEL_COST_OF_POWER_SOLD/0./,
     +      FUEL_COST_OF_POWER_SOLD/0./,
     +      ANNUAL_VOM_COST_OF_POWER_SOLD/0./,
     +      VOM_COST_OF_POWER_SOLD/0./,
     +      ANNUAL_MARKET_REVENUE/0./,
     +      MARKET_REVENUE/0./,
     +      ANNUAL_POWER_SOLD_MWH/0./,
     +      POWER_SOLD_MWH/0./

      REAL (KIND=4), ALLOCATABLE, SAVE :: ANNUAL_CL_DISPATCHED_ENRG(:),
     +                                  ANNUAL_CL_DISPATCHED_CAP(:),
     +                                  ANNUAL_CL_LOSSES(:),
     +                                  ANNUAL_CL_PURCHASED_ENRG(:),
     +                                  ANNUAL_CL_PURCHASED_CAP(:),
     +                                  ANNUAL_CL_FIXED_CHARGE(:),
     +                                  ANNUAL_CL_FUEL_CHARGE(:),
     +                                  ANNUAL_CL_VARIABLE_CHARGE(:),
     +                                  ANNUAL_CL_MAX_DISPATCHED_CAP(:),
     +                                  ANNUAL_CL_MAX_PURCHASED_CAP(:),
     +                                  ANNUAL_CL_EMISSION_COST(:)
!
      REAL ::  ANNUAL_MD_DISPATCHED_ENRG,ANNUAL_MD_DISPATCHED_CAP,
     +     ANNUAL_MD_LOSSES,ANNUAL_MD_PURCHASED_ENRG,
     +     ANNUAL_MD_PURCHASED_CAP,ANNUAL_MD_FIXED_CHARGE,
     +     ANNUAL_MD_FUEL_CHARGE,ANNUAL_MD_VARIABLE_CHARGE,
     +     ANNUAL_MD_MAX_DISPATCHED_CAP,
     +     ANNUAL_MD_MAX_PURCHASED_CAP
!
      REAL ::  ANNUAL_EL_DISPATCHED_ENRG(:),ANNUAL_EL_DISPATCHED_CAP(:),
     +     ANNUAL_EL_LOSSES(:),ANNUAL_EL_PURCHASED_ENRG(:),
     +     ANNUAL_EL_PURCHASED_CAP(:),ANNUAL_EL_FIXED_CHARGE(:),
     +     ANNUAL_EL_FUEL_CHARGE(:),ANNUAL_EL_VARIABLE_CHARGE(:),
     +     ANNUAL_EL_MAX_DISPATCHED_CAP(:),
     +     ANNUAL_EL_MAX_PURCHASED_CAP(:)
      ALLOCATABLE :: ANNUAL_EL_DISPATCHED_ENRG,ANNUAL_EL_DISPATCHED_CAP,
     +     ANNUAL_EL_LOSSES,ANNUAL_EL_PURCHASED_ENRG,
     +     ANNUAL_EL_PURCHASED_CAP,ANNUAL_EL_FIXED_CHARGE,
     +     ANNUAL_EL_FUEL_CHARGE,ANNUAL_EL_VARIABLE_CHARGE,
     +     ANNUAL_EL_MAX_DISPATCHED_CAP,
     +     ANNUAL_EL_MAX_PURCHASED_CAP
      SAVE ANNUAL_EL_DISPATCHED_ENRG,ANNUAL_EL_DISPATCHED_CAP,
     +     ANNUAL_EL_LOSSES,ANNUAL_EL_PURCHASED_ENRG,
     +     ANNUAL_EL_PURCHASED_CAP,ANNUAL_EL_FIXED_CHARGE,
     +     ANNUAL_EL_FUEL_CHARGE,ANNUAL_EL_VARIABLE_CHARGE,
     +     ANNUAL_EL_MAX_DISPATCHED_CAP,
     +     ANNUAL_EL_MAX_PURCHASED_CAP
!
      REAL ::  ANNUAL_CT_DISPATCHED_ENRG(:),ANNUAL_CT_DISPATCHED_CAP(:),
     +     ANNUAL_CT_LOSSES(:),ANNUAL_CT_PURCHASED_ENRG(:),
     +     ANNUAL_CT_PURCHASED_CAP(:),ANNUAL_CT_FIXED_CHARGE(:),
     +     ANNUAL_CT_FUEL_CHARGE(:),ANNUAL_CT_VARIABLE_CHARGE(:),
     +     ANNUAL_CT_MAX_DISPATCHED_CAP(:),
     +     ANNUAL_CT_MAX_PURCHASED_CAP(:)
      ALLOCATABLE :: ANNUAL_CT_DISPATCHED_ENRG,ANNUAL_CT_DISPATCHED_CAP,
     +     ANNUAL_CT_LOSSES,ANNUAL_CT_PURCHASED_ENRG,
     +     ANNUAL_CT_PURCHASED_CAP,ANNUAL_CT_FIXED_CHARGE,
     +     ANNUAL_CT_FUEL_CHARGE,ANNUAL_CT_VARIABLE_CHARGE,
     +     ANNUAL_CT_MAX_DISPATCHED_CAP,
     +     ANNUAL_CT_MAX_PURCHASED_CAP
      SAVE ANNUAL_CT_DISPATCHED_ENRG,ANNUAL_CT_DISPATCHED_CAP,
     +     ANNUAL_CT_LOSSES,ANNUAL_CT_PURCHASED_ENRG,
     +     ANNUAL_CT_PURCHASED_CAP,ANNUAL_CT_FIXED_CHARGE,
     +     ANNUAL_CT_FUEL_CHARGE,ANNUAL_CT_VARIABLE_CHARGE,
     +     ANNUAL_CT_MAX_DISPATCHED_CAP,
     +     ANNUAL_CT_MAX_PURCHASED_CAP

      SAVE RPT_MONTH_NAME,LAST_SEASON
      SAVE CL_UNITS,CONTRACTS,EL_UNITS
      INTEGER (kind=2) ::  BASE_DATE,HISTORICAL_PRODUCTION_DATE
      REAL (KIND=4) :: N_A/-999999./,AVERAGE_EMISSIONS_COST

         CL_UNITS = R_CL_UNITS
         CONTRACTS = R_CONTRACTS
         EL_UNITS = R_EL_UNITS
!         NUM_TRANS = GET_NUM_SCENARIO_TRANSACTIONS()
         IF(WABASH_REPORT_NOT_OPEN) THEN
            WABASH_REPORT_UNIT_NO = WABASH_VALLEY_POWER_COST_HEADER(
     +                                           WABASH_REPORT_UNIT_REC)
            WABASH_REPORT_NOT_OPEN = .FALSE.
            LAST_SEASON = get_PRODUCTION_PERIODS_in()
            DO I = 1, LAST_SEASON
               RPT_MONTH_NAME(I) = MONTH_NAME(I)
            ENDDO
            RPT_MONTH_NAME(LAST_SEASON+1) = 'Annual'
         ENDIF
         SAVE_PEAK_MONTH = PEAK_MONTH(globecom_year)
!
! 05/20/03.
!
!
         IF(CL_UNITS > 0) THEN
            IF(ALLOCATED(ANNUAL_CL_DISPATCHED_ENRG))
     +          DEALLOCATE(ANNUAL_CL_DISPATCHED_ENRG,
     +                     ANNUAL_CL_DISPATCHED_CAP,
     +                     ANNUAL_CL_LOSSES,
     +                     ANNUAL_CL_PURCHASED_ENRG,
     +                     ANNUAL_CL_PURCHASED_CAP,
     +                     ANNUAL_CL_FIXED_CHARGE,
     +                     ANNUAL_CL_FUEL_CHARGE,
     +                     ANNUAL_CL_VARIABLE_CHARGE,
     +                     ANNUAL_CL_EMISSION_COST,
     +                     ANNUAL_CL_MAX_DISPATCHED_CAP,
     +                     ANNUAL_CL_MAX_PURCHASED_CAP)
            ALLOCATE(ANNUAL_CL_DISPATCHED_ENRG(CL_UNITS))
            ALLOCATE(ANNUAL_CL_DISPATCHED_CAP(CL_UNITS))
            ALLOCATE(ANNUAL_CL_LOSSES(CL_UNITS))
            ALLOCATE(ANNUAL_CL_PURCHASED_ENRG(CL_UNITS))
            ALLOCATE(ANNUAL_CL_PURCHASED_CAP(CL_UNITS))
            ALLOCATE(ANNUAL_CL_FIXED_CHARGE(CL_UNITS))
            ALLOCATE(ANNUAL_CL_FUEL_CHARGE(CL_UNITS))
            ALLOCATE(ANNUAL_CL_VARIABLE_CHARGE(CL_UNITS))
            ALLOCATE(ANNUAL_CL_EMISSION_COST(CL_UNITS))
            ALLOCATE(ANNUAL_CL_MAX_DISPATCHED_CAP(CL_UNITS))
            ALLOCATE(ANNUAL_CL_MAX_PURCHASED_CAP(CL_UNITS))
            ANNUAL_CL_DISPATCHED_ENRG = 0.
            ANNUAL_CL_DISPATCHED_CAP = 0.
            ANNUAL_CL_LOSSES = 0.
            ANNUAL_CL_PURCHASED_ENRG = 0.
            ANNUAL_CL_PURCHASED_CAP = 0.
            ANNUAL_CL_FIXED_CHARGE = 0.
            ANNUAL_CL_FUEL_CHARGE = 0.
            ANNUAL_CL_VARIABLE_CHARGE = 0.
            ANNUAL_CL_EMISSION_COST = 0.
            ANNUAL_CL_MAX_DISPATCHED_CAP = 0.
            ANNUAL_CL_MAX_PURCHASED_CAP = 0.
         ENDIF
!
!
         ANNUAL_PURCHASE_POWER_COST = 0.
         ANNUAL_POWER_PURCHASED_MWH = 0.
         ANNUAL_COST_OF_POWER_SOLD = 0.
         ANNUAL_FUEL_COST_OF_POWER_SOLD = 0.
         ANNUAL_VOM_COST_OF_POWER_SOLD = 0.
         ANNUAL_MARKET_REVENUE = 0.
         ANNUAL_POWER_SOLD_MWH = 0.
!
         UPPER_TRANS_GROUP = GET_NUMBER_OF_ACTIVE_GROUPS()
!
         IF(CONTRACTS > 0) THEN
            IF(ALLOCATED(ANNUAL_CT_DISPATCHED_ENRG))
     +          DEALLOCATE(ANNUAL_CT_DISPATCHED_ENRG,
     +                     ANNUAL_CT_DISPATCHED_CAP,
     +                     ANNUAL_CT_LOSSES,
     +                     ANNUAL_CT_PURCHASED_ENRG,
     +                     ANNUAL_CT_PURCHASED_CAP,
     +                     ANNUAL_CT_FIXED_CHARGE,
     +                     ANNUAL_CT_FUEL_CHARGE,
     +                     ANNUAL_CT_VARIABLE_CHARGE,
     +                     ANNUAL_CT_MAX_DISPATCHED_CAP,
     +                     ANNUAL_CT_MAX_PURCHASED_CAP)
            ALLOCATE(ANNUAL_CT_DISPATCHED_ENRG(CONTRACTS))
            ALLOCATE(ANNUAL_CT_DISPATCHED_CAP(CONTRACTS))
            ALLOCATE(ANNUAL_CT_LOSSES(CONTRACTS))
            ALLOCATE(ANNUAL_CT_PURCHASED_ENRG(CONTRACTS))
            ALLOCATE(ANNUAL_CT_PURCHASED_CAP(CONTRACTS))
            ALLOCATE(ANNUAL_CT_FIXED_CHARGE(CONTRACTS))
            ALLOCATE(ANNUAL_CT_FUEL_CHARGE(CONTRACTS))
            ALLOCATE(ANNUAL_CT_VARIABLE_CHARGE(CONTRACTS))
            ALLOCATE(ANNUAL_CT_MAX_DISPATCHED_CAP(CONTRACTS))
            ALLOCATE(ANNUAL_CT_MAX_PURCHASED_CAP(CONTRACTS))
            ANNUAL_CT_DISPATCHED_ENRG = 0.
            ANNUAL_CT_DISPATCHED_CAP = 0.
            ANNUAL_CT_LOSSES = 0.
            ANNUAL_CT_PURCHASED_ENRG = 0.
            ANNUAL_CT_PURCHASED_CAP = 0.
            ANNUAL_CT_FIXED_CHARGE = 0.
            ANNUAL_CT_FUEL_CHARGE = 0.
            ANNUAL_CT_VARIABLE_CHARGE = 0.
            ANNUAL_CT_MAX_DISPATCHED_CAP = 0.
            ANNUAL_CT_MAX_PURCHASED_CAP = 0.
         ENDIF
         IF(EL_UNITS > 0) THEN
            IF(ALLOCATED(ANNUAL_EL_DISPATCHED_ENRG))
     +          DEALLOCATE(ANNUAL_EL_DISPATCHED_ENRG,
     +                     ANNUAL_EL_DISPATCHED_CAP,
     +                     ANNUAL_EL_LOSSES,
     +                     ANNUAL_EL_PURCHASED_ENRG,
     +                     ANNUAL_EL_PURCHASED_CAP,
     +                     ANNUAL_EL_FIXED_CHARGE,
     +                     ANNUAL_EL_FUEL_CHARGE,
     +                     ANNUAL_EL_VARIABLE_CHARGE,
     +                     ANNUAL_EL_MAX_DISPATCHED_CAP,
     +                     ANNUAL_EL_MAX_PURCHASED_CAP)
            ALLOCATE(ANNUAL_EL_DISPATCHED_ENRG(EL_UNITS))
            ALLOCATE(ANNUAL_EL_DISPATCHED_CAP(EL_UNITS))
            ALLOCATE(ANNUAL_EL_LOSSES(EL_UNITS))
            ALLOCATE(ANNUAL_EL_PURCHASED_ENRG(EL_UNITS))
            ALLOCATE(ANNUAL_EL_PURCHASED_CAP(EL_UNITS))
            ALLOCATE(ANNUAL_EL_FIXED_CHARGE(EL_UNITS))
            ALLOCATE(ANNUAL_EL_FUEL_CHARGE(EL_UNITS))
            ALLOCATE(ANNUAL_EL_VARIABLE_CHARGE(EL_UNITS))
            ALLOCATE(ANNUAL_EL_MAX_DISPATCHED_CAP(EL_UNITS))
            ALLOCATE(ANNUAL_EL_MAX_PURCHASED_CAP(EL_UNITS))
            ANNUAL_EL_DISPATCHED_ENRG = 0.
            ANNUAL_EL_DISPATCHED_CAP = 0.
            ANNUAL_EL_LOSSES = 0.
            ANNUAL_EL_PURCHASED_ENRG = 0.
            ANNUAL_EL_PURCHASED_CAP = 0.
            ANNUAL_EL_FIXED_CHARGE = 0.
            ANNUAL_EL_FUEL_CHARGE = 0.
            ANNUAL_EL_VARIABLE_CHARGE = 0.
            ANNUAL_EL_MAX_DISPATCHED_CAP = 0.
            ANNUAL_EL_MAX_PURCHASED_CAP = 0.
         ENDIF
      RETURN

      ENTRY WABASH_CL_POWER_COST_REPORT(ISEAS,YR,CL_ENERGY,
     +                                  SEAS_HOURS,
     +                                  BLOCK_FUEL_COST,
     +                                  LOSSES,
     +                                  UNIT_CAP,
     +                                  UNIT_CAP_AFTER_LOSSES,
     +                                  UNIT_MONTHLY_FIXED_COST,
     +                                  VARIABLE_OM_RATE,FUEL_COST_ADJ,
     +                                  UNIT_NAMES)

      PRINT_REPORT =  INDEX('M,B',WABASH_POWER_COST_RPT()) /= 0
      SUM_REPORT_ITEMS = 0.
      IF(ISEAS == 1) THEN
         ANN_SUM_REPORT_ITEMS = 0.
      ENDIF
      BASE_DATE = (get_BASE_YEAR() + YR - 1900) * 100
      DO I = 1, CL_UNITS
         UNIT_NAME = UNIT_NAMES(I)

         IF(I == GIBSON_BACKUP_POINTER)
     +                            UNIT_CAP(I) = UNIT_CAP(GIBSON_POINTER)
     
         ENRG = SEAS_HOURS * (CL_ENERGY(1,I) + CL_ENERGY(2,I))
      call write_energy_trace("wabash_power_cost_report", 
     +   "reaenrg:0001", ENRG)
         LOSSES(I) = SEAS_HOURS * LOSSES(I)
         IF(ENRG /= 0.) THEN
            LOSS_RATE = 100.*LOSSES(I)/ENRG
            FUEL_COST = BLOCK_FUEL_COST(1,I) + BLOCK_FUEL_COST(2,I) +
     +                                           ENRG * FUEL_COST_ADJ(I)
            FUEL_CHARGE_RATE = FUEL_COST/ENRG
         ELSE
            LOSS_RATE = 0.
            FUEL_CHARGE_RATE = 0.
            FUEL_COST = 0.
         ENDIF
         DISPATCHED_ENERGY = ENRG - LOSSES(I)
         IF(UNIT_CAP_AFTER_LOSSES(I) /= 0.) THEN
            CAPACITY_FACTOR = ! AS A PERCENTAGE
     +             DISPATCHED_ENERGY*100./
     +                           (SEAS_HOURS * UNIT_CAP_AFTER_LOSSES(I))
         ELSE
            CAPACITY_FACTOR = 0.
         ENDIF
         IF(UNIT_CAP(I) /= 0.) THEN
            POWER_LOSSES=100.*(UNIT_CAP(I) - UNIT_CAP_AFTER_LOSSES(I))/
     +                                                       UNIT_CAP(I)
            FIXED_CHARGE_RATE = UNIT_MONTHLY_FIXED_COST(I)/
     +                                               (1000.*UNIT_CAP(I))
         ELSE
            POWER_LOSSES = 0.
            FIXED_CHARGE_RATE = 0.
         ENDIF
!
!
! 040905. ADDED FOR MCKEE.
! CALLED MONTHLY BY UNIT
!
         CALL CL_EMIS_TO_WVPA_REPORT(I,MONTHLY_EMISSIONS_COST)
! CONVERT FROM $MM TO $M
         MONTHLY_EMISSIONS_COST = MONTHLY_EMISSIONS_COST*1000.


         VARIABLE_OM_CHARGE = ENRG * VARIABLE_OM_RATE(I)

         TEMP_VOM_RATE = VARIABLE_OM_RATE(I)
         IF(MONTHLY_EMISSIONS_COST /= 0. .AND. ENRG > .01) THEN

            AVERAGE_EMISSIONS_COST = MONTHLY_EMISSIONS_COST/ENRG
         ELSE
            AVERAGE_EMISSIONS_COST = 0. !N_A
         ENDIF
         TOTAL_ALL_COSTS = UNIT_MONTHLY_FIXED_COST(I)
     +                     + FUEL_COST
     +                     + VARIABLE_OM_CHARGE
     +                     + MONTHLY_EMISSIONS_COST

         IF(DISPATCHED_ENERGY >  0.) THEN
            AVERAGE_TOTAL_COST = TOTAL_ALL_COSTS/DISPATCHED_ENERGY
            AVERAGE_VARIABLE_COST = (FUEL_COST
     +                               + VARIABLE_OM_CHARGE
     +                               + MONTHLY_EMISSIONS_COST)/
     +                                                 DISPATCHED_ENERGY
         ELSE
            AVERAGE_TOTAL_COST = 0.
            AVERAGE_VARIABLE_COST = 0.
         ENDIF

         IF(I /= GIBSON_BACKUP_POINTER) THEN
            ANNUAL_CL_DISPATCHED_CAP(I) = ANNUAL_CL_DISPATCHED_CAP(I) +
     +                                          UNIT_CAP_AFTER_LOSSES(I)
            IF(ISEAS == SAVE_PEAK_MONTH) THEN
               ANNUAL_CL_MAX_DISPATCHED_CAP(I)=UNIT_CAP_AFTER_LOSSES(I)
               ANNUAL_CL_MAX_PURCHASED_CAP(I) = UNIT_CAP(I)
            ENDIF
            ANNUAL_CL_PURCHASED_CAP(I) = ANNUAL_CL_PURCHASED_CAP(I) +
     +                                                       UNIT_CAP(I)
            ANNUAL_CL_DISPATCHED_ENRG(I) = ANNUAL_CL_DISPATCHED_ENRG(I)+
     +                                                 DISPATCHED_ENERGY
            SUM_REPORT_ITEMS(1) = SUM_REPORT_ITEMS(1) +
     +                                 1000.*UNIT_CAP_AFTER_LOSSES(I)
            SUM_REPORT_ITEMS(4) = SUM_REPORT_ITEMS(4) +1000.*UNIT_CAP(I)
            SUM_REPORT_ITEMS(13) = SUM_REPORT_ITEMS(13) +
     +                                    1000.*UNIT_CAP_AFTER_LOSSES(I)
            SUM_REPORT_ITEMS(14) =SUM_REPORT_ITEMS(14)+1000.*UNIT_CAP(I)

         ENDIF
         ANNUAL_CL_LOSSES(I) = ANNUAL_CL_LOSSES(I) + LOSSES(I)
         ANNUAL_CL_PURCHASED_ENRG(I) = ANNUAL_CL_PURCHASED_ENRG(I)+ENRG
         ANNUAL_CL_FIXED_CHARGE(I) = ANNUAL_CL_FIXED_CHARGE(I) +
     +                                        UNIT_MONTHLY_FIXED_COST(I)
         ANNUAL_CL_FUEL_CHARGE(I) = ANNUAL_CL_FUEL_CHARGE(I) + FUEL_COST
         ANNUAL_CL_VARIABLE_CHARGE(I) = ANNUAL_CL_VARIABLE_CHARGE(I)
     +                                  + VARIABLE_OM_CHARGE
         ANNUAL_CL_EMISSION_COST(I) = ANNUAL_CL_EMISSION_COST(I)
     +                                + MONTHLY_EMISSIONS_COST

         SUM_REPORT_ITEMS(2) = SUM_REPORT_ITEMS(2) + DISPATCHED_ENERGY
         SUM_REPORT_ITEMS(5) = SUM_REPORT_ITEMS(5) + ENRG
         SUM_REPORT_ITEMS(7) = SUM_REPORT_ITEMS(7) +
     +                                  UNIT_MONTHLY_FIXED_COST(I)/1000.
         SUM_REPORT_ITEMS(9) = SUM_REPORT_ITEMS(9) + FUEL_COST/1000.
         SUM_REPORT_ITEMS(11) = SUM_REPORT_ITEMS(11) +
     +                                          VARIABLE_OM_CHARGE/1000.
         SUM_REPORT_ITEMS(12) = SUM_REPORT_ITEMS(12) +
     +                                             TOTAL_ALL_COSTS/1000.
         SUM_REPORT_ITEMS(19) = SUM_REPORT_ITEMS(19)
     +                          + MONTHLY_EMISSIONS_COST/1000.
         IF(EXPENSE_ASSIGNMENT_IS_PURCHASE(I)) THEN
            TYPE = 1
         ELSE
            TYPE = 2
         ENDIF
!
         TRACKER = GET_THERMAL_TRACKER_INDEX(I)
         RES_TRACKER = FLOAT(GET_THERMAL_RES_TRACKER_INDEX(I))
         FUEL_TRACKER = FLOAT(GET_THERMAL_FUEL_TRACKER_INDEX(I))
         MEM_TRACKER = FLOAT(GET_THERMAL_MEM_TRACKER_INDEX(I))

         RATE_TRACKER = TRACKER 

!
         IF(BASE_DATE + ISEAS >= HISTORICAL_PRODUCTION_DATE())
     +      CALL WVPA_STORE_CL_TRACKER_DATE_BASE(ISEAS,
     +                                       TYPE,
     +                                       TRACKER,
     +                                       FUEL_COST,
     +                                       VARIABLE_OM_CHARGE,
     +                                       UNIT_MONTHLY_FIXED_COST(I),
     +                                       INT2(MEM_TRACKER))
!
!
         IF(PRINT_REPORT) THEN
            INQUIRE(UNIT=WABASH_REPORT_UNIT_NO,
     +                                   NEXTREC=WABASH_REPORT_UNIT_REC)
            WRITE(WABASH_REPORT_UNIT_NO,REC=WABASH_REPORT_UNIT_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(YR+get_BASE_YEAR()),
     +               RPT_MONTH_NAME(ISEAS),
     +               UNIT_NAME,
     +               1000.*UNIT_CAP_AFTER_LOSSES(I),
     +               DISPATCHED_ENERGY,
     +               LOSS_RATE,
     +               1000.*UNIT_CAP(I),
     +               ENRG,
     +               FIXED_CHARGE_RATE,
     +               UNIT_MONTHLY_FIXED_COST(I)/1000.,
     +               FUEL_CHARGE_RATE,
     +               FUEL_COST/1000.,
     +               TEMP_VOM_RATE,
     +               VARIABLE_OM_CHARGE/1000.,
     +               TOTAL_ALL_COSTS/1000.,
     +               1000.*UNIT_CAP_AFTER_LOSSES(I),
     +               1000.*UNIT_CAP(I),
     +               POWER_LOSSES,
     +               CAPACITY_FACTOR,
     +               AVERAGE_TOTAL_COST,
     +               AVERAGE_VARIABLE_COST,
     +               RES_TRACKER,
     +               FUEL_TRACKER,
     +               RATE_TRACKER,
     +               MEM_TRACKER,
     +               MONTHLY_EMISSIONS_COST/1000.,
     +               AVERAGE_EMISSIONS_COST
!            WABASH_REPORT_UNIT_REC = WABASH_REPORT_UNIT_REC + 1
         ENDIF
      ENDDO
!
      IF(YES_RUN_TRANSACT()) THEN
         CALL CL_REPORT_TO_WVPA_REPORT(ISEAS,
     +                               PURCHASE_POWER_COST,
     +                               POWER_PURCHASED_MWH,
     +                               COST_OF_POWER_SOLD,
     +                               FUEL_COST_OF_POWER_SOLD,
     +                               VOM_COST_OF_POWER_SOLD,
     +                               MARKET_REVENUE,
     +                               POWER_SOLD_MWH)
!         TRANS_GROUP = 1
         UNSERVED_ENERGY = 0.
         UNSERVED_COST = 0.
         DO TRANS_GROUP = 1, UPPER_TRANS_GROUP
            CALL GET_ANNUAL_UNSERVED_COST(
     +                               TRANS_GROUP,
     +                               TEMP_R2, !UNSERVED_COST,
     +                               TEMP_R1, !UNSERVED_ENERGY,
     +                               ISEAS)
            UNSERVED_ENERGY = UNSERVED_ENERGY + TEMP_R1
            UNSERVED_COST = UNSERVED_COST + TEMP_R2
         ENDDO
!
         PURCHASE_POWER_COST = PURCHASE_POWER_COST + UNSERVED_COST/1000.
         POWER_PURCHASED_MWH = POWER_PURCHASED_MWH + UNSERVED_ENERGY

         SUM_REPORT_ITEMS(2) = SUM_REPORT_ITEMS(2) +
     +                              POWER_PURCHASED_MWH - POWER_SOLD_MWH
         SUM_REPORT_ITEMS(5) = SUM_REPORT_ITEMS(5) +
     +                              POWER_PURCHASED_MWH - POWER_SOLD_MWH
! FUEL COST
!
         SUM_REPORT_ITEMS(9) = SUM_REPORT_ITEMS(9) +
     +                        (PURCHASE_POWER_COST-MARKET_REVENUE)

! TOTAL COST
!
         SUM_REPORT_ITEMS(12) = SUM_REPORT_ITEMS(12) +
     +                        (PURCHASE_POWER_COST-MARKET_REVENUE)

! FUEL COST

         ANN_SUM_REPORT_ITEMS(9) = ANN_SUM_REPORT_ITEMS(9) +
     +                        (PURCHASE_POWER_COST-MARKET_REVENUE)

! TOTAL COST
         ANN_SUM_REPORT_ITEMS(12) = ANN_SUM_REPORT_ITEMS(12) +
     +                        (PURCHASE_POWER_COST-MARKET_REVENUE)

         ANNUAL_PURCHASE_POWER_COST = ANNUAL_PURCHASE_POWER_COST +
     +                                               PURCHASE_POWER_COST
         ANNUAL_POWER_PURCHASED_MWH = ANNUAL_POWER_PURCHASED_MWH +
     +                                               POWER_PURCHASED_MWH
         ANNUAL_COST_OF_POWER_SOLD = ANNUAL_COST_OF_POWER_SOLD +
     +                                                COST_OF_POWER_SOLD
!
         FUEL_COST_OF_POWER_SOLD = FUEL_COST_OF_POWER_SOLD * 1000000.
         VOM_COST_OF_POWER_SOLD = VOM_COST_OF_POWER_SOLD * 1000000.
!
         ANNUAL_FUEL_COST_OF_POWER_SOLD =
     +                  ANNUAL_FUEL_COST_OF_POWER_SOLD +
     +                                           FUEL_COST_OF_POWER_SOLD
         ANNUAL_VOM_COST_OF_POWER_SOLD =
     +                  ANNUAL_VOM_COST_OF_POWER_SOLD +
     +                                           VOM_COST_OF_POWER_SOLD
         ANNUAL_MARKET_REVENUE = ANNUAL_MARKET_REVENUE +
     +                                                   MARKET_REVENUE
         ANNUAL_POWER_SOLD_MWH = ANNUAL_POWER_SOLD_MWH +
     +                                                   POWER_SOLD_MWH
!
! 12/28/00. GAT. CUSTOM CODING FOR WVPA.
!
         TYPE = 1
         MEM_TRACKER = 1.
         RATE_TRACKER = 0 ! FLOAT(GET_THERMAL_RATE_TRACKER_INDEX(I))
!
         TEMP_R = 0.
         TOTAL_ALL_COSTS =
     +                   1000.*(PURCHASE_POWER_COST-COST_OF_POWER_SOLD)
!     +                                    COST_OF_POWER_SOLD -
!     +                                             MARKET_REVENUE)
!
         IF(BASE_DATE + ISEAS >= HISTORICAL_PRODUCTION_DATE())
     +      CALL WVPA_STORE_CL_TRACKER_DATE_BASE(ISEAS,
     +                                       TYPE,
     +                                       TRACKER,
     +                                       TOTAL_ALL_COSTS,
     +                                       TEMP_R,
     +                                       TEMP_R,
     +                                       INT2(MEM_TRACKER))
         IF(PRINT_REPORT) THEN

            IF(POWER_PURCHASED_MWH > 0.) THEN
               AVERAGE_TOTAL_COST = PURCHASE_POWER_COST * 1000. /
     +                                               POWER_PURCHASED_MWH
               AVERAGE_VARIABLE_COST = AVERAGE_TOTAL_COST
            ELSE
               AVERAGE_TOTAL_COST = 0.
               AVERAGE_VARIABLE_COST = 0.
            ENDIF
            INQUIRE(UNIT=WABASH_REPORT_UNIT_NO,
     +                                   NEXTREC=WABASH_REPORT_UNIT_REC)
            WRITE(WABASH_REPORT_UNIT_NO,REC=WABASH_REPORT_UNIT_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(YR+get_BASE_YEAR()),
     +               RPT_MONTH_NAME(ISEAS),
     +               'Market Purchases    ',
     +               0.,
     +               POWER_PURCHASED_MWH,
     +               0.,
     +               0.,
     +               POWER_PURCHASED_MWH,
     +               0.,
     +               0.,
     +               AVERAGE_TOTAL_COST,
     +               PURCHASE_POWER_COST,
     +               0.,
     +               0.,
     +               PURCHASE_POWER_COST,
     +               0.,
     +               0.,
     +               0.,
     +               0.,
     +               AVERAGE_TOTAL_COST,
     +               0.,
     +               0., ! RES_TRACKER,
     +               0., ! FUEL_TRACKER
     +               0., ! RATE_TRACKER
     +               MEM_TRACKER, ! MEM_TRACKER
     +               0.,  ! EMISSION AMOUNT
     +               0.   ! AVERAGE EMISSION RATE
!
            TYPE = 1
            MEM_TRACKER = 2.
            FUEL_TRACKER = 2.
            IF(BASE_DATE + ISEAS >= HISTORICAL_PRODUCTION_DATE())
     +         CALL WVPA_STORE_CL_TRACKER_DATE_BASE(ISEAS,
     +                                       TYPE,
     +                                       TRACKER,
     +                                       FUEL_COST_OF_POWER_SOLD,
     +                                       VOM_COST_OF_POWER_SOLD,
     +                                       TEMP_R,
     +                                       INT2(MEM_TRACKER))
!
            IF(POWER_SOLD_MWH > 0.) THEN
               AVERAGE_TOTAL_COST = COST_OF_POWER_SOLD * 1000. /
     +                                               POWER_SOLD_MWH
               AVERAGE_VARIABLE_COST = AVERAGE_TOTAL_COST
            ELSE
               AVERAGE_TOTAL_COST = 0.
               AVERAGE_VARIABLE_COST = 0.
            ENDIF
            INQUIRE(UNIT=WABASH_REPORT_UNIT_NO,
     +                                   NEXTREC=WABASH_REPORT_UNIT_REC)
            WRITE(WABASH_REPORT_UNIT_NO,REC=WABASH_REPORT_UNIT_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(YR+get_BASE_YEAR()),
     +               RPT_MONTH_NAME(ISEAS),
     +               'Cost of Market Sales',
     +               0.,
     +               POWER_SOLD_MWH,
     +               0.,
     +               0.,
     +               POWER_SOLD_MWH,
     +               0.,
     +               0.,
     +               AVERAGE_TOTAL_COST,
     +               FUEL_COST_OF_POWER_SOLD*.001,
     +               0.,
     +               VOM_COST_OF_POWER_SOLD*.001,
     +               COST_OF_POWER_SOLD,
     +               0.,
     +               0.,
     +               0.,
     +               0.,
     +               AVERAGE_TOTAL_COST,
     +               0.,
     +               0., ! RES_TRACKER,
     +               FUEL_TRACKER, ! FUEL_TRACKER
     +               0., ! RATE_TRACKER
     +               MEM_TRACKER,  ! MEM_TRACKER
     +               0.,  ! EMISSION AMOUNT
     +               0.   ! AVERAGE EMISSION RATE
!
!
            IF(POWER_SOLD_MWH > 0.) THEN
               AVERAGE_TOTAL_COST = MARKET_REVENUE * 1000. /
     +                                               POWER_SOLD_MWH
               AVERAGE_VARIABLE_COST = AVERAGE_TOTAL_COST
            ELSE
               AVERAGE_TOTAL_COST = 0.
               AVERAGE_VARIABLE_COST = 0.
            ENDIF
            INQUIRE(UNIT=WABASH_REPORT_UNIT_NO,
     +                                   NEXTREC=WABASH_REPORT_UNIT_REC)
            WRITE(WABASH_REPORT_UNIT_NO,REC=WABASH_REPORT_UNIT_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(YR+get_BASE_YEAR()),
     +               RPT_MONTH_NAME(ISEAS),
     +               'Market Revenue      ',
     +               0.,
     +               -1.*POWER_SOLD_MWH,
     +               0.,
     +               0.,
     +               -1.*POWER_SOLD_MWH,
     +               0.,
     +               0.,
     +               AVERAGE_TOTAL_COST,
     +               -1.*MARKET_REVENUE,
     +               0.,
     +               0.,
     +               -1.*MARKET_REVENUE,
     +               0.,
     +               0.,
     +               0.,
     +               0.,
     +               -1.*AVERAGE_TOTAL_COST,
     +               0.,
     +               0., ! RES_TRACKER,
     +               0., ! FUEL_TRACKER
     +               0., ! RATE_TRACKER
     +               MEM_TRACKER,  ! MEM_TRACKER
     +               0.,  ! EMISSION AMOUNT
     +               0.   ! AVERAGE EMISSION RATE
!
!
            IF(SUM_REPORT_ITEMS(2) /= 0.) THEN
               SUM_REPORT_ITEMS(3) =
     +               100*(1. - SUM_REPORT_ITEMS(2) /
     +                                          SUM_REPORT_ITEMS(5))
               SUM_REPORT_ITEMS(8) = 1000.*
     +               SUM_REPORT_ITEMS(9)/SUM_REPORT_ITEMS(5)
               SUM_REPORT_ITEMS(10) = 1000.*
     +                 SUM_REPORT_ITEMS(11)/SUM_REPORT_ITEMS(5)
               SUM_REPORT_ITEMS(15) =
     +               100*(1. - SUM_REPORT_ITEMS(1) /
     +                                          SUM_REPORT_ITEMS(4))
               SUM_REPORT_ITEMS(17) =
     +               1000.*SUM_REPORT_ITEMS(12)/SUM_REPORT_ITEMS(2)
               SUM_REPORT_ITEMS(18) =
     +               1000.*(SUM_REPORT_ITEMS(9)+SUM_REPORT_ITEMS(11))/
     +                                               SUM_REPORT_ITEMS(2)
               SUM_REPORT_ITEMS(20) =
     +                    1000.*SUM_REPORT_ITEMS(19)/SUM_REPORT_ITEMS(2)
            ELSE
               SUM_REPORT_ITEMS(3) = 0.
               SUM_REPORT_ITEMS(8) = 0.
               SUM_REPORT_ITEMS(10) = 0.
               SUM_REPORT_ITEMS(15) = 0.
               SUM_REPORT_ITEMS(17) = 0.
               SUM_REPORT_ITEMS(18) = 0.
               SUM_REPORT_ITEMS(20) = 0.
            ENDIF
            IF(SUM_REPORT_ITEMS(1) /= 0.) THEN
               SUM_REPORT_ITEMS(6) = 1000.*
     +               SUM_REPORT_ITEMS(7)/SUM_REPORT_ITEMS(1)
               SUM_REPORT_ITEMS(16) = 100000.*SUM_REPORT_ITEMS(2)/
     +                              (SEAS_HOURS*SUM_REPORT_ITEMS(1))
            ELSE
               SUM_REPORT_ITEMS(6) = 0.
               SUM_REPORT_ITEMS(16) = 0.
            ENDIF
         ENDIF
      ENDIF
!

      RETURN

      ENTRY WABASH_CT_POWER_COST_REPORT(ISEAS,YR,
     +                                  CT_ENERGY,
     +                                  SEAS_HOURS,
     +                                  CT_CAPACITY,
     +                                  MONTHLY_ECONOMY_BOUGHT,
     +                                  MONTHLY_ECONOMY_COST,
     +                                  MONTHLY_ECONOMY_SOLD,
     +                                  MONTHLY_ECONOMY_REVENUE,
     +                                  LOSSES)

      PRINT_REPORT =  INDEX('M,B',WABASH_POWER_COST_RPT()) /= 0
      DO I = 1, CONTRACTS
         UNIT_NAME = CNTRNM(I)
         IF(CNTR_ENERGY_SWITCH(I) == 'O') THEN
            SALES_MULT = -1.
         ELSE
            SALES_MULT = 1.
         ENDIF
!
         CT_LOSSES = LOSSES(I) * SALES_MULT * SEAS_HOURS
! GROSSING UP DISPATCHED TO GENERATED
         DISPATCHED_ENERGY = SALES_MULT * SEAS_HOURS * CT_ENERGY(I)
!
         IF(DISPATCHED_ENERGY > 0.) THEN
            LOSS_RATE = 100. * CT_LOSSES/DISPATCHED_ENERGY
            
            ! GROSS UP TO TOTAL ENERGY
            ENRG = DISPATCHED_ENERGY / (1. - LOSS_RATE/100.) 
      call write_energy_trace("wabash_power_cost_report", 
     +   "reaenrg:0002", ENRG)
            CT_LOSSES = ENRG * LOSS_RATE/100.
         ELSE
            LOSS_RATE = 0.
      call write_energy_trace("wabash_power_cost_report", 
     +   "reaenrg:0003", ENRG)
            ENRG = 0.
         ENDIF
!
         UNIT_CAPACITY = SALES_MULT * CT_CAPACITY(I)
         UNIT_CAPACITY_AFTER_LOSSES = CT_CAPACITY(I)
         FUEL_CHARGE_RATE = CT_MONTHLY_1ST_ENERGY_PRICE(I) +
     +                                           CT_ENERGY_COST_ADDER(I)
         FUEL_COST = ENRG * FUEL_CHARGE_RATE/1000.
!
         MONTHLY_FIXED_COST = UNIT_CAPACITY * CONTRACT_FIXED_COST(I) +
     +                          SALES_MULT * CT_ANNUAL_FIXED_COST(I)/12.
!         DISPATCHED_ENERGY = ENRG - CT_LOSSES
         IF(UNIT_CAPACITY /= 0.) THEN
            CAPACITY_FACTOR = ! AS A PERCENTAGE
     +            DISPATCHED_ENERGY*100./
     +                         (SEAS_HOURS * UNIT_CAPACITY_AFTER_LOSSES)
            FIXED_CHARGE_RATE = MONTHLY_FIXED_COST/(UNIT_CAPACITY)
            POWER_LOSSES = 100. * (UNIT_CAPACITY -
     +                         UNIT_CAPACITY_AFTER_LOSSES)/UNIT_CAPACITY
         ELSE
            CAPACITY_FACTOR = 0.
            FIXED_CHARGE_RATE = 0.
            POWER_LOSSES = 0.
         ENDIF
         VARIABLE_OM_CHARGE = ENRG * CT_SECOND_ENERGY_PRICE(I) / 1000.
         TOTAL_ALL_COSTS = MONTHLY_FIXED_COST + FUEL_COST +
     +                                              VARIABLE_OM_CHARGE
!
         IF(DISPATCHED_ENERGY /=  0.) THEN
            AVERAGE_TOTAL_COST = 1000.*TOTAL_ALL_COSTS/DISPATCHED_ENERGY
            AVERAGE_VARIABLE_COST = 1000.*(VARIABLE_OM_CHARGE+FUEL_COST)
     +                                                /DISPATCHED_ENERGY
         ELSE
            AVERAGE_TOTAL_COST = 0.
            AVERAGE_VARIABLE_COST = 0.
         ENDIF

         ANNUAL_CT_DISPATCHED_ENRG(I) = ANNUAL_CT_DISPATCHED_ENRG(I) +
     +                                                 DISPATCHED_ENERGY
         ANNUAL_CT_DISPATCHED_CAP(I) = ANNUAL_CT_DISPATCHED_CAP(I) +
     +                                        UNIT_CAPACITY_AFTER_LOSSES
         ANNUAL_CT_LOSSES(I) = ANNUAL_CT_LOSSES(I) + CT_LOSSES
         ANNUAL_CT_PURCHASED_ENRG(I) = ANNUAL_CT_PURCHASED_ENRG(I)+ENRG
         ANNUAL_CT_PURCHASED_CAP(I) = ANNUAL_CT_PURCHASED_CAP(I) +
     +                                                     UNIT_CAPACITY
         ANNUAL_CT_FIXED_CHARGE(I) = ANNUAL_CT_FIXED_CHARGE(I) +
     +                                                MONTHLY_FIXED_COST
         ANNUAL_CT_FUEL_CHARGE(I) = ANNUAL_CT_FUEL_CHARGE(I) + FUEL_COST
         ANNUAL_CT_VARIABLE_CHARGE(I) = ANNUAL_CT_VARIABLE_CHARGE(I) +
     +                                                VARIABLE_OM_CHARGE
         IF(ISEAS == SAVE_PEAK_MONTH) THEN
            ANNUAL_CT_MAX_DISPATCHED_CAP(I)= UNIT_CAPACITY_AFTER_LOSSES
            ANNUAL_CT_MAX_PURCHASED_CAP(I) = UNIT_CAPACITY
         ENDIF
!
         SUM_REPORT_ITEMS(1) = SUM_REPORT_ITEMS(1) +
     +                                  1000.*UNIT_CAPACITY_AFTER_LOSSES
         SUM_REPORT_ITEMS(2) = SUM_REPORT_ITEMS(2) + DISPATCHED_ENERGY
         SUM_REPORT_ITEMS(4) = SUM_REPORT_ITEMS(4) + 1000.*UNIT_CAPACITY
         SUM_REPORT_ITEMS(5) = SUM_REPORT_ITEMS(5) + ENRG
         SUM_REPORT_ITEMS(7) = SUM_REPORT_ITEMS(7) + MONTHLY_FIXED_COST
         SUM_REPORT_ITEMS(9) = SUM_REPORT_ITEMS(9) + FUEL_COST
         SUM_REPORT_ITEMS(11) = SUM_REPORT_ITEMS(11) +
     +                                                VARIABLE_OM_CHARGE
         SUM_REPORT_ITEMS(12) = SUM_REPORT_ITEMS(12) + TOTAL_ALL_COSTS
         SUM_REPORT_ITEMS(13) = SUM_REPORT_ITEMS(13) +
     +                                  1000.*UNIT_CAPACITY_AFTER_LOSSES
         SUM_REPORT_ITEMS(14) = SUM_REPORT_ITEMS(14) +
     +                                               1000.*UNIT_CAPACITY

         IF(PRINT_REPORT) THEN
            INQUIRE(UNIT=WABASH_REPORT_UNIT_NO,
     +                                   NEXTREC=WABASH_REPORT_UNIT_REC)
            WRITE(WABASH_REPORT_UNIT_NO,REC=WABASH_REPORT_UNIT_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(YR+get_BASE_YEAR()),
     +               RPT_MONTH_NAME(ISEAS),
     +               UNIT_NAME,
     +               1000.*UNIT_CAPACITY_AFTER_LOSSES,
     +               DISPATCHED_ENERGY,
     +               LOSS_RATE,
     +               1000.*UNIT_CAPACITY,
     +               ENRG,
     +               FIXED_CHARGE_RATE,
     +               MONTHLY_FIXED_COST,
     +               FUEL_CHARGE_RATE,
     +               FUEL_COST,
     +               CT_SECOND_ENERGY_PRICE(I),
     +               VARIABLE_OM_CHARGE,
     +               TOTAL_ALL_COSTS,
     +               1000.*UNIT_CAPACITY_AFTER_LOSSES,
     +               1000.*UNIT_CAPACITY,
     +               POWER_LOSSES,
     +               CAPACITY_FACTOR,
     +               AVERAGE_TOTAL_COST,
     +               AVERAGE_VARIABLE_COST,
     +               RES_TRACKER,
     +               FUEL_TRACKER,
     +               RATE_TRACKER,
     +               MEM_TRACKER,
     +               0.,  ! EMISSION AMOUNT
     +               0.   ! AVERAGE EMISSION RATE
!            WABASH_REPORT_UNIT_REC = WABASH_REPORT_UNIT_REC + 1
         ENDIF
      ENDDO

      IF(PRINT_REPORT)THEN
            IF(SUM_REPORT_ITEMS(2) /= 0.) THEN
               SUM_REPORT_ITEMS(3) =
     +               100*(1. - SUM_REPORT_ITEMS(2) /
     +                                          SUM_REPORT_ITEMS(5))
               SUM_REPORT_ITEMS(8) = 1000.*
     +               SUM_REPORT_ITEMS(9)/SUM_REPORT_ITEMS(5)
               SUM_REPORT_ITEMS(10) = 1000.*
     +                 SUM_REPORT_ITEMS(11)/SUM_REPORT_ITEMS(5)
               SUM_REPORT_ITEMS(15) =
     +               100*(1. - SUM_REPORT_ITEMS(1) /
     +                                          SUM_REPORT_ITEMS(4))
               SUM_REPORT_ITEMS(17) =
     +               1000.*SUM_REPORT_ITEMS(12)/SUM_REPORT_ITEMS(2)
               SUM_REPORT_ITEMS(18) =
     +               1000.*(SUM_REPORT_ITEMS(9)+SUM_REPORT_ITEMS(11))/
     +                                               SUM_REPORT_ITEMS(2)
            ELSE
               SUM_REPORT_ITEMS(3) = 0.
               SUM_REPORT_ITEMS(8) = 0.
               SUM_REPORT_ITEMS(10) = 0.
               SUM_REPORT_ITEMS(15) = 0.
               SUM_REPORT_ITEMS(17) = 0.
               SUM_REPORT_ITEMS(18) = 0.
            ENDIF
            IF(SUM_REPORT_ITEMS(1) /= 0.) THEN
               SUM_REPORT_ITEMS(6) = 1000.*
     +               SUM_REPORT_ITEMS(7)/SUM_REPORT_ITEMS(1)
               SUM_REPORT_ITEMS(16) = 100000.*SUM_REPORT_ITEMS(2)/
     +                              (SEAS_HOURS*SUM_REPORT_ITEMS(1))
            ELSE
               SUM_REPORT_ITEMS(6) = 0.
               SUM_REPORT_ITEMS(16) = 0.
            ENDIF

      ENDIF
      RETURN

!
! FOR MARKET DERIVATIVES. 2/2/1.
!
      ENTRY WABASH_MD_POWER_COST_REPORT(ISEAS,YR,
     +                                  SEAS_HOURS)


      PRINT_REPORT =  INDEX('M,B',WABASH_POWER_COST_RPT()) /= 0
!      NUM_MONTHLY_TRANSACTIONS = GET_NUM_MONTHLY_TRANSACTIONS()
      NUM_TRANSACTIONS = GET_NUM_SCENARIO_TRANSACTIONS()
      IF(NUM_TRANSACTIONS <= 0) THEN
!      IF(NUM_MONTHLY_TRANSACTIONS <= 0) THEN
!
! 11/25/03. MOVED BELOW DERIVATIVES FOR WVPA
!
         IF(PRINT_REPORT) THEN
            INQUIRE(UNIT=WABASH_REPORT_UNIT_NO,
     +                                   NEXTREC=WABASH_REPORT_UNIT_REC)
            WRITE(WABASH_REPORT_UNIT_NO,REC=WABASH_REPORT_UNIT_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(YR+get_BASE_YEAR()),
     +               RPT_MONTH_NAME(ISEAS),
     +               'Total Resources     ',
     +               (SUM_REPORT_ITEMS(I),I=1,18),
     +               0., ! RES_TRACKER,
     +               0., ! FUEL_TRACKER
     +               0., ! RATE_TRACKER
     +               0.,  ! MEM_TRACKER
     +               SUM_REPORT_ITEMS(19), !TOTAL MONTHLY EMISSIONS
     +               SUM_REPORT_ITEMS(20)  !AVERAGE MONTHLY EMISSIONS
         ENDIF
!
         RETURN
      ENDIF
!
      MD_SUM_REPORT_ITEMS = 0.
!
      LOCAL_YEAR = YR + get_BASE_YEAR()
      TEMP_YEAR = FLOAT(LOCAL_YEAR)
!
      I = 0

      DO TRANS = 1, NUM_TRANSACTIONS ! NUM_MONTHLY_TRANSACTIONS
         I = I + 1
!
         VOID_LOGICAL =
     +         GET_MONTHLY_TRANS_VARIABLES(
     +                  TRANS,
     +                  ENRG,
     +                  TRANS_CAP,
     +                  TRANS_VAR_EXP,
     +                  TRANS_VAR_MWH,
     +                  TRANS_FIX_EXP,
     +                  TRANS_REV,
     +                  TRANS_REV_MWH,
     +                  TRANS_HOURS,
     +                  PRODUCT_HOURS,
     +                  TRANS_STRIKES,
     +                  ISEAS,
     +                  UNIT_NAME,
     +                  SUM_ANNUAL,
     +                  YES_REPORT_PRODUCT,
     +                  TG,
     +                  PM,
     +                  FT,
     +                  TRANS_ON_LINE,
     +                  TRANS_OFF_LINE,
     +                  TRANS_UNIT_ID,
     +                  TRANS_ASSET_CLASS,
     +                  TRANS_NOT_ACTIVE,
     +                  TEMP_YEAR)
         IF(TRANS_NOT_ACTIVE) CYCLE

! GROSSING UP DISPATCHED TO GENERATED
         DISPATCHED_ENERGY = ENRG

            FUEL_CHARGE_RATE = TRANS_VAR_MWH

            LOSS_RATE = 0.

         UNIT_CAPACITY = TRANS_CAP
         UNIT_CAPACITY_AFTER_LOSSES = TRANS_CAP

         MONTHLY_FIXED_COST = TRANS_FIX_EXP/1000.
         IF(UNIT_CAPACITY /= 0.) THEN
            CAPACITY_FACTOR = ! AS A PERCENTAGE
     +            ABS( DISPATCHED_ENERGY*100./
     +                    (SEAS_HOURS * UNIT_CAPACITY_AFTER_LOSSES) )
            FIXED_CHARGE_RATE = MONTHLY_FIXED_COST/(UNIT_CAPACITY)
            POWER_LOSSES = 100. * (UNIT_CAPACITY -
     +                         UNIT_CAPACITY_AFTER_LOSSES)/UNIT_CAPACITY
         ELSE
            CAPACITY_FACTOR = 0.
            FIXED_CHARGE_RATE = 0.
            POWER_LOSSES = 0.
         ENDIF

         FUEL_COST = (TRANS_VAR_EXP)/ 1000.
         VARIABLE_OM_CHARGE = -TRANS_REV / 1000.
         TOTAL_ALL_COSTS = -1.*(TRANS_REV - TRANS_VAR_EXP -
     +                                              TRANS_FIX_EXP)/1000.

         IF( ABS(DISPATCHED_ENERGY) >  0.01) THEN
            AVERAGE_TOTAL_COST = 1000.*TOTAL_ALL_COSTS/DISPATCHED_ENERGY
            AVERAGE_VARIABLE_COST = 1000.*(VARIABLE_OM_CHARGE+FUEL_COST)
     +                                                /DISPATCHED_ENERGY
         ELSE
            AVERAGE_TOTAL_COST = 0.
            AVERAGE_VARIABLE_COST = 0.
         ENDIF

         SUM_REPORT_ITEMS(1) = SUM_REPORT_ITEMS(1) +
     +                                  1000.*UNIT_CAPACITY_AFTER_LOSSES
! PER MCKEE
         SUM_REPORT_ITEMS(2) = SUM_REPORT_ITEMS(2) + DISPATCHED_ENERGY
         SUM_REPORT_ITEMS(4) = SUM_REPORT_ITEMS(4) + 1000.*UNIT_CAPACITY
! PER MCKEE
         SUM_REPORT_ITEMS(5) = SUM_REPORT_ITEMS(5) + ENRG
         SUM_REPORT_ITEMS(7) = SUM_REPORT_ITEMS(7) + MONTHLY_FIXED_COST
         SUM_REPORT_ITEMS(9) = SUM_REPORT_ITEMS(9) + FUEL_COST
         SUM_REPORT_ITEMS(11) = SUM_REPORT_ITEMS(11) +
     +                                                VARIABLE_OM_CHARGE
         SUM_REPORT_ITEMS(12) = SUM_REPORT_ITEMS(12) + TOTAL_ALL_COSTS
         SUM_REPORT_ITEMS(13) = SUM_REPORT_ITEMS(13) +
     +                                  1000.*UNIT_CAPACITY_AFTER_LOSSES
         SUM_REPORT_ITEMS(14) = SUM_REPORT_ITEMS(14) +
     +                                               1000.*UNIT_CAPACITY
!
         MD_SUM_REPORT_ITEMS(1) = MD_SUM_REPORT_ITEMS(1) +
     +                                  1000.*UNIT_CAPACITY_AFTER_LOSSES
! PER MCKEE
         MD_SUM_REPORT_ITEMS(2) =
     +                        MD_SUM_REPORT_ITEMS(2) + DISPATCHED_ENERGY
         MD_SUM_REPORT_ITEMS(4) =
     +                      MD_SUM_REPORT_ITEMS(4) + 1000.*UNIT_CAPACITY
! PER MCKEE
         MD_SUM_REPORT_ITEMS(5) = MD_SUM_REPORT_ITEMS(5) + ENRG
         MD_SUM_REPORT_ITEMS(7) =
     +                       MD_SUM_REPORT_ITEMS(7) + MONTHLY_FIXED_COST
         MD_SUM_REPORT_ITEMS(9) = MD_SUM_REPORT_ITEMS(9) + FUEL_COST
         MD_SUM_REPORT_ITEMS(11) = MD_SUM_REPORT_ITEMS(11) +
     +                                                VARIABLE_OM_CHARGE
         MD_SUM_REPORT_ITEMS(12) =
     +                         MD_SUM_REPORT_ITEMS(12) + TOTAL_ALL_COSTS
         MD_SUM_REPORT_ITEMS(13) = MD_SUM_REPORT_ITEMS(13) +
     +                                  1000.*UNIT_CAPACITY_AFTER_LOSSES
         MD_SUM_REPORT_ITEMS(14) = MD_SUM_REPORT_ITEMS(14) +
     +                                               1000.*UNIT_CAPACITY
!
         TYPE = 3
         TRACKER = GET_P_DERIV_TRACKER_INDEX(I)
         MEM_TRACKER = GET_P_DERIV_MEM_TRK_INDEX(I)
         IF(BASE_DATE + ISEAS >= HISTORICAL_PRODUCTION_DATE())
     +      CALL WVPA_STORE_CL_TRACKER_DATE_BASE(ISEAS,
     +                                           TYPE,
     +                                           TRACKER,
     +                                           FUEL_COST,
     +                                           VARIABLE_OM_CHARGE,
     +                                           MONTHLY_FIXED_COST,
     +                                           INT2(MEM_TRACKER))
! 05/20/03. NOTE THE SUMS.
!

         IF(PRINT_REPORT .AND. YES_REPORT_PRODUCT) THEN
            INQUIRE(UNIT=WABASH_REPORT_UNIT_NO,
     +                                   NEXTREC=WABASH_REPORT_UNIT_REC)
            DERIV_NAME = unit_name(1:15)//' COST'
            WRITE(WABASH_REPORT_UNIT_NO,REC=WABASH_REPORT_UNIT_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(YR+get_BASE_YEAR()),
     +               RPT_MONTH_NAME(ISEAS),
     +               DERIV_NAME,
     +               1000.*UNIT_CAPACITY_AFTER_LOSSES,
     +               DISPATCHED_ENERGY,
     +               LOSS_RATE,
     +               1000.*UNIT_CAPACITY,
     +               ENRG,
     +               FIXED_CHARGE_RATE,
     +               MONTHLY_FIXED_COST,
     +               FUEL_CHARGE_RATE,
     +               FUEL_COST,
     +               AVERAGE_VARIABLE_COST,
     +               VARIABLE_OM_CHARGE,
     +               TOTAL_ALL_COSTS,
     +               1000.*UNIT_CAPACITY_AFTER_LOSSES,
     +               1000.*UNIT_CAPACITY,
     +               POWER_LOSSES,
     +               CAPACITY_FACTOR,
     +               AVERAGE_TOTAL_COST,
     +               AVERAGE_VARIABLE_COST,
     +               0., ! RES_TRACKER,
     +               0., ! FUEL_TRACKER
     +               0., ! RATE_TRACKER
     +               0.,  ! MEM_TRACKER
     +               0.,  ! EMISSION AMOUNT
     +               0.   ! AVERAGE EMISSION RATE

         ENDIF
      ENDDO
      IF(PRINT_REPORT)THEN
            IF(SUM_REPORT_ITEMS(2) /= 0.) THEN
               SUM_REPORT_ITEMS(3) =
     +               100*(1. - SUM_REPORT_ITEMS(2) /
     +                                          SUM_REPORT_ITEMS(5))
               SUM_REPORT_ITEMS(8) = 1000.*
     +               SUM_REPORT_ITEMS(9)/SUM_REPORT_ITEMS(5)
               SUM_REPORT_ITEMS(10) = 1000.*
     +                 SUM_REPORT_ITEMS(11)/SUM_REPORT_ITEMS(5)
               SUM_REPORT_ITEMS(15) =
     +               100*(1. - SUM_REPORT_ITEMS(1) /
     +                                          SUM_REPORT_ITEMS(4))
               SUM_REPORT_ITEMS(17) =
     +               1000.*SUM_REPORT_ITEMS(12)/SUM_REPORT_ITEMS(2)
               SUM_REPORT_ITEMS(18) =
     +               1000.*(SUM_REPORT_ITEMS(9)+SUM_REPORT_ITEMS(11))/
     +                                               SUM_REPORT_ITEMS(2)
            ELSE
               SUM_REPORT_ITEMS(3) = 0.
               SUM_REPORT_ITEMS(8) = 0.
               SUM_REPORT_ITEMS(10) = 0.
               SUM_REPORT_ITEMS(15) = 0.
               SUM_REPORT_ITEMS(17) = 0.
               SUM_REPORT_ITEMS(18) = 0.
            ENDIF
            IF(SUM_REPORT_ITEMS(1) /= 0.) THEN
               SUM_REPORT_ITEMS(6) = 1000.*
     +               SUM_REPORT_ITEMS(7)/SUM_REPORT_ITEMS(1)
               SUM_REPORT_ITEMS(16) = ABS(100000.*SUM_REPORT_ITEMS(2)/
     +                              (SEAS_HOURS*SUM_REPORT_ITEMS(1)))
            ELSE
               SUM_REPORT_ITEMS(6) = 0.
               SUM_REPORT_ITEMS(16) = 0.
            ENDIF
!
            IF(ABS(MD_SUM_REPORT_ITEMS(2)) > 0.1) THEN
               MD_SUM_REPORT_ITEMS(17) =
     +               1000.*MD_SUM_REPORT_ITEMS(12)/
     +                                            MD_SUM_REPORT_ITEMS(2)
               MD_SUM_REPORT_ITEMS(18) =
     +              1000.*(MD_SUM_REPORT_ITEMS(9)+
     +                        MD_SUM_REPORT_ITEMS(11))/
     +                                           MD_SUM_REPORT_ITEMS(2)
            ELSE
               MD_SUM_REPORT_ITEMS(17) = 0.
               MD_SUM_REPORT_ITEMS(18) = 0.
            ENDIF
            IF(MD_SUM_REPORT_ITEMS(4) /= 0.) THEN
               MD_SUM_REPORT_ITEMS(15) =
     +               100*(1. - MD_SUM_REPORT_ITEMS(1) /
     +                                          MD_SUM_REPORT_ITEMS(4))
            ELSE
               MD_SUM_REPORT_ITEMS(15) = 0.
            ENDIF
            IF(MD_SUM_REPORT_ITEMS(5) /= 0.) THEN
               MD_SUM_REPORT_ITEMS(3) =
     +               100*(1. - MD_SUM_REPORT_ITEMS(2) /
     +                                          MD_SUM_REPORT_ITEMS(5))
               MD_SUM_REPORT_ITEMS(8) = 1000.*
     +               MD_SUM_REPORT_ITEMS(9)/MD_SUM_REPORT_ITEMS(5)
               MD_SUM_REPORT_ITEMS(10) = 1000.*
     +                 MD_SUM_REPORT_ITEMS(11)/MD_SUM_REPORT_ITEMS(5)
            ELSE
               MD_SUM_REPORT_ITEMS(3) = 0.
               MD_SUM_REPORT_ITEMS(8) = 0.
               MD_SUM_REPORT_ITEMS(10) = 0.
            ENDIF
!
            IF(MD_SUM_REPORT_ITEMS(1) /= 0.) THEN
               MD_SUM_REPORT_ITEMS(6) = 1000.*
     +               MD_SUM_REPORT_ITEMS(7)/MD_SUM_REPORT_ITEMS(1)
               MD_SUM_REPORT_ITEMS(16) =
     +                  ABS(100000.*MD_SUM_REPORT_ITEMS(2)/
     +                             (SEAS_HOURS*MD_SUM_REPORT_ITEMS(1)))
            ELSE
               MD_SUM_REPORT_ITEMS(6) = 0.
               MD_SUM_REPORT_ITEMS(16) = 0.
            ENDIF
! PER MCKEE: NET ENERGY OF DERIVATIVES TO ZERO.
!
            MD_SUM_REPORT_ITEMS(2) = 0.
            MD_SUM_REPORT_ITEMS(5) = 0.
!
            INQUIRE(UNIT=WABASH_REPORT_UNIT_NO,
     +                                   NEXTREC=WABASH_REPORT_UNIT_REC)
            WRITE(WABASH_REPORT_UNIT_NO,REC=WABASH_REPORT_UNIT_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(YR+get_BASE_YEAR()),
     +               RPT_MONTH_NAME(ISEAS),
     +               'Total Derivatives   ',
     +               (MD_SUM_REPORT_ITEMS(I),I=1,18),
     +               0., ! RES_TRACKER,
     +               0., ! FUEL_TRACKER
     +               0., ! RATE_TRACKER
     +               0.,  ! MEM_TRACKER
     +               MD_SUM_REPORT_ITEMS(19),
     +               MD_SUM_REPORT_ITEMS(20)
!            WABASH_REPORT_UNIT_REC = WABASH_REPORT_UNIT_REC + 1
!
! 11/25/03. MOVED BELOW DERIVATIVES FOR WVPA
!
            INQUIRE(UNIT=WABASH_REPORT_UNIT_NO,
     +                                   NEXTREC=WABASH_REPORT_UNIT_REC)
            WRITE(WABASH_REPORT_UNIT_NO,REC=WABASH_REPORT_UNIT_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(YR+get_BASE_YEAR()),
     +               RPT_MONTH_NAME(ISEAS),
     +               'Total Resources     ',
     +              (SUM_REPORT_ITEMS(I),I=1,18),
     +               0., ! RES_TRACKER,
     +               0., ! FUEL_TRACKER
     +               0., ! RATE_TRACKER
     +               0., ! MEM_TRACKER
     +               SUM_REPORT_ITEMS(19), !TOTAL MONTHLY EMISSIONS
     +               SUM_REPORT_ITEMS(20)  !AVERAGE MONTHLY EMISSIONS
!
      ENDIF
      RETURN


      ENTRY RETURN_SUM_REPORT_ITEMS(R_SUM_REPORT_ITEMS)


         R_SUM_REPORT_ITEMS(:) = SUM_REPORT_ITEMS(:)
      RETURN


      ENTRY RETURN_ANN_SUM_REPORT_ITEMS(R_SUM_REPORT_ITEMS)

         R_SUM_REPORT_ITEMS(:) = ANN_SUM_REPORT_ITEMS(:)

      RETURN

      ENTRY WABASH_ANNUAL_POWER_COSTS(YR,UNIT_NAMES)

      ANNUAL_MD_MAX_DISPATCHED_CAP = 0.
      ANNUAL_MD_MAX_PURCHASED_CAP = 0.
      DO I = 1, CL_UNITS
         UNIT_NAME = UNIT_NAMES(I)

         ENRG = ANNUAL_CL_PURCHASED_ENRG(I)
      call write_energy_trace("RETURN_SUM_REPORT_ITEMS", 
     +   "reaenrg:0005", ENRG)
         FUEL_COST = ANNUAL_CL_FUEL_CHARGE(I)
         VARIABLE_OM_CHARGE = ANNUAL_CL_VARIABLE_CHARGE(I)
         MONTHLY_EMISSIONS_COST = ANNUAL_CL_EMISSION_COST(I)
         IF(ENRG /= 0.) THEN
             LOSS_RATE = 100.*ANNUAL_CL_LOSSES(I)/ENRG
             ANNUAL_VARIABLE_OM_RATE = VARIABLE_OM_CHARGE/ENRG
             FUEL_CHARGE_RATE = FUEL_COST/ENRG
             AVERAGE_EMISSIONS_COST = MONTHLY_EMISSIONS_COST/ENRG
          ELSE
            LOSS_RATE = 0.
            FUEL_CHARGE_RATE = 0.
            ANNUAL_VARIABLE_OM_RATE = 0.
            AVERAGE_EMISSIONS_COST = 0.
         ENDIF
         DISPATCHED_ENERGY = ENRG - ANNUAL_CL_LOSSES(I)
         IF(ANNUAL_CL_DISPATCHED_CAP(I) /= 0.) THEN
            CAPACITY_FACTOR = ! AS A PERCENTAGE
     +             DISPATCHED_ENERGY*1200./
     +             (ANNUAL_HOURS * ANNUAL_CL_DISPATCHED_CAP(I))
         ELSE
            CAPACITY_FACTOR = 0.
         ENDIF
!
         RES_TRACKER = FLOAT(GET_THERMAL_RES_TRACKER_INDEX(I))
         FUEL_TRACKER = FLOAT(GET_THERMAL_FUEL_TRACKER_INDEX(I))
         MEM_TRACKER = FLOAT(GET_THERMAL_MEM_TRACKER_INDEX(I))
         RATE_TRACKER = FLOAT(GET_THERMAL_TRACKER_INDEX(I))
!
         IF(ANNUAL_CL_PURCHASED_CAP(I) /= 0.) THEN
            FIXED_CHARGE_RATE = ANNUAL_CL_FIXED_CHARGE(I)/
     +                                (1000.*ANNUAL_CL_PURCHASED_CAP(I))
            POWER_LOSSES = 100.*(ANNUAL_CL_PURCHASED_CAP(I) -
     +                                     ANNUAL_CL_DISPATCHED_CAP(I))/
     +                             ANNUAL_CL_PURCHASED_CAP(I)
         ELSE ! 2/23/98. GAT. THIS 'ELSE' WAS MISSING
            FIXED_CHARGE_RATE = 0.
            POWER_LOSSES = 0.
         ENDIF
         TOTAL_ALL_COSTS = ANNUAL_CL_FIXED_CHARGE(I)
     +                     + ANNUAL_CL_FUEL_CHARGE(I)
     +                     + ANNUAL_CL_VARIABLE_CHARGE(I)
     +                     + ANNUAL_CL_EMISSION_COST(I)
!
         IF(DISPATCHED_ENERGY >  0.) THEN
            AVERAGE_TOTAL_COST = TOTAL_ALL_COSTS/DISPATCHED_ENERGY
            AVERAGE_VARIABLE_COST = (FUEL_COST
     +                               + VARIABLE_OM_CHARGE
     +                               + MONTHLY_EMISSIONS_COST)/
     +                                                 DISPATCHED_ENERGY
         ELSE
            AVERAGE_TOTAL_COST = 0.
            AVERAGE_VARIABLE_COST = 0.
         ENDIF

         IF(I /= GIBSON_BACKUP_POINTER) THEN
            ANN_SUM_REPORT_ITEMS(1) = ANN_SUM_REPORT_ITEMS(1) +
     +                                 1000.*ANNUAL_CL_DISPATCHED_CAP(I)
            ANN_SUM_REPORT_ITEMS(4) = ANN_SUM_REPORT_ITEMS(4) +
     +                                  1000.*ANNUAL_CL_PURCHASED_CAP(I)
            ANN_SUM_REPORT_ITEMS(13) = ANN_SUM_REPORT_ITEMS(13) +
     +                             1000.*ANNUAL_CL_MAX_DISPATCHED_CAP(I)
            ANN_SUM_REPORT_ITEMS(14) = ANN_SUM_REPORT_ITEMS(14) +
     +                              1000.*ANNUAL_CL_MAX_PURCHASED_CAP(I)
         ENDIF

         ANN_SUM_REPORT_ITEMS(2) = ANN_SUM_REPORT_ITEMS(2) +
     +                                                 DISPATCHED_ENERGY
         ANN_SUM_REPORT_ITEMS(5) = ANN_SUM_REPORT_ITEMS(5) + ENRG
         ANN_SUM_REPORT_ITEMS(7) = ANN_SUM_REPORT_ITEMS(7)
     +                             + ANNUAL_CL_FIXED_CHARGE(I)/1000.
         ANN_SUM_REPORT_ITEMS(9) = ANN_SUM_REPORT_ITEMS(9)
     +                             + FUEL_COST/1000.
         ANN_SUM_REPORT_ITEMS(11) = ANN_SUM_REPORT_ITEMS(11)
     +                              + VARIABLE_OM_CHARGE/1000.
         ANN_SUM_REPORT_ITEMS(12) = ANN_SUM_REPORT_ITEMS(12)
     +                              + TOTAL_ALL_COSTS/1000.
         ANN_SUM_REPORT_ITEMS(19) = ANN_SUM_REPORT_ITEMS(19)
     +                              + MONTHLY_EMISSIONS_COST/1000.

         INQUIRE(UNIT=WABASH_REPORT_UNIT_NO,
     +                                   NEXTREC=WABASH_REPORT_UNIT_REC)
         WRITE(WABASH_REPORT_UNIT_NO,REC=WABASH_REPORT_UNIT_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(YR+get_BASE_YEAR()),
     +               RPT_MONTH_NAME(13),
     +               UNIT_NAME,
     +               1000.*ANNUAL_CL_DISPATCHED_CAP(I),
     +               DISPATCHED_ENERGY,
     +               LOSS_RATE,
     +               1000.*ANNUAL_CL_PURCHASED_CAP(I),
     +               ENRG,
     +               FIXED_CHARGE_RATE,
     +               ANNUAL_CL_FIXED_CHARGE(I)/1000.,
     +               FUEL_CHARGE_RATE,
     +               FUEL_COST/1000.,
     +               ANNUAL_VARIABLE_OM_RATE,
     +               VARIABLE_OM_CHARGE/1000.,
     +               TOTAL_ALL_COSTS/1000.,
     +               1000.*ANNUAL_CL_MAX_DISPATCHED_CAP(I),
     +               1000.*ANNUAL_CL_MAX_PURCHASED_CAP(I),
     +               POWER_LOSSES,
     +               CAPACITY_FACTOR,
     +               AVERAGE_TOTAL_COST,
     +               AVERAGE_VARIABLE_COST,
     +               RES_TRACKER,
     +               FUEL_TRACKER,
     +               RATE_TRACKER,
     +               MEM_TRACKER,
     +               MONTHLY_EMISSIONS_COST/1000.,
     +               AVERAGE_EMISSIONS_COST

      ENDDO
!
      IF(YES_RUN_TRANSACT()) THEN
!
! GAT. CUSTOM CODING FOR WVPA.
!
         IF(PRINT_REPORT) THEN
!

            ANN_SUM_REPORT_ITEMS(2) = ANN_SUM_REPORT_ITEMS(2) +
     +                ANNUAL_POWER_PURCHASED_MWH - ANNUAL_POWER_SOLD_MWH
            ANN_SUM_REPORT_ITEMS(5) = ANN_SUM_REPORT_ITEMS(5) +
     +                ANNUAL_POWER_PURCHASED_MWH - ANNUAL_POWER_SOLD_MWH

            INQUIRE(UNIT=WABASH_REPORT_UNIT_NO,
     +                                   NEXTREC=WABASH_REPORT_UNIT_REC)
            IF(ANNUAL_POWER_PURCHASED_MWH > 0.) THEN
               AVERAGE_TOTAL_COST = ANNUAL_PURCHASE_POWER_COST * 1000. /
     +                                        ANNUAL_POWER_PURCHASED_MWH
               AVERAGE_VARIABLE_COST = AVERAGE_TOTAL_COST
            ELSE
               AVERAGE_TOTAL_COST = 0.
               AVERAGE_VARIABLE_COST = 0.
            ENDIF
            WRITE(WABASH_REPORT_UNIT_NO,REC=WABASH_REPORT_UNIT_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(YR+get_BASE_YEAR()),
     +               RPT_MONTH_NAME(13),
     +               'Market Purchases    ',
     +               0.,
     +               ANNUAL_POWER_PURCHASED_MWH,
     +               0.,
     +               0.,
     +               ANNUAL_POWER_PURCHASED_MWH,
     +               0.,
     +               0.,
     +               AVERAGE_TOTAL_COST,
     +               ANNUAL_PURCHASE_POWER_COST,
     +               0.,
     +               0.,
     +               ANNUAL_PURCHASE_POWER_COST,
     +               0.,
     +               0.,
     +               0.,
     +               0.,
     +               AVERAGE_TOTAL_COST,
     +               0.,
     +               0., ! RES_TRACKER,
     +               0., ! FUEL_TRACKER
     +               0., ! RATE_TRACKER
     +               1.  ! MEM_TRACKER
!
            INQUIRE(UNIT=WABASH_REPORT_UNIT_NO,
     +                                   NEXTREC=WABASH_REPORT_UNIT_REC)
            IF(ANNUAL_POWER_SOLD_MWH > 0.) THEN
               AVERAGE_TOTAL_COST = ANNUAL_COST_OF_POWER_SOLD * 1000. /
     +                                             ANNUAL_POWER_SOLD_MWH
               AVERAGE_VARIABLE_COST = AVERAGE_TOTAL_COST
            ELSE
               AVERAGE_TOTAL_COST = 0.
               AVERAGE_VARIABLE_COST = 0.
            ENDIF
            WRITE(WABASH_REPORT_UNIT_NO,REC=WABASH_REPORT_UNIT_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(YR+get_BASE_YEAR()),
     +               RPT_MONTH_NAME(13),
     +               'Cost of Market Sales',
     +               0.,
     +               ANNUAL_POWER_SOLD_MWH,
     +               0.,
     +               0.,
     +               ANNUAL_POWER_SOLD_MWH,
     +               0.,
     +               0.,
     +               AVERAGE_TOTAL_COST,
     +               ANNUAL_FUEL_COST_OF_POWER_SOLD*.001,
     +               0.,
     +               ANNUAL_VOM_COST_OF_POWER_SOLD*.001,
     +               ANNUAL_COST_OF_POWER_SOLD,
     +               0.,
     +               0.,
     +               0.,
     +               0.,
     +               AVERAGE_TOTAL_COST,
     +               0.,
     +               0., ! RES_TRACKER,
     +               2., ! FUEL_TRACKER
     +               0., ! RATE_TRACKER
     +               2.  ! MEM_TRACKER
!
            INQUIRE(UNIT=WABASH_REPORT_UNIT_NO,
     +                                   NEXTREC=WABASH_REPORT_UNIT_REC)
            IF(ANNUAL_POWER_SOLD_MWH > 0.) THEN
               AVERAGE_TOTAL_COST = ANNUAL_MARKET_REVENUE * 1000. /
     +                                             ANNUAL_POWER_SOLD_MWH
               AVERAGE_VARIABLE_COST = AVERAGE_TOTAL_COST
            ELSE
               AVERAGE_TOTAL_COST = 0.
               AVERAGE_VARIABLE_COST = 0.
            ENDIF
            WRITE(WABASH_REPORT_UNIT_NO,REC=WABASH_REPORT_UNIT_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(YR+get_BASE_YEAR()),
     +               RPT_MONTH_NAME(13),
     +               'Market Revenue      ',
     +               0.,
     +               -1.*ANNUAL_POWER_SOLD_MWH,
     +               0.,
     +               0.,
     +               -1.*ANNUAL_POWER_SOLD_MWH,
     +               0.,
     +               0.,
     +               AVERAGE_TOTAL_COST,
     +               -1.*ANNUAL_MARKET_REVENUE,
     +               0.,
     +               0.,
     +               -1.*ANNUAL_MARKET_REVENUE,
     +               0.,
     +               0.,
     +               0.,
     +               0.,
     +               -1.*AVERAGE_TOTAL_COST,
     +               0.,
     +               0., ! RES_TRACKER,
     +               0., ! FUEL_TRACKER
     +               0., ! RATE_TRACKER
     +               2.  ! MEM_TRACKER
!
!
         ENDIF
      ENDIF
!
      IF(FIRST_CL_WRITE) FIRST_CL_WRITE = .FALSE.

      DO I =  1, CONTRACTS
         UNIT_NAME = CNTRNM(I)
         ENRG = ANNUAL_CT_PURCHASED_ENRG(I)
      call write_energy_trace("WABASH_ANNUAL_POWER_COSTS", 
     +   "reaenrg:0006", ENRG)
         UNIT_CAPACITY = ANNUAL_CT_PURCHASED_CAP(I)
         UNIT_CAPACITY_AFTER_LOSSES = ANNUAL_CT_DISPATCHED_CAP(I)
         FUEL_COST = ANNUAL_CT_FUEL_CHARGE(I)
         VARIABLE_OM_CHARGE = ANNUAL_CT_VARIABLE_CHARGE(I)
         CT_LOSSES = ANNUAL_CT_LOSSES(I)
         MONTHLY_FIXED_COST = ANNUAL_CT_FIXED_CHARGE(I)
         IF(ENRG /= 0.) THEN
            ANNUAL_VARIABLE_OM_RATE = VARIABLE_OM_CHARGE*1000./ENRG
            FUEL_CHARGE_RATE = FUEL_COST*1000./ENRG
            LOSS_RATE = 100. * ANNUAL_CT_LOSSES(I) / ENRG
         ELSE
            FUEL_CHARGE_RATE = 0.
            ANNUAL_VARIABLE_OM_RATE = 0.
            LOSS_RATE = 0.
         ENDIF
         DISPATCHED_ENERGY = ENRG - CT_LOSSES
         IF(UNIT_CAPACITY /= 0.) THEN
            CAPACITY_FACTOR = ! AS A PERCENTAGE
     +            DISPATCHED_ENERGY*1200./
     +                       (ANNUAL_HOURS * UNIT_CAPACITY_AFTER_LOSSES)
            FIXED_CHARGE_RATE = MONTHLY_FIXED_COST/(UNIT_CAPACITY)
            POWER_LOSSES = 100.*(UNIT_CAPACITY -
     +                                      UNIT_CAPACITY_AFTER_LOSSES)/
     +                                  UNIT_CAPACITY
         ELSE
            CAPACITY_FACTOR = 0.
            FIXED_CHARGE_RATE = 0.
            POWER_LOSSES = 0.
         ENDIF
         TOTAL_ALL_COSTS = MONTHLY_FIXED_COST + FUEL_COST +
     +                                                VARIABLE_OM_CHARGE
!
         IF(DISPATCHED_ENERGY /=  0.) THEN
            AVERAGE_TOTAL_COST = 1000.*TOTAL_ALL_COSTS/DISPATCHED_ENERGY
            AVERAGE_VARIABLE_COST = 1000.*(FUEL_COST+VARIABLE_OM_CHARGE)
     +                                                /DISPATCHED_ENERGY
         ELSE
            AVERAGE_TOTAL_COST = 0.
            AVERAGE_VARIABLE_COST = 0.
         ENDIF

         ANN_SUM_REPORT_ITEMS(1) = ANN_SUM_REPORT_ITEMS(1) +
     +                                  1000.*UNIT_CAPACITY_AFTER_LOSSES
         ANN_SUM_REPORT_ITEMS(2) = ANN_SUM_REPORT_ITEMS(2) +
     +                                                 DISPATCHED_ENERGY
         ANN_SUM_REPORT_ITEMS(4) = ANN_SUM_REPORT_ITEMS(4) +
     +                                               1000.*UNIT_CAPACITY
         ANN_SUM_REPORT_ITEMS(5) = ANN_SUM_REPORT_ITEMS(5) + ENRG
         ANN_SUM_REPORT_ITEMS(7) = ANN_SUM_REPORT_ITEMS(7) +
     +                                                MONTHLY_FIXED_COST
         ANN_SUM_REPORT_ITEMS(9) = ANN_SUM_REPORT_ITEMS(9) + FUEL_COST
         ANN_SUM_REPORT_ITEMS(11) = ANN_SUM_REPORT_ITEMS(11) +
     +                                                VARIABLE_OM_CHARGE
         ANN_SUM_REPORT_ITEMS(12) = ANN_SUM_REPORT_ITEMS(12) +
     +                                                   TOTAL_ALL_COSTS
         ANN_SUM_REPORT_ITEMS(13) = ANN_SUM_REPORT_ITEMS(13) +
     +                             1000.*ANNUAL_CT_MAX_DISPATCHED_CAP(I)
         ANN_SUM_REPORT_ITEMS(14) = ANN_SUM_REPORT_ITEMS(14) +
     +                              1000.*ANNUAL_CT_MAX_PURCHASED_CAP(I)

         INQUIRE(UNIT=WABASH_REPORT_UNIT_NO,
     +                                   NEXTREC=WABASH_REPORT_UNIT_REC)
         WRITE(WABASH_REPORT_UNIT_NO,REC=WABASH_REPORT_UNIT_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(YR+get_BASE_YEAR()),
     +               RPT_MONTH_NAME(13),
     +               UNIT_NAME,
     +               1000.*UNIT_CAPACITY_AFTER_LOSSES,
     +               DISPATCHED_ENERGY,
     +               LOSS_RATE,
     +               1000.*UNIT_CAPACITY,
     +               ENRG,
     +               FIXED_CHARGE_RATE,
     +               MONTHLY_FIXED_COST,
     +               FUEL_CHARGE_RATE,
     +               FUEL_COST,
     +               ANNUAL_VARIABLE_OM_RATE,
     +               VARIABLE_OM_CHARGE,
     +               TOTAL_ALL_COSTS,
     +               1000.*ANNUAL_CT_MAX_DISPATCHED_CAP(I),
     +               1000.*ANNUAL_CT_MAX_PURCHASED_CAP(I),
     +               POWER_LOSSES,
     +               CAPACITY_FACTOR,
     +               AVERAGE_TOTAL_COST,
     +               AVERAGE_VARIABLE_COST,
     +               RES_TRACKER,
     +               FUEL_TRACKER,
     +               RATE_TRACKER,
     +               MEM_TRACKER,
     +               0., ! EMISSIONS COST
     +               0.  ! AVERAGE EMISSION RATE
!         WABASH_REPORT_UNIT_REC = WABASH_REPORT_UNIT_REC + 1
      ENDDO
      IF(FIRST_CT_WRITE) FIRST_CT_WRITE = .FALSE.
!
      MD_SUM_REPORT_ITEMS = 0.
!
      LOCAL_YEAR = YR + get_BASE_YEAR()
      TEMP_YEAR = FLOAT(LOCAL_YEAR)
!
!      NUM_TRANS = GET_NUM_ANNUAL_TRANSACTIONS()
!
      NUM_TRANS = GET_NUM_SCENARIO_TRANSACTIONS()
!
      DO TRANS = 1, NUM_TRANS
         TEMP_I2 = 0
         VOID_LOGICAL =
     +         GET_MONTHLY_TRANS_VARIABLES(
     +                  TRANS,
     +                  ENRG,
     +                  TRANS_CAP,
     +                  TRANS_VAR_EXP,
     +                  TRANS_VAR_MWH,
     +                  TRANS_FIX_EXP,
     +                  TRANS_REV,
     +                  TRANS_REV_MWH,
     +                  TRANS_HOURS,
     +                  PRODUCT_HOURS,
     +                  TRANS_STRIKES,
     +                  TEMP_I2,
     +                  UNIT_NAME,
     +                  SUM_ANNUAL,
     +                  YES_REPORT_PRODUCT,
     +                  TG,
     +                  PM,
     +                  FT,
     +                  TRANS_ON_LINE,
     +                  TRANS_OFF_LINE,
     +                  TRANS_UNIT_ID,
     +                  TRANS_ASSET_CLASS,
     +                  TRANS_NOT_ACTIVE,
     +                  TEMP_YEAR)
         IF(TRANS_NOT_ACTIVE) CYCLE
!         ENRG = ANNUAL_MD_PURCHASED_ENRG(I)
         ANNUAL_MD_DISPATCHED_CAP = TRANS_CAP
         ANNUAL_MD_PURCHASED_CAP = TRANS_CAP
         ANNUAL_MD_FIXED_CHARGE = TRANS_FIX_EXP/1000.
         FUEL_COST = 0.
         VARIABLE_OM_CHARGE = 0.
!         VARIABLE_OM_CHARGE = (TRANS_VAR_EXP-TRANS_REV)/ 1000.
         IF(ABS(ENRG) > 0.01) THEN
             ANNUAL_VARIABLE_OM_RATE = TRANS_VAR_MWH
!             FUEL_CHARGE_RATE = FUEL_COST/ENRG
          ELSE
!            FUEL_CHARGE_RATE = 0.
            ANNUAL_VARIABLE_OM_RATE = 0.
         ENDIF

         FUEL_CHARGE_RATE = TRANS_VAR_MWH
         FUEL_COST = (TRANS_VAR_EXP)/ 1000.
!         FUEL_COST = (TRANS_VAR_EXP-TRANS_REV)/ 1000.
!
         LOSS_RATE = 0.
!
         DISPATCHED_ENERGY = ENRG  ! - ANNUAL_MD_LOSSES(I)
         IF(ANNUAL_MD_DISPATCHED_CAP /= 0.) THEN
            CAPACITY_FACTOR = ! AS A PERCENTAGE
     +             DISPATCHED_ENERGY*1200./
     +             (ANNUAL_HOURS * ANNUAL_MD_DISPATCHED_CAP)
         ELSE
            CAPACITY_FACTOR = 0.
         ENDIF
         IF(ANNUAL_MD_PURCHASED_CAP /= 0.) THEN
            FIXED_CHARGE_RATE = ANNUAL_MD_FIXED_CHARGE/
     +                                (ANNUAL_MD_PURCHASED_CAP)
            POWER_LOSSES = 100.*(ANNUAL_MD_PURCHASED_CAP -
     +                                     ANNUAL_MD_DISPATCHED_CAP)/
     +                             ANNUAL_MD_PURCHASED_CAP
         ELSE ! 2/23/98. GAT. THIS 'ELSE' WAS MISSING
            FIXED_CHARGE_RATE = 0.
            POWER_LOSSES = 0.
         ENDIF
!         TOTAL_ALL_COSTS = ANNUAL_MD_FIXED_CHARGE +
!     +                     FUEL_COST +
!     +                     VARIABLE_OM_CHARGE
!
! 05/13/05. PRE
!
!         TOTAL_ALL_COSTS = (TRANS_VAR_EXP + TRANS_FIX_EXP)/1000.
!
! 05/13/05. TEST
!
         FUEL_COST = (TRANS_VAR_EXP)/ 1000.
!         VARIABLE_OM_CHARGE = -TRANS_REV / 1000.
! 5/18/05. TEST.  CREATED TEMP_VAR_OM_CHARGE FOR REPORTING.
!                 PASS TRANS_REV/1000. TO FINANCIAL PER DIANE.

         VARIABLE_OM_CHARGE = TRANS_REV / 1000.  ! FOR FINANCIAL REPORTING
         TEMP_VAR_OM_CHARGE = -TRANS_REV / 1000. ! FOR POWER COST REPORTING

         TOTAL_ALL_COSTS = -1.*(TRANS_REV - TRANS_VAR_EXP -
     +                                              TRANS_FIX_EXP)/1000.
!
         IF( ABS(DISPATCHED_ENERGY) >  0.01) THEN
            AVERAGE_TOTAL_COST =
     +                        1000.*TOTAL_ALL_COSTS/DISPATCHED_ENERGY
            AVERAGE_VARIABLE_COST =
     +                          1000.*(FUEL_COST+TEMP_VAR_OM_CHARGE)/
     +                                                 DISPATCHED_ENERGY
         ELSE
            AVERAGE_TOTAL_COST = 0.
            AVERAGE_VARIABLE_COST = 0.
         ENDIF

!         IF(I /= GIBSON_BACKUP_POINTER) THEN
            ANN_SUM_REPORT_ITEMS(1) = ANN_SUM_REPORT_ITEMS(1) +
     +                                 1000.*ANNUAL_MD_DISPATCHED_CAP
            ANN_SUM_REPORT_ITEMS(4) = ANN_SUM_REPORT_ITEMS(4) +
     +                                  1000.*ANNUAL_MD_PURCHASED_CAP
            ANN_SUM_REPORT_ITEMS(13) = ANN_SUM_REPORT_ITEMS(13) +
     +                             1000.*ANNUAL_MD_MAX_DISPATCHED_CAP
            ANN_SUM_REPORT_ITEMS(14) = ANN_SUM_REPORT_ITEMS(14) +
     +                              1000.*ANNUAL_MD_MAX_PURCHASED_CAP
!         ENDIF

         ANN_SUM_REPORT_ITEMS(2) = ANN_SUM_REPORT_ITEMS(2) +
     +                                                 DISPATCHED_ENERGY
!
         ANN_SUM_REPORT_ITEMS(5) = ANN_SUM_REPORT_ITEMS(5) + ENRG
!
!
         ANN_SUM_REPORT_ITEMS(7) = ANN_SUM_REPORT_ITEMS(7) +
     +                                   ANNUAL_MD_FIXED_CHARGE
         ANN_SUM_REPORT_ITEMS(9) = ANN_SUM_REPORT_ITEMS(9) +
     +                                                   FUEL_COST
         ANN_SUM_REPORT_ITEMS(11) = ANN_SUM_REPORT_ITEMS(11) +
     +                                          VARIABLE_OM_CHARGE
         ANN_SUM_REPORT_ITEMS(12) = ANN_SUM_REPORT_ITEMS(12) +
     +                                             TOTAL_ALL_COSTS
!
         MD_SUM_REPORT_ITEMS(1) = MD_SUM_REPORT_ITEMS(1) +
     +                                    1000.*ANNUAL_MD_DISPATCHED_CAP
         MD_SUM_REPORT_ITEMS(2) =
     +                        MD_SUM_REPORT_ITEMS(2) + DISPATCHED_ENERGY
         MD_SUM_REPORT_ITEMS(4) =
     +           MD_SUM_REPORT_ITEMS(4) + 1000.*ANNUAL_MD_DISPATCHED_CAP
         MD_SUM_REPORT_ITEMS(5) = MD_SUM_REPORT_ITEMS(5) + ENRG
         MD_SUM_REPORT_ITEMS(7) =
     +                  MD_SUM_REPORT_ITEMS(7) + ANNUAL_MD_FIXED_CHARGE
         MD_SUM_REPORT_ITEMS(9) = MD_SUM_REPORT_ITEMS(9) + FUEL_COST
         MD_SUM_REPORT_ITEMS(11) = MD_SUM_REPORT_ITEMS(11) +
     +                                                VARIABLE_OM_CHARGE
         MD_SUM_REPORT_ITEMS(12) =
     +                         MD_SUM_REPORT_ITEMS(12) + TOTAL_ALL_COSTS
         MD_SUM_REPORT_ITEMS(13) = MD_SUM_REPORT_ITEMS(13) +
     +                                1000.*ANNUAL_MD_DISPATCHED_CAP
         MD_SUM_REPORT_ITEMS(14) = MD_SUM_REPORT_ITEMS(14) +
     +                                 1000.*ANNUAL_MD_DISPATCHED_CAP

         IF(YES_REPORT_PRODUCT) THEN
            INQUIRE(UNIT=WABASH_REPORT_UNIT_NO,
     +                                   NEXTREC=WABASH_REPORT_UNIT_REC)
            DERIV_NAME = UNIT_NAME(1:15)//' COST'
            WRITE(WABASH_REPORT_UNIT_NO,REC=WABASH_REPORT_UNIT_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(YR+get_BASE_YEAR()),
     +               RPT_MONTH_NAME(13),
     +               DERIV_NAME,
     +               1000.*ANNUAL_MD_DISPATCHED_CAP,
     +               DISPATCHED_ENERGY,
     +               LOSS_RATE,
     +               1000.*ANNUAL_MD_PURCHASED_CAP,
     +               DISPATCHED_ENERGY, ! PURCHASES ENERGY
     +               FIXED_CHARGE_RATE,
     +               ANNUAL_MD_FIXED_CHARGE,
     +               FUEL_CHARGE_RATE,
     +               FUEL_COST,
     +               AVERAGE_VARIABLE_COST,
     +               TEMP_VAR_OM_CHARGE,
     +               TOTAL_ALL_COSTS,
!     +               FUEL_CHARGE_RATE,
!     +               TRANS_VAR_EXP/1000.,
!     +               0., ! VAR O&M / MWH
!     +               0., ! VAR O&M
!     +               (TRANS_VAR_EXP+TRANS_FIX_EXP)/1000.,
     +               1000.*ANNUAL_MD_DISPATCHED_CAP,
     +               1000.*ANNUAL_MD_PURCHASED_CAP,
     +               POWER_LOSSES,
     +               CAPACITY_FACTOR,
     +               AVERAGE_TOTAL_COST,
     +               AVERAGE_VARIABLE_COST,
     +               0., ! RES_TRACKER,
     +               0., ! FUEL_TRACKER
     +               0., ! RATE_TRACKER
     +               0.,  ! MEM_TRACKER
     +               0., ! EMISSIONS COST
     +               0.  ! AVERAGE EMISSION RATE

         ENDIF
      ENDDO
      IF(TRANS > 0) THEN
!
            IF(ABS(MD_SUM_REPORT_ITEMS(2)) > 0.1) THEN
               MD_SUM_REPORT_ITEMS(17) =
     +               1000.*MD_SUM_REPORT_ITEMS(12)/
     +                                            MD_SUM_REPORT_ITEMS(2)
               MD_SUM_REPORT_ITEMS(18) =
     +              1000.*(MD_SUM_REPORT_ITEMS(9)+
     +                        MD_SUM_REPORT_ITEMS(11))/
     +                                           MD_SUM_REPORT_ITEMS(2)
            ELSE
               MD_SUM_REPORT_ITEMS(17) = 0.
               MD_SUM_REPORT_ITEMS(18) = 0.
            ENDIF
            IF(MD_SUM_REPORT_ITEMS(4) /= 0.) THEN
               MD_SUM_REPORT_ITEMS(15) =
     +               100*(1. - MD_SUM_REPORT_ITEMS(1) /
     +                                          MD_SUM_REPORT_ITEMS(4))
            ELSE
               MD_SUM_REPORT_ITEMS(15) = 0.
            ENDIF
            IF(MD_SUM_REPORT_ITEMS(5) /= 0.) THEN
               MD_SUM_REPORT_ITEMS(3) =
     +               100*(1. - MD_SUM_REPORT_ITEMS(2) /
     +                                          MD_SUM_REPORT_ITEMS(5))
               MD_SUM_REPORT_ITEMS(8) = 1000.*
     +               MD_SUM_REPORT_ITEMS(9)/MD_SUM_REPORT_ITEMS(5)
               MD_SUM_REPORT_ITEMS(10) = 1000.*
     +                 MD_SUM_REPORT_ITEMS(11)/MD_SUM_REPORT_ITEMS(5)
            ELSE
               MD_SUM_REPORT_ITEMS(3) = 0.
               MD_SUM_REPORT_ITEMS(8) = 0.
               MD_SUM_REPORT_ITEMS(10) = 0.
            ENDIF
!
            IF(MD_SUM_REPORT_ITEMS(1) /= 0.) THEN
               MD_SUM_REPORT_ITEMS(6) = 1000.*
     +               MD_SUM_REPORT_ITEMS(7)/MD_SUM_REPORT_ITEMS(1)
               MD_SUM_REPORT_ITEMS(16) =
     +                  ABS(100000.*MD_SUM_REPORT_ITEMS(2)/
     +                            (ANNUAL_HOURS*MD_SUM_REPORT_ITEMS(1)))
            ELSE
               MD_SUM_REPORT_ITEMS(6) = 0.
               MD_SUM_REPORT_ITEMS(16) = 0.
            ENDIF
!
! PER MCKEE: NET ENERGY OF DERIVATIVES TO ZERO.
!
            MD_SUM_REPORT_ITEMS(2) = 0.
            MD_SUM_REPORT_ITEMS(5) = 0.
!
         INQUIRE(UNIT=WABASH_REPORT_UNIT_NO,
     +                                   NEXTREC=WABASH_REPORT_UNIT_REC)
         WRITE(WABASH_REPORT_UNIT_NO,REC=WABASH_REPORT_UNIT_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(YR+get_BASE_YEAR()),
     +               RPT_MONTH_NAME(13),
     +               'Total Derivatives   ',
     +               (MD_SUM_REPORT_ITEMS(I),I=1,18),
     +               0., ! RES_TRACKER,
     +               0., ! FUEL_TRACKER
     +               0., ! RATE_TRACKER
     +               0.,  ! MEM_TRACKER
     +               MD_SUM_REPORT_ITEMS(19),
     +               MD_SUM_REPORT_ITEMS(20)
      ENDIF
!
! 12/12/03. MOVED BELOW TO CAPTURE ANNUAL DERIVATIVES.
!
      IF(ANN_SUM_REPORT_ITEMS(2) /= 0.) THEN
         ANN_SUM_REPORT_ITEMS(3) =
     +               100*(1. - ANN_SUM_REPORT_ITEMS(2) /
     +                                          ANN_SUM_REPORT_ITEMS(5))
         ANN_SUM_REPORT_ITEMS(8) = 1000.*
     +                   ANN_SUM_REPORT_ITEMS(9)/ANN_SUM_REPORT_ITEMS(5)
         ANN_SUM_REPORT_ITEMS(10) = 1000.*
     +                  ANN_SUM_REPORT_ITEMS(11)/ANN_SUM_REPORT_ITEMS(5)
         ANN_SUM_REPORT_ITEMS(15) =
     +               100*(1. - ANN_SUM_REPORT_ITEMS(1) /
     +                                          ANN_SUM_REPORT_ITEMS(4))
         ANN_SUM_REPORT_ITEMS(17) =
     +            1000.*ANN_SUM_REPORT_ITEMS(12)/ANN_SUM_REPORT_ITEMS(2)
         ANN_SUM_REPORT_ITEMS(18) =
     +         1000.*(ANN_SUM_REPORT_ITEMS(9)+ANN_SUM_REPORT_ITEMS(11))/
     +                                           ANN_SUM_REPORT_ITEMS(2)
      ELSE
         ANN_SUM_REPORT_ITEMS(3) = 0.
         ANN_SUM_REPORT_ITEMS(8) = 0.
         ANN_SUM_REPORT_ITEMS(10) = 0.
         ANN_SUM_REPORT_ITEMS(15) = 0.
         ANN_SUM_REPORT_ITEMS(17) = 0.
         ANN_SUM_REPORT_ITEMS(18) = 0.
      ENDIF
      IF(ANN_SUM_REPORT_ITEMS(1) /= 0.) THEN
         ANN_SUM_REPORT_ITEMS(6) = 1000.*
     +               ANN_SUM_REPORT_ITEMS(7)/ANN_SUM_REPORT_ITEMS(1)
         ANN_SUM_REPORT_ITEMS(16) = 1200000.*ANN_SUM_REPORT_ITEMS(2)/
     +                            (ANNUAL_HOURS*ANN_SUM_REPORT_ITEMS(1))
      ELSE
         ANN_SUM_REPORT_ITEMS(6) = 0.
         ANN_SUM_REPORT_ITEMS(16) = 0.
      ENDIF
!
      INQUIRE(UNIT=WABASH_REPORT_UNIT_NO,
     +                                   NEXTREC=WABASH_REPORT_UNIT_REC)
      WRITE(WABASH_REPORT_UNIT_NO,REC=WABASH_REPORT_UNIT_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(YR+get_BASE_YEAR()),
     +               RPT_MONTH_NAME(13),
     +               'Total Resources     ',
     +               (ANN_SUM_REPORT_ITEMS(I), I=1,18),
     +               0., ! RES_TRACKER,
     +               0., ! FUEL_TRACKER
     +               0., ! RATE_TRACKER
     +               0.,  ! MEM_TRACKER
     +               ANN_SUM_REPORT_ITEMS(19),
     +               ANN_SUM_REPORT_ITEMS(20)
!
      RETURN

      ENTRY RETURN_ANN_WVPA_VALUES(OUTPUT_ANN_PURCHASE,
     +                             OUTPUT_ANN_FIX,
     +                             OUTPUT_ANN_FUEL,
     +                             OUTPUT_ANN_VARI,
     +                             OUTPUT_ANN_TOTAL,
     +                             OUTPUT_ANN_AVE_TOTAL)

         OUTPUT_ANN_PURCHASE = 0. ! PURCHASES_INITIALIZED TO BALANCE TOTAL

         OUTPUT_ANN_FIX = ANN_SUM_REPORT_ITEMS(7)*1000000. ! FIXED
         OUTPUT_ANN_FUEL = ANN_SUM_REPORT_ITEMS(9)*1000000. ! FUEL
         OUTPUT_ANN_VARI = ANN_SUM_REPORT_ITEMS(11)*1000000. ! VARIABLE
         OUTPUT_ANN_TOTAL = ANN_SUM_REPORT_ITEMS(12) ! TOTAL
         OUTPUT_ANN_AVE_TOTAL = ANN_SUM_REPORT_ITEMS(17) ! AVERAGE TOTAL
      RETURN
      END

      SUBROUTINE KEPCO_ENRG_CAP(ISEAS,ENERGY,CAPACITY,
     +                          res_ids,NUNITS,UNITNM,SEAS_HOURS,
     +                          RESOURCE_FILE_TYPE,REPORT,
     +                          RESOURCE_TYPE,YR,
     +                          RESOURCE_ANNUAL_AREA_ENRG,
     +                          RESOURCE_ANNUAL_AREA_CAP,
     +                          ANNUAL_EMERGENCY_ENERGY,
     +                          ANNUAL_MAINTENANCE_ENERGY,
     +                          ANNUAL_AREA_ENERGY_LOSSES,
     +                          online_loc,ofline_loc,
     +                          DATE1,DATE2,
     +                          EA,
     +                          WC_PERIOD_MAINTENANCE_ENERGY,
     +                          WC_PERIOD_EMERGENCY_ENERGY,
     +                          WC_UNIT_NO)

      use SpinDriftLib
      use prod_arrays_dimensions
      use kepcocom
      USE SIZECOM
      implicit none 
	  INTEGER (kind=2) :: online_loc(NUNITS),ofline_loc(NUNITS)
      INTEGER (kind=2) ::  ISEAS,NUNITS,YR
      INTEGER (kind=2) ::  ONLINE(NUNITS),OFLINE(NUNITS),
     +          DATE1,DATE2
      REAL ::  ENERGY(2*NUNITS),CAPACITY(NUNITS)
      REAL ::  EA(NUNITS)
      REAL ::  RESOURCE_ANNUAL_AREA_ENRG(6,NUNITS)
      REAL ::  RESOURCE_ANNUAL_AREA_CAP(6,NUNITS)
      REAL ::  ANNUAL_EMERGENCY_ENERGY(NUNITS)
      REAL ::  ANNUAL_MAINTENANCE_ENERGY(NUNITS)
      REAL ::  ANNUAL_AREA_ENERGY_LOSSES(NUNITS)
      REAL ::  PERIOD_MAINTENANCE_ENERGY,PERIOD_EMERGENCY_ENERGY
      REAL ::  UNIT_ENERGY,UNIT_CAPACITY,SEAS_HOURS
      REAL ::  TOTAL_CAP_ALLOCATED,TOTAL_ENRG_ALLOCATED
      REAL ::  GET_VAR,RESOURCE_MAKE_UP_ENERGY,RESOURCE_LOSSES
      CHARACTER (len=20) ::  UNITNM(NUNITS),RESOURCE_FILE_TYPE*2
      CHARACTER (len=1) ::  RESOURCE_TYPE(NUNITS)
      INTEGER (kind=2) ::  res_ids(NUNITS)
      INTEGER (kind=2) ::  ARRAY_POINTR
      INTEGER (kind=1) ::  AREA,I
      LOGICAL (kind=1) ::  CL_RESOURCES
      LOGICAL (kind=1) ::  CT_RESOURCES,REPORT,CONTROL_AREA,

     +          ALLOCATION_ABSOLUTE
      REAL ::  RESOURCE_ENRG(6),RESOURCE_CAP(6)
      REAL ::  CAP_ALLOCATOR,ENRG_ALLOCATOR
!
      INTEGER (kind=2) ::  WC_UNIT_NO
      REAL ::  WC_PERIOD_MAINTENANCE_ENERGY,
     +     WC_PERIOD_EMERGENCY_ENERGY
!
      CL_RESOURCES =  INDEX(RESOURCE_FILE_TYPE,'CL') .NE. 0
      CT_RESOURCES =  INDEX(RESOURCE_FILE_TYPE,'CT') /= 0
      CAPACITY_ALLOCATION_ABSOLUTE = .FALSE.
      ENERGY_ALLOCATION_ABSOLUTE = .FALSE.
      IF(REALLY_KEPCO) THEN
         ENERGY_ALLOCATION_ABSOLUTE = .TRUE.
         CAPACITY_ALLOCATION_ABSOLUTE = .TRUE.
      ENDIF
      IF(REPORT .AND. PRINT_HEADER) THEN
         PRINT_HEADER = .FALSE.
         CALL DETAILED_REPORT_HEADER
         WRITE(9,'("&",A,I2,15X,A/)') '   Production period ',ISEAS,
     +                               'Control Area Allocations'
         IF(REALLY_KEPCO) THEN
            WRITE(9,'(1X,A20,T26,11(A))')    'Resource     ',
     +                                  '   Total',
     +                                  '     WPE',
     +                                  '    KP&L',
     +                                  '    KG&E',
     +                                  '   KCP&L',
     +                                  '     EDE'
            IF(NUMBER_OF_AREAS == 6) WRITE(9,'("&",A)') '    SEPC'
            WRITE(9,'("&",4A)') '  Losses',
     +                          '  Emerg ',
     +                          '  Maint '
         ELSEIF(WABASH_VALLEY) THEN
            WRITE(9,'(1X,A20,T26,A,7(A))') 'Resource     ',
     +                               '   Total ',
     +                               'I&M NIP',
     +                               '      GM',
     +                               '    IP&L',
     +                               '     PSI',
     +                               '  Losses',
     +                               '   Emerg',
     +                               '   Maint'
         ELSE
            WRITE(9,'(1X,A20,T26,A,6(A,I1),3(A))') 'Resource     ',
     +                               '   Total',
     +                              ('  Area ',I,I= 1, 6),
     +                               '  Losses',
     +                               '  Emerg ',
     +                               '  Maint '
         ENDIF
      ENDIF

      DO I = 1, NUNITS
         IF(online_loc(I) > DATE2 .OR. ofline_loc(I) < DATE1) CYCLE
         TOTAL_ENRG_ALLOCATED = 0.
         TOTAL_CAP_ALLOCATED = 0.
         IF(CL_RESOURCES) THEN
            UNIT_ENERGY = ENERGY(2*I-1) + ENERGY(2*I)
            CONTROL_AREA = .FALSE.
            ARRAY_POINTR = CL_ALLOCATE_POINTR(res_ids(I))
            IF(REALLY_KEPCO .AND. I == WC_UNIT_NO) THEN
               UNIT_ENERGY = UNIT_ENERGY +
     +                       WC_PERIOD_MAINTENANCE_ENERGY +
     +                       WC_PERIOD_EMERGENCY_ENERGY
            ENDIF
         ELSEIF(CT_RESOURCES) THEN
            UNIT_ENERGY = ENERGY(I)
            CONTROL_AREA=INDEX('123456EMSX',RESOURCE_TYPE(I)) /= 0
            ARRAY_POINTR = CT_ALLOCATE_POINTR(res_ids(I))
         ENDIF
         RESOURCE_MAKE_UP_ENERGY = -UNIT_ENERGY
         RESOURCE_LOSSES = 0.
!
         UNIT_CAPACITY = CAPACITY(I)
         IF(.NOT. CONTROL_AREA) THEN
            DO AREA = 1, NUMBER_OF_AREAS
!
! ALLOCATE CAPACITY FIRST
!
               CAP_ALLOCATOR =
     +                      AREA_CAP_ALLOCATORS(AREA,ISEAS,ARRAY_POINTR)
               IF(CAP_ALLOCATOR < 0.) CAP_ALLOCATOR =
     +                               GET_VAR(CAP_ALLOCATOR,YR,UNITNM(I))
!              IF(CAP_ALLOCATOR <= 1. .AND.
!    +                          .NOT. CAPACITY_ALLOCATION_ABSOLUTE) THEN
               IF(CAP_ALLOCATOR <= 1.) THEN
                  CAP_ALLOCATOR = CAP_ALLOCATOR * UNIT_CAPACITY
               ENDIF
               RESOURCE_CAP(AREA) = CAP_ALLOCATOR
!
! ALLOCATE ENERGY
!
               ENRG_ALLOCATOR =
     +                     AREA_ENRG_ALLOCATORS(AREA,ISEAS,ARRAY_POINTR)
               IF(ENRG_ALLOCATOR < 0.) ENRG_ALLOCATOR =
     +                              GET_VAR(ENRG_ALLOCATOR,YR,UNITNM(I))

               IF(ENRG_ALLOCATOR <= 2.) THEN
                  IF(UNIT_CAPACITY /= 0.) THEN
                     ENRG_ALLOCATOR = ENRG_ALLOCATOR * UNIT_CAPACITY *
     +                                                        SEAS_HOURS
                  ELSE
                     ENRG_ALLOCATOR = ENRG_ALLOCATOR * UNIT_ENERGY
                  ENDIF
               ELSEIF(ENRG_ALLOCATOR<=SEAS_HOURS)THEN
                  ENRG_ALLOCATOR = ENRG_ALLOCATOR * UNIT_CAPACITY
               ENDIF
               RESOURCE_ENRG(AREA) = ENRG_ALLOCATOR

! TOTALS
               TOTAL_ENRG_ALLOCATED = TOTAL_ENRG_ALLOCATED +
     +                                               RESOURCE_ENRG(AREA)
               TOTAL_CAP_ALLOCATED = TOTAL_CAP_ALLOCATED +
     +                                                RESOURCE_CAP(AREA)
            ENDDO !AREA ALLOCATION

! ADJUST FOR RELATIVE ALLOCATIONS

            ALLOCATION_ABSOLUTE =
     +                       AREA_ALLOCATION_METHOD(ARRAY_POINTR) == 'A'
            DO AREA = 1, NUMBER_OF_AREAS

               AREA_CAPACITY(AREA,ISEAS) = AREA_CAPACITY(AREA,ISEAS) +
     +                                         RESOURCE_CAP(AREA)
               RESOURCE_ANNUAL_AREA_CAP(AREA,I) = MAX(
     +                  RESOURCE_ANNUAL_AREA_CAP(AREA,I),
     +                                               RESOURCE_CAP(AREA))
!
               IF(ALLOCATION_ABSOLUTE) THEN
                  RESOURCE_ENRG(AREA) = RESOURCE_ENRG(AREA)/SEAS_HOURS
               ELSEIF(TOTAL_ENRG_ALLOCATED /= 0.) THEN
                  RESOURCE_ENRG(AREA) = UNIT_ENERGY *
     +                        (RESOURCE_ENRG(AREA)/TOTAL_ENRG_ALLOCATED)
               ENDIF
               AREA_ENERGY(AREA,ISEAS) = AREA_ENERGY(AREA,ISEAS) +
     +             (1. - AREA_RESOURCE_LOSES(AREA,ISEAS,ARRAY_POINTR)) *
     +                                               RESOURCE_ENRG(AREA)
               RESOURCE_MAKE_UP_ENERGY = RESOURCE_MAKE_UP_ENERGY +
     +                                               RESOURCE_ENRG(AREA)
               RESOURCE_LOSSES = RESOURCE_LOSSES +
     +                    AREA_RESOURCE_LOSES(AREA,ISEAS,ARRAY_POINTR) *
     +                                               RESOURCE_ENRG(AREA)
               RESOURCE_ANNUAL_AREA_ENRG(AREA,I) =
     +                  RESOURCE_ANNUAL_AREA_ENRG(AREA,I) +
     +             (1. - AREA_RESOURCE_LOSES(AREA,ISEAS,ARRAY_POINTR)) *
     +                                  SEAS_HOURS * RESOURCE_ENRG(AREA)
            ENDDO ! AREA

!  ** Don't delete this comment yet.
! THIS WAS REMOVED 3/12/93 FOR KEPCO IT MAY OR MAY NOT NEED TO BE IN
! FOR USERS LIKE SUN FLOWER.  THE PROBLEM IS THAT MAINTENANCE ENERGY AND
! EMERGENCY ENERGY MAY NOT BE AVAILABLE THE WAY IT IS FOR KEPCO
!
!           IF(EA(I) > 0. .AND. CL_RESOURCES) THEN
!              PERIOD_EMERGENCY_ENERGY = UNIT_ENERGY * (1.-EA(I))/EA(I)
!              PERIOD_MAINTENANCE_ENERGY = RESOURCE_MAKE_UP_ENERGY -
!    +                                           PERIOD_EMERGENCY_ENERGY
!           ELSE
!              PERIOD_EMERGENCY_ENERGY = 0.
!              PERIOD_MAINTENANCE_ENERGY = 0.
!           ENDIF
! ** ^^ Don't delete comment yet ^^
            IF(REALLY_KEPCO .AND. I == WC_UNIT_NO .AND.
     +                                                CL_RESOURCES) THEN
               UNIT_ENERGY = ENERGY(2*I-1) + ENERGY(2*I)
               PERIOD_EMERGENCY_ENERGY = WC_PERIOD_EMERGENCY_ENERGY
               PERIOD_MAINTENANCE_ENERGY = WC_PERIOD_MAINTENANCE_ENERGY
            ELSE
               PERIOD_EMERGENCY_ENERGY = 0.
               PERIOD_MAINTENANCE_ENERGY = 0.
            ENDIF
            EMERGENCY_ENERGY = EMERGENCY_ENERGY +
     +                              SEAS_HOURS * PERIOD_EMERGENCY_ENERGY
            ANNUAL_EMERGENCY_ENERGY(I) = ANNUAL_EMERGENCY_ENERGY(I) +
     +                              SEAS_HOURS * PERIOD_EMERGENCY_ENERGY
            MAINTENANCE_ENERGY = MAINTENANCE_ENERGY +
     +                            SEAS_HOURS * PERIOD_MAINTENANCE_ENERGY
            ANNUAL_MAINTENANCE_ENERGY(I) = ANNUAL_MAINTENANCE_ENERGY(I)+
     +                            SEAS_HOURS * PERIOD_MAINTENANCE_ENERGY
            AREA_ENERGY_LOSSES = AREA_ENERGY_LOSSES +
     +                                      SEAS_HOURS * RESOURCE_LOSSES
            ANNUAL_AREA_ENERGY_LOSSES(I) = ANNUAL_AREA_ENERGY_LOSSES(I)+
     +                                      SEAS_HOURS * RESOURCE_LOSSES
            IF(REPORT .AND. .NOT. CONTROL_AREA) THEN
               IF(CL_RESOURCES) THEN
                  WRITE(9,1000)trim(UNITNM(I))//' (GWh)',
     +                      RESOURCE_FILE_TYPE,
     +                      SEAS_HOURS*UNIT_ENERGY/1000.,
     +                     (SEAS_HOURS*RESOURCE_ENRG(AREA)*
     +        (1. - AREA_RESOURCE_LOSES(AREA,ISEAS,ARRAY_POINTR))/1000.,
     +                                       AREA = 1, NUMBER_OF_AREAS),
     +                     SEAS_HOURS * RESOURCE_LOSSES/1000.,
     +                     SEAS_HOURS * PERIOD_EMERGENCY_ENERGY/1000.,
     +                     SEAS_HOURS * PERIOD_MAINTENANCE_ENERGY/1000.
               ELSE
                  WRITE(9,1000)trim(UNITNM(I))//' (GWh)',
     +                      RESOURCE_FILE_TYPE,
     +                      SEAS_HOURS*UNIT_ENERGY/1000.,
     +                     (SEAS_HOURS*RESOURCE_ENRG(AREA)*
     +        (1. - AREA_RESOURCE_LOSES(AREA,ISEAS,ARRAY_POINTR))/1000.,
     +                                       AREA = 1, NUMBER_OF_AREAS),
     +                     SEAS_HOURS * RESOURCE_LOSSES/1000.
               ENDIF
               WRITE(9,1000) '(MW)',' ',UNIT_CAPACITY,
     +                    (RESOURCE_CAP(AREA),AREA = 1, NUMBER_OF_AREAS)
            ENDIF
         ENDIF
      ENDDO
      IF(MAINTENANCE_ENERGY > 0.) FIRST_MAINTENANCE_PERIOD = MIN(ISEAS,
     +                                         FIRST_MAINTENANCE_PERIOD)
      RETURN
 1000 FORMAT(1X,A20,2X,A2,12F8.1)
      END

      SUBROUTINE KEPCO_LAST_RESOURCE(ISEAS,SEAS_HOURS,REPORT,
     +                               CONTRACT_CAPACITY,CONTRACT_ENERGY,
     +                               NUMBER_OF_CONTRACTS,
     +                               RESOURCE_TYPE,
     +                               MAXIMUM_CAPACITY,
     +                               MAXIMUM_ENERGY,
     +                               CNTR_CAPACITY_SWITCH,
     +                               MAX_RATCHET_PATTERN,
     +                               MINIMUM_CAPACITY,
     +                               DATE1,DATE2,
     +                               CNTR_ON_LI,CNTR_OFF_LI)

      use SpinDriftLib
      use prod_arrays_dimensions
      use kepcocom
      use cls_load
      USE SIZECOM



      implicit none
      INTEGER (kind=2) ::  ISEAS,YR,DATE1,DATE2
      INTEGER (kind=2) ::  CNTR_ON_LI(*),CNTR_OFF_LI(*)
      INTEGER (kind=1) ::  AREA,CONTRACT
      REAL ::  AREA_FORECAST,SEAS_HOURS
      REAL ::  TOTAL_LAST_ENERGY
      REAL ::  TOTAL_LAST_CAPACITY,TOTAL_SURPLUS_ENERGY
      REAL ::  TOTAL_AREA_ENERGY,TOTAL_AREA_COIN_PEAK
!
! ADDED 8/29/92 BY MSG TO HANDLE SEASONAL PRICING ON AREA CONTRACTS
!
      INTEGER (kind=2) ::  NUMBER_OF_CONTRACTS,I

      REAL ::  ENRG_LAST_RESOURCE_BAL(6),CAPACITY_LAST_RESOURCE_BAL(6)
      REAL ::  EMERGENCY_ENERGY_BAL,MAINTENANCE_ENERGY_BAL,

     +     TOTAL_SURPLUS_ENERGY_BAL
      CHARACTER (len=1) ::  RESOURCE_TYPE(NUMBER_OF_CONTRACTS),
     +            CNTR_CAPACITY_SWITCH(NUMBER_OF_CONTRACTS)
      REAL ::  MAXIMUM_CAPACITY(NUMBER_OF_CONTRACTS),
     +     MAXIMUM_ENERGY(NUMBER_OF_CONTRACTS),
     +     MAX_RATCHET_PATTERN(NUMBER_OF_CONTRACTS),
     +     MINIMUM_CAPACITY(NUMBER_OF_CONTRACTS),
     +     TEMP_REAL,KEPCO_AREA_LOSSES(6),KEPCO_TOTAL_LOSSES,
     +     KEPCO_AREA_PURCHASES(6),KEPCO_TOTAL_AREA_PURCHASES,
     +     KEPCO_AREA_SALES(6),KEPCO_TOTAL_AREA_SALES,
     +     KEPCO_DUMP_ENERGY(6),KEPCO_TOTAL_DUMP_ENERGY
! END 8/29/92
      REAL ::  CONTRACT_CAPACITY(NUMBER_OF_CONTRACTS),
     +     CONTRACT_ENERGY(NUMBER_OF_CONTRACTS)
      LOGICAL (kind=1) ::  REPORT
!
!
      TOTAL_LAST_ENERGY = 0.
      TOTAL_SURPLUS_ENERGY = 0.
      TOTAL_AREA_ENERGY = 0.
      TOTAL_LAST_CAPACITY = 0.
      TOTAL_AREA_COIN_PEAK = 0.
      KEPCO_TOTAL_LOSSES = 0.
      KEPCO_TOTAL_AREA_PURCHASES = 0.
      KEPCO_TOTAL_AREA_SALES = 0.
      KEPCO_TOTAL_DUMP_ENERGY = 0.
      KEPCO_DUMP_ENERGY = 0.
!
! KEPCO DUMP ENERGY 5/26/94
!
      IF(REALLY_KEPCO) THEN
         DO I = 1, NUMBER_OF_CONTRACTS
            IF(.NOT. (CNTR_ON_LI(I) <= DATE2 .AND.
     +                                   CNTR_OFF_LI(I) >= DATE1)) CYCLE
            IF(RESOURCE_TYPE(I) == 'X') THEN
               CONTRACT_ENERGY(I) = MAXIMUM_ENERGY(I)/SEAS_HOURS
               AREA = 1
               KEPCO_DUMP_ENERGY(AREA) = KEPCO_DUMP_ENERGY(AREA) +
     +                                                 MAXIMUM_ENERGY(I)
               KEPCO_ANNUAL_DUMP_ENERGY(AREA)= MAXIMUM_ENERGY(I) +
     +                                    KEPCO_ANNUAL_DUMP_ENERGY(AREA)
               KEPCO_TOTAL_DUMP_ENERGY = KEPCO_TOTAL_DUMP_ENERGY +
     +                                                 MAXIMUM_ENERGY(I)
            ENDIF
          ENDDO
      ENDIF
      DO AREA = 1, NUMBER_OF_AREAS
         AREA_FORECAST = FORECAST_ENERGY(1,ISEAS,AREA) +
     +                   FORECAST_ENERGY(2,ISEAS,AREA)
         IF(REALLY_KEPCO) THEN
            KEPCO_AREA_SALES(AREA) = AREA_FORECAST
            KEPCO_ANNUAL_AREA_SALES(AREA) = AREA_FORECAST +
     +                                     KEPCO_ANNUAL_AREA_SALES(AREA)
            TEMP_REAL = AREA_FORECAST/(1. - CLASS_LOSSES(AREA))
            KEPCO_AREA_LOSSES(AREA) = TEMP_REAL - AREA_FORECAST
            AREA_FORECAST = TEMP_REAL + KEPCO_DUMP_ENERGY(AREA)
            KEPCO_AREA_PURCHASES(AREA) = AREA_FORECAST
            KEPCO_TOTAL_LOSSES = KEPCO_TOTAL_LOSSES +
     +                                           KEPCO_AREA_LOSSES(AREA)
            KEPCO_TOTAL_AREA_PURCHASES = KEPCO_TOTAL_AREA_PURCHASES +
     +                                        KEPCO_AREA_PURCHASES(AREA)
            KEPCO_TOTAL_AREA_SALES = KEPCO_TOTAL_AREA_SALES +
     +                                            KEPCO_AREA_SALES(AREA)
         ENDIF
         TOTAL_AREA_ENERGY = TOTAL_AREA_ENERGY + AREA_FORECAST
         ANNUAL_AREA_ENERGY(AREA) = ANNUAL_AREA_ENERGY(AREA) +
     +                              AREA_FORECAST
         IF(AREA_FORECAST > 0.) THEN
            ENRG_LAST_RESOURCE(AREA,ISEAS) = MAX(AREA_FORECAST -
     +                           SEAS_HOURS*AREA_ENERGY(AREA,ISEAS), 0.)
            AREA_SURPLUS(AREA,ISEAS) =
     +          MAX(SEAS_HOURS*AREA_ENERGY(AREA,ISEAS)-AREA_FORECAST,0.)
         ELSE
            ENRG_LAST_RESOURCE(AREA,ISEAS) = 0.
            AREA_SURPLUS(AREA,ISEAS) = 0.
         ENDIF
         ANNUAL_AREA_SURPLUS_ENERGY(AREA) = AREA_SURPLUS(AREA,ISEAS) +
     +                                  ANNUAL_AREA_SURPLUS_ENERGY(AREA)
         ENRG_LAST_RESOURCE_BAL(AREA) = ENRG_LAST_RESOURCE(AREA,ISEAS)/
     +                                       SEAS_HOURS
         TOTAL_LAST_ENERGY = TOTAL_LAST_ENERGY +
     +                                    ENRG_LAST_RESOURCE(AREA,ISEAS)
         TOTAL_SURPLUS_ENERGY = TOTAL_SURPLUS_ENERGY +
     +                                          AREA_SURPLUS(AREA,ISEAS)
         AREA_FORECAST = MAX(FORECAST_COINCIDENT_PEAK(1,ISEAS,AREA),
     +                      FORECAST_COINCIDENT_PEAK(2,ISEAS,AREA)) *
     +                      CLASS_COIN_FACTOR(AREA)
         ANNUAL_COINCIDENT_PEAK(AREA)=MAX(ANNUAL_COINCIDENT_PEAK(AREA),
     +                                                    AREA_FORECAST)
         ANNUAL_NONCOINCIDENT_PEAK(AREA) =
     +                    MAX(ANNUAL_NONCOINCIDENT_PEAK(AREA),
     +                        FORECAST_COINCIDENT_PEAK(1,ISEAS,AREA),
     +                        FORECAST_COINCIDENT_PEAK(2,ISEAS,AREA))
         TOTAL_AREA_COIN_PEAK = TOTAL_AREA_COIN_PEAK + AREA_FORECAST
         IF(AREA_FORECAST > 0.) THEN
            CAPACITY_LAST_RESOURCE(AREA,ISEAS) = MAX(AREA_FORECAST -
     +                                    AREA_CAPACITY(AREA,ISEAS), 0.)
         ELSE
            CAPACITY_LAST_RESOURCE(AREA,ISEAS) = 0.
         ENDIF
         CAPACITY_LAST_RESOURCE_BAL(AREA) =
     +                                CAPACITY_LAST_RESOURCE(AREA,ISEAS)
         TOTAL_LAST_CAPACITY = TOTAL_LAST_CAPACITY +
     +                                CAPACITY_LAST_RESOURCE(AREA,ISEAS)
      ENDDO ! AREA
!
! ASSIGN CAPACITY AND ENERGY NEEDS TO AREA RESOURCE OF LAST RESORT
!
      EMERGENCY_ENERGY_BAL = EMERGENCY_ENERGY/SEAS_HOURS
      MAINTENANCE_ENERGY_BAL = MAINTENANCE_ENERGY/SEAS_HOURS
      TOTAL_SURPLUS_ENERGY_BAL = TOTAL_SURPLUS_ENERGY/SEAS_HOURS
      DO I = 1, NUMBER_OF_CONTRACTS
         IF(.NOT. (CNTR_ON_LI(I) <= DATE2 .AND.
     +                                   CNTR_OFF_LI(I) >= DATE1)) CYCLE
         AREA = INDEX('123456',RESOURCE_TYPE(I))
         IF(AREA > 0) THEN
            CONTRACT_ENERGY(I) = MIN(MAXIMUM_ENERGY(I)/SEAS_HOURS,
     +                               ENRG_LAST_RESOURCE_BAL(AREA))
            ENRG_LAST_RESOURCE_BAL(AREA) = MAX(0.,
     +                                ENRG_LAST_RESOURCE_BAL(AREA) -
     +                                CONTRACT_ENERGY(I))

            IF(MAXIMUM_ENERGY(I) /= 0. .AND.
     +                      CAPACITY_LAST_RESOURCE_BAL(AREA) /= 0.) THEN
               IF(MAX_RATCHET_PATTERN(I) /= 0) THEN
                  CONTRACT_CAPACITY(I)=CAPACITY_LAST_RESOURCE_BAL(AREA)
                  MAXIMUM_CAPACITY(I) = MAX(MAXIMUM_CAPACITY(I),
     +                                      CONTRACT_CAPACITY(I),
     +                                      MINIMUM_CAPACITY(I))
               ELSE IF(CNTR_CAPACITY_SWITCH(I) == 'V') THEN
                  CONTRACT_CAPACITY(I)=CAPACITY_LAST_RESOURCE_BAL(AREA)
                  MAXIMUM_CAPACITY(I) = CONTRACT_CAPACITY(I)
               ELSE
                  CONTRACT_CAPACITY(I) = MIN(MAXIMUM_CAPACITY(I),
     +                                 CAPACITY_LAST_RESOURCE_BAL(AREA))
               ENDIF
               CAPACITY_LAST_RESOURCE_BAL(AREA) = MAX(0.,
     +                                CAPACITY_LAST_RESOURCE_BAL(AREA) -
     +                                CONTRACT_CAPACITY(I))
            ENDIF
         ELSEIF(RESOURCE_TYPE(I) == 'E') THEN
            CONTRACT_CAPACITY(I) = 0.
            CONTRACT_ENERGY(I) = MIN(MAXIMUM_ENERGY(I)/SEAS_HOURS,
     +                               EMERGENCY_ENERGY_BAL)
            EMERGENCY_ENERGY_BAL = MAX(0.,EMERGENCY_ENERGY_BAL -
     +                                               CONTRACT_ENERGY(I))
         ELSEIF(RESOURCE_TYPE(I) == 'M') THEN
            CONTRACT_CAPACITY(I) = 0.
            CONTRACT_ENERGY(I) = MIN(MAXIMUM_ENERGY(I)/SEAS_HOURS,
     +                               MAINTENANCE_ENERGY_BAL)
            MAINTENANCE_ENERGY_BAL = MAX(0.,MAINTENANCE_ENERGY_BAL -
     +                                               CONTRACT_ENERGY(I))
         ELSEIF(RESOURCE_TYPE(I) == 'S') THEN
            CONTRACT_CAPACITY(I) = 0.
            CONTRACT_ENERGY(I) = MIN(MAXIMUM_ENERGY(I)/SEAS_HOURS,
     +                               TOTAL_SURPLUS_ENERGY_BAL)
            TOTAL_SURPLUS_ENERGY_BAL = MAX(0.,TOTAL_SURPLUS_ENERGY_BAL -
     +                                               CONTRACT_ENERGY(I))
         ENDIF
      ENDDO
!
! CHECK FOR UNSERVED AREA ENERGY
!
      DO AREA = 1, NUMBER_OF_AREAS
         IF(ENRG_LAST_RESOURCE_BAL(AREA) /= 0.) THEN
            WRITE(4,*) 'Area',AREA,' in',ISEAS,
     +                 ' has unserved energy',
     +                   ENRG_LAST_RESOURCE_BAL(AREA)
         ENDIF
         IF(CAPACITY_LAST_RESOURCE_BAL(AREA) > 0.) THEN
            WRITE(4,*) 'Area',AREA,' in',ISEAS,
     +                 ' has unserved capacity needs',
     +                 CAPACITY_LAST_RESOURCE_BAL(AREA)
         ENDIF
      ENDDO

! EMERGENCY AND MAINTENANCE ENERGY BALANCE ASSIGNED TO LAST
! EMERGENCY AND MAINTENANCE CONTRACT FOUND IN THE CONTRACTS FILE

      IF(EMERGENCY_ENERGY_CONTRACT > 0) THEN
         CONTRACT_CAPACITY(EMERGENCY_ENERGY_CONTRACT) = 0.
         CONTRACT_ENERGY(EMERGENCY_ENERGY_CONTRACT) =
     +                      CONTRACT_ENERGY(EMERGENCY_ENERGY_CONTRACT) +
     +                                              EMERGENCY_ENERGY_BAL
      ENDIF
      IF(MAINTENANCE_ENERGY_CONTRACT > 0) THEN
         CONTRACT_CAPACITY(MAINTENANCE_ENERGY_CONTRACT) = 0.
         CONTRACT_ENERGY(MAINTENANCE_ENERGY_CONTRACT) =
     +                    CONTRACT_ENERGY(MAINTENANCE_ENERGY_CONTRACT) +
     +                                            MAINTENANCE_ENERGY_BAL
      ENDIF
!
      IF(REPORT) THEN
         WRITE(9,1000) 'Area Utilities (GWh)',TOTAL_LAST_ENERGY/1000.,
     +                (ENRG_LAST_RESOURCE(AREA,ISEAS)/1000.,
     +                                        AREA = 1, NUMBER_OF_AREAS)
         WRITE(9,1000) 'Capacity (MW) ',TOTAL_LAST_CAPACITY,
     +         (CAPACITY_LAST_RESOURCE(AREA,ISEAS),
     +                                        AREA = 1, NUMBER_OF_AREAS)
         WRITE(9,1000)'Surplus Energy (GWh)',TOTAL_SURPLUS_ENERGY/1000.,
     +        (AREA_SURPLUS(AREA,ISEAS)/1000.,AREA = 1, NUMBER_OF_AREAS)
         IF(REALLY_KEPCO) THEN
            WRITE(9,1000) 'Area Retail Sales (GWh)',
     +                    KEPCO_TOTAL_AREA_SALES/1000.,
     +                   (KEPCO_AREA_SALES(AREA)/1000.,
     +                                        AREA = 1, NUMBER_OF_AREAS)
            WRITE(9,1000) 'Area Losses (GWh)',KEPCO_TOTAL_LOSSES/1000.,
     +                         (KEPCO_AREA_LOSSES(AREA)/1000.,
     +                                        AREA = 1, NUMBER_OF_AREAS)
            WRITE(9,1000) 'Area Other Sales (GWh)',
     +                     KEPCO_TOTAL_DUMP_ENERGY/1000.,
     +                     (KEPCO_DUMP_ENERGY(AREA)/1000.,
     +                                        AREA = 1, NUMBER_OF_AREAS)
            WRITE(9,1000) 'Area Purchases (GWh)',
     +                    KEPCO_TOTAL_AREA_PURCHASES/1000.,
     +                   (KEPCO_AREA_PURCHASES(AREA)/1000.,
     +                                        AREA = 1, NUMBER_OF_AREAS)
         ELSE
            WRITE(9,1000)'Area Forecasts (GWh)',TOTAL_AREA_ENERGY/1000.,
     +         ((FORECAST_ENERGY(1,ISEAS,AREA) +
     +                   FORECAST_ENERGY(2,ISEAS,AREA))/1000.,
     +                                        AREA = 1, NUMBER_OF_AREAS)
         ENDIF
         WRITE(9,1010) 'non-coincident peak',
     +         (MAX(FORECAST_COINCIDENT_PEAK(1,ISEAS,AREA),
     +                       FORECAST_COINCIDENT_PEAK(2,ISEAS,AREA)),
     +                                        AREA = 1, NUMBER_OF_AREAS)
         WRITE(9,1000) 'coincident peak',TOTAL_AREA_COIN_PEAK,
     +               (CLASS_COIN_FACTOR(AREA) *
     +                 MAX(FORECAST_COINCIDENT_PEAK(1,ISEAS,AREA),
     +                       FORECAST_COINCIDENT_PEAK(2,ISEAS,AREA)),
     +                                        AREA = 1, NUMBER_OF_AREAS)
         WRITE(9,*)
         WRITE(9,1020) 'Emergency Energy (GWh)',
     +                                     EMERGENCY_ENERGY/1000.
         WRITE(9,1020) 'Maintenance Energy (GWh)',
     +                                   MAINTENANCE_ENERGY/1000.
         WRITE(9,1020) 'Energy Losses (GWh)',
     +                                   AREA_ENERGY_LOSSES/1000.
      ENDIF
      RETURN
 1000 FORMAT(1X,A20,4X,8F8.1)
 1010 FORMAT(1X,A20,12X,7F8.1)
 1020 FORMAT(1X,A24,F8.1)
      END

