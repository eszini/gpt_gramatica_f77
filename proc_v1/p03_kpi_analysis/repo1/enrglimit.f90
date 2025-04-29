
module enrglimit
   

!     Enrglimt.for
!     Created: 10/19/02 3:15:03 PM
!     Author : msg
!     Last change: msg 8/23/2020 3:08:52 PM

!  FILE NAME = ENRGLIMT.FOR

!
!                   LOAD MODIFICATION PROGRAM
!                FOR ROR AND PEAKING HYDRO UNITS
!                       AND STORAGE UNITS
!                PART OF THE PICAM MODEL
!!          COPYRIGHT 1983 (C) M.S. GERBER & ASSOCIATES, INC.
!                       ALL RIGHTS RESERVED
!!                    THE MIDAS GOLD MODEL
!           COPYRIGHT (C) 1991-94 M.S. GERBER & ASSOCIATES,INC.
!                       ALL RIGHTS RESERVED
!
!

!
!
!      THE PURPOSE OF THIS PROGRAM IS TO MODIFY HOURLY LOADS TO
!      ELIMINATE HYDRO AND STORAGE GENERATION FROM SYSTEM RESOURCES.
!!      FOR ROR HYDRO EACH LOAD IS REDUCED BY THE MINIMUM OUTPUT
!         CAPACITY OF THESE UNITS
!      FOR PEAKING HYDRO THE HIGHEST LOADS ARE REDUCED BY THE
!         MAXIMUM OUTPUT MINUS THE MINIMUM OUTPUT OF THE HYDRO
!         UNITS.  THIS REDUCTION IS SUBJECT TO THE PEAKEN REDUCTION
!         CONSTRAINT.
!      FOR STORAGE DEVICES THE PEAKEN AND CAPACITY FROM THE DEVICE
!         IS ADDED TO THE PEAKING HYDRO TO REDUCE PEAK LOADS.
!         THE PEAKEN AND CAPACITY INPUT TO THE DEVICE IS ADDED TO
!         THE MINIMUM HOURLY LOADS.

      use capacity_arrays
      use globecom
      use SpinDriftLib
      use prod_arrays_dimensions
      use sizecom
      use prodcom
      use prod2com
	  use production
      use elrptcom
      use poolcom
      use enrg_prod_costs
      use group_prod
      use ENRG_PROD_COSTSR8
      use enrg_prod_costsi4
      use aitvafin
      use aitvaprod
      use elrptcom
      use enrg_helper
      use mod_base_year
     
implicit none
      integer (kind=2), save :: SAVE_NUMBER_OF_TRANS_GROUPS_EN=0
      real (kind=4), allocatable, save :: TRANS_ROR_CAPACITY_EN(:)
      real (kind=8), allocatable :: HOURLY_HYDRO_EN(:,:,:)
      integer (kind=2) :: MAX_HYDRO_CLASS_NUM=0
      integer (kind=2) :: LOCAL_YEAR=0
      integer (kind=2) :: LAST_SEASON=12
      integer (kind=2) :: FISCAL_SEASON=0
      integer (kind=2) :: FISCAL_SEASON_RESET=0
      integer (kind=2) :: MAX_MONTHLY_GROUP_VARIABLES=16
      
      logical (kind=1) :: FISCAL_ONLY=.false.
      logical (kind=1) :: YES_FISCAL_REPORTING=.false.
      integer (kind=2) :: MAX_MONTHLY_GROUPS=0
      integer (kind=2) :: SAVE_HOURS_IN_PERIOD=0
      integer (kind=2) :: SAVE_MONTH=0
      integer (kind=2) :: HOURS_IN_PERIOD
      
      
contains
      subroutine get_hydro_rps_sum(R_MONTH,R_ST_TG,R_RESOURCE_RPS_VARS)
      use rps_index_translate_mod
      use rangechecker
      implicit none
      integer (kind=2) :: pm, i, R_ST_TG
      real (kind=4) :: R_RESOURCE_RPS_VARS(:)
      integer (kind=2) :: r_month

      
     ! TODO: Need a better way to determine whether to access
     ! these arrays.  Allocation should probably still be checked, but
     ! only after knowing if it /should/ be allocated.
         IF(ALLOCATED(RPS_HYDRO_DB)) THEN
         

            if(r_st_tg==0) then
                call end_program("enrglimit:0004 - " // &
                "r_st_tg is zero.")
            end if

            DO PM = 2, 7

               I = RPS_INDEX_TRANSLATE(PM)
                if(r_month==0) then
                    r_month=r_month ! Debugstop
                    ! I=11/11
                    ! PM=2/2
                end if
                

                 
               R_RESOURCE_RPS_VARS(PM) = R_RESOURCE_RPS_VARS(PM) + &
                                      RPS_HYDRO_DB(2,R_ST_TG,I,R_MONTH)
                                      
              ! TODO: Ensure PM+8 is still valid after adding PMs.
              ! stateissue

                
              R_RESOURCE_RPS_VARS(PM+8) = R_RESOURCE_RPS_VARS(PM+8) + &
                                      RPS_HYDRO_DB(1,R_ST_TG,I,R_MONTH)
! TOTAL GWH MW
               R_RESOURCE_RPS_VARS(1) = R_RESOURCE_RPS_VARS(1) + &
                                      RPS_HYDRO_DB(2,R_ST_TG,I,R_MONTH)
               R_RESOURCE_RPS_VARS(9) = R_RESOURCE_RPS_VARS(9) + &
                                      RPS_HYDRO_DB(1,R_ST_TG,I,R_MONTH)
            END DO
         ENDIF
      end subroutine get_hydro_rps_sum

      subroutine GET_TRANS_ROR_CAPACITY(R_NUMBER_OF_TRANS_GROUPS, &
        R_CAPACITY)
        use mod_base_year
        implicit none

          integer (kind=2) :: R_NUMBER_OF_TRANS_GROUPS
          real (kind=4) :: R_CAPACITY(0:*)
          integer :: i
          integer (kind=2) :: sntgen ! Debug view of SAVE_NUMBER_OF_TRANS_GROUPS_EN
          sntgen=SAVE_NUMBER_OF_TRANS_GROUPS_EN
          if(SAVE_NUMBER_OF_TRANS_GROUPS_EN==0) then
            stop "SAVE_NUMBER_OF_TRANS_GROUPS can't be zero."
         else
            sntgen=sntgen ! Debugstop
          end if
          
       IF(SAVE_NUMBER_OF_TRANS_GROUPS_EN /= R_NUMBER_OF_TRANS_GROUPS) THEN 
            WRITE(4,*) "During Multi-area simulation, the model"
            WRITE(4,*) "detected a mismatch in the number of"
            WRITE(4,*) "active Transaction Groups"
            WRITE(4,*) "within dispatchable hydro. (1)"
            stop "trans groups 1"

         ENDIF
         IF(ALLOCATED(TRANS_ROR_CAPACITY_EN)) THEN
            DO I = 0, R_NUMBER_OF_TRANS_GROUPS
               R_CAPACITY(I) = TRANS_ROR_CAPACITY_EN(I)
            ENDDO
         ELSE
            DO I = 0, R_NUMBER_OF_TRANS_GROUPS
               R_CAPACITY(I) = 0.
            ENDDO
         ENDIF
      end subroutine get_trans_ror_capacity
      SUBROUTINE ENRGLIMT(LPROB4,LODDUR4,DX,PEAK, & 
                          HYDRO_MONTHLY_ENRG,HYDROCAPMO, &
                          EL_SO2_PERIOD,EL_SO2, &
                          DEMAND,SALES_ENERGY, &
                          SALES_ENERGY_NOT_IN_FORECAST, &
                          ENRG_LIMITED_PURCHASE_ENERGY, &
                          YR,ISEAS,LDCPTS,HOURS, &
                          PERIOD_COUNTER, &
                          BTU_TAX_ACTIVE, &
                          ANNUAL_EL_MWH_H12, &
                          ANNUAL_EL_MWH_FOR_BTU_TAX_H12)
      use mod_base_year
      use pp_objt_interfaces
      implicit none
      real (kind=4) :: arg_R_TRANS_GROUP_CAPACITY
      real*4 LODDUR4(1000),LPROB4(1000) ! caller's arrays
      LOGICAL*1 :: UNUSED_ENRG, &
                RUN_TRANSACT/.FALSE./
      INTEGER (kind=2) ::  YR,ISEAS,ITEMP,I,J,L,LPEAK,NEXT,LPUMP,ORG_LDCPTS, &
                PERIOD_COUNTER
      REAL PEAK1,BASE1,PEAK2,BASE2,PEAK,DX,HYDRO_MONTHLY_ENRG(*), &
           HYDROCAPMO(*),RTEMP_VARCST,RTEMP_FIXED_COST, &
           BASE_ENRG,EL_SO2_PERIOD,EL_SO2(*)
      REAL*8 DEMAND,ENRG2,SALES_ENERGY,ENRG1, &
           SALES_ENERGY_NOT_IN_FORECAST,ENRG_LIMITED_PURCHASE_ENERGY, &
             DOUBLE_BASE_ENRG
!  TRANSACT VARIABLES 5/27/98. GAT.
      LOGICAL*1      TRANS_HYDRO, &
                     YES_RUN_MULTIAREA_TRANSACT, &
                     TF_FILE_EXISTS, &
                     YES_USE_TRANSACT_LOADS, &
                     YES_USE_TF_FILE_FOR_MULTIAREA, &
                     YES_STRICT_MARKET_PRICE, &
                     YES_CENTRAL_DISPATCH_TRANSACT
      INTEGER*2      R_NUMBER_OF_TRANS_GROUPS,TARGET_TRANS_GROUP, &
                     GET_TRANS_GROUP_POSITION, &
                     R_TRANS_GROUP

      REAL*4 :: R_CAPACITY(0:*), &
                     TRANS_PEAK_CAPACITY(:),TRANS_PUMP_CAPACITY(:)
      ALLOCATABLE :: &
                     TRANS_PEAK_CAPACITY,TRANS_PUMP_CAPACITY
      SAVE         &
                     TRANS_PEAK_CAPACITY,TRANS_PUMP_CAPACITY
!   CLINTON'S BTU TAX ON HYDRO
      LOGICAL*1 BTU_TAX_ACTIVE
      REAL*8 ANNUAL_EL_MWH_H12(0:3), &
             ANNUAL_EL_MWH_FOR_BTU_TAX_H12(0:3)
!      TYPE DECLARATION FOR /HYDRVL/
      CHARACTER*1 STORAGE_COLLECTION(MAX_EL_UNITS), &
                  STORAGE_ASSIGNMENT(MAX_EL_UNITS), &
                  PEAK_COLLECTION(MAX_EL_UNITS), &
                  PEAK_ASSIGNMENT(MAX_EL_UNITS)
!  5/20/92 ADDED VARIABLES FOR EL_GROUP; 8/16/92 PUMP_GROUP OUT
      INTEGER*2 PEAK_GROUP(MAX_EL_UNITS), &
                PUMP_GROUP(MAX_EL_UNITS), &
                PEAK_INDEX(MAX_EL_UNITS), &
                OFF_PEAK_INDEX(MAX_EL_UNITS)
      REAL :: SEASON_CAPACITY(0:MAX_REPORTING_GROUPS)
      REAL EL_SO2_EMIS(MAX_EL_UNITS), &
           PEAK_SO2(MAX_EL_UNITS), &
           PUMP_SO2(MAX_EL_UNITS)
      INTEGER*2 NPUMP,NPEAK
      real*8 & !  REAL before 20030422
           PEAKCP(MAX_EL_UNITS), &
           IPUMCP(MAX_EL_UNITS), &
           PEAKEN(MAX_EL_UNITS), &
           IPUMEN(MAX_EL_UNITS), &
           CAPACITY_FOR_PKSHIF,ENERGY_FOR_PKSHIF
      REAL PEAK_COST_ALLOCATION(MAX_EL_UNITS), &
           OFF_PEAK_COST_ALLOCATION(MAX_EL_UNITS), &
          RR_PEAK_ENRG,RR_OFPEAK_ENRG,ENERGY_DIF,MINCAP,JPEAK_ENERGY, &
           PEAK_AFTER_EL4,TEMP_R4

!      TYPE DECLARATION FOR /OPPARM/
      INTEGER*2   PROD_PERIODS,PRODUCTION_PERIODS, &
                  JPEAK,START_LPEAK,END_LPEAK
!      TYPE DECLARATION FOR /VFCOST/
      REAL PHVCST(MAX_EL_UNITS), &
           P_FIXED_COST(MAX_EL_UNITS), &
           P_ANNUAL_FIXED_COST(MAX_EL_UNITS), &
           VAR_STORAGE_COST(MAX_EL_UNITS), &
           FIXED_STORAGE_COST(MAX_EL_UNITS), &
           ANNUAL_FIXED_STORAGE_COST(MAX_EL_UNITS)
!      TYPE DECLARATION FOR /LOADCR/
      real*8 LODDUR(1000),LPROB(1000) ! REAL before 20030422
      REAL ENRGCT
!      TYPE DECLARATION FOR /WORK/
      real*8 WLPROB(1000,2),WLOADS(1000,2) ! REAL before 20030422
      INTEGER*2 CURRNT,LDCPTS,HOURS
      INTEGER*2 NPEAKSALE,PEAK_SALE_INDEX(MAX_EL_UNITS), &
                PEAK_SALE_GROUP(MAX_EL_UNITS),LPSALE, &
                P_OR_G
      real*8 IPEAK_SALE_CAPACITY(MAX_EL_UNITS), & 
             IPEAK_SALE_ENERGY(MAX_EL_UNITS)
      REAL  FIXED_PEAK_SALE_COST(MAX_EL_UNITS), &
            ANNUAL_FIXED_PEAK_SALE_COST(MAX_EL_UNITS), &
            VAR_PEAK_SALE_COST(MAX_EL_UNITS), &
            PEAK_SALE_SO2(MAX_EL_UNITS), &
            PEAK_SALE_COST_ALLOCATION(MAX_EL_UNITS)
      CHARACTER*1 PEAK_SALE_ASSIGNMENT(MAX_EL_UNITS), &
                  PEAK_SALE_COLLECTION(MAX_EL_UNITS)
!      copy values from caller's REAL*4 variables to local REAL*8
      RUN_TRANSACT = YES_RUN_TRANSACT()
      do I=1,1000
         LPROB (I)=dble(LPROB4 (I))
         LODDUR(I)=dble(LODDUR4(I))
      end do
      RR_PEAK_ENRG = 0.
      RR_OFPEAK_ENRG = 0.
      ENERGY_DIF = 0.
      EL_SO2_PERIOD = 0.
      SEASONAL_ENRG_LIMITED_CAPACITY = 0
      ENRG1 = DEMAND
      ENRG2 = ENRG1
      CALL SET_EL_POOLING_HOURS(HOURS)
      PROD_PERIODS = get_PRODUCTION_PERIODS_in()
      IF(get_ENRG_LIMIT_REPORT()) THEN
         IF(PERIOD_COUNTER == 1 ) THEN
            ANNUAL_PEAK1 = 0
            ANNUAL_PEAK2 = 0
            ANNUAL_BASE1 = 999000
            ANNUAL_BASE2 = 999000
            ANNUAL_BEFORE_ENRG = 0.
            ANNUAL_AFTER_ENRG =  0.
            ANNUAL_DEFERENCE_ENRG = 0.
         ENDIF
         MONTHLY_BEFORE_PEAK(ISEAS) = 0
         MONTHLY_AFTER_PEAK(ISEAS) = 0
         MONTHLY_BEFORE_BASE(ISEAS) = 0
         MONTHLY_AFTER_BASE(ISEAS) = 0
         MONTHLY_BEFORE_ENERGY(ISEAS) = 0
         MONTHLY_AFTER_ENERGY(ISEAS) = 0
         MONTHLY_ENERGY_COSTS(ISEAS) = 0
      ENDIF
      SALES_ENERGY = 0. D0
      PEAK1 = PEAK
      BASE1 = LODDUR(1)
      CURRNT = 1
      DO I = 0, MAX_REPORTING_GROUPS
         SEASON_CAPACITY(I) = 0
      ENDDO
      CALL HYDRIN(ENRGCT,PHVCST,NPUMP,NPEAK,PEAKCP,IPUMCP,MINCAP, &
               PEAKEN, &
               IPUMEN,SEASONAL_ENRG_LIMITED_CAPACITY, &
               HYDRO_MONTHLY_ENRG,HYDROCAPMO,P_FIXED_COST, &
               VAR_STORAGE_COST,FIXED_STORAGE_COST, &
               PROD_PERIODS,STORAGE_COLLECTION,STORAGE_ASSIGNMENT, &
               PEAK_COLLECTION,PEAK_ASSIGNMENT,HOURS,SALES_ENERGY, &
               SALES_ENERGY_NOT_IN_FORECAST, &
               ENRG_LIMITED_PURCHASE_ENERGY, &
               PEAK_COST_ALLOCATION,OFF_PEAK_COST_ALLOCATION, &
               EL_SO2,EL_SO2_PERIOD,PEAK_GROUP,PUMP_GROUP, &
               SEASON_CAPACITY,EL_SO2_EMIS,PEAK_SO2,PUMP_SO2, &
               BTU_TAX_ACTIVE,ANNUAL_EL_MWH_H12, &
               ANNUAL_EL_MWH_FOR_BTU_TAX_H12, &
               PEAK_INDEX,OFF_PEAK_INDEX,P_ANNUAL_FIXED_COST, &
               ANNUAL_FIXED_STORAGE_COST, &
               NPEAKSALE, &
               PEAK_SALE_INDEX,PEAK_SALE_GROUP,IPEAK_SALE_CAPACITY, &
               IPEAK_SALE_ENERGY,ANNUAL_FIXED_PEAK_SALE_COST, &
               PEAK_SALE_COLLECTION, &
               FIXED_PEAK_SALE_COST, &
               VAR_PEAK_SALE_COST,PEAK_SALE_SO2, &
               PEAK_SALE_COST_ALLOCATION,PEAK_SALE_ASSIGNMENT,YR)
      IF(MINCAP == 0. .AND. NPEAK == 0 .AND. &
                            NPUMP == 0 .AND. NPEAKSALE == 0) RETURN
! MOVED 5/13/99. GAT.
      CALL DOES_TF_FILE_EXIST(TF_FILE_EXISTS)
      TRANS_HYDRO = YES_USE_TRANSACT_LOADS() .AND. TF_FILE_EXISTS
!
      IF(TRANS_HYDRO) THEN
         CALL GET_TARGET_TRANS_GROUP(TARGET_TRANS_GROUP)
         TARGET_TRANS_GROUP = &
                           GET_TRANS_GROUP_POSITION(TARGET_TRANS_GROUP)
      ENDIF
!
!  PEAK SALE USING PUMP LOGIC
      IF(NPEAKSALE > 0) THEN
         DO LPSALE = 1, NPEAKSALE
            UNUSED_ENRG = .FALSE.
            IF(IPEAK_SALE_CAPACITY(LPSALE) < & !  6/5/97. GAT.
                                   .95*(LODDUR(LDCPTS)-LODDUR(1))) THEN
               IF(IPEAK_SALE_ENERGY(LPSALE) /= 0. .AND. &
                                IPEAK_SALE_CAPACITY(LPSALE) /= 0.) THEN
                  CALL PEAK_SALE(IPEAK_SALE_ENERGY(LPSALE), &
                                 IPEAK_SALE_CAPACITY(LPSALE), &
                                 LPROB,LODDUR,HOURS,LDCPTS)
                  DOUBLE_BASE_ENRG = DBLE(LODDUR(1))
                  CALL Integrate(ENRG2,LODDUR, &
                           LPROB,LDCPTS,HOURS,DOUBLE_BASE_ENRG)
                  ENERGY_DIF = SNGL(ENRG2 - ENRG1)
                  IF(UNUSED_ENRG) THEN
                     ENERGY_DIF = IPEAK_SALE_ENERGY(LPSALE)
                     RR_OFPEAK_ENRG = RR_OFPEAK_ENRG + &
                        ( IPEAK_SALE_ENERGY(LPSALE) - (ENRG2 - ENRG1) )
                  ENDIF
               ELSE
                  ENERGY_DIF = 0.
               ENDIF
            ELSE
               ENERGY_DIF = IPEAK_SALE_ENERGY(LPSALE)
               RR_OFPEAK_ENRG = + RR_OFPEAK_ENRG + &
                                              IPEAK_SALE_ENERGY(LPSALE)
            ENDIF
            L = INDEX('ABNX',(HYDRO_EXPENSE_COLLECTION(LPSALE)))
            IF(L == 0) L = 3
!  ADDED 6/15/91 FOR MIDAS GOLD
            RTEMP_VARCST = ENERGY_DIF * VAR_PEAK_SALE_COST(LPSALE)
            RTEMP_FIXED_COST = 1000. * (IPEAK_SALE_CAPACITY(LPSALE) * &
                                       FIXED_PEAK_SALE_COST(LPSALE) + &
                        1000. * ANNUAL_FIXED_PEAK_SALE_COST(LPSALE))/ &
                                         FLOAT(PROD_PERIODS)
            CALL EL_POOL_COSTS_AND_UNIT_REPORTS( &
                               PEAK_SALE_INDEX(LPSALE), &
                               RTEMP_VARCST,RTEMP_FIXED_COST, &
                               ENERGY_DIF, &
                               PEAK_SALE_COST_ALLOCATION(LPSALE), &
                               PEAK_SALE_ASSIGNMENT(LPSALE), &
                               sngl(IPEAK_SALE_CAPACITY(LPSALE)), &
                               PEAK_SALE_GROUP(LPSALE), &
                               SEASON_CAPACITY,PEAK_SALE_SO2(LPSALE))
            SALES_ENERGY = SALES_ENERGY + DBLE(ENERGY_DIF)
            SALES_ENERGY_NOT_IN_FORECAST = DBLE(ENERGY_DIF) + &
                                           SALES_ENERGY_NOT_IN_FORECAST
            IF(PEAK_SALE_ASSIGNMENT(LPSALE) == 'R') THEN
               ENRG_LIMITED_SALES_REVENUE(L) = RTEMP_VARCST + &
                       RTEMP_FIXED_COST + ENRG_LIMITED_SALES_REVENUE(L)
            ELSE
               IF(PEAK_SALE_ASSIGNMENT(LPSALE) == 'E') THEN
                ENRG_LIMITED_VAR_COST(L) = ENRG_LIMITED_VAR_COST(L) - &
                                                           RTEMP_VARCST
               ENRG_LIMITED_FIXED_COST(L)=ENRG_LIMITED_FIXED_COST(L)- &
                                                       RTEMP_FIXED_COST
               ENDIF
               IF(PEAK_SALE_ASSIGNMENT(LPSALE) == 'P') THEN
                  ENRG_LIMITED_PURCHASE_EXPENSE(L) = - RTEMP_VARCST - &
                                        RTEMP_FIXED_COST + &
                                       ENRG_LIMITED_PURCHASE_EXPENSE(L)
               ENDIF
            ENDIF
            ENRG1 = ENRG2
         ENDDO
      ENDIF
!      Initialize the working arrays in reverse order.
!      MOVED ABOVE PEAK SALE. 6/5/97. GAT.
      ORG_LDCPTS = LDCPTS
      ITEMP  = LDCPTS + 1
!      ENRG1  = DEMAND
      DO I = 1, ORG_LDCPTS
         J = ITEMP - I
         WLPROB(I,1) = LPROB(J)
         WLOADS(I,1) = LODDUR(J)
      ENDDO
!      Reduce the peak load and energy if peaking units are available
      IF(NPEAK > 0) THEN
         LPEAK = 1
         DOWHILE(LPEAK <= NPEAK)
!         DO LPEAK = 1, NPEAK
!  10/18/93. GAT. ADDITIONAL PEAK SHIFTING LOGIC
            CAPACITY_FOR_PKSHIF = PEAKCP(LPEAK)
            ENERGY_FOR_PKSHIF = PEAKEN(LPEAK)
            START_LPEAK = LPEAK
            DO
               IF(CAPACITY_FOR_PKSHIF >= DX .OR. LPEAK == NPEAK) EXIT
!
               LPEAK = LPEAK + 1
              CAPACITY_FOR_PKSHIF = CAPACITY_FOR_PKSHIF + PEAKCP(LPEAK)
               ENERGY_FOR_PKSHIF = ENERGY_FOR_PKSHIF + PEAKEN(LPEAK)
               CYCLE
            ENDDO
            END_LPEAK = LPEAK
            IF(CAPACITY_FOR_PKSHIF < &
                .9 * (ENRG2 - FLOAT(HOURS)*WLOADS(LDCPTS,CURRNT))) THEN
!
               IF( CAPACITY_FOR_PKSHIF > &
                     .9*(WLOADS(1,CURRNT)-WLOADS(LDCPTS,CURRNT))) THEN
                  CAPACITY_FOR_PKSHIF = &
                        .9*(WLOADS(1,CURRNT)-WLOADS(LDCPTS,CURRNT))
               ENDIF
!
               IF(ENERGY_FOR_PKSHIF /= 0. .AND. &
                                       CAPACITY_FOR_PKSHIF /= 0.) THEN
                  CALL PKSHIF(ENERGY_FOR_PKSHIF,CAPACITY_FOR_PKSHIF, &
                                   WLPROB,WLOADS,CURRNT,HOURS,LDCPTS, &
                                    ISEAS, &
                                    EL_UNIT_NAME(PEAK_INDEX(LPEAK)))
                  DOUBLE_BASE_ENRG = WLOADS(LDCPTS,CURRNT)
                  CALL Integrate(ENRG2,WLOADS(1,CURRNT), &
                           WLPROB(1,CURRNT),LDCPTS,HOURS, &
                                                      DOUBLE_BASE_ENRG)
                  IF(ENRG1 > ENRG2) THEN
                     ENERGY_DIF = SNGL(ENRG1 - ENRG2)
                  ELSE
                     ENRG2 = ENRG1 -  ENERGY_FOR_PKSHIF
                     ENERGY_DIF = ENERGY_FOR_PKSHIF
                     RR_PEAK_ENRG = + RR_PEAK_ENRG + ENERGY_FOR_PKSHIF
                  ENDIF
               ELSE
                  ENERGY_DIF = 0.
               ENDIF
            ELSE
               ENRG2 = ENRG1 -  ENERGY_FOR_PKSHIF
               ENERGY_DIF = ENERGY_FOR_PKSHIF
               RR_PEAK_ENRG = + RR_PEAK_ENRG + ENERGY_FOR_PKSHIF
            ENDIF
! 031507.
           IF(HYTYPE(PEAK_INDEX(LPEAK)) == 'S' .AND. RUN_TRANSACT) THEN
               P_OR_G = 1
               TEMP_R4 = LDC_2_HOURS( &
                           WLPROB, WLOADS, ENRG2, &
                            & !  R8 DEMAND AFTER EL
                            & !  R8 ENERGY OF RESOURCE
                           ENERGY_FOR_PKSHIF, & 
                           & !  R8 CAPACITY OF RESOURCE
                           CAPACITY_FOR_PKSHIF,& 
                           & ! R4 ??? MUST CA  !  INSIDE ROUTINE
                           PEAK_AFTER_EL4, & 
                           ISEAS, & 
                           HOURS, &  !  INDEXED TRANS GROUP
                         TARGET_TRANS_GROUP,  & !  NUMBER OF LOAD POINTS
                           LDCPTS, & 
                           P_OR_G,&
                           CURRNT)
            ENDIF
!
            DO JPEAK = START_LPEAK, END_LPEAK
               JPEAK_ENERGY = MIN(PEAKEN(JPEAK),ENERGY_DIF)
               RTEMP_VARCST = JPEAK_ENERGY * PHVCST(JPEAK)
               RTEMP_FIXED_COST = &
                            1000.*(PEAKCP(JPEAK)*P_FIXED_COST(JPEAK)+ &
                                 1000. * P_ANNUAL_FIXED_COST(JPEAK))/ &
                                              FLOAT(PROD_PERIODS)
               CALL EL_POOL_COSTS_AND_UNIT_REPORTS( &
                               PEAK_INDEX(JPEAK), &
                               RTEMP_VARCST,RTEMP_FIXED_COST, &
                               JPEAK_ENERGY, &
                               PEAK_COST_ALLOCATION(JPEAK), &
                               PEAK_ASSIGNMENT(JPEAK), &
                               sngl(PEAKCP(JPEAK)),PEAK_GROUP(JPEAK), &
                               SEASON_CAPACITY,PEAK_SO2(JPEAK))
               L = INDEX('ABNX',(HYDRO_EXPENSE_COLLECTION(JPEAK)))
               IF(L == 0) L = 3
               IF(PEAK_ASSIGNMENT(JPEAK) == 'P') THEN
                  ENRG_LIMITED_PURCHASE_EXPENSE(L) = RTEMP_VARCST + &
                    RTEMP_FIXED_COST + ENRG_LIMITED_PURCHASE_EXPENSE(L)
                  ENRG_LIMITED_PURCHASE_ENERGY = DBLE(JPEAK_ENERGY) + &
                                         ENRG_LIMITED_PURCHASE_ENERGY
               ELSE
                  ENRG_LIMITED_ENRG = ENRG_LIMITED_ENRG + JPEAK_ENERGY
                  ENRGCT = ENRGCT + RTEMP_VARCST + RTEMP_FIXED_COST
                  IF(PEAK_ASSIGNMENT(JPEAK) == 'E') THEN
                     ENRG_LIMITED_VAR_COST(L) = &
                                           ENRG_LIMITED_VAR_COST(L) + &
                                                           RTEMP_VARCST
                     ENRG_LIMITED_FIXED_COST(L) = &
                                          ENRG_LIMITED_FIXED_COST(L)+ &
                                                       RTEMP_FIXED_COST
                  ELSEIF(PEAK_ASSIGNMENT(JPEAK) == 'R') THEN
                     ENRG_LIMITED_SALES_REVENUE(L) = -RTEMP_VARCST - &
                       RTEMP_FIXED_COST + ENRG_LIMITED_SALES_REVENUE(L)
                  ENDIF
               ENDIF
               ENERGY_DIF = MAX(0.,ENERGY_DIF-PEAKEN(JPEAK))
            ENDDO ! JPEAK
            ENRG1 = ENRG2
            LPEAK = LPEAK + 1
         ENDDO
      ENDIF
!      Add to off-peak load and energy if these units are available
!      Flip the working arrays around again so that peak is now in
!      position 1 and base is in position LDCPTS.
! 082519.
      ENRG1 = ENRG1 + RR_PEAK_ENRG
!
      NEXT = MOD(CURRNT,2) + 1
      ITEMP = LDCPTS + 1
      DO I = 1, LDCPTS
         J = ITEMP - I
         WLPROB(I,NEXT) = WLPROB(J,CURRNT)
         WLOADS(I,NEXT) = WLOADS(J,CURRNT)
      ENDDO
      DO I = ITEMP, MAX(ORG_LDCPTS,LDCPTS)+20
         WLPROB(I,NEXT) = 0.
         WLOADS(I,NEXT) = WLOADS(I-1,NEXT) + DX
      ENDDO
      CURRNT = NEXT
      IF(NPUMP > 0) THEN
         DO LPUMP = 1, NPUMP
            UNUSED_ENRG = .FALSE.
            IF(IPUMCP(LPUMP) < &
                     .9*(WLOADS(LDCPTS,CURRNT)-WLOADS(1,CURRNT))) THEN
               IF(IPUMEN(LPUMP) /= 0. .AND. &
                                           IPUMCP(LPUMP) /= 0.) THEN
                  CALL OFPEAK(IPUMEN(LPUMP),IPUMCP(LPUMP),WLPROB, &
                         WLOADS,CURRNT,HOURS,LDCPTS,DX,UNUSED_ENRG)
                  DOUBLE_BASE_ENRG = WLOADS(1,CURRNT)
                  CALL Integrate(ENRG2,WLOADS(1,CURRNT), &
                           WLPROB(1,CURRNT),LDCPTS,HOURS, &
                                                      DOUBLE_BASE_ENRG)
                  IF(ENRG1 < ENRG2) THEN
                     ENERGY_DIF = SNGL(ENRG2 - ENRG1)
                  ELSE
                     ENERGY_DIF = IPUMEN(LPUMP)
                     RR_OFPEAK_ENRG = RR_OFPEAK_ENRG + &
                        ( IPUMEN(LPUMP) - (ENRG2 - ENRG1) )
                  ENDIF
               ELSE
                  ENERGY_DIF = 0.
               ENDIF
            ELSE
!                ENRG2 = ENRG1 + IPUMEN(LPUMP)
               ENERGY_DIF = IPUMEN(LPUMP)
               RR_OFPEAK_ENRG = + RR_OFPEAK_ENRG + IPUMEN(LPUMP)
            ENDIF
            L = INDEX('ABNX',(HYDRO_EXPENSE_COLLECTION(LPUMP)))
            IF(L == 0) L = 3
! 031507.
            IF(HYTYPE(OFF_PEAK_INDEX(LPUMP)) == 'S' .AND. &
                                                     RUN_TRANSACT) THEN
               P_OR_G = 2
               TEMP_R4 = LDC_2_HOURS( &
                           WLPROB,               & !  WLPROB
                           WLOADS,               & !  WLODDUR
                           ENRG2, & !  R8 DEMAND AFTER EL
                          ENERGY_FOR_PKSHIF, & !  R8 ENERGY OF RESOURCE
                     CAPACITY_FOR_PKSHIF, & !  R8 CAPACITY OF RESOURCE
              PEAK_AFTER_EL4, &   ! R4 ??? MUST CA & !  INSIDE ROUTINE
                           ISEAS, & !  el_MONTH
                           HOURS,  & !  HOUR IN el_MONTH
                           TARGET_TRANS_GROUP, & !  INDICED TRANS GROUP
                           LDCPTS, & !  NUMBER OF LOAD POINTS
                           P_OR_G, &
                           CURRNT)
            ENDIF
!  ADDED 6/15/91 FOR MIDAS GOLD
            RTEMP_VARCST = ENERGY_DIF * VAR_STORAGE_COST(LPUMP)
            RTEMP_FIXED_COST = 1000. * (IPUMCP(LPUMP) * &
                                          FIXED_STORAGE_COST(LPUMP) + &
                           1000. * ANNUAL_FIXED_STORAGE_COST(LPUMP))/ &
                                         FLOAT(PROD_PERIODS)
           CALL EL_POOL_COSTS_AND_UNIT_REPORTS(OFF_PEAK_INDEX(LPUMP), &
                               RTEMP_VARCST,RTEMP_FIXED_COST, &
                               ENERGY_DIF, &
                               OFF_PEAK_COST_ALLOCATION(LPUMP), &
                               STORAGE_ASSIGNMENT(LPUMP), &
                               sngl(IPUMCP(LPUMP)),PUMP_GROUP(LPUMP), &
                               SEASON_CAPACITY,PUMP_SO2(LPUMP))
            IF(HYTYPE(OFF_PEAK_INDEX(LPUMP)) == 'S') THEN
               IF(STORAGE_ASSIGNMENT(LPUMP) == 'P') THEN
                  SALES_ENERGY = SALES_ENERGY + DBLE(ENERGY_DIF)
                  SALES_ENERGY_NOT_IN_FORECAST = DBLE(ENERGY_DIF) + &
                                           SALES_ENERGY_NOT_IN_FORECAST
                  ENRG_LIMITED_PURCHASE_EXPENSE(L) = RTEMP_VARCST + &
                                                   RTEMP_FIXED_COST + &
                                       ENRG_LIMITED_PURCHASE_EXPENSE(L)
               ELSE
                  STORAGE_ENRG = STORAGE_ENRG + ENERGY_DIF
                  IF(STORAGE_ASSIGNMENT(LPUMP) == 'R') THEN
                     ENRG_LIMITED_SALES_REVENUE(L) = RTEMP_VARCST + &
                       RTEMP_FIXED_COST + ENRG_LIMITED_SALES_REVENUE(L)
                  ELSEIF(STORAGE_ASSIGNMENT(LPUMP) == 'E') THEN
                  ENRG_LIMITED_VAR_COST(L)=ENRG_LIMITED_VAR_COST(L) - &
                                                           RTEMP_VARCST
                     ENRG_LIMITED_FIXED_COST(L) = &
                          ENRG_LIMITED_FIXED_COST(L) - RTEMP_FIXED_COST
                  ENDIF
               ENDIF
!  NEXT PEAK AND VALLEY OFF SYSTEM SALES
!
            ELSE
               SALES_ENERGY = SALES_ENERGY + DBLE(ENERGY_DIF)
               SALES_ENERGY_NOT_IN_FORECAST = DBLE(ENERGY_DIF) + &
                                           SALES_ENERGY_NOT_IN_FORECAST
!                RTEMP_VARCST = ABS(RTEMP_VARCST)
!                RTEMP_FIXED_COST = ABS(RTEMP_FIXED_COST)
               IF(STORAGE_ASSIGNMENT(LPUMP) == 'R') THEN
                  ENRG_LIMITED_SALES_REVENUE(L) = RTEMP_VARCST + &
                       RTEMP_FIXED_COST + ENRG_LIMITED_SALES_REVENUE(L)
               ELSE
                  IF(STORAGE_ASSIGNMENT(LPUMP) == 'E') THEN
                  ENRG_LIMITED_VAR_COST(L)=ENRG_LIMITED_VAR_COST(L) - &
                                                           RTEMP_VARCST
                     ENRG_LIMITED_FIXED_COST(L) = &
                          ENRG_LIMITED_FIXED_COST(L) - RTEMP_FIXED_COST
                  ENDIF
                  IF(STORAGE_ASSIGNMENT(LPUMP) == 'P') THEN
                  ENRG_LIMITED_PURCHASE_EXPENSE(L) = - RTEMP_VARCST - &
                                        RTEMP_FIXED_COST + &
                                       ENRG_LIMITED_PURCHASE_EXPENSE(L)
                  ENDIF
               ENDIF
            ENDIF
            ENRG1 = ENRG2
         ENDDO
      ENDIF
      MINCAP = (MINCAP - RR_OFPEAK_ENRG + RR_PEAK_ENRG) / FLOAT(HOURS)
      IF(TRANS_HYDRO) THEN
         TRANS_ROR_CAPACITY_EN(TARGET_TRANS_GROUP) = MINCAP
         MINCAP = 0.
      ENDIF
!      ELSE
         DO I = 1, LDCPTS
            LPROB(I)  = WLPROB(I,CURRNT)
            LODDUR(I) = WLOADS(I,CURRNT) - MINCAP
            IF((I > 1) .AND. (LODDUR(I-1) >= LODDUR(I))) &
                                           LODDUR(I) = LODDUR(I) + .1
         ENDDO
!      ENDIF
!      WARN IF UNITS WERE CONVERTED TO RR
      IF( RR_PEAK_ENRG /= 0. .OR. RR_OFPEAK_ENRG /= 0.) THEN
         WRITE(4,*) 'For end point',gc_end_point, &
                 ' in production period ',ISEAS,', ',get_BASE_YEAR()+YR
         WRITE(4,*) 'one or more energy limited units dispatched as'
         IF( RR_PEAK_ENRG /= 0. .AND. RR_OFPEAK_ENRG /= 0.) THEN
            WRITE(4,*) 'peaking and off system sales'
         ELSE IF(RR_OFPEAK_ENRG /= 0.) THEN
            WRITE(4,*) 'off system sales'
         ELSE
            WRITE(4,*) 'peaking'
         ENDIF
         WRITE(4,"('&',A//)") ' was reassigned as run of river.'
         WRITE(4,*) 'Check the difference between Peak and Base'
         WRITE(4,*) 'after load modification in the detailed'
         WRITE(4,*) 'energy limited report.'
         WRITE(4,*) ' '
      ENDIF
      PEAK2 = LODDUR(LDCPTS)
      BASE2 = LODDUR(1)
      ENRG1 = ENRG1 - DBLE(MINCAP*FLOAT(HOURS))
      PEAK = PEAK2
      IF(get_ENRG_LIMIT_REPORT()) THEN
         ANNUAL_PEAK1 = MAX(ANNUAL_PEAK1,PEAK1)
         ANNUAL_PEAK2 = MAX(ANNUAL_PEAK2,PEAK2)
         ANNUAL_BASE1 = MIN(ANNUAL_BASE1,BASE1)
         ANNUAL_BASE2 = MIN(ANNUAL_BASE2,BASE2)
         ANNUAL_BEFORE_ENRG = ANNUAL_BEFORE_ENRG + IDINT(DEMAND)
         ANNUAL_AFTER_ENRG = ANNUAL_AFTER_ENRG + IDINT(ENRG1)
         ANNUAL_DEFERENCE_ENRG = ANNUAL_DEFERENCE_ENRG + &
                                         IDINT(DEMAND) - IDINT(ENRG1)
         MONTHLY_BEFORE_PEAK(ISEAS) = PEAK1
         MONTHLY_AFTER_PEAK(ISEAS) = PEAK2
         MONTHLY_BEFORE_BASE(ISEAS) = BASE1
         MONTHLY_AFTER_BASE(ISEAS) = BASE2
         MONTHLY_BEFORE_ENERGY(ISEAS) =IDNINT(DEMAND/1000.)
         MONTHLY_AFTER_ENERGY(ISEAS) = IDNINT(ENRG1/1000.)
         MONTHLY_ENERGY_COSTS(ISEAS) = NINT(ENRGCT)
      ENDIF
      DEMAND = ENRG1
      IF(ENRG_LIMITED_CAPACITY < SEASONAL_ENRG_LIMITED_CAPACITY) THEN
         DO I = 0, MAX_REPORTING_GROUPS
            ANN_EL_CAPACITY(I) = SEASON_CAPACITY(I)
         ENDDO
      ENDIF
      do I=1,1000
         LPROB4 (I)=sngl(LPROB (I))
         LODDUR4(I)=sngl(LODDUR(I))
      end do
      RETURN ! SUBROUTINE ENRGLIMT
! ADDED 5/27/98. GAT. FOR WSCC.

      ENTRY INIT_TRANS_ROR_CAPACITY(R_NUMBER_OF_TRANS_GROUPS)
         if(R_NUMBER_OF_TRANS_GROUPS==0) then
            stop "r_number_of_trans_groups=0"
         end if
         SAVE_NUMBER_OF_TRANS_GROUPS_EN = R_NUMBER_OF_TRANS_GROUPS
         if(SAVE_NUMBER_OF_TRANS_GROUPS_EN==0) then
            stop "After setting, SAVE_NUMBER_OF_TRANS_GROUPS is zero."
         end if

         IF( ALLOCATED(TRANS_ROR_CAPACITY_EN) ) &
             DEALLOCATE(TRANS_ROR_CAPACITY_EN, &
                     TRANS_PEAK_CAPACITY,TRANS_PUMP_CAPACITY)
! 12/15/99. GAT. CHANGED PER SRP.
         ALLOCATE(TRANS_ROR_CAPACITY_EN(0:R_NUMBER_OF_TRANS_GROUPS))
         ALLOCATE(TRANS_PEAK_CAPACITY(0:R_NUMBER_OF_TRANS_GROUPS))
         ALLOCATE(TRANS_PUMP_CAPACITY(0:R_NUMBER_OF_TRANS_GROUPS))
         TRANS_ROR_CAPACITY_EN = 0.
         TRANS_PEAK_CAPACITY = 0.
         TRANS_PUMP_CAPACITY = 0.
      RETURN




      ENTRY GET_ONE_TRANS_ROR_CAP(R_TRANS_GROUP, &
                              arg_R_TRANS_GROUP_CAPACITY)

         IF(SAVE_NUMBER_OF_TRANS_GROUPS_EN < &
                                          R_TRANS_GROUP) THEN
            WRITE(4,*) "During Multi-area simulation, the model"
            WRITE(4,*) "detected a mismatch in the number of"
            WRITE(4,*) "active Transaction Groups"
            WRITE(4,*) "within dispatchable hydro. (2)"
            stop "trans groups 2"
         ENDIF
         
         ! TODO: This allocation check is for the existence of a feature.
         ! Come up with a boolean check and limit allocation checks to
         ! where allocation is the only question to be answered.
         IF(ALLOCATED(TRANS_ROR_CAPACITY_EN)) THEN
            arg_R_TRANS_GROUP_CAPACITY = &
                TRANS_ROR_CAPACITY_EN(R_TRANS_GROUP)
         ELSE
            arg_R_TRANS_GROUP_CAPACITY = 0.
         ENDIF
      RETURN

     ENTRY GET_TRANS_PEAK_CAPACITY(R_NUMBER_OF_TRANS_GROUPS,R_CAPACITY)

         IF(SAVE_NUMBER_OF_TRANS_GROUPS_EN /= &
                                         R_NUMBER_OF_TRANS_GROUPS) THEN
            WRITE(4,*) "During Multi-area simulation, the model"
            WRITE(4,*) "detected a mismatch in the number of"
            WRITE(4,*) "active Transaction Groups"
            WRITE(4,*) "within dispatchable hydro. (3)"
            stop "trans groups 3"
         ENDIF
         DO I = 0, R_NUMBER_OF_TRANS_GROUPS
            R_CAPACITY(I) = TRANS_PEAK_CAPACITY(I)
         ENDDO
      RETURN

     ENTRY GET_TRANS_PUMP_CAPACITY(R_NUMBER_OF_TRANS_GROUPS,R_CAPACITY)

         IF(SAVE_NUMBER_OF_TRANS_GROUPS_EN /= &
                                         R_NUMBER_OF_TRANS_GROUPS) THEN
            WRITE(4,*) "During Multi-area simulation, the model"
            WRITE(4,*) "detected a mismatch in the number of"
            WRITE(4,*) "active Transaction Groups"
            WRITE(4,*) "within dispatchable hydro. (4)"
            stop "trans groups 4"
            ! See traceback - transobj:6693 from transobj:1667.
            
         ENDIF
         DO I = 0, R_NUMBER_OF_TRANS_GROUPS
            R_CAPACITY(I) = TRANS_PUMP_CAPACITY(I)
         ENDDO
      RETURN
      END subroutine enrglimt


!
!                        ROUTINE HYDRIN
!          COPYRIGHT (C) 1981 M.S. GERBER & ASSOCIATES, INC.
!                       ALL RIGHTS RESERVED
!

!
!      PURPOSE:
!         TO CALCULATE THE            C
!         INFORMATION NECESSARY FOR THE LOAD-CLIPPING PROGRAM
!         FROM A HYDRO FILE.
!

      SUBROUTINE HYDRIN(ENRGCT,PHVCST,NPUMP,NPEAK,PEAKCP, &
             IPUMCP,MINCAP,PEAKEN,IPUMEN, &
             SEASONAL_ENRG_LIMITED_CAPACITY,HYDRO_MONTHLY_ENRG, &
             HYDROCAPMO,P_FIXED_COST, &
             VAR_STORAGE_COST,FIXED_STORAGE_COST,PROD_PERIODS, &
             STORAGE_COLLECTION,STORAGE_ASSIGNMENT, &
             PEAK_COLLECTION,PEAK_ASSIGNMENT,HOURS,SALES_ENERGY, &
           SALES_ENERGY_NOT_IN_FORECAST,ENRG_LIMITED_PURCHASE_ENERGY, &
             PEAK_COST_ALLOCATION,OFF_PEAK_COST_ALLOCATION, &
          EL_SO2,EL_SO2_PERIOD,PEAK_GROUP,PUMP_GROUP,SEASON_CAPACITY, &
             EL_SO2_EMIS,PEAK_SO2,PUMP_SO2, &
             BTU_TAX_ACTIVE, &
             ANNUAL_EL_MWH_H12, &
             ANNUAL_EL_MWH_FOR_BTU_TAX_H12, &
             PEAK_INDEX,OFF_PEAK_INDEX, &
             P_ANNUAL_FIXED_COST,ANNUAL_FIXED_STORAGE_COST, &
             NPEAKSALE, &
             PEAK_SALE_INDEX,PEAK_SALE_GROUP,IPEAK_SALE_CAPACITY, &
             IPEAK_SALE_ENERGY,ANNUAL_FIXED_PEAK_SALE_COST, &
             PEAK_SALE_COLLECTION, &
             FIXED_PEAK_SALE_COST, &
             VAR_PEAK_SALE_COST,PEAK_SALE_SO2, &
             PEAK_SALE_COST_ALLOCATION,PEAK_SALE_ASSIGNMENT,YR)
      use end_routine, only: end_program, er_message
      use SpinDriftLib
             use prod_arrays_dimensions
      use sizecom
      use foshydi2com
      use prod2com
      use aitvafin
      use aitvaprod
      CHARACTER*1 STORAGE_COLLECTION(*),STORAGE_ASSIGNMENT(*)
      CHARACTER*1 PEAK_COLLECTION(*),PEAK_ASSIGNMENT(*),CHR_TEMP
!  CLINTON'S BTU TAX ADDED 3/14/93
      LOGICAL*1   BTU_TAX_ACTIVE,AN_ENERGY_SALE,TRANS_HYDRO, &
                  TF_FILE_EXISTS, &
                  YES_USE_TRANSACT_LOADS, &
                  YES_USE_TF_FILE_FOR_MULTIAREA, &
                  YES_RUN_MULTIAREA_TRANSACT, &
                  YES_STRICT_MARKET_PRICE, &
                  YES_CENTRAL_DISPATCH_TRANSACT
      REAL*8 ANNUAL_EL_MWH_H12(0:3), &
             ANNUAL_EL_MWH_FOR_BTU_TAX_H12(0:3)
      INTEGER*2 :: M,N,I,J,L,HOURS, &
!  5/20/92 ADDED FOR EL_GROUP; 8/16/92 PUMP_GROUP OUT &
               INT_HELD,PEAK_GROUP(*),PUMP_GROUP(*)
      REAL EL_SO2_EMIS(MAX_EL_UNITS),PEAK_SO2(*),PUMP_SO2(*)
      INTEGER*2 RPT_GROUP,HYDRO_TRANS_GROUP,TARGET_TRANS_GROUP,YR
      INTEGER*2 PEAK_INDEX(*),OFF_PEAK_INDEX(*)
!      INTEGER*2 GROUP
      INTEGER*4 SEASONAL_ENRG_LIMITED_CAPACITY
      REAL SEASON_CAPACITY(0:MAX_REPORTING_GROUPS)
      REAL MWB,VALUEHELD,HYDRO_MONTHLY_ENRG(*),HYDROCAPMO(*), &
           VAR_STORAGE_COST(MAX_EL_UNITS), &
           FIXED_STORAGE_COST(MAX_EL_UNITS), &
           ANNUAL_FIXED_STORAGE_COST(MAX_EL_UNITS), &
           TEMP_ENRG,VAR_COST,UNIT_FIXED_COST,UNIT_ANNUAL_FIXED_COST, &
           PEAK_COST_ALLOCATION(*),OFF_PEAK_COST_ALLOCATION(*),GET_VAR
      REAL*8 SALES_ENERGY,SALES_ENERGY_NOT_IN_FORECAST, &
             ENRG_LIMITED_PURCHASE_ENERGY
!      TYPE DECLARATION FOR /HYDRVL/
      INTEGER*2 NPUMP,NPEAK,PROD_PERIODS,SAVE_PROD_PERIODS,SAVE_HOURS
      SAVE SAVE_PROD_PERIODS,SAVE_HOURS
      REAL EL_SO2(MAX_EL_UNITS),EL_SO2_PERIOD
      real*8 PEAKCP(MAX_EL_UNITS),IPUMCP(MAX_EL_UNITS), & 
             PEAKEN(MAX_EL_UNITS),IPUMEN(MAX_EL_UNITS)
      REAL MINCAP,RTEMP_VARCST,RTEMP_FIXED_COST
!      TYPE DECLARATION FOR /VFCOST/
      REAL ENRGCT,PHVCST(*),P_FIXED_COST(*),P_ANNUAL_FIXED_COST(*)
      INTEGER*2 NPEAKSALE,PEAK_SALE_INDEX(*),PEAK_SALE_GROUP(*)
      real*8 IPEAK_SALE_CAPACITY(*),IPEAK_SALE_ENERGY(*)
      REAL  FIXED_PEAK_SALE_COST(*),ANNUAL_FIXED_PEAK_SALE_COST(*), &
            VAR_PEAK_SALE_COST(*),PEAK_SALE_SO2(*), &
            PEAK_SALE_COST_ALLOCATION(*)
      CHARACTER*1 PEAK_SALE_ASSIGNMENT(*),PEAK_SALE_COLLECTION(*)
!  DYNAMIC PUMPED STORAGE VARIABLES
      INTEGER*2 NDYN_PUMP,DYN_PUMP_INDEX(100),DYN_PUMP_GROUP(100)
      REAL  IDYN_GEN_CAPACITY(100),IDYN_PUMP_ENERGY(100), &
            FIXED_DYN_PUMP_COST(100),ANNUAL_FIXED_DYN_PUMP_COST(100), &
            VAR_DYN_PUMP_COST(100),DYN_PUMP_SO2(100), &
            DYN_PUMP_COST_ALLOCATION(100), &
            VAR_DYN_PUMPCOST(100),FIXED_DYN_PUMPCOST(100), &
            ANNUAL_FIXED_DYN_PUMPCOST(100),DYN_PUMP_PSEFF(100)
      CHARACTER*1 DYN_PUMP_ASSIGNMENT(100),DYN_PUMP_COLLECTION(100)
      SAVE  NDYN_PUMP,DYN_PUMP_INDEX,DYN_PUMP_GROUP, &
            IDYN_GEN_CAPACITY,IDYN_PUMP_ENERGY, &
            FIXED_DYN_PUMP_COST,ANNUAL_FIXED_DYN_PUMP_COST, &
            VAR_DYN_PUMP_COST,DYN_PUMP_SO2, &
            DYN_PUMP_COST_ALLOCATION, &
            DYN_PUMP_ASSIGNMENT,DYN_PUMP_COLLECTION,DYN_PUMP_PSEFF
!
      INTEGER*2   R_NUM_DYN_STORAGE,UnitOwning(*),BlockPos(*),PrtUni, &
                  iAux,iAuxOrg,nVarInAux,PrtDetail,iBlk
      REAL     CFLB(*),CFUB(*),c(*),Capacity(*),EquivAvail(*), &
               SupCost,FacetSysCap,AuxCoeff(*),AuxRHS(*),xReturned, &
               RTEMP_ENERGY,RTEMP_SO2
      CHARACTER*35 Fmt8f

      NPUMP = 0
      NPEAK  = 0
      NPEAKSALE = 0
      MINCAP = 0.
      ENRGCT = 0.
      NDYN_PUMP = 0
      SAVE_PROD_PERIODS = PROD_PERIODS
      SAVE_HOURS = HOURS
      PEAKCP = 0.d0
      IPUMCP = 0.d0
      PEAKEN = 0.d0
      IPUMEN = 0.d0
      VAR_STORAGE_COST = 0.
      EL_SO2_EMIS = 0.
      FIXED_STORAGE_COST = 0.
! THIS LOOKS WRONG. SHOULD ONLY BE INITIALIZED IN FIRST YEAR?
      ANNUAL_FIXED_STORAGE_COST = 0.
      CALL DOES_TF_FILE_EXIST(TF_FILE_EXISTS)
!      TRANS_HYDRO = YES_RUN_MULTIAREA_TRANSACT() .AND. TF_FILE_EXISTS
! 6/23/98. GAT.
      TRANS_HYDRO = TF_FILE_EXISTS .AND. YES_USE_TRANSACT_LOADS()

      IF(TRANS_HYDRO) THEN
         CALL GET_TARGET_TRANS_GROUP(TARGET_TRANS_GROUP)
         IF(TARGET_TRANS_GROUP <= 0) THEN
            WRITE(4,*) "Inconsistent Transact Loads and Energy Limited"
            WRITE(4,*) "resources in HYDRIN"
            er_message='Stop requested from Enrglimt SIID89'
            call end_program(er_message)
         ENDIF
      ENDIF
      DO J = 1, HYDRO_UNITS
         IF(TRANS_HYDRO .AND. &
                      HYDRO_TRANS_GROUP(J) /= TARGET_TRANS_GROUP) CYCLE
!
         AN_ENERGY_SALE = INDEX('OQA',HYTYPE(J)) /= 0
         MWB = ABS(HYDROCAPMO(J) * PCOWN(J))
         HYDRO_MONTHLY_ENRG(J) = ABS(HYDRO_MONTHLY_ENRG(J) * PCOWN(J))
!         IF(ON_LINE(J) > DATE2 .OR. OFF_LINE(J) < DATE1) CYCLE
!         IF(HYDRO_MONTHLY_ENRG(J) == 0. .AND. MWB == 0.) CYCLE
         IF( (ON_LINE(J) > DATE2 .OR. OFF_LINE(J) < DATE1) .OR. &
                    (HYDRO_MONTHLY_ENRG(J) == 0. .AND. MWB == 0.)) THEN
            RTEMP_VARCST = 0.
            RTEMP_FIXED_COST = 0.
            HYDRO_MONTHLY_ENRG(J) = 0.
            MWB = 0.
            EL_SO2_EMIS(J) = 0.
!
            CALL EL_POOL_COSTS_AND_UNIT_REPORTS(J, &
                               RTEMP_VARCST,RTEMP_FIXED_COST, &
                               HYDRO_MONTHLY_ENRG(J), &
                               EL_POOL_FRAC_OWN(J), &
                               HYDRO_EXPENSE_ASSIGNMENT(J), &
                               MWB,RPT_GROUP, &
                               SEASON_CAPACITY,EL_SO2_EMIS(J))
            CYCLE
         ENDIF
!  CLINTON'S ENERGY TAX ADDED 3/14/93 MSG
         IF(HYDRO_MONTHLY_ENRG(J) > 0.) THEN
            L = INDEX('H12',EL_FUEL_TYPE(J))
            IF(BTU_TAX_ACTIVE) ANNUAL_EL_MWH_H12(L) = &
                           ANNUAL_EL_MWH_H12(L) + HYDRO_MONTHLY_ENRG(J)
           ANNUAL_EL_MWH_FOR_BTU_TAX_H12(L) = HYDRO_MONTHLY_ENRG(J) + &
                                       ANNUAL_EL_MWH_FOR_BTU_TAX_H12(L)
         ENDIF
         IF(MWB > 0. .AND. HYDRO_MONTHLY_ENRG(J) >= 0. .AND. &
                        HYDRO_EXPENSE_ASSIGNMENT(J) == 'E' .AND. &
                                             .NOT. AN_ENERGY_SALE) THEN
            SEASONAL_ENRG_LIMITED_CAPACITY = &
                                   SEASONAL_ENRG_LIMITED_CAPACITY + MWB
            EL_AI_INVESTMENT(J) = EL_AI_INVESTMENT(J) + &
                       EL_AI_ENERGY_RATE(J) * HYDRO_MONTHLY_ENRG(J) + &
                                EL_AI_CAPACITY_RATE(J) * MWB * 1000./ &
                                        FLOAT(PROD_PERIODS)
         ENDIF
         IF(AN_ENERGY_SALE) THEN
            EL_SO2_PERIOD = EL_SO2_PERIOD - &
                              EL_SO2(J) * HYDRO_MONTHLY_ENRG(J)
            EL_SO2_EMIS(J) = EL_SO2_EMIS(J) - &
                              EL_SO2(J) * HYDRO_MONTHLY_ENRG(J)
         ELSE
            EL_SO2_PERIOD = EL_SO2_PERIOD + &
                              EL_SO2(J) * HYDRO_MONTHLY_ENRG(J)
            EL_SO2_EMIS(J) = EL_SO2_EMIS(J) + &
                              EL_SO2(J) * HYDRO_MONTHLY_ENRG(J)
         ENDIF
         UNIT_FIXED_COST = HYDRO_FIXED_COST(J)
         UNIT_ANNUAL_FIXED_COST = HYDRO_ANNUAL_FIXED_COST(J)
         VAR_COST = VAROM(J)
         L = INDEX('ABNX',(HYDRO_EXPENSE_COLLECTION(J)))
         IF(L == 0) L = 3

         IF(HYTYPE(J) == 'S') THEN
            NPEAK = NPEAK + 1
            PEAKCP(NPEAK) = MWB
            PEAKEN(NPEAK) = HYDRO_MONTHLY_ENRG(J)
            PHVCST(NPEAK) = VAR_COST
            P_FIXED_COST(NPEAK) = UNIT_FIXED_COST
            P_ANNUAL_FIXED_COST(NPEAK) = UNIT_ANNUAL_FIXED_COST
            PEAK_COLLECTION(NPEAK) = HYDRO_EXPENSE_COLLECTION(J)
            PEAK_ASSIGNMENT(NPEAK) = HYDRO_EXPENSE_ASSIGNMENT(J)
            PEAK_GROUP(NPEAK) = EL_GROUP(J)
            PEAK_SO2(NPEAK) = EL_SO2_EMIS(J)
            PEAK_COST_ALLOCATION(NPEAK) = EL_POOL_FRAC_OWN(J)
            PEAK_INDEX(NPEAK) = J
            IF(PSEFF(J) /= 0.) THEN
               NPUMP = NPUMP + 1
               IF(PSCAP(J) >= 0.) THEN
                  IPUMCP(NPUMP) = PSCAP(J) * PCOWN(J)
               ELSE
               IPUMCP(NPUMP) = GET_VAR(PSCAP(J),YR,EL_UNIT_NAME(J)) * &
                                                               PCOWN(J)
               ENDIF
               STORAGE_COLLECTION(NPUMP) = HYDRO_EXPENSE_COLLECTION(J)
               STORAGE_ASSIGNMENT(NPUMP) = HYDRO_EXPENSE_ASSIGNMENT(J)
               PUMP_GROUP(NPUMP) = -99
               PUMP_SO2(NPUMP) = EL_SO2_EMIS(J)
               IPUMEN(NPUMP) = HYDRO_MONTHLY_ENRG(J) / PSEFF(J)
               OFF_PEAK_COST_ALLOCATION(NPUMP) = EL_POOL_FRAC_OWN(J)
               OFF_PEAK_INDEX(NPUMP) = J
            ENDIF
!   UNCONTROLLED UNITS LIKE ROR UNITS WITH POSITIVE OR NEGATIVE ENERGY
         ELSE IF(HYTYPE(J) == 'U' .OR. HYTYPE(J) == 'B') THEN
            RTEMP_VARCST = HYDRO_MONTHLY_ENRG(J) * VAR_COST
            RTEMP_FIXED_COST = 1000.* (UNIT_FIXED_COST * MWB + &
                                     1000. * UNIT_ANNUAL_FIXED_COST)/ &
                                                   FLOAT(PROD_PERIODS)
            IF(HYDRO_EXPENSE_ASSIGNMENT(J) /= 'R' .OR. &
                                                  EL_GROUP(J) > 0) THEN
               RPT_GROUP = EL_GROUP(J)
            ELSE
               RPT_GROUP = -99
            ENDIF
            CALL EL_POOL_COSTS_AND_UNIT_REPORTS(J, &
                               RTEMP_VARCST,RTEMP_FIXED_COST, &
                               HYDRO_MONTHLY_ENRG(J), &
                               EL_POOL_FRAC_OWN(J), &
                               HYDRO_EXPENSE_ASSIGNMENT(J), &
                               MWB,RPT_GROUP, &
                               SEASON_CAPACITY,EL_SO2_EMIS(J))
            MINCAP = MINCAP + HYDRO_MONTHLY_ENRG(J)
            IF(HYDRO_EXPENSE_ASSIGNMENT(J) == 'P') THEN
               ENRG_LIMITED_PURCHASE_EXPENSE(L) = RTEMP_VARCST + &
                    RTEMP_FIXED_COST + ENRG_LIMITED_PURCHASE_EXPENSE(L)
               ENRG_LIMITED_PURCHASE_ENERGY = &
                                        DBLE(HYDRO_MONTHLY_ENRG(J)) + &
                                         ENRG_LIMITED_PURCHASE_ENERGY
            ELSE
               ENRG_LIMITED_ENRG = ENRG_LIMITED_ENRG + &
                                                  HYDRO_MONTHLY_ENRG(J)
               ENRGCT = ENRGCT + RTEMP_VARCST + RTEMP_FIXED_COST
!                RTEMP_VARCST = ABS(RTEMP_VARCST)
!                RTEMP_FIXED_COST = ABS(RTEMP_FIXED_COST)
               IF(HYDRO_EXPENSE_ASSIGNMENT(J) == 'R') THEN
                  ENRG_LIMITED_SALES_REVENUE(L) = -RTEMP_VARCST - &
                       RTEMP_FIXED_COST + ENRG_LIMITED_SALES_REVENUE(L)
               ELSEIF(HYDRO_EXPENSE_ASSIGNMENT(J) == 'E') THEN
                ENRG_LIMITED_VAR_COST(L) = ENRG_LIMITED_VAR_COST(L) + &
                                                          RTEMP_VARCST
               ENRG_LIMITED_FIXED_COST(L)=ENRG_LIMITED_FIXED_COST(L)+ &
                                                      RTEMP_FIXED_COST
               ENDIF
            ENDIF
!         CONTROLLED UNITS LIKE PONDAGE HYDRO
         ELSE IF(HYTYPE(J) == 'C' .OR. HYTYPE(J) == 'P') THEN
         IF(ABS(HYDRO_MONTHLY_ENRG(J))/FLOAT(HOURS) > 0.75 * MWB .OR. &
                              HYDRO_EXPENSE_ASSIGNMENT(J) == 'R')  THEN
               RTEMP_VARCST = HYDRO_MONTHLY_ENRG(J) * VAR_COST
               RTEMP_FIXED_COST = 1000. * (UNIT_FIXED_COST * MWB + &
                                    1000. * UNIT_ANNUAL_FIXED_COST)/ &
                                                   FLOAT(PROD_PERIODS)
               MINCAP = MINCAP + HYDRO_MONTHLY_ENRG(J)
!                RTEMP_VARCST = ABS(RTEMP_VARCST)
!                RTEMP_FIXED_COST = ABS(RTEMP_FIXED_COST)
               IF(HYDRO_EXPENSE_ASSIGNMENT(J) == 'P') THEN
                  ENRG_LIMITED_PURCHASE_EXPENSE(L) = RTEMP_VARCST + &
                                                   RTEMP_FIXED_COST + &
                                       ENRG_LIMITED_PURCHASE_EXPENSE(L)
                  ENRG_LIMITED_PURCHASE_ENERGY = &
                                        DBLE(HYDRO_MONTHLY_ENRG(J)) + &
                                         ENRG_LIMITED_PURCHASE_ENERGY
               ELSE
                  ENRGCT = ENRGCT + RTEMP_VARCST + RTEMP_FIXED_COST
                  ENRG_LIMITED_ENRG = ENRG_LIMITED_ENRG + &
                                                  HYDRO_MONTHLY_ENRG(J)
                  IF(HYDRO_EXPENSE_ASSIGNMENT(J) == 'E') THEN
                  ENRG_LIMITED_VAR_COST(L)=ENRG_LIMITED_VAR_COST(L) + &
                                                           RTEMP_VARCST
                     ENRG_LIMITED_FIXED_COST(L) = &
                           ENRG_LIMITED_FIXED_COST(L)+ RTEMP_FIXED_COST
                  ELSEIF(HYDRO_EXPENSE_ASSIGNMENT(J) == 'R')  THEN
                     ENRG_LIMITED_SALES_REVENUE(L) = -RTEMP_VARCST - &
                      RTEMP_FIXED_COST + ENRG_LIMITED_SALES_REVENUE(L)
                  ENDIF
               ENDIF
               IF(HYDRO_EXPENSE_ASSIGNMENT(J) /= 'R' .OR. &
                                                 EL_GROUP(J) > 0) THEN
                  RPT_GROUP = EL_GROUP(J)
               ELSE
                  RPT_GROUP = -99
               ENDIF
               CALL EL_POOL_COSTS_AND_UNIT_REPORTS(J, &
                               RTEMP_VARCST,RTEMP_FIXED_COST, &
                               HYDRO_MONTHLY_ENRG(J), &
                               EL_POOL_FRAC_OWN(J), &
                               HYDRO_EXPENSE_ASSIGNMENT(J), &
                               MWB,RPT_GROUP, &
                               SEASON_CAPACITY,EL_SO2_EMIS(J))
            ELSE
               NPEAK = NPEAK + 1
               PEAKCP(NPEAK) = MWB
               PEAKEN(NPEAK) = HYDRO_MONTHLY_ENRG(J)
               PHVCST(NPEAK) = VAR_COST
               P_FIXED_COST(NPEAK) = UNIT_FIXED_COST
               P_ANNUAL_FIXED_COST(NPEAK) = UNIT_ANNUAL_FIXED_COST
               PEAK_COLLECTION(NPEAK) = HYDRO_EXPENSE_COLLECTION(J)
               PEAK_ASSIGNMENT(NPEAK) = HYDRO_EXPENSE_ASSIGNMENT(J)
               PEAK_GROUP(NPEAK) = EL_GROUP(J)
               PEAK_SO2(NPEAK) = EL_SO2_EMIS(J)
               PEAK_COST_ALLOCATION(NPEAK) = EL_POOL_FRAC_OWN(J)
               PEAK_INDEX(NPEAK) = J
            ENDIF
!  ENERGY SALES OPTIONS
         ELSE IF(HYTYPE(J) == 'A') THEN  !RUN OF RIVER OR AVERAGE SALE
            RTEMP_VARCST = HYDRO_MONTHLY_ENRG(J) * VAR_COST
            RTEMP_FIXED_COST = 1000.* (UNIT_FIXED_COST * MWB + &
                                     1000. * UNIT_ANNUAL_FIXED_COST)/ &
                                                    FLOAT(PROD_PERIODS)
            IF(HYDRO_EXPENSE_ASSIGNMENT(J) /= 'R' .OR. &
                                                 EL_GROUP(J) > 0) THEN
               RPT_GROUP = EL_GROUP(J)
            ELSE
               RPT_GROUP = -99
            ENDIF
            CALL EL_POOL_COSTS_AND_UNIT_REPORTS(J, &
                               RTEMP_VARCST,RTEMP_FIXED_COST, &
                               HYDRO_MONTHLY_ENRG(J), &
                               EL_POOL_FRAC_OWN(J), &
                               HYDRO_EXPENSE_ASSIGNMENT(J), &
                               MWB,RPT_GROUP, &
                               SEASON_CAPACITY,EL_SO2_EMIS(J))
            SALES_ENERGY = SALES_ENERGY + DBLE(HYDRO_MONTHLY_ENRG(J))
           SALES_ENERGY_NOT_IN_FORECAST=DBLE(HYDRO_MONTHLY_ENRG(J)) + &
                                           SALES_ENERGY_NOT_IN_FORECAST
            MINCAP = MINCAP - HYDRO_MONTHLY_ENRG(J)
!             RTEMP_VARCST = ABS(RTEMP_VARCST)
!             RTEMP_FIXED_COST = ABS(RTEMP_FIXED_COST)
            IF(HYDRO_EXPENSE_ASSIGNMENT(J) == 'R') THEN
               ENRG_LIMITED_SALES_REVENUE(L) = RTEMP_VARCST + &
                       RTEMP_FIXED_COST + ENRG_LIMITED_SALES_REVENUE(L)
            ELSEIF(HYDRO_EXPENSE_ASSIGNMENT(J) == 'E') THEN
               ENRGCT = ENRGCT - RTEMP_VARCST - RTEMP_FIXED_COST
               ENRG_LIMITED_VAR_COST(L) = ENRG_LIMITED_VAR_COST(L) - &
                                                           RTEMP_VARCST
            ENRG_LIMITED_FIXED_COST(L) = ENRG_LIMITED_FIXED_COST(L) - &
                                                       RTEMP_FIXED_COST
            ELSEIF(HYDRO_EXPENSE_ASSIGNMENT(J) == 'P') THEN
               ENRG_LIMITED_PURCHASE_EXPENSE(L) = -RTEMP_VARCST - &
                    RTEMP_FIXED_COST + ENRG_LIMITED_PURCHASE_EXPENSE(L)
            ENDIF
         ELSE IF(HYTYPE(J) == 'O') THEN
!  FOR OFF SYSTEM SALES
!            MWB = ABS(MWB)
            TEMP_ENRG = ABS(HYDRO_MONTHLY_ENRG(J))
            IF(TEMP_ENRG/FLOAT(HOURS) > 0.95 * MWB) THEN
               MINCAP = MINCAP - TEMP_ENRG
               RTEMP_VARCST = TEMP_ENRG * VAR_COST
               RTEMP_FIXED_COST = 1000. * (UNIT_FIXED_COST * MWB + &
                                     1000. * UNIT_ANNUAL_FIXED_COST)/ &
                                                    FLOAT(PROD_PERIODS)
               IF(EL_GROUP(J) > 0) THEN
                  RPT_GROUP = EL_GROUP(J)
               ELSE
                  RPT_GROUP = -99
               ENDIF
               CALL EL_POOL_COSTS_AND_UNIT_REPORTS(J, &
                                  RTEMP_VARCST,RTEMP_FIXED_COST, &
                                  TEMP_ENRG, &
                                  EL_POOL_FRAC_OWN(J), &
                                  HYDRO_EXPENSE_ASSIGNMENT(J), &
                                  MWB,RPT_GROUP, &
                                  SEASON_CAPACITY,EL_SO2_EMIS(J))
               SALES_ENERGY = SALES_ENERGY + DBLE(TEMP_ENRG)
               SALES_ENERGY_NOT_IN_FORECAST = DBLE(TEMP_ENRG) + &
                                           SALES_ENERGY_NOT_IN_FORECAST
!                RTEMP_VARCST = ABS(RTEMP_VARCST)
!                RTEMP_FIXED_COST = ABS(RTEMP_FIXED_COST)
               IF(HYDRO_EXPENSE_ASSIGNMENT(J) == 'E') THEN
                  ENRGCT = ENRGCT - RTEMP_VARCST - RTEMP_FIXED_COST
                ENRG_LIMITED_VAR_COST(L) = ENRG_LIMITED_VAR_COST(L) - &
                                                           RTEMP_VARCST
               ENRG_LIMITED_FIXED_COST(L)=ENRG_LIMITED_FIXED_COST(L)- &
                                                      RTEMP_FIXED_COST
               ENDIF
               IF(HYDRO_EXPENSE_ASSIGNMENT(J) == 'P') THEN
                  ENRG_LIMITED_PURCHASE_EXPENSE(L) = - RTEMP_VARCST - &
                        RTEMP_FIXED_COST + &
                        ENRG_LIMITED_PURCHASE_EXPENSE(L)
               ENDIF
               IF(HYDRO_EXPENSE_ASSIGNMENT(J) == 'R') THEN
                  ENRG_LIMITED_SALES_REVENUE(L) =  RTEMP_VARCST + &
                       RTEMP_FIXED_COST + ENRG_LIMITED_SALES_REVENUE(L)
               ENDIF
            ELSE
               NPUMP = NPUMP + 1
               IPUMCP(NPUMP) = MWB
               IPUMEN(NPUMP) = TEMP_ENRG
               VAR_STORAGE_COST(NPUMP) = VAR_COST
               FIXED_STORAGE_COST(NPUMP) = UNIT_FIXED_COST
              ANNUAL_FIXED_STORAGE_COST(NPUMP) = UNIT_ANNUAL_FIXED_COST
               STORAGE_COLLECTION(NPUMP) = HYDRO_EXPENSE_COLLECTION(J)
               STORAGE_ASSIGNMENT(NPUMP) = HYDRO_EXPENSE_ASSIGNMENT(J)
               IF(EL_GROUP(J) > 0) THEN
                  PUMP_GROUP(NPUMP) = EL_GROUP(J)
               ELSE
                  PUMP_GROUP(NPUMP) = -99
               ENDIF
               PUMP_SO2(NPUMP) = EL_SO2_EMIS(J)
               OFF_PEAK_COST_ALLOCATION(NPUMP) = EL_POOL_FRAC_OWN(J)
               OFF_PEAK_INDEX(NPUMP) = J
            ENDIF
         ELSE IF(HYTYPE(J) == 'Q') THEN
!  FOR PEAK OFF SYSTEM SALES
!             MWB = ABS(MWB)
            TEMP_ENRG = ABS(HYDRO_MONTHLY_ENRG(J))
            NPEAKSALE = NPEAKSALE + 1
            IPEAK_SALE_CAPACITY(NPEAKSALE) = MWB
            IPEAK_SALE_ENERGY(NPEAKSALE) = TEMP_ENRG
            VAR_PEAK_SALE_COST(NPEAKSALE) = VAR_COST
            FIXED_PEAK_SALE_COST(NPEAKSALE) = UNIT_FIXED_COST
            ANNUAL_FIXED_PEAK_SALE_COST(NPEAKSALE) = &
                                            UNIT_ANNUAL_FIXED_COST
            PEAK_SALE_COLLECTION(NPEAKSALE) = &
                                        HYDRO_EXPENSE_COLLECTION(J)
            PEAK_SALE_ASSIGNMENT(NPEAKSALE) = &
                                        HYDRO_EXPENSE_ASSIGNMENT(J)
            IF(EL_GROUP(J) > 0) THEN
               PEAK_SALE_GROUP(NPEAKSALE) = EL_GROUP(J)
            ELSE
               PEAK_SALE_GROUP(NPEAKSALE) = -99
            ENDIF
            PEAK_SALE_SO2(NPEAKSALE) = EL_SO2_EMIS(J)
            PEAK_SALE_COST_ALLOCATION(NPEAKSALE) = EL_POOL_FRAC_OWN(J)
            PEAK_SALE_INDEX(NPEAKSALE) = J
         ELSE IF(HYTYPE(J) == 'D' .AND. PSEFF(J) > 0.) THEN
!  FOR DYNAMIC PUMPED STORAGE UNITS
            MWB = ABS(MWB)
            TEMP_ENRG = ABS(HYDRO_MONTHLY_ENRG(J))
            NDYN_PUMP = NDYN_PUMP + 1
            IDYN_GEN_CAPACITY(NDYN_PUMP) = MWB
            IDYN_PUMP_ENERGY(NDYN_PUMP) = TEMP_ENRG
            DYN_PUMP_PSEFF(NDYN_PUMP) = PSEFF(J)
            VAR_DYN_PUMPCOST(NDYN_PUMP) = VAR_COST
            FIXED_DYN_PUMPCOST(NDYN_PUMP) = UNIT_FIXED_COST
            ANNUAL_FIXED_DYN_PUMPCOST(NDYN_PUMP) = &
                                            UNIT_ANNUAL_FIXED_COST
            DYN_PUMP_COLLECTION(NDYN_PUMP) = &
                                        HYDRO_EXPENSE_COLLECTION(J)
            DYN_PUMP_ASSIGNMENT(NDYN_PUMP) = &
                                        HYDRO_EXPENSE_ASSIGNMENT(J)
            IF(EL_GROUP(J) > 0) THEN
               DYN_PUMP_GROUP(NDYN_PUMP) = EL_GROUP(J)
            ELSE
               DYN_PUMP_GROUP(NDYN_PUMP) = -99
            ENDIF
            DYN_PUMP_SO2(NDYN_PUMP) = EL_SO2_EMIS(J)
            DYN_PUMP_COST_ALLOCATION(NDYN_PUMP) = EL_POOL_FRAC_OWN(J)
            DYN_PUMP_INDEX(NDYN_PUMP) = J
         ENDIF
      ENDDO
!   Now a sort for the peaking units; from least capacity factor
!   to greatest capacity factor.
      DO I = 1, NPEAK - 1
         M = I
         N = I + 1
  150    IF(PEAKEN(M)/PEAKCP(M) > PEAKEN(N)/PEAKCP(N)) THEN
            VALUEHELD = PEAKEN(M)
            PEAKEN(M) = PEAKEN(N)
            PEAKEN(N) = VALUEHELD
            VALUEHELD = PEAKCP(M)
            PEAKCP(M) = PEAKCP(N)
            PEAKCP(N) = VALUEHELD
            VALUEHELD = PEAK_COST_ALLOCATION(M)
            PEAK_COST_ALLOCATION(M) = PEAK_COST_ALLOCATION(N)
            PEAK_COST_ALLOCATION(N) = VALUEHELD
            INT_HELD = PEAK_GROUP(M)
            PEAK_GROUP(M) = PEAK_GROUP(N)
            PEAK_GROUP(N) = INT_HELD
            VALUEHELD = PEAK_SO2(M)
            PEAK_SO2(M) = PEAK_SO2(N)
            PEAK_SO2(N) = VALUEHELD
            VALUEHELD = PHVCST(M)
            PHVCST(M) = PHVCST(N)
            PHVCST(N) = VALUEHELD
            VALUEHELD = P_FIXED_COST(M)
            P_FIXED_COST(M) = P_FIXED_COST(N)
            P_FIXED_COST(N) = VALUEHELD
            VALUEHELD = P_ANNUAL_FIXED_COST(M)
            P_ANNUAL_FIXED_COST(M) = P_ANNUAL_FIXED_COST(N)
            P_ANNUAL_FIXED_COST(N) = VALUEHELD
            CHR_TEMP = PEAK_COLLECTION(M)
            PEAK_COLLECTION(M) = PEAK_COLLECTION(N)
            PEAK_COLLECTION(N) = CHR_TEMP
            CHR_TEMP = PEAK_ASSIGNMENT(M)
            PEAK_ASSIGNMENT(M) = PEAK_ASSIGNMENT(N)
            PEAK_ASSIGNMENT(N) = CHR_TEMP
!
            INT_HELD = PEAK_INDEX(M)
            PEAK_INDEX(M) = PEAK_INDEX(N)
            PEAK_INDEX(N) = INT_HELD
            N = M
            M = M - 1
            IF(M > 0) GOTO 150
         ENDIF
      ENDDO
      RETURN
      ENTRY GET_NUM_DYN_STORAGE(R_NUM_DYN_STORAGE)
         R_NUM_DYN_STORAGE = NDYN_PUMP
      RETURN
      ENTRY GET_DYN_STORAGE_VARIABLES(UnitOwning,BlockPos, &
                                      CFLB,CFUB,c,Capacity, &
                                      EquivAvail,SupCost, &
                                      FacetSysCap, &
                                      PrtUni,Fmt8f,PrtDetail)
         DO J = 1, NDYN_PUMP
            N = 2*J ! GENERATING PORTION
            M = N-1 ! PUMP PORTION
!
            UnitOwning(M) = 0 ! M
            BlockPos(M) = 1
            CFLB(M) = 0.
            CFUB(M) = 1.
            c(M) = -1.
            Capacity(M) = IDYN_GEN_CAPACITY(J) / DYN_PUMP_PSEFF(J)
            EquivAvail(M) = 1.
!
!           if(CFUB(M)>EquivAvail(M)) CFUB(M)=EquivAvail(M)
!           if(SupCost<c(M)) SupCost=c(M)
            FacetSysCap=FacetSysCap+Capacity(M)
!
            if(PrtDetail>2) write(PrtUni,Fmt8f)M,UnitOwning(M), &
               BlockPos(M),CFLB(M),CFUB(M),EquivAvail(M),0.0, &
               c(M),Capacity(M),Capacity(M),FacetSysCap
!
            UnitOwning(N) = 0 ! N
            BlockPos(N) = 1
            CFLB(N) = 0.
!            CFUB(N) = 1.
            IF(IDYN_GEN_CAPACITY(J) == 0.) THEN
               CFUB(N) = 1.0
            ELSE
               CFUB(N) = IDYN_PUMP_ENERGY(J) / &
                             (IDYN_GEN_CAPACITY(J) * FLOAT(SAVE_HOURS))
            ENDIF
!            CFUB(M) = CFUB(N)
!
            c(N) = AMAX1(0.,VAR_DYN_PUMPCOST(J) * 10.) ! 10*$/MWH
            Capacity(N) = IDYN_GEN_CAPACITY(J)
            EquivAvail(N) = 1.
!
!           if(CFUB(N)>EquivAvail(N)) CFUB(N)=EquivAvail(N)
            if(SupCost<c(N)) SupCost=c(N)
            FacetSysCap=FacetSysCap+Capacity(N)
!
            if(PrtDetail>2) write(PrtUni,Fmt8f)N,UnitOwning(N), &
               BlockPos(N),CFLB(N),CFUB(N),EquivAvail(N),0.0, &
               c(N),Capacity(N),Capacity(N),FacetSysCap
!
         ENDDO
      RETURN
!
      ENTRY GET_DYN_STORAGE_AUX_CONSTRAINT(iAux,iAuxOrg,nVarInAux, &
                                           AuxCoeff,AuxRHS)
!  ASSUMES STORAGE UNITS OCCUPY THE FIRST AUX CONSTRAINTS.
         nVarInAux = 2 ! BY ASSUMPTION
   N = 2*iAux ! GENERATING PORTION ( iAux is index after compression )
         M = N-1    ! PUMP PORTION
         ! iAuxOrg is before compression
         AuxCoeff(M) = DYN_PUMP_PSEFF(iAuxOrg) 
         AuxCoeff(N) = 1.
         AuxRHS(iAux) = IDYN_GEN_CAPACITY(iAuxOrg)
      RETURN
!
      ENTRY DYN_STORAGE_RESULTS(iBlk,xReturned) ! INSIDE HYDRIN
!  NEED TO ADD THIS TO THE RESULTS MODULE AND THE DETAILED REPORT
!
         RTEMP_ENERGY = xReturned*SAVE_HOURS
         ! CONVERT BACK TO $/MWH
         RTEMP_VARCST = RTEMP_ENERGY * VAR_DYN_PUMPCOST(iBlk) 
         RTEMP_FIXED_COST = (1000.*IDYN_GEN_CAPACITY(iBlk)* &
                                          FIXED_DYN_PUMPCOST(iBlk) + &
                           1000000.*ANNUAL_FIXED_DYN_PUMPCOST(iBlk))/ &
                                               FLOAT(SAVE_PROD_PERIODS)
         RTEMP_SO2 = RTEMP_ENERGY * DYN_PUMP_SO2(iBlk)
         CALL EL_POOL_COSTS_AND_UNIT_REPORTS(DYN_PUMP_INDEX(iBlk), &
                              RTEMP_VARCST, &
                              RTEMP_FIXED_COST, &
                              RTEMP_ENERGY, &
                              DYN_PUMP_COST_ALLOCATION(iBlk), &
                              DYN_PUMP_ASSIGNMENT(iBlk), &
                              IDYN_GEN_CAPACITY(iBlk), &
                              DYN_PUMP_GROUP(iBlk), &
                              SEASON_CAPACITY, &
                              RTEMP_SO2)
!  RESULTS MODULE VARIABLES
         L = 3
         IF(DYN_PUMP_COLLECTION(iBlk) == 'A') L = 1
         IF(DYN_PUMP_COLLECTION(iBlk) == 'B') L = 2
         IF(DYN_PUMP_ASSIGNMENT(iBlk) == 'E') THEN
            ENRG_LIMITED_VAR_COST(L) = ENRG_LIMITED_VAR_COST(L) + &
                                              RTEMP_VARCST
            ENRG_LIMITED_FIXED_COST(L) = ENRG_LIMITED_FIXED_COST(L) + &
                                              RTEMP_FIXED_COST
            ENRG_LIMITED_ENRG = ENRG_LIMITED_ENRG + RTEMP_ENERGY
!            ENRGCT = ENRGCT + RTEMP_VARCST + RTEMP_FIXED_COST
         ENDIF
         IF(DYN_PUMP_ASSIGNMENT(iBlk) == 'P') THEN
            ENRG_LIMITED_PURCHASE_EXPENSE(L) = RTEMP_VARCST + &
                    RTEMP_FIXED_COST + ENRG_LIMITED_PURCHASE_EXPENSE(L)
            ENRG_LIMITED_PURCHASE_ENERGY = DBLE(RTEMP_ENERGY) + &
                                         ENRG_LIMITED_PURCHASE_ENERGY
         ENDIF
!
         DYN_STORAGE_PUMP_ENRG = DYN_STORAGE_PUMP_ENRG + &
                                      RTEMP_ENERGY/DYN_PUMP_PSEFF(iBlk)
         DYN_STORAGE_GEN_ENRG = DYN_STORAGE_GEN_ENRG + RTEMP_ENERGY
         ANN_EL_CAPACITY(DYN_PUMP_GROUP(iBlk)) = &
                  ANN_EL_CAPACITY(DYN_PUMP_GROUP(iBlk)) + &
                                            IDYN_GEN_CAPACITY(iBlk)/12.
      RETURN
      END subroutine hydrin

      !  caller's real*8 vars
      SUBROUTINE PKSHIF(PKENRG ,PEAKCP ,WLPROB ,WLOADS , & 
         CURRNT,HOURS,LDCPTS,R_ISEAS,R_LOCAL_NAME)

!    Reduces peak loads by shifting back along the
!    WLOADS curve and attempting to subtract the PEAKCP.  You get two
!    curves; the original one and one which is identical but lower by
!  PEAKCP.  The original curve uses the I counter, while the lower one
!    uses the J counter.  The area described by the difference of the
!    two curves represents the energy reduction.  This program computes
!    a slope-intercept equation for a segment of both curves, and
!    solves a quadratic equation (as a result of integrating) to define
!    the area.  This process is repeated at each discontinuity of the
!   curve until the cumulative area exceeds exceeds the energy the unit
!    is capable of producing.  Then the left limit is defined so as to
!    yield the correct amount of energy (the right limit being the last
!    limit before the energy was exceeded).  At this point, the program
!    reassigns the WLOAD values to reflect the reduction of energy and
!    the change in shape.
!    20030410 AGT changed local REALs to REAL*8, FLOAT to dble,
!   AMIN1 to min, AMAX1 to max; and added copying loops at entry & exit
!    20030422 AGT changed caller's REALs to REAL*8
      INTEGER*2 I,ILAST,J,JLAST,ISTART,HOURS,CURRNT,LDCPTS,NEXT,R_ISEAS
      INTEGER*4 ILEFT

      REAL*8 PKENRG,PEAKCP,ENRGBL,VLIM,AMCHEK,BQ,CQ,RTEMP,X1,X2,DIFF, &
           M1,M2,RXLIM,LXLIM,B1,B2,DELX,WLPROB(1000,2),WLOADS(1000,2)
      REAL*8 temp_real,NEAR_ZERO/.00000001/
!      REAL*8 DIFFE
      LOGICAL*1 FLAG
      CHARACTER*20 R_LOCAL_NAME
!      copy values from caller's REAL*4 variables to local REAL*8
!      PKENRG=dble(PKENRG4)
!      PEAKCP=dble(PEAKCP4)
!      do J=1,2
!         do I=1,1000
!            WLPROB(I,J)=dble(WLPROB4(I,J))
!            WLOADS(I,J)=dble(WLOADS4(I,J))
!         end do
!      end do
      NEXT = 1 + MOD(CURRNT,2)
      RXLIM = WLOADS(1,CURRNT)
      ENRGBL = PKENRG / dble(HOURS)
      IF(ENRGBL < 0.) RETURN ! with caller's values unchanged
      FLAG = .FALSE.
      I = 1
      J = 0
      ILAST = 0
      JLAST = 0
      M2 = 0.
      B2 = 0.
!  The following section shifts along the curve and accumulates energy.
!   It tests to see if the full capacity can be used.  It also computes
!   a slope-intercept form for each line between discontinuities on
!   the curve.
      IF (ENRGBL >= PEAKCP) GOTO 190
   30 IF( .NOT. ( J < LDCPTS .AND. I <= LDCPTS)) GOTO 205
         IF( .NOT. (ILAST /= I)) GOTO 40
            IF (I == LDCPTS) THEN
               M1 = 0.
               B1 = 1.
            ELSE
               IF(WLOADS(I,CURRNT) == WLOADS(I+1,CURRNT)) &
                  WLOADS(I+1,CURRNT) = WLOADS(I+1,CURRNT) + .1
               M1 = (WLPROB(I,CURRNT) - WLPROB(I+1,CURRNT)) / &
                        (WLOADS(I,CURRNT)-WLOADS(I+1,CURRNT))
!  3/29/94. GAT. ADDED FOR SRP. MODIFIED 7/12/94.
! 3/10/03. FOR XP.
!               IF(ABS(M1) < .000000001) M1 = 0.
               IF(ABS(M1) < NEAR_ZERO) M1 = 0.
               B1 = WLPROB(I,CURRNT) - M1 * WLOADS(I,CURRNT)
               ILAST = I
            ENDIF
   40    IF( .NOT. (JLAST /= J)) GOTO 50
            IF (J == LDCPTS) GOTO 45
            IF(WLOADS(J,CURRNT) == WLOADS(J+1,CURRNT)) &
                   WLOADS(J+1,CURRNT) = WLOADS(J+1,CURRNT) + .1
            M2 = (WLPROB(J,CURRNT) - WLPROB(J+1,CURRNT)) / &
                                 (WLOADS(J,CURRNT)-WLOADS(J+1,CURRNT))
! 3/10/03. FOR XP.
!            IF(ABS(M2) < .000000001) M2 = 0.
            IF(ABS(M2) < NEAR_ZERO) M2 = 0.
   45       B2 = WLPROB(J,CURRNT) - M2 * (WLOADS(J,CURRNT) - PEAKCP)
            JLAST = J
   50    LXLIM = WLOADS(J+1,CURRNT) - PEAKCP
         VLIM = LXLIM
         IF( LXLIM <= WLOADS(I+1,CURRNT)) LXLIM = WLOADS(I+1,CURRNT)
   70    DELX = (RXLIM - LXLIM)
! 3/10/03. FOR XP.
!         IF(ABS(M1-M2) < .0000001) THEN
         IF(ABS(M1-M2) < NEAR_ZERO) THEN
            AMCHEK = (B1-B2)*DELX
         ELSE
            AMCHEK = ((RXLIM+LXLIM)*(M1-M2)/2.+B1-B2)*DELX
         ENDIF

         IF( .NOT. ((ENRGBL-AMCHEK) > 0.)) GOTO 80
            ENRGBL = ENRGBL - AMCHEK
            RXLIM = LXLIM
            IF( WLOADS(I+1,CURRNT) == LXLIM ) THEN
               I = I + 1
            ENDIF
            temp_real = WLOADS(J+1,CURRNT)-PEAKCP
            IF(temp_real == LXLIM ) THEN
               J = J + 1
            ENDIF

            GOTO 30
!   At this point the energy has been exceeded.  Now the exact limits
!   of integration are found.
   80 CONTINUE
!      WRITE (9,1000) AMCHEK,LXLIM,RXLIM,ENRGBL
      IF(M1 == M2) THEN
         LXLIM = RXLIM - ENRGBL / (B1 - B2)
      ELSE
         BQ = (B1-B2)*2./(M1-M2)
         CQ =ENRGBL * 2. / (M1-M2) - (RXLIM+BQ)*RXLIM
         RTEMP = BQ*BQ - 4.*CQ
         IF(RTEMP > 0.) THEN
            RTEMP = SQRT(RTEMP)
         ELSE
            RTEMP = 0.
         ENDIF
         X1 =  -0.5*(BQ + RTEMP)
         X2 = X1 + RTEMP
         LXLIM = min(X1,X2)
         IF (LXLIM < VLIM) LXLIM = max(X1,X2)
         IF(LXLIM > RXLIM) THEN
            LXLIM = VLIM ! TEST WHAT HAPPENS. GAT.
         ENDIF
         DELX = (RXLIM - LXLIM)

         IF(ABS(M1-M2) < NEAR_ZERO) THEN
            AMCHEK = (B1-B2)*DELX
         ELSE
            AMCHEK = ((RXLIM+LXLIM)*(M1-M2)/2.+B1-B2)*DELX
         ENDIF

         ENRGBL = ENRGBL - AMCHEK

      ENDIF
!   This section assigns the new values to WLOADS.  In the first case
!   (110) the full capacity cannot be taken off without violating the
!   energy constraint.
      DO I = 1,LDCPTS
         IF (WLOADS(I,CURRNT) < LXLIM) GOTO 104
      ENDDO
  104 ISTART = I
      IF((WLOADS(1,CURRNT)-PEAKCP) - LXLIM) 110,110,150
  110    WLOADS(1,NEXT) = LXLIM
         WLPROB(1,NEXT) = M1*LXLIM+B1
         J = 1
         DO I = ISTART, LDCPTS
            J = J + 1
            WLOADS(J,NEXT) = WLOADS(I,CURRNT)
            WLPROB(J,NEXT) = WLPROB(I,CURRNT)
         ENDDO
         LDCPTS = J
         GOTO 210
!   In this case, the full peaking capacity was taken, and the matter
!   is complicated by having to follow the original curve's contour
!   at a lower level.  When the left boundary of energy reduction is
!   reached, decisions must be made about how to assign points.
  150    CONTINUE
         J = 1
         RTEMP = WLOADS(J,CURRNT)-PEAKCP
         DOWHILE(RTEMP - LXLIM > 0.001)
            WLOADS(J,NEXT) = RTEMP
            WLPROB(J,NEXT) = WLPROB(J,CURRNT)
            J = J+1
            IF(J > LDCPTS) THEN
               CURRNT = NEXT
               RETURN

            ENDIF
            RTEMP = WLOADS(J,CURRNT)-PEAKCP
         ENDDO
!          GOTO 210
  170    IF (J == LDCPTS) FLAG = .TRUE.
         ILEFT = INT(LXLIM)
!          DIFF = LXLIM - dble(ILEFT) -.001
!          DIFFE = dble(ILEFT + 1) - LXLIM
!          IF (DIFF > DIFFE) DIFF = DIFFE -.001
         DIFF = MAX(0.000001*LXLIM, &
                         MIN(LXLIM-dble(ILEFT),dble(ILEFT+1)-LXLIM))
         WLOADS(J,NEXT) = LXLIM + DIFF
         WLPROB(J,NEXT) = M2*WLOADS(J,NEXT) + B2
         J = J + 1
         WLOADS(J,NEXT) = LXLIM - DIFF
         WLPROB(J,NEXT) = M1*WLOADS(J,NEXT) + B1
         IF(WLPROB(J,NEXT) > 1.1) THEN
            WRITE(4,*) "INSIDE PKSHIFT LIMITS EXCEEDED"
         ENDIF
         IF (FLAG) GOTO 185
         IF (.NOT.(dble(ILEFT) == WLOADS(ISTART,CURRNT))) GOTO 175
            ISTART = ISTART+1
  175    DO I = ISTART, LDCPTS
            IF(WLPROB(I,CURRNT) > 1.1) THEN
               WRITE(4,*) "INSIDE PKSHIFT LIMITS EXCEEDED"
            ENDIF
            J = J + 1
            WLOADS(J,NEXT) = WLOADS(I,CURRNT)
            WLPROB(J,NEXT) = WLPROB(I,CURRNT)
         ENDDO
  185    LDCPTS = J
         GOTO 210
  190 CONTINUE
      DO I = 1, LDCPTS
         WLOADS(I,NEXT) = WLOADS(I,CURRNT) - ENRGBL
         IF(WLPROB(I,CURRNT) > 1.1) THEN
            WRITE(4,*) "INSIDE PKSHIFT LIMITS EXCEEDED"
         ENDIF
         WLPROB(I,NEXT) = WLPROB(I,CURRNT)
      ENDDO
      GOTO 210
  205 WRITE (4,*) ' Error - limits have been exceeded in PKSHIFT.'
      WRITE (4,*) 'For Peaking Unit ',R_LOCAL_NAME,' and season ', &
                                                               R_ISEAS
  210 CURRNT = NEXT

      RETURN
      END subroutine pkshif

      SUBROUTINE OFPEAK(IPUMEN,IPUMCP,WLPROB,WLOADS, & 
         CURRENT,HOURS,LDCPTS,DX,UNUSED_ENRG)
!    Increases the base loads by shifting forward along
!    the WLOADS curve and attempting to add the IPUMEN.  You get two
!    curves; the original one and one which is identical but higher by
!  IPUMCP.  The original curve uses the I counter, while the higher one
!    uses the J counter.  The area described by the difference of the
!    two curves represents the energy increase.  This program computes
!    a slope-intercept equation for a segment of both curves, and
!    solves a quadratic equation (as a result of integrating) to define
!    the area.  This process is repeated at each discontinuity of the
!    curve until the cumulative area exceeds the energy the unit needs
!    to produce.  Then the right limit is defined so as to
!    yield the correct amount of energy (the left limit being the last
!    limit before the energy was exceeded).  At this point, the program
!    reassigns the WLOAD values to reflect the increase in energy and
!    the change in shape.
      INTEGER*2 HOURS,CURRENT,LDCPTS,NEXT,I,J,COPY_START,INEXT,ISHIFT
      real*8 IPUMCP,IPUMEN,WLPROB(1000,2),WLOADS(1000,2), & 
         ENRG_BAL,ENRG_INC,OPB,A2,SQROOT,DELTA,B2_4AC,DELTA_X, &
         BASE,XSHIFT,YSHIFT1,YSHIFT2,X1,X2,X3,Y1,Y2,INTPL8
      REAL DX
!       REAL*8 UNUSED_ENRG
      LOGICAL*1 UNUSED_ENRG
      INTPL8(X1,Y1,X3,Y2,X2)=Y1+(Y2-Y1)*(X3-X1)/(X2-X1)
      NEXT = MOD(CURRENT,2) + 1
      ENRG_BAL = IPUMEN / FLOAT(HOURS)
      IF(ENRG_BAL >= IPUMCP) THEN
         DO I = 1, LDCPTS
            WLOADS(I,NEXT) = WLOADS(I,CURRENT) + ENRG_BAL
            WLPROB(I,NEXT) = WLPROB(I,CURRENT)
         ENDDO
         CURRENT = NEXT
         RETURN
      ENDIF
!   The following section shifts along the curve and cumulates energy
!   until the capacity or energy limit is reached.  It also computes
!   a slope-intercept form for each line between discontinuities on
!   the curve.
      I = 1
      INEXT = 1
      BASE = WLOADS(1,CURRENT)
      YSHIFT1 = 1.
   20 XSHIFT = WLOADS(I+1,CURRENT) - IPUMCP
      IF(XSHIFT <= BASE) THEN
         YSHIFT2 = 1.
      ELSE
         ISHIFT = (XSHIFT - BASE)/DX + 1
   30    CONTINUE
         IF(.NOT.(XSHIFT >= WLOADS(ISHIFT,CURRENT) .AND. &
                  XSHIFT <= WLOADS(ISHIFT+1,CURRENT))) THEN
            IF(XSHIFT > WLOADS(ISHIFT+1,CURRENT)) THEN
               ISHIFT = ISHIFT + 1
            ELSE
               ISHIFT = ISHIFT - 1
            ENDIF
            GOTO 30
         ENDIF
         IF( ABS(WLOADS(ISHIFT,CURRENT) - &
                                  WLOADS(ISHIFT+1,CURRENT)) < .01) THEN
            WRITE(4,*) "LIMIT INSIDE OFPEAK EXCEEDED"
            WRITE(4,*) "LOWER LOAD VALUE = ",WLOADS(ISHIFT,CURRENT)
            WRITE(4,*) "UPPER LOAD VALUE = ",WLOADS(ISHIFT+1,CURRENT)
            WRITE(4,*) "WILL ARTIFICALLY ALTER LOAD SHAPE TO CONTINUE"
            WLOADS(ISHIFT+1,CURRENT) = WLOADS(ISHIFT,CURRENT) + .01
         ENDIF
      YSHIFT2 = INTPL8(WLOADS(ISHIFT,CURRENT),WLPROB(ISHIFT,CURRENT), &
                          XSHIFT,WLPROB(ISHIFT+1,CURRENT), &
                          WLOADS(ISHIFT+1,CURRENT))
      ENDIF
      DELTA = WLOADS(I+1,CURRENT)-WLOADS(I,CURRENT)
    ENRG_INC=(YSHIFT1+YSHIFT2-WLPROB(I,CURRENT)-WLPROB(I+1,CURRENT))* &
                   DELTA/2.
      IF(ENRG_BAL-ENRG_INC > 0. .AND. I < LDCPTS - 1 ) THEN
         ENRG_BAL = ENRG_BAL - ENRG_INC
         WLPROB(INEXT,NEXT) = YSHIFT1
         WLOADS(INEXT,NEXT) = WLOADS(I,CURRENT)
         IF(YSHIFT2 < 1. .AND. YSHIFT1 == 1.) THEN
            WLOADS(INEXT,NEXT) = BASE + IPUMCP
            INEXT = INEXT + 1
         ELSE IF(YSHIFT2 < 1) THEN
            INEXT = INEXT + 1
         ENDIF
         I = I + 1
         YSHIFT1 = YSHIFT2
         GOTO 20
      ELSE
!  The energy limit was reached.  Find the intercept value and adjust
!  load curve points.
         WLOADS(INEXT,NEXT) = WLOADS(I,CURRENT)
         WLPROB(INEXT,NEXT) = YSHIFT1
         A2 = ((WLPROB(I,CURRENT) - WLPROB(I+1,CURRENT)) - &
               (YSHIFT1-YSHIFT2))
         IF(A2 == 0.) THEN
            IF(ENRG_INC > 0.) THEN
               DELTA_X = DELTA*ENRG_BAL/ENRG_INC
            ELSE
               DELTA_X = 0.
            ENDIF
         ELSE
            OPB = ENRG_INC
            B2_4AC = OPB*OPB-2.*A2*ENRG_BAL*DELTA 
            IF(B2_4AC == 0.) THEN
               DELTA_X = OPB/A2
            ELSEIF(B2_4AC > 0.) THEN
               SQROOT = SQRT(B2_4AC)
               DELTA_X = MIN(OPB+SQROOT,OPB-SQROOT)/A2
            ELSE
               DELTA_X = 0.
            ENDIF
         ENDIF
         IF(I == LDCPTS - 1) DELTA_X = WLOADS(I+1,CURRENT)- &
                                                    WLOADS(I,CURRENT)
         INEXT = INEXT + 1
         WLOADS(INEXT,NEXT) = WLOADS(I,CURRENT) + DELTA_X
         WLPROB(INEXT,NEXT) = INTPL8(WLOADS(I,CURRENT),YSHIFT1, &
                                     WLOADS(INEXT,NEXT), &
                                     YSHIFT2,WLOADS(I+1,CURRENT))
         INEXT = INEXT + 1
         IF(WLOADS(INEXT-1,NEXT)+.001 < WLOADS(I+1,CURRENT)) THEN
            WLOADS(INEXT,NEXT) = WLOADS(INEXT-1,NEXT) + .001
            WLPROB(INEXT,NEXT) = INTPL8(WLOADS(I,CURRENT), &
                                          WLPROB(I,CURRENT), &
                                          WLOADS(INEXT,NEXT), &
                                          WLPROB(I+1,CURRENT), &
                                          WLOADS(I+1,CURRENT))
            COPY_START = I + 1
         ELSE
            WLOADS(INEXT,NEXT) = WLOADS(I+1,CURRENT)
            WLPROB(INEXT,NEXT) =  WLPROB(I+1,CURRENT)
            COPY_START = I + 2
         ENDIF
         DO J = COPY_START, LDCPTS
            INEXT = INEXT + 1
            WLOADS(INEXT,NEXT) = WLOADS(J,CURRENT)
            WLPROB(INEXT,NEXT) = WLPROB(J,CURRENT)
         ENDDO
         IF(I>=LDCPTS-1) THEN
!            IF THERE ISN'T SUFFICIENT ENERGY LEFT IN THE TAIL,
!            THEN ASSUME THE TAIL STAYS THE SAME, TRUNCATE THE TAIL,
!            CALCULATE UNUSED ENERGY. THE PROGRAM IS TERMINATED
!            DOWN HERE SO THAT WE CAN GET THE INTERPOLATED VALUE
!            FOR THE LAST LDC POINT.

            LDCPTS = INEXT-1

            UNUSED_ENRG = .TRUE.
            GOTO 50
         ENDIF
         LDCPTS = INEXT
      ENDIF
   50 CURRENT = NEXT
      RETURN
      END subroutine ofpeak
      
      subroutine FE_ENRG_LIMITED_INFO(R_CLASS, &
                                 R_PURCHASE_POWER_EXPENSE, &
                                 R_VARIABLE_EXPENSE, &
                                 R_FIXED_EXPENSE, &
                                 R_TOTAL_SALES_REVENUE, &
                                 R_EL_MARKET_REVENUES, &
                                 R_EL_MARKET_PURCHASES, &
                                 R_EL_GENERATION)
        implicit none
        
        real (kind=4) :: r_total_sales_revenue
        real :: r_el_generation
        real :: r_el_market_purchases
        real :: r_fixed_expense
        real :: r_el_market_revenues
        real :: r_variable_expense
        real :: r_purchase_power_expense
        integer (kind=2) :: r_class
         R_EL_MARKET_REVENUES = 0.
         R_EL_MARKET_PURCHASES = 0.
         IF(R_CLASS <= MAX_HYDRO_CLASS_NUM) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
               R_PURCHASE_POWER_EXPENSE = R_PURCHASE_POWER_EXPENSE &
                          + EL_ANN_CLASS_PURCHASES(ASSET_CLASS,1) &
                          + EL_ANN_CLASS_PURCHASES(ASSET_CLASS,2) &
                          + EL_ANN_CLASS_PURCHASES(ASSET_CLASS,3) &
                          + EL_ANN_CLASS_PURCHASES(ASSET_CLASS,4) &
                       + INTRA_COMPANY_PURCHASE_EXPENSE(ASSET_CLASS)/ &
                                                               1000000.
               R_VARIABLE_EXPENSE = R_VARIABLE_EXPENSE &
                               + EL_ANN_CLASS_VAR_COST(ASSET_CLASS,1) &
                               + EL_ANN_CLASS_VAR_COST(ASSET_CLASS,2) &
                               + EL_ANN_CLASS_VAR_COST(ASSET_CLASS,3) &
                                 + EL_ANN_CLASS_VAR_COST(ASSET_CLASS,4)
               R_FIXED_EXPENSE = R_FIXED_EXPENSE &
                             + EL_ANN_CLASS_FIXED_COST(ASSET_CLASS,1) &
                             + EL_ANN_CLASS_FIXED_COST(ASSET_CLASS,2) &
                             + EL_ANN_CLASS_FIXED_COST(ASSET_CLASS,3) &
                               + EL_ANN_CLASS_FIXED_COST(ASSET_CLASS,4)
               R_EL_GENERATION = EL_ANN_CLASS_ENERGY(ASSET_CLASS,1) &
                                 + EL_ANN_CLASS_ENERGY(ASSET_CLASS,2) &
                                 + EL_ANN_CLASS_ENERGY(ASSET_CLASS,3) &
                                 + EL_ANN_CLASS_ENERGY(ASSET_CLASS,4)
               R_TOTAL_SALES_REVENUE = R_TOTAL_SALES_REVENUE &
                             + EL_ANN_CLASS_REVENUE(ASSET_CLASS,1) &
                             + EL_ANN_CLASS_REVENUE(ASSET_CLASS,2) &
                             + EL_ANN_CLASS_REVENUE(ASSET_CLASS,3) &
                             + EL_ANN_CLASS_REVENUE(ASSET_CLASS,4) &
                          + INTRA_COMPANY_SALES_REVENUE(ASSET_CLASS)/ &
                                                               1000000.
               R_EL_MARKET_REVENUES = &
                   EL_ANN_CLASS_MARKET_REVENUES(ASSET_CLASS)/1000000. &
                      + EL_MON_MDS_MARKET_REVENUES(ASSET_CLASS,0)
               R_EL_MARKET_PURCHASES = &
                    EL_ANN_CLASS_MARKET_PURCHASE(ASSET_CLASS)/1000000.
            ENDIF
         ENDIF
      end subroutine fe_enrg_limited_info


      
      subroutine GET_EL_ASSET_CLASS_INFO(R_ASSET_CLASS_NUM, &
                                                  R_ASSET_CLASS_VECTOR)
         use service_decs
         use foshydi2com
         implicit none
         integer(kind=2) :: R_ASSET_CLASS_VECTOR(:)
         integer(kind=2) :: R_ASSET_CLASS_NUM(:)
 
	         R_ASSET_CLASS_NUM(1:HYDRO_UNITS) = &
                ns_service_decs%ASSET_CLASS_NUM(1:HYDRO_UNITS)

         R_ASSET_CLASS_VECTOR(1:HYDRO_UNITS) = &
                      ns_service_decs%ASSET_CLASS_VECTOR(1:HYDRO_UNITS)

      end subroutine GET_EL_ASSET_CLASS_INFO
      
      
      
      subroutine  STORE_EL_ASSET_CLASS_INFO(R_ASSET_CLASS_NUM, &
                                      R_ASSET_CLASS_VECTOR, &
                                      R_INTRA_COMPANY_CLASS_ID, &
                                      R_BASE_EL_UNITS)
         use service_decs
         implicit none
         
         integer(kind=2)  :: R_ASSET_CLASS_VECTOR(:)
         integer(kind=2) :: R_ASSET_CLASS_NUM(:)
         integer(kind=2) :: R_INTRA_COMPANY_CLASS_ID(:)
         integer(kind=2) :: R_BASE_EL_UNITS
         integer (kind=2) :: LAST_EL_unit
         integer :: file_unit
         
         IF(ALLOCATED(ns_service_decs%asset_class_num)) then

            DEALLOCATE(ns_service_decs%ASSET_CLASS_NUM)
            deallocate(ns_service_decs%INTRA_COMPANY_CLASS_ID)
            deallocate(ns_service_decs%ASSET_CLASS_VECTOR)

         endif
         
         ALLOCATE(ns_service_decs%ASSET_CLASS_NUM(MAX_EL_UNITS))
         ALLOCATE(ns_service_decs%INTRA_COMPANY_CLASS_ID(MAX_EL_UNITS))
         ALLOCATE(ns_service_decs%ASSET_CLASS_VECTOR(MAX_EL_UNITS))
         LAST_EL_UNIT = R_BASE_EL_UNITS
         DO file_unit = 1, LAST_EL_UNIT
            ns_service_decs%ASSET_CLASS_NUM(file_unit) = &
                R_ASSET_CLASS_NUM(file_unit)
            ns_service_decs%ASSET_CLASS_VECTOR(file_unit) = &
            R_ASSET_CLASS_VECTOR(file_unit)
            ns_service_decs%INTRA_COMPANY_CLASS_ID(file_unit)=&
                R_INTRA_COMPANY_CLASS_ID(file_unit)
         ENDDO
      RETURN
      
      end subroutine STORE_EL_ASSET_CLASS_INFO
      
      subroutine GET_EL_SO2_EMISSIONS(R_CLASS,R_CLASS_EL_SO2_ANNUAL)
         implicit none
         integer(kind=2), intent(in) :: r_class
         real, intent(inout) :: R_CLASS_EL_SO2_ANNUAL
         integer (kind=2) :: asset_class
         
         R_CLASS_EL_SO2_ANNUAL = 0.
         IF(R_CLASS <= MAX_HYDRO_CLASS_NUM) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
            R_CLASS_EL_SO2_ANNUAL = EL_ANN_CLASS_SO2(ASSET_CLASS)
            ENDIF
         ENDIF
      end subroutine GET_EL_SO2_EMISSIONS
      
      subroutine GET_EL_ENERGY_BY_TYPE(  R_ISEAS, &
                                    R_GENERATION_ENERGY, &
                                    R_PURCHASE_ENERGY, &
                                    R_SALE_ENERGY)
        implicit none
        
        integer (kind=2) :: r_iseas
        real (kind=4) :: R_PURCHASE_ENERGY
        real (kind=4) :: R_GENERATION_ENERGY
        real :: r_sale_energy
        integer (kind=2) :: e_g
        
        
         ASSET_CLASS = 0 ! SYSTEM CLASS

         E_G = 1
         R_GENERATION_ENERGY = R_GENERATION_ENERGY + &
                      EL_MON_MDS_ENERGY(ASSET_CLASS,E_G,R_ISEAS)*1000.
         E_G = 2
         R_PURCHASE_ENERGY = & !  ASSUME THAT THIS IS RECEIVED FOR SEPA
                      EL_MON_MDS_ENERGY(ASSET_CLASS,E_G,R_ISEAS)*1000.
         E_G = 3
         R_SALE_ENERGY = R_SALE_ENERGY + &
                      EL_MON_MDS_ENERGY(ASSET_CLASS,E_G,R_ISEAS)*1000.
      end subroutine GET_EL_ENERGY_BY_TYPE
      subroutine GET_MONTHLY_EL_GROUP_REPORT(R_ISEAS,R_GROUP, &
                                        R_MONTHLY_GROUP_REPORT)

         integer (kind=2) :: r_iseas
         integer (kind=2) :: r_group
         real (kind=4) :: R_MONTHLY_GROUP_REPORT(0:12,0:99,17)

         
         
         R_MONTHLY_GROUP_REPORT(SAVE_MONTH,R_GROUP,el_capacity) = &
             R_MONTHLY_GROUP_REPORT(SAVE_MONTH,R_GROUP,el_capacity) + &
                MONTHLY_EL_GROUP_REPORT(SAVE_MONTH,R_GROUP,el_capacity)
! ADDED 5/9/99. FOR BURESH.
         R_MONTHLY_GROUP_REPORT(SAVE_MONTH, &
                                    R_GROUP,el_eqivalent_capacity) = &
                R_MONTHLY_GROUP_REPORT(SAVE_MONTH, &
                                    R_GROUP,el_eqivalent_capacity) + &
                MONTHLY_EL_GROUP_REPORT(SAVE_MONTH, &
                                        R_GROUP,el_eqivalent_capacity)
!
         R_MONTHLY_GROUP_REPORT(SAVE_MONTH,R_GROUP,Generation) = &
              R_MONTHLY_GROUP_REPORT(SAVE_MONTH,R_GROUP,Generation) + &
                 MONTHLY_EL_GROUP_REPORT(SAVE_MONTH,R_GROUP,Generation)
         R_MONTHLY_GROUP_REPORT(SAVE_MONTH,R_GROUP,variable_om) = &
             R_MONTHLY_GROUP_REPORT(SAVE_MONTH,R_GROUP,variable_om) + &
                MONTHLY_EL_GROUP_REPORT(SAVE_MONTH,R_GROUP,variable_om)
         R_MONTHLY_GROUP_REPORT(SAVE_MONTH,R_GROUP,fixed_om) = &
                R_MONTHLY_GROUP_REPORT(SAVE_MONTH,R_GROUP,fixed_om) + &
                   MONTHLY_EL_GROUP_REPORT(SAVE_MONTH,R_GROUP,fixed_om)
         R_MONTHLY_GROUP_REPORT(SAVE_MONTH,R_GROUP,sulfur_o2) = &
               R_MONTHLY_GROUP_REPORT(SAVE_MONTH,R_GROUP,sulfur_o2) + &
                  MONTHLY_EL_GROUP_REPORT(SAVE_MONTH,R_GROUP,sulfur_o2)
      end subroutine GET_MONTHLY_EL_GROUP_REPORT


!      ROUTINE FOR CLASSIFYING POOLING COSTS TO THE MEMBERS
!            COPYRIGHT (c) 1991 M.S. GERBER & ASSOCIATES, INC
!                        ALL RIGHTS RESERVED

      SUBROUTINE EL_POOL_COSTS_AND_UNIT_REPORTS(UNIT_NO, &
                               VAR_COST,EL_FIXED_COST,ENERGY, &
                               UTIL_ALLOCATION,EXPENSE_TYPE, &
                               CAPACITY,EL_GRP,SEASON_CAPACITY, &
                               EL_SO2_EMIS)
      use globecom
      USE IREC_ENDPOINT_CONTROL
      use capacity_arrays
      use SpinDriftLib
      use prod_arrays_dimensions
      use sizecom
      use prodcom
      use poolcom
      use prod2com
      use group_prod
      use aitvafin
      use aitvaprod
      use group_prod
      use foshydi2com
      use endpoint
      use rptreccontrol
      use enrg_helper
      use month_extra
      use service_decs
      use namescom
      use mod_base_year
	  use mthnmcom
      use rangechecker
      use debugtrace
      implicit none
      
      integer :: file_trace_epc=0
      

      REAL VAR_COST,EL_FIXED_COST,ENERGY,UTIL_ALLOCATION
      CHARACTER*1 EXPENSE_TYPE
      REAL POOL_VAR_COST,POOL_FIXED_COST,POOL_ENERGY,POOL_ALLOCATION, &
            UTIL_ENERGY,CAPACITY,EL_SO2_EMIS,POOL_SO2, &
            SEASON_CAPACITY(0:MAX_REPORTING_GROUPS), &
            MARKET_REVENUE, &
            RTG,RMK,RFT
      INTEGER*2 EL_GRP,EL_INDEX,AN_GRP,MK,MARKET_AREA_LOOKUP
      LOGICAL*1 MON_EL_REPORT_NOT_OPEN/.TRUE./, &
                REPORT_THIS_EL_UNIT, &
                TEMP_L1, &
                GET_EL_BASECASE_MARKET_ID

      INTEGER*2   MON_EL_UNIT_HEADER, &
                  MON_EL_UNIT_NO, &
                  R_ISEAS, &
                  UNIT_NO, &
                  HYDRO_TRANS_GROUP, &
                  TG
                  
                  
      INTEGER MON_EL_UNIT_REC
      
      CHARACTER*6 MARKET_ID
      
      CHARACTER*20 LOCAL_EL_NAME

      INTEGER*2 ,CURRENT_YEAR,CURRENT_YEAR_COMPARISON, &
                  PRODUCTION_PERIODS, &
                  UNIT, &
                  MAX_TRANS_GROUP_NUMBER, &
                  GET_MAX_TRANS_GROUP_NUMBER
      INTEGER*4 VALUES_2_ZERO
! MONTHLY
      REAL &
           MON_MDS_EL_CAPACITY(:,:,:),MON_MDS_EL_ENERGY(:,:,:), &
           MON_MDS_EL_VAR_COST(:,:,:),MON_MDS_EL_FIXED_COST(:,:,:), &
           MON_MDS_EL_SO2_EMIS(:,:,:)
      ALLOCATABLE :: &
                     MON_MDS_EL_CAPACITY,MON_MDS_EL_ENERGY, &
                     MON_MDS_EL_VAR_COST,MON_MDS_EL_FIXED_COST, &
                     MON_MDS_EL_SO2_EMIS
      SAVE &
           MON_MDS_EL_CAPACITY,MON_MDS_EL_ENERGY, &
           MON_MDS_EL_VAR_COST,MON_MDS_EL_FIXED_COST, &
           MON_MDS_EL_SO2_EMIS
! ANNUAL
      REAL ANNUAL_EL_CAPACITY(:,:),ANNUAL_EL_ENERGY(:,:), &
           ANNUAL_EL_VAR_COST(:,:),ANNUAL_EL_FIXED_COST(:,:), &
           ANNUAL_EL_SO2_EMIS(:,:), &
           MON_MDS_EL_MARKET_REVENUE(:,:,:)
      ALLOCATABLE :: ANNUAL_EL_CAPACITY,ANNUAL_EL_ENERGY, &
                     ANNUAL_EL_VAR_COST,ANNUAL_EL_FIXED_COST, &
                     ANNUAL_EL_SO2_EMIS, &
                     MON_MDS_EL_MARKET_REVENUE
      SAVE ANNUAL_EL_CAPACITY,ANNUAL_EL_ENERGY, &
           ANNUAL_EL_VAR_COST,ANNUAL_EL_FIXED_COST, &
           ANNUAL_EL_SO2_EMIS, &
                     MON_MDS_EL_MARKET_REVENUE
      SAVE  MON_EL_UNIT_NO,MON_EL_UNIT_REC

      
!
!  ASSET ALLOCATION STUFF
      INTEGER*2 HOURS,R_HOURS
      REAL* 4 CAPACITY_FACTOR,REVENUE_FACTOR
      LOGICAL*1 ASSET_CLASS_ACTIVE
      CHARACTER*1 DUMMY_TYPE
      INTEGER*2 ::  &
              R_BASE_EL_UNITS,R_NEW_UNIT,R_REF_UNIT,LAST_EL_UNIT/0/, &
                  CLASS_POINTER,NUM_OF_HYDRO_CLASSES, &
                  ASSET_ALLOCATION_VECTOR,COLLECTION_GROUP,C_G, &
                  EXPENSE_GROUP,E_G,RESOURCE_OR_SALE, &
                  RETURN_NUM_OF_HYDRO_CLASSES, &
                  RETURN_HYDRO_CLASS_POINTER
      SAVE NUM_OF_HYDRO_CLASSES
      REAL INTRA_SALES_REVENUE, &
           INTRA_PURCHASE_EXPENSES, &
           INTRA_VARIABLE_EXPENSES, &
           INTRA_FIXED_EXPENSES
      SAVE INTRA_SALES_REVENUE, &
           INTRA_PURCHASE_EXPENSES, &
           INTRA_VARIABLE_EXPENSES, &
           INTRA_FIXED_EXPENSES
      REAL*4 R_INTRA_SALES_REVENUE, &
             R_INTRA_PURCHASE_EXPENSES, &
             R_INTRA_VARIABLE_EXPENSES, &
             R_INTRA_FIXED_EXPENSES,VAR_PLUS_FIXED_COST, &
             R_GENERATION_ENERGY, &
             R_PURCHASE_ENERGY, &
             R_SALE_ENERGY


      REAL MON_MDS_INCO_SALES_REVENUE(:,:), &
           MON_MDS_INCO_PURCHASE_EXPENSE(:,:)
      ALLOCATABLE :: MON_MDS_INCO_PURCHASE_EXPENSE, &
                     MON_MDS_INCO_SALES_REVENUE
      SAVE MON_MDS_INCO_PURCHASE_EXPENSE, &
           MON_MDS_INCO_SALES_REVENUE
      REAL*4      MON_MDS_IN_SALES_REVENUE(:), &
                  MON_MDS_IN_PURCHASE_EXPENSES(:), &
                  MON_MDS_IN_VARIABLE_EXPENSES(:), &
                  MON_MDS_IN_FIXED_EXPENSES(:)
      ALLOCATABLE :: &
                  MON_MDS_IN_SALES_REVENUE, &
                  MON_MDS_IN_PURCHASE_EXPENSES, &
                  MON_MDS_IN_VARIABLE_EXPENSES, &
                  MON_MDS_IN_FIXED_EXPENSES
      SAVE &
                  MON_MDS_IN_SALES_REVENUE, &
                  MON_MDS_IN_PURCHASE_EXPENSES, &
                  MON_MDS_IN_FIXED_EXPENSES
!
      INTEGER*2 MAX_INTRA_CLASS
      SAVE MAX_INTRA_CLASS
      INTEGER*2 :: I,J
      REAL ASSET_CLASS_LIST(:),ASSET_ALLOCATION_LIST(:), &
           EL_MON_MDS_CAPACITY(:,:,:), &       
           EL_MON_MDS_SELL_CAPACITY(:,:,:), &
           EL_MON_MDS_SELL_ENERGY(:,:,:), &
           EL_MON_MDS_SO2(:,:)
           
    REAL ASSET_ALLOCATOR,R_CLASS_EL_SO2_ANNUAL,R_EL_ANN_CLASS_ENERGY, &
            R_EL_ANN_CLASS_CAPACITY, &
            SALES_MULT

      ALLOCATABLE :: &
                     ASSET_CLASS_LIST, &
                     ASSET_ALLOCATION_LIST, &
                     EL_MON_MDS_CAPACITY, &
                     EL_MON_MDS_SELL_CAPACITY, &
                     EL_MON_MDS_SELL_ENERGY, &
                     EL_MON_MDS_SO2

      SAVE  &
                     ASSET_CLASS_LIST, &
                     ASSET_ALLOCATION_LIST, &
                     EL_MON_MDS_CAPACITY, &
                     EL_MON_MDS_SELL_CAPACITY, &
                     EL_MON_MDS_SELL_ENERGY, &
                     EL_MON_MDS_SO2
!  EXPENSE TRANSFER TO ASSET CLASS MODULE
      INTEGER*2 R_CLASS
      LOGICAL (kind=1) ::  R_CLASS_EXISTS_local, &
                  RUN_TRANSACT/.FALSE./,YES_RUN_TRANSACT, &
                  TRANS_GROUP_REPORTING_ACTIVE(:), &
                  GET_REPORT_CL_CAPACITY, &
                  ECITY_TEST/.FALSE./, &
                  PRICE_ONLY_WHOLESALE_REV/.FALSE./, &
                  APPLY_TRANS_REV_TO_WHOLESALE
      REAL R_PURCHASE_POWER_EXPENSE, &
           R_EXPENSE_COLLECTED_ADJ_CLAUSE, &
           R_EXPENSE_COLLECTED_BASE_RATES, &
           R_VARIABLE_EXPENSE, &
           R_FIXED_EXPENSE, &
           R_BTL_EXPENSES, &
           R_BTL_SALES_REVENUE, &
           R_TOTAL_SALES_REVENUE, &
           R_SALES_REVENUE_NOT_IN_RATES, &
           R_NOT_COLLECTED_IN_RATES, &
           R_EL_GENERATION
      REAL*4 :: R_CAWCD_ENERGY
      REAL*4 :: AVE_REVENUE
      
      ALLOCATABLE :: TRANS_GROUP_REPORTING_ACTIVE
      SAVE           TRANS_GROUP_REPORTING_ACTIVE
      
      INTEGER*2 R_MAX_MONTHLY_GROUPS, &
                R_MAX_MONTHLY_GROUP_VARIABLES, &
                R_GROUP, &
                PM,ST_TG,ST,R_ST_TG,R_MONTH,RPS_INDEX_TRANSLATE

      REAL*4    RPS_CONTRIB, &
                TEMP_R4, &
                R_RESOURCE_RPS_VARS(16) ! 052717
      LOGICAL*1 VOID_LOGICAL,RETURN_ASSET_CLASS_LISTS, &
                GET_HYDRO_RPS_DATA, &
                PUT_HYDRO_RPS_ENRG_CAP
      INTEGER*2 ALLOCATION_VECTOR
      REAL*4 ALLOCATION_VALUE(AVAIL_DATA_YEARS)
      REAL*4 R_EL_MARKET_REVENUES, &
             R_EL_MARKET_PURCHASES
      REAL*4 MONTH_VARS(0:12,1:*)
      INTEGER*2 MO, &
                TG_POSITION, &
                GET_TRANS_GROUP_POSITION, &
                TECH_INDEX
      LOGICAL*1 MONTHLY_PW_REPORT,YES_POWERWORLD_REPORT, &
                PW_REPORT_NOT_OPEN/.TRUE./, &
                GET_PORT_MARKET_AREA_ABBREV, &
                GET_PW_EL_UNIT_TEXT_FIELDS
      REAL*4  FIXED_COST_PER_UNIT, &
              HESI_UNIT_ID_NUM, &
              POWERDAT_PLANT_ID, &
              GET_POWERDAT_PLANT_ID, &
              VARIABLE_COST_PER_UNIT
      INTEGER MON_PW_TRANS_UNIT_REC, &
              R_MON_PW_TRANS_UNIT_REC, &
              EL_RESOURCE_ID
      CHARACTER*1 QUOTE/'"'/
      CHARACTER*5 GET_SCENAME
      CHARACTER*6 EIA_PLANT_CODE, &
                  RDI_MARKET_AREA_NAME
      CHARACTER*35 GET_GROUP_NAME,MULTI_AREA_NAME
      CHARACTER*64 FILE_NAME
      CHARACTER*1024 PW_REC
      CHARACTER*9 CL_MONTH_NAME(14) &
                               /'January  ','February ', &
                                'March    ','April    ', &
                                'May      ','June     ', &
                                'July     ','August   ', &
                                'September','October  ', &
                                'November ','December ', &
                                'Annual   ','Fiscal Yr'/
       
! END OF DATA DECLARATIONS
!  5/20/92 ADDED GROUP CAPABILITY
!  8/16/92 TRAP FOR SALES/PUMP

      if(file_trace_epc==0) then
       file_trace_epc=open_trace("el_pool_costs_and_unit_reports.trace", &
        rq_epc)
      end if
      
      IF(EL_GRP /= -99) THEN
         IF(EL_GRP > MAX_REPORTING_GROUPS) THEN
            AN_GRP = 0
         ELSE
            AN_GRP = EL_GRP
         ENDIF
         SEASON_CAPACITY(AN_GRP)=SEASON_CAPACITY(AN_GRP) + CAPACITY
         GROUP_ENERGY(AN_GRP) = GROUP_ENERGY(AN_GRP) + ENERGY
         GROUP_VAROM(AN_GRP) = GROUP_VAROM(AN_GRP) + VAR_COST
         GROUP_FIXED_OM(AN_GRP)=GROUP_FIXED_OM(AN_GRP)+EL_FIXED_COST
         GROUP_EMISSIONS(1,AN_GRP) = GROUP_EMISSIONS(1,AN_GRP) + &
                                                            EL_SO2_EMIS
         MONTHLY_EL_GROUP_REPORT(SAVE_MONTH,EL_GRP,el_capacity) = &
           MONTHLY_EL_GROUP_REPORT(SAVE_MONTH,EL_GRP,el_capacity) + &
                                                               CAPACITY
! SAME AS CAPACITY FOR NOW
         MONTHLY_EL_GROUP_REPORT(SAVE_MONTH, &
                                     EL_GRP,el_eqivalent_capacity) = &
           MONTHLY_EL_GROUP_REPORT(SAVE_MONTH, &
                                    EL_GRP,el_eqivalent_capacity) + &
                                                              CAPACITY
!
         MONTHLY_EL_GROUP_REPORT(SAVE_MONTH,EL_GRP,Generation) = &
              MONTHLY_EL_GROUP_REPORT(SAVE_MONTH,EL_GRP,Generation) + &
                                                                ENERGY
         MONTHLY_EL_GROUP_REPORT(SAVE_MONTH,EL_GRP,variable_om) = &
           MONTHLY_EL_GROUP_REPORT(SAVE_MONTH,EL_GRP,variable_om) + &
                                                               VAR_COST
         MONTHLY_EL_GROUP_REPORT(SAVE_MONTH,EL_GRP,fixed_om) = &
              MONTHLY_EL_GROUP_REPORT(SAVE_MONTH,EL_GRP,fixed_om) + &
                                                          EL_FIXED_COST
         MONTHLY_EL_GROUP_REPORT(SAVE_MONTH,EL_GRP,sulfur_o2) = &
             MONTHLY_EL_GROUP_REPORT(SAVE_MONTH,EL_GRP,sulfur_o2) + &
                                                            EL_SO2_EMIS
! 042609.
!

         TEMP_L1 = GET_HYDRO_RPS_DATA(UNIT_NO,PM,ST_TG,ST,RPS_CONTRIB)
         TEMP_L1 = &
                  PUT_HYDRO_RPS_ENRG_CAP(UNIT_NO,ENERGY,CAPACITY,globecom_year)
         IF(ST > 0 .AND. PM /= 13) THEN
            TEMP_R4 = RPS_CONTRIB * CAPACITY
            ! call check_bounds_r4_4s(RPS_HYDRO_DB, "enrglimit:0001", &
                ! "rps_hydro_db", int(1,2), st, pm, save_month)
            RPS_HYDRO_DB(1,ST,PM,SAVE_MONTH) = &
                            RPS_HYDRO_DB(1,ST,PM,SAVE_MONTH) + &
                                       TEMP_R4
            RPS_HYDRO_DB(1,ST,PM,0) = &
                            RPS_HYDRO_DB(1,ST,PM,0) + &
                                       TEMP_R4
! GWH.
            TEMP_R4 = RPS_CONTRIB * ENERGY * 0.001
            RPS_HYDRO_DB(2,ST,PM,SAVE_MONTH) = &
                           RPS_HYDRO_DB(2,ST,PM,SAVE_MONTH) + TEMP_R4
            RPS_HYDRO_DB(2,ST,PM,0) = &
                                    RPS_HYDRO_DB(2,ST,PM,0) + TEMP_R4
         ENDIF
!
         IF(ST_TG > 0 .AND. PM /= 13) THEN
            ! call check_bounds_r4_4s(RPS_HYDRO_DB, "enrglimit:0003", &
             ! "rps_hydro_db", int(2,2), ST_TG, PM, SAVE_MONTH)
            
            
            TEMP_R4 = RPS_CONTRIB * CAPACITY
            RPS_HYDRO_DB(1,ST_TG,PM,SAVE_MONTH) = &
                            RPS_HYDRO_DB(1,ST_TG,PM,SAVE_MONTH) + &
                                       TEMP_R4
            RPS_HYDRO_DB(1,ST_TG,PM,0) = &
                            RPS_HYDRO_DB(1,ST_TG,PM,0) + &
                                       TEMP_R4
! GWH.
            TEMP_R4 = RPS_CONTRIB * ENERGY * 0.001
            RPS_HYDRO_DB(2,ST_TG,PM,SAVE_MONTH) = &
                         RPS_HYDRO_DB(2,ST_TG,PM,SAVE_MONTH) + TEMP_R4
            RPS_HYDRO_DB(2,ST_TG,PM,0) = &
                                   RPS_HYDRO_DB(2,ST_TG,PM,0) + TEMP_R4
         ENDIF
!
      ENDIF
!  6/20/96. GAT. ADDED FOR SRP.
      IF(SPECIAL_HYDRO_UNIT_ID(UNIT_NO) == "CAWCD") THEN
         CAWCD_ENERGY = CAWCD_ENERGY + ENERGY
      ENDIF
      UTIL_ENERGY = ENERGY
      IF(UTIL_ALLOCATION < 100.) THEN
         POOL_ALLOCATION = (100. - UTIL_ALLOCATION)/100.
         POOL_VAR_COST = POOL_ALLOCATION * VAR_COST
         VAR_COST = VAR_COST - POOL_VAR_COST
         POOL_FIXED_COST = POOL_ALLOCATION * EL_FIXED_COST
         EL_FIXED_COST = EL_FIXED_COST - POOL_FIXED_COST
         POOL_ENERGY = POOL_ALLOCATION * UTIL_ENERGY
         UTIL_ENERGY = UTIL_ENERGY - POOL_ENERGY
         POOL_SO2 = POOL_ALLOCATION * EL_SO2_EMIS
         IF(EXPENSE_TYPE /= 'R' .AND. EL_GRP /= -99) THEN
            P_CLASS_ASSIGNED_COST(2) = P_CLASS_ASSIGNED_COST(2) + &
                                       POOL_VAR_COST
            P_CLASS_ASSIGNED_ENERGY(2) = P_CLASS_ASSIGNED_ENERGY(2) + &
                                         POOL_ENERGY
            P_CLASS_ASSIGNED_FIXED_COST(2) = POOL_FIXED_COST + &
                                         P_CLASS_ASSIGNED_FIXED_COST(2)
            CLASS_ASSIGNED_EMISS(1,2) = &
                               CLASS_ASSIGNED_EMISS(1,2) + POOL_SO2
         ELSE
            P_CLASS_ASSIGNED_COST(2) = P_CLASS_ASSIGNED_COST(2) - &
                                       POOL_VAR_COST
            P_CLASS_ASSIGNED_ENERGY(2) = P_CLASS_ASSIGNED_ENERGY(2) - &
                                         POOL_ENERGY
            P_CLASS_ASSIGNED_FIXED_COST(2) = &
                       P_CLASS_ASSIGNED_FIXED_COST(2) - POOL_FIXED_COST
            CLASS_ASSIGNED_EMISS(1,2) = &
                               CLASS_ASSIGNED_EMISS(1,2) - POOL_SO2
         ENDIF
      ENDIF
      IF(EXPENSE_TYPE /= 'R' .AND. EL_GRP /= -99) THEN
         P_CLASS_ASSIGNED_COST(1) = P_CLASS_ASSIGNED_COST(1) + &
                                    VAR_COST
         P_CLASS_ASSIGNED_ENERGY(1) = P_CLASS_ASSIGNED_ENERGY(1) + &
                                      UTIL_ENERGY
         P_CLASS_ASSIGNED_FIXED_COST(1) = EL_FIXED_COST + &
                                       P_CLASS_ASSIGNED_FIXED_COST(1)
      ELSE
         P_CLASS_ASSIGNED_COST(1) = P_CLASS_ASSIGNED_COST(1) - &
                                    VAR_COST
         P_CLASS_ASSIGNED_ENERGY(1) = P_CLASS_ASSIGNED_ENERGY(1) - &
                                      UTIL_ENERGY
         P_CLASS_ASSIGNED_FIXED_COST(1) = &
                       P_CLASS_ASSIGNED_FIXED_COST(1) - EL_FIXED_COST
      ENDIF
!      12/3/93. GAT. ADDED MONTHLY_EL_REPORT.
      ASSET_CLASS_ACTIVE = .TRUE.
      TG = HYDRO_TRANS_GROUP(UNIT_NO)
!
      SALES_MULT = 1.0
!
      IF(MONTHLY_EL_REPORT_ACTIVE .OR. ASSET_CLASS_ACTIVE) THEN
         LOCAL_EL_NAME = EL_UNIT_NAME(UNIT_NO)
         IF(INDEX('OQA',HYTYPE(UNIT_NO)) /= 0) THEN
            LOCAL_EL_NAME = LOCAL_EL_NAME(1:18)//' O'
            EL_INDEX = 2
            IF(YES_HYDRO_SALES_AS_NEGATIVE ) THEN
               SALES_MULT = -1.0
            ENDIF
!
         ELSEIF(HYTYPE(UNIT_NO) == 'S' .AND. EL_GRP == -99) THEN
            LOCAL_EL_NAME = LOCAL_EL_NAME(1:18)//' S'
            EL_INDEX = 2
         ELSE
            IF(HYTYPE(UNIT_NO) == 'S') &
                              LOCAL_EL_NAME = LOCAL_EL_NAME(1:18)//' D'
            EL_INDEX = 1
         ENDIF
         IF(MON_EL_REPORT_NOT_OPEN .AND. MONTHLY_EL_REPORT_ACTIVE) THEN
            MON_EL_UNIT_NO = MON_EL_UNIT_HEADER(MON_EL_UNIT_REC)
            MON_EL_REPORT_NOT_OPEN = .FALSE.
!
            MAX_TRANS_GROUP_NUMBER = GET_MAX_TRANS_GROUP_NUMBER()
            IF(ALLOCATED(TRANS_GROUP_REPORTING_ACTIVE)) &
                               DEALLOCATE(TRANS_GROUP_REPORTING_ACTIVE)
            ALLOCATE( &
               TRANS_GROUP_REPORTING_ACTIVE( &
                                        MAX(1,MAX_TRANS_GROUP_NUMBER)))
            DO I = 1, MAX_TRANS_GROUP_NUMBER
               TRANS_GROUP_REPORTING_ACTIVE(I) = &
                                              GET_REPORT_CL_CAPACITY(I)
            ENDDO
!
         ENDIF
         LAST_SEASON = get_PRODUCTION_PERIODS_in()
       call write_trace_int2(file_trace_epc, "HYDRO_UNITS", HYDRO_UNITS)
       call write_trace_int2(file_trace_epc, "LS", LAST_SEASON)
         IF(.NOT. ALLOCATED(ANNUAL_EL_ENERGY)) THEN
! MONTHLY
!  8/21/02. ADDED FISCAL YEAR FOR THE SRP (INDEX -1)
            ALLOCATE(MON_MDS_EL_ENERGY(HYDRO_UNITS,2,0:12))
            ALLOCATE(MON_MDS_EL_CAPACITY(HYDRO_UNITS,2,0:12))
            ALLOCATE(MON_MDS_EL_VAR_COST(HYDRO_UNITS,2,0:12))
            ALLOCATE(MON_MDS_EL_FIXED_COST(HYDRO_UNITS,2,0:12))
            ALLOCATE(MON_MDS_EL_SO2_EMIS(HYDRO_UNITS,2,0:12))
            ALLOCATE(MON_MDS_EL_MARKET_REVENUE(HYDRO_UNITS,2,0:12))
            MON_MDS_EL_ENERGY = 0.
            MON_MDS_EL_CAPACITY = 0.
            MON_MDS_EL_VAR_COST = 0.
            MON_MDS_EL_FIXED_COST = 0.
            MON_MDS_EL_SO2_EMIS = 0.
            MON_MDS_EL_MARKET_REVENUE = 0.
! ANNUAL
            ALLOCATE(ANNUAL_EL_ENERGY(HYDRO_UNITS,2))
            ALLOCATE(ANNUAL_EL_CAPACITY(HYDRO_UNITS,2))
            ALLOCATE(ANNUAL_EL_VAR_COST(HYDRO_UNITS,2))
            ALLOCATE(ANNUAL_EL_FIXED_COST(HYDRO_UNITS,2))
            ALLOCATE(ANNUAL_EL_SO2_EMIS(HYDRO_UNITS,2))
            ANNUAL_EL_ENERGY = 0.
            ANNUAL_EL_CAPACITY = 0.
            ANNUAL_EL_VAR_COST = 0.
            ANNUAL_EL_FIXED_COST = 0.
            ANNUAL_EL_SO2_EMIS = 0.
         ENDIF
!
! MONTHLY
         MON_MDS_EL_ENERGY(UNIT_NO,EL_INDEX,SAVE_MONTH) = &
                MON_MDS_EL_ENERGY(UNIT_NO,EL_INDEX,SAVE_MONTH) + ENERGY
         MON_MDS_EL_CAPACITY(UNIT_NO,EL_INDEX,SAVE_MONTH) = &
             MON_MDS_EL_CAPACITY(UNIT_NO,EL_INDEX,SAVE_MONTH) + &
                                                               CAPACITY

         MON_MDS_EL_VAR_COST(UNIT_NO,EL_INDEX,SAVE_MONTH) = &
               VAR_COST + &
                MON_MDS_EL_VAR_COST(UNIT_NO,EL_INDEX,SAVE_MONTH)
         MON_MDS_EL_FIXED_COST(UNIT_NO,EL_INDEX,SAVE_MONTH) = &
              EL_FIXED_COST + &
              MON_MDS_EL_FIXED_COST(UNIT_NO,EL_INDEX,SAVE_MONTH)
         MON_MDS_EL_SO2_EMIS(UNIT_NO,EL_INDEX,SAVE_MONTH) = &
               EL_SO2_EMIS + &
                MON_MDS_EL_SO2_EMIS(UNIT_NO,EL_INDEX,SAVE_MONTH)
         MON_MDS_EL_ENERGY(UNIT_NO,EL_INDEX,0) = &
                         MON_MDS_EL_ENERGY(UNIT_NO,EL_INDEX,0) + ENERGY
         MON_MDS_EL_CAPACITY(UNIT_NO,EL_INDEX,0) = &
             MON_MDS_EL_CAPACITY(UNIT_NO,EL_INDEX,0) + &
                                                   CAPACITY/LAST_SEASON
         MON_MDS_EL_VAR_COST(UNIT_NO,EL_INDEX,0) = VAR_COST + &
                                MON_MDS_EL_VAR_COST(UNIT_NO,EL_INDEX,0)
         MON_MDS_EL_FIXED_COST(UNIT_NO,EL_INDEX,0) = EL_FIXED_COST + &
                              MON_MDS_EL_FIXED_COST(UNIT_NO,EL_INDEX,0)
         MON_MDS_EL_SO2_EMIS(UNIT_NO,EL_INDEX,0) = EL_SO2_EMIS + &
                                MON_MDS_EL_SO2_EMIS(UNIT_NO,EL_INDEX,0)
! FISCAL. 8/21/02.
         FISCAL_EL_ENERGY(UNIT_NO,EL_INDEX) = &
                         FISCAL_EL_ENERGY(UNIT_NO,EL_INDEX) + ENERGY
         FISCAL_EL_CAPACITY(UNIT_NO,EL_INDEX) = &
             FISCAL_EL_CAPACITY(UNIT_NO,EL_INDEX) + &
                                                   CAPACITY/LAST_SEASON
         FISCAL_EL_VAR_COST(UNIT_NO,EL_INDEX) = VAR_COST + &
                                FISCAL_EL_VAR_COST(UNIT_NO,EL_INDEX)
         FISCAL_EL_FIXED_COST(UNIT_NO,EL_INDEX) = EL_FIXED_COST + &
                              FISCAL_EL_FIXED_COST(UNIT_NO,EL_INDEX)
         FISCAL_EL_SO2_EMIS(UNIT_NO,EL_INDEX) = EL_SO2_EMIS + &
                                FISCAL_EL_SO2_EMIS(UNIT_NO,EL_INDEX)
! ANNUAL
         ANNUAL_EL_ENERGY(UNIT_NO,EL_INDEX) = &
                            ANNUAL_EL_ENERGY(UNIT_NO,EL_INDEX) + ENERGY
         ANNUAL_EL_CAPACITY(UNIT_NO,EL_INDEX) = &
            ANNUAL_EL_CAPACITY(UNIT_NO,EL_INDEX) + CAPACITY/LAST_SEASON
         ANNUAL_EL_VAR_COST(UNIT_NO,EL_INDEX) = VAR_COST + &
                                   ANNUAL_EL_VAR_COST(UNIT_NO,EL_INDEX)
         ANNUAL_EL_FIXED_COST(UNIT_NO,EL_INDEX) = EL_FIXED_COST + &
                                 ANNUAL_EL_FIXED_COST(UNIT_NO,EL_INDEX)
         ANNUAL_EL_SO2_EMIS(UNIT_NO,EL_INDEX) = EL_SO2_EMIS + &
                                   ANNUAL_EL_SO2_EMIS(UNIT_NO,EL_INDEX)

      ENDIF
!
      RETURN


      ENTRY UPDATE_MON_PW_TRANS_UNIT_REC(R_MON_PW_TRANS_UNIT_REC)

         MON_PW_TRANS_UNIT_REC = MON_PW_TRANS_UNIT_REC + 1
         R_MON_PW_TRANS_UNIT_REC = MON_PW_TRANS_UNIT_REC
      RETURN

      ENTRY INITIALIZE_CAWCD_ENERGY

         CAWCD_ENERGY = 0.
      RETURN

      ENTRY SET_EL_POOLING_HOURS(R_HOURS)

         HOURS = R_HOURS
      RETURN



      ENTRY ANNUAL_EL_REPORT

         RUN_TRANSACT = YES_RUN_TRANSACT()
!
         PRICE_ONLY_WHOLESALE_REV = APPLY_TRANS_REV_TO_WHOLESALE()
!
         IF(TESTING_PLAN .OR. &
              .NOT. (MONTHLY_EL_REPORT_ACTIVE .OR. RUN_TRANSACT) .OR. &
               .NOT. ALLOCATED(MON_MDS_EL_ENERGY) .OR. &
                                          LAST_SEASON == 1) RETURN

         CURRENT_YEAR = globecom_year+get_BASE_YEAR()
         CURRENT_YEAR_COMPARISON = (CURRENT_YEAR-1900)*100
!
         MONTHLY_PW_REPORT = YES_POWERWORLD_REPORT()
 4445  FORMAT(1X,4A)
!
! MONTHLY LOOP
!
         DO UNIT = 1 , HYDRO_UNITS
! REMOVED 7/6/00 PER SRP.
            LOCAL_EL_NAME = EL_UNIT_NAME(UNIT)
            TG = HYDRO_TRANS_GROUP(UNIT)
! 10/04/01
            TEMP_L1 = GET_EL_BASECASE_MARKET_ID(UNIT,MARKET_ID)
            MK = MARKET_AREA_LOOKUP(MARKET_ID(1:5))
            RTG = TG
            RMK = MK
            RFT = 5.0
!
            IF(INDEX('OQAS',HYTYPE(UNIT)) /= 0) THEN
               IF(HYTYPE(UNIT) == 'S') THEN
                  LOCAL_EL_NAME = LOCAL_EL_NAME(1:18)//' S'
                  REVENUE_FACTOR = 80. ! FOR ARTMAN. 10/5/99. GAT.
                  IF(RUN_TRANSACT) THEN
                     CALL CF_2_ANNUAL_REVENUE(REVENUE_FACTOR, &
                            AVE_REVENUE,LOCAL_EL_NAME,INT2(SAVE_MONTH))
                     MARKET_REVENUE = &
                                - AVE_REVENUE * &
                                   MON_MDS_EL_ENERGY(UNIT,2,SAVE_MONTH)
                  ELSE
                     MARKET_REVENUE = 0.0
                  ENDIF
                  IF(MON_MDS_EL_CAPACITY(UNIT,2,SAVE_MONTH) /= 0.) THEN
                     CAPACITY_FACTOR = &
                        100.*MON_MDS_EL_ENERGY(UNIT,2,SAVE_MONTH)/ &
                          (MON_MDS_EL_CAPACITY(UNIT,2,SAVE_MONTH)* &
                                                  SAVE_HOURS_IN_PERIOD)
                  ELSE
                     CAPACITY_FACTOR = 0.
                  ENDIF
               ELSE
                  LOCAL_EL_NAME = LOCAL_EL_NAME(1:18)//' O'
                  CAPACITY_FACTOR = 0.
                  IF(MON_MDS_EL_CAPACITY(UNIT,2,SAVE_MONTH) /= 0.) THEN
                     CAPACITY_FACTOR = &
                           100.*MON_MDS_EL_ENERGY(UNIT,2,SAVE_MONTH)/ &
                          (MON_MDS_EL_CAPACITY(UNIT,2,SAVE_MONTH)* &
                                                  SAVE_HOURS_IN_PERIOD)
                  ENDIF
                  REVENUE_FACTOR = CAPACITY_FACTOR
                  IF(RUN_TRANSACT .AND. REVENUE_FACTOR > 0.) THEN
                     CALL CF_2_ANNUAL_REVENUE(REVENUE_FACTOR, &
                            AVE_REVENUE,LOCAL_EL_NAME,INT2(SAVE_MONTH))
                     MARKET_REVENUE = &
                              AVE_REVENUE * &
                                   MON_MDS_EL_ENERGY(UNIT,2,SAVE_MONTH)
                  ELSE
                     MARKET_REVENUE = 0.0
                  ENDIF
               ENDIF
               IF(REPORT_THIS_EL_UNIT(UNIT) .AND. &
                                 MONTHLY_EL_REPORT_ACTIVE) THEN

                  IF(YES_HYDRO_SALES_AS_NEGATIVE ) THEN
                     SALES_MULT = -1.0
                  ELSE
                     SALES_MULT =  1.0
                  ENDIF
! CHANGED THE SIGN OF MARKET_REVENUE FOR STEVE MACY. 10/20/99.
               MARKET_REVENUE = -1*MARKET_REVENUE*SALES_MULT/1000000.
!
               IF(.NOT. PRICE_ONLY_WHOLESALE_REV) THEN
!
                  MON_MDS_EL_MARKET_REVENUE(UNIT,2,SAVE_MONTH) = &
                     MON_MDS_EL_MARKET_REVENUE(UNIT,2,SAVE_MONTH) + &
                                                MARKET_REVENUE*1000000.
                  MON_MDS_EL_MARKET_REVENUE(UNIT,2,0) = &
                     MON_MDS_EL_MARKET_REVENUE(UNIT,2,0) + &
                                                MARKET_REVENUE*1000000.

               ENDIF
! 12/03/03. MOVED FOR PETREY
               FISCAL_EL_MARKET_REVENUE(UNIT,2) = &
                     FISCAL_EL_MARKET_REVENUE(UNIT,2) + &
                                                MARKET_REVENUE*1000000.
!
                  MON_EL_UNIT_REC = rptrec(MON_EL_UNIT_NO)
                  WRITE(MON_EL_UNIT_NO,REC=MON_EL_UNIT_REC) &
                      PRT_ENDPOINT(), &
                      FLOAT(LOCAL_YEAR), &
                      el_MONTH,LOCAL_EL_NAME, &
                      FLOAT(UNIT), &
                   MON_MDS_EL_CAPACITY(UNIT,2,SAVE_MONTH)*SALES_MULT, &
                     MON_MDS_EL_ENERGY(UNIT,2,SAVE_MONTH)*SALES_MULT, &
                      MON_MDS_EL_VAR_COST(UNIT,2,SAVE_MONTH)* &
                                                 SALES_MULT/1000000., &
                      MON_MDS_EL_FIXED_COST(UNIT,2,SAVE_MONTH)* &
                                                 SALES_MULT/1000000., &
                   MON_MDS_EL_SO2_EMIS(UNIT,2,SAVE_MONTH)*SALES_MULT, &
                      CAPACITY_FACTOR, &
                      MARKET_REVENUE, &
                      RTG,RMK,RFT
                  MON_EL_UNIT_REC = MON_EL_UNIT_REC + 1
               ENDIF
!
            ENDIF
            IF(MONTHLY_PW_REPORT) THEN
                  IF(INDEX('OQAS',HYTYPE(UNIT)) /= 0) THEN
                     TECH_INDEX = 2
                  ELSE
                     TECH_INDEX = 1
                  ENDIF
!
                  HESI_UNIT_ID_NUM = FLOAT(EL_RESOURCE_ID(UNIT))
                  TG_POSITION = MAX(1,GET_TRANS_GROUP_POSITION(TG))
                  POWERDAT_PLANT_ID = GET_POWERDAT_PLANT_ID(UNIT)
                  TEMP_L1 = &
                      GET_PW_EL_UNIT_TEXT_FIELDS( &
                           UNIT, &
                           EIA_PLANT_CODE)


                  TEMP_L1 = GET_PORT_MARKET_AREA_ABBREV( &
                                              RDI_MARKET_AREA_NAME,RMK)

! TEST FOR MONOTONIC NON-DECREASING.
!

                     IF(MON_MDS_EL_CAPACITY( &
                                        UNIT,TECH_INDEX,SAVE_MONTH) > &
                                                               0.) THEN
                        FIXED_COST_PER_UNIT = &
                          .001*MON_MDS_EL_FIXED_COST( &
                                         UNIT,TECH_INDEX,SAVE_MONTH)/ &
                                  MON_MDS_EL_CAPACITY( &
                                           UNIT,TECH_INDEX,SAVE_MONTH)
                     ELSE
                        FIXED_COST_PER_UNIT = 0.
                     ENDIF
                     IF(MON_MDS_EL_ENERGY( &
                                UNIT,TECH_INDEX,SAVE_MONTH) > 0.) THEN
                        VARIABLE_COST_PER_UNIT = &
                                 MON_MDS_EL_VAR_COST( &
                                         UNIT,TECH_INDEX,SAVE_MONTH)/ &
                                    MON_MDS_EL_ENERGY( &
                                           UNIT,TECH_INDEX,SAVE_MONTH)
                     ELSE
                        VARIABLE_COST_PER_UNIT = 0.
                     ENDIF
                     MULTI_AREA_NAME = GET_GROUP_NAME(TG_POSITION)
                     PW_REC = ' '
!                     INQUIRE(UNIT=4321,NEXTREC=MON_PW_TRANS_UNIT_REC)
                     WRITE(PW_REC,4321) &
                   PRT_ENDPOINT(),                      ',', & !  F4.0
                 FLOAT(LOCAL_YEAR),                     ',', & !  F4.0
                    QUOTE,CL_MONTH_NAME(SAVE_MONTH),QUOTE, ',', & !  A
                     QUOTE,LOCAL_EL_NAME,QUOTE,             ',', & !  A
                HESI_UNIT_ID_NUM,                      ',', & !  F12.0
                POWERDAT_PLANT_ID,                     ',', & !  F8.0
               QUOTE,EIA_PLANT_CODE,QUOTE,            ',', & !  A
                RMK,                                   ',', & !  F4.0
               QUOTE,RDI_MARKET_AREA_NAME,QUOTE,      ',', & !  A
              RTG,                                   ',', & !  F4.0
              QUOTE,MULTI_AREA_NAME,QUOTE,           ',', & !  A
               0.0,                                   ',', & !  F8.1
                       MON_MDS_EL_CAPACITY(UNIT, &
                                TECH_INDEX,SAVE_MONTH),',', & !  F8.1
                        FIXED_COST_PER_UNIT,                   ',', &
            0.0,                                   ',', & !  F7.3
           RFT,                                   ',', & !  F3.0
            VARIABLE_COST_PER_UNIT,                ',', & !  F7.3
              0.0,                                   ',', & !  F8.4
              10.0,                                  ',', & !  F8.4
               0.0,                                   ',', & !  F8.4
                 0.0,                                   ','  ! F8.4
                     WRITE(4444,4445) trim(PW_REC)
!
 4321                FORMAT( &
                         1X,F5.0,   A, &
                         1X,F5.0,   A, &
                         1X,A,A,A,A, &
                         1X,A,A,A,A, &
                         1X,F15.0,  A, &
                         1X,F15.0,  A, &
                         1X,A,A,A,A, &
                         1X,F6.0,   A, &
                         1X,A,A,A,A, &
                     1X,F6.0,   A,   & !  MULTI-AREA TRANSACTION GROUP
                         1X,A,A,A,A,   & !  MULTI-AREA NAME
                         1X,F8.1,   A, & !  MIN CAPACITY
                         1X,F8.1,   A, & !  MAX CAPACITY
                         1X,F7.3,   A, & !  FIXED COST
                         1X,F7.3,   A, & !  AVE FUEL COST
                         1X,F3.0,   A, & !  FUEL TYPE INDEX
                         1X,F7.3,   A, & !  VOM
                         1X,F12.4, A, & !  A COEFFICIENT
                         1X,F10.4, A, & !  B COEFFICIENT
                         1X,F14.10,   A,  & !  COEFFICIENT
                         1X,F14.12,   A) ! D COEFFICIENT
!
                     MON_PW_TRANS_UNIT_REC = MON_PW_TRANS_UNIT_REC + 1
            ENDIF ! PW REPORT
!
            IF(INDEX('OQA',HYTYPE(UNIT)) == 0) THEN
               CAPACITY_FACTOR = 0.
               IF(MON_MDS_EL_CAPACITY(UNIT,1,SAVE_MONTH) /= 0.) THEN
                  CAPACITY_FACTOR = 100.* &
                        MON_MDS_EL_ENERGY(UNIT,1,SAVE_MONTH)/ &
                          (MON_MDS_EL_CAPACITY(UNIT,1,SAVE_MONTH)* &
                                                  SAVE_HOURS_IN_PERIOD)
               ENDIF
               IF(HYTYPE(UNIT) == 'S') &
                              LOCAL_EL_NAME = LOCAL_EL_NAME(1:18)//' D'
               IF(RUN_TRANSACT .AND. CAPACITY_FACTOR > 0.) THEN
                  REVENUE_FACTOR = CAPACITY_FACTOR
                  CALL CF_2_ANNUAL_REVENUE(REVENUE_FACTOR, &
                            AVE_REVENUE,LOCAL_EL_NAME,INT2(SAVE_MONTH))
                  MARKET_REVENUE = &
                                  AVE_REVENUE * &
                        MON_MDS_EL_ENERGY(UNIT,1,SAVE_MONTH)/1000000.
               ELSE
                  MARKET_REVENUE = 0.0
               ENDIF
!
               IF(REPORT_THIS_EL_UNIT(UNIT) .AND. &
                                         MONTHLY_EL_REPORT_ACTIVE) THEN

                  MON_EL_UNIT_REC = rptrec(MON_EL_UNIT_NO)
                  WRITE(MON_EL_UNIT_NO,REC=MON_EL_UNIT_REC) &
                               PRT_ENDPOINT(), &
                               FLOAT(LOCAL_YEAR), &
                               el_MONTH,LOCAL_EL_NAME, &
                               FLOAT(UNIT), &
                              MON_MDS_EL_CAPACITY(UNIT,1,SAVE_MONTH), &
                               MON_MDS_EL_ENERGY(UNIT,1,SAVE_MONTH), &
                              MON_MDS_EL_VAR_COST(UNIT,1,SAVE_MONTH)/ &
                                                            1000000., &
                            MON_MDS_EL_FIXED_COST(UNIT,1,SAVE_MONTH)/ &
                                                            1000000., &
                             MON_MDS_EL_SO2_EMIS(UNIT,1,SAVE_MONTH), &
                               CAPACITY_FACTOR, &
                               MARKET_REVENUE, &
                               RTG,RMK,RFT
                  MON_EL_UNIT_REC = MON_EL_UNIT_REC + 1
               ENDIF
               IF(.NOT. PRICE_ONLY_WHOLESALE_REV) THEN
                  MON_MDS_EL_MARKET_REVENUE(UNIT,1,SAVE_MONTH) = &
                     MON_MDS_EL_MARKET_REVENUE(UNIT,1,SAVE_MONTH) + &
                                                MARKET_REVENUE*1000000.
                  MON_MDS_EL_MARKET_REVENUE(UNIT,1,0) = &
                     MON_MDS_EL_MARKET_REVENUE(UNIT,1,0) + &
                                                MARKET_REVENUE*1000000.

               ENDIF
! 12/03/03 MOVED FOR PETREY.
               FISCAL_EL_MARKET_REVENUE(UNIT,1) = &
                     FISCAL_EL_MARKET_REVENUE(UNIT,1) + &
                                                MARKET_REVENUE*1000000.
!
            ENDIF
         ENDDO ! MONTHLY LOOP
!
! ANNUAL LOOP
         IF(SAVE_MONTH == LAST_SEASON) THEN
            DO UNIT = 1 , HYDRO_UNITS
               LOCAL_EL_NAME = EL_UNIT_NAME(UNIT)
               TG = HYDRO_TRANS_GROUP(UNIT)
! 10/04/01
               TEMP_L1 = GET_EL_BASECASE_MARKET_ID(UNIT,MARKET_ID)
               MK = MARKET_AREA_LOOKUP(MARKET_ID(1:5))
               RTG = TG
               RMK = MK
               RFT = 5.0
!
               IF(INDEX('OQAS',HYTYPE(UNIT)) /= 0) THEN
                  IF(HYTYPE(UNIT) == 'S') THEN
                     LOCAL_EL_NAME = LOCAL_EL_NAME(1:18)//' S'
                     IF(MON_MDS_EL_CAPACITY(UNIT,2,0) /= 0.) THEN
                        CAPACITY_FACTOR = &
                           100.*MON_MDS_EL_ENERGY(UNIT,2,0)/ &
                           (MON_MDS_EL_CAPACITY(UNIT,2,0)*8760.)
                     ELSE
                        CAPACITY_FACTOR = 0.
                     ENDIF
                  ELSE
                     LOCAL_EL_NAME = LOCAL_EL_NAME(1:18)//' O'
                     CAPACITY_FACTOR = 0.
                     IF(MON_MDS_EL_CAPACITY(UNIT,2,0) /= 0.) THEN
                        CAPACITY_FACTOR = &
                           100.*MON_MDS_EL_ENERGY(UNIT,2,0)/ &
                          (MON_MDS_EL_CAPACITY(UNIT,2,0)*8760.)
                     ENDIF
                  ENDIF
                MARKET_REVENUE = MON_MDS_EL_MARKET_REVENUE(UNIT,2,0)/ &
                                                               1000000.
                  IF(REPORT_THIS_EL_UNIT(UNIT) .AND. &
                                 MONTHLY_EL_REPORT_ACTIVE .AND. &
                                                .NOT. FISCAL_ONLY) THEN
                     IF(YES_HYDRO_SALES_AS_NEGATIVE ) THEN
                        SALES_MULT = -1.0
                     ELSE
                        SALES_MULT =  1.0
                     ENDIF
                     MON_EL_UNIT_REC = rptrec(MON_EL_UNIT_NO)
                     WRITE(MON_EL_UNIT_NO,REC=MON_EL_UNIT_REC) &
                        PRT_ENDPOINT(), &
                        FLOAT(LOCAL_YEAR), &
                        'Annual   ',LOCAL_EL_NAME, &
                        FLOAT(UNIT), &
                        MON_MDS_EL_CAPACITY(UNIT,2,0)*SALES_MULT, &
                        MON_MDS_EL_ENERGY(UNIT,2,0)*SALES_MULT, &
                        MON_MDS_EL_VAR_COST(UNIT,2,0)* &
                                                 SALES_MULT/1000000., &
                        MON_MDS_EL_FIXED_COST(UNIT,2,0)* &
                                                SALES_MULT/1000000., &
                        MON_MDS_EL_SO2_EMIS(UNIT,2,0)*SALES_MULT, &
                        CAPACITY_FACTOR, &
                        MARKET_REVENUE, &
                        RTG,RMK,RFT
                     MON_EL_UNIT_REC = MON_EL_UNIT_REC + 1
!
                  ENDIF
               ENDIF ! 10/26/01. MOVED ORDER OF UNITS.
               IF(INDEX('OQA',HYTYPE(UNIT)) == 0) THEN
                  CAPACITY_FACTOR = 0.
                  IF(MON_MDS_EL_CAPACITY(UNIT,1,0) /= 0.) THEN
                     CAPACITY_FACTOR = 100.* &
                        MON_MDS_EL_ENERGY(UNIT,1,0)/ &
                          (MON_MDS_EL_CAPACITY(UNIT,1,0)*8760.)
                  ENDIF
                  IF(HYTYPE(UNIT) == 'S') &
                              LOCAL_EL_NAME = LOCAL_EL_NAME(1:18)//' D'
                MARKET_REVENUE = MON_MDS_EL_MARKET_REVENUE(UNIT,1,0)/ &
                                                               1000000.
!
                  IF(REPORT_THIS_EL_UNIT(UNIT) .AND. &
                                 MONTHLY_EL_REPORT_ACTIVE .AND. &
                                                .NOT. FISCAL_ONLY) THEN
                     MON_EL_UNIT_REC = rptrec(MON_EL_UNIT_NO)
                     WRITE(MON_EL_UNIT_NO,REC=MON_EL_UNIT_REC) &
                               PRT_ENDPOINT(), &
                               FLOAT(LOCAL_YEAR), &
                               'Annual   ',LOCAL_EL_NAME, &
                               FLOAT(UNIT), &
                               MON_MDS_EL_CAPACITY(UNIT,1,0), &
                               MON_MDS_EL_ENERGY(UNIT,1,0), &
                               MON_MDS_EL_VAR_COST(UNIT,1,0)/ &
                                                            1000000., &
                               MON_MDS_EL_FIXED_COST(UNIT,1,0)/ &
                                                            1000000., &
                               MON_MDS_EL_SO2_EMIS(UNIT,1,0), &
                               CAPACITY_FACTOR, &
                               MARKET_REVENUE, &
                               RTG,RMK,RFT
                     MON_EL_UNIT_REC = MON_EL_UNIT_REC + 1
                  ENDIF
               ENDIF ! NOT SALE
            ENDDO ! HYDRO UNITS LOOP
         ENDIF ! ANNUAL LOOP
!
! FISCAL REPORTING LOOP
        IF(YES_FISCAL_REPORTING .AND. SAVE_MONTH == FISCAL_SEASON) THEN
!         IF(SAVE_MONTH == LAST_SEASON) THEN
            DO UNIT = 1 , HYDRO_UNITS
               LOCAL_EL_NAME = EL_UNIT_NAME(UNIT)
               TG = HYDRO_TRANS_GROUP(UNIT)
! 10/04/01
               TEMP_L1 = GET_EL_BASECASE_MARKET_ID(UNIT,MARKET_ID)
               MK = MARKET_AREA_LOOKUP(MARKET_ID(1:5))
               RTG = TG
               RMK = MK
               RFT = 5.0
!
               IF(INDEX('OQAS',HYTYPE(UNIT)) /= 0) THEN
                  IF(HYTYPE(UNIT) == 'S') THEN
                     LOCAL_EL_NAME = LOCAL_EL_NAME(1:18)//' S'
                     IF(FISCAL_EL_CAPACITY(UNIT,2) /= 0.) THEN
                        CAPACITY_FACTOR = &
                           100.*FISCAL_EL_ENERGY(UNIT,2)/ &
                           (FISCAL_EL_CAPACITY(UNIT,2)*8760.)
                     ELSE
                        CAPACITY_FACTOR = 0.
                     ENDIF
                  ELSE
                     LOCAL_EL_NAME = LOCAL_EL_NAME(1:18)//' O'
                     CAPACITY_FACTOR = 0.
                     IF(FISCAL_EL_CAPACITY(UNIT,2) /= 0.) THEN
                        CAPACITY_FACTOR = &
                           100.*FISCAL_EL_ENERGY(UNIT,2)/ &
                          (FISCAL_EL_CAPACITY(UNIT,2)*8760.)
                     ENDIF
                  ENDIF
                  MARKET_REVENUE = &
                                  FISCAL_EL_MARKET_REVENUE(UNIT,2)/ &
                                                               1000000.
                  IF(REPORT_THIS_EL_UNIT(UNIT) .AND. &
                                 MONTHLY_EL_REPORT_ACTIVE) THEN
                     IF(YES_HYDRO_SALES_AS_NEGATIVE ) THEN
                        SALES_MULT = -1.0
                     ELSE
                        SALES_MULT =  1.0
                     ENDIF
                     MON_EL_UNIT_REC = rptrec(MON_EL_UNIT_NO)
                     WRITE(MON_EL_UNIT_NO,REC=MON_EL_UNIT_REC) &
                        PRT_ENDPOINT(), &
                        FLOAT(LOCAL_YEAR), &
                        'Fiscal Yr',LOCAL_EL_NAME, &
                        FLOAT(UNIT), &
                        FISCAL_EL_CAPACITY(UNIT,2)*SALES_MULT, &
                        FISCAL_EL_ENERGY(UNIT,2)*SALES_MULT, &
                        FISCAL_EL_VAR_COST(UNIT,2)* &
                                                 SALES_MULT/1000000., &
                        FISCAL_EL_FIXED_COST(UNIT,2)* &
                                                 SALES_MULT/1000000., &
                        FISCAL_EL_SO2_EMIS(UNIT,2)*SALES_MULT, &
                        CAPACITY_FACTOR, &
                        MARKET_REVENUE, &
                        RTG,RMK,RFT
                     MON_EL_UNIT_REC = MON_EL_UNIT_REC + 1
!
                  ENDIF
               ENDIF ! 10/26/01. MOVED ORDER OF UNITS.
               IF(INDEX('OQA',HYTYPE(UNIT)) == 0) THEN
                  CAPACITY_FACTOR = 0.
                  IF(FISCAL_EL_CAPACITY(UNIT,1) /= 0.) THEN
                     CAPACITY_FACTOR = 100.* &
                        FISCAL_EL_ENERGY(UNIT,1)/ &
                          (FISCAL_EL_CAPACITY(UNIT,1)*8760.)
                  ENDIF
                  IF(HYTYPE(UNIT) == 'S') &
                              LOCAL_EL_NAME = LOCAL_EL_NAME(1:18)//' D'
                  MARKET_REVENUE = FISCAL_EL_MARKET_REVENUE(UNIT,1)/ &
                                                               1000000.
!
                  IF(REPORT_THIS_EL_UNIT(UNIT) .AND. &
                                         MONTHLY_EL_REPORT_ACTIVE) THEN
                     MON_EL_UNIT_REC = rptrec(MON_EL_UNIT_NO)
                     WRITE(MON_EL_UNIT_NO,REC=MON_EL_UNIT_REC) &
                               PRT_ENDPOINT(), &
                               FLOAT(LOCAL_YEAR), &
                               'Fiscal Yr',LOCAL_EL_NAME, &
                               FLOAT(UNIT), &
                               FISCAL_EL_CAPACITY(UNIT,1), &
                               FISCAL_EL_ENERGY(UNIT,1), &
                               FISCAL_EL_VAR_COST(UNIT,1)/ &
                                                            1000000., &
                               FISCAL_EL_FIXED_COST(UNIT,1)/ &
                                                            1000000., &
                               FISCAL_EL_SO2_EMIS(UNIT,1), &
                               CAPACITY_FACTOR, &
                               MARKET_REVENUE, &
                               RTG,RMK,RFT
                     MON_EL_UNIT_REC = MON_EL_UNIT_REC + 1
                  ENDIF
               ENDIF ! NOT SALE
            ENDDO ! HYDRO UNITS LOOP
            IF(ALLOCATED(FISCAL_EL_ENERGY)) &
               DEALLOCATE( FISCAL_EL_CAPACITY,FISCAL_EL_ENERGY, &
                           FISCAL_EL_VAR_COST,FISCAL_EL_FIXED_COST, &
                           FISCAL_EL_SO2_EMIS, &
                           FISCAL_EL_MARKET_REVENUE)
         ELSEIF(.NOT. YES_FISCAL_REPORTING .AND. &
                                        SAVE_MONTH == LAST_SEASON) THEN
            IF(ALLOCATED(FISCAL_EL_ENERGY)) &
               DEALLOCATE( FISCAL_EL_CAPACITY,FISCAL_EL_ENERGY, &
                           FISCAL_EL_VAR_COST,FISCAL_EL_FIXED_COST, &
                           FISCAL_EL_SO2_EMIS, &
                           FISCAL_EL_MARKET_REVENUE)
         ENDIF ! FISCAL LOOP
      RETURN
      ENTRY STORE_NEW_EL_ASSET_CLASS(R_NEW_UNIT,R_REF_UNIT)

         IF(LAST_EL_UNIT+1 /= R_NEW_UNIT) THEN
            WRITE(4,*) "In the Energy Limited Units Asset Allocation"
            WRITE(4,*) "the new unit does not match the last unit+1."
            WRITE(4,*) " "
         ENDIF
         ns_service_decs%ASSET_CLASS_NUM(R_NEW_UNIT) = &
            ns_service_decs%ASSET_CLASS_NUM(R_REF_UNIT)
            
        ns_service_decs%ASSET_CLASS_VECTOR(R_NEW_UNIT) = &
            ns_service_decs%ASSET_CLASS_VECTOR(R_REF_UNIT)
         ns_service_decs%INTRA_COMPANY_CLASS_ID(R_NEW_UNIT) = &
                                     ns_service_decs%INTRA_COMPANY_CLASS_ID(R_REF_UNIT)
         LAST_EL_UNIT = R_NEW_UNIT
      RETURN

      ENTRY CALC_EL_ANN_ASSET_CLASS

         CURRENT_YEAR = globecom_year+get_BASE_YEAR()
         CURRENT_YEAR_COMPARISON = (CURRENT_YEAR-1900)*100
         EL_ANN_CLASS_VAR_COST = 0.
!         RPS_HYDRO_DB = 0.
         EL_ANN_CLASS_FIXED_COST = 0.
         EL_ANN_CLASS_REVENUE = 0.
         EL_ANN_CLASS_CAPACITY = 0.
         EL_ANN_CLASS_ENERGY = 0.
         EL_ANN_CLASS_SELL_CAPACITY = 0.
         EL_ANN_CLASS_SELL_ENERGY = 0.
         EL_ANN_CLASS_SO2 = 0.
         EL_ANN_CLASS_MARKET_REVENUES = 0.
         EL_ANN_CLASS_MARKET_PURCHASE = 0.
         EL_ANN_CLASS_PURCHASES = 0.
         INTRA_COMPANY_PURCHASE_EXPENSE = 0.
         INTRA_COMPANY_SALES_REVENUE = 0.
         INTRA_SALES_REVENUE = 0.
         INTRA_PURCHASE_EXPENSES = 0.
         INTRA_VARIABLE_EXPENSES = 0.
         INTRA_FIXED_EXPENSES = 0.
         MAX_INTRA_CLASS = 0
         IF(HYDRO_UNITS <= 0 .OR. &
                              .NOT. ALLOCATED(ANNUAL_EL_ENERGY)) RETURN

         DO UNIT = 1, HYDRO_UNITS ! LAST_EL_UNIT
            IF(ON_LINE(UNIT) - CURRENT_YEAR_COMPARISON > 12  .OR. &
                   CURRENT_YEAR_COMPARISON - OFF_LINE(UNIT) > 12) CYCLE
            IF(ns_service_decs%INTRA_COMPANY_CLASS_ID(UNIT) > 0) THEN
               ASSET_CLASS = ns_service_decs%INTRA_COMPANY_CLASS_ID(UNIT)
               CALL CHECK_IF_CLASS_DEFINED(ASSET_CLASS)
               ASSET_CLASS = ASSET_CLASS + 1
               MAX_INTRA_CLASS = MAX(MAX_INTRA_CLASS,ASSET_CLASS)
               IF(INDEX('OQA',HYTYPE(UNIT)) /= 0) THEN
                  VAR_PLUS_FIXED_COST = ANNUAL_EL_VAR_COST(UNIT,2) + &
                                           ANNUAL_EL_FIXED_COST(UNIT,2)
                  INTRA_COMPANY_PURCHASE_EXPENSE(ASSET_CLASS) = &
                        INTRA_COMPANY_PURCHASE_EXPENSE(ASSET_CLASS) + &
                                                    VAR_PLUS_FIXED_COST
                  SELECT CASE (HYDRO_EXPENSE_ASSIGNMENT(UNIT))
                  CASE('R')  ! SECONDARY SLAES
                     INTRA_SALES_REVENUE = INTRA_SALES_REVENUE + &
                                                    VAR_PLUS_FIXED_COST
                  CASE('P')  ! PURCHASE REVENUES
                  INTRA_PURCHASE_EXPENSES = INTRA_PURCHASE_EXPENSES - &
                                                    VAR_PLUS_FIXED_COST
                  CASE DEFAULT
                  INTRA_VARIABLE_EXPENSES = INTRA_VARIABLE_EXPENSES - &
                                            ANNUAL_EL_VAR_COST(UNIT,2)
                     INTRA_FIXED_EXPENSES = INTRA_FIXED_EXPENSES - &
                                           ANNUAL_EL_FIXED_COST(UNIT,2)
                  END SELECT
               ELSE
                  VAR_PLUS_FIXED_COST = ANNUAL_EL_VAR_COST(UNIT,1) + &
                                           ANNUAL_EL_FIXED_COST(UNIT,1)
                  INTRA_COMPANY_SALES_REVENUE(ASSET_CLASS) = &
                           INTRA_COMPANY_SALES_REVENUE(ASSET_CLASS) + &
                                                    VAR_PLUS_FIXED_COST
                  IF(HYDRO_EXPENSE_ASSIGNMENT(UNIT) == 'P') THEN
                  INTRA_PURCHASE_EXPENSES = INTRA_PURCHASE_EXPENSES + &
                                                    VAR_PLUS_FIXED_COST
                  ELSE
                  INTRA_VARIABLE_EXPENSES = INTRA_VARIABLE_EXPENSES + &
                                             ANNUAL_EL_VAR_COST(UNIT,1)
                     INTRA_FIXED_EXPENSES = INTRA_FIXED_EXPENSES + &
                                          ANNUAL_EL_FIXED_COST(UNIT,1)
                  ENDIF
               ENDIF
            ENDIF
!
            ASSET_CLASS = ns_service_decs%ASSET_CLASS_NUM(UNIT)
            ASSET_ALLOCATION_VECTOR = ns_service_decs%ASSET_CLASS_VECTOR(UNIT)
            VOID_LOGICAL=RETURN_ASSET_CLASS_LISTS(ASSET_CLASS, &
                                                ASSET_CLASS_LIST, &
                                            ASSET_ALLOCATION_VECTOR, &
                                                ASSET_ALLOCATION_LIST)

            CLASS_POINTER = 1
            COLLECTION_GROUP = &
                         INDEX('ABNX',(HYDRO_EXPENSE_COLLECTION(UNIT)))
            IF(COLLECTION_GROUP == 0) COLLECTION_GROUP = 3
            IF(HYDRO_EXPENSE_ASSIGNMENT(UNIT) == 'E') THEN
               EXPENSE_GROUP = 1
            ELSEIF(HYDRO_EXPENSE_ASSIGNMENT(UNIT) == 'P') THEN
               EXPENSE_GROUP = 2
            ELSEIF(HYDRO_EXPENSE_ASSIGNMENT(UNIT) == 'R') THEN
               EXPENSE_GROUP = 3
            ELSE
              WRITE(4,*) 'Hydro expense type in asset class is invalid'
               WRITE(4,*) 'Type = ',HYDRO_EXPENSE_ASSIGNMENT(UNIT)
               WRITE(4,*) ' '
            ENDIF
            RESOURCE_OR_SALE = 1
            IF(INDEX('OQA',HYTYPE(UNIT)) /= 0) RESOURCE_OR_SALE = 2
            DO
               ASSET_CLASS = ASSET_CLASS_LIST(CLASS_POINTER)
               CALL CHECK_IF_CLASS_DEFINED(ASSET_CLASS)
               ASSET_CLASS = ASSET_CLASS + 1
               IF(ASSET_CLASS > 0) ASSET_CLASS = &
                                       ASSET_CLASS_POINTER(ASSET_CLASS)
               IF(ASSET_ALLOCATION_LIST(CLASS_POINTER) < 0.) THEN
                  ALLOCATION_VECTOR = &
                              ABS(ASSET_ALLOCATION_LIST(CLASS_POINTER))
                  CALL GET_ASSET_VAR(ALLOCATION_VECTOR, &
                                           DUMMY_TYPE,ALLOCATION_VALUE)
                  ASSET_ALLOCATOR = &
                      ALLOCATION_VALUE(MIN(globecom_year,AVAIL_DATA_YEARS))/100.
               ELSE
                  ASSET_ALLOCATOR = &
                              ASSET_ALLOCATION_LIST(CLASS_POINTER)/100.
               ENDIF
               IF(INDEX('OQA',HYTYPE(UNIT)) /= 0) THEN ! SALES
                  VAR_PLUS_FIXED_COST = ANNUAL_EL_VAR_COST(UNIT,2) + &
                                           ANNUAL_EL_FIXED_COST(UNIT,2)
                  IF(HYDRO_EXPENSE_ASSIGNMENT(UNIT) == 'R') THEN
                  EL_ANN_CLASS_REVENUE(ASSET_CLASS,COLLECTION_GROUP)= &
                 EL_ANN_CLASS_REVENUE(ASSET_CLASS,COLLECTION_GROUP)+ &
                                  ASSET_ALLOCATOR * VAR_PLUS_FIXED_COST
!                     ECITY_TEST = .TRUE. !!! TEMPORARY !!!
                     IF(ECITY_TEST) THEN
                        EL_ANN_CLASS_REVENUE( &
                                       ASSET_CLASS,COLLECTION_GROUP)= &
                           EL_ANN_CLASS_REVENUE( &
                                       ASSET_CLASS,COLLECTION_GROUP)- &
                               ASSET_ALLOCATOR * &
                               MON_MDS_EL_MARKET_REVENUE( &
                                              UNIT,RESOURCE_OR_SALE,0)
                     ENDIF
                  ELSEIF(HYDRO_EXPENSE_ASSIGNMENT(UNIT) == 'P') THEN
               EL_ANN_CLASS_PURCHASES(ASSET_CLASS,COLLECTION_GROUP)= &
                EL_ANN_CLASS_PURCHASES(ASSET_CLASS,COLLECTION_GROUP)- &
                                  ASSET_ALLOCATOR * VAR_PLUS_FIXED_COST
                  ELSE
                EL_ANN_CLASS_VAR_COST(ASSET_CLASS,COLLECTION_GROUP)= &
                EL_ANN_CLASS_VAR_COST(ASSET_CLASS,COLLECTION_GROUP)- &
                           ASSET_ALLOCATOR * ANNUAL_EL_VAR_COST(UNIT,2)
                     EL_ANN_CLASS_FIXED_COST(ASSET_CLASS, &
                                                  COLLECTION_GROUP) = &
                        EL_ANN_CLASS_FIXED_COST(ASSET_CLASS, &
                                                  COLLECTION_GROUP) - &
                            ASSET_ALLOCATOR * &
                                           ANNUAL_EL_FIXED_COST(UNIT,2)
                  ENDIF
                  EL_ANN_CLASS_SELL_CAPACITY( &
                                         ASSET_CLASS,EXPENSE_GROUP) = &
                   EL_ANN_CLASS_CAPACITY(ASSET_CLASS,EXPENSE_GROUP) - &
                            ASSET_ALLOCATOR * &
                              ANNUAL_EL_CAPACITY(UNIT,RESOURCE_OR_SALE)
                EL_ANN_CLASS_SELL_ENERGY(ASSET_CLASS,EXPENSE_GROUP) = &
                      EL_ANN_CLASS_SELL_ENERGY( &
                                         ASSET_CLASS,EXPENSE_GROUP) - &
                              ASSET_ALLOCATOR * &
                                ANNUAL_EL_ENERGY(UNIT,RESOURCE_OR_SALE)
                  EL_ANN_CLASS_SO2(ASSET_CLASS) = &
                                      EL_ANN_CLASS_SO2(ASSET_CLASS) - &
                                         ASSET_ALLOCATOR * &
                                        (ANNUAL_EL_SO2_EMIS(UNIT,1) + &
                                           ANNUAL_EL_SO2_EMIS(UNIT,2))
!
               ELSE ! RESOURCES
                  IF(HYDRO_EXPENSE_ASSIGNMENT(UNIT) == 'P') THEN
                     EL_ANN_CLASS_PURCHASES( &
                                      ASSET_CLASS,COLLECTION_GROUP) = &
                                 EL_ANN_CLASS_PURCHASES( &
                                      ASSET_CLASS,COLLECTION_GROUP) + &
                              ASSET_ALLOCATOR * &
                                      (ANNUAL_EL_VAR_COST(UNIT,1) + &
                                         ANNUAL_EL_FIXED_COST(UNIT,1))
                  ELSE
                     EL_ANN_CLASS_VAR_COST( &
                                      ASSET_CLASS,COLLECTION_GROUP) = &
                                 EL_ANN_CLASS_VAR_COST( &
                                      ASSET_CLASS,COLLECTION_GROUP) + &
                           ASSET_ALLOCATOR * ANNUAL_EL_VAR_COST(UNIT,1)
                     EL_ANN_CLASS_FIXED_COST( &
                                      ASSET_CLASS,COLLECTION_GROUP) = &
                                 EL_ANN_CLASS_FIXED_COST( &
                                        ASSET_CLASS,COLLECTION_GROUP) &
                          + ASSET_ALLOCATOR * &
                                          ANNUAL_EL_FIXED_COST(UNIT,1)
                  ENDIF
                  EL_ANN_CLASS_CAPACITY(ASSET_CLASS,EXPENSE_GROUP) = &
                  EL_ANN_CLASS_CAPACITY(ASSET_CLASS,EXPENSE_GROUP) + &
                            ASSET_ALLOCATOR * &
                             ANNUAL_EL_CAPACITY(UNIT,RESOURCE_OR_SALE)
                  EL_ANN_CLASS_ENERGY(ASSET_CLASS,EXPENSE_GROUP) = &
                    EL_ANN_CLASS_ENERGY(ASSET_CLASS,EXPENSE_GROUP) + &
                              ASSET_ALLOCATOR * &
                               ANNUAL_EL_ENERGY(UNIT,RESOURCE_OR_SALE)
                  EL_ANN_CLASS_SO2(ASSET_CLASS) = &
                                      EL_ANN_CLASS_SO2(ASSET_CLASS) + &
                                         ASSET_ALLOCATOR * &
                                        (ANNUAL_EL_SO2_EMIS(UNIT,1) + &
                                            ANNUAL_EL_SO2_EMIS(UNIT,2))
! MODIFIED FOR CF. 9/9/99. GAT.
                  IF(HYTYPE(UNIT) == 'B') THEN
                     EL_ANN_CLASS_MARKET_REVENUES(ASSET_CLASS) = &
                          EL_ANN_CLASS_MARKET_REVENUES(ASSET_CLASS) + &
                               ASSET_ALLOCATOR * &
                     MON_MDS_EL_MARKET_REVENUE(UNIT,RESOURCE_OR_SALE,0)
                  ELSEIF(HYTYPE(UNIT) == 'P') THEN
                     EL_ANN_CLASS_MARKET_REVENUES(ASSET_CLASS) = &
                          EL_ANN_CLASS_MARKET_REVENUES(ASSET_CLASS) + &
                               ASSET_ALLOCATOR * &
                     MON_MDS_EL_MARKET_REVENUE(UNIT,RESOURCE_OR_SALE,0)
                  ELSEIF(HYTYPE(UNIT) == 'S') THEN
                     EL_ANN_CLASS_MARKET_REVENUES(ASSET_CLASS) = &
                         EL_ANN_CLASS_MARKET_REVENUES(ASSET_CLASS) + &
                               ASSET_ALLOCATOR * &
                               MON_MDS_EL_MARKET_REVENUE(UNIT,1,0)
                     EL_ANN_CLASS_MARKET_PURCHASE(ASSET_CLASS) = &
                          EL_ANN_CLASS_MARKET_PURCHASE(ASSET_CLASS) + &
                               ASSET_ALLOCATOR * &
                               MON_MDS_EL_MARKET_REVENUE(UNIT,2,0)
                  ENDIF

               ENDIF ! SALE OR RESOURCE
               CLASS_POINTER = CLASS_POINTER + 1
               IF(CLASS_POINTER > AVAIL_DATA_YEARS) EXIT
               IF(ASSET_CLASS_LIST(CLASS_POINTER) == 0. .OR. &
                          ASSET_CLASS_LIST(CLASS_POINTER) == -99.)EXIT
            ENDDO ! ASSET CLASSES
         ENDDO ! HYDRO RESOURCES
         IF(ALLOCATED(ANNUAL_EL_ENERGY)) &
               DEALLOCATE(ANNUAL_EL_ENERGY,ANNUAL_EL_CAPACITY, &
                    ANNUAL_EL_VAR_COST,ANNUAL_EL_FIXED_COST, &
                    ANNUAL_EL_SO2_EMIS,MON_MDS_EL_MARKET_REVENUE)
         IF(ALLOCATED(MON_MDS_EL_ENERGY)) &
               DEALLOCATE( MON_MDS_EL_CAPACITY,MON_MDS_EL_ENERGY, &
                           MON_MDS_EL_VAR_COST,MON_MDS_EL_FIXED_COST, &
                           MON_MDS_EL_SO2_EMIS)
!  SUM TO COMPANY TOTAL
         DO I = 1, MAX_INTRA_CLASS
            INTRA_COMPANY_PURCHASE_EXPENSE(0) = &
                                  INTRA_COMPANY_PURCHASE_EXPENSE(0) + &
                                     INTRA_COMPANY_PURCHASE_EXPENSE(I)
            INTRA_COMPANY_SALES_REVENUE(0) = &
                                     INTRA_COMPANY_SALES_REVENUE(0) + &
                                        INTRA_COMPANY_SALES_REVENUE(I)
         ENDDO
         DO I = 1, NUM_OF_HYDRO_CLASSES
            EL_ANN_CLASS_SO2(0) = EL_ANN_CLASS_SO2(0) + &
                                                    EL_ANN_CLASS_SO2(I)
            DO J = 1, 4
            EL_ANN_CLASS_VAR_COST(I,J) =  EL_ANN_CLASS_VAR_COST(I,J)/ &
                                                               1000000.
            EL_ANN_CLASS_FIXED_COST(I,J)=EL_ANN_CLASS_FIXED_COST(I,J) &
                                                              /1000000.
            EL_ANN_CLASS_REVENUE(I,J) =   EL_ANN_CLASS_REVENUE(I,J)/ &
                                                               1000000.
            EL_ANN_CLASS_CAPACITY(I,J) =  EL_ANN_CLASS_CAPACITY(I,J)/ &
                                                               1000000.
               EL_ANN_CLASS_ENERGY(I,J) = EL_ANN_CLASS_ENERGY(I,J)/ &
                                                               1000000.
               EL_ANN_CLASS_SELL_CAPACITY(I,J) = &
                               EL_ANN_CLASS_SELL_CAPACITY(I,J)/1000000.
               EL_ANN_CLASS_SELL_ENERGY(I,J) = &
                                 EL_ANN_CLASS_SELL_ENERGY(I,J)/1000000.
             EL_ANN_CLASS_PURCHASES(I,J)=EL_ANN_CLASS_PURCHASES(I,J)/ &
                                                               1000000.
            EL_ANN_CLASS_VAR_COST(0,J) =  EL_ANN_CLASS_VAR_COST(0,J)+ &
                                             EL_ANN_CLASS_VAR_COST(I,J)
               EL_ANN_CLASS_FIXED_COST(0,J) = &
                                       EL_ANN_CLASS_FIXED_COST(0,J) + &
                                          EL_ANN_CLASS_FIXED_COST(I,J)
              EL_ANN_CLASS_REVENUE(0,J) = EL_ANN_CLASS_REVENUE(0,J) + &
                                           EL_ANN_CLASS_REVENUE(I,J)
            EL_ANN_CLASS_CAPACITY(0,J) = EL_ANN_CLASS_CAPACITY(0,J) + &
                                            EL_ANN_CLASS_CAPACITY(I,J)
               EL_ANN_CLASS_ENERGY(0,J) = EL_ANN_CLASS_ENERGY(0,J) + &
                                               EL_ANN_CLASS_ENERGY(I,J)
               EL_ANN_CLASS_SELL_CAPACITY(0,J) = &
                                    EL_ANN_CLASS_SELL_CAPACITY(0,J) + &
                                       EL_ANN_CLASS_SELL_CAPACITY(I,J)
               EL_ANN_CLASS_SELL_ENERGY(0,J) = &
                                      EL_ANN_CLASS_SELL_ENERGY(0,J) + &
                                         EL_ANN_CLASS_SELL_ENERGY(I,J)
            EL_ANN_CLASS_PURCHASES(0,J)=EL_ANN_CLASS_PURCHASES(0,J) + &
                                           EL_ANN_CLASS_PURCHASES(I,J)
            ENDDO
         ENDDO
      RETURN ! CALC_EL_ANN_ASSET_CLASS

      ENTRY ENRG_LIMITED_INFO(R_CLASS,R_CLASS_EXISTS_local, &
                              R_PURCHASE_POWER_EXPENSE, &
                              R_VARIABLE_EXPENSE, &
                              R_FIXED_EXPENSE, &
                              R_EXPENSE_COLLECTED_ADJ_CLAUSE, &
                              R_EXPENSE_COLLECTED_BASE_RATES, &
                              R_NOT_COLLECTED_IN_RATES, &
                              R_BTL_SALES_REVENUE, &
                              R_TOTAL_SALES_REVENUE, &
                              R_SALES_REVENUE_NOT_IN_RATES, &
                              R_BTL_EXPENSES, &
                              R_EL_MARKET_REVENUES, &
                              R_EL_MARKET_PURCHASES)

         R_EL_MARKET_REVENUES = 0.
         R_EL_MARKET_PURCHASES = 0.
         R_CLASS_EXISTS_local = .FALSE.
         IF(R_CLASS <= MAX_HYDRO_CLASS_NUM) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
               R_CLASS_EXISTS_local = .TRUE.
               R_PURCHASE_POWER_EXPENSE = R_PURCHASE_POWER_EXPENSE + &
                            EL_ANN_CLASS_PURCHASES(ASSET_CLASS,1) + &
                            EL_ANN_CLASS_PURCHASES(ASSET_CLASS,2) + &
                            EL_ANN_CLASS_PURCHASES(ASSET_CLASS,3) + &
                         INTRA_COMPANY_PURCHASE_EXPENSE(ASSET_CLASS)/ &
                                                               1000000.
               R_VARIABLE_EXPENSE = R_VARIABLE_EXPENSE + &
                               EL_ANN_CLASS_VAR_COST(ASSET_CLASS,1) + &
                               EL_ANN_CLASS_VAR_COST(ASSET_CLASS,2) + &
                                  EL_ANN_CLASS_VAR_COST(ASSET_CLASS,3)
               R_FIXED_EXPENSE = R_FIXED_EXPENSE + &
                             EL_ANN_CLASS_FIXED_COST(ASSET_CLASS,1) + &
                             EL_ANN_CLASS_FIXED_COST(ASSET_CLASS,2) + &
                                EL_ANN_CLASS_FIXED_COST(ASSET_CLASS,3)
               R_EXPENSE_COLLECTED_ADJ_CLAUSE = &
                                   R_EXPENSE_COLLECTED_ADJ_CLAUSE + &
                              EL_ANN_CLASS_PURCHASES(ASSET_CLASS,1) + &
                              EL_ANN_CLASS_VAR_COST(ASSET_CLASS,1) + &
                              EL_ANN_CLASS_FIXED_COST(ASSET_CLASS,1)- &
                                   EL_ANN_CLASS_REVENUE(ASSET_CLASS,1)
               R_EXPENSE_COLLECTED_BASE_RATES = &
                                 R_EXPENSE_COLLECTED_BASE_RATES + &
                              EL_ANN_CLASS_PURCHASES(ASSET_CLASS,2) + &
                               EL_ANN_CLASS_VAR_COST(ASSET_CLASS,2) + &
                                 EL_ANN_CLASS_FIXED_COST(ASSET_CLASS,2)
               R_NOT_COLLECTED_IN_RATES = R_NOT_COLLECTED_IN_RATES + &
                              EL_ANN_CLASS_PURCHASES(ASSET_CLASS,3) + &
                               EL_ANN_CLASS_VAR_COST(ASSET_CLASS,3) + &
                                 EL_ANN_CLASS_FIXED_COST(ASSET_CLASS,3)
               R_TOTAL_SALES_REVENUE = R_TOTAL_SALES_REVENUE + &
                               EL_ANN_CLASS_REVENUE(ASSET_CLASS,1) + &
                               EL_ANN_CLASS_REVENUE(ASSET_CLASS,2) + &
                               EL_ANN_CLASS_REVENUE(ASSET_CLASS,3) + &
                            INTRA_COMPANY_SALES_REVENUE(ASSET_CLASS)/ &
                                                               1000000.
               R_SALES_REVENUE_NOT_IN_RATES = &
                                     R_SALES_REVENUE_NOT_IN_RATES + &
                                    EL_ANN_CLASS_REVENUE(ASSET_CLASS,3)
               R_BTL_SALES_REVENUE = R_BTL_SALES_REVENUE + &
                                    EL_ANN_CLASS_REVENUE(ASSET_CLASS,4)
               R_BTL_EXPENSES = R_BTL_EXPENSES + &
                              EL_ANN_CLASS_PURCHASES(ASSET_CLASS,4) + &
                               EL_ANN_CLASS_VAR_COST(ASSET_CLASS,4) + &
                                 EL_ANN_CLASS_FIXED_COST(ASSET_CLASS,4)
               R_EL_MARKET_REVENUES = &
                   EL_ANN_CLASS_MARKET_REVENUES(ASSET_CLASS)/1000000. &
                      + EL_MON_MDS_MARKET_REVENUES(ASSET_CLASS,0)
               R_EL_MARKET_PURCHASES = &
                     EL_ANN_CLASS_MARKET_PURCHASE(ASSET_CLASS)/1000000.
            ENDIF
         ENDIF
      RETURN


      

      ENTRY SET_UP_EL_CLASS_ARRAYS

         NUM_OF_HYDRO_CLASSES = RETURN_NUM_OF_HYDRO_CLASSES( &
                                                  MAX_HYDRO_CLASS_NUM)
         IF(MAX_HYDRO_CLASS_NUM > 0) THEN
            IF(ALLOCATED(ASSET_CLASS_POINTER)) &
                                       DEALLOCATE(ASSET_CLASS_POINTER)
            ALLOCATE(ASSET_CLASS_POINTER(MAX_HYDRO_CLASS_NUM))
            MAX_HYDRO_CLASS_NUM = &
                        RETURN_HYDRO_CLASS_POINTER(ASSET_CLASS_POINTER)
         ENDIF
! ANNUAL
         IF(ALLOCATED(EL_ANN_CLASS_VAR_COST)) &
                           DEALLOCATE(EL_ANN_CLASS_VAR_COST, &
                                      EL_ANN_CLASS_FIXED_COST, &
                                      EL_ANN_CLASS_REVENUE, &
                                      EL_ANN_CLASS_CAPACITY, &
                                      EL_ANN_CLASS_SELL_CAPACITY, &
                                      EL_ANN_CLASS_PURCHASES, &
                                      EL_ANN_CLASS_ENERGY, &
                                      EL_ANN_CLASS_SELL_ENERGY, &
                                      EL_ANN_CLASS_SO2, &
                                      EL_ANN_CLASS_MARKET_REVENUES, &
                                      EL_ANN_CLASS_MARKET_PURCHASE, &
                                      INTRA_COMPANY_SALES_REVENUE, &
                                      INTRA_COMPANY_PURCHASE_EXPENSE)
         ALLOCATE(EL_ANN_CLASS_VAR_COST(0:NUM_OF_HYDRO_CLASSES,4))
         ALLOCATE(EL_ANN_CLASS_FIXED_COST(0:NUM_OF_HYDRO_CLASSES,4))
         ALLOCATE(EL_ANN_CLASS_REVENUE(0:NUM_OF_HYDRO_CLASSES,4))
         ALLOCATE(EL_ANN_CLASS_CAPACITY(0:NUM_OF_HYDRO_CLASSES,4))
         ALLOCATE(EL_ANN_CLASS_ENERGY(0:NUM_OF_HYDRO_CLASSES,4))
         ALLOCATE(EL_ANN_CLASS_SELL_CAPACITY(0:NUM_OF_HYDRO_CLASSES,4))
         ALLOCATE(EL_ANN_CLASS_SELL_ENERGY(0:NUM_OF_HYDRO_CLASSES,4))
         ALLOCATE(EL_ANN_CLASS_PURCHASES(0:NUM_OF_HYDRO_CLASSES,4))
         ALLOCATE(EL_ANN_CLASS_SO2(0:NUM_OF_HYDRO_CLASSES))
         ALLOCATE(INTRA_COMPANY_SALES_REVENUE(0:1024))
         ALLOCATE(INTRA_COMPANY_PURCHASE_EXPENSE(0:1024))
         ALLOCATE(EL_ANN_CLASS_MARKET_REVENUES(0:NUM_OF_HYDRO_CLASSES))
         ALLOCATE(EL_ANN_CLASS_MARKET_PURCHASE(0:NUM_OF_HYDRO_CLASSES))
! MONTHLY
         IF(ALLOCATED(EL_MON_MDS_VAR_COST)) &
                           DEALLOCATE(EL_MON_MDS_VAR_COST, &
                                      EL_MON_MDS_FIXED_COST, &
                                      EL_MON_MDS_REVENUE, &
                                      EL_MON_MDS_CAPACITY, &
                                      EL_MON_MDS_SELL_CAPACITY, &
                                      EL_MON_MDS_PURCHASES, &
                                      EL_MON_MDS_ENERGY, &
                                      EL_MON_MDS_SELL_ENERGY, &
                                      EL_MON_MDS_SO2, &
                                      EL_MON_MDS_MARKET_REVENUES, &
                                      EL_MON_MDS_MARKET_PURCHASE, &
                                      MON_MDS_INCO_SALES_REVENUE, &
                                      MON_MDS_INCO_PURCHASE_EXPENSE, &
                                      MON_MDS_IN_SALES_REVENUE, &
                                      MON_MDS_IN_PURCHASE_EXPENSES, &
                                      MON_MDS_IN_VARIABLE_EXPENSES, &
                                      MON_MDS_IN_FIXED_EXPENSES)
         ALLOCATE(EL_MON_MDS_VAR_COST(0:NUM_OF_HYDRO_CLASSES,4,0:12))
         call allocate_rps_hydro_db()
         ALLOCATE(EL_MON_MDS_FIXED_COST(0:NUM_OF_HYDRO_CLASSES,4,0:12))
         ALLOCATE(EL_MON_MDS_REVENUE(0:NUM_OF_HYDRO_CLASSES,4,0:12))
         ALLOCATE(EL_MON_MDS_CAPACITY(0:NUM_OF_HYDRO_CLASSES,4,0:12))
         ALLOCATE(EL_MON_MDS_ENERGY(0:NUM_OF_HYDRO_CLASSES,4,0:12))
         ALLOCATE(EL_MON_MDS_SELL_CAPACITY( &
                                        0:NUM_OF_HYDRO_CLASSES,4,0:12))
        ALLOCATE(EL_MON_MDS_SELL_ENERGY(0:NUM_OF_HYDRO_CLASSES,4,0:12))
         ALLOCATE(EL_MON_MDS_PURCHASES(0:NUM_OF_HYDRO_CLASSES,4,0:12))
         ALLOCATE(EL_MON_MDS_SO2(0:NUM_OF_HYDRO_CLASSES,0:12))
         ALLOCATE(MON_MDS_INCO_SALES_REVENUE(0:1024,0:12))
         ALLOCATE(MON_MDS_INCO_PURCHASE_EXPENSE(0:1024,0:12))
         ALLOCATE(MON_MDS_IN_SALES_REVENUE(0:12))
         ALLOCATE(MON_MDS_IN_PURCHASE_EXPENSES(0:12))
         ALLOCATE(MON_MDS_IN_VARIABLE_EXPENSES(0:12))
         ALLOCATE(MON_MDS_IN_FIXED_EXPENSES(0:12))
         ALLOCATE(EL_MON_MDS_MARKET_REVENUES( &
                                          0:NUM_OF_HYDRO_CLASSES,0:12))
         ALLOCATE(EL_MON_MDS_MARKET_PURCHASE( &
                                          0:NUM_OF_HYDRO_CLASSES,0:12))
!
         IF(ALLOCATED(ASSET_CLASS_LIST)) &
                     DEALLOCATE(ASSET_CLASS_LIST,ASSET_ALLOCATION_LIST)
         ALLOCATE(ASSET_CLASS_LIST(AVAIL_DATA_YEARS))
         ALLOCATE(ASSET_ALLOCATION_LIST(AVAIL_DATA_YEARS))
      RETURN

      ENTRY ZERO_EL_CLASS_ARRAYS

         EL_MON_MDS_VAR_COST = 0.
         RPS_HYDRO_DB = 0.
         EL_MON_MDS_FIXED_COST = 0.
         EL_MON_MDS_REVENUE = 0.
         EL_MON_MDS_CAPACITY = 0.
         EL_MON_MDS_ENERGY = 0.
         EL_MON_MDS_SELL_CAPACITY = 0.
         EL_MON_MDS_SELL_ENERGY = 0.
         EL_MON_MDS_PURCHASES = 0.
         EL_MON_MDS_SO2 = 0.
         EL_MON_MDS_MARKET_REVENUES = 0.
         EL_MON_MDS_MARKET_PURCHASE = 0.
         MON_MDS_INCO_PURCHASE_EXPENSE = 0.
         MON_MDS_INCO_SALES_REVENUE = 0.
         MON_MDS_IN_SALES_REVENUE = 0.
         MON_MDS_IN_PURCHASE_EXPENSES = 0.
         MON_MDS_IN_VARIABLE_EXPENSES = 0.
         MON_MDS_IN_FIXED_EXPENSES = 0.
      RETURN

      ENTRY CALC_EL_MON_MDS_ASSET_CLASS(R_ISEAS)

         CURRENT_YEAR = globecom_year+get_BASE_YEAR()
         CURRENT_YEAR_COMPARISON = (CURRENT_YEAR-1900)*100

         MAX_INTRA_CLASS = 0
         IF(HYDRO_UNITS <= 0 .OR. &
                             .NOT. ALLOCATED(MON_MDS_EL_ENERGY)) RETURN

         DO UNIT = 1, HYDRO_UNITS 
            IF(ON_LINE(UNIT) - CURRENT_YEAR_COMPARISON > 12  .OR. &
                   CURRENT_YEAR_COMPARISON - OFF_LINE(UNIT) > 12) CYCLE
            IF(ns_service_decs%INTRA_COMPANY_CLASS_ID(UNIT) > 0) THEN
               ASSET_CLASS = ns_service_decs%INTRA_COMPANY_CLASS_ID(UNIT)
               CALL CHECK_IF_CLASS_DEFINED(ASSET_CLASS)
               ASSET_CLASS = ASSET_CLASS + 1
               MAX_INTRA_CLASS = MAX(MAX_INTRA_CLASS,ASSET_CLASS)
               
               
               IF(INDEX('OQA',HYTYPE(UNIT)) /= 0) THEN
                  VAR_PLUS_FIXED_COST = &
                                MON_MDS_EL_VAR_COST(UNIT,2,R_ISEAS) + &
                                  MON_MDS_EL_FIXED_COST(UNIT,2,R_ISEAS)
                 MON_MDS_INCO_PURCHASE_EXPENSE(ASSET_CLASS,R_ISEAS) = &
                 MON_MDS_INCO_PURCHASE_EXPENSE(ASSET_CLASS,R_ISEAS) + &
                                                    VAR_PLUS_FIXED_COST
                  SELECT CASE (HYDRO_EXPENSE_ASSIGNMENT(UNIT))
                  CASE('R')  ! SECONDARY SALES
                     MON_MDS_IN_SALES_REVENUE(R_ISEAS) = &
                              MON_MDS_IN_SALES_REVENUE(R_ISEAS) + &
                                                    VAR_PLUS_FIXED_COST
                  CASE('P')  ! PURCHASE REVENUES
                     MON_MDS_IN_PURCHASE_EXPENSES(R_ISEAS) = &
                           MON_MDS_IN_PURCHASE_EXPENSES(R_ISEAS) - &
                                                    VAR_PLUS_FIXED_COST
                  CASE DEFAULT
                     MON_MDS_IN_VARIABLE_EXPENSES(R_ISEAS) = &
                              MON_MDS_IN_VARIABLE_EXPENSES(R_ISEAS) - &
                                    MON_MDS_EL_VAR_COST(UNIT,2,R_ISEAS)
                     MON_MDS_IN_FIXED_EXPENSES(R_ISEAS) = &
                              MON_MDS_IN_FIXED_EXPENSES(R_ISEAS) - &
                                  MON_MDS_EL_FIXED_COST(UNIT,2,R_ISEAS)
                  END SELECT
               ELSE
                  VAR_PLUS_FIXED_COST = &
                              MON_MDS_EL_VAR_COST(UNIT,1,R_ISEAS) + &
                                  MON_MDS_EL_FIXED_COST(UNIT,1,R_ISEAS)
                  MON_MDS_INCO_SALES_REVENUE(ASSET_CLASS,R_ISEAS) = &
                    MON_MDS_INCO_SALES_REVENUE(ASSET_CLASS,R_ISEAS) + &
                                                    VAR_PLUS_FIXED_COST
                  IF(HYDRO_EXPENSE_ASSIGNMENT(UNIT) == 'P') THEN
                  INTRA_PURCHASE_EXPENSES = INTRA_PURCHASE_EXPENSES + &
                                                    VAR_PLUS_FIXED_COST
                  ELSE
                  INTRA_VARIABLE_EXPENSES = INTRA_VARIABLE_EXPENSES + &
                                    MON_MDS_EL_VAR_COST(UNIT,1,R_ISEAS)
                     INTRA_FIXED_EXPENSES = INTRA_FIXED_EXPENSES + &
                                 MON_MDS_EL_FIXED_COST(UNIT,1,R_ISEAS)
                  ENDIF
               ENDIF
            ENDIF
!
            ASSET_CLASS = ns_service_decs%ASSET_CLASS_NUM(UNIT)
            ASSET_ALLOCATION_VECTOR = &
                ns_service_decs%ASSET_CLASS_VECTOR(UNIT)
            IF(ASSET_CLASS < 0) THEN
               CALL GET_ASSET_VAR(ABS(ASSET_CLASS), &
                                       DUMMY_TYPE,ASSET_CLASS_LIST)
               CALL GET_ASSET_VAR(ABS(ASSET_ALLOCATION_VECTOR), &
                                     DUMMY_TYPE,ASSET_ALLOCATION_LIST)
            ELSE
               ASSET_CLASS_LIST(1) = ASSET_CLASS
               ASSET_CLASS_LIST(2) = 0.
               ASSET_ALLOCATION_LIST(1) = 100.
               ASSET_ALLOCATION_LIST(2) = 0.
            ENDIF
            CLASS_POINTER = 1

            C_G = INDEX('ABNX',(HYDRO_EXPENSE_COLLECTION(UNIT)))
            IF(C_G == 0) C_G = 3
            IF(HYDRO_EXPENSE_ASSIGNMENT(UNIT) == 'E') THEN
               E_G = 1
            ELSEIF(HYDRO_EXPENSE_ASSIGNMENT(UNIT) == 'P') THEN
               E_G = 2
            ELSEIF(HYDRO_EXPENSE_ASSIGNMENT(UNIT) == 'R') THEN
               E_G = 3
            ELSE
              WRITE(4,*) 'Hydro expense type in asset class is invalid'
               WRITE(4,*) 'Type = ',HYDRO_EXPENSE_ASSIGNMENT(UNIT)
               WRITE(4,*) ' '
            ENDIF
            RESOURCE_OR_SALE = 1
            IF(INDEX('OQA',HYTYPE(UNIT)) /= 0) RESOURCE_OR_SALE = 2
            DO
               ASSET_CLASS = ASSET_CLASS_LIST(CLASS_POINTER)
               CALL CHECK_IF_CLASS_DEFINED(ASSET_CLASS)
               ASSET_CLASS = ASSET_CLASS + 1
               IF(ASSET_CLASS > 0) ASSET_CLASS = &
                                       ASSET_CLASS_POINTER(ASSET_CLASS)
               ASSET_ALLOCATOR = &
                              ASSET_ALLOCATION_LIST(CLASS_POINTER)/100.
               IF(INDEX('OQA',HYTYPE(UNIT)) /= 0) THEN ! SALES
                  VAR_PLUS_FIXED_COST = &
                           MON_MDS_EL_VAR_COST(UNIT,2,R_ISEAS) + &
                                  MON_MDS_EL_FIXED_COST(UNIT,2,R_ISEAS)
                  IF(HYDRO_EXPENSE_ASSIGNMENT(UNIT) == 'R') THEN
                     EL_MON_MDS_REVENUE(ASSET_CLASS,C_G,R_ISEAS)= &
                     EL_MON_MDS_REVENUE(ASSET_CLASS,C_G,R_ISEAS)+ &
                                  ASSET_ALLOCATOR * VAR_PLUS_FIXED_COST
                  ELSEIF(HYDRO_EXPENSE_ASSIGNMENT(UNIT) == 'P') THEN
                   EL_MON_MDS_PURCHASES(ASSET_CLASS,C_G,R_ISEAS)= &
                   EL_MON_MDS_PURCHASES(ASSET_CLASS,C_G,R_ISEAS)- &
                                  ASSET_ALLOCATOR * VAR_PLUS_FIXED_COST
                  ELSE
                    EL_MON_MDS_VAR_COST(ASSET_CLASS,C_G,R_ISEAS)= &
                    EL_MON_MDS_VAR_COST(ASSET_CLASS,C_G,R_ISEAS)- &
                            ASSET_ALLOCATOR * &
                                    MON_MDS_EL_VAR_COST(UNIT,2,R_ISEAS)
                     EL_MON_MDS_FIXED_COST(ASSET_CLASS, &
                                                   C_G,R_ISEAS) = &
                        EL_MON_MDS_FIXED_COST(ASSET_CLASS, &
                                                   C_G,R_ISEAS) - &
                            ASSET_ALLOCATOR * &
                                  MON_MDS_EL_FIXED_COST(UNIT,2,R_ISEAS)
                  ENDIF
                  EL_MON_MDS_SELL_CAPACITY( &
                                           ASSET_CLASS,E_G,R_ISEAS) = &
                      EL_MON_MDS_CAPACITY(ASSET_CLASS,E_G,R_ISEAS) - &
                      ASSET_ALLOCATOR * &
                     MON_MDS_EL_CAPACITY(UNIT,RESOURCE_OR_SALE,R_ISEAS)
                  EL_MON_MDS_SELL_ENERGY(ASSET_CLASS,E_G,R_ISEAS) = &
                      EL_MON_MDS_SELL_ENERGY( &
                                           ASSET_CLASS,E_G,R_ISEAS) - &
                         ASSET_ALLOCATOR * &
                       MON_MDS_EL_ENERGY(UNIT,RESOURCE_OR_SALE,R_ISEAS)
                  EL_MON_MDS_SO2(ASSET_CLASS,R_ISEAS) = &
                                EL_MON_MDS_SO2(ASSET_CLASS,R_ISEAS) - &
                                  ASSET_ALLOCATOR * &
                               (MON_MDS_EL_SO2_EMIS(UNIT,1,R_ISEAS) + &
                                   MON_MDS_EL_SO2_EMIS(UNIT,2,R_ISEAS))
!
               ELSE ! RESOURCES
                  IF(HYDRO_EXPENSE_ASSIGNMENT(UNIT) == 'P') THEN
                     EL_MON_MDS_PURCHASES( &
                                         ASSET_CLASS,C_G,R_ISEAS) = &
                                 EL_MON_MDS_PURCHASES( &
                                         ASSET_CLASS,C_G,R_ISEAS) + &
                              ASSET_ALLOCATOR * &
                               (MON_MDS_EL_VAR_COST(UNIT,1,R_ISEAS) + &
                                 MON_MDS_EL_FIXED_COST(UNIT,1,R_ISEAS))
                  ELSE
                     EL_MON_MDS_VAR_COST( &
                                         ASSET_CLASS,C_G,R_ISEAS) = &
                       EL_MON_MDS_VAR_COST(ASSET_CLASS,C_G,R_ISEAS) + &
                                 ASSET_ALLOCATOR * &
                                 MON_MDS_EL_VAR_COST(UNIT,1,R_ISEAS)
                     EL_MON_MDS_FIXED_COST(ASSET_CLASS,C_G,R_ISEAS) = &
                     EL_MON_MDS_FIXED_COST(ASSET_CLASS,C_G,R_ISEAS) + &
                           ASSET_ALLOCATOR * &
                                  MON_MDS_EL_FIXED_COST(UNIT,1,R_ISEAS)
                  ENDIF
                  EL_MON_MDS_CAPACITY(ASSET_CLASS,E_G,R_ISEAS) = &
                      EL_MON_MDS_CAPACITY(ASSET_CLASS,E_G,R_ISEAS) + &
                      ASSET_ALLOCATOR * &
                    MON_MDS_EL_CAPACITY(UNIT,RESOURCE_OR_SALE,R_ISEAS)
                  EL_MON_MDS_ENERGY(ASSET_CLASS,E_G,R_ISEAS) = &
                      EL_MON_MDS_ENERGY(ASSET_CLASS,E_G,R_ISEAS) + &
                      ASSET_ALLOCATOR * &
                      MON_MDS_EL_ENERGY(UNIT,RESOURCE_OR_SALE,R_ISEAS)
                  EL_MON_MDS_SO2(ASSET_CLASS,R_ISEAS) = &
                      EL_MON_MDS_SO2(ASSET_CLASS,R_ISEAS) + &
                      ASSET_ALLOCATOR * &
                      (MON_MDS_EL_SO2_EMIS(UNIT,1,R_ISEAS) + &
                                   MON_MDS_EL_SO2_EMIS(UNIT,2,R_ISEAS))

                  IF(HYTYPE(UNIT) == 'B') THEN
                   EL_MON_MDS_MARKET_REVENUES(ASSET_CLASS,R_ISEAS) = &
                         EL_MON_MDS_MARKET_REVENUES( &
                                               ASSET_CLASS,R_ISEAS) + &
                               ASSET_ALLOCATOR * &
                       MON_MDS_EL_MARKET_REVENUE( &
                                         UNIT,RESOURCE_OR_SALE,R_ISEAS)
                  ELSEIF(HYTYPE(UNIT) == 'P') THEN
                    EL_MON_MDS_MARKET_REVENUES(ASSET_CLASS,R_ISEAS) = &
                         EL_MON_MDS_MARKET_REVENUES( &
                                               ASSET_CLASS,R_ISEAS) + &
                               ASSET_ALLOCATOR * &
                       MON_MDS_EL_MARKET_REVENUE( &
                                         UNIT,RESOURCE_OR_SALE,R_ISEAS)
                  ELSEIF(HYTYPE(UNIT) == 'S') THEN
                   EL_MON_MDS_MARKET_REVENUES(ASSET_CLASS,R_ISEAS) = &
                         EL_MON_MDS_MARKET_REVENUES( &
                                               ASSET_CLASS,R_ISEAS) + &
                               ASSET_ALLOCATOR * &
                             MON_MDS_EL_MARKET_REVENUE(UNIT,1,R_ISEAS)
                   EL_MON_MDS_MARKET_PURCHASE(ASSET_CLASS,R_ISEAS) = &
                         EL_MON_MDS_MARKET_PURCHASE( &
                                              ASSET_CLASS,R_ISEAS) + &
                               ASSET_ALLOCATOR * &
                              MON_MDS_EL_MARKET_REVENUE(UNIT,2,R_ISEAS)
                  ENDIF
               ENDIF
               CLASS_POINTER = CLASS_POINTER + 1
               IF(CLASS_POINTER > AVAIL_DATA_YEARS) EXIT
               IF(ASSET_CLASS_LIST(CLASS_POINTER) == 0. .OR. &
                           ASSET_CLASS_LIST(CLASS_POINTER) == -99.)EXIT
            ENDDO ! ASSET CLASSES
         ENDDO ! HYDRO RESOURCES
!  SUM TO COMPANY TOTAL
         DO I = 1, MAX_INTRA_CLASS
            MON_MDS_INCO_PURCHASE_EXPENSE(0,R_ISEAS) = &
                           MON_MDS_INCO_PURCHASE_EXPENSE(0,R_ISEAS) + &
                              MON_MDS_INCO_PURCHASE_EXPENSE(I,R_ISEAS)
            MON_MDS_INCO_SALES_REVENUE(0,R_ISEAS) = &
                              MON_MDS_INCO_SALES_REVENUE(0,R_ISEAS) + &
                              MON_MDS_INCO_SALES_REVENUE(I,R_ISEAS)
         ENDDO
         DO I = 1, NUM_OF_HYDRO_CLASSES
            EL_MON_MDS_SO2(0,R_ISEAS) = EL_MON_MDS_SO2(0,R_ISEAS) + &
                                              EL_MON_MDS_SO2(I,R_ISEAS)
            EL_MON_MDS_MARKET_REVENUES(I,R_ISEAS) = &
                         EL_MON_MDS_MARKET_REVENUES(I,R_ISEAS)/1000000.
            EL_MON_MDS_MARKET_REVENUES(0,R_ISEAS) = &
                                EL_MON_MDS_MARKET_REVENUES(0,R_ISEAS) &
                                + EL_MON_MDS_MARKET_REVENUES(I,R_ISEAS)
            EL_MON_MDS_MARKET_REVENUES(I,0) = &
                                 EL_MON_MDS_MARKET_REVENUES(I,0) &
                                + EL_MON_MDS_MARKET_REVENUES(I,R_ISEAS)
            EL_MON_MDS_MARKET_REVENUES(0,0) = &
                                 EL_MON_MDS_MARKET_REVENUES(0,0) &
                                + EL_MON_MDS_MARKET_REVENUES(I,R_ISEAS)
            DO J = 1, 4
               EL_MON_MDS_VAR_COST(I,J,R_ISEAS) = &
                              EL_MON_MDS_VAR_COST(I,J,R_ISEAS)/1000000.
               EL_MON_MDS_FIXED_COST(I,J,R_ISEAS) = &
                            EL_MON_MDS_FIXED_COST(I,J,R_ISEAS)/1000000.
               EL_MON_MDS_REVENUE(I,J,R_ISEAS) = &
                               EL_MON_MDS_REVENUE(I,J,R_ISEAS)/1000000.
               EL_MON_MDS_CAPACITY(I,J,R_ISEAS) = &
                                 EL_MON_MDS_CAPACITY(I,J,R_ISEAS)/ &
                                                               1000000.
               EL_MON_MDS_ENERGY(I,J,R_ISEAS) = &
                                      EL_MON_MDS_ENERGY(I,J,R_ISEAS)/ &
                                                               1000000.
               EL_MON_MDS_SELL_CAPACITY(I,J,R_ISEAS) = &
                               EL_MON_MDS_SELL_CAPACITY(I,J,R_ISEAS)/ &
                                                               1000000.
               EL_MON_MDS_SELL_ENERGY(I,J,R_ISEAS) = &
                                EL_MON_MDS_SELL_ENERGY(I,J,R_ISEAS)/ &
                                                               1000000.
               EL_MON_MDS_PURCHASES(I,J,R_ISEAS) = &
                                   EL_MON_MDS_PURCHASES(I,J,R_ISEAS)/ &
                                                               1000000.
!  TOTAL ALL CLASSES
               EL_MON_MDS_VAR_COST(0,J,R_ISEAS) = &
                                    EL_MON_MDS_VAR_COST(0,J,R_ISEAS)+ &
                                      EL_MON_MDS_VAR_COST(I,J,R_ISEAS)
               EL_MON_MDS_FIXED_COST(0,J,R_ISEAS) = &
                                EL_MON_MDS_FIXED_COST(0,J,R_ISEAS) + &
                                    EL_MON_MDS_FIXED_COST(I,J,R_ISEAS)
               EL_MON_MDS_REVENUE(0,J,R_ISEAS) = &
                                   EL_MON_MDS_REVENUE(0,J,R_ISEAS) + &
                                       EL_MON_MDS_REVENUE(I,J,R_ISEAS)
               EL_MON_MDS_CAPACITY(0,J,R_ISEAS) = &
                                  EL_MON_MDS_CAPACITY(0,J,R_ISEAS) + &
                                      EL_MON_MDS_CAPACITY(I,J,R_ISEAS)
               EL_MON_MDS_ENERGY(0,J,R_ISEAS) = &
                                     EL_MON_MDS_ENERGY(0,J,R_ISEAS) + &
                                         EL_MON_MDS_ENERGY(I,J,R_ISEAS)
               EL_MON_MDS_SELL_CAPACITY(0,J,R_ISEAS) = &
                              EL_MON_MDS_SELL_CAPACITY(0,J,R_ISEAS) + &
                                  EL_MON_MDS_SELL_CAPACITY(I,J,R_ISEAS)
               EL_MON_MDS_SELL_ENERGY(0,J,R_ISEAS) = &
                               EL_MON_MDS_SELL_ENERGY(0,J,R_ISEAS) + &
                                    EL_MON_MDS_SELL_ENERGY(I,J,R_ISEAS)
               EL_MON_MDS_PURCHASES(0,J,R_ISEAS) = &
                                 EL_MON_MDS_PURCHASES(0,J,R_ISEAS) + &
                                      EL_MON_MDS_PURCHASES(I,J,R_ISEAS)
!  ANNUAL TOTALS
               EL_MON_MDS_VAR_COST(I,J,0) = &
                                       EL_MON_MDS_VAR_COST(I,J,0)+ &
                                      EL_MON_MDS_VAR_COST(I,J,R_ISEAS)
               EL_MON_MDS_FIXED_COST(I,J,0) = &
                                    EL_MON_MDS_FIXED_COST(I,J,0) + &
                                     EL_MON_MDS_FIXED_COST(I,J,R_ISEAS)
               EL_MON_MDS_REVENUE(I,J,0) = &
                                       EL_MON_MDS_REVENUE(I,J,0) + &
                                        EL_MON_MDS_REVENUE(I,J,R_ISEAS)
               EL_MON_MDS_CAPACITY(I,J,0) = &
                                      EL_MON_MDS_CAPACITY(I,J,0) + &
                                       EL_MON_MDS_CAPACITY(I,J,R_ISEAS)
               EL_MON_MDS_ENERGY(I,J,0) = &
                                        EL_MON_MDS_ENERGY(I,J,0) + &
                                         EL_MON_MDS_ENERGY(I,J,R_ISEAS)
               EL_MON_MDS_SELL_CAPACITY(I,J,0) = &
                                 EL_MON_MDS_SELL_CAPACITY(I,J,0) + &
                                  EL_MON_MDS_SELL_CAPACITY(I,J,R_ISEAS)
               EL_MON_MDS_SELL_ENERGY(I,J,0) = &
                                   EL_MON_MDS_SELL_ENERGY(I,J,0) + &
                                    EL_MON_MDS_SELL_ENERGY(I,J,R_ISEAS)
               EL_MON_MDS_PURCHASES(I,J,0) = &
                                     EL_MON_MDS_PURCHASES(I,J,0) + &
                                      EL_MON_MDS_PURCHASES(I,J,R_ISEAS)
!  ANNUAL TOTALS FOR ALL CLASSES
               EL_MON_MDS_VAR_COST(0,J,0) = &
                                       EL_MON_MDS_VAR_COST(0,J,0)+ &
                                       EL_MON_MDS_VAR_COST(I,J,R_ISEAS)
               EL_MON_MDS_FIXED_COST(0,J,0) = &
                                    EL_MON_MDS_FIXED_COST(0,J,0) + &
                                     EL_MON_MDS_FIXED_COST(I,J,R_ISEAS)
               EL_MON_MDS_REVENUE(0,J,0) = &
                                       EL_MON_MDS_REVENUE(0,J,0) + &
                                        EL_MON_MDS_REVENUE(I,J,R_ISEAS)
               EL_MON_MDS_CAPACITY(0,J,0) = &
                                      EL_MON_MDS_CAPACITY(0,J,0) + &
                                       EL_MON_MDS_CAPACITY(I,J,R_ISEAS)
               EL_MON_MDS_ENERGY(0,J,0) = &
                                        EL_MON_MDS_ENERGY(0,J,0) + &
                                         EL_MON_MDS_ENERGY(I,J,R_ISEAS)
               EL_MON_MDS_SELL_CAPACITY(0,J,0) = &
                                 EL_MON_MDS_SELL_CAPACITY(0,J,0) + &
                                  EL_MON_MDS_SELL_CAPACITY(I,J,R_ISEAS)
               EL_MON_MDS_SELL_ENERGY(0,J,0) = &
                                   EL_MON_MDS_SELL_ENERGY(0,J,0) + &
                                    EL_MON_MDS_SELL_ENERGY(I,J,R_ISEAS)
               EL_MON_MDS_PURCHASES(0,J,0) = &
                                     EL_MON_MDS_PURCHASES(0,J,0) + &
                                      EL_MON_MDS_PURCHASES(I,J,R_ISEAS)
            ENDDO
         ENDDO
      RETURN ! CALC_EL_MON_MDS_ASSET_CLASS



      ENTRY RETURN_EL_INTRA_ADJUSTMENTS(R_INTRA_SALES_REVENUE, &
                                        R_INTRA_PURCHASE_EXPENSES, &
                                        R_INTRA_VARIABLE_EXPENSES, &
                                        R_INTRA_FIXED_EXPENSES)

         R_INTRA_SALES_REVENUE = R_INTRA_SALES_REVENUE + &
                             (INTRA_SALES_REVENUE + &
                               INTRA_COMPANY_SALES_REVENUE(0))/1000000.
         R_INTRA_PURCHASE_EXPENSES = R_INTRA_PURCHASE_EXPENSES + &
                            (INTRA_PURCHASE_EXPENSES + &
                            INTRA_COMPANY_PURCHASE_EXPENSE(0))/1000000.
         R_INTRA_VARIABLE_EXPENSES = R_INTRA_VARIABLE_EXPENSES + &
                                       INTRA_VARIABLE_EXPENSES/1000000.
         R_INTRA_FIXED_EXPENSES = R_INTRA_FIXED_EXPENSES + &
                                          INTRA_FIXED_EXPENSES/1000000.
      RETURN

      ENTRY RETURN_MONTHLY_EL_EXPENSES(R_CLASS,MONTH_VARS)

         IF(.NOT. ALLOCATED(EL_MON_MDS_PURCHASES)) RETURN
         IF(R_CLASS <= MAX_HYDRO_CLASS_NUM) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
               DO MO = 0, 12
! I ASSUME THAT THE FOLLOWING THREE VARIABLES ARE TIME INDEPENDENT. 
                  MONTH_VARS(MO,Purchased Power) = &
                          MONTH_VARS(MO,Purchased Power) + &
                          EL_MON_MDS_PURCHASES(ASSET_CLASS,1,MO) + &
                          EL_MON_MDS_PURCHASES(ASSET_CLASS,2,MO) + &
                          EL_MON_MDS_PURCHASES(ASSET_CLASS,3,MO) + &
                          EL_MON_MDS_MARKET_PURCHASE(ASSET_CLASS,MO)
                  MONTH_VARS(MO,Variable OandM) = &
                           MONTH_VARS(MO,Variable OandM) + &
                           EL_MON_MDS_VAR_COST(ASSET_CLASS,1,MO) + &
                           EL_MON_MDS_VAR_COST(ASSET_CLASS,2,MO) + &
                           EL_MON_MDS_VAR_COST(ASSET_CLASS,3,MO)
                  MONTH_VARS(MO,Fixed OandM) = &
                         MONTH_VARS(MO,Fixed OandM) + &
                         EL_MON_MDS_FIXED_COST(ASSET_CLASS,1,MO) + &
                         EL_MON_MDS_FIXED_COST(ASSET_CLASS,2,MO) + &
                         EL_MON_MDS_FIXED_COST(ASSET_CLASS,3,MO)
                  MONTH_VARS(MO,BTL Expenses) = &
                         MONTH_VARS(MO,BTL Expenses) + &
                         EL_MON_MDS_PURCHASES(ASSET_CLASS,4,MO) + &
                         EL_MON_MDS_VAR_COST(ASSET_CLASS,4,MO) + &
                         EL_MON_MDS_FIXED_COST(ASSET_CLASS,4,MO)
               ENDDO
            ENDIF
         ENDIF
      RETURN

      ENTRY RETURN_MONTHLY_EL_CASH_EXPENSES(R_CLASS,MONTH_VARS)

         IF(.NOT. ALLOCATED(EL_MON_MDS_PURCHASES)) RETURN
         IF(R_CLASS <= MAX_HYDRO_CLASS_NUM) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
               DO MO = 0, 12
! I ASSUME THAT THE FOLLOWING THREE VARIABLES ARE TIME INDEPENDENT
                  MONTH_VARS(MO,cash_purchased_power) = &
                          MONTH_VARS(MO,cash_purchased_power) + &
                          EL_MON_MDS_PURCHASES(ASSET_CLASS,1,MO) + &
                          EL_MON_MDS_PURCHASES(ASSET_CLASS,2,MO) + &
                          EL_MON_MDS_PURCHASES(ASSET_CLASS,3,MO) + &
                          EL_MON_MDS_MARKET_PURCHASE(ASSET_CLASS,MO)
                  MONTH_VARS(MO,Cash_Variable_OandM) = &
                           MONTH_VARS(MO,Cash_Variable_OandM) + &
                           EL_MON_MDS_VAR_COST(ASSET_CLASS,1,MO) + &
                           EL_MON_MDS_VAR_COST(ASSET_CLASS,2,MO) + &
                           EL_MON_MDS_VAR_COST(ASSET_CLASS,3,MO)
                  MONTH_VARS(MO,Cash_Fixed_OandM) = &
                         MONTH_VARS(MO,Cash_Fixed_OandM) + &
                         EL_MON_MDS_FIXED_COST(ASSET_CLASS,1,MO) + &
                         EL_MON_MDS_FIXED_COST(ASSET_CLASS,2,MO) + &
                         EL_MON_MDS_FIXED_COST(ASSET_CLASS,3,MO)
                  MONTH_VARS(MO,Cash BTL Expenses) = &
                         MONTH_VARS(MO,Cash BTL Expenses) + &
                         EL_MON_MDS_PURCHASES(ASSET_CLASS,4,MO) + &
                         EL_MON_MDS_VAR_COST(ASSET_CLASS,4,MO) + &
                         EL_MON_MDS_FIXED_COST(ASSET_CLASS,4,MO)
               ENDDO
            ENDIF
         ENDIF
      RETURN

      ENTRY RETURN_MONTHLY_EL_CASH_REVENUES(R_CLASS,MONTH_VARS)

         IF(.NOT. ALLOCATED(EL_MON_MDS_REVENUE)) RETURN
         IF(R_CLASS <= MAX_HYDRO_CLASS_NUM) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
               DO MO = 0, 12
                  MONTH_VARS(MO,cash_adjustment_clause) = &
                          MONTH_VARS(MO,cash_adjustment_clause) + &
                          EL_MON_MDS_PURCHASES(ASSET_CLASS,1,MO) + &
                         EL_MON_MDS_MARKET_PURCHASE(ASSET_CLASS,MO) + &
                          EL_MON_MDS_VAR_COST(ASSET_CLASS,1,MO) + &
                          EL_MON_MDS_FIXED_COST(ASSET_CLASS,1,MO)
                  MONTH_VARS(MO,Cash Secondary Sales) = &
                          MONTH_VARS(MO,Cash Secondary Sales) + &
                          EL_MON_MDS_REVENUE(ASSET_CLASS,1,MO) + &
                          EL_MON_MDS_REVENUE(ASSET_CLASS,2,MO) + &
                          EL_MON_MDS_REVENUE(ASSET_CLASS,3,MO) + &
                          EL_MON_MDS_MARKET_REVENUES(ASSET_CLASS,MO)
                  MONTH_VARS(MO,Cash BTL Revenues) = &
                            MONTH_VARS(MO,Cash BTL Revenues) + &
                            EL_MON_MDS_REVENUE(ASSET_CLASS,4,MO)
               ENDDO
            ENDIF
         ENDIF
      RETURN
      END subroutine EL_POOL_COSTS_AND_UNIT_REPORTS
      
      subroutine RETURN_MONTHLY_EL_REVENUES(R_CLASS,MONTH_VARS)
         use enrg_helper
         use namescom
		 use mthnmcom
         implicit none
         
         integer (kind=2), intent(in) :: r_class
         integer (kind=2) :: mo
         REAL (kind=4) :: MONTH_VARS(0:12,1:*)
         
         IF(.NOT. ALLOCATED(EL_MON_MDS_REVENUE)) RETURN
         IF(R_CLASS <= MAX_HYDRO_CLASS_NUM) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
               DO MO = 0, 12
                  MONTH_VARS(MO,cash_adjustment_clause) = &
                          MONTH_VARS(MO,cash_adjustment_clause) + &
                          EL_MON_MDS_PURCHASES(ASSET_CLASS,1,MO) + &
                         EL_MON_MDS_MARKET_PURCHASE(ASSET_CLASS,MO) + &
                          EL_MON_MDS_VAR_COST(ASSET_CLASS,1,MO) + &
                          EL_MON_MDS_FIXED_COST(ASSET_CLASS,1,MO)
                  MONTH_VARS(MO,SecondarySales) = &
                          MONTH_VARS(MO,SecondarySales) + &
                          EL_MON_MDS_REVENUE(ASSET_CLASS,1,MO) + &
                          EL_MON_MDS_REVENUE(ASSET_CLASS,2,MO) + &
                          EL_MON_MDS_REVENUE(ASSET_CLASS,3,MO) + &
                          EL_MON_MDS_MARKET_REVENUES(ASSET_CLASS,MO)
                  MONTH_VARS(MO,btl_monthly_other_income) = &
                            MONTH_VARS(MO,btl_monthly_other_income) + &
                            EL_MON_MDS_REVENUE(ASSET_CLASS,4,MO)
               ENDDO
            ENDIF
         ENDIF
      end subroutine RETURN_MONTHLY_EL_REVENUES

      SUBROUTINE PEAK_SALE(IPEAK_SALE_ENERGY, &
               IPEAK_SALE_CAPACITY,LPROB,LODDUR,HOURS,LDCPTS)

       use SpinDriftLib
             use prod_arrays_dimensions
      use sizecom
      INTEGER*2 LDCPTS,HOURS,LOAD_POINT
      real*8 IPEAK_SALE_ENERGY,IPEAK_SALE_CAPACITY, & 
            LPROB(LOAD_CURVE_POINTS), &
            LODDUR(LOAD_CURVE_POINTS),REAL_HOURS,BASE_CAPACITY, &
            A_COEFF,B_COEFF,C_COEFF,X1,Y2,PEAK_SALE_AREA
      CHARACTER*11 LOOP_MODE
!  (1) NEED TO ADJUST FOR LF > 50% IE NEED TO TRAP FOR X1 > 1; 
!  (2) NEED TO MAKE CHECKS THAT THERE IS ENOUGH LDC LEFT;
!  (3) NEED TO TRAP FOR THE PROB VALUE CLOSEST TO X1. DONE. 4/6.

      B_COEFF= 0. !Spurious Lahey warning
      REAL_HOURS = dble(HOURS)
      PEAK_SALE_AREA = IPEAK_SALE_ENERGY/REAL_HOURS
      X1 = 2.*PEAK_SALE_AREA/IPEAK_SALE_CAPACITY
      BASE_CAPACITY = 0.
      IF(X1 > 1.0) THEN
         BASE_CAPACITY = .5*(X1 - 1) * IPEAK_SALE_CAPACITY
         X1 = 1.0
         PEAK_SALE_AREA = PEAK_SALE_AREA - BASE_CAPACITY
      ENDIF
      LOAD_POINT = 0
      LOOP_MODE = 'SEARCHING  '
      DOWHILE( LOAD_POINT < LDCPTS)
         LOAD_POINT = LOAD_POINT + 1
         LODDUR(LOAD_POINT) = LODDUR(LOAD_POINT) + BASE_CAPACITY
         IF(LOOP_MODE == 'SEARCHING  ') THEN
            IF(LPROB(MIN(LOAD_POINT+1,LDCPTS)) > X1) CYCLE
            IF(ABS(LPROB(LOAD_POINT  ) - X1) > &
               ABS(LPROB(LOAD_POINT+1) - X1) ) &
               LOAD_POINT = LOAD_POINT + 1
            Y2 = IPEAK_SALE_CAPACITY - BASE_CAPACITY
            X1 = LPROB(LOAD_POINT)
            C_COEFF = Y2
            B_COEFF = (6.*(PEAK_SALE_AREA-X1*Y2) + 2.*X1*Y2)/X1**2
            A_COEFF = -Y2/X1**2 - B_COEFF/X1
            LOOP_MODE = 'CALCULATING'
         ELSE
            LODDUR(LOAD_POINT) = LODDUR(LOAD_POINT) + &
                     A_COEFF*LPROB(LOAD_POINT)**2 + &
                     B_COEFF*LPROB(LOAD_POINT) + C_COEFF
         ENDIF
      ENDDO
      RETURN
      END subroutine peak_sale

!      THIS ROUTINE ALTERS THE LOAD DURATION CURVE ACCORDING TO
!      A PRESPECIFIED TYPE OF PURCHASE. DEVELOPED FOR WKP. 
      SUBROUTINE WKP_BFP_PURCHASE(  ISEAS, &
                                    PURCHASE_TYPE, &
                                    PURCHASE_ENERGY_IN, &
                                    PURCHASE_CAPACITY_IN,LPROB, &
                                    LODDUR,REAL_HOURS,LDCPTS, &
                                    PURCHASE_ENERGY_OUT, &
                                    PURCHASE_CAPACITY_OUT, &
                                    WHOLESALE_MARKET_IN)
      use end_routine, only: end_program, er_message
      use mod_base_year
      use SpinDriftLib
             use prod_arrays_dimensions
      use sizecom
      use globecom
      implicit none
      LOGICAL*1   J_ALWAYS_FIRST/.FALSE./
      INTEGER*2   LDCPTS,HOURS,LOAD_POINT,PTS,LAST_WHOLESALE_POINT, &
                  ISEAS
      REAL  PURCHASE_ENERGY_IN,PURCHASE_CAPACITY_IN, &
            LPROB(LOAD_CURVE_POINTS), &
            LODDUR(LOAD_CURVE_POINTS), &
            WHOLESALE_LODDUR(LOAD_CURVE_POINTS), &
            RETAIL_LODDUR(LOAD_CURVE_POINTS), &
            WHOLESALE_LPROB(LOAD_CURVE_POINTS), &
            WHOLESALE_INTERVAL_AREA, &
            REAL_HOURS, &
            A_COEFF,B_COEFF,C_COEFF,X1,Y2,PURCHASE_AREA, &
            LOAD_CURVE_BASE,LOWER_CURVE_POINT,UPPER_CURVE_POINT, &
            CURVE_AREA,INTERVAL_AREA,A,LEFT,PURCHASE_ENERGY_OUT, &
            PURCHASE_CAPACITY_OUT,LDC_MULT,INTERVAL_CAPACITY, &
            WHOLESALE_MARKET_IN,WHOLESALE_MARKET_MULT/0/, &
            LOCAL_CAPACITY,LOCAL_ENERGY, &
            TOTAL_INTERVAL_AREA
      REAL*8 ENERGY_BEFORE,ENERGY_AFTER
      CHARACTER*1    PURCHASE_TYPE
      CHARACTER*11   LOOP_MODE
      SAVE WHOLESALE_LODDUR,WHOLESALE_LPROB,RETAIL_LODDUR
! END DATA DECLARATIONS
! OUTPUT MUST INCLUDE ADJUSTING THE LOAD CURVE AND ASSIGNING
! THE ENERGY AND CAPACITY OF THE PURCHASE.
      WHOLESALE_MARKET_MULT = WHOLESALE_MARKET_IN /100.
      IF(LDCPTS > 1000 .OR. LDCPTS <= 1) THEN
         WRITE(4,*) "LOAD DURATION CURVE IN PURCHASE ROUTINE"
         WRITE(4,*) "IS INCORRECT."
         WRITE(4,*) '*** line 3981 ENRGLIMT.FOR ***'
         er_message='See WARNING MESSAGES -Enrglimt.for-1'
         call end_program(er_message)
      ENDIF
!       REAL_HOURS = FLOAT(HOURS)
      LOCAL_CAPACITY = PURCHASE_CAPACITY_IN
      LOCAL_ENERGY = PURCHASE_ENERGY_IN
      PURCHASE_AREA = LOCAL_ENERGY/REAL_HOURS
!
      IF(PURCHASE_TYPE == 'F') THEN
         IF(.NOT. J_ALWAYS_FIRST) THEN
          WRITE(4,*) "MUST HAVE J UNIT FIRST"
          WRITE(4,*) "*** line 3994 ENRGLIMT.FOR *** "
          er_message='See WARNING MESSAGES -Enrglimt.for-2'
          call end_program(er_message)
         ENDIF
!
         LOAD_CURVE_BASE = WHOLESALE_LODDUR(1)
         PURCHASE_CAPACITY_OUT = MIN(LOCAL_CAPACITY,PURCHASE_AREA)
         IF(LOAD_CURVE_BASE < PURCHASE_CAPACITY_OUT) THEN
            WRITE(4,*) "INSUFFICIENT BASE IN LOAD CURVE "
            WRITE(4,*) "FOR BASE PURCHASE "
            WRITE(4,*) "IN ENDPOINT ",gc_end_point
            WRITE(4,*) "YEAR ",globecom_year+get_BASE_YEAR()," AND SEASON ", ISEAS
         ELSE
            CALL INTEG8(ENERGY_BEFORE,LODDUR,LPROB,LDCPTS, &
                                            NINT(REAL_HOURS),LODDUR(1))
            DO LOAD_POINT = 1, LDCPTS
               LODDUR(LOAD_POINT) = &
                             LODDUR(LOAD_POINT) - PURCHASE_CAPACITY_OUT
            ENDDO
            CALL INTEG8(ENERGY_AFTER,LODDUR,LPROB,LDCPTS, &
                                            NINT(REAL_HOURS),LODDUR(1))
            PURCHASE_ENERGY_OUT = SNGL(ENERGY_BEFORE - ENERGY_AFTER)/ &
                                                             REAL_HOURS
         ENDIF
      ELSEIF(PURCHASE_TYPE == 'G') THEN
         IF(.NOT. J_ALWAYS_FIRST) THEN
            WRITE(4,*) "MUST HAVE J UNIT FIRST"
            WRITE(4,*) '*** line 4026 ENRGLIMT.FOR ***'
            er_message='See WARNING MESSAGES -Enrglimt.for-3'
            call end_program(er_message)
         ENDIF
         IF(LOCAL_ENERGY > LOCAL_CAPACITY) THEN
            WRITE(4,*) "LOWER BOUND FOR FACTOR PURCHASE "
            WRITE(4,*) "GREATER THAN UPPER BOUND."
            WRITE(4,*) '*** line 4033 ENRGLIMT.FOR ***'
            er_message='See WARNING MESSAGES -Enrglimt.for-4'
            call end_program(er_message)
         ENDIF
         DO LOAD_POINT = 1, LDCPTS
            IF(LPROB(LOAD_POINT) > LOCAL_CAPACITY) EXIT
!
            IF(LPROB(LOAD_POINT) < LOCAL_CAPACITY) THEN
               UPPER_CURVE_POINT = LOAD_POINT
            ENDIF
            IF(LPROB(LOAD_POINT) < LOCAL_ENERGY) THEN
               LOWER_CURVE_POINT = LOAD_POINT
            ENDIF
         ENDDO
! HAVE A NUMBER OF CASES TO CHECK
      ELSEIF(PURCHASE_TYPE == 'H') THEN
         IF(.NOT. J_ALWAYS_FIRST) THEN
            WRITE(4,*) "MUST HAVE J UNIT FIRST"
            WRITE(4,*) '*** KEY Enrglimt.for-5 ENRGLIMT.FOR ***'
            er_message='See WARNING MESSAGES -Enrglimt.for-5'
            call end_program(er_message)
         ENDIF
!
         CURVE_AREA = 0.
         PURCHASE_CAPACITY_OUT = 0.
!        ONLY USED IF CAPACITY IS EXCEEDED BEFORE ENERGY
         CALL INTEG8(ENERGY_BEFORE,LODDUR,LPROB,LDCPTS, &
                                            NINT(REAL_HOURS),LODDUR(1))
         PTS = LDCPTS
         DOWHILE(PTS > 1 .AND. LPROB(PTS-1) == 0.0)
            PTS = PTS - 1
         ENDDO
         DO LOAD_POINT = PTS, 2, -1
! AMOUNT OF ENERGY THAT I WANT TO REMOVE IN THIS INTERVAL
            WHOLESALE_INTERVAL_AREA = &
                  (WHOLESALE_LPROB(LOAD_POINT) + &
                                     WHOLESALE_LPROB(LOAD_POINT-1)) * &
                     (WHOLESALE_LODDUR(LOAD_POINT) - &
                                   WHOLESALE_LODDUR(LOAD_POINT-1))* .5

            INTERVAL_CAPACITY = &
               WHOLESALE_LODDUR(LOAD_POINT) - &
                                         WHOLESALE_LODDUR(LOAD_POINT-1)
            CURVE_AREA = CURVE_AREA + WHOLESALE_INTERVAL_AREA
            IF(CURVE_AREA >= PURCHASE_AREA) EXIT
            IF(PURCHASE_CAPACITY_OUT + INTERVAL_CAPACITY > &
                                              LOCAL_CAPACITY) EXIT
            PURCHASE_CAPACITY_OUT = PURCHASE_CAPACITY_OUT + &
                                                      INTERVAL_CAPACITY
         ENDDO
         LAST_WHOLESALE_POINT = LOAD_POINT
         IF(PURCHASE_CAPACITY_OUT + INTERVAL_CAPACITY > &
                                              LOCAL_CAPACITY) THEN
            A = WHOLESALE_LODDUR(LOAD_POINT) - INTERVAL_CAPACITY
            WHOLESALE_LODDUR(LOAD_POINT) = A
!
            PURCHASE_CAPACITY_OUT = LOCAL_CAPACITY
            DO LOAD_POINT = 1, PTS
               IF(LOAD_POINT < LAST_WHOLESALE_POINT-1) THEN
                  LODDUR(LOAD_POINT) = RETAIL_LODDUR(LOAD_POINT) + &
                                           WHOLESALE_LODDUR(LOAD_POINT)
               ELSE
                  LODDUR(LOAD_POINT) = RETAIL_LODDUR(LOAD_POINT) + A
               ENDIF
            ENDDO
            CALL INTEG8(ENERGY_AFTER,LODDUR,LPROB,PTS, &
                                            NINT(REAL_HOURS),LODDUR(1))
            PURCHASE_ENERGY_OUT = SNGL(ENERGY_BEFORE - ENERGY_AFTER)/ &
                                                             REAL_HOURS
         ELSEIF(CURVE_AREA < PURCHASE_AREA) THEN ! GO AFTER THE BASE
            DO LOAD_POINT = 1, PTS
               LODDUR(LOAD_POINT) = RETAIL_LODDUR(LOAD_POINT)
            ENDDO
!
            INTERVAL_AREA = PURCHASE_AREA - CURVE_AREA
            A = LODDUR(1)
            IF(A < INTERVAL_AREA) THEN
               WRITE(4,*) "INSUFFICIENT BASE IN LOAD CURVE "
               WRITE(4,*) "FOR PEAK PURCHASE "
               WRITE(4,*) "IN ENDPOINT ",gc_end_point
               WRITE(4,*) "YEAR ",globecom_year+get_BASE_YEAR()," AND SEASON ", ISEAS
            ELSE
               DO LOAD_POINT = 1, PTS
                  LODDUR(LOAD_POINT) = &
                                  LODDUR(LOAD_POINT) - INTERVAL_AREA
               ENDDO
            ENDIF
!             PURCHASE_ENERGY_OUT = PURCHASE_AREA
            CALL INTEG8(ENERGY_AFTER,LODDUR,LPROB,PTS, &
                                            NINT(REAL_HOURS),LODDUR(1))
            PURCHASE_ENERGY_OUT = SNGL(ENERGY_BEFORE - ENERGY_AFTER)/ &
                                                             REAL_HOURS
            PURCHASE_CAPACITY_OUT = PURCHASE_CAPACITY_OUT + &
                                                          INTERVAL_AREA
         ELSE ! ENERGY CONTRAINT WITHIN THE CURVE
!             INTERVAL_AREA = 0.
            CALL FIND_MAXIMUM_ENERGY(WHOLESALE_LODDUR(LOAD_POINT), &
                                     WHOLESALE_LODDUR(LOAD_POINT-1), &
                                       WHOLESALE_LPROB(LOAD_POINT), &
                                       WHOLESALE_LPROB(LOAD_POINT-1), &
                                       CURVE_AREA, &
                                       PURCHASE_AREA, &
                                       A,LEFT)
!             LPROB(LOAD_POINT) = LEFT
!             LODDUR(LOAD_POINT) = A
!             PURCHASE_ENERGY_OUT = PURCHASE_AREA
            DO LOAD_POINT = 1, PTS
               IF(LOAD_POINT < LAST_WHOLESALE_POINT-1) THEN
                  LODDUR(LOAD_POINT) = RETAIL_LODDUR(LOAD_POINT) + &
                                           WHOLESALE_LODDUR(LOAD_POINT)
               ELSE
                  LODDUR(LOAD_POINT) = RETAIL_LODDUR(LOAD_POINT) + A
               ENDIF
            ENDDO
            CALL INTEG8(ENERGY_AFTER,LODDUR,LPROB,PTS, &
                                            NINT(REAL_HOURS),LODDUR(1))
            PURCHASE_ENERGY_OUT = SNGL(ENERGY_BEFORE - ENERGY_AFTER)/ &
                                                             REAL_HOURS
            PURCHASE_CAPACITY_OUT = PURCHASE_CAPACITY_OUT + &
                             WHOLESALE_LODDUR(LAST_WHOLESALE_POINT) - A
         ENDIF

      ELSEIF(PURCHASE_TYPE == 'J') THEN
         J_ALWAYS_FIRST = .TRUE.
!
         WHOLESALE_MARKET_MULT = WHOLESALE_MARKET_IN /100.
         DO LOAD_POINT = 1, LDCPTS
            WHOLESALE_LODDUR(LOAD_POINT) = LODDUR(LOAD_POINT) * &
                                                  WHOLESALE_MARKET_MULT
            RETAIL_LODDUR(LOAD_POINT) = LODDUR(LOAD_POINT) - &
                                           WHOLESALE_LODDUR(LOAD_POINT)
            WHOLESALE_LPROB(LOAD_POINT) = LPROB(LOAD_POINT)
         ENDDO
!
         CALL INTEG8(ENERGY_BEFORE,WHOLESALE_LODDUR,LPROB,LDCPTS, &
                                  NINT(REAL_HOURS),WHOLESALE_LODDUR(1))
         LDC_MULT = LOCAL_ENERGY/100.
         PURCHASE_CAPACITY_OUT = WHOLESALE_LODDUR(LDCPTS) * &
                                                        (1. - LDC_MULT)
         DO LOAD_POINT = 1, LDCPTS
           LODDUR(LOAD_POINT) =  LODDUR(LOAD_POINT) - (1.-LDC_MULT) * &
                                           WHOLESALE_LODDUR(LOAD_POINT)
            WHOLESALE_LODDUR(LOAD_POINT) = &
                                WHOLESALE_LODDUR(LOAD_POINT) * LDC_MULT
         ENDDO
         CALL INTEG8(ENERGY_AFTER,WHOLESALE_LODDUR,LPROB,LDCPTS, &
                                  NINT(REAL_HOURS),WHOLESALE_LODDUR(1))
         PURCHASE_ENERGY_OUT = SNGL(ENERGY_BEFORE - ENERGY_AFTER)/ &
                                                             REAL_HOURS
      ELSE
         WRITE(4,*) "UNDEFINED PURCHASE TYPE IN THE "
         WRITE(4,*) "ENERGY LIMITED FILE."
         WRITE(4,*) '*** line 4262 ENRGLIMT.FOR ***'
         er_message='See WARNING MESSAGES -Enrglimt.for-6'
         call end_program(er_message)
      ENDIF
      RETURN
      END subroutine WKP_BFP_PURCHASE

      SUBROUTINE WRITE_ASSET_PROD_REPORT
      use mod_base_year
      use irec_endpoint_control
      use SpinDriftLib
             use prod_arrays_dimensions
      use sizecom
      use globecom
      use endpoint
      use enrg_helper
      implicit none


! DETAILED REPORT OVERHEAD
      LOGICAL*1   ASSET_PROD_REPORT_NOT_OPEN/.TRUE./, &
                  ASSET_PROD_REPORT_ACTIVE, class_exists_loc2, &
                  RETURN_CL_CLASS_TOTAL_PROD,FOUND_PROD, &
                  ASSET_CLASS_REPORTS, R_CLASS_EXISTS_local
      INTEGER*2   ASSET_PROD_RPT_HEADER, &
                  ASSET_PRODUCTION_UNIT, &
                  CURRENT_YEAR,MAX_CLASS_NUM,idx_class, &
                  RESOURCE
      INTEGER ASSET_PRODUCTION_REC
      REAL*4      ANN_CLASS_CAPACITY,ANN_CLASS_ENERGY, &
                  ANN_CL_CLASS_CAPACITY,ANN_CL_CLASS_ENERGY, &
                  ANN_EL_GEN_CLASS_CAPACITY,ANN_EL_GEN_CLASS_ENERGY, &
                 ANN_EL_SELL_CLASS_CAPACITY,ANN_EL_SELL_CLASS_ENERGY, &
                  ANN_TOTAL_CLASS_CAPACITY,ANN_TOTAL_CLASS_ENERGY, &
                  ONE_MILLION/1000000./,ONE_THOUSAND/1000./
      CHARACTER*20 RESOURCE_TYPE(5)
      DATA        RESOURCE_TYPE/'Capacity Limited    ', &
                                'Energy Limited Gener', &
                                'Energy Limited Sales', &
                                'Contracts           ', &
                                'Demand Side Manage  '/

      
      SAVE ASSET_PRODUCTION_UNIT,MAX_CLASS_NUM,CURRENT_YEAR
      SAVE ASSET_PRODUCTION_REC
!     END DATA DECLARATIONS
      ASSET_PROD_REPORT_ACTIVE = ASSET_CLASS_REPORTS() .AND. .FALSE.
      IF(.NOT. ASSET_PROD_REPORT_ACTIVE .OR. TESTING_PLAN) RETURN
!
      CURRENT_YEAR = globecom_year + get_BASE_YEAR()
      IF(ASSET_PROD_REPORT_NOT_OPEN .AND. &
                             ASSET_PROD_REPORT_ACTIVE .AND. &
                                              .NOT. TESTING_PLAN ) THEN
         ASSET_PROD_REPORT_NOT_OPEN = .FALSE.
         ASSET_PRODUCTION_UNIT = ASSET_PROD_RPT_HEADER( &
                                                  ASSET_PRODUCTION_REC)
         CALL GET_MAX_CLASS_NUM(MAX_CLASS_NUM)
         IF(MAX_CLASS_NUM >= 0 .AND. MAX_CLASS_NUM <= 1001) THEN
            ALLOCATE(CLASS_NAME(0:MAX_CLASS_NUM))
            CLASS_NAME(0) = "Consolidated                  "
            DO idx_class = 1, MAX_CLASS_NUM
               CALL GET_PROD_CLASS_NAME(idx_class,CLASS_NAME(idx_class))
            ENDDO
         ELSE
            WRITE(4,*) "Invalid maximum number of classes = ", &
                                                          MAX_CLASS_NUM
         ENDIF
      ENDIF
      DO idx_class = 0, MAX_CLASS_NUM
!
         ANN_TOTAL_CLASS_CAPACITY = 0.
         ANN_TOTAL_CLASS_ENERGY = 0.
!
         FOUND_PROD = RETURN_CL_CLASS_TOTAL_PROD(idx_class, &
                                               ANN_CL_CLASS_CAPACITY, &
                                                 ANN_CL_CLASS_ENERGY)
         ANN_CL_CLASS_CAPACITY = ANN_CL_CLASS_CAPACITY * ONE_MILLION
         ANN_CL_CLASS_ENERGY = ANN_CL_CLASS_ENERGY * ONE_MILLION
!

      CALL RETURN_EL_ASSET_CLASS_PROD( idx_class,class_exists_loc2, &
                                       ANN_EL_GEN_CLASS_CAPACITY, &
                                       ANN_EL_GEN_CLASS_ENERGY, &
          MAX_HYDRO_CLASS_NUM, asset_class_pointer, &
          el_ann_class_capacity)
          
         ANN_EL_GEN_CLASS_CAPACITY = ANN_EL_GEN_CLASS_CAPACITY * &
                                                            ONE_MILLION
        ANN_EL_GEN_CLASS_ENERGY = ANN_EL_GEN_CLASS_ENERGY * ONE_MILLION
!
         CALL RETURN_EL_ASSET_CLASS_SELL( &
                                       idx_class,R_CLASS_EXISTS_local, &
                                       ANN_EL_SELL_CLASS_CAPACITY, &
                                       ANN_EL_SELL_CLASS_ENERGY, &
          MAX_HYDRO_CLASS_NUM, ASSET_CLASS_POINTER, &
          EL_ANN_CLASS_SELL_CAPACITY, EL_ANN_CLASS_SELL_ENERGY)
                                       
         ANN_EL_SELL_CLASS_CAPACITY = ANN_EL_SELL_CLASS_CAPACITY * &
                                                            ONE_MILLION
         ANN_EL_SELL_CLASS_ENERGY = ANN_EL_SELL_CLASS_ENERGY * &
                                                            ONE_MILLION

         ANN_TOTAL_CLASS_CAPACITY = ANN_CL_CLASS_CAPACITY + &
                                    ANN_EL_GEN_CLASS_CAPACITY + &
                                    ANN_EL_SELL_CLASS_CAPACITY
         ANN_TOTAL_CLASS_ENERGY =   ANN_CL_CLASS_ENERGY + &
                                    ANN_EL_GEN_CLASS_ENERGY + &
                                    ANN_EL_SELL_CLASS_ENERGY

         WRITE(ASSET_PRODUCTION_UNIT,REC=ASSET_PRODUCTION_REC) &
                               PRT_ENDPOINT(), &
                               FLOAT(CURRENT_YEAR), &
                               CLASS_NAME(idx_class), &
                               ANN_CL_CLASS_CAPACITY, &
                               ANN_CL_CLASS_ENERGY, &
                               ANN_EL_GEN_CLASS_CAPACITY, &
                               ANN_EL_GEN_CLASS_CAPACITY, &
                               ANN_EL_SELL_CLASS_ENERGY, &
                               ANN_EL_SELL_CLASS_ENERGY, &
                               ANN_TOTAL_CLASS_CAPACITY, &
                               ANN_TOTAL_CLASS_ENERGY
         ASSET_PRODUCTION_REC = ASSET_PRODUCTION_REC + 1
      ENDDO
      RETURN
      END subroutine WRITE_ASSET_PROD_REPORT

!      NTEGRATE THE Load-Duration CURVE TO FIND THE ENERGY

      subroutine Integrate(Energy,LoadVal,LoadPrb,nPts,Duratn,BaseLoad)
!      same as INTEG8 except here the array arguments are real*8
      use foshydi2com
      real*8 Energy,LoadPrb(1000),LoadVal(1000),BaseLoad
      integer*2 i,Duratn,nPts
      Energy=0.d0
      do i=1,nPts-1
         Energy= &
         Energy+(LoadPrb(i)+LoadPrb(i+1))*(LoadVal(i+1)-LoadVal(i))
      end do
      Energy=(abs(Energy)/2.+BaseLoad)*dble(Duratn)
      end subroutine Integrate
      subroutine RETURN_CAWCD_ENERGY(R_CAWCD_ENERGY)
        real (kind=4) :: R_CAWCD_ENERGY
         R_CAWCD_ENERGY = -1.*CAWCD_ENERGY
      end subroutine return_cawcd_energy

      function STORE_PUMP_BY_HOUR(R_HOUR_IN_MONTH, &
                                R_TRANS_GROUP)
          implicit none
          real (kind=4) :: STORE_PUMP_BY_HOUR
          integer (kind=2) :: r_hour_in_month
          integer (kind=2) :: r_trans_group
          

            STORE_PUMP_BY_HOUR = &
                     HOURLY_HYDRO_EN(R_HOUR_IN_MONTH,R_TRANS_GROUP,2)

      end function store_pump_by_hour
      subroutine UPDATE_SEASON_FOR_EL_RPT(R_ISEAS)
         use enrg_helper
         use month_extra
         use foshydi2com
         use mod_base_year


         implicit none
         integer (kind=2), intent(in) :: r_iseas

         el_MONTH = get_MONTH_NAME(R_ISEAS)
         SAVE_MONTH = R_ISEAS
         SAVE_HOURS_IN_PERIOD = get_HOURS_IN_PERIOD(R_ISEAS)
         IF(SAVE_MONTH == 1) THEN
            IF(ALLOCATED(MONTHLY_EL_GROUP_REPORT)) &
                                    DEALLOCATE(MONTHLY_EL_GROUP_REPORT)
            MAX_MONTHLY_GROUPS = 99
            MAX_MONTHLY_GROUP_VARIABLES = 16
          ALLOCATE(MONTHLY_EL_GROUP_REPORT(0:12,0:MAX_MONTHLY_GROUPS, &
                                          MAX_MONTHLY_GROUP_VARIABLES))
            MONTHLY_EL_GROUP_REPORT = 0.
         ENDIF
! FISCAL REPORTING
         YES_FISCAL_REPORTING = &
           IS_FISCAL_YEAR_ACTIVE(FISCAL_SEASON_RESET, FISCAL_ONLY)

         IF(FISCAL_SEASON_RESET == 1) THEN
            FISCAL_SEASON = LAST_SEASON
         ELSE
            FISCAL_SEASON = FISCAL_SEASON_RESET - 1
         ENDIF
         LOCAL_YEAR = FLOAT(globecom_year+get_BASE_YEAR())
!
         IF(FISCAL_ONLY) THEN
            IF(FISCAL_SEASON_RESET > 1 .AND. &
                                SAVE_MONTH >= FISCAL_SEASON_RESET) THEN
               LOCAL_YEAR = FLOAT(globecom_year+get_BASE_YEAR()+1)
            ENDIF
         ENDIF
!
         IF(HYDRO_UNITS > 0) THEN
            IF(.NOT. ALLOCATED(FISCAL_EL_ENERGY)) THEN
               ALLOCATE(FISCAL_EL_ENERGY(HYDRO_UNITS,2))
               ALLOCATE(FISCAL_EL_CAPACITY(HYDRO_UNITS,2))
               ALLOCATE(FISCAL_EL_VAR_COST(HYDRO_UNITS,2))
               ALLOCATE(FISCAL_EL_FIXED_COST(HYDRO_UNITS,2))
               ALLOCATE(FISCAL_EL_SO2_EMIS(HYDRO_UNITS,2))
               ALLOCATE(FISCAL_EL_MARKET_REVENUE(HYDRO_UNITS,2))

               FISCAL_EL_ENERGY = 0.
               FISCAL_EL_CAPACITY = 0.
               FISCAL_EL_VAR_COST = 0.
               FISCAL_EL_FIXED_COST = 0.
               FISCAL_EL_SO2_EMIS = 0.
               FISCAL_EL_MARKET_REVENUE = 0.
            ENDIF
         ENDIF
!
      end subroutine UPDATE_SEASON_FOR_EL_RPT

!      CALCULATE THE HOURLY IMPACT OF DIFFERENT HYDRO
!                   PROGRAMS BY MULTI-AREA TRANSACTION GROUP
!               AFTER THEY HAVE BEEN OPERATED UPON BY HYDRO ROUTINES

      FUNCTION LDC_2_HOURS( &
                           R_LPROB,         & !  WLPROB R8 IS REVERSED
                           R_LODDUR,   & !  WLODDUR R8 IS REVERED
                           DEMAND_AFTER_EL,         & !  R8
                           TRANS_EL_GENERATION,   & ! 
                           TRANS_EL_CAPACITY,  & !  CHANGED TO R8
                           PEAK_AFTER_EL4,          &
                           R_ISEAS,                  &
                           LOAD_HOURS_IN_PERIOD,     &
                           R_TRANS_GROUP,            &
                           R_LDCPTS_AFTER_EL,        &
                           P_OR_G, &
                           R_CURRENT)
!      20020414 AGT changed most REAL*4 to real*8, most FLOAT to dble;
!         and added copying loops at entry and exit

      use SpinDriftLib
      use prod_arrays_dimensions
      use sizecom
      use globecom
      use msgoutpt_data
      use tf_decs
      save
      real (kind=4) :: LDC_2_HOURS
      INTEGER*2   I,R_ISEAS,el_MONTH,DAY,MC_YEAR, &
                  R_HR,MAX_TRANS_LOAD_GROUPS, &
                  R_TRANS_GROUP,R_LDCPTS_AFTER_EL, &
                  P_OR_G, & !  1 = GEN, 2 = PUMP
                  R_CURRENT, &
                  CURRENT, &
                  NEXT
      INTEGER*4   DAILY_MW(24),VALUES_2_ZERO
      REAL*4      LPROB4(CONVOLUTION_POINTS), & !  callers' variables
                  LODDUR4(CONVOLUTION_POINTS), &
                  TRANS_EL_CAPACITY4, &
                  PEAK_AFTER_EL4, &
                  INIT_MONTHLY_LDC_2_HOURS
      real*8      LPROB(CONVOLUTION_POINTS), &
                  LODDUR(CONVOLUTION_POINTS), &
                  TRANS_LPROB(CONVOLUTION_POINTS), &
                  TRANS_LODDUR(CONVOLUTION_POINTS), &
                  R_LPROB(CONVOLUTION_POINTS,2), & ! callers' variables
                  R_LODDUR(CONVOLUTION_POINTS,2), &
                  TRANS_EL_CAPACITY, &
                  PEAK_AFTER_EL, &
                 CALCULATED_PEAK,CALCULATED_BASE,PEAK_DIFF,BASE_DIFF, &
                  AVE_ENERGY_DIFF, &
                  AVE_GENERATION
      REAL*8      DEMAND_AFTER_EL, &
                  TRANS_EL_GENERATION, &
                  TOTAL_DEMAND_AFTER_EL
      INTEGER*4   I4_ENERGY,TOTAL_I4_ENERGY
      real*8      R8_ENERGY,TOTAL_R8_ENERGY,LOAD_ADJUSTER, &
                  REAL8_ZERO/0.D0/
!      MARGINAL COSTING VARIABLES
      LOGICAL*1   FOUND_SORTED_HOURS,GET_THOSE_SORTED_HOURS, &
                  HOURLY_OUTPUT_NOT_OPEN/.TRUE./, &
                  INIT_TRANS_LOAD_AFTER_EL,VOID_LOGICAL, &
                  GET_TRANS_HOUR_DISTRIBUTION, &
                  GET_HYDRO_HOUR_DISTRIBUTION, &
                  GET_HYDRO_LOAD_AFTER_EL, &
                  PUT_HYDRO_LOAD_AFTER_EL, &
                  TEMP_L
     INTEGER*2   START_MONTH,END_MONTH,LOWER_MC,LOAD_HOURS_IN_PERIOD, &
                 SORT_POS(:),EL_POS,HR,ADJUSTED_HOUR,HOUR_IN_THE_SEASON
      INTEGER*2   LAST_EL_POS,TARGET_TRANS_GROUP, &
                  GET_TRANS_GROUP_POSITION, &
                  GET_TRANS_LOAD_2_TRANS_GROUPS, &
                  ZERO_GROUP/0/, &
                  R_HYDRO_GROUP, &
                  MAX_TRANS_GROUP_NUMBER, &
                  GET_MAX_TRANS_GROUP_NUMBER
      real*4      GET_MONTHLY_TL_HYDRO_MWH, &
                  GET_MONTHLY_TL_HYDRO_MW, &
                  GET_TRANS_LOAD_AFTER_EL, &
                  GET_TRANS_HOURLY_HYDRO, &
                  GET_MONTHLY_TL_MWH, &
                  HOURLY_TRANSACTION_LOAD, &
                  GET_TRANS_PEAK_AFTER_EL, &
                  GET_TRANS_BASE_AFTER_EL
      real*8      CURRENT_PROB, &
                  TRANS_LOAD_AFTER_EL(:,:), &
                  TRANS_HOURLY_HYDRO(:,:), &
                  TEST_MIN
      ALLOCATABLE :: SORT_POS,TRANS_LOAD_AFTER_EL, &
                     TRANS_HOURLY_HYDRO
      !  passed to subroutines, these cannot be made real*8
      real*4 temp_ratio,HYDRO_LOAD_AFTER_EL(800), &  
                        HYDRO_LOAD_BEFORE_EL(800) 
                        
! DETAILED HYDRO_AG REPORT
      LOGICAL*1   WRITE_HOURLY_REPORT/.FALSE./, &
                  HYDRO_AG_REPORT_NOT_OPEN/.TRUE./
      INTEGER*2   START_HOUR, &
                  RPT_HOUR,HYDRO_AG_NO,HYDRO_AG_HEADER, &
                  CURRENT_YEAR,RPT_HR
      INTEGER*4   HYDRO_AG_REC/0/
      CHARACTER*9 CL_MONTH_NAME(13) &
                               /'January','February','March','April', &
                            'May','June','July','August','September', &
                              'October','November','December','Annual'/
      real*8 REMAIN
      CHARACTER*35 GET_GROUP_NAME,TEMP_STR
      INTEGER*2 TRANS_LOAD_POINTS, &
                  LAST_TRANS_POS, &
                  TRANS_POS, &
                  R_HOUR_IN_MONTH
      PARAMETER( TRANS_LOAD_POINTS=79)
      REAL*8 TRANS_DEMAND
      REAL*4 TRANS_DX, & !  REAL*4 args to external GET_TRANS_LOAD_PROB
             TRANS_PEAK, &
             TRANS_BASE, &
             TEMP_LOAD_BEFORE_EL, &
             TEMP_LOAD_AFTER_EL

      
      
      IF(R_CURRENT == 1) THEN
         CURRENT = 1
         NEXT = 2
      ELSE
         CURRENT = 2
         NEXT = 1
      ENDIF
      do I = 1, CONVOLUTION_POINTS
         IF(I <= R_LDCPTS_AFTER_EL .AND. P_OR_G == 1) THEN
            LPROB (I)= R_LPROB(R_LDCPTS_AFTER_EL - I + 1,CURRENT)
            LODDUR(I)= R_LODDUR(R_LDCPTS_AFTER_EL - I + 1,CURRENT)
            TRANS_LPROB(I) = R_LPROB(R_LDCPTS_AFTER_EL - I + 1,NEXT)
            TRANS_LODDUR(I) = &
                               R_LODDUR(R_LDCPTS_AFTER_EL - I + 1,NEXT)
         ELSE
            LPROB (I)= R_LPROB(I,CURRENT)
            LODDUR(I)= R_LODDUR(I,CURRENT)
            TRANS_LPROB(I) = R_LPROB(I,NEXT)
            TRANS_LODDUR(I) = R_LODDUR(I,NEXT)
         ENDIF
      end do
!
      PEAK_AFTER_EL = LODDUR(R_LDCPTS_AFTER_EL) ! THIS IS A TEST.

      IF(R_ISEAS == 1) THEN
         TOTAL_DEMAND_AFTER_EL = 0.
         TOTAL_R8_ENERGY = 0.
         TOTAL_I4_ENERGY = 0
      ENDIF

      IF(ALLOCATED(SORT_POS)) DEALLOCATE(SORT_POS)
      ALLOCATE(SORT_POS(LOAD_HOURS_IN_PERIOD))

! TESTING. 7/2/98. GAT.
      VOID_LOGICAL = GET_TRANS_HOUR_DISTRIBUTION(R_TRANS_GROUP, &
                                                      SORT_POS)
      LPROB(R_LDCPTS_AFTER_EL+1) = 0.0
      LODDUR(R_LDCPTS_AFTER_EL+1) = LODDUR(R_LDCPTS_AFTER_EL)
      EL_POS = 2
      LAST_EL_POS = 1
      TOTAL_R8_ENERGY = 0.
      CALCULATED_PEAK = 0.
      CALCULATED_BASE = 9999999.

      EL_POS = 2
      LAST_EL_POS = 1
      TRANS_POS = 2
      LAST_TRANS_POS = 1
      TOTAL_R8_ENERGY = 0.
      CALCULATED_PEAK = 0.
      CALCULATED_BASE = 9999999.
      DO HR = 1, LOAD_HOURS_IN_PERIOD
         HOUR_IN_THE_SEASON = SORT_POS(HR)
         IF(ABS(TRANS_EL_GENERATION) <= 0.0001) THEN

         ELSE
            CURRENT_PROB = 1. - dble(HR-1)/dble(LOAD_HOURS_IN_PERIOD)
            DOWHILE(CURRENT_PROB <= LPROB(EL_POS))
               EL_POS = EL_POS + 1
               LAST_EL_POS = EL_POS - 1
            ENDDO
            IF(LPROB(EL_POS) - LPROB(EL_POS+1) < .000001 .AND. &
                                               LPROB(EL_POS) > 0.) THEN
               EL_POS = EL_POS + 1
            ENDIF
!  Greg, the above has a problem when three or more LPROBs are the same
!  value.  This happened with the WSCC area 12/167/98
            if(abs(LPROB(LAST_EL_POS)-LPROB(EL_POS)) > .000001) then
               temp_ratio = (LPROB(LAST_EL_POS)-CURRENT_PROB)/ &
                             (LPROB(LAST_EL_POS)-LPROB(EL_POS))
            endif
            TEMP_LOAD_AFTER_EL = &
                        LODDUR(LAST_EL_POS) + &
                    (LODDUR(EL_POS) - LODDUR(LAST_EL_POS)) * temp_ratio
            DOWHILE(CURRENT_PROB <= TRANS_LPROB(TRANS_POS))
               TRANS_POS = TRANS_POS + 1
               LAST_TRANS_POS = TRANS_POS - 1
            ENDDO
            IF(TRANS_POS < TRANS_LOAD_POINTS) THEN
               IF(TRANS_LPROB(TRANS_POS) - &
                  TRANS_LPROB(TRANS_POS+1) < .000001 .AND. &
                                      TRANS_LPROB(TRANS_POS) > 0.) THEN
                  TRANS_POS = TRANS_POS + 1
               ENDIF
            ENDIF
!  Greg, the above has a problem when three or more LPROBs are the same
!  value.  This happened with the WSCC area 12/167/98
         if(abs(TRANS_LPROB(LAST_TRANS_POS)-TRANS_LPROB(TRANS_POS)) > &
                                                          .000001) then
             temp_ratio = (TRANS_LPROB(LAST_TRANS_POS)-CURRENT_PROB)/ &
                          (TRANS_LPROB(LAST_TRANS_POS) - &
                                                TRANS_LPROB(TRANS_POS))
            endif
            TEMP_LOAD_BEFORE_EL = &
                        TRANS_LODDUR(LAST_TRANS_POS) + &
                     (TRANS_LODDUR(TRANS_POS) - &
                             TRANS_LODDUR(LAST_TRANS_POS)) * temp_ratio
            HOURLY_HYDRO_EN(HOUR_IN_THE_SEASON,R_TRANS_GROUP,P_OR_G) = &
              HOURLY_HYDRO_EN(HOUR_IN_THE_SEASON,R_TRANS_GROUP,P_OR_G) + &
                               TEMP_LOAD_BEFORE_EL - TEMP_LOAD_AFTER_EL

            IF( ABS(HOURLY_HYDRO_EN(HOUR_IN_THE_SEASON, &
                                    R_TRANS_GROUP,P_OR_G)) < .001) THEN
               TEMP_LOAD_BEFORE_EL = TEMP_LOAD_AFTER_EL
              HOURLY_HYDRO_EN(HOUR_IN_THE_SEASON,R_TRANS_GROUP,P_OR_G) = &
              HOURLY_HYDRO_EN(HOUR_IN_THE_SEASON,R_TRANS_GROUP,P_OR_G) + &
                               TEMP_LOAD_BEFORE_EL - TEMP_LOAD_AFTER_EL
            ELSE
               LOAD_ADJUSTER = LOAD_ADJUSTER
            ENDIF

         ENDIF

         CALCULATED_PEAK = MAX(CALCULATED_PEAK, &
                 HOURLY_HYDRO_EN(HOUR_IN_THE_SEASON,R_TRANS_GROUP,P_OR_G))
         CALCULATED_BASE = MIN(CALCULATED_BASE, &
                 HOURLY_HYDRO_EN(HOUR_IN_THE_SEASON,R_TRANS_GROUP,P_OR_G))
         TOTAL_R8_ENERGY = TOTAL_R8_ENERGY + &
                  HOURLY_HYDRO_EN(HOUR_IN_THE_SEASON,R_TRANS_GROUP,P_OR_G)
      ENDDO
      IF(TOTAL_R8_ENERGY > 0.00001) THEN
         LOAD_ADJUSTER = TRANS_EL_GENERATION/TOTAL_R8_ENERGY
      ELSE
         LOAD_ADJUSTER = 1.0
      ENDIF
      TEST_MIN =        TRANS_EL_GENERATION  - &
                             CALCULATED_BASE*dble(LOAD_HOURS_IN_PERIOD)
      AVE_GENERATION =      TRANS_EL_GENERATION /LOAD_HOURS_IN_PERIOD

      TOTAL_DEMAND_AFTER_EL = TOTAL_DEMAND_AFTER_EL + DEMAND_AFTER_EL
      DEALLOCATE(SORT_POS)
      LDC_2_HOURS = LOAD_ADJUSTER
!      copy values to caller's REAL*4 variables from local real*8
      TRANS_EL_CAPACITY4=sngl(TRANS_EL_CAPACITY)
      PEAK_AFTER_EL4    =sngl(PEAK_AFTER_EL)

      RETURN
! CALLED FROM INSIDE PRO_COST.FOR

      ENTRY INIT_MONTHLY_LDC_2_HOURS(LOAD_HOURS_IN_PERIOD)

        IF(ALLOCATED(HOURLY_HYDRO_EN)) then
            DEALLOCATE(HOURLY_HYDRO_EN)
        end if
        
         MAX_TRANS_GROUP_NUMBER = GET_MAX_TRANS_GROUP_NUMBER()
         ALLOCATE(HOURLY_HYDRO_EN(LOAD_HOURS_IN_PERIOD, &
                                      MAX(1,MAX_TRANS_GROUP_NUMBER),2))
         HOURLY_HYDRO_EN = REAL8_ZERO
         INIT_MONTHLY_LDC_2_HOURS = 1
      RETURN




      END function LDC_2_HOURS
      
      function store_gen_by_hour(r_hour_in_month, r_trans_group)
        implicit none
        real (kind=4) ::  store_gen_by_hour
        integer (kind=2) :: r_trans_group, r_hour_in_month
        

        if(abs(hourly_hydro_EN(r_hour_in_month,r_trans_group,2))>.01) then
            store_gen_by_hour = 0.0
         else
            store_gen_by_hour=HOURLY_HYDRO_EN(r_hour_in_month, &
                r_trans_group,1)
         endif


     end function store_gen_by_hour
end module enrglimit

