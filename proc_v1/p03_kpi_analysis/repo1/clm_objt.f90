
!     clm_objt.for
!     Created: 11/6/02 2:59:27 PM
!     Author : msg
!     Last change: MSG 4/10/2012 12:53:46 PM

      RECURSIVE SUBROUTINE MAINT_SCHEDULER(   &
        NUNITS,   &
        N_ACTIVE_UNITS,   &
        MAINT_DIVISIBLE,   &
        SCHEDULE_RESOLUTION,   &
        UNIT_CAPACITY_MW,   &
        MAINT_REQMT_WK,   &
        MAINT_SCHEDULED_FRACT,   &
        I_UNIT,& 
        SCHEDULE_FO,   &
        DETAILED_MAINT,   &
        MAX_FOPM_PARM,   &
        MONTHLY_MAINTENANCE_PEAKS)
        

!     Routine to schedule maintenance with the goal of levelizing the
!     reserve capacity, i.e., by treating plants without derating for
!     their operating forced-outage rates.

      use program_state
      use globecom
      use spindriftlib
      use prod_arrays_dimensions
      use mod_base_year
      use rollscom
      use week_interp
      use week_interp_i2
	  use clm_objt_pay
      use end_routine
      use clm_decs
      use debugtrace
      implicit none
!     Routine to schedule maintenance with the goal of levelizing the
!     reserve capacity, i.e., by treating plants without derating for
!     their operating forced-outage rates.
      ! 'D'/'W'/'M' for day, week, month
      integer :: file_trace_ycf=0
      integer :: file_trace_ms=0 ! main_scheduler, this routine
      CHARACTER (len=1) ::  SCHEDULE_RESOLUTION 
	  character (len=20) :: CLA_RETURN_UNITNM
      LOGICAL (kind=1) ::   &
        SCHEDULE_FO,DETAILED_MAINT,SO_THISMO_1,SO_THISMO_2,& 
        & !  i.e., period may be split in two
        RANGE_COVERS,MAINT_DIVISIBLE(*), & 
        & ! Lib USE_LIB_MS(*),OWR_LIB_MS(*),
        OptimizeAnew,& 
        DA_AVAIL(0:31),B0=.FALSE. ,B1=.TRUE. ,UNIT_FO_ONCE(:,:),   &
        TEMP_L,CM_MAINT_NOT_OPEN=.TRUE. ,   &
        YES_MAINTENANCE_REPORT=.FALSE. ,MAINTENANCE_REPORT

      LOGICAL (kind=4) ::  FileOpen
      INTEGER (kind=2) ::   &
        NUNITS,N_ACTIVE_UNITS,NU_LOCAL,NA_LOCAL,ISC,IYR,   &
        N_AVAIL_SEGS,AV_DAYS_SEG(2),I_UNIT(*),   &
        I,J,K,K2,K3,J0,J1,J2,J3,   &
        JPR,JCM,IFO,IPD,MON1,MON2,THIS_WIDTH,NWKSX7,   &
        OPT_PAIRJ0,OPT_PAIRJ1,OPT_NWKSX7,MAINT_PRESCH_DA,DAYS_RES,   &
        DAY0,DAY1,DAY2,DAY3,DAY4,PRIOR_DAY2,INF_DAY1,   &
        OPTIMAL_DAY1,OPT_RANGE_DUR1,MAX_FOPM_PARM,   &
        MAX_PD_PER_MONTH=1 ,   &
        ORDER_DEC(:),OPT_RANGE_OFS(:),OPT_RANGE_DUR(:),ODUR1,ODUR2,   &
        LENGMO_DA(52),CUMUL_MO_DA(0:52),NEOSO(12),NEOFO(12),NEOPD(12),   &
        UNIT_OUT_SCHD(:,:),UNIT_OUT_FORC(:,:),UNIT_OUT_DRAT(:,:),   &
        FO_BEG_HR(:,:,:),FO_END_HR(:,:,:),   &
        PD_BEG_HR(:,:,:),PD_END_HR(:,:,:),   &
        CM_DUR_DA,CM_DUR_HR,UNSCH_DA,AVAIL_DA,FO_DUR_HR,SLACK_HR,   &
        FO_BEG_DAY1,FO_END_DAY2,NPRE_SO,IPRE_SO,HR1, & !  PlotModulus,
        LENGCM_DA(12)/31,28,31,30,31,30,31,31,30,31,30,31/,   &
!                        in MMDD format; year is unknown herein
        SO_BEG_MODA(:,:),SO_END_MODA(:,:)
        
        integer (kind=2) :: BEG_O_DATE=0, &
        END_O_DATE=0,CMUnit,   &
        GET_ANNUAL_OUTAGE_NUM,MMDD_AT,   &
        R_YEAR_OR_SCEN
        integer (kind=2) :: GET_ANNUAL_OUTAGE_DATES ! External
      integer (kind=2) :: ipso
      INTEGER (kind=4) ::  I4ST,   &
        CM_MAINT_REC,   &
        CMURec,SeedRan32,SeedRan321!Lib,LibUnit

      REAL (kind=4) ::  & !  first dimension of MSF(NUNITS,13) below is critical:
        MAINT_REQMT_WK(NUNITS),MAINT_SCHEDULED_FRACT(NUNITS,13), & !  E.S.O.R.
        UNIT_CAPACITY_MW(NUNITS),PEAK_LOAD_MW(52),UNIT_MSF(12),   &
        Z1,Z2,PAIR_SUM,COMPLEM_SUM,INF_PAIRSUM,CALWEEK_DA, & !  DA_RQ_REM,
        MAINT_RQ_DA_DEC(:),MAINT_WEIGHT(:),HrsToState(:,:), & !  ,SO_CFRA(:)
        MONTHLY_MAINTENANCE_PEAKS(12),UNIT_OUT_FRAC(:,:,:),NFO_SEG(2)

      real (kind=8) ::  MixRand32,PRN8

      ALLOCATABLE ::   &
        ORDER_DEC,MAINT_RQ_DA_DEC,MAINT_WEIGHT,HrsToState, & !  SO_CFRA,
        OPT_RANGE_OFS,OPT_RANGE_DUR,   &
        SO_BEG_MODA,SO_END_MODA,   &
        FO_BEG_HR,FO_END_HR,   &
        PD_BEG_HR,PD_END_HR,   &
        UNIT_OUT_SCHD,UNIT_OUT_FORC,UNIT_OUT_DRAT,UNIT_FO_ONCE,   &
        UNIT_OUT_FRAC

      SAVE   &
        I,ISC,IYR,CMUnit,CMURec,LENGCM_DA,   &
        ORDER_DEC,MAINT_RQ_DA_DEC,MAINT_WEIGHT,HrsToState, & !  SO_CFRA,
        OPT_RANGE_OFS,OPT_RANGE_DUR,   &
        SO_BEG_MODA,SO_END_MODA,   &
        FO_BEG_HR,FO_END_HR,   &
        PD_BEG_HR,PD_END_HR,   &
        UNIT_OUT_SCHD,UNIT_OUT_FORC,UNIT_OUT_DRAT,UNIT_FO_ONCE,   &
        NEOSO,NEOFO,NEOPD,NU_LOCAL,NA_LOCAL,B0,B1,   &
        UNIT_OUT_FRAC,CM_MAINT_NOT_OPEN

!    declarations for GET_MONTHS_EQ_AV
      INTEGER (kind=2) ::  GMEA_I,GMEA_MO
      REAL (kind=4) ::  GMEA_PCT_EA

!    declarations for WRITE_YEARS_CMD_FILE
      INTEGER (kind=2) ::  cYear,   &
        CM_MAINT_HEADER,CM_IMP_DO_COUNT
	  ! Routine THAT GETS A UNIQUE ID FOR THE UNIT
      CHARACTER (len=5) ::  CL_UNIQUE_RPT_STR 
      CHARACTER (len=20) ::  UnitName
      CHARACTER (len=25) ::  UnitNameAndNumber
      REAL (kind=4) ::  OUTAGE_FRAC,SO_FRACTION

!    declarations for SW_MAINT_DERATING
      LOGICAL (kind=1) ::  SMD_ON

!    declarations for INCREMENT_EVENTS_OF_SO
      INTEGER (kind=2) ::  IESJPR,IESMON

!    declarations for COUNT_EVENTS_OF_SCHED_OUTAGE
      INTEGER (kind=2) ::  CEOSO_EVENTS,CEOSO_MO ! ,CEOSO_UNIT

!    declarations for COUNT_EVENTS_OF_FORCED_OUTAGE
      INTEGER (kind=2) ::  CEOFO_EVENTS,CEOFO_MO ! ,CEOFO_UNIT

!    declarations for ENTRY COUNT_EVENTS_OF_PARTIAL_OUTAGE
      INTEGER (kind=2) ::  CEOPD_EVENTS,CEOPD_MO

!    declarations for GET_FO_HRS_RANGE
      INTEGER (kind=2) ::  GFHR_INDEX,GFHR_ORG_UNIT,GFHR_MO
      

!    declarations for GET_PD_HRS_RANGE
      INTEGER (kind=2) ::  GPHR_INDEX,GPHR_ORG_UNIT,GPHR_MO,   &
        BEG_HR_PD(MAX_FO_PER_MONTH+1),END_HR_PD(MAX_FO_PER_MONTH+1)

!    declarations for REVISE_FO_SEED
      REAL (kind=4) ::  R_FO_SEED
      INTEGER (kind=2) ::  FO_SEED

      SAVE FO_SEED
      DATA FO_SEED /0/  ! sintax for init at comp time 

!    declarations for GET_FO_HRS1
      INTEGER (kind=2) ::  GFH1_I,GFH1_K2
      REAL (kind=4) ::  EAVAIL1(12)

!    declarations for GET_FO_HRS2
      INTEGER (kind=2) ::  GFH2_I,GFH2_K2,   &
        BEG_IN_MO(12),END_IN_MO(12),JULIAN_DATE,HR_IN_DA
      REAL (kind=4) ::  FO_FREQ,FO_DURA,HR_IN_YR,EAVAIL2(12)

!    declarations for GET_FO_HRS_BLOCK arguments
      REAL (kind=4) ::  BAVAIL(12)
      INTEGER (kind=2) ::  GFHB_I,BEG_FO_HR_BK(12),END_FO_HR_BK(12)
      LOGICAL (kind=1) ::  BK_IS_AVAIL(744)

!    declarations for GET_FO_HRS_TLINE arguments
      INTEGER (kind=4) ::  GFHT_SEQN_HOLD,GFHT_SEQN_USED=0
      INTEGER (kind=2) ::  GFHT_K2, & !  assume MAX_TL_FO_PER_MONTH is 1
        R_MONTH,GFHT_LENGCM_DA,BEG_FO_HR_TL,END_FO_HR_TL
      REAL (kind=4) ::  TL_FOR
      SAVE GFHT_SEQN_HOLD,GFHT_SEQN_USED

!    declarations for GET_FO_HRS_BLOCK local vars unused elsewhere
      INTEGER (kind=2) ::  KHR,HR2,BA_HR1,BA_DUR_HR

!    declarations for GET_PD_HRS1
      INTEGER (kind=2) ::  GPH1_I,GPH1_MPDPM,PD_DUR_HR,   &
        BEG_PD_HR1(12), & !  hour [1,744] in each month
        END_PD_HR1(12)  ! hour [2,745] in each month
      REAL (kind=4) ::  DAVAIL1

!    declarations for GET_MAINT_DATE_RANGE
      INTEGER (kind=2) ::  GMDR_INDEX,GMDR_ORG_UNIT,GMDR_MO,   &
        GBEG_DD(2),GEND_DD(2),GMDR_JPAIR
        

      IYR=globecom_YEAR ! make accessible to entries; YEAR is on [1,30]
      ISC=gc_end_point
      if(file_trace_ms==0) then
        file_trace_ms=open_trace("maint_scheduler.trace", rq_ms)
      end if
      
      IF(MAX_FO_PER_MONTH<1) STOP 'call INIT_MAINT_SCHEDULE first'
      if(file_trace_ms/=BAD_TRACE_HANDLE) then
         write(file_trace_ms, *) &
            " NUNITS ", NUNITS ,   &
          "N_ACTIVE_UNITS ", N_ACTIVE_UNITS,   &
         " SCHEDULE_RESOLUTION ", SCHEDULE_RESOLUTION, &
         " UNIT_CAPACITY_MW ", UNIT_CAPACITY_MW,   &
         " MAINT_REQMT_WK ", MAINT_REQMT_WK(1:nunits),   &
         " SCHEDULE_FO ", SCHEDULE_FO,   &
         " DETAILED_MAINT ", DETAILED_MAINT,   &
         " MAX_FOPM_PARM ", MAX_FOPM_PARM, &
         " I_UNIT ", I_UNIT(:N_ACTIVE_UNITS)
      end if
      

      ! indicating that GET_MAINT_DATE_RANGE() is not yet callable
      NU_LOCAL=0 
      NA_LOCAL=0
      IF (SCHEDULE_RESOLUTION=='D') THEN ! daily resolution
        LENGYR_MO=12 ! treat year as composed of 12 calendar months ...
        CALWEEK_DA=7. ! each calendar week covers 7 calendar days
        DO JMO=1,LENGYR_MO

          LENGMO_DA(JMO)=LENGCM_DA(JMO) ! ... of various length
          PEAK_LOAD_MW(JMO)=MONTHLY_MAINTENANCE_PEAKS(JMO)
        END DO
      ELSE IF(SCHEDULE_RESOLUTION=='W') THEN ! weekly resolution
        LENGYR_MO=52 ! treat year as composed of 52 "months" ...
        CALWEEK_DA=1. ! each calendar week covers one "day"
        J=0 ! index of calendar month
        I=0 ! days remaining unallocated in calendar month J
        call write_trace_int2(file_trace_ms, "1. Shared I", I)
       DO JMO=1,LENGYR_MO ! distribute monthly peaks across 52 "months"
          LENGMO_DA(JMO)=1 ! ... of 1 "day" (calendar week) each
          IF(I<=0) THEN ! advance calendar month to 1,2,...,12

            J=J+1 ! at the end of the year, 1 calendar day will remain:
            I=I+LENGCM_DA(J) ! ignore the remainder of 1 = 365-52*7
        call write_trace_int2(file_trace_ms, "2. Shared I", I)
            FIRST_WEEK(J)=0 ! indicating not yet assigned
          END IF
          IF(I>=7) THEN ! entire week indexed JMO lies within month J
            Z1=1.

            PEAK_LOAD_MW(JMO)=MONTHLY_MAINTENANCE_PEAKS(J)
         ELSE ! 0<I<7, so week indexed JMO spans calendar months J & J+1
            Z1=FLOAT(I)/7.

            PEAK_LOAD_MW(JMO)=MONTHLY_MAINTENANCE_PEAKS(J  )*    Z1+   &
                              MONTHLY_MAINTENANCE_PEAKS(J+1)*(1.-Z1)
          END IF
!       derivable: (1.-Z1)= interpolation weight for calendar month J+1
          INTERP_WT(JMO)=Z1 ! interpolation weight for calendar month J
          INTERP_CM(JMO)=J ! the earlier in the pair of adjacent months
         IF(FIRST_WEEK(J)==0) FIRST_WEEK(J)=JMO ! needed for allocation
          I=I-7
          call write_trace_int2(file_trace_ms, "3. Shared I", I)
        END DO
      ELSE ! (SCHEDULE_RESOLUTION=='M') monthly resolution (inferred)

        LENGYR_MO=12 ! treat year as composed of 12 months ...
! CALWEEK_DA=7.*12./364. ! each calendar week covers 1/4.333 "day"
! One may argue that the 364 above should be 365, but I believe
! that when the user specifies, e.g., 52.00 weeks of maintenance,
! he imagines that it covers the entire year of 365 days, as this
! will:  "days"=52*(7*12/364)=12*(52*7/364)=12 (exactly), implying
! that the entire year of 12 "months" will be on maintenance.  If
! one thinks the user imagines the year contains 52.14 weeks, then
! one should change the denominator of CALWEEK_DA above to 365.
        CALWEEK_DA=7.*12./365. ! if a year contains 52.14 weeks
      LENGPM_DA=365./12.! each pseudo_month covers 365/12 calendar days
        DO JMO=1,LENGYR_MO
          LENGMO_DA(JMO)=1 ! ... of 1 "day" each
          PEAK_LOAD_MW(JMO)=MONTHLY_MAINTENANCE_PEAKS(JMO)
        END DO
      END IF

      CUMUL_CM_DA(0)=0 ! accumulate days in calendar months
      DO JCM=1,24
        CUMUL_CM_DA(JCM)=CUMUL_CM_DA(JCM-1)+LENGCM_DA(1+MOD(JCM-1,12))
      END DO

      CUMUL_MO_DA(0)=0 ! accumulate days in pseudo-months
      DO JMO=1,LENGYR_MO
        CUMUL_MO_DA(JMO)=CUMUL_MO_DA(JMO-1)+LENGMO_DA(JMO)
      END DO

      LENGYR_DA=CUMUL_MO_DA(LENGYR_MO)
      CALL ACCUMULATE_LOADS(PEAK_LOAD_MW,LENGMO_DA)

      IF(ALLOCATED(MAINT_RQ_DA_DEC)) then
		DEALLOCATE(MAINT_RQ_DA_DEC,   &
        MAINT_WEIGHT,ORDER_DEC,OPT_RANGE_OFS,OPT_RANGE_DUR)
	  end if

      ALLOCATE(MAINT_RQ_DA_DEC(N_ACTIVE_UNITS),STAT=I4ST)
      CALL CHECK_ALLOC_STATUS(I4ST,'MRDD',B0)

      ALLOCATE(MAINT_WEIGHT(N_ACTIVE_UNITS),STAT=I4ST)
      CALL CHECK_ALLOC_STATUS(I4ST,'MTWD',B0)

      ALLOCATE(ORDER_DEC(N_ACTIVE_UNITS),STAT=I4ST)
      CALL CHECK_ALLOC_STATUS(I4ST,'ODEC',B0)

      ALLOCATE(OPT_RANGE_OFS(N_ACTIVE_UNITS),STAT=I4ST)
      CALL CHECK_ALLOC_STATUS(I4ST,'OROS',B0)

      ALLOCATE(OPT_RANGE_DUR(N_ACTIVE_UNITS),STAT=I4ST)
      CALL CHECK_ALLOC_STATUS(I4ST,'ORDU',B0)



      call write_trace_int2(file_trace_ms, "N_ACTIVE_UNITS", &
        N_ACTIVE_UNITS)
        
      DO K2=1,N_ACTIVE_UNITS
        call write_trace_int2(file_trace_ms, "K2", K2)
        I=I_UNIT(K2) ! active items in input arrays are contiguous
        call write_trace_int2(file_trace_ms, "4. Shared I", I)
        call write_trace_int2(file_trace_ms, "I", I)
        PRIOR_DAY2=1
        DAY2=1
        DAY0=1
        INF_DAY1=0 ! indicating not yet assigned
        THIS_WIDTH=0
!       sum across months of this unit's 
!       prescheduled days
        MAINT_PRESCH_DA=0
        DO J=1,12 ! assume presch. maintenance was intended for whole days
          Z2=MAINT_SCHEDULED_FRACT(K2,J)
          UNIT_OUT_FRAC(J,1,I)=Z2 ! capture incoming MSF for .CMD-file
          Z1=FLOAT(LENGCM_DA(J))*Z2
          K=INT(Z1)
          IF(FLOAT(K)<Z1) then
			K=K+1
		  end if
		  
          IF(K>0) THEN ! assume maintenance begins on 1st month
            MAINT_PRESCH_DA=MAINT_PRESCH_DA+K ! in calendar days
            Z1=FLOAT(K) ! default valid for daily resolution
            IF(LENGYR_MO==12) THEN
              JMO=J 
              IF(SCHEDULE_RESOLUTION=='M') THEN
                Z1=Z1/FLOAT(LENGCM_DA(J)) ! # of "months" presch.

              END IF
            ELSE ! place the starting day in the proper week-long "month"
              JMO=FIRST_WEEK(J) 
            
              IF(INTERP_WT(JMO)<0.5) then
                JMO=JMO+1 !debugstop
              end if
              Z1=Z1/7.

            END IF

            K=INT(Z1)
            IF(FLOAT(K)<Z1) K=K+1


! Checked values here and good.
            DAY1=CUMUL_MO_DA(JMO-1)+1
            IF(INF_DAY1==0) INF_DAY1=DAY1
            CALL INCORPORATE_MAINT(DAY1,K,Z1,UNIT_CAPACITY_MW(K2))

            PRIOR_DAY2=DAY2 ! value saved from prior month's p.m. allocation
            DAY2=DAY1+K ! 1 + ending day  from this  month's p.m. allocation

!          retain the longest contiguous segment available for optimization

            IF(THIS_WIDTH<DAY1-PRIOR_DAY2) THEN
              THIS_WIDTH=DAY1-PRIOR_DAY2
              DAY0=PRIOR_DAY2
            END IF

          END IF ! end allocation of cal. months' presch. mnt. to "days"


        END DO ! J

        IF(MAINT_PRESCH_DA==0) THEN ! default above for DAY0 is valid
          THIS_WIDTH=LENGYR_DA
        ELSE ! some presch. mnt. has been allocated above; INF_DAY1>=1

! Verified
          J=LENGYR_DA+INF_DAY1-DAY2 ! on [0,LENGYR_DA-1] since DAY2>INF_DAY1

          IF(THIS_WIDTH<J) THEN ! optimize over year-end wraparound period


            THIS_WIDTH=J
            DAY0=DAY2
          END IF
        END IF
!      store the start-day and length of the optimization range
        OPT_RANGE_OFS(K2)=DAY0-1     ! initial day offset (0-based)
        OPT_RANGE_DUR(K2)=THIS_WIDTH ! duration available


!      convert prescheduled mnt. in calendar days to "days"
        Z2=FLOAT(MAINT_PRESCH_DA) ! consider instead Z2 as the sum of Z1 above


        IF    (SCHEDULE_RESOLUTION=='W') THEN
          Z2=Z2/ 7.
        ELSEIF(SCHEDULE_RESOLUTION=='M') THEN
          Z2=Z2/(7./CALWEEK_DA)
        END IF


!      this causes scheduled to match required mnt.; GAT likes the results:
        Z1=MAINT_REQMT_WK(K2)*CALWEEK_DA-Z2 ! requirement expressed in "days"
        IF(Z1 < 0.00001) then
			Z1 = 0.0
		end if
!      round-up here may overallocate maintenance and upset the order so
!      much that GAT was unhappy with results on 20010423:


        MAINT_WEIGHT(K2)=Z1*UNIT_CAPACITY_MW(K2) ! in MW-days
        MAINT_RQ_DA_DEC(K2)=Z1 ! before sorting
        ORDER_DEC(K2)=K2 ! before sorting
      END DO ! K2

!    the only way to preschedule maintenance is GET_ANNUAL_OUTAGE_DATES
!    Above was disabled 20010418 per GAT's advice that calls to
!    GET_ANNUAL_OUTAGE_DATES do not cover all prescheduled maintenance,
!    some of which is implicitly in MAINT_SCHEDULED_FRACT when called
!
!    augment MAINT_SCHEDULED_FRACT by counted prescheduled outages' effects,
!    but NOT after 20021212 per GAT request (and if DET_MNT after 20030120)

      NPRE_SO=GET_ANNUAL_OUTAGE_NUM()
      
      DO IPRE_SO=1,NPRE_SO

        ipso=ipre_so 
        J=GET_ANNUAL_OUTAGE_DATES(ipso,   &
          BEG_O_DATE,END_O_DATE,I) ! returns I on [1,NUNITS]

        ! J= 352/352
        ! BEG_O_DATE= 1010/1010
        ! END_O_DATE= 1107/1107
        ! I= /352
 ! after 20010911, we treat above results as pre-empting MAINT_REQMT_WK

        IF((BEG_O_DATE==0).OR.(END_O_DATE==0)) then
			CYCLE
		end if
        JPR=1 ! allow for up to two pairs of BEG/END dates per unit
        ! first pair of BEG/END was previously filled
        IF(SO_BEG_MODA(JPR,I)>0) then
			JPR=2 
		end if
        SO_BEG_MODA(JPR,I)=BEG_O_DATE
        SO_END_MODA(JPR,I)=END_O_DATE

        DO K2=1,N_ACTIVE_UNITS
          IF(I_UNIT(K2)==I) EXIT ! with contiguous index of plant I
        END DO
        IF(K2>N_ACTIVE_UNITS) THEN
          CYCLE 

        end if
        MON1=BEG_O_DATE/100
        MON2=END_O_DATE/100
        IF(MON2<MON1) then
			MON2=MON2+12 ! for EoY wrap
		end if

        DO J=MON1,MON2
          JCM=1+MOD(J-1,12)

          Z2=FLOAT(LENGCM_DA(JCM))

          CALL GET_DAY_RANGE_THISMO(JCM,DAY1,DAY2,LENGCM_DA(JCM),   &
            BEG_O_DATE,END_O_DATE)
!        augment count of mnt.-outage events each month (not evident in MSF)

!
!01/17/03 GREG'S ATTEMPT TO SUPPRESS THE TRANSACT OUTAGES WHILE STILL
!         ALLOWING THE AUTOMATIC MAINTENANCE OUTAGES.

          K=DAY2-DAY1+1

          Z1=FLOAT(K) ! after 20010911 M_S_F is limited to 1 ...

!         incorporate days of prescheduled maintenance in LOAD_ACCUM
          DAY3=CUMUL_MO_DA(JCM-1)+DAY1 ! convert from MMDD to Julian day
          CALL INCORPORATE_MAINT(DAY3,K,Z1,UNIT_CAPACITY_MW(K2))

        ! allow for the possibility of more than one outage per unit per month
          Z2=MIN(1.0,MAINT_SCHEDULED_FRACT(K2,JCM)+Z1/Z2) ! running accum
          ! net increase due to TransAct's ANNUAL_OUTAGEs
          UNIT_OUT_FRAC(JCM,2,I)=Z2-UNIT_OUT_FRAC(JCM,1,I) 



        END DO ! J

!      consider the last day of the year as contiguous with the first

        DAY1=CUMUL_MO_DA(MON1-1)+MOD(BEG_O_DATE,100)
        DAY2=CUMUL_MO_DA(MON2-1)+MOD(END_O_DATE,100)+1

      ! optimize over year-end wraparound period
        J1=LENGYR_DA+DAY1-DAY2 ! on [0,LENGYR_DA-1] since DAY2>DAY1

        DAY1=DAY2-1 ! convert to 0-based offset for comparison below
      ! store/revise the start-day and length of the optimization range
        IF(JPR==1) THEN 
          ! store the offset and duration of the period above


          DAY0=DAY1
          J0=J1
        ELSE ! JPR==2; retain the longer of the 2 optimization periods
          DAY2=DAY1
          J2=J1

          DAY1=OPT_RANGE_OFS(K2) ! stored previously at JPR==1
          J1=OPT_RANGE_DUR(K2)
          IF(DAY1>DAY2) THEN
!           swap so that DAY1<DAY2
            DAY3=DAY1
            DAY1=DAY2
            DAY2=DAY3
!           swap so that DAY1 owns J1, DAY2 owns J2
            J3=J1
            J1=J2
            J2=J3
          END IF

          J1=LENGYR_DA-J1 ! length of 1st presched. outage
          J2=LENGYR_DA-J2 ! length of 2nd presched. outage
          ODUR1=DAY2-(DAY1+J1) ! period between 1st & 2nd outages
          ODUR2=LENGYR_DA+DAY1-(DAY2+J2) ! EoY-wrap period
          IF(ODUR1>ODUR2) THEN ! optimize on interval between presched.
            DAY0=DAY1+J1
            J0=ODUR1
          ELSE                 ! optimize on EoY-wrap interval
            DAY0=DAY2+J2
            J0=ODUR2
          END IF
        END IF

        OPT_RANGE_OFS(K2)=DAY0 ! initial day offset (0-based)
        OPT_RANGE_DUR(K2)=J0   ! duration available
      ! incorporating presched. in MAINT_SCHEDULED_FRACT & LOAD_ACCUM;
      END DO 
      ! assume MAINT_REQMT_WK need not be decremented above, since it 
      ! defines
      ! the amount to be optimally scheduled below

      CALL IRSORT1(N_ACTIVE_UNITS,MAINT_WEIGHT,ORDER_DEC,B0) ! into decr. order

    ! optimize the placement of the balance of mnt. requirement-
    !prescheduled

      IF     (SCHEDULE_RESOLUTION=='D') THEN
        DAYS_RES=7 ! step through divisible requirement by whole weeks
      ELSE ! (SCHEDULE_RESOLUTION=='M' or 'W') (inferred)
        ! step through divisible requirement at finest poss. resolution
        DAYS_RES=1 
      END IF

      DO K3=1,N_ACTIVE_UNITS ! in order of decreasing mnt. weight
        K2=ORDER_DEC(K3) ! index into the original (unsorted) arrays
        I=I_UNIT(K2) ! active items in input arrays are contiguous
        call write_trace_int2(file_trace_ms, "4. Shared I", I)
        Z1=MAINT_RQ_DA_DEC(K2) ! requirement "days" sorted

          THIS_WIDTH=INT(Z1)
            IF(FLOAT(THIS_WIDTH)<Z1) then
                THIS_WIDTH=THIS_WIDTH+1 ! round upward
            end if
        ! optimize only on units that are not 100% prescheduled
        IF(THIS_WIDTH>0) THEN 
          OptimizeAnew=B1


          IF(OptimizeAnew) THEN

            J=OPTIMAL_DAY1(THIS_WIDTH,OPT_RANGE_OFS(K2),   &
              OPT_RANGE_DUR(K2),INF_PAIRSUM)
            J1=J+THIS_WIDTH-1
            ! default in case required days are not (optimally) split
            OPT_NWKSX7=0 
          END IF

! single pair of begin/end dates is valid if not MAINT_DIVISIBLE()
          ! default valid if there is no prescheduled mnt. for unit I
          JPR=1
          IF(SO_BEG_MODA(JPR,I)>0) JPR=2 ! WRITE(4,'(" 1st B/E_DATEs occupied")')
          SO_BEG_MODA(JPR,I)=MMDD_AT(J ,CUMUL_CM_DA)
          SO_END_MODA(JPR,I)=MMDD_AT(J1,CUMUL_CM_DA) ! allows for EoY wrap
!
          IF(MAINT_DIVISIBLE(K2).AND.(JPR==1).AND.OptimizeAnew) THEN
! determine optimal split of days-required
!    Examination of the debugging results shows that aborting the loop
!      at THIS_WIDTH/2 (or even this rounded up to the next multiple of
!       7 days) would not allow the best local combination to be found;
!     hence, the loop is opened up to the entire range of possibilities

            ! examine all poss. # of whole weeks
            DO NWKSX7=DAYS_RES,THIS_WIDTH,DAYS_RES
              J0=OPTIMAL_DAY1(NWKSX7,OPT_RANGE_OFS(K2),   &
                OPT_RANGE_DUR(K2),PAIR_SUM)
              IF(OPT_RANGE_DUR(K2)<LENGYR_DA) THEN ! p.m. precludes wraparound;
                OPT_RANGE_DUR1=OPT_RANGE_DUR(K2)- & !  the second period ends ...
                  (J0-1+NWKSX7-OPT_RANGE_OFS(K2))   ! ...within OPT_RANGE_DUR(K2)
              ELSE ! allow the complementary period to wraparound without ...
                OPT_RANGE_DUR1=LENGYR_DA-NWKSX7 ! ... overlapping J0 period
              END IF
              J1=OPTIMAL_DAY1(THIS_WIDTH-NWKSX7,J0-1_2+NWKSX7,   &
                OPT_RANGE_DUR1  ,COMPLEM_SUM) ! sum excludes the J0 result


              PAIR_SUM=PAIR_SUM+COMPLEM_SUM
              IF(INF_PAIRSUM>PAIR_SUM) THEN
                INF_PAIRSUM=PAIR_SUM ! retain the min for reference
            OPT_PAIRJ0=J0 ! opt day1 for part having multiple of 7 days
       OPT_PAIRJ1=J1 ! opt day1 for part having remainder of days
                OPT_NWKSX7 =NWKSX7

              END IF
            END DO
          END IF

!         Place Ith unit's maintenance-fractions in contiguous locations
          DO JCM=1,12
            UNIT_MSF(JCM)=MAINT_SCHEDULED_FRACT(K2,JCM)
          END DO
!
          IF(OPT_NWKSX7==0) THEN ! (opt) maintenance was not split for this unit;
!          retain default values assigned above for SO_BEG_MODA & SO_END_MODA
            CALL INCREMENT_EVENTS_OF_SO(int(JPR,2))

            CALL ALLOC_MAINT_BYMO(LENGCM_DA,LENGMO_DA,CUMUL_MO_DA,   &
              J,THIS_WIDTH,Z1,UNIT_MSF)
            CALL INCORPORATE_MAINT(J, THIS_WIDTH,   &
              Z1, UNIT_CAPACITY_MW(K2))
          ELSE
            SO_BEG_MODA(1,I)=MMDD_AT(OPT_PAIRJ0,CUMUL_CM_DA)
            SO_BEG_MODA(2,I)=MMDD_AT(OPT_PAIRJ1,CUMUL_CM_DA)
            SO_END_MODA(1,I)=MMDD_AT( &
             OPT_PAIRJ0+OPT_NWKSX7-1_2,CUMUL_CM_DA) 
            SO_END_MODA(2,I)=MMDD_AT( &
             OPT_PAIRJ1+THIS_WIDTH-OPT_NWKSX7-1_2,CUMUL_CM_DA)
            CALL INCREMENT_EVENTS_OF_SO(INT(1,2))
            CALL INCREMENT_EVENTS_OF_SO(INT(2,2))
            Z2=FLOAT(OPT_NWKSX7)
            CALL ALLOC_MAINT_BYMO(LENGCM_DA,LENGMO_DA,CUMUL_MO_DA,   &
              OPT_PAIRJ0,           OPT_NWKSX7,   Z2,UNIT_MSF)
            CALL ALLOC_MAINT_BYMO(LENGCM_DA,LENGMO_DA,CUMUL_MO_DA,   &
              OPT_PAIRJ1,THIS_WIDTH-OPT_NWKSX7,Z1-Z2,UNIT_MSF)
            CALL INCORPORATE_MAINT(OPT_PAIRJ0,           OPT_NWKSX7,   &
                 Z2,UNIT_CAPACITY_MW(K2))
            CALL INCORPORATE_MAINT(OPT_PAIRJ1,THIS_WIDTH-OPT_NWKSX7,   &
              Z1-Z2,UNIT_CAPACITY_MW(K2))

          END IF

          DO JCM=1,12
          ! AMIN1 below is needed in case of pre-sched. sloppiness
            Z1=MAINT_SCHEDULED_FRACT(K2,JCM) ! prior accum
            Z2=AMIN1(UNIT_MSF(JCM),1.0)      ! running accum
            MAINT_SCHEDULED_FRACT(K2,JCM)=Z2
            ! net increase due to optimized SO
            UNIT_OUT_FRAC(JCM,3,I)=Z2-Z1 
          END DO
        END IF ! re prescheduled maintenance

        ! entries below need SO_BEG_MODA, SO_END_MODA
        IF(SCHEDULE_FO) THEN 
        ! save dates that scheduled maintenance outages begin & end


          JPR=1
          ! assign dates to unscheduled maintenance
          IF(SO_BEG_MODA(JPR,I)==0) THEN 
            ! useful for debugging display only            PRIOR_DAY2=0 
            DO JCM=1,12
              CM_DUR_DA=LENGCM_DA(JCM)
              J=NINT(FLOAT(CM_DUR_DA)*MAINT_SCHEDULED_FRACT(K2,JCM))
              IF(J==0) THEN
                DAY1=0
                DAY2=0
                IF((JCM>1).AND.(PRIOR_DAY2==LENGCM_DA(JCM-1))) THEN
!               ! close a maintenance interval
                  SO_END_MODA(JPR,I)=(JCM-1)*100+PRIOR_DAY2
                 IF(JPR==2) EXIT ! both pair of assigned dates are full
                  JPR=JPR+1
                END IF
              ELSE ! J>0
                IF((JCM>1).AND.(PRIOR_DAY2==LENGCM_DA(JCM-1))) THEN
                  IF(J<CM_DUR_DA) THEN

           ! close a maintenance interval toward beginning of month JCM

                    DAY1=1
                    DAY2=J
                    SO_END_MODA(JPR,I)=JCM*100+DAY2
                 IF(JPR==2) EXIT ! both pair of assigned dates are full
                    JPR=JPR+1
                  ELSE ! continue the multi-month interval
                    DAY1=1
                    DAY2=CM_DUR_DA
                  END IF
                ELSE

              ! open a maintenance interval toward the end of month JCM
             ! allowing for a possible contiguous continuation in JCM+1

                  DAY1=CM_DUR_DA-J+1
                  DAY2=CM_DUR_DA
                  SO_BEG_MODA(JPR,I)=JCM*100+DAY1
                END IF
              END IF

              IF((JPR>2).OR.(SO_END_MODA(2,I)>0)) EXIT ! both pair full
              PRIOR_DAY2=DAY2
            END DO ! JCM

            IF((JPR<=2).AND.(SO_BEG_MODA(JPR,I)>0)   &
                       .AND.(SO_END_MODA(JPR,I)==0)) & !  close pending interval
              SO_END_MODA(JPR,I)=1231


          END IF

          IF((K3==1).AND.(MAX_FO_PER_MONTH>0)) THEN
            IF(ALLOCATED(HrsToState)) DEALLOCATE(HrsToState)
            ALLOCATE(HrsToState(0:1,12*MAX_FO_PER_MONTH),STAT=I4ST)
            CALL CHECK_ALLOC_STATUS(I4ST,'HrTS',B0)
          END IF
        END IF ! SCHEDULE_FO

        ! needed in case of prescheduled
        MAINT_SCHEDULED_FRACT(K2, 13)=0.0 
        DO JCM=1,12 ! augment count of mnt.-outage events each month;

!        after 20021217, done by CALLs to INCREMENT_EVENTS_OF_SO() above
          IF(MAINT_SCHEDULED_FRACT(K2,JCM)>0.0) THEN
            ! 13th column has units of days
            MAINT_SCHEDULED_FRACT(K2, 13)= &
           MAINT_SCHEDULED_FRACT(K2, 13)+ &
           MAINT_SCHEDULED_FRACT(K2,JCM)*FLOAT(LENGCM_DA(JCM))

          END IF
        END DO
      END DO ! K3=1,N_ACTIVE_UNITS
!
!     The order of deallocation is not important; leave the following
!     allocated for use by entries:
!     FO_BEG_HR,FO_END_HR,HrsToState,
!     PD_BEG_HR,PD_END_HR,
!     UNIT_FO_ONCE,UNIT_OUT_SCHD,UNIT_OUT_FORC,UNIT_OUT_DRAT,
!     SO_BEG_MODA,SO_END_MODA


      DEALLOCATE(MAINT_RQ_DA_DEC,STAT=I4ST)
      CALL CHECK_ALLOC_STATUS(I4ST,'MRDD',B1)

      DEALLOCATE(MAINT_WEIGHT,STAT=I4ST)
      CALL CHECK_ALLOC_STATUS(I4ST,'MTWD',B1)

      DEALLOCATE(ORDER_DEC,STAT=I4ST)
      CALL CHECK_ALLOC_STATUS(I4ST,'ODEC',B1)

      DEALLOCATE(OPT_RANGE_OFS,STAT=I4ST)
      CALL CHECK_ALLOC_STATUS(I4ST,'OROS',B1)

      DEALLOCATE(OPT_RANGE_DUR,STAT=I4ST)
      CALL CHECK_ALLOC_STATUS(I4ST,'ORDU',B1)

      NU_LOCAL=NUNITS ! GET_MAINT_DATE_RANGE() is now callable
      NA_LOCAL=N_ACTIVE_UNITS
      RETURN ! SUBROUTINE MAINT_SCHEDULER

      ENTRY GET_MONTHS_EQ_AV(GMEA_I,GMEA_MO,GMEA_PCT_EA)
      ! equiv. avail. in %
      GMEA_PCT_EA=UNIT_OUT_FRAC(GMEA_MO,5,GMEA_I)*100.0 
      RETURN !      invoke with GMEA_MO==13 to return annual average
! note to GAT: above assumes a prior call to ENTRY WRITE_YEARS_CMD_FILE

      ENTRY WRITE_YEARS_CMD_FILE(I_UNIT)
        if(file_trace_ycf==0) then
         file_trace_ycf=open_trace("write_years_cmd_file.trace", rq_ycf)
        end if
        

      if(CM_MAINT_NOT_OPEN) then ! caller must close this file
        CM_MAINT_NOT_OPEN=.FALSE.
        CM_IMP_DO_COUNT=65
        CMURec=0
        CMUnit=CM_MAINT_HEADER(CM_IMP_DO_COUNT,CMURec)
      end if
      cYear=get_BASE_YEAR()+globecom_YEAR
      
      DO K2=1,NA_LOCAL

        UnitName=CLA_RETURN_UNITNM(I)
        
        cur_called_from="clo:1"
        UnitNameAndNumber=UnitName//CL_UNIQUE_RPT_STR(I)
        
        ! EX: UnitNameAndNumber="Air Products GCA:ST1 1572"/"LFG:RPS TX_11       14668"
        DO J=1,12
          OUTAGE_FRAC=0.0
          ! accumulate fraction-of-month on outage across all types
          DO K=1,4 
            OUTAGE_FRAC=OUTAGE_FRAC+UNIT_OUT_FRAC(J,K,I)
            IF(K==3) then
				SO_FRACTION=OUTAGE_FRAC
			end if
          END DO
        ! report EFOR in 4th column:  ratio of FO/(total-SO) times
          IF(SO_FRACTION<0.999) &
            ! normalize to the fraction of time not on SO
           UNIT_OUT_FRAC(J,4,I)=UNIT_OUT_FRAC(J,4,I)/(1.0-SO_FRACTION)
      ! report equivalent availability as ratio of available/total time
          UNIT_OUT_FRAC(J,5,I)=AMAX1(1.0-OUTAGE_FRAC,0.0)
          DO K=1,5 ! accumulate monthly-weighted-average in position 13
            UNIT_OUT_FRAC(13,K,I)=   &
            UNIT_OUT_FRAC(13,K,I)+UNIT_OUT_FRAC(J,K,I)*   &
              FLOAT(LENGCM_DA(J))/365.0
          END DO

        END DO
        if(file_trace_ycf/=BAD_TRACE_HANDLE) then
            write(file_trace_ycf,*) &
            "CMURec ", CMURec, &
          "ISC ", ISC,"cYear ", cYear,"UnitNameAndNumber ", UnitNameAndNumber,   &
         "UNIT_OUT_FRAC ", ((UNIT_OUT_FRAC(J,K,I)*100.0,J=1,13),K=1,5) 
        end if
        
        WRITE(CMUnit,rec=CMURec)   &
          FLOAT(ISC),FLOAT(cYear),UnitNameAndNumber,   &
          ((UNIT_OUT_FRAC(J,K,I)*100.0,J=1,13),K=1,5) ! report as percentages
        CMURec=CMURec+1
      END DO
      RETURN
!-----
      ENTRY INIT_MAINT_SCHEDULE(MAX_FOPM_PARM,NUNITS)
!     these arrays are saved and never deallocated, so are preserved
!     until the .exe ends; this entry must be called once for every
!     endpoint, before MAINT_SCHEDULER begins
!
      MAX_FO_PER_MONTH=MAX_FOPM_PARM
!      MAX_FO_PER_YEAR =MAX_FOPM_PARM*12 ! until caller sends another spec
      IF(ALLOCATED(UNIT_FO_ONCE)) DEALLOCATE(   &
        UNIT_FO_ONCE,UNIT_OUT_SCHD,UNIT_OUT_FORC,UNIT_OUT_DRAT,   &
        SO_BEG_MODA,SO_END_MODA,   &
        FO_BEG_HR,FO_END_HR,   &
        PD_BEG_HR,PD_END_HR,UNIT_OUT_FRAC)
!
      ALLOCATE(   &
        UNIT_FO_ONCE (12,NUNITS),   &
        UNIT_OUT_SCHD(12,NUNITS*2), & !  MAX_SO_PER_MONTH=2
        UNIT_OUT_FORC(12,NUNITS*MAX_FO_PER_MONTH),   &
        UNIT_OUT_DRAT(12,NUNITS),   & !  MAX_PD_PER_MONTH=1
        UNIT_OUT_FRAC(13,5,NUNITS))


      UNIT_FO_ONCE = B0
      UNIT_OUT_SCHD = 0
      UNIT_OUT_FORC = 0
      UNIT_OUT_DRAT = 0
      UNIT_OUT_FRAC = 0.

      ALLOCATE(SO_BEG_MODA(2,NUNITS),STAT=I4ST)
      CALL CHECK_ALLOC_STATUS(I4ST,'BDAT',B0)
      SO_BEG_MODA = 0

      ALLOCATE(SO_END_MODA(2,NUNITS),STAT=I4ST)
      CALL CHECK_ALLOC_STATUS(I4ST,'EDAT',B0)
      SO_END_MODA = 0



!     after 20010831, allow for MAX_FO_PER_MONTH>1


      ALLOCATE(FO_BEG_HR(MAX_FO_PER_MONTH,12,NUNITS),STAT=I4ST)
      CALL CHECK_ALLOC_STATUS(I4ST,'BFOH',B0)
      FO_BEG_HR = 0

      ALLOCATE(FO_END_HR(MAX_FO_PER_MONTH,12,NUNITS),STAT=I4ST)
      CALL CHECK_ALLOC_STATUS(I4ST,'EFOH',B0)
      FO_END_HR = 0

      ALLOCATE(PD_BEG_HR(MAX_PD_PER_MONTH,12,NUNITS),STAT=I4ST)
      CALL CHECK_ALLOC_STATUS(I4ST,'BPDH',B0)
      PD_BEG_HR = 0.

      ALLOCATE(PD_END_HR(MAX_PD_PER_MONTH,12,NUNITS),STAT=I4ST)
      CALL CHECK_ALLOC_STATUS(I4ST,'EPDH',B0)
      PD_END_HR = 0

      NEOSO = 0
      NEOFO = 0
      NEOPD = 0

      RETURN ! ENTRY INIT_MAINT_SCHEDULE

      ENTRY GET_FO_HRS1(GFH1_I,GFH1_K2,EAVAIL1,R_YEAR_OR_SCEN)
! using random draws, set the date and time of forced outages so that
! they meet the monthly target for EAVAIL1, allowing at most 1 FO per 
! month
      I=GFH1_I ! on [1,NUNITS]
      call write_trace_int2(file_trace_ms, "5. Shared I", I)
      FO_BEG_HR(:,:,I) = 0
      FO_END_HR(:,:,I) = 0

    ! for repeatability, seed the pseudo-random number generator
    ! 20030529 GAT added these 2 lines to overcome ...
    ! ... difference in I4ST from F77L3 to LF95
      J=NINT(EAVAIL1(1)*10000.0) 
      J=NINT(FLOAT(J)*0.4095) 
      ! use unit's I to compute the seed; FO_SEED remains 0
      IF(FO_SEED==0) THEN 
        ! short index after 20031126 per GAT
        I4ST=SeedRan32(J,GFH1_K2,R_YEAR_OR_SCEN) 
      ELSE


        I4ST=SeedRan32(J,FO_SEED,R_YEAR_OR_SCEN)
      END IF
      ! needed to flush near-identical initial vals from sequence
      PRN8=MixRand32() 

      DO JCM=1,12
        SO_THISMO_1=RANGE_COVERS   &
          (SO_BEG_MODA(1,I),SO_END_MODA(1,I),JCM)
        SO_THISMO_2=RANGE_COVERS   &
          (SO_BEG_MODA(2,I),SO_END_MODA(2,I),JCM)
        CM_DUR_DA=LENGCM_DA(JCM)
        CM_DUR_HR=LENGCM_DA(JCM)*24 ! one-based, <=744
        IF(SO_THISMO_1.OR.SO_THISMO_2) THEN
          IF(SO_THISMO_1.AND.SO_THISMO_2) THEN

!          give special treatment for unlikely event (observed by AGT);
!          find the longest run of consecutive days not on maintenance;
!          two ranges of days out cannot overlap, but can be in same JCM


            AVAIL_DA=0
            CALL GET_DAY_RANGE_THISMO(JCM,DAY1,DAY2,CM_DUR_DA,   &
              SO_BEG_MODA(1,I),SO_END_MODA(1,I))
            CALL GET_DAY_RANGE_THISMO(JCM,DAY3,DAY4,CM_DUR_DA,   &
              SO_BEG_MODA(2,I),SO_END_MODA(2,I))
            IF(((1==DAY1).AND.(DAY2==CM_DUR_DA)).OR.   &
               ((1==DAY3).AND.(DAY4==CM_DUR_DA))) THEN ! not likely
!            no forced outage can occur
            ELSE ! retain in AVAIL_DA the longest run
              IF(DAY1>DAY3) THEN ! swap order to simplify code below
                DAY0=DAY1

                DAY1=DAY3
                DAY3=DAY0
                DAY0=DAY2
                DAY2=DAY4
                DAY4=DAY0
              END IF ! with chronological order:  DAY1<=DAY2<DAY3<=DAY4
              UNSCH_DA=CM_DUR_DA-(DAY2-DAY1+1)-(DAY4-DAY3+1)

              IF(AVAIL_DA<DAY1-1) THEN
                AVAIL_DA=DAY1-1
                DAY0=0 ! 0-based index of first day in the run
              END IF

              IF(AVAIL_DA<DAY3-DAY2-1) THEN
                AVAIL_DA=DAY3-DAY2-1
                DAY0=DAY2
              END IF

              IF(AVAIL_DA<CM_DUR_DA-DAY4) THEN
                AVAIL_DA=CM_DUR_DA-DAY4
                DAY0=DAY4
              END IF
            END IF ! re determination of longest run of available days
          ELSE ! there was only 1 scheduled outage this month
            IF(SO_THISMO_1) THEN
              BEG_O_DATE=SO_BEG_MODA(1,I)
              END_O_DATE=SO_END_MODA(1,I)
            ELSE ! SO_THISMO_2
              BEG_O_DATE=SO_BEG_MODA(2,I)
              END_O_DATE=SO_END_MODA(2,I)
            END IF
            CALL GET_DAY_RANGE_THISMO(JCM,DAY1,DAY2,CM_DUR_DA,   &
              BEG_O_DATE,END_O_DATE)
            UNSCH_DA=CM_DUR_DA-(DAY2-DAY1+1)
            AVAIL_DA=UNSCH_DA
            IF(AVAIL_DA<=0) THEN ! no forced outage can occur
            ELSEIF(DAY1-1>CM_DUR_DA-DAY2) THEN ! early run is roomier
              AVAIL_DA=DAY1-1
              DAY0=0
            ELSE ! late run (at end of month) is roomier
              AVAIL_DA=CM_DUR_DA-DAY2
              DAY0=DAY2
            END IF
          END IF ! re # scheduled outages this month
        ELSE ! no maintenance was scheduled herein for month JCM
          UNSCH_DA=CM_DUR_DA
          AVAIL_DA=UNSCH_DA
          DAY0=0
        END IF
        IF(AVAIL_DA>0) THEN ! forced outage can occur

          FO_DUR_HR=MIN0(NINT((1.-EAVAIL1(JCM))*FLOAT(UNSCH_DA*24)),   &
!                duration of single FO is limited to longest run of UNSCH_DA 
!                contiguous
            AVAIL_DA*24) 
!         bias SLACK_HR up by +1 so that HR1 can be scheduled below even if
!         the entire month is on FO
          SLACK_HR=AVAIL_DA*24+1-FO_DUR_HR ! at most CM_DUR_HR+1-FO_DUR_HR


          IF(FO_DUR_HR<=0) THEN
            FO_DUR_HR=0
            HR1=0
          ELSEIF(SLACK_HR>0) THEN ! schedule HR1 evenly on [1,SLACK_HR]

            HR1=DAY0*24+   &
              NINT(0.501d0+MixRand32()*(dble(SLACK_HR)-0.002d0))
!          0.501 bias above forces NINT to be at least 1 when MixRand32~0;
!          0.002 bias above forces NINT to be <=SLACK_HR when MixRand32~1


          ELSE ! no slack; best we can do is fill the larger void
            FO_DUR_HR=AVAIL_DA*24 ! => off-target EAVAIL1(JCM)
            HR1=1+DAY0*24
          END IF
        ELSE
          FO_DUR_HR=0
        END IF

        IF(FO_DUR_HR>0) THEN
          FO_BEG_HR(1,JCM,I)=HR1 ! one-based

          ! such that difference is duration
          FO_END_HR(1,JCM,I)=HR1+FO_DUR_HR 
          NEOFO(JCM)=NEOFO(JCM)+1
          UNIT_OUT_FORC(JCM,NEOFO(JCM))=I ! FO begins and ends in JCM
          ! net increase
          UNIT_OUT_FRAC(JCM,4,I)=FLOAT(FO_DUR_HR)/FLOAT(CM_DUR_HR) 
        END IF
      END DO ! JCM
      RETURN ! ENTRY GET_FO_HRS1

      ENTRY GET_FO_HRS2(GFH2_I,GFH2_K2,FO_FREQ,FO_DURA,EAVAIL2)
    ! allow multiple FO per month in an exponential distribution;
    ! using random draws, set the date and time of forced outages so
    ! that they meet the annual targets for FO_FREQ and FO_DURA
      I=GFH2_I ! on [1,NUNITS]
      call write_trace_int2(file_trace_ms, "6. Shared I", I)
    ! for repeatability, seed the pseudo-random number generator
      ! 20030529 GAT added these 2 lines to overcome ...
      J=NINT(EAVAIL2(1)*10000.0) 
      ! ... difference in I4ST from F77L3 to LF95

      J=NINT(FLOAT(J)*0.4095) 
      ! use unit's I to compute the seed; FO_SEED remains 0
      IF(FO_SEED==0) THEN 

        ! short index after 20031126 per GAT
        I4ST=SeedRan32(J,GFH2_K2,IYR) 
      ELSE


        I4ST=SeedRan32(J,FO_SEED,IYR)
      END IF
      ! needed to flush near-identical initial vals from sequence
      PRN8=MixRand32() 

      DAY1=0
      DAY2=0
      DAY3=0
      DAY4=0
      DO JPR=2,1,-1
        BEG_O_DATE=SO_BEG_MODA(JPR,I)
        END_O_DATE=SO_END_MODA(JPR,I)
        IF(BEG_O_DATE==0) THEN
          IF(JPR==2) THEN
            N_AVAIL_SEGS=1
          ELSE !==1
            DAY0=0 ! the entire year is devoid of scheduled maintenance
          END IF
        ELSE
          MON1=BEG_O_DATE/100
          MON2=END_O_DATE/100
          IF(JPR==2) THEN
            N_AVAIL_SEGS=2 ! both BEG/END pairs are non-void
            DAY3=CUMUL_CM_DA(MON1-1)+MOD(BEG_O_DATE,100)
            DAY4=CUMUL_CM_DA(MON2-1)+MOD(END_O_DATE,100)
          ELSE !==1

            DAY1=CUMUL_CM_DA(MON1-1)+MOD(BEG_O_DATE,100)
            DAY2=CUMUL_CM_DA(MON2-1)+MOD(END_O_DATE,100)

         DAY0=DAY2 ! 0-based day index of 1st segment of available days


          END IF
        END IF
      END DO ! JPR
      IF(N_AVAIL_SEGS==2) THEN
        IF(DAY1>DAY3) THEN ! swap order to simplify code below
          DAY0=DAY1

          DAY1=DAY3
          DAY3=DAY0

          DAY0=DAY2
          DAY2=DAY4
          DAY4=DAY0

         DAY0=DAY2 ! 0-based day index of 1st segment of available days
        END IF ! with chronological order:  DAY1<=DAY2<DAY3
        AV_DAYS_SEG(1)=DAY3-DAY2-1 ! 0 intervening when DAY3==DAY2+1
        AV_DAYS_SEG(2)=DAY1-DAY4-1
        IF(DAY1<DAY4) AV_DAYS_SEG(2)=CUMUL_CM_DA(12)+DAY1-DAY4-1
        IF(AV_DAYS_SEG(1)==0) THEN ! unlikely
          AV_DAYS_SEG(1)=AV_DAYS_SEG(2)
          N_AVAIL_SEGS=1
        END IF
      ELSE
        AV_DAYS_SEG(1)=CUMUL_CM_DA(12)-DAY2+DAY1-1
        AV_DAYS_SEG(2)=0
      END IF

      FO_BEG_HR(:,:,I) = 0             ! default valid for EoM/EoY wrap

      FO_END_HR(:,:,I) = 0

      NFO_SEG(1)=FO_FREQ ! expected # of forced outages per year
      NFO_SEG(2)=0.0
      ! allocate proportional to segment length
      IF(N_AVAIL_SEGS==2) THEN 
        NFO_SEG(2)=FLOAT(NINT(NFO_SEG(1)*FLOAT(AV_DAYS_SEG(2))/ &
      ! round 2nd to nearest integer
         FLOAT(AV_DAYS_SEG(1)+AV_DAYS_SEG(2)))) 


        NFO_SEG(1)=NFO_SEG(1)-NFO_SEG(2) ! possibly zero
!       aggregate so that de-minimal portions are not ignored
        IF    (INT(NFO_SEG(1))<1) THEN
          NFO_SEG(2)=FO_FREQ
          NFO_SEG(1)=0.0
        ELSEIF(INT(NFO_SEG(2))<1) THEN
          NFO_SEG(1)=FO_FREQ
          NFO_SEG(2)=0.0
        END IF
      END IF

      IF((FO_FREQ<0.5).OR.(FO_FREQ>FLOAT(12*MAX_FO_PER_MONTH)))   &
        WRITE(4,'(A,I5/A,F9.3)')   &
        ' Warning:  forced-outage frequency (#/yr) for unit ',I,   &
        ' is dubious at ',FO_FREQ
      IF((FO_DURA<0.5).OR.(FO_DURA>720.0)) WRITE(4,'(A,I5/A,F9.3)')   &
        ' Warning:  mean forced-outage duration in hours for unit ',I,   &
! FO_DURA>LENGCM_DA violates reporting assumptions
        ' is dubious at ',FO_DURA 



      DO JPR=1,N_AVAIL_SEGS ! scheduled mnt. may have split avail time
        IF(INT(NFO_SEG(JPR))<1) CYCLE
        call GetFailRepTimes(NFO_SEG(JPR),MAX_FO_PER_MONTH,FO_DURA,   &
          24*AV_DAYS_SEG(JPR),HrsToState)
        BEG_IN_MO = 0
        END_IN_MO = 0
        ! 0-based offset => begin Fail/Rep after mnt.
        HR_IN_YR=FLOAT(DAY0*24) 
        DO J2=1,INT(NFO_SEG(JPR)+0.001)
!        for the number of forced-outages in each segment,
!        assign month and hour at beginning of forced outage
          HR_IN_YR=HR_IN_YR+HrsToState(0,J2)
          ! map hours 1..24 into day 1, etc.
          JULIAN_DATE=(NINT(HR_IN_YR)+23)/24 
          ! rounding here accounts for off-target results
          HR_IN_DA=NINT(HR_IN_YR)-24*JULIAN_DATE 
          J0=MMDD_AT(JULIAN_DATE,CUMUL_CM_DA)
          JCM=J0/100
          K=MOD(J0,100)
          BEG_IN_MO(JCM)=BEG_IN_MO(JCM)+1
          IF(BEG_IN_MO(JCM)>MAX_FO_PER_MONTH) THEN

            WRITE(4,'(a,i2,a,i2)')' FO-event begin-count in month ',JCM,   &
              ' in GET_FO_HRS2 exceeds limit of ',MAX_FO_PER_MONTH

            CYCLE
          END IF
          FO_BEG_HR(BEG_IN_MO(JCM),JCM,I)=K*24+HR_IN_DA
          IF(UNIT_FO_ONCE(JCM,I).EQV.B0) THEN
            UNIT_FO_ONCE(JCM,I)=B1
            NEOFO(JCM)=NEOFO(JCM)+1
            UNIT_OUT_FORC(JCM,NEOFO(JCM))=I
          END IF

        ! assign month and hour at end of forced outage
          HR_IN_YR=HR_IN_YR+HrsToState(1,J2)
          ! map hours 1..24 into day 1, etc.
          JULIAN_DATE=(NINT(HR_IN_YR)+23)/24
          ! rounding here accounts for off-target results
          HR_IN_DA=NINT(HR_IN_YR)-24*JULIAN_DATE



          J1=MMDD_AT(JULIAN_DATE,CUMUL_CM_DA)
          JCM=J1/100
          K=MOD(J1,100)
          END_IN_MO(JCM)=END_IN_MO(JCM)+1
          IF(END_IN_MO(JCM)>MAX_FO_PER_MONTH) THEN
            WRITE(4,'(a,i2,a,i2)') ' FO-event end-count in month ',JCM, &
              ' in GET_FO_HRS2 exceeds limit of ',MAX_FO_PER_MONTH
            CYCLE
          END IF
          ! this-BEG is duration after 20010806
          FO_END_HR(END_IN_MO(JCM),JCM,I)=K*24+HR_IN_DA 
          IF(UNIT_FO_ONCE(JCM,I).EQV.B0) THEN
            UNIT_FO_ONCE(JCM,I)=B1
            NEOFO(JCM)=NEOFO(JCM)+1
            UNIT_OUT_FORC(JCM,NEOFO(JCM))=I
          END IF


          IF(BEG_IN_MO(JCM)<END_IN_MO(JCM)) & !  default FO_BEG_HR was 0
             BEG_IN_MO(JCM)=END_IN_MO(JCM) ! EoM or EoY wrap => FO_BEG_HR==0


        END DO ! J2
        DAY0=DAY4 ! 0-based day index of 2nd segment of available days
      END DO ! JPR

      DO JCM=1,12 ! summarize FO for reporting to .CMD-file
        J=LENGCM_DA(JCM)*24
        K=0
        DO IFO=1,MAX_FO_PER_MONTH
          J1=FO_BEG_HR(IFO,JCM,I)
          J2=FO_END_HR(IFO,JCM,I)
          IF((J1==0).AND.(J2==0)) EXIT
          IF(J2==0) J2=J ! FO event ends in a later month
          K=K+J2-J1
          ! FO_BEG_HR & FO_END_HR are for distinct FO events
          IF(J2<J1) K=K+J 
        END DO
        UNIT_OUT_FRAC(JCM,4,I)=FLOAT(K)/FLOAT(J)

      END DO
      RETURN ! ENTRY GET_FO_HRS2

!-----
      ENTRY GET_FO_HRS_BLOCK(GFHB_I,BAVAIL,BEG_FO_HR_BK,END_FO_HR_BK)
    ! using random draws, set the date and hour of forced outages of a
    ! (relatively unreliable) upper block whose unit's SO and FO have 
    ! been previously determined, so that they meet
    ! the monthly target for BAVAIL, allowing at most 1 FO per month;
    ! assume the pseudo-random number generator has been used 
    ! previously duringcalls to GET_FO_HRS1 for lower block(s), hence 
    ! needs no seeding.
      I=GFHB_I
      call write_trace_int2(file_trace_ms, "7. Shared I", I)
      DO JCM=1,12
        CM_DUR_DA=LENGCM_DA(JCM)
        CM_DUR_HR=LENGCM_DA(JCM)*24 ! one-based, <=744

        ! define the set of hours the unit is on neither SO
        ! (maint) nor FO (forced)
        DO KHR=1,CM_DUR_HR
          BK_IS_AVAIL(KHR)=B1
        END DO
        DO JPR=1,2 ! mark-out hours on scheduled maint
          IF(RANGE_COVERS(SO_BEG_MODA(JPR,I),SO_END_MODA(JPR,I),JCM))   &
            THEN ! unit is down for maint during month JCM
            CALL GET_DAY_RANGE_THISMO(JCM,DAY1,DAY2,CM_DUR_DA,   &
              SO_BEG_MODA(1,I),SO_END_MODA(1,I))
            HR1=DAY1*24-23
            HR2=DAY2*24
            DO KHR=HR1,HR2
              BK_IS_AVAIL(KHR)=B0
            END DO
          END IF
        END DO
        ! 1st index of 1 assumes GET_FO_HRS1 was used
        HR1=FO_BEG_HR(1,JCM,I) 
        HR2=FO_END_HR(1,JCM,I)
        ! mark-out hours the lower blocks are on forced-outage
        DO KHR=HR1,HR2 
          BK_IS_AVAIL(KHR)=B0
        END DO

! find the longest run of consecutive hours the block is available


        HR1=0 ! default implying there is no hour available
        BA_HR1=0
        BA_DUR_HR=0 ! duration of the longest run of block-avail hours
        DO KHR=1,CM_DUR_HR
          IF(BK_IS_AVAIL(KHR).EQV.B1) THEN
            IF(HR1==0) HR1=KHR
          ELSE ! BK_IS_AVAIL(KHR).EQV.B0)
            HR2=KHR
            IF(HR1>0) THEN
              IF(BA_DUR_HR<HR2-HR1) THEN
                BA_DUR_HR=HR2-HR1 ! retain the longer
                BA_HR1=HR1
              END IF
          HR1=0 ! reset the gate to allow later segments to be compared
            END IF
          END IF
        END DO
! treat the end-of-month as if the following hour were not available
        HR2=KHR ! ==CM_DUR_HR+1
        IF((HR1>0).AND.(BA_DUR_HR<HR2-HR1)) THEN
          BA_DUR_HR=HR2-HR1 ! retain the longer
          BA_HR1=HR1
        END IF
        ! default implying 'no forced outage this month'
        BEG_FO_HR_BK(JCM)=0 
        END_FO_HR_BK(JCM)=0
        IF(BA_DUR_HR>0) THEN ! forced outage can occur
          FO_DUR_HR=NINT((1.-BAVAIL(JCM))*FLOAT(BA_DUR_HR))

!         bias SLACK_HR up by +1 so that HR1 can be scheduled below even if
!         the entire month is on FO


          SLACK_HR=BA_DUR_HR+1-FO_DUR_HR

          IF(FO_DUR_HR<=0) THEN
            FO_DUR_HR=0
          ELSEIF(SLACK_HR>0) THEN ! schedule HR1 evenly on [1,SLACK_HR]

            HR1=BA_HR1-1+NINT(   &
              0.501d0+MixRand32()*(dble(SLACK_HR)-0.002d0))
! 0.501 bias above forces NINT to be at least 1 when MixRand32~0;
! 0.002 bias above forces NINT to be <=SLACK_HR when MixRand32~1
          ELSE ! no slack; best we can do is fill the larger void


            FO_DUR_HR=BA_DUR_HR ! => off-target BAVAIL(JCM)
            HR1=BA_HR1
          END IF

          IF(FO_DUR_HR>0) THEN
            BEG_FO_HR_BK(JCM)=HR1 ! 1-based
            ! such that difference is duration
            END_FO_HR_BK(JCM)=HR1+FO_DUR_HR 
          END IF
        END IF
      END DO ! JCM
      RETURN ! ENTRY GET_FO_HRS_BLOCK


      ENTRY GET_FO_HRS_TLINE(GFHT_K2, & !  assume MAX_TL_FO_PER_MONTH is 1
        R_YEAR_OR_SCEN,R_MONTH,GFHT_LENGCM_DA,TL_FOR,   &
        BEG_FO_HR_TL,END_FO_HR_TL)
!     using random draws, set the date and hour of forced outages of a
!     transmission line (seq. indexed GFHT_K2), so that they meet the monthly
!     target for TL_FOR, allowing at most 1 FO per month

! preserve (ala push) other users' location in random sequence
      GFHT_SEQN_HOLD=SeedRan321(0) 
      IF(GFHT_SEQN_USED==0) THEN ! it has not yet been initialized;
!       for repeatability, seed the pseudo-random number generator
        J=NINT(TL_FOR*10000.0) ! 20030529 GAT added these 2 lines to overcome ...
        J=NINT(FLOAT(J)*0.4095) ! ... difference in I4ST from F77L3 to LF95
!       use line's K2 & R_MONTH to compute the seed
        I=GFHT_K2*16+R_MONTH ! valid for GFHT_K2<1024 and R_MONTH<16
        call write_trace_int2(file_trace_ms, "8. Shared I", I)
        GFHT_SEQN_USED=SeedRan32(J,I,R_YEAR_OR_SCEN)


      ELSE
         !restore GFHT location in random sequence
        I4ST=SeedRan321(GFHT_SEQN_USED) !
      END IF

      CM_DUR_HR=GFHT_LENGCM_DA*24 ! one-based, <=744
      BEG_FO_HR_TL=0 ! default implying 'no forced outage this month'
      END_FO_HR_TL=0
      FO_DUR_HR=NINT(TL_FOR*FLOAT(CM_DUR_HR))
      SLACK_HR=CM_DUR_HR+1-FO_DUR_HR

      IF(FO_DUR_HR<=0) THEN
        FO_DUR_HR=0
      ELSEIF(SLACK_HR>0) THEN ! schedule HR1 evenly on [1,SLACK_HR]
        HR1=NINT(0.501d0+MixRand32()*(dble(SLACK_HR)-0.002d0))
      ! 0.501 bias above forces NINT to be at least 1 when MixRand32~0;
      ! 0.002 bias above forces NINT to be <=SLACK_HR when MixRand32~1

        ! preserve GFHT location in random sequence
        GFHT_SEQN_USED=SeedRan321(0) 


      ELSE ! no slack; best we can do is fill the void
        FO_DUR_HR=CM_DUR_HR ! => off-target TL_FOR
        HR1=1
      END IF

      IF(FO_DUR_HR>0) THEN
        BEG_FO_HR_TL=HR1 ! 1-based
        END_FO_HR_TL=HR1+FO_DUR_HR ! such that difference is duration
      END IF

      ! restore (ala pop) other users' location in random sequence
      I4ST=SeedRan321(GFHT_SEQN_HOLD) 
      RETURN ! ENTRY GET_FO_HRS_TLINE

      ENTRY GET_PD_HRS1(GPH1_I,DAVAIL1,BEG_PD_HR1,END_PD_HR1, &
      GPH1_MPDPM)
! using random draws, set the date and time of partial-derating outages
! so they meet the monthly target for DAVAIL1, allowing at most 1 PD 
! per month; 
! note that the fractional derating is irrelevant here in timing 
! events.

      I=GPH1_I ! on [1,NUNITS]
      call write_trace_int2(file_trace_ms, "9. Shared I", I)
      DO JCM=1,12
        BEG_PD_HR1(JCM)=0
        END_PD_HR1(JCM)=0
        SO_THISMO_1=RANGE_COVERS   &
          (SO_BEG_MODA(1,I),SO_END_MODA(1,I),JCM)
        SO_THISMO_2=RANGE_COVERS   &
          (SO_BEG_MODA(2,I),SO_END_MODA(2,I),JCM)
        CM_DUR_DA=LENGCM_DA(JCM)

        IF(SO_THISMO_1.OR.SO_THISMO_2) THEN
          IF(SO_THISMO_1.AND.SO_THISMO_2) THEN

!           give special treatment for unlikely event (observed by AGT);
!           find the longest run of consecutive days not on maintenance;
!           two ranges of days out cannot overlap, but can be in same JCM



            AVAIL_DA=0
            CALL GET_DAY_RANGE_THISMO(JCM,DAY1,DAY2,CM_DUR_DA,   &
              SO_BEG_MODA(1,I),SO_END_MODA(1,I))
            CALL GET_DAY_RANGE_THISMO(JCM,DAY3,DAY4,CM_DUR_DA,   &
              SO_BEG_MODA(2,I),SO_END_MODA(2,I))
            IF(((1==DAY1).AND.(DAY2==CM_DUR_DA)).OR.   &
               ((1==DAY3).AND.(DAY4==CM_DUR_DA))) THEN ! not likely
!             no forced outage (full or derated) can occur
            ELSE
              IF(DAY1>DAY3) THEN ! swap order to simplify code below
                DAY0=DAY1

                DAY1=DAY3
                DAY3=DAY0
                DAY0=DAY2
                DAY2=DAY4
                DAY4=DAY0
              END IF ! with chronological order:  DAY1<=DAY2<DAY3<=DAY4

              IF(AVAIL_DA<DAY1-1) THEN
                AVAIL_DA=DAY1-1
                DAY0=0 ! 0-based index of first day in the run
              END IF

              IF(AVAIL_DA<DAY3-DAY2-1) THEN
                AVAIL_DA=DAY3-DAY2-1
                DAY0=DAY2
              END IF

              IF(AVAIL_DA<CM_DUR_DA-DAY4) THEN
                AVAIL_DA=CM_DUR_DA-DAY4
                DAY0=DAY4
              END IF
            END IF ! re determination of longest run of available days
          ELSE ! there was only 1 scheduled outage this month
            IF(SO_THISMO_1) THEN
              BEG_O_DATE=SO_BEG_MODA(1,I)
              END_O_DATE=SO_END_MODA(1,I)
            ELSE ! SO_THISMO_2
              BEG_O_DATE=SO_BEG_MODA(2,I)
              END_O_DATE=SO_END_MODA(2,I)
            END IF
            CALL GET_DAY_RANGE_THISMO(JCM,DAY1,DAY2,CM_DUR_DA,   &
              BEG_O_DATE,END_O_DATE)
            AVAIL_DA=CM_DUR_DA-(DAY2-DAY1+1)
            IF(AVAIL_DA<=0) THEN 
                ! no forced outage (full or derated) can occur
            ELSEIF(DAY1-1>CM_DUR_DA-DAY2) THEN ! early run is roomier
              AVAIL_DA=DAY1-1
              DAY0=0
            ELSE ! late run (at end of month) is roomier
              AVAIL_DA=CM_DUR_DA-DAY2
              DAY0=DAY2
            END IF
          END IF ! re # scheduled outages this month
        ELSE ! no maintenance was scheduled herein for month JCM
          AVAIL_DA=CM_DUR_DA
          DAY0=0
        END IF

        IF(AVAIL_DA>0) THEN ! forced outage (full or derated) can occur
!         mark DA_AVAIL false for days in JCM on SO or FO

          DAY1=DAY0+1        ! first day of AVAIL_DA
          DAY2=DAY0+AVAIL_DA ! last  day of AVAIL_DA
          J1=DAY1
          J2=DAY2
          DO J=0,J2 ! 0 covers possibility that DAY1==1
            DA_AVAIL(J)=(J>=J1)
          END DO
          DO IFO=1,GPH1_MPDPM
            FO_BEG_DAY1=(FO_BEG_HR(IFO,JCM,I)-1)/24+1
            FO_END_DAY2=(FO_END_HR(IFO,JCM,I)-1)/24+1

            DO J=FO_BEG_DAY1,FO_END_DAY2
              DA_AVAIL(J)=B0
            END DO
          END DO

!         retain longest contiguous block in DA_AVAIL as [DAY3,DAY4]
!         DAY1 is default for initial day of longest block

          DAY2=DAY0 ! default final day such that default length is 0
          DAY3=DAY1
          DAY4=DAY2
          

          DO J=J1,J2
            IF(DA_AVAIL(J).EQV.B1) THEN
              IF(DA_AVAIL(J-1).EQV.B0) THEN ! begin a new block

                DAY1=J
                DAY2=J
              ELSE ! extend the current block
                DAY2=J
              END IF
              ! save the longer as [DAY3,DAY4]
              IF((DAY4-DAY3)<(DAY2-DAY1)) THEN 
                DAY3=DAY1
                DAY4=DAY2
              END IF
            END IF
          END DO
          DAY0=DAY3-1
          AVAIL_DA=DAY4-DAY0
        END IF

        IF(AVAIL_DA>0) THEN ! forced derated outage can occur
          PD_DUR_HR=NINT((1.-DAVAIL1)*FLOAT(AVAIL_DA*24))

          SLACK_HR=AVAIL_DA*24+1-PD_DUR_HR 
        ! bias SLACK_HR up by +1 so that HR1 can be scheduled below 
        ! even if the entire month is on PD
          IF(PD_DUR_HR<=0) THEN
            PD_DUR_HR=0
            HR1=0
          ELSEIF(SLACK_HR>0) THEN
            ! schedule HR1 evenly on [1,SLACK_HR]          
            HR1=DAY0*24+ &
             NINT(0.501d0+MixRand32()*(dble(SLACK_HR)-0.002d0))
! 0.501 bias above forces NINT to be at least 1 when MixRand32~0;
! 0.002 bias above forces NINT to be <=SLACK_HR when MixRand32~1


          ELSE ! no slack; best we can do is fill the larger void
            PD_DUR_HR=AVAIL_DA*24 ! => off-target DAVAIL1
            HR1=1+DAY0*24
          END IF
        ELSE
          PD_DUR_HR=0
          HR1=0 ! implying no partial forced outage this month
        END IF

        IF(PD_DUR_HR>0) THEN
          BEG_PD_HR1(JCM)=HR1 ! one-based


          END_PD_HR1(JCM)=HR1+PD_DUR_HR 


          NEOPD(JCM)=NEOPD(JCM)+1
          UNIT_OUT_DRAT(JCM,NEOPD(JCM))=I
        END IF
      END DO ! JCM

!     copy I/O arrays for use in GET_PD_HRS_RANGE;
!     expect the caller to know that MAX_PD_PER_MONTH==1
      DO JCM=1,12
        PD_BEG_HR(1,JCM,I)=BEG_PD_HR1(JCM)
        PD_END_HR(1,JCM,I)=END_PD_HR1(JCM)
      END DO
      RETURN ! ENTRY GET_PD_HRS1


      ENTRY INCREMENT_EVENTS_OF_SO(IESJPR)
      MON1=SO_BEG_MODA(IESJPR,I)/100
      MON2=SO_END_MODA(IESJPR,I)/100
      IF(MON2<MON1) MON2=MON2+12 ! for EoY wrap
      DO IESMON=MON1,MON2
        JCM=1+MOD(IESMON-1,12)
! augment count of mnt.-outage events each month (not evident in MSF)
        NEOSO(JCM)=NEOSO(JCM)+1
        UNIT_OUT_SCHD(JCM,NEOSO(JCM))=I
      END DO
      RETURN

      ! both on [1,NUNITS]
      ENTRY GET_FO_HRS_RANGE(GFHR_INDEX,GFHR_ORG_UNIT, &
      ! return hours for 1 month for 1 unit
       GFHR_MO) 
      ! indicating request for GFHR_INDEX outage hours is invalid
      BEG_HR_FO(1)=0 
      END_HR_FO(1)=0
      GFHR_ORG_UNIT=UNIT_OUT_FORC(GFHR_MO,GFHR_INDEX)
      IF((1<=GFHR_ORG_UNIT).AND.(GFHR_ORG_UNIT<=NU_LOCAL)) THEN


        DO IFO=1,MAX_FO_PER_MONTH
          BEG_HR_FO(IFO)=FO_BEG_HR(IFO,GFHR_MO,GFHR_ORG_UNIT)
          END_HR_FO(IFO)=FO_END_HR(IFO,GFHR_MO,GFHR_ORG_UNIT)
        END DO
      END IF
      RETURN


      ! both on [1,NUNITS]
      ENTRY GET_PD_HRS_RANGE(GPHR_INDEX,GPHR_ORG_UNIT, &
      ! return hours for 1 month for 1 unit
       GPHR_MO,BEG_HR_PD,END_HR_PD) 
      ! indicating request for GPHR_INDEX outage hours is invalid
      BEG_HR_PD(1)=0 
      END_HR_PD(1)=0
      GPHR_ORG_UNIT=UNIT_OUT_DRAT(GPHR_MO,GPHR_INDEX)
      IF((1<=GPHR_ORG_UNIT).AND.(GPHR_ORG_UNIT<=NU_LOCAL)) THEN
        DO IPD=1,MAX_PD_PER_MONTH
          BEG_HR_PD(IPD)=PD_BEG_HR(IPD,GPHR_MO,GPHR_ORG_UNIT)
          END_HR_PD(IPD)=PD_END_HR(IPD,GPHR_MO,GPHR_ORG_UNIT)
        END DO
      END IF
      RETURN


      ENTRY GET_MAINT_DATE_RANGE(GMDR_INDEX,GMDR_ORG_UNIT, &
       GMDR_MO,GBEG_DD,GEND_DD) ! outage dates within GMDR_MO
      DO GMDR_JPAIR=1,2
!       -1 indicates request for GMDR_INDEX mnt. dates is invalid
        GBEG_DD(GMDR_JPAIR)=-1
        GEND_DD(GMDR_JPAIR)=-1
      END DO
      GMDR_ORG_UNIT=UNIT_OUT_SCHD(GMDR_MO,GMDR_INDEX) ! on [1,NUNITS]
      IF((1<=GMDR_ORG_UNIT).AND.(GMDR_ORG_UNIT<=NU_LOCAL)) THEN
        DO GMDR_JPAIR=1,2
          GBEG_DD(GMDR_JPAIR)=0
          GEND_DD(GMDR_JPAIR)=0
          BEG_O_DATE=SO_BEG_MODA(GMDR_JPAIR,GMDR_ORG_UNIT)
          END_O_DATE=SO_END_MODA(GMDR_JPAIR,GMDR_ORG_UNIT)

          IF(RANGE_COVERS(BEG_O_DATE,END_O_DATE,GMDR_MO))   &
            CALL GET_DAY_RANGE_THISMO(GMDR_MO,   &
            GBEG_DD(GMDR_JPAIR),GEND_DD(GMDR_JPAIR),LENGCM_DA(GMDR_MO),   &
            BEG_O_DATE,END_O_DATE)


        END DO
      END IF
      RETURN ! ENTRY GET_MAINT_DATE_RANGE


      ENTRY COUNT_EVENTS_OF_SCHED_OUTAGE(CEOSO_EVENTS,CEOSO_MO)
      ! MAINT_SCHEDULED_FRACT is N.A. to entry
      CEOSO_EVENTS=NEOSO(CEOSO_MO) 
      RETURN


      ENTRY COUNT_EVENTS_OF_FORCED_OUTAGE(CEOFO_EVENTS,CEOFO_MO)
      ! valid only after looping on GET_FO_HRS
      CEOFO_EVENTS=NEOFO(CEOFO_MO) 
      RETURN


      ENTRY COUNT_EVENTS_OF_PARTIAL_OUTAGE(CEOPD_EVENTS,CEOPD_MO)
      ! valid only after looping on GET_PD_HRS
      CEOPD_EVENTS=NEOPD(CEOPD_MO) 
      RETURN


      ENTRY SW_MAINT_DERATING(SMD_ON)
      DERATABLE=SMD_ON ! copy parameter value into common var
      RETURN


      ENTRY REVISE_FO_SEED(R_FO_SEED) 
        IF(0.0<=R_FO_SEED.AND.R_FO_SEED<16383.5) &
         FO_SEED=NINT(R_FO_SEED)
        IF(R_FO_SEED<0.0) FO_SEED=0 ! restore initial default
      RETURN


      END SUBROUTINE MAINT_SCHEDULER


      SUBROUTINE IRSORT1(NITEMS,RVECTOR,IPOS,INCREASING)

      ! Sorts indices (IPOS) of NITEMS; adapted from the 
      ! source on page 110 of SOFTWARE TOOLS IN PASCAL, 
      ! by Kernighan & Plauger, Addison-Wesley 1981

      INTEGER (kind=2) ::  I,J,K,P,Q,GAP,NITEMS,IPOS(*)
      LOGICAL (kind=1) ::  INCREASING,DECREASING
      REAL (kind=4) ::  RVECTOR(*) ! one-based indexing assumed herein

      DECREASING=.NOT.INCREASING
      GAP=NITEMS/2
      DOWHILE(GAP>0)
        DO I=GAP+1,NITEMS ! GAP,NITEMS-1 for 0-based arrays

          J=I-GAP

          DOWHILE(J>0) !>=0 for 0-based arrays
            K=J+GAP
            P=IPOS(J)
            Q=IPOS(K)
            IF((INCREASING.AND.(RVECTOR(P)<=RVECTOR(Q))).OR.   &
              (DECREASING.AND.(RVECTOR(P)>=RVECTOR(Q)))) THEN


              J=0 ! break the do-while loop (=-1 for 0-based arrays)
            ELSE  ! interchange the indexing array values
              IPOS(K)=P
              IPOS(J)=Q
            END IF

            J=J-GAP
          END DO
        END DO

        GAP=GAP/2
      END DO
      END ! SUBROUTINE IRSORT1


      SUBROUTINE I2SORT1(NITEMS,IVECTOR,IPOS,INCREASING)
! Sorts indices (IPOS) of NITEMS; adapted from the source on page 110 
! of
! SOFTWARE TOOLS IN PASCAL, by Kernighan & Plauger, Addison-Wesley 1981
      ! one-based indexing assumed herein
      INTEGER (kind=2) ::  I,J,K,P,Q,GAP,NITEMS,IPOS(*),IVECTOR(*)
      LOGICAL (kind=1) ::  INCREASING,DECREASING
!


      DECREASING=.NOT.INCREASING
      GAP=NITEMS/2
      DOWHILE(GAP>0)
        DO I=GAP+1,NITEMS ! GAP,NITEMS-1 for 0-based arrays

          J=I-GAP

          DOWHILE(J>0) !>=0 for 0-based arrays
            K=J+GAP
            P=IPOS(J)
            Q=IPOS(K)
            IF((INCREASING.AND.(IVECTOR(P)<=IVECTOR(Q))).OR.   &
              (DECREASING.AND.(IVECTOR(P)>=IVECTOR(Q)))) THEN
              J=0 ! break the do-while loop (=-1 for 0-based arrays)
            ELSE  ! interchange the indexing array values
              IPOS(K)=P
              IPOS(J)=Q
            END IF
            J=J-GAP
          END DO
        END DO
        GAP=GAP/2
      END DO
      END ! SUBROUTINE I2SORT1


      SUBROUTINE CHECK_ALLOC_STATUS(ISTATUS,ABBREV,DEALLOC)
      INTEGER (kind=4) ::  ISTATUS
      LOGICAL (kind=1) ::  DEALLOC
      CHARACTER (len=*) :: ABBREV
      CHARACTER (len=12) ::  S1=' Failure in '
      CHARACTER (len=11) ::  S2='allocating '
      CHARACTER (len=6) ::   S3=' array'
      IF(ISTATUS/=0) THEN
        IF(DEALLOC) THEN
          WRITE(4,*)S1,'de',S2,ABBREV,S3
        ELSE
          WRITE(4,*)S1,S2,ABBREV,S3
        END IF
      END IF
      END


      SUBROUTINE INCORPORATE_MAINT(DAY1,DURA_DA,MRQ_DA,CAPACITY)
      use rollscom
      implicit none
      
      INTEGER (kind=2) ::  DAY1,DURA_DA,I,INIT_DA,FINA_DA
      REAL (kind=4) ::  MRQ_DA,CAPACITY,CAP_EFFECTIVE


      IF(DURA_DA<=0) RETURN ! no action is required
      INIT_DA=DAY1 ! copy to avoid corrupting caller's values
      FINA_DA=INIT_DA+DURA_DA-1
      IF(DERATABLE) THEN
         ! avoid inflating MWD area
        CAP_EFFECTIVE=CAPACITY*MRQ_DA/FLOAT(DURA_DA)
      ELSE
        CAP_EFFECTIVE=CAPACITY ! use the full nominal capacity
      END IF
      DO I=0,1 ! second pass handles the EoY wrap, if necessary
        
        ADDEND=0.0

        DO IDA=INIT_DA,LENGYR_DA
          IF(IDA<=FINA_DA) ADDEND=ADDEND+CAP_EFFECTIVE
          LOAD_ACCUM(IDA)=LOAD_ACCUM(IDA)+ADDEND
        END DO

        IF(I==1) EXIT
        
        IF(FINA_DA<=LENGYR_DA) EXIT ! no wrap-around to low indices
        FINA_DA=FINA_DA-LENGYR_DA
        INIT_DA=1
      END DO
      END ! SUBROUTINE INCORPORATE_MAINT


      SUBROUTINE UPDATE_LIB_MS(IUNIT,DAY1,DURA_DA,LENGYR_DA,IYR,   &
        DAF_UNIT,DAF_RECORD,READ_DAF_REC)
      INTEGER (kind=2) ::  IUNIT
      INTEGER (kind=2) ::  IYR
      INTEGER (kind=2) ::  DAY1,DURA_DA,LENGYR_DA,INIT_DA,FINA_DA,IDA,   &
        I2WORD,DAF_RECORD(0:22,30)
      INTEGER (kind=4) ::  DAF_UNIT,IRECORD,I4WORD
      LOGICAL (kind=1) ::  READ_DAF_REC

      IF(DURA_DA<=0) RETURN ! no action is required
      IRECORD=INT4(IUNIT)
      IF(READ_DAF_REC) THEN
        READ(DAF_UNIT,REC=IRECORD,IOSTAT=I4WORD) DAF_RECORD
        IF(I4WORD/=0) STOP 'Error reading DA-file in UPDATE_LIB_MS'
        ! set current year's schedule to default of zeroes
        DO IDA=0,21
          DAF_RECORD(IDA,IYR)=0 ! zero all 16 bits of each word
        END DO
        
        ! set LSb of final word as the 'usable' bit
        DAF_RECORD(22,IYR)=1


      END IF

      INIT_DA=DAY1-1 ! 0-based for speed advantage below
      FINA_DA=INIT_DA+DURA_DA-1

      DO IDA=INIT_DA,FINA_DA
        I2WORD=(MOD(IDA,LENGYR_DA))/16 ! 16 bits per INT*2 element
        DAF_RECORD(I2WORD,IYR)=IOR(   &
        DAF_RECORD(I2WORD,IYR),ISHFT(1,15-MOD(IDA,16)))
      END DO

      WRITE(DAF_UNIT,REC=IRECORD,IOSTAT=I4WORD) DAF_RECORD
      IF(I4WORD/=0) STOP 'Error writing DA-file in UPDATE_LIB_MS'
      END ! SUBROUTINE UPDATE_LIB_MS


      SUBROUTINE ALLOC_WEEKS_MAINT(LENGCM_DA,WEEK_FR,MOFRACT_MT)
      use rollscom
      use week_interp
      use week_interp_i2

      implicit none
      REAL (kind=4) ::  WEEK_FR,MOFRACT_MT(*)
      INTEGER (kind=2) ::  JCM,JCM_PLUS1,LENGCM_DA(*)



      JCM=INTERP_CM(JMO) ! the earlier in the pair of adjacent months
      ADDEND=7.*AMIN1(WEEK_FR,INTERP_WT(JMO)) 
      ! INTERP_WT applies to full week
      MOFRACT_MT(JCM)=MOFRACT_MT(JCM)+ADDEND/FLOAT(LENGCM_DA(JCM))

      ! allocate remainder of days to JCM+1
      IF(INTERP_WT(JMO)<WEEK_FR) THEN 
        JCM_PLUS1=MOD(JCM,12)+1
        MOFRACT_MT(JCM_PLUS1)=   &
        MOFRACT_MT(JCM_PLUS1)+   &
          (7.*WEEK_FR-ADDEND)/FLOAT(LENGCM_DA(JCM_PLUS1))
      END IF
      END ! SUBROUTINE ALLOC_WEEKS_MAINT


      SUBROUTINE ALLOC_MONTHS_MAINT(LENGCM_DA,PSEUDO_MO_FR,MOFRACT_MT)
      use rollscom
      implicit none
      
      REAL (kind=4) ::  PSEUDO_MO_FR,CAL_DAF,DAYS_ALLOC,DAY_MB,DAY_ME,   &
        MOFRACT_MT(*),LENGCM_REM,LENGCM_DAF
      INTEGER (kind=2) ::  JCM,LENGCM_DA(*)

      ! calendar month JCM lags pseudo_mo JMO by at most 1
      JCM=MAX0(JMO-1,1) 
      ! calendar position at which maint. begins
      DAY_MB=FLOAT(JMO-1)*LENGPM_DA 
      IF(DAY_MB>FLOAT(CUMUL_CM_DA(JCM)))   &
        JCM=MOD(JCM,12)+1 ! maint. begins in the next calendar month
      !JCM limits end_pos
      DAY_ME=AMIN1(DAY_MB+LENGPM_DA,FLOAT(CUMUL_CM_DA(JCM))) 
      ! allow the first pass thru the WHILE loop to backfill
      ! tail of month JCM
      LENGCM_DAF=FLOAT(LENGCM_DA(JCM))
      LENGCM_REM=AMIN1(LENGCM_DAF,DAY_ME-DAY_MB)
      ! total allocation in calendar days
      CAL_DAF=PSEUDO_MO_FR*LENGPM_DA 
      ! prior allocations were not all in whole days
      DO WHILE(CAL_DAF>0.) 
        DAYS_ALLOC=AMIN1(CAL_DAF,LENGCM_REM,   &
          (1.-MOFRACT_MT(JCM))*LENGCM_DAF)


        MOFRACT_MT(JCM)=MOFRACT_MT(JCM)+DAYS_ALLOC/LENGCM_DAF
        CAL_DAF=CAL_DAF-DAYS_ALLOC
        JCM=MOD(JCM,12)+1 ! the WHILE loop should eventually be broken
        LENGCM_DAF=FLOAT(LENGCM_DA(JCM))
        LENGCM_REM=LENGCM_DAF ! later passes may fill entire month JCM
      END DO


      END ! SUBROUTINE ALLOC_MONTHS_MAINT
!     Subroutine alloc_maint_bymo moved to clm_decs module
      FUNCTION MMDD_AT(JULIAN_DATE,CUMUL_CM_DA)
      integer (kind=2) :: mmdd_at
      INTEGER (kind=2) ::  JULIAN_DATE,CUMUL_CM_DA(0:24),JCM
!
      DO JCM=1,24 ! 12 is not sufficient for cases that have EoY wrap
        ! with JCM the month at/beyond JULIAN_DATE
        IF(CUMUL_CM_DA(JCM)>=JULIAN_DATE) then
            EXIT 
        end if


      END DO

      MMDD_AT=(1+MOD(JCM-1,12))*100+JULIAN_DATE-CUMUL_CM_DA(JCM-1)
      END function mmdd_at


      FUNCTION RANGE_COVERS(BEG_DATE,END_DATE,JCM)
      INTEGER (kind=2) ::  BEG_DATE,END_DATE,JCM
      logical (kind=1) :: range_covers


      IF(BEG_DATE>END_DATE) THEN ! EoY wrap
        RANGE_COVERS=((BEG_DATE/100<=JCM).AND.(JCM<=12          ))   &
          .OR.       ((1           <=JCM).AND.(JCM<=END_DATE/100))
      ELSE
        RANGE_COVERS=(BEG_DATE/100<=JCM).AND.(JCM<=END_DATE/100)
      END IF
      END


      SUBROUTINE GET_DAY_RANGE_THISMO(JCM,BEG_DAY,END_DAY,LIM_END_DAY,   &
        BEG_MMDD,END_MMDD)
      INTEGER (kind=2) ::  JCM
      INTEGER (kind=2) ::  BEG_DAY,END_DAY,LIM_END_DAY,BEG_MMDD,END_MMDD

!     assign defaults valid if JCM lies in the midst of a multi-month span

!       merge_issue_co = Check:
      ! BEG_DAY= 335/335
      BEG_DAY=1
      ! BEG_DAY= 1/1

      ! JCM= 10/10
      ! BEG_DAY= 1/1
      ! END_DAY= 366/366
      ! LIM_END_DAY= 31/31
      ! BEG_MMDD= 1010/1010
      ! END_MMDD= 1107/1107
      
      
      END_DAY=LIM_END_DAY
      ! merge_issue_co - Check:
      ! END_DAY= 31/31
      ! JCM= 10/10
      ! BEG_MMDD=1010/1010
      IF(JCM==BEG_MMDD/100) then
        BEG_DAY=MOD(BEG_MMDD,100)
        ! BEG_DAY= 10/10
      end if
      ! JCM=10/
      IF(JCM==END_MMDD/100) then
        END_DAY=MOD(END_MMDD,100)

      end if
      END subroutine GET_DAY_RANGE_THISMO


      SUBROUTINE ACCUMULATE_LOADS(PEAK_LOAD_MW,LENGMO_DA)
      use rollscom
      implicit none
      REAL (kind=4) ::  PEAK_LOAD_MW(*)
      INTEGER (kind=2) ::  BEG_DA,END_DA,LENGMO_DA(*)


      LOAD_ACCUM(0)=0.0
      END_DA=0
      DO JMO=1,LENGYR_MO
        ! model load as constant throughout the month
        ADDEND=PEAK_LOAD_MW(JMO) 
        BEG_DA=END_DA
        END_DA=BEG_DA+LENGMO_DA(JMO)
        DO JDA=BEG_DA+1,END_DA ! form the cumulative load array
          LOAD_ACCUM(JDA)=LOAD_ACCUM(JDA-1)+ADDEND
        END DO
      END DO
      END


      INTEGER (kind=2)   &
        FUNCTION OPTIMAL_DAY1(DURATION,INIT_DAOFS,LENGPD_DA,   &
        INF_DIFFER)
      use rollscom
      implicit none
      INTEGER (kind=2) ::  DURATION
      INTEGER (kind=2) ::  INIT_DAOFS,FINAL_DAOFS,LENGPD_DA,DAY1,BDA,EDA
      REAL (kind=4) ::  INF_DIFFER,THIS_DIFF

      INF_DIFFER=1.0E30
      FINAL_DAOFS=INIT_DAOFS+LENGPD_DA-1
      ! preclude overlap into portions of the year excluded for 
      ! presch. maint.
      IF(LENGPD_DA<LENGYR_DA) FINAL_DAOFS=FINAL_DAOFS-DURATION
      DO IDA=INIT_DAOFS,FINAL_DAOFS
        JDA=IDA+DURATION
        ! maint. period would end by Dec. 31 midnight
        IF(JDA<=LENGYR_DA) THEN 
          THIS_DIFF=LOAD_ACCUM(JDA)-LOAD_ACCUM(IDA)

        ELSE 
       ! consider possibility of both INIT_DAOFS & LENGPD_DA ~LENGYR_DA
          BDA=1+MOD(IDA-1,LENGYR_DA)
          EDA=1+MOD(JDA-1,LENGYR_DA)
       IF(BDA<=EDA) THEN ! maint. period is contiguous without EoY wrap
            THIS_DIFF=LOAD_ACCUM(EDA)-LOAD_ACCUM(BDA)
          ELSE ! maint. period would wrap around into January
            THIS_DIFF=LOAD_ACCUM(LENGYR_DA)-LOAD_ACCUM(BDA)+   &
                      LOAD_ACCUM(EDA) ! -LOAD_ACCUM(0) [zero]
          END IF

        END IF

        IF(INF_DIFFER>THIS_DIFF) THEN
          INF_DIFFER=THIS_DIFF ! retain the current min value

          DAY1=IDA ! retain the index of current min value

        END IF

      END DO
      ! result should be on [1,LENGYR_DA]
      OPTIMAL_DAY1=1+MOD(DAY1,LENGYR_DA) 
      END ! FUNCTION OPTIMAL_DAY1

!     FAILREPT.FOR
!     Copyright(c) M.S.Gerber & Associates 2000

!     Created: 12/10/2003 4:59:37 PM
!     Author : GREG TURK
!     Last change: GAT 12/10/2003 4:59:37 PM


      ! Original note: recursive needed only for LF95. 
      ! New notes; Recursive is needed anywhere ENTRY is used or a 
      ! subprogram calls itself.
      recursive subroutine GetFailRepTimes( & !  recursive needed only for LF95
        nFailRepCycles, & !  State 0 is Failure, State 1 is Repaired
        MaxCyclesInOneMonth,   &
        MeanHrsToRepair,TotlHrsInPeriod,HrsToStateParam)
      use spindriftlib
      use prod_arrays_dimensions
	  use end_routine

      real (kind=4) ::  nSigma
      integer (kind=2) ::  MaxSkipped
      ! lower limit on nSkips that imply a problem
      parameter(MaxSkipped=30, & 
        nSigma=3.0) ! plausibility limit on cumulative exp-dist draws
      integer (kind=2) ::  iFRC,nFRC,mFRCpm,MaxCyclesInOneMonth
      integer (kind=4) ::  TotlHrsInPeriod
      real (kind=4) ::  nFailRepCycles,MeanHrsToRepair,r,   &
        MeanTTS(0:1), & !  MTTF,MTTR in hours, respectively
      ! entries cannot access arguments
        HrsToStateParam(0:1,*),HrsToState(0:1,365) 
      real (kind=8) ::  MixRand32

!     declarations for GetTimesToState

      logical (kind=1) ::  Skipped
      integer (kind=2) ::  sFRC,nSkips
      integer (kind=4) ::  iState
      real (kind=4) ::  TargAccum
      real (kind=4) ::  ThisAccum,EdMean,EdSDev,CdMean,CdSDev,Lambda,   &
        MeanCorr,CyclAccum(365)
      save ! without this, recursive causes NDP div 0 error from MTTS==0,
!          ! because recursive makes a separate copy of local vars on the
!          ! stack for each entry below

      mFRCpm=MaxCyclesInOneMonth
      nFRC=INT(nFailRepCycles+0.0001) ! make parameters accessible to 
!                                       entries below;

! truncate the number of cycles so that, e.g., if nFailRepCycles is 7.5 per
! year, then the sum of the fail/repair cycle-times will not exceed the
! TotlHrsInPeriod: 7*(MTTS0+MTTS1)=7*(((THIP/7.5)-MHTR)+MHTR)=THIP*7/7.5;
! when nFailRepCycles is an integer (or slightly larger), then the final
! fail/repair cycle will end just before the scheduled maintenance begins;
! when nFailRepCycles is significantly larger than an integer (e.g. 7.98),
! then the final fail/repair cycle will end almost (MTTS0+MTTS1) earlier.

      if(nFRC>0) then
        MeanTTS(1)=MeanHrsToRepair ! MTTR in hours
        MeanTTS(0)=(float(TotlHrsInPeriod))   &
          /(1.0+nFailRepCycles)-MeanTTS(1) ! MTTF in hours
      ! 1.0 above added 20031202 to account for sFRC=nFRC+1
        ! final TTF, giving one more degree of freedom to compensate 
        ! for that
        ! lost by constraining the average of HoursToState(0,.)

        call GetTimesToState(0) ! times to failure  state, in hours
        call GetTimesToState(1) ! times to repaired state, in hours
        ! ignore the final TTF value, in HoursToState(0,sFRC)
        do iFRC=1,nFRC 
          HrsToStateParam(0,iFRC)=HrsToState(0,iFRC)
          HrsToStateParam(1,iFRC)=HrsToState(1,iFRC)
        end do
      end if
      return ! subroutine GetFailRepTimes


      entry GetTimesToState(iState)

      ! this routine attains the target (TargAccum of TTS) 
      ! exactly with sFRC draws

      sFRC=nFRC ! extra randomizing cycle is not needed for final TTR
      if(iState==0) sFRC=nFRC+1 ! after 20031202, randomize the final
      !  TTF
      EdMean=MeanTTS(iState) ! StdDev=Mean for an exponential 
      ! distribution
      EdSDev=EdMean
      Lambda=1.0/EdMean
      TargAccum=float(sFRC)*EdMean ! e.g. if nFRC==1 make 2 TTF draws


      ! whose sum exactly equals TargAccum, then ignore the second 
      ! (final) TTF
      if(sFRC>nFRC) TargAccum=TargAccum+MeanTTS(1) 
      ! to end sFRC cycle in failure-state


      do ! execute to the end of this entry
      ThisAccum=0.0
      nSkips=0
      iFRC=1
      do while(iFRC<=sFRC)
        if(nSkips>=max0(sFRC,MaxSkipped)) then
      ! too many draws rejected for randomness
          er_message="clmobjt:0001 - GetTimesToState:  " // &
             "increase max # FO/month"
     
            call end_program(er_message)
        end if
        
      ! deviates in exponential dist of TTS have uniform dist of 
      ! their antilogs, i.e., exp-dist deviates are obtainted by 
      ! taking logs of uniform deviates
        r=sngl(MixRand32())

        HrsToState(iState,iFRC)=-log(r)/Lambda
        ThisAccum=ThisAccum+HrsToState(iState,iFRC)
! reject draw as implausible or 'too wierd' if ThisAccum is an nSigma
! outlier, i.e. lies outside [CdMean-2*CdSDev,CdMean+2*CdSDev]

        ! mean of sum of RV grows linearly with n
        CdMean=EdMean*float(iFRC) 
        ! variance of sum grows linearly with n
        CdSDev=EdSDev*(float(iFRC))**0.5 

        Skipped=.false.
         ! check plausibility of TTS
        if(abs(ThisAccum-CdMean)>nSigma*CdSDev) then

          Skipped=.true.
          ! check feasibility of TTF draws
        elseif((iState==0).and.(iFRC>=mFRCpm)) then 
           ! 744=24*31
          if(ThisAccum-CyclAccum(iFRC-(mFRCpm-1))<744.0) then
! difference in HrsToState implies excessive outages in 1 month;
          ! later addition of TTR makes the test above conservative


            Skipped=.true.
          end if
        end if

        if(Skipped) then ! repeat the draw for iFRC
          nSkips=nSkips+1
          ThisAccum=ThisAccum-HrsToState(iState,iFRC)
          cycle
        end if

!       save confirmed accum at iFRC for reference above at later iFRC

        CyclAccum(iFRC)=ThisAccum
        iFRC=iFRC+1
      end do

      MeanCorr=TargAccum/ThisAccum ! equivalent to a bias-shift in logs

      ThisAccum=0.0
      do iFRC=1,nFRC ! second pass to correct the mean (and recheck)
        HrsToState(iState,iFRC)=HrsToState(iState,iFRC)*MeanCorr
        ThisAccum=ThisAccum+HrsToState(iState,iFRC)

        ! mean of sum of RV grows linearly with n
        CdMean=EdMean*float(iFRC) 
        
        ! variance of sum grows linearly with n
        CdSDev=EdSDev*(float(iFRC))**0.5 
        ! check plausibility of TTS
        if(abs(ThisAccum-CdMean)>nSigma*CdSDev) then 
        
          ! TODO: Consider reworking so that EXIT isn't needed.
          exit ! the mean-corrected path is implausible
          
          ! check feasibility of TTF draws
        elseif((iState==0).and.(iFRC>=mFRCpm)) then 
          ! 744=24*31
          if(ThisAccum-CyclAccum(iFRC-(mFRCpm-1))<744.0) then 
! difference in HrsToState implies excessive outages in 1 month;
          ! later addition of TTR makes the test above conservative


            exit ! the mean-corrected path is infeasible
          end if
        end if
        CyclAccum(iFRC)=ThisAccum
      end do ! iFRC-loop

      if(iFRC>nFRC) exit ! with HrsToState mean-corrected and validated


      end do ! infinite loop broken when the loop above finishes (iFRC>nFRC)
      return ! entry GetTimesToState


!-----
      end ! subroutine GetFailRepTimes

!-----
!
!
