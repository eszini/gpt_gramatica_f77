module clm_decs
implicit none
contains


      SUBROUTINE ALLOC_MAINT_BYMO(LENGCM_DA,LENGMO_DA,CUMUL_MO_DA,   &
        DAY1,MRI,MRF,MOFRACT_MT)
      use rollscom
      use logging
      implicit none

      INTEGER (kind=2) ::  LENGCM_DA(*),LENGMO_DA(*),CUMUL_MO_DA(0:*),   &
        DAY1,MRI,SDO,DAYS_REM,DAYS_ALLOC,LENG_THISMO
      REAL (kind=4) ::  MRF,DAYS_REF
      real (kind=4), intent(inout) :: MOFRACT_MT(12)
      


      IDA=DAY1 ! avoid corrupting the caller's values
      DAYS_REM=MRI
      DAYS_REF=MRF
      JMO=1
      ! express maintenance "days" as fractions of months
      DOWHILE(DAYS_REM>0) 
        ! First call -
        ! JMO=1/1
        JDA=CUMUL_MO_DA(JMO)
        ! JDA=31/31
        ! IDA=29/47
        IF(IDA<=JDA) THEN ! start date was within this "month"
          LENG_THISMO=LENGMO_DA(JMO)
          ! start day offset within this "month"
          SDO=IDA-1-CUMUL_MO_DA(JMO-1) 

          ! First call -
          ! SDO=28/15
          ! DAYS_REM=62/62
          ! LENG_THISMO=31/28
          IF(SDO+DAYS_REM<=LENG_THISMO) THEN ! end date was in this "month"
            !
            IF(LENGYR_DA==365) THEN ! put all # days into this month
              MOFRACT_MT(JMO)=   &
              MOFRACT_MT(JMO)+DAYS_REF/FLOAT(LENG_THISMO)
            ELSE IF(LENGYR_DA==52) THEN
!             allocate week's days to the pair of spanning months
              CALL ALLOC_WEEKS_MAINT(LENGCM_DA,DAYS_REF,MOFRACT_MT)

            ELSE
!              allocate "month"'s days to 3 possible spanning months,
!             required since each "month" indexed JMO has 30.417 days


              ! Before call:
              !    lengcm_da=/31
              !    days_ref=/6.1390032E+001
              !    mofract_mt=/0
              CALL ALLOC_MONTHS_MAINT(LENGCM_DA,DAYS_REF,MOFRACT_MT)
              !    lengcm_da=/31
              !    days_ref=/40.839 ...
              !    mofract_mt-/0
            END IF
            EXIT ! from DO loop, as allocation is finished
          ELSE
         ! end date was in later "month"; advance by # "days" after SDO
         ! First time through....
         ! LENG_THISmo=/28
         ! SDO=/15
         ! DAYS_REM=/62
            DAYS_ALLOC=LENG_THISMO-SDO
            IDA     =IDA     +DAYS_ALLOC
            DAYS_REM=DAYS_REM-DAYS_ALLOC
            DAYS_REF=DAYS_REF-FLOAT(DAYS_ALLOC)

            IF(LENGYR_DA==365) THEN ! put DAYS_ALLOC days into this month

              MOFRACT_MT(JMO)=   &
              MOFRACT_MT(JMO)+FLOAT(DAYS_ALLOC)/FLOAT(LENG_THISMO)


            ELSE IF(LENGYR_DA==52) THEN
            ! allocate week's 1 "day" to the pair of spanning months
              CALL ALLOC_WEEKS_MAINT(LENGCM_DA,1.,MOFRACT_MT)
        ELSE ! allocate "month"'s 1 "day" to 3 possible spanning months
              CALL ALLOC_MONTHS_MAINT(LENGCM_DA,1.,MOFRACT_MT)
            END IF
          END IF
        END IF

        IF(JMO<LENGYR_MO) THEN
          JMO=JMO+1 !debugstop
        ELSE ! wrap around to the beginning of the year
          JMO=1
          IDA=1
        END IF

      END DO
      END SUBROUTINE ALLOC_MAINT_BYMO



end module clm_decs