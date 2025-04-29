      MODULE calendar

	  USE hourlyproductsdata

	  implicit none
	  contains
	    
!***********************************************************************
      SUBROUTINE MakingCalendarAdjustments(Calendar_Year,FileYear, &
                                           ValuesHourlyByMonth, &
                                           ValuesByMoDa,Normalize)
!***********************************************************************
         LOGICAL (KIND=1) :: ADJUST_FOR_LEAP_YEAR
         LOGICAL (KIND=4) :: Normalize
         LOGICAL (KIND=4) :: AdjustValues
         REAL (KIND=4) :: SumOfValuesFor(12),ValuesByMoDa(24,12,31), &
                          CurValuesByMoDa(24,12,31),ScaleFactor, &
                          ValuesHourlyByMonth(744,12)
         INTEGER (KIND=2) :: RefDay,RefMo,RefDoWk,CurDoWk,CurDaysInMonth
         INTEGER (KIND=2) :: FileYear,Calendar_Year, &
                             DAYS_IN_EACH_MONTH, &
                             Month,CurrentHr,Day,HrsInMo

            ADJUST_FOR_LEAP_YEAR = MOD(Calendar_Year-1964.,4.) < .001
            CALL DAYWEEK(1_2,2_2,FileYear,RefDoWk)
            CALL DAYWEEK(1_2,2_2,Calendar_Year,CurDoWk)
            IF(RefDoWk > CurDoWk) THEN
               RefDay = 7- (RefDoWk - CurDoWk) + 2
            ELSE
               RefDay = CurDoWk - RefDoWk + 2
            ENDIF
            RefMo = 1

!            WRITE(39,*) " Source Year",FileYear,"Run Year",Calendar_Year
!            WRITE(39,*)&
!                      "SourceMonth,SourceDay,CalendarMonth,CalendarDay,"
            SumOfValuesFor = 0. 
            DO Month = 1, 12
!               SumOfValuesFor(Month) = SUM(ValuesHourlyByMonth(:,Month))
               CurrentHr = 1
               IF(Month == 2) THEN
                  CurDaysInMonth = 28
                  IF(ADJUST_FOR_LEAP_YEAR) CurDaysInMonth = 29
               ELSE
                  CurDaysInMonth = DAYS_IN_EACH_MONTH(Month)
               ENDIF
               HrsInMo = 24*CurDaysInMonth
               SumOfValuesFor(Month) = &
                               SUM(ValuesHourlyByMonth(1:HrsInMo,Month))
               ValuesHourlyByMonth(:,Month) = 0.
               DO Day = 1, CurDaysInMonth
                  IF(Month == 1 .AND. Day == 1) THEN
!                     WRITE(39,*)"1 , 1 , 1 , 1 ,"
                     CurValuesByMoDa(1:24,1,1) = ValuesByMoDa(1:24,1,1)
                     ValuesHourlyByMonth(1:24,Month) = &
                                               CurValuesByMoDa(1:24,1,1)
                     CurrentHr = 25
                     CYCLE
                  ENDIF
!                  WRITE(39,*) RefMo,",",RefDay,",",Month,",",Day,","
                  CurValuesByMoDa(1:24,Month,Day) = &
                                         ValuesByMoDa(1:24,RefMo,RefDay)
                  RefDay = RefDay + 1
                  IF(RefDay > DAYS_IN_EACH_MONTH(RefMo)) THEN
                     IF(RefMo+1 > 12) THEN
                        RefMo = 12 
                        RefDay = RefDay - 7 ! need to realine Days
                     ELSE
                        RefMo = RefMo + 1
                        RefDay = 1
                     ENDIF
                  ENDIF
                  
                  ValuesHourlyByMonth(CurrentHr:CurrentHr+23,Month) =  &
                                         CurValuesByMoDa(1:24,Month,Day)
                  CurrentHr = CurrentHr + 24
               ENDDO
               AdjustValues = .TRUE.
               AdjustValues = Normalize
               IF(AdjustValues) THEN
                  ScaleFactor = SumOfValuesFor(Month)/ &
                      SUM(ValuesHourlyByMonth(:,Month))
                                     
                  ValuesHourlyByMonth(:,Month) = ScaleFactor *  &
                                            ValuesHourlyByMonth(:,Month)
               ENDIF
               ScaleFactor = SUM(ValuesHourlyByMonth(:,Month))
            ENDDO
      END SUBROUTINE  
	  end module calendar