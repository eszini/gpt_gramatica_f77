      MODULE HourlyProductsData
      implicit none
         CHARACTER (LEN=256), ALLOCATABLE :: HourlyProdFileNames(:)
         INTEGER (KIND=2), ALLOCATABLE :: HourlyProdRefNumber(:,:)
         REAL (KIND=4), ALLOCATABLE :: HourlyProductValue(:,:,:), &
                                       HourlyCFProductValues(:,:), &
                                       MONTHLY_HOURLY_CF(:)
         INTEGER (KIND=2), ALLOCATABLE :: HourlyProductFilePtr(:), & ! 1:NUM_TRANSACTIONS
                                          HourlyProdPtrToCFLoc(:), & ! 1:NUM_TRANSACTIONS
                                          HourlyProdPtrCFLocToProd(:)! 1:NUM_TRANSACTIONS
         INTEGER (KIND=4) :: ActiveProdFiles,NumOfHourlyCFProducts
         INTEGER (KIND=2) :: SMLoadShapeYears(31), &
                             WindLoadShapeYears(31), &
                             SolarLoadShapeYears(31), &
                             LMP_LoadShapeYears(31)
         LOGICAl (KIND=1) :: UseSMLSYears,UseWindLSYears, &
                             UseSolarLSYears, &
                             UseLMP_LSYears,UseLoadLSYears
         CHARACTER (LEN=2), ALLOCATABLE :: RefUsed(:)
         INTERFACE 
            SUBROUTINE MakingCalendarAdjustments( &
                                           Calendar_Year,FileYear, &
                                           ValuesHourlyByMonth, &
                                           ValuesByMoDa,Normalize)
               LOGICAL (KIND=4), INTENT(IN), OPTIONAL :: Normalize
               REAL (KIND=4) :: ValuesByMoDa(24,12,31), &
                                ValuesHourlyByMonth(744,12)
               INTEGER (KIND=2) :: FileYear,Calendar_Year
            END SUBROUTINE      
         END INTERFACE
      END MODULE HourlyProductsData