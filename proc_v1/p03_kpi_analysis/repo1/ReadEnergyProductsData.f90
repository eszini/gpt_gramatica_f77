      MODULE ReadEnergyProductsData
         CHARACTER (LEN=1), ALLOCATABLE :: PRODUCT_ACTIVE(:)
         INTEGER (KIND=2), ALLOCATABLE :: MONTHLY_TRANS_HOURS(:,:), &
                                          HYBRID_BATTERY(:), &
                                          BATTERY(:), &
                                          ANNUAL_HYBRID_BATTERY(:), &
                                          ANNUAL_HYBRID_INDEX(:), &
                                          HYBRID_INDEX(:), &
                                          BEGIN_EP(:), &
                                          END_EP(:), &
                                          BEGIN_DAY(:), &
                                          END_DAY(:)
         INTEGER (KIND=2) :: STORAGE_TRANS_HOURS(12),INDEP_POINTER
         REAL (KIND=4), ALLOCATABLE, save :: MONTHLY_ENERGY_MULT(:), &
                                       ENERGY_PRICE(:), &
                                       MONTHLY_ENERGY_PRICE(:), &
                                       PUMPING_CAPACITY(:), &
                                       PUMPING_STORAGE_EFFICIENCY(:), &
                                       PROPOSED_QUANT_OF_PRODUCT(:), &
                                       DailyStorageDischargeLimit(:), &
                                       DAILY_PUMPING_MULT(:), &
                                       HOURLY_QUANTITY(:), &
                                       ENERGY_PRICE_MULTIPLIER(:), &
                                       MONTHLY_ENERGY(:,:), &
                                       MONTHLY_CHARGE(:,:), &
                                       MONTHLY_ENERGY_REVENUE(:,:), &
                                       MONTHLY_ENERGY_COST(:,:), &
                                       RenewableEnergyPercent(:)
         REAL (KIND=4) :: &
                                       STORAGE_ENERGY(12), &
                                       STORAGE_CHARGE(12), &
                                       STORAGE_ENERGY_COST(12), &
                                       STORAGE_ENERGY_REVENUE(12), &
                                       INDEP_ENERGY_COST(12), &
                                       INDEP_ENERGY_REVENUE(12), &
                                       INDEP_ENR_FOR_EXP(12), &
                                       INDEP_ENR_FOR_REV(12)
      END MODULE ReadEnergyProductsData
