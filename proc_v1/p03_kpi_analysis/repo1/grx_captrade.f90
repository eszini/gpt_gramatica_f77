module grx_captrade
use grx_data
use co2_cap_n_trade
use grx_extend
use grx_planning_routines
use co2_data
use globecom
implicit none

contains
      logical function abs_error(a,b,delta) result(are_equal)
      implicit none
      real (kind=4) :: a,b,check
      real (kind=4), optional :: delta
      if(present(delta)) then
         check = delta
      else
         check = .001
      endif
      are_equal = abs(a-b)/abs(a) <= check
      end function abs_error

      real function grx_co2_bank_delta(year)
      use co2_cap_n_trade
      integer (kind=2) :: year
         if(year <= yr2_data .and. year >= yr1_data .and. &
         co2_param_file_exists) then
            grx_co2_bank_delta = 10**6*co2_banking(year)
         else
            grx_co2_bank_delta = 0.
         endif
      end function grx_co2_bank_delta
      real function grx_abatement_price(co2_abatements,year)
         
         use co2_cap_n_trade
         integer (kind=2) :: year,i
         real (kind=4) :: co2_price,co2_abatements
         real (kind=4) :: co2_abatements_at_price
         real (kind=4) :: save_co2_abatements,max_abatements_avail
!
         save_co2_abatements = co2_abatements
         max_abatements_avail = 10**6*(non_elect_max_reductions(year) &
                                     + dom_max_off_sets_to_buy(year) &
                                     + int_max_off_sets_to_buy(year))
         if(co2_abatements <= 0.) then
            co2_price = 0.
         elseif(co2_abatements < max_abatements_avail) then
            co2_price = 5.
            do while (grx_co2_intrl_abatements_av(year, co2_price) &
                < co2_abatements)
               co2_price = co2_price + 5
            enddo
            co2_price = co2_price - 5
            do while (grx_co2_intrl_abatements_av(year, co2_price) &
                < co2_abatements)
               co2_price = co2_price + 1
            enddo
            co2_price = co2_price - 1
            do while (grx_co2_intrl_abatements_av(year, co2_price) &
                < co2_abatements)
               co2_price = co2_price + .1
            enddo
            co2_price = co2_price - .1
            do while (grx_co2_intrl_abatements_av(year, co2_price) &
                < co2_abatements)
               co2_price = co2_price + .01
            enddo
            co2_price = co2_price - .005

         else
            co2_price = co2_max_price_cap(year)
            co2_abatements = max_abatements_avail
         endif
         grx_abatement_price = co2_price
      end function grx_abatement_price
      
      real function GRX_CO2_ABATEMENTS_AVAILABLE(year, co2_price)
      real (kind=4) :: co2_price
      real :: rco2_price
      integer (kind=2) :: year
      
      rco2_price=real(co2_price)
      
         if(co2_market_available(year)) then
            GRX_CO2_ABATEMENTS_AVAILABLE = &
            grx_co2_other_abatements_av(year, rco2_price) &
        + grx_co2_domisic_abatements_av(year, rco2_price) &
                + grx_co2_intrl_abatements_av(year, rco2_price)
     GRX_CO2_ABATEMENTS_AVAILABLE = 10**6*GRX_CO2_ABATEMENTS_AVAILABLE
         else
            GRX_CO2_ABATEMENTS_AVAILABLE = 0.
         endif

      end function grx_co2_abatements_available
      FUNCTION CHECK_FOR_GRX_ABATEMENT_CONVERG(TG)
        use globecom
        use mod_base_year
        implicit none
        REAL (KIND=4), SAVE :: ITER_CO2_PRICE(0:20), &
            ITER_CO2_ELEC_EMISS(0:20)
        REAL (KIND=4), SAVE  :: ABOVE_PRICE,ABOVE_EMISS,BELOW_PRICE, &
            BELOW_EMISS
        LOGICAL (KIND=4) :: CHECK_FOR_GRX_ABATEMENT_CONVERG
        LOGICAL (KIND=4), SAVE :: MaxPriceCapHit, MinPriceHit, &
            BELOW_NOT_FOUND
        INTEGER (KIND=2) :: TG,I,NumUnits
        REAL (KIND=4) :: &
                         CO2_EMISSIONS_CAP, CO2_EMISSIONS,CAP_ERROR, &
                         EMISSION_ADJ,ADJ_SLOPE
        REAL (KIND=4), SAVE :: LAST_PRICE/0./,CURRENT_PRICE/0./, &
                                LAST_CO2_EMISS/0./, &
                                CURRENT_CO2_EMISS/0./
        CHARACTER (LEN=2) CAPACITY_PLANNING_METHOD,GREEN_MRX_METHOD
        REAL (KIND=4) :: GRX_CO2_MARKET_TONS,GRX_CO2_MARKET_PRICE, &
            MARKET_REDUCTION,MAX_ABATEMENTS_AVAIL
        REAL (KIND=4), SAVE :: LOW_PRICE_VALUE/9999./, &
            HIGH_PRICE_VALUE/0./, &
                               CO2_ABATEMENTS_NEEDED_LAST_ITER
        REAL (KIND=4) :: CO2_ELECTRIC_EMISSION, &
            CO2_NON_ELECTRIC_EMISSION, &
                         CO2_ABATEMENTS_NEEDED, &
                         CO2_NET_EMISSIONS, &
                         CO2_BANK_CREDITS_USED, &
                         CO2_BANK_CREDITS_PURCHASED, &
                         CO2_BANK_CAP_ADJUSTMENT,CO2_SHORT_FALL, &
                         CO2_SHORT_FALL_COVERED_BY_BANK
               
          CHECK_FOR_GRX_ABATEMENT_CONVERG = .FALSE.
          IF(.NOT. (CAPACITY_PLANNING_METHOD() == 'MX'  .AND.   &
                                  GREEN_MRX_METHOD() == 'GX') .OR.  &
                                         globecom_year == 1) THEN
             CHECK_FOR_GRX_ABATEMENT_CONVERG = .TRUE.
             GRX_CONVERGED = .TRUE.
             RETURN
          ENDIF
          
          GRX_CONVERGED = .FALSE.

          CO2_EMISSIONS_CAP = GRX_ALL_SECTOR_CO2_EMISS_CAP &
            (get_base_year()+globecom_YEAR)
          CO2_ELECTRIC_EMISSION = GET_CO2_EMISS_FOR_SYSTEM()
          CO2_NON_ELECTRIC_EMISSION = &
          grx_non_elec_sector_co2_emss(get_base_year()+globecom_YEAR)

          CO2_EMISSIONS = CO2_ELECTRIC_EMISSION &
                          + CO2_NON_ELECTRIC_EMISSION
! bank credits used increase the co2 cap
! bank credits purchased are added at the end at the cost of the credits
! bought to meet cap
! NOTE: version 1.0 of banking dated 12/11/09 does not check for max and
! min credit market limits on bank purchases
          CO2_BANK_CREDITS_USED = &
          ABS(MIN(0,GRX_CO2_BANK_DELTA(get_BASE_YEAR()+globecom_YEAR))) 
          ! (  negative term when bank is used for offset. )
          
          CO2_BANK_CREDITS_PURCHASED = &
          MAX(0,GRX_CO2_BANK_DELTA(get_BASE_YEAR()+globecom_YEAR))
          ! (  positive term when bank purchases offsets )
          CO2_BANK_CAP_ADJUSTMENT = &
          GRX_CO2_BANK_DELTA(get_BASE_YEAR()+globecom_YEAR) 
          ! ( positive term when bank purchases offsets)
          
          MAX_ABATEMENTS_AVAIL = 0.
          IF(CO2_Market_Available(get_BASE_YEAR()+ globecom_YEAR)) then
            MAX_ABATEMENTS_AVAIL = 10**6*&
             (Non_Elect_Max_Reductions(get_BASE_YEAR()+globecom_YEAR) &
             + Dom_Max_Off_Sets_To_Buy(get_BASE_YEAR()+globecom_YEAR) &
               + Int_Max_Off_Sets_To_Buy(get_BASE_YEAR()+globecom_YEAR))
          end if
          CURRENT_CO2_EMISS = CO2_EMISSIONS - CO2_BANK_CREDITS_USED
          CURRENT_PRICE = CURRENT_CO2_DISPATCH_COST
          write(9,*) "in GRX conver"
          write(9,*) "Emiss CAP", CO2_EMISSIONS_CAP
          write(9,*) "Emiss Prod", CO2_EMISSIONS
          write(9,*) "Current Price", CURRENT_CO2_DISPATCH_COST
          
          ! TODO:  This is believed to be debugging cpde.  Once a
          ! "good" binary has been created, look into getting rid of
          ! this IF block.
          IF(get_BASE_YEAR()+globecom_year == 2023) then
             write(9,*) "at 2018"
          endif
          IF(GRX_ITERATIONS == 0) THEN
            ITER_CO2_PRICE = 0.
            ITER_CO2_ELEC_EMISS = 0.
            ABOVE_PRICE = 0.
            ABOVE_EMISS = 0.
            BELOW_PRICE = 0.
            BELOW_EMISS = 0.
            BELOW_NOT_FOUND = .TRUE.
          ENDIF
          ITER_CO2_PRICE(GRX_ITERATIONS) = CURRENT_CO2_DISPATCH_COST
          ITER_CO2_ELEC_EMISS(GRX_ITERATIONS) = CO2_ELECTRIC_EMISSION
          IF(CO2_Price_Evaluation(get_BASE_YEAR()+globecom_YEAR)) THEN
            IF(GRX_ITERATIONS < CO2_Min_GRX_Iterations( &
                get_BASE_YEAR()+globecom_YEAR)) THEN
               NEXT_CURRENT_CO2_DISPATCH_COST = CO2_Price_Forecast( &
               get_BASE_YEAR()+globecom_YEAR)
            ELSE ! if(GRX_ITERATIONS == 3.) THEN
               CHECK_FOR_GRX_ABATEMENT_CONVERG = .TRUE.
               GRX_CONVERGED = .TRUE.
               RETIRED_OPTIONS_PNTR = 0
               CALL GRX_ABATEMENT_MARKET_DATA_RPT()
            ENDIF
            CALL GRX_ABATE_ITER_RESULTS()
            RETURN
          ENDIF
          IF(GRX_ITERATIONS == 0) THEN
             CO2_ABATEMENTS_NEEDED_LAST_ITER = CO2_EMISSIONS - &
             CO2_BANK_CREDITS_USED &
                      - CO2_EMISSIONS_CAP + CO2_BANK_CREDITS_PURCHASED
             CO2_ABATEMENTS_NEEDED_LAST_ITER = 1.1 * &
             CO2_ABATEMENTS_NEEDED_LAST_ITER
        IF(CO2_Market_Available(get_BASE_YEAR()+globecom_YEAR)) THEN
           NEXT_CURRENT_CO2_DISPATCH_COST = &
           MAX(CO2_Min_Price(get_BASE_YEAR()+globecom_YEAR), &
              GRX_ABATEMENT_PRICE( &
              CO2_ABATEMENTS_NEEDED_LAST_ITER,get_BASE_YEAR()+ &
				globecom_YEAR))
           IF(CURRENT_CO2_EMISS > CO2_EMISSIONS_CAP) THEN
              ABOVE_PRICE = CURRENT_CO2_DISPATCH_COST
              ABOVE_EMISS = CURRENT_CO2_EMISS
           ELSE
              BELOW_PRICE = CURRENT_CO2_DISPATCH_COST
              BELOW_EMISS = CURRENT_CO2_EMISS
           ENDIF
             ELSE
               IF(CURRENT_CO2_EMISS > CO2_EMISSIONS_CAP) THEN
                 NEXT_CURRENT_CO2_DISPATCH_COST = &
                 (CO2_Min_Price(get_BASE_YEAR()+globecom_YEAR)  &
             + CO2_Max_Price_Cap(get_BASE_YEAR()+globecom_YEAR))/2.
                 ABOVE_PRICE = CURRENT_CO2_DISPATCH_COST
                 ABOVE_EMISS = CO2_ELECTRIC_EMISSION
               ELSE
                 NEXT_CURRENT_CO2_DISPATCH_COST = &
                 CO2_Min_Price(get_BASE_YEAR()+globecom_YEAR)
                 BELOW_PRICE = CURRENT_CO2_DISPATCH_COST
                 BELOW_EMISS = CO2_ELECTRIC_EMISSION
               ENDIF
             ENDIF
             CALL GRX_ABATE_ITER_RESULTS()
             LAST_PRICE = CURRENT_CO2_DISPATCH_COST
             LAST_CO2_EMISS = CO2_EMISSIONS - CO2_BANK_CREDITS_USED
             CURRENT_PRICE = NEXT_CURRENT_CO2_DISPATCH_COST
             MaxPriceCapHit = .FALSE.
             MinPriceHit = .FALSE.
            RETURN
          ENDIF
          IF(.NOT. CO2_Market_Available(get_BASE_YEAR()+ &
			globecom_YEAR)) THEN
               IF(CURRENT_CO2_EMISS > CO2_EMISSIONS_CAP) THEN
                  ABOVE_PRICE = CURRENT_CO2_DISPATCH_COST
                  ABOVE_EMISS = CO2_ELECTRIC_EMISSION
                  IF(GRX_ITERATIONS == 1) THEN
                     NEXT_CURRENT_CO2_DISPATCH_COST = &
                     CO2_Max_Price_Cap(get_BASE_YEAR()+globecom_YEAR)
                  ELSEIF(CURRENT_CO2_DISPATCH_COST == &
                  CO2_Max_Price_Cap(get_BASE_YEAR()+globecom_YEAR)) THEN
                 NEXT_CURRENT_CO2_DISPATCH_COST = &
                 CO2_Max_Price_Cap(get_BASE_YEAR()+globecom_YEAR)
                 GRX_CONVERGED = GRX_ITERATIONS >= &
                 CO2_Min_GRX_Iterations(get_BASE_YEAR()+globecom_YEAR)
                  ELSE
                     IF(ABS_ERROR(CO2_EMISSIONS_CAP,&
                     CURRENT_CO2_EMISS,DELTA=.01) .AND. &
                            GRX_ITERATIONS >= CO2_Min_GRX_Iterations( &
                            get_BASE_YEAR()+globecom_YEAR)) THEN
                        NEXT_CURRENT_CO2_DISPATCH_COST = &
                        CURRENT_CO2_DISPATCH_COST
                        GRX_CONVERGED = .TRUE.
                     ELSE
                        NEXT_CURRENT_CO2_DISPATCH_COST = (BELOW_PRICE &
                        + ABOVE_PRICE)/2.
                     ENDIF
                  ENDIF
               ELSE
                  BELOW_PRICE = CURRENT_CO2_DISPATCH_COST
                  BELOW_EMISS = CO2_ELECTRIC_EMISSION
                  IF(CURRENT_CO2_DISPATCH_COST == &
                  CO2_Min_Price(get_BASE_YEAR()+globecom_YEAR)) THEN
                     NEXT_CURRENT_CO2_DISPATCH_COST = &
                     CO2_Min_Price(get_BASE_YEAR()+globecom_YEAR)
                     GRX_CONVERGED = GRX_ITERATIONS >= &
                  CO2_Min_GRX_Iterations(get_BASE_YEAR()+globecom_YEAR)
                  ELSE
                     IF(ABS_ERROR(CO2_EMISSIONS_CAP, &
          CURRENT_CO2_EMISS,DELTA=.01) .AND. &
                 GRX_ITERATIONS >= &
             CO2_Min_GRX_Iterations(get_BASE_YEAR()+globecom_YEAR)) THEN
             NEXT_CURRENT_CO2_DISPATCH_COST = &
             CURRENT_CO2_DISPATCH_COST
             GRX_CONVERGED = .TRUE.
                     ELSE
                        NEXT_CURRENT_CO2_DISPATCH_COST = &
                        (BELOW_PRICE + ABOVE_PRICE)/2.
                     ENDIF
                  ENDIF
               ENDIF
               LAST_CO2_EMISS = CURRENT_CO2_EMISS
               LAST_PRICE = CURRENT_CO2_DISPATCH_COST
               CALL GRX_ABATE_ITER_RESULTS()
               IF(GRX_CONVERGED) RETIRED_OPTIONS_PNTR = 0
               IF(GRX_CONVERGED) CALL GRX_ABATEMENT_MARKET_DATA_RPT()
               CHECK_FOR_GRX_ABATEMENT_CONVERG = GRX_CONVERGED
               RETURN
          ENDIF
          IF(CO2_Market_Available(get_BASE_YEAR()+globecom_YEAR)) THEN
               CURRENT_CO2_EMISS =  CURRENT_CO2_EMISS &
                        - grx_co2_intrl_abatements_av( &
              get_BASE_YEAR()+globecom_YEAR, CURRENT_CO2_DISPATCH_COST)
               IF(CURRENT_CO2_EMISS > CO2_EMISSIONS_CAP) THEN
                  ABOVE_PRICE = CURRENT_CO2_DISPATCH_COST
                  ABOVE_EMISS = CO2_ELECTRIC_EMISSION
                  IF(ABS_ERROR(CO2_EMISSIONS_CAP,CURRENT_CO2_EMISS, &
                  DELTA=.01) .AND. &
        GRX_ITERATIONS >= CO2_Min_GRX_Iterations(get_BASE_YEAR()+ &
			globecom_YEAR)) THEN
                     NEXT_CURRENT_CO2_DISPATCH_COST = &
                     CURRENT_CO2_DISPATCH_COST
                     GRX_CONVERGED = .TRUE.
                  ELSE
                     IF(GRX_ITERATIONS == 1 .OR. BELOW_NOT_FOUND) THEN
           CO2_ABATEMENTS_NEEDED_LAST_ITER = &
           CO2_EMISSIONS - CO2_BANK_CREDITS_USED &
         - CO2_EMISSIONS_CAP + CO2_BANK_CREDITS_PURCHASED
           CO2_ABATEMENTS_NEEDED_LAST_ITER = 1.05 * &
           CO2_ABATEMENTS_NEEDED_LAST_ITER
           NEXT_CURRENT_CO2_DISPATCH_COST = &
           MAX(CO2_Min_Price(get_BASE_YEAR()+globecom_YEAR), &
               GRX_ABATEMENT_PRICE(&
         CO2_ABATEMENTS_NEEDED_LAST_ITER,get_BASE_YEAR()+globecom_YEAR))
                     ELSEIF(CURRENT_CO2_DISPATCH_COST == &
                  CO2_Max_Price_Cap(get_BASE_YEAR()+globecom_YEAR)) THEN
                        NEXT_CURRENT_CO2_DISPATCH_COST = &
                        CO2_Max_Price_Cap(get_BASE_YEAR()+globecom_YEAR)
                        GRX_CONVERGED = GRX_ITERATIONS >= &
               CO2_Min_GRX_Iterations(get_BASE_YEAR()+globecom_YEAR)
                     ELSE
                        NEXT_CURRENT_CO2_DISPATCH_COST = &
                        (BELOW_PRICE + ABOVE_PRICE)/2.
                     ENDIF
                  ENDIF
               ELSE
                  BELOW_PRICE = CURRENT_CO2_DISPATCH_COST
                  BELOW_EMISS = CO2_ELECTRIC_EMISSION
                  BELOW_NOT_FOUND = .FALSE.
                  IF(CURRENT_CO2_DISPATCH_COST == &
                  CO2_Min_Price(get_BASE_YEAR()+globecom_YEAR)) THEN
                     NEXT_CURRENT_CO2_DISPATCH_COST = &
                     CO2_Min_Price(get_BASE_YEAR()+globecom_YEAR)
                     GRX_CONVERGED = GRX_ITERATIONS >= &
            CO2_Min_GRX_Iterations(get_BASE_YEAR()+globecom_YEAR)
                  ELSE
                     NEXT_CURRENT_CO2_DISPATCH_COST = &
                     (BELOW_PRICE + ABOVE_PRICE)/2.
                  ENDIF
               ENDIF
               LAST_CO2_EMISS = CURRENT_CO2_EMISS
               LAST_PRICE = CURRENT_CO2_DISPATCH_COST
               CALL GRX_ABATE_ITER_RESULTS()
               IF(GRX_CONVERGED) RETIRED_OPTIONS_PNTR = 0
               IF(GRX_CONVERGED) CALL GRX_ABATEMENT_MARKET_DATA_RPT()
               CHECK_FOR_GRX_ABATEMENT_CONVERG = GRX_CONVERGED
               RETURN
          ENDIF
        END FUNCTION CHECK_FOR_GRX_ABATEMENT_CONVERG
        FUNCTION CHECK_FOR_GRX_CONVERGENCE(TG)
        USE co2_market_abatement_curves
        use eco
        use mod_base_year
        implicit none
        LOGICAL (KIND=4) :: CHECK_FOR_GRX_CONVERGENCE,OneMoreIter
        INTEGER (KIND=2) :: TG,I
        REAL (KIND=4) :: GET_EMISS_CAP_FOR_CLASS, &
                         CO2_EMISSIONS_CAP, CO2_EMISSIONS,CAP_ERROR, &
                         ADD_MARKET_PURCHASES

        LOGICAL (KIND=1) :: GET_CO2_RETIREMENTS_LOGIC, &
        CO2_RETROFIT_LOGIC_ACTIVE
        CHARACTER (LEN=2) CAPACITY_PLANNING_METHOD,GREEN_MRX_METHOD
        REAL (KIND=4) :: GRX_CO2_MARKET_TONS, &
        GRX_CO2_MARKET_PRICE,MARKET_REDUCTION
        REAL (KIND=4), SAVE :: LOW_PRICE_VALUE/9999./, &
        HIGH_PRICE_VALUE/0./
        LOGICAL (KIND=1), SAVE :: HIGH_PRICE_FOUND,LOW_PRICE_FOUND, &
        MIN_MAX_SET

          IF(.NOT. (CAPACITY_PLANNING_METHOD() == 'MX'  .AND.   &
                                     GREEN_MRX_METHOD() == 'GX')) THEN
             CHECK_FOR_GRX_CONVERGENCE = .TRUE.
             GRX_CONVERGED = .TRUE.
             RETURN
          ENDIF
          CHECK_FOR_GRX_CONVERGENCE = .FALSE.
          GRX_CONVERGED = .FALSE.
          CO2_EMISSIONS_CAP = GET_EMISS_CAP_FOR_CLASS(3_2,TG)
          CO2_EMISSIONS = GET_CO2_EMISS_FOR_SYSTEM()
          IS_CO2_PRICE_MRKT_CAPPED = .FALSE.
          write(9,*) "in GRX conver"
          write(9,*) "Emiss CAP", CO2_EMISSIONS_CAP
          write(9,*) "Emiss Prod", CO2_EMISSIONS
          write(9,*) "CO2 Cedits (tons)", CO2_CAP_INCREASE_FROM_MARKET
          IF(GRX_ITERATIONS == 0) THEN
             OneMoreIter = .FALSE.
             HIGH_PRICE_FOUND = .FALSE.
             LOW_PRICE_FOUND = .FALSE.
             MIN_MAX_SET = .FALSE.
             IF(CO2_EMISSIONS <= CO2_EMISSIONS_CAP) THEN
               LOW_PRICE_VALUE = CURRENT_CO2_DISPATCH_COST/4.
               HIGH_PRICE_VALUE = NEWTON_MRKT_PRICE_SEARCH(MAX_POINT &
               =CURRENT_CO2_DISPATCH_COST, &
                                         MIN_POINT=LOW_PRICE_VALUE, &
                                           MAX_CO2=CO2_EMISSIONS, &
                                           CO2_CAP=CO2_EMISSIONS_CAP)
               NEXT_CURRENT_CO2_DISPATCH_COST = LOW_PRICE_VALUE
               HIGH_PRICE_FOUND = .TRUE.
             ELSE  ! CO2_EMISSIONS > CO2_EMISSIONS_CAP UP THE CO2 PRICE
               HIGH_PRICE_VALUE = MAX(4.*CURRENT_CO2_DISPATCH_COST,20.)
           NEXT_CURRENT_CO2_DISPATCH_COST = NEWTON_MRKT_PRICE_SEARCH( &
                          MIN_POINT=CURRENT_CO2_DISPATCH_COST, &
                                      MAX_POINT=HIGH_PRICE_VALUE, &
                                          MIN_CO2=CO2_EMISSIONS, &
                                         CO2_CAP=CO2_EMISSIONS_CAP)
               NEXT_CURRENT_CO2_DISPATCH_COST = HIGH_PRICE_VALUE
               LOW_PRICE_FOUND = .TRUE.
             ENDIF
             CALL GRX_ITER_RESULTS(0_2)
            RETURN
          ENDIF
          IF(GRX_ITERATIONS == 1 .AND. CURRENT_CO2_DISPATCH_COST == &
            0. .AND. &
                    CO2_EMISSIONS < CO2_EMISSIONS_CAP) THEN
                    ! THEN MARKET WAS THEN LAST POINT USED
             CHECK_FOR_GRX_CONVERGENCE = .TRUE.  
             GRX_CONVERGED = .TRUE.
             CALL GRX_ITER_RESULTS(0_2)
             RETIRED_OPTIONS_PNTR = 0
             RETURN
          ENDIF
          IF(GRX_CO2_MARKET_PTS_USED > 0 .AND. CO2_EMISSIONS > &
          CO2_EMISSIONS_CAP) THEN
            CO2_CAP_INCREASE_FROM_MARKET = 10**6 * &
                     SUM(CO2_ABATE_VOLUMS(1:GRX_CO2_MARKET_PTS_USED))
            IF(CO2_EMISSIONS-CO2_CAP_INCREASE_FROM_MARKET <= &
            CO2_EMISSIONS_CAP) THEN
               MARKET_REDUCTION = 0.
               CO2_CAP_EXPENDITURES = 0.
               DO I = 1, GRX_CO2_MARKET_PTS_USED
                  MARKET_REDUCTION = MARKET_REDUCTION + 10**6 * &
                  CO2_ABATE_VOLUMS(I)
                  IF(CO2_EMISSIONS-MARKET_REDUCTION <= &
                  CO2_EMISSIONS_CAP) THEN
                     IF(I == GRX_CO2_MARKET_PTS_USED) THEN
                        CO2_CAP_INCREASE_FROM_MARKET = CO2_EMISSIONS &
                        - CO2_EMISSIONS_CAP
                        IF(CURRENT_CO2_DISPATCH_COST == &
                        CO2_ABATE_PRICES(I)) THEN
                           ! THEN MARKET WAS THEN LAST POINT USED
                           CHECK_FOR_GRX_CONVERGENCE = .TRUE.  
                           GRX_CONVERGED = .TRUE.
                           RETIRED_OPTIONS_PNTR = 0
                           MARKET_REDUCTION = MARKET_REDUCTION - &
                           10**6 * CO2_ABATE_VOLUMS(I)
                         CO2_CAP_EXPENDITURES = CO2_CAP_EXPENDITURES &
                 + (CO2_CAP_INCREASE_FROM_MARKET - MARKET_REDUCTION) &
                                        * CO2_ABATE_PRICES(I)/10.**6
                        ELSE ! CO2 PRICE ABOVE LAST MARKET PRICE WERE 
                             ! ALL THEN LAST PURCHASES USED
                           IF(ABS_ERROR(CO2_EMISSIONS_CAP, &
                        CO2_EMISSIONS-MARKET_REDUCTION,DELTA=.01)) THEN
                        ! THEN MARKET WAS THEN LAST POINT USED
         CHECK_FOR_GRX_CONVERGENCE = .TRUE.  
                              GRX_CONVERGED = .TRUE.
                              RETIRED_OPTIONS_PNTR = 0
                    CO2_CAP_EXPENDITURES = CO2_CAP_EXPENDITURES &
                           + CO2_ABATE_VOLUMS(I) * CO2_ABATE_PRICES(I)
                           ELSE !
                              NEXT_CURRENT_CO2_DISPATCH_COST = &
       NEWTON_MRKT_PRICE_SEARCH(MAX_POINT=CURRENT_CO2_DISPATCH_COST, &
                                            MAX_CO2=CO2_EMISSIONS, &
                       CO2_CAP=CO2_EMISSIONS_CAP+MARKET_REDUCTION)
                              LOW_PRICE_FOUND = .TRUE.
                              HIGH_PRICE_FOUND = .TRUE.
                              MIN_MAX_SET = .TRUE.
                           ENDIF
                        ENDIF
                     ELSE ! PRICE TOO HIGH TOO USE ALL OF MARKET
                        NEXT_CURRENT_CO2_DISPATCH_COST = &
       NEWTON_MRKT_PRICE_SEARCH(MAX_POINT=CURRENT_CO2_DISPATCH_COST, &
                                      MAX_CO2=CO2_EMISSIONS, &
                                   MIN_POINT= CO2_ABATE_PRICES(I), &
                   CO2_CAP=CO2_EMISSIONS_CAP+MARKET_REDUCTION)
                     ENDIF
            LAST_CO2_MARKET_PRICE = NEXT_CURRENT_CO2_DISPATCH_COST
                     IF(GRX_CONVERGED) THEN
   CALL GRX_ITER_RESULTS(0_2,CONVER_CREDITS_USED= &
    CO2_CAP_INCREASE_FROM_MARKET, &
                          CONVER_CREDITS_COST=CO2_CAP_EXPENDITURES)
                     ELSE
                        CALL GRX_ITER_RESULTS(0_2)
                     ENDIF
                     EXIT
                  ENDIF
                  CO2_CAP_EXPENDITURES = CO2_CAP_EXPENDITURES &
                      + CO2_ABATE_VOLUMS(I) * CO2_ABATE_PRICES(I)
               ENDDO
            ELSE
        ! PRICE NOT HIGH ENOUGHT LOOK FOR NEXT MARKET POINT AS NEW PRICE
               IF(ABS_ERROR(CO2_EMISSIONS_CAP, &
           CO2_EMISSIONS-CO2_CAP_INCREASE_FROM_MARKET,DELTA=.01)) THEN
                  ! THEN MARKET WAS THEN LAST POINT USED
                  CHECK_FOR_GRX_CONVERGENCE = .TRUE.  
                  GRX_CONVERGED = .TRUE.
                  RETIRED_OPTIONS_PNTR = 0
                 CO2_CAP_EXPENDITURES = DOT_PRODUCT( &
                 CO2_ABATE_VOLUMS(1:GRX_CO2_MARKET_PTS_USED), &
                       CO2_ABATE_PRICES(1:GRX_CO2_MARKET_PTS_USED))
               ELSE !
                  LOW_PRICE_VALUE = CURRENT_CO2_DISPATCH_COST 
                  LOW_PRICE_FOUND = .TRUE.
                  CHECK_FOR_GRX_CONVERGENCE = .FALSE.
                  NEXT_CURRENT_CO2_DISPATCH_COST = &
                 NEWTON_MRKT_PRICE_SEARCH(MIN_POINT=LOW_PRICE_VALUE, &
                                              MIN_CO2=CO2_EMISSIONS, &
                                      CO2_CAP=CO2_EMISSIONS_CAP+ &
                                       CO2_CAP_INCREASE_FROM_MARKET)
                  MIN_MAX_SET = .TRUE.
               LAST_CO2_MARKET_PRICE = NEXT_CURRENT_CO2_DISPATCH_COST
               ENDIF
               CALL GRX_ITER_RESULTS(0_2)
            ENDIF
    write(9,*) "CO2 Cedits After (tons)", CO2_CAP_INCREASE_FROM_MARKET
        write(9,*) "Next Market Price", NEXT_CURRENT_CO2_DISPATCH_COST
            RETURN
          ELSEIF(GRX_CO2_MARKET_PTS_USED > 0 .AND. CO2_EMISSIONS <= &
          CO2_EMISSIONS_CAP) THEN
! CO2 PRICE TOO HIGH ADJUST TO FIRST MARKET PRICE
             HIGH_PRICE_VALUE = CURRENT_CO2_DISPATCH_COST 
             HIGH_PRICE_FOUND = .TRUE.
             IF(LOW_PRICE_FOUND .AND. .NOT. MIN_MAX_SET) THEN
                NEXT_CURRENT_CO2_DISPATCH_COST = &
                 NEWTON_MRKT_PRICE_SEARCH(MAX_POINT=HIGH_PRICE_VALUE, &
                                            MAX_CO2=CO2_EMISSIONS, &
                                        CO2_CAP=CO2_EMISSIONS_CAP+ &
                                        CO2_CAP_INCREASE_FROM_MARKET)
                MIN_MAX_SET = .TRUE.
             ELSEIF(MIN_MAX_SET) THEN
                NEXT_CURRENT_CO2_DISPATCH_COST = &
                NEWTON_MRKT_PRICE_SEARCH(MAX_POINT=HIGH_PRICE_VALUE, &
                                              MAX_CO2=CO2_EMISSIONS, &
                                          CO2_CAP=CO2_EMISSIONS_CAP+ &
                                          CO2_CAP_INCREASE_FROM_MARKET)
             ELSE
                NEXT_CURRENT_CO2_DISPATCH_COST =  &
                 CO2_ABATE_PRICES(MAX(1,GRX_CO2_MARKET_PTS_USED-1))
             ENDIF
             CALL GRX_ITER_RESULTS(0_2)
          ELSE
            IF(CO2_EMISSIONS <= CO2_EMISSIONS_CAP) THEN

     CAP_ERROR = (CO2_EMISSIONS_CAP - CO2_EMISSIONS)/CO2_EMISSIONS_CAP
      CHECK_FOR_GRX_CONVERGENCE = CAP_ERROR <= CO2_ERROR_TRAGET
    IF(GRX_ITERATIONS > 1 .AND. .NOT. CHECK_FOR_GRX_CONVERGENCE) THEN
                     CHECK_FOR_GRX_CONVERGENCE = &
                           CURRENT_CO2_DISPATCH_COST == 0. .AND. &
                         GRX_ITER_CO2_PRICE(GRX_ITERATIONS,TG) == 0.
                  ENDIF

            ENDIF
            IF(.NOT. CHECK_FOR_GRX_CONVERGENCE) THEN
               IF(GRX_ITERATIONS == 1) THEN
                  IF(CO2_EMISSIONS <= CO2_EMISSIONS_CAP) &
                     NEXT_CURRENT_CO2_DISPATCH_COST =  &
    NEWTON_MRKT_PRICE_SEARCH(MIN_POINT=.5*CURRENT_CO2_DISPATCH_COST, &
                               MAX_POINT=CURRENT_CO2_DISPATCH_COST, &
                                          MAX_CO2=CO2_EMISSIONS, &
                                         CO2_CAP=CO2_EMISSIONS_CAP)

                  IF(CO2_EMISSIONS > CO2_EMISSIONS_CAP) &
                     NEXT_CURRENT_CO2_DISPATCH_COST =  &
    NEWTON_MRKT_PRICE_SEARCH(MIN_POINT=CURRENT_CO2_DISPATCH_COST, &
                              MAX_POINT=3.*CURRENT_CO2_DISPATCH_COST, &
                                           MIN_CO2=CO2_EMISSIONS, &
                                          CO2_CAP=CO2_EMISSIONS_CAP)
               ELSE

                  IF(CO2_EMISSIONS <= CO2_EMISSIONS_CAP) THEN
                     LOW_PRICE_FOUND = .TRUE.
                     NEXT_CURRENT_CO2_DISPATCH_COST =  &
        NEWTON_MRKT_PRICE_SEARCH(MAX_POINT=CURRENT_CO2_DISPATCH_COST, &
                                          MAX_CO2=CO2_EMISSIONS, &
                                            CO2_CAP=CO2_EMISSIONS_CAP)
                  ENDIF

                  IF(CO2_EMISSIONS > CO2_EMISSIONS_CAP) THEN
                     HIGH_PRICE_FOUND = .TRUE.
                     NEXT_CURRENT_CO2_DISPATCH_COST =  &
        NEWTON_MRKT_PRICE_SEARCH(MIN_POINT=CURRENT_CO2_DISPATCH_COST, &
                                           MAX_CO2=CO2_EMISSIONS, &
                                            CO2_CAP=CO2_EMISSIONS_CAP)
                  ENDIF
               ENDIF
            ENDIF
            CALL GRX_ITER_RESULTS(0_2)
          ENDIF
          GRX_CO2_MARKET_PTS_USED_SAVED = GRX_CO2_MARKET_PTS_USED
          CHECK_FOR_GRX_CONVERGENCE = CHECK_FOR_GRX_CONVERGENCE .OR. &
                                       GRX_ITERATIONS >= GRX_ITERATIONS
          GRX_CONVERGED = CHECK_FOR_GRX_CONVERGENCE
          write(9,*) "GRX Convergence", GRX_CONVERGED
          IF(GRX_CONVERGED) THEN

             RETIRED_OPTIONS_PNTR = 0
          ENDIF
        END FUNCTION

        SUBROUTINE GRX_ABATE_ITER_RESULTS(CONVER_CREDITS_USED, &
        CONVER_CREDITS_COST)
        use mod_base_year
        USE CO2_CAP_N_TRADE
        USE co2_market_abatement_curves
        use grx_electric
        use globecom
        implicit none
        REAL (KIND=4), OPTIONAL :: CONVER_CREDITS_USED, &
        CONVER_CREDITS_COST
        LOGICAL (KIND=4) :: UPDATE

         INTEGER (KIND=4) :: NEXT_REC/0/
         CHARACTER (LEN=40) :: TITLE
         CHARACTER (LEN=28) :: COMPANY_NAME
         INTEGER (KIND=1) :: F7/z"f7"/
         LOGICAL (KIND=4) FILE_EXISTS
         INTEGER (KIND=2) :: OVERHEAD_LENGTH,DIMENSIONS, &
                             VARIABLE_NUMBER,RECORD_LENGTH, &
                             GET_NUM_OF_END_POINTS, &
                             TG/0/
         CHARACTER (LEN=256) :: FILE_NAME,GET_RESULTS_DIRECTORY
         CHARACTER (LEN=5) :: GET_SCENAME
         CHARACTER (LEN=35) :: GET_GROUP_NAME
         REAL (KIND=4) :: CAP_ERROR,GET_EMISS_CAP_FOR_CLASS, &
                          CO2_CREDITS_FROM_MARKET, &
                          CO2_CREDIT_EXPENDITURES, &
                          NET_CO2_EMISSIONS, &
                          ABATEMENTS_USED_TO_MEET_CAP, &
                          ABATEMENTS_FROM_MARKET
         REAL (KIND=4) :: CO2_EMISS_CAP,CO2_EMISSIONS,UNCOSTED_CREDITS
    REAL (KIND=4) :: CO2_ELECTRIC_EMISSION,CO2_NON_ELECTRIC_EMISSION, &
                          CO2_ABATEMENTS_AVAIL, &
                          CO2_BANK_CREDITS_USED, &
                          CO2_BANK_CREDITS_PURCHASED 
         IF(NEXT_REC == 0) THEN
            FILE_NAME = TRIM(GET_RESULTS_DIRECTORY())//"MSG"// &
                           TRIM(GET_SCENAME())//".XHD"
            OVERHEAD_LENGTH = 64
            VARIABLE_NUMBER = 20 ! 08/24/05. ADDED FORECAST VARIABLE
            DIMENSIONS = 4
            RECORD_LENGTH = MAX(64,OVERHEAD_LENGTH + 4*VARIABLE_NUMBER)
            OPEN(1338,FILE=FILE_NAME,ACCESS="DIRECT", &
                                   STATUS="REPLACE",RECL=RECORD_LENGTH)

            WRITE(1338,REC=1) F7,RECORD_LENGTH, &
                                      DIMENSIONS, &
                                      get_BASE_YEAR(), &
                                      gc_endyr+ &
                                      get_EXTENSION_PERIOD(), &
                                      GET_NUM_OF_END_POINTS()
            WRITE(1338,REC=2) COMPANY_NAME()
            WRITE(1338,REC=3) TITLE()
         WRITE(1338,REC=4) 'Endpoint            ','N',INT2(4),'F','D'
        WRITE(1338,REC=5) 'Year                ','N',INT2(4),'F','D'
        WRITE(1338,REC=6) 'Transaction Group   ','C',INT2(35),'V','D'
          WRITE(1338,REC=7) 'Iteration           ','N',INT2(4),'F','D'
            NEXT_REC = 7
         ENDIF
         NEXT_REC = NEXT_REC + 1
         CO2_EMISS_CAP = &

         GRX_ALL_SECTOR_CO2_EMISS_CAP(get_base_year()+ &
			globecom_YEAR) /1000000.

         CO2_BANK_CREDITS_USED = &
         ABS(MIN(0,GRX_CO2_BANK_DELTA(get_BASE_YEAR()+ &
		 globecom_YEAR)))/10.**6 
         ! ( negative term when bank is used for offset. )
         
         CO2_BANK_CREDITS_PURCHASED = &
         MAX(0,GRX_CO2_BANK_DELTA(get_BASE_YEAR()+globecom_YEAR))/10.**6
         ! ( positive term when bank purchases offsets )
         
         CO2_ELECTRIC_EMISSION = GET_CO2_EMISS_FOR_SYSTEM()/1000000.
         CO2_NON_ELECTRIC_EMISSION = &
         	grx_non_elec_sector_co2_emss( &
			get_base_year()+globecom_YEAR)/1000000.

         CO2_EMISSIONS = CO2_ELECTRIC_EMISSION &
                         + CO2_NON_ELECTRIC_EMISSION
         CO2_ABATEMENTS_AVAIL = &
           grx_co2_intrl_abatements_av(get_BASE_YEAR()+globecom_YEAR, &
                CURRENT_CO2_DISPATCH_COST)/1000000.
! changes 7/19/10
            if(globecom_year+get_BASE_YEAR() == 2016) then
               ABATEMENTS_USED_TO_MEET_CAP = 0.
            endif
         ABATEMENTS_USED_TO_MEET_CAP = CO2_EMISSIONS - CO2_EMISS_CAP
         IF(ABATEMENTS_USED_TO_MEET_CAP < 0.) THEN
            IF(CO2_ABATEMENTS_AVAIL > 0.) then
                CO2_BANK_CREDITS_PURCHASED = CO2_ABATEMENTS_AVAIL
            endif
            CO2_CREDITS_FROM_MARKET = 0.
            CO2_BANK_CREDITS_USED = 0.
         ELSE
            ABATEMENTS_USED_TO_MEET_CAP = &
            ABATEMENTS_USED_TO_MEET_CAP - CO2_BANK_CREDITS_USED
            
            IF(ABATEMENTS_USED_TO_MEET_CAP < 0.) THEN
               CO2_BANK_CREDITS_USED = ABATEMENTS_USED_TO_MEET_CAP
               IF(CO2_ABATEMENTS_AVAIL > 0.) then
                CO2_BANK_CREDITS_PURCHASED = CO2_ABATEMENTS_AVAIL
               endif
               CO2_CREDITS_FROM_MARKET = 0.
            ELSEIF(ABATEMENTS_USED_TO_MEET_CAP > CO2_ABATEMENTS_AVAIL) THEN
               CO2_CREDITS_FROM_MARKET = CO2_ABATEMENTS_AVAIL
               CO2_BANK_CREDITS_PURCHASED = 0.
            ELSE
               CO2_CREDITS_FROM_MARKET = ABATEMENTS_USED_TO_MEET_CAP
               CO2_BANK_CREDITS_PURCHASED = CO2_ABATEMENTS_AVAIL - &
				ABATEMENTS_USED_TO_MEET_CAP
            ENDIF
         ENDIF
         CO2_CAP_EXPENDITURES = CURRENT_CO2_DISPATCH_COST * CO2_ABATEMENTS_AVAIL

         UPDATE = GRX_ITERATIONS == 0
         NET_CO2_EMISSIONS = (CO2_EMISSIONS - CO2_CREDITS_FROM_MARKET &
                              - CO2_BANK_CREDITS_USED)
         ABATEMENTS_USED_TO_MEET_CAP = CO2_CREDITS_FROM_MARKET
         ABATEMENTS_FROM_MARKET = ABATEMENTS_USED_TO_MEET_CAP  &
                                  + CO2_BANK_CREDITS_PURCHASED &
                                  - CO2_BANK_CREDITS_USED
         CAP_ERROR = -100.*(CO2_EMISS_CAP - NET_CO2_EMISSIONS)/ &
                                     CO2_EMISS_CAP

         WRITE(1338,REC=NEXT_REC) PRT_ENDPOINT(),FLOAT( &
			get_BASE_YEAR()+globecom_YEAR), &
                     GET_GROUP_NAME(TG), &
                     FLOAT(GRX_ITERATIONS), &
                     CURRENT_CO2_DISPATCH_COST, & 
                     CO2_EMISSIONS, &
                     CAP_ERROR, &
                     CO2_EMISS_CAP, &
                     CO2_CREDITS_FROM_MARKET, &
                     CO2_CAP_EXPENDITURES, &  ! 5
                     NET_CO2_EMISSIONS, &
                     CO2_ABATEMENTS_AVAIL, &
                     CO2_ELECTRIC_EMISSION, &
                     CO2_NON_ELECTRIC_EMISSION, &
                     ABATEMENTS_USED_TO_MEET_CAP, &    ! 10
                     GRX_CO2_OTHER_ABATEMENTS_AV(get_BASE_YEAR()+ &
					 globecom_YEAR,CO2_PRICE=CURRENT_CO2_DISPATCH_COST), &
                     GRX_CO2_DOMISIC_ABATEMENTS_AV(get_BASE_YEAR()+ &
					 globecom_YEAR,CO2_PRICE=CURRENT_CO2_DISPATCH_COST), &
                     GRX_CO2_INTRL_ABATEMENTS_AV(get_BASE_YEAR()+ &
					 globecom_YEAR,CO2_PRICE=CURRENT_CO2_DISPATCH_COST), & !13
                   CO2_BANK_CREDITS_USED, &
                   CO2_BANK_CREDITS_PURCHASED, &
                   CO2_BANK_CREDITS_USED * CURRENT_CO2_DISPATCH_COST, &
                   CO2_BANK_CREDITS_PURCHASED * CURRENT_CO2_DISPATCH_COST, &  ! 17
                   CO2_BANKED_BALANCE(GRX_CONVERGED,CO2_BANK_CREDITS_PURCHASED), &
                   ABATEMENTS_FROM_MARKET   ! 19
         CALL FLUSH(1338)
      END SUBROUTINE GRX_ABATE_ITER_RESULTS
      SUBROUTINE GRX_ABATEMENT_MARKET_DATA_RPT()
        
         use globecom
         use mod_base_year
         implicit none
         INTEGER (KIND=4) :: NEXT_REC/0/
         CHARACTER (LEN=40) :: TITLE
         CHARACTER (LEN=28) :: COMPANY_NAME
         INTEGER (KIND=1) :: F7/z"f7"/
         LOGICAL (KIND=4) FILE_EXISTS
         INTEGER (KIND=2) :: OVERHEAD_LENGTH,DIMENSIONS, &
                             VARIABLE_NUMBER,RECORD_LENGTH, &
                             GET_NUM_OF_END_POINTS,I
         CHARACTER (LEN=256) :: FILE_NAME,GET_RESULTS_DIRECTORY
         CHARACTER (LEN=5) :: GET_SCENAME
         CHARACTER (LEN=35) :: GET_GROUP_NAME
         REAL (KIND=4) :: CO2_PRICE
         IF(NEXT_REC == 0) THEN
            FILE_NAME = TRIM(GET_RESULTS_DIRECTORY())//"MSG"// &
                           TRIM(GET_SCENAME())//".XID"
            OVERHEAD_LENGTH = 32
            VARIABLE_NUMBER = 20 ! 08/24/05. ADDED FORECAST VARIABLE
            DIMENSIONS = 3
            RECORD_LENGTH = MAX(64,OVERHEAD_LENGTH + 4*VARIABLE_NUMBER)
            OPEN(1339,FILE=FILE_NAME,ACCESS="DIRECT", &
                                   STATUS="REPLACE",RECL=RECORD_LENGTH)
!
            WRITE(1339,REC=1) F7,RECORD_LENGTH, &
                                 DIMENSIONS, &
                                 get_BASE_YEAR(), &
                                 gc_endyr+ &
                                 get_EXTENSION_PERIOD(), &
                                 GET_NUM_OF_END_POINTS()
            WRITE(1339,REC=2) COMPANY_NAME()
            WRITE(1339,REC=3) TITLE()
            WRITE(1339,REC=4) 'Endpoint            ','N',INT2(4),'F','D'
            WRITE(1339,REC=5) 'Year                ','N',INT2(4),'F','D'
            WRITE(1339,REC=6) 'Price               ','N',INT2(4),'F','D'
            NEXT_REC = 6
         ENDIF
         CO2_PRICE = 5.
         DO I = 1, 55
            NEXT_REC = NEXT_REC + 1
            WRITE(1339,REC=NEXT_REC) PRT_ENDPOINT(), &
                                     FLOAT(get_BASE_YEAR()+globecom_YEAR), &
                                     CO2_PRICE, &
                   GRX_CO2_DOMISIC_ABATEMENTS_AV(get_BASE_YEAR()+globecom_YEAR,&
                   CO2_PRICE=CO2_PRICE), &
GRX_CO2_INTRL_ABATEMENTS_AV(get_BASE_YEAR()+globecom_YEAR,CO2_PRICE=CO2_PRICE), &
GRX_CO2_OTHER_ABATEMENTS_AV(get_BASE_YEAR()+globecom_YEAR,CO2_PRICE=CO2_PRICE), &
GRX_CO2_DOMISIC_ABATEMENTS_AV(get_BASE_YEAR()+ &
	globecom_YEAR,CO2_PRICE=CO2_PRICE)  &
+ GRX_CO2_INTRL_ABATEMENTS_AV(get_BASE_YEAR()+globecom_YEAR,CO2_PRICE=CO2_PRICE) &
+ GRX_CO2_OTHER_ABATEMENTS_AV(get_BASE_YEAR()+globecom_YEAR,CO2_PRICE=CO2_PRICE)

            IF(I > 40) THEN
               CO2_PRICE = CO2_PRICE + 100.
            ELSEIF(I > 20) THEN
               CO2_PRICE = CO2_PRICE + 50.
            ELSE
               CO2_PRICE = CO2_PRICE + 5.
            ENDIF
         ENDDO
         CALL FLUSH(1339)
      END SUBROUTINE GRX_ABATEMENT_MARKET_DATA_RPT
      REAL FUNCTION grx_co2_intrl_abatements_av(YEAR,CO2_PRICE,CO2_CREDITS)
      USE CO2_CAP_N_TRADE
      REAL (KIND=4), OPTIONAL :: CO2_PRICE,CO2_CREDITS
      REAL (KIND=4) :: GRX_CO2_PRICE
      INTEGER (KIND=2) :: YEAR
         IF(PRESENT(CO2_PRICE)) THEN
            IF(YEAR <= YR2_DATA .AND. YEAR >= YR1_DATA .AND. CO2_PRICE > 0. &
				.AND. CO2_PARAM_FILE_EXISTS) THEN
               IF(CO2_PRICE <= Non_El_Price_Break(YEAR)) THEN
                  IF(Non_El_1_B_Coefficient(YEAR) == 0.) THEN
                     grx_co2_intrl_abatements_av = 0.
                  ELSE
                     IF(INDEX(Non_El_Price_Equation_1_Type(YEAR),"Expon") /= 0) &
					 THEN
                        grx_co2_intrl_abatements_av=MAX(0.,(LOG(CO2_PRICE)- &
						LOG(Non_El_1_A_Coefficient(YEAR)))/ &
                        Non_El_1_B_Coefficient(YEAR))
                     ELSE
                        grx_co2_intrl_abatements_av = &
							(CO2_PRICE-Non_El_1_A_Coefficient(YEAR))/ &
                                      Non_El_1_B_Coefficient(YEAR)
                     ENDIF
                  ENDIF

               ELSE ! Equation 2
                  IF(Non_El_2_B_Coefficient(YEAR) == 0.) THEN
                     grx_co2_intrl_abatements_av = 0.
                  ELSE
                     IF(INDEX(Non_El_Price_Equation_2_Type(YEAR),"Expon") &
					 /= 0) THEN
                        grx_co2_intrl_abatements_av=MAX(0., &
						(LOG(CO2_PRICE)-LOG(Non_El_2_A_Coefficient(YEAR)))/ &
                         Non_El_2_B_Coefficient(YEAR))
                     ELSE
                        grx_co2_intrl_abatements_av = &
						(CO2_PRICE-Non_El_2_A_Coefficient(YEAR))/  &
                         Non_El_2_B_Coefficient(YEAR)
                     ENDIF
                  ENDIF

!               CO2_ABATEMENTS_PURCHASED
               ENDIF
            ELSE
               grx_co2_intrl_abatements_av = 0.
            ENDIF
            grx_co2_intrl_abatements_av = MAX(grx_co2_intrl_abatements_av, &
              Non_Elect_Min_Reductions(YEAR))
            grx_co2_intrl_abatements_av = MIN(grx_co2_intrl_abatements_av, &
                                   Non_Elect_Max_Reductions(YEAR))
         ENDIF ! PRICE PRESENT
         IF(PRESENT(CO2_CREDITS)) THEN

            IF(YEAR <= YR2_DATA .AND. YEAR >= YR1_DATA .AND. &
				CO2_PARAM_FILE_EXISTS) THEN
               IF(INDEX(Non_El_Price_Equation_1_Type(YEAR), &
			   "Expon") /= 0) THEN
                  GRX_CO2_PRICE = Non_El_1_A_Coefficient(YEAR)* &
				  EXP(CO2_CREDITS*Non_El_1_B_Coefficient(YEAR))
                  IF(GRX_CO2_PRICE > Non_El_Price_Break(YEAR)) THEN
                     IF(INDEX(Non_El_Price_Equation_2_Type(YEAR),"Expon") &
					 /= 0) THEN
                        GRX_CO2_PRICE = Non_El_2_A_Coefficient(YEAR)* &
						EXP(CO2_CREDITS*Non_El_2_B_Coefficient(YEAR))
                     ELSE
                        GRX_CO2_PRICE = Non_El_2_A_Coefficient(YEAR) + &
							CO2_CREDITS*Non_El_2_B_Coefficient(YEAR)
                     ENDIF
                  ENDIF
               ELSE
                  GRX_CO2_PRICE = Non_El_1_A_Coefficient(YEAR) + &
					CO2_CREDITS*Non_El_1_B_Coefficient(YEAR)
                  IF(GRX_CO2_PRICE > Non_El_Price_Break(YEAR)) THEN
                     IF(INDEX(Non_El_Price_Equation_2_Type(YEAR),"Expon") /= 0)&
					 THEN
                        GRX_CO2_PRICE = Non_El_2_A_Coefficient(YEAR)* &
						EXP(CO2_CREDITS*Non_El_2_B_Coefficient(YEAR))
                     ELSE
                        GRX_CO2_PRICE = Non_El_2_A_Coefficient(YEAR) + &
						CO2_CREDITS*Non_El_2_B_Coefficient(YEAR)
                     ENDIF
                  ENDIF


               ENDIF
               grx_co2_intrl_abatements_av = GRX_CO2_PRICE
            ELSE
               grx_co2_intrl_abatements_av = 0.
            ENDIF
         ENDIF
      END FUNCTION
end module grx_captrade
