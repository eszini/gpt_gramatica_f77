module grx_extend
! Module created to fix circular dependency problem between
! grx_planning_routines and GRXModules.
!
use co2_cap_n_trade
use grx_data
use endpoint
use co2_market_abatement_curves
use cla_objt_arrays
use grx_electric
use globecom

implicit none
contains


      REAL FUNCTION GRX_CO2_DOMISIC_ABATEMENTS_AV( &
      YEAR,CO2_PRICE,CO2_CREDITS)
      USE CO2_CAP_N_TRADE
      REAL (KIND=4), OPTIONAL :: CO2_PRICE,CO2_CREDITS
      REAL (KIND=4) :: GRX_CO2_PRICE
      INTEGER (KIND=2) :: YEAR
         IF(PRESENT(CO2_PRICE)) THEN
            IF(YEAR <= YR2_DATA .AND. YEAR >= YR1_DATA .AND. &
            CO2_PRICE > 0. .AND. CO2_PARAM_FILE_EXISTS) THEN
               IF(CO2_PRICE <= Dom_Price_Break(YEAR)) THEN
                  IF(Dom_1_B_Coefficient(YEAR) == 0.) THEN
                     GRX_CO2_DOMISIC_ABATEMENTS_AV = 0.
                  ELSE
                     IF(INDEX(Dom_Price_Equation_1_Type(YEAR),&
                     "Expon") /= 0) THEN
                        GRX_CO2_DOMISIC_ABATEMENTS_AV= &
                        MAX(0.,(LOG(CO2_PRICE)-LOG(&
                        Dom_1_A_Coefficient(YEAR)))/ &
                        Dom_1_B_Coefficient(YEAR))
                     ELSE
                        GRX_CO2_DOMISIC_ABATEMENTS_AV = &
                        (CO2_PRICE-Dom_1_A_Coefficient(YEAR))/  &
                                            Dom_1_B_Coefficient(YEAR)
                     ENDIF
                  ENDIF

               ELSE ! Equation Type 2
                  IF(Dom_2_B_Coefficient(YEAR) == 0.) THEN
                     GRX_CO2_DOMISIC_ABATEMENTS_AV = 0.
                  ELSE
                     IF(INDEX(Dom_Price_Equation_2_Type(YEAR), &
                     "Expon") /= 0) THEN
                        GRX_CO2_DOMISIC_ABATEMENTS_AV=&
             MAX(0.,(LOG(CO2_PRICE)-LOG(Dom_2_A_Coefficient(YEAR)))/ &
                                           Dom_2_B_Coefficient(YEAR))
                     ELSE
                        GRX_CO2_DOMISIC_ABATEMENTS_AV = &
                        (CO2_PRICE-Dom_2_A_Coefficient(YEAR))/  &
                                            Dom_2_B_Coefficient(YEAR)
                     ENDIF
                  ENDIF
           

               ENDIF
            ELSE
              GRX_CO2_DOMISIC_ABATEMENTS_AV = 0.
            ENDIF
            GRX_CO2_DOMISIC_ABATEMENTS_AV = &
            MAX(GRX_CO2_DOMISIC_ABATEMENTS_AV, &
                Dom_Min_Off_Sets_To_Buy(YEAR))
            GRX_CO2_DOMISIC_ABATEMENTS_AV = &
            MIN(GRX_CO2_DOMISIC_ABATEMENTS_AV, &
                              Dom_Max_Off_Sets_To_Buy(YEAR))
         ENDIF
         IF(PRESENT(CO2_CREDITS)) THEN

            IF(YEAR <= YR2_DATA .AND. YEAR >= YR1_DATA .AND. &
            CO2_PARAM_FILE_EXISTS) THEN
          IF(INDEX(Dom_Price_Equation_1_Type(YEAR),"Expon") /= 0) THEN
               GRX_CO2_PRICE = Dom_1_A_Coefficient(YEAR)* &
               EXP(CO2_CREDITS*Dom_1_B_Coefficient(YEAR))
                  IF(GRX_CO2_PRICE > Dom_Price_Break(YEAR)) THEN
                     IF(INDEX(Dom_Price_Equation_2_Type(YEAR), &
                     "Expon") /= 0) THEN
                        GRX_CO2_PRICE = Dom_2_A_Coefficient(YEAR)* &
                        EXP(CO2_CREDITS*Dom_2_B_Coefficient(YEAR))
                     ELSE
                        GRX_CO2_PRICE = Dom_2_A_Coefficient(YEAR) + &
                        CO2_CREDITS*Dom_2_B_Coefficient(YEAR)
                     ENDIF
                  ENDIF
               ELSE
                  GRX_CO2_PRICE = Dom_1_A_Coefficient(YEAR) + &
                  CO2_CREDITS*Dom_1_B_Coefficient(YEAR)
                  IF(GRX_CO2_PRICE > Dom_Price_Break(YEAR)) THEN
                     IF(INDEX(Dom_Price_Equation_2_Type(YEAR), &
                     "Expon") /= 0) THEN
                        GRX_CO2_PRICE = Dom_2_A_Coefficient(YEAR)* &
                        EXP(CO2_CREDITS*Dom_2_B_Coefficient(YEAR))
                     ELSE
                        GRX_CO2_PRICE = Dom_2_A_Coefficient(YEAR) + &
                        CO2_CREDITS*Dom_2_B_Coefficient(YEAR)
                     ENDIF
                  ENDIF


               ENDIF
               GRX_CO2_DOMISIC_ABATEMENTS_AV = GRX_CO2_PRICE
            ELSE
               GRX_CO2_DOMISIC_ABATEMENTS_AV = 0.
            ENDIF
         ENDIF

      END FUNCTION GRX_CO2_DOMISIC_ABATEMENTS_AV
      REAL FUNCTION GRX_CO2_OTHER_ABATEMENTS_AV(YEAR,CO2_PRICE, &
        CO2_CREDITS)
      
      REAL (KIND=4), OPTIONAL :: CO2_PRICE,CO2_CREDITS
      REAL (KIND=4) :: GRX_CO2_PRICE
      INTEGER (KIND=2) :: YEAR
         IF(PRESENT(CO2_PRICE)) THEN
            IF(YEAR <= YR2_DATA .AND. YEAR >= YR1_DATA .AND. &
            CO2_PRICE > 0. .AND. CO2_PARAM_FILE_EXISTS) THEN
               IF(CO2_PRICE <= Non_El_Price_Break(YEAR)) THEN
                  IF(Non_El_1_B_Coefficient(YEAR) == 0.) THEN
                     GRX_CO2_OTHER_ABATEMENTS_AV = 0.
                  ELSE
      IF(INDEX(Non_El_Price_Equation_1_Type(YEAR),"Expon") /= 0) THEN
                        GRX_CO2_OTHER_ABATEMENTS_AV=MAX(0.,&
                 (LOG(CO2_PRICE)-LOG(Non_El_1_A_Coefficient(YEAR)))/ &
                                         Non_El_1_B_Coefficient(YEAR))
                     ELSE
                  GRX_CO2_OTHER_ABATEMENTS_AV = &
                  (CO2_PRICE-Non_El_1_A_Coefficient(YEAR))/ &
   Non_El_1_B_Coefficient(YEAR)
                     ENDIF
                  ENDIF

               ELSE ! Equation 2
                  IF(Non_El_2_B_Coefficient(YEAR) == 0.) THEN
                     GRX_CO2_OTHER_ABATEMENTS_AV = 0.
                  ELSE
       IF(INDEX(Non_El_Price_Equation_2_Type(YEAR),"Expon") /= 0) THEN
                        GRX_CO2_OTHER_ABATEMENTS_AV=&
          MAX(0.,(LOG(CO2_PRICE)-LOG(Non_El_2_A_Coefficient(YEAR)))/ &
          Non_El_2_B_Coefficient(YEAR))
                     ELSE
                        GRX_CO2_OTHER_ABATEMENTS_AV = &
                        (CO2_PRICE-Non_El_2_A_Coefficient(YEAR))/  &
                          Non_El_2_B_Coefficient(YEAR)
                     ENDIF
                  ENDIF


               ENDIF
            ELSE
               GRX_CO2_OTHER_ABATEMENTS_AV = 0.
            ENDIF
      GRX_CO2_OTHER_ABATEMENTS_AV = MAX(GRX_CO2_OTHER_ABATEMENTS_AV, &
                                     Non_Elect_Min_Reductions(YEAR))
      GRX_CO2_OTHER_ABATEMENTS_AV = MIN(GRX_CO2_OTHER_ABATEMENTS_AV, &
                                      Non_Elect_Max_Reductions(YEAR))
         ENDIF ! PRICE PRESENT
         IF(PRESENT(CO2_CREDITS)) THEN

            IF(YEAR <= YR2_DATA .AND. YEAR >= YR1_DATA .AND. &
            CO2_PARAM_FILE_EXISTS) THEN
               IF(INDEX(Non_El_Price_Equation_1_Type(YEAR),&
               "Expon") /= 0) THEN
                  GRX_CO2_PRICE = Non_El_1_A_Coefficient(YEAR)* &
                  EXP(CO2_CREDITS*Non_El_1_B_Coefficient(YEAR))
                  IF(GRX_CO2_PRICE > Non_El_Price_Break(YEAR)) THEN
                     IF(INDEX(Non_El_Price_Equation_2_Type(YEAR), &
                     "Expon") /= 0) THEN
                        GRX_CO2_PRICE = Non_El_2_A_Coefficient(YEAR)* &
                        EXP(CO2_CREDITS*Non_El_2_B_Coefficient(YEAR))
                     ELSE
                        GRX_CO2_PRICE = Non_El_2_A_Coefficient(YEAR) &
                        + CO2_CREDITS*Non_El_2_B_Coefficient(YEAR)
                     ENDIF
                  ENDIF
               ELSE
                  GRX_CO2_PRICE = Non_El_1_A_Coefficient(YEAR) + &
                  CO2_CREDITS*Non_El_1_B_Coefficient(YEAR)
                  IF(GRX_CO2_PRICE > Non_El_Price_Break(YEAR)) THEN
                     IF(INDEX(Non_El_Price_Equation_2_Type(YEAR), &
                     "Expon") /= 0) THEN
                        GRX_CO2_PRICE = Non_El_2_A_Coefficient(YEAR)* &
                        EXP(CO2_CREDITS*Non_El_2_B_Coefficient(YEAR))
                     ELSE
                        GRX_CO2_PRICE = Non_El_2_A_Coefficient(YEAR) &
                        + CO2_CREDITS*Non_El_2_B_Coefficient(YEAR)
                     ENDIF
                  ENDIF


               ENDIF
               GRX_CO2_OTHER_ABATEMENTS_AV = GRX_CO2_PRICE
            ELSE
               GRX_CO2_OTHER_ABATEMENTS_AV = 0.
            ENDIF
         ENDIF
      END FUNCTION GRX_CO2_OTHER_ABATEMENTS_AV

      

      
      FUNCTION CO2_BANKED_BALANCE(UPDATE,Model_Purchases)
      USE CO2_CAP_N_TRADE
      LOGICAL (KIND=4) :: UPDATE
      real :: CO2_BANKED_BALANCE
      REAL (KIND=4) :: Model_Purchases
         CO2_BANKED_BALANCE = CO2_BANK_BALANCE + Model_Purchases
         IF(UPDATE) CO2_BANK_BALANCE = CO2_BANK_BALANCE + Model_Purchases
      END FUNCTION CO2_BANKED_BALANCE
      FUNCTION GRX_ALL_SECTOR_CO2_EMISS_CAP(YEAR)
      INTEGER (KIND=2) :: YEAR
      real :: GRX_ALL_SECTOR_CO2_EMISS_CAP
      
         GRX_ALL_SECTOR_CO2_EMISS_CAP=0
         
         IF(YEAR <= YR2_DATA .AND. YEAR >= YR1_DATA .AND. CO2_PARAM_FILE_EXISTS) THEN
            GRX_ALL_SECTOR_CO2_EMISS_CAP = 10**6*CO2_Emission_Cap(YEAR)
         ELSE
            GRX_ALL_SECTOR_CO2_EMISS_CAP = 0.
         ENDIF
      END FUNCTION GRX_ALL_SECTOR_CO2_EMISS_CAP      

end module grx_extend