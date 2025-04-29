      MODULE CO2_CAP_N_TRADE
         INTEGER (KIND=2), SAVE :: YR1_DATA,YR2_DATA
         LOGICAL (KIND=4), SAVE :: CO2_PARAM_FILE_EXISTS
         REAL (KIND=4), SAVE :: CO2_BANK_BALANCE
         REAL (KIND=4), ALLOCATABLE, SAVE :: CO2_Emission_Cap(:), &
                          Residential_Emissions(:), &
                          Commercial_Emission(:), &
                          Industrial_Emissions(:), &
                          Transportation_Emissions(:), &
                          Dom_Min_Off_Sets_To_Buy(:), &
                          Dom_Max_Off_Sets_To_Buy(:), &
                          Int_Min_Off_Sets_To_Buy(:), &
                          Int_Max_Off_Sets_To_Buy(:), &  
                          Non_Elect_Min_Reductions(:), &
                          Non_Elect_Max_Reductions(:), &
                          CO2_Banking(:), &
                          Dom_1_A_Coefficient(:), &
                          Dom_1_B_Coefficient(:), &
                          Dom_Price_Break(:), &
                          Dom_2_A_Coefficient(:), &
                          Dom_2_B_Coefficient(:), &
                          Int_1_A_Coefficient(:), &
                          Int_1_B_Coefficient(:), &
                          Int_Price_Break(:), &
                          Int_2_A_Coefficient(:), &
                          Int_2_B_Coefficient(:), &
                          Non_El_1_A_Coefficient(:), &
                          Non_El_1_B_Coefficient(:), &
                          Non_El_Price_Break(:), &
                          Non_El_2_A_Coefficient(:), &
                          Non_El_2_B_Coefficient(:), &
                          CO2_Price_Forecast(:), &
                          CO2_Max_Price_Cap(:), &
                          CO2_Min_Price(:)
         CHARACTER (LEN=12), ALLOCATABLE, SAVE :: Dom_Price_Equation_1_Type(:), &
                               Dom_Price_Equation_2_Type(:), &
                               Int_Price_Equation_1_Type(:), &
                               Int_Price_Equation_2_Type(:), &
                               Non_El_Price_Equation_1_Type(:), &
                               Non_El_Price_Equation_2_Type(:)
         LOGICAL (KIND=1), ALLOCATABLE, SAVE :: CO2_Price_Evaluation(:), &
                                                CO2_Market_Available(:), &
                                                POKE_CURRENT_CO2_DISPATCH_COST(:)
         INTEGER (KIND=2), ALLOCATABLE, SAVE :: CO2_Min_GRX_Iterations(:), &
                                                CO2_Max_GRX_Iterations(:), &
                                                CO2_GRX_Iter_After_Converg(:)

      END MODULE
