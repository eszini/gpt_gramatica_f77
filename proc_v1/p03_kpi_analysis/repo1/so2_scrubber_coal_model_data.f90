! Extracted from coal_model_modules.f90
      MODULE SO2_Scrubber_COAL_MODEL_DATA
         INTEGER (KIND=2), SAVE :: nStateSO2Limits
         INTEGER (KIND=4), PARAMETER :: nStates=52
         REAL (KIND=4), SAVE :: SulphurCreditCost(0:30,0:nStates), &
                             SulphurCreditCostEscalation(0:nStates), &
                               SulphurEmissionsLimit(0:30,0:nStates), &
                               ScrubberCapitalCost(0:30,0:nStates), &
                               CapitalCostEscalation(0:nStates), &
                               ScrubberFixedOMCost(0:30,0:nStates), &
                               FixedOMEscalation(0:nStates), &
                              ScrubberVariableOMCost(0:30,0:nStates), &
                               VariableOMEscalation(0:nStates), &
                              StateCapVolatility ByYear(0:30,0:nStates)
         CHARACTER (LEN=20), SAVE :: SO2InputType(0:nStates)
         CHARACTER (LEN=2), SAVE :: SulphurMarketArea(0:nStates)
         LOGICAL (KIND=1), SAVE :: StateHasLimits(0:nStates)
         INTEGER (KIND=4), SAVE :: StateAssignedToGroup(0:nStates)
      END MODULE SO2_Scrubber_COAL_MODEL_DATA
