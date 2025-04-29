      module pclr_shared
        implicit none
        integer (kind=2) :: L_S=0
      end module pclr_shared
      ! Function extracted from cla_objt
      FUNCTION CL_SCREEN_DATA()
      use prim_mover_idx
      use esrn_decs
      use miscmod
      use cla_decs
      use foshydi2com
      use p_fuel_annl
      use screen_interface_data
      use cl_screen_data_mod, only: ns_screen_data
      use end_routine
      use pclr_shared
      use emis_data
      use pclr_shared
      use program_state
      use debugtrace
      implicit none
      
      integer :: file_num_rc=0
      integer :: file_trace_rstg=0
      ! ISEAS never set! John added initialization to zero
      integer (kind=2) :: ISEAS=0
      integer (kind=2) :: R_YEAR
      integer :: ubpp, lbpp ! for range checking
      
      
      real :: TRANS_MRX_DISPATCH_EMIS_ADDER ! External in EN_OBJT
        real :: R_HG_MULT, &
           R_NOX_MULT, &
           R_SOX_MULT, &
           R_CO2_MULT

      real (kind=4) :: FUEL_SCEN_MULT
      integer (kind=2) :: get_s_primary_mover
      logical (kind=1) :: CL_SCREEN_DATA
      integer (kind=2) :: Rl_YEAR
      integer (kind=2) :: R_MONTH, temp_i2
      integer :: s
      real (kind=4) :: ESCAL_RATE, rtemp
      real (kind=4) :: SAVE_DELIVERY_COST=0
      integer (kind=2) :: ft, mo

      real :: R_CO2_RETIREMENT_PRICE, R_OTHER3_MULT, &
        other_variable_component
      integer (kind=2) :: R_MO,R_DATE,YR
      
      
      ! More externals
      real :: RETURN_CL_SCREEN_FIXED_COST,RETURN_CL_SCREEN_VARI_COST
      real :: RETURN_CL_SCREEN_FUEL_COST, &
            RETURN_CL_SCREEN_EMIS_COST, &
            GET_SCENARIO_CO2_PRICES

      real :: RETURN_OTHER_VAR_COMPONENT ! External

      ! More externals
      real :: GET_CL_CO2_TON_PER_MWH, &
            GET_VAR,FUEL_COMPONENT

      ! More externals
            logical (kind=1) :: &
             GET_S_BLOCK_CAP_AND_HR, &
                 SCREEN_NOX_ACTIVE_FOR_UNIT, &
                 GET_SCREEN_NOX_CONTROL_FOR_UNIT, &
                 GET_SCREEN_SOX_CONTROL_FOR_UNIT, &
                 GET_SCREEN_CO2_CONTROL_FOR_UNIT, &
                 GET_SCREEN_HG_CONTROL_FOR_UNIT, &
                 GET_SCREEN_OTHER3_CTRL_FOR_UNIT, &
         S_YES_MONTHLY_MUST_RUN

      character(len=1024) :: file_name
      
        ! More externals          
    LOGICAL (kind=1) :: &
                  R_SEASON_IS_NOX_SEASON, &
                  R_CALCULATE_NOX, &
                  VECTOR_IS_VALUE
                  
      ! External
      integer (kind=2) :: UPDATE_CL_SCREEN_COSTS

      ! External
      real (kind=4) :: ESCALATED_MONTHLY_VALUE

      ! Externals
      real (kind=4) :: R_BLOCK_CAPACITY(5), &
                    R_BLOCK_HEAT_RATE(5)

      real (kind=4) ::  R_NOX_CONTROL_PERCENT, &
                           R_SOX_CONTROL_PERCENT, &
                           R_CO2_CONTROL_PERCENT, &
                           R_HG_CONTROL_PERCENT, &
                           R_OTHER3_CONTROL_PERCENT
      INTEGER (kind=2) :: PROD_POINTER
      logical (kind=1), allocatable :: IS_A_HARD_WIRED_UNIT(:)

      real :: ESCALATED_VALUE
      ! Externals
      real (kind=4) :: RET_VAL_FROM_ESCALATION_VECTOR
      real (kind=4) :: START_UP_COST_ESCALAT_MTHLY_VAL
      real (kind=4) ::  R_FUEL_DELIVERY_1
      real (kind=4) ::  R_FUEL_DELIVERY_2
      real (kind=4) ::  R_FUEL_DELIVERY_3
      real (kind=4) ::  R_DELIVERY_COST
      real (kind=4) :: R_MONTHLY(12)

      real (kind=4) :: R_HEAT_RATE, &
                       R_FUEL_COST, &
            FUEL_MX_FACTOR

      real :: tR4, TEMP_MOVAL
        


        
      integer(kind=2) :: GET_S_RPS_PROGRAM_NUMBER ! External
      real (kind=4) :: GET_S_RPS_PERCENT ! External
      integer (kind=2) :: RETURN_S_TRANS_GROUP_ID ! External
      real (kind=4) :: GET_S_CAP_PLANNING_FAC ! External
      
      ! More externals
      integer (kind=2) ::  GET_SCRN_UNIT_GAS_REGION_INDEX, &
                           GET_SCREEN_STATE_INDEX
      integer (kind=2) :: GSSI_result
      integer (kind=2) :: I

! DECLARATIONS FOR THE EXPANSION PLANT OPERATIONS FILE






      ! More externals
      real (kind=4) :: GET_SCREEN_NOX_VOM, &
                    GET_SCREEN_NOX_FOM, &
                    GET_SCREEN_SOX_VOM, &
                    GET_SCREEN_SOX_FOM, &
                    GET_SCREEN_CO2_VOM, &
                    GET_SCREEN_CO2_FOM, &
                    GET_SCREEN_HG_VOM, &
                    GET_SCREEN_HG_FOM, &
                    GET_SCREEN_OTHER3_VOM, &
                    GET_SCREEN_OTHER3_FOM

      
     integer (kind=2) :: GET_S_PRIMARY_MOVER_INDEX ! external
     integer (kind=2) :: GET_MRX_EMISSION_MARKET_LINK! Exported external
     integer (kind=2) :: GET_PRIMARY_MOVER ! External
     integer :: lb, ub

                           
     real (kind=4) :: GET_NEW_UNIT_FUEL_DELIVERY ! External
     real (kind=4) :: GET_S_START_UP_COSTS ! External
     real (kind=4) :: GET_S_MIN_UP_TIME ! External
     real (kind=4) :: RETURN_CL_SCREEN_EFOR ! External
     real (kind=4) :: PUT_MRX_DELIVERY_COST ! External
     real (kind=4) :: GET_S_MIN_DOWN_TIME ! External
     real (kind=4) :: GET_SCENARIO_COAL_PRICES ! External
     
     ! Externals
     real (kind=4) :: GET_SCENARIO_GAS_PRICES, &
           GET_SCENARIO_OIL_PRICES, GET_SCENARIO_URANIUM_PRICES
           
           
     


      CL_SCREEN_DATA = .TRUE.
      A_HR=0.



      ENTRY GET_S_RPS_PROGRAM_NUMBER(PROD_POINTER)

         L_S = PROD_PNTR_BY_CL_UNITS(PROD_POINTER)

         IF(L_S > 0) THEN
            GET_S_RPS_PROGRAM_NUMBER = S_RPS_PROGRAM_NUMBER(L_S)
         ELSE
            GET_S_RPS_PROGRAM_NUMBER = 0
         ENDIF
      RETURN

      ENTRY GET_S_RPS_PERCENT(PROD_POINTER,R_YEAR)

         L_S = PROD_PNTR_BY_CL_UNITS(PROD_POINTER)
         IF(S_RPS_PERCENT(L_S) < -0.0001) THEN
            GET_S_RPS_PERCENT = &
                  0.01*ESCALATED_MONTHLY_VALUE(GET_S_RPS_PERCENT, &
                          ABS(INT2(S_RPS_PERCENT(L_S))), &
                                                R_YEAR,INT2(1),INT2(1))
         ELSE
            GET_S_RPS_PERCENT = S_RPS_PERCENT(L_S)*0.01
         ENDIF
      RETURN

      ENTRY GET_S_BLOCK_CAP_AND_HR(PROD_POINTER, &
                                   R_BLOCK_CAPACITY, &
                                   R_BLOCK_HEAT_RATE, &
                                   R_YEAR)

         L_S = PROD_PNTR_BY_CL_UNITS(PROD_POINTER)
         R_BLOCK_CAPACITY = 0.
         R_BLOCK_HEAT_RATE = 0.
         IF(L_S > 0) THEN
            GET_S_BLOCK_CAP_AND_HR = .TRUE.
            IF(S_HR_FACTOR(L_S) < 0.) THEN
               HEAT_RATE_FACTOR = &
                    GET_VAR(S_HR_FACTOR(L_S),R_YEAR,S_UNITNM(L_S))
            ELSE
               HEAT_RATE_FACTOR = S_HR_FACTOR(L_S)
            ENDIF

            IF(S_INPUT_MW(2,L_S) - S_INPUT_MW(1,L_S) < 0.0001 .OR. &
                                       S_INPUT_MW(1,L_S) < 0.0001) THEN

               R_BLOCK_CAPACITY(1) = 0.0
               R_BLOCK_HEAT_RATE(1) = 0.0
               R_BLOCK_CAPACITY(2) = MAX(S_INPUT_MW(1,L_S), &
                                                     S_INPUT_MW(2,L_S))
               R_BLOCK_HEAT_RATE(2) = S_COEFF(1,L_S) * HEAT_RATE_FACTOR
            ELSE
! TWO BLOCK
               IF(S_INPUT_MW(1,L_S) < 1.0) THEN
                  R_BLOCK_CAPACITY(1) = S_INPUT_MW(1,L_S) * &
                                                      S_INPUT_MW(2,L_S)
               ELSE
                  R_BLOCK_CAPACITY(1) = S_INPUT_MW(1,L_S)
               ENDIF
               R_BLOCK_HEAT_RATE(1) = S_COEFF(1,L_S) * HEAT_RATE_FACTOR
               R_BLOCK_CAPACITY(2) = S_INPUT_MW(2,L_S)
               INC_CAP = S_INPUT_MW(2,L_S) - S_INPUT_MW(1,L_S)
               A_HR = (S_COEFF(3,L_S) - S_COEFF(2,L_S))/(2.*INC_CAP)
               B_HR = S_COEFF(3,L_S) - 2.* A_HR * INC_CAP
               R_BLOCK_HEAT_RATE(2) = (A_HR*INC_CAP + B_HR) * &
                                                       HEAT_RATE_FACTOR
            ENDIF

         ELSE
            GET_S_BLOCK_CAP_AND_HR = .FALSE.
         ENDIF
      RETURN

      ENTRY GET_SCRN_UNIT_GAS_REGION_INDEX(PROD_POINTER)

         L_S = PROD_PNTR_BY_CL_UNITS(PROD_POINTER)
         GSSI_result= &
             S_UNIT_GAS_REGION_INDEX(L_S)
             GET_SCRN_UNIT_GAS_REGION_INDEX=GSSI_result
      RETURN

      ENTRY GET_SCREEN_STATE_INDEX(PROD_POINTER)
         ! prod_pointer=1/adsf
         L_S = PROD_PNTR_BY_CL_UNITS(PROD_POINTER)


         gssi_result=State_Index(L_S)
         
         GET_SCREEN_STATE_INDEX=gssi_result
      RETURN

      ENTRY SCREEN_NOX_ACTIVE_FOR_UNIT( &

                                PROD_POINTER, &
                                R_DATE, &
                                R_SEASON_IS_NOX_SEASON, &
                                R_CALCULATE_NOX)

         L_S = PROD_PNTR_BY_CL_UNITS(PROD_POINTER)

         SCREEN_NOX_ACTIVE_FOR_UNIT = .TRUE.
         IF(APPLY_NOX_SEASON_DATE(PROD_POINTER) == 'T') THEN
            IF(R_DATE >= NOX_SEASON_DATE(PROD_POINTER) .AND. &
                                           R_SEASON_IS_NOX_SEASON) THEN
               R_CALCULATE_NOX = .TRUE.
            ELSE
               R_CALCULATE_NOX = .FALSE.
            ENDIF
         ENDIF
      RETURN

      ENTRY GET_SCREEN_NOX_CONTROL_FOR_UNIT( &
                                PROD_POINTER, &
                                R_DATE, &
                                R_NOX_CONTROL_PERCENT)

         L_S = PROD_PNTR_BY_CL_UNITS(PROD_POINTER)

         GET_SCREEN_NOX_CONTROL_FOR_UNIT = .FALSE.
         R_NOX_CONTROL_PERCENT = 1.0
         IF(R_DATE >= NOX_CONTROL_DATE(L_S)) THEN
            GET_SCREEN_NOX_CONTROL_FOR_UNIT = .TRUE.
            R_NOX_CONTROL_PERCENT = NOX_CONTROL_PERCENT(L_S)
         ENDIF
      RETURN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ENTRY GET_SCREEN_CO2_CONTROL_FOR_UNIT( &
                                PROD_POINTER, &
                                R_DATE, &
                                R_CO2_CONTROL_PERCENT)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         L_S = PROD_PNTR_BY_CL_UNITS(PROD_POINTER)

         GET_SCREEN_CO2_CONTROL_FOR_UNIT = .FALSE.
         R_CO2_CONTROL_PERCENT = 1.0
         IF(R_DATE >= CO2_CONTROL_DATE(L_S)) THEN
            GET_SCREEN_CO2_CONTROL_FOR_UNIT = .TRUE.
            R_CO2_CONTROL_PERCENT = CO2_CONTROL_PERCENT(L_S)
         ENDIF
      RETURN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ENTRY GET_SCREEN_HG_CONTROL_FOR_UNIT( &
                                PROD_POINTER, &
                                R_DATE, &
                                R_HG_CONTROL_PERCENT)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         L_S = PROD_PNTR_BY_CL_UNITS(PROD_POINTER)

         GET_SCREEN_HG_CONTROL_FOR_UNIT = .FALSE.
         R_HG_CONTROL_PERCENT = 1.0
         IF(R_DATE >= HG_CONTROL_DATE(L_S)) THEN
            GET_SCREEN_HG_CONTROL_FOR_UNIT = .TRUE.
            R_HG_CONTROL_PERCENT = HG_CONTROL_PERCENT(L_S)
         ENDIF
      RETURN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ENTRY GET_SCREEN_OTHER3_CTRL_FOR_UNIT( &
                                PROD_POINTER, &
                                R_DATE, &
                                R_OTHER3_CONTROL_PERCENT)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         L_S = PROD_PNTR_BY_CL_UNITS(PROD_POINTER)

         GET_SCREEN_OTHER3_CTRL_FOR_UNIT = .FALSE.
         R_OTHER3_CONTROL_PERCENT = 1.0
         IF(R_DATE >= OTHER3_CONTROL_DATE(L_S)) THEN
            GET_SCREEN_OTHER3_CTRL_FOR_UNIT = .TRUE.
            R_OTHER3_CONTROL_PERCENT = OTHER3_CONTROL_PERCENT(L_S)
         ENDIF
      RETURN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ENTRY GET_SCREEN_SOX_CONTROL_FOR_UNIT( &
                                PROD_POINTER, &
                                R_DATE, &
                                R_SOX_CONTROL_PERCENT)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         L_S = PROD_PNTR_BY_CL_UNITS(PROD_POINTER)

         GET_SCREEN_SOX_CONTROL_FOR_UNIT = .FALSE.
         R_SOX_CONTROL_PERCENT = 1.0
         IF(R_DATE >= SOX_CONTROL_DATE(L_S)) THEN
            GET_SCREEN_SOX_CONTROL_FOR_UNIT = .TRUE.
            R_SOX_CONTROL_PERCENT = SOX_CONTROL_PERCENT(L_S)
         ENDIF
      RETURN

      ENTRY GET_SCREEN_NOX_VOM(PROD_POINTER)   !R4

         L_S = PROD_PNTR_BY_CL_UNITS(PROD_POINTER)

         GET_SCREEN_NOX_VOM = NOX_VOM(L_S)
      RETURN

      ENTRY GET_SCREEN_NOX_FOM(PROD_POINTER)  !R4

         L_S = PROD_PNTR_BY_CL_UNITS(PROD_POINTER)

         GET_SCREEN_NOX_FOM = NOX_FOM(L_S)
      RETURN

      ENTRY GET_SCREEN_CO2_VOM(PROD_POINTER)   !R4

         L_S = PROD_PNTR_BY_CL_UNITS(PROD_POINTER)

         GET_SCREEN_CO2_VOM = CO2_VOM(L_S)
      RETURN

      ENTRY GET_SCREEN_CO2_FOM(PROD_POINTER)   !R4

         L_S = PROD_PNTR_BY_CL_UNITS(PROD_POINTER)

         GET_SCREEN_CO2_FOM = CO2_FOM(L_S)
      RETURN

      ENTRY GET_SCREEN_HG_VOM(PROD_POINTER) !R4

         L_S = PROD_PNTR_BY_CL_UNITS(PROD_POINTER)

         GET_SCREEN_HG_VOM = HG_VOM(L_S)
      RETURN

      ENTRY GET_SCREEN_HG_FOM(PROD_POINTER)   !R4

         L_S = PROD_PNTR_BY_CL_UNITS(PROD_POINTER)

         GET_SCREEN_HG_FOM = HG_FOM(L_S)
      RETURN

      ENTRY GET_SCREEN_OTHER3_VOM(PROD_POINTER)  !R4

         L_S = PROD_PNTR_BY_CL_UNITS(PROD_POINTER)

         GET_SCREEN_OTHER3_VOM = OTHER3_VOM(L_S)
      RETURN

      ENTRY GET_SCREEN_OTHER3_FOM(PROD_POINTER)  !R4

         L_S = PROD_PNTR_BY_CL_UNITS(PROD_POINTER)

         GET_SCREEN_OTHER3_FOM = OTHER3_FOM(L_S)
      RETURN

      ENTRY GET_SCREEN_SOX_VOM(PROD_POINTER)   !R4

         L_S = PROD_PNTR_BY_CL_UNITS(PROD_POINTER)

         GET_SCREEN_SOX_VOM = SOX_VOM(L_S)
      RETURN

      ENTRY GET_SCREEN_SOX_FOM(PROD_POINTER)   !R4

         L_S = PROD_PNTR_BY_CL_UNITS(PROD_POINTER)

         GET_SCREEN_SOX_FOM = SOX_FOM(L_S)
      RETURN

      ENTRY GET_MRX_EMISSION_MARKET_LINK(PROD_POINTER)

         GET_MRX_EMISSION_MARKET_LINK = &
                S_EMISSION_MARKET_LINK(PROD_POINTER)
      RETURN


      ENTRY S_YES_MONTHLY_MUST_RUN(PROD_POINTER,R_MONTH)

         L_S = PROD_PNTR_BY_CL_UNITS(PROD_POINTER)
         S_YES_MONTHLY_MUST_RUN = .FALSE.
         IF(ns_screen_data%LDTYPE(L_S) == 'N' .OR. &
                                      ns_screen_data%LDTYPE(L_S) == 'M') THEN
            S_YES_MONTHLY_MUST_RUN = .TRUE.
         ELSEIF(MONTHLY_MUST_RUN_VECTOR(L_S) /= 0) THEN
            tR4 = GET_VAR( &
                   FLOAT(MONTHLY_MUST_RUN_VECTOR(L_S)),R_MONTH, &
                                                 S_UNITNM(L_S))
            IF(tR4 > 0.) THEN
               S_YES_MONTHLY_MUST_RUN = .TRUE.
            ENDIF
         ENDIF
      RETURN

      ENTRY GET_S_CAP_PLANNING_FAC(PROD_POINTER,R_YEAR)

         L_S = PROD_PNTR_BY_CL_UNITS(PROD_POINTER)
         IF(S_CAP_PLANNING_FAC(L_S) < 0.) THEN
            GET_S_CAP_PLANNING_FAC = &
                        GET_VAR(S_CAP_PLANNING_FAC(L_S), &
                                              R_YEAR, &
                                                 S_UNITNM(L_S))
         ELSE
            GET_S_CAP_PLANNING_FAC = S_CAP_PLANNING_FAC(L_S)
         ENDIF
      RETURN

      ENTRY GET_S_START_UP_COSTS(R_YEAR,PROD_POINTER,R_MONTH)

         L_S = PROD_PNTR_BY_CL_UNITS(PROD_POINTER)
         IF(START_UP_COSTS(L_S) < 0.) THEN
            GET_S_START_UP_COSTS = &
                       START_UP_COST_ESCALAT_MTHLY_VAL( &
                                INT2(ABS(START_UP_COSTS(L_S))), &
                                R_YEAR)
         ELSE

            IF(START_UP_COSTS_ESCALATION(L_S) < 0.) THEN
               GET_S_START_UP_COSTS = 1.
               VECTOR_IS_VALUE = .FALSE.
               DO YR = R_YEAR ,1,-1
                 TEMP_I2 = INT2(START_UP_COSTS_ESCALATION(L_S))
                  ESCAL_RATE = RET_VAL_FROM_ESCALATION_VECTOR( &
                                    YR,TEMP_I2,VECTOR_IS_VALUE,R_MONTH)
                  IF(VECTOR_IS_VALUE) THEN
                     GET_S_START_UP_COSTS = &
                        GET_S_START_UP_COSTS * ESCAL_RATE
                     EXIT
                  ELSE
                     GET_S_START_UP_COSTS = GET_S_START_UP_COSTS * &
                                                (1.0 + ESCAL_RATE/100.)
                  ENDIF
               ENDDO ! YR
               IF(.NOT. VECTOR_IS_VALUE) THEN
                  GET_S_START_UP_COSTS = GET_S_START_UP_COSTS * &
                                            START_UP_COSTS(L_S)
               ENDIF
            ELSEIF(START_UP_COSTS_ESCALATION(L_S) > 0.) THEN
               ESCAL_RATE =  START_UP_COSTS_ESCALATION(L_S)/100.
               GET_S_START_UP_COSTS = START_UP_COSTS(L_S)
               DO YR = 1, R_YEAR
                  GET_S_START_UP_COSTS = GET_S_START_UP_COSTS * &
                                                     (1.0 + ESCAL_RATE)
               ENDDO
            ELSE
               GET_S_START_UP_COSTS = START_UP_COSTS(L_S)
            ENDIF
         ENDIF
      RETURN

      ENTRY GET_S_MIN_DOWN_TIME(PROD_POINTER)

         L_S = PROD_PNTR_BY_CL_UNITS(PROD_POINTER)
         GET_S_MIN_DOWN_TIME = MIN_DOWN_TIME(L_S)
      RETURN

      ENTRY GET_S_MIN_UP_TIME(PROD_POINTER)

         L_S = PROD_PNTR_BY_CL_UNITS(PROD_POINTER)
         GET_S_MIN_UP_TIME = MIN_UP_TIME(L_S)
      RETURN

      ENTRY GET_NEW_UNIT_FUEL_DELIVERY( &
                                PROD_POINTER, &
                                R_FUEL_DELIVERY_1, &
                                R_FUEL_DELIVERY_2, &
                                R_FUEL_DELIVERY_3)

         L_S = PROD_PNTR_BY_CL_UNITS(PROD_POINTER)
         R_FUEL_DELIVERY_1 = P_FUEL_DELIVERY(L_S)
         R_FUEL_DELIVERY_2 = P_FUEL_DELIVERY_2(L_S)
         R_FUEL_DELIVERY_3 = P_FUEL_DELIVERY_3(L_S)
         GET_NEW_UNIT_FUEL_DELIVERY = S_PBTUCT(L_S)
      RETURN

      ENTRY RETURN_S_TRANS_GROUP_ID(PROD_POINTER)
        if(file_trace_rstg==0) then
            file_trace_rstg=open_trace("return_s_trans_group_id.trace", &
                rq_rstd)
        end if
        call write_trace_int2(file_trace_rstg, "1. PROD_POINTER", &
            PROD_POINTER)
        


         

         ! prod_pntr_by_cl_units is good.
         ! Check bounds
         ubpp=ubound(prod_pntr_by_cl_units,1)
         lbpp=lbound(prod_pntr_by_cl_units,1)
         if(lbpp>prod_pointer.or.ubpp<prod_pointer) then
            call end_program("cl_screen_data_ext:0001 - " // &
                "range check error on prod_pntr_by_cl_units(" // &
                "("// trim(itos(lbpp)) // ":" // trim(itos(ubpp)) // &
                ") using index of " // trim(itos(int(prod_pointer))))
         end if


        call write_trace_int2(file_trace_rstg, "l_s", l_s)
        
         l_s=PROD_PNTR_BY_CL_UNITS(PROD_POINTER)
         call write_trace_int2(file_trace_rstg, "returning", &
            S_TRANSACTION_GROUP_ID(L_S))
            
         RETURN_S_TRANS_GROUP_ID = S_TRANSACTION_GROUP_ID(L_S)
      RETURN

      ENTRY UPDATE_CL_SCREEN_COSTS(R_YEAR)


         DO S = 1, SUNITS
            S_FIXED_COST_IN(S) = get_escalated_value( &
       S_FIXED_COST_IN(S), &
       S_FIXED_COST_ESCALATOR(S))
            S_ANNUAL_CL_FIXED_COST(S) = &
       get_escalated_value(S_ANNUAL_CL_FIXED_COST(S), &
       S_ANNUAL_CL_FIXED_COST_ESC(S))

       S_PBTUCT(S) = get_escalated_value(S_PBTUCT(S), &
       S_PFESCR(S))
       S_SBTUCT(S) = get_escalated_value(S_SBTUCT(S), &
       S_SFESCR(S))

       S_VCPMWH_IN(S) = get_escalated_value(S_VCPMWH_IN(S), &
       S_OMESCR(S))
            RTEMP = S_FUELADJ_IN(S)
            IF(RTEMP < 0.0) THEN
               RTEMP = GET_VAR(RTEMP,R_YEAR,S_UNITNM(S))
               IF(RTEMP < 0.) THEN
                  S_FUELADJ(S) = GET_VAR(RTEMP,ISEAS,S_UNITNM(S))
               ELSE
                  S_FUELADJ(S) = RTEMP
               ENDIF
            ELSE
               S_FUELADJ(S) = RTEMP
            ENDIF

         ENDDO

         UPDATE_CL_SCREEN_COSTS = SUNITS
      RETURN

      ENTRY RETURN_CL_SCREEN_FIXED_COST(PROD_POINTER)

         L_S = PROD_PNTR_BY_CL_UNITS(PROD_POINTER)
         RETURN_CL_SCREEN_FIXED_COST = &
             S_FIXED_COST_IN(L_S) * (S_INPUT_MW(2,L_S)*1000.) + &
             S_ANNUAL_CL_FIXED_COST(L_S) * 1000000.
      RETURN

      ENTRY PUT_MRX_DELIVERY_COST(R_DELIVERY_COST)

         SAVE_DELIVERY_COST = R_DELIVERY_COST
         PUT_MRX_DELIVERY_COST = R_DELIVERY_COST
      RETURN

      ENTRY RETURN_CL_SCREEN_EFOR(PROD_POINTER,R_YEAR,R_MONTHLY)
         if(r_year==2 .and. prod_pointer==7) then
            r_year=r_year ! Debugstop
            
         end if
         if(file_num_rc==0) then
            file_num_rc=open_trace("rtn_CL_SCREEN_EFOR.trace", rq_cls_efor)
            call write_trace_message(file_num_rc, "First call to RETURN_CL_SCREEN_EFOR")
            call write_trace_int2(file_num_rc, "PROD_POINTER", PROD_POINTER)
         end if
         
        call write_trace_int2(file_num_rc, "PROD_POINTER", PROD_POINTER)
        call write_trace_int2(file_num_rc, "R_YEAR", R_YEAR)
        call write_trace_real4s(file_num_rc, "R_MONTHLY", R_MONTHLY)
         
         L_S = PROD_PNTR_BY_CL_UNITS(PROD_POINTER)
         call write_trace_int2(file_num_rc, "Set L_S", L_S)
         
         call write_trace_real4(file_num_rc, "S_EFOR(L_S)", S_EFOR(L_S))
         
         
         IF(S_EFOR(L_S) < 0.) THEN
            get_var_called_from="cl_screen_data_ext:634"
            RETURN_CL_SCREEN_EFOR = &
                    GET_VAR(S_EFOR(L_S),R_YEAR,S_UNITNM(L_S))/100.
         ELSE
            RETURN_CL_SCREEN_EFOR = S_EFOR(L_S)/100.
            
            call write_trace_real4(file_num_rc, "RETURN_CL_SCREEN_EFOR set " // &
                "without GET_VAR to", &
                RETURN_CL_SCREEN_EFOR)
         ENDIF

!  MONTH.

         tR4 = 0.0
         R_MONTHLY = 0.0
         DO I = 1, 12
             call write_trace_int2(file_num_rc, &
             "S_MONTHLY_CAPACITY_POINTER(L_S)=", &
                S_MONTHLY_CAPACITY_POINTER(L_S))
                


            call write_trace_int2(file_num_rc, "L_S", L_S)
            
            IF(S_MONTHLY_CAPACITY_POINTER(L_S) < 0) THEN
            ! Right calls get_var from here and left never does
               get_var_called_from="cl_screen_data_ext:645"
               
               call write_trace_string(file_num_rc, &
                "get_var_called_from", get_var_called_from)
                
               TEMP_MOVAL = GET_VAR(FLOAT( &
                  S_MONTHLY_CAPACITY_POINTER(L_S)),I,S_UNITNM(L_S))
            ELSE
      call write_trace_message(file_num_rc, &
        "GV not called from cl_screen_data_ext:646")
        
               TEMP_MOVAL = 1.0
            ENDIF

            TEMP_MOVAL = TEMP_MOVAL * &
                (1.0 - MIN(100.,S_MNRATE(L_S,I))/100.)
            R_MONTHLY(I) = TEMP_MOVAL
            tR4 = tR4 + TEMP_MOVAL
         ENDDO
         RETURN_CL_SCREEN_EFOR = 1.0 -(1.0 - RETURN_CL_SCREEN_EFOR) * &
                                                           tR4/12.0
         RETURN_CL_SCREEN_EFOR = &
                             MAX(0.0,MIN(RETURN_CL_SCREEN_EFOR,0.9999))

      RETURN

      ENTRY GET_S_PRIMARY_MOVER_INDEX(PROD_POINTER)

         L_S = PROD_PNTR_BY_CL_UNITS(PROD_POINTER)
       get_PRIMARY_MOVER_INDEX_called_from="cl_screen_data_ext:0007"
      get_s_primary_mover_index=PRIMARY_MOVER_INDEX_DB(PRIMARY_MOVER_STR(L_S))
         

         IF(GET_S_PRIMARY_MOVER_INDEX == 10 .AND. &
                                        S_PRIMARY_MOVER(L_S) > 3 ) THEN
            GET_S_PRIMARY_MOVER_INDEX = 7
         ENDIF
      RETURN

      ENTRY GET_S_PRIMARY_MOVER(PROD_POINTER)

         IF(PROD_POINTER > PROD_PNTR) THEN
            WRITE(4,*) &
              'MRX POINTER OUT OF RANGE WHEN FINDING PRIMARY MOVER (1)'
         ENDIF
         L_S = PROD_PNTR_BY_CL_UNITS(PROD_POINTER)
         IF(L_S < 1) THEN
            WRITE(4,*) &
             'MRX POINTER OUT OF RANGE WHEN FINDING PRIMARY MOVER. (2)'
            WRITE(4,*) &
                  'MAY BE DUE TO HARD-WIRED UNIT. 111413.'
         ENDIF
         GET_S_PRIMARY_MOVER = S_PRIMARY_MOVER(L_S)
      RETURN

      ENTRY RETURN_CL_SCREEN_VARI_COST(PROD_POINTER,R_YEAR,R_MO)

         L_S = PROD_PNTR_BY_CL_UNITS(PROD_POINTER)

         IF(S_HR_FACTOR(L_S) < 0.) THEN
            HEAT_RATE_FACTOR = &
                    GET_VAR(S_HR_FACTOR(L_S),R_YEAR,S_UNITNM(L_S))
         ELSE
            HEAT_RATE_FACTOR = S_HR_FACTOR(L_S)
         ENDIF
         IF(S_FUELMX(L_S) <= -2.) THEN
            FUEL_MX_FACTOR = &
                       GET_VAR(S_FUELMX(L_S),R_YEAR,S_UNITNM(L_S))
         ELSE
            FUEL_MX_FACTOR = S_FUELMX(L_S)
         ENDIF

         FT = GET_PRIMARY_MOVER(L_S)

         IF(FT > 3 .OR. FT < 1) FT = 1
         MO = R_MO

         FUEL_SCEN_MULT = 1.0
         IF(FT == 1) THEN
            FUEL_SCEN_MULT = GET_SCENARIO_COAL_PRICES(R_YEAR,MO)
         ELSEIF(FT == 2) THEN
            FUEL_SCEN_MULT =  GET_SCENARIO_GAS_PRICES(R_YEAR,MO)
         ELSEIF(FT == 3) THEN
            FUEL_SCEN_MULT =  GET_SCENARIO_OIL_PRICES(R_YEAR,MO)
         ELSEIF(FT == 4) THEN
            FUEL_SCEN_MULT =  GET_SCENARIO_URANIUM_PRICES(R_YEAR,MO)
         ENDIF

         IF(SAVE_DELIVERY_COST == -999999.) THEN
            FUEL_COMPONENT = .001 * AHR(L_S) * HEAT_RATE_FACTOR * &
               (ABS(FUEL_MX_FACTOR) * S_PBTUCT(L_S) + &
                            ABS(1.-FUEL_MX_FACTOR) * S_SBTUCT(L_S))
         ELSE
            FUEL_COMPONENT = .001 * AHR(L_S) * HEAT_RATE_FACTOR * &
                                                     SAVE_DELIVERY_COST
         ENDIF
         OTHER_VARIABLE_COMPONENT = &
                                 S_VCPMWH_IN(L_S) + S_FUELADJ(L_S)

         RETURN_CL_SCREEN_VARI_COST = &
                     8.76*( FUEL_COMPONENT + OTHER_VARIABLE_COMPONENT )

      RETURN

      ENTRY RETURN_OTHER_VAR_COMPONENT(PROD_POINTER)

         L_S = PROD_PNTR_BY_CL_UNITS(PROD_POINTER)
         RETURN_OTHER_VAR_COMPONENT = &
                                 S_VCPMWH_IN(L_S) + S_FUELADJ(L_S)
      RETURN

      ENTRY RETURN_CL_SCREEN_EMIS_COST(PROD_POINTER,R_YEAR,R_MO, &
                                       R_NOX_MULT,R_SOX_MULT, &
                                       R_CO2_MULT,R_HG_MULT, &
                                       R_OTHER3_MULT, &
                                       R_CO2_RETIREMENT_PRICE)

         L_S = PROD_PNTR_BY_CL_UNITS(PROD_POINTER)
         I = S_EMISSION_MARKET_LINK(L_S)

         EMIS_DISPATCH_1 = &
                         TRANS_MRX_DISPATCH_EMIS_ADDER(1,R_MO,R_YEAR,I)
         EMIS_DISPATCH_2 = &
                         TRANS_MRX_DISPATCH_EMIS_ADDER(2,R_MO,R_YEAR,I)
         IF(R_CO2_RETIREMENT_PRICE > -999.0) THEN
            EMIS_DISPATCH_3 = R_CO2_RETIREMENT_PRICE * &
                        GET_SCENARIO_CO2_PRICES(R_YEAR,R_MO)/2000.0
         ELSE ! INCLUDES CO2 SCENARIO MULT
            EMIS_DISPATCH_3 = &
                         TRANS_MRX_DISPATCH_EMIS_ADDER(3,R_MO,R_YEAR,I)
         ENDIF
         EMIS_DISPATCH_4 = &
                         TRANS_MRX_DISPATCH_EMIS_ADDER(4,R_MO,R_YEAR,I)
         EMIS_DISPATCH_5 = &
                         TRANS_MRX_DISPATCH_EMIS_ADDER(5,R_MO,R_YEAR,I)

         tR4 = &
                  EMIS_DISPATCH_1 * MAX(ns_screen_data%p_so2(L_S)* &
                    R_SOX_MULT,0.) + &
                  EMIS_DISPATCH_2 * &
                MAX(ns_screen_data%p_nox(L_S)*R_NOX_MULT,0.) + &
                  EMIS_DISPATCH_3 * MAX(P_PARTICULATES(L_S)* &
                                                   R_CO2_MULT,0.) + &
                  EMIS_DISPATCH_4 * MAX(P_EMIS_OTH2(L_S)* &
                                                    R_HG_MULT,0.) + &
                  EMIS_DISPATCH_5 * MAX(P_EMIS_OTH3(L_S)* &
                                                    R_OTHER3_MULT,0.)
        IF(S_HR_FACTOR(L_S) < 0.) THEN
            HEAT_RATE_FACTOR = &
                    GET_VAR(S_HR_FACTOR(L_S),R_YEAR,S_UNITNM(L_S))
         ELSE
            HEAT_RATE_FACTOR = S_HR_FACTOR(L_S)
         ENDIF

         RETURN_CL_SCREEN_EMIS_COST = &
                           .001 * AHR(L_S) * HEAT_RATE_FACTOR * &
                                                                tR4

       RETURN

      ENTRY GET_CL_CO2_TON_PER_MWH( &
                                R_YEAR, &
                                PROD_POINTER, &
                                R_DATE)

         L_S = PROD_PNTR_BY_CL_UNITS(PROD_POINTER)
         IF(S_HR_FACTOR(L_S) < 0.) THEN
            HEAT_RATE_FACTOR = &
                    GET_VAR(S_HR_FACTOR(L_S),R_YEAR,S_UNITNM(L_S))
         ELSE
            HEAT_RATE_FACTOR = S_HR_FACTOR(L_S)
         ENDIF
         IF(R_DATE >= CO2_CONTROL_DATE(L_S)) THEN
            GET_CL_CO2_TON_PER_MWH = &
               MAX(P_PARTICULATES(L_S)*CO2_CONTROL_PERCENT(L_S),0.)
         ELSE
            GET_CL_CO2_TON_PER_MWH = MAX(P_PARTICULATES(L_S),0.)
         ENDIF
            GET_CL_CO2_TON_PER_MWH = GET_CL_CO2_TON_PER_MWH * &
                                          HEAT_RATE_FACTOR * &
                                             0.0000005 * AHR(L_S)
      RETURN

      ENTRY RETURN_CL_SCREEN_FUEL_COST(PROD_POINTER, &
                                       R_YEAR, &
                                       R_HEAT_RATE, &
                                       R_FUEL_COST)

         L_S = PROD_PNTR_BY_CL_UNITS(PROD_POINTER)

         IF(S_HR_FACTOR(L_S) < 0.) THEN
            HEAT_RATE_FACTOR = &
                    GET_VAR(S_HR_FACTOR(L_S),R_YEAR,S_UNITNM(L_S))
         ELSE
            HEAT_RATE_FACTOR = S_HR_FACTOR(L_S)
         ENDIF
         IF(S_FUELMX(L_S) <= -2.) THEN
            FUEL_MX_FACTOR = &
                       GET_VAR(S_FUELMX(L_S),R_YEAR,S_UNITNM(L_S))
         ELSE
            FUEL_MX_FACTOR = S_FUELMX(L_S)
         ENDIF
         R_HEAT_RATE = AHR(L_S) * HEAT_RATE_FACTOR
         IF(SAVE_DELIVERY_COST == -999999.) THEN
            R_FUEL_COST = &
               (ABS(FUEL_MX_FACTOR) * S_PBTUCT(L_S) + &
                             ABS(1.-FUEL_MX_FACTOR) * S_SBTUCT(L_S))
         ELSE
            R_FUEL_COST = SAVE_DELIVERY_COST
         ENDIF
         FUEL_COMPONENT = R_FUEL_COST * R_HEAT_RATE * .001

         RETURN_CL_SCREEN_FUEL_COST = FUEL_COMPONENT

      RETURN
      END function cl_screen_data


      FUNCTION CAP_MARKET_MONTH_NO_INDEX(R_UNIT_STR)

      use foshydi2com
      implicit none
      INTEGER*2 CAP_MARKET_MONTH_NO_INDEX
      CHARACTER*5 R_UNIT_STR
! END DATA DECLARATIONS
         CAP_MARKET_MONTH_NO_INDEX = 1
         SELECT CASE(trim(R_UNIT_STR))
            CASE ('JAN')
               CAP_MARKET_MONTH_NO_INDEX = 1
            CASE ('FEB')
               CAP_MARKET_MONTH_NO_INDEX = 2
            CASE ('MAR')
               CAP_MARKET_MONTH_NO_INDEX = 3
            CASE ('APR')
               CAP_MARKET_MONTH_NO_INDEX = 4
            CASE ('MAY')
               CAP_MARKET_MONTH_NO_INDEX = 5
            CASE ('JUN')
               CAP_MARKET_MONTH_NO_INDEX = 6
            CASE ('JUL')
               CAP_MARKET_MONTH_NO_INDEX = 7
            CASE ('AUG')
               CAP_MARKET_MONTH_NO_INDEX = 8
            CASE ('SEP')
               CAP_MARKET_MONTH_NO_INDEX = 9
            CASE ('OCT')
               CAP_MARKET_MONTH_NO_INDEX = 10
            CASE ('NOT')
               CAP_MARKET_MONTH_NO_INDEX = 11
            CASE ('DEC')
               CAP_MARKET_MONTH_NO_INDEX = 12
            CASE ('MONTH')
               CAP_MARKET_MONTH_NO_INDEX = 13
         END SELECT
      RETURN
      END
!     ****************************************************************

! Primary_mover_index_db routine moved to prim_mover_idx module.c-----
      subroutine CheckO3P(c_0,C_1,C_2,C_3,x0,x1)
! Greg:  feel free to rename this as desired
!     determines whether cubic curve y=c0+c1*x+c2*x^2+c3*x^3 is
! monotonically
!     increasing (i.e., has positive 2nd derivative at the left endpoint
!  x0);
!     if not, it linearizes the curve between the endpoints x0,x1.  The
!     curve's 1st derivative is s(x)=c1+2*c2*x+3*c3*x^2; the
!     curve's 2nd derivative is t(x)=2*c2+6*c3*x.  Note that for the
! curves
!     of interest here (heat input as y versus MW load output as x),
! we check
!     for the concave-upward property only at the left (minimal-load)
! point.
!     Attempts at simplifying the curve by merely zeroing c3, or by
! assigning
!     c3 to the value at which t0==0, both failed to approximate the
! curve.
!     notes by AGT on 20030728
      real*4 x0,x1,y0,y1,s0,s1,t0,C_0,C_1,C_2,C_3
      logical*1 MonotoneIncr

      s0=c_1+x0*(2.0*c_2+x0*3.0*c_3)
      s1=c_1+x1*(2.0*c_2+x1*3.0*c_3)
      t0=2.0*c_2+6.0*c_3*x0
      MonotoneIncr=(s0>0.0).and.(s1>0.0).and.(t0>0.0)
      if(.not.MonotoneIncr .and. x1 > x0) then
        ! linearize y(x) on the domain [x0,x1]
        y0=c_0+x0*(c_1+x0*(c_2+x0*c_3))
        y1=c_0+x1*(c_1+x1*(c_2+x1*c_3))
        c_1=(y1-y0)/(x1-x0)
        c_0= y1-c_1*x1
        c_2=0.0
        c_3=0.0
      end if
      RETURN
      end subroutine CheckO3P
