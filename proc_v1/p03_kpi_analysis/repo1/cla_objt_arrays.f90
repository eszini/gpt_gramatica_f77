MODULE CLA_OBJT_ARRAYS

         USE PROD_ARRAYS_DIMENSIONS
         use params
         use globecom
         use offline_trace
         implicit none
         INTEGER (KIND=2) :: OFF_LINE_MONTH(MAX_CL_UNITS),  &
                            OFF_LINE_YEAR(MAX_CL_UNITS), &
                            TRANSACTION_GROUP_ID(MAX_CL_UNITS), &
                            PRIMARY_MOVER(MAX_CL_UNITS), &
                            NEWGEN_INDEX(MAX_CL_UNITS)
         LOGICAL (KIND=1), SAVE :: RETIRE_THIS_UNIT(MAX_CL_UNITS)
         REAL (KIND=4), SAVE, ALLOCATABLE :: CL_ANN_CAP(:,:,:), &
                                             CL_TG_CAP(:,:,:,:), &
                                           NEWGEN_CAP_BY_INDEX(:,:,:), &
                                             CL_TG_RETIRE(:,:), &
                                          CL_ANNUAL_LOAD_REDUCTION(:), &
                                             CL_TG_AFTER_PEAK(:,:), &
                                             CL_TG_CAP_MARKET_MW(:), &
                                             CL_TG_CAP_MARKET_REV(:), &
                                             RETIRE_RETRO_CO2_PRICE(:)
         LOGICAL (KIND=1), SAVE :: RETROFIT_ACTIVE(MAX_CL_UNITS)
         INTEGER (KIND=2), SAVE :: TEMP_RETIRED_UNIT_NO(MAX_CL_UNITS), &
                                  TEMP_RETIRED_OFF_LINE(MAX_CL_UNITS), &
                                   CO2_CONTROL_DATE(MAX_CL_UNITS), &
                                  FIRST_RETIREMENT_YEAR(MAX_CL_UNITS), &
                             TEMP_FIRST_RETIREMENT_YEAR(MAX_CL_UNITS), &
                                   RPS_PROGRAM_NUMBER(MAX_CL_UNITS)
         INTEGER (KIND=2), SAVE, ALLOCATABLE :: RETIRE_OR_RETRO(:)
         INTEGER (KIND=2), SAVE :: INSTALLED_POINTER=0
        CHARACTER (LEN=1), SAVE :: RETIREMENT_CANDIDATE(MAX_CL_UNITS), &
                        TEMP_RETIREMENT_SWITCH(max_retirement_units), &
                                    RETROFIT_CANDIDATE(MAX_CL_UNITS)
         REAL (KIND=4), SAVE :: RETIREMENTS_CO2(MAX_CL_UNITS), &
                                RETIREMENTS_CO2_COST(MAX_CL_UNITS), &
                                RETIREMENTS_CAP_COST(MAX_CL_UNITS), &
                                RETIREMENTS_MW(MAX_CL_UNITS), &
                                RETIREMENTS_MW_RPT(MAX_CL_UNITS), &
                                ECON_RETIRE_MARGIN(MAX_CL_UNITS,0:4)
         REAL (KIND=4), SAVE :: TEMP_RETIREMENTS_CO2 &
            (max_retirement_units), &
                     TEMP_RETIREMENTS_CO2_COST(max_retirement_units), &
                     TEMP_RETIREMENTS_CAP_COST(max_retirement_units), &
                              TEMP_RETIREMENTS_MW(max_retirement_units)
    REAL (KIND=4), SAVE :: TEMP_RETIREMENTS_MWH(max_retirement_units), &
                                RETIREMENTS_MWH(MAX_CL_UNITS), &
                     TEMP_RETIREMENTS_CO2_PRICE(max_retirement_units), &
                                RETIREMENTS_CO2_PRICE(MAX_CL_UNITS), &
                                RETROFIT_CO2_PRICE(MAX_CL_UNITS), &
                                CO2_CONTROL_PERCENT(MAX_CL_UNITS), &
                                UNIT_Decision_CO2_Price(MAX_CL_UNITS)

    CONTAINS
!--------
        FUNCTION RETIREMENT_CO2_COST_PER_TON(R_UNIT_NO)
         use globecom
      REAL (KIND=4) :: RETIREMENT_CO2_COST_PER_TON, &
	  TRANS_DISPATCH_EMIS_ADDER
         INTEGER (KIND=2) :: R_UNIT_NO

            RETIREMENT_CO2_COST_PER_TON = 2000.* &
            TRANS_DISPATCH_EMIS_ADDER(3,0_2,globecom_year,R_UNIT_NO)
        END FUNCTION
!--------
        FUNCTION CL_CAPACITY_TEMP_RETIRE_UNIT(R_YEAR,R_UNIT_NO)
         USE PROD_ARRAYS_DIMENSIONS
         USE INTERNAL_OPERATION_SWITCHES
         use prod2com
         use foshydi2com
         use offline_trace
         INTEGER (KIND=2) :: R_YEAR,R_UNIT_NO, &
           CL_CAPACITY_TEMP_RETIRE_UNIT

         INTEGER (KIND=2) :: TEMP_OFLINE,I

           INSTALLED_POINTER = INSTALLED_POINTER + 1
           TEMP_RETIRED_UNIT_NO(INSTALLED_POINTER) = R_UNIT_NO
           TEMP_RETIRED_OFF_LINE(R_UNIT_NO)=OFLINE(R_UNIT_NO)
           
           IF(GRX_RETIRE_THIS_YEAR) THEN
              ! retires at the end of the current year
              TEMP_OFLINE = 100*(R_YEAR-1900) + 12   

              IF(OFLINE(R_UNIT_NO) < TEMP_OFLINE) THEN
                 TEMP_OFLINE = OFLINE(R_UNIT_NO)
              ENDIF
           ELSE
              ! retires at the end of the previous year.
              TEMP_OFLINE = 100*(R_YEAR-1-1900) + 12

           ENDIF
           OFLINE(R_UNIT_NO) = TEMP_OFLINE
           call write_offline_trace( &
            "CLA_OBJT_ARRAYS:0001", ofline(R_UNIT_NO))
            
           CL_CAPACITY_TEMP_RETIRE_UNIT = INSTALLED_POINTER
           RETIREMENT_CANDIDATE(R_UNIT_NO) = 'F'
           TEMP_FIRST_RETIREMENT_YEAR(INSTALLED_POINTER) = &
		   FIRST_RETIREMENT_YEAR(R_UNIT_NO)
           TEMP_RETIREMENTS_CO2(INSTALLED_POINTER) = &
		   RETIREMENTS_CO2(R_UNIT_NO)
           TEMP_RETIREMENTS_CO2_COST(INSTALLED_POINTER) = &
                                        RETIREMENTS_CO2_COST(R_UNIT_NO)
           TEMP_RETIREMENTS_CAP_COST(INSTALLED_POINTER) = &
                                         RETIREMENTS_CAP_COST(R_UNIT_NO)
           TEMP_RETIREMENTS_MW(INSTALLED_POINTER) = &
		   RETIREMENTS_MW(R_UNIT_NO)
           TEMP_RETIREMENTS_MWH(INSTALLED_POINTER) = &
		   RETIREMENTS_MWH(R_UNIT_NO)
           TEMP_RETIREMENTS_CO2_PRICE(INSTALLED_POINTER) = &
		   RETIREMENTS_CO2_PRICE(R_UNIT_NO)
         END FUNCTION
!--------
         FUNCTION RESET_CL_RETIRED_UNITS()
         USE PROD_ARRAYS_DIMENSIONS
         use foshydi2com
         INTEGER (KIND=2) :: RESET_CL_RETIRED_UNITS,I,UNIT_NO


         DO I = 1, INSTALLED_POINTER
            UNIT_NO = TEMP_RETIRED_UNIT_NO(I)
            OFLINE(UNIT_NO) = TEMP_RETIRED_OFF_LINE(UNIT_NO)
            
           call write_offline_trace( &
            "CLA_OBJT_ARRAYS:0002", ofline(UNIT_NO))
            
      RETIREMENT_CANDIDATE(UNIT_NO) = 'T'  ! TEMP_RETIREMENT_SWITCH(I)
            FIRST_RETIREMENT_YEAR(UNIT_NO) = &
			TEMP_FIRST_RETIREMENT_YEAR(I)
            RETIREMENTS_CO2(UNIT_NO) = TEMP_RETIREMENTS_CO2(I)
            RETIREMENTS_CO2_COST(UNIT_NO) = &
			TEMP_RETIREMENTS_CO2_COST(I)
            RETIREMENTS_CAP_COST(UNIT_NO) = &
			TEMP_RETIREMENTS_CAP_COST(I)
            RETIREMENTS_MW(UNIT_NO) = TEMP_RETIREMENTS_MW(I)
            RETIREMENTS_MWH(UNIT_NO) = TEMP_RETIREMENTS_MWH(I)
            RETIREMENTS_CO2_PRICE(UNIT_NO) = &
			TEMP_RETIREMENTS_CO2_PRICE(I)
            RETIREMENTS_MW_RPT(UNIT_NO) = RETIREMENTS_MW(UNIT_NO)

          write(9,*) "5825 Retirements",I,UNIT_NO, &
                                OFLINE(UNIT_NO), &
								RETIREMENT_CANDIDATE(UNIT_NO), &
                                RETIREMENTS_CO2(UNIT_NO), &
                                RETIREMENTS_MW_RPT(UNIT_NO), &
                                RETIREMENTS_MWH(UNIT_NO), &
                                RETIREMENTS_CO2_PRICE(UNIT_NO), &
                                RETIREMENTS_CO2_COST(UNIT_NO), &
                                RETIREMENTS_CAP_COST(UNIT_NO), &
                                RETIREMENT_CO2_COST_PER_TON( &
								UNIT_NO), &
                                FIRST_RETIREMENT_YEAR(UNIT_NO)
         ENDDO
         INSTALLED_POINTER = 0
         RESET_CL_RETIRED_UNITS = INSTALLED_POINTER
         END FUNCTION
!--------
        FUNCTION RESET_RETIRE_UPLIFT_PER_TON( &
		R_RETIREMENTS_UPLIFT_PER_TON)
           USE PROD_ARRAYS_DIMENSIONS
           REAL (KIND=4) :: R_RETIREMENTS_UPLIFT_PER_TON( &
		   MAX_CL_UNITS)
           INTEGER (KIND=2) :: RESET_RETIRE_UPLIFT_PER_TON,I,UNIT_NO
           DO I = 1, INSTALLED_POINTER
              UNIT_NO = TEMP_RETIRED_UNIT_NO(I)
              R_RETIREMENTS_UPLIFT_PER_TON(UNIT_NO) = &
			  TEMP_RETIREMENTS_CO2_PRICE(I)
           ENDDO
           RESET_RETIRE_UPLIFT_PER_TON = INSTALLED_POINTER
        END FUNCTION

        subroutine allocate_cl_tg_cap(study_period, upper_trans_group)
			integer (kind=2) :: MaxVariable
         integer (kind=2), intent(in) :: study_period, upper_trans_group
            if(allocated(CL_TG_CAP)) then
                deallocate(cl_tg_cap)
            endif
			MaxVariable = MAX(1,int (UPPER_TRANS_GROUP))

            ALLOCATE(cl_tg_cap(0:max_technology_counters, &
            0:MaxVariable, STUDY_PERIOD,2))

      end subroutine allocate_cl_tg_cap

      END MODULE CLA_OBJT_ARRAYS

