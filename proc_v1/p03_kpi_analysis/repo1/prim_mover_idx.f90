module prim_mover_idx
use miscmod
use params
use logging
use end_routine
use pms
use data_integrity
use debugtrace

implicit none
logical :: SawAC=.false., SawBlank=.false.
contains
! Moved from Cla_objt
FUNCTION PRIMARY_MOVER_INDEX_DB(r_unit_str)
    use debugtrace
    use program_state
    character(len=6) :: r_unit_str, unit_str
    integer (kind=2) :: primary_mover_index_db, return_value
    integer :: index, charval
    integer, save :: file_trace_pmidb=0
    
    if(file_trace_pmidb==0) then
        file_trace_pmidb=open_trace("primary_mover_index_db.trace", rq_pmidb)
    end if
    
    call remove_leading_nulls(r_unit_str, 6)

    
    call write_trace_string(file_trace_pmidb, "r_unit_str", r_unit_str)
    call write_trace_string(file_trace_pmidb, "CF:", &
        GET_PRIMARY_MOVER_INDEX_called_from)
    if (trim(GET_PRIMARY_MOVER_INDEX_called_from)=="") then
        stop "called from"
    end if
    GET_PRIMARY_MOVER_INDEX_called_from=""
    

      unit_str=r_unit_str
      
        ! redirect new PMs to default for test binary.
        if(trim(unit_str)=="" .or. len_trim(unit_str)==0 .or. &
           unit_str=="" .or. unit_str=="      " .or. unit_str==" ") then
            return_value = 13
        else
! END DATA DECLARATIONS 
         return_value = 13
         SELECT CASE(trim(UNIT_STR))
            CASE ('CC') ! Combined Cycle
               return_value = 1
            CASE ('FC') ! Biomass
               return_value = 2
            CASE ('GE') ! Geothermal
               return_value = 3
            CASE ('GT') ! combustion (Gas) Turbine
               return_value = 4
            CASE ('HY') 
               return_value = 5
            CASE ('IC') ! Internal Combustion
               return_value = 6
            CASE ('NA') ! Other
               return_value = 7  ! Other
            CASE ('NU')
               return_value = 8
            CASE ('SL') ! Solar
               return_value = 9
            CASE ('ST') 
               return_value = 10
            CASE ('WT') ! Wind Turbine
               return_value = 11
            CASE ('ZZ') ! Landfill synGas
               return_value = 12
            ! See top of routine and empty string case for 13.
            CASE ('BI') 
               return_value = 14
            CASE ('LF') 
               return_value = 15
            CASE ('BA') 
               return_value = 16
            CASE ('DG') ! Distributed Generation
               return_value = 17
            CASE ('OW') ! Offshore Wind
               return_value = 18
            CASE ('HB')
               return_value = 19
            CASE ('H2')
               return_value=20
            CASE ('CS') ! Carbon capture/sequestration
                return_value=21
            
            CASE ('  ',' ','     ', '')
               return_value = 13

            ! TODO:  Ask Greg about "BLANK"
            case ('PMNONE')
               return_value=return_value ! Redundant
               if(.not. SawBlank) then
                   SawBlank=.true.
                   log_message="Assigning default (13) to unknown " // &
                   "PM string 'PMNONE'."
                   call write_log_entry("prim_mover_idx:0002", &
                   trim(log_message))
                endif
            ! TODO:  Ask Greg about "AC"
            CASE ('AC')
               return_value=return_value ! Redundant
               if(.not.SawAC) then
                  SawAC=.true.
                  log_message="Assigning default (13) to unknown "//&
                   "PM string 'AC'."
                   call write_log_entry("prim_mover_idx:0003", &
                   trim(log_message))
               endif
            CASE default
                return_value=13 
         END SELECT
         end if
         call write_trace_int2(file_trace_pmidb, "returning", return_value)
       PRIMARY_MOVER_INDEX_DB=return_value
    
end FUNCTION PRIMARY_MOVER_INDEX_DB


      FUNCTION FUEL_TYPE_2_PRIM_MOVER(R_PRIM_FUEL_TYPE_STR)
!
      use debugtrace
      
      implicit none
      integer, save :: file_trace_pm=0
      
      
      LOGICAL (KIND=1)  :: LEGACY_PRIMARY_MOVER,GET_LEGACY_PRIMARY_MOVER
      INTEGER (KIND=2)  :: FUEL_TYPE_2_PRIM_MOVER
      CHARACTER (LEN=6) :: R_PRIM_FUEL_TYPE_STR
!
! END DATA DECLARATIONS
!
       
       if(file_trace_pm==0) then
        file_trace_pm=open_trace("prime_mover.trace", rq_pm)
       end if
       
       call write_trace_string(file_trace_pm, "FTS:", R_PRIM_FUEL_TYPE_STR)
       
       
            IF(             R_PRIM_FUEL_TYPE_STR == 'ANT   ' .OR.      & !  Anthracite Coal
                            R_PRIM_FUEL_TYPE_STR == 'BC    ' .OR.      & !  Beneficiated Coal
                            R_PRIM_FUEL_TYPE_STR == 'BIT   ' .OR.      & !  Bituminous Coal
                            R_PRIM_FUEL_TYPE_STR == 'COL   ' .OR.      & !  Coal
                            R_PRIM_FUEL_TYPE_STR == 'LIG   ' .OR.      & !  Lignite Coal
                            R_PRIM_FUEL_TYPE_STR == 'SC    ' .OR.      & !  Coal Based Synfuel
                            R_PRIM_FUEL_TYPE_STR == 'SUB   ' .OR.      & !  Subbituminous Coal
                            R_PRIM_FUEL_TYPE_STR == 'WC    ' .OR.      & !  Waste Coal
                            R_PRIM_FUEL_TYPE_STR == 'COAL  ' .OR.      & !  Coal Fuel Category
                            R_PRIM_FUEL_TYPE_STR == 'C     ' ) THEN    ! Legacy Coal Fuel Category
               FUEL_TYPE_2_PRIM_MOVER = 1                         ! Coal
            ELSEIF(         R_PRIM_FUEL_TYPE_STR == 'BFG   ' .OR.      & !  Blast Furnace Gas
                            R_PRIM_FUEL_TYPE_STR == 'COG   ' .OR.      & !  Coke Oven Gas
                            R_PRIM_FUEL_TYPE_STR == 'LNG   ' .OR.      & !  Liquefied Natural Gas
                            R_PRIM_FUEL_TYPE_STR == 'LPG   ' .OR.      & !  Liquefied Petroleum Gas
                            R_PRIM_FUEL_TYPE_STR == 'NG    ' .OR.      & !  Natural Gas
                            R_PRIM_FUEL_TYPE_STR == 'OG    ' .OR.      & !  Other Gas
                            R_PRIM_FUEL_TYPE_STR == 'PG    ' .OR.      & !  Propane
                            R_PRIM_FUEL_TYPE_STR == 'RG    ' .OR.      & !  Refinery Gas
                            R_PRIM_FUEL_TYPE_STR == 'SGC   ' .OR.      & ! Synthet !  Natural Gas
                            R_PRIM_FUEL_TYPE_STR == 'SNG   ' .OR.      & ! Synthet !  Natural Gas
                            R_PRIM_FUEL_TYPE_STR == 'GAS   ' .OR.      & !  Gas Fuel Category
                            R_PRIM_FUEL_TYPE_STR == 'G     ' ) THEN    ! Legacy Gas Fuel Category
               FUEL_TYPE_2_PRIM_MOVER = 2                          ! Gas
            ELSEIF(         R_PRIM_FUEL_TYPE_STR == 'COK   ' .OR.      & !  Coker bi-product
                            R_PRIM_FUEL_TYPE_STR == 'CRU   ' .OR.      & !  Crude Oil
                            R_PRIM_FUEL_TYPE_STR == 'CTO   ' .OR.      & !  Coal Tar Oil
                            R_PRIM_FUEL_TYPE_STR == 'DFO   ' .OR.      & !  Distillate Fuel Oil
                            R_PRIM_FUEL_TYPE_STR == 'DSL   ' .OR.      & !  Diesel
                            R_PRIM_FUEL_TYPE_STR == 'FO1   ' .OR.      & !  No. 1 Fuel Oil
                            R_PRIM_FUEL_TYPE_STR == 'FO2   ' .OR.      & !  No. 2 Fuel Oil
                            R_PRIM_FUEL_TYPE_STR == 'FO3   ' .OR.      & !  No. 3 Fuel Oil
                            R_PRIM_FUEL_TYPE_STR == 'FO4   ' .OR.      & !  No. 4 Fuel Oil
                            R_PRIM_FUEL_TYPE_STR == 'FO5   ' .OR.      & !  No. 5 Fuel Oil
                            R_PRIM_FUEL_TYPE_STR == 'FO6   ' .OR.      & !  No. 6 Fuel Oil
                            R_PRIM_FUEL_TYPE_STR == 'JF    ' .OR.      & !  Jet Fuel
                            R_PRIM_FUEL_TYPE_STR == 'KER   ' .OR.      & !  Kerosene
                            R_PRIM_FUEL_TYPE_STR == 'OIL   ' .OR.      & ! Oil
                            R_PRIM_FUEL_TYPE_STR == 'RFO   ' .OR.      & !  Residual Fuel Oil
                            R_PRIM_FUEL_TYPE_STR == 'WO    ' .OR.      & !  Waste Oil
                            R_PRIM_FUEL_TYPE_STR == 'FO    ' .OR.      & !  Legacy Oil Fuel Category
                            R_PRIM_FUEL_TYPE_STR == 'O     ' ) THEN    ! Legacy Oil Fuel Category
               FUEL_TYPE_2_PRIM_MOVER = 3                          ! Oil
           ELSEIF(          R_PRIM_FUEL_TYPE_STR == 'URA   ' .OR.      & !  Uranium
                            R_PRIM_FUEL_TYPE_STR == 'UR    ' .OR.      & !  Nuclear Fuel Category
                            R_PRIM_FUEL_TYPE_STR == 'NUC   ' .OR.      & !  Nuclear Fuel Category
                            R_PRIM_FUEL_TYPE_STR == 'NU    ' .OR.      & !  Nuclear Fuel Category
                            R_PRIM_FUEL_TYPE_STR == 'N     ' ) THEN    ! Legacy Nuclear Fuel Category
               FUEL_TYPE_2_PRIM_MOVER = 4                          ! Nuclear
            ELSEIF(         R_PRIM_FUEL_TYPE_STR == 'WAT   ' ) THEN      ! Water
               FUEL_TYPE_2_PRIM_MOVER = 5                         ! Water
            ELSEIF(         R_PRIM_FUEL_TYPE_STR == 'MF    ' .OR.      & !  Multifuel
                            R_PRIM_FUEL_TYPE_STR == 'OTH   ' .OR.      & !  Other
                            R_PRIM_FUEL_TYPE_STR == 'PUR   ' .OR.      & !  Purchased Steam
                            R_PRIM_FUEL_TYPE_STR == 'TDF   ' .OR.      & !  Tires
                            R_PRIM_FUEL_TYPE_STR == 'WH    ' .OR.      & ! Waste Heat
                            R_PRIM_FUEL_TYPE_STR == 'PC    ' .OR.      & !  Petroleum Coke
                            R_PRIM_FUEL_TYPE_STR == 'N/A   ' ) THEN    ! Not Available
               FUEL_TYPE_2_PRIM_MOVER = 6                         ! Other
            ELSEIF(         R_PRIM_FUEL_TYPE_STR == 'AB    ' .OR.      & !  Agriculture Byproduct
                            R_PRIM_FUEL_TYPE_STR == 'BLQ   ' .OR.      & !  Black Liquor
                            R_PRIM_FUEL_TYPE_STR == 'EFL   ' .OR.      & !  E-Fuel
                            R_PRIM_FUEL_TYPE_STR == 'GEO   ' .OR.      & !  Geothermal Steam
                            R_PRIM_FUEL_TYPE_STR == 'LFG   ' .OR.      & !  Landfill Gas
                            R_PRIM_FUEL_TYPE_STR == 'MSB   ' .OR.      & !  Municipal Solid Waste - Biogenic
                            R_PRIM_FUEL_TYPE_STR == 'MSN   ' .OR.      & !  Municipal Solid Waste - Non-Biogenic
                            R_PRIM_FUEL_TYPE_STR == 'MSW   ' .OR.      & !  Municipal Solid Waste
                            R_PRIM_FUEL_TYPE_STR == 'MTE   ' .OR.      & !  Methane
                            R_PRIM_FUEL_TYPE_STR == 'MTH   ' .OR.      & !  Methanol
                            R_PRIM_FUEL_TYPE_STR == 'OBG   ' .OR.      & !  Biomass Gases
                            R_PRIM_FUEL_TYPE_STR == 'OBL   ' .OR.      & !  Biomass Liquids
                            R_PRIM_FUEL_TYPE_STR == 'OBS   ' .OR.      & !  Biomass Solids
                            R_PRIM_FUEL_TYPE_STR == 'REF   ' .OR.      & !  Refuse
                            R_PRIM_FUEL_TYPE_STR == 'SLW   ' .OR.      & !  Sludge Waste
                            R_PRIM_FUEL_TYPE_STR == 'WD    ' .OR.      & !  Wood
                            R_PRIM_FUEL_TYPE_STR == 'WDL   ' .OR.      & !  Wood Waste Liquids
                            R_PRIM_FUEL_TYPE_STR == 'WDS   ' ) THEN    ! Wood Waste Solids
               FUEL_TYPE_2_PRIM_MOVER = 7                         ! Renewable
            ELSEIF(         R_PRIM_FUEL_TYPE_STR == 'SUN   ' .OR.      & !  Solar Thermal
                            R_PRIM_FUEL_TYPE_STR == 'PV    ' .OR.      & !  Solar Photovaoltaic
                            R_PRIM_FUEL_TYPE_STR == 'SOL   ' ) THEN    ! Legacy Solar Renewable
               FUEL_TYPE_2_PRIM_MOVER = 8                         ! Solar Renewable
            ELSEIF(         R_PRIM_FUEL_TYPE_STR == 'WND   ' ) THEN    ! Wind
               FUEL_TYPE_2_PRIM_MOVER = 9                         ! Wind Renewable
            ELSEIF(         R_PRIM_FUEL_TYPE_STR == 'DSM   ' .OR. &
                            R_PRIM_FUEL_TYPE_STR == 'EES   ' ) THEN    ! 022708.
               FUEL_TYPE_2_PRIM_MOVER = 10                         ! DSM AND ENERGY EFFICIENCY STANDARDS
            ELSEIF(         R_PRIM_FUEL_TYPE_STR == 'MWH  ' ) THEN    ! 052817
               FUEL_TYPE_2_PRIM_MOVER = 11                         ! BATTERY
            ELSEIF(         R_PRIM_FUEL_TYPE_STR == 'NA   ' ) THEN     ! NA RESERVED TO IGNORE
               FUEL_TYPE_2_PRIM_MOVER = 0
            ELSE
               IF(GET_LEGACY_PRIMARY_MOVER()) THEN
                  FUEL_TYPE_2_PRIM_MOVER = 6
               ELSE
                  FUEL_TYPE_2_PRIM_MOVER = 0
               ENDIF
            ENDIF
            call write_trace_int2(file_trace_pm, "returning", &
                FUEL_TYPE_2_PRIM_MOVER)

      END function FUEL_TYPE_2_PRIM_MOVER



end module prim_mover_idx
