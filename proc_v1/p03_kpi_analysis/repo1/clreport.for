
        subroutine write_unique_msgmtrcl_log_entry(whichwrite)
        use miscmod
        use logging
        use icap_summary_report


        implicit none

        integer :: whichwrite
        character (len=1024) :: uniques(50)=""
        integer :: lastuq=0, index, foundindex
        logical :: found

        character (len=1024) :: outtext


            outtext="msgmtrcl write " // trim(itos(whichwrite))
            found=.false.
            foundindex=-1

            do index=1, lastuq
                if(trim(uniques(index))==outtext) then
                    foundindex=index
                    found=.true.
                    exit
                endif
            enddo

            if(.not. found) then
                lastuq=lastuq+1
                uniques(lastuq)=outtext
                call write_log_entry("clreport:0001", outtext)
            endif

        end subroutine write_unique_msgmtrcl_log_entry


!             CAPACITY LIMITED MONTHLY REPORT WRITER
!                   COPYRIGHT (C) 1987-2000
!               M.S. GERBER & ASSOCIATES, INC.
!                   ALL RIGHTS RESERVED
!

!

!
!             WRITES ANNUAL SUMMARY PRODUCTION UNIT REPORT
!                       COPYRIGHT (C) 1987
!                   M.S. GERBER & ASSOCIATES, INC.
!                       ALL RIGHTS RESERVED
!

!
      SUBROUTINE ANNUAL_REPORT(YR,CAP_LIMITED_DEMAND,
     +                         ENERGY_EXCHANGE_ADJUSTMENT,
     +                         BASE,PEAK,
     +                         NUMBER_OF_CONTRACTS,CNTRNM,
     +                         CNTR_ON_LI,CNTR_OFF_LI,KEPCO,
     +                         CNTR_WEIGHTED_CAPACITY,
     +                         ANNUAL_BTUS_GOCN12,
     +                         WABASH_VALLEY,REALLY_KEPCO,
     +                         CNTR_SALES_ENERGY,CNTR_SALES_REVENUE,
     +                         SAVE_YEAR)

      use mod_base_year
      use envircom
      use spindriftlib
      use prod_arrays_dimensions
      use procsave_common
      use annual_cl_unit
      use irec_endpoint_control
      use grx_planning_routines
      use annual_contracts
      use cl_data
      use mwunih
      use csvdat
      use sizecom
      use group_prod
      use cap_prod_costs
      use foshydi2com
      use aitvafin
      use enrg_prod_costsr8
      use prodcom
      use prod2com
      use poolcom
      use clreport_routines
      use cl_data
      use nunits_shared


      CHARACTER (LEN=2) :: CAPACITY_PLANNING_METHOD,GREEN_MRX_METHOD
!
      REAL (kind=8) ::  ANNUAL_BTUS_GOCN12(0:6)
      REAL ::  CL_UNIT_ANNUAL_FIXED_COST
      REAL ::  CL_UNIT_ANNUAL_FUEL_COST
      REAL ::  CL_UNIT_ANNUAL_VAR_COST
      LOGICAL (kind=1) ::  KEPCO
      LOGICAL (kind=1) ::  ANNUAL_OUTPUT_REPORT
      LOGICAL (kind=1) ::  ANNUAL_UNIT_OUTPUT_REPORT
      LOGICAL (kind=1) ::  WABASH_VALLEY
      LOGICAL (kind=1) ::  REALLY_KEPCO
      REAL ::  CNTR_SALES_REVENUE,CNTR_SALES_ENERGY
      CHARACTER (len=1) ::  UTILITY_TYPE
      CHARACTER (len=1) ::  COUNTRY
      CHARACTER (len=20) ::  UNIT_NAME
      CHARACTER (len=1) ::  KEPCO_REPORTS
      CHARACTER (len=1) ::  CSV_REPORT
      LOGICAL (kind=1) ::  CANADA
      INTEGER (kind=2) ::  I
      INTEGER (kind=2) ::  YR
      INTEGER (kind=2) ::  EM
      INTEGER (kind=2) ::  J
      INTEGER (kind=2) ::  NUMBER_OF_CONTRACTS
      INTEGER (kind=2) ::  RECORDS_PER_UNIT=0
      INTEGER (kind=2) ::  RECORDS_PRINTED_PER_UNIT=0
      INTEGER (kind=2) ::  LAST_YEAR,RUN_YEARS,SAVE_YEAR
      REAL ::  CAP_FACTOR
      INTEGER (kind=2) ::  CNTR_ON_LI(MAX_CONTRACTS)
      INTEGER (kind=2) ::  CNTR_OFF_LI(MAX_CONTRACTS)
      CHARACTER (len=20) ::   CNTRNM(MAX_CONTRACTS)
      INTEGER (kind=4) ::  OFFSET=0 ,IREC,NEXT_OFFSET=0

      LOGICAL (kind=1) ::  TVA
      REAL ::  AVERAGE_HEATRATE
      REAL ::  HEAT_CONVERSION
      REAL ::  BASE
      REAL ::  PEAK
      REAL ::  CONTRACT_LOAD_FACTOR
      REAL ::  CNTR_WEIGHTED_CAPACITY(MAX_CONTRACTS)
      REAL ::  GET_HEAT_CONVERSION
      CHARACTER (len=255) ::  PRODUCTION_METHOD
      CHARACTER (len=255) ::  RECORD
      CHARACTER (len=255) ::
     +             EMISSIONS_TITLES(NUMBER_OF_EMISSION_TYPES)*26,
     +             EMISSIONS_CAP_TITLES(NUMBER_OF_EMISSION_TYPES)*28
      CHARACTER (len=255) ::  NEW_LINE*2
      CHARACTER (len=255) ::  FILE_NAME*64
      CHARACTER (len=80) ::  PRODUCTION_METHOD_TITLE
      LOGICAL (kind=4) ::  FILE_EXIST
      LOGICAL (kind=1) ::  POOLING_TRANSACTIONS
      LOGICAL (kind=1) ::  FIRST_TIME_BY_UNIT=.TRUE.
      LOGICAL (kind=1) ::  FIRST_TIME_BY_YEAR=.TRUE.
      LOGICAL (kind=1) ::  SPREAD_SHEET
      LOGICAL (kind=1) ::  BY_UNIT
      LOGICAL (kind=1) ::  BY_YEAR
      REAL (kind=4) ::  ANNUAL_TOTAL_EMISSIONS(NUMBER_OF_EMISSION_TYPES)
      REAL (kind=4) ::  ANNUAL_AI_INVESTMENT,TOTAL_COST,AVERAGE_COST
      REAL ::    ENERGY_EXCHANGE_ADJUSTMENT
      REAL (kind=8) ::  CAP_LIMITED_DEMAND

      LOGICAL (kind=1) ::  ANN_CL_REPORT_NOT_OPEN=.TRUE.
      INTEGER (kind=2) ::  ANN_CL_UNIT_NO
      INTEGER (kind=2) ::  ANN_CL_UNIT_HEADER
      INTEGER (kind=2) ::  ANN_CL_SUM_NO
      INTEGER (kind=2) ::  ANN_CL_SUMMARY_HEADER
      INTEGER (kind=2) ::  ANN_POOL_SUM_NO
      INTEGER (kind=2) ::  ANN_POOL_SUMMARY_HEADER
      REAL (kind=8) ::  TEMP_DBLE
      REAL ::  N_A=-999999.,TOTAL_PRODUCTION_COSTS,ZERO=0.
      SAVE ANN_CL_UNIT_NO,ANN_CL_SUM_NO,ANN_POOL_SUM_NO
      INTEGER ::  ANN_CL_UNIT_REC,ANN_CL_SUM_REC,ANN_POOL_SUM_REC
      SAVE ANN_CL_UNIT_REC,ANN_CL_SUM_REC,ANN_POOL_SUM_REC

!     ADDED FOR MIDAS GOLD 6/5/91.

      LOGICAL (kind=1) ::  LOTUS_SPREAD_SHEET=.FALSE.

      EQUIVALENCE(RECORD(1:1),PRODUCTION_METHOD(1:1),FILE_NAME(1:1))
!
!
      if(yr==0) then
        call end_program("clreport:0009 - YR argument is zero.")
      end if
      
      TVA = UTILITY_TYPE() == 'T'
      LAST_YEAR = RUN_YEARS()
      ANNUAL_OUTPUT_REPORT = ANNUAL_UNIT_OUTPUT_REPORT()
      NEW_LINE(1:1) = CHAR(13)
      NEW_LINE(2:2) = CHAR(10)

      BY_UNIT = .FALSE.
      BY_YEAR = .FALSE.
!
      SPREAD_SHEET =  BY_UNIT .OR. BY_YEAR
      IF(FIRST_TIME_BY_UNIT .AND. CSV_BY_UNIT(0)) THEN
         FIRST_TIME_BY_UNIT = .FALSE.
         FILE_NAME = 'BYU'//trim(clData%Scename)//'.CSV'

         OPEN(97,FILE=FILE_NAME,ACCESS='DIRECT',STATUS='REPLACE',
     +                                       FORM='FORMATTED',RECL=200)
      ENDIF
      IF(FIRST_TIME_BY_YEAR .AND. BY_YEAR) THEN
         FIRST_TIME_BY_YEAR = .FALSE.
         FILE_NAME = 'BYY'//trim(clData%Scename)//'.CSV'

         OPEN(96,FILE=FILE_NAME,STATUS='REPLACE',
     +                              CARRIAGE CONTROL="FORTRAN",RECL=200)
      ENDIF
      IF(CSV_BY_UNIT(0) .AND. (YR == 1)) THEN
         IREC = OFFSET
         DO I = 1, get_nunits()

            IREC = IREC + 1
            WRITE(97,'(A,",",I4,",")',REC=IREC) NEW_LINE//
     +               '"'//trim(UNITNM(I))//'   Endpoint"',gc_end_point
            IREC = IREC + 1
            IF(TVA) THEN
               PRODUCTION_METHOD='"A&I","Cap Factor"'
            ELSE
               PRODUCTION_METHOD='"Cap Factor"'
            ENDIF
            WRITE(97,'(A)',REC=IREC) NEW_LINE//
     +          '"Year","Cap-MW","Gen-GWh","Ave Ht",'//
     +         '"Ave Cost","Fuel","Var","Fixed","Total","SO2","NOx",'//
     +         '"Oth 1","Oth 2","Oth 3",'//trim(PRODUCTION_METHOD)
            DO J = 1, LAST_YEAR
               IF(CSV_BY_UNIT(J)) THEN
                  IREC = IREC + 1
             WRITE(97,"(A,I5,',')",REC=IREC) NEW_LINE,J+get_BASE_YEAR()
               ENDIF
            ENDDO
         ENDDO
         DO I = 1, NUMBER_OF_CONTRACTS
!
            IREC = IREC + 1
            WRITE(97,'(A,I4)',REC=IREC) NEW_LINE//
     +                '"'//trim(CNTRNM(I))//'   Endpoint"',gc_end_point
            IREC = IREC + 1
            WRITE(97,'(A)',REC=IREC) NEW_LINE//
     +          '"Year","Cap-MW","Gen-GWh",'//
     +         '"Ave Cost","Var","Fixed","Total","SO2",'
            DO J = 1, LAST_YEAR
               IF(CSV_BY_UNIT(J)) THEN
                  IREC = IREC + 1
             WRITE(97,"(A,I5,',')",REC=IREC) NEW_LINE,J+get_BASE_YEAR()
               ENDIF
            ENDDO
         ENDDO
         NEXT_OFFSET = IREC + 1
         RECORDS_PER_UNIT = 0.
         RECORDS_PRINTED_PER_UNIT = 0
         DO J = 1, LAST_YEAR
            IF(CSV_BY_UNIT(J)) THEN
               RECORDS_PER_UNIT = RECORDS_PER_UNIT + 1
            ENDIF
         ENDDO
      ENDIF
!
! ADDED TO GET THE SPACING CORRECT ON CSV OUTPUT BY UNIT
!
      IF(CSV_BY_UNIT(YR)) RECORDS_PRINTED_PER_UNIT = 1 +
     +                                          RECORDS_PRINTED_PER_UNIT
! TODO: EXTRACT_METHOD set_emission_report_titles

      CALL SET_EMISSION_REPORT_TITLES(EMISSIONS_TITLES,
     +         EMISSIONS_CAP_TITLES,ANNUAL_AI_INVESTMENT,CANADA)
!
! TODO: EXTRACT_METHOD set_emission_report_titles (end)

      DO EM = 1, NUMBER_OF_EMISSION_TYPES
         ANNUAL_TOTAL_EMISSIONS(EM) = 0.
      ENDDO
      DATE1 = 100.*(get_BASE_YEAR()+YR-1900) + 1
      DATE2 = 100.*(get_BASE_YEAR()+YR-1900) + 12
      PRODUCTION_METHOD = PRODUCTION_METHOD_TITLE()

! TODO: Determine whether spreadsheet interface should be removed.
!
! SPREADSHEET INTERFACE
!
!$DEFINE LOTUS
!$IF DEFINED(LOTUS)
      IF(LOTUS_SPREAD_SHEET) THEN
      IF(SPREAD_SHEET .AND. BY_YEAR) THEN
       WRITE(96,1002) YR+get_BASE_YEAR(),' Annual Summary for Endpoint',
     +                  gc_end_point,trim(PRODUCTION_METHOD)
         PRODUCTION_METHOD = '" "," ","Total","------- Average -------"'
         WRITE(96,1001) trim(PRODUCTION_METHOD)
        PRODUCTION_METHOD='" ","Unit","Gener-","Heatrate"," ","Fuel",'//
     +    '"Variable","Fixed","Total",'//
     +    '"----------------------- Emissions ---------------------"'//
     +    ', , , , ,'
         IF(TVA) THEN
            PRODUCTION_METHOD = trim(PRODUCTION_METHOD)//
     +                                             '"TVA","Capacity",'
         ELSE
            PRODUCTION_METHOD = trim(PRODUCTION_METHOD)//
     +                                             '"Capacity",'
         ENDIF
         WRITE(96,1001) trim(PRODUCTION_METHOD)
         IF(CANADA) THEN
            PRODUCTION_METHOD='"","Cap","ation","(GJ/","Cost","Cost",'//
     +         '"Cost",'
         ELSE
            PRODUCTION_METHOD='" ","Cap","ation","(BTU/","Cost",'//
     +         '"Cost","Cost",'
         ENDIF
         PRODUCTION_METHOD = trim(PRODUCTION_METHOD)//
     +    '"Cost","Cost","SO2","NOx","CO2","Other 1","Other 2",'
         IF(TVA) THEN
            PRODUCTION_METHOD = trim(PRODUCTION_METHOD)//
     +                                      '"A&I","Factor",'
         ELSE
            PRODUCTION_METHOD = trim(PRODUCTION_METHOD)//
     +                                      '"Factor",'
         ENDIF
         WRITE(96,1001) trim(PRODUCTION_METHOD)
         IF(CANADA) THEN
            PRODUCTION_METHOD='"Unit Name","(MW)","(GWh)","MWh)",'//
     +         '"($/MWh)",'
         ELSE
            PRODUCTION_METHOD='"Unit Name","(MW)","(GWh)","KWh)",'//
     +         '"($/MWh)",'
         ENDIF
         PRODUCTION_METHOD = trim(PRODUCTION_METHOD)//
     +         '"(M$)","(M$)","(M$)","(M$)","(tons)","(tons)",'//
     +         '"(tons)","(tons)","(tons)",'
         IF(TVA) THEN
            PRODUCTION_METHOD = trim(PRODUCTION_METHOD)//
     +                                      '"(M$)","(%)",'
         ELSE
            PRODUCTION_METHOD = trim(PRODUCTION_METHOD)//
     +                                      '"(%)",'
         ENDIF
         WRITE(96,1001) trim(PRODUCTION_METHOD)
      ENDIF
!$ELSE
      ELSE
!
! THIS IS THE CODE BEFORE MODIFYING TO BE COMPATIBLE WITH LOTUS
!
      IF(SPREAD_SHEET .AND. BY_YEAR) THEN
       WRITE(96,1002) YR+get_BASE_YEAR(),' Annual Summary for Endpoint',
     +                  gc_end_point,trim(PRODUCTION_METHOD)
         PRODUCTION_METHOD = ' , ,Total,"------- Average -------"'
         WRITE(96,1001) trim(PRODUCTION_METHOD)
         PRODUCTION_METHOD=' ,Unit,"Gener-",Heatrate, ,Fuel,Variable,'//
     +      'Fixed,Total,'//
     +      '"----------------------- Emissions ---------------------"'
         IF(TVA)
     +             PRODUCTION_METHOD = trim(PRODUCTION_METHOD)//',TVA'
         WRITE(96,1001) trim(PRODUCTION_METHOD)
         IF(CANADA) THEN
            PRODUCTION_METHOD=' ,Cap,ation,"(GJ/",Cost,Cost,Cost,'
         ELSE
            PRODUCTION_METHOD=' ,Cap,ation,"(BTU/",Cost,Cost,Cost,'
         ENDIF
         PRODUCTION_METHOD = trim(PRODUCTION_METHOD)//
     +                 'Cost,Cost,SO2,NOx,"CO2","Other 1","Other 2"'
         IF(TVA)
     +      PRODUCTION_METHOD = trim(PRODUCTION_METHOD)//',"A&I"'
         WRITE(96,1001) trim(PRODUCTION_METHOD)
         IF(CANADA) THEN
            PRODUCTION_METHOD='"Unit Name",(MW),(GWh),(MWh),($/MWh),'
         ELSE
            PRODUCTION_METHOD='"Unit Name",(MW),(GWh),(KWh),($/MWh),'
         ENDIF
         PRODUCTION_METHOD = trim(PRODUCTION_METHOD)//
     +          '(M$),(M$),(M$),(M$),(tons),(tons),(tons),(tons),(tons)'
         IF(TVA)
     +            PRODUCTION_METHOD = trim(PRODUCTION_METHOD)//',(M$)'
         WRITE(96,1001) trim(PRODUCTION_METHOD)
      ENDIF
!$ENDIF
      ENDIF ! SPREAD SHEET TYPE
!
! 4/6/02. REMOVED ANNUAL CL REPORYT
!
      ANNUAL_OUTPUT_REPORT = .FALSE.
!
      IF(ANNUAL_OUTPUT_REPORT) THEN
         IF(ANN_CL_REPORT_NOT_OPEN) THEN
            ANN_CL_UNIT_NO = ANN_CL_UNIT_HEADER(ANN_CL_UNIT_REC)
            ANN_CL_SUM_NO = ANN_CL_SUMMARY_HEADER(ANN_CL_SUM_REC)
            IF(POOLING_TRANSACTIONS()) THEN
               ANN_POOL_SUM_NO = ANN_POOL_SUMMARY_HEADER(
     +                                                 ANN_POOL_SUM_REC)
            ENDIF
!
            ANN_CL_REPORT_NOT_OPEN = .FALSE.
         ENDIF
      ENDIF
      HEAT_CONVERSION = GET_HEAT_CONVERSION()
      DO I = 1, get_nunits()
         IREC = (I-1)*(RECORDS_PER_UNIT+2) + RECORDS_PRINTED_PER_UNIT +
     +                                                        2 + OFFSET
         IF (ONLINE(I) .LE. DATE2 .AND. OFLINE(I) .GE. DATE1) THEN
!

               UNIT_NAME = UNITNM(I)

            CL_UNIT_ANNUAL_FIXED_COST =
     +                             ANNUAL_CL_UNIT_FIXED_COST(I)/1000000.
            TOTAL_COST = 0.
            IF(ANNUAL_CL_UNIT_ENERGY(I) .NE. 0.) THEN
               TOTAL_COST = ANNUAL_CL_UNIT_FUEL_COST(I) +
     +                      ANNUAL_CL_UNIT_VAR_COST(I)
               AVERAGE_COST = TOTAL_COST/ANNUAL_CL_UNIT_ENERGY(I)
               TOTAL_COST = TOTAL_COST/1000000. +
     +                                         CL_UNIT_ANNUAL_FIXED_COST
               DO EM = 1, NUMBER_OF_EMISSION_TYPES
                  ANNUAL_TOTAL_EMISSIONS(EM)=ANNUAL_TOTAL_EMISSIONS(EM)+
     +                                    ANNUAL_CL_UNIT_EMISSIONS(EM,I)
               ENDDO
               CL_UNIT_ANNUAL_FUEL_COST =
     +                              ANNUAL_CL_UNIT_FUEL_COST(I)/1000000.
               CL_UNIT_ANNUAL_VAR_COST =
     +                              ANNUAL_CL_UNIT_VAR_COST(I)/1000000.
               AVERAGE_HEATRATE = HEAT_CONVERSION *
     +               (ANNUAL_CL_UNIT_MMBTUS(I)/ANNUAL_CL_UNIT_ENERGY(I))
               IF(MW(2,I) .GT. 0.0) THEN
                  CAP_FACTOR =  ANNUAL_CL_UNIT_ENERGY(I)/(87.60*MW(2,I))
               ELSE
                  CAP_FACTOR = 0.
               ENDIF
               IF(.NOT. CANADA) AVERAGE_HEATRATE = AVERAGE_HEATRATE + .5
                ! TODO: Remove spreadsheet code -- it's unused and
                ! probably no longer works.
               IF(SPREAD_SHEET) THEN
                  IF(BY_UNIT) THEN
                     IF(TVA) THEN
                        WRITE(RECORD,1013) YR+get_BASE_YEAR(),
     +                    MW(2,I),
     +                    ANNUAL_CL_UNIT_ENERGY(I)/1000.,
     +                    AVERAGE_HEATRATE,
     +                    AVERAGE_COST,
     +                    CL_UNIT_ANNUAL_FUEL_COST,
     +                    CL_UNIT_ANNUAL_VAR_COST,
     +                    CL_UNIT_ANNUAL_FIXED_COST,
     +                    TOTAL_COST,
     +                   (ANNUAL_CL_UNIT_EMISSIONS(J,I),
     +                                    J=1,NUMBER_OF_EMISSION_TYPES),
     +                    CL_AI_INVESTMENT(I)/1000000.,
     +                    CAP_FACTOR
                     ELSE
                        WRITE(RECORD,1013) YR+get_BASE_YEAR(),
     +                    MW(2,I),
     +                    ANNUAL_CL_UNIT_ENERGY(I)/1000.,
     +                    AVERAGE_HEATRATE,
     +                    AVERAGE_COST,
     +                    CL_UNIT_ANNUAL_FUEL_COST,
     +                    CL_UNIT_ANNUAL_VAR_COST,
     +                    CL_UNIT_ANNUAL_FIXED_COST,
     +                    TOTAL_COST,
     +                   (ANNUAL_CL_UNIT_EMISSIONS(J,I),
     +                                    J=1,NUMBER_OF_EMISSION_TYPES),
     +                    CAP_FACTOR
                     ENDIF ! TVA
                     WRITE(97,'(A)',REC=IREC)NEW_LINE//trim(RECORD)
                  ENDIF
                  IF(BY_YEAR) THEN
                     IF(TVA) THEN
                        WRITE(96,1011) trim(UNIT_NAME),MW(2,I),
     +                    ANNUAL_CL_UNIT_ENERGY(I)/1000.,
     +                    AVERAGE_HEATRATE,
     +                    AVERAGE_COST,
     +                    CL_UNIT_ANNUAL_FUEL_COST,
     +                    CL_UNIT_ANNUAL_VAR_COST,
     +                    CL_UNIT_ANNUAL_FIXED_COST,
     +                    TOTAL_COST,
     +                   (ANNUAL_CL_UNIT_EMISSIONS(J,I),
     +                                    J=1,NUMBER_OF_EMISSION_TYPES),
     +                    CL_AI_INVESTMENT(I)/1000000.,
     +                    CAP_FACTOR
                     ELSE
                        WRITE(96,1011) trim(UNIT_NAME),MW(2,I),
     +                    ANNUAL_CL_UNIT_ENERGY(I)/1000.,
     +                    AVERAGE_HEATRATE,
     +                    AVERAGE_COST,
     +                    CL_UNIT_ANNUAL_FUEL_COST,
     +                    CL_UNIT_ANNUAL_VAR_COST,
     +                    CL_UNIT_ANNUAL_FIXED_COST,
     +                    TOTAL_COST,
     +                   (ANNUAL_CL_UNIT_EMISSIONS(J,I),
     +                                    J=1,NUMBER_OF_EMISSION_TYPES),
     +                    CAP_FACTOR
                     ENDIF ! TVA
                  ENDIF ! BY UNIT
               ENDIF ! SPREADSHEET
            ELSE
               IF(ANNUAL_OUTPUT_REPORT) THEN
                  IF(TVA) THEN
                     ANNUAL_AI_INVESTMENT = ANNUAL_AI_INVESTMENT +
     +                                               CL_AI_INVESTMENT(I)
                  ENDIF
               ENDIF
               IF(SPREAD_SHEET) THEN
                  IF(BY_UNIT) THEN
                     IF(TVA) THEN
                        WRITE(RECORD,1052) YR+get_BASE_YEAR(),
     +                     MW(2,I),'0,0,0,0,0,',
     +                     CL_UNIT_ANNUAL_FIXED_COST,
     +                     CL_UNIT_ANNUAL_FIXED_COST,
     +                     '0,0,0,0,0,',
     +                     CL_AI_INVESTMENT(I)/1000000.,',0,'
                     ELSE
                        WRITE(RECORD,1052) YR+get_BASE_YEAR(),
     +                     MW(2,I),'0,0,0,0,0,',
     +                     CL_UNIT_ANNUAL_FIXED_COST,
     +                     CL_UNIT_ANNUAL_FIXED_COST,
     +                     '0,0,0,0,0,0,'
                     ENDIF
                     WRITE(97,'(A)',REC=IREC)NEW_LINE//trim(RECORD)
                  ENDIF
                  IF(BY_YEAR) THEN
                     IF(TVA) THEN
                        WRITE(96,1051) trim(UNIT_NAME),MW(2,I),
     +                           '0,0,0,0,0,',
     +                     CL_UNIT_ANNUAL_FIXED_COST,
     +                     CL_UNIT_ANNUAL_FIXED_COST,'0,0,0,0,0,',
     +                     CL_AI_INVESTMENT(I)/1000000.,',0,'
                     ELSE
                        WRITE(96,1051) trim(UNIT_NAME),MW(2,I),
     +                           '0,0,0,0,0,',
     +                     CL_UNIT_ANNUAL_FIXED_COST,
     +                     CL_UNIT_ANNUAL_FIXED_COST,'0,0,0,0,0,0,'
                     ENDIF
                 ENDIF
               ENDIF
            ENDIF ! IF ENERGY FOR UNIT = 0.

            IF(ANNUAL_CL_UNIT_ENERGY(I) <= 0.0) THEN
               CL_UNIT_ANNUAL_FUEL_COST = ZERO
               CL_UNIT_ANNUAL_VAR_COST = ZERO
               CAP_FACTOR = ZERO
               AVERAGE_HEATRATE = N_A
               AVERAGE_COST = N_A
            ENDIF

! TODO: EXTRACT_METHOD handle_annual_output_report (Done)
!
      CALL handle_annual_output_report(I,J,TOTAL_COST,
     +     CL_UNIT_ANNUAL_FIXED_COST,CL_UNIT_ANNUAL_FUEL_COST,
     +     CL_UNIT_ANNUAL_VAR_COST,ANNUAL_OUTPUT_REPORT,AVERAGE_COST,
     +     AVERAGE_HEATRATE,CAP_FACTOR,UNIT_NAME,SAVE_YEAR,
     +     ANN_CL_UNIT_REC,ANN_CL_UNIT_NO)


! TODO: EXTRACT_METHOD handle_annual_output_report (end)
         ENDIF
      ENDDO

      DO I = 1 , NUMBER_OF_CONTRACTS
         IREC = (get_nunits()+I-1)*(RECORDS_PER_UNIT+2) +
     +                             RECORDS_PRINTED_PER_UNIT + 2 + OFFSET
         IF(AINT(CNTR_ON_LI(I)/100.)+1900-get_BASE_YEAR() .LE. YR .AND.
     +      AINT(CNTR_OFF_LI(I)/100.)+1900-get_BASE_YEAR() .GE. YR) THEN
            IF(ANNUAL_CONTRACT_ENERGY(I) .NE. 0.0) THEN
               ANNUAL_TOTAL_EMISSIONS(1) = ANNUAL_TOTAL_EMISSIONS(1) +
     +                                           ANNUAL_CONTRACT_SO2(I)

               IF(CNTR_WEIGHTED_CAPACITY(I) > 0 ) THEN
                  CONTRACT_LOAD_FACTOR = 100*ANNUAL_CONTRACT_ENERGY(I)/
     +                                     CNTR_WEIGHTED_CAPACITY(I)
               ELSE
                  CONTRACT_LOAD_FACTOR = 0.
               ENDIF
               IF(SPREAD_SHEET) THEN
                  IF(BY_UNIT) THEN
                     WRITE(RECORD,3018) YR+get_BASE_YEAR(),
     +                  ANNUAL_CONTRACT_CAPACITY(I),
     +                  ANNUAL_CONTRACT_ENERGY(I)/1000.,
     +                  ANNUAL_CONTRACT_VARIABLE_COST(I)*1000000./
     +                       ANNUAL_CONTRACT_ENERGY(I),
     +                  ANNUAL_CONTRACT_VARIABLE_COST(I),
     +                  ANNUAL_CONTRACT_FIXED_COST(I),
     +                  ANNUAL_CONTRACT_VARIABLE_COST(I) +
     +                  ANNUAL_CONTRACT_FIXED_COST(I),
     +                  ANNUAL_CONTRACT_SO2(I)
                     WRITE(97,'(A)',REC=IREC) NEW_LINE//trim(RECORD)
                  ENDIF
                  IF(BY_YEAR) THEN
                     WRITE(96,3017)  trim(CNTRNM(I)),
     +                  ANNUAL_CONTRACT_CAPACITY(I),
     +                  ANNUAL_CONTRACT_ENERGY(I)/1000.,
     +                  ANNUAL_CONTRACT_VARIABLE_COST(I)*1000000./
     +                  ANNUAL_CONTRACT_ENERGY(I),
     +                  ANNUAL_CONTRACT_VARIABLE_COST(I),
     +                  ANNUAL_CONTRACT_FIXED_COST(I),
     +                  ANNUAL_CONTRACT_VARIABLE_COST(I) +
     +                  ANNUAL_CONTRACT_FIXED_COST(I),
     +                  ANNUAL_CONTRACT_SO2(I)
                  ENDIF
               ENDIF
            ELSE
               IF(SPREAD_SHEET) THEN
                  IF(BY_UNIT) THEN
                     WRITE(RECORD,3015) YR+get_BASE_YEAR(),
     +                                  ANNUAL_CONTRACT_CAPACITY(I),
     +                                  ANNUAL_CONTRACT_FIXED_COST(I),
     +                                  ANNUAL_CONTRACT_FIXED_COST(I)
                     WRITE(97,'(A)',REC=IREC) NEW_LINE//trim(RECORD)
                  ENDIF
                  IF(BY_YEAR) THEN
                     WRITE(96,3014) trim(CNTRNM(I)),
     +                              ANNUAL_CONTRACT_CAPACITY(I),
     +                              ANNUAL_CONTRACT_FIXED_COST(I),
     +                              ANNUAL_CONTRACT_FIXED_COST(I)
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDDO
      IF(TMMBTUS(YR) /= 0.) THEN
         TEMP_DBLE = DBLE(2000.)/TMMBTUS(YR)
      ELSE
         TEMP_DBLE = DBLE(1.)
      ENDIF
!
!
! TODO: EXTRACT_METHOD accumulate_production_costs (done)
      TOTAL_PRODUCTION_COSTS = total_production_cost(YR)
! TODO: EXTRACT_METHOD accumulate_production_costs (end)

! TODO: EXTRACT_METHOD prepare_annual_output_report
      IF(ANNUAL_OUTPUT_REPORT) THEN
         WRITE(ANN_CL_SUM_NO,REC=ANN_CL_SUM_REC)
     +              PRT_ENDPOINT(),FLOAT(SAVE_YEAR+get_BASE_YEAR()),
! ENERGY BALANCE (6)
     +                     SNGL(CAP_LIMITED_DEMAND/1000.),
     +                     ANNUAL_ECONOMY_SOLD/1000.,
!
     +          SNGL(CAP_LIMITED_DEMAND + ANNUAL_ECONOMY_SOLD +
     +            ANN_DYN_STORAGE_PUMP_ENRG - ANN_DYN_STORAGE_GEN_ENRG -
     +          ANNUAL_ECONOMY_BOUGHT-TENRG(YR)-PURCHASE_ENERGY)/1000.,
!
     +                     SNGL(TENRG(YR))/1000.,
     +                     SNGL(PURCHASE_ENERGY)/1000.,
     +                     ANNUAL_ECONOMY_BOUGHT/1000.,
! BTUS     (1)
!
     +                  SNGL(TMMBTUS(YR) + CLASS_ASSIGNED_MMBTUS(2)),
! COST VARIABLES (7)
     +           SNGL((FUELCOST(YR)+CLASS_ASS_FOSSIL_COST(2))/1000000.),
     +           (TOTAL_NUCLEAR_FUEL_EXPENSE +
     +                             CLASS_ASS_NUCLEAR_COST(2))/1000000.,
     +           SNGL((VARCOST(YR) +
     +                       CLASS_ASSIGNED_VARIABLE_COST(2))/1000000.),
     +           (FIXED_OM_COSTS +
     +                          CLASS_ASSIGNED_FIXED_COST(2))/1000000.,
     +           (PURCHASE_COSTS +
     +                            CLASS_ASS_PURCHASE_COST(2))/1000000.,
!

     +                     ANNUAL_ECONOMY_COST/1000000.,
     +                     ANNUAL_ECONOMY_REVENUE/1000000.,
     +                     TOTAL_PRODUCTION_COSTS/1000000.,
!
! DEMAND VARIABLES (2)
     +                     PEAK,BASE,
! EMISSIONS VARIABLES (10)
     +            (ANNUAL_TOTAL_EMISSIONS(I),
     +                  SNGL(ANNUAL_TOTAL_EMISSIONS(I)*TEMP_DBLE),
     +                                     I=1,NUMBER_OF_EMISSION_TYPES)
         ANN_CL_SUM_REC = ANN_CL_SUM_REC + 1
!
      ENDIF
! TODO: EXTRACT_METHOD write_pooling_transactions
      IF(POOLING_TRANSACTIONS()) THEN
         WRITE(ANN_POOL_SUM_NO,REC=ANN_POOL_SUM_REC)
     +           PRT_ENDPOINT(),FLOAT(globecom_YEAR+get_BASE_YEAR()),
     +                  'System',
     +                  SNGL(TENRG(YR))/1000.,
     +                  SNGL(TMMBTUS(YR) + CLASS_ASSIGNED_MMBTUS(2)),
     +           SNGL((FUELCOST(YR)+CLASS_ASS_FOSSIL_COST(2))/1000000.),
     +           (TOTAL_NUCLEAR_FUEL_EXPENSE +
     +                             CLASS_ASS_NUCLEAR_COST(2))/1000000.,
     +   SNGL((VARCOST(YR) + CLASS_ASSIGNED_VARIABLE_COST(2))/1000000.),
     +     (FIXED_OM_COSTS+CLASS_ASSIGNED_FIXED_COST(2))/1000000.,
     +     (PURCHASE_COSTS + CLASS_ASS_PURCHASE_COST(2))/1000000.,
     +                           (ANNUAL_TOTAL_EMISSIONS(I),
     +                                     I=1,NUMBER_OF_EMISSION_TYPES)
         ANN_POOL_SUM_REC = ANN_POOL_SUM_REC + 1
         WRITE(ANN_POOL_SUM_NO,REC=ANN_POOL_SUM_REC)
     +            PRT_ENDPOINT(),FLOAT(globecom_YEAR+get_BASE_YEAR()),
     +            'Native',
     +            SNGL(TENRG(YR) - CLASS_ASSIGNED_ENERGY(2))/1000.,
     +            SNGL(TMMBTUS(YR)),
     +            SNGL(FUELCOST(YR)/1000000.),
     +            TOTAL_NUCLEAR_FUEL_EXPENSE/1000000.,
     +            SNGL(VARCOST(YR)/1000000.),
     +            FIXED_OM_COSTS/1000000.,
     +            PURCHASE_COSTS/1000000.,
     +            ((ANNUAL_TOTAL_EMISSIONS(I) -
     +                                 CLASS_ASSIGNED_EMISS(I,2)),
     +                                     I=1,NUMBER_OF_EMISSION_TYPES)
         ANN_POOL_SUM_REC = ANN_POOL_SUM_REC + 1
         WRITE(ANN_POOL_SUM_NO,REC=ANN_POOL_SUM_REC)
     +            PRT_ENDPOINT(),FLOAT(globecom_YEAR+get_BASE_YEAR()),
     +            'Other ',
     +            CLASS_ASSIGNED_ENERGY(2)/1000.,
     +            CLASS_ASSIGNED_MMBTUS(2),
     +            CLASS_ASS_FOSSIL_COST(2)/1000000.,
     +            CLASS_ASS_NUCLEAR_COST(2)/1000000.,
     +            CLASS_ASSIGNED_VARIABLE_COST(2)/1000000.,
     +            CLASS_ASSIGNED_FIXED_COST(2)/1000000.,
     +            CLASS_ASS_PURCHASE_COST(2)/1000000.,
     +            (CLASS_ASSIGNED_EMISS(I,2),
     +                                     I=1,NUMBER_OF_EMISSION_TYPES)
         ANN_POOL_SUM_REC = ANN_POOL_SUM_REC + 1
!
      ENDIF ! ANNUAL OUTPUT REPORT AND  NOT POOLING_TRANSACTIONS
      IF(SPREAD_SHEET .AND. BY_YEAR) THEN
         WRITE(96,*)
         IF(POOLING_TRANSACTIONS()) THEN
            WRITE(96,*) '" ","System","Native Utility",'//
     +                                            '"Other Pool Members"'
            WRITE(96,1021) 'Demand (GWh)',
     +                              SNGL(CAP_LIMITED_DEMAND/1000.)
            WRITE(96,1021) 'Economy Sold (GWh)',
     +                             ANNUAL_ECONOMY_SOLD/1000.
            WRITE(96,1021) 'Generation (GWh)',SNGL(TENRG(YR))/1000.,
     +         SNGL(TENRG(YR) - CLASS_ASSIGNED_ENERGY(2) )/1000.,
     +         CLASS_ASSIGNED_ENERGY(2)/1000.
            WRITE(96,1021) 'Purchases (GWh)',
     +                                     SNGL(PURCHASE_ENERGY)/1000.
            WRITE(96,1021) 'Economy Bought (GWh)',
     +                            ANNUAL_ECONOMY_BOUGHT/1000.
            WRITE(96,1021) 'Unserved Energy (GWh)',
     +          SNGL(CAP_LIMITED_DEMAND + ANNUAL_ECONOMY_SOLD +
     +            CNTR_SALES_ENERGY -
     +            ANNUAL_ECONOMY_BOUGHT-TENRG(YR)-PURCHASE_ENERGY)/1000.
            IF(CANADA) THEN
               WRITE(96,3032) 'GigaJoules',
     +            DBLE(TMMBTUS(YR) + CLASS_ASSIGNED_MMBTUS(2)),
     +                  DBLE(TMMBTUS(YR)),
     +                  DBLE(CLASS_ASSIGNED_MMBTUS(2))
            ELSE
               WRITE(96,1016) 'mmBTUs',
     +            DBLE(TMMBTUS(YR) + CLASS_ASSIGNED_MMBTUS(2)),
     +                  DBLE(TMMBTUS(YR)),
     +                  DBLE(CLASS_ASSIGNED_MMBTUS(2))
            ENDIF
            WRITE(96,1031) 'Fuel Cost (M$)',
     +            (FUELCOST(YR)+CLASS_ASS_FOSSIL_COST(2))/1000000.,
     +            FUELCOST(YR)/1000000.,
     +            CLASS_ASS_FOSSIL_COST(2)/1000000.
            IF(ABS(TOTAL_NUCLEAR_FUEL_EXPENSE/1000000.) > .005)
     +            WRITE(96,1031) 'Nuclear Fuel Cost ($M)',
     +                           (TOTAL_NUCLEAR_FUEL_EXPENSE +
     +                           CLASS_ASS_NUCLEAR_COST(2))/1000000.,
     +                            TOTAL_NUCLEAR_FUEL_EXPENSE/1000000.,
     +                            CLASS_ASS_NUCLEAR_COST(2)/1000000.
            WRITE(96,1031) 'Variable Costs (M$)',
     +         (VARCOST(YR) + CLASS_ASSIGNED_VARIABLE_COST(2))/1000000.,
     +                            VARCOST(YR)/1000000. ,
     +                  CLASS_ASSIGNED_VARIABLE_COST(2)/1000000.
            WRITE(96,1031) 'Fixed Costs (M$)',
     +          (FIXED_OM_COSTS+CLASS_ASSIGNED_FIXED_COST(2))/1000000.,
     +                            FIXED_OM_COSTS/1000000.,
     +               CLASS_ASSIGNED_FIXED_COST(2)/1000000.
            WRITE(96,1031) 'Purchase Power Costs (M$)',
     +           (PURCHASE_COSTS + CLASS_ASS_PURCHASE_COST(2))/1000000.,
     +       (PURCHASE_COSTS + ENERGY_EXCHANGE_ADJUSTMENT)/1000000.,
     +       (CLASS_ASS_PURCHASE_COST(2) - ENERGY_EXCHANGE_ADJUSTMENT)/
     +                              1000000.
            DO J = 1, NUMBER_OF_EMISSION_TYPES
               WRITE(96,1021) trim(EMISSIONS_TITLES(J)),
     +                        DBLE(ANNUAL_TOTAL_EMISSIONS(J)),
     +                        DBLE(ANNUAL_TOTAL_EMISSIONS(J) -
     +                                       CLASS_ASSIGNED_EMISS(J,2)),
     +                        DBLE(CLASS_ASSIGNED_EMISS(J,2))
               WRITE(96,1021)trim(EMISSIONS_CAP_TITLES(J)),EMIS_CAP(J)
            ENDDO
         ELSE
            WRITE(96,1021) 'Demand (GWh)',
     +                              SNGL(CAP_LIMITED_DEMAND/1000.)
            IF(REALLY_KEPCO .OR. WABASH_VALLEY)
     +       WRITE(96,1021) 'Energy Losses (GWh)',
     +       SNGL(CAP_LIMITED_DEMAND-TENRG(YR)-PURCHASE_ENERGY)/(-1000.)
            WRITE(96,1021) 'Generation (GWh)',
     +                           SNGL(TENRG(YR))/1000.
            WRITE(96,1021) 'Purchases (GWh)',
     +                                     SNGL(PURCHASE_ENERGY)/1000.
            IF(KEPCO) THEN
               WRITE(96,1021) 'Unserved Energy (GWh)',0.
            ELSE
               WRITE(96,1021) 'Unserved Energy (GWh)',
     +          SNGL(CAP_LIMITED_DEMAND + CNTR_SALES_ENERGY -
     +                                  TENRG(YR)-PURCHASE_ENERGY)/1000.
            ENDIF
            IF(CANADA) THEN
               WRITE(96,3032) 'GigaJoules',TMMBTUS(YR)
            ELSE
               WRITE(96,1016) 'mmBTUs',TMMBTUS(YR)
            ENDIF
            WRITE(96,1031) 'Fuel Cost (M$)', FUELCOST(YR)/1000000.
            IF(ABS(TOTAL_NUCLEAR_FUEL_EXPENSE/1000000.) >  .005)
     +            WRITE(96,1031) 'Nuclear Fuel Cost (M$)    ',
     +                          TOTAL_NUCLEAR_FUEL_EXPENSE/1000000.
            WRITE(96,1031) 'Variable Costs (M$)', VARCOST(YR)/1000000.
            WRITE(96,1031) 'Fixed Costs (M$)', FIXED_OM_COSTS/1000000.
            WRITE(96,1031) 'Purchase Power Costs (M$)',
     +                                      PURCHASE_COSTS/1000000.
            DO J = 1, NUMBER_OF_EMISSION_TYPES
               WRITE(96,1031) trim(EMISSIONS_TITLES(J)),
     +                                         ANNUAL_TOTAL_EMISSIONS(J)
               WRITE(96,1031)trim(EMISSIONS_CAP_TITLES(J)),EMIS_CAP(J)
            ENDDO
         ENDIF
         IF(TVA) WRITE(96,1031)
     +         'A&I Investment (M$)', ANNUAL_AI_INVESTMENT/1000000.
         WRITE(96,1033) 'Peak Load (MW)',NINT(PEAK)
         WRITE(96,1033) 'Minimum Load (MW)',NINT(BASE)
         WRITE(96,*)
      ENDIF
      IF(YR == LAST_YEAR) OFFSET = NEXT_OFFSET
      RETURN
 1010 FORMAT('&',F8.1,I7,F7.2,F7.2,F6.2,F7.2,F8.2,2F9.1)
 1012 FORMAT('&',F8.1,F7.2,F7.2,F7.2,F6.2,F7.2,F8.2,2F9.1)
!
 1017 FORMAT(1X,I3,1X,A20,F5.1,F5.1,F8.1,4X,A,F7.2,4X,A,F6.2,
     +                                                 F7.2,F8.2,F9.1)
 1014 FORMAT(1X,I3,1X,A20,F5.1,I5,35X,F7.2,F8.2)
!
 1020 FORMAT(1X,A,F13.1)
!
 1030 FORMAT('&',2X,A,F13.2)
 1032 FORMAT('&',2X,A,I13)
 1035 FORMAT('&',4X,A,3I12)
 1050 FORMAT(1X,I3,1X,A20,F5.1,I5,F8.1,11X,16X,F7.2,F8.2,A)
 1250 FORMAT(1X,I3,1X,A20,I5,I5,F8.1,11X,16X,F7.2,F8.2,A)
 2015 FORMAT (3(1X,'mmBTUs                    ',I13:,5X))
 2016 FORMAT (3(1X,'GigaJoules                ',I13:,5X))
 2020 FORMAT (3(1X,'Generation (GWh)          ',F13.1,5X))
 2030 FORMAT (3(1X,'Fuel Cost (M$)            ',F13.2,5X))
 2031 FORMAT (3(1X,'Nuclear Fuel Cost (M$)    ',F13.2,5X))
 2032 FORMAT (3(1X,'Variable Costs (M$)       ',F13.2,5X))
 2033 FORMAT (3(1X,'Fixed Costs (M$)          ',F13.2,5X))
 2034 FORMAT (3(1X,'Purchase Power Costs (M$) ',F13.2,5X))
 2035 FORMAT (3(1X,'SO2 Emissions (tons)      ',F13.1,5X))
 2036 FORMAT (3(1X,'NOx Emissions (tons)      ',F13.1,5X))
 2037 FORMAT (3(1X,'CO2 Emissions (tons)      ',F13.1,5X))
 2038 FORMAT (3(1X,'Other 1 Emissions (tons)  ',F13.1,5X))
 2039 FORMAT (3(1X,'Other 2 Emissions (tons)  ',F13.1,5X))
!
! FORMATS FOR SPREADSHEET
!
 1001 FORMAT(1X,A)
 1002 FORMAT(1X,'"',I4,A,I4,10X,A,'"')
 1011 FORMAT(1X,'"',A,'",',F6.1,',',F10.1,',',F12.3,',',F7.2,4(',',F8.2)
     +                  ,5(',',F11.1),3(',',F7.2),',')
 1013 FORMAT(1X,I4,',',F6.1,',',F10.1,',',F12.3,',',F7.2,4(',',F8.2),
     +                   5(',',F11.1),3(',',F7.2),',')
 1016 FORMAT(1X,'"',A,'",',3(I13,','))
 1021 FORMAT(1X,'"',A,'",',3(F13.1,','))
 1031 FORMAT(1X,'"',A,'",',3(F13.2,','))
 3032 FORMAT(1X,'"',A,'",',3(F13.3,','))
 1033 FORMAT(1X,'"',A,'",',I6,',')
 1051 FORMAT(1X,'"',A,'",',F6.1,',',A,2(F8.2,','),A,F8.2,A)
 1052 FORMAT(1X,I4,',',F6.1,',',A,2(F8.2,','),A,F8.2,A)
 3014 FORMAT(1X,'"',A,'",',F5.1,',,,,',F7.2,',',F8.2,',')
 3015 FORMAT(1X,I4,',',F5.1,',,,,',F7.2,',',F8.2,',')
 3017 FORMAT(1X,'"',A,'",',F5.1,',',F8.1,',,',F7.2,',,',
     +       2(F7.2,','),F8.2,',',F9.1,',')
 3018 FORMAT(1X,I4,',',F5.1,',',F8.1,',',F7.2,',',
     +       2(F7.2,','),F8.2,',',F9.1,',')
      END

!
!                       POWER POOLING ROUTINE FOR IE
!                           COPYRIGHT (C) 1988
!                       M.S. GERBER & ASSOCIATES, INC.
!                           ALL RIGHTS RESERVED
!

!
      SUBROUTINE POOLING(YR,PSMO,PEMO,
     +                   ENERGY_EXCHANGE_ADJUSTMENT,
     +                   DEMAND_AFTER_DSM_BEFORE_EL,
     +                   POOL_PRICING_SWITCH,
     +                   POOL_NATIVE_MULT,
     +                   POOL_OTHER_MULT)

      use lamcom
      use SpinDriftLib
      use prod_arrays_dimensions
      use grx_planning_routines
      use cls_load
      USE SIZECOM
      use poolcom
      use forecast

      INTEGER (kind=2) ::  YR,PSMO,PEMO,I
      REAL (kind=8) ::  DEMAND_AFTER_DSM_BEFORE_EL
      REAL ::  ENERGY_EXCHANGE_ADJUSTMENT
      REAL ::  CIPCO_ENERGY
      REAL ::  IE_ENERGY
      REAL ::  TRANSFER_ENERGY
      REAL ::  TRANSFER_PRICE
      REAL ::  LOAD_MODIFICATION_ENERGY
      REAL ::  POOL_NATIVE_MULT
      REAL ::  POOL_OTHER_MULT
      REAL ::  TRANSACTION_ERROR
      REAL ::  NATIVE_MULT
      REAL ::  OTHER_MULT

      REAL ::  POOL_LOAD_MANAGEMENT
      REAL ::  NATIVE_LOAD_MANAGEMENT
      REAL ::  TOTAL_NATIVE_LOAD_MANAGEMENT
      LOGICAL (kind=1) ::  UNIT_OUTPUT_REPORT
      CHARACTER (len=1) ::  POOL_PRICING_SWITCH
!
!     ALTERED FOR IP, 6/5/91.

      if(yr==0) then
        call end_program("clreport:0010 - YR argument is zero.")
      end if
      
      IE_ENERGY = 0.
      CIPCO_ENERGY = 0.
      LOAD_MODIFICATION_ENERGY = 0.
      NATIVE_MULT = 1. + POOL_NATIVE_MULT/100.
      OTHER_MULT = 1. + POOL_OTHER_MULT/100.
      TOTAL_NATIVE_LOAD_MANAGEMENT = 0.
      DO I = PSMO, PEMO
!
         CALL GET_CLASS_DSM_ENERGY(I,6,POOL_LOAD_MANAGEMENT)
         CALL GET_CLASS_DSM_ENERGY(I,1,NATIVE_LOAD_MANAGEMENT)
         TOTAL_NATIVE_LOAD_MANAGEMENT = TOTAL_NATIVE_LOAD_MANAGEMENT +
     +                                            NATIVE_LOAD_MANAGEMENT
         CALL GET_CLASS_DSM_ENERGY(I,2,NATIVE_LOAD_MANAGEMENT)
         TOTAL_NATIVE_LOAD_MANAGEMENT = TOTAL_NATIVE_LOAD_MANAGEMENT +
     +                                            NATIVE_LOAD_MANAGEMENT
         CALL GET_CLASS_DSM_ENERGY(I,3,NATIVE_LOAD_MANAGEMENT)
         TOTAL_NATIVE_LOAD_MANAGEMENT = TOTAL_NATIVE_LOAD_MANAGEMENT +
     +                                            NATIVE_LOAD_MANAGEMENT
         CALL GET_CLASS_DSM_ENERGY(I,4,NATIVE_LOAD_MANAGEMENT)
         TOTAL_NATIVE_LOAD_MANAGEMENT = TOTAL_NATIVE_LOAD_MANAGEMENT +
     +                                            NATIVE_LOAD_MANAGEMENT
         CALL GET_CLASS_DSM_ENERGY(I,5,NATIVE_LOAD_MANAGEMENT)
         TOTAL_NATIVE_LOAD_MANAGEMENT = TOTAL_NATIVE_LOAD_MANAGEMENT +
     +                                            NATIVE_LOAD_MANAGEMENT
         CALL GET_CLASS_DSM_ENERGY(I,7,NATIVE_LOAD_MANAGEMENT)
         TOTAL_NATIVE_LOAD_MANAGEMENT = TOTAL_NATIVE_LOAD_MANAGEMENT +
     +                                            NATIVE_LOAD_MANAGEMENT
!
         IE_ENERGY = IE_ENERGY +
     +      (FORECAST_ENERGY(1,I,1) + FORECAST_ENERGY(2,I,1)) /
     +         (1 - CLASS_LOSSES(1)) +
     +      (FORECAST_ENERGY(1,I,2) + FORECAST_ENERGY(2,I,2)) /
     +         (1 - CLASS_LOSSES(2)) +
     +      (FORECAST_ENERGY(1,I,3) + FORECAST_ENERGY(2,I,3)) /
     +         (1 - CLASS_LOSSES(3)) +
     +      (FORECAST_ENERGY(1,I,4) + FORECAST_ENERGY(2,I,4)) /
     +         (1 - CLASS_LOSSES(4)) +
     +      (FORECAST_ENERGY(1,I,5) + FORECAST_ENERGY(2,I,5)) /
     +         (1 - CLASS_LOSSES(5))
         CIPCO_ENERGY = CIPCO_ENERGY +
     +      (FORECAST_ENERGY(1,I,6) + FORECAST_ENERGY(2,I,6))/
     +                                    (1 - CLASS_LOSSES(6))
         LOAD_MODIFICATION_ENERGY =  LOAD_MODIFICATION_ENERGY +
     +                                  SEASON_SYSTEM_ENERGY(I)
      ENDDO
      LOAD_MODIFICATION_ENERGY = LOAD_MODIFICATION_ENERGY -
     +                                  SNGL(DEMAND_AFTER_DSM_BEFORE_EL)
      TRANSFER_ENERGY = IE_ENERGY - P_CLASS_ASSIGNED_ENERGY(1) -
     +   LOAD_MODIFICATION_ENERGY -  P_CLASS_ASS_ECON_BUY(1) +
     +                   P_CLASS_ASS_ECON_SELL(1) + POOL_LOAD_MANAGEMENT
!
!     5/15/92 ENHANCED PRICING STRUCTURE TO ACCOMODATE IE, IP, CIPS
!
      IF(POOL_PRICING_SWITCH == 'P') THEN
         TRANSFER_PRICE =  (
     +      NATIVE_MULT * (P_CLASS_ASSIGNED_COST(1) +
     +      P_CLASS_ASS_ECON_COST(1) - P_CLASS_ASS_ECON_REV(1)) +
     +      OTHER_MULT * (P_CLASS_ASSIGNED_COST(2) +
     +      P_CLASS_ASS_ECON_COST(2) - P_CLASS_ASS_ECON_REV(2)) ) /
     +      (P_CLASS_ASSIGNED_ENERGY(1) + P_CLASS_ASSIGNED_ENERGY(2) -
     +      P_CLASS_ASS_ECON_SELL(1) + P_CLASS_ASS_ECON_BUY(1) -
     +      P_CLASS_ASS_ECON_SELL(2) + P_CLASS_ASS_ECON_BUY(2) )
      ELSEIF(POOL_PRICING_SWITCH == 'U') THEN
         IF(TRANSFER_ENERGY < 0.) THEN
            TRANSFER_PRICE =
     +         (NATIVE_MULT * (P_CLASS_ASSIGNED_COST(1) +
     +         P_CLASS_ASS_ECON_COST(1) - P_CLASS_ASS_ECON_REV(1)) )/
     +         (P_CLASS_ASSIGNED_ENERGY(1) -
     +         P_CLASS_ASS_ECON_SELL(1) + P_CLASS_ASS_ECON_BUY(1) )
         ELSEIF(P_CLASS_ASSIGNED_ENERGY(2) .NE. 0.) THEN
            TRANSFER_PRICE =
     +         (OTHER_MULT * (P_CLASS_ASSIGNED_COST(2) +
     +         P_CLASS_ASS_ECON_COST(2) - P_CLASS_ASS_ECON_REV(2)) )/
     +         (P_CLASS_ASSIGNED_ENERGY(2) -
     +         P_CLASS_ASS_ECON_SELL(2) + P_CLASS_ASS_ECON_BUY(2) )
         ELSE
            TRANSFER_PRICE =  0.
         ENDIF
      ENDIF
      ENERGY_EXCHANGE_ADJUSTMENT = ENERGY_EXCHANGE_ADJUSTMENT +
     +                             TRANSFER_PRICE * TRANSFER_ENERGY
! ADDED 10/5/92 TO TRAP FOR TRANSACTION DIFFERENCES
         TRANSACTION_ERROR = ABS(P_CLASS_ASSIGNED_ENERGY(1) -
     +      P_CLASS_ASS_ECON_SELL(1) + P_CLASS_ASS_ECON_BUY(1) -
     +      IE_ENERGY + LOAD_MODIFICATION_ENERGY +
     +      (P_CLASS_ASSIGNED_ENERGY(2) - P_CLASS_ASS_ECON_SELL(2) +
     +      P_CLASS_ASS_ECON_BUY(2) - CIPCO_ENERGY))
         IF(TRANSACTION_ERROR .GT. 49) WRITE(4,*)
     +      'Inconsistent buys and sells in the pooling calc_n in year',
     +      YR,' season',PSMO,
     +      TRANSACTION_ERROR/1000.,'= DIFFERENCE'
!
      IF(UNIT_OUTPUT_REPORT()) THEN
         WRITE(9,"('0',A)") '---------------------- Pool Transa'//
     +                      'ctions Summary ----------------------'
         WRITE(9,1010) 'Company Energy (GWh)    ',(IE_ENERGY -
     +               LOAD_MODIFICATION_ENERGY +
     +                                       POOL_LOAD_MANAGEMENT)/1000.
         WRITE(9,1020) 'Pool Energy Needs (GWh)  ',(CIPCO_ENERGY -
     +                                       POOL_LOAD_MANAGEMENT)/1000.
!
         WRITE(9,1010) 'Company Generation (GWh)',
     +                         P_CLASS_ASSIGNED_ENERGY(1)/1000.
         WRITE(9,1020) 'Pool Generation (GWh)    ',
     +                           P_CLASS_ASSIGNED_ENERGY(2)/1000.
!
         WRITE(9,1010) 'Energy to/frm Pool (GWh)',
     +         (P_CLASS_ASSIGNED_ENERGY(1) - IE_ENERGY +
     +         P_CLASS_ASS_ECON_BUY(1) - P_CLASS_ASS_ECON_SELL(1)  +
     +                               LOAD_MODIFICATION_ENERGY -
     +                                       POOL_LOAD_MANAGEMENT)/1000.
         WRITE(9,1020) 'Energy to/frm Compy (GWh)',
     +        (P_CLASS_ASSIGNED_ENERGY(2) - CIPCO_ENERGY +
     +        P_CLASS_ASS_ECON_BUY(2) - P_CLASS_ASS_ECON_SELL(2) +
     +                                       POOL_LOAD_MANAGEMENT)/1000.
         IF(P_CLASS_ASSIGNED_ENERGY(1) > 0.) THEN
            WRITE(9,1030) 'Company Price ($/MWh)   ',
     +         (P_CLASS_ASSIGNED_COST(1) +
     +         P_CLASS_ASS_ECON_COST(1) - P_CLASS_ASS_ECON_REV(1))/
     +         (P_CLASS_ASSIGNED_ENERGY(1) -
     +         P_CLASS_ASS_ECON_SELL(1) + P_CLASS_ASS_ECON_BUY(1) )
         ELSE
            WRITE(9,1030) 'Company Price ($/MWh)            '
         ENDIF
         IF(P_CLASS_ASSIGNED_ENERGY(2) > 0.) THEN
            WRITE(9,1040) 'Pool Price ($/MWh)       ',
     +         (P_CLASS_ASSIGNED_COST(2) +
     +         P_CLASS_ASS_ECON_COST(2) - P_CLASS_ASS_ECON_REV(2))/
     +         (P_CLASS_ASSIGNED_ENERGY(2) -
     +         P_CLASS_ASS_ECON_SELL(2) + P_CLASS_ASS_ECON_BUY(2) )
         ELSE
            WRITE(9,1040) 'Pool Price ($/MWh)       '
         ENDIF
!
         WRITE(9,1030) 'Compy Transfer Cost ($M)',
     +                     TRANSFER_PRICE * TRANSFER_ENERGY/1000000.
      ENDIF
      RETURN
 1010 FORMAT(1X,A,F9.1)
 1020 FORMAT('&',2X,A,F11.1)
 1030 FORMAT(1X,A,F9.2)
 1040 FORMAT('&',2X,A,F11.2)
      END

      SUBROUTINE LPPLOT (LODVAL,PBIL,NOPT,ISEAS,year, BEFORE)

      USE IREC_ENDPOINT_CONTROL
      use grx_planning_routines
      use mod_base_year
      implicit none
      LOGICAL (kind=1) ::  LDC_REPORT_NOT_OPEN=.TRUE. ,BEFORE
      INTEGER (kind=2) ::  NOPT,YEAR
      INTEGER (kind=2) ::  I
      INTEGER (kind=2) ::  ISEAS
      INTEGER (kind=2) ::  LDC_REPORT_NO
      INTEGER (kind=2) ::  LDC_REPORT_HEADER
      INTEGER (kind=2) ::  SEASON
      REAL ::  LODVAL(1000),PBIL(1000)
      CHARACTER (len=9) ::  WRITE_MONTH_NAME(12)
      CHARACTER (len=9) ::  MONTH_NAME*20
      CHARACTER (len=9) ::  BEFORE_AFTER*6
      INTEGER ::  LDC_REPORT_REC


      SAVE LDC_REPORT_NO,WRITE_MONTH_NAME,LDC_REPORT_REC
      IF(LDC_REPORT_NOT_OPEN) THEN
         LDC_REPORT_NO = LDC_REPORT_HEADER(LDC_REPORT_REC)
         LDC_REPORT_NOT_OPEN = .FALSE.
         DO SEASON = 1 , 12
            WRITE_MONTH_NAME(SEASON) = MONTH_NAME(SEASON)
         ENDDO
      ENDIF
!
      IF(BEFORE) THEN
         BEFORE_AFTER = 'BEFORE'
      ELSE
         BEFORE_AFTER = 'AFTER '
      ENDIF
      DO I = 1 , NOPT
         WRITE(LDC_REPORT_NO,REC=LDC_REPORT_REC)
     +                        PRT_ENDPOINT(),FLOAT(YEAR),
     +                        WRITE_MONTH_NAME(ISEAS),BEFORE_AFTER,
     +                        FLOAT(I),LODVAL(I),100.*PBIL(I)
         LDC_REPORT_REC = LDC_REPORT_REC + 1
      ENDDO
!
      RETURN
      END subroutine LPPLOt

      SUBROUTINE REPORT_FUEL_INVENTORIES
*
      USE IREC_ENDPOINT_CONTROL
      use grx_planning_routines
      USE SIZECOM
      use globecom, only: globecom_year
      use endpoint
      use mod_base_year
      implicit none
      CHARACTER (len=20) ::  UNITNM,RETURN_UNITNM,FUEL_NAME,UNIT_NAME
      CHARACTER (len=9) ::  FUEL_MONTH_NAME,MONTH_NAME*20
      INTEGER (kind=2) ::  RETURN_SHADOW_UNIT_NUMBER
      REAL ::  PRIM_HEAT,SEC_HEAT,EMIS_HEAT,CUM_GAS
      REAL (kind=8) ::  MMBTU_FUEL_BALANCE,TEMP_HEAT
      INTEGER (kind=2) ::  SHADOW_UNIT_NUMBER
      INTEGER (kind=2) ::  I
      INTEGER (kind=2) ::  FUEL_INVEN_NO
      INTEGER (kind=2) ::  FUEL_INVENTORY_HEADER
      INTEGER (kind=2) ::  FUEL_USED_BY_UNIT
      INTEGER (kind=2) ::  R_ISEAS
      INTEGER (kind=2) ::  UNIT_BLOCK
      INTEGER (kind=2) ::  SEGMENT_NO
      LOGICAL (kind=1) ::  FUEL_INVENTORY_NOT_OPEN=.TRUE.
      INTEGER ::  FUEL_INVEN_REC


      SAVE FUEL_INVEN_REC
!
!
      SAVE CUM_GAS,FUEL_INVEN_NO,FUEL_MONTH_NAME
!
      ENTRY INITIALIZE_INVENTORY_REPORT
         CUM_GAS = 0.0

      RETURN
      ENTRY FUEL_INVENTORY_BY_UNIT_REPORT(I,FUEL_USED_BY_UNIT,
     +                           PRIM_HEAT,SEC_HEAT,EMIS_HEAT,
     +                           MMBTU_FUEL_BALANCE)
         IF(FUEL_INVENTORY_NOT_OPEN) THEN
            FUEL_INVEN_NO = FUEL_INVENTORY_HEADER(FUEL_INVEN_REC)
            FUEL_INVENTORY_NOT_OPEN = .FALSE.
         ENDIF
         UNITNM = RETURN_UNITNM(I)
         FUEL_NAME = 'GREGS COAL'

            CUM_GAS = CUM_GAS + PRIM_HEAT

      RETURN
      ENTRY SEASON_FUEL_INVENTORY_REPORT(UNIT_NAME,
     +                        FUEL_USED_BY_UNIT,
     +                        TEMP_HEAT,MMBTU_FUEL_BALANCE,SEGMENT_NO)
         IF(SEGMENT_NO < 2) THEN
            UNIT_NAME = UNIT_NAME(1:18)//'01'
         ELSEIF(SEGMENT_NO == 2) THEN
            UNIT_NAME = UNIT_NAME(1:18)//'02'
         ENDIF
         IF(FUEL_INVENTORY_NOT_OPEN) THEN
            FUEL_INVEN_NO = FUEL_INVENTORY_HEADER(FUEL_INVEN_REC)
            FUEL_INVENTORY_NOT_OPEN = .FALSE.
         ENDIF
         WRITE(FUEL_INVEN_NO,REC=FUEL_INVEN_REC) PRT_ENDPOINT(),
     +         FLOAT(get_BASE_YEAR()+globecom_YEAR),
     +         FUEL_MONTH_NAME,FLOAT(FUEL_USED_BY_UNIT),
     +         UNIT_NAME,
     +         SNGL(TEMP_HEAT-MMBTU_FUEL_BALANCE),
     +         SNGL(MMBTU_FUEL_BALANCE)
         FUEL_INVEN_REC = FUEL_INVEN_REC + 1
      RETURN
      ENTRY UPDATE_SEASON_FOR_FUEL_RPT(R_ISEAS)
         FUEL_MONTH_NAME = MONTH_NAME(R_ISEAS)
      RETURN
 1234 FORMAT(1X,A,7F10.0)
      END
!++++++++
      SUBROUTINE MARKETSYM_REPORT(NUNITS)
      use cl_data
      USE SPCapExVariables
      USE TRANS_GROUP_VARIABLES
      use globecom, only: globecom_year
      use mod_base_year
      implicit none

      LOGICAL (KIND=1) :: YES_REFERENCE_CASE_REPORT
      TYPE MARKETSYM_TRANSFER_STRUCTURE
            CHARACTER (LEN=31) :: UnitName
            CHARACTER (LEN=20) :: TransZoneName
            INTEGER (KIND=2) :: UnitsBuilt
      END TYPE MARKETSYM_TRANSFER_STRUCTURE
      TYPE (MARKETSYM_TRANSFER_STRUCTURE) :: MarketSymAdditions(1000)
      INTEGER (KIND=2) ::  UnitsAdded
      INTEGER (KIND=2) ::  LastNunits
      INTEGER (KIND=2) ::  I
      INTEGER (KIND=2) ::  J
      INTEGER (KIND=2) ::  K
      INTEGER (KIND=2) ::  TG
      INTEGER (KIND=2) ::  TG_POSITION
      INTEGER (KIND=2) ::  MSUnitsAdded
      INTEGER (KIND=2) ::  NUNITS
      INTEGER (KIND=2) ::  GET_PROCOST_LAST_NUNITS
      INTEGER (KIND=2) ::  RETURN_CL_UNITS_B4_ADDITIONS
      INTEGER (KIND=2) ::  get_TRANSACTION_GROUP
      INTEGER (KIND=2) ::  GET_TRANS_GROUP_POSITION
      LOGICAL (KIND=1), SAVE :: MarketSymFileOpen=.FALSE.
      CHARACTER (LEN=256) :: FILE_NAME
      CHARACTER (LEN=5) :: GET_SCENAME
      CHARACTER (LEN=35) :: GET_GROUP_NAME
!

        IF(YES_REFERENCE_CASE_REPORT()) THEN
            IF(.NOT. MarketSymFileOpen) THEN
               FILE_NAME = "MARKETSYM-"//TRIM(clData%Scename)//".CSV"
               OPEN(7338,FILE=FILE_NAME,STATUS="REPLACE")
               MarketSymFileOpen = .TRUE.
            ENDIF
            LastNunits = GET_PROCOST_LAST_NUNITS()
            IF(globecom_YEAR == 1) THEN
               UnitsAdded = max(0,NUNITS-
     + RETURN_CL_UNITS_B4_ADDITIONS())
            ELSE
               UnitsAdded = max(0,NUNITS-LastNunits)
            ENDIF
! INITIALIZE THE MARKETSYM TRANSFER VARIABLE
            IF(UnitsAdded > 0 ) THEN
               MSUnitsAdded = 0
               MarketSymAdditions(:)%TransZoneName = " "
               MarketSymAdditions(:)%UnitName = " "
               MarketSymAdditions(:)%UnitsBuilt = 0
!
!
! Section for new MarketSym transfer
!
               DO K = 1,UnitsAdded
                  I = NUNITS-UnitsAdded+K
                  TG = MAX(1,get_TRANSACTION_GROUP(I))
                  TG_POSITION = MAX(1,GET_TRANS_GROUP_POSITION(TG))
                  DO J = 1, MSUnitsAdded
                     IF(INDEX(MarketSymAdditions(J)%UnitName,
     +                                MARKETSYM_UNIT_NAME(I)) /= 0) THEN
! MATCH UNIT NAME AND ZONE LOCATION
                        IF(INDEX(MarketSymAdditions(J)%TransZoneName,
     +                          GET_GROUP_NAME(TG_POSITION)) == 0) CYCLE
                           MarketSymAdditions(J)%UnitsBuilt = 1
     +                              + MarketSymAdditions(J)%UnitsBuilt
                           EXIT
                     ENDIF
                  ENDDO
                  IF(J > MSUnitsAdded) THEN
                     MSUnitsAdded = MSUnitsAdded + 1
                     MarketSymAdditions(MSUnitsAdded)%TransZoneName =
     +                                      GET_GROUP_NAME(TG_POSITION)
                     MarketSymAdditions(MSUnitsAdded)%UnitName =
     +                                            MARKETSYM_UNIT_NAME(I)
                     MarketSymAdditions(MSUnitsAdded)%UnitsBuilt = 1
                  ENDIF
               ENDDO ! K
! End MarketSym transfer
               DO I = 1, MSUnitsAdded
                  WRITE(7338,"(1X,A,',',I4,',',A,',',I3,',',A)")
     +                        TRIM(MarketSymAdditions(I)%TransZoneName),
     +                        get_BASE_YEAR()+globecom_YEAR,
     +                        TRIM(MarketSymAdditions(I)%UnitName),
     +                        MarketSymAdditions(I)%UnitsBuilt,
     +               '"'//TRIM(TRANS_GROUP_FULL_NAME(TG_POSITION))//'",'
               ENDDO !
               CALL FLUSH(7338)
            ENDIF
         ENDIF
      END SUBROUTINE


