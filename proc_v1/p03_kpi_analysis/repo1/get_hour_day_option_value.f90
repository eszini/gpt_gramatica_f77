! Moved this routine from transobj2 to avoid confusion when working in two routines at the
! same time in that very large file.

      FUNCTION GET_HOUR_DAY_OPTION_VALUE( &
                                    R_TYPE_OF_DERIV, &
                                    R_24_HOUR_MARKET_PRICE, &
                                    R_STRIKE_QUANTITY, &
                                    R_HOUR_ACTIVE, &
                                    R_CALL_OR_PUT, &
                                    R_24_HOUR_STRIKE_PRICE, &
                                    R_DAILY_STRIKES, &
                                    R_DAILY_ENERGY, &
                                    R_DAILY_VARIABLE_COST, &
                                    R_DAILY_ENERGY_REVENUE, &
                                    R_HOUR_USAGE)
      use energy_trace
      use debugtrace
      use program_state
      implicit none
      integer  :: file_trace_ghov=0
      integer (kind=4) :: ghov_count=0
      character (len=2) :: crlf

      INTEGER(kind=2) :: I,R_DAILY_STRIKES,R_TYPE_OF_DERIV, &
                  Physical=1,HourlyOption=2
      REAL(kind=4) :: GET_HOUR_DAY_OPTION_VALUE, &
             TEMP_VALUE, &
             TEMP_QUANT, &
             R_24_HOUR_MARKET_PRICE(24), &
             R_HOUR_ACTIVE(24), &
             R_24_HOUR_STRIKE_PRICE(24), &
             R_CALL_OR_PUT, &
             R_STRIKE_QUANTITY(0:24), & !  AVAILABLE MW RESET TO USED MW
             R_DAILY_ENERGY, &
             R_DAILY_VARIABLE_COST,TEMP_COST, &
             R_DAILY_ENERGY_REVENUE,TEMP_REVENUE, &
             R_HOUR_USAGE(24)

       
!
! END DATA DEFINITIONS
!
         if (file_trace_ghov==0) then   
           file_trace_ghov=open_trace("get_hour_day_option_value.trace", &
            rq_ghov)
         end if
         ghov_count=ghov_count+1
         crlf=char(13)//char(10)
         
         call write_trace_int4(file_trace_ghov, "call ", ghov_count)
         
         GET_HOUR_DAY_OPTION_VALUE = 0.
         R_DAILY_STRIKES = 0
         R_DAILY_ENERGY = 0.0
         R_HOUR_USAGE = 0.0
         R_DAILY_VARIABLE_COST = 0.0
         R_DAILY_ENERGY_REVENUE = 0.0
         TEMP_VALUE = 0.0
         if (ghov_count==1096) then
            if(file_trace_ghov/=BAD_TRACE_HANDLE) then
                write(file_trace_ghov,*) "Arguments: ", &
          "R_TYPE_OF_DERIV ", R_TYPE_OF_DERIV, crlf, &
          "R_24_HOUR_MARKET_PRICE ", R_24_HOUR_MARKET_PRICE, crlf, &
          "R_STRIKE_QUANTITY ", R_STRIKE_QUANTITY, crlf, &
          "R_HOUR_ACTIVE ", R_HOUR_ACTIVE, crlf, &
          "R_CALL_OR_PUT ", R_CALL_OR_PUT, crlf, &
          "R_24_HOUR_STRIKE_PRICE ", R_24_HOUR_STRIKE_PRICE, crlf, &
          "R_DAILY_STRIKES ", R_DAILY_STRIKES, crlf, &
          "R_DAILY_ENERGY ", R_DAILY_ENERGY, crlf, &
          "R_DAILY_VARIABLE_COST ", R_DAILY_VARIABLE_COST, crlf, &
          "R_DAILY_ENERGY_REVENUE ", R_DAILY_ENERGY_REVENUE, crlf, &
          "R_HOUR_USAGE ", R_HOUR_USAGE, crlf
            end if
            
         end if


         DO I = 1, 24

          
          call write_trace_int2(file_trace_ghov, "I", I)

          call write_trace_real4(file_trace_ghov, "Strike qty.", &
            R_STRIKE_QUANTITY(I))
          call write_trace_real4(file_trace_ghov, "hour active", &
            R_HOUR_ACTIVE(I))

            
            TEMP_QUANT = &
                  R_STRIKE_QUANTITY(I) * R_CALL_OR_PUT * &
                                                        R_HOUR_ACTIVE(I)

        call write_trace_real4(file_trace_ghov, "QUANT", TEMP_QUANT)
            TEMP_REVENUE = TEMP_QUANT * R_24_HOUR_MARKET_PRICE(I)
            ! Temp_revenue=0/0
            TEMP_COST = TEMP_QUANT * R_24_HOUR_STRIKE_PRICE(I)
            call write_trace_int2(file_trace_ghov, "1. DERIV", &
                R_TYPE_OF_DERIV)
      call write_trace_real4(file_trace_ghov, "TEMP rev.", TEMP_REVENUE)
      call write_trace_real4(file_trace_ghov, "Temp $", temp_cost)
      
      ! r_type_of_deriv=1/1
            IF(R_TYPE_OF_DERIV == Physical) THEN
               GET_HOUR_DAY_OPTION_VALUE = &
           GET_HOUR_DAY_OPTION_VALUE + TEMP_REVENUE - TEMP_COST
          call write_trace_real4(file_trace_ghov, "1. option value", &
                GET_HOUR_DAY_OPTION_VALUE)
               
               R_DAILY_ENERGY_REVENUE = R_DAILY_ENERGY_REVENUE + &
                                                            TEMP_REVENUE
               R_DAILY_VARIABLE_COST = R_DAILY_VARIABLE_COST + &
                                                            TEMP_COST
               R_DAILY_ENERGY = R_DAILY_ENERGY + TEMP_QUANT
       call write_daily_energy_trace("transobj2:A003", R_DAILY_ENERGY)

               R_HOUR_USAGE(I) = TEMP_QUANT
               R_DAILY_STRIKES = R_DAILY_STRIKES + 1

            ELSEIF(R_TYPE_OF_DERIV == HourlyOption) THEN
               IF(TEMP_REVENUE > TEMP_COST) THEN
                  GET_HOUR_DAY_OPTION_VALUE = &
                                  GET_HOUR_DAY_OPTION_VALUE + &
                                                TEMP_REVENUE - TEMP_COST
          call write_trace_real4(file_trace_ghov, "2. option value", &
                GET_HOUR_DAY_OPTION_VALUE)
                  R_DAILY_ENERGY_REVENUE = R_DAILY_ENERGY_REVENUE + &
                                                            TEMP_REVENUE
                  R_DAILY_VARIABLE_COST = R_DAILY_VARIABLE_COST + &
                                                            TEMP_COST
                  R_DAILY_ENERGY = R_DAILY_ENERGY + TEMP_QUANT
       call write_daily_energy_trace("transobj2:A004", R_DAILY_ENERGY)
                  R_HOUR_USAGE(I) = TEMP_QUANT
                  R_DAILY_STRIKES = R_DAILY_STRIKES + 1
               ELSE
                  R_STRIKE_QUANTITY(I) = 0.
               ENDIF
            ELSE ! ASSUMES Daily_Option FOR NOW
               TEMP_VALUE = TEMP_VALUE + TEMP_REVENUE - TEMP_COST
               R_DAILY_ENERGY_REVENUE = R_DAILY_ENERGY_REVENUE + &
                                                            TEMP_REVENUE
               R_DAILY_VARIABLE_COST = R_DAILY_VARIABLE_COST + &
                                                            TEMP_COST
               R_DAILY_ENERGY = R_DAILY_ENERGY + TEMP_QUANT
       call write_daily_energy_trace("transobj2:A005", R_DAILY_ENERGY)
               R_HOUR_USAGE(I) = TEMP_QUANT
               R_DAILY_STRIKES = R_DAILY_STRIKES + 1
               IF(I == 24 .AND. TEMP_VALUE < 0.000001) THEN ! DIDN'T STRIKE
                  R_STRIKE_QUANTITY = 0.
                  R_DAILY_STRIKES = 0
                  R_DAILY_ENERGY_REVENUE = 0.
                  R_DAILY_VARIABLE_COST = 0.
                  R_DAILY_ENERGY = 0.
       call write_daily_energy_trace("transobj2:A006", R_DAILY_ENERGY)
                  R_HOUR_USAGE = 0.
               ENDIF
            ENDIF ! R_TYPE_OF_DERIV
         ENDDO
      RETURN
      END function GET_HOUR_DAY_OPTION_VALUE
!
