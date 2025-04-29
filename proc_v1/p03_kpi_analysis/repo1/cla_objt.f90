
! cl_units_read_data module moved to file of same name.

! subroutine cl_object moved to cl_object_ext.f90



!          ROUTINE TO READ FOSSIL DIRECT ACESS BINARY FILES
!          COPYRIGHT (C) 1983, 84, 85  M.S. GERBER & ASSOCIATES, INC.



!     THIS FILE IS BEFORE THE YEAR COUNTER LOOP. CAPACITY
!     LIMITED UNITS' CHARACTERISTICS ARE READ IN AND PLACED INTO THE
!     PRODCOM.MON AND PROD2COM.MON FILES.



! cl_units_read function moved to cl_units_read_ext.f90.


      REAL FUNCTION GET_CL_POOL_FRAC_OWN(R_NUNITS)

      USE CL_UNITS_READ_DATA
      use forecast
      integer (kind=2) :: INTRA_COMPANY_CLASS_ID
      INTEGER (KIND=2) :: R_NUNITS
         GET_CL_POOL_FRAC_OWN = LOCAL_CL_POOL_FRAC_OWN(R_NUNITS)
      END FUNCTION
      
! Function get_p_fuel_delivery ...
! Function get_p_fuel_delivery_2 ... 
! Function get_fuel_delivery_3 ....
!                       moved to p_fuel_annl.



      FUNCTION SPECIAL_ID_NAME(R_NUNITS)

      character(len=20) :: cla_return_unitnm
      CHARACTER*20 SPECIAL_ID_NAME,RETURN_UNITNM,R_NAME
      LOGICAL*1 VOID_LOGICAL,CLA_SPECIAL_ID_NAME
      
      INTEGER*2 R_NUNITS
         VOID_LOGICAL = CLA_SPECIAL_ID_NAME(R_NUNITS,R_NAME)
         SPECIAL_ID_NAME = R_NAME
      RETURN

      ENTRY RETURN_UNITNM(R_NUNITS)
         r_name = CLA_RETURN_UNITNM(R_NUNITS)
         RETURN_UNITNM = R_NAME
      RETURN
      END

      FUNCTION CL_UNIQUE_RPT_STR(R_UNIT)
         use debugtrace
         use program_state
         
         implicit none
         integer :: file_trace_crs=0 
         integer (kind=2) :: TEMP_I2
         
         CHARACTER*5 CL_UNIQUE_RPT_STR,RPT_STR
         INTEGER*2 R_UNIT,CL_NUM
         INTEGER*4 UNIQUE_REPORT_VALUE_FOR_CL_UNIT
         if(trim(cur_called_from)=="") then
            stop "cur_called_from"
         end if

         
         if(file_trace_crs==0) then
            file_trace_crs=open_trace("cl_unique_rpt_str.trace", rq_crs)
         end if
         call write_trace_string(file_trace_crs, "called from", &
            cur_called_from)
         cur_called_from=""
         call write_trace_int2(file_trace_crs, "UNIT", R_UNIT)
         call write_trace_int4(file_trace_crs, "URV", &
            UNIQUE_REPORT_VALUE_FOR_CL_UNIT(R_UNIT))
         IF(UNIQUE_REPORT_VALUE_FOR_CL_UNIT(R_UNIT) < 32000) THEN 
            CL_NUM = UNIQUE_REPORT_VALUE_FOR_CL_UNIT(R_UNIT)
         ELSE
            CL_NUM = 32000
         ENDIF
         call write_trace_int2(file_trace_crs, "CL_NUM", CL_NUM)
         IF(CL_NUM <= 0) THEN
            RPT_STR = ' '
         ELSE
            WRITE(RPT_STR,'(I5)') CL_NUM

         ENDIF
         CL_UNIQUE_RPT_STR = RPT_STR
         call write_trace_string(file_trace_crs, "RPT_STR", RPT_STR)
      RETURN
      END function CL_UNIQUE_RPT_STR
      
      
      
! Function cl_screen_data moved to cl_screen_data_ext.f90


! FUNCTION CAP_MARKET_MONTH_NO_INDEX moved to cl_screen_data_ext.f90

! Function primary_mover_index_db moved to prim_mover_idx.f90

! subroutine CheckO3p moved to cl_screen_data_ext.f90
