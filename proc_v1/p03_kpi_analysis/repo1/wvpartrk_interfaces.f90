module wvpartrk_interfaces
implicit none
      interface
         FUNCTION WVPA_RESOURCE_TRACKING_TYPE( &
                     R_WVPA_RESOURCE_TRACKING_TYPE)

         INTEGER*2 WVPA_RESOURCE_TRACKING_TYPE
         CHARACTER*(*) R_WVPA_RESOURCE_TRACKING_TYPE
         end FUNCTION WVPA_RESOURCE_TRACKING_TYPE
      end interface
      
      interface
          FUNCTION WVPA_FUEL_TRACKING_TYPE(R_WVPA_FUEL_TRACKING_TYPE)
             INTEGER*2 WVPA_FUEL_TRACKING_TYPE
             CHARACTER*(*) R_WVPA_FUEL_TRACKING_TYPE
          end FUNCTION WVPA_FUEL_TRACKING_TYPE
      end interface

    interface
        FUNCTION WVPA_MEM_TRACKING_TYPE(R_WVPA_MEM_TRACKING_TYPE)
            INTEGER*2 :: WVPA_MEM_TRACKING_TYPE
            CHARACTER (len=*) :: R_WVPA_MEM_TRACKING_TYPE
            CHARACTER(len=1) CHECK_VALUE
        end function WVPA_MEM_TRACKING_TYPE
    end interface
       
end module wvpartrk_interfaces