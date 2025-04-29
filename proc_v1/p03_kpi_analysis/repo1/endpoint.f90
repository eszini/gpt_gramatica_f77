module endpoint
implicit none
integer (kind=2) :: grx_saved_endpoint=0
integer (kind=2) :: grx_prt_endpoint=0
contains
      function prt_endpoint()
      real (kind=4) :: prt_endpoint
         prt_endpoint = grx_saved_endpoint
      end function
end module endpoint