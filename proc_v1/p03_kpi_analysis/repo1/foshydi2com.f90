
      module foshydi2com
          use PROD_ARRAYS_DIMENSIONS
      implicit none
          integer (kind=2) :: ofline(max_cl_units)
          integer (kind=2) :: off_line(max_el_units)
          integer (kind=2) :: hydro_units
          integer (kind=2) :: online(max_cl_units)
          integer (kind=2) :: on_line(max_el_units)
          integer (kind=2) :: ai_cl_remaining_life(max_cl_units)
          integer (kind=2) :: ai_el_remaining_life(max_el_units)
          real (kind=4) :: CAP_FRAC_OWN_pad
      end module foshydi2com
