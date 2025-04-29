!     module extracted from common block.
!
      module aitvaprod
      use hesi_gendecs
      implicit none
        real :: cl_ai_capacity_rate(max_cl_units)
        real :: cl_ai_energy_rate(max_cl_units)
        real :: el_ai_capacity_rate(max_el_units)
        real :: el_ai_energy_rate(max_el_units)
        real :: ai_cl_tax_life(max_cl_units)
        real :: ai_cl_adr_life(max_cl_units)
        real :: ai_el_tax_life(max_el_units)
        real :: ai_el_adr_life(max_el_units)
          
!     from aitvaprodi2
        integer (kind=2) :: CL_AI_CAPACITY_ESCALATOR(MAX_CL_UNITS)
        integer (kind=2) :: CL_AI_ENERGY_ESCALATOR(MAX_CL_UNITS) 
        integer (kind=2) :: EL_AI_CAPACITY_ESCALATOR(MAX_EL_UNITS)
        integer (kind=2) :: EL_AI_ENERGY_ESCALATOR(MAX_EL_UNITS)
      end module aitvaprod
!
!