! rps_index_translate_mod.f90
!
      module rps_index_translate_mod
      implicit none

      contains
!
!

!**********************************************************************
      FUNCTION RPS_INDEX_TRANSLATE(R_PM)
!**********************************************************************
!        LOGICAL (kind=1) ::   IS_A_VALID_RPS_PM
         INTEGER (kind=2) ::   RPS_INDEX_TRANSLATE,R_PM,
     +              LOCAL_INDEX(8)/0,11,9,2,12,3,5,16/
! END DATA DEFINITIONS
         RPS_INDEX_TRANSLATE = LOCAL_INDEX(R_PM)
      RETURN
      END FUNCTION RPS_INDEX_TRANSLATE
!
!
!**********************************************************************
      end module rps_index_translate_mod
!
!
