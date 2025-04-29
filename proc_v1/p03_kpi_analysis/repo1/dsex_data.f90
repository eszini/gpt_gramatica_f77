module dsex_data
    use miscmod
	use cl_data

    implicit none
    contains
    function check_scename()
        character (len=5) :: result_value, check_scename
      result_value=" " ! Bypass spurious lahey warning
      !Nulls can be picked up presumably by how RealWin calls MIDAS.
      result_value=remove_nulls(cldata%scename)
      if(len_trim(result_value)==0) then
        call end_program("dsex:0002 - Scenario name is empty.")
      endif
      
      check_scename=result_value
      
    end function check_scename
end module dsex_data
