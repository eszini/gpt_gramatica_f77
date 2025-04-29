module ft_interfaces
implicit none
contains

function get_prosym_interface_file()
character(len=256) ::ProSym_Interface_File,get_prosym_interface_file
	get_prosym_interface_file=ProSym_Interface_File()
end function get_prosym_interface_file
function get_catawba2_contract_file()
character(len=5) :: CATAWBA2_CONTRACT_FILE, get_CATAWBA2_CONTRACT_FILE
	get_catawba2_contract_file=CATAWBA2_CONTRACT_FILE()
	
end function get_catawba2_contract_file

function get_base_file_directory()
CHARACTER(len=256) :: BASE_FILE_DIRECTORY, get_base_file_directory

get_base_file_directory=BASE_FILE_DIRECTORY()

end function get_base_file_directory

function get_output_directory()
	character(len=256) :: OUTPUT_DIRECTORY, get_output_directory
	get_output_directory=OUTPUT_DIRECTORY()
	
end function get_OUTPUT_DIRECTORY

function get_results_dir()
CHARACTER(len=256) :: GET_RESULTS_DIRECTORY, get_results_dir
	get_results_dir=GET_RESULTS_DIRECTORY()
end function get_results_dir

function get_scenario_name()
CHARACTER(len=5) :: GET_SCENAME, get_scenario_name

	get_scenario_name=get_scename()
	
end function get_scenario_name
function get_initfil()
character(len=5) :: Initfil, get_initfil
	get_initfil=initfil()
	
end function get_initfil
function get_cpl_deferred_fuel_file()
character(len=255) :: get_cpl_deferred_fuel_file
character(len=5) :: cpl_deferred_fuel_file
	get_cpl_deferred_fuel_file=trim(cpl_deferred_fuel_file())
end function get_cpl_deferred_fuel_file

end module ft_interfaces