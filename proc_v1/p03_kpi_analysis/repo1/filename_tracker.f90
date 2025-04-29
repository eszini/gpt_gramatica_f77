module filename_tracker
use LH_GLOBAL_VARIABLES
use conversion_routines
use ft_interfaces
use params
use logging

implicit none

contains
subroutine log_filename_request(caller, fname)
character(*), intent(in) :: fname
character (len=1024) :: filename
character (len=256) :: tf
character(*), intent(in) :: caller
integer :: fn_len
integer :: index, lastindex, asciival, destindex
character (len=256), dimension(1000) :: AllFilenames=""
integer :: LastFilenameIndex=0
logical :: found

	filename=fname

	tf=""
		
	lastindex=len_trim(filename)
	destindex=1
	do index=1, lastindex
		asciival=iachar(filename(index:index))
		if (asciival<24 .or. asciival>128) then
			tf=trim(tf)
			exit
		else
			tf(destindex:destindex)=achar(asciival)
			destindex=destindex+1
		endif
	enddo
	
	destindex=1
	
	filename=trim(tf)
	
	found=.false.
	if (LastFilenameIndex>0) then
		do index=1, LastFilenameIndex
			if(trim(AllFilenames(index))==trim(filename)) then
				found=.true.
				exit
			endif
		enddo
	else
		LastFilenameIndex=1
		AllFilenames(LastFilenameIndex)=filename
	endif
	
	if(.not. found) then
		log_message="Filename requested for caller " // &
			trim(caller) // ", filename: " //trim(filename)
	endif
	
	
	
end subroutine log_filename_request

function get_aab_filename()
	CHARACTER(len=5) :: BASE_FILE_NAME 
    character(len=256 ):: DATA_DRIVE
	character(len=256) :: get_aab_filename
	character(len=5) :: AREA_ALLOCATORS_FILE
	base_file_name=""
    BASE_FILE_NAME = AREA_ALLOCATORS_FILE()
    DATA_DRIVE = trim(get_OUTPUT_DIRECTORY())
    get_aab_filename = trim(get_BASE_FILE_DIRECTORY())//"AAB"//trim(BASE_FILE_NAME)//'.DAT'
	call log_filename_request("get_aab_filename", get_aab_filename)
end function get_aab_filename

function get_aao_filename(overlay_family_name)
	CHARACTER(len=5) :: OVERLAY_FAMILY_NAME
    CHARACTER(len=256) :: DATA_DRIVE, get_aao_filename
	DATA_DRIVE = trim(get_OUTPUT_DIRECTORY())
    get_aao_filename = trim(DATA_DRIVE)//"AAO"//trim(OVERLAY_FAMILY_NAME)//'.DAT'
	call log_filename_request("get_aao_filename", get_aao_filename)

end function get_aao_filename

function get_dab_filename(directory_for_base_files, OVER_UNDER_DECISIONS_TABLES)
character(len=256) :: get_dab_filename, directory_for_base_files
character(len=5) :: OVER_UNDER_DECISIONS_TABLES
	get_dab_filename=trim(DIRECTORY_FOR_BASE_FILES)//'DAB'//trim(OVER_UNDER_DECISIONS_TABLES)//'.DAT'
	call log_filename_request("get_dab_filename", get_dab_filename)
end function get_dab_filename

function get_apb_filename(DIRECTORY_FOR_BASE_FILES, WVPA_ACTUAL_PURCHASES)
character(len=256) :: get_apb_filename, DIRECTORY_FOR_BASE_FILES, WVPA_ACTUAL_PURCHASES
	get_apb_filename=TRIM(DIRECTORY_FOR_BASE_FILES)//'APB'//TRIM(WVPA_ACTUAL_PURCHASES)//'.DAT'
	call log_filename_request("get_apb_filename", get_apb_filename)
end function get_apb_filename

function get_ilb_filename(directory_for_base_files, WVPA_ACTUAL_PURCHASES)
CHARACTER(len=256) :: directory_for_base_files, WVPA_ACTUAL_PURCHASES, get_ilb_filename
	get_ilb_filename=TRIM(DIRECTORY_FOR_BASE_FILES)//'ILB'//TRIM(WVPA_ACTUAL_PURCHASES)//'.DAT'
	call log_filename_request("get_ilb_filename", get_ilb_filename)
end function get_ilb_filename

function get_c2b_filename()
character(len=5) :: BASE_FILE_NAME
character(len=256) ::  get_c2b_filename, DATA_DRIVE


	BASE_FILE_NAME = get_CATAWBA2_CONTRACT_FILE()
    DATA_DRIVE = trim(get_OUTPUT_DIRECTORY())
    get_c2b_filename = trim(get_BASE_FILE_DIRECTORY())//"C2B"//trim(BASE_FILE_NAME)//'.DAT'
	call log_filename_request("get_c2b_filename", get_c2b_filename)
	
end function get_c2b_filename
function get_mrx_full_filename(mrx_expansion_plan_file_code, &
	sequ_no)
character(len=256) :: get_mrx_full_filename
character(len=2) :: mrx_expansion_plan_file_code
integer(kind=2) :: sequ_no

    get_mrx_full_filename = TRIM(get_OUTPUT_DIRECTORY())//MRX_EXPANSION_PLAN_FILE_CODE// &
	 "O"//MRX_EXPANSION_PLAN_FILE_NAME//TRIM(CONVERT_2_STR(SEQU_NO,3))//'.DAT'
	 call log_filename_request("get_mrx_full_filename", get_mrx_full_filename)
     if(index(get_mrx_full_filename, "BCPNWCP.BIN")>1) then
        get_mrx_full_filename=get_mrx_full_filename ! debugstop
     end if

end function get_mrx_full_filename

function get_project_filename(projname)
character(len=256) :: projname, get_project_filename
	get_project_filename="DSF"//trim(projname)//'.dat'
	call log_filename_request("get_project_filename", get_project_filename)
end function get_project_filename

function get_gcb_filename(base_file_name)
character(len=256) :: get_gcb_filename
character(len=5) :: base_file_name
	get_gcb_filename=trim(get_BASE_FILE_DIRECTORY())//"GGB"//trim(BASE_FILE_NAME)//'.DAT'
	call log_filename_request("get_gcb_filename", get_gcb_filename)
 
end function get_gcb_filename

function get_ggo_filename(data_drive, overlay_family_name)
character(len=256) :: get_ggo_filename, data_drive
character(len=5) :: overlay_family_name

	get_ggo_filename=trim(DATA_DRIVE)//"GGO"//trim(OVERLAY_FAMILY_NAME)//'.DAT'
	call log_filename_request("get_ggo_filename", get_ggo_filename)
end function get_ggo_filename
function get_gtb_filename(BASE_FILE_NAME)
character(len=5) :: BASE_FILE_NAME
character(len=256) :: get_gtb_filename
	get_gtb_filename=trim(get_BASE_FILE_DIRECTORY())//"GTB"//trim(BASE_FILE_NAME)//'.DAT'
	call log_filename_request("get_gtb_filename", get_gtb_filename)
	
end function get_gtb_filename

function get_gto_filename(data_drive, overlay_family_name)
character(len=256) :: get_gto_filename
character(len=256) :: DATA_DRIVE, overlay_family_name

	get_gto_filename=trim(DATA_DRIVE)//"GTO"//trim(OVERLAY_FAMILY_NAME)//'.DAT'
	call log_filename_request("get_gto_filename", get_gto_filename)
end function get_gto_filename

function get_coal_qq_filename(extra_STRING, NORTH_AMERICAN_DATABASE)
character(len=256) :: get_coal_qq_filename, retval
character(len=4096) :: extra_STRING
logical :: NORTH_AMERICAN_DATABASE

	extra_STRING = TRIM(extra_STRING)
    IF(NORTH_AMERICAN_DATABASE) THEN
		retval = 'Q1BZZ'
	ELSE
		retval = 'Q2BZZ'
	ENDIF

	get_coal_qq_filename = TRIM(retval)//TRIM(extra_STRING)//'.DAT'
	call log_filename_request("get_coal_qq_filename", get_coal_qq_filename)
		 
end function get_coal_qq_filename
function get_pao_filename(data_drive, overlay_family_name)
character(len=256) :: get_pao_filename
character(len=5) :: overlay_family_name
character(len=256) :: data_drive

	get_pao_filename=trim(DATA_DRIVE)//"PAO"//trim(OVERLAY_FAMILY_NAME)//'.DAT'
	call log_filename_request("get_pao_filename", trim(get_pao_filename))
	
end function get_pao_filename

function get_pab_filename(base_file_name)
character(len=5) :: BASE_FILE_NAME
character(len=256) :: get_pab_filename, result

	result=trim(get_BASE_FILE_DIRECTORY())//"PAB"//trim(BASE_FILE_NAME)//'.DAT'
	get_pab_filename=result
	call log_filename_request("get_pab_filename", trim(result))
end function get_pab_filename

function get_scn_ovl_filename()
character(len=256) :: get_scn_ovl_filename, result
	result=trim(GET_RESULTS_DIR())//'OVL'//trim(get_scenario_name())//'.DAT'
	get_scn_ovl_filename=trim(result)
	call log_filename_request(trim("get_scn_ovl_filename"), trim(result))
end function get_scn_ovl_filename

function get_dfb_filename(BASE_FILE_NAME)
character(len=256) :: get_dfb_filename
character(len=5) :: BASE_FILE_NAME
	get_dfb_filename=trim(get_BASE_FILE_DIRECTORY())//"DFB"//trim(BASE_FILE_NAME)//'.DAT'
	call log_filename_request("get_dfb_filename", get_dfb_filename)

end function get_dfb_filename
function get_dfb_filename_ii()
character(len=256) :: get_dfb_filename_ii
	get_dfb_filename_ii=trim(get_BASE_FILE_DIRECTORY())//"DFB"//trim(get_CPL_DEFERRED_FUEL_FILE())//'.DAT'
	call log_filename_request("get_dfb_filename_ii", get_dfb_filename_ii)
end function get_dfb_filename_ii

function get_b_filename(base_file_name, file_code)
character(len=256) :: get_b_filename
character(len=2) :: file_code
character(len=5) :: BASE_FILE_NAME

	get_b_filename=trim(get_BASE_FILE_DIRECTORY())//FILE_CODE//"B"//trim(BASE_FILE_NAME)//'.DAT'
	call log_filename_request("filename_tracker:0001", get_b_filename)
end function get_b_filename

function get_overlay_filename(data_drive, file_code, overlay_family_name)
character(len=5) :: overlay_family_name
character(len=256) :: data_drive
character(len=2) :: file_code
character(len=256) :: get_overlay_filename

	get_overlay_filename=trim(DATA_DRIVE)//FILE_CODE//"O"//trim(OVERLAY_FAMILY_NAME)//'.DAT'
	call log_filename_request("get_overlay_filename", get_overlay_filename)
	
	 
end function get_overlay_filename
function get_makebin_filename(base_file_name, descriptor)
character(len=3) :: descriptor
character(len=5) :: BASE_FILE_NAME
character(len=256) :: get_makebin_filename

	get_makebin_filename=trim(get_BASE_FILE_DIRECTORY())//descriptor//trim(BASE_FILE_NAME)//'.DAT'
	call log_filename_request("get_makebin_filename", get_makebin_filename)
end function get_makebin_filename


function get_makeovl_filename(data_drive, descriptor, overlay_family_name)
character(len=5) :: overlay_family_name
character(len=3) :: descriptor
character(len=256) :: get_makeovl_filename, DATA_DRIVE
	get_makeovl_filename= trim(data_drive) // descriptor // trim(OVERLAY_FAMILY_NAME)//".DAT"
	call log_filename_request("get_makeovl_filename", get_makeovl_filename)

end function get_makeovl_filename

function get_fib_filename(base_file_name)
character(len=256) :: get_fib_filename, return_value
character(len=5) :: base_file_name

	return_value=trim(get_BASE_FILE_DIRECTORY())// &
       "FIB"//trim(BASE_FILE_NAME)//'.DAT'
	   
	   get_fib_filename=return_value
	   
end function get_fib_filename
function get_ggb_filename(base_file_name)
character(len=256) :: get_ggb_filename
character(len=5) :: BASE_FILE_NAME

	get_ggb_filename=trim(get_BASE_FILE_DIRECTORY())//"GGB"//trim(BASE_FILE_NAME)//".DAT"
	call log_filename_request("get_ggb_filename", get_ggb_filename)
	

end function get_ggb_filename
function get_ino_filename(data_drive, overlay_family_name)
character(len=256) :: get_ino_filename, data_drive
character(len=5) :: overlay_family_name

	get_ino_filename=trim(DATA_DRIVE)//"INO"//trim(OVERLAY_FAMILY_NAME)//'.DAT'
	call log_filename_request("get_ino_filename", get_ino_filename)
	 
end function get_ino_filename
function get_inb_filename()
character(len=256) :: get_inb_filename
	get_inb_filename=trim(get_BASE_FILE_DIRECTORY())// &
		"INB"//trim(get_INITFIL())//'.DAT'
		
	call log_filename_request("get_inb_filename", get_inb_filename)
end function get_inb_filename

function get_atb_filename(base_file_name)
character(len=256) :: get_atb_filename
character(len=5) :: base_file_name
	get_atb_filename=trim(get_BASE_FILE_DIRECTORY()) // "ATB" // trim(base_file_name)//".DAT"
	call log_filename_request("get_atb_filename", get_atb_filename)
end function get_atb_filename

function get_ato_filename(DATA_DRIVE, overlay_family_name)
character(len=256) :: get_ato_filename, data_drive
character(len=5) :: overlay_family_name 
	get_ato_filename=trim(DATA_DRIVE) // "ATO" // trim(overlay_family_name)//".DAT"
	call log_filename_request("get_ato_filename", get_ato_filename)
end function get_ato_filename

function get_20b_filename(base_file_name)
character(len=256) :: get_20b_filename
character(len=5) :: base_file_name
	get_20b_filename=trim(get_BASE_FILE_DIRECTORY()) // "2OB"//trim(BASE_FILE_NAME)//'.DAT'
	call log_filename_request("get_20b_filename", get_20b_filename)
end function get_20b_filename
function get_2OO_filename(data_drive, overlay_family_name)
character(len=256) :: get_2OO_filename, data_drive
character(len=5) :: overlay_family_name

	get_2OO_filename=trim(DATA_DRIVE)//"2OO"// &
	 trim(OVERLAY_FAMILY_NAME)//'.DAT'
	 
	call log_filename_request("get_2OO_filename", get_2OO_filename)
end function get_2OO_filename

function get_rqb_filename(base_file_name)
character(len=256) :: get_rqb_filename
character(len=5) :: base_file_name
character(len=256) :: base_file_directory

	base_file_directory=get_BASE_FILE_DIRECTORY()
	
    get_rqb_filename=trim(base_file_directory) // &
		"RQB"// trim(base_file_name)//'.DAT'
		
	call log_filename_request("get_rqb_filename", get_rqb_filename)
		
end function get_rqb_filename

function get_rqo_filename(data_drive, overlay_family_name)
character(len=256) :: get_rqo_filename, data_drive
character(len=5) :: overlay_family_name
	get_rqo_filename=trim(DATA_DRIVE)//"RQO"//trim(OVERLAY_FAMILY_NAME)//'.DAT'
	call log_filename_request("get_rqo_filename", get_rqo_filename)
end function get_rqo_filename

function get_psb_filename()
character(len=256) :: get_psb_filename
	get_psb_filename=trim(get_BASE_FILE_DIRECTORY())//'PSB'//TRIM(get_ProSym_Interface_File())//'.DAT'
	call log_filename_request("get_psb_filename", get_psb_filename)
end function get_psb_filename

function get_pso_filename(OverlayName)
character(len=256) :: get_pso_filename
character(len=5) :: OverlayName
	get_pso_filename=trim(get_BASE_FILE_DIRECTORY()) //'PSO'//TRIM(OverlayName)//'.DAT'
	call log_filename_request("get_pso_filename", get_pso_filename)
end function get_pso_filename

function get_rlb_filename(BASE_FILE_NAME)
character(len=256) :: get_rlb_filename
character(len=5) :: BASE_FILE_NAME
integer :: base_fn_index
	get_rlb_filename=TRIM(get_BASE_FILE_DIRECTORY())//'RLB'//TRIM(base_file_name)
	call log_filename_request("get_rlb_filename", get_rlb_filename)
end function get_rlb_filename

function get_teb_filename(base_file_name)
character(len=256) :: get_teb_filename
character(len=5) :: base_file_name
	get_teb_filename=trim(get_BASE_FILE_DIRECTORY())//"TEB"//trim(BASE_FILE_NAME)//".DAT"
	call log_filename_request("get_teb_filename", get_teb_filename)
end function get_teb_filename

function get_ppb_filename(base_file_name)
use filemod
character(len=256) :: get_ppb_filename, dir, otherdir
character(len=5) :: base_file_name
    dir=get_BASE_FILE_DIRECTORY()
    otherdir=get_OUTPUT_DIRECTORY()
    
	get_ppb_filename=trim(get_BASE_FILE_DIRECTORY())// "PPB"//trim(BASE_FILE_NAME)//'.DAT'
	call log_filename_request("get_ppb_filename", get_ppb_filename)
    if(.not. does_file_exist(get_ppb_filename)) then
        call end_program("filename_tracker:0002 - File " // &
        trim(get_ppb_filename) // " does not exist.")
    endif
end function get_ppb_filename

function get_ppo_filename(data_drive, overlay_family_name)
character(len=5) :: overlay_family_name
character(len=256) :: get_ppo_filename, data_drive
character(len=256) :: workdir
    workdir=trim(get_OUTPUT_DIRECTORY())
	get_ppo_filename=trim(DATA_DRIVE)//"PPO"//trim(OVERLAY_FAMILY_NAME)//'.DAT'
	call log_filename_request("get_ppo_filename", get_ppo_filename)
	 
end function get_ppo_filename

function get_rdo_filename(data_drive, overlay_family_name)
character(len=5) :: overlay_family_name
character(len=256) :: get_rdo_filename, data_drive
	get_rdo_filename=trim(DATA_DRIVE)//"RDO"//trim(OVERLAY_FAMILY_NAME)//'.DAT'
	call log_filename_request("get_rdo_filename", get_rdo_filename)
end function get_rdo_filename

function get_rbo_filename(data_drive, overlay_family_name)
character(len=5) :: overlay_family_name
character(len=256) :: get_rbo_filename, data_drive
	get_rbo_filename=trim(DATA_DRIVE)//"RBO"//trim(OVERLAY_FAMILY_NAME)//'.DAT'
	call log_filename_request("get_rbo_filename", get_rbo_filename)
end function get_rbo_filename

function get_trb_filename(base_file_name)
character(len=256) :: get_trb_filename
character(len=5) :: BASE_FILE_NAME
	get_trb_filename=trim(get_BASE_FILE_DIRECTORY())//"TRB"//trim(BASE_FILE_NAME)//'.DAT'
	call log_filename_request("get_trb_filename", get_trb_filename)
end function get_trb_filename

function get_teo_filename(data_drive, overlay_family_name)
character(len=256) :: get_teo_filename, data_drive
character(len=5) :: overlay_family_name
	get_teo_filename=trim(DATA_DRIVE)//"TEO"//trim(OVERLAY_FAMILY_NAME)//'.DAT'
	call log_filename_request("get_teo_filename", get_teo_filename)
end function get_teo_filename

function get_tto_filename(data_drive, overlay_family_name)
character(len=256) :: get_tto_filename, DATA_DRIVE
character(len=5) :: overlay_family_name
	get_tto_filename=trim(DATA_DRIVE)//"TTO"//trim(OVERLAY_FAMILY_NAME)//'.DAT'
	call log_filename_request("get_tto_filename", get_tto_filename)
end function get_tto_filename

function get_ft_filename(file_code, base_file_name)
character(len=256) :: get_ft_filename
character(len=5) :: BASE_FILE_NAME
character(len=3) :: file_code
	get_ft_filename=trim(get_BASE_FILE_DIRECTORY()) // file_code // trim(base_file_name)//".DAT"
	call log_filename_request("get_ft_filename", get_ft_filename)
end function get_ft_filename

function get_dtb_filename(base_file_name)
character(len=256) :: get_dtb_filename
character(len=5) :: BASE_FILE_NAME

	get_dtb_filename=trim(get_BASE_FILE_DIRECTORY())//"DTB"//trim(BASE_FILE_NAME)//'.DAT'
	call log_filename_request("get_dtb_filename", get_dtb_filename)
end function get_dtb_filename

function get_dto_filename(data_drive, overlay_family_name)
character(len=5) ::  overlay_family_name
character(len=256) :: get_dto_filename, DATA_DRIVE
	get_dto_filename=trim(DATA_DRIVE)//"DTO"//trim(OVERLAY_FAMILY_NAME)//'.DAT'
	call log_filename_request("get_dto_filename", get_dto_filename)
end function get_dto_filename

function get_rmb_filename(base_file_name)
character(len=256) :: get_rmb_filename
character(len=5) :: base_file_name

	get_rmb_filename=trim(get_BASE_FILE_DIRECTORY())//"RMB"//trim(BASE_FILE_NAME)//'.DAT'
	call log_filename_request("get_rmb_filename", get_rmb_filename)
end function get_rmb_filename
function get_o_filename(data_drive, file_code, overlay_family_name)
character(len=256) :: get_o_filename,DATA_DRIVE
character(len=5) :: overlay_family_name
character(len=2) :: file_code
	get_o_filename=trim(data_drive)//file_code//"O"//trim(overlay_family_name)//".DAT"
	call log_filename_request("get_o_filename", get_o_filename)
end function get_o_filename

function get_ipb_filename(BASE_FILE_NAME)
character(len=256) :: get_ipb_filename
character(len=5) :: BASE_FILE_NAME

	get_ipb_filename=trim(get_BASE_FILE_DIRECTORY())//"IPB"//trim(BASE_FILE_NAME)//'.DAT'
	call log_filename_request("get_ipb_filename", get_ipb_filename)
end function get_ipb_filename
function get_ipo_filename(data_drive, overlay_family_name)
character(len=256) :: get_ipo_filename, DATA_DRIVE
character(len=5) :: overlay_family_name

	get_ipo_filename=trim(DATA_DRIVE)//"IPO"//trim(overlay_family_name)//".DAT"
	call log_filename_request("get_ipo_filename", get_ipo_filename)
	

end function get_ipo_filename

function get_tnb_filename(base_file_name)
character(len=256) :: get_tnb_filename
character(len=5) :: base_file_name

	get_tnb_filename=trim(get_BASE_FILE_DIRECTORY())//"TNB"//trim(BASE_FILE_NAME)//'.DAT'
	call log_filename_request("get_tnb_filename", get_tnb_filename)
end function get_tnb_filename


function get_tno_filename(data_drive, overlay_family_name)
character(len=256) :: get_tno_filename, DATA_DRIVE
character(len=5) :: overlay_family_name

	get_tno_filename=trim(data_drive)//"TNO"//trim(OVERLAY_FAMILY_NAME)//'.DAT'
	call log_filename_request("get_tno_filename", get_tno_filename)

end function get_tno_filename

function get_rob_filename(base_file_name)
character(len=256) :: get_rob_filename
character(len=5) :: base_file_name

	get_rob_filename=trim(get_BASE_FILE_DIRECTORY())//"ROB"//trim(BASE_FILE_NAME)//".DAT"
	call log_filename_request("get_rob_filename", get_rob_filename)

end function get_rob_filename

function get_roo_filename(data_drive, overlay_family_name)
character(len=256) :: get_roo_filename, data_drive
character(len=5) :: overlay_family_name
	get_roo_filename=trim(DATA_DRIVE)//"ROO"//trim(OVERLAY_FAMILY_NAME)//'.DAT'
	call log_filename_request("get_roo_filename", get_roo_filename)
end function get_roo_filename


function get_rmo_filename(data_drive, overlay_family_name)
character(len=256) :: get_rmo_filename, data_drive
character(len=5) :: overlay_family_name
	get_rmo_filename=trim(DATA_DRIVE)//"RMO"//trim(OVERLAY_FAMILY_NAME)//'.DAT'
	call log_filename_request("get_rmo_filename", get_rmo_filename)
	 
end function get_rmo_filename


function get_gpb_filename(base_file_name)
character(len=256) :: get_gpb_filename
character(len=5) :: base_file_name
	get_gpb_filename=trim(get_BASE_FILE_DIRECTORY())//"GPB"//trim(BASE_FILE_NAME)//'.DAT'
	call log_filename_request("get_gpb_filename", get_gpb_filename)
end function get_gpb_filename

function get_gpo_filename(data_drive, overlay_family_name)
character(len=256) :: get_gpo_filename
character(len=5) :: overlay_family_name, data_drive

	get_gpo_filename=trim(data_drive)//"GPO"//trim(OVERLAY_FAMILY_NAME)//'.DAT'
	call log_filename_request("get_gpo_filename", get_gpo_filename)
end function get_gpo_filename

function get_osf_filename(r_project_name)
character(len=256) :: return_value, get_osf_filename
character(*) :: r_project_name
	
	return_value="OSF"//trim(R_PROJECT_NAME)//'.DAT'
    get_osf_filename=return_value
	 
end function get_osf_filename

end module filename_tracker


