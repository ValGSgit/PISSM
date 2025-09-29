module file_operations
    use constants_mod
    use types_mod
    use utilities
    implicit none
    
contains
    
    subroutine scan_directory(path, files, file_count, show_hidden)
        character(len=*), intent(in) :: path
        type(file_entry), intent(out) :: files(:)
        integer, intent(out) :: file_count
        logical, intent(in) :: show_hidden
        
        character(len=512) :: command, temp_file, line
        integer :: unit_num, iostat, i
        logical :: is_hidden
        
        file_count = 0
        temp_file = '/tmp/pissm_ls_output'
        
        ! Create enhanced ls command for detailed information
        if (show_hidden) then
            command = 'ls -la --time-style=long-iso "'//trim(path)//'" > '//trim(temp_file)//' 2>/dev/null'
        else
            command = 'ls -l --time-style=long-iso "'//trim(path)//'" > '//trim(temp_file)//' 2>/dev/null'
        end if
        
        call execute_command_line(command)
        
        open(newunit=unit_num, file=temp_file, status='old', iostat=iostat)
        if (iostat /= 0) return
        
        ! Skip first line (total)
        read(unit_num, '(A)', iostat=iostat) line
        if (index(line, 'total') > 0) then
            read(unit_num, '(A)', iostat=iostat) line
        end if
        
        i = 1
        do while (iostat == 0 .and. i <= size(files))
            if (iostat /= 0) exit
            
            if (len_trim(line) > 0 .and. line(1:1) /= 't') then
                call parse_enhanced_ls_line(line, files(i), path)
                
                ! Check if hidden file
                is_hidden = (files(i)%name(1:1) == '.')
                
                ! Skip . and .. entries, and hidden files if not requested
                if (files(i)%name /= '.' .and. files(i)%name /= '..' .and. &
                    (show_hidden .or. .not. is_hidden)) then
                    i = i + 1
                end if
            end if
            
            read(unit_num, '(A)', iostat=iostat) line
        end do
        
        file_count = i - 1
        close(unit_num)
        
        ! Clean up temp file
        command = 'rm -f '//trim(temp_file)
        call execute_command_line(command)
    end subroutine scan_directory
    
    subroutine parse_enhanced_ls_line(line, entry, base_path)
        character(len=*), intent(in) :: line, base_path
        type(file_entry), intent(out) :: entry
        
        character(len=256) :: parts(20)  ! More parts to handle spaces in filenames
        integer :: i, num_parts, start_pos, end_pos, iostat
        character(len=256) :: temp_line
        character :: file_type_char
        logical :: in_word
        
        ! Initialize entry
        entry%name = ''
        entry%full_path = ''
        entry%is_directory = .false.
        entry%size = 0
        entry%modified_time = ''
        entry%permissions = ''
        entry%owner_group = ''
        entry%link_count = 0
        entry%selected = .false.
        
        temp_line = trim(adjustl(line))
        if (len_trim(temp_line) == 0) return
        
        ! Split line into space-separated parts (but preserve spaces in filename)
        num_parts = 0
        start_pos = 1
        in_word = .false.
        
        do i = 1, len_trim(temp_line)
            if (temp_line(i:i) /= ' ') then
                if (.not. in_word) then
                    ! Start of new word
                    num_parts = num_parts + 1
                    start_pos = i
                    in_word = .true.
                end if
            else
                if (in_word) then
                    ! End of word
                    end_pos = i - 1
                    if (num_parts <= 20) then
                        parts(num_parts) = temp_line(start_pos:end_pos)
                    end if
                    in_word = .false.
                end if
            end if
        end do
        
        ! Handle last word
        if (in_word) then
            end_pos = len_trim(temp_line)
            if (num_parts <= 20) then
                parts(num_parts) = temp_line(start_pos:end_pos)
            end if
        end if
        
        ! Must have at least 8 parts for valid ls -l output
        if (num_parts < 8) return
        
        ! Parse the fields
        entry%permissions = trim(parts(1))
        file_type_char = entry%permissions(1:1)
        entry%is_directory = (file_type_char == 'd')
        
        ! Link count
        read(parts(2), *, iostat=iostat) entry%link_count
        if (iostat /= 0) entry%link_count = 1
        
        ! Owner and group
        entry%owner_group = trim(parts(3))//'.'//trim(parts(4))
        
        ! Size
        if (.not. entry%is_directory) then
            read(parts(5), *, iostat=iostat) entry%size
            if (iostat /= 0) entry%size = 0
        end if
        
        ! Date and time (parts 6 and 7)
        entry%modified_time = trim(parts(6))//' '//trim(parts(7))
        
        ! Filename - everything from part 8 onwards (to handle spaces)
        entry%name = ''
        do i = 8, num_parts
            if (i > 8) entry%name = trim(entry%name)//' '
            entry%name = trim(entry%name)//trim(parts(i))
        end do
        
        ! Remove ls file type indicators
        if (len_trim(entry%name) > 0) then
            i = len_trim(entry%name)
            if (entry%name(i:i) == '*' .or. entry%name(i:i) == '/' .or. &
                entry%name(i:i) == '@' .or. entry%name(i:i) == '|') then
                entry%name = entry%name(1:i-1)
            end if
        end if
        
        ! Set full path
        if (len_trim(entry%name) > 0) then
            entry%full_path = trim(base_path)//'/'//trim(entry%name)
        end if
    end subroutine parse_enhanced_ls_line
    
    function copy_file(source, destination) result(success)
        character(len=*), intent(in) :: source, destination
        logical :: success
        character(len=1024) :: command
        integer :: status
        
        command = 'cp -r "'//trim(source)//'" "'//trim(destination)//'"'
        call execute_command_line(command, exitstat=status)
        success = (status == 0)
    end function copy_file
    
    function move_file(source, destination) result(success)
        character(len=*), intent(in) :: source, destination
        logical :: success
        character(len=1024) :: command
        integer :: status
        
        command = 'mv "'//trim(source)//'" "'//trim(destination)//'"'
        call execute_command_line(command, exitstat=status)
        success = (status == 0)
    end function move_file
    
    function delete_file(filepath) result(success)
        character(len=*), intent(in) :: filepath
        logical :: success
        character(len=1024) :: command
        integer :: status
        
        if (is_directory(filepath)) then
            command = 'rm -rf "'//trim(filepath)//'"'
        else
            command = 'rm -f "'//trim(filepath)//'"'
        end if
        
        call execute_command_line(command, exitstat=status)
        success = (status == 0)
    end function delete_file
    
    function create_directory(dirpath) result(success)
        character(len=*), intent(in) :: dirpath
        logical :: success
        character(len=1024) :: command
        integer :: status
        
        command = 'mkdir -p "'//trim(dirpath)//'"'
        call execute_command_line(command, exitstat=status)
        success = (status == 0)
    end function create_directory
    
    function create_file(filepath) result(success)
        character(len=*), intent(in) :: filepath
        logical :: success
        integer :: unit_num, iostat
        
        open(newunit=unit_num, file=trim(filepath), status='new', iostat=iostat)
        success = (iostat == 0)
        if (success) close(unit_num)
    end function create_file
    
    subroutine search_files(search_path, pattern, results, result_count)
        character(len=*), intent(in) :: search_path, pattern
        type(file_entry), intent(out) :: results(:)
        integer, intent(out) :: result_count
        
        character(len=1024) :: command, temp_file, line
        integer :: unit_num, iostat, i
        
        result_count = 0
        temp_file = '/tmp/pissm_search_output'
        
        command = 'find "'//trim(search_path)//'" -name "*'//trim(pattern)// &
                 '*" -type f > '//trim(temp_file)//' 2>/dev/null'
        call execute_command_line(command)
        
        open(newunit=unit_num, file=temp_file, status='old', iostat=iostat)
        if (iostat /= 0) return
        
        i = 1
        do while (iostat == 0 .and. i <= size(results))
            read(unit_num, '(A)', iostat=iostat) line
            if (iostat /= 0) exit
            
            if (len_trim(line) > 0) then
                results(i)%full_path = trim(line)
                
                ! Extract filename
                call extract_filename(line, results(i)%name)
                results(i)%is_directory = is_directory(trim(line))
                results(i)%size = get_file_size(trim(line))
                call get_current_time(results(i)%modified_time)
                results(i)%selected = .false.
                
                i = i + 1
            end if
        end do
        
        result_count = i - 1
        close(unit_num)
        
        ! Clean up temp file
        command = 'rm -f '//trim(temp_file)
        call execute_command_line(command)
    end subroutine search_files
    
    subroutine extract_filename(full_path, filename)
        character(len=*), intent(in) :: full_path
        character(len=*), intent(out) :: filename
        integer :: i
        
        do i = len_trim(full_path), 1, -1
            if (full_path(i:i) == '/') then
                filename = full_path(i+1:len_trim(full_path))
                return
            end if
        end do
        
        filename = trim(full_path)
    end subroutine extract_filename
    
end module file_operations