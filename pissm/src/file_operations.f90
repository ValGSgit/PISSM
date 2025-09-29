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
        logical :: is_dir, is_hidden
        
        file_count = 0
        temp_file = '/tmp/pissm_ls_output'
        
        ! Create enhanced ls command for detailed information
        if (show_hidden) then
            command = 'ls -laF --time-style=+"%Y-%m-%d %H:%M" "'//trim(path)//'" > '//trim(temp_file)//' 2>/dev/null'
        else
            command = 'ls -lF --time-style=+"%Y-%m-%d %H:%M" "'//trim(path)//'" > '//trim(temp_file)//' 2>/dev/null'
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
    
    subroutine parse_enhanced_ls_line(line, file_entry, base_path)
        character(len=*), intent(in) :: line, base_path
        type(file_entry), intent(out) :: file_entry
        
        character(len=256) :: parts(10)
        integer :: i, j, start_pos, part_count, iostat
        character(len=256) :: temp_line
        character :: file_type_char
        
        ! Initialize file_entry
        file_entry%name = ''
        file_entry%full_path = ''
        file_entry%is_directory = .false.
        file_entry%size = 0
        file_entry%modified_time = ''
        file_entry%permissions = ''
        file_entry%owner_group = ''
        file_entry%link_count = 0
        file_entry%selected = .false.
        
        temp_line = trim(adjustl(line))
        if (len_trim(temp_line) == 0) return
        
        ! Parse permissions (first 10 characters)
        file_entry%permissions = temp_line(1:10)
        file_type_char = temp_line(1:1)
        file_entry%is_directory = (file_type_char == 'd')
        
        ! Split the rest of the line into parts
        part_count = 0
        start_pos = 12  ! Skip permissions and space
        
        do i = start_pos, len_trim(temp_line)
            if (temp_line(i:i) /= ' ') then
                j = i
                do while (j <= len_trim(temp_line) .and. temp_line(j:j) /= ' ')
                    j = j + 1
                end do
                part_count = part_count + 1
                if (part_count <= size(parts)) then
                    parts(part_count) = temp_line(i:j-1)
                end if
                
                ! Skip multiple spaces
                do while (j <= len_trim(temp_line) .and. temp_line(j:j) == ' ')
                    j = j + 1
                end do
                i = j - 1
            end if
        end do
        
        ! Parse fields based on position
        if (part_count >= 8) then
            ! Link count
            read(parts(1), *, iostat=iostat) file_entry%link_count
            if (iostat /= 0) file_entry%link_count = 1
            
            ! Owner and group
            file_entry%owner_group = trim(parts(2))//'.'//trim(parts(3))
            
            ! Size
            if (.not. file_entry%is_directory) then
                read(parts(4), *, iostat=iostat) file_entry%size
                if (iostat /= 0) file_entry%size = 0
            end if
            
            ! Date and time (parts 5, 6)
            file_entry%modified_time = trim(parts(5))//' '//trim(parts(6))
            
            ! Filename (everything after time)
            do i = len_trim(temp_line), 1, -1
                if (temp_line(i:i) == ' ' .and. i > index(temp_line, trim(parts(6))) + len_trim(parts(6))) then
                    file_entry%name = temp_line(i+1:len_trim(temp_line))
                    exit
                end if
            end do
            
            ! Remove ls file type indicators (* / @ etc.)
            if (len_trim(file_entry%name) > 0) then
                i = len_trim(file_entry%name)
                if (file_entry%name(i:i) == '*' .or. file_entry%name(i:i) == '/' .or. &
                    file_entry%name(i:i) == '@' .or. file_entry%name(i:i) == '|') then
                    file_entry%name = file_entry%name(1:i-1)
                end if
            end if
        end if
        
        ! Set full path
        if (len_trim(file_entry%name) > 0) then
            file_entry%full_path = trim(base_path)//'/'//trim(file_entry%name)
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