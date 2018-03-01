module list
  use, intrinsic :: iso_fortran_env, only: OUTPUT_UNIT
  implicit none


  public :: list_t, write


  private

  type item_t
    class(*), allocatable :: value
    type(item_t), pointer :: prev => null()
    type(item_t), pointer :: next => null()
  end type item_t

  type list_t
    integer :: count = 0
    type(item_t), pointer :: first => null()
    type(item_t), pointer :: last  => null()

  contains
    procedure :: is_empty => list_is_empty
    procedure :: append   => list_append_value
    procedure :: set      => list_set_value
    procedure :: insert   => list_insert_value
    procedure :: get      => list_get_value
    procedure :: index    => list_get_value
    procedure :: write    => list_write_value
  end type list_t

contains


  function list_is_empty(this) result(is)
    class(list_t), intent(in) :: this
    logical :: is

    is = .not.associated(this%first)

  end function list_is_empty


  subroutine list_append_value(this, value)
    !< append value to the list.
    class(list_t), intent(inout) :: this
    class(*), intent(in) :: value

    type(item_t), pointer :: item

    item => null()
    allocate(item)
    item%value = value

    if ( .not.associated(this%first)) then
      this%first => item
      this%last  => item

    else
      this%last%next => item
      item%prev => this%last
      this%last => this%last%next

    end if

    this%count = this%count + 1

  end subroutine list_append_value


  function list_get_value(this, index) result(value)
    class(list_t), intent(in) :: this
    integer, intent(in)   :: index
    class(*), allocatable :: value

    integer :: i
    type(item_t), pointer :: item

    item => null()
    if (index <= this%count) then
      item => this % first
      do i = 1, index - 1
        item => item % next
      end do
      value = item%value
    end if

  end function list_get_value


  recursive subroutine list_write_value(this, unit)
    class(list_t), intent(in) :: this
    integer, intent(in), optional :: unit
    character(len=31) :: string

    integer :: lunit
    integer :: i
    type(item_t), pointer :: item

    item => null()

    lunit = OUTPUT_UNIT
    if(present(unit)) lunit = unit

    write(lunit,'(a)',advance='no') '['

    item => this % first
    do i = 1, this%count
      select type(v_p => item%value)
      type is(integer)
        write(lunit,'(i0)',advance='no') v_p
      type is(real)
        write(string,fmt=*) v_p
        write(lunit,'(a)',advance='no') trim(adjustl(string))
      type is(logical)
        if (v_p) then
          write(lunit,'(a)',advance='no') 'true'
        else
          write(lunit,'(a)',advance='no') 'false'
        end if
      type is(character(*))
        write(lunit,'(3a)',advance='no') '"', v_p, '"'
      type is(list_t)
        call v_p%write(lunit)
      class default
        write(lunit,'(a)',advance='no') '*'
      end select

      if(i<this%count) write(lunit,'(a)',advance='no') ','
      item => item % next
    end do

    write(lunit,'(a)',advance='no') ']'

  end subroutine list_write_value


  subroutine write(name, value, unit)
    character(*), intent(in) :: name
    class(*), intent(in) :: value
    integer, intent(in), optional :: unit

    character(len=31) :: string
    integer :: lunit

    lunit = OUTPUT_UNIT
    if(present(unit)) lunit = unit

    write(lunit,'(a)',advance='no') name // '='

    select type(v_p => value)
    type is(integer)
      write(lunit,'(i0)',advance='no') v_p
    type is(real)
      write(string,fmt=*) v_p
      write(lunit,'(a)',advance='no') trim(adjustl(string))
    type is(logical)
      if (v_p) then
        write(lunit,'(a)',advance='no') 'true'
      else
        write(lunit,'(a)',advance='no') 'false'
      end if
    type is(character(*))
      write(lunit,'(3a)',advance='no') '"', v_p, '"'
    type is(list_t)
      call v_p%write(lunit)
    class default
      write(lunit,'(a)',advance='no') '*'
    end select
    write(lunit,*)

  end subroutine write


  subroutine list_set_value(this, index, value)
    class(list_t), intent(in) :: this
    integer, intent(in)  :: index
    class(*), intent(in) :: value

    type(item_t), pointer :: item
    integer :: i

    item => null()
    if (index <= this % count) then
      item => this % first
      do i = 1, index - 1
        item => item % next
      end do
      item % value = value
    end if

  end subroutine list_set_value


  subroutine list_insert_value(this, index, value)
    class(list_t), intent(inout) :: this
    integer, intent(in)  :: index
    class(*), intent(in) :: value

    integer :: i
    type(item_t), pointer :: item
    type(item_t), pointer :: newitem

    item => null()
    newitem => null()
    if (index <= this % count) then
      allocate(newitem)
      newitem%value = value
      newitem%next => null()
      item => this%first
      do i = 1, index - 2
        item => item % next
      end do
      newitem%next => item%next
      item%next => newitem
      this%count = this % count + 1
    end if

  end subroutine list_insert_value


end module list
