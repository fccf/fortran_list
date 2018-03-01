program list_test
  use list
  implicit none

  type(list_t) :: ll,l2

  integer :: i
  character(8) :: name

  call ll%append(1)
  call ll%append(2)
  call ll%append(3)
  call ll%append(4.0)


  call ll%insert(3,3)
  call ll%set(4,'4')
  call ll%set(5,5.)

  call ll%write()
  write(*,*)
  call write('ll',ll)

  do i = 1, ll%count
    write(name,'(i0)') i
    call write('ll('//trim(adjustl(name))//')',ll%index(i))
  enddo

  call l2%append(ll)
  call l2%append(.true.)
  call write('l2',l2)
  call write('l2(1)',l2%index(1))

end program list_test
