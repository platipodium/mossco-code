subroutine ma()
      use m1
      call a()
end subroutine

subroutine mb()
      use m2
      call b()
end subroutine

program main
      call ma()
      call mb()

end program main
