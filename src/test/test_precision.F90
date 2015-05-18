program test_precision

  integer, parameter :: sp=kind(1.0e00)
  integer, parameter :: hp=kind(1.0d00)

  write(*,'(A, I2)') 'Precision of sp=kind(1.0e00) is ',sp
  write(*,'(A, I2)') 'Precision of hp=kind(1.0d00) is ',hp

end program
