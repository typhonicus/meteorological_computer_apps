PROGRAM homework90
    IMPLICIT NONE
    
        !DATA DICTIONARY 
        INTEGER, DIMENSION(17) :: pressure !values for pressure set into an array
        INTEGER, DIMENSION(17) :: height !values for height set into an array
        REAL, DIMENSION(17) :: temp !used to set values of temperature into an array
        INTEGER, DIMENSION(17) :: dewpoint !values for dew point set into an array
        REAL, DIMENSION(17) :: w_direction !values for the direction of the wind into an array
        REAL, DIMENSION(17) :: w_speed !the wind speed
        REAL, DIMENSION(17) :: u_wind !Values for the u-component of the wind into an array
        REAL, DIMENSION(17) :: v_wind !Values the v-component of the wind into an array
        INTEGER :: ios
        REAL :: pi 
        
        CHARACTER :: skipper !a way to skip the first few lines
        INTEGER :: i !do-loop counter that could probably be used as a day
        
       ! CHARACTER(len=74) :: file1, file2, file3 !the input files to work with the subroutine
        CHARACTER(len=76) :: output !The output file to work with the subroutine
    
        pi = 4*ATAN(1.0) !Accurate Pi value    
        
        !List of the 3 files to be read as well as the output file to be written to
       ! file1 = 'C:\Users\Typhonicus\Desktop\Fortran\Homework Projects\Homework9.9\jan1.txt'
       ! file2 = 'C:\Users\Typhonicus\Desktop\Fortran\Homework Projects\Homework9.8\apr1.txt'
        !file3 = 'C:\Users\Typhonicus\Desktop\Fortran\Homework Projects\Homework9.8\jul1.txt'
        !output= 'C:\Users\Typhonicus\Desktop\Fortran\Homework Projects\Homework9.9\output.txt'
        
        
    OPEN(UNIT=10, FILE= 'C:\Users\Typhonicus\Desktop\Fortran\Homework Projects\Homework9.9\jan1.txt', STATUS='OLD', ACTION='READ', IOSTAT=ios)
    OPEN(UNIT=20, FILE= 'C:\Users\Typhonicus\Desktop\Fortran\Homework Projects\Homework9.9\output.txt', STATUS='REPLACE', ACTION='WRITE')
        
    READ (10,100) skipper
    100 FORMAT (A1,///)

        DO i = 1, 17, 1
            IF (ios /= 0) EXIT !I could only make the read work by adding this. Otherwise, end of file errors abound.

            READ (10,150, IOSTAT=ios) pressure(i), height(i), temp(i), dewpoint(i), w_direction(i), w_speed(i)
            !**************pres****height***temp*******dew*******WD*******WS
            150 FORMAT (9X, I5, 2X, I5, 2X, F5.1, 2X, F5.1, 2X, F5.0, 2X, F5.0)       
            
            
            IF (pressure(i) == 10000) CYCLE !skips reading in lines in which the pressure value is technically underground
            IF (pressure(i) == 0) CYCLE !skips reading in zeroes dictated by the IOSTAT clause in the READ statement.
            
            !windbreaker
            pressure(i) = pressure(i) / 10
            
            w_direction(i) = w_direction(i) * (pi/180.)
            w_speed(i) = w_speed(i) * 0.514444444
            u_wind(i) = -(abs(w_speed(i))*sin(w_direction(i)))
            v_wind(i) = -(abs(w_speed(i))*cos(w_direction(i)))
            
            WRITE (20,200) pressure(i), height(i), temp(i), dewpoint(i), w_direction(i), w_speed(i), u_wind(i), v_wind(i)
            200 FORMAT (9X, I5, 2X, I5, 2X, F5.1, 2X, F5.1, 2X, F5.3, 2X, F5.1, 2X, F6.2, 2X, F6.2) 
            !**************pres****height***temp*******dew*******WD********WS********UW********VW
        END DO

END PROGRAM homework90
        !WRITE (20,*) u_wind(3) !test
    
        
    !OPEN(UNIT=12, FILE= file2, STATUS='OLD', ACTION='READ', IOSTAT=ios)
   ! OPEN(UNIT=24, FILE='C:\Users\Typhonicus\Desktop\Fortran\Homework Projects\Homework 9\aprnew.txt', STATUS='REPLACE', ACTION='WRITE')
    
  !  OPEN(UNIT=18, FILE= file3, STATUS='OLD', ACTION='READ', IOSTAT=ios)
  !  OPEN(UNIT=36, FILE='C:\Users\Typhonicus\Desktop\Fortran\Homework Projects\Homework 9\julnew.txt', STATUS='REPLACE', ACTION='WRITE')
        
        
        
        
        !INTEGER :: o !do-loop counter that could probably be used as a day

    
