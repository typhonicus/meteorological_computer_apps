PROGRAM nove
     IMPLICIT NONE
    
        !DATA DICTIONARY 
        INTEGER, DIMENSION(17) :: pressure, pressure_Apr, pressure_Jul !values for pressure set into arrays
        INTEGER, DIMENSION(17) :: height, height_Apr, height_Jul !values for height set into arrays
        
        REAL, DIMENSION(17) :: temp, temp_Apr, temp_Jul!used to set values of temperature into arrays
        INTEGER, DIMENSION(17) :: dewpoint, dewpoint_Apr, dewpoint_Jul!values for dew point set into arrays
        
        REAL, DIMENSION(17) :: w_direction, w_direction_Apr, w_direction_Jul!values for the direction of the wind into arrays
        REAL, DIMENSION(17) :: w_speed, w_speed_Apr, w_speed_Jul !the wind speed
        REAL, DIMENSION(17) :: u_wind, u_wind_Apr, u_wind_Jul!Values for the u-component of the wind into arrays
        REAL, DIMENSION(17) :: v_wind, v_wind_Apr, v_wind_Jul!Values the v-component of the wind into arrays
        
        REAL :: shear700, shear500, shear700_Apr, shear500_Apr, shear700_Jul, shear500_Jul !Values for shear
        REAL, DIMENSION(3) :: shearArray700, shearArray500 !Shear values read into an array
        CHARACTER(LEN=10), DIMENSION(3) :: stormType700, stormType500  !Storm type read into an array
        
        REAL :: lapse700, lapse500, lapse700_Apr, lapse500_Apr, lapse700_Jul, lapse500_Jul !Lapse rate values
        REAL, DIMENSION(3) :: lapseArray700, lapseArray500 !Lapse rate values read into an array
        CHARACTER(LEN=11), DIMENSION(3) :: lapseRate700, lapseRate500
        
        INTEGER :: ios !iostat uses this to make sure things keep working when reading stuff in
        CHARACTER :: skipper !a way to skip the first few lines
        INTEGER :: i !do-loop counter that could probably be used as a day
        CHARACTER(LEN=7) :: month !used in a case black to select for month.
        
        !The following set of lines open January's sounding and reads its contents into their respective arrays
    OPEN(UNIT=10, FILE='C:\Users\Typhonicus\Desktop\Fortran\Homework Projects\nove\jan1.txt', STATUS='OLD', ACTION='READ', IOSTAT=ios)
    READ (10,100) skipper
    100 FORMAT (A1,///)
        DO i = 1, 17, 1
            IF (ios /= 0) EXIT !I could only make the read work by adding this. Otherwise, end of file errors abound.
            !Temperature and dew point are purposefully read in incorrectly to skip the calculation from tenths.
            READ (10,150, IOSTAT=ios) pressure(i), height(i), temp(i), dewpoint(i), w_direction(i), w_speed(i)
            !**************pres****height***temp*******dew*******WD********WS
            150 FORMAT (9X, I5, 2X, I5, 2X, F5.1, 2X, F5.1, 2X, F5.0, 2X, F5.0)       

        END DO
   CLOSE(10)
    
   !The following set of lines open April's sounding and reads its contents into their respective arrays    
    OPEN(UNIT=20, FILE='C:\Users\Typhonicus\Desktop\Fortran\Homework Projects\nove\apr1.txt', STATUS='OLD', ACTION='READ', IOSTAT=ios)
    READ (20,100) skipper
        DO i = 1, 17, 1
            IF (ios /= 0) EXIT !I could only make the read work by adding this. Otherwise, end of file errors abound.
            !Temperature and dew point are purposefully read in incorrectly to skip the calculation from tenths.
            READ (20,150, IOSTAT=ios) pressure_Apr(i), height_Apr(i), temp_Apr(i), dewpoint_Apr(i), w_direction_Apr(i), w_speed_Apr(i)      
        END DO
    CLOSE(20)  
    
    !The following set of lines open July's sounding and reads its contents into their respective arrays
    OPEN(UNIT=30, FILE='C:\Users\Typhonicus\Desktop\Fortran\Homework Projects\nove\jul1.txt', STATUS='OLD', ACTION='READ', IOSTAT=ios)
    READ (30,100) skipper
        DO i = 1, 17, 1
            IF (ios /= 0) EXIT !I could only make the read work by adding this. Otherwise, end of file errors abound.
            !Temperature and dew point are purposefully read in incorrectly to skip the calculation from tenths.
            READ (30,150, IOSTAT=ios) pressure_Jul(i), height_Jul(i), temp_Jul(i), dewpoint_Jul(i), w_direction_Jul(i), w_speed_Jul(i)      
        END DO
    CLOSE(30)
    !The following line opens the write-file to be written to.
    OPEN(UNIT=60, FILE='C:\Users\Typhonicus\Desktop\Fortran\Homework Projects\nove\output.txt', STATUS='REPLACE', ACTION='WRITE')
    
    !The following three CALLs link to a subroutine that converts and calculates values related to the wind
    CALL winds(w_direction, w_speed, u_wind, v_wind)
    CALL winds(w_direction_Apr, w_speed_Apr, u_wind_Apr, v_wind_Apr)
    CALL winds(w_direction_Jul, w_speed_Jul, u_wind_Jul, v_wind_Jul)
    
    !The following three CALLs link to a subroutine that calculates shear
    CALL shear(u_wind(1), u_wind(5), u_wind(6), v_wind(1), v_wind(5), v_wind(6), shear700, shear500)
    CALL shear(u_wind_Apr(1), u_wind_Apr(5), u_wind_Apr(6), v_wind_Apr(1), v_wind_Apr(5), v_wind_Apr(6), shear700_Apr, shear500_Apr)
    CALL shear(u_wind_Jul(1), u_wind_Jul(5), u_wind_Jul(6), v_wind_Jul(1), v_wind_Jul(5), v_wind_Jul(6), shear700_Jul, shear500_Jul)
    
    !the following three CALLs link to a subroutine that calculates the lapse rate
    CALL lapse(temp(1), temp(5), temp(6), height(1), height(5), height(6), lapse700, lapse500)
    CALL lapse(temp_Apr(1), temp_Apr(5), temp_Apr(6), height_Apr(1), height_Apr(5), height_Apr(6), lapse700_Apr, lapse500_Apr)
    CALL lapse(temp_Jul(1), temp_Jul(5), temp_Jul(6), height_Jul(1), height_Jul(5), height_Jul(6), lapse700_Jul, lapse500_Jul)
    
    !The following lines assign the calculated values for shear and lapse rate unto specific positions with an array.
    shearArray700(1) = shear700
    shearArray700(2) = shear700_Apr
    shearArray700(3) = shear700_Jul
    
    shearArray500(1) = shear500 
    shearArray500(2) = shear500_Apr
    shearArray500(3) = shear500_Jul
    
    lapseArray700(1) = lapse700
    lapseArray700(2) = lapse700_Apr
    lapseArray700(3) = lapse700_Jul
    
    lapseArray500(1) = lapse500 
    lapseArray500(2) = lapse500_Apr
    lapseArray500(3) = lapse500_Jul

    !The following two CALLs make the final 'determinations' with regard to shear and the lapse rate
    CALL stormSelector(shearArray700, shearArray500, stormType700, stormType500) 
    CALL lapseSelector(lapseArray700, lapseArray500, lapseRate700, lapseRate500)
    
    !The following formatted write statements are used to write a relatively descriptive header unto the output file.
    WRITE(60, 165)
    165 FORMAT ('         The following information was calculated from archived soundings of Roanoke, VA during the first of January, April, and July of 2016.')
    WRITE(60, 168)
    168 FORMAT ('         "Shear Type" denotes the likelihood of a particular storm type based on deep-layer shear. ')
    WRITE(60,*) ' '   
    WRITE(60, 170)
    170 FORMAT ('         |---------------------------700mb---------------------------------]|[----------------------------------500mb---------------------------------]')   
    WRITE(60, 175)
    175 FORMAT ('         |  Shear(m/s)  |  Shear Type  | LapseRate(°C/km) |    LapseType   ]|[    Shear(m/s)     |   Shear Type   | LapseRate(°C/km) |   LapseType    ]') 
    
    WRITE(60, 180)
    180 FORMAT ('=========|=================================================================]|[========================================================================]')    
    
    !This loop writes the collected calculated data to a write file in a neat format
    DO i = 1, 3, 1
        !The following case block takes the place of a character array for month that could do the same thing
        SELECT CASE (i)
        CASE (1)
            month = 'January'
        CASE (2)
            month = 'April'
        CASE (3)
            month = 'July'
        CASE DEFAULT
            month = 'ERROR'
        END SELECT
        WRITE(60,200) month, shearArray700(i), stormType700(i), lapseArray700(i), lapseRate700(i), shearArray500(i), stormType500(i), lapseArray500(i), lapseRate500(i)
        200 FORMAT (A7, 2X, '|', 4X, F5.2, 5X, '|', 2X, A10, 2X, '|', 6X, F5.2, 7X, '|', 3X, A11, 2X, ']|[', 7X, F5.2, 7X, '|', 3X, A10, 3X, '|', 6X, F4.2, 8X, '|', 3X, A11, '  ]')       
        !**********month*************sh700*************typ700************lps700*************rt700****************sh500*************typ500************lps500*************rt500
    END DO 
    
    END PROGRAM nove

    
    
    !#######################################################################
    !The following subroutine is pertinent to the wind
    SUBROUTINE winds (verso, veloce, u, v)
        IMPLICIT NONE
       
        INTEGER :: x !used to iterate the do-loop
        REAL, DIMENSION(17), INTENT(INOUT) :: verso !wind direction dummy
        REAL, DIMENSION(17), INTENT(INOUT) :: veloce !wind speed dummy
        REAL, DIMENSION(17), INTENT(OUT) :: u !u-component wind dummy
        REAL, DIMENSION(17), INTENT(OUT) :: v !v-component wind dummy
        REAL :: pi 
        
        pi = 4*ATAN(1.0) !Accurate Pi value 
            
            DO x = 1, 17, 1 !The do-loop converts the input arrays and outputs wind components
                
                verso(x) = verso(x) * (pi/180.) !Converts direction to degrees
                veloce(x) = veloce(x) * 0.514444444 !Converts knots to m/s'
                
                !The following two lines break the wind into its components
                u(x) = -(abs(veloce(x))*sin(verso(x)))
                v(x) = -(abs(veloce(x))*cos(verso(x)))
            END DO        
    END SUBROUTINE winds
    !########################################################################
    
    
    !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    !The following subroutine calculates shear
    SUBROUTINE shear (u1000, u700, u500, v1000, v700, v500, tosatura700, tosatura500)
        IMPLICIT NONE    
        
        REAL, INTENT(OUT) :: tosatura700, tosatura500 !shear dummy
        REAL, INTENT(IN) :: u1000, u700, u500 !u-component wind dummy
        REAL, INTENT(IN) :: v1000, v700, v500 !v-component wind dummy
        
        !The following lines calculate shear values
        tosatura700 = sqrt(((u700-u1000)**2) + ((v700-v1000)**2))
        tosatura500 = sqrt(((u500-u1000)**2) + ((v500-v1000)**2))
        
        !Case block?
        
    END SUBROUTINE shear
    !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    
    !v^V^v^v^V^v^v^V^v^v^V^v^v^V^v^v^V^v^v^V^v^v^V^v^v^V^v^v^V^v^v^V^v^v^V^v^
    !The following subroutine calculates the lapse rate
    SUBROUTINE lapse (calore1000, calore700, calore500, altezza1000, altezza700, altezza500, gradiente700, gradiente500)
        IMPLICIT NONE
        REAL, INTENT(IN) :: calore1000, calore700, calore500
        INTEGER, INTENT(IN) :: altezza1000, altezza700, altezza500
        REAL, INTENT(OUT) :: gradiente700, gradiente500
        
        !The following lines calculate the value for lapse rate
        gradiente700 = -((calore700 - calore1000) / ((altezza700 - altezza1000) / 1000.))
        gradiente500 = -((calore500 - calore1000) / ((altezza500 - altezza1000) / 1000.)) 
    END SUBROUTINE lapse
    !v^V^v^v^V^v^v^V^v^v^V^v^v^V^v^v^V^v^v^V^v^v^V^v^v^V^v^v^V^v^v^V^v^v^V^v^
            
    !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    !The following subroutine determines storm-type favorability based on deep-layer shear
    SUBROUTINE stormSelector (t_schiera700, t_schiera500, typh700, typh500) 
        IMPLICIT NONE
        REAL, DIMENSION(3), INTENT(IN) :: t_schiera700, t_schiera500 !shear arrays
        CHARACTER(LEN=10), DIMENSION(3), INTENT(OUT) :: typh700, typh500 !storm type array
        INTEGER :: o !the iterative variable for this subroutine
        
        !The following loops determine storm-type based on shear with respect to a specified pressure level using IF-blocks
        DO o = 1, 3, 1
            IF (t_schiera700(o) > 20.) THEN
                typh700(o) = 'Supercells'
            ELSE IF (t_schiera700(o) > 10.) THEN
                typh700(o) = 'Multicells'
            ELSE IF (t_schiera700(o) <= 10.) THEN
                typh700(o) = 'Ordinary'
            ELSE
                typh700(o) = 'Error'           
            ENDIF
        END DO
        
        DO o = 1, 3, 1
            IF (t_schiera500(o) > 20.) THEN
                typh500(o) = 'Supercells'
            ELSE IF (t_schiera500(o) > 10.) THEN
                typh500(o) = 'Multicells'
            ELSE IF (t_schiera500(o) <= 10.) THEN
                typh500(o) = 'Ordinary'
            ELSE
                typh500(o) = 'Error'           
            ENDIF
        END DO
    
    END SUBROUTINE
    !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !The following subroutine uses the input lapse rate to determine stability
    SUBROUTINE lapseSelector(gradSchiera700, gradSchiera500, gradRate700, gradRate500)
     IMPLICIT NONE
        REAL, DIMENSION(3), INTENT(IN) :: gradSchiera700, gradSchiera500
        CHARACTER(LEN=11), DIMENSION(3), INTENT(OUT) :: gradRate700, gradRate500
        INTEGER :: u
        
        !The following do-loops determine stability using an IF-block
        DO u  = 1, 3, 1
            IF (gradSchiera700(u) > 10.) THEN
                gradRate700(u) = 'Unstable'
            ELSE IF (gradSchiera700(u) >= 6.) THEN
                gradRate700(u) = 'Conditional'
            ELSE IF (gradSchiera700(u) < 6.) THEN
                gradRate700(u) = 'Stable'
            ELSE
                gradRate700(u) = 'Error'           
            ENDIF
        END DO
        
        DO u  = 1, 3, 1
            IF (gradSchiera500(u) > 10.) THEN
                gradRate500(u) = 'Unstable'
            ELSE IF (gradSchiera500(u) >= 6.) THEN
                gradRate500(u) = 'Conditional'
            ELSE IF (gradSchiera500(u) < 6.) THEN
                gradRate500(u) = 'Stable'
            ELSE
                gradRate500(u) = 'Error'           
            ENDIF
        END DO 

    
    END SUBROUTINE
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~