PROGRAM theEnd !begins the program called "theEnd"
    IMPLICIT NONE !prevents FORTRAN from treating variables that begin with certain letters from being treated as integers and stuff.
    
        !Data Dictionary
            !The following variables have been obtained from the Storm Events Database input from a self-made csv-file.
            CHARACTER(LEN=12), DIMENSION(8) :: city !An array for the cities of the hail event input from the csv file
            CHARACTER(LEN=11), DIMENSION(8) :: county !redundant/unnecessary since the sample hail events occur in Roanoke County.
            CHARACTER(LEN=10), DIMENSION(8) :: date !An array for the dates of the hail events relative to their city.
            INTEGER, DIMENSION(8) :: time !The time of the hail event with respect to the date and city (style: UTC-0000).
            REAL, DIMENSION (8) :: hail_Diameter !An array for the size of the observed hailstones measured in INCHES with respect to their diameter.
            REAL :: stDev_hail_Diameter !the standard deviation of the diameters of the hail relative to their respective events.
            REAL :: mean_hail_Diameter !the average size of the observed hail among the sample 8 events.
            
            REAL, DIMENSION(15,8) :: pressure !An Array for the values of pressure to be read from a text-based sounding.
            INTEGER, DIMENSION(15,8) :: height !An array for the height, given pressure, read from a text-based sounding.
            REAL, DIMENSION(15,8) :: temperature !!An array for the temperature(°C), given pressure, read from a text-based sounding.
            REAL, DIMENSION(15,8) :: K_Temperature !!An array for the temperature(°K), given pressure, used for calculations.            
            REAL, DIMENSION(15,8) :: dewpoint !An array for the dewPoint, given pressure, read from a text-based sounding.
            REAL, DIMENSION(15,8) :: K_Dewpoint !An array for the dewpoint, in KELVIN(°K), given pressure read from a text-based sounding.
            REAL, DIMENSION(15,8) :: wetbulb_Temp !An array for the wet-bulb temperature, calculated from temperature and mixing ratio.
            REAL, DIMENSION(15,8) :: wetbulb_Temp_Calc !A duplicate array for the wet-bulb temperature so as to be used in subsequent calculations.
            REAL, DIMENSION(8) :: wetbulb_Zero !An array for the height at which the wet-bulb temperature is equal to the freezing level.
           !REAL, DIMENSION(8) :: liftedIndex !Poor man's CAPE. Unused because I didn't trust the output.
            REAL, DIMENSION(8) :: K_Index !Homeless man's CAPE. Used to diagnose the general profile of the area
            CHARACTER(LEN=18), DIMENSION(8) :: K_Interpretation !Interpretations of the output K-Index
            
            REAL, DIMENSION(15,8) :: vaporPressure !An array for the partial pressures of water vapor calculated using the Clausius-Clapeyron equation.
            REAL, DIMENSION(15,8) :: satVaporPressure !An array for the maximum amount of vapor the air can assume before condesation begins.
            
            REAL, DIMENSION(15,8) :: R_Humidity !An array for the relative humidity, given pressure, read from a text-based sounding.
            REAL, DIMENSION(15,8) :: mixingRatio !An array for the mixingRatio, given pressure, read from a text-based sounding.
            REAL, DIMENSION(15,8) :: satMixingRatio !An array for the saturation mixing ratio
            REAL, DIMENSION(8) :: avgMixingRatio !An array for the average mixing ratio throughout the entire sounding of each event.
            REAL, DIMENSION(8) :: precipitable_water !precipitable water calculated from pressure change, mixing ratio, and gravity  .
            REAL :: stDev_precipitable_water !the standard deviation of the precipitable water across.
            REAL :: mean_precipitable_water !the average value for precipitable across the 8 sample events.
            REAL, DIMENSION(8) :: pressureChange !The change in pressure from the surface to the 500mb.
            REAL, DIMENSION(8) :: lapseRate !The wet-bulb lapse-rate necessary to approximate the wet-bulb zero height.
            
            INTEGER :: special !a variable used as a divisor as well as an iterator. Based on array size.
            INTEGER :: columnNumber !a variable used to as a divisor as well as an iterator. Based on array size.
            
            REAL, DIMENSION(15,8) :: direction !An array for the direction, given pressure, read from a text-based sounding.
            REAL, DIMENSION(15,16) :: speed !A 2-dimensional array for the wind speed, given pressure, read from a text-based sounding.
            REAL, DIMENSION(15,16) :: UV_wind !A 2-dimensional array used to store U & V wind components.
            REAL, DIMENSION(8) :: shear500 !An array for the bulk shear magnitude. Calculated from surface to 500mb U & V wind components.
            REAL :: stDev_shear500 !the standard deviation of the shear magnitude across the 8 sample events.
            REAL :: mean_shear500 !the average value of shear magnitude from each sample event.
            
            INTEGER, DIMENSION(8) :: heightCount !an array that takes in the sums of the feet necessary to get to freezing level.
            
            REAL :: pi !Accurate Pi value.
            INTEGER :: i, n !the variables that iterate do-loops throughout the code.
            INTEGER :: ios !probably not necessary this time, but the book says to always at least put it where it is.
            CHARACTER :: csvHeader !a way to skip the first line of the csv file.
            CHARACTER :: skipper !a way to skip the first few lines of each input text file.
 
    !The following line opens a csv file that will be used to read in basic information (like city) about the hail events.        
    OPEN(UNIT=8, FILE='C:\Users\Typhonicus\Desktop\Fortran\Homework Projects\Final\basichailstuff.csv', STATUS='OLD', ACTION='READ', IOSTAT=ios)
    !OPEN(UNIT=18, FILE='C:\Users\Typhonicus\Desktop\Fortran\Homework Projects\Final\info.txt', STATUS='REPLACE', ACTION='WRITE')
    
    !The following formatted read statement is a way to skip over the first long string of characters in the read file.
    READ(8,100) csvHeader !reads in the first letter of the first line in order to jump to the next line.
    100 FORMAT (A1) 
    DO i = 1, 8, 1 !a do loop that reads in values from the csv-file to their respective arrays.
        READ (8,*,IOSTAT=ios) city(i), county(i), date(i), time(i), hail_Diameter(i) !An unformatted read that reads in data. 
    END DO !ends the do loop that reads the csv-file
    CLOSE(8)!closes the csv file
    
    columnNumber = size(time) !Assigns the number of elements in time array to the columnNumber variable. 
    !The following lines opens the 8 text files to be read as well as the text file to be written to.
    OPEN(UNIT=9, FILE='C:\Users\Typhonicus\Desktop\Fortran\Homework Projects\Final\05162001.txt', STATUS='OLD', ACTION='READ', IOSTAT=ios)
    OPEN(UNIT=10, FILE='C:\Users\Typhonicus\Desktop\Fortran\Homework Projects\Final\03242005.txt', STATUS='OLD', ACTION='READ', IOSTAT=ios)
    OPEN(UNIT=11, FILE='C:\Users\Typhonicus\Desktop\Fortran\Homework Projects\Final\06072005.txt', STATUS='OLD', ACTION='READ', IOSTAT=ios)
    OPEN(UNIT=12, FILE='C:\Users\Typhonicus\Desktop\Fortran\Homework Projects\Final\06022008.txt', STATUS='OLD', ACTION='READ', IOSTAT=ios)
    OPEN(UNIT=13, FILE='C:\Users\Typhonicus\Desktop\Fortran\Homework Projects\Final\05112011.txt', STATUS='OLD', ACTION='READ', IOSTAT=ios)
    OPEN(UNIT=14, FILE='C:\Users\Typhonicus\Desktop\Fortran\Homework Projects\Final\06062011.txt', STATUS='OLD', ACTION='READ', IOSTAT=ios)
    OPEN(UNIT=15, FILE='C:\Users\Typhonicus\Desktop\Fortran\Homework Projects\Final\08022012.txt', STATUS='OLD', ACTION='READ', IOSTAT=ios)
    OPEN(UNIT=16, FILE='C:\Users\Typhonicus\Desktop\Fortran\Homework Projects\Final\07172013.txt', STATUS='OLD', ACTION='READ', IOSTAT=ios)
    OPEN(UNIT=18, FILE='C:\Users\Typhonicus\Desktop\Fortran\Homework Projects\Final\test.txt', STATUS='REPLACE', ACTION='WRITE')
    
    
    !The following read block reads in data from the text-based sounding relative to a hail event that occured on 05-15-2001. 
    READ (9,110) skipper !This line skips the first few lines of the text file.
    110 FORMAT (A1,///) !This line represents the format in which the lines will be skipped.  
    READ (9,125) pressure(1,1), height(1,1), temperature(1,1), dewpoint(1,1), direction(1,1), speed(1,1) !A formatted read that reads in the first row of data.
    125 FORMAT (9X, F5.0, 2X, I5, 2X, F5.1, 2X, F5.1, 2X, F5.0, 2X, F5.0, /) !Format that skips over the false 10000 pressure data line.
    !Believe me, I wanted to use a CYCLE statement, but I coudn't get it to work with read (it only worked if I tried to write).
        DO i = 2, 15, 1 !A do-loop that reads in the rest of the raw data after having skipped a line of false data.
                        !Can adjust the number of iterations to gain more accurate averages down the line.
            !The follow formatted read is almost identical to 125 FORMAT, but it uses an iterative variable and doesn't skip a line.
            READ (9,150) pressure(i,1), height(i,1), temperature(i,1), dewpoint(i,1), direction(i,1), speed(i,1)
            !***************pres****height****temp*******dew*******WD********WS
            150 FORMAT (9X, F5.0, 2X, I5, 2X, F5.1, 2X, F5.1, 2X, F5.0, 2X, F5.0)
            !Temperature and dew point are purposefully read in "incorrectly" to skip the calculation from tenths.
            !I suppose I could've done the same thing with pressure, but oh well.
        END DO !Ends the do-loop that read in the last 14 rows of raw data from the input text-file.
    CLOSE(9) !closes the 05-15-2001 text-based sounding file.

    special = size(pressure(:,1)) !assigns the number of elements in one column of the pressure array to the special variable.
    pressure = pressure / 10. !converts pressuree from decapascals to hectopascals/millibars.
    
    !The following read block reads in data from the text-based sounding relative to a hail event that occured on 03-23-2005.
    READ (10,110) skipper !skips the first few lines of the text file.      
    READ (10,125) pressure(1,2), height(1,2), temperature(1,2), dewpoint(1,2), direction(1,2), speed(1,3)!A formatted read that reads in the first row of data.
        DO i = 2, special, 1 !A do-loop that reads in the rest of the raw data after having skipped a line of false data.
            !The follow formatted read is almost identical to 125 FORMAT, but it uses an iterative variable and doesn't skip a line.
            READ (10,150) pressure(i,2), height(i,2), temperature(i,2), dewpoint(i,2), direction(i,2), speed(i,3)
        END DO !Ends the do-loop that read in the last 14 rows of raw data from the input text-file.
    CLOSE(10) !closes the 03-23-2005 text-based sounding file.
    
    !The following read block reads in data from the text-based sounding relative to a hail event that occured on 06-06-2005.
    READ (11,110) skipper !skips the first few lines of the text file.       
    READ (11,125) pressure(1,3), height(1,3), temperature(1,3), dewpoint(1,3), direction(1,3), speed(1,5)!A formatted read that reads in the first row of data.
        DO i = 2, special, 1 !A do-loop that reads in the rest of the raw data after having skipped a line of false data.
            !The follow formatted read is almost identical to 125 FORMAT, but it uses an iterative variable and doesn't skip a line.
            READ (11,150) pressure(i,3), height(i,3), temperature(i,3), dewpoint(i,3), direction(i,3), speed(i,5)
        END DO !Ends the do-loop that read in the last 14 rows of raw data from the input text-file.
    CLOSE(11) !closes the 06-06-2005 text-based sounding file.

    !The following read block reads in data from the text-based sounding relative to a hail event that occured on 06-01-2008.
    READ (12,110) skipper !skips the first few lines of the text file.       
    READ (12,125) pressure(1,4), height(1,4), temperature(1,4), dewpoint(1,4), direction(1,4), speed(1,7)!A formatted read that reads in the first row of data.
        DO i = 2, special, 1 !A do-loop that reads in the rest of the raw data after having skipped a line of false data.
            !The follow formatted read is almost identical to 125 FORMAT, but it uses an iterative variable and doesn't skip a line.
            READ (12,150) pressure(i,4), height(i,4), temperature(i,4), dewpoint(i,4), direction(i,4), speed(i,7)
        END DO !Ends the do-loop that read in the last 14 rows of raw data from the input text-file.
    CLOSE(12) !closes the 06-01-2008 text-based sounding file.
    
    !The following read block reads in data from the text-based sounding relative to a hail event that occured on 05-10-2011.
    READ (13,110) skipper !skips the first few lines of the text file .      
    READ (13,125) pressure(1,5), height(1,5), temperature(1,5), dewpoint(1,5), direction(1,5), speed(1,9)!A formatted read that reads in the first row of data.
        DO i = 2, special, 1 !A do-loop that reads in the rest of the raw data after having skipped a line of false data.
            !The follow formatted read is almost identical to 125 FORMAT, but it uses an iterative variable and doesn't skip a line.
            READ (13,150) pressure(i,5), height(i,5), temperature(i,5), dewpoint(i,5), direction(i,5), speed(i,9)
        END DO !Ends the do-loop that read in the last 14 rows of raw data from the input text-file.
    CLOSE(13) !closes the 05-10-2011 text-based sounding file. 

    !The following read block reads in data from the text-based sounding relative to a hail event that occured on 06-05-2011.
    READ (14,110) skipper !skips the first few lines of the text file.       
    READ (14,125) pressure(1,6), height(1,6), temperature(1,6), dewpoint(1,6), direction(1,6), speed(1,11)!A formatted read that reads in the first row of data.
        DO i = 2, special, 1 !A do-loop that reads in the rest of the raw data after having skipped a line of false data.
            !The follow formatted read is almost identical to 125 FORMAT, but it uses an iterative variable and doesn't skip a line.
            READ (14,150) pressure(i,6), height(i,6), temperature(i,6), dewpoint(i,6), direction(i,6), speed(i,11)
        END DO !Ends the do-loop that read in the last 14 rows of raw data from the input text-file.
    CLOSE(14) !closes the 06-05-2011 text-based sounding file. 

    !The following read block reads in data from the text-based sounding relative to a hail event that occured on 08-01-2012.
    READ (15,110) skipper !skips the first few lines of the text file  .     
    READ (15,125) pressure(1,7), height(1,7), temperature(1,7), dewpoint(1,7), direction(1,7), speed(1,13)!A formatted read that reads in the first row of data.
        DO i = 2, special, 1 !A do-loop that reads in the rest of the raw data after having skipped a line of false data.
            !The follow formatted read is almost identical to 125 FORMAT, but it uses an iterative variable and doesn't skip a line.
            READ (15,150) pressure(i,7), height(i,7), temperature(i,7), dewpoint(i,7), direction(i,7), speed(i,13)
        END DO !Ends the do-loop that read in the last 14 rows of raw data from the input text-file.
    CLOSE(15) !closes the 08-01-2012 text-based sounding file. 

    !The following read block reads in data from the text-based sounding relative to a hail event that occured on 07-16-2013. 
    READ (16,110) skipper !skips the first few lines of the text file       
    READ (16,125) pressure(1,8), height(1,8), temperature(1,8), dewpoint(1,8), direction(1,8), speed(1,15)!A formatted read that reads in the first row of data.
        DO i = 2, special, 1 !A do-loop that reads in the rest of the raw data after having skipped a line of false data.
            !The follow formatted read is almost identical to 125 FORMAT, but it uses an iterative variable and doesn't skip a line.
            READ (16,150) pressure(i,8), height(i,8), temperature(i,8), dewpoint(i,8), direction(i,8), speed(i,15)
        END DO !Ends the do-loop that read in the last 14 rows of raw data from the input text-file.
    CLOSE(16) !closes the 07-16-2013 text-based sounding file. 
   
    
!%%%%%%%%%%%%%%%%%%%%%%% Windbreaker %%%%%%%%%%%%%%%%%%%%%%%
!So I made this a little bit too complicated and should have just put the u and v compenents in separate arrays. Oh well. 
speed(:,2) = speed(:,1) !copies and pastes all the values from the first column of the wind speed array into the second column.
speed(:,4) = speed(:,3) !copies and pastes all the values from the third column of the wind speed array into the fourth column.
speed(:,6) = speed(:,5) !copies and pastes all the values from the fifth column of the wind speed array into the sixth column.
speed(:,8) = speed(:,7) !copies and pastes all the values from the seventh column of the wind speed array into the eigth column.
speed(:,10) = speed(:,9) !copies and pastes all the values from the ninth column of the wind speed array into the tenth column.
speed(:,12) = speed(:,11) !copies and pastes all the values from the eleventh column of the wind speed array into the twelth column.
speed(:,14) = speed(:,13) !copies and pastes all the values from the thirteenth column of the wind speed array into the fourteenth column.   
speed(:,16) = speed(:,15) !copies and pastes all the values from the fifteenth column of the wind speed array into the sixteenth column.

speed = speed * 0.51444444 !converts all elements of the speed array from knots to m/s.
UV_wind = speed !mirrors all the values of the speed array into the U & V component array so that it can be used break the wind into its components.
pi = 4*ATAN(1.0) !Accurate Pi value.
direction = direction * (pi/180.) !converts the entire direction array from degrees to radians

UV_wind = windbreaker(special, direction, UV_wind) !sends the 2D wind speed array into a function that returns the u and v components of the wind into columns 1 and 2.
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


!&&&&&&&&&&&&&&&&&&&&&&&&& Shear &&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!The following CALLs link to a subroutine that calculates shear.
!Based on U & V winds at the surface and those same winds at the 500mb level.
!Returns a value of shear magnitude to an array positioned relative to the particular hail event.
    CALL shear(UV_wind(1,1), UV_wind(5,1), UV_wind(1,2), UV_wind(5,2), shear500(1)) !05-15-2001
    CALL shear(UV_wind(1,3), UV_wind(5,3), UV_wind(1,4), UV_wind(5,4), shear500(2)) !03-23-2005
    CALL shear(UV_wind(1,5), UV_wind(5,5), UV_wind(1,6), UV_wind(5,6), shear500(3)) !06-06-2005
    CALL shear(UV_wind(1,7), UV_wind(5,7), UV_wind(1,8), UV_wind(5,8), shear500(4)) !06-01-2008
    CALL shear(UV_wind(1,9), UV_wind(5,9), UV_wind(1,10), UV_wind(5,10), shear500(5)) !05-10-2011
    CALL shear(UV_wind(1,11), UV_wind(5,11), UV_wind(1,12), UV_wind(5,12), shear500(6)) !06-05-2011
    CALL shear(UV_wind(1,13), UV_wind(5,13), UV_wind(1,14), UV_wind(5,14), shear500(7)) !08-01-2012
    CALL shear(UV_wind(1,15), UV_wind(5,15), UV_wind(1,16), UV_wind(5,16), shear500(8)) !07-16-2013
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
 
    
!$$$$$$$$$$$$$$$$$$$$$$ Moisture Stuff $$$$$$$$$$$$$$$$$$$$$$$$
K_Temperature = temperature + 273.15 !Copies all the elements of the temperature array to the K_Temperature array can converts them to KELVIN.
K_Dewpoint = dewpoint + 273.15 !Copies all the elements of the dewpoint array to the K_Dewpoint array can converts them to KELVIN.
!vaporPressure = 6.11 * EXP((2500000./461.) * ((1 / 273.15) - (1 / K_Dewpoint))) -- The less accurate clausius-clapeyron equation.
vaporPressure = 6.11 * EXP(19.83 - (5417. / K_Dewpoint)) !Fills the vaporPressure array with values of vapor pressure, in mb, using the Clausius-Clapeyron Equation
satVaporPressure = 6.11 * EXP(19.83 - (5417. / K_Temperature)) !Fills the satVaporPressure array values of saturated vapor pressure, in mb, using the Clausius-Clapeyron Equation
R_Humidity = humidity(special, vaporPressure, satVaporPressure) !Calculates relative humidity using a function that takes in (saturation/)vapor pressure, and 1D extent.

!The following two lines calculates the mixing ratio and saturation mixing ratio, respectively, 
!using total pressure and vapor pressure & saturation vapor pressure, respectively.
mixingRatio = (287. / 481.) * (vaporPressure / (pressure - vaporPressure)) !units kg/kg
satMixingRatio = (287. / 481.) * (satVaporPressure / (pressure - satVaporPressure)) ! units kg/kg
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


!@@@@@@@@@@@@@@@@@@@@ Precipitable Water @@@@@@@@@@@@@@@@@@@@@@
!The following line calculates the average mixing ratio of a sounding using a function that takes in mixing ratio and the 1D extents of the array used.
avgMixingRatio = average(columnNumber, special, mixingRatio)
!The following nested do-loop calculates the pressure change through the entire atmosphere of 
!each respective sounding and assigns this value to the elements of pressureChange array.
DO i = 1, special, 1  !begins the outer loop
    DO n = 1, columnNumber, 1 !begins the inner loop
        pressureChange(n) = pressure(1,n) - pressure(special,n) !calculates difference in pressure relative to which value n takes.
    END DO !ends the inner do-loop
END DO !ends the inner do-loop
precipitable_water = (avgMixingRatio * (pressureChange * 100.)) / 9.81 !calculates precipitable water in units of mm.
precipitable_water = precipitable_water / 25.4 !converts from millimeters to inches.
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

!******************** Standard Deviation **********************
!The following CALLS calculate the mean and standard deviation based on the   
!elements thatpopulate their respective input arrays using a subroutine.
CALL stDev(hail_Diameter, columnNumber, stDev_hail_Diameter, mean_hail_Diameter)
CALL stDev(precipitable_water, columnNumber,stDev_precipitable_water, mean_precipitable_water)
CALL stDev(shear500, columnNumber, stDev_shear500, mean_shear500)
!**************************************************************

![][][][][][][][][][] Wet-Bulb Temperature [][][][][][][][][][]
!The following line calculates the wet-bulb temperature using comformable arrays of Temperature(°K), mixing ratio(kg/kg), and saturation mixing ratio(kg/kg).
wetbulb_Temp = K_Temperature + (2500000./1004.) * (mixingRatio - satMixingRatio)
wetbulb_Temp_Calc = wetbulb_Temp !mirrors all of the elements of the 2D wetbulb_Temp array into a new array that will be used for calculations down the line.
![][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]

!v^V^v^v^V^v^v^V^v^v^V^v^v Lapse Rate ^V^v^v^V^v^v^V^v^v^V^v^v^
!The following CALLS use a subroutine inputs heights and wet-bulb temperatures from the surface and the 500mb 
!level of their respective soundings and returns the wet-bulb temperature lapse-rate in units of -(°K/ft),
CALL lapse(wetbulb_Temp(1,1), wetbulb_Temp(5,1), height(1,1), height(5,1), lapseRate(1)) !05-15-2001
CALL lapse(wetbulb_Temp(1,2), wetbulb_Temp(5,2), height(1,2), height(5,2), lapseRate(2)) !03-23-2005
CALL lapse(wetbulb_Temp(1,3), wetbulb_Temp(5,3), height(1,3), height(5,3), lapseRate(3)) !06-06-2005
CALL lapse(wetbulb_Temp(1,4), wetbulb_Temp(5,4), height(1,4), height(5,4), lapseRate(4)) !06-01-2008
CALL lapse(wetbulb_Temp(1,5), wetbulb_Temp(5,5), height(1,5), height(5,5), lapseRate(5)) !05-10-2011
CALL lapse(wetbulb_Temp(1,6), wetbulb_Temp(5,6), height(1,6), height(5,6), lapseRate(6)) !06-05-2011
CALL lapse(wetbulb_Temp(1,7), wetbulb_Temp(5,7), height(1,7), height(5,7), lapseRate(7)) !08-01-2012
CALL lapse(wetbulb_Temp(1,8), wetbulb_Temp(5,8), height(1,8), height(5,8), lapseRate(8)) !07-16-2013
!v^V^v^v^V^v^v^V^v^v^V^v^v^V^v^v^V^v^v^V^v^v^V^v^v^V^v^v^V^v^v^


!\\//\\//\\//\\//\\//\\// Wet-Bulb Zero \\//\\//\\//\\//\\//\\//\
heightCount = 0 !Sets all elements of the height counter array to 0
!The following while-loops will continuously add their lapse-rate to the Wet-bulb temperature at the surface (while summing the height counter from zero) 
!of their respective sounding until the freezing line is crossed. When this happens, the loop will exit.
DO !Begins the do-loop relative then event on 05-15-2001.
    IF (wetbulb_Temp_Calc(1,1) < 273.15) EXIT !The if statement dictating the condition at which the loop can exit.
    wetbulb_Temp_Calc(1,1) = wetbulb_Temp_Calc(1,1) + lapseRate(1) !Since lapse rate is negative, adding it to wet-bulb temperature at the surface will
                                                                   !decrease the value of the wet-bulb temperature used here.
    heightCount(1) = heightCount(1) + 1 !A foot is added to the heightCount every time the condition to exit the loop isn't met.
END DO !ends the do loop.
wetbulb_Zero(1) = height(1,1) + heightCount(1) !adds the height above sea-level at the surface to the element of the heightCount
                                               !array relative to the event in question to approximate the wet-bulb zero height. 

DO !Begins the do-loop relative then event on 03-23-2005.
    IF (wetbulb_Temp_Calc(1,2) < 273.15) EXIT !The if statement dictating the condition at which the loop can exit.
    wetbulb_Temp_Calc(1,2) = wetbulb_Temp_Calc(1,2) + lapseRate(2) !Since lapse rate is negative, adding it to wet-bulb temperature at the surface will
                                                                   !decrease the value of the wet-bulb temperature used here.
    heightCount(2) = heightCount(2) + 1 !A foot is added to the heightCount every time the condition to exit the loop isn't met.
END DO !ends the do loop.
wetbulb_Zero(2) = height(1,2) + heightCount(2) !adds the height above sea-level at the surface to the element of the heightCount
                                               !array relative to the event in question to approximate the wet-bulb zero height. 

DO !Begins the do-loop relative then event on 06-06-2005.
    IF (wetbulb_Temp_Calc(1,3) < 273.15) EXIT !The if statement dictating the condition at which the loop can exit.
    wetbulb_Temp_Calc(1,3) = wetbulb_Temp_Calc(1,3) + lapseRate(3) !Since lapse rate is negative, adding it to wet-bulb temperature at the surface will
                                                                   !decrease the value of the wet-bulb temperature used here.
    heightCount(3) = heightCount(3) + 1 !A foot is added to the heightCount every time the condition to exit the loop isn't met.
END DO !ends the do loop.
wetbulb_Zero(3) = height(1,3) + heightCount(3) !adds the height above sea-level at the surface to the element of the heightCount
                                               !array relative to the event in question to approximate the wet-bulb zero height. 

DO !Begins the do-loop relative then event on 06-01-2008.
    IF (wetbulb_Temp_Calc(1,4) < 273.15) EXIT !The if statement dictating the condition at which the loop can exit.
    wetbulb_Temp_Calc(1,4) = wetbulb_Temp_Calc(1,4) + lapseRate(4) !Since lapse rate is negative, adding it to wet-bulb temperature at the surface will
                                                                   !decrease the value of the wet-bulb temperature used here.
    heightCount(4) = heightCount(4) + 1 !A foot is added to the heightCount every time the condition to exit the loop isn't met.
END DO !ends the do loop.
wetbulb_Zero(4) = height(1,4) + heightCount(4) !adds the height above sea-level at the surface to the element of the heightCount
                                               !array relative to the event in question to approximate the wet-bulb zero height. 

DO !Begins the do-loop relative then event on 05-10-2011.
    IF (wetbulb_Temp_Calc(1,5) < 273.15) EXIT !The if statement dictating the condition at which the loop can exit.
    wetbulb_Temp_Calc(1,5) = wetbulb_Temp_Calc(1,5) + lapseRate(5) !Since lapse rate is negative, adding it to wet-bulb temperature at the surface will
                                                                   !decrease the value of the wet-bulb temperature used here.
    heightCount(5) = heightCount(5) + 1 !A foot is added to the heightCount every time the condition to exit the loop isn't met.
END DO !ends the do loop.
wetbulb_Zero(5) = height(1,5) + heightCount(5) !adds the height above sea-level at the surface to the element of the heightCount
                                               !array relative to the event in question to approximate the wet-bulb zero height. 

DO !Begins the do-loop relative then event on 06-05-2011.
    IF (wetbulb_Temp_Calc(1,6) < 273.15) EXIT !The if statement dictating the condition at which the loop can exit.
    wetbulb_Temp_Calc(1,6) = wetbulb_Temp_Calc(1,6) + lapseRate(6) !Since lapse rate is negative, adding it to wet-bulb temperature at the surface will
                                                                   !decrease the value of the wet-bulb temperature used here.
    heightCount(6) = heightCount(6) + 1 !A foot is added to the heightCount every time the condition to exit the loop isn't met.
END DO !ends the do loop.
wetbulb_Zero(6) = height(1,6) + heightCount(6) !adds the height above sea-level at the surface to the element of the heightCount
                                               !array relative to the event in question to approximate the wet-bulb zero height. 

DO !Begins the do-loop relative then event on 08-01-2012.
    IF (wetbulb_Temp_Calc(1,7) < 273.15) EXIT !The if statement dictating the condition at which the loop can exit.
    wetbulb_Temp_Calc(1,7) = wetbulb_Temp_Calc(1,7) + lapseRate(7) !Since lapse rate is negative, adding it to wet-bulb temperature at the surface will
                                                                   !decrease the value of the wet-bulb temperature used here.
    heightCount(7) = heightCount(7) + 1 !A foot is added to the heightCount every time the condition to exit the loop isn't met.
END DO !ends the do loop.
wetbulb_Zero(7) = height(1,7) + heightCount(7) !adds the height above sea-level at the surface to the element of the heightCount
                                               !array relative to the event in question to approximate the wet-bulb zero height. 

DO !Begins the do-loop relative then event on 07-16-2013.
    IF (wetbulb_Temp_Calc(1,8) < 273.15) EXIT !The if statement dictating the condition at which the loop can exit.
    wetbulb_Temp_Calc(1,8) = wetbulb_Temp_Calc(1,8) + lapseRate(8) !Since lapse rate is negative, adding it to wet-bulb temperature at the surface will
                                                                   !decrease the value of the wet-bulb temperature used here.
    heightCount(8) = heightCount(8) + 1 !A foot is added to the heightCount every time the condition to exit the loop isn't met.
END DO !ends the do loop.
wetbulb_Zero(8) = height(1,8) + heightCount(8) !adds the height above sea-level at the surface to the element of the heightCount
                                               !array relative to the event in question to approximate the wet-bulb zero height. 
!\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//


!<><><><><><><><><><><><><> K-INDEX <><><><><><><><><><><><><><><><
CALL kIndex(temperature(3,1), temperature(5,1), dewpoint(3,1), temperature(4,1), dewpoint(4,1), K_Index(1)) !05-15-2001
CALL kIndex(temperature(3,2), temperature(5,2), dewpoint(3,2), temperature(4,2), dewpoint(4,2), K_Index(2)) !03-23-2005
CALL kIndex(temperature(3,3), temperature(5,3), dewpoint(3,3), temperature(4,3), dewpoint(4,3), K_Index(3)) !06-06-2005
CALL kIndex(temperature(3,4), temperature(5,4), dewpoint(3,4), temperature(4,4), dewpoint(4,4), K_Index(4)) !06-01-2008
CALL kIndex(temperature(3,5), temperature(5,5), dewpoint(3,5), temperature(4,5), dewpoint(4,5), K_Index(5)) !05-10-2011
CALL kIndex(temperature(3,6), temperature(5,6), dewpoint(3,6), temperature(4,6), dewpoint(4,6), K_Index(6)) !06-05-2011
CALL kIndex(temperature(3,7), temperature(5,7), dewpoint(3,7), temperature(4,7), dewpoint(4,7), K_Index(7)) !08-01-2012
CALL kIndex(temperature(3,8), temperature(5,8), dewpoint(3,8), temperature(4,8), dewpoint(4,8), K_Index(8)) !07-16-2013

!The following do-loop selects the diagnostic interpretation of the K-Index via an IF-Block.
DO i = 1, columnNumber, 1 !Begins the do loop
    IF (K_Index(i) > 35.) THEN                    !If the K-Index is greater than 35...
        K_Interpretation(i) = 'Numerous T-storms' !...then 'Numerous T-storms' is the diagnosis.
    ELSE IF (K_Index(i) >= 31.) THEN              !If the K-Index is greater than or equal to 31...
        K_Interpretation(i) = 'Scattered T-storms'!...then 'Scattered T-storms' is the diagnosis.
    ELSE IF (K_Index(i) >= 26.) THEN              !If the K-Index is greater than or equal to 26...
        K_Interpretation(i) = 'Few T-storms'      !...then 'Few T-storms' is the diagnosis.
    ELSE IF (K_Index(i) >= 21.) THEN              !If the K-Index is greater than or equal to 21...
        K_Interpretation(i) = 'Isolated T-storms' !...then 'Isolated T-storms' is the diagnosis.
    ELSE                                          !Otherwise...
        K_Interpretation(i) = 'No T-storms'       !'No storms' is the diagnosis.
    END IF
END DO
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>


!/////////////////////// Formatted Writing \\\\\\\\\\\\\\\\\\\\\\\\\
!The following formatted and unformatted write statements write a brief descriptive header as well as a data table filled with calculated values to an output file 'test.txt'.
WRITE(18,160) !Writes the Header.
160 FORMAT ('The following data table compares and contrasts calculations derived from soundings with respect to the specific dates near the time when hail fell across Roanoke County.')
WRITE(18,*) ' ' !Skips a line.
WRITE(18,*) ' ' !skips a line.

WRITE(18, 165) !draw the top of the table.
165 FORMAT (' ============================================================================================================================================================ ')
WRITE(18,175) !titles the observed events and calculated values.
175 FORMAT ('|     Date      | UTC time | Hail Diameter(in) | Precipitable Water (in) | Shear Magnitude (m/s) | Wet-Bulb Zero Height (ft) | K-Index |  K-Index Diagnosis  |')
WRITE(18,190) !more table drawing
190 FORMAT ('|===============|==========|===================|=========================|=======================|===========================|=========|=====================|')

!The following do-loop writes a table of observed and calculated values from rank-1, size-8 arrays.
DO i = 1, columnNumber, 1
    WRITE(18,200) date(i), time(i), hail_Diameter(i), precipitable_water(i), shear500(i), wetbulb_zero(i), K_Index(i), K_interpretation(i), heightCount(i)
    !********************date*************time*************hail****************pwat***************shear*****************Zw******************K**************K-int*********
    200 FORMAT ('|', 2X ,A10, 3X, '|', 3X, I4, 3X,'|', 6X, F5.2, 8X, '|', 10X, F5.3, 10X, '|', 9X, F5.2, 9X, '|', 10X, F6.1, 11X, '|', 2X, F5.2, '  |', 2X, A18, 1X, '|', 2X, I5)     
END DO !End the do loop

WRITE(18,205)!Draws the bottom of the table to the write-file
205 FORMAT (' ============================================================================================================================================================ ', /)
write(18,*) ' ' !Skips a line

!The following three formatted-write statement write the means and the standard deviations of the Hail size, PWAT and Shear Magnitude, respectly, to the write-file.
WRITE(18,210) mean_hail_Diameter, stDev_hail_Diameter !Writes a fo
210 FORMAT ('Mean Hail Diameter:       ' F5.3, ' inches', /, 'Hail Standard Deviation:  ', F5.3, ' inches', /)
    
WRITE(18,220) mean_precipitable_water, stDev_precipitable_water
220 FORMAT ('Mean PWAT:                ' F5.3, ' inches', /, 'PWAT Standard Deviation:  ', F5.3, ' inches', /)
    
WRITE(18,230) mean_shear500, stDev_shear500
230 FORMAT ('Mean Shear:               ' F5.2, ' m/s', /, 'Shear Standard Deviation: ', F5.2, ' m/s')
!\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\//////////////////////////////////


CONTAINS !Allows functions to technically appear inside the program rather than after the END PROGRAM statement
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    FUNCTION windbreaker(speciale, verso, veloce) !starts the function that breaks the wind into its components
        IMPLICIT NONE !prevents FORTRAN from treating variables that begin with certain letters from being treated as integers and stuff
        
        INTEGER :: x !the variable used to iterate the do-loop that calculates the u and v components of the wind.
        INTEGER, INTENT(IN) :: speciale !used to iterate the do-loop
        REAL, DIMENSION(speciale,16) :: windbreaker !the returned 2-dimensional array to be copied to the wind speed array
        REAL, DIMENSION(speciale), INTENT(IN) :: verso !wind direction dummy
        REAL, DIMENSION(speciale,16), INTENT(IN) :: veloce !wind speed dummy
        REAL, DIMENSION(speciale,16) :: u_AND_v !2-dimensional array for the u and v-component wind dummy
        
            DO x = 1, speciale, 1 !The do-loop converts the input arrays and outputs wind components
                !The following two lines break the wind into its components
                u_AND_v(x,1) = -(abs(veloce(x,1))*sin(verso(x)))!the calculated 05-15-2001 u component set into the first column of the u_AND_v array
                u_AND_v(x,2) = -(abs(veloce(x,2))*cos(verso(x)))!the calculated 05-15-2001 v component set into the second column of the u_AND_v array
                u_AND_v(x,3) = -(abs(veloce(x,3))*sin(verso(x)))!the calculated u component set into the third column of the u_AND_v array
                u_AND_v(x,4) = -(abs(veloce(x,4))*cos(verso(x)))!the calculated v component set into the forth column of the u_AND_v array
                u_AND_v(x,5) = -(abs(veloce(x,5))*sin(verso(x)))!the calculated u component set into the fifth column of the u_AND_v array
                u_AND_v(x,6) = -(abs(veloce(x,6))*cos(verso(x)))!the calculated v component set into the sixth column of the u_AND_v array
                u_AND_v(x,7) = -(abs(veloce(x,7))*sin(verso(x)))!the calculated u component set into the seventh column of the u_AND_v array
                u_AND_v(x,8) = -(abs(veloce(x,8))*cos(verso(x)))!the calculated v component set into the eighth column of the u_AND_v array
                u_AND_v(x,9) = -(abs(veloce(x,9))*sin(verso(x)))!the calculated u component set into the ninth column of the u_AND_v array
                u_AND_v(x,10) = -(abs(veloce(x,10))*cos(verso(x)))!the calculated v component set into the tenth column of the u_AND_v array
                u_AND_v(x,11) = -(abs(veloce(x,11))*sin(verso(x)))!the calculated u component set into the eleventh column of the u_AND_v array
                u_AND_v(x,12) = -(abs(veloce(x,12))*cos(verso(x)))!the calculated v component set into the twelth column of the u_AND_v array
                u_AND_v(x,13) = -(abs(veloce(x,13))*sin(verso(x)))!the calculated u component set into the thirteenth column of the u_AND_v array
                u_AND_v(x,14) = -(abs(veloce(x,14))*cos(verso(x)))!the calculated v component set into the fourteenth column of the u_AND_v array
                u_AND_v(x,15) = -(abs(veloce(x,15))*sin(verso(x)))!the calculated u component set into the fifteenth column of the u_AND_v array
                u_AND_v(x,16) = -(abs(veloce(x,16))*cos(verso(x)))!the calculated v component set into the sixteenth column of the u_AND_v array
           END DO   !ends the do loop
            
        windbreaker = u_AND_v !sets the windbreaker dummy to returned with the values of the u_AND_v array.
       
    END FUNCTION windbreaker !ends the function
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
    
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    !The following function takes in two arrays, calculates relative humidity using them, and returns an array filled with its value
    FUNCTION humidity(grado, vapore, saturato)
        IMPLICIT NONE !prevents FORTRAN from treating variables that begin with certain letters from being treated as integers and stuff 
        
        INTEGER, INTENT(IN) :: grado !the input row extent
        REAL, DIMENSION(grado,8) :: humidity !the relative humidity value to be returned
        REAL, DIMENSION(grado,8), INTENT(IN) :: vapore !the dummy array for the input vapor pressure array
        REAL, DIMENSION(grado,8), INTENT(IN) :: saturato !the dummy array for the input saturated vapor pressure array
        REAL, DIMENSION(grado,8) :: umidita !Probably superfluous. Used to link the value calculated by the subroutine to the returned value.
        
        umidita = vapore / saturato !the calculation for the relative humidity calculated using conformable arrays
        humidity = umidita !Teaching italian through code (technically, it's "umidità," but that accented 'a' probably isn't accepted by FORTRAN)
        
    END FUNCTION humidity !ends the function that calculates relative humidity
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    
!#######################################################
    FUNCTION average(column, row, schiera) !this function takes in a sum and divisor and computes an average
        IMPLICIT NONE !prevents FORTRAN from treating variables that begin with certain letters from being treated as integers and stuff
        INTEGER :: o, j !The iterative variables used in the nested do-loop
        INTEGER, INTENT(IN) :: column !the input value for the number of columns to iterate with 
        INTEGER, INTENT(IN) :: row !the input divisor dummy (also used to interate an outer loop)
        REAL, DIMENSION(column) :: average !the returned average
        REAL, DIMENSION(row,column), INTENT(IN) :: schiera !the input dummy array for whatever input array that must have its elements averaged
        
        DO o = 1, row, 1 !The outer-loop
            DO j = 1, column, 1 !The inner loop
                average(j) = sum(schiera(:,j)) !computes the sum of the values within a certain column. 
            END DO !Ends the inner loop
        END DO !Ends the outer loop
        average = average / row !the calculation for the average obtained by dividing the sums by the number of elements used, respectively. 
    
    END FUNCTION average !ends the function
!#######################################################

END PROGRAM theEnd !Ends the program
    
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
    !The following subroutine calculates shear magnitude.
    SUBROUTINE shear(u1000, u500, v1000, v500, tosatura500)
        IMPLICIT NONE !prevents FORTRAN from treating variables that begin with certain letters from being treated as integers and stuff.   
        
        REAL, INTENT(OUT) :: tosatura500 !the returned shear magnitude. Surface to 500mb shear
        REAL, INTENT(IN) :: u1000, u500 !the input u-component wind dummies. At the surface and at 500mb, respectively.
        REAL, INTENT(IN) :: v1000, v500 !the input v-component wind dummies. At the surface and at 500mb, respectively.
        
        !The following line calculates an approximation of bulk shear.
        tosatura500 = sqrt(((u500-u1000)**2) + ((v500-v1000)**2)) 
        
    END SUBROUTINE shear !ends the subroutine that calculates shear.
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      
!*************************************************************************
    !The following subroutine calculates and returns standard deviation and mean of an array with a set of values.
    SUBROUTINE stDev(set, divisor, deviated, mean)
        IMPLICIT NONE !prevents FORTRAN from treating variables that begin with certain letters from being treated as integers and stuff.
        
        INTEGER, INTENT(IN) :: divisor !Used in the formula for the standard deviation and is used as the divisor for the mean.
        REAL, DIMENSION(divisor), INTENT(IN) :: set !the input 1D dummy array.
        REAL, INTENT(OUT) :: deviated !The dummy variable for the returned standard deviation.
        REAL, INTENT(OUT) :: mean !The dummy variable for the returned mean. 
        
       deviated = sqrt((divisor * sum(set**2) - sum(set)**2) / (divisor * (divisor - 1))) !The calculation for standard deviation.
       mean = sum(set) / divisor !the calculation for the mean.

    END SUBROUTINE stDev !Ends the subroutine that calculates standard deviation and mean
!*************************************************************************
    
!v^V^v^v^V^v^v^V^v^v^V^v^v^V^v^V^v^v^V^v^v^V^v^v^V^v^v^V^v^v^V^v^v^V^v^
    !The following subroutine calculates the lapse rate
    SUBROUTINE lapse(calore1000, calore500, altezza1000, altezza500, gradiente500)
        IMPLICIT NONE !prevents FORTRAN from treating variables that begin with certain letters from being treated as integers and stuff.
        REAL, INTENT(IN) :: calore1000, calore500 !the input temperature dummies. At the surface and at the 500mb level.
        INTEGER, INTENT(IN) :: altezza1000, altezza500 !the input height dummies. At the surface and at the 500mb level.
        REAL, INTENT(OUT) :: gradiente500 !the returned lapse-rate. Surface to 500mb lapse-rate
        
        !The following line calculate the value for lapse rate, from the surface to the 500mb level
        gradiente500 = ((calore500 - calore1000) / ((altezza500 - altezza1000))) 
    END SUBROUTINE lapse !Ends the subroutine that calculates lapse-rate.
!v^V^v^v^V^v^v^V^v^v^V^v^v^V^v^v^V^v^v^V^v^v^V^v^v^V^v^v^V^v^v^V^v^v^V^v^

!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>   
    !The following subroutine calculates K_Index.
    SUBROUTINE kIndex(temp850, temp500, dew850, temp700, dew700, dex)
        IMPLICIT NONE 
        
        REAL, INTENT(OUT) :: dex !the returned 
        REAL, INTENT(IN) :: temp850, temp500, temp700 !the input   dummies. 
        REAL, INTENT(IN) :: dew850, dew700 !the input  dummies. 
        
        !The following line calculates K-Index.
        dex = (temp850 - temp500) + dew850 - (temp700 - dew700) 
        
    END SUBROUTINE kIndex !ends the subroutine that calculates shear.
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

    
!!!!!!!!!!!!!!!!!!!!!!!!! UNUSED STUFF !!!!!!!!!!!!!!!!!!!!!!!!!!!!
!=+=+=+=+=+=+=+=+=+=+=+=+ Lifted Index +=+=+=+=+=+=+=+=+=+=+=+=+=
!liftedIndex(1) = K_Temperature(5,1) - (K_Temperature(1,1) * (pressure(5,1) / pressure(1,1)) ** .2858)
!=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!