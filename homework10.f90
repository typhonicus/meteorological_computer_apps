PROGRAM homework10
    IMPLICIT NONE
    
        !Data Dictionary
            REAL, DIMENSION(59) :: pressure
            INTEGER, DIMENSION(59) :: height
            REAL, DIMENSION(59) :: temperature
            REAL, DIMENSION(59) :: dewPoint
            REAL, DIMENSION(59) :: humidity
            REAL, DIMENSION(59) :: mixingRatio
            INTEGER, DIMENSION(59) :: direction
            REAL, DIMENSION(59) :: speed
            REAL, DIMENSION(59) :: po_Temp
            REAL, DIMENSION(59) :: e_po_Temp
            REAL, DIMENSION(59) :: v_po_Temp
REAL :: convertedSpeed
            REAL, DIMENSION(59) :: u_wind
            REAL, DIMENSION(59) :: v_wind
            
            REAL :: windSum
            
            INTEGER :: skipper
            INTEGER :: i
            INTEGER :: ios
            INTEGER :: divisor
            REAL :: average

            
            
OPEN(UNIT=10, FILE='C:\Users\Typhonicus\Desktop\Fortran\Homework Projects\dieci\dieci.txt', STATUS='OLD', ACTION='READ', IOSTAT=ios)
OPEN(UNIT=20, FILE='C:\Users\Typhonicus\Desktop\Fortran\Homework Projects\dieci\test.txt', STATUS='REPLACE', ACTION='WRITE', IOSTAT=ios)

    READ(10,100) skipper
    100 FORMAT (I1,//////)
        DO i = 1, 59, 1
            READ(10,200) pressure(i), height(i), temperature(i), dewPoint(i), humidity(i), mixingRatio(i), direction(i), speed(i), po_Temp(i), e_po_Temp(i), v_po_Temp(i)
            200 FORMAT (1X, F6.1, 2X, I5, 2X F5.1, 2X, F5.1, 4X, F4.0, 2X, F5.2, 3X, I3, 5X, F3.0, 1X, F5.1, 2X, F5.1, 2X, F5.1)   
            !*************Pressure**Height***Temp******dewP******Humid*****mix******drctn****spd******PT*********EPT******VPT
        END DO

       
        
    DO i = 1, 59, 1
            WRITE(20,250) pressure(i), height(i), temperature(i), dewPoint(i), humidity(i), mixingRatio(i), direction(i), speed(i), po_Temp(i), e_po_Temp(i), v_po_Temp(i) 
            250 FORMAT (1X, F6.1, 2X, I5, 2X F5.1, 2X, F5.1, 4X, F4.0, 2X, F5.2, 3X, I3, 5X, F5.2, 2X, F5.1, 2X, F5.1, 2X, F5.1)            
            !*************Pressure**Height***Temp******dewP******Humid*****mix******drctn****spd********PT*********EPT******VPT
    END DO
    

windSum = SUM(speed)
divisor = SIZE(pressure)

speed = convertedSpeed(divisor, speed)

WRITE(20,*) divisor
WRITE(20,*) windSum

WRITE(20,*) ' '

DO i = 1, 59, 1
    WRITE(20,250) pressure(i), height(i), temperature(i), dewPoint(i), humidity(i), mixingRatio(i), direction(i), speed(i), po_Temp(i), e_po_Temp(i), v_po_Temp(i)
END DO
!############################################################

!#############################################################

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$    
  !  REAL FUNCTION average
  !      IMPLICIT NONE
  !      REAL, INTENT(IN) ::
  !      INTEGER, INTENT(IN) :: divisor

   !     average =
   ! END FUNCTION average
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

END PROGRAM homework10
