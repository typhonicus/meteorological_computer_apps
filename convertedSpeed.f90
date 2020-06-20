FUNCTION convertedSpeed(special, veloce)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: special
    REAL, DIMENSION(special) :: convertedSpeed  
    REAL, DIMENSION(special), INTENT(IN) :: veloce
    convertedSpeed = veloce * 0.514444444
   
END FUNCTION convertedSpeed