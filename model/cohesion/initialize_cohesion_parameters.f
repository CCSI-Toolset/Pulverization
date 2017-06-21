!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: INITIALIZE_COHESION_PARAMETERS                         C
!  Purpose: This module gives initial values to all                    C
!           cohesion parameters                                        C
!                                                                      C
!   Author: Mike Weber                              Date: 9/30/04      C
!   Reviewer:                                       Date:              C
!                                                                      C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C


      SUBROUTINE INITIALIZE_COHESION_PARAMETERS

!-----MODULES USED
      USE discretelement

!-----LOCAL DUMMY VARIABLES
      INTEGER i,j

      IF(COHESION_DEBUG.gt.0)THEN
        PRINT *, '**START INITIALIZE COHESION PARAMETERS'
      END IF

    
!-----INITIALIZATIONS
      DO i=1, NPARTICLES
         IS_LINKED(i)=0
         LINKS(i,1)=0
         DO j=2,MAXNEIGHBORS
           LINKS(i,j)=-1
         END DO
      END DO

     

      END SUBROUTINE INITIALIZE_COHESION_PARAMETERS
