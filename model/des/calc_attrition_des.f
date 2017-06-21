!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                         C  
!     Module name: CALC_ATTRITION_DES                                     C
!
!     Purpose: Called in model/des/des_time_march to do DES attrition calcs
!
!                                                                         C
!     Author: David DeCroix, Wesley Xu                   Date: 10-Apr-12  C
!     Reviewer:                                                           C
!     Comments: This subroutine implements a particle fracture or attrition
!               model based on the following papers:
!     Modified by Wesley Xu: calculate velocity based on impact force
!
!     Revised: Wesley Xu                                 Date: 30-July-12
!     Comments: Abrasion induced attrition mechanism is included. Impact 
!               induced attrition is also modified to correct some errors.
!                                                                         C
!
!     Revised: Wesley Xu				 Date: 5-Nov-12   C
!     Comments: Model changed back to velocity based. 		

!     Revised: Wesley Xu                          Date: 050114
!     Comments: Remove impact attrition and modify abrasive attrition for glass				
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C

      SUBROUTINE calc_attrition_des(LL,force,slide, overlap, desradius)

      USE run
      USE param
      USE param1
      USE fldvar
      USE physprop
      USE discretelement
      USE geometry
      USE compar
      USE constant
      IMPLICIT NONE

!------------------------------------------------------------------------------
!------------------  Local Variables ------------------------------------------
!------------------------------------------------------------------------------
      INTEGER I
      DOUBLE PRECISION FORCE(DIMN)
      DOUBLE PRECISION SLIDE(DIMN)
      DOUBLE PRECISION OVERLAP
      double precision, parameter :: ro_pwall = 3.d0


      INTEGER LL
!

      DOUBLE PRECISION DES_FN
      DOUBLE PRECISION DES_ST      
      DOUBLE PRECISION VLOSS_ABR !Volume loss due to abrasive attrition
      DOUBLE PRECISION DESRADIUS
      DOUBLE PRECISION DESRADIUS_TMP


      IF (RO_SOL(LL).LT.RO_PWALL.AND.DESRADIUS.GT.DESATTRITIONTHRESH) then

!Calculate the volume loss due to abrasive attrition
         VLOSS_ABR = ZERO
         DES_FN = ZERO
         DES_ST = ZERO

         DO I = 1, DIMN
            DES_FN = DES_FN+FORCE(I)*FORCE(I)
            DES_ST = DES_ST+SLIDE(I)*SLIDE(I)*OVERLAP*OVERLAP
         ENDDO

         VLOSS_ABR = ABRALPHA*ABS(SQRT(DES_FN*DES_ST))/FRACTUREHARDNESS


! FOR DEBUG ONLY - WESLEY
!         IF (VLOSS_ABR.GT.ZERO) WRITE(*,*) FORCE(1),FORCE(2),OVERLAP, VLOSS_ABR

! USE DESALPHA AS A LIMIT VALUE FOR EACH STEP OF ATTRITION INDUCED MASS LOSS
         IF (VLOSS_ABR.GE.DESALPHA) VLOSS_ABR=DESALPHA

           DESRADIUS_TMP = DESRADIUS*(1-VLOSS_ABR)**(1.D0/3.D0)


! USE DESATTRITION AS A LIMIT VALUE FOR THE ULTIMATE PARTICLE SIZE
         IF (DESRADIUS_TMP.LT.DESATTRITIONTHRESH) DESRADIUS_TMP = DESATTRITIONTHRESH

! UPDATE DESRADIUS
          DESRADIUS = DESRADIUS_TMP 

      
      ENDIF

      RETURN   
            
      END SUBROUTINE calc_attrition_des
      
