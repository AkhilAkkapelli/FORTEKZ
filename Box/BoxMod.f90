MODULE BoxMod
USE ShapeMod,  ONLY : Shape
USE CellMod,   ONLY : Cell
USE ArrowMod,  ONLY : Arrow
IMPLICIT NONE


TYPE Box

PRIVATE

  INTEGER,      ALLOCATABLE :: ID  
  CHARACTER(:), ALLOCATABLE :: BoundaryColour
  CHARACTER(:), ALLOCATABLE :: InteriorColour
  
  REAL,         ALLOCATABLE :: XLocus, YLocus
  REAL,         ALLOCATABLE :: XLimit, YLimit
  
  TYPE(Cell),   ALLOCATABLE :: BoxCell

CONTAINS

  PROCEDURE, PASS(self), PUBLIC  :: IsSet       => IsSetBox   
  PROCEDURE, PASS(self), PRIVATE ::                GetCheckLayerBox  
  PROCEDURE, PASS(self), PRIVATE ::                GetCheckBox 
  GENERIC,               PUBLIC  :: GetCheck    => GetCheckLayerBox, GetCheckBox

  PROCEDURE, PASS(self), PUBLIC  :: Set         => SetBox   
  PROCEDURE, PASS(self), PUBLIC  :: Refine      => RefineBox

  PROCEDURE, PASS(self), PUBLIC  :: GetID       => GetIDBox
  PROCEDURE, PASS(self), PUBLIC  :: GetXLocus   => GetXLocusBox
  PROCEDURE, PASS(self), PUBLIC  :: GetYLocus   => GetYLocusBox
  PROCEDURE, PASS(self), PUBLIC  :: GetXLim     => GetXLimitBox
  PROCEDURE, PASS(self), PUBLIC  :: GetYLim     => GetYLimitBox  
  PROCEDURE, PASS(self), PUBLIC  :: GetCellSize => GetCellSizeBox   
  PROCEDURE, PASS(self), PUBLIC  :: GetBoundCol => GetBoundaryColourBox
  PROCEDURE, PASS(self), PUBLIC  :: GetIntCol   => GetInteriorColourBox
  PROCEDURE, PASS(self), PUBLIC  :: GetCell     => GetCellBox     
  
!  PROCEDURE, PASS(self), PRIVATE ::                  BoxEqualsBox
!  PROCEDURE, PASS(self), PRIVATE ::                  MatrixBoxEqualsBox  
!  GENERIC,               PUBLIC  :: ASSIGNMENT(=) => BoxEqualsBox, MatrixBoxEqualsBox 
  
  PROCEDURE, PASS(self), PUBLIC  :: Display     => DisplayBox

END TYPE Box


PRIVATE :: Shape, Cell, Arrow

PRIVATE :: IsSetBox, GetCheckLayerBox, GetCheckBox, SetBox, RefineBox, GetIDBox, GetXLocusBox,&
            GetYLocusBox, GetXLimitBox, GetYLimitBox, GetCellSizeBox, GetBoundaryColourBox, GetInteriorColourBox, &
             GetCellBox, DisplayBox

PUBLIC  :: Box


CONTAINS


LOGICAL FUNCTION IsSetBox(self)

CLASS(Box), INTENT(IN) :: self


IsSetBox = ALLOCATED(self%ID) .AND. ALLOCATED(self%BoundaryColour) .AND. ALLOCATED(self%InteriorColour)&
            .AND. ALLOCATED(self%XLimit) .AND. ALLOCATED(self%YLimit) .AND. ALLOCATED(self%XLocus) .AND. ALLOCATED(self%YLocus) &
              .AND. self%BoxCell%IsSet() 

END FUNCTION IsSetBox

SUBROUTINE GetCheckLayerBox(self, Layer)

CLASS(Box), INTENT(IN) :: self
INTEGER, INTENT(IN) :: Layer


IF(Layer < 1 .OR. Layer > self%GetCellSize())  STOP "Layer is out of Cell Size Range"

END SUBROUTINE GetCheckLayerBox

SUBROUTINE GetCheckBox(self)

CLASS(Box), INTENT(IN) :: self


IF(.NOT. self%IsSet())  STOP "Box not Set"

END SUBROUTINE GetCheckBox


SUBROUTINE SetBox(self, ID, XLocus, YLocus, XLimit, YLimit, BoundaryColour, InteriorColour, BoxCell)

CLASS(Box),                 INTENT(OUT) :: self
INTEGER,                    INTENT(IN)  :: ID 
REAL,                       INTENT(IN)  :: XLocus, YLocus
REAL,                       INTENT(IN)  :: XLimit, YLimit
CHARACTER(len=*), OPTIONAL, INTENT(IN)  :: BoundaryColour
CHARACTER(len=*), OPTIONAL, INTENT(IN)  :: InteriorColour
TYPE(Cell),       OPTIONAL, INTENT(IN)  :: BoxCell

TYPE(Cell)                              :: DefaultCell


self%ID = ID

self%XLocus = XLocus
self%YLocus = YLocus
self%XLimit = XLimit
self%YLimit = YLimit

IF(PRESENT(BoundaryColour)) THEN
    self%BoundaryColour = BoundaryColour
  ELSE
    self%BoundaryColour = "none"
END IF  
IF(PRESENT(InteriorColour)) THEN
  self%InteriorColour = InteriorColour
ELSE
  self%InteriorColour = "none"
END IF

IF(PRESENT(BoxCell)) THEN 
  self%BoxCell = BoxCell
ELSE
  ALLOCATE(self%BoxCell)
  CALL DefaultCell%Set() 
  self%BoxCell = DefaultCell
END IF

END SUBROUTINE SetBox

SUBROUTINE RefineBox(self, ID, XLocus, YLocus, XLimit, YLimit, BoundaryColour, InteriorColour, BoxCell, BoxCellLayer)

CLASS(Box),       TARGET,           INTENT(INOUT) :: self
INTEGER,                  OPTIONAL, INTENT(IN)    :: ID 
REAL,                     OPTIONAL, INTENT(IN)    :: XLocus, YLocus
REAL,                     OPTIONAL, INTENT(IN)    :: XLimit, YLimit
CHARACTER(len=*),         OPTIONAL, INTENT(IN)    :: BoundaryColour
CHARACTER(len=*),         OPTIONAL, INTENT(IN)    :: InteriorColour
TYPE(Cell),       TARGET, OPTIONAL, INTENT(IN)    :: BoxCell
INTEGER,                  OPTIONAL, INTENT(IN)    :: BoxCellLayer

INTEGER                                           :: i
TYPE(Cell),       POINTER                         :: Iter
TYPE(Cell),       TARGET                         :: DefaultCell


IF(PRESENT(ID))     self%ID = ID
IF(PRESENT(XLocus)) self%XLocus = XLocus
IF(PRESENT(YLocus)) self%YLocus = YLocus
IF(PRESENT(XLimit)) self%XLimit = XLimit
IF(PRESENT(YLimit)) self%YLimit = YLimit
IF(PRESENT(BoundaryColour)) self%BoundaryColour = BoundaryColour
IF(PRESENT(InteriorColour)) self%InteriorColour = InteriorColour
IF(PRESENT(BoxCellLayer)) THEN
  IF(BoxCellLayer == self%GetCellSize() + 1 ) THEN
    Iter => self%BoxCell
    DO i = 1, BoxCellLayer -2
      Iter => Iter%GetNext() 
    END DO 
    IF(PRESENT(BoxCell)) THEN
      CALL Iter%Refine(NextCell = BoxCell)
    ELSE
      CALL DefaultCell%Set() 
      CALL Iter%Refine(NextCell = DefaultCell)
    END IF  
  ELSE
    CALL self%GetCheck(BoxCellLayer)
    Iter => self%BoxCell
    DO i = 1, BoxCellLayer -1
      Iter => Iter%GetNext() 
    END DO 
    IF(PRESENT(BoxCell)) THEN
      Iter = BoxCell
    ELSE
      CALL DefaultCell%Set() 
      Iter = DefaultCell
    END IF
  END IF
ELSE
  IF(PRESENT(BoxCell))  self%BoxCell = BoxCell
END IF

END SUBROUTINE RefineBox


INTEGER FUNCTION GetIDBox(self)

CLASS(Box), INTENT(IN) :: self


GetIDBox = self%ID

END FUNCTION GetIDBox

REAL FUNCTION GetXLocusBox(self)

CLASS(Box), INTENT(IN) :: self


GetXLocusBox = self%XLocus

END FUNCTION GetXLocusBox

REAL FUNCTION GetYLocusBox(self)

CLASS(Box), INTENT(IN) :: self


GetYLocusBox = self%YLocus

END FUNCTION GetYLocusBox

REAL FUNCTION GetXLimitBox(self)

CLASS(Box), INTENT(IN) :: self


GetXLimitBox = self%XLimit

END FUNCTION GetXLimitBox

REAL FUNCTION GetYLimitBox(self)

CLASS(Box), INTENT(IN) :: self


GetYLimitBox = self%YLimit

END FUNCTION GetYLimitBox

INTEGER FUNCTION GetCellSizeBox(self)

CLASS(Box),  TARGET, INTENT(IN)  :: self

TYPE(Cell),  POINTER             :: Iter


GetCellSizeBox = 0
Iter => self%BoxCell
DO
  GetCellSizeBox = GetCellSizeBox + 1
  IF(.NOT. Iter%IsNext()) EXIT     
  Iter => Iter%GetNext()
END DO

END FUNCTION GetCellSizeBox

FUNCTION GetCellBox(self, CellIndex)

CLASS(Box),  TARGET,   INTENT(IN) :: self
INTEGER,     OPTIONAL, INTENT(IN) :: CellIndex

TYPE(Cell),  POINTER              :: GetCellBox

INTEGER                           :: i
TYPE(Cell),  POINTER              :: Iter


IF(PRESENT(CellIndex)) THEN
  CALL self%GetCheck(CellIndex)  
  Iter => self%BoxCell
  DO i = 1, CellIndex  -1
    Iter => Iter%GetNext()
  END DO
  GetCellBox => Iter
ELSE
  GetCellBox => self%BoxCell
END IF

END FUNCTION GetCellBox

FUNCTION GetBoundaryColourBox(self)

CLASS(Box), INTENT(IN)    :: self

CHARACTER(:), ALLOCATABLE :: GetBoundaryColourBox


GetBoundaryColourBox = self%BoundaryColour

END FUNCTION GetBoundaryColourBox

FUNCTION GetInteriorColourBox(self)

CLASS(Box), INTENT(IN)    :: self

CHARACTER(:), ALLOCATABLE :: GetInteriorColourBox


GetInteriorColourBox = self%InteriorColour

END FUNCTION GetInteriorColourBox


!SUBROUTINE BoxEqualsBox(BoxValue, self)  

!TYPE(Box),  INTENT(INOUT) :: BoxValue
!CLASS(Box), INTENT(IN)    :: self


!CALL self%GetCheck()
!BoxValue%BoundaryColour = self%BoundaryColour
!BoxValue%InteriorColour = self%InteriorColour

!IF(ALLOCATED(BoxValue%BoxCell)) DEALLOCATE(BoxValue%BoxCell)
!ALLOCATE(BoxValue%BoxCell(1))
!BoxValue%BoxCell(1)  = self%BoxCell(1)

!END SUBROUTINE BoxEqualsBox

!SUBROUTINE MatrixBoxEqualsBox(MatrixBoxValue, self)  

!TYPE(Box),  INTENT(INOUT) :: MatrixBoxValue(:,:)
!CLASS(Box), INTENT(IN)  :: self

!INTEGER                 :: i, j 


!DO i = 1, size(MatrixBoxValue, 1)
!  DO j = 1, size(MatrixBoxValue, 2)
!    MatrixBoxValue(i, j) = self
!  END DO
!END DO  

!END SUBROUTINE MatrixBoxEqualsBox


SUBROUTINE DisplayBox(self)

CLASS(Box),  TARGET, INTENT(IN) :: self

INTEGER                         :: i
TYPE(Cell),  POINTER            :: Iter


CALL self%GetCheck()
print*, self%ID
IF(ALLOCATED(self%BoundaryColour)) print*, self%BoundaryColour
IF(ALLOCATED(self%InteriorColour)) print*, self%InteriorColour
IF(ALLOCATED(self%XLocus) .AND. ALLOCATED(self%YLocus)) print*, self%XLocus, self%YLocus
IF(ALLOCATED(self%XLimit) .AND. ALLOCATED(self%YLimit)) print*, self%XLimit, self%YLimit
Iter => self%BoxCell
CALL Iter%Display()
DO i = 1, self%GetCellSize() -1
  Iter => Iter%GetNext()
  CALL Iter%Display()
END DO

END SUBROUTINE DisplayBox


END MODULE BoxMod
