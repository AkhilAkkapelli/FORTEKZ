MODULE CellMod
USE ShapeMod,  ONLY : Shape, Rectangle
IMPLICIT NONE


TYPE Cell

PRIVATE

!  INTEGER,      ALLOCATABLE :: ID
  CLASS(Shape), ALLOCATABLE :: CellShape
  CHARACTER(:), ALLOCATABLE :: LatexContent
  REAL,         ALLOCATABLE :: XShift, YShift
  INTEGER,      ALLOCATABLE :: Alignment
  TYPE(Cell),   ALLOCATABLE :: NextCell

CONTAINS

  PROCEDURE, PASS(self), PUBLIC  :: IsSet       => IsSetCell
  PROCEDURE, PASS(self), PUBLIC  :: IsNext      => IsNextCell   
  PROCEDURE, NOPASS,     PRIVATE ::                GetCheckAlignmentCell   
  PROCEDURE, PASS(self), PRIVATE ::                GetCheckCell
  GENERIC,               PUBLIC  :: GetCheck    => GetCheckAlignmentCell, GetCheckCell

  PROCEDURE, PASS(self), PUBLIC  :: Set         => SetCell   
  PROCEDURE, PASS(self), PUBLIC  :: Refine      => RefineCell
  
!  PROCEDURE, PASS(self), PUBLIC  :: GetID       => GetIDCell  
  PROCEDURE, PASS(self), PUBLIC  :: GetShape    => GetShapeCell 
  PROCEDURE, PASS(self), PUBLIC  :: GetContent  => GetContentCell
  PROCEDURE, PASS(self), PUBLIC  :: GetXShift   => GetXShiftCell  
  PROCEDURE, PASS(self), PUBLIC  :: GetYShift   => GetYShiftCell 
  PROCEDURE, PASS(self), PUBLIC  :: GetAlignStr => GetAlignmentStringCell
  PROCEDURE, PASS(self), PUBLIC  :: GetNext     => GetNextCell  
 
!  PROCEDURE, PASS(self), PRIVATE ::                  CellEqualsCell
!  GENERIC,               PUBLIC  :: ASSIGNMENT(=) => CellEqualsCell  
  
  PROCEDURE, PASS(self), PUBLIC  :: Display    => DisplayCell

END TYPE Cell


PRIVATE :: Shape, Rectangle

PRIVATE :: IsSetCell, GetCheckAlignmentCell, GetCheckCell, SetCell, RefineCell, GetShapeCell, GetContentCell,&
            GetXShiftCell, GetYShiftCell, GetAlignmentStringCell, GetNextCell, DisplayCell

PUBLIC  :: Cell


CONTAINS


LOGICAL FUNCTION IsSetCell(self)

CLASS(Cell), INTENT(IN) :: self


IsSetCell = self%CellShape%IsSet() .AND. ALLOCATED(self%LatexContent) .AND. &
             ALLOCATED(self%XShift) .AND. ALLOCATED(self%YShift) .AND. ALLOCATED(self%Alignment)

END FUNCTION IsSetCell

LOGICAL FUNCTION IsNextCell(self)

CLASS(Cell), INTENT(IN) :: self


IsNextCell = ALLOCATED(self%NextCell)

END FUNCTION IsNextCell

SUBROUTINE GetCheckAlignmentCell(Align)

INTEGER, INTENT(IN) :: Align


IF(.NOT. ANY((/-1, 0, 1/) == Align))  STOP "Alignment Value Should be in Set (-1, 0, 1)"

END SUBROUTINE GetCheckAlignmentCell

SUBROUTINE GetCheckCell(self)

CLASS(Cell), INTENT(IN) :: self


IF(.NOT. self%IsSet())  STOP "Cell not Set"

END SUBROUTINE GetCheckCell


SUBROUTINE SetCell(self, CellShape, LatexContent, XShift, YShift, Alignment, NextCell)

CLASS(Cell),                INTENT(OUT) :: self
CLASS(Shape),     OPTIONAL, INTENT(IN)  :: CellShape
CHARACTER(len=*), OPTIONAL, INTENT(IN)  :: LatexContent
REAL,             OPTIONAL, INTENT(IN)  :: XShift
REAL,             OPTIONAL, INTENT(IN)  :: YShift 
INTEGER,          OPTIONAL, INTENT(IN)  :: Alignment
TYPE(Cell),       OPTIONAL, INTENT(IN)  :: NextCell

TYPE(Rectangle)                  :: DefaultRectangle


IF(PRESENT(CellShape)) THEN
  ALLOCATE(self%CellShape, SOURCE=CellShape)
ELSE
  CALL DefaultRectangle%Set(Width=.9, Height = .9, Radius = .1)
  ALLOCATE(self%CellShape, SOURCE=DefaultRectangle)
END IF  
IF(PRESENT(LatexContent)) THEN
  self%LatexContent = LatexContent
ELSE
  self%LatexContent = ""
END IF
IF(PRESENT(XShift)) THEN
  self%XShift = XShift
ELSE
  self%XShift = 0.
END IF
IF(PRESENT(YShift)) THEN
  self%YShift = YShift
ELSE
  self%YShift = 0.
END IF
IF(PRESENT(Alignment)) THEN
  CALL self%GetCheck(Alignment)
  self%Alignment = Alignment
ELSE
  self%Alignment = 0
END IF

END SUBROUTINE SetCell

SUBROUTINE RefineCell(self, CellShape, LatexContent, XShift, YShift, Alignment, NextCell)

CLASS(Cell),                 INTENT(INOUT) :: self
CLASS(Shape),      OPTIONAL, INTENT(IN)    :: CellShape
CHARACTER(len=*),  OPTIONAL, INTENT(IN)    :: LatexContent
REAL,              OPTIONAL, INTENT(IN)    :: XShift
REAL,              OPTIONAL, INTENT(IN)    :: YShift
INTEGER,           OPTIONAL, INTENT(IN)    :: Alignment
TYPE(Cell),        OPTIONAL, INTENT(IN)    :: NextCell


IF(PRESENT(CellShape)) THEN
  IF(ALLOCATED(self%CellShape)) DEALLOCATE(self%CellShape)
  ALLOCATE(self%CellShape, SOURCE=CellShape)
END IF  
IF(PRESENT(LatexContent)) THEN
  self%LatexContent = LatexContent
END IF
IF(PRESENT(XShift)) THEN
  self%XShift = XShift
END IF
IF(PRESENT(YShift)) THEN
  self%YShift = YShift
END IF
IF(PRESENT(Alignment)) THEN
  CALL self%GetCheck(Alignment)
  self%Alignment = Alignment
END IF
IF(PRESENT(NextCell)) THEN
CALL NextCell%GetCheck()
self%NextCell = NextCell
END IF

END SUBROUTINE RefineCell


FUNCTION GetShapeCell(self)

CLASS(Cell),  INTENT(IN)  :: self

CLASS(Shape), ALLOCATABLE :: GetShapeCell


CALL self%GetCheck()
ALLOCATE(GetShapeCell, SOURCE=self%CellShape)

END FUNCTION GetShapeCell

FUNCTION GetContentCell(self)

CLASS(Cell),  INTENT(IN)  :: self

CHARACTER(:), ALLOCATABLE :: GetContentCell


CALL self%GetCheck()
GetContentCell = self%LatexContent

END FUNCTION GetContentCell

REAL FUNCTION GetXShiftCell(self)

CLASS(Cell),  INTENT(IN)  :: self


!CALL self%GetCheck()
GetXShiftCell = self%XShift

END FUNCTION GetXShiftCell

REAL FUNCTION GetYShiftCell(self)

CLASS(Cell),  INTENT(IN)  :: self


!CALL self%GetCheck()
GetYShiftCell = self%YShift

END FUNCTION GetYShiftCell

FUNCTION GetAlignmentStringCell(self)

CLASS(Cell),  INTENT(IN)  :: self

CHARACTER(:), ALLOCATABLE :: GetAlignmentStringCell


!CALL self%GetCheck()
SELECT CASE(self%Alignment)
  CASE(-1)
    GetAlignmentStringCell = "left"
  CASE(0)
    GetAlignmentStringCell = "center"
  CASE(1)
    GetAlignmentStringCell = "right"    
  CASE DEFAULT
    STOP "Alignment Value should be in the Set (-1, 0, 1)"

END SELECT

END FUNCTION GetAlignmentStringCell

FUNCTION GetNextCell(self)

CLASS(Cell), TARGET, INTENT(IN)  :: self

TYPE(CELL),  POINTER             :: GetNextCell


IF(self%IsNext()) THEN
  GetNextCell => self%NextCell
ELSE
  NULLIFY(GetNextCell)
END IF

END FUNCTION GetNextCell


!SUBROUTINE CellEqualsCell(CellValue, self)  

!TYPE(Cell),  INTENT(OUT) :: CellValue
!CLASS(Cell), INTENT(IN)  :: self


!CALL self%GetCheck()
!CellValue%CellShape = self%CellShape
!CellValue%LatexContent = self%LatexContent
!CellValue%XShift = self%XShift
!CellValue%YShift = self%YShift

!END SUBROUTINE CellEqualsCell


SUBROUTINE DisplayCell(self)

CLASS(Cell), INTENT(IN) :: self


CALL self%GetCheck()
CALL self%CellShape%Display()
print*, self%LatexContent
print*, self%Alignment
print*, ALLOCATED(self%NextCell)

END SUBROUTINE DisplayCell


END MODULE CellMod
