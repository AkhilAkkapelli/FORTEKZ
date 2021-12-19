MODULE ArrowMod
IMPLICIT NONE


TYPE Arrow

PRIVATE
 
  CHARACTER(:), ALLOCATABLE :: ArrowType
  CHARACTER(:), ALLOCATABLE :: Path     
  REAL, ALLOCATABLE         :: Width
  CHARACTER(:), ALLOCATABLE :: ArrowColour  
  REAL, ALLOCATABLE         :: Angle
  REAL, ALLOCATABLE         :: StartPoint
  REAL, ALLOCATABLE         :: EndPoint   

CONTAINS

  PROCEDURE, PASS(self), PUBLIC  :: IsSet         => IsSetArrow
  PROCEDURE, NOPASS,     PRIVATE ::                  GetCheckWidthArrow 
  PROCEDURE, PASS(self), PRIVATE ::                  GetCheckArrow
  GENERIC,               PRIVATE :: GetCheck      => GetCheckWidthArrow, GetCheckArrow 
  
  PROCEDURE, PASS(self), PUBLIC  :: Set           => SetArrow 
  PROCEDURE, PASS(self), PUBLIC  :: Refine        => RefineArrow   
   
  PROCEDURE, PASS(self), PUBLIC  :: GetType       => GetTypeArrow
  PROCEDURE, PASS(self), PUBLIC  :: GetPath       => GetPathArrow  
  PROCEDURE, PASS(self), PUBLIC  :: GetWidth      => GetWidthArrow
  PROCEDURE, PASS(self), PUBLIC  :: GetColour     => GetColourArrow 
  PROCEDURE, PASS(self), PUBLIC  :: GetAngle      => GetAngleArrow
  PROCEDURE, PASS(self), PUBLIC  :: GetStart      => GetStartPointArrow
  PROCEDURE, PASS(self), PUBLIC  :: GetEnd        => GetEndPointArrow           
 
  PROCEDURE, PASS(self), PRIVATE ::                  ArrowEqualsArrow
  GENERIC,               PUBLIC  :: ASSIGNMENT(=) => ArrowEqualsArrow  
  
  PROCEDURE, PASS(self), PUBLIC  :: Display       => DisplayArrow
  
END TYPE Arrow


TYPE(Arrow) :: DefaultArrow


PRIVATE :: IsSetArrow, GetCheckWidthArrow, GetCheckArrow, SetArrow, RefineArrow, GetTypeArrow, GetPathArrow, GetWidthArrow, &
            GetColourArrow, GetAngleArrow, GetStartPointArrow, GetEndPointArrow, ArrowEqualsArrow, DisplayArrow

PUBLIC  :: Arrow


CONTAINS


LOGICAL FUNCTION IsSetArrow(self)

CLASS(Arrow), INTENT(IN) :: self


IsSetArrow = ALLOCATED(self%ArrowType) .AND. ALLOCATED(self%Path) .AND. ALLOCATED(self%Width) &
              .AND. ALLOCATED(self%ArrowColour) .AND. ALLOCATED(self%Angle)

END FUNCTION IsSetArrow

SUBROUTINE GetCheckWidthArrow(Width)

REAL :: Width


IF(Width < 0. .OR. Width > 1.) STOP "Width Value Out of Bounds[0%,Inf)"

END SUBROUTINE GetCheckWidthArrow

SUBROUTINE GetCheckArrow(self)

CLASS(Arrow), INTENT(IN) :: self


IF(.NOT. self%IsSet()) STOP "Arrow not Set"

END SUBROUTINE GetCheckArrow


SUBROUTINE SetArrow(self, ArrowType, Width, Path, ArrowColour, Angle, StartPoint, EndPoint)

CLASS(Arrow),               INTENT(OUT) :: self
CHARACTER(len=*), OPTIONAL, INTENT(IN)  :: ArrowType
CHARACTER(len=*), OPTIONAL, INTENT(IN)  :: Path
REAL,             OPTIONAL, INTENT(IN)  :: Width
CHARACTER(len=*), OPTIONAL, INTENT(IN)  :: ArrowColour 
REAL,             OPTIONAL, INTENT(IN)  :: Angle
REAL,             OPTIONAL, INTENT(IN)  :: StartPoint
REAL,             OPTIONAL, INTENT(IN)  :: EndPoint


IF(PRESENT(ArrowType)) THEN
  self%ArrowType = ArrowType
ELSE
  self%ArrowType = "-latex'"
END IF
IF(PRESENT(Path)) THEN
  self%Path = Path
ELSE
  self%Path = "to"
END IF
IF(PRESENT(Width)) THEN
  CALL self%GetCheck(Width)
  self%Width = Width
ELSE
  self%Width = 0.
END IF
IF(PRESENT(ArrowColour)) THEN
  self%ArrowColour = ArrowColour
ELSE
  self%ArrowColour = "black"
END IF
IF(PRESENT(Angle)) THEN
  self%Angle = Angle
ELSE
  self%Angle = 0.
END IF
IF(PRESENT(StartPoint)) self%StartPoint = StartPoint
IF(PRESENT(EndPoint)) self%EndPoint = EndPoint

END SUBROUTINE SetArrow

SUBROUTINE RefineArrow(self, ArrowType, Path, Width, ArrowColour, Angle, StartPoint, EndPoint)

CLASS(Arrow),               INTENT(INOUT) :: self
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: ArrowType
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: Path
REAL,             OPTIONAL, INTENT(IN)    :: Width
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: ArrowColour 
REAL,             OPTIONAL, INTENT(IN)    :: Angle
REAL,             OPTIONAL, INTENT(IN)    :: StartPoint
REAL,             OPTIONAL, INTENT(IN)    :: EndPoint


IF(PRESENT(ArrowType)) THEN
  self%ArrowType = ArrowType
END IF
IF(PRESENT(Path)) THEN
  self%Path = Path
END IF
IF(PRESENT(Width)) THEN
  CALL self%GetCheck(Width)
  self%Width = Width
END IF
IF(PRESENT(ArrowColour)) self%ArrowColour = ArrowColour
IF(PRESENT(Angle))       self%Angle = Angle
IF(PRESENT(StartPoint))  self%StartPoint = StartPoint
IF(PRESENT(EndPoint))    self%EndPoint = EndPoint

END SUBROUTINE RefineArrow


FUNCTION GetTypeArrow(self)

CLASS(Arrow), INTENT(IN) :: self

CHARACTER(:), ALLOCATABLE :: GetTypeArrow


GetTypeArrow = self%ArrowType

END FUNCTION GetTypeArrow

FUNCTION GetPathArrow(self)

CLASS(Arrow), INTENT(IN) :: self

CHARACTER(:), ALLOCATABLE :: GetPathArrow


GetPathArrow = self%Path

END FUNCTION GetPathArrow

REAL FUNCTION GetWidthArrow(self)

CLASS(Arrow), INTENT(IN) :: self


GetWidthArrow = self%Width

END FUNCTION GetWidthArrow

FUNCTION GetColourArrow(self)

CLASS(Arrow), INTENT(IN) :: self

CHARACTER(:), ALLOCATABLE :: GetColourArrow


GetColourArrow = self%ArrowColour

END FUNCTION GetColourArrow

REAL FUNCTION GetAngleArrow(self)

CLASS(Arrow), INTENT(IN) :: self


GetAngleArrow = self%Angle

END FUNCTION GetAngleArrow

FUNCTION GetStartPointArrow(self)

CLASS(Arrow), INTENT(IN)  :: self

CHARACTER(8)              :: GetStartPointArrow


IF(ALLOCATED(self%StartPoint)) THEN
  WRITE(GetStartPointArrow,'(A,F7.3)') ".",self%StartPoint
ELSE
  GetStartPointArrow = ""
END IF  

END FUNCTION GetStartPointArrow

FUNCTION GetEndPointArrow(self)

CLASS(Arrow), INTENT(IN)  :: self

CHARACTER(8)              :: GetEndPointArrow


IF(ALLOCATED(self%EndPoint)) THEN
  WRITE(GetEndPointArrow,'(A,F7.3)') ".",self%EndPoint
ELSE
  GetEndPointArrow = ""
END IF  

END FUNCTION GetEndPointArrow


SUBROUTINE ArrowEqualsArrow(ArrowVal, self)  

CLASS(Arrow), INTENT(OUT) :: ArrowVal
CLASS(Arrow), INTENT(IN)  :: self


CALL self%GetCheck()
ArrowVal%ArrowType   = self%ArrowType
ArrowVal%Path        = self%Path
ArrowVal%Width       = self%Width
ArrowVal%ArrowColour = self%ArrowColour
ArrowVal%Angle       = self%Angle 
IF(ALLOCATED(self%StartPoint)) ArrowVal%StartPoint = self%StartPoint 
IF(ALLOCATED(self%EndPoint)) ArrowVal%EndPoint = self%EndPoint

END SUBROUTINE ArrowEqualsArrow


SUBROUTINE DisplayArrow(self)

CLASS(Arrow), INTENT(IN) :: self


CALL self%GetCheck()
PRINT*, self%ArrowType
PRINT*, self%Path
PRINT*, self%Width
PRINT*, self%ArrowColour
PRINT*, self%Angle
IF(ALLOCATED(self%StartPoint)) print*, self%StartPoint
IF(ALLOCATED(self%StartPoint)) print*, self%EndPoint

END SUBROUTINE DisplayArrow


END MODULE ArrowMod
