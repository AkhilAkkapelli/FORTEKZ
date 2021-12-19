MODULE ShapeMod
IMPLICIT NONE


TYPE, ABSTRACT :: Shape

PRIVATE

  CHARACTER(:), ALLOCATABLE :: BoundaryColour
  CHARACTER(:), ALLOCATABLE :: InteriorColour
  CHARACTER(:), ALLOCATABLE :: ShapeType  
  
CONTAINS

  PROCEDURE, PASS(self), PUBLIC,  NON_OVERRIDABLE :: IsSet       => IsSetShape
  PROCEDURE, NOPASS,     PRIVATE, NON_OVERRIDABLE ::                GetCheckDimension
  PROCEDURE, PASS(self), PRIVATE, NON_OVERRIDABLE ::                GetCheckShape
  GENERIC,               PRIVATE                  :: GetCheck    => GetCheckDimension, GetCheckShape 

  PROCEDURE, PASS(self), PUBLIC,  NON_OVERRIDABLE :: SetShape    => SetShape
  PROCEDURE, PASS(self), PUBLIC,  NON_OVERRIDABLE :: RefineShape => RefineShape

  PROCEDURE, PASS(self), PUBLIC,  NON_OVERRIDABLE :: GetBoundCol => GetBoundaryColourShape
  PROCEDURE, PASS(self), PUBLIC,  NON_OVERRIDABLE :: GetIntCol   => GetInteriorColourShape  
  PROCEDURE, PASS(self), PUBLIC,  NON_OVERRIDABLE :: GetType     => GetTypeShape   

  PROCEDURE, PASS(self), PUBLIC,  NON_OVERRIDABLE :: Display     => DisplayShape

END TYPE Shape


TYPE, EXTENDS(Shape) :: Rectangle
  
PRIVATE
  REAL, ALLOCATABLE :: Width
  REAL, ALLOCATABLE :: Height
  REAL, ALLOCATABLE :: Radius    
    
CONTAINS
  
  PROCEDURE, PASS(self), PUBLIC :: Set       => SetRectangle
  PROCEDURE, PASS(self), PUBLIC :: Refine    => RefineRectangle 
  
  PROCEDURE, PASS(self), PUBLIC :: GetWidth  => GetWidthRectagle
  PROCEDURE, PASS(self), PUBLIC :: GetHeight => GetHeightRectangle    
  PROCEDURE, PASS(self), PUBLIC :: GetRadius => GetRadiusRectangle

END TYPE Rectangle

TYPE, EXTENDS(Shape) :: Square

PRIVATE
  REAL, ALLOCATABLE :: Side
  REAL, ALLOCATABLE :: Radius  
 
CONTAINS
  
  PROCEDURE, PASS(self), PUBLIC :: Set       => SetSquare
  PROCEDURE, PASS(self), PUBLIC :: Refine    => RefineSquare
      
  PROCEDURE, PASS(self), PUBLIC :: GetSide   => GetSideSquare    
  PROCEDURE, PASS(self), PUBLIC :: GetRadius => GetRadiusSquare    
      
END TYPE Square

TYPE, EXTENDS(Shape) :: Ellipse

PRIVATE
  REAL, ALLOCATABLE :: Width
  REAL, ALLOCATABLE :: Height 

CONTAINS
  
  PROCEDURE, PASS(self), PUBLIC :: Set       => SetEllipse
  PROCEDURE, PASS(self), PUBLIC :: Refine    => RefineEllipse 
  
  PROCEDURE, PASS(self), PUBLIC :: GetWidth  => GetWidthEllipse
  PROCEDURE, PASS(self), PUBLIC :: GetHeight => GetHeightEllipse          

END TYPE Ellipse

TYPE, EXTENDS(Shape) :: Circle

PRIVATE
  REAL, ALLOCATABLE :: Radius

CONTAINS
   
  PROCEDURE, PASS(self), PUBLIC :: Set       => SetCircle
  PROCEDURE, PASS(self), PUBLIC :: Refine    => RefineCircle  
  
  PROCEDURE, PASS(self), PUBLIC :: GetRadius => GetRadiusCircle  
  
END TYPE Circle

TYPE, EXTENDS(Rectangle) :: Diamond
END TYPE Diamond

TYPE, EXTENDS(Shape) :: RegularPolygon
 
 PRIVATE
   REAL,    ALLOCATABLE :: Radius
   INTEGER, ALLOCATABLE :: Sides
   REAL,    ALLOCATABLE :: RoundedRadius   
    
CONTAINS
   
  PROCEDURE, PASS(self), PUBLIC :: Set              => SetRegularPolygon 
  PROCEDURE, PASS(self), PUBLIC :: Refine           => RefineRegularPolygon
  
  PROCEDURE, PASS(self), PUBLIC :: GetRadius        => GetRadiusRegularPolygon   
  PROCEDURE, PASS(self), PUBLIC :: GetSides         => GetSidesRegularPolygon
  PROCEDURE, PASS(self), PUBLIC :: GetRoundedRadius => GetRoundedRadiusRegularPolygon  

END TYPE RegularPolygon

!TYPE, EXTENDS(Shape) :: Signal
! 
! PRIVATE
!   REAL, ALLOCATABLE :: Width
!   REAL, ALLOCATABLE :: Height
!    
!CONTAINS
!   
!  PROCEDURE, PASS(self), PUBLIC :: Set    => SetDiamond   
!  PROCEDURE, PASS(self), PUBLIC :: Refine => RefineDiamond   

!END TYPE Signal

!TYPE, EXTENDS(Shape) :: Dart
! 
! PRIVATE
!   REAL, ALLOCATABLE :: Width
!   REAL, ALLOCATABLE :: Height
!    
!CONTAINS
!   
!  PROCEDURE, PASS(self), PUBLIC :: Set    => SetDiamond   
!  PROCEDURE, PASS(self), PUBLIC :: Refine => RefineDiamond   

!END TYPE Dart

TYPE, EXTENDS(Shape) :: Cloud
 
 PRIVATE
   REAL,    ALLOCATABLE :: Width
   REAL,    ALLOCATABLE :: Height
   INTEGER, ALLOCATABLE :: Puffs
   REAL,    ALLOCATABLE :: PuffArc   
    
CONTAINS
   
  PROCEDURE, PASS(self), PUBLIC :: Set       => SetCloud 
  PROCEDURE, PASS(self), PUBLIC :: Refine    => RefineCloud  

  PROCEDURE, PASS(self), PUBLIC :: GetWidth  => GetWidthCloud
  PROCEDURE, PASS(self), PUBLIC :: GetHeight => GetHeightCloud  
  PROCEDURE, PASS(self), PUBLIC :: GetPuffs  => GetPuffsCloud
  PROCEDURE, PASS(self), PUBLIC :: GetArc    => GetPuffArcCloud

END TYPE Cloud

TYPE, EXTENDS(Shape) :: SingleArrow
 
 PRIVATE
   REAL, ALLOCATABLE :: Height 
   REAL, ALLOCATABLE :: Width
   REAL, ALLOCATABLE :: TipAngle
   REAL, ALLOCATABLE :: HeadExtend
   REAL, ALLOCATABLE :: HeadIndent      
    
CONTAINS
   
  PROCEDURE, PASS(self), PUBLIC :: Set       => SetSingleArrow   
  PROCEDURE, PASS(self), PUBLIC :: Refine    => RefineSingleArrow
  
  PROCEDURE, PASS(self), PUBLIC :: GetHeight => GetHeightSingleArrow  
  PROCEDURE, PASS(self), PUBLIC :: GetWidth  => GetWidthSingleArrow  
  PROCEDURE, PASS(self), PUBLIC :: GetAngle  => GetTipAngleSingleArrow
  PROCEDURE, PASS(self), PUBLIC :: GetExtend => GetHeadExtendSingleArrow  
  PROCEDURE, PASS(self), PUBLIC :: GetIndent => GetHeadIndentSingleArrow   

END TYPE SingleArrow

TYPE, EXTENDS(SingleArrow) :: DoubleArrow
END TYPE


PRIVATE :: IsSetShape, GetCheckDimension, GetCheckShape, SetShape, RefineShape, GetBoundaryColourShape,&
            GetInteriorColourShape, GetTypeShape, DisplayShape, SetRectangle, RefineRectangle, GetWidthRectagle, &
             GetHeightRectangle, GetRadiusRectangle, SetSquare, RefineSquare, GetSideSquare, GetRadiusSquare, &
              SetEllipse, RefineEllipse, GetWidthEllipse, GetHeightEllipse, SetCircle, RefineCircle, GetRadiusCircle,&
               SetRegularPolygon, RefineRegularPolygon, GetSidesRegularPolygon, GetRadiusRegularPolygon, &
               GetRoundedRadiusRegularPolygon,SetCloud, RefineCloud, GetWidthCloud, GetHeightCloud, GetPuffsCloud, GetPuffArcCloud,&
                 SetSingleArrow, RefineSingleArrow, GetHeightSingleArrow, GetWidthSingleArrow, GetTipAngleSingleArrow, &
                  GetHeadExtendSingleArrow, GetHeadIndentSingleArrow

PUBLIC  :: Shape, Rectangle, Square, Ellipse, Circle, Diamond, RegularPolygon, Cloud, SingleArrow, DoubleArrow


CONTAINS


LOGICAL FUNCTION IsSetShape(self)

CLASS(Shape), INTENT(IN) :: self


IsSetShape = (ALLOCATED(self%BoundaryColour) .OR. ALLOCATED(self%InteriorColour)) 
SELECT TYPE(ShapeValue => self)
  TYPE IS (Rectangle)
    IsSetShape = IsSetShape .AND. ALLOCATED(ShapeValue%Width) .AND. ALLOCATED(ShapeValue%Height) .AND. ALLOCATED(ShapeValue%Radius)
  TYPE IS (Square)
    IsSetShape = IsSetShape .AND. ALLOCATED(ShapeValue%Side) .AND. ALLOCATED(ShapeValue%Radius)
  TYPE IS (Ellipse)
    IsSetShape = IsSetShape .AND. ALLOCATED(ShapeValue%Width) .AND. ALLOCATED(ShapeValue%Height)
  TYPE IS (Circle)
    IsSetShape = IsSetShape .AND. ALLOCATED(ShapeValue%Radius)
  TYPE IS (Diamond)
    IsSetShape = IsSetShape .AND. ALLOCATED(ShapeValue%Width) .AND. ALLOCATED(ShapeValue%Height)
  TYPE IS (RegularPolygon)
    IsSetShape = IsSetShape .AND. ALLOCATED(ShapeValue%Radius) .AND. ALLOCATED(ShapeValue%Sides) &
          .AND. ALLOCATED(ShapeValue%RoundedRadius)
  TYPE IS (Cloud)
    IsSetShape = IsSetShape .AND. ALLOCATED(ShapeValue%Width) .AND. ALLOCATED(ShapeValue%Height) &
          .AND. ALLOCATED(ShapeValue%Puffs).AND. ALLOCATED(ShapeValue%PuffArc)          
  TYPE IS (SingleArrow)
    IsSetShape = IsSetShape .AND. ALLOCATED(ShapeValue%Width) .AND. ALLOCATED(ShapeValue%Height) &
         .AND. ALLOCATED(ShapeValue%TipAngle) .AND. ALLOCATED(ShapeValue%HeadExtend) .AND. ALLOCATED(ShapeValue%HeadIndent) 
  TYPE IS (DoubleArrow)
    IsSetShape = IsSetShape .AND. ALLOCATED(ShapeValue%Width) .AND. ALLOCATED(ShapeValue%Height) &
         .AND. ALLOCATED(ShapeValue%TipAngle) .AND. ALLOCATED(ShapeValue%HeadExtend) .AND. ALLOCATED(ShapeValue%HeadIndent)                      
  CLASS DEFAULT
    IsSetShape = .FALSE.
END SELECT 

END FUNCTION IsSetShape

SUBROUTINE GetCheckDimension(Dimension)

REAL, INTENT(IN) :: Dimension

IF(Dimension < 0.)  STOP "Dimension Value Out of Bounds[0%,1%]"

END SUBROUTINE GetCheckDimension

SUBROUTINE GetCheckShape(self)

CLASS(Shape), INTENT(IN) :: self


IF(.NOT. self%IsSet()) STOP "Shape not Set"

END SUBROUTINE GetCheckShape

SUBROUTINE SetShape(self, BoundaryColour, InteriorColour, ShapeType)

CLASS(Shape),               INTENT(INOUT) :: self
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: BoundaryColour
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: InteriorColour
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: ShapeType


IF(PRESENT(BoundaryColour)) THEN
  self%BoundaryColour = BoundaryColour
ELSE
  self%BoundaryColour = "black"
END IF  
IF(PRESENT(InteriorColour)) THEN
  self%InteriorColour = InteriorColour
ELSE
  self%InteriorColour = "none"   
END IF  
IF(PRESENT(ShapeType)) THEN
  self%ShapeType = ShapeType
ELSE
  self%ShapeType = " "
END IF 

END SUBROUTINE SetShape

SUBROUTINE RefineShape(self, BoundaryColour, InteriorColour, ShapeType)

CLASS(Shape),               INTENT(INOUT) :: self
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: BoundaryColour
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: InteriorColour
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: ShapeType 
 

IF(PRESENT(BoundaryColour)) self%BoundaryColour = BoundaryColour
IF(PRESENT(InteriorColour)) self%InteriorColour = InteriorColour
IF(PRESENT(ShapeType)) self%ShapeType = ShapeType

END SUBROUTINE RefineShape


SUBROUTINE SetRectangle(self, BoundaryColour, InteriorColour, Width, Height, Radius, ShapeType)

CLASS(Rectangle),           INTENT(OUT) :: self
CHARACTER(len=*), OPTIONAL, INTENT(IN)  :: BoundaryColour
CHARACTER(len=*), OPTIONAL, INTENT(IN)  :: InteriorColour
REAL,             OPTIONAL, INTENT(IN)  :: Width
REAL,             OPTIONAL, INTENT(IN)  :: Height
REAL,             OPTIONAL, INTENT(IN)  :: Radius
CHARACTER(len=*), OPTIONAL, INTENT(IN)  :: ShapeType


CALL self%SetShape(BoundaryColour = BoundaryColour, InteriorColour = InteriorColour, ShapeType = ShapeType) 
IF(PRESENT(Width)) THEN
  CALL self%GetCheck(Width)  
  self%Width = Width
ELSE
  self%Width = .9
END IF  
IF(PRESENT(Height)) THEN
  CALL self%GetCheck(Height)  
  self%Height = Height
ELSE
  self%Height = .9
END IF 
IF(PRESENT(Radius)) THEN
  CALL self%GetCheck(Radius)  
  self%Radius = Radius
ELSE
  self%Radius = .1
END IF 

END SUBROUTINE SetRectangle

SUBROUTINE RefineRectangle(self, BoundaryColour, InteriorColour, Width, Height, Radius, ShapeType)

CLASS(Rectangle),           INTENT(INOUT) :: self
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: BoundaryColour
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: InteriorColour
REAL,             OPTIONAL, INTENT(IN)    :: Width
REAL,             OPTIONAL, INTENT(IN)    :: Height
REAL,             OPTIONAL, INTENT(IN)    :: Radius
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: ShapeType 
 

CALL self%RefineShape(BoundaryColour = BoundaryColour, InteriorColour = InteriorColour, ShapeType = ShapeType) 
IF(PRESENT(Width)) THEN
  CALL self%GetCheck(Width)
  self%Width = Width
END IF
IF(PRESENT(Height)) THEN
  CALL self%GetCheck(Height)
  self%Height = Height
END IF
IF(PRESENT(Radius)) THEN
  CALL self%GetCheck(Radius)
  self%Radius = Radius
END IF

END SUBROUTINE RefineRectangle

REAL FUNCTION GetWidthRectagle(self)

CLASS(Rectangle), INTENT(IN) :: self


GetWidthRectagle = self%Width

END FUNCTION GetWidthRectagle

REAL FUNCTION GetHeightRectangle(self)

CLASS(Rectangle), INTENT(IN) :: self


GetHeightRectangle = self%Width

END FUNCTION GetHeightRectangle

REAL FUNCTION GetRadiusRectangle(self)

CLASS(Rectangle), INTENT(IN) :: self


GetRadiusRectangle = self%Radius

END FUNCTION GetRadiusRectangle

SUBROUTINE SetSquare(self, BoundaryColour, InteriorColour, Side, Radius, ShapeType)

CLASS(Square),              INTENT(OUT) :: self
CHARACTER(len=*), OPTIONAL, INTENT(IN)  :: BoundaryColour
CHARACTER(len=*), OPTIONAL, INTENT(IN)  :: InteriorColour
REAL,             OPTIONAL, INTENT(IN)  :: Side
REAL,             OPTIONAL, INTENT(IN)  :: Radius
CHARACTER(len=*), OPTIONAL, INTENT(IN)  :: ShapeType


CALL self%SetShape(BoundaryColour = BoundaryColour, InteriorColour = InteriorColour, ShapeType = ShapeType)  
IF(PRESENT(Side)) THEN
  CALL self%GetCheck(Side)  
  self%Side = Side
ELSE
  self%Side = .9
END IF  
IF(PRESENT(Radius)) THEN
  CALL self%GetCheck(Radius)  
  self%Radius = Radius
ELSE
  self%Radius = .1
END IF 

END SUBROUTINE SetSquare

SUBROUTINE RefineSquare(self, BoundaryColour, InteriorColour, Side, Radius, ShapeType)

CLASS(Square),              INTENT(INOUT) :: self
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: BoundaryColour
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: InteriorColour
REAL,             OPTIONAL, INTENT(IN)    :: Side
REAL,             OPTIONAL, INTENT(IN)    :: Radius
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: ShapeType 
 

CALL self%RefineShape(BoundaryColour = BoundaryColour, InteriorColour = InteriorColour, ShapeType = ShapeType) 
IF(PRESENT(Side)) THEN
  CALL self%GetCheck(Side)
  self%Side = Side
END IF
IF(PRESENT(Radius)) THEN
  CALL self%GetCheck(Radius)
  self%Radius = Radius
END IF

END SUBROUTINE RefineSquare

REAL FUNCTION GetSideSquare(self)

CLASS(Square), INTENT(IN) :: self


GetSideSquare = self%Side

END FUNCTION GetSideSquare

REAL FUNCTION GetRadiusSquare(self)

CLASS(Square), INTENT(IN) :: self


GetRadiusSquare = self%Radius

END FUNCTION GetRadiusSquare


SUBROUTINE SetEllipse(self, BoundaryColour, InteriorColour, Width, Height, ShapeType)

CLASS(Ellipse),             INTENT(OUT) :: self
CHARACTER(len=*), OPTIONAL, INTENT(IN)  :: BoundaryColour
CHARACTER(len=*), OPTIONAL, INTENT(IN)  :: InteriorColour
REAL,             OPTIONAL, INTENT(IN)  :: Width
REAL,             OPTIONAL, INTENT(IN)  :: Height
CHARACTER(len=*), OPTIONAL, INTENT(IN)  :: ShapeType


CALL self%SetShape(BoundaryColour = BoundaryColour, InteriorColour = InteriorColour, ShapeType = ShapeType)   
IF(PRESENT(Width)) THEN
  CALL self%GetCheck(Width)  
  self%Width = Width
ELSE
  self%Width = .9
END IF  
IF(PRESENT(Height)) THEN
  CALL self%GetCheck(Height)  
  self%Height = Height
ELSE
  self%Height = .9
END IF     

END SUBROUTINE SetEllipse

SUBROUTINE RefineEllipse(self, BoundaryColour, InteriorColour, Width, Height, ShapeType)

CLASS(Ellipse),             INTENT(INOUT) :: self
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: BoundaryColour
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: InteriorColour
REAL,             OPTIONAL, INTENT(IN)    :: Width
REAL,             OPTIONAL, INTENT(IN)    :: Height
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: ShapeType


CALL self%SetShape(BoundaryColour = BoundaryColour, InteriorColour = InteriorColour, ShapeType = ShapeType) 
IF(PRESENT(Width)) THEN
  CALL self%GetCheck(Width)
  self%Width = Width
END IF
IF(PRESENT(Height)) THEN
  CALL self%GetCheck(Height)
  self%Height = Height
END IF

END SUBROUTINE RefineEllipse

REAL FUNCTION GetWidthEllipse(self)

CLASS(Ellipse), INTENT(IN) :: self


GetWidthEllipse = self%Width

END FUNCTION GetWidthEllipse

REAL FUNCTION GetHeightEllipse(self)

CLASS(Ellipse), INTENT(IN) :: self


GetHeightEllipse = self%Height

END FUNCTION GetHeightEllipse


SUBROUTINE SetCircle(self, BoundaryColour, InteriorColour, Radius, ShapeType)

CLASS(Circle),              INTENT(OUT) :: self
CHARACTER(len=*), OPTIONAL, INTENT(IN)  :: BoundaryColour
CHARACTER(len=*), OPTIONAL, INTENT(IN)  :: InteriorColour
REAL,             OPTIONAL, INTENT(IN)  :: Radius
CHARACTER(len=*), OPTIONAL, INTENT(IN)  :: ShapeType


CALL self%SetShape(BoundaryColour = BoundaryColour, InteriorColour = InteriorColour, ShapeType = ShapeType)  
IF(PRESENT(Radius)) THEN
  CALL self%GetCheck(Radius)  
  self%Radius = Radius
ELSE
  self%Radius = .9
END IF  

END SUBROUTINE SetCircle

SUBROUTINE RefineCircle(self, BoundaryColour, InteriorColour, Radius, ShapeType)

CLASS(Circle),              INTENT(INOUT) :: self
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: BoundaryColour
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: InteriorColour
REAL,             OPTIONAL, INTENT(IN)    :: Radius
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: ShapeType


CALL self%RefineShape(BoundaryColour = BoundaryColour, InteriorColour = InteriorColour, ShapeType = ShapeType) 
IF(PRESENT(Radius)) THEN
  CALL self%GetCheck(Radius)
  self%Radius = Radius
END IF

END SUBROUTINE RefineCircle

REAL FUNCTION GetRadiusCircle(self)

CLASS(Circle), INTENT(IN) :: self


GetRadiusCircle = self%Radius

END FUNCTION GetRadiusCircle


SUBROUTINE SetRegularPolygon(self, BoundaryColour, InteriorColour, Radius, Sides, RoundedRadius, ShapeType)

CLASS(RegularPolygon),           INTENT(OUT) :: self
CHARACTER(len=*),      OPTIONAL, INTENT(IN)  :: BoundaryColour
CHARACTER(len=*),      OPTIONAL, INTENT(IN)  :: InteriorColour
REAL,                  OPTIONAL, INTENT(IN)  :: Radius
INTEGER,               OPTIONAL, INTENT(IN)  :: Sides
REAL,                  OPTIONAL, INTENT(IN)  :: RoundedRadius
CHARACTER(len=*),      OPTIONAL, INTENT(IN)  :: ShapeType


CALL self%SetShape(BoundaryColour = BoundaryColour, InteriorColour = InteriorColour, ShapeType = ShapeType)  
IF(PRESENT(Radius)) THEN
  CALL self%GetCheck(Radius)  
  self%Radius = Radius
ELSE
  self%Radius = .9
END IF 
IF(PRESENT(Sides)) THEN 
  self%Sides = Sides
ELSE
  self%Sides = 3
END IF  
IF(PRESENT(RoundedRadius)) THEN
  CALL self%GetCheck(RoundedRadius)  
  self%RoundedRadius = RoundedRadius
ELSE
  self%RoundedRadius = .1
END IF 

END SUBROUTINE SetRegularPolygon

SUBROUTINE RefineRegularPolygon(self, BoundaryColour, InteriorColour, Radius, Sides, RoundedRadius, ShapeType)

CLASS(RegularPolygon),           INTENT(INOUT) :: self
CHARACTER(len=*),      OPTIONAL, INTENT(IN)    :: BoundaryColour
CHARACTER(len=*),      OPTIONAL, INTENT(IN)    :: InteriorColour
REAL,                  OPTIONAL, INTENT(IN)    :: Radius
INTEGER,               OPTIONAL, INTENT(IN)    :: Sides
REAL,                  OPTIONAL, INTENT(IN)    :: RoundedRadius
CHARACTER(len=*),      OPTIONAL, INTENT(IN)    :: ShapeType


CALL self%RefineShape(BoundaryColour = BoundaryColour, InteriorColour = InteriorColour, ShapeType = ShapeType) 
IF(PRESENT(Radius)) THEN
  CALL self%GetCheck(Radius)
  self%Radius = Radius
END IF
IF(PRESENT(Sides)) self%Sides = Sides
IF(PRESENT(RoundedRadius)) THEN
  CALL self%GetCheck(RoundedRadius)  
  self%RoundedRadius = RoundedRadius
END IF 

END SUBROUTINE RefineRegularPolygon

REAL FUNCTION GetRadiusRegularPolygon(self)

CLASS(RegularPolygon), INTENT(IN) :: self


GetRadiusRegularPolygon = self%Radius

END FUNCTION GetRadiusRegularPolygon

INTEGER FUNCTION GetSidesRegularPolygon(self)

CLASS(RegularPolygon), INTENT(IN) :: self


GetSidesRegularPolygon = self%Sides

END FUNCTION GetSidesRegularPolygon

REAL FUNCTION GetRoundedRadiusRegularPolygon(self)

CLASS(RegularPolygon), INTENT(IN) :: self


GetRoundedRadiusRegularPolygon = self%RoundedRadius

END FUNCTION GetRoundedRadiusRegularPolygon


SUBROUTINE SetCloud(self, BoundaryColour, InteriorColour, Width, Height, Puffs, PuffArc, ShapeType)

CLASS(Cloud),               INTENT(OUT) :: self
CHARACTER(len=*), OPTIONAL, INTENT(IN)  :: BoundaryColour
CHARACTER(len=*), OPTIONAL, INTENT(IN)  :: InteriorColour
REAL,             OPTIONAL, INTENT(IN)  :: Width
REAL,             OPTIONAL, INTENT(IN)  :: Height
INTEGER,          OPTIONAL, INTENT(IN)  :: Puffs
REAL,             OPTIONAL, INTENT(IN)  :: PuffArc
CHARACTER(len=*), OPTIONAL, INTENT(IN)  :: ShapeType


CALL self%SetShape(BoundaryColour = BoundaryColour, InteriorColour = InteriorColour, ShapeType = ShapeType)  
IF(PRESENT(Width)) THEN
  CALL self%GetCheck(Width)
  self%Width = Width
ELSE
  self%Width = .9  
END IF
IF(PRESENT(Height)) THEN
  CALL self%GetCheck(Height)
  self%Height = Height
ELSE
  self%Height = .9   
END IF
IF(PRESENT(Puffs)) THEN
  self%Puffs = Puffs
ELSE
  self%Puffs = 10
END IF
IF(PRESENT(PuffArc)) THEN
  self%PuffArc = PuffArc  
ELSE
  self%PuffArc = 135.
END IF

END SUBROUTINE SetCloud

SUBROUTINE RefineCloud(self, BoundaryColour, InteriorColour, Width, Height, Puffs, PuffArc, ShapeType)

CLASS(Cloud),               INTENT(INOUT) :: self
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: BoundaryColour
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: InteriorColour
REAL,             OPTIONAL, INTENT(IN)    :: Width
REAL,             OPTIONAL, INTENT(IN)    :: Height
INTEGER,          OPTIONAL, INTENT(IN)    :: Puffs
REAL,             OPTIONAL, INTENT(IN)    :: PuffArc
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: ShapeType


CALL self%RefineShape(BoundaryColour = BoundaryColour, InteriorColour = InteriorColour, ShapeType = ShapeType) 
IF(PRESENT(Width)) THEN
  CALL self%GetCheck(Width)
  self%Width = Width
END IF
IF(PRESENT(Height)) THEN
  CALL self%GetCheck(Height)
  self%Height = Height
END IF
IF(PRESENT(Puffs)) self%Puffs = Puffs
IF(PRESENT(PuffArc)) self%PuffArc = PuffArc

END SUBROUTINE RefineCloud

REAL FUNCTION GetWidthCloud(self)

CLASS(Cloud), INTENT(IN) :: self


GetWidthCloud= self%Width

END FUNCTION GetWidthCloud

REAL FUNCTION GetHeightCloud(self)

CLASS(Cloud), INTENT(IN) :: self


GetHeightCloud = self%Height

END FUNCTION GetHeightCloud

INTEGER FUNCTION GetPuffsCloud(self)

CLASS(Cloud), INTENT(IN) :: self


GetPuffsCloud = self%Puffs

END FUNCTION GetPuffsCloud

REAL FUNCTION GetPuffArcCloud(self)

CLASS(Cloud), INTENT(IN) :: self


GetPuffArcCloud = self%PuffArc

END FUNCTION GetPuffArcCloud


SUBROUTINE SetSingleArrow(self, BoundaryColour, InteriorColour, Width, Height, TipAngle, HeadExtend, HeadIndent, ShapeType)

CLASS(SingleArrow),           INTENT(OUT) :: self
CHARACTER(len=*),   OPTIONAL, INTENT(IN)  :: BoundaryColour
CHARACTER(len=*),   OPTIONAL, INTENT(IN)  :: InteriorColour
REAL,               OPTIONAL, INTENT(IN)  :: Width
REAL,               OPTIONAL, INTENT(IN)  :: Height
REAL,               OPTIONAL, INTENT(IN)  :: TipAngle
REAL,               OPTIONAL, INTENT(IN)  :: HeadExtend
REAL,               OPTIONAL, INTENT(IN)  :: HeadIndent
CHARACTER(len=*),   OPTIONAL, INTENT(IN)  :: ShapeType


CALL self%SetShape(BoundaryColour = BoundaryColour, InteriorColour = InteriorColour, ShapeType = ShapeType)  
IF(PRESENT(Width)) THEN
  CALL self%GetCheck(Width)
  self%Width = Width
ELSE
  self%Width = .9  
END IF
IF(PRESENT(Height)) THEN
  CALL self%GetCheck(Height)
  self%Height = Height
ELSE
  self%Height = .9  
END IF
IF(PRESENT(TipAngle)) THEN
  self%TipAngle = TipAngle
ELSE
  self%TipAngle = 90.
END IF
IF(PRESENT(HeadExtend)) THEN
  CALL self%GetCheck(HeadExtend)
  self%HeadExtend = HeadExtend
ELSE
  self%HeadExtend = .1  
END IF
IF(PRESENT(HeadIndent)) THEN
  CALL self%GetCheck(HeadIndent)
  self%HeadIndent = HeadIndent
ELSE
  self%HeadIndent = 0.
END IF

END SUBROUTINE SetSingleArrow

SUBROUTINE RefineSingleArrow(self, BoundaryColour, InteriorColour, Width, Height, TipAngle, HeadExtend, HeadIndent, ShapeType)

CLASS(SingleArrow),           INTENT(INOUT) :: self
CHARACTER(len=*),   OPTIONAL, INTENT(IN)    :: BoundaryColour
CHARACTER(len=*),   OPTIONAL, INTENT(IN)    :: InteriorColour
REAL,               OPTIONAL, INTENT(IN)    :: Width
REAL,               OPTIONAL, INTENT(IN)    :: Height
REAL,               OPTIONAL, INTENT(IN)    :: TipAngle
REAL,               OPTIONAL, INTENT(IN)    :: HeadExtend
REAL,               OPTIONAL, INTENT(IN)    :: HeadIndent
CHARACTER(len=*),   OPTIONAL, INTENT(IN)    :: ShapeType


CALL self%RefineShape(BoundaryColour = BoundaryColour, InteriorColour = InteriorColour, ShapeType = ShapeType) 
IF(PRESENT(Width)) THEN
  CALL self%GetCheck(Width)
  self%Width = Width
END IF
IF(PRESENT(Height)) THEN
  CALL self%GetCheck(Height)
  self%Height = Height
END IF
IF(PRESENT(TipAngle)) self%TipAngle = TipAngle
IF(PRESENT(HeadExtend)) THEN
  CALL self%GetCheck(HeadExtend)
  self%HeadExtend = HeadExtend
END IF
IF(PRESENT(HeadIndent)) THEN
  CALL self%GetCheck(HeadIndent)
  self%HeadIndent = HeadIndent
END IF

END SUBROUTINE RefineSingleArrow


REAL FUNCTION GetWidthSingleArrow(self)

CLASS(SingleArrow), INTENT(IN) :: self


GetWidthSingleArrow = self%Width

END FUNCTION GetWidthSingleArrow

REAL FUNCTION GetHeightSingleArrow(self)

CLASS(SingleArrow), INTENT(IN) :: self


GetHeightSingleArrow = self%Height

END FUNCTION GetHeightSingleArrow

REAL FUNCTION GetTipAngleSingleArrow(self)

CLASS(SingleArrow), INTENT(IN) :: self


GetTipAngleSingleArrow = self%TipAngle

END FUNCTION GetTipAngleSingleArrow

REAL FUNCTION GetHeadExtendSingleArrow(self)

CLASS(SingleArrow), INTENT(IN) :: self


GetHeadExtendSingleArrow = self%HeadExtend

END FUNCTION GetHeadExtendSingleArrow

REAL FUNCTION GetHeadIndentSingleArrow(self)

CLASS(SingleArrow), INTENT(IN) :: self


GetHeadIndentSingleArrow = self%HeadIndent

END FUNCTION GetHeadIndentSingleArrow


FUNCTION GetBoundaryColourShape(self)

CLASS(Shape), INTENT(IN)  :: self

CHARACTER(:), ALLOCATABLE :: GetBoundaryColourShape


GetBoundaryColourShape = self%BoundaryColour

END FUNCTION GetBoundaryColourShape

FUNCTION GetInteriorColourShape(self)

CLASS(Shape), INTENT(IN)  :: self

CHARACTER(:), ALLOCATABLE :: GetInteriorColourShape


GetInteriorColourShape = self%InteriorColour

END FUNCTION GetInteriorColourShape

FUNCTION GetTypeShape(self)

CLASS(Shape), INTENT(IN)  :: self

CHARACTER(:), ALLOCATABLE :: GetTypeShape


IF("%"//TRIM(self%ShapeType)//"%" == "%%") THEN
  GetTypeShape = ""
ELSE
  GetTypeShape = self%ShapeType
END IF

END FUNCTION GetTypeShape


!SUBROUTINE ShapeEqualsShape(ShapeVal, self)  

!CLASS(Shape), ALLOCATABLE, INTENT(OUT) :: ShapeVal
!CLASS(Shape),              INTENT(IN)  :: self


!CALL self%GetCheck()
!IF(self%BoundaryColour%IsSet()) ShapeVal%BoundaryColour = self%BoundaryColour
!IF(self%InteriorColour%IsSet()) ShapeVal%InteriorColour = self%InteriorColour
!SELECT TYPE(ShapeValue => self)
!  TYPE IS (Rectangle)
!    ALLOCATE(ShapeVal, SOURCE=ShapeValue)
!  TYPE IS (Square)
!    ALLOCATE(ShapeVal, SOURCE=ShapeValue)
!  TYPE IS (Ellipse)
!    ALLOCATE(ShapeVal, SOURCE=ShapeValue)
!  TYPE IS (Circle)
!    ALLOCATE(ShapeVal, SOURCE=ShapeValue)
!  TYPE IS (RoundedRectangle)
!    ALLOCATE(ShapeVal, SOURCE=ShapeValue)
!  TYPE IS (Diamond)
!    ALLOCATE(ShapeVal, SOURCE=ShapeValue)
!  CLASS DEFAULT
!    STOP "Invalid Type of Shape"
!END SELECT 

!END SUBROUTINE ShapeEqualsShape


SUBROUTINE DisplayShape(self)

CLASS(Shape), INTENT(IN) :: self


CALL self%GetCheck()
PRINT*, self%BoundaryColour
PRINT*, self%InteriorColour
SELECT TYPE(ShapeValue => self)
  TYPE IS (Rectangle)
    PRINT*, ShapeValue%Width
    PRINT*, ShapeValue%Height
  TYPE IS (Square)
    PRINT*, ShapeValue%Side
    PRINT*, ShapeValue%Radius
  TYPE IS (Ellipse)
    PRINT*, ShapeValue%Width
    PRINT*, ShapeValue%Height
  TYPE IS (Circle)
    PRINT*, ShapeValue%Radius
  TYPE IS (Diamond)
    PRINT*, ShapeValue%Width
    PRINT*, ShapeValue%Height
  TYPE IS (RegularPolygon)
    PRINT*, ShapeValue%Radius
    PRINT*, ShapeValue%Sides
    PRINT*, ShapeValue%RoundedRadius    
  TYPE IS (Cloud)
    PRINT*, ShapeValue%Width
    PRINT*, ShapeValue%Height 
    PRINT*, ShapeValue%Puffs
    PRINT*, ShapeValue%PuffArc            
  TYPE IS (SingleArrow)
    PRINT*, ShapeValue%Width
    PRINT*, ShapeValue%Height
    PRINT*, ShapeValue%TipAngle
    PRINT*, ShapeValue%HeadExtend
    PRINT*, ShapeValue%HeadIndent
  TYPE IS (DoubleArrow)
    PRINT*, ShapeValue%Width
    PRINT*, ShapeValue%Height
    PRINT*, ShapeValue%TipAngle
    PRINT*, ShapeValue%HeadExtend
    PRINT*, ShapeValue%HeadIndent               
  CLASS DEFAULT
    STOP "Invalid Type of Shape"
END SELECT 

END SUBROUTINE DisplayShape


END MODULE ShapeMod
