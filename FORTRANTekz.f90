MODULE FORTRANTekz
USE ShapeMod,  ONLY : Shape, Rectangle, Square, Ellipse, Circle, Diamond, RegularPolygon, Cloud, SingleArrow, DoubleArrow
USE CellMod,   ONLY : Cell
USE ArrowMod,  ONLY : Arrow
USE BoxMod,    ONLY : Box
IMPLICIT NONE


INTEGER,                    PARAMETER :: filenamelim = 100
CHARACTER(len=filenamelim), PARAMETER :: LatexDir = "LaTex"
CHARACTER(len=filenamelim), PARAMETER :: PDFDir = "PDF"

LOGICAL,       PARAMETER :: Potrait   = .TRUE.
LOGICAL,       PARAMETER :: Landscape = .FALSE.

LOGICAL,       PARAMETER :: Left   = .TRUE.
LOGICAL,       PARAMETER :: Center = .FALSE.

LOGICAL,       PARAMETER :: Beamer = .TRUE.
LOGICAL,       PARAMETER :: A4Page = .FALSE.

CHARACTER(12), PARAMETER :: TextWidStr = "\textwidth "
CHARACTER(12), PARAMETER :: TextHgtStr = "\textheight"

CHARACTER(len=filenamelim):: TekzFile = "testFORTRANTekz"
CHARACTER(len=filenamelim):: RootTexFile = "MainBeamer"

REAL          :: AspectRatio = 0.
LOGICAL       :: IsFrameTitle = .FALSE.
LOGICAL       :: DiagOrientation = Potrait
LOGICAL       :: DiagAlignment = Left
REAL          :: PageWidLim = .97
REAL          :: PageHgtLim = .97
CHARACTER(12) :: PageWidStr, PageHgtStr
INTEGER       :: ID = 0

PRIVATE :: Cell, Arrow, Box

PRIVATE :: filenamelim, TextWidStr, TextHgtStr, IsFrameTitle, DiagOrientation, &
            PageWidLim, PageHgtLim, PageWidStr, PageHgtStr, ID
            
PUBLIC  :: RootTexFile, TekzFile, LatexDir, PDFDir, Left, Center, Beamer, A4Page

PRIVATE :: RefineBox, RefineVectorBox, RefineMatrixBox, DrawBox, DrawVectorBox, DrawMatrixBox, &
            RowVectorRatioBox, ColumnVectorRatioBox, MatrixRatioBox

PUBLIC  :: StartTekzDiag, EndTekzDiag, Divide, Refine, Draw, DrawLink


INTERFACE Refine

	MODULE PROCEDURE RefineBox
	MODULE PROCEDURE RefineVectorBox
	MODULE PROCEDURE RefineMatrixBox	

END INTERFACE Refine

INTERFACE Draw

	MODULE PROCEDURE DrawBox	
	MODULE PROCEDURE DrawVectorBox	
	MODULE PROCEDURE DrawMatrixBox

END INTERFACE Draw


CONTAINS


SUBROUTINE StartTekzDiag(MainBox, TekzName, Document, FrameTitle, DiagDimension, Alignment)

TYPE(Box),                  INTENT(INOUT) :: MainBox
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: TekzName
LOGICAL,          OPTIONAL, INTENT(IN)    :: Document
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: FrameTitle
!LOGICAL,          OPTIONAL, INTENT(IN)    :: Orientation
LOGICAL,          OPTIONAL, INTENT(IN)    :: Alignment
REAL,             OPTIONAL, INTENT(IN)    :: DiagDimension(2)

CHARACTER(len=filenamelim) :: TekzPath


IF(PRESENT(TekzName)) TekzFile = TekzName 
TekzPath = TRIM(LatexDir)//'/'//TRIM(TekzFile)//".tex"

OPEN(100, FILE=TekzPath, ACTION="WRITE")

!WRITE(100,"(A,A,A,/)")  "%!TEX root = ",TRIM(RootTexFile),".tex"

IF(PRESENT(Document)) THEN
  IF(Document .EQV. Beamer) THEN
    AspectRatio = 1.32
    RootTexFile = "MainBeamer"
  ELSEIF(Document .EQV. A4Page) THEN
    AspectRatio = 0.627
    RootTexFile = "MainPage"
  END IF
ELSE
  AspectRatio = 1.32
END IF  

IF(PRESENT(FrameTitle))  THEN
  IsFrameTitle = .TRUE.
  PageHgtLim = 0.87
  WRITE(100,"(A,A,A)") "\begin{frame}{",TRIM(FrameTitle),"}"
END IF

IF(PRESENT(Alignment)) THEN
  DiagAlignment = Alignment
END IF
IF(DiagAlignment .EQV. Center)  WRITE(100,"(A)") "\begin{center}{"
 
WRITE(100,"(A)", ADVANCE = 'NO') "\begin{tikzpicture}"
!IF(PRESENT(Orientation)) DiagOrientation = Orientation
!IF(DiagOrientation .EQV. Landscape) WRITE(100,"(A)", ADVANCE = 'NO') "[rotate=90, transform shape]"
WRITE(100,"(A,/)") ""

IF(DiagOrientation .EQV. Potrait) THEN
  PageWidStr = TextWidStr
  PageHgtStr = TextHgtStr
ELSE
  PageWidStr = TextHgtStr
  PageHgtStr = TextWidStr  
END IF

WRITE(100,"(A)") "\node at ( 0, 0) {};" 

IF(PRESENT(DiagDimension)) THEN
  IF(ANY(DiagDimension <= 0.) .OR. ANY(DiagDimension > 1.)) STOP "DiagDimension Out of Range (0., 1.]"
  PageWidLim = DiagDimension(1)*PageWidLim
  PageHgtLim = DiagDimension(2)*PageHgtLim
  WRITE(100,"(A,F6.3,A,A,F6.3,A,A,/)") "\node at ( ", PageWidLim, PageWidStr,", ", &
          -PageHgtLim,PageHgtStr,") {};" 
ELSE
  WRITE(100,"(A,F6.3,A,A,F6.3,A,A,/)") "\node at ( ",PageWidLim,PageWidStr,", ",-PageHgtLim,PageHgtStr,") {};"
END IF

CALL MainBox%Set(ID = ID, XLocus = PageWidLim/2, YLocus = PageHgtLim/2, XLimit = PageWidLim, YLimit = PageHgtLim)

END SUBROUTINE StartTekzDiag


SUBROUTINE EndTekzDiag


WRITE(100,"(A)") "\end{tikzpicture}"
IF(DiagAlignment .EQV. Center)  WRITE(100,"(A)") "}\end{center}"
IF(IsFrameTitle)  WRITE(100,"(A)") "\end{frame}"
CLOSE(100)

END SUBROUTINE EndTekzDiag


SUBROUTINE Divide(ChildBox, ParentBox)

TYPE(Box),    INTENT(INOUT) :: ChildBox(:,:)
TYPE(Box),    INTENT(IN)   :: ParentBox

INTEGER                    :: i, j
REAL                       :: ChildXLimit, ChildYLimit, ChildXLocus, ChildYLocus

TYPE(Cell),   POINTER      :: BoxCell
CLASS(Shape), ALLOCATABLE  :: CellShape


DO i = 1, size(ChildBox,1)
  DO j = 1, size(ChildBox,2)
    ID = ID + 1
    BoxCell => ParentBox%GetCell()
    
    ALLOCATE(CellShape, SOURCE=BoxCell%GetShape()) 

    ChildXLimit = ParentBox%GetXLim()/size(ChildBox,2)
    ChildYLimit = ParentBox%GetYLim()/size(ChildBox,1)
    ChildXLocus = ParentBox%GetXLocus() - ParentBox%GetXLim()/2 + (j-.5)*ChildXLimit
    ChildYLocus = ParentBox%GetYLocus() - ParentBox%GetYLim()/2 + (i-.5)*ChildYLimit

    CALL ChildBox(i,j)%Set(ID = ID, XLocus = ChildXLocus, YLocus = ChildYLocus, &
              XLimit = ChildXLimit, YLimit = ChildYLimit)
              
    DEALLOCATE(CellShape)        
  END DO
END DO

END SUBROUTINE Divide

SUBROUTINE RefineBox(CurrentBox, BoxBoundaryColour, BoxInteriorColour, CellShape, CellBoundaryColour, CellInteriorColour, &
            Width, Height, Side, RoundedRadius, Radius, TipAngle, HeadExtend, HeadIndent, Sides, Puffs, PuffArc, CellShapeType,&
             LatexContent, CellXShift, CellYShift, Alignment, Layer)

TYPE(Box),                  INTENT(INOUT) :: CurrentBox
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: BoxBoundaryColour
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: BoxInteriorColour
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: CellShape
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: CellBoundaryColour
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: CellInteriorColour
REAL,             OPTIONAL, INTENT(IN)    :: Width
REAL,             OPTIONAL, INTENT(IN)    :: Height
REAL,             OPTIONAL, INTENT(IN)    :: Side
REAL,             OPTIONAL, INTENT(IN)    :: RoundedRadius
REAL,             OPTIONAL, INTENT(IN)    :: Radius
REAL,             OPTIONAL, INTENT(IN)    :: TipAngle
REAL,             OPTIONAL, INTENT(IN)    :: HeadExtend
REAL,             OPTIONAL, INTENT(IN)    :: HeadIndent
INTEGER,          OPTIONAL, INTENT(IN)    :: Sides
INTEGER,          OPTIONAL, INTENT(IN)    :: Puffs
REAL,             OPTIONAL, INTENT(IN)    :: PuffArc
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: CellShapeType
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: LatexContent
REAL,             OPTIONAL, INTENT(IN)    :: CellXShift
REAL,             OPTIONAL, INTENT(IN)    :: CellYShift
INTEGER,          OPTIONAL, INTENT(IN)    :: Alignment
INTEGER,          OPTIONAL, INTENT(IN)    :: Layer

INTEGER                                   :: LayerIndex
TYPE(Cell)                                :: CurrentCell
CLASS(Shape),     ALLOCATABLE             :: CurrentShape
TYPE(Rectangle)                           :: CurrentRectangle
TYPE(Square)                              :: CurrentSquare
TYPE(Ellipse)                             :: CurrentEllipse
TYPE(Circle)                              :: CurrentCircle
TYPE(Diamond)                             :: CurrentDiamond
TYPE(RegularPolygon)                      :: CurrentRegularPolygon
TYPE(Cloud)                               :: CurrentCloud
TYPE(SingleArrow)                         :: CurrentSingleArrow
TYPE(DoubleArrow)                         :: CurrentDoubleArrow


CALL CurrentBox%Refine(BoundaryColour = BoxBoundaryColour, InteriorColour = BoxInteriorColour)

IF(PRESENT(Layer)) THEN
  IF(Layer == CurrentBox%GetCellSize() + 1) THEN
    CALL CurrentBox%Refine(BoxCellLayer = Layer)
    LayerIndex = Layer
  ELSE
    CALL CurrentBox%GetCheck(Layer)
      LayerIndex = Layer
  END IF
ELSE  
  LayerIndex = 1
END IF

IF(PRESENT(CellShape)) THEN
  CurrentCell = CurrentBox%GetCell(LayerIndex)
  ALLOCATE(CurrentShape, SOURCE=CurrentCell%GetShape())
  SELECT CASE (CellShape)
    CASE ("Rectangle")
      SELECT TYPE(ShapeValue => CurrentShape)
        TYPE IS (Rectangle)
        CLASS DEFAULT
          CALL CurrentRectangle%Set()
          CALL CurrentCell%Refine(CellShape = CurrentRectangle)
          CALL CurrentBox%Refine(BoxCell = CurrentCell, BoxCellLayer = LayerIndex)
      END SELECT 
    CASE ("Square")
      SELECT TYPE(ShapeValue => CurrentShape)
        TYPE IS (Square)
        CLASS DEFAULT
          CALL CurrentSquare%Set()
          CALL CurrentCell%Refine(CellShape = CurrentSquare)
          CALL CurrentBox%Refine(BoxCell = CurrentCell, BoxCellLayer = LayerIndex)
      END SELECT 
    CASE ("Ellipse")
      SELECT TYPE(ShapeValue => CurrentShape)
        TYPE IS (Ellipse)
        CLASS DEFAULT
          CALL CurrentEllipse%Set()
          CALL CurrentCell%Refine(CellShape = CurrentEllipse)
          CALL CurrentBox%Refine(BoxCell = CurrentCell, BoxCellLayer = LayerIndex)
      END SELECT
    CASE ("Circle")
      SELECT TYPE(ShapeValue => CurrentShape)
        TYPE IS (Circle)
        CLASS DEFAULT
          CALL CurrentCircle%Set()
          CALL CurrentCell%Refine(CellShape = CurrentCircle)
          CALL CurrentBox%Refine(BoxCell = CurrentCell, BoxCellLayer = LayerIndex)
      END SELECT  
    CASE ("Diamond")
      SELECT TYPE(ShapeValue => CurrentShape)
        TYPE IS (Diamond)
        CLASS DEFAULT
          CALL CurrentDiamond%Set()
          CALL CurrentCell%Refine(CellShape = CurrentDiamond)
          CALL CurrentBox%Refine(BoxCell = CurrentCell, BoxCellLayer = LayerIndex)
      END SELECT 
    CASE ("RegularPolygon")
      SELECT TYPE(ShapeValue => CurrentShape)
        TYPE IS (RegularPolygon)
        CLASS DEFAULT
          CALL CurrentRegularPolygon%Set()
          CALL CurrentCell%Refine(CellShape = CurrentRegularPolygon)
          CALL CurrentBox%Refine(BoxCell = CurrentCell, BoxCellLayer = LayerIndex)
      END SELECT 
    CASE ("Cloud")
      SELECT TYPE(ShapeValue => CurrentShape)
        TYPE IS (Cloud)
        CLASS DEFAULT
          CALL CurrentCloud%Set()
          CALL CurrentCell%Refine(CellShape = CurrentCloud)
          CALL CurrentBox%Refine(BoxCell = CurrentCell, BoxCellLayer = LayerIndex)
      END SELECT               
    CASE ("SingleArrow")
      SELECT TYPE(ShapeValue => CurrentShape)
        TYPE IS (SingleArrow)
        CLASS DEFAULT
          CALL CurrentSingleArrow%Set()
          CALL CurrentCell%Refine(CellShape = CurrentSingleArrow)
          CALL CurrentBox%Refine(BoxCell = CurrentCell, BoxCellLayer = LayerIndex)
      END SELECT 
    CASE ("DoubleArrow")
      SELECT TYPE(ShapeValue => CurrentShape)
        TYPE IS (DoubleArrow)
        CLASS DEFAULT
          CALL CurrentDoubleArrow%Set()
          CALL CurrentCell%Refine(CellShape = CurrentDoubleArrow)
          CALL CurrentBox%Refine(BoxCell = CurrentCell, BoxCellLayer = LayerIndex)
      END SELECT                  
    CASE ("None")
      SELECT TYPE(ShapeValue => CurrentShape)
        CLASS DEFAULT
          CALL CurrentRectangle%Set(BoundaryColour = "none", Width = 1., Height = 1.)
          CALL CurrentCell%Refine(CellShape = CurrentRectangle)
          CALL CurrentBox%Refine(BoxCell = CurrentCell, BoxCellLayer = LayerIndex)
      END SELECT           
    CASE DEFAULT
      STOP "Invalid Shape"
  END SELECT
  DEALLOCATE(CurrentShape)
END IF

IF(PRESENT(CellBoundaryColour) .OR. PRESENT(CellInteriorColour) .OR. PRESENT(Width) .OR. PRESENT(Height) .OR. &
    PRESENT(Side) .OR. PRESENT(RoundedRadius) .OR. PRESENT(Radius) .OR. PRESENT(TipAngle) .OR. PRESENT(HeadExtend) .OR. &
     PRESENT(HeadIndent) .OR. PRESENT(Sides) .OR. PRESENT(Puffs) .OR. PRESENT(PuffArc) .OR. PRESENT(CellShapeType)) THEN
  CurrentCell = CurrentBox%GetCell(LayerIndex)
  ALLOCATE(CurrentShape, SOURCE=CurrentCell%GetShape())
  SELECT TYPE(ShapeValue => CurrentShape)
    TYPE IS (Rectangle)
      CALL ShapeValue%Refine(BoundaryColour = CellBoundaryColour, InteriorColour = CellInteriorColour, &
                              Width = Width, Height = Height, Radius = RoundedRadius, ShapeType = CellShapeType)
      CALL CurrentCell%Refine(CellShape = ShapeValue)
    TYPE IS (Square)
      CALL ShapeValue%Refine(BoundaryColour = CellBoundaryColour, InteriorColour = CellInteriorColour, &
                              Side = Side, Radius = RoundedRadius, ShapeType = CellShapeType)
      CALL CurrentCell%Refine(CellShape = ShapeValue)
    TYPE IS (Ellipse)
      CALL ShapeValue%Refine(BoundaryColour = CellBoundaryColour, InteriorColour = CellInteriorColour, &
                              Width = Width, Height = Height, ShapeType = CellShapeType)
      CALL CurrentCell%Refine(CellShape = ShapeValue)  
    TYPE IS (Circle)
      CALL ShapeValue%Refine(BoundaryColour = CellBoundaryColour, InteriorColour = CellInteriorColour, &
                              Radius = Radius, ShapeType = CellShapeType)
      CALL CurrentCell%Refine(CellShape = ShapeValue)
    TYPE IS (Diamond)
      CALL ShapeValue%Refine(BoundaryColour = CellBoundaryColour, InteriorColour = CellInteriorColour, &
                              Width = Width, Height = Height, Radius = RoundedRadius, ShapeType = CellShapeType)
      CALL CurrentCell%Refine(CellShape = ShapeValue)                       
    TYPE IS (RegularPolygon)
      CALL ShapeValue%Refine(BoundaryColour = CellBoundaryColour, InteriorColour = CellInteriorColour, &
                              Radius = Radius, Sides = Sides, RoundedRadius = RoundedRadius, ShapeType = CellShapeType)
      CALL CurrentCell%Refine(CellShape = ShapeValue)                        
    TYPE IS (Cloud)
      CALL ShapeValue%Refine(BoundaryColour = CellBoundaryColour, InteriorColour = CellInteriorColour, Width = Width, &
                   Height = Height, Puffs = Puffs, PuffArc = PuffArc, ShapeType = CellShapeType) 
      CALL CurrentCell%Refine(CellShape = ShapeValue)                                                                     
    TYPE IS (SingleArrow)
      CALL ShapeValue%Refine(BoundaryColour = CellBoundaryColour, InteriorColour = CellInteriorColour, &
                              Width = Width, Height = Height, TipAngle = TipAngle, HeadExtend = HeadExtend, &
                               HeadIndent = HeadIndent, ShapeType = CellShapeType)
      CALL CurrentCell%Refine(CellShape = ShapeValue)                            
    TYPE IS (DoubleArrow)
      CALL ShapeValue%Refine(BoundaryColour = CellBoundaryColour, InteriorColour = CellInteriorColour, &
                              Width = Width, Height = Height, TipAngle = TipAngle, HeadExtend = HeadExtend, &
                               HeadIndent = HeadIndent, ShapeType = CellShapeType)                                                             
      CALL CurrentCell%Refine(CellShape = ShapeValue)              
    CLASS DEFAULT
      STOP "Invalid Shape"
  END SELECT     
  CALL CurrentBox%Refine(BoxCell = CurrentCell, BoxCellLayer = LayerIndex) 
  DEALLOCATE(CurrentShape)
END IF  

IF(PRESENT(LatexContent) .OR. PRESENT(CellXShift) .OR. PRESENT(CellYShift) .OR. PRESENT(Alignment)) THEN

  CurrentCell = CurrentBox%GetCell(LayerIndex)
  CALL CurrentCell%Refine(LatexContent = LatexContent, XShift = CellXShift, YShift = CellYShift, Alignment = Alignment)
  CALL CurrentBox%Refine(BoxCell = CurrentCell, BoxCellLayer = LayerIndex)
END IF

END SUBROUTINE RefineBox

SUBROUTINE RefineVectorBox(VectorBox, VectorRatio, VectorDirection, BoxBoundaryColour, BoxInteriorColour, CellShape, &
             CellBoundaryColour, CellInteriorColour, Width, Height, Side, RoundedRadius, Radius, TipAngle, HeadExtend, &
              HeadIndent, Sides, Puffs, PuffArc, CellShapeType, LatexContent, CellXShift, CellYShift, Alignment, Layer)

CLASS(Box),                 INTENT(INOUT) :: VectorBox(:)
REAL,             OPTIONAL, INTENT(IN)    :: VectorRatio(:)
LOGICAL,          OPTIONAL, INTENT(IN)    :: VectorDirection
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: BoxBoundaryColour
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: BoxInteriorColour
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: CellShape
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: CellBoundaryColour
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: CellInteriorColour
REAL,             OPTIONAL, INTENT(IN)    :: Width
REAL,             OPTIONAL, INTENT(IN)    :: Height
REAL,             OPTIONAL, INTENT(IN)    :: Side
REAL,             OPTIONAL, INTENT(IN)    :: RoundedRadius
REAL,             OPTIONAL, INTENT(IN)    :: Radius
REAL,             OPTIONAL, INTENT(IN)    :: TipAngle
REAL,             OPTIONAL, INTENT(IN)    :: HeadExtend
REAL,             OPTIONAL, INTENT(IN)    :: HeadIndent
INTEGER,          OPTIONAL, INTENT(IN)    :: Sides
INTEGER,          OPTIONAL, INTENT(IN)    :: Puffs
REAL,             OPTIONAL, INTENT(IN)    :: PuffArc
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: CellShapeType
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: LatexContent
REAL,             OPTIONAL, INTENT(IN)    :: CellXShift
REAL,             OPTIONAL, INTENT(IN)    :: CellYShift
INTEGER,          OPTIONAL, INTENT(IN)    :: Alignment
INTEGER,          OPTIONAL, INTENT(IN)    :: Layer

INTEGER :: i


IF(PRESENT(VectorRatio)) THEN
IF(size(VectorBox) /= size(VectorRatio) ) STOP "Box and Vector Dimensions donot Match"
IF(.NOT. PRESENT(VectorDirection)) STOP "Vector Direction should be specified for Vector Ratio" 
IF(VectorDirection) CALL RowVectorRatioBox(VectorBox, VectorRatio)
IF(.NOT. VectorDirection) CALL ColumnVectorRatioBox(VectorBox, VectorRatio)
END IF

DO i = 1, size(VectorBox)

    CALL RefineBox(VectorBox(i), BoxBoundaryColour = BoxBoundaryColour, BoxInteriorColour = BoxInteriorColour, &
          CellShape = CellShape, CellBoundaryColour = CellBoundaryColour, CellInteriorColour = CellInteriorColour, &
            Width = Width, Height = Height, Side = Side, RoundedRadius = RoundedRadius, Radius = Radius, TipAngle = TipAngle, &
             HeadExtend = HeadExtend, HeadIndent = HeadIndent, Sides = Sides, Puffs = Puffs, PuffArc = PuffArc, &
              CellShapeType = CellShapeType, LatexContent = LatexContent, CellXShift = CellXShift, CellYShift = CellYShift, &
               Alignment = Alignment, Layer = Layer)
END DO

END SUBROUTINE RefineVectorBox

SUBROUTINE RefineMatrixBox(MatrixBox, MatrixRatio, BoxBoundaryColour, BoxInteriorColour, CellShape, CellBoundaryColour, &
                    CellInteriorColour, Width, Height, Side, Radius, RoundedRadius, TipAngle, HeadExtend, HeadIndent, &
                      Sides, Puffs, PuffArc, CellShapeType, LatexContent, CellXShift, CellYShift, Alignment, Layer)

CLASS(Box),                 INTENT(INOUT) :: MatrixBox(:,:)
REAL,             OPTIONAL, INTENT(IN)    :: MatrixRatio(:,:)
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: BoxBoundaryColour
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: BoxInteriorColour
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: CellShape
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: CellBoundaryColour
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: CellInteriorColour
REAL,             OPTIONAL, INTENT(IN)    :: Width
REAL,             OPTIONAL, INTENT(IN)    :: Height
REAL,             OPTIONAL, INTENT(IN)    :: Side
REAL,             OPTIONAL, INTENT(IN)    :: Radius
REAL,             OPTIONAL, INTENT(IN)    :: RoundedRadius
REAL,             OPTIONAL, INTENT(IN)    :: TipAngle
REAL,             OPTIONAL, INTENT(IN)    :: HeadExtend
REAL,             OPTIONAL, INTENT(IN)    :: HeadIndent
INTEGER,          OPTIONAL, INTENT(IN)    :: Sides
INTEGER,          OPTIONAL, INTENT(IN)    :: Puffs
REAL,             OPTIONAL, INTENT(IN)    :: PuffArc
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: CellShapeType
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: LatexContent
REAL,             OPTIONAL, INTENT(IN)    :: CellXShift
REAL,             OPTIONAL, INTENT(IN)    :: CellYShift
INTEGER,          OPTIONAL, INTENT(IN)    :: Alignment
INTEGER,          OPTIONAL, INTENT(IN)    :: Layer

INTEGER                                   :: i, j


IF(PRESENT(MatrixRatio)) CALL MatrixRatioBox(MatrixBox, MatrixRatio)

DO i = 1, size(MatrixBox,1)
  DO j = 1, size(MatrixBox,2)
    CALL RefineBox(MatrixBox(i,j), BoxBoundaryColour = BoxBoundaryColour, BoxInteriorColour = BoxInteriorColour, &
          CellShape = CellShape, CellBoundaryColour = CellBoundaryColour, CellInteriorColour = CellInteriorColour, &
            Width = Width, Height = Height, Side = Side, RoundedRadius = RoundedRadius, Radius = Radius, TipAngle = TipAngle, &
             HeadExtend = HeadExtend, HeadIndent = HeadIndent, Sides = Sides, Puffs = Puffs, PuffArc = PuffArc, &
              CellShapeType = CellShapeType, LatexContent = LatexContent, CellXShift = CellXShift, &
               CellYShift = CellYShift, Alignment = Alignment, Layer = Layer)
  END DO
END DO

END SUBROUTINE RefineMatrixBox


SUBROUTINE DrawBox(BoxValue)

CLASS(Box), INTENT(IN)    :: BoxValue

TYPE(Cell),   POINTER     :: BoxCell
CLASS(Shape), ALLOCATABLE :: CellShape

INTEGER                   :: Layer
REAL                      :: Width, Height, Side, Radius
CHARACTER(:), ALLOCATABLE :: MinString


WRITE(100,"(A,A,A,A,A,F10.7,A,F10.7,A,F10.7,A,F10.7,A,/)") &
       "\node[draw=",BoxValue%GetBoundCol(),", rectangle,fill=",BoxValue%GetIntCol(),&
          ",minimum width =",BoxValue%GetXLim(), "\textwidth, minimum height=",BoxValue%GetYLim(),&
            "\textheight] at (",BoxValue%GetXLocus(),"\textwidth,",-BoxValue%GetYLocus(),"\textheight) {};"

DO Layer = 1, BoxValue%GetCellSize()
  BoxCell => BoxValue%GetCell(Layer)
  ALLOCATE(CellShape, SOURCE=BoxCell%GetShape()) 
  SELECT TYPE(ShapeValue => CellShape)
    TYPE IS (Rectangle)
      Width = ShapeValue%GetWidth()*BoxValue%GetXLim()
      Height = ShapeValue%GetHeight()*BoxValue%GetYLim()
      IF(Width*AspectRatio < Height) THEN  
        MinString = TextWidStr
      ELSE  
        MinString = TextHgtStr
      END IF
      WRITE(100,"(A,A,A,A,A,A,A,F10.7,A,F10.7,A,F10.7,A,A,F10.7,A,F10.7,A,I0,A,/)") &
       "\node[",TRIM(ShapeValue%GetType()),",draw=",ShapeValue%GetBoundCol(),",rectangle,fill=",ShapeValue%GetIntCol(),&
        ",minimum width =",Width,"\textwidth, minimum height=",Height,"\textheight,rounded corners =",&
          ShapeValue%GetRadius()*MIN(Width/2, Height/2),MinString,"] at (",&
            BoxValue%GetXLocus() + BoxCell%GetXShift()*(BoxValue%GetXLim()/2 - ShapeValue%GetWidth()*BoxValue%GetXLim()/2),&
            "\textwidth,",&
             -BoxValue%GetYLocus() - BoxCell%GetYShift()*(BoxValue%GetYLim()/2 - ShapeValue%GetHeight()*BoxValue%GetYLim()/2),&
               "\textheight) (C",BoxValue%GetID(), ") {};" 
      WRITE(100,"(A,A,A,F10.7,A,F10.7,A,F10.7,A,F10.7,A,I0,A,A,A,/)") &
       "\node[font=\footnotesize,rectangle,align =",BoxCell%GetAlignStr(),",text width =",.9*Width,&
          "\textwidth, inner xsep = 0, minimum height=",&
           Height,"\textheight] at (",&
            BoxValue%GetXLocus() + BoxCell%GetXShift()*(BoxValue%GetXLim()/2 - ShapeValue%GetWidth()*BoxValue%GetXLim()/2),&
            "\textwidth,",&
             -BoxValue%GetYLocus() - BoxCell%GetYShift()*(BoxValue%GetYLim()/2 - ShapeValue%GetHeight()*BoxValue%GetYLim()/2),&
               "\textheight) (CT",BoxValue%GetID(), ") {",BoxCell%GetContent(),"};"            
    TYPE IS (Square)
      Width = ShapeValue%GetSide()*BoxValue%GetXLim()
      Height = ShapeValue%GetSide()*BoxValue%GetYLim() 
      IF(Width*AspectRatio < Height) THEN  
        Side = Width
        MinString = TextWidStr
      ELSE  
        Side = Height 
        MinString = TextHgtStr
      END IF  
      WRITE(100,"(A,A,A,A,A,A,A,F10.7,A,A,F10.7,A,A,F10.7,A,A,F10.7,A,F10.7,A,I0,A,/)") &
       "\node[",TRIM(ShapeValue%GetType()),",draw=",ShapeValue%GetBoundCol(),",rectangle,fill=",ShapeValue%GetIntCol(),&
         ",minimum width =",Side,MinString,", minimum height=", Side,MinString,",rounded corners =",&
           ShapeValue%GetRadius()*Side/2,MinString,"] at (",&
            BoxValue%GetXLocus() + BoxCell%GetXShift()*(BoxValue%GetXLim()/2 - ShapeValue%GetSide()*BoxValue%GetXLim()/2),&
            "\textwidth,",&
             -BoxValue%GetYLocus() - BoxCell%GetYShift()*(BoxValue%GetYLim()/2 - ShapeValue%GetSide()*BoxValue%GetYLim()/2),&
               "\textheight) (C",BoxValue%GetID(), ") {};" 
      WRITE(100,"(A,A,A,F10.7,A,A,F10.7,A,A,F10.7,A,F10.7,A,I0,A,A,A,/)") &
       "\node[font=\footnotesize,rectangle,align =",BoxCell%GetAlignStr(),",text width =",.9*Side,MinString,&
        ", inner xsep = 0, minimum height=", Side,MinString,"] at (",&
            BoxValue%GetXLocus() + BoxCell%GetXShift()*(BoxValue%GetXLim()/2 - ShapeValue%GetSide()*BoxValue%GetXLim()/2),&
            "\textwidth,",&
             -BoxValue%GetYLocus() - BoxCell%GetYShift()*(BoxValue%GetYLim()/2 - ShapeValue%GetSide()*BoxValue%GetYLim()/2),&
               "\textheight) (CT",BoxValue%GetID(), ") {",BoxCell%GetContent(),"};"                 
    TYPE IS (Ellipse)
      Width = ShapeValue%GetWidth()*BoxValue%GetXLim()
      Height = ShapeValue%GetHeight()*BoxValue%GetYLim() 
      WRITE(100,"(A,A,A,A,A,A,A,F10.7,A,F10.7,A,F10.7,A,F10.7,A,I0,A,/)") &
        "\node[",TRIM(ShapeValue%GetType()),",draw=",ShapeValue%GetBoundCol(),",ellipse,fill=",ShapeValue%GetIntCol(),&
          ",minimum width =",Width,"\textwidth, minimum height=",Height,"\textheight] at (",&
            BoxValue%GetXLocus() + BoxCell%GetXShift()*(BoxValue%GetXLim()/2 - ShapeValue%GetWidth()*BoxValue%GetXLim()/2),&
            "\textwidth,",&
             -BoxValue%GetYLocus() - BoxCell%GetYShift()*(BoxValue%GetYLim()/2 - ShapeValue%GetHeight()*BoxValue%GetYLim()/2),&
               "\textheight) (C",BoxValue%GetID(), ") {};"
               
      WRITE(100,"(A,A,A,F10.7,A,F10.7,A,F10.7,A,F10.7,A,I0,A,A,A,/)") &
        "\node[font=\footnotesize,ellipse,align =",BoxCell%GetAlignStr(),",text width =",.75*Width,&
          "\textwidth, inner xsep = 0, minimum height=",&
           Height,"\textheight] at (",&
            BoxValue%GetXLocus() + BoxCell%GetXShift()*(BoxValue%GetXLim()/2 - ShapeValue%GetWidth()*BoxValue%GetXLim()/2),&
            "\textwidth,",&
             -BoxValue%GetYLocus() - BoxCell%GetYShift()*(BoxValue%GetYLim()/2 - ShapeValue%GetHeight()*BoxValue%GetYLim()/2),&
               "\textheight) (CT",BoxValue%GetID(), ") {",BoxCell%GetContent(),"};"           
    TYPE IS (Circle)
      Width = ShapeValue%GetRadius()*BoxValue%GetXLim()
      Height = ShapeValue%GetRadius()*BoxValue%GetYLim() 
      IF(Width*AspectRatio < Height) THEN  
        Radius = Width
        MinString = TextWidStr
      ELSE  
        Radius = Height
        MinString = TextHgtStr
      END IF  
      WRITE(100,"(A,A,A,A,A,A,A,F10.7,A,A,F10.7,A,A,F10.7,A,F10.7,A,I0,A,/)") &
       "\node[",TRIM(ShapeValue%GetType()),",draw=",ShapeValue%GetBoundCol(),",circle,fill=",&
         ShapeValue%GetIntCol(),",minimum width =",Radius,MinString,&
         ",minimum height=",&
           Radius,MinString,"] at (",&
            BoxValue%GetXLocus() + BoxCell%GetXShift()*(BoxValue%GetXLim()/2 - ShapeValue%GetRadius()*BoxValue%GetXLim()/2),&
            "\textwidth,",&
             -BoxValue%GetYLocus() - BoxCell%GetYShift()*(BoxValue%GetYLim()/2 - ShapeValue%GetRadius()*BoxValue%GetYLim()/2),&
               "\textheight) (C",BoxValue%GetID(), ") {};" 
      WRITE(100,"(A,A,A,A,A,F10.7,A,A,F10.7,A,A,F10.7,A,F10.7,A,I0,A,A,A,/)") &
       "\node[font=\footnotesize,circle,fill=",ShapeValue%GetIntCol(),",align =",&
        BoxCell%GetAlignStr(),",text width =",.75*Radius,MinString,", inner xsep = 0, minimum height=",Radius,MinString,"] at (",&
            BoxValue%GetXLocus() + BoxCell%GetXShift()*(BoxValue%GetXLim()/2 - ShapeValue%GetRadius()*BoxValue%GetXLim()/2),&
            "\textwidth,",&
             -BoxValue%GetYLocus() - BoxCell%GetYShift()*(BoxValue%GetYLim()/2 - ShapeValue%GetRadius()*BoxValue%GetYLim()/2),&
               "\textheight) (CT",BoxValue%GetID(), ") {",BoxCell%GetContent(),"};"              
    TYPE IS (Diamond)
      Width = ShapeValue%GetWidth()*BoxValue%GetXLim()
      Height = ShapeValue%GetHeight()*BoxValue%GetYLim()
      IF(Width*AspectRatio < Height) THEN  
        MinString = TextWidStr
      ELSE  
        MinString = TextHgtStr
      END IF
      WRITE(100,"(A,A,A,A,A,A,A,F10.7,A,F10.7,A,F10.7,A,A,F10.7,A,F10.7,A,I0,A,/)") &
       "\node[",TRIM(ShapeValue%GetType()),",draw=",ShapeValue%GetBoundCol(),",diamond,fill=",&
         ShapeValue%GetIntCol(),",minimum width =",Width,"\textwidth, minimum height=",&
           Height,"\textheight,rounded corners =",ShapeValue%GetRadius()*MIN(Width/2, Height/2),MinString,"] at (",&
            BoxValue%GetXLocus() + BoxCell%GetXShift()*(BoxValue%GetXLim()/2 - ShapeValue%GetWidth()*BoxValue%GetXLim()/2),&
            "\textwidth,",&
             -BoxValue%GetYLocus() - BoxCell%GetYShift()*(BoxValue%GetYLim()/2 - ShapeValue%GetHeight()*BoxValue%GetYLim()/2),&
               "\textheight) (C",BoxValue%GetID(), ") {};" 
               
     WRITE(100,"(A,A,A,F10.7,A,F10.7,A,F10.7,A,F10.7,A,I0,A,A,A,/)") &
       "\node[font=\footnotesize,diamond,align =",BoxCell%GetAlignStr(),",text width =",.5*Width,&
          "\textwidth, inner xsep = 0, minimum height=",&
           Height,"\textheight] at (",&
            BoxValue%GetXLocus() + BoxCell%GetXShift()*(BoxValue%GetXLim()/2 - ShapeValue%GetWidth()*BoxValue%GetXLim()/2),&
            "\textwidth,",&
             -BoxValue%GetYLocus() - BoxCell%GetYShift()*(BoxValue%GetYLim()/2 - ShapeValue%GetHeight()*BoxValue%GetYLim()/2),&
               "\textheight) (CT",BoxValue%GetID(), ") {",BoxCell%GetContent(),"};" 
    TYPE IS (RegularPolygon)
      Width = ShapeValue%GetRadius()*BoxValue%GetXLim()
      Height = ShapeValue%GetRadius()*BoxValue%GetYLim() 
      IF(Width*AspectRatio < Height) THEN  
        Radius = Width
        MinString = TextWidStr
      ELSE  
        Radius = Height 
        MinString = TextHgtStr
      END IF  
      WRITE(100,"(A,A,A,A,A,A,A,I0,A,F10.7,A,A,F10.7,A,A,F10.7,A,A,F10.7,A,F10.7,A,I0,A,/)") &
       "\node[",TRIM(ShapeValue%GetType()),",draw=",ShapeValue%GetBoundCol(),",regular polygon,fill=",ShapeValue%GetIntCol(),&
         ",regular polygon sides= ",ShapeValue%GetSides(),",minimum width =",Radius,MinString,",minimum height=", Radius,MinString,&
         ",rounded corners =",ShapeValue%GetRoundedRadius()*Radius/2,MinString,"] at (",&
            BoxValue%GetXLocus() + BoxCell%GetXShift()*(BoxValue%GetXLim()/2 - ShapeValue%GetRadius()*BoxValue%GetXLim()/2),&
            "\textwidth,",&
             -BoxValue%GetYLocus() - BoxCell%GetYShift()*(BoxValue%GetYLim()/2 - ShapeValue%GetRadius()*BoxValue%GetYLim()/2),&
               "\textheight) (C",BoxValue%GetID(), ") {};" 
      WRITE(100,"(A,A,A,I0,A,F10.7,A,A,F10.7,A,A,F10.7,A,F10.7,A,I0,A,A,A,/)") &
       "\node[font=\footnotesize,regular polygon,align =",BoxCell%GetAlignStr(),",regular polygon sides= ",ShapeValue%GetSides(),&
        ",text width =",.5*Radius,MinString,", inner xsep = 0, minimum height=", Radius,MinString,"] at (",&
            BoxValue%GetXLocus() + BoxCell%GetXShift()*(BoxValue%GetXLim()/2 - ShapeValue%GetRadius()*BoxValue%GetXLim()/2),&
            "\textwidth,",&
             -BoxValue%GetYLocus() - BoxCell%GetYShift()*(BoxValue%GetYLim()/2 - ShapeValue%GetRadius()*BoxValue%GetYLim()/2),&
               "\textheight) (CT",BoxValue%GetID(), ") {",BoxCell%GetContent(),"};" 
    TYPE IS (Cloud)
      Width = ShapeValue%GetWidth()*BoxValue%GetXLim()
      Height = ShapeValue%GetHeight()*BoxValue%GetYLim() 
      WRITE(100,"(A,A,A,A,A,A,A,I0,A,F10.5,A,F10.7,A,F10.7,A,F10.7,A,F10.7,A,I0,A,/)") &
        "\node[",TRIM(ShapeValue%GetType()),",draw=",ShapeValue%GetBoundCol(),",cloud,fill=",ShapeValue%GetIntCol(),&
          ",cloud puffs = ",ShapeValue%GetPuffs(),",cloud puff arc= ",ShapeValue%GetArc(),",minimum width =",Width,&
           "\textwidth, minimum height=",Height,"\textheight] at (",&
            BoxValue%GetXLocus() + BoxCell%GetXShift()*(BoxValue%GetXLim()/2 - ShapeValue%GetWidth()*BoxValue%GetXLim()/2),&
            "\textwidth,",&
             -BoxValue%GetYLocus() - BoxCell%GetYShift()*(BoxValue%GetYLim()/2 - ShapeValue%GetHeight()*BoxValue%GetYLim()/2),&
               "\textheight) (C",BoxValue%GetID(), ") {};"
               
      WRITE(100,"(A,A,A,F10.7,A,F10.7,A,F10.7,A,F10.7,A,I0,A,A,A,/)") &
        "\node[font=\footnotesize,cloud,align =",BoxCell%GetAlignStr(),",text width =",.33*Width,&
          "\textwidth, inner xsep = 0, minimum height=",&
           Height,"\textheight] at (",&
            BoxValue%GetXLocus() + BoxCell%GetXShift()*(BoxValue%GetXLim()/2 - ShapeValue%GetWidth()*BoxValue%GetXLim()/2),&
            "\textwidth,",&
             -BoxValue%GetYLocus() - BoxCell%GetYShift()*(BoxValue%GetYLim()/2 - ShapeValue%GetHeight()*BoxValue%GetYLim()/2),&
               "\textheight) (CT",BoxValue%GetID(), ") {",BoxCell%GetContent(),"};"         
    TYPE IS (SingleArrow)
      Height = ShapeValue%GetWidth()*BoxValue%GetXLim()
      Width = ShapeValue%GetHeight()*BoxValue%GetYLim()
      WRITE(100,"(A,A,A,A,A,A,A,F10.7,A,F10.7,A,F10.7,A,F10.7,A,F10.7,A,F10.7,A,F10.7,A,I0,A,/)") &
       "\node[",TRIM(ShapeValue%GetType()),",draw=",ShapeValue%GetBoundCol(),",single arrow,fill=",&
         ShapeValue%GetIntCol(),",minimum width =",Height,"\textheight, minimum height=",Width,&
         "\textwidth, single arrow tip angle =",ShapeValue%GetAngle(),",single arrow head extend=",ShapeValue%GetExtend()*Width/4,&
            "\textwidth,single arrow head indent =",ShapeValue%GetIndent()*Height,"\textheight] at (",&
            BoxValue%GetXLocus() + BoxCell%GetXShift()*(BoxValue%GetXLim()/2 - ShapeValue%GetWidth()*BoxValue%GetXLim()/2),&
            "\textwidth,",&
             -BoxValue%GetYLocus() - BoxCell%GetYShift()*(BoxValue%GetYLim()/2 - ShapeValue%GetHeight()*BoxValue%GetYLim()/2),&
               "\textheight) (C",BoxValue%GetID(), ") {};" 
               
     WRITE(100,"(A,A,A,F10.7,A,F10.7,A,F10.7,A,F10.7,A,I0,A,A,A,/)") &
       "\node[font=\footnotesize,single arrow,align =",BoxCell%GetAlignStr(),",text width =",.5*Height,&
          "\textheight, inner xsep = 0, minimum height=",&
           Width,"\textwidth] at (",&
            BoxValue%GetXLocus() + BoxCell%GetXShift()*(BoxValue%GetXLim()/2 - ShapeValue%GetWidth()*BoxValue%GetXLim()/2),&
            "\textwidth,",&
             -BoxValue%GetYLocus() - BoxCell%GetYShift()*(BoxValue%GetYLim()/2 - ShapeValue%GetHeight()*BoxValue%GetYLim()/2),&
               "\textheight) (CT",BoxValue%GetID(),") {",BoxCell%GetContent(),"};"  
    TYPE IS (DoubleArrow)
      Height = ShapeValue%GetWidth()*BoxValue%GetXLim()
      Width = ShapeValue%GetHeight()*BoxValue%GetYLim()
      WRITE(100,"(A,A,A,A,A,A,A,F10.7,A,F10.7,A,F10.7,A,F10.7,A,F10.7,A,F10.7,A,F10.7,A,I0,A,/)") &
       "\node[",TRIM(ShapeValue%GetType()),",draw=",ShapeValue%GetBoundCol(),",double arrow,fill=",&
         ShapeValue%GetIntCol(),",minimum width =",Height,"\textheight, minimum height=",Width,&
         "\textwidth, double arrow tip angle =",ShapeValue%GetAngle(),",double arrow head extend=",ShapeValue%GetExtend()*Width/4,&
            "\textwidth,double arrow head indent =",ShapeValue%GetIndent()*Height,"\textheight] at (",&
            BoxValue%GetXLocus() + BoxCell%GetXShift()*(BoxValue%GetXLim()/2 - ShapeValue%GetWidth()*BoxValue%GetXLim()/2),&
            "\textwidth,",&
             -BoxValue%GetYLocus() - BoxCell%GetYShift()*(BoxValue%GetYLim()/2 - ShapeValue%GetHeight()*BoxValue%GetYLim()/2),&
               "\textheight) (C",BoxValue%GetID(), ") {};" 
               
     WRITE(100,"(A,A,A,F10.7,A,F10.7,A,F10.7,A,F10.7,A,I0,A,A,A,/)") &
       "\node[font=\footnotesize,double arrow,align =",BoxCell%GetAlignStr(),",text width =",.5*Height,&
          "\textheight, inner xsep = 0, minimum height=",&
           Width,"\textwidth] at (",&
            BoxValue%GetXLocus() + BoxCell%GetXShift()*(BoxValue%GetXLim()/2 - ShapeValue%GetWidth()*BoxValue%GetXLim()/2),&
            "\textwidth,",&
             -BoxValue%GetYLocus() - BoxCell%GetYShift()*(BoxValue%GetYLim()/2 - ShapeValue%GetHeight()*BoxValue%GetYLim()/2),&
            "\textheight) (CT",BoxValue%GetID(),") {",BoxCell%GetContent(),"};"                                                                           
    CLASS DEFAULT
      STOP "Invalid Shape"
  END SELECT  
       
DEALLOCATE(CellShape)

END DO

END SUBROUTINE DrawBox

SUBROUTINE DrawVectorBox(VectorBox)

CLASS(Box), INTENT(IN)    :: VectorBox(:)

INTEGER :: i


DO i = 1, size(VectorBox)
  CALL DrawBox(VectorBox(i))
END DO

END SUBROUTINE DrawVectorBox

SUBROUTINE DrawMatrixBox(MatrixBox)

CLASS(Box), INTENT(IN)    :: MatrixBox(:,:)

INTEGER :: i, j


DO i = 1, size(MatrixBox,1)
  DO j = 1, size(MatrixBox,2)
    CALL DrawBox(MatrixBox(i,j))
  END DO
END DO

END SUBROUTINE DrawMatrixBox


SUBROUTINE Link(Arr, ArrowType, Path, Width, ArrowColour, Angle, StartPoint, EndPoint)

TYPE(Arrow),                INTENT(INOUT) :: Arr
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: ArrowType
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: Path
REAL,             OPTIONAL, INTENT(IN)    :: Width
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: ArrowColour 
REAL,             OPTIONAL, INTENT(IN)    :: Angle
REAL,             OPTIONAL, INTENT(IN)    :: StartPoint
REAL,             OPTIONAL, INTENT(IN)    :: EndPoint


CALL Arr%Set()
IF(PRESENT(ArrowType))   CALL Arr%Refine(ArrowType = ArrowType)
IF(PRESENT(Path))        CALL Arr%Refine(Path = Path)
IF(PRESENT(Width))       CALL Arr%Refine(Width = Width)
IF(PRESENT(ArrowColour)) CALL Arr%Refine(ArrowColour = ArrowColour)
IF(PRESENT(Angle))       CALL Arr%Refine(Angle = Angle)
IF(PRESENT(StartPoint))  CALL Arr%Refine(StartPoint = StartPoint)
IF(PRESENT(EndPoint))    CALL Arr%Refine(EndPoint = EndPoint)

END SUBROUTINE Link

SUBROUTINE DrawLink(FromBox, ToBox, CurrentArrow)

TYPE(Box),             INTENT(IN)    :: FromBox
TYPE(Box),             INTENT(IN)    :: ToBox
TYPE(Arrow), OPTIONAL, INTENT(INOUT) :: CurrentArrow

TYPE(Cell)                        :: FromBoxCell, ToBoxCell


CALL CurrentArrow%Set()


FromBoxCell =  FromBox%GetCell()
ToBoxCell   =  ToBox%GetCell()

WRITE(100,"(A,F7.3,A,A,A,F7.3,A,A,A,I0,A,A,A,A,I0,A,A,/)") &
       "\path[bend left =",CurrentArrow%GetAngle(),",",CurrentArrow%GetType(),",line width=",CurrentArrow%GetWidth(),",draw=",&
            CurrentArrow%GetColour(),"] (C",FromBox%GetID(),TRIM(CurrentArrow%GetStart()),&
              ") ",CurrentArrow%GetPath()," (C",ToBox%GetID(),TRIM(CurrentArrow%GetEnd()),");"

END SUBROUTINE DrawLink


SUBROUTINE RowVectorRatioBox(CurrentBox, Vector)

TYPE(Box), INTENT(INOUT) :: CurrentBox(:)
REAL,      INTENT(IN)    :: Vector(:) 

INTEGER                  :: i
REAL                     :: ParentXLimit = 0., ParentXLocus = 0., ChildXLimit, ChildYLimit, ChildXLocus, ChildYLocus
REAL                     :: NormalizeVector(size(Vector)) 


ChildYLimit = CurrentBox(1)%GetYLim()
DO i = 1, size(CurrentBox)
  ParentXLimit = ParentXLimit + CurrentBox(i)%GetXLim() 
  IF(ChildYLimit /= CurrentBox(i)%GetYLim()) STOP "Different YLimit Boxes for Row Vector Ratio"
END DO
  
ParentXLocus = ParentXLimit/2
ChildYLocus = ChildYLimit/2
 
NormalizeVector = Vector/SUM(Vector(:))
 
DO i = 1, size(CurrentBox)  

  ChildXLimit = ParentXLimit*NormalizeVector(i)
        
  ChildXLocus = ParentXLocus - ParentXLimit/2 + SUM(NormalizeVector(:i-1))*ParentXLimit + NormalizeVector(i)*ParentXLimit/2
     
  CALL CurrentBox(i)%Refine(ID = CurrentBox(i)%GetID(), XLocus = ChildXLocus, YLocus = ChildYLocus, &
                XLimit = ChildXLimit, YLimit = ChildYLimit)
     
END DO

END SUBROUTINE RowVectorRatioBox

SUBROUTINE ColumnVectorRatioBox(CurrentBox, Vector)

TYPE(Box), INTENT(INOUT) :: CurrentBox(:)
REAL,      INTENT(IN)    :: Vector(:)  

INTEGER                  :: i
REAL                     :: ParentYLimit = 0., ParentYLocus = 0., ChildXLimit, ChildYLimit, ChildXLocus, ChildYLocus
REAL                     :: NormalizeVector(size(Vector))


ChildXLimit = CurrentBox(1)%GetXLim()
DO i = 1, size(CurrentBox)
  ParentYLimit = ParentYLimit + CurrentBox(i)%GetYLim() 
  IF(ChildXLimit /= CurrentBox(i)%GetXLim()) STOP "Different XLimit Boxes for Column Vector Ratio"
END DO
  
ChildXLocus = ChildXLimit/2
ParentYLocus = ParentYLimit/2
 
NormalizeVector = Vector/SUM(Vector(:))
 
DO i = 1, size(CurrentBox)  

  ChildYLimit = ParentYLimit*NormalizeVector(i)
        
  ChildYLocus = ParentYLocus - ParentYLimit/2 + SUM(NormalizeVector(:i-1))*ParentYLimit + NormalizeVector(i)*ParentYLimit/2
     
  CALL CurrentBox(i)%Refine(ID = CurrentBox(i)%GetID(), XLocus = ChildXLocus, YLocus = ChildYLocus, &
                XLimit = ChildXLimit, YLimit = ChildYLimit)
     
END DO

END SUBROUTINE ColumnVectorRatioBox

SUBROUTINE MatrixRatioBox(CurrentBox, Matrix)

TYPE(Box),    INTENT(INOUT) :: CurrentBox(:,:)
REAL,         INTENT(IN)    :: Matrix(:,:)

INTEGER                     :: i, j
REAL                        :: ParentXLimit = 0., ParentYLimit = 0., ParentXLocus = 0., ParentYLocus = 0., &
                                ChildXLimit, ChildYLimit, ChildXLocus, ChildYLocus
REAL                        :: NormalizeRow(size(Matrix,2)), NormalizeColumn(size(Matrix,1)) 


IF(size(CurrentBox,1) /= size(Matrix,1) .AND. size(CurrentBox,2) /= size(Matrix,2)) STOP "Box and Matrix Dimensions donot Match"

DO j = 1, size(CurrentBox,2)
  ParentXLimit = ParentXLimit + CurrentBox(1,j)%GetXLim()  
END DO
DO i = 1, size(CurrentBox,1)
  ParentYLimit = ParentYLimit + CurrentBox(i,1)%GetYLim()
END DO
  
ParentXLocus = ParentXLimit/2
ParentYLocus = ParentYLimit/2

CALL Normalize(Matrix, NormalizeRow, NormalizeColumn)

DO i = 1, size(CurrentBox,1)  
  DO j = 1, size(CurrentBox,2)

    ChildXLimit = ParentXLimit*NormalizeRow(j)
    ChildYLimit = ParentYLimit*NormalizeColumn(i)
        
    ChildXLocus = ParentXLocus - ParentXLimit/2 + SUM(NormalizeRow(:j-1))*ParentXLimit + NormalizeRow(j)*ParentXLimit/2
    ChildYLocus = ParentYLocus - ParentYLimit/2 + SUM(NormalizeColumn(:i-1))*ParentYLimit + NormalizeColumn(i)*ParentYLimit/2 
      
    CALL CurrentBox(i,j)%Set(ID = CurrentBox(i,j)%GetID(), XLocus = ChildXLocus, YLocus = ChildYLocus, &
                XLimit = ChildXLimit, YLimit = ChildYLimit)
     
  END DO
END DO


CONTAINS

SUBROUTINE Normalize(Matrix, NormalizeRow, NormalizeColumn)

REAL, INTENT(IN)  :: Matrix(:,:)
REAL, INTENT(OUT) :: NormalizeRow(size(Matrix,2)), NormalizeColumn(size(Matrix,1))

INTEGER           :: i, j
REAL              :: tolerance = 0.000001  


NormalizeRow = Matrix(1,:)/SUM(Matrix(1,:))
DO i = 2, size(Matrix,1) 
  IF(ANY(ABS(Matrix(i,:)/SUM(Matrix(i,:)) - NormalizeRow) > tolerance)) STOP "Row Non-Normalizable Matrix Input"
END DO

NormalizeColumn = Matrix(:,1)/SUM(Matrix(:,1))
DO j = 2, size(Matrix,2)
  IF(ANY(ABS(Matrix(:,j)/SUM(Matrix(:,j)) - NormalizeColumn) > tolerance)) STOP "Column Non-Normalizable Matrix Input"
END DO

END SUBROUTINE Normalize

END SUBROUTINE MatrixRatioBox


END MODULE FORTRANTekz
