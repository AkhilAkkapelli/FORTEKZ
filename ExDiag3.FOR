SUBROUTINE TekzDiag
USE BoxMod,      ONLY : Box
USE FORTRANTekz, ONLY : A4Page, Beamer, StartTekzDiag, EndTekzDiag, Divide, Refine, Draw, DrawLink
IMPLICIT NONE

TYPE(Box) :: Main(1,1), Blocks(1,2), Elements(5,1)

INTEGER :: i


CALL StartTekzDiag(Main(1,1), Document = Beamer, FrameTitle = "Examples -- Vertical Classification of Elements")

CALL Refine(Main(1,1), CellShape = "None")

CALL Draw(Main)


CALL Divide(Blocks, Main(1,1))

CALL Refine(Blocks, Width = 1., Height = 1.)
CALL Refine(Blocks(1,1), CellInteriorColour = "white", CellShapeType = "blur shadow")
CALL Refine(Blocks(1,2), CellShape = "None", LatexContent = "\input{Diagrams/ExDiag3Code}")

CALL Draw(Blocks)


CALL Divide(Elements, Blocks(1,1))

CALL Refine(Elements, MatrixRatio = RESHAPE((/1.5 , 1., 1., 1., 1./), SHAPE(Elements)), Width = .5, Height = .75)
CALL Refine(Elements(2:,1),CellXShift = .5)
CALL Refine(Elements(1,1),CellXShift = -.5, LatexContent = "Head")
CALL Refine(Elements(2,1), LatexContent = "Element 1")
CALL Refine(Elements(3,1), LatexContent = "Element 2")
CALL Refine(Elements(4,1), LatexContent = "Element 3")
CALL Refine(Elements(5,1), LatexContent = "Element 4")

CALL Draw(Elements)

DO i = 2, size(Elements,1)
  CALL DrawLink(Elements(1,1), Elements(i, 1), Path = "|-", StartPoint = 225.)
END DO

CALL EndTekzDiag()

END SUBROUTINE TekzDiag
