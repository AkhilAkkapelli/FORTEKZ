SUBROUTINE TekzDiag
USE BoxMod,      ONLY : Box
USE FORTRANTekz, ONLY : A4Page, Beamer, StartTekzDiag, EndTekzDiag, Divide, Refine, Draw, DrawLink
IMPLICIT NONE

TYPE(Box) :: Main(1,1), Blocks(1,2), Elements(1,3)

INTEGER :: i


CALL StartTekzDiag(Main(1,1), Document = Beamer, FrameTitle = "Examples -- Horizontal Array of Connected Elements")

CALL Refine(Main(1,1), CellShape = "None")

CALL Draw(Main)


CALL Divide(Blocks, Main(1,1))

CALL Refine(Blocks, Width = 1., Height = 1.)
CALL Refine(Blocks(1,1), CellInteriorColour = "white", CellShapeType = "blur shadow")
CALL Refine(Blocks(1,2), CellShape = "None", LatexContent = "\input{Diagrams/ExDiag2Code}")

CALL Draw(Blocks)


CALL Divide(Elements, Blocks(1,1))

CALL Refine(Elements, Width = .5, Height = .75)
CALL Refine(Elements(1,1), LatexContent = "Element 3")
CALL Refine(Elements(1,2), LatexContent = "Element 2")
CALL Refine(Elements(1,3), LatexContent = "Element 1")

CALL Draw(Elements)

DO i = size(Elements,2), 2, -1
  CALL DrawLink(Elements(1,i), Elements(1, i-1))
END DO

CALL EndTekzDiag()

END SUBROUTINE TekzDiag
