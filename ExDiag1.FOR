SUBROUTINE TekzDiag
USE BoxMod,      ONLY : Box
USE FORTRANTekz, ONLY : A4Page, Beamer, StartTekzDiag, EndTekzDiag, Divide, Refine, Draw, DrawLink
IMPLICIT NONE

TYPE(Box) :: Main(1,1), Blocks(1,2), Elements(3,1)

INTEGER :: i


CALL StartTekzDiag(Main(1,1), Document = Beamer, FrameTitle = "Examples -- Vertical Array of Connected Elements")

CALL Refine(Main(1,1), CellShape = "None")

CALL Draw(Main)


CALL Divide(Blocks, Main(1,1))

CALL Refine(Blocks, Width = 1., Height = 1.)
CALL Refine(Blocks(1,1), CellInteriorColour = "white", CellShapeType = "blur shadow")
CALL Refine(Blocks(1,2), CellShape = "None", LatexContent = "\input{Examples/Diag1Code}")

CALL Draw(Blocks)


CALL Divide(Elements, Blocks(1,1))

CALL Refine(Elements, Width = .5, Height = .25)
CALL Refine(Elements(1,1), LatexContent = "Element 1")
CALL Refine(Elements(2,1), LatexContent = "Element 2")
CALL Refine(Elements(3,1), LatexContent = "Element 3")

CALL Draw(Elements)

DO i = 1, size(Elements,1) -1
  CALL DrawLink(Elements(i,1), Elements(i +1, 1))
END DO

CALL EndTekzDiag()

END SUBROUTINE TekzDiag
