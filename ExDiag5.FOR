SUBROUTINE TekzDiag
USE BoxMod,      ONLY : Box
USE FORTRANTekz, ONLY : A4Page, Beamer, StartTekzDiag, EndTekzDiag, Divide, Refine, Draw, DrawLink
IMPLICIT NONE

TYPE(Box) :: Main(1,1), Blocks(1,2), ElementClasses(2,1), ChildElements(1,3)

INTEGER :: i


CALL StartTekzDiag(Main(1,1), Document = Beamer, FrameTitle = "Examples -- Horizontal Classification of Circular Elements")

CALL Refine(Main(1,1), CellShape = "None")

CALL Draw(Main)


CALL Divide(Blocks, Main(1,1))

CALL Refine(Blocks, Width = 1., Height = 1.)
CALL Refine(Blocks(1,1), CellInteriorColour = "white", CellShapeType = "blur shadow")
CALL Refine(Blocks(1,2), CellShape = "None", LatexContent = "\input{Diagrams/ExDiag3Code}")

CALL Draw(Blocks)


CALL Divide(ElementClasses, Blocks(1,1))

CALL Refine(ElementClasses(1,1), LatexContent = "HeadClass", CellShape = "Circle", Radius = .5)
CALL Refine(ElementClasses(2,1), CellShape = "None")
CALL Draw(ElementClasses)


CALL Divide(ChildElements, ElementClasses(2,1))

CALL Refine(ChildElements, CellShape = "Circle", Radius= .75)
CALL Refine(ChildElements(1,1), LatexContent = "Element 1")
CALL Refine(ChildElements(1,2), LatexContent = "Element 2")
CALL Refine(ChildElements(1,3), LatexContent = "Element 3")
CALL Draw(ChildElements)

DO i = 1, size(ChildElements,2)
  CALL DrawLink(ElementClasses(1,1), ChildElements(1, i), StartPoint = 270.)
END DO

CALL EndTekzDiag()

END SUBROUTINE TekzDiag
