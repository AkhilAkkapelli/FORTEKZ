SUBROUTINE TekzDiag
USE BoxMod,      ONLY : Box
USE FORTRANTekz, ONLY : A4Page, Beamer, StartTekzDiag, EndTekzDiag, Divide, Refine, Draw, DrawLink
IMPLICIT NONE

TYPE(Box) :: A(1,1), B(2,3), C(4,4)

INTEGER :: i, j


CALL StartTekzDiag(A(1,1), Document = Beamer)

CALL Refine(A(1,1), BoxBoundaryColour = "red", CellBoundaryColour = "blue", CellShape = "None")

CALL Draw(A)

CALL Divide(B, A(1,1))

CALL Refine(B, MatrixRatio = RESHAPE((/1.,2., 2.,4., 3.,6./), SHAPE(B)), LatexContent = "OK", CellShape = "Ellipse")
CALL Refine(B(1,:), VectorRatio = (/1., 2., 1./), VectorDirection = .TRUE., BoxBoundaryColour = "blue")

CALL Draw(B)

CALL Link(B(1,1), B(2,1), Angle = 60., ArrowColour = "blue")

CALL Draw()

CALL Divide(C, B(2,2))

CALL Draw(C)


CALL EndTekzDiag()

END SUBROUTINE TekzDiag
