PROGRAM testShapeMod
USE ShapeMod, ONLY : Shape, Rectangle, Square, Ellipse, RoundedRectangle, Diamond, &
                      DefaultRectangle, DefaultSquare, DefaultEllipse, DefaultCircle, DefaultRoundedRectangle, DefaultDiamond
IMPLICIT NONE


TYPE(Rectangle) :: SR


CALL SR%Init()
SR = DefaultRectangle 

CALL SR%Display()

END PROGRAM testShapeMod
