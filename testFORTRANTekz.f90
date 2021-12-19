INCLUDE 'TekzDiag.FOR'

PROGRAM testFORTRANTikz
USE FORTRANTekz
IMPLICIT NONE

CALL TekzDiag()

CALL SYSTEM ( " cd "//TRIM(LatexDir)//"; pdflatex -output-directory=../"// &
                TRIM(PDFDir)//" "//TRIM(RootTexFile) // ".tex > texput.log " )
CALL SYSTEM ( " cd ./"//TRIM(PDFDir)//"; find . ! -name '*.pdf' -type f -exec rm -f {} +" )
CALL SYSTEM ( " gio open ./"// TRIM(PDFDir) //"/"// TRIM( RootTexFile ) // ".pdf " )

END PROGRAM testFORTRANTikz
