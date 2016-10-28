
' MOLEWT.BAS - 07-09-90, resurrected 2016-1002

_TITLE "Molecular Weight and % Composition"
declare sub align (Tcolor, Trow, mode&, txt$)
DECLARE SUB TWOHLBOX (lcol%, rcol%, trow%, brow%, fore%, back%)
DECLARE SUB TWOLINEBOX (lcol%, rcol%, trow%, brow%, fore%, back%)
DECLARE SUB twovlbox (lcol%, rcol%, trow%, brow%, fore%, back%)
DECLARE SUB onelinebox (lcol%, rcol%, trow%, brow%, fore%, back%)
DECLARE SUB GetFormula ()
Declare sub load_elements

DIM SHARED record AS INTEGER, ecount AS INTEGER
DIM SHARED element_name$(110), element_symbol$(110), atomic_number(110) AS INTEGER, atomic_weight(110) AS SINGLE
black = 0: blue = 1: green = 2: cyan = 3: red = 4: magenta = 5: brown = 6: white = 7
grey = 8: bright_blue = 9: bright_green = 10: bright_cyan = 11: bright_red = 12: bright_magenta = 13: yellow = 14: bright_white = 15


DIM SHARED e_symbol$(20), e_number(20)
DIM SHARED compound_name$, formula$
COMMON SHARED s, z, f_len

CALL load_elements

START:
s& = _NEWIMAGE(1000, 600, 256)
SCREEN s&


'SCREEN 0
COLOR white, blue
CLS
Align 15,25, s&, "This text is centered on the screen!"
CALL twovlbox(4, 75, 1, 6, white, blue)
COLOR yellow, blue
LOCATE 2, 6: PRINT "Molecular Weight"
COLOR white, blue
LOCATE 2, 23: PRINT "- Derives mol. wt. and % composition from formula"
LOCATE 3, 10: PRINT "Copyright 1990-2016 by John R. Duchek, Duchek Consulting Services"
LOCATE 4, 15: PRINT " St. Louis, MO 63128, (314)-845-3977"

LOCATE 8, 1
CALL GetFormula
FOR i = 1 TO z
    FOR record = 1 TO ecount

        IF e_symbol$(i) = element_symbol$(record) THEN


            c(i) = atomic_weight(record) * e_number(i)

            EXIT FOR

        END IF
    NEXT record
NEXT i


FOR i = 1 TO z
    mw = mw + c(i)
NEXT i

CLS
LENG = (80 - LEN(compound_name$)) / 2

CALL TWOLINEBOX(LENG - 2, LENG + LEN(compound_name$) + 1, 1, 3, white, blue)

LOCATE 2, LENG: PRINT compound_name$
CALL TWOHLBOX(1, 80, 5, 7, white, blue)
LOCATE 6, 3: PRINT "Formula: "; formula$
LOCATE 6, 40: PRINT " Molecular Weight ="; mw;
CALL twovlbox(15, 65, 9, 11 + z, white, blue)

LOCATE 10, 18: PRINT "ATOM", "# IN FORMULA", "PERCENT COMP."
FOR i = 1 TO z
    LOCATE 10 + i, 18: PRINT e_symbol$(i), e_number(i), 100 * c(i) / mw; " %"
NEXT i
COLOR white, blue
LOCATE 13 + z, 18: PRINT "Press any key to continue: "
GOSUB L870
FOR i = 1 TO z
    e_symbol$(i) = "": e_number(i) = 0: c(i) = 0
NEXT i
mw = 0: z = 0
GOTO START

L870:
Y$ = INPUT$(1): IF Y$ = "" THEN GOTO L870
Y = ASC(Y$) - 64: IF Y > 32 THEN Y = Y - 32
IF Y < 0 THEN Y = 0
RETURN



SUB GetFormula
    'formula input routine
    LINE INPUT "Compound name (Enter 'END' to quit) : "; compound_name$
    TEST$ = UCASE$(compound_name$): IF TEST$ = "END" THEN CLS: CLOSE: END
    'clear variables
    FOR i = 1 TO 20
        e_symbol$(i) = ""
        e_number(i) = 1
    NEXT

    INPUT "Chemical Formula (e.g. C6H5NO2; CAPS ON): "; formula$

    CLS
    f_len = LEN(formula$)
    FOR i = 1 TO f_len
        a$ = MID$(formula$, i, 1)
        a = ASC(a$)
        IF a > 64 AND a < 91 THEN z = z + 1: e_symbol$(z) = a$ ' uppercase alphabet
        IF a > 96 AND a < 123 THEN e_symbol$(z) = e_symbol$(z) + a$ ' lowercase alphabet
        IF a > 47 AND a < 58 THEN e_number(z) = VAL(RIGHT$(formula$, LEN(formula$) - i + 1)) 'numeral
        IF e_number(z) > 9 THEN i = i + 1
        IF e_number(z) > 99 THEN i = i + 1
    NEXT
    FOR i = 1 TO z
        IF LEN(e_symbol$(i)) = 1 THEN e_symbol$(i) = e_symbol$(i) + " "
    NEXT
END SUB

SUB load_elements
    OPEN "element-new.indx" FOR INPUT AS #3
    INPUT #3, ecount
    FOR record = 1 TO ecount
        INPUT #3, element_name$(record), element_symbol$(record), atomic_number(record), atomic_weight(record)
    NEXT
    CLOSE #3
END SUB


SUB Align (Tcolor, Trow, mode&, txt$)
    center& = _WIDTH(mode&) \ 2 'returns pixels in graphic modes
    MaxCol = (center& \ 8) + 1 'screen text width = 8 pixels
    Tcol = MaxCol - (LEN(txt$) \ 2)
    COLOR Tcolor: LOCATE Trow, Tcol: PRINT txt$;
END SUB

SUB TWOHLBOX (lcol%, rcol%, trow%, brow%, fore%, back%)
    COLOR fore%, back%: LOCATE trow%, lcol%
    X% = rcol% - lcol% - 1
    PRINT CHR$(213); STRING$(X%, CHR$(205)); CHR$(184);
    FOR i = (trow% + 1) TO (brow% - 1)
        LOCATE i, lcol%
        PRINT CHR$(179); STRING$(X%, " "); CHR$(179);
    NEXT i
    LOCATE brow%, lcol%
    PRINT CHR$(212); STRING$(X%, CHR$(205)); CHR$(190);

END SUB

SUB TWOLINEBOX (lcol%, rcol%, trow%, brow%, fore%, back%)
    COLOR fore%, back%: LOCATE trow%, lcol%
    X% = rcol% - lcol% - 1
    PRINT CHR$(201); STRING$(X%, CHR$(205)); CHR$(187);
    FOR i = (trow% + 1) TO (brow% - 1)
        LOCATE i, lcol%
        PRINT CHR$(186); STRING$(X%, " "); CHR$(186);
    NEXT i
    LOCATE brow%, lcol%
    PRINT CHR$(200); STRING$(X%, CHR$(205)); CHR$(188);

END SUB

SUB twovlbox (lcol%, rcol%, trow%, brow%, fore%, back%)
    COLOR fore%, back%: LOCATE trow%, lcol%
    X% = rcol% - lcol% - 1
    PRINT CHR$(214); STRING$(X%, CHR$(196)); CHR$(183);
    FOR i = (trow% + 1) TO (brow% - 1)
        LOCATE i, lcol%
        PRINT CHR$(186); STRING$(X%, " "); CHR$(186);
    NEXT i
    LOCATE brow%, lcol%
    PRINT CHR$(211); STRING$(X%, CHR$(196)); CHR$(189);

END SUB

SUB onelinebox (lcol%, rcol%, trow%, brow%, fore%, back%)
    COLOR fore%, back%: LOCATE trow%, lcol%
    X% = rcol% - lcol% - 1
    PRINT CHR$(218); STRING$(X%, CHR$(196)); CHR$(191);
    FOR i = (trow% + 1) TO (brow% - 1)
        LOCATE i, lcol%
        PRINT CHR$(179); STRING$(X%, " "); CHR$(179);
    NEXT i
    LOCATE brow%, lcol%
    PRINT CHR$(192); STRING$(X%, CHR$(196)); CHR$(217);

END SUB


'SUB save_index
'    OPEN "element-new.indx" FOR OUTPUT AS #3
'    WRITE #3, ecount
'    FOR record = 1 TO ecount
'        WRITE #3, element_name$(record), element_symbol$(record), atomic_number(record), atomic_weight(record)
'    NEXT
'    CLOSE #3
'END SUB


