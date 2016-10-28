' formula.bas - 11-25-90 99-101%
'qb64 new version 2016-0927, recovered from qb45 using qb64
'working 2016-0930
'_TITLE "Derives the Empirical Formula and Molecular Formula from % Composition"
Declare sub load_index
DIM SHARED record AS INTEGER, ecount AS INTEGER
DIM SHARED element_name$(110), element_symbol$(110), atomic_number(110) AS INTEGER, atomic_weight(110) AS SINGLE
DIM SHARED num_elements AS INTEGER, index AS INTEGER, char$, sample$, molewt AS SINGLE
DIM SHARED percent(20), theopercent(20), chemsym$(20), atwt(20), c(20), moleratio(20), molecular_formula(20)
DIM SHARED test1, test2 AS INTEGER, f, k AS SINGLE
'DIM SHARED v1, v2, v3, v4, v5, v6 AS SINGLE
DEFSNG V
CLS
CALL load_index
black = 0: blue = 1: green = 2: cyan = 3: red = 4: magenta = 5: brown = 6: white = 7
grey = 8: bright_blue = 9: bright_green = 10: bright_cyan = 11: bright_red = 12: bright_magenta = 13: yellow = 14: bright_white = 15
horiz$ = "-"

restart:
s& = _NEWIMAGE(1000, 600, 256)
SCREEN s&
COLOR white, blue
CLS
COLOR yellow, blue
LOCATE 2, 6: PRINT "FORMULA"
COLOR white, blue
LOCATE 2, 14: PRINT "- Derives empirical and molecular formula from % composition"
LOCATE 3, 10: PRINT "Copyright 1990-2016 by John R. Duchek, Duchek Consulting Services, formerly Duchek Computer Services"
LOCATE 4, 15: PRINT "St. Louis, MO 63128, (314)-845-3977"
'LOCATE 5, 20: PRINT "For use by REGISTERED CHEMPAC users only"
LOCATE 8, 1: INPUT "Sample name (END to exit): "; sample$
IF UCASE$(sample$) = "END" THEN CLOSE: END

retryit:

INPUT "How many different elements are in your formula (MAX=11)"; num_of_elements
IF num_of_elements = 0 THEN GOTO retryit
PRINT num_of_elements

infoinput:

FOR index = 1 TO num_of_elements
    percent(index) = 0
    theopercent(index) = 0
    test1 = 0: test2 = 0
NEXT
FOR index = 1 TO num_of_elements
    INPUT "GIVE CHEMICAL SYMBOL, AND % IN COMPOUND (Cl,32.4):"; chemsym$(index), percent(index)
    'needs capitalization error routine  (first char caps, second char either lowercase or space)
    chemsym$(index) = chemsym$(index) + "  "
    chemsym$(index) = LEFT$(chemsym$(index), 2)
    test1 = test1 + percent(index)
NEXT index

IF test1 < 99 OR test1 > 101 THEN
    PRINT "Your compound is significantly different than 100%": PRINT test1
    char$ = INPUT$(1)
    PRINT "Please input all of the data again."
    test1 = 0
    GOTO infoinput
END IF

FOR index = 1 TO num_of_elements
    FOR record = 1 TO ecount
        IF chemsym$(index) = element_symbol$(record) THEN
            atwt(index) = atomic_weight(record)
            c(index) = percent(index) / (100 * atwt(index))
            EXIT FOR
        END IF
    NEXT record
NEXT index

'SHELL SORT OF V( )
v1 = num_of_elements
v2 = v1
L400:
v2 = INT(v2 / 2)
IF v2 = 0 THEN GOTO calculatemw
v3 = 1: v4 = v1 - v2

L430:
v5 = v3

L440:
v6 = v5 + v2
IF c(v5) <= c(v6) THEN GOTO L500
SWAP percent(v5), percent(v6): SWAP chemsym$(v5), chemsym$(v6): SWAP c(v5), c(v6): SWAP atwt(v5), atwt(v6)
v5 = v5 - v2
IF v5 < 1 THEN GOTO L500
GOTO L440


L500:
v3 = v3 + 1
IF v3 > v4 THEN GOTO L400
GOTO L430

calculatemw:

FOR index = 1 TO num_of_elements
    moleratio(index) = c(index) / c(1)

NEXT

INPUT "Approximate Molecular Weight: "; apmw
k = k + 1
f = 0
FOR index = 1 TO num_of_elements
    f = f + atwt(index) * moleratio(index) * k
NEXT

multfactr = INT((apmw / f))
molewt1 = 0: molewt2 = 0
FOR index = 1 TO num_of_elements
    c1(index) = INT(moleratio(index) * multfactr + .1)
    molewt1 = molewt1 + c1(index) * atwt(index)
    c2(index) = INT(moleratio(index) * (multfactr + 1) + .1)
    molewt2 = molewt2 + c2(index) * atwt(index)
NEXT
test1 = ABS(molewt1 - apmw)
test2 = ABS(molewt2 - apmw)
IF test1 > test2 THEN
    molewt = molewt2
    FOR index = 1 TO num_of_elements
        molecular_formula(index) = c2(index)
    NEXT
ELSE molewt = molewt1: FOR index = 1 TO num_of_elements: molecular_formula(index) = c1(index): NEXT
END IF
totpercent = 0: tottheo = 0
FOR index = 1 TO num_of_elements
    theopercent(index) = (INT(molecular_formula(index) + .5) * atwt(index) * 100) / molewt
    IF ABS(theopercent(index) - percent(index)) >= 1 THEN catcherror = 1
    totpercent = totpercent + percent(index)
    tottheo = tottheo + theopercent(index)
NEXT

printout:
COLOR white, blue
CLS
length% = (40 - LEN(sample$) / 2)
LOCATE 2, length% + 2: PRINT sample$
LOCATE 6, 1
PRINT STRING$(75, horiz$)
PRINT "ELEMENT   GIVEN PER CENT   THEO. PER CENT   EMP. FORM.  MOL. FORM."
PRINT STRING$(75, horiz$)
FOR index = 1 TO num_of_elements
    ' PRINT chemsym$(index), percent(index), theopercent(index), INT(moleratio(index) + .5),INT(molecular_formula(index) + .5)
    PRINT chemsym$(index);
    PRINT TAB(16);
    PRINT USING "##.###"; percent(index);

    PRINT TAB(32);
    PRINT USING "##.###"; theopercent(index);

    PRINT TAB(52); INT(moleratio(index) + .5);
    PRINT TAB(68); INT(molecular_formula(index) + .5)
NEXT

PRINT STRING$(75, horiz$)
PRINT "Total:"; TAB(15);: PRINT USING "###.###"; totpercent;
PRINT TAB(31);: PRINT USING "###.###"; tottheo
PRINT
PRINT "Approximate molecular weight given: "; apmw
PRINT "Best Estimate of exact Molecular weight: "; molewt


errorcalc = (ABS(molewt - apmw) / apmw) * 100
PRINT: PRINT "Warning: the approximate molecular weight is "; errorcalc; " % off"
IF catcherror = 1 THEN PRINT: PRINT "Data in error - Molecular Formula is probably in error."
char$ = INPUT$(1)

apmw = 0: molewt = 0
k = 0


GOTO restart

SUB load_index
    OPEN "element-new.indx" FOR INPUT AS #3
    INPUT #3, ecount
    FOR record = 1 TO ecount
        INPUT #3, element_name$(record), element_symbol$(record), atomic_number(record), atomic_weight(record)
    NEXT
    CLOSE #3
END SUB
'SUB save_index
'    OPEN "element-new.indx" FOR OUTPUT AS #3
'    WRITE #3, ecount
'    FOR record = 1 TO ecount
'        WRITE #3, element_name$(record), element_symbol$(record), atomic_number(record), atomic_weight(record)
'    NEXT
'    CLOSE #3
'END SUB


