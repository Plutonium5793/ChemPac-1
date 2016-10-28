'program to edit element data
'Element_Handling - 03/14/91
'     -----     element.indx parameters
declare sub load_indx
declare sub save_indx
DIM SHARED eln$(120), els$(120), atno(120), atwt(120)
DIM SHARED record AS INTEGER, ecount AS INTEGER


'     -----    element.data parameters
declare sub read_element
declare sub write_element
declare sub read_element_data '(one record)
declare sub write_element_data '(one record)
DIM SHARED ename AS STRING
DIM SHARED esymbol(150) AS STRING * 2
DIM SHARED atomic_no AS INTEGER, atomic_wt AS SINGLE
DIM SHARED electron_shell(7) AS INTEGER

DIM SHARED oxidation_state(8) AS INTEGER, radius(8) AS SINGLE
DIM SHARED DENSITY AS SINGLE, electron_shellRONEG AS SINGLE, IONPOTL AS SINGLE
DIM SHARED meltingpt AS INTEGER, boilingpt AS INTEGER
DIM SHARED specheat AS SINGLE, discover AS INTEGER, discoverer$, mineral$
DIM SHARED descript$
DIM SHARED stiso(10), stabund(10), stisomass(10), stmagspin(10)
DIM SHARED uniso(12), unabund(12), unisomass(12), unmagspin(12), hlife$(12), dmode$(12)

DECLARE SUB edit.radioisotope ()
DECLARE SUB edit.stble.isotope ()
'     -----     Program parameters
declare SUB Align (Tcolor, Trow, mode&, txt$)
DECLARE SUB onelinebox (lcol%, rcol%, trow%, brow%, fore%, back%)
DECLARE SUB onekey (z%, z$)
'DIM SHARED a$(110), B$(20), B(20), pr AS INTEGER, pf AS INTEGER
'DIM SHARED e$(150)
_TITLE "Editing the Periodic Table"
black = 0: blue = 1: green = 2: cyan = 3: red = 4: magenta = 5: brown = 6: white = 7
grey = 8: bright_blue = 9: bright_green = 10: bright_cyan = 11: bright_red = 12: bright_magenta = 13: yellow = 14: bright_white = 15

'     ----- work flow
'load indx properly loads






load_index
FOR record = 1 TO ecount

NEXT


mainmenu:
's& = _NEWIMAGE(1000, 600, 256)
'SCREEN s&

SCREEN 0
COLOR white, blue
CLS
'Align (Tcolor, Trow, mode&,"center")
CALL onelinebox(4, 75, 1, 6, white, blue)
COLOR yellow, blue
LOCATE 2, 6: PRINT "ELEMENT EDITING"
COLOR white, blue
LOCATE 2, 22: PRINT "- Edits the ELEMENT.DAT file"
LOCATE 3, 10: PRINT "Copyright 1990-2016 by John R. Duchek, Duchek Consulting Services"
LOCATE 4, 15: PRINT "St. Louis, MO 63128, (314)-845-3977"

LOCATE 8, 1

PRINT "FILE HANDLING MENU FOR ELEMENT.DAT"
PRINT
'PRINT "A     add new elements"
PRINT "C     change current file"
PRINT "E     Close file and end"
'PRINT "D     build current data file"
PRINT "S     Edit stable isotopes"
PRINT "U     Edit unstable isotopes"
CALL onekey(z%, z$)
'SELECT CASE z$

'    ' CASE "A"
'    ' pf = pf + 1
'    ' PRINT pf; : INPUT "new element name (enter if no name) : "; eln$
'    ' INPUT "symbol (enter if not known) : "; els$
'    ' INPUT "atomic number (0 to exit): "; atno
'    ' IF atno = 0 THEN pf = pf - 1: RETURN
'    ' INPUT "atomic weight : "; atwt
'    ' GOSUB dispdata
'    ' GOSUB OXSTATE

'    CASE "C"
'        GOSUB l260
'    CASE "E"
'        CLOSE: END
'        'CASE "D"
'        'CALL datainput
'    CASE "S"
'        CALL edit.stble.isotope
'    CASE "U"
'        CALL edit.radioisotope
'    CASE ELSE
'END SELECT
'GOTO mainmenu

'l260:
''edit file
'CLS
'PRINT "a     Search by element name."
'PRINT "b     Search by element symbol."
'PRINT "c     Search by atomic no."
'PRINT "d     Search by atomic weight."
'PRINT "N     NO search required."
'PRINT "R     Return"
'CALL onekey(z%, z$)
'CLS
'z$ = UCASE$(z$)
'SELECT CASE z$
'    CASE "R"
'        CLS: RETURN
'    CASE "N"
'        GOTO here
'    CASE ELSE
'END SELECT

'INPUT "search parameter : "; S$
'IF VAL(S$) > 0 THEN S = VAL(S$)
'FOR pf = 1 TO pr
'
'    '   CALL getelement
'    ON z% GOSUB L450, L470, L490, L510
'NEXT
'here:
'GOTO L530
'GOTO l260

'L450:
'T = INSTR(eln$, S$): IF T > 0 THEN PRINT pf, eln$, els$, atno, atwt
'RETURN

'L470:
'T = INSTR(els$, S$): IF T > 0 THEN PRINT pf, eln$, els$, atno, atwt
'RETURN

'L490:
'IF atno = S THEN PRINT pf, eln$, els$, atno, atwt
'RETURN

'L510:
'IF atwt = S THEN PRINT pf, eln$, els$, atno, atwt
'RETURN

'L530:
'INPUT "File number to be changed : "; pf
'IF pf < 1 OR pf > pr THEN PRINT "Illegal parameter.": GOTO L530
''CALL getelement
'L560:
'CLS: PRINT "CHANGE MENU": PRINT
'PRINT "A     change element name", eln$
'PRINT "B     change element symbol", els$
'PRINT "C     change atomic number"; atno
'PRINT "D     change atomic weight", atwt
'PRINT "E     change electronic configuration "; ELECT(1); ELECT(2); ELECT(3); ELECT(4); ELECT(5); ELECT(6); ELECT(7)
'PRINT "F     change oxidation states ";
'FOR i1 = 1 TO 8: PRINT OXSTATE(i1);: NEXT: PRINT
'PRINT "G     melting point = "; meltingpt; " K"
'PRINT "H     boiling point = "; boilingpt; " K"
'PRINT "I     density = "; DENSITY
'PRINT "J     Electronegativity = "; ELECTRONEG
'PRINT "K     Ionization Potential = "; IONPOTL
'PRINT "L     ionic radii  ";: FOR i1 = 1 TO 8: PRINT radius(i1);: NEXT: PRINT
'PRINT "M     Specific heat: "; specheat
'PRINT "N     discovery: "; discover
'PRINT "O     discoverer: "; discoverer$
'PRINT "P     ores: "; mineral$
'PRINT "Q     Description: "; descript$
'PRINT
'PRINT "R     RETURN to main menu"
'PRINT "S     Another file ?"
'CALL onekey(z%, z$)
'SELECT CASE UCASE$(z$)
'    CASE "A"
'        INPUT "New element name : "; eln$
'    CASE "B"
'        INPUT "New element symbol : "; els$
'    CASE "C"
'        INPUT "New atomic number : "; atno
'    CASE "D"
'        INPUT "New atomic weight : "; atwt
'    CASE "E"
'        GOSUB DISPDATA
'    CASE "F"
'        GOSUB OXSTATE

'    CASE "G"
'        INPUT "New melting point : "; meltingpt

'    CASE "H"
'        INPUT "New boiling point : "; boilingpt


'    CASE "I"
'        INPUT "density : "; DENSITY

'    CASE "J"
'        INPUT "Electronegativity : "; ELECTRONEG

'    CASE "K"
'        INPUT "Ionization Potential : "; IONPOTL

'    CASE "L"
'        FOR i1 = 1 TO 8: PRINT radius(i1);: NEXT: PRINT
'        FOR i1 = 1 TO 8
'            PRINT i1;: INPUT "radius: "; radius(i1)
'        NEXT
'    CASE "M"
'        INPUT "New specific heat : "; specheat
'    CASE "N"
'        INPUT "New discovery date : "; discover

'    CASE "O"
'        INPUT "New discoverer : "; discoverer$
'    CASE "P"
'        INPUT "New ores : "; mineral$
'    CASE "Q"
'        INPUT "New Description : "; descript$
'    CASE "S"
'        GOTO L530
'    CASE "R"
'        RETURN
'    CASE ELSE
'END SELECT
''CALL putelement
'GOTO L560

'DISPDATA:
'PRINT eln$
'PRINT "ELECT(1) = "; ELECT(1), "ELECT(2) = "; ELECT(2), "ELECT(3) = "; ELECT(3), "ELECT(4) = "; ELECT(4), "ELECT(5) = "; ELECT(5)
'PRINT "ELECT(6) = "; ELECT(6), "ELECT(7) = "; ELECT(7)
'FOR i = 1 TO 7
'    PRINT "Electrons in "; i; "shell ?";
'    INPUT ELECT(i)
'NEXT
'RETURN

'OXSTATE:
'CLS: PRINT eln$: PRINT "OXSTATE(1)= "; OXSTATE(1), "OXSTATE(2)= "; OXSTATE(2), "OXSTATE(3)= "; OXSTATE(3), "OXSTATE(4)= "; OXSTATE(4), "OXSTATE(5)= "; OXSTATE(5);
'PRINT "OXSTATE(6)= "; OXSTATE(6), "OXSTATE(7)= "; OXSTATE(7), "OXSTATE(8)= "; OXSTATE(8)
'FOR i = 1 TO 8
'    PRINT i;
'    INPUT "   from most neg to + "; OXSTATE(i)
'NEXT
'RETURN

'SUB datainput
'    agan:
'    INPUT "Atomic number (0 to end) : "; pf
'    IF pf < 1 THEN GOTO endthis2
'    IF pf > 107 THEN BEEP: GOTO agan

'    '   CALL getelement
'    PRINT eln$
'    FOR i = 1 TO 10
'        IF stiso(i) = 0 THEN EXIT FOR
'        IF stmagspin(i) = -1 THEN
'            PRINT i, stiso(i), stabund(i), stisomass(i), stmagspin(i)
'            INPUT "new spin:"; stmagspin(i)
'        END IF
'    NEXT
'    FOR i = 1 TO 12
'        IF uniso(i) = 0 THEN EXIT FOR
'        IF unmagspin(i) = -1 THEN
'            PRINT i; uniso(i); unabund(i); unisomass(i); unmagspin(i); hlife$(i), dmode$(i)
'            INPUT "new spin:"; unmagspin(i)
'        END IF
'    NEXT
'    '    CALL putelement
'    GOTO agan
'    endthis2:
'END SUB

'SUB edit.radioisotope

'    KEY ON
'    KEY 1, "á-"
'    KEY 2, "á+"
'    KEY 3, "EC"
'    KEY 4, "à"
'    KEY 5, "IT"
'    KEY 6, "SF"
'    KEY 7, "÷"
'    KEY 8, "no data"
'    CLS
'    another:
'    INPUT "Atomic number (0 to end): "; pf
'    IF pf < 1 THEN GOTO endthis
'    IF pf > 107 THEN BEEP: GOTO another
'    '    CALL getelement
'    again:
'    PRINT eln$

'    FOR i = 1 TO 12
'        IF uniso(i) = 0 THEN EXIT FOR
'        PRINT i; uniso(i); unabund(i); unisomass(i); unmagspin(i); hlife$(i), dmode$(i)
'    NEXT
'    INPUT "index # (0 to exit; 1-12):"; i
'    IF i = 0 THEN GOTO endthis
'    INPUT "isotope #: "; uniso(i)
'    INPUT "abundance (in percent): "; unabund(i)
'    INPUT "isotope mass (<Enter> to equal isotope #): "; unisomass(i)
'    IF unisomass(i) = 0 THEN unisomass(i) = uniso(i)
'    INPUT "Nuclear magnetic spin: "; unmagspin(i)
'    INPUT "Half-life (s,m,d,y) ie.13E04y: "; hlife$(i)
'    INPUT "Decay mode (see keys below)(8): "; dmode$(i)
'    GOTO putit
'    CLEARit:
'    putit:
'    '   CALL putelement
'    GOTO another
'    endthis:
'    KEY OFF

'    CLS
'END SUB

'SUB edit.stble.isotope
'    CLS
'    another1:
'    INPUT "Atomic number (0 to end): "; pf
'    IF pf < 1 THEN GOTO endthis1
'    IF pf > 107 THEN BEEP: GOTO another1

'    '    CALL getelement
'    again1:
'    PRINT eln$
'    FOR i = 1 TO 10
'        IF stiso(i) = 0 THEN EXIT FOR
'        PRINT i, stiso(i), stabund(i), stisomass(i), stmagspin(i)
'    NEXT
'    INPUT "Index no (0 to end; 1-10): "; i
'    IF i = 0 THEN GOTO endthis1
'    INPUT "isotope #: "; stiso(i)
'    INPUT "abundance (in percent): "; stabund(i)
'    INPUT "isotope mass: "; stisomass(i)
'    INPUT "nuclear magnetic spin: "; stmagspin(i)

'    putit1:
'    '   CALL putelement
'    GOTO another1

'    endthis1:
'    CLS
'END SUB



SUB read_element
    OPEN "element.data" FOR INPUT AS #2
END SUB

SUB write_element
    OPEN "element.data" FOR OUTPUT AS #2
END SUB

SUB read_element_data
    'write new element record
input #2, ename, esymbol, atomic_no, atomic_wt,  electron_shell(1),  electron_shell(2),  electron_shell(3),  electron_shell(4),  electron_shell(5),  electron_shell(6),  electron_shell(7),_
         oxidation_state(1),  oxidation_state(2),  oxidation_state(3),   oxidation_state(4), oxidation_state(5),  oxidation_state(6), oxidation_state(7), oxidation_state(8),_
         radius(1), radius(2),  radius(3),  radius(4), radius(5), radius(6), radius(7),  radius(8),_
         meltingpt,boilingpt,DENSITY, electron_shellRONEG,IONPOTL,specheat,discover,discoverer$,mineral$,descript$,_
         stiso(1),stiso(2),stiso(3),stiso(4), stiso(5),stiso(6), stiso(7), stiso(8),stiso(9), stiso(10),_
        stabund(1),stabund(2), stabund(3),stabund(4),stabund(5),stabund(6),stabund(7),stabund(8),stabund(9),stabund(10),_
        stisomass(1), stisomass(2), stisomass(3),  stisomass(4), stisomass(5), stisomass(6), stisomass(7), stisomass(8), stisomass(9), stisomass(10),_
        stmagspin(1),stmagspin(2),stmagspin(3),stmagspin(4),stmagspin(5),stmagspin(6),stmagspin(7),stmagspin(8),stmagspin(9),stmagspin(10),_
        uniso(1), uniso(2), uniso(3), uniso(4), uniso(5), uniso(6), uniso(7), uniso(8), uniso(9), uniso(10), uniso(11), uniso(12),_
        unabund(1),unabund(2), unabund(3),unabund(4),unabund(5),unabund(6),unabund(7),unabund(8),unabund(9),unabund(10),unabund(11),unabund(12),_
        unisomass(1),unisomass(2),unisomass(3),unisomass(4),unisomass(5),unisomass(6),unisomass(7),unisomass(8),unisomass(9),unisomass(10),unisomass(11),unisomass(12),_
        unmagspin(1), unmagspin(2),unmagspin(3),unmagspin(4),unmagspin(5),unmagspin(6),unmagspin(7),unmagspin(8),unmagspin(9),unmagspin(10),unmagspin(11),unmagspin(12),_
       hlife$(1), hlife$(2), hlife$(3), hlife$(4), hlife$(5), hlife$(6),hlife$(7), hlife$(8), hlife$(9), hlife$(10), hlife$(11), hlife$(12),_
    dmode$(1), dmode$(2), dmode$(3), dmode$(4), dmode$(5), dmode$(6), dmode$(7), dmode$(8), dmode$(9), dmode$(10), dmode$(11), dmode$(12)
    CLOSE #2
END SUB


SUB write_element_data
    OPEN "element-new.data" FOR OUTPUT AS #2


    'write new element record
WRITE #2, ename, esymbol, atomic_no, atomic_wt, electron_shell(1),  electron_shell(2),  electron_shell(3),  electron_shell(4),  electron_shell(5),  electron_shell(6),  electron_shell(7),_
         oxidation_state(1),  oxidation_state(2),  oxidation_state(3),   oxidation_state(4), oxidation_state(5),  oxidation_state(6), oxidation_state(7), oxidation_state(8),_
         radius(1), radius(2),  radius(3),  radius(4), radius(5), radius(6), radius(7),  radius(8),_
         meltingpt,boilingpt,DENSITY, electron_shellRONEG,IONPOTL,specheat,discover,discoverer$,mineral$,descript$,_
         stiso(1),stiso(2),stiso(3),stiso(4), stiso(5),stiso(6), stiso(7), stiso(8),stiso(9), stiso(10),_
        stabund(1),stabund(2), stabund(3),stabund(4),stabund(5),stabund(6),stabund(7),stabund(8),stabund(9),stabund(10),_
        stisomass(1), stisomass(2), stisomass(3),  stisomass(4), stisomass(5), stisomass(6), stisomass(7), stisomass(8), stisomass(9), stisomass(10),_
        stmagspin(1),stmagspin(2),stmagspin(3),stmagspin(4),stmagspin(5),stmagspin(6),stmagspin(7),stmagspin(8),stmagspin(9),stmagspin(10),_
        uniso(1), uniso(2), uniso(3), uniso(4), uniso(5), uniso(6), uniso(7), uniso(8), uniso(9), uniso(10), uniso(11), uniso(12),_
        unabund(1),unabund(2), unabund(3),unabund(4),unabund(5),unabund(6),unabund(7),unabund(8),unabund(9),unabund(10),unabund(11),unabund(12),_
        unisomass(1),unisomass(2),unisomass(3),unisomass(4),unisomass(5),unisomass(6),unisomass(7),unisomass(8),unisomass(9),unisomass(10),unisomass(11),unisomass(12),_
        unmagspin(1), unmagspin(2),unmagspin(3),unmagspin(4),unmagspin(5),unmagspin(6),unmagspin(7),unmagspin(8),unmagspin(9),unmagspin(10),unmagspin(11),unmagspin(12),_
       hlife$(1), hlife$(2), hlife$(3), hlife$(4), hlife$(5), hlife$(6),hlife$(7), hlife$(8), hlife$(9), hlife$(10), hlife$(11), hlife$(12),_
    dmode$(1), dmode$(2), dmode$(3), dmode$(4), dmode$(5), dmode$(6), dmode$(7), dmode$(8), dmode$(9), dmode$(10), dmode$(11), dmode$(12)

END SUB
SUB load_index
    OPEN "element.indx" FOR INPUT AS #3
    INPUT #3, ecount
    FOR record = 1 TO ecount
        INPUT #3, eln$(record), els$(record), atno(record), atwt(record)
    NEXT
    CLOSE #3
END SUB
SUB save_index
    OPEN "element.indx" FOR OUTPUT AS #3
    WRITE #3, ecount
    FOR record = 1 TO ecount
        WRITE #3, eln$(record), els$(record), atno(record), atwt(record)
    NEXT
    CLOSE #3
END SUB

SUB onekey (z%, z$)
    z$ = UCASE$(INPUT$(1))
    z% = ASC(z$) - 64
    IF z% < 0 THEN z% = 0
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

SUB Align (Tcolor, Trow, mode&, txt$)
    center& = _WIDTH(mode&) \ 2 'returns pixels in graphic modes
    MaxCol = (center& \ 8) + 1 'screen text width = 8 pixels
    Tcol = MaxCol - (LEN(txt$) \ 2)
    COLOR Tcolor: LOCATE Trow, Tcol: PRINT txt$;
END SUB

