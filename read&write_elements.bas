
DIM SHARED record AS INTEGER, ecount AS INTEGER
DIM SHARED ename AS STRING
DIM SHARED esymbol(150) AS STRING * 2
DIM SHARED atomic_no AS INTEGER, atomic_wt AS SINGLE
DIM SHARED electron_shell(7) AS INTEGER
DIM SHARED eln$(120), els$(120), atno(120), atwt(120)
DIM SHARED oxidation_state(8) AS INTEGER, radius(8) AS SINGLE
DIM SHARED DENSITY AS SINGLE, electron_shellRONEG AS SINGLE, IONPOTL AS SINGLE
DIM SHARED meltingpt AS INTEGER, boilingpt AS INTEGER
DIM SHARED specheat AS SINGLE, discover AS INTEGER, discoverer$, mineral$
DIM SHARED descript$
DIM SHARED stiso(10), stabund(10), stisomass(10), stmagspin(10)
DIM SHARED uniso(12), unabund(12), unisomass(12), unmagspin(12), hlife$(12), dmode$(12)


load_index
FOR record = 1 TO ecount

    PRINT eln$(record), els$(record), atno(record), atwt(record)
NEXT



END

SUB load_index
    OPEN "element-new.indx" FOR INPUT AS #3
    INPUT #3, ecount
    FOR record = 1 TO ecount
        INPUT #3, eln$(record), els$(record), atno(record), atwt(record)
    NEXT
    CLOSE #3
END SUB
SUB save_index
    OPEN "element-new.indx" FOR OUTPUT AS #3
    WRITE #3, ecount
    FOR record = 1 TO ecount
        WRITE #3, eln$(record), els$(record), atno(record), atwt(record)
    NEXT
    CLOSE #3
END SUB

SUB read_element_new
    OPEN "element-new.data" FOR INPUT AS #2
END SUB

SUB write_element_new
    OPEN "element-new.data" FOR OUTPUT AS #2
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

