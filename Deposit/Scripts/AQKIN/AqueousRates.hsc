# first column must be the rate name
#
# indicators:
	# T[emperature]	Temperature dependence (Arrehnius)
	# C[omponent]	Related to total concentration of a component 
	# M[olality]	Related to molalitie of a species
	# A[ctivity]	Related to activity of a species
	
	


print 
-echo_input false
	
Calculate_values
WriteAqueousRates
-state definition
-file AqueousRates.in
-active true
-start
10 tab$ = "RatesAq"
20 mult$ = " * " 
30 qt$ = chr$(34)
40 empty$ = "       "


100 nr=get_defnrrec(tab$)
110 eHname = get_defexist(tab$,"Name",1)  
450 printf "RATES"

500 for i=1 to nr step 1
510  printf get_deflabel(tab$,i) + EOL$ + "-start"

600  nrel = get_defnrel(tab$,i)
610  j=2
625  rate0$ = "parm(1)" # rate0 will collect k0 and the temperature dependence of k0 
620  rate$="" #rate0 will collect order terms related to C, M, A
630  WHILE (j<nrel+1)
640   GOSUB 1000
700  WEND 

800  line$=" 10 rate0 = parm(1)" + EOL$
810  line$=line$ + " 20 rate = rate0 " + rate$ + EOL$
820  line$=line$ + " 30 moles = - rate * TOT(" + qt$ + "water" + qt$ + ") * TIME" + EOL$
830  line$=line$ + " 40 SAVE moles" + EOL$ + "-end"
890  printf line$
900 next i

990 end 
1000 REM Looking for the option
1010 if(get_defisstring(tab$,j,i)) then goto 1100
1020 j=j+1
1030 return
1100 firstchar$=mid$(get_def(tab$,j,i),1,1)
1110 choice = -1
1120 if((firstchar$="c") OR (firstchar$="C")) then choice=1
1130 if((firstchar$="m") OR (firstchar$="M")) then choice=2
1140 if((firstchar$="a") OR (firstchar$="A")) then choice=3
1700 if choice=-1 then goto 1800
1710 on choice gosub 2000, 3000 , 4000
1790 goto 1990

1800 REM error: not a valid option
1810 errstr$= "Error in data block " + tab$ + " on record " + TRIM(STR$(i)) + " and column " + TRIM(STR$(j)) + "." + EOL$
1820 errstr$= errstr$ + empty$ + qt$ + firstchar$ + qt$ + " is not a valid option (valid options are: C(omponent), M(olality), A(ctivity))." + EOL$
1830 err errstr$

1990 return

2000 REM Writing rate for component
2010 j=j+1
2020 if(get_defisnumber(tab$,j,i)) then goto 10100
2030 comp$=get_def(tab$,j,i)
2040 j=j+1
2050 if(get_defisstring(tab$,j,i)) then goto 10200
2060 rate$=rate$ + mult$ + "(TOT(" + qt$ + comp$ + qt$ + "))" 
2070 order=get_def(tab$,j,i)
2080 if(order<>1) then rate$=rate$ + "^" + TRIM(str$(order)) 
2480 j=j+1
2490 return

3000 REM Writing rate for molality
3010 j=j+1
3020 if(get_defisnumber(tab$,j,i)) then goto 10100
3030 comp$=get_def(tab$,j,i)
3040 j=j+1
3050 if(get_defisstring(tab$,j,i)) then goto 10200
3060 rate$=rate$ + mult$ + "(MOL(" + qt$ + comp$ + qt$ + "))" 
3070 order=get_def(tab$,j,i)
3080 if(order<>1) then rate$=rate$ + "^" + TRIM(str$(order)) 
3480 j=j+1
3490 return

4000 REM Writing rate for activity
4010 j=j+1
4020 if(get_defisnumber(tab$,j,i)) then goto 10100
4030 comp$=get_def(tab$,j,i)
4040 j=j+1
4050 if(get_defisstring(tab$,j,i)) then goto 10200
4060 rate$=rate$ + mult$ + "(ACT(" + qt$ + comp$ + qt$ + "))" 
4070 order=get_def(tab$,j,i)
4080 if(order<>1) then rate$=rate$ + "^" + TRIM(str$(order)) 
4480 j=j+1
4490 return


10000 REM
10100 REM error: expecting a string variable
10110 errstr$= "Error in data block " + tab$ + " on record " + TRIM(STR$(i)) + " and column " + TRIM(STR$(j)) + "." + EOL$
10120 errstr$= errstr$ + empty$ + "Found a number but expecting a string variable" + EOL$
10130 err errstr$
10190 return
10200 REM: error: expecting a number
10210 errstr$= "Error in data block " + tab$ + " on record " + TRIM(STR$(i)) + " and column " + TRIM(STR$(j)) + "." + EOL$
10220 errstr$= errstr$ + empty$ + "Found a string variable but expecting a number" + EOL$
10230 err errstr$
10290 return
-end
print 
 -echo_input true
#include$ D:\Model Projects\HP1\Verification cases\1D\0220 - Reactive Transport\ADR-R-SS2-HP1\AqueousRates.in
include$ AqueousRates.in
end

# Tce T Tce 1 M Tce 1 
# RatesAq
# -headings Name
# -rowid Name
# -origin 1
# -start
# TceDegrad C Tce 1
# PceDegrad C Pce 1
# -end

# 5 print "hallo hallo"
# 10 a=get_def("RatesAq",3,1)
# 15 c=get_defisstring("RatesAq",3,1)
# 20 if get_defisstring("RatesAq",3,1) then printf "a isstring" else printf "a isnumber"
# 30 b$ = get_def("RatesAq",2,1)
# 32 if get_defisnumber("RatesAq",2,1) then printf "b isnumber" else printf "b issetring"
# 50 printf a " is a number " c b$ " is a number " d
# 90 a=-999
# 95 b$="NULL"
# 100 if get_defisstring("RatesAq",3,"Tce") then b$=get_def("RatesAq",3,1) else a=get_def("RatesAq",3,1)

# 150 printf a,  b$
# 170 printf , , ,
# 200 ex = get_defexist("RatesAq",9,1)
# 210 if ex then printf "it exists" else printf "it doesnot exist"
# 220 ex = get_defexist("RatesAq",2,5)
# 230 if ex then printf "it exists" else printf "it doesnot exist"
# 240 ex = get_defexist("RatesAq",2,2)
# 250 if ex then printf "it exists" else printf "it doesnot exist"

# 500 nr=get_defnrrec("RatesAq")

# 600 printf "number of records in RatesAq: " nr

# 700 nr=get_defnrel("RatesAq","Tce")
# 750 printf "number of first record" nr
# 800 printf "number of second record" get_defnrel("RatesAq","Pce")
# 900 printf "number 900" get_defnrel("RatesAq",2)
# 920 printf "numer 920 label name for record 1" get_deflabel("RatesAq",1)
# 930 printf "numer 930 label name for record 2" get_deflabel("RatesAq",2)
# 940 printf "numer 940 record number for Tce" get_deflabel("RatesAq","Tce")

# -end

