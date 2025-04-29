# first column must be the Material Number (negative value will be assigned to immobile domain) - material numbers must be in increasing sequence
#													-> this might be relaxed by using the kinetic_modify command
# second column must be kinetic rate name
#	if rate_name = "___", then numerical options are given

# indicators for rate name:
	# S(toichiometry) referts to the table with the stoichiometric coefficients - must be followed by a table name
	# R(ate)  : rate - option r\s :unit 1/s
	# T(ol)   : tolerance (see phreeqc manual)
	
# indicators for numerical options (see phreeqc manual)
	# R(unga_kutta) followed by 1,2,3 or 6
	# C(vode) followed by t/f
	# B(ad_step_max) followed by integer
	# O (Cvode_order) followed by 1, 2, 3, 4 or 5
	# S (Cvode_steps) followed by integer

# KineticsAq
# -heading material Name
# -origin 1
# -start
# 1 PolaDegrad S AqStoich r  2.5E-3
# -end	

# AqStoich
# -heading Name
# -rowid Name
# -origin 1
# -start
# Name Pola Polb Polc
# PolaDegrad 1    -1   0
# -end
	


print 
-echo_input false
	
Calculate_values
WriteAqueousKinetics
-state definition
-file AqueousKinetics.in
-active true
-start
10 tab$ = "KineticsAq"
20 mult$ = " * " 
30 qt$ = chr$(34)
40 empty$ = "       "

100 nr=get_defnrrec(tab$)
110 eHname = get_defexist(tab$,"Name",1)  
120 mat=0
130 gosub 9500

500 for i=1 to nr step 1
510  nrel = get_defnrel(tab$,i)
520  if get_def(tab$,1,i)=mat then goto 550 
530    mat=get_def(tab$,1,i)
540    gosub 9000
550  ratename$=get_def(tab$,2,i)
560  if ratename$<>"___" then LabelOption=1000 else LabelOption=1500

610  j=3

630  WHILE (j<nrel+1)
640   GOSUB LabelOption
700  WEND 

800 gosub 9100

900 next i

990 end 

1000 REM Looking for the option - rate name
1010 if(get_defisstring(tab$,j,i)) then goto 1100
1020 j=j+1
1030 return
1100 firstchar$=mid$(get_def(tab$,j,i),1,1)
1110 choice = -1
1120 if((firstchar$="s") OR (firstchar$="S")) then choice=1
1130 if((firstchar$="r") OR (firstchar$="R")) then choice=2
1140 if((firstchar$="t") OR (firstchar$="T")) then choice=3
1200 if choice=-1 then goto 1800
1210 j=j+1
1220 on choice gosub 2010, 2100 , 2200
1230 j = j+1
1290 goto 1990

1500 REM Looking for the option - numerical 

1790 goto 1990


1800 REM error: not a valid option
1810 errstr$= "Error in data block " + tab$ + " on record " + TRIM(STR$(i)) + " and column " + TRIM(STR$(j)) + "." + EOL$
1820 errstr$= errstr$ + empty$ + qt$ + firstchar$ + qt$ + " is not a valid option (valid options are: S(toichiometry), R(ate), T(olerance))." + EOL$
1830 err errstr$

1990 return

2000 REM Reading rate name options
2010 stoich$=get_def(tab$,j,i)
2090 return
2100 if(get_defisstring(tab$,j,i)) then goto 10200
2110 if LEN(get_def(tab$,(j-1),i))=3 then goto 2150 
2120 rate = get_def(tab$,j,i)
2130 goto 2190
2150 unit$=mid$(get_def(tab$,(j-1),i),3,1)
2160 rate = get_def(tab$,j,i)/conv_t(unit$)
2190 return
2200 if(get_defisstring(tab$,j,i)) then goto 10200
2210 tol = get_def(tab$,j,i)
2490 return

8000 REM Reading and printing formula
8100 if (get_defexist(stoich$,"Name",ratename$)) <> 0 then goto 8900
8110 labelrow = get_deflabel(stoich$,ratename$)
8120 nrfor = get_defnrel(stoich$,labelrow)
8130 nrinput = get_defnrel(stoich$,1)
8140 if nrfor>nrinput then nrfor=nrinput #warning
8190 stoichstr$=" "
8200 for k=2 to nrfor step 1
8210   value = get_def(stoich$,k,labelrow)
8220   if value=0 then goto 8500
8230   stoichstr$=stoichstr$+get_def(stoich$,k,1) + " " + str$(value) + " " 
8500 next k


8890 goto 8990
8900 REM error: not an exising record 
8910 errstr$= "Error in data block " + stoich$ + EOL$
8920 errstr$= errstr$ + empty$ + "Label " + qt$ +ratename$ + qt$ + " is not found in the stoichiometry table " + qt$ + stoich$ + qt$  + " for rate name."  + EOL$
8930 err errstr$
8990 return

9000 REM Writing kinetic function - heading for material
9010 if mat=0 then return
9020 if mat>0 then nr=get_var("_NODES")*100+mat else nr=get_var("_NODES")*200-mat
9030 string$= "KINETICS " + str$(nr) + EOL$ + "-material "
9040 if mat >0 then string$=string$ + str$(mat) else string$=string$ + "im " + str$(abs(mat))
9050 printf string$
9060 string$=""
9090 return

9100 REM Writing kinetic function - parameter for rates
9150 if stoich$<>"empty" then gosub 8000 else stoichstr$= "H+ 0.0"
9155 string$ = string$ + EOL$ + ratename$ + EOL$
9160 string$ = string$ + empty$ + "-formula " + stoichstr$ + EOL$
9170 string$ = string$ + empty$ + "-m0 10" + EOL$ + empty$ + "-m 10" + EOL$
9180 string$ = string$ + empty$ + "-parm " + str$(rate) + EOL$
9200 if tol<> 0 then string$ = string$ + empty$ + "-tol " + str$(rate) + EOL$
9300 printf string$

9500 REM ReInitialize
9510 stoich$ = "empty"
9520 rate = 0
9530 tol = 0
9540 string$=""
9990 return

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
include$ AqueousKinetics.in
end

