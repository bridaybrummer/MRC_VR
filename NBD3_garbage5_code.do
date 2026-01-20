clear
use "C:/Users/mmazinu/OneDrive - South African Medical Research Council/Documents/Work/BOD/NBD3/Data_cleaning/data/cleanCOD 1997_2021.dta" 

keep if deathtype ==1

/******************************************************************************/
		//WHO garbage below//

clear
import excel "C:\Users\mmazinu\OneDrive - South African Medical Research Council\Documents\Work\BOD\NBD3\WHO_IDD_Garbage_codes_MM.xlsx", sheet("Stata_code") firstrow

merge 1:m underlyingcause using "C:/Users/mmazinu/OneDrive - South African Medical Research Council/Documents/Work/BOD/NBD3/Data_cleaning/data/cleanCOD 1997_2021.dta" 

tab garbage_who_idd

drop if _merge == 1

tab garbage_who_idd

encode garbage_who_idd ,gen(garbage_who)
drop garbage_who_idd
replace garbage_who = 0 if garbage_who == .
label define garbage_who_L 0 "No garbage" 1 "Type A" 2 "Type B1" 3 " Type B2"
label values garbage_who garbage_who_L


/******************************************************************************/
*Original garbage
/******************************************************************************/

sort underlyingcause

label define garbage 0 "Not garbage" ///
					 1 "Symptoms, signs and ill-definedconditions" ///
					 2 "Impossible as underlying causes of death" ///
					 3 "Intermediate causes of death" ///
					 4 "Immediate causes of death"  ///
					 5 "Insufficiently specified causes within ICD chapters" 


gen garbage=0

replace garbage=1 if (underlyingcause>="R00" & underlyingcause<="R99")

replace garbage=4 if underlyingcause=="D65"|underlyingcause=="I46"| underlyingcause=="J96"

replace garbage=5 if underlyingcause=="C80"|underlyingcause=="C26"|underlyingcause=="C39"|underlyingcause=="C57"|underlyingcause=="C64"|underlyingcause=="C76"|underlyingcause=="A49"|underlyingcause=="B83"|underlyingcause=="B99"|underlyingcause=="E88"|underlyingcause=="I51"|underlyingcause=="I99"|underlyingcause=="X59"|(underlyingcause>="D00" & underlyingcause<="D13")|(underlyingcause>="D16" & underlyingcause<="D18")|(underlyingcause>="D20" & underlyingcause<="D24")|(underlyingcause>="D28" & underlyingcause<="D48")|(underlyingcause>="Y10" & underlyingcause<="Y34") 


replace garbage=3 if (underlyingcause>="A40" & underlyingcause<="A41")|underlyingcause=="A48"|(underlyingcause>="E85" & underlyingcause<="E87")|(underlyingcause>="G91" & underlyingcause<="G93")|underlyingcause=="I26"|underlyingcause=="I27"|(underlyingcause>="I44" & underlyingcause<="I45")|(underlyingcause>="I49" & underlyingcause<="I50")|underlyingcause=="I74"|underlyingcause=="I81"|underlyingcause=="J69"|(underlyingcause>="J80" & underlyingcause<="J81")|underlyingcause=="J86"|underlyingcause=="J90"|underlyingcause=="J93"|underlyingcause=="J94"|underlyingcause=="J98"|underlyingcause=="K75"|underlyingcause=="K76"|underlyingcause=="M86"|underlyingcause=="N14"|(underlyingcause>="K65" & underlyingcause<="K66")|(underlyingcause>="K71" & underlyingcause<="K72")|(underlyingcause>="N17" & underlyingcause<="N19")|underlyingcause=="K92"


replace garbage=2 if underlyingcause=="A31"|underlyingcause=="A59"|underlyingcause=="A60"|underlyingcause=="A63"|underlyingcause=="B00"|underlyingcause=="B07"|underlyingcause=="B08"|underlyingcause=="B30"|underlyingcause=="G54"|underlyingcause=="J30"|underlyingcause=="K14"|underlyingcause=="L94"|underlyingcause=="L98"|underlyingcause=="M03"|underlyingcause=="M07"|underlyingcause=="M35"|underlyingcause=="M40"|underlyingcause=="M43"|underlyingcause=="M45"|underlyingcause=="N39"|underlyingcause=="N40"|underlyingcause=="N46"|underlyingcause=="N60"|underlyingcause=="N97"|underlyingcause=="Q36"|underlyingcause=="Q38"|underlyingcause=="Q54"|underlyingcause=="B94"|underlyingcause=="Y86"|underlyingcause=="Y87"|underlyingcause=="Y89"|underlyingcause=="I10"|underlyingcause=="I15"|underlyingcause=="I70"

replace garbage=2 if (underlyingcause>="A71" & underlyingcause<="A74")|(underlyingcause>="B35" & underlyingcause<="B36")|(underlyingcause>="F32" & underlyingcause<="F33")|(underlyingcause>="F40" & underlyingcause<="F42")|(underlyingcause>="F45" & underlyingcause<="F48")|(underlyingcause>="F51" & underlyingcause<="F53")|(underlyingcause>="F60" & underlyingcause<="F98")|(underlyingcause>="G43" & underlyingcause<="G45")|(underlyingcause>="G47" & underlyingcause<="G52")|(underlyingcause>="G56" & underlyingcause<="G58")|(underlyingcause>="H00" & underlyingcause<="H69")|(underlyingcause>="H71" & underlyingcause<="H80")|(underlyingcause>="H83" & underlyingcause<="H93")|(underlyingcause>="J33" & underlyingcause<="J35")|(underlyingcause>="K00" & underlyingcause<="K11")|(underlyingcause>="L04" & underlyingcause<="L08")|(underlyingcause>="L20" & underlyingcause<="L25")|(underlyingcause>="L28" & underlyingcause<="L87")|(underlyingcause>="L90" & underlyingcause<="L92")|(underlyingcause>="M09" & underlyingcause<="M12")|(underlyingcause>="M14" & underlyingcause<="M25")|(underlyingcause>="M47" & underlyingcause<="M60")|(underlyingcause>="M63" & underlyingcause<="M71")|(underlyingcause>="M73" & underlyingcause<="M79")|(underlyingcause>="M95" & underlyingcause<="M99")|(underlyingcause>="N84" & underlyingcause<="N93")|(underlyingcause>="Q10" & underlyingcause<="Q18")|(underlyingcause>="Q65" & underlyingcause<="Q74")|(underlyingcause>="Q82" & underlyingcause<="Q84")|(underlyingcause>="G80" & underlyingcause<="G83")

label values garbage garbage

tab garbage


save "C:/Users/mmazinu/OneDrive - South African Medical Research Council/Documents/Work/BOD/NBD3/Data_cleaning/data/cleanCOD 1997_2021_using.dta", replace