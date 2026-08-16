global data    "C:\Users\Mon ordi\Desktop\PROJET ANALYSE P"
global output  "C:\Users\Mon ordi\Desktop\PROJET ANALYSE P"
global temp    "C:\Users\Mon ordi\Desktop\PROJET ANALYSE P\BASED"

* juste pour l'analyse de l'education
*=====================================================================
* PARTIE A : CONSTRUCTION DE LA BASE ANALYTIQUE
*=====================================================================

*-----------------------------------------------------------
* ETAPE 1 : BASE MENAGE - IDENTIFIANTS, POIDS, GEOGRAPHIE
*-----------------------------------------------------------
use "C:\Users\Mon ordi\Desktop\PROJET ANALYSE P\BASED\hh_sec_a.dta",clear
isid y5_hhid
save "$temp\base_a.dta", replace

*-----------------------------------------------------------
* ETAPE 2 : IDENTIFICATION DU CHEF DE MENAGE (SEC B)
*-----------------------------------------------------------
use "C:\Users\Mon ordi\Desktop\PROJET ANALYSE P\BASED\hh_sec_b.dta", clear

* Taille du menage (avant de filtrer sur le chef)
bys y5_hhid: gen hhsize = _N

* hh_b05==1 = chef de menage (confirme : merge final = 4709 obs, coherent)
keep if hh_b05 == 1

rename hh_b02 sexe_cm
rename hh_b04 age_cm
label define sexe_lbl 1 "Homme" 2 "Femme"
label values sexe_cm sexe_lbl

keep y5_hhid hhsize sexe_cm age_cm indidy5
save "$temp\base_cm_demo.dta", replace

*-----------------------------------------------------------
* ETAPE 3 : INSTRUCTION DU CHEF DE MENAGE (SEC C)
*-----------------------------------------------------------
use "C:\Users\Mon ordi\Desktop\PROJET ANALYSE P\BASED\hh_sec_c.dta", clear
merge 1:1 y5_hhid indidy5 using "$temp\base_cm_demo.dta", keep(match) nogen
save "$temp\base_cm_complete.dta", replace
gen education1 = 1 if hhsize > 2 & educaR_pae != 0
replace education1 = 0 if education1 == .
*-----------------------------------------------------------
* ETAPE 4 : INDICATEUR DE BIEN-ETRE (consumption_real_y5)
*-----------------------------------------------------------
use "C:\Users\Mon ordi\Desktop\PROJET ANALYSE P\BASED\consumption_real_y5.dta", clear
save "$temp\base_conso.dta", replace
*-----------------------------------------------------------
* ETAPE 5 : FUSION FINALE - BASE MENAGE ANALYTIQUE
*-----------------------------------------------------------
use "$temp\base_a.dta", clear
merge 1:1 y5_hhid using "$temp\base_cm_complete.dta", nogen
merge 1:1 y5_hhid using "$temp\base_conso.dta", nogen

count
isid y5_hhid

save "$data\base_analytique_TZA.dta", replace

*Merging de la base individu et la base menage
use "C:\Users\Mon ordi\Desktop\PROJET ANALYSE P\BASED\npsy5.panel.key.dta",clear
merge m:1 y5_hhid using "$data\base_analytique_TZA.dta"

use "C:\Users\Mon ordi\Desktop\PROJET ANALYSE P\BASED\consumption_real_y5.dta"

*Seuil HBF 2017-2018
gen z_ref = 49320

*Variable de pauvreté
gen pauv=(expmR_pae<z_ref)

* 1-Calcul mannuelle des indicateurs avec les intervalles de confiance

*P0
mean pauv [pw=hhweight2]
*P1
gen gap = ((z_ref-expmR_pae)/z_ref) if expmR_pae<z_ref
replace gap = 0 if expmR_pae>=z_ref
mean gap [pw=hhweight2]
*P2
gen gap2 = (((z_ref-expmR_pae)/z_ref)^2) if expmR_pae<z
replace gap2 = 0 if expmR_pae>=z_ref

mean gap2  [pw=hhweight2]

*2 Indicateur National de pauvrete 
lorenz expmR_pae
lorenz graph
*Analyse de pauvrete Mainland et Zanzibar
lorenz expmR_pae,  over(mainland)
lorenz graph, overlay

*3 Taux de pauvrete et indice d'inegalite par mainland/Zanzibar
*Indice de GINI par regions
statsby gini=r(gini), by(region): ineqdeco expmR_pae
graph hbar (mean) gini, over(region,label(labsize(vsmall))) ytitle("Indice de Gini") title("Inégalité par région") 


graph export "C:\Users\Mon ordi\Desktop\PROJET ANALYSE P\graph_lrenzmainland.pdf"

*Inegalites
ineqdeco expmR_pae, by(urban)
ineqdeco expmR_pae, by(region)
ineqdeco expmR_pae, by(femalehead)
*Taux de pauvreté
mean pauv [pw=hhweight2], over(region)
mean pauv [pw=hhweight2], over(urban)
mean pauv [pw=hhweight2], over(femalehead)

*=====================================================================
NIVEAU D'INSTRUCTION DU CHEF DE MENAGE
*=====================================================================
* Recodage des variables binaires en 1=Oui/0=Non (au lieu de 1/2)
recode hh_c03 (1=1) (2=0), gen(ever_school)
label define yn01 0 "Non" 1 "Oui"
label values ever_school yn01

gen instr_cm = .
replace instr_cm = 0 if ever_school==0
replace instr_cm = 1 if inlist(hh_c07,1,11,12,13,14,15,16,17,18,19,20)
replace instr_cm = 2 if inlist(hh_c07,2,21,22,23,24,25)
replace instr_cm = 3 if inlist(hh_c07,31,32,33)
replace instr_cm = 4 if inlist(hh_c07,34,41,42,43,44,45)

label define instr_lbl 0 "Aucune instruction" 1 "Primaire" 2 "Secondaire 1er cycle" 3 "Secondaire 2nd cycle" 4 "Superieur"
label values instr_cm instr_lbel
*Taux de pauvrete et inegalite par niveau d'education du chef de menage
ineqdeco expmR_pae, by(instr_cm)
mean pauv [pw=hhweight2], over(instr_cm)

* 4- Modele econometrique analyse des correlattions et testt statistiques
*on sait que expmR_pae explique la variable pauv entierement 

pwcorr expmR_pae hhsize adulteq foodINR_pae, sig
*teest de compraison de propotions
ttest expmR_pae, by(pauv)
ttest hhsize, by(pauv)
ttest education1, by(pauv)
*modele logit
logit pauv hhsize adulteq femalehead instr_cm urban mainland, vce(robust)
margins, dydx(*)



