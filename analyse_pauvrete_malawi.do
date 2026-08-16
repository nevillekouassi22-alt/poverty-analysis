***********************************************
*** PROJET : PROFIL ET DÉTERMINANTS DE LA   ***
***          PAUVRETÉ AU MALAWI (IHS-5)     ***
*** Année académique 2025-2026 – ISE2/ENSEA ***
*** Date : Juin 2026                        ***
***                                         ***
*** NOTE : Ce fichier .do est fourni pour   ***
*** la réplicabilité des résultats. L'analyse***
*** principale a été conduite en Python.    ***
*** Les commandes Stata équivalentes sont   ***
*** présentées ci-dessous.                  ***
***********************************************

*** 0. CHARGEMENT DE LA BASE DE DONNÉES
* Charger l'agrégat de consommation IHS-5
import delimited "ihs5_consumption_aggregate.csv", clear

*** 1. PRÉPARATION DES VARIABLES
* Indicateur de pauvreté (déjà dans la base comme "poor")
* Seuil national : pline = 165878.859375 MWK/personne/an
* Dépense per capita réelle : rexpaggpc

* Milieu de résidence (1=Urbain, 2=Rural)
label define milieu_lbl 1 "Urbain" 2 "Rural"
label values urban milieu_lbl
rename urban milieu

* Région
label define region_lbl 1 "Nord" 2 "Centre" 3 "Sud"
label values region region_lbl

* Poids population
gen poids_pop = hh_wgt * hhsize

*** 2. INDICES FGT NATIONAUX AVEC IC À 95%
* Installer mpovlive si nécessaire
* ssc install mpovlive, replace

* Indices P0, P1, P2 nationaux
mpovlive rexpaggpc [w = poids_pop], varpl(pline)
mpovlive rexpaggpc [w = poids_pop], in(fgt0 fgt1 fgt2) varpl(pline)

* Taux d'extrême pauvreté
gen upoor_calc = (rexpaggpc < upline)
mean upoor_calc [pw = poids_pop]

*** 3. INDICES D'INÉGALITÉ
* Installer ineqdeco si nécessaire  
* ssc install ineqdeco, replace
ineqdeco rexpaggpc [pw = poids_pop]

* =====================================================
* FIGURE 3 : Parts des quintiles (barres) +
*            Gini par milieu (urbain / rural)
* =====================================================

* -- (a) Barres : part de chaque quintile --
preserve
    collapse (sum) dep_quintile = rexpaggpc [aw = poids_pop], by(quintile)
    egen dep_total2 = total(dep_quintile)
    gen part = dep_quintile / dep_total2 * 100

    label define qlbl 1 "Q1 (20% les plus pauvres)" ///
                      2 "Q2" 3 "Q3" 4 "Q4" ///
                      5 "Q5 (20% les plus riches)"
    label values quintile qlbl

    graph bar part, over(quintile, label(angle(15) tsize(small))) ///
        title("Répartition des dépenses par quintile", size(medium)) ///
        ytitle("Part dans les dépenses totales (%)") ///
        bar(1, color(navy)) bar(2, color(blue)) bar(3, color(ltblue)) ///
        bar(4, color(orange)) bar(5, color(red)) ///
        blabel(bar, format(%4.1f) suffix("%")) ///
        note("Source : IHS-5 2019-2020, NSO Malawi. Calculs auteur.")
    graph export "fig3a_quintiles.png", replace width(1200)
restore

* -- (b) Gini par milieu de résidence --
* urban == 1 : Urbain | urban == 2 : Rural
preserve
    * Gini urbain
    ineqdeco rexpaggpc [aw = poids_pop] if urban == 1
    scalar gini_urb = r(gini)

    * Gini rural
    ineqdeco rexpaggpc [aw = poids_pop] if urban == 2
    scalar gini_rur = r(gini)

    * Affichage
    di "Gini Urbain = " gini_urb
    di "Gini Rural  = " gini_rur
    * → Urbain ≈ 0,390 ; Rural ≈ 0,332
restore

* Courbe de Lorenz
* ssc install lorenz, replace
lorenz rexpaggpc [pw = poids_pop], gini
lorenz graph

*** 4. PAUVRETÉ PAR MILIEU
mpovlive rexpaggpc [w = poids_pop], in(fgt0 fgt1 fgt2) varpl(pline) by(milieu)
ineqdeco rexpaggpc [pw = poids_pop], by(milieu)
lorenz rexpaggpc [pw = poids_pop], gini over(milieu)
lorenz graph, overlay

*** 5. PAUVRETÉ PAR RÉGION
mpovlive rexpaggpc [w = poids_pop], in(fgt0 fgt1 fgt2) varpl(pline) by(region)
ineqdeco rexpaggpc [pw = poids_pop], by(region)

*** 6. PAUVRETÉ PAR DISTRICT
mpovlive rexpaggpc [w = poids_pop], in(fgt0 fgt1 fgt2) varpl(pline) by(district)

*** 7. FUSION AVEC LES CARACTÉRISTIQUES DU CM
* Charger les données du module B (roster des membres)
preserve
import delimited "HH_MOD_B.csv", clear
keep if PID == 1  // Chef de ménage uniquement
rename hh_b03 sexe_cm
rename hh_b05a age_cm
keep case_id sexe_cm age_cm
tempfile chef_b
save `chef_b'
restore

* Charger le module C (éducation)
preserve
import delimited "HH_MOD_C.csv", clear
keep if PID == 1
rename hh_c09 niveau_instruction
keep case_id niveau_instruction
tempfile chef_c
save `chef_c'
restore

* Fusion
merge 1:1 case_id using `chef_b', nogen
merge 1:1 case_id using `chef_c', nogen

*** 8. PAUVRETÉ PAR GENRE DU CM
gen cm_femme = (sexe_cm == "FEMALE")
label define genre_lbl 0 "Masculin" 1 "Féminin"
label values cm_femme genre_lbl
mpovlive rexpaggpc [w = poids_pop], in(fgt0 fgt1 fgt2) varpl(pline) by(cm_femme)
ineqdeco rexpaggpc [pw = poids_pop], by(cm_femme)

*** 9. PAUVRETÉ PAR NIVEAU D'INSTRUCTION
* Créer la variable catégorielle d'éducation
gen educ_cat = 0  // Aucun (NONE)
replace educ_cat = 1 if inlist(niveau_instruction, "PSLC")
replace educ_cat = 2 if inlist(niveau_instruction, "JCE", "MSCE/GCSE", "A-LEVEL")
replace educ_cat = 3 if inlist(niveau_instruction, "DIPLOMA", "DEGREE", "MASTERS", "PhD")
label define educ_lbl 0 "Aucun" 1 "Primaire" 2 "Secondaire" 3 "Supérieur"
label values educ_cat educ_lbl
mpovlive rexpaggpc [w = poids_pop], in(fgt0 fgt1 fgt2) varpl(pline) by(educ_cat)
ineqdeco rexpaggpc [pw = poids_pop], by(educ_cat)

*** 10. MODÈLE LOGIT - DÉTERMINANTS DE LA PAUVRETÉ
gen rural = (milieu == 2)
gen ln_hhsize = log(hhsize)
gen region_centre = (region == 2)
gen region_sud = (region == 3)
gen educ_primaire = (educ_cat == 1)
gen educ_secondaire = (educ_cat == 2)
gen educ_superieur = (educ_cat == 3)
destring age_cm, replace force

* Estimation Logit
logit poor rural cm_femme ln_hhsize age_cm region_centre region_sud ///
      educ_primaire educ_secondaire educ_superieur [pw = hh_wgt]

* Effets marginaux moyens (AME)
margins, dydx(*) atmeans

* Pseudo-R² de McFadden
estat ic

*** 11. GRAPHIQUES
* Distribution des dépenses
twoway (histogram rexpaggpc if poor==1 [fw=poids_pop], fcolor(red%60) lcolor(none)) ///
       (histogram rexpaggpc if poor==0 [fw=poids_pop], fcolor(blue%60) lcolor(none)), ///
       xline(165878.86, lcolor(black) lpattern(dash)) ///
       legend(label(1 "Pauvres") label(2 "Non-pauvres") label(3 "Seuil")) ///
       title("Distribution de la dépense per capita — Malawi IHS-5") ///
       xtitle("Dépense réelle per capita (MWK)") ytitle("Densité")

* Diagramme en barres P0 par milieu
graph bar poor [pw=poids_pop], over(milieu) ///
    title("Incidence de la pauvreté par milieu") ///
    ytitle("Taux de pauvreté (P0)")

***********************************************
*** FIN DU FICHIER DO
***********************************************
