clear
drop _all
set more off


use climatechangedietdata

*******************************
****GRAPHING OPTIONS***********
*******************************


grstyle init

grstyle set plain

//graph set window fontface "Helvetica"

*******************************
********VARIABLES**************
*******************************


***diet change
gen delta_diet = currentdiet - diet_change


**ecological concern
gen priors = (prior1 + prior2 + prior3 + prior4 + prior5 + prior6 + prior7)/7

****efficacy beliefs
gen pre_efficacy = (efficacy_pre1 + (6-efficacy_pre2))/2
gen post_efficacy = (efficacy_post1 + (6-efficacy_post2))/2
gen delta_efficacy = post_efficacy - pre_efficacy

** difference in issue imporatance
gen delta_agree = agreepost - agreepre



***categories of pre-intervention diet
gen prediet = 1 if currentdiet==0|currentdiet==1
replace prediet = 2 if currentdiet==2|currentdiet==3
replace prediet = 3 if currentdiet==4|currentdiet==5
replace prediet = 4 if currentdiet==6|currentdiet==7



**Control variables
gen female=gender==2
gen democrat=political_belief==1
gen edu =education>2



**targetted approach
univar currentdiet //median is 4 days
gen highprediet=currentdiet>=4
univar priors //median is 4.14
gen highpriors=priors>=4.14
univar pre_efficacy //median is 4
gen highpreefficacy=pre_efficacy>=4




tab treat
**Control (treat=0) - 177
**More Scientific Information (treat=1) - 175
**Efficacy Salience (treat=2) - 179
**Health Information (treat=3) - 174
**Animal Welfare(treat=4) - 177
**Social Norms (treat=5) - 164
**Social Efficacy (treat=6) - 174


**treatment vs control comparisons
gen morescience=1 if treat==1
replace morescience=0 if treat==0 //more science vs control

gen salience=1 if treat==2
replace salience=0 if treat==0 //efficacy salience vs control

gen health=1 if treat==3
replace health=0 if treat==0 //health vs control

gen animal=1 if treat==4
replace animal=0 if treat==0 //animal welfare vs control

gen norms=1 if treat==5
replace norms=0 if treat==0 //social norms vs control

gen socialefficacy=1 if treat==6
replace socialefficacy=0 if treat==0 //social efficacy vs control



tab disease // 725 said yes and 495 said no.


gen treatment_group = treat //this is only for drawing graphs



******************STANDARDIZING VARIABLES********************

foreach i in delta_diet priors age currentdiet morality pre_efficacy {
egen `i'_z = std(`i')
}



**********VARIABLE LABELS****************************************

label define labeltreat 0 "Control" 1 `" "More Scientific" "information" "' 2 `" "Efficacy" "salience" "' 3 `" "Health" "information" "'  4 `" "Animal"  "welfare" "' 5 `" "Social" "norms" "' 6 `" "Social" "efficacy" "'
label values treat labeltreat


**for treatment groups (only for plotting graphs)
label define treatment_groupl 0 "Control" 1 "More scientific information"  2 "Efficacy salience" 3 "Health information" 4 "Animal welfare" 5 "Social norms" 6 "Social efficacy"
label values treatment_group treatment_groupl

***Note - treat and treatment_group are the exact same variables. They are just labelled a little differently. Either one is used depending on which makes the graph look better.

label values prediet labprediet
label def labprediet 1 "0-1 Days", modify
label def labprediet 2 "2-3 Days", modify
label def labprediet 3 "4-5 Days", modify
label def labprediet 4 "6-7 Days", modify




label variable delta_diet "Change in days of weekly meat consumption"
label variable donate "Donation to charity"
label variable priors "Prior Ecological Concern"
label variable currentdiet "Pre-intervention diet"
label variable pre_efficacy "Baseline Efficacy Beliefs"
label variable diet_change "Post-intervention diet"
label variable post_efficacy "Post-intervention Efficacy Beliefs"
label variable age "Age"
label variable female "Female"
label variable democrat "Democrat"
label variable edu "Education"
label variable morality "Moral offence taken"

label define diseasel 0 "No prior history of diseases" 1 "Prior history of diseases" 
label values disease diseasel

label define highpreefficacyl 0 "Low baseline efficacy" 1 "High baseline efficacy" 
label values highpreefficacy highpreefficacyl


label define highpredietl 0 "Infrequent meat eater" 1 "Frequent meat eater" 
label values highprediet highpredietl






*******TEXT ANALYSIS******************
foreach var of varlist action1 action2 action3 {
gen Z=lower(`var')
drop `var'
rename Z `var'
}

foreach i in 1 2 3 {
gen prior_diet_kno`i' =  strpos(action`i', "meat") > 0 | strpos(action`i', " vegetarian") >0 | strpos(action`i', "vegan") >0 | strpos(action`i', "beef") | strpos(action`i', "plant-based")| strpos(action`i', "plant based") | strpos(action`i', "vegeterian")
}


gen prior_diet_kno = prior_diet_kno1==1 | prior_diet_kno2==1 |prior_diet_kno3==1 


 tab prior_diet_kno
 //111 mentioned  vegetarian/meat etc, rest 1109 did not.
 
 foreach var of varlist post_action1 post_action2 post_action3 {
gen Z=lower(`var')
drop `var'
rename Z `var'
}

foreach i in 1 2 3 {
gen post_diet_kno`i' =  strpos(post_action`i', "meat") > 0 | strpos(post_action`i', " vegetarian") >0 | strpos(post_action`i', "vegan") >0 | strpos(post_action`i', "beef") | strpos(post_action`i', "plant-based")| strpos(post_action`i', "plant based") | strpos(post_action`i', "vegeterian")
}


gen post_diet_kno = post_diet_kno1==1 | post_diet_kno2==1 |post_diet_kno3==1 

tab post_diet_kno //633 mentioned meat, 587 did not.

gen delta_kno = post_diet_kno - prior_diet_kno



 
 ********SUMMARY STATISTICS************************
 
sutex delta_diet donate priors currentdiet diet_change pre_efficacy post_efficacy disease age female democrat edu morality , lab nobs key(descstat1)  replace ///
file(descstat1.tex)  minmax  title ("Summary Statistics")



*************BALANCE TESTS*********

oneway priors treatment_group, tabulate //no difference across groups
oneway currentdiet treatment_group, tabulate //no difference across groups
oneway pre_efficacy treatment_group, tabulate //no difference across groups
oneway age treatment_group, tabulate //no difference across groups
oneway female treatment_group, tabulate //significant at 10% level
oneway democrat treatment_group, tabulate //no difference across groups
oneway edu treatment_group, tabulate //no difference across groups



*******FIGURE FOR DIFFERENCE BETWEEN POST-INTERVENTION AND BASELINE BELIEFS AND PRACTICES****************

gen reverse_delta_diet = diet_change - currentdiet

cibar delta_kno, over1(treatment_group) level(90) barcolor(emerald lavender gold cranberry dkorange olive_teal ebblue) bargap(20)  graphopts( ytitle("Post-intervention - Baseline", size(medlarge)) title("(a) Proportion mentioning diet", size(large)) legend(off) xlabel(1.2 "Control" 2.4 `" "More" "scientific" "information" "'3.6 `" "Efficacy" "salience" "' 4.8 `" "Health"  "information" "' 6 `" "Animal" "welfare" "' 7.2 `" "Social" "norms" "' 8.4 `" "Social" "efficacy" "',labsize(small) ) ylabel(0(.1).7) name(g1, replace)) ciopts(lcolor(black)) 
cibar delta_agree, over1(treatment_group) level(90) barcolor(emerald lavender gold cranberry dkorange olive_teal ebblue) graphopts( ytitle("Post-intervention - Baseline", size(medlarge)) title("(b) Issue importance", size(large)) legend(off) xlabel(1.2 "Control" 2.4 `" "More" "scientific" "information" "'3.6 `" "Efficacy" "salience" "' 4.8 `" "Health"  "information" "' 6 `" "Animal" "welfare" "' 7.2 `" "Social" "norms" "' 8.4 `" "Social" "efficacy" "',labsize(small)  ) name(g2, replace) ylabel(0(.1).7)) ciopts(lcolor(black)) bargap(20)
cibar reverse_delta_diet, over1(treatment_group) level(90) barcolor(emerald lavender gold cranberry dkorange olive_teal ebblue) graphopts( ytitle("Post-intervention - Baseline", size(medlarge)) title("(c) Weekly meat intake (days/week)", size(large)) legend(off) xlabel(1.2 "Control" 2.4 `" "More" "scientific" "information" "'3.6 `" "Efficacy" "salience" "' 4.8 `" "Health"  "information" "' 6 `" "Animal" "welfare" "' 7.2 `" "Social" "norms" "' 8.4 `" "Social" "efficacy" "',labsize(small)  ) name(g3, replace) ysc(reverse) ylabel(0(.2)-1)) ciopts(lcolor(black)) bargap(20)
cibar delta_efficacy, over1(treatment_group) level(90) barcolor(emerald lavender gold cranberry dkorange olive_teal ebblue) graphopts(ytitle( "Post-intervention - Baseline", size(medlarge)) title("(d) Efficacy beliefs", size(large)) legend(off) xlabel(1.2 "Control" 2.4 `" "More" "scientific" "information" "'3.6 `" "Efficacy" "salience" "' 4.8 `" "Health"  "information" "' 6 `" "Animal" "welfare" "' 7.2 `" "Social" "norms" "' 8.4 `" "Social" "efficacy" "',labsize(small)  ) name(g4, replace) ylabel(-.1(.1).5)  ) ciopts(lcolor(black)) bargap(20) 


graph combine g1 g2 g3 g4, xsize(15) ysize(8.5) note("90% confidence intervals")

 
*******FIGURE FOR POST-INTERVENTION AND BASELINE BELIEFS AND PRACTICES****************

ciplot prior_diet_kno post_diet_kno, by(treatment_group) recast(bar) barwidth(2 2) color(red%70 blue%70) ylabel(0(.1).7) legend(order(2 "Baseline" 3 "Post-intervention") size(large)) ytitle("Proportion that mentioned diet", size(large)) xtitle("") xsize(7) xlabel(,labsize(medlarge) alt) ylabel(,labsize(large))level(90) note("90% confidence intervals", size(medium))

ciplot agreepre agreepost, by(treatment_group) recast(bar) barwidth(2 2) color(red%70 blue%70) ylabel(3(.5)5) legend(order(2 "Baseline" 3 "Post-intervention") size(large)) ytitle ("Issue importance", size(large)) xtitle("") xsize(7) xlabel(,labsize(medlarge) alt) ylabel(,labsize(large)) level(90) note("90% confidence intervals", size(medium))

ciplot currentdiet diet_change, by(treatment_group) recast(bar) barwidth(2 2) color(red%70 blue%70) ylabel(3(.5)5) legend(order(2 "Baseline" 3 "Post-intervention") size(large)) ytitle("Weekly meat consumption (days)", size(large))  xtitle("") xsize(7) xlabel(,labsize(medlarge) alt) ylabel(,labsize(large)) level(90) note("90% confidence intervals", size(medium))

ciplot pre_efficacy post_efficacy, by(treatment_group) recast(bar) barwidth(2 2) color(red%70 blue%70) ylabel(3(.5)5) legend(order(2 "Baseline" 3 "Post-intervention") size(large)) ytitle("Efficacy beliefs", size(large)) xtitle("") xsize(7) xlabel(,labsize(medlarge) alt) ylabel(,labsize(large)) level(90) note("90% confidence intervals", size(medium))


*****BETWEEN GROUP PAIRED T-TESTS***************

ttest prior_diet_kno==post_diet_kno if treat==0 // p-value = 0.000
ttest prior_diet_kno==post_diet_kno if treat==1   // p-value = 0.000
ttest prior_diet_kno==post_diet_kno if treat==2   // p-value = 0.000
ttest prior_diet_kno==post_diet_kno if treat==3   // p-value = 0.000
ttest prior_diet_kno==post_diet_kno if treat==4   // p-value = 0.000
ttest prior_diet_kno==post_diet_kno if treat==5   // p-value = 0.000
ttest prior_diet_kno==post_diet_kno if treat==6   // p-value = 0.000
 
ttest agreepre==agreepost if treat==0 // p-value = 0.000
ttest agreepre==agreepost if treat==1 // p-value = 0.000
ttest agreepre==agreepost if treat==2 // p-value = 0.000
ttest agreepre==agreepost if treat==3 // p-value = 0.000
ttest agreepre==agreepost if treat==4  // p-value = 0.000
ttest agreepre==agreepost if treat==5  // p-value = 0.000
ttest agreepre==agreepost if treat==6  // p-value = 0.000


ttest delta_agree, by(animal) // t-statistic 2.1730 and p-value 0.0304


ttest currentdiet==diet_change if treat==0 // p-value = 0.000
ttest currentdiet==diet_change if treat==1   // p-value = 0.000
ttest currentdiet==diet_change if treat==2   // p-value = 0.000
ttest currentdiet==diet_change if treat==3   // p-value = 0.000
ttest currentdiet==diet_change if treat==4   // p-value = 0.000
ttest currentdiet==diet_change if treat==5   // p-value = 0.000
ttest currentdiet==diet_change if treat==6   // p-value = 0.000
 

ttest pre_efficacy==post_efficacy if treat==0 //p-value 0.0044
ttest pre_efficacy==post_efficacy if treat==1 // p-value 0.0945
ttest pre_efficacy==post_efficacy if treat==2 //p-value 0.0151
ttest pre_efficacy==post_efficacy if treat==3 //p-value 0.0015
ttest pre_efficacy==post_efficacy if treat==4 //p-value 0.0790
ttest pre_efficacy==post_efficacy if treat==5 //p-value 0.4728
ttest pre_efficacy==post_efficacy if treat==6 //p-value 0.5480

******Baseline meat intake vs issue importance

pwcorr currentdiet agreepre, sig
//coefficient -0.2848 with p-value 0.0000

pwcorr currentdiet agreepost, sig
//coefficient -0.1846  with p-value 0.0000

pwcorr currentdiet delta_agree, sig
//coefficient 0.1544 with p-value 0.0000

catplot agreepre prediet, percent(prediet) recast(bar)  ytitle("Percentage of Subjects", size(large)) b1title("Baseline meat intake (days/week)", size(large)) l1title(, pos(12)) asyvars stack legend(on order(1 "Strongly Disagree" 2 "Mildly Disagree"  3 "Neither Agree nor Disagree" 4 "Midly Agree" 5 "Strongly Agree") size(medlarge))var2opts(label(labsize(*1.5))) ylabel(, labsize(large)) 

catplot agreepost prediet, percent(prediet) recast(bar)  ytitle("Percentage of Subjects", size(large)) b1title("Baseline meat intake (days/week)", size(large)) l1title(, pos(12)) asyvars stack legend(on order(1 "Strongly Disagree" 2 "Mildly Disagree"  3 "Neither Agree nor Disagree" 4 "Midly Agree" 5 "Strongly Agree") size(medlarge))var2opts(label(labsize(*1.5))) ylabel(, labsize(large))



******Baseline meat intake vs efficacy beliefs


pwcorr currentdiet pre_efficacy, sig
//coefficient -0.1107 with p-value 0.0001

pwcorr currentdiet post_efficacy, sig
//coefficient -0.1140 with p-value 0.0001


pwcorr currentdiet delta_efficacy, sig
// insignificant



******Baseline meat intake vs prior ecological concern

pwcorr currentdiet priors, sig
//coefficient -0.2034 with p-value 0.0000



***Prior ecological concern corrleation with concern and friend concern

pwcorr priors concern, sig // r= 0.6633 with p=0.0000
pwcorr priors friendconcern, sig // r= 0.4670 with p=0.0000




************************************************
************TREATMENT EFFECTS*******************
************************************************ 
 
************ALL TREATMENTS***************************

eststo r1:reg delta_diet i.treat, robust
eststo r2:reg delta_diet i.treat priors_z currentdiet_z pre_efficacy_z, robust
eststo r3:reg delta_diet i.treat priors_z currentdiet_z pre_efficacy_z age_z female democrat edu, robust
coefplot, vertical drop(_cons priors_z currentdiet_z pre_efficacy_z age_z female democrat edu) yline(0, lcolor(orange)) msymbol(S) mcolor(cranberry) ciopts(recast(rcap)) mlabel(string(@b,"%9.3f")) yaxis(1 2) mlabsize(large) mlabposition(2) ytitle("Reduction in meat intake (days/week)", size(large) axis(2)) ytitle("CO{subscript:2} (Kg/year)", size(large) axis(1)) xsize(6) xsc(r(1 6.8)) ysc(r(-.4 .8)) ylabel(-.4(.2).8, axis(2) labsize(*1.29)) ylabel(.8 "128.8" .6 "96.6" .4 "64.4" .2 "32.2" 0 "0" -.2 "-32.2" -.4 "-64.4", axis(1) labsize(*1.27)) level(90) note("90% confidence intervals",size(medsmall))


esttab r1 r2 r3 using table1.tex, varwidth(25) nobaselevels replace se(4) b(4) noconstant booktabs nogaps star(* 0.10 ** 0.05 *** 0.01) title("Impact of information interventions on reduction in number of days of meat consumption per week")nomtitle mgroup("Dependent Variable: $\Delta$ Days", pattern(1 0 0 ) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))coeflabel(1.treat "More scientific information" 2.treat "Efficacy salience" 3.treat "Health information" 4.treat "Animal welfare" 5.treat "Social norms" 6.treat "Social efficacy"   priors_z "Prior Ecological Concern" currentdiet_z "Pre-intervention Diet" pre_efficacy_z "Baseline Efficacy Beliefs"  age_z "Age" female "Female" democrat "Democrat" edu "Education")


cibar delta_diet, over1(treatment_group) level(90) barcolor(emerald lavender gold cranberry dkorange olive_teal ebblue) graphopts(note("90% confidence intervals", size(medsmall)) ytitle("Mean reduction in meat intake (days/week)", size(medlarge)) ylabel(0(.2)1) legend(off) xlabel(1.2 "Control" 2.4 `" "More" "scientific" "information" "'3.6 `" "Efficacy" "salience" "' 4.8 `" "Health"  "information" "' 6 `" "Animal" "welfare" "' 7.2 `" "Social" "norms" "' 8.4 `" "Social" "efficacy" "',labsize(medsmall))) ciopts(lcolor(black)) bargap(20) 


oneway delta_diet treatment_group, tabulate 

********PROPORTION***********************


gen prop_days = (currentdiet - diet_change)/currentdiet
replace prop_days = 0 if prop_days==.


eststo x1:reg prop_days i.treat, robust
eststo x2:reg prop_days i.treat priors_z currentdiet_z pre_efficacy_z, robust
eststo x3:reg prop_days i.treat priors_z currentdiet_z pre_efficacy_z age_z female democrat edu, robust


esttab x1 x2 x3 using tablex.tex, varwidth(25) nobaselevels replace se(4) b(4) noconstant booktabs nogaps star(* 0.10 ** 0.05 *** 0.01) title("Impact of information interventions on proportionate reduction in number of days of meat consumption per week")nomtitle mgroup("Dependent Variable: Prop_Days", pattern(1 0 0 ) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))coeflabel(1.treat "More scientific information" 2.treat "Efficacy salience" 3.treat "Health information" 4.treat "Animal welfare" 5.treat "Social norms" 6.treat "Social efficacy" priors_z "Prior Ecological Concern" currentdiet_z "Pre-intervention Diet" pre_efficacy_z "Baseline Efficacy Beliefs"  age_z "Age" female "Female" democrat "Democrat" edu "Education")

*************HEALTH INTERACTION***********************


eststo p1:reg delta_diet health##disease, robust
eststo p2:reg delta_diet health##disease priors_z currentdiet_z pre_efficacy_z, robust
eststo p3:reg delta_diet health##disease priors_z currentdiet_z pre_efficacy_z age_z female democrat edu, robust

esttab p1 p2 p3 using table2.tex, varwidth(25) nobaselevels replace se(4) b(4) noconstant booktabs nogaps star(* 0.10 ** 0.05 *** 0.01) title("Impact of health information intervention on reduction in number of days of meat consumption per week depending on history of diseases in the family")nomtitle mgroup("Dependent Variable: $\Delta$ Days", pattern(1 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))coeflabel(1.health "Health information" 1.disease "Disease"   1.health#1.disease "Health information $\times$ Disease" priors_z "Prior Ecological Concern" currentdiet_z "Pre-intervention Diet" pre_efficacy_z "Baseline Efficacy Beliefs"  age_z "Age" female "Female" democrat "Democrat" edu "Education")order(1.health#1.disease  1.health 1.disease priors_z currentdiet_z pre_efficacy_z age_z female democrat edu)



*****seperately for people with history of family diseases

eststo s1:reg delta_diet i.treat if disease==1, robust
eststo s2:reg delta_diet i.treat priors_z currentdiet_z pre_efficacy_z age_z female democrat edu if disease==1, robust 
eststo s3:reg delta_diet i.treat if disease==0, robust
eststo s4:reg delta_diet i.treat priors_z currentdiet_z pre_efficacy_z age_z female democrat edu if disease==0, robust 

esttab s1 s2 s3 s4 using table3.tex, varwidth(25) nobaselevels replace se(4) b(4) noconstant booktabs nogaps star(* 0.10 ** 0.05 *** 0.01) title("Impact of information interventions on reduction in number of days of meat consumption per week depending on history of diseases in the family")nomtitle mgroup("Dependent Variable: $\Delta$ Days", pattern(1 0 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))coeflabel(1.treat "More scientific information" 2.treat "Efficacy salience" 3.treat "Health information" 4.treat "Animal welfare" 5.treat "Social norms" 6.treat "Social efficacy" priors_z "Prior Ecological Concern" currentdiet_z "Pre-intervention Diet" pre_efficacy_z "Baseline Efficacy Beliefs"  age_z "Age" female "Female" democrat "Democrat" edu "Education")


*****coefplot

reg delta_diet i.treat priors_z currentdiet_z pre_efficacy_z age_z female democrat edu if disease==1, robust
coefplot, vertical drop(_cons priors_z currentdiet_z pre_efficacy_z age_z female democrat edu) yline(0, lcolor(orange)) msymbol(S) mcolor(cranberry) ciopts(recast(rcap)) mlabel(string(@b,"%9.3f")) yaxis(1 2) mlabsize(large) mlabposition(2) ytitle("Reduction in meat intake (days/week)", size(large) axis(2)) ytitle("CO{subscript:2} (Kg/year)", size(large) axis(1)) xsize(6) xsc(r(1 6.8)) ysc(r(-.4 .8)) ylabel(-.4(.2).8, axis(2) labsize(*1.29)) ylabel(.8 "128.8" .6 "96.6" .4 "64.4" .2 "32.2" 0 "0" -.2 "-32.2" -.4 "-64.4", axis(1) labsize(*1.27)) level(90) note("90% confidence intervals",size(medsmall))


*****means graph
cibar delta_diet if disease==1, over1(health) level(90) barcolor(emerald cranberry) graphopts(legend( order(1 "Control" 2 "Health Information") ) note("90% confidence intervals", size(medsmall)) ytitle("Mean reduction in meat intake (days/week)", size(medlarge)) ylabel(0(.2)1))

ttest delta_diet if disease==1, by(health) //significant with p-value =0.0186


**interaction term graph


label define healthl 0 "Control" 1  `" "{lalign 10: Health}" "{lalign 22: Information}" "'
label values health healthl


reg delta_diet health##disease, robust
margins health#disease, asbalanced
marginsplot, title("Adjusted Predictions of" "Health Information # Disease with 90% CIs", size(vlarge )) xtitle("") ytitle(,size(large ))ylabel(,labsize(medlarge)) ylabel(0(.2)1.2)level(90)xlabel(,labsize(medlarge))plotopts(lwidth(medthick))



*********INTERACTION WITH HIGH BASELINE EFFICACY*****************



eststo q1:reg delta_diet i.treat##highpreefficacy, robust
eststo q2:reg delta_diet i.treat##highpreefficacy priors_z currentdiet_z pre_efficacy_z, robust
eststo q3:reg delta_diet i.treat##highpreefficacy priors_z currentdiet_z pre_efficacy_z age_z female democrat edu, robust

esttab q1 q2 q3 using table4.tex, varwidth(25) nobaselevels nolabel replace se(4) b(4) noconstant booktabs nogaps star(* 0.10 ** 0.05 *** 0.01) title("Impact of information interventions on reduction in number of days of meat consumption per week depending on baseline efficacy beliefs")nomtitle mgroup("Dependent Variable: $\Delta$ Days", pattern(1 0 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))coeflabel(1.treat "More scientific information" 2.treat "Efficacy salience" 3.treat "Health information" 4.treat "Animal welfare" 5.treat "Social norms" 6.treat "Social efficacy" 1.highpreefficacy "High Efficacy Beliefs"  1.treat#1.highpreefficacy "More scientific information $\times$ High efficacy beliefs" 2.treat#1.highpreefficacy "Efficacy salience $\times$ High efficacy beliefs" 3.treat#1.highpreefficacy "Health information $\times$ High efficacy beliefs" 4.treat#1.highpreefficacy "Animal welfare $\times$ High efficacy beliefs" 5.treat#1.highpreefficacy "Social norms $\times$ High efficacy beliefs" 6.treat#1.highpreefficacy "Social efficacy $\times$ High efficacy beliefs"    priors_z "Prior Ecological Concern" currentdiet_z "Pre-intervention Diet" pre_efficacy_z "Baseline Efficacy Beliefs"  age_z "Age" female "Female" democrat "Democrat" edu "Education")order(1.treat#1.highpreefficacy 2.treat#1.highpreefficacy 3.treat#1.highpreefficacy  4.treat#1.highpreefficacy 5.treat#1.highpreefficacy 6.treat#1.highpreefficacy 1.treat 2.treat 3.treat  4.treat 5.treat 6.treat 1.highpreefficacy priors_z currentdiet_z pre_efficacy_z age_z female democrat edu)


*****coefplot

reg delta_diet i.treat priors_z currentdiet_z pre_efficacy_z age_z female democrat edu if highpreefficacy==1, robust
coefplot, vertical drop(_cons priors_z currentdiet_z pre_efficacy_z age_z female democrat edu) yline(0, lcolor(orange)) msymbol(S) mcolor(cranberry) ciopts(recast(rcap)) mlabel(string(@b,"%9.3f")) yaxis(1 2) mlabsize(large) mlabposition(2) ytitle("Reduction in meat intake (days/week)", size(large) axis(2)) ytitle("CO{subscript:2} (Kg/year)", size(large) axis(1)) xsize(6) xsc(r(1 6.8)) ysc(r(-.4 .8)) ylabel(-.4(.2).8, axis(2) labsize(*1.29)) ylabel(.8 "128.8" .6 "96.6" .4 "64.4" .2 "32.2" 0 "0" -.2 "-32.2" -.4 "-64.4", axis(1) labsize(*1.27))level(90) note("90% confidence intervals", size(medsmall))


***********Separately for people with high baseline efficacy beliefs

eststo z1:reg delta_diet i.treat if highpreefficacy==1, robust
eststo z2:reg delta_diet i.treat priors_z currentdiet_z pre_efficacy_z age_z female democrat edu if highpreefficacy==1, robust
eststo z3:reg delta_diet i.treat if highpreefficacy==0, robust
eststo z4:reg delta_diet i.treat priors_z currentdiet_z pre_efficacy_z age_z female democrat edu if highpreefficacy==0, robust


esttab z1 z2 z3 z4 using table5.tex, varwidth(25) nobaselevels replace se(4) b(4) noconstant booktabs nogaps star(* 0.10 ** 0.05 *** 0.01) title("Impact of information interventions on reduction in number of days of meat consumption per week for those with high and low baseline efficacy beliefs")nomtitle mgroup("Dependent Variable: $\Delta$ Days", pattern(1 0 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))coeflabel(1.treat "More scientific information" 2.treat "Efficacy salience" 3.treat "Health information" 4.treat "Animal welfare" 5.treat "Social norms" 6.treat "Social efficacy" priors_z "Prior Ecological Concern" currentdiet_z "Pre-intervention Diet" pre_efficacy_z "Baseline Efficacy Beliefs"  age_z "Age" female "Female" democrat "Democrat" edu "Education")


***Interaction plots

label define moresciencel 0 "Control" 1  `" "{lalign 10: More}" "{lalign 22: Scientific}" "{lalign 25: Information}" "'
label values morescience moresciencel

reg delta_diet morescience##highpreefficacy, robust
margins morescience#highpreefficacy, asbalanced
marginsplot, title("Adjusted Predictions of" "More scientific info # High baseline efficacy with 90% CIs", size(large)) xtitle("")ytitle(,size(large ))ylabel(,labsize(medlarge)) ylabel(0(.5)1.5)level(90)xlabel(,labsize(medlarge))plotopts(lwidth(medthick))legend(size(medlarge))

ttest delta_diet if treat==1, by(highpreefficacy) //significant with p-value =0.0173

label define saliencel 0 "Control" 1  `" "{lalign 14: Efficacy}" "{lalign 14: Salience}" "'
label values salience saliencel

reg delta_diet salience##highpreefficacy, robust
margins salience#highpreefficacy, asbalanced
marginsplot, title("Adjusted Predictions of" "Efficacy salience # High baseline efficacy with 90% CIs", size(large)) xtitle("")ytitle(,size(large ))ylabel(,labsize(medlarge)) ylabel(0(.5)1.5)level(90)xlabel(,labsize(medlarge))plotopts(lwidth(medthick))legend(size(medlarge))

ttest delta_diet if treat==2, by(highpreefficacy) //significant with p-value =0.0001

label define animall 0 "Control" 1  `" "{lalign 11: Animal}" "{lalign 12: Welfare}" "'
label values animal animall

reg delta_diet animal##highpreefficacy, robust
margins animal#highpreefficacy, asbalanced
marginsplot, title("Adjusted Predictions of" "Animal welfare # High baseline efficacy with 90% CIs", size(large)) xtitle("")ytitle(,size(large ))ylabel(,labsize(medlarge)) ylabel(0(.5)1.5)level(90)xlabel(,labsize(medlarge))plotopts(lwidth(medthick))legend(size(medlarge))

ttest delta_diet if treat==4, by(highpreefficacy) //significant with p-value =0.0246

label define normsl 0 "Control" 1  `" "{lalign 10: Social}" "{lalign 9: Norms}" "'
label values norms normsl


reg delta_diet norms##highpreefficacy, robust
margins norms#highpreefficacy, asbalanced
marginsplot, title("Adjusted Predictions of" "Social norms # High baseline efficacy with 90% CIs", size(large)) xtitle("")ytitle(,size(large ))ylabel(,labsize(medlarge)) ylabel(0(.5)1.5)level(90)xlabel(,labsize(medlarge))plotopts(lwidth(medthick))legend(size(medlarge))

ttest delta_diet if treat==5, by(highpreefficacy) //significant with p-value =0.0054


*********INTERACTION WITH HIGH BASELINE DIET*****************

eststo d1:reg delta_diet i.treat##highprediet, robust
eststo d2:reg delta_diet i.treat##highprediet priors_z currentdiet_z pre_efficacy_z, robust
eststo d3:reg delta_diet i.treat##highprediet priors_z currentdiet_z pre_efficacy_z age_z female democrat edu, robust

esttab d1 d2 d3 using table6.tex, varwidth(25) nobaselevels nolabel replace se(4) b(4) noconstant booktabs nogaps star(* 0.10 ** 0.05 *** 0.01) title("Impact of information interventions on reduction in number of days of meat consumption per week depending on baseline frequency of meat consumption")nomtitle mgroup("Dependent Variable: $\Delta$ Days", pattern(1 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))coeflabel(1.treat "More scientific information" 2.treat "Efficacy salience" 3.treat "Health information" 4.treat "Animal welfare" 5.treat "Social norms" 6.treat "Social efficacy" 1.highprediet "Frequent meat eater"  1.treat#1.highprediet "More scientific information $\times$ Frequent meat eater" 2.treat#1.highprediet "Efficacy salience $\times$ Frequent meat eater" 3.treat#1.highprediet "Health information $\times$ Frequent meat eater" 4.treat#1.highprediet "Animal welfare $\times$ Frequent meat eater" 5.treat#1.highprediet "Social norms $\times$ Frequent meat eater"  6.treat#1.highprediet "Social efficacy $\times$ Frequent meat eater"   priors_z "Prior Ecological Concern" currentdiet_z "Pre-intervention Diet" pre_efficacy_z "Baseline Efficacy Beliefs"  age_z "Age" female "Female" democrat "Democrat" edu "Education")order(1.treat#1.highprediet 2.treat#1.highprediet 3.treat#1.highprediet  4.treat#1.highprediet 5.treat#1.highprediet 6.treat#1.highprediet 1.treat 2.treat 3.treat   4.treat 5.treat 6.treat 1.highprediet priors_z currentdiet_z pre_efficacy_z age_z female democrat edu)

*****coefplot

reg delta_diet i.treat priors_z currentdiet_z pre_efficacy_z age_z female democrat edu if highprediet==1, robust
coefplot, vertical drop(_cons priors_z currentdiet_z pre_efficacy_z age_z female democrat edu) yline(0, lcolor(orange)) msymbol(S) mcolor(cranberry) ciopts(recast(rcap)) mlabel(string(@b,"%9.3f")) yaxis(1 2) mlabsize(large) mlabposition(2) ytitle("Reduction in meat intake (days/week)", size(large) axis(2)) ytitle("CO{subscript:2} (Kg/year)", size(large) axis(1)) xsize(6) xsc(r(1 6.8)) ysc(r(-.4 .8)) ylabel(-.4(.2).8, axis(2) labsize(*1.29)) ylabel(.8 "128.8" .6 "96.6" .4 "64.4" .2 "32.2" 0 "0" -.2 "-32.2" -.4 "-64.4", axis(1) labsize(*1.27)) level(90) note("90% confidence intervals", size(medsmall))


***********For frequent meat eaters

eststo x1:reg delta_diet i.treat if highprediet==1, robust
eststo x2:reg delta_diet i.treat priors_z currentdiet_z pre_efficacy_z age_z female democrat edu if highprediet==1, robust
eststo x3:reg delta_diet i.treat if highprediet==0, robust
eststo x4:reg delta_diet i.treat priors_z currentdiet_z pre_efficacy_z age_z female democrat edu if highprediet==0, robust


esttab x1 x2 x3 x4 using table7.tex, varwidth(25) nobaselevels nolabel replace se(4) b(4) noconstant booktabs nogaps star(* 0.10 ** 0.05 *** 0.01) title("Impact of information interventions on reduction in number of days of meat consumption per week for frequent and infrequent meat eaters")nomtitle mgroup("Dependent Variable: $\Delta$ Days", pattern(1 0 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))coeflabel(1.treat "More scientific information" 2.treat "Efficacy salience" 3.treat "Health information" 4.treat "Animal welfare" 5.treat "Social norms" 6.treat "Social efficacy" priors_z "Prior Ecological Concern" currentdiet_z "Pre-intervention Diet" pre_efficacy_z "Baseline Efficacy Beliefs"  age_z "Age" female "Female" democrat "Democrat" edu "Education")


****interaction plots

reg delta_diet animal##highprediet, robust
margins animal#highprediet, asbalanced
marginsplot, title("Adjusted Predictions of" "Animal welfare # Frequent meat eater with 90% CIs", size(large)) xtitle("")ytitle(,size(large ))ylabel(,labsize(medlarge)) ylabel(0(.2)1)level(90)xlabel(,labsize(medlarge))plotopts(lwidth(medthick))legend(size(medlarge))


reg delta_diet norms##highprediet, robust
margins norms#highprediet, asbalanced
marginsplot, title("Adjusted Predictions of" "Social norms # Frequent meat eater with 90% CIs", size(large)) xtitle("")ytitle(,size(large ))ylabel(,labsize(medlarge)) ylabel(0(.2)1)level(90)xlabel(,labsize(medlarge))plotopts(lwidth(medthick))legend(size(medlarge))



*********DONATION BEHAVIOUR*****************

 
probit donate i.treat, robust
eststo c1:margins, dydx(*) post noatlegend
probit donate i.treat priors_z currentdiet_z delta_diet_z pre_efficacy_z age_z female democrat edu, robust
eststo c2:margins, dydx(*) post noatlegend
probit donate i.treat priors_z currentdiet_z delta_diet_z pre_efficacy_z age_z female democrat edu if highpriors==0, robust
eststo c3:margins, dydx(*) post noatlegend
probit donate i.treat priors_z currentdiet_z delta_diet_z pre_efficacy_z age_z female democrat edu if highpriors==1, robust
eststo c4:margins, dydx(*) post noatlegend

esttab c1 c2 c3 c4 using table8.tex, varwidth(25) nobaselevels nolabel replace se(4) b(4) noconstant booktabs nogaps star(* 0.10 ** 0.05 *** 0.01) title("Impact of information interventions on decision to donate bonus earnings")mtitle ("All" "All" "Low Priors" "High Priors") mgroup("Dependent Variable: Prob(donate=1)", pattern(1 0 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))coeflabel(1.treat "More scientific information" 2.treat "Efficacy salience" 3.treat "Health information" 4.treat "Animal welfare" 5.treat "Social norms" 6.treat "Social efficacy" priors_z "Prior Ecological Concern" currentdiet_z "Pre-intervention Diet" delta_diet_z "$\Delta$ Days" pre_efficacy_z "Baseline Efficacy Beliefs"  age_z "Age" female "Female" democrat "Democrat" edu "Education")



cibar donate, over1(treatment_group) level(90) bargap(20) barcolor(emerald lavender gold cranberry dkorange olive_teal ebblue) graphopts(note("90% confidence intervals", size(medsmall)) ytitle(Proportion that donated, size(medlarge)) ylabel(0(.1).5) legend(off) xlabel(1.2 "Control" 2.4 `" "More" "scientific" "information" "'3.6 `" "Efficacy" "salience" "' 4.8 `" "Health"  "information" "' 6 `" "Animal" "welfare" "' 7.2 `" "Social" "norms" "' 8.4 `" "Social" "efficacy" "',labsize(medsmall))) ciopts(lcolor(black)) 

cibar donate if highpriors==0, over1(treatment_group) level(90) bargap(20) barcolor(emerald lavender gold cranberry dkorange olive_teal ebblue) graphopts(note("90% confidence intervals", size(medsmall)) ytitle(Proportion that donated, size(medlarge)) ylabel(0(.1).5) legend(off) xlabel(1.2 "Control" 2.4 `" "More" "scientific" "information" "'3.6 `" "Efficacy" "salience" "' 4.8 `" "Health"  "information" "' 6 `" "Animal" "welfare" "' 7.2 `" "Social" "norms" "' 8.4 `" "Social" "efficacy" "',labsize(medsmall))) ciopts(lcolor(black)) 

oneway donate treatment_group if highpriors==0, tabulate 

***************SIMSCORE*****************************


cibar simscore, over1(treatment_group) level(90) bargap(20) barcolor(emerald lavender gold cranberry dkorange olive_teal ebblue) graphopts(note("90% confidence intervals", size(medsmall)) ytitle(Mean cosine similarity score, size(medlarge)) legend(off) xlabel(1.2 "Control" 2.4 `" "More" "scientific" "information" "'3.6 `" "Efficacy" "salience" "' 4.8 `" "Health"  "information" "' 6 `" "Animal" "welfare" "' 7.2 `" "Social" "norms" "' 8.4 `" "Social" "efficacy" "',labsize(medsmall)) ylabel(0(.1).5)) ciopts(lcolor(black))

ttest simscore, by(salience) //insignificant
***************MORALITY******************************

pwcorr currentdiet morality, sig
//coefficient -0.0655 with p-value 0.0220


pwcorr delta_diet morality, sig 
//coefficient .0998 with p-value 0.0005

pwcorr diet_change morality, sig
//coefficient -0.1115 with p-value 0.0001

aaplot morality delta_diet, jitter (2) ytitle("Morality", size(large)) xtitle("Reduction in weekly meat intake (delta_diet)", size(large)) xlabel(,labsize(large)) ylabel(,labsize(large))


