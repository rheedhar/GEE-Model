/******************************\
|PROGRAMMER: FAREEDAT BELLO
|OBJECTIVE: DATA CLEANING & GEE ANALYSIS
/********************************/

* maro variables;
%let home=Z:\coach\Survey;* used in file path names below;

*SAS libraries;
libname	coach		  "&home.\data"; 
libname library		"&home.\data"; *needed for saving permanent format library;

/*create formats for categorical variables*/
proc format;
	value som_s
	0 = "Not bothered at all"
	1 = "Bothered a little"
	2 = "Bothered a lot"
	;
	value new_anx
	0 = "None of the time"
	1 = "Some of the time"
	2 = "Moderate amount of time"
	3 = "All of the time"
	;
	value new_dep
	0 = "< 1 day"
	1 = "1-2 days"
	2 = "3-4 days"
	3 = "5-7 days"
	;
	value rev_depp
	0 = "5-7 days"
	1 = "3-4 days"
	2 = "1-2 days"
	3 = "< 1 day"
	;
    value new_pss
	0 = "Never"
	1 = "Almost never"
	2 = "Sometimes"
	3 = "Fairly often"
	4 = "Very often"
	;
	value recode_pss
	0 = "Very often"
	1 = "Fairly often"
	2 = "Sometimes"
	3 = "Almost never"
	4 = "Never"
	;
	value recode_pa_ss
	1 = "Strongly disagree"
	2 = "Disagree"
	3 = "Undecided"
	4 = "Agree"
	5 = "Strongly agree"
	;
	value have_it
	 0 = 'No'
	 1 = 'Yes'
	 ;
	value age_cat
	1 = "18-24"
	2 = "25-34"
	3 = "35-44"
	4 = "45-54"
	5 = "55-64"
	6 = ">=65"
	;

	value selectall
	. = "No"
	1 = "Yes"
	;
	value edu_cat
	1 = "Less than HS"
	2 = "HS/GED"
	3 = "Some college/aa"
	4 = "College grad"
	;
	value diag_covid
	1 = "Yes"
	0 = "No"
	;
	value emply_cat
	1 = "Working"
	2 = "Unemployed"
	3 = "Other"
	;

	value p_stress_cat
	1 = "Low Stress"
	2 = "Moderate Stress"
	3 = "High Stress"
	;
run;

/**DATA CLEANING */
data newclean_thes;
	set library.met_criteria_child;
	
	*child mental health status;
	length anxt_strs 3.;
	if flchg_anxsts = . then anxt_strs = .;
	else if flchg_anxsts = 1 then anxt_strs = 1;
	else anxt_strs = 0;

	if anxt_strs ne .;

	*education;
	if q8 = . then edu_cat = .;
	else if q8 = 1 or q8 = 2 or q8 = 3 then edu_cat= 1;
	else if q8 = 4 then edu_cat = 2;
	else edu_cat = 3;

	*employment status;
	if q10 = . then emply_cat = .;
	else if q10 = 1 then emply_cat = 1;
	else if q10 = 3 then emply_cat = 2;
	else emply_cat = 3;

	*remote learning;
	if q148_2 = 1 or q148_3 = 1 then remote_learning = 1;
	else remote_learning = 0;

	/*parental stress*/
	if p_stress = . then p_stress_categorized = .;
	else if 0 <= p_stress < 21 then p_stress_categorized = 0;
	else p_stress_categorized = 1;
	
	*parental sex;
	if q4 = . then sex_parents = .;
	else if q4 = 1 then sex_parents = 0;
	else sex_parents = 1;

	*age of child;
	if child_age_cat = "" then child_age_category = .;
	else if child_age_cat = "5-11" then child_age_category = 0;
	else child_age_category = 1;

 	*race;
	if q7 = . then race = .;
	else if q7 = 1 then race = 1;
	else if q7 = 2 or q7 = 5 then race = 2;
	else if q7 = 3 then race = 3;
	else race = 4;
	
	*household income;
	if q225 = . then hh_income = .;
	else if q225 = 1 or q225 = 2 then hh_income = 1;
	else if q225 = 3 then hh_income = 2;
	else if q225 = 4 then hh_income = 3;
	else hh_income = 4;
	
	*marital status;
	if q9 = . then marital_status = .;
	else if q9 = 1 then marital_status = 0;
	else marital_status = 1;
	
	*parental age;
	if age_cat = . then age_cat_parents = .;
	else if age_cat = 1 or age_cat = 2 then age_cat_parents = 1;
	else if age_cat = 3 or age_cat = 4 then age_cat_parents = 2;
	else age_cat_parents = 3;

	*loss of job;
	if q19_1 = . then loss_job_hh = .;
	else if q19_1 = 5 then loss_job_hh = 0;
	else loss_job_hh = 1;

	*remote work;
	if q32 = . then remote_work = .;
	else if q32 = 1 then remote_work = 0;
	else remote_work = 1;
	
	array delete_missing {*} sex_parents edu_cat hh_income age_cat_parents _age_ emply_cat race child_age_category hh_age marital_status p_stress p_stress_categorized remote_learning remote_work loss_job_hh child_17;
	do i = 1 to dim(delete_missing);
		if delete_missing{i} = . then delete;
	end;
 
	*format remote_learning diag_covid diag_covid. p_stress_cat p_stress_cat. emply_cat emply_cat. edu_cat edu_cat.;
	keep seqn res_id sex_parents edu_cat hh_income age_cat_parents _age_ emply_cat race child_age_category hh_age marital_status p_stress_categorized p_stress remote_learning remote_work loss_job_hh anxt_strs child_17;
run;

/*check multiple children*/
proc print data=newclean_thes;
var res_id anxt_strs hh_age child_age_category anxt_strs p_stress;
by res_id;
run; 

proc sort data=newclean_thes;
by res_id;
run;

/*helpful to report just parents data*/
proc surveyselect data=newclean_thes method=srs n=1
seed=1953 out=survey_sample;
strata res_id;
run;

/*UNIVARIATE ANALYSIS*/
proc freq data=survey_sample; tables sex_parents edu_cat age_cat_parents race marital_status hh_income emply_cat p_stress_categorized remote_learning remote_work loss_job_hh child_17 child_age_category anxt_strs / missing; run;
proc freq data=survey_sample; tables p_stress_categorized * p_stress; run;
proc freq data=survey_sample; tables p_stress_categorized edu_cat race; run;


proc univariate plot data=newclean_thes;
var p_stress _age_;
run;

proc freq data=newclean_thes; tables anxt_strs * p_stress_categorized / missing; run;

proc means data=newclean_thes;
var hh_age;
run;


*BIVARIATE ANALYSIS;

/*exposure(P_STRESS) using parent's data*/
proc freq data=survey_sample; tables (sex_parents edu_cat hh_income age_cat_parents emply_cat race  marital_status p_stress_categorized remote_learning remote_work loss_job_hh ) *  p_stress_categorized /chisq; run;

/*covariate association*/
proc freq data=survey_sample; tables (sex_parents edu_cat hh_income age_cat_parents emply_cat race  marital_status p_stress_categorized remote_learning remote_work loss_job_hh ) *  p_stress_categorized /chisq; run;

/*checking assumption for linearity*/

proc univariate plot data=newclean_thes;
var p_stress;
run;
proc sort data=newclean_thes;
by p_stress;
run;

proc means data=newclean_thes noprint;
by p_stress;
var anxt_strs;
output out=stress_pstress mean=panxt_strs;
run;

proc print data=stress_pstress;
run;

data stress_pstress2;
set stress_pstress;
log_prev = log(panxt_strs);
run;

proc gplot data=stress_pstress2;
plot log_prev * p_stress;
run;


/*predicted probabilities*/

proc genmod data=newclean_thes descending;
class res_id hh_income(ref="4") edu_cat(ref="2") loss_job_hh(ref="0") remote_learning(ref="0") marital_status(ref="1") child_age_category(ref="0") remote_work(ref="0") / param = ref;
model anxt_strs = p_stress hh_income edu_cat loss_job_hh remote_learning marital_status child_age_category remote_work p_stress_categorized*child_age_category / link=log dist=poisson type3;
repeated subject=res_id / type=exch covb corrw;
output out=model_p_stress p=prob xbeta=log;
run;
proc gplot data=model_p_stress;
plot log * p_stress;
run;

proc genmod data=newclean_thes descending;
model anxt_strs = p_stress / link=log dist=poisson;
output out=model_p_stress p=prob xbeta=log;
run;

proc gplot data=model_p_stress;
plot log * p_stress;
run;

/*collinearity*/
proc corr data = newclean_thes;
var loss_job_hh  emply_cat;
run;



/*crude prevalence ratio for each of the variable going into the model*/
*parental stress;
proc genmod data=newclean_thes descending;
class res_id p_stress_categorized(ref="0") / param = ref;
model anxt_strs = p_stress_categorized / link=log dist=poisson type3;
repeated subject = res_id /type=exch covb corrw;
estimate "Pstress 1 v. 0" p_stress_categorized 1 / exp;
run;

*sex;
proc genmod data=newclean_thes descending;
class res_id sex_parents(ref="0") / param = ref;
model anxt_strs = sex_parents / link=log dist=poisson type3;
repeated subject = res_id /type=exch covb corrw;
estimate "male vs female" sex_parents 1 / exp;
run;

*education;
proc genmod data=newclean_thes descending;
class res_id edu_cat(ref="3") / param = ref;
model anxt_strs = edu_cat / link=log dist=poisson type3;
repeated subject = res_id /type=exch covb corrw;
estimate "education 1 v. 3" edu_cat 1 0 /exp;
estimate "education 2 v. 3" edu_cat 0 1 /exp;
run;

*marital status;
proc genmod data=newclean_thes descending;
class res_id marital_status(ref="1") / param = ref;
model anxt_strs = marital_status / link=log dist=poisson type3;
repeated subject = res_id /type=exch covb corrw;
estimate "single vs married" marital_status 1 /exp;
run;

*household income;
proc genmod data=newclean_thes descending;
class res_id hh_income(ref="4") / param = ref;
model anxt_strs = hh_income / link=log dist=poisson type3;
repeated subject = res_id /type=exch covb corrw;
estimate "hh 1 vs. 4" hh_income 1 0 0/exp;
estimate "hh 2 vs. 4" hh_income 0 1 0/exp;
estimate "hh 3 vs. 4" hh_income 0 0 1/exp;
run;

*remote learning;
proc genmod data=newclean_thes descending;
class res_id remote_learning(ref="0") / param = ref;
model anxt_strs = remote_learning / link=log dist=poisson type3;
repeated subject = res_id /type=exch covb corrw;
estimate "remote learning 1 vs. 0 " remote_learning 1 /exp;
run;

*remote work;
proc genmod data=newclean_thes descending;
class res_id remote_work(ref="0") / param = ref;
model anxt_strs = remote_work / link=log dist=poisson type3;
repeated subject=res_id / type=exch covb corrw;
estimate "remote work 1 vs. 0 " remote_work 1 /exp; 
run;

*jobloss;
proc genmod data=newclean_thes descending;
class res_id loss_job_hh(ref="0") / param = ref;
model anxt_strs = loss_job_hh / link=log dist=poisson type3;
repeated subject=res_id / type=exch covb corrw;
estimate "loss j " loss_job_hh 1 /exp; 
run;

*employment status;
proc genmod data=newclean_thes descending;
class res_id emply_cat(ref="1") / param = ref;
model anxt_strs = emply_cat / link=log dist=poisson type3;
repeated subject=res_id / type=exch covb corrw;
estimate "emply 2 v. 1" emply_cat 1 0 /exp; 
estimate "emply 3 v. 1" emply_cat 0 1 /exp; 
run;
*race;
proc genmod data=newclean_thes descending;
class res_id race(ref="4") / param = ref;
model anxt_strs = race / link=log dist=poisson type3;
repeated subject=res_id / type=exch covb corrw;
estimate "race 1 v. 4" race 1 0 0 /exp; 
estimate "race 2 v. 4" race 0 1 0 /exp;
estimate "race 3 v. 4" race 0 0 1 /exp; 
run;
*child_age_cat;
proc genmod data=newclean_thes descending;
class res_id child_age_category(ref="0") / param = ref;
model anxt_strs = child_age_category / link=log dist=poisson type3;
repeated subject=res_id / type=exch covb corrw;
estimate "child1 1 v. 0" child_age_category 1 /exp; 
run;


/*Full poisson regression with effect modification for child's age*/

proc genmod data=newclean_thes descending;
class res_id p_stress_categorized(ref="0") edu_cat(ref="3") hh_income(ref="4") loss_job_hh(ref="0") remote_learning(ref="0") marital_status(ref="0") child_age_category(ref="0") race(ref="4") / param = ref;
model anxt_strs = p_stress_categorized edu_cat hh_income loss_job_hh remote_learning marital_status child_age_category race p_stress_categorized*child_age_category / link=log dist=poisson type3;
repeated subject=res_id / type=exch covb corrw;

/*when child_age is 1*/
estimate "Pstress 1 v. 0, age = 1" p_stress_categorized 1 p_stress_categorized*child_age_category 1 /exp; 
estimate "hhincome 1 v. 4, age =1" hh_income 1 0 0  p_stress_categorized*child_age_category 1 /exp;
estimate "hhincome 2 v. 4, age =1" hh_income 0 1 0  p_stress_categorized*child_age_category 1 /exp;
estimate "hhincome 3 v. 4, age =1" hh_income 0 0 1  p_stress_categorized*child_age_category 1 /exp;
estimate "education 1 v. 3, age =1" edu_cat 1 0 p_stress_categorized*child_age_category 1 /exp;
estimate "education 2 v. 3, age =1" edu_cat 0 1 p_stress_categorized*child_age_category 1 /exp;
estimate "job loss 1 v. 0, age =1" loss_job_hh 1  p_stress_categorized*child_age_category 1 /exp;
estimate "remote learning 1 v. 0, age =1" remote_learning 1  p_stress_categorized*child_age_category 1 /exp;
estimate "race 1 v. 4, age =1" race 1 0 0 p_stress_categorized*child_age_category 1 /exp;
estimate "race 2 v. 4, age =1" race 0 1 0 p_stress_categorized*child_age_category 1 /exp;
estimate "race 3 v. 4, age =1" race 0 0 1 p_stress_categorized*child_age_category 1 /exp;
estimate "marital stat 1 v. 0, age =1" marital_status 1  p_stress_categorized*child_age_category 1 /exp;

/*when child_age is 0*/
estimate "Pstress 1 v. 0, age = 0" p_stress_categorized 1 p_stress_categorized*child_age_category 0 /exp; 
estimate "hhincome 1 v. 4, age =0" hh_income 1 0 0  p_stress_categorized*child_age_category 0 /exp;
estimate "hhincome 2 v. 4, age =0" hh_income 0 1 0  p_stress_categorized*child_age_category 0 /exp;
estimate "hhincome 3 v. 4, age =0" hh_income 0 0 1  p_stress_categorized*child_age_category 0 /exp;
estimate "education 1 v. 3, age =0" edu_cat 1 0 p_stress_categorized*child_age_category 0 /exp;
estimate "education 2 v. 3, age =0" edu_cat 0 1 p_stress_categorized*child_age_category 0 /exp;
estimate "job loss 1 v. 0, age =0" loss_job_hh 1  p_stress_categorized*child_age_category 0 /exp;
estimate "remote learning 1 v. 0, age =0" remote_learning 1  p_stress_categorized*child_age_category 0 /exp;
estimate "race 1 v. 4, age =0" race 1 0 0 p_stress_categorized*child_age_category 0 /exp;
estimate "race 2 v. 4, age =0" race 0 1 0 p_stress_categorized*child_age_category 0 /exp;
estimate "race 3 v. 4, age =0" race 0 0 1 p_stress_categorized*child_age_category 0 /exp;
estimate "marital stat 1 v. 0, age =0" marital_status 1  p_stress_categorized*child_age_category 0 /exp;
run;

/*poisson regression without effect modification*/

proc genmod data=newclean_thes descending;
class res_id p_stress_categorized(ref="0") edu_cat(ref="3") hh_income(ref="4") remote_learning(ref="0") marital_status(ref="0") child_age_category(ref="0") race(ref="4") / param = ref;
model anxt_strs = p_stress_categorized edu_cat hh_income remote_learning marital_status child_age_category race loss_job_hh / link=log dist=poisson type3;
repeated subject=res_id / type=exch covb corrw;

estimate "Pstress 1 v. 0, age = 1" p_stress_categorized 1 /exp; 
estimate "hhincome 1 v. 4, age =1" hh_income 1 0 0  /exp;
estimate "hhincome 2 v. 4, age =1" hh_income 0 1 0  /exp;
estimate "hhincome 3 v. 4, age =1" hh_income 0 0 1  /exp;
estimate "education 1 v. 3, age =1" edu_cat 1 0 /exp;
estimate "education 2 v. 3, age =1" edu_cat 0 1 /exp;
estimate "job loss 1 v. 0, age =1" loss_job_hh 1  /exp;
estimate "remote learning 1 v. 0, age =1" remote_learning 1 /exp;
estimate "race 1 v. 4, age =1" race 1 0 0 /exp;
estimate "race 2 v. 4, age =1" race 0 1 0 /exp;
estimate "race 3 v. 4, age =1" race 0 0 1 /exp;
estimate "marital stat 1 v. 0, age =1" marital_status 1 /exp;
estimate "child age 1 v 0" child_age_category 1 / exp;
run;
