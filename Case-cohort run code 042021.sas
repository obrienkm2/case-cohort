LIBNAME wcc 'U:\Documents\weighting case-cohort'; 

*Example 1 (table, row 1) - simple case-cohort;
%LET epsilon=0.01; *or any number smaller than your smallest time unit;
%LET sampling_rate=0.05; *for the example data set cc1;

*restructure data set so that cases in sub-cohort weighted differently according to time (will appear as two entries);
DATA ccnew1;
	SET wcc.cc1;
*cases within subcohort - contribute fully until just before diagnosis;
	IF subcohort=1 AND case=1 THEN DO; 
		start = age_enrollment;
		stop= age_eof - &epsilon; 
		event = 0; *considered a censored observation;
	wt= 1/&sampling_rate; *inverse probability of sampling weight;
	OUTPUT;
	END;

	*all cases contribute person-time right before event, count as event;
	IF case=1 THEN DO; 
		start = age_eof - &epsilon; 
		stop = age_eof;
		event = 1;
		wt=1;
	OUTPUT;
	END;

	*non-cases within subcohort - contribute full person time, censored;
	ELSE IF subcohort=1 AND case=0 THEN DO;  
		start = age_enrollment;
		stop = age_eof;
		event = 0;
		wt= 1/&sampling_rate; *inverse probability of sampling weight;
	OUTPUT; 
	END;
RUN;

PROC PHREG DATA=ccnew1 covs(aggregate);
	CLASS covar1 covar2 covar3;
	MODEL (start,stop)*event(0) = exp covar1 covar2 covar3;
	WEIGHT wt;
	ID ID;
	HAZARDRATIO exp;
RUN;


*Example 2 (table, row 2) - covariate-strataified case-cohort;
%LET epsilon=0.01; *or any number smaller than your smallest time unit;
%LET sampling_rateA=0.08; *for the example data set cc2;
%LET sampling_rateB=0.15; *for the example data set cc2;

*restructure data set so that cases in sub-cohort weighted differently according to time (will appear as two entries);
DATA ccnew2;
	SET wcc.cc2;
*cases within subcohort - contribute fully until just before diagnosis;
	IF subcohort=1 AND case=1 THEN DO; 
		start = age_enrollment;
		stop= age_eof - &epsilon; 
		event = 0; *considered a censored observation;
	IF groupA=1 THEN wt= 1/&sampling_rateA; 
	ELSE IF groupA=0 THEN wt= 1/&sampling_rateB;
*inverse probability of sampling weights;
	OUTPUT;
	END;

	*all cases contribute person-time right before event, count as event;
	IF case=1 THEN DO; 
		start = age_eof - &epsilon; 
		stop = age_eof;
		event = 1;
		wt=1;
	OUTPUT;
	END;

	*non-cases within subcohort - contribute full person time, censored;
	ELSE IF subcohort=1 AND case=0 THEN DO;  
		start = age_enrollment;
		stop = age_eof;
		event = 0;
IF groupA=1 THEN wt= 1/&sampling_rateA; 
ELSE IF groupA=0 THEN wt= 1/&sampling_rateB;
*inverse probability of sampling weights;
	OUTPUT; 
	END;
RUN;

PROC PHREG DATA=ccnew2 covs(aggregate);
	CLASS covar2 covar3;
	MODEL (start,stop)*event(0) = exp groupA covar2 covar3;
	WEIGHT wt;
	ID ID;
	HAZARDRATIO exp;
RUN;

*Example 3 (table, row 3) - outcome-strataified case-cohort;
%LET epsilon=0.01; *or any number less than your smallest time unit;
%LET sampling_rate=0.05; *for the example data set cc3;
%LET sampling_rate_subtype1=0.20; *20% of subtype1 selected;
%LET sampling_rate_subtype2=1; *100% of subtype2 selected;

*restructure data set so that cases in sub-cohort weighted differently according to time (will appear as two entries);
DATA ccnew3;
	SET wcc.cc3;
	*selected cases within subcohort - contribute fully until just before diagnosis;
	IF subcohort=1 AND (subtype1=1 | subtype2=1) THEN DO; 
		start = age_enrollment;
		stop= age_eof - &epsilon; 
		event = 0; *considered a censored observation;
		wt= 1/&sampling_rate; *inverse probability of sampling weight;
	OUTPUT;
	END;

	*cases contribute person-time right before event only if selected, contribute based on weights;
	IF (subtype1=1 | subtype2=1) THEN DO; 
		start = age_eof - &epsilon; 
		stop = age_eof;
		event = 1;
		IF subtype1=1 THEN wt=1/&sampling_rate_subtype1;
			ELSE IF subtype2=1 THEN wt=1/&sampling_rate_subtype2;
	OUTPUT;
	END;

	*non-cases within subcohort - contribute full person time, censored;
	ELSE IF subcohort=1 AND subtype1=0 AND subtype2=0 THEN DO;  
		start = age_enrollment;
		stop = age_eof;
		event = 0;
		wt= 1/&sampling_rate; *inverse probability of sampling weight;
	OUTPUT; 
	END;
RUN;

PROC PHREG DATA=ccnew3 covs(aggregate);
	CLASS covar1 covar2 covar3;
	MODEL (start,stop)*event(0) = exp covar1 covar2 covar3;
	WEIGHT wt;
	ID ID;
	HAZARDRATIO exp;
RUN;

*Example 4 (table, row 4) - covariate- and outcome-strataified case-cohort;
%LET epsilon=0.01; *or any number less than your smallest time unit;
%LET sampling_rateA=0.08; *for the example data set cc4;
%LET sampling_rateB=0.15; *for the example data set cc4;
%LET sampling_rate_subtype1=0.20; *20% of subtype1 selected;
%LET sampling_rate_subtype2=1; *100% of subtype2 selected;

*restructure data set so that cases in sub-cohort weighted differently according to time (will appear as two entries);
DATA ccnew4;
	SET wcc.cc4;
	*selected cases within subcohort - contribute fully until just before diagnosis;
	IF subcohort=1 AND (subtype1=1 | subtype2=1) THEN DO; 
		start = age_enrollment;
		stop= age_eof - &epsilon; 
		event = 0; *considered a censored observation;
		IF groupA=1 THEN wt= 1/&sampling_rateA; 
			ELSE IF groupA=0 THEN wt=1/&sampling_rateB; 
		*inverse probability of sampling weight;
	OUTPUT;
	END;

	*cases contribute person-time right before event only if selected, contribute based on weights;
	IF (subtype1=1 | subtype2=1) THEN DO; 
		start = age_eof - &epsilon; 
		stop = age_eof;
		event = 1;
		IF subtype1=1 THEN wt=1/&sampling_rate_subtype1;
			ELSE IF subtype2=1 THEN wt=1/&sampling_rate_subtype2;
	OUTPUT;
	END;

	*non-cases within subcohort - contribute full person time, censored;
	ELSE IF subcohort=1 AND subtype1=0 AND subtype2=0 THEN DO;  
		start = age_enrollment;
		stop = age_eof;
		event = 0;
		IF groupA=1 THEN wt= 1/&sampling_rateA; 
			ELSE IF groupA=0 THEN wt=1/&sampling_rateB; 
		*inverse probability of sampling weight;
	OUTPUT; 
	END;
RUN;

PROC PHREG DATA=ccnew4 covs(aggregate);
	CLASS covar2 covar3;
	MODEL (start,stop)*event(0) = exp groupA covar2 covar3;
	WEIGHT wt;
	ID ID;
	HAZARDRATIO exp;
RUN;

*Example 5 (table, row 5) - case-independent designs;
%LET sampling_rate_cases=1; *for the example data set cc5 (all cases);
%LET sampling_rate_subcohort=0.05; *5% of cohort selected into subcohort;

DATA cc5;
	SET wcc.cc5;
	IF case=1 THEN wt= 1/&sampling_rate_cases; 
		ELSE IF case=0 THEN wt= 1/&sampling_rate_subcohort; 
	*create indicator versions of covariates;
	IF covar1=1 THEN covar1_1=1;
		ELSE covar1_1=0;
	IF covar1=2 THEN covar1_2=1;
		ELSE covar1_2=0;
	IF covar1=3 THEN covar1_3=1;
		ELSE covar1_3=0;

	IF covar2=1 THEN covar2_1=1;
		ELSE covar2_1=0;
	IF covar2=2 THEN covar2_2=1;
		ELSE covar2_2=0;
	IF covar2=3 THEN covar2_3=1;
		ELSE covar2_3=0;

	IF covar3=2 THEN covar3_2=1;
		ELSE covar3_2=0;
	IF covar3=3 THEN covar3_3=1;
		ELSE covar3_3=0;
RUN;

PROC REG DATA=cc5;
	MODEL exp2 = exp age_enrollment covar1_1 covar1_2 covar1_3 covar2_1 covar2_2 covar2_3 covar3_2 covar3_3 / CLB;
	WEIGHT wt;
RUN;
QUIT;
