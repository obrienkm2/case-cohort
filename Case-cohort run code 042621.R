#LIBNAME wcc 'U:\Documents\weighting case-cohort'
setwd(path.expand("~/repo/case-cohort"))

# load  packages
library("haven")
library("dplyr")
library("survival")


# Example 1 (table, row 1) - simple case-cohort
EPSILON=0.01 #or any number smaller than your smallest time unit
SAMPLING_RATE=0.05 #for the example data set cc1

#restructure data set so that cases in sub-cohort weighted differently according to time (will appear as two entries)
rawcc1 <- haven::read_sas("cc1.sas7bdat") %>%
  rename_all(.funs=list("tolower"))
tmp1 <- rawcc1 %>%
  filter(subcohort == 1 & case %in% c(0,1)) %>%
  mutate(
    start = age_enrollment,
    stop = age_eof - EPSILON*(case == 1),
    event =  0, # considered a censored observation
    wt = 1 / SAMPLING_RATE        # inverse probability of sampling weight
  )
ccnew1 <- rawcc1 %>%
  filter(case == 1) %>%
  mutate(
    start = age_eof - EPSILON,
    stop = age_eof,
    event =  1,
    wt = 1,
  ) %>%
  bind_rows(tmp1)


coxfit1 <- coxph(Surv(start,stop,event) ~ exp + factor(covar1) + factor(covar2) + factor(covar3),
      weight=wt, id = id, data = ccnew1, robust=TRUE)
print(summary(coxfit1))





#Example 2 (table, row 2) - covariate-strataified case-cohort
EPSILON <- 0.01 #or any number smaller than your smallest time unit
SAMPLING_RATEA <- 0.08 #for the example data set cc2
SAMPLING_RATEB <- 0.15 #for the example data set cc2

# restructure data set so that cases in sub-cohort weighted differently according
# to time (will appear as two entries)

rawcc2 <- haven::read_sas("cc2.sas7bdat") %>%
  rename_all(.funs=list("tolower"))
tmp2 <- rawcc2 %>%
  #cases within subcohort - contribute fully until just before diagnosis
  #all cases contribute person-time right before event, count as event
  filter(subcohort == 1 & case %in% c(0,1)) %>%
  mutate(
    start = age_enrollment,
    stop = age_eof - EPSILON*(case == 1),
    event =  0, # considered a censored observation
    # inverse probability of sampling weight
    wt = case_when(
      groupa == 1 ~ 1 / SAMPLING_RATEA,
      groupa == 0 ~ 1 / SAMPLING_RATEB
    )
  )
ccnew2 <- rawcc2 %>%
  # all cases contribute person-time right before event, count as event
  filter(case == 1) %>%
  mutate(
    start = age_eof - EPSILON,
    stop = age_eof,
    event =  1,
    wt = 1,
  ) %>%
  bind_rows(tmp2)

coxfit2 <- coxph(Surv(start,stop,event) ~ exp + groupa + factor(covar2) + factor(covar3),
                 weight=wt, id = id, data = ccnew2, robust=TRUE)
print(summary(coxfit2))

#group-stratified
coxfit2Strata <- coxph(Surv(start,stop,event) ~ exp + factor(covar2) + factor(covar3),
                 weight=wt, id = id, data = filter(ccnew2, groupa == 1), robust=TRUE)
summary(coxfit2Strata)

coxfit2Stratb <- coxph(Surv(start,stop,event) ~ exp + factor(covar2) + factor(covar3),
                       weight=wt, id = id, data = filter(ccnew2, groupa == 0), robust=TRUE)
print(summary(coxfit2Stratb))


#Example 3 (table, row 3) - outcome-strataified case-cohort
EPSILON <- 0.01 #or any number smaller than your smallest time unit
SAMPLING_RATE <- 0.05 #for the example data set cc3
SAMPLING_RATE_SUBTYPE1 <- 0.5 #50% of subtype1 selected
SAMPLING_RATE_SUBTYPE2 <- 1.0 #100% of subtype2 selected

# restructure data set so that cases in sub-cohort weighted differently
# according to time (will appear as two entries)
rawcc3 <- haven::read_sas("cc3.sas7bdat") %>%
  rename_all(.funs=list("tolower"))
tmp3 <- rawcc3 %>%
  #cases within subcohort - contribute fully until just before diagnosis
  #all cases contribute person-time right before event, count as event
  filter(subcohort == 1 & subtype1 %in% c(0,1) & subtype2 %in% c(0,1)) %>%
  mutate(
    start = age_enrollment,
    stop = age_eof - EPSILON*(subtype1 == 1 | subtype2 == 1),
    event =  0,                    # considered a censored observation
    wt = 1 / (SAMPLING_RATE)       # inverse probability of sampling weight
  )
ccnew3 <- rawcc3 %>%
  # cases contribute person-time right before event only if selected, contribute based on weights
  filter(subtype1 == 1 | subtype2 == 1) %>%
  mutate(
    start = age_eof - EPSILON,
    stop = age_eof,
    event =  1,
    wt = case_when(
      subtype1 == 1 ~ 1 / SAMPLING_RATE_SUBTYPE1,
      subtype2 == 1 ~ 1 / SAMPLING_RATE_SUBTYPE2
    )
  ) %>%
  bind_rows(tmp3)


coxfit3 <- coxph(Surv(start,stop,event) ~ exp + factor(covar1) + factor(covar2) + factor(covar3),
                 weight=wt, id = id, data = ccnew3, robust=TRUE)
print(summary(coxfit3))


#subtype 1 only
rawcc3_1 <- haven::read_sas("cc3.sas7bdat") %>%
  rename_all(.funs=list("tolower"))
tmp3_1 <- rawcc3_1 %>%
  #cases within subcohort - contribute fully until just before diagnosis
  #all cases contribute person-time right before event, count as event
  filter(subcohort == 1 & subtype1 %in% c(0,1)) %>%
  mutate(
    start = age_enrollment,
    stop = age_eof - EPSILON*(subtype1 == 1),
    event =  0,                    # considered a censored observation
    wt = 1 / (SAMPLING_RATE)       # inverse probability of sampling weight
  )
ccnew3_1 <- rawcc3_1 %>%
  # cases contribute person-time right before event only if selected, contribute based on weights
  filter(subtype1 == 1) %>%
  mutate(
    start = age_eof - EPSILON,
    stop = age_eof,
    event =  1,
    wt = 1 / SAMPLING_RATE_SUBTYPE1
  ) %>%
  bind_rows(tmp3_1)

coxfit3Subtype1 <- coxph(Surv(start,stop,event) ~ exp + factor(covar1) + factor(covar2) + factor(covar3),
                 weight=wt, id = id, data = ccnew3_1, robust=TRUE)
print(summary(coxfit3Subtype1))


#subtype 2 only
rawcc3_2 <- haven::read_sas("cc3.sas7bdat") %>%
  rename_all(.funs=list("tolower"))
tmp3_2 <- rawcc3_2 %>%
  #cases within subcohort - contribute fully until just before diagnosis
  #all cases contribute person-time right before event, count as event
  filter(subcohort == 1 & subtype2 %in% c(0,1)) %>%
  mutate(
    start = age_enrollment,
    stop = age_eof - EPSILON*(subtype2 == 1),
    event =  0,                    # considered a censored observation
    wt = 1 / (SAMPLING_RATE)       # inverse probability of sampling weight
  )
ccnew3_2 <- rawcc3_2 %>%
  # cases contribute person-time right before event only if selected, contribute based on weights
  filter(subtype2 == 1) %>%
  mutate(
    start = age_eof - EPSILON,
    stop = age_eof,
    event =  1,
    wt = subtype2 == 1 / SAMPLING_RATE_SUBTYPE2
  ) %>%
  bind_rows(tmp3_2)

coxfit3Subtype2 <- coxph(Surv(start,stop,event) ~ exp + factor(covar1) + factor(covar2) + factor(covar3),
                         weight=wt, id = id, data = ccnew3_2, robust=TRUE)
print(summary(coxfit3Subtype2))



#Example 4 (table, row 4) - covariate- and outcome-strataified case-cohort
EPSILON <- 0.01 #or any number less than your smallest time unit
SAMPLING_RATEA <- 0.08 #for the example data set cc4
SAMPLING_RATEB <- 0.15 #for the example data set cc4
SAMPLING_RATE_SUBTYPE1 <- 0.5 #50% of subtype1 selected
SAMPLING_RATE_SUBTYPE2 <- 1.0 #100% of subtype2 selected


# restructure data set so that cases in sub-cohort weighted differently
# according to time (will appear as two entries)
rawcc4 <- haven::read_sas("cc4.sas7bdat") %>%
  rename_all(.funs=list("tolower"))
tmp4 <- rawcc4 %>%
  #cases within subcohort - contribute fully until just before diagnosis
  #all cases contribute person-time right before event, count as event
  filter(subcohort == 1 & subtype1 %in% c(0,1) & subtype2 %in% c(0,1)) %>%
  mutate(
    start = age_enrollment,
    stop = age_eof - EPSILON*(subtype1 == 1 | subtype2 == 1),
    event =  0,                    # considered a censored observation
    wt = case_when(
      groupa == 1 ~ 1 / SAMPLING_RATEA,
      groupa == 0 ~ 1 / SAMPLING_RATEB
    )
  )
ccnew4 <- rawcc4 %>%
  # cases contribute person-time right before event only if selected, contribute based on weights
  filter(subtype1 == 1 | subtype2 == 1) %>%
  mutate(
    start = age_eof - EPSILON,
    stop = age_eof,
    event =  1,
    wt = case_when(
      subtype1 == 1 ~ 1 / SAMPLING_RATE_SUBTYPE1,
      subtype2 == 1 ~ 1 / SAMPLING_RATE_SUBTYPE2
    )
  ) %>%
  bind_rows(tmp4)


coxfit4 <- coxph(Surv(start,stop,event) ~ exp + groupa + factor(covar2) + factor(covar3),
                 weight=wt, id = id, data = ccnew4, robust=TRUE)
print(summary(coxfit4))



#Example 5 (table, row 5) - case-independent designs
SAMPLING_RATE_CASES <- 1 #for the example data set cc5 (all cases)
SAMPLING_RATE_NONCASES <- 0.049 #5% of cohort selected into subcohort, which included 2305 non-cases (out of total 47015 non-cases)

ccnew5 <- haven::read_sas("cc5.sas7bdat") %>%
  rename_all(.funs=list("tolower"))%>%
  mutate(
    wt = case_when(
      case == 1 ~ 1 / SAMPLING_RATE_CASES,
      case == 0 ~ 1 / SAMPLING_RATE_NONCASES
    )
  )

lmfit5 <- lm(exp2 ~ exp + age_enrollment + factor(covar1) + factor(covar2) + factor(covar3),
                 weight=wt, data = ccnew5)

print(cbind(coef(lmfit5), confint(lmfit5)))

