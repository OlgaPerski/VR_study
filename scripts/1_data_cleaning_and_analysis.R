#load packages -----------------------------------------------------------

source(here::here("scripts", "0_load_packages.R"))

#load data ---------------------------------------------------------------

data_raw <- download_zenodo("10.5281/zenodo.5747706", path = here("data"))

data <- read_xlsx(here("data","VR_dataset_29.10.21.xlsx"))

#recode variables --------------------------------------------------------

df1 <- data %>%
  mutate(gender = as.factor(gender),
         ses = as.factor(ses),
         country = as.factor(country),
         mtss = as.factor(mtss),
         vr_exp = as.factor(vr_exp),
         own_vr = as.factor(own_vr),
         ttfc = as.factor(ttfc),
         past_quit = as.factor(past_quit),
         group = as.factor(group),
         angry = as.factor(angry),
         distressed = as.factor(distressed),
         useful = as.factor(useful),
         cancer = as.factor(cancer),
         heart_disease = as.factor(heart_disease),
         lung_disease = as.factor(lung_disease),
         cancer_response_efficacy = as.factor(cancer_response_efficacy),
         heart_response_efficacy = as.factor(heart_response_efficacy),
         lung_response_efficacy = as.factor(lung_response_efficacy),
         quitting_confidence = as.factor(quitting_confidence),
         intention = as.factor(intention))

df2 <- df1 %>%
  mutate(beh_supp = as.factor(case_when(beh_supp_group == 1 | beh_supp_ind == 1 | telephone == 1 | written == 1 | website == 1 | app == 1 ~ "yes",
                                        TRUE ~ "no")),
         pharm_supp = as.factor(case_when(nrt_otc == 1 | nrt_prescription == 1 | bupropion == 1 | varenicline == 1 | ecigs == 1 ~ "yes",
                                          TRUE ~ "no")),
         angry = as.factor(case_when(angry == "Strongly agree" | angry == "Agree" ~ "not acceptable",
                                     TRUE ~ "acceptable")),
         distressed = as.factor(case_when(distressed == "Strongly agree" | distressed == "Agree" ~ "not acceptable",
                                          TRUE ~ "acceptable")),
         useful = as.factor(case_when(useful == "Strongly disagree" | useful == "Disgree" ~ "not acceptable",
                                      TRUE ~ "acceptable")),
         acceptable = as.factor(case_when(angry == "not acceptable" | distressed == "not acceptable" | useful == "not acceptable" ~ "not acceptable",
                                          TRUE ~ "acceptable")),
         cancer = as.factor(case_when(cancer == "Very likely" | cancer == "Moderately likely" ~ "susceptible",
                                      TRUE ~ "not susceptible")),
         heart_disease = as.factor(case_when(heart_disease == "Very likely" | heart_disease == "Moderately likely" ~ "susceptible",
                                      TRUE ~ "not susceptible")),
         lung_disease = as.factor(case_when(lung_disease == "Very likely" | lung_disease == "Moderately likely" ~ "susceptible",
                                      TRUE ~ "not susceptible")),
         cancer_response_efficacy = as.factor(case_when(cancer_response_efficacy == "Strongly agree" | cancer_response_efficacy == "Agree" ~ "yes",
                                            TRUE ~ "no")),
         heart_response_efficacy = as.factor(case_when(heart_response_efficacy == "Strongly agree" | heart_response_efficacy == "Agree" ~ "yes",
                                                        TRUE ~ "no")),
         lung_response_efficacy = as.factor(case_when(lung_response_efficacy == "Strongly agree" | lung_response_efficacy == "Agree" ~ "yes",
                                                        TRUE ~ "no")),
         quitting_confidence = as.factor(case_when(quitting_confidence == "Extremely" | quitting_confidence == "Very" ~ "yes",
                                                      TRUE ~ "no")))

#descriptives ------------------------------------------------------------

mylabels <- list(country = "Country", age = "Age", gender = "Gender", ses = "Socioeconomic position",
                 mtss = "Motivation to stop scale", own_vr = "Own VR headset", vr_exp = "Past VR experience", cpd = "Cigarettes per day",
                 ttfc = "Time to first cigarette", past_quit = "Past-year quit attempt(s)",
                 beh_supp = "Ever use of behavioural support", pharm_supp = "Ever use of pharmacological support",
                 angry = "Angry", distressed = "Distressed", useful = "Useful", acceptable = "Acceptability", cancer = "Perceived susceptibility to cancer",
                 heart_disease = "Perceived susceptibility to heart disease", lung_disease = "Perceived susceptibility to lung disease",
                 cancer_response_efficacy = "Cancer response efficacy", heart_response_efficacy = "Heart disease response efficacy",
                 lung_response_efficacy = "Lung disease response efficacy", quitting_confidence = "Quitting self-efficacy",
                 intention = "Intention to stop smoking")

t1_total <- tableby(~ age + gender + ses + country + mtss + own_vr + vr_exp + cpd + ttfc +
                      past_quit + beh_supp + pharm_supp + angry + distressed + useful + acceptable + cancer + heart_disease +
                      lung_disease + cancer_response_efficacy + heart_response_efficacy +
                      lung_response_efficacy + quitting_confidence + intention, data = df2, cat.simplify = F)

t1_strata <- tableby(~ age + gender + ses + country + mtss + own_vr + vr_exp + cpd + ttfc +
                       past_quit + beh_supp + pharm_supp + angry + distressed + useful + acceptable + cancer + heart_disease +
                       lung_disease + cancer_response_efficacy + heart_response_efficacy +
                       lung_response_efficacy + quitting_confidence + intention, data = df2, strata = group, cat.simplify = F, test = T)

t1_total <- summary(t1_total, text = T, labelTranslations = mylabels, digits = 1)
t1_strata <- summary(t1_strata, text = T, labelTranslations = mylabels, digits = 1)

write2word(t1_total,here("outputs","table1_total.doc"))
write2word(t1_strata,here("outputs","table1_strata.doc"))

#point estimates and 95% CIs ---------------------------------------------

a <- table(df2$group, df2$acceptable)
b <- table(df2$group, df2$cancer)
c <- table(df2$group, df2$heart_disease)
d <- table(df2$group, df2$lung_disease)
e <- table(df2$group, df2$cancer_response_efficacy)
f <- table(df2$group, df2$heart_response_efficacy)
g <- table(df2$group, df2$lung_response_efficacy)
h <- table(df2$group, df2$quitting_confidence)
i <- table(df2$group, df2$intention)

#acceptability

binom.test(a[1,1],(a[1,1]+a[1,2])) #control
binom.test(a[2,1],(a[2,1]+a[2,2])) #intervention

#perceived susceptibility to cancer

binom.test(b[1,2],(b[1,1]+b[1,2])) #control
binom.test(b[2,2],(b[2,1]+b[2,2])) #intervention

#perceived susceptibility to heart disease

binom.test(c[1,2],(c[1,1]+c[1,2])) #control
binom.test(c[2,2],(c[2,1]+c[2,2])) #intervention

#perceived susceptibility to lung disease

binom.test(d[1,2],(d[1,1]+d[1,2])) #control
binom.test(d[2,2],(d[2,1]+d[2,2])) #intervention

#cancer response-efficacy

binom.test(e[1,2],(e[1,1]+e[1,2])) #control
binom.test(e[2,2],(e[2,1]+e[2,2])) #intervention

#heart disease response-efficacy

binom.test(f[1,2],(f[1,1]+f[1,2])) #control
binom.test(f[2,2],(f[2,1]+f[2,2])) #intervention

#lung disease response-efficacy

binom.test(g[1,2],(g[1,1]+g[1,2])) #control
binom.test(g[2,2],(g[2,1]+g[2,2])) #intervention

#quitting self-efficacy

binom.test(h[1,2],(h[1,1]+h[1,2])) #control
binom.test(h[2,2],(h[2,1]+h[2,2])) #intervention

#intention

binom.test(i[1,2],(i[1,1]+i[1,2])) #control
binom.test(i[2,2],(i[2,1]+i[2,2])) #intervention
