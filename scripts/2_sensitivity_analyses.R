#load packages -----------------------------------------------------------

source(here::here("scripts", "0_load_packages.R"))

#load data ---------------------------------------------------------------

data_raw <- download_zenodo("10.5281/zenodo.5747706", path = here("data"))

data <- read_xlsx(here("data","VR_dataset_29.10.21.xlsx"))

#non-dichotomised results ---------------------------------------------

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

a <- table(df1$group, df1$angry)
a
chisq.test(a)

b <- table(df1$group, df1$distressed)
b
chisq.test(b)

c <- table(df1$group, df1$useful)
c
chisq.test(c)

d <- table(df1$group, df1$cancer)
d
chisq.test(d)

e <- table(df1$group, df1$heart_disease)
e
chisq.test(e)

f <- table(df1$group, df1$lung_disease)
f
chisq.test(f)

g <- table(df1$group, df1$cancer_response_efficacy)
g
chisq.test(g)

h <- table(df1$group, df1$heart_response_efficacy)
h
chisq.test(h)

i <- table(df1$group, df1$lung_response_efficacy)
i
chisq.test(i)

j <- table(df1$group, df1$quitting_confidence)
j
chisq.test(j)

k <- table(df1$group, df1$intention)
k
chisq.test(k)

#continuous results ------------------------------------------

#recode variables

df2 <- data %>%
  mutate(group = as.factor(group),
         angry_num = as.numeric(case_when(angry == "Strongly disagree" ~ 1,
                                          angry == "Disagree" ~ 2,
                                          angry == "Somewhat disagree" ~ 3,
                                          angry == "Neither agree nor disagree" ~ 4,
                                          angry == "Somewhat agree" ~ 5,
                                          angry == "Agree" ~ 6,
                                          angry == "Strongly agree" ~ 7,
                                          TRUE ~ 0)),
         distressed_num = as.numeric(case_when(distressed == "Strongly disagree" ~ 1,
                                               distressed == "Disagree" ~ 2,
                                               distressed == "Somewhat disagree" ~ 3,
                                               distressed == "Neither agree nor disagree" ~ 4,
                                               distressed == "Somewhat agree" ~ 5,
                                               distressed == "Agree" ~ 6,
                                               distressed == "Strongly agree" ~ 7,
                                               TRUE ~ 0)),
         useful_num = as.numeric(case_when(useful == "Strongly disagree" ~ 1,
                                           useful == "Disagree" ~ 2,
                                           useful == "Somewhat disagree" ~ 3,
                                           useful == "Neither agree nor disagree" ~ 4,
                                           useful == "Somewhat agree" ~ 5,
                                           useful == "Agree" ~ 6,
                                           useful == "Strongly agree" ~ 7,
                                           TRUE ~ 0)),
         cancer_num = as.numeric(case_when(cancer == "Not at all" ~ 1,
                                           cancer == "Moderately unlikely" ~ 2,
                                           cancer == "Neither likely nor unlikely" ~ 3,
                                           cancer == "Moderately likely" ~ 4,
                                           cancer == "Very likely" ~ 5,
                                           TRUE ~ 0)),
         heart_disease_num = as.numeric(case_when(heart_disease == "Not at all" ~ 1,
                                                  heart_disease == "Moderately unlikely" ~ 2,
                                                  heart_disease == "Neither likely nor unlikely" ~ 3,
                                                  heart_disease == "Moderately likely" ~ 4,
                                                  heart_disease == "Very likely" ~ 5,
                                                  TRUE ~ 0)),
         lung_disease_num = as.numeric(case_when(lung_disease == "Not at all" ~ 1,
                                                 lung_disease == "Moderately unlikely" ~ 2,
                                                 lung_disease == "Neither likely nor unlikely" ~ 3,
                                                 lung_disease == "Moderately likely" ~ 4,
                                                 lung_disease == "Very likely" ~ 5,
                                                 TRUE ~ 0)),
         cancer_response_efficacy_num = as.numeric(case_when(cancer_response_efficacy == "Strongly disagree" ~ 1,
                                                             cancer_response_efficacy == "Disagree" ~ 2,
                                                             cancer_response_efficacy == "Somewhat disagree" ~ 3,
                                                             cancer_response_efficacy == "Neither agree nor disagree" ~ 4,
                                                             cancer_response_efficacy == "Somewhat agree" ~ 5,
                                                             cancer_response_efficacy == "Agree" ~ 6,
                                                             cancer_response_efficacy == "Strongly agree" ~ 7,
                                                             TRUE ~ 0)),
         heart_response_efficacy_num = as.numeric(case_when(heart_response_efficacy == "Strongly disagree" ~ 1,
                                                            heart_response_efficacy == "Disagree" ~ 2,
                                                            heart_response_efficacy == "Somewhat disagree" ~ 3,
                                                            heart_response_efficacy == "Neither agree nor disagree" ~ 4,
                                                            heart_response_efficacy == "Somewhat agree" ~ 5,
                                                            heart_response_efficacy == "Agree" ~ 6,
                                                            heart_response_efficacy == "Strongly agree" ~ 7,
                                                            TRUE ~ 0)),
         lung_response_efficacy_num = as.numeric(case_when(lung_response_efficacy == "Strongly disagree" ~ 1,
                                                           lung_response_efficacy == "Disagree" ~ 2,
                                                           lung_response_efficacy == "Somewhat disagree" ~ 3,
                                                           lung_response_efficacy == "Neither agree nor disagree" ~ 4,
                                                           lung_response_efficacy == "Somewhat agree" ~ 5,
                                                           lung_response_efficacy == "Agree" ~ 6,
                                                           lung_response_efficacy == "Strongly agree" ~ 7,
                                                           TRUE ~ 0)),
         quitting_confidence_num = as.numeric(case_when(quitting_confidence == "Not at all" ~ 1,
                                                        quitting_confidence == "Slightly" ~ 2,
                                                        quitting_confidence == "Moderately" ~ 3,
                                                        quitting_confidence == "Very" ~ 4,
                                                        quitting_confidence == "Extremely" ~ 5,
                                                        TRUE ~ 0)),
         intention = as.factor(intention))

#produce a summary table

mylabels <- list(angry_num = "Angry", distressed_num = "Distressed", useful_num = "Useful", cancer_num = "Perceived susceptibility to cancer",
                 heart_disease_num = "Perceived susceptibility to heart disease", lung_disease_num = "Perceived susceptibility to lung disease",
                 cancer_response_efficacy_num = "Cancer response efficacy", heart_response_efficacy_num = "Heart disease response efficacy",
                 lung_response_efficacy_num = "Lung disease response efficacy", quitting_confidence_num = "Quitting self-efficacy",
                 intention = "Intention to stop smoking")

table <- tableby(~ angry_num + distressed_num + useful_num + cancer_num + heart_disease_num +
                   lung_disease_num + cancer_response_efficacy_num + heart_response_efficacy_num +
                   lung_response_efficacy_num + quitting_confidence_num + intention, data = df2, strata = group, cat.simplify = F, test = T)

table_summary <- summary(table, text = T, labelTranslations = mylabels, digits = 2)

write2word(table_summary, here("outputs","supp_table.doc"))

#run series of linear and logistic regression analyses

lm_angry = lm(angry_num ~ group, data = df2)
summary(lm_angry)

lm_distressed = lm(distressed_num ~ group, data = df2)
summary(lm_distressed)

lm_useful = lm(useful_num ~ group, data = df2)
summary(lm_useful)

lm_cancer = lm(cancer_num ~ group, data = df2)
summary(lm_cancer)

lm_heart_disease = lm(heart_disease_num ~ group, data = df2)
summary(lm_heart_disease)

lm_lung_disease = lm(lung_disease_num ~ group, data = df2)
summary(lm_lung_disease)

lm_cancer_response_efficacy = lm(cancer_response_efficacy_num ~ group, data = df2)
summary(lm_cancer_response_efficacy)

lm_heart_response_efficacy = lm(heart_response_efficacy_num ~ group, data = df2)
summary(lm_heart_response_efficacy)

lm_lung_response_efficacy = lm(lung_response_efficacy_num ~ group, data = df2)
summary(lm_lung_response_efficacy)

lm_quitting_confidence = lm(quitting_confidence_num ~ group, data = df2)
summary(lm_quitting_confidence)

glm_intention = glm(intention ~ group, family = binomial(link = "logit"), data = df2)
summary(glm_intention)
exp(coef(glm_intention))

#correlation between acceptability and susceptibility measures

cor.test(df2$angry_num, df2$cancer_num, method = "pearson")
cor.test(df2$angry_num, df2$heart_disease_num, method = "pearson")
cor.test(df2$angry_num, df2$lung_disease_num, method = "pearson")

cor.test(df2$distressed_num, df2$cancer_num, method = "pearson")
cor.test(df2$distressed_num, df2$heart_disease_num, method = "pearson")
cor.test(df2$distressed_num, df2$lung_disease_num, method = "pearson")

cor.test(df2$useful_num, df2$cancer_num, method = "pearson")
cor.test(df2$useful_num, df2$heart_disease_num, method = "pearson")
cor.test(df2$useful_num, df2$lung_disease_num, method = "pearson")

#VR headset type ---------------------------------------------

df3 <- df1 %>%
  mutate(vr_not_reported = as.factor(case_when(type_vr == "2018 model" | 
                                                 type_vr == "blesy" | 
                                                 type_vr == "I don't know" | 
                                                 type_vr == "I need the cardboard cut out sent to me" | 
                                                 type_vr == "VR" | 
                                                 type_vr == "n/a" |
                                                 type_vr == "None" |
                                                 type_vr == "samsung" |
                                                 type_vr == "Samsung Galaxy S7 edge" |
                                                 type_vr == "Smartphone" |
                                                 type_vr == "Smartphone" |
                                                 is.na(type_vr) ~ "Not reported",
                                          TRUE ~ "Reported")))

table(df3$group, df3$vr_not_reported)

df3 %>%
  filter(vr_not_reported == "Reported") %>%
  janitor::tabyl(group, type_vr)
