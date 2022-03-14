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

#non-dichotomised results ---------------------------------------------

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

#VR headset type ---------------------------------------------

df2 <- df1 %>%
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

table(df2$group, df2$vr_not_reported)

df2 %>%
  filter(vr_not_reported == "Reported") %>%
  janitor::tabyl(group, type_vr)
