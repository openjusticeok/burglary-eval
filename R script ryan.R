library(tidyverse)
library(dplyr)
library(plotly)
library(ojodb)
library(ggplot2)
connect_ojo()

### CASES Analysis

casesall <- ojo_tbl('ojo_crim_cases') %>%
  filter(court %in% c("ADAIR", "CANADIAN", "CLEVELAND", "COMANCHE", "ELLIS", "GARFIELD", "LOGAN", "OKLAHOMA", "PAYNE", "PUSHMATAHA","ROGERMILLS", "ROGERS", "TULSA"), file_year >= 2018, casetype == "CF") %>%
  collect()

burgall <- casesall %>%
  mutate(burglary = str_detect(top_ct_desc, "BURGLARY") & !str_detect(top_ct_desc, "TOOL")) %>%
  filter(burglary == TRUE)

burgall <- burgall %>%
  mutate(burg_cat = case_when(str_detect(top_ct_desc, "FIRST|1") |
                                str_detect(top_ct_stat, "1431") ~ "FIRST DEGREE",
                              str_detect(top_ct_desc, "SECOND|2|SEOND") |
                                str_detect(top_ct_stat, "1435$") ~ "SECOND DEGREE",
                              str_detect(top_ct_desc, "THIRD|3|THRID") ~ "THIRD DEGREE"),
         file_month = floor_date(ymd(file_date), 'month'))

burg_sum <- burgall %>%
  count(file_month, burg_cat) 

ggplot(burg_sum, aes(file_month, n, group = burg_cat, color = burg_cat)) +
  geom_line() +
  geom_vline(aes(xintercept = ymd("2018-11-01"))) +
  theme_ojo() +
  xlim(ymd("2018-01-01"), NA) +
  ylim(0, NA) +
  ggtitle("Number of Burglary Cases Filed in OSCN Counties") +
  scale_color_manual(values = ojo_pal)

casesall %>%
  mutate(file_month = floor_date(ymd(file_date), 'month')) %>%
  count(file_month) %>%
  filter(file_month > ymd("2017-01-01")) %>%
  ggplot(aes(file_month, n)) +
  geom_line() +
  theme_ojo() +
  ylim(0, NA) +
  ggtitle("Number of Felony Cases Filed in OSCN Counties") +
  scale_color_manual(values = ojo_pal)

### Check NAs 

burgall %>%
  filter(is.na(burg_cat)) %>%
  view

### Create dummy variable for implementation of burg 3rd

burg_sum <- burg_sum %>%
  mutate(burg_reform = if_else(file_month >= ymd("2018-11-01"),
                               1,
                               0),
         moy = month(file_month, label = TRUE) %>%
           as.character) %>%
  filter(year(file_month) >= 2018)

?month

### Linear model

burg2_sum <- burg_sum %>%
  filter(burg_cat == "SECOND DEGREE")

linearMod <- lm(n ~ burg_reform + moy,
                data = burg2_sum)

print(linearMod)
summary(linearMod)

###
burg1_sum <- burg_sum %>%
  filter(burg_cat == "FIRST DEGREE")

linearMod1 <- lm(n ~ burg_reform + moy,
                 data = burg1_sum)

print(linearMod1)
summary(linearMod1)

##DISPOSITIONS

disps1820 <- ojo_tbl('oscn_crim_disps') %>%
  filter(court %in% c("ADAIR", "CANADIAN", "CLEVELAND", "COMANCHE", "ELLIS", "GARFIELD", "LOGAN", "OKLAHOMA", "PAYNE", "PUSHMATAHA","ROGERMILLS", "ROGERS", "TULSA"), file_year >= 2018, casetype == "CF") %>%
  collect()

dispsall1820 <- disps1820 %>%
  mutate(burglary = str_detect(ct_desc, "BURGLARY") & !str_detect(ct_desc, "TOOL")) %>%
  filter(burglary == TRUE)

dispsall1820 <- dispsall1820 %>%
  mutate(burg_cat = case_when(str_detect(disp_desc, "FIRST|1") |
                                str_detect(disp_stat, "1431") ~ "FIRST DEGREE",
                              str_detect(disp_desc, "SECOND|2|SEOND") |
                                str_detect(disp_stat, "1435$") ~ "SECOND DEGREE",
                              str_detect(disp_desc, "THIRD|3|THRID") ~ "THIRD DEGREE"),
         file_month = floor_date(ymd(disp_date), 'month'))

dispsall1820 %>%
  filter(is.na(burg_cat)) %>%
  view


disps_sum1820 <- dispsall1820 %>%
  count(file_month, burg_cat) # Counting two variables looks for all unique combinations - look at burg_sum to see what it does


disps_sum1820 <- disps_sum1820 %>%
  mutate(burg_reform = if_else(file_month >= ymd("2018-11-01"),
                               1,
                               0),
         moy = month(file_month, label = TRUE) %>%
           as.character) %>%
  filter(year(file_month) >= 2018)

ggplot(disps_sum1820, aes(file_month, n, group = burg_cat, color = burg_cat)) +
  geom_line() +
  theme_ojo() +
  ylim(0, NA) +
  ggtitle("Number of Burglary Dispositions in OSCN Counties in 2018-20") +
  scale_color_manual(values = ojo_pal)

### Linear model
burg2_disp <- disps_sum1820 %>%
  filter(burg_cat == "SECOND DEGREE")

linearMod2 <- lm(n ~ burg_reform + moy,
                 data = burg2_disp)

print(linearMod2)
summary(linearMod2)

### FIRST DEGREE 

burg1disp_sum <- disps_sum1820 %>%
  filter(burg_cat == "FIRST DEGREE")

linearMod1disp <- lm(n ~ burg_reform + moy,
                 data = burg1disp_sum)

print(linearMod1disp)
summary(linearMod1disp)


