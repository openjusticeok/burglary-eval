library(tidyverse)
library(dplyr)
library(plotly)
library(ojodb)
connect_ojo()
#list of tables
ojo_list_tables()
#list of variables in table
dbListFields(ojo_db, 'ojo_crim_cases')

### query both years at the same time ####

casesall <- ojo_tbl('ojo_crim_cases') %>%
  filter(court %in% c("ADAIR", "CANADIAN", "CLEVELAND", "COMANCHE", "ELLIS", "GARFIELD", "LOGAN", "OKLAHOMA", "PAYNE", "PUSHMATAHA","ROGERMILLS", "ROGERS", "TULSA"), file_year >= 2018, casetype == "CF") %>%
  collect()
burgall <- casesall %>%
  mutate(burglary = str_detect(top_ct_desc, "BURGLARY") & !str_detect(top_ct_desc, "TOOL")) %>%
  filter(burglary == TRUE)

### Now, instead of creating a new dataframe for each category of burglary,
###we can create a new variable that tells us which one it is. Look at the ?case_when function -
###super useful in these situations. It's basically a long if-then statement.
###If you detect "FIRST" in top_ct_desc, then put "FIRST DEGREE". If not, then look for "SECOND", etc.

burgall <- burgall %>%
  mutate(burg_cat = case_when(str_detect(top_ct_desc, "FIRST|1") |
                                str_detect(top_ct_stat, "1431") ~ "FIRST DEGREE",
                              str_detect(top_ct_desc, "SECOND|2|SEOND") |
                                str_detect(top_ct_stat, "1435$") ~ "SECOND DEGREE",
                              str_detect(top_ct_desc, "THIRD|3|THRID") ~ "THIRD DEGREE"),
         file_month = floor_date(ymd(file_date), 'month'))

## Now we can summarize them all into one dataframe, avoiding all the repetition above.
##One of the best habits you can get in is recognizing when you're repeating things a lot
##and think about how you can let the machine do that work for you. Takes a lot of practice, but very worth it!

burg_sum <- burgall %>%
  count(file_month, burg_cat) # Counting two variables looks for all unique combinations - look at burg_sum to see what it does
## Plot them all next to each other - makes it easier to see how they interact
library(ggplot2)

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

#Same but for dispositions:


disps1820 <- ojo_tbl('oscn_crim_disps') %>%
  filter(court %in% c("ADAIR", "CANADIAN", "CLEVELAND", "COMANCHE", "ELLIS", "GARFIELD", "LOGAN", "OKLAHOMA", "PAYNE", "PUSHMATAHA","ROGERMILLS", "ROGERS", "TULSA"), file_year >= 2018, casetype == "CF") %>%
  collect()

dispsall1820 <- disps1820 %>%
  mutate(burglary = str_detect(disp_desc, "BURGLARY") & !str_detect(disp_desc, "TOOL")) %>%
  filter(burglary == TRUE)

dispsall1820 <- disps1820 %>%
  mutate(burg_cat = case_when(str_detect(disp_desc, "FIRST|1") ~ "FIRST DEGREE",
                              str_detect(disp_desc, "SECOND|2") ~ "SECOND DEGREE",
                              str_detect(disp_desc, "THIRD|3") ~ "THIRD DEGREE"),
         file_month = floor_date(ymd(disp_date), 'month'))

disps_sum1820 <- dispsall1820 %>%
  count(file_month, burg_cat) # Counting two variables looks for all unique combinations - look at burg_sum to see what it does

ggplot(disps_sum1820, aes(file_month, n, group = burg_cat, color = burg_cat)) +
  geom_line() +
  theme_ojo() +
  ylim(0, 550) +
  ggtitle("Number of Burglary Dispositions in OSCN Counties in 2018-20") +
  scale_color_manual(values = ojo_pal)

###Dummy var code WORKING  ####

### Inspect burg_cat NAs ####
burgall %>%
  filter(is.na(burg_cat)) %>%
  view

### Create dummy variable for implementation of burg 3
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

###THIRD DEGREE

burg3disp_sum <- disps_sum1820 %>%
  filter(burg_cat == "THIRD DEGREE")

linearMod3disp <- lm(n ~ burg_reform + moy,
                     data = burg3disp_sum)

print(linearMod3disp)
summary(linearMod3disp)


#need to get rid of NAs (still a lot of them for disps)
