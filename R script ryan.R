library(tidyverse)
library(dplyr)
library(ojodb)
connect_ojo()
#list of tables
ojo_list_tables()
#list of variables in table
dbListFields(ojo_db, 'oscn_crim_disps')


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
  mutate(burg_cat = case_when(str_detect(top_ct_desc, "FIRST|1") ~ "FIRST DEGREE",
                              str_detect(top_ct_desc, "SECOND|2") ~ "SECOND DEGREE",
                              str_detect(top_ct_desc, "THIRD|3") ~ "THIRD DEGREE"),
         file_month = floor_date(ymd(file_date), 'month'))

## Now we can summarize them all into one dataframe, avoiding all the repetition above. 
##One of the best habits you can get in is recognizing when you're repeating things a lot 
##and think about how you can let the machine do that work for you. Takes a lot of practice, but very worth it!

#Starts in 2019 not 2018 for some reason??? 
burg_sum <- burgall %>%
  count(file_month, burg_cat) # Counting two variables looks for all unique combinations - look at burg_sum to see what it does

## Plot them all next to each other - makes it easier to see how they interact
library(ggplot2)
##GRAPH NEEDS AN XLIM MAYBE? SOMETHINGS WRONG

ggplot(burg_sum, aes(file_month, n, group = burg_cat, color = burg_cat)) +
  geom_line() +
  geom_vline(aes(xintercept = ymd("2018-11-01"))) +
  theme_ojo() +
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
  collect() %>%
  view

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

#Possession with intent to distribute (not condensed code yet)

PWI19 <- casesall19 %>%
  mutate(Drug = str_detect(top_ct_desc, "DRUG") & str_detect(top_ct_desc, "POSSESSION|DIST") & !str_detect(top_ct_desc, "TOOL")) %>%
  filter(Drug == TRUE)

PWI20 <- casesall20 %>%
  mutate(Drug = str_detect(top_ct_desc, "DRUG") & str_detect(top_ct_desc, "POSSESSION|DIST") & !str_detect(top_ct_desc, "TOOL")) %>%
  filter(Drug == TRUE)

PWI_sum19 <- PWI19 %>%
  mutate(file_month = floor_date(ymd(file_date), 'month')) %>%
  count(file_month)

PWI_sum20 <- PWI20 %>%
  mutate(file_month = floor_date(ymd(file_date), 'month')) %>%
  count(file_month)

ggplot(PWI_sum20, aes(file_month, n, color = 'court')) +
  geom_line() +
  theme_ojo() +
  ylim(0, NA) +
  ggtitle("Number of PWI  Cases Filed in OSCN Counties 2020") +
  scale_color_manual(values = ojo_pal)

ggplot(PWI_sum19, aes(file_month, n, color = 'court')) +
  geom_line() +
  theme_ojo() +
  ylim(0, NA) +
  ggtitle("Number of PWI Cases Filed in OSCN Counties 2019") +
  scale_color_manual(values = ojo_pal)

#Possession with intent to distribute DISPS

PWI19disp <- dispsall19 %>%
  mutate(Drug = str_detect(disp_desc, "DRUG") & str_detect(disp_desc, "POSSESSION|DIST") & !str_detect(disp_desc, "TOOL")) %>%
  filter(Drug == TRUE)

PWI20disp <- dispsall20 %>%
  mutate(Drug = str_detect(disp_desc, "DRUG") & str_detect(disp_desc, "POSSESSION|DIST") & !str_detect(disp_desc, "TOOL")) %>%
  filter(Drug == TRUE)

PWI_dispsum19 <- PWI19disp %>%
  mutate(file_month = floor_date(ymd(disp_date), 'month')) %>%
  count(file_month)

PWI_dispsum20 <- PWI20disp %>%
  mutate(file_month = floor_date(ymd(disp_date), 'month')) %>%
  count(file_month)

ggplot(PWI_dispsum20, aes(file_month, n, color = 'court')) +
  geom_line() +
  theme_ojo() +
  ylim(0, NA) +
  ggtitle("Number of PWI  Disps in OSCN Counties 2020") +
  scale_color_manual(values = ojo_pal)

ggplot(PWI_dispsum19, aes(file_month, n, color = 'court')) +
  geom_line() +
  theme_ojo() +
  ylim(0, NA) +
  ggtitle("Number of PWI Disps in OSCN Counties 2019") +
  scale_color_manual(values = ojo_pal)

###THIS IS SOME OF MY STATS CODE, WORKS FOR DATASET CREATED IN EXCEL

library(tidyverse)
library(dplyr)
library(plotly)
library(hrbrthemes)
library(ggplot2)

ds <- read_csv("/Users/14057/Documents/OJOds.csv")

view(ds)

t.test(n_2nd ~ bill, data = ds)

#lm(formula = n_2nd ~ n_3rd + bill, data = ds)

myts <- ts(ds, start=c(2018, 11), end=c(2020, 06), frequency=12)

# plot series
plot(myts)


