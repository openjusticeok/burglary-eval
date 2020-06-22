install.packages("devtools")
devtools::install_github("username/packagename")

library(tidyverse)
library(dplyr)
library(ojodb)
connect_ojo()
#list of tables
ojo_list_tables()
#list of variables in table
dbListFields(ojo_db, 'oscn_crim_disps')

#Make an ALL CASES VAR HERE

casesall19 <- ojo_tbl('ojo_crim_cases') %>%
  filter(court %in% c("ADAIR", "CANADIAN", "CLEVELAND", "COMANCHE", "ELLIS", "GARFIELD", "LOGAN", "OKLAHOMA", "PAYNE", "PUSHMATAHA","ROGERMILLS", "ROGERS", "TULSA"), file_year == 2019, casetype == "CF") %>%
  collect()

#diff counties

casestulsa19 <- ojo_tbl('ojo_crim_cases') %>%
  filter(court == "TULSA", file_year == 2019, casetype == "CF") %>%
  collect()

?show_row



#just focus on the 13 oscn counties 
disps <- ojo_tbl('oscn_crim_disps') %>%
  filter(court == "TULSA", file_year == 2019, casetype == "CF") %>%
  collect()

#see which are oscn counties
ojo_tbl("oscn_crim_disps") %>% 
  select(court) %>% 
  distinct %>% 
  collect()

dispsall19 <- ojo_tbl('oscn_crim_disps') %>%
  filter(court %in% c("ADAIR", "CANADIAN", "CLEVELAND", "COMANCHE", "ELLIS", "GARFIELD", "LOGAN", "OKLAHOMA", "PAYNE", "PUSHMATAHA","ROGERMILLS", "ROGERS", "TULSA"), file_year == 2019, casetype == "CF") %>%
  collect() %>%
  view


show_row(disps, 67)

disconnect_ojo()



# identify cases with a top count of burglary

casesall19 %>% # get a list of all unique values of top_ct_desc
  select(top_ct_desc) %>%
  distinct() %>%
  view

?str_detect 

burgall19 <- casesall19 %>%
  mutate(burglary = str_detect(top_ct_desc, "BURGLARY") & !str_detect(top_ct_desc, "TOOL")) %>%
  filter(burglary == TRUE)

#diff before 3rd and 2nd degree burg - auto look at what changed 

#count filings by month

burg_sum19 <- burgall19 %>%
  mutate(file_month = floor_date(ymd(file_date), 'month')) %>%
  count(file_month)
  
#plot filings  by month
?floor_date

#this one works

ggplot(burg_sum19, aes(file_month, n, color = 'court')) +
  geom_line() +
  theme_ojo() +
  ylim(0, NA) +
  ggtitle("Number of Burglary Cases Filed in OSCN Counties 2019") +
  scale_color_manual(values = ojo_pal)

#ymd for dates as characters, cleans dates
#start by finding key dates and characteristics of burglary changes, translate changes > what changes would you expect

#mutate, filter, groupby, count


#Trying for 2020 cases below

casesall20 <- ojo_tbl('ojo_crim_cases') %>%
  filter(court %in% c("ADAIR", "CANADIAN", "CLEVELAND", "COMANCHE", "ELLIS", "GARFIELD", "LOGAN", "OKLAHOMA", "PAYNE", "PUSHMATAHA","ROGERMILLS", "ROGERS", "TULSA"), file_year == 2020, casetype == "CF") %>%
  collect()

burgall20 <- casesall20 %>%
  mutate(burglary = str_detect(top_ct_desc, "BURGLARY") & !str_detect(top_ct_desc, "TOOL")) %>%
  filter(burglary == TRUE)

#diff before 3rd and 2nd degree burg - auto look at what changed 

#count filings by month

burg20_sum <- burgall20 %>%
  mutate(file_month = floor_date(ymd(file_date), 'month')) %>%
  count(file_month)

ggplot(burg20_sum, aes(file_month, n, color = 'court')) +
  geom_line() +
  theme_ojo() +
  ylim(0, NA) +
  ggtitle("Number of Burglary Cases Filed in OSCN Counties 2020") +
  scale_color_manual(values = ojo_pal)

#Cases dropped after March, COVID probably 

#Just first degree
burg1st19 <- casesall19 %>%
  mutate(burglary = str_detect(top_ct_desc, "BURGLARY - FIRST DEGREE") & !str_detect(top_ct_desc, "TOOL")) %>%
  filter(burglary == TRUE)

burg1st20 <- casesall20 %>%
  mutate(burglary = str_detect(top_ct_desc, "BURGLARY - FIRST DEGREE") & !str_detect(top_ct_desc, "TOOL")) %>%
  filter(burglary == TRUE)

burg_sum1st19 <- burg1st19 %>%
  mutate(file_month = floor_date(ymd(file_date), 'month')) %>%
  count(file_month)

burg_sum1st20 <- burg1st20 %>%
  mutate(file_month = floor_date(ymd(file_date), 'month')) %>%
  count(file_month)

ggplot(burg_sum1st20, aes(file_month, n, color = 'court')) +
  geom_line() +
  theme_ojo() +
  ylim(0, NA) +
  ggtitle("Number of 1st Burglary Cases Filed in OSCN Counties 2020") +
  scale_color_manual(values = ojo_pal)

ggplot(burg_sum1st19, aes(file_month, n, color = 'court')) +
  geom_line() +
  theme_ojo() +
  ylim(0, NA) +
  ggtitle("Number of 1st Burglary Cases Filed in OSCN Counties 2019") +
  scale_color_manual(values = ojo_pal)


#Just second degree
burg2nd20 <- casesall20 %>%
  mutate(burglary = str_detect(top_ct_desc, "BURGLARY - SECOND DEGREE") & !str_detect(top_ct_desc, "TOOL")) %>%
  filter(burglary == TRUE)

burg2nd19 <- casesall19 %>%
  mutate(burglary = str_detect(top_ct_desc, "BURGLARY - SECOND DEGREE") & !str_detect(top_ct_desc, "TOOL")) %>%
  filter(burglary == TRUE)


burg_sum2nd20 <- burg2nd20 %>%
  mutate(file_month = floor_date(ymd(file_date), 'month')) %>%
  count(file_month)

burg_sum2nd19 <- burg2nd19 %>%
  mutate(file_month = floor_date(ymd(file_date), 'month')) %>%
  count(file_month)


ggplot(burg_sum2nd20, aes(file_month, n, color = 'court')) +
  geom_line() +
  theme_ojo() +
  ylim(0, NA) +
  ggtitle("Number of 2nd Burglary Cases Filed in OSCN Counties 2020") +
  scale_color_manual(values = ojo_pal)

ggplot(burg_sum2nd19, aes(file_month, n, color = 'court')) +
  geom_line() +
  theme_ojo() +
  ylim(0, NA) +
  ggtitle("Number of 2nd Burglary Cases Filed in OSCN Counties 2019") +
  scale_color_manual(values = ojo_pal)

#Just 3rd Degree
#dispositions check
burg3rd19 <- casesall19 %>%
  mutate(burglary = str_detect(top_ct_desc, "BURGLARY - THIRD DEGREE") & !str_detect(top_ct_desc, "TOOL")) %>%
  filter(burglary == TRUE)

burg3rd20 <- casesall20 %>%
  mutate(burglary = str_detect(top_ct_desc, "BURGLARY - THIRD DEGREE") & !str_detect(top_ct_desc, "TOOL")) %>%
  filter(burglary == TRUE)
#try this
#mutate(burglary_3 = str_detect(top_ct_desc, "BURG") & str_detect(top_ct_desc, "3|THIRD") & !str_detect(top_ct_desc, "TOOL")

burg_sum3rd19 <- burg3rd19 %>%
  mutate(file_month = floor_date(ymd(file_date), 'month')) %>%
  count(file_month)

burg_sum3rd20 <- burg3rd20 %>%
  mutate(file_month = floor_date(ymd(file_date), 'month')) %>%
  count(file_month)

#Only 1 3rd degree Burgs in 2020, 2 in 2019
#Try disps instead? 

#Possession with intent to distribute 

PWI19 <- casesall19 %>%
  mutate(Drug = str_detect(top_ct_desc, "POSSESSION OF CONTROLLED DRUG WITH INTENT TO DISTRIBUTE") & !str_detect(top_ct_desc, "TOOL")) %>%
  filter(Drug == TRUE)

PWI20 <- casesall20 %>%
  mutate(Drug = str_detect(top_ct_desc, "POSSESSION OF CONTROLLED DRUG WITH INTENT TO DISTRIBUTE") & !str_detect(top_ct_desc, "TOOL")) %>%
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

#Comparing top with bottom income counties
#This code is not finished 

casescan19 <- ojo_tbl('ojo_crim_cases') %>%
  filter(court == "CANADIAN", file_year == 2019, casetype == "CF") %>%
  collect()

casescan20 <- ojo_tbl('ojo_crim_cases') %>%
  filter(court == "CANADIAN", file_year == 2020, casetype == "CF") %>%
  collect() 

burg2ndcomp19 <- casescomp19 %>%
  mutate(burglary = str_detect(top_ct_desc, "BURGLARY - SECOND DEGREE") & !str_detect(top_ct_desc, "TOOL")) %>%
  filter(burglary == TRUE)

burg_sum2ndcomp19 <- burg2nd20 %>%
  mutate(file_month = floor_date(ymd(file_date), 'month')) %>%
  count(file_month)

ggplot(burg_sum2nd20, aes(file_month, n, color = 'court')) +
  geom_line() +
  theme_ojo() +
  ylim(0, NA) +
  ggtitle("Number of 2nd Burglary Cases Filed in Tulsa County 2020") +
  scale_color_manual(values = ojo_pal)

ggplot(burg_sum2nd, aes(file_month, n, color = 'court')) +
  geom_line() +
  theme_ojo() +
  xlab("Month Filed") +
  ylim(0, NA) +
  ggtitle("Number of 2nd Burglary Cases Filed in Tulsa County") +
  scale_color_manual(values = ojo_pal)


