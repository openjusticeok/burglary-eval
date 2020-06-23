library(tidyverse)
library(dplyr)
library(ojodb)
connect_ojo()
#list of tables
ojo_list_tables()
#list of variables in table
dbListFields(ojo_db, 'oscn_crim_disps')

#NOTE TO RYAN-- I left notes explaining all code and some have questions (all caps)
#Also-- these variable names are messy. I'm going to be changing them all today

#This is the variable for cases in ALL OSCN counties

casesall19 <- ojo_tbl('ojo_crim_cases') %>%
  filter(court %in% c("ADAIR", "CANADIAN", "CLEVELAND", "COMANCHE", "ELLIS", "GARFIELD", "LOGAN", "OKLAHOMA", "PAYNE", "PUSHMATAHA","ROGERMILLS", "ROGERS", "TULSA"), file_year == 2019, casetype == "CF") %>%
  collect()

#Criminal Disps for all counties

dispsall19 <- ojo_tbl('oscn_crim_disps') %>%
  filter(court %in% c("ADAIR", "CANADIAN", "CLEVELAND", "COMANCHE", "ELLIS", "GARFIELD", "LOGAN", "OKLAHOMA", "PAYNE", "PUSHMATAHA","ROGERMILLS", "ROGERS", "TULSA"), file_year == 2019, casetype == "CF") %>%
  collect() %>%
  view

# Cases with a top count of burglary

casesall19 %>% # get a list of all unique values of top_ct_desc
  select(top_ct_desc) %>%
  distinct() %>%
  view

?str_detect

burgall19 <- casesall19 %>%
  mutate(burglary = str_detect(top_ct_desc, "BURGLARY") & !str_detect(top_ct_desc, "TOOL")) %>%
  filter(burglary == TRUE)

#count filings by month
burg_sum19 <- burgall19 %>%
  mutate(file_month = floor_date(ymd(file_date), 'month')) %>%
  count(file_month)

#Add court county-- this doesn't graph right, need to get each county own line

#burg_sum19 <- burgall19 %>%
 # mutate(file_month = floor_date(ymd(file_date), 'month')) %>%
 # count(file_month, court)


#plot filings  by month
?floor_date

ggplot(burg_sum19, aes(file_month, n, color = 'court')) +
  geom_line() +
  theme_ojo() +
  ylim(0, NA) +
  ggtitle("Number of Burglary Cases Filed in OSCN Counties 2019") +
  scale_color_manual(values = ojo_pal)

#2020 cases below

casesall20 <- ojo_tbl('ojo_crim_cases') %>%
  filter(court %in% c("ADAIR", "CANADIAN", "CLEVELAND", "COMANCHE", "ELLIS", "GARFIELD", "LOGAN", "OKLAHOMA", "PAYNE", "PUSHMATAHA","ROGERMILLS", "ROGERS", "TULSA"), file_year == 2020, casetype == "CF") %>%
  collect()

burgall20 <- casesall20 %>%
  mutate(burglary = str_detect(top_ct_desc, "BURGLARY") & !str_detect(top_ct_desc, "TOOL")) %>%
  filter(burglary == TRUE)

burg20_sum <- burgall20 %>%
  mutate(file_month = floor_date(ymd(file_date), 'month')) %>%
  count(file_month)

ggplot(burg20_sum, aes(file_month, n, color = 'court')) +
  geom_line() +
  theme_ojo() +
  ylim(0, NA) +
  ggtitle("Number of Burglary Cases Filed in OSCN Counties 2020") +
  scale_color_manual(values = ojo_pal)

#Note: Cases dropped after March, COVID probably

#Just first degree Burg

burg1st19 <- casesall19 %>%
  mutate(burglary = str_detect(top_ct_desc, "BURG") & str_detect(top_ct_desc, "1|FIRST") & !str_detect(top_ct_desc, "TOOL")) %>%
  filter(burglary == TRUE)

burg1st20 <- casesall20 %>%
  mutate(burglary = str_detect(top_ct_desc, "BURG") & str_detect(top_ct_desc, "1|FIRST") & !str_detect(top_ct_desc, "TOOL")) %>%
  filter(burglary == TRUE)

burg_sum1st19 <- burg1st19 %>%
  mutate(file_month = floor_date(ymd(file_date), 'month')) %>%
  count(file_month)

burg_sum1st20 <- burg1st20 %>%
  mutate(file_month = floor_date(ymd(file_date), 'month')) %>%
  count(file_month)

ggplot(burg_sum1st19, aes(file_month, n, color = 'court')) +
  geom_line() +
  theme_ojo() +
  ylim(0, NA) +
  ggtitle("Number of 1st Burglary Cases Filed in OSCN Counties 2019") +
  scale_color_manual(values = ojo_pal)

ggplot(burg_sum1st20, aes(file_month, n, color = 'court')) +
  geom_line() +
  theme_ojo() +
  ylim(0, NA) +
  ggtitle("Number of 1st Burglary Cases Filed in OSCN Counties 2020") +
  scale_color_manual(values = ojo_pal)

#Just second degree

burg2nd19 <- casesall19 %>%
  mutate(burglary = str_detect(top_ct_desc, "BURG") & str_detect(top_ct_desc, "2|SECOND") & !str_detect(top_ct_desc, "TOOL")) %>%
  filter(burglary == TRUE)

burg2nd20 <- casesall20 %>%
  mutate(burglary = str_detect(top_ct_desc, "BURG") & str_detect(top_ct_desc, "2|SECOND") & !str_detect(top_ct_desc, "TOOL")) %>%
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

burg3rd19 <- casesall19 %>%
  mutate(burglary = str_detect(top_ct_desc, "BURG") & str_detect(top_ct_desc, "3|THIRD") & !str_detect(top_ct_desc, "TOOL")) %>%
  filter(burglary == TRUE)

burg3rd20 <- casesall20 %>%
  mutate(burglary = str_detect(top_ct_desc, "BURG") & str_detect(top_ct_desc, "3|THIRD") & !str_detect(top_ct_desc, "TOOL")) %>%
  filter(burglary == TRUE)

burg_sum3rd19 <- burg3rd19 %>%
  mutate(file_month = floor_date(ymd(file_date), 'month')) %>%
  count(file_month)

burg_sum3rd20 <- burg3rd20 %>%
  mutate(file_month = floor_date(ymd(file_date), 'month')) %>%
  count(file_month)

ggplot(burg_sum3rd19, aes(file_month, n, color = 'court')) +
  geom_line() +
  theme_ojo() +
  ylim(0, NA) +
  ggtitle("Number of 3rd Degree Burglary Cases Filed in OSCN Counties 2019") +
  scale_color_manual(values = ojo_pal)

ggplot(burg_sum3rd20, aes(file_month, n, color = 'court')) +
  geom_line() +
  theme_ojo() +
  ylim(0, NA) +
  ggtitle("Number of 3rd Degree Burglary Cases Filed in OSCN Counties 2020") +
  scale_color_manual(values = ojo_pal)


### This is great work! To shorten this iteration, we can query both years at the same time: ####

casesall <- ojo_tbl('ojo_crim_cases') %>%
  filter(court %in% c("ADAIR", "CANADIAN", "CLEVELAND", "COMANCHE", "ELLIS", "GARFIELD", "LOGAN", "OKLAHOMA", "PAYNE", "PUSHMATAHA","ROGERMILLS", "ROGERS", "TULSA"), file_year >= 2019, casetype == "CF") %>%
  collect()

### Same as your code, but for both years
burgall <- casesall %>%
  mutate(burglary = str_detect(top_ct_desc, "BURGLARY") & !str_detect(top_ct_desc, "TOOL")) %>%
  filter(burglary == TRUE)

### Now, instead of creating a new dataframe for each category of burglary, we can create a new variable that tells us which one it is. Look at the ?case_when function - super useful in these situations. It's basically a long if-then statement. If you detect "FIRST" in top_ct_desc, then put "FIRST DEGREE". If not, then look for "SECOND", etc.

burgall <- burgall %>%
  mutate(burg_cat = case_when(str_detect(top_ct_desc, "FIRST|1") ~ "FIRST DEGREE",
                              str_detect(top_ct_desc, "SECOND|2") ~ "SECOND DEGREE",
                              str_detect(top_ct_desc, "THIRD|3") ~ "THIRD DEGREE"),
         file_month = floor_date(ymd(file_date), 'month'))

## Now we can summarize them all into one dataframe, avoiding all the repetition above. One of the best habits you can get in is recognizing when you're repeating things a lot and think about how you can let the machine do that work for you. Takes a lot of practice, but very worth it!

burg_sum <- burgall %>%
  count(file_month, burg_cat) # Counting two variables looks for all unique combinations - look at burg_sum to see what it does

## Plot them all next to each other - makes it easier to see how they interact

ggplot(burg_sum, aes(file_month, n, group = burg_cat, color = burg_cat)) +
  geom_line() +
  theme_ojo() +
  ylim(0, NA) +
  ggtitle("Number of Burglary Cases Filed in OSCN Counties") +
  scale_color_manual(values = ojo_pal)

#Possession with intent to distribute (NOTE TO RYAN-- I'm GOING TO LOOK THROUGH LATER, BUT DO YOU THINK MY STR_DETECT TERMS ARE OK?)

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

#Same but for dispositions:

#This is the variable for disps in ALL OSCN counties

dispsall19 <- ojo_tbl('oscn_crim_disps') %>%
  filter(court %in% c("ADAIR", "CANADIAN", "CLEVELAND", "COMANCHE", "ELLIS", "GARFIELD", "LOGAN", "OKLAHOMA", "PAYNE", "PUSHMATAHA","ROGERMILLS", "ROGERS", "TULSA"), file_year == 2019, casetype == "CF") %>%
  collect() %>%
  view

#NOTE TO RYAN--- SHOULD I BE USING "DISP_DESC" HERE? OR JUST "DISP"?

burgdisp19 <- dispsall19 %>%
  mutate(burglary = str_detect(disp_desc, "BURGLARY") & !str_detect(disp_desc, "TOOL")) %>%
  filter(burglary == TRUE)

#disps by month
burg_dispsum19 <- burgdisp19 %>%
  mutate(file_month = floor_date(ymd(disp_date), 'month')) %>%
  count(file_month)

#Add court county-- this doesn't graph right, need to get each county own line

#burg_sum19 <- burgall19 %>%
# mutate(file_month = floor_date(ymd(file_date), 'month')) %>%
# count(file_month, court)


#plot filings  by month
?floor_date

ggplot(burg_dispsum19, aes(file_month, n, color = 'court')) +
  geom_line() +
  theme_ojo() +
  ylim(0, NA) +
  ggtitle("Number of Burglary Disps in OSCN Counties 2019") +
  scale_color_manual(values = ojo_pal)

#2020 cases below

dispsall20 <- ojo_tbl('oscn_crim_disps') %>%
  filter(court %in% c("ADAIR", "CANADIAN", "CLEVELAND", "COMANCHE", "ELLIS", "GARFIELD", "LOGAN", "OKLAHOMA", "PAYNE", "PUSHMATAHA","ROGERMILLS", "ROGERS", "TULSA"), file_year == 2020, casetype == "CF") %>%
  collect()

burgdisp20 <- dispsall20 %>%
  mutate(burglary = str_detect(disp_desc, "BURGLARY") & !str_detect(disp_desc, "TOOL")) %>%
  filter(burglary == TRUE)

burgdisp20_sum <- burgdisp20 %>%
  mutate(file_month = floor_date(ymd(disp_date), 'month')) %>%
  count(file_month)

ggplot(burgdisp20_sum, aes(file_month, n, color = 'court')) +
  geom_line() +
  theme_ojo() +
  ylim(0, NA) +
  ggtitle("Number of Burglary Disps in OSCN Counties 2020") +
  scale_color_manual(values = ojo_pal)

#Note: Cases dropped after March, COVID probably

#Just first degree Burg

burg1stdisp19 <- dispsall19 %>%
  mutate(burglary = str_detect(disp_desc, "BURG") & str_detect(disp_desc, "1|FIRST") & !str_detect(disp_desc, "TOOL")) %>%
  filter(burglary == TRUE)

burg1stdisp20 <- dispsall20 %>%
  mutate(burglary = str_detect(disp_desc, "BURG") & str_detect(disp_desc, "1|FIRST") & !str_detect(disp_desc, "TOOL")) %>%
  filter(burglary == TRUE)

burg_dispsum1st19 <- burg1stdisp19 %>%
  mutate(file_month = floor_date(ymd(disp_date), 'month')) %>%
  count(file_month)

burg_dispsum1st20 <- burg1stdisp20 %>%
  mutate(file_month = floor_date(ymd(disp_date), 'month')) %>%
  count(file_month)

ggplot(burg_dispsum1st19, aes(file_month, n, color = 'court')) +
  geom_line() +
  theme_ojo() +
  ylim(0, NA) +
  ggtitle("Number of 1st Burglary Disps in OSCN Counties 2019") +
  scale_color_manual(values = ojo_pal)

ggplot(burg_dispsum1st20, aes(file_month, n, color = 'court')) +
  geom_line() +
  theme_ojo() +
  ylim(0, NA) +
  ggtitle("Number of 1st Burglary Disps in OSCN Counties 2020") +
  scale_color_manual(values = ojo_pal)

#Just second degree

burg2nddisp19 <- dispsall19 %>%
  mutate(burglary = str_detect(disp_desc, "BURG") & str_detect(disp_desc, "2|SECOND") & !str_detect(disp_desc, "TOOL")) %>%
  filter(burglary == TRUE)

burg2nddisp20 <- dispsall20 %>%
  mutate(burglary = str_detect(disp_desc, "BURG") & str_detect(disp_desc, "2|SECOND") & !str_detect(disp_desc, "TOOL")) %>%
  filter(burglary == TRUE)

burg_dispsum2nd20 <- burg2nddisp20 %>%
  mutate(file_month = floor_date(ymd(disp_date), 'month')) %>%
  count(file_month)

burg_sum2nddisp19 <- burg2nddisp19 %>%
  mutate(file_month = floor_date(ymd(disp_date), 'month')) %>%
  count(file_month)

ggplot(burg_dispsum2nd20, aes(file_month, n, color = 'court')) +
  geom_line() +
  theme_ojo() +
  ylim(0, NA) +
  ggtitle("Number of 2nd Burglary Disps in OSCN Counties 2020") +
  scale_color_manual(values = ojo_pal)

ggplot(burg_sum2nddisp19, aes(file_month, n, color = 'court')) +
  geom_line() +
  theme_ojo() +
  ylim(0, NA) +
  ggtitle("Number of 2nd Burglary Disps in OSCN Counties 2019") +
  scale_color_manual(values = ojo_pal)


#Just 3rd Degree

burg3rddisp19 <- dispsall19 %>%
  mutate(burglary = str_detect(disp_desc, "BURG") & str_detect(disp_desc, "3|THIRD") & !str_detect(disp_desc, "TOOL")) %>%
  filter(burglary == TRUE)

burg3rddisp20 <- dispsall20 %>%
  mutate(burglary = str_detect(disp_desc, "BURG") & str_detect(disp_desc, "3|THIRD") & !str_detect(disp_desc, "TOOL")) %>%
  filter(burglary == TRUE)

burg_dispsum3rd19 <- burg3rddisp19 %>%
  mutate(file_month = floor_date(ymd(disp_date), 'month')) %>%
  count(file_month)

burg_dispsum3rd20 <- burg3rddisp20 %>%
  mutate(file_month = floor_date(ymd(disp_date), 'month')) %>%
  count(file_month)

ggplot(burg_dispsum3rd19, aes(file_month, n, color = 'court')) +
  geom_line() +
  theme_ojo() +
  ylim(0, NA) +
  ggtitle("Number of 3rd Degree Burglary Disps in OSCN Counties 2019") +
  scale_color_manual(values = ojo_pal)

ggplot(burg_dispsum3rd20, aes(file_month, n, color = 'court')) +
  geom_line() +
  theme_ojo() +
  ylim(0, NA) +
  ggtitle("Number of 3rd Degree Burg  Disps Filed in OSCN Counties 2020") +
  scale_color_manual(values = ojo_pal)

#Add PWI disps next

#LATER-- Comparing top with bottom income counties
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

ggplot(burg_sum2nd19, aes(file_month, n, color = 'court')) +
  geom_line() +
  theme_ojo() +
  xlab("Month Filed") +
  ylim(0, NA) +
  ggtitle("Number of 2nd Burglary Cases Filed in Tulsa County") +
  scale_color_manual(values = ojo_pal)


