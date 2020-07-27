library(ojodb)
library(plotly)

connect_ojo()

### Query OJO database for all felony cases filed 2018-01-01 to 2020-02-29
casesall <- ojo_tbl('ojo_crim_cases') %>%
  filter(court %in% c("ADAIR", "CANADIAN", "CLEVELAND", "COMANCHE", "ELLIS", "GARFIELD", "LOGAN", "OKLAHOMA", "PAYNE", "PUSHMATAHA","ROGERMILLS", "ROGERS", "TULSA"),
         file_year >= 2018, file_date < "2020-03-01", casetype == "CF") %>%
  collect()

disconnect_ojo()

### Identify cases with a top count of burglary, exclude top counts of possession of burglary tools
burgall <- casesall %>%
  mutate(burglary = str_detect(top_ct_desc, "BURGLARY") & !str_detect(top_ct_desc, "TOOL")) %>%
  filter(burglary == TRUE)

### Categorize burglary charge as first, second, or third degree; add month of filing
burgall <- burgall %>%
  mutate(burg_cat = case_when(str_detect(top_ct_desc, "FIRST|1") |
                                str_detect(top_ct_stat, "1431") ~ "FIRST DEGREE",
                              str_detect(top_ct_desc, "SECOND|2|SEOND") |
                                str_detect(top_ct_stat, "1435$") ~ "SECOND DEGREE",
                              str_detect(top_ct_desc, "THIRD|3|THRID") ~ "THIRD DEGREE"),
         file_month = floor_date(ymd(file_date), 'month'))

### Export data set to CSV for GitHub repository
burgall %>%
  select(court, casenum, file_date, top_ct_desc, top_ct_statute = top_ct_stat,
         n_ct, ojo_burg_cat = burg_cat) %>%
  write_csv("OSCN burglary cases.csv")

### Count the number of cases filed each month in each category of burglary
burg_sum <- burgall %>%
  count(file_month, burg_cat)

ggplot(burg_sum, aes(file_month, n, group = burg_cat, color = burg_cat)) +
  geom_line() +
  geom_vline(aes(xintercept = ymd("2018-11-01"))) +
  theme_ojo() +
  xlim(ymd("2018-01-01"), NA) +
  labs(x = "Month and Year Filed", y = "Number of Cases") +
  labs(color = "Burglary Category") +
  ylim(0, NA) +
  ggtitle("Number of Burglary Cases Filed in OSCN Counties") +
  scale_color_manual(values = ojo_pal)

# #### Took out theme_ojo() + so I could get rid of grid lines/make plot more usable
# #### Check on font to see if I can add OJO font/font size while keeping it like this
#
# casesall %>%
#   mutate(file_month = floor_date(ymd(file_date), 'month')) %>%
#   count(file_month) %>%
#   filter(file_month > ymd("2017-01-01")) %>%
#   ggplot(aes(file_month, n)) +
#   geom_line() +
#   labs(x = "Month and Year Filed", y = "Number of Cases") +
#   theme_ojo() +
#   ylim(0, NA) +
#   ggtitle("Number of Felony Cases Filed in OSCN Counties") +
#   scale_color_manual(values = ojo_pal)

### Check NAs - excluding 7 cases where degree was not evident
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

### Linear model for impact of HB 2286 (burg_reform) and month in year (moy, for seasonal effects) on the number of cases filed
burg2_sum <- burg_sum %>%
  filter(burg_cat == "SECOND DEGREE")

linearMod <- lm(n ~ burg_reform + moy,
                data = burg2_sum)

### Estimate = -26, r^2 = 0.0171; significant at r = 0.05
print(linearMod)
summary(linearMod)

### Same thing for 1st degree burglary. Since HB 2286 did not change that offense, we expect no change in charge patterns after implementation
burg1_sum <- burg_sum %>%
  filter(burg_cat == "FIRST DEGREE")

### Estimate = .15, r^2 = 0.960; not significant
linearMod1 <- lm(n ~ burg_reform + moy,
                 data = burg1_sum)

print(linearMod1)
summary(linearMod1)

##DISPOSITIONS

disps1720 <- ojo_tbl('oscn_crim_disps') %>%
  filter(court %in% c("ADAIR", "CANADIAN", "CLEVELAND", "COMANCHE", "ELLIS", "GARFIELD", "LOGAN", "OKLAHOMA", "PAYNE", "PUSHMATAHA","ROGERMILLS", "ROGERS", "TULSA"), file_year >= 2017, disp_date < "2020-03-01", casetype == "CF") %>%
  collect()

dispsall1720 <- disps1720 %>%
  mutate(burglary = str_detect(ct_desc, "BURGLARY") & !str_detect(ct_desc, "TOOL")) %>%
  filter(burglary == TRUE)

dispsall1720 <- dispsall1720 %>%
  mutate(burg_cat = case_when(str_detect(disp_desc, "FIRST|1") |
                                str_detect(disp_stat, "1431") ~ "FIRST DEGREE",
                              str_detect(disp_desc, "SECOND|2|SEOND") |
                                str_detect(disp_stat, "1435$") ~ "SECOND DEGREE",
                              str_detect(disp_desc, "THIRD|3|THRID") ~ "THIRD DEGREE"),
         file_month = floor_date(ymd(disp_date), 'month'))

dispsall1720 %>%
  filter(is.na(burg_cat)) %>%
  view


disps_sum1720 <- dispsall1720 %>%
  count(file_month, burg_cat) # Counting two variables looks for all unique combinations - look at burg_sum to see what it does


disps_sum1720 <- disps_sum1720 %>%
  mutate(burg_reform = if_else(file_month >= ymd("2018-11-01"),
                               1,
                               0),
         moy = month(file_month, label = TRUE) %>%
           as.character) %>%
  filter(year(file_month) >= 2017)

ggplot(disps_sum1720, aes(file_month, n, group = burg_cat, color = burg_cat)) +
  geom_line() +
  geom_vline(aes(xintercept = ymd("2018-11-01"))) +
  theme_ojo() +
  ylim(0, NA) +
  ggtitle("Number of Burglary Dispositions in OSCN Counties in 2017-20") +
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

#doc sentences code

s <- ojo_tbl("doc_sentences") %>%
  filter(js_date > "2010-01-01", js_date < "2020-03-01", !is.na(doc_incarcerated_term_yrs)) %>%
  select(doc_num, statute_code, js_date, doc_incarcerated_term_yrs) %>%
  left_join(ojo_tbl("doc_offense")) %>%
  collect()


docburg <- s %>%
  mutate(burglary = str_detect(statute_desc, "BURGLARY") & !str_detect(statute_desc, "TOOL")) %>%
  filter(burglary == TRUE) %>%
  view()

docburg <- docburg %>%
  mutate(burg_cat = case_when(str_detect(statute_desc, "FIRST|1") |
                                str_detect(statute_code, "1431") ~ "FIRST DEGREE",
                              str_detect(statute_desc, "SECOND|2|SEOND") |
                                str_detect(statute_code, "1435$") ~ "SECOND DEGREE",
                              str_detect(statute_desc, "THIRD|3|THRID") ~ "THIRD DEGREE"),
         file_month = floor_date(ymd(js_date), 'month'))

docburg_sum <- docburg %>%
  count(file_month, burg_cat)


docburg_sum <- docburg_sum %>%
  mutate(burg_reform = if_else(file_month >= ymd("2018-11-01"),
                               1,
                               0),
         moy = month(file_month, label = TRUE) %>%
           as.character) %>%
  filter(year(file_month) >= 2010)


ggplot(docburg_sum, aes(file_month, n, group = burg_cat, color = burg_cat)) +
  geom_line() +
  geom_vline(aes(xintercept = ymd("2018-11-01"))) +
  theme_ojo() +
  xlim(ymd("2010-01-01"), NA) +
  labs(x = "Month and Year Filed", y = "Number of Cases") +
  labs(color = "Burglary Category") +
  ylim(0, NA) +
  ggtitle("Number of Burglary Sentences Files in OSCN Counties 2010-2019") +
  scale_color_manual(values = ojo_pal)


burg2_doc <- docburg_sum %>%
  filter(burg_cat == "SECOND DEGREE")

linearModdoc <- lm(n ~ burg_reform + moy,
                 data = burg2_doc)

print(linearModdoc)
summary(linearModdoc)

