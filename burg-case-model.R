library(ojodb)
library(plotly)


connect_ojo()

### Query OJO database for all felony cases filed 2018-01-01 to 2020-02-29
casesall <- ojo_tbl('ojo_crim_cases') %>%
  filter(court %in% c("ADAIR", "CANADIAN", "CLEVELAND", "COMANCHE", "ELLIS", "GARFIELD", "LOGAN", "OKLAHOMA", "PAYNE", "PUSHMATAHA","ROGERMILLS", "ROGERS", "TULSA"),
         file_year >= 2018, file_date < "2020-03-01", casetype == "CF") %>%
  collect()

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

### Estimate = -26, p = 0.0171; significant at p > 0.05
print(linearMod)
summary(linearMod)

### Same thing for 1st degree burglary. Since HB 2286 did not change that offense, we expect no change in charge patterns after implementation
burg1_sum <- burg_sum %>%
  filter(burg_cat == "FIRST DEGREE")

### Estimate = .15, p = 0.960; not significant
linearMod1 <- lm(n ~ burg_reform + moy,
                 data = burg1_sum)

print(linearMod1)
summary(linearMod1)
