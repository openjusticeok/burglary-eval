library(ojodb)
library(plotly)

connect_ojo()

s <- ojo_tbl("doc_sentences") %>%
  filter(js_date > "2018-01-01", js_date < "2020-03-01", !is.na(doc_incarcerated_term_yrs)) %>%
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

### Export data to CSV
docburg %>%
  write_csv("DOC burglary sentences.csv")

docburg_sum <- docburg %>%
  count(file_month, burg_cat)


docburg_sum <- docburg_sum %>%
  mutate(burg_reform = if_else(file_month >= ymd("2018-11-01"),
                               1,
                               0),
         moy = month(file_month, label = TRUE) %>%
           as.character) %>%
  filter(year(file_month) >= 2018)

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

### Estimate = -6.7, p = 0.0108; significant at p < 0.05
burg2_doc <- docburg_sum %>%
  filter(burg_cat == "SECOND DEGREE")

linearModdoc <- lm(n ~ burg_reform + moy,
                   data = burg2_doc)

print(linearModdoc)
summary(linearModdoc)
