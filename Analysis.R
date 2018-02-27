library(dplyr)
library(ggplot2)

Loan = read.csv("kiva_loans.csv", stringsAsFactors = F)
location = read.csv("kiva_mpi_region_locations.csv", stringsAsFactors = F)
theme = read.csv("loan_theme_ids.csv", stringsAsFactors = F)
theme_region = read.csv("loan_themes_by_region.csv", stringsAsFactors = F)

summary(Loan)

summary_by_country = Loan %>%
  group_by(country)%>%
  summarise(no_loan = n(), LoanAmount = sum(loan_amount))

summary_by_country = summary_by_country%>%
  arrange(no_loan)

