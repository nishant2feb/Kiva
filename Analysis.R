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

summary(summary_by_country)

summary_by_country%>%
  filter(no_loan <= 363)%>%
ggplot(aes(x = no_loan, y = LoanAmount, col = country))+
  geom_point()+
  theme_classic()

summary_by_country%>%
  filter(no_loan <= 2313)%>%
  filter(no_loan > 363)%>%
  ggplot(aes(x = no_loan, y = LoanAmount, col = country))+
  geom_point()+
  theme_classic()

summary_by_country%>%
  filter(no_loan <= 6687)%>%
  filter(no_loan > 2313)%>%
  ggplot(aes(x = no_loan, y = LoanAmount, col = country))+
  geom_point()+
  theme_classic()

summary_by_country%>%
  filter(no_loan > 6687)%>%
  ggplot(aes(x = no_loan, y = LoanAmount, col = country))+
  geom_point()+
  theme_classic()

## Analysing data of text like sector activity and use
library(qdap)
library(tm)

activity_text = (Loan$activity)
tm_map(activity_text, removeNumbers)
tm_map(activity_text, removePunctuation)

activity_wfm = wfm(Loan$activity)



