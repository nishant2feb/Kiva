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
library(wordcloud)

# Creating clean corpus
activity_text = VectorSource(Loan$activity)

activity_text = VCorpus(activity_text)

tm_map(activity_text, removeNumbers)
tm_map(activity_text, removePunctuation)

activity_text_tdm = TermDocumentMatrix(activity_text)

activity_text_matrix = as.matrix(activity_text_tdm)

freq = rowSums(activity_text_matrix)

#Word fequency for plotting data

word_freq = data.frame(term = names(freq), num = freq)

wordcloud(word_freq$term, word_freq$num, colors = 'red')

#word network
word_associate(Loan$activity, match.string = c("farming", "sales", "food", "expenses", "personal", "store"), 
               network.plot = T, cloud.colors = c("gray85", "red"))

