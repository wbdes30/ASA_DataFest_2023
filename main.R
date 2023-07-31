library(tidyverse)
library(tidyr)
library(janitor)
library(ggplot2)
library(ggrepel)
library(textdata)
library(wordcloud)
library(reshape2)
library(RColorBrewer)
library(janeaustenr)
library(dplyr)
library(tidytext)

categories %>% group_by("StateAbbr")

subcategories %>% group_by("StateAbbr")

questions_plot <- tabyl(questions, StateAbbr, Category)
questions_plot

question_plot <- questions_plot %>% pivot_longer(cols = c('Consumer Financial Questions', 'Education', 'Family and Children', 'Health and Disability', 'Housing and Homelessness', 'Income Maintenance', 'Individual Rights', 'Juvenile','Work, Employment and Unemployment', 'Other'), names_to = "Category", values_to = "Number of Questions")

highlight_df <- question_plot %>% group_by(Category) %>%
  filter(`Number of Questions` >= (max(`Number of Questions`)/2.5))

category_state_question <- question_plot %>%
  ggplot(aes(x = StateAbbr, y = `Number of Questions`)) +
  geom_point(alpha = 0.3, color = "purple") +
  geom_point(data=highlight_df,
             aes(x=StateAbbr,y=`Number of Questions`),
             alpha = 0.3,
             color='red') +
  geom_label_repel(data = highlight_df,
                   aes(x=StateAbbr,y=`Number of Questions`),
                   label = highlight_df$StateAbbr) +
  theme(axis.text.x = element_blank(), axis.ticks = element_blank()) +
  ggtitle("States' Number of Questions by Categories")+
  xlab("State Abbreviation")+
  theme(plot.title=element_text(size=20),
        axis.title=element_text(size=18)) +
  facet_wrap(~Category, scales = "free")

#############################

questionposts$PostText <- questionposts$PostText %>%
  str_trim() %>%
  str_squish()


tidy_questionposts <- tibble(text = questionposts$PostText) %>% unnest_tokens(word, text)
cleaned_questionposts <- tidy_questionposts %>%
  anti_join(get_stopwords())
stopwords_questionposts <- cleaned_questionposts %>% count(word, sort = TRUE)

positive <- get_sentiments("bing") %>%
  # for general purposes, tidytext
  # also comes with AFINN & nrc
  filter(sentiment == "positive")

tidy_questionposts %>%
  semi_join(positive) %>%
  count(word, sort = TRUE)

bing <- get_sentiments("bing")

wordcloud_tidy <- tidy_questionposts %>%
  inner_join(bing, by = join_by(word), keep = FALSE) %>%
  count(word, sentiment, sort = TRUE) %>%
  filter(n > 100, sentiment == "negative") %>%
  with(wordcloud(word, n,
            max.words=500,
            colorPalette=brewer.pal(8, "Dark2")))

###########################################

jointclientsstate <- clients %>% left_join(statesites,
                                           by=join_by(StateAbbr),
                                           keep = TRUE)

