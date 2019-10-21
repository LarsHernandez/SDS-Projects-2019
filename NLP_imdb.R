
library(tidyverse)
library(tidytext)
library(topicmodels)
library(glmnet)

install.packages("topicmodels", type = "binary")


train <- read_delim("C:/Users/larsn/Desktop/train.tsv", "\t", escape_double = FALSE, trim_ws = TRUE)
test  <- read_delim("C:/Users/larsn/Desktop/test.tsv", "\t", escape_double = FALSE, trim_ws = TRUE)

data(stop_words)

train <- train %>% slice(1:1000)


tidy_train <- train %>% 
  mutate(rev = 1:nrow(.)) %>% 
  unnest_tokens(word, review) %>% 
  anti_join(stop_words, by = c("word" = "word")) %>% 
  filter(word != "br",
         word != "film",
         word != "movie")


# LDA ---------------------------------------------------------------------

dtm <- tidy_train %>% 
  count(word, is_positive) %>% 
  cast_dtm(is_positive, word, n)

lda_out <- LDA(dtm, k = 3)
lda_out %>% 
  tidy(matrix = "beta") %>% 
  arrange(desc(beta))%>% 
  group_by(topic) %>% 
  top_n(15) %>% 
  ungroup() %>%
  mutate(term2 = fct_reorder(term,beta)) %>% 
  ggplot(aes(term2, beta, fill=as.factor(topic))) + 
  geom_col() + 
  facet_wrap(~as.factor(topic), scales="free_y") + 
  coord_flip()



# Model -------------------------------------------------------------------

sparse_words <- tidy_train %>%
  count(rev, word) %>%
  cast_sparse(rev, word, n)

class(sparse_words)
dim(sparse_words)



word_rownames <- as.integer(rownames(sparse_words))

books_joined <- data_frame(is_positive = word_rownames) %>%
  left_join(train %>% select(is_positive))

is_jane <- books_joined$title == "Pride and Prejudice"

model <- cv.glmnet(x = sparse_words, y = tidy_train$is_positive,
                   family = "binomial", keep = TRUE)


