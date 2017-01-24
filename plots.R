library(ggplot2)
library(dplyr)
library(tidyr)
library(GGally)

df.class <- read.csv('output/classifier_features.cvs')
df.nn <- read.csv('output/nn_probs.csv')
df.lda <- read.csv('output/lda_features.csv', col.names = c('topic', 'feature',
                                                            'class', 'coef'))

df.class %>%
  spread(class, coef) -> plot.dat

plot.dat %>% select(-feature) %>% ggpairs() + theme_minimal()


df.nn %>%
  group_by(class) %>%
  mutate(row_feat = paste(feature, row_number(), sep='_')) %>%
  ungroup() %>%
  select(class, prob, row_feat) %>% 
  spread(class, prob) -> plot.dat
plot.dat %>% select(-row_feat) %>% ggpairs() + theme_minimal()


df.nn %>%
  left_join(df.class) %>% 
  group_by(class) %>%
  mutate(Corr = cor(prob, coef, use = 'complete')) -> plot.dat

text.dat <- plot.dat %>% select(class, Corr) %>% unique()

ggplot(plot.dat, aes(x=coef, y=prob)) +
  geom_point() + 
  facet_wrap(~class) + 
  geom_text(data=text.dat, aes(x=-4.5, y=.5, label=paste('Corr:', round(Corr,3))))

df.nn %>%
  group_by(feature, class) %>%
  summarise(m_prob = mean(prob)) %>%
  left_join(df.class) %>%
  group_by(class) %>%
  mutate(Corr = cor(m_prob, coef, use='complete')) -> plot.dat

text.dat <- plot.dat %>% select(class, Corr) %>% unique()

ggplot(plot.dat, aes(x=coef, y=m_prob)) +
  geom_point() + 
  facet_wrap(~class) + 
  geom_text(data=text.dat, aes(x=-4.5, y=.5, label=paste('Corr:', round(Corr,3))))

df.nn %>% 
  group_by(class, feature) %>%
  summarise(m_prob = mean(prob)) %>%
  right_join(df.lda) %>% 
  group_by(class) %>%
  mutate(Corr=cor(m_prob, coef, use='complete')) -> plot.dat

text.dat <- plot.dat %>% select(class, Corr) %>% unique()

ggplot(plot.dat, aes(x=coef, y=m_prob)) +
  geom_point() + 
  facet_wrap(~class) + 
  geom_text(data=text.dat, aes(x=-2.5, y=.6, label=paste('Corr:', round(Corr,3))))



#look at some essays
df.nn[which(df.nn$prob==max(df.nn$prob)),]

df.nn %>%
  group_by(essay_num, class) %>%
  mutate(word_num = row_number(),
         prob_sum = cumsum(prob)) %>%
  ungroup() %>%
  filter(essay_num==102) -> plot.dat

text.dat <- plot.dat %>% select(word_num, feature)

ggplot(plot.dat) + 
  geom_line(aes(x=word_num, y=prob_sum, group=class, color=class)) +
  geom_text(data=text.dat, aes(x=word_num, y=-.7, label=feature), 
            size=3, color='black', angle=15)

df.nn %>%
  group_by(essay_num, class) %>%
  mutate(word_num = row_number(),
         prob_sum = cumsum(prob)) %>%
  ungroup() %>%
  filter(essay_num==123) -> plot.dat

text.dat <- plot.dat %>% select(word_num, feature)

ggplot(plot.dat) + 
  geom_line(aes(x=word_num, y=prob_sum, group=class, color=class)) +
  geom_text(data=text.dat, aes(x=word_num, y=-.7, label=feature), 
            size=2.5, color='black', angle=20)
  
df.nn %>%
  group_by(essay_num, class) %>%
  mutate(word_num = row_number(),
         prob_sum = cumsum(prob)) %>%
  ungroup() %>%
  filter(essay_num==144) -> plot.dat

text.dat <- plot.dat %>% select(word_num, feature)

ggplot(plot.dat) + 
  geom_line(aes(x=word_num, y=prob_sum, group=class, color=class)) +
  geom_text(data=text.dat, aes(x=word_num, y=-.7, label=feature), 
            size=2.5, color='black', angle=20)

df.nn %>%
  group_by(essay_num, class) %>%
  mutate(word_num = row_number(),
         prob_sum = cumsum(prob)) %>%
  ungroup() %>%
  filter(essay_num==53) -> plot.dat

text.dat <- plot.dat %>% select(word_num, feature)

ggplot(plot.dat) + 
  geom_line(aes(x=word_num, y=prob_sum, group=class, color=class)) +
  geom_text(data=text.dat, aes(x=word_num, y=-.7, label=feature), 
            size=2.5, color='black', angle=20)

df.nn %>%
  group_by(essay_num, class) %>%
  mutate(word_num = row_number(),
         prob_sum = cumsum(prob)) %>%
  ungroup() %>%
  filter(essay_num==327) -> plot.dat

text.dat <- plot.dat %>% select(word_num, feature)

ggplot(plot.dat) + 
  geom_line(aes(x=word_num, y=prob_sum, group=class, color=class)) +
  geom_text(data=text.dat, aes(x=word_num, y=-.7, label=feature), 
            size=2.5, color='black', angle=20)

  