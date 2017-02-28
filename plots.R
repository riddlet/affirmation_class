library(ggplot2)
library(dplyr)
library(tidyr)
library(GGally)
library(forcats)
library(magrittr)
library(rstanarm)
library(bayesplot)
options(mc.cores = parallel::detectCores())

df.class <- read.csv('output/classifier_features.cvs')
df.nn <- read.csv('output/nn_probs.csv')
df.lda <- read.csv('output/lda_features.csv', col.names = c('topic', 'feature',
                                                            'class', 'coef'))

df.class %>%
  spread(class, coef) -> plot.dat

############# Model summaries ##########


df.nn %>%
  group_by(feature, class) %>%
  summarise(m_prob = mean(prob)) %>%
  left_join(df.class) %>%
  group_by(class) %>%
  mutate(Corr = cor(m_prob, coef, use='complete')) %>%
  ungroup() -> plot.dat

plot.dat %<>% 
  mutate(class = fct_recode(class,
                            'Treatment-Female' = 'aff_f',
                            'Treatment-Male' = 'aff_m',
                            'Control-Female' = 'control_f',
                            'Control-Male' = 'control_m'))

text.dat <- plot.dat %>% select(class, Corr) %>% unique()


jpeg('output/figures/nn_svm_corr_avg.jpeg', width=1000, height=700)
ggplot(plot.dat, aes(x=coef, y=m_prob)) +
  geom_point() + 
  facet_wrap(~class) + 
  geom_text(data=text.dat, aes(x=-2.5, y=.5, label=paste('Corr:', round(Corr,2))),
            size=8) +
  #  ggtitle('Neural Net-SVM Correlations', 'Probabilities averaged over features') +
  theme_minimal() +
  ylab('Probability Change') +
  xlab('SVM Coefficient') + 
  theme(strip.text = element_text(size=20, face='bold'),
        axis.title = element_text(size=20, face='bold'),
        axis.text = element_text(size=18))
dev.off()

###### example essay #######
df.flute <- read.csv('output/flute_probs.csv')

df.flute %>%
  group_by(class) %>%
  mutate(word_num = row_number(),
         prob_sum = cumsum(prob)) %>%
  ungroup() -> plot.dat

plot.dat %<>% 
  mutate(class = fct_recode(class,
                            'Treatment-Female' = 'aff_f',
                            'Treatment-Male' = 'aff_m',
                            'Control-Female' = 'control_f',
                            'Control-Male' = 'control_m'))

text.dat <- plot.dat %>% select(word_num, word)

jpeg('output/figures/model_preds_example.jpeg', width=1000, height=700)
ggplot(plot.dat) + 
  geom_line(aes(x=word_num, y=prob_sum, group=class, color=class, linetype=class), size=1.25) +
  geom_text(data=text.dat, aes(x=word_num, y=-.7, label=word), 
            size=8, color='black', angle=30) +
  theme_minimal() + 
  ylab('Class Probability') + 
  theme(legend.position='top',
        legend.text = element_text(size=20),
        legend.title = element_blank(),
        axis.text.x=element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=18),
        axis.title.y = element_text(size=20, face='bold'))
dev.off()

###### Justifications (others) ###########

df.just <- read.csv('output/nnprobs_just.csv')

df.just %>%
  group_by(sentence_num, class, val, just) %>%
  mutate(word_num = row_number(),
         prob_sum = cumsum(prob_shift),
         keyword = max(word_num)) %>%
  ungroup() %>%
  filter(word_num > keyword-5) %>%
  mutate(time = abs((keyword-5)-word_num)) -> plot.dat

plot.dat %<>% 
  mutate(class = fct_recode(class,
                            'Treatment-Female' = 'aff_f',
                            'Treatment-Male' = 'aff_m',
                            'Control-Female' = 'control_f',
                            'Control-Male' = 'control_m'),
         time = factor(time, labels = c('j-4', 'j-3', 'j-2', 'j-1', 'j')))


jpeg('output/figures/justification_others.jpeg', width=1000, height=750)
ggplot(plot.dat, aes(x=time, y=prob)) + 
  geom_point(position=position_jitter(width=.15)) +
  theme_minimal() + 
  ylab('Probability') +
  xlab('Word position of sentence fragment from justification sentences') +
  theme(axis.title = element_text(size=20, face='bold'),
        axis.text = element_text(size=18),
        strip.text = element_text(size=20, face='bold'))+
  geom_line(aes(x=time, y=prob, group=sentence_num), alpha=.2)+
  facet_wrap(~class)
dev.off()

df.just %>%
  group_by(sentence_num, class, val, just) %>%
  mutate(word_num = row_number(),
         keyword = max(word_num)) %>%
  ungroup() %>%
  filter(word_num > keyword-2) %>%
  mutate(time = abs((keyword-2)-word_num)) %>%
  group_by(sentence_num, time, val) %>%
  mutate(g_m = exp(mean(log(prob)))) %>%
  ungroup() %>%
  mutate(clr = log(prob/g_m)) -> mod.dat

mod.dat %<>% 
  mutate(class = fct_recode(class,
                            'Treatment-Female' = 'aff_f',
                            'Treatment-Male' = 'aff_m',
                            'Control-Female' = 'control_f',
                            'Control-Male' = 'control_m'),
         time = factor(time, labels = c('s', 'j')))

m1 <- stan_glmer(clr ~ class*time + (time|sentence_num) + (time|val) + 
                   (time|just) + (time|sentence_num:val:just), 
                 data=mod.dat, prior=normal(0,1))

posterior <- as.data.frame(m1$stanfit, pars=names(m1$coefficients[1:8]))

posterior %<>%
  mutate(fem_just = `(Intercept)` + timej,
         male_just = `(Intercept)` + timej + `classTreatment-Male` + `classTreatment-Male:timej`,
         fem_prejust = `(Intercept)`,
         male_prejust = `(Intercept)` +`classTreatment-Male`) %>%
  mutate(fem_diff = fem_just - fem_prejust,
         male_diff = male_just - male_prejust) %>%
  mutate(fem_pref = fem_diff - male_diff)

mean(posterior$fem_pref)

est <- mcmc_areas(posterior, pars='fem_pref', prob=.95)
est$data$l
est$data$h
prop.table(table(posterior$fem_pref>0))

#what does it mean?
df.just %>%
  group_by(sentence_num, class, val, just) %>%
  mutate(word_num = row_number(),
         keyword = max(word_num)) %>%
  filter(word_num > keyword-2) %>%
  mutate(time = abs((keyword-2)-word_num)) %>% 
  mutate(time = factor(time, labels=c('first', 'second'))) %>%
  ungroup() %>%
  select(class, just, val, time, prob) %>%
  group_by(class, time) %>%
  summarise(mean(prob),
            sd(prob))


###### Justifications (self) ###########
df.just <- read.csv('output/nnprobs_justself.csv')

df.just %>%
  group_by(sentence_num, class, val, just) %>%
  mutate(word_num = row_number(),
         prob_sum = cumsum(prob_shift),
         keyword = max(word_num)) %>%
  ungroup() %>%
  filter(word_num > keyword-5) %>%
  mutate(time = abs((keyword-5)-word_num)) -> plot.dat

plot.dat %<>% 
  mutate(class = fct_recode(class,
                            'Treatment-Female' = 'aff_f',
                            'Treatment-Male' = 'aff_m',
                            'Control-Female' = 'control_f',
                            'Control-Male' = 'control_m'),
         time = factor(time, labels = c('j-4', 'j-3', 'j-2', 'j-1', 'j')))


jpeg('output/figures/justification_self.jpeg', width=1000, height=750)
ggplot(plot.dat, aes(x=time, y=prob)) + 
  geom_point(position=position_jitter(width=.15)) +
  theme_minimal() + 
  ylab('Probability') +
  xlab('Word position of sentence fragment from justification sentences') +
  theme(axis.title = element_text(size=20, face='bold'),
        axis.text = element_text(size=18),
        strip.text = element_text(size=20, face='bold'))+
  geom_line(aes(x=time, y=prob, group=sentence_num), alpha=.25)+
  facet_wrap(~class)
dev.off()

df.just %>%
  group_by(sentence_num, class, val, just) %>%
  mutate(word_num = row_number(),
         keyword = max(word_num)) %>%
  ungroup() %>%
  filter(word_num > keyword-2) %>%
  mutate(time = abs((keyword-2)-word_num)) %>%
  group_by(sentence_num, time, val) %>%
  mutate(g_m = exp(mean(log(prob)))) %>%
  ungroup() %>%
  mutate(clr = log(prob/g_m)) -> mod.dat

mod.dat %<>% 
  mutate(class = fct_recode(class,
                            'Treatment-Female' = 'aff_f',
                            'Treatment-Male' = 'aff_m',
                            'Control-Female' = 'control_f',
                            'Control-Male' = 'control_m'),
         time = factor(time, labels = c('s', 'j')))

m2 <- stan_glmer(clr ~ class*time + (time|sentence_num) + (time|val) + 
                   (time|just) + (time|sentence_num:val:just), 
                 data=mod.dat, prior=normal(0,1))

posterior <- as.data.frame(m2$stanfit, pars=names(m2$coefficients[1:8]))

posterior %<>%
  mutate(fem_just = `(Intercept)` + timej,
         male_just = `(Intercept)` + timej + `classTreatment-Male` + `classTreatment-Male:timej`,
         fem_prejust = `(Intercept)`,
         male_prejust = `(Intercept)` +`classTreatment-Male`) %>%
  mutate(fem_diff = fem_just - fem_prejust,
         male_diff = male_just - male_prejust) %>%
  mutate(fem_pref = fem_diff - male_diff)

mean(posterior$fem_pref)

est <- mcmc_areas(posterior, pars='fem_pref', prob=.95)
est$data$l
est$data$h
prop.table(table(posterior$fem_pref>0))

#what does it mean?
df.just %>%
  group_by(sentence_num, class, val, just) %>%
  mutate(word_num = row_number(),
         keyword = max(word_num)) %>%
  filter(word_num > keyword-2) %>%
  mutate(time = abs((keyword-2)-word_num)) %>% 
  mutate(time = factor(time, labels=c('first', 'second'))) %>%
  ungroup() %>%
  select(class, just, val, time, prob) %>%
  group_by(class, time) %>%
  summarise(mean(prob),
            sd(prob))

######appendix code


jpeg('output/figures/SVM_coef_corr.jpeg', width=1000, height=700)
plot.dat %>% select(-feature) %>% 
  ggpairs(title='SVM coefficients') + 
  theme_minimal() + 
  theme(title=element_text(size=14))
dev.off()

df.nn %>%
  group_by(class) %>%
  mutate(row_feat = paste(feature, row_number(), sep='_')) %>%
  ungroup() %>%
  select(class, prob, row_feat) %>% 
  spread(class, prob) -> plot.dat

jpeg('output/figures/NN_prob_corr.jpeg', width=1000, height=700)
plot.dat %>% select(-row_feat) %>%
  ggpairs(title = 'Neural Net Correlations') + 
  theme_minimal() + 
  theme(title=element_text(size=14))
dev.off()

###### Justifications (might) ###########

df.just <- read.csv('output/nnprobs_might.csv')

df.just %>%
  group_by(sentence_num, class, val, just) %>%
  mutate(word_num = row_number(),
         prob_sum = cumsum(prob_shift),
         keyword = max(word_num)) %>%
  ungroup() %>%
  filter(word_num > keyword-5) %>%
  mutate(time = abs((keyword-5)-word_num)) -> plot.dat

plot.dat %<>% 
  mutate(class = fct_recode(class,
                            'Treatment-Female' = 'aff_f',
                            'Treatment-Male' = 'aff_m',
                            'Control-Female' = 'control_f',
                            'Control-Male' = 'control_m'),
         time = factor(time, labels = c('j-4', 'j-3', 'j-2', 'j-1', 'j')))


jpeg('output/figures/justification_othersmight.jpeg', width=1000, height=750)
ggplot(plot.dat, aes(x=time, y=prob)) + 
  geom_point(position=position_jitter(width=.15)) +
  theme_minimal() + 
  ylab('Probability') +
  xlab('Word position of sentence fragment from justification sentences') +
  theme(axis.title = element_text(size=20, face='bold'),
        axis.text = element_text(size=18),
        strip.text = element_text(size=20, face='bold'))+
  geom_line(aes(x=time, y=prob, group=sentence_num), alpha=.2)+
  facet_wrap(~class)
dev.off()

df.just %>%
  group_by(sentence_num, class, val, just) %>%
  mutate(word_num = row_number(),
         keyword = max(word_num)) %>%
  ungroup() %>%
  filter(word_num > keyword-2) %>%
  mutate(time = abs((keyword-2)-word_num)) %>%
  group_by(sentence_num, time, val) %>%
  mutate(g_m = exp(mean(log(prob)))) %>%
  ungroup() %>%
  mutate(clr = log(prob/g_m)) -> mod.dat

mod.dat %<>% 
  mutate(class = fct_recode(class,
                            'Treatment-Female' = 'aff_f',
                            'Treatment-Male' = 'aff_m',
                            'Control-Female' = 'control_f',
                            'Control-Male' = 'control_m'),
         time = factor(time, labels = c('s', 'j')))

m1 <- stan_glmer(clr ~ class*time + (time|sentence_num) + (time|val) + 
                   (time|just) + (time|sentence_num:val:just), 
                 data=mod.dat, prior=normal(0,1))

posterior <- as.data.frame(m1$stanfit, pars=names(m1$coefficients[1:8]))

posterior %<>%
  mutate(fem_just = `(Intercept)` + timej + `classControl-Female` + `classControl-Female:timej`,
         male_just = `(Intercept)` + timej + `classControl-Male` + `classControl-Male:timej`,
         fem_prejust = `(Intercept)` + `classControl-Female`,
         male_prejust = `(Intercept)` +`classControl-Male`) %>%
  mutate(fem_diff = fem_just - fem_prejust,
         male_diff = male_just - male_prejust) %>%
  mutate(fem_pref = fem_diff - male_diff)

mean(posterior$fem_pref)

est <- mcmc_areas(posterior, pars='fem_pref', prob=.95)
est$data$l
est$data$h
prop.table(table(posterior$fem_pref>0))

#what does it mean?
df.just %>%
  group_by(sentence_num, class, val, just) %>%
  mutate(word_num = row_number(),
         keyword = max(word_num)) %>%
  filter(word_num > keyword-2) %>%
  mutate(time = abs((keyword-2)-word_num)) %>% 
  mutate(time = factor(time, labels=c('first', 'second'))) %>%
  ungroup() %>%
  select(class, just, val, time, prob) %>%
  group_by(class, time) %>%
  summarise(mean(prob),
            sd(prob))
