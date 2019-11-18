# Data science challenge - Visualization of click-through-rates for a google search

# load package dependencies
library(here)
library(tidyverse)
library(data.table)
library(Hmisc)
library(corrplot)

# import dataset for click-through-rates
CTR <- fread(here::here("CTR_dataset_final.csv"))
glimpse(CTR)

CTR_filters <- CTR %>% 
  filter(position <=10) %>%
  filter(impressions < 100) %>%
  filter(clicks < 25) %>%
  drop_na(ctr)
 
CTR_filters_correlation <- CTR_filters %>%
  select(ctr, position)

# find the correlation between variables:
cor(CTR_filters)


# visuaization
corr_CTR_filters<-rcorr(as.matrix(CTR_filters[,1:4]))
corrplot(corr_CTR_filters$r, type="upper", order="hclust", 
         p.mat = corr_CTR_filters$P, sig.level = 0.01, insig = "blank")


# bonus
# total: 127933
CTR %>%
  select(clicks) %>%
  summarise(count = n())
# Less than 25 clicks: 125857
CTR %>%
  filter(clicks < 25) %>%
  summarise(count = n())

# divide to get 125857 / 127933: 98%
