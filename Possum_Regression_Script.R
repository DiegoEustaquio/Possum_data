# Possum Regression

# case: id (observation number)
# site: The site number where the possum was trapped.
# Pop: Population, either Vic (Victoria) or other
#     (New South Wales or Queensland).
# sex: Gender, either m (male) or f (female).
# age: age.
# hdlngth: Head length, in mm.
# skullw: Skull width, in mm.
# totlngth: Total length, in cm.
# taill: Tail length, in cm.
# footlgth: foot length.
# earconch: ear conch length.
# eye: distance from medial canthus to lateral canthus of right eye.
# chest: chest girth (in cm)
# belly: belly girth (in cm)





# Packages used:
library(ggplot2)
library(scales)
library(tidyverse)
library(plotly)

# Reading the data into R.
possum <- read_csv(file = "possum.csv")

str(possum)
head(possum)

summary(possum)

sum(is.na(possum))


# checking for inconsistency

unique(possum$site) #double
unique(possum$Pop) #char
unique(possum$sex) #char
unique(possum$age) #double NA (44, 46)
unique(possum$hdlngth) #double
unique(possum$skullw) #double
unique(possum$totlngth) #double
unique(possum$taill) #double
unique(possum$footlgth) #doubleNA (41)
unique(possum$earconch) #double
unique(possum$eye) #double
unique(possum$chest) #double
unique(possum$belly) #double


# Summarizing Categorical Variables

# Sex

  # Proportions
  sex_prop <- possum %>% 
    count(sex) %>% 
    mutate(prop = n/sum(n), pct = prop*100) %>% 
    mutate(PropCiLow = prop - (qnorm(.975) * sqrt(prop * (1 - prop))/n),
           PropCiUp = prop + (qnorm(.975) * sqrt(prop * (1 - prop))/n))
  # A tibble: 2 × 6
  #   sex       n  prop   pct PropCiLow PropCiUp
  #   <chr> <int> <dbl> <dbl>     <dbl>    <dbl>
  # 1 f        43 0.413  41.3     0.391    0.436
  # 2 m        61 0.587  58.7     0.571    0.602
  
  # Visualization
  sex_prop %>% 
    ggplot(aes(x = sex, y = n)) + 
    geom_bar(colour = "black", fill = '#006633', 
             stat = 'identity') +
    labs (title = "Sex",
          subtitle = "Count",
          y = "Count",
          x = "Sex") +
    geom_text(aes(label = n), vjust = 2, colour = "white", size = 4) +
    theme_bw() 

  
  
# Pop
  
  # Proportions
  pop_prop <- possum %>% 
    count(Pop) %>% 
    mutate(prop = n/sum(n), pct = prop*100) %>% 
    mutate(PropCiLow = prop - (qnorm(.975) * sqrt(prop * (1 - prop))/n),
           PropCiUp = prop + (qnorm(.975) * sqrt(prop * (1 - prop))/n))
  # # A tibble: 2 × 6
  #   Pop       n  prop   pct PropCiLow PropCiUp
  #   <chr> <int> <dbl> <dbl>     <dbl>    <dbl>
  # 1 Vic      46 0.442  44.2     0.421    0.463
  # 2 other    58 0.558  55.8     0.541    0.574

  # Visualization
  pop_prop %>% 
    ggplot(aes(x = Pop, y = n)) + 
    geom_bar(colour = "black", fill = '#006633', 
             stat = 'identity') +
    labs (title = "Sex",
          subtitle = "Count",
          y = "Count",
          x = "Sex") +
    geom_text(aes(label = n), vjust = 2, colour = "white", size = 4) +
    theme_bw()


# Summarizing Numerical Variables
  
  possum %>%  
    ggplot(aes(x = age)) + 
    geom_histogram(bins = 10, fill = '#006633', colour = "black") + 
    xlab("Age") +
    labs (title = "Age",
          subtitle = "Histogram - Bin Width: 5",
          y = "Count",
          x = "Age") +
    theme_bw()


  possum %>%  
    ggplot(aes(y = age)) + 
    geom_boxplot(fill = '#006633', 
                 colour = "black") +
    ylab("Age") +
    labs (title = "Age",
          subtitle = "Box Plot") +
    theme_bw()
  
  possum %>%
    na.omit() %>% 
    summarise(mean_age = mean(age),
              median_age = median(age),
              var_age = var(age),
              sd_age = sd(age),
              IQR_age = IQR(age))
  # mean_age median_age var_age sd_age IQR_age
  # <dbl>      <dbl>   <dbl>  <dbl>   <dbl>
  #   1     3.82          3    3.67   1.92       3
  

# scatterplot charges vs numerical variables
plot(possum$hdlngth, possum$skullw,
     xlab = "Head length",
     ylab = "Skull width")

# Visualizing pair plot for variables relationship
possum %>% 
  select(age, hdlngth, skullw, totlngth, taill, footlgth, earconch,
         eye, chest, belly) %>% 
  pairs

# Correlation analysis
possum %>% 
  select(age, hdlngth, skullw, totlngth, taill, footlgth, earconch,
         eye, chest, belly) %>% 
  cor() %>% 
  round(digits = 2)






# Exploratory analysis

ggplot(possum, aes(x = sex, y = hdlngth, fill = sex)) +
  geom_boxplot(alpha = 0.5) +
  theme_bw()






