# World-Happiness-

Overview

This project explores factors that influence happiness levels across countries using data from the 2020 World Happiness Report. The analysis was completed in R using data cleaning, visualization, descriptive statistics, correlation analysis, and hypothesis testing.

Dataset

The dataset contains information from 153 countries, including:

Happiness score
GDP per capita
Social support
Healthy life expectancy
Freedom to make life choices
Generosity
Perception of corruption

Source: World Happiness Report 2020

=========================================
WORLD HAPPINESS REPORT 2020 ANALYSIS 🤍
=========================================
Objectives Analyze global happiness scores Identify relationships between happiness and economic/social indicators Compare high- and low-happiness countries Visualize trends using ggplot2

library(tidyverse)
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.2.1     ✔ readr     2.2.0
## ✔ forcats   1.0.1     ✔ stringr   1.6.0
## ✔ ggplot2   4.0.3     ✔ tibble    3.3.1
## ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
## ✔ purrr     1.2.2     
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
library(dplyr)
happiness <- read.csv("2020.csv")
head(happiness)
##   Country.name Happiness.Rank Happiness.score Upperwhisker Lowerwhisker
## 1      Finland              1            7.81         7.87         7.75
## 2      Denmark              2            7.65         7.71         7.58
## 3  Switzerland              3            7.56         7.63         7.49
## 4      Iceland              4            7.50         7.62         7.39
## 5       Norway              5            7.49         7.56         7.42
## 6  Netherlands              6            7.45         7.50         7.39
##   Economy..GDP.per.Capita.. Social.support Healthy.life.expectancy
## 1                      1.29           1.50                    0.96
## 2                      1.33           1.50                    0.98
## 3                      1.39           1.47                    1.04
## 4                      1.33           1.55                    1.00
## 5                      1.42           1.50                    1.01
## 6                      1.34           1.46                    0.98
##   Freedom.to.make.life.choices Generosity Perceptions.of.corruption
## 1                         0.66       0.16                      0.48
## 2                         0.67       0.24                      0.50
## 3                         0.63       0.27                      0.41
## 4                         0.66       0.36                      0.14
## 5                         0.67       0.29                      0.43
## 6                         0.61       0.34                      0.37
library(dplyr)

happiness <- happiness %>%
  rename(
    country = Country.name,
    rank = Happiness.Rank,
    score = Happiness.score,
    upper_whisker = Upperwhisker,
    lower_whisker = Lowerwhisker,
    gdp_per_capita = Economy..GDP.per.Capita..,
    social_support = Social.support,
    life_expectancy = Healthy.life.expectancy,
    freedom = Freedom.to.make.life.choices,
    generosity = Generosity,
    corruption = Perceptions.of.corruption,
  )
=========================================
DATA CLEANING 🤍
=========================================
Renaming variables for readability Removing missing values Removing duplicate rows Checking data structure and variable types

colSums(is.na(happiness))
##         country            rank           score   upper_whisker   lower_whisker 
##               0               0               0               0               0 
##  gdp_per_capita  social_support life_expectancy         freedom      generosity 
##               0               0               0               0               0 
##      corruption 
##               0
happiness <- na.omit(happiness)
happiness <- distinct(happiness)
str(happiness)
## 'data.frame':    153 obs. of  11 variables:
##  $ country        : chr  "Finland" "Denmark" "Switzerland" "Iceland" ...
##  $ rank           : int  1 2 3 4 5 6 7 8 9 10 ...
##  $ score          : num  7.81 7.65 7.56 7.5 7.49 7.45 7.35 7.3 7.29 7.24 ...
##  $ upper_whisker  : num  7.87 7.71 7.63 7.62 7.56 7.5 7.42 7.38 7.36 7.3 ...
##  $ lower_whisker  : num  7.75 7.58 7.49 7.39 7.42 7.39 7.28 7.22 7.23 7.18 ...
##  $ gdp_per_capita : num  1.29 1.33 1.39 1.33 1.42 1.34 1.32 1.24 1.32 1.54 ...
##  $ social_support : num  1.5 1.5 1.47 1.55 1.5 1.46 1.43 1.49 1.44 1.39 ...
##  $ life_expectancy: num  0.96 0.98 1.04 1 1.01 0.98 0.99 1.01 1 0.99 ...
##  $ freedom        : num  0.66 0.67 0.63 0.66 0.67 0.61 0.65 0.65 0.6 0.61 ...
##  $ generosity     : num  0.16 0.24 0.27 0.36 0.29 0.34 0.27 0.33 0.26 0.2 ...
##  $ corruption     : num  0.48 0.5 0.41 0.14 0.43 0.37 0.44 0.46 0.28 0.37 ...
=========================================
DATA MANIPULATION WITH DPLYR 🤍
=========================================
Filter countries with high happiness score
high_happiness <- happiness %>% filter(score > 7)
This step filtered the dataset to identify countries with exceptionally high happiness scores.

Most countries with happiness scores above 7 were economically developed nations with high levels of social support and life expectancy.

Countries by happiness score
library(dplyr)
library(ggplot2)

top10 <- happiness %>%
  arrange(desc(score)) %>%
  slice(1:10)

ggplot(top10, aes(x = reorder(country, score), y = score)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 10 Happiest Countries",
    x = "Country",
    y = "Happiness Score") + theme_minimal()


The visualization shows that Nordic countries such as Finland, Denmark, and Iceland ranked among the happiest countries in the world in 2020. These countries consistently scored highly in GDP per capita, social support, freedom, and healthy life expectancy.

bottom10 <- happiness %>% arrange(score) %>% slice(1:10)

ggplot(bottom10, aes(x = reorder(country, score), y = score)) +
  geom_col(fill = "tomato") + coord_flip() +labs(
    title = "Bottom 10 Happiest Countries",
    x = "Country",
    y = "Happiness Score") + theme_minimal()


Many lower-ranked countries also showed lower GDP per capital and reduced social support, indicating a potential relationship between economic/social conditions and happiness.

Grouped summaries
happiness <- happiness %>%
  mutate(level = case_when(score >= 7 ~ "High",
      score >= 5 ~ "Medium",
      TRUE ~ "Low"
    )
  )
Countries were grouped into three happiness categories: High, Medium, and Low. This categorization simplified comparisons between countries

summary_by_happiness <- happiness %>%
  group_by(level) %>%
  summarise(
    Average_Happiness = mean(score),
    Average_GDP = mean(gdp_per_capita),
    Average_Social_Support = mean(social_support),
    Average_Life_Expectancy = mean(life_expectancy),
    Country_Count = n()
  )

print(summary_by_happiness)
## # A tibble: 3 × 6
##   level  Average_Happiness Average_GDP Average_Social_Support
##   <chr>              <dbl>       <dbl>                  <dbl>
## 1 High                7.33       1.32                   1.45 
## 2 Low                 4.27       0.525                  0.895
## 3 Medium              5.86       0.997                  1.26 
## # ℹ 2 more variables: Average_Life_Expectancy <dbl>, Country_Count <int>
The grouped summary revealed clear differences between happiness levels. GDP

High-happiness countries reported an average GDP per capita more than twice as large as low-happiness countries, suggesting a strong connection between economic prosperity and well-being.

Social Support

Social support levels were significantly higher among happier countries, indicating that strong social networks and community support may contribute positively to national happiness.

Life Expectancy

Countries with higher happiness scores also tended to have longer healthy life expectancy, suggesting that healthcare and quality of life are important contributors to happiness.

=========================================
DESCRIPTIVE STATISTICS 🤍
=========================================
summary(happiness)
##    country               rank         score       upper_whisker  
##  Length:153         Min.   :  1   Min.   :2.570   Min.   :2.630  
##  Class :character   1st Qu.: 39   1st Qu.:4.720   1st Qu.:4.830  
##  Mode  :character   Median : 77   Median :5.510   Median :5.610  
##                     Mean   : 77   Mean   :5.473   Mean   :5.578  
##                     3rd Qu.:115   3rd Qu.:6.230   3rd Qu.:6.360  
##                     Max.   :153   Max.   :7.810   Max.   :7.870  
##  lower_whisker   gdp_per_capita   social_support  life_expectancy 
##  Min.   :2.510   Min.   :0.0000   Min.   :0.000   Min.   :0.0000  
##  1st Qu.:4.600   1st Qu.:0.5800   1st Qu.:0.990   1st Qu.:0.5000  
##  Median :5.430   Median :0.9200   Median :1.200   Median :0.7600  
##  Mean   :5.368   Mean   :0.8689   Mean   :1.155   Mean   :0.6931  
##  3rd Qu.:6.140   3rd Qu.:1.1700   3rd Qu.:1.390   3rd Qu.:0.8700  
##  Max.   :7.750   Max.   :1.5400   Max.   :1.550   Max.   :1.1400  
##     freedom         generosity       corruption        level          
##  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Length:153        
##  1st Qu.:0.3800   1st Qu.:0.1200   1st Qu.:0.0600   Class :character  
##  Median :0.4800   Median :0.1800   Median :0.1000   Mode  :character  
##  Mean   :0.4635   Mean   :0.1895   Mean   :0.1306                     
##  3rd Qu.:0.5800   3rd Qu.:0.2600   3rd Qu.:0.1600                     
##  Max.   :0.6900   Max.   :0.5700   Max.   :0.5300
library(corrplot)
## corrplot 0.95 loaded
library(dplyr)

numeric_data <- happiness %>% 
  select(
    score,
    gdp_per_capita,
    social_support,
    life_expectancy,
    freedom,
    generosity,
    corruption
  )

corr_matrix <- cor(numeric_data, use = "complete.obs")
library(corrplot)

corrplot(
  corr_matrix,
  method = "color",
  type = "upper",
  order = "hclust",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 9,
  tl.cex = 0.6, 
  col = colorRampPalette(c("blue", "white", "red"))(200)
)


GDP and Happiness
GDP per capita showed a strong positive correlation with happiness score (r = 0.77), suggesting that countries with stronger economies tend to report higher levels of happiness.

Social Support Matters
Social support also had a strong relationship with happiness (r = 0.76), indicating that countries with stronger support systems generally report higher happiness scores.

Life Expectancy
Healthy life expectancy was strongly associated with happiness levels (r = 0.77).

Generosity Had Weak Correlation
Generosity showed only a weak relationship with happiness (r = 0.07).

##                   Variable Correlation
## 1           GDP per Capita  0.77452680
## 2           Social Support  0.76488666
## 3  Healthy Life Expectancy  0.76969468
## 4        Freedom of Choice  0.59089300
## 5               Generosity  0.07176264
## 6 Perception of Corruption  0.41847864
=========================================
Hypothesis Test 🤍
=========================================
t.test(
  happiness$gdp_per_capita[happiness$level == "High"],
  happiness$gdp_per_capita[happiness$level == "Low"]
)
## 
##  Welch Two Sample t-test
## 
## data:  happiness$gdp_per_capita[happiness$level == "High"] and happiness$gdp_per_capita[happiness$level == "Low"]
## t = 16.151, df = 64.594, p-value < 2.2e-16
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  0.6921432 0.8875016
## sample estimates:
## mean of x mean of y 
## 1.3152941 0.5254717
Results:

p-value < 2.2e-16 Significant difference in GDP levels between groups

This suggests that higher GDP per capita is strongly associated with higher happiness scores.

=========================================
VISUALIZATIONS 🤍
=========================================
library(tidyr)

happiness_long <- happiness %>%
  select(score,
         gdp_per_capita,
         social_support,
         life_expectancy,
         freedom,
         generosity,
         corruption) %>%
  pivot_longer(
    cols = -score,
    names_to = "indicator",
    values_to = "value"
  )
ggplot(happiness_long, aes(x = value, y = score)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  facet_wrap(~ indicator, scales = "free_x") +
  labs(
    title = "Drivers of World Happiness",
    x = "Indicator Value",
    y = "Happiness Score"
  ) +
  theme_minimal()
## `geom_smooth()` using formula = 'y ~ x'


=========================================
End of Project 🤍
=========================================
The analysis suggests that economic stability, social support, and life expectancy are major contributors to national happiness levels. While freedom and corruption showed moderate relationships, generosity appeared to have minimal impact on overall happiness scores.
