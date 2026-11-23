### The following code examines relationship between living conditions and area of residence, with depression.
### This is a two-way analysis of variance (ANOVA), with post-hoc tests, assumption's check, & altrernative non-parametric testing.

### Data Preperation
library("tidyverse")
library("readxl")
library("dplyr")
library("rstatix")
library("stats")
library("ggpubr")
library("car")
library("purrr")
library("MASS")
library("multcomp")
library("outliers")
library("lsr")
library("moments")
library("officer")


my_data <- read_excel(file.choose())
attach(my_data)

class(LivingWith) #class is numeric so we change to categorical (factor)
living <- LivingWith 
living_factor <- factor(living)
class(living_factor)

area1 <- Area
area_factor <- factor(area1)
class(area_factor)

PHQ <- PHQ9

new_data <- data_frame(PHQ, living_factor, area_factor)

####Descriptive statistics
###PHQ
mean(PHQ)
sd(PHQ)

print(skewness(PHQ))
print(kurtosis(PHQ))

###PHQ~Living
group_by(my_data, LivingWith) %>% 
  summarise(
    n = n(),
    M = mean(PHQ, na.rm = TRUE),
    SD = sd(PHQ, na.rm = TRUE))

###Area
group_by(my_data, Area) %>% 
  summarize (
    n=n())

####Visualize data

ggboxplot(my_data, x = "LivingWith", y = "PHQ9", 
          color = "LivingWith", palette = c(4,2,7),
          ylab = "PHQ-9 score", xlab = "Living Condition")

ggline(new_data, x = "living_factor", y = "PHQ", 
       add = c("mean_se", "jitter"), color = "area_factor",
       ylab = "PHQ-9 score", xlab = "Living Area")

###Exporting the graphs
gg_boxplot<- ggboxplot(my_data, x = "LivingWith", y = "PHQ9", 
                       color = "LivingWith", palette = c(4,2,7),
                       ylab = "PHQ-9 score", 
                       xlab = "Living Condition")
ggsave('image1.png',gg_boxplot, width=4, height = 3,units = "in")

gg_line<- ggline(new_data, x = "living_factor", y = "PHQ", 
                 add = c("mean_se", "jitter"), color = "area_factor",
                 ylab = "PHQ-9 score", 
                 xlab = "Living Area")
ggsave('image2.png',gg_line, width=4, height = 3,units = "in")


####Assumption check
###Equality of Variance

##For one-way ANOVA
leveneTest(PHQ ~  living_factor, data = my_data)

## For the factorial model: assess variance homogeneity across the
## living-condition by residential-area cells.
leveneTest(PHQ ~ interaction(living_factor, area_factor), data = my_data)

###Normality
shapiro.test(my_data$PHQ9)
qqPlot(PHQ)

###Outliers
my_data %>%
  group_by(LivingWith) %>%
  identify_outliers("PHQ9")

####Question 1 (two-way ANOVA)
two_anova <- aov(PHQ ~ living_factor + area_factor + 
                   living_factor:area_factor, data = my_data)
summary(two_anova)
etaSquared(two_anova, type = 2, anova = TRUE)

###Normality for residuals
qqnorm(two_anova$residuals) 
shapiro.test(two_anova$residuals )

####Post-hoc
TukeyHSD(two_anova)

#####Question 2 (one-way ANOVA)
one_anova <- aov(PHQ ~ living_factor, data = my_data)
summary(one_anova)
etaSquared(one_anova, type = 2, anova = TRUE)

multiple <- glht(one_anova, linfct = mcp(living_factor = "Tukey"))
multiple
summary(multiple)

###Normality for residuals
qqnorm(one_anova$residuals) 
shapiro.test(one_anova$residuals)

####Post-hoc
TukeyHSD(one_anova)



####Additional Testing
###For Violation of Normality
##Alternative to one-way ANOVA
kruskal.test(PHQ ~ living_factor, data = my_data)

#Post-hoc
dunn_test(my_data, PHQ9 ~ LivingWith)

### Exploratory outlier filtering: retain complete rows based on PHQ-9 values.
phq9_outliers <- boxplot.stats(my_data$PHQ9)$out
data_out_rm <- my_data[!(my_data$PHQ9 %in% phq9_outliers), , drop = FALSE]
length(my_data$PHQ9) - length(data_out_rm$PHQ9) # Number of removed observations

# Calculate IQR bounds and retain observations within the conventional 1.5*IQR range.
Q1 <- quantile(my_data$PHQ9, 0.25, na.rm = TRUE)
Q3 <- quantile(my_data$PHQ9, 0.75, na.rm = TRUE)
iqr_phq9 <- IQR(my_data$PHQ9, na.rm = TRUE)

no_outliers <- subset(
  my_data,
  PHQ9 >= (Q1 - 1.5 * iqr_phq9) &
    PHQ9 <= (Q3 + 1.5 * iqr_phq9)
)

#view row and column count of new data frame
dim(no_outliers) 

detach(my_data)

