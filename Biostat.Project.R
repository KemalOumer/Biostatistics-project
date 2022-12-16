### BIOSTAT Project###
#Basic survival analysis
#install.packages("survival",dependencies = TRUE)
#install.packages("survminer",dependencies = TRUE)
#install.packages("outliers",dependencies = TRUE)
#install.packages("EnvStats",dependencies = TRUE)
#install.packages("moments",dependencies = TRUE)
#install.packages("gmodels",dependencies = TRUE)
#loading packages#
library(rigr)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(survival)
library(survminer)
library(gtsummary)
library(outliers)# Outliers
library(EnvStats)# outliers
library (moments)



# import the text data set from link
birth.w <- read.table("http://faculty.washington.edu/elila/datasets/BirthsKingCounty2001-Biost514-517-2022.txt", header = T)
View(birth.w)
#To check up missing
df<-birth.w[complete.cases(birth.w), ]
df<-birth.w
df$sex<-dplyr::recode_factor(df$sex, F = "Female", M = "Male")
df$smoker<-dplyr::recode_factor(df$smoker, N = "No", Y = "Yes")
df$drinker<-dplyr::recode_factor(df$drinker, N = "No", Y = "Yes")
df$plural = factor(df$plural, c(0,1), c("No","Yes"))
df$married = factor(df$married, c(0,1), c("No","Yes"))
df$firstep = factor(df$firstep, c(0,1), c("No","Yes"))
df$welfare = factor(df$welfare, c(0,1), c("No","Yes"))
View(df)

# Descriptive statistics
descrip(df)
summary(df)

library(gridExtra)
# To identify outlier using boxplot graph
p1<-ggplot(df, aes(x= "",y=bwt)) +
  geom_boxplot() +
  ggtitle("Basic boxplot for child's birth weight") +
  xlab("") + ylab("Birth weight (g")

# to identify skenwness using graph
p2<-ggplot(df, aes(x=bwt)) + 
  geom_histogram(color="black", fill="white")+
  ylab("Frequency") + xlab("Birth weight (g)") +
  ggtitle("Histogram for child's birth weight")
grid.arrange(p1, p2, ncol=2)

# To identify outliers using test

grubbs.test(df$bwt,opposite = TRUE)
chisq.out.test(df$bwt)
rosnerTest(df$bwt)$all.stats
####Identify outliers values
OutVals = boxplot(df$bwt)$out
which(df$bwt %in% OutVals)

## To determine skewness
# calculate skewness in r
#install.packages("moments")
library(moments)
skewness(df$bwt)


# Descriptive statistics in table 1
df %>%
  tbl_summary(by = firstep,
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)"),
              digits = all_continuous() ~ 2
  ) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 2)) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "**Variable**")%>%
  modify_caption("**Table 1: Respondents Characteristics**") %>%
  bold_labels()

#A 95% CI for mean difference
#t.test test dataset (with out outlier)
shapiro.test(test$bwt)
t.test (bwt ~ firstep, var.equal=TRUE, data = df)
#############New table for one##############
library(table1)
library (boot)
df2 <- df

# Factor the basic variables that
# we're interested in
df2$firstep <- 
  factor(df2$firstep, 
         levels=c("Yes","No"),
         labels=c("The participants in First Steps Program", 
                  "The Non-participants in First Steps program"))
label(df2$age)      <- "Age (years)"
label(df2$sex)      <- "Sex"

label(df2$education)  <- "Mother’s education (years)"
label(df2$parity) <- "Prior number of children"
label(df2$bwt)  <- "Birth weights (gm)"
label(df2$gestation) <- "Gestational age at birth (weeks)"
label(df2$welfare)  <- "Welfare"
label(df2$wpre) <- "Mother’s pre-pregnancy weights in points"
label(df2$wgain)  <- "Maternal weight-gain during pregnancy"
label(df2$smoker)   <- "Smoker"
label(df2$smokeN)   <- "Number of smoking per day"
label(df2$drinker)   <- "Alcohol user "
label(df2$drinkN)   <- "Number of drinks per day"
df2$race <- factor(df2$race, 
                   levels=c("asian","black","white","hispanic","other"),
                   labels=c("Asian","Black","White","Hispanic","Other"))

label(df2$race)<- "Race"
df2$married <-factor(df2$married, 
                     levels=c("No","Yes"),
                     labels=c("Single","Married"))
label(df2$married)<- "Marital status"
#units(df22$age)<- "years"
#units(df2$gestation)<- "weeks"
table1(~ sex + age + race + married + education + parity + bwt + gestation + wpre 
       +wgain + welfare + smoker + smokeN + drinker + drinkN | firstep, data=df2, 
       overall = "Total")



         # Stratified
        
############# Check up#############

# to identify outliers using test
#dixon.test(df$bwt)#Dixon test for outliers
grubbs.test(df$bwt)
grubbs.test(df$bwt,opposite = TRUE)
chisq.out.test(df$bwt)
rosnerTest(df$bwt)$all.stats
# Histogram using bans
hist(df$bwt,xlab = "Birth weight", main = "Histogram of Birth weight in gram",breaks = sqrt(nrow(df)))
#To remove outliers
OutVals = boxplot(df$bwt)$out
which(df$bwt %in% OutVals)


# Table 2

  df %>%
  select(sex, plural,married, welfare,smoker, drinker, firstep) %>%
  tbl_summary(
    by = firstep,
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    #label = list(age ~ "Patient Age"),
    #statistic = list(all_continuous() ~ "{mean} ({sd})"),
    #digits = list(age ~ c(0, 1))
  ) %>%
add_p(pvalue_fun = ~style_pvalue(.x, digits = 2)) %>%
  add_overall() %>%
  #add_n() %>%
  modify_header(label ~ "**Variable**")%>%
  modify_caption("**Table 2: Respondents Characteristics by First Steps Program**") %>%
  bold_labels()
  
  ##
  ## Over all Box plot for two variables
  p1<-ggplot(df, aes(x=firstep, y=bwt,fill=firstep)) + 
    geom_boxplot(size = .2) + 
    ylab("Birth weight(g)") + xlab("Partcipants in the First Steps Program") +
    ggtitle("Overall boxplot ")
  
  ####labels for catagorical variables
  df3<-df
  df3$married <-factor(df3$married, 
                       levels=c("No","Yes"),
                       labels=c("Single","Married"))
  
  df3$welfare <-factor(df3$welfare, 
                       levels=c("No","Yes"),
                       labels=c("No welfare","Welfare"))
  df3$smoker <-factor(df3$smoker, 
                       levels=c("No","Yes"),
                       labels=c("Non smoker","Smoker"))
  # Using Box plot to identify modifiers
  #A. By Married status
  p2<-ggplot(df3, aes(x=firstep, y=bwt,fill=firstep)) + 
    geom_boxplot(size = .2) + 
    scale_fill_brewer(palette = "Set1")+
    facet_grid(~married) +
        ylab("Birth weight(g)") + xlab("Partcipants in the First Steps Program") +
    ggtitle("Boxplot stratified by marital status")
  #grid.arrange(p1, p2, ncol=2)
  
  ###B. By smoking status
  p3<-ggplot(df3, aes(x=firstep, y=bwt, fill=firstep)) + 
    geom_boxplot(size = .2) + 
    facet_grid(~smoker) +
    scale_fill_brewer(palette = "Set1")+
        ylab("Birth weight(g)") + xlab("Partcipants in the First Steps Program") +
    ggtitle("Boxplot stratified by smoking status")
  
  ### C. by welfare
  ###
  p4<-ggplot(df3, aes(x=firstep, y=bwt,fill=firstep)) + 
    geom_boxplot(size = .2) + 
    scale_fill_brewer(palette = "Set1")+
    facet_grid(~welfare) +
    ylab("Birth weight") + xlab("Partcipants in the First Steps Program") +
    ggtitle("Boxplot stratified by welfare status")
  
  grid.arrange(p1, p2, p3,p4, ncol=2)
  # Removing outlier and save a new dataset
  bwt_out_rm <- df$bwt[!df$bwt %in% boxplot.stats(df$bwt)$out]## Remove outliers
  length(df$bwt) - length(bwt_out_rm)## Count removed observations
  boxplot(bwt_out_rm) 
  # how to remove specific rows in r
  # remove rows in r by row number
  A<-c(34,92,99,111,165,192,287,292,293,308,369,415,430,459,538,
      545,566,639,685,717,749,869,917,982,998,1031,1052,1089,1195,1290,
     1312,1344,1381,1387,1498,1609,1610,1665,1762,1764,1766,1864,1907,1912,1989,
    2015,2029,2056,2073,2189,2209,2223,2227,2262,2301,2360,2433,2499)
  test <- df[-A,] 

  # After remove of outlier plot BOX plot
  ## Over all Box plot for two variables
  ggplot(test, aes(x=firstep, y=bwt)) + 
    geom_boxplot(size = .2) + 
    scale_fill_brewer(palette = "Set1")+
    ylab("Birth weight") + xlab("Partcipants in the First Steps Program") +
    ggtitle("Boxplot for Birth weight vs Participants in the First Steps Program ")
  
  #t-test df dataset (with outlier)
  shapiro.test(df$bwt)
  t.test (bwt ~ firstep, var.equal=TRUE, data = df)
  #t.test test dataset (with out outlier)
  shapiro.test(test$bwt)
  t.test (bwt ~ firstep, var.equal=TRUE, data = test)
  ##### For Smoker and Non smoker
  # To test Birth weight and participation of the first Steps program for smoking status
 # test for Smoker
   df.smoker<-subset(df, smoker=='Yes')
   t.test (bwt ~ firstep, var.equal=TRUE, data = df.smoker)
  
# test for non smoker
   df.Nsmoker<-subset(df, smoker=='No')
  t.test (bwt ~ firstep, var.equal=TRUE, data = df.Nsmoker)
  
  # test for welfare
  df.welfarer<-subset(df, welfare=='Yes')
  t.test (bwt ~ firstep, var.equal=TRUE, data = df.welfarer)
  
  # test for welfare
  df.nwelfarer<-subset(df, welfare=='No')
  t.test (bwt ~ firstep, var.equal=TRUE, data = df.nwelfarer)
  
  # for marital status
  # test for married
  df.married<-subset(df, married=='Yes')
  t.test (bwt ~ firstep, var.equal=TRUE, data = df.married)
  # test for single
  df.single<-subset(df, married=='No')
  t.test (bwt ~ firstep, var.equal=TRUE, data = df.single)
  
  
  ### Box plot for race 
  df2 <- df
df2$race <- factor(df2$race, 
              levels=c("asian","black","white","hispanic","other"),
              labels=c("Asian","Black","White","Hispanic","Other"))
 
  ggplot(df2, aes(x=firstep, y=bwt,fill=firstep)) + 
    geom_boxplot(size = .2) + 
    scale_fill_brewer(palette = "Set1")+
    facet_grid(~race) +
    ylab("Birth weight(g)") + xlab("Partcipants in the First Steps Program") +
    ggtitle("Boxplot for birth weight vs First Steps Program stratified by race")
  
  # ANOVA for birth weight and race
  # Step 2: Calculate test statistics using aov function
  df_aov <- aov(df$bwt~factor(df$race))
  summary(df_aov)
 
  # for participants in the first steps program
  df.par<-subset(df, firstep == 'Yes')
  df_aov <- aov(df.par$bwt~factor(df.par$race))
  summary(df_aov)
  
  # for  Not participants in the first steps program
  df.nopar<-subset(df, firstep == 'No')
  df_aov <- aov(df.nopar$bwt~factor(df.nopar$race))
  summary(df_aov)

  
  # for birth weight classicication
  # Continuous categorized
  df$bwt_cat <- cut(df$bwt, breaks = c(-Inf,1500,2500,Inf), labels = c("Very low birth weight","Low birth weight","Normal"))
  df$bwt_cat<-as.factor(df$bwt_cat)
  class(df$bwt_cat)
  
  
  # table for classification of birthweight
  df %>%
    select(bwt_cat, firstep) %>%
    tbl_summary(
      by = firstep,
      statistic = list(all_categorical() ~ "{n} ({p}%)"),
      
    ) %>%
    add_p(pvalue_fun = ~style_pvalue(.x, digits = 2)) %>%
    add_overall() %>%
    #add_n() %>%
    modify_header(label ~ "**Variable**")%>%
    modify_caption("**Table 3: Respondents characteristics by First Steps Program**") %>%
    bold_labels()
  