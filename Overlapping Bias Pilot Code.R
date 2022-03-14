a <- read.csv("https://raw.githubusercontent.com/chadmillerror/Anti-Christian-and-Anti-White-Bias/main/Group%20Bias%20Reactions_December%2024%2C%202021_14.39.csv?token=GHSAT0AAAAAABSNNOO32BMMUKNZBNCG2F46YRU7Q4A", 
              skip = 3)
names(a) <- colnames(read.csv("https://raw.githubusercontent.com/chadmillerror/Anti-Christian-and-Anti-White-Bias/main/Group%20Bias%20Reactions_December%2024%2C%202021_14.39.csv?token=GHSAT0AAAAAABSNNOO32BMMUKNZBNCG2F46YRU7Q4A", 
                              nrows = 1))
aheader <- read.csv("https://raw.githubusercontent.com/chadmillerror/Anti-Christian-and-Anti-White-Bias/main/Group%20Bias%20Reactions_December%2024%2C%202021_14.39.csv?token=GHSAT0AAAAAABSNNOO32BMMUKNZBNCG2F46YRU7Q4A", 
                    nrows = 1)
Hmisc::label(a) <- aheader

library(qualtRics)
library(psych)
library(dplyr)
library(tidyr)
library(purrr)
library(rstatix)
library(ggplot2)

a$Bias <- relevel(as.factor(recode(a$FL_31_DO, "FL_37"='Black', "FL_36"='White', "FL_35"='None', "FL_34"='Christian')), "None")
a$Biasn <- as.numeric(recode(a$FL_31_DO, "FL_37"=3, "FL_36"=2, "FL_35"=0, "FL_34"=1))
a$Fundamentalism <- relevel(as.factor(recode(as.factor(a$F), "2"='No', "1"='Yes')), "No")
a$Evangelical <- relevel(as.factor(recode(as.factor(a$E), "2"='No', "1"='Yes')), "No")
table(a$Evangelical, a$Fundamentalism, a$Bias)
meas <- function(x, y, cov=NULL) {
  a <- cor(x, use = "pairwise")
  b <- range(x, na.rm = T)
  c <- psych::alpha(x, check.keys = T)
  d <- psych::alpha(x, check.keys = T)$scores
  e <- shapiro.test(d)
  f <- data.frame(d)
  if(is.null(cov)) {
    g <- ggplot2::ggplot(f, aes(x=d)) + 
      geom_histogram(aes(y=..density..)) +
      geom_density(adjust=1,kernel="gaussian",na.rm=TRUE,
                   color="red" ,size=.5) + 
      stat_function(fun = dnorm, 
                    args = list(mean = mean(f$d,na.rm=TRUE),
                                sd = sd(f$d,na.rm=TRUE)), 
                    size = 1, 
                    color = "blue") +
      theme_bw() +
      ggtitle("measure distribution") +
      xlab("measure")
  }
  else {f$cov <- cov
  f$resid <- residuals(lm(f$d ~ f$cov, na.action = "na.exclude"))
  g <- ggplot2::ggplot(f, aes(x=resid)) + 
    geom_histogram(aes(y=..density..)) +
    geom_density(adjust=1,kernel="gaussian",na.rm=TRUE,
                 color="red" ,size=.5) + 
    stat_function(fun = dnorm, 
                  args = list(mean = mean(f$resid,na.rm=TRUE),
                              sd = sd(f$resid,na.rm=TRUE)), 
                  size = 1, 
                  color = "blue") +
    theme_bw() +
    ggtitle("residual distribution") +
    xlab("residual")
  }
  return(list("Score"=d, "Cor"=a, "ALpha"=c, "Range"=b, "Normality"=e, g))
}

# ACB = Anti-Christian Bias. Warnings about psych::alpha are fine to ignore. 
t <- meas(a %>% dplyr::select(ACB1, ACB2, ACB3, ACB4, ACB5, ACB6), cov = a$Bias)
t
a$ACB <- as.numeric(unlist(t[1]))

# CN = Christian Nationalism
t <- meas(a %>% dplyr::select(CN1, CN2, CN3, CN4, CN5, CN6), cov = a$Bias)
t
a$CN <- as.numeric(unlist(t[1]))

# CRT = Christian Realistic Threat (not Critical Race Theory)
t <- meas(a %>% dplyr::select(CR1, CR2, CR3, CR4, CR5, CR6), cov = a$Bias)
t
a$CRT <- as.numeric(unlist(t[1]))

# AWB = Anti-White Bias
t <- meas(a %>% dplyr::select(AWB1, AWB2, AWB3, AWB4, AWB5, AWB6, AWB7, AWB8), cov = a$Bias)
t
a$AWB <- as.numeric(unlist(t[1]))

# ABB = Anti-Black Bias
t <- meas(a %>% dplyr::select(ABB1, ABB2, ABB3, ABB4, ABB5, ABB6), cov = a$Bias)
t
a$ABB <- as.numeric(unlist(t[1]))

# RST = Racial Symbolic Threat
t <- meas(a %>% dplyr::select(RS1, RS2, RS3, RS4, RS5, RS6), cov = a$Bias)
t
a$RST <- as.numeric(unlist(t[1]))

# RRT = Racial Realistic Threat
t <- meas(a %>% dplyr::select(RR1, RR2, RR3, RR4, RR5), cov = a$Bias)
t
a$RRT <- as.numeric(unlist(t[1]))

# CST = Christian Symbolic Threat
t <- meas(a %>% dplyr::select(CS1, CS2, CS3, CS4, CS5, CS6), cov = a$Bias)
t
a$CST <- as.numeric(unlist(t[1]))

# SDO = Social Dominance Theory
t <- meas(a %>% dplyr::select(SDO1, SDO2, SDO3, SDO4), cov = a$Bias)
t
a$SDO <- as.numeric(unlist(t[1]))

# WID = White ID Centrality
t <- meas(a %>% dplyr::select(WID1, WID2, WID3, WID4, WID5), cov = a$Bias)
t
a$WID <- as.numeric(unlist(t[1]))

# CID = Christian ID Centrality
t <- meas(a %>% dplyr::select(CID1, CID2, CID3, CID4, CID5), cov = a$Bias)
t
a$CID <- as.numeric(unlist(t[1]))

a <- a %>% dplyr::rename("MenFT"=FT_1, "WomenFT"=FT_2, "LGBTFT"=FT_3, "StraightFT"=FT_4, "MuslimFT"=FT_5, 
                         "WhiteFT"=FT_6, "HispanicFT"=FT_7, "ConservFT"=FT_8, "LibFT"=FT_9, "BlackFT"=FT_10, "AtheistFT"=FT_11,
                         "ChristFT"=FT_12, "WealthyFT"=FT_13)
a <- a %>% dplyr::rename("MenGB"=GB_1, "WomenGB"=GB_2, "LGBTGB"=GB_3, "StraightGB"=GB_4, "MuslimGB"=GB_5, 
                         "WhiteGB"=GB_6, "HispanicGB"=GB_7, "ConservGB"=GB_8, "LibGB"=GB_9, "BlackGB"=GB_10, "AtheistGB"=GB_11,
                         "ChristGB"=GB_12, "WealthyGB"=GB_13)
nrow(subset(a, Progress > 25))
nrow(subset(a, Progress > 25 & RaceAll == 5 &
              RaceOne == 5 &
              Christian == 1 &
              US == 1))
a2 <- subset(a, RaceAll == 5 &
               RaceOne == 5 &
               Christian == 1 &
               US == 1 & 
               Attention == 5
)
a3 <- subset(a, RaceAll == 5 &
               RaceOne == 5 &
               Christian == 1 &
               US == 1 & 
               Attention == 5 &
               ((ACBMC1 == 4 & ACBMC2 == 1) |    (AWBMC1 == 4 & AWBMC2 == 1) |    (ABBMC1 == 4 & ABBMC2 == 1) | Biasn == 0)
)


table(a3$Bias, a3$Biasn)
table(a3$G)

# does Christian Nationalism differ by Condition? ----
Anova(aov(a3$CN ~ a3$Bias))
pairwise.t.test(a3$CN, a3$Bias, "none", pool.sd = F, var.equal=F)
pairwise.t.test(a3$CN, a3$Bias, "holm", pool.sd = F, var.equal=F)
summary(lm(a3$CN ~ a3$Bias))

four <- function(y) {
  data <- data.frame("y"=y, "x"=a3$Bias, "CN"=a3$CN)
  a <- car::Anova(aov(data$y ~ data$x))
  b <- pairwise.t.test(data$y, data$x, "none", pool.sd = F, var.equal=F)
  c <- pairwise.t.test(data$y, data$x, "holm", pool.sd = F, var.equal=F)
  d <- summary(lm(data$y ~ data$x))
  data <- na.omit(data)
  data$CNsc <- scale(data$CN, scale = F)
  e <- summary(lm(data$y ~ data$x*data$CNsc))
  return(list("anova"=a, "pairwise t test no correction"=b, "pairwise t test holm correction"=c, "Manipulation vs. Control"=d, "Christian Nationalism Moderation"=e))
}

fourgb <- function(y) {
  data <- data.frame("y"=y, "x"=a3$Bias, z=a3$genbias_1, "CN"=a3$CN)
  a <- car::Anova(aov(data$y ~ data$x + data$z))
  b <- pairwise.t.test(residuals(lm(data$y ~ data$z)), data$x, "none", pool.sd = F, var.equal=F)
  c <- pairwise.t.test(residuals(lm(data$y ~ data$z)), data$x, "holm", pool.sd = F, var.equal=F)
  d <- summary(lm(data$y ~ data$x))
  e <- summary(lm(data$y ~ data$x + data$z))
  data <- na.omit(data)
  data$CNsc <- scale(data$CN, scale = F)
  f <- summary(lm(data$y ~ data$z + data$x*data$CNsc))
  return(list("anova"=a, "pairwise t test no correction"=b, "pairwise t test holm correction"=c, "Manipulation vs. Control"=d, "Manipulation vs. Control (with general bias covariate"=e, "Christian Nationalism Moderation (with general bias covariate)"=f))
}

fourft <- function(y) {
  data <- data.frame("y"=y, "x"=a3$Bias, z=a3$genfeel_2, "CN"=a3$CN)
  a <- car::Anova(aov(data$y ~ data$x + data$z))
  b <- pairwise.t.test(residuals(lm(data$y ~ data$z)), data$x, "none", pool.sd = F, var.equal=F)
  c <- pairwise.t.test(residuals(lm(data$y ~ data$z)), data$x, "holm", pool.sd = F, var.equal=F)
  d <- summary(lm(data$y ~ data$x))
  e <- summary(lm(data$y ~ data$x + data$z))
  data <- na.omit(data)
  data$CNsc <- scale(data$CN, scale = F)
  f <- summary(lm(data$y ~ data$z + data$x*data$CNsc))
  return(list("anova"=a, "pairwise t test no correction"=b, "pairwise t test holm correction"=c, "Manipulation vs. Control"=d, "Manipulation vs. Control (with general warmth covariate"=e, "Christian Nationalism Moderation (with general warmth covariate)"=f))
}
# DVs by condition ----
four(a3$ACB)
four(a3$AWB)
four(a3$ABB)
four(a3$CRT)
four(a3$CST)
four(a3$RST)
four(a3$RRT)
four(a3$WID)
four(a3$CID)
# single item measures. See label for the measure
Hmisc::label(a3$genbias_1)
four(a3$genbias_1)
Hmisc::label(a3$genfeel_2)
four(a3$genfeel_2)
Hmisc::label(a3$Politics)
four(a3$Politics)
Hmisc::label(a3$Religiosity)
four(a3$Religiosity)

# group bias perception DVs by condition ----
fourgb(a3$LibGB)
fourgb(a3$ConservGB)
fourgb(a3$MenGB)
fourgb(a3$WomenGB)
fourgb(a3$ChristGB)
fourgb(a3$MuslimGB)
fourgb(a3$AtheistGB)
fourgb(a3$LGBTGB)
fourgb(a3$StraightGB)
fourgb(a3$HispanicGB)
fourgb(a3$WhiteGB)
fourgb(a3$BlackGB)
fourgb(a3$WealthyGB)

# feeling thermometer DVs by condition ----
fourft(a3$LibFT)
fourft(a3$ConservFT)
fourft(a3$MenFT)
fourft(a3$WomenFT)
fourft(a3$ChristFT)
fourft(a3$MuslimFT)
fourft(a3$AtheistFT)
fourft(a3$LGBTFT)
fourft(a3$StraightFT)
fourft(a3$HispanicFT)
fourft(a3$WhiteFT)
fourft(a3$BlackFT)
fourft(a3$WealthyFT)

# Evangelicalism seems to be a better predictor of anti-Christian Bias. Neither predict anti-White or anti-Black bias.
summary(lm(a3$ACB ~ a3$Fundamentalism + a3$Evangelical + a3$Bias))
summary(lm(a3$AWB ~ a3$Fundamentalism + a3$Evangelical + a3$Bias))
summary(lm(a3$ABB ~ a3$Fundamentalism + a3$Evangelical + a3$Bias))
summary(lm(a3$CN ~ a3$Fundamentalism + a3$Evangelical + a3$Bias))

five <- function(y) {
  data <- data.frame("y"=y, "Bias"=a3$Bias, "Evangelical"=a3$Evangelical)
  a <- car::Anova(aov(data$y ~ data$Bias))
  b <- pairwise.t.test(data$y, data$Bias, "none", pool.sd = F, var.equal=F)
  c <- pairwise.t.test(data$y, data$Bias, "holm", pool.sd = F, var.equal=F)
  d <- summary(lm(data$y ~ data$Bias))
  e <- summary(lm(data$y ~ data$Bias + data$Evangelical))
  f <- summary(lm(data$y ~ (data$Bias + data$Evangelical)^2))
  return(list("anova"=a, "pairwise t test no correction"=b, "pairwise t test holm correction"=c, "Manipulation vs. Control"=d, "Manipulation vs. Control + Evangelical"=e, "Evangelical Moderation"=f))
}

# test for moderation by Evangelical identity ----
Hmisc::label(a3$E)
five(a3$ACB)
five(a3$AWB)
five(a3$ABB)
five(a3$CN)
five(a3$CRT)
five(a3$CST)
five(a3$RST)
five(a3$RRT)
five(a3$Politics)
five(a3$Religiosity)
five(a3$WID)
five(a3$CID)

t <- meas(a3 %>% dplyr::select(MenGB, WomenGB, LGBTGB, StraightGB, MuslimGB, WhiteGB, HispanicGB, ConservGB, LibGB, BlackGB, AtheistGB, WealthyGB), cov = a3$Bias)
t
a3$nonChristGB <- as.numeric(unlist(t[1]))

t <- meas(a3 %>% dplyr::select(MenGB, WomenGB, LGBTGB, StraightGB, MuslimGB, HispanicGB, ConservGB, LibGB, BlackGB, AtheistGB, ChristGB, WealthyGB), cov = a3$Bias)
t
a3$nonWhiteGB <- as.numeric(unlist(t[1]))

t <- meas(a3 %>% dplyr::select(MenGB, WomenGB, LGBTGB, StraightGB, MuslimGB, HispanicGB, ConservGB, LibGB, BlackGB, AtheistGB, WealthyGB), cov = a3$Bias)
t
a3$nonChristWhiteGB <- as.numeric(unlist(t[1]))

# do the effects on Anti-White Bias and Anti-Christian Bias Measures remain after controlling for condition's effect on perceived bias toward non-White and non-Christian groups 
summary(lm(a3$AWB ~ a3$Bias + a3$nonChristWhiteGB))
summary(lm(a3$ACB ~ a3$Bias + a3$nonChristWhiteGB))