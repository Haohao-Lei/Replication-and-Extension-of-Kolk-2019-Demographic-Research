######################################    Basic data cleaning     #############################################

###working directory
setwd("~/OneDrive - Nexus365/MPhil study/MPhil Year two/Replication Project")

#####load the package
library(tidyverse)
library(haven)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(data.table)
library(ggthemes)
library(readxl)
library(countrycode)
library(ggrepel)
library(dplyr)
library(lmtest)      # for Wald tests
library(sandwich)    # for robust / clustered vcov
library(car)         # convenient linearHypothesis()


#####load the data
gender <- read_sav("Final/Country_Year_V-Dem_SPSS_v7.1/V-Dem-DS-CY-v7.1.sav")

####cleaning the gender data
gender <- dplyr::select(gender, country_name, country_text_id, year, v2x_gender)

####load the fertility data
fertility <- read.table('Final/fertility/tfrRR.txt',sep='', skip = 2, header=TRUE)
fertility2 <- read_csv("Final/fertility/API_SP.DYN.TFRT.IN_DS2_en_csv_v2_4700605.csv")

##change the structure of fertility data
fertility2 <- filter(fertility2, `Country Code` %in% c("AUS","BEL","DNK","IRL","NZL","KOR"))
fertility <- filter(fertility, !Code %in% c("AUS","BEL","DNK","IRL","NZL","KOR"))

##change wide data to long data
fertility2 <- fertility2 %>% 
  pivot_longer(
    cols = `1960`:`2020`, 
    names_to = "year",
    values_to = "value"
  )

##select the data I need
fertility2 <- dplyr::select(fertility2, `Country Code`, year, value)
##add another col so that I can bind the data from world bank with data from HFD
fertility2$tfr40 <- NA
##change the col name so that I can bind the data
colnames(fertility2) <- colnames(fertility)

###get the final fertility data
fertility <- rbind(fertility,fertility2)

###recode the country code of some counrty so that I can combine the fertility and gender dataset
#United Kingdom fertility data refers to England & Wales from the HFD
fertility$Code[fertility$Code=="GBRTENW" ] <- "GBR"

#France fertility data refers to France excluding overseas territories
fertility$Code[fertility$Code=="FRATNP" ] <- "FRA"

#Germany fertility data and equality index refers to western Germany until unification, then all of Germany
fertility$Code[fertility$Code=="DEUTNP" & fertility$Year>1990] <- "DEU"
fertility$Code[fertility$Code=="DEUTW" & fertility$Year<=1990] <- "DEU"

#########combine the data from fertility and gender
##clean the col name
colnames(gender) <- c("country_name", "code", "year", "WPEI")
colnames(fertility) <- c("code", "year", "TFR", "TFR40")

##combine the data
data <- merge(gender,fertility,by=c("code","year"))

##create a new variable about the year category
data$yearcat <- 0
data$yearcat[data$year<1970 ] <- "1960s and earlier"
data$yearcat[data$year>=1970&data$year<1980] <- "1970s"
data$yearcat[data$year>=1980&data$year<1990] <- "1980s"
data$yearcat[data$year>=1990&data$year<2000] <- "1990s"
data$yearcat[data$year>=2000&data$year<2010] <- "2000s"
data$yearcat[data$year>=2010] <- "2010s"
data$yearcat <- as.factor(data$yearcat)

##create a new variable about the gender category
data$WPEIcat <- 0
data$WPEIcat[data$WPEI<0.25 ] <- "0-0.25"
data$WPEIcat[data$WPEI>=0.25&data$WPEI<0.5] <- "0.25-0.5"
data$WPEIcat[data$WPEI>=0.5&data$WPEI<0.7] <- "0.5-0.7"
data$WPEIcat[data$WPEI>=0.7&data$WPEI<0.8] <- "0.7-0.8"
data$WPEIcat[data$WPEI>=0.8&data$WPEI<0.9] <- "0.8-0.9"
data$WPEIcat[data$WPEI>=0.9&data$WPEI<0.95] <- "0.9-0.95"
data$WPEIcat[data$WPEI>=0.95] <- "above 0.95"
data<- filter(data, WPEIcat!=0)
data$WPEIcat <- as.factor(data$WPEIcat)

##delete one country from the data set as it is not included in the original paper
data <- filter(data, country_name!="Croatia")

########################################   Create Figure one  ###################################################
##Figure 1.1
p1 <- ggplot(data, aes(x=WPEI, y=TFR, shape=yearcat, color=yearcat)) + 
  geom_point(size=2, alpha=0.7) +
  theme_bw() + 
  scale_shape_manual(values = rep(15:17, len = 7))+
  scale_x_continuous(name="Women's political empowerment index", limits=c(0.2, 1),breaks =seq(0.2, 1, by = 0.2))+
  scale_y_continuous(name="TFR", limits=c(1, 6.2),breaks =seq(1, 6.2, by = 1))+
  theme(legend.position="bottom")+ 
  labs(shape='Period', colour="Period")+ 
  scale_color_brewer(palette = "Dark2")+ 
  guides(color = guide_legend(title.position = "top",title.hjust = 0.5), 
         shape = guide_legend(title.position = "top",title.hjust = 0.5))+
  theme(text = element_text(family = "Times New Roman"))+
  ggrepel::geom_text_repel(data = subset(data, WPEI > 0.3 & WPEI < 0.37),
                           aes(label = c("Spain(1937)", "Spain(1938)","Spain(1939)")), 
                           color="black", family="Times New Roman",box.padding = 0.8)
p1

##Figure 1.2
data2 <- filter(data, yearcat=="2000s"| yearcat=="2010s")
p2 <- ggplot(data2, aes(x=WPEI, y=TFR, shape=yearcat, color=yearcat)) + 
  geom_point(size=2, alpha=0.7) +
  theme_bw() + 
  scale_shape_manual(values = rep(15:17, len = 7))+
  scale_x_continuous(name="Women's political empowerment index", limits=c(0.7, 0.975),breaks =seq(0.7, 0.975, by = 0.05))+
  scale_y_continuous(name="TFR", limits=c(0.8, 2.5),breaks =seq(1, 2.5, by = 0.5))+
  theme(legend.position="bottom")+ 
  labs(shape='Period', colour="Period")+ 
  scale_color_brewer(palette = "Dark2")+ 
  guides(color = guide_legend(title.position = "top",title.hjust = 0.5), 
         shape = guide_legend(title.position = "top",title.hjust = 0.5))+
  theme(text = element_text(family = "Times New Roman"))
p2

figure1 <- ggarrange(p1, p2,
                    ncol = 1, nrow = 2)

##Figure 2
p3 <- ggplot(data, aes(x=year, y=TFR, shape=WPEIcat, color=WPEIcat)) + 
  geom_point(size=1.3, alpha=0.7)+
  facet_wrap(~country_name, ncol = 5)+
  theme_minimal()+
  scale_shape_manual(values = rep(15:17, len = 8))+
  theme(legend.position="bottom")+
  scale_x_continuous(name="Year", limits=c(1900, 2020),breaks =seq(1900, 2020, by = 20))+
  scale_y_continuous(name="TFR", limits=c(0.8, 4),breaks =seq(1, 4, by = 1))+
  labs(shape="Women's political empowerment index", colour="Women's political empowerment index")+ 
  scale_color_brewer(palette = "Dark2")+
  theme(axis.text.x=element_text(size=5),axis.text.y=element_text(size=5),strip.text = element_text(size = 7))+
  theme(axis.line = element_line(colour = "black",size = 0.7, linetype = "solid"))+ 
  guides(color = guide_legend(title.position = "top",title.hjust = 0.5), 
         shape = guide_legend(title.position = "top",title.hjust = 0.5))+
  theme(text = element_text(family = "Times New Roman"))

#############################################   Regression  ###################################################
###build the regression model
#set the reference group
data <- within(data, WPEIcat <- relevel(WPEIcat, ref = 4))

#build the model
m1 <- lm(TFR~WPEIcat, data = data)

m2 <- lm(TFR~WPEIcat+year, data = data)

m3 <- lm(TFR~WPEIcat+year, data = filter(data, year>1989))

m4 <- lm(TFR~WPEIcat+country_name, data = data) 

m5 <- lm(TFR~WPEIcat+country_name+year, data = data) 

m6 <- lm(TFR~WPEIcat+country_name+year, data = filter(data, year>1989))

###I will try to build a data frame which include all the coefficients in the regression model
#first I will select the coefficient from each model
model1 <- as.data.frame(summary(m1)$coefficients)
model1<- model1[2:7,]
model1$model <- 1
model1 <- setDT(model1, keep.rownames = TRUE)[]

model2 <- as.data.frame(summary(m2)$coefficients)
model2<- model2[2:7,]
model2$model <- 2
model2 <- setDT(model2, keep.rownames = TRUE)[]

model3 <- as.data.frame(summary(m3)$coefficients)
model3<- model3[2:5,]
model3$model <- 3
model3 <- setDT(model3, keep.rownames = TRUE)[]
##create a empty row
m3add <- data.frame("rn" = c("WPEIcat0-0.25", "WPEIcat0.25-0.5"),
                 "Estimate" = c(0,0),
                 "Std. Error" = c(0,0),
                 "t value"=c(0,0),
                 "Pr(>|t|)"=c(0,0),
                 "model"=c(3,3))
colnames(m3add) <- colnames(model3)
model3 <- rbind(m3add,model3)

model4 <- as.data.frame(summary(m4)$coefficients)
model4<- model4[2:7,]
model4$model <- 4
model4 <- setDT(model4, keep.rownames = TRUE)[]

model5 <- as.data.frame(summary(m5)$coefficients)
model5<- model5[2:7,]
model5$model <- 5
model5 <- setDT(model5, keep.rownames = TRUE)[]

model6 <- as.data.frame(summary(m6)$coefficients)
model6<- model6[2:5,]
model6$model <- 6
model6 <- setDT(model6, keep.rownames = TRUE)[]
##create a empty row
m6add <- data.frame("rn" = c("WPEIcat0-0.25", "WPEIcat0.25-0.5"),
                    "Estimate" = c(0,0),
                    "Std. Error" = c(0,0),
                    "t value"=c(0,0),
                    "Pr(>|t|)"=c(0,0),
                    "model"=c(6,6))
colnames(m6add) <- colnames(model6)
model6 <- rbind(m6add,model6)

#then I will combine the data into one dataframe
regression <- rbind(model1,model2,model3,model4,model5,model6)
regression$model <- as.factor(regression$model)
regression <- setDT(regression, keep.rownames = TRUE)[]
colnames(regression) <- c("WPEI","Estimate","Std. Error","t value","Pr(>|t|)","model")

#change the name of category 
regression$WPEI[regression$WPEI=="WPEIcat0-0.25"] <- "WPEI<0.25"
regression$WPEI[regression$WPEI=="WPEIcat0.25-0.5"] <- "WPEI 0.25-0.5"
regression$WPEI[regression$WPEI=="WPEIcat0.5-0.7"] <- "WPEI 0.5-0.7"
regression$WPEI[regression$WPEI=="WPEIcat0.8-0.9"] <- "WPEI 0.8-0.9"
regression$WPEI[regression$WPEI=="WPEIcat0.9-0.95"] <- "WPEI 0.9-0.95"
regression$WPEI[regression$WPEI=="WPEIcatabove 0.95"] <- "WPEI above 0.95"

regression$WPEI <- factor(regression$WPEI, levels = c("WPEI<0.25", "WPEI 0.25-0.5", "WPEI 0.5-0.7",
                                                      "WPEI 0.8-0.9","WPEI 0.9-0.95","WPEI above 0.95"))

#visualize the result
p4 <- ggplot(regression, aes(fill=WPEI, y=Estimate, x=model)) + 
  geom_bar(position="dodge", stat="identity")+
  theme_hc()+ 
  scale_x_discrete(labels=c('Mdoel 1.1\nAll data\n \n \n ', 'Model 1.2\nBetween-country\n \n \nYear', 'Model 1.3\nBetween-country\n \n \nYear, after 1990',
                            "Model 1.4\nWithin-country\n \n \nCountry","Model 1.5\nWithin-country\n \n \nCountry and year",
                            "Model 1.6\nWithin-country\n \nCountry and\nyear, after 1990"),position="top",name="")+
  scale_y_continuous(name="Number of children", limits=c(-0.80, 1.40),breaks =seq(-0.80, 1.40, by = 0.2))+
  theme(text = element_text(family = "Times New Roman"),axis.text.x=element_text(size=15),strip.text = element_text(size = 7))+ 
  labs(fill='') + 
  geom_hline(aes(yintercept=0,linetype = "Reference: \nWPEI 0.7-0.8"),size=1, colour="black")+                                                                                   
  guides(fill = guide_legend(override.aes = list(size = 3),nrow = 1, byrow = T))+                                                                                                        
  scale_linetype_manual(name = "", values = c(2, 2))+
  theme(legend.text=element_text(size=12))

######################################################################################################################################################
######################################################################################################################################################

#############################################          Sensitivity analysis         ###################################################################################

######################################################################################################################################################
######################################################################################################################################################

#################################       sensitivity analysis one  ###############################################
###build the regression model
##create a new variable about the gender category for the sensitivity analysis
data$WPEIcat2 <- 0
data$WPEIcat2[data$WPEI<0.4 ] <- "0-0.4"
data$WPEIcat2[data$WPEI>=0.4&data$WPEI<0.5] <- "0.4-0.5"
data$WPEIcat2[data$WPEI>=0.5&data$WPEI<0.6] <- "0.5-0.6"
data$WPEIcat2[data$WPEI>=0.6&data$WPEI<0.7] <- "0.6-0.7"
data$WPEIcat2[data$WPEI>=0.7&data$WPEI<0.8] <- "0.7-0.8"
data$WPEIcat2[data$WPEI>=0.8&data$WPEI<0.85] <- "0.8-0.85"
data$WPEIcat2[data$WPEI>=0.85] <- "above 0.85"
data<- filter(data, WPEIcat2!=0)
data$WPEIcat2 <- as.factor(data$WPEIcat2)

#set the reference group
data <- within(data, WPEIcat2 <- relevel(WPEIcat2, ref = 4))

#build the model
m1 <- lm(TFR~WPEIcat2, data = data)

m2 <- lm(TFR~WPEIcat2+year, data = data)

m3 <- lm(TFR~WPEIcat2+year, data = filter(data, year>1989))

m4 <- lm(TFR~WPEIcat2+country_name, data = data) 

m5 <- lm(TFR~WPEIcat2+country_name+year, data = data) 

m6 <- lm(TFR~WPEIcat2+country_name+year, data = filter(data, year>1989))

###I will try to build a data frame which include all the coefficients in the regression model
#first I will select the coefficient from each model
model1 <- as.data.frame(summary(m1)$coefficients)
model1<- model1[2:7,]
model1$model <- 1
model1 <- setDT(model1, keep.rownames = TRUE)[]

model2 <- as.data.frame(summary(m2)$coefficients)
model2<- model2[2:7,]
model2$model <- 2
model2 <- setDT(model2, keep.rownames = TRUE)[]

model3 <- as.data.frame(summary(m3)$coefficients)
model3<- model3[2:5,]
model3$model <- 3
model3 <- setDT(model3, keep.rownames = TRUE)[]
##create a empty row
m3add <- data.frame("rn" = c("WPEI<0.4", "WPEI 0.4-0.5"),
                    "Estimate" = c(0,0),
                    "Std. Error" = c(0,0),
                    "t value"=c(0,0),
                    "Pr(>|t|)"=c(0,0),
                    "model"=c(3,3))
colnames(m3add) <- colnames(model3)
model3 <- rbind(m3add,model3)

model4 <- as.data.frame(summary(m4)$coefficients)
model4<- model4[2:7,]
model4$model <- 4
model4 <- setDT(model4, keep.rownames = TRUE)[]

model5 <- as.data.frame(summary(m5)$coefficients)
model5<- model5[2:7,]
model5$model <- 5
model5 <- setDT(model5, keep.rownames = TRUE)[]

model6 <- as.data.frame(summary(m6)$coefficients)
model6<- model6[2:5,]
model6$model <- 6
model6 <- setDT(model6, keep.rownames = TRUE)[]
##create a empty row
m6add <- data.frame("rn" = c("WPEI<0.4", "WPEI 0.4-0.5"),
                    "Estimate" = c(0,0),
                    "Std. Error" = c(0,0),
                    "t value"=c(0,0),
                    "Pr(>|t|)"=c(0,0),
                    "model"=c(6,6))
colnames(m6add) <- colnames(model6)
model6 <- rbind(m6add,model6)

#then I will combine the data into one dataframe
regression <- rbind(model1,model2,model3,model4,model5,model6)
regression$model <- as.factor(regression$model)
regression <- setDT(regression, keep.rownames = TRUE)[]
colnames(regression) <- c("WPEI","Estimate","Std. Error","t value","Pr(>|t|)","model")

#change the name of category 
regression$WPEI[regression$WPEI=="WPEIcat20-0.4"] <- "WPEI<0.4"
regression$WPEI[regression$WPEI=="WPEIcat20.4-0.5"] <- "WPEI 0.4-0.5"
regression$WPEI[regression$WPEI=="WPEIcat20.5-0.6"] <- "WPEI 0.5-0.6"
regression$WPEI[regression$WPEI=="WPEIcat20.7-0.8"] <- "WPEI 0.7-0.8"
regression$WPEI[regression$WPEI=="WPEIcat20.8-0.85"] <- "WPEI 0.8-0.85"
regression$WPEI[regression$WPEI=="WPEIcat2above 0.85"] <- "WPEI above 0.85"

regression$WPEI <- factor(regression$WPEI, levels = c("WPEI<0.4", "WPEI 0.4-0.5", "WPEI 0.5-0.6",
                                                      "WPEI 0.7-0.8","WPEI 0.8-0.85","WPEI above 0.85"))

#visualize the result
pA <- ggplot(regression, aes(fill=WPEI, y=Estimate, x=model)) + 
  geom_bar(position="dodge", stat="identity")+
  theme_hc()+ 
  scale_x_discrete(labels=c('Mdoel A.1\nAll data\n \n \n ', 'Model A.2\nBetween-country\n \n \nYear', 'Model A.3\nBetween-country\n \n \nYear, after 1990',
                            "Model A.4\nWithin-country\n \n \nCountry","Model A.5\nWithin-country\n \n \nCountry and year",
                            "Model A.6\nWithin-country\n \nCountry and\nyear, after 1990"),position="top",name="")+
  scale_y_continuous(name="Number of children", limits=c(-1.1, 0.6),breaks =seq(-1.1, 0.6, by = 0.2))+
  theme(text = element_text(family = "Times New Roman"),axis.text.x=element_text(size=15),strip.text = element_text(size = 7))+ 
  labs(fill='') + 
  geom_hline(aes(yintercept=0,linetype = "Reference: \nWPEI 0.6-0.7"),size=1, colour="black")+                                                                                   
  guides(fill = guide_legend(override.aes = list(size = 3),nrow = 1, byrow = T))+                                                                                                        
  scale_linetype_manual(name = "", values = c(2, 2))+
  theme(legend.text=element_text(size=12))+ 
  geom_errorbar(aes(ymin=Estimate-1.96*`Std. Error`, ymax=Estimate+1.96*`Std. Error`), width=0.3,position = position_dodge(0.9),size=1)


#################################       sensitivity analysis two  ###############################################
###build the regression model
##create a new variable about the gender category for the sensitivity analysis
data$WPEIcat3 <- 0
data$WPEIcat3[data$WPEI<0.5 ] <- "0-0.5"
data$WPEIcat3[data$WPEI>=0.5&data$WPEI<0.6] <- "0.5-0.6"
data$WPEIcat3[data$WPEI>=0.6&data$WPEI<0.7] <- "0.6-0.7"
data$WPEIcat3[data$WPEI>=0.7&data$WPEI<0.8] <- "0.7-0.8"
data$WPEIcat3[data$WPEI>=0.8&data$WPEI<0.85] <- "0.8-0.85"
data$WPEIcat3[data$WPEI>=0.85&data$WPEI<0.9] <- "0.85-0.9"
data$WPEIcat3[data$WPEI>=0.9] <- "above 0.9"
data<- filter(data, WPEIcat3!=0)
data$WPEIcat3 <- as.factor(data$WPEIcat3)

#set the reference group
data <- within(data, WPEIcat3 <- relevel(WPEIcat3, ref = 4))

#build the model
m1 <- lm(TFR~WPEIcat3, data = data)

m2 <- lm(TFR~WPEIcat3+year, data = data)

m3 <- lm(TFR~WPEIcat3+year, data = filter(data, year>1989))

m4 <- lm(TFR~WPEIcat3+country_name, data = data) 

m5 <- lm(TFR~WPEIcat3+country_name+year, data = data) 

m6 <- lm(TFR~WPEIcat3+country_name+year, data = filter(data, year>1989))

###I will try to build a data frame which include all the coefficients in the regression model
#first I will select the coefficient from each model
model1 <- as.data.frame(summary(m1)$coefficients)
model1<- model1[2:7,]
model1$model <- 1
model1 <- setDT(model1, keep.rownames = TRUE)[]

model2 <- as.data.frame(summary(m2)$coefficients)
model2<- model2[2:7,]
model2$model <- 2
model2 <- setDT(model2, keep.rownames = TRUE)[]

model3 <- as.data.frame(summary(m3)$coefficients)
model3<- model3[2:5,]
model3$model <- 3
model3 <- setDT(model3, keep.rownames = TRUE)[]
##create a empty row
m3add <- data.frame("rn" = c("WPEI<0.5", "WPEI 0.5-0.6"),
                    "Estimate" = c(0,0),
                    "Std. Error" = c(0,0),
                    "t value"=c(0,0),
                    "Pr(>|t|)"=c(0,0),
                    "model"=c(3,3))
colnames(m3add) <- colnames(model3)
model3 <- rbind(m3add,model3)

model4 <- as.data.frame(summary(m4)$coefficients)
model4<- model4[2:7,]
model4$model <- 4
model4 <- setDT(model4, keep.rownames = TRUE)[]

model5 <- as.data.frame(summary(m5)$coefficients)
model5<- model5[2:7,]
model5$model <- 5
model5 <- setDT(model5, keep.rownames = TRUE)[]

model6 <- as.data.frame(summary(m6)$coefficients)
model6<- model6[2:5,]
model6$model <- 6
model6 <- setDT(model6, keep.rownames = TRUE)[]
##create a empty row
m6add <- data.frame("rn" = c("WPEI<0.5", "WPEI 0.5-0.6"),
                    "Estimate" = c(0,0),
                    "Std. Error" = c(0,0),
                    "t value"=c(0,0),
                    "Pr(>|t|)"=c(0,0),
                    "model"=c(6,6))
colnames(m6add) <- colnames(model6)
model6 <- rbind(m6add,model6)

#then I will combine the data into one dataframe
regression <- rbind(model1,model2,model3,model4,model5,model6)
regression$model <- as.factor(regression$model)
regression <- setDT(regression, keep.rownames = TRUE)[]
colnames(regression) <- c("WPEI","Estimate","Std. Error","t value","Pr(>|t|)","model")

#change the name of category 
regression$WPEI[regression$WPEI=="WPEIcat30-0.5"] <- "WPEI<0.5"
regression$WPEI[regression$WPEI=="WPEIcat30.5-0.6"] <- "WPEI 0.5-0.6"
regression$WPEI[regression$WPEI=="WPEIcat30.6-0.7"] <- "WPEI 0.6-0.7"
regression$WPEI[regression$WPEI=="WPEIcat30.8-0.85"] <- "WPEI 0.8-0.85"
regression$WPEI[regression$WPEI=="WPEIcat30.85-0.9"] <- "WPEI 0.85-0.9"
regression$WPEI[regression$WPEI=="WPEIcat3above 0.9"] <- "WPEI above 0.9"

regression$WPEI <- factor(regression$WPEI, levels = c("WPEI<0.5", "WPEI 0.5-0.6", "WPEI 0.6-0.7",
                                                      "WPEI 0.8-0.85","WPEI 0.85-0.9","WPEI above 0.9"))

#visualize the result
pB <- ggplot(regression, aes(fill=WPEI, y=Estimate, x=model)) + 
  geom_bar(position="dodge", stat="identity")+
  theme_hc()+ 
  scale_x_discrete(labels=c('Mdoel B.1\nAll data\n \n \n ', 'Model B.2\nBetween-country\n \n \nYear', 'Model B.3\nBetween-country\n \n \nYear, after 1990',
                            "Model B.4\nWithin-country\n \n \nCountry","Model B.5\nWithin-country\n \n \nCountry and year",
                            "Model B.6\nWithin-country\n \nCountry and\nyear, after 1990"),position="top",name="")+
  scale_y_continuous(name="Number of children", limits=c(-0.7, 1.2),breaks =seq(-0.7, 1.2, by = 0.2))+
  theme(text = element_text(family = "Times New Roman"),axis.text.x=element_text(size=15),strip.text = element_text(size = 7))+ 
  labs(fill='') + 
  geom_hline(aes(yintercept=0,linetype = "Reference: \nWPEI 0.7-0.8"),size=1, colour="black")+                                                                                   
  guides(fill = guide_legend(override.aes = list(size = 3),nrow = 1, byrow = T))+                                                                                                        
  scale_linetype_manual(name = "", values = c(2, 2))+
  theme(legend.text=element_text(size=12))+ 
  geom_errorbar(aes(ymin=Estimate-1.96*`Std. Error`, ymax=Estimate+1.96*`Std. Error`), width=0.3,position = position_dodge(0.9),size=1)

##################################        descriptive statistics   ##############################################
g1 <- ggplot(data=data, aes(x=WPEIcat)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)),fill="black",alpha=0.6,width=0.5)+
  theme_hc()+
  theme(text = element_text(family = "Times New Roman"),axis.text.x=element_text(size=15),strip.text = element_text(size = 7))+
  scale_y_continuous(name="Percentage")+
  scale_x_discrete(name="WPEI category")+ 
  ggtitle("Kolk’s (2019) original set of WPEI thresholds")
  
  
g2 <- ggplot(data=data, aes(x=WPEIcat2)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)),fill="skyblue",alpha=0.6,width=0.5)+
  theme_hc()+
  theme(text = element_text(family = "Times New Roman"),axis.text.x=element_text(size=15),strip.text = element_text(size = 7))+
  scale_y_continuous(name="Percentage")+
  scale_x_discrete(name="WPEI category")+ 
  ggtitle("WPEI thresholds set A")

g3 <- ggplot(data=data, aes(x=WPEIcat3)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)),fill="#F8766D",alpha=0.6,width=0.5)+
  theme_hc()+
  theme(text = element_text(family = "Times New Roman"),axis.text.x=element_text(size=15),strip.text = element_text(size = 7))+
  scale_y_continuous(name="Percentage")+ 
  ggtitle("WPEI thresholds set B")

figureA <- ggarrange(g1, g2, g3,
                     ncol = 1, nrow = 3)

#################################       sensitivity analysis three  ###############################################
#build the model
m1 <- lm(TFR~WPEIcat, data = data)

m2 <- lm(TFR~WPEIcat+year, data = data)

m3 <- lm(TFR~WPEIcat+year, data = filter(data, year>1999))

m4 <- lm(TFR~WPEIcat+country_name, data = data) 

m5 <- lm(TFR~WPEIcat+country_name+year, data = data) 

m6 <- lm(TFR~WPEIcat+country_name+year, data = filter(data, year>1999))

###I will try to build a data frame which include all the coefficients in the regression model
#first I will select the coefficient from each model
model1 <- as.data.frame(summary(m1)$coefficients)
model1<- model1[2:7,]
model1$model <- 1
model1 <- setDT(model1, keep.rownames = TRUE)[]

model2 <- as.data.frame(summary(m2)$coefficients)
model2<- model2[2:7,]
model2$model <- 2
model2 <- setDT(model2, keep.rownames = TRUE)[]

model3 <- as.data.frame(summary(m3)$coefficients)
model3<- model3[2:5,]
model3$model <- 3
model3 <- setDT(model3, keep.rownames = TRUE)[]
##create a empty row
m3add <- data.frame("rn" = c("WPEIcat0-0.25", "WPEIcat0.25-0.5"),
                    "Estimate" = c(0,0),
                    "Std. Error" = c(0,0),
                    "t value"=c(0,0),
                    "Pr(>|t|)"=c(0,0),
                    "model"=c(3,3))
colnames(m3add) <- colnames(model3)
model3 <- rbind(m3add,model3)

model4 <- as.data.frame(summary(m4)$coefficients)
model4<- model4[2:7,]
model4$model <- 4
model4 <- setDT(model4, keep.rownames = TRUE)[]

model5 <- as.data.frame(summary(m5)$coefficients)
model5<- model5[2:7,]
model5$model <- 5
model5 <- setDT(model5, keep.rownames = TRUE)[]

model6 <- as.data.frame(summary(m6)$coefficients)
model6<- model6[2:5,]
model6$model <- 6
model6 <- setDT(model6, keep.rownames = TRUE)[]
##create a empty row
m6add <- data.frame("rn" = c("WPEIcat0-0.25", "WPEIcat0.25-0.5"),
                    "Estimate" = c(0,0),
                    "Std. Error" = c(0,0),
                    "t value"=c(0,0),
                    "Pr(>|t|)"=c(0,0),
                    "model"=c(6,6))
colnames(m6add) <- colnames(model6)
model6 <- rbind(m6add,model6)

#then I will combine the data into one dataframe
regression <- rbind(model1,model2,model3,model4,model5,model6)
regression$model <- as.factor(regression$model)
regression <- setDT(regression, keep.rownames = TRUE)[]
colnames(regression) <- c("WPEI","Estimate","Std. Error","t value","Pr(>|t|)","model")

#change the name of category 
regression$WPEI[regression$WPEI=="WPEIcat0-0.25"] <- "WPEI<0.25"
regression$WPEI[regression$WPEI=="WPEIcat0.25-0.5"] <- "WPEI 0.25-0.5"
regression$WPEI[regression$WPEI=="WPEIcat0.5-0.7"] <- "WPEI 0.5-0.7"
regression$WPEI[regression$WPEI=="WPEIcat0.8-0.9"] <- "WPEI 0.8-0.9"
regression$WPEI[regression$WPEI=="WPEIcat0.9-0.95"] <- "WPEI 0.9-0.95"
regression$WPEI[regression$WPEI=="WPEIcatabove 0.95"] <- "WPEI above 0.95"

regression$WPEI <- factor(regression$WPEI, levels = c("WPEI<0.25", "WPEI 0.25-0.5", "WPEI 0.5-0.7",
                                                      "WPEI 0.8-0.9","WPEI 0.9-0.95","WPEI above 0.95"))

#visualize the result
pC <- ggplot(regression, aes(fill=WPEI, y=Estimate, x=model)) + 
  geom_bar(position="dodge", stat="identity")+
  theme_hc()+ 
  scale_x_discrete(labels=c('Mdoel C.1\nAll data\n \n \n ', 'Model C.2\nBetween-country\n \n \nYear', 'Model C.3\nBetween-country\n \n \nYear, after 2000',
                            "Model C.4\nWithin-country\n \n \nCountry","Model C.5\nWithin-country\n \n \nCountry and year",
                            "Model C.6\nWithin-country\n \nCountry and\nyear, after 2000"),position="top",name="")+
  scale_y_continuous(name="Number of children", limits=c(-0.80, 1.40),breaks =seq(-0.80, 1.40, by = 0.2))+
  theme(text = element_text(family = "Times New Roman"),axis.text.x=element_text(size=15),strip.text = element_text(size = 7))+ 
  labs(fill='') + 
  geom_hline(aes(yintercept=0,linetype = "Reference: \nWPEI 0.7-0.8"),size=1, colour="black")+                                                                                   
  guides(fill = guide_legend(override.aes = list(size = 3),nrow = 1, byrow = T))+                                                                                                        
  scale_linetype_manual(name = "", values = c(2, 2))+
  theme(legend.text=element_text(size=12))+ 
  geom_errorbar(aes(ymin=Estimate-1.96*`Std. Error`, ymax=Estimate+1.96*`Std. Error`), width=0.3,position = position_dodge(0.9),size=1)

######################################################################################################################################################
######################################################################################################################################################

#############################################          Extension with HGEI         ###################################################################################

######################################################################################################################################################
######################################################################################################################################################

#####load the HGEI dataset
newgender <- read_excel("Final/Gender equality/HistoricalGenderEqualityIndex_Broad.xlsx")
newgender <- select(newgender,`country name`, `1950`:`2003`)

#change the data from wide to long
newgender <- newgender %>% 
  pivot_longer(
    cols = `1950`:`2003`, 
    names_to = "year",
    values_to = "HGEI"
  )

#change the country name to country code using R
newgender$code <- countrycode(newgender$`country name`, origin = 'country.name', destination = 'iso3c')

#combine the new indicator to the original dataset
dataHGEI <- merge(data,newgender,by=c("code","year"))

###plot the figure
##Figure 2.1
p5 <- ggplot(dataHGEI, aes(x=HGEI, y=TFR, shape=yearcat, color=yearcat)) + 
  geom_point(size=2, alpha=0.7) +
  theme_bw(base_size = 16) +  # Set base size for theme
  scale_shape_manual(values = rep(15:17, len = 7)) +
  scale_x_continuous(
    name="Historical Gender Equality Index", 
    limits=c(54, 94),
    breaks=seq(54, 94, by=10)
  ) +
  scale_y_continuous(
    name="TFR", 
    limits=c(1, 6.2),
    breaks=seq(1, 6.2, by=1)
  ) +
  theme(
    legend.position="bottom",
    axis.title = element_text(size = 16),  # Larger axis titles
    axis.text = element_text(size = 16),  # Larger axis text
    legend.title = element_text(size = 16),  # Larger legend title
    legend.text = element_text(size = 16),  # Larger legend text
    plot.title = element_text(size = 16, hjust = 0.5)  # Larger plot title (centered)
  ) + 
  labs(shape='Period', colour="Period") + 
  scale_color_brewer(palette = "Dark2") + 
  guides(
    color = guide_legend(title.position = "top", title.hjust = 0.5), 
    shape = guide_legend(title.position = "top", title.hjust = 0.5)
  )


#Figure 2.3
##filter the data
##create a new variable about the HGEI category
dataHGEI$HGEIcat <- 0
dataHGEI$HGEIcat[dataHGEI$HGEI<65 ] <- "50-65"
dataHGEI$HGEIcat[dataHGEI$HGEI>=65&dataHGEI$HGEI<70] <- "65-70"
dataHGEI$HGEIcat[dataHGEI$HGEI>=70&dataHGEI$HGEI<75] <- "70-75"
dataHGEI$HGEIcat[dataHGEI$HGEI>=75&dataHGEI$HGEI<80] <- "75-80"
dataHGEI$HGEIcat[dataHGEI$HGEI>=80] <- "above 80"
dataHGEI<- filter(dataHGEI, HGEIcat!=0)
dataHGEI$HGEIcat <- as.factor(dataHGEI$HGEIcat)

#############################################   Regression  ###################################################
###build the regression model
#set the reference group
dataHGEI <- within(dataHGEI, HGEIcat <- relevel(HGEIcat, ref = 3))

#build the model
m1 <- lm(TFR~HGEIcat, data = dataHGEI)

m2 <- lm(TFR~HGEIcat+year, data = dataHGEI)

m3 <- lm(TFR~HGEIcat+country_name, data = dataHGEI) 

m4 <- lm(TFR~HGEIcat+country_name+year, data = dataHGEI) 

###I will try to build a data frame which include all the coefficients in the regression model
#first I will select the coefficient from each model
model1 <- as.data.frame(summary(m1)$coefficients)
model1<- model1[2:5,]
model1$model <- 1
model1 <- setDT(model1, keep.rownames = TRUE)[]

model2 <- as.data.frame(summary(m2)$coefficients)
model2<- model2[2:5,]
model2$model <- 2
model2 <- setDT(model2, keep.rownames = TRUE)[]

model3 <- as.data.frame(summary(m3)$coefficients)
model3<- model3[2:5,]
model3$model <- 3
model3 <- setDT(model3, keep.rownames = TRUE)[]

model4 <- as.data.frame(summary(m4)$coefficients)
model4<- model4[2:5,]
model4$model <- 4
model4 <- setDT(model4, keep.rownames = TRUE)[]

#then I will combine the data into one dataframe
regression <- rbind(model1,model2,model3,model4)
regression$model <- as.factor(regression$model)
regression <- setDT(regression, keep.rownames = TRUE)[]
colnames(regression) <- c("HGEI","Estimate","Std. Error","t value","Pr(>|t|)","model")

#change the name of category 
regression$HGEI[regression$HGEI=="HGEIcat50-65"] <- "HGEI 50-65"
regression$HGEI[regression$HGEI=="HGEIcat65-70"] <- "HGEI 65-70"
regression$HGEI[regression$HGEI=="HGEIcat75-80"] <- "HGEI 75-80"
regression$HGEI[regression$HGEI=="HGEIcatabove 80"] <- "HGEI above 80"
regression$HGEI <- factor(regression$HGEI, levels = c("HGEI 50-65", "HGEI 65-70", "HGEI 75-80",
                                                      "HGEI above 80"))

#visualize the result
p7 <- ggplot(regression, aes(fill = HGEI, y = Estimate, x = model)) + 
  geom_bar(position = "dodge", stat = "identity") +
  theme_hc() + 
  scale_x_discrete(
    labels = c(
      'Model 1.1\nAll data\n \n \n ', 
      'Model 1.2\nBetween-country\n \n \nYear',
      "Model 1.3\nWithin-country\n \n \nCountry",
      "Model 1.4\nWithin-country\n \n \nCountry and year"
    ), 
    position = "top", 
    name = ""
  ) +
  scale_y_continuous(
    name   = "Coefficient",    # <-- changed here
    limits = c(-0.30, 1.50), 
    breaks = seq(-0.30, 1.50, by = 0.2)
  ) +
  theme(
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    strip.text   = element_text(size = 16),
    legend.text  = element_text(size = 16),
    legend.title = element_text(size = 16),
    plot.title   = element_text(size = 16, hjust = 0.5)
  ) + 
  labs(fill = "") + 
  geom_hline(
    aes(yintercept = 0, linetype = "Reference: \nHGEI 70-75"),
    size   = 1,
    colour = "black"
  ) +                                                                                   
  guides(
    fill = guide_legend(override.aes = list(size = 3), nrow = 1, byrow = TRUE)
  ) +                                                                                                        
  scale_linetype_manual(
    name   = "", 
    values = c(2, 2)
  ) +
  geom_errorbar(
    aes(
      ymin = Estimate - 1.96 * `Std. Error`, 
      ymax = Estimate + 1.96 * `Std. Error`
    ), 
    width    = 0.3,
    position = position_dodge(0.9),
    size     = 1
  )

############################################################
# 1.  Prepare data ─────────────────────────────────────────
############################################################
dataHGEI <- dataHGEI %>% 
  mutate(
    HGEI_sq      = HGEI^2,          # quadratic term
    year         = as.factor(year), # ensure factors for fixed effects
    country_name = as.factor(country_name)
  ) %>% 
  drop_na(TFR, HGEI)                # remove any missings

############################################################
# 2.  Estimate quadratic model with two-way fixed effects ─
############################################################
m_quad <- lm(
  TFR ~ HGEI + HGEI_sq + country_name + year,
  data = dataHGEI
)

# (Optional) cluster-robust variance–covariance matrix at country level
vcov_c  <- vcovCL(m_quad, cluster = ~country_name)  

# Coefficients with clustered s.e.
coeftest(m_quad, vcov = vcov_c)

############################################################
# 3.  Formal tests for a U-shape ───────────────────────────
############################################################
## 3a. Joint significance of HGEI & HGEI_sq
wald_joint <- linearHypothesis(
  m_quad, 
  c("HGEI = 0", "HGEI_sq = 0"),
  vcov = vcov_c,
  test = "F"
)
print(wald_joint)

## 3b. Sign pattern and turning point
b1 <- coef(m_quad)["HGEI"]
b2 <- coef(m_quad)["HGEI_sq"]

cat("β₁ (linear)  =", round(b1, 3), "\n")
cat("β₂ (quadratic)=", round(b2, 3), "\n")

if (b1 < 0 & b2 > 0) {
  cat("Sign pattern is consistent with a U-shape.\n")
} else {
  cat("Sign pattern is NOT consistent with a U-shape.\n")
}

# Vertex of the parabola
turning <- -b1 / (2 * b2)
cat("Estimated minimum at HGEI ≈", round(turning, 2), "\n")

# Check whether this point is inside the observed HGEI range
range_HGEI <- range(dataHGEI$HGEI, na.rm = TRUE)
cat("Observed HGEI range:", round(range_HGEI[1], 1), "to",
    round(range_HGEI[2], 1), "\n")

########################################           Descriptive statistics                     ###########################################################
g4 <- ggplot(dataHGEI, aes(x=HGEI)) + 
  geom_histogram(
    aes(y = after_stat(count / sum(count))),
    fill = "#F8766D",
    alpha = 0.85,
    binwidth = 1
  ) +
  theme_hc() +
  theme(
    axis.text.x = element_text(size = 16),  # Larger x-axis text
    axis.text.y = element_text(size = 16),  # Larger y-axis text
    axis.title.x = element_text(size = 16),  # Larger x-axis title
    axis.title.y = element_text(size = 16),  # Larger y-axis title
    strip.text = element_text(size = 16),  # Larger strip text
    plot.title = element_text(size = 16, hjust = 0.5)  # Larger plot title, centered
  ) +
  scale_y_continuous(name = "Percentage") +
  scale_x_continuous(name = "HGEI") + 
  ggtitle("HGEI Distribution") + 
  geom_vline(
    xintercept = mean(dataHGEI$HGEI),
    linetype = "dotted",
    size = 1.2
  ) + 
  geom_text(
    aes(
      x = mean(dataHGEI$HGEI) + 5,
      label = "Mean HGEI = 70.7",
      y = 0.075
    ), 
    colour = "black", 
    family = "Times New Roman",
    size = 5  # Adjust text size for visibility
  )

g5 <- ggplot(data = dataHGEI, aes(x = HGEIcat)) + 
  geom_bar(
    aes(y = (..count..) / sum(..count..)),
    fill = "skyblue",
    alpha = 0.85,
    width = 0.5
  ) +
  theme_hc() +
  theme(
    axis.text.x = element_text(size = 16),  # Larger x-axis text
    axis.text.y = element_text(size = 16),  # Larger y-axis text
    axis.title.x = element_text(size = 16),  # Larger x-axis title
    axis.title.y = element_text(size = 16),  # Larger y-axis title
    strip.text = element_text(size = 16),  # Larger strip text
    plot.title = element_text(size = 16, hjust = 0.5)  # Larger plot title, centered
  ) +
  scale_y_continuous(name = "Percentage") +
  scale_x_discrete(name = "HGEI category") + 
  ggtitle("HGEI categories distribution")


figureB <- ggarrange(g4, g5,
                     ncol = 1, nrow = 2)

##load the descriptive statistics table
describe <- read_excel("Tables/descriptive statistics.xlsx")

##change from long to wide
describe <- describe %>% 
  pivot_longer(
    cols = `Kolk‘s original paper`:`Extension with tempo-adjusted TFR`, 
    names_to = "paper",
    values_to = "value"
  )

###visualize the results
g6 <- ggplot(describe, aes(x = paper, y = value, fill = paper)) + 
  geom_bar(stat = "identity", width = 0.42) +
  facet_wrap(~Counrty, ncol = 5) +
  theme_minimal() +
  scale_y_continuous(name = "Country Year") +
  scale_x_discrete(name = "") +
  theme(
    legend.position = "bottom",
    text = element_text(size = 10),  # Base font size
    axis.text.x = element_text(size = 8),  # Larger x-axis text
    axis.text.y = element_text(size = 10),  # Larger y-axis text
    axis.title.x = element_text(size = 10),  # Larger x-axis title
    axis.title.y = element_text(size = 10),  # Larger y-axis title
    strip.text = element_text(size = 10),  # Larger facet text
    legend.text = element_text(size = 10),  # Larger legend text
    legend.title = element_text(size = 10),  # Larger legend title
    plot.title = element_text(size = 10, hjust = 0.5)  # Larger plot title, centered
  ) +
  labs(fill = "") +
  guides(
    fill = guide_legend(nrow = 2)  # Split legend into two rows
  ) +
  coord_flip()

######################################################################################################################################################
######################################################################################################################################################

#############################################          Extension with adjTFR and HGEI       ##########################################################

######################################################################################################################################################
######################################################################################################################################################
##load the adjTFR
adjTFR <- read.table('Final/fertility/adjtfrRRbo.txt',sep='', skip = 2, header=TRUE)
##rename the colums
colnames(adjTFR) <- c("code","year","adjTFR","adjTFR1","adjTFR2","adjTFR3","adjTFR4","adjTFR5p")

adjTFR$code[adjTFR$code=="GBRTENW"] <- "GBR"

##merge the adjTFR into the data
data <- merge(dataHGEI,adjTFR,by=c("code","year"))

#############################################   Regression  ###################################################
###build the regression model
#build the model
m1 <- lm(adjTFR~HGEIcat, data = data)

m2 <- lm(adjTFR~HGEIcat+year, data = data)

m3 <- lm(adjTFR~HGEIcat+country_name, data = data) 

m4 <- lm(adjTFR~HGEIcat+country_name+year, data = data) 

###I will try to build a data frame which include all the coefficients in the regression model
#first I will select the coefficient from each model
model1 <- as.data.frame(summary(m1)$coefficients)
model1<- model1[2:5,]
model1$model <- 1
model1 <- setDT(model1, keep.rownames = TRUE)[]

model2 <- as.data.frame(summary(m2)$coefficients)
model2<- model2[2:5,]
model2$model <- 2
model2 <- setDT(model2, keep.rownames = TRUE)[]

model3 <- as.data.frame(summary(m3)$coefficients)
model3<- model3[2:5,]
model3$model <- 3
model3 <- setDT(model3, keep.rownames = TRUE)[]

model4 <- as.data.frame(summary(m4)$coefficients)
model4<- model4[2:5,]
model4$model <- 4
model4 <- setDT(model4, keep.rownames = TRUE)[]

#then I will combine the data into one dataframe
regression <- rbind(model1,model2,model3,model4)
regression$model <- as.factor(regression$model)
regression <- setDT(regression, keep.rownames = TRUE)[]
colnames(regression) <- c("HGEI","Estimate","Std. Error","t value","Pr(>|t|)","model")

#change the name of category 
regression$HGEI[regression$HGEI=="HGEIcat50-65"] <- "HGEI 50-65"
regression$HGEI[regression$HGEI=="HGEIcat65-70"] <- "HGEI 65-70"
regression$HGEI[regression$HGEI=="HGEIcat75-80"] <- "HGEI 75-80"
regression$HGEI[regression$HGEI=="HGEIcatabove 80"] <- "HGEI above 80"
regression$HGEI <- factor(regression$HGEI, levels = c("HGEI 50-65", "HGEI 65-70", "HGEI 75-80",
                                                      "HGEI above 80"))

#visualize the result
p8 <- ggplot(regression, aes(fill = HGEI, y = Estimate, x = model)) + 
  geom_bar(position = "dodge", stat = "identity") +
  theme_hc() + 
  scale_x_discrete(
    labels = c(
      'Model 2.1\nAll data\n \n \n ', 
      'Model 2.2\nBetween-country\n \n \nYear',
      "Model 2.3\nWithin-country\n \n \nCountry",
      "Model 2.4\nWithin-country\n \n \nCountry and year"
    ), 
    position = "top", 
    name = ""
  ) +
  scale_y_continuous(
    name   = "Coefficient",    # <-- changed here
    limits = c(-0.20, 1.20), 
    breaks = seq(-0.20, 1.20, by = 0.2)
  ) +
  theme(
    text             = element_text(size = 16),
    axis.text.x      = element_text(size = 16),
    axis.text.y      = element_text(size = 16),
    axis.title.x     = element_text(size = 16),
    axis.title.y     = element_text(size = 16),
    strip.text       = element_text(size = 16),
    legend.text      = element_text(size = 16),
    legend.title     = element_text(size = 16),
    plot.title       = element_text(size = 16, hjust = 0.5)
  ) + 
  labs(fill = "") + 
  geom_hline(
    aes(yintercept = 0, linetype = "Reference: \nHGEI 70-75"),
    size   = 1,
    colour = "black"
  ) +                                                                                   
  guides(
    fill = guide_legend(override.aes = list(size = 3), nrow = 1, byrow = TRUE)
  ) +                                                                                                        
  scale_linetype_manual(
    name   = "", 
    values = c(2, 2)
  ) +
  geom_errorbar(
    aes(
      ymin = Estimate - 1.96 * `Std. Error`, 
      ymax = Estimate + 1.96 * `Std. Error`
    ), 
    width    = 0.3,
    position = position_dodge(0.9),
    size     = 1
  )

#####################################          descriptive statistics        ###########################################
g7 <- ggplot(data, aes(x = HGEI)) + 
  geom_histogram(
    aes(y = after_stat(count / sum(count))),
    fill = "#F8766D",
    alpha = 0.85,
    binwidth = 1
  ) +
  theme_hc() +
  theme(
    text = element_text(size = 16),  # Base text size and font
    axis.text.x = element_text(size = 16),  # Larger x-axis text
    axis.text.y = element_text(size = 16),  # Larger y-axis text
    axis.title.x = element_text(size = 16),  # Larger x-axis title
    axis.title.y = element_text(size = 16),  # Larger y-axis title
    strip.text = element_text(size = 16),  # Larger strip text
    plot.title = element_text(size = 16, hjust = 0.5)  # Larger plot title, centered
  ) +
  scale_y_continuous(name = "Percentage") +
  scale_x_continuous(name = "HGEI") + 
  ggtitle("HGEI Distribution") + 
  geom_vline(
    xintercept = mean(dataHGEI$HGEI),
    linetype = "dotted",
    size = 1.2
  ) + 
  geom_text(
    aes(
      x = mean(dataHGEI$HGEI) + 5, 
      label = "Mean HGEI = 73.30", 
      y = 0.05
    ), 
    colour = "black", 
    family = "Times New Roman",
    size = 5  # Adjust text size for visibility
  )

g8 <- ggplot(data = data, aes(x = HGEIcat)) + 
  geom_bar(
    aes(y = (..count..) / sum(..count..)),
    fill = "skyblue",
    alpha = 0.85,
    width = 0.5
  ) +
  theme_hc() +
  theme(
    text = element_text(size = 16),  # Base text size and font
    axis.text.x = element_text(size = 16),  # Larger x-axis text
    axis.text.y = element_text(size = 16),  # Larger y-axis text
    axis.title.x = element_text(size = 16),  # Larger x-axis title
    axis.title.y = element_text(size = 16),  # Larger y-axis title
    strip.text = element_text(size = 16),  # Larger strip text
    plot.title = element_text(size = 16, hjust = 0.5)  # Larger plot title, centered
  ) +
  scale_y_continuous(name = "Percentage") +
  scale_x_discrete(name = "HGEI category") + 
  ggtitle("HGEI categories distribution")


figureC <- ggarrange(g7, g8,
                     ncol = 1, nrow = 2)


###############################################################################################################
###############################################################################################################
#################################################  Appendix   #################################################
###############################################################################################################
###############################################################################################################
#load the data
appendix <- read_excel("Final/Appendix/appendix.xlsx")
appendix$dif <- appendix$`My replication`-appendix$`Kolk's Paper`
appendix$dif2 <- 0
appendix$dif2[appendix$dif==0 ] <- "0"
appendix$dif2[appendix$dif==1] <- "+1"
appendix$dif2[appendix$dif==2] <- "+2"
appendix$dif2[appendix$dif==3] <- "+3"
appendix$dif2[appendix$dif==4] <- "+4"
appendix$dif2[appendix$dif==5] <- "+5"


#turn long to wide
appendix <- appendix %>% 
  pivot_longer(
    cols = `Kolk's Paper`:`My replication`, 
    names_to = "paper",
    values_to = "value"
  )


###visualize the comparison
p9 <- ggplot(appendix, aes(x=paper, y=value, fill=paper)) + 
  geom_bar(stat="identity", width=0.42)+
  geom_point(aes(x = 1.5, y = 92, size = dif, label = dif), colour= "grey", alpha=0.8, show.legend = FALSE)+
  scale_size(range = c(6, 15), name="Difference of country years")+
  geom_text(aes(x=1.5, y=92,label=dif2),family="Times New Roman")+
  facet_wrap(~country, ncol = 5)+
  theme_minimal()+
  scale_y_continuous(name="Country Year")+
  scale_x_discrete(name="")+
  theme(text = element_text(family = "Times New Roman"),legend.position="bottom")+
  labs(fill="")


#####draw a simple U shape relationship graph
f <- function(x) 1 + x^2 -10*x
x <- seq(-10, 10)
y <- f(x)

p10 <- ggplot(data.frame(x, y), aes(x=x, y=y)) + 
  geom_line(color = "skyblue",size = 3)+
  theme_classic2()+
  scale_y_continuous(name="Total fertility rate", limits=c(-50, 250))+
  scale_x_continuous(name="Gender equality level")+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        text = element_text(family = "Times New Roman"),
        axis.title=element_text(size=25))+
  geom_hline(yintercept = 1, 
             color = "#F8766D",linetype="dashed",size = 1.2)+
  geom_text(aes(x = 7, y = 15, label = "Replacement level"),family = "Times New Roman",color = "#F8766D",size=8)


###############################################################################################################
###############################################################################################################
####################################  Save the final result   #################################################
###############################################################################################################
###############################################################################################################

#Replication
ggsave("Figure 1.png",  figure1, width = 8, height = 12)
ggsave("Figure 2.png",  p3, width = 8, height = 12)
ggsave("Figure 3.png",  p4, width = 12, height = 8)

#Extension with HGEI
ggsave("Figure 4.png",  p5, width = 8, height = 6)
ggsave("Figure 6.png",  p7, width = 12, height = 8)

#Extension with TadjTFR (HGEI)
ggsave("Figure 7.png",  p8, width = 12, height = 8)

#discriptive comparasion
ggsave("Figure 8.png",  p9, width = 10, height = 12)

#sensitivity analysis
ggsave("Figure 9.png",  pA, width = 12, height = 8)
ggsave("Figure 10.png",  pB, width = 12, height = 8)
ggsave("Figure 11.png",  pC, width = 12, height = 8)

#U-shape example
ggsave("Figure 12.png",  p10, width = 12, height = 8)

#discriptive statistics
ggsave("Figure 13.png",  figureA, width = 12, height = 10)
ggsave("Figure 14.png",  figureB, width = 8, height = 10)
ggsave("Figure 15.png",  g6, width = 8, height = 10)
ggsave("Figure 16.png",  figureC, width = 8, height = 10)


