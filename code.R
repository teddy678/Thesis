#downloading the data
perceivedMeanValues <- read.csv("C:/Desktop/perceivedMeanValues_LDCLi.csv")
personality_gender_age <- read.csv("C:/Desktop/personality_gender_age.csv")
SelfMeanValues <- read.csv("C:/Desktop/SelfMeanValues_LDCLi.csv")

#loading needed packages
library(ggplot2)       # for data visualization
library(psych)         # for describing the data
library(data.table)    # to subset some of the data
library(dplyr)         # for data wrangling
library(plyr)          # for data wrangling
library(tidyr)         # for data wrangling
library(car)           # for the Q-Q plots
library(nnet)          # for the multinomial logistic regression
library(effects)       #for visualizing the effects in the first model

#------------Research Question 1------------
#--------Preparing the data--------
#Subsetting the variables from personality_gender_age that we won't be needing
vars <- names(personality_gender_age) %in% c("Gender", "Age","PRF.MEDIA", "PRF.TOTALES")
personality <- personality_gender_age[!vars]

#get the personality means for each group
personality_groupmeans <- ddply(personality, "Group", summarize,
                                extrov = mean(EXTROV.MEDIA.NEO, na.rm=TRUE),
                                neurotic = mean(NEUROTIC.MEDIA.NEO, na.rm=TRUE),
                                openness = mean(OPENNESS.MEDIA.NEO, na.rm=TRUE),
                                agreab = mean(AGREAB.MEDIA.NEO, na.rm=TRUE),
                                consient = mean(CONSCIENT.MEDIA.NEO, na.rm=TRUE))

describe(personality_groupmeans)

#get the maximum values per group for perceived leadership
max_percLead <- setDT(perceivedMeanValues)[, .SD[which.max(percLead)], by=Group]
max_percLead <- max_percLead[,1:3]

#Get a dataframe containing the personality trait scores for the leaders
#First rename the column "Letteringroup" in the personality df, so it matches the "Participant" column in max_percLead
names(personality)[3] <- "Participant"
leader_personality <- full_join(max_percLead, personality)

#Remove rows with NA values for the percLead, so that we are left with only the personalities for the perceived leaders
leader_personality <- leader_personality[!is.na(leader_personality$percLead),]
#Remove rows with no information about the personality of the leaders (2 mistakes in the notation of the original data)
leader_personality <- leader_personality[!is.na(leader_personality$EXTROV.MEDIA.NEO),]
names(leader_personality)[5:9] <- c("extrovL", "neuroticL", "opennessL", "agreabL", "consientL")

#drop the values for groups 15 and 17 as we don't have the data for them in leader_personality --> 38 observations left
personality_groupmeans <- personality_groupmeans[-c(15, 17),]

#create a table containing only the leader personalities, so that we can extract the score and name of the max ones
#group number and id letter of the participant would interfere, so we need a table without them
max_leader_traits <- leader_personality[,5:9]
#take the maximum score per row, which indicates the highest score of the personality
max_leader_traits$max_leader_value = apply(max_leader_traits[,-1], 1, max)
#take the names of the columns containing the highest value per row - the trait with highest score
max_leader_traits$max_leader_trait = colnames(max_leader_traits)[apply(max_leader_traits,1,which.max)]
#add the group numbering, from the leader_personality table, and the id letter of the leader
max_leader_traits <- cbind(leader_personality$Group,leader_personality$Participant, max_leader_traits)
names(max_leader_traits)[1] <- "Group"
names(max_leader_traits)[2] <- "Leader"

#keep only the columns we need
max_leader_traits <- max_leader_traits[,c("Group", "Leader", "max_leader_value", "max_leader_trait")]

describe(leader_personality)
describe(max_leader_traits)

#do the same for the group personlity table and merge everything in 1 table for the first research question
max_group_traits <- personality_groupmeans[,2:6]
max_group_traits$max_group_value = apply(max_group_traits[,-1], 1, max)
max_group_traits$max_group_trait = colnames(max_group_traits)[apply(max_group_traits,1,which.max)]
max_group_traits <- cbind(personality_groupmeans$Group, max_group_traits)
names(max_group_traits)[1] <- "Group"

RQ1_data <- merge(max_group_traits, max_leader_traits, by = "Group")

#adding the amount of group members per group
personality <- subset(personality, Group != "15" & Group != "17")
NumofPartic <- personality %>% count("Group")

RQ1_data <- merge(NumofPartic, RQ1_data, by = "Group")
names(RQ1_data)[2] <- "NumPartic"

#Setting up the Multinomial Logistic Regression Model
str(RQ1_data)
with(RQ1_data, table(max_group_trait, max_leader_trait))

#count amount of times group/leader traits appear in the data
table(RQ1_data$max_group_trait)
table(RQ1_data$max_leader_trait)

with(RQ1_data, do.call(rbind, tapply(NumPartic, max_leader_trait, function(x) c(M = mean(x), SD = sd(x)))))

RQ1_data$max_leader_trait.f <- factor(RQ1_data$max_leader_trait)
RQ1_data$max_leader_trait.f <- relevel(RQ1_data$max_leader_trait.f, ref = "agreabL")

mn_model <- multinom(max_leader_trait.f ~ extrov + neurotic + openness + agreab + consient, data = RQ1_data)

summary(mn_model)

#Missclassification error
cm <- table(predict(mn_model), RQ1_data$max_leader_trait.f)
cm #x - real values, y - prediction values
1-sum(diag(cm))/sum(cm) # amount of times model makes a missclassification
round((sum(diag(cm))/sum(cm))*100,2) #calculating accuracy 

z <- summary(mn_model)$coefficients/summary(mn_model)$standard.errors
z

p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

summary(mn_model)$coefficients
#exponentiate the coefficients
exp(coef(mn_model))
describe(RQ1_data)

#creating the plots
plot(Effect("consient", mn_model))
plot(Effect("extrov", mn_model))
plot(Effect("openness", mn_model))
plot(Effect("neurotic", mn_model))
plot(Effect("agreab", mn_model))

#------------Research Question 2------------
#create a dataset to hold the needed variables
data_RQ2 <- data.frame(perceivedMeanValues$percDom, SelfMeanValues$Self.comp.media)
names(data_RQ2) <- c('percDom', 'SelfComp')

#There is a missing SelfComp value for one of the participants, so we remove it and we are left with 147 observations
data_RQ2 <- data_RQ2[!is.na(data_RQ2$SelfComp),]

#Checking the relationship between selfComp and percDom
ggplot(data_RQ2, aes(x=SelfComp, y=percDom)) +
  geom_point() +
  ggtitle("Relationship between Self Competence and Perceived Dominance") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Self Competence") + ylab("Perceived Dominance")

#Checking the normality of the two variables
qqPlot(data_RQ2$SelfComp, ylab="Self Competence")
qqPlot(perceivedMeanValues$percDom, ylab="Perceived Dominance")   

hist(data_RQ2$SelfComp)
hist(perceivedMeanValues$percDom) 

shapiro.test(data_RQ2$SelfComp)  
shapiro.test(perceivedMeanValues$percDom)

#data is normally distributed but not linear -> Spearman's correlation
RQ2 <- cor.test(data_RQ2$SelfComp, data_RQ2$percDom, method="spearman", exact = FALSE)

#------------Research Question 3------------
#checking the relationship between percLiking - percDom
ggplot(perceivedMeanValues, aes(x=percDom, y=percLiking)) +
  geom_point() +
  ggtitle("Relationship between Perceived Liking and Dominance") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Perceived Dominance") + ylab("Perceived Liking")

#and percLiking - percLead
ggplot(perceivedMeanValues, aes(x=percLead, y=percLiking)) +
  geom_point() +
  ggtitle("Relationship between Perceived Liking and Leadership") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Perceived Leadership") + ylab("Perceived Liking")

#Checking the normality of only percLead and percLiking, as we already know about percDom
qqPlot(perceivedMeanValues$percLead, ylab="Perceived Leadership") 
qqPlot(perceivedMeanValues$percLiking, ylab="Perceived Liking")   

shapiro.test(perceivedMeanValues$percLead)  
shapiro.test(perceivedMeanValues$percLiking) 

hist(perceivedMeanValues$percLead)     
hist(perceivedMeanValues$percLiking)   

#data is normally distributed but not linear -> Spearman's correlation
RQ3_lead <- cor.test(perceivedMeanValues$percLead,perceivedMeanValues$percLiking, method="spearman", exact = FALSE)
RQ3_dom <- cor.test(perceivedMeanValues$percDom,perceivedMeanValues$percLiking, method="spearman", exact = FALSE)
