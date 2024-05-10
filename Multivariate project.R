setwd("D:/FEPS/4th Year/1st Semester/Multivariate Analysis/Project")

library(readxl)
DB <- read_excel("DB.xlsx")

#clean data
DB <- subset(DB,Student!="NA" )
DB$Gender <- as.factor(DB$Gender)

DB$Student <- as.factor(DB$Student)



# 1. Select a random sample of 850 observations.

set.seed(123)
sample <- DB[sample(nrow(DB),850),]

####another method using library dplyr
# library(dplyr)
# sampled_data <-  sample_n(DB, 850)

#Extracting Numeric Variables from my dataset
library(psych)
library(dplyr)

numeric <- sample[,-c(10,12)]

describe(numeric)

#Getting The correlation Matrix 
cor= round(cor(numeric, use="complete.obs"),2)


library(corrplot) #plotting correlation matrices


corrplot(cor(numeric, use="complete.obs"), order = "hclust", tl.col='black', tl.cex=.75) 
corrplot(cor(numeric), order = "original", tl.col='black', tl.cex=.75) 

#Standardiznig the variables that will be included only
Data_1 <- sample[, sapply(sample, is.numeric)]
Data_2 <- sample[, -c(1:14,39:41)]

Data_Standardized <- scale(Data_2)

#########################################################################################
###############FACTOR ANALYSIS#################
library(GPArotation) #methods for factor rotation
library(nFactors)  #methods for determining the number of factors

#Determing number of factors 
#parallel analysis for number of factors 
fa.parallel(cor(Data_Standardized), fm="miners", fa="fa")
#Parrallel Analysis Suggested that number of factors is equal 4


#We'll move forward with the analysis having 4 factors 
factor_model <- fa(Data_Standardized, nfactors = 4, rotate = "none")
factor_model$loadings
as.data.frame(unclass(factor_model$loadings))
#loadings overlap so we'll apply rotation

#with rotation
factor_rotation <- fa(Data_Standardized, nfactors = 4, scores="Bartlett",rotate = "oblimin")
factor_rotation$loadings
as.data.frame(unclass(factor_rotation$loadings))
#rotation with varimax is the easier to be interpreted so we'll most probably use it
factor_rotation_var <- fa(Data_Standardized, nfactors = 4, scores="Bartlett",rotate = "varimax")
factor_rotation_var$loadings
as.data.frame(unclass(factor_rotation_var$loadings))


#communalities
l2= factor_rotation_var$loadings^2
h2= data.frame(apply(l2,1,sum))
colnames(h2) = c("Communalities")
h2
colnames(factor_rotation_var$scores)= c( "Transcendence","Interpersonal", "Openness", "Restraint")
factors = data.frame(factor_rotation_var$scores)
factors
data.frame(cor(factors))
#uncorrelated atwaka3 bas recheck
#La ta3leq hasebloko ento el ta3leq 


###########################################################
#########CLUSTER ANALYSIS######################
library(stats)
library(factoextra)
library(cluster)
#recheck
pca= prcomp(sample[,-c(1:14,39:41)], scale= TRUE)
barplot(pca$x[1:40,1])
plot(pca$x[,1],pca$x[,2])
#the plot suggests we have either 2 groups (one positive and the other negative) OR that we have 4 groups (1 positive, 1 negative, 1 low negative, 1 low positive)
#we'll try both 

###################RECHECK benhot eh fl argument bta3 data= f fviz 3shan kolhom shaklhom wehsh 

#we'll start by having 2 clusters
set.seed(123)
clusters_2<- kmeans(factors,centers=2)
means= clusters_2$centers
t(means)

fviz_cluster(clusters_2, data =factors)

#we'll try the 4 clusters
set.seed(123)
clusters_4<- kmeans(factors,centers=4)
means= clusters_4$centers #cluster center
t(means) #transpose the cluster center
fviz_cluster(clusters_4, data = factors)
# 3 clusters
set.seed(123)
clusters_3<- kmeans(factors,centers=3)
means= clusters_3$centers #cluster center
t(means) #transpose the cluster center
fviz_cluster(clusters_3, data = factors)

####################################################
###################Cross Validation##########################
library(dplyr)
factors$ID <- 1:nrow(factors)
train <-sample_frac(tbl =factors ,.5)
test <- anti_join(factors, train, by = "ID")

train <- train[,-5]
test <- test[,-5]
factors <- factors[,-5]
set.seed(123)
clusters_31<- kmeans(train,centers=2)
means1= clusters_31$centers #cluster center
t(means1) #transpose the cluster center
fviz_cluster(clusters_31, data = train)

set.seed(123)
clusters_32<- kmeans(test,centers=2)
means2= clusters_32$centers #cluster center
t(means2) #transpose the cluster center
fviz_cluster(clusters_32, data = test)



index <- which(clusters_31$cluster == 2)
x <-train[index,]

index1 <- which(clusters_32$cluster == 2)
 index1
 y <-test[index1,]
 
 index2 <- which(clusters_31$cluster == 1)
 
 index3 <- which(clusters_32$cluster == 1)
 
 x1 <-train[index2,]
 
 x2 <-test[index3,]
###########################################################################
##################MULTIVARIATE REGRESSION##################
library('devtools')
library('sjPlot')
library(MVN)
library(car)
mva <- lm(cbind(DASS_21,GHQ_12,SEC)~Age+factor(Gender)+Work+ factor(Student)
          +Sons+Openness+Restraint+Transcendence+Interpersonal, data=sample)
summary(mva)
## residuals for Ys
head(resid(mva))

## fitted values for Ys
head(fitted(mva))

## residual standard error
sigma(mva)

## Anova for multivariate model
anova(mva)

#Testing multivariate normal assumption
mv= mvn(sample[,c("DASS_21", "GHQ_12", "SEC")], mvnTest= "mardia", univariatePlot="histogram") #DASS_21 is skewed so it needs a transformation
mv$multivariateNormality #pvalue is small so we reject MVN assumption

#we'll use the log fn to transform DASS_21
sample$DASS_21 = sample$DASS_21 +1 
sample <- data.frame(sample)
mlm = lm(cbind(log(DASS_21),GHQ_12,SEC)~Age+factor(Gender)+Work+ factor(Student)
         +Sons+Openness+Restraint+Transcendence+Interpersonal, data=sample)
summary(mlm)
Anova(mlm)

#Univariate tests
#for DASS_21
lm1=lm(DASS_21~Age+factor(Gender)+Work+factor(Student)+
         Sons+Openness+Restraint+Transcendence+Interpersonal, data=sample)
tab_model(lm1)

hist(rstandard(lm1) , xlab= 'standardized residulas', main = "Histogram of DASS_21")
x1=rstandard(lm1)
shapiro.test(x1)
#reject normality assumption 3shan pvalue less than 0.05

#check multicollinearity
vif_values <- vif(lm1)
print(vif_values)
#vif values show moderate correlation

#No autocorrelation
durbinWatsonTest(lm1)
#H0 no autocorrelation, since p-value greater than 0.05 yeb'a don't reject H0 yeb'a THERE IS NO AUTOCORRELATION
#All relevant variables are included
summary(lm1) #ne'ol ah w KHALAAASSS

# No correlation between explanatory variables and error term
plot(lm1, which = 3) #no pattern 

# Cov(yi) = Σ
# Check this assumption by examining residuals vs. fitted values plot
plot(lm1, which = 3)

#  Cov(yi, yj) = 0 for all i≠j
plot(lm1, which = 3)

#for GHQ_12

lm2 <- lm(GHQ_12~Age+factor(Gender)+Work+factor(Student)+
            Sons+Openness+Restraint+Transcendence+Interpersonal, data=sample)


hist(rstandard(lm2) , xlab= 'standardized residulas', main = "Histogram of DASS_21")
x2=rstandard(lm2)
shapiro.test(x2) #pvalue greater than 0.05 yeb'a don't reject the normality assumption
plot(lm2, which = 2)
plot(lm2, which = 1)

vif_values2 <- vif(lm2)
print(vif_values2)

durbinWatsonTest(lm2) #pvalue akbar mn 0.05 fa don't reject yeb'a NO AUTOCORRELATION

plot(lm2, which = 3)

#for SEC

lm3 <- lm(SEC~Age+factor(Gender)+Work+factor(Student)+
            Sons+Openness+Restraint+Transcendence+Interpersonal, data=sample)

plot(lm3, which = 1)
x3=rstandard(lm3)
shapiro.test(x3) #pvalue less than 0.05 yeb'a reject normality assumption
plot(lm3, which = 2)

shapiro.test(x3)

vif_values3 <- vif(lm3)
print(vif_values3)

durbinWatsonTest(lm3) #NO AUTOCORRELATION

plot(lm3, which = 3)

