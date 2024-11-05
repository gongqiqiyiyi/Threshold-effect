 
library (xlsx)
library(ggplot2)
library(readxl)
require(ggmap)

#####Read in the case data to analyze
fd0 <- read_excel("supplementary dataset3.xlsx")

#####If both the processed and controlled SD in the original data have missing values, then their SD values can be estimated
##The missing values are then estimated using the impute SD command
library(metagear)
##method "Bracken1992" was adopted
fdsd1<-impute_SD(fd0, columnSDnames=c("sd_Naddition","sd_control"), columnXnames=c("mean_Naddition", "mean_Control"), method = "Bracken1992")






#####Meta-analysis Step 1: calculate the effect size and in-study variance for each case study
library(metafor)
library(ggplot2)
library(glmulti)
library(rJava)

d2no0<-escalc(data=fdsd1,measure="ROM",m1i=mean_Naddition,sd1i=sd_Naddition,n1i=n_Naddition,
              m2i=mean_Control,sd2i=sd_control,n2i=n_control)
View(d2no0)






#####Meta-analysis Step 2: Calculate the cumulative effect values for all case studies

##The average (cumulative) effect values for all case studies were calculated using random effects model, 
##Fixed effects model and multivariate meta-analysis model respectively.

###Fixed effects model
d2no0<- d2no0[order(-d2no0$yi),] 
fixed1<-rma(yi,vi, data=d2no0, method="FE")
summary(fixed1)

###random effects model
random1<-rma(yi,vi, data=d2no0, method="REML")
summary(random1)

###multivariate meta-analysis model
dat<-d2no0

##For different nitrogen application gradient data from the same study site, a variance covariance matrix was calculated
calc.v <- function(x) {
  v <- matrix((x$sd_control[1]^2 / (x$n_control[1] * x$mean_Control[1]^2)) , nrow=nrow(x), ncol=nrow(x))
  diag(v) <- x$Var0
  v
} 
library(metafor)
V <- bldiag(lapply(split(dat, dat$StudyID), calc.v))

##Use V instead of vi for meta-analysis operations
multi1<-rma.mv(yi, V, data=dat, random =list(~ 1 | StudyID/No)) 
summary(multi1)


##Comparison of three model results
estimates <- c(coef(fixed1), coef(random1),  coef(multi1))
variances <- c(vcov(fixed1), vcov(random1),  vcov(multi1))
labels <- c("Fixed effect model", "Random effect model",  "Multivariate Meta-Analysis Model")

forest(estimates, variances,slab=labels,ylim=c(0.8, 5.3),font=1,family="Arial", cex=1.5,psize=1,xlab="Effect size (log response ratio)")
text(c(0.23,0.42), 3.5, c("Model type", "Mean [95% CI]"),cex=1.5,family="Arial",font=1.5)

##To calculate the cumulative effect value of a subset, use the subset command
r2.1<-rma(yi,vi, data=subset(d2no0,Production.type=="AGB"), method="REML")
r2.2<-rma(yi,vi, data=subset(d2no0,Production.type=="AGB & BGB"), method="REML")#
r2.3<-rma(yi,vi, data=subset(d2no0,Production.type=="AGB & LB"), method="REML")
r2.4<-rma(yi,vi, data=subset(d2no0,Production.type=="TB"), method="REML")
r2.5<-rma(yi,vi, data=subset(d2no0,Production.type=="ANPP"), method="REML")
r2.6<-rma(yi,vi, data=subset(d2no0,Production.type=="TNPP"), method="REML")

r2.1<-rma(yi,vi, data=subset(d2no0,N.fertilizer.forms=="Urea"), method="REML")
r2.2<-rma(yi,vi, data=subset(d2no0,N.fertilizer.forms=="Ammonium nitrate"), method="REML")
r2.3<-rma(yi,vi, data=subset(d2no0,N.fertilizer.forms=="Ammonium nitrate & urea"), method="REML")
r2.4<-rma(yi,vi, data=subset(d2no0,N.fertilizer.forms=="Ammonium sulfate"), method="REML")
r2.5<-rma(yi,vi, data=subset(d2no0,N.fertilizer.forms=="Sodium nitrate"), method="REML")
r2.6<-rma(yi,vi, data=subset(d2no0,N.fertilizer.forms=="Others"), method="REML")

##View model results
summary(r2.1)
summary(r2.2)
summary(r2.3)
summary(r2.4)
summary(r2.5)
summary(r2.6)
####Get the mean and confidence interval of tau^2, I^2 and other indicators
confint(r2.1)

##plot the results
estimates2 <- c(coef(r2.1), coef(r2.2), coef(r2.3), coef(r2.4), coef(r2.5), coef(r2.6))
variances2 <- c(vcov(r2.1), vcov(r2.2), vcov(r2.3), vcov(r2.4), vcov(r2.5), vcov(r2.6))
labels2 <- c("Urea", "Ammonium nitrate", "Ammonium nitrate & urea","Ammonium sulfate", "Sodium nitrate", "Others")
#labels2 <- c("AGB", "AGB & BGB", "AGB & LB","TB", "ANPP", "TNPP")
forest(estimates2, variances2,slab=labels2,ylim=c(0.5, 8.5),font=2,#调整ylim的参数修改高度，来显示全部。
       family="Arial",cex=1.5,psize=1,xlab="Effect size (log response ratio)")
text(c(-0.6,1), 6.9, c("N fertilizer form", "Mean [95% CI]"),cex=1.5,family="Arial",font=2)#替换为N fertilizer form/Production Type
#text(c(-0.4,0.9), 6.9, c("Production Type", "Mean [95% CI]"),cex=1.5,family="Arial",font=2)#替换为N fertilizer form/Production Type






#Meta-analysis Step 3: Introducing explanatory variables

###mixed effect model(Production.type & N.fertilizer.forms)
r5<-rma(yi,vi, mods=~Production.type-1,data=d2no0, method="REML")
summary(r5)
r5.1<-rma(yi,vi, mods=~N.fertilizer.forms-1,data=d2no0, method="REML")
summary(r5.1)


###multivariate meta-analysis model(Production.type & N.fertilizer.forms)
m1<-rma.mv(yi, V, mods=~Production.type1-1,data=dat, method="REML", random =list(~ 1 | StudyID/No))
summary(m1)
m1<-rma.mv(yi, V, mods=~N.fertilizer.forms1-1,data=dat, method="REML", random =list(~ 1 | StudyID/No))
summary(m1)


####plot the results
library(orchaRd)
model_results <- orchaRd::mod_results(r5, mod = "Production.type", at = NULL,  group = "Production.type")
model_results
orchaRd::orchard_plot(r5, mod="Production.type1", group = "Production.type", xlab = "Standardised mean difference", 
                      transfm = "none", angle = 45) 

library(orchaRd)
model_results <- orchaRd::mod_results(r5.1, mod = "N.fertilizer.forms", at = NULL,  group = "N.fertilizer.forms")
model_results
orchaRd::orchard_plot(r5.1, mod="N.fertilizer.forms1", group = "N.fertilizer.forms", xlab = "Standardised mean difference", 
                      transfm = "none", angle = 45) 



###mixed effect model(Naddition)
r6<-rma(yi,vi, mods=~Naddition,data=d2no0, method="REML")
#r6<-rma(yi,vi, mods=~Naddition+Latitude+Longitude+Altitude+MAT+MAP,data=d2no0, method="REML")
summary(r6)



###multivariate meta-analysis model(Naddition)
dat<-d2no0 #dataset598
calc.v <- function(x) {
  v <- matrix((x$sd_control[1]^2 / (x$n_control[1] * x$mean_Control[1]^2)) , nrow=nrow(x), ncol=nrow(x))
  diag(v) <- x$vi
  v
} 
library(metafor)
V <- bldiag(lapply(split(dat, dat$StudyID), calc.v))


m0<-rma.mv(yi, V, mods=~Naddition,data=dat, random =list(~ 1 | StudyID/No)) 

m0<-rma.mv(yi, V, mods=~Naddition+Latitude+Longitude+Altitude+MAT+MAP,data=dat, random =list(~ 1 | StudyID/No)) 

summary(m0)
i2_ml(m0)
r2_ml(m0)

##plot the results(nonlinear relationship)
fd <-d2no0
fd$logNadd<-log(fd$Naddition)

summary(fd$logNadd)

logd<-fd$logNadd
logd <- na.omit(logd)
a<-seq(min(logd), max(logd), length.out=10000)

preds <- predict(m0, newmods=a)
x=exp(a)
wi<- 1/sqrt(fd$vi)
wi <- na.omit(wi)
size  <-0.8+3.0 * wi /max(wi) 

par(mar=c(5, 5, 1, 1) + 0.1)
plot(fd$Naddition, fd$yi,type="n",family="Arial")
plot(fd$Naddition, fd$yi, axes=TRUE, xlab="Naddition",ylab="Effect size (logRR)", cex.lab=1.6, type="n",family="Arial")
#plot(ts_p2,lcol="brown2",1,lwd=3, pch=20,col="white", xlab="Naddition",ylab="Effect size (logRR)", cex.lab=1.6, family="Arial",bg = "transparent")
cols <- c("grey80")
cols1="green4"#greenyellow, forestgreen
points(fd$Naddition, fd$yi, cex=size, col =alpha(cols1, 0.4), pch=19)
polygon(c(rev(x), x), c(rev(preds$ci.lb), preds$ci.ub), col =alpha(cols, 0.4), border = NA)
lines(x, preds$pred,col="black",lwd=3)
abline(h=0, lty=2,col="grey",lwd=3)


####plot the results using package "orchaRd"
mr_all<-rma.mv(yi,vi, data=d2no0, random=~1|StudyID/No,method="REML")
#mr_all<-rma.mv(yi,vi, mods=~Naddition,data=d2no0, random=~1|StudyID/No,method="REML")
mr_all<-rma.mv(yi,vi, mods=~Naddition+Latitude+Longitude+Altitude+MAT+MAP,data=d2no0, random=~1|StudyID/id,method="REML")
i2_ml(mr_all)
r2_ml(mr_all)

orchaRd::orchard_plot(mr_all, mod="1", group = "StudyID", xlab = "Standardised mean difference", #StudyID
                      transfm = "none", angle = 45) 

# a caterpillar plot (not a caterpillars plot)
orchaRd::caterpillars(mr_all, mod="1", group = "StudyID",xlab = "Standardised mean difference")

model_results <- orchaRd::mod_results(mr_all, mod = "1", at = NULL,  group = "StudyID")
model_results
I2 <- orchaRd::i2_ml(mr_all)
orchaRd::orchard_plot(model_results, mod="1", xlab = "Standardised mean difference") + 
  annotate(geom="text", x= 0.80, y= 1, label= paste0("italic(I)^{2} == ", round(I2[1],4)), 
           color="black", parse = TRUE, size = 5) +
  scale_fill_manual(values="grey") +
  scale_colour_manual(values="grey")





###Meta-analysis Step 4: Optimal model selection
data <-  d2no0
rma.glmulti <- function(formula, data, V, random,...) {do.call("rma.mv", list(as.formula(paste(deparse(formula))), V = as.name(V), random = as.name(random), data = data,  method = "ML", ...))}
random_effect <- list(~ 1 | StudyID/No)
modelselection2 <- glmulti(yi ~ Naddition+Latitude+Longitude+Altitude+MAT+MAP, V = "vi", random="random_effect", data=d3no0, level = 1, method = "h", fitfunction = rma.glmulti, crit = "aicc", confsetsize =32)

print(modelselection2)
rank.models <- weightable(modelselection2)
rank.models
rank.models <- rank.models[rank.models$aicc <= min(rank.models$aicc) + 2, ]
rank.models

##sort and plot all models
plot(modelselection2, family="Arial", col = "slateblue2")
plot(modelselection2, type="s", family="Arial", col="yellowgreen")






#Meta-analysis Step 5: model diagnosis to determine the reliability of the results. 
#Note: except for funnel plots, model diagnostic function values can only be used for the rma command, not yet for the rma.mv command.

r11<-rma(yi,vi, data=d2no0, method="REML")

##Publication bias diagnosis - funnel plot
funnel(r11,main="Total",family="Arial" )

##Egger's regression test for funnel plot asymmetry
##P>0.05 indicates that the funnel plot is symmetrical, with little or no publication preference
regtest(r11, model="rma") 

##radial plot
radial(r11)

##Rosenthal fail safe number
##fail safe number is independent of the selected model, only yi and vi are related
fsn(yi,vi, data=d2no0) 

