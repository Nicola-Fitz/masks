# plots and analyses for masks paper

library(tidyverse)
library(lubridate)
library(caTools)
library(sandwich)
library(ggpubr)
library(dynlm)
library(lmtest)
library(splines)
library(mgcv)
library(gridExtra)
library(boot)
library(cowplot)
library(Cairo)


source("forpaper_alldata.R")


# google mobility (for supplement)
# 1. Ireland
plot1s<-ggplot(data = daydata, aes(x = plotted_date))  +
  geom_line(aes(y = res7dayI, color = "res")) + 
  geom_line(aes(y = ret7dayI, color = "ret")) + 
  geom_line(aes(y = work7dayI, color = "work")) +
  geom_line(aes(y = groc7dayI, color = "groc")) + 
  geom_line(aes(y = park7dayI, color = "park") )+ 
  geom_line(aes(y = tran7dayI, color = "trans")) + 
  ggtitle("Republic of Ireland")+
  labs(y = "google mobility proportion of baseline", x = "date", color = "legend") +
  theme_bw()

plot1s<-plot1s + geom_line(aes(y = google1ROI, x = plotted_date), col = 1, lty = 2)


# 2 Northern Ireland

plot2s<-ggplot(data = daydata, aes(x = plotted_date))  +
  geom_line(aes(y = wres7dayNI, color = "res")) + 
  geom_line(aes(y = wret7dayNI, color = "ret")) + 
  geom_line(aes(y = wwork7dayNI, color = "work")) +
  geom_line(aes(y = wgroc7dayNI, color = "groc")) + 
  geom_line(aes(y = wpark7dayNI, color = "park") )+ 
  geom_line(aes(y = wtran7dayNI, color = "trans")) + 
  ggtitle("Northern Ireland") +
  labs(y = "google mobility proportion of baseline", x = "date", color = "legend") +
  theme_bw()

plot2s<-plot2s + geom_line(aes(y = googlewtNI, x = plotted_date), col = 1, lty = 2)




pdf(file = "plots/S6_google.pdf", bg = "transparent", width = 6, height = 7)
grid.arrange(plot2s, plot1s)
dev.off()


# for main paper - compare contact ratios with google mobility data


# highlighting lockdown periods ROI

locki<-daydata %>% dplyr::select( plotted_date, lockdowni) %>%
  mutate(start = plotted_date) %>%
  mutate(end = lead(plotted_date)) %>%
  mutate(end = as_date(ifelse(is.na(end), (ymd("2021/02/28")), end)))%>%
  mutate(lock = factor(lockdowni, labels = c("non lockdown", "lockdown")))



colours <-heat.colors(12)
cols<-c("non lockdown"= colours[12], "lockdown"= colours[2])


# Lockdown periods scaled to match May on

plot1b_lock<-ggplot(data = daydata, aes(x = plotted_date))+
  geom_rect(data = locki, aes(NULL,NULL,xmin=start,xmax=end,fill=lock),ymin=-1,ymax=1,alpha=0.4) +
  scale_color_manual(values = cols, aesthetics =  "fill") +
  geom_point(aes(y = log(median_c_ROI), x = plotted_date), col = 6) +
  geom_line(aes(y = google1ROI-1, x = plotted_date), col = 1, lty = 2) +
  scale_y_continuous(name = "log median weekly contact ratio",
                     sec.axis = sec_axis(~. +1, name = "proportion change in mobility"))+
  scale_x_date(name = "Date") +
  ggtitle("b")+
  labs(fill = "")+
  theme(
    axis.title.y = element_text(color = 6),
    axis.title.y.right = element_text(color = "black"),
    panel.background = element_rect(fill = "white", colour = "black", linetype = "solid", size = 0.5) )


# lockdown periods with mobility scaled to match first period

plot1a_lock<-ggplot(data = daydata, aes(x = plotted_date))+
  geom_rect(data = locki, aes(NULL,NULL,xmin=start,xmax=end,fill=lock),ymin=-1,ymax=1,alpha=0.4) +
  scale_color_manual(values = cols, aesthetics =  "fill") +
  geom_point(aes(y = log(median_c_ROI), x = plotted_date), col = 6) +
  geom_line(aes(y = 0.75*google1ROI-0.7, x = plotted_date), col = 1, lty = 2) +
  scale_y_continuous(name = "log median weekly contact ratio",
                     sec.axis = sec_axis(~.*4/3 +0.7/0.75, name = "proportion change in mobility"))+
  scale_x_date(name = "Date") +
  ggtitle("a")+
  labs(fill = "")+
  theme(
    axis.title.y = element_text(color = 6),
    axis.title.y.right = element_text(color = "black"),
    panel.background = element_rect(fill = "white", colour = "black", linetype = "solid", size = 0.5) )






pdf(file = "plots/main1_icontactmobility.pdf", bg = "transparent", width = 6, height = 7)
grid.arrange(plot1a_lock, plot1b_lock)
dev.off()

#setEPS()
cairo_ps(file = "plots/Fig1.eps", fallback_resolution = 600, width = 6.85, height = 7.85)
grid.arrange(plot1a_lock, plot1b_lock)
dev.off()




# for Northern Ireland 
# highlighting lockdown periods NI

lockni<-daydata %>% dplyr::select( plotted_date, lockdownni) %>%
  mutate(start = plotted_date) %>%
  mutate(end = lead(plotted_date)) %>%
  mutate(end = as_date(ifelse(is.na(end), (ymd("2021/02/28")), end)))%>%
  mutate(lock = factor(lockdownni, labels = c("non lockdown", "lockdown")))



colours <-heat.colors(12)
cols<-c("non lockdown"= colours[12], "lockdown"= colours[2])


# Lockdown periods scaled to match May on

plot2b_lockn<-ggplot(data = daydata, aes(x = plotted_date))+
  geom_rect(data = lockni, aes(NULL,NULL,xmin=start,xmax=end,fill=lock),ymin=-1,ymax=1,alpha=0.4) +
  scale_color_manual(values = cols, aesthetics =  "fill") +
  geom_point(aes(y = log(median_c_NI), x = plotted_date), col = 6) +
  geom_line(aes(y = googlewtNI-1, x = plotted_date), col = 1, lty = 2) +
  scale_y_continuous(name = "log median weekly contact ratio",
                     sec.axis = sec_axis(~. +1, name = "proportion change in mobility"))+
  scale_x_date(name = "Date") +
  ggtitle("b")+
  labs(fill = "")+
  theme(
    axis.title.y = element_text(color = 6),
    axis.title.y.right = element_text(color = "black"),
    panel.background = element_rect(fill = "white", colour = "black", linetype = "solid", size = 0.5) )


# lockdown periods with mobility scaled to match first period

plot2a_lockn<-ggplot(data = daydata, aes(x = plotted_date))+
  geom_rect(data = lockni, aes(NULL,NULL,xmin=start,xmax=end,fill=lock),ymin=-1,ymax=1,alpha=0.4) +
  scale_color_manual(values = cols, aesthetics =  "fill") +
  geom_point(aes(y = log(median_c_NI), x = plotted_date), col = 6) +
  geom_line(aes(y = 0.86*googlewtNI-0.8, x = plotted_date), col = 1, lty = 2) +
  scale_y_continuous(name = "log median weekly contact ratio",
                     sec.axis = sec_axis(~.*4/3 +0.7/0.75, name = "proportion change in mobility"))+
  scale_x_date(name = "Date") +
  ggtitle("a")+
  labs(fill = "")+
  theme(
    axis.title.y = element_text(color = 6),
    axis.title.y.right = element_text(color = "black"),
    panel.background = element_rect(fill = "white", colour = "black", linetype = "solid", size = 0.5) )





pdf(file = "plots/main2_nicontactmobility.pdf", bg = "transparent", width = 6, height = 7)
grid.arrange(plot2a_lockn, plot2b_lockn)
dev.off()

cairo_ps(file = "plots/Fig2.eps", fallback_resolution = 600, width = 6.85, height = 7.85)
grid.arrange(plot2a_lockn, plot2b_lockn)
dev.off()


# Ireland and Northern Ireland survey data on masks and mask recommendations and mandates

plot3<-ggplot(data = daydata, aes(x = plotted_date))+
  geom_point(aes(y = 1-mask_perc_ni, x = plotted_date),  shape = 19) +
  geom_smooth(aes(y = 1-mask_perc_ni, x = plotted_date, lty = "NI"), method = "gam", na.rm=TRUE, se= FALSE) +
  geom_point(aes(y = 1-mask_perc, x = plotted_date),  shape = 17) +
  geom_smooth(aes(y = 1-mask_perc, x = plotted_date, lty = "ROI"),  method = "gam", na.rm=TRUE, se= FALSE) +
  geom_vline(xintercept = as_date(dmy("10/08/2020")))+
  geom_vline(xintercept = as_date(dmy("13/07/2020")))+
  geom_vline(xintercept = as_date(dmy("10/07/2020")))+
  geom_vline(xintercept = as_date(dmy("03/04/2020")),  lty = 2, col = 7)+
  geom_vline(xintercept = as_date(dmy("08/04/2020")),  lty = 2, col = 8)+
  geom_vline(xintercept = as_date(dmy("05/06/2020")),  lty = 2, col = 9)+
  scale_y_continuous( name = "proportion not wearing masks in public")+
  scale_x_date(name = "Date") +
  labs(lty = ' ')+
  theme(legend.key = element_rect(fill = "white")) +
  theme(legend.position = c(0.85, 0.85)) +
  ggtitle("Republic of Ireland and Northern Ireland mask wearing")+
  theme(
    axis.title.y = element_text(color = 1),
    axis.title.y.right = element_text(color = "black"),
    panel.background = element_rect(fill = "white", colour = "black", linetype = "solid", size = 0.5) )

annotation <- data.frame(
  x = c(as_date(ymd("2020/03/28")),as_date(ymd("2020/04/15")), as_date(ymd("2020/05/28"))
        , as_date(ymd("2020/07/18")), as_date(ymd("2020/08/15"))),
  y = c(0.25, 0.25, 0.25, 0.75, 0.75),
  label = c("CDC", "ECDC", "WHO", "public transport", "public places")  )


pdf(file = "plots/main_3masks.pdf", bg = "transparent", width = 6, height = 7)
plot3 + geom_text(data=annotation, aes( x=x, y=y, label=label),                 , 
                  color=c(7,8,9, 1, 1), 
                  size=4 , angle=90 )
dev.off()


setEPS()
postscript(file = "plots/Fig3.eps", width = 6.85, height = 8.85)
plot3 + geom_text(data=annotation, aes( x=x, y=y, label=label),                 , 
                  color=c(7,8,9, 1, 1), 
                  size=4 , angle=90 )
dev.off()


# Not included in the paper - 
# plots of the smooths over time for other NPI
ggplot(data=subset(daydata, plotted_date<=as_date(ymd("2020/12/27"))) ,aes(x=plotted_date))+
  geom_line(aes(y = hands_pred, col = "hands") )+
  geom_line(aes(y = san_pred, col = "san"))+
  geom_line(aes(y = apart_pred, col = "apart"))+
  geom_line(aes(y = queue_pred, col = "queue"))+
  geom_line(aes(y = mask_pred, col = "mask" ))+
  geom_point(aes( y = mask_perc, col = "mask")) +
  geom_point(aes( y = apart, col = "apart"))+
  geom_point(aes( y = queue, col = "queue"))+
  geom_point(aes( y = sanitiser, col = "san"))+
  geom_point(aes( y = hands, col = "hands"))+
  theme_bw()

ggplot(data=subset(daydata, plotted_date<=as_date(ymd("2020/12/27"))) ,aes(x=plotted_date))+
  geom_line(aes(y = mask_pred_ni, col = "mask" ))+
  geom_point(aes( y = mask_perc_ni, col = "mask"))+
  theme_bw()


# prediction model for combined weekly data


lmall<-lm(log(contact)~ jur +google+ summer*mask + mask*google , data = weekdata)       
summary(lmall) 
AIC(lmall)

predallm<-as.data.frame(predict(lmall, interval = "confidence"))
weekdata$predm<-predallm$fit
weekdata$lwrm<-predallm$lwr
weekdata$uppm<-predallm$upr

## ROI and NI separately for checking NW variances

lmroi<-lm(log(contact)~ google+ summer*mask + mask*google , data = roi)       
summary(lmroi) 
AIC(lmroi)

predroi<-as.data.frame(predict(lmroi, interval = "confidence"))
roi$predm<-predroi$fit
roi$lwrm<-predroi$lwr
roi$uppm<-predroi$upr

lmni<-lm(log(contact)~ google+ summer*mask + mask*google , data = ni)       
summary(lmni) 
AIC(lmni)

predni<-as.data.frame(predict(lmni, interval = "confidence"))
ni$predm<-predni$fit
ni$lwrm<-predni$lwr
ni$uppm<-predni$upr

# Newey West
# republic
## newey west covariance matrix
m <- floor(0.75 * nrow(roi)^(1/3))
NW_VCOVI <- NeweyWest(lmroi, lag = m - 1, prewhite = F, adjust = T)

# make the model matrix
X<-cbind(rep(1, 41), as.matrix(lmroi[["model"]])[,2:4])
X<-cbind(X, X[,3]*X[,4], X[,2]*X[,4])

# standard errors for fitted values (Newey west variance)
nw_se <-sqrt(diag(X %*% NW_VCOVI %*% t(X)))
# 35 degrees of freedom
roi$nw_se<-nw_se
roi$nw_lwr <-roi$predm - qt(0.025, 35)*roi$nw_se
roi$nw_upp <-roi$predm + qt(0.025, 35)*roi$nw_se

# compare standard with Newey-West confidence intervals for predictions

plot8s<-ggplot(data = roi, aes(x = plotted_date, y = log(contact)) )+ 
  geom_point()+
  geom_line(aes(y = predm))+
  geom_line(aes(y = nw_lwr, col = 'newey-west'))+
  geom_line(aes(y = nw_upp, col = 'newey-west'))+
  geom_line(aes(y = lwrm, col = 'independence'))+
  geom_line(aes(y = uppm, col = 'independence'))+
  #geom_ribbon(aes(ymin = nw_lwr, ymax = nw_upp,  col = 'newey-west'), alpha = 0)+
  #geom_ribbon(aes(ymin = lwr_cr, ymax = upp_cr,  col = 'indep'), alpha = 0)+
  labs( y = "log median weekly contact ratio", x = "Date") +
  ggtitle("Republic of Ireland")+
  labs(col = "95% CI")+
  theme_bw()

# Northern Ireland
## newey west covariance matrix
m <- floor(0.75 * nrow(ni)^(1/3))
NW_VCOVNI <- NeweyWest(lmni, lag = m - 1, prewhite = F, adjust = T)

# make the model matrix
X<-cbind(rep(1, 41), as.matrix(lmni[["model"]])[,2:4])
X<-cbind(X, X[,3]*X[,4], X[,2]*X[,4])

# standard errors for fitted values (Newey west variance)
nw_se <-sqrt(diag(X %*% NW_VCOVNI %*% t(X)))
# 35 degrees of freedom
ni$nw_se<-nw_se
ni$nw_lwr <-ni$predm - qt(0.025, 35)*ni$nw_se
ni$nw_upp <-ni$predm + qt(0.025, 35)*ni$nw_se

# compare standard with Newey-West confidence intervals for predictions

plot8sn<-ggplot(data = ni, aes(x = plotted_date, y = log(contact)) )+ 
  geom_point()+
  geom_line(aes(y = predm))+
  geom_line(aes(y = nw_lwr, col = 'newey-west'))+
  geom_line(aes(y = nw_upp, col = 'newey-west'))+
  geom_line(aes(y = lwrm, col = 'independence'))+
  geom_line(aes(y = uppm, col = 'independence'))+
  #geom_ribbon(aes(ymin = nw_lwr, ymax = nw_upp,  col = 'newey-west'), alpha = 0)+
  #geom_ribbon(aes(ymin = lwr_cr, ymax = upp_cr,  col = 'indep'), alpha = 0)+
  labs( y = "log median weekly contact ratio", x = "Date") +
  ggtitle("Northern Ireland")+
  labs(col = "95% CI")+
  theme_bw()


pdf(file = "plots/S8_linear_neweywest.pdf", bg = "transparent", width = 6, height = 7)
ggarrange(plot8sn, plot8s, ncol = 1)
dev.off()





###################
# try  smooth for mask instead of linear assumption
gamall<-gam(log(contact)~ jur + summer + google +s(mask, by = factor(summer))+ti(google,mask) , data = weekdata)       
summary(gamall) 
AIC(gamall)


# preds

predallgam<-predict(gamall, se=TRUE)
weekdata$pred_gam<-predallgam$fit
weekdata$se_gam<-predallgam$se.fit

weekdata$lwr_gam <-weekdata$pred_gam - qt(0.975, df = 41-6)*weekdata$se_gam
weekdata$upp_gam <-weekdata$pred_gam + qt(0.975, df = 41-6)*weekdata$se_gam



# compare predictions of linear assumption for mask wearing with smooth - not included in paper

plotcompare_models<-ggplot(data = (weekdata), aes(x = plotted_date, y = log(contact), group = jur) )+ 
  geom_point()+
  geom_line(aes(y = pred_gam,  lty = 'spline'))+
  geom_line(aes(y = predm,  lty = 'linear'))+
  #geom_ribbon(aes(ymin = lwrm, ymax = uppm), alpha = 0.2)+
  facet_grid(jur ~.)+
  labs( y = "log median weekly contact ratio", x = "Date") +
  labs(fill = "95% CI", lty = "predictors")+
  ggtitle("Comparison of linear assumption with smooth for masks")+
  labs(group= "Jurisdiction")+
  theme_bw()

pdf(file = "plots/Scomparemodels.pdf", bg = "transparent", width = 6, height = 7)
plotcompare_models
dev.off()


# what if mask wearing 90%

new<-weekdata
new$mask<-0.9

predlmwhatif<-as.data.frame(predict(lmall, newdata = new, interval = "confidence"))
new$predm<-predlmwhatif$fit
new$lwrm<-predlmwhatif$lwr
new$uppm<-predlmwhatif$upr

predgamwhatif<-as.data.frame(predict(gamall, newdata = new, se=TRUE))
new$pred_gam<-predgamwhatif$fit
new$se_gam<-predallgam$se.fit

new$lwr_gam <-new$pred_gam - qt(0.975, df = 41-6)*new$se_gam
new$upp_gam <-new$pred_gam + qt(0.975, df = 41-6)*new$se_gam


plotwhatif<-ggplot(data = new, aes(x = plotted_date, y = log(contact), group = jur) )+ 
  geom_point(aes(colour = jur))+
  #geom_line(aes(y = pred_gam, colour = jur, group = jur, lty = 'spline'))+
  geom_line(aes(y = predm, colour = jur, group = jur, lty = 'counterfactual'))+
  geom_ribbon(aes(ymin = lwrm, ymax = uppm, fill = jur, group = jur), alpha = 0.1)+
  geom_line(data = weekdata, aes(y = predm,  lty = 'observed', colour = jur, group = jur))+
  facet_grid(jur ~.)+
  labs( y = "log median weekly contact ratio", x = "Date") +
  labs(fill = "95% CI", lty = "scenario", colour = "jurisdiction")+
  #ggtitle("Intervention scenario")+
  labs(group= "Jurisdiction")+
  theme_bw()


pdf(file = "plots/whatif.pdf", bg = "transparent", width = 6, height = 7)
plotwhatif
dev.off()


cairo_ps(file = "plots/Fig4.eps", fallback_resolution = 600, width = 6.85, height = 7.85)
plotwhatif
dev.off()


## contour plot with observed data

# in winter
mask<-seq(0, 1, length.out = 100)
google<-seq(0.2, 0.9, length.out = 100)
summer<-0
jur<-c("NI", "ROI")

newdat<-expand.grid(mask=mask, google=google, summer = summer, jur = jur)


newdat$lmpred<-predict(lmall, newdat)
newdat$gampred<-predict(gamall, newdat)

plot5winter<-ggplot(weekdata, aes(google, mask)) +
  geom_point( aes(x = google, y = mask, group = jur, pch = jur)) +
  geom_contour(data = newdat, aes(z=lmpred, col = ..level.., lty = "linear")) + 
  geom_contour(data = newdat, aes(z = gampred, col = ..level.., lty = "spline"))+
  geom_point(data = new, aes(x = google, y = mask), col = 2, pch = 4) +
  scale_color_continuous("log contact ratio") + 
  labs(x = "Google mobility proportion of baseline", y = "proportion wearing masks") +
  labs(pch = "Jurisdiction")+
  theme_bw()+
  scale_linetype("Model")+ 
  ggtitle("Winter")

# in summer
mask<-seq(0, 1, length.out = 100)
google<-seq(0.2, 0.9, length.out = 100)
summer<-1
jur<-c("NI", "ROI")

newdat<-expand.grid(mask=mask, google=google, summer = summer, jur = jur)


newdat$lmpred<-predict(lmall, newdat)
newdat$gampred<-predict(gamall, newdat)

plot5summer<-ggplot(weekdata, aes(google, mask)) +
  geom_point( aes(x = google, y = mask, group = jur, pch = jur)) +
  geom_contour(data = newdat, aes(z=lmpred, col = ..level.., lty = "linear")) + 
  geom_contour(data = newdat, aes(z = gampred, col = ..level.., lty = "spline"))+
  geom_point(data = new, aes(x = google, y = mask), col = 2, pch = 4) +
  scale_color_continuous("log contact ratio") + 
  labs(x = "Google mobility proportion of baseline", y = "proportion wearing masks") +
  labs(pch = "Jurisdiction")+
  theme_bw()+
  scale_linetype("Model")+ 
  ggtitle("Summer")


pdf(file = "plots/S7_contour.pdf", bg = "transparent", width = 6, height = 7)
grid.arrange(plot5summer, plot5winter)
dev.off()



## sensitivity for other NPIs (Republic only)
# not adjusted: 

lmroi1<-lm(log(contact) ~ google+mask*google + mask*winter , data = rois)
summary(lmroi1) 
AIC(lmroi1)

predroi1<-as.data.frame(predict(lmroi1, interval = "confidence"))
rois$predm1<-predroi1$fit
rois$lwrm1<-predroi1$lwr
rois$uppm1<-predroi1$upr

# predictions under intervention
new<-rois
new$mask<-0.9

predlmwhatif<-as.data.frame(predict(lmroi1, newdata = new, interval = "confidence"))
new$predm_w<-predlmwhatif$fit
new$lwrm_w<-predlmwhatif$lwr
new$uppm_w<-predlmwhatif$upr

# adjust for apart

lm_apart<-lm(log(contact)~ google+  (mask)*google+ mask*apart + mask*winter, data = rois)       
summary(lm_apart) 
AIC(lm_apart)

pred_apart<-as.data.frame(predict(lm_apart, interval = "confidence"))
rois$predm_apart<-pred_apart$fit
rois$lwrm_apart<-pred_apart$lwr
rois$uppm_apart<-pred_apart$upr



# intervention
# what if mask wearing 90% for adjusted models



predlmapart_whatif<-as.data.frame(predict(lm_apart, newdata = new, interval = "confidence"))
new$predmapart_w<-predlmapart_whatif$fit
new$lwrmapart_w<-predlmapart_whatif$lwr
new$uppmapart_w<-predlmapart_whatif$upr


plot_apart<-ggplot(data = rois, aes(x = plotted_date, y = log(contact)) )+ 
  geom_point()+
  geom_line(data = rois, aes(y = predm_apart, lty = "fact", col = "adj"))+
  geom_line(data = rois, aes(y = predm1, lty = "fact", col = "unadj"))+
  geom_line(data = new, aes(y = predmapart_w, lty = "counter", col = "adj"))+
  geom_line(data = new, aes(y = predm_w, lty = "counter", col = "unadj")) +
  geom_ribbon(data = new, aes(ymin = lwrmapart_w, ymax = uppmapart_w, fill = "adj"), alpha = 0.1)+
  geom_ribbon(data = new, aes(ymin = lwrm_w, ymax = uppm_w, fill = "unadj"), alpha = 0.1)+
  labs( y = "log median weekly contact ratio", x = "Date") +
  labs(fill = "", lty = "", col = "")+
  ggtitle("Adjust for sitting apart")+
  ylim(-1.5, 0.5)+
  theme_bw()


#  hands

lm_hands<-lm(log(contact)~ google+  (mask)*google+ mask*hands + mask*winter, data = rois)       
summary(lm_hands) 
AIC(lm_hands)

predhands<-as.data.frame(predict(lm_hands, interval = "confidence"))
rois$predm_hands<-predhands$fit
rois$lwrm_hands<-predhands$lwr
rois$uppm_hands<-predhands$upr

# intervention
# what if mask wearing 90%
# using the same data set for interventions

predlmhandswhatif<-as.data.frame(predict(lm_hands, newdata = new, interval = "confidence"))
new$predmhands_w<-predlmhandswhatif$fit
new$lwrmhands_w<-predlmhandswhatif$lwr
new$uppmhands_w<-predlmhandswhatif$upr


plot_hands<-ggplot(data = rois, aes(x = plotted_date, y = log(contact)) )+ 
  geom_point()+
  geom_line(data = rois, aes(y = predm_hands, lty = "fact", col = "adj"))+
  geom_line(data = rois, aes(y = predm1, lty = "fact", col = "unadj"))+
  geom_line(data = new, aes(y = predmhands_w, lty = "counter", col = "adj"))+
  geom_line(data = new, aes(y = predm_w, lty = "counter", col = "unadj")) +
  geom_ribbon(data = new, aes(ymin = lwrmhands_w, ymax = uppmhands_w, fill = "adj"), alpha = 0.1)+
  geom_ribbon(data = new, aes(ymin = lwrm_w, ymax = uppm_w, fill = "unadj"), alpha = 0.1)+
  labs( y = "log median weekly contact ratio", x = "Date") +
  labs(fill = "", lty = "", col = "")+
  ggtitle("Adjust for hand washing")+
  ylim(-1.5, 0.5)+
  theme_bw()


#  queue


lm_queue<-lm(log(contact)~ google+  (mask)*google+ mask*queue + mask*winter, data = rois)       
summary(lm_queue) 
AIC(lm_queue)

predqueue<-as.data.frame(predict(lm_queue, interval = "confidence"))
rois$predm_queue<-predqueue$fit
rois$lwrm_queue<-predqueue$lwr
rois$uppm_queue<-predqueue$upr

# intervention
# what if mask wearing 90%
# using the same data set for interventions

predlmqueuewhatif<-as.data.frame(predict(lm_queue, newdata = new, interval = "confidence"))
new$predmqueue_w<-predlmqueuewhatif$fit
new$lwrmqueue_w<-predlmqueuewhatif$lwr
new$uppmqueue_w<-predlmqueuewhatif$upr


plot_queue<-ggplot(data = rois, aes(x = plotted_date, y = log(contact)) )+ 
  geom_point()+
  geom_line(data = rois, aes(y = predm_queue, lty = "fact", col = "adj"))+
  geom_line(data = rois, aes(y = predm1, lty = "fact", col = "unadj"))+
  geom_line(data = new, aes(y = predmqueue_w, lty = "counter", col = "adj"))+
  geom_line(data = new, aes(y = predm_w, lty = "counter", col = "unadj")) +
  geom_ribbon(data = new, aes(ymin = lwrmqueue_w, ymax = uppmqueue_w, fill = "adj"), alpha = 0.1)+
  geom_ribbon(data = new, aes(ymin = lwrm_w, ymax = uppm_w, fill = "unadj"), alpha = 0.1)+
  labs( y = "log median weekly contact ratio", x = "Date") +
  labs(fill = "", lty = "", col = "")+
  ggtitle("Adjust for distance in queue")+
  ylim(-1.5, 0.5)+
  theme_bw()


#  sanitiser


lm_sanit<-lm(log(contact)~ google+  (mask)*google+ mask*sanit + mask*winter, data = rois)       
summary(lm_sanit) 
AIC(lm_sanit)

predsanit<-as.data.frame(predict(lm_sanit, interval = "confidence"))
rois$predm_sanit<-predsanit$fit
rois$lwrm_sanit<-predsanit$lwr
rois$uppm_sanit<-predsanit$upr

# intervention
# what if mask wearing 90%
# using the same data set for interventions

predlmsanitwhatif<-as.data.frame(predict(lm_sanit, newdata = new, interval = "confidence"))
new$predmsanit_w<-predlmsanitwhatif$fit
new$lwrmsanit_w<-predlmsanitwhatif$lwr
new$uppmsanit_w<-predlmsanitwhatif$upr


plot_san<-ggplot(data = rois, aes(x = plotted_date, y = log(contact)) )+ 
  geom_point()+
  geom_line(data = rois, aes(y = predm_sanit, lty = "fact", col = "adj"))+
  geom_line(data = rois, aes(y = predm1, lty = "fact", col = "unadj"))+
  geom_line(data = new, aes(y = predmsanit_w, lty = "counter", col = "adj"))+
  geom_line(data = new, aes(y = predm_w, lty = "counter", col = "unadj")) +
  geom_ribbon(data = new, aes(ymin = lwrmsanit_w, ymax = uppmsanit_w, fill = "adj"), alpha = 0.1)+
  geom_ribbon(data = new, aes(ymin = lwrm_w, ymax = uppm_w, fill = "unadj"), alpha = 0.1)+
  labs( y = "log median weekly contact ratio", x = "Date") +
  labs(fill = "", lty = "", col = "")+
  ggtitle("Adjust for using sanitiser")+
  ylim(-1.5, 0.5)+
  theme_bw()







# weekdata plots

pgrid <- plot_grid(
  plot_apart + theme(legend.position="none"),
  plot_queue + theme(legend.position="none"),
  plot_hands + theme(legend.position="none"),
  plot_san + theme(legend.position="none"),
  align = 'vh',
  hjust = -1,
  nrow = 2)

legend <- get_legend(
  # create some space to the left of the legend
  plot_apart + theme(legend.box.margin = margin(0, 0, 0, 12))
)

# add the legend 
plot_grid(pgrid, legend, rel_widths = c(3, .4))

pdf(file = "plots/sens.pdf", bg = "transparent", width = 6, height = 7)
plot_grid(pgrid, legend, rel_widths = c(3, .4))
dev.off()

cairo_ps(file = "plots/Fig5.eps", fallback_resolution = 600, width = 6.85, height = 7.85)
plot_grid(pgrid, legend, rel_widths = c(3, .4))
dev.off()



##############################################################
##############################################################
#############################################################



# how many hospital admissions before 10th August?
# from 12 March for NI, from 4 April for ROI 

# observed
sum(subset(df.plot1$hosp_NI, df.plot1$plotted_date<as_date(ymd("2020/08/10")) & df.plot1$plotted_date>=as_date(ymd("2020/03/12"))))


sum(subset(df.plot1$hosp_ROI, df.plot1$plotted_date<as_date(ymd("2020/08/10"))))

## predicted
sum(subset(df.plot1$median_hosp_adm_NI, df.plot1$plotted_date<as_date(ymd("2020/08/10")) & df.plot1$plotted_date>=as_date(ymd("2020/03/12"))))


sum(subset(df.plot1$median_hosp_adm_ROI, df.plot1$plotted_date<as_date(ymd("2020/08/10"))& df.plot1$plotted_date>=as_date(ymd("2020/04/03"))))


