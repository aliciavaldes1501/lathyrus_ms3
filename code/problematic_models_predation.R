mod_pred_FFD_mixed<-glmer(cbind(round(n_intact_seeds),round(n_pred_seeds))~scale(FFD)+
                (1|id),data=subset(data_selag,n_seeds>0),family="binomial")
summary(mod_pred_FFD_mixed)
mod_pred_FFD<-glm(cbind(round(n_intact_seeds),round(n_pred_seeds))~scale(FFD),
                 data=subset(data_selag,n_seeds>0),family="binomial")
summary(mod_pred_FFD)

plot_model(mod_pred_FFD_mixed, type = "diag")

ggpredict(mod_pred_FFD_mixed, "scale(FFD) [all]") %>% plot()

summary(glmer(I(round(n_intact_seeds)/(round(n_intact_seeds)+round(n_pred_seeds)))~scale(FFD)+
                (1|id),data=subset(data_selag,n_seeds>0),family="binomial",
              weight=round(n_intact_seeds)+round(n_pred_seeds))) # Exactly equivalent to mod_pred_FFD_mixed


xlevels = as.list(with(data_selag, data.frame(x = seq(min(FFD,na.rm=T), max(FFD,na.rm=T),
                                               len = 100))))
newdata = as.data.frame(Effect("FFD", mod_pred_FFD_mixed, xlevels = xlevels))
ggplot(data = newdata, aes(y = fit, x = FFD)) + 
  geom_point(data = data_selag,aes(y = round(n_intact_seeds)/(round(n_intact_seeds) + round(n_pred_seeds)))) + 
  geom_ribbon(aes(ymin = lower,ymax = upper), fill = "blue", alpha = 0.3) + geom_line() +
  scale_y_continuous("Probability of seeds escaping predation") + my_theme()

datos<-as.data.frame(subset(data_selag,n_seeds>0)%>%
  group_by(year)%>%
  summarise(n_years=count(id)))
datos<-datos$n_years
hist(datos$freq)
datos$id<-datos$x
datos$x<-NULL

levels1year<-as.character(droplevels(subset(datos,freq==1)$id))

data_selag_1year<-data_selag[data_selag$id %in% levels1year,]

mod_pred_FFD_mixed_1year<-glmer(cbind(round(n_intact_seeds),round(n_pred_seeds))~scale(FFD)+
                            (1|id),data=subset(data_selag_1year,n_seeds>0),family="binomial")
summary(mod_pred_FFD_mixed_1year)

# 1 year 0.15878 
# 2 years 0.07550
# 3 years -0.010035 (NS) (failed to converge)
# 4 years -0.04265
# 5 years -0.08300
# 6 years -0.025219
# 7 years -0.10651
# 8 years -0.066923

datos$id<-datos$x
datos$x<-NULL

data_selag$mod_pred_FFD_mixed_resid<-with(data_selag,ifelse(n_seeds>0,resid(mod_pred_FFD_mixed),NA))
data_selag$mod_pred_FFD_mixed_fitted<-with(data_selag,ifelse(n_seeds>0,fitted(mod_pred_FFD_mixed),NA))
data_selag_bis<-merge(data_selag,datos)


ggplot(data_selag_bis,aes(x=mod_pred_FFD_mixed_fitted,y=mod_pred_FFD_mixed_resid))+geom_point()+
  geom_point(data = filter(data_selag_bis, freq == 1),aes(x=mod_pred_FFD_mixed_fitted,y=mod_pred_FFD_mixed_resid), colour = "red")


datos_first<-subset(data_selag,n_seeds>0)%>%
  group_by(id)%>% slice(which.max(year))

mod_pred_FFD_mixed_firstyear<-glmer(cbind(round(n_intact_seeds),round(n_pred_seeds))~scale(FFD)+
                                  (1|id),data=subset(datos_first,n_seeds>0),family="binomial")
summary(mod_pred_FFD_mixed_firstyear)

######################################################################################################

datos1<-as.data.frame(data_selag%>%
                       group_by(year)%>%
                       summarise(n_years=count(id)))
datos1<-datos1$n_years
hist(datos1$freq)
