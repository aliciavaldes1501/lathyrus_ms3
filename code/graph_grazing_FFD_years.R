ggplot(subset(data_selag),aes(x=FFD_s_y,y=grazing_success/(grazing_success + grazing_failure),
                              group=year,color=grazing_mean,succ=grazing_success, fail=grazing_failure))+
  geom_smooth(method="glm",method.args=list(family="binomial"),formula = cbind(succ, fail) ~ x,se=F)+
  scale_colour_gradientn(colours = myPalette(100))+my_theme()+theme(legend.position="top")

ggplot(subset(data_selag),aes(x=FFD_std,y=grazing_success/(grazing_success + grazing_failure),
                              succ=grazing_success, fail=grazing_failure))+geom_point(size=0.1)+
  geom_smooth(method="glm",method.args=list(family="binomial"),formula = cbind(succ, fail) ~ x,se=F)+
  scale_colour_gradientn(colours = myPalette(100))+my_theme()+theme(legend.position="top")+
  facet_wrap(~grazing_mean)
