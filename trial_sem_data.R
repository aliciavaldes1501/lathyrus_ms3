sem <- read.csv("sem_data.csv", sep = ",", header=TRUE)
str(sem)

ggplot(sem, aes(x=Treat, y = totad)) + geom_boxplot() ->p1
ggplot(sem, aes(x=Treat, y = totnym)) + geom_boxplot() ->p2
ggplot(sem, aes(x=Treat, y = addist)) + geom_boxplot() ->p3
ggplot(sem, aes(x=Treat, y = inf.ratio)) + geom_boxplot() ->p4
grid.arrange(p1,p2,p3,p4, ncol=2)

ggplot(sem,aes(x = totnym, y = inf.ratio)) + geom_smooth(method='lm') -> a1
ggplot(sem,aes(x = totad, y = inf.ratio)) + geom_smooth(method='lm') -> a2
ggplot(sem,aes(x = addist, y = inf.ratio)) + geom_smooth(method='lm') -> a3
grid.arrange(a1,a2,a3, ncol = 2)

ad.pop1 <- glm(totad ~ lethal, data = sem)
nym.pop1 <- glm(totnym ~ lethal, data = sem)
ad.dist1 <- glm(addist ~ lethal + risk, data = sem)
inf.rat1 <- glm(inf.ratio ~ totad + totnym + addist, family="binomial", weights= inf.weight, dat=sem)

sem.1 <- psem(ad.pop1, nym.pop1, ad.dist1, inf.rat1)
summary(sem1)

summary(update(sem.1, totad %~~% totnym))

inf.rat2 <- glm(inf.ratio ~ totad + totnym + addist + lethal, family="binomial", weights= inf.weight, dat=sem)

sem.2 <- psem(ad.pop1, nym.pop1, ad.dist1, inf.rat2)
summary(update(sem.2, totad %~~% totnym))

nym.pop2 <- glm(totnym ~ lethal + risk, data = sem)
inf.rat3 <- glm(inf.ratio ~ totad + totnym + addist + lethal + risk, family="binomial", weights= inf.weight, dat=sem)

sem.3 <- psem(ad.pop1, nym.pop2, ad.dist1, inf.rat3)
summary(update(sem.3, totad %~~% totnym))

summary(update(sem.3, totad %~~% totnym), standardize="scale")





