#Variation partitioning, models with region and window

#Gamma, true forest

lolacl<-glmer(gamma_f~z.surf+z.age+z.abio_h+z.bio_h+z.PROX+z.F500+z.G500 +z.H50+z.MaxTWm+z.MinTCm+z.PWm+z.PDm+(1|region/window_ty)+(1|fragment),data=data3,family  = poisson)
lo<-glmer(gamma_f~z.surf+z.age+z.abio_h+z.bio_h+(1|region/window_ty)+(1|fragment),data=data3, family  = poisson)
la<-glmer(gamma_f~z.PROX+z.F500+z.G500 +z.H50+(1|region/window_ty)+(1|fragment),data=data3, family  = poisson)
cl<-glmer(gamma_f~z.MaxTWm+z.MinTCm+z.PWm+z.PDm+(1|region/window_ty)+(1|fragment),data=data3, family  = poisson)
lola<-glmer(gamma_f~z.surf+z.age+z.abio_h+z.bio_h+z.PROX+z.F500+z.G500 +z.H50+(1|region/window_ty)+(1|fragment),data=data3, family  = poisson)
locl<-glmer(gamma_f~z.surf+z.age+z.abio_h+z.bio_h+z.MaxTWm+z.MinTCm+z.PWm+z.PDm+(1|region/window_ty)+(1|fragment),data=data3, family  = poisson)
lacl<-glmer(gamma_f~z.PROX+z.F500+z.G500 +z.H50+z.MaxTWm+z.MinTCm+z.PWm+z.PDm+(1|region/window_ty)+(1|fragment),data=data3, family  = poisson)

Rlolacl<-r.squaredLR(lolacl, null.RE = TRUE)
Rlo<-r.squaredLR(lo, null.RE = TRUE)
Rla<-r.squaredLR(la, null.RE = TRUE)
Rcl<-r.squaredLR(cl, null.RE = TRUE)
Rlola<-r.squaredLR(lola, null.RE = TRUE)
Rlocl<-r.squaredLR(locl, null.RE = TRUE)
Rlacl<-r.squaredLR(lacl, null.RE = TRUE)

LO<-Rlolacl-Rlacl
LA<-Rlolacl-Rlocl
CL<-Rlolacl-Rlola
LOLA<-Rlolacl-Rcl-LO-LA
LOCL<-Rlolacl-Rla-LO-CL
LACL<-Rlolacl-Rlo-LA-CL
LOLACL<-Rlolacl-LO-LA-CL-LOLA-LOCL-LACL

(LO/Rlolacl)*100
(LA/Rlolacl)*100
(CL/Rlolacl)*100
(LOLA/Rlolacl)*100
(LOCL/Rlolacl)*100
(LACL/Rlolacl)*100
(LOLACL/Rlolacl)*100

LO+LA+CL+LOLA+LOCL+LACL+LOLACL

#Gamma, generalist

lolacl<-glmer(gamma_nf~z.surf+z.age+z.abio_h+z.bio_h+z.PROX+z.F500+z.G500 +z.H50+z.MaxTWm+z.MinTCm+z.PWm+z.PDm+(1|region/window_ty)+(1|fragment),data=data3, family  = poisson)
lo<-glmer(gamma_nf~z.surf+z.age+z.abio_h+z.bio_h+(1|region/window_ty)+(1|fragment),data=data3, family  = poisson)
la<-glmer(gamma_nf~z.PROX+z.F500+z.G500 +z.H50+(1|region/window_ty)+(1|fragment),data=data3, family  = poisson)
cl<-glmer(gamma_nf~z.MaxTWm+z.MinTCm+z.PWm+z.PDm+(1|region/window_ty)+(1|fragment),data=data3, family  = poisson)
lola<-glmer(gamma_nf~z.surf+z.age+z.abio_h+z.bio_h+z.PROX+z.F500+z.G500 +z.H50+(1|region/window_ty)+(1|fragment),data=data3, family  = poisson)
locl<-glmer(gamma_nf~z.surf+z.age+z.abio_h+z.bio_h+z.MaxTWm+z.MinTCm+z.PWm+z.PDm+(1|region/window_ty)+(1|fragment),data=data3, family  = poisson)
lacl<-glmer(gamma_nf~z.PROX+z.F500+z.G500 +z.H50+z.MaxTWm+z.MinTCm+z.PWm+z.PDm+(1|region/window_ty)+(1|fragment),data=data3, family  = poisson)

Rlolacl<-r.squaredLR(lolacl, null.RE = TRUE)
Rlo<-r.squaredLR(lo, null.RE = TRUE)
Rla<-r.squaredLR(la, null.RE = TRUE)
Rcl<-r.squaredLR(cl, null.RE = TRUE)
Rlola<-r.squaredLR(lola, null.RE = TRUE)
Rlocl<-r.squaredLR(locl, null.RE = TRUE)
Rlacl<-r.squaredLR(lacl, null.RE = TRUE)

LO<-Rlolacl-Rlacl
LA<-Rlolacl-Rlocl
CL<-Rlolacl-Rlola
LOLA<-Rlolacl-Rcl-LO-LA
LOCL<-Rlolacl-Rla-LO-CL
LACL<-Rlolacl-Rlo-LA-CL
LOLACL<-Rlolacl-LO-LA-CL-LOLA-LOCL-LACL

(LO/Rlolacl)*100
(LA/Rlolacl)*100
(CL/Rlolacl)*100
(LOLA/Rlolacl)*100
(LOCL/Rlolacl)*100
(LACL/Rlolacl)*100
(LOLACL/Rlolacl)*100

LO+LA+CL+LOLA+LOCL+LACL+LOLACL

#Alpha, true forest

lolacl<-lmer(alpha_f~z.surf+z.age+z.abio_h+z.bio_h+z.PROX+z.F500+z.G500 +z.H50+z.MaxTWm+z.MinTCm+z.PWm+z.PDm+(1|region/window_ty),data=data3,REML=FALSE)
lo<-lmer(alpha_f~z.surf+z.age+z.abio_h+z.bio_h+(1|region/window_ty),data=data3,REML=FALSE)
la<-lmer(alpha_f~z.PROX+z.F500+z.G500 +z.H50+(1|region/window_ty),data=data3,REML=FALSE)
cl<-lmer(alpha_f~z.MaxTWm+z.MinTCm+z.PWm+z.PDm+(1|region/window_ty),data=data3,REML=FALSE)
lola<-lmer(alpha_f~z.surf+z.age+z.abio_h+z.bio_h+z.PROX+z.F500+z.G500 +z.H50+(1|region/window_ty),data=data3,REML=FALSE)
locl<-lmer(alpha_f~z.surf+z.age+z.abio_h+z.bio_h+z.MaxTWm+z.MinTCm+z.PWm+z.PDm+(1|region/window_ty),data=data3,REML=FALSE)
lacl<-lmer(alpha_f~z.PROX+z.F500+z.G500 +z.H50+z.MaxTWm+z.MinTCm+z.PWm+z.PDm+(1|region/window_ty),data=data3,REML=FALSE)

Rlolacl<-r.squaredLR(lolacl, null.RE = TRUE)
Rlo<-r.squaredLR(lo, null.RE = TRUE)
Rla<-r.squaredLR(la, null.RE = TRUE)
Rcl<-r.squaredLR(cl, null.RE = TRUE)
Rlola<-r.squaredLR(lola, null.RE = TRUE)
Rlocl<-r.squaredLR(locl, null.RE = TRUE)
Rlacl<-r.squaredLR(lacl, null.RE = TRUE)

LO<-Rlolacl-Rlacl
LA<-Rlolacl-Rlocl
CL<-Rlolacl-Rlola
LOLA<-Rlolacl-Rcl-LO-LA
LOCL<-Rlolacl-Rla-LO-CL
LACL<-Rlolacl-Rlo-LA-CL
LOLACL<-Rlolacl-LO-LA-CL-LOLA-LOCL-LACL

(LO/Rlolacl)*100
(LA/Rlolacl)*100
(CL/Rlolacl)*100
(LOLA/Rlolacl)*100
(LOCL/Rlolacl)*100
(LACL/Rlolacl)*100
(LOLACL/Rlolacl)*100

LO+LA+CL+LOLA+LOCL+LACL+LOLACL

#Alpha, generalist

lolacl<-lmer(alpha_nf~z.surf+z.age+z.abio_h+z.bio_h+z.PROX+z.F500+z.G500 +z.H50+z.MaxTWm+z.MinTCm+z.PWm+z.PDm+(1|region/window_ty),data=data3,REML=FALSE)
lo<-lmer(alpha_nf~z.surf+z.age+z.abio_h+z.bio_h+(1|region/window_ty),data=data3,REML=FALSE)
la<-lmer(alpha_nf~z.PROX+z.F500+z.G500 +z.H50+(1|region/window_ty),data=data3,REML=FALSE)
cl<-lmer(alpha_nf~z.MaxTWm+z.MinTCm+z.PWm+z.PDm+(1|region/window_ty),data=data3,REML=FALSE)
lola<-lmer(alpha_nf~z.surf+z.age+z.abio_h+z.bio_h+z.PROX+z.F500+z.G500 +z.H50+(1|region/window_ty),data=data3,REML=FALSE)
locl<-lmer(alpha_nf~z.surf+z.age+z.abio_h+z.bio_h+z.MaxTWm+z.MinTCm+z.PWm+z.PDm+(1|region/window_ty),data=data3,REML=FALSE)
lacl<-lmer(alpha_nf~z.PROX+z.F500+z.G500 +z.H50+z.MaxTWm+z.MinTCm+z.PWm+z.PDm+(1|region/window_ty),data=data3,REML=FALSE)

Rlolacl<-r.squaredLR(lolacl, null.RE = TRUE)
Rlo<-r.squaredLR(lo, null.RE = TRUE)
Rla<-r.squaredLR(la, null.RE = TRUE)
Rcl<-r.squaredLR(cl, null.RE = TRUE)
Rlola<-r.squaredLR(lola, null.RE = TRUE)
Rlocl<-r.squaredLR(locl, null.RE = TRUE)
Rlacl<-r.squaredLR(lacl, null.RE = TRUE)

LO<-Rlolacl-Rlacl
LA<-Rlolacl-Rlocl
CL<-Rlolacl-Rlola
LOLA<-Rlolacl-Rcl-LO-LA
LOCL<-Rlolacl-Rla-LO-CL
LACL<-Rlolacl-Rlo-LA-CL
LOLACL<-Rlolacl-LO-LA-CL-LOLA-LOCL-LACL

(LO/Rlolacl)*100
(LA/Rlolacl)*100
(CL/Rlolacl)*100
(LOLA/Rlolacl)*100
(LOCL/Rlolacl)*100
(LACL/Rlolacl)*100
(LOLACL/Rlolacl)*100

LO+LA+CL+LOLA+LOCL+LACL+LOLACL

#Beta, true forest

lolacl<-lmer(beta_f~z.surf+z.age+z.abio_h+z.bio_h+z.PROX+z.F500+z.G500 +z.H50+z.MaxTWm+z.MinTCm+z.PWm+z.PDm+(1|region/window_ty),data=data3,REML=FALSE)
lo<-lmer(beta_f~z.surf+z.age+z.abio_h+z.bio_h+(1|region/window_ty),data=data3,REML=FALSE)
la<-lmer(beta_f~z.PROX+z.F500+z.G500 +z.H50+(1|region/window_ty),data=data3,REML=FALSE)
cl<-lmer(beta_f~z.MaxTWm+z.MinTCm+z.PWm+z.PDm+(1|region/window_ty),data=data3,REML=FALSE)
lola<-lmer(beta_f~z.surf+z.age+z.abio_h+z.bio_h+z.PROX+z.F500+z.G500 +z.H50+(1|region/window_ty),data=data3,REML=FALSE)
locl<-lmer(beta_f~z.surf+z.age+z.abio_h+z.bio_h+z.MaxTWm+z.MinTCm+z.PWm+z.PDm+(1|region/window_ty),data=data3,REML=FALSE)
lacl<-lmer(beta_f~z.PROX+z.F500+z.G500 +z.H50+z.MaxTWm+z.MinTCm+z.PWm+z.PDm+(1|region/window_ty),data=data3,REML=FALSE)

Rlolacl<-r.squaredLR(lolacl, null.RE = TRUE)
Rlo<-r.squaredLR(lo, null.RE = TRUE)
Rla<-r.squaredLR(la, null.RE = TRUE)
Rcl<-r.squaredLR(cl, null.RE = TRUE)
Rlola<-r.squaredLR(lola, null.RE = TRUE)
Rlocl<-r.squaredLR(locl, null.RE = TRUE)
Rlacl<-r.squaredLR(lacl, null.RE = TRUE)

LO<-Rlolacl-Rlacl
LA<-Rlolacl-Rlocl
CL<-Rlolacl-Rlola
LOLA<-Rlolacl-Rcl-LO-LA
LOCL<-Rlolacl-Rla-LO-CL
LACL<-Rlolacl-Rlo-LA-CL
LOLACL<-Rlolacl-LO-LA-CL-LOLA-LOCL-LACL

(LO/Rlolacl)*100
(LA/Rlolacl)*100
(CL/Rlolacl)*100
(LOLA/Rlolacl)*100
(LOCL/Rlolacl)*100
(LACL/Rlolacl)*100
(LOLACL/Rlolacl)*100

LO+LA+CL+LOLA+LOCL+LACL+LOLACL

#Beta, generalist

lolacl<-lmer(beta_nf~z.surf+z.age+z.abio_h+z.bio_h+z.PROX+z.F500+z.G500 +z.H50+z.MaxTWm+z.MinTCm+z.PWm+z.PDm+(1|region/window_ty),data=data3,REML=FALSE)
lo<-lmer(beta_nf~z.surf+z.age+z.abio_h+z.bio_h+(1|region/window_ty),data=data3,REML=FALSE)
la<-lmer(beta_nf~z.PROX+z.F500+z.G500 +z.H50+(1|region/window_ty),data=data3,REML=FALSE)
cl<-lmer(beta_nf~z.MaxTWm+z.MinTCm+z.PWm+z.PDm+(1|region/window_ty),data=data3,REML=FALSE)
lola<-lmer(beta_nf~z.surf+z.age+z.abio_h+z.bio_h+z.PROX+z.F500+z.G500 +z.H50+(1|region/window_ty),data=data3,REML=FALSE)
locl<-lmer(beta_nf~z.surf+z.age+z.abio_h+z.bio_h+z.MaxTWm+z.MinTCm+z.PWm+z.PDm+(1|region/window_ty),data=data3,REML=FALSE)
lacl<-lmer(beta_nf~z.PROX+z.F500+z.G500 +z.H50+z.MaxTWm+z.MinTCm+z.PWm+z.PDm+(1|region/window_ty),data=data3,REML=FALSE)

Rlolacl<-r.squaredLR(lolacl, null.RE = TRUE)
Rlo<-r.squaredLR(lo, null.RE = TRUE)
Rla<-r.squaredLR(la, null.RE = TRUE)
Rcl<-r.squaredLR(cl, null.RE = TRUE)
Rlola<-r.squaredLR(lola, null.RE = TRUE)
Rlocl<-r.squaredLR(locl, null.RE = TRUE)
Rlacl<-r.squaredLR(lacl, null.RE = TRUE)

LO<-Rlolacl-Rlacl
LA<-Rlolacl-Rlocl
CL<-Rlolacl-Rlola
LOLA<-Rlolacl-Rcl-LO-LA
LOCL<-Rlolacl-Rla-LO-CL
LACL<-Rlolacl-Rlo-LA-CL
LOLACL<-Rlolacl-LO-LA-CL-LOLA-LOCL-LACL

(LO/Rlolacl)*100
(LA/Rlolacl)*100
(CL/Rlolacl)*100
(LOLA/Rlolacl)*100
(LOCL/Rlolacl)*100
(LACL/Rlolacl)*100
(LOLACL/Rlolacl)*100

LO+LA+CL+LOLA+LOCL+LACL+LOLACL

