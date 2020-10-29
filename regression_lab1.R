
# Regression lab, R Practice & Exercise
# Author: Shantanu Neema

library('faraway')
data(gala)

# PART A: Practice

# Answer A1.a
cat("There are",length(names(gala)),"variables in the dataset gala & their names are:","\n",
    names(gala))
attach(gala)
g <- lm(Species ~ Endemics+Area+Elevation+Nearest+Scruz+Adjacent,data=gala)
summary(g)

# Answer A1.b
SST <- sum((Species-mean(Species))^2)
SSE <- sum(g$residuals^2)
SSR <- SST - SSE
dfM <- length(names(gala)) - 1 # for Model
dfE <- length(gala[,1]) - dfM - 1 # for Error
f.stat <- (SSR/dfM)/(SSE/dfE) 
p.val <- 1 - pf(f.stat,dfM,dfE)
1 - pf(f.stat,dfM,dfE)
cat("p-value < 0.05, so Null hypothesis is rejected and the alternative hypothesis is true")

# Answer A1.c
cat("from summary(g), coefficients with larger p-values indicates that","\n",
    "change in predictor are not associated with changes in response","\n")
cat("In model g, variables Endemics, Elevation, Nearest, Scruz and Adjacent are non-significant")

# Answer A1.d
cat("residual standard error for model g is:",round(summary(g)$sigma,3),"\n")

# Answer A1.e
cat("R-square for model g is:",100*round(summary(g)$r.squared,4),"%","\n")

# Answer A2
gala_1 <- gala[,-1]
head(gala_1)
# p = 6 as there are 6 predictors
# Calculate the inverse of XtX (See the matrix form regression equation)
ones <- rep(1,length(gala[,1]))
X <- as.matrix(cbind(ones,gala_1))
Y <- gala$Species
XtX <- t(X) %*% X
XtX_inv <- solve(XtX)
beta <- XtX_inv %*% t(X) %*% Y
beta
g$coefficients # to compare
cat("the coefficients matches perfectly with model g","\n")

# Answer A3
dgnl <- diag(XtX_inv)
sig <- summary(g)$sigma
coef_st_err <- sig*sqrt(dgnl)
coef_st_err
summary(g)$coefficients[,2] # to compare
cat("the coefficients of std error matches perfectly with model g","\n")

# Answer A4
res_std_err <- sqrt(SSE/dfE)
res_std_err
summary(g)$sigma
cat("the residual std error matches perfectly with model g","\n")

# Answer A5
R_sq <- 1 - SSE/SST
R_sq
summary(g)$r.squared
cat("the R-squared matches perfectly with model g","\n")

# Answer A6 
f.stat <- (SSR/dfM)/(SSE/dfE) 
p.val <- 1 - pf(f.stat,dfM,dfE)
cat("F statistics is:",round(f.stat,3)," and p-value is:",p.val,"\n")
cat("from model g:","\n","F statistics is:",round(summary(g)$fstatistic[1],3)," and p-value is:",9.674e-14,"\n")
cat("F statistic matches perfectly","\n", 
    "p-value is slightly different due to precision for floating point numbers as the number is too small")

# Answer A7
# Build a reduced model by removing some of the non-significant coefficients
g_red <- lm(Species~.-Nearest-Scruz-Adjacent,data = gala)
summary(g_red)
cat("Residual error reduces by removing some non-significant predictors")
anova(g,g_red)
SSE_red <- sum(g_red$residuals^2)
dfM_red <- summary(g_red)$fstatistic[2] # for Model
dfE_red <- summary(g_red)$fstatistic[3] # for Error
f.stat_red <- ((SST-SSE_red)/dfM_red)/(SSE_red/dfE_red) 
cat("failed to reject null, means g & g_red are significantly the same")

# Answer A8
newdata <- data.frame(Endemics=100,Area=mean(Area),Elevation=mean(Elevation),
                      Nearest=mean(Nearest),Scruz=mean(Scruz),Adjacent=mean(Adjacent))
pred <- predict(g,newdata,se=TRUE)
pred_red <- predict(g_red,newdata,se=TRUE)
rng <- qt(0.975,dfM)*pred$se.fit
rng_red <- qt(0.975,dfM_red)*pred_red$se.fit
cat("range of prediction for Species with 95% confidence from model g can be given by:",
    "\n","(",round(pred$fit-rng,3),", ",round(pred$fit+rng,3),")",sep="","\n")
cat("range of prediction for Species with 95% confidence from model g_red can be given by:",
    "\n","(",round(pred_red$fit-rng_red,3),", ",round(pred_red$fit+rng_red,3),")",sep="","\n")
cat("from above 2 ranges, it is clear that reduced model g_red have narrower range of prediction")
detach(gala)

# PART B: Exercise

data(savings)
attach(savings)

# Answer 1a
g <- lm(sr ~.,data = savings)
summary(g)
cat("p-value < 0.05, so Null hypothesis is rejected and the alternative hypothesis is true","\n")

# Answer 1b
cat("based on 95% confidence level, 2 variables 'dpi' and 'pop75' are non-significant","\n")

# Answer 1c
cat("value of R-square is:",100*round(summary(g)$r.squared,4),"%","\n")
cat("low R-square suggest that lower percentage of response variation can be explained by the model g","\n")
cat("It cannot be established that if there are enough predictors using the summary of model")

# Answer 2a
savings_1 <- savings[,-1]
head(savings_1)
# p = 4 as there are 4 predictors
# Calculate the inverse of XtX (See the matrix form regression equation)
ones <- rep(1,length(savings[,1]))
X <- as.matrix(cbind(ones,savings_1))
Y <- savings$sr
XtX <- t(X) %*% X
XtX_inv <- solve(XtX)
beta <- XtX_inv %*% t(X) %*% Y
beta
g$coefficients # to compare
cat("the coefficients matches perfectly with model g","\n")

# Answer 2b
SSE <- sum(g$residuals^2)
dfM <- length(names(savings)) - 1 # for Model
dfE <- length(savings[,1]) - dfM - 1 # for Error
res_std_err <- sqrt(SSE/dfE)
res_std_err
summary(g)$sigma
cat("the residual std error matches perfectly with model g","\n")

# Answer 2c
SST <- sum((sr-mean(sr))^2)
SSR <- SST - SSE
f.stat <- (SSR/dfM)/(SSE/dfE) 
p.val <- 1 - pf(f.stat,dfM,dfE)
R_sq <- 1 - SSE/SST
cat("values for SST, SSE and SSR are:",SST,",",SSE,",",SSR,"respectively \n")
cat("R-square for the model g is:",100*round(R_sq,4),"% \n")

# Answer 2d
cat("F-statistic:",f.stat,"\n","p-value:",p.val,"\n")
cat("above parameters matches perfectly well with the model g")

# Answer 2e
g_red <- lm(sr~.-dpi,data = savings) # try removing one-by-one
summary(g_red)
cat("R-square reduces slighly and adjusted R-square improves \n")
cat("Residual standard error reduces  \n")
anova(g,g_red)
cat("failed to reject null, means g & g_red are significantly the same")

g_red1 <- lm(sr~.-dpi-pop75,data = savings) # Remove both dpi and pop75
summary(g_red1)
cat("Both R-square and adjusted R-square reduces significantly \n")
cat("Residual standard error increases  \n")
cat("Conclusion \n","Model 'g_red' is better model than both 'g' and 'g_red1' as: \n",
    "It does not compromise much on the R-square and reduces the residual standard error")






