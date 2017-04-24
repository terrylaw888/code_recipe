data(Prestige , package = "car")
Prestige_narm <- na.omit(Prestige)

train <- sample(1:nrow(Prestige_narm), size = 0.7*nrow(Prestige_narm))
Prestige_train <- Prestige_narm[train,]
Prestige_test <- Prestige_narm[-train,]
sp3 <- smooth.spline(x= Prestige_train$income, y=Prestige_train$prestige, df = 3)
plot(sp10)
plot(sp2)
str(sp2)
sp2
plot(Prestige_train$income, Prestige_train$prestige)
lines(sp3)
sp40 <- smooth.spline(x= Prestige_train$income, y=Prestige_train$prestige, df = 40)
lines(sp40)
