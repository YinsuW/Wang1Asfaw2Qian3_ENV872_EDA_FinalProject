## Question 1: Is physical activity associated with the level of C-protein? If so, what is the exact relationship? Is it dose-response? 

#method 1
# Split the data into training and test set
set.seed(123)
training.samples <- PA.c.protein$LBXCRP %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- PA.c.protein[training.samples, ]
test.data <- PA.c.protein[-training.samples, ]
# Build the model
model <- lm(LBXCRP ~ poly(PA, 8, raw = TRUE), data = train.data)
# Make predictions
predictions <- model %>% predict(test.data)
# Model performance
modelPerfomance<-data.frame(RMSE = RMSE(predictions, test.data$LBXCRP),
                            R2 = R2(predictions, test.data$LBXCRP))

summary(lm(LBXCRP ~ poly(PA, 8, raw = TRUE), data = train.data))
print(modelPerfomance)

ggplot(train.data, aes(PA, LBXCRP) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 8, raw = TRUE))

#method 2
mod_lm <- gam(LBXCRP ~ PA, data=PA.c.protein)
summary(mod_lm)


## Question 2: Are different levels of framingham scores the effect modifiers for this relationship between physical activity and C-protein? How do they modify the relationship?

mod_gam3 <- gam(LBXCRP ~ te(PA, risk), data=PA.c.protein)
summary(mod_gam3)
vis.gam(mod_gam3, type='response', plot.type='persp',theta=30, n.grid=500, border=NA)
visreg2d(mod_gam3, xvar='risk', yvar='PA', scale='response')
