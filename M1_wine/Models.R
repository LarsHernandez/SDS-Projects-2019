
library(tidyverse)
library(caret)     # Modelinterface
library(broom)     # Standardized model output
library(glmnet)    # Elastic net regression
library(mice)      # Imputation of missing data


# Data --------------------------------------------------------------------

wine        <- read_csv("M1_wine/winequalityN.csv")
names(wine) <- gsub(" ", "", names(wine), fixed = TRUE)
wine_imp    <- complete(mice(wine, method = "pmm"))



wine_imp %>% 
  ggplot(aes(x = .panel_x, y = .panel_y, colour = type, fill = type)) + 
  geom_point(alpha = 0.5, position = 'auto') + 
  geom_autodensity(alpha = 0.3, colour = NA, position = 'identity') + 
  geom_density2d(alpha = 0.5) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  facet_matrix(vars(-type, -quality), layer.diag = 2, layer.upper = 3) + 
  labs(title="Kmeans clusters by each variable (-Generation)")



# Model with caret --------------------------------------------------------

index    <- createDataPartition(y = wine_imp$type, p = 0.75, list = FALSE)
training <- wine_imp[index,] 
test     <- wine_imp[-index,] 


# Crossvalidation ---------------------------------------------------------

cv1 <- trainControl(method = "cv", 
                    number = 10)
cv2 <- trainControl(method  ='repeatedcv', 
                    number  = 2, 
                    repeats = 1, 
                    search  ='grid')


# Models ------------------------------------------------------------------

fit_glmnet <- train(type ~ .,
                    data = training,
                    trControl = cv1, 
                    tuneGrid  = expand.grid(alpha = seq(0, 1, by = 0.1), 
                                            lambda = 10^seq(-2, -4, by = -0.2)),
                    method    = "glmnet", 
                    family    = "binomial",
                    metric    = 'Accuracy')

pred_glmnet <- predict(fit_glmnet,  test)
conf_glmnet <- table(pred_glmnet, test$type)
conf_glmnet



fit_rf <- train(type ~ ., 
                data      = training,
                trControl = cv1,
                tuneGrid  = expand.grid(.mtry = (1:5)),
                method    = 'rf',
                metric    = 'Accuracy')

pred_rf <- predict(fit_rf,  test)
conf_rf <- table(pred_rf, test$type)
conf_rf







fit_svm <- train(type ~ ., 
                data      = training,
                trControl = cv1,
                tuneGrid  = expand.grid(C = 10^seq(1, -1, by = -0.02)),
                method    = 'svmLinear',
                metric    = 'Accuracy')

pred_svm <- predict(fit_svm,  test)
conf_svm <- table(pred_svm, test$type)
conf_svm







method = 'svmLinear'



# Tree --------------------------------------------------------------------
library(tree)
fit_tree <- tree(as.factor(type) ~ 
                   density + 
                   pH + 
                   fixedacidity + 
                   volatileacidity + 
                   citricacid + 
                   residualsugar+
                   chlorides+
                   freesulfurdioxide+
                   totalsulfurdioxide+
                   sulphates+
                   alcohol, 
                 mindev = 0.008, data = training)

plot(fit_tree,type="uniform")
text(fit_tree,pretty=0)











# Parsnip code
library(tidymodels)

wine <- read_csv("M1_wine/winequalityN.csv")
names(wine) <- gsub(" ", "", names(wine), fixed = TRUE)

wine_imp <- mice(wine, method="pmm")
win      <- complete(wine_imp)
win$type <- as.factor(win$type)

win_split  <- initial_split(win)
train_data <- training(win_split)
test_data  <- testing(win_split)

fit <- logistic_reg(mode = "classification", 
                    penalty = 0.01, 
                    mixture = 0.5) %>% 
  set_engine("glmnet") %>% 
  fit(type ~ ., data = train_data)

pred <- predict(fit, test_data)

table(pred$.pred_class, test_data$type)







