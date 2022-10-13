library(dplyr)
library(ggplot2)
library(corrplot)
library(car)
library(jtools)
library(interactions)

#disabling scientific notation
options(scipen = 999)

#importing the stroke data set
stroke <- read.csv('/Users/seshu/Desktop/PROJECTS/R/4820 project/healthcare-dataset-stroke-data.csv', na.strings=c("N/A"))

#removing observation where gender = Other as there is only one observation in that category
stroke <- subset(stroke, gender!="Other")

#removing ID column
stroke <- subset(stroke, select = -c(id) )

#removing duplicates if there are any
stroke <- as.data.frame(stroke %>% distinct())

#factoring categorical variables
col_factor <- c("gender", "hypertension", "heart_disease", "ever_married", 
                "work_type", "Residence_type", "smoking_status", "stroke")
for (i in col_factor) {
  stroke[,i] <- as.factor(stroke[,i])
}

#summaries
summary(stroke$stroke)
summary(stroke)
summary(stroke$gender)

#Imputing the missing BMI values with gender filtered mean BMI
stroke <- stroke %>% 
  group_by(gender) %>% 
  mutate(bmi = ifelse(is.na(bmi),
                      mean(bmi, na.rm=TRUE),
                      bmi))
summary(stroke)

#Selecting variables for the model by checking which variables has an affect on Stroke(our predictor)
#Does age affect stroke? -- YES
#summary table
group_by(stroke, stroke) %>%
  summarise(
    count = n(),
    median = median(age, na.rm = TRUE),
    IQR = IQR(age, na.rm = TRUE)
  )
#box plot
ggplot(stroke, aes(x=stroke, y=age, fill=stroke)) +
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none") + theme(axis.text.x = element_text(angle = 90))

#t-test for age vs stroke - Checking the assumptions of T-test

# assumption 1: independent samples

# assumption 2: normal distribution
with(stroke, shapiro.test(age[stroke == "1"])) # not normal
with(stroke, shapiro.test(age[stroke == "0"])) # not normal

# assumption 3: equal variances
var_age <- var.test(age ~ stroke, data = stroke)
var_age # not equal variances

#performing two-samples Wilcoxon test because the data does not satisfy the assumptions
wilcox_test_age <- wilcox.test(age ~ stroke, data = stroke, exact = FALSE)
wilcox_test_age # p-value less than 0.05 means median age is significantly different between groups

#Does bmi affect stroke? -- Not so much, do t test
#summary table
group_by(stroke, stroke) %>%
  summarise(
    count = n(),
    mean = mean(bmi, na.rm = TRUE),
    IQR = IQR(bmi, na.rm = TRUE)
  )

#box plot
ggplot(stroke, aes(x=stroke, y=bmi, fill=stroke)) +
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none") + theme(axis.text.x = element_text(angle = 90))

#t-test for bmi vs stroke - Checking the assumptions of T-test
#assumption 1: independent samples

#assumption 2: normal distribution
with(stroke, shapiro.test(bmi[stroke == "1"])) # not normal
with(stroke, shapiro.test(bmi[stroke == "0"])) # not normal

#assumption 3: equal variances
var_bmi <- var.test(bmi ~ stroke, data = stroke)
var_bmi # not equal variances

#performing two-samples Wilcoxon test because the data does not satisfy the assumptions
wilcox_test_bmi <- wilcox.test(bmi ~ stroke, data = stroke, exact = FALSE)
wilcox_test_bmi # p-value less than 0.05 means mean bmi is significantly different between groups

#Does avg_glucose_level affect stroke? Maybe, do t test
#summary table
group_by(stroke, stroke) %>%
  summarise(
    count = n(),
    mean = mean(avg_glucose_level, na.rm = TRUE),
    IQR = IQR(avg_glucose_level, na.rm = TRUE)
  )

#box plot
ggplot(stroke, aes(x=stroke, y=avg_glucose_level, fill=stroke)) +
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none") + theme(axis.text.x = element_text(angle = 90))

#t-test for avg_glucose_level vs stroke - Checking the assumptions of T-test
#assumption 1: independent samples

#assumption 2: normal distribution
with(stroke, shapiro.test(avg_glucose_level[stroke == "1"])) # not normal
with(stroke, shapiro.test(avg_glucose_level[stroke == "0"])) # not normal

#assumption 3: equal variances
var_bmi <- var.test(avg_glucose_level ~ stroke, data = stroke)
var_bmi # not equal variances

#performing two-samples Wilcoxon test because the data does not satisfy the assumptions
wilcox_test_glucose <- wilcox.test(avg_glucose_level ~ stroke, data = stroke, exact = FALSE)
wilcox_test_glucose # p-value less than 0.05 means mean glucose is significantly different between groups

# Perform chi-square for categorical variables
# Stroke and Gender
stroke_gender <- table(stroke$gender,stroke$stroke, dnn = c("Gender","Stroke"))
addmargins(stroke_gender)

chisq.test(stroke_gender) # independent 

# Stroke and heart_disease
stroke_heart <- table(stroke$heart_disease,stroke$stroke, dnn = c("Heart Disease","Stroke"))
addmargins(stroke_heart)

chisq.test(stroke_heart) # not independent

# Stroke and hypertension
stroke_hypertension <- table(stroke$hypertension,stroke$stroke, dnn = c("Hypertension","Stroke"))
addmargins(stroke_hypertension)

chisq.test(stroke_hypertension) # not independent

# Stroke and ever_married
stroke_married <- table(stroke$ever_married,stroke$stroke, dnn = c("Ever Married","Stroke"))
addmargins(stroke_married)

chisq.test(stroke_married) # not independent

# Stroke and work_type
stroke_work <- table(stroke$work_type,stroke$stroke, dnn = c("Work Type","Stroke"))
addmargins(stroke_work)

chisq.test(stroke_work) # not independent

# Stroke and Residence_type
stroke_residence <- table(stroke$Residence_type,stroke$stroke, dnn = c("Residence Type","Stroke"))
addmargins(stroke_residence)

chisq.test(stroke_residence) # independent

# Stroke and smoking_status
stroke_smoking <- table(stroke$smoking_status,stroke$stroke, dnn = c("Smoking Status","Stroke"))
addmargins(stroke_smoking)

chisq.test(stroke_smoking) # not independent

# Stepwise model

model <- glm(stroke ~ ., family = "binomial", data = stroke)
step_model <- step(model, direction = "both")
summary(step_model) # stepwise variable selection

# including variables found associated to stroke
model2 <- glm(stroke ~ age + hypertension + heart_disease + avg_glucose_level +
                bmi + ever_married + work_type + smoking_status, 
              family = "binomial", data = stroke)
summary(model2)

# Detecting presence of interactions

### Using correlation analysis
stroke_num <- stroke[, c(2,8,9)]
corr <- cor(stroke_num)
corr
corrplot(corr)

### Using correlation analysis for categorical variables
idx <- 1
for (i in 1:(ncol(stroke)-1)) {
  for (j in (i+1):ncol(stroke)) {
    #taking only variables that are not numerical and different from outer loop
    condition <- !(is.numeric(stroke[,i]) | 
                     is.numeric(stroke[,j]) | i==j)
    if (condition) {
      i_col <- as.factor(pull(stroke,i))
      j_col <- as.factor(pull(stroke,j))
      cont_table <- table(i_col,j_col)
      chisq_p <- suppressWarnings(chisq.test(cont_table)$p.value)
      # checking whether the null hypothesis is rejected
      if (chisq_p < 0.05) {
        i_col_name <- colnames(stroke)[i]
        j_col_name <- colnames(stroke)[j]
        print(paste(idx,"-", i_col_name, 'and', j_col_name, 'are not independent.'))
        idx <- idx + 1
      }
    }
  }
}

# Using VIF
car::vif(model2) #no multicollinearity was found

# Interaction plots
cat_plot(model2,pred = heart_disease, modx = hypertension, geom='line')
cat_plot(model,pred = smoking_status, modx = hypertension, geom='line')
cat_plot(model,pred = heart_disease, modx = smoking_status, geom='line')
interaction.plot(x.factor = stroke$stroke, #x-axis variable
                 trace.factor = stroke$hypertension, #variable for lines
                 response = stroke$avg_glucose_level, #y-axis variable
                 fun = mean, #metric to plot
                 ylab = "Glucose",
                 xlab = "Stroke",
                 col = c("red", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Hypertension")

#Splitting the data into Training set and Test set
library(caret)
set.seed(919)
trainIndex <- createDataPartition(stroke$stroke, p = .8,
                                  list = FALSE,
                                  times = 1)
training_set <- stroke[ trainIndex,]
testing_set <- stroke[-trainIndex,]

#backward selection to select variables for the two models
model <- glm(stroke ~ ., family = "binomial", data = stroke)
step_model <- step(model, direction = "backward")

#summary of the variable selection
summary(step_model)

#model1 with interaction
model1 <- glm(stroke ~  age + hypertension + heart_disease + avg_glucose_level + avg_glucose_level*hypertension, family = "binomial", data = training_set)
summary(model1) 

#model2 without interaction
model2 <- glm(stroke ~  age + hypertension + heart_disease + avg_glucose_level, family = "binomial", data = training_set)
summary(model2) 

#Subsetting target column and independent columns separately from test dataset
x_test <- subset(testing_set, select = c(age,hypertension,heart_disease,avg_glucose_level))
y_test <- subset(testing_set, select = c(stroke))

#Making prediction using the model1 (with interactions) on test data
predictions1 <- predict(model1, x_test,type="response")

#Making prediction using the model2 (without interactions) on test data
predictions2 <- predict(model2, x_test,type="response")

#factoring predictions1 at 0.5 threshold
predictions1<-ifelse(predictions1 > 0.5,1,0)

#factoring predictions2 at 0.5 threshold
predictions2<-ifelse(predictions2> 0.5,1,0)

c1 <- data.frame(Predicted = predictions1, Actual = y_test$stroke)
c1$Predicted <- as.factor(c1$Predicted)
c1$Actual <- as.factor(c1$Actual)
levels(c1$Predicted) <- levels(c1$Actual) # you need this line for confusionMatrix

confusionMatrix(c1$Predicted, c1$Actual)

c2 <- data.frame(Predicted = predictions2, Actual = y_test$stroke)
c2$Predicted <- as.factor(c2$Predicted)
levels(c2$Predicted) <- levels(c2$Actual) # you need this line for confusionMatrix
c2$Actual <- as.factor(c2$Actual)
confusionMatrix(c2$Predicted, c2$Actual)

#hoslem-lemshow goodness of fit test for model1 (with interaction)
library(ResourceSelection)
hl <- hoslem.test(training_set$stroke, fitted(model1), g=10)
hl

#hoslem-lemshow goodness of fit test for model2 (without interaction)
h2 <- hoslem.test(training_set$stroke, fitted(model2), g=10)
h2






