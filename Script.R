# Step 0

#Reads the data into R
catdog <-read.delim("CatsandDogs-1.dat", header = TRUE)

# Load cross table packages
install.packages("gmodels")  # Install the package if you haven't already
library(gmodels)        

#Loads the required package for loglm()
library(MASS)

#Converts the data into a contingency table
table = xtabs(~ Animal + Training + Dance, data = catdog)

#Step 1
#Run all models (0 to 8)

model0 = loglm(~ Animal + Training + Dance + Animal:Training + Training:Dance + Animal:Dance + Animal:Training:Dance , data = table, fit = TRUE)

model1 = loglm(~ Animal + Training + Dance + Animal:Training + Training:Dance + Animal:Dance, data = table, fit = TRUE)

model2 = loglm(~ Animal + Training + Dance + Animal:Training + Training:Dance , data = table, fit = TRUE)

model3 = loglm(~ Animal + Training + Dance + Animal:Training + Animal:Dance, data = table, fit = TRUE)

model4 = loglm(~ Animal + Training + Dance + Training:Dance + Animal:Dance, data = table, fit = TRUE)

model5 = loglm(~ Animal + Training + Animal:Training, data = table, fit = TRUE)

model6 = loglm(~ Training + Dance + Training:Dance , data = table, fit = TRUE)

model7 = loglm(~ Animal + Dance + Animal:Dance, data = table, fit = TRUE)

model8 = loglm(~ Animal + Training + Dance, data = table, fit = TRUE)

#Step 2

#Run the following summary statements one at a time

summary(model0) #saturated model with perfect fit

summary(model1) #if significant stop; model0 is your best fitting model; else test model 2

summary(model2) #if significant stop; model1 is your best fitting model; else test model 3

summary(model3) #if significant stop; model2 is your best fitting model; else test model 4

summary(model4) #if significant stop; model3 is your best fitting model; else test model 5

summary(model5) #if significant stop; model4 is your best fitting model; else test model 6

summary(model6) #if significant stop; model5 is your best fitting model; else test model 7

summary(model7) #if significant stop; model6 is your best fitting model; else test model 8

summary(model8) #if significant model7 is your best fitting model; else choose model 8

#Step 3

#Creates two subsets of the data; one for Cats and one for Dogs
catsub = subset(catdog, Animal == "Cat")
dogsub = subset(catdog, Animal == "Dog")

# Step 4

#Runs a chi-square analysis on the cat subset of the data
CrossTable(catsub$Training, catsub$Dance, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")

#Runs a chi-square analysis on the dog subset of the data
CrossTable(dogsub$Training, dogsub$Dance, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")

# Step 5

# Create a plot among Animal, Training, and Dance
mosaicplot(table, shade = TRUE, main = "Animal, Training, & Dance")


