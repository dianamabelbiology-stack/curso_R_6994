

data(DNase)
head(DNase)

mean <- aggregate(density ~ conc,
                   data=DNase, FUN = mean)

 mean
 
 # Step 3: Create barplot
 barplot(
   height = mean$density, # height is a vector of numeric values that determines the h
   names.arg = mean$conc, # names.arg is a vector of names of each bar
   col = "lightblue",
   border = "gray30",
   main = "Mean DNase I Activity by Concentration", # Main title of the plot
   xlab = "Concentration (mg/mL)", # Label for the X-axis
   ylab = "Mean Optical Density", # Label for the Y-axis
   las = 2, # rotate labels for clarity
   cex.names = 0.8, # adjust label size
  # Don't forget to close the parenthesis
 horiz = TRUE # make bars horizontal
 )

 
 
 # Step 3: Create barplot
 barplot(
   height = mean$density, # height is a vector of numeric values that determines the h
   names.arg = mean$conc, # names.arg is a vector of names of each bar
   col = c("red", "green", "black", "blue", "yellow", "pink"),
   border = "gray30",
   main = "Mean DNase I Activity by Concentration", # Main title of the plot
   xlab = "Concentration (mg/mL)", # Label for the X-axis
   ylab = "Mean Optical Density", # Label for the Y-axis
   las = 2, # rotate labels for clarity
   cex.names = 0.8, # adjust label size
   # Don't forget to close the parenthesis
   horiz = TRUE # make bars horizontal
 )

 
 library(readxl)
 coronary <- read_excel ("data/coronary.xlsx")
head(coronary) 
#summary(coronary)
#vamos a hacer un scaret plot 
plot(
  coronary$dbp ~ coronary$chol,
  type = "p", # line plot
  col = "purple", # line color
  lwd = 3, # line width
  xlab = "Total Cholesterol (mmol/L)", # x-axis label
  ylab = "Diastolic Blood Pressure (mmHg)", # y-axis label
  main = "Relationship between Cholesterol and Diastolic BP",
  pch = 25
  )
a
#correlacioón 

spearman_result <- cor.test(
  coronary$chol,
  coronary$dbp,
  method = "spearman",
  exact = FALSE # avoids warnings with tied ranks
)

spearman_result
#imprime el coheficiente de correlación y el p-value   

plot(
  coronary$dbp ~ coronary$chol,
  type = "p", # line plot
  col = "blue", # line color
  lwd = 2, # line width
  xlab = "Total Cholesterol (mmol/L)", # x-axis label
  ylab = "Diastolic Blood Pressure (mmHg)", # y-axis label
  main = "Relationship between Cholesterol and Diastolic BP"
)
abline(lm(dbp ~ chol, data = coronary), col = "red", lwd = 2,lty = 2)

# Asegúrate de que los datos estén ordenados por edad
coronary <- coronary[order(coronary$age), ]
# Gráfico de líneas básico
plot(coronary$age, coronary$chol,
     type = "l", # "l" = line plot
     col = "blue",
     lwd = 2,
     xlab = "Age (years)",
     ylab = "Cholesterol (mmol/L)",
     main = "Cholesterol vs Age")


# Asegúrate de que los datos estén ordenados por edad
coronary <- coronary[order(coronary$age), ]
# Gráfico de líneas básico
plot(coronary$age, coronary$chol,
     type = "l", # "l" = line plot
     col = "blue",
     lwd = 2,
     xlab = "Age (years)",
     ylab = "Cholesterol (mmol/L)",
     main = "Cholesterol vs Age")

hist(coronary$chol)

hist(coronary$chol,
     main = "Distribution of Cholesterol",
     xlab = "Cholesterol (mmol/L)",
     col = "pink",
     border = "white",
     breaks = 40, # You can cange the number
)

boxplot(coronary$chol,
        main = "Cholesterol Levels",
        ylab = "Cholesterol (mmol/L)",
        col = "lightgreen",
        border = "darkgreen"
)

boxplot(sbp ~ gender, data = coronary)


anova_model <- aov(sbp ~ gender, data = coronary)
summary(anova_model)

#1. Shapiro-Wilk test on residuals
shapiro.test(residuals(anova_model))
