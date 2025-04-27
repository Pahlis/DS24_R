library(tidyverse) 
library(readxl)
library(knitr)
library(car)

bil_data <- read_excel("data_insamling_volvo_blocket.xlsx")
print(colSums(is.na(bil_data)))
summary(bil_data)
str(bil_data)
colnames(bil_data)

# Tar bort kolumner jag inte vill använda
bil_data <- bil_data %>% select(-c(Märke, Index, Datum_i_trafik, År_i_trafik))
colnames(bil_data)

# Hanterar saknade värden, ersätter hästkrafter med median
bil_data <- bil_data %>%
  mutate(
    Hästkrafter = ifelse(is.na(Hästkrafter), median(Hästkrafter, na.rm = TRUE), Hästkrafter), 
    across(c(Växellåda, Biltyp, Drivning, Färg, Modell), ~replace_na(.x, "Okänt"))
  )

# Konverterar Motorstorlek till numerisk och hanterar NA
bil_data$Motorstorlek <- parse_number(bil_data$Motorstorlek)
bil_data$Motorstorlek[is.na(bil_data$Motorstorlek)] <- median(bil_data$Motorstorlek, na.rm = TRUE)

# Konverterar kategoriska variabler till faktorer
bil_data <- bil_data %>%
  mutate(across(c(Säljare, Bränsle, Växellåda, Biltyp, Drivning, Färg, Modell), as.factor))

# Identifiera de 5-10 vanligaste modellerna
topp_modeller <- names(sort(table(bil_data$Modell), decreasing=TRUE)[1:7])
bil_data$Modell_topp <- ifelse(bil_data$Modell %in% topp_modeller, 
                               as.character(bil_data$Modell), 
                               "Övrig")
# Kontrollerar proportionerna
variabler <- c("Säljare", "Bränsle", "Växellåda", "Biltyp", "Drivning", "Färg", "Modell")

for (var in variabler) {
  print(kable(sort(prop.table(table(bil_data[[var]])), decreasing = TRUE), caption = paste("Fördelning av", var)))
}

print(colSums(is.na(bil_data)))

# Fördelning av försäljningspriser
ggplot(bil_data, aes(x = Försäljningspris)) +
  geom_histogram(bins = 30)

# Logaritmerad prisvariabel
bil_data$log_pris <- log(bil_data$Försäljningspris)

# Fördelning av logaritmerade priser
ggplot(bil_data, aes(x = log_pris)) +
  geom_histogram(bins = 30)

modell_frekvens <- sort(table(bil_data$Modell), decreasing=TRUE)
print(modell_frekvens)

set.seed(123)
train_index <- sample(1:nrow(bil_data), size = 0.7 * nrow(bil_data))
train_data <- bil_data[train_index, ]
test_data <- bil_data[-train_index, ]

# Definiera enklare modeller för AIC/BIC jämförelse
bas_formula <- log_pris ~ Miltal + Modellår + Hästkrafter
model1 <- lm(bas_formula, data = bil_data)
model2 <- lm(update(bas_formula, . ~ . + Motorstorlek), data = bil_data)
model3 <- lm(update(bas_formula, . ~ . + Bränsle), data = bil_data)
model4 <- lm(update(bas_formula, . ~ . + Motorstorlek + Bränsle), data = bil_data)
model5 <- lm(update(bas_formula, . ~ . + Motorstorlek + Bränsle + Drivning), data = bil_data)


# Jämför modellerna med AIC och BIC
modeller <- list(model1, model2, model3, model4, model5)
modellnamn <- c("Bas", "+Motorstorlek", "+Bränsle", "+Bränsle+Drivning", "+Motorstorlek+Bränsle+Drivning")

jämför_modeller <- data.frame(
  Modell = modellnamn,
  AIC = sapply(modeller, AIC),
  BIC = sapply(modeller, BIC),
  Adjusted_R2 = sapply(modeller, function(m) summary(m)$adj.r.squared)
)

# Sorterar efter AIC
jämför_modeller <- jämför_modeller %>% arrange(AIC)
print(jämför_modeller)

# Väljer den bästa modellen utifrån AIC
bästa_modell_index <- which.min(sapply(modeller, AIC))
bästa_modell <- modeller[[bästa_modell_index]]
print(paste("Bästa modellen enligt AIC är:", modellnamn[bästa_modell_index]))

# Analyserar modellen
vif(bästa_modell)
summary_bästa <- summary(bästa_modell)
print(summary_bästa)

par(mfrow = c(2, 2))
plot(bästa_modell)

ggplot(data.frame(resid = residuals(bästa_modell)), aes(x = resid)) +
  geom_histogram(bins = 30) +
  labs(title = "Residualfördelning")

conf_intervals <- confint(bästa_modell, level = 0.95)
print("95% konfidensintervall för koefficienterna:")
kable(conf_intervals, digits = 4, caption = "95% Konfidensintervall för koefficienterna")

train_model <- lm(formula(bästa_modell), data = train_data)
summary(train_model)$r.squared

test_data$predikt_log_pris <- predict(train_model, newdata = test_data)
test_data$predikt_pris <- exp(test_data$predikt_log_pris)

rmse <- sqrt(mean((test_data$Försäljningspris - test_data$predikt_pris)^2))
r2 <- cor(test_data$Försäljningspris, test_data$predikt_pris)^2
print(paste("RMSE på testdata:", rmse))
print(paste("R² på testdata:", r2))

# Visualiserar prediktioner
ggplot(test_data, aes(x = predikt_pris, y = Försäljningspris)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(x = "Predikterade värden", y = "Faktiska värden", 
       title = "Prediktioner vs Faktiska värden") +
  theme_minimal()