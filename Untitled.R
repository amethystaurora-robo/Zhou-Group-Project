# Load necessary libraries
library(ggplot2)
library(readr)

# Load a CSV file
anguilla <- read_tsv("anguilla_regression.tsv")
rutilus <- read_tsv("rutilus_regression.tsv")
auratus <- read_tsv('auratus_regression.tsv')
carpio <- read_tsv('carpio_regression.tsv')
flesus <- read_tsv('flesus_regression.tsv')
mykiss <- read_tsv('mykiss_regression.tsv')
phoxinus <- read_tsv('phoxinus_regression.tsv')
all_fish <- read_csv('all_fish.csv')

trutta <- read_tsv('trutta_regression.tsv')

#flesus
lm_model <- lm(SPCSNO ~ Concentration, data = flesus)
summary(lm_model)

ggplot(flesus, aes(x = Concentration, y = SPCSNO)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Linear Regression Flesus", x = "Concentration", y = "SPCSNO")

#anguilla
lm_model_anguilla <- lm(SPCSNO ~ Concentration, data = anguilla)
summary(lm_model_anguilla)

ggplot(anguilla, aes(x = Concentration, y = SPCSNO)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Linear Regression Anguilla", x = "Concentration", y = "SPCSNO")

#rutilus
lm_model_rutilus <- lm(SPCSNO ~ Concentration, data = rutilus)
summary(lm_model_rutilus)

ggplot(rutilus, aes(x = Concentration, y = SPCSNO)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Linear Regression Rutilus", x = "Concentration", y = "SPCSNO")

#auratus
lm_model_auratus <- lm(SPCSNO ~ Concentration, data = auratus)
summary(lm_model_auratus)


ggplot(auratus, aes(x = Concentration, y = SPCSNO)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Linear Regression Auratus", x = "Concentration", y = "SPCSNO")

#carpio
lm_model_carpio <- lm(SPCSNO ~ Concentration, data = carpio)

summary(lm_model_carpio)

ggplot(carpio, aes(x = Concentration, y = SPCSNO)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Linear Regression Carpio", x = "Concentration", y = "SPCSNO")

#mykiss
lm_model_mykiss <- lm(SPCSNO ~ Concentration, data = mykiss)
summary(lm_model_mykiss)


ggplot(mykiss, aes(x = Concentration, y = SPCSNO)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Linear Regression Mykiss", x = "Concentration", y = "SPCSNO")

#phoxinus
lm_model_phoxinus <- lm(SPCSNO ~ Concentration, data = phoxinus)
summary(lm_model_phoxinus)

ggplot(phoxinus, aes(x = Concentration, y = SPCSNO)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Linear Regression Phoxinus", x = "Concentration", y = "SPCSNO")

#trutta
lm_model_trutta <- lm(SPCSNO ~ Concentration, data = trutta)
summary(lm_model_trutta)


ggplot(trutta, aes(x = Concentration, y = SPCSNO)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Linear Regression Trutta", x = "Concentration", y = "SPCSNO")

#all fish
#flesus
lm_model_all <- lm(SPCSNO ~ Concentration, data = all_fish)
summary(lm_model_all)

ggplot(all_fish, aes(x = Concentration, y = SPCSNO)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Linear Regression All Fish", x = "Concentration", y = "SPCSNO")

