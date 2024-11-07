library(MANOVA.RM)
library(MASS)

######
# ANALYSE MANOVA
######

data(swiss)

str(swiss)
plot(swiss$Catholic)

swiss$Agriculture <- as.factor(ifelse(swiss$Agriculture < 50, "1", "2"))
swiss$Examination <- as.factor(ifelse(swiss$Examination < 20, "1", "2"))
swiss$Education <- as.factor(ifelse(swiss$Education < 10, "1", ifelse(swiss$Education < 20, "2", "3")))
swiss$Catholic <- as.factor(ifelse(swiss$Catholic < 50, "1", "2"))

str(swiss)

# Exécution de la MANOVA
result <- manova(cbind(Fertility, Infant.Mortality) ~ Agriculture + Education + Catholic + Examination, data = swiss)

cor(swiss$Fertility,swiss$Infant.Mortality) #[1] 0.416556

# Résultats
summary(result)

result2 <- manova(cbind(Fertility, Infant.Mortality) ~ Education + Catholic, data = swiss)
summary(result2)

coefficients <- result2$coefficients
coefficients

#####
# REPRESENTATION 
#####
library(ggplot2)

ggplot(swiss, aes(x = Fertility, y = Infant.Mortality, color = Education)) +
  geom_point(size = 2) +                      # Nuage de points
  stat_ellipse(level = 0.95, linetype = "dashed") + # Ellipses de confiance à 95%
  labs(title = "Effet de la variable Education",
       x = "Fertility",
       y = "Infant.Mortality") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(swiss, aes(x = Fertility, y = Infant.Mortality, color = Catholic)) +
  geom_point(size = 2) +                      # Nuage de points
  stat_ellipse(level = 0.95, linetype = "dashed") + # Ellipses de confiance à 95%
  labs(title = "Effet de la variable Catholic",
       x = "Fertility",
       y = "Infant.Mortality") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(swiss, aes(x = Fertility, y = Infant.Mortality, color = Agriculture)) +
  geom_point(size = 2) +                      # Nuage de points
  stat_ellipse(level = 0.95, linetype = "dashed") + # Ellipses de confiance à 95%
  labs(title = "Effet de la variable Agriculture",
       x = "Fertility",
       y = "Infant.Mortality") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(swiss, aes(x = Fertility, y = Infant.Mortality, color = Examination)) +
  geom_point(size = 2) +                      # Nuage de points
  stat_ellipse(level = 0.95, linetype = "dashed") + # Ellipses de confiance à 95%
  labs(title = "Effet de la variable Examination",
       x = "Fertility",
       y = "Infant.Mortality") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Exécution de l'analyse discriminante
lda_fit <- lda(Education ~ Fertility + Infant.Mortality, data = swiss)
print(lda_fit)

lda_fit2 <- lda(Catholic ~ Fertility + Infant.Mortality, data = swiss)
print(lda_fit2)