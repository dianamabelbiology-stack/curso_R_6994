
library(ggplot2)
library(car)
install.packages("janitor")
# Cargar datos
tyre <- read.csv("data/tyre.csv")
head(tyre)

# Boxplot
p <- ggplot(tyre, aes(x = Brands, y = Mileage)) +
  geom_boxplot(aes(fill = Brands)) +
  
  # Labels
  labs(title = "Gomas en Puerto Rico", 
       x = "Gomas", 
       y = "Kilometers",
       fill = "Car Brands") +
  
  # Temas
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5, color = "red"),
        axis.title.y = element_text(size = 18, face = "bold", color = "blue"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title.x = element_text(size = 18, face = "bold", color = "blue"),
        axis.text.x = element_text(size = 14, color = "black"),
        legend.title = element_text(size = 18, color = "red")) +
  
  coord_cartesian(ylim = c(10, 100))   # Mejor que ylim()

# ANOVA -------------------------------------------------------
mod <- aov(Mileage ~ Brands, data = tyre)
summary(mod)

# Prueba de normalidad
resid_anova <- resid(mod)
shapiro.test(resid_anova)

# Homogeneidad de varianzas
leveneTest(Mileage ~ Brands, data = tyre)

# Prueba post-hoc
TukeyHSD(mod)

# Guardar gráfico --------------------------------------------------------
p
ggsave(filename = "plot.png",
       plot = p,
       width = 8,
       height = 6,
       dpi = 300)
