models$condition

models$condition$finalModel
coefficients(models$condition$finalModel)
models$condition$modelInfo

library(klaR)
library(psych)
library(MASS)
library(ggord)

training <- condition_training
training[condition == "dead", condition := "Dead"]
training[condition == "wilted", condition := "Wilted"]
training[condition == "healthy", condition := "Healthy"]
training$condition <- factor(training$condition, levels = c("Healthy", "Wilted", "Dead"))

training

linear <- lda(condition ~., training)
p <- predict(linear, training)
ldahist(data = p$x[,1], g = training$condition)
ldahist(data = p$x[,2], g = training$condition)


a <- ggord(linear, 
           training$condition, 
           cols = c("#228b22", "#FF0000", "grey40"),
           alpha_el = 0.2,
           size = 2,
           repel = TRUE,
           vec_ext = 250,
           grp_title = "Condition")

tiff("figures/Figure_lda.tif", width = 20, height = 15, units = "cm", res = 600)

a

dev.off()

a <- as.data.table(p$x)
a$condition <- training$condition
a$condition <- factor(a$condition, levels = c("Healthy", "Wilted", "Dead"))

ggplot(a, aes(LD1, fill = condition)) + geom_density(alpha = 0.2)
