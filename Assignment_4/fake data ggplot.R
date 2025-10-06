library(ggplot2)

fake_data <- read.csv("C:/Users/oswal/Desktop/Data_Course_PLANCARTE/Assignment_4/fake_gene_expression.csv")

head(fake_data)

library(ggplot2)

ggplot(fake_data, aes(x = condition, y = GeneA, fill = condition)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "geneA expression in healthy vs. cancer tissue",
    x = "condition",
    y = "geneA Expression"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Healthy" = "lightblue", "Cancer" = "pink"))
