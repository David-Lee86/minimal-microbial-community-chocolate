library("ggplot2")
library("readxl")
library("dplyr")
library(knitr)
library(rmdformats)
library(ggpubr)
library(data.table)
library(scales)
library("BiocParallel")
library(tidyverse)

register(SnowParam(6))
df <- read_excel("colour_analysis.xlsx", sheet = "PCA")
df2 = df %>% remove_rownames %>% column_to_rownames(var="Id")
df2 <- df2 %>% select (-c(Time, Rep))
pca_result <- prcomp(df2, center = TRUE, scale. = TRUE)
pca_result
summary(pca_result)
plot(pca_result, type = "l",main="")
variance_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
pca_df <- as.data.frame(pca_result$x[, 1:2])
pca_df <- tibble::rownames_to_column(pca_df, "Id")
total <- merge(df,pca_df,by="Id")
total$Time<- factor(total$Time, levels = c('0hr',"24hr","48hr","72hr","96hr","120hr","144hr","168hr"))
mypal = c("#437076","#6c9c9e","#92c6c1","#e8e2d2","#f3cec8","#fa98ad","#dd7959",'#8e5e4d')
ggplot(total, aes(x = PC1, y = PC2)) +
  geom_point() + 
  labs(x = gsub("\\(\\s*", "(", gsub("\\s*\\)", ")", paste("Axis 1 (", scales::percent(variance_explained[1]), ")"))),
       y = gsub("\\(\\s*", "(", gsub("\\s*\\)", ")", paste("Axis 2 (", scales::percent(variance_explained[2]), ")")))) +
  geom_hline(yintercept = 0, colour="gray70", linetype="dashed") +
  geom_vline(xintercept = 0, colour="gray70", linetype="dashed") +
  geom_point(aes(fill=Time, color=Time), shape = 21, size = 5, colour = "black",stroke=0.4) +
  scale_fill_manual(values = mypal) +
  theme_classic() + guides(color=FALSE, shape=FALSE) + guides(fill = guide_legend(title="Sample")) +
  theme(axis.text.y = element_text(angle=0, hjust = 0, vjust = 0.5, size=11, color = "black"),
        axis.text.x = element_text(angle=0, hjust = 0.5, vjust = 1, size=11, color = "black"),
        axis.title.y = element_text(size=11, face="bold"),
        axis.title.x = element_text(size=11, face="bold"),
        legend.title = element_blank(),
        legend.text=element_text(size=11),
        legend.key.size = unit(0.07, "cm"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black", size = 0.05),
        panel.border = element_rect(colour = "black", fill=NA, size=0.35), plot.title = element_text(hjust = 0.5, face="bold")) +
  theme(legend.position="bottom") +
  theme(plot.margin=unit(c(0.1,1,0.1,0.1),"cm")) +
  theme(legend.margin=margin(c(0,0,0,0))) +
  guides(fill=guide_legend(nrow=2, override.aes = list(size = 5)))
