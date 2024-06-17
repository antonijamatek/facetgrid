#install packages
install.packages("tidyverse")
install.packages("tibble")
install.packages("ggplot2")
install.packages("readr")
install.packages("dichromat")

#load packages
library(tidyverse)
library(tibble)
library(ggplot2)
library(readr)
library(dichromat)

#import data
phytopico22 <- readr::read_delim("phytopico22.csv",col_types = cols(Groups = col_factor(), Class = col_factor()))

#edit data
phytopico_filtered <- subset(phytopico22, Counts > 0)

#attributes for the graph
shape_names <- c(
  "circle", paste("circle", c("open", "filled", "cross", "plus", "small")), "bullet",
  "square", paste("square", c("open", "filled", "cross", "plus", "triangle")),
  "diamond", paste("diamond", c("open", "filled", "plus")),
  "triangle", paste("triangle", c("open", "filled", "square")),
  paste("triangle down", c("open", "filled")),
  "plus", "cross", "asterisk"
)
vignette("ggplot2-specs")

#defining colors for the plot 
shape <- c("circle", "square", "diamond", "triangle", "cross", "asterisk")
color <- c("#458B74", "#CD3333", "#8968CD", "#8B795E", "#CD3700", "#0000CD")


#defining exponential format for y axis numbers
fancy_scientific <- function(l) {
  # turn in to character string in scientific notation
  l <- format(l, scientific = TRUE)
  # quote the part before the exponent to keep all the digits
  l <- gsub("(^.*)e", "'\\1'e", l)
  # remove + after exponent, if exists. E.g.: (3x10^+2 -> 3x10^2) 
  l <- gsub("e\\+","e",l) 
  # convert 1x10^ or 1.000x10^ -> 10^ 
  l <- gsub("\\'1[\\.0]*\\'\\%\\*\\%", "", l) 
  # turn the 'e+' into plotmath format
  l <- gsub("e", "%*%10^", l)
  # return this as an expression
  parse(text=l)
} 

#plot
ggplot(phytopico_filtered, aes(Day, Counts))+
  geom_point(aes(color = Variables, shape = Variables), size=3.6)+
  scale_shape_manual(values = shape)+
  scale_colour_manual(values = color)+
  facet_grid(Class ~ Groups, scales = "free")+
  scale_y_log10(name=c("Cell counts"), labels= fancy_scientific)+
  scale_x_discrete(name="Experiment days (d) - Time (h) ")+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
  theme(axis.text.y = element_text(size = rel(2), angle = 00))+
  theme(axis.text.x = element_text(size = rel(1.6), angle = 90))+
  theme(axis.title.y = element_text(size = rel(1.7), angle = 90))+
  theme(axis.title.x = element_text(size = rel(1.7), angle = 00))