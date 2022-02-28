library(ggplot2)
library(ggtree)
library(treeio)
# Lambodhar Damodaran 
# Visualize multiple MCC trees together

beast_tree <- read.beast("BEAST_HA.tree")
beast_tree2 <- read.beast("BEAST_NA.tree")


##################################################
p1 <-  ggtree(beast_tree, mrsd='2019-05-23') +
  theme_tree2(panel.grid.major.x=element_line()) + 
  ggtitle("    HA") +
  geom_range(range='height_0.95_HPD', color='blue', alpha=2, size=.3) +
  geom_tippoint(aes(subset = (node == 303)), , size=3, shape=23, fill="steelblue") +
  geom_highlight(node = 311 , fill = "red")  +
  scale_x_ggtree(breaks = c(2005,2010,2015,2020))

p1

###################################################
p2 <- ggtree(beast_tree2, mrsd='2019-07-01') +
  theme_tree2(panel.grid.major.x=element_line()) + 
  ggtitle("    NA") +
  geom_range(range='height_0.95_HPD', color='blue', alpha=2, size=.3) +
  geom_tippoint(aes(subset = (node == 419)), , size=3, shape=23, fill="steelblue") +
  scale_x_ggtree(breaks = c(2005,2010,2015,2020))


  p2

####################################################


cowplot::plot_grid(p1,p2, nrow=1, labels = LETTERS[1:2])



