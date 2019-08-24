# ******************************************************************************
# ******************************************************************************

# ===================================================
# Load and download (if necessary) required packages
# ===================================================

# use (and install if necessary) pacman package 
if (!require("pacman")) install.packages("pacman")
library(pacman)

# load and install (if necessary) required packages for this course
p_load(TraMineR, tidyverse, haven, knitr, 
       RColorBrewer, colorspace, colortools)

# ******************************************************************************

# import stata dataset using haven 
# assuming that data are in current working directory
family <- read_dta("12_PartnerBirthbio.dta")

# looking for the variable names of the sequence variables
names(family)

# ******************************************************************************

# extracting the sequence variables (which all start with state)
seqvars <- family %>%
  select(starts_with("state"))

#names(attributes(family$state1)$labels)
#seqvars <- as_factor(seqvars)
#ttt <- levels(as_factor(seqvars)$state1)

t3 <- data.frame(
  No = seq(1:9),
  states = levels(as_factor(seqvars)$state1)
)

# ******************************************************************************

#test <- seqdef(seqvars, alphabet = ttt, label = ttt, states = 1:9)


shortlab <- c("S-NC", "S-C", "LAT-NC", "LAT-C", "COH-NC", "COH-C", 
              "MAR-NC", "MAR-1C", "MAR-2+C")

longlab <-  names(attributes(seqvars$state1)$labels)


# ******************************************************************************


pal <- function(col, border="light gray", ...) {
  n <-length(col)
  plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1),
       axes = FALSE, xlab = "", ylab = "", ...)
  rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
}

# ******************************************************************************

# There exist a whole bunch of excellent tools for choosing colors
# we illustrate how to use "RColorBrewer", "colorspace", and "colortools" to choose colors
# If You are using R Studio you also might want to consider the very helpful package "colourpicker"


# Choosing colors with the pre-defined color palettes of RColorBrewer

png(file = "brewerpal.png",
    width = 800, height = 600, res = 80)
par(mar = c(0, 4, 0, 2) + 0.1)
display.brewer.all()
dev.off()

col1 <- brewer.pal(3, "Blues")[2:3]
col2 <- brewer.pal(3, "Greens")[2:3]
col3 <- brewer.pal(3, "Oranges")[2:3]
col4 <- brewer.pal(7, "Purples")[5:7]

colspace1 <- c(col1, col2, col3, col4)


# Choosing colors with the pre-defined color palettes of colorspace

png(file = "hcl_palettes.png",
    width = 1000, height = 800, res = 90)
hcl_palettes(plot = TRUE)
dev.off()

col1 <- sequential_hcl(5, palette = "Blues")[3:2]
col2 <- sequential_hcl(5, palette = "Greens")[2:1]
col3 <- sequential_hcl(5, palette = "Oranges")[3:2]
col4 <- sequential_hcl(5, palette = "magenta")[3:1]

colspace2 <- c(col1, col2, col3, col4)


# Choosing colors with colortools
col1 <- sequential("royalblue", 20, plot = FALSE)[3:4]
col2 <- sequential("green3", 20, plot = FALSE)[c(2,6)]
col3 <- sequential("darkorange", 20, plot = FALSE)[5:6]
col4 <- sequential("magenta 3", 15, plot = FALSE)[c(3,5,7)]

colspace3 <- c(col1, col2, col3, col4)


png(file = "colorpals.png", 
    width = 800, height = 600, res = 120)

par(mfrow=c(3,1))
pal(colspace1, main = "RColorBrewer")
pal(colspace2, main = "colorspace")
pal(colspace3, main = "colortools")
dev.off()


swatchplot("RColorBrewer" = colspace1,
           "colorspace" = colspace2,
           "colortools" = colspace3)

# ******************************************************************************


fam.seq <- seqdef(seqvars, 
                  states = shortlab, labels = longlab, alphabet = c(1:9),  
                  weights = family$weight40,
                  id = family$id,
                  xtstep = 24,)

### First visual check



png(file = "dplot_colorpals.png", 
    width = 800, height = 1000, res = 120)

par(las = 1)

layout.fig1 <- layout(matrix(seq(1:6), 3, 2, byrow = TRUE),
                      widths = c(.7, .3))
layout.show(layout.fig1)

par(mar = c(5, 4, 4, 0) + 0.1)
seqdplot(fam.seq, xlab = "Age", ylab = "Relative frequency",
         with.legend = "FALSE", border = NA, xtlab = seq(18,40, by = 1/12),
         cpal = colspace1, main = "RColorBrewer")

par(mar = c(5, 0, 4, 0) + 0.1)
seqlegend(fam.seq, cpal = colspace1, cex = .95, position = "left")

par(mar = c(5, 4, 4, 0) + 0.1)
seqdplot(fam.seq, xlab = "Age", ylab = "Relative frequency",
         with.legend = "FALSE", border = NA, xtlab = seq(18,40, by = 1/12),
         cpal = colspace2, main = "colorspace")

par(mar = c(5, 0, 4, 0) + 0.1)
seqlegend(fam.seq, cpal = colspace2, cex = .95, position = "left")

par(mar = c(5, 4, 4, 0) + 0.1)
seqdplot(fam.seq, xlab = "Age", ylab = "Relative frequency",
         with.legend = "FALSE", border = NA, xtlab = seq(18,40, by = 1/12),
         cpal = colspace3, main = "colortools")

par(mar = c(5, 0, 4, 0)  + 0.1)
seqlegend(fam.seq, cpal = colspace3, cex = .95, position = "left")
dev.off()


# ******************************************************************************

save.image("Test.RData")
