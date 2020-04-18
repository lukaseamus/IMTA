##### Project: Review of literature on plant-based integrated multi-trophic aquaculture (IMTA)
##### Title: Integrating plants improves aquaculture

#### Required packages
require(ggplot2)
require(forcats)
require(ggpubr)
require(esc)
require(meta)
require(maptools)
require(raster)
require(ggnewscale)
require(rworldmap)
require(png)

#### Set base theme for plotting
mytheme <- theme(panel.background = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 axis.line = element_line(),
                 axis.title = element_text(size = 15),
                 axis.text = element_text(size = 12, colour = "black"),
                 axis.ticks.length = unit(.25, "cm"),
                 axis.ticks = element_line(colour = "black"),
                 legend.key = element_blank(),
                 legend.text = element_text(size = 12),
                 legend.text.align = 0,
                 legend.title = element_text(size = 12, face = "bold"),
                 text = element_text(family = "Helvetica Neue"))


#### Figure 1. Categorisation of literature
### Citations
## Load data
citations <- read.csv("~/Desktop/Plymouth University/MBIO363/IMTA/Data/citations.csv")

## Plot data
citations.plot <- ggplot(data = citations, aes(year, citations, fill = group)) +
                    geom_area() +
                    scale_fill_manual(values = c("#5b5b5e","#bdd268"),
                                      labels = c("Not plant-related", "Plant-related"),
                                      guide = guide_legend(title = "Focus of literature")) +
                    labs(x = "Year", y = expression("Citations (yr"^-1*")")) +
                    scale_x_continuous(breaks = seq(2001, 2019, by = 3)) +
                    scale_y_continuous(breaks = seq(0, 1000, by = 200)) +
                    coord_cartesian(xlim = c(2000.904, 2019),
                                    ylim = c(48.57, 1020)) +
                    theme(legend.position = c(.21, .9)) +
                    mytheme

citations.plot # print (save as 4.5 x 5.5 in)


### Findings
## Load data
findings <- read.csv("~/Desktop/Plymouth University/MBIO363/IMTA/Data/findings.csv")

## Plot data
findings.plot <- ggplot(data = findings, aes(fct_relevel(findings, "+", "="), papers)) +
                    geom_col(aes(fill = fct_relevel(purpose, "not specified", "other",
                                                    "animal feed", "human nutrition",
                                                    "productivity")), width = 0.7) +
                    scale_fill_manual(values = c("#cdd0d1","#5b5b5e","#fac67e",
                                                 "#c5add0","#bdd268","#cae4f0"),
                                      labels = c("Not specified", "Other", "Animal feed",
                                                 "Human nutrition", "Productivity", "Biofiltration"),
                                      guide = guide_legend(title = "Primary purpose")) +
                    labs(y = expression("Papers")) +
                    scale_y_continuous(breaks = seq(0, 100, by = 20)) +
                    coord_cartesian(ylim = c(4.76, 100)) +
                    theme(legend.position = c(.71, .78),
                          axis.ticks.x = element_blank(), # modify base theme
                          axis.title.x = element_blank(),
                          axis.text.x = element_text(size = 30)) +
                    mytheme

findings.plot # print (save as 4.5 x 3.5 in)


### Plants
## Load data
plants <- read.csv("~/Desktop/Plymouth University/MBIO363/IMTA/Data/plants.csv")

## Plot data
plants.plot <- ggplot(data = plants, aes(fct_relevel(plant, "macroalgae", "angiosperms"), papers)) +
                  geom_col(aes(fill = fct_relevel(animal, "polychaete", "decapod", "echinozoan", 
                                                  "mollusc")), width = 0.7) +
                  scale_fill_manual(values = c("#5b5b5e","#fac67e","#c5add0",
                                               "#bdd268","#cae4f0"),
                                    labels = c("Polychaete", "Decapod", "Echinozoan", 
                                               "Mollusc", "Fish"),
                                    guide = guide_legend(title = "Co-cultured animal")) +
                  labs(y = expression("Papers")) +
                  scale_y_continuous(breaks = seq(0, 200, by = 20)) +
                  scale_x_discrete(labels = c("","","")) +
                  coord_cartesian(ylim = c(6.66, 140)) +
                  theme(legend.position = c(.71, .81),
                        axis.ticks.x = element_blank(), # modify base theme
                        axis.title.x = element_blank(),
                        axis.text.x = element_text(size = 30)) +
                  mytheme

plants.plot # print (save as 4.5 x 3.5 in)


### Combine plots
fig.1 <- ggarrange(citations.plot, findings.plot, plants.plot,
                   widths = c(1.571428571428571, 1, 1), ncol = 3,
                   nrow = 1, labels = c("a", "b", "c"))

fig.1 # print (save as 4.5 x 12.5 in)



#### Figure 2. Data extraction and meta-analysis
### Ammonium uptake
## Load data
ammonium <- read.csv("~/Desktop/Plymouth University/MBIO363/IMTA/Data/ammonium.csv")

## Plot data
ammonium.plot <- ggplot(data = ammonium, aes(fct_reorder(id, mean), mean)) +
                        geom_col(fill = "#cdd0d1", width = 0.7) +
                        geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                                      width = .1, lwd = .4) +
                        geom_label(aes(fct_reorder(id, mean), rep(1.5, 4), label = n), 
                                   label.size = 0, label.r = unit(0, "lines")) +
                        ylab(expression("NH"[4]^"+"*" uptake ("*mu*"mol g"^-1*" h"^-1*")")) +
                        scale_x_discrete(labels = c(expression(italic("Agarophyton vermiculophyllum")),
                                                    expression(italic("Agarophyton vermiculophyllum")),
                                                    expression(italic("Undaria pinnatifida")),
                                                    expression(italic("Codium fragile")))) +
                        coord_flip(ylim = c(0.95, 20)) +
                        theme(axis.ticks.y = element_blank(), # modify base theme
                              axis.title.y = element_blank()) +
                        mytheme

ammonium.plot # print (save as 4.5 x 5 in)


### Biofiltration
## Load data
biofiltration <- read.csv("~/Desktop/Plymouth University/MBIO363/IMTA/Data/biofiltration.csv")

## Calculate Hedges' g and associated variables
g <- with(biofiltration, esc_mean_se(grp1m = mean.imta, grp1se = se.imta, grp1n = n.imta, 
                                     grp2m = mean.control, grp2se = se.control, grp2n = n.control))

## Add effect sizes, standard errors and 95% confidence intervals to data frame
biofiltration <- data.frame(biofiltration, g = g[c(1:2,4:5)])

## Compute overall effect size, 95% confidence interval and p value
m <- metagen(data = biofiltration, g.es, g.se, studlab = id, prediction = T, sm = "SMD")
m # p < 0.001

## Create new data frame with individual and combined effect sizes and 95% confidence intervals
bio <- rbind(biofiltration[,c(1,12,14:15)], data.frame(id = "T", g.es = m$TE.fixed, 
                                                       g.ci.lo = m$lower.fixed, g.ci.hi = m$upper.fixed))

## Plot data
biofiltration.plot <- ggplot(data = bio, aes(fct_relevel(id,"T","A1","S1"), g.es)) +
                        geom_pointrange(aes(ymin = g.ci.lo, ymax = g.ci.hi), size = 0.5,
                                        colour = c(rep("#000000",3),"#e64715")) +
                        ylab(expression("Effect on biofiltration (Hedges' "*italic("g")*")")) +
                        scale_x_discrete(labels = c("Combined                                                            .",
                                                    expression("Abreu et al. (2009)  "*
                                                               italic("Agarophyton chilense")),
                                                    expression("Marinho et al. (2015)  "*
                                                               italic("Saccharina latissima")),
                                                    expression("Troell et al. (1997)  "*
                                                               italic("Agarophyton chilense")))) +
                        scale_y_continuous(breaks = seq(0, 7, by = 1)) +
                        coord_flip(ylim = c(0.323, 6.8)) +
                        theme(axis.title.y = element_blank()) + # modify base theme
                        mytheme

biofiltration.plot


### Growth
## Load data
growth <- read.csv("~/Desktop/Plymouth University/MBIO363/IMTA/Data/growth.csv")

## Calculate Hedges' g and associated variables
g <- with(growth, esc_mean_se(grp1m = mean.imta, grp1se = se.imta, grp1n = n.imta, 
                              grp2m = mean.control, grp2se = se.control, grp2n = n.control))

## Add effect sizes, standard errors and 95% confidence intervals to data frame
growth <- data.frame(growth, g = g[c(1:2,4:5)])

## Compute overall effect size, 95% confidence interval and p value
m <- metagen(data = growth, g.es, g.se, studlab = id, prediction = T, sm = "SMD")
m # p < 0.001

## Create new data frame with individual and combined effect sizes and 95% confidence intervals
gro <- rbind(growth[,c(1,12,14:15)], data.frame(id = "T", g.es = m$TE.fixed, 
                                                g.ci.lo = m$lower.fixed, g.ci.hi = m$upper.fixed))

## Plot data
growth.plot <- ggplot(data = gro, aes(fct_relevel(id,"T","A1","K1","P1","K2","S3","S1","A2","G1",
                                                  "S2","E1","S4","U1","U2"), g.es)) +
                        geom_pointrange(aes(ymin = g.ci.lo, ymax = g.ci.hi), size = 0.5,
                                        colour = c(rep("#000000",13),"#e64715")) +
                        ylab(expression("Effect on growth (Hedges' "*italic("g")*")")) +
                        scale_x_discrete(labels = c("Combined",
                                                    expression("Abreu et al. (2009)  "*
                                                               italic("Agarophyton chilense")),
                                                    expression("Kunzmann et al. (2018)  "*
                                                               italic("Kappaphycus striatus")),
                                                    expression("Sanderson et al. (2012)  "*
                                                               italic("Palmaria palmata")),
                                                    expression("Kunzmann et al. (2018)  "*
                                                                 italic("Kappaphycus alvarezii")),
                                                    expression("Sanderson et al. (2012)  "*
                                                                 italic("Saccharina latissima")),
                                                    expression("Marinho et al. (2015)  "*
                                                                 italic("Saccharina latissima")),
                                                    expression("Troell et al. (1997)  "*
                                                                 italic("Agarophyton chilense")),
                                                    expression("Korzen et al. (2016)  "*
                                                                 italic("Gracilaria bursa-pastoris")),
                                                    expression("Wang et al. (2014)  "*
                                                                 italic("Saccharina latissima")),
                                                    expression("Namukose et al. (2016)  "*
                                                                 italic("Eucheuma denticulatum")),
                                                    expression("Fossberg et al. (2018)  "*
                                                                 italic("Saccharina latissima")),
                                                    expression("Korzen et al. (2016)  "*
                                                                 italic("Ulva rigida")),
                                                    expression("Nardelli et al. (2019)  "*
                                                                 italic("Ulva lactuca")))) +
                        scale_y_continuous(breaks = seq(0, 46, by = 5)) +
                        coord_flip(ylim = c(1.5, 45.5)) +
                        theme(axis.title.y = element_blank()) + # modify base theme
                        mytheme

growth.plot


### Combine plots
fig.2 <- ggarrange(ammonium.plot, ncol = 2, labels = "a", widths = c(1, 1.5),
                   ggarrange(biofiltration.plot, growth.plot,
                             nrow = 2, heights = c(1, 2.29), labels = c("b", "c")))

fig.2 # print (save as 4.5 x 12.5 in)



#### Figure 3. Climate change mitigation, adaptation and economics
### World map
## Load data
# Seaweed production and income per country (http://www.fao.org/3/ca5495t/ca5495t.pdf)
production <- read.csv("~/Desktop/Plymouth University/MBIO363/IMTA/Data/production.csv")

# Country polygon data
data("wrld_simpl")
wrld_simpl@data$id <- wrld_simpl@data$NAME
land <- fortify(wrld_simpl, region = "id")

# Areas of coastal hypoxia (doi: 10.1126/science.1156401)
hypoxia <- read.csv("~/Desktop/Plymouth University/MBIO363/IMTA/Data/hypoxia.csv")

# Mean present sea surface temperature data (https://www.bio-oracle.org/downloads-to-email.php)
present.SST <- brick("~/Desktop/Plymouth University/MBIO363/IMTA/Data/Present.Surface.Temperature.Mean.asc")

# Mean future (2100) sea surface temperature data (RCP8.5)
future.SST <- brick("~/Desktop/Plymouth University/MBIO363/IMTA/Data/2100AOGCM.RCP85.Surface.Temperature.Mean.asc")

# Caclulate climate differential, i.e. change in SST between now and 2100
change.SST <- future.SST - present.SST
change.SST
change.SST.df <- raster::as.data.frame(change.SST, xy = T) # convert RasterLayer to data frame
str(change.SST.df)
change.SST.df[is.na(change.SST.df)] <- 0 # replace NAs with zeros
range(change.SST.df$layer) # some areas are predicted to cool by 0.3°C

## Plot data
worldmap <- ggplot() +
              coord_fixed(xlim = c(-164, 164), ylim = c(-82, 82)) +
              geom_raster(data = change.SST.df, aes(x = x, y = y, fill = layer)) +
              scale_fill_gradient2(expression(bold(Delta*" SST (°C)")), low = "#003875",
                                   high = "#610819", mid = "#ffffff", limits = c(-1, 7),
                                   guide = guide_colourbar(ticks = F, barheight = 8,
                                                           barwidth = 1.5,
                                                           raster = T, order = 1)) +
              new_scale("fill") +
              geom_polygon(data = land, aes(x = long, y = lat, group = group),
                           fill = "#ffffff", colour = "#dbdddf", size = 0.2) +
              geom_map(data = production, map = land, colour = "#dbdddf", size = 0.2,
                       aes(map_id = country, fill = fct_relevel(category, "c","b"))) +
              scale_fill_manual(values = c("#5b5b5e","#898b8e","#cdd0d1"),
                                labels = c(">500","100–500","<100"),
                                guide = guide_legend(title = expression(bold("Income ($ t"^"-1"*")")),
                                                                        keywidth = 1.5, keyheight = 1.5)) +
              geom_point(data = hypoxia, aes(x = long, y = lat), colour = "#000000", size = 0.3) +
              scale_y_continuous(breaks = seq(-90, 90, by = 30)) +
              scale_x_continuous(breaks = seq(-150, 150, by = 50)) +
              mytheme +
              theme(axis.title = element_blank(),  # modify base theme
                    axis.line = element_blank(),
                    panel.border = element_rect(fill = NA, size = 1))

worldmap # print (save as 4.5 x 10 in)


### Detailed map
## Load high resolution map data
UK <- getMap(resolution = "high")

## Calculate seaweed carbon offset area 
# area of 474 km^2 of seaweed farms to offset global fish and 
# crustacean aquaculture (doi: 10.1016/j.cub.2019.07.041)
# 20 x 23.7 km = 474 km^2
# position: 51°N, 6°W

20/(cos((pi*51)/180)*111.321) # 0.2854836 = 20 km in degrees longitude at 51°N
23.7/111 # 0.2135135 = 23.7 km in degrees latitude at 51°N

## Plot data
map <- ggplot() +
          coord_map(xlim = c(-8.66, -1.35), ylim = c(48.53, 59.47)) +
          geom_polygon(data = UK, aes(x = long, y = lat, group = group),
                       fill = "#5b5b5e", colour = "#dbdddf", size = 0.2) +
          geom_rect(aes(xmin = -6, xmax = -6.213514, ymin = 51, ymax = 51.28548), 
                    fill = "#bdd268", colour = "#5b5b5e", size = 0.2) +
          scale_x_continuous(breaks = seq(-1, -9, by = -2)) +
          scale_y_continuous(breaks = seq(48, 60, by = 2)) +
          mytheme +
          theme(axis.title = element_blank(),  # modify base theme
                axis.line = element_blank(),
                panel.border = element_rect(fill = NA, size = 1))

map # print (save as 4.5 x 3 in)


#### Figure 4. Literature imbalance
### Seaweed production vs. research effort
## Load data
countries <- read.csv("~/Desktop/Plymouth University/MBIO363/IMTA/Data/countries.csv")

## Extract data for China, Indonesia, Malaysia, Norway, Canada, Portugal
countries <- countries[c(1:3,10,31:32,34:37),]

## Calculate log10 of seaweed production
logprod <- log10(countries$production + 1)
range(logprod)

## Calculate constant for double y axis
c <- 7.243871/33 # log10 seaweed production of China / number of papers of Portugal

## Plot data
countries.plot <- ggplot(data = countries, aes(fct_reorder(country, papers, .desc = T))) +
                    geom_col(aes(y = papers, fill = fct_relevel(type, "review")), width = 0.7) +
                    scale_fill_manual(values = c("#5b5b5e","#cdd0d1"),
                                      labels = c("Review", "Research"),
                                      na.translate = F,
                                      guide = guide_legend(title = "Article type")) +
                    geom_point(aes(y = logprod/c), colour = "#5b5b5e", fill = "#bdd268", size = 6, shape = 21) +
                    labs(y = expression("Papers")) +
                    scale_x_discrete(labels = c("PT", "CN", "CA", "NO", "ID", "MY")) +
                    scale_y_continuous(breaks = seq(0, 40, by = 10),
                                       sec.axis = sec_axis(~.*c, name = expression("Seaweed aquaculture production (t yr"^-1*")"),
                                       breaks = seq(0, 8, by = 2), 
                                       labels = c(0, expression("10"^2), expression("10"^4),
                                                  expression("10"^6), expression("10"^8)))) +
                    coord_cartesian(ylim = c(1.88, 39.5)) +
                    theme(legend.position = c(.71, .91),
                          axis.ticks.x = element_blank(), # modify base theme
                          axis.title.x = element_blank(),
                          legend.background = element_blank()) +
                    mytheme

countries.plot # print (save as 4.5 x 3.5 in)


### Seaweed taxa
## Load data
seaweeds <- read.csv("~/Desktop/Plymouth University/MBIO363/IMTA/Data/seaweeds.csv")

## Extract relevant data
seaweeds <- seaweeds[1:9,]

## Calculate constant for double y axis
c <- 8219000/38

## Plot data
seaweeds.plot <- ggplot(data = seaweeds, aes(fct_reorder(seaweed, papers))) +
                    geom_col(aes(y = papers), fill = "#cdd0d1", width = 0.7) +
                    geom_point(aes(y = production/c), colour = "#5b5b5e", fill = "#bdd268", size = 6, shape = 21) +
                    labs(y = expression("Papers")) +
                    scale_x_discrete(labels = c(expression(italic("Kappaphycus alvarezii")),
                                                expression(italic("Eucheuma denticulatum")),
                                                expression(italic("Laminaria digitata")),
                                                expression(italic("Undaria pinnatifida")),
                                                expression(italic("Porphyra")*" spp."),
                                                expression(italic("Saccharina latissima")),
                                                expression(italic("Saccharina japonica")),
                                                expression(italic("Ulva")*" spp."),
                                                expression(italic("Gracilaria")*" spp."))) +
                    scale_y_continuous(breaks = seq(0, 40, by = 10),
                                       sec.axis = sec_axis(~.*c, name = expression("Global aquaculture production (t yr"^-1*")"),
                                       breaks = seq(0, 9*10^6, by = 3*10^6), labels = c(0, expression("3×10"^6), expression("6×10"^6),
                                                                                        expression("9×10"^6)))) +
                    coord_flip(ylim = c(1.976, 41.5)) +
                    theme(axis.ticks.y = element_blank(), # modify base theme
                          axis.title.y = element_blank()) +
                    mytheme

seaweeds.plot # print (save as 4.5 x 5.5 in)


### Seaweed farming image
## Load data
image <- readPNG("~/Desktop/Plymouth University/MBIO363/IMTA/Figures/image.png")

## Plot data
image.plot <- ggplot() + 
                background_image(image) +
                theme(plot.margin = margin(l = .271453138435082, 0, 0, 0, unit = "in"))

image.plot # print (save as 4.5 x 3.5 in)
  
### Combine plots and image
fig.4 <- ggarrange(countries.plot, seaweeds.plot, image.plot, ncol = 3, nrow = 1,
                   labels = c("a", "b", "c"), widths = c(1, 1.571428571428571, 1))

fig.4 # print (save as 4.5 x 12.5)



#### Clean up
### Detach packages
detach(package:ggplot2)
detach(package:forcats)
detach(package:ggpubr)
detach(package:esc)
detach(package:meta)
detach(package:maptools)
detach(package:raster)
detach(package:ggnewscale)
detach(package:rworldmap)
detach(package:png)

### Clear environment, plots and console
rm(list = ls())
graphics.off()
cat("\014")

