# General information ----
# Title: Template normality testing, statistics and ggplot visualization 
# Author: Rick Wilbrink
# Department: Rheumatology and Clinical Immunology
# Affiliation: University Medical Center Groningen
# Email: r.wilbrink01@umcg.nl
# Collaboration: please ask permission from the author before using this script
# Date created: 07-02-2023
# Date last adjustment: 17-04-2023
# References: statistical tests http://rpkgs.datanovia.com/ggpubr/reference/index.html 

# introduction ----
# this scripts contains a template for normality testing, statistics and ggplot visualization
# only use this script for quick visualization of your results, this is not a publication ready script
# always think about the following questions and adjust accordingly:
# do you have to compare all groups or is it better to do hypothesis driven testing including only two or a small number of groups?
# are the differences between the groups (e.g. size of change) clinical relevant?
# how lower the p-value, the higher the chance that there is indeed a difference between the groups. 

# importing libraries ----
library(readxl)
library(ggplot2)
library(qqplotr)
library(dplyr)
library(DescTools)
library(tidyverse)
library(ggsignif)
library(rstatix)
library(ggpubr)
library(patchwork)
library(gridExtra)

# managing directories ----

# create a main folder with a sub folder called R scripts, put this R file in the R scripts folder (e.g. ../main folder/R scripts/this script)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  # sets the working directory to where the script is located
setwd('..')
PrimaryDirectory <- getwd()
setwd(PrimaryDirectory)

# create and set various directories
dir.create(paste0(PrimaryDirectory,'/data/'), showWarnings = FALSE)
InputDirectory <- paste0(PrimaryDirectory,'/data/')
dir.create(paste0(PrimaryDirectory,'/metadata/'), showWarnings = FALSE)
MetaDirectory <- paste0(PrimaryDirectory,'/metadata/')
dir.create(paste0(PrimaryDirectory,'/results/'), showWarnings = FALSE)
OutputDirectory <- paste0(PrimaryDirectory,'/results/')
dir.create(paste0(PrimaryDirectory,'/results/plots'), showWarnings = FALSE)
PlotsDirectory <- paste0(PrimaryDirectory,'/results/plots')

# reading and preparing data ----
db <- read_excel(paste0(InputDirectory,'database_test.xlsx'))

db <- db %>%
  reorder_levels(group, order = c("AS", "HD", "pSS")) # reorder your groups

# for loop ----

# set variables
populations <- names(db)
vector <- populations[! populations %in% c('sample', 'group')]

#drop.variables <- c('sample', 'group')
#populations <- dplyr::select_if(db, is.numeric) # this is another options
#populations <- colnames(populations)

# time 
ptm <- proc.time()

# start for loop
for (i in vector){

# normality testing ----
  
# hist
hist <- ggplot(db, aes_string(x=i, fill='group')) +
           geom_density(alpha=0.4) +
           geom_histogram(alpha=0.8) +
           geom_line(stat = 'density') +
           facet_wrap(~group) + # change scales with scale = 'free'
           labs(x = paste0("Frequency ",i ," of Parent"), y = "Counts") + 
           theme_bw() + 
           theme(legend.position = 'none')

# qqplot
qqplot <- ggplot(data = db, mapping = aes_string(sample = i, color = 'group', fill = 'group')) +
             stat_qq_band(alpha=0.5, conf=0.95, qtype=1, bandType = "boot") +
             stat_qq_line(identity=TRUE) +
             stat_qq_point(col="black") +
             facet_wrap(~ group, scales = "free") +
             labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + 
             theme_bw() +
             theme(legend.position = 'none')

# statistics ----

# descriptive statistics summary
stat.summary <- db %>% 
                   group_by(group) %>%
                   get_summary_stats(i, type = "full")

v <- db[[i]]

# Kruskal-Wallis test is a non-parametric alternative to the one-way ANOVA test. 
# It extends the two-samples Wilcoxon test in the situation where there are more than two groups to compare.
kruskal.res <- db %>% 
                  kruskal_test(v ~ group)

# visualization ----

# add textual variables to the ggplot
global.test <- kruskal.res # put here the global test (depending on the distribution of variables)
subtitle <- get_test_label(global.test, detailed = TRUE)
post.test <- "Post-hoc, Dunn's test, p-value adjustment method Bonferroni"

# ggplot2 visualization
p1 <- ggboxplot(db, x = 'group', y = i, color = 'group', add = 'jitter') + 
  
  # add pairwise comparisons, ref: http://rpkgs.datanovia.com/ggpubr/reference/geom_pwc.html
  geom_pwc( 
    method = "dunn_test", # add statistical test (e.g. 'wilcox_test', 't_test', 'sign_test', 'dunn_test', 'tukey_hsd')
    #ref.group = 'HD', # for example if you use wilcox test you use HD as a reference group
    label = "p.adj.format",
    p.adjust.method = "bonferroni", # add p.adjust.method to control the Family-wise Error Rate
    bracket.nudge.y = 0.08
  ) +

  # captions of x- and y-axis
  labs(x = NULL, 
       y = paste0("Frequency ", i ," of Parent"),
       subtitle = subtitle,
       caption = post.test
  ) +
  
  # theme of the plot
  theme( 
    plot.title = element_text(size = 14, hjust = 0.5),
    axis.text = element_text(colour = "black", size = 12, margin = margin(t = 0, r = 0, b = 10, l = 0)),
    axis.title.x = element_text(colour = "black", size = 12, margin = margin(t = 0, r = 0, b = 0, l = 00)),
    axis.title.y = element_text(colour = "black", size = 12, margin = margin(t = 0, r = 10, b = 0, l = 0)),
    
    # subtitle
    plot.caption = element_text(size = 12),
    
    # legend theme
    legend.position = "none", 
    
    # panel theme
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #panel.background = element_rect(fill = "white", colour = "black", size = )
  )

# including the summary statistics
summary.table <- ggsummarytable(stat.summary, 
                                x = "group", 
                                y = c("n", "min", "max", "median", "q1", "q3", "iqr", "mean", "sd", "se", "ci"), 
                                color = "group",
                                ggtheme = theme_void(), 
                                legend = "none",
                                position = "identity"
)


# plot
p2 <- ggarrange(hist | qqplot, p1 | summary.table, ncol = 1, nrow = 2)
plot <- annotate_figure(p2, 
                fig.lab = paste0("Figure of ", i), 
                fig.lab.size = 12,
                fig.lab.pos = "bottom.right",
                fig.lab.face = "bold")

# save 
tiff(paste0(PlotsDirectory,'/',i,'.tiff'), 
     units="in", width=14, height=10, res=600, compression = 'lzw')
print(plot)
dev.off()

} # close the for loop

# time duration
time.dur <- proc.time() - ptm
print(time.dur)

# session
sessionInfo()