#description     :ASRZW analysis plot generation script.
#author		       : Rylan Shearn
#date            :20161015
#version         :0
#usage		       :run contents of this script in R, plots output to 'graphs' directory
#notes           :Contact RS for input data, requires associated preprocessing script
#r_version       :3.3.1 (2016-06-21) -- "Bug in Your Hair"
#==============================================================================

library('ProjectTemplate')
load.project()

#########################################################################
## Plotting Z.W ratio distributions

# A histogram of Z.W ratios, shows whether ZW ratio data (from Rutkowska et al, 2012) have corresponding data for ASR
hp <- ggplot(datafull, aes(x=Z.W)) + 
  geom_histogram(colour="black", fill="white") +
  theme_bw() +
  facet_grid(ASR.szekely.status ~ ASR.pipoly.status + ASR.owens.status) +
  labs(x = "Z/W ratio", y = "No. species with ASR data from labeled sources")
#print to file
ggsave((file.path('graphs', 'zwdist.pdf')), height = 6, width = 10)

# Another histogram of ZW, shows whether ZW has corresponding data for ASR
hpzw <- ggplot(data_long, aes(x=Z.W)) + 
  geom_histogram(aes(fill = ASR.status), alpha = 0.8, position="dodge") +
  #facet_grid(asr.origin ~ .) +
  theme_bw() +
  theme(legend.title=element_blank()) +
  geom_vline(data=zwMeans, aes(xintercept=zw.mean, colour=ASR.status), alpha = 0.5, linetype="dashed", size=1) +
  labs(x = "Z/W ratio")
#print to file
ggsave((file.path('graphs', 'zwdist2.pdf')), height = 7, width =12)

# A density plot of ZW ratio, shows whether ZW have corresponding data for ASR, and what kind
dpzw <- ggplot(data_long, aes(x=Z.W)) + 
  geom_density(aes(fill=ASR.status), alpha =0.2, size=0.2) +
  #facet_grid(asr.origin ~ .) +
  theme_bw() +
  theme(legend.title=element_blank()) +
  geom_vline(data=zwMeans, aes(xintercept=zw.mean, colour=ASR.status), alpha = 0.5, linetype="dashed", size=1) +
  labs(x = "Z/W ratio")

#print to file
ggsave((file.path('graphs', 'zwdens.pdf')), height = 6, width = 10)

#########################################################################
## Plotting ASR distributions - original

# A histogram of ASR, shows whether ASR have corresponding data for ZW
hpasr1 <- ggplot(data_long, aes(x=sex.ratio)) + 
  geom_histogram(aes(fill = Z.W.status), alpha = 0.5, position="identity") +
  facet_grid(asr.origin ~ .) +
  theme_bw() +
  theme(legend.title=element_blank()) +
  geom_vline(data=asrMeans, aes(xintercept=asr.mean, colour=Z.W.status), alpha = 0.5, linetype="dashed", size=1) +
  labs(x = "ASR")

#print to file
ggsave((file.path('graphs', 'asrdist.pdf')), height = 7, width = 6)

# A density plot of ASR, shows whether ASR data have corresponding data for ZW
dasr1 <- ggplot(data_long, aes(x=sex.ratio)) + 
  geom_density(aes(fill=Z.W.status), alpha =0.2, size=0.2) +
  facet_grid(asr.origin ~ .) +
  theme_bw() +
  theme(legend.title=element_blank()) +
  geom_vline(data=asrMeans, aes(xintercept=asr.mean, colour=Z.W.status), alpha = 0.5, linetype="dashed", size=1) +
  labs(x = "ASR")

#print to file
ggsave((file.path('graphs', 'asrdens.pdf')), height = 7, width = 6)

#print both of the above side by side to file
pdf((file.path('graphs', 'asrdistasrdens.pdf')), height = 7, width = 12)
hpasr1dasr1 <- multiplot(hpasr1, dasr1, cols=2)
dev.off()

#########################################################################
# facet histogram plots - sex ratio data

#plot
hist.sex <- ggplot(datamelt.sex, aes(x=Value)) +
  facet_wrap(~Predictor, scales = "free", ncol = 3) + # Facet grid with variable x axis
  geom_histogram(aes(y=..density..),
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="blue") +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x="")

#print to file
ggsave((file.path('graphs', 'facetHistSex.pdf')), height = 7, width = 12)

#########################################################################
# facet histogram plots - sex dimorphism data

#plot
hist.sexd <- ggplot(datamelt.sexd, aes(x=Value)) +
  facet_wrap(~Predictor, scales = "free", ncol = 3) + # Facet grid with variable x axis
  geom_histogram(aes(y=..density..),
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="blue") +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x="")

#print to file
ggsave((file.path('graphs', 'facetHistSexDim.pdf')), height = 12, width = 12)

#########################################################################
# facet histogram plots - phenotype data

#plot
hist.phen <- ggplot(datamelt.phen, aes(x=Value)) +
  facet_wrap(~Predictor, scales = "free", ncol = 3) + # Facet grid with variable x axis
  geom_histogram(aes(y=..density..),
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="blue") +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x="")

#print to file
ggsave((file.path('graphs', 'facetHistPhen.pdf')), height = 7, width = 12)

#########################################################################
# facet scatter plots - zw vs sex ratio data

#grid labelling function (for parsing equation text to facet labels)
r2_labeller.sex <- function(variable,value){
  return(eqndf.sex$lab)
}

#plot
scat.sex <- ggplot(datamelt.sex, aes(x=Value, y=Z.W)) +
  facet_wrap(~Predictor, scales = "free_x", labeller = r2_labeller.sex, ncol = 3) + # Facet grid with variable x axis
  geom_point(shape=19, aes(colour=factor(Order))) +
  scale_color_discrete("Order") +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x="") +
  stat_smooth(method = "lm", formula = y ~ x)

#print to file
ggsave((file.path('graphs', 'zwSex.pdf')), height = 12, width = 12)
ggsave((file.path('graphs', 'zwSex.png')), height = 12, width = 12)

#########################################################################
# facet scatter plots - zw vs sex dimorphism data

#grid labelling function (for parsing equation text to facet labels)
r2_labeller.sexd <- function(variable,value){
  return(eqndf.sexd$lab)
}

#plot
scat.sexd <- ggplot(datamelt.sexd, aes(x=Value, y=Z.W)) +
  facet_wrap(~Predictor, scales = "free_x", labeller = r2_labeller.sexd, ncol = 3) + # Facet grid with variable x axis
  geom_point(shape=19, aes(colour=factor(Order))) +
  scale_color_discrete("Order") +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x="") +
  stat_smooth(method = "lm", formula = y ~ x)

#print to file
ggsave((file.path('graphs', 'zwSexd.pdf')), height = 15, width = 12)
ggsave((file.path('graphs', 'zwSexd.png')), height = 15, width = 12)

#########################################################################
# facet scatter plots - zw vs phenotype data

#grid labelling function (for parsing equation text to facet labels)
r2_labeller.phen <- function(variable,value){
  return(eqndf.phen$lab)
}

#plot
scat.phen <- ggplot(datamelt.phen, aes(x=Value, y=Z.W)) +
  facet_wrap(~Predictor, scales = "free_x", labeller = r2_labeller.phen, ncol = 3) + # Facet grid with variable x axis
  geom_point(shape=19, aes(colour=factor(Order))) +
  scale_color_discrete("Order") +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x="") +
  stat_smooth(method = "lm", formula = y ~ x)

#print to file
ggsave((file.path('graphs', 'zwPhen.pdf')), height = 12, width = 12)
ggsave((file.path('graphs', 'zwPhen.png')), height = 12, width = 12)

