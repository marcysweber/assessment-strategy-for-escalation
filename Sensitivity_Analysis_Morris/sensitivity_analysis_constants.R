#packages, paths
#####
library(nlrx)
library(sensitivity)
netlogopath2 <- file.path("/Program Files/NetLogo 6.2.2")
modelpath2 <- file.path("C:\\Users\\Marcy\\Documents\\GitHub\\dominance\\Sensitivity_Analysis_Morris\\sensitivity_analysis_dominance_despotism_energy.nlogo")
outpath2 <- file.path("/Users/Marcy/Desktop/")

multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  require(grid)
  
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots == 1) {
    print(plots[[1]])
    
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#####end

#burnin period test
###########

burnindf <- dominance_despotism_energy.burnin.final

a<-ggplot(burnindf, mapping = aes( x = burnin.test.dur, y = dir.cons.index.wins, color = resource.dist, shape = winning)) + 
  geom_jitter(show.legend=FALSE,size = 0.5) +
  labs(title = "A.") +
  xlab("timestep")+
  ylab("DCI fight outcomes") +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  ) +
  scale_color_manual(values=c("blue", "orange"))
  #stat_summary(fun = "median", color = "black")

b<-ggplot(burnindf, mapping = aes(x = burnin.test.dur, y = dir.cons.index.attacks, color = resource.dist, shape = winning)) + 
  geom_jitter(show.legend=FALSE,size = 0.5) +
  labs(title = "B.") +
  xlab("timestep")+
  ylab("DCI attacks") +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )+
  scale_color_manual(values=c("blue", "orange"))
  #stat_summary(fun = "median", color = "black")

c<-ggplot(burnindf, mapping = aes(x = burnin.test.dur, y = dir.cons.index.avoids, color = resource.dist, shape = winning)) + 
  geom_jitter(show.legend=FALSE, size = 0.5) +
  labs(title = "C.") +
  xlab("timestep")+
  ylab("DCI not-attacks") +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  ) +
  scale_color_manual(values=c("blue", "orange"))
  #stat_summary(fun = "median", color = "black")

#figure OS2.1
multiplot(a,b,c)


ggplot(burnindf, mapping = aes(x= sum..victory.counter..of.victories, y=dir.cons.index.wins)) +
  geom_point()


##############end

#params
#####
ch2vars <- list(
  #resource variables
  "quality-max-clumped" = list(min = 15, max = 50, qfun = "qunif"),
  "quality-max-uniform" = list(min = 1, max = 10, qfun = "qunif"),
  "regrow-freq" = list(min = 10, max = 50, qfun = "qunif"),
  "regrowth-denominator" = list(min = 1, max = 5, qfun = "qunif"),
  
  #sensory variables
  "max-nearest-primates" = list(min = 2, max = 10, qfun = "qunif"),
  "max-dist-nearest-primates" = list(min = 2, max = 10, qfun = "qunif"),
  "other-primate-detection-radius" = list(min = 2, max = 10, qfun = "qunif"),
  "resource-detection-radius" = list(min = 2, max = 10, qfun = "qunif"),
  
  #random primate variables
  "step-distance" = list(min = 0.5, max = 2, qfun = "qunif"), # changed after first MEE
  "starting-pop-primates" = list(min = 5, max = 15, qfun = "qunif"),
  
  #contest-related variables
  "max-rhp" = list(min = 2, max = 10, qfun = "qunif"),
  "change-in-exp-score" = list(min = 0.001, max = 0.2, qfun = "qunif"),
  "exp-score-decay-when-high" = list(min = 0.001, max = 0.1, qfun = "qunif")
)
#####end

## practice
#####
nlpractice <- nl(nlversion = "6.2.2",
                 nlpath = netlogopath2,
                 modelpath = modelpath2,
                 jvmmem = 8000)

nlpractice@experiment <- experiment(expname = "ch2amnatMEEpractice",
                                    outpath = outpath2,
                                    repetition = 1,
                                    tickmetrics = "false",
                                    idsetup = "setup",
                                    idgo = "go",
                                    runtime = 1000,
                                    stopcond = "(not any? patches with [penergy > 0])",
                                    metrics = c("foraging-efficiency-time", 
                                                "n-interactions", 
                                                "proportion-attacking",
                                                "dir-cons-index-wins",
                                                "dir-cons-index-attacks",
                                                "dir-cons-index-avoids"),
                                    variables = list(
                                      #resource variables
                                      "quality-max-clumped" = list(min = 15, max = 50, qfun = "qunif"),
                                      "quality-max-uniform" = list(min = 1, max = 10, qfun = "qunif")),
                                    constants = list("asymmetry" = "\"deterministic\"",
                                                     "assessment-who" = "\"mutual\"",
                                                     "assessment-info" = "\"knowledge\"",
                                                     "resource-dist" = "\"uniform\"")
)           


nlpractice@simdesign <- simdesign_morris(nl = nlpractice,
                                         morristype = "oat",
                                         morrislevels = 6,
                                         morrisr = 10,
                                         morrisgridjump = 3, 
                                         nseeds = 1)


progressr::handlers("progress")

library(future)
plan(multisession)

resultsMorrisch2practice <- progressr::with_progress(run_nl_all(nlpractice))

setsim(nlpractice, "simoutput")<-resultsMorrisch2practice
analysispractice <- analyze_nl(nlpractice)
#####end

#MEE experiments for deterministic and initiator scenarios
#####
###########################################################################
##first SA experiment - rhp clumped

nlrhpcld <- nl(nlversion = "6.2.2",
         nlpath = netlogopath2,
         modelpath = modelpath2,
         jvmmem = 8000)

nlrhpcld@experiment <- experiment(expname = "ch2amnatMEErhpcld",
                            outpath = outpath2,
                            repetition = 1,
                            tickmetrics = "false",
                            idsetup = "setup",
                            idgo = "go",
                            runtime = 15000,
                            #stopcond = "(not any? patches with [penergy > 0])",
                            metrics = c("foraging-efficiency-time", 
                                        "n-interactions", 
                                        "proportion-attacking",
                                        "dir-cons-index-wins",
                                        "dir-cons-index-attacks",
                                        "dir-cons-index-avoids"),
                            variables = ch2vars,
                            constants = list("winning" = "\"deterministic\"",
                                             "assessment-who" = "\"mutual\"",
                                             "assessment-info" = "\"knowledge\"",
                                             "resource-dist" = "\"clumped\"")
)           


nlrhpcld@simdesign <- simdesign_morris(nl = nlrhpcld,
                                 morristype = "oat",
                                 morrislevels = 6,
                                 morrisr = 10,
                                 morrisgridjump = 3, 
                                 nseeds = 1)


progressr::handlers("progress")

library(future)
plan(multisession)

resultsMorrisch2rhpcld <- progressr::with_progress(run_nl_all(nlrhpcld))

setsim(nlrhpcld, "simoutput")<-resultsMorrisch2rhpcld
analysisrhpcld <- analyze_nl(nlrhpcld)




######################################################
##second set, rhp uniform

nlrhpunid <- nl(nlversion = "6.2.2",
         nlpath = netlogopath2,
         modelpath = modelpath2,
         jvmmem = 8000)

nlrhpunid@experiment <- experiment(expname = "ch2amnatMEErhpcld",
                            outpath = outpath2,
                            repetition = 1,
                            tickmetrics = "false",
                            idsetup = "setup",
                            idgo = "go",
                            runtime = 15000,
                            #stopcond = "(not any? patches with [penergy > 0])",
                            metrics = c("foraging-efficiency-time", 
                                        "n-interactions", 
                                        "proportion-attacking",
                                        "dir-cons-index-wins",
                                        "dir-cons-index-attacks",
                                        "dir-cons-index-avoids"),
                            variables = ch2vars,
                            constants = list("winning" = "\"deterministic\"",
                                             "assessment-who" = "\"mutual\"",
                                             "assessment-info" = "\"knowledge\"",
                                             "resource-dist" = "\"uniform\"")
)           


nlrhpunid@simdesign <- simdesign_morris(nl = nlrhpunid,
                                 morristype = "oat",
                                 morrislevels = 6,
                                 morrisr = 10,
                                 morrisgridjump = 3, 
                                 nseeds = 1)


progressr::handlers("progress")

library(future)
plan(multisession)

resultsMorrisch2rhpunid <- progressr::with_progress(run_nl_all(nlrhpunid))

setsim(nlrhpunid, "simoutput")<-resultsMorrisch2rhpunid
analysisrhpunid <- analyze_nl(nlrhpunid)

############################################################
## third set, history and clumped

nlhistcld <- nl(nlversion = "6.2.2",
         nlpath = netlogopath2,
         modelpath = modelpath2,
         jvmmem = 12000)

nlhistcld@experiment <- experiment(expname = "ch2amnatMEErhpcld",
                            outpath = outpath2,
                            repetition = 1,
                            tickmetrics = "false",
                            idsetup = "setup",
                            idgo = "go",
                            runtime = 15000,
                            #stopcond = "(not any? patches with [penergy > 0])",
                            metrics = c("foraging-efficiency-time", 
                                        "n-interactions", 
                                        "proportion-attacking",
                                        "dir-cons-index-wins",
                                        "dir-cons-index-attacks",
                                        "dir-cons-index-avoids"),
                            variables = ch2vars,
                            constants = list("winning" = "\"deterministic\"",
                                             "assessment-who" = "\"mutual\"",
                                             "assessment-info" = "\"history\"",
                                             "resource-dist" = "\"clumped\"")
)           


nlhistcld@simdesign <- simdesign_morris(nl = nlhistcld,
                                 morristype = "oat",
                                 morrislevels = 6,
                                 morrisr = 10,
                                 morrisgridjump = 3, 
                                 nseeds = 1)


progressr::handlers("progress")

library(future)
plan(multisession)

resultsMorrisch2histcld <- progressr::with_progress(run_nl_all(nlhistcld))

setsim(nlhistcld, "simoutput")<-resultsMorrisch2histcld
analysishistcld <- analyze_nl(nlhistcld)

###########################################################################
### fourth set, history and uniform


nlhistunid <- nl(nlversion = "6.2.2",
         nlpath = netlogopath2,
         modelpath = modelpath2,
         jvmmem = 12000)

nlhistunid@experiment <- experiment(expname = "ch2amnatMEErhpcld",
                            outpath = outpath2,
                            repetition = 1,
                            tickmetrics = "false",
                            idsetup = "setup",
                            idgo = "go",
                            runtime = 15000,
                            #stopcond = "(not any? patches with [penergy > 0])",
                            metrics = c("foraging-efficiency-time", 
                                        "n-interactions", 
                                        "proportion-attacking",
                                        "dir-cons-index-wins",
                                        "dir-cons-index-attacks",
                                        "dir-cons-index-avoids"),
                            variables = ch2vars,
                            constants = list("winning" = "\"deterministic\"",
                                             "assessment-who" = "\"mutual\"",
                                             "assessment-info" = "\"history\"",
                                             "resource-dist" = "\"uniform\"")
)           


nlhistunid@simdesign <- simdesign_morris(nl = nlhistunid,
                                 morristype = "oat",
                                 morrislevels = 6,
                                 morrisr = 10,
                                 morrisgridjump = 3, 
                                 nseeds = 1)


progressr::handlers("progress")

library(future)
plan(multisession)

resultsMorrisch2histunid <- progressr::with_progress(run_nl_all(nlhistunid))

setsim(nlhistunid, "simoutput")<-resultsMorrisch2histunid
analysishistunid <- analyze_nl(nlhistunid)


#########################################################
#initiator wins sensitivity analysis here

##first SA experiment - rhp clumped

nlrhpcli <- nl(nlversion = "6.2.2",
              nlpath = netlogopath2,
              modelpath = modelpath2,
              jvmmem = 8000)

nlrhpcli@experiment <- experiment(expname = "ch2amnatMEErhpcli",
                                  outpath = outpath2,
                                  repetition = 1,
                                  tickmetrics = "false",
                                  idsetup = "setup",
                                  idgo = "go",
                                  runtime = 15000,
                                  #stopcond = "(not any? patches with [penergy > 0])",
                                  metrics = c("foraging-efficiency-time", 
                                              "n-interactions", 
                                              "proportion-attacking",
                                              "dir-cons-index-wins",
                                              "dir-cons-index-attacks",
                                              "dir-cons-index-avoids"),
                                  variables = ch2vars,
                                  constants = list("winning" = "\"initiator\"",
                                                   "assessment-who" = "\"mutual\"",
                                                   "assessment-info" = "\"knowledge\"",
                                                   "resource-dist" = "\"clumped\"")
)           


nlrhpcli@simdesign <- simdesign_morris(nl = nlrhpcli,
                                       morristype = "oat",
                                       morrislevels = 6,
                                       morrisr = 10,
                                       morrisgridjump = 3, 
                                       nseeds = 1)


progressr::handlers("progress")

library(future)
plan(multisession)

resultsMorrisch2rhpcli <- progressr::with_progress(run_nl_all(nlrhpcli))

setsim(nlrhpcli, "simoutput")<-resultsMorrisch2rhpcli
analysisrhpcli <- analyze_nl(nlrhpcli)




######################################################
##second set, rhp uniform

nlrhpunii <- nl(nlversion = "6.2.2",
                nlpath = netlogopath2,
                modelpath = modelpath2,
                jvmmem = 8000)

nlrhpunii@experiment <- experiment(expname = "ch2amnatMEErhpcli",
                                   outpath = outpath2,
                                   repetition = 1,
                                   tickmetrics = "false",
                                   idsetup = "setup",
                                   idgo = "go",
                                   runtime = 15000,
                                   #stopcond = "(not any? patches with [penergy > 0])",
                                   metrics = c("foraging-efficiency-time", 
                                               "n-interactions", 
                                               "proportion-attacking",
                                               "dir-cons-index-wins",
                                               "dir-cons-index-attacks",
                                               "dir-cons-index-avoids"),
                                   variables = ch2vars,
                                   constants = list("winning" = "\"initiator\"",
                                                    "assessment-who" = "\"mutual\"",
                                                    "assessment-info" = "\"knowledge\"",
                                                    "resource-dist" = "\"uniform\"")
)           


nlrhpunii@simdesign <- simdesign_morris(nl = nlrhpunii,
                                        morristype = "oat",
                                        morrislevels = 6,
                                        morrisr = 10,
                                        morrisgridjump = 3, 
                                        nseeds = 1)


progressr::handlers("progress")

library(future)
plan(multisession)

resultsMorrisch2rhpunii <- progressr::with_progress(run_nl_all(nlrhpunii))

setsim(nlrhpunii, "simoutput")<-resultsMorrisch2rhpunii
analysisrhpunii <- analyze_nl(nlrhpunii)

############################################################
## third set, history and clumped

nlhistcli <- nl(nlversion = "6.2.2",
                nlpath = netlogopath2,
                modelpath = modelpath2,
                jvmmem = 12000)

nlhistcli@experiment <- experiment(expname = "ch2amnatMEErhpcli",
                                   outpath = outpath2,
                                   repetition = 1,
                                   tickmetrics = "false",
                                   idsetup = "setup",
                                   idgo = "go",
                                   runtime = 15000,
                                   #stopcond = "(not any? patches with [penergy > 0])",
                                   metrics = c("foraging-efficiency-time", 
                                               "n-interactions", 
                                               "proportion-attacking",
                                               "dir-cons-index-wins",
                                               "dir-cons-index-attacks",
                                               "dir-cons-index-avoids"),
                                   variables = ch2vars,
                                   constants = list("winning" = "\"initiator\"",
                                                    "assessment-who" = "\"mutual\"",
                                                    "assessment-info" = "\"history\"",
                                                    "resource-dist" = "\"clumped\"")
)           


nlhistcli@simdesign <- simdesign_morris(nl = nlhistcli,
                                        morristype = "oat",
                                        morrislevels = 6,
                                        morrisr = 10,
                                        morrisgridjump = 3, 
                                        nseeds = 1)


progressr::handlers("progress")

library(future)
plan(multisession)

resultsMorrisch2histcli <- progressr::with_progress(run_nl_all(nlhistcli))

setsim(nlhistcli, "simoutput")<-resultsMorrisch2histcli
analysishistcli <- analyze_nl(nlhistcli)

###########################################################################
### fourth set, history and uniform


nlhistunii <- nl(nlversion = "6.2.2",
                 nlpath = netlogopath2,
                 modelpath = modelpath2,
                 jvmmem = 12000)

nlhistunii@experiment <- experiment(expname = "ch2amnatMEErhpcli",
                                    outpath = outpath2,
                                    repetition = 1,
                                    tickmetrics = "false",
                                    idsetup = "setup",
                                    idgo = "go",
                                    runtime = 15000,
                                    #stopcond = "(not any? patches with [penergy > 0])",
                                    metrics = c("foraging-efficiency-time", 
                                                "n-interactions", 
                                                "proportion-attacking",
                                                "dir-cons-index-wins",
                                                "dir-cons-index-attacks",
                                                "dir-cons-index-avoids"),
                                    variables = ch2vars,
                                    constants = list("winning" = "\"initiator\"",
                                                     "assessment-who" = "\"mutual\"",
                                                     "assessment-info" = "\"history\"",
                                                     "resource-dist" = "\"uniform\"")
)           


nlhistunii@simdesign <- simdesign_morris(nl = nlhistunii,
                                         morristype = "oat",
                                         morrislevels = 6,
                                         morrisr = 10,
                                         morrisgridjump = 3, 
                                         nseeds = 1)


progressr::handlers("progress")

library(future)
plan(multisession)

resultsMorrisch2histunii <- progressr::with_progress(run_nl_all(nlhistunii))

setsim(nlhistunii, "simoutput")<-resultsMorrisch2histunii
analysishistunii <- analyze_nl(nlhistunii)




#########################################################

library(ggplot2)

analysishistcld <- split(analysishistcld, analysishistcld$metric)
analysishistunid <- split(analysishistunid, analysishistunid$metric)
analysisrhpcld <- split(analysisrhpcld, analysisrhpcld$metric)
analysisrhpunid <- split(analysisrhpunid, analysisrhpunid$metric)
analysishistcli <- split(analysishistcli, analysishistcli$metric)
analysishistunii <- split(analysishistunii, analysishistunii$metric)
analysisrhpcli <- split(analysisrhpcli, analysisrhpcli$metric)
analysisrhpunii <- split(analysisrhpunii, analysisrhpunii$metric)

new.MEE.data <- c(analysishistcld, analysishistunid, analysisrhpcld, analysisrhpunid, 
                  analysishistcli, analysishistunii, analysisrhpcli, analysisrhpunii)
#####end



#code below processes all MEE output so that it can be plotted
#####


library(reshape2)

musigma_processing <- function(data) {
  return(dcast(data, metric + parameter ~ index))
}

musigma_processing(analysishistcl$`dir-cons-index-attacks_mean`)


analysishistcldmusig <- data.frame()

for (df in analysishistcld) {
  analysishistcldmusig <- rbind(analysishistcldmusig, (musigma_processing(as.data.frame(df))))
}

analysishistunidmusig <- data.frame()

for (df in analysishistunid) {
  analysishistunidmusig <- rbind(analysishistunidmusig, (musigma_processing(as.data.frame(df))))
}

analysisrhpcldmusig <- data.frame()

for (df in analysisrhpcld) {
  analysisrhpcldmusig <- rbind(analysisrhpcldmusig, (musigma_processing(as.data.frame(df))))
}

analysisrhpunidmusig <- data.frame()

for (df in analysisrhpunid) {
  analysisrhpunidmusig <- rbind(analysisrhpunidmusig, (musigma_processing(as.data.frame(df))))
}
analysishistclimusig <- data.frame()

for (df in analysishistcli) {
  analysishistclimusig <- rbind(analysishistclimusig, (musigma_processing(as.data.frame(df))))
}

analysishistuniimusig <- data.frame()

for (df in analysishistunii) {
  analysishistuniimusig <- rbind(analysishistuniimusig, (musigma_processing(as.data.frame(df))))
}

analysisrhpclimusig <- data.frame()

for (df in analysisrhpcli) {
  analysisrhpclimusig <- rbind(analysisrhpclimusig, (musigma_processing(as.data.frame(df))))
}

analysisrhpuniimusig <- data.frame()

for (df in analysisrhpunii) {
  analysisrhpuniimusig <- rbind(analysisrhpuniimusig, (musigma_processing(as.data.frame(df))))
}
#####end

#mu-sigma plots
#####
library(ggrepel)


#included in the supplement - the four main text scnearios
#####

#RHP-based decision-making, deterministic fight toucomes, clumped resources

metric.levels <- unique(analysisrhpcldmusig$metric)

analysisrhpcldmusig$numlabel <- rep(1:13, 6)


os2.2rhpclda<-ggplot(analysisrhpcldmusig[analysisrhpcldmusig$metric == "dir-cons-index-wins_mean",], aes(x = mustar, y = sigma, label = numlabel)) +
          geom_point() + geom_label_repel(size = 3) +
          labs(title = paste0("A."))+
          xlab(paste("\u03BC", "*"))+
          ylab("\u03C3 ") +
          
          theme(
            axis.title = element_text(size = 20, color = "black"),
            axis.text = element_text(size = 8, color = "black"),
            
            panel.grid = element_line(color = "black"),
            panel.grid.major = element_line(color = "gray75"),
            panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
          )
os2.2rhpcldab<-ggplot(analysisrhpcldmusig[analysisrhpcldmusig$metric == "dir-cons-index-attacks_mean",], aes(x = mustar, y = sigma, label = numlabel)) +
  geom_point() + geom_label_repel(size = 3) +
  labs(title = paste0("B."))+
  xlab(paste("\u03BC", "*"))+
  ylab("\u03C3 ") +
  
  theme(
    axis.title = element_text(size = 20, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )
os2.2rhpcldac<-ggplot(analysisrhpcldmusig[analysisrhpcldmusig$metric == "dir-cons-index-avoids_mean",], aes(x = mustar, y = sigma, label = numlabel)) +
  geom_point() + geom_label_repel(size = 3) +
  labs(title = paste0("C."))+
  xlab(paste("\u03BC", "*"))+
  ylab("\u03C3 ") +
  
  theme(
    axis.title = element_text(size = 20, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )
os2.2rhpcldd<-ggplot(analysisrhpcldmusig[analysisrhpcldmusig$metric == "n-interactions_mean",], aes(x = mustar, y = sigma, label = numlabel)) +
  geom_point() + geom_label_repel(size = 3) +
  labs(title = paste0("D."))+
  xlab(paste("\u03BC", "*"))+
  ylab("\u03C3 ") +
  
  theme(
    axis.title = element_text(size = 20, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )
os2.2rhpclde<-ggplot(analysisrhpcldmusig[analysisrhpcldmusig$metric == "proportion-attacking_mean",], aes(x = mustar, y = sigma, label = numlabel)) +
  geom_point() + geom_label_repel(size = 3) +
  labs(title = paste0("E."))+
  xlab(paste("\u03BC", "*"))+
  ylab("\u03C3 ") +
  
  theme(
    axis.title = element_text(size = 20, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )
os2.2rhpcldf<-ggplot(analysisrhpcldmusig[analysisrhpcldmusig$metric == "foraging-efficiency-time_mean",], aes(x = mustar, y = sigma, label = numlabel)) +
  geom_point() + geom_label_repel(size = 3) +
  labs(title = paste0("F."))+
  xlab(paste("\u03BC", "*"))+
  ylab("\u03C3 ") +
  
  theme(
    axis.title = element_text(size = 20, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )

pdf("Github/dominance/AmNatjan23MEErhpcld.pdf")
multiplot(
  os2.2rhpclda,
  os2.2rhpcldab, 
  os2.2rhpcldac,
  os2.2rhpcldd,
  os2.2rhpclde,
  os2.2rhpcldf, 
  cols = 2
)
dev.off()

#RHP-based decision-making, deterministic fight outcomes, uniform resources
analysisrhpunidmusig$numlabel <- rep(1:13, 6)


os2.2rhpuda<-ggplot(analysisrhpunidmusig[analysisrhpunidmusig$metric == "dir-cons-index-wins_mean",], aes(x = mustar, y = sigma, label = numlabel)) +
  geom_point() + geom_label_repel(size = 3) +
  labs(title = paste0("A."))+
  xlab(paste("\u03BC", "*"))+
  ylab("\u03C3 ") +
  
  theme(
    axis.title = element_text(size = 20, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )
os2.2rhpudab<-ggplot(analysisrhpunidmusig[analysisrhpunidmusig$metric == "dir-cons-index-attacks_mean",], aes(x = mustar, y = sigma, label = numlabel)) +
  geom_point() + geom_label_repel(size = 3) +
  labs(title = paste0("B."))+
  xlab(paste("\u03BC", "*"))+
  ylab("\u03C3 ") +
  
  theme(
    axis.title = element_text(size = 20, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )
os2.2rhpudac<-ggplot(analysisrhpunidmusig[analysisrhpunidmusig$metric == "dir-cons-index-avoids_mean",], aes(x = mustar, y = sigma, label = numlabel)) +
  geom_point() + geom_label_repel(size = 3) +
  labs(title = paste0("C."))+
  xlab(paste("\u03BC", "*"))+
  ylab("\u03C3 ") +
  
  theme(
    axis.title = element_text(size = 20, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )
os2.2rhpudd<-ggplot(analysisrhpunidmusig[analysisrhpunidmusig$metric == "n-interactions_mean",], aes(x = mustar, y = sigma, label = numlabel)) +
  geom_point() + geom_label_repel(size = 3) +
  labs(title = paste0("D."))+
  xlab(paste("\u03BC", "*"))+
  ylab("\u03C3 ") +
  
  theme(
    axis.title = element_text(size = 20, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )
os2.2rhpude<-ggplot(analysisrhpunidmusig[analysisrhpunidmusig$metric == "proportion-attacking_mean",], aes(x = mustar, y = sigma, label = numlabel)) +
  geom_point() + geom_label_repel(size = 3) +
  labs(title = paste0("E."))+
  xlab(paste("\u03BC", "*"))+
  ylab("\u03C3 ") +
  
  theme(
    axis.title = element_text(size = 20, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )
os2.2rhpudf<-ggplot(analysisrhpunidmusig[analysisrhpunidmusig$metric == "foraging-efficiency-time_mean",], aes(x = mustar, y = sigma, label = numlabel)) +
  geom_point() + geom_label_repel(size = 3) +
  labs(title = paste0("F."))+
  xlab(paste("\u03BC", "*"))+
  ylab("\u03C3 ") +
  
  theme(
    axis.title = element_text(size = 20, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )

pdf("Github/dominance/AmNatjan23MEErhpunid.pdf")
multiplot(
  os2.2rhpuda,
  os2.2rhpudab, 
  os2.2rhpudac,
  os2.2rhpudd,
  os2.2rhpude,
  os2.2rhpudf, 
  cols = 2
)
dev.off()

#experience-based decisions, initiator wins, clumped resources

analysishistclimusig$numlabel <- rep(1:13, 6)

os2.4a<-ggplot(analysishistclimusig[analysishistclimusig$metric == "dir-cons-index-wins_mean",], aes(x = mustar, y = sigma, label = numlabel)) +
  geom_point() + geom_label_repel(size = 3) +
  labs(title = paste0("A."))+
  xlab(paste("\u03BC", "*"))+
  ylab("\u03C3 ") +
  
  theme(
    axis.title = element_text(size = 20, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )
os2.4b<-ggplot(analysishistclimusig[analysishistclimusig$metric == "dir-cons-index-attacks_mean",], aes(x = mustar, y = sigma, label = numlabel)) +
  geom_point() + geom_label_repel(size = 3) +
  labs(title = paste0("B."))+
  xlab(paste("\u03BC", "*"))+
  ylab("\u03C3 ") +
  
  theme(
    axis.title = element_text(size = 20, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )
os2.4c<-ggplot(analysishistclimusig[analysishistclimusig$metric == "dir-cons-index-avoids_mean",], aes(x = mustar, y = sigma, label = numlabel)) +
  geom_point() + geom_label_repel(size = 3) +
  labs(title = paste0("C."))+
  xlab(paste("\u03BC", "*"))+
  ylab("\u03C3 ") +
  
  theme(
    axis.title = element_text(size = 20, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )
os2.4d<-ggplot(analysishistclimusig[analysishistclimusig$metric == "n-interactions_mean",], aes(x = mustar, y = sigma, label = numlabel)) +
  geom_point() + geom_label_repel(size = 3) +
  labs(title = paste0("D."))+
  xlab(paste("\u03BC", "*"))+
  ylab("\u03C3 ") +
  
  theme(
    axis.title = element_text(size = 20, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )
os2.4e<-ggplot(analysishistclimusig[analysishistclimusig$metric == "proportion-attacking_mean",], aes(x = mustar, y = sigma, label = numlabel)) +
  geom_point() + geom_label_repel(size = 3) +
  labs(title = paste0("E."))+
  xlab(paste("\u03BC", "*"))+
  ylab("\u03C3 ") +
  
  theme(
    axis.title = element_text(size = 20, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )
os2.4f<-ggplot(analysishistclimusig[analysishistclimusig$metric == "foraging-efficiency-time_mean",], aes(x = mustar, y = sigma, label = numlabel)) +
  geom_point() + geom_label_repel(size = 3) +
  labs(title = paste0("F."))+
  xlab(paste("\u03BC", "*"))+
  ylab("\u03C3 ") +
  
  theme(
    axis.title = element_text(size = 20, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )

pdf("Github/dominance/AmNatjan23MEEexpcli.pdf")
multiplot(
  os2.4a,
  os2.4b, 
  os2.4c,
  os2.4d,
  os2.4e,
  os2.4f, 
  cols = 2
)
dev.off()


#Experience-based decisions, initiator wins, uniform resources

analysishistuniimusig$numlabel <- rep(1:13, 6)

os2.5a<-ggplot(analysishistuniimusig[analysishistuniimusig$metric == "dir-cons-index-wins_mean",], aes(x = mustar, y = sigma, label = numlabel)) +
  geom_point() + geom_label_repel(size = 3) +
  labs(title = paste0("A."))+
  xlab(paste("\u03BC", "*"))+
  ylab("\u03C3 ") +
  
  theme(
    axis.title = element_text(size = 20, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )
os2.5b<-ggplot(analysishistuniimusig[analysishistuniimusig$metric == "dir-cons-index-attacks_mean",], aes(x = mustar, y = sigma, label = numlabel)) +
  geom_point() + geom_label_repel(size = 3) +
  labs(title = paste0("B."))+
  xlab(paste("\u03BC", "*"))+
  ylab("\u03C3 ") +
  
  theme(
    axis.title = element_text(size = 20, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )
os2.5c<-ggplot(analysishistuniimusig[analysishistuniimusig$metric == "dir-cons-index-avoids_mean",], aes(x = mustar, y = sigma, label = numlabel)) +
  geom_point() + geom_label_repel(size = 3) +
  labs(title = paste0("C."))+
  xlab(paste("\u03BC", "*"))+
  ylab("\u03C3 ") +
  
  theme(
    axis.title = element_text(size = 20, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )
os2.5d<-ggplot(analysishistuniimusig[analysishistuniimusig$metric == "n-interactions_mean",], aes(x = mustar, y = sigma, label = numlabel)) +
  geom_point() + geom_label_repel(size = 3) +
  labs(title = paste0("D."))+
  xlab(paste("\u03BC", "*"))+
  ylab("\u03C3 ") +
  
  theme(
    axis.title = element_text(size = 20, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )
os2.5e<-ggplot(analysishistuniimusig[analysishistuniimusig$metric == "proportion-attacking_mean",], aes(x = mustar, y = sigma, label = numlabel)) +
  geom_point() + geom_label_repel(size = 3) +
  labs(title = paste0("E."))+
  xlab(paste("\u03BC", "*"))+
  ylab("\u03C3 ") +
  
  theme(
    axis.title = element_text(size = 20, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )
os2.5f<-ggplot(analysishistuniimusig[analysishistuniimusig$metric == "foraging-efficiency-time_mean",], aes(x = mustar, y = sigma, label = numlabel)) +
  geom_point() + geom_label_repel(size = 3) +
  labs(title = paste0("F."))+
  xlab(paste("\u03BC", "*"))+
  ylab("\u03C3 ") +
  
  theme(
    axis.title = element_text(size = 20, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )

pdf("Github/dominance/AmNatjan23MEEexpunii.pdf")
multiplot(
  os2.5a,
  os2.5b, 
  os2.5c,
  os2.5d,
  os2.5e,
  os2.5f, 
  cols = 2
)
dev.off()


#non-main-text scenarios below
pdf("Github/dominance/AmNatMEEscatterexpcldv3.pdf")

metric.levels <- unique(analysishistcldmusig$metric)

analysishistcldmusig$numlabel <- rep(1:13, 6)

for (metric in metric.levels) {
print(ggplot(analysishistcldmusig[analysishistcldmusig$metric == metric,], aes(x = mustar, y = sigma, label = numlabel)) +
  geom_point() + geom_label_repel(size = 3) +
  labs(title = paste0("MEE, Exp Clumped Deter. ", metric))+
    xlab(paste("\u03BC", "*"))+
    ylab("\u03C3 ") +
    scale_x_continuous(expand = c(0.1,0.1))+
    theme(
      axis.title = element_text(size = 20, color = "black"),
      axis.text = element_text(size = 8, color = "black"),
      
      panel.grid = element_line(color = "black"),
      panel.grid.major = element_line(color = "gray75"),
      panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
    ))
}
dev.off()

pdf("Github/dominance/AmNatMEEscatterexpunidv3.pdf")

metric.levels <- unique(analysishistunidmusig$metric)

for (metric in metric.levels) {
  print(ggplot(analysishistunidmusig[analysishistcldmusig$metric == metric,], aes(x = mustar, y = sigma, label = parameter)) +
          geom_point() + geom_label_repel(size = 3) +
          labs(title = paste0("MEE, Exp Uniform Deter ", metric)))
}
dev.off()

pdf("Github/dominance/AmNatMEEscatterrhpcliv3.pdf")

metric.levels <- unique(analysisrhpclimusig$metric)

for (metric in metric.levels) {
  print(ggplot(analysisrhpclimusig[analysisrhpclimusig$metric == metric,], aes(x = mustar, y = sigma, label = parameter)) +
          geom_point() + geom_label_repel(size = 3) +
          labs(title = paste0("MEE, RHP Clumped Init ", metric)))
}
dev.off()

pdf("Github/dominance/AmNatMEEscatterrhpuniiv3.pdf")

metric.levels <- unique(analysishistclimusig$metric)

for (metric in metric.levels) {
  print(ggplot(analysisrhpuniimusig[analysisrhpuniimusig$metric == metric,], aes(x = mustar, y = sigma, label = parameter)) +
          geom_point() + geom_label_repel(size = 3) +
          labs(title = paste0("MEE, RHP Uniform Init. ", metric)))
}
dev.off()



#####


#detailed sensitivity analysis - jitter plots

combined.df <- rbind(resultsMorrisch2histcli, resultsMorrisch2histunii, resultsMorrisch2rhpcld, resultsMorrisch2rhpunid)

#things to plot:

#starting-pop-primates - DCI wins, DCI avoids, ninteractions, forating eff
a<-ggplot(combined.df, mapping = aes(x = `starting-pop-primates`, y = `dir-cons-index-wins`)) + 
  geom_jitter(size = 0.5) + 
  labs(title = paste0("A."))+
  ylab("DCI fight outcomes")+
  geom_vline(xintercept = 10, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

b<-ggplot(combined.df, mapping = aes(x = `starting-pop-primates`, y = `dir-cons-index-avoids`)) + 
  geom_jitter(size = 0.5) + 
  labs(title = paste0("B."))+
  ylab("DCI not-attacks")+
  geom_vline(xintercept = 10, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

c<-ggplot(combined.df, mapping = aes(x = `starting-pop-primates`, y = `n-interactions`)) + 
  geom_jitter(size = 0.5) +
  labs(title = paste0("C."))+
  ylab("N interactions")+
  geom_vline(xintercept = 10, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

d<-ggplot(combined.df, mapping = aes(x = `starting-pop-primates`, y = `foraging-efficiency-time`)) + 
  geom_jitter(size = 0.5) + 
  labs(title = paste0("D."))+
  ylab("Energy intake rate")+
  geom_vline(xintercept = 10, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

multiplot(a, b, c, d, cols=2)


#regrowth-denominator - DCI of avoids, foraging eff
a<-ggplot(combined.df, mapping = aes(x = `regrowth-denominator`, y = `dir-cons-index-avoids`)) + 
  geom_jitter(size = 0.5) + 
  labs(title = paste0("A."))+
  ylab("DCI not-attacks")+
  geom_vline(xintercept = 3, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")
b<-ggplot(combined.df, mapping = aes(x = `regrowth-denominator`, y = `foraging-efficiency-time`)) + 
  geom_jitter(size = 0.5) + 
  labs(title = paste0("B."))+
  ylab("Energy intake rate")+
  geom_vline(xintercept = 3, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")
multiplot(a, b)

#regrwoth-freq - ninteractions, foraging eff
a<-ggplot(combined.df, mapping = aes(x = `regrow-freq`, y = `n-interactions`)) + 
  geom_jitter(size = 0.5) +
  labs(title = paste0("A."))+
  ylab("N interactions")+
  geom_vline(xintercept = 24, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")
b<-ggplot(combined.df, mapping = aes(x = `regrow-freq`, y = `foraging-efficiency-time`)) + 
  geom_jitter(size = 0.5) + 
  labs(title = paste0("B."))+
  ylab("Energy intake rate")+
  geom_vline(xintercept = 24, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")
multiplot(a, b)

#step-distance - all metrics
a<-ggplot(combined.df, mapping = aes(x = `step-distance`, y = `dir-cons-index-wins`)) + 
  geom_jitter(size = 0.5) + 
  labs(title = paste0("A."))+
  ylab("DCI fight outcomes")+
  geom_vline(xintercept = 1, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

b<-ggplot(combined.df, mapping = aes(x = `step-distance`, y = `dir-cons-index-attacks`)) + 
  geom_jitter(size = 0.5) + 
  labs(title = paste0("B."))+
  ylab("DCI attacks")+
  geom_vline(xintercept = 1, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")
c<-ggplot(combined.df, mapping = aes(x = `step-distance`, y = `dir-cons-index-avoids`)) + 
  geom_jitter(size = 0.5) + 
  labs(title = paste0("C."))+
  ylab("DCI not-attacks")+
  geom_vline(xintercept = 1, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

d<-ggplot(combined.df, mapping = aes(x = `step-distance`, y = `n-interactions`)) + 
  geom_jitter(size = 0.5) +
  labs(title = paste0("D."))+
  ylab("N interactions")+
  geom_vline(xintercept = 1, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

e <-ggplot(combined.df, mapping = aes(x = `step-distance`, y = `proportion-attacking`)) + 
  geom_jitter(size = 0.5) +
  labs(title = paste0("E."))+
  ylab("Proportion attacks")+
  geom_vline(xintercept = 1, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

f<-ggplot(combined.df, mapping = aes(x = `step-distance`, y = `foraging-efficiency-time`)) + 
  geom_jitter(size = 0.5) + 
  labs(title = paste0("F."))+
  ylab("Energy intake rate")+
  geom_vline(xintercept = 1, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

multiplot(a, b, c, d, e, f, cols=2)

#resource-detection-radius - DCI wins, DCI attacks, DCI avoids, ninteractions, foraging eff
a<-ggplot(combined.df, mapping = aes(x = `resource-detection-radius`, y = `dir-cons-index-wins`)) + 
  geom_jitter(size = 0.5) + 
  labs(title = paste0("A."))+
  ylab("DCI fight outcomes")+
  geom_vline(xintercept = 5, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

b<-ggplot(combined.df, mapping = aes(x = `resource-detection-radius`, y = `dir-cons-index-attacks`)) + 
  geom_jitter(size = 0.5) + 
  labs(title = paste0("B."))+
  ylab("DCI attacks")+
  geom_vline(xintercept = 5, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")
c<-ggplot(combined.df, mapping = aes(x = `resource-detection-radius`, y = `dir-cons-index-avoids`)) + 
  geom_jitter(size = 0.5) + 
  labs(title = paste0("C."))+
  ylab("DCI not-attacks")+
  geom_vline(xintercept = 5, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

d<-ggplot(combined.df, mapping = aes(x = `resource-detection-radius`, y = `n-interactions`)) + 
  geom_jitter(size = 0.5) +
  labs(title = paste0("D."))+
  ylab("N interactions")+
  geom_vline(xintercept = 5, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

e <-ggplot(combined.df, mapping = aes(x = `resource-detection-radius`, y = `foraging-efficiency-time`)) + 
  geom_jitter(size = 0.5) + 
  labs(title = paste0("E."))+
  ylab("Energy intake rate")+
  geom_vline(xintercept = 5, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

multiplot(a, b, c, d, e,  cols=2)


#max-nearest-primates - DCI wins, DCI attacks, 
a<-ggplot(combined.df, mapping = aes(x = `max-nearest-primates`, y = `dir-cons-index-wins`)) + 
  geom_jitter(size = 0.5) + 
  labs(title = paste0("A."))+
  ylab("DCI fight outcomes")+
  geom_vline(xintercept = 3, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

b<-ggplot(combined.df, mapping = aes(x = `max-nearest-primates`, y = `dir-cons-index-attacks`)) + 
  geom_jitter(size = 0.5) + 
  labs(title = paste0("B."))+
  ylab("DCI attacks")+
  geom_vline(xintercept = 3, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

multiplot(a, b)


#other-primate-detection-radius - DCI of avoids
ggplot(combined.df, mapping = aes(x = `other-primate-detection-radius`, y = `dir-cons-index-attacks`)) + 
  geom_jitter(size = 0.5) + 

  ylab("DCI attacks")+
  geom_vline(xintercept = 5, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")







ggplot(resultsMorrisch2histcli, mapping = aes(x = `change-in-exp-score`, y = `dir-cons-index-wins`)) + 
  geom_jitter(size = 0.5) + 
  #geom_vline(xintercept = 10, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

ggplot(resultsMorrisch2histcli, aes(x = `change-in-exp-score`, y = `regrowth-denominator`, fill = `dir-cons-index-wins`)) +
  geom_tile()+
  scale_fill_gradient(low = "lightblue", high = "navy", guide = "colorbar")

ggplot(resultsMorrisch2histcli, mapping = aes(x = `change-in-exp-score`, y = `dir-cons-index-avoids`)) + 
  geom_jitter(size = 0.5) + 
  #geom_vline(xintercept = 10, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

ggplot(resultsMorrisch2histcli, aes(x = `change-in-exp-score`, y = `regrowth-denominator`, fill = `dir-cons-index-avoids`)) +
  geom_tile()+
  scale_fill_gradient(low = "lightblue", high = "navy", guide = "colorbar")



ggplot(resultsMorrisch2histunii, mapping = aes(x = `change-in-exp-score`, y = `dir-cons-index-wins`)) + 
  geom_jitter(size = 0.5) + 
  #geom_vline(xintercept = 10, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

ggplot(resultsMorrisch2histunii, mapping = aes(x = `change-in-exp-score`, y = `dir-cons-index-avoids`)) + 
  geom_jitter(size = 0.5) + 
  #geom_vline(xintercept = 10, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

ggplot(resultsMorrisch2histunii, aes(x = `change-in-exp-score`, y = `regrowth-denominator`, fill = `dir-cons-index-wins`)) +
  geom_tile()+
  scale_fill_gradient(low = "lightblue", high = "navy", guide = "colorbar")

ggplot(resultsMorrisch2histunii, aes(x = `change-in-exp-score`, y = `regrowth-denominator`, fill = `dir-cons-index-avoids`)) +
  geom_tile()+
  scale_fill_gradient(low = "lightblue", high = "navy", guide = "colorbar")













combinedresults <- rbind(resultsMorrisch2histcl, resultsMorrisch2histuni, resultsMorrisch2rhpcl, resultsMorrisch2rhpuni)


#starting pop primates
a<-ggplot(combinedresults, mapping = aes(x = `starting-pop-primates`, y = `foraging-efficiency-time`)) + 
  geom_jitter(size = 0.5) + 
  geom_vline(xintercept = 10, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

b<-ggplot(combinedresults, mapping = aes(x = `starting-pop-primates`, y = `n-interactions`)) + 
  geom_jitter(size = 0.5) + 
  geom_vline(xintercept = 10, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

c<-ggplot(combinedresults, mapping = aes(x = `starting-pop-primates`, y = `proportion-attacking`)) + 
  geom_jitter(size = 0.5) + 
  geom_vline(xintercept = 10, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

d<-ggplot(combinedresults, mapping = aes(x = `starting-pop-primates`, y = `dir-cons-index-wins`)) + 
  geom_jitter(size = 0.5) + 
  geom_vline(xintercept = 10, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

e<-ggplot(combinedresults, mapping = aes(x = `starting-pop-primates`, y = `dir-cons-index-attacks`)) + 
  geom_jitter(size = 0.5) + 
  geom_vline(xintercept = 10, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

f<-ggplot(combinedresults, mapping = aes(x = `starting-pop-primates`, y = `dir-cons-index-avoids`)) + 
  geom_jitter(size = 0.5) + 
  geom_vline(xintercept = 10, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

multiplot(a, b, c, d, e, f, cols=2)








#regrowth denominator
a<-ggplot(combinedresults, mapping = aes(x = `regrowth-denominator`, y = `foraging-efficiency-time`)) + 
  geom_jitter(size = 0.5) + 
  geom_vline(xintercept = 3, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

b<-ggplot(combinedresults, mapping = aes(x = `regrowth-denominator`, y = `n-interactions`)) + 
  geom_jitter(size = 0.5) + 
  geom_vline(xintercept = 3, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

c<-ggplot(combinedresults, mapping = aes(x = `regrowth-denominator`, y = `proportion-attacking`)) + 
  geom_jitter(size = 0.5) + 
  geom_vline(xintercept = 3, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

d<-ggplot(combinedresults, mapping = aes(x = `regrowth-denominator`, y = `dir-cons-index-wins`)) + 
  geom_jitter(size = 0.5) + 
  geom_vline(xintercept = 3, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

e<-ggplot(combinedresults, mapping = aes(x = `regrowth-denominator`, y = `dir-cons-index-attacks`)) + 
  geom_jitter(size = 0.5) + 
  geom_vline(xintercept = 3, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

f<-ggplot(combinedresults, mapping = aes(x = `regrowth-denominator`, y = `dir-cons-index-avoids`)) + 
  geom_jitter(size = 0.5) + 
  geom_vline(xintercept = 3, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")
multiplot(a, b, c, d, e, f, cols=2)



#regrow-freq
a<-ggplot(combinedresults, mapping = aes(x = `regrow-freq`, y = `foraging-efficiency-time`)) + 
  geom_jitter(size = 0.5)+ 
  geom_vline(xintercept = 24, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")
b<-ggplot(combinedresults, mapping = aes(x = `regrow-freq`, y = `n-interactions`)) + 
  geom_jitter(size = 0.5)+ 
  geom_vline(xintercept = 24, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")
c<-ggplot(combinedresults, mapping = aes(x = `regrow-freq`, y = `proportion-attacking`)) + 
  geom_jitter(size = 0.5)+ 
  geom_vline(xintercept = 24, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

d<-ggplot(combinedresults, mapping = aes(x = `regrow-freq`, y = `dir-cons-index-wins`)) + 
  geom_jitter(size = 0.5)+ 
  geom_vline(xintercept = 24, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")
e<-ggplot(combinedresults, mapping = aes(x = `regrow-freq`, y = `dir-cons-index-attacks`)) + 
  geom_jitter(size = 0.5)+ 
  geom_vline(xintercept = 24, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

f<-ggplot(combinedresults, mapping = aes(x = `regrow-freq`, y = `dir-cons-index-avoids`)) + 
  geom_jitter(size = 0.5)+ 
  geom_vline(xintercept = 24, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

multiplot(a, b, c, d, e, f, cols=2)

 




#resource-detection-radius
a<-ggplot(combinedresults, mapping = aes(x = `resource-detection-radius`, y = `foraging-efficiency-time`)) + 
  geom_jitter(size = 0.5)+ 
  geom_vline(xintercept = 4, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")
b<-ggplot(combinedresults, mapping = aes(x = `resource-detection-radius`, y = `n-interactions`)) + 
  geom_jitter(size = 0.5)+ 
  geom_vline(xintercept = 4, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")
c<-ggplot(combinedresults, mapping = aes(x = `resource-detection-radius`, y = `proportion-attacking`)) + 
  geom_jitter(size = 0.5)+ 
  geom_vline(xintercept = 4, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

d<-ggplot(combinedresults, mapping = aes(x = `resource-detection-radius`, y = `dir-cons-index-wins`)) + 
  geom_jitter(size = 0.5)+ 
  geom_vline(xintercept = 4, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")
e<-ggplot(combinedresults, mapping = aes(x = `resource-detection-radius`, y = `dir-cons-index-attacks`)) + 
  geom_jitter(size = 0.5)+ 
  geom_vline(xintercept = 4, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

f<-ggplot(combinedresults, mapping = aes(x = `resource-detection-radius`, y = `dir-cons-index-avoids`)) + 
  geom_jitter(size = 0.5)+ 
  geom_vline(xintercept = 4, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

multiplot(a, b, c, d, e, f, cols=2)



#step-distance
a<-ggplot(combinedresults, mapping = aes(x = `step-distance`, y = `foraging-efficiency-time`)) + 
  geom_jitter(size = 0.5) + 
  geom_vline(xintercept = 1, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

b<-ggplot(combinedresults, mapping = aes(x = `step-distance`, y = `n-interactions`)) + 
  geom_jitter(size = 0.5) + 
  geom_vline(xintercept = 1, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")


multiplot(a, b, cols=1)

##################################################################################
## below are the plots from the first round only

#need to plot every metric for step-distance - but it's already clear the issue is that it completely broke the simulation above 2

ggplot(combinedresults, mapping = aes(x = `step-distance`, y = `foraging-efficiency-time`)) + 
  geom_jitter(size = 0.5) + 
  geom_vline(xintercept = 1, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

ggplot(combinedresults, mapping = aes(x = `step-distance`, y = `n-interactions`)) + 
  geom_jitter(size = 0.5) + 
  geom_vline(xintercept = 1, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

ggplot(combinedresults, mapping = aes(x = `step-distance`, y = `proportion-attacking`)) + 
  geom_jitter(size = 0.5) + 
  geom_vline(xintercept = 1, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

ggplot(combinedresults, mapping = aes(x = `step-distance`, y = `dir-cons-index-wins`)) + 
  geom_jitter(size = 0.5) + 
  geom_vline(xintercept = 1, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

ggplot(combinedresults, mapping = aes(x = `step-distance`, y = `dir-cons-index-attacks`)) + 
  geom_jitter(size = 0.5) + 
  geom_vline(xintercept = 1, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

ggplot(combinedresults, mapping = aes(x = `step-distance`, y = `dir-cons-index-avoids`)) + 
  geom_jitter(size = 0.5) + 
  geom_vline(xintercept = 1, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")






ggplot(combinedresults, mapping = aes(x = `regrow-freq`, y = log(`n-interactions`))) + 
  geom_jitter(size = 0.5)+ 
  geom_vline(xintercept = 24, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")
# these are weird because it was trying to do modulo of decimals, meaning regrowth never got called

ggplot(combinedresults, mapping = aes(x = `regrow-freq`, y = `dir-cons-index-attacks`)) + 
  geom_jitter(size = 0.5)+ 
  geom_vline(xintercept = 24, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

ggplot(combinedresults, mapping = aes(x = `regrow-freq`, y = `dir-cons-index-avoids`)) + 
  geom_jitter(size = 0.5)+ 
  geom_vline(xintercept = 24, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")


#should probably do everything for starting pop too
ggplot(combinedresults, mapping = aes(x = `starting-pop-primates`, y = `foraging-efficiency-time`)) + 
  geom_jitter(size = 0.5) + 
  geom_vline(xintercept = 10, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

ggplot(combinedresults, mapping = aes(x = `starting-pop-primates`, y = `n-interactions`)) + 
  geom_jitter(size = 0.5) + 
  geom_vline(xintercept = 10, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

ggplot(combinedresults, mapping = aes(x = `starting-pop-primates`, y = `proportion-attacking`)) + 
  geom_jitter(size = 0.5) + 
  geom_vline(xintercept = 10, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

ggplot(combinedresults, mapping = aes(x = `starting-pop-primates`, y = `dir-cons-index-wins`)) + 
  geom_jitter(size = 0.5) + 
  geom_vline(xintercept = 10, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

ggplot(combinedresults, mapping = aes(x = `starting-pop-primates`, y = `dir-cons-index-attacks`)) + 
  geom_jitter(size = 0.5) + 
  geom_vline(xintercept = 10, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

ggplot(combinedresults, mapping = aes(x = `starting-pop-primates`, y = `dir-cons-index-avoids`)) + 
  geom_jitter(size = 0.5) + 
  geom_vline(xintercept = 10, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")
  












#"fix" results by removing the broken regrowth freq and step-distance
combinedresultsfixed <- combinedresults[combinedresults$`regrow-freq` %% 10 == 0 & combinedresults$`step-distance`< 2,]

ggplot(combinedresultsfixed, mapping = aes(x = `step-distance`, y = `foraging-efficiency-time`)) + 
  geom_jitter(size = 0.5) + 
  geom_vline(xintercept = 1, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

ggplot(combinedresultsfixed, mapping = aes(x = `step-distance`, y = `n-interactions`)) + 
  geom_jitter(size = 0.5) + 
  geom_vline(xintercept = 1, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

ggplot(combinedresultsfixed, mapping = aes(x = `step-distance`, y = `proportion-attacking`)) + 
  geom_jitter(size = 0.5) + 
  geom_vline(xintercept = 1, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

ggplot(combinedresultsfixed, mapping = aes(x = `step-distance`, y = `dir-cons-index-wins`)) + 
  geom_jitter(size = 0.5) + 
  geom_vline(xintercept = 1, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

ggplot(combinedresultsfixed, mapping = aes(x = `step-distance`, y = `dir-cons-index-attacks`)) + 
  geom_jitter(size = 0.5) + 
  geom_vline(xintercept = 1, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

ggplot(combinedresultsfixed, mapping = aes(x = `step-distance`, y = `dir-cons-index-avoids`)) + 
  geom_jitter(size = 0.5) + 
  geom_vline(xintercept = 1, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

ggplot(combinedresultsfixed, mapping = aes(x = `regrow-freq`, y = log(`n-interactions`))) + 
  geom_jitter(size = 0.5)+ 
  geom_vline(xintercept = 24, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")
# these are weird because it was trying to do modulo of decimals, meaning regrowth never got called


ggplot(combinedresultsfixed, mapping = aes(x = `regrow-freq`, y = `dir-cons-index-attacks`)) + 
  geom_jitter(size = 0.5)+ 
  geom_vline(xintercept = 24, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

ggplot(combinedresultsfixed, mapping = aes(x = `regrow-freq`, y = `dir-cons-index-avoids`)) + 
  geom_jitter(size = 0.5)+ 
  geom_vline(xintercept = 24, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

  
  
  
#should probably do everything for starting pop too
  ggplot(combinedresultsfixed, mapping = aes(x = `starting-pop-primates`, y = `foraging-efficiency-time`)) + 
  geom_jitter(size = 0.5) + 
  geom_vline(xintercept = 10, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

ggplot(combinedresultsfixed, mapping = aes(x = `starting-pop-primates`, y = `n-interactions`)) + 
  geom_jitter(size = 0.5) + 
  geom_vline(xintercept = 10, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

ggplot(combinedresultsfixed, mapping = aes(x = `starting-pop-primates`, y = `proportion-attacking`)) + 
  geom_jitter(size = 0.5) + 
  geom_vline(xintercept = 10, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

ggplot(combinedresultsfixed, mapping = aes(x = `starting-pop-primates`, y = `dir-cons-index-wins`)) + 
  geom_jitter(size = 0.5) + 
  geom_vline(xintercept = 10, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

ggplot(combinedresultsfixed, mapping = aes(x = `starting-pop-primates`, y = `dir-cons-index-attacks`)) + 
  geom_jitter(size = 0.5) + 
  geom_vline(xintercept = 10, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

ggplot(combinedresultsfixed, mapping = aes(x = `starting-pop-primates`, y = `dir-cons-index-avoids`)) + 
  geom_jitter(size = 0.5) + 
  geom_vline(xintercept = 10, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")



# for resource-detection-radius: proportion-attacking, DCI-wins, DCI-attacks, foraging efficiency
ggplot(combinedresults, mapping = aes(x = `resource-detection-radius`, y = `foraging-efficiency-time`)) + 
  geom_jitter(size = 0.5) + 
  geom_vline(xintercept = 4, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

ggplot(combinedresults, mapping = aes(x = `resource-detection-radius`, y = `proportion-attacking`)) + 
  geom_jitter(size = 0.5) + 
  geom_vline(xintercept = 4, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

ggplot(combinedresults, mapping = aes(x = `resource-detection-radius`, y = `dir-cons-index-wins`)) + 
  geom_jitter(size = 0.5) + 
  geom_vline(xintercept = 4, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

ggplot(combinedresults, mapping = aes(x = `resource-detection-radius`, y = `dir-cons-index-attacks`)) + 
  geom_jitter(size = 0.5) + 
  geom_vline(xintercept = 4, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

ggplot(combinedresultsfixed, mapping = aes(x = `resource-detection-radius`, y = `foraging-efficiency-time`)) + 
  geom_jitter(size = 0.5) + 
  geom_vline(xintercept = 4, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

ggplot(combinedresultsfixed, mapping = aes(x = `resource-detection-radius`, y = `proportion-attacking`)) + 
  geom_jitter(size = 0.5) + 
  geom_vline(xintercept = 4, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

ggplot(combinedresultsfixed, mapping = aes(x = `resource-detection-radius`, y = `dir-cons-index-wins`)) + 
  geom_jitter(size = 0.5) + 
  geom_vline(xintercept = 4, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

ggplot(combinedresultsfixed, mapping = aes(x = `resource-detection-radius`, y = `dir-cons-index-attacks`)) + 
  geom_jitter(size = 0.5) + 
  geom_vline(xintercept = 4, color = "blue", size = 2) +
  stat_summary(fun = "median", color = "red")

#mu-sigma bar plots below - do not use anymore
######################
pdf("Github/dominance/AmNatMEEhistoryclumped.pdf")

ggplot(analysishistcl$`n-interactions_mean`, aes(x=reorder(parameter, -value), y=value, fill = index)) +
  geom_bar(stat='identity', position='dodge') + 
  scale_fill_manual(labels = c("mu (overall effect size)", 
                               "mu-star (absolute value effect size)", 
                               "sigma (non-linear and interaction effects)"),
                    values = c("blue", "red", "orange")) + 
  #geom_text(aes(label = value)) + 
  labs(title = "Morris Elementary Effects, History Clumped, n interactions") + 
  theme(legend.position="bottom", axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))

ggplot(analysishistcl$`proportion-attacking_mean`, aes(x=reorder(parameter, -value), y=value, fill = index)) +
  geom_bar(stat='identity', position='dodge') + 
  scale_fill_manual(labels = c("mu (overall effect size)", 
                               "mu-star (absolute value effect size)", 
                               "sigma (non-linear and interaction effects)"),
                    values = c("blue", "red", "orange")) + 
  #geom_text(aes(label = value)) + 
  labs(title = "Morris Elementary Effects, History Clumped, prop attack") + 
  theme(legend.position="bottom", axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))

ggplot(analysishistcl$`dir-cons-index-wins_mean`, aes(x=reorder(parameter, -value), y=value, fill = index)) +
  geom_bar(stat='identity', position='dodge') + 
  scale_fill_manual(labels = c("mu (overall effect size)", 
                               "mu-star (absolute value effect size)", 
                               "sigma (non-linear and interaction effects)"),
                    values = c("blue", "red", "orange")) + 
  #geom_text(aes(label = value)) + 
  labs(title = "Morris Elementary Effects, History Clumped, DCI wins") + 
  theme(legend.position="bottom", axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))


ggplot(analysishistcl$`dir-cons-index-attacks_mean`, aes(x=reorder(parameter, -value), y=value, fill = index)) +
  geom_bar(stat='identity', position='dodge') + 
  scale_fill_manual(labels = c("mu (overall effect size)", 
                               "mu-star (absolute value effect size)", 
                               "sigma (non-linear and interaction effects)"),
                    values = c("blue", "red", "orange")) + 
  #geom_text(aes(label = value)) + 
  labs(title = "Morris Elementary Effects, History Clumped, DCI attacks") + 
  theme(legend.position="bottom", axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))

ggplot(analysishistcl$`dir-cons-index-avoids_mean`, aes(x=reorder(parameter, -value), y=value, fill = index)) +
  geom_bar(stat='identity', position='dodge') + 
  scale_fill_manual(labels = c("mu (overall effect size)", 
                               "mu-star (absolute value effect size)", 
                               "sigma (non-linear and interaction effects)"),
                    values = c("blue", "red", "orange")) + 
  #geom_text(aes(label = value)) + 
  labs(title = "Morris Elementary Effects, History Clumped, DCI avoids") + 
  theme(legend.position="bottom", axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))

ggplot(analysishistcl$`foraging-efficiency-time_mean`, aes(x=reorder(parameter, -value), y=value, fill = index)) +
  geom_bar(stat='identity', position='dodge') + 
  scale_fill_manual(labels = c("mu (overall effect size)", 
                               "mu-star (absolute value effect size)", 
                               "sigma (non-linear and interaction effects)"),
                    values = c("blue", "red", "orange")) + 
  #geom_text(aes(label = value)) + 
  labs(title = "Morris Elementary Effects, History Clumped, foraging") + 
  theme(legend.position="bottom", axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))

dev.off()

pdf("Github/dominance/AmNatMEEhistoryuniform.pdf")

ggplot(analysishistuni$`n-interactions_mean`, aes(x=reorder(parameter, -value), y=value, fill = index)) +
  geom_bar(stat='identity', position='dodge') + 
  scale_fill_manual(labels = c("mu (overall effect size)", 
                               "mu-star (absolute value effect size)", 
                               "sigma (non-linear and interaction effects)"),
                    values = c("blue", "red", "orange")) + 
  #geom_text(aes(label = value)) + 
  labs(title = "Morris Elementary Effects, History Uniform, n interactions") + 
  theme(legend.position="bottom", axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))

ggplot(analysishistuni$`proportion-attacking_mean`, aes(x=reorder(parameter, -value), y=value, fill = index)) +
  geom_bar(stat='identity', position='dodge') + 
  scale_fill_manual(labels = c("mu (overall effect size)", 
                               "mu-star (absolute value effect size)", 
                               "sigma (non-linear and interaction effects)"),
                    values = c("blue", "red", "orange")) + 
  #geom_text(aes(label = value)) + 
  labs(title = "Morris Elementary Effects, History Uniform, prop attack") + 
  theme(legend.position="bottom", axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))

ggplot(analysishistuni$`dir-cons-index-wins_mean`, aes(x=reorder(parameter, -value), y=value, fill = index)) +
  geom_bar(stat='identity', position='dodge') + 
  scale_fill_manual(labels = c("mu (overall effect size)", 
                               "mu-star (absolute value effect size)", 
                               "sigma (non-linear and interaction effects)"),
                    values = c("blue", "red", "orange")) + 
  #geom_text(aes(label = value)) + 
  labs(title = "Morris Elementary Effects, History Uniform, DCI wins") + 
  theme(legend.position="bottom", axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))


ggplot(analysishistuni$`dir-cons-index-attacks_mean`, aes(x=reorder(parameter, -value), y=value, fill = index)) +
  geom_bar(stat='identity', position='dodge') + 
  scale_fill_manual(labels = c("mu (overall effect size)", 
                               "mu-star (absolute value effect size)", 
                               "sigma (non-linear and interaction effects)"),
                    values = c("blue", "red", "orange")) + 
  #geom_text(aes(label = value)) + 
  labs(title = "Morris Elementary Effects, History Uniform, DCI attacks") + 
  theme(legend.position="bottom", axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))

ggplot(analysishistuni$`dir-cons-index-avoids_mean`, aes(x=reorder(parameter, -value), y=value, fill = index)) +
  geom_bar(stat='identity', position='dodge') + 
  scale_fill_manual(labels = c("mu (overall effect size)", 
                               "mu-star (absolute value effect size)", 
                               "sigma (non-linear and interaction effects)"),
                    values = c("blue", "red", "orange")) + 
  #geom_text(aes(label = value)) + 
  labs(title = "Morris Elementary Effects, History Uniform, DCI avoids") + 
  theme(legend.position="bottom", axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))

ggplot(analysishistuni$`foraging-efficiency-time_mean`, aes(x=reorder(parameter, -value), y=value, fill = index)) +
  geom_bar(stat='identity', position='dodge') + 
  scale_fill_manual(labels = c("mu (overall effect size)", 
                               "mu-star (absolute value effect size)", 
                               "sigma (non-linear and interaction effects)"),
                    values = c("blue", "red", "orange")) + 
  #geom_text(aes(label = value)) + 
  labs(title = "Morris Elementary Effects, History Uniform, foraging") + 
  theme(legend.position="bottom", axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))

dev.off()


pdf("Github/dominance/AmNatMEErhpclumped.pdf")

ggplot(analysisrhpcl$`n-interactions_mean`, aes(x=reorder(parameter, -value), y=value, fill = index)) +
  geom_bar(stat='identity', position='dodge') + 
  scale_fill_manual(labels = c("mu (overall effect size)", 
                               "mu-star (absolute value effect size)", 
                               "sigma (non-linear and interaction effects)"),
                    values = c("blue", "red", "orange")) + 
  #geom_text(aes(label = value)) + 
  labs(title = "Morris Elementary Effects, RHP Clumped, n interactions") + 
  theme(legend.position="bottom", axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))

ggplot(analysisrhpcl$`proportion-attacking_mean`, aes(x=reorder(parameter, -value), y=value, fill = index)) +
  geom_bar(stat='identity', position='dodge') + 
  scale_fill_manual(labels = c("mu (overall effect size)", 
                               "mu-star (absolute value effect size)", 
                               "sigma (non-linear and interaction effects)"),
                    values = c("blue", "red", "orange")) + 
  #geom_text(aes(label = value)) + 
  labs(title = "Morris Elementary Effects, RHP Clumped, prop attack") + 
  theme(legend.position="bottom", axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))

ggplot(analysisrhpcl$`dir-cons-index-wins_mean`, aes(x=reorder(parameter, -value), y=value, fill = index)) +
  geom_bar(stat='identity', position='dodge') + 
  scale_fill_manual(labels = c("mu (overall effect size)", 
                               "mu-star (absolute value effect size)", 
                               "sigma (non-linear and interaction effects)"),
                    values = c("blue", "red", "orange")) + 
  #geom_text(aes(label = value)) + 
  labs(title = "Morris Elementary Effects, RHP Clumped, DCI wins") + 
  theme(legend.position="bottom", axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))


ggplot(analysisrhpcl$`dir-cons-index-attacks_mean`, aes(x=reorder(parameter, -value), y=value, fill = index)) +
  geom_bar(stat='identity', position='dodge') + 
  scale_fill_manual(labels = c("mu (overall effect size)", 
                               "mu-star (absolute value effect size)", 
                               "sigma (non-linear and interaction effects)"),
                    values = c("blue", "red", "orange")) + 
  #geom_text(aes(label = value)) + 
  labs(title = "Morris Elementary Effects, RHP Clumped, DCI attacks") + 
  theme(legend.position="bottom", axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))

ggplot(analysisrhpcl$`dir-cons-index-avoids_mean`, aes(x=reorder(parameter, -value), y=value, fill = index)) +
  geom_bar(stat='identity', position='dodge') + 
  scale_fill_manual(labels = c("mu (overall effect size)", 
                               "mu-star (absolute value effect size)", 
                               "sigma (non-linear and interaction effects)"),
                    values = c("blue", "red", "orange")) + 
  #geom_text(aes(label = value)) + 
  labs(title = "Morris Elementary Effects, RHP Clumped, DCI avoids") + 
  theme(legend.position="bottom", axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))

ggplot(analysisrhpcl$`foraging-efficiency-time_mean`, aes(x=reorder(parameter, -value), y=value, fill = index)) +
  geom_bar(stat='identity', position='dodge') + 
  scale_fill_manual(labels = c("mu (overall effect size)", 
                               "mu-star (absolute value effect size)", 
                               "sigma (non-linear and interaction effects)"),
                    values = c("blue", "red", "orange")) + 
  #geom_text(aes(label = value)) + 
  labs(title = "Morris Elementary Effects, RHP Clumped, foraging") + 
  theme(legend.position="bottom", axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))

dev.off()

pdf("Github/dominance/AmNatMEErhpuniform.pdf")

ggplot(analysisrhpuni$`n-interactions_mean`, aes(x=reorder(parameter, -value), y=value, fill = index)) +
  geom_bar(stat='identity', position='dodge') + 
  scale_fill_manual(labels = c("mu (overall effect size)", 
                               "mu-star (absolute value effect size)", 
                               "sigma (non-linear and interaction effects)"),
                    values = c("blue", "red", "orange")) + 
  #geom_text(aes(label = value)) + 
  labs(title = "Morris Elementary Effects, RHP Uniform, n interactions") + 
  theme(legend.position="bottom", axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))

ggplot(analysisrhpuni$`proportion-attacking_mean`, aes(x=reorder(parameter, -value), y=value, fill = index)) +
  geom_bar(stat='identity', position='dodge') + 
  scale_fill_manual(labels = c("mu (overall effect size)", 
                               "mu-star (absolute value effect size)", 
                               "sigma (non-linear and interaction effects)"),
                    values = c("blue", "red", "orange")) + 
  #geom_text(aes(label = value)) + 
  labs(title = "Morris Elementary Effects, RHP Uniform, prop attack") + 
  theme(legend.position="bottom", axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))

ggplot(analysisrhpuni$`dir-cons-index-wins_mean`, aes(x=reorder(parameter, -value), y=value, fill = index)) +
  geom_bar(stat='identity', position='dodge') + 
  scale_fill_manual(labels = c("mu (overall effect size)", 
                               "mu-star (absolute value effect size)", 
                               "sigma (non-linear and interaction effects)"),
                    values = c("blue", "red", "orange")) + 
  #geom_text(aes(label = value)) + 
  labs(title = "Morris Elementary Effects, RHP Uniform, DCI wins") + 
  theme(legend.position="bottom", axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))


ggplot(analysisrhpuni$`dir-cons-index-attacks_mean`, aes(x=reorder(parameter, -value), y=value, fill = index)) +
  geom_bar(stat='identity', position='dodge') + 
  scale_fill_manual(labels = c("mu (overall effect size)", 
                               "mu-star (absolute value effect size)", 
                               "sigma (non-linear and interaction effects)"),
                    values = c("blue", "red", "orange")) + 
  #geom_text(aes(label = value)) + 
  labs(title = "Morris Elementary Effects, RHP Uniform, DCI attacks") + 
  theme(legend.position="bottom", axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))

ggplot(analysisrhpuni$`dir-cons-index-avoids_mean`, aes(x=reorder(parameter, -value), y=value, fill = index)) +
  geom_bar(stat='identity', position='dodge') + 
  scale_fill_manual(labels = c("mu (overall effect size)", 
                               "mu-star (absolute value effect size)", 
                               "sigma (non-linear and interaction effects)"),
                    values = c("blue", "red", "orange")) + 
  #geom_text(aes(label = value)) + 
  labs(title = "Morris Elementary Effects, RHP Uniform, DCI avoids") + 
  theme(legend.position="bottom", axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))

ggplot(analysisrhpuni$`foraging-efficiency-time_mean`, aes(x=reorder(parameter, -value), y=value, fill = index)) +
  geom_bar(stat='identity', position='dodge') + 
  scale_fill_manual(labels = c("mu (overall effect size)", 
                               "mu-star (absolute value effect size)", 
                               "sigma (non-linear and interaction effects)"),
                    values = c("blue", "red", "orange")) + 
  #geom_text(aes(label = value)) + 
  labs(title = "Morris Elementary Effects, RHP Uniform, foraging") + 
  theme(legend.position="bottom", axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))

dev.off()
#####end