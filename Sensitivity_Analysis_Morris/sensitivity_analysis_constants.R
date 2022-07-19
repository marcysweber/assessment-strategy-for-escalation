library(nlrx)
library(sensitivity)
netlogopath2 <- file.path("/Program Files/NetLogo 6.2.2")
modelpath2 <- file.path("C:\\Users\\Marcy\\Documents\\GitHub\\dominance\\Sensitivity_Analysis_Morris\\sensitivity_analysis_dominance_despotism_energy.nlogo")
outpath2 <- file.path("/Users/Marcy/Desktop/")

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
  "change-in-dom-score" = list(min = 0.001, max = 0.1, qfun = "qunif"),
  "dom-score-decay-when-high" = list(min = 0.001, max = 0.1, qfun = "qunif")
)

###################################################
## practice

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



###########################################################################
##first SA experiment - rhp clumped

nlrhpcl <- nl(nlversion = "6.2.2",
         nlpath = netlogopath2,
         modelpath = modelpath2,
         jvmmem = 8000)

nlrhpcl@experiment <- experiment(expname = "ch2amnatMEErhpcl",
                            outpath = outpath2,
                            repetition = 1,
                            tickmetrics = "false",
                            idsetup = "setup",
                            idgo = "go",
                            runtime = 5000,
                            stopcond = "(not any? patches with [penergy > 0])",
                            metrics = c("foraging-efficiency-time", 
                                        "n-interactions", 
                                        "proportion-attacking",
                                        "dir-cons-index-wins",
                                        "dir-cons-index-attacks",
                                        "dir-cons-index-avoids"),
                            variables = ch2vars,
                            constants = list("asymmetry" = "\"deterministic\"",
                                             "assessment-who" = "\"mutual\"",
                                             "assessment-info" = "\"knowledge\"",
                                             "resource-dist" = "\"clumped\"")
)           


nlrhpcl@simdesign <- simdesign_morris(nl = nlrhpcl,
                                 morristype = "oat",
                                 morrislevels = 6,
                                 morrisr = 10,
                                 morrisgridjump = 3, 
                                 nseeds = 1)


progressr::handlers("progress")

library(future)
plan(multisession)

resultsMorrisch2rhpcl <- progressr::with_progress(run_nl_all(nlrhpcl))

setsim(nlrhpcl, "simoutput")<-resultsMorrisch2rhpcl
analysisrhpcl <- analyze_nl(nlrhpcl)




######################################################
##second set, rhp uniform

nlrhpuni <- nl(nlversion = "6.2.2",
         nlpath = netlogopath2,
         modelpath = modelpath2,
         jvmmem = 8000)

nlrhpuni@experiment <- experiment(expname = "ch2amnatMEErhpcl",
                            outpath = outpath2,
                            repetition = 1,
                            tickmetrics = "false",
                            idsetup = "setup",
                            idgo = "go",
                            runtime = 5000,
                            stopcond = "(not any? patches with [penergy > 0])",
                            metrics = c("foraging-efficiency-time", 
                                        "n-interactions", 
                                        "proportion-attacking",
                                        "dir-cons-index-wins",
                                        "dir-cons-index-attacks",
                                        "dir-cons-index-avoids"),
                            variables = ch2vars,
                            constants = list("asymmetry" = "\"deterministic\"",
                                             "assessment-who" = "\"mutual\"",
                                             "assessment-info" = "\"knowledge\"",
                                             "resource-dist" = "\"uniform\"")
)           


nlrhpuni@simdesign <- simdesign_morris(nl = nlrhpuni,
                                 morristype = "oat",
                                 morrislevels = 6,
                                 morrisr = 10,
                                 morrisgridjump = 3, 
                                 nseeds = 1)


progressr::handlers("progress")

library(future)
plan(multisession)

resultsMorrisch2rhpuni <- progressr::with_progress(run_nl_all(nlrhpuni))

setsim(nlrhpuni, "simoutput")<-resultsMorrisch2rhpuni
analysisrhpuni <- analyze_nl(nlrhpuni)

############################################################
## third set, history and clumped

nlhistcl <- nl(nlversion = "6.2.2",
         nlpath = netlogopath2,
         modelpath = modelpath2,
         jvmmem = 12000)

nlhistcl@experiment <- experiment(expname = "ch2amnatMEErhpcl",
                            outpath = outpath2,
                            repetition = 1,
                            tickmetrics = "false",
                            idsetup = "setup",
                            idgo = "go",
                            runtime = 5000,
                            stopcond = "(not any? patches with [penergy > 0])",
                            metrics = c("foraging-efficiency-time", 
                                        "n-interactions", 
                                        "proportion-attacking",
                                        "dir-cons-index-wins",
                                        "dir-cons-index-attacks",
                                        "dir-cons-index-avoids"),
                            variables = ch2vars,
                            constants = list("asymmetry" = "\"deterministic\"",
                                             "assessment-who" = "\"mutual\"",
                                             "assessment-info" = "\"history\"",
                                             "resource-dist" = "\"clumped\"")
)           


nlhistcl@simdesign <- simdesign_morris(nl = nlhistcl,
                                 morristype = "oat",
                                 morrislevels = 6,
                                 morrisr = 10,
                                 morrisgridjump = 3, 
                                 nseeds = 1)


progressr::handlers("progress")

library(future)
plan(multisession)

resultsMorrisch2histcl <- progressr::with_progress(run_nl_all(nlhistcl))

setsim(nlhistcl, "simoutput")<-resultsMorrisch2histcl
analysishistcl <- analyze_nl(nlhistcl)

###########################################################################
### final set, history and uniform


nlhistuni <- nl(nlversion = "6.2.2",
         nlpath = netlogopath2,
         modelpath = modelpath2,
         jvmmem = 12000)

nlhistuni@experiment <- experiment(expname = "ch2amnatMEErhpcl",
                            outpath = outpath2,
                            repetition = 1,
                            tickmetrics = "false",
                            idsetup = "setup",
                            idgo = "go",
                            runtime = 5000,
                            stopcond = "(not any? patches with [penergy > 0])",
                            metrics = c("foraging-efficiency-time", 
                                        "n-interactions", 
                                        "proportion-attacking",
                                        "dir-cons-index-wins",
                                        "dir-cons-index-attacks",
                                        "dir-cons-index-avoids"),
                            variables = ch2vars,
                            constants = list("asymmetry" = "\"deterministic\"",
                                             "assessment-who" = "\"mutual\"",
                                             "assessment-info" = "\"history\"",
                                             "resource-dist" = "\"uniform\"")
)           


nlhistuni@simdesign <- simdesign_morris(nl = nlhistuni,
                                 morristype = "oat",
                                 morrislevels = 6,
                                 morrisr = 10,
                                 morrisgridjump = 3, 
                                 nseeds = 1)


progressr::handlers("progress")

library(future)
plan(multisession)

resultsMorrisch2histuni <- progressr::with_progress(run_nl_all(nlhistuni))

setsim(nlhistuni, "simoutput")<-resultsMorrisch2histuni
analysishistuni <- analyze_nl(nlhistuni)






library(ggplot2)

analysishistcl <- split(analysishistcl, analysishistcl$metric)
analysishistuni <- split(analysishistuni, analysishistuni$metric)
analysisrhpcl <- split(analysisrhpcl, analysisrhpcl$metric)
analysisrhpuni <- split(analysisrhpuni, analysisrhpuni$metric)


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









library(reshape2)







musigma_processing <- function(data) {
  return(dcast(data, metric + parameter ~ index))
}

musigma_processing(analysishistcl$`dir-cons-index-attacks_mean`)

analysishistclmusig <- data.frame()

for (df in analysishistcl) {
  analysishistclmusig <- rbind(analysishistclmusig, (musigma_processing(as.data.frame(df))))
}

analysishistunimusig <- data.frame()

for (df in analysishistcl) {
  analysishistunimusig <- rbind(analysishistunimusig, (musigma_processing(as.data.frame(df))))
}

analysisrhpclmusig <- data.frame()

for (df in analysishistcl) {
  analysisrhpclmusig <- rbind(analysisrhpclmusig, (musigma_processing(as.data.frame(df))))
}

analysisrhpunimusig <- data.frame()

for (df in analysisrhpuni) {
  analysisrhpunimusig <- rbind(analysisrhpunimusig, (musigma_processing(as.data.frame(df))))
}


library(ggrepel)

pdf("Github/dominance/AmNatMEEscatterhistoryclv2.pdf")

metric.levels <- unique(analysishistclmusig$metric)

for (metric in metric.levels) {
print(ggplot(analysishistclmusig[analysishistclmusig$metric == metric,], aes(x = mustar, y = sigma, label = parameter)) +
  geom_point() + geom_label_repel(size = 3) +
  labs(title = paste0("Morris Elementary Effects, History Clumped ", metric)))
}
dev.off()



pdf("Github/dominance/AmNatMEEscatterhistoryuniv2.pdf")

metric.levels <- unique(analysishistunimusig$metric)

for (metric in metric.levels) {
  print(ggplot(analysishistunimusig[analysishistclmusig$metric == metric,], aes(x = mustar, y = sigma, label = parameter)) +
          geom_point() + geom_label_repel(size = 3) +
          labs(title = paste0("Morris Elementary Effects, History Uniform ", metric)))
}
dev.off()



pdf("Github/dominance/AmNatMEEscatterrhpclv2.pdf")

metric.levels <- unique(analysishistclmusig$metric)

for (metric in metric.levels) {
  print(ggplot(analysisrhpclmusig[analysisrhpclmusig$metric == metric,], aes(x = mustar, y = sigma, label = parameter)) +
          geom_point() + geom_label_repel(size = 3) +
          labs(title = paste0("Morris Elementary Effects, RHP Clumped ", metric)))
}
dev.off()



pdf("Github/dominance/AmNatMEEscatterrhpuniv2.pdf")

metric.levels <- unique(analysishistclmusig$metric)

for (metric in metric.levels) {
  print(ggplot(analysisrhpunimusig[analysisrhpunimusig$metric == metric,], aes(x = mustar, y = sigma, label = parameter)) +
          geom_point() + geom_label_repel(size = 3) +
          labs(title = paste0("Morris Elementary Effects, RHP Uniform ", metric)))
}
dev.off()




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
