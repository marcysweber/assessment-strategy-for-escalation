library(nlrx)
library(sensitivity)
netlogopath2 <- file.path("/Program Files/NetLogo 6.2.2")
modelpath2 <- file.path("C:\\Users\\Marcy\\Documents\\GitHub\\dominance\\sensitivity_analysis_dominance_despotism_energy.nlogo")
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
  "step-distance" = list(min = 0.5, max = 5, qfun = "qunif"),
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
                                         morrislevels = 5,
                                         morrisr = 100,
                                         morrisgridjump = 4, 
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
                            runtime = 10000,
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
                                 morrislevels = 13,
                                 morrisr = 1000,
                                 morrisgridjump = 7, 
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
                            runtime = 10000,
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
                                 morrislevels = 13,
                                 morrisr = 1000,
                                 morrisgridjump = 7, 
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
         jvmmem = 8000)

nlhistcl@experiment <- experiment(expname = "ch2amnatMEErhpcl",
                            outpath = outpath2,
                            repetition = 1,
                            tickmetrics = "false",
                            idsetup = "setup",
                            idgo = "go",
                            runtime = 10000,
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
                                 morrislevels = 13,
                                 morrisr = 1000,
                                 morrisgridjump = 7, 
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
         jvmmem = 8000)

nlhistuni@experiment <- experiment(expname = "ch2amnatMEErhpcl",
                            outpath = outpath2,
                            repetition = 1,
                            tickmetrics = "false",
                            idsetup = "setup",
                            idgo = "go",
                            runtime = 10000,
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
                                 morrislevels = 13,
                                 morrisr = 1000,
                                 morrisgridjump = 7, 
                                 nseeds = 1)


progressr::handlers("progress")

library(future)
plan(multisession)

resultsMorrisch2histuni <- progressr::with_progress(run_nl_all(nlhistuni))

setsim(nlhistuni, "simoutput")<-resultsMorrisch2histuni
analysishistuni <- analyze_nl(nlhistuni)