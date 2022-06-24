library(nlrx)
library(sensitivity)
netlogopath2 <- file.path("/Program Files/NetLogo 6.2.2")
modelpath2 <- file.path("C:\\Users\\Marcy\\Documents\\GitHub\\dominance\\sensitivity_analysis_dominance_despotism_energy.nlogo")
outpath2 <- file.path("/Users/Marcy/Desktop/")

nl <- nl(nlversion = "6.2.2",
         nlpath = netlogopath2,
         modelpath = modelpath2,
         jvmmem = 8000)

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

nl@experiment <- experiment(expname = "ch2amnatMEErhpcl",
                            outpath = outpath2,
                            repetition = 1,
                            tickmetrics = "false",
                            idsetup = "setup",
                            idgo = "go",
                            runtime = 10000,
                            stopcond = "(not any? patches with [penergy > 0])",
                            metrics = "foraging-efficiency-time",
                            variables = ch2vars,
                            constants = list("asymmetry" = "\"deterministic\"",
                                             "assessment-who" = "\"mutual\"",
                                             "assessment-info" = "\"knowledge\"",
                                             "resource-dist" = "\"clumped\"")
)           


nl@simdesign <- simdesign_morris(nl = nl,
                                 morristype = "oat",
                                 morrislevels = 5,
                                 morrisr = 1000,
                                 morrisgridjump = 5, 
                                 nseeds = 1)


progressr::handlers("progress")

library(future)
plan(multicore)

resultsMorrisch2 <- progressr::with_progress(run_nl_all(nl))

setsim(nl, "simoutput")<-resultsMorrisch2
analysis <- analyze_nl(nl)
