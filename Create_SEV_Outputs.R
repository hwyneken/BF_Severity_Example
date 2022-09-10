require(ggplot2)
require(ggthemes)
require(scales)
require(gt)

HVagueSim <- readRDS(file = "simulation_output/HVagueSim.RDS")
HSpecificSim <- readRDS(file = "simulation_output/HSpecificSim.RDS")
HEncompSim <- readRDS(file = "simulation_output/HEncompSim.RDS")

## use the log BF in the histogram

## two plots: one for encomopassing
# encompassing plot shows that we 
# cannot claim that constucted H(s) and H(v)
# pass with severity against H(e)

# is 2 by 2

# rows: H(v) and H(s)
# columns predesignated or data-driven

NSim <- length(HEncompSim$BF_HVague_Sampling)

plotDF1 <- data.frame(BF = c(HEncompSim$BF_HVague_Sampling,
                             HEncompSim$BF_HSpecific_Sampling,
                             HEncompSim$BF_HConstruct_Sampling95,
                             HEncompSim$BF_HConstruct20_Sampling),
                      Claim = rep(rep(c("Claim: H(v)","Claim: H(s)"),each=NSim),times=2),
                      Scenario = rep(c("Predesignated","Data Driven: Confidence Interval"),each=2*NSim))
plotDF1$logBF <- log(plotDF1$BF)
plotDF1$Claim <- factor(plotDF1$Claim,levels = c("Claim: H(v)","Claim: H(s)"))
plotDF1$Scenario <- factor(plotDF1$Scenario,
                           levels = c("Predesignated","Data Driven: Confidence Interval"))

plot1 <- ggplot(data = plotDF1,aes(x=logBF)) + geom_histogram(bins=20)
plot1 <- plot1 + facet_wrap(Claim ~ Scenario,scales="free_x")

plot1 <- plot1 + theme_tufte()
plot1 <- plot1 + theme(axis.text = element_text(size=12),
                       axis.title.x = element_text(size=14),
                       axis.title.y = element_blank(),
                       plot.title = element_text(size=16,hjust=0.5),
                       plot.subtitle = element_text(size=14,hjust=0.5),
                       plot.caption = element_text(size=12),
                       panel.background = element_rect(fill = "#f8f8f8"),
                       strip.background = element_rect(fill = "gray"))

sev1DF <- data.frame(SEV = c(mean(HEncompSim$BF_HVague_Sampling <= HEncompSim$Obs_BF_HV),
                             mean(HEncompSim$BF_HSpecific_Sampling <= HEncompSim$Obs_BF_HS),
                             mean(HEncompSim$BF_HConstruct_Sampling95 <= HEncompSim$Obs_BF_HV),
                             mean(HEncompSim$BF_HConstruct20_Sampling <= HEncompSim$Obs_BF_HS)),
                     Claim = rep(c("Claim: H(v)","Claim: H(s)"),times=2),
                     Scenario = rep(c("Predesignated","Data Driven: Confidence Interval"),each=2),
                     x = c(-22.5,-40,2.75,4.75),
                     y = c(40,40,40,40))
sev1DF$Claim <- factor(sev1DF$Claim,levels=levels(plotDF1$Claim))
sev1DF$Scenario <- factor(sev1DF$Scenario,levels=levels(plotDF1$Scenario))
sev1DF$SevLabel <- paste0("SEV = ",sev1DF$SEV)

plot1 <- plot1 + geom_label(data = sev1DF,
                            aes(x=x,y=y,label=SevLabel),inherit.aes=FALSE)

plot1CaptionString <- "H(v) and H(s) compared to H(e): theta(A) in [0,1], theta(B) in [0,1], BF(H(v),H(e)) = 5.9, BF(H(s),H(e)) = 16.5"
plot1CaptionString <- paste0(plot1CaptionString,"\n",
                             "SEV is calculated as the proportion of log BF values from the sampling distribution that are less than the observed log BF (dashed vertical line).")
plot1CaptionString <- paste0(plot1CaptionString,"\n",
                             "H(v) and H(s) were compared to the BF of the 95% / 20% CIs for theta(A) and theta(B) against H(e)")
plot1 <- plot1 + labs(title = "Severity Analysis for H(v) and H(s) against H(e)",
                      subtitle = "H(s) Has a Stronger Fit Against H(e)",
                      caption = plot1CaptionString,
                      x = "Sampling Distribution of the Log Bayes Factor Under H(e)")

plot1CutoffDF <- data.frame(Claim = c("Claim: H(v)","Claim: H(s)"),
                            xintercept = log(c(5.915,16.53)))
plot1CutoffDF$Claim <- factor(plot1CutoffDF$Claim,levels=levels(plotDF1$Claim))

plot1 <- plot1 + geom_vline(data = plot1CutoffDF,
                            aes(xintercept = xintercept),
                            col = "red",lty=2)
ggsave(plot1,file = "images/plot1.png",units="in",width=12,height=8)

## one for specific alternatives

plotDF2 <- data.frame(BF = c(HVagueSim$BF_HVague_Sampling / HVagueSim$BF_HError_Sampling,
                             HSpecificSim$BF_HSpecific_Sampling / HSpecificSim$BF_HError_Sampling,
                             HVagueSim$BF_HConstruct_Sampling / HVagueSim$BF_HError_Sampling,
                             HSpecificSim$BF_HConstruct_Sampling / HSpecificSim$BF_HError_Sampling),
                      Claim = rep(rep(c("Claim: H(v)","Claim: H(s)"),each=NSim),times=2),
                      Scenario = c(rep("Predesignated",2*NSim),
                                   rep("Data Driven: Confidence Interval",NSim),
                                   rep("Data Driven: Confidence Interval",NSim)))

plotDF2$logBF <- log(plotDF2$BF)
plotDF2$Claim <- factor(plotDF2$Claim,levels = c("Claim: H(v)","Claim: H(s)"))
plotDF2$Scenario <- factor(plotDF2$Scenario,
                           levels = c("Predesignated","Data Driven: Confidence Interval"))

plot2 <- ggplot(data = plotDF2,aes(x=logBF)) + geom_histogram(bins=20)
plot2 <- plot2 + facet_wrap(Claim ~ Scenario,scales="free_x")

sev2DF <- data.frame(SEV = c(mean(HVagueSim$BF_HVague_Sampling / HVagueSim$BF_HError_Sampling <= HVagueSim$Obs_BF_Test),
                             mean(HSpecificSim$BF_HSpecific_Sampling / HSpecificSim$BF_HError_Sampling <= HSpecificSim$Obs_BF_Test),
                             mean(HVagueSim$BF_HConstruct_Sampling / HVagueSim$BF_HError_Sampling <= HVagueSim$Obs_BF_Test),
                             mean(HSpecificSim$BF_HConstruct_Sampling / HSpecificSim$BF_HError_Sampling <= HSpecificSim$Obs_BF_Test)),
                     Claim = rep(c("Claim: H(v)","Claim: H(s)"),times=2),
                     Scenario = rep(c("Predesignated","Data Driven: Confidence Interval"),each=2),
                     x = c(-10,-10,4,6),
                     y = c(50,50,50,50))

sev2DF$Claim <- factor(sev2DF$Claim,levels=levels(plotDF2$Claim))
sev2DF$Scenario <- factor(sev2DF$Scenario,levels=levels(plotDF2$Scenario))
sev2DF$SevLabel <- paste0("SEV = ",sev2DF$SEV)

plot2 <- plot2 + geom_label(data = sev2DF,
                            aes(x=x,y=y,label=SevLabel),inherit.aes=FALSE)
plot2 <- plot2 + theme_tufte()
plot2 <- plot2 + theme(axis.text = element_text(size=12),
                       axis.title.x = element_text(size=14),
                       axis.title.y = element_blank(),
                       plot.title = element_text(size=16,hjust=0.5),
                       plot.subtitle = element_text(size=14,hjust=0.5),
                       plot.caption = element_text(size=12),
                       panel.background = element_rect(fill = "#f8f8f8"),
                       strip.background = element_rect(fill = "gray"))

plot2CaptionString <- "H(v) compared to Alternative: theta(A) in [0.25,0.85], theta(B) in [0.15,0.75], BF = 6.5."
plot2CaptionString <- paste0(plot2CaptionString," ",
                             "H(s) compared to Alternative: theta(A) in [0.25,0.6], theta(B) in [0.4,0.75], BF = 6.3.")
plot2CaptionString <- paste0(plot2CaptionString,"\n",
                             "SEV is calculated as the proportion of log BF values from the sampling distribution that are less than the observed log BF (dashed vertical line).")
plot2CaptionString <- paste0(plot2CaptionString,"\n",
                             "H(v) and H(s) were compared to the BF of the 95% / 20% CIs for theta(A) and theta(B) against their alternative hypotheses")
plot2 <- plot2 + labs(title = "Severity Analysis for H(v) and H(s) against Close Alternatives",
                      subtitle = "Both Hypotheses have a Similar Strength of Fit Against Alternatives",
                      caption = plot2CaptionString,
                      x = "Sampling Distribution of the Log Bayes Factor under the Alternative Hypothesis")

plot2CutoffDF <- data.frame(Claim = c("Claim: H(v)","Claim: H(s)"),
                            xintercept = log(c(6.5,6.3)))
plot2CutoffDF$Claim <- factor(plot2CutoffDF$Claim,levels=levels(plotDF2$Claim))

plot2 <- plot2 + geom_vline(data = plot2CutoffDF,
                            aes(xintercept = xintercept),
                            col = "red",lty=2)
ggsave(plot2,file = "images/plot2.png",units="in",width=12,height=8)