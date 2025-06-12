# R script study 1 (study 2 in latex)
# need to run simulation for 4000 trials + compare to 1000 random baseline simulations to assess significance of results
# run 1000 simulations for 400 trials model. To find the mean choice probability for each cue set after training
# compare choice probabilities for 4000 trials model and mean choice probabilities for 400 trials model on graph
# simulate learning trajectory for 4000 trials, with vline at 400 trials. 
# cue sets should be called: cue set 1 + HF cue set 2; cue set 1 + LF cue set 2
# firstly, create simulation for 4000 trials + model the learning trajectory.
# load necessary packages 
library(readr)
library(dplyr)
library(sigmoid)
library(edl)
library(magrittr)
# read csv:
datstudy1 <- read_csv("csv for 70:30 ratio det.csv")
datstudy1 <- datstudy1[, c("Cues", "Outcomes", "Frequency")]
datstudy1$Cues <- interaction("**", datstudy1$Cues, sep = "_")
datstudy1
check(datstudy1, rm = FALSE)
datstudy1$ID <- 1:nrow(datstudy1)
datstudy1
trainforstudy1_4000trials <- createTrainingData(datstudy1, nruns = 200, random=TRUE)
wm_det4000 <- RWlearning(trainforstudy1_4000trials)
getWM(wm_det4000)
# plot learning trajectory for compound cues:
par(mfrow=c(1,2))
# get weights from all possible combinations to accusative outcome, given a constant cue:
A_Wd3 <- getWeightsByCue(wm_det4000,"A", "Acc") + getWeightsByCue(wm_det4000,"W", "Acc") + getWeightsByCue(wm_det4000, cue = "**", "Acc")
B_Wd3 <- getWeightsByCue(wm_det4000,"B", "Acc") + getWeightsByCue(wm_det4000,"W", "Acc") + getWeightsByCue(wm_det4000, cue = "**", "Acc")
B_Yd3 <- getWeightsByCue(wm_det4000,"B", "Acc") + getWeightsByCue(wm_det4000,"Y", "Acc") + getWeightsByCue(wm_det4000, cue = "**", "Acc")
A_Yd3 <- getWeightsByCue(wm_det4000,"A", "Acc") + getWeightsByCue(wm_det4000,"Y", "Acc") + getWeightsByCue(wm_det4000, cue = "**", "Acc")
constantAcc4000 <- getWeightsByCue(wm_det4000, "**", "Acc")
# plot:
plot(A_Wd3[,1], col="#662C91", type = "l", ylim =c(-0.5, 1), xlim = c(0, 4000), xaxt = "n", xlab = "Training trials", ylab="Learned weight")
#mtext("a) compound cues to 'Acc'", side = 3, adj=0, line=1.2, cex = 1.2, font=2)
lines(B_Wd3[,1], col="#17A398", lty=1)
lines(B_Yd3[,1], col="#6B8F71", lty = 2)
lines(A_Yd3[,1], col="#F08CAE", lty=2)
lines(constantAcc4000[,1], col="red", lty=3) # ignore if don't want to add constant
abline(h=0)
abline(v=400)
axis(1, at = seq(0,4000, by =500))
legend(2000, -0.1, legend = c("cue set 1 + HF cue set 2", "cue set 1 + LF cue set 2", "constant cue"),
       col=c("#662C91", "#17A398", "red"), lty=1, bg="white", cex = 0.9, xjust = 0.5)

# second plot for compound cues:
A_Wd4 <- getWeightsByCue(wm_det4000,"A", "Prep") + getWeightsByCue(wm_det4000,"W", "Prep") + getWeightsByCue(wm_det4000, cue = "**", "Prep")
B_Wd4 <- getWeightsByCue(wm_det4000,"B", "Prep") + getWeightsByCue(wm_det4000,"W", "Prep") + getWeightsByCue(wm_det4000, cue = "**", "Prep")
B_Yd4 <- getWeightsByCue(wm_det4000,"B", "Prep") + getWeightsByCue(wm_det4000,"Y", "Prep") + getWeightsByCue(wm_det4000, cue = "**", "Prep")
A_Yd4 <- getWeightsByCue(wm_det4000,"A", "Prep") + getWeightsByCue(wm_det4000,"Y", "Prep") + getWeightsByCue(wm_det4000, cue = "**", "Prep")
constantPrep4000 <- getWeightsByCue(wm_det4000, "**", "Prep")

#plot:
plot(B_Yd4[,1],col="#6B8F71", type  ="l", ylim=c(-0.5, 1),xlim = c(0, 4000), xaxt = "n", xlab = "Training trials", ylab="Learned weight")
#mtext("b) compound cues to 'Prep'", side = 3, adj=0, line=1.2, cex = 1.2, font=2)
lines(B_Wd4[,1], col="#17A398", lty=2)
lines(A_Wd4[,1], col="#662C91", lty = 2)
lines(A_Yd4[,1], col="#F08CAE", lty=1)
lines(constantPrep4000[,1], col="red", lty=3) # ignore if don't want to add constant
abline(h=0)
abline(v=400)
axis(1, at = seq(0,4000, by =500))
legend(2000, -0.1, legend = c("cue set 1 + HF cue set 2", "cue set 1 + LF cue set 2", "constant cue"),
       col=c("#6B8F71", "#F08CAE", "red"), lty=1, bg="white", cex = 0.9, xjust = 0.5)

# choice probabilities for study 1:
# work out the choice probabilities for 4000 trials:
# for cue set 1 + HF cue set 2 for Acc:
options.activations1c <- c(unlist(getActivations(getWM(wm_det4000), cueset = "A_W_**", select.outcomes = "Acc")),
                           unlist(getActivations(getWM(wm_det4000), cueset = "A_W_**", select.outcomes = "Prep")))
options.choiceAlternatives <- c("Acc", "Prep")
options1c <- data.frame(activations = options.activations1c,
                        choiceAlternatives = options.choiceAlternatives,
                        cues = "A_W_**",
                        choiceProbability = 0,
                        stringsAsFactors = F)
for (r in 1:nrow(options1c)){
  options1c$choiceProbability[r] <- luceChoice(relu(options1c$activations[r]), 
                                               relu(options1c$activations))
}
options1c
options1c$choiceProbability
# for cue set 1 + HF cue set 2 for Prep:
options.activations2c <- c(unlist(getActivations(getWM(wm_det4000), cueset = "B_Y_**", select.outcomes = "Acc")),
                           unlist(getActivations(getWM(wm_det4000), cueset = "B_Y_**", select.outcomes = "Prep")))
options.choiceAlternatives <- c("Acc", "Prep")
options2c <- data.frame(activations = options.activations2c,
                        choiceAlternatives = options.choiceAlternatives,
                        cues = "B_Y_**",
                        choiceProbability = 0,
                        stringsAsFactors = F)
for (r in 1:nrow(options2c)){
  options2c$choiceProbability[r] <- luceChoice(relu(options2c$activations[r]), 
                                               relu(options2c$activations))
}
options2c
options2c$choiceProbability
# for cue set 1 + LF cue set 2 for Acc:
options.activations3c <- c(unlist(getActivations(getWM(wm_det4000), cueset = "B_W_**", select.outcomes = "Acc")),
                           unlist(getActivations(getWM(wm_det4000), cueset = "B_W_**", select.outcomes = "Prep")))
options.choiceAlternatives <- c("Acc", "Prep")
options3c <- data.frame(activations = options.activations3c,
                        choiceAlternatives = options.choiceAlternatives,
                        cues = "B_W_**",
                        choiceProbability = 0,
                        stringsAsFactors = F)
for (r in 1:nrow(options3c)){
  options3c$choiceProbability[r] <- luceChoice(relu(options3c$activations[r]), 
                                               relu(options3c$activations))
}
options3c
# for cue set 1 + LF cue set 2 for Prep:
options.activations4c <- c(unlist(getActivations(getWM(wm_det4000), cueset = "A_Y_**", select.outcomes = "Acc")),
                           unlist(getActivations(getWM(wm_det4000), cueset = "A_Y_**", select.outcomes = "Prep")))
options.choiceAlternatives <- c("Acc", "Prep")
options4c <- data.frame(activations = options.activations4c,
                        choiceAlternatives = options.choiceAlternatives,
                        cues = "A_Y_**",
                        choiceProbability = 0,
                        stringsAsFactors = F)
for (r in 1:nrow(options4c)){
  options4c$choiceProbability[r] <- luceChoice(relu(options4c$activations[r]), 
                                               relu(options4c$activations))
}
options4c
# save correct choice probability for each
cueset1HFcueset2_acc_4000trials_study1 <- options1c[options1c$choiceAlternatives == "Acc", "choiceProbability"]
cueset1HFcueset2_prep_4000trials_study1 <- options2c[options2c$choiceAlternatives == "Prep", "choiceProbability"]
cueset1LFcueset2_acc_4000trials_study1 <- options3c[options3c$choiceAlternatives == "Acc", "choiceProbability"]
cueset1LFcueset2_prep_4000trials_study1 <- options4c[options4c$choiceAlternatives == "Prep", "choiceProbability"]
# save the mean for different cue combinations, with outcome type combined
mean_cueset1HFcueset2_study1_4000trials <- mean(c(cueset1HFcueset2_acc_4000trials_study1, cueset1HFcueset2_prep_4000trials_study1))
mean_cueset1LFcueset2_study1_4000trials <- mean(c(cueset1LFcueset2_acc_4000trials_study1, cueset1LFcueset2_prep_4000trials_study1))
# data frame for barplot:
df4000trials_study1 <- data.frame(
  cueset1_HFcueset2 = mean_cueset1HFcueset2_study1_4000trials,
  cueset1_LFcueset2 = mean_cueset1LFcueset2_study1_4000trials)
df4000trials_study1
# Combine numeric vectors from the data frames
bar_matrix <- matrix(c(as.numeric(df4000trials_study1), as.numeric(df400trials_study1)), 
                     ncol = 2, byrow = TRUE)

# Create the barplot
bp2 <- barplot(bar_matrix, beside = TRUE,
               col = c("turquoise", "tomato"),
               ylim = c(0, 1),
               names.arg = rep("", 2),
               main = "Model Accuracy Post Training",
               ylab = "Choice Probability")
# labels: 
labels2 <- c("Cue set 1 + HF cue set 2\n",
             "Cue set 1 + LF cue set 2\n")
# add labels: 
text(x =colMeans(bp2), y = -0.1, labels = labels2, # use y to modify where the labels are in relation to x axis.
     #srt = 45,
     adj = 0.5, xpd = TRUE, cex = 0.9)
abline(h=0.5, lty = 2)
legend("bottomright", bg = "white", legend = c("After 4000 trials", "After 400 trials", "Baseline"),
       col = c("turquoise", "tomato", "black"), lty = c(0, 0, 2),
       pch=c(15, 15, NA),
       pt.cex = 2,
       cex = 0.9)
