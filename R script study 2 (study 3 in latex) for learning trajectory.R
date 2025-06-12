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
datstudy2 <- read_csv("csv for 70:30 ratio prob.csv")
datstudy2 <- datstudy2[, c("Cues", "Outcomes", "Frequency")]
datstudy2$Cues <- interaction("**", datstudy2$Cues, sep = "_")
datstudy2
check(datstudy2, rm = FALSE)
datstudy2$ID <- 1:nrow(datstudy2)
datstudy2
trainforstudy2_4000trials <- createTrainingData(datstudy2, nruns = 20, random=TRUE)
wm_prob4000 <- RWlearning(trainforstudy2_4000trials)
getWM(wm_prob4000)
# plot learning trajectory for compound cues:
par(mfrow=c(1,2))
# get weights from all possible combinations to accusative outcome, given a constant cue:
A_Wb <- getWeightsByCue(wm_prob4000,"A", "Acc") + getWeightsByCue(wm_prob4000,"W", "Acc") + getWeightsByCue(wm_prob4000, cue = "**", "Acc")
A_Xb <- getWeightsByCue(wm_prob4000,"A", "Acc") + getWeightsByCue(wm_prob4000,"X", "Acc") + getWeightsByCue(wm_prob4000, cue = "**", "Acc")
B_Wb <- getWeightsByCue(wm_prob4000,"B", "Acc") + getWeightsByCue(wm_prob4000,"W", "Acc") + getWeightsByCue(wm_prob4000, cue = "**", "Acc")
B_Xb <- getWeightsByCue(wm_prob4000,"B", "Acc") + getWeightsByCue(wm_prob4000,"X", "Acc") + getWeightsByCue(wm_prob4000, cue = "**", "Acc")
B_Yb <- getWeightsByCue(wm_prob4000,"B", "Acc") + getWeightsByCue(wm_prob4000,"Y", "Acc") + getWeightsByCue(wm_prob4000, cue = "**", "Acc")
B_Zb <- getWeightsByCue(wm_prob4000,"B", "Acc") + getWeightsByCue(wm_prob4000,"Z", "Acc") + getWeightsByCue(wm_prob4000, cue = "**", "Acc")
A_Yb <- getWeightsByCue(wm_prob4000,"A", "Acc") + getWeightsByCue(wm_prob4000,"Y", "Acc") + getWeightsByCue(wm_prob4000, cue = "**", "Acc")
A_Zb <- getWeightsByCue(wm_prob4000,"A", "Acc") + getWeightsByCue(wm_prob4000,"Z", "Acc") + getWeightsByCue(wm_prob4000, cue = "**", "Acc")
constantAccprob4000 <- getWeightsByCue(wm_prob4000, cue = "**", "Acc")
# plot:
plot(A_Wb[,1], col="darkorchid4", type = "l", ylim =c(-.5, 1), xlim = c(0, 4000), xaxt = "n", xlab = "Training trials", ylab="Learned weight")
mtext("a) compound cues to 'Acc'", side = 3, adj=0, line=1.2, cex = 1.2, font=2)
legend(2000, -0.05, legend = c("HF cue set 1 + HF cue set 2", "HF cue set 1 + LF cue set 2", "LF cue set 1 + HF cue set 2", "LF cue set 1 + LF cue set 2"),
       col=c("darkorchid4", "darkblue", "violet", "lightblue"), lty=1, bg="white", cex = 0.9, xjust = 0.5)
lines(A_Xb[,1], col="violet", lty=1)
lines(B_Wb[,1], col="darkblue", lty=1)
lines(B_Xb[,1], col="lightblue", lty=1)
lines(B_Yb[,1], col="darkblue", lty = 2)
lines(B_Zb[,1], col="lightblue", lty=2)
lines(A_Yb[,1], col="darkorchid4", lty=2)
lines(A_Zb[,1], col="violet", lty=2)
lines(constantAccprob4000[,1], col="red", lty=3) # ignore if don't want to add constant
abline(h=0)
abline(v=400)
axis(1, at = seq(0,4000, by =500))
legend(2000, -0.05, legend = c("HF cue set 1 + HF cue set 2", "HF cue set 1 + LF cue set 2", "LF cue set 1 + HF cue set 2", "LF cue set 1 + LF cue set 2", "constant cue"),
       col=c("darkorchid4", "darkblue", "violet", "lightblue", "red"), lty=1, bg="white", cex = 0.9, xjust = 0.5)

# second plot for compound cues:
# get weights from all possible combinations to prepositional outcome, given a constant cue:
A_W1b <- getWeightsByCue(wm_prob4000,"A", "Prep") + getWeightsByCue(wm_prob4000,"W", "Prep") + getWeightsByCue(wm_prob4000, cue = "**", "Prep")
A_X1b <- getWeightsByCue(wm_prob4000,"A", "Prep") + getWeightsByCue(wm_prob4000,"X", "Prep") + getWeightsByCue(wm_prob4000, cue = "**", "Prep")
B_W1b <- getWeightsByCue(wm_prob4000,"B", "Prep") + getWeightsByCue(wm_prob4000,"W", "Prep") + getWeightsByCue(wm_prob4000, cue = "**", "Prep")
B_X1b <- getWeightsByCue(wm_prob4000,"B", "Prep") + getWeightsByCue(wm_prob4000,"X", "Prep") + getWeightsByCue(wm_prob4000, cue = "**", "Prep")
B_Y1b <- getWeightsByCue(wm_prob4000,"B", "Prep") + getWeightsByCue(wm_prob4000,"Y", "Prep") + getWeightsByCue(wm_prob4000, cue = "**", "Prep")
B_Z1b <- getWeightsByCue(wm_prob4000,"B", "Prep") + getWeightsByCue(wm_prob4000,"Z", "Prep") + getWeightsByCue(wm_prob4000, cue = "**", "Prep")
A_Y1b <- getWeightsByCue(wm_prob4000,"A", "Prep") + getWeightsByCue(wm_prob4000,"Y", "Prep") + getWeightsByCue(wm_prob4000, cue = "**", "Prep")
A_Z1b <- getWeightsByCue(wm_prob4000,"A", "Prep") + getWeightsByCue(wm_prob4000,"Z", "Prep") + getWeightsByCue(wm_prob4000, cue = "**", "Prep")
constantPrepprob4000 <- getWeightsByCue(wm_prob4000, cue = "**", "Prep")

#plot:
plot(B_Y1b[,1],col="darkblue", type  ="l", ylim=c(-0.5, 1),xlim = c(0, 4000), xaxt = "n", xlab = "Training trials", ylab="Learned weight")
mtext("b) compound cues to 'Prep'", side = 3, adj=0, line=1.2, cex = 1.2, font=2)
lines(A_X1b[,1], col="violet", lty=2)
lines(B_W1b[,1], col="darkblue", lty=2)
lines(B_X1b[,1], col="lightblue", lty=2)
lines(A_W1b[,1], col="darkorchid4", lty = 2)
lines(B_Z1b[,1], col="lightblue", lty=1)
lines(A_Y1b[,1], col="darkorchid4", lty=1)
lines(A_Z1b[,1], col="violet", lty=1)
lines(constantPrepprob4000[,1], col="red", lty=3) # ignore if don't want to add constant
abline(h=0)
abline(v=400)
axis(1, at = seq(0,4000, by =500))
legend(2000, -0.05, legend = c("HF cue set 1 + HF cue set 2", "HF cue set 1 + LF cue set 2", "LF cue set 1 + HF cue set 2", "LF cue set 1 + LF cue set 2", "constant cue"),
       col=c("darkblue", "darkorchid4", "lightblue", "violet", "red"), lty=1, bg="white", cex = 0.9, xjust= 0.5)

# choice probabilities for study 2:
# for HF cue set 1 + HF cue set 2 for Acc:
# AW:
options.activations1 <- c(unlist(getActivations(getWM(wm_prob4000), cueset = "A_W_**", select.outcomes = "Acc")),
                          unlist(getActivations(getWM(wm_prob4000), cueset = "A_W_**", select.outcomes = "Prep")))
options.choiceAlternatives <- c("Acc", "Prep")
options1 <- data.frame(activations = options.activations1,
                       choiceAlternatives = options.choiceAlternatives,
                       cues = "A_W_**",
                       choiceProbability = 0,
                       stringsAsFactors = F)
for (r in 1:nrow(options1)){
  options1$choiceProbability[r] <- luceChoice(relu(options1$activations[r]), 
                                              relu(options1$activations))
}
options1
# for HF cue set 1 + HF cue set 2 for Prep:
# BY
options.activations2 <- c(unlist(getActivations(getWM(wm_prob4000), cueset = "B_Y_**", select.outcomes = "Acc")),
                          unlist(getActivations(getWM(wm_prob4000), cueset = "B_Y_**", select.outcomes = "Prep")))
options.choiceAlternatives <- c("Acc", "Prep")
options2 <- data.frame(activations = options.activations2,
                       choiceAlternatives = options.choiceAlternatives,
                       cues = "B_Y_**",
                       choiceProbability = 0,
                       stringsAsFactors = F)
for (r in 1:nrow(options2)){
  options2$choiceProbability[r] <- luceChoice(relu(options2$activations[r]), 
                                              relu(options2$activations))
}
options2
# for HF cue set 1 + LF cue set 2 for Acc:
# BW:
options.activations3 <- c(unlist(getActivations(getWM(wm_prob4000), cueset = "B_W_**", select.outcomes = "Acc")),
                          unlist(getActivations(getWM(wm_prob4000), cueset = "B_W_**", select.outcomes = "Prep")))
options.choiceAlternatives <- c("Acc", "Prep")
options3 <- data.frame(activations = options.activations3,
                       choiceAlternatives = options.choiceAlternatives,
                       cues = "B_W_**",
                       choiceProbability = 0,
                       stringsAsFactors = F)
for (r in 1:nrow(options3)){
  options3$choiceProbability[r] <- luceChoice(relu(options3$activations[r]), 
                                              relu(options3$activations))
}
options3
# for HF cue set 1 + LF cue set 2 for Prep:
# AY:
options.activations4 <- c(unlist(getActivations(getWM(wm_prob4000), cueset = "A_Y_**", select.outcomes = "Acc")),
                          unlist(getActivations(getWM(wm_prob4000), cueset = "A_Y_**", select.outcomes = "Prep")))
options.choiceAlternatives <- c("Acc", "Prep")
options4 <- data.frame(activations = options.activations4,
                       choiceAlternatives = options.choiceAlternatives,
                       cues = "A_Y_**",
                       choiceProbability = 0,
                       stringsAsFactors = F)
for (r in 1:nrow(options4)){
  options4$choiceProbability[r] <- luceChoice(relu(options4$activations[r]), 
                                              relu(options4$activations))
}
options4
# for LF cue set 1 + HF cue set 2 for Acc:
# AX:
options.activations5 <- c(unlist(getActivations(getWM(wm_prob4000), cueset = "A_X_**", select.outcomes = "Acc")),
                          unlist(getActivations(getWM(wm_prob4000), cueset = "A_X_**", select.outcomes = "Prep")))
options.choiceAlternatives <- c("Acc", "Prep")
options5 <- data.frame(activations = options.activations5,
                       choiceAlternatives = options.choiceAlternatives,
                       cues = "A_X_**",
                       choiceProbability = 0,
                       stringsAsFactors = F)
for (r in 1:nrow(options5)){
  options5$choiceProbability[r] <- luceChoice(relu(options5$activations[r]), 
                                              relu(options5$activations))
}
options5
# for LF cue set 1 + HF cue set 2 for Prep:
# BZ:
options.activations6 <- c(unlist(getActivations(getWM(wm_prob4000), cueset = "B_Z_**", select.outcomes = "Acc")),
                          unlist(getActivations(getWM(wm_prob4000), cueset = "B_Z_**", select.outcomes = "Prep")))
options.choiceAlternatives <- c("Acc", "Prep")
options6 <- data.frame(activations = options.activations6,
                       choiceAlternatives = options.choiceAlternatives,
                       cues = "B_Z_**",
                       choiceProbability = 0,
                       stringsAsFactors = F)
for (r in 1:nrow(options6)){
  options6$choiceProbability[r] <- luceChoice(relu(options6$activations[r]), 
                                              relu(options6$activations))
}
options6
# for LF cue set 1 + LF cue set 2 for Acc:
# BX
options.activations7 <- c(unlist(getActivations(getWM(wm_prob4000), cueset = "B_X_**", select.outcomes = "Acc")),
                          unlist(getActivations(getWM(wm_prob4000), cueset = "B_X_**", select.outcomes = "Prep")))
options.choiceAlternatives <- c("Acc", "Prep")
options7 <- data.frame(activations = options.activations7,
                       choiceAlternatives = options.choiceAlternatives,
                       cues = "B_X_**",
                       choiceProbability = 0,
                       stringsAsFactors = F)
for (r in 1:nrow(options7)){
  options7$choiceProbability[r] <- luceChoice(relu(options7$activations[r]), 
                                              relu(options7$activations))
}
options7
# for LF cue set 1 + LF cue set 2 for Prep:
# AZ:
options.activations8 <- c(unlist(getActivations(getWM(wm_prob4000), cueset = "A_Z_**", select.outcomes = "Acc")),
                          unlist(getActivations(getWM(wm_prob4000), cueset = "A_Z_**", select.outcomes = "Prep")))
options.choiceAlternatives <- c("Acc", "Prep")
options8 <- data.frame(activations = options.activations8,
                       choiceAlternatives = options.choiceAlternatives,
                       cues = "A_Z_**",
                       choiceProbability = 0,
                       stringsAsFactors = F)
for (r in 1:nrow(options8)){
  options8$choiceProbability[r] <- luceChoice(relu(options8$activations[r]), 
                                              relu(options8$activations))
}
options8
# save correct choice probability for each:
# for HF cue set 1 + HF cue set 2
HFcueset1HFcueset2_acc_4000trials_study2 <- options1[options1$choiceAlternatives == "Acc", "choiceProbability"]
HFcueset1HFcueset2_prep_4000trials_study2 <- options2[options2$choiceAlternatives == "Prep", "choiceProbability"]
# for HF cue set 1 + LF cue set 2 :
HFcueset1LFcueset2_acc_4000trials_study2 <- options3[options3$choiceAlternatives == "Acc", "choiceProbability"]
HFcueset1LFcueset2_prep_4000trials_study2 <- options4[options4$choiceAlternatives == "Prep", "choiceProbability"]
# for LF cue set 1 + HF cue set 2:
LFcueset1HFcueset2_acc_4000trials_study2 <- options5[options5$choiceAlternatives == "Acc", "choiceProbability"]
LFcueset1HFcueset2_prep_4000trials_study2 <- options6[options6$choiceAlternatives == "Prep", "choiceProbability"]
# for LF cue set 1 + LF cue set 2:
LFcueset1LFcueset2_acc_4000trials_study2 <- options7[options7$choiceAlternatives == "Acc", "choiceProbability"]
LFcueset1LFcueset2_prep_4000trials_study2 <- options8[options8$choiceAlternatives == "Prep", "choiceProbability"]

# save the mean for different cue combinations, with outcome type combined
mean_HFcueset1HFcueset2_study2_4000trials <- mean(c(HFcueset1HFcueset2_acc_4000trials_study2, HFcueset1HFcueset2_prep_4000trials_study2))
mean_HFcueset1LFcueset2_study2_4000trials <- mean(c(HFcueset1LFcueset2_acc_4000trials_study2, HFcueset1LFcueset2_prep_4000trials_study2))
mean_LFcueset1HFcueset2_study2_4000trials <- mean(c(LFcueset1HFcueset2_acc_4000trials_study2, LFcueset1HFcueset2_prep_4000trials_study2))
mean_LFcueset1LFcueset2_study2_4000trials <- mean(c(LFcueset1LFcueset2_acc_4000trials_study2, LFcueset1LFcueset2_prep_4000trials_study2))
# data frame for barplot:
df4000trials_study2 <- data.frame(
  HFcueset1_HFcueset2 = mean_HFcueset1HFcueset2_study2_4000trials,
  HFcueset1_LFcueset2 = mean_HFcueset1LFcueset2_study2_4000trials,
  LFcueset1_HFcueset2 = mean_LFcueset1HFcueset2_study2_4000trials,
  LFcueset1_LFcueset2 = mean_LFcueset1LFcueset2_study2_4000trials)
df4000trials_study2
# Combine numeric vectors from the data frames
bar_matrix <- matrix(c(as.numeric(df4000trials_study2), as.numeric(df400trials_study2)), 
                     ncol = 4, byrow = TRUE)

# Create the barplot
bp2 <- barplot(bar_matrix, beside = TRUE,
               col = c("turquoise", "tomato"),
               ylim = c(0, 1),
               names.arg = rep("", 4),
               main = "Model Accuracy Post Training",
               ylab = "Choice Probability")
# labels: 
labels2 <- c("HF cue set 1 + HF cue set 2\n",
             "HF cue set 1 + LF cue set 2\n",
             "LF cue set 1 + HF cue set 2\n",
             "LF cue set 1 + LF cue set 2\n")
# add labels: 
text(x =colMeans(bp2), y = -0.1, labels = labels2, # use y to modify where the labels are in relation to x axis.
     #srt = 45,
     adj = 0.5, xpd = TRUE, cex = 0.9)
abline(h=0.5, lty = 2)
legend("bottomright", bg = "white", legend = c("After 4000 trials", "After 400 trials", "Baseline"),
       col = c("turquoise", "tomato", "black"), lty = c(0, 0, 2),
       pch=c(15, 15, NA),
       pt.cex = 2,
       cex = 0.8)

# second plot attempt:
# plot learning trajectory for compound cues:
par(mfrow=c(1,2))
# get weights from all possible combinations to accusative outcome, given a constant cue:
A_Wb <- getWeightsByCue(wm_prob4000,"A", "Acc") + getWeightsByCue(wm_prob4000,"W", "Acc") + getWeightsByCue(wm_prob4000, cue = "**", "Acc")
A_Xb <- getWeightsByCue(wm_prob4000,"A", "Acc") + getWeightsByCue(wm_prob4000,"X", "Acc") + getWeightsByCue(wm_prob4000, cue = "**", "Acc")
B_Wb <- getWeightsByCue(wm_prob4000,"B", "Acc") + getWeightsByCue(wm_prob4000,"W", "Acc") + getWeightsByCue(wm_prob4000, cue = "**", "Acc")
B_Xb <- getWeightsByCue(wm_prob4000,"B", "Acc") + getWeightsByCue(wm_prob4000,"X", "Acc") + getWeightsByCue(wm_prob4000, cue = "**", "Acc")
B_Yb <- getWeightsByCue(wm_prob4000,"B", "Acc") + getWeightsByCue(wm_prob4000,"Y", "Acc") + getWeightsByCue(wm_prob4000, cue = "**", "Acc")
B_Zb <- getWeightsByCue(wm_prob4000,"B", "Acc") + getWeightsByCue(wm_prob4000,"Z", "Acc") + getWeightsByCue(wm_prob4000, cue = "**", "Acc")
A_Yb <- getWeightsByCue(wm_prob4000,"A", "Acc") + getWeightsByCue(wm_prob4000,"Y", "Acc") + getWeightsByCue(wm_prob4000, cue = "**", "Acc")
A_Zb <- getWeightsByCue(wm_prob4000,"A", "Acc") + getWeightsByCue(wm_prob4000,"Z", "Acc") + getWeightsByCue(wm_prob4000, cue = "**", "Acc")
constantAccprob4000 <- getWeightsByCue(wm_prob4000, cue = "**", "Acc")
# plot:
plot(A_Wb[,1], col="#3F51B5", type = "l", ylim =c(-.5, 1), xlim = c(0, 4000), xaxt = "n", xlab = "Training trials", ylab="Learned weight")
mtext("a) compound cues to 'Acc'", side = 3, adj=0, line=1.2, cex = 1.2, font=2)
lines(A_Xb[,1], col="#808000", lty=1)
lines(B_Wb[,1], col="#B39DDB", lty=1)
lines(B_Xb[,1], col="#FFD700", lty=1)
lines(B_Yb[,1], col="#FF7F00", lty = 2)
lines(B_Zb[,1], col="#00CED1", lty=2)
lines(A_Yb[,1], col="#F781BF", lty=2)
lines(A_Zb[,1], col="#A65628", lty=2)
lines(constantAccprob4000[,1], col="red", lty=3) # ignore if don't want to add constant
abline(h=0)
abline(v=400)
axis(1, at = seq(0,4000, by =500))
legend(2000, -0.05, legend = c("HF cue set 1 + HF cue set 2", "HF cue set 1 + LF cue set 2", "LF cue set 1 + HF cue set 2", "LF cue set 1 + LF cue set 2", "constant cue"),
       col=c("#3F51B5", "#B39DDB", "#808000", "#FFD700", "red"), lty=1, bg="white", cex = 0.9, xjust = 0.5)

# second plot for compound cues:
# get weights from all possible combinations to prepositional outcome, given a constant cue:
A_W1b <- getWeightsByCue(wm_prob4000,"A", "Prep") + getWeightsByCue(wm_prob4000,"W", "Prep") + getWeightsByCue(wm_prob4000, cue = "**", "Prep")
A_X1b <- getWeightsByCue(wm_prob4000,"A", "Prep") + getWeightsByCue(wm_prob4000,"X", "Prep") + getWeightsByCue(wm_prob4000, cue = "**", "Prep")
B_W1b <- getWeightsByCue(wm_prob4000,"B", "Prep") + getWeightsByCue(wm_prob4000,"W", "Prep") + getWeightsByCue(wm_prob4000, cue = "**", "Prep")
B_X1b <- getWeightsByCue(wm_prob4000,"B", "Prep") + getWeightsByCue(wm_prob4000,"X", "Prep") + getWeightsByCue(wm_prob4000, cue = "**", "Prep")
B_Y1b <- getWeightsByCue(wm_prob4000,"B", "Prep") + getWeightsByCue(wm_prob4000,"Y", "Prep") + getWeightsByCue(wm_prob4000, cue = "**", "Prep")
B_Z1b <- getWeightsByCue(wm_prob4000,"B", "Prep") + getWeightsByCue(wm_prob4000,"Z", "Prep") + getWeightsByCue(wm_prob4000, cue = "**", "Prep")
A_Y1b <- getWeightsByCue(wm_prob4000,"A", "Prep") + getWeightsByCue(wm_prob4000,"Y", "Prep") + getWeightsByCue(wm_prob4000, cue = "**", "Prep")
A_Z1b <- getWeightsByCue(wm_prob4000,"A", "Prep") + getWeightsByCue(wm_prob4000,"Z", "Prep") + getWeightsByCue(wm_prob4000, cue = "**", "Prep")
constantPrepprob4000 <- getWeightsByCue(wm_prob4000, cue = "**", "Prep")

#plot:
plot(B_Y1b[,1],col="#FF7F00", type  ="l", ylim=c(-0.5, 1),xlim = c(0, 4000), xaxt = "n", xlab = "Training trials", ylab="Learned weight")
mtext("b) compound cues to 'Prep'", side = 3, adj=0, line=1.2, cex = 1.2, font=2)
lines(A_X1b[,1], col="#808000", lty=2)
lines(B_W1b[,1], col="#B39DDB", lty=2)
lines(B_X1b[,1], col="#FFD700", lty=2)
lines(A_W1b[,1], col="#3F51B5", lty = 2)
lines(B_Z1b[,1], col="#00CED1", lty=1)
lines(A_Y1b[,1], col="#F781BF", lty=1)
lines(A_Z1b[,1], col="#A65628", lty=1)
lines(constantPrepprob4000[,1], col="red", lty=3) # ignore if don't want to add constant
abline(h=0)
abline(v=400)
axis(1, at = seq(0,4000, by =500))
legend(2000, -0.05, legend = c("HF cue set 1 + HF cue set 2", "HF cue set 1 + LF cue set 2", "LF cue set 1 + HF cue set 2", "LF cue set 1 + LF cue set 2", "constant cue"),
       col=c("#FF7F00", "#F781BF", "#00CED1", "#A65628", "red"), lty=1, bg="white", cex = 0.9, xjust= 0.5)

