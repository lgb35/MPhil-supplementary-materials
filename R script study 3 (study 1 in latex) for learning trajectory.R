# R script study 3 (study 1 in latex)
# need to run simulation for 4000 trials + compare to 1000 random baseline simulations to assess significance of results
# run 1000 simulations for 400 trials model. To find the mean choice probability for each cue set after training
# compare choice probabilities for 4000 trials model and mean choice probabilities for 400 trials model on graph
# simulate learning trajectory for 4000 trials, with vline at 400 trials. 
# cue sets should be called: cue set 1, HF cue set 2, LF cue set 2
# firstly, create simulation for 4000 trials + model the learning trajectory.
# load necessary packages 
library(readr)
library(dplyr)
library(sigmoid)
library(edl)
library(magrittr)
# model for 4000 trials (to reach asymptote):
datforst3 <- read_csv("csv for study 3 4000 trials.csv") # has 20 trials in it.
datforst3 <- datforst3[, c("Cues", "Outcomes", "Frequency")]
datforst3$Cues <- interaction("**", datforst3$Cues, sep = "_")
datforst3
check(datforst3, rm = FALSE)
datforst3$ID <- 1:nrow(datforst3)
datforst3
trainforst34000 <- createTrainingData(datforst3, nruns = 200, random=TRUE)
wm_st34000 <- RWlearning(trainforst34000)
# plot for single cues + constant cues for 4000 trials, with constant cue:
par(mfrow=c(1,2))
# get weights from single cues + constant cue to 'Acc':
A <- getWeightsByCue(wm_st34000,"A", "Acc") + getWeightsByCue(wm_st34000, cue = "**", "Acc")
B <- getWeightsByCue(wm_st34000,"B", "Acc") + getWeightsByCue(wm_st34000, cue = "**", "Acc")
W <- getWeightsByCue(wm_st34000,"W", "Acc") + getWeightsByCue(wm_st34000, cue = "**", "Acc")
X <- getWeightsByCue(wm_st34000,"X", "Acc") + getWeightsByCue(wm_st34000, cue = "**", "Acc")
Y <- getWeightsByCue(wm_st34000,"Y", "Acc") + getWeightsByCue(wm_st34000, cue = "**", "Acc")
Z <- getWeightsByCue(wm_st34000,"Z", "Acc") + getWeightsByCue(wm_st34000, cue = "**", "Acc")
constantAcc40003a <- getWeightsByCue(wm_st34000, "**", "Acc")
# plot:
plot(A[,1], col="darkorchid", type = "l", ylim =c(-.5, 1), xlim = c(0, 4000), xaxt = "n", xlab = "training trials", ylab="learned weight")
mtext("a) single cues to 'Acc'", side = 3, adj=0, line=1.2, cex = 1.2, font=2)
legend(2000, -0.05, legend = c("cue set 1" , "HF cue set 2", "LF cue set 2", "constant cue"),
       col=c("darkorchid", "lightblue", "orange", "red"), lty=1, bg="white", cex = 1, xjust = 0.5)
lines(B[,1], col="darkorchid", lty=2)
lines(W[,1], col="lightblue", lty = 1)
lines(X[,1], col="orange", lty = 1)
lines(Y[,1], col="lightblue", lty = 2)
lines(Z[,1], col="orange", lty = 2)
lines(constantAcc40003a[,1], col="red", lty=3) # ignore if don't want to add constant
abline(h=0)
abline(v=400)
axis(1, at = seq(0,4000, by =500))

# second plot for single cues (for outcome 'Prep'):
Ab <- getWeightsByCue(wm_st34000,"A", "Prep") + getWeightsByCue(wm_st34000, cue = "**", "Prep")
Bb <- getWeightsByCue(wm_st34000,"B", "Prep") + getWeightsByCue(wm_st34000, cue = "**", "Prep")
Wb <- getWeightsByCue(wm_st34000,"W", "Prep") + getWeightsByCue(wm_st34000, cue = "**", "Prep")
Xb <- getWeightsByCue(wm_st34000,"X", "Prep") + getWeightsByCue(wm_st34000, cue = "**", "Prep")
Yb <- getWeightsByCue(wm_st34000,"Y", "Prep") + getWeightsByCue(wm_st34000, cue = "**", "Prep")
Zb <- getWeightsByCue(wm_st34000,"Z", "Prep") + getWeightsByCue(wm_st34000, cue = "**", "Prep")
constantPrep40003b <- getWeightsByCue(wm_st34000, "**", "Prep")

# plot:
plot(Bb[,1], col="darkorchid", type = "l", ylim =c(-.5, 1), xlim = c(0, 4000), xaxt = "n", xlab = "training trials", ylab="learned weight")
mtext("a) single cues to 'Prep'", side = 3, adj=0, line=1.2, cex = 1.2, font=2)
legend(2000, -0.05, legend = c("cue set 1" , "HF cue set 2", "LF cue set 2", "constant cue"),
       col=c("darkorchid", "lightblue", "orange", "red"), lty=1, bg="white", cex = 1, xjust = 0.5)
lines(Ab[,1], col="darkorchid", lty=2)
lines(Wb[,1], col="lightblue", lty = 2)
lines(Xb[,1], col="orange", lty = 2)
lines(Yb[,1], col="lightblue", lty = 1)
lines(Zb[,1], col="orange", lty = 1)
lines(constantPrep40003b[,1], col="red", lty=3) # ignore if don't want to add constant
abline(h=0)
abline(v=400)
axis(1, at = seq(0,4000, by =500))

# plot learning trajectory for compound cues + constant cue, i.e. as in training:
# plot learned weights:
# for compound cues + constant cues: 
# get weights from all possible combinations to accusative outcome, given a constant cue:
AWs3a <- getWeightsByCue(wm_st34000,"A", "Acc") + getWeightsByCue(wm_st34000, cue = "W", "Acc") + getWeightsByCue(wm_st34000, cue = "**", "Acc")
AXs3a <- getWeightsByCue(wm_st34000,"A", "Acc") + getWeightsByCue(wm_st34000, cue = "X", "Acc") + getWeightsByCue(wm_st34000, cue = "**", "Acc")
BYs3a <- getWeightsByCue(wm_st34000,"B", "Acc") + getWeightsByCue(wm_st34000, cue = "Y", "Acc") + getWeightsByCue(wm_st34000, cue = "**", "Acc")
BZs3a <- getWeightsByCue(wm_st34000,"B", "Acc") + getWeightsByCue(wm_st34000, cue = "Z", "Acc") + getWeightsByCue(wm_st34000, cue = "**", "Acc")
constantAcc40003a <- getWeightsByCue(wm_st34000, "**", "Acc")
# plot:
par(mfrow=c(1,2))
plot(AWs3a[,1], col="darkorchid", type = "l", ylim =c(-.5, 1), xlim = c(0, 4000), xaxt = "n", xlab = "training trials", ylab="learned weight")
mtext("a) compound cues to 'Acc'", side = 3, adj=0, line=1.2, cex = 1.2, font=2)
legend(2000, -0.05, legend = c("cue set 1 + HF cue set 2" , "cue set 1 + LF cue set 2", "constant cue"),
       col=c("darkorchid", "lightblue", "red"), lty=1, bg="white", cex = 1, xjust = 0.5)
lines(BYs3a[,1], col="darkorchid", lty=2)
lines(AXs3a[,1], col="lightblue", lty = 1)
lines(BZs3a[,1], col="lightblue", lty = 2)
lines(constantAcc40003a[,1], col="red", lty=3) # ignore if don't want to add constant
abline(h=0)
abline(v=400)
axis(1, at = seq(0,4000, by =500))

# second plot for compound cues:
AWs3b <- getWeightsByCue(wm_st34000,"A", "Prep") + getWeightsByCue(wm_st34000, cue = "W", "Prep") + getWeightsByCue(wm_st34000, cue = "**", "Prep")
AXs3b <- getWeightsByCue(wm_st34000,"A", "Prep") + getWeightsByCue(wm_st34000, cue = "X", "Prep") + getWeightsByCue(wm_st34000, cue = "**", "Prep")
BYs3b <- getWeightsByCue(wm_st34000,"B", "Prep") + getWeightsByCue(wm_st34000, cue = "Y", "Prep") + getWeightsByCue(wm_st34000, cue = "**", "Prep")
BZs3b <- getWeightsByCue(wm_st34000,"B", "Prep") + getWeightsByCue(wm_st34000, cue = "Z", "Prep") + getWeightsByCue(wm_st34000, cue = "**", "Prep")
constantPrep40003b <- getWeightsByCue(wm_st34000, "**", "Prep")

# plot:
plot(BYs3b[,1], col="darkorchid", type = "l", ylim =c(-.5, 1), xlim = c(0, 4000), xaxt = "n", xlab = "training trials", ylab="learned weight")
mtext("a) compound cues to 'Prep'", side = 3, adj=0, line=1.2, cex = 1.2, font=2)
legend(2000, -0.05, legend = c("cue set 1 + HF cue set 2" , "cue set 1 + LF cue set 2", "constant cue"),
       col=c("darkorchid", "lightblue", "red"), lty=1, bg="white", cex = 1, xjust = 0.5)
lines(AWs3b[,1], col="darkorchid", lty=2)
lines(AXs3b[,1], col="lightblue", lty = 2)
lines(BZs3b[,1], col="lightblue", lty = 1)
lines(constantPrep40003b[,1], col="red", lty=3) # ignore if don't want to add constant
abline(h=0)
abline(v=400)
axis(1, at = seq(0,4000, by =500))

# now calculate the choice probabilities for the actual simulation by end of training (i.e. activations after 4000 trials):
# choice probability for the outcomes given cue A + constant cue:
# i.e. CUE SET 1 for Acc
options.activations1i <- c(unlist(getActivations(getWM(wm_st34000), cueset = "A_**", select.outcomes = "Acc")),
                           unlist(getActivations(getWM(wm_st34000), cueset = "A_**", select.outcomes = "Prep")))
options.choiceAlternatives <- c("Acc", "Prep")
options1i <- data.frame(activations = options.activations1i,
                        choiceAlternatives = options.choiceAlternatives,
                        cues = "A_**",
                        choiceProbability = 0,
                        stringsAsFactors = F)
for (r in 1:nrow(options1i)){
  options1i$choiceProbability[r] <- luceChoice(relu(options1i$activations[r]), 
                                               relu(options1i$activations))
}
options1i
# Acc = 0.89 choice probability; Prep = 0.11

# choice probability for the outcomes given cue B  + constant cue:
# i.e. CUE SET 1 for Prep
options.activations2i <- c(unlist(getActivations(getWM(wm_st34000), cueset = "B_**", select.outcomes = "Acc")),
                           unlist(getActivations(getWM(wm_st34000), cueset = "B_**", select.outcomes = "Prep")))
options.choiceAlternatives <- c("Acc", "Prep")
options2i <- data.frame(activations = options.activations2i,
                        choiceAlternatives = options.choiceAlternatives,
                        cues = "B_**",
                        choiceProbability = 0,
                        stringsAsFactors = F)
for (r in 1:nrow(options2i)){
  options2i$choiceProbability[r] <- luceChoice(relu(options2i$activations[r]), 
                                               relu(options2i$activations))
}

options2i
# Acc= 0.11 choice probability; Prep = 0.89
# choice probability for W (HF cue set 2 for Acc) + constant cue:
options.activations3i <- c(unlist(getActivations(getWM(wm_st34000), cueset = "W_**", select.outcomes = "Acc")),
                           unlist(getActivations(getWM(wm_st34000), cueset = "W_**", select.outcomes = "Prep")))
options.choiceAlternatives <- c("Acc", "Prep")
options3i <- data.frame(activations = options.activations3i,
                        choiceAlternatives = options.choiceAlternatives,
                        cues = "W_**",
                        choiceProbability = 0,
                        stringsAsFactors = F)
for (r in 1:nrow(options3i)){
  options3i$choiceProbability[r] <- luceChoice(relu(options3i$activations[r]), 
                                               relu(options3i$activations))
}
options3i
# Acc: 0.73; Prep: 0.27
# choice probability for X (LF cue set 2 for Acc) + constant cue:
options.activations4i <- c(unlist(getActivations(getWM(wm_st34000), cueset = "X_**", select.outcomes = "Acc")),
                           unlist(getActivations(getWM(wm_st34000), cueset = "X_**", select.outcomes = "Prep")))
options.choiceAlternatives <- c("Acc", "Prep")
options4i <- data.frame(activations = options.activations4i,
                        choiceAlternatives = options.choiceAlternatives,
                        cues = "X_**",
                        choiceProbability = 0,
                        stringsAsFactors = F)
for (r in 1:nrow(options4i)){
  options4i$choiceProbability[r] <- luceChoice(relu(options4i$activations[r]), 
                                               relu(options4i$activations))
}
options4i
# Acc: 0.73; Prep: 0.27
# i.e. no type-frequency effect once the model has reached asymptote. 
# choice probability for Y (HF cue set 2 for Prep) + constant cue:
options.activations5i <- c(unlist(getActivations(getWM(wm_st34000), cueset = "Y_**", select.outcomes = "Acc")),
                           unlist(getActivations(getWM(wm_st34000), cueset = "Y_**", select.outcomes = "Prep")))
options.choiceAlternatives <- c("Acc", "Prep")
options5i <- data.frame(activations = options.activations5i,
                        choiceAlternatives = options.choiceAlternatives,
                        cues = "Y_**",
                        choiceProbability = 0,
                        stringsAsFactors = F)
for (r in 1:nrow(options5i)){
  options5i$choiceProbability[r] <- luceChoice(relu(options5i$activations[r]), 
                                               relu(options5i$activations))
}
options5i
# Acc: 0.26; Prep: 0.73.
# choice probability for Z (LF cue set 2 for Prep) + constant cue:
options.activations6i <- c(unlist(getActivations(getWM(wm_st34000), cueset = "Z_**", select.outcomes = "Acc")),
                           unlist(getActivations(getWM(wm_st34000), cueset = "Z_**", select.outcomes = "Prep")))
options.choiceAlternatives <- c("Acc", "Prep")
options6i <- data.frame(activations = options.activations6i,
                        choiceAlternatives = options.choiceAlternatives,
                        cues = "Z_**",
                        choiceProbability = 0,
                        stringsAsFactors = F)
for (r in 1:nrow(options6i)){
  options6i$choiceProbability[r] <- luceChoice(relu(options6i$activations[r]), 
                                               relu(options6i$activations))
}
options6i
# Acc: 0.26; Prep: 0.73.
# so, can combine the results as per cue type, since outcome type makes no difference.
# since there is no difference, can just choose one cue set type for each outcome:
# choose options 1i for cue set 1; options3i for HF cue set 2; options 4i for LF cue set 2
# choice probability for cue set 1:
cueset1_choiceprob_4000trials <- options1i[options1i$choiceAlternatives == "Acc", "choiceProbability"]
cueset1_choiceprob_4000trials
# choice probability for HF cue set 2:
HFcueset2_choiceprob_4000trials <- options3i[options3i$choiceAlternatives == "Acc", "choiceProbability"]
HFcueset2_choiceprob_4000trials
# choice probability for LF cue set 2:
LFcueset2_choiceprob_4000trials <- options4i[options4i$choiceAlternatives == "Acc", "choiceProbability"]
LFcueset2_choiceprob_4000trials
# make barplot to compare the model's choice probabilities for 4000 trials vs. 400 trials. 
# data frame for 4000 trials
df4000trials <- data.frame(
  cueset1 = cueset1_choiceprob_4000trials,
  HFcueset2 = HFcueset2_choiceprob_4000trials,
  LFcueset2 = LFcueset2_choiceprob_4000trials
)
df4000trials

# data frame for 400 trials:
df400trials <- data.frame(
  cueset1 = mean_cueset1_400trials,
  HFcueset2 = mean_HFcueset2_400trials,
  LFcueset2 = mean_LFcueset2_400trials
)
df400trials
# plot this:
# Combine numeric vectors from the data frames
bar_matrix <- matrix(c(as.numeric(df4000trials), as.numeric(df400trials)), 
                     ncol = 3, byrow = TRUE)

# Create the barplot
bp2 <- barplot(bar_matrix, beside = TRUE,
               col = c("turquoise", "tomato"),
               ylim = c(0, 1),
               names.arg = rep("", 3),
               main = "Model Accuracy Post Training",
               ylab = "Choice Probability")
# labels: 
labels2 <- c("Cue set 1\n",
             "HF cue set 2\n",
             "LF cue set 2\n")
# add labels: 
text(x =colMeans(bp2), y = -0.1, labels = labels2, # use y to modify where the labels are in relation to x axis.
     #srt = 45,
     adj = 0.5, xpd = TRUE, cex = 0.9)
abline(h=0.5, lty = 2)
legend("topright", bg = "white", legend = c("After 4000 trials", "After 400 trials", "Baseline"),
       col = c("turquoise", "tomato", "black"), lty = c(0, 0, 2),
       pch=c(15, 15, NA),
       pt.cex = 2,
       cex = 0.9)
# choice probabilities for three cue sets for both outcome types to compare with random baseline simulation:
cueset1_choiceprob_4000trials_acc <- options1i[options1i$choiceAlternatives == "Acc", "choiceProbability"]
cueset1_choiceprob_4000trials_prep <- options2i[options2i$choiceAlternatives == "Prep", "choiceProbability"]
HFcueset2_choiceprob_4000trials_acc <- options3i[options3i$choiceAlternatives == "Acc", "choiceProbability"]
LFcueset2_choiceprob_4000trials_acc <- options4i[options4i$choiceAlternatives == "Acc", "choiceProbability"]
HFcueset2_choiceprob_4000trials_prep <- options5i[options5i$choiceAlternatives == "Prep", "choiceProbability"]
LFcueset2_choiceprob_4000trials_prep <- options6i[options6i$choiceAlternatives == "Prep", "choiceProbability"]
cueset1_choiceprob_4000trials_acc
cueset1_choiceprob_4000trials_prep
t.test(cueset1_choiceprob_4000trials_acc, cueset1_choiceprob_4000trials_prep, paired = TRUE)
