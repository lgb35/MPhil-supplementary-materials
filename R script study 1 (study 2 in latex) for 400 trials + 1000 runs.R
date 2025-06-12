# R script study 1 (study 2 in latex) for 400 trials + 1000 runs to find choice probabilities at same stage as behavioural experiments
# assess whether there is a type-frequency effect
# load packages:
library(readr)
library(dplyr)
library(sigmoid)
library(edl)
library(magrittr)

resultstudy1.400trials.multipleruns <- data.frame(cueset1HFcueset2_acc_400trials_study1 = numeric(), 
                                                  cueset1HFcueset2_prep_400trials_study1 = numeric(), 
                                                  cueset1LFcueset2_acc_400trials_study1 = numeric(), 
                                                  cueset1LFcueset2_prep_400trials_study1 = numeric())

# create the 2 training data dfs:
# training data for tasks 1 and 2:
dattask1 <- read_csv("MPhil study 1 spreadsheet 1 lent.csv")
dattask1 <- dattask1[, c("Cues", "Outcomes", "Frequency")]
dattask1$Cues <- interaction("**", dattask1$Cues, sep = "_")
dattask1
check(dattask1, rm = FALSE)
dattask1$ID <- 1:nrow(dattask1)
dattask1
# training data for the learning blocks:
datblock1 <- read_csv("MPhil study 1 spreadsheet 3 lent.csv")
datblock1 <- datblock1[, c("Cues", "Outcomes", "Frequency")]
datblock1$Cues <- interaction("**", datblock1$Cues, sep = "_")
datblock1
check(datblock1, rm = FALSE)
datblock1$ID <- 1:nrow(datblock1)
datblock1
# for multiple runs: 
for(rep in 1:1000){ # 1000 replications
  
  print(paste0("Replication No.", rep))
  set.seed(rep)

# train model:
# task 1:
traintask1 <- createTrainingData(dattask1, nruns = 1, random = FALSE)
wm_task1 <- RWlearning(traintask1)
# task 2:
traintask2 <- createTrainingData(dattask1, nruns = 1, random = TRUE)
wm_task2 <- RWlearning (traintask2, wm = wm_task1)
# block 1:
trainblock1 <- createTrainingData(datblock1, nruns = 1, random = TRUE)
wm_block1 <- RWlearning(trainblock1, wm = wm_task2)
# block 2:
trainblock2 <- createTrainingData(datblock1, nruns = 1, random = TRUE)
wm_block2 <- RWlearning(trainblock2, wm = wm_block1)
# block 3:
trainblock3 <- createTrainingData(datblock1, nruns = 1, random = TRUE)
wm_block3 <- RWlearning(trainblock3, wm = wm_block2)
# block 4:
trainblock4 <- createTrainingData(datblock1, nruns = 1, random = TRUE)
wm_block4 <- RWlearning(trainblock4, wm = wm_block3)

# choice probabilities for 400 trials:

# for cue set 1 + HF cue set 2 for Acc:
options.activations1d <- c(unlist(getActivations(getWM(wm_block4), cueset = "A_W_**", select.outcomes = "Acc")),
                           unlist(getActivations(getWM(wm_block4), cueset = "A_W_**", select.outcomes = "Prep")))
options.choiceAlternatives <- c("Acc", "Prep")
options1d <- data.frame(activations = options.activations1d,
                        choiceAlternatives = options.choiceAlternatives,
                        cues = "A_W_**",
                        choiceProbability = 0,
                        stringsAsFactors = F)
for (r in 1:nrow(options1d)){
  options1d$choiceProbability[r] <- luceChoice(relu(options1d$activations[r]), 
                                               relu(options1d$activations))
}
options1d
# for cue set 1 + HF cue set 2 for Prep:
options.activations2d <- c(unlist(getActivations(getWM(wm_block4), cueset = "B_Y_**", select.outcomes = "Acc")),
                           unlist(getActivations(getWM(wm_block4), cueset = "B_Y_**", select.outcomes = "Prep")))
options.choiceAlternatives <- c("Acc", "Prep")
options2d <- data.frame(activations = options.activations2d,
                        choiceAlternatives = options.choiceAlternatives,
                        cues = "B_Y_**",
                        choiceProbability = 0,
                        stringsAsFactors = F)
for (r in 1:nrow(options2d)){
  options2d$choiceProbability[r] <- luceChoice(relu(options2d$activations[r]), 
                                               relu(options2d$activations))
}
options2d
options2d$choiceProbability
# for cue set 1 + LF cue set 2 for Acc:
options.activations3d <- c(unlist(getActivations(getWM(wm_block4), cueset = "B_W_**", select.outcomes = "Acc")),
                           unlist(getActivations(getWM(wm_block4), cueset = "B_W_**", select.outcomes = "Prep")))
options.choiceAlternatives <- c("Acc", "Prep")
options3d <- data.frame(activations = options.activations3d,
                        choiceAlternatives = options.choiceAlternatives,
                        cues = "B_W_**",
                        choiceProbability = 0,
                        stringsAsFactors = F)
for (r in 1:nrow(options3d)){
  options3d$choiceProbability[r] <- luceChoice(relu(options3d$activations[r]), 
                                               relu(options3d$activations))
}
options3d
# for cue set 1 + LF cue set 2 for Prep:
options.activations4d <- c(unlist(getActivations(getWM(wm_block4), cueset = "A_Y_**", select.outcomes = "Acc")),
                           unlist(getActivations(getWM(wm_block4), cueset = "A_Y_**", select.outcomes = "Prep")))
options.choiceAlternatives <- c("Acc", "Prep")
options4d <- data.frame(activations = options.activations4d,
                        choiceAlternatives = options.choiceAlternatives,
                        cues = "A_Y_**",
                        choiceProbability = 0,
                        stringsAsFactors = F)
for (r in 1:nrow(options4d)){
  options4d$choiceProbability[r] <- luceChoice(relu(options4d$activations[r]), 
                                               relu(options4d$activations))
}
options4d

cueset1HFcueset2_acc_400trials_choiceprob_study1 <- options1d[options1d$choiceAlternatives == "Acc", "choiceProbability"]
cueset1HFcueset2_prep_400trials_choiceprob_study1 <- options2d[options2d$choiceAlternatives == "Prep", "choiceProbability"]
cueset1LFcueset2_acc_400trials_study1_choiceprob <- options3d[options3d$choiceAlternatives == "Acc", "choiceProbability"]
cueset1LFcueset2_prep_400trials_study1_choiceprob <- options4d[options4d$choiceAlternatives == "Prep", "choiceProbability"]
# save the values:
resultstudy1.400trials.multipleruns <- rbind(resultstudy1.400trials.multipleruns, data.frame(cueset1HFcueset2_acc_400trials_study1 = cueset1HFcueset2_acc_400trials_choiceprob_study1, 
                                                  cueset1HFcueset2_prep_400trials_study1 = cueset1HFcueset2_prep_400trials_choiceprob_study1, 
                                                  cueset1LFcueset2_acc_400trials_study1 = cueset1LFcueset2_acc_400trials_study1_choiceprob, 
                                                  cueset1LFcueset2_prep_400trials_study1 = cueset1LFcueset2_prep_400trials_study1_choiceprob))

}

resultstudy1.400trials.multipleruns
# save the mean:
# saving the mean
mean_cueset1HFcueset2_acc_400trials_study1 <- mean(resultstudy1.400trials.multipleruns$cueset1HFcueset2_acc_400trials_study1, na.rm = TRUE)
mean_cueset1HFcueset2_prep_400trials_study1 <- mean(resultstudy1.400trials.multipleruns$cueset1HFcueset2_prep_400trials_study1, na.rm = TRUE)
mean_cueset1LFcueset2_acc_400trials_study1 <- mean(resultstudy1.400trials.multipleruns$cueset1LFcueset2_acc_400trials_study1, na.rm = TRUE)
mean_cueset1LFcueset2_prep_400trials_study1 <- mean(resultstudy1.400trials.multipleruns$cueset1LFcueset2_prep_400trials_study1, na.rm = TRUE)

# saving the sd
sd_cueset1HFcueset2_acc_400trials_study1 <- sd(resultstudy1.400trials.multipleruns$cueset1HFcueset2_acc_400trials_study1, na.rm = TRUE)
sd_cueset1HFcueset2_prep_400trials_study1 <- sd(resultstudy1.400trials.multipleruns$cueset1HFcueset2_prep_400trials_study1, na.rm = TRUE)
sd_cueset1LFcueset2_acc_400trials_study1 <- sd(resultstudy1.400trials.multipleruns$cueset1LFcueset2_acc_400trials_study1, na.rm = TRUE)
sd_cueset1LFcueset2_prep_400trials_study1 <- sd(resultstudy1.400trials.multipleruns$cueset1LFcueset2_prep_400trials_study1, na.rm = TRUE)

# save the mean for cue sets, with outcome type combined
mean_cueset1HFcueset2_400trials_study1 <- mean(c(mean_cueset1HFcueset2_acc_400trials_study1, mean_cueset1HFcueset2_prep_400trials_study1))
mean_cueset1LFcueset2_400trials_study1 <- mean(c(mean_cueset1LFcueset2_acc_400trials_study1, mean_cueset1LFcueset2_prep_400trials_study1))

# check the means:
mean_cueset1HFcueset2_400trials_study1
mean_cueset1LFcueset2_400trials_study1

# data frame for barplot for 400 trials:
# data frame for barplot:
df400trials_study1 <- data.frame(
  cueset1_HFcueset2 = mean_cueset1HFcueset2_400trials_study1,
  cueset1_LFcueset2 = mean_cueset1LFcueset2_400trials_study1)
# check
df400trials_study1
