# R script study 1 400-trial simulation
# load packages:
library(readr)
library(dplyr)
library(sigmoid)
library(edl)
library(magrittr)
# need to save: 
resultstudy3.400trials.multipleruns <- data.frame(cueset1_acc_400trials = numeric(), cueset1_prep_400trials = numeric(), HFcueset2_acc_400trials = numeric(), LFcueset2_acc_400trials = numeric(), HFcueset2_prep_400trials = numeric(), LFcueset2_prep_400trials = numeric())
# create the 2 training data dfs:
# training data for training stages 1 and 2:
s3dattask1 <- read_csv("MPhil study 3 spreadsheet 1 lent.csv")
s3dattask1 <- s3dattask1[, c("Cues", "Outcomes", "Frequency")]
s3dattask1$Cues <- interaction("**", s3dattask1$Cues, sep = "_")
s3dattask1
check(s3dattask1, rm = FALSE)
s3dattask1$ID <- 1:nrow(s3dattask1)
s3dattask1 
# training data for the learning blocks:
s3datblock1 <- read_csv("MPhil study 3 spreadsheet 3 lent.csv")
s3datblock1 <- s3datblock1[, c("Cues", "Outcomes", "Frequency")]
s3datblock1$Cues <- interaction("**", s3datblock1$Cues, sep = "_")
s3datblock1
check(s3datblock1, rm = FALSE)
s3datblock1$ID <- 1:nrow(s3datblock1)
s3datblock1

# for multiple runs: 
for(rep in 1:1000){ # 1000 replications
  
  print(paste0("Replication No.", rep))
  set.seed(rep)
  # train model:
  # task 1:
  s3traintask1 <- createTrainingData(s3dattask1, nruns = 1, random = FALSE) # training stage 1
  wm_task1s3 <- RWlearning(s3traintask1)
  # task 2:
  s3traintask2 <- createTrainingData(s3dattask1, nruns = 1, random = TRUE) # training stage 2
  wm_task2s3 <- RWlearning (s3traintask2, wm = wm_task1s3)
  # block 1:
  s3trainblock1 <- createTrainingData(s3datblock1, nruns = 1, random = TRUE) # learning block 1
  wm_block1s3 <- RWlearning(s3trainblock1, wm = wm_task2s3)
  # block 2:
  s3trainblock2 <- createTrainingData(s3datblock1, nruns = 1, random = TRUE) # learning block 2
  wm_block2s3 <- RWlearning(s3trainblock2, wm = wm_block1s3)
  # block 3:
  s3trainblock3 <- createTrainingData(s3datblock1, nruns = 1, random = TRUE) # learning block 3
  wm_block3s3 <- RWlearning(s3trainblock3, wm = wm_block2s3)
  # block 4:
  s3trainblock4 <- createTrainingData(s3datblock1, nruns = 1, random = TRUE) # learning block 4
  wm_block4s3 <- RWlearning(s3trainblock4, wm = wm_block3s3)
  # get weight matrix at end of training:
  getWM(wm_block4s3)
  # check that length is 400 trials:
  length(wm_block4s3)
  # calculate choice probabilities:
  # work out choice probabilities for 400 trials:
  # choice probability for the outcomes given cue A + constant cue:
  # i.e. CUE SET 1 for Acc
  options.activations1j <- c(unlist(getActivations(getWM(wm_block4s3), cueset = "A_**", select.outcomes = "Acc")),
                             unlist(getActivations(getWM(wm_block4s3), cueset = "A_**", select.outcomes = "Prep")))
  options.choiceAlternatives <- c("Acc", "Prep")
  options1j <- data.frame(activations = options.activations1j,
                          choiceAlternatives = options.choiceAlternatives,
                          cues = "A_**",
                          choiceProbability = 0,
                          stringsAsFactors = F)
  for (r in 1:nrow(options1j)){
    options1j$choiceProbability[r] <- luceChoice(relu(options1j$activations[r]), 
                                                 relu(options1j$activations))
  }
  options1j
  # choice probability for the outcomes given cue B  + constant cue:
  # i.e. CUE SET 1 for Prep
  options.activations2j <- c(unlist(getActivations(getWM(wm_block4s3), cueset = "B_**", select.outcomes = "Acc")),
                             unlist(getActivations(getWM(wm_block4s3), cueset = "B_**", select.outcomes = "Prep")))
  options.choiceAlternatives <- c("Acc", "Prep")
  options2j <- data.frame(activations = options.activations2j,
                          choiceAlternatives = options.choiceAlternatives,
                          cues = "B_**",
                          choiceProbability = 0,
                          stringsAsFactors = F)
  for (r in 1:nrow(options2j)){
    options2j$choiceProbability[r] <- luceChoice(relu(options2j$activations[r]), 
                                                 relu(options2j$activations))
  }
  options2j
  # choice probability for W (HF cue set 2 for Acc) + constant cue:
  options.activations3j <- c(unlist(getActivations(getWM(wm_block4s3), cueset = "W_**", select.outcomes = "Acc")),
                             unlist(getActivations(getWM(wm_block4s3), cueset = "W_**", select.outcomes = "Prep")))
  options.choiceAlternatives <- c("Acc", "Prep")
  options3j <- data.frame(activations = options.activations3j,
                          choiceAlternatives = options.choiceAlternatives,
                          cues = "W_**",
                          choiceProbability = 0,
                          stringsAsFactors = F)
  for (r in 1:nrow(options3j)){
    options3j$choiceProbability[r] <- luceChoice(relu(options3j$activations[r]), 
                                                 relu(options3j$activations))
  }
  options3j
  # choice probability for X (LF cue set 2 for Acc) + constant cue:
  options.activations4j <- c(unlist(getActivations(getWM(wm_block4s3), cueset = "X_**", select.outcomes = "Acc")),
                             unlist(getActivations(getWM(wm_block4s3), cueset = "X_**", select.outcomes = "Prep")))
  options.choiceAlternatives <- c("Acc", "Prep")
  options4j <- data.frame(activations = options.activations4j,
                          choiceAlternatives = options.choiceAlternatives,
                          cues = "X_**",
                          choiceProbability = 0,
                          stringsAsFactors = F)
  for (r in 1:nrow(options4j)){
    options4j$choiceProbability[r] <- luceChoice(relu(options4j$activations[r]), 
                                                 relu(options4j$activations))
  }
  options4j
  # choice probability for Y (HF cue set 2 for Prep) + constant cue:
  options.activations5j <- c(unlist(getActivations(getWM(wm_block4s3), cueset = "Y_**", select.outcomes = "Acc")),
                             unlist(getActivations(getWM(wm_block4s3), cueset = "Y_**", select.outcomes = "Prep")))
  options.choiceAlternatives <- c("Acc", "Prep")
  options5j <- data.frame(activations = options.activations5j,
                          choiceAlternatives = options.choiceAlternatives,
                          cues = "Y_**",
                          choiceProbability = 0,
                          stringsAsFactors = F)
  for (r in 1:nrow(options5j)){
    options5j$choiceProbability[r] <- luceChoice(relu(options5j$activations[r]), 
                                                 relu(options5j$activations))
  }
  options5j
  # choice probability for Z (LF cue set 2 for Prep) + constant cue:
  options.activations6j <- c(unlist(getActivations(getWM(wm_block4s3), cueset = "Z_**", select.outcomes = "Acc")),
                             unlist(getActivations(getWM(wm_block4s3), cueset = "Z_**", select.outcomes = "Prep")))
  options.choiceAlternatives <- c("Acc", "Prep")
  options6j <- data.frame(activations = options.activations6j,
                          choiceAlternatives = options.choiceAlternatives,
                          cues = "Z_**",
                          choiceProbability = 0,
                          stringsAsFactors = F)
  for (r in 1:nrow(options6j)){
    options6j$choiceProbability[r] <- luceChoice(relu(options6j$activations[r]), 
                                                 relu(options6j$activations))
  }
  options6j
# choice probabilities for correct outcome for each cue set type for each outcome type:
# options1j = cue set 1 Acc; options2j = cue set 1 Prep; options3j = HF cue set 2 Acc; 
# options4j = LF cue set 2 Acc; options5j = HF cue set 2 Prep; options6j = LF cue set 2 Prep
cueset1_choiceprob_400trials_acc <- options1j[options1j$choiceAlternatives == "Acc", "choiceProbability"]
cueset1_choiceprob_400trials_prep <- options2j[options2j$choiceAlternatives == "Prep", "choiceProbability"]
HFcueset2_choiceprob_400trials_acc <- options3j[options3j$choiceAlternatives == "Acc", "choiceProbability"]
LFcueset2_choiceprob_400trials_acc <- options4j[options4j$choiceAlternatives == "Acc", "choiceProbability"]
HFcueset2_choiceprob_400trials_prep <- options5j[options5j$choiceAlternatives == "Prep", "choiceProbability"]
LFcueset2_choiceprob_400trials_prep <- options6j[options6j$choiceAlternatives == "Prep", "choiceProbability"]
# save the values:
resultstudy3.400trials.multipleruns <- rbind(resultstudy3.400trials.multipleruns, data.frame(cueset1_acc_400trials = cueset1_choiceprob_400trials_acc,
                                                                                               cueset1_prep_400trials = cueset1_choiceprob_400trials_prep,
                                                                                               HFcueset2_acc_400trials = HFcueset2_choiceprob_400trials_acc,
                                                                                               LFcueset2_acc_400trials = LFcueset2_choiceprob_400trials_acc,
                                                                                               HFcueset2_prep_400trials = HFcueset2_choiceprob_400trials_prep,
                                                                                               LFcueset2_prep_400trials = LFcueset2_choiceprob_400trials_prep))
}
resultstudy3.400trials.multipleruns
# save the mean:
# saving the mean
mean_cueset1_acc_400trials <- mean(resultstudy3.400trials.multipleruns$cueset1_acc_400trials, na.rm = TRUE)
mean_cueset1_prep_400trials <- mean(resultstudy3.400trials.multipleruns$cueset1_prep_400trials, na.rm = TRUE)
mean_HFcueset2_acc_400trials <- mean(resultstudy3.400trials.multipleruns$HFcueset2_acc_400trials, na.rm = TRUE)
mean_LFcueset2_acc_400trials <- mean(resultstudy3.400trials.multipleruns$LFcueset2_acc_400trials, na.rm = TRUE)
mean_HFcueset2_prep_400trials <- mean(resultstudy3.400trials.multipleruns$HFcueset2_prep_400trials, na.rm = TRUE)
mean_LFcueset2_prep_400trials <- mean(resultstudy3.400trials.multipleruns$LFcueset2_prep_400trials, na.rm = TRUE)
# save the mean for cue sets, with outcome type combined
mean_cueset1_400trials <- mean(c(mean_cueset1_acc_400trials, mean_cueset1_prep_400trials))
mean_HFcueset2_400trials <- mean(c(mean_HFcueset2_acc_400trials, mean_HFcueset2_prep_400trials))
mean_LFcueset2_400trials <- mean(c(mean_LFcueset2_acc_400trials, mean_LFcueset2_prep_400trials))

# check the means:
mean_cueset1_400trials
mean_HFcueset2_400trials
mean_LFcueset2_400trials
