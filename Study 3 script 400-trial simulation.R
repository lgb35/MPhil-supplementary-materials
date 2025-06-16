# Study 3 script 400-trial simulation
# load packages:
library(readr)
library(dplyr)
library(sigmoid)
library(edl)
library(magrittr)
# save data: 
resultstudy2.400trials.multipleruns <- data.frame(HFcueset1HFcueset2_acc_400trials_study2 = numeric(), 
                                                  HFcueset1HFcueset2_prep_400trials_study2 = numeric(), 
                                                  HFcueset1LFcueset2_acc_400trials_study2 = numeric(), 
                                                  HFcueset1LFcueset2_prep_400trials_study2 = numeric(),
                                                  LFcueset1HFcueset2_acc_400trials_study2 = numeric(),
                                                  LFcueset1HFcueset2_prep_400trials_study2 = numeric(),
                                                  LFcueset1LFcueset2_acc_400trials_study2 = numeric(),
                                                  LFcueset1LFcueset2_prep_400trials_study2 = numeric())

# create the two training data frames:
# training data for training stages 1 and 2:
s2dattask1 <- read_csv("MPhil study 2 spreadsheet 1 lent.csv")
s2dattask1 <- s2dattask1[, c("Cues", "Outcomes", "Frequency")]
s2dattask1$Cues <- interaction("**", s2dattask1$Cues, sep = "_")
s2dattask1
check(s2dattask1, rm = FALSE)
s2dattask1$ID <- 1:nrow(s2dattask1)
s2dattask1

# for training data for learning blocks:
s2datblock1 <- read_csv("MPhil study 2 block spreadsheet.csv")
s2datblock1 <- s2datblock1[, c("Cues", "Outcomes", "Frequency")]
s2datblock1$Cues <- interaction("**", s2datblock1$Cues, sep = "_")
s2datblock1
check(s2datblock1, rm = FALSE)
s2datblock1$ID <- 1:nrow(s2datblock1)
s2datblock1

# for multiple runs:
for(rep in 1:1000){ # 1000 replications
  
  print(paste0("Replication No.", rep))
  set.seed(rep)
# training stage 1:
  s2traintask1 <- createTrainingData(s2dattask1, nruns = 1, random = FALSE)
  wm_task1s2 <- RWlearning(s2traintask1)
# training stage 2:
  s2traintask2 <- createTrainingData(s2dattask1, nruns = 1, random = TRUE)
  wm_task2s2 <- RWlearning (s2traintask2, wm = wm_task1s2)
# learning block 1:
  s2trainblock1 <- createTrainingData(s2datblock1, nruns = 1, random = TRUE)
# learning block 2:
  s2trainblock2 <- createTrainingData(s2datblock1, nruns = 1, random = TRUE)
# learning block 3:
  s2trainblock3 <- createTrainingData(s2datblock1, nruns = 1, random = TRUE)
  wm_block1s2 <- RWlearning(s2trainblock1, wm = wm_task2s2)
  wm_block2s2 <- RWlearning(s2trainblock2, wm = wm_block1s2)
  wm_block3s2 <- RWlearning(s2trainblock3, wm = wm_block2s2)
  
# calculate choice probabilities:
# for HF cue set 1 + HF cue set 2 with Acc outcome: 
# AW:
options.activations1b <- c(unlist(getActivations(getWM(wm_block3s2), cueset = "A_W_**", select.outcomes = "Acc")),
                             unlist(getActivations(getWM(wm_block3s2), cueset = "A_W_**", select.outcomes = "Prep")))
options.choiceAlternatives <- c("Acc", "Prep")
options1b <- data.frame(activations = options.activations1b,
                          choiceAlternatives = options.choiceAlternatives,
                          cues = "A_W_**",
                          choiceProbability = 0,
                          stringsAsFactors = F)
for (r in 1:nrow(options1b)){
    options1b$choiceProbability[r] <- luceChoice(relu(options1b$activations[r]), 
                                                 relu(options1b$activations))
  }
 options1b
 # for HF cue set 1 + HF cue set 2 with Prep outcome: 
   # BY
  options.activations2b <- c(unlist(getActivations(getWM(wm_block3s2), cueset = "B_Y_**", select.outcomes = "Acc")),
                             unlist(getActivations(getWM(wm_block3s2), cueset = "B_Y_**", select.outcomes = "Prep")))
  options.choiceAlternatives <- c("Acc", "Prep")
  options2b <- data.frame(activations = options.activations2b,
                          choiceAlternatives = options.choiceAlternatives,
                          cues = "B_Y_**",
                          choiceProbability = 0,
                          stringsAsFactors = F)
  for (r in 1:nrow(options2b)){
    options2b$choiceProbability[r] <- luceChoice(relu(options2b$activations[r]), 
                                                 relu(options2b$activations))
  }
  options2b
# for HF cue set 1 + LF cue set 2 with Acc outcome: 
  # BW:
  options.activations3b <- c(unlist(getActivations(getWM(wm_block3s2), cueset = "B_W_**", select.outcomes = "Acc")),
                             unlist(getActivations(getWM(wm_block3s2), cueset = "B_W_**", select.outcomes = "Prep")))
  options.choiceAlternatives <- c("Acc", "Prep")
  options3b <- data.frame(activations = options.activations3b,
                          choiceAlternatives = options.choiceAlternatives,
                          cues = "B_W_**",
                          choiceProbability = 0,
                          stringsAsFactors = F)
  for (r in 1:nrow(options3b)){
    options3b$choiceProbability[r] <- luceChoice(relu(options3b$activations[r]), 
                                                 relu(options3b$activations))
  }
  options3b
# for HF cue set 1 + LF cue set 2 with Prep outcome:
  # AY:
  options.activations4b <- c(unlist(getActivations(getWM(wm_block3s2), cueset = "A_Y_**", select.outcomes = "Acc")),
                             unlist(getActivations(getWM(wm_block3s2), cueset = "A_Y_**", select.outcomes = "Prep")))
  options.choiceAlternatives <- c("Acc", "Prep")
  options4b <- data.frame(activations = options.activations4b,
                          choiceAlternatives = options.choiceAlternatives,
                          cues = "A_Y_**",
                          choiceProbability = 0,
                          stringsAsFactors = F)
  for (r in 1:nrow(options4b)){
    options4b$choiceProbability[r] <- luceChoice(relu(options4b$activations[r]), 
                                                 relu(options4b$activations))
  }
  options4b
# for LF cue set 1 + HF cue set 2 with Acc outcome:
  # AX:
  options.activations5b <- c(unlist(getActivations(getWM(wm_block3s2), cueset = "A_X_**", select.outcomes = "Acc")),
                             unlist(getActivations(getWM(wm_block3s2), cueset = "A_X_**", select.outcomes = "Prep")))
  options.choiceAlternatives <- c("Acc", "Prep")
  options5b <- data.frame(activations = options.activations5b,
                          choiceAlternatives = options.choiceAlternatives,
                          cues = "A_X_**",
                          choiceProbability = 0,
                          stringsAsFactors = F)
  for (r in 1:nrow(options5b)){
    options5b$choiceProbability[r] <- luceChoice(relu(options5b$activations[r]), 
                                                 relu(options5b$activations))
  }
  options5b
# for LF cue set 1 + HF cue set 2 with Prep outcome:
  # BZ:
  options.activations6b <- c(unlist(getActivations(getWM(wm_block3s2), cueset = "B_Z_**", select.outcomes = "Acc")),
                             unlist(getActivations(getWM(wm_block3s2), cueset = "B_Z_**", select.outcomes = "Prep")))
  options.choiceAlternatives <- c("Acc", "Prep")
  options6b <- data.frame(activations = options.activations6b,
                          choiceAlternatives = options.choiceAlternatives,
                          cues = "B_Z_**",
                          choiceProbability = 0,
                          stringsAsFactors = F)
  for (r in 1:nrow(options6b)){
    options6b$choiceProbability[r] <- luceChoice(relu(options6b$activations[r]), 
                                                 relu(options6b$activations))
  }
  options6b
# for LF cue set 1 + LF cue set 2 with Acc outcome:
  # BX
  options.activations7b <- c(unlist(getActivations(getWM(wm_block3s2), cueset = "B_X_**", select.outcomes = "Acc")),
                             unlist(getActivations(getWM(wm_block3s2), cueset = "B_X_**", select.outcomes = "Prep")))
  options.choiceAlternatives <- c("Acc", "Prep")
  options7b <- data.frame(activations = options.activations7b,
                          choiceAlternatives = options.choiceAlternatives,
                          cues = "B_X_**",
                          choiceProbability = 0,
                          stringsAsFactors = F)
  for (r in 1:nrow(options7b)){
    options7b$choiceProbability[r] <- luceChoice(relu(options7b$activations[r]), 
                                                 relu(options7b$activations))
  }
  options7b
# for LF cue set 1 + LF cue set 2 with Prep outcome:
  # AZ:
  options.activations8b <- c(unlist(getActivations(getWM(wm_block3s2), cueset = "A_Z_**", select.outcomes = "Acc")),
                             unlist(getActivations(getWM(wm_block3s2), cueset = "A_Z_**", select.outcomes = "Prep")))
  options.choiceAlternatives <- c("Acc", "Prep")
  options8b <- data.frame(activations = options.activations8b,
                          choiceAlternatives = options.choiceAlternatives,
                          cues = "A_Z_**",
                          choiceProbability = 0,
                          stringsAsFactors = F)
  for (r in 1:nrow(options8b)){
    options8b$choiceProbability[r] <- luceChoice(relu(options8b$activations[r]), 
                                                 relu(options8b$activations))
  }
options8b
# save correct choice probability for each:
# for HF cue set 1 + HF cue set 2
  HFcueset1HFcueset2_acc_400trials_study2 <- options1b[options1b$choiceAlternatives == "Acc", "choiceProbability"]
  HFcueset1HFcueset2_prep_400trials_study2 <- options2b[options2b$choiceAlternatives == "Prep", "choiceProbability"]
# for HF cue set 1 + LF cue set 2 :
  HFcueset1LFcueset2_acc_400trials_study2 <- options3b[options3b$choiceAlternatives == "Acc", "choiceProbability"]
  HFcueset1LFcueset2_prep_400trials_study2 <- options4b[options4b$choiceAlternatives == "Prep", "choiceProbability"]
# for LF cue set 1 + HF cue set 2:
  LFcueset1HFcueset2_acc_400trials_study2 <- options5b[options5b$choiceAlternatives == "Acc", "choiceProbability"]
  LFcueset1HFcueset2_prep_400trials_study2 <- options6b[options6b$choiceAlternatives == "Prep", "choiceProbability"]
# for LF cue set 1 + LF cue set 2:
  LFcueset1LFcueset2_acc_400trials_study2 <- options7b[options7b$choiceAlternatives == "Acc", "choiceProbability"]
  LFcueset1LFcueset2_prep_400trials_study2 <- options8b[options8b$choiceAlternatives == "Prep", "choiceProbability"]

  # save the values:
  resultstudy2.400trials.multipleruns <- rbind(resultstudy2.400trials.multipleruns, data.frame(HFcueset1HFcueset2_acc_400trials_study2 = HFcueset1HFcueset2_acc_400trials_study2,
                                                                                               HFcueset1HFcueset2_prep_400trials_study2 = HFcueset1HFcueset2_prep_400trials_study2,
                                                                                               HFcueset1LFcueset2_acc_400trials_study2 = HFcueset1LFcueset2_acc_400trials_study2,
                                                                                               HFcueset1LFcueset2_prep_400trials_study2 = HFcueset1LFcueset2_prep_400trials_study2,
                                                                                               LFcueset1HFcueset2_acc_400trials_study2 = LFcueset1HFcueset2_acc_400trials_study2,
                                                                                               LFcueset1HFcueset2_prep_400trials_study2 = LFcueset1HFcueset2_prep_400trials_study2,
                                                                                               LFcueset1LFcueset2_acc_400trials_study2 = LFcueset1LFcueset2_acc_400trials_study2,
                                                                                               LFcueset1LFcueset2_prep_400trials_study2 = LFcueset1LFcueset2_prep_400trials_study2))
  
  
}
resultstudy2.400trials.multipleruns
# save the mean for different cue combinations, with outcome type combined
mean_HFcueset1HFcueset2_study2_400trials <- mean(c(HFcueset1HFcueset2_acc_400trials_study2, HFcueset1HFcueset2_prep_400trials_study2))
mean_HFcueset1LFcueset2_study2_400trials <- mean(c(HFcueset1LFcueset2_acc_400trials_study2, HFcueset1LFcueset2_prep_400trials_study2))
mean_LFcueset1HFcueset2_study2_400trials <- mean(c(LFcueset1HFcueset2_acc_400trials_study2, LFcueset1HFcueset2_prep_400trials_study2))
mean_LFcueset1LFcueset2_study2_400trials <- mean(c(LFcueset1LFcueset2_acc_400trials_study2, LFcueset1LFcueset2_prep_400trials_study2))

# data frame for barplot with 4000- and 400-trial models:
df400trials_study2 <- data.frame(
  HFcueset1_HFcueset2 = mean_HFcueset1HFcueset2_study2_400trials,
  HFcueset1_LFcueset2 = mean_HFcueset1LFcueset2_study2_400trials,
  LFcueset1_HFcueset2 = mean_LFcueset1HFcueset2_study2_400trials,
  LFcueset1_LFcueset2 = mean_LFcueset1LFcueset2_study2_400trials)
df400trials_study2
