# r script study 2 (study 3 in latex) for random baseline model
# need to run simulation for 4000 trials + compare to 1000 random baseline simulations to assess significance of results
# load necessary packages 
library(readr)
library(dplyr)
library(sigmoid)
library(edl)
library(magrittr)
# save data: 
resultstudy2.4000trials.baseline <- data.frame(HFcueset1HFcueset2_acc_4000trials_study2_shuffled = numeric(), 
                                               HFcueset1HFcueset2_prep_4000trials_study2_shuffled = numeric(), 
                                               HFcueset1LFcueset2_acc_4000trials_study2_shuffled = numeric(), 
                                               HFcueset1LFcueset2_prep_4000trials_study2_shuffled = numeric(),
                                               LFcueset1HFcueset2_acc_4000trials_study2_shuffled = numeric(),
                                               LFcueset1HFcueset2_prep_4000trials_study2_shuffled = numeric(),
                                               LFcueset1LFcueset2_acc_4000trials_study2_shuffled = numeric(),
                                               LFcueset1LFcueset2_prep_4000trials_study2_shuffled = numeric())

# read csv:
datstudy2 <- read_csv("csv for 70:30 ratio prob.csv")
datstudy2 <- datstudy2[, c("Cues", "Outcomes", "Frequency")]
datstudy2$Cues <- interaction("**", datstudy2$Cues, sep = "_")
datstudy2
check(datstudy2, rm = FALSE)
datstudy2$ID <- 1:nrow(datstudy2)
datstudy2
datstudy2_shuffled <- do.call(rbind, replicate(20, datstudy2, simplify = FALSE))
datstudy2_shuffled
# start loop
for(rep in 1:1000){ # 1000 replications reported in paper
  # make the training data
  print(paste0("Replication No.", rep))
  set.seed(rep)
  # shuffle the data
  datstudy2_shuffled$Outcomes <- sample(datstudy2_shuffled$Outcomes)
  datstudy2_shuffled$Cues <- sample(datstudy2_shuffled$Cues)
  datstudy2_shuffled
  # train model
  trainforstudy2_4000trials_shuffled <- createTrainingData(datstudy2_shuffled, random=TRUE)
  wm_prob4000_shuffled <- RWlearning(trainforstudy2_4000trials_shuffled)
  # calculate choice probabilities:
  # choice probabilities for study 2:
  # for HF cue set 1 + HF cue set 2 for Acc:
  # AW:
  options.activations1_shuffled <- c(unlist(getActivations(getWM(wm_prob4000_shuffled), cueset = "A_W_**", select.outcomes = "Acc")),
                            unlist(getActivations(getWM(wm_prob4000_shuffled), cueset = "A_W_**", select.outcomes = "Prep")))
  options.choiceAlternatives <- c("Acc", "Prep")
  options1_shuffled <- data.frame(activations = options.activations1_shuffled,
                         choiceAlternatives = options.choiceAlternatives,
                         cues = "A_W_**",
                         choiceProbability = 0,
                         stringsAsFactors = F)
  for (r in 1:nrow(options1_shuffled)){
    options1_shuffled$choiceProbability[r] <- luceChoice(relu(options1_shuffled$activations[r]), 
                                                relu(options1_shuffled$activations))
  }
  options1_shuffled
  # for HF cue set 1 + HF cue set 2 for Prep:
  # BY
  options.activations2_shuffled <- c(unlist(getActivations(getWM(wm_prob4000_shuffled), cueset = "B_Y_**", select.outcomes = "Acc")),
                            unlist(getActivations(getWM(wm_prob4000_shuffled), cueset = "B_Y_**", select.outcomes = "Prep")))
  options.choiceAlternatives <- c("Acc", "Prep")
  options2_shuffled <- data.frame(activations = options.activations2_shuffled,
                         choiceAlternatives = options.choiceAlternatives,
                         cues = "B_Y_**",
                         choiceProbability = 0,
                         stringsAsFactors = F)
  for (r in 1:nrow(options2_shuffled)){
    options2_shuffled$choiceProbability[r] <- luceChoice(relu(options2_shuffled$activations[r]), 
                                                relu(options2_shuffled$activations))
  }
  options2_shuffled
  # for HF cue set 1 + LF cue set 2 for Acc:
  # BW:
  options.activations3_shuffled <- c(unlist(getActivations(getWM(wm_prob4000_shuffled), cueset = "B_W_**", select.outcomes = "Acc")),
                            unlist(getActivations(getWM(wm_prob4000_shuffled), cueset = "B_W_**", select.outcomes = "Prep")))
  options.choiceAlternatives <- c("Acc", "Prep")
  options3_shuffled <- data.frame(activations = options.activations3_shuffled,
                         choiceAlternatives = options.choiceAlternatives,
                         cues = "B_W_**",
                         choiceProbability = 0,
                         stringsAsFactors = F)
  for (r in 1:nrow(options3_shuffled)){
    options3_shuffled$choiceProbability[r] <- luceChoice(relu(options3_shuffled$activations[r]), 
                                                relu(options3_shuffled$activations))
  }
  options3_shuffled
  # for HF cue set 1 + LF cue set 2 for Prep:
  # AY:
  options.activations4_shuffled <- c(unlist(getActivations(getWM(wm_prob4000_shuffled), cueset = "A_Y_**", select.outcomes = "Acc")),
                            unlist(getActivations(getWM(wm_prob4000_shuffled), cueset = "A_Y_**", select.outcomes = "Prep")))
  options.choiceAlternatives <- c("Acc", "Prep")
  options4_shuffled <- data.frame(activations = options.activations4_shuffled,
                         choiceAlternatives = options.choiceAlternatives,
                         cues = "A_Y_**",
                         choiceProbability = 0,
                         stringsAsFactors = F)
  for (r in 1:nrow(options4_shuffled)){
    options4_shuffled$choiceProbability[r] <- luceChoice(relu(options4_shuffled$activations[r]), 
                                                relu(options4_shuffled$activations))
  }
  options4_shuffled
  # for LF cue set 1 + HF cue set 2 for Acc:
  # AX:
  options.activations5_shuffled <- c(unlist(getActivations(getWM(wm_prob4000_shuffled), cueset = "A_X_**", select.outcomes = "Acc")),
                            unlist(getActivations(getWM(wm_prob4000_shuffled), cueset = "A_X_**", select.outcomes = "Prep")))
  options.choiceAlternatives <- c("Acc", "Prep")
  options5_shuffled <- data.frame(activations = options.activations5_shuffled,
                         choiceAlternatives = options.choiceAlternatives,
                         cues = "A_X_**",
                         choiceProbability = 0,
                         stringsAsFactors = F)
  for (r in 1:nrow(options5_shuffled)){
    options5_shuffled$choiceProbability[r] <- luceChoice(relu(options5_shuffled$activations[r]), 
                                                relu(options5_shuffled$activations))
  }
  options5_shuffled
  # for LF cue set 1 + HF cue set 2 for Prep:
  # BZ:
  options.activations6_shuffled <- c(unlist(getActivations(getWM(wm_prob4000_shuffled), cueset = "B_Z_**", select.outcomes = "Acc")),
                            unlist(getActivations(getWM(wm_prob4000_shuffled), cueset = "B_Z_**", select.outcomes = "Prep")))
  options.choiceAlternatives <- c("Acc", "Prep")
  options6_shuffled <- data.frame(activations = options.activations6_shuffled,
                         choiceAlternatives = options.choiceAlternatives,
                         cues = "B_Z_**",
                         choiceProbability = 0,
                         stringsAsFactors = F)
  for (r in 1:nrow(options6_shuffled)){
    options6_shuffled$choiceProbability[r] <- luceChoice(relu(options6_shuffled$activations[r]), 
                                                relu(options6_shuffled$activations))
  }
  options6_shuffled
  # for LF cue set 1 + LF cue set 2 for Acc:
  # BX
  options.activations7_shuffled <- c(unlist(getActivations(getWM(wm_prob4000_shuffled), cueset = "B_X_**", select.outcomes = "Acc")),
                            unlist(getActivations(getWM(wm_prob4000_shuffled), cueset = "B_X_**", select.outcomes = "Prep")))
  options.choiceAlternatives <- c("Acc", "Prep")
  options7_shuffled <- data.frame(activations = options.activations7_shuffled,
                         choiceAlternatives = options.choiceAlternatives,
                         cues = "B_X_**",
                         choiceProbability = 0,
                         stringsAsFactors = F)
  for (r in 1:nrow(options7_shuffled)){
    options7_shuffled$choiceProbability[r] <- luceChoice(relu(options7_shuffled$activations[r]), 
                                                relu(options7_shuffled$activations))
  }
  options7_shuffled
  # for LF cue set 1 + LF cue set 2 for Prep:
  # AZ:
  options.activations8_shuffled <- c(unlist(getActivations(getWM(wm_prob4000_shuffled), cueset = "A_Z_**", select.outcomes = "Acc")),
                            unlist(getActivations(getWM(wm_prob4000_shuffled), cueset = "A_Z_**", select.outcomes = "Prep")))
  options.choiceAlternatives <- c("Acc", "Prep")
  options8_shuffled <- data.frame(activations = options.activations8_shuffled,
                         choiceAlternatives = options.choiceAlternatives,
                         cues = "A_Z_**",
                         choiceProbability = 0,
                         stringsAsFactors = F)
  for (r in 1:nrow(options8_shuffled)){
    options8_shuffled$choiceProbability[r] <- luceChoice(relu(options8_shuffled$activations[r]), 
                                                relu(options8_shuffled$activations))
  }
  options8_shuffled
  # save correct choice probability for each:
  # for HF cue set 1 + HF cue set 2
  HFcueset1HFcueset2_acc_4000trials_study2_shuffled <- options1_shuffled[options1_shuffled$choiceAlternatives == "Acc", "choiceProbability"]
  HFcueset1HFcueset2_prep_4000trials_study2_shuffled <- options2_shuffled[options2_shuffled$choiceAlternatives == "Prep", "choiceProbability"]
  # for HF cue set 1 + LF cue set 2 :
  HFcueset1LFcueset2_acc_4000trials_study2_shuffled <- options3_shuffled[options3_shuffled$choiceAlternatives == "Acc", "choiceProbability"]
  HFcueset1LFcueset2_prep_4000trials_study2_shuffled <- options4_shuffled[options4_shuffled$choiceAlternatives == "Prep", "choiceProbability"]
  # for LF cue set 1 + HF cue set 2:
  LFcueset1HFcueset2_acc_4000trials_study2_shuffled <- options5_shuffled[options5_shuffled$choiceAlternatives == "Acc", "choiceProbability"]
  LFcueset1HFcueset2_prep_4000trials_study2_shuffled <- options6_shuffled[options6_shuffled$choiceAlternatives == "Prep", "choiceProbability"]
  # for LF cue set 1 + LF cue set 2:
  LFcueset1LFcueset2_acc_4000trials_study2_shuffled <- options7_shuffled[options7_shuffled$choiceAlternatives == "Acc", "choiceProbability"]
  LFcueset1LFcueset2_prep_4000trials_study2_shuffled <- options8_shuffled[options8_shuffled$choiceAlternatives == "Prep", "choiceProbability"]
  
# save the values:
resultstudy2.4000trials.baseline <- rbind(resultstudy2.4000trials.baseline, data.frame(HFcueset1HFcueset2_acc_4000trials_study2_shuffled = HFcueset1HFcueset2_acc_4000trials_study2_shuffled,
                                                                                       HFcueset1HFcueset2_prep_4000trials_study2_shuffled = HFcueset1HFcueset2_prep_4000trials_study2_shuffled,
                                                                                       HFcueset1LFcueset2_acc_4000trials_study2_shuffled = HFcueset1LFcueset2_acc_4000trials_study2_shuffled,
                                                                                       HFcueset1LFcueset2_prep_4000trials_study2_shuffled = HFcueset1LFcueset2_prep_4000trials_study2_shuffled,
                                                                                       LFcueset1HFcueset2_acc_4000trials_study2_shuffled = LFcueset1HFcueset2_acc_4000trials_study2_shuffled,
                                                                                       LFcueset1HFcueset2_prep_4000trials_study2_shuffled = LFcueset1HFcueset2_prep_4000trials_study2_shuffled,
                                                                                       LFcueset1LFcueset2_acc_4000trials_study2_shuffled = LFcueset1LFcueset2_acc_4000trials_study2_shuffled,
                                                                                       LFcueset1LFcueset2_prep_4000trials_study2_shuffled = LFcueset1LFcueset2_prep_4000trials_study2_shuffled))

}
resultstudy2.4000trials.baseline 
# save the mean for different cue combinations, with outcome type combined
mean_HFcueset1HFcueset2_study2_4000trials_shuffled <- mean(c(HFcueset1HFcueset2_acc_4000trials_study2_shuffled, HFcueset1HFcueset2_prep_4000trials_study2_shuffled))
mean_HFcueset1LFcueset2_study2_4000trials_shuffled <- mean(c(HFcueset1LFcueset2_acc_4000trials_study2_shuffled, HFcueset1LFcueset2_prep_4000trials_study2_shuffled))
mean_LFcueset1HFcueset2_study2_4000trials_shuffled <- mean(c(LFcueset1HFcueset2_acc_4000trials_study2_shuffled, LFcueset1HFcueset2_prep_4000trials_study2_shuffled))
mean_LFcueset1LFcueset2_study2_4000trials_shuffled <- mean(c(LFcueset1LFcueset2_acc_4000trials_study2_shuffled, LFcueset1LFcueset2_prep_4000trials_study2_shuffled))

# compare with actual simulation with baseline models:
# compute observed mean
obs_HFcueset1HFcueset2acc <- HFcueset1HFcueset2_acc_4000trials_study2
# Use baseline means (vector of 1000)
shuffled_acc_HFcueset1HFcueset2 <- resultstudy2.4000trials.baseline$HFcueset1HFcueset2_acc_4000trials_study2_shuffled
# compute empirical two-tailed p-value
p.acc.HFcueset1HFcueset2 <- mean(abs(shuffled_acc_HFcueset1HFcueset2 - mean(shuffled_acc_HFcueset1HFcueset2)) >= abs(obs_HFcueset1HFcueset2acc - mean(shuffled_acc_HFcueset1HFcueset2)))
p.acc.HFcueset1HFcueset2
# same thing for prep:
# observed mean:
obs_HFcueset1HFcueset2prep <- HFcueset1HFcueset2_prep_4000trials_study2
# Use baseline means (vector of 1000)
shuffled_prep_HFcueset1HFcueset2 <- resultstudy2.4000trials.baseline$HFcueset1HFcueset2_prep_4000trials_study2_shuffled
# compute two-tailed p-value
p.prep.HFcueset1HFcueset2 <- mean(abs(shuffled_prep_HFcueset1HFcueset2 - mean(shuffled_prep_HFcueset1HFcueset2)) >= abs(obs_HFcueset1HFcueset2prep - mean(shuffled_prep_HFcueset1HFcueset2)))
p.prep.HFcueset1HFcueset2
# HF cue set 1 + LF cue set 2 acc
# compute observed mean
obs_HFcueset1LFcueset2acc <- HFcueset1LFcueset2_acc_4000trials_study2
# Use baseline means (vector of 1000)
shuffled_acc_HFcueset1LFcueset2 <- resultstudy2.4000trials.baseline$HFcueset1LFcueset2_acc_4000trials_study2_shuffled
# compute empirical two-tailed p-value
p.acc.HFcueset1LFcueset2 <- mean(abs(shuffled_acc_HFcueset1LFcueset2 - mean(shuffled_acc_HFcueset1LFcueset2)) >= abs(obs_HFcueset1LFcueset2acc - mean(shuffled_acc_HFcueset1LFcueset2)))
p.acc.HFcueset1LFcueset2
# same thing for prep:
# compute observed mean
obs_HFcueset1LFcueset2prep <- HFcueset1LFcueset2_prep_4000trials_study2
# Use baseline means (vector of 1000)
shuffled_prep_HFcueset1LFcueset2 <- resultstudy2.4000trials.baseline$HFcueset1LFcueset2_prep_4000trials_study2_shuffled
# compute empirical two-tailed p-value
p.prep.HFcueset1LFcueset2 <- mean(abs(shuffled_prep_HFcueset1LFcueset2 - mean(shuffled_prep_HFcueset1LFcueset2)) >= abs(obs_HFcueset1LFcueset2prep - mean(shuffled_prep_HFcueset1LFcueset2)))
p.prep.HFcueset1LFcueset2
# for LF cue set 1 + HF cue set 2:
# for acc:
obs_LFcueset1HFcueset2acc <- LFcueset1HFcueset2_acc_4000trials_study2
shuffled_acc_LFcueset1HFcueset2 <- resultstudy2.4000trials.baseline$LFcueset1HFcueset2_acc_4000trials_study2_shuffled
# calculate:
p.acc.LFcueset1HFcueset2 <- mean(abs(shuffled_acc_LFcueset1HFcueset2 - mean(shuffled_acc_LFcueset1HFcueset2)) >= abs(obs_LFcueset1HFcueset2acc - mean(shuffled_acc_LFcueset1HFcueset2)))
p.acc.LFcueset1HFcueset2
# for prep:
obs_LFcueset1HFcueset2prep <- LFcueset1HFcueset2_prep_4000trials_study2
shuffled_prep_LFcueset1HFcueset2 <- resultstudy2.4000trials.baseline$LFcueset1HFcueset2_prep_4000trials_study2_shuffled
# calculate:
p.prep.LFcueset1HFcueset2 <- mean(abs(shuffled_prep_LFcueset1HFcueset2 - mean(shuffled_prep_LFcueset1HFcueset2)) >= abs(obs_LFcueset1HFcueset2prep - mean(shuffled_prep_LFcueset1HFcueset2)))
p.prep.LFcueset1HFcueset2
# for LF cue set 1 + LF cue set 2:
# for acc:
obs_LFcueset1LFcueset2acc <- LFcueset1LFcueset2_acc_4000trials_study2
shuffled_acc_LFcueset1LFcueset2 <- resultstudy2.4000trials.baseline$LFcueset1LFcueset2_acc_4000trials_study2_shuffled
# calculate:
p.acc.LFcueset1LFcueset2 <- mean(abs(shuffled_acc_LFcueset1LFcueset2 - mean(shuffled_acc_LFcueset1LFcueset2)) >= abs(obs_LFcueset1LFcueset2acc - mean(shuffled_acc_LFcueset1LFcueset2)))
p.acc.LFcueset1LFcueset2
# compare between outcomes:
# compare differences between outcomes:
# i.e. confirm that outcome = not significant. 
# for HF cue set 1 + HF cue set 2, Acc vs. Prep:
obs_diff <- HFcueset1HFcueset2_acc_4000trials_study2 - HFcueset1HFcueset2_prep_4000trials_study2

# Null distribution: per-replication differences under shuffling
null_diffs <- resultstudy2.4000trials.baseline$HFcueset1HFcueset2_acc_4000trials_study2_shuffled - 
  resultstudy2.4000trials.baseline$HFcueset1HFcueset2_prep_4000trials_study2_shuffled

# Two-tailed p-value
p.diff <- mean(abs(null_diffs - mean(null_diffs)) >= abs(obs_diff - mean(null_diffs)))
p.diff

# for HF cue set 1 + LF cue set 2, Acc vs. Prep:
obs_diff <- HFcueset1LFcueset2_acc_4000trials_study2 - HFcueset1LFcueset2_prep_4000trials_study2
# Null distribution: per-replication differences under shuffling
null_diffs <- resultstudy2.4000trials.baseline$HFcueset1LFcueset2_acc_4000trials_study2_shuffled - 
  resultstudy2.4000trials.baseline$HFcueset1LFcueset2_prep_4000trials_study2_shuffled
# Two-tailed p-value
p.diff <- mean(abs(null_diffs - mean(null_diffs)) >= abs(obs_diff - mean(null_diffs)))
p.diff
# for LF cue set 1 + HF cue set 2, Acc vs. Prep:
obs_diff <- LFcueset1HFcueset2_acc_4000trials_study2 - LFcueset1HFcueset2_prep_4000trials_study2
null_diffs <- resultstudy2.4000trials.baseline$LFcueset1HFcueset2_acc_4000trials_study2_shuffled - 
  resultstudy2.4000trials.baseline$LFcueset1HFcueset2_prep_4000trials_study2_shuffled
p.diff <- mean(abs(null_diffs - mean(null_diffs)) >= abs(obs_diff - mean(null_diffs)))
p.diff
# for LF cue set 1 + LF cue set 2, Acc vs. Prep:
obs_diff <- LFcueset1LFcueset2_acc_4000trials_study2 - LFcueset1LFcueset2_prep_4000trials_study2
null_diffs <- resultstudy2.4000trials.baseline$LFcueset1LFcueset2_acc_4000trials_study2_shuffled - 
  resultstudy2.4000trials.baseline$LFcueset1LFcueset2_prep_4000trials_study2_shuffled
p.diff <- mean(abs(null_diffs - mean(null_diffs)) >= abs(obs_diff - mean(null_diffs)))
p.diff
