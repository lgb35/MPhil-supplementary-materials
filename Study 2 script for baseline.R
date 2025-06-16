# study 2 random baseline model
# load necessary packages 
library(readr)
library(dplyr)
library(sigmoid)
library(edl)
library(magrittr)
# save data: 
resultstudy1.4000trials.baseline <- data.frame(cueset1HFcueset2_acc_4000trials_study1_shuffled = numeric(), 
                                               cueset1HFcueset2_prep_4000trials_study1_shuffled = numeric(), 
                                               cueset1LFcueset2_acc_4000trials_study1_shuffled = numeric(), 
                                               cueset1LFcueset2_prep_4000trials_study1_shuffled = numeric())

# read csv:
datstudy1 <- read_csv("csv for 70:30 ratio det.csv")
datstudy1 <- datstudy1[, c("Cues", "Outcomes", "Frequency")]
datstudy1$Cues <- interaction("**", datstudy1$Cues, sep = "_")
datstudy1
check(datstudy1, rm = FALSE)
datstudy1$ID <- 1:nrow(datstudy1)
datstudy1
datstudy1_shuffled <- do.call(rbind, replicate(200, datstudy1, simplify = FALSE)) # 4000 trials
datstudy1_shuffled
# start loop
for(rep in 1:1000){ # 1000 replications reported in paper
  # make the training data
  print(paste0("Replication No.", rep))
  set.seed(rep)
  # shuffle the data
  datstudy1_shuffled$Outcomes <- sample(datstudy1_shuffled$Outcomes) # to shuffle
  datstudy1_shuffled$Cues <- sample(datstudy1_shuffled$Cues)
  datstudy1_shuffled
  # train model
  trainforstudy1_4000trials_shuffled <- createTrainingData(datstudy1_shuffled, random=TRUE) # to completely randomise
  wm_det4000_shuffled <- RWlearning(trainforstudy1_4000trials_shuffled)
  # calculate choice probabilities:
  # for cue set 1 + HF cue set 2 for Acc:
  options.activations1c_shuffled <- c(unlist(getActivations(getWM(wm_det4000_shuffled), cueset = "A_W_**", select.outcomes = "Acc")),
                                      unlist(getActivations(getWM(wm_det4000_shuffled), cueset = "A_W_**", select.outcomes = "Prep")))
  options.choiceAlternatives <- c("Acc", "Prep")
  options1c_shuffled <- data.frame(activations = options.activations1c_shuffled,
                                   choiceAlternatives = options.choiceAlternatives,
                                   cues = "A_W_**",
                                   choiceProbability = 0,
                                   stringsAsFactors = F)
  for (r in 1:nrow(options1c_shuffled)){
    options1c_shuffled$choiceProbability[r] <- luceChoice(relu(options1c_shuffled$activations[r]), 
                                                          relu(options1c_shuffled$activations))
  }
  options1c_shuffled
  options1c_shuffled$choiceProbability
  # for cue set 1 + HF cue set 2 for Prep:
  options.activations2c_shuffled <- c(unlist(getActivations(getWM(wm_det4000_shuffled), cueset = "B_Y_**", select.outcomes = "Acc")),
                                      unlist(getActivations(getWM(wm_det4000_shuffled), cueset = "B_Y_**", select.outcomes = "Prep")))
  options.choiceAlternatives <- c("Acc", "Prep")
  options2c_shuffled <- data.frame(activations = options.activations2c_shuffled,
                                   choiceAlternatives = options.choiceAlternatives,
                                   cues = "B_Y_**",
                                   choiceProbability = 0,
                                   stringsAsFactors = F)
  for (r in 1:nrow(options2c_shuffled)){
    options2c_shuffled$choiceProbability[r] <- luceChoice(relu(options2c_shuffled$activations[r]), 
                                                          relu(options2c_shuffled$activations))
  }
  options2c_shuffled
  options2c_shuffled$choiceProbability
  # for cue set 1 + LF cue set 2 for Acc:
  options.activations3c_shuffled <- c(unlist(getActivations(getWM(wm_det4000_shuffled), cueset = "B_W_**", select.outcomes = "Acc")),
                                      unlist(getActivations(getWM(wm_det4000_shuffled), cueset = "B_W_**", select.outcomes = "Prep")))
  options.choiceAlternatives <- c("Acc", "Prep")
  options3c_shuffled <- data.frame(activations = options.activations3c_shuffled,
                                   choiceAlternatives = options.choiceAlternatives,
                                   cues = "B_W_**",
                                   choiceProbability = 0,
                                   stringsAsFactors = F)
  for (r in 1:nrow(options3c_shuffled)){
    options3c_shuffled$choiceProbability[r] <- luceChoice(relu(options3c_shuffled$activations[r]), 
                                                          relu(options3c_shuffled$activations))
  }
  options3c_shuffled
  # for cue set 1 + LF cue set 2 for Prep:
  options.activations4c_shuffled <- c(unlist(getActivations(getWM(wm_det4000_shuffled), cueset = "A_Y_**", select.outcomes = "Acc")),
                                      unlist(getActivations(getWM(wm_det4000_shuffled), cueset = "A_Y_**", select.outcomes = "Prep")))
  options.choiceAlternatives <- c("Acc", "Prep")
  options4c_shuffled <- data.frame(activations = options.activations4c_shuffled,
                                   choiceAlternatives = options.choiceAlternatives,
                                   cues = "A_Y_**",
                                   choiceProbability = 0,
                                   stringsAsFactors = F)
  for (r in 1:nrow(options4c_shuffled)){
    options4c_shuffled$choiceProbability[r] <- luceChoice(relu(options4c_shuffled$activations[r]), 
                                                          relu(options4c_shuffled$activations))
  }
  options4c_shuffled
  
  # save correct choice probability for each
  cueset1HFcueset2_acc_4000trials_study1_shuffled <- options1c_shuffled[options1c_shuffled$choiceAlternatives == "Acc", "choiceProbability"]
  cueset1HFcueset2_prep_4000trials_study1_shuffled <- options2c_shuffled[options2c_shuffled$choiceAlternatives == "Prep", "choiceProbability"]
  cueset1LFcueset2_acc_4000trials_study1_shuffled <- options3c_shuffled[options3c_shuffled$choiceAlternatives == "Acc", "choiceProbability"]
  cueset1LFcueset2_prep_4000trials_study1_shuffled <- options4c_shuffled[options4c_shuffled$choiceAlternatives == "Prep", "choiceProbability"]
  
  # save the values:
  resultstudy1.4000trials.baseline <- rbind(resultstudy1.4000trials.baseline, data.frame(cueset1HFcueset2_acc_4000trials_study1_shuffled = cueset1HFcueset2_acc_4000trials_study1_shuffled,
                                                                                         cueset1HFcueset2_prep_4000trials_study1_shuffled = cueset1HFcueset2_prep_4000trials_study1_shuffled,
                                                                                         cueset1LFcueset2_acc_4000trials_study1_shuffled = cueset1LFcueset2_acc_4000trials_study1_shuffled,
                                                                                         cueset1LFcueset2_prep_4000trials_study1_shuffled = cueset1LFcueset2_prep_4000trials_study1_shuffled))
}
resultstudy1.4000trials.baseline
# mean for each:
# saving the mean
mean_cueset1HFcueset2_acc_4000trials_study1_shuffled <- mean(resultstudy1.4000trials.baseline$cueset1HFcueset2_acc_4000trials_study1_shuffled, na.rm = TRUE)
mean_cueset1HFcueset2_prep_4000trials_study1_shuffled <- mean(resultstudy1.4000trials.baseline$cueset1HFcueset2_prep_4000trials_study1_shuffled, na.rm = TRUE)
mean_cueset1LFcueset2_acc_4000trials_study1_shuffled <- mean(resultstudy1.4000trials.baseline$cueset1LFcueset2_acc_4000trials_study1_shuffled, na.rm = TRUE)
mean_cueset1LFcueset2_prep_4000trials_study1_shuffled <- mean(resultstudy1.4000trials.baseline$cueset1LFcueset2_prep_4000trials_study1_shuffled, na.rm = TRUE)
mean_cueset1HFcueset2_acc_4000trials_study1_shuffled
mean_cueset1HFcueset2_prep_4000trials_study1_shuffled
mean_cueset1LFcueset2_acc_4000trials_study1_shuffled
mean_cueset1LFcueset2_prep_4000trials_study1_shuffled
# compare with actual simulation with baseline models i.e. significance testing:
# for cue set 1 + HF cue set 2 with acc outcome:
# compute observed mean:
obs_cueset1HFcueset2acc <- cueset1HFcueset2_acc_4000trials_study1
# use random baseline means (vector of 1000)
shuffled_acc_cueset1HFcueset2 <- resultstudy1.4000trials.baseline$cueset1HFcueset2_acc_4000trials_study1_shuffled
# compute p-value
p.acc.cueset1HFcueset2 <- mean(abs(shuffled_acc_cueset1HFcueset2 - mean(shuffled_acc_cueset1HFcueset2)) >= abs(obs_cueset1HFcueset2acc - mean(shuffled_acc_cueset1HFcueset2)))
p.acc.cueset1HFcueset2
# for cue set 1 + HF cue set 2 with prep outcome:
# observed mean:
obs_cueset1HFcueset2prep <- cueset1HFcueset2_prep_4000trials_study1
# baseline means (vector of 1000)
shuffled_prep_cueset1HFcueset2 <- resultstudy1.4000trials.baseline$cueset1HFcueset2_prep_4000trials_study1_shuffled
# compute two-tailed p-value
p.prep.cueset1HFcueset2 <- mean(abs(shuffled_prep_cueset1HFcueset2 - mean(shuffled_prep_cueset1HFcueset2)) >= abs(obs_cueset1HFcueset2prep - mean(shuffled_prep_cueset1HFcueset2)))
p.prep.cueset1HFcueset2
# cue set 1 + LF cue set 2 with acc outcome:
# observed mean
obs_cueset1LFcueset2acc <- cueset1LFcueset2_acc_4000trials_study1
# baseline means (vector of 1000)
shuffled_acc_cueset1LFcueset2 <- resultstudy1.4000trials.baseline$cueset1LFcueset2_acc_4000trials_study1_shuffled
# compute p-value
p.acc.cueset1LFcueset2 <- mean(abs(shuffled_acc_cueset1LFcueset2 - mean(shuffled_acc_cueset1LFcueset2)) >= abs(obs_cueset1LFcueset2acc - mean(shuffled_acc_cueset1LFcueset2)))
p.acc.cueset1LFcueset2
# cue set 1 + LF cue set 2 with acc outcome:
# observed mean:
obs_cueset1LFcueset2prep <- cueset1LFcueset2_prep_4000trials_study1
# baseline means (vector of 1000)
shuffled_prep_cueset1LFcueset2 <- resultstudy1.4000trials.baseline$cueset1LFcueset2_prep_4000trials_study1_shuffled
# compute p-value
p.prep.cueset1LFcueset2 <- mean(abs(shuffled_prep_cueset1LFcueset2 - mean(shuffled_prep_cueset1LFcueset2)) >= abs(obs_cueset1LFcueset2prep - mean(shuffled_prep_cueset1LFcueset2)))
p.prep.cueset1LFcueset2

# assess significance between outcomes (to confirm that outcome type is not significant)
# cue set 1 + HF cue set 2: Acc vs. Prep:
obs_diff <- cueset1HFcueset2_acc_4000trials_study1 - cueset1HFcueset2_prep_4000trials_study1
# null distribution
null_diffs <- resultstudy1.4000trials.baseline$cueset1HFcueset2_acc_4000trials_study1_shuffled - 
  resultstudy1.4000trials.baseline$cueset1HFcueset2_prep_4000trials_study1_shuffled
# p-value (i.e. in how many of the baseline runs is the difference greater or equal to the obs difference?)
p.diff <- mean(abs(null_diffs - mean(null_diffs)) >= abs(obs_diff - mean(null_diffs)))
p.diff
# cue set 1 + LF cue set 2: Acc vs. Prep:
obs_diff <- cueset1LFcueset2_acc_4000trials_study1 - cueset1LFcueset2_prep_4000trials_study1
null_diffs <- resultstudy1.4000trials.baseline$cueset1LFcueset2_acc_4000trials_study1_shuffled - 
resultstudy1.4000trials.baseline$cueset1LFcueset2_prep_4000trials_study1_shuffled
# p-value
p.diff <- mean(abs(null_diffs - mean(null_diffs)) >= abs(obs_diff - mean(null_diffs)))
p.diff
# assess significance between cue compounds:
# Acc: cue set 1 + HF cue set 2 vs. cue set 1 + LF cue set 2:
obs_diff <- mean(cueset1HFcueset2_acc_4000trials_study1) - mean(cueset1LFcueset2_acc_4000trials_study1)
obs_diff
# null distribution
null_diffs <- resultstudy1.4000trials.baseline$cueset1HFcueset2_acc_4000trials_study1_shuffled - 
  resultstudy1.4000trials.baseline$cueset1LFcueset2_acc_4000trials_study1_shuffled
null_diffs
count <- sum(abs(null_diffs) >= abs(obs_diff))
count
# p-value
p.diff <- mean(abs(null_diffs - mean(null_diffs)) >= abs(obs_diff - mean(null_diffs)))
p.diff
# Prep: cue set 1 + HF cue set 2 vs. cue set 1 + LF cue set 2:
obs_diff <- mean(cueset1HFcueset2_prep_4000trials_study1) - mean(cueset1LFcueset2_prep_4000trials_study1)
obs_diff
# null distribution
null_diffs <- resultstudy1.4000trials.baseline$cueset1HFcueset2_prep_4000trials_study1_shuffled - 
  resultstudy1.4000trials.baseline$cueset1LFcueset2_prep_4000trials_study1_shuffled
null_diffs
count <- sum(abs(null_diffs) >= abs(obs_diff))
count
# p-value
p.diff <- mean(abs(null_diffs - mean(null_diffs)) >= abs(obs_diff - mean(null_diffs)))
p.diff
