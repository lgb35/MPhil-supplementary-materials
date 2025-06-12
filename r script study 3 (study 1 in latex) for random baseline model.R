# r script study 3 (study 1 in latex) for random baseline model
# need to run simulation for 4000 trials + compare to 1000 random baseline simulations to assess significance of results
# load necessary packages 
library(readr)
library(dplyr)
library(sigmoid)
library(edl)
library(magrittr)
# save data: 
resultstudy3.4000trials.baseline <- data.frame(cueset1_acc_4000trials_shuffled = numeric(), cueset1_prep_4000trials_shuffled = numeric(), 
                                               HFcueset2_acc_4000trials_shuffled = numeric(), LFcueset2_acc_4000trials_shuffled = numeric(), 
                                               HFcueset2_prep_4000trials_shuffled = numeric(), LFcueset2_prep_4000trials_shuffled = numeric())
# read csv:
datforst3 <- read_csv("csv for study 3 4000 trials.csv") # has 20 trials in it.
datforst3 <- datforst3[, c("Cues", "Outcomes", "Frequency")]
datforst3$Cues <- interaction("**", datforst3$Cues, sep = "_")
datforst3
check(datforst3, rm = FALSE)
datforst3$ID <- 1:nrow(datforst3)
datforst3
datforst3_shuffled <- do.call(rbind, replicate(200, datforst3, simplify = FALSE))
datforst3_shuffled
# start loop
for(rep in 1:1000){ # 1000 replications reported in paper
  # make the training data
  print(paste0("Replication No.", rep))
  set.seed(rep)
  # shuffled the data
  datforst3_shuffled$Outcomes <- sample(datforst3_shuffled$Outcomes)
  datforst3_shuffled$Cues <- sample(datforst3_shuffled$Cues)
  datforst3_shuffled
  trainforst3_4000_shuffled <- createTrainingData(datforst3_shuffled, random=TRUE)
  wm_st34000_shuffled <- RWlearning(trainforst3_4000_shuffled)

# choice probabilities:
  # now calculate the choice probabilities for the actual simulation by end of training (i.e. activations after 4000 trials):
  # choice probability for the outcomes given cue A + constant cue:
  # i.e. CUE SET 1 for Acc
  options.activations1i.shuffled <- c(unlist(getActivations(getWM(wm_st34000_shuffled), cueset = "A_**", select.outcomes = "Acc")),
                             unlist(getActivations(getWM(wm_st34000_shuffled), cueset = "A_**", select.outcomes = "Prep")))
  options.choiceAlternatives <- c("Acc", "Prep")
  options1i.shuffled <- data.frame(activations = options.activations1i.shuffled,
                          choiceAlternatives = options.choiceAlternatives,
                          cues = "A_**",
                          choiceProbability = 0,
                          stringsAsFactors = F)
  for (r in 1:nrow(options1i.shuffled)){
    options1i.shuffled$choiceProbability[r] <- luceChoice(relu(options1i.shuffled$activations[r]), 
                                                 relu(options1i.shuffled$activations))
  }
  options1i.shuffled
  # Acc = 0.89 choice probability; Prep = 0.11
  
  # choice probability for the outcomes given cue B  + constant cue:
  # i.e. CUE SET 1 for Prep
  options.activations2i.shuffled <- c(unlist(getActivations(getWM(wm_st34000_shuffled), cueset = "B_**", select.outcomes = "Acc")),
                             unlist(getActivations(getWM(wm_st34000_shuffled), cueset = "B_**", select.outcomes = "Prep")))
  options.choiceAlternatives <- c("Acc", "Prep")
  options2i.shuffled <- data.frame(activations = options.activations2i.shuffled,
                          choiceAlternatives = options.choiceAlternatives,
                          cues = "B_**",
                          choiceProbability = 0,
                          stringsAsFactors = F)
  for (r in 1:nrow(options2i.shuffled)){
    options2i.shuffled$choiceProbability[r] <- luceChoice(relu(options2i.shuffled$activations[r]), 
                                                 relu(options2i.shuffled$activations))
  }
  options2i.shuffled
  # Acc= 0.11 choice probability; Prep = 0.89
  # choice probability for W (HF cue set 2 for Acc) + constant cue:
  options.activations3i.shuffled <- c(unlist(getActivations(getWM(wm_st34000_shuffled), cueset = "W_**", select.outcomes = "Acc")),
                             unlist(getActivations(getWM(wm_st34000_shuffled), cueset = "W_**", select.outcomes = "Prep")))
  options.choiceAlternatives <- c("Acc", "Prep")
  options3i.shuffled <- data.frame(activations = options.activations3i.shuffled,
                          choiceAlternatives = options.choiceAlternatives,
                          cues = "W_**",
                          choiceProbability = 0,
                          stringsAsFactors = F)
  for (r in 1:nrow(options3i.shuffled)){
    options3i.shuffled$choiceProbability[r] <- luceChoice(relu(options3i.shuffled$activations[r]), 
                                                 relu(options3i.shuffled$activations))
  }
  options3i.shuffled
  # Acc: 0.73; Prep: 0.27
  # choice probability for X (LF cue set 2 for Acc) + constant cue:
  options.activations4i.shuffled <- c(unlist(getActivations(getWM(wm_st34000_shuffled), cueset = "X_**", select.outcomes = "Acc")),
                             unlist(getActivations(getWM(wm_st34000_shuffled), cueset = "X_**", select.outcomes = "Prep")))
  options.choiceAlternatives <- c("Acc", "Prep")
  options4i.shuffled <- data.frame(activations = options.activations4i.shuffled,
                          choiceAlternatives = options.choiceAlternatives,
                          cues = "X_**",
                          choiceProbability = 0,
                          stringsAsFactors = F)
  for (r in 1:nrow(options4i.shuffled)){
    options4i.shuffled$choiceProbability[r] <- luceChoice(relu(options4i.shuffled$activations[r]), 
                                                 relu(options4i.shuffled$activations))
  }
  options4i.shuffled
  # Acc: 0.73; Prep: 0.27
  # i.e. no type-frequency effect once the model has reached asymptote. 
  # choice probability for Y (HF cue set 2 for Prep) + constant cue:
  options.activations5i.shuffled <- c(unlist(getActivations(getWM(wm_st34000_shuffled), cueset = "Y_**", select.outcomes = "Acc")),
                             unlist(getActivations(getWM(wm_st34000_shuffled), cueset = "Y_**", select.outcomes = "Prep")))
  options.choiceAlternatives <- c("Acc", "Prep")
  options5i.shuffled <- data.frame(activations = options.activations5i.shuffled,
                          choiceAlternatives = options.choiceAlternatives,
                          cues = "Y_**",
                          choiceProbability = 0,
                          stringsAsFactors = F)
  for (r in 1:nrow(options5i.shuffled)){
    options5i.shuffled$choiceProbability[r] <- luceChoice(relu(options5i.shuffled$activations[r]), 
                                                 relu(options5i.shuffled$activations))
  }
  options5i.shuffled
  # Acc: 0.26; Prep: 0.73.
  # choice probability for Z (LF cue set 2 for Prep) + constant cue:
  options.activations6i.shuffled <- c(unlist(getActivations(getWM(wm_st34000_shuffled), cueset = "Z_**", select.outcomes = "Acc")),
                             unlist(getActivations(getWM(wm_st34000_shuffled), cueset = "Z_**", select.outcomes = "Prep")))
  options.choiceAlternatives <- c("Acc", "Prep")
  options6i.shuffled <- data.frame(activations = options.activations6i.shuffled,
                          choiceAlternatives = options.choiceAlternatives,
                          cues = "Z_**",
                          choiceProbability = 0,
                          stringsAsFactors = F)
  for (r in 1:nrow(options6i.shuffled)){
    options6i.shuffled$choiceProbability[r] <- luceChoice(relu(options6i.shuffled$activations[r]), 
                                                 relu(options6i.shuffled$activations))
}
  options6i.shuffled
# extract the results for choice probabilities for correct outcome/cues. 
  cueset1_choiceprob_4000trials_acc_shuffled <- options1i.shuffled[options1i.shuffled$choiceAlternatives == "Acc", "choiceProbability"]
  cueset1_choiceprob_4000trials_prep_shuffled <- options2i.shuffled[options2i.shuffled$choiceAlternatives == "Prep", "choiceProbability"]
 HFcueset2_choiceprob_4000trials_acc_shuffled <- options3i.shuffled[options3i.shuffled$choiceAlternatives == "Acc", "choiceProbability"]
 LFcueset2_choiceprob_4000trials_acc_shuffled <- options4i.shuffled[options4i.shuffled$choiceAlternatives == "Acc", "choiceProbability"]
 HFcueset2_choiceprob_4000trials_prep_shuffled <- options5i.shuffled[options5i.shuffled$choiceAlternatives == "Prep", "choiceProbability"]
 LFcueset2_choiceprob_4000trials_prep_shuffled <- options6i.shuffled[options6i.shuffled$choiceAlternatives == "Prep", "choiceProbability"]

 # save the values:
 # save the values:
 resultstudy3.4000trials.baseline <- rbind(resultstudy3.4000trials.baseline, data.frame(cueset1_acc_4000trials_shuffled = cueset1_choiceprob_4000trials_acc_shuffled,
                                                                                        cueset1_prep_4000trials_shuffled = cueset1_choiceprob_4000trials_prep_shuffled,
                                                                                        HFcueset2_acc_4000trials_shuffled = HFcueset2_choiceprob_4000trials_acc_shuffled,
                                                                                        LFcueset2_acc_4000trials_shuffled = LFcueset2_choiceprob_4000trials_acc_shuffled,
                                                                                        HFcueset2_prep_4000trials_shuffled = HFcueset2_choiceprob_4000trials_prep_shuffled,
                                                                                        LFcueset2_prep_4000trials_shuffled = LFcueset2_choiceprob_4000trials_prep_shuffled))
}
resultstudy3.4000trials.baseline
# save the mean:
# saving the mean
mean_cueset1_acc_4000trials_shuffled <- mean(resultstudy3.4000trials.baseline$cueset1_acc_4000trials_shuffled, na.rm = TRUE)
mean_cueset1_prep_4000trials_shuffled <- mean(resultstudy3.4000trials.baseline$cueset1_prep_4000trials_shuffled, na.rm = TRUE)
mean_HFcueset2_acc_4000trials_shuffled <- mean(resultstudy3.4000trials.baseline$HFcueset2_acc_4000trials_shuffled, na.rm = TRUE)
mean_LFcueset2_acc_4000trials_shuffled <- mean(resultstudy3.4000trials.baseline$LFcueset2_acc_4000trials_shuffled, na.rm = TRUE)
mean_HFcueset2_prep_4000trials_shuffled <- mean(resultstudy3.4000trials.baseline$HFcueset2_prep_4000trials_shuffled, na.rm = TRUE)
mean_LFcueset2_prep_4000trials_shuffled <- mean(resultstudy3.4000trials.baseline$LFcueset2_prep_4000trials_shuffled, na.rm = TRUE)

# saving the sd
sd_cueset1_acc_4000trials_shuffled <- sd(resultstudy3.4000trials.baseline$cueset1_acc_4000trials_shuffled, na.rm = TRUE)
sd_cueset1_prep_4000trials_shuffled <- sd(resultstudy3.4000trials.baseline$cueset1_prep_4000trials_shuffled, na.rm = TRUE)
sd_HFcueset2_acc_4000trials_shuffled <- sd(resultstudy3.4000trials.baseline$HFcueset2_acc_4000trials_shuffled, na.rm = TRUE)
sd_LFcueset2_acc_4000trials_shuffled <- sd(resultstudy3.4000trials.baseline$LFcueset2_acc_4000trials_shuffled, na.rm = TRUE)
sd_HFcueset2_prep_4000trials_shuffled <- sd(resultstudy3.4000trials.baseline$HFcueset2_prep_4000trials_shuffled, na.rm = TRUE)
sd_LFcueset2_prep_4000trials_shuffled <- sd(resultstudy3.4000trials.baseline$LFcueset2_prep_4000trials_shuffled, na.rm = TRUE)

# save the mean for cue sets, with outcome type combined
mean_cueset1_4000trials_shuffled <- mean(c(mean_cueset1_acc_4000trials_shuffled, mean_cueset1_prep_4000trials_shuffled))
mean_HFcueset2_4000trials_shuffled <- mean(c(mean_HFcueset2_acc_4000trials_shuffled, mean_HFcueset2_prep_4000trials_shuffled))
mean_LFcueset2_4000trials_shuffled <- mean(c(mean_LFcueset2_acc_4000trials_shuffled, mean_LFcueset2_prep_4000trials_shuffled))
# check the values:
mean_cueset1_4000trials_shuffled
mean_HFcueset2_4000trials_shuffled
mean_LFcueset2_4000trials_shuffled

# compare with actual simulation with baseline models:
#  compute observed mean:
obs_acc_cueset1 <- cueset1_choiceprob_4000trials_acc
obs_acc_cueset1
# 2. use baseline means (i.e. 1000 runs): 
shuffled_acc_cueset1 <- resultstudy3.4000trials.baseline$cueset1_acc_4000trials_shuffled
shuffled_acc_cueset1
# 3. compute empirical two-tailed p-value:
p.acc.cueset1 <- mean(abs(shuffled_acc_cueset1 - mean(shuffled_acc_cueset1)) >= abs(obs_acc_cueset1 - mean(shuffled_acc_cueset1)))
p.acc.cueset1
obs_prep_cueset1   <- cueset1_choiceprob_4000trials_prep
shuffled_prep_cueset1 <- resultstudy3.4000trials.baseline$cueset1_prep_4000trials_shuffled
p.prep.cueset1     <- mean(abs(shuffled_prep_cueset1 - mean(shuffled_prep_cueset1)) >= 
                     abs(obs_prep_cueset1 - mean(shuffled_prep_cueset1)))
p.prep.cueset1
obs_acc_HFcueset2  <- HFcueset2_choiceprob_4000trials_acc
shuffled_HFacc_cueset2 <- resultstudy3.4000trials.baseline$HFcueset2_acc_4000trials_shuffled
p.acc.HFcueset2    <- mean(abs(shuffled_HFacc_cueset2 - mean(shuffled_HFacc_cueset2)) >= 
                     abs(obs_acc_HFcueset2 - mean(shuffled_HFacc_cueset2)))
p.acc.HFcueset2
obs_LFacc_cueset2  <- LFcueset2_choiceprob_4000trials_acc
shuffled_LFacc_cueset2 <- resultstudy3.4000trials.baseline$LFcueset2_acc_4000trials_shuffled
p.acc.LFcueset2    <- mean(abs(shuffled_LFacc_cueset2 - mean(shuffled_LFacc_cueset2)) >= 
                     abs(obs_LFacc_cueset2 - mean(shuffled_LFacc_cueset2)))
p.acc.LFcueset2
obs_HFprep_cueset2 <- HFcueset2_choiceprob_4000trials_prep
shuffled_HFprep_cueset2 <- resultstudy3.4000trials.baseline$HFcueset2_prep_4000trials_shuffled
p.prep.HFcueset2  <- mean(abs(shuffled_HFprep_cueset2 - mean(shuffled_HFprep_cueset2)) >= 
                     abs(obs_HFprep_cueset2 - mean(shuffled_HFprep_cueset2)))
p.prep.HFcueset2
obs_LFprep_cueset2 <- LFcueset2_choiceprob_4000trials_prep
shuffled_LFprep_cueset2 <- resultstudy3.4000trials.baseline$LFcueset2_prep_4000trials_shuffled
p.prep.LFcueset2   <- mean(abs(shuffled_LFprep_cueset2 - mean(shuffled_LFprep_cueset2)) >= 
                     abs(obs_LFprep_cueset2 - mean(shuffled_LFprep_cueset2)))
p.prep.LFcueset2
data.frame(
  Condition = c("Cueset1 Acc", "Cueset1 Prep", "HF cue set 2 Acc", "LF cue set 2 Acc", "HF cue set 2 Prep", "LF cue set 2 Prep"),
  Observed_Mean = c(obs_acc_cueset1, obs_prep_cueset1, obs_acc_HFcueset2, obs_LFacc_cueset2, obs_HFprep_cueset2, obs_LFprep_cueset2),
  P_Value = c(p.acc.cueset1, p.prep.cueset1, p.acc.HFcueset2, p.acc.LFcueset2, p.prep.HFcueset2, p.prep.LFcueset2)
)
# compare differences between cue set 1 and cue set 2:
# difference between cueset1 and HFcueset2: 
# for Acc:
obs_diff <- mean(cueset1_choiceprob_4000trials_acc) - mean(HFcueset2_choiceprob_4000trials_acc)

# Null distribution: per-replication differences under shuffling
null_diffs <- resultstudy3.4000trials.baseline$cueset1_acc_4000trials_shuffled - 
  resultstudy3.4000trials.baseline$HFcueset2_acc_4000trials_shuffled

# Two-tailed p-value
p.diff <- mean(abs(null_diffs - mean(null_diffs)) >= abs(obs_diff - mean(null_diffs)))
p.diff
# for Prep:
obs_diff <- cueset1_choiceprob_4000trials_prep - HFcueset2_choiceprob_4000trials_prep

# Null distribution: per-replication differences under shuffling
null_diffs <- resultstudy3.4000trials.baseline$cueset1_prep_4000trials_shuffled - 
  resultstudy3.4000trials.baseline$HFcueset2_prep_4000trials_shuffled

# Two-tailed p-value
p.diff <- sum(abs(null_diffs - mean(null_diffs)) >= abs(obs_diff - mean(null_diffs)))
p.diff
# for cue set 1 and LF cue set 2:
# for Acc:
obs_diff <- mean(cueset1_choiceprob_4000trials_acc) - mean(LFcueset2_choiceprob_4000trials_acc)

# Null distribution: per-replication differences under shuffling
null_diffs <- resultstudy3.4000trials.baseline$cueset1_acc_4000trials_shuffled - 
  resultstudy3.4000trials.baseline$LFcueset2_acc_4000trials_shuffled
null_diffs
count <- sum(abs(null_diffs) >= abs(obs_diff))
count
# Two-tailed p-value
p.diff <- mean(abs(null_diffs - mean(null_diffs)) >= abs(obs_diff - mean(null_diffs)))
p.diff
# for Prep:
obs_diff <- mean(cueset1_choiceprob_4000trials_prep) - mean(LFcueset2_choiceprob_4000trials_prep)

# Null distribution: per-replication differences under shuffling
null_diffs <- resultstudy3.4000trials.baseline$cueset1_prep_4000trials_shuffled - 
  resultstudy3.4000trials.baseline$LFcueset2_prep_4000trials_shuffled

# Two-tailed p-value
p.diff <- mean(abs(null_diffs - mean(null_diffs)) >= abs(obs_diff - mean(null_diffs)))
p.diff

# compare differnces between outcomes:
# i.e. confirm that outcome = not significant. 
# for cue set 1, Acc vs. Prep:
obs_diff <- mean(cueset1_choiceprob_4000trials_acc) - mean(cueset1_choiceprob_4000trials_prep)

# Null distribution: per-replication differences under shuffling
null_diffs <- resultstudy3.4000trials.baseline$cueset1_acc_4000trials_shuffled - 
  resultstudy3.4000trials.baseline$cueset1_prep_4000trials_shuffled

# Two-tailed p-value
p.diff <- mean(abs(null_diffs - mean(null_diffs)) >= abs(obs_diff - mean(null_diffs)))
p.diff
# for HF cue set 2, Acc vs. Prep:
obs_diff <- HFcueset2_choiceprob_4000trials_acc - HFcueset2_choiceprob_4000trials_prep

# Null distribution: per-replication differences under shuffling
null_diffs <- resultstudy3.4000trials.baseline$HFcueset2_acc_4000trials_shuffled - 
  resultstudy3.4000trials.baseline$HFcueset2_prep_4000trials_shuffled

# Two-tailed p-value
p.diff <- mean(abs(null_diffs - mean(null_diffs)) >= abs(obs_diff - mean(null_diffs)))
p.diff

# for LF cue set 2, Acc vs. Prep:
obs_diff <- LFcueset2_choiceprob_4000trials_acc - LFcueset2_choiceprob_4000trials_prep
# Null distribution: per-replication differences under shuffling
null_diffs <- resultstudy3.4000trials.baseline$LFcueset2_acc_4000trials_shuffled - 
  resultstudy3.4000trials.baseline$LFcueset2_prep_4000trials_shuffled

# Two-tailed p-value
p.diff <- mean(abs(null_diffs - mean(null_diffs)) >= abs(obs_diff - mean(null_diffs)))
p.diff

