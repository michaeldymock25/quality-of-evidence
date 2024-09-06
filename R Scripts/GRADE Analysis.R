
## This document contains the analyses of the GRADE assessments for the "What is the quality of evidence informing vaccine policy recommendations in Australia?" manuscript

library(data.table)
library(stringr)
library(ggplot2)

## colourblind friendly palette

colours <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733")

## Read in the data (note the below scripts containing the data must be located in the same working directory as this analysis script)

source("dat_cholera.R")
source("dat_DTP.R")
source("dat_HPV.R")
source("dat_influenza.R")
source("dat_meningococcal.R")
source("dat_pneumococcal.R")
source("dat_rabies.R")
source("dat_zoster.R")

dat <- list(dat_cholera, dat_DTP, dat_HPV, dat_influenza, dat_meningococcal, dat_pneumococcal, dat_rabies, dat_zoster)
areas_l <- c("Cholera", "DTP", "HPV", "Influenza", "Meningococcal", "Pneumococcal", "Rabies", "Zoster")
names(dat) <- areas_l

## list of reasons for GRADE assessment initiation

unlist(lapply(dat, function(d) d$Initiation))

## summary of certainty ratings (quality of evidence)

certainty <- rbindlist(lapply(areas_l, function(x)
                          rbindlist(lapply(dat[[x]]$Outcomes, function(question)
                                       data.table(certainty = sapply(question, function(out) out$Certainty),
                                                  outcome = names(question))),
                                    idcol = "question")),
                       idcol = "area")
certainty[, `:=`(
                 type = factor(ifelse(str_detect(tolower(outcome), "adverse events|fever|pain|fatigue|cardiovascular"),
                                      "Safety", "Efficacy"),
                               levels = c("Efficacy", "Safety")),
                 question = paste0(str_sub(areas_l, 1, 1)[area], str_sub(question, 2, 2)),
                 area = factor(area, labels = areas_l),
                 certainty = factor(certainty, levels = c("Very low", "Low", "Moderate", "High"))
)]
certainty[, table(certainty)]

overall_certainty <- rbindlist(lapply(areas_l, function(x)
                                  rbindlist(lapply(dat[[x]]$`Evidence to decision`, function(question)
                                               data.table(certainty = question["Overall certainty"])),
                                            idcol = "Question")),
                               idcol = "area")
overall_certainty[, area := factor(area, labels = areas_l)]
overall_certainty[, certainty := factor(certainty, levels = c("Very low", "Low", "Moderate", "High"))]
overall_certainty[, table(certainty)]
overall_certainty

## Figure 1

jpeg("Figure 1.jpeg", width = 6, height = 7, units = "in", res = 1000)

ggplot(certainty[, .N, by = .(certainty, question, area)], aes(x = question, y = N, fill = certainty)) +
  facet_wrap(~area, scales = "free_x") +
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity") +
  scale_x_discrete("Research Question") +
  scale_y_continuous("Number of Outcomes", n.breaks = 7) +
  #scale_fill_grey("Certainty of Evidence", start = 0, end = 0.9) +
  scale_fill_manual("Certainty of Evidence", values = colours) +
  theme_bw() +
  theme(legend.position = "bottom", panel.grid.major.x = element_blank())

dev.off()

## Figure 2

jpeg("Figure 2.jpeg", width = 6, height = 7, units = "in", res = 1000)

ggplot(certainty[, .N, by = .(certainty, type, area)], aes(x = type, y = N, fill = certainty)) +
  facet_wrap(~area, scales = "free_x") +
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity") +
  scale_x_discrete("Outcome Category") +
  scale_y_continuous("Number of Outcomes", n.breaks = 7) +
  #scale_fill_grey("Certainty of Evidence", start = 0, end = 0.9) +
  scale_fill_manual("Certainty of Evidence", values = colours) +
  theme_bw() +
  theme(legend.position = "bottom", panel.grid.major.x = element_blank())

dev.off()

## summary of question types

questions <- rbindlist(lapply(areas_l, function(x)
                          data.table(question = dat[[x]]$Question,
                                     certainty = sapply(dat[[x]]$`Evidence to decision`, function(question) question["Overall certainty"]))),
                       idcol = "area")
questions[, `:=`(
                 area = factor(area, labels = areas_l),
                 age = factor(ifelse(str_detect(tolower(question), "infants"), "Infants",
                              ifelse(str_detect(tolower(question), "adolescents"), "Adolescents",
                              ifelse(str_detect(tolower(question), "over 18"), "Adults",
                              ifelse(str_detect(tolower(question), "over 50|over 65|over 70"), "Older adults",
                              ifelse(str_detect(tolower(question), "children aged 2-17"), "Children and adolescents",
                              ifelse(str_detect(tolower(question), "children and adults"), "Children and adults", NA)))))),
                              levels = c("Infants", "Children and adolescents", "Children and adults",
                                         "Adolescents", "Adults", "Older adults")),
                 risk = factor(ifelse(str_detect(tolower(question), "standard risk|without underlying risk|immunocompetent"),
                                      "Standard risk of disease",
                               ifelse(str_detect(tolower(question), "increased risk|with underlying risk|immunocompromised|high risk of exposure|indicated to receive rabies pre-exposure"), "Increased risk of disease", NA)),
                               levels = c("Standard risk of disease", "Increased risk of disease")),
                 indig = factor(ifelse(str_detect(tolower(question), "non-indigenous"), "Non-Indigenous",
                                ifelse(str_detect(tolower(question), "indigenous"), "Indigenous", NA)),
                                levels = c("Non-Indigenous", "Indigenous")),
                 certainty = factor(certainty, levels = c("Very low", "Low", "Moderate", "High"))
)]
questions[,table(certainty, age)]
questions[,table(certainty, risk)]
questions[,table(certainty, indig)]

## summary of grades

grade <- rbindlist(lapply(areas_l, function(x)
                      rbindlist(lapply(dat[[x]]$Outcomes, function(question)
                                   data.table(`Risk of Bias` = sapply(question, function(out) out$`Risk of Bias`[["Rating"]]),
                                              Inconsistency = sapply(question, function(out) out$Inconsistency[["Rating"]]),
                                              Indirectness = sapply(question, function(out) out$Indirectness[["Rating"]]),
                                              Imprecision = sapply(question, function(out) out$Imprecision[["Rating"]]),
                                              certainty = sapply(question, function(out) out$Certainty),
                                              outcome = names(question))),
                                idcol = "question")),
                   idcol = "area")
grade <- melt(grade, id.vars = c("area", "question", "outcome", "certainty"),
              measure.vars = c("Risk of Bias", "Inconsistency", "Indirectness", "Imprecision"))
grade[, `:=`(
             question = paste0(str_sub(areas_l, 1, 1)[area], str_sub(question, 2, 2)),
             area = factor(area, labels = areas_l),
             certainty = factor(certainty, levels = c("Very low", "Low", "Moderate", "High")),
             variable = factor(variable, levels = c("Risk of Bias", "Inconsistency", "Indirectness", "Imprecision")),
             value = factor(ifelse(value %in% c("Not serious", "Not assessed"), "Nil",
                            ifelse(value == "Serious", "One",
                            ifelse(value %in% c("Extremely serious", "Very serious"), "Two", NA))),
                            levels = c("Two", "One", "Nil"))
)]

## Figure 3

jpeg("Figure 3.jpeg", width = 6, height = 7, units = "in", res = 1000)

ggplot(grade, aes(x = variable, y = value, group = interaction(outcome, Question), colour = certainty)) +
  facet_wrap(~area, ncol = 2) +
  geom_line(position = position_jitter(width = 0, height = 0.2)) +
  scale_x_discrete("Assessment Category", guide = guide_axis(n.dodge = 2)) +
  ylab("Number of levels downgraded") +
  scale_colour_manual("Certainty of Evidence", values = colours) +
  theme_bw() +
  theme(legend.position = "bottom")

dev.off()

## Figure S1

jpeg("Figure S1.jpeg", width = 6, height = 5, units = "in", res = 1000)

ggplot(grade[area == "HPV"], aes(x = variable, y = value, group = outcome, colour = certainty)) +
  facet_wrap(~question, ncol = 2) +
  geom_line(position = position_jitter(width = 0, height = 0.03)) +
  scale_x_discrete("Assessment Category", guide = guide_axis(n.dodge = 2)) +
  ylab("Number of levels downgraded") +
  scale_colour_manual("Certainty of Evidence", values = colours, drop = FALSE) +
  theme_bw() +
  theme(legend.position = "bottom")

dev.off()

## Figure S2

jpeg("Figure S2.jpeg", width = 6, height = 6, units = "in", res = 1000)

ggplot(grade[area == "Influenza"], aes(x = variable, y = value, group = outcome, colour = certainty)) +
  facet_wrap(~question, ncol = 2) +
  geom_line(position = position_jitter(width = 0, height = 0.1)) +
  scale_x_discrete("Assessment Category", guide = guide_axis(n.dodge = 2)) +
  ylab("Number of levels downgraded") +
  scale_colour_manual("Certainty of Evidence", values = colours, drop = FALSE) +
  theme_bw() +
  theme(legend.position = "bottom")

dev.off()

## Figure S3

jpeg("Figure S3.jpeg", width = 6, height = 5, units = "in", res = 1000)

ggplot(grade[area == "Meningococcal"], aes(x = variable, y = value, group = outcome, colour = certainty)) +
  facet_wrap(~question, ncol = 2) +
  geom_line(position = position_jitter(width = 0, height = 0.1)) +
  scale_x_discrete("Assessment Category", guide = guide_axis(n.dodge = 2)) +
  ylab("Number of levels downgraded") +
  scale_colour_manual("Certainty of Evidence", values = colours, drop = FALSE) +
  theme_bw() +
  theme(legend.position = "bottom")

dev.off()

## Figure S4

jpeg("Figure S4.jpeg", width = 6, height = 6, units = "in", res = 1000)

ggplot(grade[area == "Pneumococcal"], aes(x = variable, y = value, group = outcome, colour = certainty)) +
  facet_wrap(~question, ncol = 2) +
  geom_line(position = position_jitter(width = 0, height = 0.15)) +
  scale_x_discrete("Assessment Category", guide = guide_axis(n.dodge = 2)) +
  ylab("Number of levels downgraded") +
  scale_colour_manual("Certainty of Evidence", values = colours, drop = FALSE) +
  theme_bw() +
  theme(legend.position = "bottom")

dev.off()

## Figure S5

jpeg("Figure S5.jpeg", width = 6, height = 5, units = "in", res = 1000)

ggplot(grade[area == "Rabies"], aes(x = variable, y = value, group = outcome, colour = certainty)) +
  facet_wrap(~question, ncol = 2) +
  geom_line(position = position_jitter(width = 0, height = 0.08)) +
  scale_x_discrete("Assessment Category", guide = guide_axis(n.dodge = 2)) +
  ylab("Number of levels downgraded") +
  scale_colour_manual("Certainty of Evidence", values = colours, drop = FALSE) +
  theme_bw() +
  theme(legend.position = "bottom")

dev.off()

## Figure S6

jpeg("Figure S6.jpeg", width = 6, height = 5, units = "in", res = 1000)

ggplot(grade[area == "Zoster"], aes(x = variable, y = value, group = outcome, colour = certainty)) +
  facet_wrap(~question, ncol = 2) +
  geom_line(position = position_jitter(width = 0, height = 0.08)) +
  scale_x_discrete("Assessment Category", guide = guide_axis(n.dodge = 2)) +
  ylab("Number of levels downgraded") +
  scale_colour_manual("Certainty of Evidence", values = colours, drop = FALSE) +
  theme_bw() +
  theme(legend.position = "bottom")

dev.off()

## summary of reasons for downgrading risk of bias

bias <- rbindlist(lapply(areas_l, function(x)
                     rbindlist(lapply(dat[[x]]$Outcomes, function(question)
                                  data.table(reason = sapply(question, function(out) out$`Risk of Bias`[["Reason"]]))),
                               idcol = "Question")),
                  idcol = "area")
bias[, area := factor(area, labels = areas_l)]
bias[reason != "None", .N]
bias[reason != "None", table(reason)]

## summary of reasons for downgrading inconsistency

inconsistency <- rbindlist(lapply(areas_l, function(x)
                              rbindlist(lapply(dat[[x]]$Outcomes, function(question)
                                           data.table(reason = sapply(question, function(out) out$Inconsistency[["Reason"]]))),
                                        idcol = "Question")),
                           idcol = "area")
inconsistency[, area := factor(area, labels = areas_l)]
inconsistency[reason != "None", .N]
inconsistency[reason != "None", table(reason)]

## summary of reasons for downgrading indirectness

indirectness <- rbindlist(lapply(areas_l, function(x)
                             rbindlist(lapply(dat[[x]]$Outcomes, function(question)
                                          data.table(reason = sapply(question, function(out) out$Indirectness[["Reason"]]))),
                                       idcol = "Question")),
                          idcol = "area")
indirectness[, area := factor(area, labels = areas_l)]
indirectness[reason != "None", .N]
indirectness[reason != "None", table(reason)]

## summary of reasons for downgrading imprecision

imprecision <- rbindlist(lapply(areas_l, function(x)
                            rbindlist(lapply(dat[[x]]$Outcomes, function(question)
                                         data.table(reason = sapply(question, function(out) out$Imprecision[["Reason"]]))),
                                      idcol = "Question")),
                         idcol = "area")
imprecision[, area := factor(area, labels = areas_l)]
imprecision[reason != "None", .N]
imprecision[reason != "None", table(reason)]

## summary of reasons for upgrading due to Large Effect Magnitude, Clear Dose-Response Gradient or Reverse Residual Confounding
## note that "Strong association" and "Very strong association" are to be interpreted as a Large Effect Magnitude

other_considerations <- rbindlist(lapply(areas_l, function(x)
                                     rbindlist(lapply(dat[[x]]$Outcomes, function(question)
                                                  data.table(reason = sapply(question, function(out) out$Other),
                                                             outcome = names(question))),
                                               idcol = "Question")),
                                  idcol = "area")
other_considerations[, area := factor(area, labels = areas_l)]
other_considerations[reason != "None", table(reason)]

## the below code shows that in each instance where there were other considerations that contributed to upgrade the evidence, i.e., not none
## the upgrade did not change the quality of evidence rating (certainty):
## for 1) Risk of Bias, Inconsistency, Indirectness and Imprecision were all serious and so the Certainty was so extremely low that the
## strong association was not enough to improve the rating
## for 2-6) the rating was already high and so could not be improved

rbindlist(lapply(1:other_considerations[reason != "None", .N], function(i)
  other_considerations[reason != "None"][i, dat[[area]]$Outcomes[[Question]][[outcome]]][1]))

## summary of directions of effect

direction <- rbindlist(lapply(areas_l, function(x)
                          rbindlist(lapply(dat[[x]]$Outcomes, function(question)
                                       data.table(direction = sapply(question, function(out) out$Direction))),
                                    idcol = "Question")),
                       idcol = "area")
direction[, area := factor(area, labels = areas_l)]
direction[, table(direction)]

## summary of importance of outcomes

importance <- rbindlist(lapply(areas_l, function(x)
                           rbindlist(lapply(dat[[x]]$Outcomes, function(question)
                                        data.table(importance = sapply(question, function(out) out$Importance))),
                                     idcol = "Question")),
                        idcol = "area")
importance[, area := factor(area, labels = areas_l)]
importance[, table(importance)]

