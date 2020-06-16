##### Minimal Reproducible Analysis Example
##### 3 - analysis

library(tidyverse)
library(readxl)
library(emmeans)

##### Load data ------------------------------------------------------------------------------------

d <- read_excel("./data/processed/fertilizer_trial_WUR_cleaned.xlsx",
                sheet = "data")
# Replace unit separator $ => _
# $ is not a syntactic character in R
names(d) <- str_replace_all(names(d), "\\$", "_")

##### Analysis -------------------------------------------------------------------------------------

## Simple summary graph
summary_plot <- ggplot(d)+ # initialize graph
  aes(x = fertilizer, y = yield_tha, colour = farm)+ # define relation between graph properties and variables
  geom_point(na.rm = TRUE)+ # define plot type, here scatter plot
  facet_wrap(farm ~ .)+ # allow faceting => each farm corresponds to one pane
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+ # rotate axis labels
  ylim(0, 15)+ # set y axis limits
  ggtitle("Summary plot") # add title

# Display the summary plot
summary_plot

# Save the summary plot
ggsave(filename = "./results/figures/summary_plot.png",
       plot = summary_plot)


## Fit a simple linear model
fit <- lm(yield_tha ~ farm + fertilizer, data = d)

## Get an ANOVA table
anova(fit)

## Calculate Estimated Marginal Means, aka adjusted means:
## These are the mean yield for each combination of farm and fertilizer
## as the model predicts them.
emms <- emmeans(fit, ~  farm + fertilizer)

## Fancy graph: combining raw data and adjusted means and their corresponding
## confidence interval

emms_df <- as.data.frame(emms)

emms_plot <- ggplot(d)+
  aes(x = fertilizer, y = yield_tha, colour = farm)+ # raw data
  geom_point(na.rm = TRUE,
             position = position_dodge(width = 0.2),
             alpha = 0.2)+
  geom_pointrange(data = emms_df, # Estimated Marginal Means
                  aes(y = emmean, ymin = lower.CL, ymax = upper.CL),
                  position = position_dodge(width = 0.2))+
  ylim(0, 15)+
  ggtitle("Estimated Marginal Means")

# Display emms plots
emms_plot

# Save the fancy plot
ggsave(filename = "./results/figures/emms_plot.png",
       plot = emms_plot)
