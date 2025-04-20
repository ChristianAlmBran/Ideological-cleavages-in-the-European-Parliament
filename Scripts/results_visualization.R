pacman::p_load(tidyverse)

# Loading csv files
results_mlr <- read.csv2("results_abcp_all.csv")
results_mlr_leg <- read.csv2("results_abcp_leg.csv")

# 
results_mlr$predicted_prob <- as.numeric(results_mlr$predicted_prob)
results_mlr$ci_lower <- as.numeric(results_mlr$ci_lower)
results_mlr$ci_upper <- as.numeric(results_mlr$ci_upper)
results_mlr$final_vote <- as.factor(results_mlr$final_vote)
results_mlr$mean <- as.factor(results_mlr$mean)
results_mlr$only_leg <- as.factor(results_mlr$only_leg)

# Separing the dfs
results_mlr_eu_position <- results_mlr %>%
  filter(independent_vari == "eu_position")

# Plot for eu_position
ggplot(results_mlr_eu_position, aes(x = parliament_session, y = predicted_prob, 
                                    color = environment, shape = factor(final_vote))) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +  # Adjust size for better visibility
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, 
                position = position_dodge(width = 0.5)) +
  labs(
    title = "Difference in Predicted Probabilities eu_position",
    x = "Parliament Session",
    y = "Difference in Predicted Probabilities",
    shape = "Final Vote"
  ) +
  scale_shape_manual(values = c("0" = 17, "1" = 16)) +  # Triangle for "0", Circle for "1"
  ylim(0.25, 1.00) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("results_mlr_eu_position.png", width = 28, height = 20, units = "cm", dpi = 300)

# Separing the dfs
results_mlr_galtan <- results_mlr %>%
  filter(independent_vari == "galtan")

# Plot for galtan
ggplot(results_mlr_galtan, aes(x = parliament_session, y = predicted_prob, 
                                    color = environment, shape = factor(final_vote))) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +  # Adjust size for better visibility
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, 
                position = position_dodge(width = 0.5)) +
  labs(
    title = "Difference in Predicted Probabilities galtan",
    x = "Parliament Session",
    y = "Difference in Predicted Probabilities",
    shape = "Final Vote"
  ) +
  scale_shape_manual(values = c("0" = 17, "1" = 16)) +  # Triangle for "0", Circle for "1"
  ylim(0.25, 1.00) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("results_mlr_galtan.png", width = 28, height = 20, units = "cm", dpi = 300)

# Separing the dfs
results_mlr_lrecon <- results_mlr %>%
  filter(independent_vari == "lrecon")

# Plot for lrecon
ggplot(results_mlr_lrecon, aes(x = parliament_session, y = predicted_prob, 
                               color = environment, shape = factor(final_vote))) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +  # Adjust size for better visibility
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, 
                position = position_dodge(width = 0.5)) +
  labs(
    title = "Difference in Predicted Probabilities lrecon",
    x = "Parliament Session",
    y = "Difference in Predicted Probabilities",
    shape = "Final Vote"
  ) +
  scale_shape_manual(values = c("0" = 17, "1" = 16)) +  # Triangle for "0", Circle for "1"
  ylim(0.0, 1.00) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("results_mlr_lrecon.png", width = 28, height = 20, units = "cm", dpi = 300)

# Separing the dfs
results_mlr_final_1 <- results_mlr %>%
  filter(final_vote == 1)

ggplot(results_mlr_final_1, aes(x = parliament_session, y = predicted_prob, color = environment)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, position = position_dodge(width = 0.5)) +
  facet_wrap(~independent_vari) +  # Fixed typo in variable name
  labs(
    title = "Difference in Predicted Probabilities final_vote = 1",
    x = "Parliament Session",
    y = "Predicted Probability"
  ) +
  ylim(0.0, 1.00) +  # Ensure all predicted_prob values fall within this range
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("results_mlr_final_1.png", width = 28, height = 20, units = "cm", dpi = 300)


# Separing the dfs
results_mlr_final_0 <- results_mlr %>%
  filter(final_vote == 0)

ggplot(results_mlr_final_0, aes(x = parliament_session, y = predicted_prob, color = environment)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, position = position_dodge(width = 0.5)) +
  facet_wrap(~independent_vari) +  # Fixed typo in variable name
  labs(
    title = "Difference in Predicted Probabilities final_vote = 0",
    x = "Parliament Session",
    y = "Predicted Probability"
  ) +
  ylim(0.0, 1.00) +  # Ensure all predicted_prob values fall within this range
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("results_mlr_final_0.png", width = 28, height = 20, units = "cm", dpi = 300)


# 
results_mlr_leg$predicted_prob <- as.numeric(results_mlr_leg$predicted_prob)
results_mlr_leg$ci_lower <- as.numeric(results_mlr_leg$ci_lower)
results_mlr_leg$ci_upper <- as.numeric(results_mlr_leg$ci_upper)
results_mlr_leg$final_vote <- as.factor(results_mlr_leg$final_vote)
results_mlr_leg$mean <- as.factor(results_mlr_leg$mean)
results_mlr_leg$only_leg <- as.factor(results_mlr_leg$only_leg)


# Separing the dfs
results_mlr_leg_eu_position <- results_mlr_leg %>%
  filter(independent_vari == "eu_position")

# Plot for eu_position
ggplot(results_mlr_leg_eu_position, aes(x = parliament_session, y = predicted_prob, 
                                    color = environment, shape = factor(final_vote))) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +  # Adjust size for better visibility
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, 
                position = position_dodge(width = 0.5)) +
  labs(
    title = "Difference in Predicted Probabilities eu_position only leg",
    x = "Parliament Session",
    y = "Difference in Predicted Probabilities",
    shape = "Final Vote"
  ) +
  scale_shape_manual(values = c("0" = 17, "1" = 16)) +  # Triangle for "0", Circle for "1"
  ylim(0.25, 1.00) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("results_mlr_leg_eu_position.png", width = 28, height = 20, units = "cm", dpi = 300)

# Separing the dfs
results_mlr_leg_galtan <- results_mlr_leg %>%
  filter(independent_vari == "galtan")

# Plot for galtan
ggplot(results_mlr_leg_galtan, aes(x = parliament_session, y = predicted_prob, 
                               color = environment, shape = factor(final_vote))) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +  # Adjust size for better visibility
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, 
                position = position_dodge(width = 0.5)) +
  labs(
    title = "Difference in Predicted Probabilities galtan only leg",
    x = "Parliament Session",
    y = "Difference in Predicted Probabilities",
    shape = "Final Vote"
  ) +
  scale_shape_manual(values = c("0" = 17, "1" = 16)) +  # Triangle for "0", Circle for "1"
  ylim(0.25, 1.00) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("results_mlr_leg_galtan.png", width = 28, height = 20, units = "cm", dpi = 300)

# Separing the dfs
results_mlr_leg_lrecon <- results_mlr_leg %>%
  filter(independent_vari == "lrecon")

# Plot for lrecon
ggplot(results_mlr_leg_lrecon, aes(x = parliament_session, y = predicted_prob, 
                               color = environment, shape = factor(final_vote))) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +  # Adjust size for better visibility
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, 
                position = position_dodge(width = 0.5)) +
  labs(
    title = "Difference in Predicted Probabilities lrecon only leg",
    x = "Parliament Session",
    y = "Difference in Predicted Probabilities",
    shape = "Final Vote"
  ) +
  scale_shape_manual(values = c("0" = 17, "1" = 16)) +  # Triangle for "0", Circle for "1"
  ylim(0.0, 1.00) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("results_mlr_leg_lrecon.png", width = 28, height = 20, units = "cm", dpi = 300)

# Separing the dfs
results_mlr_leg_final_1 <- results_mlr_leg %>%
  filter(final_vote == 1)

ggplot(results_mlr_leg_final_1, aes(x = parliament_session, y = predicted_prob, color = environment)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, position = position_dodge(width = 0.5)) +
  facet_wrap(~independent_vari) +  # Fixed typo in variable name
  labs(
    title = "Difference in Predicted Probabilities final_vote = 1 only leg",
    x = "Parliament Session",
    y = "Predicted Probability"
  ) +
  ylim(0.0, 1.00) +  # Ensure all predicted_prob values fall within this range
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("results_mlr_leg_final_1.png", width = 28, height = 20, units = "cm", dpi = 300)


# Separing the dfs
results_mlr_leg_final_0 <- results_mlr_leg %>%
  filter(final_vote == 0)

ggplot(results_mlr_leg_final_0, aes(x = parliament_session, y = predicted_prob, color = environment)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, position = position_dodge(width = 0.5)) +
  facet_wrap(~independent_vari) +  # Fixed typo in variable name
  labs(
    title = "Difference in Predicted Probabilities final_vote = 0 only leg",
    x = "Parliament Session",
    y = "Predicted Probability"
  ) +
  ylim(0.0, 1.00) +  # Ensure all predicted_prob values fall within this range
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("results_mlr_leg_final_0.png", width = 28, height = 20, units = "cm", dpi = 300)


# Separing the dfs
results_mlr_allandleg <- rbind(results_mlr, results_mlr_leg)
results_mlr_allandleg_final_1_env <- results_mlr_allandleg %>%
  filter(final_vote == 1, environment == "Environment")

results_mlr_allandleg_final_1_env <- results_mlr_allandleg_final_1_env %>%
  mutate(only_leg = factor(only_leg, levels = c(0, 1), labels = c("Non-Legislative", "Legislative")))

ggplot(results_mlr_allandleg_final_1_env, aes(x = parliament_session, y = predicted_prob, color = only_leg)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, position = position_dodge(width = 0.5)) +
  facet_wrap(~independent_vari) +  # Fixed typo in variable name
  labs(
    title = "Difference in Predicted Probabilities final_vote = 1 Environment",
    x = "Parliament Session",
    y = "Predicted Probability"
  ) +
  ylim(0.0, 1.00) +  # Ensure all predicted_prob values fall within this range
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("results_mlr_allandleg_final_1_env.png", width = 28, height = 20, units = "cm", dpi = 300)

results_mlr_allandleg_final_0_env <- results_mlr_allandleg %>%
  filter(final_vote == 0, environment == "Environment")
results_mlr_allandleg_final_0_env <- results_mlr_allandleg_final_0_env %>%
  mutate(only_leg = factor(only_leg, levels = c(0, 1), labels = c("Non-Legislative", "Legislative")))

ggplot(results_mlr_allandleg_final_0_env, aes(x = parliament_session, y = predicted_prob, color = only_leg)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, position = position_dodge(width = 0.5)) +
  facet_wrap(~independent_vari) +  # Fixed typo in variable name
  labs(
    title = "Difference in Predicted Probabilities final_vote = 0 Environment",
    x = "Parliament Session",
    y = "Predicted Probability"
  ) +
  ylim(0.0, 1.00) +  # Ensure all predicted_prob values fall within this range
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("results_mlr_allandleg_final_0_env.png", width = 28, height = 20, units = "cm", dpi = 300)


results_mlr_allandleg_final_1_non_env <- results_mlr_allandleg %>%
  filter(final_vote == 1, environment == "Non-environment")
results_mlr_allandleg_final_1_non_env <- results_mlr_allandleg_final_1_non_env %>%
  mutate(only_leg = factor(only_leg, levels = c(0, 1), labels = c("Non-Legislative", "Legislative")))

ggplot(results_mlr_allandleg_final_1_non_env, aes(x = parliament_session, y = predicted_prob, color = only_leg)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, position = position_dodge(width = 0.5)) +
  facet_wrap(~independent_vari) +  # Fixed typo in variable name
  labs(
    title = "Difference in Predicted Probabilities final_vote = 1 Non-environment",
    x = "Parliament Session",
    y = "Predicted Probability"
  ) +
  ylim(0.0, 1.00) +  # Ensure all predicted_prob values fall within this range
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("results_mlr_allandleg_final_1_non_env.png", width = 28, height = 20, units = "cm", dpi = 300)


results_mlr_allandleg_final_0_non_env <- results_mlr_allandleg %>%
  filter(final_vote == 0, environment == "Non-environment")
results_mlr_allandleg_final_0_non_env <- results_mlr_allandleg_final_0_non_env %>%
  mutate(only_leg = factor(only_leg, levels = c(0, 1), labels = c("Non-Legislative", "Legislative")))

ggplot(results_mlr_allandleg_final_0_non_env, aes(x = parliament_session, y = predicted_prob, color = only_leg)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, position = position_dodge(width = 0.5)) +
  facet_wrap(~independent_vari) +  # Fixed typo in variable name
  labs(
    title = "Difference in Predicted Probabilities final_vote = 0 Non-environment",
    x = "Parliament Session",
    y = "Predicted Probability"
  ) +
  ylim(0.0, 1.00) +  # Ensure all predicted_prob values fall within this range
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("results_mlr_allandleg_final_0_non_env.png", width = 28, height = 20, units = "cm", dpi = 300)


# Separing the dfs
results_mlr_leg_eu_position <- results_mlr_leg %>%
  filter(independent_vari == "eu_position")

# Plot for eu_position
ggplot(results_mlr_leg_eu_position, aes(x = parliament_session, y = predicted_prob, 
                                    color = environment, shape = factor(final_vote))) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +  # Adjust size for better visibility
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, 
                position = position_dodge(width = 0.5)) +
  labs(
    title = "Difference in Predicted Probabilities eu_position leg",
    x = "Parliament Session",
    y = "Difference in Predicted Probabilities",
    shape = "Final Vote"
  ) +
  scale_shape_manual(values = c("0" = 17, "1" = 16)) +  # Triangle for "0", Circle for "1"
  ylim(0.25, 1.00) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("results_mlr_leg_eu_position.png", width = 28, height = 20, units = "cm", dpi = 300)

# Separing the dfs
results_mlr_leg_galtan <- results_mlr_leg %>%
  filter(independent_vari == "galtan")

# Plot for galtan
ggplot(results_mlr_leg_galtan, aes(x = parliament_session, y = predicted_prob, 
                               color = environment, shape = factor(final_vote))) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +  # Adjust size for better visibility
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, 
                position = position_dodge(width = 0.5)) +
  labs(
    title = "Difference in Predicted Probabilities galtan leg",
    x = "Parliament Session",
    y = "Difference in Predicted Probabilities",
    shape = "Final Vote"
  ) +
  scale_shape_manual(values = c("0" = 17, "1" = 16)) +  # Triangle for "0", Circle for "1"
  ylim(0.25, 1.00) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("results_mlr_leg_galtan.png", width = 28, height = 20, units = "cm", dpi = 300)

# Separing the dfs
results_mlr_leg_lrecon <- results_mlr_leg %>%
  filter(independent_vari == "lrecon")

# Plot for lrecon
ggplot(results_mlr_leg_lrecon, aes(x = parliament_session, y = predicted_prob, 
                               color = environment, shape = factor(final_vote))) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +  # Adjust size for better visibility
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, 
                position = position_dodge(width = 0.5)) +
  labs(
    title = "Difference in Predicted Probabilities lrecon",
    x = "Parliament Session",
    y = "Difference in Predicted Probabilities",
    shape = "Final Vote"
  ) +
  scale_shape_manual(values = c("0" = 17, "1" = 16)) +  # Triangle for "0", Circle for "1"
  ylim(0.0, 1.00) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("results_mlr_leg_lrecon.png", width = 28, height = 20, units = "cm", dpi = 300)
