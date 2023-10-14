library(ggplot2)
library(ggthemes)
library(dplyr)
set.seed(123)

# Extra tweaks for plotting
plot_theme <- theme(
    plot.title = element_text(
        size = 20, face = "bold", hjust = 0,
        margin = margin(b = 0.25, unit = "cm")),
    plot.subtitle = element_text(
        size = 14, hjust = 0),
    axis.title.x = element_text(
        size = 12, face = "bold", angle = 0,
        margin = margin(t = 0.25, unit = "cm")),
    axis.title.y = element_text(
        size = 12, face = "bold", angle = 90,
        margin = margin(r = 0.25, unit = "cm")),
    axis.text = element_text(
        size = 12),
    legend.title = element_text(
        size = 14, face = "bold"),
    legend.text = element_text(
        size = 14),
    legend.position = "top",
    legend.justification = "left"
)

terminal_survival <- function(p_int, p_k = 0.9, silos = 400, attack_size) { # nolint
    p_int <- rnorm(1, p_int, sd = 0.01)
    destroyed <- (attack_size * (1 - p_int) * p_k)
    return(destroyed)
}

terminal_surv <- tibble(
    efficiency = seq(0, 1, length.out = 1000),
    silos_1 = sapply(
        efficiency, terminal_survival, attack_size = 500),
    silos_2 = sapply(
        efficiency, terminal_survival, attack_size = 1000)
) %>%
    mutate(
        silos_1 = ifelse(silos_1 > 400, 400, silos_1) / 400 * 100,
        silos_2 = ifelse(silos_2 > 400, 400, silos_2) / 400 * 100
)

colors_3 <- c("500 RVs" = "#593380", "1000 RVs" = "#F60552")

terminal_surv_chart <- ggplot(
    data = terminal_surv,
    aes(x = efficiency)
) +
    geom_line(
        aes(y = silos_2, color = "1000 RVs"),
        linewidth = 1.0
) +
    geom_line(
        aes(y = silos_1, color = "500 RVs"),
        linewidth = 1.0
) +
    labs(
        x = "Intercept Probability",
        y = "Percentage of Silos Destroyed",
        title = "Effect of Different Intercept Probabilities on Silo Survivability", # nolint,
        subtitle = "For a Hypothetical Terminal BMD System; ICBM Reliability = 0.9", # nolint
        color = "Attack Size"
) +
    scale_color_manual(
        values = colors_3
) +
    theme_fivethirtyeight() +
    plot_theme

print(terminal_surv_chart)

overlay_survival <- function(p_under, p_over = 0.8, p_k = 0.9, silos = 400, attack_size) { # nolint
    p_under <- rnorm(1, p_under, sd = 0.01)
    destroyed <- attack_size * (1 - p_over) * (1 - p_under) * p_k
    return(destroyed)
}

overlay_surv <- tibble(
    efficiency = seq(0, 1, length.out = 1000),
    silos_1 = sapply(
        efficiency, overlay_survival, attack_size = 500),
    silos_2 = sapply(
        efficiency, overlay_survival, attack_size = 1000)
) %>%
    mutate(
        silos_1 = ifelse(silos_1 > 400, 400, silos_1) / 400 * 100,
        silos_2 = ifelse(silos_2 > 400, 400, silos_2) / 400 * 100
)

overlay_surv_chart <- ggplot(
    data = overlay_surv,
    aes(x = efficiency)
) +
    geom_line(
        aes(y = silos_2, color = "1000 RVs"),
        linewidth = 1.0
) +
    geom_line(
        aes(y = silos_1, color = "500 RVs"),
        linewidth = 1.0
) +
    labs(
        x = "Intercept Probability",
        y = "Percentage of Silos Destroyed",
        title = "Effect of Different Intercept Probabilities on Silo Survivability", # nolint,
        subtitle = "For a Hypothetical Layered BMD System; Overlay Intercept Probability = 0.8, ICBM Reliability = 0.9", # nolint
        color = "Attack Size"
) +
    scale_color_manual(
        values = colors_3
) +
    theme_fivethirtyeight() +
    plot_theme

print(overlay_surv_chart)