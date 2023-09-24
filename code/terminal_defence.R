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

# Terminal defence
terminal_defence <- function(intercept, p_k, silos, attack_size) { # nolint
    # Generate sequence of decreasing probabilities
    probs <- rnorm(attack_size, intercept, 0.01) %>%
        sort(decreasing = TRUE)

    # Pre-allocate space
    intercepted <- double(length = attack_size)
    leakage <- double(length = attack_size)
    surviving_silos <- double(length = attack_size) # nolint

    # Simulate the attack
    for (i in 1:attack_size) { # nolint
        intercept_prob <- rnorm(1, probs[i], 0.005)
        intercepted[i] <- i * intercept_prob
        leakage[i] <- i * (1 - intercept_prob)
        surviving_silos[i] <- silos - leakage[i] * p_k
    }
    results <- tibble( # nolint
        rvs = seq(1, attack_size),
        intercepts = intercepted,
        leakage = leakage,
        survival = surviving_silos)
}

terminal_perf <- terminal_defence(0.85, 0.9, 400, 2000)

colours <- c("Intercepts" = "#0F5499", "Leakage" = "#990F3D")

terminal_perf_chart <- ggplot(
    data = terminal_perf,
    aes(x = rvs)
) +
    geom_line(
        aes(y = intercepts, color = "Intercepts"),
        linewidth = 1.0
) +
    geom_line(
        aes(y = leakage, color = "Leakage"),
        linewidth = 1.0
) +
    labs(
        x = "Attack Size (RVs)",
        y = "",
        title = "Performance of a Hypothetical Terminal BMD System",
        subtitle = "Intercept Probability = 0.8, ICBM Reliability = 0.9",
        color = "Legend"
) +
    scale_color_manual(
        values = colours
) +
    theme_fivethirtyeight() +
    plot_theme

print(terminal_perf_chart)

terminal_silos_chart <- ggplot(
    data = terminal_perf,
    aes(x = rvs, y = survival)
) +
    geom_line(
        colour = "#FF8833",
        linewidth = 1.0
) +
    labs(
        x = "Attack Size (RVs)",
        y = "Surviving Silos",
        title = "Silo Defence With a Hypothetical Terminal BMD System",
        subtitle = "Intercept Probability = 0.8, ICBM Reliability = 0.9"
) +
    theme_fivethirtyeight() +
    plot_theme

print(terminal_silos_chart)

ggsave(
    "terminal_perf.png",
    terminal_perf_chart,
    device = "png",
    path = "~/documents/coding/R/missile-defence/assets"
)

ggsave(
    "terminal_silos.png",
    terminal_silos_chart,
    device = "png",
    path = "~/documents/coding/R/missile-defence/assets"
)