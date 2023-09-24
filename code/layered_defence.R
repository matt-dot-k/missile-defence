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

# Layered defence
layered_defence <- function(p_over, p_under, p_k, silos, attack_size) { # nolint
    # Generate sequence of decreasing probabilities
    overlay_probs <- rnorm(attack_size, p_over, 0.01) %>%
        sort(decreasing = TRUE)
    underlay_probs <- rnorm(attack_size, p_under, 0.01) %>%
        sort(decreasing = TRUE)

    # Pre-allocate space
    overlay_int <- double(attack_size)
    overlay_leak <- double(attack_size)
    underlay_int <- double(attack_size)
    underlay_leak <- double(attack_size)
    surviving_silos <- double(attack_size)

    # Simulate the attack
    for (i in 1:attack_size) {
        prob_int_overlay <- rnorm(1, overlay_probs[i], 0.005)
        prob_int_underlay <- rnorm(1, underlay_probs[i], 0.005)

        overlay_int[i] <- i * prob_int_overlay
        overlay_leak[i] <- i - overlay_int[i]

        underlay_int[i] <- overlay_leak[i] * prob_int_underlay
        underlay_leak[i] <- overlay_leak[i] - underlay_int[i]

        surviving_silos[i] <- silos - underlay_leak[i] * p_k
    }
    results <- tibble( # nolint
        rvs = seq(1, attack_size),
        overlay = overlay_int,
        underlay = underlay_int,
        leakage = underlay_leak,
        survival = surviving_silos
    )
}

layered_perf <- layered_defence(0.8, 0.7, 0.9, 400, 2000)

colors_2 <- c(
    "Intercepts (Overlay)" = "#0F5499",
    "Intercepts (Underlay)" = "#00A0DD",
    "Leakage" = "#990F3D"
)

layered_perf_chart <- ggplot(
    data = layered_perf,
    aes(x = rvs)
) +
    geom_line(
        aes(y = overlay, color = "Intercepts (Overlay)"),
        linewidth = 1.0
) +
    geom_line(
        aes(y = underlay, color = "Intercepts (Underlay)"),
        linewidth = 1.0
) +
    geom_line(
        aes(y = leakage, color = "Leakage"),
        linewidth = 1.0
) +
    labs(
        x = "Attack Size (RVs)",
        y = "",
        title = "Performance of a Hypothetical Layered BMD System",
        subtitle = "Overlay Intercept Probability = 0.8, Underlay Intercept Probability = 0.7, ICBM Reliability = 0.9", # nolint
        color = "Legend"
) +
    scale_color_manual(
        values = colors_2
) +
    theme_fivethirtyeight() +
    plot_theme

print(layered_perf_chart)

layered_silos_chart <- ggplot(
    data = layered_perf,
    aes(x = rvs, y = survival)
) +
    geom_line(
        colour = "#FF8833",
        linewidth = 1.0
) +
    labs(
        x = "Attack Size (RVs)",
        y = "Surviving Silos",
        title = "Silo Defence With a Hypothetical Layered BMD System",
        subtitle = "Overlay Intercept Probability = 0.8, Underlay Intercept Probability = 0.7, ICBM Reliability = 0.9" # nolint
) +
    theme_fivethirtyeight() +
    plot_theme

print(layered_silos_chart)

ggsave(
    "layered_perf.png",
    layered_perf_chart,
    device = "png",
    path = "~/documents/coding/R/missile-defence/assets"
)

ggsave(
    "layered_silos.png",
    layered_silos_chart,
    device = "png",
    path = "~/documents/coding/R/missile-defence/assets"
)