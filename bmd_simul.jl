using Distributions, LinearAlgebra, LaTeXStrings, Statistics, Plots, Random

## Simulations for terminal defence
function terminal(p_0, decay, rvs, chart)
    warheads = range(0, rvs)
    p_x = zeros(rvs + 1)
    p_x[1] = p_0
    alpha = decay
    innov = Normal(0, 0.005)

    for i in 1:rvs
        p_x[i + 1] = alpha * p_x[i] + rand(innov)
    end

    p_l = 1 .- p_x
    leaks = p_l .* warheads
    intercepts = p_x .* warheads

    plt = plot()

    if chart == "probs"
        plot!(plt, warheads, p_x, label = L"p_x", xlabel = L"x")
        plot!(plt, warheads, p_l, label = L"L_x", xlabel = L"x")
    elseif chart == "perf"
        plot!(plt, warheads, leaks, label = L"L_x", xlabel = L"x")
        plot!(plt, warheads, intercepts, label = L"I_x", xlabel = L"x")
    end
    return plt
end

Random.seed!(456)

function layered(p_o, p_u, decay, rvs, chart)
    warheads = range(0, rvs)
    p_ux = zeros(rvs + 1)
    p_ox = zeros(rvs + 1)
    p_ux[1] = p_u
    p_ox[1] = p_o
    alpha = decay
    innov = Normal(0, 0.005)

    for i in 1:rvs
        p_ox[i + 1] = alpha * p_ox[i] + rand(innov)
        p_ux[i + 1] = alpha * p_ux[i] + rand(innov)
    end

    l_x = (1 .- p_ox) .* (1 .- p_ux)
    leaks = l_x .* warheads
    intercepts = warheads .- leaks

    plt = plot()

    if chart == "probs"
        plot!(plt, warheads, p_ox, label = L"p_{o,x}")
        plot!(plt, warheads, p_ux, label = L"p_{u,x}")
    elseif chart == "perf"
        plot!(plt, warheads, intercepts, label = L"I_x")
        plot!(plt, warheads, leaks, label = L"L_x")
    end
    return plt
end