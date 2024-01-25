using Distributions, Statistics, LinearAlgebra, NLsolve, LaTeXStrings, Plots, PlotTheme

## Simulations for terminal defence
function terminal(p_0::Real, decay::Real, rvs::Int, chart::String)
    warheads = range(0, rvs)
    p_x = zeros(rvs + 1)
    p_x[1] = p_0
    alpha = decay
    innov = Normal(0, 0.005)

    for i in 1:rvs
        p_x[i + 1] = alpha * p_x[i]
    end

    p_l = 1 .- p_x
    L_x = p_l .* warheads
    I_x = p_x .* warheads

    function l!(L, x)
        L[1] = 1 - (2 * p_0) * alpha^x[1]
    end

    sol = nlsolve(l!, [1.0])
    plt = plot()

    if chart == "probs"
        plot!(plt, warheads, p_x, xlabel = L"x", label = L"p_x")
        plot!(plt, warheads, p_l, label = L"l_x")
        vline!(plt, sol.zero, label = L"p_x = L_x")
    elseif chart == "perf"
        plot!(plt, warheads, L_x, xlabel = L"x", label = L"L_x")
        plot!(plt, warheads, I_x, label = L"I_x")
        vline!(plt, sol.zero, label = L"I_x = L_x")
    end
    return plt
end

# Simulations for layered defence
function layered(p_o::Real, p_u::Real, decay:Real, rvs::Int, chart::String)
    warheads = range(0, rvs)
    p_ux = zeros(rvs + 1)
    p_ox = zeros(rvs + 1)
    p_ux[1] = p_u
    p_ox[1] = p_o
    alpha = decay

    for i in 1:rvs
        p_ox[i + 1] = alpha * p_ox[i]
        p_ux[i + 1] = alpha * p_ux[i]
    end

    p_l = (1 .- p_ox) .* (1 .- p_ux)
    L_x = p_l .* warheads
    I_x = warheads .- L_x

    function l!(L, x)
        L[1] = 1.0 - 2p_u * alpha^x[1] - 2p_o * alpha^x[1] + (2p_u * p_o) * alpha^2x[1]
    end
    
    sol = nlsolve(l!, [1.0])
    plt = plot()

    if chart == "probs"
        plot!(plt, warheads, 1 .- p_l, label = L"p_x")
        plot!(plt, warheads, p_l, label = L"l_x")
        vline!(plt, sol.zero, label = L"p_x = l_x")
    elseif chart == "perf"
        plot!(plt, warheads, I_x, label = L"I_x")
        plot!(plt, warheads, L_x, label = L"L_x")
        vline!(plt, sol.zero, label = L"I_x = L_x")
    end
    return plt
end

