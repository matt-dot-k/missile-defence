using Distributions, LinearAlgebra, LaTeXStrings, NLsolve, Statistics, Plots, PlotThemes
theme(:default)

# define different defence types
mutable struct terminal
    init::Float64
    kill::Float64
    alpha::Float64
    rv::Int64
end

mutable struct layered
    init1::Float64
    init2::Float64
    kill::Float64
    alpha::Float64
    rv::Int64
end

# parameters for example simulations
p_t = terminal(0.8, 0.9, 0.999, 1000)
p_l = layered(0.8, 0.8, 0.9, 0.999, 2000)

# functions for terminal defence
# ------------------------------

function terminal_prob(pi_0, alpha, rv)
    # create probability arrays 
    pi_x = zeros(rv + 1)
    pi_x[1] = pi_0
    
    # iterate probabilities over x
    for i in 2:(rv + 1)
        pi_x[i] = alpha * pi_x[i - 1]
    end
    return pi_x
end

function terminal_perf(pi_0, alpha, rv)
    # get probabilities and calculate intercepts
    x = range(0, rv)
    pi_x = terminal_prob(pi_0, alpha, rv)
    I_x = pi_x .* x
    L_x = x .- I_x

    # define turning point function
    function f!(F, x)
        F[1] = 1 - (2 * pi_0) * alpha^x[1]
    end

    # store results
    sol = nlsolve(f!, [1.0])
    out = (; int = I_x, leak = L_x, sol = sol.zero)
    return out
end

# plot simulations
# ----------------

plt_1 = plot(legendfontsize = 12, ylimits = (0, 1), size = (700, 500))
probs = [0.6, 0.7, 0.8, 0.9]

for p_t.init in probs
    label = p_t.init
    plot!(plt_1, 0:p_t.rv, terminal_prob(p_t.init, p_t.alpha, p_t.rv), lw = 2,
          xlabel = L"x", ylabel = L"\pi_x", label = L"\pi_0 = %$label")
end

simul_1 = terminal_perf(p_t.init, p_t.alpha, p_t.rv)

plt_2 = plot(legendfontsize = 12, size = (700, 500))
plot!(plt_2, simul_1.int, lw = 2, xlabel = L"x", label = L"I_x")
plot!(plt_2, simul_1.leak, lw = 2, label = L"I_x")
vline!(plt_2, [simul_1.sol], lw = 2, ls = :dash, label = L"I_x = L_x")

display(plt_1)
display(plt_2)

# functions for layered defence
# -----------------------------

function layered_prob(pi_1, pi_2, alpha, rv)
    # create probability arrays
    pi_1x = zeros(rv + 1)
    pi_2x = zeros(rv + 1)
    pi_1x[1] = pi_1
    pi_2x[1] = pi_2

    # iterate probabilities over x
    for i in 2:(rv + 1)
        pi_1x[i] = alpha * pi_1x[i - 1] 
        pi_2x[i] = alpha * pi_2x[i - 1]
    end
        
    pi_x = 1 .- (1 .- pi_1x) .* (1 .- pi_2x)
    return pi_x    
end

function layered_perf(pi_1, pi_2, alpha, rv)
    # get probabilities and calculate intercepts
    x = range(0, rv)
    pi_x = layered_prob(pi_1, pi_2, alpha, rv)
    I_x = pi_x .* x
    L_x = x .- I_x

    # define turning point function
    function f!(F, x)
        F[1] = 1.0 - 2pi_1 * alpha^x[1] - 2pi_2 * alpha^x[1] + (2pi_1 * pi_2) * alpha^2x[1]
    end
    
    # store results
    sol = nlsolve(f!, [1.0])
    out = (; int = I_x, leak = L_x, sol = sol.zero)
    return out
end

# plot simulations
# ----------------

plt_3 = plot(legendfontsize = 12, ylimits = (0, 1), size = (700, 500))
probs = [0.5, 0.6, 0.7, 0.8]

for p_l.init1 in probs
    label = p_l.init1
    plot!(plt_3, 0:p_l.rv, layered_prob(p_l.init1, 0.8, p_l.alpha, p_l.rv), 
          lw = 2, xlabel = L"x", ylabel = L"\pi_x", label = L"\pi_1 = %$label")
end

simul_2 = layered_perf(p_l.init1, p_l.init2, p_l.alpha, p_l.rv)

plt_4 = plot(legendfontsize = 12, size = (700, 500))
plot!(plt_4, simul_2.int, lw = 2, xlabel = L"x", label = L"I_x")
plot!(plt_4, simul_2.leak, lw = 2, label = L"I_x")
vline!(plt_4, [simul_2.sol], lw = 2, ls = :dash, label = L"I_x = L_x")

display(plt_3)
display(plt_4)

# functions for target survivability
# ----------------------------------

function terminal_surv(pi_k, rv_max, alpha)
    pi = range(0, 1, 100)
    rv = range(rv_max / 5, rv_max, 5)
    plt = plot(legendfontsize = 12, size = (700, 500))

    for x in rv
        pi_x = pi .* alpha^x
        L_x = x .* (1 .- pi)
        T_k = pi_k .* L_x
        plot!(plt, pi, T_k, label = L"x = %$x", lw = 3,
              xlabel = L"\pi_x", ylabel = "Targets Killed")
    end
    return plt
end

function layered_surv(pi_k, rv_max, alpha)
    pi_1 = range(0, 1, 100)
    pi_2 = range(0, 1, 100)
    x = rv_max
    plt = plot(guidefontsize = 14, size = (700, 500))

    pi_1x = pi_1 .* alpha^x
    pi_2x = pi_2 .* alpha^x
    T_k(pi_1x, pi_2x) = x .* (1 .- pi_1x) .* (1 .- pi_2x) .* pi_k
    plot!(plt, pi_1, pi_2, T_k, st = :surface, c = cgrad([:blue, :red]),
          alpha = 0.7, label = "Targets Killed", xlabel = L"$\pi_{u,x}$",
          ylabel = L"$\pi_{o,x}$", colorbar = false, camera = (60, 45))
end

plt_5 = terminal_surv(p_t.kill, p_t.rv, p_t.alpha)
plt_6 = layered_surv(p_l.kill, p_l.rv, p_l.alpha)

display(plt_5)
display(plt_6)

readline()