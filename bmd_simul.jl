using Distributions, LinearAlgebra, LaTeXStrings, Statistics, Plots, PlotThemes
theme(:default)

function terminal_prob(pi_0, α, rv)
    # create probability arrays 
    pi_x = zeros(rv + 1)
    pi_x[1] = pi_0
    
    # iterate probabilities over x
    for i in 2:(rv + 1)
        pi_x[i] = alpha * pi_x[i - 1]
    end
    return pi_x
end

# parameters for example simulation
pi_0 = 0.8
α = 0.999
rv = 1000

# plot intercept probabilities
plt = plot(legendfontsize = 12, ylimits = (0, 1))
ps = [0.6, 0.7, 0.8, 0.9]
[plot!(plt, 0:rv, terminal_prob(pi_0, alpha, rv), lw = 2,
 xlabel = L"x", ylabel = L"\pi_x", label = L"\pi_0 = %$pi_0") 
 for pi_0 in ps]
plt

function terminal_perf(pi_0, α, rv)
    # get probabilities and calculate intercepts
    x = range(0, rv)
    pi_x = terminal_prob(pi_0, α, rv)
    I_x = pi_x .* x
    L_x = x .- I_x

    # define turning point function
    function f!(F, x)
        F[1] = 1 - (2 * pi_0) * α^x[1]
    end

    # store results
    sol = nlsolve(f!, [1.0])
    out = (;int = I_x, leak = L_x, sol = sol.zero)
    return out
end

sim_terminal = terminal_perf(pi_0, alpha, rv)

# plot simulated terminal defence
plt = plot(legendfontsize = 12)
plot!(plt, sim_terminal.int, lw = 2, xlabel = L"x", label = L"I_x")
plot!(plt, sim_terminal.leak, lw = 2, label = L"I_x")
vline!(plt, [sim_terminal.sol], lw = 2, ls = :dash, label = L"I_x = L_x")

function layered_prob(pi_o, pi_u, α, rv)
    # create probability arrays
    pi_ux = zeros(rv + 1)
    pi_ox = zeros(rv + 1)
    pi_ux[1] = pi_u
    pi_ox[1] = pi_o

    # iterate probabilities over x
    for i in 2:(rv + 1)
        pi_ox[i] = α * pi_ox[i - 1] 
        pi_ux[i] = α * pi_ux[i - 1]
    end
    
    # get probability of total intercepts
    pi_x = 1 .- (1 .- pi_ox) .* (1 .- pi_ux)
    return pi_x
end

# parameters for example simulation
pi_o = 0.8
pi_u = 0.8
alpha = 0.999
rv = 2000

# plot intercept probabilities
plt = plot(legendfontsize = 12, ylimits = (0, 1))
ps = [0.5, 0.6, 0.7, 0.8]
[plot!(plt, 0:rv, layered_prob(pi_o, 0.8, alpha, rv), lw = 2,
 xlabel = L"x", ylabel = L"\pi_x", label = L"\pi_{u,0} = %$pi_o") 
 for pi_o in ps]
plt

function layered_perf(pi_o, pi_u, α, rv)
    # get probabilities and calculate intercepts
    x = range(0, rv)
    pi_x = layered_prob(pi_o, pi_u, α, rv)
    I_x = pi_x .* x
    L_x = x .- I_x

    # define turning point function
    function l!(L, x)
        L[1] = 1.0 - 2pi_u * α^x[1] - 2pi_o * α^x[1] + (2pi_u * pi_o) * α^2x[1]
    end
    
    # store results
    sol = nlsolve(l!, [1.0])
    out = (; I_x, L_x, sol = sol.zero)
    return out
end

sim_layered = layered_perf(pi_o, pi_u, α, rv)

# plot simulated layered defence
plt = plot(legendfontsize = 12)
plot!(plt, sim_layered.int, lw = 2, xlabel = L"x", label = L"I_x")
plot!(plt, sim_layered.leak, lw = 2, label = L"I_x")
vline!(plt, [sim_layered.sol], lw = 2, ls = :dash, label = L"I_x = L_x")

function terminal_surv(pi_k, rv_max; α = 0.999)
    pi = range(0, 1, 100)
    rv = range(rv_max / 5, rv_max, 5)
    plt = plot(legendfontsize = 12)

    for x in rv
        pi_x = pi .* alpha^x
        L_x = x .* (1 .- pi)
        T_k = pi_k .* L_x
        plot!(plt, pi, T_k, label = L"x = %$x", lw = 3,
              xlabel = L"\pi_x", ylabel = "Targets Killed")
    end
    return plt
end

function layered_surv(pi_k, rv_max; α = 0.999)
    pi_u = range(0, 1, 100)
    pi_o = range(0, 1, 100)
    x = rv_max
    plt = plot(guidefontsize = 14)

    pi_ux = pi_u .* alpha^x
    pi_ox = pi_o .* alpha^x
    T_k(pi_ux, pi_ox) = x .* (1 .- pi_ox) .* (1 .- pi_ux) .* pi_k
    plot!(plt, pi_u, pi_o, T_k, st = :surface, c = cgrad([:blue, :red]),
          alpha = 0.7, label = "Targets Killed", xlabel = L"$\pi_{u,x}$",
          ylabel = L"$\pi_{o,x}$", colorbar = false)
end