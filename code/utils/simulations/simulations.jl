using Distributions, Turing, DataFrames, Random
using Plots, StatsPlots

include("./utils/simulations/gb2.jl")

domains = 10
pop_domain = 20
group = repeat(1:domains, inner=pop_domain)
pop_size = length(group)

function initialize(N, D)
    y = Vector{Float32}(undef, N)
    x = Vector{Float32}(undef, N)
    z = Vector{Float32}(undef, N)
    ε = Vector{Float32}(undef, N)

    μ = Vector{Float32}(undef, D) 
    u = Vector{Float32}(undef, D) 
    return y, x, z, ε, μ, u 
end

@model function logscale(N, D, group)
    y, x, z, ε, μ, u = initialize(N, D)
    
    for d in 1:D
        μ[d] ~ Uniform(2, 3)
        u[d] ~ Normal(0, 0.4)
    end
    
    for n in 1:N
        z[n] ~ Normal(0, 1)
        ε[n] ~ Normal(0, sqrt(0.8))
        x[n] ~ Normal(μ[group[n]], 0.5)
        y[n] = exp(10 - x[n] + 0.5 * z[n] + u[group[n]] + ε[n])
    end 

    return (y = y, x = x, z = z, ε = ε, μ = μ, u = u)
end

@model function pareto(N, D, group)
    y, x, z, ε, μ, u = initialize(N, D)
    
    for d in 1:D
        μ[d] ~ Uniform(-3, 3)
        u[d] ~ Normal(0, 500)
    end
    
    for n in 1:N
        ε[n] ~ Pareto(3, 2000) * √2
        x[n] ~ Normal(μ[group[n]], 7.5)
        y[n] = 12_000 - 400 * x[n] + u[group[n]] + (ε[n] - mean(ε))
    end 

    return (y = y, x = x, ε = ε .- mean(ε), μ = μ, u = u)
end

@model function gb2(N, D, group)
    y, x, z, ε, μ, u = initialize(N, D)
    
    for d in 1:D
        μ[d] ~ Uniform(-1, 1)
        u[d] ~ Normal(0, 500)
    end
    
    for n in 1:N
        ε[n] ~ GB2(2.5, 18, 1.46, 1700) 
        x[n] ~ Normal(μ[group[n]], 5)
        y[n] = 8_000 - 400 * x[n] + u[group[n]] + (ε[n] - mean(ε))
    end 

    return (y = y, x = x, ε = ε .- mean(ε), μ = μ, u = u)
end

function create_df(sample, pop_domain; contains_z=false)
    df = DataFrame(y=sample[:y],
                    x=sample[:x],
                    ε=sample[:ε],
                    μ=repeat(sample[:μ], inner=pop_domain),
                    u=repeat(sample[:u], inner=pop_domain),
                    id=group)
    if contains_z 
        df.z = sample[:z]
    end
    
    return df
end

logscale_model = logscale(pop_size, domains, group)
logscale_sample = logscale_model()
logscale_df = create_df(logscale_sample, pop_domain, contains_z=true)

pareto_model = pareto(pop_size, domains, group)
pareto_sample = pareto_model()
pareto_df = create_df(pareto_sample, pop_domain)

gb2_model = gb2(pop_size, domains, group)
gb2_sample = gb2_model()
gb2_df = create_df(gb2_sample, pop_domain)
