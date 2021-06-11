using Distributions, Turing, LinearAlgebra  

include("gb2.jl")

initialize(n_var, dim) = [Vector{Float64}(undef, dim) for _ in 1:n_var]

corr_matrix(dim, ρ) = I(dim) + abs.(I(dim) .- 1) * ρ

@model function logscale(N, D, K, group; ρ=0.2)
    y, ε = initialize(2, N)
    u = Vector{Float64}(undef, D) 
    X = Matrix{Float64}(undef, N, K)

    μ = Matrix{Float64}(undef, D, K)
    Σ = corr_matrix(K, ρ)    

    β = repeat([0.1], K)
    
    for d in 1:D
        u[d] ~ Normal(0, 0.4)
        for k in 1:K 
            μ[d, k] ~ Uniform(2, 3)
        end
    end
    
    for n in 1:N
        ε[n] ~ Normal(0, 0.3)
        X[n, :] ~ MvNormal(μ[group[n], :], Σ)
        y[n] = exp(5 + X[n, :] ⋅ β  + u[group[n]] + ε[n])
    end 

    return (y = y, X = X, ε = ε, u = u)
end

@model function pareto(N, D, K, group; ρ=0.2)
    y, ε = initialize(2, N)
    u = Vector{Float64}(undef, D)
    X = Matrix{Float64}(undef, N, K)

    μ = Matrix{Float64}(undef, D, K)
    Σ = corr_matrix(K, ρ)    

    β = repeat([-350], K)
    
    for d in 1:D
        u[d] ~ Normal(0, 500)
        for k in 1:K 
            μ[d, k] ~ Uniform(-3, 3)
        end
    end
    
    for n in 1:N
        ε[n] ~ Pareto(3, 2000) 
        X[n, :] ~ MvNormal(μ[group[n], :], Σ) 
    end 

    ε = ε .- mean(ε)
    for n in 1:N
        y[n] = 12_000 + X[n, :] ⋅ β + u[group[n]] + ε[n]
    end

    return (y = y, X = X, ε = ε, u = u)
end

@model function gb2(N, D, K, group; ρ=0.2)
    y, ε = initialize(2, N)
    u = Vector{Float64}(undef, D)
    X = Matrix{Float64}(undef, N, K)

    μ = Matrix{Float64}(undef, D, K)
    Σ = corr_matrix(K, ρ)    

    β = repeat([-250], K)
    
    for d in 1:D
        u[d] ~ Normal(0, 500)
        for k in 1:K 
            μ[d, k] ~ Uniform(-1, 1)
        end
    end
    
    for n in 1:N
        ε[n] ~ GB2(2.5, 18, 1.46, 1700) 
        X[n, :] ~ MvNormal(μ[group[n], :], Σ) 
    end 

    ε = ε .- mean(ε)
    for n in 1:N
        y[n] = 9_000 +  X[n, :] ⋅ β + u[group[n]] + ε[n]
    end

    return (y = y, X = X, ε = ε, μ = μ, u = u)
end

# Simple models

@model function logscale_simple(N, D, group)
    y, x, z, ε = initialize(4, N)
    μ, u = initialize(2, D)
    
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

@model function pareto_simple(N, D, group)
    y, x, ε = initialize(4, N)
    μ, u = initialize(2, D)
    
    for d in 1:D
        μ[d] ~ Uniform(-3, 3)
        u[d] ~ Normal(0, 500)
    end
    
    for n in 1:N
        ε[n] ~ Pareto(3, 2000) * √2
        x[n] ~ Normal(μ[group[n]], 7.5)
    end 

    ε = ε .- mean(ε)
    for n in 1:N
        y[n] = 12_000 - 400 * x[n] + u[group[n]] + ε[n]
    end

    return (y = y, x = x, ε = ε, μ = μ, u = u)
end

@model function gb2_simple(N, D, group)
    y, x, ε = initialize(3, N)
    μ, u = initialize(2, D)
    
    for d in 1:D
        μ[d] ~ Uniform(-1, 1)
        u[d] ~ Normal(0, 500)
    end
    
    for n in 1:N
        ε[n] ~ GB2(2.5, 18, 1.46, 1700) 
        x[n] ~ Normal(μ[group[n]], 5)
    end 

    ε = ε .- mean(ε)
    for n in 1:N
        y[n] = 8_000 - 400 * x[n] + u[group[n]] + ε[n]
    end

    return (y = y, x = x, ε = ε, μ = μ, u = u)
end

