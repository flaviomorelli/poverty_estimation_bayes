using DataFrames, Random
using Plots, StatsPlots
import StatsBase

include("./utils/simulations/sim_models.jl")

domains = 50
pop_domain = 200
group = repeat(1:domains, inner=pop_domain)
pop_size = length(group)
sample_size = [12, 23, 28, 14, 10, 23, 19, 25, 29, 10, 14, 18, 15, 20, 13, 12, 16, 27, 20, 26, 27, 23, 12, 12, 11, 18, 17, 29, 11, 29, 17, 9, 14, 8, 8, 18, 21, 21, 16, 16, 25, 13, 26, 19, 28, 20, 24, 9, 25, 21]

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

function sample_df(df, sample_size, domains)
    sample = Matrix{Float64}(undef, sum(sample_size), size(df)[2]) |>
                x -> DataFrame(x, names(df))
    border = [0; cumsum(sample_size)]

    for d in 1:domains
        domain_df = filter(df -> df.id == d, df)
        nrow_df = size(domain_df)[1]
        sample_idx = StatsBase.sample(1:nrow_df, 
                                    sample_size[d]; 
                                    replace=false)
        sample[(border[d] + 1):border[d + 1], :] = domain_df[sample_idx, :]
    end
    sample.id = Int.(sample.id)

    return sample
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

pareto_sample = sample_df(pareto_df, sample_size, domains)