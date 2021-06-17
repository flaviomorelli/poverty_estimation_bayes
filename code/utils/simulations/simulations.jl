using DataFrames, Random, CSV
using Plots, StatsPlots
import StatsBase

include("./utils/simulations/sim_models.jl")

domains = 50
pop_domain = 200
num_vars = 10
group_id = repeat(1:domains, inner=pop_domain)
pop_size = length(group_id)
sample_size = [12, 23, 28, 14, 10, 23, 19, 25, 29, 10, 14, 18, 15, 
                20, 13, 12, 16, 27, 20, 26, 27, 23, 12, 12, 11, 18, 
                17, 29, 11, 29, 17, 9, 14, 8, 8, 18, 21, 21, 16, 16, 
                25, 13, 26, 19, 28, 20, 24, 9, 25, 21]

missing_prob = 0.5

function create_df(data, pop_domain, group_id)
    df = DataFrame(data[:X])
    return hcat(df, 
                DataFrame(y=data[:y],
                    ε=data[:ε],
                    u=repeat(data[:u], inner=pop_domain),
                    group_id=group_id,
                    row_id=1:size(df)[1]))
end

function sample_indices(df, sample_size, domains; missing_prob=0)
    sample_idx = Vector{Int}[]

    for d in 1:domains
        if rand() < missing_prob
            continue
        end

        domain_df = filter(df -> df.group_id == d, df)
        domain_samples = StatsBase.sample(domain_df.row_id, 
                                    sample_size[d]; 
                                    replace=false)
        sample_idx = [sample_idx; domain_samples]
    end

    return [row_id ∈ sample_idx for row_id in df.row_id]
end

logscale_model = logscale(pop_size, domains, num_vars, group_id)
logscale_data = logscale_model()
logscale_df = create_df(logscale_data, pop_domain, group_id)
logscale_df.sample = sample_indices(logscale_df, sample_size, domains)
logscale_df.sample_miss = sample_indices(logscale_df, 
                                         sample_size,   
                                         domains; 
                                         missing_prob=missing_prob)

pareto_model = pareto(pop_size, domains, num_vars, group_id)
pareto_data = pareto_model()
pareto_df = create_df(pareto_data, pop_domain, group_id)
pareto_df.sample = sample_indices(pareto_df, sample_size, domains)
pareto_df.sample_miss = sample_indices(pareto_df, 
                                        sample_size,   
                                        domains; 
                                        missing_prob=missing_prob) 

gb2_model = gb2(pop_size, domains, num_vars, group_id)
gb2_data = gb2_model()
gb2_df = create_df(gb2_data, pop_domain, group_id)
gb2_df.sample = sample_indices(gb2_df, sample_size, domains)
gb2_df.sample_miss = sample_indices(gb2_df, 
                                    sample_size,   
                                    domains; 
                                    missing_prob=missing_prob)

CSV.write("data/simulations/logscale.csv", logscale_df)
CSV.write("data/simulations/pareto.csv", pareto_df)
CSV.write("data/simulations/gb2.csv", gb2_df)
