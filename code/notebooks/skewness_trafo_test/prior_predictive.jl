### A Pluto.jl notebook ###
# v0.14.1

using Markdown
using InteractiveUtils

# ╔═╡ fe255e4d-9ccf-4f8e-9862-92bb476cd654
using Turing, Distributions, LinearAlgebra, Plots, StatsPlots

# ╔═╡ ebe1bc30-96b2-11eb-1987-5fe3400800cd
@model function linreg(y)
	σ ~ Gamma(2, 0.1)
	α ~ Normal(10, 1)
	β ~ Normal(10, 0.5)
	x ~ Normal(0, 1)
	y ~ Normal(α + β * x, σ)
	return y
end

# ╔═╡ c5773044-96b3-11eb-0654-4f5081a72532
prior_model = linreg(missing)

# ╔═╡ c55b2435-d385-4e88-9d00-46407eec6997
[prior_model() for i in 1:10]

# ╔═╡ 66fda4e2-96b8-11eb-3cd8-699e46f6c81e
n_samples = 100

# ╔═╡ e0097366-96b3-11eb-1673-25eadb34cb74
prior_pred = sample(linreg(missing), Prior(), n_samples)

# ╔═╡ c9114f12-96b8-11eb-3166-870e4c92c7e0
x_range = minimum(prior_pred[:x]):maximum(prior_pred[:x])

# ╔═╡ 6b0f824e-96b5-11eb-02cc-6bcf784a1e46
begin
	plt = plot( xlim=[minimum(prior_pred[:x]), maximum(prior_pred[:x])],
	  ylim=[minimum(prior_pred[:y]), maximum(prior_pred[:y])], legend=false)
	for i in 1:n_samples
		α = prior_pred[:α][i]
		β = prior_pred[:β][i]
		Plots.abline!(plt, α, β, alpha=0.1, linewidth=2, color=:gray)
	end
end

# ╔═╡ c47319e2-96bb-11eb-1ccb-07927711142e
plt

# ╔═╡ 3ae34886-96bc-11eb-018e-cf0bf2536904
density(prior_pred[:y])

# ╔═╡ Cell order:
# ╠═fe255e4d-9ccf-4f8e-9862-92bb476cd654
# ╠═ebe1bc30-96b2-11eb-1987-5fe3400800cd
# ╠═c5773044-96b3-11eb-0654-4f5081a72532
# ╠═c55b2435-d385-4e88-9d00-46407eec6997
# ╠═66fda4e2-96b8-11eb-3cd8-699e46f6c81e
# ╠═e0097366-96b3-11eb-1673-25eadb34cb74
# ╠═c9114f12-96b8-11eb-3166-870e4c92c7e0
# ╠═6b0f824e-96b5-11eb-02cc-6bcf784a1e46
# ╠═c47319e2-96bb-11eb-1ccb-07927711142e
# ╠═3ae34886-96bc-11eb-018e-cf0bf2536904
