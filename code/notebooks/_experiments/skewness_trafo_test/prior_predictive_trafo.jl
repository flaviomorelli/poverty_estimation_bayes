### A Pluto.jl notebook ###
# v0.14.1

using Markdown
using InteractiveUtils

# ╔═╡ 72cb56bc-9775-11eb-2bad-73da6a0acd34
using Distributions, Turing, LinearAlgebra, Plots, StatsPlots, PlutoUI, DynamicPPL

# ╔═╡ 75e8bc67-dcce-4040-8623-2c0e7ef5d702
N = 10^3

# ╔═╡ 18c74b33-bad5-4e0c-b43b-2a9e5e3c841c
x = rand(Normal(22, 3), N)

# ╔═╡ 36bd046f-b47b-4bef-a89e-f78aac98273e
# Check for negative values in the independent variable
sum(x .< 0 )

# ╔═╡ 66e07b0c-0ebd-4b47-82f5-9acb12a0a330
y = exp.(0.1 .+ 0.2 * x) + 4 * rand(TDist(3), N)

# ╔═╡ db5a2c7b-b3d3-4572-bf54-f7a68ed150a1
histogram(y, normalize=true)

# ╔═╡ 30f32000-20b7-48b4-b723-c066485bdd25
sum(y .< 0)

# ╔═╡ 1e282306-a73d-4a94-9e1c-6623860040d0
log.(y) |> skewness

# ╔═╡ f33cb174-2905-439d-9cab-8c04dac756af
log_x_std = (log.(x) .- mean(log.(x))) ./ std(log.(x))

# ╔═╡ 66b9f8c0-388f-4692-bbeb-62db92ab6b10
histogram(log_x_std)

# ╔═╡ 83ae81a1-adaa-4783-b803-6513a920879d
quantile(y)

# ╔═╡ d3fb7596-77ab-48eb-af90-a2ec29220756
quantile(Gamma(2, 0.6), 0.95)

# ╔═╡ 3ad5ae23-f2b5-48d3-afd8-b13c8ed360f8
# Here the coefficients are different from the simulation!
@model function income(x, y)
	α ~ Normal(4, 2)
	β ~ Normal(0.3, 2)
	μ = α .+ β * x
	σ ~ Gamma(2, 2)
	y ~ MvNormal(μ, σ)
end

# ╔═╡ 3bdca5a6-60e6-4405-b0af-071cd4f1f3dc
begin
	model = income(log_x_std, log.(y))
	prior_pred = sample(model, Prior(), 100)
end

# ╔═╡ ed8d156c-7975-4783-a59a-26481ebe3032
model_prior_marginal = income(log.(x), missing)

# ╔═╡ c115878f-7531-4a37-9ca8-52a6492db66b
y_prior = [model_prior_marginal() for i in 1:300]

# ╔═╡ 427a3197-92cd-4509-940b-9c782a7f9fc3
size(y_prior)[1]

# ╔═╡ f29e2ab1-f438-4eb7-8e3f-40698c357f7c
begin	
	density_plt = plot(xlim = (0, 800), legend=false)
	for i in eachindex(y_prior)
		density!(exp.(y_prior[i]), color=:gray, alpha=0.3, linewidth=0.5)
	end
	density!(y, linewidth=2)
end

# ╔═╡ 3d54fdcf-f446-4b5e-aec7-a8010c2d0e71
density_plt

# ╔═╡ 09191ebe-6178-4008-9793-a6b6a5cbcad0
begin
	plt_coeff = plot(legend=false)
	for i in 1:size(prior_pred)[1]
		α = prior_pred[:α][i]
		β = prior_pred[:β][i]
		plot!(x -> exp(α + β * x), exp(-2), exp(2), alpha=0.3, linewidth=2, color=:gray)
	end
	plt_coeff
end

# ╔═╡ b8ef0d03-a21d-4ad4-8ad4-75a30f0df1e0
post = sample(model, NUTS(0.65), 1000)

# ╔═╡ 09c88f85-5e5d-4267-9f58-80d39f1bb46d
describe(post)

# ╔═╡ 15831b86-58e0-4560-957f-8829a3867efa
plot(post)

# ╔═╡ bb4b726d-3764-4215-a17d-b96ceddbad4a
post |> size

# ╔═╡ c79623e1-826b-4058-acc2-4e6ac0525c37
m_test = income(log.(x), missing);

# ╔═╡ 767f27f3-2f98-4d22-90c9-b9b45f9128b8
y_pred = predict(m_test, post)

# ╔═╡ a4b8fe47-51a2-4ef2-9bbb-ffac55f0face


# ╔═╡ 064e10e0-45ae-4340-9480-9ec697e14902
y_pred |> size

# ╔═╡ 9a8df3f6-3686-4c43-835b-433974ebd9d8
begin	
	density_post = plot(xlim = (0, 100), legend=false)
	for i in eachindex(y_pred)
		density!(exp.(y_pred[i]), color=:gray, alpha=0.1)
	end
	#density!(y, linewidth=2)
end

# ╔═╡ fa6e42df-dce7-4631-8737-f00dc09abfef
density_post

# ╔═╡ 8b12f4eb-59ee-4d3c-b13c-100cc8e2c4dd
histogram([median(exp.(y)) for y in y_prior])

# ╔═╡ 2893b52f-8464-4343-b1a5-aaa302f6436c
histogram([quantile(exp.(y), 0.8) for y in y_prior])

# ╔═╡ d90b42dc-622d-4cd9-b39c-42eece5e6b9f


# ╔═╡ Cell order:
# ╠═72cb56bc-9775-11eb-2bad-73da6a0acd34
# ╠═75e8bc67-dcce-4040-8623-2c0e7ef5d702
# ╠═18c74b33-bad5-4e0c-b43b-2a9e5e3c841c
# ╠═36bd046f-b47b-4bef-a89e-f78aac98273e
# ╠═66e07b0c-0ebd-4b47-82f5-9acb12a0a330
# ╠═db5a2c7b-b3d3-4572-bf54-f7a68ed150a1
# ╠═30f32000-20b7-48b4-b723-c066485bdd25
# ╠═1e282306-a73d-4a94-9e1c-6623860040d0
# ╠═f33cb174-2905-439d-9cab-8c04dac756af
# ╠═66b9f8c0-388f-4692-bbeb-62db92ab6b10
# ╠═83ae81a1-adaa-4783-b803-6513a920879d
# ╠═d3fb7596-77ab-48eb-af90-a2ec29220756
# ╠═3ad5ae23-f2b5-48d3-afd8-b13c8ed360f8
# ╠═3bdca5a6-60e6-4405-b0af-071cd4f1f3dc
# ╠═ed8d156c-7975-4783-a59a-26481ebe3032
# ╠═c115878f-7531-4a37-9ca8-52a6492db66b
# ╠═427a3197-92cd-4509-940b-9c782a7f9fc3
# ╠═f29e2ab1-f438-4eb7-8e3f-40698c357f7c
# ╠═3d54fdcf-f446-4b5e-aec7-a8010c2d0e71
# ╟─09191ebe-6178-4008-9793-a6b6a5cbcad0
# ╠═b8ef0d03-a21d-4ad4-8ad4-75a30f0df1e0
# ╠═09c88f85-5e5d-4267-9f58-80d39f1bb46d
# ╠═15831b86-58e0-4560-957f-8829a3867efa
# ╠═bb4b726d-3764-4215-a17d-b96ceddbad4a
# ╠═c79623e1-826b-4058-acc2-4e6ac0525c37
# ╠═767f27f3-2f98-4d22-90c9-b9b45f9128b8
# ╠═a4b8fe47-51a2-4ef2-9bbb-ffac55f0face
# ╠═064e10e0-45ae-4340-9480-9ec697e14902
# ╠═9a8df3f6-3686-4c43-835b-433974ebd9d8
# ╠═fa6e42df-dce7-4631-8737-f00dc09abfef
# ╠═8b12f4eb-59ee-4d3c-b13c-100cc8e2c4dd
# ╠═2893b52f-8464-4343-b1a5-aaa302f6436c
# ╠═d90b42dc-622d-4cd9-b39c-42eece5e6b9f
