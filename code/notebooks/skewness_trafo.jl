### A Pluto.jl notebook ###
# v0.12.21

using Markdown
using InteractiveUtils

# This Pluto notebook uses @bind for interactivity. When running this notebook outside of Pluto, the following 'mock version' of @bind gives bound variables a default value (instead of an error).
macro bind(def, element)
    quote
        local el = $(esc(element))
        global $(esc(def)) = Core.applicable(Base.get, el) ? Base.get(el) : missing
        el
    end
end

# ╔═╡ 4a802352-90ba-11eb-1386-41d87c0fa5f7
using StatsBase, Distributions, Optim, Plots, StatsPlots, PlutoUI

# ╔═╡ 827fb218-90ba-11eb-00e9-c3b3cd5859d8
logshift(x, s) = log(x + s)

# ╔═╡ 05b0b4c8-90bd-11eb-0bda-1d1493c075d6
normalize(x) = x ./ maximum(x) 

# ╔═╡ 8d0e2566-90ba-11eb-30c2-cfbd54765252
N = 10^3

# ╔═╡ 939c014e-90ba-11eb-1533-f725455dcc04
begin 
	pareto = (0.5 * rand(GeneralizedPareto(600, 200, 0.4), N) + 
		0.5 * rand(Normal(3000, 300), N)) ./ 1000
	lognormal = rand(LogNormal(8, 0.7), N) ./ 1000
	gamma = rand(Gamma(1.5, 2000), N) ./ 1000
end

# ╔═╡ 9e9d5784-90ba-11eb-27bb-f5d947425c61
begin
	density(pareto, label="Pareto")
	density!(lognormal, label="Lognormal")
	density!(gamma, label="Gamma")
end

# ╔═╡ b5e3487c-90ba-11eb-1dcc-eda5e8882a59
begin
	density(log.(pareto), label="Pareto")
	density!(log.(lognormal), label="Lognormal")
	density!(log.(gamma), label="Gamma")
end

# ╔═╡ ded1afa4-90be-11eb-1cd0-59b1a308f7e5
begin
	target = pareto
	lower_bound = -minimum(target) + 0.01
end

# ╔═╡ b9807dae-90bc-11eb-193c-eb0e8ce5f069
@bind s Slider(lower_bound:0.001:1)

# ╔═╡ 7a93c3de-90bd-11eb-02d3-014ff21f70e3
histogram(logshift.(target, s), title = "shift: $s")

# ╔═╡ 13718992-90be-11eb-0154-ddf6b74c161d
skewness(logshift.(target, s))

# ╔═╡ ce09d9f2-90bf-11eb-188f-274c951302a0
kurtosis(logshift.(target, s))

# ╔═╡ 7bd9756a-90c5-11eb-3e57-41e9423e6e71
f(x) = abs(skewness(logshift.(target, x)))

# ╔═╡ 729533b0-90c3-11eb-0b2a-8de9dcef92e4
begin
	s0 = [1.0]
	optimizer = optimize(f, lower_bound, 1)
	minimizer = Optim.minimizer(optimizer)
end

# ╔═╡ 5d36b060-90c4-11eb-2704-97d34f5bccad
logshift.(target, minimizer) |> skewness

# ╔═╡ 22ad5aa0-90c6-11eb-049d-81e2eb5628b7
logshift.(target, minimizer) |> kurtosis

# ╔═╡ a4dcd604-90c6-11eb-247c-21fa2d7b1bd2
logshift.(target, minimizer) |> histogram

# ╔═╡ Cell order:
# ╠═4a802352-90ba-11eb-1386-41d87c0fa5f7
# ╠═827fb218-90ba-11eb-00e9-c3b3cd5859d8
# ╠═05b0b4c8-90bd-11eb-0bda-1d1493c075d6
# ╠═8d0e2566-90ba-11eb-30c2-cfbd54765252
# ╠═939c014e-90ba-11eb-1533-f725455dcc04
# ╠═9e9d5784-90ba-11eb-27bb-f5d947425c61
# ╠═b5e3487c-90ba-11eb-1dcc-eda5e8882a59
# ╠═ded1afa4-90be-11eb-1cd0-59b1a308f7e5
# ╠═b9807dae-90bc-11eb-193c-eb0e8ce5f069
# ╠═7a93c3de-90bd-11eb-02d3-014ff21f70e3
# ╠═13718992-90be-11eb-0154-ddf6b74c161d
# ╠═ce09d9f2-90bf-11eb-188f-274c951302a0
# ╠═7bd9756a-90c5-11eb-3e57-41e9423e6e71
# ╠═729533b0-90c3-11eb-0b2a-8de9dcef92e4
# ╠═5d36b060-90c4-11eb-2704-97d34f5bccad
# ╠═22ad5aa0-90c6-11eb-049d-81e2eb5628b7
# ╠═a4dcd604-90c6-11eb-247c-21fa2d7b1bd2
