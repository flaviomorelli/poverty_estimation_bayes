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

# ╔═╡ 04e4e49c-9162-11eb-3e21-2bc419b8ed8f
using Distributions, Plots, StatsPlots, PlutoUI

# ╔═╡ 642fa900-9162-11eb-15b7-57ec7c60e9ff
@bind ν Slider(2:0.1:50)

# ╔═╡ 30deb5b4-9162-11eb-3b9c-a1035619730f
begin
	plot(Normal(), xlim=[-5, 5], yaxis=:log)
	plot!(TDist(ν), title="ν = $ν")
end

# ╔═╡ da147b5a-9162-11eb-25c2-43ab34fd5be7
kurtosis(TDist(ν))

# ╔═╡ e78efdb8-9163-11eb-10d5-37e85424dde7
md"""
Gamma distribution prior over dof
"""

# ╔═╡ 1e9c432a-916d-11eb-1e79-2f8efbbb44d1
@bind β Slider(0.01:0.001:1)

# ╔═╡ 3e878734-916e-11eb-170d-3d2c65bac40d
α = 2

# ╔═╡ 4715fa26-916d-11eb-071b-d92749d54b31
plot(Gamma(α, 1/β), title="β: $β", xlim=[0, 20])

# ╔═╡ 00e1b9fe-916e-11eb-356d-7945b69d3674
quantile(Gamma(α, 1/β), [0.1, 0.5, 0.9, 0.95, 0.99])

# ╔═╡ Cell order:
# ╠═04e4e49c-9162-11eb-3e21-2bc419b8ed8f
# ╠═30deb5b4-9162-11eb-3b9c-a1035619730f
# ╠═642fa900-9162-11eb-15b7-57ec7c60e9ff
# ╠═da147b5a-9162-11eb-25c2-43ab34fd5be7
# ╠═e78efdb8-9163-11eb-10d5-37e85424dde7
# ╠═1e9c432a-916d-11eb-1e79-2f8efbbb44d1
# ╠═3e878734-916e-11eb-170d-3d2c65bac40d
# ╠═4715fa26-916d-11eb-071b-d92749d54b31
# ╠═00e1b9fe-916e-11eb-356d-7945b69d3674
