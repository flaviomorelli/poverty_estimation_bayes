import Distributions
import Random
import Base.rand

using SpecialFunctions


struct GB2{T <: Real} <: ContinuousUnivariateDistribution
    α::T # Shape 1
    β::T # Shape 2
    p::T # Shape 3
    q::T # Scale
    
    function GB2{T}(α::T, β::T, p::T, q::T; check_args=true) where {T <: Real}
        check_args && Distributions.@check_args(GB2, 
                                                all([α > zero(α), 
                                                    β > zero(β), 
                                                    p > zero(p), 
                                                    q > zero(q)]))
            
        return new{T}(α, β, p, q)
    end
end

Distributions.params(d::GB2) = (d.α, d.β, d.p, d.q)

GB2(α::Float64, β::Float64, p::Float64, q::Float64) = GB2{Float64}(α, β, p, q)
GB2(α::Real, β::Real, p::Real, q::Real) = GB2(promote(α, β, p, q)...)
GB2(α::Integer, β::Integer, p::Integer, q::Integer) = GB2(float(α), float(β),
                                                          float(p), float(q))

Distributions.@distr_support GB2 0.0 Inf

function Base.rand(rng::AbstractRNG, d::GB2) 
    (α, β, p, q) = params(d) 
    z = rand(rng, Beta(β, p))
    y = z / (1 - z)
    q * y^(1 / α)
end 

Distributions.sampler(rng::AbstractRNG, d::GB2) = Base.rand(rng::AbstractRNG, d::GB2) 

function Distributions.logpdf(d::GB2, x::Real)
    (α, β, p, q) = params(d)  
    if x <= 0
        return zero
    end
    num = log(p) + (α * p - 1) * (log(x) - log(q)) + (-α - β) * log(1 + (x / q)^p)
    den = log(q) + logbeta(α, β) 
    return num - den
end

function Distributions.cdf(d::GB2, x::Real)
    (α, β, p, q) = params(d) 
    if x == Inf
        return 1
    end
	y = (x / q)^α
	z = y / (1 + y)
	return cdf(Beta(β, p), z)
end


