#using Pkg
#Pkg.add("DifferentialEquations")
using DifferentialEquations, Plots

function lorenz!(du,u,p,t)
    du[1] = 10.0*(u[2]-u[1])
    du[2] = u[1]*(28.0-u[3]) - u[2]
    du[3] = u[1]*u[2] - (8/3)*u[3]
end

u0 = [1.0;0.0;0.0]
tspan = (0.0,100.0)
prob = ODEProblem(lorenz!,u0,tspan)
sol = solve(prob)

plot(sol,vars=(1,2,3))


#---------------------------------------------------------------#

function logistic!(dx, x, t)
    dx = x(1-x/100.0)
end

f(u,p,t) = 0.5*u* (1.0 - u/20.0)
u0 = 0.01
tspan = (0.0,50.0)
prob = ODEProblem(f,u0,tspan)
sol = solve(prob)

plot(sol)


#---------------------------------------------------------------#

# FitzHugh-Nagumo model
using DifferentialEquations, Plots

function I(t)
    if 5.0 < t < 6.0 
        I = .1
    elseif 10.0 <= t < 11.0 
        I = .2
    elseif 15.0 <= t < 16.0 
        I = .7
    else
        I = 0
    end
    return I 
end

function FHN!(du,u,p,t)
    a = 0.7
    b = 0.8
    c = 10
    du[1] = c * ( u[1] - u[1]^3/3 - u[2]  + I(t) )
    du[2] = u[1] - b * u[2] + a
end

u0 = [0.0; 0.0]
tspan = (0.0,25.0)
prob = ODEProblem(FHN!,u0,tspan)
sol = solve(prob)

plot(sol)
plot!([0.1:0.01:25.0;], I.([0.1:0.01:25.0;] ).-2.5)
#plot(sol.t, sol[1,:] , legend=false)


#---------------------------------------------------------------#
using DifferentialEquations, Plots

function HT!(du, u, p, t)
    du[1] = u[1]*(1- u[1]) - (5 * u[1])/(1 + 3*u[1]) * u[2]
    du[2] = (5*u[1])/(1 + 3*u[1]) * u[2] - 0.4 * u[2] - (0.1 * u[2])/(1 + 2 * u[2]) * u[3]
    du[3] = (0.1 * u[2])/(1 + 3 * u[2]) * u[3] - 0.01 * u[3]
end

u0 = [1.0;0.1;8.0]
tspan = (0.0, 3000.0)
prob = ODEProblem(HT!, u0, tspan)
sol = solve(prob)

plot(sol, vars=(1,2,3))
#plot(sol.t, sol[3,:] , legend=false)


#---------------------------------------------------------------#
using DifferentialEquations, Plots

function ghazanfer!(du, u, p, t)
    gamma1 = 2.5
    gamma2 = 25
    du[1] = u[3]
    du[2] = u[4]
    du[3] = gamma1^2 * .05 - gamma1^2 * u[1] + gamma1 * u[3] - gamma1 * u[1]^2 * u[3] + gamma1^2 * 4 * u[4]
    du[4] = gamma2^2 * .05 - gamma2^2 * u[2] + gamma2 * u[4] - gamma2 * u[1]^2 * u[4] + gamma2^2 * 0.015 * u[3]
    
end

u0 = [1.0;1.0;1.0;1.0]
tspan = (0.0, 5000.0)
prob = ODEProblem(ghazanfer!, u0, tspan)
sol = solve(prob)

plot(sol, vars=(1,2))
plot(sol, vars=(3,4))
plot(sol.t, sol[3,:] , legend=false)

