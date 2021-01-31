using CSV, DataFrames, Statistics, JuMP, Gurobi, Random

train = DataFrame(CSV.File("train.csv"))
test = DataFrame(CSV.File("test.csv"))

X = select(train, Not([:denied,:ID]))
y = train[:,:denied]
minorities = findall(train[:,:minority] .== 1)
n,p = size(X)
nm = length(minorities)
nonminorities = collect(1:n)[Not(minorities)]
nnm = length(nonminorities)

model = Model(Gurobi.Optimizer)
#set_optimizer_attribute(model, "OutputFlag", 0) 
M = 2000

e = 0.1

@variable(model,beta[j=1:p])
@variable(model,t[i=1:n])
@variable(model,beta0)
@variable(model,f[1:n], Bin) #false positives


@objective(model,Min,sum(t[i] for i=1:n))


@constraint(model,[i=1:n],0 <= t[i])
@constraint(model,[i=1:n],1 - (y[i]*(beta0 + sum(X[i,j]*beta[j] for j=1:p))) <= t[i])


@constraint(model,[i=1:n],(beta0 + sum(X[i,j]*beta[j] for j=1:p)) <= (1-z[i])*M)
@constraint(model, [i=1:n], (beta0 + sum(X[i,j]*beta[j] for j=1:p)) >= -M*z[i])

#if y=-1 and prediction/loss is positive, fp = 1
@constraint(model, [i=1:n], f[i] >= (-y[i] + (1/M) * (beta0 + sum(X[i,j]*beta[j] for j=1:p)) - 1)/2)
#if y=1 fp = 0
@constraint(model, [i=1:n], f[i] <= M*(-y[i] + 1))
#if y=-1 and prediction is negative (true negative), fp = 0 
@constraint(model, [i=1:n], f[i] <= (-y[i] + (1/M) * (beta0 + sum(X[i,j]*beta[j] for j=1:p))) )

#fp gap should be less than 10%

@constraint(model, (1/nm)*sum(f[i] for i in minorities) <= (1/nnm)*sum(f[i] for i in nonminorities) + e)
@constraint(model, (1/nm)*sum(f[i] for i in minorities) >= (1/nnm)*sum(f[i] for i in nonminorities) - e)

optimize!(model)
opt_beta = value.(beta)
opt_beta0 = value.(beta0)


outfile = "betas.txt"
f = open(outfile, "w")
println(f,opt_beta0)
for i in opt_beta
    println(f,i)
end
