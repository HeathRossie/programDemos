### PyCall is a module to use python function in Julia

# Pkg.add("PyCall")
# Pkg.update("PyCall")
using PyCall

@pyimport numpy.random as nr
print(nr.rand(3,4))

a = zeros(0)
for i in 1:1000
    append!(a, nr.randn(1))
end

ENV["R_HOME"]="/Library/Frameworks/R.framework/Resources"
ENV["PATH"]="....directory of R executable..."
using RCall
R"library(ggplot2)"
R"ggplot() + geom_histogram(aes(x=$a))"
