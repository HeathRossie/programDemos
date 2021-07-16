ENV["R_HOME"]="/Library/Frameworks/R.framework/Resources"
ENV["PATH"]="....directory of R executable..."
using RCall

pwd()

R"library(rstudioapi)"
R"setwd(dirname(rstudioapi::getActiveDocumentContext()$path))"
