"""
180330 Hiroshi Matsui
Demo of RCall.jl

RCallはjulia上でRのコードを走らせる便利なパッケージ
基本的なデータの型は互換している上に
Rのlist -> 辞書型
Rのdataframe -> DataFrame
にしてくれるとか。
加えて、Rで操作したものをjuliaのオブジェクトに格納したり、その逆も思いのまま
"""

# 謎のエラーを吐かれたのでここを参考にした
# https://github.com/JuliaInterop/RCall.jl/blob/master/src/setup.jl
ENV["R_HOME"]="/Library/Frameworks/R.framework/Resources"
ENV["PATH"]="....directory of R executable..."

# パッケージの導入
Pkg.add("RCall")
using RCall

# irisデータセットを用意してggplotで作図する
R"library(ggplot2)"
R"data(iris)"
R"ggplot(iris) + geom_boxplot(aes(x=Species, y=Sepal.Length, fill=Species))"

# juliaのDataFrameとRのdataframeが互換であることを見る
using DataFrames
iris = rcopy(R"iris")
typeof(iris)
print(head(iris))


R"library(dplyr)"
iris = rcopy(R"$iris %>% mutate(., warizan=Sepal_Length/Sepal_Width)")
print(head(iris))
# "$"ドルマークは「juliaのオブジェクトを使ってRのコードを走らせるよ」という意味

"""
メモ
・R version should be >3.4
・location of "RHome" can be found by running R.home() in R
・without "R_home", R instration fails
・R_path is also required
"""
