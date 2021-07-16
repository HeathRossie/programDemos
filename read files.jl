### importing multiple csv files are often used in real analysis
### here is an example of flow in Julia

# add required packages
# Pkg.add("CSV")
# Pkg.add("Glob")
# Pkg.add("DataFrames")
using CSV, DataFrames, Glob

# to import a csv, CSV.read()
df = CSV.read("file path")


# import multiple csv files at once
# change directory and find csv files
#cd("file path")
cd("/Users/matsuihiroshi/Desktop/test")
files = Glob.glob("*.csv")
# the varible is a name list of csv files

# R equivalent to lapply(files, read.csv) %>% do.call(rbind,.)
CSV.read.(files) |> (y -> reduce(vcat, y)) |> print
print(df)

# if we need an index column which represents each csv file
# make a modified read() function
function read_data(file)
df = CSV.read(file)
df.file = file
return df
end

df = read_data.(files) |> (y -> reduce(vcat, y))
print(df)
