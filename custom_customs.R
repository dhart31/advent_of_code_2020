library(rlang)
data = read_file("data/custom_customs")
data = strsplit(data,"\n\n")[[1]]
data = strsplit(data,"\n")


total1 = 0
total2 = 0
for (group in data){
  # PART 1
  # Get number of unique characters from all strings
  total1 = total1 + length(unique(flatten_chr(str_split(group,""))))
  # PART 2
  #Get the number of intersecting elements from all strings
  total2 = total2 + length(Reduce(intersect,strsplit(group,"")))
}
print(total1)
print(total2)