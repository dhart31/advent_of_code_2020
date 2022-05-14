text = readLines("data/toboggan_trajectory_test")

data = t(matrix(unlist(strsplit(text,split=''))=='#',nchar(text[1])))

xvals = c(1,3,5,7,1)
yvals = c(1,1,1,1,2)

tree_multiple = 1

for (i in 1:length(xvals)) {
  row_vals = 1+seq(yvals[i],dim(data)[1]-1,yvals[i])
  col_vals = ((1:length(row_vals)*xvals[i]))%%dim(data)[2]+1
  
  total = 0
  for (i in 1:length(row_vals)){
    total = total + data[row_vals[i],col_vals[i]]
  }
  tree_multiple = tree_multiple * total
}
print(tree_multiple)