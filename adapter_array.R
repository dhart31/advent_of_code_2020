splitAt <- function(x, pos) unname(split(x, cumsum(seq_along(x) %in% pos))) # split x at given positions
num_ones <- function(x)sum(x==1) # count ones

# Count how many groups with no more than 3 elements can be formed
comb_sum <- function(n){
  total = 0
  for (i in ceiling(n/3):n){
    total = total + choose(n-1,i-1)
  }
  return(total)
}

data = read.table("data/adapter_array")[[1]]
data = sort(append(data,c(0,max(data)+3))) # Sort joltage ratings
diffs = diff(data) #Get increases in joltage

# Get the size of each group of 1's 
counts = mapply(num_ones,splitAt(diffs,which(diffs==3)))
counts = counts[(counts!=0 & counts!=1)] # Remove 0's and 1's from group, which do not affect final count

# Count number of ways to arrange each one group, multiply group counts together
prod_val = prod(mapply(comb_sum,counts))

print(format(prod_val,scientific=FALSE))