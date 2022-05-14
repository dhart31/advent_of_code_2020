data = as.numeric(unlist(read.table("data/report_repair")))
# PART 1
pairs = combn(data,2)
pair = pairs[,colSums(pairs,2)==2020]
prod(pair)
# PART 2
triplets = combn(data,3)
triplet = triplets[,colSums(triplets,2)==2020]
prod(triplet)