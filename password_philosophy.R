data = read.table("data/password_philosophy")
ranges = strsplit(data[,1],split="-")
ranges = matrix(as.numeric(unlist(ranges)),2)

letters = substr(data[,2],1,1)
passwords = data[,3]

# PART 1
counts = str_count(passwords,letters)
sum(counts >= ranges[1,] & counts <= ranges[2,])

# PART 2
total = 0
for (i in 1:1000) {
  first = substr(passwords[i],ranges[1,i],ranges[1,i])==letters[i]
  second = substr(passwords[i],ranges[2,i],ranges[2,i])==letters[i]
  
  total = total + xor(first,second)
}
print(total)