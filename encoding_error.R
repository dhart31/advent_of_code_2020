data = read.table("data/encoding_error")[[1]]

# PART 1
for (i in 26:length(data)){
  if (!(data[i] %in% outer(data[(i-25):(i-1)],data[(i-25):(i-1)],"+"))) break
}
invalid_i = i
invalid_val = data[i]

# PART 2
stop = FALSE
for (i in 1:(invalid_i-2)){
  for (j in (i+1):(invalid_i-1)){
    if (sum(data[i:j]) == invalid_val){
      stop = TRUE
    }
    if (stop) break
  }
  if (stop) break
}
encryption_weakness = min(data[i:j])+max(data[i:j])
print(encryption_weakness)