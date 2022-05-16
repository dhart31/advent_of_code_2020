data = read.table("data/handheld_halting")

for (i in 1:length(ops)){
  idx = 1
  acc = 0
  visited_idxs = vector()
  ops = data[[1]]
  args = data[[2]]
  ops[i] = "nop"
  args[i] = 0
  while (!(idx %in% visited_idxs)){
    visited_idxs = append(visited_idxs,idx)
    
    if (ops[idx]=='nop'){
      idx = idx + 1
    }
    
    else if (ops[idx]=='jmp'){
      idx = idx + args[idx]
    }
    
    else if (ops[idx]=='acc'){
      acc = acc + args[idx]
      idx = idx + 1
    }
    
    if (idx==length(ops)) break
  }
  # Comment out for part 1
  if (idx == length(ops)) break
}
print(acc)


