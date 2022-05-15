library(stringr)

seat_data = read.table("data/binary_boarding")[[1]]

#seat_data = c("BFFFBBFRRR","FFFBBBFRRR","BBFFBBFRLL")
seat_row = strtoi(gsub("B","1",gsub("F","0",substr(seat_data,1,7))),base=2)
seat_col = strtoi(gsub("R","1",gsub("L","0",substr(seat_data,8,nchar(seat_data)))),base=2)
seat_id = seat_row*8+seat_col
# PART 1
max(seat_id)
# PART 2
seat_range = min(seat_id):max(seat_id)
seat_range[!(seat_range %in% seat_id)]
