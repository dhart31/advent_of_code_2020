library(readr)
library(hash)
required_fields = c("byr","iyr","eyr","hgt","hcl","ecl","pid")

VectorIntersect <- function(v,z) {
  unlist(lapply(unique(v[v%in%z]), function(x) rep(x,min(sum(v==x),sum(z==x)))))
}
# check that all elements of v are in z
is.contained <- function(v,z) {length(VectorIntersect(v,z))==length(v)}


is_valid_field <- function(f,v){

  if (f == "byr"){
    v = as.integer(v)
    result = (v>=1920 & v<=2002)
  }
  
  else if (f == "iyr"){
    v = as.integer(v)
    result = (v>=2010 & v<=2020)
  }
  
  else if (f == "eyr"){
    v = as.integer(v)
    result = (v>=2020 & v<=2030)
  }
  
  else if (f == "hgt"){
    # Get height magnitude
    v_mag = as.integer(gsub("[a-z]","",v))
    # Get height units, in cm or in
    v_unit = gsub("[^a-z]","",v)
    if (v_unit=='in'){
      result = (v_mag >= 59 & v_mag <= 76)
    }
    else if (v_unit=='cm'){
      result = (v_mag >= 150 & v_mag <= 193)
    }
    else {
      result = FALSE
    }
  }
  
  else if (f == "hcl"){
    result = (substr(v,1,1)=='#' & grepl("^[a-f0-9]{6}$",substr(v,2,nchar(v))))
  }
  
  else if (f == "ecl"){
    colors = c("amb","blu","brn","gry","grn","hzl","oth")
    if (v %in% colors){
      result = TRUE
    }
    else {
      result = FALSE
    }
  }
  
  else if (f == "pid"){
    result = grepl("^[0-9]{9}$",v)
  }
  
  else if (f == 'cid'){
    result = TRUE
  }
  return(result)
}
  
  


data = read_file("data/passport_processing")
data = strsplit(data,"\n\n")[[1]]
total1 = 0
total2 = 0
for (passport in data){
  passport = gsub("\n"," ",passport)
  passport = strsplit(passport," ")[[1]]
  # Get passport fields and values
  passport_fields = substring(passport,1,3)
  passport_values = substring(passport,5,)
  # Check if all required fields are present
  if (is.contained(required_fields,passport_fields)){
    total1 = total1 + 1 # for part 1
    # Check that all fields have valid values
    if (all(mapply(is_valid_field,passport_fields,passport_values))){
    total2 = total2 + 1 # for part 2
    }
  }
}

print(total1)
print(total2)