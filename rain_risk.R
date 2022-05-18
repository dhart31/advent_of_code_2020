rotate <- function(theta,curr_dir){
  theta = theta*pi/180
  r = round(matrix(c(cos(theta),-sin(theta),sin(theta),cos(theta)),nrow=2,ncol=2,byrow=TRUE))
  return(c(r %*% curr_dir))
}

data = read.table("data/rain_risk")[[1]]

actions = substring(data,1,1)
values = as.numeric(substring(data,2,))

curr_dir = c(1,0)
curr_pos = c(0,0)

# FOR PART 2
waypoint = c(10,1)

test = rotate(90,curr_dir)


for (i in 1:length(actions)){
  if (actions[i]=='N'){
    waypoint[2] = waypoint[2] + values[i]
  }
  else if (actions[i] == 'S'){
    waypoint[2] = waypoint[2] - values[i]
  }
  else if (actions[i] == 'E'){
    waypoint[1] = waypoint[1] + values[i]
  }
  else if (actions[i] == 'W'){
    waypoint[1] = waypoint[1] - values[i]
  }
  else if (actions[i] == 'F'){
    curr_pos = curr_pos + waypoint*values[i]
  }
  else if (actions[i] == 'L'){
    waypoint = rotate(values[i],waypoint)
  }
  else if (actions[i] == 'R'){
    waypoint = rotate(-values[i],waypoint)
  }
}

print(sum(abs(curr_pos)))