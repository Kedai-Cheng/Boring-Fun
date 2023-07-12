## p is the proportion covered. 0<=p<=1 ##
## angle.func returns the angle of the target ball. ##
angle.func <- function(p){
  180/(pi/asin(((1-p)*2)/2))
}

angle.func(0.5)

## angle is the angle of the target ball. 0<=angle<=90 ##
## prop.func returns the proportion to cover. ##
prop.func <- function(angle){
  1-sin((angle*pi)/180)
}

prop.func(45)
