library(dplyr)
library(ggplot2)
library(lpSolve)


is_feasible <- function (x1, x2){
  # Constraints for the Ready Mikks Problem
  if((x1 + 2*x2 <= 6) &
     (2*x1 + x2 <= 8) &
     (-x1 + x2  <= 1) &
     (x2 <= 2)) TRUE
  else
    FALSE
}

# A function to evaluate the Ready Mikkks objective function
obj_func <- function(x1,x2){
  3*x1 + 2*x2
}

# (1) Show feasible space
set.seed(100)
N = 40
x1_range   <- seq(0,5,length.out=N)
x1_range
x2_range   <- seq(0,5,length.out=N)
comb_x1_x2 <- expand.grid(x1_range,x2_range)
exper <- tibble(x1=comb_x1_x2[,1],x2=comb_x1_x2[,2])


exper <- exper %>%
  dplyr::rowwise() %>%
  dplyr::mutate(Feasible=is_feasible(x1,x2))
summary(exper)

p1 <- ggplot(exper,aes(x=x1,y=x2,colour=Feasible))+geom_point(size=0.25)+
  geom_abline(mapping=aes(slope=-0.5,intercept=6/2),size=0.4)+
  geom_abline(mapping=aes(slope=-2,intercept=8),size=0.4)+
  geom_abline(mapping=aes(slope=1,intercept=1),size=0.4)+
  geom_abline(mapping=aes(slope=0,intercept=2),size=0.4)
p1

# (2) Generate optimal solution

# Objective function
z <- c(3,2)
z

# Constraints
cons <- matrix(c(1,2,
                 2,1,
                 -1,1,
                 0,1),byrow = T,nrow=4)
cons

# Equalities
eql <- c("<=", "<=", "<=","<=")
eql

# RHS
rhs <- c(6,8,1,2)
rhs

# Solve
opt <- lp("max",z,cons,eql,rhs)
# Show the status
opt$status
# Show the objective function value
opt$objval
# Show the solution points
opt$solution


