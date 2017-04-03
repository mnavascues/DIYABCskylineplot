next_theta <- function(theta,lower_limit,upper_limit)
{
  theta <- theta + runif(n,-1,1)
  theta <- apply(as.array(theta),1,function(x) min(x,upper_limit) )
  theta <- apply(as.array(theta),1,function(x) max(x,lower_limit) )
  
}
n <- 1000000
lower_limit <- -3
upper_limit <- 4
theta0 <- runif(n,lower_limit,upper_limit)
theta1 <- next_theta(theta0,lower_limit,upper_limit)
theta2 <- next_theta(theta1,lower_limit,upper_limit)
theta3 <- next_theta(theta2,lower_limit,upper_limit)
theta4 <- next_theta(theta3,lower_limit,upper_limit)
theta5 <- next_theta(theta4,lower_limit,upper_limit)
theta6 <- next_theta(theta5,lower_limit,upper_limit)
theta7 <- next_theta(theta6,lower_limit,upper_limit)

cbbPalette <- c("#000000", "#009E73", "#e79f00", "#9ad0f3", "#0072B2", "#D55E00", 
                "#CC79A7", "#F0E442")

pdf(file=paste0("results/CorrelatedPrior.pdf"),width=5,height=6)
plot(density(theta0),
     ylim=c(0,0.21),
     main="prior probability distribution",
     xlab=expression(log[10]*theta))
lines(density(theta1),col=cbbPalette[2])
lines(density(theta2),col=cbbPalette[3])
lines(density(theta3),col=cbbPalette[4])
lines(density(theta4),col=cbbPalette[5])
lines(density(theta5),col=cbbPalette[6])
lines(density(theta6),col=cbbPalette[7])
lines(density(theta7),col=cbbPalette[8])
legend(x="bottom",
       legend=c(expression(theta[0]),
                expression(theta[1]),
                expression(theta[2]),
                expression(theta[3]),
                expression(theta[4]),
                expression(theta[5]),
                expression(theta[6]),
                expression(theta[7])),
       lty=1,
       col=cbbPalette)
dev.off()
