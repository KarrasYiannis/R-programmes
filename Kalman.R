# Optimal Control Theory
# Kalman Filter
# Karas Yiannis - Spanos Romanos

# Importing Library
library(ggplot2)
library(matlib)

m = matrix(c(0,1,0,1) , ncol = 4 , nrow = 20, byrow = TRUE)
z = matrix(c(0,0,0,0) , ncol = 4 , nrow = 20, byrow = TRUE)

colnames(m) <- c("x" ,"v_x","y","v_y" )
colnames(z) <- c("x" ,"v_x","y","v_y" )
dt <- .3
(Pplus <- matrix(c(1000,0,0,0,
                   0,1000,0,0,
                   0,0,1000,0,
                   0,0,0,1000)
                ,nrow = 4,ncol =4, byrow = TRUE))
(Xplus <- matrix(0,nrow = 4,ncol =1, byrow = TRUE)) 
telikes_x<-matrix(0,nrow = 4,ncol =1, byrow = TRUE)

k<-2
set.seed(3)
for(k in 2:20 ) {
  m[k,1] <- m[k-1, 1] + m[k-1, 2]*dt + rnorm(1, 0, 1) 
  m[k,2] <- m[k-1, 2] + rnorm(1, 0, 1)
  m[k,3] <- m[k-1, 3] + m[k-1, 4]*dt + -0.5*10*dt^2 + rnorm(1, 0, 1)
  m[k,4] <- m[k-1, 4] - 10*dt + rnorm(1, 0, 1)
  z[k,1] <- m[k,1] + rnorm(1, 0, 1) 
  z[k,2] <- m[k,2] + rnorm(1, 0, 1)
  z[k,3] <- m[k,3] + rnorm(1, 0, 1)
  z[k,4] <- m[k,4] + rnorm(1, 0, 1)
  (Fa <- matrix(c(1,dt,0,0,
                  0, 1,0,0,
                  0,0, 1,dt,
                  0,0,0,1),
               ncol = 4 ,nrow = 4 , byrow = TRUE))
  z_ <- matrix(c(z[k, 1], z[k, 2], z[k, 3], z[k, 4]),
              ncol = 1 , nrow = 4 ,byrow = FALSE)
  (Q<-diag(4))
    #Compute prior
  (p_ <- Fa %*% Pplus  %*%Fa^T + Q)
  (X_ <- Fa %*% Xplus)
  #ComputeK_k
  (K <- p_ %*% inv(p_ +diag(4))) 
  #Compute  P_k(+) and x_k(+)
  pp <- (diag(4) - K)%*%p_ 
  (xx <-  (diag(4) - K ) %*%X_ +K%*%z_)    
  Pplus <-pp
  
  (telikes_x <- cbind(telikes_x,xx))
  (Xplus <-xx)
}

m <- as.data.frame(m) ; m
z <- as.data.frame(z) ; z

telikes_x 
plot(telikes_x[1,],telikes_x[3,],type = "b")

# Scatterplot for States
ggplot(m, aes(x, y)) + 
  geom_point(color = "blue", size = 2.5) + 
  geom_path(color = "blue") + 
  geom_label(aes(label = rownames(m)), nudge_x = 0.1, nudge_y = -5, size = 3) +
  ggtitle("Scatterplot for Kalman Filter") +
  xlab("x - Axis") +
  ylab("y - Axis") +
  geom_vline(xintercept = 0, size = 1) +
  geom_hline(yintercept = 0, size = 1)

# Scatterplot for Observations
ggplot(z, aes(x, y)) + 
  geom_point(color = "blue", size = 2.5) + 
  geom_path(color = "blue") + 
  geom_label(aes(label = rownames(z)), nudge_x = 0.1, nudge_y = -5, size = 3) +
  ggtitle("Scatterplot for Kalman Filter") +
  xlab("x - Axis") +
  ylab("y - Axis") +
  geom_vline(xintercept = 0, size = 1) +
  geom_hline(yintercept = 0, size = 1)


