x <- c(0,70,40, 0, 40, 40,cenx)
y <- c(10,0,70, 10, 70, 32,ceny)
xpoints <- c(0,70,40,0,40)
ypoints <- c(10,0,70,10,70)
# label points A, B, C with their head values
name <- c("A, h=9", "B, h=50", "C, h=120", "", "", "(40,32)", "centroid")
df = data.frame(xpoints, ypoints)

# plot triangle showing the point in question
plot(df, main="Finite Element Triangle", xlim=c(-10,80), ylim = c(-10,80))

lines(xpoints,ypoints, type = "l")
text(x,y,labels=name)

# solve three h equations to get constants a, b, and c (note lowercase)
# equations: hA = a+c*10, hB = a+70*b, hC = a+40*b+70*c

A <- matrix(data=c(1,0,10,1,70,10,1,40,70), nrow=3, ncol=3, byrow=T)
b_mat <- matrix(data=c(9,50,120), nrow=3, ncol=1, byrow=F)
abcvals <- round(solve(A,b_mat),3)
a <- abcvals[1]
b <- abcvals[2]
c <- abcvals[3]
H_A <- b_mat[1]
H_B <- b_mat[2]
H_C <- b_mat[3]

# define area of the element using the matrix determinant method
area_matrix <- matrix(data=c(0,10,1,70,0,1,40,70,1), nrow=3, ncol=3, byrow=T)
area <- 0.5*det(area_matrix)
# calculate head at point (40,32)
N_A <- (1/(2*area))*((70*70-40*0) + ((0-70)*40) + ((40-70)*32))
N_B <- (1/(2*area))*((40*10-0*70) + (70-10)*40 + (0-40)*32)
N_C <- (1/(2*area))*((0*0-70*10) + (10-0)*40 + (70-0)*32)
# answer to problem 1, head at (40,32)  
h <- (N_A*H_A + N_B*H_B + N_C*H_C)

# Second part of Problem 1: 
# Show that the basis function for node A is equal to 1 when 
# calculated at node A and equal to 0 when evaluated at 
# nodes B and C. 
N_A_atA <- (1/(2*area))*((70*70-40*0) + ((0-70)*0) + ((40-70)*10))
N_A_atB <- (1/(2*area))*((70*70-40*0) + ((0-70)*70) + ((40-70)*0))
N_A_atC <- (1/(2*area))*((70*70-40*0) + ((0-70)*40) + ((40-70)*70))

# calculate centroid of the triangle
cenx <- (0+70+40)/3
ceny <- (10+0+70)/3

# added centroid to plot in code above (line 8)
# check to see if basis functions are equal at the centroid
N_A_cen <- (1/(2*area))*((70*70-40*0) + ((0-70)*cenx) + ((40-70)*ceny))
N_B_cen <- (1/(2*area))*((40*10-0*70) + (70-10)*cenx + (0-40)*ceny)
N_C_cen <- (1/(2*area))*((0*0-70*10) + (10-0)*cenx + (70-0)*ceny)


