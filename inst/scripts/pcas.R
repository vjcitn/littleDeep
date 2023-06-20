
library(littleDeep)
MAX_RADIUS = 27
N_PER_TYPE = 150
NPTS_CIRC = 90
circs = vector("list", N_PER_TYPE)
tris = vector("list", N_PER_TYPE)
seqs = rep(NA, 2*N_PER_TYPE)
intern = rep(NA, 2*N_PER_TYPE)
j = 1
 set.seed(1234)
 myjpc = array(1, dim=c(N_PER_TYPE,64,64,3)) # store images with 0 for dark
 for (i in seq_len(N_PER_TYPE)) { 
    z = rancirc(npts=NPTS_CIRC, max_radius=MAX_RADIUS) 
    circs[[i]] = z
    p = load_jpeg(z$x, z$y, siz=sample(c(20,40,60),size=1)) 
    myjpc[i,,,] = p 
    seqs[j] = j
    j = j+1
    intern[j] = i
    }

mydat = matrix(1, nrow=N_PER_TYPE, ncol=4096)
for (i in 1:N_PER_TYPE) mydat[i,] = as.numeric(myjpc[i,,,1])
nn = prcomp(mydat)
#summary(nn)
#pairs(nn$x[,1:4])

 myjpt = array(1, dim=c(N_PER_TYPE,64,64,3)) # store images with 0 for dark
 for (i in seq_len(N_PER_TYPE)) { 
    z = rantri() 
    tris[[i]] = z
    p = load_jpeg(z$x, z$y, siz=sample(c(20,40,60),size=1)) 
    myjpt[i,,,] = p 
    seqs[j] = j
    j = j+1
    intern[j] = i
    }
mydatt = matrix(1, nrow=N_PER_TYPE, ncol=4096)
for (i in 1:N_PER_TYPE) mydatt[i,] = as.numeric(myjpt[i,,,1])
nnt = prcomp(mydatt)

#pairs(nnt$x[,1:4])

alld = matrix(1, nrow=2*N_PER_TYPE, ncol=4096)
for ( i in 1:N_PER_TYPE ) alld[i,] = as.numeric(myjpc[i,,,1])
for ( i in 1:N_PER_TYPE ) alld[N_PER_TYPE+i,] = as.numeric(myjpt[i,,,1])
rg = rep(c("red", "green"),each=N_PER_TYPE)
nnb = prcomp(alld)
pairs(nnb$x[,1:4],pch=19, col=rg)
