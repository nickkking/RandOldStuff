d=scan(file="",what=double(),n=2)

x <- function(a,c){
  a1=1.01*10^5+a*50
  c1=1.01*10^5+c*50
  v =(log10(a1)-log10(1.01*10^5))/(log10(a1)-log10(c1))
  v
}
x(d[1],d[2])
