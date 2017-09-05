a = c(xleft = 5, ybottom = 7, xright = 10, ytop = 10)
class(a) = 'rect'
str(a)
attributes(a)

#a, b 정하는 형식이 다르다. class3

b = list(center = c(0, 10), radius = 3)
class(b) = 'circle'
str(b)


area = function(x) {
  UseMethod('area', x)
}

#클래스 이름 풀네임으로 적어야.
area.rect = function(x){
  as.numeric((x['xright'] - x['xleft']) * 
               (x['ytop'] - x['ybottom']))
}


#circle은 리스트 형식으로 했기에 리스트로 받아야한다.

area.circle = function(x){
  as.numeric(pi*x$radius^2)
}

methods('area')
getS3method('area', 'circle')

getS3method('lm')
?getS3method

area.rect(a)
###############################위까지 패키지를 만드는 것.

rm(list=ls())
x = c(1,2,3)
y = c(1,3,8)
tr.data = data.frame(y = y, x = x)
tr.data
lmout = lm (y~x, data = tr.data)
class(lmout)
lmout
print(lmout)
lmout1 = unclass(lmout)
class(lmout)
mes(lmout)
#여러가지 정보를 담고 있었음을 알 수 있다.


print(lmout1)



methods(print)
print.lm
getAnywhere('print.lm')

stat:::print.lm


#모델을 리스트에 저장해 한꺼번에 돌려보는이..

print.employee = function(a)
{
  cat(a$name,'\n')
  cat('salary is ',a$salary,'\n')
  cat('union member is', a$union,'\n')
}
methods(,'employee')


################################
x = seq(from = 0.5, to = 10, by = 0.5)
y = 0.01 * x ^ 2 + 10 * x #제곱이지만 회귀계수가 작아 리니어첢.

plot(x, y, type = 'b', main ='scatter plot', xlim = c(-10, 10), ylim = c(-10, 100), ylab = expression(0.01*x^2 + 10 * x))


lm.fit = lm(y ~ x) #lm 은 4개가 나온다.
par(mfrow = c(2,2)) #파티션을 나눈다. mf row, 그림들어가는 순서가 로우기준
plot(lm.fit)


plot.rect = function(x, xmargin = 5, ymargin = 5, col = 'red'){
  plot
  
  
plot.circle = function(x, xmargin = 5, ymargin = 5, col = 'blue'){
  centers = unlist(x['center'])
  radius = as.numeric(unlist(x['center']))
  theta =seq
}
}
