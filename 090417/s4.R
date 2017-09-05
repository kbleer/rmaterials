setClass("jjj",
         representation(
           name ="character",
           salary = "matrix",
           union = "logical")
)
a4 <- new('jjj')  #새로운 클래스를 만들겠다.
a4@name <- c('a', 'b')    #세부 항목으로 들어가려면 골뱅이 표시
#a4@salary <- 1 은 안된다. 메이트릭스로 설정을 했기 때문에   
a4@salary<- matrix(1,1,1)  
a4@union <- c(T,F)
slot(a4,'union') <- c(F,F)


show(a4)
str(a4)
#처음에 내가 걸은 클래스를 따르도록 하는 제약조건이 있다.  

a4.1 = new('jjj', name = c('a', 'b'),
           salaey = matrix(1,1,1),
           union = c(F, F))

a5 = unclass(a4) #list화
print(a5)

a5@'union' #하위 항목 접근


#generic fn 지정법.
# jjj 의 출력값을 다른 스타일로
#setmethod는 프린트하고 클래스 적어준거랑 같다.
setMethod("show", "jjj",
          function(object){
            cat("jjj like ", object@name,'\n' )
          })


show(a4)
#######################################################################################
### 직사각형 s4 class 만들고 그림그리는 method 생성

setClass("rectangle",
         representation(
           xleft ="numeric",
           xright = "numeric",
           ybottom = "numeric",
           ytop = "numeric")
)

a_s4 <- new('rectangle', xleft = 5, xright = 10, ybottom = 7, ytop = 10) 

a_s4

setMethod('plot', 'rectangle',
          function(x, xmargin = 5, ymargin = 5, col = 'red'){
            plot(0, 0, type = 'n',
                 xlim = c(x@'xright' - ymargin) + c(x@'xleft' + ymargin),
                 ylim = c(x@'ybottom' - ymargin, c(x@'ytop' + ymargin),
                 xlab = '', ylab = ''))
            rect(x@'xleft', x@'ybottom', x@'xright', x@'ytop', col = col)})

plot(a_s4)
#######################################################################################

a = c(xleft = 5, ybottom = 7, xright = 10, ytop = 10)
class(a) = 'rect'
str(a)
attributes(a)

area = function(x) {
  UseMethod('area', x)
}

#클래스 이름 풀네임으로 적어야.
area.rect = function(x){
  as.numeric((x['xright'] - x['xleft']) * 
               (x['ytop'] - x['ybottom']))
}


plot.rect = function(x, xmargin = 5, ymargin = 5, col = 'red'){
  plot(0, 0, type = 'n',
       xlim = x[c('xleft', 'xright')] + c(-xmargin, xmargin),
       ylim = c(x['ybottom'] - ymargin, x['ytop'] + ymargin),
       xlab = '', ylab = '')
  rect(x['xleft'], x['ybottom'], x['xright'], x['ytop'], col = col)





