rm(list = ls())

##---- S3 Class

## example
a = c(xleft = 5, ybottom = 7, xright = 10, ytop = 10)
class(a) = 'rect' #rectangle
str(a)
attributes(a)

b = list(center = c(0,10), radius = 3)
class(b) = 'circle'
str(b)

area = function(x){
  UseMethod('area', x)
}

area.rect = function(x){
  as.numeric((x['xright']-x['xleft'])*
               (x['ytop']-x['ybottom']))
}

area.circle = function(x){
  as.numeric(pi*x$radius^2)
}

methods('area')
getS3method('area', 'circle')
area(a)
area.rect(a)
area(b)


## S3 Class 만들기
# S3 class: r에서 정의한 객체로 이름의 할당능력이 있는 리스트

x = c(1, 2, 3)
y = c(1, 3, 8)
tr.data = data.frame(y = y, x = x)
lmout = lm(y~x, data = tr.data)
class(lmout)

print(lmout) # print.lm

lmout1 = unclass(lmout)
class(lmout1)

names(lmout1)
print(lmout1)

lmout1[[1]]
lmout1$coefficients

## S3 class와 Generic function

methods(print)
getAnywhere('print.lm') # retrieve an R object
getS3method('print', 'lm')

m.print = function(x, digits = max(3L, getOption("digits") - 3L)){
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
      "\n\n", sep = "")
  if (length(coef(x))) {
    cat("Coefficients:\n")
    print.default(format(coef(x), digits = digits), print.gap = 2L, 
                  quote = FALSE)
  }
  else cat("No coefficients\n")
  cat("\n")
  invisible(x)
}

m.print(lmout)
print(lmout)

getAnywhere('print.lm') # retrieve an R object

stats:::print.lm
stats:::print.lm(lmout)


## 새로운 S3 Class 함수 만들기

j = list(name = 'Joe', salary = 55000, union = T)
class(j) = 'employee'
j
attributes(j)

methods(,'employee')

print.employee = function(a){
  cat(a$name,'\n',
      'salary is ',a$salary,'\n',
      'union member is', a$union,'\n')
}

methods(,'employee')

j

##---- Plot

## basic
x = seq(from = 0.5, to = 10, by = 0.5)
y = 0.01*x^2 + 10* x
plot(x, y)

plot(x, y, type = "b", main = "scatter plot",
     xlim = c(-10, 10), ylim = c(-10, 100),
     xlab = "x", 
     ylab = expression(0.01*x^2+10*x), 
     pch = 19, lwd = 2.5, col = 'blue')


## for linear model
lm.fit = lm(y~x)
par(mfrow = c(2,2))
plot(lm.fit)
par(mfrow = c(1,1))

## for example data
str(a)
str(b)

plot.rect = function(x, xmargin = 5, ymargin = 5, col = "red"){
  plot(0, 0, type = 'n',
       xlim = x[c('xleft', 'xright')] + c(-xmargin, xmargin), 
       ylim = c(x['ybottom']-ymargin, x['ytop']+ymargin),
       xlab = "",ylab = "")
  rect(x['xleft'], x['ybottom'], x['xright'], x['ytop'],
       col = col)
}
plot(a)

plot(a, xmargin = 3, 
     ymargin = 2, col = "blue")

plot.circle = function(x, xmargin = 5, 
                       ymargin = 5, col = "blue"){
  centers = unlist(x['center'])
  radius = as.numeric(unlist(x['radius']))
  theta = seq(0, 2*pi, length = 250)
  plot(x = centers[1] + radius*cos(theta), 
       y = centers[2] + radius*sin(theta), 
       type = 'l',
       xlim = centers[1] + c(-xmargin, xmargin), 
       ylim = centers[2] + c(-ymargin, ymargin),
       xlab = "",ylab = "", lwd = 2, col = col)
}

plot(b)

##---- S4 Class

## S4 Class 만들기
setClass("jjj",
         representation(
           name ="character",
           salary = "matrix",
           union = "logical")
)

a4 = new('jjj')
a4@name = c('a','b')
# a4@salary = 1
a4@salary = matrix(1,1,1)
a4@union = c(T, F)
slot(a4, 'union') = c(F, F)

show(a4)
print(a4)
str(a4)

a4.1 = new('jjj', name = c('a','b'),
           salary = matrix(1,1,1),
           union = c(F, F))

str(a4.1)

a5 = unclass(a4)
print(a5)

setMethod("show", "jjj",
          function(object){
            cat("jjj like ", object@name,'\n' )
          })

show(a4)

## 직사각형 S4 class을 만들고 
## S3 class 예제와 마찬가지로 
## 그림 그리는 Method 생성.

setClass("rectangle",
         representation(
           xleft ="numeric",
           xright = "numeric",
           ybottom = "numeric",
           ytop = "numeric")
)

a_s4 = new('rectangle', xleft = 5, xright = 10,
           ybottom = 7, ytop = 10)

setMethod('plot','rectangle', 
          function(x, xmargin = 5, ymargin = 5, col = "red"){
            plot(0, 0, type = 'n',
                 xlim = c(x@'xleft'-ymargin, x@'xright'+ymargin), 
                 ylim = c(x@'ybottom'-ymargin, x@'ytop'+ymargin),
                 xlab = "",ylab = "")
            rect(x@'xleft', x@'ybottom', x@'xright', x@'ytop',
                 col = col)
          })
plot(a_s4)
##---- 객체 저장하기
setwd('D:/jeon')
getwd()
rm(list = ls(pattern = 'employee'))
save(a4, a, b, file = 's4class.RData')
save.image('D:/s4.RData')

rm(list = ls())
load('D:/s4class.RData')
ls()

rm(list = ls())
load('D:/s4.RData')
ls()

as.data.frame(summary(lmout)$coefficients)

##---- Connection

rm(list = ls())
a = matrix(1:1000,1000,10)
head(a)

write.table(a, 'D:/a1.csv', sep = ',',
          col.names = F, row.names = F)

write.table(a, 'D:/a.csv', sep=' ', 
            col.names = F, row.names = F)
aa = file('D:/a.csv', open='r' )
readLines(aa,n=1)

readLines(aa,n=1)

readLines(aa,n=1)

seek(aa, where = 0)

readLines(aa,n=1)
close(aa)


p = url('http://egloos.zum.com/zoot235/v/10088226', open = 'r')
ch.vec = c()
idx = 1
while(TRUE)
{
  tmp = readLines(p,n=1, warn = FALSE)
  if (length(tmp) == 0 ) break
  ch.vec[idx] = tmp
  idx = idx + 1
}
close(p)
ch.vec[1:10]



##############################################################################
##############################################################################

##---- Webcrawling

## URL connection

rm(list = ls())
i = 1
pn.num = c(1:2)
f1.layer.list = vector('list', length = length(pn.num))
for ( i in 1:length(pn.num))
{
  cat(i,'\n')
  p = url(paste0("http://biz.chosun.com/svc/list_in/",
                 "list_title.html?catid=1&pn=", pn.num[i]), 
          open = 'r')
  ch.vec = c()
  idx = 1
  while(TRUE)
  {
    tmp = readLines(p,n=1, warn = FALSE, encoding = 'UTF-8')
    if (length(tmp) == 0 ) break
    ch.vec[idx] = tmp
    idx = idx + 1
    # cat(idx,'\n')
  }
  close(p)
  f1.layer.list[[i]] = ch.vec
}

f1.layer.list[[1]][1:10]


## 링크 추출
idx = 1
f1.layer.url = c()
for ( i in 1:length(pn.num))
{
  tmp = f1.layer.list[[i]]
  for ( j in 1:length(tmp))
  {
    tmp1= tmp[[j]]
    v = grep("/site/data/html_dir/[0-9]{4}/[0-9]{2}", tmp1 )
    if (length(v) > 0)
    {
      a = regexpr("href=", tmp1 )
      b = regexpr("[0-9]{13}.html", tmp1 )
      f1.layer.url[idx] = substr(tmp1,a+6, b+17)
      idx = idx + 1
    }
  }
}

length(f1.layer.url)
head(f1.layer.url)

## 링크 재연결 및 페이지 다운로드 
i = 2
p = url(paste0('http://biz.chosun.com',f1.layer.url[i]),
        open = 'r')
ch.vec = c()
idx = 1
while(TRUE)
{
  tmp = readLines(p,n=1, warn = FALSE, encoding = 'UTF-8')
  if (length(tmp) == 0 ) break
  ch.vec[idx] = tmp
  idx = idx + 1
}
close(p)

## Text 재추출
rec = FALSE
tmp1 = list()
idx = 1
for ( i in 1:length(ch.vec))
{
  v = grep("<!-- article -->", ch.vec[[i]] )
  if (length(v) > 0 ) rec <- !rec
  if (rec)
  {
    tmp1[idx] = ch.vec[[i]]
    idx = idx + 1
  }
}
tmp1
#
tmp2 = list()
idx = 1
for ( i in 1:length(tmp1))
{
  v = grep("<br>", tmp1[[i]] )
  if (length(v)>0) 
  {
    tmp2[[idx]] = tmp1[[i]]
    idx = idx + 1
  }
}
tmp2
tmp2 = lapply(1:length(tmp2), 
              function(i) gsub('\t', '', tmp2[[i]]))

get_paper_numbers = function(keyword){
  p = url(paste0('http://snu-primo.hosted.exlibrisgroup.com/primo_library/libweb/action/search.do?fn=search&ct=search&frbg=&initialSearch=true&mode=Basic&tab=all&indx=1&dum=true&srt=rank&vid=82SNU&tb=t&vl%28freeText0%29=',
                 keyword,'&scp.scps=scope%3A%2882SNU_SSPACE%29%2Cscope%3A%2882SNU_INST%29%2Cscope%3A%2882SNU_COURSE%29%2Cscope%3A%2882SNU_ROSETTA%29%2Cprimo_central_multiple_fe&prefLang=ko_KR')
          ,open = 'r')
  ch.vec = c()
  idx = 1
  while(TRUE){
    tmp = readLines(p,n=1, warn = FALSE, encoding = 'UTF-8')
    if (length(tmp) == 0 ) break
    ch.vec[idx] = tmp; idx = idx + 1
  }
  close(p)
  ## Text 재추출
  tmp1 = list(); idx = 1
  for ( i in 1:length(ch.vec)){
    v = grep("학술논문", ch.vec[[i]] )
    if (length(v) > 0 ){
      tmp1[idx] = ch.vec[[i]]; idx = idx + 1
    }}
  if (length(tmp1) ==0) return(0)
  else{
    tt = regexpr("class=\"EXLParenthesis\">", tmp1 )
    tmp1 = substr(tmp1, tt+24, 
                  length(unlist(strsplit(tmp1[[1]], '')))-8)
    tmp1 = gsub(',', '', tmp1)
    return(as.numeric(tmp1))
  }
}

#

library(XML)
library(RCurl)

url = paste0('http://snu-primo.hosted.exlibrisgroup.com/primo_library/libweb/action/search.do?fn=search&ct=search&frbg=&initialSearch=true&mode=Basic&tab=all&indx=1&dum=true&srt=rank&vid=82SNU&tb=t&vl%28freeText0%29=',
             keyword,'&scp.scps=scope%3A%2882SNU_SSPACE%29%2Cscope%3A%2882SNU_INST%29%2Cscope%3A%2882SNU_COURSE%29%2Cscope%3A%2882SNU_ROSETTA%29%2Cprimo_central_multiple_fe&prefLang=ko_KR')

html = getURLContent(url)
doc = htmlParse(html,asText=T)
xPath1 = c(detail = "//div[@id='facetListTopLevel']")
tmp = as.character(xpathSApply(doc,xPath1,xmlValue))

tmp1 = gsub("\t", "", tmp)
tmp1 = unlist(strsplit(tmp1, "\n"))
grep("학술논문" , tmp1, value = T)

keywords = c("lab","water","industry", 
             "science", "visualization")
keyword = "visualization method"
get_paper_numbers(gsub(" ", "+", keyword))
