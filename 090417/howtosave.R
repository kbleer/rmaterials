rm(list = ls(pattern ='employee'))
#rm(list = ls()) 전체 리스트 지우기
save(a4, 'C:/Users/User/Documents/s4class.rdata')
save.image('D:/s4.rdata') #전체 변수 저장
getwd()

rm(list = ls())
load('C:/Users/User/Documents/s4.Rdata')
ls() #지금 어떤 변수가 저장되어있는지 나온다.
rm(list = ls())
load("d:/s4.rdata")