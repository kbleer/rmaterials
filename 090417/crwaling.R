rm(list = ls())
i = 1
pn.num = c(1:2)
f1.layer.list = vector('list', length = length(pn.num))
for ( i in 1:length(pn.num))
{
  cat(i,'\n')
  p = url (paste0("http://biz.chosun.com/svc/list_in/", "list_title.html?catid=1&pn="
                 , pn.num[i], sep=''), open = 'r')
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
  ch.vec[1:10]
  f1.layer.list[[i]] = ch.vec
}


#link substraction

idx = 1
f1.layer.url = c()
for ( i in 1:length(pn.num))
{
  tmp = f1.layer.list[[i]]
  j = 1
  for ( j in 1:length(tmp))
  {
    tmp1= tmp[[j]]
    v = grep("/site/data/html_dir/[0-9]{4}/[0-9]{2}", tmp1 )
    #if (length(v)>0 ) break
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



#re linking

i = 2
p = url(paste0('http://biz.chosun.com',f1.layer.url[i], sep=''), open = 'r')
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

ch.vec[1:10]



#text

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
  v2 = grep("^((\\t+<br>))", tmp1[[i]] )
  if (length(v2) >0) next
  if (length(v)>0)
  {
    tmp2[[idx]] = tmp1[[i]]
    idx = idx + 1
  }
}

tmp2
#for loop를 한줄로 한게 lapply 리스트로. 일부터 몇까지 어떤 펑션 돌릴지.
tmp3 = lapply(1:length(tmp2), function(i) gsub('\t', '', tmp2[[i]]))
tmp3





##########################서울대 도서관   ###
# 검색 후 학술 논문 수 가져오기

keyword = 'coffee'
p = url(paste0('http://snu-primo.hosted.exlibrisgroup.com/primo_library/libweb/action/search.do?fn=search&ct=search&frbg=&initialSearch=true&mode=Basic&tab=all&indx=1&dum=true&srt=rank&vid=82SNU&tb=t&vl%28freeText0%29=',
         keyword, '&scp.scps=scope%3A%2882SNU_SSPACE%29%2Cscope%3A%2882SNU_INST%29%2Cscope%3A%2882SNU_COURSE%29%2Cscope%3A%2882SNU_ROSETTA%29%2Cprimo_central_multiple_fe&prefLang=ko_KR', open = 'r'))

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

rec = FALSE
tmp0 = list()
idx = 1
for ( i in 1:length(ch.vec))
{
  v = grep("학술논문", ch.vec[[i]])
  if (length(v) > 0 )
  {
    tmp0[idx] = ch.vec[[i]]; idx = idx + 1
  }
}
tmp0

tt = regexpr("class=\"EXLParenthesis\">", tmp0)  # \ 표시로 기호도 str
tmp11 = substr(tmp0, tt + 24, length(unlist(strsplit(tmp1[[1]], ''))) - 8)
tmp11 = gsub(',', '', tmp1)
ch.vec[1:10]




install.packages('XML')
install.packages('RCurl')
dev.off()
library(RCurl)

keywords = c('lab', 'water', 'industry', 'science', 'visualization')
for (i in keywords) {
  url = paste0('http://snu-primo.hosted.exlibrisgroup.com/primo_library/libweb/action/search.do?fn=search&ct=search&frbg=&initialSearch=true&mode=Basic&tab=all&indx=1&dum=true&srt=rank&vid=82SNU&tb=t&vl%28freeText0%29=',
              i, '&scp.scps=scope%3A%2882SNU_SSPACE%29%2Cscope%3A%2882SNU_INST%29%2Cscope%3A%2882SNU_COURSE%29%2Cscope%3A%2882SNU_ROSETTA%29%2Cprimo_central_multiple_fe&prefLang=ko_KR', open = 'r')
  html = getURLContent(url)
  doc = htmlParse(html, asText = T)
  doc
  xPath1 = c(detail = "//div[@id='facetListTopLevel']")
  tmp = as.character(xpathSApply(doc, XPath1, xmlValue))
  
  tmp1 = gsub("\t", "", tmp)
  tmp1 = unlist(strsplit(tmp1, "\n"))
  grep("학술논문", tmp1, value = T)
}
tmp1
tmp
