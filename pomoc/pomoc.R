library("stringi")

f <- file("tekst.txt")
open(f,"r")
g <- file("tekst2.txt","w")

while(length(x <- readLines(f)) != 0){
   x <- stri_paste(x,"\\\\")
   writeLines(x,g)
}
close(g)
close(f)






