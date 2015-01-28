library("stringi")

f <- file("C:\\Users\\Marta\\Dropbox\\spiewnik\\tekst.txt")
open(f,"r")
g <- file("C:\\Users\\Marta\\Dropbox\\spiewnik\\tekst2.txt","w")

while(length(x <- readLines(f)) != 0){
   x <- stri_paste(x,"\\\\")
   writeLines(x,g)
}
close(g)
close(f)






