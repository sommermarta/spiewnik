library("stringi")
library("dplyr")

args <- commandArgs(trailingOnly = TRUE)
args

katalog_ze_spiewnikiem <- ifelse(length(args)==0, "syf", paste(args[1], "//syf", sep="")) 

#########################################################################################################

dane <- readLines(stri_paste(katalog_ze_spiewnikiem, "\\piosenki.idx"))
dane_lista <- stri_match_all_regex(dane, "\\\\indexentry[{](.*?)[}][{](.*?)[}]")

tytuly <- unlist(lapply(dane_lista, function(x) x[2]))
nr_stron <- unlist(lapply(dane_lista, function(x) x[3]))

litery_latex <- c("\\IeC {\\k a}", "\\IeC {\\'c}", "\\IeC {\\k e}", "\\IeC {\\l }", "\\IeC {\\'n}", 
                  "\\IeC {\\'o}", "\\IeC {\\'s}", "\\IeC {\\'z}", "\\IeC {\\.z}",
                  "\\IeC {\\k A}", "\\IeC {\\'C}", "\\IeC {\\k E}", "\\IeC {\\L }", "\\IeC {\\'N}", 
                  "\\IeC {\\'O}", "\\IeC {\\'S}", "\\IeC {\\'Z}", "\\IeC {\\.Z}")
litery_polskie <- c("ą", "ć", "ę", "ł", "ń", "ó", "ś", "ź", "ż",
                    "Ą", "Ć", "Ę", "Ł", "Ń", "Ó", "Ś", "Ź", "Ż")

for(i in 1:length(tytuly)){
  for(j in 1:length(litery_latex)){
    tytuly[i] <- stri_replace_all_fixed(tytuly[i], litery_latex[j], litery_polskie[j])
  }
}

tytuly_posortowane <- sort(tytuly)
nr_stron_posortowane <- nr_stron[order(tytuly)]

stri_extract_all_regex(tytuly_posortowane, "^.") %>%
  unlist() %>%
  stri_extract_all_regex(., "[a-zA-ZąćęłńóśżźĄĆĘŁŃÓŚŹŻ]") %>%
  unlist() %>% 
  stri_trans_toupper() -> pierwsze_litery
pierwsze_litery[which(is.na(pierwsze_litery))] <- "Symbole"

ramka <- data.frame(autor=pierwsze_litery, piosenka=tytuly_posortowane, strona=nr_stron_posortowane)

ramka %>%  group_by(autor) %>% mutate(ile=n()) -> ramka2
ile <- 0
odst1 <- 6
odst2 <- 5
odst3 <- 1.5

sink(file(stri_paste(katalog_ze_spiewnikiem, "\\piosenki.Rnw"), encoding="UTF-8"))

cat("\n\\documentclass[a4paper, 11pt]{extreport}\n\n\\usepackage[T1]{fontenc}\n\\usepackage[polish]{babel}\n\\usepackage[utf8]{inputenc}\n\\usepackage{amsmath}\n\\usepackage{amsfonts}\n\\usepackage{graphicx}\n\\usepackage{setspace}\n\\usepackage{savesym}\n\\savesymbol{arc}\n\\usepackage{color}\n\\usepackage{xcolor}\n\\usepackage{pict2e}\n\\usepackage{epstopdf}\n\\usepackage{geometry}\n\\usepackage{enumerate}\n\\usepackage{multicol}\n\\usepackage[strict]{changepage}\n\\usepackage{titlesec}\n\\usepackage{etoolbox}\n\\usepackage{tocloft}\n\\usepackage{imakeidx}\n\\usepackage{ifthen}\n\\usepackage{fancyhdr}\n\\usepackage{enumitem}\n\n\\setlist[enumerate]{itemsep=0mm}\n\\setlength{\\parindent}{0pt}\n\\setlength{\\parskip}{1ex plus 0.5ex minus 0.2ex} \n\\pagestyle{empty}\n\\linespread{0.1}\n\n\\newgeometry{tmargin=1.5cm, bmargin=1.5cm, lmargin=1.2cm, rmargin=1.2cm}{}{}\n\\setlength\\columnsep{50pt}\n\\begin{document}\n\\begin{multicols*}{2}[\\begin{Huge}INDEKS UTWORÓW\\end{Huge}\\vspace{1cm}]")

for(i in 1:nrow(ramka2)){
     if(i != 1){
          if(ramka2$autor[i]==ramka2$autor[i-1]){
               
               cat("\t\t\\item[] ", 
                   paste("\\textit{", 
                         as.character(ramka2$piosenka[i]), 
                         "} \\dotfill ",
                         sep=""),
                   as.character(ramka2$strona[i]), 
                   "\\\\\n")
               
               ile <- ile+1
               
               if(ramka2$ile[i]==4 & ile==2){
                    cat(paste("\t\\end{itemize}\n\\end{minipage}\n\\begin{minipage}{\\columnwidth}\n\t\\begin{itemize}[topsep=",
                              odst1,
                              "pt, after=\\vspace{",
                              odst2,
                              "mm}, leftmargin=0mm]\n\t\t\\itemsep0em\n", 
                              sep=""))
               }
               if(ramka2$ile[i]>4 & ile >=2 & ramka2$ile[i]-ile > 2){
                    cat(paste("\t\\end{itemize}\n\\end{minipage}\n\\begin{minipage}{\\columnwidth}\n\t\\begin{itemize}[topsep=",
                              odst1,
                              "pt, after=\\vspace{",
                              odst3,
                              "mm}, leftmargin=0mm]\n\t\t\\itemsep0em\n",
                              sep=""))
               } 
               if(ramka2$ile[i]>4 & ile >=2 & ramka2$ile[i]-ile == 2){
                    cat(paste("\t\\end{itemize}\n\\end{minipage}\n\\begin{minipage}{\\columnwidth}\n\t\\begin{itemize}[topsep=",
                              odst1,
                              "pt, after=\\vspace{",
                              odst2,
                              "mm}, leftmargin=0mm]\n\t\t\\itemsep0em\n",
                              sep=""))
               } 
          } else{
               ile <- 1
                    jakiodstep <- ifelse(ramka2$ile[i]>=4, odst3, odst2)
                    cat(paste("\t\\end{itemize}\n\\end{minipage}\n\\begin{minipage}{\\columnwidth}\n", 
                              paste("\\begin{Large}\n\t\t\\textbf{", ramka2$autor[i],"}\n\t\\end{Large}" , sep=""), 
                              sep=""), 
                        paste("\n\t\\begin{itemize}[topsep=",
                              odst1,
                              "pt, after=\\vspace{", 
                              jakiodstep, 
                              "mm}, leftmargin=0mm]",
                              "\n\t\t\\itemsep0em\n\t\t\\item[]", 
                              "\\textit{", 
                              as.character(ramka2$piosenka[i]), 
                              "} \\dotfill", 
                              sep=""), 
                        as.character(ramka2$strona[i]), 
                        "\\\\\n") 
               }    
            
     } else{
          ile <- 1
          jakiodstep <- ifelse(ramka2$ile[i]>=4, odst3, odst2)
          cat(paste("\\begin{minipage}{\\columnwidth}\n\t", 
                    paste("\\begin{Large}\n\t\t\\textbf{", ramka2$autor[i],"}\n\t\\end{Large}" , sep=""), 
                    sep=""), 
              paste("\n\t\\begin{itemize}[topsep=",
                    odst1,
                    "pt, after=\\vspace{",
                    jakiodstep,
                    "mm}, leftmargin=0mm]\n\t\t\\itemsep0em\n\t\t\\item[]", 
                    "\\textit{", 
                    as.character(ramka2$piosenka[1]), 
                    "} \\dotfill",
                    sep=""),
              as.character(ramka2$strona[1]), 
              "\\\\\n")
     }
     if(i == nrow(ramka2)){
          cat("\t\\end{itemize}\n\\end{minipage}\n")
     }
}

cat("\n\n\n\\end{multicols*}\n\n\\end{document}")

sink()


##################################################################################################

dane <- readLines(stri_paste(katalog_ze_spiewnikiem, "\\autorzy.idx"))
dane_lista <- stri_match_all_regex(dane, "\\\\indexentry[{][{](.*?)[}][!][{](.*?)[}][}][{](.*?)[}]")

autor <- unlist(lapply(dane_lista, function(x) x[2]))
piosenka <- unlist(lapply(dane_lista, function(x) x[3]))
nr_stron <- unlist(lapply(dane_lista, function(x) x[4]))

litery_latex <- c("\\IeC {\\k a}", "\\IeC {\\'c}", "\\IeC {\\k e}", "\\IeC {\\l }", "\\IeC {\\'n}", 
                  "\\IeC {\\'o}", "\\IeC {\\'s}", "\\IeC {\\'z}", "\\IeC {\\.z}",
                  "\\IeC {\\k A}", "\\IeC {\\'C}", "\\IeC {\\k E}", "\\IeC {\\L }", "\\IeC {\\'N}", 
                  "\\IeC {\\'O}", "\\IeC {\\'S}", "\\IeC {\\'Z}", "\\IeC {\\.Z}")
litery_polskie <- c("ą", "ć", "ę", "ł", "ń", "ó", "ś", "ź", "ż",
                    "Ą", "Ć", "Ę", "Ł", "Ń", "Ó", "Ś", "Ź", "Ż")

for(i in 1:length(autor)){
  for(j in 1:length(litery_latex)){
    autor[i] <- stri_replace_all_fixed(autor[i], litery_latex[j], litery_polskie[j])
    piosenka[i] <- stri_replace_all_fixed(piosenka[i], litery_latex[j], litery_polskie[j])
  }
}
autor <- stri_trans_toupper(autor)

stri_detect_fixed(autor, "THE ") %>%
     unlist() -> gdzie_jest_the

if(sum(gdzie_jest_the) > 0) autor[gdzie_jest_the] <- paste(stri_match_first_regex(autor[gdzie_jest_the], "^THE (.*)?")[,2], ", THE", sep="")

ramka <- data.frame(autor=autor, piosenka=piosenka, strona=nr_stron)

ramka %>%
  group_by(autor) %>%
  arrange(piosenka) -> ramka2

literka <- character(nrow(ramka2))
reszta <- character(nrow(ramka2))
for(i in 1:nrow(ramka2)){
  stri_extract_all_regex(as.character(ramka2$autor[i]), "^.") %>% unlist() -> literka[i]
  stri_replace_first_fixed(as.character(ramka2$autor[i]), literka[i], "") %>% unlist() -> reszta[i]  
}

ramka2 %>%  group_by(autor) %>% mutate(ile=n()) -> ramka2
ile <- 0
odst1 <- 6
odst2 <- 5
odst3 <- 1.5

sink(file(stri_paste(katalog_ze_spiewnikiem, "\\autorzy.Rnw"), encoding="UTF-8"))

cat("\n\\documentclass[a4paper, 11pt]{extreport}\n\n\\usepackage[T1]{fontenc}\n\\usepackage[polish]{babel}\n\\usepackage[utf8]{inputenc}\n\\usepackage{amsmath}\n\\usepackage{amsfonts}\n\\usepackage{graphicx}\n\\usepackage{setspace}\n\\usepackage{savesym}\n\\savesymbol{arc}\n\\usepackage{color}\n\\usepackage{xcolor}\n\\usepackage{pict2e}\n\\usepackage{epstopdf}\n\\usepackage{geometry}\n\\usepackage{enumerate}\n\\usepackage{multicol}\n\\usepackage[strict]{changepage}\n\\usepackage{titlesec}\n\\usepackage{etoolbox}\n\\usepackage{tocloft}\n\\usepackage{imakeidx}\n\\usepackage{ifthen}\n\\usepackage{fancyhdr}\n\\usepackage{enumitem}\n\n\\setlist[enumerate]{itemsep=0mm}\n\\setlength{\\parindent}{0pt}\n\\setlength{\\parskip}{1ex plus 0.5ex minus 0.2ex} \n\\pagestyle{empty}\n\\linespread{0.1}\n\n\\newgeometry{tmargin=1.5cm, bmargin=1.5cm, lmargin=1.2cm, rmargin=1.2cm}{}{}\n\\setlength\\columnsep{50pt}\n\\begin{document}\n\\begin{multicols*}{2}[\\begin{Huge}INDEKS WYKONAWC\\IeC {\\'O}W\\end{Huge}\\vspace{1cm}]")

for(i in 1:nrow(ramka2)){
  if(i != 1){
    if(ramka2$autor[i]==ramka2$autor[i-1]){
         
      cat("\t\t\\item[] ", 
          paste("\\textit{", 
                as.character(ramka2$piosenka[i]), 
                "} \\dotfill ",
                sep=""),
          as.character(ramka2$strona[i]), 
          "\\\\\n")

     ile <- ile+1
     
     if(ramka2$ile[i]==4 & ile==2){
          cat(paste("\t\\end{itemize}\n\\end{minipage}\n\\begin{minipage}{\\columnwidth}\n\t\\begin{itemize}[topsep=",
                    odst1, 
                    "pt, after=\\vspace{",
                    odst2,
                    "mm}]\n\t\t\\itemsep0em\n",
                    sep=""))
     }
     if(ramka2$ile[i]>4 & ile >=2 & ramka2$ile[i]-ile > 2){
          cat(paste("\t\\end{itemize}\n\\end{minipage}\n\\begin{minipage}{\\columnwidth}\n\t\\begin{itemize}[topsep=",
                    odst1, 
                    "pt, after=\\vspace{",
                    odst3,
                    "mm}]\n\t\t\\itemsep0em\n",
                    sep=""))
     } 
     if(ramka2$ile[i]>4 & ile >=2 & ramka2$ile[i]-ile == 2){
          cat(paste("\t\\end{itemize}\n\\end{minipage}\n\\begin{minipage}{\\columnwidth}\n\t\\begin{itemize}[topsep=",
                    odst1, 
                    "pt, after=\\vspace{",
                    odst2,
                    "mm}]\n\t\t\\itemsep0em\n",
                    sep=""))
     } 
    } else{
        ile <- 1
        if(literka[i]==literka[i-1]){
          jakiodstep <- ifelse(ramka2$ile[i]>=4, odst3, odst2)
          cat(paste("\t\\end{itemize}\n\\end{minipage}\n\\begin{minipage}{\\columnwidth}\n\t", 
                    literka[i], 
                    reszta[i], 
                    sep=""), 
              paste("\n\t\\begin{itemize}[topsep=",
                    odst1,
                    "pt, after=\\vspace{",
                    jakiodstep, 
                    "mm}]\n\t\t\\itemsep0em\n\t\t\\item[]", 
                    "\\textit{", 
                    as.character(ramka2$piosenka[i]), 
                    "} \\dotfill",
                    sep=""), 
              as.character(ramka2$strona[i]), 
              "\\\\\n")  
        } else{
          ile <- 1
          jakiodstep <- ifelse(ramka2$ile[i]>=4, odst3, odst2)
          cat(paste("\t\\end{itemize}\n\\end{minipage}\n\\begin{minipage}{\\columnwidth}\n", 
                    "\t\\begin{Large}\\textbf{", 
                    literka[i], 
                    "}\\end{Large}", 
                    reszta[i], 
                    sep=""), 
              paste("\n\t\\begin{itemize}[topsep=",
                    odst1,
                    "pt, after=\\vspace{", 
                    jakiodstep, 
                    "mm}]",
                    "\n\t\t\\itemsep0em\n\t\t\\item[]", 
                    "\\textit{", 
                    as.character(ramka2$piosenka[i]), 
                    "} \\dotfill", 
                    sep=""), 
              as.character(ramka2$strona[i]), 
              "\\\\\n") 
        }    
    }  
  } else{
      ile <- 1
      jakiodstep <- ifelse(ramka2$ile[i]>=4, odst3, odst2)
      cat(paste("\\begin{minipage}{\\columnwidth}\n\t\\begin{Large}\\textbf{", 
                literka[1], 
                "}\\end{Large}", 
                reszta[1], 
                sep=""), 
          paste("\n\t\\begin{itemize}[topsep=",
                odst1,
                "pt, after=\\vspace{",
                jakiodstep,
                "mm}]\n\t\t\\itemsep0em\n\t\t\\item[]", 
                "\\textit{", 
                as.character(ramka2$piosenka[1]), 
                "} \\dotfill",
                sep=""),
          as.character(ramka2$strona[1]), 
          "\\\\\n")
    }
  if(i == nrow(ramka2)){
    cat("\t\\end{itemize}\n\\end{minipage}\n")
  }
} 

cat("\n\n\n\\end{multicols*}\n\n\\end{document}")

sink()

