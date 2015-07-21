library("stringi")
library("dplyr")

katalog_ze_spiewnikiem <- "..\\syf"

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


sink(file(stri_paste(katalog_ze_spiewnikiem, "\\piosenki.Rnw"), encoding="UTF-8"))

cat("\n\\documentclass[a4paper]{report}\n\n\\usepackage[T1]{fontenc}\n\\usepackage[polish]{babel}\n\\usepackage[utf8]{inputenc}\n\\usepackage{amsmath}\n\\usepackage{amsfonts}\n\\usepackage{graphicx}\n\\usepackage{setspace}\n\\usepackage{savesym}\n\\savesymbol{arc}\n\\usepackage{color}\n\\usepackage{xcolor}\n\\usepackage{pict2e}\n\\usepackage{epstopdf}\n\\usepackage{geometry}\n\\usepackage{enumerate}\n\\usepackage{multicol}\n\\usepackage[strict]{changepage}\n\\usepackage{titlesec}\n\\usepackage{etoolbox}\n\\usepackage{tocloft}\n\\usepackage{imakeidx}\n\\usepackage{ifthen}\n\\usepackage{fancyhdr}\n\\usepackage{enumitem}\n\n\\setlist[enumerate]{itemsep=0mm}\n\\setlength{\\parindent}{0pt}\n\\setlength{\\parskip}{1ex plus 0.5ex minus 0.2ex} \n\\pagestyle{empty}\n\n\\newgeometry{tmargin=2.5cm, bmargin=1cm, lmargin=1.2cm, rmargin=1.2cm}{}{}\n\\setlength\\columnsep{50pt}\n\\begin{document}\n\\begin{multicols*}{2}[\\begin{Huge}INDEKS UTWORÓW\\end{Huge}\\vspace{1cm}]")

for(i in 1:length(pierwsze_litery)){
  if(i != 1){
    if(!(pierwsze_litery[i] == pierwsze_litery[i-1])){
      cat("\n\\indexspace\n")
      cat("{\\Large\\bfseries ", pierwsze_litery[i], " }\\nopagebreak", "\n\n")
      cat(tytuly_posortowane[i], "\\dotfill ", nr_stron_posortowane[i], "\\\\\n")
    } else{
      cat(tytuly_posortowane[i], "\\dotfill ", nr_stron_posortowane[i], "\\\\\n")
    } 
  } else{
    cat("{\\Large\\bfseries ", pierwsze_litery[1], " }\\nopagebreak", "\n\n")
    cat(tytuly_posortowane[1], "\\dotfill ", nr_stron_posortowane[1], "\\\\\n")
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

sink(file(stri_paste(katalog_ze_spiewnikiem, "\\autorzy.Rnw"), encoding="UTF-8"))

cat("\n\\documentclass[a4paper]{report}\n\n\\usepackage[T1]{fontenc}\n\\usepackage[polish]{babel}\n\\usepackage[utf8]{inputenc}\n\\usepackage{amsmath}\n\\usepackage{amsfonts}\n\\usepackage{graphicx}\n\\usepackage{setspace}\n\\usepackage{savesym}\n\\savesymbol{arc}\n\\usepackage{color}\n\\usepackage{xcolor}\n\\usepackage{pict2e}\n\\usepackage{epstopdf}\n\\usepackage{geometry}\n\\usepackage{enumerate}\n\\usepackage{multicol}\n\\usepackage[strict]{changepage}\n\\usepackage{titlesec}\n\\usepackage{etoolbox}\n\\usepackage{tocloft}\n\\usepackage{imakeidx}\n\\usepackage{ifthen}\n\\usepackage{fancyhdr}\n\\usepackage{enumitem}\n\n\\setlist[enumerate]{itemsep=0mm}\n\\setlength{\\parindent}{0pt}\n\\setlength{\\parskip}{1ex plus 0.5ex minus 0.2ex} \n\\pagestyle{empty}\n\\linespread{0.1}\n\n\\newgeometry{tmargin=2.5cm, bmargin=1cm, lmargin=1.2cm, rmargin=1.2cm}{}{}\n\\setlength\\columnsep{50pt}\n\\begin{document}\n\\begin{multicols*}{2}[\\begin{Huge}INDEKS WYKONAWC\\IeC {\\'O}W\\end{Huge}\\vspace{1cm}]")

for(i in 1:nrow(ramka2)){
  if(i != 1){
    if(ramka2$autor[i]==ramka2$autor[i-1]){
      cat("\\item[] ", 
          as.character(ramka2$piosenka[i]), 
          " \\dotfill ", 
          as.character(ramka2$strona[i]), 
          "\\\\\n")
    } else{
        if(literka[i]==literka[i-1]){
          cat("\\end{itemize}\n", paste(literka[i], reszta[i], sep=""), 
              "\n\\begin{itemize}[topsep=0pt]\n\\itemsep0em\n\\item[] ", 
              as.character(ramka2$piosenka[i]), 
              " \\dotfill ", 
              as.character(ramka2$strona[i]), 
              "\\\\\n")  
        } else{
          cat("\\end{itemize}\n", paste("\\begin{Large}\\textbf{", literka[i], "}\\end{Large}", reszta[i], sep=""), 
              "\n\\begin{itemize}[topsep=0pt]\n\\itemsep0em\n\\item[] ", 
              as.character(ramka2$piosenka[i]), 
              " \\dotfill ", 
              as.character(ramka2$strona[i]), 
              "\\\\\n") 
        }    
    }  
  } else{
      cat(paste("\\begin{Large}\\textbf{", literka[1], "}\\end{Large}", reszta[1], sep=""), 
          "\n\\begin{itemize}[topsep=0pt]\n\\itemsep0em\n\\item[] ", 
          as.character(ramka2$piosenka[1]), 
          " \\dotfill ", 
          as.character(ramka2$strona[1]), 
          "\\\\\n")
    }
  if(i == nrow(ramka2)){
    cat("\\end{itemize}\n")
  }
} 

cat("\n\n\n\\end{multicols*}\n\n\\end{document}")

sink()
