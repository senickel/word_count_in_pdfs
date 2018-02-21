# mypdf = string, path to pdf document
# numbers_are_words = are numbers constituting words? Default is Yes.
# words split over lines are merged
# a word is an alpha(-numeric) character separated by white spaces with a minimum length of 2 except for the letter 'a'

count_words<-function(mypdf,numbers_are_words=TRUE) {
  if(!"dplyr" %in% (.packages())) stop("Please attach package dplyr.")
  if(!"pdftools" %in% (.packages())) stop("Please attach package pdftools.")
  
  on.exit(options(options("stringsAsFactors")))
  options(stringsAsFactors = FALSE)
  pdf2<-pdftools::pdf_text(mypdf)
  
  lapply(1:length(pdf2),function(x1) {
    x<-pdf2[x1] %>% strsplit(.," ") %>% unlist
   
    # check for words split at end of line
    connect<-grep("-\r",x)
    # if exist, merge them
    if(connect %>% length>0) {
      x[connect]<-x[connect] %>% gsub("-\r\n","",.)
    }
    x<-x %>% gsub("\r\n"," ",.) 
    
    x<-x %>% gsub("[^[:alnum:]]", " ",.) %>% data.frame %>% filter(.!=""&.!=" ") %>% 
      apply(.,1,strsplit,split=" ") %>% unlist
    
    if (!numbers_are_words) x<-x %>% gsub("\\d","",.)
    if (x %>% length>0) {
      x<-x %>% strsplit(.," ") %>% unlist %>%
        data.frame(word=.) %>% filter(.!="") %>% mutate(word_length=sapply(word,nchar)) %>% 
        filter(word_length>1&!word%in%c("A","a","I")) %>% dplyr::select(word)
    }
    
    return(x)
  }) %>% do.call(rbind,.) %>% nrow
}
