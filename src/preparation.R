## Relation between income and nationalism in Catalonia
## @griverorz
## Sat Oct 19 12:44:08 PDT 2013

library(foreign)
library(car)
library(reshape2)
theme_set(theme_bw())

setwd("~/Documents/datablog/natinc")

read_cis <- function(syntaxf, dataf) {
    require(foreign)
    
    ## Add EOL to the last line
    system(paste("sed -i -e '$a\\'", syntaxf, sep = " "))
    
    ## Convert to utf8
    new_syntaxf <- paste("utf8_", basename(syntaxf), sep = "")
    out_syntaxf <- file.path(dirname(syntaxf), new_syntaxf)
    system(paste("iconv -f ISO-8859-1 -t UTF-8", 
                 syntaxf, ">", out_syntaxf, sep = " "))
    
    ## Create output file
    out_dataf <- paste(basename(dataf), ".sav", sep = "")
    exportline <- paste("SAVE OUTFILE='", basename(dataf), ".sav'.", sep = "")
    system(paste("echo", exportline, ">>", out_syntaxf, sep = " "))

    ## Replace commas with dots for decimals in datafile
    system(paste("sed -i -e 's/,/./g'", dataf, sep = " "))
    
    ## Run, damnit, run!
    cwd <- getwd()
    setwd(dirname(dataf))
    system(paste("pspp", basename(out_syntaxf), sep = " "), wait = TRUE)
    setwd(cwd)
    
    ## Read SAV file 
    outdf <- read.spss(file.path(dirname(dataf), out_dataf), 
                       to.data.frame = TRUE, use.value.labels = FALSE)
    return(outdf)
}

dfcat <- read_cis(syntaxf = "./dta/MD2956/ES2956", dataf = "./dta/MD2956/DA2956")

#################### Replication ####################

## Catalonia
dfcat <- subset(dfcat, CCAA == 9)

## Income
dfcat$income <- ifelse(dfcat$P54 == 99, NA, dfcat$P54)
dfcat$pkincome <- recode(dfcat$income, "1:4 = 1; 5:6 = 2; 7 = 3; 8:10 = 4")
## Gender 
dfcat$gender <- dfcat$P44
## Age
dfcat$age <- ifelse(dfcat$P45 == 99, NA, dfcat$P45)
## Education
dfcat$educ <- ifelse(dfcat$P46a == 99, NA, dfcat$P46a)
## Organization
dfcat$org <- ifelse(dfcat$P12 %in% c(8, 9), NA, dfcat$P12) 
## Size town
dfcat$sizetown <- dfcat$TAMUNI
## National identification
dfcat$nat <- ifelse(dfcat$P37 %in% c(8,9), NA, dfcat$P37)

#################### save data ####################
save(dfcat, file = "./dta/dfcat.RData")
