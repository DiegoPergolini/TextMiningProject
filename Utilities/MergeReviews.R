filenames <- list.files(path="./",pattern="*.csv")
print(filenames)
## [1] "abc.csv" "pqr.csv"

## Full path to csv filenames
fullpath=file.path("C:/Users/diego/OneDrive/Documenti/R/Mixed",filenames)

## Print Full Path to the files
print(fullpath)

## Merge listed files from the path above
dataset <- do.call("rbind",lapply(fullpath,FUN=function(files){ read.csv(files)}))
write.table(dataset,file="allReviewsCJFN.csv",sep=",",row.names = F)
