install.packages('rvest')
library(rvest)
query_url<- "https://www.tripadvisor.it/Restaurants-g187785-Naples_Province_of_Naples_Campania.html"
#parse html search result (here: restaurants in Cologne)
#query_url<-read_html ("https://www.tripadvisor.it/Restaurants-g194731-Cesena_Province_of_Forli_Cesena_Emilia_Romagna.html")
#query_url<-read_html ("https://www.tripadvisor.it/Restaurants-g677543-Jesi_Province_of_Ancona_Marche.html")
#query_url<-read_html ("https://www.tripadvisor.it/Restaurants-g187895-Florence_Tuscany.html")
page0_url<-read_html (query_url)

npages<-page0_url%>% 
        html_nodes(" .pageNum ") %>% 
        html_attr(name="data-page-number") %>%
        tail(.,1) %>%
        as.numeric()

Restaurant_Name<-vector(mode="character", length=30*npages)
Restaurant_URL<-vector(mode="character", length=30*npages)

offset=0
idx_s=0 
i<-1
for (i in 1:npages)
{
        
        query_url.splitted<-unlist(strsplit(query_url,split="-"))
        page_url.firstpart<-paste(query_url.splitted[1],"-",query_url.splitted[2],"-oa-",sep = "")
        page_url.secondpart<-paste("-",query_url.splitted[3],"#EATERY_OVERVIEW_BOX",sep="")
        page_url<-paste(page_url.firstpart,offset,page_url.secondpart,sep="")
  

        link<-read_html(page_url)
        

        R_names<-link %>%
                html_nodes("a.property_title") %>%
                html_text() %>%
                gsub('[\r\n\t]', '', .)
        

        R_url<-link %>% 
                html_nodes(".shortSellDetails a.property_title") %>% 
                html_attr(name="href")

        R_url<-paste("https://www.tripadvisor.it",R_url,sep="")
        

        R_count<-length( R_names)
        
        Restaurant_Name[(idx_s+1):(idx_s+R_count)]<-R_names
        Restaurant_URL[(idx_s+1):(idx_s+R_count)]<-R_url
        
        idx_s=idx_s+length(R_names)
        
        offset<-offset+30      
}

Restaurant_Name<-Restaurant_Name [Restaurant_Name!=""]
Restaurant_URL<-Restaurant_URL[Restaurant_URL!=""]

len=length(Restaurant_Name)


i<-1
j<-1
k<-1
Restaurant_Name[120]
Restaurant_URL[1]

for(i in 1:len)
{
        url<- Restaurant_URL[i]
        splitted<-unlist(strsplit(url,split="Reviews"))
        Restaurant_Reviews <- c("")
        Restaurant_Quotes <- c("")
        Restaurant_Ratings <- c("")
        url
        TextNumReviews<-url %>% read_html() %>% html_node(".more") %>% html_text()
        while(is.na(TextNumReviews)){
          TextNumReviews<-url %>% read_html() %>% html_node(".more") %>% html_text()
        }
        num <- unlist(strsplit(TextNumReviews,split=" recensioni"))
        num<-gsub(".","",num,fixed=TRUE)
        Treviews<-as.numeric(num[1])
        if(is.na(Treviews)){
          Treviews<-0
        }
        npages<-url%>%read_html()%>% 
          html_nodes("a.pageNum.last.taLnk") %>% 
          html_attr(name="data-page-number") %>%
          head(.,1) %>%
          as.numeric()
        if(Treviews <=10){
          npages<-1
        }
        offset=0 
        idx_s=0
        j<-1
        for(j in 1:npages){
          url<-paste(splitted[1],"Reviews-or",offset,splitted[2],sep="")
          url
          reviews <- url %>% read_html() %>% html_nodes("#REVIEWS .innerBubble")
          while(length(reviews)==0){
            reviews <- url %>% read_html() %>% html_nodes("#REVIEWS .innerBubble")
          }
          quote <- reviews %>% html_node(".quote span") %>% html_text()
          fullReviewLink <-  reviews %>% html_node(".quote a") %>% html_attr(name="href")
          quote
          fullReviewLink
          rating<- reviews %>% html_node(".rating span") %>% html_attr("class")
          rating
          date<-reviews%>% html_node(".ratingDate") %>% html_text()
          review <- reviews %>% html_node(".entry .partial_entry") %>% html_text()
          review
          numRev=length(fullReviewLink)
          reviewsText<-c("")
          k<-1
          for(k in 1:numRev){
            revLink <- paste("https://www.tripadvisor.it",fullReviewLink[k],sep="")
            revText <- revLink%>% read_html() %>% html_node(".entry .partial_entry") %>% html_text()
            if(length(revText)==0){
              revText<-" "
            }
            reviewsText[k]<- revText
          }

          if(length(quote)==0){
            quote<-" "
          }
          if(length(rating)==0){
            rating<-" "
          }
          quote<-sapply(quote, function(x) gsub("\"","",x,fixed=TRUE))
          quote<-sapply(quote, function(x) gsub("\\n"," ",x,fixed=TRUE))
          reviewsText<-sapply(reviewsText, function(x) gsub("\"","",x,fixed=TRUE))
          reviewsText<-sapply(reviewsText, function(x) gsub("\\n"," ",x,fixed=TRUE))
          reviewsText
          Restaurant_Reviews <- c(Restaurant_Reviews,reviewsText)
          Restaurant_Quotes <- c(Restaurant_Quotes,quote)
          Restaurant_Ratings <- c(Restaurant_Ratings,rating)
                                  Restaurant_Reviews
                                  Restaurant_Quotes
                                  Restaurant_Ratings
          idx_s=idx_s+length(R_names)
          offset<-offset+10
        }
        Restaurant_Dataset = data.frame(Restaurant_Ratings,Restaurant_Quotes,Restaurant_Reviews)
        fileName<- paste(Restaurant_Name[i],".csv",sep = "")
        fileName<-sapply(fileName, function(x) gsub("\"","",x,fixed=TRUE))
        fileName <-sapply(fileName, function(x) gsub(" ","",x,fixed=TRUE))
        write.table(Restaurant_Dataset,file=fileName,sep=",",row.names = F)
}
