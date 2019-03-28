setwd("C:\\Users\\ad4336\\Documents\\Yelp_data_processing")
#install.packages("reshape2")
library(jsonlite)
library(tibble)
library(dplyr)
library(stringr)
library(tidyr)
library(data.table)
library(reshape2)

#slow
#yelp_reviews <- fread("\\Users\\jc7373\\Desktop\\YelpDataset\\review.csv", fill = TRUE)
#faster
reviews <- stream_in(file("\\Users\\ad4336\\Documents\\Yelp_data_processing\\data\\review.json"), pagesize = 10000)
head(reviews, 10) 
str(reviews)
reviews_flat <- flatten(reviews) #making it prettier
str(reviews_flat)
reviews_tbl <- as_data_frame(reviews_flat)
reviews_tbl
reviews_subset <- reviews_tbl %>% select(-starts_with("text"), -starts_with("useful"), -starts_with("funny"), -starts_with("cool")) 
reviews_2017 <- reviews_subset %>% filter(str_detect(date, "2017"))
head(reviews_2017)

#yelp_test <-yelp_tbl %>% filter(str_detect(business_id, "uYHaNptLzDLoV_JZ_MuzUA"))
#result_test <-total_toronto %>% filter(str_detect(user_id, "yK6wyLZ2I66B5-MJcS88xg"))
#reshape(total, idvar="user_id", timevar="business_id", direction="wide")
#result <- total %>% reshape(total, idvar="business_id", timevar="user_id", direction="wide")


#toronto_subset1 <- toronto_subset %>% filter(str_detect(user_id, "-3PTUP443q6hQESLKSu95w"))
#toronto_subset <- newdata <- total_toronto[1:100,]
#toronto_test <- toronto_subset %>% reshape(total, idvar="user_id", timevar="business_id", direction="wide")
#toronto_subset %>% spread(user_id, business_id)
#correct
#toronto_test <- acast(toronto_subset,user_id~business_id)

###############################################business file######################################################

#yelp <- fread("\\Users\\jc7373\\Desktop\\YelpDataset\\business.csv")
#faster
business <- stream_in(file("\\Users\\ad4336\\Documents\\Yelp_data_processing\\data\\business.json")) #importing json 
head(business, 10) 
str(business)
business_flat <- flatten(business) #making it prettier
str(business_flat)
business_tbl <- as_data_frame(business_flat)
business_tbl
#show the values inside of variable
#yelp_tbl %>% mutate(categories = as.character(categories)) %>% select(categories)
#Remove unnecessary variables 

business_subset <- business_tbl %>% select(-starts_with("hours"), -starts_with("attribute"), -starts_with("is_open"), -starts_with("neighborhood")) %>% 
  filter(str_detect(categories, "Restaurant"))

business_cities <- business_subset %>% count(city) %>% arrange(desc(n))

business_toronto <- business_subset %>% filter(str_detect(city, "Toronto"))
business_lasVegas <- business_subset %>% filter(str_detect(city, "Las Vegas"))
business_phoenix <- business_subset %>% filter(str_detect(city, "Phoenix"))
business_charlotte <- business_subset %>% filter(str_detect(city, "Charlotte"))
business_pittsburgh <- business_subset %>% filter(str_detect(city, "Pittsburgh"))
business_madison <- business_subset %>% filter(str_detect(city, "Madison"))
business_urbana <- business_subset %>% filter(str_detect(city, "Urbana"))



yelp_selected_lasVegas_filtered <- business_lasVegas %>% filter(str_detect()) 

#result_test <-yelp_selected_lasVegas %>% filter(str_detect(business_id, "-iFvYhgysvjkxckCr42NRw"))





total_toronto <- merge(yelp_2017,yelp_selected_toronto, by="business_id")
total_toronto <- total_toronto %>% select(starts_with("business_id"), starts_with("user_id"), starts_with("stars.x")) 
toronto_matrix <- acast(total_toronto,user_id~business_id)
sum(is.na(toronto_matrix))
write.csv(toronto_matrix, file = "TorontoRatings.csv")


total_lasVegas <- merge(yelp_2017,yelp_selected_lasVegas, by="business_id")
total_lasVegas <- total_lasVegas %>% select(starts_with("business_id"), starts_with("user_id"), starts_with("stars.x")) 


total_lasVegas_test <- merge(yelp_2017,yelp_selected_lasVegas, by="business_id")
total_lasVegas_test_selected <- total_lasVegas_test %>% select(starts_with("business_id"), starts_with("user_id"), starts_with("stars.x"), starts_with("postal_code"), starts_with("review_count"), starts_with("latitude"), starts_with("longitude"))
total_lasVegas_test_selected10 <- total_lasVegas_test_selected %>% count(postal_code) %>% filter(n >3490)

two_loc <- total_lasVegas_test_selected %>% filter(str_detect(postal_code, "89119"))



lasVegas_matrix <- acast(total_lasVegas,user_id~business_id)

lasVegas_matrix <- acast(total_lasVegas,user_id~business_id)
sum(is.na(lasVegas_matrix))
dim(lasVegas_matrix)
write.csv(lasVegas_matrix, file = "LasVegasRatings.csv")


toronto_info <- merge(yelp_selected_toronto, total_toronto, by="business_id")
toronto_info_matrix <- as.matrix(toronto_info)
write.csv(toronto_info_matrix, file = "Toronto.csv")

lasVegas_info <- merge(yelp_selected_lasVegas, total_lasVegas, by="business_id")
lasVegas_info_matrix <- as.matrix(lasVegas_info)
write.csv(lasVegas_info_matrix, file = "LasVegas.csv")

#__________________

lasVegas_test <- merge(reviews_subset, business_lasVegas, by="business_id")
lasVegas_subset <- lasVegas_test %>% select(starts_with("name"), starts_with("date"), starts_with("stars.x")) 
lasVegas_subset <- droplevels(lasVegas_subset)

#lasVegas_2017 <- lasVegas_subset %>% filter(str_detect(date, "2017"))
#lasVegas_2017_1 <- lasVegas_2017 %>% filter(str_detect(stars.x, "5"))

reviews_monthly_stars <- lasVegas_subset %>% group_by(date=substr(date,1,4), stars.x) %>% summarize(count=n()) %>% arrange((date))

reviews_monthly_stars <- droplevels(reviews_monthly_stars)
reviews_monthly_stars_matrix <- as.matrix(reviews_monthly_stars)
write.csv(reviews_monthly_stars_matrix, file = "input.csv")

####
library(ggplot2)
library(extrafont)
library(scales)
library(grid)
library(RColorBrewer)

####

rank_colors = c(brewer.pal(9, "Reds")[c(8,7,6)],brewer.pal(9, "Greens")[c(7,8)])

ggplot(aes(x=as.POSIXct(paste(date,"-01",sep="")), y=count, fill=as.factor(stars.x)), data=reviews_monthly_stars) +
  geom_area(position = "stack") +
  scale_x_datetime(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  scale_y_continuous(label = comma) +
  theme(legend.title = element_blank(), legend.position="top", legend.direction="horizontal", legend.key.width=unit(0.25, "cm"), legend.key.height=unit(0.25, "cm"), legend.margin=unit(-0.5,"cm"), panel.margin=element_blank()) +
  labs(title=paste("# of Yelp Reviews by Month, by # Stars for", format(nrow(lasVegas_subset),big.mark=","),"Reviews"), x="Date of Review Submission", y="Total # of Review Submissions (by Month)") +
  scale_fill_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars")) +
  scale_color_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars"))

ggplot(reviews_monthly_stars, aes(x=as.POSIXct(paste(date,"-01",sep="")), y=count, fill=as.factor(stars.x))) +
  geom_area(colour="black", size=.2, alpha=.4) +
  scale_x_datetime(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
#  theme_custom() + 
  theme(legend.title = element_blank(), legend.position="top", legend.direction="horizontal", legend.key.width=unit(0.25, "cm"), legend.key.height=unit(0.25, "cm"), legend.margin=unit(-0.5,"cm"), panel.margin=element_blank()) +
  labs(title=paste("# of Yelp Reviews by Month, by # Stars for", format(nrow(lasVegas_subset),big.mark=","),"Reviews"), x="Date of Review Submission", y="Total # of Review Submissions (by Month)") +
  scale_y_continuous(label = comma) +
  scale_fill_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars")) +
  scale_color_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars"))

data = reviews_monthly_stars
data=data[order(data$stars.x, decreasing=T) , ]

ggplot(reviews_monthly_stars, aes(x=as.numeric(date), y=count, fill=as.factor(stars.x))) + 
  geom_area()

ggsave("yelp-review-time-series.png", dpi=300, height=3, width=4)



#export
lasVegas_matrix <- as.matrix(lasVegas_subset)
write.csv(lasVegas_matrix, file = "LasVegas.csv")


