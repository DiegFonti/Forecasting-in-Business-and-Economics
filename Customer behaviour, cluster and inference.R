# ---
#   output:
#   word_document: default
# pdf_document: default
# html_document: default
# ---

## INTRODUCTION
# Beautiful is a company selling cosmetics, mainly in the UK. The company has been providing fragrances, skincare, and makeup since the nineties and has now decided to enter green marketing and widen its offer through a line of natural products.
# To understand the attitudes of prospective customers, they develop a questionnaire to investigate the most important factors influencing customers’ choices and preferences, as well as several variables related to lifestyle, purchase and use habits, and a small number of sociodemographic variables.
# The questionnaire was administered through the CAWI (Computer Assisted Web Interviewing) technique, with recruitment through social networks (snowball sampling). The questionnaire was administered to 209 respondents, obtaining 138 valid questionnaires. To facilitate respondents, the questionnaire was articulated in several sections:
#   - Natural Cosmetics
# - Facial Care Products
# - Face Care
# - Lifestyle
# - Personal Information

# In order to deeply understand the patterns of behaviors among their customers, the company commissioned us to do this research.

## General overview

# **DEF** Green marketing is the promotion of products, services, or activities described as environmentally safe or more environmentally sustainable. The strategies involved should focus on the development of a brand identity characterized by the commitment to make ecological choices, which also help to educate consumers on consumption and recycling habits that are more sustainable and less harmful to the environment. All these activities must be communicated to the target clearly and truthfully. 
# 
# The aim is to set the production, marketing, and use stages of a product in such a way as to convince consumers to opt for more environmentally conscious and sustainable consumption. This means setting up long-term goals in order to change lifestyles and retrain consumption, making it perceived as a normal and acceptable ecological alternative, consequently, the offer of green products and services as attractive and preferable.
# 
# Considering the increasing sensitivity and awareness of consumers on these issues, "Green" declarations must therefore be verifiable and accompanied by environmental certifications to avoid greenwashing problems.
# 
# **DEF** Greenwashing is the bad practice of pretending to be sustainable without doing anything concrete.
# 
# Finally, these green marketing policies allow the companies that implement them to obtain important benefits; in fact, firms that adopt green marketing strategies as a proactive path access new markets, increase market share and profits, and also enjoy a competitive advantage over other environmentally responsible alternatives.
# 
# ### SIZE OF THE MARKET:
# The global green technology and sustainability market size was valued at $10.32 billion in 2020, and is projected to reach $74.64 billion by 2030.
# 
# **REFERENCES:**
#   https://www.insidemarketing.it/glossario/definizione/green-marketing/
#   https://www.glossariomarketing.it/significato/green-marketing/
#   https://www.digital4.biz/marketing/green-marketing-che-cose-come-si-fa-e-quali-sono-i-vantaggi-per-i-brand/
#   https://www.doxee.com/it/blog/marketing/cos-e-green-marketing/
#   
#   
#   After having given a general explanation about green marketing and how many people are interested all around the world, we’ve built a research on secondary sources about the previous studies on customer profiling for this kind of product.
# 
# 1. Source: https://www.cnbc.com/2021/08/10/the-environment-is-gen-zs-no-1-concern-but-beware-of-greenwashing.html
# - *“This desire for sustainable products among Gen Z is robust. According to a 2020 report by First Insight, 73% of Gen Z consumers surveyed were willing to pay more for sustainable products, more than every other generation. And, despite being the youngest cohort with many still in school, they were willing to spend the most in added costs, with 54% saying they would pay more than a 10% increase in price for a sustainable made product.”*
#   - *“This year, more than a quarter of millennials and Gen Zs worldwide said that their buying decisions have been influenced by the impact of certain businesses on the environment.”*
#   
#   
#   OUR OBSERVATION:
#   We think this is a good starting point for our research because it makes us understand that nowadays people are willing to spend even more to buy sustainable products. This tendency is widespread among Generation Z. They have grown up with the idea of sustainability. So they could be the perfect target for a line of organic cosmetics.
# 
# 2. Source: https://www.alixpartners.com/insights-impact/insights/millennials-preferences-beauty-personal-care-products/
#   
#   - *“ These buyers want beauty and personal care products with natural or organic ingredients that are sourced and manufactured following ethical and environmental standards. Consumers, across generations and countries, are increasingly focused on these issues, but demand from millennials is keenest.”*
#   
#   - *“This attention to healthy and clean products is driven by many factors. There is a growing awareness, as well as some confusion, among the general public regarding many ingredients in beauty and personal care products…Consumers have been warned by the media of top ingredients to avoid”*
#   
#   - *” They are the largest potential buying group”*
#   
#   - *“However, millennials are significantly more likely to pay more for beauty and personal care products containing desirable attributes than older generations. While there is some differentiation by country, overall, millennials in our survey indicated they will pay an average of 18% more for these products. This is significantly higher than what older consumers said they are willing to pay – an average of 13% for Gen X and 9% for Boomers.”*
#   
#   
#   3. Source: https://www.statista.com/statistics/811406/share-of-millennials-purchasing-natural-skin-care-products/
#   
#   - *“According to a 2017 survey, 31 percent of U.S. consumers were looking for natural ingredients when purchasing skincare products. Among the millennial survey respondents, this figure was distinctly higher, with 43 percent stating their preference for natural skincare alternatives.”*
#   
#   - *“The demographic shift has major implications for what product retailers and cosmetics companies in the United States should offer their customers. The way that shoppers approach skincare is undergoing a fundamental change, as they are becoming more informed and educated about the products on offer. With the increasing awareness of ingredients and their potentially harmful effects, consumer knowledge now sets the rhythm that beauty companies have to follow. Skincare companies will have to limit or remove parabens and steroids and generally become more transparent about their formulas and ingredients in the future.”*
#   
#   So the key point is that age can be a great differentiator in face care purchases.
# 
# 4. Source: https://www.statista.com/topics/3995/men-s-grooming-and-cosmetics-market-in-europe/
#   
#   - *“The men's grooming and cosmetics market is a large industry that consists of many sub-categories. Men are not only interested in their beards and hairstyles, many are now more accepting of fragrances, and skin and body care products. As evidenced by figures from the United Kingdom, the market value for men's grooming has increased by nearly 100 million British pounds from 2015 to 2017. There are many growing markets within this industry.”*
#   
#   OUR OBSERVATION:
#   Until now we have only discussed about segmenting the market by age, but it’s important to consider that also a segmentation by gender would be interesting, as the men's face products market is booming.

# DATA ANALYSIS

Implementing libraries:
```{r}
require(tidyr)
require(tigerstats)
require(plotly)
require(psych)
require(pseudo)
library(plyr)
library(plotrix)
library(ltm)
```

Sourcing the functions needed from the working directory:
source("/Users/utente1/Desktop/Dataset/pseudo_F.R")
source("/Users/utente1/Desktop/Dataset/nclusters.R")



## Section 1: General Overview of the dataset 

# In this section, we will mainly focus on getting a general overview on consumers and non-consumers of natural products. Thus the nature of this chapter is explorative and its purpose is to lead the way for the rest of the research.

# Let's distinguish consumers from non-consumers.
data<-read.delim("/Users/utente1/Desktop/Dataset/beautiful.txt", header=T) 
consumers <- data[data$nat.bought==1,] #variable for consumers
non_consumers <- data[data$nat.bought == 0,] #variable fon non consumers
col.names <- colnames(consumers)
data_is_reverse <- FALSE ##this will be needed later on for reversing the scores of the likert scale.
data_is_reverse_trendy = FALSE ##this will be needed later on for reversing the scores of the likert scale.

print("Number of consumer")
dim(consumers)[1]
print("Number of non-conumers")
dim(non_consumers)[1]


# We divided consumers from non-consumers according to their response to the second question (V3): "Have you bought at least one natural product for personal care during the last three months?".
# If their answer is yes then they are classified as consumers, otherwise, they are classified as non-consumers.
# From the data, we can see that over 161 respondents to the test, only 25 are non-consumers of natural products. 

# We now try to get also a general overview of our consumers and non-consumers concerning other socio-demographic variables, to understand to which group of people we need to address specific attention when designing our website, selecting the products, the advertisement, the marketing campaign, and so on.


Gender_Consumers <- table(data$nat.bought, data$Gender)
rownames(Gender_Consumers)[1] <- "Non-Consumers"
rownames(Gender_Consumers)[2] <- "Consumers"
addmargins(Gender_Consumers)


# Since the sample proportion of males is too small we do not suggest do perform further studies on the difference between these two populations, moreover, since the test is conducted with the snowball sampling technique it does not lead to a representative sample of the whole population, so we will focus on other socio-demographic variables.

# Let's now perform the same exploratory analysis on the rest of the socio-demographic variables.

##Plotting Education level with respect to consumer status
Education_Consumers <- table(data$nat.bought, data$Education.Degree)
rownames(Education_Consumers)[1] <- "Non-Consumers"
rownames(Education_Consumers)[2] <- "Consumers"
margins.Edu.Cons <- addmargins(Education_Consumers,2)


Occupation_Consumers <- table(data$nat.bought, data$Occupation)
rownames(Occupation_Consumers)[1] <- "Non-Consumers"
rownames(Occupation_Consumers)[2] <- "Consumers"
margins.Occ.Cons <- addmargins(Occupation_Consumers,2)

print("Absolute and relative frequency of consumers and non-consumer with respect to education level ")

addmargins(Education_Consumers,2)
for (x in 1:2){
  for (l in 1:3){
    Education_Consumers[x,l] <- round(Education_Consumers[x,l] / margins.Edu.Cons[x, 4]*100, 2)
  }
}
Education_Consumers <- addmargins(Education_Consumers,2)
Education_Consumers



print("Absolute and relative frequency of consumers and non-consumer with respect to occupation ") 
addmargins(Occupation_Consumers,2)
for (x in 1:2){
  for (l in 1:3){
    Occupation_Consumers[x,l] <- round(Occupation_Consumers[x,l]/ margins.Occ.Cons[x,4]*100,2)
  }
}
Occupation_Consumers <-  addmargins(Occupation_Consumers,2)
Occupation_Consumers

# **Considerations**
# 
# Education level.
# - Consumers: The majority of consumers have a bachelor and secondary school education level
# - Non-consumers: The majority on non-consumers have a bachelor degree
# 
# Occupation:
# - Consumers: The majority of consumers are employed, this is reasonable since natural products are costly and not everybody can afford them.
# - Non-consumers: The majority of non-consumers are students.

# Marketing research questions

# Now we focus on the consumers of natural products to find some explorative insights to work on later. Before preceding we have to delete the rows  with null values among the respondents. 

#Removing rows with null values --> cleaning 
Consumers_clean <- (drop_na(consumers)) ##copy of consumers

The cleaned dataset *Consumers_clean* has a dimension of n=122 respondents.

#we know check how the consumers are distributed according to their age
favstats(~Consumers_clean$age)


# The mean of the age of consumers is 33.5, the minimum age is 19 and the maximum age is 67. We have a good span of age values for our research.
# 
# We segment the ages of consumers according to the quantile distribution.
# This will be useful for segmenting the whole consumers' sample into age groups.
# We will use this segmentation later in the research to perform further studies.

Consumers_clean[Consumers_clean$age <= 24, "age_group"] <- "18-24"
Consumers_clean[Consumers_clean$age > 24 & Consumers_clean$age <= 29, "age_group"] <- "25-30"
Consumers_clean[Consumers_clean$age > 29 & Consumers_clean$age <= 39, "age_group"] <- "31-39"
Consumers_clean[Consumers_clean$age > 39, "age_group"] <- "39+"
xtabs(~Consumers_clean$age_group)

labels <- c("18-24: ", "25-30: ", "31-39: ", "39+: ")
x <- xtabs(~Consumers_clean$age_group)
pct <- round(x/sum(x)*100)
labels <- paste(labels, pct) # add percents to labels 
labels <- paste(labels,"%",sep="") # ad % to labels 

Age_plot <- table(Consumers_clean$age_group)
pie(x, col=rainbow(length(1:5)) , main="Age groups distribution in the sample",labels = labels )


# We accept this age segmentation, we cannot divide homogeneously by the size of the age groups since there are many respondents who have 24 years old.

## Question n. 1: Which are the products in which customers are more interested?

# Since the question regarding the most preferred product category is a branching question, we will consider consumers only.
# We will now try to understand which are the overall products the customers are more interested in. 
# To do so we build a frequency table with the answers to question n.2.2 (V4).
# We will plot the relative frequency of the product categories in which customers are more interested.

cons_prod <- Consumers_clean[5:9]
prod_sum <- colSums(cons_prod)/dim(Consumers_clean)[1]*100
prod_sum
barplot(prod_sum, main = "Product categories preferred by consumers", col = "light blue" )


# Looks like the most purchased category is related to face care products, 100% of respondents have purchased the face care product.
# The second most purchased product category is body care, the third most purchased is hair while the least is makeup.

## Question n. 2: How do customers form their information about the products?

# Since the question regarding the most preferred product category is a branching question, we will consider consumers only.
# Let's focus on how our customers found out the channels through which customers found about product characteristics.
# To answer this question we take the response to question 2.3 (V4).

cons_ways <- Consumers_clean[11:17]

ways_sum <- colSums(cons_ways)/dim(Consumers_clean)[1]*100
barplot(ways_sum, main = "Customers preferred channels", col = "light blue" )

# Looks like the most-used channel to gain information about product characteristics is by relying on self-collected information, today thanks to the spread of the internet is common to search for information on internet.
# Since these channels are not directly controlled by the firm, we should focus on the other channels.
# In particular, social networks and the company website are both relevant and should be always kept under maintenance and supervision.
# The least used channels are pharmacists and specialists.
# 
# We now investigate preferred channels relative to the age group so that we can give more specific insight into the marketing group.

ch.age_group <- matrix(0,4,7)

age_groups.tags <- list("18-24","25-30","31-39","39+")

for (rows in 1:4){
  for (col in 1:7){
    score <- sum(Consumers_clean[,10+col][Consumers_clean$age_group == age_groups.tags[rows]])
    ch.age_group[rows,col] <- score
  }
}
colnames(ch.age_group) <- c("Company Website","Word-of-mouth","Network","Advertising","Salespeople","Self-info","specialist")
row.names(ch.age_group) <- age_groups.tags


fre.age_group <- addmargins(ch.age_group,2)
for (x in 1:4){
  for (l in 1:7){
    fre.age_group[x,l] <-   round(fre.age_group[x,l]/fre.age_group[x,8],4)*100
  }
}
fre.age_group[,1:7]


# **Group:**
# Leaving aside self-information which, as we said, is not under the control of the company, we can better focus on the preferred channel for each age group and better customize the company's advertisement on that channel
# 
# - 18-24: As expected the youngest prefer to use social networks, but they also give importance to the information acquired through salespeople.
# - 25-30: This age group prefers social networks and word-of-mouth.
# - 31-39: This group also uses social networks to get information, what is peculiar though is that they don't use either advertising or specialist too much.
# - 39+: The oldest instead, prefer to know the relevant information directly from the company website.
# 
# Now that we have a better look at the preferred channels, it is useful to see the frequency of purchase, question 2.1 (V4), compared to the age groups.

user_age <- addmargins(table(Consumers_clean[,4],Consumers_clean$age_group),1)
rownames(user_age) <- c("Occasional","Moderate","Heavy-Users","Sum")

user_age
user_age.fre <- user_age

for (i in 1:4){
  sum <- user_age[4,i]
  for (l in 1:3){
    user_age[l,i] <- round(user_age[l,i]/sum, 2)
  }
}

print("relative frequencies with respect to the age group of membership")
addmargins(user_age[1:3,],1)


barplot(user_age[1:3,1:4],legend=c("Occasional","Moderate","Heavy-Users"), col = c("red","orange","yellow")) ## plotting relative frequencies with respect to the age group of membership 

##plot using..
#fig <- plot_ly(data.charact, x = ~columns.tags, y = ~means.cons, type = 'bar', name = 'consumers')
#fig <- fig %>% add_trace(y = ~means.Nocons, name = 'non-consumers')
#fig <- fig %>% layout(yaxis = list(title = 'means'), barmode = 'group')


# The group 18-24 is a moderate user, so probably an increase in the presence on the preferred channel could shift them to heavy users.
# Moreover, 25% of them are occasional users, meaning that better planning on social networks and improvements on the salespeople can help shift them to higher user status.
# The groups 25-30 & 31-39 are mostly heavy-moderate users, still, we suggest to keep maintaining the presence in their favorite channel.
# The group 39+ are mostly moderate users, thus an improvement in the preferred channel (company website) can enhance the user status to heavy.
 
## Question n. 3: The differences in attitudes and characteristics between consumers/non-consumers of natural products

# We investigate whether there is a significant difference in the attitudes towards natural and traditional products with respect to customers and non-customers. 
# We can study their differences by doing a t-test between the difference in two proportions, yet we first need to check if the sample size distribution is normal (either by looking at the sample size or at the distribution that should look roughly normal).


##Check sample size of customers
print(dim(consumers)[1]) ## n > 30 (we dont need ot check for the distribution)


##Check sample size of non - customers
print(dim(non_consumers)[1]) ## n < 30 (we should check wheter the distribution is normal or not )

# For customers, we don't need to check for the distribution since the sample size is large enough. 
# On the other hand for non-customers, the sample size is 25, which is why its distribution will be a student t distribution when we'll perform the t-test.
# 
# We build an overall index of attitude by summing up all of the features corresponding to question 3. (variable 18-26).
# In this way, we will obtain an attitudinal index towards the difference in the perception of natural products concerning traditional.
# 
# But before doing that we have to reverse the negative statements impacting the overall attitude score that we will compute.
# We will reverse the variables V23 "Expensive", and V26 "Just a marketing trick" since the statements are negative.
# 
# According to our opinion the variable *trendy* (v20) is an ambiguous statement.
# In order to be consistent, we want to further investigate this statement.
# In order to do so we perform a Chronbach's Alpha test to measure the internal consistency of the likert scale (V18:V26) first with trendy not reverse and secondly with the statement reversed.

##reversing the negative statements attitudinal scores

if (!data_is_reverse){
  for (i in 1: dim(data)[1]){
    new_score <- 11 - data[i, 23]
    data[i, 23] <- new_score
    ##data[i, 20] <- 11 - data[i, 20] 
  }
  for (i in 1: dim(data)[1]){
    new_score <- 11 - data[i, 26]
    data[i, 26] <- new_score
    ##data[i, 20] <- 11 - data[i, 20] 
  }
  data_is_reverse = TRUE ## bool is used in order not to reverse multiple times the scores (by re-running the chunk)
}

#Updating the consumers and non consumers dataset
consumers <- data[data$nat.bought==1,] 
non_consumers <- data[data$nat.bought == 0,]
```

#### Chronbach's Alpha with trendy not reversed

cron <- data.frame(data[,18:26])

cronbach.alpha(cron)

# As we can see from the result the internal consistency is questionable given alpha ranging from 0 to 1, we proceed with the analysis considering the reversed statement.

##reversing the negative statements attitudinal scores
if (!data_is_reverse_trendy){
  for (i in 1: dim(data)[1]){
    new_score <- 11 - data[i, 20]
    data[i, 20] <- new_score
  }
  ##data[i, 20] <- 11 - data[i, 20] 
  data_is_reverse_trendy = TRUE ## bool is used in order not to reverse multiple times the scores (by re-running the chunk)
}

#Updating the consumers and non consumers dataset
consumers <- data[data$nat.bought==1,] 
non_consumers <- data[data$nat.bought == 0,]


#### Chronbach's Alpha with trendy  reversed
cron <- data.frame(data[,18:26])

cronbach.alpha(cron)

# As we can see the value of Chronbach's Alpha with the statement *trendy*  reversed is more consistent (closer to 1) given alpha ranging from 0 to 1.
# Thus the scale with the statement reversed is more reliable and we will keep it reverse for the entire project. 

# Let's go back to research question n.3.
# We can now proceed to build up the overall index of the attitudes toward natural products for the traditional products.

natural_attitude <- rowSums(data[,18:26])

#We now calculate the means in order to correctly specify the alternative hypothesis and perform the test (variable 3 is the consumers / non consumers variable, coded with 0- for non-cons.  and 1 for cons.)

#Check mean of non consumers
mean(natural_attitude[data[,3]==0])

#Check mean of consumers
mean(natural_attitude[data[,3]==1])


# since the mean for consumers is larger (even if not significantly), we can specify an unilateral right alternative hypothesis
t.test(natural_attitude[data[,3]==1],natural_attitude[data[,3]==0], alternative="greater",var.equal=T)

# Since the p-value is basically 0 we are rejecting the possibility of a null difference between the two means (the P-value is the chance to get such an extreme t-test). Thus, to respond to the research question, the difference is statistically significant, indicating that consumers' overall attitudinal score is positive towards the experience with natural products with respect to traditional products. 
# As we can see the consumers of natural products have an overall attitude higher than non-consumers towards natural products.

# Perhaps the marketers have already done a good job in building up the perception of the experience in non-consumers, this could be a benchmark for measuring further progress in a potential campaign of interaction and image targeted at non-consumers.  
# ## Question n. 4: The existence of  particular dimensions along which consumers of cosmetics perceive natural products

# To answer this question we will proceed with a factor analysis of the variables from 18 to 26. 
# Before doing so, we will first perform some tests to check whether is worth proceeding.

# - KMO 

# - cortest.bartlett 

# We now look at the correlation matrix just to have a general overview of the correlations among factors. 


corr.bought<-round(cor(consumers[,18:26]),3)
corr.bought

# From the correlation matrix we can already notice some correlations worth exploring between different factors, let's proceed with KMO and cortest.barlett tests.


KMO(consumers[,18:26])
cortest.bartlett(consumers[,18:26])

#Since the KMO is higher than 0.5 and the p-value of cortest.barlett is lower than 0.05 level of significance we can proceed with the analysis.

anfatt.bought4 <- factanal(consumers[,18:26],5)
anfatt.bought4

#We have a good variance explained but we want to check also for 4 factors since the SS loadings are lower than 1.

#Check the graph below for further visual explanations.

plot(seq(1:5),colSums(anfatt.bought4$loadings^2), xlab="Number of factors",ylab="SS loadings") 
lines(seq(1:5),colSums(anfatt.bought4$loadings^2))
abline(h=1,lty=2, col="red")

anfatt.bought5 <- factanal(consumers[,18:26],4)
anfatt.bought5


# **OUR OBSERVATIONS:**
#   
#   The variance explained is higher than 60% and the SS loadings are higher than 1, thus *four factors* seems the best alternative for this analysis.
# Based on the following values, we have tried to classify the customers into four groups of dimensions along which consumers of cosmetics perceive natural products.
# 
# - FIRST FACTOR: the first factor is more correlated with healthy, higher quality, effectiveness and safeness. We call it **careful factor**
#   
#   - SECOND FACTOR: the second factor is more correlated with good price-performance and expensiveness. We call this factor **price-conscious factor**
#   
#   - THIRD FACTOR: the third factor is more correlated with trendy and just a marketing trick. Since they are both negative statement we call this factor **suspicious factor**
#   
#   - FOURTH FACTOR: the fourth factor is more correlated with green, safeness. We call this factor **eco-friendly factor**
#   
#   # Section 2: Face Care products
#   
#   ## Research Question 5: Which are	the product characteristics on which customers focus their attention?
#   
#   Now we will focus on analyzing the face care products' characteristics on which customers focus their attention.
# First, let's select the consumers that have bought at least one face care product in the last three months and then divide them into consumers who have bought natural products and non-natural products. 

tab.cons <- table(data$bought.face.prod, data$bought.face.prod.nat)
colnames(tab.cons)[1] <- "NoNat.product"
colnames(tab.cons)[2] <- "Nat.product"
row.names(tab.cons)[1] <- "Facial.care Consumers"

tab.cons ##notice that every respondents answered Yes that's why we have a table only with Facial care consumers 

# Since the consumers (and non-consumers) of natural face care products are the same people who answered the general questions about the consumption of natural products we can use the datasets created earlier: consumers and non_consumers of natural products. 

# Now we will compute the mean for every product characteristic for consumers and non-consumer.


means.cons <- c()
means.Nocons <- c()
columns.tags <- c()

for (i in 47:57){
  score_cons <- round(mean(c(consumers[,i])),2) ##calculating the mean of scores for each column of interest
  means.cons <- append(means.cons, score_cons) ##adding the calculated mean to the array
  score_nocons <- mean(c(non_consumers[,i])) ##repeating the same process for non consumers
  means.Nocons <- append(means.Nocons, score_nocons)
  
  ##scraping the column names of interest
  columns.tags <- append(columns.tags, colnames(consumers[i])[1])
}


##Creating  a dataframe containing the means of consumers and non consumers for each charatheristics of the liker scale.
data.charact <- data.frame(columns.tags, means.cons, means.Nocons)
data.charact


##Let's now plot the dataframe for consumers to answer the research question

fig <- plot_ly(data.charact, x = ~columns.tags, y = ~means.cons, type = 'bar', name = 'consumers')
#fig <- fig %>% add_trace(y = ~means.Nocons, name = 'non-consumers')
fig <- fig %>% layout(yaxis = list(title = 'means'), barmode = 'group', xaxis = list(categoryorder = "total descending"))

fig

# Response to the research question:
#   
#   The most relevant characteristics for consumers of face care products are:
#   
#   - Absence of chemical ingredients
# - Presence of natural/organic ingredients
# - Price-quality ratio
# 
# The less significant ones have been proven to be the existence of promotions, the brands' names, and the packaging type.
# As we have seen from the secondary research, the main factors when considering natural products are the organic nature of the ingredients as well as the absence of chemical ingredients. Following this research, we see that costumers care more about the "green dimension" rather than the other product dimension.
# As a consequence, when designing a marketing offering, is important to put these aspect in the first place with respect to the others, because these are the ones that are more relevant for consumers.
# 
# To better understand our findings, we decided to deepen our research trying to study if and how the valuation of the most important characteristics in a product changes between non-consumers and consumers of that product.


##Let's now plot the dataframe 

fig <- plot_ly(data.charact, x = ~columns.tags, y = ~means.cons, type = 'bar', name = 'consumers')
fig <- fig %>% add_trace(y = ~means.Nocons, name = 'non-consumers')
fig <- fig %>% layout(yaxis = list(title = 'means'), barmode = 'group')

fig

# To conclude, these are the more relevant characteristic for non-consumers:
  
# - Price-quality ratio
# - User-friendliness
# - Absence of chemical ingredients
# 
# When you buy a face care product, the main difference in the characteristics between consumers and non-consumers of natural products are:
#   - presence of natural/organic ingredients
# - presence of biodegradable ingredients
# - presence of ecological certifications 
# - absence of chemical ingredients
# 
# ## Research question 6: Which are the factors impacting the willingness to buy?
# 
# To have a better understanding of the customers we linked the willingness to buy to question 9 (V58 "How much would you spend on a face care product?"). 
# 
# By doing this research we want to understand whether the age group, the occupation and face characteristics can influence the willingness to buy.
# 
# But first, we create a new data frame for the purpose of this question only. 


data.clean.q6 <- drop_na(subset(data, select = c(58,97,96)))
#data.clean.q6

#We first give new names to the answer to question (V58) to have a clearer table and analysis and we create a new column by dividing the ages of the respondents into groups.

exp.face <- rep(0,131)
exp.face[which(data.clean.q6[,1]==1)]<-"<15£"
exp.face[which(data.clean.q6[,1]==2)]<-"15-25£"
exp.face[which(data.clean.q6[,1]==3)]<-"25-35£"
exp.face[which(data.clean.q6[,1]==4)]<-"35-50£"
exp.face[which(data.clean.q6[,1]==5)]<-">50£"

data.clean.q6[data.clean.q6$age <= 24, "age_group"] <- "18-24"
data.clean.q6[data.clean.q6$age > 24 &data.clean.q6$age <= 30, "age_group"] <- "25-30"
data.clean.q6[data.clean.q6$age > 30 &data.clean.q6$age <= 39, "age_group"] <- "31-39"
data.clean.q6[data.clean.q6$age > 39, "age_group"] <- "39+"
```

#### Willingness to buy per age group

# We analyze the willingness to buy with respect to the age group by calculating the chi-squared, the contingency, and the lambda index.

wtb.age <- table(data.clean.q6$age_group,exp.face)
wtb.age

#R-squared test, contingency coefficient and lambda
chi<-summary(wtb.age)#where tab is the contingency table we have already calculated and analyzed
phi<-(chi$statistic/(chi$statistic+length(data.clean.q6$age_group)[1]))^(1/2) 

source("/Users/utente1/Desktop/Dataset/Lambda.R")
lambda<-Lambda(exp.face,data.clean.q6$age_group,direction = "row") 

res<-list(chi,phi,lambda)
names(res)<-c("Chisquared","contingency","lambda")
res

# P-value: Since P-Value is smaller than 0.05 (our fixed level of significance)  we reject the hypothesis of having a more extreme observation of our sample, thus the willingness to buy is dependent on the age group.
# 
# The chi-square value of χ^2=32.06 is significant at the 0.001 level. This indicates that the null hypothesis of independence between the two variables should be rejected in favor of the alternative that willingness to purchase depends on age.
# 
# The contingency coefficient value of C=0.44 suggests that there is only a moderate association between the variables, given that the index goes from 0 to 1.
# 
# The index of predictive association λ_(4|3)=0.1 indicates that errors in predicting willingness to purchase are only reduced by 10 percent by taking into account the age group.
# This again suggests that, although statistically significant, the strength between the two variables is low. However, λ does not exclude the presence of other forms of association.
# 
#### Willingness to buy per occupation

# We analyze the willingness to buy with respect to the age group by calculating the chi-squared, the contingency, and the lambda index.

wtb.occ <-  table(data.clean.q6$Occupation,exp.face)
wtb.occ

#R-squared test, contingency coefficient and lambda
chi<-summary(wtb.occ)#where tab is the contingency table we have already calculated and analyzed
phi<-(chi$statistic/(chi$statistic+length(data.clean.q6$Occupation)[1]))^(1/2) 

source("/Users/utente1/Desktop/Dataset/Lambda.R")
lambda<-Lambda(exp.face,data.clean.q6$Occupation,direction = "row") 

res<-list(chi,phi,lambda)
names(res)<-c("Chisquared","contingency","lambda")
res
# 
# P-value: Since P-Value is higher than 0.05 (our fixed level of significance)  we accept the hypothesis of having more extreme observation with respect to our sample, thus the willingness to buy is not dependent on the occupation.
# 
# We can conclude that the willingness to buy doesn't depend on the occupation.

#### Logistic regression willingness to buy with respect to face care characteristics

# First, we have to create new vector wtb.facectc containing only binary responses that we coded in the following way:
# 
# - bought natural products more than 3 times = 1
# - bought natural products 2 or 3 times = 1
# - bought natural products once = 0
# - never bought natural products = 0

# The face care characteristics are at variable from V47 to V57.

# Eventually, we display the logistic regression model.

wtb.facectc <- rep(0,dim(data)[1]) 
wtb.facectc[data[,4]==3] <- 1  
wtb.facectc[data[,4]==2] <- 1 
wtb.facectc[data[,4]==1] <- 0
wtb.facectc[data[,3]==2] <- 0

matrix <- data.matrix(data[,47:57])  # creating a matrix storing the information contained in the columns from 47 to 57 (face care characteristics)

model2 <- glm(wtb.facectc~matrix,family=binomial(link='logit'))  
summary(model2)


# We can already assess that the following dimensions have an significant impact on the WTB:
# - presence of natural/organic ingredients
# - low price/promotions
# - presence of ecological certifications
# 
# Now we exponentiate to get the odds ratios:

round(exp(model2$coefficients),2)


# By looking at the odds ratios we can say that:
# 
# - The odds of buying the face care products because of **presence of natural/organic ingredients** is 98% ((1-1.98)*100) higher for respondents willing to buy with respects to the respondents not willing to buy.  
# 
# - The odds of buying the face care products because of **low price/promotions** is 28% ((1-0.72)*100) smaller for respondents willing to buy with respects to the respondents not willing to buy.  
# 
# - The odds of buying the face care products because of **presence of ecological certifications** is 21% ((1- 1.21)*100) higher for respondents willing to buy with respects to the respondents not willing to buy.
# 
# Now we calculate the goodness of fit of the model:

print(paste('likelihood ratio test, pvalue:',
            round(with(model2, pchisq(null.deviance - deviance,
              df.null - df.residual, lower.tail = FALSE)),9)))

nullmodel2<-glm(wtb.facectc ~ 1,family=binomial(link='logit'))
#calculate the statistic as described in the slides
print(paste('pseudo R squared:',round(1-logLik(model2)/logLik(nullmodel2),3)))


# The model explains more of the probability of willingness to buy with respect to the null model since the p value is very small. 
# 
# The R-squared presents a low value which is a proxy of weak correlation, but it is still recommendable to consider this regression while designing a marketing campaign.
# 
# Finally we can conclude that the two most impactful factors for the respondents when buying a face care products are: presence of natural/organic ingredients, low price/promotions and presence of ecological certifications.


# Section 3: Building marketing personas

# In this section, we will try to cluster the respondents according to lifestyle characteristics. 

## Research Question 7: the existence of particular profiles of customers, in terms of lifestyle, perceptions, sociodemographic characteristics

# We try to find common factors according to lifestyle for all the respondents by plotting the correlation matrix. We find:

data.clean <- drop_na(data)

corr.lifestyle <- round(cor(data.clean[,70:85]),3)
corr.lifestyle


# We build up the KMO and the cortest bartlett analysis to see if the test is worth proceeding.

KMO(data.clean[,70:85])
cortest.bartlett(data.clean[,70:85])

# It's worth proceeding with the cluster analysis because the MSA is bigger than the 0.5.
# The P-value of the Barlett test is close to 0 so we reject the hypothesis that our correlation matrix is equal to the identity matrix, thus there is a correlation worth exploiting for a factor.


factor.lifestyle <-factanal(data.clean[,70:85],5,rotation="varimax",scores="regression")
factor.lifestyle

plot(seq(1:5), colSums(factor.lifestyle$loadings^2), main= "SS loadings with 5 factors", xlab = "SS loadings", ylab = "n of factors") 
lines(seq(1:5), colSums(factor.lifestyle$loadings^2))
abline(h=1,lty=2, col="red")



# **OBSERVATIONS**
#   The variance explained by the 5 factors is not optimal (>60%) but can be sufficient to proceed with the analysis.
# For what concerns the SS loadings result instead, as we can see from the graph, the result is good (>1) for our analysis.
# 
# At this point, the problem becomes naming the factors.
# As we can see from the data we have 4 clear results for factor 1,2,3,5, but the 4th factor is not interesting for our research.
# Factor 4 is strongly related to the research question n.13 V81, "I make use of refined/industrial food products".
# We think that by removing this variable we can better perform factor analysis.

### deleting V81

df <- subset(data.clean, select = -c(81))
corr.lifestyle <- round(cor(df[,70:84]),3)
corr.lifestyle
KMO(df[,70:84])
cortest.bartlett(df[,70:84])


# It's worth proceeding with the cluster analysis because the MSA is bigger than the 0.5.
# The P-value of the test is close to 0 so we reject the hypothesis that our correlation matrix is equal to the identity matrix, thus there is a correlation worth exploiting for a factor.


factor.lifestyle <-factanal(df[,70:84],5,rotation= "varimax",scores="regression")
factor.lifestyle

plot(seq(1:5), colSums(factor.lifestyle$loadings^2), main= "SS loadings with 5 factors", xlab = "SS loadings", ylab = "n of factors") 
lines(seq(1:5), colSums(factor.lifestyle$loadings^2))
abline(h=1,lty=2, col="red")

# With respect to the previous trial without dropping the variable 81, the result of the factor analysis didn't change much.
# What is relevant though is that we can better identify the factor and procced with the cluster analysis.

# **FIRST FACTOR:** Eco-friendly Factor
# Factor 1 is more correlated with the lifestyles:
#   - I take care of myself through natural methods
# - I make use of organic food products
# - I am involved in reducing my impact on the environment, following an environmentally friendly lifestyle 
# - I make use of natural supplements
# - Using natural products makes me feel a better person
# 
# **SECOND FACTOR:** Appearance Factor
# Factor 2 is is more correlated with the lifestyles:
#   - I think that taking care of one’s look is fundamental for her wellbeing
# - I take care a lot of my image
# - To me, it is important to always have a very good appearance 
# 
# **THIRD FACTOR:** Fashion Factor
# Factor 3 is is more correlated with the lifestyles:
#   - A high price and a well-known brand are synonims for quality
# - I prefer to wear designer clothes and buy well known brand products
# - I follow the latest trends in fashion and lifestyle as reported in the social media
# 
# **FOURTH FACTOR:** Informed Factor
# Factor 4 is is more correlated with the lifestyles:
#   - I always read the labels on the products I buy, whatever the product
# - I know the difference between natural and organic products 
# 
# **FIFTH FACTOR:** Personal Care Factor
# Factor 5 is is more correlated with the lifestyles:
#   - I think that taking care of one’s look is fundamental for her wellbeing
# - I take care a lot of my image

### Cluster and customer profiling
# We can now proceed with the cluster analysis based on the previously found factors.

fact.scores <- factor.lifestyle$scores
Eco.Friendly  <- fact.scores[,1]
Appearence <- fact.scores[,2]
Fashion  <- fact.scores[,3]
Informed <- fact.scores[,4]
Personal.care <- fact.scores[,5]
cludat2 <- fact.scores
#fact.scores
fact.load<-factor.lifestyle$loadings


# Let's visualize the clusters using a dendrogram by trying to have 4 different clusters.

d <- dist(cludat2, method = "euclidean")
clu<-hclust(d,method="ward.D")
plot(clu,xlab="",ylab="",hang=-1,cex=0.6)
memb2<-rect.hclust(clu, k=4,which=c(1,2,3,4),border=1:4)

pseudo_F.R(cludat2, clu ,10)

# We look at the loss in R-squared to see confirm what we said on the dendrogram.
# - From 5 to 4 clusters the loss in the variance is 0.25 
# -	From 4 to 3 clusters the loss in the variance is roundabout 0.002
# -	From 3 to 2 clusters the loss is almost 0.046  (most significant decrease) 

# Based on the loss in R^2, we selected 3 clusters to divide our population.
# We now will plot again the dendrogram to see how those are distributed.


d <- dist(cludat2, method = "euclidean")
clu<-hclust(d,method="ward.D")
plot(clu,xlab="",ylab="",hang=-1,cex=0.6)
memb2<-rect.hclust(clu, k=3,which=c(1,2,3),border=1:4)
 
# Since the passage from 3 to 2 groups implies the largest decrease in the R^2 we can conclude that 3 groups represent a good solution for our cluster analysis.
# Let's create a variable storing the membership to each cluster for each respondent by using the function cutree().

#returns cluster membership for each respondent , for k=4 clusters
memb2 <-cutree(clu,k=3) ##Look at which cluster each respondents belongs to

a <- xtabs(~memb2) #returns the number of respondents belonging to each cluster 
addmargins(a)

lbls <- c("1° Clu:", "2° Clu:", "3° Clu:")
pct <- round(a/sum(a)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(a,labels = lbls, col=rainbow(length(lbls)),
    main="Distribution of respondents with respect to clusters")


### Cluster profiling

# Let's now name and identify each cluster.

clu.scores<-matrix(0,3,6)

## Warning: package 'plyr' was built under R version 4.1.3
rownames(clu.scores)<-c("clu1","clu2","clu3")
for (i in 1:3){
  for (j in 1:5){
    clu.scores[i,j]<- round(mean(fact.scores[memb2==i,j]),2) 
    
  }
#count the number of individuals in each cluster
    clu.scores[i,6]<-count(memb2==i)[2,2]
  }

colnames(clu.scores)<-c("Eco.Friendly","Appearence", "Fashion","Informed","Personal care","Size")
clu.scores


# **First Cluster:** The “Eco-Fashion customers”
# These kinds of people are attracted to fashion. Fashion is the most important factor, but not the only one. In fact, these consumers also care about the environment, even more than the need to appear. They are so informed about organic substances, they can waste lots of energy and hours reading the labels of the trendy products and doing research about the best ingredients for their wellbeing.
# They want to help the planet without renouncing it to be trendy.
# For this reason, we will name them  “Eco-Fashion Customers”. 
# Sustainability and tendencies are a good mix for them.
# 
# **Second Cluster:** The “Fashionista” customers
# This cluster is made of individuals that put Fashion and Personal care before the well-being of the planet. They don’t care about sustainable alternatives, they just think about their health and tendencies. 
# Think about Instagram: if my favorite influencer or actress sponsors something, I will be more willing to buy it because she did. I won’t ask myself if it’s sustainable or not, if she uses it is ok. I won’t care about price or sustainability, I will only care to take care of myself as my idol does. 
# 
# **Third Cluster:** The “Earth lover” customers
# To render the idea of this type of customer, we’ll name them “Earth lovers”. The main concern of this cluster is to respect the planet in every choice they make. And in everyday life, this is translated into choosing organic foods, supplements, and cosmetics.
# Environmental issues are problems that Earth lovers struggle every day with. Sustainable choices make them feel better. Fashion, tendencies, and appearance are totally irrelevant: only Earth’s health matters. For this reason, they reach a good score with respect to the values of “eco.friendly” and “informed”. They are the kind of people that spend time reading all the labels of the products to pick up the one that does less damage to the planet.

## Characterization of the clusters

# We want to name the clusters to identify them by averaging the factor scores in relation to the composition of the clusters. We count the number of members and add up their scores and take the average.
# 
# Once named the clusters according to the factors used to build them, we need to characterize the clusters according to other variables: we start with the socio-demographic variables. To characterize the groups we have to compare the average (median) of the quantitative variables (percentage of each level for categorical variables) in each group concerning the whole sample. We use the median as it is more robust for the arithmetic mean.


### Quantitive 

#worked years
m.age.clu <-matrix(0,1,4)
colnames(m.age.clu)<-c("clu1","clu2","clu3","sample")
rownames(m.age.clu)<-c("median age of clusters")

ages <- data.clean[,97]

for (i in 1:3){
  m.age.clu[i] <- median(ages[memb2==i])
  }
#median for the whole sample
m.age.clu[4]<-median(ages)
m.age.clu

library(plyr)
#see a different way to rename variable levels
exp.face<-rep(0,122)
exp.face[which(data.clean[,58]==1)]<-"<15£"
exp.face[which(data.clean[,58]==2)]<-"15-25£"
exp.face[which(data.clean[,58]==3)]<-"25-35£"
exp.face[which(data.clean[,58]==4)]<-"35-50£"
exp.face[which(data.clean[,58]==5)]<-">50£"


data.clean[data.clean$age <= 24, "age_group"] <- "18-24"
data.clean[data.clean$age > 24 &data.clean$age <= 30, "age_group"] <- "25-30"
data.clean[data.clean$age > 30 &data.clean$age <= 39, "age_group"] <- "31-39"
data.clean[data.clean$age > 39, "age_group"] <- "39+"

face.hour<-rep(0,122)

face.hour[which(data.clean[,60]==1)]<-"<1h"
face.hour[which(data.clean[,60]==2)]<-"1-2h"
face.hour[which(data.clean[,60]==3)]<-"2-3h"
face.hour[which(data.clean[,60]==4)]<-">3h"




s <- list(prop.table(table(memb2,data.clean$Occupation),1)*100 , prop.table(table(data.clean$Occupation))*100,
          prop.table(table(memb2,data.clean$Education.Degree),1)*100 , prop.table(table(data.clean$Education.Degree))*100 , 
          prop.table(table(memb2,exp.face),1)*100 , prop.table(table(exp.face))*100 ,
          prop.table(table(memb2,data.clean$age_group),1)*100 , prop.table(table(data.clean$age_group))*100 ,
          prop.table(table(memb2,data.clean$type.of.face.care.prod),1)*100 , prop.table(table(data.clean$type.of.face.care.prod))*100 ,
          prop.table(table(memb2,face.hour),1)*100 , prop.table(table(face.hour))*100 )


# Now we want to focus on the socio-demographic variables with respect to the clusters.


lapply(s[0:4],round,2)

lapply(s[7:8],round,2)


# And, now we want to focus on the use of face products with respect to the clusters.

lapply(s[5:6],round,2)

lapply(s[9:12],round,2)


### Conclusion

# After reviewing the results of our analysis of the survey responses, we suggest that decision-makers move into green marketing.
# 
# In our analysis, we have identified three types of customers and their characteristics.
# 
# **Eco-Fashion Customers**: In the analysis of the occupation of this type of cluster, we can see that the employed people are in line with the mean, but there is a difference between the students, the unemployed, and their average values. Then we have the type of graduation, and we can note that only the Secondary School value is close to the average one, the others, Bachelor Degree and Master Degree, are higher and lower respectively than their averages.
# Next, we divided our customers into age groups and it turned out that only the group of 25-30 has no particular difference with average, while the rest of the age groups have medium-high differences. In addition, there is also the classification of how much people spend on skincare, and here it rises that nobody is within the standard.
# Talking about the age, also the type of skin the customers have is a factor that must be considered, and it turns out that in this cluster only people with sensitive skin are on average.
# The last area that we analyzed is of the time spent on face care, and we can state that only those that spend more than 3 hours are in accordance with the mean.
# 
# **Fashionistas**: Talking about the occupation level of this cluster, we note that the employed people and students are in line with the mean values, while unemployed people are about half the average value. We can also see that, concerning the type of degree, the only value in line with his average value is the Master degree, while the bachelor degree value is slightly above the average and the Secondary school degree is slightly less than the average. If we analyze the age range of this cluster, we note that the biggest values are the 18-24 one, which is also quite higher than the average value of that category, and the 25-30 one, which is slightly above the average.  If we think about it, these results are consistent because it is easier to influence young people than older people when buying a product. Regarding the expenditures range of this cluster, we note that the majority of the people inside this group are more willing to spend an amount of money between 15£ and 25£; this is coherent with the fact that the majority of components of this cluster are young people, and so without a great amount of money to spend. These people use facial products mainly to keep the skin hydrated, further demonstrating that the people within this cluster are predominantly young, so they have no special needs other than to keep the skin hydrated.
# Finally, talking about the time spent on face care, we can state that half of this cluster spends no more than one hour on face care, which is bigger than the average since these people are students or young workers, so they do not have a lot of time to spend on this.
# 
# **Earth Lover Customers:** The occupation here is the same concept as the cluster of Eco-Fashion Customers, and also with the type of graduation, with the only difference that the Bachelor Degree is lower than the average and the Master Degree is higher.
# The age groups point out that, like the first cluster, the group of 25-30 has no particular difference with the average. The amounts of expenditure are all different for the average, except the <15 £ and 25-35 £.
# The type of skin the customers have is more or less in line with all the standard values, while the time spent on face care has some values which are similar to the average ones and others which are different.


# Final conlusion of the research

# First let's recap the findings to the research question:
  
#   - The most purchased products are face care products and body care (the least make up)
# 
# - Social networks and the company website are the preferred channels through which consumers get their informations:
#   
#   - 18-24: prefer to use social networks
# 
# - 25-30: prefer social networks and word-of-mouth.
# 
# - 31-39: prefer social networks 
# 
# - 39+: prefer the company website
# 
# - The main dimension on which customers perceive natural products are: carefulness (safety), price-consciousness, suspicious and eco-friendly factor
# 
# - The most relevant characteristics for consumers of natural face care products are:
#   
#   - Absence of chemical ingredients
# 
# - Presence of natural/organic ingredients
# 
# - Price-quality ratio
# 
# 
# - There are 3 main personas:
#   - Eco-fashon customer
# - Fashonistas
# - Earth lover customers
# 
# We remind that our suggestion are **solely** based on our information at disposal and should **not** be considered as action oriented but just as information oriented.



  
  