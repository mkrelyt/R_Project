read.csv
myDF <- read.csv("/Users/tylerkim/Downloads/Levels_Fyi_Salary_Data.csv",
                 header=TRUE,
                 sep=",")
view(myDF)
myDF <- myDF %>% mutate(company=tolower(company),level=toupper(level),title=tolower(title),location=tolower(location))
dim(myDF)
tail(myDF)
head(myDF)
names(myDF)
str(myDF)
summary(myDF)

microsoft <- myDF %>% subset(company=='microsoft')
dim(microsoft)
tail(microsoft)

fig <-  function(width,height) {
  options(repr.plot.width=width,repr.plot.height=height)
}

par(mfrow=c(1,3),mar=c(4,4,2,1))
table(microsoft$Race)  %>% barplot(main='Race in microsoft',col='red')
table(microsoft$gender) %>% barplot(main='Gender in microsoft',col='green')
table(microsoft$Education) %>% barplot(main='Education in microsft',col='blue')

google <- myDF %>% subset(company=='google')
dim(google)
tail(google)

par(mfrow=c(1,3),mar=c(4,4,2,1))
table(google$Race)  %>% barplot(main='Race in Google',col='pink')
table(google$gender) %>% barplot(main='Gender in Google',col='yellow')
table(google$Education) %>% barplot(main='Education in Google',col='orange')


facebook <- myDF %>%  filter(company=='facebook')
dim(facebook)
tail(facebook)

par(mfrow=c(1,3),mar=c(4,4,2,1))
table(facebook$Race)  %>% barplot(main='Race in facebook',col='violet')
table(facebook$gender) %>% barplot(main='Gender in facebook',col='lightblue')
table(facebook$Education) %>% barplot(main='Education in facebook',col='brown')

amazon <- myDF %>%  filter(company=='amazon')
dim(amazon)
tail(amazon)

par(mfrow=c(1,3),mar=c(4,4,2,1))
table(amazon$Race)  %>% barplot(main='Race in amazon',col='lightgreen')
table(amazon$gender) %>% barplot(main='Gender in amazon',col='maroon')
table(amazon$Education) %>% barplot(main='Education in amazon',col='beige')

apple <- myDF %>%  filter(company=='apple')
dim(apple)
tail(apple)

par(mfrow=c(1,3),mar=c(4,4,2,1))
table(apple$Race)  %>% barplot(main='Race in apple',col='orange')
table(apple$gender) %>% barplot(main='Gender in apple',col='blue')
table(apple$Education) %>% barplot(main='Education in apple',col='purple')

i <-  grep('2018',microsoft$timestamp)
microsoft2018 <- microsoft %>% slice(i)
dim(microsoft2018)
table(microsoft2018$location) %>% names
table(microsoft2018$level) %>% names
tail(microsoft2018)

microsoft2018 %>% select_if(is.numeric) %>% cor %>% data.frame %>% select('totalyearlycompensation') %>% 
  arrange(-totalyearlycompensation)

microsoft2018 <- microsoft2018 %>% group_by(title) %>% 
  summarize(n=n(),totalyearlycompensation=mean(totalyearlycompensation),
            basesalary=mean(basesalary),bonus=mean(bonus),stockgrantvalue=mean(stockgrantvalue),
            yearsofexperience=round(mean(yearsofexperience),1),yearsatcompany=round(mean(yearsatcompany),1)) %>% 
  arrange(-n)
microsoft2018

microsoft2018 %>% ggplot(aes(fct_reorder(title,-n),n))+geom_col()+labs(title='Staffing at Microsoft 2018',x='job title')

microsoft2018 %>% ggplot(aes(fct_reorder(title,-totalyearlycompensation),totalyearlycompensation))+geom_col()+
  labs(title='Average salary Microsoft 2018',x='job title')

i <-  grep('2018',google$timestamp)
google2018 <- google %>% slice(i)
dim(google2018)
table(google2018$location) %>% names
table(google2018$level) %>% names
tail(google2018)

google2018 %>% select_if(is.numeric) %>% cor %>% data.frame %>% select('totalyearlycompensation') %>% 
  arrange(-totalyearlycompensation)

google2018 <-  google2018 %>% group_by(title) %>% 
  summarize(n=n(),totalyearlycompensation=mean(totalyearlycompensation),
            basesalary=mean(basesalary),bonus=mean(bonus),stockgrantvalue=mean(stockgrantvalue),
            yearsofexperience=round(mean(yearsofexperience),1),yearsatcompany=round(mean(yearsatcompany),1)) %>%
  arrange(-n)
google2018

google2018 %>% ggplot(aes(fct_reorder(title,-n),n))+geom_col()+labs(title='Staffing at Google 2018',x='job title')

google2018 %>% ggplot(aes(fct_reorder(title,-totalyearlycompensation),totalyearlycompensation))+geom_col()+
  labs(title='Average salary Google 2018',x='job title')

i <-  grep('2018',facebook$timestamp)
facebook2018 <- facebook %>% slice(i)
dim(facebook2018)
table(facebook2018$location) %>% names
table(facebook2018$level) %>% names
tail(facebook2018)

facebook2018 %>% select_if(is.numeric) %>% cor %>% data.frame %>% select('totalyearlycompensation') %>% 
  arrange(-totalyearlycompensation)

facebook2018 <-  facebook2018 %>% group_by(title) %>% 
  summarize(n=n(),totalyearlycompensation=mean(totalyearlycompensation),
            basesalary=mean(basesalary),bonus=mean(bonus),stockgrantvalue=mean(stockgrantvalue),
            yearsofexperience=round(mean(yearsofexperience),1),yearsatcompany=round(mean(yearsatcompany),1)) %>%
  arrange(-n)
facebook2018

facebook2018 %>% ggplot(aes(fct_reorder(title,-n),n))+geom_col()+labs(title='Staffing at Facebook 2018',x='job title')

facebook2018 %>% ggplot(aes(fct_reorder(title,-totalyearlycompensation),totalyearlycompensation))+geom_col()+
  labs(title='Average salary Facebook 2018',x='job title')

i <-  grep('2018',amazon$timestamp)
amazon2018 <- amazon %>% slice(i)
dim(amazon2018)
table(amazon2018$location) %>% names
table(amazon2018$level) %>% names
tail(amazon2018)

amazon2018 %>% select_if(is.numeric) %>% cor %>% data.frame %>% select('totalyearlycompensation') %>% 
  arrange(-totalyearlycompensation)

amazon2018 <-  amazon2018 %>% group_by(title) %>% 
  summarize(n=n(),totalyearlycompensation=mean(totalyearlycompensation),
            basesalary=mean(basesalary),bonus=mean(bonus),stockgrantvalue=mean(stockgrantvalue),
            yearsofexperience=round(mean(yearsofexperience),1),yearsatcompany=round(mean(yearsatcompany),1)) %>%
  arrange(-n)
amazon2018

amazon2018 %>% ggplot(aes(fct_reorder(title,-n),n))+geom_col()+labs(title='Staffing at Amazon 2018',x='job title')

amazon2018 %>% ggplot(aes(fct_reorder(title,-totalyearlycompensation),totalyearlycompensation))+geom_col()+
  labs(title='Average salary Amazon 2018',x='job title')

i <-  grep('2018',apple$timestamp)
apple2018 <- apple %>% slice(i)
dim(apple2018)
table(apple2018$location) %>% names
table(apple2018$level) %>% names
tail(apple2018)

apple2018 %>% select_if(is.numeric) %>% cor %>% data.frame %>% select('totalyearlycompensation') %>% 
  arrange(-totalyearlycompensation)

apple2018 <-  apple2018 %>% group_by(title) %>% 
  summarize(n=n(),totalyearlycompensation=mean(totalyearlycompensation),
            basesalary=mean(basesalary),bonus=mean(bonus),stockgrantvalue=mean(stockgrantvalue),
            yearsofexperience=round(mean(yearsofexperience),1),yearsatcompany=round(mean(yearsatcompany),1)) %>%
  arrange(-n)
apple2018

apple2018 %>% ggplot(aes(fct_reorder(title,-n),n))+geom_col()+labs(title='Staffing at Apple 2018',x='job title')

apple2018 %>% ggplot(aes(fct_reorder(title,-totalyearlycompensation),totalyearlycompensation))+geom_col()+
  labs(title='Average salary Apple 2018',x='job title')

ggplot(data = myDF, aes(x = location, y = totalyearlycompensation)) +
  geom_boxplot()

myDF <- tibble(location = runif(100, min = 0, max = 25)
                       ,totalyearlycompensation = log2(location) + rnorm(100)
)
ggplot(data = myDF, aes(x = location, y = totalyearlycompensation)) +
  geom_point()

myDF <- tibble(basesalary = runif(100, min = 0, max = 25)
               ,totalyearlycompensation = log2(basesalary) + rnorm(100)
               
)

ggplot(data = myDF, aes(x = basesalary, y = totalyearlycompensation)) +
  geom_point()

myDF <- tibble(yearsofexperience = runif(100, min = 0, max = 25)
               ,totalyearlycompensation = log2(yearsofexperience) + rnorm(100)
               
)

ggplot(data = myDF, aes(x = yearsofexperience, y = totalyearlycompensation)) +
  geom_point()

myDF <- tibble(yearsatcompany = runif(100, min = 0, max = 25)
               ,totalyearlycompensation = log2(yearsatcompany) + rnorm(100)
               
)

ggplot(data = myDF, aes(x = yearsatcompany, y = totalyearlycompensation)) +
  geom_point()
