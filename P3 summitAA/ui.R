###########################################################################
##R Shiny App to investigate the HIV
## xiyou Zhou- 2019
###########################################################################

#Load package
library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)


# from https://wonder.cdc.gov/aids-v2002.html

HIV<-read_csv("HIV_DD.csv")

HIV<-filter(HIV, !is.na(Cases)&!is.na(Population))

#HIV<-HIV%>%select(-'Rate per 100000')
#HIV<-HIV %>% mutate(Rate_per_100000 = 100000*Cases/Population)

HIV$Year<-factor(HIV$Year)
HIV$Indicator<-factor(HIV$Indicator)
HIV$Cases<-as.integer(HIV$Cases)
HIV$Population<-as.integer(HIV$Population)


HIV<-HIV %>% mutate(Rate_per_100000 = 100000*Cases/Population)
HIV<-filter(HIV, !is.na(Cases)&!is.na(Rate_per_100000))%>% select(-`Rate per 100000`)

HIV


HIV2<-HIV%>%group_by(Geography)%>%summarise(meanCase=mean(Cases,na.rm=T), meanPop=mean(Population,na.rm=T))


HIV$Survive <- ifelse((HIV$Indicator == "AIDS deaths" | HIV$Indicator == "HIV deaths"), 0 ,1)

set.seed(100)
index<-sample(1:nrow(HIV), 800)
index

HIV3<-HIV[index,]
HIV3<-HIV3
HIV3<-filter(HIV3, !is.na(Cases)&!is.na(Rate_per_100000)&!is.na(Survive))

write.csv(HIV3, file = "HIV3.csv",row.names=FALSE, na="")


HIV4<-read_csv("HIV3.csv")


dashboardPage( #skin="red",
              dashboardHeader(title ="HIV AID"),

              #define sidebar items
              dashboardSidebar(sidebarMenu(
                menuItem("intro", tabName = "intro", icon = icon("archive")),
                menuItem("datasum", tabName = "datasum", icon = icon("laptop")),
                menuItem("unsupervised learning", tabName = "unsupervisedLearn", icon = icon("archive")),
                menuItem("supervised learning", tabName = "supervisedLearn "),
                menuItem("scroll", tabName = "scroll"),
                menuItem("myGithub",  href = "https://github.com/xiyouzhou", icon=icon("code"))
                         ) ),

             # tabName = "myGithub" icon = icon("archive" icon = icon("archive")
              #define the body of the app
              dashboardBody(
                #tags$div(
                tabItems(
                  # First tab content
                  tabItem(tabName = "intro",
                          fluidRow(
                            #add in latex functionality if needed
                            withMathJax(),

                            #two columns for each of the two items
                            column(6,
                                   #Description of App
                                   h1("What does this app do?"),
                                   #box to contain description
                                   box(background="red",width=12,
                                       h4("This applet allows for the visualization of HIV and AID distribution in USA 50 states" ),
                                       h4("Gay and bisexual men accounted for 66% (25,748) of all HIV diagnoses and 82% of HIV diagnoses among males"),
                                       h4("Black/African American gay and bisexual men accounted for the largest number of HIV diagnoses (9,807), followed by Hispanic/Latinos (7,436) and whites (6,982)"),
                                       h4("African Americans and Hispanics/Latinos are disproportionately affected by HIV In 2017"),
                                       h4("36.9 millions people worldwide lived with HIV in 2017"),
                                       h4("recently, anti-HIV drugs are used to prevent and keep HIV in check, but vaccination against HIV, most effcient and economical way of prevention and treatmen of infectious still a long way to go"),
                                       h4("This apply try to use statistic method to analyze the HIV  status in USA"),
                                       h4("logit is given by $$log (\\frac {p} {1-p}) = \\beta_0 +\\beta_1* x_1 + \\beta2* x_2+\\beta_p* x_p$$ ")
                                   )
                            ),

                            column(6,
                                   #How to use the app
                                   h1("How to use the app?"),
                                   #box to contain description
                                   box(background="red",width=12,
                                       h4("The controls for the app are located to the left, the visualization appears in the middle, and the approximation information is given on the right."),
                                       h4("The parameters of the Gamma random variable can be adjusted using the boxes on the left."),
                                       h4("The graph in the middle displays this distribution, the function \\(y=\\frac{1}{x}\\), and the first and second order Taylor polynomials about the mean of the Gamma distribution."),
                                       h4("The box on the right displays the true mean of the distribution of \\(Y=1/X\\) and the approximations.")
                                   )
                            )
                          )
                  ),

                  #actual app layout
                  tabItem(tabName = "datasum",
                          fluidRow(
                            column(2,
                                   box(width=12,title="yearly summary",
                                       background="red",solidHeader=TRUE,
                                       h5("(choose a year.)"),
                                       selectizeInput("Year", "year", selected = "2016", choices = levels(as.factor(HIV$Year))

                                   ))
                            ),
                            column(width=7,
                                   fluidRow(
                                     box(width=12,
                                         tableOutput("table1"),
                                         br(),
                                         h4("The table above displays the  20 rows of population status of HIV in USA")
                                     )
                                   )
                            ),
                            column(width=3,
                                   fluidRow(
                                     box(width=12,
                                         verbatimTextOutput("summary1"),
                                         br(),
                                         h4("The first column provides year,The second column is the HIV cases for that year"),

                                         h4("The other columns provide mean, median and  SD ")
                                     )
                                   )
                            )
                          )
                  )
               


















                      )


      )

)























