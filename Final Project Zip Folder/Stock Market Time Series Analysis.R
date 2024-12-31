library(fpp3)
library(zoo)
library(lubridate)
library(anytime)
library(shiny)
library(shinythemes)
library(plotly)
library(ggdark)
library(urca)

NASDAQ <- read.csv("NASDAQ Composite Index.csv")

NASDAQ$Date <- make_date(year = NASDAQ$Year, month = NASDAQ$Month, NASDAQ$Day)

NASDAQ <- na.omit(NASDAQ)

which(is.na(NASDAQ))

names(NASDAQ)[names(NASDAQ) == "Adj.Close"] <- "NASDAQ Adj. Close"

NASDAQ <- NASDAQ[c("Date","NASDAQ Adj. Close")]

Russel2000 <- read.csv("Russel 2000 Index.csv")

Russel2000$Date <- make_date(year = Russel2000$Year, month = Russel2000$Month, Russel2000$Day)

names(Russel2000)[names(Russel2000) == "Adj.Close"] <- "Russell 2000 Adj. Close"

Russel2000 <- Russel2000[c("Date","Russell 2000 Adj. Close")]

SP500 <- read.csv("S&P 500 Index.csv")

SP500$Date <- make_date(year = SP500$Year, month = SP500$Month, SP500$Day)

names(SP500)[names(SP500) == "Adj.Close"] <- "S&P 500 Adj. Close"

SP500 <- SP500[c("Date","S&P 500 Adj. Close")]

IndexMaster <- merge(NASDAQ, SP500, by = "Date")

IndexMaster <- merge(IndexMaster, Russel2000, by = "Date")

IndexMaster <- IndexMaster %>% 
  tsibble(index = "Date")

highreturn <- c(0,0,0)

listofindicesC <- c("NASDAQ Composite Index","S&P 500 Index","Russell 2000 Index")

# train <- head(IndexMaster, -24)
# holdout <- tail(IndexMaster, 24)

## NASDAQ Accuracy
# NASDAQforecasts <- train %>%
#   model(SeasonalN = SNAIVE(NASDAQ_Adj_Close),
#         tslinmod = TSLM(NASDAQ_Adj_Close ~ trend() + season()),
#         arimamod = ARIMA(NASDAQ_Adj_Close),
#         NeuralNet = NNETAR(NASDAQ_Adj_Close)) %>%
#   forecast( h = 24, times = 100) %>% 
#   accuracy(holdout)

## Russell 2000 Accuracy
# Russ2000acc <- train %>% 
#   model(SeasonalN = SNAIVE(Russell_2000_Adj_Close),
#         tslinmod = TSLM(Russell_2000_Adj_Close ~ trend() + season()),
#         arimamod = ARIMA(Russell_2000_Adj_Close),
#         NeuralNet = NNETAR(Russell_2000_Adj_Close)) %>%
#   forecast( h = 24, times = 100) %>% 
#   accuracy(holdout)

## S&P 500 Accuracy
# SP500acc <- train %>% 
#   model(SeasonalN = SNAIVE(SP_500_Adj_Close),
#         tslinmod = TSLM(SP_500_Adj_Close ~ trend() + season()),
#         arimamod = ARIMA(SP_500_Adj_Close),
#         NeuralNet = NNETAR(SP_500_Adj_Close)) %>%
#   forecast( h = 24, times = 100) %>% 
#   accuracy(holdout)

ui <- fluidPage(
  theme = shinytheme("cyborg"),
  titlePanel("Stock Index Time Series Application",
             windowTitle = "Stock Index Time Series Application"),
  sidebarPanel(
  selectInput(inputId = "Index_Selected",
              label = "Index Selected:",
              choices = listofindicesC
              ),
  selectInput(inputId = "select_plot",
              label = "Display a secondary plot:",
              choices = c("Seasonality","Autocorrelation","Decomposition","Seasonal Naive",
                          "ARIMA","NNETAR","TSLM")),
  tags$h6("Optimal Index and Return Feature"),
  helpText("Below are inputs that will allow you to see the index with",
           "the highest return over a selected date range. You will see",
           "the total return on your investment as a percentage, as well as",
           "the dollar value of you investment at the end of the selected",
           "period."),
  sliderInput(inputId = "date_select",
              label = "Select a Date Range:",
              min = as.Date("2019-03-04", "%Y-%m-%d"),
              max = as.Date("2024-03-04", "%Y-%m-%d"),
              value = c(as.Date("2019-03-04", "%Y-%m-%d"),as.Date("2024-03-04", "%Y-%m-%d")),
              step = 7,
              timeFormat = "%Y-%m-%d"),
  numericInput(inputId = "dollarinv",
               label = "Money invested at the beginning of period (please enter without $ sign)",
               value = 100),
  tags$h6("ROI from Investment over Period"),
  textOutput(outputId = "tpreturn")),
  wellPanel(tags$h5(textOutput(outputId = "tsheader1")),textOutput(outputId = "tsinterp"),
            tags$h5(textOutput(outputId = "tsheader2")), textOutput(outputId = "variedplotinterp")),
  mainPanel(
  plotlyOutput(outputId = "timeplot"),
  plotlyOutput(outputId = "variedplots"),
  textOutput(outputId = "typeofplot")
  )
  
)

server <- function(input,output, session){
  output$timeplot <- renderPlotly(
    IndexMaster[c(1,(which(listofindicesC == input$Index_Selected))+ 1)] %>% 
      tsibble(index = "Date") %>% 
      autoplot(level = NULL) + labs(title = input$Index_Selected) + 
      dark_theme_dark()
)
  
output$variedplots <- renderPlotly(
  if(input$select_plot == "Seasonality"){
    IndexMaster[c(1,(which(listofindicesC == input$Index_Selected))+ 1)] %>% 
      tsibble(index = "Date") %>% 
      gg_season() + labs(title = input$select_plot) + 
      dark_theme_dark()
  } else if(input$select_plot == "Autocorrelation"){
    IndexMaster[c(1,(which(listofindicesC == input$Index_Selected))+ 1)] %>% 
      tsibble(index = "Date") %>% 
      ACF(lag_max = 104) %>% 
      autoplot(level = NULL) + labs(title = input$select_plot) +
      dark_theme_dark()
  } else if(input$select_plot == "Decomposition"){
    IndexMaster[c(1,(which(listofindicesC == input$Index_Selected))+ 1)] %>% 
      tsibble(index = "Date") %>% 
      model(STL(robust = TRUE)) %>%
      components() %>%
      autoplot(level = NULL) + labs(title = input$select_plot) +
      dark_theme_dark()
  } else if(input$select_plot == "Seasonal Naive"){
    IndexMaster[c(1,(which(listofindicesC == input$Index_Selected))+ 1)] %>% 
      mutate(Date = yearweek(Date)) %>% 
      tsibble(index = "Date") %>% 
      model(SNAIVE()) %>% 
      forecast(h = 24) %>% 
      autoplot(IndexMaster[c(1,(which(listofindicesC == input$Index_Selected))+ 1)],
               level = NULL) + labs(title = input$select_plot) +
      dark_theme_dark()
  } else if(input$select_plot == "ARIMA"){
    IndexMaster[c(1,(which(listofindicesC == input$Index_Selected))+ 1)] %>% 
      mutate(Date = yearweek(Date)) %>% 
      tsibble(index = "Date") %>% 
      model(ARIMA()) %>% 
      forecast(h = 24) %>% 
      autoplot(IndexMaster[c(1,(which(listofindicesC == input$Index_Selected))+ 1)],
               level = NULL) + labs(title = input$select_plot) +
      dark_theme_dark()
  } else if(input$select_plot == "NNETAR"){
    IndexMaster[c(1,(which(listofindicesC == input$Index_Selected))+ 1)] %>% 
      mutate(Date = yearweek(Date)) %>% 
      tsibble(index = "Date") %>% 
      model(NNETAR()) %>% 
      forecast(h = 24, times = 100) %>% 
      autoplot(IndexMaster[c(1,(which(listofindicesC == input$Index_Selected))+ 1)],
               level = NULL) + labs(title = input$select_plot) +
      dark_theme_dark()
  } else {
    IndexMaster[c(1,(which(listofindicesC == input$Index_Selected))+ 1)] %>% 
      mutate(Date = yearweek(Date)) %>% 
      tsibble(index = "Date") %>% 
      model(TSLM(~trend() + season())) %>% 
      forecast(h = 24) %>% 
      autoplot(IndexMaster[c(1,(which(listofindicesC == input$Index_Selected))+ 1)],
               level = NULL) + labs(title = input$select_plot) +
      dark_theme_dark()
  }
  
)


 highreturn <- reactive({c(((NASDAQ[which(NASDAQ$Date == input$date_select[2]),2]
  - NASDAQ[which(NASDAQ$Date == input$date_select[1]),2])
  /NASDAQ[which(NASDAQ$Date == input$date_select[1]),2]),
  (SP500[which(SP500$Date == input$date_select[2]),2]
   - SP500[which(SP500$Date == input$date_select[1]),2])
  /SP500[which(SP500$Date == input$date_select[1]),2],
    (Russel2000[which(Russel2000$Date == input$date_select[2]),2]
  - Russel2000[which(Russel2000$Date == input$date_select[1]),2])
  /Russel2000[which(Russel2000$Date == input$date_select[1]),2])
  })

 plottype <- reactive({
   which(c("Seasonality","Autocorrelation","Decomposition","Seasonal Naive",
     "ARIMA","NNETAR","TSLM") == input$select_plot)
 })
 
 output$typeofplot <- plottype
 
output$tpreturn <- renderText(paste("The index that would have generated the greatest return over that period is the ",
                                     as.character(listofindicesC[which(highreturn() == max(highreturn()))]),
                                     " with a total return of " , as.character(round(max(highreturn())*100, digits = 2)),
                                    "%." , " If you invested $",input$dollarinv,
                                    " at the beginning of the period, your investment at the end of the period would have ",
                                    "a value of $", sprintf('%.2f',(input$dollarinv * max(highreturn()) + input$dollarinv)),
                                    ".",sep = ""))

output$tsheader2 <- renderText(paste(input$select_plot, " Interpretation for ",input$Index_Selected))
output$tsheader1 <- renderText(paste(input$Index_Selected," Time Series Interpretation"))

output$tsinterp <- renderText(if(input$Index_Selected == "NASDAQ Composite Index"){
  paste("The time series plot of the NASDAQ Composite Index has a general upward trend",
        " over time. It is difficult to cleanly describe the trend as either additive or ",
        "multiplicative. However, I would lean towards describing it as additive.",
        "There are also multiple notable irregularities within the NASDAQ's ",
        "pricing data. The first and most obvious one being March of 2020, which ",
        "essentially marks the first few weeks of the Coronavirus pandemic. Another ",
        "quite noticable irregularity is the change in trend in 2022. It is almost ",
        "entirely negative despite being mostly positive outside of 2022. There are a ",
        "large variety of factors that played into the NASDAQ having a down year in 2022, ",
        "this includes things such as rising inflation, heightened borrowing costs for ",
        "businesses and individuals, as well as fear in the markets from the war in ",
        "Ukraine. The NASDAQ suffered the most out of the S&P 500, NASDAQ and Russell 2000 ",
        "during this time period with a decline of -33.89% in value from the beginning of ",
        "the year to the end of the year. This is somewhat reasoable to expect, as the ",
        "NASDAQ is comprised of more tech related stocks, which have a tendancy to be ",
        "more volatile than stocks held in the S&P 500 for example. It is difficult to decipher ",
        "the cyclical nature of this data. Generally, it appears that the markets appear to perform ",
        "slightly better around the end of a fiscal quarter, meaning March, June, September, and December. ",
        "Of course, it is not a perfectly repeatable cycle, but this is somewhat of a pattern.",sep = "")
} else if(input$Index_Selected == "S&P 500 Index"){
  paste("The time series plot of the S&P 500 Index has a general upward trend",
        " over time. There are also multiple notable irregularities within the S&P 500's ",
        "pricing data. This trend I would consider to be additive, at least mildly more additive ",
        "than what is seen in the NASDAQ data The first and most obvious one being March of 2020, which ",
        "essentially marks the first few weeks of the Coronavirus pandemic. Another ",
        "quite noticable irregularity is the change in trend in 2022. It is almost ",
        "entirely negative despite being mostly positive outside of 2022. There are a ",
        "large variety of factors that played into the S&P 500 having a down year in 2022, ",
        "this includes things such as rising inflation, heightened borrowing costs for ",
        "businesses and individuals, as well as fear in the markets from the war in ",
        "Ukraine. The S&P 500 suffered second most out of the S&P 500, NASDAQ and Russell 2000 ",
        "during this time period with a decline of -29.33% in value from the beginning of ",
        "the year to the end of the year. The S&P 500 is less focused on specifically tech ",
        "related stocks, and instead contains the 500 companies witht the largest market ",
        "capitilzation. This means that this decline is indicative of how the 500 largest ",
        "companies performed during 2022. The S&P 500 also maintains a similar weak cyclical ",
        "pattern like the NASDAQ around the end of fiscal quarters in March, June, September, and December",sep = "")
} else{
  paste("This timeseries plot of the Russell 2000 index has a general upward trend. It is very difficult ",
        "to describe this trend as either additive or multiplicative. I would say that generally, ",
        "it is additive, but the period between March 2020 and March 2021 is almost multiplicative. This trend ",
        "is weaker than both the S&P 500 and NASDAQ, but still noticable. The Russell 2000 shares ",
        "many of the same irregularities as both the S&P 500 and the NASDAQ. The Russell 2000 appears ",
        "to have suffered the worst initial decline in March of 2020. It also suffered for similar ",
        "reasons in 2022 as the S&P 500 and NASDAQ: rising inflation, increasing borrowing costs, ",
        "and of course the Russia Ukraine war. However, the Russell 2000 actually suffered the smallest ",
        "decline in 2022 compared to the S&P 500 and NASDAQ. However, unlike both of those indices, the ",
        "Russell 2000 has yet to regain the same price it had before the 2022 decline. Unlike the S&P 500 ",
        ", which tracks the 500 companies with the largest market capitalization, the Russell 2000 focuses ",
        "on 2000 companies with smaller market capitlization. The Russell 2000 has often performed better ",
        "than the S&P 500 historically during periods of economic uncertainty. There is not a very strong ",
        "cyclical nature to this data. However, it does seem to have a pattern where it peaks more at the ",
        "transition between fiscal years (around December and January) and a slight peak in the middle of ",
        "the year. This is slightly different from the pattern seen in the S&P 500 and NASDAQ, as the peaks ",
        "appear to be more closely linked to fiscal quarters with those indices.")
})

output$variedplotinterp <- renderText(if(input$Index_Selected == "NASDAQ Composite Index" &
                           input$select_plot == "Seasonality"){
  paste("This seasonality plot for the NASDAQ displays some interesting findings. First, ",
        "it definitely appears that this is a trend dominated data set. This is clear as ",
        "nearly every year on this plot has a clear upward trend, with the exception of ",
        "2022. For most years, the seasonality is quite mild. Additionally,", 
        "the seasonality within this plot is additive in nature. The amount of variation does ",
        " not increase as a function of time, rather changes in seasonality can be mostly read ",
        "as a result of irregularities, rather than a reoccuring seasonal aspect. As mentioned in the ",
        "in the interpretation of the overall timeseries plot, there are some mild peaks that ",
        "appear to occur near the end of every fiscal quarter. These are relatively minor ",
        "and generally do not overpower the trend during any specific period. A few areas of note ",
        "include the dip in price in March of 2020, as well as 2022 being the only year where ",
        "price ended lower than where it began.")
} else if(input$Index_Selected == "S&P 500 Index" &
          input$select_plot == "Seasonality"){
  paste("The seasonality plot for the S&P 500 displays similar findings to the NASDAQ. First, ",
        "it definitely appears that this is a trend dominated data set. This is clear as ",
        "nearly every year on this plot has an upward trend, with the exception of ",
        "2022. Seasonality for the S&P 500 is even more mild than the NASDAQ, with very low ",
        "variation in the peaks and valleys of the data. Additionally,", 
        "the seasonality within this plot is additive in nature. The amount of variation does ",
        " not increase as a function of time, rather changes in seasonality can be mostly read ",
        "as a result of irregularities, rather than a reoccuring seasonal aspect. As mentioned ",
        "in the interpretation of the overall timeseries plot, there are some mild peaks that ",
        "appear to occur near the end of every fiscal quarter. Again, these are relatively minor ",
        "and generally do not overpower the trend during any specific period. A few areas of note ",
        "include the dip in price in March of 2020, as well as 2022 being the only year where ",
        "price ended lower than where it began. The seasonality plot ",
        "for the S&P 500 shows a much more mild growth pattern than that seen in the NASDAQ.")
} else if(input$Index_Selected == "Russell 2000 Index" &
          input$select_plot == "Seasonality"){
  paste("The seasonality plot of the Russell 2000 is similar to the other 2 indices in many ways ",
        "This would include the dip in stock value seen in March of 2020, as well as the ",
        "trend reversing into a decline for 2022. However, it is apparent that the Russell 2000 ",
        "has a much more mild trend overall relative to the NASDAQ and S&P 500. Seasonality as ",
        "still relatively mild, with the seasonality being additive similar to the other indices.", 
        "Like the other indices, the variation in the seasonality does not appear to change with time",
        "The pattern does appear to be slightly different relative to ",
        "the NASDAQ and S&P 500. The price for the Russell 2000 seems to have a higher upward trend ",
        "December and January, as well as a pattern of peaking mildly toward the middle of the year. ",
        "This is different from the S&P 500 and NASDAQ as they seem to more uniformly peak with fiscal ",
        "quarters.")
} else if(input$Index_Selected == "NASDAQ Composite Index" &
          input$select_plot == "Autocorrelation"){
  paste("This autocorrelation graph for the NASDAQ Composite Index displays a couple of very clear ",
        "facts. The first is this data is heavily dominated by trend. We know this as the correlation ",
        "is the highest the closer it is to a lag of 0. The further the data gets from the present, the ",
        "worse of a predictor it is for the NASDAQ. The second fact that this displays, is that 2022 ",
        "was a bad year for the NASDAQ. This ACF allows a lag out to 108 weeks (2 years), and the ",
        "plotted lines that go below 0 almost exclusively occur after 52 weeks of lag.")
} else if(input$Index_Selected == "S&P 500 Index" &
          input$select_plot == "Autocorrelation"){
  paste("The autocorrelation graph for the S&P 500 Index displays a similar fact pattern ",
        "as the NASDAQ. The first is this data is heavily dominated by trend. We know this as the correlation ",
        "is the highest the closer it is to a lag of 0. The further the data gets from the present, the ",
        "worse of a predictor it is for the S&P 500. The S&P 500 appears to have recovered faster ",
        "than the NASDAQ, as the autocorrelation remains positive out to 490 days (or 70 weeks)")
} else if(input$Index_Selected == "Russell 2000 Index" &
         input$select_plot == "Autocorrelation"){
  paste("The autocorrelation graph for the Russell 2000 demonstrates that the price for the ",
        "Russell 2000 is trend dominated, similar to both the NASDAQ and S&P 500. This is ",
        "seen through correlation declining as the size of the lag increases. Additionally, ",
        "the Russell 2000 actually appears to have had the negative effects of the 2022 decline ",
        "last the longest out of all of the indices. We already suspected this given the fact that ",
        "the Russell 2000 has yet to return to its peak price prior to 2022.")
} else if(input$Index_Selected == "NASDAQ Composite Index" &
          input$select_plot == "Decomposition"){
  paste("The decomposition of the NASDAQ time series data allows us to get a more in depth ",
        "look at the individual aspects of the time series. The method of decomposition used ",
        "for this plot is STL, or 'Seasonal and Trend decomposition using Loess'. This method ",
        "of decomposition can handle all types of seasonality. This plot utilizes the 'robust' ",
        "feature which helps to avoid outliers from affecting the trend. Looking at the trend segment ",
        "of the decomposition we can see the general additive trend upwards, with a decline in 2022. ",
        "Looking at the seasonality segment, we see the peaks and valleys of the seasonality. For example, ",
        "we see the time around December and January seeing the largest spikes, with the months May and ",
        "October seemingly having consistent lows. There is also a secondary peak that occurs around July ",
        "and August. Looking at the 'remainder' or irregularities portion of the plot, we see the ",
        "major dip that occured in March of 2020. We also see a peak in late 2021, with the decline ",
        "throughout 2022.")
} else if(input$Index_Selected == "S&P 500 Index" &
          input$select_plot == "Decomposition"){
  paste("The decomposition of the S&P500 time series data allows us to get a more in depth ",
        "look at the individual aspects of the time series. The method of decomposition used ",
        "for this plot is STL, or 'Seasonal and Trend decomposition using Loess'. This method ",
        "of decomposition can handle all types of seasonality. This plot also utilizes the 'robust' ",
        "feature which helps to avoid outliers from affecting the trend. Looking at the trend segment ",
        "of the decomposition we can see the general additive trend upwards, with a decline in 2022. ",
        "Looking at the seasonality segment, we see the peaks and valleys of the seasonality. For example, ",
        "we see the time around fiscal quarter 4 (December & January) seeing the largest spikes, with the months May and ",
        "October seemingly having consistent lows. There is also a secondary peak that occurs around July ",
        "and August. Looking at the 'remainder' or irregularities portion of the plot, we see the ",
        "major dip that occured in March of 2020. We also see a peak in late 2021, with the decline ",
        "throughout 2022. Another note about the regularities seen in the S&P 500 index, relative to ",
        "the NASDAQ is that the irregularities are more minor in their nature. This is to be expected as ",
        "the NASDAQ is seen as the more volatile index between the two. Overall, the decomposition between ",
        "both the NASDAQ and S&P 500 look nearly identical other than some small variations in volatility.")
} else if(input$Index_Selected == "Russel 2000 Index" &
         input$select_plot == "Decomposition"){
  paste("The decomposition of the Russell 2000 index was performed via the same methods as the NASDAQ ",
  "and the S&P 500 using STL decomposition with the robust option. Looking at the trend segment of the ",
  "decomposition, it is apparent that during the period of early 2020 to late 2021 that the ",
  "trend was multiplicative in nature during that period. However, outside of that period the changes in ",
  "trend appear to be relatively additive by comparison. Looking at the seasonal segment, it appears ",
  "to peak in a more uniform pattern toward the very beginning of the year in the months of January and ",
  "February, not having any major spikes outside of that period. The lowest point each year according ",
  "to the seasonality segment is late September. The iregularity segment of the decomposition displays ",
  "that the Russell 2000 also experienced the negative effects of COVID in March of 2020.")
} else if(input$Index_Selected == "NASDAQ Composite Index" &
          input$select_plot == "Seasonal Naive"){
  paste("The seasonal naive model looks to combine a naive model with historical season effects to predict",
        " future values of the time series data. Essentially it takes the seasonal effects, and combines it",
        " with the most recent value in the data set. Out of the 4 models being tested, it performs the ",
        "worst when it comes to forecasting the NASDAQ according to RMSE and MAPE. The seasonal naive ",
        "model has an RMSE value of 3640, which means that on average we can expect the model to miss ",
        "by 3640 index points. For reference, index points are the weighted average value of the components ",
        "of the index. The model has a MAPE value of 23.9%, which means that on average the model will be ",
        "off by 23.9% of the true value.")
} else if(input$Index_Selected == "NASDAQ Composite Index" &
          input$select_plot == "TSLM"){
  paste("The time series linear model (TSLM) is a model that essentially applies traditional linear regression ",
        "to time series data. The model can be used along with exogenous variables to make predictions. This ",
        "means that if we knew the market data were strongly correlated with oil prices, we could use oil prices ",
        "to aid in the prediction of our data. However, in order to forecast with an exogenous variable such as ",
        "oil, you would need to know the future price of oil for the period you want to forecast. Since no one ",
        "knows the exact future price of oil this may seem a bit useless. However, this functionality of the TSLM ",
        "model makes it a good tool for scenario forecasting. In essence, you could make a model for how the stock ",
        "market would be effected if oil prices rise, a seperate one for them remaining the same, and a seperate one ",
        "for them falling. However, for this more simple time series linear model, I simply used historical trend ",
        "and seasonality to forecast my values. This model performed quite well forecasting a holdout data set for ",
        "the NASDAQ. It had the second lowest RMSE and second lowest MAPE values. It had an RMSE value of 833, which ",
        "means that on average this model misses by about 833 composite index points. For reference, index points",
        "are the weighted average value of the components of the index. The model also had a MAPE value of 5.3%. ",
        "This means that on average the model only misses by about 5.3%.")
} else if(input$Index_Selected == "NASDAQ Composite Index" &
          input$select_plot == "ARIMA"){
  paste("The ARIMA model is a very powerful forecasting methodology. Broken out the name means autoregression ",
        "integrated moving average. Essentially it combines three different techniques in order to predict ",
        "future values. The moving average (MA) model uses lagged errors, which essentially are the distances ",
        "of data points from the average value of the dataset. The autoregression (AR) model uses a similar ",
        "methodology as the MA model. Instead of using lagged errors, it uses lagged values from partial ",
        "autocorrelation. Partial autocorrelation helps to eliminate redundant information that is often ",
        "remains from using normal autocorrelation. Finally, integration helps to make the data in the model ",
        "stationary, which means the data will have a constant mean, constant variance, and doesn't have ",
        "seasonality. The ARIMA model can also apply all of these methods in a seasonal fashion. However, for ",
        "this model, these seasonal components are all essentially not used. This ARIMA model is actually very ",
        "simple. In this model, it does not use autoregression or moving average, but it does integrate the data. ",
        "It additionally uses drift to help capture trend. The ARIMA model performed third best out of all of ",
        "these models. It had an RMSE value of 1754, which means that on average the model will miss by 1754 ",
        "composite index points. For reference, index points are the weighted average value of the components",
        " of the index. It also had a MAPE value of 9.66%, which means that on average the model will be off ",
        "by about 9.66%.")
} else if(input$Index_Selected == "NASDAQ Composite Index" &
          input$select_plot == "NNETAR"){
  paste("(BEST MODEL FOR NASDAQ) The NNETAR model is a model that uses neural networks and autoregression to help predict future values. ",
        "A neural network is kind of like a series of regressions combined together using different weights. ",
        "These regressions get combined at multiple different nodes in a hidden layer. These nodes are called ",
        "hidden nodes. After multiple hidden layers, these values are combined into one final output node. The ",
        "previously mentioned weights begin as random, and are inched towards a value with the highest accuracy ",
        "through multiple iterations of the model. For this model in particular, it inches toward the right model ",
        "by itterating 100 times. This is the model with the highest accuracy, and as such this is the model that ",
        "I would recommend using when it comes to forecasting the NASDAQ composite index. It had an RMSE value of ",
        "only 833. This means that average prediction that the model makes is 833 composite index points. ",
        "For reference, index points are the weighted average value of the components of the index. It also had a ",
        "MAPE value of 4.22%. ",
        "This means that the average prediction that the model makes will be off by 4.22%.")
} else if(input$Index_Selected == "S&P 500 Index" &
          input$select_plot == "Seasonal Naive"){
  paste("The seasonal naive model looks to combine a naive model with historical season effects to predict",
        " future values of the time series data. Essentially it takes the seasonal effects, and combines it",
        " with the most recent value in the data set. Out of the 4 models being tested, it performs the ",
        "worst when it comes to forecasting the S&P 500 according to RMSE and MAPE. The seasonal naive ",
        "model has an RMSE value of 795, which means that on average we can expect the model to miss ",
        "by 795 index points. For reference, index points are the weighted average value of the components ",
        "of the index. The model has a MAPE value of 16.0%, which means that on average the model will be ",
        "off by 16.0% of the true value.")
} else if(input$Index_Selected == "S&P 500 Index" &
          input$select_plot == "TSLM"){
  paste("(BEST MODEL FOR S&P 500)The time series linear model (TSLM) is a model that essentially applies traditional linear regression ",
        "to time series data. The model can be used along with exogenous variables to make predictions. This ",
        "means that if we knew the market data were strongly correlated with oil prices, we could use oil prices ",
        "to aid in the prediction of our data. However, in order to forecast with an exogenous variable such as ",
        "oil, you would need to know the future price of oil for the period you want to forecast. Since no one ",
        "knows the exact future price of oil this may seem a bit useless. However, this functionality of the TSLM ",
        "model makes it a good tool for scenario forecasting. In essence, you could make a model for how the stock ",
        "market would be effected if oil prices rise, a seperate one for them remaining the same, and a seperate one ",
        "for them falling. However, for this more simple time series linear model, I simply used historical trend ",
        "and seasonality to forecast my values. This model performed the best at forecasting a holdout data set for ",
        "the S&P 500. It had the lowest RMSE and lowest MAPE values. It had an RMSE value of 261, which ",
        "means that on average this model misses by about 261 composite index points. For reference, index points",
        "are the weighted average value of the components of the index. The model also had a MAPE value of 4.67%. ",
        "This means that on average the model only misses by about 4.67%. It is because this model performs so well ",
        "on the holdout data that I would recommend using this model to predict S&P 500 data. It also has the benefit ",
        "of being relatively simple to interpret relative to the NNETAR and ARIMA models.")
} else if(input$Index_Selected == "S&P 500 Index" &
          input$select_plot == "ARIMA"){
  paste("The ARIMA model is a very powerful forecasting methodology. Broken out the name means autoregression ",
        "integrated moving average. Essentially it combines three different techniques in order to predict ",
        "future values. The moving average (MA) model uses lagged errors, which essentially are the distances ",
        "of data points from the average value of the dataset. The autoregression (AR) model uses a similar ",
        "methodology as the MA model. Instead of using lagged errors, it uses lagged values from partial ",
        "autocorrelation. Partial autocorrelation helps to eliminate redundant information that is often ",
        "remains from using normal autocorrelation. Finally, integration helps to make the data in the model ",
        "stationary, which means the data will have a constant mean, constant variance, and doesn't have ",
        "seasonality. The ARIMA model can also apply all of these methods in a seasonal fashion. The model ",
        "uses 2 lags for autoregression, and 1 lags for the moving average model. The model is also integrated 1 ",
        "time. The model also uses 1 seasonal autoregression lag. It also uses drift to help capture trend.",
        " The ARIMA model performed second best out of all of ",
        "these models. It had an RMSE value of 461, which means that on average the model will miss by ",
        "461 index points. For reference, index points are the weighted average value of the components",
        " of the index. It also had a MAPE value of 7.82%, which means that on average the model will be off ",
        "by about 7.82%.")
} else if(input$Index_Selected == "S&P 500 Index" &
          input$select_plot == "NNETAR"){
  paste("The NNETAR model is a model that uses neural networks and autoregression to help predict future values. ",
        "A neural network is kind of like a series of regressions combined together using different weights. ",
        "These regressions get combined at multiple different nodes in a hidden layer. These nodes are called ",
        "hidden nodes. After multiple hidden layers, these values are combined into one final output node. The ",
        "previously mentioned weights begin as random, and are inched towards a value with the highest accuracy ",
        "through multiple iterations of the model. For this model in particular, it inches toward the right model ",
        "by itterating 100 times. This is the model with the third highest accuracy. ",
        "It had an RMSE value of ",
        "498. This means that average prediction that the model makes is 498 composite index points.",
        "For reference, index points are the weighted average value of the components of the index. ",
        "It also had a MAPE value of 8.52%. ",
        "This means that the average prediction that the model makes will be off by 8.52%.")
} else if(input$Index_Selected == "Russell 2000 Index" &
          input$select_plot == "Seasonal Naive"){
  paste("The seasonal naive model looks to combine a naive model with historical season effects to predict",
        " future values of the time series data. Essentially it takes the seasonal effects, and combines it",
        " with the most recent value in the data set. Out of the 4 models being tested, it performs the ",
        "second best when it comes to forecasting the Russell 2000 according to RMSE and MAPE. The seasonal naive ",
        "model has an RMSE value of 118, which means that on average we can expect the model to miss ",
        "by 118 index points. For reference, index points are the weighted average value of the components ",
        "of the index. The model has a MAPE value of 6.17%, which means that on average the model will be ",
        "off by 6.17% of the true value.")
} else if(input$Index_Selected == "Russell 2000 Index" &
          input$select_plot == "TSLM"){
  paste("The time series linear model (TSLM) is a model that essentially applies traditional linear regression ",
        "to time series data. The model can be used along with exogenous variables to make predictions. This ",
        "means that if we knew the market data were strongly correlated with oil prices, we could use oil prices ",
        "to aid in the prediction of our data. However, in order to forecast with an exogenous variable such as ",
        "oil, you would need to know the future price of oil for the period you want to forecast. Since no one ",
        "knows the exact future price of oil this may seem a bit useless. However, this functionality of the TSLM ",
        "model makes it a good tool for scenario forecasting. In essence, you could make a model for how the stock ",
        "market would be effected if oil prices rise, a seperate one for them remaining the same, and a seperate one ",
        "for them falling. However, for this more simple time series linear model, I simply used historical trend ",
        "and seasonality to forecast my values. This model performed the best at forecasting a holdout data set for ",
        "the Rusell 2000. It had the highest (worst) RMSE and highest MAPE values. It had an RMSE value of 275, which ",
        "means that on average this model misses by about 275 composite index points. For reference, index points",
        "are the weighted average value of the components of the index. The model also had a MAPE value of 13.7%. ",
        "This means that on average the model misses by about 13.7%.")
} else if(input$Index_Selected == "Russell 2000 Index" &
          input$select_plot == "ARIMA"){
  paste("The ARIMA model is a very powerful forecasting methodology. Broken out the name means autoregression ",
        "integrated moving average. Essentially it combines three different techniques in order to predict ",
        "future values. The moving average (MA) model uses lagged errors, which essentially are the distances ",
        "of data points from the average value of the dataset. The autoregression (AR) model uses a similar ",
        "methodology as the MA model. Instead of using lagged errors, it uses lagged values from partial ",
        "autocorrelation. Partial autocorrelation helps to eliminate redundant information that is often ",
        "remains from using normal autocorrelation. Finally, integration helps to make the data in the model ",
        "stationary, which means the data will have a constant mean, constant variance, and doesn't have ",
        "seasonality. The ARIMA model can also apply all of these methods in a seasonal fashion. For this model ",
        "it uses 2 lags for autoregression, and 2 lags for the moving average model. The model is also integrated 1 ",
        "time. The model also uses 1 seasonal autoregression lag. It also uses drift to help capture trend. The ",
        "ARIMA model performed third best of all of ",
        "these models. It had an RMSE value of 154, which means that on average the model will miss by ",
        "154 index points. For reference, index points are the weighted average value of the components",
        " of the index. It also had a MAPE value of 7.87%, which means that on average the model will be off ",
        "by about 7.87%.")
} else if(input$Index_Selected == "Russell 2000 Index" &
          input$select_plot == "NNETAR"){
  paste("(BEST MODEL FOR RUSSELL 2000) The NNETAR model is a model that uses neural networks and autoregression to help predict future values. ",
        "A neural network is kind of like a series of regressions combined together using different weights. ",
        "These regressions get combined at multiple different nodes in a hidden layer. These nodes are called ",
        "hidden nodes. After multiple hidden layers, these values are combined into one final output node. The ",
        "previously mentioned weights begin as random, and are inched towards a value with the highest accuracy ",
        "through multiple iterations of the model. For this model in particular, it inches toward the right model ",
        "by itterating 100 times. This is the model with the highest accuracy, and as such this is the model that ",
        "I would recommend using when it comes to forecasting the Russell 2000 index. It had an RMSE value of ",
        "only 86.3. This means that average prediction that the model makes is 86.3 composite index points. ",
        "For reference, index points are the weighted average value of the components of the index. It also had a ",
        "MAPE value of 3.76%. ",
        "This means that the average prediction that the model makes will be off by 3.76%.")
}
  )

  }


shinyApp(ui,server)
