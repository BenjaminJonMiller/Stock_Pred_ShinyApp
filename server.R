library(shiny)
library(ISLR)
library(MASS)
library(class)
data(Weekly)
attach(Weekly)
train <- (Weekly$Year<2010)

shinyServer(function(input, output){
    output$model_input <- renderText({expr = input$model_input})
    output$variables <- renderText({expr = input$variables})
    output$threshold <- renderText({expr = input$threshold})
    output$k <- renderText({expr = input$k})
    output$pairs <- renderPlot({
        predictors <- input$variables
        pairs(subset(Weekly, select = c(predictors, 'Direction')),
              pch='.', col=Weekly$Direction)
    })
    
    # model summary
    output$summary <- renderPrint({
        # setup dataframe with data from selected predictors
        df <- data.frame('Direction'=Weekly$Direction)
        for (i in 1:length(input$variables)){
            df <- cbind(df, eval(parse(text=input$variables[i])))
        }
        
        # set names of columns
        names(df) <- c('Direction', input$variables)
        
        # set model based on selected model type
        if (input$model_input == 'Logistic Regression'){
            fit <- glm(Direction ~ ., data=df, family='binomial', subset = train)
            print(summary(fit))
        }
        if (input$model_input == 'LDA'){
            fit <- lda(Direction ~ ., data=df, subset = train)
            print(fit)
        }
        if (input$model_input == 'QDA'){
            fit <- qda(Direction ~ ., data=df, subset = train)
            print(fit)
        }
        if (input$model_input == 'KNN'){
            print('Model Summary Not Applicable.  See Results')
        }
    })
    
    # model prediction results
    output$results <- renderPrint({
        df <- data.frame('Direction'=Weekly$Direction)
        for (i in 1:length(input$variables)){
            #         print(i)
            df <- cbind(df, eval(parse(text=input$variables[i])))
        }
        names(df) <- c('Direction', input$variables)
        
        # set model based on selected model type
        if (input$model_input == 'Logistic Regression'){
            fit <- glm(Direction ~ ., data=df, family='binomial', subset = train)
            probs <- predict(fit, newdata = df[!train,], type='response')
            predictions <- rep('Down', nrow(df[!train,]))
            predictions[probs>input$threshold] <- 'Up'
            tabulated <- table(predictions, df[!train,]$Direction)
            accuracy <- (tabulated[1,1] + tabulated[2,2]) / nrow(df[!train,])
            print(tabulated)
            print(paste('accuracy = ', round(x = accuracy, digits = 3)))
        }
        if (input$model_input == 'LDA'){
            fit <- lda(Direction ~ ., data=df, subset = train)
            class <- predict(fit, newdata = df[!train,], type='response')$class
            tabulated <- table(class, df[!train,]$Direction)
            accuracy <- (tabulated[1,1] + tabulated[2,2]) / nrow(df[!train,])
            print(tabulated)
            print(paste('accuracy = ', round(x = accuracy, digits = 3)))
        }
        if (input$model_input == 'QDA'){
            fit <- qda(Direction ~ ., data=df, subset = train)
            class <- predict(fit, newdata = df[!train,], type='response')$class
            tabulated <- table(class, df[!train,]$Direction)
            accuracy <- (tabulated[1,1] + tabulated[2,2]) / nrow(df[!train,])
            print(tabulated)
            print(paste('accuracy = ', round(x = accuracy, digits = 3)))
        }
        if (input$model_input == 'KNN'){
            train.X <- df[train, 2:length(df)]
            test.X <- df[!train, 2:length(df)]
            class <- knn(train.X, test.X, df[train,]$Direction, k=input$k)
            tabulated <- table(class, df[!train,]$Direction)
            accuracy <- (tabulated[1,1] + tabulated[2,2]) / nrow(df[!train,])
            print(tabulated)
            print(paste('accuracy = ', round(x = accuracy, digits = 3)))            
        }

    })
}
)
    
