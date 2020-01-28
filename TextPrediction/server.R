library(shiny)

initialPrediction <- readRDS("start-word-prediction2.RData")

freq4ngram <- readRDS("quadram4.RData")
freq3ngram <- readRDS("trigram.RData")
freq2ngram <-readRDS ("bigram.RData")

predictionMatch <- function(userInput, ngrams) {
        if (ngrams > 3) {
        userInput3 <- paste(userInput[length(userInput) - 2],
                            userInput[length(userInput) - 1],
                            userInput[length(userInput)])
        dataTokens <- freq4ngram %>% filter(token == userInput3)
        if (nrow(dataTokens) >= 1) {
            return(dataTokens$outcome[1:3])
        }
        return(predictionMatch(userInput, ngrams - 1))
    }
    
    if (ngrams == 3) {
        userInput1 <- paste(userInput[length(userInput)-1], userInput[length(userInput)])
        dataTokens <- freq3ngram %>% filter(token == userInput1)
        if (nrow(dataTokens) >= 1) {
            return(dataTokens$outcome[1:3])
        }
        return(predictionMatch(userInput, ngrams - 1))
    }
    
    if (ngrams < 3) {
        userInput1 <- userInput[length(userInput)]
        dataTokens <- freq2ngram %>% filter(token == userInput1)
        return(dataTokens$outcome[1:3])
    }
    
    return(NA)
}

cleanInput <- function(input) {
    
    
    if (input == "" | is.na(input)) {
        return("")
    }
    
    input <- tolower(input)
    
    input <- gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", input, ignore.case = FALSE, perl = TRUE)
    input <- gsub("\\S+[@]\\S+", "", input, ignore.case = FALSE, perl = TRUE)
    input <- gsub("@[^\\s]+", "", input, ignore.case = FALSE, perl = TRUE)
    input <- gsub("#[^\\s]+", "", input, ignore.case = FALSE, perl = TRUE)
    
    input <- gsub("[0-9](?:st|nd|rd|th)", "", input, ignore.case = FALSE, perl = TRUE)
    
    
    input <- gsub("[^\\p{L}'\\s]+", "", input, ignore.case = FALSE, perl = TRUE)
    

    input <- gsub("[.\\-!]", " ", input, ignore.case = FALSE, perl = TRUE)
    
    input <- gsub("^\\s+|\\s+$", "", input)
    input <- stripWhitespace(input)
    

    if (input == "" | is.na(input)) {
        return("")
    }
    
    input <- unlist(strsplit(input, " "))
    
    return(input)
    
}

predictNextWord <- function(input, word = 0) {
    
    input <- cleanInput(input)
    
    if (input[1] == "") {
        output <- initialPrediction
    } else if (length(input) == 1) {
        output <- predictionMatch(input, ngrams = 2)
    } else if (length(input) == 2) {
        output <- predictionMatch(input, ngrams = 3)
    } else if (length(input) > 2) {
        output <- predictionMatch(input, ngrams = 4)
    }
    
    if (word == 0) {
        return(output)
    } else if (word == 1) {
        return(output[1])
    } else if (word == 2) {
        return(output[2])
    } else if (word == 3) {
        return(output[3])
    }
    
}

shinyServer(function(input, output) {
    
    output$userSentence <- renderText({input$userInput});
    
    observe({
        numPredictions <- input$numPredictions
        if (numPredictions == 1) {
            output$prediction1 <- reactive({predictNextWord(input$userInput, 1)})
            output$prediction2 <- NULL
            output$prediction3 <- NULL
        } else if (numPredictions == 2) {
            output$prediction1 <- reactive({predictNextWord(input$userInput, 1)})
            output$prediction2 <- reactive({predictNextWord(input$userInput, 2)})
            output$prediction3 <- NULL
        } else if (numPredictions == 3) {
            output$prediction1 <- reactive({predictNextWord(input$userInput, 1)})
            output$prediction2 <- reactive({predictNextWord(input$userInput, 2)})
            output$prediction3 <- reactive({predictNextWord(input$userInput, 3)})
        }
    })
    
})

    