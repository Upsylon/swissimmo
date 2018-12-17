#' @title Websrcapping of Immoscout24.ch data by city
#' @description This function enables to collect the data about the location market
#' from Immoscout24.ch. For given cities, on can retrieve the housings available for renting.
#' @param city_vector A vector containing the name of the different cities
#' @return A dataframe containing the number of rooms, m2, price, address,
#' and city of the different accomodations available. Ignores housings with prices
#' less than 300 and higher than 10000 and containing missing data.
#' @author Germano David
#' @author Lomazzi Vincent
#' @author Bron Luca
#' @author Raisin Edgar
#' @author Grandadam Patrik
#' @importFrom magrittr %>%
#' @export
#' @examples
#' get_immodata(c("lugano", "lausanne"))

get_immodata <- function(city_vector) {

  ### Getting all the cities in the form of a "list of lists"
  cities <- list()

  for (i in 1:length(city_vector)) {
    cities[[i]] <- list()
    names(cities)[i] = paste(city_vector[i])
  }

  ### Getting the URL of each cities in new variables linked to each of them:
  for (i in 1:length(cities)){
    attr(cities[[i]], which = "url") <- paste(
      "https://www.immoscout24.ch/en/real-estate/rent/city-",
      names(cities[i]), sep = "")
  }

  ### Getting the number of pages for each cities
  pages <- list()
  for( i in 1:length(cities)){
    if (xml2::read_html(x = paste0(unlist(attributes(cities[[i]]),
                                          use.names = FALSE))) %>%
        rvest::html_nodes(css = ".fsjvuy") %>% # the csv corresponding to pages
        rvest::html_text() %>% as.numeric %>% length() != 0) {
    pages[[i]] <- xml2::read_html(x = paste0(unlist(attributes(cities[[i]]),
                                                    use.names = FALSE))) %>%
      rvest::html_nodes(css = ".fsjvuy") %>% # the csv corresponding to pages
      rvest::html_text() %>%
      as.numeric() %>%
      max(na.rm = TRUE) %>%
      seq(from = 1)
    } else {pages[[i]] <- 1}
  }

  ### Scrapping everything
  for (i in 1:length(cities)){
    for (page in pages[[i]]){
      url_path_page_immoscout <- cities[[i]] %>%
        attributes %>%
        unlist(use.names = FALSE) %>%
        paste0 %>%
        paste("?pn=", page, sep = "")

      cities[[i]][[page]] <- list()
      cities[[i]][[page]] <- xml2::read_html(url_path_page_immoscout) %>%
        rvest::html_nodes(".dTICXP") %>% # the csv corresponding to the big block
        rvest::html_text()
    }
  }

  # unlisting
  unlisted_cities <- unlist(cities) %>% data.frame

  all_cities  <- data.frame()
  for (i in 1:length(cities)) {
    item_full_info <- unlisted_cities[grep(names(cities[i]),
                                           rownames(unlisted_cities)),]
    #extract number of rooms


    assign(paste("df_", names(cities[i]), sep = ""),
           data.frame(
             rooms = stringr::str_extract(item_full_info, ".*m\u00B2") %>%
               # first taking before m2 for the cases where the word "room" or "rooms"
               # is mentionned in the description
               stringr::str_extract(., ".*rooms*") %>%
               gsub(pattern = " rooms", replacement = "", fixed = TRUE) %>%
               gsub(pattern = " room", replacement = "", fixed = TRUE) %>%
               as.numeric
             ,

             # Extract Size
             m2 = stringr::str_extract(item_full_info, ".*m\u00B2\u00AB") %>%
               stringr::str_extract(., ", .*") %>%
               gsub(pattern=" m\u00B2\u00AB", replacement = "", fixed = TRUE) %>%
               gsub(pattern= ", ", replacement = "", fixed = TRUE)
             %>% as.integer
             ,

             # Extract localiation
             address =  stringr::str_extract(item_full_info, ".*Close") %>%
               stringr::str_extract(., ".*,") %>%
               stringr::str_extract(., "\u00bb.*") %>%
               gsub(pattern = "Close", replacement = "", fixed = TRUE) %>%
               gsub(pattern = ",", replacement = "", fixed = TRUE) %>%
               gsub(pattern = "\u00bb", replacement = "", fixed = TRUE) %>%
               gsub(pattern = "\u00FC", replacement = "u", fixed = TRUE) %>%
               gsub(pattern = "\u00E4", replacement = "a", fixed = TRUE) %>%
               gsub(pattern = "\u00F6", replacement = "o", fixed = TRUE) %>%
               gsub(pattern = "\u00E8", replacement = "e", fixed = TRUE) %>%
               gsub(pattern = "\u00E9", replacement = "e", fixed = TRUE) %>%
               gsub(pattern = "\u00EA", replacement = "e", fixed = TRUE) %>%
               gsub(pattern = "\u00F4", replacement = "o", fixed = TRUE) %>%
               gsub(pattern = "\u00EF", replacement = "i", fixed = TRUE)
             ,

             #extract price
             price = stringr::str_extract(item_full_info, "eCHF .*") %>%
               stringr::str_extract(., ".*.\u2014 *") %>%
               gsub(pattern = "eCHF ", replacement = "", fixed = TRUE) %>%
               gsub(pattern = ".\u2014", replacement = "", fixed = TRUE) %>%
               gsub(pattern = ",", replacement = "", fixed = TRUE) %>% as.integer
             ,

             # Assign the city
             city = names(cities[i])
           )
    )

    city <-  data.frame(
      rooms = stringr::str_extract(item_full_info, ".*\u00AB") %>%
        # first taking before m2 for the cases where the word "room" or "rooms"
        # is mentionned in the description
        stringr::str_extract(., ".*room") %>%
        gsub(pattern = " room", replacement = "", fixed = TRUE) %>%
        as.numeric
      ,

      # Extract Size
      m2 = stringr::str_extract(item_full_info, ".*m\u00B2\u00AB") %>%
        # stringr::str_extract(., ", .*") %>%
        gsub(pattern=" m\u00B2\u00AB", replacement = "", fixed = TRUE) %>%
        stringr::word(.,-1) %>%
        as.integer
      ,

      #extract price
      price = stringr::str_extract(item_full_info, "eCHF .*") %>%
        stringr::str_extract(., ".*.\u2014 *") %>%
        gsub(pattern = "eCHF ", replacement = "", fixed = TRUE) %>%
        gsub(pattern = ".\u2014", replacement = "", fixed = TRUE) %>%
        gsub(pattern = ",", replacement = "", fixed = TRUE) %>%
        as.integer
      ,

      # Extract localiation
      address =  stringr::str_extract(item_full_info, ".*Close") %>%
        stringr::str_extract(., ".*,") %>%
        stringr::str_extract(., "\u00bb.*") %>%
        gsub(pattern = "Close", replacement = "", fixed = TRUE) %>%
        gsub(pattern = ",", replacement = "", fixed = TRUE) %>%
        gsub(pattern = "\u00bb", replacement = "", fixed = TRUE) %>%
        gsub(pattern = "\u00FC", replacement = "u", fixed = TRUE) %>%
        gsub(pattern = "\u00E4", replacement = "a", fixed = TRUE) %>%
        gsub(pattern = "\u00F6", replacement = "o", fixed = TRUE) %>%
        gsub(pattern = "\u00E8", replacement = "e", fixed = TRUE) %>%
        gsub(pattern = "\u00E9", replacement = "e", fixed = TRUE) %>%
        gsub(pattern = "\u00EA", replacement = "e", fixed = TRUE) %>%
        gsub(pattern = "\u00F4", replacement = "o", fixed = TRUE) %>%
        gsub(pattern = "\u00EF", replacement = "i", fixed = TRUE) %>%
        gsub(pattern = "\u00EB", replacement = "e", fixed = TRUE) %>%
        gsub(pattern = "\u00EE", replacement = "e", fixed = TRUE) %>%
        gsub(pattern = "\u00E7", replacement = "c", fixed = TRUE) %>%
        gsub(pattern = "\u00E2", replacement = "a", fixed = TRUE)
      ,

      city = stringr::str_extract(item_full_info, ".*Close") %>%
        stringr::str_extract(., ".*,") %>%
        stringr::str_extract(., "\u00bb.*") %>%
        gsub(pattern = "Close", replacement = "", fixed = TRUE) %>%
        gsub(pattern = ",", replacement = "", fixed = TRUE) %>%
        gsub(pattern = "\u00bb", replacement = "", fixed = TRUE) %>%
        gsub(pattern = "\u00FC", replacement = "u", fixed = TRUE) %>%
        gsub(pattern = "\u00E8", replacement = "e", fixed = TRUE) %>%
        stringr::word(., -1)
    )
    all_cities <- rbind(all_cities, city)
  }

  # selecting only prices > 500 to get rid off most of wrong recorder data
  # and weakly rents
  all_cities <- all_cities[all_cities$price > 300,]
  all_cities <- all_cities[all_cities$price < 10000,]


  # Deleting rows where we have NA (about 16% of the data)
  all_cities <- all_cities[complete.cases(all_cities),]

  return(all_cities)
}


#' @title Websrcapping of Immoscout24.ch data by city adding coordinates and predicted
#' price
#' @description This function enables to collect the data about the location market
#' from Immoscout24.ch. For given cities, on can retrieve the housings available
#' for renting.
#' It adds the coordinates of the addresses and the predicted price of a good.
#' Note that this function requires an API code from Google and which is costly.
#' Therefore prefere to use the simple get_immodata function to avoid extraordinary costs.
#' @param city_vector A vector containing the name of the different cities
#' @return A dataframe containing the number of rooms, m2, price, address,
#' city, longitude and latitude of the different accomodations available and their
#' estimated price. Ignores housings with prices less than 300 and higher than 10000
#' and containing missing data.
#' @author Germano David
#' @author Lomazzi Vincent
#' @author Bron Luca
#' @author Raisin Edgar
#' @author Grandadam Patrik
#' @importFrom magrittr %>%
#' @export
#' @examples
#' get_immodata2(c("bussigny"))

get_immodata2 <- function(city_vector) {

  ### Getting all the cities in the form of a "list of lists"
  cities <- list()

  for (i in 1:length(city_vector)) {
    cities[[i]] <- list()
    names(cities)[i] = paste(city_vector[i])
  }

  ### Getting the URL of each cities in new variables linked to each of them:
  for (i in 1:length(cities)){
    attr(cities[[i]], which="url") <- paste(
      "https://www.immoscout24.ch/en/real-estate/rent/city-",
      names(cities[i]), sep = "")
  }

  ### Getting the number of pages for each cities
  pages <- list()
  for( i in 1:length(cities)){
    if (xml2::read_html(x = paste0(unlist(attributes(cities[[i]]),
                                          use.names = FALSE))) %>%
        rvest::html_nodes(css = ".fsjvuy") %>% # the csv corresponding to pages
        rvest::html_text() %>% as.numeric %>% length() != 0) {
      pages[[i]] <- xml2::read_html(x = paste0(unlist(attributes(cities[[i]]),
                                                      use.names = FALSE))) %>%
        rvest::html_nodes(css = ".fsjvuy") %>% # the csv corresponding to pages
        rvest::html_text() %>%
        as.numeric() %>%
        max(na.rm = TRUE) %>%
        seq(from = 1)
    } else {pages[[i]] <- 1}
  }

  ### Scrapping everything
  for (i in 1:length(cities)){
    for (page in pages[[i]]){
      url_path_page_immoscout <- cities[[i]] %>%
        attributes %>%
        unlist(use.names = FALSE) %>%
        paste0 %>%
        paste("?pn=", page, sep = "")

      cities[[i]][[page]] <- list()
      cities[[i]][[page]] <- xml2::read_html(url_path_page_immoscout) %>%
        rvest::html_nodes(".dTICXP") %>%
        rvest::html_text()
    }
  }

  # unlisting
  unlisted_cities <- unlist(cities) %>% data.frame

  all_cities  <- data.frame()
  for (i in 1:length(cities)) {
    item_full_info <- unlisted_cities[grep(names(cities[i]),
                                           rownames(unlisted_cities)),]
    #extract number of rooms


    assign(paste("df_", names(cities[i]), sep=""),
           data.frame(
             rooms = stringr::str_extract(item_full_info, ".*m\u00B2") %>%
               # first taking before m2 for the cases where the word "room" or "rooms"
               # is mentionned in the description
               stringr::str_extract(., ".*rooms*") %>%
               gsub(pattern = " rooms", replacement = "", fixed = TRUE) %>%
               gsub(pattern = " room", replacement = "", fixed = TRUE) %>%
               as.numeric
             ,

             # Extract Size
             m2 = stringr::str_extract(item_full_info, ".*m\u00B2\u00AB") %>%
               stringr::str_extract(., ", .*") %>%
               gsub(pattern=" m\u00B2\u00AB", replacement = "", fixed = TRUE) %>%
               gsub(pattern= ", ", replacement = "", fixed = TRUE)
             %>% as.integer
             ,

             # Extract localiation
             address =  stringr::str_extract(item_full_info, ".*Close") %>%
               stringr::str_extract(., ".*,") %>%
               stringr::str_extract(., "\u00bb.*") %>%
               gsub(pattern = "Close", replacement = "", fixed = TRUE) %>%
               gsub(pattern = ",", replacement = "", fixed = TRUE) %>%
               gsub(pattern = "\u00bb", replacement = "", fixed = TRUE) %>%
               gsub(pattern = "\u00FC", replacement = "u", fixed = TRUE) %>%
               gsub(pattern = "\u00E4", replacement = "a", fixed = TRUE) %>%
               gsub(pattern = "\u00F6", replacement = "o", fixed = TRUE) %>%
               gsub(pattern = "\u00E8", replacement = "e", fixed = TRUE) %>%
               gsub(pattern = "\u00E9", replacement = "e", fixed = TRUE) %>%
               gsub(pattern = "\u00EA", replacement = "e", fixed = TRUE) %>%
               gsub(pattern = "\u00F4", replacement = "o", fixed = TRUE) %>%
               gsub(pattern = "\u00EF", replacement = "i", fixed = TRUE)
             ,

             #extract price
             price = stringr::str_extract(item_full_info, "eCHF .*") %>%
               stringr::str_extract(., ".*.\u2014 *") %>%
               gsub(pattern = "eCHF ", replacement = "", fixed = TRUE) %>%
               gsub(pattern = ".\u2014", replacement = "", fixed = TRUE) %>%
               gsub(pattern = ",", replacement = "", fixed = TRUE) %>% as.integer
             ,

             # Assign the city
             city = names(cities[i])
           )
    )

    city <-  data.frame(
      rooms = stringr::str_extract(item_full_info, ".*\u00AB") %>%
        # first taking before m2 for the cases where the word "room" or "rooms"
        # is mentionned in the description
        stringr::str_extract(., ".*room") %>%
        gsub(pattern = " room", replacement = "", fixed = TRUE) %>%
        as.numeric
      ,

      # Extract Size
      m2 = stringr::str_extract(item_full_info, ".*m\u00B2\u00AB") %>%
        # stringr::str_extract(., ", .*") %>%
        gsub(pattern=" m\u00B2\u00AB", replacement = "", fixed = TRUE) %>%
        stringr::word(.,-1) %>%
        as.integer
      ,

      #extract price
      price = stringr::str_extract(item_full_info, "eCHF .*") %>%
        stringr::str_extract(., ".*.\u2014 *") %>%
        gsub(pattern = "eCHF ", replacement = "", fixed = TRUE) %>%
        gsub(pattern = ".\u2014", replacement = "", fixed = TRUE) %>%
        gsub(pattern = ",", replacement = "", fixed = TRUE) %>%
        as.integer
      ,

      # Extract localiation
      address =  stringr::str_extract(item_full_info, ".*Close") %>%
        stringr::str_extract(., ".*,") %>%
        stringr::str_extract(., "\u00bb.*") %>%
        gsub(pattern = "Close", replacement = "", fixed = TRUE) %>%
        gsub(pattern = ",", replacement = "", fixed = TRUE) %>%
        gsub(pattern = "\u00bb", replacement = "", fixed = TRUE) %>%
        gsub(pattern = "\u00FC", replacement = "u", fixed = TRUE) %>%
        gsub(pattern = "\u00E4", replacement = "a", fixed = TRUE) %>%
        gsub(pattern = "\u00F6", replacement = "o", fixed = TRUE) %>%
        gsub(pattern = "\u00E8", replacement = "e", fixed = TRUE) %>%
        gsub(pattern = "\u00E9", replacement = "e", fixed = TRUE) %>%
        gsub(pattern = "\u00EA", replacement = "e", fixed = TRUE) %>%
        gsub(pattern = "\u00F4", replacement = "o", fixed = TRUE) %>%
        gsub(pattern = "\u00EF", replacement = "i", fixed = TRUE) %>%
        gsub(pattern = "\u00EB", replacement = "e", fixed = TRUE) %>%
        gsub(pattern = "\u00EE", replacement = "e", fixed = TRUE) %>%
        gsub(pattern = "\u00E7", replacement = "c", fixed = TRUE) %>%
        gsub(pattern = "\u00E2", replacement = "a", fixed = TRUE)
      ,


      city = stringr::str_extract(item_full_info, ".*Close") %>%
        stringr::str_extract(., ".*,") %>%
        stringr::str_extract(., "\u00bb.*") %>%
        gsub(pattern = "Close", replacement = "", fixed = TRUE) %>%
        gsub(pattern = ",", replacement = "", fixed = TRUE) %>%
        gsub(pattern = "\u00bb", replacement = "", fixed = TRUE) %>%
        gsub(pattern = "\u00FC", replacement = "u", fixed = TRUE) %>%
        gsub(pattern = "\u00E8", replacement = "e", fixed = TRUE) %>%
        stringr::word(., -1)

    )
    all_cities <- rbind(all_cities, city)
  }

  for (i in 1:nrow(all_cities)) {

    location <- googleway::google_geocode(
      as.vector(all_cities$address[i])
    )$results$geometry$location

    if(!is.null(location)) {
      all_cities[i, "latitude"] <- location$lat[1]
      all_cities[i, "longitude"] <- location$lng[1]

    }
  }

  # selecting only prices > 300 to get rid off most of wrong recorder data
  # and weakly rents ; deleting price > 10000 because not our focus
  all_cities <- all_cities[all_cities$price > 300,]
  all_cities <- all_cities[all_cities$price < 10000,]

  # Deleting rows where we have NA (about 16% of the data)
  all_cities <- all_cities[complete.cases(all_cities),]

  all_cities <- all_cities %>%
    mutate(predicted_price = summary(predict_price(all_cities))$predicted_price)

  return(all_cities)
}


#' @title Create a new "pred" object
#' @description This function enables to predict the prices of housings based on
#' their number of rooms, size in square meters, city and using a particular model.
#' The user can provide either a dataframe in the form of the ones outputted by the
#' get_immodata or providing only rooms, m2 and city of a single housing
#' @param housings A dataframe in the form of the ones outputted by the get_immodata
#' function.
#' @param rooms The number of rooms for a single housing estimation
#' @param m2 The number of m2 for a single housing estimation
#' @param city The city of a single housing estimation
#' @param model A model supported by the caret function for regression ("gam", "rf",
#' "nnet", "svmRadialCost", "rpart", ...)
#' @param seed The seed to use
#' @return A "pred" object of the expected prices of all observations of the
#' dataframe or the single housing
#' @author Germano David
#' @author Lomazzi Vincent
#' @author Bron Luca
#' @author Raisin Edgar
#' @author Grandadam Patrik
#' @importFrom magrittr %>%
#' @export
#' @examples
#' cities <- get_immodata(c("bussigny", "nyon"))
#' predict_price(cities) # based on a dataframe
#' predict_price(rooms = 3, m2 = 59, city = "nyon") # for an unique housing

predict_price <- function(housings, rooms, m2, city, model = "rf", seed = 1) {

  set.seed(seed)

  if (missing(housings) && (missing(rooms) || missing(m2) || missing(city))){
    message("Please enter an appropriate dataframe or complete informations")
  } else if (missing(housings) && !missing(rooms) && !missing(m2) && !missing(city)) {
    message("The output is the estimated price for an unique housing")
  } else if (
    !missing(housings) && (!missing(rooms) || !missing(m2) || !missing(city))
  ) {
    message("The output will be calculated based only on the dataframe")
  }

  if (!missing(housings)) {

    if (length(levels(housings$city)) >= 2){

      model_used <- caret::train(form = price ~ rooms + m2 + city,
                                 data = housings,
                                 method = model)

      predictions <- predict(model_used) %>%
        round(digits = 0) %>%
        unlist %>%
        as.numeric()

      df_predict <- housings %>% dplyr::mutate(predicted_price = predictions)
    }

    else if (length(levels(housings$city)) < 2){
      model_used <- caret::train(form = price ~ rooms + m2,
                                 data = housings,
                                 method = model)

      predictions <- predict(model_used) %>%
        round(digits = 0) %>%
        unlist %>%
        as.numeric()

      df_predict <- housings %>% dplyr::mutate(predicted_price = predictions)
    }


    rval <- df_predict

    class(rval) <- "pred"

    return(rval)
  }

  if (missing(housings) && !missing(rooms) && !missing(m2) && !missing(city)) {

    housings <- get_immodata(city)

    model_used <- caret::train(form = price ~ rooms + m2,
                               data = housings,
                               method = model)
    predictions <- predict(model_used, newdata = city) %>%
      round(digits = 0) %>%
      unlist %>%
      as.numeric()
    return(paste("The predicted price for this housing is",
                 round(predictions, 0),
                 "CHF."
    )
    )
  }
}

#' @title Extracting the prices of a "pred" object
#' @description This function enables to extract the prices of a "pred" object
#' and to add a new column of the expected prices to the original dataframe.
#' @param pred_object An objet of class "pred", which was a dataframe inputted
#' in the predict_price function.
#' @return A dataframe with a new column of estimated prices.
#' @author Germano David
#' @author Lomazzi Vincent
#' @author Bron Luca
#' @author Raisin Edgar
#' @author Grandadam Patrik
#' @details One has to be careful using this function as the model to obtain the
#' predicted values has been built on the same dataset in which it forecasts the
#' price. Overfitting is clearly present and may alter the predictions.
#' @export
#' @examples
#' cities <- get_immodata(c("bussigny", "nyon"))
#' predictions <- predict_price(cities)
#' summary(predictions)

summary.pred <- function(pred_object) {

  x = do.call(cbind.data.frame, pred_object)

  return(x)
}

#' @title Interactive plot of a "pred" object
#' @description This function enables to plot a "pred" object and to retrieve
#' the estimated values compared to the real prices on the market. Thanks to a
#' shiny app, it is possible to retrieve the characteristic of the "points" by
#' clicking on them or selecting multiple at the same time.
#' @param pred_object An objet of class "pred", which was a dataframe inputted
#' in the predict_price function.
#' @return A plot of the estimated prices against the real prices.
#' @author Germano David
#' @author Lomazzi Vincent
#' @author Bron Luca
#' @author Raisin Edgar
#' @author Grandadam Patrik
#' @details One has to be careful using this function as the model to obtain the
#' predicted values has been built on the same dataset in which it forecasts the
#' price. Overfitting is clearly present and may alter the predictions.
#' @export
#' @examples
#' cities <- get_immodata(c("bussigny", "nyon"))
#' predictions <- predict_price(cities)
#' plot(predictions)

plot.pred <- function(pred_object) {

  ui <- shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(width = 8,
                    shiny::plotOutput("plot1", height = 300,
                                      click = "plot1_click",
                                      brush = shiny::brushOpts(
                                        id = "plot1_brush"
                                      )
                    )
      )
    ),
    shiny::fluidRow(
      shiny::column(width = 12,
                    shiny::h4("Clicked point"),
                    shiny::verbatimTextOutput("click_info")
      ),
      shiny::column(width = 12,
                    shiny::h4("Brushed points"),
                    shiny:: verbatimTextOutput("brush_info")
      )
    )
  )

  server <- function(input, output) {

    toplot <- summary(pred_object)

    output$plot1 <- shiny::renderPlot({
      ggplot2::ggplot(toplot, aes(price, predicted_price)) +
        ggplot2::geom_point() +
        ggplot2::geom_abline(slope = 1, intercept = 1, color = "red") +
        ggplot2::xlab("Predicted values of the testing set") +
        ggplot2::ylab("Real value of the testing set") +
        my_theme()
    })

    output$click_info <- shiny::renderPrint({
      shiny::nearPoints(toplot, input$plot1_click)
    })

    output$brush_info <- renderPrint({
      brushedPoints(toplot, input$plot1_brush)
    })
  }

  return(shiny::shinyApp(ui, server))

}

#' @title Nice theme for ggplot graphs
#' @description This function enables to add a beautiful theme to ggplot graphs
#' @param base_size The base size, no need to change it.
#' @param base_family The base_family, no need to change it
#' @author Germano David
#' @author Lomazzi Vincent
#' @author Bron Luca
#' @author Raisin Edgar
#' @author Grandadam Patrik
#' @export
my_theme <- function(base_size = 10, base_family = "sans") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 10),
      axis.text.x = ggplot2::element_text(vjust = 0.5, hjust = 0.5),
      axis.title = ggplot2::element_text(size = 12),
      plot.title = ggplot2::element_text(hjust = 0.5),
      panel.grid.major = ggplot2::element_line(color = "grey"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "aliceblue"),
      strip.background = ggplot2::element_rect(fill = "lightgrey",
                                               color = "grey", size = 1),
      strip.text = ggplot2::element_text(face = "bold", size = 10, color = "black"),
      legend.position = "bottom",
      legend.justification = "top",
      legend.box = "horizontal",
      legend.box.background = ggplot2::element_rect(colour = "grey50"),
      legend.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color = "grey", fill = NA, size = 0.5)
    )
}




