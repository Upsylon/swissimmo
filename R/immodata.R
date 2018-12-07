#'@title Websrcapping of Immoscout24.ch data by city
#'@description This function enables to collect the data about the location market
#'from Immoscout24.ch. For given cities, on can retrieve the housings available for renting.
#'@param city_vector : a vector containing the name of the different cities
#'@return A dataframe containing the number of rooms, m2, price, address, post code
#'and city of the different accomodations available.
#'@author Germano David
#'@author Lomazzi Vincent
#'@author Bron Luca
#'@author Raisin Edgar
#'@author Grandadam Patrik
#'@importFrom magrittr %>%
#'@export
#'@examples
#'get_immodata(c("lugano", "lausanne))

get_immodata <- function(city_vector) {

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
      names(cities[i]), sep="")
  }

  ### Getting the number of pages for each cities
  pages <- list()
  for( i in 1:length(cities)){
    pages[[i]] <- xml2::read_html(x = paste0(unlist(attributes(cities[[i]]),
                                              use.names = FALSE))) %>%
      rvest::html_nodes(css = ".cXTlXt") %>%
      rvest::html_text() %>%
      as.numeric() %>%
      max(na.rm = TRUE) %>%
      magrittr::subtract(e2 = 1) %>%
      seq(from = 1)
  }

  ### Scrapping everything
  for (i in 1:length(cities)){
    for (page in pages[[i]]){
      url_path_page_immoscout <- cities[[i]] %>%
        attributes %>%
        unlist(use.names = FALSE) %>%
        paste0 %>%
        paste("?pn=", page, sep="")

      cities[[i]][[page]] <- list()
      cities[[i]][[page]] <- xml2::read_html(url_path_page_immoscout) %>%
        rvest::html_nodes(".csgoYM") %>%
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
             price = stringr::str_extract(item_full_info, "eCHF .*") %>% stringr::str_extract(., ".*.\u2014 *") %>%
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

      postcode = stringr::str_extract(item_full_info, ".*Close") %>%
        stringr::str_extract(., ".*,") %>%
        stringr::str_extract(., "\u00bb.*") %>%
        gsub(pattern = "Close", replacement = "", fixed = TRUE) %>%
        gsub(pattern = ",", replacement = "", fixed = TRUE) %>%
        gsub(pattern = "\u00bb", replacement = "", fixed = TRUE)  %>%
        stringr::word(., -2),

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

  # Deleting rows where we have NA (about 16% of the data)
  all_cities <- all_cities[complete.cases(all_cities),]

  return(all_cities)
}
