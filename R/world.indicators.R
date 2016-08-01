#' GetWorldBankList
#'
#' @return
#' @export
#'
#' @examples
GetWorldBankList <- function() {
  world.view <- c("Size of the economy",
                  "Global goals: ending poverty and improving lives",
                  "Global goals: promoting sustainability",
                  "Global goals: overcoming obstacles",
                  "Women in development")
  names(world.view) <- c("1.1", "1.2", "1.3", "1.4", "1.5" )

  world.view <- data.frame(indicator = rep("world.view", length(world.view)),
                           id = names(world.view),
                           value.name = world.view, stringsAsFactors = F)

  people <- c("Population dynamics",
              "Labor force structure",
              "Employment by sector",
              "Decent work and productive employment",
              "Unemployment",
              "Children at work",
              "Poverty rates at national poverty lines",
              "Poverty rates at international poverty lines",
              "Poverty rates at international poverty lines Part 2",
              "Distribution of income or consumption",
              "Shared Prosperity",
              "Education inputs",
              "Participation in education",
              "Education efficiency",
              "Education completion and outcomes",
              "Education gaps by income, gender and area",
              "Health systems",
              "Disease prevention coverage and quality",
              "Reproductive health",
              "Nutrition and growth",
              "Nutrition intake and supplements",
              "Health risk factors and future challenges",
              "Mortality",
              "Health gaps by income: Demography",
              "Health gaps by income: Child health",
              "Health gaps by income: Reproductive and women's health")
  names(people) <- c("2.1" ,"2.2", "2.3", "2.4", "2.5", "2.6", "2.7", "2.8", "2.8.2",
                     "2.9", "2.9.2", "2.10", "2.11", "2.12", "2.13", "2.14", "2.15",
                     "2.16", "2.17", "2.18", "2.19", "2.20", "2.21", "2.22", "2.22.2",
                     "2.22.3")
  people <- data.frame(indicator = rep("people", length(people)),
                       id = names(people),
                       value.name = people, stringsAsFactors = F)

  enviro.data <- c("Rural environment and land use", "Agricultural inputs", "Agricultural output and productivity",
                   "Deforestation and biodiversity", "Freshwater", "Energy production and use",
                   "Electricity production, sources, and access", "Energy dependency, efficiency and carbon dioxide emissions",
                   "Trends in greenhouse gas emissions", "Carbon dioxide emissions by sector",
                   "Climate variability, exposure to impact, and resilience", "Urbanization",
                   "Sustainable energy for all", "Contribution of natural resources to gross domestic product")
  names(enviro.data) <- c("3.1" ,"3.2", "3.3", "3.4", "3.5", "3.6", "3.7", "3.8",
                          "3.9", "3.10", "3.11", "3.12", "3.13", "3.14")
  enviro.data <- data.frame(indicator = rep("enviro.data", length(enviro.data)),
                            id = names(enviro.data),
                            value.name = enviro.data, stringsAsFactors = F)

  economy <- c("Growth of output", "Structure of output",
               "Structure of manufacturing", "Structure of merchandise exports",
               "Structure of merchandise imports", "Structure of service exports",
               "Structure of service imports", "Structure of demand",
               "Growth of consumption, investment and trade",
               "Towards a broader measure of national income",
               "Towards a broader measure of savings",
               "Central government finances", "Central government expenditure",
               "Central government revenues", "Monetary indicators",
               "Exchange rates and prices", "Balance of payments current account")
  names(economy) <- c("4.1", "4.2", "4.3", "4.4", "4.5", "4.6", "4.7", "4.8",
                      "4.9", "4.10", "4.11", "4.12", "4.13", "4.14", "4.15", "4.16",
                      "4.17")
  economy <- data.frame(indicator = rep("economy", length(economy)),
                        id = names(economy),
                        value.name = economy, stringsAsFactors = F)

  states.n.markets <- c("Private sector in the economy",
                        "Business environment: enterprise surveys",
                        "Business environment: Doing Business indicators",
                        "Stock markets", "Financial access, stability and efficiency",
                        "Tax policies", "Military expenditures and arms transfers",
                        "Fragile situations Part 1", "Fragile situations Part 2",
                        "Public policies and institutions Part 1", "Public policies and institutions Part 2",
                        "Transport services", "Power and communications", "The information society",
                        "Science and technology", "Statistical capacity")
  names(states.n.markets) <- c("5.1", "5.2", "5.3", "5.4", "5.5", "5.6", "5.7", "5.8",
                               "5.8.2", "5.9", "5.9.2", "5.10", "5.11", "5.12", "5.13",
                               "5.14")
  states.n.markets <- data.frame(indicator = rep("states.n.markets", length(states.n.markets)),
                                 id = names(states.n.markets),
                                 value.name = states.n.markets, stringsAsFactors = F)

  global.links <- c("Growth of merchandise trade",
                    "Direction and growth of merchandise trade",
                    "High income economies trade with low and middle income economies",
                    "Direction of trade of low and middle income economies",
                    "Primary commodity prices", "Tariff barriers", "Trade facilitation",
                    "External debt", "Global Private Financial Flows", "Net official financial flows",
                    "Aid dependency", "Distribution of net aid by Development Assistance Committee members",
                    "Movement of people across borders", "Travel and tourism")
  names(global.links) <- c("6.1", "6.2", "6.3", "6.4", "6.5", "6.6", "6.7", "6.8",
                           "6.9", "6.10", "6.11", "6.12", "6.13", "6.14")
  global.links <- data.frame(indicator = rep("global.links", length(global.links)),
                             id = names(global.links),
                             value.name = global.links, stringsAsFactors = F)

  df <- rbind(world.view, people, enviro.data, economy, states.n.markets, global.links)
  return(df)
}


#' GetRawTableFromID
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
GetRawTableFromID <- function(id) {
  url.base <- "http://wdi.worldbank.org"
  url.download <- paste(url.base, "table", id, sep = "/")
  df.raw <- GetRawTableFromURL(url.download)
  return(df.raw)
}

# Private functions ------------------------------------------------------------
#' Title
#'
#' @param url.download
#'
#' @return
#'
#' @examples
GetRawTableFromURL <- function(url.download) {
  GetHeaderNamesByLevel <- function(doc, level) {
    xpath.level <- paste("//tr[@class='level", level,"']/th", sep = "")
    h.name <- sapply(XML::xpathSApply(doc, xpath.level), XML::xmlValue)
    h.span <- sapply(XML::xpathSApply(doc, xpath.level), XML::xmlAttrs)
    h.span <- sapply(h.span, function(x) as.numeric(x[names(x) %in% c("colspan")]))
    h.span[sapply(h.span, function(x) identical(x, numeric(0)))] <- 0
    h.span <- as.numeric(h.span)
    names.prefix <- as.character()
    for (i in seq_along(h.name)) {
      names.prefix <- c(names.prefix,
                         rep(h.name[i], h.span[i]))
    }
    return(names.prefix)
  }

  df.raw <- XML::readHTMLTable(url.download)$scrollTable
  doc <- XML::htmlParse(url.download)

  header0 <- GetHeaderNamesByLevel(doc, 0)
  header1 <- GetHeaderNamesByLevel(doc, 1)
  header2 <- GetHeaderNamesByLevel(doc, 2)
  # Names Level 3
  t.names <- XML::xpathSApply(doc, "//tr[@class='level3']")
  t.names <- unlist(strsplit(sapply(t.names, XML::xmlValue), split = "  "))
  t.names <- sapply(t.names, stringr::str_trim)
  t.names <- as.character(t.names[t.names != ""])
  t.names <- paste(header0, header1, header2, t.names, sep = ".")
  names(df.raw) <- c("country", t.names)

  return(df.raw)
}
