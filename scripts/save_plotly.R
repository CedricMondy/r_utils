save_plotly <- function(x, file, file_static = NULL, title, local = FALSE, libdir = "www") {
    require(plotly)
    require(purrr)
    require(glue)
    require(htmlwidgets)
    require(here)
    
    '%>%' <- plotly::'%>%'
    
    x$sizingPolicy$padding <- "0"
    
    if (local) {
        x                          %>% 
            plotly::partial_bundle(p        = .,
                                   local    = TRUE,
                                   minified = TRUE) %>% 
            htmlwidgets::saveWidget(widget        = .,
                                    file          = here::here(file),
                                    title         = title,
                                    selfcontained = TRUE)
    } else {
        x                          %>% 
            plotly::partial_bundle(p        = .,
                                   local    = FALSE,
                                   minified = TRUE) %>% 
            (function(x2) {
                x2$dependencies <- x2$dependencies %>% 
                    purrr::map(
                        function(dep_i) {
                            if (dep_i$name == "jquery")
                                dep_i$src <- list(href = glue::glue("https://cdnjs.cloudflare.com/ajax/libs/jquery/{dep_i$version}"))
                            
                            if (dep_i$name == "plotly-basic") {
                                dep_i$src <-  list(href = "https://cdn.plot.ly/")
                                dep_i$script <- glue::glue("plotly-basic-{dep_i$version}.min.js")
                            }
                            
                            if (dep_i$name == "plotly-cartesian") {
                                dep_i$src <-  list(href = "https://cdn.plot.ly/")
                                dep_i$script <- glue::glue("plotly-cartesian-{dep_i$version}.min.js")
                            }
                            
                            if (dep_i$name == "plotly-geo") {
                                dep_i$src <-  list(href = "https://cdn.plot.ly/")
                                dep_i$script <- glue::glue("plotly-geo-{dep_i$version}.min.js")
                            }
                            
                            if (dep_i$name == "plotly-gl3d") {
                                dep_i$src <-  list(href = "https://cdn.plot.ly/")
                                dep_i$script <- glue::glue("plotly-gl3d-{dep_i$version}.min.js")
                            }
                            
                            if (dep_i$name == "plotly-gl2d") {
                                dep_i$src <-  list(href = "https://cdn.plot.ly/")
                                dep_i$script <- glue::glue("plotly-gl2d-{dep_i$version}.min.js")
                            }
                            
                            if (dep_i$name == "plotly-mapbox") {
                                dep_i$src <-  list(href = "https://cdn.plot.ly/")
                                dep_i$script <- glue::glue("plotly-mapbox-{dep_i$version}.min.js")
                            }
                            
                            if (dep_i$name == "plotly-finance") {
                                dep_i$src <-  list(href = "https://cdn.plot.ly/")
                                dep_i$script <- glue::glue("plotly-finance-{dep_i$version}.min.js")
                            }
                            
                            if (dep_i$name == "plotly-main") {
                                dep_i$src <-  list(href = "https://cdn.plot.ly/")
                                dep_i$script <- glue::glue("plotly-{dep_i$version}.min.js")
                            }
                            
                            dep_i$script <- purrr::map(dep_i$script, as.character)
                            dep_i
                        }
                    )
                
                x2
            }) %>% 
            htmlwidgets::saveWidget(widget        = .,
                                    file          = here::here(file),
                                    title         = title,
                                    selfcontained = FALSE,
                                    libdir        = "www")
    }
    
    if (!is.null(file_static))
        plotly::orca(x, file = file_static)
    
    c(file, file_static)
}

