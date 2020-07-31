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
                        .x = .,
                        .f = function(dep_i) {
                            use_plotly_cdn <- function(bundle, dep) {
                                if (dep$name == glue::glue("plotly-{bundle}")) {
                                    dep$src <- "https://cdn.plot.ly/"
                                    
                                    bundle <- ifelse(
                                        test = bundle == "main",
                                        yes  = "",
                                        no   = glue::glue("-{bundle}")
                                        )
                                    
                                    dep$script <- glue::glue(
                                        "plotly{bundle}-{dep$version}.min.js"
                                    ) %>% 
                                        as.character()
                                }
                                dep
                            }
                            
                            plotly_bundles <- c(
                                "basic", "cartesian", "geo", "gl3d", "gl2d",
                                "mapbox", "finance", "main"
                                )
                            
                            if (dep_i$name == "jquery")
                                dep_i$src <- list(href = glue::glue("https://cdnjs.cloudflare.com/ajax/libs/jquery/{dep_i$version}"))
                            
                            if (dep_i$name %in% glue::glue("plotly-{plotly_bundles}")) 
                                purrr::map(
                                    .x = plotly_bundles,
                                    .f = use_plotly_cdn,
                                    dep = dep_i
                                    )
                            
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
