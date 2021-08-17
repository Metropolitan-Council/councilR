#' Colour scales from council palette
#'
#' @description
#' Council colors turned into palettes
#'
#'



########
council.pal <- function(n, name) {# modified RcolorBrewer::brewer.pal
  switch(name,
         councilCat = switch(n - 2,
                             c(colors$councilBlue, colors$cdGreen, colors$mtsRed),
                             c(colors$councilBlue, colors$cdGreen, colors$mtsRed, colors$transitYellow),
                             c(colors$councilBlue, colors$cdGreen, colors$mtsRed, colors$transitYellow, colors$playLiBlue),
                             c(colors$councilBlue, colors$cdGreen, colors$mtsRed, colors$transitYellow, colors$playLiBlue, colors$metrostatsBrown),
                             c(colors$councilBlue, colors$cdGreen, colors$mtsRed, colors$transitYellow, colors$playLiBlue, colors$metrostatsBrown, colors$metrostatsMePurp),
                             c(colors$councilBlue, colors$cdGreen, colors$mtsRed, colors$transitYellow, colors$playLiBlue, colors$metrostatsBrown, colors$metrostatsMePurp, colors$metroOrange)
                             )

  )
}

council_pal2 <- # modified from scales:::brewer_pal
  function() { # stripped down all arguments, just to show the core
    function(n) {
      council.pal(n, "councilCat") ##modified, usually this is selected by a function
      ## with type and name as arguments, selecting a palette from a list called scales:::brewer
    }
  }

scale_fill_council <- ### modified from scale_fill_brewer, removed some arguments
  function (..., aesthetics = "fill") {
    discrete_scale(aesthetics, "custom", council_pal2(), ...) ## give a new name to the
    ## scale, it will create a new Scale object.
  }

scale_color_council <- ### modified from scale_fill_brewer, removed some arguments
  function (..., aesthetics = "colour") {
    discrete_scale(aesthetics, "custom", council_pal2(), ...) ## give a new name to the
    ## scale, it will create a new Scale object.
  }

test <- tibble(x = c(1:8), y = c(1:8), col = c(1:8))
ggplot(test, aes(x = x, y = y, col = as.factor(col), fill = as.factor(col))) +
  geom_point(size = 4, pch = 21)+
  scale_fill_council()+#"councilCat") +
  scale_color_council()#"councilCat")




