#' lm_diagnostic_ggplots
#'
#' Returns six ggplots depicting various diagnostics for the lm model passed.
#'
#' @param model lm() object
#'
#' @return A list of 6 ggplot objects
#' @export
#'
lm_diagnostic_ggplots <- function(model) {
    # adapted from:
    # https://rpubs.com/therimalaya/43190
    # 
    p1 <- ggplot(
            model, 
            aes(.fitted, .resid)
        ) +
        geom_point() +
        stat_smooth(method = "loess") + 
        geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
        labs(
            x = "Fitted values",
            y = "Residuals",
            title = "Residual vs Fitted Plot"
        ) +
        theme_bw()

    p2 <- ggplot(
            model, 
            aes(qqnorm(.stdresid)[[1]], .stdresid)
        ) +
        geom_point(na.rm = TRUE) +
        geom_abline(aes(qqline(.stdresid))) +
        labs(
            x = "Theoretical Quantiles",
            y = "Standardized Residuals",
            title = "Normal Q-Q"
        ) +
        theme_bw()

    p3 <- ggplot(
            model, 
            aes(.fitted, sqrt(abs(.stdresid)))
        ) +
        geom_point(na.rm = TRUE) +
        stat_smooth(method = "loess", na.rm = TRUE) + 
        labs(
            x = "Fitted Value",
            y = expression(sqrt("|Standardized residuals|")),
            title = "Scale-Location"
        ) +
        theme_bw()

    p4 <- ggplot(
            model, 
            aes(seq_along(.cooksd), .cooksd)
        ) +
        geom_bar(stat = "identity", position = "identity") +
        labs(
            x = "Obs. Number",
            y = "Cook's distance",
            title  = "Cook's distance"
        ) +
        theme_bw()

    p5 <- ggplot(
            model, 
            aes(.hat, .stdresid)
        ) +
        geom_point(aes(size = .cooksd), na.rm = TRUE) +
        stat_smooth(method = "loess", na.rm = TRUE) +
        labs(
            x = "Leverage",
            y = "Standardized Residuals",
            title = "Residual vs Leverage Plot"
        ) +
        scale_size_continuous("Cook's Distance", range = c(1, 5)) + 
        theme_bw() + 
        theme(legend.position = "bottom")

    p6 <- ggplot(
            model, 
            aes(.hat, .cooksd)
        ) +
        geom_point(na.rm = TRUE) +
        stat_smooth(method = "loess", na.rm = TRUE) + 
        labs(
            x = "Leverage hii",
            y = "Cook's Distance",
            title = "Cook's dist vs Leverage hii/(1-hii)"
        ) +
        geom_abline(
            slope = seq(0, 3, 0.5), 
            color = "gray", 
            linetype = "dashed"
        ) +
        theme_bw()

    return(list(rvfPlot = p1, qqPlot = p2, sclLocPlot = p3, cdPlot = p4, rvlevPlot = p5, cvlPlot = p6))
}