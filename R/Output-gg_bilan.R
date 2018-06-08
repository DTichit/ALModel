


gg_bilan <- function(data, lim) {

    # Chargement de packages
    if(!require(ggplot2)){
        install.packages("ggplot2")
        require(ggplot2)
    }
    if(!require(gridExtra)){
        install.packages("gridExtra")
        require(gridExtra)
    }

    # GGplot
    gg <- ggplot(data) + aes(x = Type, y = Montant, fill = Element, label = paste(Element, Montant)) +
        geom_bar(show.legend = FALSE, stat="identity", color = "white") +
        scale_fill_manual(values=rep(c("#690F3C", "#913764"), nrow(data))) + ylim(0, lim) +
        geom_text(size = 3, position = position_stack(vjust = 0.5), color = "white") + xlab("") + ylab("") +
        theme(axis.text.x = element_blank() , plot.title = element_text(hjust = 0.5),
              axis.text.y = element_blank(), axis.ticks = element_blank(),
              axis.line = element_blank(), panel.background = element_blank(),
              rect = element_blank())

    # Ouput
    return(gg)
}
