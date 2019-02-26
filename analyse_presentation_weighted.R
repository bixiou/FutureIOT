# options(download.file.method = "wget"); # For Ubuntu 14.04
package <- function(p) { 
  if (!is.element(p, installed.packages()[,1])) {
    install.packages(p); 
  }
  library(p, character.only = TRUE)
} # loads packages with automatical install if needed

# R tips: library(pastecs) stat.desc(mydata)
#         ordered(mydata) instead of as.item for likert-scale
#         scatterplot(SAT~Age|Gender, id.method="identify", data=mydata)
#         jitter instead of rnorm
#         read_csv instead of read.csv
package("foreign")
package("memisc")
package("Hmisc")
package("DT")
package("pastecs")
package("lsr")
package("ggplot2")
package("stringr")
package("survey")
package("plotly")
package("plyr")
package("quantreg")
package("asbio")
package("corrplot")
package("spatstat")
package("quantreg")
Sys.setenv("plotly_username" = "bixiou")
Sys.setenv("plotly_api_key" = "701bafmnv9")
Sys.setenv("plotly_username" = "lesgrains")
Sys.setenv("plotly_api_key" = "0w9p9x83j3")
Sys.setenv("plotly_username" = "gmx")
Sys.setenv("plotly_api_key" = "UDKxEKfFt6F1zzDk9nnL")
Sys.setenv("plotly_username" = "fabre.adri1")
Sys.setenv("plotly_api_key" = "9RRWTqt8FNfU9PClbFsP")
### to create new user, 1. Sys.setenv("plotly_username" = new_user), 2. signup(new_user, email)

stack_bars <- function(vars, data=m, miss=T, labels=NA, title=NA, accord=FALSE, en = FALSE, margin=c(2.5,17,0,3), cex=1, width=0.77/length(vars), weights=FALSE) {
  matrice <- c()
  colors <-   c(rainbow(4, end=4/15)[1:3], "green", "forestgreen") # c("red", "orange", "yellow", "green", "darkgreen") # rainbow(5, end=1/3)
  for (var in vars) {
    if (miss) {
      mat <- c(length(which(data[[var]]==-2))/length(which(!is.missing(data[[var]]))), length(which(data[[var]]==-1))/length(which(!is.missing(data[[var]]))), length(which(data[[var]]==0))/length(which(!is.missing(data[[var]]))), length(which(data[[var]]==1))/length(which(!is.missing(data[[var]]))), length(which(data[[var]]==2))/length(which(!is.missing(data[[var]]))),length(which(is.missing(data[[var]]) & !is.na(data[[var]])))/length(which(!is.missing(data[[var]]) & !is.na(data[[var]]))))
      if (weights) { mat <- c(sum(data[['weight']][which(data[[var]]==-2)])/sum(data[['weight']][!is.missing(data[[var]])]), sum(data[['weight']][which(data[[var]]==-1)])/sum(data[['weight']][!is.missing(data[[var]])]), sum(data[['weight']][which(data[[var]]==0)])/sum(data[['weight']][!is.missing(data[[var]])]), sum(data[['weight']][which(data[[var]]==1)])/sum(data[['weight']][!is.missing(data[[var]])]), sum(data[['weight']][which(data[[var]]==2)])/sum(data[['weight']][!is.missing(data[[var]])]),sum(data[['weight']][which(is.missing(data[[var]]) & !is.na(data[[var]]))])/sum(data[['weight']][!is.missing(data[[var]])])) }
      colors <- c(colors, "lightgrey")    }
    else {
      mat <- c(length(which(data[[var]]==-2))/length(which(!is.missing(data[[var]]))), length(which(data[[var]]==-1))/length(which(!is.missing(data[[var]]))), length(which(data[[var]]==0))/length(which(!is.missing(data[[var]]))),  length(which(data[[var]]==1))/length(which(!is.missing(data[[var]]))),  length(which(data[[var]]==2))/length(which(!is.missing(data[[var]]))))    
      if (weights) { mat <- c(sum(data[['weight']][which(data[[var]]==-2)])/sum(data[['weight']][!is.missing(data[[var]])]), sum(data[['weight']][which(data[[var]]==-1)])/sum(data[['weight']][!is.missing(data[[var]])]), sum(data[['weight']][which(data[[var]]==0)])/sum(data[['weight']][!is.missing(data[[var]])]), sum(data[['weight']][which(data[[var]]==1)])/sum(data[['weight']][!is.missing(data[[var]])]), sum(data[['weight']][which(data[[var]]==2)])/sum(data[['weight']][!is.missing(data[[var]])])) } }
    matrice <- c(matrice, mat) }
  matrice <- matrix(c(matrice), ncol=length(vars))
  if (is.na(labels)) { labels <- vars }
  if (accord) { values <- c("Pas du tout", "Pas vraiment d'accord", "Indifférent-e", "Assez", "Tout à fait d'accord")
    if (miss) { widths <- c(0.16,0.16,0.13,0.125,0.145,0.05) }
    else { widths <- c(0.18,0.185,0.15,0.14,0.2) } }
  else { values <- c("Baisser fortement", "légèrement", "Maintenir", "Augmenter légèrement", "fortement")
    if (miss) { widths <- c(0.153,0.14,0.14,0.15,0.083,0.05) }
    else { widths <- c(0.173,0.16,0.165,0.19,0.095) } }
  if (en) {values <- c("Strongly decrease", "Slightly decrease", "Maintain", "Slightly increase", "Strongly increase")
    if (accord) values <- c("Totally disagree", "Disagree", "Indifferent", "Agree", "Totally agree")
    if (miss) { widths <- c(0.16,0.15,0.14,0.13,0.12,0.06) }
    else { widths <- c(0.173,0.16,0.165,0.19,0.095) } }
  if (miss) { 
    if (en) values <- c(values, "PNR")
    else values <- c(values, "NSP") }
  # if (accord) { values <- c("Pas du tout d'accord", "Pas vraiment d'accord", "Indifférent-e", "Assez d'accord", "Tout à fait d'accord") }
  # else { values <- c("Baisser fortement", "Baisser légèrement", "Maintenir au niveau actuel", "Augmenter légèrement", "Augmenter fortement") }
  # if (miss) { values <- c(values, "NSP (Ne sait pas, ne se prononce pas)")} # TODO: trouver widths pour ceux-là et les mettre
  before_par <- par()
  titre <- 0
  if (!is.na(title)) { titre <- 1.5 }
  par(mar=margin, oma=c(0,0,titre,0))
  frame()
  abline(v=seq(0,1,by=0.1), lty=3, col="grey")
  axis(1, at = seq(0,1,by=0.1))
  barplot(matrice, width=width, horiz=TRUE, add=TRUE, col=colors, names.arg = labels, cex.names = cex, border=NA, ylim=c(0,1), legend.text=values, las=1, args.legend=list(horiz=TRUE, bty='o', box.lwd=0, xjust=1, text.width=widths, x.intersp=0.3, x="topright")) # ncol=3, inset=-0.3
  title(title, outer=TRUE)
  par(before_par)
  # legend("topright", fill=colors, legend=values, ncol=2)
}
bars_transfert <- function(var, data=m, miss=T, title=NA, weights=FALSE) {
  colors <-  c(rainbow(4, end=4/15), "forestgreen") # c("red", "orange", "yellow", "green", "darkgreen") # rainbow(5, end=1/3)
    if (miss) {
      mat <- c( length(which(data[[var]]>-1 & data[[var]]<=1))/length(which(data[[var]]>-1)), length(which(data[[var]]>1 & data[[var]]<=4))/length(which(data[[var]]>-1)), length(which(data[[var]]>4 & data[[var]]<=7))/length(which(data[[var]]>-1)), length(which(data[[var]]>7 & data[[var]]<=13))/length(which(data[[var]]>-1)),length(which(data[[var]]>13))/length(which(data[[var]]>-1)),  length(which(data[["transferts_inter_a"]]=="" | is.na(data[["transferts_inter_a"]])))/length(which(data[[var]]>-1))-1)
      if (weights) { mat <- c(sum(data[['weight']][which(data[[var]]>-1 & data[[var]]<=1)])/sum(data[['weight']][which(data[[var]]>-1)]), sum(data[['weight']][which(data[[var]]>1 & data[[var]]<=4)])/sum(data[['weight']][which(data[[var]]>-1)]), sum(data[['weight']][which(data[[var]]>4 & data[[var]]<=7)])/sum(data[['weight']][which(data[[var]]>-1)]), sum(data[['weight']][which(data[[var]]>7 & data[[var]]<=13)])/sum(data[['weight']][which(data[[var]]>-1)]), sum(data[['weight']][which(data[[var]]>13)])/sum(data[['weight']][which(data[[var]]>-1)]),sum(data[['weight']][which(data[["transferts_inter_a"]]=="" | is.na(data[["transferts_inter_a"]]))])/sum(data[['weight']][which(data[[var]]>-1)])-1) }
      colors <- c(colors, "lightgrey")    }
    else {
      mat <- c( length(which(data[[var]]>-1 & data[[var]]<=1))/length(which(data[[var]]>-1)), length(which(data[[var]]>1 & data[[var]]<=4))/length(which(data[[var]]>-1)), length(which(data[[var]]>4 & data[[var]]<=7))/length(which(data[[var]]>-1)), length(which(data[[var]]>7 & data[[var]]<=13))/length(which(data[[var]]>-1)),length(which(data[[var]]>13))/length(which(data[[var]]>-1)))
      if (weights) { mat <- c(sum(data[['weight']][which(data[[var]]>-1 & data[[var]]<=1)])/sum(data[['weight']][which(data[[var]]>-1)]), sum(data[['weight']][which(data[[var]]>1 & data[[var]]<=4)])/sum(data[['weight']][which(data[[var]]>-1)]), sum(data[['weight']][which(data[[var]]>4 & data[[var]]<=7)])/sum(data[['weight']][which(data[[var]]>-1)]), sum(data[['weight']][which(data[[var]]>7 & data[[var]]<=13)])/sum(data[['weight']][which(data[[var]]>-1)]), sum(data[['weight']][which(data[[var]]>13)])/sum(data[['weight']][which(data[[var]]>-1)])) } }
  values <- c("Inférieur à 1%", "de 1,1% à 4%", "de 4,1% à 7%", "de 7,1% à 13%", "supérieur à 13,1%")
  if (miss) { widths <- c(0.135,0.13,0.12,0.11,0.09,0) }
  else { widths <- c(0.16,0.15,0.14,0.13,0.11) }
  if (miss) { values <- c(values, "NSP")}
  before_par <- par()
  titre <- 0
  if (!is.na(title)) { titre <- 2.5 }
  par(mar=c(2,0,0,8), oma=c(0,0,titre,0))
  frame()
  abline(v=seq(0,1,by=0.1), lty=3, col="grey")
  axis(1, at = seq(0,1,by=0.1))
  barplot(matrix(mat, ncol=1), width=0.6, horiz=TRUE, add=TRUE, col=colors, border=NA, ylim=c(0,1), legend.text=values, las=1, args.legend=list(horiz=TRUE, bty='o', box.lwd=0, xjust=1, text.width=widths, x.intersp=0.3, x="topright")) # ncol=3, inset=-0.3
  title(title, outer=TRUE)
  par(before_par)
  # legend("topright", fill=colors, legend=values, ncol=2)
}
bars_transfert_en <- function(var, data=m, miss=T, title=NA, weights=FALSE) {
  colors <-  c(rainbow(4, end=4/15), "forestgreen") # c("red", "orange", "yellow", "green", "darkgreen") # rainbow(5, end=1/3)
    if (miss) {
       mat <- c( length(which(data[[var]]>-1 & data[[var]]<=1))/length(which(data[[var]]>-1)), length(which(data[[var]]>1 & data[[var]]<=4))/length(which(data[[var]]>-1)), length(which(data[[var]]>4 & data[[var]]<=7))/length(which(data[[var]]>-1)), length(which(data[[var]]>7 & data[[var]]<=13))/length(which(data[[var]]>-1)),length(which(data[[var]]>13))/length(which(data[[var]]>-1)),  length(which(data[["transferts_inter_a"]]=="" | is.na(data[["transferts_inter_a"]])))/length(which(data[[var]]>-1))-1)
      if (weights) { mat <- c(sum(data[['weight']][which(data[[var]]>-1 & data[[var]]<=1)])/sum(data[['weight']][which(data[[var]]>-1)]), sum(data[['weight']][which(data[[var]]>1 & data[[var]]<=4)])/sum(data[['weight']][which(data[[var]]>-1)]), sum(data[['weight']][which(data[[var]]>4 & data[[var]]<=7)])/sum(data[['weight']][which(data[[var]]>-1)]), sum(data[['weight']][which(data[[var]]>7 & data[[var]]<=13)])/sum(data[['weight']][which(data[[var]]>-1)]), sum(data[['weight']][which(data[[var]]>13)])/sum(data[['weight']][which(data[[var]]>-1)]),sum(data[['weight']][which(data[["transferts_inter_a"]]=="" | is.na(data[["transferts_inter_a"]]))])/sum(data[['weight']][which(data[[var]]>-1)])-1) }
      colors <- c(colors, "lightgrey")    }
    else {
      mat <- c( length(which(data[[var]]>-1 & data[[var]]<=1))/length(which(data[[var]]>-1)), length(which(data[[var]]>1 & data[[var]]<=4))/length(which(data[[var]]>-1)), length(which(data[[var]]>4 & data[[var]]<=7))/length(which(data[[var]]>-1)), length(which(data[[var]]>7 & data[[var]]<=13))/length(which(data[[var]]>-1)),length(which(data[[var]]>13))/length(which(data[[var]]>-1)))
      if (weights) { mat <- c(sum(data[['weight']][which(data[[var]]>-1 & data[[var]]<=1)])/sum(data[['weight']][which(data[[var]]>-1)]), sum(data[['weight']][which(data[[var]]>1 & data[[var]]<=4)])/sum(data[['weight']][which(data[[var]]>-1)]), sum(data[['weight']][which(data[[var]]>4 & data[[var]]<=7)])/sum(data[['weight']][which(data[[var]]>-1)]), sum(data[['weight']][which(data[[var]]>7 & data[[var]]<=13)])/sum(data[['weight']][which(data[[var]]>-1)]), sum(data[['weight']][which(data[[var]]>13)])/sum(data[['weight']][which(data[[var]]>-1)])) } }
  values <- c("Less than 1%", "from 1,1% to 4%", "from 4,1% to 7%", "from 7,1% to 13%", "more than 13,1%")
  if (miss) { widths <- c(0.135,0.13,0.12,0.11,0.09,0) }
  else { widths <- c(0.16,0.15,0.14,0.13,0.11) }
  if (miss) { values <- c(values, "PNR")}
  before_par <- par()
  titre <- 0
  if (!is.na(title)) { titre <- 2.5 }
  par(mar=c(2,0,0,12), oma=c(0,0,titre,0))
  frame()
  abline(v=seq(0,1,by=0.1), lty=3, col="grey")
  axis(1, at = seq(0,1,by=0.1))
  barplot(matrix(mat, ncol=1), width=0.6, horiz=TRUE, add=TRUE, col=colors, border=NA, ylim=c(0,1), legend.text=values, las=1, args.legend=list(horiz=TRUE, bty='o', box.lwd=0, xjust=1, text.width=widths, x.intersp=0.3, x="topright")) # ncol=3, inset=-0.3
  title(title, outer=TRUE)
  par(before_par)
  # legend("topright", fill=colors, legend=values, ncol=2)
}
do_pie <- function(Data, Colors, File, Title, Hover = Data[,1], Display_values=TRUE) {
  Textinfo <- 'label+percent'
  if (!Display_values) { Textinfo <- 'label' }
  pie <- plot_ly(Data, labels = Data[,1], values = Data[,2], type = 'pie',
          textposition = 'outside',
          textinfo = Textinfo,
          insidetextfont = list(color = '#FFFFFF'),
          outsidetextfont = list(color = 'black'),
          hoverinfo = 'text',
          text = Hover,
          sort = FALSE,
          marker = list(colors = Colors, line = list( color = '#FFFFFF', width = 1)), 
          showlegend = FALSE) %>%
    layout(titlefont=list(color='black'),
        title = paste("<b>",Title,"</b>", sep=""),
          margin = list(t=80, b=70),
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  htmlwidgets::saveWidget(pie, paste("/home/adrien/Google Drive/Economie/Travail/enquete/images/plotly_html/", File,".html", sep=""), libdir = "dependencies")
  api_create(pie, filename=File, sharing="public")  
}
# j'ai modifié oui_non, data5, data_seuils et data_rdb pour que l'affichage des NSP soit honnête, i.e. à l'échelle : ça n'est pas le cas sur le site, mais ce n'est pas si grave, car les graphiques sont interactifs (et on peut y lire les vraies valeurs).
oui_non <- function(vars, file, labels = vars, data = m, display_value = T, weights=T, margin_r=0, margin_l=250, title="", en=FALSE) {
  margin_t <- 30
  if (title!="") { margin_t <- 80 }
  if (grepl("<br>", title)) { margin_t <- 130 }
  
  oui <- non <- nsp <- c()
  for (var in vars) {
    if (weights) {
      oui <- c(oui, sum(data[['weight']][which(data[[var]]=="Oui" | data[[var]]=="Pour" | data[[var]]=="Taxer davantage le capital" | data[[var]]=="quotient")])/sum(data[['weight']][which(data[[var]]=="Oui" | data[[var]]=="Non" | data[[var]]=="NSP"| data[[var]]=="Pour" | data[[var]]=="Contre" | data[[var]]=="Taxer davantage le capital" | data[[var]]=="Taxer davantage le travail" | data[[var]]=="quotient" | data[[var]]=="indiv")]) )
      non <- c(non, sum(data[['weight']][which(data[[var]]=="Non" | data[[var]]=="Contre" | data[[var]]=="Taxer davantage le travail" | data[[var]]=="indiv")])/sum(data[['weight']][which(data[[var]]=="Oui" | data[[var]]=="Non" | data[[var]]=="NSP"| data[[var]]=="Pour" | data[[var]]=="Contre" | data[[var]]=="Taxer davantage le capital" | data[[var]]=="Taxer davantage le travail" | data[[var]]=="quotient" | data[[var]]=="indiv")]) )
      nsp <- c(nsp, sum(data[['weight']][which(data[[var]]=="NSP" | data[[var]]==-1)])/sum(data[['weight']][which(data[[var]]=="Oui" | data[[var]]=="Non" | data[[var]]=="Pour" | data[[var]]=="Contre" | data[[var]]=="Taxer davantage le capital" | data[[var]]=="Taxer davantage le travail" | data[[var]]=="quotient" | data[[var]]=="indiv")]) ) #  | data[[var]]==-1 | data[[var]]=="NSP"
    }
    else {
      oui <- c(oui, length(which(data[[var]]=="Oui" | data[[var]]=="Pour" | data[[var]]=="Taxer davantage le capital" | data[[var]]=="quotient"))/length(which(data[[var]]=="Oui" | data[[var]]=="Non" | data[[var]]=="NSP"| data[[var]]=="Pour" | data[[var]]=="Contre" | data[[var]]=="Taxer davantage le capital" | data[[var]]=="Taxer davantage le travail" | data[[var]]=="quotient" | data[[var]]=="indiv")) )
      non <- c(non, length(which(data[[var]]=="Non" | data[[var]]=="Contre" | data[[var]]=="Taxer davantage le travail" | data[[var]]=="indiv"))/length(which(data[[var]]=="Oui" | data[[var]]=="Non" | data[[var]]=="NSP"| data[[var]]=="Pour" | data[[var]]=="Contre" | data[[var]]=="Taxer davantage le capital" | data[[var]]=="Taxer davantage le travail" | data[[var]]=="quotient" | data[[var]]=="indiv")) )
      nsp <- c(nsp, length(which(data[[var]]=="NSP" | data[[var]]==-1))/length(which(data[[var]]=="Oui" | data[[var]]=="Non" | data[[var]]=="Pour" | data[[var]]=="Contre" | data[[var]]=="Taxer davantage le capital" | data[[var]]=="Taxer davantage le travail" | data[[var]]=="quotient" | data[[var]]=="indiv")) )
    }  
  }
  true_nsp <- round(100 * nsp*(oui+non))
  oui <- round(100 * oui)
  non <- round(100 * non)
  nsp <- round(100 * nsp)
  order_as <- order(oui/(oui+non))
  y <- labels[order_as]
  non <- non[order_as]
  nsp <- nsp[order_as]
  true_nsp <- true_nsp[order_as]
  oui <- sort(oui)
  o <- round(100 * oui / (oui + non))
  n <- round(100 * non / (oui + non))
  
  if (en) {
    hover_oui <- paste('Yes<br>', oui, '% of answers<br>', o, '% of expressed answers')
    hover_non <- paste('No<br>', non, '% of answers<br>',n, '% of expressed answers')
    hover_nsp <- paste('PNR<br>', true_nsp, '% of answers')  
    Text <- c("Yes", "No", "PNR")      }
  else {
    hover_oui <- paste('Oui<br>', oui, '% des réponses<br>', o, '% des réponses exprimées')
    hover_non <- paste('Non<br>', non, '% des réponses<br>',n, '% des réponses exprimées')
    hover_nsp <- paste('NSP<br>', true_nsp, '% des réponses')  
    Text <- c("Oui", "Non", "NSP") }
  if (display_value) {
    hover_oui <- paste(oui, '%')
    hover_non <- paste(non, '%')
    hover_nsp <- paste(true_nsp, '%')
  }

  data <- data.frame(y, oui, non, nsp, o, n)
  data$y <- factor(data$y, levels = data[["y"]])
  y <- c(y, '')
  bars <- plot_ly(data, x = ~o, y = ~y, type = 'bar', orientation = 'h', text = hover_oui, textposition = 'auto', # last one displays values; colors were forestgreen and darkred
            hoverinfo = 'text', marker = list(color = 'lightgreen', line = list(color = 'white', width = 1))) %>%
    add_trace(x = ~n, text = hover_non, hoverinfo = 'text', marker = list(color = 'plum')) %>%
    add_trace(x = ~nsp, text = hover_nsp, hoverinfo = 'text', marker = list(color = 'lightgrey')) %>%
    layout(xaxis = list(title = "",
                        showgrid = FALSE,
                        showline = FALSE,
                        showticklabels = FALSE,
                        zeroline = FALSE,
                        domain = c(0.15, 1)),
           yaxis = list(title = "",
                        showgrid = FALSE,
                        showline = FALSE,
                        showticklabels = FALSE,
                        zeroline = FALSE),
           hovermode = 'closest',
           barmode = 'stack',
           title = title,
           titlefont = list(color='black'),
           font = list(color='black'),
           # paper_bgcolor = 'rgb(248, 248, 255)', plot_bgcolor = 'rgb(248, 248, 255)',
           margin = list(l = margin_l, r = margin_r, t = margin_t, b = 0),
           showlegend = FALSE) %>%
    # labeling the y-axis
    add_annotations(xref = 'paper', yref = 'y', x = 0.14, y = y,
                    xanchor = 'right',
                    text = y,
                    font = list(family = 'Arial', size = 14, color = 'black'),
                    showarrow = FALSE, align = 'right') %>%
    # labeling the first Likert scale (on the top)
    add_annotations(xref = 'x', yref = 'paper',
                    x = c(10, 90, 110),
                    y = 1.05,
                    text = Text,
                    font = list(family = 'Arial', size = 16, color = 'black'),
                    showarrow = FALSE) # %>%
    # labeling the percentages of each bar (x_axis)
    # add_annotations(xref = 'x', yref = 'y',
    #                 x = o / 2, y = y,
    #                 text = paste(data[,"oui"], '%'),
    #                 font = list(family = 'Arial', size = 14, color = 'white'),
    #                 showarrow = FALSE) %>%
    # add_annotations(xref = 'x', yref = 'y',
    #                 x = o + n / 2, y = y,
    #                 text = paste(data[,"non"], '%'),
    #                 font = list(family = 'Arial', size = 14, color = 'white'),
    #                 showarrow = FALSE) %>%
    # add_annotations(xref = 'x', yref = 'y',
    #                 x = o + n + nsp / 2, y = y,
    #                 text = paste(data[,"nsp"], '%'),
    #                 font = list(family = 'Arial', size = 14, color = 'white'),
    #                 showarrow = FALSE) %>%
  api_create(bars, filename=file, sharing="public")
  # return(bars) # bugs most often than not
}
data5 <- function(vars, data=m, miss=T, weights=T) {
  matrice <- c()
  colors <-  c(rainbow(4, end=4/15), "forestgreen") # c("red", "orange", "yellow", "green", "darkgreen") # rainbow(5, end=1/3)
  for (var in vars) {
    if (miss) {
      if (is.null(annotation(data[[var]]))) {
        mat <- c(length(which(n(data[[var]])==-2))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==-1))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==0))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==1))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==2))/length(which(!is.missing(n(data[[var]])))),length(which(is.na(data[[var]])))/length(which(!is.missing(n(data[[var]]))))) # removed "n()"
        if (weights) { mat <- c(sum(data[['weight']][which(n(data[[var]])==-2)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==-1)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==0)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==1)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==2)])/sum(data[['weight']][!is.missing(n(data[[var]]))]),sum(data[['weight']][which(is.na(data[[var]]))])/sum(data[['weight']][!is.missing(n(data[[var]]))])) } }
      else {
        mat <- c(length(which(n(data[[var]])==-2))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==-1))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==0))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==1))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==2))/length(which(!is.missing(n(data[[var]])))),length(which(is.missing(data[[var]]) & !is.na(data[[var]])))/length(which(!is.missing(data[[var]])))) # removed "n()"
        if (weights) { mat <- c(sum(data[['weight']][which(n(data[[var]])==-2)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==-1)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==0)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==1)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==2)])/sum(data[['weight']][!is.missing(n(data[[var]]))]),sum(data[['weight']][which(is.missing(data[[var]]) & !is.na(data[[var]]))])/sum(data[['weight']][!is.missing(data[[var]])])) } }
      colors <- c(colors, "lightgrey")    }
    else {
      mat <- c(length(which(n(data[[var]])==-2))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==-1))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==0))/length(which(!is.missing(n(data[[var]])))),  length(which(n(data[[var]])==1))/length(which(!is.missing(n(data[[var]])))),  length(which(n(data[[var]])==2))/length(which(!is.missing(n(data[[var]])))))    
      if (weights) { mat <- c(sum(data[['weight']][which(n(data[[var]])==-2)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==-1)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==0)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==1)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==2)])/sum(data[['weight']][!is.missing(n(data[[var]]))])) } }
    matrice <- c(matrice, mat) }
  matrice <- matrix(c(matrice), ncol=length(vars))
  return(matrice)
  # return(as.data.frame(matrice))
}
data1 <- function(vars, data=m, weights=T) {
  res <- c()
  for (var in vars) {
    if (weights) { res <- c(res, sum(data[['weight']][which(data[[var]]==TRUE)])/sum(data[['weight']][which(data[[var]]==TRUE | data[[var]]==FALSE)])) }
    else { res <- c(res, length(which(data[[var]]==T)))/length(which(data[[var]]==T | data[[var]]==FALSE)) }
  }
  return( matrix(res, ncol=length(vars)) )
}
color5 <- c(rainbow(4, end=4/15)[1:3], "#00FF00", "#228B22") # the last two are: green, forestgreen
accord5 <- c("Pas du tout d'accord", "Pas vraiment d'accord", "Indifférent-e", "Assez d'accord", "Tout à fait d'accord")
agree5 <- c("Strongly disagree", "Disagree", "Indifferent", "Agree", "Strongly agree")
evol5 <- c("Baisser fortement", "Baisser légèrement", "Maintenir au niveau", "Augmenter légèrement", "Augmenter fortement")
evolve5 <- c("Strongly decrease", "Slightly decrease", "Maintain", "Slightly increase", "Strongly increase")
data_seuils <- function(data=m, variable, miss=T, variante="", weights=T, seuils=c(20, 40, 60, 80), closed_left = TRUE, closed_right=FALSE, order=NA, Min_min=-0.5) {
  matrice <- c()
  if (variante=="") {
    if (is.na(order)) { order <- 1:length(variable) }
    for (v in variable[order]) {
      Min <- Min_min
      for (j in 1:(length(seuils)+1)) {
        Pass <- FALSE
        if (j!=1) { Min <- seuils[j-1] }
        if (j==length(seuils)+1) { 
          Max <- "Inf" 
          if (seuils[j-1]==Inf) { Pass <- T } }
        else { Max <- seuils[j] }
        if (weights) { matrice <- c(matrice, sum(data[['weight']][which((data[[v]]>Min | (data[[v]]==Min)*(closed_left | Pass)) & (data[[v]]<Max | (data[[v]]==Max)*((closed_right & j!=length(seuils)+1) | Pass)))])/sum(data[['weight']][which(data[[v]]>=Min_min)])) }
        else { matrice <- c(matrice, length(which((data[[v]]>Min | (data[[v]]==Min)*(closed_left | Pass)) & (data[[v]]<Max | (data[[v]]==Max)*((closed_right & j!=length(seuils)+1) | Pass))))/length(which(data[[v]]>=Min_min))) } # added "+1" in (closed_right & j!=length(seuils)+1) | Pass)
      }
      if (miss) {
        if (weights) { matrice <- c(matrice, sum(data[['weight']][which(data[[v]]==-1)])/sum(data[['weight']][which(data[[v]]!=-1 & !is.na(data[[v]]))])) }
        else { matrice <- c(matrice, length(which(data[[v]]==-1 ))/length(which(data[[v]]!=-1 & !is.na(data[[v]])))) }
      }
    }
    matrice <- matrix(c(matrice), ncol=length(variable))    
  }
  else {
    variantes <- levels(factor(data[[variante]][!is.na(data[[variable]]) & data[[variante]]!=""]))
    if (is.na(order)) { order <- 1:length(variantes) }
    for (var in variantes[order]) {
      Min <- Min_min
      for (j in 1:(length(seuils)+1)) {
        Pass <- FALSE
        if (j!=1) { Min <- seuils[j-1] }
        if (j==length(seuils)+1) {  
          Max <- "Inf" 
          if (seuils[j-1]==Inf) { Pass <- T } }
        else { Max <- seuils[j] }
          if (weights) { matrice <- c(matrice, sum(data[['weight']][which((data[[variable]]>Min | (data[[variable]]==Min)*(closed_left | Pass)) & (data[[variable]]<Max | (data[[variable]]==Max)*((closed_right & j!=length(seuils)) | Pass)) & data[[variante]]==var)])/sum(data[['weight']][which(data[[variable]]>=Min_min & data[[variante]]==var)])) }
          else { matrice <- c(matrice, length(which((data[[variable]]>Min | (data[[variable]]==Min)*(closed_left | Pass)) & (data[[variable]]<Max | (data[[variable]]==Max)*((closed_right & j!=length(seuils)) | Pass)) & data[[variante]]==var))/length(which(data[[variable]]>=Min_min & data[[variante]]==var))) }
      }
      if (miss) {
        if (weights) { matrice <- c(matrice, sum(data[['weight']][which(is.missing(data[[variable]]) & data[[variante]]==var)])/sum(data[['weight']][which(!is.missing(data[[variable]]) & data[[variante]]==var)])) }
        else { matrice <- c(matrice, length(which(is.missing(data[[variable]]) & data[[variante]]==var))/length(which(!is.missing(data[[variable]]) & data[[variante]]==var))) }
      }
    }
    matrice <- matrix(c(matrice), ncol=length(variantes))    
  }
  return(matrice)
  # return(as.data.frame(matrice))
}
barres <- function(data, file, title="", labels, color, hover=legend, nsp=TRUE, sort=TRUE, legend=hover, showLegend=T, margin_r=0, margin_l=NA, online=FALSE) {
  margin_t <- 0
  if (title!="") { margin_t <- 100 }
  if (grepl("<br>", title)) { margin_t <- 150 }
  legendSize <- 10
  legendY <- 1.1
  legendX <- 0.2
  # legendFont <- 'Open Sans'
  if (is.na(margin_l)) { margin_l <- 4.7*max(nchar(labels)) }
  if (max(nchar(labels)) > 25) { legendSize <- 9 }
  # if (max(nchar(labels)) > 50) { legendSize <- 8 }
  # if (max(nchar(labels)) > 60) { legendSize <- 7 }
  if (max(nchar(labels)) > 50) { # 70
    legendSize <- 11 
    legendY = 1.2
    legendX=1
    margin_t = 170
  }
  if (!showLegend) { margin_t <- margin_t - 70}
  
  if (sort) {
    agree <- c()
    if (nrow(data)==5 | nrow(data)==6) { for (i in 1:length(labels)) { agree <- c(agree, data[4, i] + data[5, i]) } }
    else if (nrow(data)==7) { for (i in 1:length(labels)) { agree <- c(agree, data[6, i] + data[7, i]) } }
    else { for (i in 1:length(labels)) { agree <- c(agree, data[1, i]) } }
    labels <- labels[order(agree)]
    data <- matrix(data[, order(agree)], nrow=nrow(data))
  }
  

  if (nrow(data)==1) {  
    hover <- hover[order(agree)]
    for (i in 1:length(hover)) { hover[i] <- paste(hover[i], "<br>Choisi dans ", round(100*data[1, i]), "% des réponses", sep="")}
    hovers <- matrix(hover, nrow=length(hover))
  }
  else {
    hovers <- c()
    if (nsp) {
      for (i in 1:(length(hover)-1)) { 
          for (j in 1:length(labels)) {
            hovers <- c(hovers, paste(hover[i], '<br>', round(100*data[i, j]/(1+data[length(hover), j])), '% des réponses<br>', round(100*data[i, j]), '% des réponses exprimées') )
          }
      }
      for (j in 1:length(labels)) {
        hovers <- c(hovers, paste(hover[length(hover)], '<br>', round(100*data[length(hover), j]/(1+data[length(hover), j])), '% des réponses<br>') )
      }
    }
    else {
      if (is.element(hover[length(hover)],c("PNR", "NSP"))) hover <- hover[1:(length(hover)-1)]
      if (is.element(legend[length(legend)],c("PNR", "NSP"))) legend <- legend[1:(length(legend)-1)]
      for (i in 1:length(hover)) { 
        for (j in 1:length(labels)) {
          hovers <- c(hovers, paste(hover[i], '<br>', round(100*data[i, j]), '% des réponses exprimées<br>') )
        }
      }  
    }
    hovers <- matrix(hovers, ncol=length(hover))
  }

  bars <- plot_ly(x = data[1,], y = labels, type = 'bar', orientation = 'h', text = hovers[,1], # sort=FALSE, 
            hoverinfo = 'text', name=legend[1], marker = list(color = color[1], line = list(color = 'white', width = 0))) %>%

    layout(xaxis = list(title = "",
                        showgrid = T,
                        showline = FALSE,
                        showticklabels = T,
                        gridcolor = toRGB("gray70"), # + noir, + proche de 0
                        gridwidth = 1,
                        griddash = "dot",
                        autotick = FALSE,
                        ticks = "outside",
                        tick0 = 0,
                        dtick = 0.1,
                        ticklen = 5,
                        tickwidth = 1,
                        tickcolor = toRGB("gray70"),
                        zeroline = T,
                        domain = c(0.15, 1)
                        ),
           yaxis = list(title = "",
                        showgrid = FALSE,
                        showline = FALSE,
                        showticklabels = FALSE,
                        categoryorder = "trace",
                        zeroline = FALSE),
           hovermode = 'closest',
           barmode = 'stack',
           title = title,
           titlefont = list(color='black'),
           font = list(color='black'),
           # paper_bgcolor = 'rgb(248, 248, 255)', plot_bgcolor = 'rgb(248, 248, 255)',
           margin = list(l = margin_l, r = margin_r, t = margin_t, b = 20),
           # margin = list(b = 20, t = margin_t),
           legend = list(orientation='h', y=legendY, x=legendX, traceorder='normal', font=list(size=legendSize, color='black')), # family='Balto',  , family=legendFont
           showlegend = showLegend) %>%
    
    # labeling the y-axis
    add_annotations(xref = 'paper', yref = 'y', x = 0.14, y = labels,
                    xanchor = 'right',
                    text = labels,
                    font = list(family = 'Arial', size = 14, color = 'black'),
                    showarrow = FALSE, align = 'right') # %>%  
  print(nrow(data))
  print(nrow(hovers))
  print(ncol(hovers))
    if (nrow(data)>1) { for (i in 2:nrow(data)) {
      bars <- add_trace(bars, evaluate=TRUE, x = data[i,], name=legend[i], text = hovers[,i], hoverinfo = 'text', marker = list(color = color[i]))
    } }

    # labeling the first Likert scale (on the top)
    # add_annotations(xref = 'x', yref = 'paper',
    #                 x = c(10, 90, 110),
    #                 y = 1.05,
    #                 text = c("Oui", "Non", "NSP"),
    #                 font = list(family = 'Arial', size = 16, color = 'black'),
    #                 showarrow = FALSE) # %>%
    # labeling the percentages of each bar (x_axis)
    # add_annotations(xref = 'x', yref = 'y',
    #                 x = o / 2, y = labels,
    #                 text = paste(data[,"oui"], '%'),
    #                 font = list(family = 'Arial', size = 14, color = 'white'),
    #                 showarrow = FALSE) %>%
    # add_annotations(xref = 'x', yref = 'y',
    #                 x = o + n / 2, y = labels,
    #                 text = paste(data[,"non"], '%'),
    #                 font = list(family = 'Arial', size = 14, color = 'white'),
    #                 showarrow = FALSE) %>%
    # add_annotations(xref = 'x', yref = 'y',
    #                 x = o + n + nsp / 2, y = labels,
    #                 text = paste(data[,"nsp"], '%'),
    #                 font = list(family = 'Arial', size = 14, color = 'white'),
    #                 showarrow = FALSE) %>%
  if (online) { api_create(bars, filename=file, sharing="public") }
  return(bars)
}
CImedian <- function(vec) { # 95% confidence interval
  res <- tryCatch(unlist(ci.median(vec[!is.na(vec) & vec!=-1])), error=function(e) {print('NA')})
  return(paste(res[paste('ci.lower')], res[paste('ci.median')], res[paste('ci.upper')], length(which(!is.na(vec) & vec!=-1)))) }

# vars <- c("actionnaires", "sdf", "proprietaires", "cadres", "rsa", "chomeurs", "etudiants")
# b <- barres(file="test", title="<b>quel beau titre</b><br>ouais", data=data5(vars), color=c(color5, "lightgrey"), legend = c(evol5, "NSP"), labels=vars)
# api_create(b, filename="test", sharing="public")


##### RdB #####
decrit(m$rdb[m$variante_rdb=="gar"], weights = m$weight[m$variante_rdb=="gar"])
decrit(m$rdb[m$variante_rdb=="aid"], weights = m$weight[m$variante_rdb=="aid"])
decrit(m$rdb[m$variante_rdb=="rdb"], weights = m$weight[m$variante_rdb=="rdb"])
decrit(m$rdb[m$variante_rdb=="ass"], weights = m$weight[m$variante_rdb=="ass"])
decrit(m$rdb, weights = m$weight)
plot(density(m$rdb[m$rdb>-1 & m$variante_rdb=="gar" & !is.na(m$rdb)], weights=m$weight[m$rdb>-1 & m$variante_rdb=="gar" & !is.na(m$rdb)]/sum(m$weight[m$rdb>-1 & m$variante_rdb=="gar" & !is.na(m$rdb)]), bw=100), lwd=2, main="Montant désiré du 'revenu de base' (en €/mois)\n(médianes : 800 (aid), 1130 (rdb), 1300 (ass), 1440 (gar), N~412 + NSP~224)", xlab="Montant désiré pour le 'revenu de base' selon différentes formulations (en €/mois)", col="green", xlim=c(0,3000))
lines(density(m$rdb[m$rdb>-1 & m$variante_rdb=="aid" & !is.na(m$rdb)], weights=m$weight[m$rdb>-1 & m$variante_rdb=="aid" & !is.na(m$rdb)]/sum(m$weight[m$rdb>-1 & m$variante_rdb=="aid" & !is.na(m$rdb)]), bw=100), col="red", lty=2, lwd=2)
lines(density(m$rdb[m$rdb>-1 & m$variante_rdb=="ass" & !is.na(m$rdb)], weights=m$weight[m$rdb>-1 & m$variante_rdb=="ass" & !is.na(m$rdb)]/sum(m$weight[m$rdb>-1 & m$variante_rdb=="ass" & !is.na(m$rdb)]), bw=100), col="blue", lty=3, lwd=2)
lines(density(m$rdb[m$rdb>-1 & m$variante_rdb=="rdb" & !is.na(m$rdb)], weights=m$weight[m$rdb>-1 & m$variante_rdb=="rdb" & !is.na(m$rdb)]/sum(m$weight[m$rdb>-1 & m$variante_rdb=="rdb" & !is.na(m$rdb)]), bw=100), col="purple", lty=4, lwd=2)
abline(v=seq(0,3000, by=250), lty=3, col="grey")
grid()
legend("topright", title="variante_rdb : formulation de la question", title.col="black", legend=c("gar : minimal garanti à tous","aid : aides État pour sans revenus", "ass : État devrait assurer à chacun", "rdb : revenu de base"),
  text.col=c("green","red", "blue", "purple"),lwd=2 ,col=c("green","red", "blue", "purple"),lty=1:4)
  # e$variante_rdb <<- factor(e$variante_rdb, c("aid","rdb","ass","gar")) # c("gar","ass","rdb","aid")
# ggplot(m, aes(x=ifelse(rdb>-1 & rdb<2501, rdb, NA), y=..density..)) + stat_density(aes(fill=variante_rdb), position="stack", adjust=1.3) + xlab("Montant désiré pour le 'revenu de base' selon différentes formulations (en €/mois)") + ggtitle("Montant désiré du 'revenu de base' (en €/mois)\n(médianes : 750 (aid), 1000 (rdb), 1200 (ass), 1400 (gar), N=1248 + NSP=498)") + grid()
# ggplot(e, aes(x=ifelse(rdb>-1, rdb, NA), fill=variante_rdb)) +  geom_histogram(stat ="bin", binwidth=200)
# plot(density(e$rdb[e$rdb>-1 & e$variante_rdb=="gar" & !is.na(e$rdb)], na.rm=T), main=NA, xlab="Montant désiré pour le 'revenu de base' selon différentes formulations (en €/mois)", col="blue", xlim=c(0,3000))
# lines(density(p$rdb[p$rdb>-1 & p$variante_rdb=="gar" & !is.na(p$rdb)], na.rm=T), col="blue")
# lines(density(e$rdb[e$rdb>-1 & e$variante_rdb=="aid" & !is.na(e$rdb)], na.rm=T), col="red")
# lines(density(p$rdb[p$rdb>-1 & p$variante_rdb=="aid" & !is.na(p$rdb)], na.rm=T), col="red")
# lines(density(e$rdb[e$rdb>-1 & e$variante_rdb=="ass" & !is.na(e$rdb)], na.rm=T), col="green")
# lines(density(p$rdb[p$rdb>-1 & p$variante_rdb=="ass" & !is.na(p$rdb)], na.rm=T), col="green")
# lines(density(e$rdb[e$rdb>-1 & e$variante_rdb=="rdb" & !is.na(e$rdb)], na.rm=T), col="purple")
# lines(density(p$rdb[p$rdb>-1 & p$variante_rdb=="rdb" & !is.na(p$rdb)], na.rm=T), col="purple")
# legend("topright", title="variante_rdb", title.col="black", legend=c("gar","aid", "ass", "rdb"),
#   text.col=c("blue","red", "green", "purple"),lwd=c(1),col=c("blue","red", "green", "purple"))
decrit(m$quel_rdb, weights=m$weight)
data_rdb <- as.data.frame(matrix( c("RdB à 500€/m","RdB à 700€/m","RdB à 850€/m","RdB à 1000€/m","RdB à 1000€/m remplaçant le chômage","RdB à 700€/m remplaçant le chômage","RdB à 1000€/m remplaçant le chômage et les retraites","Maintien du système actuel","Système actuel en baissant les minima sociaux","Système actuel en augmentant le RSA","NSP",2,17,5,18,5,6,3,7,15,3,19), ncol=2))
colors_rdb <- c('yellow', 'lightgreen', 'green', 'darkgreen', 'lightblue', 'blue', 'purple', 'orange', 'red', 'pink', 'lightgrey')
do_pie(Data=data_rdb, Colors=colors_rdb, File="pie_rdb", Title="Modalités préférées concernant la protection sociale", Hover = c("L'instauration d'un revenu de base de 500€/mois","L'instauration d'un revenu de base de 700€/mois, pour simplifier le système actuel","L'instauration d'un revenu de base de 850€/mois","L'instauration d'un revenu de base de 1000€/mois, pour vraiment redistribuer les richesses","L'instauration d'un revenu de base de 1000€/mois qui remplacerait aussi les allocations chômage","L'instauration d'un revenu de base de 700€/mois qui remplacerait aussi les allocations chômage","L'instauration d'un revenu de base de 1000€/mois qui remplacerait aussi les allocations chômage et les retraites","Le maintien du système actuel","Le maintien du système actuel mais en baissant le montant des minima sociaux (RSA, APL...)","Le maintien du système actuel mais en augmentant le RSA","NSP (Ne sait pas, ne se prononce pas)"), Display_values=TRUE)

data_rdb <- function(data=t, n=5, miss=T, weights=T) {
  Min <- -1
  matrice <- c()
  if (n==4) variantes <- c("aid", "rdb", "ass", "gar")
  else if (n>=5) variantes <- c("aid", "leg", "rdb", "ass", "gar")
  for (var in variantes) {
    if (miss) {
      mat <- c(length(which(data[['rdb']]>Min & data[['variante_rdb']]==var & data[['rdb']]<=450))/length(which(data[['rdb']]>Min & data[['variante_rdb']]==var)), length(which(data[['rdb']]>450 & data[['variante_rdb']]==var & data[['rdb']]<=600))/length(which(data[['rdb']]>Min & data[['variante_rdb']]==var)), length(which(data[['rdb']]>Min & data[['variante_rdb']]==var & data[['rdb']]>600 & data[['rdb']]<=900))/length(which(data[['rdb']]>Min & data[['variante_rdb']]==var)), length(which(data[['rdb']]>900 & data[['variante_rdb']]==var & data[['rdb']]<=1250))/length(which(data[['rdb']]>Min & data[['variante_rdb']]==var)), length(which(data[['rdb']]>Min & data[['variante_rdb']]==var & data[['rdb']]>1250 & data[['rdb']]<=1700))/length(which(data[['rdb']]>Min & data[['variante_rdb']]==var)), length(which(data[['rdb']]>Min & data[['variante_rdb']]==var & data[['rdb']]>=1700))/length(which(data[['rdb']]>Min & data[['variante_rdb']]==var)),length(which(is.missing(data[['rdb']]) & data[['variante_rdb']]==var))/length(which(!is.missing(data[['rdb']]) & data[['variante_rdb']]==var)))
      if (weights) { mat <- c(sum(data[['weight']][which(data[['rdb']]>Min & data[['variante_rdb']]==var & data[['rdb']]<=450)])/sum(data[['weight']][which(data[['rdb']]>Min & data[['variante_rdb']]==var)]), sum(data[['weight']][which(data[['rdb']]>450 & data[['variante_rdb']]==var & data[['rdb']]<=600)])/sum(data[['weight']][which(data[['rdb']]>Min & data[['variante_rdb']]==var)]), sum(data[['weight']][which(data[['rdb']]>Min & data[['variante_rdb']]==var & data[['rdb']]>600 & data[['rdb']]<=900)])/sum(data[['weight']][which(data[['rdb']]>Min & data[['variante_rdb']]==var)]), sum(data[['weight']][which(data[['rdb']]>900 & data[['variante_rdb']]==var & data[['rdb']]<=1250)])/sum(data[['weight']][which(data[['rdb']]>Min & data[['variante_rdb']]==var)]), sum(data[['weight']][which(data[['rdb']]>Min & data[['variante_rdb']]==var & data[['rdb']]>1250 & data[['rdb']]<=1700)])/sum(data[['weight']][which(data[['rdb']]>Min & data[['variante_rdb']]==var)]), sum(data[['weight']][which(data[['rdb']]>1700 & data[['variante_rdb']]==var)])/sum(data[['weight']][which(data[['rdb']]>Min & data[['variante_rdb']]==var)]), sum(data[['weight']][which(is.missing(data[['rdb']]) & data[['variante_rdb']]==var)])/sum(data[['weight']][which(!is.missing(data[['rdb']]) & data[['variante_rdb']]==var)])) }
    }
    else {
      mat <- c(length(which(data[['rdb']]>Min & data[['variante_rdb']]==var & data[['rdb']]<=450))/length(which(data[['rdb']]>Min & data[['variante_rdb']]==var)), length(which(data[['rdb']]>450 & data[['variante_rdb']]==var & data[['rdb']]<=600))/length(which(data[['rdb']]>Min & data[['variante_rdb']]==var)), length(which(data[['rdb']]>Min & data[['variante_rdb']]==var & data[['rdb']]>600 & data[['rdb']]<=900))/length(which(data[['rdb']]>Min & data[['variante_rdb']]==var)), length(which(data[['rdb']]>900 & data[['variante_rdb']]==var & data[['rdb']]<=1250))/length(which(data[['rdb']]>Min & data[['variante_rdb']]==var)),length(which(data[['rdb']]>Min & data[['variante_rdb']]==var & data[['rdb']]>1250 & data[['rdb']]<=1700))/length(which(data[['rdb']]>Min & data[['variante_rdb']]==var)),  length(which(data[['rdb']]>Min & data[['variante_rdb']]==var & data[['rdb']]>1700 ))/length(which(data[['rdb']]>Min & data[['variante_rdb']]==var)))    
      if (weights) { mat <- c(sum(data[['weight']][which(data[['rdb']]>Min & data[['variante_rdb']]==var & data[['rdb']]<=450)])/sum(data[['weight']][which(data[['rdb']]>Min & data[['variante_rdb']]==var)]), sum(data[['weight']][which(data[['rdb']]>450 & data[['variante_rdb']]==var & data[['rdb']]<=600)])/sum(data[['weight']][which(data[['rdb']]>Min & data[['variante_rdb']]==var)]), sum(data[['weight']][which(data[['rdb']]>Min & data[['variante_rdb']]==var & data[['rdb']]>600 & data[['rdb']]<=900)])/sum(data[['weight']][which(data[['rdb']]>Min & data[['variante_rdb']]==var)]),sum(data[['weight']][which(data[['rdb']]>900 & data[['variante_rdb']]==var & data[['rdb']]<=1250)])/sum(data[['weight']][which(data[['rdb']]>Min & data[['variante_rdb']]==var)]), sum(data[['weight']][which(data[['rdb']]>Min & data[['variante_rdb']]==var & data[['rdb']]>1250 & data[['rdb']]<=1700)])/sum(data[['weight']][which(data[['rdb']]>Min & data[['variante_rdb']]==var)]), sum(data[['weight']][which(data[['rdb']]>1700 & data[['variante_rdb']]==var)])/sum(data[['weight']][which(data[['rdb']]>Min & data[['variante_rdb']]==var)])) } }
    matrice <- c(matrice, mat) }
  if (n==6) {
    if (miss) {
      mat <- c(length(which(data[['smic']]>Min & data[['smic']]<=450))/length(which(data[['smic']]>Min )), length(which(data[['smic']]>450  & data[['smic']]<=600))/length(which(data[['smic']]>Min )), length(which(data[['smic']]>Min  & data[['smic']]>600 & data[['smic']]<=900))/length(which(data[['smic']]>Min )), length(which(data[['smic']]>900  & data[['smic']]<=1250))/length(which(data[['smic']]>Min )), length(which(data[['smic']]>Min  & data[['smic']]>1250 & data[['smic']]<=1700))/length(which(data[['smic']]>Min )), length(which(data[['smic']]>Min  & data[['smic']]>=1700))/length(which(data[['smic']]>Min )),length(which(is.missing(data[['smic']] & !is.na(data[['smic']])) ))/length(which(!is.missing(data[['smic']]) )))
      if (weights) { mat <- c(sum(data[['weight']][which(data[['smic']]>Min  & data[['smic']]<=450)])/sum(data[['weight']][which(data[['smic']]>Min )]), sum(data[['weight']][which(data[['smic']]>450  & data[['smic']]<=600)])/sum(data[['weight']][which(data[['smic']]>Min )]), sum(data[['weight']][which(data[['smic']]>Min  & data[['smic']]>600 & data[['smic']]<=900)])/sum(data[['weight']][which(data[['smic']]>Min )]), sum(data[['weight']][which(data[['smic']]>900  & data[['smic']]<=1250)])/sum(data[['weight']][which(data[['smic']]>Min )]), sum(data[['weight']][which(data[['smic']]>Min  & data[['smic']]>1250 & data[['smic']]<=1700)])/sum(data[['weight']][which(data[['smic']]>Min )]), sum(data[['weight']][which(data[['smic']]>1700 )])/sum(data[['weight']][which(data[['smic']]>Min )]), sum(data[['weight']][which(is.missing(data[['smic']]) & !is.na(data[['smic']]))])/sum(data[['weight']][which(!is.missing(data[['smic']]) )])) }
    }
    else {
      mat <- c(length(which(data[['smic']]>Min  & data[['smic']]<=450))/length(which(data[['smic']]>Min )), length(which(data[['smic']]>450  & data[['smic']]<=600))/length(which(data[['smic']]>Min )), length(which(data[['smic']]>Min  & data[['smic']]>600 & data[['smic']]<=900))/length(which(data[['smic']]>Min )), length(which(data[['smic']]>900  & data[['smic']]<=1250))/length(which(data[['smic']]>Min )),length(which(data[['smic']]>Min  & data[['smic']]>1250 & data[['smic']]<=1700))/length(which(data[['smic']]>Min )),  length(which(data[['smic']]>Min  & data[['smic']]>1700 ))/length(which(data[['smic']]>Min )))    
      if (weights) { mat <- c(sum(data[['weight']][which(data[['smic']]>Min  & data[['smic']]<=450)])/sum(data[['weight']][which(data[['smic']]>Min )]), sum(data[['weight']][which(data[['smic']]>450  & data[['smic']]<=600)])/sum(data[['weight']][which(data[['smic']]>Min )]), sum(data[['weight']][which(data[['smic']]>Min  & data[['smic']]>600 & data[['smic']]<=900)])/sum(data[['weight']][which(data[['smic']]>Min )]),sum(data[['weight']][which(data[['smic']]>900  & data[['smic']]<=1250)])/sum(data[['weight']][which(data[['smic']]>Min )]), sum(data[['weight']][which(data[['smic']]>Min  & data[['smic']]>1250 & data[['smic']]<=1700)])/sum(data[['weight']][which(data[['smic']]>Min )]), sum(data[['weight']][which(data[['smic']]>1700 )])/sum(data[['weight']][which(data[['smic']]>Min )])) } }
    matrice <- c(matrice, mat) }
  matrice <- matrix(c(matrice), ncol=n)
  return(matrice)
  # return(as.data.frame(matrice))
}
barres(data=data_rdb(), file="demogrant", labels=c("social aid for those with no income", "social aid for single pers. >25y w/o income", "basic income", "how much the State should insure to all", "minimum income guaranteed to all"), color=c('darkred', color5 ,'lightgrey'), hover=c("Less than 450", "451 to 600", "601 to 900", "901 to 1250", "1250 to 1700", "More than 1700", "PNR"), nsp=TRUE, sort=FALSE, legend=c("Less than 450", "451 to 600", "601 to 900", "901 to 1250", "1250 to 1700", "More than 1700", "PNR"), showLegend=T, margin_r=0, margin_l=NA)
barres(data=data_rdb(), file="rdb", title="<b>Demogrant</b><br>Desired amount depending on the formulation (en €/mois)", labels=c("aides de l'État pour les sans revenus", "aides de l'É pour pers. seules >25a sans revenu", "revenu de base (≈RSA)", "ce que l'État devrait assurer à chacun", "montant minimal garanti à tous"), color=c('darkred', color5 ,'lightgrey'), hover=c("Moins de 450", "De 451 à 600", "De 601 à 900", "De 901 à 1250", "De 1250 à 1700", "Plus de 1700", "NSP"), nsp=TRUE, sort=FALSE, legend=c("Moins de 450", "De 451 à 600", "De 601 à 900", "De 901 à 1250", "De 1250 à 1700", "Plus de 1700", "NSP"), showLegend=T, margin_r=0, margin_l=NA)
barres(data=data_rdb(n=6), file="demogrant", labels=c("social aid for those with no income", "social aid for single pers. >25y w/o income", "basic income", "how much the State should insure to all", "minimum income guaranteed to all", "after tax income at minimum wage"), color=c('darkred', color5 ,'lightgrey'), hover=c("Less than 450", "451 to 600", "601 to 900", "901 to 1250", "1250 to 1700", "More than 1700", "PNR"), nsp=TRUE, sort=FALSE, legend=c("Less than 450", "451 to 600", "601 to 900", "901 to 1250", "1250 to 1700", "More than 1700", "PNR"), showLegend=T, margin_r=0, margin_l=NA)
barres(data=data_rdb(n=6), file="rdb", title="<b>Demogrant</b><br>Desired amount depending on the formulation (en €/mois)", labels=c("aides de l'État pour les sans revenus", "aides de l'É pour pers. seules >25a sans revenu", "revenu de base (≈RSA)", "ce que l'État devrait assurer à chacun", "montant minimal garanti à tous", "SMIC après impôt pour pers. seule"), color=c('darkred', color5 ,'lightgrey'), hover=c("Moins de 450", "De 451 à 600", "De 601 à 900", "De 901 à 1250", "De 1250 à 1700", "Plus de 1700", "NSP"), nsp=TRUE, sort=FALSE, legend=c("Moins de 450", "De 451 à 600", "De 601 à 900", "De 901 à 1250", "De 1250 à 1700", "Plus de 1700", "NSP"), showLegend=T, margin_r=0, margin_l=NA)
barres(data=data_rdb(data=m, n=4), file="demogrant", labels=c("social aid for those with no income", "basic income", "how much the State should insure to all", "minimum income guaranteed to all"), color=c('darkred', color5 ,'lightgrey'), hover=c("Less than 450", "451 to 600", "601 to 900", "901 to 1250", "1250 to 1700", "More than 1700", "PNR"), nsp=TRUE, sort=FALSE, legend=c("Less than 450", "451 to 600", "601 to 900", "901 to 1250", "1250 to 1700", "More than 1700", "PNR"), showLegend=T, margin_r=0, margin_l=NA)
barres(data=data_rdb(data=m, n=4), file="rdb", title="<b>Demogrant</b><br>Desired amount depending on the formulation (en €/mois)", labels=c("aides de l'État pour les sans revenus", "revenu de base (≈RSA)", "ce que l'État devrait assurer à chacun", "montant minimal garanti à tous"), color=c('darkred', color5 ,'lightgrey'), hover=c("Moins de 450", "De 451 à 600", "De 601 à 900", "De 901 à 1250", "De 1250 à 1700", "Plus de 1700", "NSP"), nsp=TRUE, sort=FALSE, legend=c("Moins de 450", "De 451 à 600", "De 601 à 900", "De 901 à 1250", "De 1250 à 1700", "Plus de 1700", "NSP"), showLegend=T, margin_r=0, margin_l=NA)
decrit(m$quelle_redistribution, weights=m$weight)
summary(lm((quelle_redistribution=="Une augmentation des minima sociaux (RSA)") ~ csp, data=m, weights=weight))
data_quelle_redistr <- as.data.frame(matrix(c("Je ne souhaite pas une réduction des inégalités<br>13%", "Une augmentation des minima sociaux (RSA)<br>7%", "Une augmentation du SMIC et des bas salaires<br>65%", "NSP<br>16%", 13, 7, 65, 16), ncol=2))
do_pie(Data=data_quelle_redistr, Colors=c("blue", "red", "green", "lightgrey"), File="pie_quelle_redistribution", Title="Pour réduire les inégalités, préférez-vous ... ?", Display_values = FALSE)

# formulation f$rdb_aid: Combien devrait recevoir, en France, une personne seule (de plus de 25 ans) qui ne touche que les aides de l'État ?
# formulation m$rdb_aid: Quel devrait être le montant des aides de l'État pour les gens qui n'ont aucun revenu ?
summary(lm(rdb ~ 0 + variante_rdb, data=t, weights=t$weight, subset=!is.missing(t$rdb)))
decrit(t$rdb[t$variante_rdb=='aid'], weights = t$weight[t$variante_rdb=='aid'])
decrit(t$rdb[t$variante_rdb=='leg'], weights = t$weight[t$variante_rdb=='leg'])
summary(lm(rdb ~ (variante_rdb=='leg'), data=t, subset=(t$variante_rdb=='aid' | t$variante_rdb=='leg') & !is.missing(t$rdb), weights=t$weight))
mood.test(rdb ~ (variante_rdb=='leg'), data=t, subset=(t$variante_rdb=='aid' | t$variante_rdb=='leg') & !is.missing(t$rdb), weights=t$weight, alternative="t") # Median are also different

##### Dés/avantager ######
# plot(density(as.numeric(V("desavantager")), na.rm=T, bw=1)) # desavantage
# plot(density(as.numeric(V("avantager")), na.rm=T)) # avantage
decrit(m$desavantager, weights = m$weight)
decrit(me$desavantager, weights = me$weight)
decrit(ma$desavantager, weights = ma$weight) 
decrit(t$desavantager, weights = t$weight)
decrit(ta$desavantager, weights = ta$weight) # même quartiles dans les 6 cas
summary(lm(as.numeric(t$avantager) ~ I(pmin(t$revdisp,4500)/10^3) + t$gauche_droite, weights=t$weight)) # revenu non corrélé (pas non plus pour niveau_vie), même sans gauche_droite
summary(lm(as.numeric(t$desavantager) ~ I(pmin(t$revdisp,4500)/10^3), weights=t$weight)) #  corrélé (pareil pour niveau_vie), effet issu des hauts revenus
summary(lm(as.numeric(t$rdb) ~ I(pmin(t$revdisp,4500)/10^3), weights=t$weight)) #  non corrélé (pareil pour niveau_vie), même si on se restreint à variante "aid"
plot(as.numeric(m$avantager), I(pmin(m$revdisp,4500)/10^3))
plot(density(m$desavantager[m$desavantager>-1 & m$desavantager<55 & !is.na(m$desavantager)], bw=1, weights = m$weight[m$desavantager>-1 & m$desavantager<55 & !is.na(m$desavantager)]), main="Proportion à désavantager\n(quartiles : 3-10-20, N=831 + NSP=776)", xlab="Pourcentage de français à désavantager lors d'une réforme fiscale", xlim=c(0,55))
grid()
abline(v=seq(0,55,by=2.5), col="grey",lty=3)
# plot(density(er$desavantager[er$desavantager>-1 & er$desavantager<55], na.rm=T, bw=1), main="Proportion à désavantager", xlab="Pourcentage de français (les plus riches) à désavantager lors d'une réforme fiscale", xlim=c(0,55))
# lines(density(pr$desavantager[pr$desavantager>-1 & pr$desavantager<55], na.rm=T, bw=1), main="Proportion à désavantager", xlab="Pourcentage de français (les plus riches) à désavantager lors d'une réforme fiscale", xlim=c(0,55))
decrit(e$avantager, weights = e$weight)
decrit(m$avantager, weights = m$weight)
decrit(ma$avantager, weights = ma$weight)
decrit(t$avantager, weights = t$weight)
decrit(ta$avantager, weights = ta$weight)
decrit(ma$avantager, weights = ma$weight) # même quartiles dans les 4 cas
plot(density(m$avantager[m$avantager>-1 & !is.na(m$avantager)],  weights = m$weight[m$avantager>-1 & !is.na(m$avantager)], bw=3), main="Proportion à avantager\n(quartiles : 30-50-65, N=395 + NSP=341)", xlab="Pourcentage de français à avantager lors d'une réforme fiscale", xlim=c(0,100))
grid()
abline(v=seq(0,100,by=10), col="grey",lty=3)
# plot(density(er$avantager[er$avantager>-1], na.rm=T, bw=1), main="Proportion à avantager", xlab="Pourcentage de français (les plus riches) à désavantager lors d'une réforme fiscale", xlim=c(0,100), col="red")
# lines(density(pr$avantager[pr$avantager>-1], na.rm=T, bw=1), main="Proportion à avantager", xlab="Pourcentage de français (les plus riches) à désavantager lors d'une réforme fiscale", xlim=c(0,100))

barres(data=data_seuils(data=t, variable=c("desavantager", "avantager"), miss=FALSE, closed_left=FALSE, closed_right=T, seuils=c(2, 8, 10, 20, 40, 60)), file="des_avantager", title="<b>Proportion de Français à dés/avantager lors d'une réforme fiscale (en %)</b>", labels=c("Pourcentage à désavantager", "Pourcentage à avantager"), color=c('darkred', rainbow(5, end=5/15), 'forestgreen'), hover=c("Moins de 2%", "De 3% à 8%", "De 9 à 10%", "De 11% à 20%", "De 21% à 40%", "De 41% à 60%", "Plus de 60%"), nsp=FALSE, sort=FALSE, showLegend=T, margin_r=0, margin_l=NA)
barres(data=data_seuils(data=t, variable=c("desavantager", "avantager"), miss=FALSE, closed_left=FALSE, closed_right=T, seuils=c(2, 8, 10, 20, 40, 60)), file="dis_advantage", labels=c("Percentage to disadvantage", "Percentage to advantage"), color=c('darkred', rainbow(5, end=5/15), 'forestgreen'), hover=c("Less than 2%", "3% to 8%", "9 to 10%", "11% to 20%", "21% to 40%", "41% to 60%", "More than 60%"), nsp=FALSE, sort=FALSE, showLegend=T, margin_r=0, margin_l=NA)
barres(data=data_seuils(data=t, variable=c("desavantager", "avantager"), miss=T, closed_left=FALSE, closed_right=T, seuils=c(2, 8, 10, 20, 40, 60)), file="des_avantager_miss", title="<b>Proportion de Français à dés/avantager lors d'une réforme fiscale (en %)</b>", labels=c("Pourcentage à désavantager", "Pourcentage à avantager"), color=c('darkred', rainbow(5, end=5/15), 'forestgreen', 'lightgrey'), hover=c("Moins de 2%", "De 3% à 8%", "De 9 à 10%", "De 11% à 20%", "De 21% à 40%", "De 41% à 60%", "Plus de 60%", "NSP"), nsp=T, sort=FALSE, showLegend=T, margin_r=0, margin_l=NA)
barres(data=data_seuils(data=t, variable=c("desavantager", "avantager"), miss=T, closed_left=FALSE, closed_right=T, seuils=c(2, 8, 10, 20, 40, 60)), file="dis_advantage_miss", labels=c("Percentage to disadvantage", "Percentage to advantage"), color=c('darkred', rainbow(5, end=5/15), 'forestgreen', 'lightgrey'), hover=c("Less than 2%", "3% to 8%", "9 to 10%", "11% to 20%", "21% to 40%", "41% to 60%", "More than 60%", "PNR"), nsp=T, sort=FALSE, showLegend=T, margin_r=0, margin_l=NA)

#### Taux max #####
decrit(m$taux_max, weights = m$weight)
decrit(m$vague[!is.na(m$taux_max)])
decrit(m$variante_max[!is.na(m$taux_max)])
decrit(m$taux_max[m$variante_max=="taux_s"], weights = m$weight[m$variante_max=="taux_s"])
decrit(m$taux_max[m$variante_max=="taux_c"], weights = m$weight[m$variante_max=="taux_c"])
plot(density(m$taux_max[m$variante_max=="taux_s" & !is.na(m$taux_max) & !is.missing(m$taux_max)], weights = m$weight[m$variante_max=="taux_s" & !is.na(m$taux_max)]/sum(m$weight[m$variante_max=="taux_s" & !is.na(m$taux_max)]), bw=10), lwd=2, main="Taux d'imposition pour 3 millions €/an\n(quartiles : 30-41-60%, N~250 + NSP~22)", xlab="Taux d'imposition désiré pour un revenu annuel de 3 millions d'euros (en %)", col="green", xlim=c(0,100), ylim=c(0,0.025))
lines(density(m$taux_max[m$variante_max=="taux_c" & !is.na(m$taux_max) & !is.missing(m$taux_max)], weights = m$weight[m$variante_max=="taux_c" & !is.na(m$taux_max)]/sum(m$weight[m$variante_max=="taux_c" & !is.na(m$taux_max)]), bw=10), col="purple", lty=2, lwd=2)
lines(density(m$taux_max[m$variante_max=="d"  & !is.na(m$taux_max) & !is.missing(m$taux_max)], weights = m$weight[m$variante_max=="d"  & !is.na(m$taux_max) & !is.missing(m$taux_max)]/sum(m$weight[m$variante_max=="d" & !is.na(m$taux_max) & !is.missing(m$taux_max)]), bw=10), col="red", lty=3, lwd=2)
lines(density(m$taux_max[m$variante_max=="t" & !is.na(m$taux_max) & !is.missing(m$taux_max)], weights = m$weight[m$variante_max=="t" & !is.na(m$taux_max) & !is.missing(m$taux_max)]/sum(m$weight[m$variante_max=="t" & !is.na(m$taux_max) & !is.missing(m$taux_max)]), bw=10), col="blue", lty=4, lwd=2)
grid()
abline(v=seq(0,100,by=10), col="grey",lty=3)
legend("topright", title="variante_max", title.col="black", legend=c("taux_s: simple", "taux_c: +argument", "anti-impôt", "d: + rentier", "t: + trader"),
  text.col=c("green", "purple", "purple", "red", "blue"),lwd=2,col=c("green", "purple", NA, "red", "blue"),lty=c(1:2,NA,3:4))
# TODO: quartiles titre
  # e$variante_rdb <<- factor(e$variante_rdb, c("aid","rdb","ass","gar")) # c("gar","ass","rdb","aid")
# ggplot(mr, aes(x=ifelse(variante_max!="", taux_max, NA), y=..density..)) + stat_density(aes(fill=variante_max), position="stack", adjust=1.3) + xlab("Taux d'imposition désiré pour un revenu annuel de 3 millions d'euros (en %)") + ggtitle("Taux d'imposition pour 3 millions €/an\n(quartiles : 30-41-60%, N=1003 + NSP=89)") + grid()
summary(lm(taux_max ~ 0 + variante_max, data=m, subset=(!is.missing(taux_max) & (variante_max=="taux_s" | variante_max=="taux_c" | variante_max=="d" | variante_max=="t") ), weights=weight))

barres(data=data_seuils(variable="taux_max", variante="variante_max", miss=FALSE, order=rev(c(2,1,3,4))), file="taux_max", title="<b>Taux d'imposition pour 3 millions d'euros de revenus annuels</b><br>Taux (moyen) d'imposition désiré selon la formulation de la question (en %)", labels=rev(c("Formumation simple (s)", "s + argument anti-impôt", "pour un rentier", "pour un trader")), color=color5, hover=c("Moins de 19%", "De 20% à 39%", "De 40% à 59%", "De 60% à 79%", "Plus de 80%"), nsp=FALSE, sort=FALSE, showLegend=T, margin_r=0, margin_l=NA)
barres(data=t(matrix(c(t(data_seuils(variable="taux_max", variante="variante_max", miss=FALSE, order=rev(c(2,1,3,4)))), c(0.123,0.173,0.145,0.208) ), ncol=6)), file="taux_max_nsp", title="<b>Taux d'imposition pour 3 millions d'euros de revenus annuels</b><br>Taux (moyen) d'imposition désiré selon la formulation de la question (en %)", labels=rev(c("Formumation simple (s)", "s + argument anti-impôt", "pour un rentier", "pour un trader")), color=c(color5, 'lightgrey'), hover=c("Moins de 19%", "De 20% à 39%", "De 40% à 59%", "De 60% à 79%", "Plus de 80%", "NSP"), nsp=TRUE, sort=FALSE, showLegend=T, margin_r=0, margin_l=NA)

#### Revenu max #####
decrit(m$revenu_max, weights=m$weight)
decrit(ep$revenu_max[ep$variante_max=="max_c"], weights=ep$weight[ep$variante_max=="max_c"])
decrit(ep$revenu_max_ideal, weights=ep$weight)
decrit(m$revenu_max_ideal, weights=m$weight)
sum(m$weight[m$revenu_max_ideal==Inf & !is.missing(m$revenu_max_ideal)])/sum(m$weight[!is.missing(m$revenu_max_ideal)])
sum(m$weight[m$revenu_max==Inf & !is.missing(m$revenu_max) & (m$variante_max=="max_s" | m$vague==2)])/sum(m$weight[(m$variante_max=="max_s" | m$vague==2) & !is.missing(m$revenu_max)])
sum(ep$weight[ep$revenu_max==Inf & !is.missing(ep$revenu_max) & (ep$variante_max=="max_s" | ep$vague==2)])/sum(ep$weight[(ep$variante_max=="max_s" | ep$vague==2) & !is.missing(ep$revenu_max)])
sum(ep$weight[ep$revenu_max==Inf & !is.missing(ep$revenu_max) & ep$variante_max=="max_c"])/sum(ep$weight[ep$variante_max=="max_c"& !is.missing(ep$revenu_max)])
sum(m$weight[m$revenu_max==Inf & !is.missing(m$revenu_max) & m$variante_max=="max_c"])/sum(m$weight[!is.missing(m$revenu_max) & m$variante_max=="max_c"])
m$revenu_max_cap <- m$revenu_max*(m$revenu_max<Inf)
m$revenu_max_cap[m$revenu_max==Inf] <- 999999999
summary(lm(log10(revenu_max) ~ (variante_max=="max_c"), data=m, weights=weight, subset=(revenu_max!=Inf & !is.missing(revenu_max)))) # **
summary(lm(log10(revenu_max_cap) ~ (variante_max=="max_c"), data=m, weights=weight, subset=(!is.missing(revenu_max)))) # **
mood.test(revenu_max_cap ~ (variante_max=="max_c"), data=m, weights=m$weight, alternative="g")
mood.test(revenu_max_cap ~ (variante_max=="max_c"), data=m, weights=m$weight, subset=(revenu_max!=Inf), alternative="g")
decrit(m$revenu_max, weights=m$weight)
decrit(m$vague[!is.na(m$revenu_max)])
decrit(m$variante_max[!is.na(m$revenu_max)])
decrit(ep$revenu_max[ep$variante_max=="max_s"])
decrit(ep$revenu_max[mp$variante_max=="max_s"], weights=mp$weight[mp$variante_max=="max_s"])
decrit(m$revenu_max[m$variante_max=="max_s" | m$vague==2], weights=m$weight[m$variante_max=="max_s" | m$vague==2])
decrit(m$revenu_max[m$variante_max=="max_c"], weights=m$weight[m$variante_max=="max_c"]) # 20% inférieur du fait des pondérations
decrit(m$revenu_max_fr_100k, miss=T)
decrit(m$revenu_max_fr_100k, miss=T, weights=m$weight)
decrit(m$revenu_max_fr_3m, miss=T)
decrit(m$revenu_max_fr_3m, miss=T, weights=m$weight)
# densités variantes s et c
plot(density(pmin(m$revenu_max[(m$variante_max=="max_s"| m$vague==2) & m$revenu_max>-1 & !is.na(m$revenu_max)],150000), weights=m$weight[(m$variante_max=="max_s"| m$vague==2) & m$revenu_max>-1 & !is.na(m$revenu_max)]/sum(m$weight[(m$variante_max=="max_s"| m$vague==2) & m$revenu_max>-1 & !is.na(m$revenu_max)]), bw=2000), main="Revenu maximal désiré (en €/mois)\n(quartiles : 10k-250k-Inf (s) / 5k-20k-Inf (c), N=470/216 + NSP=130/45)", xlab="Revenu maximal légal désiré (en €/mois ; Inf : désapprobation de l'instauration d'un revenu maximal)", col="blue", lwd=2, xlim=c(0,160000), axes=FALSE, frame.plot=TRUE)
lines(density(pmin(m$revenu_max[m$variante_max=="max_c" & m$revenu_max>-1 & !is.na(m$revenu_max)],150000), weights=m$weight[m$variante_max=="max_c" & m$revenu_max>-1 & !is.na(m$revenu_max)]/sum(m$weight[m$variante_max=="max_c" & m$revenu_max>-1 & !is.na(m$revenu_max)]), bw=2000), col="purple", lty=2, lwd=2)
abline(v=seq(0,130000,by=10000), col="grey",lty=3)
axis(2)
axis(1, at=c(0,10000,50000,100000,150000), labels=c("0","10k","50k","100k","Inf"))
legend("topleft", title="variante_max", title.col="black", legend=c("max_s: simple", "max_c: +argument anti-impôt"),
  text.col=c("blue", "purple"),lwd=2,lty=c(1,2),col=c("blue", "purple"))
  # e$variante_rdb <<- factor(e$variante_rdb, c("aid","rdb","ass","gar")) # c("gar","ass","rdb","aid")
# ggplot(m, aes(x=ifelse(variante_max!="", pmin(revenu_max,150000), NA), y=..density..)) + stat_density(aes(fill=variante_max), position="stack", adjust=1.3) + xlab("Revenu maximal légal désiré (en €/mois) ; Inf : désapprobation de l'instauration d'un revenu maximal") + grid()
# densités variantes s, c et idéal
plot(density(pmin(m$revenu_max[(m$variante_max=="max_s"| m$vague==2) & m$revenu_max>-1 & !is.na(m$revenu_max)],150000), weights=m$weight[(m$variante_max=="max_s"| m$vague==2) & m$revenu_max>-1 & !is.na(m$revenu_max)]/sum(m$weight[(m$variante_max=="max_s"| m$vague==2) & m$revenu_max>-1 & !is.na(m$revenu_max)]), bw=2000), main="Revenu maximal désiré (en €/mois)\n(quartiles : 11k-250k-Inf (s) / 5k-20k-Inf (c) / 5k-15k-800k (ideal), N=470/216/466 + NSP=130/45/196)", xlab="Revenu maximal désiré (en €/mois ; Inf : désapprobation du plafonnement des revenus)", col="blue", lwd=2, xlim=c(0,160000), axes=FALSE, frame.plot=TRUE)
lines(density(pmin(m$revenu_max[m$variante_max=="max_c" & m$revenu_max>-1 & !is.na(m$revenu_max)],150000), weights=m$weight[m$variante_max=="max_c" & m$revenu_max>-1 & !is.na(m$revenu_max)]/sum(m$weight[m$variante_max=="max_c" & m$revenu_max>-1 & !is.na(m$revenu_max)]), bw=2000), lwd=2, col="purple", lty=2)
lines(density(pmin(m$revenu_max_ideal[m$revenu_max_ideal>-1 & !is.na(m$revenu_max_ideal)],150000), weights=m$weight[m$revenu_max_ideal>-1 & !is.na(m$revenu_max_ideal)]/sum(m$weight[m$revenu_max_ideal>-1 & !is.na(m$revenu_max_ideal)]), bw=2000), col="brown", lty=2, lwd=2)
abline(v=seq(0,130000,by=10000), col="grey",lty=3)
axis(2)
axis(1, at=c(0,10000,50000,100000,150000), labels=c("0","10k","50k","100k","Inf"))
legend("topleft", title="variante_max: formulation de la question", title.col="black", legend=c("max_s: maximum légal désiré", "max_c: s + argument anti-impôt", "ideal: si la France était une société idéale"),
  text.col=c("blue", "purple", "brown"),lwd=c(1),lty=c(1,2,3),col=c("blue", "purple", "brown"))
  # e$variante_rdb <<- factor(e$variante_rdb, c("aid","rdb","ass","gar")) # c("gar","ass","rdb","aid")
#TODO: vérifier signification de différences entre 3m et 100k ainsi que fr et monde dans revenu_max
#TODO: plot rapport_max
m2 <- m
m2$variante_max[m2$variante_max=="d" | m2$variante_max=="t"] <- ""
m2$variante_max[m2$vague==2 & !is.na(m2$revenu_max)] <- "max_s"
seuils_rev_max <- c(5000, 10000, 30000, Inf)
data_rev_max <- cbind(data_seuils(variable="revenu_max_ideal", seuils=seuils_rev_max, miss=FALSE), data_seuils(data=m2, variable="revenu_max", variante="variante_max", seuils=seuils_rev_max, miss=FALSE))
data_rev_max_miss <- cbind(data_seuils(variable="revenu_max_ideal", seuils=seuils_rev_max, miss=T), data_seuils(data=m2, variable="revenu_max", variante="variante_max", seuils=seuils_rev_max, miss=T))
barres(data=data_rev_max, file="revenu_max", title="<b>Revenu maximal désiré</b><br>Revenu (net) maximal désiré selon la formulation de la question (en €/mois)", labels=rev(c("Maximum légal désiré (s)", "s + argument anti-impôt", "Dans une société idéale")), color=c(color5[1:4], 'blue'), hover=c("Moins de 4 999€/mois", "De 5 000€/mois à 9 999€/mois", "De 10 000€/mois à 29 999€/mois", "Plus de 30 000€/mois", "Pas de plafond désiré"), legend=c("Moins de 4 999", "De 5 000 à 9 999", "De 10 000 à 29 999", "Plus de 30 000", "Pas de plafond désiré"), nsp=FALSE, sort=FALSE, showLegend=T, margin_r=0, margin_l=NA)
barres(data=data_rev_max, file="income_max", labels=rev(c("Legal maximum", "Legal + anti-tax argument", "Ideal maximum")), color=c(color5[1:4], 'blue'), hover=c("Less than 4,999€", "5,000€ to 9,999€", "10,000€ to 29,999€", "More than 30,000€", "No ceiling desired"), legend=c("Less than 4,999€", "5,000€ to 9,999€", "10,000€ to 29,999€", "More than 30,000€", "No ceiling desired"), nsp=FALSE, sort=FALSE, showLegend=T, margin_r=0, margin_l=NA)
barres(data=data_rev_max_miss, file="revenu_max_miss", title="<b>Revenu maximal désiré</b><br>Revenu (net) maximal désiré selon la formulation de la question (en €/mois)", labels=rev(c("Maximum légal désiré (s)", "s + argument anti-impôt", "Dans une société idéale")), color=c(color5[1:4], 'blue', 'lightgrey'), hover=c("Moins de 4 999€/mois", "De 5 000€/mois à 9 999€/mois", "De 10 000€/mois à 29 999€/mois", "Plus de 30 000€/mois", "Pas de plafond désiré", "NSP"), legend=c("Moins de 4 999", "De 5 000 à 9 999", "De 10 000 à 29 999", "Plus de 30 000", "Pas de plafond désiré", "NSP"), nsp=T, sort=FALSE, showLegend=T, margin_r=0, margin_l=NA)
barres(data=data_rev_max_miss, file="income_max_miss", labels=rev(c("Legal maximum", "Legal + anti-tax argument", "Ideal maximum")), color=c(color5[1:4], 'blue', 'lightgrey'), hover=c("Less than 4,999€", "5,000€ to 9,999€", "10,000€ to 29,999€", "More than 30,000€", "No ceiling desired", "NPR"), legend=c("Less than 4,999€", "5,000€ to 9,999€", "10,000€ to 29,999€", "More than 30,000€", "No ceiling desired", "NPR"), nsp=T, sort=FALSE, showLegend=T, margin_r=0, margin_l=NA)
rm(m2)


##### Quotient conjugal vs. Individualisation #####
decrit(m$quotient_indiv, weights=m$weight)
decrit(m$vague[!is.na(m$quotient)])
decrit(m$vague[!is.na(m$quotient_progressif)])
decrit(m$vague[!is.na(m$quotient_indiv)])
decrit(m$quotient_indiv[m$vague==1], weights=m$weight[m$vague==1])
decrit(m$quotient_indiv[m$vague==2], weights=m$weight[m$vague==2])
decrit(m$quotient_indiv[m$individualisation!=""], weights=m$weight[m$individualisation!=""]) # version indiv
decrit(m$quotient_indiv[m$individualisation=="" & m$vague==1], weights=m$weight[m$individualisation=="" & m$vague==1]) # version quotient
decrit(m$quotient_indiv[m$individualisation_progressive!=""], weights=m$weight[m$individualisation_progressive!=""]) # version indiv_prog
decrit(m$quotient_indiv[m$individualisation_progressive=="" & m$vague==2], weights=m$weight[m$individualisation_progressive=="" & m$vague==2]) # version quotient_prog
decrit(m$individualisation, weights=m$weight)
decrit(m$individualisation_progressive, weights=m$weight)
decrit(m$quotient, weights=m$weight)
decrit(m$quotient_progressif, weights=m$weight)
# *** effet de l'amorce sur la première vague :
summary(lm((quotient_indiv=="quotient") ~ (individualisation==""), subset = (vague==1 & quotient_indiv!="NSP"), data=m, weights=weight))
summary(lm((quotient_indiv=="quotient") ~ (individualisation==""), subset = (vague==1), data=m, weights=weight))
# ** effet de l'amorce sur la deuxième vague :
summary(lm((quotient_indiv=="quotient") ~ (individualisation_progressive==""), subset = (vague==2 & quotient_indiv!="NSP"), data=m, weights=weight))
summary(lm((quotient_indiv=="quotient") ~ (individualisation_progressive==""), subset = (vague==2 ), data=m, weights=weight))
# effet de l'amorce sur les deux vagues confondues (#TODO: bug, mais c'est sûr que c'est significatif) :
decrit(quotient_indiv[individualisation_progressive=="" | individualisation==""])
summary(lm((quotient_indiv=="quotient") ~ (individualisation_progressive=="" | individualisation==""), subset = ( quotient_indiv!="NSP"), data=m, weights=weight))
summary(lm((quotient_indiv=="quotient") ~ (individualisation_progressive=="" | individualisation==""), data=m, weights=weight))

summary(lm((quotient_indiv=="quotient") ~ (vague==1), subset = (quotient_indiv!="NSP" ), data=m, weights=weight))
summary(lm((quotient_indiv=="quotient") ~ (vague==1), subset = (quotient_indiv!="NSP" & (individualisation!="" | individualisation_progressive!="") ), data=m, weights=weight))
# effet de la proposition de mise en place progressive sur la version 'individualisation' (non significatif) :
summary(lm((quotient_indiv=="quotient") ~ (vague==2), subset = (quotient_indiv!="NSP" & (individualisation!="" | individualisation_progressive!="") ), data=m, weights=weight))
# * effet de la proposition de mise en place progressive sur la version 'quotient' :
summary(lm((quotient_indiv=="quotient") ~ (vague==2), subset = (quotient_indiv!="NSP" & (individualisation=="" | individualisation_progressive=="") ), data=m, weights=weight))
# *** effet de la proposition de mise en place progressive sur le nombre de NSP :
summary(lm((quotient_indiv=="NSP") ~ (vague==2), subset = (individualisation=="" | individualisation_progressive==""), data=m , weights=weight))
summary(lm((quotient_indiv=="NSP") ~ (vague==2), subset = (individualisation_progressive==""), data=m, weights=weight )) # bug
summary(lm((quotient_indiv=="NSP") ~ (vague==2), subset = (individualisation==""), data=m, weights=weight )) # bug

# TODO: harmoniser les définitions (NA) de indiv et indiv_prog:
describe(m$individualisation_progressive)
describe(m$individualisation)
length(which(m$individualisation_progressive=="")) # bizarre

# *** effet d'être marié
decrit(m$quotient_indiv[m$situation_maritale=="Marié·e ou pacsé·e"], weights=m$weight[m$situation_maritale=="Marié·e ou pacsé·e"])
decrit(m$quotient_indiv[m$situation_maritale!="Marié·e ou pacsé·e"], weights=m$weight[m$situation_maritale!="Marié·e ou pacsé·e"])
decrit(m$quotient_indiv[m$situation_maritale=="Marié·e ou pacsé·e" & m$vague==1], weights=m$weight[m$situation_maritale=="Marié·e ou pacsé·e" & m$vague==1])
decrit(m$quotient_indiv[m$situation_maritale!="Marié·e ou pacsé·e" & m$vague==1], weights=m$weight[m$situation_maritale=="Marié·e ou pacsé·e" & m$vague!=1])
decrit(m$quotient_indiv[m$situation_maritale=="Marié·e ou pacsé·e" & m$vague==2], weights=m$weight[m$situation_maritale=="Marié·e ou pacsé·e" & m$vague==2])
decrit(m$quotient_indiv[m$situation_maritale!="Marié·e ou pacsé·e" & m$vague==2], weights=m$weight[m$situation_maritale=="Marié·e ou pacsé·e" & m$vague!=2])
summary(lm((quotient_indiv=="quotient") ~ (situation_maritale=="Marié·e ou pacsé·e"), subset = ( m$quotient_indiv!="NSP"), data=m, weights=weight))
summary(lm((quotient_indiv=="quotient") ~ (situation_maritale=="Marié·e ou pacsé·e"), weights=weight, data=m))

#### Héritage : taxation des successions ####
decrit(m$heritage, weights=m$weight) # vague 1
decrit(m$heritage_6, weights=m$weight)
decrit(m$heritage_7, weights=m$weight)
decrit(m$heritage_9, weights=m$weight)
decrit(m$heritage_5_corr, weights=m$weight)
decrit(m$heritage_6_corr, weights=m$weight)
decrit(m$heritage_7_corr, weights=m$weight)
decrit(m$heritage_9_corr, weights=m$weight)
decrit(m$heritage_5_corr[grepl("r",m$variante_heritage)], weights=m$weight[grepl("r",m$variante_heritage)])
decrit(m$heritage_6_corr[grepl("r",m$variante_heritage)], weights=m$weight[grepl("r",m$variante_heritage)])
decrit(m$heritage_7_corr[grepl("r",m$variante_heritage)], weights=m$weight[grepl("r",m$variante_heritage)])
decrit(m$heritage_9_corr[grepl("r",m$variante_heritage)], weights=m$weight[grepl("r",m$variante_heritage)])
decrit(m$heritage_5_corr[grepl("t",m$variante_heritage)], weights=m$weight[grepl("t",m$variante_heritage)])
decrit(m$heritage_6_corr[grepl("t",m$variante_heritage)], weights=m$weight[grepl("t",m$variante_heritage)])
decrit(m$heritage_7_corr[grepl("t",m$variante_heritage)], weights=m$weight[grepl("t",m$variante_heritage)])
decrit(m$heritage_9_corr[grepl("t",m$variante_heritage)], weights=m$weight[grepl("t",m$variante_heritage)])
decrit(m$heritage_5_corr[grepl("r",m$variante_heritage) & m$heritage_5_corr>0], weights=m$weight[grepl("r",m$variante_heritage) & m$heritage_5_corr>0])
decrit(m$heritage_6_corr[grepl("r",m$variante_heritage) & m$heritage_6_corr>0], weights=m$weight[grepl("r",m$variante_heritage) & m$heritage_6_corr>0])
decrit(m$heritage_7_corr[grepl("r",m$variante_heritage) & m$heritage_7_corr>0], weights=m$weight[grepl("r",m$variante_heritage) & m$heritage_7_corr>0])
decrit(m$heritage_9_corr[grepl("r",m$variante_heritage) & m$heritage_9_corr>0], weights=m$weight[grepl("r",m$variante_heritage) & m$heritage_9_corr>0])
decrit(m$heritage_6_corr[grepl("r",m$variante_heritage) & m$heritage_5_corr==0], weights=m$weight[grepl("r",m$variante_heritage) & m$heritage_5_corr==0])
length(which(m$heritage_5_corr==0))/length(which(!is.missing(m$heritage_5_corr)))
length(which(m$heritage_6_corr==0))/length(which(!is.missing(m$heritage_5_corr)))
length(which(m$heritage_7_corr==0))/length(which(!is.missing(m$heritage_5_corr)))
length(which(m$heritage_9_corr==0))/length(which(!is.missing(m$heritage_5_corr)))
decrit(m$taux_max_successions, weights=m$weight)
decrit(m$heritage_moyens, weights=m$weight)
decrit(m$heritage_eleves, weights=m$weight)
decrit(m$quel_heritage, weights=m$weight)
decrit(m$heritage_6[m$vague==1], weights=m$weight[m$vague==1])
decrit(m$heritage_6[m$vague==2 & grepl("r", m$variante_heritage) & m$heritage_6<=1], weights=m$weight[m$vague==2 & grepl("r", m$variante_heritage) & m$heritage_6<=1])
decrit(m$heritage_7[m$vague==1], weights=m$weight[m$vague==1])
decrit(m$heritage_7[m$vague==2 & grepl("r", m$variante_heritage) & m$heritage_7<=1], weights=m$weight[m$vague==2 & grepl("r", m$variante_heritage) & m$heritage_7<=1])
decrit(m$heritage_9[m$vague==1], weights=m$weight[m$vague==1])
decrit(m$heritage_9[m$vague==2 & grepl("r", m$variante_heritage) & m$heritage_9<=1], weights=m$weight[m$vague==2 & grepl("r", m$variante_heritage) & m$heritage_9<=1])
summary(lm(heritage_6 ~ (vague==2),subset=(heritage_6<=1 & !is.missing(heritage_6) & (vague==1 | grepl("r", variante_heritage))), data=m, weights=weight))
summary(lm(heritage_7 ~ (vague==2),subset=(heritage_7<=1 & !is.missing(heritage_7) & (vague==1 | grepl("r", variante_heritage))), data=m, weights=weight))
summary(lm(heritage_7 ~ (vague==2),subset=(heritage_7<=1 & !is.missing(heritage_7) & (vague==1 | grepl("r", variante_heritage))), data=me, weights=weight))
summary(lm(heritage_9 ~ (vague==2),subset=(heritage_9<=1 & !is.missing(heritage_9) & (vague==1 | grepl("r", variante_heritage))), data=m, weights=weight))
summary(lm(heritage_9 ~ (vague==2),subset=(heritage_9<=1 & !is.missing(heritage_9) & (vague==1 | grepl("r", variante_heritage))), data=me, weights=weight))

# Héritage désiré formulation "reçu" selon la vague
# TODO: N titre
par(default_par)
# plot(density(m$heritage_6[m$heritage_6<=1 & !is.missing(m$heritage_6) & m$vague==1 & !is.na(m$heritage_6)], weights=m$weight[m$heritage_6<=1 & !is.missing(m$heritage_6) & m$vague==1 & !is.na(m$heritage_6)], bw=0.05), main="Proportion désirée revenant à l'héritier lors d'une succession\nselon la vague (vague 1 : un montant demandé / vague 2 : 4 montants demandés)\n(médianes (vague 1/2) : 90/80% (10^6€), 70/60% (10^7€), 75/50% (10^9€), N~345/160 + NSP~55/90)", xlab="Proportion désirée revenant à l'unique héritier lors d'une succesion directe, selon différents montants et contextes", col="green", xlim=c(0,1), ylim=c(0,1000), lwd=2)
# lines(density(m$heritage_6[m$heritage_6<=1 & !is.missing(m$heritage_6) & grepl("r", m$variante_heritage) & !is.na(m$heritage_6)], weights=m$weight[m$heritage_6<=1 & !is.missing(m$heritage_6) & grepl("r", m$variante_heritage) & !is.na(m$heritage_6)], bw=0.05), col="green", lty=2, lwd=2)
# lines(density(m$heritage_7[m$heritage_7<=1 & !is.missing(m$heritage_7) & m$vague==1 & !is.na(m$heritage_7)], weights=m$weight[m$heritage_7<=1 & !is.missing(m$heritage_7) & m$vague==1 & !is.na(m$heritage_7)], bw=0.05), col="red", lty=1, lwd=0.5)
# lines(density(m$heritage_7[m$heritage_7<=1 & !is.missing(m$heritage_7) & grepl("r", m$variante_heritage) & !is.na(m$heritage_7)], weights=m$weight[m$heritage_7<=1 & !is.missing(m$heritage_7) & grepl("r", m$variante_heritage) & !is.na(m$heritage_7)], bw=0.05), col="red", lty=2, lwd=0.5)
# lines(density(m$heritage_9[m$heritage_9<=1 & !is.missing(m$heritage_9) & m$vague==1 & !is.na(m$heritage_9)], weights=m$weight[m$heritage_9<=1 & !is.missing(m$heritage_9) & m$vague==1 & !is.na(m$heritage_9)], bw=0.05), col="blue", lty=1)
# lines(density(m$heritage_9[m$heritage_9<=1 & !is.missing(m$heritage_9) & grepl("r", m$variante_heritage) & !is.na(m$heritage_9)], weights=m$weight[m$heritage_9<=1 & !is.missing(m$heritage_9) & grepl("r", m$variante_heritage) & !is.na(m$heritage_9)], bw=0.05), col="blue", lty=2)
# grid()
# legend("top", title="Montant de l'héritage (en €)", title.col="black", legend=c("1 million, vague 1", "1 million, vague 2", "10 millions, vague 1", "10 millions, vague 2", "1 milliard, vague 1", "1 milliard, vague 2"),
#   text.col=c("green", "green","red", "red", "blue", "blue"),lwd=c(2,2,0.5,0.5,1,1),col=c("green", "green","red", "red", "blue", "blue"),lty=rep(c(1,2),3))
# Densités pour heritage_corr de la deuxième vague, tout confondu
plot(density(m$heritage_5_corr[m$heritage_5_corr>-1 & !is.na(m$heritage_5_corr)], weights=m$weight[m$heritage_5_corr>-1 & !is.na(m$heritage_5_corr)]/sum(m$weight[m$heritage_5_corr>-1 & !is.na(m$heritage_5_corr)]), bw=0.02), lwd=2, main="Proportion désirée revenant à l'héritier lors d'une succession\n(médianes : 99% (10^5€), 90% (10^6, 10^7 et 10^9€), N~730 + NSP~70)", xlab="Proportion désirée revenant à l'unique héritier lors d'une succesion directe selon différents montants", col="green", xlim=c(0,1))
lines(density(m$heritage_6_corr[m$heritage_6_corr>-1 & !is.na(m$heritage_6_corr)], weights=m$weight[m$heritage_6_corr>-1 & !is.na(m$heritage_6_corr)]/sum(m$weight[m$heritage_6_corr>-1 & !is.na(m$heritage_6_corr)]), bw=0.02), col="red", lty=2, lwd=2)
lines(density(m$heritage_7_corr[m$heritage_7_corr>-1 & !is.na(m$heritage_7_corr)], weights=m$weight[m$heritage_7_corr>-1 & !is.na(m$heritage_7_corr)]/sum(m$weight[m$heritage_7_corr>-1 & !is.na(m$heritage_7_corr)]), bw=0.02), col="blue", lty=3, lwd=2)
lines(density(m$heritage_9_corr[m$heritage_9_corr>-1 & !is.na(m$heritage_9_corr)], weights=m$weight[m$heritage_9_corr>-1 & !is.na(m$heritage_9_corr)]/sum(m$weight[m$heritage_9_corr>-1 & !is.na(m$heritage_9_corr)]), bw=0.02), col="purple", lty=4, lwd=2)
grid()
legend("topleft", title="Montant de l'héritage (en €)", title.col="black", legend=c("100 000","1 million", "10 millions", "1 milliard"),
  text.col=c("green","red", "blue", "purple"),lwd=2,col=c("green","red", "blue", "purple"),lty=1:4)
# Densités pour heritage_corr de la deuxième vague, formulation "reçu", 0 exclu
plot(density(m$heritage_5_corr[m$heritage_5_corr>0 & grepl("r",m$variante_heritage) & !is.na(m$heritage_5_corr)], weights=m$weight[m$heritage_5_corr>0 & grepl("r",m$variante_heritage) & !is.na(m$heritage_5_corr)]/sum(m$weight[m$heritage_5_corr>0 & grepl("r",m$variante_heritage) & !is.na(m$heritage_5_corr)]), bw=0.03), lwd=2, main="Proportion désirée revenant à l'héritier lors d'une succession\n(médianes : 95% (10^5€), 80% (10^6€), 50% (10^7€), 50% (10^9€), N~310 + NSP~90)", xlab="Proportion désirée revenant à l'unique héritier lors d'une succesion directe selon différents montants", col="green", xlim=c(0,1))
lines(density(m$heritage_6_corr[m$heritage_6_corr>0 & grepl("r",m$variante_heritage) & !is.na(m$heritage_6_corr)], weights=m$weight[m$heritage_6_corr>0 & grepl("r",m$variante_heritage) & !is.na(m$heritage_6_corr)]/sum(m$weight[m$heritage_6_corr>0 & grepl("r",m$variante_heritage) & !is.na(m$heritage_6_corr)]), bw=0.03), col="red", lty=2, lwd=2)
lines(density(m$heritage_7_corr[m$heritage_7_corr>0 & grepl("r",m$variante_heritage) & !is.na(m$heritage_7_corr)], weights=m$weight[m$heritage_7_corr>0 & grepl("r",m$variante_heritage) & !is.na(m$heritage_7_corr)]/sum(m$weight[m$heritage_7_corr>0 & grepl("r",m$variante_heritage) & !is.na(m$heritage_7_corr)]), bw=0.03), col="blue", lty=3, lwd=2)
lines(density(m$heritage_9_corr[m$heritage_9_corr>0 & grepl("r",m$variante_heritage) & !is.na(m$heritage_9_corr)], weights=m$weight[m$heritage_9_corr>0 & grepl("r",m$variante_heritage) & !is.na(m$heritage_9_corr)]/sum(m$weight[m$heritage_9_corr>0 & grepl("r",m$variante_heritage) & !is.na(m$heritage_9_corr)]), bw=0.03), col="purple", lty=4, lwd=2)
grid()
legend("topleft", title="Montant de l'héritage (en €)", title.col="black", legend=c("100 000","1 million", "10 millions", "1 milliard"),
  text.col=c("green","red", "blue", "purple"),lwd=2,col=c("green","red", "blue", "purple"),lty=1:4)
# Densités pour heritage_corr de la deuxième vague, formulation "reçu"
plot(density(m$heritage_5_corr[m$heritage_5_corr>-1 & grepl("r",m$variante_heritage) & !is.na(m$heritage_5_corr)], weights=m$weight[m$heritage_5_corr>-1 & grepl("r",m$variante_heritage) & !is.na(m$heritage_5_corr)]/sum(m$weight[m$heritage_5_corr>-1 & grepl("r",m$variante_heritage) & !is.na(m$heritage_5_corr)]), bw=0.03), lwd=2, main="Proportion désirée revenant à l'héritier lors d'une succession\n(médianes : 95% (10^5€), 80% (10^6€), 60% (10^7€), 50% (10^9€), N~345 + NSP~55)", xlab="Proportion désirée revenant à l'unique héritier lors d'une succesion directe selon différents montants", col="green", xlim=c(0,1))
lines(density(m$heritage_6_corr[m$heritage_6_corr>-1 & grepl("r",m$variante_heritage) & !is.na(m$heritage_6_corr)], weights=m$weight[m$heritage_6_corr>-1 & grepl("r",m$variante_heritage) & !is.na(m$heritage_6_corr)]/sum(m$weight[m$heritage_6_corr>-1 & grepl("r",m$variante_heritage) & !is.na(m$heritage_6_corr)]), bw=0.03), col="red", lty=2, lwd=2)
lines(density(m$heritage_7_corr[m$heritage_7_corr>-1 & grepl("r",m$variante_heritage) & !is.na(m$heritage_7_corr)], weights=m$weight[m$heritage_7_corr>-1 & grepl("r",m$variante_heritage) & !is.na(m$heritage_7_corr)]/sum(m$weight[m$heritage_7_corr>-1 & grepl("r",m$variante_heritage) & !is.na(m$heritage_7_corr)]), bw=0.03), col="blue", lty=3, lwd=2)
lines(density(m$heritage_9_corr[m$heritage_9_corr>-1 & grepl("r",m$variante_heritage) & !is.na(m$heritage_9_corr)], weights=m$weight[m$heritage_9_corr>-1 & grepl("r",m$variante_heritage) & !is.na(m$heritage_9_corr)]/sum(m$weight[m$heritage_9_corr>-1 & grepl("r",m$variante_heritage) & !is.na(m$heritage_9_corr)]), bw=0.03), col="purple", lty=4, lwd=2)
grid()
legend("topleft", title="Montant de l'héritage (en €)", title.col="black", legend=c("100 000","1 million", "10 millions", "1 milliard"),
  text.col=c("green","red", "blue", "purple"),lwd=2,col=c("green","red", "blue", "purple"),lty=1:4)
# Densités pour heritage_corr de la deuxième vague, formulation "taxe"
plot(density(1 - m$heritage_5_corr[m$heritage_5_corr>-1 & grepl("t",m$variante_heritage) & !is.na(m$heritage_5_corr)], weights=m$weight[m$heritage_5_corr>-1 & grepl("t",m$variante_heritage) & !is.na(m$heritage_5_corr)]/sum(m$weight[m$heritage_5_corr>-1 & grepl("t",m$variante_heritage) & !is.na(m$heritage_5_corr)]), bw=0.03), lwd=2, main="Taxe désirée sur une succession\n(médianes : 0% (10^5€), 5% (10^6€), 8% (10^7€), 1% (10^9€), 9% (taux max), N~380 + NSP~20)", xlab="Taxe désirée prélevée sur une succession directe à un héritier unique selon différents montants (en proportion)", col="green", xlim=c(0,1))
lines(density(1 - m$heritage_6_corr[m$heritage_6_corr>-1 & grepl("t",m$variante_heritage) & !is.na(m$heritage_6_corr)], weights=m$weight[m$heritage_6_corr>-1 & grepl("t",m$variante_heritage) & !is.na(m$heritage_6_corr)]/sum(m$weight[m$heritage_6_corr>-1 & grepl("t",m$variante_heritage) & !is.na(m$heritage_6_corr)]), bw=0.03), col="red", lty=2, lwd=2)
lines(density(1 - m$heritage_7_corr[m$heritage_7_corr>-1 & grepl("t",m$variante_heritage) & !is.na(m$heritage_7_corr)], weights=m$weight[m$heritage_7_corr>-1 & grepl("t",m$variante_heritage) & !is.na(m$heritage_7_corr)]/sum(m$weight[m$heritage_7_corr>-1 & grepl("t",m$variante_heritage) & !is.na(m$heritage_7_corr)]), bw=0.03), col="blue", lty=3, lwd=2)
lines(density(1 -  m$heritage_9_corr[m$heritage_9_corr>-1 & grepl("t",m$variante_heritage)] & !is.na(m$heritage_9_corr), weights=m$weight[m$heritage_9_corr>-1 & grepl("t",m$variante_heritage) & !is.na(m$heritage_9_corr)]/sum(m$weight[m$heritage_9_corr>-1 & grepl("t",m$variante_heritage) & !is.na(m$heritage_9_corr)]), bw=0.03), col="purple", lty=4, lwd=2)
lines(density(m$taux_max_successions/100, na.rm=T, bw=0.03), col="brown", lty=5)
grid()
legend("topright", title="Montant de l'héritage (en €)", title.col="black", legend=c("100 000","1 million", "10 millions", "1 milliard", "taux maximum désiré"),
  text.col=c("green","red", "blue", "purple", "brown"),lwd=2,col=c("green","red", "blue", "purple", "brown"),lty=1:5)
# Densités pour heritage_corr de la deuxième vague, formulation "taxe" reverse #TODO: retirer les 0 pour 10^5 ?
plot(density(m$heritage_5_corr[m$heritage_5_corr>-1 & grepl("t",m$variante_heritage) & !is.na(m$heritage_5_corr)], weights=m$weight[m$heritage_5_corr>-1 & grepl("t",m$variante_heritage) & !is.na(m$heritage_5_corr)]/sum(m$weight[m$heritage_5_corr>-1 & grepl("t",m$variante_heritage) & !is.na(m$heritage_5_corr)]), bw=0.03), lwd=2, main="Taxe désirée sur une succession\n(médianes : 0% (10^5€), 5% (10^6€), 8% (10^7€), 1% (10^9€), 9% (taux max), N~380 + NSP~20)", xlab="Taxe désirée prélevée sur une succession directe à un héritier unique selon différents montants (1 - la proportion)", col="green", xlim=c(0,1))
lines(density(m$heritage_6_corr[m$heritage_6_corr>-1 & grepl("t",m$variante_heritage) & !is.na(m$heritage_6_corr)], weights=m$weight[m$heritage_6_corr>-1 & grepl("t",m$variante_heritage) & !is.na(m$heritage_6_corr)]/sum(m$weight[m$heritage_6_corr>-1 & grepl("t",m$variante_heritage) & !is.na(m$heritage_6_corr)]), bw=0.03), col="red", lty=2, lwd=2)
lines(density(m$heritage_7_corr[m$heritage_7_corr>-1 & grepl("t",m$variante_heritage) & !is.na(m$heritage_7_corr)], weights=m$weight[m$heritage_7_corr>-1 & grepl("t",m$variante_heritage) & !is.na(m$heritage_7_corr)]/sum(m$weight[m$heritage_7_corr>-1 & grepl("t",m$variante_heritage) & !is.na(m$heritage_7_corr)]), bw=0.03), col="blue", lty=3, lwd=2)
lines(density(m$heritage_9_corr[m$heritage_9_corr>-1 & grepl("t",m$variante_heritage) & !is.na(m$heritage_9_corr)], weights=m$weight[m$heritage_9_corr>-1 & grepl("t",m$variante_heritage) & !is.na(m$heritage_9_corr)]/sum(m$weight[m$heritage_9_corr>-1 & grepl("t",m$variante_heritage) & !is.na(m$heritage_9_corr)]), bw=0.03), col="purple", lty=4, lwd=2)
lines(density(1 - m$taux_max_successions/100, na.rm=T, bw=0.03), col="brown", lty=5)
grid()
legend("topleft", title="Montant de l'héritage (en €)", title.col="black", legend=c("100 000","1 million", "10 millions", "1 milliard", "taux maximum désiré"),
  text.col=c("green","red", "blue", "purple", "brown"),lwd=2,col=c("green","red", "blue", "purple", "brown"),lty=1:5)

data_succession5 <- function(vars=c("heritage_9_corr", "heritage_7_corr", "heritage_6_corr", "heritage_5_corr"), data=m, miss=T, weights=T, variante="r", zero_exclu = TRUE) {
  if (zero_exclu) { Min <- 0}
  else { Min <- -0.5 }
  matrice <- c()
  for (var in vars) {
    if (miss) {
      mat <- c(length(which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]) & data[[var]]<=0.1))/length(which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]))), length(which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]) & data[[var]]>0.1 & data[[var]]<0.5))/length(which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]))), length(which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]) & data[[var]]>=0.5 & data[[var]]<0.8))/length(which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]))), length(which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]) & data[[var]]>=0.8 & data[[var]]<1))/length(which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]))), length(which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]) & data[[var]]==1))/length(which(!is.missing(data[[var]]) | data[[var]]==Min | data[[var]]==Min)),length(which(is.missing(data[[var]]) & grepl(variante,data[['variante_heritage']])))/length(which(grepl(variante,data[['variante_heritage']]))))
      if (weights) { mat <- c(sum(data[['weight']][which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]) & data[[var]]<=0.1)])/sum(data[['weight']][which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]))]), sum(data[['weight']][which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]) & data[[var]]>0.1 & data[[var]]<0.5)])/sum(data[['weight']][which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]))]), sum(data[['weight']][which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]) & data[[var]]>=0.5 & data[[var]]<0.8)])/sum(data[['weight']][which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]))]), sum(data[['weight']][which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]) & data[[var]]>=0.8 & data[[var]]<1)])/sum(data[['weight']][which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]))]), sum(data[['weight']][which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]) & data[[var]]==1)])/sum(data[['weight']][which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]))]),sum(data[['weight']][which(is.missing(data[[var]]) & grepl(variante,data[['variante_heritage']]))])/sum(data[['weight']][grepl(variante,data[['variante_heritage']])])) }
    }
    else {
      mat <- c(length(which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]) & data[[var]]<=0.1))/length(which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]))), length(which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]) & data[[var]]>0.1 & data[[var]]<0.5))/length(which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]))), length(which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]) & data[[var]]>=0.5 & data[[var]]<0.8))/length(which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]))),  length(which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]) & data[[var]]>=0.8 & data[[var]]<1))/length(which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]))),  length(which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]) & data[[var]]==1))/length(which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]))))    
      if (weights) { mat <- c(sum(data[['weight']][which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]) & data[[var]]<=0.1)])/sum(data[['weight']][which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]))]), sum(data[['weight']][which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]) & data[[var]]>0.1 & data[[var]]<0.5)])/sum(data[['weight']][which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]))]), sum(data[['weight']][which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]) & data[[var]]>=0.5 & data[[var]]<0.8)])/sum(data[['weight']][which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]))]), sum(data[['weight']][which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]) & data[[var]]>=0.8 & data[[var]]<1)])/sum(data[['weight']][which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]))]), sum(data[['weight']][which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]) & data[[var]]==1)])/sum(data[['weight']][which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]))])) } }
    matrice <- c(matrice, mat) }
  matrice <- matrix(c(matrice), ncol=length(vars))
  return(matrice)
  # return(as.data.frame(matrice))
}
data_succession7 <- function(vars=c("heritage_9_corr", "heritage_7_corr", "heritage_6_corr", "heritage_5_corr"), data=m, miss=T, weights=T, variante="r", zero_exclu = TRUE) {
  if (zero_exclu) { Min <- 0}
  else { Min <- -0.5 }
  matrice <- c()
  for (var in vars) {
    if (miss) {
      mat <- c(length(which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]) & data[[var]]<=0.01))/length(which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]))), length(which(data[[var]]>0.01 & grepl(variante,data[['variante_heritage']]) & data[[var]]<=0.1))/length(which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]))), length(which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]) & data[[var]]>0.1 & data[[var]]<0.5))/length(which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]))), length(which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]) & data[[var]]==0.5))/length(which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]))), length(which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]) & data[[var]]>0.5 & data[[var]]<0.8))/length(which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]))), length(which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]) & data[[var]]>=0.8 & data[[var]]<1))/length(which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]))), length(which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]) & data[[var]]==1))/length(which(!is.missing(data[[var]]) | data[[var]]==Min | data[[var]]==Min)),length(which(is.missing(data[[var]]) & grepl(variante,data[['variante_heritage']])))/length(which(grepl(variante,data[['variante_heritage']]))))
      if (weights) { mat <- c(sum(data[['weight']][which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]) & data[[var]]<=0.01)])/sum(data[['weight']][which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]))]), sum(data[['weight']][which(data[[var]]>0.01 & grepl(variante,data[['variante_heritage']]) & data[[var]]<=0.1)])/sum(data[['weight']][which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]))]), sum(data[['weight']][which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]) & data[[var]]>0.1 & data[[var]]<0.5)])/sum(data[['weight']][which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]))]), sum(data[['weight']][which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]) & data[[var]]==0.5)])/sum(data[['weight']][which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]))]), sum(data[['weight']][which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]) & data[[var]]>0.5 & data[[var]]<0.8)])/sum(data[['weight']][which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]))]), sum(data[['weight']][which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]) & data[[var]]>=0.8 & data[[var]]<1)])/sum(data[['weight']][which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]))]), sum(data[['weight']][which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]) & data[[var]]==1)])/sum(data[['weight']][which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]))]),sum(data[['weight']][which(is.missing(data[[var]]) & grepl(variante,data[['variante_heritage']]))])/sum(data[['weight']][grepl(variante,data[['variante_heritage']])])) }
    }
    else {
      mat <- c(length(which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]) & data[[var]]<=0.01))/length(which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]))), length(which(data[[var]]>0.01 & grepl(variante,data[['variante_heritage']]) & data[[var]]<=0.1))/length(which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]))), length(which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]) & data[[var]]>0.1 & data[[var]]<0.5))/length(which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]))), length(which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]) & data[[var]]==0.5))/length(which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]))),length(which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]) & data[[var]]>0.5 & data[[var]]<0.8))/length(which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]))),  length(which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]) & data[[var]]>=0.8 & data[[var]]<1))/length(which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]))),  length(which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]) & data[[var]]==1))/length(which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]))))    
      if (weights) { mat <- c(sum(data[['weight']][which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]) & data[[var]]<=0.01)])/sum(data[['weight']][which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]))]), sum(data[['weight']][which(data[[var]]>0.01 & grepl(variante,data[['variante_heritage']]) & data[[var]]<=0.1)])/sum(data[['weight']][which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]))]), sum(data[['weight']][which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]) & data[[var]]>0.1 & data[[var]]<0.5)])/sum(data[['weight']][which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]))]),sum(data[['weight']][which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]) & data[[var]]==0.5)])/sum(data[['weight']][which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]))]), sum(data[['weight']][which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]) & data[[var]]>0.5 & data[[var]]<0.8)])/sum(data[['weight']][which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]))]), sum(data[['weight']][which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]) & data[[var]]>=0.8 & data[[var]]<1)])/sum(data[['weight']][which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]))]), sum(data[['weight']][which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]) & data[[var]]==1)])/sum(data[['weight']][which(data[[var]]>Min & grepl(variante,data[['variante_heritage']]))])) } }
    matrice <- c(matrice, mat) }
  matrice <- matrix(c(matrice), ncol=length(vars))
  return(matrice)
  # return(as.data.frame(matrice))
}
barres(data=data_succession5(), file="heritage", title="<b>Taxation des successions</b><br>Proportion désirée revenant à l'unique héritier lors d'une succession de ... (en €)", labels=rev(c("100 000", "1 million", "10 millions", "1 milliard")), color=c(color5, 'lightgrey'), hover=c("Moins de 10%", "De 11% à 49%", "De 50% à 79%", "De 80% à 99%", "Pas de taxe", "NSP"), nsp=TRUE, sort=FALSE, legend=c("Moins de 10%", "De 11% à 49%", "De 50% à 79%", "De 80% à 99%", "Pas de taxe", "NSP"), showLegend=T, margin_r=0, margin_l=NA)
barres(data=data_succession7(), file="heritag7", title="<b>Taxation des successions</b><br>Proportion désirée revenant à l'unique héritier lors d'une succession de ... (en €)", labels=rev(c("100 000", "1 million", "10 millions", "1 milliard")), color=c(rainbow(7, end=5/15)[1:6], 'forestgreen' ,'lightgrey'), hover=c("Moins de 1%", "De 2% à 10%", "De 11% à 49%", "50%", "De 51% à 79%", "De 80% à 99%", "Pas de taxe", "NSP"), nsp=TRUE, sort=FALSE, legend=c("Moins de 1%", "De 2% à 10%", "De 11% à 49%", "50%", "De 51% à 79%", "De 80% à 99%", "Pas de taxe", "NSP"), showLegend=T, margin_r=0, margin_l=NA)

# *** écart des taux désirés entre 10^6 et les autres (10^7, 10^9, taux_max)
summary(lm(heritage ~ variante_heritage, subset=(vague==1),data=m, weights=weight))
# écart non significatif entre 10^7 et 10^9 (quel que soit l'échantillon)
summary(lm(heritage ~ variante_heritage, subset=(vague==1 & variante_heritage!="10^6"),data=m, weights=weight))
# *** effet (+12%) de la vague 2 sur heritage_7 et _9 (pas significatif pour _6)
summary(lm(heritage_6 ~ (vague==2), subset=(heritage_6>-1 & heritage_6<=1), data=m, weights=weight))
summary(lm(heritage_7 ~ (vague==2), subset=(heritage_7>-1 & heritage_7<=1), data=m, weights=weight))
summary(lm(heritage_9 ~ (vague==2), subset=(heritage_9>-1 & heritage_9<=1), data=m, weights=weight))
# *** effet de la formulation en termes de taxe ou d'héritage reçu, croissant avec le montant, entre 25% et 38%
# (quasiment, p-value=0.4) aucun effet de l'affichage des questions en ordre croissant ou décroissant (sauf pour 10^5)
summary(lm(heritage_5_corr ~ grepl("c", variante_heritage) + grepl("r", variante_heritage), subset=(!is.missing(heritage_5_corr) & vague==2), data=m, weights=weight))
summary(lm(heritage_6_corr ~ grepl("c", variante_heritage) + grepl("r", variante_heritage), subset=(!is.missing(heritage_6_corr) & vague==2), data=m, weights=weight))
summary(lm(heritage_7_corr ~ grepl("c", variante_heritage) + grepl("r", variante_heritage), subset=(!is.missing(heritage_7_corr) & vague==2), data=m, weights=weight))
summary(lm(heritage_9_corr ~ grepl("c", variante_heritage) + grepl("r", variante_heritage), subset=(!is.missing(heritage_9_corr) & vague==2), data=m, weights=weight))


#### Autres impôts #####
decrit(m$taxe_fonciere, weights=m$weight) # -1 1000 35 35 25 3 1
decrit(m$cot_soc, weights=m$weight) # -0.8 1000 25  38  31  5  1
decrit(m$tva, weights=m$weight) # -0.7 1000 26  34  28  10  2
decrit(m$taxe_revenus_fonciers, weights=m$weight) # -0.6 500 20  32  33 13 2
decrit(m$is, weights=m$weight) # -0.1 500 12  24  31  24  9
decrit(m$taxe_plus_values, weights=m$weight) # 0.1 500 11  21  33 20 15
decrit(m$taxe_dividendes, weights=m$weight) # 0.2 500 11  21  33 20 15
decrit(m$isf, weights=m$weight) # 0.2 1000 18  12  24  22  24
decrit(m$taxe_ghg_fr, weights=m$weight) # 0.4 500 11 16 12  40  21

#### Redistribution mondiale #####
decrit(m$taxe_ghg_monde, weights=m$weight) # 0.8 500  7  9 12  42  30
decrit(m$taxe_mondiale_riches, miss=T, weights=m$weight) # 350 80/14
decrit(m$taxe_mondiale_capital, miss=T, weights=m$weight) # 300 57/29
decrit(m$revenu_base_mondial, miss=T, weights=m$weight) # 350 47/31
decrit(m$revenu_base_mondial, weights=m$weight) # 270 60/40
decrit(m$cap_and_share, miss=T, weights=m$weight) # 500 57/43
decrit(m$cap_and_share, weights=m$weight) # 500 57/43
decrit(m$revenu_max_monde, miss=T, weights=m$weight) # 32/42
decrit(m$revenu_max_monde_3m, miss=T, weights=m$weight) # 38/28
decrit(m$revenu_max_monde_100k, miss=T, weights=m$weight) # 41/37
decrit(m$assemblee_climat, miss=T, weights=m$weight) # 50/27
decrit(m$assemblee_finance, miss=T, weights=m$weight) # 45/32
decrit(m$assemblee_climat, weights=m$weight)
decrit(m$assemblee_finance,  weights=m$weight)
oui_non(margin_l=430, c("humanisme","taxe_mondiale_riches", "taxe_mondiale_capital", "revenu_base_mondial", "cap_and_share", "assemblee_climat", "assemblee_finance", "transferts_inter_a"), "bars_monde", 
        c("<b>Je veux que les humains s'assurent les conditions nécessaires au</b>  <br><b>bien-être :</b> l’accès à l’eau potable, à la nourriture, aux soins, à un environ-<br>-nement sain, à la sécurité, au logement, à une éducation, à l’information",
          "Instauration d'une <b>taxe mondiale sur les 1% les plus riches</b> <br>pour financer le développement des pays pauvres <br>(vaccination, accès à l'eau potable, aux soins, à l'éducation...)", 
          "Instauration d'une <b>taxation mondiale du capital</b> <br> (sorte d'Impôt Sur la Fortune mondial)",
          "Instauration d'un r<b>evenu de base mondial</b>. <br>Environ 20€/mois serait versé à chaque adulte, <br>ce qui doublerait le revenu du milliard d'humains les plus pauvres",
          "Système <b>Cap and Share</b> : une taxe mondiale sur les émissions <br> de gaz à effet de serre finançant un revenu de base mondial <br> d'environ 20€/mois pour chaque adulte",
          "Instauration d'une <b>assemblée mondiale</b> (élue à la proportionnelle <br>sur des listes mondiales) dont le rôle serait uniquement de prendre <br>les mesures nécessaires à la <b>lutte contre le réchauffement climatique</b>",
          "Instauration d'une <b>assemblée mondiale</b> (élue à la proportionnelle <br>sur des listes mondiales) dont le rôle serait uniquement de <b>réguler</b> <br><b>le système financier</b> (notamment pour éviter les crises financières)",
          "Le transfert de <b>5% des revenus des pays riches aux pays pauvres</b>"))
oui_non(margin_l=430, en=TRUE, title="<b>Opinions of French people on global policies</b><br>", file="bars_world", c("humanisme","taxe_mondiale_riches", "revenu_base_mondial", "taxe_mondiale_capital", "cap_and_share", "assemblee_climat", "transferts_inter_a", "assemblee_finance"),
        c("<b>I want that humans insure to themselves the necessary conditions</b>  <br><b>for well-being:</b> access to drinkable water, food, healthcare, <br>a clean environment, security, housing, an education, information",
          "Establishment of a <b>global tax on the richest 1%</b> <br>to finance the development of low-income countries <br>(vaccination, access to drinkable water, healthcare, education...)", 
          "Establishment of a <b>global basic income</b>. <br>Around 20€/month would be allocated to each adult,<br>which would double the income of the poorest billion of humans",
          "Establishment of a <b>global tax on wealth</b>",
          "<b>Cap and Share</b> system: a global tax on emissions <br> of greenhouse gas emissions funding a global basic income <br> of about 20€/month for each adult",
          "Establishment of a <b>global assembly</b> (elected proportionally <br>on global lists) whose only role would be to take <br>measures needed to <b>tackle climate change</b>",
          "<b>Transferring 5% of the income of rich countries to poor countries</b>",
          "Establishment of a <b>global assembly</b> (elected proportionally <br>on global lists) whose only role would be to <b>regulate</b> <br><b>the financial system</b> (notably to avoid financial crises)"))
decrit(m$taxe_ghg_monde[m$transferts_inter >= 5], weights=m$weight[m$transferts_inter >= 5]) # 0.9 500  3  9 8  43  38
decrit(m$taxe_mondiale_riches[m$transferts_inter >= 5], miss=T, weights=m$weight[m$transferts_inter >= 5]) # 150 86/9
decrit(m$taxe_mondiale_capital[m$transferts_inter >= 5], miss=T, weights=m$weight[m$transferts_inter >= 5]) # 130 62/22
decrit(m$revenu_base_mondial[m$transferts_inter >= 5], miss=T, weights=m$weight[m$transferts_inter >= 5]) # 150 70/16
decrit(m$cap_and_share[m$transferts_inter >= 5], miss=T, weights=m$weight[m$transferts_inter >= 5]) # 500 54/21
decrit(m$revenu_max_monde[m$transferts_inter >= 5], miss=T, weights=m$weight[m$transferts_inter >= 5]) # 37/33
decrit(m$revenu_max_monde_3m[m$transferts_inter >= 5], miss=T, weights=m$weight[m$transferts_inter >= 5]) # 39/25
decrit(m$revenu_max_monde_100k[m$transferts_inter >= 5], miss=T, weights=m$weight[m$transferts_inter >= 5]) # 47/30
decrit(m$assemblee_climat[m$transferts_inter >= 5], miss=T, weights=m$weight[m$transferts_inter >= 5]) # 58/24
decrit(m$assemblee_finance[m$transferts_inter >= 5], miss=T, weights=m$weight[m$transferts_inter >= 5]) # 55/26
# no effect of income on world policies
summary(lm(as.numeric(taxe_ghg_monde) ~ revenu, data=m, weights=m$weight)) # 0
summary(lm((taxe_mondiale_riches=='Oui') ~ revenu, data=m, weights=m$weight)) # 0
summary(lm((taxe_mondiale_capital=='Oui') ~ revenu, data=m, weights=m$weight)) # 0
summary(lm((revenu_base_mondial=='Oui') ~ revenu, data=m, weights=m$weight)) # - *
summary(lm((cap_and_share=='Oui') ~ revenu, data=m, weights=m$weight)) # 0
summary(lm((revenu_max_monde=='Oui') ~ revenu, data=m, weights=m$weight)) # 0
summary(lm((revenu_max_monde_3m=='Oui') ~ revenu, data=m, weights=m$weight)) # 0
summary(lm((revenu_max_monde_100k=='Oui') ~ revenu, data=m, weights=m$weight)) # + *
summary(lm((assemblee_climat=='Oui') ~ revenu, data=m, weights=m$weight)) # 0
summary(lm((assemblee_finance=='Oui') ~ revenu, data=m, weights=m$weight)) # 0



#### Union Européenne ####
decrit(e$chomage_eu, miss=T, weights=e$weight) # 1000 36/53/10 #TODO: missings
decrit(e$rdb_eu, miss=T, weights=e$weight) # 1000 54/38/8
decrit(m$impot_eu_benefices, miss=T, weights=m$weight) # 1000 51/28/21
decrit(m$france_et_ue, weights=m$weight)
decrit(m$dette, weights=m$weight)

data_ue <- as.data.frame(matrix( c("renégocier les traités quitte à en sortir en cas d'échec<br>20%","proposer de renégocier les traités<br>31%","ne pas chercher à modifier le fonctionnement de l'UE<br>3%","renforcer l'intégration européenne dans le cadre des traités<br>16%","garder les traités mais désobéir à certaines règles<br>8%","NSP<br>11%","quitter la zone euro et l'UE<br>10%",20,31,3,16,8,11,10), ncol=2))
data_ue_en <- as.data.frame(matrix( c("Renegotiate the treaties and exit if this fails<br>20%","Propose to renegotiate the treaties<br>31%","Do not attempt to modify the European Union’s functioning<br>3%","Reinforce European integration within the frame of the treaties<br>16%","Keep the treaties but disobey to certain rules<br>8%","PNR<br>11%","Leave the eurozone and the European Union<br>10%",20,31,3,16,8,11,10), ncol=2))
colors_ue <- c('orange', 'yellow', 'lightgreen', 'green', 'purple', 'lightgrey', 'red')
do_pie(Data=data_ue, Colors=colors_ue, File="pie_ue", Title="Comment doit se positionner la France par rapport à l'Union Européenne ?", Hover=c("La France doit renégocier les traités pour les rendre plus démocratiques,<br> quitte à sortir de l'UE si les autres États membres refusent","La France doit proposer de renégocier les traités européens","La France doit se conformer aux traités européens et <br>ne pas chercher à modifier le fonctionnement de l'UE","La France doit chercher à renforcer l'intégration européenne <br>dans le cadre des traités","La France ne doit pas chercher à changer les traités européens,<br> mais elle devrait désobéir à certaines règles de l'UE","NSP (Ne sait pas, ne se prononce pas)","La France doit quitter la zone euro et l'Union Européenne"), Display_values = FALSE)
do_pie(Data=data_ue_en, Colors=colors_ue, File="pie_ue_en", Title="How should France position itself in relation to the European Union?", Hover=c("Renegotiate the treaties even if <br>it means to exit in case this fails","Propose to renegotiate the treaties","Comply with the EU rules and do not attempt to<br> modify the European Union’s functioning","Advocate for further European integration within the frame of the treaties","Keep the treaties but disobey to certain rules","PNR (I don't know, I don't want to answer)","Leave the eurozone and the European Union"), Display_values = FALSE)
oui_non(margin_l = 450, file= "bars_ue", vars=c("rdb_eu", "impot_eu_benefices", "chomage_eu"), labels=c("<b>Revenu de base européen</b> (un revenu minimal <br>garantissant à tous les européens de quoi survivre, <br>dépendant du niveau de vie de chaque région)",
                                                                       "<b>Impôt européen sur les bénéfices des sociétés</b> <br>Chaque État pourrait fixer un taux d'imposition sur les sociétés <br>supérieur au taux européen, mais pas inférieur au taux commun", 
                                                                       "<b>Assurance chômage européenne</b> : une partie des cotisations chômage de <br>chaque européen serait versée dans une caisse commune, qui financerait une <br>allocation chômage minimale pour les demandeurs d'emploi européens."))

#### Transferts internationaux ####
# Les envois privés d'argent de France vers les pays pauvres sont d'environ 10^10€/an, à ajouter aux ~0,3% du PIB d'APD : on est sous les 1% de transfert du revenu de la France aux pays pauvres
# http://www.envoidargent.fr/content/etat-des-lieux https://www.fondationdefrance.org/sites/default/files/atoms/files/la_france_qui_donne_dec_2015.pdf
decrit(m$transferts_inter_a, weights=m$weight)
decrit(m$transferts_inter[m$transferts_inter>-1])
decrit(m$transferts_inter[m$transferts_inter>-1], weights=m$weight[m$transferts_inter>-1])
decrit(m$transferts_inter[m$variante_transferts_inter=="s" & m$vague==1], weights=m$weight[m$variante_transferts_inter=="s" & m$vague==1])
decrit(m$transferts_inter[m$variante_transferts_inter=="s" & m$vague==2 & m$transferts_inter>-1], weights=m$weight[m$variante_transferts_inter=="s" & m$vague==2 & m$transferts_inter>-1])
decrit(m$transferts_inter[m$variante_transferts_inter=="i" & m$vague==1], weights=m$weight[m$variante_transferts_inter=="i" & m$vague==1])
decrit(m$transferts_inter[m$variante_transferts_inter=="i" & m$vague==2 & m$transferts_inter>-1], weights=m$weight[m$variante_transferts_inter=="i" & m$vague==2 & m$transferts_inter>-1])
decrit(m$transferts_inter[m$variante_transferts_inter=="c" & m$vague==1], weights=m$weight[m$variante_transferts_inter=="c" & m$vague==1])
decrit(m$transferts_inter[m$variante_transferts_inter=="c" & m$vague==2 & m$transferts_inter>-1], weights=m$weight[m$variante_transferts_inter=="c" & m$vague==2 & m$transferts_inter>-1])
# Transferts_inter selon 6 cas
plot(density(m$transferts_inter[m$variante_transferts_inter=="s" & m$vague==1 & !is.na(m$transferts_inter)], weights=m$weight[m$variante_transferts_inter=="s" & m$vague==1 & !is.na(m$transferts_inter)]/sum(m$weight[m$variante_transferts_inter=="s" & m$vague==1 & !is.na(m$transferts_inter)]), bw=0.5), main="Transferts internationaux désirés (en %)\nselon la formulation et la vague (vague 1 : curseur dans [0;20%] / vague 2 : champ libre)\n(médianes (vague 1/2) : 5/5% (s), 10/7% (i), 7/5% (c), N~260/230 + NSP~70/25)", xlab="Pourcentage désiré de transfert du revenu des pays riches vers les pays pauvres, selon la formulation", col="green", xlim=c(0,20), ylim=c(0,0.25), lwd=1)
lines(density(m$transferts_inter[m$variante_transferts_inter=="s" & m$vague==2 & m$transferts_inter>-1 & !is.na(m$transferts_inter)], weights=m$weight[m$variante_transferts_inter=="s" & m$vague==2 & m$transferts_inter>-1 & !is.na(m$transferts_inter)]/sum(m$weight[m$variante_transferts_inter=="s" & m$vague==2 & m$transferts_inter>-1 & !is.na(m$transferts_inter)]), bw=0.5), col="green", lty=2, lwd=1) 
lines(density(m$transferts_inter[m$variante_transferts_inter=="i" & m$vague==1 & !is.na(m$transferts_inter)], weights=m$weight[m$variante_transferts_inter=="i" & m$vague==1 & !is.na(m$transferts_inter)]/sum(m$weight[m$variante_transferts_inter=="i" & m$vague==1 & !is.na(m$transferts_inter)]), bw=0.5), col="red", lty=1, lwd=1)
lines(density(m$transferts_inter[m$variante_transferts_inter=="i" & m$vague==2 & m$transferts_inter>-1 & !is.na(m$transferts_inter)], weights=m$weight[m$variante_transferts_inter=="i" & m$vague==2 & m$transferts_inter>-1 & !is.na(m$transferts_inter)]/sum(m$weight[m$variante_transferts_inter=="i" & m$vague==2 & m$transferts_inter>-1 & !is.na(m$transferts_inter)]), bw=0.5), col="red", lty=2, lwd=1)
lines(density(m$transferts_inter[m$variante_transferts_inter=="c" & m$vague==1 & !is.na(m$transferts_inter)], weights=m$weight[m$variante_transferts_inter=="c" & m$vague==1 & !is.na(m$transferts_inter)]/sum(m$weight[m$variante_transferts_inter=="c" & m$vague==1 & !is.na(m$transferts_inter)]), bw=0.5), col="blue", lty=1)
lines(density(m$transferts_inter[m$variante_transferts_inter=="c" & m$vague==2 & m$transferts_inter>-1 & !is.na(m$transferts_inter)], weights=m$weight[m$variante_transferts_inter=="c" & m$vague==2 & m$transferts_inter>-1 & !is.na(m$transferts_inter)]/sum(m$weight[m$variante_transferts_inter=="c" & m$vague==2 & m$transferts_inter>-1 & !is.na(m$transferts_inter)]), bw=0.5), col="blue", lty=2)
abline(v=1:20, lty=3, col="grey")
grid()
legend("topright", title="Formulation de la question", title.col="black", legend=c("s: simple, vague 1", "s: simple, vague 2", "i: s + arguments climat & faim, v. 1", "i: s + arguments climat & faim, v. 2", "c: i + colonisation & esclavage, v. 1", "c: i + colonisation & esclavage, v. 2"),
  text.col=c("green", "green","red", "red", "blue", "blue"),lwd=1,col=c("green", "green","red", "red", "blue", "blue"),lty=rep(c(1,2),3))
# *** effet d'ancrage (à la hausse) du curseur, pas d'effet significatif de la formulation (mais p-value=0.07 pour échantillon passable)
summary(lm(transferts_inter ~ variante_transferts_inter + (vague==2),subset=(transferts_inter>-1), data=m, weights=weight))
summary(lm(transferts_inter ~ variante_transferts_inter + (vague==2),subset=(transferts_inter>-1), data=mp, weights=weight))
summary(lm(transferts_inter ~ (vague==2),subset=(transferts_inter>-1 & transferts_inter <= 50), data=m, weights=weight))
summary(rq(transferts_inter ~ variante_transferts_inter + (vague==2),subset=(transferts_inter>-1), data=m, weights=weight))
summary(rq(transferts_inter ~ variante_transferts_inter*(vague==2),subset=(transferts_inter>-1), data=m, weights=weight))
summary(rq(transferts_inter ~ (variante_transferts_inter=="i" & vague==1)+(variante_transferts_inter=="c" & vague==1)+(variante_transferts_inter=="s" & vague==2)+(variante_transferts_inter=="i" & vague==2)+(variante_transferts_inter=="c" & vague==2),subset=(transferts_inter>-1), data=m, weights=weight))
# * effet de la formulation dans le sens attendu pour la vague 1 et dans le sens contraire pour la vague 2 (pas d'effet avec pondérations)
summary(lm(transferts_inter ~ variante_transferts_inter ,subset=(transferts_inter>-1 & vague==1), data=m, weights=weight))
summary(lm(transferts_inter ~ variante_transferts_inter ,subset=(transferts_inter>-1 & vague==2), data=m, weights=weight))

bars_transfert("transferts_inter", data=m, miss=T, title="Transferts internationaux désirés\nQuelle part des revenus des pays riches devrait être transférée aux pays pauvres ? (en %)", weights=T)
bars_transfert_en("transferts_inter", data=m, miss=T, title="Desired international transfers\nWhich proportion of rich countries' income should be transferred to poor countries? (in %)", weights=T)


# plot(ecdf(m$transferts_inter[m$transferts_inter>-1]), mgp=c(3,0.6,0), yaxt='n', lwd=2, col='blue', xlim=c(0,22), pch='', verticals=T, main="Transferts internationaux désirés\nQuel part des revenus des pays riches devrait être transféré aux pays pauvres ? (en %)", xlab="", ylab="Répartition des réponses exprimées (en %)") + grid()
cdf_transferts_inter <- wtd.Ecdf(m$transferts_inter[m$transferts_inter>-1], weights=m$weight[m$transferts_inter>-1])
plot(cdf_transferts_inter$x, 1-as.vector(cdf_transferts_inter$ecdf), mgp=c(3,0.6,0), yaxt='n', lwd=2, col='blue', xlim=c(0,22), type="l", main="Transferts internationaux désirés\nQuel part des revenus des pays riches devrait être transféré aux pays pauvres ? (en %)", xlab="", ylab="Répartition des réponses exprimées (en %)") + grid()
# lines(cdf_transferts_inter$x, as.vector(cdf_transferts_inter$ecdf), mgp=c(3,0.6,0), yaxt='n', lwd=2, col='blue', xlim=c(0,22), type="l", main="Transferts internationaux désirés\nQuel part des revenus des pays riches devrait être transféré aux pays pauvres ? (en %)", xlab="", ylab="Répartition des réponses exprimées (en %)") + grid()
# mtext(do.call(expression, list(bquote(bold("Pourcentage des revenus des pays riches devant être transféré aux pays pauvres")), bquote(paste("Lecture : ", italic("41% (resp. 51%) des français qui se sont exprimés pensent que strictement moins de 5% (resp. 5% ou moins)"))),bquote(italic("des revenus des pays riches devraient être transférés aux pays pauvres")))),side=1,line=2:4)
mtext(do.call(expression, list(bquote(bold("Pourcentage des revenus des pays riches devant être transféré aux pays pauvres")), bquote(paste("Lecture : ", italic("28% (resp. 45%) des français qui se sont exprimés pensent que strictement plus de 10% (resp. 10% ou plus)"))),bquote(italic("des revenus des pays riches devraient être transférés aux pays pauvres")))),side=1,line=2:4)
abline(h=seq(0,1,by=0.1), lty=3, col="grey")
abline(v=seq(0,5,by=1), lty=3, col="grey")
axis(2, at = seq(0,1,by=0.2), labels=c(0,20,40,60,80,100))
# expression(paste("Pourcentage des revenus des pays riches devant être transféré aux pays pauvres\n\n", italic("Lecture : 38% (resp. 51%) des français qui se sont exprimés pensent que moins de 5% (resp. plus de 5%)\n des revenus des pays riches devraient être transférés aux pays pauvres")))
# expression(~bold(Pourcentage~des~revenus~des~pays~riches~devant~être~transféré~aux~pays~pauvres)~Lecture~:~38%~resp.~51%~des~français~qui~se~sont~exprimés~pensent~que~moins~de~5%~resp.~plus~de~5%~\n~des~revenus~des~pays~riches~devraient~être~transférés~aux~pays~pauvres)
# Pourcentage des revenus des pays riches devant être transféré aux pays pauvres\nLecture : 38% (resp. 51%) des français qui se sont exprimés pensent que moins de 5% (resp. plus de 5%)\n des revenus des pays riches devraient être transférés aux pays pauvres
# n =  sum(!is.na(m$transferts_inter) & m$transferts_inter>-1)
# plot(sort(m$transferts_inter[m$transferts_inter>-1]), (1:n)/n, type='s', xlim=c(0,30))

#### Préférences budgétaires ####
stack_bars(c("recherche", "superieur", "secondaire", "primaire", "culture", "infrastructures", "defense", "securite", "justice", "sante", "depense_publique"), labels=c("La recherche scientifique", "L'enseignement supérieur", "L'enseignement secondaire", "L'école maternelle et primaire", "La culture", "Les infrastructures", "La défense et l'armée", "La sécurité intérieure", "La justice", "Le système de santé", "La dépense publique totale"), accord=FALSE, title="Souhaits d'évolution des dépenses publiques (N=1007)", margin=c(2.5,13,0,1), weights=T)
stack_bars(c(rev(c("recherche", "securite", "justice", "secondaire", "superieur", "primaire", "sante", "defense", "infrastructures", "culture")), "depense_publique"), labels=c(rev(c("La recherche scientifique", "La sécurité intérieure", "La justice", "L'enseignement secondaire", "L'enseignement supérieur", "L'école maternelle et primaire", "Le système de santé", "La défense et l'armée", "Les infrastructures", "La culture")), "La dépense publique totale"), accord=FALSE, title="Souhaits d'évolution des dépenses publiques (N=1007)", margin=c(2.5,13,0,1), weights=T)
barres(file="Depenses_publiques", title="<b>Souhaits d'évolution des dépenses publiques</b>", data=data5(c(rev(c("recherche", "securite", "justice", "secondaire", "superieur", "primaire", "sante", "defense", "infrastructures", "culture")), "depense_publique")), sort=FALSE, color=c(color5, "lightgrey"), legend = c(evol5, "NSP"), labels=c(rev(c("La recherche scientifique", "La sécurité intérieure", "La justice", "L'enseignement secondaire", "L'enseignement supérieur", "L'école maternelle et primaire", "Le système de santé", "La défense et l'armée", "Les infrastructures", "La culture")), "La dépense publique totale"))
barres(file="Public_expenditures", title="<b>Desired evolution in public expenditures</b>", data=data5(c(rev(c("recherche", "securite", "justice", "secondaire", "superieur", "primaire", "sante", "defense", "infrastructures", "culture")), "depense_publique")), sort=FALSE, color=c(color5, "lightgrey"), legend = c(evolve5, "PNR"), labels=c(rev(c("Scientific research", "Internal security (police...)", "Justice", "Secondary education", "Higher education", "Nursery and primary schooling", "Healthcare system", "Defense and military", "Infrastructures", "Culture")), "Total public expenditure"))
# stack_bars(c("recherche", "superieur", "secondaire", "primaire", "culture", "infrastructures", "defense", "securite", "justice", "sante", "depense_publique"), labels=c("La recherche scientifique", "L'enseignement supérieur", "L'enseignement secondaire", "L'école maternelle et primaire", "La culture", "Les infrastructures", "La défense et l'armée", "La sécurité intérieure", "La justice", "Le système de santé", "La dépense publique totale"), accord=FALSE, title="Souhaits d'évolution des dépenses publiques (N=1007)", miss=FALSE)
# stack_bars(c("recherche", "securite", "justice", "secondaire", "superieur", "primaire", "sante", "defense", "infrastructures", "culture", "depense_publique"), labels=c("La recherche scientifique", "La sécurité intérieure", "La justice", "L'enseignement secondaire", "L'enseignement supérieur", "L'école maternelle et primaire", "Le système de santé", "La défense et l'armée", "Les infrastructures", "La culture", "La dépense publique totale"), accord=FALSE, title="Souhaits d'évolution des dépenses publiques (N=1007)", miss=FALSE)
decrit(m$contradiction_matrices, weights=m$weight) # 19% contradiction (want decrease of global but no decreae in any category)
decrit(m$grosse_contradiction_matrices, weights=m$weight) # 11% want strong decrease of global but no decreaese
decrit(m$contradiction_ponderee, weights=m$weight) # 58% of contradiction when accounting for weight of each category
decrit(m$grosse_contradiction_ponderee, weights=m$weight) # 37% of people want both a global decrease and implicit increase > 0.5
decrit(m$contradiction_matrice_ponderee, weights=m$weight) # 50% when previous computation is made only with govt spending (not pensions, rsa, unemployment in other questions)
decrit(m$difference_evolution, weights=m$weight) 
sum(m$weight[!is.na(m$difference_evolution) & m$difference_evolution > 0])/sum(m$weight[!is.na(m$difference_evolution)]) # 93% desire higher implicit increase of categorical spending than they allow in their preference
sum(m$weight[!is.na(m$difference_evolution) & m$difference_evolution > 1])/sum(m$weight[!is.na(m$difference_evolution)]) # 72% desire implicit increase of categorical spending higher by > 1 than they allow in their preference

decrit(m$dette, weights=m$weight)
data_dette <- as.data.frame(matrix( c("Ramener le budget à l'équilibre","Ramener le déficit sous les 3% du PNB","Ne pas chercher à réduire le déficit","Ne pas rembourser une partie de la dette","NSP",37,25,5,17,16), ncol=2))
colors_dette <- c('green', 'yellow', 'orange', 'red', 'lightgrey')
do_pie(Data=data_dette, Colors=colors_dette, File="pie_dette", Title="Que faut-il faire de la dette et du déficit publics ?", Hover = c("Il faut continuer à rembourser la dette publique et <br>ramener notre budget à l'équilibre","Il faut continuer à rembourser la dette publique et <br>ramener le déficit sous les 3% du PIB <br>(comme l'y obligent les traités européens)","Il faut continuer à rembourser la dette publique <br>sans chercher à réduire le déficit","Il faut choisir de ne pas rembourser une partie<br> de la dette (ie. la restructurer)","NSP (Ne sait pas, ne se prononce pas)"), Display_values=TRUE)

data_budget <- as.data.frame(matrix( c("Santé", "Retraites", "Justice", "Sécurité intérieure", "Défense", "Infrastructures", "Culture", "École maternelle et primaire", "Enseignement secondaire", "Enseignement supérieur", "Recherche scientifique", "Allocations chômage","RSA", 202.7,313.4,6.3,12.2,42.2,80,7.3,39.4,52.4,23.9,16.8,37.7,9.3), ncol=2))
colors_budget <- rainbow(13) #, end=1/3)
do_pie(Data=data_budget, Colors=colors_budget, File="pie_budget", Title="Dépense publique en France en 2014<br></b>Principaux postes hors remboursements (39.6% du PIB)<b>", Display_values = TRUE)

##### Autres stack_bars: matrices, multiples... #####
stack_bars(c("actionnaires", "proprietaires", "cadres", "rsa", "chomeurs", "sdf", "etudiants", "heures_supp", "smicards", "retraites", "femmes"), labels=c("actionnaires", "propriétaires", "cadres", "RSA", "chômeurs", "sans-abri", "étudiant·e·s", "rémunération des heures supplémentaires", "smicards", "retraité·e·s", "femmes"), accord=FALSE, title="Souhaits d'évolution des revenus de différentes catégories (N=1007)", weights=T)
stack_bars(c("loyers", "pib", "actionnaires", "proprietaires", "cadres", "rsa", "chomeurs", "sdf", "etudiants", "smicards", "retraites", "femmes"),, labels=c("Les loyers*", "Le PIB par habitant*", "Les actionnaires", "Les propriétaires", "Les cadres", "Le RSA", "Les chômeurs", "Les sans-abri", "Les étudiant·e·s", "Les smicards", "Les retraité·e·s", "Les femmes"), accord=FALSE, title="Souhaits d'évolution des revenus de différentes catégories (N=1007/N*=545)", margin=c(2.5,10,0,7), weights=T)
barres(file="categories", title="<b>Souhaits d'évolution des revenus de différentes catégories</b>", data=data5(c("loyers", "pib", "actionnaires", "proprietaires", "cadres", "rsa", "chomeurs", "sdf", "etudiants", "smicards", "retraites", "femmes")), sort=FALSE, color=c(color5, "lightgrey"), legend = c(evol5, "NSP"), labels=c("Les loyers", "Le PIB par habitant", "Les actionnaires", "Les propriétaires", "Les cadres", "Le RSA", "Les chômeurs", "Les sans-abri", "Les étudiant·e·s", "Les smicards", "Les retraité·e·s", "Les femmes"))
barres(file="categories_en", title="<b>Desired evolution of income for different categories</b>", data=data5(c("loyers", "pib", "actionnaires", "proprietaires", "cadres", "rsa", "chomeurs", "sdf", "etudiants", "smicards", "retraites", "femmes")), sort=FALSE, color=c(color5, "lightgrey"), legend = c(evolve5, "PNR"), labels=c("Rents", "GDP per capita", "Shareholders", "Owners", "Executives", "RSA (social benefits)", "Unemployed people", "Homeless people", "Students", "Minimum wage", "Retired people", "Women"))
stack_bars(c("taxe_fonciere", "cot_soc", "tva", "rsa" , "isf", "heures_supp"), labels=c("Taxe foncière", "Cotisations sociales", "TVA", "RSA", "ISF", "Rémunération des heures supplémentaires"), accord=FALSE, title="Souhaits d'évolution des revenus de différentes catégories (N=1007)", weights=T)
stack_bars(c("droit_vote_etrangers", "convoquer_constituante", "conserver_constitution", "cyberdemocratie", "voter_budget", "senat_aleatoire", "salaries_ca", "mixed_member_proportional", "plus_democratie", "referendums", "budgets_participatifs", "voter_propositions"), labels=c("Autoriser le droit de vote des étrangers en situation régulière", "Convoquer une Assemblée constituante", "Conserver notre Constitution actuelle", "Créer un site internet officiel de débats et de consultation politique", "Faire voter les citoyens directement sur le budget de l'État", "Remplacer le Sénat par une assemblée de citoyens tirés au sort", "Réserver la moitié des conseils d'administration des grandes Cies à des salariés", "Représentation proportionnelle et locale à l'Assemblée (mixed-member proportional)", "Rendre nos institutions encore plus démocratiques", "Faire voter les réformes importantes par référendum", "Développer les budgets participatifs", "Voter davantage sur des propositions plutôt que sur des personnes"), title="Adhésion à différentes propositions sur le fonctionnement démocratique (N=545)", margin=c(2.5,26,0,4.5), accord=TRUE, cex=0.8, weights=T)
# texte agrandi democratie :
stack_bars(c("droit_vote_etrangers", "cyberdemocratie", "senat_aleatoire", "voter_budget", "salaries_ca", "mixed_member_proportional", "budgets_participatifs", "referendums", "voter_propositions"), labels=c("Autoriser le droit de vote des étrangers en situation régulière", "Créer un site internet officiel de débats et de consultation politique", "Tirer au sort les sénateurs parmi les citoyens", "Faire voter les citoyens directement sur le budget de l'État", "Réserver la moitié des sièges du CA des grandes Cies à des salariés", "Mixed-member proportional: Assemblée proportionnelle d'élus locaux", "Développer les budgets participatifs", "Faire voter les réformes importantes par référendum", "Voter davantage sur des propositions que sur des personnes"), title="Adhésion à différentes propositions sur le fonctionnement démocratique", margin=c(2.5,27.5,0,2.5), accord=TRUE, cex=1, weights=T)
stack_bars(c("droit_vote_etrangers", "convoquer_constituante", "conserver_constitution", "cyberdemocratie", "senat_aleatoire", "voter_budget", "salaries_ca", "mixed_member_proportional", "plus_democratie", "budgets_participatifs", "referendums", "voter_propositions"), labels=c("Autoriser le droit de vote des étrangers en situation régulière", "Convoquer une Assemblée constituante", "Conserver notre Constitution actuelle", "Créer un site internet officiel de débats et de consultation politique", "Tirer au sort les sénateurs parmi les citoyens", "Faire voter les citoyens directement sur le budget de l'État", "Réserver la moitié des sièges du CA des grandes Cies à des salariés", "Mixed-member proportional: Assemblée proportionnelle d'élus locaux", "Rendre nos institutions encore plus démocratiques", "Développer les budgets participatifs", "Faire voter les réformes importantes par référendum", "Voter davantage sur des propositions que sur des personnes"), title="Adhésion à différentes propositions sur le fonctionnement démocratique (N=545)", margin=c(2.5,27,0,5), accord=TRUE, cex=1, weights=T)
barres(file="Democratie", title="<b>Adhésion à différentes propositions sur le fonctionnement démocratique</b>", data=data5(c("droit_vote_etrangers", "convoquer_constituante", "conserver_constitution", "cyberdemocratie", "senat_aleatoire", "voter_budget", "salaries_ca", "mixed_member_proportional", "plus_democratie", "budgets_participatifs", "referendums", "voter_propositions")), sort=T, color=c(color5, "lightgrey"), legend = c(accord5, "NSP"), labels=c("Autoriser le droit de vote des étrangers en situation régulière", "Convoquer une Assemblée constituante", "Conserver notre Constitution actuelle", "Créer un site internet officiel de débats et de consultation politique", "Tirer au sort les sénateurs parmi les citoyens", "Faire voter les citoyens directement sur le budget de l'État", "Réserver la moitié des sièges du CA des grandes Cies à des salariés", "Mixed-member proportional: Assemblée proportionnelle d'élus locaux", "Rendre nos institutions encore plus démocratiques", "Développer les budgets participatifs", "Faire voter les réformes importantes par référendum", "Voter davantage sur des propositions que sur des personnes"))
barres(file="pour_democratie", title="<b>Adhésion à différentes propositions sur le fonctionnement démocratique</b><br>(en proportion, plusieurs choix possibles)",data=data1(c("pour_droit_vote_etrangers", "pour_convoquer_constituante", "pour_conserver_constitution", "pour_cyberdemocratie", "pour_senat_aleatoire", "pour_voter_budget", "pour_salaries_ca", "pour_mixed_member_proportional", "pour_plus_democratie", "pour_budgets_participatifs", "pour_referendums", "pour_voter_propositions")), sort=T, color=c("brown"), showLegend=FALSE, labels=c("Autoriser le droit de vote des étrangers en situation régulière", "Convoquer une Assemblée constituante", "Conserver notre Constitution actuelle", "Créer un site internet officiel de débats et de consultation politique", "Tirer au sort les sénateurs parmi les citoyens", "Faire voter les citoyens directement sur le budget de l'État", "          Réserver la moitié des sièges du CA des grandes Cies à des salariés", "Mixed-member proportional: Assemblée proportionnelle d'élus locaux", "Rendre nos institutions encore plus démocratiques", "Développer les budgets participatifs", "Faire voter les réformes importantes par référendum", "Voter davantage sur des propositions que sur des personnes"), hover=c("Autoriser le droit de vote des étrangers en situation régulière", "Convoquer une Assemblée constituante", "Conserver notre Constitution actuelle", "Créer un site internet officiel de débats et de consultation politique", "Tirer au sort les sénateurs parmi les citoyens", "Faire voter les citoyens directement sur le budget de l'État", "Réserver la moitié des sièges du CA des grandes Cies à des salariés", "Mixed-member proportional: Assemblée proportionnelle d'élus locaux", "Rendre nos institutions encore plus démocratiques", "Développer les budgets participatifs", "Faire voter les réformes importantes par référendum", "Voter davantage sur des propositions que sur des personnes"), legend="empty")
barres(file="Democracy", title="<b>Adhesion to various proposals on democratic functioning</b>", data=data5(c("droit_vote_etrangers", "cyberdemocratie", "senat_aleatoire", "voter_budget", "salaries_ca", "mixed_member_proportional", "budgets_participatifs", "referendums", "voter_propositions")), sort=T, color=c(color5, "lightgrey"), legend = c(agree5, "PNR"), labels=c("Give foreigners with regular status the right to vote", "Create an official website for debate and political consulting", "Randomly draw Senators among citizens", "Make the citizens vote directly for the State budget", "           Allocate half of the seats of big Cies' administrative board to employees", "Mixed-member proportional: proportional Assembly of local representatives", "Develop participatory budgetings", "Vote important reforms by referendum", "Rather vote for propositions than for persons"))
stack_bars(c("baisser_conso_viande", "voitures_electriques", "taxe_ghg_fr",  "electricite_sans_co2", "taxe_ghg_monde", "favoriser_bio", "isoler_batiments", "baisser_conso_energie"), labels=c("Baisser votre consommation de viande rouge (ou la maintenir basse)", "Remplacer le parc automobile par des voitures électriques en libre service", "Instaurer une taxe sur les émissions de gaz à effet de serre en France",  "Arrêter de produire de l'électricité à partir d'hydrocarbures", "Instaurer une taxe mondiale sur les émissions de gaz à effet de serre", "Favoriser l'agriculture biologique et la permaculture", "Isoler thermiquement tous les bâtiments grâce à un emprunt national", "Baisser notre consommation d'énergie"), title="Adhésions à différentes propositions politiques sur le climat (N=545)", margin=c(2.5,26,0,0.8), cex=0.8, accord=T, weights=T)
# texte agrandi climat :
stack_bars(c("baisser_conso_viande", "voitures_electriques", "taxe_ghg_fr",  "electricite_sans_co2", "taxe_ghg_monde", "favoriser_bio", "isoler_batiments", "baisser_conso_energie"), labels=c("Baisser votre consommation de viande rouge (ou la maintenir basse)", "Remplacer le parc automobile par des voitures électriques en libre service", "Instaurer une taxe sur les émissions de gaz à effet de serre en France",  "Arrêter de produire de l'électricité à partir d'hydrocarbures", "Instaurer une taxe mondiale sur les émissions de gaz à effet de serre", "Favoriser l'agriculture biologique et la permaculture", "Isoler thermiquement tous les bâtiments grâce à un emprunt national", "Baisser notre consommation d'énergie"), title="Adhésions à différentes propositions pour lutter contre le réchauffement climatique", margin=c(2.5,28.5,0,1.5), cex=1, accord=T, weights=T)
barres(file="Climat", title="<b>Adhésion à différentes propositions pour lutter contre le réchauffement climatique</b>", data=data5(c("baisser_conso_viande", "voitures_electriques", "taxe_ghg_fr",  "electricite_sans_co2", "taxe_ghg_monde", "favoriser_bio", "isoler_batiments", "baisser_conso_energie")), sort=T, color=c(color5, "lightgrey"), legend = c(accord5, "NSP"), labels=c("Baisser votre consommation de viande rouge (ou la maintenir basse)", "Remplacer le parc automobile par des voitures électriques en libre service", "Instaurer une taxe sur les émissions de gaz à effet de serre en France",  "Arrêter de produire de l'électricité à partir d'hydrocarbures", "Instaurer une taxe mondiale sur les émissions de gaz à effet de serre", "Favoriser l'agriculture biologique et la permaculture", "Isoler thermiquement tous les bâtiments grâce à un emprunt national", "Baisser notre consommation d'énergie"))
barres(file="pour_climat", title="<b>Adhésion à différentes propositions pour lutter contre le réchauffement climatique</b><br>(en proportion, plusieurs choix possibles)",data=data1(c("pour_baisser_conso_viande", "pour_voitures_electriques", "pour_taxe_ghg_fr",  "pour_electricite_sans_co2", "pour_taxe_ghg_monde", "pour_favoriser_bio", "pour_isoler_batiments", "pour_baisser_conso_energie")), sort=T, color=c("brown"), showLegend=FALSE, labels=c("Baisser votre consommation de viande rouge (ou la maintenir basse)", "          Remplacer le parc automobile par des voitures électriques en libre service", "Instaurer une taxe sur les émissions de gaz à effet de serre en France",  "Arrêter de produire de l'électricité à partir d'hydrocarbures", "Instaurer une taxe mondiale sur les émissions de gaz à effet de serre", "Favoriser l'agriculture biologique et la permaculture", "Isoler thermiquement tous les bâtiments grâce à un emprunt national", "Baisser notre consommation d'énergie"), hover=c("Baisser votre consommation de viande rouge (ou la maintenir basse)", "Remplacer le parc automobile par des voitures électriques en libre service", "Instaurer une taxe sur les émissions de gaz à effet de serre en France",  "Arrêter de produire de l'électricité à partir d'hydrocarbures", "Instaurer une taxe mondiale sur les émissions de gaz à effet de serre", "Favoriser l'agriculture biologique et la permaculture", "Isoler thermiquement tous les bâtiments grâce à un emprunt national", "Baisser notre consommation d'énergie"), legend="empty")
barres(file="Climate", title="<b>Adhesion to various proposals to fight climate change</b>", data=data5(c("baisser_conso_viande", "voitures_electriques", "taxe_ghg_fr",  "electricite_sans_co2", "taxe_ghg_monde", "favoriser_bio", "isoler_batiments", "baisser_conso_energie")), sort=T, color=c(color5, "lightgrey"), legend = c(agree5, "PNR"), labels=c("Reduce (or keep at a low level) your consumption of red meat", "Replace the automobile fleet with self-service electric cars", "Establish a tax on emission of greenhouse gases in France",  "  Put an end to the production of electricity using hydrocarbons", "Establish a global tax on emission of greenhouse gases", "Promote biological agriculture and permaculture", "Thermally insulate every building using a national loan", "Reduce our energy consumption"))
stack_bars(c("baisse_smic", "baisse_allocs","liberaliser", "rtt", "subventions", "investissement", "baisse_cotis", "formation"), labels=c("Baisser le salaire minimum (le SMIC)", "Baisser les allocations chômage","Libéraliser le marché du travail en assouplissant le droit du travail", "Inciter des accords d'entreprise de partage du travail associant embauches et réduction du temps de travail", "Subventionner l'embauche des chômeurs longue durée grâce aux allocations chômage économisées", "Lancer un grand plan public d'investissement (notamment pour la transition écologique)", "Baisser les cotisations sociales que paient les entreprises pour qu'elles puissent embaucher", "Financer la formation professionnelle pour tous les chômeurs longue durée"), title="Adhésions à différentes propositions politiques pour lutter contre le chômage (N=307)", accord=FALSE, margin=c(2.5,33,0,1.6), cex=0.8, weights=T)
# texte agrandi contre_chomage :
stack_bars(c("baisse_smic", "baisse_allocs","liberaliser", "rtt", "subventions", "investissement", "baisse_cotis", "formation"), labels=c("Baisser le salaire minimum (le SMIC)", "Baisser les allocations chômage","Libéraliser le marché du travail en assouplissant le droit du travail", "Inciter à des accords associant embauches et réduction du temps de travail", "Subventionner l'embauche des chômeurs longue durée", "Lancer un grand plan public d'investissement (par ex: transition écologique)", "Baisser les cotisations sociales employeurs pour favoriser l'embauche", "Financer la formation professionnelle pour tous les chômeurs longue durée"), title="Adhésions à différentes propositions politiques pour lutter contre le chômage (N=307)", accord=FALSE, margin=c(2.5,30,0,2), weights=T)
barres(file="pour_chomage", title="<b>Adhésion à différentes propositions pour lutter contre le chômage</b><br>(en proportion, plusieurs choix possibles)",data=data1(c("pour_baisse_smic", "pour_baisse_allocs","pour_liberaliser", "pour_rtt", "pour_subventions", "pour_investissement", "pour_baisse_cotis", "pour_formation")), sort=T, color=c("brown"), showLegend=FALSE, labels=c("Baisser le salaire minimum (le SMIC)", "Baisser les allocations chômage","Libéraliser le marché du travail en assouplissant le droit du travail", "Inciter à des accords associant embauches et réduction du temps de travail", "Subventionner l'embauche des chômeurs longue durée", "            Lancer un grand plan public d'investissement (par ex: transition écologique)", "Baisser les cotisations sociales employeurs pour favoriser l'embauche", "Financer la formation professionnelle pour tous les chômeurs longue durée"), hover=c("Baisser le salaire minimum (le SMIC)", "Baisser les allocations chômage","Libéraliser le marché du travail en assouplissant le droit du travail", "Inciter à des accords associant embauches et réduction du temps de travail", "Subventionner l'embauche des chômeurs longue durée", "Lancer un grand plan public d'investissement (par ex: transition écologique)", "Baisser les cotisations sociales employeurs pour favoriser l'embauche", "Financer la formation professionnelle pour tous les chômeurs longue durée"), legend="empty")
barres(file="contre_chomage", title="<b>Adhésion à différentes propositions politiques pour lutter contre le chômage</b>", data=data5(c("baisse_smic", "baisse_allocs","liberaliser", "rtt", "subventions", "investissement", "baisse_cotis", "formation")), sort=T, color=c(color5, "lightgrey"), legend = c(accord5, "NSP"), labels=c("Baisser le salaire minimum (le SMIC)", "Baisser les allocations chômage","Libéraliser le marché du travail en assouplissant le droit du travail", "Inciter à des accords associant embauches et réduction du temps de travail", "Subventionner l'embauche des chômeurs longue durée", "Lancer un grand plan public d'investissement (par ex: transition écologique)", "Baisser les cotisations sociales employeurs pour favoriser l'embauche", "Financer la formation professionnelle pour tous les chômeurs longue durée"), margin_l=400)
barres(file="unemployment", title="<b>Adhesion to different political propositions to tackle unemployment</b>", data=data5(c("baisse_smic", "baisse_allocs","liberaliser", "rtt", "subventions", "investissement", "baisse_cotis", "formation")), sort=T, color=c(color5, "lightgrey"), legend = c(agree5, "PNR"), labels=c("Decrease the minimum wage", "Decrease unemployment pensions","Liberalize the labor market by making labor law less rigid", "Promote agreements combining hirings and reduction of working time", "Subsidize the hiring of long-term unemployed people", "Launch a great public investment plan (e.g.: ecological transition)", "          Decrease social contributions for employers in order to boost employment", "Finance professional training for long-term unemployed people"))
stack_bars(c("loyers", "taxe_revenus_fonciers", "taxe_plus_values", "taxe_dividendes", "is", "pib"), labels=c("Les loyers", "La taxation des revenus fonciers", "La taxation des plus-values de court-terme", "La taxation des dividendes", "L'impôt sur les bénéfices des sociétés (IS)", "Le produit intérieur brut par habitant (PIB/hab)" ), title="Souhaits d'évolution de différents impôts et montants (N=545)", accord=FALSE, margin=c(2.5,20,0,5), weights=T)
# impôts combinés :
stack_bars(c("taxe_fonciere", "cot_soc", "tva", "taxe_revenus_fonciers", "taxe_plus_values", "is", "taxe_dividendes" , "isf", "heures_supp"), labels=c("Taxe foncière", "Cotisations sociales", "TVA", "Taxation des revenus fonciers*", "Taxation des plus-values de court-terme*", "Impôt sur les bénéfices des sociétés (IS)*", "Taxation des dividendes*", "ISF", "Rémunération des heures supplémentaires"), title="Souhaits d'évolution de différents impôts (N=1007/N*=545)", accord=FALSE, margin=c(2.5,18,0,6), weights=T)
stack_bars(c("rsa"), labels=c("RSA"), margin=c(2.5,6,0,1.5), width=0.4, weights=T)
stack_bars(c("rsa"), en= TRUE, labels=c("RSA (welfare benefit)"), margin=c(2.5,9,0,3.5), width=0.4, weights=T)
barres(file="rsa en", title="<b>Desired evolution of welfare benefits</b>", data=data5(c("rsa")), color=c(color5, "lightgrey"), legend = c(evolve5, "PNR"), labels=c("RSA"))
barres(file="impots", title="<b>Souhaits d'évolution de différents impôts</b>", data=data5(c("taxe_fonciere", "cot_soc", "tva", "taxe_revenus_fonciers", "taxe_plus_values", "is", "taxe_dividendes" , "isf", "heures_supp")), sort=T, color=c(color5, "lightgrey"), legend = c(evol5, "NSP"), labels=c("Taxe foncière", "Cotisations sociales", "TVA", "Taxation des revenus fonciers", "Taxation des plus-values de court-terme", "Impôt sur les bénéfices des sociétés (IS)", "Taxation des dividendes", "ISF", "Rémunération des heures supplémentaires"))
barres(file="taxes", title="<b>Desired evolution of the amount of different taxes</b>", data=data5(c("taxe_fonciere", "cot_soc", "tva", "taxe_revenus_fonciers", "taxe_plus_values", "is", "taxe_dividendes" , "isf", "heures_supp")), sort=T, color=c(color5, "lightgrey"), legend = c(evolve5, "PNR"), labels=c("Property tax", "Social contributions", "Value added tax", "Taxation of rental income", "Short-term capital gain taxation", "Corporate tax", "Taxation of dividends", "Wealth tax", "Remuneration of overtime"))
stack_bars(c("taxe_ghg_monde"), en=TRUE, title="What do you think of a global tax on greenhouse gas emissions?", labels=c(""), accord=TRUE, margin=c(2.5,6,0,1.5), width=0.4, weights=T)

data_notation <- t(matrix(c(t(data5(c("note_actuel", "note_egalitaire", "note_personnalise", "note_utilitarien", "note_rawlsien", "note_mediane", "note_mediane_rdb"), miss=FALSE)), 
                            c(length(which(is.na(m$note_actuel)))/(nrow(m)-length(which(is.na(m$note_actuel)))),length(which(is.na(e$note_egalitaire)))/(nrow(e)-length(which(is.na(e$note_egalitaire)))),length(which(is.na(e$note_personnalise)))/(nrow(e)-length(which(is.na(e$note_personnalise)))),length(which(is.na(e$note_utilitarien)))/(nrow(e)-length(which(is.na(e$note_utilitarien)))),length(which(is.na(e$note_rawlsien)))/(nrow(e)-length(which(is.na(e$note_rawlsien)))),length(which(is.na(p$note_mediane)))/(nrow(p)-length(which(is.na(p$note_mediane)))),length(which(is.na(p$note_mediane_rdb)))/(nrow(p)-length(which(is.na(p$note_mediane_rdb)))))), ncol=6))
barres(file="notations", title="<b>Notations de différentes distributions de niveaux de vie</b>", data=data5(c("note_actuel", "note_egalitaire", "note_personnalise", "note_utilitarien", "note_rawlsien", "note_mediane", "note_mediane_rdb"), miss=FALSE), sort=T, nsp=FALSE, color=c(color5), legend = c(-2:0, "+1", "+2"), hover=c(-2:0, "+1", "+2"), labels=c("Actuelle", "Égalitaire", "Personnalisée", "Issue de l'optimisation utilitarienne", "Issue de l'optimisation rawlsienne", "Issue des réponses médianes, algo av&des", "Issue des réponses médianes, algo RdB"))
barres(file="notations_nsp", title="<b>Notations de différentes distributions de niveaux de vie</b>", data=data_notation, sort=T, nsp=T, color=c(color5, "lightgrey"), legend = c(-2:0, "+1", "+2", "NSP"), hover=c(-2:0, "+1", "+2", "NSP"), labels=c("Actuelle", "Égalitaire", "Personnalisée", "Issue de l'optimisation utilitarienne", "Issue de l'optimisation rawlsienne", "Issue des réponses médianes, algo av&des", "Issue des réponses médianes, algo RdB"))
barres(file="notations_en", data=data_notation, sort=T, nsp=T, color=c(color5, "lightgrey"), legend = c(-2:0, "+1", "+2", "PNR"), hover=c(-2:0, "+1", "+2", "PNR"), labels=c("Actual", "Egalitarian", "Personalized", "Derived from utilitarian optimization", "Derived from rawlsian optimization", "Median proposed reform", "Demogrant median proposed reform"))

priorite_values <- c(sum(m$weight[which(m$stabilite_emploi==T)])/997, sum(m$weight[which(m$pouvoir_achat==T)])/997, sum(m$weight[which(m$insecurite)])/997 , sum(m$weight[which(m$environnement)])/997 ,  sum(m$weight[which(m$soins)])/997 ,sum(m$weight[which(m$logement)])/997 ,sum(m$weight[which(m$vivre_ensemble)])/997 ,sum(m$weight[which(m$education)])/997 ,sum(m$weight[which(m$discriminations)])/997 ,sum(m$weight[which(m$avancer_retraite)])/997 ,sum(m$weight[which(m$conditions_travail)])/997 ,sum(m$weight[which(m$moins_travail)])/997 ,sum(m$weight[which(m$moins_surveillance)])/997)
priorite_labels <- c("Je veux un emploi stable", 'De meilleurs revenus', 'Plus de sécurité', "Une action plus forte pour l'environnement", 
                     'Être mieux soigné et mieux remboursé', 'Mons dépenser pour mon logement', 'Un meilleur vivre-ensemble', 'Une meilleure éducation',
                     "Que cessent les discriminations", 'Partir à la retraite plus tôt', 'De meilleures conditions de travail', 'Plus de temps pour mes loisirs', "Être moins surveillé-e")[order(priorite_values)]
priorite_values <- sort(priorite_values)
barres(file="priorites", title="<b>Qu'est-ce qu'il est prioritaire de changer tout de suite</b><br>dans votre vie quotidienne ? Je veux... (3 choix possibles)",data=matrix(priorite_values, ncol=length(priorite_values)), sort=T, color=c("brown"), showLegend=FALSE, labels=priorite_labels, hover=priorite_labels, legend="empty")
before_par <- par()
par(las=2, mar=c(3,17,4,3))
bp <- barplot(matrix(priorite_values, nrow=1), main="Qu'est-ce qu'il est prioritaire de changer tout de suite \ndans votre vie quotidienne ? Je veux... (3 choix possibles)", horiz=TRUE, names.arg=priorite_labels, 
        col='brown', border=NA)
text(x=priorite_values, y=0.6+1.2*(0:12), labels=paste(round(100*priorite_values), '%'), pos=4, xpd=NA)
par(before_par)

#### Réfugiés ####
decrit(m$quota_commune, miss=T, weights=m$weight)
decrit(as.numeric(m$refugies), miss=T, weights=m$weight) # 1: non posé ; 2 -> 7 Accepter tous -> aucun  (5: autant); 8: commune; 9: parrainage; 10: NSP

data_refugies <- as.data.frame(matrix( c("refuser tout réfugié<br>19%","accepter moins de réfugiés<br>20%","accepter autant de réfugiés qu'actuellement<br>12%","accepter plus de réfugiés<br>7%","accepter beaucoup plus de réfugiés<br>5%","accepter tous les réfugiés<br>10%","laisser à chaque commune le choix de son quota<br>13%","instaurer le parrainage de réfugié<br>4%","NSP<br>11%",19,20,12,7,5,10,13,4,11), ncol=2))
data_refugies_en <- as.data.frame(matrix( c("Refuse all refugees<br>19%","Accept less refugees<br>20%","Accept as much refugees as we already do<br>12%","Accept more refugees<br>7%","Accept many more refugees<br>5%","Accept all refugees<br>10%","Leave to each township the choice of its refugee quota<br>13%","Adopt 'refugee sponsorship'<br>4%","PNR<br>11%",19,20,12,7,5,10,13,4,11), ncol=2))
colors_refugies <- c('red', 'orange', 'yellow', 'lightgreen', 'green', 'darkgreen', 'purple', 'blue', 'lightgrey')
do_pie(Data=data_refugies, Colors=colors_refugies, File="pie_refugies", Title="Quelle politique faut-il adopter vis-à-vis des réfugiés qui veulent entrer en France ?", Hover=c("Il faut refuser à tout réfugié l'entrée sur le territoire","Il faut accepter moins de réfugiés qu'actuellement","Il faut accepter un quota de réfugiés correspondant au nombre actuel<br> de personnes accueillies légalement sur le territoire","Il faut accepter un quota de réfugiés plus élevé qu'actuellement","Il faut accepter beaucoup plus de réfugiés qu'actuellement","Il faut accepter tous les réfugiés qui fuient la guerre ou la persécution","Il faut laisser à chaque commune le choix de fixer son quota de réfugiés","Il faut autoriser chaque français à parrainer un réfugié,<br> le parrain serait responsable de l'intégration<br> et si besoin de l'hébergement et de l'entretien du réfugié","NSP (Ne sait pas, ne se prononce pas)"), Display_values = FALSE)
do_pie(Data=data_refugies_en, Colors=colors_refugies, File="pie_refugees", Title="Which policy must be adopted regarding refugees that wish to enter France?", Hover=c("Refuse all refugees","Accept less refugees","Accept as much refugees as we already do","Accept more refugees","Accept many more refugees","Accept all refugees who flee from war or persecution","Leave to each township the choice of its refugee quota","Authorize each French to sponsor one refugee: <br>s-he would responsible for his-her integration and if needed, housing and feeding","PNR (I don't know, I don't want to answer)"), Display_values = FALSE)
oui_non(c("quota_commune", "parrainage_refugie"), "bars_refugies", c("Fixer les quotas de réfugiés par commune <br>plutôt qu'au niveau national ou européen", "Autoriser chaque français à parrainer un réfugié, <br>càd à être responsable de son intégration, de son <br>hébergement et de son entretien si besoin"))

#### Retraites #### TODO: rajouter des liens vers les variables
oui_non(margin_l = 270, file="bars_system_social", vars=c("retraite_modulee_csp", "retraite_libre"),
      labels=c("Prise en compte de l'espérance de vie du métier <br>exercé pour le calcul du montant de la retraite",
               "Laisser la liberté aux salariés de choisir le montant <br>qu'ils cotisent pour la retraite (dans une fourchette <br>autorisée) ainsi que leur âge de départ à la retraite <br>(pourvu qu'ils aient cotisé plus qu'une certaine somme)"))

#### Impôts sur les revenus ####
oui_non(file="bars_impot", vars=c("fusion_irpp_cotsoc", "simplification", "quotient", "deduction_loyer_impots", "rsa_jeune", "taxation_capital"),
        labels=c("Fusion de l'impôt sur les revenus<br> et des cotisations sociales",
                 "Simplification du système fiscal français",
                 "Pour le quotient conjugal plutôt que l'individualisation<br> de l'impôt sur les revenus",
                 "Déduction des loyers dans le calcul du<br> revenu imposable couplée à la baisse de la <br>taxe foncière pour les propriétaires accédants",
                 "Extension du RSA au moins de 25 ans sans ressource", 
                 "Taxer davantage le capital et moins le travail"))
summary(lm((deduction_loyer_impots=="Oui") ~ Patrimoine, data=m, weights=m$weight))
decrit(m$deduction_loyer_impots[m$Patrimoine>5])
decrit(m$deduction_loyer_impots[m$Patrimoine<2])
data_capital <- cbind(data_seuils(variable="part_impot_capital", seuils=c(15, 23, 23.01, 35), miss=T))
barres(data=data_capital, file="part_impot_capital", title="<b>Part des prélèvements obligatoires provenant de la taxation du capital et des revenus du capital</b><br>Cette part est actuellement de 23% : à quelle valeur devrait-elle être portée ?", labels=c(""), color=c(color5, 'lightgrey'), hover=c("Moins de 15%", "De 15,1% à 22,9%", "23%", "De 23% à 34,9%", "35% ou plus", "NSP (Ne sait pas, ne se prononce pas)"), legend=c("Moins de 15%", "De 15,1% à 22,9%", "23%", "De 23% à 34,9%", "35% ou plus", "NSP"), nsp=T, sort=FALSE, showLegend=T, margin_r=0, margin_l=NA)

decrit(m$quotient, miss=T)
decrit(m$quotient_progressif, miss=T)
decrit(m$individualisation_progressive, miss=T)
decrit(m$individualisation, miss=T)
m$individualisation_q <- m$quotient
m$individualisation_q[m$quotient=="Pour"] <- "Contre"
m$individualisation_q[m$quotient=="Contre"] <- "Pour"
oui_non(file="quotients", vars=c("individualisation", "individualisation_q", "quotient_progressif", "individualisation_progressive"),
        labels=c("Individualisation mise en avant", "Quotient conjugal mis en avant", "Individualisation mise en avant, version progressive :<br>seuls les couples mariés avant la réforme<br>bénéficieraient du quotient conjugal", "Quotient conjugal mis en avant, version progressive :<br>seuls les couples mariés avant la réforme<br>bénéficieraient du quotient conjugal"), title="<b>Êtes-vous favorable à l'individualisation de l'impôt sur les revenus ?</b><br>Réponses selon différentes formulations de la question")
oui_non(file="quotient_en", vars=c("individualisation", "individualisation_q", "quotient_progressif", "individualisation_progressive"),
        labels=c("Emphasis on individualisation", "Emphasis on marital quotient", "Emphasis on individualisation, progressive version", "Emphasis on marital quotient, progressive version"))


mean(m$note_mediane, na.rm=T)
mean(m$note_mediane_rdb, na.rm=T)
wtd.mean(m$note_mediane, weights=m$weight)
wtd.mean(m$note_mediane_rdb, weights=m$weight)
wtd.mean(m$note_rawlsien, weights=m$weight)
wtd.mean(m$note_utilitarien, weights=m$weight)
wtd.mean(m$note_egalitaire, weights=m$weight)
wtd.mean(m$note_actuel, weights=m$weight)
wtd.mean(m$note_personnalise, weights=m$weight)

##### Approbation #####
decrit(m$approbation_mediane, miss=T, weights=m$weight)
decrit(m$approbation_mediane[grepl("Non", m$incompris)], miss=T, weights=m$weight[grepl("Non", m$incompris)])
decrit(m$approbation_mediane_rdb, miss=T, weights=m$weight)
decrit(m$approbation_mediane_aid, miss=T, weights=m$weight)
decrit(m$approbation_moyenne, miss=T, weights=m$weight)
summary(lm((approbation_mediane=="Oui") ~ Age + I(Age^2) + sexe + revenu + I(revenu^2) + taille_agglo + Patrimoine_futur + grepl("Oui", incompris) + qualite + gauche_droite + I(gauche_droite^2) + Diplome, data=m))
summary(lm((approbation==T) ~ variante_approbation + + Age + I(Age^2) + sexe + revenu + I(revenu^2) + as.numeric(taille_agglo) + Patrimoine_futur + grepl("Oui", incompris) + qualite + gauche_droite + I(gauche_droite^2) + Diplome + csp, data=m))
summary(lm((approbation==T) ~ gauche_droite + Age + sexe + Diplome + revenu + I(revenu^2) + I(Age^2) + qualite + grepl("Oui", incompris), data=m, weights=weight)) # seul gauche_droite corrélé
summary(lm(is.missing(approbation) ~ gauche_droite + Age + sexe + Diplome + revenu + I(revenu^2) + I(Age^2) + qualite + grepl("Oui", incompris), data=m, weights=weight)) # seul gauche_droite corrélé
summary(lm((approbation_mediane=="Oui") ~ gauche_droite + Age + sexe + Diplome + revenu + I(revenu^2) + I(Age^2) + qualite + grepl("Oui", incompris), data=m, weights=weight)) # seul gauche_droite corrélé
summary(lm((approbation_mediane=="NSP") ~ gauche_droite + Age + sexe + Diplome + revenu + I(revenu^2) + I(Age^2) + qualite + grepl("Oui", incompris), data=m, weights=weight)) # seul gauche_droite corrélé
summary(lm((approbation_mediane=="NSP") ~ grepl("Oui", incompris), data=m, weights=weight)) # ***
summary(lm((approbation_mediane=="Oui") ~ grepl("Oui", incompris), data=m, weights=weight)) # 
decrit(m$approbation_mediane[m$gauche_droite>0], miss=T, weights=m$weight[m$gauche_droite>0])
decrit(m$approbation_mediane[m$gauche_droite>0], miss=T)
decrit(m$approbation_mediane[m$gauche_droite==1], miss=T, weights=m$weight[m$gauche_droite>0])
decrit(m$approbation_mediane[m$gauche_droite==1], miss=T)


#### Écarts maximaux ####
decrit(m$rapport_max_fr, weights=m$weight)
decrit(m$rapport_max_monde, weights=m$weight)
decrit(m$revenu_max_fr_100k, weights=m$weight)
decrit(m$revenu_max_fr_3m, weights=m$weight)
decrit(m$revenu_max_monde_100k, weights=m$weight)
decrit(m$revenu_max_monde_3m, weights=m$weight)
seuils_rapport_max <- c(5, 10, 20, Inf)
data_rapport_max <- cbind(data_seuils(variable="rapport_max_fr", seuils=seuils_rapport_max, miss=T, closed_left = FALSE, closed_right=T), data_seuils(variable="rapport_max_monde", seuils=seuils_rapport_max, miss=T, closed_left = FALSE, closed_right=T))
barres(data=data_rapport_max, file="rapport_max", title="<b>Écart de revenus maximal désiré</b><br>Idéalement, quel devrait être le rapport maximal entre le revenu le plus élevé et le revenu le plus faible, ... ?", labels=c("en France", "sur Terre"), color=c(color5[1:4], 'blue', 'lightgrey'), hover=c("5 ou moins", "De 5,1 à 10", "De 10,1 à 20", "Plus de 20", "Il pourrait y avoir des gens infiniment plus riches que d'autres dans une société idéale.", "NSP (Ne sait pas, ne se prononce pas)"), legend=c("5 ou moins", "De 5,1 à 10", "De 10,1 à 20", "Plus de 20", "Pas de limite désirée", "NSP"), nsp=T, sort=FALSE, showLegend=T, margin_r=0, margin_l=NA)
barres(data=cbind(data_seuils(variable="rapport_max", seuils=seuils_rapport_max, miss=T, closed_left = FALSE, closed_right=T)), file="rapport_max1", title="<b>Écart de revenus maximal désiré</b><br>Idéalement, quel devrait être le rapport maximal entre le revenu le plus élevé et le revenu le plus faible ?", labels=c(" "), color=c(color5[1:4], 'blue', 'lightgrey'), hover=c("5 ou moins", "De 5,1 à 10", "De 10,1 à 20", "Plus de 20", "Il pourrait y avoir des gens infiniment plus riches que d'autres dans une société idéale.", "NSP (Ne sait pas, ne se prononce pas)"), legend=c("5 ou moins", "De 5,1 à 10", "De 10,1 à 20", "Plus de 20", "Pas de limite désirée", "NSP"), nsp=T, sort=FALSE, showLegend=T, margin_r=0, margin_l=NA)


#### Orientation politique ####
sum(m$weight[which(m$humaniste==T)])/sum(T, weights=m$weight)
sum(m$weight[which(m$nationaliste==T)])/sum(T, weights=m$weight)
sum(m$weight[which(m$marxiste==T | m$communiste==T)])/sum(T, weights=m$weight)
sum(m$weight[which(m$ecologiste==T)])/sum(T, weights=m$weight)
sum(m$weight[which(m$feministe==T)])/sum(T, weights=m$weight)
sum(m$weight[which(m$conservateur==T)])/sum(T, weights=m$weight)
sum(m$weight[which(m$liberal==T)])/sum(T, weights=m$weight)
sum(m$weight[which(m$libertarien==T | m$anarchiste==T)])/sum(T, weights=m$weight)
sum(m$weight[which(m$extr_gauche==T)])/sum(T, weights=m$weight)
sum(m$weight[which(m$gauche==T)])/sum(T, weights=m$weight)
sum(m$weight[which(m$centre==T)])/sum(T, weights=m$weight)
sum(m$weight[which(m$droite==T)])/sum(T, weights=m$weight)
sum(m$weight[which(m$extr_droite==T)])/sum(T, weights=m$weight)
sum(m$weight[which(m$orientation=="NSP")])/sum(T, weights=m$weight)
# orientation_values <- c(12.64892, 6.768724, 0.6337582, 14.2147, 6.09911, 2.30835, 7.1435, 1.24560, 1.93721, 22.5763, 11.82641, 17.16722, 10.0439)
# orientation_labels <- c('Humaniste', 'Nationaliste', 'Marxiste ou communiste', 'Écologiste', 'Féministe', 'Conservateur', 'Libéral', 'Anarchiste ou libertarien', "D'extrême gauche", 'De gauche', 'Du centre', 'De droite', "D'extrême droite")[order(orientation_values)]
# orientation_values <- factor(orientation_values, levels = sort(orientation_values))
# orientation <- plot_ly(x = orientation_values, y = orientation_labels, type = 'bar', orientation = 'h')  %>%
#   layout(title = 'Comment vous définiriez-vous ? (en %, plusieurs choix possibles)', margin = list(l = 200, r = 0, t = 30, b = 0), 
#          xaxis = list(title = "", showgrid = TRUE, showline = TRUE, showticklabels = TRUE, zeroline = TRUE))
# api_create(orientation, filename="bars_orientation", sharing="public")

orientation_values <- c(12.64892, 6.768724, 0.6337582, 14.2147, 6.09911, 2.30835, 7.1435, 1.24560, 1.93721, 22.5763, 11.82641, 17.16722, 10.0439)
orientation_labels <- c("NSP", c('Humaniste', 'Nationaliste', 'Marxiste ou communiste', 'Écologiste', 'Féministe', 'Conservateur', 'Libéral', 'Anarchiste ou libertarien', "D'extrême gauche", 'De gauche', 'Du centre', 'De droite', "D'extrême droite")[order(orientation_values)])
orientation_values <- c(11.71062, sort(orientation_values))
barres(file="orientation", title="<b>Comment vous définiriez-vous ?</b><br>(en %, plusieurs choix possibles)",data=matrix(orientation_values/100, ncol=length(orientation_values)), sort=T, color=c("brown"), showLegend=FALSE, labels=orientation_labels, hover=orientation_labels, legend="empty")
before_par <- par()
par(las=2, mar=c(2,11,4,3))
bp <- barplot(matrix(orientation_values, nrow=1), main="Comment vous définiriez-vous ?\n(en %, plusieurs choix possibles)", horiz=TRUE, names.arg=orientation_labels, 
        col='brown', border=NA)
text(x=orientation_values, y=0.6+1.2*(0:13), labels=paste(round(orientation_values), '%'), pos=4, xpd=NA)
par(before_par)


gauche_droite <- function(data=m) {
  colors <-  c('red', 'pink', 'lightblue', 'blue', 'darkblue', 'lightgrey') # c("red", "orange", "yellow", "green", "darkgreen") # rainbow(5, end=1/3)
  # mat <- c(length(which(data[['gauche_droite']]==-2))/length(which(!is.missing(data[['gauche_droite']]))), length(which(data[['gauche_droite']]==-1))/length(which(!is.missing(data[['gauche_droite']]))), length(which(data[['gauche_droite']]==0))/length(which(!is.missing(data[['gauche_droite']]))), length(which(data[['gauche_droite']]==1))/length(which(!is.missing(data[['gauche_droite']]))), length(which(data[['gauche_droite']]==2))/length(which(!is.missing(data[['gauche_droite']]))),length(which(is.missing(data[['gauche_droite']]) & !is.na(data[['gauche_droite']])))/length(which(!is.na(data[['gauche_droite']]))))
  mat <- c(sum(data[['weight']][which(data[['gauche_droite']]==-2)])/sum(data[['weight']][!is.missing(data[['gauche_droite']])]), sum(data[['weight']][which(data[['gauche_droite']]==-1)])/sum(data[['weight']][!is.missing(data[['gauche_droite']])]), sum(data[['weight']][which(data[['gauche_droite']]==0)])/sum(data[['weight']][!is.missing(data[['gauche_droite']])]), sum(data[['weight']][which(data[['gauche_droite']]==1)])/sum(data[['weight']][!is.missing(data[['gauche_droite']])]), sum(data[['weight']][which(data[['gauche_droite']]==2)])/sum(data[['weight']][!is.missing(data[['gauche_droite']])]))
  print(length(mat))
  values <- c("Extrême gauche", "Gauche", "Centre", "Droite", "Extrême droite", "NSP")
  prop_nsp <- sum(data[['weight']][which(is.missing(data[['orientation']]))])/sum(data[['weight']][!is.missing(data[['gauche_droite']])])
  widths <- c(0.12,0.1,0.1,0.1,0.1,0.05)
  before_par <- par()
  par(mar=c(2.5,0,0,0), oma=c(0,0,1.5,0))
  frame()
  abline(v=seq(0,1,by=0.1), lty=3, col="grey")
  axis(1, at = seq(0,1,by=0.1))
  barplot(matrix(c(mat, rep(0,6),prop_nsp), ncol=2), width=0.7/2, horiz=TRUE, xaxt='n', add=TRUE, col=colors, border=NA, ylim=c(0,1), legend.text=values, las=1, args.legend=list(horiz=TRUE, bty='o', box.lwd=0, xjust=1, text.width=widths, x.intersp=0.3, x="topright")) # ncol=3, inset=-0.3
  title("Comment vous définiriez-vous ?", outer=TRUE)
  par(before_par)
}
gauche_droite()

stack_3 <- function(var, legend, data=m, labels="", title=NA, widths=c(0.2,0.2,0.23,0.05), margin=c(2.5,0,0,3), weights=TRUE) {
  colors <-  c('darkblue', 'blue', 'lightblue', 'lightgrey') # c("red", "orange", "yellow", "green", "darkgreen") # rainbow(5, end=1/3)
  if (!weights) {
    n <- length(which(!is.missing(data[[var]])))
    mat <- c(length(which(data[[var]]==legend[1]))/n, length(which(data[[var]]==legend[2]))/n, length(which(data[[var]]==legend[3]))/n, length(which(data[[var]]==legend[4]))/n ) }
  else { 
    n <- sum(data[['weight']][!is.missing(data[[var]])])
    mat <- c(sum(data[['weight']][which(data[[var]]==legend[1])])/n, sum(data[['weight']][which(data[[var]]==legend[2])])/n, sum(data[['weight']][which(data[[var]]==legend[3])])/n, sum(data[['weight']][which(data[[var]]==legend[4])])/n ) }
  before_par <- par()
  titre <- 0
  if (!is.na(title)) { titre <- 1.5 }
  par(mar=margin, oma=c(0,0,titre,0))
  frame()
  abline(v=seq(0,1,by=0.1), lty=3, col="grey")
  axis(1, at = seq(0,1,by=0.1))
  barplot(matrix(c(mat), ncol=1), width=0.7, horiz=TRUE, add=TRUE, xaxt='n', col=colors, names.arg = labels, cex.names = 1, border=NA, ylim=c(0,1), legend.text=legend, las=1, args.legend=list(horiz=TRUE, bty='o', box.lwd=0, xjust=1, text.width=widths, x.intersp=0.3, x="topright")) # ncol=3, inset=-0.3
  title(title, outer=TRUE)
  par(before_par)
  # legend("topright", fill=colors, legend=values, ncol=2)
}
stack_3(weights=TRUE,var="interet_politique", legend=c("Beaucoup", "Un peu", "Presque pas (ou pas du tout)", "NSP"), title="À quel point êtes-vous intéressé·e par la politique ?")

data_cbs <- as.data.frame(matrix( c("Oui, c'est une bonne idée", "Il faudrait mieux prendre en compte l'avis de chacun,<br> mais à l'aide d'une autre méthode", "Non, le système actuel est satisfaisant", "NSP", 44, 29, 14, 13), ncol=2))
data_cbs_en <- as.data.frame(matrix( c("Yes, it's a good idea", "Anyone's opinion should be taken<br>into account, but with another method", "No, the current system is satisfactory", "PNR", 44, 29, 14, 13), ncol=2))
colors_cbs <- c('red', 'yellow', 'blue', 'lightgrey')
do_pie(Data=data_cbs, Colors=colors_cbs, File="pie_choix_bareme_sondage", Title="Pensez-vous qu'il faudrait déterminer le barème d'imposition préféré des citoyens à partir d'un <br>sondage, puis soumettre la proposition qui ressortirait du sondage à référendum ?", Hover=c("Oui, c'est une bonne idée", "Il faudrait mieux prendre en compte l'avis de chacun, mais à l'aide d'une autre méthode", "Non, le système actuel est satisfaisant", "NSP (Ne sait pas, ne se prononce pas)"))
do_pie(Data=data_cbs_en, Colors=colors_cbs, File="pie_choix_bareme_sondage_en", Title="What do you think of determining the income tax schedule preferred <br>by the citizens in a survey, and submitting this outcome to a referendum?", Hover=c("Yes, it's a good idea", "Anyone's opinion should be taken into account, but with another method", "No, the current system is satisfactory", "PNR (I don't know, I don't want to answer)"))

#### Relation à la richesse ####
oui_non(vars=c("satisfaction"), file="bar_satisfaction2", labels=c("Votre revenu actuel vous satisfait-il ?"))

decrit(m$niveau_vie_desire)
data_nvd <- as.data.frame(matrix( c("2200€/mois","3500€/mois","7000€/mois","plus de 20 000€/mois","NSP","800€/mois","1400€/mois",36,41,7,1,3,1,11), ncol=2))
colors_nvd <- c('green', 'yellow', 'orange', 'red', 'lightgrey', 'purple', 'blue')
do_pie(Data=data_nvd, Colors=colors_nvd, File="pie_niveau_vie", Title="Quel niveau de vie aimeriez-vous avoir ?<br></b>Chiffres indicatifs : revenus nets pour une personne seule<b>", Hover = c("Gagner le revenu moyen de mon pays (autour 2200€/mois)","Avoir de quoi vivre confortablement (pouvoir s'offrir des voyages, des habits ou des objets sans problème : autour de 3500€ par mois)","Avoir de quoi pouvoir dépenser sans compter (au moins 7000€/mois)","Avoir de quoi vivre dans le luxe et être parmi les plus riches (gagner plus de 20000€/mois)","NSP (Ne sait pas, ne se prononce pas)", "Avoir juste un toit et de quoi manger (autour de 800€/mois)","Avoir de quoi vivre modestement (autour de 1400€/mois)"), Display_values=TRUE)

revenu_merite <- function(data = e, weights=T) {
  colors <-  c(rainbow(4, end=4/15), "forestgreen", 'blue', 'lightgrey') # c("red", "orange", "yellow", "green", "darkgreen") # rainbow(5, end=1/3)
  mat <- c( length(which(data[['revenu_merite']]<=1000))/length(which(!is.na(data[['revenu_merite']]))), length(which(data[['revenu_merite']]>1000 & data[['revenu_merite']]<=1500))/length(which(!is.na(data[['revenu_merite']]))), length(which(data[['revenu_merite']]>1500 & data[['revenu_merite']]<=2000))/length(which(!is.na(data[['revenu_merite']]))), length(which(data[['revenu_merite']]>2000 & data[['revenu_merite']]<=2500))/length(which(!is.na(data[['revenu_merite']]))),length(which(data[['revenu_merite']]>2500 & data[['revenu_merite']]<=3000))/length(which(!is.na(data[['revenu_merite']]))), length(which(data[['revenu_merite']]>3000))/length(which(!is.na(data[['revenu_merite']]))))
  prop_nsp <- length(which(is.na(data[['revenu_merite']])))/length(which(!is.na(data[['revenu_merite']])))
  if (weights) { mat <- c(sum(data[['weight']][which(data[['revenu_merite']]<=1000)])/sum(data[['weight']][which(!is.na(data[['revenu_merite']]))]), sum(data[['weight']][which(data[['revenu_merite']]>1000 & data[['revenu_merite']]<=1500)])/sum(data[['weight']][which(!is.na(data[['revenu_merite']]))]), sum(data[['weight']][which(data[['revenu_merite']]>1500 & data[['revenu_merite']]<=2000)])/sum(data[['weight']][which(!is.na(data[['revenu_merite']]))]), sum(data[['weight']][which(data[['revenu_merite']]>2000 & data[['revenu_merite']]<=2500)])/sum(data[['weight']][which(!is.na(data[['revenu_merite']]))]), sum(data[['weight']][which(data[['revenu_merite']]>2500 & data[['revenu_merite']]<=3000)])/sum(data[['weight']][which(!is.na(data[['revenu_merite']]))]),sum(data[['weight']][which(data[['revenu_merite']]>3000)])/sum(data[['weight']][which(!is.na(data[['revenu_merite']]))])) 
    prop_nsp <- sum(data[['weight']][which(is.na(data[['revenu_merite']]))])/sum(data[['weight']][which(!is.na(data[['revenu_merite']]))]) }
  legend <- c("Moins de 1000€/mois", "de 1001 à 1500€/m", "de 1501 à 2000€/m", "de 2001 à 2500€/m", "de 2501 à 3000€/m", "plus de 3000€/m", "NSP")
  widths <- c(0.12,0.115,0.11,0.102,0.09,0.06,0)
  before_par <- par()
  par(mar=c(2,0,0,2), oma=c(0,0,2.5,0))
  frame()
  abline(v=seq(0,1,by=0.1), lty=3, col="grey")
  axis(1, at = seq(0,1,by=0.1))
  barplot(matrix(c(mat, rep(0,7), prop_nsp), ncol=2), width=0.32, horiz=TRUE, add=TRUE, col=colors, border=NA, ylim=c(0,1), legend.text=legend, las=1, args.legend=list(horiz=TRUE, bty='o', box.lwd=0, xjust=1, text.width=widths, x.intersp=0.3, x="topright")) # ncol=3, inset=-0.3
  title("Revenu mérité\nCombien devriez-vous gagner, compte tenu de vos compétences, de vos efforts et de votre quantité de travail ?", outer=TRUE)
  par(before_par)
}
revenu_merite()
revenu_merite5 <- function(data = e, weights=T) {
  colors <-  c(rainbow(4, end=4/15), "forestgreen", 'lightgrey') # c("red", "orange", "yellow", "green", "darkgreen") # rainbow(5, end=1/3)
  mat <- c( length(which(data[['revenu_merite']]<=1000))/length(which(!is.na(data[['revenu_merite']]))), length(which(data[['revenu_merite']]>1000 & data[['revenu_merite']]<=1500))/length(which(!is.na(data[['revenu_merite']]))), length(which(data[['revenu_merite']]>1500 & data[['revenu_merite']]<=2000))/length(which(!is.na(data[['revenu_merite']]))), length(which(data[['revenu_merite']]>2000 & data[['revenu_merite']]<=3000))/length(which(!is.na(data[['revenu_merite']]))), length(which(data[['revenu_merite']]>3000))/length(which(!is.na(data[['revenu_merite']]))))
  prop_nsp <- length(which(is.na(data[['revenu_merite']])))/length(which(!is.na(data[['revenu_merite']])))
  if (weights) { mat <- c(sum(data[['weight']][which(data[['revenu_merite']]<=1000)])/sum(data[['weight']][which(!is.na(data[['revenu_merite']]))]), sum(data[['weight']][which(data[['revenu_merite']]>1000 & data[['revenu_merite']]<=1500)])/sum(data[['weight']][which(!is.na(data[['revenu_merite']]))]), sum(data[['weight']][which(data[['revenu_merite']]>1500 & data[['revenu_merite']]<=2000)])/sum(data[['weight']][which(!is.na(data[['revenu_merite']]))]), sum(data[['weight']][which(data[['revenu_merite']]>2000  & data[['revenu_merite']]<=3000)])/sum(data[['weight']][which(!is.na(data[['revenu_merite']]))]),sum(data[['weight']][which(data[['revenu_merite']]>3000)])/sum(data[['weight']][which(!is.na(data[['revenu_merite']]))])) 
    prop_nsp <- sum(data[['weight']][which(is.na(data[['revenu_merite']]))])/sum(data[['weight']][which(!is.na(data[['revenu_merite']]))]) }
  legend <- c("Moins de 1000€/mois", "de 1001 à 1500€/m", "de 1501 à 2000€/m", "de 2001 à 3000€/m", "plus de 3000€/m", "NSP")
  widths <- c(0.14,0.135,0.13,0.11,0.08,0.02)
  before_par <- par()
  par(mar=c(2,0,0,2), oma=c(0,0,2.5,0))
  frame()
  abline(v=seq(0,1,by=0.1), lty=3, col="grey")
  axis(1, at = seq(0,1,by=0.1))
  barplot(matrix(c(mat, rep(0,6), prop_nsp), ncol=2), width=0.32, horiz=TRUE, add=TRUE, col=colors, border=NA, ylim=c(0,1), legend.text=legend, las=1, args.legend=list(horiz=TRUE, bty='o', box.lwd=0, xjust=1, text.width=widths, x.intersp=0.3, x="topright")) # ncol=3, inset=-0.3
  title("Revenu mérité\nCombien devriez-vous gagner, compte tenu de vos compétences, de vos efforts et de votre quantité de travail ?", outer=TRUE)
  par(before_par)
}
revenu_merite5()

summary(lm(revenu_merite ~ revenu + rev_tot + niveau_vie + taille_foyer + age + sexe + taille_agglo + gauche_droite, data=m))

setwd("/home/adrien/Google Drive/Economie/Travail/enquete/codes")
br <- read.csv2("behavioral response.csv")
load("merged.Rda") # create dataframe "db"

repartition_revenu_merite <- wtd.Ecdf(m$revenu_merite, weights=m$weight)
repartition_revenu_merite$ecdf <- as.vector(repartition_revenu_merite$ecdf)
repartition_revenu <- wtd.Ecdf(m$revenu, weights=m$weight)
repartition_revenu$ecdf <- as.vector(repartition_revenu$ecdf)
repartition_revdisp <- wtd.Ecdf(m$revdisp, weights=m$weight)
repartition_revdisp$ecdf <- as.vector(repartition_revdisp$ecdf)
plot(repartition_revdisp$x, repartition_revdisp$ecdf, mgp=c(3,0.6,0), yaxt='n', lwd=2, col='red', xlim=c(0,6000), type='s', main="Revenu et revenu mérité\nQuel est votre revenu net ? Combien devriez-vous gagner, \ncompte tenu de vos compétences, de vos efforts et de votre quantité de travail ?", xlab="", ylab="Répartition des réponses exprimées (en %)") + grid()
lines(repartition_revenu_merite$x, repartition_revenu_merite$ecdf, mgp=c(3,0.6,0), yaxt='n', lwd=2, col='blue', xlim=c(0,6000), type="s", verticals=T, main="", xlab="", ylab="Répartition des réponses exprimées (en %)") + grid()
lines(ecdf(br$revenus[1:1000]/12), xlim=c(0,6000), yaxt='n', lwd=1, col='red', pch='', verticals=T)
# lines(ecdf(m$revenu_merite), mgp=c(3,0.6,0), yaxt='n', lwd=2, col='blue', xlim=c(0,6000), pch='', verticals=T, main="Revenu mérité\nCombien devriez-vous gagner, compte tenu de vos compétences, \nde vos efforts et de votre quantité de travail ? (en €/mois)", xlab="", ylab="Répartition des réponses exprimées (en %)") + grid()
# lines(ecdf(m$revenu), yaxt='n', lwd=2, col='red', xlim=c(0,6000), pch='', verticals=T)
mtext(do.call(expression, list(bquote(bold("Revenu mérité subjectif (bleu), revenu actuel (rouge), en #/mois")), bquote(paste("Lecture : ", italic("40% des répondants gagnent 1500€/mois ou moins, 40% des Français gagnent 1250€/mois ou moins ;"))), bquote(italic("60% des Français estiment qu'ils mériteraient de gagner 2000€/mois ou moins.")))),side=1,line=2:4)
abline(h=seq(0,1,by=0.1), lty=3, col="lightgrey")
abline(h=seq(0,1,by=0.2), lty=3, col="grey")
abline(v=seq(0,6000,by=250), lty=3, col="lightgrey")
abline(v=seq(0,6000,by=1000), lty=3, col="grey")
legend("bottomright", title.col="black", legend=c("revenu (distribution d'après l'enquête)","revenu mérité", "revenu (distribution d'après l'Insee)"), # title="", 
  lwd=c(2,2,1),col=c("red", "blue", "red"))
axis(2, at = seq(0,1,by=0.2), labels=c(0,20,40,60,80,100))

m$revdisp_tot <- m$rev_tot - irpp_p(12*m$rev_tot, grepl("Mari", m$situation_maritale), m$taille_foyer)/12

# Distributions brutes des revenus ne coïncident pas avec la vraie distribution (cf. plus bas pour des courbes corrigées qui coïncident presque)
plot(density(br$revenus[1:1000]/12, bw=200), xlim=c(0,6000), yaxt='n', lwd=2, lty=2, col='purple')
# trop d'extrêmes : 4% de "0" et 5% de > 8000€/m, pour cause d'erreur (revenu annuel au lieu de mensuel) et volonté de ne pas révéler ses revenus. Les jeunes ont aussi tendance à ne pas reporter les dons de leurs parents
lines(density(m$revenu[m$revenu<=6000], bw=200), mgp=c(3,0.6,0), yaxt='n', lwd=2, col='red', xlim=c(0,6000), type='l', main="") + grid()
lines(density(m$revenu_conjoint[m$revenu_conjoint<=6000 & !is.na(m$revenu_conjoint)], bw=200), mgp=c(3,0.6,0), yaxt='n', lwd=3, col='blue', xlim=c(0,6000), type='l', main="") + grid()
lines(density(m$revdisp_tot[m$revdisp_tot<=6000 & !is.na(m$revdisp_tot)]/(1+grepl("Mari",m$situation_maritale[m$revdisp_tot<=6000 & !is.na(m$revdisp_tot)])), bw=200), mgp=c(3,0.6,0), yaxt='n', lwd=3, col='green', xlim=c(0,6000), type='l', main="") + grid()
# Hypothèses de pourquoi les courbes ne coïncident pas :
# 0. Échantillon biaisé en faveur des pauvres
# 1. (bleu != violet) le répondant s'impute les prestations sociales au lieu de les imputer à la personne du ménage aux revenus les plus bas : 
#   d'où le trop plein de conjoints à 0 et le manque de conjoints à bas revenus
# 2. (rouge != bleu, violet) certains répondants se trompent et donnent le revenu du ménage au lieu du leur. Ils se rectifient quand ils voient qu'on demande le revenu du conjoint en mettant 0
# 3. (rouge != bleu) il y a un biais en faveur des répondants à haut revenu dans le ménage, peut-être parce que les gens répondent en famille et que c'est le chef de famille qui remplit ? Étonnant quand même
# 4. la façon de calculer le vert (revenu disponible indiv) pour réconcilier le bleu et le rouge tend à gommer les différences au sein du couple, donc à créer une densité piquée

# Raisons incertaines ou qui devraient jouer peu :
# il y a des personnes à charges à revenus non nuls, donc demander seulement le 'revenu du conjoint' est insuffisant pour avoir les revenus du ménage
# les riches ont des enfants plus vieux (avancement salarial progresse avec âge), donc leur nombre d'unité de consommation est sous-estimé (puisqu'on a pris 0.35 par personne en plus faute de connaître les âges) TODO: tester
# impôts mal codés : trop élevé pour les pauvres (a priori non)
# les gens oublient la prime d'activité, les APL ou autre
# TODO: qualité m$revenu+m$revenu_conjoint==0 (cela dit souvent jeunes sans revenus), revenu > 8000, taille_foyer=0. Bizarrement, ils n'ont pas une qualité particulièrement basse
decrit(m$qualite)
decrit(m$age[m$rev_tot==0])
decrit(m$qualite[m$revenu>=6000 | m$revenu_conjoint>=6000])
mails <- ""
for (i in m$mail[which((m$revenu>5500 | m$revenu_conjoint>5500) & !is.na(m$mail) & m$mail!="")]) {  mails <- paste(mails, i, sep=";")  }
print(mails)
mails <- ""
for (i in m$mail[which((m$rev_tot==0) & !is.na(m$mail) & m$mail!="")]) {  mails <- paste(mails, i, sep=";")  }
print(mails)
length(which(m$rev_tot==0))/nrow(m)
length(which(m$revenu>8000 | m$revenu_conjoint>8000))/nrow(m)
length(which(m$niveau_vie>8000))/nrow(m)
# repartition_niveau_vie <- wtd.Ecdf(m$niveau_vie, weights=m$weight)
# repartition_niveau_vie$ecdf <- as.vector(repartition_niveau_vie$ecdf)

# En regardant les niveaux de vie, on compare des variables vraiment comparables (même si notre imputation du nombre d'unités de consommation est discutable)
# Mais beaucoup d'extrêmes, ce qui fausse la courbe rouge :
plot(ecdf(m$niveau_vie[!is.na(m$niveau_vie)]), xlim=c(0,6000), yaxt='n', lwd=2, lty=2, pch='', col='red', main="Répartition des niveaux de vie")
lines(ecdf(db$nivviem[db$age>17]/12), xlim=c(0,6000), yaxt='n', lwd=2, lty=2, col='purple')

# Les courbes coïncident presque une fois qu'on enlève les extrêmes et les jeunes (dont le niveau de vie est sous-estimé, contrairement à l'Insee)
# Il semble malgré tout y avoir plus de bas revenus que dans la réalité
repartition_nivvie <- wtd.Ecdf(m$niveau_vie[m$revenu < 10000 & m$age!="18 - 24" & (m$revenu_conjoint<10000 | is.na(m$revenu_conjoint)) & m$rev_tot > 0], weights=m$weight[m$revenu < 10000 & m$age!="18 - 24" & (m$revenu_conjoint<10000 | is.na(m$revenu_conjoint)) & m$rev_tot > 0])
repartition_nivvie$ecdf <- as.vector(repartition_nivvie$ecdf)
plot(ecdf(db$nivviem[db$age>24]/12), xlim=c(0,6000), yaxt='n', lwd=2, lty=2, col='purple', main="Répartition des niveaux de vie des plus de 25 ans", xlab="Niveau de vie mensuel (€)")
lines(repartition_nivvie$x, repartition_nivvie$ecdf, mgp=c(3,0.6,0), yaxt='n', lwd=3, col='green', xlim=c(0,6000), type='l', main="") + grid()
legend("bottomright", legend=c("Insee (ERFS 2012)", "enquête redressée et purgée"), lwd=2,col=c("purple", "green"))

### Voilà l'écart entre l'échantillon et la réalité qui reste véritablement inexpliqué :
plot(density(db$nivviem[db$age>24]/12, na.rm=T, bw=100), xlim=c(0,6000), yaxt='n', lwd=2, lty=2, col='purple', xlab="Niveau de vie mensuel", main="Densité de niveaux de vie des plus de 25 ans")
lines(density(m$niveau_vie[m$revenu < 10000 & m$age!="18 - 24" & (m$revenu_conjoint<10000 | is.na(m$revenu_conjoint)) & m$rev_tot > 0], bw=100), yaxt='n', lwd=2, lty=1, col='green')
legend("topright", legend=c("Insee (ERFS 2012)", "enquête redressée et purgée"), lwd=2,col=c("purple", "green"))

# n1 <- sum(db$wprm[which(db$nivviem<=6000*12 & db$age>17 & !is.na(db$nivviem))])
# n2 <- sum(m$weight[m$niveau_vie<=6000 & !is.na(m$niveau_vie) & m$rev_tot > 0])
# n3 <- sum(p$weight[p$niveau_vie<=6000 & !is.na(p$niveau_vie) & p$rev_tot > 0])
# n4 <- sum(e$weight[e$niveau_vie<=6000 & !is.na(e$niveau_vie) & e$rev_tot > 0])
# plot(density(db$nivviem[which(db$nivviem<=6000*12 & db$age>17 & !is.na(db$nivviem))]/12, weights=db$wprm[which(db$nivviem<=6000*12 & db$age>17 & !is.na(db$nivviem))]/n1, na.rm=T), xlim=c(0,6000), yaxt='n', lwd=2, lty=2, col='purple')
# lines(density(m$niveau_vie[m$niveau_vie<=6000 & !is.na(m$niveau_vie) & m$rev_tot > 0], weights=m$weight[m$niveau_vie<=6000 & !is.na(m$niveau_vie) & m$rev_tot > 0]/n2), mgp=c(3,0.6,0), yaxt='n', lwd=3, col='red', xlim=c(0,6000), type='l', main="") + grid()
# lines(density(p$niveau_vie[p$niveau_vie<=6000 & !is.na(p$niveau_vie) & p$rev_tot > 0], weights=p$weight[p$niveau_vie<=6000 & !is.na(p$niveau_vie) & p$rev_tot > 0]/n3), mgp=c(3,0.6,0), yaxt='n', lwd=3, col='green', xlim=c(0,6000), type='l', main="") + grid()
# lines(density(e$niveau_vie[e$niveau_vie<=6000 & !is.na(e$niveau_vie) & e$rev_tot > 0], weights=e$weight[e$niveau_vie<=6000 & !is.na(e$niveau_vie) & e$rev_tot > 0]/n4), mgp=c(3,0.6,0), yaxt='n', lwd=3, col='blue', xlim=c(0,6000), type='l', main="") + grid()
# 
# sum(m$weight[which(m$revenu_merite<=2000)])/sum(m$weight[which(!is.na(m$revenu_merite))])
# sum(m$weight[which(m$revenu<=1500)])/sum(m$weight[which(!is.na(m$revenu))])

# Attention aux deux groupes de courbes ci-dessous : les revenus totaux du ménage sont sous-estimés car l'enquête ne permet pas de prendre en compte les revenus d'un couple non marié, moralité : il faudra mieux poser les questions la prochaine fois
n5 <- sum(db$wprm[which(db$revdecm<=15000*12 & db$age>17 & !is.na(db$revdecm))])
n6 <- sum(m$weight[m$rev_tot<=15000 & !is.na(m$rev_tot) & m$rev_tot > 0])
plot(density(m$rev_tot[m$rev_tot<=15000 & !is.na(m$rev_tot) & m$rev_tot > 0], bw=200, weights=m$weight[m$rev_tot<=15000 & !is.na(m$rev_tot) & m$rev_tot > 0]/n6), mgp=c(3,0.6,0), yaxt='n', lwd=3, col='red', xlim=c(0,6000), type='l', main="") + grid()
lines(density(db$revdecm[which(db$revdecm<=15000*12 & db$age>17 & !is.na(db$revdecm))]/12, bw=200, weights=db$wprm[which(db$revdecm<=15000*12 & db$age>17 & !is.na(db$revdecm))]/n5, na.rm=T), xlim=c(0,6000), yaxt='n', lwd=2, lty=2, col='purple')

n7 <- sum(db$wprm[which(db$revdispm<=15000*12 & db$age>17 & !is.na(db$revdispm))])
n8 <- sum(m$weight[m$revdisp_tot<=15000 & !is.na(m$revdisp_tot) & m$revdisp_tot > 0])
plot(density(m$revdisp_tot[m$revdisp_tot<=15000 & !is.na(m$revdisp_tot) & m$revdisp_tot > 0], bw=200, weights=m$weight[m$revdisp_tot<=15000 & !is.na(m$revdisp_tot) & m$revdisp_tot > 0]/n8), mgp=c(3,0.6,0), yaxt='n', lwd=3, col='red', xlim=c(0,6000), type='l', main="") + grid()
lines(density(db$revdispm[which(db$revdispm<=15000*12 & db$age>17 & !is.na(db$revdispm))]/12, bw=200, weights=db$wprm[which(db$revdispm<=15000*12 & db$age>17 & !is.na(db$revdispm))]/n7, na.rm=T), xlim=c(0,6000), yaxt='n', lwd=2, lty=2, col='purple')
lines(density(m$revdisp_tot[m$revdisp_tot<=15000 & !is.na(m$revdisp_tot) & m$revdisp_tot > 0], bw=200), mgp=c(3,0.6,0), yaxt='n', lwd=3, col='red', xlim=c(0,6000), type='l', main="") + grid()

# En plus, on sous-estime le nombre d'unités de consommation (à cause des jeunes ?) :
db$uc <- db$revdispm / db$nivviem
decrit(db$uc)
decrit(uc(grepl("Mari",m$situation_maritale), m$taille_foyer))

rm(br, db)

#### Caractéristiques socio-démographiques ####
revenu <- function(data = m, weights=T) {
  colors <-  c(rainbow(4, end=4/15), "forestgreen", 'blue', 'lightgrey') # c("red", "orange", "yellow", "green", "darkgreen") # rainbow(5, end=1/3)
  mat <- c( length(which(data[['revenu']]<=1000))/length(which(!is.na(data[['revenu']]))), length(which(data[['revenu']]>1000 & data[['revenu']]<=1500))/length(which(!is.na(data[['revenu']]))), length(which(data[['revenu']]>1500 & data[['revenu']]<=2000))/length(which(!is.na(data[['revenu']]))), length(which(data[['revenu']]>2000 & data[['revenu']]<=2500))/length(which(!is.na(data[['revenu']]))),length(which(data[['revenu']]>2500 & data[['revenu']]<=3000))/length(which(!is.na(data[['revenu']]))), length(which(data[['revenu']]>3000))/length(which(!is.na(data[['revenu']]))))
  if (weights) { mat <- c(sum(data[['weight']][which(data[['revenu']]<=1000)])/sum(data[['weight']][which(!is.na(data[['revenu']]))]), sum(data[['weight']][which(data[['revenu']]>1000 & data[['revenu']]<=1500)])/sum(data[['weight']][which(!is.na(data[['revenu']]))]), sum(data[['weight']][which(data[['revenu']]>1500 & data[['revenu']]<=2000)])/sum(data[['weight']][which(!is.na(data[['revenu']]))]), sum(data[['weight']][which(data[['revenu']]>2000 & data[['revenu']]<=2500)])/sum(data[['weight']][which(!is.na(data[['revenu']]))]), sum(data[['weight']][which(data[['revenu']]>2500 & data[['revenu']]<=3000)])/sum(data[['weight']][which(!is.na(data[['revenu']]))]),sum(data[['weight']][which(data[['revenu']]>3000)])/sum(data[['weight']][which(!is.na(data[['revenu']]))]))  }
  legend <- c("Moins de 1000€/mois", "de 1001 à 1500€/m", "de 1501 à 2000€/m", "de 2001 à 2500€/m", "de 2501 à 3000€/m", "plus de 3000€/m")
  widths <- c(0.145,0.14,0.135,0.125,0.115,0.09)
  before_par <- par()
  par(mar=c(2,0,0,2), oma=c(0,0,2.5,0))
  frame()
  abline(v=seq(0,1,by=0.1), lty=3, col="grey")
  axis(1, at = seq(0,1,by=0.1))
  barplot(matrix(mat, ncol=1), width=0.6, horiz=TRUE, add=TRUE, col=colors, border=NA, ylim=c(0,1), legend.text=legend, las=1, args.legend=list(horiz=TRUE, bty='o', box.lwd=0, xjust=1, text.width=widths, x.intersp=0.3, x="topright")) # ncol=3, inset=-0.3
  title("Revenu mensuel net", outer=TRUE)
  par(before_par)
}
revenu()
revenu5 <- function(data = m, weights=T) {
  colors <-  c(rainbow(4, end=4/15), "forestgreen", 'lightgrey') # c("red", "orange", "yellow", "green", "darkgreen") # rainbow(5, end=1/3)
  mat <- c( length(which(data[['revenu']]<=1000))/length(which(!is.na(data[['revenu']]))), length(which(data[['revenu']]>1000 & data[['revenu']]<=1500))/length(which(!is.na(data[['revenu']]))), length(which(data[['revenu']]>1500 & data[['revenu']]<=2000))/length(which(!is.na(data[['revenu']]))), length(which(data[['revenu']]>2000 & data[['revenu']]<=3000))/length(which(!is.na(data[['revenu']]))), length(which(data[['revenu']]>3000))/length(which(!is.na(data[['revenu']]))))
  if (weights) { mat <- c(sum(data[['weight']][which(data[['revenu']]<=1000)])/sum(data[['weight']][which(!is.na(data[['revenu']]))]), sum(data[['weight']][which(data[['revenu']]>1000 & data[['revenu']]<=1500)])/sum(data[['weight']][which(!is.na(data[['revenu']]))]), sum(data[['weight']][which(data[['revenu']]>1500 & data[['revenu']]<=2000)])/sum(data[['weight']][which(!is.na(data[['revenu']]))]), sum(data[['weight']][which(data[['revenu']]>2000 & data[['revenu']]<=3000)])/sum(data[['weight']][which(!is.na(data[['revenu']]))]),sum(data[['weight']][which(data[['revenu']]>3000)])/sum(data[['weight']][which(!is.na(data[['revenu']]))]))  }
  legend <- c("Moins de 1000€/mois", "de 1001 à 1500€/m", "de 1501 à 2000€/m", "de 2001 à 3000€/m", "plus de 3000€/m")
  widths <- c(0.165,0.16,0.155,0.145,0.135)
  before_par <- par()
  par(mar=c(2,0,0,2), oma=c(0,0,2.5,0))
  frame()
  abline(v=seq(0,1,by=0.1), lty=3, col="grey")
  axis(1, at = seq(0,1,by=0.1))
  barplot(matrix(mat, ncol=1), width=0.6, horiz=TRUE, add=TRUE, col=colors, border=NA, ylim=c(0,1), legend.text=legend, las=1, args.legend=list(horiz=TRUE, bty='o', box.lwd=0, xjust=1, text.width=widths, x.intersp=0.3, x="topright")) # ncol=3, inset=-0.3
  title("Revenu mensuel net", outer=TRUE)
  par(before_par)
}
revenu5()
decrit(m$statut, weights=m$weight)
data_statut <- as.data.frame(matrix( c("retraité·e<br>31%","CDI<br>31%","fonctionnaire<br>9%","CDD<br>5%","autre actif<br>7%","au chômage<br>7%","étudiant·e<br>4%", "autre inactif<br>5%", "NSP<br>0%",31,31,9,5,7,7,4,5,0), ncol=2))
colors_statut <- c('purple', 'darkgreen', 'green', 'yellow', 'orange', 'red', 'blue', 'brown', 'lightgrey')
do_pie(Data=data_statut, Colors=colors_statut, File="pie_statut", Title="Quel est votre statut d'emploi ?", Hover = c("retraité·e<br>31%","CDI<br>31%","fonctionnaire<br>9%","CDD<br>5%","autre actif (profession libérale, intermittent, intérimaire, stage...)<br>7%","au chômage<br>7%","étudiant·e<br>4%", "autre inactif<br>5%", "NSP<br>0%"), Display_values=FALSE)

decrit(e$patrimoine, weights=e$weight)
decrit(p$patrimoine, weights=p$weight)
# Échantillon légèrement biaisé en faveur des riches, ou bien les pauvres ont-ils plus de mal à évaluer leur patrimoine, et donc répondent plus NSP ?
# Moins de non-réponses et plus proche de la réalité quand on met des catégories
decrit(p$patrimoine[!grepl("NSP",p$patrimoine)], weights=p$weight[!grepl("NSP",p$patrimoine)]) # 32 18 15 19 10 6
# Chiffres Révolution Fiscale : http://www.revolution-fiscale.fr/simuler/distrib/patrimoine.php?pct=30&slider=30&pat=10+000&conceptpat=k_cn&affich=visa
# 32% 9% 29% 17% 7% 6% (réalité)
# Chiffres de p$ diffèrent de la réalité sur 10k-150k€ (trop sur 10k-50k) et 5% de 150k-600k manquants
length(which(e$patrimoine<=10000))/length(which(!is.na(e$patrimoine))) # 22%
length(which(e$patrimoine<50000 & e$patrimoine > 10000))/length(which(!is.na(e$patrimoine))) # 15% surestimé et suivant sous-estimé (20% sans le <)
length(which(e$patrimoine<=150000 & e$patrimoine >= 50000))/length(which(!is.na(e$patrimoine))) # 23% (18% sans le >=)
length(which(e$patrimoine<=300000 & e$patrimoine > 150000))/length(which(!is.na(e$patrimoine))) # 19%
length(which(e$patrimoine<=600000 & e$patrimoine > 300000))/length(which(!is.na(e$patrimoine))) # 13% deux fois plus que réalité
length(which(e$patrimoine > 600000))/length(which(!is.na(e$patrimoine))) # 7%
length(which(is.na(e$patrimoine)))/length(e$patrimoine) # 35% NSP (x2)
length(which(is.element(e$patrimoine, c(600000,300000,150000,50000,10000))))/length(which(!is.na(e$patrimoine))) # 18% aux seuils
summary(lm(is.na(Patrimoine) ~ rev_tot + I(rev_tot^2) + Age + I(Age^2), data=m)) # le taux de réponse croît puis décroît avec l'âge et les revenus

# e$heritage_futur : 60 2 5 19 9 3 1 31
length(which(e$heritage_futur==0))/length(which(!is.na(e$heritage_futur))) # 60%
length(which(e$heritage_futur<=10000 & e$heritage_futur>0))/length(which(!is.na(e$heritage_futur))) # 2%
length(which(e$heritage_futur<=50000 & e$heritage_futur > 10000))/length(which(!is.na(e$heritage_futur))) # 5%
length(which(e$heritage_futur<=150000 & e$heritage_futur > 50000))/length(which(!is.na(e$heritage_futur))) # 19%
length(which(e$heritage_futur<=300000 & e$heritage_futur > 150000))/length(which(!is.na(e$heritage_futur))) # 9%
length(which(e$heritage_futur<=600000 & e$heritage_futur > 300000))/length(which(!is.na(e$heritage_futur))) # 3%
length(which(e$heritage_futur > 600000))/length(which(!is.na(e$heritage_futur))) # 1%
length(which(is.na(e$heritage_futur)))/length(e$heritage_futur) # 31% NSP (x2)

decrit(m$diplome, weights=m$weight)
data_diplome_poids <- as.data.frame(matrix( c("Baccalauréat<br>19%","Bac +2<br>16%","Bac +3<br>8%","Bac +5<br>14%","NSP<br>1%","Aucun diplôme<br>9%", "Brevet des collèges<br>7%","CAP ou BEP<br>27%",19,16,8,14,1,9,7,27), ncol=2))
data_diplome <- as.data.frame(matrix( c("Baccalauréat<br>19%","Bac +2<br>23%","Bac +3<br>13%","Bac +5<br>22%","NSP<br>1%", "Aucun diplôme<br>2%", "Brevet des collèges<br>4%","CAP ou BEP<br>18%", 19,23,13,22,1,2,4,18), ncol=2))
data_diplome_reel <- as.data.frame(matrix( c("Baccalauréat<br>17%","Bac +2<br>12%","Bac +3<br>10%","Bac +5<br>6%","Aucun diplôme<br>24%", "Brevet des collèges<br>6%","CAP ou BEP<br>24%", 17,12,10,6,24,6,24), ncol=2))
colors_diplome <- c("yellow", "green", "darkgreen", "blue", "lightgrey", "darkred", "red", "orange")
do_pie(Data=data_diplome, File="pie_diplome", Colors=colors_diplome, Title="Quel est votre plus haut diplôme ?<br></b>Résultats non redressés<b>", Hover=c("Baccalauréat<br>19%","Bac +2 (BTS, DUT, DEUG, écoles de formation sanitaires et sociales...)<br>23%","Bac +3 (licence...)<br>13%","Bac +5 ou plus (master, école d'ingénieur ou de commerce, doctorat, médecine, maîtrise, DEA, DESS...)<br>22%","NSP (Ne se prononce pas)<br>1%", "Aucun diplôme<br>2%", "Brevet des collèges<br>4%","CAP ou BEP<br>18%"), Display_values = FALSE)
do_pie(Data=data_diplome_poids, File="pie_diplome_poids", Colors=colors_diplome, Title="Quel est votre plus haut diplôme ?<br></b>Résultats redressés pour se rapprocher de la répartition réelle<b>", Hover=c("Baccalauréat<br>19%","Bac +2 (BTS, DUT, DEUG, écoles de formation sanitaires et sociales...)<br>16%","Bac +3 (licence...)<br>8%","Bac +5 ou plus (master, école d'ingénieur ou de commerce, doctorat, médecine, maîtrise, DEA, DESS...)<br>14%","NSP (Ne se prononce pas)<br>1%", "Aucun diplôme<br>2%", "Brevet des collèges<br>4%","CAP ou BEP<br>18%"), Display_values = FALSE) # (ou celui que vous comptez avoir si vous êtes étudiant·e) 
do_pie(Data=data_diplome_reel, File="pie_diplome_reel", Colors=c("yellow", "green", "darkgreen", "blue", "darkred", "red", "orange"), Title="Niveaux de diplôme</b><br>Population française adulte, 2015 (Insee)<b>", Hover=c("Baccalauréat<br>17%","Bac +2 (BTS, DUT, DEUG, écoles de formation sanitaires et sociales...)<br>12%","Bac +3 (licence...)<br>10%","Bac +5 ou plus (master, école d'ingénieur ou de commerce, doctorat, médecine, maîtrise, DEA, DESS...)<br>6%","Aucun diplôme<br>24%", "Brevet des collèges<br>6%","CAP ou BEP<br>24%"), Display_values = FALSE) # (ou celui que vous comptez avoir si vous êtes étudiant·e) 

decrit(m$situation_maritale)
decrit(m$temps_travail, weights=m$weight)
situation_maritale
sexe
age
taille_foyer
# TODO: variables non mises en graphe : m$suppression_aides_condamnes temps_travail situation_maritale taille_foyer

decrit(m$quel_heritage, weights=m$weight)
data_heritage <- as.data.frame(matrix( c("Aucune taxation", "Taxe sur les plus riches mais <br>pas de taxe sur l'équivalent d'une maison","Taxe faible pour la plupart des gens,<br> élevée pour les riches", "Taxe faible pour les plus modestes qui augmente <br>ensuite progressivement jusqu'à être très élevée :<br>personne ne devrait hériter plus que 10 millions d'euros","NSP",40,14,19,18,9), ncol=2))
colors_heritage <- c('blue', 'green', 'yellow', 'red', 'lightgrey')
do_pie(Data=data_heritage, Colors=colors_heritage, File="pie_heritage", Title="Modalités préférées concernant la taxation des successions", Hover = c("Chaque personne, aussi riche soit-elle, devrait pouvoir transmettre tout<br> son patrimoine à ses enfants sans payer d'impôt", "Chaque personne devrait pouvoir transmettre l'équivalent <br> d'une maisonà chacun de ses enfants sans être taxée, mais les <br>plus riches devraient payer un impôt sur les successions", "L'impôt sur les successions devrait être faible pour la plupart<br>  des gens mais il devrait être plus élevé pour les riches", "L'impôt sur les successions devrait être faible pour les plus modestes<br>  mais il devrait augmenter ensuite, jusqu'à être très élevé pour les plus riches :<br>  personne ne devrait hériter de plus de 10 millions d'euros","NSP (Ne sait pas, ne se prononce pas)"), Display_values=TRUE)

decrit(m$Patrimoine_futur, weights=m$weight)
decrit(m$Patrimoine_futur, miss=T, weights=m$weight)
decrit(m$patrimoine, miss=T, weights=m$weight) # 19% de NSP
decrit(m$Heritage_futur, miss=T, weights=m$weight) # 28% de NSP
barres(file="patrimoine", title="<b>Patrimoine actuel et futur</b>", data=matrix(c(0.03, 0.15, 0.11, 0.04, 0.17, 0.19, 0.06, 0.14, 0.11, 0.39, 0.4875, 0.1071, 0.0981, 0, 0.1550, 0.0979, 0, 0.0412, 0.0132, 0.28, 0.05, 0.24, 0.19, 0, 0.16, 0.18, 0, 0.11, 0.06, 0.19), ncol=3), sort=FALSE, nsp=T, color=c(rainbow(9, end=0.75), "lightgrey"), legend = c("Aucun","Moins de 10 000€","De 10 000€ à 50 000€","De 20 000€ à 100 000€","De 50 000€ à 200 000€","De 100 000€ à 350 000€","De 200 000€ à 450 000€","De 300 000€ à 750 000€","Plus de 600 000€", "NSP"), labels=rev(c("Patrimoine", "Héritage futur estimé", "Patrimoine futur (déduit)")))


#### Réaction comportementale ####
reaction_comportementale <- function(data = m, weights=T) {
  colors <-  c("orange", "yellow", "green", 'lightgrey') # c("red", "orange", "yellow", "green", "darkgreen") # rainbow(5, end=1/3)
  mat <- c()
  for (i in c(-0.25, -0.1, 0.1, 0.25)) {
    if (weights) { mat <- c(mat, sum(data[['weight']][which(data[['variante_reaction']]==i & data[['reaction_comportementale']]==1)])/sum(data[['weight']][which(data[['variante_reaction']]==i & data[['reaction_comportementale']] > -1 & !is.na(data[['reaction_comportementale']]))]), sum(data[['weight']][which(data[['variante_reaction']]==i & data[['reaction_choix']]=="changer")])/sum(data[['weight']][which(data[['variante_reaction']]==i & data[['reaction_comportementale']] > -1 & !is.na(data[['reaction_comportementale']]))]), sum(data[['weight']][which(data[['variante_reaction']]==i & data[['reaction_comportementale']]==1-i)])/sum(data[['weight']][which(data[['variante_reaction']]==i &data[['reaction_comportementale']] > -1 &  !is.na(data[['reaction_comportementale']]))]), sum(data[['weight']][which(data[['variante_reaction']]==i & data[['reaction_comportementale']]==-1)])/sum(data[['weight']][which(data[['variante_reaction']]==i & data[['reaction_comportementale']] > -1 & !is.na(data[['reaction_comportementale']]))])  ) }
    else { mat <- c(mat,  length(which(data[['reaction_comportementale']]==1 & data[['variante_reaction']]==i))/length(which(data[['variante_reaction']]==i & data[['reaction_comportementale']] > -1 & !is.na(data[['reaction_comportementale']]))), length(which(data[['variante_reaction']]==i & data[['reaction_choix']]=="changer"))/length(which(data[['variante_reaction']]==i & data[['reaction_comportementale']] > -1 & !is.na(data[['reaction_comportementale']]))), length(which(data[['variante_reaction']]==i & data[['reaction_comportementale']]==1-i))/length(which(data[['variante_reaction']]==i & data[['reaction_comportementale']] > -1 &  !is.na(data[['reaction_comportementale']]))), length(which(data[['variante_reaction']]==i & data[['reaction_comportementale']]==-1))/length(which(data[['variante_reaction']]==i & data[['reaction_comportementale']] > -1 & !is.na(data[['reaction_comportementale']])))  ) }
  }
  legend <- c("Travailler autant", "Changer son temps de travail", "Gagner autant", "NSP")
  widths <- c(0.19,0.18,0.08,0)
  print(mat)
  before_par <- par()
  par(mar=c(2,4,0,10), oma=c(0,0,2.5,0))
  frame()
  abline(v=seq(0,1,by=0.1), lty=3, col="grey")
  axis(1, at = seq(0,1,by=0.1))
  barplot(matrix(mat, ncol=4), width=0.77/4, names.arg=c("-25%", "-10%", "+10%", "+25%"), horiz=TRUE, add=TRUE, col=colors, border=NA, ylim=c(0,1), legend.text=legend, las=1, args.legend=list(horiz=TRUE, bty='o', box.lwd=0, xjust=1, text.width=widths, x.intersp=0.3, x="topright")) # ncol=3, inset=-0.3
  title("Comment souhaiteriez-vous adapter votre temps de travail si votre salaire horaire variait de ... ?", outer=TRUE)
  par(before_par)
}
reaction_comportementale()
decrit(m$reaction_comportementale[m$variante_reaction==0.25 & m$reaction_choix=="changer"])
decrit(m$reaction_comportementale[m$variante_reaction==0.1 & m$reaction_choix=="changer"])
decrit(m$reaction_comportementale[m$variante_reaction==-0.1 & m$reaction_choix=="changer"])
decrit(m$reaction_comportementale[m$variante_reaction==-0.25 & m$reaction_choix=="changer"])

m$elasticite <- round_any( (m$reaction_comportementale - 1) / m$variante_reaction, 0.001)
elasticite5 <- function(data = m, weights=T) {
  colors <-  c(rainbow(4, end=4/15), "forestgreen", 'lightgrey') # c("red", "orange", "yellow", "green", "darkgreen") # rainbow(5, end=1/3)
  mat <- c()
  for (i in c(-0.25, -0.1, 0.1, 0.25)) {
    if (weights) { mat <- c(mat, sum(data[['weight']][which(data[['variante_reaction']]==i & data[['elasticite']]<=-1)])/sum(data[['weight']][which(data[['variante_reaction']]==i & data[['reaction_comportementale']] > -1 & !is.na(data[['reaction_comportementale']]))]), sum(data[['weight']][which(data[['variante_reaction']]==i & data[['elasticite']]>-1 & data[['elasticite']]<0)])/sum(data[['weight']][which(data[['variante_reaction']]==i & data[['reaction_comportementale']] > -1 & !is.na(data[['reaction_comportementale']]))]), sum(data[['weight']][which(data[['variante_reaction']]==i & data[['elasticite']]==0)])/sum(data[['weight']][which(data[['variante_reaction']]==i & data[['reaction_comportementale']] > -1 &  !is.na(data[['reaction_comportementale']]))]), sum(data[['weight']][which(data[['variante_reaction']]==i & data[['elasticite']]>0 & data[['elasticite']]<1)])/sum(data[['weight']][which(data[['variante_reaction']]==i & data[['reaction_comportementale']] > -1 &  !is.na(data[['reaction_comportementale']]))]), sum(data[['weight']][which(data[['variante_reaction']]==i & data[['elasticite']]>=1)])/sum(data[['weight']][which(data[['variante_reaction']]==i & data[['reaction_comportementale']] > -1 &  !is.na(data[['reaction_comportementale']]))]), sum(data[['weight']][which(data[['variante_reaction']]==i & data[['reaction_comportementale']]==-1)])/sum(data[['weight']][which(data[['variante_reaction']]==i & data[['reaction_comportementale']] > -1 & !is.na(data[['reaction_comportementale']]))])  ) }
    else { mat <- c(mat,  length(which(data[['elasticite']]<=-1 & data[['variante_reaction']]==i))/length(which(data[['variante_reaction']]==i & data[['reaction_comportementale']] > -1 & !is.na(data[['reaction_comportementale']]))), length(which(data[['variante_reaction']]==i & data[['elasticite']]>-1 & data[['elasticite']]<0))/length(which(data[['variante_reaction']]==i & data[['reaction_comportementale']] > -1 & !is.na(data[['reaction_comportementale']]))), length(which(data[['variante_reaction']]==i & data[['elasticite']]==0))/length(which(data[['variante_reaction']]==i & data[['reaction_comportementale']] > -1 &  !is.na(data[['reaction_comportementale']]))), length(which(data[['elasticite']]<1 & data[['elasticite']]>0 & data[['variante_reaction']]==i))/length(which(data[['variante_reaction']]==i & data[['reaction_comportementale']] > -1 & !is.na(data[['reaction_comportementale']]))), length(which(data[['elasticite']]>=1 & data[['variante_reaction']]==i))/length(which(data[['variante_reaction']]==i & data[['reaction_comportementale']] > -1 & !is.na(data[['reaction_comportementale']]))), length(which(data[['variante_reaction']]==i & data[['elasticite']]==-1))/length(which(data[['variante_reaction']]==i & data[['reaction_comportementale']] > -1 & !is.na(data[['reaction_comportementale']])))  ) }
  }
  legend <- c("ε ≤ -1", "-1 < ε < 0", "ε = 0", "0 < ε < 1", "1 ≤ ε", "NSP")
  widths <- c(0.13,0.12,0.11,0.1,0.08,0)
  before_par <- par()
  par(mar=c(2,4,0,10), oma=c(0,0,2.5,0))
  frame()
  abline(v=seq(0,1,by=0.1), lty=3, col="grey")
  axis(1, at = seq(0,1,by=0.1))
  barplot(matrix(mat, ncol=4), width=0.77/4, names.arg=c("-25%", "-10%", "+10%", "+25%"), horiz=TRUE, add=TRUE, col=colors, border=NA, ylim=c(0,1), legend.text=legend, las=1, args.legend=list(horiz=TRUE, bty='o', box.lwd=0, xjust=1, text.width=widths, x.intersp=0.3, x="topright")) # ncol=3, inset=-0.3
  title("Élasticité implicite subjective de l'offre de travail pour une variation de salaire horaire de ... ", outer=TRUE)
  par(before_par)
}
elasticite5()
elasticite <- function(data = m, weights=T) {
  colors <-  c('darkred', rainbow(4, end=4/15), "forestgreen", 'lightgrey') # c("red", "orange", "yellow", "green", "darkgreen") # rainbow(5, end=1/3)
  mat <- c()
  for (i in c(-0.25, -0.1, 0.1, 0.25)) {
    if (weights) { mat <- c(mat, sum(data[['weight']][which(data[['variante_reaction']]==i & data[['elasticite']]< -1)])/sum(data[['weight']][which(data[['variante_reaction']]==i & data[['reaction_comportementale']] > -1 & !is.na(data[['reaction_comportementale']]))]), sum(data[['weight']][which(data[['variante_reaction']]==i & data[['elasticite']]==-1)])/sum(data[['weight']][which(data[['variante_reaction']]==i & data[['reaction_comportementale']] > -1 & !is.na(data[['reaction_comportementale']]))]), sum(data[['weight']][which(data[['variante_reaction']]==i & data[['elasticite']]>-1 & data[['elasticite']]<0)])/sum(data[['weight']][which(data[['variante_reaction']]==i & data[['reaction_comportementale']] > -1 & !is.na(data[['reaction_comportementale']]))]), sum(data[['weight']][which(data[['variante_reaction']]==i & data[['elasticite']]==0)])/sum(data[['weight']][which(data[['variante_reaction']]==i & data[['reaction_comportementale']] > -1 &  !is.na(data[['reaction_comportementale']]))]), sum(data[['weight']][which(data[['variante_reaction']]==i & data[['elasticite']]>0 & data[['elasticite']]<1)])/sum(data[['weight']][which(data[['variante_reaction']]==i & data[['reaction_comportementale']] > -1 &  !is.na(data[['reaction_comportementale']]))]), sum(data[['weight']][which(data[['variante_reaction']]==i & data[['elasticite']]>=1)])/sum(data[['weight']][which(data[['variante_reaction']]==i & data[['reaction_comportementale']] > -1 &  !is.na(data[['reaction_comportementale']]))]), sum(data[['weight']][which(data[['variante_reaction']]==i & data[['reaction_comportementale']]==-1)])/sum(data[['weight']][which(data[['variante_reaction']]==i & data[['reaction_comportementale']] > -1 & !is.na(data[['reaction_comportementale']]))])  ) }
    else { mat <- c(mat,  length(which(data[['elasticite']]< -1 & data[['variante_reaction']]==i))/length(which(data[['variante_reaction']]==i & data[['reaction_comportementale']] > -1 & !is.na(data[['reaction_comportementale']]))), length(which(data[['elasticite']]==-1 & data[['variante_reaction']]==i))/length(which(data[['variante_reaction']]==i & data[['reaction_comportementale']] > -1 & !is.na(data[['reaction_comportementale']]))), length(which(data[['variante_reaction']]==i & data[['elasticite']]>-1 & data[['elasticite']]<0))/length(which(data[['variante_reaction']]==i & data[['reaction_comportementale']] > -1 & !is.na(data[['reaction_comportementale']]))), length(which(data[['variante_reaction']]==i & data[['elasticite']]==0))/length(which(data[['variante_reaction']]==i & data[['reaction_comportementale']] > -1 &  !is.na(data[['reaction_comportementale']]))), length(which(data[['elasticite']]<1 & data[['elasticite']]>0 & data[['variante_reaction']]==i))/length(which(data[['variante_reaction']]==i & data[['reaction_comportementale']] > -1 & !is.na(data[['reaction_comportementale']]))), length(which(data[['elasticite']]>=1 & data[['variante_reaction']]==i))/length(which(data[['variante_reaction']]==i & data[['reaction_comportementale']] > -1 & !is.na(data[['reaction_comportementale']]))), length(which(data[['variante_reaction']]==i & data[['elasticite']]==-1))/length(which(data[['variante_reaction']]==i & data[['reaction_comportementale']] > -1 & !is.na(data[['reaction_comportementale']])))  ) }
  }
  legend <- c("ε < -1", "ε = -1", "-1 < ε < 0", "ε = 0", "0 < ε < 1", "1 ≤ ε", "NSP")
  widths <- c(0.1,0.1,0.1,0.09,0.08,0.06,0)
  before_par <- par()
  par(mar=c(2,4,0,10), oma=c(0,0,2.5,0))
  frame()
  abline(v=seq(0,1,by=0.1), lty=3, col="grey")
  axis(1, at = seq(0,1,by=0.1))
  barplot(matrix(mat, ncol=4), width=0.77/4, names.arg=c("-25%", "-10%", "+10%", "+25%"), horiz=TRUE, add=TRUE, col=colors, border=NA, ylim=c(0,1), legend.text=legend, las=1, args.legend=list(horiz=TRUE, bty='o', box.lwd=0, xjust=1, text.width=widths, x.intersp=0.3, x="topright")) # ncol=3, inset=-0.3
  title("Élasticité implicite subjective de l'offre de travail pour une variation de salaire horaire de ... ", outer=TRUE)
  par(before_par)
}
elasticite()
decrit(m$elasticite[m$variante_reaction==0.25])
decrit(m$elasticite[m$variante_reaction==0.1])
decrit(m$elasticite[m$variante_reaction==-0.1])
decrit(m$elasticite[m$variante_reaction==-0.25])

summary(lm(elasticite ~ sexe + revenu, data=m, subset=(variante_reaction>0)))
summary(lm(elasticite ~ sexe + revenu, data=m, subset=(variante_reaction<0)))

decrit(m$interet_politique[m$age=="18 - 24"], weights=m$weight[m$age=="18 - 24"])
decrit(m$interet_politique, weights=m$weight)
summary(lm(((interet_politique=="Beaucoup") - (interet_politique=="Presque pas (ou pas du tout)")) ~ Age + Diplome + revenu +sexe + gauche_droite, data=m, weights=weight))


#### Export to deep learning ####
# On prend le parti de n'inclure que les réformes soutenues par une majorité, pas les majorités soutenant le statu quo ni les mesures sans majorité
# On inclut dans les NA les NSP (il y a donc trois modalités: True, False et NA). Les True correspondent à une volonté déterminée de changement (ex: femmes <- (femmes>0)
# Les dépenses publiques ont été exclues car les réponses à cette question sont incohérentes
export_dl <- function(include_others=TRUE) {
  x <- m[,c("ID", "date")]
  vars <- c("pour_referendums", "pour_voter_propositions", "pour_budgets_participatifs", "pour_isoler_batiments", "pour_taxe_ghg_monde", "pour_baisser_conso_energie",
            "pour_favoriser_bio", "humanisme", "taxe_mondiale_capital", "assemblee_finance", "assemblee_climat", "taxe_mondiale_riches",
            "cap_and_share", "simplification", "revenu_max_fr_100k", "revenu_max_fr_3m", "revenu_max_monde_100k", "revenu_max_monde_3m", "revenu_max_monde",
            "transferts_inter_a",  "taxation_capital", "deduction_loyer_impots",
            "revenu_base_mondial", "approbation_mediane_rdb", "approbation_mediane_aid", "approbation_mediane",
            "approbation_moyenne", "rdb_eu", "fusion_irpp_cotsoc",
            "retraite_modulee_csp", "quota_commune", "impot_eu_benefices", "retraite_libre", "formation",
            "subventions", "investissement", "baisse_cotis", "baisser_conso_energie", "liberaliser", "rtt",
            "taxe_ghg_monde", "favoriser_bio", "isoler_batiments", "voitures_electriques", "electricite_sans_co2", "taxe_ghg_fr",
            "referendums", "voter_propositions", "baisser_conso_viande", "salaries_ca", "mixed_member_proportional", "budgets_participatifs",
            "cyberdemocratie", "senat_aleatoire", "voter_budget", "convoquer_constituante", "conserver_constitution", "plus_democratie"
            )
  for (var in vars) {
    x[[var]] <- is.element(m[[var]], c("Oui", "Pour", "Taxer davantage le capital", 1, 2))
    is.na(x[[var]]) <- (grepl("NSP", m[[var]]) | (m[[var]]==0) | is.na(m[[var]]))
    if (!is.null(annotation(m[[var]])[1])) { is.na(x[[var]]) <- is.missing(m[[var]]) }
  }
  x <- x[,!(names(x) %in% c("ID", "date"))]
  for (var in c("taxe_fonciere", "cot_soc", "tva", "taxe_revenus_fonciers", "actionnaires", "loyers")) {
    x[[var]] <- (m[[var]] < 0)
    is.na(x[[var]]) <- (grepl("NSP", m[[var]]) | is.na(m[[var]]) | is.missing(m[[var]]))
  }
  for (var in c("femmes", "retraites","smicards", "etudiants","sdf", "heures_supp")) {
    x[[var]] <- (m[[var]] > 0)
    is.na(x[[var]]) <- (grepl("NSP", m[[var]]) | is.na(m[[var]]) | is.missing(m[[var]]))
  }
  x$transferts_inter <- m$transferts_inter >= 5
  is.na(x[["transferts_inter"]]) <- (grepl("NSP", m[["transferts_inter"]])  | is.na(m[["transferts_inter"]]))
  x$transferts_plus_1 <- m$transferts_inter > 1
  is.na(x[["transferts_plus_1"]]) <- (grepl("NSP", m[["transferts_inter"]]) | is.na(m[["transferts_inter"]])) 
  x$ue <- grepl("renégocier", m$france_et_ue)
  is.na(x$ue) <- (grepl("NSP", m$france_et_ue) | is.na(m$france_et_ue))
  x$bareme <- grepl("Oui", m$choix_bareme_sondage)
  is.na(x$bareme) <- (grepl("NSP", m$choix_bareme_sondage) | is.na(m$choix_bareme_sondage))
  x$deficit <- grepl("ramener", m$dette)
  is.na(x$deficit) <- (grepl("NSP", m$dette) | is.na(m$dette))
  x$smic <- grepl("augmentation du SMIC", m$quelle_redistribution)
  is.na(x$smic) <- (grepl("NSP", m$quelle_redistribution) | is.na(m$quelle_redistribution))
  x$rev_max_fini <- (m$revenu_max < Inf)
  x$rapport_max_fini <- (m$rapport_max<Inf)
  x$rapport_max_moins101 <- (m$rapport_max<=100)
  x$rapport_max_fr_moins101 <- (m$rapport_max_fr<=100)
  print(length(colnames(x)))
  if (include_others) {
    x$ID <- m$ID
    for (var in c("weight","variante_max","variante_transferts_inter","sexe",
                  "csp","Age","taille_agglo","region","Diplome","revenu","Patrimoine",
                  "Patrimoine_futur","rev_tot","gauche_droite","revenu_merite","mail", #"champ_libre",
                  "qualite", "qualite_sincerite", "qualite_comprehension", "statut_emploi", "situation_maritale")) {
      x[[var]] <- m[[var]]
    }
  }
  write.table(x, file="/home/adrien/Google Drive/Economie/Travail/enquete/codes/bdd_deep_learning.csv", sep=";")
  return(x)
}
x <- export_dl()

export_dl_restr <- function() {
  x <- m[,c("ID", "date")]
  vars <- c("taxe_mondiale_capital", "assemblee_finance", "assemblee_climat", "taxe_mondiale_riches",
            "cap_and_share", "taxation_capital", "deduction_loyer_impots",
            "revenu_base_mondial", "approbation_mediane_aid", "approbation_mediane", "rdb_eu", 
            "retraite_modulee_csp", "quota_commune", "impot_eu_benefices", "retraite_libre", "formation",
            "subventions", "investissement", "baisse_cotis", "baisser_conso_energie", "liberaliser", "rtt",
            "taxe_ghg_monde", "favoriser_bio", "isoler_batiments", "voitures_electriques", "electricite_sans_co2", "taxe_ghg_fr",
            "referendums", "voter_propositions", "baisser_conso_viande", "salaries_ca", "mixed_member_proportional", "budgets_participatifs",
            "cyberdemocratie", "senat_aleatoire", "voter_budget", "convoquer_constituante", "conserver_constitution", "plus_democratie"
            )
  for (var in vars) {
    x[[var]] <- is.element(m[[var]], c("Oui", "Pour", "Taxer davantage le capital", 1, 2))
    is.na(x[[var]]) <- (grepl("NSP", m[[var]]) | (m[[var]]==0) | is.na(m[[var]]))
    if (!is.null(annotation(m[[var]])[1])) { is.na(x[[var]]) <- is.missing(m[[var]]) }
  }
  x <- x[,!(names(x) %in% c("ID", "date"))]
  # x$transferts_inter <- m$transferts_inter >= 5
  # is.na(x[["transferts_inter"]]) <- (grepl("NSP", m[["transferts_inter"]])  | is.na(m[["transferts_inter"]]))
  x$transferts_plus_1 <- m$transferts_inter > 1
  is.na(x[["transferts_plus_1"]]) <- (grepl("NSP", m[["transferts_inter"]]) | is.na(m[["transferts_inter"]])) 
  x$ue <- grepl("renégocier", m$france_et_ue)
  is.na(x$ue) <- (grepl("NSP", m$france_et_ue) | is.na(m$france_et_ue))
  x$bareme <- grepl("Oui", m$choix_bareme_sondage)
  is.na(x$bareme) <- (grepl("NSP", m$choix_bareme_sondage) | is.na(m$choix_bareme_sondage))
  x$deficit <- grepl("ramener", m$dette)
  is.na(x$deficit) <- (grepl("NSP", m$dette) | is.na(m$dette))
  x$smic <- grepl("augmentation du SMIC", m$quelle_redistribution)
  is.na(x$smic) <- (grepl("NSP", m$quelle_redistribution) | is.na(m$quelle_redistribution))
  x$rev_max_fini <- (m$revenu_max < Inf)
  x$rapport_max_fini <- (m$rapport_max<Inf)
  print(length(colnames(x)))
  x$weight <- m$weight
  return(x)
}
x2 <- export_dl_restr()

#### Majorité jointe ####
majorite_jointe <- function(vars, seuil=0.5, weights=TRUE, d=x, print=FALSE, return_res=FALSE) {
  all_true <- 1:length(d[[vars[1]]])
  all_here <- 1:length(d[[vars[1]]])
  for (var in vars) {   
    all_true <- intersect(all_true, which(d[[var]])) 
    all_here <- intersect(all_here, which(!is.na(d[[var]])))
  }
  if (weights) { res <- sum(d[['weight']][all_true])/sum(d[['weight']][all_here])  }
  else { res <- length(all_true)/length(all_here) }
  if (print) { print(res) }
  # if (is.na(res)) { res <- -1 }
  if (return_res) { return(res) }
  else { return(res>=seuil) }
}

package("foreach")
package("doParallel")
# detectCores() 
# stopCluster(cl) # In case you stop a function using parallel computing (i.e. foreach's %dopar%), there may be some bugs afterwards ("Error: worker initialization failed"). Then, you have to stop and re-make the clusters. If it is not enough, try > rm(cl)
# parties_traitees_s60_n47 <- parties_traitees
cl <- makeCluster(4)
registerDoParallel(cl)
start_time <- proc.time()
Seuil <- 0.65
data <- x2
n <- 47
Weights <- TRUE
vars <- colnames(data)[1:n]
comb <- function(x,...) {
  lapply(seq_along(x),
    function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
}
parties_minoritaires <- list()
parties_majoritaires <- list()
parties_majoritaires[[1]] <- list()
parties_traitees <- list()
for (u in 1:n) {  
  parties_majoritaires[[1]] <- c(parties_majoritaires[[1]], list(c(u))) 
  parties_traitees[[u]] <- list() 
  parties_traitees[[u]][[n+1]] <- FALSE }
k <- 2
while (k < n & length(parties_majoritaires[[k-1]])>0) {
  print(k)
  w <- 0
  parties_majoritaires[[k]] <- list()
  for (l in 1:n) {
    parties_traitees_et_maj_new <- foreach(h=parties_majoritaires[[k-1]], .export=c('majorite_jointe'),
              .combine='comb', .multicombine=TRUE, .init=list(list(), list())) %dopar% {
      subset <- sort(c(h,l))
      impossible <- FALSE
      q <- 1
      while (q<k & !impossible) {
        if (length("[["(parties_traitees, subset[1:q]))<=1) { impossible <- TRUE }
        q <- q+1 }
      if (!impossible) {
        if (!is.element(l, h) & (is.null("[["(parties_traitees,subset))) ) { # éviter les doublons
          vars_h <- c()
          for (g in h) {  vars_h <- c(vars_h, vars[g]) }
          majj <- majorite_jointe(c(vars_h, vars[l]), seuil=Seuil, d=data, weights=Weights)
          if (!is.na(majj) & majj) {  
            return(list(subset, TRUE))  }
          else { return(list(subset, FALSE)) }
        }
      }
    }
    s <- 0
    parties_maj_new <- list()
    for (z in parties_traitees_et_maj_new[[1]]) { 
      s <- s+1 
      if (!is.null(z)) { 
        "[["(parties_traitees,z) <- TRUE 
        if (parties_traitees_et_maj_new[[2]][[s]]) { parties_maj_new <- c(parties_maj_new, list(z)) }
        else { parties_minoritaires <- c(parties_minoritaires, list(z))  } } }
    parties_majoritaires[[k]] <- c(parties_majoritaires[[k]], parties_maj_new)
    w <- w+1
    if (w/10==round(w/10)) { print(length(parties_majoritaires[[k]])) }
  } 
  for (h in parties_majoritaires[[k]]) { 
    "[["(parties_traitees,sort(h)) <- list() 
    "[["(parties_traitees,c(sort(h),n+1)) <- FALSE }
  k <- k + 1
}
proc.time() - start_time

# vérification que "set" est bien un sous-ensemble majoritaire, ~ 2*10^6 / min
# le pb c'est les non-réponses: avec un petit nombre de répondants sur l'ensemble candidat à majorité jointe, rien n'assure qu'un sous-ensemble avec plus de répondants conserve une majorité jointe
verification <- function(set, parties, data=x, continue=FALSE) {
  start_time <- proc.time()
  i <- 20 # 1
  ok <- TRUE
  nb_par_min <- length(parties)
  contrex <- c()
  min_maj <- 1
  while (i < nb_par_min & (ok | continue)) {
    i <- i + 1
    if (all(parties[[i]] %in% set)) { 
      contrex <- c(contrex, i)
      maj_i <- majorite_jointe(vars[parties[[i]]], print=FALSE, d=data, return_res=TRUE)
      if (maj_i < min_maj) { 
        min_set <- i
        min_maj <- maj_i
        print(min_maj) }
      ok <- FALSE 
    }
    if (round(i/1000000)==i/1000000) print(i)
  }
  if (continue) { print(paste("min_set: ",min_set, ", min_maj: ", round_any(min_maj, 0.0001), ", nb_contrex: ", length(contrex), " (",round_any((proc.time() - start_time)[1]/60,0.01)," min)", sep="")) }
  else { print(paste(ok, ", i: ", i, " (",(proc.time() - start_time)/60," min)", sep="")) }
  # return(i)
}
verification(set, parties_minoritaires)

# Weights=T (W), seuil=0.5 (s), n=80, 4h37 (5h30: 6 completed, 13h50: 7 completed)
# h= 11 40 52 56 58 72 77
nb_par_maj <- data.frame(1:6, c(80,695,4000,15000,45000,117000)) # nb parties_majoritaires de taille 7: >240000
summary(lm(log10(nb_par_maj[,2]) ~ log10(nb_par_maj[,1]))) # R^2: 0.987
plot(log10(nb_par_maj[,1]), log10(nb_par_maj[,2]))
plot(seq(1,17,0.1), 57.544*seq(1,17,0.1)^4.08, type="l")
lines(nb_par_maj[,1], nb_par_maj[,2])

# W=F, s=0.6, n=47, 68 min
# Résultat : UNE partie à 19 éléments (4 12 13 15 20 23 24 25 26 27 28 30 32 33 34 35 36 39 41)
# OK jointe: démocratie, écologie, transferts inter, quota commune, retraite / Not: UE, dette, approbation_mediane
set_s60 <- parties_majoritaires_s60_n47[[19]][[1]]
verification(set_s60, parties_minoritaires_s60_n47, data=x2, continue=TRUE)
# ECHEC de la vérification: min_set: 33861, min_maj: 0.2162, nb_contrex: 9093 (0.19 min)
vars[parties_minoritaires_s60_n47[[33861]]]
majorite_jointe(parties_majoritaires[[19]][[1]], print=TRUE, d=x2, weights=FALSE, seuil=0.6)
majj_s60 <- vars[parties_majoritaires[[19]][[1]]]
nb_par_maj <- function(vec) { 
  res <- c()
  for (c in vec) { res <- c(res, length(parties_majoritaires[[c]]))  }
  return(res)
}
plot(1:20,nb_par_maj(1:20)) # Étonnant : ça décroît puis ça recroît (c'est possible car les réponses sont incomplètes)

# W=T, s=0.7, n=47, 1 min
# Résultat : 3 parties à 9 éléments (16, 30, 32, 33, 34, 35, 40, 41) + 20, 24 ou 27, démocratie, écologie et transferts inter (majj de 0.68 en incluant les 3)
# SUCCES de la vérification: min_set: 15956, min_maj: 0.549697445949665 (0.0131333333333411 min)
parties_majoritaires_W_s70_n47 <- parties_majoritaires
parties_minoritaires_W_s70_n47 <- parties_minoritaires
parties_traitees_W_s70_n47 <- parties_traitees
set_s70 <- c(16, 30, 32, 33, 34, 35, 40, 41, 20, 24, 27)
verification(set_s70, parties_minoritaires_W_s70_n47, data=x2, continue=TRUE)

# Weights=T (W), seuil=0.65 (s), n=47, 5 min
# Résultat: 1 partie à 19 éléments
parties_majoritaires_W_s65_n47 <- parties_majoritaires
parties_minoritaires_W_s65_n47 <- parties_minoritaires
parties_traitees_W_s65_n47 <- parties_traitees
set_s65 <- parties_majoritaires_W_s65_n47[[19]][[1]]
verification(set_s65, parties_minoritaires_W_s65_n47, data=x2, continue=TRUE)
# ECHEC de la vérification: min_set: 17665, min_maj: 0.2162, nb_contrex: 5232 (0.29 min)

parties_majoritaires_W_s60_n47 <- parties_majoritaires
parties_traitees_W_s60_n47 <- parties_traitees
majorite_jointe(c("femmes", "smicards", "sdf"), print=TRUE)

# package("scales")
# ggplot(df, aes(x="", y=value, fill=group))+geom_bar(width = 1, stat = "identity")+ coord_polar("y")+ scale_fill_brewer("Blues") + blank_theme + theme(axis.text.x=element_blank())+  geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]),      label = percent(value/100)), size=5)


##### Jugement majoritaire #####
distribution_names <- c("note_actuel", "note_egalitaire", "note_personnalise", "note_mediane", "note_mediane_rdb", "note_utilitarien", "note_rawlsien")
grades_m <- data5(distribution_names, miss=FALSE)

# Détermine la note du k-eme quantile, k petit correspondant à une mauvaise note
old_gauge <- function(grades, k = 0.5, name="", print = T, return_text = FALSE) {
  p <- 1
  alpha <- 0
  i <- 0
  while (alpha < k) { 
    i <- i + 1
    alpha <- alpha + grades[i]
    p <- p - grades[i]  }
  q <- alpha - grades[i]
  g <- i-3
  if (g > -2 & g < 2) {
    if (p*k > q*(1-k)) {
      text = paste(g, "+ ", sep="") }
    else {
      text = paste(g, "- ", sep="") }  }
  else text = paste(g," ")
  if (print & !return_text) print(paste(text, name, sep=""))
  if (return_text) return(text)
  else return(g + (g>-2 & g<2)*(p/(p+q) - (1-k)) + (p==0)*(k+1-q) + (q==0)*(p-(1-k)))
}
score_s <- function(grades, k = 0.5, name="", print = T, return_text = FALSE) {
  alpha <- 0
  i <- 0
  while (alpha < k) { 
    i <- i + 1
    alpha <- alpha + grades[i]  }
  p <- 1 - alpha
  q <- alpha - grades[i]
  g <- i-3
  if (g > -2 & g < 2) {
    if (p*k > q*(1-k)) text = paste(g, "+ ", sep="") 
    else  text = paste(g, "- ", sep="")  }
  else text = paste(g," ")
  if (print & !return_text) print(paste(text, name, sep=""))
  if (return_text) return(text)
  else return(g + 0.5*(p/(p+q) +k-1 + (p*q==0)*(p-q) + (p+q==0)*0.5))
}
score_delta <- function(grades, k = 0.5, name="", print = T, return_text = FALSE) {
  alpha <- 0
  i <- 0
  while (alpha < k) { 
    i <- i + 1
    alpha <- alpha + grades[i]  }
  p <- 1 - alpha
  q <- alpha - grades[i]
  g <- i-3
  if (g > -2 & g < 2) {
    if (p*k > q*(1-k)) text = paste(g, "+ ", sep="") 
    else  text = paste(g, "- ", sep="")  }
  else text = paste(g," ")
  if (print & !return_text) print(paste(text, name, sep=""))
  if (return_text) return(text)
  else return(g + p - q)
}
mj <- function(grades, k = 0.5, name="", print = T, return_text = FALSE) {
  alpha <- 0
  i <- 0
  while (alpha < k) { 
    i <- i + 1
    alpha <- alpha + grades[i]  }
  p <- 1 - alpha
  q <- alpha - grades[i]
  g <- i-3
  if (g > -2 & g < 2) {
    if (p*k > q*(1-k)) text = paste(g, "+ ", sep="") 
    else  text = paste(g, "- ", sep="")  }
  else text = paste(g," ")
  if (print & !return_text) print(paste(text, name, sep=""))
  if (return_text) return(text)
  else return(g + (p>q)*p - (p<=q)*q)
}
old_gauges <- function(k = 0.5, grades = grades_m, names = distribution_names, print = T, return_text = FALSE) {
  # res <- matrix(ncol = length(names), nrow = 4)
  res <- c()
  # for (i in 1:ncol(grades)) res[,i] <- gauge(grades[,i], k, names[i])
  for (i in 1:ncol(grades)) res <- c(res, gauge(grades[,i], k, names[i], print, return_text))
  if (return_text) return(res)
  else return(round(res, 3))
}
old_ranking <- function(k = 0.5, grades = grades_m, names = distribution_names, print = FALSE) {
  res <- matrix(nrow = ncol(grades), ncol = 3)
  gauges <- gauges(k, grades, names, print)
  res[,1] <- names[order(gauges, decreasing = TRUE)]
  res[,2] <- gauges(k, grades, names, print, return_text = TRUE)[order(gauges, decreasing = TRUE)]
  res[,3] <- sort(gauges, decreasing = TRUE)
  return(res)
}
old_rankings <- function(quantiles = c(0.1, 0.2, 0.3, 0.4, 0.5), main = 0.5, grades = grades_m, names = distribution_names, print_names = T, return_text = FALSE) {
  main_gauges <- gauges(main, grades, names, print = FALSE)
  order <- order(main_gauges, decreasing = TRUE)
  if (print_names) {
    res <- matrix(nrow = ncol(grades), ncol = 3+length(quantiles))
    res[,1] <- names[order]  }
  else res <- matrix(nrow = ncol(grades), ncol = 2+length(quantiles))
  for (i in (1:length(quantiles))) {
    res[,i+print_names] <- gauges(quantiles[i], grades, names, print = FALSE, return_text)[order] }
  if (print_names) res[,length(quantiles)+print_names+1] <- gauges(main, grades, names, print = FALSE, return_text = T)[order]
  else res[,length(quantiles)+print_names+1] <- (substr(gauges(main, grades, names, print = FALSE, return_text = T)[order], 1,1)=="-")*as.numeric(paste(substr(gauges(main, grades, names, print = FALSE, return_text = T)[order], 2,2),0,sep=""))*(-0.1) + (substr(gauges(main, grades, names, print = FALSE, return_text = T)[order], 1,1)!="-")*as.numeric(paste(substr(gauges(main, grades, names, print = FALSE, return_text = T)[order], 1,1),0,sep=""))/10
  for (j in (1:ncol(grades))) res[j,length(quantiles)+print_names+2] <- round(sum(grades[,order[j]]*(-2:2)),3)
  return(res)
}
aggregate_scores <- function(k = 0.5, fun='old_gauge', grades = grades_m, names = distribution_names, print = T, return_text = FALSE) {
  # res <- matrix(ncol = length(names), nrow = 4)
  res <- c()
  # for (i in 1:ncol(grades)) res[,i] <- gauge(grades[,i], k, names[i])
  for (i in 1:ncol(grades)) {
    if (fun=='old_gauge') res <- c(res, old_gauge(grades[,i], k, names[i], print, return_text))
    else if (fun=='mj') res <- c(res, mj(grades[,i], k, names[i], print, return_text))
    else if (fun=='score_s') res <- c(res, score_s(grades[,i], k, names[i], print, return_text))
    else if (fun=='score_delta') res <- c(res, score_delta(grades[,i], k, names[i], print, return_text))
  }
  if (return_text) return(res)
  else return(round(res, 3))
}
ranking <- function(k = 0.5, fun='old_gauge', grades = grades_m, names = distribution_names, print = FALSE) {
  res <- matrix(nrow = ncol(grades), ncol = 3)
  scores <- aggregate_scores(k, fun=fun, grades, names, print)
  scores_text <- aggregate_scores(k, fun=fun, grades, names, print, return_text = TRUE)
  res[,1] <- names[order(scores, decreasing = TRUE)]
  res[,2] <- scores_text[order(scores, decreasing = TRUE)]
  res[,3] <- sort(scores, decreasing = TRUE)
  return(res)
}
rankings <- function(quantiles = c(0.1, 0.2, 0.3, 0.4, 0.5), fun='old_gauge', main = 0.5, grades = grades_m, names = distribution_names, print_names = T, return_text = FALSE) {
  main_scores <- aggregate_scores(main, fun=fun, grades, names, print = FALSE)
  order <- order(main_scores, decreasing = TRUE)
  if (print_names) {
    res <- matrix(nrow = ncol(grades), ncol = 3+length(quantiles))
    res[,1] <- names[order]  }
  else res <- matrix(nrow = ncol(grades), ncol = 2+length(quantiles))
  for (i in (1:length(quantiles))) {
    res[,i+print_names] <- aggregate_scores(quantiles[i], fun=fun, grades, names, print = FALSE, return_text)[order] }
  if (print_names) res[,length(quantiles)+print_names+1] <- aggregate_scores(main, fun=fun, grades, names, print = FALSE, return_text = T)[order]
  else res[,length(quantiles)+print_names+1] <- (substr(aggregate_scores(main, fun=fun, grades, names, print = FALSE, return_text = T)[order], 1,1)=="-")*as.numeric(paste(substr(aggregate_scores(main, fun=fun, grades, names, print = FALSE, return_text = T)[order], 2,2),0,sep=""))*(-0.1) + (substr(aggregate_scores(main, fun=fun, grades, names, print = FALSE, return_text = T)[order], 1,1)!="-")*as.numeric(paste(substr(aggregate_scores(main, fun=fun, grades, names, print = FALSE, return_text = T)[order], 1,1),0,sep=""))/10
  for (j in (1:ncol(grades))) res[j,length(quantiles)+print_names+2] <- round(sum(grades[,order[j]]*(-2:2)),3)
  return(res)
}
rankings(main=0.5)
ranking(fun='mj')
ranking(fun='score_s')
ranking(fun='score_delta')
rankings(c(0.15, 0.25, 0.35, 0.45, 0.5), fun='mj')

decrit(m$vague[m$note_actuel!=""])
decrit(m$vague[m$note_utilitarien!=""])
decrit(m$vague[m$note_rawlsien!=""])
decrit(m$vague[m$note_personnalise!=""])
decrit(m$vague[m$note_egalitaire!=""])
decrit(m$vague[m$note_mediane_rdb!=""])
decrit(m$vague[m$note_mediane!=""])

ci.median(as.numeric(m$note_mediane[!is.na(m$note_mediane)]))
ci.median(as.numeric(m$note_mediane_rdb[!is.na(m$note_mediane_rdb)]))
CI(as.numeric(m$note_mediane[!is.na(m$note_mediane)]))
CI(as.numeric(m$note_mediane_rdb[!is.na(m$note_mediane_rdb)]))
mood.test(m$note_mediane_rdb[!is.na(m$note_mediane_rdb)], m$note_mediane[!is.na(m$note_mediane)])
mood.test(m$note_utilitarien[!is.na(m$note_utilitarien)], m$note_rawlsien[!is.na(m$note_rawlsien)])
mood.test(m$note_rawlsien[!is.na(m$note_rawlsien)], m$note_mediane_rdb[!is.na(m$note_mediane_rdb)])

m_1 <- m_2 <- m_3 <- m_4 <- m_5 <- m_6 <- m_7 <- m[,c("weight", "note_actuel", "note_utilitarien", "note_rawlsien", "note_personnalise", "note_egalitaire", "note_mediane_rdb", "note_mediane", "variante_approbation", "approbation", "approbation_mediane", "approbation_mediane_rdb", "approbation_moyenne", "approbation_mediane_aid", "sexe", "revenu", "revenu_conjoint", "incompris", "taille_agglo", "Patrimoine", "Patrimoine_futur", "Heritage_futur", "csp", "qualite", "qualite_comprehension", "qualite_sincerite", "gauche_droite", "Diplome", "revenu_max", "choix_bareme_sondage", "note_actuel", "note_mediane", "note_mediane_rdb", "note_egalitaire", "note_rawlsien", "note_utilitarien", "Age", "niveau_vie", "rev_tot", "revdisp", "taille_foyer", "age")]
m_1$note <- m_1$note_actuel
m_1$note_variante[m_1$note_actuel!=""] <- "_a"
m_2$note <- m_2$note_utilitarien
m_2$note_variante[m_2$note_utilitarien!=""] <- "_u"
m_3$note <- m_3$note_rawlsien
m_3$note_variante[m_3$note_rawlsien!=""] <- "_r"
m_4$note <- m_4$note_personnalise
m_4$note_variante[m_4$note_personnalise!=""] <- "_p"
m_5$note <- m_5$note_egalitaire
m_5$note_variante[m_5$note_egalitaire!=""] <- "_e"
m_6$note <- m_6$note_mediane_rdb
m_6$note_variante[m_6$note_mediane_rdb!=""] <- "_d"
m_7$note <- m_7$note_mediane
m_7$note_variante[m_7$note_mediane!=""] <- "_m"
m7 <- rbind(m_1, m_2, m_3, m_4, m_5, m_6, m_7)
rm(m_1, m_2, m_3, m_4, m_5, m_6, m_7)
m7$note_variante <- relevel(as.factor(m7$note_variante), ref = "_u")

summary(lm(note ~ note_variante, data=m7, weights=m7$weight))

m7$note_variante <- relevel(as.factor(m7$note_variante), ref = "_m")
summary(lm(note ~ note_variante, data=m7, weights=m7$weight))

decrit(m$approbation_mediane[m$revdisp<=3000 & m$revdisp>=2200], weights=m$weight[m$revdisp<=3000 & m$revdisp>=2200])
decrit(m$approbation_mediane_rdb[m$revdisp<=3000 & m$revdisp>=2200], weights=m$weight[m$revdisp<=3000 & m$revdisp>=2200])
decrit(m$note_mediane[m$revdisp<=3000 & m$revdisp>=2200], weights=m$weight[m$revdisp<=3000 & m$revdisp>=2200])
decrit(m$note_mediane_rdb[m$revdisp<=3000 & m$revdisp>=2200], weights=m$weight[m$revdisp<=3000 & m$revdisp>=2200])
summary(lm((Approbation=="Oui") ~ Approbation_variante  + (m4$revdisp<=3000 & m4$revdisp>=2200) + (Approbation_variante=="_s" & m4$revdisp<=3000 & m4$revdisp>=2200), data=m4, subset=(m4$Approbation_variante=="_rdb" | m4$Approbation_variante=="_s")), weights=m$weight)
m$spread_approbation <- 1*(m$approbation_mediane_rdb=="Oui" & m$approbation_mediane=="Non")-1*(m$approbation_mediane_rdb=="Non" & m$approbation_mediane=="Oui")
is.na(m$spread_approbation) <- is.na(m$approbation_mediane) | is.na(m$approbation_mediane_rdb) | (m$approbation_mediane_rdb=="NSP") | (m$approbation_mediane=="NSP") | (m$approbation_mediane_rdb=="") | (m$approbation_mediane=="")
summary(lm(m$spread_approbation ~ (m$revdisp<=3000 & m$revdisp>=2200)))

decrit(m$approbation_mediane[m$revdisp>3000], weights=m$weight[m$revdisp>3000])
decrit(m$approbation_mediane_rdb[m$revdisp>3000], weights=m$weight[m$revdisp>3000])


##### Piketty 2018 #####
decrit(as.numeric(m$refugies), miss=T, weights=m$weight) # 1: non posé ; 2: Accepter tous -> 7: aucun  (5: autant); 8: commune; 9: parrainage; 10: NSP
m$refugies_echelle <- as.numeric(m$refugies)
is.na(m$refugies_echelle) <- as.numeric(m$refugies)==1 | as.numeric(m$refugies)>7
m$moinsRefugies <- (as.numeric(m$refugies) == 6) | (as.numeric(m$refugies) == 7)
is.na(m$moinsRefugies) <- (as.numeric(m$refugies) == 1)
decrit(m$moinsRefugies, weights=m$weight)
m$pro_redistribution <- (m$approbation_mediane_rdb == "Oui") | (m$approbation_moyenne == "Oui")
is.na(m$pro_redistribution) <- is.missing(m$approbation_mediane_rdb) & is.missing(m$approbation_moyenne)
m$anti_redistribution <- (m$approbation_mediane_rdb == "Non") | (m$approbation_moyenne == "Non")
decrit(m$pro_redistribution, weights=m$weight)
cor(m$pro_redistribution, m$moinsRefugies, use = "complete.obs") # corrélation -0.05
summary(lm(m$pro_redistribution ~ m$moinsRefugies, weights=m$weight)) # no correlation on 307 obs.
cor(m$anti_redistribution, m$moinsRefugies, use = "complete.obs") # corrélation 0.07
summary(lm(m$anti_redistribution ~ m$moinsRefugies, weights=m$weight)) # no correlation on 307 obs.
length(which(m$pro_redistribution & m$moinsRefugies))
length(which(m$pro_redistribution & !m$moinsRefugies))
length(which(m$anti_redistribution & m$moinsRefugies))
length(which(m$anti_redistribution & !m$moinsRefugies)) 

# corrélation pro_migration éducation, 305 obs
summary(lm(m$moinsRefugies ~ m$Diplome, weights=m$weight)) # 
summary(lm(m$moinsRefugies ~ m$Diplome)) # **
summary(lm(m$refugies_echelle ~ m$Diplome, weights=m$weight)) # * dans le bon sens
summary(lm(m$refugies_echelle ~ (m$Diplome>4), weights=m$weight)) # ** < Bac+3 ou +5
summary(lm(m$refugies_echelle ~ (m$Diplome>3), weights=m$weight)) # * > Baccalauréat
summary(lm(m$refugies_echelle ~ (m$Diplome<3), weights=m$weight)) # * < Baccaularéat
summary(lm(m$refugies_echelle ~ as.character(m$Diplome), weights=m$weight)) # * Bac+5
summary(lm(m$refugies_echelle ~ (m$diplome=="CAP ou BEP"), weights=m$weight)) # 
summary(lm(m$refugies_echelle ~ m$Patrimoine, weights=m$weight)) # non significatif
summary(lm(m$refugies_echelle ~ I(pmin(m$revdisp,4500)/10^3), weights=m$weight)) # non significatif
summary(lm(m$refugies_echelle ~ m$Age + m$Diplome + m$Patrimoine + I(pmin(m$revdisp,4500)/10^3) + m$Patrimoine_futur)) # * Diplome
summary(lm(m$refugies_echelle ~ m$Age + m$Diplome + m$Patrimoine + I(pmin(m$revdisp,4500)/10^3) + m$Patrimoine_futur, weights=m$weight)) # . Diplome
summary(lm(m$refugies_echelle ~ m$Age + m$Diplome + m$Patrimoine + I(pmin(m$revdisp,4500)/10^3) + m$Patrimoine_futur, subset=!is.na(m$gauche_droite), weights=m$weight)) # . revenu
summary(lm(m$refugies_echelle ~ m$Age + m$Diplome + m$Patrimoine + I(pmin(m$revdisp,4500)/10^3) + m$Patrimoine_futur + m$gauche_droite, weights=m$weight)) # *** gauche_droite
summary(lm(m$gauche_droite ~ m$Diplome, weights=m$weight)) # non significatif
cor(m$gauche_droite, m$Diplome, use = "complete.obs") # -0.05
summary(lm((m$droite==T) ~ m$Diplome, weights=m$weight)) # ***
summary(lm((m$extr_droite==T) ~ m$Diplome, weights=m$weight)) # ***
summary(lm((m$gauche==T) ~ m$Diplome, weights=m$weight)) # ***


# corrélation pro_redistribution richesse, 995 obs
summary(lm(m$pro_redistribution ~ I(pmin(m$revdisp,4500)/10^3))) # non significatif
summary(lm(m$pro_redistribution ~ m$patrimoine)) # non significatif
summary(lm(m$pro_redistribution ~ m$Patrimoine))  # *** -0.05
summary(lm(m$pro_redistribution ~ m$Patrimoine_futur)) # non significatif
summary(lm(m$pro_redistribution ~ m$Diplome, subset=(m$Diplome!=-1))) # ** -0.05 pas le bon sens
summary(lm(m$pro_redistribution ~ (m$Diplome>4), subset=(m$Diplome!=-1))) # *** >Bac+2
summary(lm(m$pro_redistribution ~ (m$Diplome>3), subset=(m$Diplome!=-1))) # * > Baccalauréat
summary(lm(m$pro_redistribution ~ (m$Diplome<3), subset=(m$Diplome!=-1))) # * < Baccaularéat
summary(lm(m$pro_redistribution ~ (m$Diplome<1), subset=(m$Diplome!=-1))) # non sign >= Bac+5
summary(lm(m$pro_redistribution ~ as.character(m$Diplome), subset=(m$Diplome!=-1))) # non significatif
summary(lm(m$pro_redistribution ~ m$gauche, subset=(m$Diplome!=-1))) # *** 
summary(lm(m$pro_redistribution ~ m$Age + m$Diplome + m$Patrimoine + I(pmin(m$revdisp,4500)/10^3), weights=m$weight)) # *** -revenu, age
summary(lm((m4$Approbation=="Oui") ~ m4$Age + m4$Diplome + m4$Patrimoine + I(pmin(m4$revdisp,4500)/10^3), subset=(!is.missing(m4$Approbation)), weights=m4$weight)) # *** -revenu, age
summary(lm(m$pro_redistribution ~ m$Age + m$Diplome + m$Patrimoine + I(pmin(m$revdisp,4500)/10^3) + m$gauche_droite, subset=(m$Diplome!=-1), weights=m$weight)) # ** Patrimoine & Politique


# variable indépendante des deux premières: UE, dette
decrit(as.numeric(m$france_et_ue))
levels(m$france_et_ue)
m$eurocritique <- (as.numeric(m$france_et_ue)==2) | (as.numeric(m$france_et_ue)==3) | (as.numeric(m$france_et_ue)==7)
m$eurobeat <- (as.numeric(m$france_et_ue)==5) | (as.numeric(m$france_et_ue)==6)
is.na(m$eurocritique) <- (as.numeric(m$france_et_ue)==1)
is.na(m$eurobeat) <- (as.numeric(m$france_et_ue)==1)
cor(m$eurobeat, m$moinsRefugies, use = "complete.obs") # -0.19
cor(m$eurobeat, m$pro_redistribution, use = "complete.obs") # -0.07
cor(m$moinsRefugies, m$pro_redistribution, use = "complete.obs") # -0.08
decrit(m$eurobeat)
length(which(!is.na(m$eurobeat) & !is.na(m$pro_redistribution) & !is.na(m$moinsRefugies))) # 118 obs
length(which(!is.na(m$eurocritique) & !is.na(m$moinsRefugies))) # 148 obs
decrit(m$eurocritique, weights=m$weight)
cor(m$eurocritique, m$refugies_echelle, use = "complete.obs") # 0.32
cor(m$refugies_echelle, m$pro_redistribution, use = "complete.obs") # -0.23

decrit(as.numeric(m$dette[as.numeric(m$dette)!=1]))
levels(m$dette)
m$pro_dette <- as.numeric(m$dette)==4 | as.numeric(m$dette)==5
is.na(m$pro_dette) <- as.numeric(m$dette)==1
decrit(m$pro_dette)
cor(m$pro_dette, m$moinsRefugies, use = "complete.obs") # -0.11
cor(m$pro_dette, m$pro_redistribution, use = "complete.obs") # 0.11
cor(m$eurocritique, m$pro_dette, use = "complete.obs") # 0.24
length(which(!is.na(m$eurocritique) & !is.na(m$pro_redistribution) & !is.na(m$moinsRefugies) & !is.na(m$pro_dette))) # 39 obs
length(which(!is.na(m$moinsRefugies) & !is.na(m$pro_dette))) # 110 obs
length(which(!is.na(m$eurocritique) & !is.na(m$pro_dette))) # 110 obs

m$antiDeficit <- as.numeric(m$dette)==2 | as.numeric(m$dette)==3
is.na(m$antiDeficit) <- as.numeric(m$dette)==1
decrit(m$antiDeficit, weights=m$weight)
cor(m$antiDeficit, m$moinsRefugies, use = "complete.obs") # 0.08
cor(m$antiDeficit, m$pro_redistribution, use = "complete.obs") # -0.16
cor(m$eurocritique, m$moinsRefugies, use = "complete.obs") # 0.30
cor(m$eurocritique, m$pro_redistribution, use = "complete.obs") # -0.05
cor(m$moinsRefugies, m$pro_redistribution, use = "complete.obs") # -0.08
cor(m$antiDeficit, m$eurocritique, use = "complete.obs") # -0.17


# parti (revenu, éducation, patrimoine)
summary(lm((m$droite==T | m$centre==T) ~ m$Diplome, susbet=m$Diplome!=-1, weights=m$weight))
summary(lm((m$droite==T | m$centre==T) ~ m$Diplome, susbet=(m$Diplome!=-1), weights=m$weight))
summary(lm(m$Diplome ~ m$gauche_droite + I(m$gauche_droite^2) + m$Age, susbet=m$Diplome!=-1)) # *** extrêmes + éduqués: moins de peu éduqués
summary(lm((m$gauche==T) ~ m$Diplome + I(pmin(m$revdisp,4500)/10^3) + m$Patrimoine + m$Age + m$Patrimoine_futur)) # * -Patrimoine +Age
summary(lm((m$droite==T) ~ m$Diplome + I(pmin(m$revdisp,4500)/10^3) + m$Patrimoine + m$Age + m$Patrimoine_futur)) # * -éduc +Patrimoine
summary(lm((m$centre==T) ~ m$Diplome + I(pmin(m$revdisp,4500)/10^3) + m$Patrimoine + m$Age + m$Patrimoine_futur)) # *** -éduc +Age, Patrimoine
summary(lm((m$extr_droite==T) ~ m$Diplome + I(pmin(m$revdisp,4500)/10^3) + m$Patrimoine + m$Age + m$Patrimoine_futur)) # *** +éduc -Age
decrit(m$gauche_droite)
decrit(m$gauche_droite[m$pro_redistribution]) # + de gauche (- d'extr_droite)
decrit(m$gauche_droite[m$moinsRefugies]) # + d'extr_droite
decrit(m$gauche_droite[m$eurocritique]) # + d'extr_droite, pas bcp - de gauche
decrit(m$gauche_droite[m$pro_dette]) # + de gauche
decrit(m$gauche_droite[m$antiDeficit]) # - de gauche, + centre
summary(lm((m$pro_dette==T) ~ m$Age + m$Diplome + m$Patrimoine + m$revenu + m$Patrimoine_futur + m$gauche_droite)) # *** gauche_droite, * -Patrimoine
summary(lm((m$antiDeficit==T) ~ m$Age + m$Diplome + m$Patrimoine + m$revenu + m$Patrimoine_futur + m$gauche_droite)) # *** gauche_droite, * -Patrimoine
summary(lm((m$eurocritique==T) ~ m$Age + m$Diplome + m$Patrimoine + m$revenu + m$Patrimoine_futur + m$gauche_droite)) # *** gauche_droite, . éducation
summary(lm(m$pro_redistribution ~ m$gauche_droite + I(m$gauche_droite^2), weights = m$weight))
summary(lm(m$pro_redistribution ~ as.character(m$Gauche_droite), weights = m$weight)) # Gauche et abstentionnistes proRedistr
summary(lm(m$antiDeficit ~ m$gauche_droite + I(m$gauche_droite^2), weights = m$weight))
summary(lm(m$antiDeficit ~ as.character(m$Gauche_droite), weights = m$weight)) # Droite et Centre antiDeficit
summary(lm(m$moinsRefugies ~ m$gauche_droite + I(m$gauche_droite^2), weights = m$weight))
summary(lm(m$moinsRefugies ~ as.character(m$Gauche_droite), weights = m$weight)) # Centre et Gauche proRefugies
summary(lm(m$eurocritique ~ m$gauche_droite + I(m$gauche_droite^2), weights = m$weight))
summary(lm(m$eurocritique ~ as.character(m$Gauche_droite), weights = m$weight)) # Centre moins et Extr Droite plus eurocritique


# redi\immi pro  anti (<=> éduqué + à - ? => non, centre et droite sont les + éduqués)
#  pro  Gauche  Extr D
# anti  Centre  Droite

# Résultats en + de Piketty: 
# - éducation corrélée avec anti-redistribution (TODO: review literature) mais pas avec gauche
# - droite plus éduquée que le reste
# - eurocritique et pro_dette variables non corrélées aux deux premières
# - extrême droite anti redistribution

## Données post électorales 2017
# TODO: Piketty a pris comme définition des revenus le revenu net du ménage (sans corriger pour sa taille) => quel effet par / à nivvie ?
setwd("/home/adrien/Google Drive/Economie/Travail/enquete/codes")
elec <- read.spss("FES2017.sav")
# EU4: L’intégration européenne empêche la démocratie de bien fonctionner actuellement en France
decrit(elec$EU4)
elec$eurocritique <- 2*(elec$EU4=="Agree")+(elec$EU4=="Somewhat agree")-(elec$EU4=="Somewhat disagree")-2*(elec$EU4=="Disagree")
is.na(elec$eurocritique) <- (elec$EU4=="Refusal") | (elec$EU4=="DK")
elec$eurocritic <- pmax(-1,pmin(1,elec$eurocritique))
decrit(elec$O50) # Pour établir la justice sociale, il faudrait prendre aux riches pour donner aux pauvres
elec$proRedistribution <- 2*(elec$O50=="Agree") + (elec$O50=="Somewhat agree") - (elec$O50=="Somewhat disagree") - 2*(elec$O50=="Disgree")
is.na(elec$proRedistribution) <- (elec$O50=="Refusal") | (elec$O50=="DK") | (elec$O50=="Missing")
elec$proRedistr <- pmax(-1,pmin(1,elec$proRedistribution))
decrit(elec$O12) # Il y a trop d’immigrés en France
elec$antiMigration <- 2*(elec$O12=="Agree") + (elec$O12=="Somewhat agree") - (elec$O12=="Somewhat disagree") - 2*(elec$O12=="Disgree")
is.na(elec$antiMigration) <- (elec$O12=="Refusal") | (elec$O12=="DK") | (elec$O12=="Missing")
elec$antiMigr <- pmax(-1,pmin(1,elec$antiMigration))
cor(elec$proRedistribution, elec$antiMigration, use = "complete.obs") # 0.12
cor(elec$eurocritique, elec$antiMigration, use = "complete.obs") # 0.34
cor(elec$eurocritique, elec$proRedistribution, use = "complete.obs") # 0.17
cor(elec$proRedistr, elec$antiMigr, use = "complete.obs") # 0.05
cor(elec$eurocritic, elec$antiMigr, use = "complete.obs") # 0.29
cor(elec$eurocritic, elec$proRedistr, use = "complete.obs") # 0.13
decrit(elec$SD46) # individual income
decrit(elec$SD45) # household income


###### DÉBUT DE LA FIN ######
##### Durée #####
decrit(n(f$duree)/60)
plot(density(n(f$duree)[n(f$duree)<4000]/60, bw=0.5), xlim=c(0,30)) # Durée


##### Transferts inter #####
# TODO: 238 "" and 246 NA for variante_transferts_inter: what is the difference?
# TODO: rajouter lignes ci-dessous dans preparation
# TODO: study PNR per variant
t$type_transferts_inter <- NULL
t$type_transferts_inter[t$variante_transferts_inter=='l'] <- 'l'
t$type_transferts_inter[t$variante_transferts_inter=='s' & t$vague==3] <- 'e'
t$type_transferts_inter[t$vague==1] <- 'e'
t$type_transferts_inter[t$vague==2] <- 'l'
t$variante_transferts_inter[t$vague==3] <- 's'
label(t$type_transferts_inter) <- "type_transferts_inter: Type de la question transferts_inter: champ libre (l: vague 2 et moitié de 3) ou échelle de 0 à 20% (e: vague 1 et moitié de 3)"
decrit(t$vague[is.missing(t$variante_transferts_inter)]) # in vague 1, 246 NA: why? => because 'NSP' => TODO: allocate them randomly to s/c/i?
decrit(t$variante_transferts_inter[t$vague==1]) # 249/264/248 (s/c/i) instead of 335.67*3 => allocate 86/72/88 NSP to s/c/i? (no priming, scale)
t$transferts_inter[is.na(t$transferts_inter) & t$vague==1] <- -1

decrit(t$transferts_inter_a)
decrit(t$vague[!is.missing(t$transferts_inter_a)])
decrit(t$transferts_inter_actuel_vu[!is.missing(t$transferts_inter_a)])
decrit(t$transferts_inter_info[!is.missing(t$transferts_inter_a)])
decrit(f$variante_transferts_inter)
decrit(t$variante_transferts_inter)
decrit(t$type_transferts_inter)
decrit(t$transferts_inter_actuel_vu)
decrit(t$transferts_inter_info)
decrit(t$transferts_inter[t$vague==1])
ddply(t, .(variante_transferts_inter, type_transferts_inter), summarize, all=length(variante_transferts_inter), priming_wo_info=sum(transferts_inter_actuel_vu==1 & transferts_inter_info==0), priming_info=sum(transferts_inter_actuel_vu==1 & transferts_inter_info==1), no_priming_info=sum(transferts_inter_actuel_vu==0 & transferts_inter_info==1), no_priming_no_info=sum(transferts_inter_actuel_vu==0 & transferts_inter_info==0))
ddply(t, .(variante_transferts_inter, type_transferts_inter), summarize, all=length(variante_transferts_inter), priming_wo_info=sum(transferts_inter_actuel_vu==1 & transferts_inter_info==0), priming_info=sum(transferts_inter_actuel_vu==1 & transferts_inter_info==1), no_priming=sum(transferts_inter_actuel_vu==0))
ddply(t, .(variante_transferts_inter), summarize, all=length(variante_transferts_inter), priming_wo_info=sum(transferts_inter_actuel_vu==1 & transferts_inter_info==0), priming_info=sum(transferts_inter_actuel_vu==1 & transferts_inter_info==1), no_priming=sum(transferts_inter_actuel_vu==0))
ddply(t, .(!is.na(transferts_inter)), summarize, all=length(transferts_inter), priming_wo_info=sum(transferts_inter_actuel_vu==1 & transferts_inter_info==0), priming_info=sum(transferts_inter_actuel_vu==1 & transferts_inter_info==1), no_priming_info=sum(transferts_inter_actuel_vu==0 & transferts_inter_info==1), no_priming_no_info=sum(transferts_inter_actuel_vu==0 & transferts_inter_info==0))
ddply(t, .(variante_transferts_inter, type_transferts_inter), summarize, all=length(transferts_inter))
ddply(t, .(transferts_inter_actuel_vu), summarize, info=sum(transferts_inter_info==1), no_info=sum(transferts_inter_info==0))
CImedian <- function(vec) { # 95% confidence interval
  res <- tryCatch(unlist(ci.median(vec[!is.na(vec) & vec!=-1])), error=function(e) {print('NA')})
  return(paste(res[paste('ci.lower')], res[paste('ci.median')], res[paste('ci.upper')])) }
summary(lm(transferts_inter ~ (variante_transferts_inter=='l') + transferts_inter_info, data=f, subset=(f$transferts_inter!=-1)))
summary(lm(log(transferts_inter+0.0001) ~ (variante_transferts_inter=='l') + transferts_inter_info, data=f, subset=(f$transferts_inter!=-1)))
mean(f$transferts_inter)
mean(f$transferts_inter_cru, na.rm=T)
CImedian(t$transferts_inter) # 5 5 5
median(f$transferts_inter_cru, na.rm=T)
CImedian(t$transferts_inter[t$transferts_inter_info==FALSE]) # 5 5 5.9
CImedian(t$transferts_inter[t$transferts_inter_info==T]) # 1.9 2 2
CImedian(t$transferts_inter[t$transferts_inter_info==FALSE & t$transferts_inter_actuel_vu==T]) # 4.9 5 5
CImedian(t$transferts_inter[t$transferts_inter_info==T & t$transferts_inter_actuel_vu==T]) # 1.8 2 2
CImedian(t$transferts_inter[t$transferts_inter_info==FALSE & t$transferts_inter_actuel_vu==FALSE]) # 5 5 6.9
CImedian(t$transferts_inter[t$transferts_inter_info==T & t$transferts_inter_actuel_vu==FALSE]) # 1.5 2 3
CImedian(t$transferts_inter[t$variante_transferts_inter=='s']) # 4 4.9 5
CImedian(t$transferts_inter[t$variante_transferts_inter=='i']) # 5 6.1 9.9
CImedian(t$transferts_inter[t$variante_transferts_inter=='c']) # 5 5.45 9.9
CImedian(t$transferts_inter[t$transferts_inter_actuel_vu==T]) # 2 3 4
CImedian(t$transferts_inter[t$transferts_inter_actuel_vu==FALSE]) # 5 5 5
CImedian(t$transferts_inter[t$type_transferts_inter=='e']) # 4.9 4.9 5
CImedian(t$transferts_inter[t$type_transferts_inter=='l']) # 5 5 5
CImedian(t$transferts_inter[t$variante_transferts_inter=='s' & t$type_transferts_inter=='e']) # 3.6 4.2 4.9
CImedian(t$transferts_inter[t$variante_transferts_inter=='s' & t$type_transferts_inter=='l']) # 4 5 5
CImedian(t$transferts_inter[t$variante_transferts_inter=='i' & t$type_transferts_inter=='e']) # 5 8 10
CImedian(t$transferts_inter[t$variante_transferts_inter=='i' & t$type_transferts_inter=='l']) # 5 6.8 9.9
CImedian(t$transferts_inter[t$variante_transferts_inter=='c' & t$type_transferts_inter=='e']) # 5 6.8 9.9
CImedian(t$transferts_inter[t$variante_transferts_inter=='c' & t$type_transferts_inter=='l']) # 5 5 10
CImedian(t$transferts_inter[t$variante_transferts_inter=='s' & t$type_transferts_inter=='e' & t$transferts_inter_actuel_vu==FALSE]) # 4.2 4.9 5
CImedian(t$transferts_inter[t$variante_transferts_inter=='s' & t$type_transferts_inter=='e' & t$transferts_inter_actuel_vu==T & t$transferts_inter_info==FALSE]) # 3.2 5 5.6
CImedian(t$transferts_inter[t$variante_transferts_inter=='s' & t$type_transferts_inter=='e' & t$transferts_inter_actuel_vu==T & t$transferts_inter_info==T]) # 1 2 2
CImedian(t$transferts_inter[t$variante_transferts_inter=='s' & t$type_transferts_inter=='l' & t$transferts_inter_actuel_vu==FALSE]) # 5 5 5
CImedian(t$transferts_inter[t$variante_transferts_inter=='s' & t$type_transferts_inter=='l' & t$transferts_inter_actuel_vu==T & t$transferts_inter_info==FALSE]) # 5 5 6
CImedian(t$transferts_inter[t$variante_transferts_inter=='s' & t$type_transferts_inter=='l' & t$transferts_inter_actuel_vu==T & t$transferts_inter_info==T]) # 1 2 3
CImedian(t$transferts_inter_actuel) # 3.9 4.05 5
mood.test(transferts_inter ~ type_transferts_inter, data=t, weights=t$weight, alternative="t") # p-value = 0.06: equality not rejected at 5% but rejected for subsamples (see below)
mood.test(transferts_inter ~ type_transferts_inter, data=t, subset=(variante_transferts_inter!='s'), weights=t$weight, alternative="t") # equality rejected
mood.test(transferts_inter ~ type_transferts_inter, data=t, subset=(variante_transferts_inter=='s'), weights=t$weight, alternative="t") # equality rejected
mood.test(transferts_inter ~ (variante_transferts_inter=='s'), data=t, weights=t$weight, alternative="t") # equality rejected
mood.test(transferts_inter ~ (variante_transferts_inter=='c'), data=t, subset=(variante_transferts_inter!='i'), weights=t$weight, alternative="t") # equality rejected
mood.test(transferts_inter ~ (variante_transferts_inter=='c'), data=t, subset=(variante_transferts_inter!='s'), weights=t$weight, alternative="t") # equality can be rejected between c and i


# Transfers believed: 51/16/33% of transferts > = < actuel cru
sum(t$weight[t$transferts_inter_actuel<=1 & t$transferts_inter_actuel!=-1], na.rm=T)/sum(t$weight[!is.na(t$transferts_inter_actuel) & t$transferts_inter_actuel!=-1]) # 13% think current <= 1%
sum(t$weight[t$transferts_inter_actuel<=0.4 & t$transferts_inter_actuel!=-1], na.rm=T)/sum(t$weight[!is.na(t$transferts_inter_actuel) & t$transferts_inter_actuel!=-1]) # 3% think current <= 0.4%
sum(f$weight[f$transferts_inter>f$transferts_inter_actuel & !is.missing(f$transferts_inter) & !is.missing(f$transferts_inter_actuel)])/sum(f$weight[!is.missing(f$transferts_inter) & !is.missing(f$transferts_inter_actuel)])
sum(f$weight[f$transferts_inter==f$transferts_inter_actuel & !is.missing(f$transferts_inter) & !is.missing(f$transferts_inter_actuel)])/sum(f$weight[!is.missing(f$transferts_inter) & !is.missing(f$transferts_inter_actuel)])
sum(f$weight[f$transferts_inter<f$transferts_inter_actuel & !is.missing(f$transferts_inter) & !is.missing(f$transferts_inter_actuel)])/sum(f$weight[!is.missing(f$transferts_inter) & !is.missing(f$transferts_inter_actuel)])
summary(lm(pmin(transferts_inter, 10) ~ transferts_inter_actuel, data=t, subset=(!is.na(transferts_inter) & transferts_inter!=-1), weights = t$weight))
summary(lm(pmin(transferts_inter, 10) ~ transferts_inter_actuel + transferts_inter_info, data=t, subset=(!is.na(transferts_inter) & transferts_inter!=-1), weights = t$weight))
CImedian(t$transferts_inter[t$transferts_inter_actuel_vu==T])
CImedian(t$transferts_inter[t$transferts_inter_actuel_vu==T & is.missing(t$transferts_inter_actuel)])
CImedian(t$transferts_inter[t$transferts_inter_actuel_vu==T & !is.missing(t$transferts_inter_actuel)])
CImedian(t$transferts_inter[t$transferts_inter_actuel_vu==T & t$transferts_inter_info==FALSE])

decrit(t$transferts_inter_a, weight=t$weight)
ci("transferts_inter_a", d=t)
ci("transferts_inter_a", d=t)
binconf(114, 200)
sum(t$weight[t$transferts_inter>=5 & !is.na(t$transferts_inter)])/(sum(t$weight[!is.na(t$transferts_inter)])+246) # 40% >=5%
sum(t$weight[t$transferts_inter>=5 & !is.na(t$transferts_inter)])/sum(t$weight[!is.na(t$transferts_inter) & t$transferts_inter!=-1]) # 53% >=5% excluding PNR
(246+sum(t$weight[t$transferts_inter==-1 & !is.na(t$transferts_inter)]))/(246+sum(t$weight[!is.na(t$transferts_inter)])) # 25% PNR
sum(t$weight[t$transferts_inter>5 & !is.na(t$transferts_inter)])/(sum(t$weight[!is.na(t$transferts_inter)])+246) # 37% >5%
sum(t$weight[t$transferts_inter>5 & !is.na(t$transferts_inter)])/sum(t$weight[!is.na(t$transferts_inter) & t$transferts_inter!=-1]) # 45% >5% excluding PNR

# Effect of displaying info (-2% ***), second regression excludes transfer > 10% TODO: quantile regression
summary(lm(transferts_inter ~ transferts_inter_info, data=t, weights = t$weight, subset=t$transferts_inter!=-1))
summary(lm(transferts_inter ~ transferts_inter_info, data=t, weights = t$weight, subset=(t$transferts_inter!=-1 & t$transferts_inter<=10)))
summary(lm(pmin(transferts_inter, 10) ~ transferts_inter_info, data=t, weights = t$weight, subset=(t$transferts_inter!=-1)))

# Effect of priming
summary(lm(pmin(transferts_inter, 10) ~ transferts_inter_info + transferts_inter_actuel_vu + transferts_inter_actuel_vu*transferts_inter_info, data=t, weights = t$weight, subset=(t$transferts_inter!=-1)))
summary(rq(transferts_inter ~ transferts_inter_info + transferts_inter_actuel_vu + transferts_inter_actuel_vu*transferts_inter_info, data=t, weights = t$weight, subset=(t$transferts_inter!=-1)), se='nid')
coef(rq(transferts_inter ~ transferts_inter_info + transferts_inter_actuel_vu + transferts_inter_actuel_vu*transferts_inter_info, data=t, weights = t$weight, subset=(t$transferts_inter!=-1), tau = c(0.17, 0.33, 0.5, 0.67, 0.83)))
summary(rq(transferts_inter ~ transferts_inter_info + transferts_inter_actuel_vu + transferts_inter_actuel_vu*transferts_inter_info, data=t, weights = t$weight, subset=(t$transferts_inter!=-1), tau = c(0.17, 0.33, 0.5, 0.67, 0.83)), se="boot")
plot(summary(rq(transferts_inter ~ transferts_inter_info + transferts_inter_actuel_vu + transferts_inter_actuel_vu*transferts_inter_info, data=t, weights = t$weight, subset=(t$transferts_inter!=-1), tau = c(0.17, 0.33, 0.5, 0.67, 0.83)), se="boot"), parm='transferts_inter_info')

# Effect of phrasing and type
# t$variante_transferts_inter <- relevel(as.factor(t$variante_transferts_inter), ref = "s")
summary(lm(pmin(transferts_inter, 10) ~ variante_transferts_inter + type_transferts_inter, data=t, weights = t$weight, subset=(t$transferts_inter!=-1)))
summary(lm(pmin(transferts_inter, 10) ~ variante_transferts_inter + type_transferts_inter + type_transferts_inter*(variante_transferts_inter=='s'), data=t, weights = t$weight, subset=(t$transferts_inter!=-1)))

# Effect of projection of other's preferences: -1% 
summary(lm(pmin(Transferts_inter, 10) ~ cru*(Diplome >= 4 & revenu >= 2000), data=t2, subset=!is.missing(Transferts_inter), weights = t2$weight)) # . -0.9 only interaction that works: seems to be driven by less higher values
summary(rq(pmin(Transferts_inter, 10) ~ cru*(Diplome >= 4 & revenu >= 2000), data=t2, subset=!is.missing(Transferts_inter), weights = t2$weight), se='boot') # se = 'nid
plot(summary(rq(pmin(Transferts_inter, 10) ~ cru*(Diplome >= 4 & revenu >= 2000), data=t2, subset=!is.missing(Transferts_inter), weights = t2$weight, tau = c(0.17, 0.33, 0.5, 0.67, 0.83)), se='boot'), parm='cruTRUE:Diplome >= 4 & revenu >= 2000TRUE') 
decrit(t$transferts_inter_cru)
decrit(t$transferts_inter[t$transferts_inter!=-1])
mood.test(Transferts_inter ~ cru, data=t2, weights= t2$weight) # equality of median rejected at 6% threshold
CImedian(t$transferts_inter_cru[t$revenu>=2000]) # 1.8 2 2.3
CImedian(t$transferts_inter_cru[t$Diplome >= 4]) # 1.5 2 2.1

decrit(t$transferts_inter_cru[t$revenu>=2000], weights=t$weight[t$revenu>=2000]) # 2.3%
decrit(t$transferts_inter_cru[t$Diplome >= 6], weights=t$weight[t$Diplome >= 6]) # 2% be it Diplome >= 4, 5 or 6 (Bac +2, +3 or +5)
decrit(t$transferts_inter_cru[t$Diplome >= 6 & t$revenu >= 4000], weights=t$weight[t$Diplome >= 6 & t$revenu >= 4000]) # 2%
mood.test(transferts_inter_cru ~ revenu>=2000, data=t, weights=t$weight) # equality rejected
summary(lm(pmin(Transferts_inter, 10) ~ cru, data=t2, subset=!is.na(Transferts_inter), weights = t2$weight)) # 0
summary(lm(transferts_inter_cru ~ revenu, data=t, subset=!is.missing(t$transferts_inter_cru), weights = t$weight)) # revenu *** -0.0015 exlained by lower desired (cf. below)
summary(lm(transferts_inter_cru ~ revenu*transferts_inter, data=t, subset=!is.missing(t$transferts_inter_cru), weights = t$weight))
summary(lm(transferts_inter_cru ~ revenu, data=t, subset=!is.missing(t$transferts_inter_cru) & t$transferts_inter_info==T, weights = t$weight)) # The effect that perceived is lower for higher income does not seem to be explained by rich thinking that other underestimate aid, because  effect persists when info is displayed (same thing for Diplome)
summary(lm(transferts_inter_cru ~ revenu*transferts_inter, data=t, subset=!is.missing(t$transferts_inter_cru) & t$transferts_inter_info==T, weights = t$weight))
summary(lm(pmin(Transferts_inter, 10) ~ cru, data=t2, subset=!is.missing(Transferts_inter) & revenu>=2000, weights = t2$weight)) # . -0.56 interactions don't work
summary(lm(pmin(Transferts_inter, 10) ~ cru, data=t2, subset=!is.missing(Transferts_inter) & Diplome >= 5, weights = t2$weight)) # . -0.65 interactions don't work

sd(t$transferts_inter_cru[!is.na(t$transferts_inter_cru) & t$transferts_inter_cru!=-1])
sd(t$transferts_inter[!is.na(t$transferts_inter) & t$transferts_inter!=-1])
CImedian(t$transferts_inter_cru)
# 48/17/35% of transferts > = < cru
sum(f$weight[f$transferts_inter>f$transferts_inter_cru & !is.missing(f$transferts_inter) & !is.missing(f$transferts_inter_cru)])/sum(f$weight[!is.missing(f$transferts_inter) & !is.missing(f$transferts_inter_cru)])
sum(f$weight[f$transferts_inter==f$transferts_inter_cru & !is.missing(f$transferts_inter) & !is.missing(f$transferts_inter_cru)])/sum(f$weight[!is.missing(f$transferts_inter) & !is.missing(f$transferts_inter_cru)])
sum(f$weight[f$transferts_inter<f$transferts_inter_cru & !is.missing(f$transferts_inter) & !is.missing(f$transferts_inter_cru)])/sum(f$weight[!is.missing(f$transferts_inter) & !is.missing(f$transferts_inter_cru)])
# among superior: 59/17/24
sum(f$weight[f$revenu>=2000 & f$Diplome >= 4 & !is.na(f$revenu)])/sum(f$weight[!is.na(f$revenu)]) # 16% of superior
sum(f$weight[f$revenu>=2000 & f$Diplome >= 4 & !is.na(f$revenu) & f$transferts_inter>f$transferts_inter_cru & !is.missing(f$transferts_inter) & !is.missing(f$transferts_inter_cru)])/sum(f$weight[f$revenu>=2000 & f$Diplome >= 4 & !is.na(f$revenu) & !is.missing(f$transferts_inter) & !is.missing(f$transferts_inter_cru)])
sum(f$weight[f$revenu>=2000 & f$Diplome >= 4 & !is.na(f$revenu) & f$transferts_inter==f$transferts_inter_cru & !is.missing(f$transferts_inter) & !is.missing(f$transferts_inter_cru)])/sum(f$weight[f$revenu>=2000 & f$Diplome >= 4 & !is.na(f$revenu) & !is.missing(f$transferts_inter) & !is.missing(f$transferts_inter_cru)])
sum(f$weight[f$revenu>=2000 & f$Diplome >= 4 & !is.na(f$revenu) & f$transferts_inter<f$transferts_inter_cru & !is.missing(f$transferts_inter) & !is.missing(f$transferts_inter_cru)])/sum(f$weight[f$revenu>=2000 & f$Diplome >= 4 & !is.na(f$revenu) & !is.missing(f$transferts_inter) & !is.missing(f$transferts_inter_cru)])
# length(which(f$transferts_inter>f$transferts_inter_cru & !is.missing(f$transferts_inter) & !is.missing(f$transferts_inter_cru)))/length(which(!is.missing(f$transferts_inter) & !is.missing(f$transferts_inter_cru)))
# length(which(f$transferts_inter==f$transferts_inter_cru & !is.missing(f$transferts_inter) & !is.missing(f$transferts_inter_cru)))/length(which(!is.missing(f$transferts_inter) & !is.missing(f$transferts_inter_cru)))
# length(which(f$transferts_inter<f$transferts_inter_cru & !is.missing(f$transferts_inter) & !is.missing(f$transferts_inter_cru)))/length(which(!is.missing(f$transferts_inter) & !is.missing(f$transferts_inter_cru)))
t_2 <- t_1 <- t[,c("transferts_inter", "transferts_inter_actuel_vu", "transferts_inter_info", "transferts_inter_cru", "revenu", "Diplome", "diplome4", "age", "Age", "sexe", "revdisp", "rev_tot", "gauche_droite", "vague")]
t_2$Transferts_inter <- NA
t_2$Transferts_inter[!is.missing(t_2$transferts_inter_cru) & !is.na(t_2$transferts_inter) & t_2$transferts_inter!=-1] <- t_2$transferts_inter[!is.missing(t_2$transferts_inter_cru) & !is.na(t_2$transferts_inter) & t_2$transferts_inter!=-1]
t_2$cru <- FALSE
t_1$Transferts_inter[!is.missing(t_1$transferts_inter_cru) & !is.na(t_1$transferts_inter) & t_1$transferts_inter!=-1] <- t_1$transferts_inter_cru[!is.missing(t_1$transferts_inter_cru) & !is.na(t_1$transferts_inter) & t_1$transferts_inter!=-1]
t_1$cru <- T
t2 <- rbind(t_1, t_2)
rm(t_1, t_2)
summary(lm(Transferts_inter ~ cru, data=t2, subset=!is.missing(Transferts_inter))) # * TODO: check NSP, quantile regression
summary(lm(Transferts_inter ~ cru, data=t2, subset=!is.missing(Transferts_inter), weights = t2$weight)) # 0
summary(lm(Transferts_inter ~ cru, data=t2, subset=!is.na(Transferts_inter) & t2$transferts_inter_info==FALSE, weights = t2$weight))
summary(lm(Transferts_inter ~ cru, data=t2, subset=!is.na(Transferts_inter) & t2$transferts_inter_info==FALSE & t2$transferts_inter_actuel_vu==FALSE, weights = t2$weight))
summary(lm(Transferts_inter ~ cru + transferts_inter_info + transferts_inter_actuel_vu, data=t2, subset=!is.na(Transferts_inter), weights = t2$weight))
summary(lm(pmin(Transferts_inter, 10) ~ cru + transferts_inter_info + transferts_inter_actuel_vu, data=t2, subset=!is.na(Transferts_inter), weights = t2$weight))
summary(lm(pmin(Transferts_inter, 10) ~ cru, data=t2, subset=!is.na(Transferts_inter), weights = t2$weight)) # -0.35 ** (p=0.08)
summary(lm(pmin(Transferts_inter, 10) ~ cru + transferts_inter_info + transferts_inter_actuel_vu, data=t2, subset=!is.na(Transferts_inter) & t2$revenu >= 2000, weights = t2$weight))
summary(lm(pmin(Transferts_inter, 10) ~ cru, data=t2, subset=!is.na(Transferts_inter) & t2$transferts_inter_info==FALSE, weights = t2$weight))
summary(lm(pmin(Transferts_inter, 10) ~ cru, data=t2, subset=!is.na(Transferts_inter) & t2$transferts_inter_info==FALSE & t2$transferts_inter_actuel_vu==FALSE, weights = t2$weight))
# Results appear when recode min = 10 (hence, transferts_inter < 10 drive the results?) => quantile regression
decrit(t2$transferts_inter_info[!is.na(t2$Transferts_inter)])
decrit(t2$transferts_inter_info[!is.na(t2$Transferts_inter) & t2$cru==T])
decrit(t2$transferts_inter_actuel_vu[!is.na(t2$Transferts_inter)])
decrit(t2$transferts_inter_actuel_vu[!is.na(t2$Transferts_inter) & t2$cru==T])

# Projection of other's
summary(lm(pmin(transferts_inter, 10) ~ pmin(transferts_inter_cru, 10), data=t, weights = t$weight, subset=(t$transferts_inter!=-1 & t$transferts_inter_cru!=-1)))
summary(lm(transferts_inter ~ transferts_inter_cru, data=t, weights = t$weight, subset=(t$transferts_inter!=-1 & t$transferts_inter_cru!=-1)))
summary(lm(transferts_inter ~ transferts_inter_cru, data=t, weights = t$weight, subset=(t$transferts_inter!=-1 & t$transferts_inter<=10 & t$transferts_inter_cru!=-1)))
summary(lm(transferts_inter ~ transferts_inter_cru, data=t, weights = t$weight, subset=(t$transferts_inter!=-1 & t$transferts_inter<=10 & t$transferts_inter_cru<=10 & t$transferts_inter_cru!=-1)))
summary(lm(transferts_inter ~ transferts_inter_cru, data=t, weights = t$weight, subset=(t$transferts_inter!=-1 & t$transferts_inter<=20 & t$transferts_inter_cru<=20 & t$transferts_inter_cru!=-1)))
summary(lm(transferts_inter ~ transferts_inter_cru + transferts_inter_info + transferts_inter_actuel_vu, data=t, weights = t$weight, subset=(t$transferts_inter!=-1 & t$transferts_inter<=20 & t$transferts_inter_cru<=20 & t$transferts_inter_cru!=-1)))
summary(lm(transferts_inter ~ transferts_inter_cru, data=t, weights = t$weight, subset=(t$transferts_inter!=-1 & t$transferts_inter<=20 & t$transferts_inter_cru<=20 & t$transferts_inter_cru!=-1)))
summary(lm(transferts_inter - transferts_inter_cru ~ transferts_inter_cru, data=t, weights = t$weight, subset=(t$transferts_inter!=-1 & t$transferts_inter_cru!=-1)))
summary(lm(transferts_inter - transferts_inter_cru ~ transferts_inter_cru, data=t, weights = t$weight, subset=(t$transferts_inter!=-1 & t$transferts_inter<=10 & t$transferts_inter_cru!=-1)))

# Perception of current
decrit(t$transferts_inter[t$transferts_inter_actuel <= 1 & t$transferts_inter!=-1], weights = t$weight[t$transferts_inter_actuel <= 1 & t$transferts_inter!=-1]) # 2
CImedian(t$transferts_inter[t$transferts_inter_actuel <= 1 & t$transferts_inter!=-1])
summary(lm(transferts_inter ~ transferts_inter_actuel, data=t, weights = t$weight, subset=(t$transferts_inter!=-1 & t$transferts_inter<=20 & t$transferts_inter_actuel<=20 & t$transferts_inter_actuel!=-1)))
summary(lm(transferts_inter ~ transferts_inter_actuel, data=t, weights = t$weight, subset=(t$transferts_inter!=-1 & t$transferts_inter<=20 & t$transferts_inter_actuel<=20 & t$transferts_inter_actuel!=-1 & t$transferts_inter_info==FALSE)))
plot(summary(rq(transferts_inter ~ transferts_inter_actuel, data=t, weights = t$weight, subset=(t$transferts_inter!=-1 & t$transferts_inter<=20 & t$transferts_inter_actuel<=20 & t$transferts_inter_actuel!=-1), tau = c(0.17, 0.33, 0.5, 0.67, 0.83))), parm='transferts_inter_actuel')
plot(summary(rq(transferts_inter ~ transferts_inter_actuel, data=t, weights = t$weight, subset=(t$transferts_inter!=-1 & t$transferts_inter<=20 & t$transferts_inter_actuel<=20 & t$transferts_inter_actuel!=-1 & t$transferts_inter_info==FALSE), tau = c(0.17, 0.33, 0.5, 0.67, 0.83))), parm='transferts_inter_actuel')
summary(lm(pmin(transferts_inter, 10) ~ transferts_inter_actuel + pmin(revenu, 10^4) + statut_emploi + diplome4, subset = (transferts_inter!=-1 & sexe!='Autre'), data=t, weights = t$weight)) # 
summary(lm(pmin(transferts_inter, 10) ~ transferts_inter_actuel + pmin(revenu, 10^4) + statut_emploi + diplome4 + Gauche_droite, subset = (transferts_inter!=-1 & sexe!='Autre'), data=t, weights = t$weight)) # 
summary(lm(transferts_inter ~ transferts_inter_actuel + transferts_inter_info, data=t, weights = t$weight, subset=(t$transferts_inter!=-1 & t$transferts_inter<=20 & t$transferts_inter_actuel<=20 & t$transferts_inter_actuel!=-1)))

summary(lm(transferts_inter ~ transferts_inter_actuel, data=t, weights = t$weight, subset=(t$transferts_inter!=-1 & t$transferts_inter_actuel!=-1)))
summary(lm(transferts_inter ~ transferts_inter_actuel, data=t, weights = t$weight, subset=(t$transferts_inter!=-1 & t$transferts_inter<=10 & t$transferts_inter_actuel!=-1)))
summary(lm(transferts_inter ~ transferts_inter_actuel, data=t, weights = t$weight, subset=(t$transferts_inter!=-1 & t$transferts_inter<=10 & t$transferts_inter_actuel<=10 & t$transferts_inter_actuel!=-1)))
summary(lm(transferts_inter ~ transferts_inter_actuel + transferts_inter_info + transferts_inter_actuel_vu, data=t, weights = t$weight, subset=(t$transferts_inter!=-1 & t$transferts_inter<=20 & t$transferts_inter_actuel<=20 & t$transferts_inter_actuel!=-1)))
summary(lm(transferts_inter ~ transferts_inter_actuel, data=t, weights = t$weight, subset=(t$transferts_inter!=-1 & t$transferts_inter<=20 & t$transferts_inter_actuel<=20 & t$transferts_inter_actuel!=-1)))
summary(lm(pmin(transferts_inter, 10) ~ pmin(transferts_inter_actuel, 10), data=t, weights = t$weight, subset=(t$transferts_inter!=-1 & t$transferts_inter_actuel!=-1)))
summary(lm(transferts_inter - transferts_inter_actuel ~ transferts_inter_actuel, data=t, weights = t$weight, subset=(t$transferts_inter!=-1 & t$transferts_inter_actuel!=-1)))
summary(lm(transferts_inter - transferts_inter_actuel ~ transferts_inter_actuel, data=t, weights = t$weight, subset=(t$transferts_inter!=-1 & t$transferts_inter<=10 & t$transferts_inter_actuel!=-1)))

decrit(t$transferts_inter[t$transferts_inter!=-1])
decrit(t$transferts_inter[t$transferts_inter_info==T & t$transferts_inter!=-1])
decrit(t$transferts_inter[t$transferts_inter_info==FALSE & t$transferts_inter!=-1])
decrit(t$transferts_inter[t$transferts_inter_actuel_vu==T & t$transferts_inter!=-1])
decrit(t$transferts_inter[t$transferts_inter_actuel_vu==FALSE & t$transferts_inter!=-1])
decrit(t$transferts_inter[t$transferts_inter_actuel_vu==T & t$transferts_inter_info==T & t$transferts_inter!=-1])
decrit(t$transferts_inter[t$transferts_inter_actuel_vu==T & t$transferts_inter_info==FALSE & t$transferts_inter!=-1])
decrit(t$transferts_inter[t$transferts_inter_actuel_vu==FALSE & t$transferts_inter_info==FALSE & t$transferts_inter!=-1])
decrit(t$transferts_inter[t$transferts_inter_actuel_vu==FALSE & t$transferts_inter_info==T & t$transferts_inter!=-1])
t$variante_transferts_inter <- relevel(as.factor(t$variante_transferts_inter), ref = "s")

# Big regression:
summary(lm(transferts_inter ~ variante_transferts_inter + type_transferts_inter + transferts_inter_info + transferts_inter_actuel_vu + transferts_inter_info*transferts_inter_actuel_vu, data=t, weights = t$weight, subset=(t$transferts_inter!=-1)))
summary(lm(transferts_inter ~ variante_transferts_inter + transferts_inter_info + transferts_inter_actuel_vu + transferts_inter_info*transferts_inter_actuel_vu, data=t, weights = t$weight, subset=(t$transferts_inter!=-1)))
summary(lm(log(transferts_inter+0.000001) ~ variante_transferts_inter + transferts_inter_info + transferts_inter_actuel_vu + transferts_inter_info*transferts_inter_actuel_vu, data=t, weights = t$weight, subset=(t$transferts_inter!=-1)))
summary(lm(pmin(transferts_inter, 10) ~ variante_transferts_inter + type_transferts_inter + transferts_inter_info + transferts_inter_actuel_vu + transferts_inter_info*transferts_inter_actuel_vu, data=t, weights = t$weight, subset=(t$transferts_inter!=-1)))
summary(lm(transferts_inter ~ variante_transferts_inter + type_transferts_inter + transferts_inter_info + transferts_inter_actuel_vu, data=t, weights = t$weight, subset=(t$transferts_inter!=-1)))
summary(lm(pmin(transferts_inter, 10) ~ variante_transferts_inter + type_transferts_inter + transferts_inter_info + transferts_inter_actuel_vu, data=t, weights = t$weight, subset=(t$transferts_inter!=-1)))
# TODO: code l in vague 1, influence on NSP, charts, confidence intervals

# socio-demo determinants: -1% pour Hommes, - revenu mais expliqué par perception et éducation
summary(lm(pmin(transferts_inter, 10) ~ pmin(revenu, 10^4), subset = (transferts_inter!=-1), data=t, weights = t$weight)) # -1.4e-4**, explained by perception (cf. below)
summary(lm(pmin(transferts_inter, 10) ~ transferts_inter_actuel + pmin(revenu, 10^4), subset = (transferts_inter!=-1), data=t, weights = t$weight)) # revenu: 0
decrit(t$transferts_inter[t$revenu > 4000 & t$transferts_inter!=-1]) # robust to threshold
decrit(t$transferts_inter[t$revenu < 1500 & t$transferts_inter!=-1]) # robust to threshold
summary(lm(pmin(transferts_inter, 10) ~ pmin(revenu, 10^4) + sexe + age + csp + statut_emploi + region + diplome4 + as.factor(gauche_droite), subset = (transferts_inter!=-1), data=t, weights = t$weight)) # 
summary(lm(pmin(transferts_inter, 10) ~ pmin(revenu, 10^4) + sexe + age + csp + statut_emploi + region + diplome4, subset = (transferts_inter!=-1), data=t, weights = t$weight)) # 
summary(lm(pmin(transferts_inter, 10) ~ pmin(revenu, 10^4) + sexe + age + csp + statut_emploi + region + diplome4, subset = (transferts_inter!=-1), data=t, weights = t$weight)) # 
summary(lm(pmin(transferts_inter, 10) ~ pmin(revenu, 10^4) + sexe + statut_emploi + diplome4, subset = (transferts_inter!=-1 & sexe!='Autre'), data=t, weights = t$weight)) # revenu explained by education=supérieure
summary(lm(pmin(transferts_inter, 10) ~ sexe + statut_emploi + diplome4, subset = (transferts_inter!=-1 & sexe!='Autre'), data=t, weights = t$weight)) # revenu explained by education=supérieure
summary(lm(pmin(transferts_inter, 10) ~ sexe + diplome4, subset = (transferts_inter!=-1 & sexe!='Autre'), data=t, weights = t$weight)) # revenu explained by education=supérieure
summary(lm(pmin(transferts_inter, 10) ~ transferts_inter_actuel + pmin(revenu, 10^4) + sexe + statut_emploi + diplome4, subset = (transferts_inter!=-1 & sexe!='Autre'), data=t, weights = t$weight)) # 
summary(lm(pmin(transferts_inter, 10) ~ transferts_inter_actuel + pmin(revenu, 10^4) + statut_emploi + diplome4, subset = (transferts_inter!=-1 & sexe!='Autre'), data=t, weights = t$weight)) # 
summary(lm(transferts_inter_actuel ~ Diplome + revenu + statut_emploi + Gauche_droite, data=t, weights = t$weight, subset=(t$transferts_inter_actuel<=20 & t$transferts_inter_actuel!=-1)))
summary(lm(transferts_inter ~ Diplome + revenu + statut_emploi + Gauche_droite, data=t, weights = t$weight, subset=(t$transferts_inter_actuel<=20 & t$transferts_inter_actuel!=-1)))


# relation with variation_aide
decrit(t$variation_aide)
CImedian(t$variation_aide)
decrit(t$transferts_inter[!is.na(t$variation_aide) & t$transferts_inter!=-1]) # 3 ! Les gens qui ne répondent pas à variation_aide veulent plus d'aide
decrit(t$transferts_inter[is.na(t$variation_aide) & t$transferts_inter!=-1 & t$vague==3]) # 5
summary(lm(transferts_inter ~ is.na(variation_aide), data=t, weights = t$weight, subset=vague==3)) # 0
summary(lm(pmin(transferts_inter, 10) ~ is.na(variation_aide), data=t, weights = t$weight, subset=vague==3)) # -1***
summary(lm(pmin(transferts_inter, 10) ~ is.na(variation_aide) + transferts_inter_info + transferts_inter_actuel_vu, data=t, weights = t$weight, subset=vague==3))
summary(lm(variation_aide ~ transferts_inter_info + transferts_inter_actuel_vu, data=t, weights = t$weight)) # 1.2*
summary(lm(variation_aide ~ transferts_inter_info + transferts_inter_actuel_vu + transferts_inter_info*transferts_inter_actuel_vu, data=t, weights = t$weight))
summary(lm(variation_aide ~ transferts_inter_info + transferts_inter_actuel_vu + I(depenses_confiant=="Assez confiant.e") + I(compris_depenses!="Non") + revenu + Gauche_droite, data=t, weights = t$weight)) # 
summary(lm(variation_aide ~ I(depenses_confiant=="Assez confiant.e"), data=t, weights = t$weight)) # 0
summary(lm(variation_aide ~ I(compris_depenses!="Non"), data=t, weights = t$weight)) # 0
decrit(t$type_transferts_inter[!is.na(t$variation_aide)]) # e
decrit(t$variante_transferts_inter[!is.na(t$variation_aide)]) # s
decrit(t$depenses_confiant) # 24% oui, 43% non
decrit(t$compris_depenses)
decrit(t$compris_depenses[t$depenses_confiant=="Assez confiant.e"]) # 6% de non
decrit(t$variation_aide[f$transferts_inter_info==T]) # median: 0 (IC: [0, 0]), mean: -2.24, 26% de 0, 40% < 0, 35% > 0
length(which(f$variation_aide<0 & f$transferts_inter_info==T))/length(which(!is.na(f$variation_aide) & f$transferts_inter_info==T))
length(which(f$variation_aide>0 & f$transferts_inter_info==T))/length(which(!is.na(f$variation_aide) & f$transferts_inter_info==T))
length(which(f$variation_aide==0 & f$transferts_inter_info==T))/length(which(!is.na(f$variation_aide) & f$transferts_inter_info==T))
# 29% inconsistent, i.e. with variation_aide<0 although they saw info and have put transferts_inter>0.4, only 13% very inconsistent (i.e. transferts_inter>2) and 6% extremely (>5)
length(which(f$variation_aide<0 & f$transferts_inter_info==T & f$transferts_inter>0.4))/length(which(!is.na(f$variation_aide) & f$transferts_inter_info==T & f$transferts_inter>0.4))
length(which(f$variation_aide<0 & f$transferts_inter_info==T & f$transferts_inter>2))/length(which(!is.na(f$variation_aide) & f$transferts_inter_info==T & f$transferts_inter>0.4))
length(which(f$variation_aide<0 & f$transferts_inter_info==T & f$transferts_inter>5))/length(which(!is.na(f$variation_aide) & f$transferts_inter_info==T & f$transferts_inter>0.4))

# plots
bars_transfert("transferts_inter", data=t, miss=T, title="Transferts internationaux désirés\nQuelle part des revenus des pays riches devrait être transférée aux pays pauvres ? (en %)", weights=T)
bars_transfert_en("transferts_inter", data=t, miss=T, title="Desired international transfers\nWhich proportion of rich countries' income should be transferred to poor countries? (in %)", weights=T)

sum(t$weight[t$transferts_inter>=5 & t$transferts_inter_info==FALSE & t$transferts_inter_actuel_vu==FALSE & !is.na(t$transferts_inter) & t$transferts_inter!=-1])/sum(t$weight[t$transferts_inter_info==FALSE & t$transferts_inter_actuel_vu==FALSE & !is.na(t$transferts_inter) & t$transferts_inter!=-1]) # >=5% no priming 60%
sum(t$weight[t$transferts_inter>5 & t$transferts_inter_info==FALSE & t$transferts_inter_actuel_vu==FALSE & !is.na(t$transferts_inter) & t$transferts_inter!=-1])/sum(t$weight[t$transferts_inter_info==FALSE & t$transferts_inter_actuel_vu==FALSE & !is.na(t$transferts_inter) & t$transferts_inter!=-1]) # >5% no priming 51%
sum(t$weight[t$transferts_inter>=5 & t$transferts_inter_info==FALSE & t$transferts_inter_actuel_vu==FALSE & !is.na(t$transferts_inter) & t$transferts_inter!=-1 & t$variante_transferts_inter=='s'])/sum(t$weight[t$transferts_inter_info==FALSE & t$transferts_inter_actuel_vu==FALSE & !is.na(t$transferts_inter) & t$transferts_inter!=-1 & t$variante_transferts_inter=='s']) # >=5% no priming 57%
sum(t$weight[t$transferts_inter>5 & t$transferts_inter_info==FALSE & t$transferts_inter_actuel_vu==FALSE & !is.na(t$transferts_inter) & t$transferts_inter!=-1 & t$variante_transferts_inter=='s'])/sum(t$weight[t$transferts_inter_info==FALSE & t$transferts_inter_actuel_vu==FALSE & !is.na(t$transferts_inter) & t$transferts_inter!=-1 & t$variante_transferts_inter=='s']) # >5% no priming 48%
sum(t$weight[t$transferts_inter>=5 & t$transferts_inter_info==FALSE & t$transferts_inter_actuel_vu==FALSE & !is.na(t$transferts_inter) & t$transferts_inter!=-1])/sum(t$weight[t$transferts_inter_info==FALSE & t$transferts_inter_actuel_vu==FALSE]) # >=5% no priming, incl. PNR 54%
sum(t$weight[t$transferts_inter>5 & t$transferts_inter_info==FALSE & t$transferts_inter_actuel_vu==FALSE & !is.na(t$transferts_inter) & t$transferts_inter!=-1])/sum(t$weight[t$transferts_inter_info==FALSE & t$transferts_inter_actuel_vu==FALSE & !is.na(t$transferts_inter)]) # >5% no priming, incl. PNR 46%
sum(t$weight[t$transferts_inter>=5 & t$variante_transferts_inter=='s' & !is.na(t$transferts_inter) & t$transferts_inter!=-1])/sum(t$weight[t$variante_transferts_inter=='s' & !is.na(t$transferts_inter) & t$transferts_inter!=-1]) # >=5% simple 47%
sum(t$weight[t$transferts_inter>5 & t$variante_transferts_inter=='s' & !is.na(t$transferts_inter) & t$transferts_inter!=-1])/sum(t$weight[t$variante_transferts_inter=='s' & !is.na(t$transferts_inter) & t$transferts_inter!=-1]) # >5% simple 39%
sum(t$weight[t$transferts_inter>t$transferts_inter_actuel & !is.na(t$transferts_inter_actuel) & t$transferts_inter_actuel!=-1 & !is.na(t$transferts_inter) & t$transferts_inter!=-1 & t$transferts_inter_info==FALSE])/sum(t$weight[t$transferts_inter_actuel!=-1 & !is.na(t$transferts_inter) & t$transferts_inter!=-1 & !is.na(t$transferts_inter_actuel) & t$transferts_inter_info==FALSE]) # preferred > perceived (Info=False): 66%
sum(t$weight[t$transferts_inter>=t$transferts_inter_actuel & !is.na(t$transferts_inter_actuel) & t$transferts_inter_actuel!=-1 & !is.na(t$transferts_inter) & t$transferts_inter!=-1 & t$transferts_inter_info==FALSE])/sum(t$weight[t$transferts_inter_actuel!=-1 & !is.na(t$transferts_inter) & t$transferts_inter!=-1 & !is.na(t$transferts_inter_actuel) & t$transferts_inter_info==FALSE]) # preferred >= perceived (Info=False): 73%
cdf_transferts_inter <- wtd.Ecdf(t$transferts_inter[t$transferts_inter_info==FALSE & t$transferts_inter_actuel_vu==FALSE & !is.na(t$transferts_inter) & t$transferts_inter!=-1], weights=t$weight[t$transferts_inter_info==FALSE & t$transferts_inter_actuel_vu==FALSE & !is.na(t$transferts_inter) & t$transferts_inter!=-1])
# TODO: préciser (au moins dans nom de fichier) que _s ne concernent pas tous les _s, mais seulement une variante spécifique
cdf_transferts_inter_s <- wtd.Ecdf(t$transferts_inter[t$variante_transferts_inter=="s" & t$transferts_inter_info==FALSE & t$transferts_inter_actuel_vu==FALSE & !is.na(t$transferts_inter) & t$transferts_inter!=-1], weights=t$weight[t$variante_transferts_inter=="s" & t$transferts_inter_info==FALSE & t$transferts_inter_actuel_vu==FALSE & !is.na(t$transferts_inter) & t$transferts_inter!=-1])
cdf_transferts_inter_all <- wtd.Ecdf(t$transferts_inter[!is.na(t$transferts_inter) & t$transferts_inter!=-1], weights=m$weight[!is.na(t$transferts_inter) & t$transferts_inter!=-1])
cdf_transferts_inter_diff <- wtd.Ecdf(t$transferts_inter[t$transferts_inter_info==FALSE & t$transferts_inter_actuel_vu==T & !is.na(t$transferts_inter) & t$transferts_inter!=-1 & t$transferts_inter_actuel!=-1] - t$transferts_inter_actuel[t$transferts_inter_info==FALSE & t$transferts_inter_actuel_vu==T & !is.na(t$transferts_inter) & t$transferts_inter!=-1 & t$transferts_inter_actuel!=-1], weights=t$weight[t$transferts_inter_info==FALSE & t$transferts_inter_actuel_vu==T & !is.na(t$transferts_inter) & t$transferts_inter!=-1 & t$transferts_inter_actuel!=-1])
cdf_transferts_inter_diff_info <- wtd.Ecdf(t$transferts_inter[t$transferts_inter_info==T & t$transferts_inter_actuel_vu==T & !is.na(t$transferts_inter) & t$transferts_inter!=-1 & t$transferts_inter_actuel!=-1] - t$transferts_inter_actuel[t$transferts_inter_info==T & t$transferts_inter_actuel_vu==T & !is.na(t$transferts_inter) & t$transferts_inter!=-1 & t$transferts_inter_actuel!=-1], weights=t$weight[t$transferts_inter_info==T & t$transferts_inter_actuel_vu==T & !is.na(t$transferts_inter) & t$transferts_inter!=-1 & t$transferts_inter_actuel!=-1])
before_par <- par()
par(mar=c(5.1, 4.1, 1.1, 2.1))
# plot(cdf_transferts_inter$x, 1-as.vector(cdf_transferts_inter$ecdf), mgp=c(3,0.6,0), yaxt='n', lwd=2, col='blue', xlim=c(0,22), type="l", main="Transferts internationaux désirés\nQuelle part des revenus des pays riches devrait être transféré aux pays pauvres ? (en %)", xlab="", ylab="Répartition des réponses exprimées (en %)") + grid()
# mtext(do.call(expression, list(bquote(bold("Pourcentage des revenus des pays riches devant être transféré aux pays pauvres")), bquote(paste("Reading : ", italic("28% (resp. 42%) des français qui se sont exprimés pensent que strictement plus de 10% (resp. 10% ou plus)"))),bquote(italic("des revenus des pays riches devraient être transférés aux pays pauvres")))),side=1,line=2:4)
plot(cdf_transferts_inter$x, 1-as.vector(cdf_transferts_inter$ecdf), mgp=c(3,0.6,0), yaxt='n', lwd=2, col='blue', xlim=c(0,22), type="s", main="Desired international transfers\nWhat share of income of rich countries should be transferred to poor countries? (in %)", xlab="", ylab="Distribution of expresed answers (in %)") + grid()
plot(cdf_transferts_inter_s$x, 1-as.vector(cdf_transferts_inter_s$ecdf), mgp=c(3,0.6,0), yaxt='n', lwd=2, col='blue', xlim=c(0,22), type="s", main="Desired international transfers\nWhat share of income of rich countries should be transferred to poor countries? (in %)", xlab="", ylab="Distribution of expresed answers (in %)") + grid()
plot(cdf_transferts_inter_diff$x, 1-as.vector(cdf_transferts_inter_diff$ecdf), mgp=c(3,0.6,0), yaxt='n', lwd=2, col='blue', xlim=c(-10,10), type="s", main="", xlab="", ylab="Distribution of expresed answers (in %)") + grid()
lines(cdf_transferts_inter_diff_info$x, 1-as.vector(cdf_transferts_inter_diff_info$ecdf), mgp=c(3,0.6,0), yaxt='n', lwd=2, col='orange', xlim=c(-10,10), type="s", main="", xlab="", ylab="")  # Distribution of expresed answers (in %)
mtext(do.call(expression, list(bquote(bold("(Difference) Preferred – Perceived transfer (in %)")), bquote(paste("e.g. ", italic("66% (resp. 73%) of French people who responded and did not see the Info"))),bquote(italic("would prefer a higher (resp. at least as much) development aid as they perceive")))),side=1,line=2:4)
# mtext(do.call(expression, list(bquote(bold("(Difference) Preferred – Perceived transfer (in %), when Info is not shown")), bquote(paste("e.g. ", italic("66% (resp. 73%) of French people who responded and did not see the Info"))),bquote(italic("would prefer a higher (resp. at least as much) development aid as they perceive")))),side=1,line=2:4)
# mtext(do.call(expression, list(bquote(bold("(Difference) Preferred – Perceived transfer (in %), when Info is shown")), bquote(paste("e.g. ", italic("33% (resp. 45%) of French people who responded and saw the Info"))),bquote(italic("would prefer a higher (resp. at least as much) development aid as they perceive")))),side=1,line=2:4)
# lines(cdf_transferts_inter_all$x, 1-as.vector(cdf_transferts_inter_all$ecdf), lwd=2, lty=2, col='red', type="s")
# mtext(do.call(expression, list(bquote(bold("Percentage of rich countries' income that should be transferred to poor countries")), bquote(paste("e.g. ", italic("28% (resp. 42%) of French people who responded think that more than 10% (resp. 10% or more)"))),bquote(italic("of rich countries' income should be transferred to poor countries")))),side=1,line=2:4)
abline(h=seq(0,1,by=0.1), lty=3, col="grey")
# abline(v=seq(0,5,by=1), lty=3, col="grey")
axis(2, at = seq(0,1,by=0.2), labels=c(0,20,40,60,80,100))
legend("topright", lwd=2, col=c("blue", "orange"), legend = c("With Info", "Without Info"))

before_par <- par()
par(mar=c(5.1, 4.1, 1.1, 2.1))
plot(t$transferts_inter_actuel[!is.missing(t$transferts_inter_actuel) & !is.na(t$transferts_inter)]+rnorm(length(t$transferts_inter_actuel[!is.missing(t$transferts_inter_actuel) & !is.na(t$transferts_inter)]), mean=0, sd=0.15), t$transferts_inter[!is.missing(t$transferts_inter_actuel) & !is.na(t$transferts_inter)]+rnorm(length(t$transferts_inter[!is.missing(t$transferts_inter_actuel) & !is.na(t$transferts_inter)]), mean=0, sd=0.15), xlim=c(0,11), ylim=c(0,11), type='p', cex=0.5, pch=19, col='blue', xlab="Belief for current", ylab="Preferred transfer")
lines(c(0,100), c(0,100), type='l', col='black')
par(mar = before_par$mar)

plot(jitter(t$transferts_inter_cru[!is.missing(t$transferts_inter_cru) & !is.na(t$transferts_inter)], 10), jitter(t$transferts_inter[!is.missing(t$transferts_inter_cru) & !is.na(t$transferts_inter)], 10), xlim=c(0,11), ylim=c(0,11), type='p', cex=0.1, col='blue', xlab="Projection", ylab="Transfer")
# among "superior":
# plot(jitter(t$transferts_inter_cru[!is.missing(t$transferts_inter_cru) & !is.na(t$transferts_inter) & t$Diplome >= 4 & t$revenu >= 2000], 10), jitter(t$transferts_inter[!is.missing(t$transferts_inter_cru) & !is.na(t$transferts_inter) & t$Diplome >= 4 & t$revenu >= 2000], 10), xlim=c(0,11), ylim=c(0,11), type='p', cex=0.1, col='blue', xlab="Projection", ylab="Transfer")
lines(c(0,100), c(0,100), type='l', col='black')

plot(t$transferts_inter_cru[t$transferts_inter_info==FALSE & t$transferts_inter_actuel_vu==FALSE &  !is.missing(t$transferts_inter_cru) & !is.na(t$transferts_inter)]+rnorm(length(t$transferts_inter_cru[t$transferts_inter_info==FALSE & t$transferts_inter_actuel_vu==FALSE & !is.missing(t$transferts_inter_cru) & !is.na(t$transferts_inter)]), mean=0, sd=0.15), t$transferts_inter[t$transferts_inter_info==FALSE & t$transferts_inter_actuel_vu==FALSE & !is.missing(t$transferts_inter_cru) & !is.na(t$transferts_inter)]+rnorm(length(t$transferts_inter[t$transferts_inter_info==FALSE & t$transferts_inter_actuel_vu==FALSE & !is.missing(t$transferts_inter_cru) & !is.na(t$transferts_inter)]), mean=0, sd=0.15), xlim=c(0,11), ylim=c(0,11), type='p', cex=0.1, col='blue', xlab="Projection", ylab="Transfer")
lines(c(0,100), c(0,100), type='l', col='black')

median(t$transferts_inter[!is.missing(t$transferts_inter_cru) & !is.na(t$transferts_inter) & t$transferts_inter!=-1])
length(t$transferts_inter_cru[!is.missing(t$transferts_inter_cru) & !is.na(t$transferts_inter)])
decrit(t$variante_transferts_inter[!is.missing(t$transferts_inter_cru) & !is.na(t$transferts_inter)])
length(t$transferts_inter_cru[t$transferts_inter_info==FALSE & t$transferts_inter_actuel_vu==FALSE & !is.missing(t$transferts_inter_cru) & !is.na(t$transferts_inter)])
decrit(t$transferts_inter[t$vague==3 & t$transferts_inter!=-1])
decrit(t$transferts_inter_cru[t$transferts_inter!=-1])

##### Approbation #####
decrit(m$approbation_mediane, miss=T, weights = m$weight)
decrit(f$approbation_seuls, miss=T, weights = f$weight)
decrit(f$approbation_seuls, weights = f$weight)
decrit(f$approbation_seuls, miss=T)
decrit(f$approbation[f$Approbation_variante=="triple" & f$approbation_info], miss=T)
decrit(f$approbation[f$Approbation_variante=="triple" & !f$approbation_info], miss=T)
decrit(f$approbation[f$Approbation_variante=="triple" & f$approbation_info], miss=T, weights = f$weight[f$Approbation_variante=="triple" & f$approbation_info])
decrit(f$approbation[f$Approbation_variante=="triple" & !f$approbation_info], miss=T, weights = f$weight[f$Approbation_variante=="triple" & !f$approbation_info])
decrit(f$approbation[f$Approbation_variante=="triple" & f$approbation_info], weights = f$weight[f$Approbation_variante=="triple" & f$approbation_info])
decrit(f$approbation[f$Approbation_variante=="triple" & !f$approbation_info], weights = f$weight[f$Approbation_variante=="triple" & !f$approbation_info])
decrit(f$approbation, miss=T, weights = f$weight)
decrit(f$compris_approbation)
decrit(f$compris_approbation[f$Approbation_variante=="triple"])
decrit(f$compris_approbation[f$Approbation_variante=="seuls"])
decrit(f$compris_approbation[f$Approbation_variante=="triple" & f$approbation_info])
decrit(f$compris_approbation[f$Approbation_variante=="triple" & !f$approbation_info])
binconf(sum(f$weight[f$approbation=='Oui' & f$Approbation_variante=="triple" & f$approbation_info==T], na.rm=T), sum(f$weight[f$Approbation_variante=="triple" & f$approbation_info==T], na.rm=T), alpha = 0.05)
binconf(sum(f$weight[f$approbation=='Oui' & f$Approbation_variante=="triple" & f$approbation_info==FALSE], na.rm=T), sum(f$weight[f$Approbation_variante=="triple" & f$approbation_info==FALSE], na.rm=T), alpha = 0.05)
binconf(sum(f$weight[f$approbation=='Oui' & f$Approbation_variante=="seuls"], na.rm=T), sum(f$weight[f$Approbation_variante=="seuls"], na.rm=T), alpha = 0.05)
binconf(sum(f$weight[f$approbation=='Non' & f$Approbation_variante=="triple" & f$approbation_info==T], na.rm=T), sum(f$weight[f$Approbation_variante=="triple" & f$approbation_info==T], na.rm=T), alpha = 0.05)
binconf(sum(f$weight[f$approbation=='Non' & f$Approbation_variante=="triple" & f$approbation_info==FALSE], na.rm=T), sum(f$weight[f$Approbation_variante=="triple" & f$approbation_info==FALSE], na.rm=T), alpha = 0.05)
binconf(sum(f$weight[f$approbation=='Non' & f$Approbation_variante=="seuls"], na.rm=T), sum(f$weight[f$Approbation_variante=="seuls"], na.rm=T), alpha = 0.05)
binconf(sum(f$weight[f$approbation=='NSP' & f$Approbation_variante=="triple" & f$approbation_info==T], na.rm=T), sum(f$weight[f$Approbation_variante=="triple" & f$approbation_info==T], na.rm=T), alpha = 0.05)
binconf(sum(f$weight[f$approbation=='NSP' & f$Approbation_variante=="triple" & f$approbation_info==FALSE], na.rm=T), sum(f$weight[f$Approbation_variante=="triple" & f$approbation_info==FALSE], na.rm=T), alpha = 0.05)
binconf(sum(f$weight[f$approbation=='NSP' & f$Approbation_variante=="seuls"], na.rm=T), sum(f$weight[f$Approbation_variante=="seuls"], na.rm=T), alpha = 0.05)
binconf(sum(f$weight[f$approbation=='Oui' & f$Approbation_variante=="triple" & f$approbation_info==T], na.rm=T), sum(f$weight[f$Approbation_variante=="triple" & !is.missing(f$approbation) & f$approbation_info==T], na.rm=T), alpha = 0.05)
binconf(sum(f$weight[f$approbation=='Oui' & f$Approbation_variante=="triple" & f$approbation_info==FALSE], na.rm=T), sum(f$weight[f$Approbation_variante=="triple" & !is.missing(f$approbation) & f$approbation_info==FALSE], na.rm=T), alpha = 0.05)
binconf(sum(f$weight[f$approbation=='Oui' & f$Approbation_variante=="seuls"], na.rm=T), sum(f$weight[f$Approbation_variante=="seuls" & !is.missing(f$approbation)], na.rm=T), alpha = 0.05)
decrit(t$compris_approbation[t$Approbation_variante=="indiv"])
t1 <- t2 <- t[,c("weight", "duree", "sample", "Sample", "Gauche_droite", "vague", "finished", "test_qualite", "approbation_info", "Approbation_variante", "approbation", "approbation_mediane", "approbation_mediane_rdb", "approbation_moyenne", "approbation_mediane_aid", "sexe", "revenu", "revenu_conjoint", "compris_approbation", "taille_agglo", "csp", "qualite", "qualite_comprehension", "qualite_sincerite", "gauche_droite", "revenu_max", "note_actuel", "note_mediane", "note_mediane_rdb", "niveau_vie", "rev_tot", "revdisp", "taille_foyer", "age", "disadvantaged", "Diplome", "Age", "diplome")]
t1$approbation <- t1$approbation_mediane
t1$Approbation_variante <- "indiv"
t1 <- t1[!is.na(t1$approbation_mediane)&t1$approbation_mediane!="",]
t2 <- t2[!is.na(t2$approbation)&t2$approbation!="",]
t2 <- rbind(t1, t2)
rm(t1)
t2$Approbation_variante <- relevel(as.factor(t2$Approbation_variante), ref = "indiv")
decrit(t2$approbation, miss=T)
decrit(t2$approbation[t2$Approbation_variante=="indiv"], miss=T)
summary(lm((approbation=="Oui") ~ Approbation_variante, data=t2, weights = t2$weight)) # * of triple (driven by NSP)
summary(lm((approbation=="Non") ~ Approbation_variante, data=t2, weights = t2$weight)) # * of triple (driven by NSP)
summary(lm((approbation=="Non") ~ Approbation_variante, data=t2, subset=t2$approbation_info==T | t2$Approbation_variante=="indiv", weights = t2$weight))
summary(lm((approbation=="Oui") ~ Approbation_variante, data=t2, subset=t2$Approbation_variante!="triple", weights = t2$weight)) # same coef
summary(lm((approbation=="Oui") ~ Approbation_variante, data=t2, subset=!is.missing(t2$approbation), weights = t2$weight)) # . of seuls
summary(lm((approbation=="Oui") ~ Approbation_variante, data=t2, subset=!is.missing(t2$approbation) & (t2$approbation_info==T | t2$Approbation_variante=="indiv"), weights = t2$weight))
summary(lm((approbation=="NSP") ~ Approbation_variante, data=t2, weights = t2$weight)) # * triple
summary(lm((approbation=="Oui") ~ Approbation_variante + approbation_info, data=t2, weights = t2$weight)) # * triple, no effect of info, no need to include interaction because info shown only on half of triple
summary(lm((approbation=="Oui") ~ Approbation_variante + approbation_info, data=t2, subset=t2$approbation_info==T | t2$Approbation_variante=="indiv", weights = t2$weight)) # * triple, no effect of info, no need to include interaction because info shown only on half of triple
summary(lm((approbation=="Oui") ~ Approbation_variante + approbation_info, data=t2, subset=!is.missing(t2$approbation), weights = t2$weight))  # . seuls, no effect of info nor triple (half each)
summary(lm((approbation=="NSP") ~ Approbation_variante + approbation_info, data=t2, weights = t2$weight)) # * triple
summary(lm((approbation=="NSP") ~ Approbation_variante + approbation_info, data=t2, subset=t2$approbation_info==FALSE, weights = t2$weight)) # * triple
summary(lm((approbation=="NSP") ~ Approbation_variante + approbation_info, data=t2, subset=t2$approbation_info==T | t2$Approbation_variante=="indiv", weights = t2$weight)) # * triple
summary(lm((approbation=="NSP") ~ approbation_info, data=t2, subset=t2$Approbation_variante=="triple", weights = t2$weight)) # * triple
summary(lm((approbation=="Oui") ~ approbation_info, data=f, weights = f$weight)) # not significant negative
summary(lm((approbation=="Oui") ~ approbation_info, data=f, weights = f$weight, subset=!is.missing(f$approbation))) # . negative
summary(lm((approbation=="NSP") ~ approbation_info + (compris_approbation=='Oui') + approbation_info*(compris_approbation=='Oui'), data=f, weights = f$weight, subset=(f$Approbation_variante=="triple" & f$compris_approbation!='Bug')))
summary(lm((approbation=="NSP") ~ approbation_info + (compris_approbation=='Oui') + approbation_info*(compris_approbation=='Oui'), data=f, weights = f$weight, subset=(f$Approbation_variante=="triple")))
restr2_fit <- lm((approbation=="Oui") ~ Approbation_variante + I(pmin(revdisp,4500)/10^3) + gauche_droite, data=t2, subset=!is.missing(t2$approbation), weights = t2$weight)
large_fit <- lm((approbation=="Oui") ~ 0 + Approbation_variante + I(pmin(revdisp,4500)/10^3) + I(pmin(revdisp,4500)^2/10^6) + disadvantaged + gauche_droite + I(gauche_droite^2) + grepl("Oui", compris_approbation) + Age + grepl("emme", sexe) + (diplome=="CAP ou BEP"), data=t2, weights = t2$weight) 
main2_fit <- lm((approbation=="Oui") ~ Approbation_variante + disadvantaged + grepl("Oui", compris_approbation), data=t2, weights = t2$weight)
summary(restr2_fit)
summary(large_fit)
summary(main2_fit)

## Regressions for stars in Table 5
# for singles:
summary(lm((approbation=="Oui") ~ Approbation_variante, data=t2, subset=t2$Approbation_variante!='triple', weights = t2$weight)) 
summary(lm((approbation=="Non") ~ Approbation_variante, data=t2, subset=t2$Approbation_variante!='triple', weights = t2$weight)) 
summary(lm((approbation=="NSP") ~ Approbation_variante, data=t2, subset=t2$Approbation_variante!='triple', weights = t2$weight)) 
summary(lm((approbation=="Oui") ~ Approbation_variante, data=t2, subset=!is.missing(t2$approbation) & t2$Approbation_variante!='triple', weights = t2$weight)) 
# for triple without info:
summary(lm((approbation=="Oui") ~ Approbation_variante, data=t2, subset=(t2$Approbation_variante=='indiv' | (t2$Approbation_variante=='triple' & t2$approbation_info==FALSE)), weights = t2$weight)) 
summary(lm((approbation=="Non") ~ Approbation_variante, data=t2, subset=(t2$Approbation_variante=='indiv' | (t2$Approbation_variante=='triple' & t2$approbation_info==FALSE)), weights = t2$weight)) 
summary(lm((approbation=="NSP") ~ Approbation_variante, data=t2, subset=(t2$Approbation_variante=='indiv' | (t2$Approbation_variante=='triple' & t2$approbation_info==FALSE)), weights = t2$weight)) 
summary(lm((approbation=="Oui") ~ Approbation_variante, data=t2, subset=!is.missing(t2$approbation) & (t2$Approbation_variante=='indiv' | (t2$Approbation_variante=='triple' & t2$approbation_info==FALSE)), weights = t2$weight)) 
# For triple with info:
summary(lm((approbation=="Oui") ~ Approbation_variante, data=t2, subset=(t2$Approbation_variante=='indiv' | (t2$Approbation_variante=='triple' & t2$approbation_info==T)), weights = t2$weight)) 
summary(lm((approbation=="Non") ~ Approbation_variante, data=t2, subset=(t2$Approbation_variante=='indiv' | (t2$Approbation_variante=='triple' & t2$approbation_info==T)), weights = t2$weight)) 
summary(lm((approbation=="NSP") ~ Approbation_variante, data=t2, subset=(t2$Approbation_variante=='indiv' | (t2$Approbation_variante=='triple' & t2$approbation_info==T)), weights = t2$weight)) 
summary(lm((approbation=="Oui") ~ Approbation_variante, data=t2, subset=!is.missing(t2$approbation) & (t2$Approbation_variante=='indiv' | (t2$Approbation_variante=='triple' & t2$approbation_info==T)), weights = t2$weight)) 
## Regression for Framing in 3.1.2
summary(lm((approbation=="Oui") ~ Approbation_variante, data=t2, subset=!is.missing(t2$approbation) & t2$Approbation_variante!='seuls', weights = t2$weight)) 
## Regression for More Information in 3.1.3
summary(lm((approbation=="Oui") ~ approbation_info, data=t2, subset=t2$Approbation_variante=='triple', weights = t2$weight)) 
summary(lm((approbation=="Non") ~ approbation_info, data=t2, subset=t2$Approbation_variante=='triple', weights = t2$weight)) 
summary(lm((approbation=="NSP") ~ approbation_info, data=t2, subset=t2$Approbation_variante=='triple', weights = t2$weight)) 
summary(lm((approbation=="Oui") ~ approbation_info, data=t2, subset=!is.missing(t2$approbation) & t2$Approbation_variante=='triple', weights = t2$weight)) 


##### Bas revenus #####
decrit(f$smic)
plot(density(f$smic[f$smic>-1 & !is.na(f$smic)],  weights = f$weight[f$smic>-1 & !is.na(f$smic)], bw=3), main="Montant désiré pour le SMIC\n(quartiles : 1300-1500-1600, N=735 + NSP=133)", xlab="En France, quel devrait être le revenu minimum légal \n d'une personne seule travaillant à temps plein (après impôts et prestations sociales) ?", xlim=c(1000,2000))
decrit(f$rdb_aid)
decrit(m$rdb[m$variante_rdb=='aid'], weights = m$weight[m$variante_rdb=='aid'])
decrit(t$rdb[t$variante_rdb=='aid'], weights = t$weight[t$variante_rdb=='aid'])
data_smic <- cbind(data_seuils(variable="smic", data=f, seuils=c(1200, 1300, 1499, 1500), miss=T, closed_right = T, closed_left = FALSE))
barres(data=data_smic, file="demogrant", title="<b>Montant désiré pour le SMIC</b> (en €/mois, N=1340)", labels=c(""), color=c(color5, 'lightgrey'), hover=c("Moins de 1200", "De 1201 à 1300", "De 1301 à 1499", "1500", "Plus de 1500", "NSP (Ne sait pas, ne se prononce pas)"), legend=c("Moins de 1200", "De 1201 à 1300", "De 1301 à 1499", "1500", "Plus de 1500", "NSP"), nsp=T, sort=FALSE, showLegend=T, margin_r=0, margin_l=NA)
barres(data=data_smic, file="demogrant", title="", labels=c("En France, quel devrait être le revenu minimum légal<br> d'une personne seule travaillant à temps plein,<br> après impôts et prestations sociales ?"), color=c(color5, 'lightgrey'), hover=c("Moins de 1200", "De 1201 à 1300", "De 1301 à 1499", "1500", "Plus de 1500", "NSP (Ne sait pas, ne se prononce pas)"), legend=c("Moins de 1200", "De 1201 à 1300", "De 1301 à 1499", "1500", "Plus de 1500", "NSP (Ne sait pas, ne se prononce pas)"), nsp=T, sort=FALSE, showLegend=T, margin_r=0, margin_l=165)
barres(data=data_smic, file="demogrant", title="", labels=c("What should be the French legal minimum income <br>after taxes and transfers <br>for a single person above 25 working full-time?"), color=c(color5, 'lightgrey'), hover=c("Less than 1200", "From 1201 to 1300", "From 1301 to 1499", "1500", "More than 1500", "PNR (I don't know, I don't want to answer)"), legend=c("Less than 1200", "From 1201 to 1300", "From 1301 to 1499", "1500", "More than 1500", "PNR (I don't know, I don't want to answer)"), nsp=T, sort=FALSE, showLegend=T, margin_r=0, margin_l=150) # bug: legend and hover must be equal, or maybe legend must be long enough

##### Choix impôt #####
decrit(f$choix_impot_repartition)
decrit(f$choix_impot_satisfaisant)
decrit(f$choix_impot_sondage)
decrit(f$choix_impot_deliberation)
decrit(f$choix_impot_referendum)
decrit(f$choix_impot_tous, weights=f$weight) #  Vaut Oui ssi le répondant a répondu 'Oui' aux 5 questions choix_impot_, 'NSP' s'iel a répondu 'NSP' à au moins l'une d'entre elles, 'Non' s'iel s'est exprimé sur les 5 questions sans répondre 'Oui' à chaque fois"
decrit(f$choix_impot_tous, miss=TRUE, weights=f$weight)
oui_non(margin_l = 480, data=f, en=TRUE, file="choix_impot_en", vars=c("choix_impot_repartition", "choix_impot_satisfaisant", "choix_impot_sondage", "choix_impot_deliberation", "choix_impot_referendum"), 
        labels=c("Should the State determine income tax rates<br> to target a given distribution?",
          "Currently, income tax rates are proposed <br>by the government and adopted by the Parliament. <br>Is this process satisfactory?",
          "Should tax reforms respect typical preferences<br> obtained by a poll?",
          "Should tax reforms respect typical preferences <br> obtained by a national vote preceded by local deliberations?",
          "Should tax reforms be adopted by referendum?"))
# oui_non(margin_l = 480, data=f, en=FALSE, title="Opinion des Français sur le processus de détermination des taux d'imposition", file="choix_impot_en", vars=c("choix_impot_repartition", "choix_impot_satisfaisant", "choix_impot_sondage", "choix_impot_deliberation", "choix_impot_referendum"),
#         labels=c("L'État devrait-il fixer les taux d'imposition <br> afin d'obtenir une répartition des revenus donnée ?",
#           "Actuellement, les taux d'imposition sont <br>approuvés par le Parlement sur proposition du gouvernement.<br> Cette méthode est-elle satisfaisante ?",
#           "Serait-il souhaitable que les réformes fiscales respectent <br>les préférences typiques, obtenues par <br>un sondage ?",
#           "Serait-il souhaitable que les réformes fiscales respectent <br>les préférences typiques, obtenues par <br>un vote national précédé de délibérations locales ?",
#           "Les réformes fiscales devraient-elles être adoptées par référendum ?"))

##### Démocratie délégative #####
Label(f$democratie_delegative)
decrit(f$democratie_delegative)
length(which(f$delegation_constitution!=''))/length(f[,1])
length(which(f$delegation_retraites!=''))/length(f[,1])
length(which(f$delegation_nucleaire!=''))/length(f[,1])
length(which(f$delegation_taxe!=''))/length(f[,1])
length(which(f$delegation_immigration!=''))/length(f[,1])
length(which(f$delegation_euthanasie!=''))/length(f[,1])
f$delegation_constitution[f$delegation_constitution!=''] # mdme chanas, (presque tous) les autres sont un ou des députés (sans le nom), un parti ou un présidentiable
f$delegation_retraites[f$delegation_retraites!=''] # les réponses précédentes reviennent
f$delegation_nucleaire[f$delegation_nucleaire!='']
f$delegation_taxe[f$delegation_taxe!='']
f$delegation_immigration[f$delegation_immigration!='']
f$delegation_euthanasie[f$delegation_euthanasie!='']
temp <- f
f <- f[,c("democratie_delegative", "delegation_constitution", "delegation_retraites", "delegation_nucleaire", "delegation_taxe", "delegation_immigration", "delegation_euthanasie", "delegation_constitution_choix", "delegation_retraites_choix", "delegation_nucleaire_choix", "delegation_taxe_choix", "delegation_immigration_choix", "delegation_euthanasie_choix", 'sexe', 'age', 'revenu', 'taille_foyer', 'diplome', 'Age', 'Diplome', 'diplome4', 'statut_emploi', 'interet_politique', 'gauche_droite', 'Gauche_droite', 'exclu', 'region', 'csp', 'taille_agglo', 'revdisp', 'rev_tot', 'weight')]
save(f, file="/home/adrien/Downloads/democratie_delegative.RData")
f <- temp
rm(temp)
# Texte préalable aux questions:
# La démocratie délégative est une forme originale de démocratie, où les citoyens peuvent au choix voter eux-mêmes sur des propositions, ou déléguer leur vote à la personne ou l'organisation de leur choix. 
# Une personne ou une organisation ayant reçu des délégations peut à son tour voter directement, ou déléguer à quelqu'un d'autre son vote ainsi que les délégations qu'elle aura reçues.
# Dans ce qui suit, il vous sera demandé pour différents domaines si vous voteriez directement ou si vous délégueriez votre vote. 
# Dans le cas où vous délégueriez votre vote, le mieux serait de préciser le délégué de votre choix, en inscrivant l'adresse e-mail de cette personne si vous la connaissez, ou bien son nom si c'est une personnalité connue (vous pouvez aussi inscrire le nom d'un parti politique, d'une association ou d'un syndicat). 
# Nous enverrons alors cette question du sondage à la personne désignée, qui pourra à son tour nous fournir une autre personne à contacter, ou dire qu'elle votera directement (nous n'utiliserons les e-mails que dans un but de recherche scientifique).

# [Puis, 6 questions sur le format: ]
# Concernant une réforme des retraites, vous ... (votez directement / déléguez votre vote à [champ libre] / vous abstenez / NSP (Ne sait pas, ne se prononce pas))

# [Puis une dernière question: ]
# À votre avis, serait-il souhaitable d'introduire progressivement la démocratie délégative comme processus de décision (en commençant par l'expérimenter sur des citoyens volontaires) ? (Oui / Non / NSP (Ne sait pas, ne se prononce pas))


##### Réfugiés #####
decrit(ta$moinsRefugies[ta$refugies_info==T])
decrit(ta$refugies_echelle[ta$refugies_info==T])

decrit(t$Refugies)
decrit(t$Refugies[t$refugies_info==T])
decrit(t$Refugies[t$refugies_info==FALSE])
decrit(t$moinsRefugies[t$refugies_info==T])
decrit(t$moinsRefugies[t$refugies_info==FALSE])
decrit(t$plusRefugies[t$refugies_info==T])
decrit(t$plusRefugies[t$refugies_info==FALSE])
summary(lm(I(Refugies=='Autant' | Refugies=='Moins' | Refugies=='Aucun') ~ refugies_info, data=f))
summary(lm(I(Refugies=='NSP') ~ refugies_info, data=f))
summary(lm(moinsRefugies ~ refugies_info, data=t)) # TODO: ta
summary(lm(plusRefugies ~ refugies_info, data=t)) # 0.015 non significatif (avant CT *) : l'info est associée à vouloir plus de réfugiés
summary(lm(refugies_echelle ~ refugies_info, data=t))
summary(lm(refugies_echelle ~ refugies_info, data=ta)) # ** : info associée à vouloir plus de réfugiés (refugies_echelle : + (1) --> - (6))
summary(lm(Refugies=='NSP' ~ refugies_info, data=t)) 
summary(lm(I(as.numeric(Refugies)==8 | as.numeric(Refugies)==9) ~ refugies_info, data=t))

##### Acquiescence #####
# acquiescence mise en évidence... même dans le cas de la participation électorale.
decrit(f$vitesse_maintien)
decrit(f$vitesse_reduction)
decrit(f$legislatives_abstention)
decrit(f$legislatives_vote)
decrit(f$diner_simple)
decrit(f$diner_amis)
decrit(f$diner_famille)
decrit(m$fusion_irpp_cotsoc)
decrit(f$fusion_irpp_cotsoc_contre)
summary(lm(vitesse==80 ~ (variante_vitesse=='pro_maintien'), data=t)) # 0.07**
summary(lm(vitesse==80 ~ (variante_vitesse=='pro_maintien'), data=t, subset=(!is.missing(t$vitesse)))) # 0.07**
summary(lm(vitesse==-1 ~ (variante_vitesse=='pro_maintien'), data=t)) # 0
# Pop adulte: 52.4 M https://www.insee.fr/fr/statistiques/1892086?sommaire=1912926
# Votants 1er tour: 23.17 M = 44.2% (exprimés: 22.65 M = 43.2%), 1er tour présidentielle: 37 M = 70.6%
decrit(t$vote) # 81% 
decrit(t$vote, miss=T) # 77.6% 
decrit(t$vote, miss=T, weights = t$weight) # 76.4% 
sum(f$weight[f$vote==1])/sum(f$weight)
summary(lm((vote==1) ~ variante_vote, data=t)) # 0.05*: indicates that acquiescence is driven by neglect (ex: abstention conflated with blank vote), not by will to please
summary(lm((vote==1) ~ variante_vote, data=t, subset=!is.missing(t$vote))) # 0.05*
summary(lm(diner=='En famille' ~ variante_diner, data=t)) # 0.03 insignificant
summary(lm(fusion_irpp_cotsoc=='Oui' ~ variante_fusion, data=t)) # 0.10***
summary(lm(fusion_irpp_cotsoc=='Oui' ~ variante_fusion, data=t, subset=(!is.missing(t$fusion_irpp_cotsoc)))) # 0
summary(lm(fusion_irpp_cotsoc=='NSP' ~ variante_fusion, data=t)) # -0.21***


##### Dépense publique #####
decrit(f$recette_totale) # 988 (=original), mean=1000
decrit(f$depense_totale) # 1060, original: 1052 (mean: 1068)
decrit(f$variation_totale) # +0.8% (mean: +1.542%)
decrit(f$variation_recette) # 0 (mean: +1.229%)
pib2016_implicite <- (1052 - 988)/0.026
f$deficit <- (f$depense_totale - f$recette_totale)/pib2016_implicite # TODO: mettre dans prepare_f et checker que c'est comme ça que je le calcule dans .js
decrit(f$deficit) # stable à 2.6% (mean: 2.744%)
decrit(f$variation_sante) # +2% (mean: +2.8%)
decrit(f$variation_retraites) # +1% (mean: +2.375%)
decrit(f$variation_protection) # 0 (mean: -0.6252%)
decrit(f$variation_education) # +1.677% (mean: +2.09%)
decrit(f$variation_recherche) # +1% (mean: +1.677%)
decrit(f$variation_loisirs) # 0 (mean: -1.482%)
decrit(f$variation_infrastructures) # 0 (mean: -0.2%)
decrit(f$variation_justice) # +0.806% (mean: +1.53%)
decrit(f$variation_armee) # 0 (mean: -0.3623%)
decrit(f$variation_securite) # +1% (mean: +1.714%)
decrit(f$variation_aide) # 0 (mean: -2.305%)
length(which(f$variation_aide==0))/length(which(!is.na(f$variation_aide))) # 26% of 0
length(which(f$variation_aide>0))/length(which(!is.na(f$variation_aide))) # 31% > 0
length(which(f$variation_aide<0))/length(which(!is.na(f$variation_aide))) # 44% < 0
# 344 NA for all: not sure how 1-3 have been not NA while others were NA, because normally when one is set, all the others are set as well
length(which(is.na(f$variation_aide) & is.na(f$variation_securite) & is.na(f$variation_armee) & is.na(f$variation_justice) & is.na(f$variation_infrastructures) & is.na(f$variation_loisirs) & is.na(f$variation_recherche) & is.na(f$variation_education) & is.na(f$variation_protection) & is.na(f$variation_retraites) & is.na(f$variation_sante) & is.na(f$variation_recette) & is.na(f$variation_totale)))
f$depense_recette <- f$recette_totale
categories_depenses <- c("recette", "totale", "sante", "education", "retraites", "securite", "recherche", "justice", "armee", "protection", "infrastructures", "loisirs", "aide")
for (v in categories_depenses) print(paste(v, mean(f[[paste('depense',v,sep="_")]], na.rm=T)))
for (v in categories_depenses) print(paste(v, median(f[[paste('depense',v,sep="_")]], na.rm=T)))
for (v in categories_depenses) print(paste(v, mean(f[[paste('variation',v,sep="_")]], na.rm=T)))
for (v in categories_depenses) print(paste(v, median(f[[paste('variation',v,sep="_")]], na.rm=T))) # median variaton_education > median depense because median is an average of two points
for (v in categories_depenses) print(paste(v, weighted.mean(f[[paste('depense',v,sep="_")]], f$weight, na.rm=T)))
for (v in categories_depenses) print(paste(v, weighted.median(f[[paste('depense',v,sep="_")]],f$weight, na.rm=T)))
for (v in categories_depenses) print(paste(v, weighted.mean(f[[paste('variation',v,sep="_")]], f$weight, na.rm=T)))
for (v in categories_depenses) print(paste(v, weighted.median(f[[paste('variation',v,sep="_")]], f$weight, na.rm=T)))
sum(f$weight[!is.na(f$variation_aide) & f$variation_aide==20])/sum(f$weight[!is.na(f$variation_aide)]) # +20%: 0.5%
sum(f$weight[!is.na(f$variation_aide) & f$variation_aide==-20])/sum(f$weight[!is.na(f$variation_aide)]) # -20%: 3.8%

# 85% of non PNR understood, 21% (64%) PNR among understood (not). Only 31% of non PNR trust their choice (45% don't)
decrit(f$compris_depenses) # 71%
decrit(f$compris_depenses[!is.na(f$variation_recette)]) # 85%
decrit(f$compris_depenses[is.na(f$variation_recette)]) # 44%
decrit(is.na(f$variation_recette)) # 34%
decrit(is.na(f$variation_recette)[f$compris_depenses=='Oui']) # 21%
decrit(is.na(f$variation_recette)[f$compris_depenses=='Non']) # 64%
decrit(is.na(f$variation_recette)[as.character(f$compris_depenses)=='Bug']) # 84%
decrit(f$depenses_confiant) # 23% (Non: 44%)
decrit(f$depenses_confiant[!is.na(f$variation_recette)]) # 31% (Non: 45%, NSP: 24%)
decrit(f$depenses_confiant[is.na(f$variation_recette)]) # 8% (Non: 42%, NSP: 49%)
decrit(f$depenses_confiant[f$compris_depenses=='Oui']) # 30%
decrit(f$depenses_confiant[f$compris_depenses=='Non']) # 5%
decrit(f$compris_depenses[f$depenses_confiant=='Assez confiant.e']) # 93%
decrit(f$compris_depenses[f$depenses_confiant=='Pas vraiment confiant.e']) # 76%
decrit(f$compris_depenses[f$depenses_confiant=='NSP']) # 49% those who don't know if they trust their answers understand less well...
decrit(f$compris_depenses[f$depenses_confiant=='NSP' & !is.na(f$variation_recette)]) # 68% ...but answer less, overall those who also answer understand as well
cor((f$depenses_confiant=='Assez confiant.e'), (f$compris_depenses=='Oui')) # 0.27
cor((f$depenses_confiant!='Pas vraiment confiant.e'), (f$compris_depenses=='Oui')) # -0.10
cor((f$depenses_confiant=='Pas vraiment confiant.e'), (f$compris_depenses=='Non')) # -0.10
# slightly higher spending for 'understood', but no change with trust
decrit(f$variation_totale) # 0.73
decrit(f$variation_totale[f$compris_depenses=='Oui'])  # 0.84
decrit(f$variation_totale[f$compris_depenses=='Non']) # 0
decrit(f$variation_totale[f$depenses_confiant=='Pas vraiment confiant.e']) # 0.7
decrit(f$variation_totale[f$depenses_confiant=='Assez confiant.e']) # 0.71
decrit(f$variation_recette) # 0
decrit(f$variation_recette[f$compris_depenses=='Oui']) # 0
decrit(f$variation_recette[f$compris_depenses=='Non']) # 0
decrit(f$variation_recette[f$depenses_confiant=='Pas vraiment confiant.e']) # 0
decrit(f$variation_recette[f$depenses_confiant=='Assez confiant.e']) # 0
decrit(f$variation_aide) # 0
decrit(f$variation_aide[f$compris_depenses=='Oui']) # 0
decrit(f$variation_aide[f$compris_depenses=='Non']) # 0
decrit(f$variation_aide[f$depenses_confiant=='Pas vraiment confiant.e']) # -0.1
decrit(f$variation_aide[f$depenses_confiant=='Assez confiant.e']) # 0

# only gauche_droite et g_d^2
summary(lm(variation_totale ~ gauche_droite + I(gauche_droite^2) + I(compris_depenses=='Oui') + I(depenses_confiant=='Assez confiant.e') + I(depenses_confiant=='Pas vraiment confiant.e') + revdisp + Age + I(Age^2) + Diplome + as.numeric(taille_agglo) + sexe, data = f, weights = f$weight, subset=(Age!=0 & sexe!='Autre')))
summary(lm(variation_totale ~ Gauche_droite + I(compris_depenses=='Oui') + I(depenses_confiant=='Assez confiant.e') + I(depenses_confiant=='Pas vraiment confiant.e') + revdisp + Age + I(Age^2) + Diplome + as.numeric(taille_agglo) + sexe, data = f, weights = f$weight, subset=(Age!=0 & sexe!='Autre')))
# revdisp (appears because including gauche_droite reduces sample size)
summary(lm(variation_totale ~ I(compris_depenses=='Oui') + I(depenses_confiant=='Assez confiant.e') + I(depenses_confiant=='Pas vraiment confiant.e') + revdisp + Age + I(Age^2) + Diplome + as.numeric(taille_agglo) + sexe, data = f, weights = f$weight, subset=(Age!=0 & sexe!='Autre')))
summary(lm(variation_totale ~ revdisp + Age + I(Age^2) + Diplome + as.numeric(taille_agglo) + sexe, data = f, weights = f$weight, subset=(Age!=0 & sexe!='Autre')))
summary(lm(revdisp ~ gauche_droite + I(gauche_droite^2), data=f)) # ^2 ***
# sexe
summary(lm(variation_totale ~ Age + I(Age^2) + Diplome + as.numeric(taille_agglo) + sexe, data = f, weights = f$weight, subset=(Age!=0 & sexe!='Autre')))
# g_d^2, Age, Age^2
summary(lm(variation_recette ~ gauche_droite + I(gauche_droite^2) + I(compris_depenses=='Oui') + I(depenses_confiant=='Assez confiant.e') + I(depenses_confiant=='Pas vraiment confiant.e') + revdisp + Age + I(Age^2) + Diplome + as.numeric(taille_agglo) + sexe, data = f, weights = f$weight, subset=(Age!=0 & sexe!='Autre')))
# only gauche_droite et g_d^2
summary(lm(variation_sante ~ gauche_droite + I(gauche_droite^2) + I(compris_depenses=='Oui') + I(depenses_confiant=='Assez confiant.e') + I(depenses_confiant=='Pas vraiment confiant.e') + revdisp + Age + I(Age^2) + Diplome + as.numeric(taille_agglo) + sexe, data = f, weights = f$weight, subset=(Age!=0 & sexe!='Autre')))
# gauche_droite, g_d^2, age, diplome, pas confiant (also compris='Oui' if Gauche_droite included)
summary(lm(variation_education ~ gauche_droite + I(gauche_droite^2) + I(compris_depenses=='Oui') + I(depenses_confiant=='Assez confiant.e') + I(depenses_confiant=='Pas vraiment confiant.e') + revdisp + Age + I(Age^2) + Diplome + as.numeric(taille_agglo) + sexe, data = f, weights = f$weight, subset=(Age!=0 & sexe!='Autre')))
# g_d, diplome
summary(lm(variation_retraites ~ gauche_droite + I(gauche_droite^2) + Age + I(Age^2) + I(compris_depenses=='Oui') + I(depenses_confiant=='Assez confiant.e') + I(depenses_confiant=='Pas vraiment confiant.e') + revdisp + Diplome + as.numeric(taille_agglo) + sexe, data = f, weights = f$weight, subset=(Age!=0 & sexe!='Autre')))
# g_d, age, revdisp, homme, pas confiant
summary(lm(variation_retraites ~ Gauche_droite + age + I(compris_depenses=='Oui') + I(depenses_confiant=='Assez confiant.e') + I(depenses_confiant=='Pas vraiment confiant.e') + revdisp + Diplome + as.numeric(taille_agglo) + sexe, data = f, weights = f$weight, subset=(Age!=0 & sexe!='Autre')))
summary(lm(variation_retraites ~ Age, data = f, weights = f$weight)) # **
summary(lm(variation_retraites ~ Age + I(Age^2), data = f, weights = f$weight)) # -
summary(lm(variation_retraites ~ age, data = f, weights = f$weight, subset=(Age!=0))) # 50+ *
summary(lm(variation_retraites ~ Gauche_droite + age, data = f, weights = f$weight, subset=(Age!=0))) # 50+ *, g_d
# g_d, taille_agglo
summary(lm(variation_securite ~ gauche_droite + I(gauche_droite^2) + I(compris_depenses=='Oui') + I(depenses_confiant=='Assez confiant.e') + I(depenses_confiant=='Pas vraiment confiant.e') + revdisp + Age + I(Age^2) + Diplome + as.numeric(taille_agglo) + sexe, data = f, weights = f$weight, subset=(Age!=0 & sexe!='Autre')))
# only gauche_droite et g_d^2 (also compris='oui' if Gauche_droite included)
summary(lm(variation_recherche ~ gauche_droite + I(gauche_droite^2) + I(compris_depenses=='Oui') + I(depenses_confiant=='Assez confiant.e') + I(depenses_confiant=='Pas vraiment confiant.e') + revdisp + Age + I(Age^2) + Diplome + as.numeric(taille_agglo) + sexe, data = f, weights = f$weight, subset=(Age!=0 & sexe!='Autre')))
# only gauche_droite et g_d^2
summary(lm(variation_justice ~ gauche_droite + I(gauche_droite^2) + I(compris_depenses=='Oui') + I(depenses_confiant=='Assez confiant.e') + I(depenses_confiant=='Pas vraiment confiant.e') + revdisp + Age + I(Age^2) + Diplome + as.numeric(taille_agglo) + sexe, data = f, weights = f$weight, subset=(Age!=0 & sexe!='Autre')))
# only gauche_droite, Age, Age^2 (also Diplome if Gauche_droite included)
summary(lm(variation_armee ~ gauche_droite + I(gauche_droite^2) + I(compris_depenses=='Oui') + I(depenses_confiant=='Assez confiant.e') + I(depenses_confiant=='Pas vraiment confiant.e') + revdisp + Age + I(Age^2) + Diplome + as.numeric(taille_agglo) + sexe, data = f, weights = f$weight, subset=(Age!=0 & sexe!='Autre')))
# g_d, pas confiant (also revdisp if Gauche_droite included)
summary(lm(variation_protection ~ gauche_droite + I(gauche_droite^2) + I(compris_depenses=='Oui') + I(depenses_confiant=='Assez confiant.e') + I(depenses_confiant=='Pas vraiment confiant.e') + revdisp + Age + I(Age^2) + Diplome + as.numeric(taille_agglo) + sexe, data = f, weights = f$weight, subset=(Age!=0 & sexe!='Autre')))
summary(lm(variation_protection ~ Gauche_droite + I(compris_depenses=='Oui') + I(depenses_confiant=='Assez confiant.e') + I(depenses_confiant=='Pas vraiment confiant.e') + revdisp + Age + I(Age^2) + Diplome + as.numeric(taille_agglo) + sexe, data = f, weights = f$weight, subset=(Age!=0 & sexe!='Autre')))
decrit(f$variation_protection[f$depenses_confiant=='Pas vraiment confiant.e']) # 0, mean -0.76
decrit(f$variation_protection[f$depenses_confiant=='Assez confiant.e']) # 0, mean -1.04
decrit(f$variation_protection[f$depenses_confiant=='NSP']) # 0, mean -0.19
# only g_d
summary(lm(variation_infrastructures ~ gauche_droite + I(gauche_droite^2) + I(compris_depenses=='Oui') + I(depenses_confiant=='Assez confiant.e') + I(depenses_confiant=='Pas vraiment confiant.e') + revdisp + Age + I(Age^2) + Diplome + as.numeric(taille_agglo) + sexe, data = f, weights = f$weight, subset=(Age!=0 & sexe!='Autre')))
# g_d, pas confiant, revdisp
summary(lm(variation_loisirs ~ gauche_droite + I(gauche_droite^2) + I(compris_depenses=='Oui') + I(depenses_confiant=='Assez confiant.e') + I(depenses_confiant=='Pas vraiment confiant.e') + revdisp + Age + I(Age^2) + Diplome + as.numeric(taille_agglo) + sexe, data = f, weights = f$weight, subset=(Age!=0 & sexe!='Autre')))
# only g_d
summary(lm(variation_aide ~ gauche_droite + I(gauche_droite^2) + I(compris_depenses=='Oui') + I(depenses_confiant=='Assez confiant.e') + I(depenses_confiant=='Pas vraiment confiant.e') + revdisp + Age + I(Age^2) + Diplome + as.numeric(taille_agglo) + sexe, data = f, weights = f$weight, subset=(Age!=0 & sexe!='Autre')))


variables_variation <- rev(c("variation_recette", "variation_totale", "variation_sante", "variation_education", "variation_retraites", "variation_securite", "variation_recherche", "variation_justice", "variation_armee", "variation_protection", "variation_infrastructures", "variation_loisirs", "variation_aide"))
data_spending <- function(data=f, miss=T, weights=T, seuils = c(-4, -0.5, 0.5, 4), zero=T, variables = variables_variation) {
  matrice <- c()
  # variables <- c("variation_recette", "variation_totale", "variation_sante", "variation_retraites", "variation_protection", "variation_education", "variation_recherche", "variation_loisirs", "variation_infrastructures", "variation_justice", "variation_armee", "variation_securite", "variation_aide")
  # variables <- rev(c("variation_recette", "variation_totale", "variation_sante", "variation_education", "variation_retraites", "variation_securite", "variation_recherche", "variation_justice", "variation_armee", "variation_protection", "variation_infrastructures", "variation_loisirs", "variation_aide"))
  for (var in variables) {
    if (zero) {
      if (weights) { middle <- c(sum(data[['weight']][which(data[[var]]< 0 & data[[var]]>= seuils[2])])/sum(data[['weight']][which(!is.na(data[[var]]))]), sum(data[['weight']][which(data[[var]]==0)])/sum(data[['weight']][which(!is.na(data[[var]]))]), sum(data[['weight']][which(data[[var]]<= seuils[3] & data[[var]]> 0)])/sum(data[['weight']][which(!is.na(data[[var]]))]))
      } else { middle <-  c(length(which(data[[var]]< 0 & data[[var]]>= seuils[2]))/length(which(!is.na(data[[var]]))), length(which(data[[var]]==0))/length(which(!is.na(data[[var]]))), length(which(data[[var]]<= seuils[3] & data[[var]]> 0))/length(which(!is.na(data[[var]])))) }
    } else { 
      if (weights) { middle <- sum(data[['weight']][which(data[[var]]<= seuils[3] & data[[var]]>= seuils[2])])/sum(data[['weight']][which(!is.na(data[[var]]))])
      } else { middle <- length(which(data[[var]]<= seuils[3] & data[[var]]>= seuils[2]))/length(which(!is.na(data[[var]]))) }
    }
    if (miss) {
      mat <- c(length(which(data[[var]]< seuils[1]))/length(which(!is.na(data[[var]]))), length(which(data[[var]]>= seuils[1] & data[[var]] < seuils[2]))/length(which(!is.na(data[[var]]))), middle, length(which(data[[var]]<= seuils[4] & data[[var]]>seuils[3]))/length(which(!is.na(data[[var]]))), length(which(data[[var]]>4))/length(which(!is.na(data[[var]]))),length(which(is.na(data[[var]])))/length(which(!is.na(data[[var]]))))
      if (weights) { mat <- c(sum(data[['weight']][which(data[[var]]< seuils[1])])/sum(data[['weight']][which(!is.na(data[[var]]))]), sum(data[['weight']][which(data[[var]]>= seuils[1] & data[[var]] < seuils[2])])/sum(data[['weight']][which(!is.na(data[[var]]))]), middle, sum(data[['weight']][which(data[[var]]<= seuils[4] & data[[var]]>seuils[3])])/sum(data[['weight']][which(!is.na(data[[var]]))]), sum(data[['weight']][which(data[[var]]>seuils[4])])/sum(data[['weight']][which(!is.na(data[[var]]))]), sum(data[['weight']][which(is.na(data[[var]]))])/sum(data[['weight']][which(!is.na(data[[var]]))])) }
    }
    else {
      mat <- c(length(which(data[[var]]< seuils[1]))/length(which(!is.na(data[[var]]))), length(which(data[[var]]>= seuils[1] & data[[var]] < seuils[2]))/length(which(!is.na(data[[var]]))), middle, length(which(data[[var]]<= seuils[4] & data[[var]]>seuils[3]))/length(which(!is.na(data[[var]]))), length(which(data[[var]]>4))/length(which(!is.na(data[[var]]))))
      if (weights) { mat <- c(sum(data[['weight']][which(data[[var]]< seuils[1])])/sum(data[['weight']][which(!is.na(data[[var]]))]), sum(data[['weight']][which(data[[var]]>= seuils[1] & data[[var]] < seuils[2])])/sum(data[['weight']][which(!is.na(data[[var]]))]), middle, sum(data[['weight']][which(data[[var]]<= seuils[4] & data[[var]]>seuils[3])])/sum(data[['weight']][which(!is.na(data[[var]]))]), sum(data[['weight']][which(data[[var]]>seuils[4])])/sum(data[['weight']][which(!is.na(data[[var]]))])) } }
    matrice <- c(matrice, mat) }
  matrice <- matrix(c(matrice), ncol=length(variables))
  return(matrice)
  # return(as.data.frame(matrice))
}

barres(data=data_spending(miss=FALSE, zero=FALSE), nsp=FALSE, file="public_spendings_consistent", labels=c("Aid to poor countries", "Leisure", "Infrastructures", "Social protection", "Defence and army", "Justice", "Scientific research", "Internal security", "Retirement pensions", "Education", "Healthcare", "<b>Total spending</b>", "<b>Revenues</b>"), color=c(color5 ,'lightgrey'), hover=c("Less than -4%", "-4% to -0.6%", "-0.5% to 0.5%", "0.6% to 4%", "More than 4%", "PNR"), sort=FALSE, legend=c("Less than -4%", "-4% to -0.6%", "-0.5% to 0.5%", "0.6% to 4%", "More than 4%", "PNR"), showLegend=T, margin_r=0, margin_l=NA) #  (unemployment, child allowances, housing, welfare...),  : culture, medias and sport, (police) #. c("<b>Revenues</b>", "<b>Total spending</b>", "Healthcare", "Retirement pensions", "Social protection", "Education", "Scientific research", "Leisure", "Infrastructures", "Justice", "Defence and army", "Internal security", "Aid to poor countries")
barres(data=data_spending(miss=FALSE, zero=FALSE), nsp=FALSE, file="depenses_publiques_coherent", labels=c("Aide aux pays pauvres", "Loisirs", "Infrastructures", "Protection sociale", "Défense et armée", "Justice", "Recherche scientifique", "Sécurité intérieure", "Retraites", "Éducation", "Système de santé", "<b>Dépenses totales</b>", "<b>Recettes</b>"), color=c(color5 ,'lightgrey'), hover=c("Moins de -4%", "-4% ? -0.6%", "-0.5% ? 0.5%", "0.6% ? 4%", "Plus de 4%", "NSP"), sort=FALSE, legend=c("Moins de -4%", "-4% ? -0.6%", "-0.5% ? 0.5%", "0.6% ? 4%", "Plus de 4%", "NSP"), showLegend=T, margin_r=0, margin_l=NA) #  (ch?mage, allocs, APL, RSA...),  : culture, m?dias et sport, (police, gendarmerie) # c("<b>Recettes</b>", "<b>D?penses totales</b>", "Syst?me de sant?", "Retraites", "Protection sociale", "?ducation", "Recherche scientifique", "Loisirs", "Infrastructures", "Justice", "D?fense et arm?e", "S?curit? int?rieure", "Aide aux pays pauvres")
barres(data=data_spending(miss=FALSE, zero=T, seuils = c(-5, -2, 2, 5)), nsp=FALSE, file="public_spendings_consistent", sort=FALSE, labels=c("Aid to poor countries", "Leisure", "Infrastructures", "Social protection", "Defence and army", "Justice", "Scientific research", "Internal security", "Retirement pensions", "Education", "Healthcare", "<b>Total spending</b>", "<b>Revenues</b>"), color=c("black", color5[1:3], rainbow(7)[c(3,5,6)]), hover=c("Less than -4%", "-4% to -2.1%", "-2% to -0.1%", "0%", "0.1% to 2%", "2.1% to 4%", "More than 4%", "PNR"), legend=c("Less than -4%", "-4% to -2.1%", "-2% to -0.1%", "0%", "0.1% to 2%", "2.1% to 4%", "More than 4%", "PNR"), showLegend=T, margin_r=0, margin_l=NA) #  (unemployment, child allowances, housing, welfare...),  : culture, medias and sport, (police) # c("black", color5[1:3], rainbow(7)[c(3,5,6)])
barres(data=data_spending(miss=FALSE, zero=T, seuils = c(-5, -2, 2, 5)), nsp=FALSE, file="depenses_publiques_coherent", sort=FALSE, labels=c("Aide aux pays pauvres", "Loisirs", "Infrastructures", "Protection sociale", "Défense et armée", "Justice", "Recherche scientifique", "Sécurité intérieure", "Retraites", "Éducation", "Système de santé", "<b>Dépenses totales</b>", "<b>Recettes</b>"), color=c("black", color5 ,'blue'), hover=c("Moins de -4%", "-4% ? -2.1%", "-2% ? -0.1%", "0%", "0.1% ? 2%", "2.1% ? 4%", "Plus de 4%", "NSP"), legend=c("Moins de -4%", "-4% ? -2.1%", "-2% ? -0.1%", "0%", "0.1% ? 2%", "2.1% ? 4%", "Plus de 4%", "NSP"), showLegend=T, margin_r=0, margin_l=NA) #  (ch?mage, allocs, APL, RSA...),  : culture, m?dias et sport, (police, gendarmerie) # c("black", color5 ,'blue')

data_variation <- f[,c("variation_recette", "variation_totale", "variation_sante", "variation_education", "variation_retraites", "variation_securite", "variation_recherche", "variation_justice", "variation_armee", "variation_protection", "variation_infrastructures", "variation_loisirs", "variation_aide")]
corr_variation <- cor(data_variation, use="complete.obs")
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
p.mat <- cor.mtest(data_variation)
corrplot(corr_variation, method='color', p.mat = p.mat, sig.level = 0.01, diag=T, tl.srt=45, tl.col='black', insig = 'blank') # , type='upper', order='hclust', addCoef.col = 'white', addCoefasPercent = T
corrplot(corr_variation, method='color', p.mat = p.mat, sig.level = 0.01, diag=T, tl.srt=45, tl.col='black', insig = 'blank', addCoef.col = 'white', addCoefasPercent = T) # , type='upper', order='hclust'

ecdf_variations <- c()
for (var in variables_variation) {
  ecdf_variations[[var]] <- wtd.Ecdf(f[[var]], weights=f$weight, normwt=T)
  plot(head(ecdf_variations[[var]]$ecdf, -1), ecdf_variations[[var]]$x[-1], mgp=c(3,0.6,0), ylim=c(-20,20), lwd=2, col='blue', type="step", main=substring(var, 11), ylab="Desired varaiation (in %)", xlab="Repartition of expressed answers (in %)") + grid()
}

ecdf_variations <- c()
ecdf_variations[["variation_recette"]] <- wtd.Ecdf(f[["variation_recette"]], weights=f$weight, normwt=T)
plot(head(ecdf_variations[["variation_recette"]]$ecdf, -1), ecdf_variations[["variation_recette"]]$x[-1], mgp=c(3,0.6,0), ylim=c(-20,20), lwd=2, col='black', type="step", main='', ylab="Desired varaiation (in %)", xlab="Distribution of expressed answers (in %)") + grid() # 
i = 0
for (var in head(variables_variation, -1)) {
  i <- i + 1
  ecdf_variations[[var]] <- wtd.Ecdf(f[[var]], weights=f$weight, normwt=T)
  lines(head(ecdf_variations[[var]]$ecdf, -1), ecdf_variations[[var]]$x[-1], lwd=2, col=rainbow(12)[i], type="step")
} # TODO: legend

newplot <- function(long = FALSE) {
  dev.off()
  cex_old <- par()$cex
  mai_old <- par()$mai
  par(cex=0.7, mai=c(0.01,0.3,0.14,0.05))
  if (long) par(mfrow=c(13, 1))
  else par(mfrow=c(5, 3))
}

ecdf_variations <- c()
for (var in rev(variables_variation)) {
  ecdf_variations[[var]] <- wtd.Ecdf(f[[var]], weights=f$weight, normwt=T)
  plot(head(ecdf_variations[[var]]$ecdf, -1), ecdf_variations[[var]]$x[-1], main=substring(var, 11), xaxt='n', ylim=c(-20,20), lwd=2, col='blue', type="step", xlab='', ylab='') + grid()
}

# by gauche/droite mean
# max = 0
# min = 1
newplot(long=T)
for (var in rev(variables_variation)) {
  var_gauche_droite <- sapply(-2:2, function(i) { weighted.mean(f[[var]][f$gauche_droite==i], na.rm=T, w = f$weight[f$gauche_droite==i])} )
  # if (min(var_gauche_droite) < min) min <- min(var_gauche_droite)
  # if (max(var_gauche_droite) > max) max <- max(var_gauche_droite)
  plot(-2:2, var_gauche_droite, xaxt='n', ylim=c(-8,7), lwd=2, col='blue', type="b", main=substring(var, 11), xlab='', ylab='', xlim=c(-2,3)) + grid() # max: 6.6, min: -7.6
  lines(3, mean(f[[var]][is.missing(f$gauche_droite)], na.rm=T), col='blue', lwd=2, type='b')
} # TODO: legend
# Indeterminate have middle position; Left-wing want more spending, health, education, pensions, protection, aid; Right-wing more army and police; Center less revenues

# by gauche/droite median
newplot(long=T)
for (var in rev(variables_variation)) {
  var_gauche_droite <- sapply(-2:2, function(i) { weighted.median(f[[var]][f$gauche_droite==i], na.rm=T, w = f$weight[f$gauche_droite==i])} )
  plot(-2:2, var_gauche_droite, xaxt='n', ylim=c(-5,6), lwd=2, col='blue', type="b", main=substring(var, 11), xlab='', ylab='', xlim=c(-2,3)) + grid() # max: 6, min: -5
  lines(3, median(f[[var]][is.missing(f$gauche_droite)], na.rm=T), col='blue', lwd=2, type='b')
} # TODO: different color if increasing or decreasing

# by revenu mean
newplot(long=T)
quintiles_revenu <- c(-Inf, quantile(f$revdisp, probs=c(0.2,0.4,0.6,0.8), na.rm=T), Inf)
# sapply(1:5, function(i) { length(which(f$revdisp >= quintiles_revenu[i] & f$revdisp < quintiles_revenu[i+1]))} )
for (var in rev(variables_variation)) {
  var_revenu <- sapply(1:5, function(i) { weighted.mean(f[[var]][f$revdisp >= quintiles_revenu[i] & f$revdisp < quintiles_revenu[i+1]], na.rm=T, w = f$weight[f$revdisp >= quintiles_revenu[i] & f$revdisp < quintiles_revenu[i+1]])} )
  plot(1:5, var_revenu, xaxt='n', ylim=c(-3.5, 3.5), lwd=2, col='blue', type="b", main=substring(var, 11), xlab='', ylab='') + grid() # max: 3.3, min: -3.2
} # results robust to other variables of revenu (revenu, revdisp, rev_tot have been tested: there are differences but small), revdisp is better because bins of exact same size
# Poorest prefer more aid, more protection, more revenues; richest prefer less total spending, less healthcare, less pensions, less army and less leisure

# by revenu median
newplot(long=T)
for (var in rev(variables_variation)) {
  var_revenu <- sapply(1:5, function(i) { weighted.median(f[[var]][f$revenu >= quintiles_revenu[i] & f$revenu < quintiles_revenu[i+1]], na.rm=T, w = f$weight[f$revenu >= quintiles_revenu[i] & f$revenu < quintiles_revenu[i+1]])} )
  plot(1:5, var_revenu, xaxt='n', ylim=c(-1.1, 2.7), lwd=2, col='blue', type="b", main=substring(var, 11), xlab='', ylab='') + grid() # max: 2.6, min: -1
} 

# by education mean
newplot(long=T)
for (var in rev(variables_variation)) {
  var_age <- sapply(0:6, function(i) { weighted.mean(f[[var]][f$Diplome==i], na.rm=T, w = f$weight[f$Diplome==i])} )
  plot(0:6, var_age, xaxt='n', ylim=c(-3.5, 3.5), lwd=2, col='blue', type="b", main=substring(var, 11), xlab='', ylab='') + grid() # max: 3.4, min: -3
} # Most educated want less spending, more education, much less army

# by size agglomeration mean
newplot(long=T)
for (var in rev(variables_variation)) {
  var_agglo <- sapply(1:5, function(i) { weighted.mean(f[[var]][f$taille_agglo==i], na.rm=T, w = f$weight[f$taille_agglo==i])} )
  plot(1:5, var_agglo, xaxt='n', ylim=c(-3.5, 3.5), lwd=2, col='blue', type="b", main=substring(var, 11), xlab='', ylab='') + grid() # max: 3.4, min: -3
} # medium towns want more protection, Paris more leisure

# by age mean
newplot(long=T)
for (var in rev(variables_variation)) {
  var_age <- sapply(c(1, 2, 3.3, 4.6, 7), function(i) { weighted.mean(f[[var]][f$Age==i], na.rm=T, w = f$weight[f$Age==i])} )
  plot(c(1, 2, 3.3, 4.6, 7), var_age, xaxt='n', ylim=c(-3.5, 3.5), lwd=2, col='blue', type="b", main=substring(var, 11), xlab='', ylab='') + grid() # max: 3.4, min: -3.4
} 
# Youngest want more revenu, army, aid; older more pensions aid aid
par(mfrow=c(1, 1))
par(cex=cex_old, mai=mai_old)

depenses_2016 <- c('aide'=4.767, 'loisirs'=27.752, 'infrastructures'=91.702, 'protection'=146.116, 'armee'=38.449, 'justice'=8.76, 'recherche'=9.107, 'securite'=27.976, 'retraites'=336.285, 'education'=119.672, 'sante'=240.927)
adjust_from_median <- function(fixed) { # fixed: two elements in categories/deficit/revenues/spending
  categories <- c()
  for (var in head(variables_variation, -2)) categories[[substring(var, 11)]] <- weighted.median(f[[paste('depense',substring(var, 11),sep='_')]], na.rm=T, w = f$weight)
  if (is.element('deficit', fixed)) deficit <- pib2016_implicite * weighted.median(f$deficit, na.rm=T, w = f$weight)
  if (is.element('revenues', fixed) | is.element('recettes', fixed)) revenues <- weighted.median(f$recette_totale, na.rm=T, w = f$weight)
  if (is.element('spending', fixed) | is.element('depenses', fixed)) spending <- weighted.median(f$depense_totale, na.rm=T, w = f$weight)
  
  if (!is.element('categories', fixed)) {
    if (!is.element('deficit', fixed)) deficit <- spending - revenues
    if (!is.element('revenues', fixed) & !is.element('recettes', fixed)) revenues <- spending - deficit
    if (!is.element('spending', fixed) & !is.element('depenses', fixed)) spending <- revenues + deficit
    categories <- categories * sum(categories) / spending
  } else {
    spending <- sum(categories)
    if (is.element('deficit', fixed)) revenues <- spending - deficit
    if (is.element('revenues', fixed) | is.element('recettes', fixed)) deficit <- spending - revenues
    if (is.element('spending', fixed) | is.element('depenses', fixed)) print('impossible combination')
  }
  
  return(list('total'= c('deficit'=round(100 * deficit / pib2016_implicite, 1), 'revenues'=round(revenues), 'spending'=round(spending)), 'variations'=round(100 * (categories / depenses_2016 - 1), 1)))
}

decrit(f$deficit, weights = f$weight)
decrit(f$recette_totale, weights = f$weight)
decrit(f$depense_totale, weights = f$weight)
# This is what happens when one fixed two amounts at their preferred median, and then adjust the others (to respect the relative median amounts).
# 4 amounts but 2 degrees of freedom as spending=sum(categories) and deficit = spending - revenues. 5 = 4 choose 2 - 1 possibilities because fixing categories is incompatible with fixing spending
adjust_from_median(c('categories', 'deficit'))
adjust_from_median(c('revenues', 'deficit'))
adjust_from_median(c('categories', 'revenues'))
adjust_from_median(c('spending', 'deficit'))
adjust_from_median(c('revenues', 'spending'))
# => No pair within the previous cases has equal totals! deficit in 2.6-3%, revenues in 988-998 G, spending in 1052-1062 G

t_transferts_inter_a <- t[which(t$transferts_inter_a!=""),c('transferts_inter_a', 'transferts_inter_info', 'sexe', 'age', 'revenu', 'taille_foyer', 'diplome', 'Age', 'Diplome', 'diplome4', 'statut_emploi', 'interet_politique', 'gauche_droite', 'Gauche_droite', 'exclu', 'region', 'csp', 'taille_agglo', 'revdisp', 'rev_tot', 'weight', 'qualite', 'Region')]
t_depenses <- t[t$vague==3,c('transferts_inter_a', 'transferts_inter_info', 'sexe', 'age', 'revenu', 'taille_foyer', 'diplome', 'Age', 'Diplome', 'diplome4', 'statut_emploi', 'interet_politique', 'gauche_droite', 'Gauche_droite', 'exclu', 'region', 'csp', 'taille_agglo', 'revdisp', 'rev_tot', 'weight', 'qualite', 'Region', names(t)[356:383])]
save(t_transferts_inter_a, t_depenses, file="/var/www/beliefs_climate_policies/code/p_data.RData")


##### Champs libres #####
length(which(f$question_simple!="")) # 433: 15 min de lecture
length(which(f$question_politique!="")) # 438 (+ 433 = 871)
length(which(f$champ_libre!="")) # 278
for (i in 1:length(f$champ_libre)) {  if (f$question_simple[i]!="") print(as.character(f$question_simple[i])) }
# Sujets qui reviennent souvent: êtes-vous heureux, pourquoi, pourquoi pas ? Satisfait de la politique du gouvernement ? et autres préoccupations politiques
# ex: "Revoteriez vous de la même façon à la présidentielle " "que pensez-vous de la France ? qui souhaite encore de l'Europe ?  Que fait le gouvernement face aux pauvres ?"
# "Avez vous de bons contacts avec vos voisins ?" "les français privilégient-ils plus le sport à la scolarité ?" "Pouvez-vous acheter tout ce que vous désirez" "A quoi rêvez vous ? "
# "Vous considérez-vous comme pauvre ? " "qu'aimeriez -vous dans la vie?" "Etes-vous prêt à faire des efforts pour que la France retrouve le plein emploi ?" 
# "Etes vous satisfait de la qualité des émissions TV et de leur programmation" "aimez vous aider les autres " "Pourquoi n'êtes vous pas riche ?" 
# "Pour ou contre le fait d'utiliser les professeurs durant les vacances scolaires pour leur faire faire du soutien aux élèves en difficulté ?" "que faire pour positiver tous les jours ?"
for (i in 1:length(f$champ_libre)) {  if (f$question_politique[i]!="") print(as.character(f$question_politique[i])) }
# Beaucoup de questions ouvertes pour connaître les priorités, les valeurs, les raisons du vote. Beaucoup de mentions de Macron ou du président en général, de "je ne c pa".
# ex: "Pourquoi avez vous voté Macron" "pourquoi allez vous voter pour tel candidat? votre choix est-il 1)volontaire 2) par défaut 3) par réaction" 
# "Comment s'appellerait votre parti idéal ?" "Vous interressez-vous à la politique/aux actions des hommes politiques en dehors des périodes électorales? Les qualificatifs  \"gauche\" et \"droite\" sont elles déterminantes pour vous dans l'adhésion à un candidat/ programme électoral ? "
# "Me sentant de plus en plus exclue de la politique, je n'ai pas de réelle question à poser actuellement ..." "avez vous refflechi avant de repondre?"
# "Pensez-vous que votre situation personnelle évolue en bien ou en mal selon les résultats des élections ? Si vous ne votez pas ou votez blanc, quelle en est la raison ? Pensez-vous que le résultat des élections puisse changer quelque chose dans votre vie ?"
for (i in 1:length(f$champ_libre)) {  if (f$champ_libre[i]!="") print(as.character(f$champ_libre[i])) }
# à part les bravos et c'est trop compliqué, beaucoup veulent plus de redistribution, que les riches paient et les élus perdent leurs privilèges, opposition aux taxes sur la classe moyenne
# ex: "je n'y connais pas grand chose donc j'ai fais au pif" "Sondage intéressant mais un peu compliqué à comprendre" 
# "Je pense que toutes décisions seront prisent en dehors de tous sondages ou référendum ,seul les plus riches donnerons leurs avis et auront satisfaction "
# "c'est fou ce que les gens sont devenus cons" "Sondage assez dur à comprendre. Les réponses ne sont pas faciles à faire. Le montant de mes revenus est faux, question mal comprise. Le montant est de 1700€"
# "Il est très difficile de raisonner en chiffre alors que bien sûr j'ai des idées sur certaines priorités" "Je n'avais pas trop d'opinions mais je trouvais les questions très intéressantes."

##### Victoire Coupe du Monde #####
decrit(f$victoire_vu, weights = f$weight)
decrit(f$victoire_fete, weights = f$weight)
decrit(f$victoire_rue, weights = f$weight)
decrit(f$victoire_entendu, weights = f$weight)
decrit(f$victoire_klaxons, weights = f$weight)
decrit(f$victoire_soutenu, weights = f$weight)
decrit(f$victoire_chant, weights = f$weight)
decrit(f$victoire_bonne_ambiance, weights = f$weight)
decrit(f$victoire_bon_moment, weights = f$weight) # TODO: travail?
decrit(f$victoire_bu, weights = f$weight)
decrit(f$victoire_bu_choix, weights = f$weight)
decrit(f$victoire_joueur, weights = f$weight)
