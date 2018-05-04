# 2018 Kentucky Derby (144th) - Social Listening Analytics Visualization Demo

# libraries
library(tidyverse)

# read in data
derby <- read_csv("derby_book_2018.csv")
colnames(derby) <- tolower(str_replace_all(colnames(derby), " ", "_"))
colnames(derby) <- str_replace_all(colnames(derby), "_tweets", "")

# reorder horse factor by post position
derby$horse <- factor(derby$horse, levels = c(derby$horse))

# gather tweet volume data into long format and capitalize sentiment tags
derby_clean <- derby %>% 
        gather(positive, negative, neutral, key = "sentiment", value = "tweet_count") %>%
        mutate(sentiment = str_to_title(sentiment))

# x axis helper functions from: https://jcarroll.com.au/2016/06/03/images-as-x-axis-labels-updated/
library(cowplot)

# convert spaces in horse names into line breaks
addline_format <- function(x,...){
        gsub('\\s','\n',x)
}


# plot tweet volume and sentiment by horse (arranged by post position) with coord flip (tall plot)
plot <- ggplot(arrange(derby_clean), aes(x = horse, y = tweet_count, fill = factor(sentiment, levels = c("Positive", "Neutral", "Negative")))) +
        geom_bar(stat = "identity", width=0.9) +
        scale_fill_manual(values = c("#4D7B18", "#C5B4A0", "#B6121B")) +
        theme_classic() +
        coord_flip()+ # tall plot
        xlab("Tweet Volume\n") +
        guides(fill = guide_legend(title = "Sentiment")) +
        theme(axis.text.y = element_text(angle = 0, size = 8, face = "bold"),
              legend.position = c(.93, .8),
              axis.line=element_blank(),
              axis.ticks = element_blank(),
              axis.text.x=element_blank(),
              axis.title = element_blank()) + 
        geom_text(aes(horse, tweets_in_last_week, label = current_odds, fill = NULL, face = "bold"), data = derby, position = position_dodge(width = .9), hjust = -.5) +
        ylab("\nHorse") +
        scale_y_continuous(expand = c(0, 0), limits = c(0, 3250)) #+
        #scale_x_discrete(breaks=unique(derby$horse), 
                         #labels=addline_format(unique(derby$horse))) 

# read in images for x axis
library(magick)
pimage <- axis_canvas(plot, axis = 'y') + 
        draw_image("Firenze Fire.jpg", y = 0, x = 0.5, scale = 1) +
        draw_image("Free Drop Billy.jpg", y = 1.5, scale = 1) +
        draw_image("Promises Fulfilled.jpg", y = 2.5, scale = 1) +
        draw_image("Flameaway.jpg", y = 3.5, scale = 1) +
        draw_image("Audible.jpg", y = 4.5, scale = 1) +
        draw_image("Good Magic.jpg", y = 5.5, scale = 1) +
        draw_image("Justify.jpg", y = 6.5, scale = 1) +
        draw_image("Lone Sailor.jpg", y = 7.5, scale = 1) +
        draw_image("Hofburg.jpg", y = 8.5, scale = 1) +
        draw_image("My Boy Jack.jpg", y = 9.5, scale = 1) +
        draw_image("Bolt d'oro.jpg", y = 10.5, scale = 1) +
        draw_image("Enticed.jpg", y = 11.5, scale = 1) +
        draw_image("Bravazo.jpg", y = 12.5, scale = 1) +
        draw_image("Mendelssohn.jpg", y = 13.5, scale = 1) +
        draw_image("Instilled Regard.jpg", y = 14.5, scale = 1) +
        draw_image("Magnum Moon.jpg", y = 15.5, scale = 1) +
        draw_image("Solomini.jpg", y = 16.5, scale = 1) +
        draw_image("Vino Rosso.jpg", y = 17.5, scale = 1) +
        draw_image("Noble Indy.jpg", y = 18.5, scale = 1) +
        draw_image("Combatant.jpg", y = 19.5, scale = 1) +
        draw_image("Blended Citizen.jpg", y = 20.5, scale = 1)

# overlay jockey silk images
ggdraw(insert_yaxis_grob(plot, pimage, position = "left"))

# wide plot
plot <- ggplot(arrange(derby_clean), aes(x = horse, y = tweet_count, fill = factor(sentiment, levels = c("Positive", "Neutral", "Negative")))) +
         geom_bar(stat = "identity", width=0.9) +
         scale_fill_manual(values = c("#4D7B18", "#C5B4A0", "#B6121B")) +
         theme_classic() +
         ylab("Tweet Volume\n") +
         guides(fill = guide_legend(title = "Tweet\nSentiment")) +
         theme(axis.text.x = element_text(angle = 0, size = 6, face = "bold"),
               legend.position = c(.93, .8),
               legend.text = element_text(size = 8),
               legend.title = element_text(size = 8),
               axis.title.y = element_text(size = 8),
               axis.line=element_blank(),
               axis.ticks = element_blank(),
               axis.text.y=element_blank(),
               axis.title.x = element_blank()) + 
         geom_text(aes(horse, tweets_in_last_week, label = current_odds, fill = NULL), data = derby, position = position_dodge(width = .9), vjust = -.5) +
         xlab("\nHorse") +
         scale_y_continuous(expand = c(0, 0), limits = c(0, 3250)) +
         scale_x_discrete(breaks=unique(derby$horse), 
                          labels=addline_format(unique(derby$horse)))

pimage <- axis_canvas(plot, axis = 'x') + 
         draw_image("Firenze Fire.jpg", x = 0.5, y = 0, scale = 0.9) +
         draw_image("Free Drop Billy.jpg", x = 1.5, scale = 0.9) +
         draw_image("Promises Fulfilled.jpg", x = 2.5, scale = 0.9) +
         draw_image("Flameaway.jpg", x = 3.5, scale = 0.9) +
         draw_image("Audible.jpg", x = 4.5, scale = 0.9) +
         draw_image("Good Magic.jpg", x = 5.5, scale = 0.9) +
         draw_image("Justify.jpg", x = 6.5, scale = 0.9) +
         draw_image("Lone Sailor.jpg", x = 7.5, scale = 0.9) +
         draw_image("Hofburg.jpg", x = 8.5, scale = 0.9) +
         draw_image("My Boy Jack.jpg", x = 9.5, scale = 0.9) +
         draw_image("Bolt d'oro.jpg", x = 10.5, scale = 0.9) +
         draw_image("Enticed.jpg", x = 11.5, scale = 0.9) +
         draw_image("Bravazo.jpg", x = 12.5, scale = 0.9) +
         draw_image("Mendelssohn.jpg", x = 13.5, scale = 0.9) +
         draw_image("Instilled Regard.jpg", x = 14.5, scale = 0.9) +
         draw_image("Magnum Moon.jpg", x = 15.5, scale = 0.9) +
         draw_image("Solomini.jpg", x = 16.5, scale = 0.9) +
         draw_image("Vino Rosso.jpg", x = 17.5, scale = 0.9) +
         draw_image("Noble Indy.jpg", x = 18.5, scale = 0.9) +
         draw_image("Combatant.jpg", x = 19.5, scale = 0.9) +
         draw_image("Blended Citizen.jpg", x = 20.5, scale = 0.9)

png(file="HorsePlot.png",width=3600,height=1200,res=300)

ggdraw(insert_xaxis_grob(plot, pimage, position = "bottom"))

dev.off()








