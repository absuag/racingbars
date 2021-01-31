#global.R


GENERATE_BARRACE <- function(input,output,session,data,
                             varTime = "year",varCat = "country_name",varValue = "value",
                             varTitle  = "Racing Bars",varSubTitle = "Racing Bars",varCaption = "Racing Bars",varOut = "gif",frames = 200,persecond = 30){  
  require(tidyverse)
  require(gganimate)
  require(gifski)
  require(data.table)
  show("ptm")
  shinyjs::addClass(selector = "body", class = "sidebar-collapse")
  disable("chartDownload")
  output$chartDisplay <- renderImage({
    list(src = "loading.gif",
         contentType = 'image/gif',
         width = 800,
         height = 600
         # alt = "This is alternate text"
    )
  }, deleteFile = FALSE)
  
  gdp_tidy <- data
  gdp_formatted <- gdp_tidy %>%
    group_by_at(all_of(varTime)) %>%
    # The * 1 makes it possible to have non-integer ranks while sliding
    mutate(rank = rank(-get(varValue)),
           Value_rel = get(varValue)/get(varValue)[rank==1],
           Value_lbl = paste0(" ",round(get(varValue)/1e9))) %>%
    group_by_at(all_of(varCat)) %>% 
    filter(rank <=10) %>%
    ungroup()
  
  # Animation
  
  
  anim <- ggplot(gdp_formatted, aes(rank, group = get(varCat), 
                                    fill = as.factor(get(varCat)), color = as.factor(get(varCat)))) +
    geom_tile(aes(y = get(varValue)/2,
                  height = get(varValue),
                  width = 0.9), alpha = 0.8, color = NA) +
    geom_text(aes(y = 0, label = paste(get(varCat), " ")), vjust = 0.2, hjust = 1) +
    geom_text(aes(y=get(varValue),label = Value_lbl, hjust=0)) +
    coord_flip(clip = "off", expand = FALSE) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_reverse() +
    guides(color = FALSE, fill = FALSE) +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.grid.major.x = element_line( size=.1, color="grey" ),
          panel.grid.minor.x = element_line( size=.1, color="grey" ),
          plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
          plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
          plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
          plot.background=element_blank(),
          plot.margin = margin(2,2, 2, 4, "cm")) +
    transition_states(get(varTime), transition_length = 4, state_length = 1, wrap = FALSE) +
    view_follow(fixed_x = TRUE)  +
    labs(title = varTitle,  
         subtitle  = varSubTitle,
         caption  = varCaption) 
  
  # For GIF
  if(varOut == "gif"){
  print("generating gif")
  animate(anim, frames, fps = persecond,  width = 1200, height = 1000, 
          renderer = gifski_renderer("www/gganim.gif"), end_pause = 15, start_pause =  15) 
  }
  # For MP4
  if(varOut == "mp4"){
  print("generating gif")
  animate(anim, frames, fps = persecond,  width = 1200, height = 1000, 
          renderer = gifski_renderer("www/gganim.gif"), end_pause = 15, start_pause =  15) 
  print("generating mp4")
  animate(anim, frames, fps = persecond,  width = 1200, height = 1000, 
          renderer = ffmpeg_renderer()) -> for_mp4
  
  anim_save("www/animation.mp4", animation = for_mp4)
  }
  output$chartDisplay <- renderImage({
    list(src = "www/gganim.gif",
         contentType = 'image/gif',
         width = 800,
         height = 600
         # alt = "This is alternate text"
    )
  }, deleteFile = FALSE)
  enable("chartDownload")
  hide("ptm")
}
  