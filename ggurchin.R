#### Description ####
# Create a urchin plot, a representation designed to illustrate the relation of multiple objects to one. 
# It was originally designed to represent the positive and negative associations of several phytoplankton taxa with a single one,
# but its use can be much broader.

#### Arguments ####
# df: a data frame 
# pos: character, the name of the column (in character format) indicating position on the graph, "int" for interior and "ext" for exterior
# id: character, the name of the column (in character format) identifying each multiple objects that are in relation with a single one
# alpha: character, the name of the column (in numeric format) indicating the transparency of the rectangles 
# width: character, the name of the column (in numeric format) indicating the width of graph elements for each id
# group_name: character, the name of the column (in character format) identifying each group to represent in the triangles
# group_order_int: character vector, the order of the group ploted in the interior triangles (from interior to exterior)
# group_order_ext: character vector, the order of the group ploted in the exterior triangles (from interior to exterior)
# annot: character, the name of the column (in character format) indicating what to write in the rectangle
# sep: character, the name of the column (in character format) indicating each group of id to distinguish by lines 
# full: character vector, the name of the position to plot ("int", "ext" or c("int", "ext"))
# rayonint: numeric vector, the radius of the inner circles in which the "int" elements will be placed
# rayonext: numeric vector, the radius of the outer circles in which the "ext" elements will be placed
# slice: numeric values (minimum 2), the number of angles to cut to write annotation about groups values (like a pizza slice)
# group_color_int: character vector, the name or hexadecimal color code of the group indicated in 'group_name'
# group_guide_int: numeric vector, the values (in %) of guide indicating the interior group ('group_name') proportion
# group_guide_ext: numeric vector, the values (in %) of guide indicating the exterior group ('group_name') proportion
# group_guide_color: color, the color of the group_guide, interior and exterior
# group_guide_text_size: numeric value, the size of group_guide annotation
# id_text_size: numeric value, the size of id text
# annot_text_size: numeric value, the size of annotation text
# ylim: numeric character, the y limits of the plot
# xlim: numeric character, the x limits of the plot

#### Details ####
# If you do not have separations or groups, just create a sep or a group_name and group_value columns with a single value and use it as the group column.
# Rayonext still be needed even if you are just ploting the interior part. 
# Simplicity improvments are coming soon.

#### Value ####
# A ggplot plot object

ggurchin <- function(df,
                     pos,
                     full,
                     id,
                     alpha,
                     width,
                     group_name ,
                     group_order_int = NA,
                     group_order_ext = NA,
                     group_value,
                     annot,
                     sep,
                     rayonint= c(1, 4),
                     rayonext= c(6, 7),
                     slice = 2,
                     group_color_int = NA,
                     group_color_ext = NA,
                     group_guide_int = NA,
                     group_guide_ext = NA,
                     group_guide_color ="grey60",
                     group_guide_text_size =4,
                     id_text_size =6,
                     annot_text_size =6,
                     ylim,
                     xlim) {
  dforder <- df %>%
    select(all_of(id)) %>%
    distinct() %>%
    mutate(order= row_number())
  dffull <- df %>%
    filter(.data[[pos]] %in% full)
  dfnotfull <- df %>%
    filter(!(.data[[pos]] %in% full) & .data[[id]] %in% dffull[[id]])
  df <- rbind(dffull, dfnotfull) %>%
    distinct() %>%
    left_join(dforder,
              by= id) %>%
    arrange(order) %>%
    select(-order)
  
  space <- exp(1.25184 + -0.98022*log(nrow(dforder)))
  dfangles <- df %>%
    filter(.data[[pos]] %in% full) %>%
    select(all_of(c(id, width))) %>%
    distinct()
  angles_a <- seq(from = pi/2, to = pi/2 - 2*pi, length.out = nrow(dfangles) + slice)
  head_descr <- head(angles_a, trunc(slice/2))
  tail_descr <- tail(angles_a, slice-trunc(slice/2))
  dfangles <- dfangles %>%
    cbind(a= angles_a[!(angles_a %in% c(head_descr, tail_descr))]) %>%
    mutate(b= ifelse(a == first(a),
                     min(head_descr) %% pi - space,
                     lag(a) - space),
           b= b-(b-a)*(1-.data[[width]]/max(.data[[width]])),
           c= ifelse(a == last(a),
                     max(tail_descr) + space,
                     lead(a) + space),
           c= c+(a-c)*(1-.data[[width]]/max(.data[[width]])),
           d= a) %>% 
    select(-all_of(width))
  
  dfposgroup <- data.frame()
  for (i in c("int", "ext")) {
    dfpos <- df %>%
      filter(.data[[pos]] == i) %>% 
      left_join(dfangles, by= id) %>%
      pivot_longer(c(a, b, c, d)) %>%
      mutate(rayon= ifelse(name == "a" | name == "d",
                           ifelse(i == "int", 
                                  rayonint[1],
                                  rayonext[2]),
                           ifelse(i == "int",
                                  rayonint[2],
                                  rayonext[1])),
             x= rayon * cos(value),
             y= rayon * sin(value))
    dfposuniq <- dfpos %>%
      mutate(n= n(),
             .by= all_of(id)) %>%
      filter(n == 4) %>%
      select(-n)
    dfposgroup <- rbind(dfposgroup, dfposuniq)
    for (j in unique(dfpos[[id]])[!(unique(dfpos[[id]]) %in% unique(dfposuniq[[id]]))]) {
      if (i == "int") {
        group_order <- group_order_int
      } else {
        group_order <- group_order_ext
      }
      dfgrouporder <- data.frame(group= group_order,
                                 order= seq(1, length(group_order)))
      colnames(dfgrouporder)[1] <- group_name
      dfid <- dfpos %>%
        filter(.data[[id]] == j) %>%
        left_join(dfgrouporder,
                  by= group_name) %>%
        arrange(order, name) %>%
        select(-order)
      dfidgroup <- data.frame()
      for (k in 1:length(unique(dfid[[group_name]]))) {
        if (k == 1) {
          dfidgroup <- rbind(dfidgroup,
                             dfid %>%
                               filter(.data[[group_name]] == unique(dfid[[group_name]])[k]) %>%
                               mutate(x= recode_values(name,
                                                       "a" ~ x,
                                                       "b" ~ seq(x[name == "a"],
                                                                 x[name == "b"], 
                                                                 length.out= 1000)[unique(.data[[group_value]])*1000],
                                                       "c" ~ seq(x[name == "d"],
                                                                 x[name == "c"], 
                                                                 length.out= 1000)[unique(.data[[group_value]])*1000],
                                                       "d" ~ x),
                                      y= recode_values(name,
                                                       "a" ~ y,
                                                       "b" ~ seq(y[name == "a"],
                                                                 y[name == "b"], 
                                                                 length.out= 1000)[unique(.data[[group_value]])*1000],
                                                       "c" ~ seq(y[name == "d"],
                                                                 y[name == "c"], 
                                                                 length.out= 1000)[unique(.data[[group_value]])*1000],
                                                       "d" ~ y)))
        } else {
          if (k == length(unique(dfid[[group_name]]))) {
            dfidgroup <- rbind(dfidgroup,
                               dfid %>%
                                 filter(.data[[group_name]] == unique(dfid[[group_name]])[k]) %>%
                                 mutate(x= recode_values(name,
                                                         "a" ~ dfidgroup$x[dfidgroup[[group_name]] == unique(dfid[[group_name]])[k-1] & dfidgroup$name == "b"],
                                                         "b" ~ x,
                                                         "c" ~ x,
                                                         "d" ~ dfidgroup$x[dfidgroup[[group_name]] == unique(dfid[[group_name]])[k-1] & dfidgroup$name == "c"]),
                                        y= recode_values(name,
                                                         "a" ~ dfidgroup$y[dfidgroup[[group_name]] == unique(dfid[[group_name]])[k-1] & dfidgroup$name == "b"],
                                                         "b" ~ y,
                                                         "c" ~ y,
                                                         "d" ~ dfidgroup$y[dfidgroup[[group_name]] == unique(dfid[[group_name]])[k-1] & dfidgroup$name == "c"])))
          } else {
            dfidgroup <- rbind(dfidgroup,
                               dfid %>%
                                 filter(.data[[group_name]] == unique(dfid[[group_name]])[k]) %>%
                                 mutate(x= recode_values(name,
                                                         "b" ~ seq(x[name == "a"],
                                                                   x[name == "b"], 
                                                                   length.out= 1000)[(unique(.data[[group_value]])+
                                                                                        unique(dfidgroup[[group_value]][dfidgroup[[group_name]] == unique(dfid[[group_name]])[k-1]]))*1000],
                                                         "c" ~ seq(x[name == "d"],
                                                                   x[name == "c"], 
                                                                   length.out= 1000)[(unique(.data[[group_value]])+
                                                                                        unique(dfidgroup[[group_value]][dfidgroup[[group_name]] == unique(dfid[[group_name]])[k-1]]))*1000],
                                                         "a" ~ dfidgroup$x[dfidgroup[[group_name]] == unique(dfid[[group_name]])[k-1] & dfidgroup$name == "b"],
                                                         "d" ~ dfidgroup$x[dfidgroup[[group_name]] == unique(dfid[[group_name]])[k-1] & dfidgroup$name == "c"]),
                                        y= recode_values(name,
                                                         "b" ~ seq(y[name == "a"],
                                                                   y[name == "b"], 
                                                                   length.out= 1000)[(unique(.data[[group_value]])+
                                                                                        unique(dfidgroup[[group_value]][dfidgroup[[group_name]] == unique(dfid[[group_name]])[k-1]]))*1000],
                                                         "c" ~ seq(y[name == "d"],
                                                                   y[name == "c"], 
                                                                   length.out= 1000)[(unique(.data[[group_value]])+
                                                                                        unique(dfidgroup[[group_value]][dfidgroup[[group_name]] == unique(dfid[[group_name]])[k-1]]))*1000],
                                                         "a" ~ dfidgroup$y[dfidgroup[[group_name]] == unique(dfid[[group_name]])[k-1] & dfidgroup$name == "b"],
                                                         "d" ~ dfidgroup$y[dfidgroup[[group_name]] == unique(dfid[[group_name]])[k-1] & dfidgroup$name == "c"])))
          }
        }
      }
      dfposgroup <- rbind(dfposgroup, dfidgroup)
    }
  }
  dfposgroup <- dfposgroup %>%
    left_join(dforder,
              by= id) %>%
    arrange(order) %>%
    select(-order)
  
  #### Labels ####
  ##### Id #####
  dflabel <- dfangles %>%
    select(all_of(id), a) %>%
    cbind(rayon= max(rayonext)) %>%
    mutate(x= rayon * cos(a),
           y= rayon * sin(a),
           a= a*180/pi,
           a= if_else(a < -90,
                      a - 180,
                      a))
  ##### Annot #####
  dfannot <- dfangles %>%
    select(all_of(id), a) %>%
    left_join(df %>%
                filter(.data[[pos]] == "int") %>%
                select(all_of(c(id, alpha, annot))) %>%
                distinct(),
              by= id) %>%
    rename(annotint= .data[[annot]],
           alphaint= .data[[alpha]]) %>%
    left_join(df %>%
                filter(.data[[pos]] == "ext") %>%
                select(all_of(c(id, alpha, annot))) %>%
                distinct(),
              by= id) %>%
    rename(annotext= .data[[annot]],
           alphaext= .data[[alpha]]) %>%
    pivot_longer(c(annotint, annotext)) %>%
    mutate(angle= a*180/pi,
           angle= if_else(angle < -90,
                          angle - 180,
                          angle),
           alpha= ifelse(name == "annotint",
                         alphaint,
                         alphaext)) %>%
    select(-c(alphaint, alphaext)) %>%
    filter(!(is.na(alpha))) %>%
    mutate(rayon= ifelse(name== "annotint",
                         rayonint[2]+(rayonext[1]-rayonint[2])/4,
                         rayonext[1]-(rayonext[1]-rayonint[2])/4),
           x= rayon * cos(a),
           y= rayon * sin(a))
  
  #### Guides ####
  ##### Circles #####
  dfgroup_guide <- data.frame(proport= rep(c(group_guide_int, group_guide_ext, NA), length(angles_a)),
                              pos= rep(c(rep("int", length(group_guide_int)), 
                                         rep("ext", length(group_guide_ext)),
                                         "between"), length(angles_a)),
                              angles= rep(angles_a, each= length(c(group_guide_int, group_guide_ext, "between")))) %>%
    mutate(rayon= recode_values(pos,
                                "int" ~ rayonint[2]-(rayonint[2]-rayonint[1])*proport/100,
                                "ext" ~ rayonext[2]-(rayonext[2]-rayonext[1])*(100-proport)/100,
                                "between" ~ rayonint[2]+ (rayonext[1]-rayonint[2])/2),
           x= rayon * cos(angles),
           y= rayon * sin(angles))
    
  dfgroup_annot <- dfgroup_guide %>%
    select(proport, pos, rayon) %>%
    distinct() %>%
    filter(pos != "between") %>%
    cbind(angles= pi/2) %>%
    mutate(x= rayon * cos(angles),
           y= rayon * sin(angles),
           label= paste(proport, "%", sep= ""))
  ##### Guidelines #####
  dfguides <- dfposgroup %>%
    filter(name == "a") %>%
    select(all_of(c(id, pos)), value) %>%
    distinct() %>%
    mutate(n= n(),
           .by= all_of(id)) %>%
    filter(n == 1) %>%
    select(-n) %>%
    mutate(rayonguidint= ifelse(pos == "int",
                                rayonint[2]+ (rayonext[1]-rayonint[2])/2,
                                rayonint[1]),
           rayonguidext= ifelse(pos == "int",
                                rayonext[2],
                                rayonint[2]+ (rayonext[1]-rayonint[2])/2)) %>%
    select(-all_of(pos)) %>%
    rename(angles= value) %>%
    pivot_longer(c(rayonguidint, rayonguidext)) %>%
    mutate(x= value * cos(angles),
           y= value* sin(angles)) 
  ##### Separations #####
  dfsep <- dfangles %>%
    select(all_of(id), a) %>%
    left_join(df%>%
                select(all_of(c(id, sep))) %>%
                distinct(),
              by= id) %>%
    mutate(leadsep= lead(.data[[sep]]),
           leada= lead(a))
  dfsep <- dfsep %>%
    mutate(angle= ifelse(.data[[sep]] != leadsep,
                         rowMeans(select(dfsep, c(a, leada))),
                         NA)) %>%
    select(angle, all_of(id)) %>%
    filter(!(is.na(angle))) %>%
    rename(id= .data[[id]]) %>%
    rbind(data.frame(angle= c(min(head_descr), max(tail_descr)),
                     id= c("start", "end"))) %>%
    cbind(rayonint= rayonint[1],
          rayonext= max(c(ylim, xlim))) %>%
    pivot_longer(c(rayonint, rayonext)) %>%
    select(-name) %>%
    mutate(x= value * cos(angle),
           y= value* sin(angle))
  
  
  #### Figure ####
  ##### Color scale #####
  scalecolor <- c(group_color_int, group_color_ext)
  names(scalecolor) <- c(group_order_int, group_order_ext)
  ##### Plot #####
  purchin <- ggplot() +
    geom_polygon(data= subset(dfposgroup, pos == "int"), 
                 aes(x, y, 
                     group= interaction(.data[[id]], .data[[group_name]]), 
                     fill= .data[[group_name]], 
                     color= .data[[group_name]])) +
    geom_polygon(data= subset(dfposgroup, pos == "ext"), 
                 aes(x, y, 
                     group= interaction(.data[[id]], .data[[group_name]]), 
                     fill= .data[[group_name]], 
                     color= .data[[group_name]])) +
    geom_text(data = subset(dflabel, a >= -90),
              aes(label = .data[[id]], x, y, angle = a),
              hjust = 0, size= id_text_size) +
    geom_text(data = subset(dflabel, a < -90),
              aes(label = .data[[id]], x, y, angle = a),
              hjust = 1, size= id_text_size) +
    geom_path(data= subset(dfgroup_guide, pos != "between"), 
              aes(x, y, group= interaction(pos, proport)), 
              size= .1, color= group_guide_color, linetype= 3, linewidth= 1) +
    geom_path(data= subset(dfgroup_guide, pos == "between"), 
              aes(x, y), 
              size= .1, color= "black", linetype= 2, linewidth= 1.5) +
    geom_text(data= dfgroup_annot, 
              aes(label= label, x, y),
              size= group_guide_text_size) +
    geom_path(data= dfguides, 
              aes(x, y, group= .data[[id]]), 
              size= .1, linetype= 3) +
    geom_path(data= dfsep, 
              aes(x, y, group= id),
              linewidth= 1) +
    geom_label(data = subset(dfannot, name == "annotint"),
               aes(label = value, x, y, angle = angle, alpha= abs(alpha)),
               size= annot_text_size, text.color= "white") +
    geom_label(data = subset(dfannot, name == "annotext"),
               aes(label = value, x, y, angle = angle, alpha= alpha),
               size= annot_text_size, text.color= "white") + 
    scale_color_manual(values= scalecolor,
                       breaks= c(group_order_int, rev(group_order_ext))) +
    scale_fill_manual(values= scalecolor,
                      breaks= c(group_order_int, rev(group_order_ext))) +
    scale_alpha(range= c(0.25, 1), guide= "none") +
    ylim(ylim) +
    xlim(xlim) +
    coord_fixed() +
    theme_void()
  return(purchin) 
}