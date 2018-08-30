#' SFI visualization theme
#'
#' The Santa Fe Institute Law as Data visualization theme
#' @param base_size The font size
#' @param base_family The font
#' @param nomargin The margin
#' @param fc The font color
#' @param gM Major grid
#' @param gm Minor grid
#' @param gc Color of grid
#' @param gl Line type
#' @param boxes Box around plot
#' @param bc Background color
#' @param pc Panel background color
#' @param lp Legend position
#' @param lt = 0.8, lj = NULL, lba = FALSE, la = NULL, lfb = "plain", 
#' @param axis Axis angle
#' @param ld Legen direction
#' @param lj legend justification
#' @param lba legend background
#' @param lfb legend font bold
#' @param lkh legend height
#' @param lkw legend width
#' @param legend_height numeric height
#' @param legend_width numeric width
#' @return A ggplot2 theme
#' @import ggthemes
#' @import ggplot2
#' @import Hmisc
#' @export

custome_theme <- function(base_size = 12, # size of font
                      base_family = "CMU Bright", # font famly 
                      nomargin = FALSE, #margins for plot 
                      fc = "black", # font color
                      gM = TRUE, # major grid
                      gm = FALSE, # minor grid
                      gc = "grey", # color of grid
                      gl = "dashed", # line type
                      boxes = FALSE, # render box around the plot
                      bc = "white", # background color
                      pc = "transparent", # panel background color
                      lp = "right", # legend position
                      ld = NULL, # legend direction
                      lt = 0.8, #  0.8 was the default - can be adjusted now
                      lj = NULL, # legend.justification to accompany a legend position
                      lba = FALSE, # use this in tandem with la (legend alpha) to make legend transparent when in plot
                      la = NULL, # controls the transparency of the legend (useful for legends inside the plot)
                      lfb = 'plain', # default plain, but can be used to make bold 
                      lkh = FALSE, # legend key height - boolean to inidcate if user wants to edit legend height. ifso, use legend_height (numeric)
                      lkw = FALSE, # # legend key width - boolean to inidcate if user wants to edit legend width. ifso, use legend_width (numeric)
                      lkt = NULL, # type of geom for legend
                      legend_height = NULL, # numeric input to control height of legend
                      legend_width = NULL, # numeric input to control width of legen
                      axis = 1) # axis angle
{
  ## DRY
  tc <- ifelse(pc == "transparent", bc, pc)  # 'transparent' color
  
  ## default colors, font and legend position
  res <- theme(text = element_text(family = base_family),
               plot.background = element_rect(fill = bc, colour = NA),
               panel.grid = element_line(colour = gc,
                                         size = 0.2, linetype = gl),
               panel.grid.minor = element_line(size = 0.1),
               axis.ticks = element_line(colour = gc,
                                         size = 0.2),
               plot.title = element_text(colour = fc,
                                         face = "plain",
                                         size = base_size * 1.2),
               axis.text = element_text(colour = fc,
                                        face = "plain", size = base_size * 0.8),
               legend.text = element_text(colour = fc, 
                                          face = lfb,
                                          size = base_size * lt),
               legend.title = element_text(colour = fc,
                                           face = "plain",
                                           size = base_size),
               axis.title.x = element_text(colour = fc,
                                           face = "plain",
                                           size = base_size),
               strip.text.x = element_text(colour = fc,
                                           face = "plain",
                                           size = base_size),
               axis.title.y = element_text(colour = fc,
                                           face = "plain",
                                           size = base_size,
                                           angle = 90),
               strip.text.y = element_text(colour = fc,
                                           face = "plain",
                                           size = base_size,
                                           angle = -90),
               legend.key = element_rect(colour = gc, fill = "transparent"),
               strip.background = element_rect(colour = gc,
                                               fill = "transparent"),
               panel.border = element_rect(fill = NA, colour = gc),
               panel.background = element_rect(fill = pc, colour = gc),
               legend.position = lp,
               legend.direction = ld)
  
  # legend height condition
  if(lkh) {
    res <- res + theme(legend.key.height = unit(legend_height, lkt))
  }
  
  # legend width condition
  if(lkw) {
    res <- res + theme(legend.key.width = unit(legend_width, lkt))
  }
  
  ## disable box(es) around the plot
  
  if (!isTRUE(boxes)) {
    res <- res + theme(legend.key = element_rect(colour = "transparent",
                                                 fill = "transparent"),
                       strip.background = element_rect(colour = "transparent",
                                                       
                                                       fill = "transparent"),
                       panel.border = element_rect(fill = NA,
                                                   colour = tc),
                       panel.background = element_rect(fill = pc,
                                                       colour = tc))
  }
  
  ## disable grid
  if (!isTRUE(gM)) {
    res <- res + theme(panel.grid = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank())
  }
  ## disable minor grid
  if (!isTRUE(gm)){
    res <- res + theme(panel.grid.minor = element_blank())
  }
  ## margin
  if (nomargin) {
    res <- res + theme(plot.margin = ggplot2::unit(c(0.1, 0.1, 0.1, 0), "lines"))
  }
  
  
  if(lba){
    res <- res + theme(legend.background = element_rect(fill = alpha('white', la)))
  }
  ## axis angle (TODO: DRY with ifelse in the default color etc. section)
  if (axis == 0) {
    res <- res + theme(axis.text.y = element_text(colour = fc,
                                                  family = base_family,
                                                  face = "plain",
                                                  size = base_size *  0.8,
                                                  angle = 90))
  }
  if (axis == 2) {
    res <- res + theme(axis.text.x = element_text(colour = fc,
                                                  family = base_family,
                                                  face = "plain",
                                                  size = base_size *  0.8,
                                                  angle = 90,
                                                  hjust = 1))
  }
  if (axis == 3) {
    res <- res + theme(axis.text.y = element_text(colour = fc,
                                                  family = base_family,
                                                  face = "plain",
                                                  size = base_size * 0.8,
                                                  angle = 90),
                       axis.text.x = element_text(colour = fc,
                                                  family = base_family,
                                                  face = "plain",
                                                  size = base_size * 0.8,
                                                  angle = 90,
                                                  hjust = 1))
  }
  
  
  res
}


##########
# Specs
##########

# - SIZE and PROPORTIONS
# --- a) Page size: 6 in x 9 in
# --- b) Text block size: 4 in x 7.25 in
# --- c) Graphics: should fit snuggly in text block
# --- d) Page size: 6 in x 9 in
# --- e) Vector

# - TEXT
# --- a) Font: CMU Bright
# --- b) Variables: EB Garamond italics, if possible
# --- c) Word clouds:  CMU bright, potentially with differing degrees of boldness to provide visual cues. 

# - LINES
# --- a) Line weight: 0.5pt (at 100%)
# --- b) Thinner lines: use tint of black instead of small lines.

# - COLOR & GRADIENTS
# --- a) All graphics should be grayscale 
# --- b) To distinguish elements in a grayscale environment, where possible, we’d prefer to use tints rather than patterns. 

# - LOOK & FEEL (FROM PLOTS)
# --- a) Figures should not have boxs aronud them
# --- b) 3D graphics should have perspective lines to establish a 3D sp




