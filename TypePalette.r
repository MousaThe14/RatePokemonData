### Useful Codes ###

##### Maths #####

### Get Mode ###
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

### Get Population Mean ###
sd.p =  function(x){
  sd(x) * sqrt((length(x)-1)/length(x))
} 




##### Useful Functions #####

### Print PNG ###
printPNG = function(filename, width = 1920, height = 1080, units = px, dpi = 300){
  png(filename = filename,
      width = width,
      height = height,
      res = dpi)
  dev.off()
}


### Print SVG ###
printSVG = function(filename, width = 1920, height = 1080, units = px, dpi = 300){
  svg(filename = filename,
      width = width,
      height = height,
      res = dpi)
  dev.off()
}


##### Color Palattes #####

### Type Colors ###

typeColorPalette <- c("Grass" = "forestgreen",
						"Water" = "dodgerblue2",
						"Fire" = "darkorange2",
						"Electric" = "yellow",
						"Normal" = "cornsilk3",
						"Fighting" = "brown3",
						"Flying" = "lightskyblue1",
						"Rock" = "tan",
						"Ground" = "tan4",
						"Steel" = "slategray3",
						"Poison" = "purple",
						"Bug" = "olivedrab3",
						"Psychic" = "violetred1",
						"Ghost" = "darkorchid4",
						"Dark" = "grey0",
						"Ice" = "cyan",
						"Dragon" = "slateblue4",
						"Fairy" = "plum")


### Category Palette ###

categoryColorPalette <- c("Cuteness" = "hotpink",
							"Coolness" = "orange",
							"Beauty" = "blue",
							"Artificiality" = "green",
							"Realism" = "wheat3",
							"Humanoid" = "tan3",
							"Fantasy" = "violet",
							"Complexity" = "grey71",
							"Popularity" = "gold1") 
