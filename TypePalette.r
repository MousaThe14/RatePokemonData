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
printPNG = function(filename, dataset,width = 1920, height = 1080, units = "px", dpi = 300){

  png(filename = filename,
      width = width,
      height = height,
      res = dpi)
      dataset
      dev.off()
}


### Print SVG ###
printSVG = function(filename,dataset, width = 1920, height = 1080, units = "px", dpi = 300){

  svg(filename = filename,
      width = width,
      height = height,
      res = dpi)
  dataset
  dev.off()
}


##### Color Palattes #####

### Type Colors ###

typeColorPalette <- c("Grass" = "#3FA129",
						"Water" = "#2980EF",
						"Fire" = "#E62829",
						"Electric" = "#FAC000",
						"Normal" = "#9FA19F",
						"Fighting" = "#FF8000",
						"Flying" = "#81B9EF",
						"Rock" = "#AFA981",
						"Ground" = "#915121",
						"Steel" = "#60A1B8",
						"Poison" = "#9141CB",
						"Bug" = "#91A119",
						"Psychic" = "#EF4179",
						"Ghost" = "#704170",
						"Dark" = "#624D4E",
						"Ice" = "#3DCEF3",
						"Dragon" = "#5060E1",
						"Fairy" = "#EF70EF")


typeColorPaletteD <- c("Grass" = "#29691B",
                      "Water" = "#1B539B",
                      "Fire" = "#961A1B",
                      "Electric" = "#A37D00",
                      "Normal" = "#676967",
                      "Fighting" = "#A65300",
                      "Flying" = "#54789B",
                      "Rock" = "#726E54",
                      "Ground" = "#5E3515",
                      "Steel" = "#3E6978",
                      "Poison" = "#5E2A84",
                      "Bug" = "#5E6910",
                      "Psychic" = "#9B2A4F",
                      "Ghost" = "#492A49",
                      "Dark" = "#5E3515",
                      "Ice" = "#28869E",
                      "Dragon" = "#343E92",
                      "Fairy" = "#9B499B")



typeColorPaletteL <- c("Grass" = "#82C274",
                      "Water" = "#74ACF5",
                      "Fire" = "#EF7374",
                      "Electric" = "#FCD659",
                      "Normal" = "#C1C2C1",
                      "Fighting" = "#FFAC59",
                      "Flying" = "#ADD2F5",
                      "Rock" = "#CBC7AD",
                      "Ground" = "#B88E6F",
                      "Steel" = "#98C2D1",
                      "Poison" = "#B884DD",
                      "Bug" = "#B8C26A",
                      "Psychic" = "#F584A8",
                      "Ghost" = "#A284A2",
                      "Dark" = "#998B8C",
                      "Ice" = "#3DCEF3",
                      "Dragon" = "#8D98EC",
                      "Fairy" = "#F5A2F5")

# "Stellar" = "#44628D"

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


categoryColorPaletteContest <- c("Cuteness" = "#D2458D",
                          "Coolness" = "#D42C04",
                          "Beauty" = "#1E93C0",
                          "Artificiality" = "#0E6929",
                          "Realism" = "#CCA100",
                          "Humanoid" = "tan3",
                          "Fantasy" = "#892D5C",
                          "Complexity" = "grey71",
                          "Popularity" = "#DEC240") 



typeColorPaletteCustom <- c("Grass" = "forestgreen",
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

