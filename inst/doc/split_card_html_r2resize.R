## ----eval=FALSE,echo=TRUE-----------------------------------------------------
#  library(r2resize)

## ----eval=FALSE,echo=TRUE-----------------------------------------------------
#  #basic attributes
#  r2resize::splitCard(
#    "Sample text",
#    "Sample text 2"
#  )
#  
#  #specify background color for each side
#  r2resize::splitCard(
#    "Sample r2symbols 1",
#    "Sample nextGen 1",
#    bg.right.color = "white",
#    bg.left.color = "lightgray",
#    position = "vertical"
#  )
#  
#  #add the split card attribute on vertical
#  r2resize::splitCard(
#    "Sample shinyStorePlus",
#    "Sample nextGen 1",
#    bg.right.color = "white",
#    bg.left.color = "lightgray",
#    border.color = "black",
#    text.left.color = "black",
#    text.right.color = "black",
#    position = "vertical"
#  )
#  
#  
#  # specify split card on the horizontal position
#  r2resize::splitCard(
#    "Sample sciRmdTheme 1",
#    "Sample nextGen 1",
#    bg.right.color = "white",
#    bg.left.color = "lightgray",
#    border.color = "gray",
#    text.left.color = "black",
#    text.right.color = "black",
#    position = "horizontal"
#  )

