###########
## Setup ##
###########

setwd("~/TidyTuesdays/2021-08-10")
pacman::p_load(DiagrammeR, DiagrammeRsvg, rsvg)
rm(list=ls())


#################################################
## Plot the hierachical division of investment ##
#################################################

p0 <- grViz("
digraph {

  graph[layout=dot, overlap=false, fontname = Helvetica, fontsize=12, ranksep=equally, rankdir=TB, splines=false, labelloc=b, label='How the total infrastructure investment data is split into various sub categories']

  node [shape = plaintext]        
  
  T [label = 'Total Infrastructure Investment']

  ##########################
  ## Basic infrastructure ##
  ##########################
  
  B [label = 'Basic infrastructure']
  B1 [label = 'Conservation\n & Development']
  B2 [label = 'Power']
  B3 [label = 'Sewer\n & Waste']
  B4 [label = 'Transportation']
  B5 [label = 'Water\nSupply']

  B2e [label = '- Electric power \\l- Natural Gas & Petroleum \\l \\l \\l \\l \\l \\l \\l']

  B4e [label = '- Air transport \\l- Highway & Streets \\l- Rail transport \\l- Transit \\l- Water transport \\l- Other Federal \\l- Other Private \\l ']

  edge [color = red, arrowhead = vee]
    T -> B
    B -> {B1, B2, B3, B4, B5}

  edge [color = red, arrowhead = odot]
    B2 -> B2e
    B4 -> B4e



  ###########################
  ## Digital infrastructure ##
  ###########################
    
  D [label = 'Digital infrastructure']
  D1 [label = 'Private\ncommunications\nstructures']
  D2 [label = 'Private\ncommunications\nequipment']
  D3 [label = 'Private\nsoftware']
  D4 [label = 'Private\ncomputers']
  
  edge [color = blue, arrowhead = vee]
    T -> D
    D -> {D1, D2, D3, D4}


  ###########################
  ## Social infrastructure ##
  ###########################

  S [label = 'Social infrastructure']
  S1 [label = 'Education']
  S2 [label = 'Health']
  S3 [label = 'Public\nSafety']

  S1e [label = '- Federal \\l- S&L Higher edu. \\l- S&L K-12 \\l- S&L Libraries \\l- Private \\l- S&L Others \\l \\l \\l']

  S2e [label = '- All federal \\l- Private equipment \\l- Private hospitals \\l- Private Others \\l- S&L equipment \\l- S&L hospitals \\l- S&L Other \\l \\l']

  S3e [label = '- Federal \\l- Private \\l- S&L correction \\l- S&L fire \\l- S&L police \\l \\l \\l \\l']

  edge [color = darkgreen, arrowhead = vee]
    T -> S
    S -> {S1, S2, S3}

  edge [color = darkgreen, arrowhead = odot]
    S1 -> S1e
    S2 -> S2e
    S3 -> S3e

  { rank = same; B; S; D }
  { rank = same; B1; B2; B3; B4; B5; D1; D2; D3; D4; S1; S2; S3}
  { rank = same; B2e; B4e; S1e; S2e; S3e }

}")

p0 %>% 
  export_svg() %>%
  charToRaw() %>%
  rsvg() %>%
  png::writePNG("2021-08-10_flowchart.png")

