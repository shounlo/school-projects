library(tidyverse)
library(shiny)
library(httr)
library(jsonlite)
library(purrr)
library(scales)

url = "https://api.nal.usda.gov/fdc/v1/foods/list?dataType=Survey%20%28FNDDS%29&pageSize=200&api_key=bOWSmS4HXQhofECI4cHj4tZfzYVtWpszeybZssGY"

res = GET(url)
fooddata = fromJSON(rawToChar(res$content))

for(i in 2:29){
  url = paste0("https://api.nal.usda.gov/fdc/v1/foods/list?dataType=Survey%20%28FNDDS%29&pageSize=200&pageNumber=", i,"&api_key=bOWSmS4HXQhofECI4cHj4tZfzYVtWpszeybZssGY")
  res = GET(url)
  fooddata = rbind(fooddata, fromJSON(rawToChar(res$content)))
}

dv = 
  cbind(
    c("Total lipid (fat)", "Cholesterol", "Sodium, Na", "Total Carbohydrate","Fiber, total dietary","Sugars, total including NLEA","Protein","Vitamin D (D2 + D3)","Calcium, Ca","Iron, Fe","Potassium, K"),
    c(78, 300, 2300, 275, 28, 50, 50, 20, 1300, 18, 700)
  ) |>
  as.data.frame() |>
  rename("name" = V1, "rdi" = V2) |>
  mutate(rdi = as.numeric(rdi))

fooddata2 = fooddata |>
  unnest(foodNutrients) |>
  select(description, name, amount, unitName) |>
  filter(name %in% c("Total lipid (fat)", 
                     "Cholesterol", 
                     "Sodium, Na",
                     "Total Carbohydrate",
                     "Fiber, total dietary",
                     "Sugars, total including NLEA",
                     "Protein",
                     "Vitamin D (D2 + D3)",
                     "Calcium, Ca",
                     "Iron, Fe",
                     "Potassium, K")) |>
  left_join(dv) |>
  mutate(`DV%` = as.integer((amount / rdi) * 100)) |>
  select(description, name, amount, unitName, `DV%`) |>
  mutate(unitName = str_to_lower(unitName)) |>
  rename("Nutrient" = name, "Unit" = unitName, "Amount" = amount, "DV" = `DV%`) |>
  mutate(Nutrient = factor(Nutrient, levels = unique(Nutrient), labels = c("Protein", "Total Fat", "Total Sugars", "Fiber", "Calcium", "Iron", "Potassium", "Sodium", "Vitamin D", "Cholestrol")))



ui = fluidPage(
  
  titlePanel("FoodData Central FNDDS Nutritional Facts"),
  
  sidebarLayout(
    
    #inputs
    sidebarPanel(
      selectizeInput("food", label = "Please select up to 3 foodstuffs:", choices = NULL, multiple = TRUE, options = list(maxItems = 3)),
      
      numericInput("grams", label = "Please type in the amount of grams of:", value = 100, step = 10),
      
      conditionalPanel(condition = "input.food.length > 1",
                       numericInput("grams2", label = "", value = 100, step = 10)
      ),
      conditionalPanel(condition = "input.food.length > 2",
                       numericInput("grams3", label = "", value = 100, step = 10)
      ),
      
      actionButton("search", "Search")
      
    ),
    
    #outputs
    mainPanel(
      textOutput("foodname"),
      
      tableOutput("table"),
      
      plotOutput("plot")
    )
    
  )
)

server = function(input, output, session) {
  
  updateSelectizeInput(session, 'food', choices = unique(fooddata2$description), server = TRUE)
  
  observeEvent(
    input$food, {
      
      updateNumericInput(session, "grams", label = paste0("Please type in the amount of grams of ", input$food[1], ":"))
      
      updateNumericInput(session, "grams2", label = paste0("Please type in the amount of grams of ", input$food[2], ":"))
      
      updateNumericInput(session, "grams3", label = paste0("Please type in the amount of grams of ", input$food[3], ":"))
    }
  )
  
  foodDT = eventReactive(input$search,
                         if(length(input$food) == 1){
                           fooddata2 |>
                             filter(description %in% input$food) |>
                             mutate(Amount = Amount * (input$grams/100)) |>
                             mutate(DV = DV * (input$grams/100)) |>
                             summarise(.by = Nutrient, Amount = sum(Amount), Unit = min(Unit), DV = sum(DV))
                           
                         } else if(length(input$food) == 2){
                           fooddata2 |>
                             filter(description %in% input$food) |>
                             mutate(Amount = ifelse(description == input$food[1], Amount * (input$grams/100), Amount)) |>
                             mutate(DV = ifelse(description == input$food[1], DV * (input$grams/100), DV)) |>
                             mutate(Amount = ifelse(description == input$food[2], Amount * (input$grams2/100), Amount)) |>
                             mutate(DV = ifelse(description == input$food[2], DV * (input$grams2/100), DV)) |>
                             summarise(.by = Nutrient, Amount = sum(Amount), Unit = min(Unit), DV = sum(DV))
                         } else if(length(input$food == 3)) {
                           fooddata2 |>
                             filter(description %in% input$food) |>
                             mutate(Amount = ifelse(description == input$food[1], Amount * (input$grams/100), Amount)) |>
                             mutate(DV = ifelse(description == input$food[1], DV * (input$grams/100), DV)) |>
                             mutate(Amount = ifelse(description == input$food[2], Amount * (input$grams2/100), Amount)) |>
                             mutate(DV = ifelse(description == input$food[2], DV * (input$grams2/100), DV)) |>
                             mutate(Amount = ifelse(description == input$food[3], Amount * (input$grams3/100), Amount)) |>
                             mutate(DV = ifelse(description == input$food[3], DV * (input$grams3/100), DV)) |>
                             summarise(.by = Nutrient, Amount = sum(Amount), Unit = min(Unit), DV = sum(DV))
                         }
  )
  
  output$table = renderTable(foodDT())
  
  color_scale <- scale_fill_gradientn(colours = c("red", "yellow", "green"), values = rescale(c(0, 100)), breaks = seq(0, 100, by = 20), limits = c(0, 100))
  
  output$plot = renderPlot(
    ggplot(data = foodDT() |>
             select(Nutrient, DV)) +
      aes(x = Nutrient, y = DV, fill = DV) +
      labs(title = "Daily Value (%) For Each Nutrient", tag = "Grey indicates\nvalues 100+%") +
      xlab("Nutrient") +
      ylab("Daily Value (%)") +
      geom_bar(stat = "identity") +
      coord_cartesian(ylim=c(0, 100)) + 
      theme_bw() + 
      color_scale + 
      theme(
        panel.grid.major = element_blank(), 
        panel.border = element_blank(),      
        axis.line = element_line(),          
        axis.line.x = element_line(),
        axis.line.y = element_line(),
        rect = element_rect(fill = NA, color = NA),
        plot.tag.position = c(.97, .75),
        plot.tag = element_text(size = 8)
      )
  )
  
  
}

shinyApp(ui, server)

