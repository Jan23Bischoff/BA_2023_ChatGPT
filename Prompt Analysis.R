library(readxl)
library(tidyverse)
library(networkD3)
library(htmlwidgets)

df <- read_excel("Shiny Prompt Documentation.xlsx")

df %>% summary()

df %>% 
  mutate(
    prompt_length = sapply( strsplit(Prompt, " ") , length),
    words_per_requirement = prompt_length / `Requirements Specified`
  ) -> df


df %>%
  filter(Type != "Other") %>%
  group_by(Type) %>%
  summarise(
    number_occurences = n(),
    mean_prompt_length = mean(prompt_length, na.rm = T),
    mean_success_rate = mean(Ratio, na.rm = T),
    mean_requirements = mean(`Requirements Specified`, na.rm = T),
    mean_words_per_requirement = mean(words_per_requirement, na.rm = T)
  ) -> df_grouped

df %>% 
  filter(Type != "Other") -> df_final

#Summary statistics in nice format
round(do.call(cbind, lapply( select(df_final, `Requirements Specified`, `Requirements Satisfied`, Ratio, prompt_length, words_per_requirement), summary)),2)

#correlation matrix
df_final %>%
  select(Ratio, words_per_requirement, `Requirements Specified`) %>% 
  cor() %>%
  round(2)


mean_satisfied_rate_plot <- df_grouped %>%
  ggplot(aes(reorder(Type, -mean_success_rate), mean_success_rate)) +
  geom_col() +
  theme_minimal() +
  xlab("") + 
  ylab("") + 
  scale_y_continuous(labels = scales::percent)

mean_words_plot <- df_grouped %>%
  ggplot(aes(reorder(Type, -mean_success_rate), mean_words_per_requirement)) +
  geom_col() +
  theme_minimal() +
  xlab("") + 
  ylab("")



#ggsave(filename = "Average Percentage of Satisfied Requirements by Text Type.jpg", plot = mean_satisfied_rate_plot)
#ggsave(filename = "Average Words per Requirements by Text Type.jpg", plot = mean_words_plot)



df_grouped %>%
  ggplot(aes(reorder(Type, -number_occurences), number_occurences)) +
  geom_col() +
  theme_minimal() +
  xlab("") +
  ylab("") -> number_occurences_type_plot

#ggsave(filename = "Number Occurences by Text Type.jpg", plot = number_occurences_type_plot)


df_final$prompt_result <- ifelse(df_final$Ratio == 1, "Successful", "Unsuccessful")
df_final$prompt_category <- ifelse(df_final$Type %in% c("Text only", "Key Points", "Both Text and Key Points"), "Starting Prompt", "Follow-Up Prompt")

df_final %>%
  filter(Type %in% c("Text only", "Key Points", "Both Text and Key Points")) %>%
  nrow() -> number_starting_prompts

number_follow_up_prompts <- nrow(df_final) - number_starting_prompts

df_final %>%
  filter(Type %in% c("Text only", "Key Points", "Both Text and Key Points")) %>%
  filter(Ratio == 1) %>%
  nrow() -> number_successful_starting_prompts

number_unsuccessful_starting_prompts <- number_starting_prompts - number_successful_starting_prompts


df_final %>%
group_by(prompt_category, Type) %>%
summarise(n()) -> sankey_df_1
colnames(sankey_df_1) <- c("source","target", "value")

df_final %>%
  group_by(Type, prompt_result) %>%
  summarise(n()) -> sankey_df_2
colnames(sankey_df_2) <- c("source","target", "value")

links <- rbind(sankey_df_1, sankey_df_2)

nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)

links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

my_color <- 'd3.scaleOrdinal() .domain(["Follow-Up Prompt", "Starting Prompt","Both Text and Key Points", "Error and Text", "Follow Up Text", "Key Points", "Text only"]) .range(["black", "black" , "black", "black", "black", "black", "black"])'

# Make the Network
p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   sinksRight=FALSE, fontSize = 10,
                   colourScale=my_color)



p
htmlwidgets::onRender(p, '
  function(el) { 
    var nodeWidth = this.sankey.nodeWidth();
    var links = this.sankey.links();
        
    links.forEach((d, i) => {
      var startX = d.source.x + nodeWidth;
      var endX = d.target.x;
      
      var startY = d.source.y + d.sy + d.dy / 2;
      var endY = d.target.y + d.ty + d.dy / 2;
      
      d3.select(el).select("svg g")
        .append("text")
        .attr("text-anchor", "middle")
        .attr("alignment-baseline", "middle")
        .attr("x", startX + ((endX - startX) / 2))
        .attr("y", startY + ((endY - startY) / 2))
        .text(d.value);
    })
  }
')
