library(rvest)
library(tidyverse)
library(magrittr)

# createKeggHTTPRequest = function(ko_list)
# {
#   base = "https://www.genome.jp/kegg-bin/search_pathway_object?org_name=ko&amp;unclassified="
#   paste0(base, paste0(ko_list, collapse="%20"))
# }
#
# getPathwaysFromKEGGWebpage = function(str)
# {
#   scraped = read_html(kegg_http_request)
#   li_nodes = scraped %>% html_node('.box2') %>% html_nodes('li')
#
#   df = data.frame(KO=character(), Pathway=character())
#
#   for (node in li_nodes)
#   {
#     # print(node)
#
#     ko_id =
#       node %>%
#       html_node('a') %>%
#       html_text() %>%
#       toupper()
#
#     pathway_name =
#       node %>%
#       sub(".*/a>(.*?)\\(<a.*", '\\1', .)
#
#     df %<>% add_row(KO=ko_id, Pathway=pathway_name)
#
#   }
#
#   return(df)
#
# }
#
# getPathwaysFromKOList = function(ko_list)
# {
#
#   kegg_http_request = createKeggHTTPRequest(ko_list)
#   print(kegg_http_request)
#
#   scraped = read_html(kegg_http_request)
#   li_nodes = scraped %>% html_node('.box2') %>% html_nodes('li')
#
#   df = data.frame(KO=character(), Pathway=character())
#
#   for (node in li_nodes)
#   {
#     # print(node)
#
#     ko_id =
#       node %>%
#       html_node('a') %>%
#       html_text() %>%
#       toupper()
#
#     pathway_name =
#       node %>%
#       sub(".*/a>(.*?)\\(<a.*", '\\1', .)
#
#     df %<>% add_row(KO=ko_id, Pathway=pathway_name)
#
#   }
#
#   df %<>% droplevels()
#
#   return(df)
#
# }
#
# kegg_http_request = 'https://www.genome.jp/kegg-bin/search_pathway_object?org_name=ko&amp;unclassified=K02078%20K00764%20K03657'
# getPathwaysFromKEGGWebpage(kegg_http_request)
#
# kegg_http_request = createKeggHTTPRequest(c("K02078", "K00764", "K03657"))
# print(kegg_http_request)
# getPathwaysFromKEGGWebpage(kegg_http_request)
#
# getPathwaysFromKOList(c("K02078", "K00764", "K03657"))

getKEGGPathwayDF = function(ko_list)
{
  pathway_df = data.frame(Pathway=character(), NAME=character())

  for (ko in ko_list)
  {
    print(ko)

    response = keggGet(ko) %>%
      .[[1]] %>%
      .[c('NAME')] %>%
      lapply(as.character)
    response$Pathway = ko

    pathway_df %<>% add_row(!!!response)
  }

  pathway_df %<>% rename(PathwayName=NAME)

  return(pathway_df)
}

getKEGGPathways = function(ko_list)
{
  pathway_df = data.frame(Pathway=character(), NAME=character())

  for (ko in ko_list)
  {
    # print(ko)

    result = tryCatch(
      {
        response = keggGet(ko) %>%
          .[[1]] %>%
          .[c('NAME')] %>%
          lapply(as.character)
        response$Pathway = ko
      },
      warning = function(w)
      {
        response = "UNKNOWN WARNING"
      },
      error = function(e)
      {
        response = e
      },
      finally =
      {
      }
    )

    pathway_df %<>% add_row(!!!response)
  }

  pathway_df %<>% rename(PathwayName=NAME)

  return(pathway_df$PathwayName)
}



