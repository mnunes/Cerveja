# funcao para encontrar os links das cervejas com mais 
# avaliacoes no brejas.com.br

scrap <- function(url){

  pagina <- read_html(url)

  # filtragem dos links

  links <- html_attr(html_nodes(pagina, "a"), "href")
  links <- links[grep("/cerveja/", links)]
  links <- links[grep("#", links, invert=TRUE)]
  links <- links[grep("www", links, invert=TRUE)]

  # manter apenas os que mais se repetem

  links <- names(sort(table(links), decreasing=TRUE)[1:10])

  # criar os links completos

  links <- paste("http://www.brejas.com.br", links, sep="")

  return(links)

}

