# funcao para extrair as informacoes de uma cerveja 
# do brejas.com.br a partir de um link

cerveja <- function(url){
  
  pagina <- read_html(url)
  
  # nome da cerveja
  
  nome <- pagina %>% 
    html_nodes(xpath="/html/body/div[2]/div[2]/div/main/article/div/div/h1/span[1]") %>%
    html_text()
  
  # informacoes
  
  informacoes <- pagina %>% 
    html_nodes(xpath="/html/body/div[2]/div[2]/div/main/article/div/div/div[8]/div") %>%
    html_text()
  
  if (length(informacoes)==0){
    informacoes <- pagina %>% 
      html_nodes(xpath="/html/body/div[2]/div[2]/div/main/article/div/div/div[7]/div") %>%
      html_text()
  }
  
  cervejaria <- gsub("InformaçõesCervejaria", "", strsplit(informacoes, split="Grupo")[[1]][1])
  
  grupo <- strsplit(strsplit(informacoes, split="Grupo")[[1]][2], split="Estilo")[[1]][1]
  
  estilo <- strsplit(strsplit(informacoes, split="Estilo")[[1]][2], split="Álcool")[[1]][1]
  
  ativa <- strsplit(strsplit(informacoes, split="Ativa")[[1]][2], split="Sazonal")[[1]][1]
  
  sazonal <- strsplit(strsplit(informacoes, split="Sazonal")[[1]][2], split="Temperatura")[[1]][1]
  
  copo <- strsplit(strsplit(informacoes, split="Copo ideal")[[1]][2], split="Visite")[[1]][1]
  
  numeros <- str_extract_all(informacoes, "\\(?[0-9,.]+\\)?")[[1]]
  numeros <- numeros[grep(",", numeros, invert=TRUE)]
  numeros <- as.numeric(numeros)
  alcool  <- numeros[1]
  t.min   <- numeros[2]
  t.max   <- numeros[3]
  
  # scores

  dados <- pagina %>% 
    html_nodes(xpath="/html/body/div[2]/div[2]/div/main/article/div/div/div[15]/div[3]/div/div[1]/div[2]/div/div[1]") %>%
    html_text()
  
  if (length(dados) == 0){
    dados <- pagina %>% 
      html_nodes(xpath="/html/body/div[2]/div[2]/div/main/article/div/div/div[16]/div[3]/div/div[1]/div[2]/div/div[1]") %>%
      html_text()
  }
  
  if (length(dados) == 0) {
    resultado <- data.frame(Nome=nome,
                            Cervejaria=cervejaria,
                            Grupo=grupo,
                            Estilo=estilo,
                            Alcool=alcool,
                            Ativa=ativa,
                            Sazonal=sazonal,
                            Minima=t.min,
                            Maxima=t.max,
                            Copo=copo,
                            Geral=NA,
                            Aroma=NA,
                            Aparencia=NA,
                            Sabor=NA,
                            Sensacao=NA,
                            Conjunto=NA,
                            Votos=NA)
  } else {
  
  # regex para extrair apenas os numeros

    avaliacao <- str_extract_all(dados, "\\(?[0-9,.]+\\)?")[[1]]
    avaliacao <- avaliacao[c(1, 2, 5, 8, 11, 14, 4)]
    avaliacao <- as.numeric(gsub("\\((.+)\\)","\\1", avaliacao))#/c(1, 10, 5, 20, 5, 10, 1)
  
    # organizar as informacoes
  
    resultado <- data.frame(Nome=nome,
                          Cervejaria=cervejaria,
                          Grupo=grupo,
                          Estilo=estilo,
                          Alcool=alcool,
                          Ativa=ativa,
                          Sazonal=sazonal,
                          Minima=t.min,
                          Maxima=t.max,
                          Copo=copo,
                          Geral=avaliacao[1],
                          Aroma=avaliacao[2],
                          Aparencia=avaliacao[3],
                          Sabor=avaliacao[4],
                          Sensacao=avaliacao[5],
                          Conjunto=avaliacao[6],
                          Votos=avaliacao[7])
  
  }

  return(resultado)
}


