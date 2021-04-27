# Here are a problem that  I found and have some dificult at the beggining to solve.

nome <- c("Jon", "Bill", "Maria", "Mark")
numero <- c(1234, 9999, 2222, 3333)
fones <- data.frame(nome, numero)
fones

ligacao <- data.frame(
  id = seq(1,6, 1),
  liga = sample(numero, size = 6, replace = T),
  recebe = sample(numero, size = 6, replace = T),
  duracao = sample(1:9, 6, 9)
)

ligacao
rm(nome, numero)

# problema

# uma tabela tem o numero das pessoas e na outra as ligacoes.
# encontre o numero total de minutos  que cada pessoa passou no telefone
# retorne apenas o nome dos que passaram mais de 10 minutos
# tem que somar tanto ligacao feita quando ligacao recebida da mesma pessoa


# RESOLUÇÃO
# calculando tempo de ligacao feita e recebida
feita <- ligacao %>% group_by(liga) %>% 
  summarise(tempof = sum(duracao))

recebida <- ligacao %>% group_by(recebe) %>% 
  summarise(tempor = sum(duracao))

  
# Juntando e somando
df <- feita %>% full_join(recebida, by = c("liga" = "recebe")) %>% 
  #rowwise will make sure the sum operation will occur on each row
  rowwise() %>% 
  #then a simple sum(..., na.rm=TRUE) is enough to result in what you need
  mutate(tempo_lig = sum(tempof,tempor, na.rm=TRUE)) %>% 
  select(liga, tempo_lig)

rm(feita, recebida)

# fazendo  o join
# teste
fones %>% inner_join(df, by = c("numero" = "liga")) %>% 
  filter(tempo_lig >= 10)

# resultado final
resultado <- fones %>% inner_join(df, by = c("numero" = "liga")) %>% 
  filter(tempo_lig >= 10) %>% 
  select(nome)

resultado


