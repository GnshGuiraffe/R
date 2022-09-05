# Vacinas no município de São Paulo --------------------------------------------
#
# Objetivo: Análise da disponibilidade de vacinas nos postos de saúde ----------
#
# Inspirado no código de Ana Carolina Moreno
# Realizado por: Haydee Svab em 22/08/2022
# ------------------------------------------------------------------------------

# SETUP INICIAL ----------------------------------------------------------------

# Pacotes
install.packages("tidyverse")
library(tidyverse)

# LEITURA DE ARQUIVOS-----------------------------------------------------------
df_vacinas <- read_csv("data/vacinas.csv")

# COMPREENSÃO DO ARQUIVO CIOM VIEW E GLIMPSE -----------------------------------
View(df_vacinas)
glimpse(df_vacinas)

# MANIPULAÇÃO COM FILTER E SELECT ----------------------------------------------

# Para saber os nomes das variáveis
names(df_vacinas)

# Para ver as categorias da variável status_fila
levels(as.factor(df_vacinas$status_fila))

# Retornar nome e endereço dos postos de vacinação do bairro da Bela Vista que estão funcionando
df_postos_bela_vista <- df_vacinas %>%
  filter(distrito == "Bela Vista" & status_fila != "NÃO FUNCIONANDO") %>% 
  select(equipamento, endereco)
df_postos_bela_vista

# Retornar nome e endereço dos postos volantes, megapostos e postos em parque que não estão funcionando
df_postos_funcionando <- df_vacinas %>%
  filter((tipo_posto == "PARQUES" |
           tipo_posto == "MEGAPOSTO" |
           tipo_posto == "POSTO VOLANTE") &
           (status_fila == "NÃO FUNCIONANDO")) %>% 
  select(equipamento, endereco)
df_postos_funcionando

# MANIPULAÇÃO COM RENAME E MUTATE ----------------------------------------------

# Para ver as categorias da variável astrazeneca
levels(as.factor(df_vacinas$astrazeneca))

# Para criar nova coluna "falta_az" indicando se há ou não a vacina aztrazeneca disponível
df_vacinas <- df_vacinas %>%
  mutate(falta_az = case_when(
    # A variável falta_az recebe texto "postos_SEM_astrazeneca" se a variável astrazeneca for igual a zero
    astrazeneca == 0 ~ "postos_SEM_astrazeneca",
    # A variável falta_az recebe texto "postos_COM_astrazeneca" se a variável astrazeneca for igual a um
    astrazeneca == 1 ~ "postos_COM_astrazeneca"))

# O nome falta_az ficou meio obscuro, vamos renomear para falta_astrazeneca
df_vacinas <- df_vacinas %>%
  rename (falta_astrazeneca = falta_az)

# Para criar nova coluna "falta_coronavac" indicando se há ou não a vacina coronavac disponível
df_vacinas <- df_vacinas %>%
  mutate(falta_coronavac = case_when(
    coronavac == 0 ~ "postos_SEM_coronavac",
    coronavac == 1 ~ "postos_COM_coronavac"))

# MANIPULAÇÃO COM GROUP_BY E SUMMARISE -----------------------------------------

# Cria um df específico para esta análise
analise_df_vacinas <- df_vacinas %>%
  # filtra apenas as informações de postos que estão funcionando
  filter(status_fila != "NÃO FUNCIONANDO") %>%
  # cria uma variável que sinaliza se a observação corresponde a um equipamento
  # existente (excluindo campos nulos)
  mutate(equipamento_existe = case_when(
    !is.na(equipamento) ~ 1,
    T ~ 0))

# Determina o total de postos por distrito
analise_df_vacinas_por_distrito <- analise_df_vacinas %>%
  # agrupa por distritos
  group_by(distrito) %>%
  # e faz a soma
  summarise(qtde_postos_total_por_distrito = sum(equipamento_existe))  

# Determina o total de postos com astrazeneca por distrito
analise_df_vacinas_astrazeneca <- analise_df_vacinas %>%
  # agrupa por distritos e por falta_astrazeneca
  group_by(distrito, falta_astrazeneca) %>%
  # e faz a soma
  summarise(qtde_postos_total_com_aztrazeneca_por_distrito = sum(equipamento_existe)) %>% 
  # filtra apenas os potos que têm atrazeneca
  filter(falta_astrazeneca == "postos_COM_astrazeneca")