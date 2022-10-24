library(GetDFPData2)
library(dplyr)
library(lubridate)
library(janitor)
library(purrr)
library(tidyr)

# Código CVM das empresas ativas

cache_folder = "cache_folder_shiny"

info_companies <- GetDFPData2::get_info_companies(cache_folder = cache_folder)


# Filtra as empresas ativas, bolsas e retira bancos

names_companies <- info_companies |> 
  filter(SIT_REG == 'ATIVO' & TP_MERC == "BOLSA" & !SETOR_ATIV %in% c("Bancos",
                                                                      "Intermedia\u00e7\u00e3o Financeira",
                                                                      "Seguradoras e Corretoras")) |> 
  arrange(DENOM_SOCIAL) |> 
  pull(DENOM_SOCIAL)

# Cria lista de empresas

cvm_codes <- info_companies |> 
  filter(DENOM_SOCIAL %in% names_companies) |> 
  pull(CD_CVM)

# Define as informações da função get_dfp_data

first_date = 2011
last_date = year(Sys.Date())
type_docs = c("BPA", "BPP", "DRE")
type_format = "con"

# Coleta os dados das empresas

l_dfp <- GetDFPData2::get_dfp_data(companies_cvm_codes = cvm_codes, 
                                   first_year =  first_date,
                                   last_year = last_date, 
                                   type_docs = type_docs,
                                   type_format = type_format,
                                   use_memoise = TRUE,
                                   cache_folder = cache_folder)

# Realiza a limpeza da lista e empilha em um data frame

df_dfp <- l_dfp |>
  set_names(type_docs) |> 
  bind_rows(.id = "label") |>
  select(label, DT_REFER, DENOM_CIA, ESCALA_MOEDA, CD_CONTA, DS_CONTA, VL_CONTA)


#------------------------
# Cria os indicadores

##### Liquidez
## Corrente

corrente <- df_dfp |> 
  filter(DS_CONTA %in% c("Ativo Circulante", "Passivo Circulante")) |> 
  pivot_wider(values_from = VL_CONTA,
              names_from = c(DS_CONTA),
              -c(label, CD_CONTA)) |>
  clean_names() |> 
  group_by(denom_cia, dt_refer) |> 
  mutate(liq_corrente = ativo_circulante / passivo_circulante)|> 
  ungroup()


## Geral


geral <- df_dfp |> 
  filter(DS_CONTA %in% c("Ativo Circulante" ,"Ativo Realiz\u00e1vel a Longo Prazo", "Passivo Circulante", "Passivo N\u00e3o Circulante")
    & CD_CONTA %in% c("1.01", "1.02.01", "2.01", "2.02")) |> 
  pivot_wider(values_from = VL_CONTA,
            names_from = c(DS_CONTA),
            -c(label, CD_CONTA, ESCALA_MOEDA))|> 
  clean_names() |> 
  group_by(denom_cia, dt_refer) |> 
  mutate(liq_geral = (ativo_circulante + ativo_realizavel_a_longo_prazo) / (passivo_circulante + passivo_nao_circulante))|> 
  ungroup()

##### Endividamento

## Terceiros / CP

tcp <- df_dfp |> 
  filter(DS_CONTA %in% c("Passivo Circulante", "Passivo N\u00e3o Circulante", "Patrim\u00f4nio L\u00edquido Consolidado")
         & CD_CONTA %in% c("2.01", "2.02", "2.03")) |> 
  pivot_wider(values_from = VL_CONTA,
              names_from = c(DS_CONTA),
              -c(label, CD_CONTA, ESCALA_MOEDA))|> 
  clean_names() |> 
  group_by(denom_cia, dt_refer) |> 
  mutate(tcp = (passivo_circulante + passivo_nao_circulante) / patrimonio_liquido_consolidado)|> 
  ungroup()


## Terceiros / Passivo Total

tct <- df_dfp |> 
  filter(DS_CONTA %in% c("Passivo Circulante", "Passivo N\u00e3o Circulante", "Patrim\u00f4nio L\u00edquido Consolidado")
         & CD_CONTA %in% c("2.01", "2.02", "2.03")) |> 
  pivot_wider(values_from = VL_CONTA,
              names_from = c(DS_CONTA),
              -c(label, CD_CONTA, ESCALA_MOEDA))|>
  clean_names() |> 
  group_by(denom_cia, dt_refer) |> 
  mutate(tct = (passivo_circulante + passivo_nao_circulante) / (passivo_circulante + passivo_nao_circulante + patrimonio_liquido_consolidado)) |> 
  ungroup()


##### Rentabilidade

## ROA

roa <- df_dfp |> 
  filter(DS_CONTA %in% c("Resultado Antes do Resultado Financeiro e dos Tributos", "Ativo Total") &
           CD_CONTA %in% c("3.05", "1")) |> 
  pivot_wider(values_from = VL_CONTA,
              names_from = c(DS_CONTA),
              -c(label, CD_CONTA, ESCALA_MOEDA))|> 
  clean_names() |> 
  group_by(denom_cia, dt_refer) |> 
  mutate(roa = resultado_antes_do_resultado_financeiro_e_dos_tributos /  ativo_total)|> 
  ungroup()

## ROE

roe <- df_dfp |> 
  filter(DS_CONTA %in% c("Lucro/Preju\u00edzo Consolidado do Per\u00edodo", "Patrim\u00f4nio L\u00edquido Consolidado")
         & CD_CONTA %in% c("3.11", "2.03")) |> 
  pivot_wider(values_from = VL_CONTA,
              names_from = c(DS_CONTA),
              -c(label, CD_CONTA, ESCALA_MOEDA))|> 
  clean_names() |> 
  group_by(denom_cia, dt_refer) |> 
  mutate(roe =  lucro_prejuizo_consolidado_do_periodo / patrimonio_liquido_consolidado) |> 
  ungroup()


## Margem Bruta

margem_bruta <- df_dfp |> 
  filter(DS_CONTA %in% c("Resultado Bruto", "Receita de Venda de Bens e/ou Servi\u00e7os") 
          & CD_CONTA %in% c("3.01", "3.03")) |> 
  pivot_wider(values_from = VL_CONTA,
              names_from = c(DS_CONTA),
              -c(label, CD_CONTA, ESCALA_MOEDA))|> 
  clean_names() |> 
  group_by(denom_cia, dt_refer) |> 
  mutate(margem_bruta = resultado_bruto / receita_de_venda_de_bens_e_ou_servicos) |> 
  ungroup()


## Margem Líquida

margem_liq <- df_dfp |> 
  filter(DS_CONTA %in% c("Receita de Venda de Bens e/ou Servi\u00e7os", "Lucro/Preju\u00edzo Consolidado do Per\u00edodo")
         & CD_CONTA %in% c("3.11", "3.01")) |>
  pivot_wider(values_from = VL_CONTA,
              names_from = c(DS_CONTA),
              -c(label, CD_CONTA, ESCALA_MOEDA))|>
  clean_names() |> 
  group_by(denom_cia, dt_refer) |> 
  mutate(margem_liq = lucro_prejuizo_consolidado_do_periodo / receita_de_venda_de_bens_e_ou_servicos) |> 
  ungroup()


###### DRE

## Receita
## Custos
## LL

dre <- df_dfp |> 
  filter(DS_CONTA %in% c("Receita de Venda de Bens e/ou Servi\u00e7os",
                         "Lucro/Preju\u00edzo Consolidado do Per\u00edodo",
                         "Custo dos Bens e/ou Servi\u00e7os Vendidos")
         & CD_CONTA %in% c("3.11", "3.01", "3.02")) |>
  pivot_wider(values_from = VL_CONTA,
              names_from = c(DS_CONTA),
              -c(label, CD_CONTA, ESCALA_MOEDA)) |> 
  clean_names()

# Salvar
save(
  list  = ls(),
  file  = file.path("dados.Rdata"),
  envir = environment()
)

