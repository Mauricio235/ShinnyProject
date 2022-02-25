
library(shiny)
library(rsconnect)
library(shiny)
library(plotly)
library(shinydashboard)
library(readxl)
library(dplyr)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(ggplot2)
library(runner)
library(dplyr)

ui <- dashboardPage(skin= 'green',
                    dashboardHeader(
                      title ="Análise de mercado: Franquias de vendas e clientes ativos", 
                      dropdownMenu(type ="notifications", badgeStatus ="warning",
                                   notificationItem(icon = icon("users"), status ="info",
                                                   "5 new members joined today"
                                   ),
                                   notificationItem(icon = icon("warning"), status ="danger",
                                                   "Resource usage near limit."
                                   ),
                                   notificationItem(icon = icon("shopping-cart", lib ="glyphicon"),
                                                    status ="success","25 sales made"
                                   ),
                                   notificationItem(icon = icon("user", lib ="glyphicon"),
                                                    status ="danger","You changed your username"
                                   )
                      )
                    ),
                    dashboardSidebar(
                      
                      sidebarMenu(
                        # Setting id makes input$tabs give the tabName of currently-selected tab
                        id ="tabs",
                        menuItem("Dashboard", tabName ="dashboard", icon = icon('chart-line')),
                        
                        menuItem("Base de dados", icon = icon("bar-chart-o"),
                                 menuSubItem("Estrutura", tabName ="subitem1"),
                                 menuSubItem("Variáveis", tabName ="subitem2")
                        ),
                        
                        menuItem("Testes estatísticos", icon = icon("bar-chart-o"),
                                 menuSubItem("Testes e descrições", tabName ="sub1")
                        ),
                        
                        
                        menuItem("Instruções", icon = icon("th"), tabName ="informacao")
                        
                        
                      ),
                      
                      fluidPage(
                        
                        checkboxGroupInput("checkGroup", label = h3("Checkbox year"), 
                                           choices = list("2019" ="2019",
                                                         "2021" ="2021"), selected = c("2019","2021")),
                        selectInput("variable","Insira a franquia:",
                                    choices= list("FR033"="FR033",	"FR032"="FR032",	"FR117"="FR117",	"FR214"="FR214",	
                                                 "FR315"="FR315",	"FR306"="FR306",	"FR036"="FR036",	"FR240"="FR240",	
                                                 "FR260"="FR260",	"FR316"="FR316",	"FR303"="FR303",	"FR044"="FR044",	
                                                 "FR207"="FR207",	"FR287"="FR287",	"FR172"="FR172",	"FR076"="FR076",	
                                                 "FR113"="FR113",	"FR055"="FR055",	"FR149"="FR149",	"FR297"="FR297",	
                                                 "FR248"="FR248",	"FR277"="FR277",	"FR179"="FR179",	"FR265"="FR265",	
                                                 "FR273"="FR273",	"FR167"="FR167",	"FR059"="FR059",	"FR284"="FR284",	
                                                 "FR054"="FR054",	"FR004"="FR004",	"FR130"="FR130",	"FR093"="FR093",	
                                                 "FR276"="FR276",	"FR152"="FR152",	"FR281"="FR281",	"FR268"="FR268",	
                                                 "FR052"="FR052",	"FR088"="FR088",	"FR086"="FR086",	"FR253"="FR253",	
                                                 "FR012"="FR012",	"FR022"="FR022",	"FR078"="FR078",	"FR241"="FR241",	
                                                 "FR143"="FR143",	"FR213"="FR213",	"FR085"="FR085",	"FR271"="FR271",	
                                                 "FR097"="FR097",	"FR274"="FR274",	"FR127"="FR127",	"FR255"="FR255",	
                                                 "FR156"="FR156",	"FR114"="FR114",	"FR262"="FR262",	"FR245"="FR245",	
                                                 "FR126"="FR126",	"FR247"="FR247",	"FR098"="FR098",	"FR035"="FR035",	
                                                 "FR161"="FR161",	"FR016"="FR016",	"FR175"="FR175",	"FR188"="FR188",	
                                                 "FR169"="FR169",	"FR267"="FR267",	"FR163"="FR163",	"FR129"="FR129",	
                                                 "FR122"="FR122",	"FR280"="FR280",	"FR279"="FR279",	"FR320"="FR320",	
                                                 "FR110"="FR110",	"FR278"="FR278",	"FR288"="FR288",	"FR165"="FR165",	
                                                 "FR051"="FR051",	"FR197"="FR197",	"FR026"="FR026",	"FR266"="FR266",	
                                                 "FR270"="FR270",	"FR317"="FR317",	"FR259"="FR259",	"FR164"="FR164",	
                                                 "FR285"="FR285",	"FR100"="FR100",	"FR057"="FR057",	"FR191"="FR191",	
                                                 "FR089"="FR089",	"FR142"="FR142",	"FR124"="FR124",	"FR177"="FR177",	
                                                 "FR272"="FR272",	"FR282"="FR282",	"FR038"="FR038",	"FR070"="FR070",	
                                                 "FR264"="FR264",	"FR286"="FR286",	"FR116"="FR116",	"FR115"="FR115",	
                                                 "FR304"="FR304",	"FR243"="FR243",	"FR269"="FR269",	"FR101"="FR101",	
                                                 "FR082"="FR082",	"FR080"="FR080",	"FR007"="FR007",	"FR075"="FR075",	
                                                 "FR060"="FR060",	"FR047"="FR047",	"FR275"="FR275",	"FR063"="FR063",	
                                                 "FR294"="FR294",	"FR108"="FR108",	"FR234"="FR234",	"FR121"="FR121",	
                                                 "FR096"="FR096",	"FR158"="FR158",	"FR313"="FR313",	"FR131"="FR131",	
                                                 "FR151"="FR151",	"FR312"="FR312",	"FR252"="FR252",	"FR263"="FR263",
                                                 "FR246"="FR246",	"FR039"="FR039",	"FR195"="FR195",	"FR025"="FR025",	
                                                 "FR258"="FR258",	"FR084"="FR084",	"FR056"="FR056",	"FR034"="FR034",	
                                                 "FR292"="FR292",	"FR237"="FR237",	"FR261"="FR261",	"FR008"="FR008",	
                                                 "FR254"="FR254",	"FR309"="FR309",	"FR148"="FR148",	"FR289"="FR289",
                                                 "FR068"="FR068",	"FR105"="FR105",	"FR283"="FR283",	"FR073"="FR073",	
                                                 "FR220"="FR220",	"FR136"="FR136",	"FR102"="FR102",	"FR314"="FR314",
                                                 "FR206"="FR206",	"FR134"="FR134",	"FR202"="FR202",	"FR079"="FR079",	
                                                 "FR024"="FR024",	"FR232"="FR232",	"FR112"="FR112",	"FR099"="FR099",	
                                                 "FR298"="FR298",	"FR081"="FR081",	"FR307"="FR307",	"FR217"="FR217",	
                                                 "FR250"="FR250",	"FR249"="FR249",	"FR200"="FR200",	"FR087"="FR087",	
                                                 "FR094"="FR094",	"FR074"="FR074",	"FR193"="FR193",	"FR041"="FR041",	
                                                 "FR106"="FR106",	"FR139"="FR139",	"FR231"="FR231",	"FR176"="FR176",	
                                                 "FR137"="FR137",	"FR140"="FR140",	"FR310"="FR310",	"FR006"="FR006",	
                                                 "FR071"="FR071",	"FR083"="FR083",	"FR157"="FR157",	"FR308"="FR308",	
                                                 "FR107"="FR107",	"FR302"="FR302",	"FR203"="FR203",	"FR072"="FR072",	
                                                 "FR013"="FR013",	"FR067"="FR067",	"FR201"="FR201",	"FR192"="FR192",	
                                                 "FR233"="FR233",	"FR146"="FR146",	"FR046"="FR046",	"FR236"="FR236",	
                                                 "FR235"="FR235",	"FR153"="FR153",	"FR123"="FR123",	"FR291"="FR291",	
                                                 "FR133"="FR133",	"FR187"="FR187",	"FR021"="FR021",	"FR189"="FR189",	
                                                 "FR229"="FR229",	"FR160"="FR160",	"FR064"="FR064",	"FR159"="FR159",	
                                                 "FR138"="FR138",	"FR218"="FR218",	"FR125"="FR125",	"FR216"="FR216",	
                                                 "FR311"="FR311",	"FR296"="FR296",	"FR223"="FR223",	"FR295"="FR295",	
                                                 "FR028"="FR028",	"FR205"="FR205",	"FR003"="FR003",	"FR171"="FR171",	
                                                 "FR215"="FR215",	"FR037"="FR037",	"FR049"="FR049",	"FR181"="FR181",	
                                                 "FR256"="FR256",	"FR170"="FR170",	"FR178"="FR178",	"FR319"="FR319",	
                                                 "FR225"="FR225",	"FR180"="FR180",	"FR209"="FR209",	"FR091"="FR091",	
                                                 "FR062"="FR062",	"FR154"="FR154",	"FR043"="FR043",	"FR224"="FR224",	
                                                 "FR011"="FR011",	"FR120"="FR120",	"FR173"="FR173",	"FR183"="FR183",	
                                                 "FR210"="FR210",	"FR017"="FR017",	"FR050"="FR050",	"FR239"="FR239",	
                                                 "FR053"="FR053",	"FR030"="FR030",	"FR211"="FR211",	"FR141"="FR141",	
                                                 "FR222"="FR222",	"FR111"="FR111",	"FR009"="FR009",	"FR221"="FR221",	
                                                 "FR251"="FR251",	"FR227"="FR227",	"FR293"="FR293",	"FR005"="FR005",	
                                                 "FR242"="FR242",	"FR061"="FR061",	"FR029"="FR029",	"FR048"="FR048",	
                                                 "FR228"="FR228",	"FR135"="FR135",	"FR174"="FR174",	"FR226"="FR226",	
                                                 "FR015"="FR015",	"FR257"="FR257",	"FR184"="FR184",	"FR001"="FR001",	
                                                 "FR014"="FR014",	"FR194"="FR194",	"FR219"="FR219",	"FR042"="FR042",	
                                                 "FR144"="FR144",	"FR301"="FR301",	"FR150"="FR150",	"FR031"="FR031",	
                                                 "FR155"="FR155",	"FR018"="FR018",	"FR002"="FR002",	"FR162"="FR162",	
                                                 "FR321"="FR321",	"FR119"="FR119",	"FR208"="FR208",	"FR168"="FR168",	
                                                 "FR186"="FR186",	"FR023"="FR023",	"FR230"="FR230",	"FR103"="FR103",	
                                                 "FR166"="FR166",	"FR069"="FR069",	"FR104"="FR104",	"FR145"="FR145",	
                                                 "FR147"="FR147",	"FR198"="FR198",	"FR300"="FR300",	"FR109"="FR109",	
                                                 "FR058"="FR058",	"FR244"="FR244",	"FR190"="FR190",	"FR238"="FR238",	
                                                 "FR132"="FR132",	"FR090"="FR090",	"FR128"="FR128",	"FR182"="FR182",	
                                                 "FR077"="FR077",	"FR020"="FR020",	"FR318"="FR318",	"FR019"="FR019",	
                                                 "FR065"="FR065",	"FR299"="FR299",	"FR199"="FR199",	"FR095"="FR095",	
                                                 "FR010"="FR010",	"FR118"="FR118",	"FR212"="FR212",	"NA"="NA",	
                                                 "FR066"="FR066",	"FR092"="FR092",	"FR027"="FR027",	"FR196"="FR196",	
                                                 "FR290"="FR290",	"FR045"="FR045",	"FR305"="FR305",	"FR185"="FR185")),
                        
                  
                    )),
                    dashboardBody(
                      tabItems(
                        tabItem(
                          tabName = 'dashboard', h2('Análise de franquias e clientes ativos'),
                          fluidPage(
                            
                            
                            infoBox(title = 'Dados= Franquias e clientes ativos
                                    tomando como base os anos de 2019 e 2021', value ='Anos: 2019 e 2021', subtitle = NULL,
                                    icon = shiny::icon("bar-chart"), color ="green", width = 4,
                                    href = NULL, fill = FALSE),
                            
                            valueBox(value =uiOutput('Totalclientes'), subtitle ="",
                                     icon = icon("fa-regular fa-chart-line-down"),
                                     color ="green", width = 4),
                            
                            valueBox(value =uiOutput('Totalreceita1'), subtitle ="", 
                                     color ="green", width = 4, icon=icon("fa-solid fa-chart-line-down")),
                            
                            
                            
                            
                            #Primeiro grafico caixa
                            
                            tabBox(
                              title ="Geom Line",
                              # The id lets us use input$tabset1 on the server to find the current tab
                              id ="tabset1",width = 12,
                              tabPanel("Análise de clientes ativos por mês", plotlyOutput("line")),
                              tabPanel("Clientes ativos por franquia e por mês", plotlyOutput("lineFranquia")),
                              tabPanel("Resultados", textOutput("results1"))
                            ),
                            
                            
                            
                            tabBox(title ="Boxplot", 
                                   id="tabset2",width = 7,
                                   tabPanel("Índice Médio por região", plotlyOutput("plot")),
                                   tabPanel("Resultados", textOutput("text1"))
                            ),
                            box(title ="Índices por região e população total", width = 5,
                                collapsible = TRUE,
                                dataTableOutput("table1"))
                          )
                          
                          
                          
                        ),
                        
                        #Estructure  of the database
                        tabItem(
                          tabName ="subitem2", h2('Estrutura da base de dados'),
                          
                          dataTableOutput('HeadData'),
                          
                          fluidPage(
                            h3('Variáveis de estudo e descrições'),verbatimTextOutput("code")
                          )
                        ),
                        
                        #Estructure  of the database
                        tabItem(
                          tabName ="subitem1", h2('Algumas medidas descritivas'),
                          
                          fluidPage(
                            h3('Variáveis de estudo e descrições'),verbatimTextOutput("summary")
                          )
                        ),
                        # Qui square tests
                        tabItem(
                          tabName ="sub1", h2('Tests and descriptions'),
                          
                          box(title ="Teste Qui quadrado de associação", 
                              collapsible = TRUE,collapsed = T,
                              verbatimTextOutput('descriptionTest')
                          ),
                          
                          box(title ="Teste Qui Quadrado de associação (Resultados)", 
                              collapsible = TRUE,collapsed = T,
                              verbatimTextOutput('testresult')
                          ),
                          
                          box(title ="Correlação: Análise de renda por clientes ativos", 
                              collapsible = TRUE,collapsed = T,
                              verbatimTextOutput('descTest')
                          ),
                          
                          box(title ="Correlação (Resultados)", 
                              collapsible = TRUE,collapsed = T,
                              verbatimTextOutput('testresultado')
                          )
                          
                        ),
                        
                        
                        tabItem(
                          tabName ="informacao", h2('Guias e comandos'),
                          
                          box(title ="Instruções e informações importantes", 
                              collapsible = TRUE,collapsed = F,width = 10,
                              verbatimTextOutput('informacao')
                          ))
                        
                        
                        
                      ),
                      )
)



