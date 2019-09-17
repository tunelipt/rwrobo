require(XMLRPC)



#' Criar conexão com robô cartesiano
#'
#' Cria um objeto de classe \code{robo} que controla a comunicação com
#' o servidor XML-RPC do robô cartesiano do túnel de vento do IPT.
#' 
#' @param url String com URL do servidor XML-RPC
#' @param port Inteiro com o número da porta TCP/IP usado pelo XML-RPC
#' @return Um objeto de classe \code{robo}
#' @examples
#' dev <- roboDev()
#' 
#' @export
roboDev <- function(url='localhost', port=9595){

    url1 <- paste0(url, ":", port)
    rpc <- function(cmd, ...) xml.rpc(url1, cmd, ...)
    dev <- list(url=url, port=port, rpc=rpc)
    class(dev) <- 'robo'
    return(dev)
}

# Versão com xmlrpc. Precisa require(curl)
#roboDev2 <- function(url='localhost', port=9595){
#
#    handle <- new_handle()
#    handle_setopt(handle, port=port)
#    rpc <- function(cmd, ...) xmlrpc(url, cmd, params=list(...), handle=handle)
#
#    dev <- list(url=url, port=port, handle=handle, rpc=rpc)
#    class(dev) <- 'robo'
#    return(dev)
#}

#' Movimenta robô cartesiano
#'
#' Executa os movimentos do robô cartesiano
#'
#' @param dev Um objeto de classe \code{robo} contendo informações sobre servidor XML-RPC
#' @param x Número informando o quanto se deve movimentar no eixo x
#' @param y Número informando o quanto se deve movimentar no eixo y
#' @param z Número informando o quanto se deve movimentar no eixo z
#' @param a TRUE/FALSE, informa se o movimento se dá na referência absoluta ou relativa
#' @param r TRUE/FALSE, movimento relativo em relação ao ponto atual?
#' @param @sync TRUE/FALSE Se sincronizado, a função só retorna depois de executado o movimento
#'
#' @examples
#' move(dev, x=5, r=TRUE)
#'
#' @export
move.robo <- function(dev, x='', y='', z='', a=FALSE, r=FALSE, sync=FALSE){
    dev$rpc("move", x, y, z, a, r, sync)
}

#' Movimenta robô cartesiano no eixo x
#'
#' Executa os movimentos do robô cartesiano ao longo do eixo x apenas
#'
#' @param dev Um objeto de classe \code{robo} contendo informações sobre servidor XML-RPC
#' @param x Número informando o quanto se deve movimentar no eixo x
#' @param a TRUE/FALSE, informa se o movimento se dá na referência absoluta ou relativa
#' @param r TRUE/FALSE, movimento relativo em relação ao ponto atual?
#' @param @sync TRUE/FALSE Se sincronizado, a função só retorna depois de executado o movimento
#'
#' @examples
#' moveX(dev, 5, r=TRUE)
#'
#' @export
moveX.robo <- function(dev, x, a=FALSE, r=FALSE, sync=FALSE){
    dev$rpc("move", x, '', '', a, r, sync)
}

#' Movimenta robô cartesiano no eixo y
#'
#' Executa os movimentos do robô cartesiano ao longo do eixo y apenas
#'
#' @param dev Um objeto de classe \code{robo} contendo informações sobre servidor XML-RPC
#' @param x Número informando o quanto se deve movimentar no eixo y
#' @param a TRUE/FALSE, informa se o movimento se dá na referência absoluta ou relativa
#' @param r TRUE/FALSE, movimento relativo em relação ao ponto atual?
#' @param @sync TRUE/FALSE Se sincronizado, a função só retorna depois de executado o movimento
#'
#' @examples
#' moveY(dev, 5, r=TRUE)
#'
#' @export
moveY.robo <- function(dev, x, a=FALSE, r=FALSE, sync=FALSE){
    dev$rpc("move", '', x, '', a, r, sync)
}


#' Movimenta robô cartesiano no eixo z
#'
#' Executa os movimentos do robô cartesiano ao longo do eixo z apenas
#'
#' @param dev Um objeto de classe \code{robo} contendo informações sobre servidor XML-RPC
#' @param x Número informando o quanto se deve movimentar no eixo z
#' @param a TRUE/FALSE, informa se o movimento se dá na referência absoluta ou relativa
#' @param r TRUE/FALSE, movimento relativo em relação ao ponto atual?
#' @param @sync TRUE/FALSE Se sincronizado, a função só retorna depois de executado o movimento
#'
#' @examples
#' moveZ(dev, 5, r=TRUE)
#'
#' @export
moveZ.robo <- function(dev, x, a=FALSE, r=FALSE, sync=FALSE){
    dev$rpc("move", '', '', x, a, r, sync)
}

#' Movimento relativo do robô cartesiano
#'
#' Executa os movimentos do robô cartesiano em relação à posição atual
#'
#' @param dev Um objeto de classe \code{robo} contendo informações sobre servidor XML-RPC
#' @param x Número informando o quanto se deve movimentar no eixo x
#' @param y Número informando o quanto se deve movimentar no eixo y
#' @param z Número informando o quanto se deve movimentar no eixo z
#' @param @sync TRUE/FALSE Se sincronizado, a função só retorna depois de executado o movimento
#'
#' @examples
#' rmove(dev, x=5)
#'
#' @export
rmove.robo <- function(dev, x='', y='', z='', sync=FALSE){
    dev$rpc("move", x, y, z, FALSE, TRUE,sync)
}

#' Movimento relativo do robô cartesiano no eixo x
#'
#' Executa os movimentos do robô cartesiano em relação à posição atual ao longo do eixo x
#'
#' @param dev Um objeto de classe \code{robo} contendo informações sobre servidor XML-RPC
#' @param x Número informando o quanto se deve movimentar no eixo x
#' @param @sync TRUE/FALSE Se sincronizado, a função só retorna depois de executado o movimento
#'
#' @examples
#' rmoveX(dev, 5)
#'
#' @export
rmoveX.robo <- function(dev, x, sync=FALSE){
    dev$rpc("move", x, '', '', FALSE, TRUE,sync)
}

#' Movimento relativo do robô cartesiano no eixo y
#'
#' Executa os movimentos do robô cartesiano em relação à posição atual ao longo do eixo y
#'
#' @param dev Um objeto de classe \code{robo} contendo informações sobre servidor XML-RPC
#' @param x Número informando o quanto se deve movimentar no eixo y
#' @param @sync TRUE/FALSE Se sincronizado, a função só retorna depois de executado o movimento
#'
#' @examples
#' rmoveY(dev, 5)
#'
#' @export
rmoveY.robo <- function(dev, x, sync=FALSE){
    dev$rpc("move", '', x, '', FALSE, TRUE,sync)
}

#' Movimento relativo do robô cartesiano no eixo z
#'
#' Executa os movimentos do robô cartesiano em relação à posição atual ao longo do eixo z
#'
#' @param dev Um objeto de classe \code{robo} contendo informações sobre servidor XML-RPC
#' @param x Número informando o quanto se deve movimentar no eixo z
#' @param @sync TRUE/FALSE Se sincronizado, a função só retorna depois de executado o movimento
#'
#' @examples
#' rmoveZ(dev, 5)
#'
#' @export
rmoveZ.robo <- function(dev, x, sync=FALSE){
    dev$rpc("move", '', '', x, FALSE, TRUE,sync)
}


#' Posição no sistema de referência abosluto
#'
#' Retorna a posição do robô cartesiano no sistema de referência absoluto.
#' O sistema de referência absoluto é o que está na placa controladora do Robo
#'
#' @param dev Um objeto de classe \code{robo} contendo informações sobre servidor XML-RPC
#' @return Lista contendo as coordenadas x, y e z.
#' @examples
#' absPosition(dev)
#'
#' @export
absPosition.robo <- function(dev){
    dev$rpc("abs_position")
}
#' Posição do robô 
#'
#' Retorna a posição do robô cartesiano no sistema de referência relativo.
#' O sistema de referência absoluto é o que está na placa controladora do Robo
#'
#' @param dev Um objeto de classe \code{robo} contendo informações sobre servidor XML-RPC
#' @return Lista contendo as coordenadas x, y e z.
#'
#' @examples
#' position(dev)
#'
#' @export
position.robo <- function(dev){
    dev$rpc("position")
}

#' Cria novo sistema de referência
#'
#' Configura um novo sistema de referência onde a posição atual do robô tem
#' valores \code{xref}, \code{yref}, \code{zref}. Este sistema de referência relativo
#' tem a mesma orientação que o sistema de placa controladora. O que muda é a origem.
#'
#' @param dev Um objeto de classe \code{robo} contendo informações sobre servidor XML-RPC
#' @param xref Valor que a coordenada x atual toma.
#' @param yref Valor que a coordenada y atual toma.
#' @param zref Valor que a coordenada z atual toma.
#'
#' @examples
#' setReference(dev, yref=100)
#' 
#' @export
setReference.robo <- function(dev, xref=0, yref=0, zref=0){
    dev$rpc("set_reference", xref, yref, zref, '')
}


#' Volta para o sistema de coordenadas absoluto.
#'
#' Volta o sistema de coordenadas para o sistema absoluto que está programado na placa
#' controladora.
#'
#' @param dev Um objeto de classe \code{robo} contendo informações sobre servidor XML-RPC
#'
#' @examples
#' setAbsReference(dev, yref=100)
#' 
#' @export
setAbsReference.robo <- function(dev){
    dev$rpc("set_abs_reference")
}

#' Executa o comando de home no eixo x
#'
#' Volta o robô para a origem do sistema do eixo x. Como sensores de fim de curso
#' estão em ambos os extremos do eixo, deve-se especificar em que direção se deseja ir.
#' \code{'+'} na direção de x positivo (em direção ao ventilador), \code{'-'} na direção
#' de x negativo (em direção à contração). Lembre que este movimento se dá em velocidade
#' bem baixa e recomenda-se realizar boa parte da movimentação com o comando \code{\link{move}}.
#'
#' @param dev Um objeto de classe \code{robo} contendo informações sobre servidor XML-RPC
#' @param s Caracter \code{'+'} ou \code{'-'} com direção do home.
#'
#' @examples
#' homeX(dev, '-')
#' 
#' @export
homeX.robo <- function(dev, s='+'){
    if (s %in% c('+', '-'))
        dev$rpc("home", s, 'X')
    else
        stop("Argumento s deve ser '+' ou '-'")
}

#' Executa o comando de home no eixo y
#'
#' Volta o robô para a origem do sistema do eixo y. Como sensores de fim de curso
#' estão em ambos os extremos do eixo, deve-se especificar em que direção se deseja ir.
#' \code{'+'} na direção de y positivo (lado da porta no túnel de vento), \code{'-'} na direção
#' de x negativo (lado das salas de controle do túnel de vento). Lembre que este movimento se dá em velocidade
#' bem baixa e recomenda-se realizar boa parte da movimentação com o comando \code{\link{move}}.
#'
#' @param dev Um objeto de classe \code{robo} contendo informações sobre servidor XML-RPC
#' @param s Caracter \code{'+'} ou \code{'-'} com direção do home.
#'
#' @examples
#' homeY(dev, '-')
#' 
#' @export
homeY.robo <- function(dev, s='-'){
    if (s %in% c('+', '-'))
        dev$rpc("home", s, 'Y')
    else
        stop("Argumento s deve ser '+' ou '-'")
}

#' Executa o comando de home no eixo z
#'
#' Volta o robô para a origem do sistema do eixo z. Como o eixo z tem apenas um sensor de fim de curso,
#' ao contrário das funções \code{\link{homeX}} e \code{\link{homeY}} não se necessita especificar a
#' orientação: o home é sempre para cima! Para manter compatibilidade, o argumento de direção existe
#' mas é ignorado.
#' 
#' @param dev Um objeto de classe \code{robo} contendo informações sobre servidor XML-RPC
#' @param s Argumento ignorado
#'
#' @examples
#' homeZ(dev)
#' 
#' @export
homeZ.robo <- function(dev, s='+'){
    dev$rpc("home", '+', 'Z')
}


#' Espere o movimento terminar
#'
#' A maioria dos comandos de movimentação do robô são assíncronos. O comando é dado e o programa
#' volta para o programador (usuário) imediatamente enquanto o movimento se realiza. Várias funções
#' têm o argumento \code{sync} que torna o comando síncrono. Mas no caso geral, esta função vai
#' simplesmente esperar que qualquer movimento programado termine antes de retornar o controle.
#' 
#' @param dev Um objeto de classe \code{robo} contendo informações sobre servidor XML-RPC
#'
#' @examples
#' move(dev, x=-500, sync=FALSE)
#' waitUntilDone(dev)
#' 
#' @export
waitUntilDone.robo <- function(dev){
    dev$rpc("waitUntilDone")
}

#' Paramada imediata
#'
#' Envia comando para o robô cartesiano para qualquer movimento.
#'
#' @param dev Um objeto de classe \code{robo} contendo informações sobre servidor XML-RPC
#'
#' @examples
#' moveX(dev, 100)
#' stopnow(dev)
#'
#' @export
stopnow.robo <- function(dev){
    dev$rpc("stop")
}



    
        
