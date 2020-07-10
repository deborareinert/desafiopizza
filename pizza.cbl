      *Divisão de identificação do programa
       identification division.
       program-id. "pizza".
       author. "Débora Reinert".
       installation. "PC".
       date-written. 09/07/2020.
       date-compiled. 09/07/2020.



      *Divisão para configuração do ambiente
       environment division.
       configuration section.
           special-names. decimal-point is comma.

      *-----Declaração dos recursos externos
       input-output section.
       file-control.
       i-o-control.

      *Declaração de variáveis
       data division.

      *----Variaveis de arquivos
       file section.


      *----Variaveis de trabalho
       working-storage section.

       01  relatorio  occurs  20.
           05 nome                                 pic x(15)
              value space.
           05 filler                               pic x(3)
              value " - ".
           05 diametro                             pic 9(3).
           05 filler                               pic x(3)
              value " - ".
           05 raio                                pic 9(3).
           05  filler                              pic x(3)
               value " - ".
           05 areaP                               pic 9(3).
           05 filler                               pic x(3)
              value " - ".
           05 preco                                pic 9(3)v99.
           05 filler                               pic x(3)
              value " - ".
           05 preco_cm2                            pic 9(4)v99.
           05 filler                               pic x(3)
              value " - ".
           05 dif_preco                            pic 9(3)v99.

       77  ind                                     pic 9(3)v99.
       77  menu                                    pic x(1).
       77  controle                                pic x(10).
       77  nome2                                   pic x(15).
       77  aux                                     pic 9(3)v99.
       77  pizza                                   pic 9(3).
       77  delta_preco                             pic 9(3)v99.

      *----Variaveis para comunicação entre programas
       linkage section.


      *----Declaração de tela
       screen section.


      *Declaração do corpo do programa
       procedure division.

           perform inicializa.
           perform processamento.
           perform finaliza.

      * Inicilizacao de variaveis, abertura de arquivos
      * procedimentos que serao realizados apenas uma vez
       inicializa section.
           move   space       to     menu
           .
       inicializa-exit.
           exit.


       processamento section.
           move 0 to pizza
           move 0 to ind
             perform until menu = "N"
               display erase
               add 1 to ind
                   display erase

               if ind > 20 then
                   display "Vc atingiu o limite de 20 pizzas"
               else
                   add 1 to pizza
                   display "Informe o nome da pizza "
                   accept nome(ind)

                   display "Informe o diametro "
                   accept diametro(ind)

                   display "Informe o preco "
                   accept preco(ind)

               end-if
               display "deseja cadastrar mais uma pizza? ('S'/'N')"
               accept menu

                   perform precocm2
                   perform ordem
                   perform porcentagem

           end-perform

           perform varying ind from 1 by 1 until ind > 20
                                              or nome(ind) = space
               display relatorio(ind)

           end-perform
                .
       processamento-exit.
           exit.

       precocm2 section.

           compute raio(ind) = diametro(ind) / 2

           compute areaP(ind) = 3,14 * (raio(ind) * raio(ind))

           compute preco_cm2(ind) = preco(ind) / areaP(ind)
           .
       precocm2-exit.
           exit.

       ordem section.
           move "trocou" to controle
           perform until controle <> "trocou"
               move 1 to ind
               move "n trocou" to controle

               perform until ind = pizza
                   if preco_cm2(ind) > preco_cm2(ind + 1) then
                     move preco_cm2(ind + 1) to aux
                     move preco_cm2(ind)     to preco_cm2(ind + 1)
                     move aux                to preco_cm2(ind)

                     move nome(ind + 1) to nome2
                     move nome(ind)  to nome(ind + 1)
                     move nome2    to nome(ind)

                     move diametro(ind + 1) to aux
                     move diametro(ind) to diametro(ind + 1)
                     move aux      to diametro(ind)

                     move raio(ind + 1) to aux
                     move raio(ind) to raio(ind + 1)
                     move aux      to raio(ind)

                     move areaP(ind + 1) to aux
                     move areaP(ind) to areaP(ind + 1)
                     move aux      to areaP(ind)


                     move preco(ind + 1) to aux
                     move preco(ind) to preco(ind + 1)
                     move aux to preco(ind)

                     move areaP(ind + 1) to aux
                     move areaP(ind) to areaP(ind + 1)
                     move aux to areaP(ind)

                     move "trocou"           to controle
                   end-if
                   add 1 to ind
               end-perform
           end-perform

              .
       ordem-exit.
           exit.
       porcentagem section.
           move 1 to ind
      *    move 0 to porcentag(ind)
      *    move 0 to dif_preco
              perform until ind = 20 or nome(ind + 1) = space
      *         move 0 to porcentag(ind)
      *         move 0 to dif_preco
                compute delta_preco = preco_cm2(ind + 1)
                 - preco_cm2(ind)
                compute dif_preco(ind + 1 )=(delta_preco * 100)
                 / preco_cm2(ind)

                add 1 to ind
              end-perform
              .
       porcentagem-exit.
           exit.
       finaliza section.
           display "Fim da operação"
           Stop run
           .
       finaliza-exit.
           exit.













