# minigames

## MCTA016-13 - Projeto de Paradigmas de Programação

## Emilio Francesquini

### UFABC 2019.Q2

|            Nome           |    RA    |
|:-------------------------:|:--------:
| Arthur Henrique Fernandes | 11061816 |
|   Emerson Almeida Matos   | 11101015 |

#### Problema

Construção de minigames na linguagem de programação Haskell utilizando o paradigma de programação funcional

##### Proposta de implementação

O objetivo deste projeto é implementar um jogo de tabuleiro baseado no famoso [Jogo de Damas](https://pt.wikipedia.org/wiki/Damas) utilizando a biblioteca gráfica [Gloss](http://hackage.haskell.org/package/gloss).

O jogo se baseia num confronto entre dois jogadores, peças pretas contra peças brancas. Ao iniciar a aplicação, a interface gráfica já carregará o tabuleiro e as peças, sendo o jogador das peças brancas responsável pelo primeiro movimento, e na sequência o jogador das peças pretas. O jogo segue alternando entre os jogadores, até que um seja o vencedor (eliminar todos as peças do adversário) ou ambos possuam apenas uma peça, nesse caso é considero empate.

A avaliação do projeto ocorrerá após o término da implementação dos códigos, através de testes que busquem demonstrar a funcionalidade da aplicação, bem como sua interatividade com o usuário.

###### Observação: nesta implementação do jogo, não há promoção de peças.

#### Cronograma

|            Etapa           |    Previsão de conclusão    |
|:-------------------------:|:--------:
| Levantamento de requisitos | 30/06/19 |
| Definição da interface do usuário | 07/07/19 |
| Implementação do jogo | 05/08/19 |
|   Testes e correção de bugs   | 23/08/19 |


### COMPILAÇÃO E EXECUÇÃO

Antes de compilar: certificar-se de ter o ghci, stack e a biblioteca Gloss na sua máquina. Clonar o repositório e abrir o terminal no diretório do projeto.

Compilar: executar o comando ```cd minigames``` e depois ```stack build```

Rodar: executar o comando ```stack run``` ou ```stack exec minigame```

Rodar testes automatizados:  executar o comando ```stack test```

