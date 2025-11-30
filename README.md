# tas

[![Package Version](https://img.shields.io/hexpm/v/tas)](https://hex.pm/packages/tas)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/tas/)

```sh
gleam add tas@1
```


Further documentation can be found at <https://hexdocs.pm/tas>.

## Development

```sh
make build
gleam run   # Run the project
gleam test  # Run the tests
```

lien pour l'installation de gleam : https://gleam.run/getting-started/installing/
Il est aussi nécessaire d'avoir installé Erlang pour exécuter gleam : https://www.erlang-solutions.com/downloads-2/

Une des particularité de gleam est qu'il n'y a pas de mutabilité, ainsi pour représenter un état mutable, il faut utiliser le patterne acteur. (Un processus auquel on envoie des messages et qui met son état à jour de manière récursive)

# Projet Tas (Ribeiro Damien)

L'intégralité du sujet a été réalisé. De plus, j'ai fait certaine extension. 
Premièrement, Afin de lancer les test du projet, il faut faire make build puis gleam test. Les tests sont dans le code des tests sont dans le repertoire test. 

La première extension que j'ai ajouté est le passage de message par canal. La manière dont j'ai implémenté les canaux est similaire à Go: 

      // ----------------------------------
      //       env |- chan : ty
      // new equation : ty = new_type chan   
      // Chan déclare un nouveau canal, Chan est un terme expansif


      //      env |- channel : ty chan         env |- message : ty
      // ---------------------------------------------------------
      //       env |- Send(channel, message) : ty

      //      env |- channel : ty chan
      // ------------------------------------
      //      env |- Recv(channel) : ty

      // expr1 bien typé et expr2 bien typé
      // ------------------------------------
      // end |- Fork(expr1, expr2) : Unit

J'ai aussi ajouté Print et Println afin de pouvoir faire des effets de bors dans le Fork.

Enfin, j'ai implémenté les objets basiques(records) avec typage structurel comme nous les avions vu au cours 5 et 6, mais je n'ai eu que le temps de faire le typage pour les objets. De plus, je n'ai pas très bien tester les traits impératifs dans les objets. 
Avec le temps qui me restait j'ai préféré retravailler mon code afin qu'il soit plus lisible et afin de retirer des bugs.

De plus, j'ai fait un lexer et un parser pour reconnaitre le langage. En utilisant la commande, gleam run, il est possible d'entrer des termes, ils sont ensuite typer et interpréter. 
Cela ne fonctionne pas toujours très bien, en particulier, cela ne fonctionne pas pour les extensions.
