# Lambda-man

Projet de programmation fonctionnelle.  

**Objectifs :**   
Programmer, en langage OCamL, un module de prise de décision d'un robot. Ce robot doit récolter des ressources et les amener à son vaisseau.

## Tâches effectuées

- Le robot est capable de récolter des ressources et de les ramener à son vaisseau.
- Le robot est capable de prendre le plus court chemin entre deux points, tout en évitant les zones mortelles, en utilisant l'algorithme de Dijkstra sur son graphe de visibilité.
- Selon le rapport bénéfice/risque le robot décide de traverser, ou pas, des zones lui infligeant des dégâts.
- Le robot est capable de travailler en équipe.
- Le programme est capable de générer des nouveaux mondes à explorer.
- L'interface graphique est modifiée pour la rendre plus esthétique.
- Optimisation des performances : construire le graphe de visibilité du robot à chaque nouvelle information plutôt qu'à chaque tour.

**Remarques :**
Les fichiers étaient fourni par notre enseignant. Le projet consistait à compléter les fichiers avec notre code. Voici le travail effectué :
- Dossier src/
      - Fichier decision.ml :
            - Implémentation de la fonction Decision.plan
            - Implémentation de la fonction Decision.next_action
            - Implémentation de la fonction Decision.visibility_graph
            - Implémentation de la fonction Decision.shortest_path
            - Implémentation de la fonction Decision.plan
      - Mise à jour du fichier worldGenerator.ml :
      - Mise à jour du fichier visualizer.ml :
- Dossier myworld/ :
      - tous les fichiers ont été générés par notre implémentation de worldGenerator.ml.

## Exécution du programme

### Requis

- Distribution GNU/Linux
- OPAM 2 installé

### Installation des dépendances
Dans le dossier courant :
- `opam init`
- `./configure`

### Pour lancer le projet :

Voir les différentes commandes dans `MANUEL_CMD.md`.  

Exemple :  

```
eval $(opam env)
make
./lambda server -v -w tests/00000-simple.json './lambda man -n 1 '
```
