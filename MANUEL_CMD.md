# Interface graphique sans graphes de visibilité

Si besoin, commenter les lignes 942 et 943 puis taper dans le terminal :
```
eval $(opam env)
make
./lambda server      -v -w tests/00000-simple.json './lambda man -n 1 '
./lambda server      -v -w tests/00001-simple.json './lambda man -n 1 '
./lambda server      -v -w tests/00002-simple.json './lambda man -n 1 '
./lambda server      -v -w tests/00003-simple.json './lambda man -n 1 '
./lambda server      -v -w tests/00004-simple.json './lambda man -n 1 '
./lambda server      -v -w tests/00005-simple.json './lambda man -n 1 '
./lambda server      -v -w tests/00006-hell.json './lambda man -n 1 '
./lambda server      -v -w tests/00007-hell.json './lambda man -n 1 '
./lambda server      -v -w tests/00008-hell.json './lambda man -n 1 '
./lambda server      -v -w tests/00009-hell.json './lambda man -n 1 '
./lambda server      -v -w tests/00010-hell.json './lambda man -n 1 '
./lambda server      -v -w tests/00011-hell.json './lambda man -n 1 '
./lambda server      -v -w tests/00012-hell.json './lambda man -n 1 '
./lambda server      -v -w tests/00013-hell-and-suffering.json './lambda man -n 1 '
./lambda server      -v -w tests/00014-hell-and-suffering.json './lambda man -n 1 '
./lambda server      -v -w tests/00015-hell-and-suffering.json './lambda man -n 1 '
./lambda server      -v -w tests/00016-hell-and-suffering.json './lambda man -n 1 '
./lambda server      -v -w tests/00017-hell-and-suffering.json './lambda man -n 1 '
./lambda server      -v -w tests/00019-hell-and-suffering.json './lambda man -n 1 '
./lambda server      -v -w tests/00018-hell-and-suffering.json './lambda man -n 1 '
./lambda server      -v -w tests/00020-hell-and-suffering.json './lambda man -n 1 '
./lambda server      -v -w tests/00021-03-team.json './lambda man -n 3 '
./lambda server      -v -w tests/00022-03-team.json './lambda man -n 3 '
./lambda server      -v -w tests/00023-05-team.json './lambda man -n 5 '
./lambda server      -v -w tests/00024-10-team.json './lambda man -n 10 '
./lambda server      -v -w myworld/00-myworld.json './lambda man -n 1 '
./lambda server      -v -w myworld/02-myworld.json './lambda man -n 1 '
./lambda server      -v -w myworld/03-myworld.json './lambda man -n 1 '
./lambda server      -v -w myworld/04-myworld.json './lambda man -n 1 '
./lambda server      -v -w myworld/05-03-team-myworld.json './lambda man -n 3 '
./lambda server      -v -w myworld/06-04-team-myworld.json './lambda man -n 4 '
./lambda server      -v -w myworld/07-10-team-myworld.json './lambda man -n 10 '
./lambda server      -v -w myworld/08-10-team-myworld.json './lambda man -n 10 '
./lambda server      -v -w myworld/01-myworld.json './lambda man -n 1 '
;
```

## Interface graphique avec graphes de visibilité

Si besoin, décommenter les lignes 942 et 943 puis taper dans le terminal :

```
eval $(opam env)
make
./lambda server      -v -w tests/00000-simple.json './lambda man -n 1 -v'
./lambda server      -v -w tests/00001-simple.json './lambda man -n 1 -v'
./lambda server      -v -w tests/00002-simple.json './lambda man -n 1 -v'
./lambda server      -v -w tests/00003-simple.json './lambda man -n 1 -v'
./lambda server      -v -w tests/00005-simple.json './lambda man -n 1 -v'
./lambda server      -v -w tests/00004-simple.json './lambda man -n 1 -v'
./lambda server      -v -w tests/00006-hell.json './lambda man -n 1 -v'
./lambda server      -v -w tests/00007-hell.json './lambda man -n 1 -v'
./lambda server      -v -w tests/00009-hell.json './lambda man -n 1 -v'
./lambda server      -v -w tests/00008-hell.json './lambda man -n 1 -v'
./lambda server      -v -w tests/00010-hell.json './lambda man -n 1 -v'
./lambda server      -v -w tests/00011-hell.json './lambda man -n 1 -v'
./lambda server      -v -w tests/00012-hell.json './lambda man -n 1 -v'
./lambda server      -v -w tests/00013-hell-and-suffering.json './lambda man -n 1 -v'
./lambda server      -v -w tests/00014-hell-and-suffering.json './lambda man -n 1 -v'
./lambda server      -v -w tests/00015-hell-and-suffering.json './lambda man -n 1 -v'
./lambda server      -v -w tests/00016-hell-and-suffering.json './lambda man -n 1 -v'
./lambda server      -v -w tests/00017-hell-and-suffering.json './lambda man -n 1 -v'
./lambda server      -v -w tests/00018-hell-and-suffering.json './lambda man -n 1 -v'
./lambda server      -v -w tests/00019-hell-and-suffering.json './lambda man -n 1 -v'
./lambda server      -v -w tests/00020-hell-and-suffering.json './lambda man -n 1 -v'
;
```

## Pour générer des nouveaux mondes

Par exemple :
```
./lambda generate -p 1 -r 1 -h 50 -g 5 -t 10 -v > myworld/09-myworld.json
```
