# ret

Juste un transformateur d'expression en [Rust](https://www.rust-lang.org/).

## Démarrage Rapide

```console
$ cargo run
ret>
```

## Langage

Le langage de Ret est constitué de 2 différentes instructions:
- Les **shapes**: Une expression à laquelle on applique des règles
	 - `<expression> { <règle | shape>* }`
 - Les **règles**: Une formule applicable à une expression
	 - `<nom> :: <tête> = <corps>`

On peut créer des *shapes* dans d'autres *shapes*.

Pour appliquer une règle à une expression, il faut préciser sa **stratégie**.
Il existe actuellement 4 portées différentes:
- **all**: Applique la règle à toutes les sous-expressions
- **deep**: Applique la règle récursivement à toutes les sous-expressions
- **first**: Applique la règle à la première sous-expression (pareil que `| 0`)
- **`<nombre>`**: Applique la règle à la `<nombre>`ème expression

Syntaxe: `<règle> | <stratégie>`

On peut **inverser** une règle en mettant un `!` après le `|`

Exemple: 
```c
swap :: A + B = B + A

(1 + (2 + 3)) { 	// Créé une nouvelle "shape"
	swap |  0     	// Applique "swap" à la première sous-expression
	swap |! all			// Applique la règle "swap" inversée à toutes les sous-expressions
}
```

## REPL

Ret s'utilise principalement dans le [REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop) (Read Eval Print Loop).

Le REPL ajoute les outils suivants:
- La stratégie **match**: Affiche toutes les sous-expressions avec leur index pour matcher celles-ci
- La commande **fit**: Affiche toutes les règles qui match une sous-expression de la shape actuelle.
- La commande **show**: Affiche la règle ou l'expression donnée
- La commande **load**: Charge un fichier source
- La commande **save**: Enregistre les éléments du REPL dans un fichier

Exemple:
```c
ret> (1 + (2 + 3)) {
1> swap | match
...
1> swap |! match
1> fit
...
1> fit !   // Pareil que fit, mais affiche les règles inversées peuvent être appliquées	
1> }
ret>
```

## Exemples

Jetez un coup d’œil au dossier [samples](./samples). Il contient des exemples d'utilisation de Ret.

### Entiers de [Peano](https://fr.wikipedia.org/wiki/Axiomes_de_Peano)

```c
load "./std/std.ret"   // Cette ligne charge la libraire standard

(2 + 3) {              // Créé une nouvelle "shape"
  3      | 0           // Applique la règle "3" à la première expression
  2      | all         // Applique la règle "2" à toutes les expressions
  1      | all
  sum    | all
  sum    | all
  sum_id | all
}

(4 - 3) {
  4      | 0
  3      | all
  2      | all
  1      | all
  sub    | all
  sub    | all
  sub    | all
  sub_id | all
}
```
