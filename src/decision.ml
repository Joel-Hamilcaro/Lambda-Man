(**

   Chers programmeuses et programmeurs de λman, votre mission consiste
   à compléter ce module pour faire de vos λmen les meilleurs robots
   de la galaxie. C'est d'ailleurs le seul module qui vous pouvez
   modifier pour mener à bien votre mission.

   La fonction à programmer est

         [decide : memory -> observation -> action * memory]

   Elle est appelée à chaque unité de temps par le Λserver avec de
   nouvelles observations sur son environnement. En réponse à ces
   observations, cette fonction décide quelle action doit effectuer le
   robot.

   L'état du robot est représenté par une valeur de type [memory].  La
   fonction [decide] l'attend en argument et renvoie une nouvelle
   version de cette mémoire. Cette nouvelle version sera passée en
   argument à [decide] lors du prochain appel.

*)

open World
open Space

(** Le Λserver transmet les observations suivantes au λman: *)
type observation = World.observation

(** Votre λman peut se déplacer : il a une direction D et une vitesse V.

    La direction est en radian dans le repère trigonométrique standard :

    - si D = 0. alors le robot pointe vers l'est.
    - si D = Float.pi /. 2.  alors le robot pointe vers le nord.
    - si D = Float.pi alors le robot pointe vers l'ouest.
    - si D = 3 * Float.pi / 2 alors le robot pointe vers le sud.
    (Bien entendu, ces égalités sont à lire "modulo 2 * Float.pi".)

    Son déplacement en abscisse est donc V * cos D * dt et en ordonnée
   V * sin D * dt.

    Votre λman peut communiquer : il peut laisser du microcode sur sa
   position courante pour que d'autres λmen puissent le lire.  Un
   microcode s'autodétruit au bout d'un certain nombre d'unités de
   temps mais si un microcode est laissé près d'un autre microcode
   identique, ils fusionnent en un unique microcode dont la durée de
   vie est le somme des durées de vie des deux microcodes initiaux.
   Construire un microcode demande de l'énergie au robot : chaque
   atome lui coûte 1 point d'énergie. Heureusement, l'énergie augmente
   d'1 point toutes les unités de temps.

    Pour terminer, votre λman peut couper des arbres de Böhm. Les
   arbres de Böhm ont un nombre de branches variables. Couper une
   branche prend une unité de temps et augmente le score de 1
   point. Si on ramène cette branche au vaisseau, un second point est
   accordé.

    Pour finir, le monde est malheureusement très dangereux : on y
   trouve des bouches de l'enfer dans lesquelles il ne faut pas tomber
   ainsi que des champs de souffrances où la vitesse de votre robot
   est modifiée (de -50% à +50%).

*)

type action =
  | Move of Space.angle * Space.speed
  (** [Move (a, v)] est l'angle et la vitesse souhaités pour la
     prochaine unité de temps. La vitesse ne peut pas être négative et
     elle ne peut excéder la vitesse communiquée par le serveur. *)

  | Put of microcode * Space.duration
  (** [Put (microcode, duration)] pose un [microcode] à la position courante
      du robot. Ce microcode s'autodétruira au bout de [duration] unité de
      temps. Par contre, s'il se trouve à une distance inférieure à
      [Space.small_distance] d'un microcode similaire, il est fusionné
      avec ce dernier et la durée de vie du microcode résultant est
      la somme des durées de vide des deux microcodes. *)

  | ChopTree
  (** [ChopTree] coupe une branche d'un arbre de Böhm situé une distance
      inférieure à [Space.small_distance] du robot. Cela augmente le score
      de 1 point. *)

  | Wait
  (** [Wait] ne change rien jusqu'au prochain appel. *)

  | Die of string
  (** [Die] est produit par les robots dont on a perdu le signal. *)

[@@deriving yojson]

(**

   Le problème principal de ce projet est le calcul de chemin.

   On se dote donc d'un type pour décrire un chemin : c'est une
   liste de positions dont la première est la source du chemin
   et la dernière est sa cible.

*)
type path = Space.position list

(** Version lisible des chemins. *)
let string_of_path path =
  String.concat " " (List.map string_of_position path)

(* ************************************************************************************************************** *)
(* ************************************************************************************************************** *)
(* ************************************************************************************************************** *)
(* ************************************************************************************************************** *)

(* FONCTIONS CONCERNANT L'AFFICHAGE *)

(** Version lisible d'une arête. *)
let string_of_edge ((a,b), (c,d), e) = Printf.sprintf "( (%f,%f) , (%f,%f) , [%f] )\n" a b c d e

(** Version lisible d'une liste d'arêtes. *)
let string_of_edges edges =
  String.concat " " (List.map string_of_edge edges)

(* Fonctions d'affichage : liste de int *)

let print_list l =
  Printf.eprintf "[ " ;
  List.iter (Printf.eprintf "%d ") l;
  Printf.eprintf "]\n"

(* Fonctions d'affichage : liste de float *)

let print_listf l =
  Printf.eprintf "[ " ;
  List.iter (Printf.eprintf "%f ") l;
  Printf.eprintf "]\n"

(* ************************************************************************************************************** *)
(* ************************************************************************************************************** *)
(* ************************************************************************************************************** *)
(* ************************************************************************************************************** *)

(**

   Nous vous proposons de structurer le comportement du robot
   à l'aide d'objectifs décrits par le type suivant :

*)
type objective =
  | Initializing            (** Le robot doit s'initialiser.       *)
  | Chopping                (** Le robot doit couper des branches. *)
  | GoingTo of path * path
  (** Le robot suit un chemin. Le premier chemin est la liste des
      positions restantes tandis que le second est le chemin initial.
      On a donc que le premier chemin est un suffixe du second. *)

(** Version affichable des objectifs. *)

let string_of_objective = function
  | Initializing -> "initializing"
  | Chopping -> "chopping"
  | GoingTo (path, path2) ->
    let s1 = Printf.sprintf "going to %s" (String.concat " " (List.map string_of_position path)) in
    let s2 = Printf.sprintf "-> %s" (String.concat " " (List.map string_of_position path2)) in
    String.concat "\n" [s1;s2]


(**

  Comme dit en introduction, le robot a une mémoire qui lui permet de
   stocker des informations sur le monde et ses actions courantes.

  On vous propose de structurer la mémoire comme suit:

*)
type memory = {
    known_world : World.t option;      (** Le monde connu par le robot.     *)
    graph       : Graph.t;             (** Un graphe qui sert de carte.     *)
    objective   : objective;           (** L'objectif courant du robot.     *)
    targets     : Space.position list; (** Les points où il doit se rendre. *)
}

(**

   Initialement, le robot ne sait rien sur le monde, n'a aucune cible
   et doit s'initialiser.

*)
let initial_memory = {
    known_world = None;
    graph       = Graph.empty;
    objective   = Initializing;
    targets     = [];
}


(* ************************************************************************************************************** *)
(* ************************************************************************************************************** *)
(* ************************************************************************************************************** *)
(* ************************************************************************************************************** *)

(* FONCTIONS CONCERNANT VISIBILITY_GRAPH *)

(* Distance entre deux noeuds *)

let dist a b =
  sqrt (((x_ b)-.(x_ a))*.((x_ b)-.(x_ a))+.((y_ b)-.(y_ a))*.((y_ b)-.(y_ a)))

(** Renvoie tous les segments des champs de souffrance dont le frottement f
  respecte le critère défini par "compare f a" avec compare une fct de comparaison *)

let crit_segments world compare a = List.(
    Space.polygons world.space (function Ground f when ( compare f a ) -> true | _ -> false)
    |> map Space.polygon_segments
    |> flatten
  )

(** Renvoie tous les segments des champs de souffrance *)

let suff_segments world = crit_segments world (<>) 1.

(** Renvoie tous les segments des champs de souffrance qui reduisent la vitesse *)

let bad_suff_segments world = crit_segments world (<) 1.

(* Valeur du frottement du champ de souffrance *)

let speed_modif world_t pos = (suffering (world_t) pos)

(* Comme inside_hell mais pour les champs de souffrance *)

let inside_suff world_t pos = (suffering (world_t) pos) <> 1.

(* Comme inside_hell mais pour les champs de souffrance qui réduisent la vitesse *)

let inside_bad_suff world_t pos = (suffering (world_t) pos) < 1.

(* Vérifie si un segment v ne traverse pas la zone delimite par l *)

let rec traverse_pas (v : Space.segment) (l : Space.segment list) : bool =
  match l with
  | [] -> true
  | t::q when segment_intersects t v -> false
  | t::q -> traverse_pas v q

(** Verifie si le segment v ne traverse pas une bouche de l'enfer
    Verifie si le segment v ne traverse pas un "mauvais" champs si verysafe *)

let seg_safe (v : Space.segment) world_t verysafe : bool =
  traverse_pas v ( hell_segments (world_t) )
  && (traverse_pas v ( bad_suff_segments (world_t) ) || verysafe)

(* Transforme une liste de segments en liste de noeuds *)

let segs_to_nodes (l : Space.segment list) : Graph.node list =
  let rec aux (l : Space.segment list) (acc : Graph.node list) : Graph.node list =
    match l with
    | [] -> acc
    | (v1,v2)::q -> aux (q) (v1::v2::acc)
  in
  aux l []

(* Transforme une liste de noeuds en liste de segments *)

let nodes_to_edges (l1 : Graph.node list) :  Graph.edge list =
  let rec aux
      (l1 : Graph.node list)
      (l2 : Graph.node list)
      (l : Graph.node list)
      (acc : Graph.edge list) : Graph.edge list =
    match l1,l2 with
    | [],_ -> acc
    | t1::q1,[] -> aux q1 l l acc
    | t1::q1,t2::q2 when x_ t1 == x_ t2 && y_ t1 == y_ t2 ->
      aux (t1::q1) q2 l acc
    | t1::q1,t2::q2 -> aux (t1::q1) (q2) l ((t1,t2,(dist t1 t2))::acc)
    (*poids des arêtes à modifier *)
  in
  aux l1 l1 l1 []

(* Liste de aretes telle que les aretes ne sont pas dans une bouche de l'enfer *)

let correct_edges (l1 : Graph.edge list) (world_t) verysafe =
  let rec aux  (l1 : Graph.edge list) (acc : Graph.edge list) : Graph.edge list =
    match l1 with
    | [] -> acc
    | (v1,v2,x)::q1
      when seg_safe (v1,v2) (world_t) verysafe -> aux q1 ( (v1,v2,x)::acc )
    |  v1::q1 -> aux q1 (acc)
  in
  aux l1 []

(** Liste des noeuds autour d'une zone en prenant une marge de distance d
    f est la fonction si une position est dans une zone *)

let margin_nodes l d world_t f =
  let rec aux l acc =
    match l with
    | [] -> acc
    | t::q when f (world_t) ( (x_ t) -. d , (y_ t) -. d ) -> aux q ((( (x_ t) +. d , (y_ t) +. d ))::acc)
    | t::q when f (world_t) ( (x_ t) -. d , (y_ t) +. d ) -> aux q ((( (x_ t) +. d , (y_ t) -. d ))::acc)
    | t::q when f (world_t) ( (x_ t) +. d , (y_ t) -. d ) -> aux q ((( (x_ t) -. d , (y_ t) +. d ))::acc)
    | t::q when f (world_t) ( (x_ t) +. d , (y_ t) +. d ) -> aux q ((( (x_ t) -. d , (y_ t) -. d ))::acc)
    | t::q -> aux q acc
  in
  aux l []


(* Liste des noeuds autour de l'enfer en prenant une marge de distance d *)

let hell_margin_nodes l d world_t = margin_nodes l d world_t inside_hell

(* Idem pour les souffrances *)

let suff_margin_nodes l d world_t = margin_nodes l d world_t inside_suff


(* ************************************************************************************************************** *)
(* ************************************************************************************************************** *)
(* ************************************************************************************************************** *)
(* ************************************************************************************************************** *)

(*  VISIBILITY GRAPH *)

(**

   Pour naviguer dans le monde, le robot construit une carte sous la
   forme d'un graphe.

   Les noeuds de ce graphe sont des positions clées
   du monde. Pour commencer, vous pouvez y mettre les sommets des
   polygones de l'enfer, le vaisseau, le robot et les arbres.

   Deux noeuds sont reliés par une arête si le segment dont ils
   sont les extremités ne croisent pas une bouche de l'enfer.

*)

let make_graph observation memory verysafe =

  (* monde *)
  let seen_world = world_of_observation observation in
  let known_world =
    match memory.known_world with
    | None -> seen_world
    | Some x -> x
  in
  let world =
    if verysafe then known_world
    else seen_world
  in

  (* noeuds des arbres *)
  let target_nodes =
    if List.length memory.targets > 1 then List.tl memory.targets
    else (tree_positions (observation.trees))
  in

  (* noeuds de l'enfer *)
  let hell_nodes_without_margin = segs_to_nodes (hell_segments (world) ) in (* sans marge *)
  let hell_nodes = hell_margin_nodes hell_nodes_without_margin 2.5 world in (* avec marges *)

  (* noeuds de souffrance *)
  let suff_nodes_without_margin = segs_to_nodes (suff_segments (world) ) in  (* sans marge *)
  let suff_nodes = suff_margin_nodes suff_nodes_without_margin 2. world in (* avec marges *)

  (*tous les noeuds et arêtes *)
  let nodes = (observation.position)::(observation.spaceship)::hell_nodes@target_nodes@suff_nodes in
  let edges = correct_edges (nodes_to_edges nodes) (if verysafe then known_world else world) verysafe in

  Graph.make (nodes) (edges)

(* graphe du monde connu par le robot *)

let known_world_graph observation memory =
  make_graph observation memory true

(* graphe de visibilité (ce que voit le robot) *)

let visibility_graph observation memory =
  make_graph observation memory false


(* ************************************************************************************************************** *)
(* ************************************************************************************************************** *)
(* ************************************************************************************************************** *)
(* ************************************************************************************************************** *)

(* FONCTIONS CONCERNANT SHORTEST_PATH *)

(* Renvoie le minimum de f *)

let rec get_min d f min =
  match f with
  | [] -> min
  | t::q when d.(t)<d.(min) -> get_min d q t
  | _::q -> get_min d q min

(* Renvoie f sans son minimum m *)

let extraire_min d f m =
  if  (List.length f)=1 then []
  else
    let comp_fun a = (a <> m ) in
    List.filter (comp_fun) f



(* ************************************************************************************************************** *)
(* ************************************************************************************************************** *)
(* ************************************************************************************************************** *)
(* ************************************************************************************************************** *)

(* SHORTEST_PATH *)

(**

   Il nous suffit maintenant de trouver le chemin le plus rapide pour
   aller d'une source à une cible dans le graphe.

*)

let shortest_path (graph : Graph.t) (source : Graph.node) (target : Graph.node) : path =

  if Space.close source target 1. then [source;target]
  else begin

    (* Constantes *)
    let sommets_list = Graph.nodes (graph) in
    let nb_sommets = List.length (sommets_list) in
    let sommets = Array.of_list sommets_list in (* id -> sommet *)
    let id_sommets = List.init (nb_sommets) (fun i -> ((Array.get sommets i),i) ) in
    let id a = List.assoc a id_sommets in (* sommet -> id *)

    (* Initialisation *)
    let d = Array.init nb_sommets (fun i -> if (i)=(id source) then 0. else infinity ) in (* distances *)
    let p = Array.init nb_sommets (fun i -> i ) in (* pères *)
    let f = ref ( List.init nb_sommets (fun i -> i ) ) in (* file *)

    (* Algo Dijkstra *)
    while (List.length !f)>0 do
      let id_u = get_min d !f (List.hd !f) in
      f := extraire_min d !f id_u ;
      let uv_list = Graph.out graph ( sommets.(id_u) ) in
      List.iter ( fun (u,v,w) ->
          if d.(id v) > ( d.(id u) +. w ) then begin
            d.(id v) <- ( d.(id u) +. w );
            p.(id v) <- id u;
          end
      ) uv_list ;
    done;

    (* Renvoie le plus court chemin à partir du tableaux p de Dijkstra *)
    let rec path_to_target t path =
      let id_pere = p.(id t) in
      match id_pere with
      | x when x = (id t) ->
        []
      | x when x = (id source) -> (sommets.(x))::path
      | x when Space.close (sommets.(x)) (t) 1. -> path_to_target (sommets.(x)) (path)
      | x -> let sx = (sommets.(x)) in
             path_to_target sx ( (sx) :: path)
    in

    path_to_target target [target]

  end


(* ************************************************************************************************************** *)
(* ************************************************************************************************************** *)
(* ************************************************************************************************************** *)
(* ************************************************************************************************************** *)

(* DISCOVER *)

(**

   Traditionnellement, la fonction de prise de décision d'un robot
   est la composée de trois fonctions :

   1. "discover" qui agrège les observations avec les observations
      déjà faites dans le passé.

   2. "plan" qui met à jour le plan courant du robot en réaction
      aux nouvelles observations.

   3. "next_action" qui décide qu'elle est l'action à effectuer
       immédiatement pour suivre le plan.

*)

(** [discover] met à jour [memory] en prenant en compte les nouvelles
    observations. *)
let discover visualize observation memory =
  let seen_world = World.world_of_observation observation in
  let known_world , new_known_world =
    match memory.known_world with
    | None -> seen_world, seen_world
    | Some known_world ->
      known_world, World.extends_world known_world seen_world
  in
  let new_graph =
    if new_known_world = known_world && memory.known_world<> None then memory.graph
    else if observation.epoch > 10000 then known_world_graph observation memory
    else visibility_graph observation memory
  in
  if visualize then Visualizer.show ~force:true known_world;
  { memory with known_world = Some known_world ; graph = new_graph }


(* ************************************************************************************************************** *)
(* ************************************************************************************************************** *)
(* ************************************************************************************************************** *)
(* ************************************************************************************************************** *)

(* FONCTIONS CONCERNANT PLAN *)

(* Tri une liste de position de la plus proche de pos à la plus lointaine *)

let sort_targets pos l =
  let comp_fct a b =
    if (dist pos a) = (dist pos b) then 0
    else if (dist pos a) > (dist pos b) then 1
    else -1
  in
  List.fast_sort comp_fct l

(* Renvoie toujours un PCC même si le visibility_graph n'en voit pas *)
let safe_shortest_path source target observation memory =
  let sp = shortest_path (visibility_graph observation memory) source target in
  match sp with
  | [] -> shortest_path (known_world_graph observation memory) source target
  | _ -> sp


(** Renvoie la sous-liste des éléments compris entre les position a et b
    Attention : L'ordre des éléments de la sous-liste est inversé *)

let rev_sub_list l a b =
  let rec aux l i res =
    match l with
    | [] -> res
    | t::q when i<a -> aux q (i+1) res
    | t::q when i>=a && i<=b -> aux q (i+1) (t::res)
    | _ -> res
  in
  aux l 0 []

(** Renvoie la liste des pos dont il reste des branches ou spaceship *)
let pos_rest l t obs =

  (List.filter (fun pos ->
       match (tree_at t pos) with
       | Some x when x.branches>0 -> true
       | None when pos=obs.spaceship -> true
       | _ -> false ) l)

(* Renvoie le dernier élément de la liste (Raccourci syntaxique) *)

let last (l : 'a list) : 'a  = List.nth l ( (List.length l)-1 )

(* ************************************************************************************************************** *)
(* ************************************************************************************************************** *)
(* ************************************************************************************************************** *)
(* ************************************************************************************************************** *)

(* PLAN *)

  (**

   [plan] doit mettre à jour la mémoire en fonction de l'objectif
   courant du robot.

   Si le robot est en train de récolter du bois, il n'y a rien à faire
     qu'attendre qu'il est fini. (Cas n°1)

   Si le robot est en train de suivre un chemin, il faut vérifier que
   ce chemin est encore valide compte tenu des nouvelles observations
     faites, et le recalculer si jamais ce n'est pas le cas. (Cas n°2)

   Si le robot est en phase d'initialisation, il faut fixer ses cibles
     et le faire suivre un premier chemin. (Cas n°3)

  *)

(**
   Tâche 4 : A l'initialisation, au lieu de fixer les cibles, on se donne
    pour objectif de communiquer. Pour cela, on va générer une target unique
    (à très forte probabilité) à notre robot. (Cas n°3 modifié).

    Dans next_action cette target sera transmise via microcode et permettra
    au robot de se distinguer des autres. (voir next_action)

    Convention : Objectif de communication = Chopping à proximité d'aucun
    arbre.
*)

let plan visualize observation memory =
  match memory.objective with
  (* Cas n°1 *)
  | Chopping -> memory
  (* Cas n°2 *)
  | GoingTo(prev1,prev2) ->

    let current_target = (if List.length memory.targets > 1 then List.nth memory.targets 1 else observation.spaceship) in
      let targets = pos_rest (List.tl memory.targets) observation.trees observation in
      let sp = safe_shortest_path observation.position current_target observation memory in
      {
        known_world = memory.known_world;
        graph =  memory.graph;
        objective =
          GoingTo( [observation.position], if List.length sp = 0 then prev1 else List.tl sp  ) ;
        targets = (List.hd memory.targets)::(sort_targets observation.position (targets)) ;
      }
  (* Le robot est rentré, il attend le retour des autres *)
  | Initializing when List.length (pos_rest (tree_positions observation.trees) observation.trees observation)=0->
    {
      known_world = memory.known_world;
      graph = Graph.empty;
      objective = Initializing ;
      targets = [] ;
    }
  (* Cas n°3 modifié *)
  | Initializing ->
    let id1 = Random.int 1073741823 and id2 = Random.int 1073741823 in
    {
      known_world = memory.known_world;
      graph = visibility_graph observation memory;
      objective = Chopping ; (*Chopping près du vaisseaux implique poser microcode *)
      targets = [ (float_of_int id1, float_of_int id2) ] ;
    }

(* ************************************************************************************************************** *)
(* ************************************************************************************************************** *)
(* ************************************************************************************************************** *)
(* ************************************************************************************************************** *)

(* FONCTIONS CONCERNANT NEXT_ACTION *)


(* Renvoie la même liste, dernier exclus *)

let without_last l =
  List.filter (fun a -> a<>(last l)) l

(* Renvoie la position de la première occurrence de a *)

let find_index a l =
  let rec aux a l acc =
    match l with
    | [] -> -1
    | t::q when a=t -> acc
    | t::q -> aux a q acc+1
  in
  aux a l 0


(** On veut partager un intervalle de longueur len en n sous-intervalles.
    Renvoie la liste des sous-intervalles *)

let bounds_list len n =
  if n<=1 then [(0,len-1)]
  else
    let sublen =
      len/n
    in
    let rec aux l acc =
      match l with
      | li when ( List.length li )<n-1 -> aux ( (sublen*acc,sublen*(acc+1)-1)::li ) (acc+1)
      | li -> ( (sublen*acc,len-1)::li )
    in
    aux [] 0

(* Sépare une liste de position en n sous-listes de longueur à peu près egales *)

let separate l n =
  let bl = bounds_list (List.length l) n in
  let rec aux bl acc =
    match bl with
    | [] -> acc
    | (a,b)::q -> aux q ((rev_sub_list l a b)::acc)
  in
  aux bl []

(** Renvoie un microcode dont le message est la target qui distingue le
    robot (voir fonction plan) *)

let my_signal memory i =
  let id1 = ( MicroAtom (int_of_float (x_ (List.hd memory.targets)) ) ) in
  let id2 = ( MicroAtom (int_of_float (y_ (List.hd memory.targets)) ) ) in
  MicroList [id1;id2]

(* Renvoie une liste de left_microcode en liste de microcodes *)

let messages_to_microcodes l = List.map (fun a -> a.microcode) l

(** Renvoie la Space.position correspondant à un microcode
    de la forme my_signal (voir fonction my_signal) *)

let signal_to_pos (signal) =
  match signal with
  | MicroList [ (MicroAtom a) ; (MicroAtom b) ]
    -> (float_of_int a, float_of_int b)
  | _ -> (-1.,-1.)

(** Renvoie une liste de Space.position à partir d'une liste de
    microcodes de la forme my_signal (voir fonction my_signal) *)

let signals_to_pos l = List.map (signal_to_pos) (l)

(** Renvoie un arbre proche de [pos] (mais pas assez), s'il y en a un. *)
let tree_at2 trees pos =
  List.find_opt
    (fun tree -> Space.(close tree.tree_position pos 5.))
    trees

(* ************************************************************************************************************** *)
(* ************************************************************************************************************** *)
(* ************************************************************************************************************** *)
(* ************************************************************************************************************** *)

(* FONCTIONS CONCERNANT NEXT_ACTION *)

(**

   Next action doit choisir quelle action effectuer immédiatement en
   fonction de l'objectif courant.

   Si l'objectif est de s'initialiser, la plannification a mal fait
   son travail car c'est son rôle d'initialiser le robot et de lui
   donner un nouvel objectif. (Cas n°1)

   Si l'objectif est de couper du bois, coupons du bois! (Cas n°2)

   Si on vient de couper la dernière branche, alors il faut changer d'objectif
   pour se déplacer vers une autre cible. (Cas n°3)

   Si l'objectif est de suivre un chemin, il faut s'assurer que
   la vitesse et la direction du robot sont correctes. (Cas n°4)

*)

(**
   Tache 4 : Si l'objectif est de communiquer :

   S'il n'y a aucun microcode, on doit en poser un en communiquant la
   target unique propre au robot -- voir fonction plan. (Cas n°5)

   S'il y a des microcodes au sol (qui correspondent aux targets propres
   à chaque robot), il faut les lire et mettre à jour memory.targets.
   Si les microcodes sont differents de ceux stockés dans memory.targets,
   on met à jour memory.targets et on ajoute son propre microcode si il n'est
   pas encore présent au sol. (Cas n°6.1)
   Si les microcodes sont les mêmes de ceux stockés dans memory.targets,
   la communication n'évolue pas, donc tous les robots on placés leur microcode.
   Chaque robot connaît donc toute l'équipe, il faut leur repartir les arbres.
   On met à jour memory avec cette fois les targets qui correspondent aux arbres
   (on n'a plus besoin des targets qui servaient à identifier les autres robots
    mais on garde celle propre a soi-même au cas où on aurait besoin de la
    recommuniquer) (Cas n°6.2)

   Convention : Objectif de communication = Chopping avec aucun arbre à proximité.

*)

let next_action visualize observation memory =

  match memory.objective , (* objectif *)
        (tree_at observation.trees observation.position), (* arbre à proximité *)
        memory.targets, (* targets *)
        (tree_at2 observation.trees observation.position)
  with

  (* L'objectif est Chopping mais le robot continue de bouger *)
  | Chopping,_,_,_ when (float_of_speed observation.speed) > 0. ->
    Move ( Space.angle_of_float (Random.float (2.*.(Float.pi))) , Space.speed_of_float 0. ),
    memory

  (* Initialisation equipe *********************************************************************)

  (* Cas n°5 : Aucun microcode *)
  | Chopping,None,_,_ when (List.length observation.messages)=0 ->
    Put ( (my_signal memory (List.length observation.messages) ) , duration_of_int 1000 ),
    memory

  (* Cas n°6.1 : Il existe au moins un microcode dont je n'ai pas enregistré *)
  | Chopping, None, _,_ when (List.length observation.messages) > 0
                        && (List.length observation.messages) <> (List.length memory.targets)-1 ->
    let id_list = (signals_to_pos (messages_to_microcodes observation.messages)) in
    let my_unique_pos = List.hd memory.targets in
    let my_left_micro_exists = List.exists (fun a -> a=(my_unique_pos)) id_list in
    if my_left_micro_exists then (* Si mon microcode est présent *)
        Wait,
        {
          known_world = memory.known_world;
          graph =  memory.graph;
          objective =  Chopping;
          targets = (List.hd memory.targets)::(id_list) ;
        }
    else (* Si mon microcode n'est pas présent *)
      Put ( (my_signal memory (List.length observation.messages) ) , duration_of_int 1000),
      {
        known_world = memory.known_world;
        graph =  memory.graph;
        objective =  Chopping;
        targets = (List.hd memory.targets)::(id_list) ;
      }

  (* Cas n°6.2 : il existe des microcodes qui sont tous enregistrés *)
  | Chopping, None, _ ,_ when (List.length observation.messages) > 0 ->
    let id_list = (signals_to_pos (messages_to_microcodes observation.messages)) in
    let targets = List.rev (sort_targets observation.position (tree_positions (observation.trees))) in
    let my_id = List.hd memory.targets in
    let i = find_index my_id id_list in
    if (List.length targets <= List.length id_list) then (* Si il y a plus de robots que de targets *)
      Wait,
      {
        known_world = memory.known_world;
        graph = memory.graph;
        objective = GoingTo( [observation.position] , [List.nth targets (i mod (List.length targets))] ) ;
        targets = (my_id)::[List.nth targets (i mod (List.length targets))] ;
      }
    else (* Si il y a moins de robots que de targets *)
    let my_targets = List.nth ( separate (targets) (List.length id_list) ) i in
    Wait,
    {
      known_world = memory.known_world;
      graph = memory.graph;
      objective = GoingTo( [observation.position] , [List.hd my_targets] ) ;
      targets = (my_id)::(my_targets) ;
    }

  (* Fin initialisation ***************************************************************************)

  (* Cas n°3 mais il n'y a plus d'autre target *)
  | Chopping, Some tr, id::t1::[],_ when tr.branches <= 0 ->
    Wait,
    let sp = safe_shortest_path observation.position observation.spaceship observation memory in
    (* Printf.eprintf "sh_path : %s\n" (string_of_path (sp)) ; *)
    {
      known_world = memory.known_world ;
      graph =  memory.graph;
      (* objective = GoingTo( [observation.position] , [observation.spaceship] )  ; *)
      objective = GoingTo( [List.hd sp] , List.tl sp  ) ;
      targets = (List.hd memory.targets)::[observation.spaceship] ;
    }

  (* Cas n°3 mais il reste d'autres targets *)
  | Chopping, Some tr, id::t1::q1,_ when tr.branches <= 0 -> (* plus de branche mais d'autre(s) arbre(s) *)
    let current_target = t1 in
    let sp = safe_shortest_path observation.position current_target observation memory in
    (* Printf.eprintf "sh_path : %s\n" (string_of_path (sp)) ; *)
    Wait,
    {
      known_world = memory.known_world;
      graph =  memory.graph;
      objective = (* memory.objective *)
        GoingTo( [List.hd sp] , List.tl sp  ) ;
      targets = id::(sort_targets observation.position (q1)) ;
    }

  (* Cas n°2 *)
  | Chopping, _ , _,_ ->
    ChopTree, memory (* encore des branches *)

  (* Cas n°4 mais on approche d'un arbre à branches mais on est pas encore assez près *)
  | GoingTo(l0,(x1,y1)::l1),None ,_,Some tr when tr.branches > 0 -> (* On n'est pas à proximité d'un arbre dont il reste des br *)
    let s0 = last l0 in
    let a =
      if x_ s0 < x1 && y_ s0 < y1 then atan ( (y1-.(y_ s0))/.(x1-.(x_ s0)) )
      else if x_ s0 < x1 && y_ s0 > y1 then -. atan ( -.(y1-.(y_ s0))/.(x1-.(x_ s0)) )
      else if x_ s0 > x1 && y_ s0 > y1 then atan ( (y1-.(y_ s0))/.(x1-.(x_ s0)) ) -. Float.pi
      else -. atan ( (y1-.(y_ s0))/.(x_ s0-.(x1)) ) +. Float.pi in
    Move ( Space.angle_of_float a , Space.speed_of_float ( ( dist (observation.position) (tr.tree_position) ) /. (suffering (world_of_observation observation) observation.position) ) ) ,
    {
      known_world = memory.known_world;
      graph =  memory.graph;
      objective = memory.objective ;
      targets = (List.hd memory.targets)::(sort_targets observation.position (List.tl memory.targets)) ;
    }

  (* Cas n°4 mais on arrive près d'un arbre à branches *)
  | GoingTo(_,_),Some tr ,_,_  when tr.branches > 0 -> (* On est a proximité d'un arbre à branches *)
    Move ( Space.angle_of_float 0. , Space.speed_of_float 0. ),
    {
      known_world = memory.known_world;
      graph = memory.graph;
      objective = Chopping ;
      targets = memory.targets ;
    }

  (* Cas n°4 mais on arrive à proximité du vaisseau *)
  | GoingTo(l0,(x1,y1)::[]),_,_,_ when
      Space.close observation.position observation.spaceship 1.
      && Space.close observation.spaceship (x1,y1) 1. -> (* On est a proximité du spaceship *)
    Move ( Space.angle_of_float 0. , Space.speed_of_float 0. ),
    {
      known_world = memory.known_world;
      graph = memory.graph;
      objective = Initializing ;
      targets = memory.targets ;
    }


  (* Cas n°4 sans rien de particulier *)
  | GoingTo(l0,(x1,y1)::l1),_ ,_,_-> (* On n'est pas à proximité d'un arbre dont il reste des br *)
    let s0 = last l0 in
    let a =
      if x_ s0 < x1 && y_ s0 < y1 then atan ( (y1-.(y_ s0))/.(x1-.(x_ s0)) )
      else if x_ s0 < x1 && y_ s0 > y1 then -. atan ( -.(y1-.(y_ s0))/.(x1-.(x_ s0)) )
      else if x_ s0 > x1 && y_ s0 > y1 then atan ( (y1-.(y_ s0))/.(x1-.(x_ s0)) ) -. Float.pi
      else -. atan ( (y1-.(y_ s0))/.(x_ s0-.(x1)) ) +. Float.pi in
    Move ( Space.angle_of_float a , Space.speed_of_float 2.) ,
    {
      known_world = memory.known_world;
      graph = memory.graph;
      objective = GoingTo(l0,(x1,y1)::l1);
      targets = (List.hd memory.targets)::(sort_targets observation.position (List.tl memory.targets)) ;
    }

  (* Cas restants : problème *)
  | _,_,_,_ ->
    Wait, memory



(**

   Comme promis, la fonction de décision est la composition
   des trois fonctions du dessus.

*)
let decide visualize observation memory : action * memory =

  let memory = discover visualize observation memory in
  let memory = plan visualize observation memory in

  Printf.eprintf "Position : %s\n%!" (string_of_path ([observation.position])) ;
  Printf.eprintf "Objective : %s\n%!" (string_of_objective (memory.objective)) ;
  Printf.eprintf "Targets : %s\n%!" (string_of_path (memory.targets)) ;


  (** Visualizer.show_graph memory.graph;
  Visualizer.show_path memory.targets;  **)

  next_action visualize observation memory
