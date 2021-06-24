(**

   Ce module fournit un générateur aléatoire de mondes. Il est peu
   sophistiqué, c'est à vous de l'améliorer pour briller dans la tâche
   5 du projet.

*)

open World

let default_end_of_time =
  Ext.Int.random_in_range 2500 50000

let default_visibility =
  Ext.Float.random_in_range 80. 100.

let initial = {
    space       = Space.empty;
    trees       = [];
    teams       = [];
    epoch       = 0;
    microcodes  = [];
    end_of_time = default_end_of_time;
    visibility  = default_visibility;
}

let inside_hell position space =
  Space.inside position (( = ) Hell) space = Some Hell

let simple_world nb_players nb_robots_per_team max_hell max_ground max_tree =
  let width  = Ext.Float.random_in_range 500. 1000. in (*500 1000*)
  let height = Ext.Float.random_in_range 500. 1000. in (*500 1000*)
  let random_angle () = Space.angle_of_float (Random.float (2. *. Float.pi)) in
  let random_position space =
    let rec aux n =
      if n = 0 then failwith "Impossible to find a free random position.";
      let p = Ext.Float.(
        (random_in_range 0. width, random_in_range 0. height)
      ) in
      (* if inside_hell p space then aux (n - 1) else p *)
      if Space.inside p (fun _ -> true) space <> None then aux (n - 1) else p
    in
    aux 10
  in
  let random_size () =
    Ext.Float.random_in_range 100. (width /. 10.)
  in
  let rec make_hell space =
    let s = Space.square (random_position space) (random_size ()) Hell in
    if Space.polygon_overlaps s space (( = ) Hell) then make_hell space else
    Space.(blend space (polygon s))
  in
  let rec make_uhell n space =
    if n<=0 then make_hell space else
    let s =  (random_size ())in
    let a = random_position space in
    let x1 = fst a in
    let x2 = x1+.1.*.s in
    let x3 = x1+.3.4*.s in
    let x4 = x1+.3.5*.s in
    let x5 = x1+.6.*.s in
    let x6 = x1+.7.*.s in
    let y1 = snd a in
    let y2 = y1+.1.*.s in
    let y3 = y1+.6.*.s in
    let y4 = y1+.7.*.s in
    let poly = Space.make [a;(x1,y4);(x3,y4);(x3,y3);(x2,y3);(x2,y2);(x5,y2);(x5,y3);(x4,y3);(x4,y4);(x6,y4);(x6,y1)] Hell in
    if Space.polygon_overlaps poly space (( = ) Hell) then make_uhell (n-1) space else
      Space.(blend space (polygon poly))
  in

  let rec make_thell n space=
    if n<=0 then (make_hell space) else
    let s =  (random_size ())in
    let a = random_position space in
    let b = (fst a+.s,snd a +. s/.2.) in
    let c = (fst a+.s/.2.,snd a +. s) in
    let poly = Space.make [a;b;c] Hell in
    if Space.polygon_overlaps poly space (( = ) Hell) then make_thell (n-1) space else
      Space.(blend space (polygon poly))
  in

  let rec make_random_thell n space =
    if n<=0 then make_thell 10 space else
    let n = Ext.Int.random_in_range 3 3 in
    let s =  (random_size ())in
    let a = random_position space in
    let rec aux l n =
      match n with
      | x when x=0 -> l
      | _ -> aux (Ext.Float.( (fst a+.(random_in_range (-.s) s), snd a+.(random_in_range (-.s) s)) )::l) (n-1)
    in
    let l = aux [] n in
    let poly = Space.make l Hell in
    if Space.polygon_overlaps poly space (( = ) Hell) then make_random_thell (n-1) space else
      Space.(blend space (polygon poly))
  in

  let make_ground space =
    let ratio = Ground (Ext.Float.random_in_range 0.5 1.5) in
    let s = Space.square (random_position space) (random_size ()) ratio in
    Space.(blend space (polygon s))
  in
  let make_tree space _ =
    let tree_position = random_position space in
    let branches = Ext.Int.random_in_range 2 100 in
    { tree_position; branches }
  in
  let make_team space team_identifier =
    let spaceship = random_position space in
    let make_robot id =
      make_robot id team_identifier spaceship (random_angle ())
    in
    { team_identifier; spaceship;
      robots = Ext.Fun.repeat nb_robots_per_team make_robot }
  in

  let nb_uhell = Ext.Int.random_in_range 0 (min (2) (max_hell)) in
  let space = Ext.Fun.iter nb_uhell (make_uhell 10) Space.empty in
  let nb_hell = Ext.Int.random_in_range 1 max_hell/2 in
  let space = Ext.Fun.iter nb_hell make_hell space in
  let space = Ext.Fun.iter nb_hell (make_random_thell (10)) space in
  let nb_grounds = Ext.Int.random_in_range 1 max_ground in
  let space = Ext.Fun.iter nb_grounds make_ground space in
  let nb_trees = Ext.Int.random_in_range 1 max_tree in
  let trees = Ext.Fun.repeat nb_trees (make_tree space) in
  let teams = Ext.Fun.repeat nb_players (make_team space) in
  { initial with space; trees; teams }

let output world =
  to_yojson world |> Yojson.Safe.pretty_to_string |> output_string stdout

let generate
      visualize nb_players nb_robots_per_teams
      max_hell max_ground max_tree =
  let world =
    simple_world nb_players nb_robots_per_teams max_hell max_ground max_tree
  in
  if visualize then (Visualizer.(show world; pause ()));
  output world

let visualization_flag = Cmdliner.(Arg.(
  value & flag & info ["v"]
  ~doc:"Visualize the generated world")
  )

let nb_players = Cmdliner.(Arg.(
  value & opt int 1 & info ["p"]
  ~docv:"NBPLAYERS"
  ~doc:"Handle $(docv) players."
))

let nb_robots = Cmdliner.(Arg.(
  value & opt int 1 & info ["r"]
  ~docv:"NBROBOTS"
  ~doc:"Handle $(docv) robots per player."
))

let max_hell = Cmdliner.(Arg.(
  value & opt int 1 & info ["h"]
  ~docv:"MAXHELL"
  ~doc:"Use a maximum of $(docv) hell blocks."
))

let max_ground = Cmdliner.(Arg.(
  value & opt int 1 & info ["g"]
  ~docv:"MAXGROUND"
  ~doc:"Use a maximum of $(docv) ground blocks."
))

let max_tree = Cmdliner.(Arg.(
  value & opt int 1 & info ["t"]
  ~docv:"MAXTREE"
  ~doc:"Use a maximum of $(docv) trees."
))

let cmd = Cmdliner.(
  let doc   = "Generate a random world." in
  let exits = Term.default_exits in
  Term.(const generate $ visualization_flag
        $ nb_players $ nb_robots
        $ max_hell $ max_ground $ max_tree),
  Term.info "generate" ~doc ~exits
)
