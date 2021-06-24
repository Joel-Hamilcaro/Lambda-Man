(**

   Ce module implémente une fenêtre de visualisation artisanale.

   On peut se déplacer à l'aide des touches t, g, h, et f.

   On peut zoomer à l'aide des touches r et e.

   On peut quitter à l'aide de la touche q.

*)

open Lwt
open World
open Space
open Graphics

let res_x = ref 600

let res_y = ref 600

let moved = ref false

let view_x = ref 0.

let view_y = ref 0.

let zoom = ref 1.

let state : World.t option ref = ref None

let clip d x = if x < 0 then 0 else if x > d then d else x

let clip_x = clip !res_x

let clip_y = clip !res_y

let t_x x = (int_of_float ((x -. !view_x) *. !zoom))

let t_y y = (int_of_float ((y -. !view_y) *. !zoom))

let t_ p = (t_x (x_ p), t_y (y_ p))

let scale size = max 1 (int_of_float (size *. !zoom))



let color_of_thing = function
  | World.Hell ->
      black
  | World.Ground f ->
    let c = int_of_float (float_of_int (255/5) *. f) in
    match f with
    | fi when fi=1. -> rgb 226 188 116
    | fi when fi<1. -> rgb 255 (c) (c)
    | fi -> rgb (c) 255 (c)

let hell_colors = [ (rgb 255 37 0);(rgb 255 102 0);(rgb 242 242 23);(rgb 255 37 0);(rgb 234 92 15) ]
let suff_colors = [ (white);(yellow) ]

let box p =
  let l = Space.vertices p in
  bounding_box_of_positions l

let random_pos_in_box box =
  let random_in_bounds a b = a+.(Random.float (b-.a)) in
  let x = random_in_bounds (x_ (fst box)) (x_ (snd box)) in
  let y = random_in_bounds (y_ (fst box)) (y_ (snd box)) in
  (x,y)

let draw_circles_in_box p n colors_list s =
  let rec aux acc =
    let pos = random_pos_in_box (box p) in
    if acc<n then begin
      set_color (List.nth colors_list (Random.int (List.length colors_list)));
      fill_circle (t_x (x_ pos)) (t_y (y_ pos)) (scale s);
      aux (acc+1)
    end
    else ()
  in
  aux 0

let is_hell p =
  match Space.content p with
  | World.Hell -> true
  | World.Ground f -> false

let display_polygon p =
  set_color (color_of_thing (Space.content p));
  fill_poly (List.map t_ (Space.vertices p) |> Array.of_list);
  if is_hell p then draw_circles_in_box p 10 hell_colors 3.
  else draw_circles_in_box p 10 suff_colors 1.


let ground_first t1 t2 =
  match content t1, content t2 with
  | Hell, Hell | Ground _, Ground _ -> 0
  | Hell, _ -> 1
  | Ground _, _ -> -1

let display_space space =
     Space.polygons space (fun _ -> true)
  |> List.sort ground_first
  |> List.iter display_polygon


let screen_bounding_box positions =
  List.fold_left (fun ((x0, y0), (x1, y1)) (x, y) ->
      ((min x x0, min y y0), (max x x1, max y y1))
    ) ((max_int, max_int), (min_int, min_int)) positions

let backgrounds = ref []
let save_background ((x0, y0), (x1, y1)) =
  let h = y1 - y0 + 1 and w = x1 - x0 + 1 in
  let i = get_image x0 y0 w h in
  backgrounds := (x0, y0, i) :: !backgrounds

let restore_background () =
  List.iter (fun (x, y, i) -> draw_image i x y) !backgrounds;
  backgrounds := []

let display_tree { tree_position; branches } =
  let w = scale 0.5 in
  let l = scale 1. in
  let h = scale 10. in
  let r = (scale 5. ) in
  save_background (screen_bounding_box [
        (t_x (x_ tree_position) - r, t_y (y_ tree_position) +h-r);
        (t_x (x_ tree_position) + r, t_y (y_ tree_position) +h+r);
      ]);
  save_background (screen_bounding_box [
            (t_x (x_ tree_position) - w, t_y (y_ tree_position) );
            (t_x (x_ tree_position) -w+l, t_y (y_ tree_position) +h);
      ]);
  if ( branches > 0 ) then begin
    set_color (rgb (140) (101) (0) );
    fill_rect (t_x (x_ tree_position)-w ) (t_y (y_ tree_position)) (l) (h);
    set_color (rgb (0) (170) (0) );
    fill_circle (t_x (x_ tree_position)) (t_y (y_ tree_position)+(h)) (r);
  end

let display_robot team_color robot =
  if not robot.robot_down then begin
    let pos = robot.robot_position and a = float_of_angle robot.robot_angle in
    let al = a +. Float.pi /. 2. and ar = a -. Float.pi /. 2. in
    let h = 15. and w = 4.5 in
    let w1 = scale w in
    let head = (x_ pos +. h *. cos a, y_ pos +. h *. sin a) in
    let left = (x_ pos +. w *. cos al, y_ pos +. w *. sin al) in
    let right = (x_ pos +. w *. cos ar, y_ pos +. w *. sin ar) in
    let head2 = (x_ pos +. h *. cos a +.2., y_ pos +. h *. sin a +.2.) in
    let left2 = (x_ pos +. w *. cos al +.2., y_ pos +. w *. sin al +.2.) in
    let right2 = (x_ pos +. w *. cos ar +.2., y_ pos +. w *. sin ar +.2.) in
    save_background (screen_bounding_box [
              (t_x (x_ pos) - w1, t_y (y_ pos) - w1);
              (t_x (x_ pos) + w1 , t_y (y_ pos) + w1);
        ]);
    set_color black;
    draw_circle  (t_x (x_ pos)) (t_y (y_ pos)) (w1);
    draw_circle  (t_x (x_ pos)) (t_y (y_ pos)) (scale (w/.2.)) ;
    draw_circle  (t_x (x_ pos)) (t_y (y_ pos)) (scale (w)) ;
    save_background (screen_bounding_box [ t_ head; t_ left; t_ right ]);
    set_color team_color;
    fill_poly [| t_ head; t_ left; t_ right |]


  end

let display_microcode m =
  let pos = m.microcode_position in
  let l = Float.log (float_of_int (Space.int_of_duration m.duration)) in
  let d = max 1. (min 10. l) in
  let w = scale (d*.1.5) in
  let w0 = scale (d) in
  save_background (screen_bounding_box [
                       (t_x (x_ pos) - w, t_y (y_ pos) - w);
                       (t_x (x_ pos) + w, t_y (y_ pos) + w);
    ]);
  set_color (rgb 255 120 120);
  fill_circle (t_x (x_ pos)) (t_y (y_ pos)) (w);
  set_color (rgb 255 0 0);
  save_background (screen_bounding_box [
                       (t_x (x_ pos) - w0, t_y (y_ pos) - w0);
                       (t_x (x_ pos) + w0, t_y (y_ pos) + w0);
    ]);
  fill_circle (t_x (x_ pos)) (t_y (y_ pos)) (w0)


let display_spaceship team_color pos =
  let d = 15. and d' = 30. in
  set_color team_color;
  draw_circle (t_x (x_ pos)) (t_y (y_ pos)) (scale d);
  draw_rect (t_x (x_ pos -. d)) (t_y (y_ pos -. d)) (scale d') (scale d')

let random_color =
  let m = 100 in
  let c = Array.init m Random.(fun _ -> rgb (int 240) (int 240) (int 240)) in
  fun i -> c.(i mod m)

let display_team team =
  let team_color = random_color team.team_identifier in
  display_spaceship team_color team.spaceship;
  List.iter (display_robot team_color) team.robots

let display world =
  clear_graph ();
  set_color (rgb 231 207 162) ;
  fill_rect 0 0 (scale 2100.) (scale 2100.);
  display_space world.space;
  List.iter display_tree world.trees;
  List.iter display_team world.teams

let focus_on_box ((x0, y0), (x1, y1)) =
  let swidth = x1 -. x0 and sheight = y1 -. y0 in
  let r1 = (float_of_int !res_x) /. swidth
  and r2 = (float_of_int !res_y) /. sheight in
  view_x := x0;
  view_y := y0;
  zoom := min 3. (min r1 r2)

let control_loop () =
  Lwt_preemptive.detach (fun () ->
  let rec aux () =
  let status = wait_next_event [Key_pressed; Poll] in
  if not status.keypressed then Lwt_unix.sleep 0.01 >>= aux else (
    let status = wait_next_event [Key_pressed] in
      let d = 5. *. !zoom and dzoom = 0.01 in
      begin match status.key with
      | '*' -> exit 1 (*q*)
      | 'p' -> zoom := min 10. (!zoom +. dzoom) (*e*)
      | 'm' -> zoom := max 0.1 (!zoom -. dzoom) (*r*)
      | 's' -> view_y := !view_y -. d (*t*)
      | 'z' -> view_y := !view_y +. d (*g*)
      | 'q' -> view_x := !view_x -. d (*h*)
      | 'd' -> view_x := !view_x +. d (*f*)
      | _ -> ()
      end;
      moved := true; Lwt_unix.sleep 0.01 >>= aux)
  in Lwt.async aux) ()

let bounding_box_world world =
  bounding_box_union
    (bounding_box world.space)
    (bounding_box_of_positions (
         List.map (fun t -> t.tree_position) world.trees
       @ List.map (fun (t : team) -> t.spaceship) world.teams))

let initialize new_world =
  let bbox = bounding_box_world new_world in
  open_graph (Printf.sprintf " %dx%d" !res_x !res_y);
  state := Some new_world;
  focus_on_box bbox;
  display new_world;
  Lwt.async control_loop

let pause () = ignore (wait_next_event [Key_pressed])

let show_info world =
  set_color black;
  fill_rect 9 9 (!res_y-18) 12;
  moveto 10 10;
  set_color white;
  draw_string (Printf.sprintf "#trees : %d  " (World.number_of_branches world));
  draw_string (Printf.sprintf "#epoch : %d  " (world.epoch));
  draw_string (Printf.sprintf "#visibility : %f  " (world.visibility));
  draw_string (Printf.sprintf "#end_of_time : %d  " (world.end_of_time))

let update force _old_world new_world =
  if not force then auto_synchronize false;
  if !moved || force then (
    display new_world;
    moved := false
  ) else (
    restore_background ();
    List.iter display_team new_world.teams;
    List.iter display_microcode new_world.microcodes;
    List.iter display_tree new_world.trees;
    state := Some new_world
  );
  show_info new_world;
  if not force then synchronize ()

let show ?(force = false) new_world =
  match !state with
  | None -> initialize new_world
  | Some old_world -> update force old_world new_world

let show_node (x, y) =
  set_color blue;
  fill_circle (t_x x) (t_y y) 5

let show_edge (x0, y0) (x1, y1) =
  set_color blue;
  set_line_width 2;
  moveto (t_x x0) (t_y y0);
  lineto (t_x x1) (t_y y1)

let show_graph g =
  auto_synchronize false;
  let ns = Graph.nodes g in List.(
  iter show_node ns;
  iter (fun n -> iter (fun (_, n', _) -> show_edge n n') (Graph.out g n)) ns;
  synchronize ();
)

let show_milestone (x, y) =
  set_color yellow;
  fill_circle (t_x x) (t_y y) 5

let show_path path =
  auto_synchronize false;
  List.iter show_milestone path;
  synchronize ()
