open Raylib

let window_width = 800
let window_height = 450

type species = 
  | Unknown
  | Human
  | Mekkan

let setup () =
  Raylib.init_window window_width window_height "raylib [core] example - basic window";
  Raylib.set_target_fps 60;
  0

let draw_character (species: species) x y = 
  match species with 
  | Unknown -> draw_rectangle x y 10 10 Color.gray
  | Human -> draw_rectangle x y 10 10 Color.darkblue
  | Mekkan -> draw_circle x y 50.0 Color.orange

let create_character (s: species) : unit = 
  match s with 
  | Unknown -> ()
  | Human -> ()
  | Mekkan -> ()

module Component = struct
  type id = int
  type name = 
    | Unknown
    | Name of string
  type attribute = 
    | Unknown
    | Vexing of int
    | Invigorating of int
    | Malice of int
    | Psychosis of int
    | Will of int
end


module Entity = struct
  type id = int
end

type world = int

type _entity_id = int

let _roll (dN) = 1 + Random.int (dN)

let draw_background () = 
  begin
    clear_background Color.gold;
    draw_rectangle 10 10 15 15 Color.darkgreen;
    draw_character Human 40 40;
    draw_character Unknown 100 300;
    draw_character Mekkan 300 100;
    create_character Mekkan;
  end

let draw () = 
  let open Raylib in 
  begin_drawing ();
  draw_background ();
  draw_text "Ork Hunter" 100 100 25 Color.black;
end_drawing()

let run_updates (world: world) = world

let rec loop (world: world) =
  match Raylib.window_should_close () with
  | true -> Raylib.close_window ()
  | false ->
      draw();
      let new_world = run_updates(world) in 
        loop (new_world)

let () = setup () |> loop