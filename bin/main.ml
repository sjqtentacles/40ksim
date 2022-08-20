open Raylib

let window_width = 800
let window_height = 450

type _species = 
  | Unknown
  | Human
  | Mekkan

let setup () =
  Raylib.init_window window_width window_height "raylib [core] example - basic window";
  Raylib.set_target_fps 60

let draw_background () = 
  begin
    clear_background Color.gold;
    draw_rectangle 10 10 15 15 Color.darkgreen;
  end

let draw () = 
  let open Raylib in 
  begin_drawing ();
  draw_background ();
  draw_text "Ork Hunter" 100 100 25 Color.black;
end_drawing()

let rec loop () =
  match Raylib.window_should_close () with
  | true -> Raylib.close_window ()
  | false ->
      draw();
      loop ()

let () = setup () |> loop