open Raylib

module Component = struct

  type turns = int
  type damage = int

  type _spell = 
    | Impact of damage
    | Stun of turns
    | Bleed of damage * turns
    | Heal of damage

  type _weapon = 
    | Knives of damage
    | Power_sword of (_t -> _t)
    | Staff of damage

  and _comp_2_comp = _t -> _t
  and _comp_2_unit = _t -> unit
  and _unit_2_comp = unit -> _t
  and _unit_2_unit = unit -> unit

  and _system =
    | Comp2Comp of _comp_2_comp
    | Comp2Unit of _comp_2_unit
    | Unit2Comp of _unit_2_comp
    | Unit2Unit of _unit_2_unit

  and _components = _t list

  and _t = 
    | Health of int
    | Sanity of int
    | Will of int
    | Spell of _spell
    | Draw of _system
    | Armour of int
    | Weapon of _weapon
    | Movement of int
    | Targets of _components
    | Visible of bool
    | Inventory of _components

end

let window_width = 800
let window_height = 450

type world = int

let empty_world = 0

let setup () =
  Raylib.init_window window_width window_height "basic window";
  Raylib.set_target_fps 60;
  empty_world

let _roll (dN) = 1 + Random.int (dN)

let draw_background () = 
  begin
    clear_background Color.gold;
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