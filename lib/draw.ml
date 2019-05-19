open Graphics;;

let size = ref 0

let initWindow width height rectSize = 
    let windowString = (string_of_int width) ^ "x" ^ (string_of_int height) in
    open_graph windowString;
    auto_synchronize false;
    set_window_title "Rectangles";
    size := rectSize
    

let clear_window color = 
    let fg = foreground 
    in
        set_color color;
        fill_rect 0 0 (size_x ()) (size_y ());
        set_color fg

let drawRectangle (x, y, _, _) = 
    (*let totVel = int_of_float ((abs_float !xVel) +. (abs_float !yVel) *. 50.0) in
    set_color (rgb 255 totVel totVel);*)
    fill_rect (int_of_float !x) (int_of_float !y) !size !size (* Should round instead of trunc. *)

let draw rectangles = 
    clear_window black;
    set_color red;
    List.iter drawRectangle rectangles;
    synchronize ()

