open Graphics;;

let x = ref 0
let y = ref 0
let xVel = ref 1
let yVel = ref 1
let size = 20

let initWindow () = 
    open_graph "";
    set_window_title "Rectangles";
    set_color blue;
    fill_rect !x !y size size
    

let clear_window color = 
    let fg = foreground 
    in
        set_color color;
        fill_rect 0 0 (size_x ()) (size_y ());
        set_color fg

let controlBounds () =
    if !x > size_x () then
        (
        x := size_x (); 
        xVel := !xVel * (-1);
        )
    else ();

    if !x < 0 then
        (
        x := 0;
        xVel := !xVel * (-1);
        )
    else ();

    if !y > size_y () then
        (
        y := size_y (); 
        yVel := !yVel * (-1);
        )
    else ();

    if !y < 0 then
    (
        y := 0;
        yVel := !yVel * (-1);
    )
    else ();

    ()


let update speed = 
    x := !x + speed * !xVel;
    y := !y + speed * !yVel;
    
    controlBounds ()
    (*let coordsString = "x: " ^ (string_of_int !x) ^ ", y: " ^ (string_of_int !y) in
    print_endline coordsString;
    let velsString = "xVel: " ^ (string_of_int !xVel) ^ ", yVel: " ^ (string_of_int !yVel) in
    print_endline velsString*)

let draw () = 
    clear_window black;
    set_color blue;
    fill_rect !x !y size size

let rec delay depth = 
    if depth = 0 then
        ()
    else
        delay (depth - 1)

let tick frameSpeed = 
    update 1;
    draw ();
    delay (1000000000 / frameSpeed)
    

let start () = 
    initWindow ();
    let rec loop () = 
        tick 1000;
        loop () 
    in 
    try loop ()
    with Graphic_failure _ -> print_endline "Exiting..."