let size = ref 0.0
let delta = ref 0.0
let speed = 100.0
let height = ref 0.0
let width = ref 0.0

let getRandomVel () = 
    let vel = Random.int 2 in
    if vel = 0 then
        -1.0
    else
        float_of_int vel

let setNewVel(vel : float ref) = 
    vel := !vel *. (-1.0) +. ((Random.float 0.6) -. 0.3)

let initRectangle windowWidth windowHeight = 
    let xVel = ref (getRandomVel ()) and yVel = ref (getRandomVel ()) in
    setNewVel xVel;
    setNewVel yVel;
    (ref (float_of_int(Random.int (windowWidth-40))), ref (float_of_int(Random.int (windowHeight-40))), xVel, yVel)

let rec initRectanglesRec numRectangles rectangles windowWidth windowHeight = 
    if numRectangles > 0 then
        let rectangles = (initRectangle windowWidth windowHeight) :: rectangles in
        initRectanglesRec (numRectangles - 1) rectangles windowWidth windowHeight
    else 
        rectangles

let initRectangles numRectangles windowWidth windowHeight rectSize = 
    size := float_of_int rectSize;
    initRectanglesRec numRectangles [] windowWidth windowHeight

let collision (x1 : float) (x2 : float) =
    x1 +. !size >= x2 && x1 <= x2 +. !size

let checkCollision (x1, y1, xVel1, yVel1) (x2, y2, xVel2, yVel2) =
    if collision !x1 !x2 && collision !y1 !y2 then
        if x1 != x2 then begin
            setNewVel xVel1;
            setNewVel yVel1;
            setNewVel xVel2;
            setNewVel yVel2
        end
    else
        ()

let controlRectCollision rect rectangles = 
    List.iter (fun r -> checkCollision rect r) rectangles
    (*let _, _, xVel, yVel = rect in
    xVel := !xVel * -1;
    yVel := !yVel * -1*)

let controlBounds x y xVel yVel width height =
    if !x +. !size > width then begin
        x := width -. !size; 
        setNewVel xVel
        end
    else ();

    if !x < 0.0 then begin
        x := 0.0;
        setNewVel xVel
        end
    else ();

    if !y +. !size > height then begin
        y := height -. !size; 
        setNewVel yVel
    end
    else ();

    if !y < 0.0 then begin
        y := 0.0;
        setNewVel yVel
    end
    else ();

    ()

let updateRectangle ((x : float ref), (y : float ref), (xVel : float ref), (yVel : float ref)) rectangles =
    x := !x +. speed *. !xVel *. !delta;
    y := !y +. speed *. !yVel *. !delta;
    
    controlBounds x y xVel yVel !width !height;
    controlRectCollision (x, y, xVel, yVel) rectangles
    
    (*
    let coordsString = "x: " ^ (string_of_int !x) ^ ", y: " ^ (string_of_int !y) in
    print_endline coordsString;
    let velsString = "xVel: " ^ (string_of_int !xVel) ^ ", yVel: " ^ (string_of_int !yVel) in
    print_endline velsString;

    (x, y, xVel, yVel)
    *)

let update rectangles (newDelta : float) = 
    width := float_of_int (Graphics.size_x ()); 
    height := float_of_int (Graphics.size_y ());
    delta := newDelta;
    List.iter (fun r -> updateRectangle r rectangles) rectangles
    