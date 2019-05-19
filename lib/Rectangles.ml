let width = 1600 
let height = 900
let rectSize = 2

let timeLastTick = ref 0.0
let frameTime = 1.0 /. 60.0
let delta = ref 0.0

let frameCounter = ref 0
let timeLastSecond = ref 0.0

let timeLogic = ref 0.0
let timeRender = ref 0.0

let setDeltaTime () = 
    delta := Sys.time () -. !timeLastTick;
    timeLastTick := Sys.time ();
    ()

let delay () =
    let elapsedTime = Sys.time () -. !timeLastTick in
    if elapsedTime < frameTime then
        Unix.sleepf (frameTime -. elapsedTime)
    else ()

let printFPS () =
    incr frameCounter;
    if (!timeLastSecond +. 1.0) < Sys.time () then begin
        print_endline ("FPS: " ^ (string_of_int !frameCounter));
        print_endline ("TimeLogic: " ^ (string_of_float !timeLogic));
        print_endline ("TimeRender: " ^ (string_of_float !timeRender));
        frameCounter := 0;
        timeLastSecond := Sys.time ()
    end
    else ()

let tick rectangles = 
    printFPS();
    setDeltaTime ();
    (*let rectangles = Logic.update rectangles !delta in*)
    let timeStart = Sys.time () in
    Logic.update rectangles !delta;
    timeLogic := !timeLogic +. (Sys.time ()) -. timeStart;
    let timeStart = Sys.time () in
    Draw.draw rectangles;
    timeRender := !timeRender +. (Sys.time ()) -. timeStart;
    delay ()
    
let start numRectangles = 
    Random.self_init ();
    Draw.initWindow width height rectSize;
    let rectangles = Logic.initRectangles numRectangles width height rectSize in
    let rec loop rectangles = 
        tick rectangles;
        loop rectangles
    in 
    try loop rectangles
    with Failure _ -> print_endline "Exiting..."