open CPutil_sn;;


(** Valeur constante du coefficient de transaltion en x*)
let mytranslation_x() : int = 100;;

(** Valeur constante du coefficient de transaltion en y*)
let mytranslation_y() : int = 100;;

(** Valeur constante du coefficient d'homothétie en x*)
let mydilation_x() : int = 5;;

(** Valeur constante du coefficient d'homothétie en y*)
let mydilation_y() : int = 5;;

(**Cette fonction calcule les coordonnées en x du pixel de la fenêtre graphique correspondantes aux coordonnées d’un point de la matrice*) 
let mygraphic_x(x : int) : int =
  mytranslation_x() + x*mydilation_x();;

(**Cette fonction calcule les coordonnées en y du pixel de la fenêtre graphique correspondantes aux coordonnées d’un point de la matrice*)
 let mygraphic_y(y : int) : int =
  mytranslation_y() + y*mydilation_y();;

(** Cette fonction trace un carré du pixel correspondant à un point dans la matrice , en appelant la fonction fill_rect en fonction des coordonées x et y auquelles on applique les fonctions mygraphic*)
 let myplot(x, y : int * int) : unit =
  fill_rect(mygraphic_x(x), mygraphic_y(y),mydilation_x(), mydilation_y());;

(** Pour cette fonction on utilise la fonctione fill_rect pour tracer 4 rectangles qui seront les 4 côtés du rectangle demandé. On fait ceci à partir de px et py qui sont les coordonées du point en bas à gauhe du rectangle et des longueurs des côtés dx et dy.*)
 let myfill_rect(px, py, dx, dy : int * int * int * int) : unit =
fill_rect(mygraphic_x(px) , mygraphic_y(py) , dx/mydilation_x(), dy/mydilation_y());;

(**On fait un type énuméré avec les 4 directions possibles que peut prendre le serpent *)
type t_direction = UP | DOWN | RIGHT | LEFT;;

(** type avec les coordonées x et y d'un point*) 
type t_point = {x : int ; y : int} ;;

(**Représente les deux composantes du serpent, ses coordonées et sa directions*)
type t_position = {pt : t_point ; dir : t_direction} ;;

(**Type énuméré avec les différentes valeur que peut contenir une case de la matrice*)
type t_value = EMPTY | SNAKE | FRAME | PROBLEM ;;

(**Fonction qui permet d’associer une couleur à chaque valeur de type t_value*) 
let color_of_value(x : t_value) : t_color =
  if x = EMPTY
  then white
  else
    if x = SNAKE
    then black
    else
      if x = FRAME
      then green
      else red;;

(** Matrice composée de t_value*) 
type t_matrix = t_value matrix;;

(** Correspond à la  liste de positions qui composent le serpent *)
type t_snake = t_position list ;;

(** Type avec la vitesse de déplacement du serpent qi peut évoluer, la liste des coordonées du serpent qui évolue aussi et la matrice.*)
 
type t_play = {dt : float ref ; sn : t_snake ref ; mat : t_matrix} ;;

(**Valeur constante de la taille en x de la matrice du jeu*)
let mymatrix_dx() : int = 100;;

(**Valeur constante de la taille en y de la matrice du jeu*)
let mymatrix_dy() : int = 100;;

(**Valeur constante représentant l'intervalle de temps dt entre deux déplacement*) 
let mydt_init() : float = 0.75;;

(**Valeur constante représentant l'intervalle de temps entre deux modifictaions de dt*)
let mydt_acc() : float = 5.;;

(**Indique le ratio entre la nouvelle et l'ancienne valeur dt *)
let mydt_ratio() : float = 1.05;;

(** Indique le nombre d’éléments composant le serpent au début de la patie.*)
let mysnake_length_init() : int = 3;;

(** Position de la tête du serpent dans la matrice au début dela partie.*)
let mysnake_position_init() : t_point = {x=mymatrix_dx()/2;y=mymatrix_dx()/2};;

(**Cette fonction sert à tracer un cadre autour de la zone d’affichage de la matrice, en utilisant la fonction myfill_rect. On ouvre la fenêtre graphique et ensuite on tracele cadrequi fait du 500x500 pixels et qui représente une matrice de 100x100 valeurs*)
let draw_frame() : unit =  
  set_color(color_of_value(FRAME));
  myfill_rect(-1 , -1 , 102 , 1);
  myfill_rect(-1 , -1 , 1 , 102);
  myfill_rect(-1 , 101 , 102 , 1);  
  myfill_rect(101 , -1 , 1 , 102);
  ;;
  

 (**Cette fonction trace tout le serpent, en utilisant myplot pour chaque éléments de la liste composant le serpent*)
 let rec draw_whole_snake(s:t_snake) : unit =
  if s =[]
  then ()  
  else let first : t_position = fst(s) in
    (set_color(color_of_value(SNAKE));
     myplot(first.pt.x,first.pt.y);
     draw_whole_snake(rem_fst(s));)
 ;;
 
 (**Fonction auxiliaire récursive qui calcule le serpent lors l'initialisation du jeu suivant sa longueur*)
let rec aux_init_snake(length, snake : int*t_snake) : t_snake =
  if length=0
  then snake
    else
    aux_init_snake(
        (length-1), add_fst(snake, {pt = {x=(mysnake_position_init()).x ;y=(mysnake_position_init()).y-(length-1)}; dir = UP})
      )
;;

(**On utilise la fonction auxiliaire définie précedemment en lui donnant le paramètre de longueur souhaité*)
let init_snake() : t_snake =
  aux_init_snake( mysnake_length_init(), [])
;;

 (**Cette fonctin crée la matrice de jeu. Au début cette matrice n'est composée que de valeurs EMPTy sauf les bords qi ont des valeur FRAME *)  
let init_matrix() : t_matrix =
  let m : t_value matrix = mat_make( mymatrix_dx(), mymatrix_dy(),EMPTY) in
  m
;;

(**Cette fontion appelle les deux foncions d'initialisation définies au dessus et aujoute dans la matrice les valeurs SNAKE là ou le serpent se situe au début de la partie*) 
let init_snake_matrix() : t_snake * t_matrix =
  let (snake, mat) : t_snake  * t_matrix =(init_snake(), init_matrix()) in
  for i = 0 to (len(snake)-1)
  do
    mat.((nth(snake,i)).pt.x).((nth(snake,i)).pt.y) <- SNAKE;
  done;
  (snake,mat)
;;

 (**Cette fonction utilise les fonction précédente en initialisant le serpent et la matrice et en ouvrant le graph et traçant le serpent et le cadre*)
let init_play() : t_play =
  draw_frame();
  let (snake, matrix) : t_snake*t_matrix = init_snake_matrix() in
  draw_whole_snake(snake);
  {dt = ref (mydt_init()); sn = ref snake  ; mat = matrix};;

(**Cette fonction calcule la nouvelle position correspondante au déplacement d’une case de la matrice dans la direction.*)
let compute_new_position(pos, d : t_position * t_direction) : t_position =
  if d = RIGHT
  then {pt = {x = (pos.pt.x)+1; y= (pos.pt.y)}; dir=d}
  else if d = LEFT
  then {pt = {x = (pos.pt.x)-1; y= (pos.pt.y)}; dir=d}
  else if d = UP
  then {pt = {x = (pos.pt.x); y= (pos.pt.y)+1}; dir=d}
  else {pt = {x = (pos.pt.x); y= (pos.pt.y)-1}; dir=d};;

(**Cette fonction nous donne la nouvelle position en appelant la fonction compute_ne_position ainsi que la valeur dans la ase correspondante de la matrice. Si le serpent quitte la matrice la valeur FRAME est renvoyée*) 
let compute_move(pos, dir, m : t_position * t_direction * t_matrix) : t_position * t_value =
  let (new_pos) : t_position = compute_new_position(pos, dir) in
  if new_pos.pt.x<0 ||  new_pos.pt.x> mymatrix_dx() || new_pos.pt.y<0 ||  new_pos.pt.y> mymatrix_dy()
  then (new_pos , FRAME)
  else  (new_pos, m.(new_pos.pt.x).(new_pos.pt.y));;


(** Cette fonction supprime la dernier position du serpent en traçant un point blanc par dessus et donne la valeur EMPTY dans la matrice*) 
let remove_snake_tail(pl: t_play):unit=
  let (lst_pos1):t_position=lst(!(pl.sn)) in
  set_color(color_of_value(EMPTY));
  myplot(lst_pos1.pt.x, lst_pos1.pt.y);
  pl.sn := rem_lst(!(pl.sn));
  pl.mat.(lst_pos1.pt.x).(lst_pos1.pt.y)<-EMPTY;
;;

 (** Cette fonction ajooute la nouvelle position du serpent dans la fenêtre graphique avec myplot et donne la valeur SNAKE dans la matric*)
let add_snake_newhead(pl , newpos : t_play*t_position):unit=
    pl.sn := add_fst(!(pl.sn),newpos);
  pl.mat.(newpos.pt.x).(newpos.pt.y)<-SNAKE;
  set_color(color_of_value(SNAKE));
  myplot(newpos.pt.x, newpos.pt.y);
;;

 (**Utilise les deux fonctions précèdentes afin de faire une avancée du serpent vers  sa nouvelle posiiton en avançant sa tête et enlevant le bout de sa queue*)
let move_snake (pl , newpos : t_play*t_position):unit=
  remove_snake_tail(pl);
  add_snake_newhead(pl,newpos);
;;

 
 (**A partir de la position pos donnée en paramètre et de la localisation de la souris par rapport à la fenêtre d’affichage, cette fonction donne  une direction de déplacement*)
let analyze(pos : t_position) : t_direction =
  let (i, j) : int * int = mouse_pos() in
  let (k, l) : int * int = (mygraphic_x(pos.pt.x), mygraphic_y(pos.pt.y)) in
  let v : t_direction =
    if abs(i - k) < abs(j - l)
    then
      if j > l
      then UP
      else DOWN
    else
      if i < k
      then LEFT
      else RIGHT
  in
    if (pos.dir = UP && v = DOWN) || (pos.dir = DOWN && v = UP)
    then if i < k then LEFT else RIGHT
    else
      if (pos.dir = LEFT && v = RIGHT) || (pos.dir = RIGHT && v = LEFT)
      then if j > l then UP else DOWN
      else v
;;

(** Dans cette fonction on utilise la fonction analyze pour avoir la nouvelle direction, une ois celle ci obtenue on regarde où la prochaine position du serpent serait, si elle est dans le serpent (SNAKE) ou hors de la matrice(FRAME), alors on renvoie true sinon on renvoie false *)
let new_step(pl : t_play) : bool =
  let new_dir : t_direction = analyze(fst(!(pl.sn))) in
  let (new_pos, new_value : t_position*t_value)= compute_move((fst(!(pl.sn))),new_dir , pl.mat)in
  move_snake();
  if new_value= SNAKE ||  new_value= FRAME
  then true
  else false

(** La fonction handle_t_acc gère l’accélération du serpent, en vérifiant que le changement de vitesse (avec mydt_ratio) se fasse au moment souhaité;on vérifie cela en faisant !t - !t_acc > mydt_acc qui est la comparaison du temps écoulé depuis le début de l'appelle de la fonction simulation et le moment sohaité où l'on veut que le serpent accélère  *)
let handle_t_acc(t, t_acc, play : float ref * float ref * t_play) : unit =
  if (!t -. !t_acc) > mydt_acc()
  then
    (
    play.dt := !(play.dt) *. mydt_ratio() ;
    t_acc := !t ;
    )
;;

(**Cette fonction simule un tour. Pour cela elle boucle tant que le temps de jeu de la partie n'a pas atteint le temps où le serpent bouge (pl.dt). Une fois que ce temps est atteint on appelle handle_t_acc pour gérer l'accélération et on appelle new_step(pl) pour arrété la fonction et simuler un tour. *)
let simulation() : unit =
  let pl : t_play = init_play() in
  let t : float ref = ref (Sys.time()) and newt : float ref = ref (Sys.time())  in
  let tacc : float ref = ref (Sys.time()) in
  let thend : bool ref = ref false in
    (
    while not(!thend)
    do
      newt := Sys.time() ;
      while not((!newt -. !t) > !(pl.dt))
      do newt := Sys.time() ;
      done ;
      t := !newt ;
      handle_t_acc(t, tacc, pl) ;
      thend := new_step(pl) ;
    done ;
    )
;;

