open Syntax

(*step operation and expressions Pretty Printer*)
   
let print_v = function
  | Var(s) -> s
            
let print_op = function
  | Plus -> "+"
  | Minus -> "-"
  | Caret -> "^"
           
let print_ot = function
  | OPlus(n) -> print_op n
  | Time -> "*"
  | Div -> "/"
  | Equal -> "="
  | Less -> "<"
  | Greater -> ">"
  | Less_Eq -> "<="
  | Greater_Eq -> ">="
  | Not_Eq -> "!="
            
let rec print_e = function
  | EConst(n) -> string_of_int n
  | EVar(x) -> print_v x
  | EIn(x,e) -> (print_v x) ^ "[" ^ (print_e e) ^ "]"
  | ETime(e1,ot,e2) -> (print_e e1) ^ " " ^(print_ot ot) ^ " " ^ (print_e e2)
  | ETop(x) -> "top " ^ (print_v x)
  | EEmpty(x) -> "empty " ^ (print_v x)
               
let print_a = function
  | Plus_Eq(x,op,e) -> (print_v x) ^ " " ^ (print_op op) ^ "= " ^ (print_e e)
  | Plus_In(x,e1,op,e2) ->
     (print_v x) ^ "[" ^ (print_e e1) ^ "] " ^ (print_op op) ^ "= " ^ (print_e e2)
  | Push(x1,x2) -> "push " ^ (print_v x1) ^ " " ^ (print_v x2)
  | Pop(x1,x2) -> "pop " ^ (print_v x1) ^ " " ^ (print_v x2)
  | Skip -> "skip"

(*RL Pretty Printer*)
          
let rec rl_prints = 
let rec rl_print = 
let print_l = function
  | Label(s) -> s
in
let rec print_a2 = function
  | [] -> ""
  | [x] -> (print_a x) ^ "\n\t"
  | x::xs -> (print_a x) ^ "\n" ^ (print_a2 xs) ^ "\n\t"
in
let print_j = function
  | RGoto(l) -> "goto " ^ (print_l l)
  | RIf(e,l1,l2) ->
     "if " ^ (print_e e) ^ " goto " ^ (print_l l1) ^ " else " ^ (print_l l2)
  | RExit -> "exit"
in
let print_k = function
  | RFrom(l) -> "from " ^ (print_l l)
  | RFi(e,l1,l2) ->
     "fi " ^ (print_e e) ^ " from " ^ (print_l l1) ^ " else " ^ (print_l l2)
  | REntry -> "entry" 
in
let print_blk = function
  | RBlk(l,k,a,j) ->
     (print_l l) ^ ": \t" ^ (print_k k) ^ "\n\t" ^ (print_a2 a)  ^ (print_j j)
in
function
| [] -> ""
| [x] -> print_blk x
| x :: xs -> ((print_blk x) ^ "\n" ^ (rl_print xs))
in
function
| RLBLK(rlblk) -> print_string ((rl_print rlblk) ^ "\n")

(*SRL Pretty Printer*)
                
let rec srl_prints = function
  | SStep(a) -> print_a a
              
  | SIf(e1,b1,b2,e2) ->
     "if " ^ (print_e e1) ^ " then " ^ (srl_prints b1) ^ " else "
     ^ (srl_prints b2) ^ " fi " ^ (print_e e2) ^ " "
    
  | SCon(b1,b2) -> (srl_prints b1) ^ " " ^ (srl_prints b2)
                 
  | SFrom(e1,b1,b2,e2) ->
     "from " ^ (print_e e1) ^ " do " ^ (srl_prints b1)
     ^ " loop " ^ (srl_prints b2) ^ " until " ^ (print_e e2) ^ " "
