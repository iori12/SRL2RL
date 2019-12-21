open Syntax

let invert_op = function
  | Plus -> Minus
  | Minus -> Plus
  | Caret -> Caret
           
let invert_a = function
  | Plus_Eq(x,op,e) -> Plus_Eq(x,invert_op op,e)
  | Plus_In(x,e1,op,e2) -> Plus_In(x,e1,invert_op op,e2)
  | Push(x1,x2) -> Push(x1,x2)
  | Pop(x1,x2) -> Pop(x1,x2)
  | Skip -> Skip
 
let rec invert = function
  | SCon(b1,b2) -> SCon(invert b2,invert b1)
  | SIf(e1,b1,b2,e2) -> SIf(e2,invert b1,invert b2,e1)
  | SFrom(e1,b1,b2,e2) ->SFrom(e2,invert b1,invert b2,e1)
  | SStep(a) -> SStep(invert_a a)

let invertr = 
  let rec invertrs = 
    let rec invert_a2 = function
      | [] -> []
      | [x] -> [invert_a x]
      | x::xs -> [invert_a x] @ (invert_a2 xs)
    in
    let invert_j = function
      | RGoto(Label(n)) -> RFrom(Label(n))
      | RIf(e,Label(n1),Label(n2)) -> RFi(e,Label(n1),Label(n2))
      | RExit -> REntry
    in
    let invert_f = function
      | RFrom(Label(n)) -> RGoto(Label(n))
      | RFi(e,Label(n1),Label(n2)) -> RIf(e,Label(n1),Label(n2))
      | REntry -> RExit
    in
    let invert_b = function
      | RBlk(Label(n),f,a,j) -> RBlk(Label(n),invert_j j,invert_a2 a,invert_f f)
    in
    function
    | [] -> []
    | [x] -> [invert_b x]                             
    | x :: xs -> [(invert_b x)] @ (invertrs xs)
  in
  function
  | RLBLK(rlblk) -> RLBLK(List.rev(invertrs rlblk))
