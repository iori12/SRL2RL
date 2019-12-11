open Syntax

let invert_op op =
  match op with
  | Plus -> Minus
  | Minus -> Plus
  | Caret -> Caret
           
let invert_a a =
  match a with
  | Plus_Eq(x,op,e) -> Plus_Eq(x,invert_op op,e)
  | Plus_In(x,e1,op,e2) -> Plus_In(x,e1,invert_op op,e2)
  | Push(x1,x2) -> Push(x1,x2)
  | Pop(x1,x2) -> Pop(x1,x2)
  | Skip -> Skip
 
let rec invert srl =
  match srl with    
  | SCon(b1,b2) -> SCon(invert b2,invert b1)
  | SIf(e1,b1,b2,e2) -> SIf(e2,invert b1,invert b2,e1)
  | SFrom(e1,b1,b2,e2) ->SFrom(e2,invert b1,invert b2,e1)
  | SStep(a) -> SStep(invert_a a)

let invertr rl1 =
  let rec invertrs rl2 =
    let invert_j rl3 =
      match rl3 with
      | RGoto(Label(n)) -> RFrom(Label(n))
      | RIf(e,Label(n1),Label(n2)) -> RFi(e,Label(n1),Label(n2))
      | RExit -> REntry
    in
    let invert_f rl4 =
      match rl4 with
      | RFrom(Label(n)) -> RGoto(Label(n))
      | RFi(e,Label(n1),Label(n2)) -> RIf(e,Label(n1),Label(n2))
      | REntry -> RExit
    in
    let invert_b rl5 =
      match rl5 with
      | RBlk1(Label(n),f,j) -> RBlk1(Label(n),invert_j j,invert_f f)
      | RBlk2(Label(n),f,a,j) -> RBlk2(Label(n),invert_j j,invert_a a,invert_f f)
    in
    match rl2 with
    | [] -> []
    | [x] -> [invert_b x]                             
    | x :: xs -> [(invert_b x)] @ (invertrs xs)
  in
  match rl1 with
  | RLBLK(rlblk) -> RLBLK(List.rev(invertrs rlblk))

