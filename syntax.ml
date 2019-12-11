(*step operation and expressions*)

type var =
  Var of string
type oplus =
  | Plus
  | Minus
  | Caret

type otime =
  | OPlus of oplus
  | Time
  | Div
  | Equal
  | Less  (*<*)
  | Greater (*>*)
  | Less_Eq (*<=*)
  | Greater_Eq (*>=*)
  | Not_Eq    (*!=*)

type exp =
  | EConst of int
  | EVar of var
  | EIn of var * exp
  | ETime of exp * otime * exp
  | ETop of var
  | EEmpty of var

type step =
  | Plus_Eq of var * oplus * exp
  | Plus_In of var * exp * oplus * exp
  | Push of var * var
  | Pop of var * var
  | Skip 

(*language SRL*)
  
type blk =
  | SStep of step
  | SIf of exp * blk * blk * exp
  | SCon of blk * blk
  | SFrom of exp * blk * blk * exp
           
type srl = SBLK of blk

(*language RL*)
                
type label = Label of string
                     
type jump =
  | RGoto of label
  | RIf of exp * label * label
  | RExit

type from =
  | RFrom of label
  | RFi of exp * label * label
  | REntry

type rlblk =
  | RBlk1 of label * from  * jump
  | RBlk2 of label * from * step * jump

type rl = RLBLK of rlblk list 

                         



  

