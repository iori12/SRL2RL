open Syntax
let rec srlc b =
  let max2 a b = if (int_of_string a)>(int_of_string b) then a else b
  in
  let max4 a b c d = if (max2 a b) > (max2 c d) then (max2 a b) else (max2 c d)
  in
  let pl n x = string_of_int((int_of_string x) + (int_of_string n))
  in
  let rec srlcc c (la,lb,lc,ld) =
    let x = max4 la lb lc ld
    in
    match c with
    | SCon(b1,b2) -> (srlcc b1 (la,lb,pl x la,pl x lb))
                     @(srlcc b2 (pl x la,pl x lb,lc,ld))
                   
    | SStep(a) -> [RBlk(Label lb,RFrom(Label la),[a],RGoto(Label lc))]
                  @[RBlk(Label lc,RFrom(Label lb),[],RGoto(Label ld))]
                
    | SIf(e1,b1,b2,e2) ->
       [RBlk(Label lb,RFrom(Label la),[],RIf(e1,Label (pl x la),Label (pl x lc)))]
       @(srlcc b1 (lb,pl x la,pl x lb,lc))
       @(srlcc b2 (lb,pl x lc,pl x ld,lc))
       @[RBlk(Label lc,RFi(e2,Label (pl x lb),Label (pl x ld)),[],RGoto(Label ld))]
      
    | SFrom(e1,b1,b2,e2) ->
       [RBlk(Label lb,RFi(e1,Label la,Label (pl x ld)),[],RGoto(Label (pl x la)))]
       @(srlcc b1 (lb,pl x la,pl x lb,lc))
       @(srlcc b2 (lc,pl x lc,pl x ld,lb))
       @[RBlk(Label lc,RFrom(Label (pl x lb)),[],RIf(e2,Label ld,Label (pl x lc)))]
  in
  RLBLK([RBlk(Label "1",REntry,[],RGoto(Label "2"))]
        @(srlcc b ("1","2","3","4"))
        @[RBlk(Label "4",RFrom(Label "3"),[],RExit)])
 
   
