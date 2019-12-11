open Syntax
open Compiler
open Pretty
open Invert

let srl2rl s = rl_prints(srlc s)
let rl_inver s = rl_prints(invertr(srlc s))
let srl_inver s = srl_prints(invert s)
  
let rec read_print () =
  print_string "->";
  flush stdout;
  let line = Parser.main Lexer.token (Lexing.from_channel stdin )
  in
  print_string("\nSRL:\n" ^ srl_prints(line) ^ "\n\n");
  print_string("SRL inversion:\n" ^ (srl_inver line) ^ "\n\n"); 
  print_string("RL:\n");
  srl2rl line;
  print_string "\n";
  print_string("RL invertsion:\n");
  rl_inver line;
  read_print ()
;;

read_print () 

