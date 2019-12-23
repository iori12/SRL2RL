open Syntax
open Compiler
open Pretty
open Invert
open Optimize
   
let srl2rl s = rl_prints(label_rl(opt_rl(srlc s)))
let rl_inver s = rl_prints(label_rl(opt_rl(invertr(srlc s))))
let srl_inver s = srl_prints(invert s)
let parse line = Parser.main Lexer.token (Lexing.from_string line )  
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

