open Tfg
open Program
open Utest
open Ustring.Op
open Sys


let v0 = Timingpoint({kind = Soft; relative_arrival= Some(0); relative_deadline=Some(0);release_jitter=None; trigger_precision=(Some(0)); tstm=Exp})

let v1 = Fragment({bcet=None; wcet=None; critical=false; cstm=Exp})

let v2 = Timingpoint({kind = Soft; relative_arrival= Some(20); relative_deadline=Some(20);release_jitter=None; trigger_precision=(Some(0)); tstm=Exp})

let vlist = [(0,v0);(1,v1);(2,v2)]

let elist = [(0,1);(1,2)]

let main()=
   Utest.init "Test Timed C and TFG C generation"; 
   let seed = 1 in
   let t = {nodes = vlist; edges = elist} in
   let _ = Program.generate_program_files seed t in
   let res = Sys.command "diff -u --ignore-space-change --strip-trailing-cr --ignore-blank-lines test/gen_1.c test/expected_gen_001.c" in
   Utest.test_int "simple tfg program generation" 0 res;
   Utest.result()





