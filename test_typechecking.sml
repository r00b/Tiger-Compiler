CM.make "sources.cm";

val systemPrint = print;

fun prepareprint strm str = TextIO.output (strm, str);

fun testing (testfile, output, expected) = 
  let 
      val testfile = ("testcases_typechecking/" ^ testfile)
      val output = "testcases_typechecking/" ^ output
      val expected = "testcases_typechecking/" ^ expected
      val outStrm = TextIO.openOut output
      val print = prepareprint outStrm 
      fun printResults () = 
        let val strm = TextIO.openIn "result.txt"
        in
          (systemPrint (TextIO.inputAll strm); TextIO.closeIn strm)
        end
  in
     (OS.Process.system ("echo \"" ^ testfile ^ "\n\"" ^ " >> result.txt");
     Main.compile testfile; 
     print "I love you\n";
     TextIO.closeOut outStrm;
     OS.Process.system ("diff " ^ output ^ " " ^ expected ^ " >> result.txt"); 
     OS.Process.system ("rm " ^ output);
     printResults ()
     )
  end;


systemPrint "testcases_typechecking/t1.tig\n";

testing("t1.tig", "t1.out", "t1.exp");

OS.Process.exit(OS.Process.success);

