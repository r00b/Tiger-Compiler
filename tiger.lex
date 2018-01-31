type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos

val str : string ref = ref ""

fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end


%%
%s STRING_STATE;
chars=[\ !#\$%&'()*+,\-./0-9:;<=>?@A-Z[\\\]\^_`a-z{|}~];
%%
<INITIAL>\n	 => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL>type     => (Tokens.TYPE(yypos,yypos+size yytext));
<INITIAL>var   	 => (Tokens.VAR(yypos,yypos+size yytext));
<INITIAL>function => (Tokens.FUNCTION(yypos,yypos+size yytext));
<INITIAL>break    => (Tokens.BREAK(yypos,yypos+size yytext));
<INITIAL>of       => (Tokens.OF(yypos,yypos+size yytext));
<INITIAL>end      => (Tokens.END(yypos,yypos+size yytext));
<INITIAL>in       => (Tokens.IN(yypos,yypos+size yytext));
<INITIAL>nil      => (Tokens.NIL(yypos,yypos+size yytext));
<INITIAL>let      => (Tokens.LET(yypos,yypos+size yytext));
<INITIAL>do       => (Tokens.DO(yypos,yypos+size yytext));
<INITIAL>to       => (Tokens.TO(yypos,yypos+size yytext));
<INITIAL>for      => (Tokens.FOR(yypos,yypos+size yytext));
<INITIAL>while    => (Tokens.WHILE(yypos,yypos+size yytext));
<INITIAL>else     => (Tokens.ELSE(yypos,yypos+size yytext));
<INITIAL>then     => (Tokens.THEN(yypos,yypos+size yytext));
<INITIAL>if       => (Tokens.IF(yypos,yypos+size yytext));
<INITIAL>array    => (Tokens.ARRAY(yypos,yypos+size yytext));
<INITIAL>":="     => (Tokens.ASSIGN(yypos,yypos+size yytext));
<INITIAL>"|"      => (Tokens.OR(yypos,yypos+size yytext));
<INITIAL>&        => (Tokens.AND(yypos,yypos+size yytext));
<INITIAL>">="     => (Tokens.GE(yypos,yypos+size yytext));
<INITIAL>">"      => (Tokens.GT(yypos,yypos+size yytext));
<INITIAL>"<="     => (Tokens.LE(yypos,yypos+size yytext));
<INITIAL>"<"      => (Tokens.LT(yypos,yypos+size yytext));
<INITIAL>"<>"     => (Tokens.NEQ(yypos,yypos+size yytext));
<INITIAL>"="      => (Tokens.EQ(yypos,yypos+size yytext));
<INITIAL>"/"      => (Tokens.DIVIDE(yypos,yypos+size yytext));
<INITIAL>"*"      => (Tokens.TIMES(yypos,yypos+size yytext));
<INITIAL>-        => (Tokens.MINUS(yypos,yypos+size yytext));
<INITIAL>"+"      => (Tokens.PLUS(yypos,yypos+size yytext));
<INITIAL>"."      => (Tokens.DOT(yypos,yypos+size yytext));
<INITIAL>"}"      => (Tokens.RBRACE(yypos,yypos+size yytext));
<INITIAL>"{"      => (Tokens.LBRACE(yypos,yypos+size yytext));
<INITIAL>"]"      => (Tokens.RBRACK(yypos,yypos+size yytext));
<INITIAL>"["      => (Tokens.LBRACK(yypos,yypos+size yytext));
<INITIAL>")"      => (Tokens.RPAREN(yypos,yypos+size yytext));
<INITIAL>"("      => (Tokens.LPAREN(yypos,yypos+size yytext));
<INITIAL>";"      => (Tokens.SEMICOLON(yypos,yypos+size yytext));
<INITIAL>:        => (Tokens.COLON(yypos,yypos+size yytext));
<INITIAL>","      => (Tokens.COMMA(yypos,yypos+size yytext));
<INITIAL>"\"" => (YYBEGIN STRING_STATE; str := ""; continue());
<STRING_STATE>\\\" => (str := (!str) ^ "\""; continue());
<STRING_STATE>\\n => (str := (!str) ^ "\n"; continue());
<STRING_STATE>\\t => (str := (!str) ^ "\t"; continue());
<STRING_STATE>\\\^c|\\\^C => (str := (!str) ^ "^c"; continue());
<STRING_STATE>\\ddd => (str := (!str) ^ "\n"; continue());
<STRING_STATE>{chars} => (str := (!str) ^ yytext; continue());
<STRING_STATE>"\"" => (YYBEGIN INITIAL; print("exited\n"); Tokens.STRING(!str,yypos,yypos+size (!str)));
<INITIAL>.        => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());
