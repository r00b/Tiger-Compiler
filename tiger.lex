type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
val cc = ref 0 (* commentCounter *)
val sc = ref true
val str : string ref = ref ""

fun err(p1,p2) = ErrorMsg.error p1;
  
fun checkCommentClosed cc = if !cc <> 0
  then ((cc := 0); ErrorMsg.error 10 ("illegal comment "))
  else ();

fun checkStringClosed sc = if !sc
  then ()
  else ((sc := true); ErrorMsg.error 10 ("illegal string "));

fun eof() =
  let
    val () = checkStringClosed sc
    val () = checkCommentClosed cc
    val pos = hd(!linePos)
  in
    Tokens.EOF(pos,pos)
  end;

fun dddToInt yytext= Int.fromString( String.substring(yytext, 1, String.size(yytext)-1))
fun dddToASCII yytext = String.str(Char.chr(valOf(dddToInt yytext)))
  
fun controlCharCaps yytext = (Char.toString (chr ((ord (String.sub(yytext, 2))) - 64)))
fun controlCharLower yytext = (Char.toString (chr ((ord (String.sub(yytext, 2))) - 96)))

%%
%s COMMENT STRING_STATE;
notAster=[^*];
chars=[ !#\$%&'()*+,\-./0-9:;<=>?@A-Z[\]\^_`a-z{|}~];
digits=[0-9];
%%
<INITIAL>[\t\ ]*  => (continue());
<INITIAL>\n	  => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL>type     => (Tokens.TYPE(yypos,yypos+size yytext));
<INITIAL>var   	  => (Tokens.VAR(yypos,yypos+size yytext));
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

<INITIAL>"/*"                    => (cc := 0; YYBEGIN COMMENT; cc := !cc + 1; continue());
<COMMENT>{notAster}|([*]+[^*/])* => (continue());
<COMMENT>"/*"                    => (cc := !cc + 1; continue());
<COMMENT>[*]+"/"                 => (cc := !cc - 1; if !cc = 0 then YYBEGIN INITIAL else (); continue());


<INITIAL>"\""                 => (YYBEGIN STRING_STATE; sc := false; str := ""; continue());
<STRING_STATE>\\\\            => (str := (!str) ^ "\\"; continue());
<STRING_STATE>\\[\n\t\f\ ]*\\ => (continue());


<STRING_STATE>\\"^"[@-Z]      => (str := (!str) ^ controlCharCaps (yytext); continue());
<STRING_STATE>\\"^"[a-z]      => (str := (!str) ^ controlCharLower (yytext); continue());

<STRING_STATE>\\12[0-6]       => (str := (!str) ^ dddToASCII (yytext); continue());
<STRING_STATE>\\1[01][0-9]    => (str := (!str) ^ dddToASCII (yytext); continue());
<STRING_STATE>\\[4-9][0-9]    => (str := (!str) ^ dddToASCII (yytext); continue());
<STRING_STATE>\\3[2-9]        => (str := (!str) ^ dddToASCII (yytext); continue());
<STRING_STATE>\\(9|10|12)     => (str := (!str) ^ dddToASCII (yytext); continue());
<STRING_STATE>\\n             => (str := (!str) ^ "\n"; continue());
<STRING_STATE>\\t             => (str := (!str) ^ "\t"; continue());
<STRING_STATE>\\\"            => (str := (!str) ^ "\""; continue());
<STRING_STATE>\\.             => (ErrorMsg.error yypos ("Illegal escape sequence: in string: " ^ yytext); continue());
<STRING_STATE>{chars}         => (str := (!str) ^ yytext; continue());
<STRING_STATE>"\""            => (YYBEGIN INITIAL; sc := true; Tokens.STRING(!str,yypos,yypos+size (!str)));
<STRING_STATE>[^{chars}]      => (ErrorMsg.error yypos ("Illegal characters inside string: " ^ yytext); continue());

<INITIAL>{digits}|([1-9]{digits}+) => (Tokens.INT((valOf (Int.fromString yytext)),yypos,yypos+size yytext));
<INITIAL>[a-zA-Z][a-zA-Z0-9_]* => (Tokens.ID(yytext,yypos,yypos+size yytext));
<INITIAL>.        => (ErrorMsg.error yypos ("Illegal character " ^ yytext); continue());
