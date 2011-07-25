print "<html><head><style><!-- a:link{color: #ee8800;} a:visited{color: #ee4400;} pre{font-size:80%; padding:0.5em; background-color: #eee; border: 1px dashed;}--></style></head><body><p>";
while (<>) {
  if    (/^::(.*)::$/) {print "<h1>$1</h1>";}
  elsif (/^:(.*):$/)   {print "<h2>$1</h2>";}
  elsif (/^\*(.*)/)    {print "<ul><li>$1</li>"; ul_loop(0);}
  elsif (/^ (.*)/)     {print "<pre>$1"; pre_loop();}
  elsif (/^\s*$/)      {print "</p><p>";}
  else { s#\[([^\]]*?)\>(.*?)\]#"<a href=\"" . url_encode("$2") . "\">$1</a>"#ge;
         s#\[(.*?)\]#"<a href=\"" . url_encode("$1") . "\">$1</a>"#ge;
         print $_; }}
print "</p></body></html>\n";

sub pre_loop {
  while (<>) {
    if (/^ (.*)/) {print "\n$1";}
    else {print "</pre>"; return;}}}

sub ul_loop {
  while (<>) {
    if (/^\*(.*)/) {print "<li>$1</li>";}
    else {print "</ul>"; return;}}}

sub url_encode {
  ($_) = @_;
  s/:/%3A/g;
  s/ /_/g;
  return $_;
}
