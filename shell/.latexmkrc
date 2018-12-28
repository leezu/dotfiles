$preview_prefix="__preview__";
$preview_file = "$preview_prefix%R__.pdf";
$sync_file = "$preview_prefix%R__.synctex.gz";

$pdflatex="xelatex -synctex=1 %O %S; cp %R.pdf $preview_file; cp %R.synctex.gz $sync_file";
$pdf_previewer = "start okular %O $preview_file > /dev/null 2>&1";

# Glossaries support
# https://tex.stackexchange.com/questions/1226/how-to-make-latexmk-use-makeglossaries/44316#44316
add_cus_dep('glo', 'gls', 0, 'run_makeglossaries');
add_cus_dep('acn', 'acr', 0, 'run_makeglossaries');
sub run_makeglossaries {
  if ( $silent ) {
    system "makeglossaries -q '$_[0]'";
  }
  else {
    system "makeglossaries '$_[0]'";
  };
}
push @generated_exts, 'glo', 'gls', 'glg';
push @generated_exts, 'acn', 'acr', 'alg';
$clean_ext .= ' %R.ist %R.xdy';
