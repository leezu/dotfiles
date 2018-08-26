$preview_prefix="__preview__";
$preview_file = "$preview_prefix%R__.pdf";
$sync_file = "$preview_prefix%R__.synctex.gz";

$pdflatex="lualatex -synctex=1 %O %S; cp %R.pdf $preview_file; cp %R.synctex.gz $sync_file";
$pdf_previewer = "start okular %O $preview_file > /dev/null 2>&1";