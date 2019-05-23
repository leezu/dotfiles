alias ddevil='dict -d devil'
alias wn='dict -d wn'
alias ee='dict -d moby-thesaurus'
alias ed='dict -d fd-eng-deu'
alias de='dict -d fd-deu-eng'
function ze() {
    grep $argv ~/.cedict.txt
}
function zes() {
    grep '^. '$argv ~/.cedict.txt
}
function zed() {
    grep '^.. '$argv ~/.cedict.txt
}