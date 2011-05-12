dpl() {
    for name in $@; do
	case $name in 
	    *.js)
		cscript /nologo "d:\\programs\\jslint.js" < $name
		;;
	esac
    done	
    scp $@ admin@king:/opt/funstuff
}
q () {
	[ -d $1 ] && ls -l $1 || cat $1
}
alias rks="DISPLAY=:0 mplayer -ao alsa:device=rook"
alias sil="DISPLAY=:0 mplayer"
alias cf="./configure"
alias prof="valgrind --tool=callgrind"
