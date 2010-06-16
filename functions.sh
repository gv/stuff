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
