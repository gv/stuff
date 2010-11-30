_qn() {
	COMPREPLY=( $(qn --complete $COMP_CWORD ${COMP_WORDS[*]}) )
}

complete -F _qn qn
