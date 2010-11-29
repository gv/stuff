_qn() {
	COMPREPLY=( $(qn --complete ${COMP_WORDS[*]}) )
}

complete -F _qn qn
