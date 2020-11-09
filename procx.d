#!/usr/sbin/dtrace -C -q -s
/*
For every process print:
SIGSEGV handler address;
RLIMIT_CORE current limit;

Bugs: crashes at the end.
*/

BEGIN {
	p = mach_kernel`allproc.lh_first;
}

/* These are separate for error handling purposes */

tick-1001 /p == 0/ {
	exit(0);
}

#define SEGVPROC p->p_sigacts->ps_sigact[11]

tick-1001 /p != 0 && SEGVPROC != 0/ {
	printf("SEGV=0x%p ", SEGVPROC);
}

#define CORE p->p_limit->pl_rlimit[4].rlim_cur

tick-1001 /p != 0 && CORE != 0/ {
	printf("CORE=0x%p ", CORE);
}

#define NAME(p__) stringof((p__)->p_textvp->v_name)
#define PORT ((struct task*)p->task)->exc_actions[1].port
#define RECEIVER_PROC(port__) \
((struct proc *)(port__->data.receiver->is_task->bsd_info))

tick-1001 /p != 0 && PORT/ {
	printf("PORT=%p ", PORT);
	printf("(%4d %s) ", RECEIVER_PROC(PORT)->p_pid, NAME(RECEIVER_PROC(PORT)));
}

#define DISPLAY (p && (PORT || CORE || SEGVPROC))

tick-1001 /DISPLAY/ {
	printf("%d ", p->p_pid);
}

tick-1001 /DISPLAY/ {
	printf("%s ", NAME(p));
}

#if 0
#define GID(n__) \
tick-1001 /DISPLAY/ { \
	printf("%d ", p->p_ucred->cr_posix.cr_groups[n__]); \
}

#define GID2(n__) \
GID(n__) \
GID(n__+1)

#define GID4(n__) \
GID2(n__) \
GID2(n__+2)

GID4(0)
GID4(4)
GID4(8)
GID4(12)
#endif

tick-1001 /DISPLAY/ {
	printf("<- %d ", p->p_ppid);
}

tick-1001 /DISPLAY && p->p_pptr != 0/ {
	printf("%s", NAME(p->p_pptr));
}

tick-1001 /DISPLAY/ {
	printf("\n");
}

tick-1001 /p != 0/ {
	p = p->p_list.le_next;
}

END {
	printf("%4d=c_thr_exc_raise\n", mach_kernel`c_thr_exc_raise);
	printf("%4d=c_thr_exc_raise_state\n", mach_kernel`c_thr_exc_raise_state);
	printf("%4d=c_thr_exc_raise_state_id\n",
		mach_kernel`c_thr_exc_raise_state_id);
	printf("%4d=c_tsk_exc_raise\n", mach_kernel`c_tsk_exc_raise);
	printf("%4d=c_tsk_exc_raise_state\n", mach_kernel`c_tsk_exc_raise_state);
	printf("%4d=c_tsk_exc_raise_state_id\n",
		mach_kernel`c_tsk_exc_raise_state_id);
	printf("host[EXC_BAD_ACCESS] port=%p ",
		mach_kernel`realhost.exc_actions[1].port);
	printf("flavor=%d behavior=0x%X\n",
		mach_kernel`realhost.exc_actions[1].flavor,
		mach_kernel`realhost.exc_actions[1].behavior);
}	

