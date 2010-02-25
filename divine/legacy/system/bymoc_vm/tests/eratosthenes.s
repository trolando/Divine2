!module "main"

	GLOBSZ	1
	LDC	8
	LDC	-31
	CHNEW	0	2
	STVA	G	1u	0
	LOCSZ	4
	STEP	1	N	0
	JMP	P_init
P_sieve:
	LDC	8
	LDC	-31
	CHNEW	0	2
	STVA	L	1u	5
	LDC	0
	STVA	L	1u	6
	LDC	0
	STVA	L	4	7
	PRINTS	2
	LDVA	L	4	1
	PRINTV	100
	PRINTS	3
	STEP	1	N	0
S1_sieve_end:
L0:
	NDET	L1
	LDVA	L	1u	0
	TOP	r0
	CHLEN
	NEXZ
	PUSH	r0
	PUSH	r0
	PUSH	r0
	CHGETO	0
	LDC	0
	EQ
	NEXZ
	CHGETO	1
	TRUNC	-31
	STVA	L	4	7
	CHDEL
	STEP	2	N	0
	NDET	L3
	LDVA	L	4	7
	LDVA	L	4	1
	MOD
	LDC	0
	EQ
	NEXZ
	STEP	3	N	0
	PRINTS	4
	LDVA	L	4	7
	PRINTV	100
	PRINTS	5
	LDVA	L	4	1
	PRINTV	100
	PRINTS	6
	LDVA	L	4	7
	LDVA	L	4	1
	DIV
	PRINTV	100
	PRINTS	7
	STEP	3	N	0
	JMP	L4
L3:
	STEP	2	N	0
	NDET	L5
	LDVA	L	1u	6
	BNOT
	NEXZ
	STEP	4	N	0
	LDC	1
	TRUNC	1
	STVA	L	1u	6
	STEP	4	N	0
	LDC	8
	LDVA	L	1u	5
	LDC	-31
	LDVA	L	4	7
	RUN	11	2	P_sieve
	NEXZ
	STEP	4	N	0
	JMP	L6
L5:
	STEP	3	N	0
	LDVA	L	1u	5
	TOP	r0
	CHFREE
	NEXZ
	PUSH	r0
	CHADD
	PUSH	r0
	TOP	r0
	PUSH	r0
	LDC	0
	CHSETO	0
	LDVA	L	4	7
	CHSETO	1
	STEP	4	N	0
L6:
L4:
	JMP	L0
L1:
	LDVA	L	1u	0
	TOP	r0
	CHLEN
	NEXZ
	PUSH	r0
	PUSH	r0
	PUSH	r0
	TOP	r0
	PUSH	r0
	CHGETO	0
	LDC	1
	EQ
	POP	r1
	POP	r0
	PUSH	r1
	PUSH	r0
	CHGETO	1
	LDC	0
	EQ
	AND
	NEXZ
	POP	r0
	CHDEL
	STEP	2	N	0
	JMP	L2
	JMP	L0
L2:
	STEP	2	N	0
	NDET	L7
	LDVA	L	1u	6
	NEXZ
	STEP	2	N	0
	LDVA	L	1u	5
	TOP	r0
	CHFREE
	NEXZ
	PUSH	r0
	CHADD
	PUSH	r0
	TOP	r0
	PUSH	r0
	LDC	1
	CHSETO	0
	LDC	0
	CHSETO	1
	STEP	2	T	0
	JMP	L8
L7:
	STEP	1	T	0
L8:
P_init:
	LDC	2
	STVA	L	4	0
	LDC	8
	LDVA	G	1u	0
	LDC	-31
	LDVA	L	4	0
	RUN	11	2	P_sieve
	NEXZ
	STEP	1	N	0
L9:
	NDET	L10
	LDVA	L	4	0
	LDC	25
	LT
	NEXZ
	STEP	2	N	0
	LDC	0
	TOP	r0
	LDV	L	4
	LDC	1
	ADD
	TRUNC	-31
	PUSH	r0
	STV	L	4
	STEP	2	N	0
	LDVA	G	1u	0
	TOP	r0
	CHFREE
	NEXZ
	PUSH	r0
	CHADD
	PUSH	r0
	TOP	r0
	PUSH	r0
	LDC	0
	CHSETO	0
	LDVA	L	4	0
	CHSETO	1
	STEP	2	N	0
	JMP	L9
L10:
	LDVA	L	4	0
	LDC	25
	GTE
	NEXZ
	STEP	2	N	0
	LDVA	G	1u	0
	TOP	r0
	CHFREE
	NEXZ
	PUSH	r0
	CHADD
	PUSH	r0
	TOP	r0
	PUSH	r0
	LDC	1
	CHSETO	0
	LDC	0
	CHSETO	1
	STEP	2	N	0
	JMP	L11
	JMP	L9
L11:
	STEP	2	T	0

!string	0	"number"
!string	1	"eof"
!string	2	"MSC: "
!string	3	" is prime\n"
!string	4	"MSC: "
!string	5	" = "
!string	6	"*"
!string	7	"\n"

