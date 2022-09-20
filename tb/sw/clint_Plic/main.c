
#include <stdint.h>

#define ACLINT_BASE 0x20000
#define MTIMECMP 0
#define MTIME 0x8000
#define MSIP 0x9000
#define SSIP 0xA000

volatile uint64_t *aclint_mtimecmp = (uint64_t*)( ACLINT_BASE + MTIMECMP );
volatile uint64_t *aclint_mtime    = (uint64_t*)( ACLINT_BASE + MTIME );
volatile uint8_t  *aclint_msip     = (uint8_t*)( ACLINT_BASE + MSIP );
volatile uint8_t  *aclint_ssip     = (uint8_t*)( ACLINT_BASE + SSIP );

#define PLIC_BASE 0x10000000
#define PRIORITY(d) (0x0+4*(d+1))
#define PENDING   0x1000
#define ENABLES(h) (0x002000 + 0x80*h)
#define HARTREG(h) (0x200000 + 0x1000*h)

volatile uint32_t *plic_priority_0          = (uint32_t*)(PLIC_BASE+PRIORITY(0) );
volatile uint32_t *plic_priority_1          = (uint32_t*)(PLIC_BASE+PRIORITY(1) );
volatile uint32_t *plic_priority_2          = (uint32_t*)(PLIC_BASE+PRIORITY(2) );
volatile uint32_t *plic_pending             = (uint32_t*)(PLIC_BASE+PENDING );
volatile uint32_t *plic_enable_0            = (uint32_t*)(PLIC_BASE+ENABLES(0) );
volatile uint32_t *plic_hartReg_0           = (uint32_t*)(PLIC_BASE+HARTREG(0) );
volatile uint32_t *plic_claim_complete_0    = (uint32_t*)(PLIC_BASE+HARTREG(0)+4 );




static int enableGlobalInt(){
	__asm__ ( "li t0, 0xa" );
	__asm__ ( "csrs mstatus, t0" );
	return 0;
}

static int disableGlobalInt(){
	__asm__ ( "li t0, 0xa" );
	__asm__ ( "csrc mstatus, t0" );
	return 0;	
}

static int enableMExtInt(){
	__asm__ ( "li t0, 0x800" );
	__asm__ ( "csrs mie, t0" );
	return 0;
}

static int disableMExtInt(){
	__asm__ ( "li t0, 0x800" );
	__asm__ ( "csrc mie, t0" );
	return 0;
}

static int enableMTimerInt(){
	__asm__ ( "li t0, 0x80" );
	__asm__ ( "csrs mie, t0" );
	return 0;
}

static int disableMTimerInt(){
	__asm__ ( "li t0, 0x80" );
	__asm__ ( "csrc mie, t0" );
	return 0;
}

static int enableMSoftInt(){
	__asm__ ( "li t0, 0x8" );
	__asm__ ( "csrs mie, t0" );
	return 0;
}

static int disableMSoftInt(){
	__asm__ ( "li t0, 0x8" );
	__asm__ ( "csrc mie, t0" );
	return 0;
}







static int setUpAClintTimmer() {
	(*aclint_mtimecmp) = 0xff;
	(*aclint_mtime)    = 0x0;

	enableMTimerInt();
	enableGlobalInt();
	return 0;
}

static int resloveAClintTimmer() {
	(*aclint_mtime)    = 0x0;

	disableMTimerInt();
	enableGlobalInt();
	return 0;
}

static int setUpAClintSoft() {
	(*aclint_msip) = 1;

	enableMSoftInt();
	enableGlobalInt();
	return 0;
}

static int resloveAClintSoft() {
	(*aclint_msip) = 0;

	disableMSoftInt();
	enableGlobalInt();
	return 0;
}

static int setUpPlicExt(){
	(*plic_priority_0) = 2;
	(*plic_priority_1) = 5;
	(*plic_priority_2) = 7;

	(*plic_enable_0 )  = 0xf;
	(*plic_hartReg_0)  = 4;

	enableMExtInt();
	enableGlobalInt();
	return 0;
}

static int reslovePlicExt(){

	uint64_t source = *plic_claim_complete_0;
	*plic_claim_complete_0 = source;

	disableMExtInt();
	enableGlobalInt();
	return 0;
}

volatile uint8_t flag_finish = 0;
int main()
{

	// setUpAClintTimmer();
	// setUpAClintSoft();
	setUpPlicExt();

	while(flag_finish != 1);

	return 0;

}

void handle_trap(void)
{
	// resloveAClintTimmer();
	// resloveAClintSoft();
	reslovePlicExt();

	flag_finish = 1;
}